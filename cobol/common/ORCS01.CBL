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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCS01.
      ****************************************************************
      *    OS           LINUX                                         
      *                                                               
      *    PROGRAM      ORCS01  /  ÉéÃ´¶â³Û»»Äê¥µ¥Ö                   
      *                          ¡Ê¿·Ï·¿ÍÊÝ¸±ÂÐ±þ¡Ë                   
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  00/12/01    MCC¡Ý¾®Æ¦Âô   ¿·µ¬ºîÀ®
      ****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    MCC¡Ý¾®Æ¦Âô  01/06/05  ÌôºÞ°ìÉôÉéÃ´¶â¤Î»»Äê¥í¥¸¥Ã¥¯
      *                                     ½¤Àµ
      *  01.00.02    MCC¡Ý¾®Æ¦Âô  01/06/11  º£²óÀÁµá³Û¤ËÊÝ¸±Å¬ÍÑ³°¶â³Û¤Î
      *                                     ²Ã»»¤â¤ì
      *  01.00.03    MCC¡ÝÆ£¸¶    01/07/10  ÅêÌô¤Î¤È¤­ºÞ·¿¶èÊ¬¤Ç¤Þ¤È¤á¤ë
      *  01.00.04    MCC¡Ý¾®Æ¦Âô  01/08/08  ÌôºÞ°ìÉôÉéÃ´¶â¤Î»»Äê¥í¥¸¥Ã¥¯
      *                                     ½¤Àµ(¾å¸Â¥Á¥§¥Ã¥¯ÄÉ²Ã)
      *  01.00.05    MCC¡Ý¾®Æ¦Âô  01/10/17  ÌôºÞ°ìÉôÉéÃ´¶â¤Î»»Äê¥í¥¸¥Ã¥¯
      *                                     ½¤Àµ(ÆÖÉþÌôºÞ¤Î²ó¿ô¤ò£±¤Ë¥»¥Ã¥È)
      *  01.00.06    MCC¡Ý¾®Æ¦Âô  01/10/21  ÌôºÞ°ìÉôÉéÃ´¶â¤Î»»Äê¥í¥¸¥Ã¥¯
      *                                     ½¤Àµ(¾å¸Â¶â³Û¤Î»»½Ð¤ò½èÊýÌôºÞ¤Î
      *                                          ¹ç·×ÃÍ¤ËÊÑ¹¹)
      *  01.00.07    MCC¡Ý¾®Æ¦Âô  01/10/25  ÌôºÞ°ìÉôÉéÃ´¶â¤Î»»Äê¥í¥¸¥Ã¥¯
      *                                     ½¤Àµ(£Å£Ö£Á£Ì£Õ£Á£Ô£Å¤Î£×£È£Å£Î
      *                                          ¾ò·ï¤òÊÑ¹¹)
      *  01.00.08    MCC¡Ý¾®Æ¦Âô  02/01/09  ´µ¼ÔÉéÃ´Ìµ¤·¡¢ÌôºÞ°ìÉôÉéÃ´Í­¤ê
      *                                     ¤Î¸øÈñ»ÈÍÑ»þ¤ÎÀÁµá¶â³Û»»Äê¤ò½¤Àµ
      *  01.00.09    MCC¡Ý¾®Æ¦Âô  02/01/23  ÆâÉþÌôºÞº®¹ç»þ¤ÎÌôºÞ°ìÉôÉéÃ´¶â
      *                                     »»Äê¤ò½¤Àµ
      *  01.00.10    MCC¡Ý¾®Æ¦Âô  02/02/05  Ï·¿ÍÉéÃ´¤È¸øÈñÉéÃ´¤Î·×»»Á°¥¯¥ê¥¢
      *                                     ¤òÄÉ²Ã
      *  01.00.11    MCC¡Ý¾®Æ¦Âô  02/03/07  ÌôºÞ°ìÉôÉéÃ´¶â¤Î·×»»»þ¤ËºÞÅÀ¿ô¤¬
      *                                     £²£°ÅÀ¤Ç¤¢¤ì¤ÐÆ±°ìÌôºÞ¤Î¤Þ¤È¤á
      *                                     (¼ïÎà¿ô¤Î¥«¥¦¥ó¥È)¤ò¤·¤Ê¤¤¤è¤¦¤Ë
      *                                     ½¤Àµ
      *  01.00.12    MCC¡Ý¾®Æ¦Âô  02/02/12  Ï·¿Í£´£±ÊÝ¸±¤ÎÆþÎÏ»þ¤Ë¥ì¥»¥×¥È¤Î
      *                                     ´µ¼ÔÉéÃ´³Û¤ò£±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ¤ò 
      *                                     ¹Ô¤Ê¤ï¤Ê¤¤¶â³Û¤Ë¤·¤¿
      *                                     (¸øÈñ£±¤Î¾å¸Â³Û¤¢¤ê¸ÂÄê¤ÇÄÉ²Ã)
      *  01.00.13    MCC¡Ý¾®Æ¦Âô  02/02/13  ´µ¼ÔÉéÃ´¶èÊ¬¤Ë£³¡§¸ÇÄê³Û¤òÄÉ²Ã
      *  01.00.14    MCC¡Ý¾®Æ¦Âô  02/04/12  Ï«ºÒÊÝ¸±ÂÐ±þ
      *                                     ºÆ·×»»»þ¤Ë¿ÇÎÅ¶èÊ¬Ëè¤ÎÅÀ¿ô½¸·×
      *                                     ¤«¤é±ß·ú¤Æ¹àÌÜÊ¬¤ÎÅÀ¿ô¤ò¸º»»¤¹¤ë
      *  01.00.15    MCC¡Ý¾®Æ¦Âô  02/09/06  ÆÃÄê¼À´µ¤Î¸øÈñ´µ¼Ô¤Î¥ì¥»¥×¥È
      *                                     °ìÉôÉéÃ´¶â³Û¤ò±ßÃ±°Ì¤Çµ­ºÜ¤¹¤ë¤è¤¦
      *                                     ½¤Àµ¡Ê»Í¼Î¸ÞÆþ½èÍý¤ò¥¹¥­¥Ã¥×¡Ë
      *                                     ¢¨´µ¼ÔÉéÃ´¶â¤Ï£±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ
      *  01.00.16    MCC¡Ý¾®Æ¦Âô  02/09/09  Ï«ºÒÊÝ¸±ÂÐ±þ
      *                                     Ï«ºÒÊÝ¸±»þ¤Ï´µ¼ÔÀÁµá³Û¤òÌµ¾ò·ï¤Ë
      *                                     ¥¼¥í±ß¤È¤¹¤ë
      *  01.00.17    MCC¡Ý¾®Æ¦Âô  02/09/10  Ï·¿Í£´£±ÊÝ¸±¤ÎÆþÎÏ»þ¤Ë¥ì¥»¥×¥È¤Î
      *                                     ´µ¼ÔÉéÃ´³Û¤ò£±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ¤ò 
      *                                     ¹Ô¤Ê¤ï¤Ê¤¤¾ò·ï¤ËÄê³ÛÀ©¤Î¾ì¹ç¤â
      *                                     ÄÉ²Ã¤·¤¿
      *  01.00.18    MCC¡Ý¾®Æ¦Âô  02/09/17  ÆÃÄê¼À´µ¥ì¥»¥×¥È¤Î´µ¼ÔÉéÃ´³Û¤Î
      *                                     ·×»»¤òÄûÀµ
      *  01.00.19    MCC¡ÝÌçÏÆ    02/09/17  ¼«ÇåÀÕÊÝ¸±ÂÐ±þ
      *  01.00.20    MCC¡Ý¾®Æ¦Âô  02/09/17  ¡ÊÊ¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ¡Ë
      *                                     ·òÊÝ¤Î¹âÎð¼Ô¤Ë¤Ä¤¤¤ÆÊä½õ¶èÊ¬¤è¤ê
      *                                     ÉéÃ´³ä¹ç¤ò¥»¥Ã¥È
      *  01.00.21    MCC¡Ý¾®Æ¦Âô  02/09/17  ¡ÊÊ¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ¡Ë
      *                                     £³ºÐÌ¤Ëþ¤Î´µ¼Ô¤Ë¤Ä¤¤¤Æ¤ÏÊÝ¸±¤Î
      *                                     ÉéÃ´³ä¹ç¤ò£²³ä¤È¤¹¤ë
      *                                     Ã¢¤·¹ñÊÝÅù¤ÇÊä½õ¶èÊ¬¤Ë£±³ä¤¬ÁªÂò
      *                                     ¤µ¤ì¤¿¾ì¹ç¡¢¤½¤Á¤é¤òÍ¥Àè¤¹¤ë¤³¤È
      *  01.00.22    MCC¡Ý¾®Æ¦Âô  02/09/19  ¡ÊÊ¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ¡Ë
      *                                     Ï·¿ÍÊÝ¸±¤Î£¸¡¦£¹³äÂÐ±þ
      *  01.00.23    MCC¡Ý¾®Æ¦Âô  02/09/25  ¡ÊÊ¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ¡Ë
      *                                     ¿²¤¿¤­¤êÏ·¿ÍºßÂðÁí¹ç¿ÇÎÅÎÁ¤È
      *                                     ºßÂðËö´ü°åÎÅÁí¹ç¿ÇÎÅÎÁ¤Î»»Äê
      *                                     ¹âÎð¼Ô¤Ä¤¤¤ÆÉéÃ´³Û¤Î¾å¸Â¤òÀßÄê
      *  01.00.24    NACL¡ÝÌçÏÆ   02/10/08  ¡ÊÊ¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ¡Ë
      *                                     ÄÉ²Ã½¤Àµ
      *  01.00.25    NACL¡ÝÌçÏÆ   02/10/28  ÄûÀµ»þ¤Î¸ºÌÈ»»Äê½¤Àµ
      *  01.00.26    NACL¡Ý¾®Æ¦Âô 02/12/05  £¶£µºÐ°Ê¾å¤Î¿²¤¿¤­¤êÏ·¿Í¤Ë¤Ä¤¤¤Æ
      *                                     ÌôºÞÉéÃ´¶â¤ò»»Äê¤·¤Ê¤¤¤è¤¦½¤Àµ
      *  01.00.27    NACL¡Ý¾®Æ¦Âô 02/12/20  £¶£µºÐ°Ê¾å¤Î¿²¤¿¤­¤êÏ·¿Í¤Ë¤Ä¤¤¤Æ
      *                                     ÌôºÞÉéÃ´¶â¤ò»»Äê¤·¤Ê¤¤¤è¤¦½¤Àµ
      *                                     ¡Ê¥Õ¥é¥°½é´ü²½°ÌÃÖÊÑ¹¹¡Ë
      *  01.00.28    NACL¡ÝÌçÏÆ   03/02/06  ¼«ÈñÊÝ¸±¤Î²ÝÀÇÂÐ±þ
      *  01.00.29    NACL¡ÝÌçÏÆ   03/03/07  Ï·¿Í¸ÂÅÙ³Û¤Î½¸·×ÊÑ¹¹
      *  01.00.30    NACL¡ÝÌçÏÆ   03/03/24  Ê¿À®£±£µÇ¯£´·î£±Æü²þÀµÂÐ±þ
      *                                     ¡ãÌôºÞ°ìÉôÉéÃ´¶âÇÑ»ß¡ä
      *  01.00.31    NACL¡ÝÌçÏÆ   03/03/25  ¹¹Àµ¡¦°éÀ®Åù¤Î´µ¼Ô¸øÈñÉéÃ´ÂÐ±þ
      *  01.00.32    NACL¡ÝÌçÏÆ   03/04/11  Ï«ºÒ¡¦¼«ÇåÀÕÉéÃ´¶â·×»»ÊÑ¹¹
      *                                     ¡ÊÏ«ºÒÈó»ØÄê°åÎÅµ¡´ØÂÐ±þ´Þ¤à¡Ë
      *  01.00.33    NACL¡ÝÌçÏÆ   03/04/15  ¼«ÇåÀÕÊ¸½ñÎÁ¾ÃÈñÀÇÂÐ±þ
      *  01.00.34    NACL¡ÝÌçÏÆ   03/04/16  Á¥°÷ÊÝ¸±¡ÊÈïÊÝ¸±¼Ô¡Ë¡ÜÄÌ¶ÐºÒ³²
      *                                     ¡Ü½é¿Ç»þ¡¡ÂÐ±þ
      *  01.00.35    NACL-ÌçÏÆ    03/05/07  ÃÏÊýÏ·¿Í¤ÎÉéÃ´³Û¤Î»Í¼Î¸ÞÆþ¤ÎÀßÄê
      *                                     ¤òÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¤ÎÀßÄê¤Ç¹Ô¤¦¡£
      *  01.00.36    NACL-ÌçÏÆ    03/05/12  ¼«ÇåÀÕ¤Î´µ¼ÔÀÁµá¤·¤Ê¤¤¡ã£°¡ó¡ä
      *                                     ¾ì¹ç¤ÎÂÐ±þ
      *  01.00.37    NACL-ÌçÏÆ    03/05/21  ¸ºÌÈ¤ÎÃ¼¿ôÀßÄêÂÐ±þ
      *  01.00.38    NACL-ÌçÏÆ    03/05/22  ¼«ÇåÀÕ¤ÎÎÅÍÜÃ´Åö¼êÅöÂÐ±þ
      *  01.00.39    NACL-ÌçÏÆ    03/06/06  ¾ÃÈñÀÇÃ¼¿ôÂÐ±þ
      *  01.00.40    NACL-ÌçÏÆ    03/07/01  ¥ì¥»µ­ºÜÍÑ°ìÉôÉéÃ´¶âÊÔ½¸½¤Àµ
      *  01.00.41    NACL-ÌçÏÆ    03/08/27  ÆÃÄê¼À´µ²þÀµÂÐ±þ
      *  01.00.42    NACL-¾®Æ¦Âô  03/09/02  Ê¡²¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.00.43    NACL-ÌçÏÆ    03/10/02  ÅìµþÅÔÃÏÊý¸øÈñÂÐ±þ
      *  01.00.44    NACL-¾®Æ¦Âô  03/11/19  ´µ¼ÔÉéÃ´¶âÆÈ¼«¥×¥í¥°¥é¥àÁÈ¹þ¤ß
      *  01.00.45    NACL-ÌçÏÆ    04/01/14  ¿ÇÎÅ¹Ô°Ù¤ÎÊñ³çÊ¬¤Ï½ü¤¯
      *                                     SRYACCT5¢ª31
      *  01.00.46    NACL-ÌçÏÆ    04/01/15  ¾ÃÈñÀÇ¹àÌÜÅùÄÉ²Ã¹àÌÜÊÔ½¸
      *  01.00.47    NACL-ÌçÏÆ    04/02/10  À¸ÊÝ¤Î¸øÈñÉéÃ´¸ÂÅÙ³ÛÀßÄê
      *  01.00.48    NACL-¾®Æ¦Âô  04/02/19  Ê¡²¬´µ¼Ô¤ÎµëÉÕ³°¥¨¥ê¥¢½é´ü²½
      *  01.00.49    NACL-ÌçÏÆ    04/03/12  ¸øÈñÉéÃ´±ßÃ±°ÌÊÔ½¸
      *  01.00.50    NACL-ÌçÏÆ    04/04/06  ºßÁí¿Ç¥Õ¥é¥°ÊÔ½¸½¤Àµ
      *  01.00.51    NACL-ÌçÏÆ    04/07/27  ¹ñÊÝÉéÃ´³ä¹çÊÔ½¸½¤Àµ
      *  01.00.52    NACL-ÌçÏÆ    04/07/27  ¿·³ã¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.00.53    NACL-ÌçÏÆ    04/08/04  Æ±°ìÊÝ¸±ÈÖ¹æÂÐ±þ
      *
      *  01.01.01    NACL-ÌçÏÆ    04/08/10  Ê£¿ôÊÝ¸±¡¦Ê£¿ô²ÊÆþÎÏÂÐ±þ
      *  01.01.02    NACL-ÌçÏÆ    04/08/20  Ê¼¸Ë¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.01.03    NACL-ÌçÏÆ    04/08/23  ¹­Åç¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.01.04    NACL-ÌçÏÆ    04/09/07  £±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç
      *                                     ¤Î½¤Àµ
      *  01.01.05    NACL-ÌçÏÆ    04/09/07  ËÌ³¤Æ»ÃÏÊý¸øÈñÂÐ±þ
      *  01.01.06    NACL-ÌçÏÆ    04/09/08  À¸ÊÝÃ±ÆÈ¤Î¸ÂÅÙ³Û½¤Àµ
      *  01.01.07    NACL-ÌçÏÆ    04/09/09  »³·Á¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.01.08    NACL-ÌçÏÆ    04/09/10  ·§ËÜ¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.01.09    NACL-ÌçÏÆ    04/09/24  ÀÁµá³ÛÃ¼¿ô½èÍýÂÐ±þ
      *  01.01.10    NACL-ÌçÏÆ    04/10/01  ¼«ÇåÀÕÆÃÄê´ïºàÅù¾ÃÈñÀÇÂÐ±þ
      *  01.01.11    NACL-ÌçÏÆ    04/10/21  ¸øÈñ»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç¤Î½¤Àµ
      *  01.01.12    NACL-ÌçÏÆ    04/11/15  ¸øÈñ·î¾å¸Â³Û¼èÆÀ½¤Àµ
      *  01.01.13    NACL-ÌçÏÆ    04/11/24  ºë¶Ì¸©£´£±Ï·¿Í¤¬Ä¹´ü¤ÈÊ»ÍÑ¤Î
      *                                     ¾ì¹ç¡¢£±£°£°£°£°±ß¾å¸Â¤È¤¹¤ë
      *  01.01.14    NACL-ÌçÏÆ    04/11/25  Ê¡Åç¸©ÆýÍÄ»ùÆÃ¼ì·×»»ÂÐ±þ
      *  01.01.15    NACL-ÌçÏÆ    05/01/14  ²¬»³¸©ÆýÍÄ»ùÆÃ¼ì·×»»ÂÐ±þ
      *  01.01.16    NACL-ÌçÏÆ    05/03/04  ÀÅ²¬¸©Êì»Ò¾ã³²¤¬Ä¹´ü¤ÈÊ»ÍÑ¤Î
      *                                     ¾ì¹ç¡¢£±£°£°£°£°±ß¾å¸Â¤È¤¹¤ë
      *  01.01.17    NACL-ÌçÏÆ    05/03/23  ÊÝ¸±ÁÈ¹ç¤»¤Î¾å¸Â¶èÊ¬ÊÔ½¸
      *  01.02.01    NACL-ÌçÏÆ    05/03/24  ¾®»ùÆÃÄê¾å¸ÂÂÐ±þ
      *  01.02.02    NACL-ÌçÏÆ    05/03/30  ÂçºåÉÜÃÏÊý¸øÈñ¡ÜÄ¹´üÂÐ±þ
      *  01.02.03    NACL-ÌçÏÆ    05/04/13  ÀÅ²¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.02.04    NACL-ÌçÏÆ    05/05/06  °ñ¾ë¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.02.05    NACL-ÌçÏÆ    05/06/09  Äã½êÆÀ¾ðÊóÊÔ½¸½¤Àµ
      *  01.02.06    NACL-ÌçÏÆ    05/06/21  ¹¹À¸¡¦°éÀ®¤¬Ä¹´ü¤ÈÊ»ÍÑ¤Î¾ì¹ç¤ÎÂÐ±þ
      *  01.02.07    NACL-ÌçÏÆ    05/06/22  Ê¼¸Ë¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.02.08    NACL-ÌçÏÆ    05/06/27  ²Ã»»Î¨£³·åÂÐ±þ
      *  01.02.09    NACL-ÌçÏÆ    05/07/01  ºë¶Ì¸©¿·ºÂ»ÔÂÐ±þ
      *  01.02.10    NACL-ÌçÏÆ    05/07/08  Ê¡°æ¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.02.11    NACL-ÌçÏÆ    05/07/12  ½©ÅÄ¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.02.12    NACL-¾®Æ¦Âô  05/07/15  ¼«Èñ£±£°¹àÌÜÂÐ±þ
      *  01.02.13    NACL-ÌçÏÆ    05/07/19  ¼ýÇ¼¹àÌÜÄÉ²Ã¤ËÈ¼¤¦½¤Àµ
      *  01.02.14    NACL-ÌçÏÆ    05/08/25  Ê¡Åç¡¢²¬»³¡¢ºë¶ÌÆÃ¼ì·×»»½¤Àµ
      *  01.02.15    NACL-ÌçÏÆ    05/08/26  Ä¹´ü¤Ë¤Ä¤¤¤Æ¤Î»»Äê½¤Àµ
      *  01.02.16    NACL-ÌçÏÆ    05/09/01  ¾ÊÄ£ÂÐ±þ
      *  01.02.17    NACL-ÌçÏÆ    05/09/15  Åçº¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.02.18    NACL-ÌçÏÆ    05/09/21  Á´¹ñÅÚÌÚ·úÃÛ¡¢Á´¹ñÈÄ¶â¶È¡Ü
      *                                     Àº¿À¡¢·ë³ËÂÐ±þ
      *  01.02.19    NACL-ÌçÏÆ    05/10/03  µÜ¾ë¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.02.20    NACL-ÌçÏÆ    06/01/17  ¹âÎð¼Ô¤Î¼çÊÝ¸±Ëô¤Ï£²£·Ï·¿Í¾ðÊó
      *                                     ¡Ê±ßÃ±°Ì¡Ë½¤Àµ
      *  01.02.21    NACL-ÌçÏÆ    06/02/17  ÆÃÊÌÎÅÍÜÈñÂÐ±þ
      *  01.02.22    NACL-ÌçÏÆ    06/02/22  ¼«ÈñÊÝ¸±·×»»¡¦½¸·×ÀèÊÔ½¸½¤Àµ
      *  01.03.01    NACL-ÌçÏÆ    06/03/09  ¤¤¤ï¤­»Ô½Å¿´ÂÐ±þ
      *  01.03.02    NACL-ÌçÏÆ    06/03/09  ÅìµþÅÔ¥Þ¥ë¾ã¡ÜÄ¹´üÂÐ±þ
      *  01.03.03    NACL-ÌçÏÆ    06/03/20  ¼«Î©»Ù±çË¡ÂÐ±þ
      *  01.03.04    NACL-ÌçÏÆ    06/03/30  ¼«Î©»Ù±çË¡¡Ê¹â³ÛÎÅÍÜÈñ¸½ÊªÂÐ±þ¡Ë
      *  01.03.05    NACL-ÌçÏÆ    06/04/04  ºßÁí¿ÇÂÐ¾ÝÊÔ½¸½¤Àµ
      *  02.09.01    NACL-ÌçÏÆ    06/04/19  ´ä¼ê¸©ÃÏÊý¸øÈñÂÐ±þ
      *  02.09.02    NACL-ÌçÏÆ    06/05/02  ¼«Î©»Ù±ç¿À¸Í»ÔÂÐ±þ
      *  02.09.03    NACL-ÌçÏÆ    06/05/18  ²¬»³¸©À¾°ÀÁÒÂ¼Âà¿¦¼Ô¹ñÊÝ£²³äÂÐ±þ
      *  02.09.04    NACL-ÌçÏÆ    06/06/09  ¼«Î©»Ù±ç¿À¸Í»ÔÂÐ±þ½¤Àµ
      *  02.09.05    NACL-ÌçÏÆ    06/06/12  ¼«Î©»Ù±ç¤Î¾å¸Â³Û¤¬¤¢¤ë¾ì¹ç¤Î
      *                                     ¹âÎð¼Ô¡¢Ä¹´ü¤Ë´Ø¤¹¤ëÉéÃ´·×»»½¤Àµ
      *  02.09.06    NACL-ÌçÏÆ    06/06/22  Ê¿À®£±£¸Ç¯£´·î°Ê¹ß¤ÎÁ´¹ñÅÚÌÚ·úÃÛ¡¢
      *                                     Á´¹ñÈÄ¶â¶È¡Ü£°£²£±Àº¿ÀÂÐ±þ
      *  03.01.01    NACL-ÌçÏÆ    06/07/06  ÎÎ¼ý½ñÂÐ±þ
      *  03.02.01    NACL-ÌçÏÆ    06/07/31  ¹âÎð¼Ô¾å¸Â³Û·Ð²áÁ¼ÃÖÂÐ±þ
      *  03.02.02    NACL-ÌçÏÆ    06/08/17  °ñ¾ë¸©ÃÏÊý¸øÈñÂÐ±þ
      *  03.02.03    NACL-ÌçÏÆ    06/08/18  ²¬»³¸©ÃÏÊý¸øÈñ£±£°·î²þÀµÂÐ±þ
      *  03.02.04    NACL-ÌçÏÆ    06/09/05  ·ò¹¯ÊÝ¸±Ë¡£±£°·î²þÀµÂÐ±þ
      *  03.02.05    NACL-ÌçÏÆ    06/09/16  »³Íü¸©ÃÏÊý¸øÈñÂÐ±þ
      *  03.02.06    NACL-ÌçÏÆ    06/09/20  ÂçÊ¬¸©ÃÏÊý¸øÈñ£±£°·î²þÀµÂÐ±þ
      *  03.02.07    NACL-ÌçÏÆ    06/09/25  °¦ÃÎ¸©ÃÏÊý¸øÈñ£±£°·î²þÀµÂÐ±þ
      *  03.02.08    NACL-ÌçÏÆ    06/10/02  ÅìµþÅÔÃÏÊý¸øÈñ£±£°·î²þÀµÂÐ±þ
      *  03.02.09    NACL-ÌçÏÆ    06/10/03  ¿ÀÆàÀî¸©ÃÏÊý¸øÈñ£±£°·î²þÀµÂÐ±þ
      *  03.03.01    NACL-ÌçÏÆ    06/10/17  ¸º³Û¡¦ÌÈ½ü¡¦»ÙÊ§Í±Í½ÂÐ±þ
      *  03.03.02    NACL-ÌçÏÆ    06/10/18  ËÌ³¤Æ»ÃÏÊý¸øÈñÂÐ±þ
      *  03.03.03    NACL-ÌçÏÆ    06/10/23  ÃÏÊý¸øÈñÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿Äã½êÆÀ
      *                                     ÄãÇ¯¶â¥¿¥ÖÍ­¸ú²½ÂÐ±þ
      *  03.03.04    NACL-ÌçÏÆ    06/10/30  ¾ã³²»ù»ÜÀß°åÎÅ¡¦ÎÅÍÜ²ð¸î°åÎÅ
      *                                     ÂÐ±þ
      *  03.03.05    NACL-ÌçÏÆ    06/11/02  ¸º³Û¡¦ÌÈ½ü¡¦»ÙÊ§Í±Í½ÂÐ±þ
      *  03.03.06    NACL-ÌçÏÆ    06/11/08  ²¬»³¸©ÃÏÊý¸øÈñ£±£°·î²þÀµÂÐ±þ
      *  03.03.07    NACL-ÌçÏÆ    06/11/09  ¹âÎð¼Ô¤ÇÄã½êÆÀ¤«¤ÄÁ´¹ñ¸øÈñ¤ò
      *                                     ¤â¤Ä¾ì¹ç¤Î¾å¸Â³ÛÊÔ½¸½¤Àµ
      *  03.04.01    NACL-ÌçÏÆ    06/11/20  ¹â³Û°ÑÇ¤Ê§¤¤À©ÅÙÂÐ±þ
      *  03.04.02    NACL-ÌçÏÆ    06/11/24  µÜ¾ë¸©ÃÏÊý¸øÈñ£±£°·î²þÀµÂÐ±þ
      *  03.04.03    NACL-ÌçÏÆ    06/12/01  ¼«ÇåÀÕ·òÊÝ½àµòÂÐ±þ
      *  03.04.04    NACL-ÌçÏÆ    06/12/26  ÅìµþÍýÍÆÅù¡ÜÀº¿À¡¢·ë³ËÂÐ±þ
      *  03.04.05    NACL-ÌçÏÆ    07/01/25  ¾ÃÈñÀÇÁí³ÛÉ½¼¨ÂÐ±þ
      *  03.04.06    NACL-ÌçÏÆ    07/02/05  ËÌ³¤Æ»°°Àî»ÔÂÐ±þ
      *  03.04.07    NACL-ÌçÏÆ    07/02/14  Ê¡Åç¸©ÈÓ´ÛÂ¼ÂÐ±þ
      *  03.04.08    NACL-ÌçÏÆ    07/03/19  ¹â³Û¸½ÊªµëÉÕÂÐ±þ
      *  03.04.09    NACL-ÌçÏÆ    07/03/22  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *                                     ¡ÊÀî¸ý¡¦½ÕÆüÉô»Ô¡Ë
      *  03.04.10    NACL-ÌçÏÆ    07/04/26  ¹­Åç¸©½Å¿´¡ÜÄ¹´üÂÐ±þ
      *  03.04.11    NACL-ÌçÏÆ    07/05/08  ËÌ³¤Æ»°°Àî»ÔÂÐ±þ
      *  03.04.12    NACL-ÌçÏÆ    07/06/13  ¸½ÊªµëÉÕ¹â³Û£´²óÌÜ¸øÈñÊ»ÍÑ»þÂÐ±þ
      *  03.04.13    NACL-ÌçÏÆ    07/07/13  ËÌ³¤Æ»ÃÏÊý¸øÈñÂÐ±þ
      *  03.04.14    NACL-ÌçÏÆ    07/07/20  ËÌ³¤Æ»£´£±Ï·¿Í¸½ÊªµëÉÕÂÐ±þ
      *****************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE SECTION.
      *
       01  STS-AREA.
           03  STS-SYUNOU              PIC X(02).
           03  STS-TSYRRK              PIC X(02).
           03  STS-TNKRRK              PIC X(02).
           03  STS-HKNNUM              PIC X(02).
           03  STS-HKNCOMBI            PIC X(02).
       01  SYURYO-YMD.
           03  SYURYO-YY               PIC 9(04).
           03  SYURYO-MM               PIC 9(02).
           03  SYURYO-DD               PIC 9(02).
       01  KEISAN-AREA.
           03  WK-KNJFTNKIN            PIC 9(10).
           03  WK-KNJFTNKIN-ENTANI     PIC 9(08)V9(01).
           03  WK-ENTANI               PIC 9(08).
           03  WK-ENTANI2              PIC 9(08).
           03  WK-CYOKI                PIC 9(08).
           03  WK-CYOKI-ENTANI         PIC 9(08).
           03  WK-CYOKI-ZAN            PIC 9(08).
           03  WK-CYOKI-ZAN-ENTANI     PIC 9(08).
           03  WK-ZENGAKU              PIC 9(10).
           03  WK-ZENGAKU-SYU          PIC 9(10).
           03  WK-ZENGAKU-YAK          PIC 9(10).
           03  WK-FTNKIN1              PIC 9(10).
           03  WK-FTNKIN2              PIC 9(10).
           03  WK-FTNKIN3              PIC 9(10).
           03  WK-FUTANWARI            PIC 9V99.
           03  WK-SEIKYUGK             PIC 9(10).
           03  WK-YAKFTN-TENSUU        PIC 9(10).
           03  WK-GAIKFTNRATE          PIC 9(03).
           03  WK-GAIYKZFTNKBN         PIC X(01).
           03  WK-GAIKKOTGAK           PIC 9(06).
           03  WK-TOKUTEI-COMPFTN      PIC 9(08).
           03  WK-TOKUTEI-COMPFTN2     PIC 9(08).
           03  WK-TOKUTEI-COMPFTN3     PIC 9(08).
           03  WK-HANGAKU-COMPFTN      PIC 9(07).
           03  WK-HANGAKU-ENTANI       PIC 9(07)V9(01).
           03  WK-GENMEN-KEISAN        PIC 9(07).
           03  WK-GENMEN-KEISAN2       PIC 9(07).
           03  WK-GENMEN-KEISAN3       PIC 9(07).
           03  WK-MIYAGI-COMPFTN       PIC 9(07).
           03  WK-SEISIN-COMPFTN       PIC 9(07).
           03  WK-JYOSEIGAKU           PIC 9(07).
           03  WK-KEISU                PIC 9V99.
           03  WK-JIHIGAKU             PIC 9(10).
           03  WK-JIHIGAKU2            PIC 9(10).
           03  WK-JIHIGAKU3            PIC 9(10).
           03  WK-JIHIFTN              PIC 9V99.
           03  WK-ZAN-JIRITU           PIC 9(08).
           03  WK-KOH-GENDOGAKU        PIC 9(07).
           03  WK-GEN70-GENDOGAKU      PIC 9(07).
           03  WK-GEN70-GENDOGAKU2     PIC 9(07).
           03  WK-SKYMONEY-ZEI         PIC 9(07).
           03  WK-RSIETC-ZEITAISYO     PIC 9(07).
           03  WK-KEISAN-JIBAI         PIC 9(07).
       01  SHOHOUSEN-AREA.
           03  FLG-INNAI               PIC X(01).
           03  FLG-INGAI               PIC X(01).
       01  FLG-AREA.
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-TNKRRK              PIC 9(01).
           03  FLG-TSYRRK              PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-NYUGAI              PIC 9(01).
           03  FLG-SANSHOU             PIC X(01).
           03  FLG-SANSHOU2            PIC X(01).
           03  FLG-SANSHOU3            PIC X(01).                       
           03  FLG-SANTEI1             PIC X(01).
           03  FLG-SANTEI2             PIC X(01).
           03  FLG-SANTEI3             PIC X(01).
           03  FLG-SANTEI4             PIC X(01).
           03  FLG-FTNKIN1             PIC X(01).
           03  FLG-FTNKIN2             PIC X(01).
           03  FLG-FTNKIN3             PIC X(01).
           03  FLG-SHOHOZUMI           PIC X(01).
           03  FLG-YAKFTN              PIC X(01).
           03  FLG-KUMAMOTO            PIC X(01).
           03  FLG-TAIGAI              PIC X(01).
      *
           03  FLG-ZAIGATAKBN          PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-ADDKBN              PIC 9(01). 
      *
           03  FLG-LOCALRJN            PIC 9(01).
           03  FLG-NOCOUNT             PIC 9(01).
           03  FLG-GRPNYURYOKU         PIC 9(01).
           03  FLG-GENMEN              PIC 9(01).
           03  FLG-SEISINJYO           PIC 9(01).
           03  FLG-TOKUREI-HKNJYA      PIC 9(01).
           03  FLG-MIYAGI              PIC 9(01).
           03  FLG-KEIKASOCHI-IPAN     PIC 9(01).
           03  FLG-TOKYO               PIC 9(01).
           03  FLG-HIKAKU              PIC 9(01).
           03  FLG-KOHININBARAI        PIC 9(01).
           03  FLG-KOHYONKAIME         PIC 9(01).
           03  FLG-GENBUTU70           PIC 9(01).
           03  FLG-GEN70-TOKUSYU       PIC 9(01).
           03  FLG-SYUKEI              PIC 9(01).
           03  FLG-SYUKEI2             PIC 9(01).
      *
       01  WRK-AREA.
           03  WK-END                  PIC 9(08).
           03  WK-KEISAN               PIC S9(07).
           03  WK-RYOYO                PIC 9(07).
           03  WK-4001-KSNRATE9        PIC 9(03).
           03  WK-4001-KSNRATE         PIC 9V99.
           03  WK-4001-TAXKBN          PIC X(01).
           03  WK-4001-TAXKBN-KZI      PIC X(01).
           03  WK-4001-JIBAIKBN        PIC X(01).
           03  WK-4001-SYUTENTANKA     PIC 9(02).
           03  WK-4001-YKZTENTANKA     PIC 9(02).
      *
           03  WK-ERR                  PIC 9(01).
           03  WK-UERR                 PIC 9(01).
           03  WK-PAYKBN               PIC X(02).
           03  WK-1001-HOSPID          PIC X(24).
           03  WK-1001-HOSPSBT         PIC 9(01).
           03  WK-1001-ROUPAYKBN       PIC X(01).
           03  WK-1001-JIHIPOSITION    PIC X(01).
           03  WK-1001-CHIHOUNUMKBN    PIC X(01).
      *
           03  WK-GAIJGNKBN            PIC X(01).
           03  WK-GAIJGNGAK            PIC 9(07).
      *
           03  WK-TEISYOTOKU-YMD.
               05  WK-TEISYOTOKU-YM    PIC 9(06).
               05  WK-TEISYOTOKU-DD    PIC 9(02).
      *
      *    ÊÝ¸±¶èÊ¬¡Ê¶õÇò¡§°åÊÝ¡¡£±¡§Ï«ºÒ¡¡£²¡§¼«ÇåÀÕ¡Ë
           03  WK-HKNKBN               PIC X(01).
      *
      *    ´µ¼ÔÉéÃ´·×»»»þ¤Î»Í¼Î¸ÞÆþÍÑ¥ï¡¼¥¯
           03  WK-FTN                  PIC 9(10).
      *
      *    ÉÂ±¡³ä°ú³Û·×»»»þ¤Î³ä°úÎ¨ÍÑ¥ï¡¼¥¯
           03  WK-RATE                 PIC 9(1)V9(2).
      *
      *    ÀÁµá³Û¶â³ÛÃ¼¿ô¶èÊ¬¡Ê¸ºÌÈÍ­¤ê¡Ë
           03  WK-HASU1001-KBN         PIC X(1).
      *    ÀÁµá³Û¶â³ÛÃ¼¿ô¶èÊ¬¡Ê¸ºÌÈÌµ¤·¡Ë
           03  WK-HASU1001-KBN2        PIC X(1).
      *
      *    ¾ÃÈñÀÇÃ¼¿ô¶èÊ¬
           03  WK-TAX1001-KBN          PIC X(1).
      *
      *    ¸©ÈÖ¹æ
           03  WK-1001-PREFNUM         PIC 9(2).
      *
      *    Ê¡²¬¸©ÃÏÊý¸øÈñÉéÃ´¶èÊ¬¡¦¼çÊÝ¸±¤ÎÉéÃ´³ä¹ç
           03  WK-KOH-FTNKBN           PIC X(1).
           03  WK-KOH-FTNRATE          PIC 9(3).
      *
           03  WK-TAIHI-DENPNUMXX-OCC.
               05  WK-TAIHI-DENPNUMXX  PIC 9(7)    OCCURS   4.
      *
           03  WK-TAIHI-RECEFTNDSPKBN-OCC.
               05  WK-TAIHI-RECEFTNDSPKBN
                                       PIC X(1)    OCCURS   4.
      *
      *    ÆÃ¼ì·×»»ÍÑ
           03  WK-TOKUSYU-COMPFTN      PIC 9(07).
           03  WK-TOKUSYU-COMPFTN2     PIC 9(07).
           03  WK-TOKUSYU-ENTANI       PIC 9(07)V9(01).
           03  WK-TOKUSYU              PIC 9(07).
           03  WK-TOKUSYU2             PIC 9(07).
           03  WK-TAIHI-TOKU1          PIC 9(07)V9(01).
           03  WK-TAIHI-TOKU2          PIC 9(07).
      *
      *    ¼«ÇåÀÕÆÃÄê´ïºàÅù¾ÃÈñÀÇÂÐ¾Ý³°¥³¡¼¥É
           03  WK-4003-TAIGAI.
               05  WK-4003-TAIGAI-OCC      OCCURS  10.
                 06  WK-4003-TAIGAI-SRYCD  PIC  X(09).
      *
      *    ¼«ÇåÀÕ¤½¤ÎÂ¾¥³¡¼¥É¡Ê£°£¹£µ£¹£±¡¥£¹£²¡¥£¹£³¡Ë¤Î¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÅù
           03  WK-SONOTA91.
               05  WK-SONOTA91-OCC         OCCURS  20.
                 06  WK-SONOTA91-SRYCD     PIC  X(09).
                 06  WK-SONOTA91-ZAIKAISU  PIC  9(07).
                 06  WK-SONOTA91-TANKA     PIC  9(07).
           03  WK-SONOTA92.
               05  WK-SONOTA92-OCC         OCCURS  20.
                 06  WK-SONOTA92-SRYCD     PIC  X(09).
                 06  WK-SONOTA92-ZAIKAISU  PIC  9(07).
                 06  WK-SONOTA92-TANKA     PIC  9(07).
           03  WK-SONOTA93.
               05  WK-SONOTA93-OCC         OCCURS  20.
                 06  WK-SONOTA93-SRYCD     PIC  X(09).
                 06  WK-SONOTA93-ZAIKAISU  PIC  9(07).
                 06  WK-SONOTA93-TANKA     PIC  9(07).
      *
           03  WK-JIRITU-TBL.
               05  WK-JIRITU-OCC           OCCURS   4.
                 06  WK-JIRITU-CYUKANSOU   PIC  9(01).
                 06  WK-JIRITU-TAFTNRUIKEI PIC  9(07).
      *
           03  WK-JIRITU-SRYYMD            PIC  X(08).
      *
           03  WK-OITA-HKNFTNMONEY         PIC  9(07).
           03  WK-OITA-CYOFTN              PIC  9(07).
           03  WK-OITA-CYOFTN-KON          PIC  9(07).
           03  WK-OITA-KEISAN              PIC  9(07).
           03  WK-OITA-KEISAN2             PIC  9(07).
      *
           03  WK-GENGAKUKBN               PIC  9(01).
           03  WK-GENGAKU-KOHID            PIC  9(10).
           03  WK-GENGAKU-RITU             PIC  9(03).
           03  WK-GENGAKU-RITUX            PIC  9(03).
           03  WK-GENGAKU-KIN              PIC  9(07).
           03  WK-GENGAKU-GAK              PIC  9(07).
           03  WK-GENGAKU-GAK-ENTANI       PIC  9(07)V9(01).
      *
           03  WK-KOHININBARAI-KOHID       PIC  9(10).
      *
           03  WK-CHIHOU-TAFTNRUIKEI-TBL.
               05  WK-CHIHOU-TAFTNRUIKEI-OCC    OCCURS   4.
                 06  WK-CHIHOU-TAFTNRUIKEI      PIC  9(07).
      *
      *    ¸øÈñ¤Î´µ¼ÔÉéÃ´¾å¸Â²ó¿ô¤È¾å¸Â¶â³Û¤ÎÂàÈòÍÑ¥¨¥ê¥¢
       01  HOKEN-MAX-AREA.
           03  MAX-KAI-KOHI1-D         PIC 9(05).
           03  MAX-KAI-KOHI2-D         PIC 9(05).
           03  MAX-KAI-KOHI3-D         PIC 9(05).
           03  MAX-KAI-KOHI4-D         PIC 9(05).
           03  MAX-KAI-KOHI1-M         PIC 9(05).
           03  MAX-KAI-KOHI2-M         PIC 9(05).
           03  MAX-KAI-KOHI3-M         PIC 9(05).
           03  MAX-KAI-KOHI4-M         PIC 9(05).
           03  MAX-KIN-KOHI1-D         PIC 9(08).
           03  MAX-KIN-KOHI2-D         PIC 9(08).
           03  MAX-KIN-KOHI3-D         PIC 9(08).
           03  MAX-KIN-KOHI4-D         PIC 9(08).
           03  MAX-KIN-KOHI1-M         PIC 9(08).
           03  MAX-KIN-KOHI2-M         PIC 9(08).
           03  MAX-KIN-KOHI3-M         PIC 9(08).
           03  MAX-KIN-KOHI4-M         PIC 9(08).
           03  MAX-KIN-KOHI1-K         PIC 9(08).
           03  MAX-KIN-KOHI2-K         PIC 9(08).
           03  MAX-KIN-KOHI3-K         PIC 9(08).
           03  MAX-KIN-KOHI4-K         PIC 9(08).
           03  TEI-KIN-KOHI1-K         PIC 9(08).
           03  TEI-KIN-KOHI2-K         PIC 9(08).
           03  TEI-KIN-KOHI3-K         PIC 9(08).
           03  TEI-KIN-KOHI4-K         PIC 9(08).
      *
      *    ¸øÈñ¤Î»ÈÍÑ²ó¿ô¥«¥¦¥ó¥ÈÍÑ¥¨¥ê¥¢
       01  HOKEN-KAISU-AREA.
           03  KAISU-KOHI1-D           PIC 9(05).
           03  KAISU-KOHI2-D           PIC 9(05).
           03  KAISU-KOHI3-D           PIC 9(05).
           03  KAISU-KOHI4-D           PIC 9(05).
           03  KAISU-KOHI1-M           PIC 9(05).
           03  KAISU-KOHI2-M           PIC 9(05).
           03  KAISU-KOHI3-M           PIC 9(05).
           03  KAISU-KOHI4-M           PIC 9(05).
      *
       01  HOKEN-KAISU-TABLE.
           03  HOKEN-KAISU-TBL.
             04 HOKEN-KAISU-TBL2       OCCURS  32.
               05  KOHI1-TBL           PIC 9(02).
               05  KOHI2-TBL           PIC 9(02).
               05  KOHI3-TBL           PIC 9(02).
               05  KOHI4-TBL           PIC 9(02).
           03  HOKEN-KAISU-TBL-R   REDEFINES   HOKEN-KAISU-TBL.
             04 HOKEN-KAISU-TBL2-R     OCCURS  32.
               05  KOHI-TBL            PIC 9(02)   OCCURS   4.
      *
      *    ¸øÈñ¤Î´µ¼ÔÉéÃ´ºÑ¤ß¶â³Û¤Î²Ã»»ÍÑ¥¨¥ê¥¢
       01  HOKEN-FUTAN-AREA.
           03  FTN-KOHI1-D             PIC 9(08).
           03  FTN-KOHI2-D             PIC 9(08).
           03  FTN-KOHI3-D             PIC 9(08).
           03  FTN-KOHI4-D             PIC 9(08).
           03  FTN-KOHI1-D-K           PIC 9(08).
           03  FTN-KOHI2-D-K           PIC 9(08).
           03  FTN-KOHI3-D-K           PIC 9(08).
           03  FTN-KOHI4-D-K           PIC 9(08).
           03  FTN-KOHI1-M             PIC 9(08).
           03  FTN-KOHI2-M             PIC 9(08).
           03  FTN-KOHI3-M             PIC 9(08).
           03  FTN-KOHI4-M             PIC 9(08).
           03  FTN-KOHI1-M-JI          PIC 9(08).
           03  FTN-KOHI2-M-JI          PIC 9(08).
           03  FTN-KOHI3-M-JI          PIC 9(08).
           03  FTN-KOHI4-M-JI          PIC 9(08).
           03  FTN-CYOKI-M-JI          PIC 9(08).
      *
      *    ¸øÈñ¤Î¥ì¥»¥×¥ÈÉéÃ´¶èÊ¬»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç,
      *    ¥ì¥»µ­ºÜ¶â³Û¤Î²Ã»»ÍÑ¥¨¥ê¥¢
       01  HOKEN-FUTAN-AREA2.                                           
           03  FTN-KOHI1-D2            PIC 9(08).                       
           03  FTN-KOHI2-D2            PIC 9(08).                       
           03  FTN-KOHI3-D2            PIC 9(08).                       
           03  FTN-KOHI4-D2            PIC 9(08).                       
           03  FTN-KOHI1-D2-K          PIC 9(08).                       
           03  FTN-KOHI2-D2-K          PIC 9(08).                       
           03  FTN-KOHI3-D2-K          PIC 9(08).                       
           03  FTN-KOHI4-D2-K          PIC 9(08). 
           03  FTN-KOHI1-M2            PIC 9(08).                       
           03  FTN-KOHI2-M2            PIC 9(08).                       
           03  FTN-KOHI3-M2            PIC 9(08).                       
           03  FTN-KOHI4-M2            PIC 9(08).                       
           03  FTN-KOHI1-M2-JI         PIC 9(08).                       
           03  FTN-KOHI2-M2-JI         PIC 9(08).                       
           03  FTN-KOHI3-M2-JI         PIC 9(08).                       
           03  FTN-KOHI4-M2-JI         PIC 9(08).   
           03  FTN-CYOKI-M2-JI         PIC 9(08).
      *
      *    ¸øÈñ¤ÎÉéÃ´¶â³Û±ßÃ±°ÌÍÑ
       01  HOKEN-FUTAN-AREA3.                                     
           03  FTN-KOHI1-D3            PIC 9(08).                 
           03  FTN-KOHI2-D3            PIC 9(08).                 
           03  FTN-KOHI3-D3            PIC 9(08).                 
           03  FTN-KOHI4-D3            PIC 9(08).                 
           03  FTN-KOHI1-D3-K          PIC 9(08).                 
           03  FTN-KOHI2-D3-K          PIC 9(08).                 
           03  FTN-KOHI3-D3-K          PIC 9(08).                 
           03  FTN-KOHI4-D3-K          PIC 9(08).                 
           03  FTN-KOHI1-M3            PIC 9(08).                 
           03  FTN-KOHI2-M3            PIC 9(08).                 
           03  FTN-KOHI3-M3            PIC 9(08).                 
           03  FTN-KOHI4-M3            PIC 9(08).                 
           03  FTN-KOHI1-M3-JI         PIC 9(08).                 
           03  FTN-KOHI2-M3-JI         PIC 9(08).                 
           03  FTN-KOHI3-M3-JI         PIC 9(08).                 
           03  FTN-KOHI4-M3-JI         PIC 9(08).                 
           03  FTN-CYOKI-M3-JI         PIC 9(08).
      *
      *    ¸øÈñ¤Î´µ¼ÔÉéÃ´²ÄÇ½¤Ê»Ä³Û»»ÄêÍÑ¥¨¥ê¥¢
       01  HOKEN-ZAN-AREA.
           03  ZAN-KOHI1-D             PIC 9(08).
           03  ZAN-KOHI2-D             PIC 9(08).
           03  ZAN-KOHI3-D             PIC 9(08).
           03  ZAN-KOHI4-D             PIC 9(08).
           03  ZAN-KOHI1-M             PIC 9(08).
           03  ZAN-KOHI2-M             PIC 9(08).
           03  ZAN-KOHI3-M             PIC 9(08).
           03  ZAN-KOHI4-M             PIC 9(08).
           03  ZAN-KOHI-AREA.
             05  ZAN-KOHI1             PIC 9(08).
             05  ZAN-KOHI2             PIC 9(08).
             05  ZAN-KOHI3             PIC 9(08).
             05  ZAN-KOHI4             PIC 9(08).
           03  ZAN-KOHI-TBL  REDEFINES  ZAN-KOHI-AREA
                                        OCCURS  4.
             05  ZAN-KOHI-T            PIC 9(08).
      *
      *    ¸øÈñ¤Î¥ì¥»¥×¥ÈÉéÃ´¶èÊ¬»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç,
      *    ¥ì¥»µ­ºÜ²ÄÇ½¤Ê»Ä³Û»»ÄêÍÑ¥¨¥ê¥¢
       01  HOKEN-ZAN-AREA2.                                             
           03  ZAN-KOHI1-D2            PIC 9(08).                       
           03  ZAN-KOHI2-D2            PIC 9(08).                       
           03  ZAN-KOHI3-D2            PIC 9(08).                       
           03  ZAN-KOHI4-D2            PIC 9(08).                       
           03  ZAN-KOHI1-M2            PIC 9(08).                       
           03  ZAN-KOHI2-M2            PIC 9(08).                       
           03  ZAN-KOHI3-M2            PIC 9(08).                       
           03  ZAN-KOHI4-M2            PIC 9(08).                       
           03  ZAN-KOHI-AREA2.                                          
             05  ZAN-KOHI12            PIC 9(08).                       
             05  ZAN-KOHI22            PIC 9(08).                       
             05  ZAN-KOHI32            PIC 9(08).                       
             05  ZAN-KOHI42            PIC 9(08).                       
           03  ZAN-KOHI-TBL2 REDEFINES  ZAN-KOHI-AREA2                  
                                        OCCURS  4.                      
             05  ZAN-KOHI-T2           PIC 9(08).                       
      *
      *    ¸øÈñ¤ÎÉéÃ´¶â³Û±ßÃ±°ÌÍÑ
       01  HOKEN-ZAN-AREA3.                                       
           03  ZAN-KOHI1-D3            PIC 9(08).                 
           03  ZAN-KOHI2-D3            PIC 9(08).                 
           03  ZAN-KOHI3-D3            PIC 9(08).                 
           03  ZAN-KOHI4-D3            PIC 9(08).                 
           03  ZAN-KOHI1-M3            PIC 9(08).                 
           03  ZAN-KOHI2-M3            PIC 9(08).                 
           03  ZAN-KOHI3-M3            PIC 9(08).                 
           03  ZAN-KOHI4-M3            PIC 9(08).                 
           03  ZAN-KOHI-AREA3.                                          
             05  ZAN-KOHI13            PIC 9(08).                 
             05  ZAN-KOHI23            PIC 9(08).                 
             05  ZAN-KOHI33            PIC 9(08).                 
             05  ZAN-KOHI43            PIC 9(08).                 
           03  ZAN-KOHI-TBL3 REDEFINES  ZAN-KOHI-AREA3                  
                                        OCCURS  4.                      
             05  ZAN-KOHI-T3           PIC 9(08).                 
      *
      *    ¸øÈñ¤Î¥ì¥»¥×¥ÈÉéÃ´¶èÊ¬»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç,
      *    ¥ì¥»µ­ºÜ¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢
       01  WK-KOH-COMPFTN-AREA.                                         
           03  WK-KOH-COMPFTN          PIC 9(07)                        
                                        OCCURS  4.                      
      *
      *    ´µ¼ÔÇ¯Îð»»½ÐÍÑ¥¨¥ê¥¢
       01  WRK-NENREI-AREA.
           03  WRK-NENREI              PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1            PIC 9(04).
               05  WRK-MMS1            PIC 9(02).
               05  WRK-DDS1            PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2            PIC 9(04).
               05  WRK-MMS2            PIC 9(02).
               05  WRK-DDS2            PIC 9(02).
      *
      *    ÌôºÞ°ìÉôÉéÃ´¶â¡Ê¸øÈñÊÌ·×»»¡ËÂàÈòÎÎ°è
       01  WK-YAKFTN.
           03  WK-SYU-YAKFTN           PIC 9(08).
           03  WK-KOH-YAKFTN-AREA.
             05  WK-KOH1-YAKFTN        PIC 9(08).
             05  WK-KOH2-YAKFTN        PIC 9(08).
             05  WK-KOH3-YAKFTN        PIC 9(08).
             05  WK-KOH4-YAKFTN        PIC 9(08).
           03  WK-KOH-YAKFTN-TBL    REDEFINES  WK-KOH-YAKFTN-AREA
                                               OCCURS  4.
             05  WK-KOH-YAKFTN-T       PIC 9(08).
                                                
      *
      *    ÌôºÞ°ìÉôÉéÃ´¶â·×»»¥¨¥ê¥¢
       01  YAKFTN-AREA.
      *
      *        ¥Ñ¥é¥á¥¿¤«¤é¤Î¼õ¤±¼è¤êÍÑ¡ÊÊ£¿ôÊÝ¸±ÁÈ¹ç¤»ÍÑ¡Ë
           03  LNK-S02-TBL         OCCURS   40.
               07  LNK-S02-SRYKBN       PIC  X(02).
               07  LNK-S02-ZAIBAN       PIC  9(08).
               07  LNK-S02-SRYCD        PIC  X(09).
               07  LNK-S02-SURYOU       PIC  9(05)V9(03).
               07  LNK-S02-KAISUU       PIC  9(03).
               07  LNK-S02-TENSUU       PIC  9(07).
      *
      *        ¥Ñ¥é¥á¥¿¤«¤é¤Î¼õ¤±¼è¤êÍÑ
           03  YAKFTN-TABLE.
               05  YAKFTN-TBL-OCC       OCCURS  40.
                   07  YAKFTN-KEY.
                       09  YAKFTN-SRYKBN
                                          PIC X(02).
                       09  YAKFTN-SRYCD   PIC X(09).
                   07  YAKFTN-ZAIBAN      PIC 9(08).
                   07  YAKFTN-TENSUU      PIC 9(07).
                   07  YAKFTN-KAISUU-TABLE.
                       09  YAKFTN-KAISUU  PIC 9(02)  OCCURS 180.
                   07  YAKFTN-MAX         PIC 9(03).
           03  YAKFTN-TABLE2.
               05  YAKFTN-TBL-OCC2       OCCURS  40.
                   07  YAKFTN-KEY2.
                       09  YAKFTN-SRYKBN2
                                          PIC X(02).
                       09  YAKFTN-SRYCD2   PIC X(09).
                       09  YAKFTN-ZAIBAN2  PIC 9(08).
                   07  YAKFTN-TENSUU2      PIC 9(07).
                   07  YAKFTN-KAISUU-TABLE2.
                       09  YAKFTN-KAISUU2  PIC 9(02)  OCCURS 180.
                   07  YAKFTN-MAX2         PIC 9(03).
      *
      *        ºÞÈÖ¹æÊÌÌôºÞ¿ô·×»»ÍÑ
           03  YAKFTN-ZAI-TABLE.
               05  YAKFTN-ZAI-TBL-OCC   OCCURS  40.
                   07  YAKFTN-ZAI-SRYKBN
                                        PIC X(02).
                   07  YAKFTN-ZAI-SRYCD PIC X(09).
                   07  YAKFTN-ZAI-ZAIBAN
                                        PIC 9(08).
                   07  YAKFTN-ZAI-TENSUU
                                        PIC 9(07).
                   07  YAKFTN-ZAI-KAISUU-TABLE.
                       09  YAKFTN-ZAI-KAISUU
                                        PIC 9(02)    OCCURS 180.
                   07  YAKFTN-ZAI-MAX   PIC 9(03).
      *        °ìÆü½èÊýÊ¬¤ÎÌôºÞ·×»»ÍÑ
           03  YAKFTN-TOTAL-TABLE.
               05  YAKFTN-TOTAL-TBL-OCC   OCCURS   3.
                   07  YAKFTN-TOTAL-KAISUU  PIC  9(2)  OCCURS  180.
      *
      *        ÊÂ¤ÓÂØ¤¨ÍÑ¥ï¡¼¥¯
           03  WK-SRT-YAKFTN-TABLE.
               05  WK-SRT-YAKFTN-KEY.
                   07  WK-SRT-YAKFTN-SRYKBN     PIC X(02).
                   07  WK-SRT-YAKFTN-SRYCD      PIC X(09).
               05  WK-SRT-YAKFTN-ZAIBAN         PIC 9(08).
               05  WK-SRT-YAKFTN-TENSUU         PIC 9(07).
               05  WK-SRT-YAKFTN-KAISUU         PIC 9(02)  OCCURS 180.
               05  WK-SRT-YAKFTN-MAX            PIC 9(03).
      *
           03  WK-KEY-SRY.
               05  WK-KEY-SRYKBN          PIC X(02).
               05  WK-KEY-SRYCD           PIC X(09).
           03  WK-KEY-ZAIBAN              PIC 9(08).  
      *
      *        ÉþÍÑ´ü´ÖÊÔ½¸ÍÑ¥ï¡¼¥¯
           03  WK-KAISUU-TABLE.
               05  WK-KAISUU           PIC 9(02)    OCCURS 180.
           03  WK-ZAI-KAISUU-TABLE.
               05  WK-ZAI-KAISUU       PIC 9(02)    OCCURS 180.
           03  WK-KINGAK               PIC 9(07).
           03  YAKFTN-SOGOKEI          PIC 9(07).
           03  WK-FUTAN                PIC 9(05).
           03  YAKFTN-JOUGEN           PIC 9(07).
      *
      *    ÊÝ¸±ÊÑ¹¹»þ¤Î·ÑÂ³ÆþÎÏÍÑ¤ÎÌôºÞ°ìÉôÉéÃ´¶âÎÎ°è
           03  WK-CONT-YAKFTNAREA.
               05  WK-CONT-YAKFTNKIN1  PIC 9(07).
               05  WK-CONT-YAKFTNKIN2  PIC 9(07).
      *
           03  WK-KBNBETSU-YAKUZAI.
               05  WK-TOYAKU-TENSU     PIC 9(10)    OCCURS  4.
           03  WK-TOYAKU-ZAIBAN-AREA   OCCURS  40.
               05  WK-TOYAKU-SRYKBN    PIC X(02).
               05  WK-TOYAKU-ZAIBAN    PIC 9(08).
               05  WK-TOYAKU-ZAITEN    PIC 9(07).
           03  WK-NAI-FUTAN            PIC 9(05).
           03  WK-TON-FUTAN            PIC 9(05).
           03  WK-GAI-FUTAN            PIC 9(05).
      *
      *    Åº¤¨»úÎÎ°è
       01  IDX-AREA.
           03  IDX                     PIC  9(02)  VALUE  ZERO.
           03  IDY                     PIC  9(02)  VALUE  ZERO.
           03  IDZ                     PIC  9(02)  VALUE  ZERO.
           03  IDX2X                   PIC  X(02).
           03  IDX2R                   REDEFINES    IDX2X.
               05  IDX2                PIC  9(02).
           03  IDX3                    PIC  9(02).
           03  IDX4                    PIC  9(03).
           03  IDX5                    PIC  9(03).
           03  IDX6                    PIC  9(03).
           03  IDX7                    PIC  9(03).
           03  IDX8                    PIC  9(03).
           03  IDX9                    PIC  9(03).
           03  IDX10                   PIC  9(03).
           03  Y-IDX                   PIC  9(03)  VALUE  ZERO.
           03  Y-IDX2                  PIC  9(03)  VALUE  ZERO.
           03  Y-IDX3                  PIC  9(03)  VALUE  ZERO.
           03  Y-IDX4                  PIC  9(03)  VALUE  ZERO.
           03  Y-IDX5                  PIC  9(03)  VALUE  ZERO.
           03  HKNGAI-IDX              PIC  9(02)  VALUE  ZERO.
           03  DSP-IDX                 PIC  9(02)  VALUE  ZERO.
           03  DSP-IDX2                PIC  9(02)  VALUE  ZERO.
           03  KOH-IDX                 PIC  9(02)  VALUE  ZERO.
           03  GEN-IDX                 PIC  9(02).
      *
      *Ê¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ
      *¹âÎð¼Ô¤ÎºßÂðÁí¹ç¡¦ºßÂðËö´ü»»Äê´µ¼Ô¤ÎÉéÃ´¾å¸Â»»Äê¥¨¥ê¥¢
      *
       01  WK-SYUHKN-TAIHI-AREA.
           03  MAX-KIN-SYU-M           PIC 9(08).
           03  FTN-SYU-M               PIC 9(08).
           03  ZAN-SYU-M               PIC 9(08).
           03  FLG-SANTEI-SYU          PIC X(01).
           03  WK-SYU-SYUCOMPFTN       PIC 9(08).                       
           03  FLG-ZAITAKU             PIC 9(01).                       
           03  FLG-KOUREI-KBN          PIC X(01).                       
      *
           03  FTN-KOHI1-D-ENTANI      PIC 9(08).                       
           03  FTN-KOHI1-M-ENTANI      PIC 9(08).                       
           03  ZAN-KOHI1-D-ENTANI      PIC 9(08).                       
           03  ZAN-KOHI1-M-ENTANI      PIC 9(08).                       
           03  ZAN-KOHI1-ENTANI        PIC 9(08).                       
      *
           03  FTN-SYU-M-ENTANI        PIC 9(08).                       
           03  ZAN-SYU-M-ENTANI        PIC 9(08).                       
      *
           03  FLG-SANTEI1-ENTANI      PIC X(01).                       
           03  FLG-SANTEI-SYU-ENTANI   PIC X(01).                       
      *
           03  FLG-ROUJINHOKEN         PIC X(01).
      *
           03  FLG-KOUGAKUCYO2         PIC 9(01).
           03  FLG-KOUGAKUCYO-JIRITU   PIC 9(01).
           03  FLG-KOUGAKUCYO-ININ     PIC 9(01).
           03  FLG-KOUGAKUCYO-GEN70    PIC 9(01).
           03  FLG-KOUGAKUCYO-GEN70K   PIC 9(01).
           03  FLG-KOUGAKUCYO-OITA     PIC 9(01).
           03  FLG-IRYOHI              PIC 9(01).
           03  FLG-IRYOHI2             PIC 9(01).
           03  FLG-IRYOHI70            PIC 9(02).
           03  FLG-IRYOHI70K           PIC 9(02).
           03  WK-IRYOHI               PIC 9(08).
           03  WK-IRYOHI2              PIC 9(08).
           03  WK-KOUREI-IRYOHI70      PIC 9(08).
           03  WK-KOUREI-IRYOHI70K     PIC 9(08).
           03  WK-KOUREI-IRYOHI70X     PIC 9(08).
      *
      *ÃÏÊý¸øÈñÍÑ¥Õ¥é¥°¡Ê¸øÈñ¸ÂÅÙ³Û·èÄêÍÑ¡Ë
       01  WK-CHIHOU-FLG-TAIHI-AREA.
           03  FLG-CHIHOU              PIC X(01)  OCCURS  4.
      *
      *½þ´ÔÊ§¤¤ÃÏÊý¸øÈñ¥Õ¥é¥°
       01  WK-SYOUKAN-FLG-TAIHI-AREA.
           03  FLG-SYOUKAN             PIC X(01)  OCCURS  4.
      *
      *¼çÊÝ¸±ÉéÃ´¶â³Û¤Î¸øÈñ¸ÂÅÙ³ÛÂÐ±þÍÑ¥¨¥ê¥¢
       01  WK-SYUHEN-TAIHI-AREA.
           03  FLG-SYUHEN              PIC X(01).
      *
           03  FLG-SANTEI1-HEN         PIC X(01).
           03  FLG-SANTEI-SYU-HEN      PIC X(01).
      *
           03  FTN-KOHI1-D-HEN         PIC 9(08).
           03  FTN-KOHI1-M-HEN         PIC 9(08).
           03  ZAN-KOHI1-D-HEN         PIC 9(08).
           03  ZAN-KOHI1-M-HEN         PIC 9(08).
           03  ZAN-KOHI1-HEN           PIC 9(08).
      *
           03  FTN-SYU-M-HEN           PIC 9(08).
           03  ZAN-SYU-M-HEN           PIC 9(08).
      *
           03  WK-HEN-SYUCOMPFTN       PIC 9(07).
           03  WK-HEN-KOH1COMPFTN      PIC 9(07).
      *
      *
           03  FLG-SANTEI1-HEN-ENTANI         PIC X(01).
           03  FLG-SANTEI-SYU-HEN-ENTANI      PIC X(01).
      *
           03  FTN-KOHI1-D-HEN-ENTANI         PIC 9(08).
           03  FTN-KOHI1-M-HEN-ENTANI         PIC 9(08).
           03  ZAN-KOHI1-D-HEN-ENTANI         PIC 9(08).
           03  ZAN-KOHI1-M-HEN-ENTANI         PIC 9(08).
           03  ZAN-KOHI1-HEN-ENTANI           PIC 9(08).
      *
           03  FTN-SYU-M-HEN-ENTANI           PIC 9(08).
           03  ZAN-SYU-M-HEN-ENTANI           PIC 9(08).
      *
           03  WK-HEN-SYUCOMPFTN-ENTANI       PIC 9(07).
           03  WK-HEN-KOH1COMPFTN-ENTANI      PIC 9(07).
      *
      *
      *    ºßÁí¿ÇÈ½ÄêÍÑ¥Æ¡¼¥Ö¥ë¡Ê£²£°£°£¶Ç¯£³·î¤Þ¤Ç¡Ë
       01  TBL-ZAITAKU-AREA.
           03  FILLER          PIC X(09)   VALUE    "114701210".
           03  FILLER          PIC X(09)   VALUE    "114701710".
           03  FILLER          PIC X(09)   VALUE    "114007610".
           03  FILLER          PIC X(09)   VALUE    "114007710".
      *
       01  TBL-ZAITAKU-R       REDEFINES   TBL-ZAITAKU-AREA.
           03  TBL-ZAITAKU     PIC X(09)   OCCURS    4.
      *
       01  TBL-ZAITAKU-MAX     PIC 9(02)   VALUE     4.
      *
      *    ºßÁí¿ÇÈ½ÄêÍÑ¥Æ¡¼¥Ö¥ë¡Ê£²£°£°£¶Ç¯£´·î¤«¤é¡Ë
       01  TBL-ZAITAKU2-AREA.
           03  FILLER          PIC X(09)   VALUE    "114007610".
           03  FILLER          PIC X(09)   VALUE    "114007710".
           03  FILLER          PIC X(09)   VALUE    "114012210".
           03  FILLER          PIC X(09)   VALUE    "114012310".
           03  FILLER          PIC X(09)   VALUE    "114012410".
           03  FILLER          PIC X(09)   VALUE    "114007510".
      *
       01  TBL-ZAITAKU2-R      REDEFINES   TBL-ZAITAKU2-AREA.
           03  TBL-ZAITAKU2    PIC X(09)   OCCURS    6.
      *
       01  TBL-ZAITAKU2-MAX    PIC 9(02)   VALUE     6.
      *
      *    ¸º³Û¡¦ÌÈ½ü¡¦»ÙÊ§Í±Í½È½ÄêÍÑ¥Æ¡¼¥Ö¥ë
       01  TBL-GENGAKU-AREA.
           03  FILLER          PIC X(03)   VALUE    "960".
           03  FILLER          PIC X(03)   VALUE    "961".
           03  FILLER          PIC X(03)   VALUE    "962".
           03  FILLER          PIC X(03)   VALUE    "963".
      *
       01  TBL-GENGAKU-R       REDEFINES   TBL-GENGAKU-AREA.
           03  TBL-GENGAKU     PIC X(03)   OCCURS    4.
      *
       01  TBL-GENGAKU-MAX     PIC 9(02)   VALUE     4.
      *
      *    £·£°ºÐÌ¤Ëþ¸½ÊªµëÉÕÈ½ÄêÍÑ¥Æ¡¼¥Ö¥ë
       01  TBL-GENBUTU70-AREA.
           03  FILLER          PIC X(03)   VALUE    "966".
           03  FILLER          PIC X(03)   VALUE    "967".
      *
       01  TBL-GENBUTU70-R     REDEFINES   TBL-GENBUTU70-AREA.
           03  TBL-GENBUTU70   PIC X(03)   OCCURS    2.
      *
       01  TBL-GENBUTU70-MAX   PIC 9(02)   VALUE     2.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼
       01  SYU-SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    Äã½êÆÀ¼ÔÍúÎò
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    Ç¯¶âÍúÎò
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *    ¥ï¡¼¥¯ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  WRK-HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC"
                               REPLACING  //COMB-// BY //WRK-COMB-//.
      *
      *    ÊÝ¸±ÈÖ¹æ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *    ¥ï¡¼¥¯ÊÝ¸±ÈÖ¹æ
       01  WRK-HKNNUM-REC.
           COPY    "CPHKNNUM.INC"
                               REPLACING  //HKN-// BY //WRK-HKN-//.
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSK1001.INC".
           COPY    "CPSK1026.INC".
           COPY    "CPSK1027.INC".
           COPY    "CPSK1037.INC".
           COPY    "CPSK4001.INC".
           COPY    "CPSK4003.INC".
      *
      *    ´µ¼Ô¾ðÊó
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    ¸øÈñÉÕ²Ã¾ðÊó¼èÆÀ¥µ¥Ö
           COPY    "CPORCSKOHPLUS.INC".
      *
      *    ¸øÈñ·î¾å¸Â³Û¼èÆÀ¥µ¥Ö
           COPY    "CPORCSJGNGET.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      /
      ***
       LINKAGE             SECTION.
           COPY  "MCPAREA".
           COPY  "CPORCS01.INC".
       01  LNK-SYU-SYUNOU-REC.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-WRK-SYUNOU-REC-T.
        02 LNK-WRK-SYUNOU-REC      OCCURS 15.
           COPY  "CPSYUNOU.INC" 
                               REPLACING  //SYU-// BY //LNK-WRK-SYU-//.
      /
       PROCEDURE           DIVISION  USING
                                     MCPAREA
                                     LNK-ORCS01-REC
                                     LNK-SYU-SYUNOU-REC
                                     LNK-WRK-SYUNOU-REC-T.
      *
      ******************************************************************
      *        °ÍÍêÀ©¸æ½èÍý
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
      *
      ***  MOVE    ZERO            TO  LNK-S01-RC.
           MOVE    LNK-S01-IDX     TO  IDY.
           MOVE    SPACE           TO  KEISAN-AREA.
           INITIALIZE                  KEISAN-AREA.
           MOVE    SPACE           TO  WRK-AREA.
           INITIALIZE                  WRK-AREA.
           MOVE    ZERO            TO  LNK-SYU-RJN-FTN
                                       LNK-SYU-KOH-FTN
                                       LNK-SYU-KOH-FTN-ENTANI
                                       LNK-SYU-YKZ-FTN
                                       LNK-SYU-HKNTEKMONEY.
           MOVE    ZERO            TO  LNK-SYU-SYUHKNFTNMONEY
                                       LNK-SYU-SYUCOMPFTN
                                       LNK-SYU-SYUCOMPFTN-ENTANI
                                       LNK-SYU-SYUCOMPYKZFTN
                                       LNK-SYU-SYUCOMPTOTAL.
           MOVE    SPACE    TO  LNK-SYU-SYUYKZFTNKBN.
           INITIALIZE    WK-SYUHKN-TAIHI-AREA.
           PERFORM   VARYING   IDX   FROM   1    BY   1
             UNTIL   IDX   >   4
                  MOVE    ZERO     TO  LNK-SYU-KOH-HKNFTNMONEY(IDX)
                                       LNK-SYU-KOH-COMPFTN(IDX)
                                       LNK-SYU-KOH-COMPFTN-ENTANI(IDX)
                                       LNK-SYU-KOH-COMPYKZFTN(IDX)
                                       LNK-SYU-KOH-COMPTOTAL(IDX)
                  MOVE    SPACE    TO  LNK-SYU-KOH-YKZFTNKBN(IDX)
                                       
           END-PERFORM.
           MOVE    ZERO            TO  LNK-SYU-TGMONEY-TAX-SAI
                                       LNK-SYU-RSI-TAX-SAI
                                       LNK-SYU-RSI-TOTAL
                                       LNK-SYU-RSIJIBAI-SKYMONEY
                                       LNK-SYU-SKYMONEY-TAX-SAI.
           MOVE    SPACE           TO  LNK-SYU-ZAITAKU.
           MOVE    SPACE           TO  LNK-SYU-DISCOUNT-KBN
                                       LNK-SYU-DISCOUNT-BODY
                                       LNK-SYU-DISCOUNT-RATEKBN
                                       LNK-SYU-DISCOUNT-TEIRITU.
           MOVE    ZERO            TO  LNK-SYU-DISCOUNT-RATE.
           MOVE    ZERO            TO  LNK-SYU-DISCOUNT-MONEY.
           MOVE    ZERO            TO  LNK-SYU-NYNSHOKAISU.
           MOVE    SPACE           TO  LNK-SYU-KOHTAIYKZKBN.
           MOVE    ZERO            TO  LNK-SYU-YKZKENNUM.
      *
           PERFORM   VARYING   IDX   FROM   1    BY   1
             UNTIL   IDX   >   10
                  MOVE    ZERO     TO  LNK-SYU-SHOKUJI(IDX)
                                       LNK-SYU-SHOKUJI-NISSU(IDX)
           END-PERFORM.              
           MOVE    ZERO            TO  LNK-SYU-JYO-HKNFTNMONEY.
           MOVE    ZERO            TO  LNK-SYU-JYO-COMPFTN.
           MOVE    ZERO            TO  LNK-SYU-JYO-COMPFTN-ENTANI.
           MOVE    ZERO            TO  LNK-SYU-TAX-TAISHOU.
      *
           MOVE    ZERO            TO  FLG-SEISINJYO
                                       FLG-TOKUREI-HKNJYA.
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý£±£°£°£±¤Î¼è¤ê¹þ¤ß
           PERFORM    SYS1001-READ-SEC.
      *
           MOVE   SYS-1001-DISCOUNT-HASU-KBN  TO   WK-HASU1001-KBN.
           MOVE   SYS-1001-DISCOUNT-HASU-KBN2 TO   WK-HASU1001-KBN2.
           MOVE   SYS-1001-TAX-HASU-KBN       TO   WK-TAX1001-KBN.
           MOVE   SYS-1001-PREFNUM            TO   WK-1001-PREFNUM.
           MOVE   SYS-1001-HOSPID             TO   WK-1001-HOSPID.
           MOVE   SYS-1001-HOSPSBT            TO   WK-1001-HOSPSBT.
           MOVE   SYS-1001-ROUPAYKBN          TO   WK-1001-ROUPAYKBN.
           MOVE   SYS-1001-JIHIPOSITION       TO   WK-1001-JIHIPOSITION.
           MOVE   SYS-1001-CHIHOUNUMKBN       TO   WK-1001-CHIHOUNUMKBN.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¤Î¸¡º÷
           INITIALIZE                           HKNCOMBI-REC.
           MOVE    LNK-SYU-HOSPID       TO      COMB-HOSPID.
           MOVE    LNK-SYU-PTID         TO      COMB-PTID.
           MOVE    LNK-SYU-HKNCOMBINUM  TO      COMB-HKNCOMBINUM.
           MOVE    HKNCOMBI-REC         TO      MCPDATA-REC.
           MOVE    "DBSELECT"           TO      MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"       TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC.
           IF      MCP-RC               =       ZERO
               MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH 
               PERFORM   HKNCOMBI-READ-SEC
               IF      FLG-HKNCOMBI     =       1
                   DISPLAY "HKNCOMBI DBFETCH ERR"
                   MOVE     10          TO      LNK-S01-RC
                   MOVE    "DBCLOSECURSOR"  TO      MCP-FUNC
                   MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
                   CALL    "ORCMCPSUB"      USING   MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                   GO   TO              MAIN-EXT
               END-IF
           ELSE
               MOVE    1                TO      FLG-HKNCOMBI
               DISPLAY "HKNCOMBI DBSELECT ERR"
               MOVE     10              TO      LNK-S01-RC
               MOVE    "DBCLOSECURSOR"  TO      MCP-FUNC
               MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
               CALL    "ORCMCPSUB"      USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               GO   TO                  MAIN-EXT
           END-IF.
           MOVE    "DBCLOSECURSOR"      TO      MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"       TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¤Î»²¾ÈÉôÊ¬³ÎÄê
           PERFORM   100-KAKUTEI-SEC.
      *
      *    ¼«ÈñÊÝ¸±¤Î¥Ç¡¼¥¿ÊÔ½¸
           MOVE      ZERO    TO    WK-SKYMONEY-ZEI.
           PERFORM   104-JIHIHOKEN-DATA-HENSYU-SEC.
      *    ÊÝ¸±ÁÈ¹ç¤»¤Î¾å¸Â¶èÊ¬ÊÔ½¸
           PERFORM   105-JYOGENKBN-HENSYU-SEC.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¤«¤é¼çÊÝ¸±¤ÎÊÝ¸±¼ïÎà¤òÈ½Äê
      *    ¡Ê¶õÇò¡§°åÊÝ¡¢£±¡§Ï«ºÒ¡¢£²¡§¼«Çå¡Ë
           MOVE     SPACE               TO       WK-HKNKBN.
           EVALUATE    COMB-SYU-HKNKOHSBTKBN
               WHEN    "4"
                       MOVE    "1"      TO       WK-HKNKBN
           END-EVALUATE.
      *
      *    ÌôºÞ°ìÉôÉéÃ´¶â¤Î·×»»½èÍý
           MOVE         ZERO              TO     LNK-SYU-YKZ-FTN.
           MOVE         ZERO              TO     LNK-SYU-RESE-YKZ-FTN.
           IF    LNK-SYU-SRYYMD(1:6)   <   "200304"
                PERFORM      YAKFTN-SANTEI-SEC
           ELSE
                MOVE      SPACE       TO      YAKFTN-AREA
                INITIALIZE                    YAKFTN-AREA
           END-IF.
           MOVE         YAKFTN-SOGOKEI    TO     LNK-SYU-RESE-YKZ-FTN.
           EVALUATE     FLG-SANSHOU
               WHEN     "1"
                   IF  COMB-GAI-YKZFTNKBN      NOT =   "0"
                       MOVE   YAKFTN-SOGOKEI   TO  LNK-SYU-RESE-YKZ-FTN
                       IF     COMB-GAI-YKZFTNKBN     =   "1"
                            MOVE   YAKFTN-SOGOKEI  TO  LNK-SYU-YKZ-FTN
                       END-IF
      *             ELSE
      *                 MOVE   YAKFTN-SOGOKEI   TO  LNK-SYU-RESE-YKZ-FTN
                   END-IF
               WHEN     "2"
                   IF  COMB-TSY-GAIYKZFTNKBN   NOT =   "0"
                       MOVE   YAKFTN-SOGOKEI   TO  LNK-SYU-RESE-YKZ-FTN
                       IF   COMB-TSY-GAIYKZFTNKBN  =   "1"
                            MOVE   YAKFTN-SOGOKEI  TO  LNK-SYU-YKZ-FTN
                       END-IF
      *             ELSE
      *                 MOVE   YAKFTN-SOGOKEI   TO  LNK-SYU-RESE-YKZ-FTN
                   END-IF
               WHEN     "3"
                   IF  COMB-TNK-GAIYKZFTNKBN   NOT =   "0"
                       MOVE   YAKFTN-SOGOKEI   TO  LNK-SYU-RESE-YKZ-FTN
                       IF   COMB-TNK-GAIYKZFTNKBN  =   "1"
                            MOVE   YAKFTN-SOGOKEI  TO  LNK-SYU-YKZ-FTN
                       END-IF
      *             ELSE
      *                 MOVE   YAKFTN-SOGOKEI   TO  LNK-SYU-RESE-YKZ-FTN
                   END-IF
           END-EVALUATE.
      *
      *    ´µ¼ÔÉéÃ´³Û¤Î»»Äê
           PERFORM       200-HKNFTN-SANTEI-SEC
           EVALUATE      FLG-SANSHOU
               WHEN      "1"
                   IF   (COMB-GAI-FTNKBN       =    "2")    AND
                        (COMB-GAI-YKZFTNKBN    =    "2")
                         MOVE   ZERO        TO   WK-KNJFTNKIN
                                                 WK-KNJFTNKIN-ENTANI
                                                 LNK-SYU-SKYMONEY
                   END-IF
               WHEN      "2"
                   IF   (COMB-TSY-GAIFTNKBN    =    "2")    AND
                        (COMB-TSY-GAIYKZFTNKBN =    "2")
                         MOVE   ZERO        TO   WK-KNJFTNKIN
                                                 WK-KNJFTNKIN-ENTANI
                                                 LNK-SYU-SKYMONEY
                   END-IF
               WHEN      "3"
                   IF   (COMB-TNK-GAIFTNKBN    =    "2")    AND
                        (COMB-TNK-GAIYKZFTNKBN =    "2")
                         MOVE   ZERO        TO   WK-KNJFTNKIN
                                                 WK-KNJFTNKIN-ENTANI
                                                 LNK-SYU-SKYMONEY
                   END-IF
           END-EVALUATE.
           EVALUATE      FLG-SANSHOU
               WHEN      "1"
                   IF    COMB-GAI-YKZFTNKBN    =    "0"   OR  "3"
                         MOVE   ZERO        TO      LNK-SYU-YKZ-FTN
                   END-IF
               WHEN      "2"
                   IF    COMB-TSY-GAIYKZFTNKBN    =    "0"   OR  "3"
                         MOVE   ZERO        TO      LNK-SYU-YKZ-FTN
                   END-IF
               WHEN      "3"
                   IF    COMB-TNK-GAIYKZFTNKBN    =    "0"   OR  "3"
                         MOVE   ZERO        TO      LNK-SYU-YKZ-FTN
                   END-IF
           END-EVALUATE.
      *
      *    »»½ÐÀÁµá³Û¤Î£±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ¡ÊÌôºÞ°ìÉôÉéÃ´¶â¤â´Þ¤à¡Ë
      *    ¢¨Ï«ºÒÊÝ¸±¤Î¾ì¹ç¤Ï»Í¼Î¸ÞÆþ¤ò¹Ô¤ï¤Ê¤¤
           IF     (LNK-SYU-SKYMONEY      NOT =   ZERO)   OR
                  (LNK-SYU-YKZ-FTN       NOT =   ZERO)
               IF   WK-HKNKBN            NOT =   "1"
                 IF   FLG-KUMAMOTO           =   "1"
                 OR   COMB-SYU-HKNKOHSBTKBN  =   "8"
                 OR   FLG-KOUGAKUCYO2        =    1 
                 OR   FLG-KOUGAKUCYO-JIRITU  =    1
                 OR   FLG-KOUGAKUCYO-ININ    =    1
                 OR   FLG-KOUGAKUCYO-GEN70   =    1
                 OR   FLG-KOUGAKUCYO-GEN70K  =    1
                 OR   FLG-KOUGAKUCYO-OITA    =    1
                   COMPUTE  LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY  +
                                               LNK-SYU-YKZ-FTN
                 ELSE
                   COMPUTE  LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY  +
                                               LNK-SYU-YKZ-FTN
                   COMPUTE  LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY  +  5
                   COMPUTE  WK-SEIKYUGK      = LNK-SYU-SKYMONEY  /  10
                   COMPUTE  LNK-SYU-SKYMONEY = WK-SEIKYUGK       *  10
                 END-IF
               ELSE
                   COMPUTE  LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY  +
                                               LNK-SYU-YKZ-FTN
               END-IF 
           END-IF.
      *
           IF   WK-HKNKBN  =   "1"
              IF     LNK-SYU-YKZ-FTN    NOT =    ZERO
                   COMPUTE  LNK-SYU-RSIJIBAI-SKYMONEY =
                            LNK-SYU-RSIJIBAI-SKYMONEY +
                            LNK-SYU-YKZ-FTN
              END-IF
           END-IF.
      *
      *    ¼çÊÝ¸±¡¦Ï·¿Í¡¦¸øÈñ¤ÎÈ½ÊÌ
      *      ¡Ê¼çÊÝ¸±¡Ë
           IF     (LNK-SYU-SYUHKNNUM     NOT =   SPACE)    AND
                  (LNK-SYU-KOH1HKNNUM    =       SPACE)    AND
                  (LNK-SYU-KOH2HKNNUM    =       SPACE)    AND
                  (LNK-SYU-KOH3HKNNUM    =       SPACE)    AND
                  (LNK-SYU-KOH4HKNNUM    =       SPACE)
                   MOVE   LNK-SYU-SKYMONEY   TO   LNK-SYU-HKNTEKMONEY
                   IF     LNK-SYU-YKZ-FTN    NOT =   ZERO
                          COMPUTE  LNK-SYU-HKNTEKMONEY  =
                                   LNK-SYU-HKNTEKMONEY  -
                                   LNK-SYU-YKZ-FTN
                   END-IF
           END-IF.
      *       ¡ÊÏ·¿Í¡Ë 
      *
           IF     (LNK-SYU-KOH1HKNNUM    =   SPACE)    AND
                  (LNK-SYU-KOH2HKNNUM    =   SPACE)    AND
                  (LNK-SYU-KOH3HKNNUM    =   SPACE)    AND
                  (LNK-SYU-KOH4HKNNUM    =   SPACE)
                   CONTINUE
           ELSE
               IF      COMB-KOH1-HKNKOHSBTKBN    =       "3"
                  IF  (COMB-KOH2-HKNKOHSBTKBN    =     SPACE)  AND
                      (COMB-KOH3-HKNKOHSBTKBN    =     SPACE)  AND
                      (COMB-KOH4-HKNKOHSBTKBN    =     SPACE)
                       MOVE   LNK-SYU-SKYMONEY   TO   LNK-SYU-RJN-FTN
                  ELSE
      *            ¡Ê¸øÈñ¡Ë
                       MOVE   LNK-SYU-SKYMONEY   TO   LNK-SYU-KOH-FTN
                       IF     LNK-SYU-YKZ-FTN    NOT =   ZERO
                           IF   LNK-SYU-KOH-FTN  >    LNK-SYU-YKZ-FTN
                               COMPUTE  LNK-SYU-KOH-FTN  =
                                        LNK-SYU-KOH-FTN  -
                                        LNK-SYU-YKZ-FTN
                           ELSE
                               MOVE     ZERO     TO   LNK-SYU-KOH-FTN
                           END-IF               
                       END-IF
                       MOVE    WK-KNJFTNKIN-ENTANI
                                          TO   LNK-SYU-KOH-FTN-ENTANI
                  END-IF
               ELSE
      *            ¡Ê¸øÈñ¡Ë
                       MOVE   LNK-SYU-SKYMONEY   TO   LNK-SYU-KOH-FTN
                       IF     LNK-SYU-YKZ-FTN    NOT =   ZERO
                           IF   LNK-SYU-KOH-FTN  >    LNK-SYU-YKZ-FTN
                               COMPUTE  LNK-SYU-KOH-FTN  =
                                        LNK-SYU-KOH-FTN  -
                                        LNK-SYU-YKZ-FTN
                           ELSE
                               MOVE     ZERO     TO   LNK-SYU-KOH-FTN
                           END-IF               
                       END-IF
                       MOVE    WK-KNJFTNKIN-ENTANI
                                          TO   LNK-SYU-KOH-FTN-ENTANI
               END-IF
           END-IF.
      *
      *    ¼«Èñ¶â³Û¤Î¾ÃÈñÀÇ·×»»
           COMPUTE   LNK-SYU-JIHI-TOTAL =  (LNK-SYU-JIHI-TAXNASI(1)  +
                                            LNK-SYU-JIHI-TAXNASI(2)  +
                                            LNK-SYU-JIHI-TAXNASI(3)  +
                                            LNK-SYU-JIHI-TAXNASI(4)  +
                                            LNK-SYU-JIHI-TAXNASI(5)  +
                                            LNK-SYU-JIHI-TAXNASI(6)  +
                                            LNK-SYU-JIHI-TAXNASI(7)  +
                                            LNK-SYU-JIHI-TAXNASI(8)  +
                                            LNK-SYU-JIHI-TAXNASI(9)  +
                                            LNK-SYU-JIHI-TAXNASI(10)).
           COMPUTE  LNK-SYU-JIHI-TOTAL-TAX = (LNK-SYU-JIHI-TAXARI(1) +
                                              LNK-SYU-JIHI-TAXARI(2) +
                                              LNK-SYU-JIHI-TAXARI(3) +
                                              LNK-SYU-JIHI-TAXARI(4) +
                                              LNK-SYU-JIHI-TAXARI(5) +
                                              LNK-SYU-JIHI-TAXARI(6) +
                                              LNK-SYU-JIHI-TAXARI(7) +
                                              LNK-SYU-JIHI-TAXARI(8) +
                                              LNK-SYU-JIHI-TAXARI(9) +
                                              LNK-SYU-JIHI-TAXARI(10)).
      *
      *    ÊÝ¸±Å¬ÍÑ³°¶â³Û¤Î²Ã»»
           MOVE       ZERO              TO  LNK-SYU-TGMONEY(16).
           MOVE       ZERO              TO  LNK-SYU-TGMONEY-TAX(16).
           PERFORM    VARYING   HKNGAI-IDX   FROM   1   BY   1
                      UNTIL     HKNGAI-IDX   >     15
                      ADD       LNK-SYU-TGMONEY(HKNGAI-IDX)
                                            TO  LNK-SYU-TGMONEY(16)
                      ADD       LNK-SYU-TGMONEY-TAX(HKNGAI-IDX)
                                        TO  LNK-SYU-TGMONEY-TAX(16)
           END-PERFORM.
      *
           IF    LNK-SYU-SRYYMD(1:6)       >=   "200704"
             IF    WK-TAX1001-KBN  =  "1"
                COMPUTE LNK-SYU-JIHI-TAX =
                       (LNK-SYU-JIHI-TOTAL-TAX  * 5 / 105) + 0.5
                COMPUTE LNK-SYU-TGMONEY-TAX-SAI   =
                       (LNK-SYU-TGMONEY-TAX(16) * 5 / 105) + 0.5
             ELSE
                COMPUTE LNK-SYU-JIHI-TAX =
                       (LNK-SYU-JIHI-TOTAL-TAX  * 5 / 105)
                COMPUTE LNK-SYU-TGMONEY-TAX-SAI   =
                       (LNK-SYU-TGMONEY-TAX(16) * 5 / 105)
             END-IF
      *      ÀÁµá¶â³Û¤Î·×»»¡ÊÄ´À°¶â´Þ¤Þ¤º¡Ë
             COMPUTE    LNK-SYU-SKYMONEY  =  LNK-SYU-SKYMONEY        +
                                             LNK-SYU-TGMONEY(16)     +
                                             LNK-SYU-TGMONEY-TAX(16) +
                                             LNK-SYU-JIHI-TOTAL      +
                                             LNK-SYU-JIHI-TOTAL-TAX
             IF   WK-HKNKBN  =   "1"
                  COMPUTE    LNK-SYU-RSIJIBAI-SKYMONEY  =
                             LNK-SYU-RSIJIBAI-SKYMONEY  +
                             LNK-SYU-TGMONEY(16)        +
                             LNK-SYU-TGMONEY-TAX(16)    +
                             LNK-SYU-JIHI-TOTAL         +
                             LNK-SYU-JIHI-TOTAL-TAX
             END-IF
             IF  ( COMB-SYU-HKNKOHSBTKBN  =  "8" ) AND
                 ( COMB-HOJOKBN           =  "1" )
                 COMPUTE  LNK-SYU-TAX-TAISHOU     =
                          LNK-SYU-HKNTEKMONEY     +
                          LNK-SYU-TGMONEY-TAX(16) +
                          LNK-SYU-JIHI-TOTAL-TAX
             ELSE
                 COMPUTE  LNK-SYU-TAX-TAISHOU     =
                          LNK-SYU-TGMONEY-TAX(16) +
                          LNK-SYU-JIHI-TOTAL-TAX
             END-IF
             IF    WK-TAX1001-KBN  =  "1"
                 COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI  =
                         (LNK-SYU-TAX-TAISHOU  * 5 / 105) + 0.5
             ELSE
                 COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI  =
                         (LNK-SYU-TAX-TAISHOU  * 5 / 105)
             END-IF
      *
      ***    COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI  =
      ***             LNK-SYU-JIHI-TAX          +
      ***             LNK-SYU-TGMONEY-TAX-SAI   +
      ***             WK-SKYMONEY-ZEI
           ELSE
             IF  ( COMB-SYU-HKNKOHSBTKBN  =  "8" ) AND
                 ( COMB-HOJOKBN           =  "1" )
              CONTINUE
             ELSE
              IF    WK-TAX1001-KBN  =  "1"
                COMPUTE LNK-SYU-JIHI-TAX =
                       (LNK-SYU-JIHI-TOTAL-TAX  * 0.05)  +  0.5
                COMPUTE LNK-SYU-TGMONEY-TAX-SAI   =
                       (LNK-SYU-TGMONEY-TAX(16) * 0.05)  +  0.5
                COMPUTE LNK-SYU-TGMONEY-TAX(16)   = 
                       (LNK-SYU-TGMONEY-TAX(16) * 1.05)  +  0.5
              ELSE
                COMPUTE LNK-SYU-JIHI-TAX =
                        LNK-SYU-JIHI-TOTAL-TAX  * 0.05
                COMPUTE LNK-SYU-TGMONEY-TAX-SAI   = 
                        LNK-SYU-TGMONEY-TAX(16) * 0.05
                COMPUTE LNK-SYU-TGMONEY-TAX(16)   = 
                        LNK-SYU-TGMONEY-TAX(16) * 1.05
              END-IF
             END-IF
      *      ÀÁµá¶â³Û¤Î·×»»¡ÊÄ´À°¶â´Þ¤Þ¤º¡Ë
             COMPUTE    LNK-SYU-SKYMONEY  =  LNK-SYU-SKYMONEY        +
                                             LNK-SYU-TGMONEY(16)     +
                                             LNK-SYU-TGMONEY-TAX(16) +
                                             LNK-SYU-JIHI-TOTAL      +
                                             LNK-SYU-JIHI-TOTAL-TAX  +
                                             LNK-SYU-JIHI-TAX
             IF   WK-HKNKBN  =   "1"
                  COMPUTE    LNK-SYU-RSIJIBAI-SKYMONEY  =
                             LNK-SYU-RSIJIBAI-SKYMONEY  +
                             LNK-SYU-TGMONEY(16)        +
                             LNK-SYU-TGMONEY-TAX(16)    +
                             LNK-SYU-JIHI-TOTAL         +
                             LNK-SYU-JIHI-TOTAL-TAX     +
                             LNK-SYU-JIHI-TAX
             END-IF
      *      ¼«ÈñÊÝ¸±¤ÎÊä½õ¶èÊ¬¤¬²ÝÀÇ¤Î¾ì¹ç¡¢¾ÃÈñÀÇ·×»»¤ò¹Ô¤¦
             IF  ( COMB-SYU-HKNKOHSBTKBN  =  "8" ) AND
                 ( COMB-HOJOKBN           =  "1" )
               IF    WK-TAX1001-KBN  =  "1"
                 COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI  =
                        ( LNK-SYU-SKYMONEY  *  0.05 ) + 0.5
                 COMPUTE  LNK-SYU-SKYMONEY          =
                        ( LNK-SYU-SKYMONEY  *  1.05 ) + 0.5
               ELSE
                 COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI  =
                          LNK-SYU-SKYMONEY  *  0.05
                 COMPUTE  LNK-SYU-SKYMONEY          =
                          LNK-SYU-SKYMONEY  *  1.05
               END-IF
             ELSE
               COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI  =
                        LNK-SYU-JIHI-TAX          +
                        LNK-SYU-TGMONEY-TAX-SAI
             END-IF
           END-IF.
      *
      *    ¸ºÌÈ·×»»¡¦ÀÁµá³ÛÃ¼¿ô·×»»¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¤ò½ü¤¯¡Ë
           MOVE     ZERO      TO    FLG-GENMEN.
           IF     LNK-SYU-SKYMONEY         NOT  =   ZERO
              IF   LNK-SYU-SYUHKNNUM         =    "971" OR "973"
                   CONTINUE
              ELSE
      *
      *            ¸ºÌÈ·×»»
                   INITIALIZE                           PTINF-REC
                   MOVE    LNK-SYU-HOSPID       TO      PTINF-HOSPID
                   MOVE    LNK-SYU-PTID         TO      PTINF-PTID
                   MOVE    PTINF-REC            TO      MCPDATA-REC
                   MOVE    "DBSELECT"           TO      MCP-FUNC
                   MOVE    "PTINF-KEY"          TO      ORC-DBPATH
                   CALL    "ORCMCPSUB"           USING   MCPAREA
                                                         ORCMCPAREA
                                                         MCPDATA-REC
                   IF      MCP-RC               =       ZERO
                       MOVE    "PTINF-KEY"      TO      ORC-DBPATH
                       PERFORM  PTINF-READ-SEC
                       IF       MCP-RC          =       ZERO
                           PERFORM   300-GENMEN-KEISAN-SEC
                       ELSE
                           DISPLAY  "PTINF FETCH ERR"
                       END-IF
                   ELSE
                       DISPLAY   "PTINF SELECT ERR"
                   END-IF
      *
                   MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
                   MOVE    "PTINF-KEY"          TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"           USING
                                                 MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
      *
      *            ÀÁµá³ÛÃ¼¿ô·×»»¡Ê¸ºÌÈÌµ¤·¡Ë
                   IF   FLG-GENMEN  =  ZERO
                       PERFORM   400-SEIKYU-HASUU-KEISAN-SEC
                   END-IF
              END-IF
           END-IF.
      *
      *    ÀÁµá¶â³Û¤Î·×»»¡ÊÏ«ºÒÊÝ¸±Å¬ÍÑ»þ¤Î±ß·ú¤Æ¹àÌÜ²Ã»»¡Ë
      *    ¡ãÏ«ºÒ»ØÄê¡Ê£°¡ó¡Ë¡ä                                         
           IF   (WK-HKNKBN               =    "1"  ) AND                
                (LNK-SYU-SYUHKNNUM       =    "971") AND                
                (LNK-SYU-PTFTNRATE       =    ZERO )
             MOVE    ZERO      TO       LNK-SYU-SKYMONEY
             IF    LNK-SYU-SRYYMD(1:6)       >=   "200704"
               COMPUTE LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY        +
                                          LNK-SYU-TGMONEY(16)     +
                                          LNK-SYU-TGMONEY-TAX(16) +
                                          LNK-SYU-JIHI-TOTAL      +
                                          LNK-SYU-JIHI-TOTAL-TAX
             ELSE
               COMPUTE LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY        +
                                          LNK-SYU-TGMONEY(16)     +
                                          LNK-SYU-TGMONEY-TAX(16) +
                                          LNK-SYU-JIHI-TOTAL      +
                                          LNK-SYU-JIHI-TOTAL-TAX  +
                                          LNK-SYU-JIHI-TAX
             END-IF
           END-IF.
      *
      *    ¡ãÈóÏ«ºÒ»ØÄê¡Ê£±£°£°¡ó¡Ë¡ä                                   
           IF   (WK-HKNKBN               =    "1"  ) AND                
                (LNK-SYU-SYUHKNNUM       =    "971") AND                
                (LNK-SYU-PTFTNRATE       =     100 )
                 COMPUTE    LNK-SYU-SKYMONEY  =   LNK-SYU-SKYMONEY   +  
                                            LNK-SYU-RSISHOSHIN-MONEY +  
                                            LNK-SYU-RSISAISHIN-MONEY +  
                                            LNK-SYU-RSISDO-MONEY     +  
                                            LNK-SYU-RSIETC-MONEY        
      *
                 COMPUTE    LNK-SYU-RSI-TOTAL =                         
                                            LNK-SYU-RSISHOSHIN-MONEY +  
                                            LNK-SYU-RSISAISHIN-MONEY +  
                                            LNK-SYU-RSISDO-MONEY     +  
                                            LNK-SYU-RSIETC-MONEY        
           END-IF.                                                      
      *
           IF   (WK-HKNKBN               =    "1"  ) AND                
                (LNK-SYU-SYUHKNNUM       =    "971")                    
                   COMPUTE    LNK-SYU-RSIJIBAI-SKYMONEY  =  
                              LNK-SYU-RSIJIBAI-SKYMONEY  +  
                              LNK-SYU-RSISHOSHIN-MONEY   +  
                              LNK-SYU-RSISAISHIN-MONEY   +  
                              LNK-SYU-RSISDO-MONEY       +  
                              LNK-SYU-RSIETC-MONEY          
           END-IF.
      *
           MOVE     ZERO      TO   WK-RSIETC-ZEITAISYO.
      *
      *    ¼«ÇåÀÕÊÝ¸±¤Î¾ì¹ç¡¢Ï«ºÒ±ß·ú¤Æ¹àÌÜ¤Ø¤Î²Ã»»Î¨·×»»ÄÉ²Ã
      *    ¡ã´µ¼ÔÀÁµá¤¹¤ë¡Ê£±£°£°¡ó¡Ë¤Î¾ì¹ç¡ä
           IF   (WK-HKNKBN               =    "1")   AND
                (LNK-SYU-SYUHKNNUM       =    "973") AND
                (LNK-SYU-PTFTNRATE       =     100 )
              IF   WK-4001-JIBAIKBN  =  "1"
                 COMPUTE  LNK-SYU-RSISHOSHIN-MONEY  =
                         (LNK-SYU-RSISHOSHIN-MONEY  *  WK-4001-KSNRATE)
                                                    +  0.5      
                 COMPUTE  LNK-SYU-RSISAISHIN-MONEY  =
                         (LNK-SYU-RSISAISHIN-MONEY  *  WK-4001-KSNRATE)
                                                    +  0.5
              END-IF
      *
      *          ¼«ÇåÀÕ¤½¤ÎÂ¾¥³¡¼¥É¾ðÊó¤ÎÊÔ½¸¡Ê£°£¹£µ£¹£±¡¥£¹£²¡¥£¹£³¡Ë
                 PERFORM   500-JIBAI-SONOTA-HENSYU-SEC
      *
                 COMPUTE    LNK-SYU-SKYMONEY  =   LNK-SYU-SKYMONEY  +
                                            LNK-SYU-RSISHOSHIN-MONEY +
                                            LNK-SYU-RSISAISHIN-MONEY +
                                            LNK-SYU-RSISDO-MONEY     +
                                            LNK-SYU-RSIETC-MONEY
      *
                 COMPUTE    LNK-SYU-RSI-TOTAL =   
                                            LNK-SYU-RSISHOSHIN-MONEY +  
                                            LNK-SYU-RSISAISHIN-MONEY +  
                                            LNK-SYU-RSISDO-MONEY     +  
                                            LNK-SYU-RSIETC-MONEY        
      *
                 IF    LNK-SYU-SRYYMD(1:6)       >=   "200704"
                     COMPUTE  LNK-SYU-TAX-TAISHOU     =
                              LNK-SYU-TGMONEY-TAX(16) +
                              LNK-SYU-JIHI-TOTAL-TAX  +
                              WK-RSIETC-ZEITAISYO
                     IF    WK-TAX1001-KBN  =  "1"
                        COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI  =
                                (LNK-SYU-TAX-TAISHOU  * 5 / 105) + 0.5
                     ELSE
                        COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI  =
                                (LNK-SYU-TAX-TAISHOU  * 5 / 105)
                     END-IF
      ***            COMPUTE    LNK-SYU-SKYMONEY-TAX-SAI  =
      ***                       LNK-SYU-SKYMONEY-TAX-SAI  +
      ***                       LNK-SYU-RSI-TAX-SAI    
                 ELSE
                     COMPUTE    LNK-SYU-SKYMONEY-TAX-SAI  =
                                LNK-SYU-SKYMONEY-TAX-SAI  +
                                LNK-SYU-RSI-TAX-SAI        
                 END-IF
           END-IF.
      *    ¡ã´µ¼ÔÀÁµá¤·¤Ê¤¤¡Ê£°¡ó¡Ë¤Î¾ì¹ç¡ä
           IF   (WK-HKNKBN               =    "1")   AND
                (LNK-SYU-SYUHKNNUM       =    "973") AND
                (LNK-SYU-PTFTNRATE       =    ZERO )
              IF   WK-4001-JIBAIKBN  =  "1"
                 COMPUTE  LNK-SYU-RSISHOSHIN-MONEY  =
                         (LNK-SYU-RSISHOSHIN-MONEY  *  WK-4001-KSNRATE)
                                                    +  0.5      
                 COMPUTE  LNK-SYU-RSISAISHIN-MONEY  =
                         (LNK-SYU-RSISAISHIN-MONEY  *  WK-4001-KSNRATE)
                                                    +  0.5
              END-IF
      *
      *       ¼«ÇåÀÕ¤½¤ÎÂ¾¥³¡¼¥É¾ðÊó¤ÎÊÔ½¸¡Ê£°£¹£µ£¹£±¡¥£¹£²¡¥£¹£³¡Ë
              PERFORM   500-JIBAI-SONOTA-HENSYU-SEC
      *
              MOVE    ZERO      TO       LNK-SYU-SKYMONEY
              IF    LNK-SYU-SRYYMD(1:6)       >=   "200704"
               COMPUTE LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY        +
                                          LNK-SYU-TGMONEY(16)     +
                                          LNK-SYU-TGMONEY-TAX(16) +
                                          LNK-SYU-JIHI-TOTAL      +
                                          LNK-SYU-JIHI-TOTAL-TAX
              ELSE
               COMPUTE LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY        +
                                          LNK-SYU-TGMONEY(16)     +
                                          LNK-SYU-TGMONEY-TAX(16) +
                                          LNK-SYU-JIHI-TOTAL      +
                                          LNK-SYU-JIHI-TOTAL-TAX  +
                                          LNK-SYU-JIHI-TAX
              END-IF
           END-IF.
      *
           IF   (WK-HKNKBN               =    "1"  ) AND                
                (LNK-SYU-SYUHKNNUM       =    "973")                    
                   COMPUTE    LNK-SYU-RSIJIBAI-SKYMONEY  =  
                              LNK-SYU-RSIJIBAI-SKYMONEY  +  
                              LNK-SYU-RSISHOSHIN-MONEY   +  
                              LNK-SYU-RSISAISHIN-MONEY   +  
                              LNK-SYU-RSISDO-MONEY       +  
                              LNK-SYU-RSIETC-MONEY          
           END-IF.
      *
      *    ´µ¼ÔÉéÃ´¶â¡¡ÆÈ¼«·×»»ÂÐ±þ
      *
           INITIALIZE                         SYS-1037-REC
           MOVE    "1037"               TO    SYS-1037-KANRICD
           MOVE    "*"                  TO    SYS-1037-KBNCD
           MOVE    LNK-SYU-SRYYMD       TO    ORC-DBYMD
           MOVE    SYS-1037-REC         TO    MCPDATA-REC
           MOVE    "DBSELECT"           TO    MCP-FUNC
           MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH
           CALL    "ORCMCPSUB"    USING       MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
           IF      MCP-RC               =     ZERO
               MOVE     "SYSKANRI-KEY"  TO    ORC-DBPATH
               PERFORM   SYSKANRI-READ-SEC
               IF    MCP-RC             =     ZERO
                   IF   WK-1001-HOSPID  =    SYS-1037-HOSPID
                       IF  SYS-1037-PGID NOT = SPACE
                          CALL   SYS-1037-PGID   USING   MCPAREA
                                                 LNK-SYU-SYUNOU-REC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *
      *    ÀÁµá¶â³Û¤Î·×»»¡ÊÄ´À°¶â¤Î²Ã¸º»»¡Ë
      ***  COMPUTE    LNK-SYU-SKYMONEY  =   LNK-SYU-SKYMONEY    +
      ***                                   LNK-SYU-CHOSEI.
           COMPUTE    WK-KEISAN         =   LNK-SYU-SKYMONEY    +
                                            LNK-SYU-CHOSEI.
           IF   WK-KEISAN  <  ZERO
               COMPUTE  LNK-SYU-CHOSEI  =   LNK-SYU-SKYMONEY
                                        *   -1
               MOVE   ZERO            TO   LNK-SYU-SKYMONEY
           ELSE
               MOVE   WK-KEISAN       TO   LNK-SYU-SKYMONEY
           END-IF.
      *
       MAIN-EXT.
           EXIT PROGRAM
           STOP RUN.
      *
      ******************************************************************
      *        ¸ºÌÈ·×»»½èÍý                                            *
      ******************************************************************
       300-GENMEN-KEISAN-SEC          SECTION.
      *
           IF   PTINF-DISCOUNT  NOT =   SPACE  AND  "00"
      *
               INITIALIZE                         SYS-1026-REC
               MOVE    "1026"               TO    SYS-1026-KANRICD
               MOVE    PTINF-DISCOUNT-KBN   TO    SYS-1026-KBNCD
               MOVE    LNK-SYU-SRYYMD       TO    ORC-DBYMD
               MOVE    SYS-1026-REC         TO    MCPDATA-REC
               MOVE    "DBSELECT"           TO    MCP-FUNC
               MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH
               CALL    "ORCMCPSUB"    USING        MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
               IF      MCP-RC               =     ZERO
                   MOVE     "SYSKANRI-KEY"  TO    ORC-DBPATH
                   PERFORM   SYSKANRI-READ-SEC
                   IF    MCP-RC             =     ZERO
                       MOVE   SYS-1026-KBNCD  TO  LNK-SYU-DISCOUNT-KBN
                       MOVE   SYS-1026-DISCOUNT-BODY
                                              TO  LNK-SYU-DISCOUNT-BODY
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
      *
               INITIALIZE                         SYS-1027-REC
               MOVE    "1027"               TO    SYS-1027-KANRICD
               MOVE    PTINF-DISCOUNT       TO    SYS-1027-KBNCD
               MOVE    LNK-SYU-SRYYMD       TO    ORC-DBYMD
               MOVE    SYS-1027-REC         TO    MCPDATA-REC
               MOVE    "DBSELECT"           TO    MCP-FUNC
               MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH
               CALL    "ORCMCPSUB"    USING       MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
               IF      MCP-RC               =     ZERO
                   MOVE     "SYSKANRI-KEY"  TO    ORC-DBPATH
                   PERFORM   SYSKANRI-READ-SEC
                   IF    MCP-RC             =     ZERO
                       EVALUATE  SYS-1027-TEIRITU
                           WHEN  "1"
                               MOVE    ZERO    TO   WK-RATE
                               COMPUTE  WK-RATE  =  
                                        (100 - SYS-1027-DISCOUNT-RATE)
                                        /  100
                               MOVE       LNK-SYU-SKYMONEY
                                                  TO   WK-GENMEN-KEISAN
                               COMPUTE    WK-GENMEN-KEISAN2  =
                                          LNK-SYU-SKYMONEY   *  WK-RATE
                               COMPUTE    WK-GENMEN-KEISAN3  =
                                          WK-GENMEN-KEISAN   -
                                          WK-GENMEN-KEISAN2
                               EVALUATE   WK-HASU1001-KBN
                                  WHEN  " "
                                  WHEN  "1"
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                         (LNK-SYU-SKYMONEY * WK-RATE)
                                                           + 5
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                               LNK-SYU-SKYMONEY  / 10
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                               LNK-SYU-SKYMONEY  * 10
                                  WHEN  "2"
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                          LNK-SYU-SKYMONEY * WK-RATE
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                               LNK-SYU-SKYMONEY  / 10
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                               LNK-SYU-SKYMONEY  * 10
                                  WHEN  "3"
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                         (LNK-SYU-SKYMONEY * WK-RATE)
                                                           + 9
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                               LNK-SYU-SKYMONEY  / 10
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                               LNK-SYU-SKYMONEY  * 10
                                  WHEN  "4"
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                          LNK-SYU-SKYMONEY * WK-RATE
                               END-EVALUATE
                           WHEN  "2"
                               IF   SYS-1027-DISCOUNT-RATE >= 
                                                     LNK-SYU-SKYMONEY
                                    MOVE    LNK-SYU-SKYMONEY
                                                   TO  WK-GENMEN-KEISAN3
                                    MOVE    ZERO   TO   LNK-SYU-SKYMONEY
                               ELSE
                                    MOVE    SYS-1027-DISCOUNT-RATE
                                                   TO  WK-GENMEN-KEISAN3
                                    COMPUTE    LNK-SYU-SKYMONEY  =
                                               LNK-SYU-SKYMONEY  -
                                               SYS-1027-DISCOUNT-RATE
                                    EVALUATE   WK-HASU1001-KBN
                                      WHEN  " "
                                      WHEN  "1"
                                        COMPUTE  LNK-SYU-SKYMONEY  =
                                                 LNK-SYU-SKYMONEY  +   5
                                        COMPUTE  WK-SEIKYUGK       =
                                                 LNK-SYU-SKYMONEY  /  10
                                        COMPUTE  LNK-SYU-SKYMONEY  =
                                                 WK-SEIKYUGK       *  10
                                      WHEN  "2"
                                        COMPUTE  WK-SEIKYUGK       =
                                                 LNK-SYU-SKYMONEY  /  10
                                        COMPUTE  LNK-SYU-SKYMONEY  =
                                                 WK-SEIKYUGK       *  10
                                      WHEN  "3"
                                        COMPUTE  LNK-SYU-SKYMONEY  =
                                                 LNK-SYU-SKYMONEY  +   9
                                        COMPUTE  WK-SEIKYUGK       =
                                                 LNK-SYU-SKYMONEY  /  10
                                        COMPUTE  LNK-SYU-SKYMONEY  =
                                                 WK-SEIKYUGK       *  10
                                      WHEN  "4"
                                        CONTINUE
                                    END-EVALUATE
                               END-IF
                       END-EVALUATE
                       MOVE   SYS-1027-KBNCD   TO
                                               LNK-SYU-DISCOUNT-RATEKBN
                       MOVE   SYS-1027-TEIRITU TO
                                               LNK-SYU-DISCOUNT-TEIRITU
                       MOVE   SYS-1027-DISCOUNT-RATE  TO
                                               LNK-SYU-DISCOUNT-RATE
                       IF   LNK-SYU-DISCOUNT-TEIRITU  NOT =  SPACE
                       AND  LNK-SYU-DISCOUNT-RATE     NOT =  ZERO
                           MOVE     1     TO    FLG-GENMEN
                           MOVE     WK-GENMEN-KEISAN3
                                          TO    LNK-SYU-DISCOUNT-MONEY
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           END-IF.
      *
       300-GENMEN-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÀÁµá³ÛÃ¼¿ô·×»»½èÍý¡Ê¸ºÌÈÌµ¤·¡Ë                                      *
      ******************************************************************
       400-SEIKYU-HASUU-KEISAN-SEC          SECTION.
      *
           EVALUATE   WK-HASU1001-KBN2
              WHEN  "1"
                  COMPUTE  LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY  +   5
                  COMPUTE  WK-SEIKYUGK      = LNK-SYU-SKYMONEY  /  10
                  COMPUTE  LNK-SYU-SKYMONEY = WK-SEIKYUGK       *  10
              WHEN  "2"
                  COMPUTE  WK-SEIKYUGK      = LNK-SYU-SKYMONEY  /  10
                  COMPUTE  LNK-SYU-SKYMONEY = WK-SEIKYUGK       *  10
              WHEN  "3"
                  COMPUTE  LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY  +   9
                  COMPUTE  WK-SEIKYUGK      = LNK-SYU-SKYMONEY  /  10
                  COMPUTE  LNK-SYU-SKYMONEY = WK-SEIKYUGK       *  10
              WHEN  " "
              WHEN  "4"
                  CONTINUE
           END-EVALUATE
      *
           .
      *
       400-SEIKYU-HASUU-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¼«ÇåÀÕ¤½¤ÎÂ¾¥³¡¼¥É¾ðÊó¤ÎÊÔ½¸¡Ê£°£¹£µ£¹£±¡¥£¹£²¡¥£¹£³¡Ë                                      *
      ******************************************************************
       500-JIBAI-SONOTA-HENSYU-SEC          SECTION.
      *
           INITIALIZE                            WK-4003-TAIGAI.
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£´£°£°£³¡Ë¤«¤é¾ÃÈñÀÇÂÐ¾Ý³°¥³¡¼¥É¼èÆÀ
      *
           IF   WK-4001-TAXKBN-KZI   =   "2"
              INITIALIZE                         SYS-4003-REC
              MOVE    "4003"               TO    SYS-4003-KANRICD
              MOVE    "*"                  TO    SYS-4003-KBNCD
              MOVE    LNK-SYU-SRYYMD       TO    ORC-DBYMD
              MOVE    SYS-4003-REC         TO    MCPDATA-REC
              MOVE    "DBSELECT"           TO    MCP-FUNC
              MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH
              CALL    "ORCMCPSUB"    USING       MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
              IF      MCP-RC               =     ZERO
                  MOVE     "SYSKANRI-KEY"  TO    ORC-DBPATH
                  PERFORM   SYSKANRI-READ-SEC
                  IF    MCP-RC             =     ZERO
                     PERFORM   VARYING   IDX8    FROM   1   BY   1
                                UNTIL   (IDX8                 >    10 )
                                  OR    (SYS-4003-SRYCD(IDX8) =  SPACE)
                          MOVE    SYS-4003-SRYCD(IDX8)
                                        TO   WK-4003-TAIGAI-SRYCD(IDX8)
                     END-PERFORM
                  END-IF
              END-IF
      *
              MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
              CALL    "ORCMCPSUB"           USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           END-IF.
      *
      *    ¥³¡¼¥ÉÊÌ¤ËÂàÈò
      *
           INITIALIZE              WK-SONOTA91.
           INITIALIZE              WK-SONOTA92.
           INITIALIZE              WK-SONOTA93.
           MOVE     ZERO      TO   IDX5.
           MOVE     ZERO      TO   IDX6.
           MOVE     ZERO      TO   IDX7.
      *
           PERFORM   VARYING   IDX4    FROM   1   BY   1
                      UNTIL   (IDX4                           >    20 )
                       OR     (LNK-S01-SONOTA-SRYCD(IDY IDX4) =  SPACE)
                 EVALUATE      LNK-S01-SONOTA-SRYCD(IDY IDX4)(1:5)
                    WHEN   "09591"
                       ADD     1     TO   IDX5
                       MOVE    LNK-S01-SONOTA-SRYCD(IDY IDX4)
                                     TO   WK-SONOTA91-SRYCD(IDX5)
                       MOVE    LNK-S01-SONOTA-ZAIKAISU(IDY IDX4)
                                     TO   WK-SONOTA91-ZAIKAISU(IDX5)
                       MOVE    LNK-S01-SONOTA-TANKA(IDY IDX4)
                                     TO   WK-SONOTA91-TANKA(IDX5)
                    WHEN   "09592"
                       ADD     1     TO   IDX6
                       MOVE    LNK-S01-SONOTA-SRYCD(IDY IDX4)
                                     TO   WK-SONOTA92-SRYCD(IDX6)
                       MOVE    LNK-S01-SONOTA-ZAIKAISU(IDY IDX4)
                                     TO   WK-SONOTA92-ZAIKAISU(IDX6)
                       MOVE    LNK-S01-SONOTA-TANKA(IDY IDX4)
                                     TO   WK-SONOTA92-TANKA(IDX6)
                    WHEN   "09593"
                       MOVE     SPACE    TO    FLG-TAIGAI
                       PERFORM  VARYING  IDX8    FROM   1   BY   1
                            UNTIL  (IDX8                       >   10  )
                              OR   (WK-4003-TAIGAI-SRYCD(IDX8) =  SPACE)
                              OR   (FLG-TAIGAI                 =  "1"  )
                         IF   WK-4003-TAIGAI-SRYCD(IDX8)  =
                                         LNK-S01-SONOTA-SRYCD(IDY IDX4)
                              MOVE    "1"     TO    FLG-TAIGAI
                         END-IF
                       END-PERFORM
                       IF   FLG-TAIGAI  =  SPACE
                          ADD     1     TO   IDX7
                          MOVE    LNK-S01-SONOTA-SRYCD(IDY IDX4)
                                        TO   WK-SONOTA93-SRYCD(IDX7)
                          MOVE    LNK-S01-SONOTA-ZAIKAISU(IDY IDX4)
                                        TO   WK-SONOTA93-ZAIKAISU(IDX7)
                          MOVE    LNK-S01-SONOTA-TANKA(IDY IDX4)
                                        TO   WK-SONOTA93-TANKA(IDX7)
                       END-IF
                 END-EVALUATE
           END-PERFORM.
      *
      *    Ê¸½ñÎÁ¾ÃÈñÀÇ·×»»
      *
           IF   WK-4001-TAXKBN   =   "2"
      *
              IF    WK-TAX1001-KBN  =  "1"
      *
                 PERFORM   VARYING   IDX4    FROM   1   BY   1
                            UNTIL   (IDX4                    >    20 )
                              OR    (WK-SONOTA91-SRYCD(IDX4) =  SPACE)
      *
                   COMPUTE  WK-KEISAN  =
                           (WK-SONOTA91-TANKA(IDX4)  *  0.05)  +  0.5
                   COMPUTE  WK-KEISAN  =  WK-KEISAN
                                       *  WK-SONOTA91-ZAIKAISU(IDX4)
                   COMPUTE  LNK-SYU-RSI-TAX-SAI  = LNK-SYU-RSI-TAX-SAI
                                                 + WK-KEISAN
                   COMPUTE  LNK-SYU-RSIETC-MONEY = LNK-SYU-RSIETC-MONEY
                                                 + WK-KEISAN
                   IF    LNK-SYU-SRYYMD(1:6)       >=   "200704"
                     COMPUTE  WK-KEISAN-JIBAI =
                              WK-SONOTA91-TANKA(IDX4)    *
                              WK-SONOTA91-ZAIKAISU(IDX4)
                     COMPUTE  WK-RSIETC-ZEITAISYO = WK-RSIETC-ZEITAISYO
                                                  + WK-KEISAN-JIBAI
                                                  + WK-KEISAN
                   END-IF
      *
                 END-PERFORM
      *
                 PERFORM   VARYING   IDX4    FROM   1   BY   1
                            UNTIL   (IDX4                    >    20 )
                              OR    (WK-SONOTA92-SRYCD(IDX4) =  SPACE)
      *
                   COMPUTE  WK-KEISAN  =
                           (WK-SONOTA92-TANKA(IDX4)  *  0.05)  +  0.5
                   COMPUTE  WK-KEISAN  =  WK-KEISAN
                                       *  WK-SONOTA92-ZAIKAISU(IDX4)
                   COMPUTE  LNK-SYU-RSI-TAX-SAI  = LNK-SYU-RSI-TAX-SAI
                                                 + WK-KEISAN
                   COMPUTE  LNK-SYU-RSIETC-MONEY = LNK-SYU-RSIETC-MONEY
                                                 + WK-KEISAN
                   IF    LNK-SYU-SRYYMD(1:6)       >=   "200704"
                     COMPUTE  WK-KEISAN-JIBAI =
                              WK-SONOTA92-TANKA(IDX4)    *
                              WK-SONOTA92-ZAIKAISU(IDX4)
                     COMPUTE  WK-RSIETC-ZEITAISYO = WK-RSIETC-ZEITAISYO
                                                  + WK-KEISAN-JIBAI
                                                  + WK-KEISAN
                   END-IF
      *
                 END-PERFORM
      *
              ELSE
      *
                 PERFORM   VARYING   IDX4    FROM   1   BY   1
                            UNTIL   (IDX4                    >    20 )
                              OR    (WK-SONOTA91-SRYCD(IDX4) =  SPACE)
      *
                   COMPUTE  WK-KEISAN  =
                           (WK-SONOTA91-TANKA(IDX4)  *  0.05)
                   COMPUTE  WK-KEISAN  =  WK-KEISAN
                                       *  WK-SONOTA91-ZAIKAISU(IDX4)
                   COMPUTE  LNK-SYU-RSI-TAX-SAI  = LNK-SYU-RSI-TAX-SAI
                                                 + WK-KEISAN
                   COMPUTE  LNK-SYU-RSIETC-MONEY = LNK-SYU-RSIETC-MONEY
                                                 + WK-KEISAN
                   IF    LNK-SYU-SRYYMD(1:6)       >=   "200704"
                     COMPUTE  WK-KEISAN-JIBAI =
                              WK-SONOTA91-TANKA(IDX4)    *
                              WK-SONOTA91-ZAIKAISU(IDX4)
                     COMPUTE  WK-RSIETC-ZEITAISYO = WK-RSIETC-ZEITAISYO
                                                  + WK-KEISAN-JIBAI
                                                  + WK-KEISAN
                   END-IF
      *
                 END-PERFORM
      *
                 PERFORM   VARYING   IDX4    FROM   1   BY   1
                            UNTIL   (IDX4                    >    20 )
                              OR    (WK-SONOTA92-SRYCD(IDX4) =  SPACE)
      *
                   COMPUTE  WK-KEISAN  =
                           (WK-SONOTA92-TANKA(IDX4)  *  0.05)
                   COMPUTE  WK-KEISAN  =  WK-KEISAN
                                       *  WK-SONOTA92-ZAIKAISU(IDX4)
                   COMPUTE  LNK-SYU-RSI-TAX-SAI  = LNK-SYU-RSI-TAX-SAI
                                                 + WK-KEISAN
                   COMPUTE  LNK-SYU-RSIETC-MONEY = LNK-SYU-RSIETC-MONEY
                                                 + WK-KEISAN
                   IF    LNK-SYU-SRYYMD(1:6)       >=   "200704"
                     COMPUTE  WK-KEISAN-JIBAI =
                              WK-SONOTA92-TANKA(IDX4)    *
                              WK-SONOTA92-ZAIKAISU(IDX4)
                     COMPUTE  WK-RSIETC-ZEITAISYO = WK-RSIETC-ZEITAISYO
                                                  + WK-KEISAN-JIBAI
                                                  + WK-KEISAN
                   END-IF
      *
                 END-PERFORM
      *
              END-IF
      *
           END-IF.
      *
      *    ÆÃÄê´ïºàÅù¾ÃÈñÀÇ·×»»
      *
           IF   WK-4001-TAXKBN-KZI   =   "2"
      *
              IF    WK-TAX1001-KBN  =  "1"
      *
                 PERFORM   VARYING   IDX4    FROM   1   BY   1
                            UNTIL   (IDX4                    >    20 )
                              OR    (WK-SONOTA93-SRYCD(IDX4) =  SPACE)
      *
                   COMPUTE  WK-KEISAN  =
                           (WK-SONOTA93-TANKA(IDX4)  *  0.05)  +  0.5
                   COMPUTE  WK-KEISAN  =  WK-KEISAN
                                       *  WK-SONOTA93-ZAIKAISU(IDX4)
                   COMPUTE  LNK-SYU-RSI-TAX-SAI  = LNK-SYU-RSI-TAX-SAI
                                                 + WK-KEISAN
                   COMPUTE  LNK-SYU-RSIETC-MONEY = LNK-SYU-RSIETC-MONEY
                                                 + WK-KEISAN
                   IF    LNK-SYU-SRYYMD(1:6)       >=   "200704"
                     COMPUTE  WK-KEISAN-JIBAI =
                              WK-SONOTA93-TANKA(IDX4)    *
                              WK-SONOTA93-ZAIKAISU(IDX4)
                     COMPUTE  WK-RSIETC-ZEITAISYO = WK-RSIETC-ZEITAISYO
                                                  + WK-KEISAN-JIBAI
                                                  + WK-KEISAN
                   END-IF
      *
                 END-PERFORM
      *
              ELSE
      *
                 PERFORM   VARYING   IDX4    FROM   1   BY   1
                            UNTIL   (IDX4                    >    20 )
                              OR    (WK-SONOTA93-SRYCD(IDX4) =  SPACE)
      *
                   COMPUTE  WK-KEISAN  =
                           (WK-SONOTA93-TANKA(IDX4)  *  0.05)
                   COMPUTE  WK-KEISAN  =  WK-KEISAN
                                       *  WK-SONOTA93-ZAIKAISU(IDX4)
                   COMPUTE  LNK-SYU-RSI-TAX-SAI  = LNK-SYU-RSI-TAX-SAI
                                                 + WK-KEISAN
                   COMPUTE  LNK-SYU-RSIETC-MONEY = LNK-SYU-RSIETC-MONEY
                                                 + WK-KEISAN
                   IF    LNK-SYU-SRYYMD(1:6)       >=   "200704"
                     COMPUTE  WK-KEISAN-JIBAI =
                              WK-SONOTA93-TANKA(IDX4)    *
                              WK-SONOTA93-ZAIKAISU(IDX4)
                     COMPUTE  WK-RSIETC-ZEITAISYO = WK-RSIETC-ZEITAISYO
                                                  + WK-KEISAN-JIBAI
                                                  + WK-KEISAN
                   END-IF
      *
                 END-PERFORM
      *
              END-IF
      *
           END-IF.
      *
       500-JIBAI-SONOTA-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÊÝ¸±»²¾ÈÉôÊ¬³ÎÄê½èÍý                                    *
      ******************************************************************
       100-KAKUTEI-SEC          SECTION.
      *
           MOVE    SPACE                TO      FLG-SANSHOU.
           MOVE    SPACE                TO      FLG-SANSHOU2.
           MOVE    SPACE                TO      FLG-SANSHOU3.     
      *
           MOVE    LNK-SYU-SRYYMD       TO      WK-TEISYOTOKU-YMD.
           MOVE    01                   TO      WK-TEISYOTOKU-DD.
      *
           INITIALIZE                           TNKRRK-REC.
           MOVE    LNK-SYU-HOSPID       TO      TNKRRK-HOSPID.
           MOVE    LNK-SYU-PTID         TO      TNKRRK-PTID.
           MOVE    TNKRRK-REC           TO      MCPDATA-REC.
           MOVE    "DBSELECT"           TO      MCP-FUNC.
           MOVE    "TNKRRK-KEY2"        TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC.
           IF      MCP-RC               =       ZERO
               MOVE    "TNKRRK-KEY2"    TO      ORC-DBPATH
               PERFORM  TNKRRK-READ-SEC
               PERFORM  UNTIL
                          (FLG-TNKRRK         =                   1) OR
                         ((TNKRRK-STYMD      <=   WK-TEISYOTOKU-YMD) AND
                          (WK-TEISYOTOKU-YMD <=   TNKRRK-EDYMD     ))
                   PERFORM   TNKRRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE    1                TO      FLG-TNKRRK
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "TNKRRK-KEY2"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
           INITIALIZE                           TSYRRK-REC.
           MOVE    LNK-SYU-HOSPID       TO      TSYRRK-HOSPID.
           MOVE    LNK-SYU-PTID         TO      TSYRRK-PTID.
           MOVE    TSYRRK-REC           TO      MCPDATA-REC.
           MOVE    "DBSELECT"           TO      MCP-FUNC.
           MOVE    "TSYRRK-KEY2"        TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC.
           IF      MCP-RC               =       ZERO
               MOVE    "TSYRRK-KEY2"    TO      ORC-DBPATH
               PERFORM  TSYRRK-READ-SEC
               PERFORM  UNTIL
                       (FLG-TSYRRK          =                   1) OR
                      ((TSYRRK-NINSTYMD    <=   WK-TEISYOTOKU-YMD) AND
                       (WK-TEISYOTOKU-YMD  <=   TSYRRK-NINEDYMD  ))
                   PERFORM   TSYRRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE    1                TO      FLG-TSYRRK
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "TSYRRK-KEY2"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
           IF  LNK-SYU-SRYYMD(1:6)          <  "200210"
              EVALUATE   FLG-TSYRRK       ALSO     FLG-TNKRRK
                 WHEN    1                ALSO     0
                 WHEN    1                ALSO     1
                         MOVE     "1"     TO       FLG-SANSHOU
                 WHEN    0                ALSO     1
                         MOVE     "2"     TO       FLG-SANSHOU
                 WHEN    0                ALSO     0
                         MOVE     "3"     TO       FLG-SANSHOU
              END-EVALUATE
              MOVE       "1"              TO       FLG-SANSHOU
           ELSE
              IF    LNK-SYU-SRYYMD(1:6)   <   "200304"
                 EVALUATE   FLG-TSYRRK       ALSO     FLG-TNKRRK
                    WHEN    1                ALSO     1
                            MOVE     "1"     TO       FLG-SANSHOU
                    WHEN    0                ALSO     1
                            MOVE     "2"     TO       FLG-SANSHOU
                    WHEN    0                ALSO     0
                    WHEN    1                ALSO     0
                            MOVE     "3"     TO       FLG-SANSHOU
                 END-EVALUATE
                 MOVE       "1"              TO       FLG-SANSHOU
              ELSE
                 MOVE       SPACE            TO       FLG-SANSHOU
              END-IF
           END-IF.
      *
      *    ¸øÈñÉéÃ´ÉôÊ¬¸¡º÷ÍÑ¤Î»²¾ÈÉôÊ¬¥»¥Ã¥È
      *
           EVALUATE   FLG-TSYRRK        ALSO     FLG-TNKRRK
               WHEN    1                ALSO     1
                       MOVE     "1"     TO       FLG-SANSHOU2
               WHEN    0                ALSO     1
                       MOVE     "2"     TO       FLG-SANSHOU2
               WHEN    0                ALSO     0
               WHEN    1                ALSO     0
                       MOVE     "3"     TO       FLG-SANSHOU2
           END-EVALUATE.
      *
      *
           EVALUATE    FLG-TSYRRK       ALSO     FLG-TNKRRK        
               WHEN    1                ALSO     1                 
                       MOVE    SPACE    TO       FLG-SANSHOU3      
               WHEN    0                ALSO     1                 
                       MOVE     "2"     TO       FLG-SANSHOU3      
               WHEN    0                ALSO     0                 
               WHEN    1                ALSO     0
                       MOVE     "1"     TO       FLG-SANSHOU3      
           END-EVALUATE.                                    
      *
      *    ¸øÈñÉéÃ´¸ÂÅÙ³Û¤Î¥»¥Ã¥È
           MOVE    12000          TO   WK-KOH-GENDOGAKU.
           EVALUATE  FLG-SANSHOU2
             WHEN       "2"
             WHEN       "3"
                  MOVE    8000    TO   WK-KOH-GENDOGAKU
           END-EVALUATE.
      *
           IF   ( COMB-KOH1HKNNUM  =   "972" )
           OR   ( COMB-KOH2HKNNUM  =   "972" )
           OR   ( COMB-KOH3HKNNUM  =   "972" )
           OR   ( COMB-KOH4HKNNUM  =   "972" )
              IF   WK-KOH-GENDOGAKU > 10000
                 MOVE   10000   TO  WK-KOH-GENDOGAKU
              END-IF
           END-IF.
      *
           IF   ( COMB-KOH1HKNNUM  =   "974" )
           OR   ( COMB-KOH2HKNNUM  =   "974" )
           OR   ( COMB-KOH3HKNNUM  =   "974" )
           OR   ( COMB-KOH4HKNNUM  =   "974" )
              IF   WK-KOH-GENDOGAKU > 20000
                 MOVE   20000   TO  WK-KOH-GENDOGAKU
              END-IF
           END-IF.
      *
       100-KAKUTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¼«ÈñÊÝ¸±¤Î¥Ç¡¼¥¿ÊÔ½¸                                *
      ******************************************************************
       104-JIHIHOKEN-DATA-HENSYU-SEC          SECTION.
      *
           IF  ((COMB-HKNNUM            NOT =   SPACE   ) AND
                (COMB-SYU-HKNKOHSBTKBN      =   "8"     ) AND
                (WK-1001-JIHIPOSITION       =   "2"     ) AND
                (LNK-SYU-SRYYMD(1:6)       >=   "200604"))
           OR  ((COMB-HKNNUM            NOT =   SPACE   ) AND
                (COMB-SYU-HKNKOHSBTKBN      =   "8"     ) AND
                (WK-1001-JIHIPOSITION   NOT =   "2"     ) AND
                (COMB-HOJOKBN               =   "1"     ) AND
                (LNK-SYU-SRYYMD(1:6)       >=   "200704"))
      *
      *      ³ºÅöÊÝ¸±ÈÖ¹æ¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
             INITIALIZE                      HKNNUM-REC
             MOVE   COMB-HOSPID        TO    HKN-HOSPID
             MOVE   COMB-HKNNUM        TO    HKN-HKNNUM
             MOVE   "00"               TO    WK-PAYKBN
             MOVE   HKNNUM-REC         TO    MCPDATA-REC
             MOVE   "DBSELECT"         TO    MCP-FUNC
             MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
             CALL   "ORCMCPSUB"         USING MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
             IF      MCP-RC            =     ZERO
                 MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
                 PERFORM  HKNNUM-READ-SEC
                 PERFORM  UNTIL
                  ( FLG-HKNNUM           =  1                   ) OR
                  ((HKN-TEKSTYMD(1:6)   <=  LNK-SYU-SRYYMD(1:6)) AND
                   (LNK-SYU-SRYYMD(1:6) <=  HKN-TEKEDYMD(1:6)  ) AND
                   (WK-PAYKBN            =  HKN-PAYKBN         ))
                     PERFORM   HKNNUM-READ-SEC
                 END-PERFORM
             END-IF
      *
             MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH
             CALL    "ORCMCPSUB"           USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
             IF  HKN-TENTNK   =   ZERO
                 MOVE     10      TO      HKN-TENTNK
             END-IF
      *
             IF   COMB-HONKZKKBN        =   "1"
               IF  HKN-HON-GAIKFTNRATE   NOT =   ZERO
                  IF  HKN-TENTNK   =   10
                      COMPUTE  WK-JIHIFTN   =
                               HKN-HON-GAIKFTNRATE  /  100
                  ELSE
                      MOVE       1     TO      WK-JIHIFTN
                  END-IF
               ELSE
                  MOVE     ZERO        TO      WK-JIHIFTN
               END-IF
             ELSE
               IF  HKN-KZK-GAIKFTNRATE   NOT =   ZERO
                  IF  HKN-TENTNK   =   10
                      COMPUTE  WK-JIHIFTN   =
                               HKN-KZK-GAIKFTNRATE  /  100
                  ELSE
                      MOVE       1     TO      WK-JIHIFTN
                  END-IF
               ELSE
                  MOVE     ZERO        TO      WK-JIHIFTN
               END-IF
             END-IF
      *
             IF     WK-JIHIFTN    NOT =    ZERO
               PERFORM   VARYING   IDX9    FROM    1   BY  1
                          UNTIL    IDX9       >   16
                 IF   (LNK-SYU-SRYYMD(1:6)       >=   "200704")
                 AND  (COMB-HOJOKBN               =   "1"     )
                    COMPUTE WK-JIHIGAKU   =
                            LNK-SYU-HKNTEN (IDX9) *  HKN-TENTNK
                    COMPUTE WK-JIHIGAKU2  =
                            WK-JIHIGAKU           *  WK-JIHIFTN
                    IF    WK-TAX1001-KBN  =  "1"
                       COMPUTE WK-JIHIGAKU3  =
                              (WK-JIHIGAKU2  * 0.05) + 0.5
                    ELSE
                       COMPUTE WK-JIHIGAKU3  =
                              (WK-JIHIGAKU2  * 0.05)
                    END-IF
                    IF    IDX9  =  16
                       MOVE   ZERO  TO  WK-JIHIGAKU2
                       MOVE   ZERO  TO  WK-JIHIGAKU3
                    END-IF
                    IF    WK-1001-JIHIPOSITION  =   "2"
                       COMPUTE LNK-SYU-TGMONEY-TAX(IDX9) =
                               LNK-SYU-TGMONEY-TAX(IDX9) + WK-JIHIGAKU2
                                                         + WK-JIHIGAKU3
                       MOVE    ZERO    TO      LNK-SYU-HKNTEN(IDX9)
                    ELSE
                       COMPUTE WK-SKYMONEY-ZEI = WK-SKYMONEY-ZEI
                                               + WK-JIHIGAKU3
                    END-IF
                 ELSE
                    COMPUTE WK-JIHIGAKU   =
                            LNK-SYU-HKNTEN (IDX9) *  HKN-TENTNK
                    COMPUTE WK-JIHIGAKU2  =
                            WK-JIHIGAKU           *  WK-JIHIFTN
                    IF    IDX9  =  16
                       MOVE   ZERO  TO  WK-JIHIGAKU2
                    END-IF
                    COMPUTE LNK-SYU-TGMONEY(IDX9) =
                            LNK-SYU-TGMONEY(IDX9) +  WK-JIHIGAKU2
                    MOVE    ZERO    TO      LNK-SYU-HKNTEN(IDX9)
                 END-IF
               END-PERFORM
             END-IF
      *
           END-IF
      *
           .
      *
       104-JIHIHOKEN-DATA-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¤Î¾å¸Â¶èÊ¬ÊÔ½¸                                *
      ******************************************************************
       105-JYOGENKBN-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE    TO   COMB-KOH1-GAIJGNKBN.
           MOVE    SPACE    TO   COMB-KOH2-GAIJGNKBN.
           MOVE    SPACE    TO   COMB-KOH3-GAIJGNKBN.
           MOVE    SPACE    TO   COMB-KOH4-GAIJGNKBN.
      *
      *    ¸øÈñ£±¤Î¾å¸Â¶èÊ¬ÊÔ½¸
           IF  COMB-KOH1HKNNUM           NOT =  SPACE
      *
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-KOH1HKNNUM    TO    HKN-HKNNUM
               IF     COMB-KOH1HKNNUM    =     "027"
                   IF    LNK-SYU-SRYYMD  >=    "20021001"
                       MOVE     COMB-KOH1PAYKBN     TO  WK-PAYKBN
                   ELSE
                       MOVE     WK-1001-HOSPSBT     TO  WK-PAYKBN(1:1)
                       MOVE     WK-1001-ROUPAYKBN   TO  WK-PAYKBN(2:1)
                   END-IF
               ELSE
                       MOVE     "00"         TO    WK-PAYKBN
               END-IF
               MOVE   HKNNUM-REC         TO    MCPDATA-REC
               MOVE   "DBSELECT"         TO    MCP-FUNC
               MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
               CALL   "ORCMCPSUB"         USING MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC            =     ZERO
                   MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   PERFORM  UNTIL
                     (FLG-HKNNUM     =    1)                       OR
                    ((HKN-TEKSTYMD(1:6)   <=  LNK-SYU-SRYYMD(1:6)) AND
                     (LNK-SYU-SRYYMD(1:6) <=  HKN-TEKEDYMD(1:6))   AND
                     (WK-PAYKBN            =  HKN-PAYKBN))
                       PERFORM   HKNNUM-READ-SEC
                   END-PERFORM
                   IF     FLG-HKNNUM         =       0
                          PERFORM   1051-JYOGENKBN-CHECK-SEC
                          MOVE   WK-GAIJGNKBN  TO  COMB-KOH1-GAIJGNKBN
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
      *
           END-IF.
      *
      *    ¸øÈñ£²¤Î¾å¸Â¶èÊ¬ÊÔ½¸
           IF  COMB-KOH2HKNNUM           NOT = SPACE
      *
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-KOH2HKNNUM    TO    HKN-HKNNUM
               MOVE     "00"             TO    WK-PAYKBN
               MOVE   HKNNUM-REC         TO    MCPDATA-REC
               MOVE   "DBSELECT"         TO    MCP-FUNC
               MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
               CALL   "ORCMCPSUB"         USING MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC            =     ZERO
                   MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   PERFORM  UNTIL
                     (FLG-HKNNUM     =    1)                      OR
                    ((HKN-TEKSTYMD(1:6)   <= LNK-SYU-SRYYMD(1:6)) AND
                     (LNK-SYU-SRYYMD(1:6) <= HKN-TEKEDYMD(1:6))   AND
                     (WK-PAYKBN            = HKN-PAYKBN))
                       PERFORM   HKNNUM-READ-SEC
                   END-PERFORM
                   IF     FLG-HKNNUM         =       0
                          PERFORM   1051-JYOGENKBN-CHECK-SEC
                          MOVE   WK-GAIJGNKBN  TO  COMB-KOH2-GAIJGNKBN
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
      *
           END-IF.
      *
      *    ¸øÈñ£³¤Î¾å¸Â¶èÊ¬ÊÔ½¸
           IF  COMB-KOH3HKNNUM           NOT = SPACE
      *
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-KOH3HKNNUM    TO    HKN-HKNNUM
               MOVE     "00"             TO    WK-PAYKBN
               MOVE   HKNNUM-REC         TO    MCPDATA-REC
               MOVE   "DBSELECT"         TO    MCP-FUNC
               MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
               CALL   "ORCMCPSUB"         USING MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC            =     ZERO
                   MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   PERFORM  UNTIL
                     (FLG-HKNNUM     =    1)                      OR
                    ((HKN-TEKSTYMD(1:6)   <= LNK-SYU-SRYYMD(1:6)) AND
                     (LNK-SYU-SRYYMD(1:6) <= HKN-TEKEDYMD(1:6))   AND
                     (WK-PAYKBN            = HKN-PAYKBN))
                       PERFORM   HKNNUM-READ-SEC
                   END-PERFORM
                   IF     FLG-HKNNUM         =       0
                          PERFORM   1051-JYOGENKBN-CHECK-SEC
                          MOVE   WK-GAIJGNKBN  TO  COMB-KOH3-GAIJGNKBN
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
      *
           END-IF.
      *
      *    ¸øÈñ£´¤Î¾å¸Â¶èÊ¬ÊÔ½¸
           IF  COMB-KOH4HKNNUM           NOT = SPACE
      *
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-KOH4HKNNUM    TO    HKN-HKNNUM
               MOVE     "00"             TO    WK-PAYKBN
               MOVE   HKNNUM-REC         TO    MCPDATA-REC
               MOVE   "DBSELECT"         TO    MCP-FUNC
               MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
               CALL   "ORCMCPSUB"         USING MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC            =     ZERO
                   MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   PERFORM  UNTIL
                     (FLG-HKNNUM     =    1)                       OR
                    ((HKN-TEKSTYMD(1:6)   <= LNK-SYU-SRYYMD(1:6)) AND
                     (LNK-SYU-SRYYMD(1:6) <= HKN-TEKEDYMD(1:6))   AND
                     (WK-PAYKBN            = HKN-PAYKBN))
                       PERFORM   HKNNUM-READ-SEC
                   END-PERFORM
                   IF     FLG-HKNNUM         =       0
                          PERFORM   1051-JYOGENKBN-CHECK-SEC
                          MOVE   WK-GAIJGNKBN  TO  COMB-KOH4-GAIJGNKBN
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
      *
           END-IF.
      *
       105-JYOGENKBN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¤Î¾å¸Â¶èÊ¬¥Á¥§¥Ã¥¯                                *
      ******************************************************************
       1051-JYOGENKBN-CHECK-SEC           SECTION.
      *
           MOVE     SPACE     TO    WK-GAIJGNKBN.
      *
           IF    HKN-HKNNUM(1:1)   >=  "1" AND <=  "8"
             IF  WK-1001-CHIHOUNUMKBN       =   "1"
               EVALUATE   FLG-SANSHOU2
                 WHEN   "1"
                      IF   HKN-HON-GAIMNAIJGNGAK  NOT = ZERO
                      OR   HKN-HON-GAIMGAIJGNGAK  NOT = ZERO
                      OR   HKN-HON-GAIMJGNCNT     NOT = ZERO
                      OR   HKN-HON-GAIDJGNGAK     NOT = ZERO
                      OR   HKN-HON-GAIDJGNCNT     NOT = ZERO
                      OR   HKN-HON-GAIKJGNGAK     NOT = ZERO
                             MOVE     "1"      TO     WK-GAIJGNKBN
                      END-IF
                 WHEN   "2"
                      IF   HKN-TSY-GAIFTNKBN      =  "0"
                        IF   HKN-HON-GAIMNAIJGNGAK  NOT = ZERO
                        OR   HKN-HON-GAIMGAIJGNGAK  NOT = ZERO
                        OR   HKN-HON-GAIMJGNCNT     NOT = ZERO
                        OR   HKN-HON-GAIDJGNGAK     NOT = ZERO
                        OR   HKN-HON-GAIDJGNCNT     NOT = ZERO
                        OR   HKN-HON-GAIKJGNGAK     NOT = ZERO
                             MOVE     "1"      TO     WK-GAIJGNKBN
                        END-IF
                      ELSE
                        IF   HKN-TSY-GAIMNAIJGNGAK  NOT = ZERO
                        OR   HKN-TSY-GAIMGAIJGNGAK  NOT = ZERO
                        OR   HKN-TSY-GAIMJGNCNT     NOT = ZERO
                        OR   HKN-TSY-GAIDJGNGAK     NOT = ZERO
                        OR   HKN-TSY-GAIDJGNCNT     NOT = ZERO
                        OR   HKN-TSY-GAIKJGNGAK     NOT = ZERO
                             MOVE     "1"      TO     WK-GAIJGNKBN
                        END-IF
                      END-IF
                 WHEN   "3"
                      IF  HKN-TNK-GAIFTNKBN       =  "0"
                        IF   HKN-HON-GAIMNAIJGNGAK  NOT = ZERO
                        OR   HKN-HON-GAIMGAIJGNGAK  NOT = ZERO
                        OR   HKN-HON-GAIMJGNCNT     NOT = ZERO
                        OR   HKN-HON-GAIDJGNGAK     NOT = ZERO
                        OR   HKN-HON-GAIDJGNCNT     NOT = ZERO
                        OR   HKN-HON-GAIKJGNGAK     NOT = ZERO
                             MOVE     "1"      TO     WK-GAIJGNKBN
                        END-IF
                      ELSE
                        IF   HKN-TNK-GAIMNAIJGNGAK  NOT = ZERO
                        OR   HKN-TNK-GAIMGAIJGNGAK  NOT = ZERO
                        OR   HKN-TNK-GAIMJGNCNT     NOT = ZERO
                        OR   HKN-TNK-GAIDJGNGAK     NOT = ZERO
                        OR   HKN-TNK-GAIDJGNCNT     NOT = ZERO
                        OR   HKN-TNK-GAIKJGNGAK     NOT = ZERO
                             MOVE     "1"      TO     WK-GAIJGNKBN
                        END-IF
                      END-IF
               END-EVALUATE
             ELSE
               IF (( WK-1001-PREFNUM   =  28                      ) AND
                   ( HKN-HKNNUM        = "241" OR "180" OR
                                         "182" OR "143" OR
                                         "185" OR "127"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200507"          ))
               OR (( WK-1001-PREFNUM   =  11                      ) AND
                   ( HKN-HKNNUM        = "344"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200507"          ))
               OR (( WK-1001-PREFNUM   =  05                      ) AND
                   ( HKN-HKNNUM        = "374" OR "474"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200508"          ))
               OR (( WK-1001-PREFNUM   =  32                      ) AND
                   ( HKN-HKNNUM        = "191" OR "391"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200510"          ))
               OR (( WK-1001-PREFNUM   =  28                      ) AND
                   ( HKN-HKNNUM        = "100"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200604"          ))
               OR (( WK-1001-PREFNUM   =  33                      ) AND
                   ( HKN-HKNNUM        = "141" OR "285" OR
                                         "485" OR "180" OR "186"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200610"          ))
               OR (( WK-1001-PREFNUM   =  19                      ) AND
                   ( HKN-HKNNUM        = "141"                    ))
               OR (( WK-1001-PREFNUM   =  23                      ) AND
                   ( HKN-HKNNUM        = "141"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200610"          ))
                 EVALUATE   FLG-SANSHOU2
                   WHEN   "1"
                      IF   HKN-HON-GAIMNAIJGNGAK  NOT = ZERO
                      OR   HKN-HON-GAIMGAIJGNGAK  NOT = ZERO
                      OR   HKN-HON-GAIMJGNCNT     NOT = ZERO
                      OR   HKN-HON-GAIDJGNGAK     NOT = ZERO
                      OR   HKN-HON-GAIDJGNCNT     NOT = ZERO
                      OR   HKN-HON-GAIKJGNGAK     NOT = ZERO
                             MOVE     "1"      TO     WK-GAIJGNKBN
                      END-IF
                   WHEN   "2"
                      IF   HKN-TSY-GAIFTNKBN      =  "0"
                        IF   HKN-HON-GAIMNAIJGNGAK  NOT = ZERO
                        OR   HKN-HON-GAIMGAIJGNGAK  NOT = ZERO
                        OR   HKN-HON-GAIMJGNCNT     NOT = ZERO
                        OR   HKN-HON-GAIDJGNGAK     NOT = ZERO
                        OR   HKN-HON-GAIDJGNCNT     NOT = ZERO
                        OR   HKN-HON-GAIKJGNGAK     NOT = ZERO
                             MOVE     "1"      TO     WK-GAIJGNKBN
                        END-IF
                      ELSE
                        IF   HKN-TSY-GAIMNAIJGNGAK  NOT = ZERO
                        OR   HKN-TSY-GAIMGAIJGNGAK  NOT = ZERO
                        OR   HKN-TSY-GAIMJGNCNT     NOT = ZERO
                        OR   HKN-TSY-GAIDJGNGAK     NOT = ZERO
                        OR   HKN-TSY-GAIDJGNCNT     NOT = ZERO
                        OR   HKN-TSY-GAIKJGNGAK     NOT = ZERO
                             MOVE     "1"      TO     WK-GAIJGNKBN
                        END-IF
                      END-IF
                   WHEN   "3"
                      IF  HKN-TNK-GAIFTNKBN       =  "0"
                        IF   HKN-HON-GAIMNAIJGNGAK  NOT = ZERO
                        OR   HKN-HON-GAIMGAIJGNGAK  NOT = ZERO
                        OR   HKN-HON-GAIMJGNCNT     NOT = ZERO
                        OR   HKN-HON-GAIDJGNGAK     NOT = ZERO
                        OR   HKN-HON-GAIDJGNCNT     NOT = ZERO
                        OR   HKN-HON-GAIKJGNGAK     NOT = ZERO
                             MOVE     "1"      TO     WK-GAIJGNKBN
                        END-IF
                      ELSE
                        IF   HKN-TNK-GAIMNAIJGNGAK  NOT = ZERO
                        OR   HKN-TNK-GAIMGAIJGNGAK  NOT = ZERO
                        OR   HKN-TNK-GAIMJGNCNT     NOT = ZERO
                        OR   HKN-TNK-GAIDJGNGAK     NOT = ZERO
                        OR   HKN-TNK-GAIDJGNCNT     NOT = ZERO
                        OR   HKN-TNK-GAIKJGNGAK     NOT = ZERO
                             MOVE     "1"      TO     WK-GAIJGNKBN
                        END-IF
                      END-IF
                 END-EVALUATE
               ELSE
                 IF   HKN-HON-GAIMNAIJGNGAK  NOT = ZERO
                 OR   HKN-HON-GAIMGAIJGNGAK  NOT = ZERO
                 OR   HKN-HON-GAIMJGNCNT     NOT = ZERO
                 OR   HKN-HON-GAIDJGNGAK     NOT = ZERO
                 OR   HKN-HON-GAIDJGNCNT     NOT = ZERO
                 OR   HKN-HON-GAIKJGNGAK     NOT = ZERO
                        MOVE     "1"      TO     WK-GAIJGNKBN
                 END-IF
               END-IF
             END-IF
           ELSE
             IF   HKN-HON-GAIMNAIJGNGAK  NOT = ZERO
             OR   HKN-HON-GAIMGAIJGNGAK  NOT = ZERO
             OR   HKN-HON-GAIMJGNCNT     NOT = ZERO
             OR   HKN-HON-GAIDJGNGAK     NOT = ZERO
             OR   HKN-HON-GAIDJGNCNT     NOT = ZERO
             OR   HKN-HON-GAIKJGNGAK     NOT = ZERO
                        MOVE     "1"      TO     WK-GAIJGNKBN
             END-IF
           END-IF.
      *
       1051-JYOGENKBN-CHECK-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÊÝ¸±ÉéÃ´³Û»»Äê½èÍý                                      *
      ******************************************************************
       200-HKNFTN-SANTEI-SEC          SECTION.
      *
           MOVE    SPACE                 TO    SHOHOUSEN-AREA
                                               HOKEN-MAX-AREA
                                               HOKEN-KAISU-AREA
                                               HOKEN-KAISU-TABLE
                                               HOKEN-FUTAN-AREA
                                               HOKEN-FUTAN-AREA2        
                                               HOKEN-FUTAN-AREA3
                                               HOKEN-ZAN-AREA
                                               HOKEN-ZAN-AREA2          
                                               HOKEN-ZAN-AREA3
                                               WK-YAKFTN
           INITIALIZE                          SHOHOUSEN-AREA
                                               HOKEN-MAX-AREA
                                               HOKEN-KAISU-AREA
                                               HOKEN-KAISU-TABLE
                                               HOKEN-FUTAN-AREA
                                               HOKEN-FUTAN-AREA2        
                                               HOKEN-FUTAN-AREA3
                                               HOKEN-ZAN-AREA
                                               HOKEN-ZAN-AREA2          
                                               HOKEN-ZAN-AREA3
                                               WK-YAKFTN
           MOVE    ALL"9"                TO    WK-KOH-COMPFTN-AREA     
      *
      *    ¼«ÇåÀÕ¡¦ÎÅÍÜÃ´Åö¼êÅöÍÑÆÃÎã½èÍý
      *    ¡Ê¼«ÇåÀÕ¤Î¾ì¹ç¡¢£·ÅÀ¤ò£¸£´±ß¤ËÆÉ¤ßÂØ¤¨¤ë¡Ë
      *    ¡ÊÎÅÍÜÃ´Åö¼êÅö¡§£±·î£±²ó¤Î»»Äê¡Ë
           IF    LNK-SYU-SYUHKNNUM       =    "973"
           AND   LNK-S01-RYOYO(IDY)  NOT =    ZERO
               COMPUTE  LNK-SYU-HKNTEN (15)  =  LNK-SYU-HKNTEN (15)
                                             -  LNK-S01-RYOYO(IDY)
               COMPUTE  LNK-SYU-HKNTEN (16)  =  LNK-SYU-HKNTEN (16)
                                             -  LNK-S01-RYOYO(IDY)
      *
               COMPUTE  WK-RYOYO  =  LNK-S01-RYOYO(IDY)  /   7
      *
               COMPUTE  LNK-SYU-RSIETC-MONEY =  LNK-SYU-RSIETC-MONEY
                                             +  ( 84 *  WK-RYOYO )
           END-IF
      *
      *Ê¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ
      *¹âÎð¼Ô¤ÎºßÂðÁí¹ç¡¦ºßÂðËö´ü»»Äê´µ¼Ô¤ÎÉéÃ´¾å¸Â»»Äê¥¨¥ê¥¢½é´ü²½
      *
           INITIALIZE    WK-SYUHKN-TAIHI-AREA
      *
      *    ¸øÈñÃ±ÆÈ¤ÇÏ·¿ÍÊÝ¸±ÆþÎÏ¤¬¤¢¤ë¤«¥Á¥§¥Ã¥¯
           IF      COMB-HKNNUM          =   SPACE
           AND     COMB-KOH1HKNNUM  NOT =   SPACE
           AND     COMB-KOH2HKNNUM      =   SPACE
           AND     COMB-KOH3HKNNUM      =   SPACE
           AND     COMB-KOH4HKNNUM      =   SPACE
                INITIALIZE                       PTKOHINF-REC
                MOVE    LNK-SYU-HOSPID       TO  PTKOH-HOSPID
                MOVE    LNK-SYU-PTID         TO  PTKOH-PTID
                MOVE    "027"                TO  PTKOH-KOHNUM
                MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                MOVE    "DBSELECT"           TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"           USING   MCPAREA
                                                      ORCMCPAREA
                                                      MCPDATA-REC
                IF      MCP-RC               =       ZERO
                    MOVE    "PTKOHINF-KEY4"  TO      ORC-DBPATH
                    PERFORM  PTKOHINF-READ-SEC
                    PERFORM  UNTIL   FLG-PTKOHINF    =   1
                        IF      PTKOH-TEKSTYMD(1:6)  <=
                                                 LNK-SYU-SRYYMD(1:6)
                        AND     LNK-SYU-SRYYMD(1:6)  <=
                                                 PTKOH-TEKEDYMD(1:6)
                        AND     PTKOH-SAKJOKBN   =  SPACE
                           MOVE   "1"           TO  FLG-ROUJINHOKEN
                           MOVE    1            TO  FLG-PTKOHINF     
                        ELSE
                           MOVE    "PTKOHINF-KEY4"  TO  ORC-DBPATH
                           PERFORM  PTKOHINF-READ-SEC
                        END-IF   
                    END-PERFORM
                END-IF
                MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"          USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           END-IF
      *
           IF    LNK-SYU-SRYYMD(1:6)    >=   "200210"
      *        ´µ¼ÔÇ¯Îð·×»»
               INITIALIZE                           PTINF-REC
               MOVE    LNK-SYU-HOSPID       TO      PTINF-HOSPID
               MOVE    LNK-SYU-PTID         TO      PTINF-PTID
               MOVE    PTINF-REC            TO      MCPDATA-REC
               MOVE    "DBSELECT"           TO      MCP-FUNC
               MOVE    "PTINF-KEY"          TO      ORC-DBPATH
               CALL    "ORCMCPSUB"          USING   MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
               IF      MCP-RC               =       ZERO
                   MOVE    "PTINF-KEY"      TO      ORC-DBPATH
                   PERFORM  PTINF-READ-SEC
               ELSE
                   DISPLAY "PTINF SELECT ERR"
                   INITIALIZE      PTINF-REC
               END-IF
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "PTINF-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               INITIALIZE                  ORCSAGECHKAREA
               MOVE    LNK-SYU-HOSPID  TO  AGECHK-HOSPID
               MOVE    LNK-SYU-PTID    TO  AGECHK-PTID
               MOVE    LNK-SYU-SRYYMD  TO  AGECHK-SRYYMD
               MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
               IF      AGECHK-RC       =   ZERO
                   IF  ( AGECHK-NENREI   >=  70      ) OR
                       ( AGECHK-NENREI   >=  65  AND
                         COMB-KOH1HKNNUM  = "027"    ) OR
                       ( AGECHK-NENREI   >=  65  AND
                         FLG-ROUJINHOKEN  = "1"      )
                       MOVE    "1"     TO  FLG-KOUREI-KBN
                   END-IF
               ELSE
                   DISPLAY  "PTINF AGE-CHK ERR " LNK-SYU-PTID
               END-IF
           END-IF
      *
      *    £·£°ºÐÌ¤Ëþ¹â³Û¸½ÊªµëÉÕÂÐ¾Ý¼Ô
           MOVE      ZERO     TO    FLG-GENBUTU70
           IF     LNK-SYU-SRYYMD(1:6)       >=  "200704"
             IF  (FLG-KOUREI-KBN             =   "1"  )
             OR  (COMB-HKNNUM                =   SPACE)
             OR  (COMB-SYU-HKNKOHSBTKBN      =   "8"  )
             OR  (COMB-HKNNUM  =  "971" OR "973" OR
                                  "068" OR "069"      )
               CONTINUE
             ELSE
               PERFORM   VARYING   GEN-IDX   FROM   1    BY   1
                          UNTIL   (GEN-IDX    >    TBL-GENBUTU70-MAX)
                            OR    (FLG-GENBUTU70   NOT = ZERO       )
                 INITIALIZE                         PTKOHINF-REC
                 MOVE    LNK-SYU-HOSPID         TO  PTKOH-HOSPID
                 MOVE    LNK-SYU-PTID           TO  PTKOH-PTID
                 MOVE    TBL-GENBUTU70(GEN-IDX) TO  PTKOH-KOHNUM
                 MOVE    PTKOHINF-REC           TO  MCPDATA-REC
                 MOVE    "DBSELECT"             TO  MCP-FUNC
                 MOVE    "PTKOHINF-KEY4"        TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"            USING   MCPAREA
                                                        ORCMCPAREA
                                                        MCPDATA-REC
                 IF      MCP-RC               =       ZERO
                     MOVE    "PTKOHINF-KEY4"  TO      ORC-DBPATH
                     PERFORM  PTKOHINF-READ-SEC
                     PERFORM  UNTIL   FLG-PTKOHINF    =   1
                        IF  (PTKOH-TEKSTYMD(1:6)  <=
                                                 LNK-SYU-SRYYMD(1:6))
                        AND (LNK-SYU-SRYYMD(1:6)  <=
                                                 PTKOH-TEKEDYMD(1:6))
                        AND (PTKOH-SAKJOKBN   =  SPACE              )
                           EVALUATE   GEN-IDX
                             WHEN        1
                               MOVE    1   TO  FLG-GENBUTU70
                             WHEN        2
                               IF    FLG-SANSHOU3  =  SPACE
                                 MOVE  2   TO  FLG-GENBUTU70
                               ELSE
                                 MOVE  3   TO  FLG-GENBUTU70
                               END-IF
                           END-EVALUATE
                           MOVE    1       TO  FLG-PTKOHINF
                        ELSE
                           MOVE    "PTKOHINF-KEY4"  TO  ORC-DBPATH
                           PERFORM  PTKOHINF-READ-SEC
                        END-IF
                     END-PERFORM
                 END-IF
                 MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
                 MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"          USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
               END-PERFORM
             END-IF
           END-IF
      *
           IF   FLG-KOUREI-KBN        =   SPACE
             IF   FLG-GENBUTU70   NOT =   ZERO
               IF   LNK-S01-KOUREI-ZAITAKU-KBN  =  "1"
                 MOVE     SPACE  TO  LNK-S01-KOUREI-ZAITAKU-KBN
               ELSE
                 PERFORM   2002-ZAITAKU-HENSYU200604-SEC
                 IF LNK-S01-KOUREI-ZAITAKU-KBN  =  "1"
                   MOVE   SPACE  TO  LNK-S01-KOUREI-ZAITAKU-KBN
                 ELSE
                   MOVE   ZERO   TO  FLG-GENBUTU70
                 END-IF
               END-IF
             ELSE
               IF   LNK-S01-KOUREI-ZAITAKU-KBN  =  "1"
                 MOVE     SPACE  TO  LNK-S01-KOUREI-ZAITAKU-KBN
               END-IF
             END-IF
           ELSE
             IF     LNK-S01-KOUREI-ZAITAKU-KBN  =  SPACE
               IF   LNK-SYU-SRYYMD(1:6)   >=   "200604"
                    PERFORM   2002-ZAITAKU-HENSYU200604-SEC
               ELSE
                    PERFORM   2001-ZAITAKU-HENSYU-SEC
               END-IF
             END-IF
           END-IF
      *
           IF   LNK-S01-KOUREI-ZAITAKU-KBN   =  "1"
               IF  COMB-SYU-HKNKOHSBTKBN   =   "8"
                   MOVE  SPACE       TO  LNK-S01-KOUREI-ZAITAKU-KBN
               END-IF
               IF  COMB-HKNNUM  =  "971"  OR  "973"  OR
                                   "068"  OR  "069"
                   MOVE  SPACE       TO  LNK-S01-KOUREI-ZAITAKU-KBN
               END-IF
           END-IF
      *
           IF     (LNK-S01-KOUREI-ZAITAKU-KBN  =  "1" )
           OR     (FLG-GENBUTU70           NOT =  ZERO)
              MOVE      "1"          TO  LNK-SYU-ZAITAKU
           END-IF
      *
           IF   FLG-KOUREI-KBN  =  "1"
               IF  COMB-SYU-HKNKOHSBTKBN   =   "8"
                   MOVE  SPACE       TO  FLG-KOUREI-KBN
               END-IF
               IF  COMB-HKNNUM  =  "971"  OR  "973"  OR
                                   "068"  OR  "069"
                   MOVE  SPACE       TO  FLG-KOUREI-KBN
               END-IF
           END-IF
      *
      *    °ìÈÌ·Ð²áÁ¼ÃÖÂÐ¾Ý¼Ô
           MOVE      ZERO     TO    FLG-KEIKASOCHI-IPAN
           IF  (LNK-S01-KOUREI-ZAITAKU-KBN    =   "1"    )
           AND (LNK-SYU-SRYYMD(1:6)          >=  "200608")
           AND (LNK-SYU-SRYYMD(1:6)          <=  "200808")
                INITIALIZE                       PTKOHINF-REC
                MOVE    LNK-SYU-HOSPID       TO  PTKOH-HOSPID
                MOVE    LNK-SYU-PTID         TO  PTKOH-PTID
                MOVE    "978"                TO  PTKOH-KOHNUM
                MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                MOVE    "DBSELECT"           TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"           USING   MCPAREA
                                                      ORCMCPAREA
                                                      MCPDATA-REC
                IF      MCP-RC               =       ZERO
                    MOVE    "PTKOHINF-KEY4"  TO      ORC-DBPATH
                    PERFORM  PTKOHINF-READ-SEC
                    PERFORM  UNTIL   FLG-PTKOHINF    =   1
                        IF  (PTKOH-TEKSTYMD(1:6)  <=
                                                 LNK-SYU-SRYYMD(1:6))
                        AND (LNK-SYU-SRYYMD(1:6)  <=
                                                 PTKOH-TEKEDYMD(1:6))
                        AND (PTKOH-SAKJOKBN   =  SPACE              )
                           MOVE    1         TO  FLG-KEIKASOCHI-IPAN
                           MOVE    1         TO  FLG-PTKOHINF
                        ELSE
                           MOVE    "PTKOHINF-KEY4"  TO  ORC-DBPATH
                           PERFORM  PTKOHINF-READ-SEC
                        END-IF
                    END-PERFORM
                END-IF
                MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"          USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           END-IF
      *
      *    ¸º³Û¡¦ÌÈ½ü¡¦»ÙÊ§Í±Í½ÂÐ¾Ý¼Ô
           MOVE     ZERO     TO     WK-GENGAKUKBN
           MOVE     ZERO     TO     WK-GENGAKU-KOHID
      *
           IF   (LNK-SYU-SRYYMD(1:6)       >=  "200610")
           AND  (COMB-HKNNUM            NOT =   SPACE  )
      *
            IF  (COMB-SYU-HKNKOHSBTKBN      =   "8"              )
            OR  (COMB-HKNNUM  =  "971" OR "973" OR "068" OR "069")
              CONTINUE
            ELSE
              PERFORM   VARYING   GEN-IDX   FROM   1    BY   1
                         UNTIL   (GEN-IDX    >    TBL-GENGAKU-MAX)
                           OR    (WK-GENGAKUKBN   NOT = ZERO     )
                INITIALIZE                       PTKOHINF-REC
                MOVE    LNK-SYU-HOSPID       TO  PTKOH-HOSPID
                MOVE    LNK-SYU-PTID         TO  PTKOH-PTID
                MOVE    TBL-GENGAKU(GEN-IDX) TO  PTKOH-KOHNUM
                MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                MOVE    "DBSELECT"           TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"           USING   MCPAREA
                                                      ORCMCPAREA
                                                      MCPDATA-REC
                IF      MCP-RC               =       ZERO
                    MOVE    "PTKOHINF-KEY4"  TO      ORC-DBPATH
                    PERFORM  PTKOHINF-READ-SEC
                    PERFORM  UNTIL   FLG-PTKOHINF    =   1
                        IF  (PTKOH-TEKSTYMD(1:6)  <=
                                                 LNK-SYU-SRYYMD(1:6))
                        AND (LNK-SYU-SRYYMD(1:6)  <=
                                                 PTKOH-TEKEDYMD(1:6))
                        AND (PTKOH-SAKJOKBN   =  SPACE              )
                           EVALUATE   GEN-IDX
                             WHEN        1
                               MOVE    1     TO  WK-GENGAKUKBN
                               MOVE   PTKOH-KOHID
                                             TO  WK-GENGAKU-KOHID
                             WHEN        2
                               MOVE    2     TO  WK-GENGAKUKBN
                               MOVE   PTKOH-KOHID
                                             TO  WK-GENGAKU-KOHID
                             WHEN        3
                               MOVE    3     TO  WK-GENGAKUKBN
                             WHEN        4
                               MOVE    4     TO  WK-GENGAKUKBN
                           END-EVALUATE
                           MOVE    1         TO  FLG-PTKOHINF
                        ELSE
                           MOVE    "PTKOHINF-KEY4"  TO  ORC-DBPATH
                           PERFORM  PTKOHINF-READ-SEC
                        END-IF
                    END-PERFORM
                END-IF
                MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"          USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
              END-PERFORM
            END-IF
      *
            IF   WK-GENGAKUKBN    =   2
                   MOVE     ZERO    TO    WK-GENGAKUKBN
            END-IF
            MOVE   WK-GENGAKUKBN    TO    LNK-SYU-YKZKENNUM
      *
           END-IF
      *
      *    ¹â³Û°ÑÇ¤Ê§¤¤À©ÅÙÂÐ¾Ý¼Ô
           MOVE      ZERO     TO    FLG-KOHININBARAI
           MOVE      ZERO     TO    WK-KOHININBARAI-KOHID
           IF   LNK-S01-KOUREI-ZAITAKU-KBN    =   "1"
                CONTINUE
           ELSE
            IF   (COMB-HKNNUM      NOT =  SPACE         )
            AND  (COMB-KOH1HKNNUM      =  SPACE OR "027")
            AND  (COMB-KOH2HKNNUM      =  SPACE         )
            AND  (COMB-KOH3HKNNUM      =  SPACE         )
            AND  (COMB-KOH4HKNNUM      =  SPACE         )
              IF  (COMB-SYU-HKNKOHSBTKBN      =   "8"              )
              OR  (COMB-HKNNUM  =  "971" OR "973" OR "068" OR "069")
                CONTINUE
              ELSE
                INITIALIZE                       PTKOHINF-REC
                MOVE    LNK-SYU-HOSPID       TO  PTKOH-HOSPID
                MOVE    LNK-SYU-PTID         TO  PTKOH-PTID
                MOVE    "964"                TO  PTKOH-KOHNUM
                MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                MOVE    "DBSELECT"           TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"           USING   MCPAREA
                                                      ORCMCPAREA
                                                      MCPDATA-REC
                IF      MCP-RC               =       ZERO
                    MOVE    "PTKOHINF-KEY4"  TO      ORC-DBPATH
                    PERFORM  PTKOHINF-READ-SEC
                    PERFORM  UNTIL   FLG-PTKOHINF    =   1
                        IF  (PTKOH-TEKSTYMD(1:6)  <=
                                                 LNK-SYU-SRYYMD(1:6))
                        AND (LNK-SYU-SRYYMD(1:6)  <=
                                                 PTKOH-TEKEDYMD(1:6))
                        AND (PTKOH-SAKJOKBN   =  SPACE              )
                           MOVE    1         TO  FLG-KOHININBARAI
                           MOVE   PTKOH-KOHID
                                             TO  WK-KOHININBARAI-KOHID
                           MOVE    1         TO  FLG-PTKOHINF
                        ELSE
                           MOVE    "PTKOHINF-KEY4"  TO  ORC-DBPATH
                           PERFORM  PTKOHINF-READ-SEC
                        END-IF
                    END-PERFORM
                END-IF
                MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"          USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
              END-IF
            END-IF
           END-IF
      *
      *    ¹â³Û£´²óÌÜ°Ê¹ßÂÐ¾Ý¼Ô
           MOVE      ZERO     TO    FLG-KOHYONKAIME
           IF   FLG-GENBUTU70    NOT =  ZERO
                INITIALIZE                       PTKOHINF-REC
                MOVE    LNK-SYU-HOSPID       TO  PTKOH-HOSPID
                MOVE    LNK-SYU-PTID         TO  PTKOH-PTID
                MOVE    "965"                TO  PTKOH-KOHNUM
                MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                MOVE    "DBSELECT"           TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"           USING   MCPAREA
                                                      ORCMCPAREA
                                                      MCPDATA-REC
                IF      MCP-RC               =       ZERO
                    MOVE    "PTKOHINF-KEY4"  TO      ORC-DBPATH
                    PERFORM  PTKOHINF-READ-SEC
                    PERFORM  UNTIL   FLG-PTKOHINF    =   1
                        IF  (PTKOH-TEKSTYMD(1:6)  <=
                                                 LNK-SYU-SRYYMD(1:6))
                        AND (LNK-SYU-SRYYMD(1:6)  <=
                                                 PTKOH-TEKEDYMD(1:6))
                        AND (PTKOH-SAKJOKBN   =  SPACE              )
                           MOVE    1         TO  FLG-KOHYONKAIME
                           MOVE    1         TO  FLG-PTKOHINF
                        ELSE
                           MOVE    "PTKOHINF-KEY4"  TO  ORC-DBPATH
                           PERFORM  PTKOHINF-READ-SEC
                        END-IF
                    END-PERFORM
                END-IF
                MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"          USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           END-IF
      *
           IF    (FLG-GENBUTU70   NOT =  ZERO)
           AND   (FLG-KOHININBARAI    =    1 )
              MOVE   ZERO     TO    FLG-KOHININBARAI
           END-IF
           IF     FLG-GENBUTU70   NOT =  ZERO
              EVALUATE   FLG-GENBUTU70
                WHEN        1
                WHEN        2
                  MOVE     80100   TO   WK-GEN70-GENDOGAKU
                  IF  FLG-KOHYONKAIME = 1
                     MOVE  44400   TO   WK-GEN70-GENDOGAKU
                  END-IF
                WHEN        3
                  MOVE     35400   TO   WK-GEN70-GENDOGAKU
                  IF  FLG-KOHYONKAIME = 1
                     MOVE  24600   TO   WK-GEN70-GENDOGAKU
                  END-IF
              END-EVALUATE
              IF   ( COMB-KOH1HKNNUM  =   "972" )
              OR   ( COMB-KOH2HKNNUM  =   "972" )
              OR   ( COMB-KOH3HKNNUM  =   "972" )
              OR   ( COMB-KOH4HKNNUM  =   "972" )
                 IF   WK-GEN70-GENDOGAKU > 10000
                    MOVE   10000   TO  WK-GEN70-GENDOGAKU
                 END-IF
              END-IF
              IF   ( COMB-KOH1HKNNUM  =   "974" )
              OR   ( COMB-KOH2HKNNUM  =   "974" )
              OR   ( COMB-KOH3HKNNUM  =   "974" )
              OR   ( COMB-KOH4HKNNUM  =   "974" )
                 IF   WK-GEN70-GENDOGAKU > 20000
                    MOVE   20000   TO  WK-GEN70-GENDOGAKU
                 END-IF
              END-IF
           END-IF
      *
      *    Ï«ºÒ¡¦¼«ÇåÀÕ¤ÎºÆ·×»»»þ¤ÎÉéÃ´³ä¹ç¤òµá¤á¤ë
      *
      ***  IF   WK-HKNKBN              =   "1"
      ***  AND  LNK-S01-SAIKEISAN-KBN  =   "1"
           IF   LNK-S01-SAIKEISAN-KBN  =   "1"
      *
               INITIALIZE                           PTINF-REC
               MOVE    LNK-SYU-HOSPID       TO      PTINF-HOSPID
               MOVE    LNK-SYU-PTID         TO      PTINF-PTID
               MOVE    PTINF-REC            TO      MCPDATA-REC
               MOVE    "DBSELECT"           TO      MCP-FUNC
               MOVE    "PTINF-KEY"          TO      ORC-DBPATH
               CALL    "ORCMCPSUB"          USING   MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
               IF      MCP-RC               =       ZERO
                   MOVE    "PTINF-KEY"      TO      ORC-DBPATH
                   PERFORM  PTINF-READ-SEC
               ELSE
                   DISPLAY "PTINF SELECT ERR"
                   INITIALIZE      PTINF-REC
               END-IF
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "PTINF-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               INITIALIZE                  ORCSAGECHKAREA
               MOVE    LNK-SYU-HOSPID  TO  AGECHK-HOSPID
               MOVE    LNK-SYU-PTID    TO  AGECHK-PTID
               MOVE    LNK-SYU-SRYYMD  TO  AGECHK-SRYYMD
               MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
               MOVE    LNK-SYU-HKNCOMBINUM 
                                       TO  AGECHK-HKNCOMBINUM
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
               IF      AGECHK-RC       =   ZERO
                   MOVE   AGECHK-FTNRATE  TO  LNK-SYU-PTFTNRATE
               ELSE
                   DISPLAY  "PTINF AGE-CHK ERR " LNK-SYU-PTID
               END-IF
      *
           END-IF
      *
      *    ÊÝ¸±ÈÖ¹æ¥Æ¡¼¥Ö¥ë¤«¤é¸øÈñ¤ÎÉéÃ´¾å¸Â²ó¿ô¤È¾å¸Â¶â³Û¤Î¼èÆÀ
           PERFORM         210-HOKEN-KAISU-SEC
      *
      *    ¿ÇÎÅ·îÆâ¤Î¼ýÇ¼¥Þ¥¹¥¿¤«¤é¸øÈñËè¤Î»ÈÍÑ²ó¿ô¤È´µ¼ÔÉéÃ´¶â³Û¤Î
      *    Îß·×¤òÄ´ºº
           PERFORM         220-SYUNOU-SEC
      *
      *    ÊÝ¸±¡¦¸øÈñ¤ÎÉéÃ´¶â·×»»
           PERFORM         230-FTNKIN-SEC
      *
           .
      *
       200-HKNFTN-SANTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *        ºßÁí¿ÇÈ½Äê¡Ê£²£°£°£¶Ç¯£´·î¤Þ¤Ç¡Ë                        *
      ******************************************************************
       2001-ZAITAKU-HENSYU-SEC           SECTION.
      *
           PERFORM   VARYING   IDX10   FROM    1   BY  1
                      UNTIL  ( IDX10    >   TBL-ZAITAKU-MAX      )
                       OR    ( LNK-S01-KOUREI-ZAITAKU-KBN  = "1" )
      *
               INITIALIZE                           SRYACT-REC
               MOVE    LNK-SYU-HOSPID       TO      SRY-HOSPID
               MOVE    LNK-SYU-NYUGAIKBN    TO      SRY-NYUGAIKBN
               MOVE    LNK-SYU-PTID         TO      SRY-PTID
               MOVE    LNK-SYU-SRYYMD(1:6)  TO      SRY-SRYYM
               MOVE    TBL-ZAITAKU(IDX10)   TO      SRY-SRYCD(1)
               MOVE    TBL-ZAITAKU(IDX10)   TO      SRY-SRYCD(2)
               MOVE    TBL-ZAITAKU(IDX10)   TO      SRY-SRYCD(3)
               MOVE    TBL-ZAITAKU(IDX10)   TO      SRY-SRYCD(4)
               MOVE    TBL-ZAITAKU(IDX10)   TO      SRY-SRYCD(5)
               MOVE    SRYACT-REC           TO      MCPDATA-REC
               MOVE    "DBSELECT"           TO      MCP-FUNC
               MOVE    "SRYACT-KEY7"        TO      ORC-DBPATH
               CALL    "ORCMCPSUB"           USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
               IF      MCP-RC               =       ZERO
                   MOVE    "SRYACT-KEY7"    TO      ORC-DBPATH 
                   PERFORM   SRYACT-READ-SEC
                   IF      FLG-SRYACT       =       ZERO
                       INITIALIZE                   SRYACCT-REC
                       MOVE    SRY-HOSPID       TO   ACCT-HOSPID
                       MOVE    SRY-NYUGAIKBN    TO   ACCT-NYUGAIKBN
                       MOVE    SRY-SRYKA        TO   ACCT-SRYKA
                       MOVE    SRY-PTID         TO   ACCT-PTID
                       MOVE    SRY-SRYYM        TO   ACCT-SRYYM
                       MOVE    SRY-ZAINUM       TO   ACCT-ZAINUM
                       MOVE    SRYACCT-REC      TO   MCPDATA-REC
                       MOVE    "DBSELECT"       TO   MCP-FUNC
                       MOVE    "SRYACCT-KEY31"  TO   ORC-DBPATH
                       CALL    "ORCMCPSUB"           USING
                                                     MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
                       IF      MCP-RC           =       ZERO
                           MOVE    "SRYACCT-KEY31"  TO  ORC-DBPATH
                           PERFORM   SRYACCT-READ-SEC
                           IF      FLG-SRYACCT  =       ZERO
                               IF  (ACCT-ZAITEN   NOT =  ZERO)  AND
                                   (ACCT-ZAIKAISU NOT =  ZERO)
                                   MOVE  "1"    TO
                                            LNK-S01-KOUREI-ZAITAKU-KBN
                               END-IF
                           END-IF
                       END-IF
                       MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
                       MOVE    "SRYACCT-KEY31" TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"         USING
                                                   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
                   END-IF
               END-IF
               MOVE    "DBCLOSECURSOR"  TO      MCP-FUNC
               MOVE    "SRYACT-KEY7"    TO      ORC-DBPATH
               CALL    "ORCMCPSUB"      USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
      *
           END-PERFORM
      *
           .
      *
       2001-ZAITAKU-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ºßÁí¿ÇÈ½Äê¡Ê£²£°£°£¶Ç¯£´·î¤«¤é¡Ë                        *
      ******************************************************************
       2002-ZAITAKU-HENSYU200604-SEC           SECTION.
      *
           PERFORM   VARYING   IDX10   FROM    1   BY  1
                      UNTIL  ( IDX10    >   TBL-ZAITAKU2-MAX     )
                       OR    ( LNK-S01-KOUREI-ZAITAKU-KBN  = "1" )
      *
               INITIALIZE                           SRYACT-REC
               MOVE    LNK-SYU-HOSPID       TO      SRY-HOSPID
               MOVE    LNK-SYU-NYUGAIKBN    TO      SRY-NYUGAIKBN
               MOVE    LNK-SYU-PTID         TO      SRY-PTID
               MOVE    LNK-SYU-SRYYMD(1:6)  TO      SRY-SRYYM
               MOVE    TBL-ZAITAKU2(IDX10)  TO      SRY-SRYCD(1)
               MOVE    TBL-ZAITAKU2(IDX10)  TO      SRY-SRYCD(2)
               MOVE    TBL-ZAITAKU2(IDX10)  TO      SRY-SRYCD(3)
               MOVE    TBL-ZAITAKU2(IDX10)  TO      SRY-SRYCD(4)
               MOVE    TBL-ZAITAKU2(IDX10)  TO      SRY-SRYCD(5)
               MOVE    SRYACT-REC           TO      MCPDATA-REC
               MOVE    "DBSELECT"           TO      MCP-FUNC
               MOVE    "SRYACT-KEY7"        TO      ORC-DBPATH
               CALL    "ORCMCPSUB"           USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
               IF      MCP-RC               =       ZERO
                   MOVE    "SRYACT-KEY7"    TO      ORC-DBPATH 
                   PERFORM   SRYACT-READ-SEC
                   IF      FLG-SRYACT       =       ZERO
                       INITIALIZE                   SRYACCT-REC
                       MOVE    SRY-HOSPID       TO   ACCT-HOSPID
                       MOVE    SRY-NYUGAIKBN    TO   ACCT-NYUGAIKBN
                       MOVE    SRY-SRYKA        TO   ACCT-SRYKA
                       MOVE    SRY-PTID         TO   ACCT-PTID
                       MOVE    SRY-SRYYM        TO   ACCT-SRYYM
                       MOVE    SRY-ZAINUM       TO   ACCT-ZAINUM
                       MOVE    SRYACCT-REC      TO   MCPDATA-REC
                       MOVE    "DBSELECT"       TO   MCP-FUNC
                       MOVE    "SRYACCT-KEY31"  TO   ORC-DBPATH
                       CALL    "ORCMCPSUB"           USING
                                                     MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
                       IF      MCP-RC           =       ZERO
                           MOVE    "SRYACCT-KEY31"  TO  ORC-DBPATH
                           PERFORM   SRYACCT-READ-SEC
                           IF      FLG-SRYACCT  =       ZERO
                               IF  (ACCT-ZAITEN   NOT =  ZERO)  AND
                                   (ACCT-ZAIKAISU NOT =  ZERO)
                                   MOVE  "1"    TO
                                            LNK-S01-KOUREI-ZAITAKU-KBN
                               END-IF
                           END-IF
                       END-IF
                       MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
                       MOVE    "SRYACCT-KEY31" TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"         USING
                                                   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
                   END-IF
               END-IF
               MOVE    "DBCLOSECURSOR"  TO      MCP-FUNC
               MOVE    "SRYACT-KEY7"    TO      ORC-DBPATH
               CALL    "ORCMCPSUB"      USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
      *
           END-PERFORM
      *
           .
      *
       2002-ZAITAKU-HENSYU200604-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÊÝ¸±¡¦¸øÈñ¤ÎÀßÄê¾å¸Â²ó¿ô¤È¾å¸Â¶â³Û¤Î¼èÆÀ                *
      ******************************************************************
       210-HOKEN-KAISU-SEC           SECTION.
      *
           INITIALIZE       WK-CHIHOU-FLG-TAIHI-AREA.
      *
           INITIALIZE       WK-SYUHEN-TAIHI-AREA.
      *
           PERFORM    2101-PTHKNINF-KENSAKU-SEC.
      *
           MOVE      ZERO      TO      WK-ZENGAKU.
           PERFORM   VARYING   IDX9    FROM    1   BY  1
                     UNTIL     IDX9       >   15
               COMPUTE WK-ZENGAKU  =   WK-ZENGAKU  +
                            (LNK-SYU-HKNTEN (IDX9) *  10)
           END-PERFORM.
      *
           IF ((COMB-KOH1HKNNUM       NOT =  SPACE )  AND
               (COMB-KOH1-GAIJGNKBN       =   "1"  ))
           OR ((COMB-KOH2HKNNUM       NOT =  SPACE )  AND
               (COMB-KOH2-GAIJGNKBN       =   "1"  ))
           OR ((COMB-KOH3HKNNUM       NOT =  SPACE )  AND
               (COMB-KOH3-GAIJGNKBN       =   "1"  ))
           OR ((COMB-KOH4HKNNUM       NOT =  SPACE )  AND
               (COMB-KOH4-GAIJGNKBN       =   "1"  ))
               PERFORM    2102-SHOHOUSEN-HANTEI-SEC
           END-IF.
      *
      *    *¸øÈñ£±¤Î¾å¸Â²ó¿ô¡Ê¶â³Û¡Ë¼è¹þ¤ß
           IF (COMB-KOH1HKNNUM           NOT =  SPACE)    AND
              (COMB-KOH1-GAIJGNKBN       =     "1")
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-KOH1HKNNUM    TO    HKN-HKNNUM
               IF     COMB-KOH1HKNNUM    =     "027"
                   IF    LNK-SYU-SRYYMD  >=    "20021001"
                       MOVE     COMB-KOH1PAYKBN     TO  WK-PAYKBN
                   ELSE
                       MOVE     WK-1001-HOSPSBT     TO  WK-PAYKBN(1:1)
                       MOVE     WK-1001-ROUPAYKBN   TO  WK-PAYKBN(2:1)
                   END-IF
               ELSE
                       MOVE     "00"         TO    WK-PAYKBN
               END-IF
               MOVE   HKNNUM-REC         TO    MCPDATA-REC
               MOVE   "DBSELECT"         TO    MCP-FUNC
               MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
               CALL   "ORCMCPSUB"         USING MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC            =     ZERO
                   MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   PERFORM  UNTIL
                     (FLG-HKNNUM     =    1)                       OR
                    ((HKN-TEKSTYMD(1:6)   <=  LNK-SYU-SRYYMD(1:6)) AND
                     (LNK-SYU-SRYYMD(1:6) <=  HKN-TEKEDYMD(1:6))   AND
                     (WK-PAYKBN            =  HKN-PAYKBN))
                       PERFORM   HKNNUM-READ-SEC
                   END-PERFORM
                   IF     FLG-HKNNUM         =       0
                          PERFORM   2103-KOH1-KAISU-SEC
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           ELSE
      *
               IF    (COMB-KOH1HKNNUM       =  "021"   )
               AND   (LNK-SYU-SRYYMD(1:6)   >= "200604")
                  CONTINUE
               ELSE
                 IF  ((LNK-S01-KOUREI-ZAITAKU-KBN =       "1")  OR
                      (FLG-KOUREI-KBN             =       "1"))
                 AND ( COMB-KOH1HKNNUM       =  "010" OR "021")
                      MOVE   WK-KOH-GENDOGAKU
                                     TO    MAX-KIN-KOHI1-M
                      MOVE    "1"    TO    COMB-KOH1-GAIJGNKBN
                 END-IF
                 IF  (FLG-GENBUTU70   NOT =   ZERO)
                 AND (COMB-KOH1HKNNUM     =  "010")
                    MOVE     WK-GEN70-GENDOGAKU
                                     TO    MAX-KIN-KOHI1-M
                    MOVE      "1"    TO    COMB-KOH1-GAIJGNKBN
                    EVALUATE  WK-GEN70-GENDOGAKU
                      WHEN       80100
                        MOVE  WK-ZENGAKU TO WK-KOUREI-IRYOHI70K
                        MOVE  1          TO FLG-IRYOHI70K
                    END-EVALUATE
                 END-IF
      *
                 IF    COMB-KOH1HKNNUM       =  "010" OR "021"
                     IF    FLG-SEISINJYO  =   1
                        MOVE   ZERO   TO    MAX-KIN-KOHI1-M
                        MOVE   SPACE  TO    COMB-KOH1-GAIJGNKBN
                     END-IF
                 END-IF
               END-IF
      *
               IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" ) OR
                    (FLG-KOUREI-KBN               =       "1" ))
               AND ( COMB-KOH1HKNNUM(1:1)        >=       "1"  )
               AND ( COMB-KOH1HKNNUM(1:1)        <=       "8"  )
               AND ( LNK-SYU-SRYYMD(1:6)         <     "200610")
                 IF   WK-1001-PREFNUM       NOT   =
                                     20 AND 15 AND 22 AND 18 AND 03
                    MOVE   12000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-GAIJGNKBN
                    MOVE    "1"    TO    FLG-CHIHOU(1)
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  11   ) AND
                   ( COMB-KOH1HKNNUM(2:2)    = "41"  )
                 IF (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  22             ) AND
                   ( COMB-KOH1HKNNUM         = "184" OR "185"  )
                 IF (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  13             ) AND
                   ( COMB-KOH1HKNNUM         = "180"           )
                 IF (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  33   ) AND
                   ( COMB-KOH1HKNNUM(2:2)    = "80"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)  >= "200610")
                 IF (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-GAIJGNKBN
                 END-IF
               END-IF
      *
           END-IF.
      *
      *    *¸øÈñ£²¤Î¾å¸Â²ó¿ô¡Ê¶â³Û¡Ë¼è¹þ¤ß
           IF (COMB-KOH2HKNNUM           NOT = SPACE)    AND
              (COMB-KOH2-GAIJGNKBN       =     "1")
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-KOH2HKNNUM    TO    HKN-HKNNUM
               MOVE     "00"             TO    WK-PAYKBN
               MOVE   HKNNUM-REC         TO    MCPDATA-REC
               MOVE   "DBSELECT"         TO    MCP-FUNC
               MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
               CALL   "ORCMCPSUB"         USING MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC            =     ZERO
                   MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   PERFORM  UNTIL
                     (FLG-HKNNUM     =    1)                      OR
                    ((HKN-TEKSTYMD(1:6)   <= LNK-SYU-SRYYMD(1:6)) AND
                     (LNK-SYU-SRYYMD(1:6) <= HKN-TEKEDYMD(1:6))   AND
                     (WK-PAYKBN            = HKN-PAYKBN))
                       PERFORM   HKNNUM-READ-SEC
                   END-PERFORM
                   IF     FLG-HKNNUM         =       0
                          PERFORM   2104-KOH2-KAISU-SEC
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           ELSE
      *
               IF    (COMB-KOH2HKNNUM       =  "021"   )
               AND   (LNK-SYU-SRYYMD(1:6)   >= "200604")
                  CONTINUE
               ELSE
                 IF  ((LNK-S01-KOUREI-ZAITAKU-KBN =       "1")  OR
                      (FLG-KOUREI-KBN             =       "1"))
                 AND ( COMB-KOH2HKNNUM       =  "010" OR "021")
                      MOVE   WK-KOH-GENDOGAKU
                                     TO    MAX-KIN-KOHI2-M
                      MOVE    "1"    TO    COMB-KOH2-GAIJGNKBN
                 END-IF
      *
                 IF    COMB-KOH2HKNNUM       =  "010" OR "021"
                     IF    FLG-SEISINJYO  =   1
                        MOVE   ZERO   TO    MAX-KIN-KOHI2-M
                        MOVE   SPACE  TO    COMB-KOH2-GAIJGNKBN
                     END-IF
                 END-IF
               END-IF
      *
               IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" ) OR
                    (FLG-KOUREI-KBN               =       "1" ))
               AND ( COMB-KOH2HKNNUM(1:1)        >=       "1"  )
               AND ( COMB-KOH2HKNNUM(1:1)        <=       "8"  )
               AND ( LNK-SYU-SRYYMD(1:6)         <     "200610")
                 IF   WK-1001-PREFNUM       NOT   =
                                     20 AND 15 AND 22 AND 18 AND 03
                    MOVE   12000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-GAIJGNKBN
                    MOVE    "1"    TO    FLG-CHIHOU(2)
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  11   ) AND
                   ( COMB-KOH2HKNNUM(2:2)    = "41"  )
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  22             ) AND
                   ( COMB-KOH2HKNNUM         = "184" OR "185"  )
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  13             ) AND
                   ( COMB-KOH2HKNNUM         = "180"           )
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  33   ) AND
                   ( COMB-KOH2HKNNUM(2:2)    = "80"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)  >= "200610")
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-GAIJGNKBN
                 END-IF
               END-IF
      *
           END-IF.
      *
      *    *¸øÈñ£³¤Î¾å¸Â²ó¿ô¡Ê¶â³Û¡Ë¼è¹þ¤ß
           IF (COMB-KOH3HKNNUM           NOT = SPACE)    AND
              (COMB-KOH3-GAIJGNKBN       =     "1")
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-KOH3HKNNUM    TO    HKN-HKNNUM
               MOVE     "00"             TO    WK-PAYKBN
               MOVE   HKNNUM-REC         TO    MCPDATA-REC
               MOVE   "DBSELECT"         TO    MCP-FUNC
               MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
               CALL   "ORCMCPSUB"         USING MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC            =     ZERO
                   MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   PERFORM  UNTIL
                     (FLG-HKNNUM     =    1)                      OR
                    ((HKN-TEKSTYMD(1:6)   <= LNK-SYU-SRYYMD(1:6)) AND
                     (LNK-SYU-SRYYMD(1:6) <= HKN-TEKEDYMD(1:6))   AND
                     (WK-PAYKBN            = HKN-PAYKBN))
                       PERFORM   HKNNUM-READ-SEC
                   END-PERFORM
                   IF     FLG-HKNNUM         =       0
                          PERFORM   2105-KOH3-KAISU-SEC
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           ELSE
      *
               IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" ) OR
                    (FLG-KOUREI-KBN               =       "1" ))
               AND ( COMB-KOH3HKNNUM(1:1)        >=       "1"  )
               AND ( COMB-KOH3HKNNUM(1:1)        <=       "8"  )
               AND ( LNK-SYU-SRYYMD(1:6)         <     "200610")
                 IF   WK-1001-PREFNUM       NOT   =
                                     20 AND 15 AND 22 AND 18 AND 03
                    MOVE   12000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-GAIJGNKBN
                    MOVE    "1"    TO    FLG-CHIHOU(3)
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  11   ) AND
                   ( COMB-KOH3HKNNUM(2:2)    = "41"  )
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  22             ) AND
                   ( COMB-KOH3HKNNUM         = "184" OR "185"  )
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  13             ) AND
                   ( COMB-KOH3HKNNUM         = "180"           )
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  33   ) AND
                   ( COMB-KOH3HKNNUM(2:2)    = "80"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)  >= "200610")
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH4HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-GAIJGNKBN
                 END-IF
               END-IF
      *
           END-IF.
      *
      *    *¸øÈñ£´¤Î¾å¸Â²ó¿ô¡Ê¶â³Û¡Ë¼è¹þ¤ß
           IF (COMB-KOH4HKNNUM           NOT = SPACE)    AND
              (COMB-KOH4-GAIJGNKBN       =     "1")
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-KOH4HKNNUM    TO    HKN-HKNNUM
               MOVE     "00"             TO    WK-PAYKBN
               MOVE   HKNNUM-REC         TO    MCPDATA-REC
               MOVE   "DBSELECT"         TO    MCP-FUNC
               MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
               CALL   "ORCMCPSUB"         USING MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC            =     ZERO
                   MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   PERFORM  UNTIL
                     (FLG-HKNNUM     =    1)                       OR
                    ((HKN-TEKSTYMD(1:6)   <= LNK-SYU-SRYYMD(1:6)) AND
                     (LNK-SYU-SRYYMD(1:6) <= HKN-TEKEDYMD(1:6))   AND
                     (WK-PAYKBN            = HKN-PAYKBN))
                       PERFORM   HKNNUM-READ-SEC
                   END-PERFORM
                   IF     FLG-HKNNUM         =       0
                          PERFORM   2106-KOH4-KAISU-SEC
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           ELSE
      *
               IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" ) OR
                    (FLG-KOUREI-KBN               =       "1" ))
               AND ( COMB-KOH4HKNNUM(1:1)        >=       "1"  )
               AND ( COMB-KOH4HKNNUM(1:1)        <=       "8"  )
               AND ( LNK-SYU-SRYYMD(1:6)         <     "200610")
                 IF   WK-1001-PREFNUM       NOT   =
                                     20 AND 15 AND 22 AND 18 AND 03
                    MOVE   12000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-GAIJGNKBN
                    MOVE    "1"    TO    FLG-CHIHOU(4)
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  11   ) AND
                   ( COMB-KOH4HKNNUM(2:2)    = "41"  )
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  22             ) AND
                   ( COMB-KOH4HKNNUM         = "184" OR "185"  )
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  13             ) AND
                   ( COMB-KOH4HKNNUM         = "180"           )
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-GAIJGNKBN
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  33   ) AND
                   ( COMB-KOH4HKNNUM(2:2)    = "80"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)  >= "200610")
                 IF (COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972")
                    MOVE   10000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-GAIJGNKBN
                 END-IF
                 IF (COMB-KOH1HKNNUM         = "974") OR
                    (COMB-KOH2HKNNUM         = "974") OR
                    (COMB-KOH3HKNNUM         = "974")
                    MOVE   20000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-GAIJGNKBN
                 END-IF
               END-IF
      *
           END-IF.
      *
      *    ´µ¼Ô¸øÈñÉéÃ´¡Ê¶â³Û¡Ë¼è¤ê¹þ¤ß
           IF    LNK-SYU-SRYYMD(1:6)   <   "200304"
               CONTINUE
           ELSE
               PERFORM   2107-KOHFTN-KINGAKU-SEC
           END-IF.
      *
      *Ê¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ
      *¹âÎð¼Ô¤ÎºßÂðÁí¹ç¡¦ºßÂðËö´ü»»Äê´µ¼Ô¤Î¼çÊÝ¸±¾å¸Â³Û»»Äê
      *
           IF  LNK-S01-KOUREI-ZAITAKU-KBN   =   "1"
               IF  COMB-KOH1-HKNKOHSBTKBN   =   "3"
                   EVALUATE   COMB-KOH1PAYKBN
                       WHEN   "01"
                           MOVE  "1"          TO   COMB-KOH1-GAIJGNKBN
                           IF  FLG-SANSHOU3     =   SPACE               
                               MOVE   12000   TO   MAX-KIN-KOHI1-D
                                                   MAX-KIN-KOHI1-M
                           ELSE
                               MOVE    8000   TO   MAX-KIN-KOHI1-D
                                                   MAX-KIN-KOHI1-M
                           END-IF
                       WHEN   "02"
                         IF    LNK-SYU-SRYYMD(1:6)   >=  "200610"
                           MOVE  "1"          TO   COMB-KOH1-GAIJGNKBN
                           IF   FLG-KEIKASOCHI-IPAN  =  1
                              MOVE   12000    TO   MAX-KIN-KOHI1-D
                                                   MAX-KIN-KOHI1-M
                           ELSE
                              MOVE   44400    TO   MAX-KIN-KOHI1-D
                                                   MAX-KIN-KOHI1-M
                           END-IF
                         ELSE
                           MOVE  "1"          TO   COMB-KOH1-GAIJGNKBN
                           IF   FLG-KEIKASOCHI-IPAN  =  1
                              MOVE   12000    TO   MAX-KIN-KOHI1-D
                                                   MAX-KIN-KOHI1-M
                           ELSE
                              MOVE   40200    TO   MAX-KIN-KOHI1-D
                                                   MAX-KIN-KOHI1-M
                           END-IF
                         END-IF
                       WHEN   "03"
                         IF    LNK-SYU-SRYYMD(1:6)   >=  "200610"
                           MOVE  "1"          TO   COMB-KOH1-GAIJGNKBN
                           IF   FLG-KEIKASOCHI-IPAN  =  1
                              MOVE   12000    TO   MAX-KIN-KOHI1-D
                                                   MAX-KIN-KOHI1-M
                           ELSE
                              MOVE   44400    TO   MAX-KIN-KOHI1-D
                                                   MAX-KIN-KOHI1-M
                           END-IF
                         END-IF
                   END-EVALUATE
               ELSE
                   EVALUATE   COMB-HOJOKBN
                       WHEN   "1"
                       WHEN   "4"
                       WHEN   "9"
                           IF  FLG-SANSHOU3      =   SPACE              
                               MOVE   12000   TO   MAX-KIN-SYU-M
                           ELSE
                               MOVE    8000   TO   MAX-KIN-SYU-M
                           END-IF
                       WHEN   "2"
                       WHEN   "5"
                       WHEN   "8"
                         IF    LNK-SYU-SRYYMD(1:6)   >=  "200610"
                           IF   FLG-KEIKASOCHI-IPAN  =  1
                               MOVE   12000   TO   MAX-KIN-SYU-M
                           ELSE
                               MOVE   44400   TO   MAX-KIN-SYU-M
                           END-IF
                         ELSE
                           IF   FLG-KEIKASOCHI-IPAN  =  1
                               MOVE   12000   TO   MAX-KIN-SYU-M
                           ELSE
                               MOVE   40200   TO   MAX-KIN-SYU-M
                           END-IF
                         END-IF
                       WHEN   "3"
                       WHEN   "6"
                       WHEN   "7"
                         IF    LNK-SYU-SRYYMD(1:6)   >=  "200610"
                           IF   FLG-KEIKASOCHI-IPAN  =  1
                               MOVE   12000   TO   MAX-KIN-SYU-M
                           ELSE
                               MOVE   44400   TO   MAX-KIN-SYU-M
                           END-IF
                         END-IF
                   END-EVALUATE
               END-IF
           END-IF.
      *
           IF  ( LNK-S01-KOUREI-ZAITAKU-KBN   =       "1"  )
           AND ( COMB-HKNNUM            NOT   =     SPACE  )
      *
                IF   COMB-KOH1HKNNUM      =    "027"
                   IF (( COMB-KOH2HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH2HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH3HKNNUM        =    SPACE) AND
                       ( COMB-KOH4HKNNUM        =    SPACE))
                   OR (( COMB-KOH2HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH2HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH3HKNNUM(1:2)   =     "99") AND
                       ( COMB-KOH4HKNNUM        =    SPACE))
                   OR (( COMB-KOH2HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH2HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH3HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH3HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH4HKNNUM        =    SPACE))
                   OR (( COMB-KOH2HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH2HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH3HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH3HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH4HKNNUM(1:2)   =     "99"))
                   OR (( COMB-KOH2HKNNUM(1:2)   =     "99") AND
                       ( COMB-KOH3HKNNUM        =    SPACE) AND
                       ( COMB-KOH4HKNNUM        =    SPACE))
                          MOVE    "1"    TO   FLG-SYUHEN
                   ELSE
                    IF   ( COMB-KOH2HKNNUM   =   "012" )
                    OR   ( COMB-KOH3HKNNUM   =   "012" )
                    OR   ( COMB-KOH4HKNNUM   =   "012" )
                          MOVE    "1"    TO   FLG-SYUHEN
                          IF   MAX-KIN-KOHI1-M   >  8000
                            MOVE   8000    TO   MAX-KIN-KOHI1-D
                                                MAX-KIN-KOHI1-M
                          END-IF
                    ELSE
                     IF   ( COMB-KOH2HKNNUM  =   "972" )
                     OR   ( COMB-KOH3HKNNUM  =   "972" )
                     OR   ( COMB-KOH4HKNNUM  =   "972" )
                          MOVE    "1"    TO   FLG-SYUHEN
                          IF   MAX-KIN-KOHI1-M   >  10000
                            MOVE   10000   TO   MAX-KIN-KOHI1-D
                                                MAX-KIN-KOHI1-M
                          END-IF
                     ELSE
                      IF  (COMB-KOH2HKNNUM(1:1) = "0")
                      OR  (COMB-KOH3HKNNUM(1:1) = "0")
                      OR  (COMB-KOH4HKNNUM(1:1) = "0")
                          MOVE    "1"    TO   FLG-SYUHEN
      *****               IF   MAX-KIN-KOHI1-M   >  12000
                            MOVE   12000   TO   MAX-KIN-KOHI1-D
                                                MAX-KIN-KOHI1-M
      *****               END-IF
                      ELSE
                        IF  (COMB-KOH2HKNNUM = "974")
                        OR  (COMB-KOH3HKNNUM = "974")
                        OR  (COMB-KOH4HKNNUM = "974")
                          MOVE    "1"    TO   FLG-SYUHEN
                          IF   MAX-KIN-KOHI1-M   >  20000
                            MOVE   20000   TO   MAX-KIN-KOHI1-D
                                                MAX-KIN-KOHI1-M
                          END-IF
                        END-IF
                      END-IF
                     END-IF
                    END-IF
                   END-IF
                ELSE
                   IF (( COMB-KOH1HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH1HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH2HKNNUM        =    SPACE) AND
                       ( COMB-KOH3HKNNUM        =    SPACE) AND
                       ( COMB-KOH4HKNNUM        =    SPACE))
                   OR (( COMB-KOH1HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH1HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH2HKNNUM(1:2)   =     "99") AND
                       ( COMB-KOH3HKNNUM        =    SPACE) AND
                       ( COMB-KOH4HKNNUM        =    SPACE))
                   OR (( COMB-KOH1HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH1HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH2HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH2HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH3HKNNUM        =    SPACE) AND
                       ( COMB-KOH4HKNNUM        =    SPACE))
                   OR (( COMB-KOH1HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH1HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH2HKNNUM(1:1)  >=     "1" ) AND
                       ( COMB-KOH2HKNNUM(1:1)  <=     "8" ) AND
                       ( COMB-KOH3HKNNUM(1:2)   =     "99") AND
                       ( COMB-KOH4HKNNUM        =    SPACE))
                   OR (( COMB-KOH1HKNNUM(1:2)   =     "99") AND
                       ( COMB-KOH2HKNNUM        =    SPACE) AND
                       ( COMB-KOH3HKNNUM        =    SPACE) AND
                       ( COMB-KOH4HKNNUM        =    SPACE))
                          MOVE    "1"    TO   FLG-SYUHEN
                   ELSE
                    IF   ( COMB-KOH1HKNNUM   =   "012" )
                    OR   ( COMB-KOH2HKNNUM   =   "012" )
                    OR   ( COMB-KOH3HKNNUM   =   "012" )
                    OR   ( COMB-KOH4HKNNUM   =   "012" )
                          MOVE    "1"    TO   FLG-SYUHEN
                          IF   MAX-KIN-SYU-M   >  8000
                            MOVE   8000    TO   MAX-KIN-SYU-M
                          END-IF
                    ELSE
                     IF   ( COMB-KOH1HKNNUM  =   "972" )
                     OR   ( COMB-KOH2HKNNUM  =   "972" )
                     OR   ( COMB-KOH3HKNNUM  =   "972" )
                     OR   ( COMB-KOH4HKNNUM  =   "972" )
                          MOVE    "1"    TO   FLG-SYUHEN
                          IF   MAX-KIN-SYU-M   >  10000
                            MOVE   10000   TO   MAX-KIN-SYU-M
                          END-IF
                     ELSE
                      IF  (COMB-KOH1HKNNUM(1:1) = "0")
                      OR  (COMB-KOH2HKNNUM(1:1) = "0")
                      OR  (COMB-KOH3HKNNUM(1:1) = "0")
                      OR  (COMB-KOH4HKNNUM(1:1) = "0")
                          MOVE    "1"    TO   FLG-SYUHEN
      *****               IF   MAX-KIN-SYU-M   >  12000
                            MOVE   12000   TO   MAX-KIN-SYU-M
      *****               END-IF
                      ELSE
                        IF  (COMB-KOH1HKNNUM = "974")
                        OR  (COMB-KOH2HKNNUM = "974")
                        OR  (COMB-KOH3HKNNUM = "974")
                        OR  (COMB-KOH4HKNNUM = "974")
                          MOVE    "1"    TO   FLG-SYUHEN
                          IF   MAX-KIN-SYU-M   >  20000
                            MOVE   20000   TO   MAX-KIN-SYU-M
                          END-IF
                        END-IF
                      END-IF
                     END-IF
                    END-IF
                   END-IF
                END-IF
      *
           END-IF.
      *
           IF  FLG-GENBUTU70   NOT =   ZERO
      *
               EVALUATE   FLG-GENBUTU70
                WHEN        1
                  MOVE    150000   TO   MAX-KIN-SYU-M
                  IF  FLG-KOHYONKAIME = 1
                     MOVE  83400   TO   MAX-KIN-SYU-M
                  END-IF
                WHEN        2
                  MOVE     80100   TO   MAX-KIN-SYU-M
                  IF  FLG-KOHYONKAIME = 1
                     MOVE  44400   TO   MAX-KIN-SYU-M
                  END-IF
                WHEN        3
                  MOVE     35400   TO   MAX-KIN-SYU-M
                  IF  FLG-KOHYONKAIME = 1
                     MOVE  24600   TO   MAX-KIN-SYU-M
                  END-IF
               END-EVALUATE
      *
               IF  (COMB-KOH1HKNNUM    =  SPACE)
               AND (COMB-KOH2HKNNUM    =  SPACE)
               AND (COMB-KOH3HKNNUM    =  SPACE)
               AND (COMB-KOH4HKNNUM    =  SPACE)
                  EVALUATE  MAX-KIN-SYU-M
                   WHEN      150000
                     MOVE  WK-ZENGAKU  TO  WK-KOUREI-IRYOHI70
                     MOVE  1           TO  FLG-IRYOHI70
                   WHEN       80100
                     MOVE  WK-ZENGAKU  TO  WK-KOUREI-IRYOHI70
                     MOVE  2           TO  FLG-IRYOHI70
                  END-EVALUATE
               END-IF
      *
               IF (( COMB-KOH1HKNNUM(1:1)  >=     "1" ) AND
                   ( COMB-KOH1HKNNUM(1:1)  <=     "8" ) AND
                   ( COMB-KOH2HKNNUM        =    SPACE) AND
                   ( COMB-KOH3HKNNUM        =    SPACE) AND
                   ( COMB-KOH4HKNNUM        =    SPACE))
               OR (( COMB-KOH1HKNNUM(1:1)  >=     "1" ) AND
                   ( COMB-KOH1HKNNUM(1:1)  <=     "8" ) AND
                   ( COMB-KOH2HKNNUM(1:2)   =     "99") AND
                   ( COMB-KOH3HKNNUM        =    SPACE) AND
                   ( COMB-KOH4HKNNUM        =    SPACE))
               OR (( COMB-KOH1HKNNUM(1:1)  >=     "1" ) AND
                   ( COMB-KOH1HKNNUM(1:1)  <=     "8" ) AND
                   ( COMB-KOH2HKNNUM(1:1)  >=     "1" ) AND
                   ( COMB-KOH2HKNNUM(1:1)  <=     "8" ) AND
                   ( COMB-KOH3HKNNUM        =    SPACE) AND
                   ( COMB-KOH4HKNNUM        =    SPACE))
               OR (( COMB-KOH1HKNNUM(1:1)  >=     "1" ) AND
                   ( COMB-KOH1HKNNUM(1:1)  <=     "8" ) AND
                   ( COMB-KOH2HKNNUM(1:1)  >=     "1" ) AND
                   ( COMB-KOH2HKNNUM(1:1)  <=     "8" ) AND
                   ( COMB-KOH3HKNNUM(1:2)   =     "99") AND
                   ( COMB-KOH4HKNNUM        =    SPACE))
               OR (( COMB-KOH1HKNNUM(1:2)   =     "99") AND
                   ( COMB-KOH2HKNNUM        =    SPACE) AND
                   ( COMB-KOH3HKNNUM        =    SPACE) AND
                   ( COMB-KOH4HKNNUM        =    SPACE))
      ***          MOVE    "1"    TO   FLG-SYUHEN
                   IF  (WK-1001-PREFNUM  =  01            )
                   AND (COMB-KOH1HKNNUM  =  "141" OR "241")
                     MOVE     80100   TO   MAX-KIN-SYU-M
                     IF  FLG-KOHYONKAIME = 1
                        MOVE  44400   TO   MAX-KIN-SYU-M
                     END-IF
                   END-IF
                   EVALUATE  MAX-KIN-SYU-M
                     WHEN      150000
                       MOVE  WK-ZENGAKU  TO  WK-KOUREI-IRYOHI70
                       MOVE  3           TO  FLG-IRYOHI70
                     WHEN       80100
                       MOVE  WK-ZENGAKU  TO  WK-KOUREI-IRYOHI70
                       MOVE  4           TO  FLG-IRYOHI70
                   END-EVALUATE
               ELSE
                IF   ( COMB-KOH1HKNNUM   =   "012" )
                OR   ( COMB-KOH2HKNNUM   =   "012" )
                OR   ( COMB-KOH3HKNNUM   =   "012" )
                OR   ( COMB-KOH4HKNNUM   =   "012" )
      ***          MOVE    "1"    TO   FLG-SYUHEN
                   IF  FLG-KOHYONKAIME = 1
                      MOVE  24600   TO   WK-GEN70-GENDOGAKU2
                   ELSE
                      MOVE  35400   TO   WK-GEN70-GENDOGAKU2
                   END-IF
                   IF MAX-KIN-SYU-M  >   WK-GEN70-GENDOGAKU2
                      MOVE WK-GEN70-GENDOGAKU2
                                    TO   MAX-KIN-SYU-M
                   END-IF
                ELSE
                 IF   ( COMB-KOH1HKNNUM  =   "972" )
                 OR   ( COMB-KOH2HKNNUM  =   "972" )
                 OR   ( COMB-KOH3HKNNUM  =   "972" )
                 OR   ( COMB-KOH4HKNNUM  =   "972" )
      ***             MOVE    "1"    TO   FLG-SYUHEN
                      IF   MAX-KIN-SYU-M   >  10000
                          MOVE   10000   TO   MAX-KIN-SYU-M
                      END-IF
                 ELSE
                  IF  ( COMB-KOH1HKNNUM  =   "974" )
                  OR  ( COMB-KOH2HKNNUM  =   "974" )
                  OR  ( COMB-KOH3HKNNUM  =   "974" )
                  OR  ( COMB-KOH4HKNNUM  =   "974" )
      ***             MOVE    "1"    TO   FLG-SYUHEN
                      IF   MAX-KIN-SYU-M   >  20000
                          MOVE   20000   TO   MAX-KIN-SYU-M
                      END-IF
                  ELSE
                    IF  (COMB-KOH1HKNNUM(1:1) = "0")
                    OR  (COMB-KOH2HKNNUM(1:1) = "0")
                    OR  (COMB-KOH3HKNNUM(1:1) = "0")
                    OR  (COMB-KOH4HKNNUM(1:1) = "0")
      ***             MOVE    "1"        TO   FLG-SYUHEN
      ***>            IF  FLG-KOHYONKAIME = 1
      ***>              MOVE   44400       TO   MAX-KIN-SYU-M
      ***>            ELSE
                        MOVE   80100       TO   MAX-KIN-SYU-M
                        MOVE   WK-ZENGAKU  TO   WK-KOUREI-IRYOHI70
                        MOVE   6           TO   FLG-IRYOHI70
      ***>            END-IF
                    END-IF
                  END-IF
                 END-IF
                END-IF
               END-IF
      *
           END-IF.
      *
      *    ¸º³Û¡Ê³ä¹ç¡¦¶â³Û¡Ë¼è¤ê¹þ¤ß
           IF    WK-GENGAKUKBN     NOT  =   ZERO
               PERFORM   2109-KOHFTN-KINGAKU-GEN-SEC
           END-IF.
      *
      *    ¹â³Û°ÑÇ¤Ê§¤¤¶â³Û¼è¤ê¹þ¤ß
           IF    FLG-KOHININBARAI       =   1
               PERFORM   2110-KOHININBARAI-KINGAKU-SEC
           END-IF.
      *
      *    £´£±Ï·¿Í¾å¸Â³ÛÊÔ½¸½èÍýÅù
           PERFORM    2108-ROUJIN41-JYOGEN-HENSYU-SEC.
      *
       210-HOKEN-KAISU-EXT.
           EXIT.
      *
      ***************************************************************
      *        ´µ¼ÔÊÝ¸±¾ðÊó¸¡º÷
      ***************************************************************
       2101-PTHKNINF-KENSAKU-SEC           SECTION.
      *
           MOVE   ZERO               TO    FLG-SEISINJYO
                                           FLG-TOKUREI-HKNJYA
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE   COMB-HOSPID        TO    PTHKN-HOSPID
           MOVE   COMB-PTID          TO    PTHKN-PTID
           MOVE   COMB-HKNID         TO    PTHKN-HKNID
           MOVE   PTHKNINF-REC       TO    MCPDATA-REC
           MOVE   "DBSELECT"         TO    MCP-FUNC
           MOVE   "PTHKNINF-KEY"     TO    ORC-DBPATH
           CALL   "ORCMCPSUB"        USING MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC            =     ZERO
               MOVE   "PTHKNINF-KEY" TO    ORC-DBPATH
               PERFORM  PTHKNINF-READ-SEC
               IF      FLG-PTHKNINF   =     ZERO
                  IF   PTHKN-HKNJANUM(1:6) = "133033" OR "133280" OR
                                             "133041" OR "133066" OR
                                             "133074" OR "133090" OR
                                             "133116" OR "133132" OR
                                             "133140" OR "133157" OR
                                             "133165" OR "133173" OR
                                             "133181" OR "133199" OR
                                             "133207" OR "133223" OR
                                             "133249" OR "133256" OR
                                             "133272"
                      MOVE    1      TO    FLG-SEISINJYO
                  END-IF
                  IF   PTHKN-HKNJANUM      = "67330753"
                      MOVE    1      TO    FLG-TOKUREI-HKNJYA
                  END-IF
               END-IF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       2101-PTHKNINF-KENSAKU-EXT.
           EXIT.
      *
      ***************************************************************
      *        ¼ýÇ¼¥Æ¡¼¥Ö¥ë¤«¤é±¡Æâ½èÊý¡¢±¡³°½èÊý¤ÎÈ½Äê             *
      ***************************************************************
       2102-SHOHOUSEN-HANTEI-SEC           SECTION.
      *
           IF      LNK-SYU-INGAISHOHOKBN  =      "1"
               MOVE       "1"             TO     FLG-INGAI
           ELSE
             INITIALIZE                          SYU-SYUNOU-REC
             MOVE    LNK-SYU-HOSPID       TO     SYU-HOSPID
             MOVE    LNK-SYU-NYUGAIKBN    TO     SYU-NYUGAIKBN
             MOVE    LNK-SYU-PTID         TO     SYU-PTID
             MOVE    LNK-SYU-SRYYMD(1:6)  TO     SYU-SRYYMD(1:6)
             MOVE    "__"                 TO     SYU-SRYYMD(7:2)
             MOVE    SYU-SYUNOU-REC       TO     MCPDATA-REC 
             MOVE    "DBSELECT"           TO     MCP-FUNC
             MOVE    "SYUNOU-KEY14"       TO     ORC-DBPATH
             CALL    "ORCMCPSUB"           USING  MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
             IF      MCP-RC               =      ZERO
                 MOVE    "SYUNOU-KEY14"   TO     ORC-DBPATH
                 PERFORM  SYUNOU-READ-SEC
                 PERFORM  UNTIL  (FLG-SYUNOU   =    "1")
                     IF   SYU-DENPJTIKBN  NOT =   "3"
                         IF   SYU-INGAISHOHOKBN   =     "1"
                              MOVE   "1"  TO     FLG-INGAI
                         ELSE
                              MOVE   "1"  TO     FLG-INNAI
                         END-IF
                     END-IF
                     MOVE      "SYUNOU-KEY14"  TO     ORC-DBPATH
                     PERFORM   SYUNOU-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE    "SYUNOU-KEY14"       TO  ORC-DBPATH
             CALL    "ORCMCPSUB"           USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-IF.
      *
       2102-SHOHOUSEN-HANTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñ£±¡Ê¶â³Û¡Ë¼è¹þ¤ß½èÍý                                *
      ******************************************************************
       2103-KOH1-KAISU-SEC           SECTION.
      *
           IF    COMB-KOH1HKNNUM(1:1)   >=  "1" AND <=  "8"
             IF  WK-1001-CHIHOUNUMKBN       =   "1"
               EVALUATE   FLG-SANSHOU2
                 WHEN   "1"
                    MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                    MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                    MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                    MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                    MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                    IF    FLG-INGAI          =   "1"
                          MOVE  HKN-HON-GAIMGAIJGNGAK
                                             TO  MAX-KIN-KOHI1-M
                    ELSE
                          MOVE  HKN-HON-GAIMNAIJGNGAK
                                             TO  MAX-KIN-KOHI1-M
                    END-IF
                 WHEN   "2"
                    IF   HKN-TSY-GAIFTNKBN      =  "0"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI1-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI1-M
                      END-IF
                    ELSE
                      MOVE  HKN-TSY-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                      MOVE  HKN-TSY-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                      MOVE  HKN-TSY-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                      MOVE  HKN-TSY-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                      MOVE  HKN-TSY-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-TSY-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI1-M
                      ELSE
                            MOVE  HKN-TSY-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI1-M
                      END-IF
                    END-IF
                 WHEN   "3"
                    IF  HKN-TNK-GAIFTNKBN       =  "0"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI1-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI1-M
                      END-IF
                    ELSE
                      MOVE  HKN-TNK-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                      MOVE  HKN-TNK-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                      MOVE  HKN-TNK-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                      MOVE  HKN-TNK-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                      MOVE  HKN-TNK-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-TNK-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI1-M
                      ELSE
                            MOVE  HKN-TNK-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI1-M
                      END-IF
                    END-IF
               END-EVALUATE
             ELSE
               IF (( WK-1001-PREFNUM   =  28                      ) AND
                   ( COMB-KOH1HKNNUM   = "241" OR "180" OR
                                         "182" OR "143" OR
                                         "185" OR "127"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200507"          ))
               OR (( WK-1001-PREFNUM   =  11                      ) AND
                   ( COMB-KOH1HKNNUM   = "344"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200507"          ))
               OR (( WK-1001-PREFNUM   =  05                      ) AND
                   ( COMB-KOH1HKNNUM   = "374" OR "474"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200508"          ))
               OR (( WK-1001-PREFNUM   =  32                      ) AND
                   ( COMB-KOH1HKNNUM   = "191" OR "391"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200510"          ))
               OR (( WK-1001-PREFNUM   =  28                      ) AND
                   ( COMB-KOH1HKNNUM   = "100"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200604"          ))
               OR (( WK-1001-PREFNUM   =  33                      ) AND
                   ( COMB-KOH1HKNNUM   = "141" OR "285" OR
                                         "485" OR "180" OR "186"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200610"          ))
               OR (( WK-1001-PREFNUM   =  19                      ) AND
                   ( COMB-KOH1HKNNUM   = "141"                    ))
               OR (( WK-1001-PREFNUM   =  23                      ) AND
                   ( COMB-KOH1HKNNUM   = "141"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200610"          ))
                 EVALUATE   FLG-SANSHOU2
                   WHEN   "1"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI1-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI1-M
                      END-IF
                   WHEN   "2"
                      IF   HKN-TSY-GAIFTNKBN      =  "0"
                        MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                        MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                        MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                        MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                        MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-HON-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI1-M
                        ELSE
                              MOVE  HKN-HON-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI1-M
                        END-IF
                      ELSE
                        MOVE  HKN-TSY-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                        MOVE  HKN-TSY-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                        MOVE  HKN-TSY-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                        MOVE  HKN-TSY-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                        MOVE  HKN-TSY-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-TSY-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI1-M
                        ELSE
                              MOVE  HKN-TSY-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI1-M
                        END-IF
                      END-IF
                   WHEN   "3"
                      IF  HKN-TNK-GAIFTNKBN       =  "0"
                        MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                        MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                        MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                        MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                        MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-HON-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI1-M
                        ELSE
                              MOVE  HKN-HON-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI1-M
                        END-IF
                      ELSE
                        MOVE  HKN-TNK-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                        MOVE  HKN-TNK-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                        MOVE  HKN-TNK-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                        MOVE  HKN-TNK-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                        MOVE  HKN-TNK-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-TNK-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI1-M
                        ELSE
                              MOVE  HKN-TNK-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI1-M
                        END-IF
                      END-IF
                 END-EVALUATE
               ELSE
                 MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
                 MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
                 MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
                 MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
                 MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
                 IF    FLG-INGAI          =   "1"
                       MOVE  HKN-HON-GAIMGAIJGNGAK
                                          TO  MAX-KIN-KOHI1-M
                 ELSE
                       MOVE  HKN-HON-GAIMNAIJGNGAK
                                          TO  MAX-KIN-KOHI1-M
                 END-IF
               END-IF
             END-IF
           ELSE
             MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI1-K
             MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI1-K
             MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI1-D
             MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI1-M
             MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI1-D
             IF    FLG-INGAI          =   "1"
                   MOVE  HKN-HON-GAIMGAIJGNGAK
                                      TO  MAX-KIN-KOHI1-M
             ELSE
                   MOVE  HKN-HON-GAIMNAIJGNGAK
                                      TO  MAX-KIN-KOHI1-M
             END-IF
           END-IF.
      *
           IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" ) OR
                (FLG-KOUREI-KBN               =       "1" ))
           AND ( COMB-KOH1HKNNUM(1:1)        >=       "1"  )
           AND ( COMB-KOH1HKNNUM(1:1)        <=       "8"  )
           AND ( LNK-SYU-SRYYMD(1:6)         <     "200610")
              IF  (WK-1001-PREFNUM       NOT   =
                                    20 AND 15 AND 22 AND 18 AND 03 )
              AND (MAX-KIN-KOHI1-M             >   12000           )
                IF    WK-1001-PREFNUM       =   07
                AND   COMB-KOH1HKNNUM       =  "246"
                AND   LNK-SYU-SRYYMD(1:6)  >=  "200604"
                     CONTINUE
                ELSE
                     MOVE    12000   TO    MAX-KIN-KOHI1-M
                END-IF
              END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  11   ) AND
               ( COMB-KOH1HKNNUM(2:2)    = "41"  )
             IF (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI1-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI1-M
               END-IF
             END-IF
             IF (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI1-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI1-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  22             ) AND
               ( COMB-KOH1HKNNUM         = "184" OR "185"  )
             IF (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI1-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI1-M
               END-IF
             END-IF
             IF (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI1-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI1-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  13             ) AND
               ( COMB-KOH1HKNNUM         = "180"           )
             IF (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI1-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI1-M
               END-IF
             END-IF
             IF (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI1-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI1-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  33   ) AND
               ( COMB-KOH1HKNNUM(2:2)    = "80"  ) AND
               ( LNK-SYU-SRYYMD(1:6)  >= "200610")
             IF (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI1-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI1-M
               END-IF
             END-IF
             IF (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI1-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI1-M
               END-IF
             END-IF
           END-IF.
      *
       2103-KOH1-KAISU-EXT.
           EXIT.
      ******************************************************************
      *        ¸øÈñ£²¡Ê¶â³Û¡Ë¼è¹þ¤ß½èÍý                                *
      ******************************************************************
       2104-KOH2-KAISU-SEC           SECTION.
      *
           IF    COMB-KOH2HKNNUM(1:1)   >=  "1" AND <=  "8"
             IF  WK-1001-CHIHOUNUMKBN       =   "1"
               EVALUATE   FLG-SANSHOU2
                 WHEN   "1"
                    MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                    MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                    MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                    MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                    MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                    IF    FLG-INGAI          =   "1"
                          MOVE  HKN-HON-GAIMGAIJGNGAK
                                             TO  MAX-KIN-KOHI2-M
                    ELSE
                          MOVE  HKN-HON-GAIMNAIJGNGAK
                                             TO  MAX-KIN-KOHI2-M
                    END-IF
                 WHEN   "2"
                    IF   HKN-TSY-GAIFTNKBN      =  "0"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI2-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI2-M
                      END-IF
                    ELSE
                      MOVE  HKN-TSY-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                      MOVE  HKN-TSY-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                      MOVE  HKN-TSY-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                      MOVE  HKN-TSY-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                      MOVE  HKN-TSY-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-TSY-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI2-M
                      ELSE
                            MOVE  HKN-TSY-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI2-M
                      END-IF
                    END-IF
                 WHEN   "3"
                    IF  HKN-TNK-GAIFTNKBN       =  "0"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI2-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI2-M
                      END-IF
                    ELSE
                      MOVE  HKN-TNK-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                      MOVE  HKN-TNK-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                      MOVE  HKN-TNK-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                      MOVE  HKN-TNK-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                      MOVE  HKN-TNK-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-TNK-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI2-M
                      ELSE
                            MOVE  HKN-TNK-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI2-M
                      END-IF
                    END-IF
               END-EVALUATE
             ELSE
               IF (( WK-1001-PREFNUM   =  28                      ) AND
                   ( COMB-KOH2HKNNUM   = "241" OR "180" OR
                                         "182" OR "143" OR
                                         "185" OR "127"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200507"          ))
               OR (( WK-1001-PREFNUM   =  11                      ) AND
                   ( COMB-KOH2HKNNUM   = "344"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200507"          ))
               OR (( WK-1001-PREFNUM   =  05                      ) AND
                   ( COMB-KOH2HKNNUM   = "374" OR "474"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200508"          ))
               OR (( WK-1001-PREFNUM   =  32                      ) AND
                   ( COMB-KOH2HKNNUM   = "191" OR "391"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200510"          ))
               OR (( WK-1001-PREFNUM   =  28                      ) AND
                   ( COMB-KOH2HKNNUM   = "100"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200604"          ))
               OR (( WK-1001-PREFNUM   =  33                      ) AND
                   ( COMB-KOH2HKNNUM   = "141" OR "285" OR
                                         "485" OR "180" OR "186"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200610"          ))
               OR (( WK-1001-PREFNUM   =  19                      ) AND
                   ( COMB-KOH2HKNNUM   = "141"                    ))
               OR (( WK-1001-PREFNUM   =  23                      ) AND
                   ( COMB-KOH2HKNNUM   = "141"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200610"          ))
                 EVALUATE   FLG-SANSHOU2
                   WHEN   "1"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI2-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI2-M
                      END-IF
                   WHEN   "2"
                      IF   HKN-TSY-GAIFTNKBN      =  "0"
                        MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                        MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                        MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                        MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                        MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-HON-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI2-M
                        ELSE
                              MOVE  HKN-HON-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI2-M
                        END-IF
                      ELSE
                        MOVE  HKN-TSY-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                        MOVE  HKN-TSY-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                        MOVE  HKN-TSY-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                        MOVE  HKN-TSY-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                        MOVE  HKN-TSY-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-TSY-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI2-M
                        ELSE
                              MOVE  HKN-TSY-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI2-M
                        END-IF
                      END-IF
                   WHEN   "3"
                      IF  HKN-TNK-GAIFTNKBN       =  "0"
                        MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                        MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                        MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                        MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                        MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-HON-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI2-M
                        ELSE
                              MOVE  HKN-HON-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI2-M
                        END-IF
                      ELSE
                        MOVE  HKN-TNK-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                        MOVE  HKN-TNK-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                        MOVE  HKN-TNK-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                        MOVE  HKN-TNK-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                        MOVE  HKN-TNK-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-TNK-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI2-M
                        ELSE
                              MOVE  HKN-TNK-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI2-M
                        END-IF
                      END-IF
                 END-EVALUATE
               ELSE
                 MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
                 MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
                 MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
                 MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
                 MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
                 IF    FLG-INGAI          =   "1"
                       MOVE  HKN-HON-GAIMGAIJGNGAK
                                          TO  MAX-KIN-KOHI2-M
                 ELSE
                       MOVE  HKN-HON-GAIMNAIJGNGAK
                                          TO  MAX-KIN-KOHI2-M
                 END-IF
               END-IF
             END-IF
           ELSE
             MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI2-K
             MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI2-K
             MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI2-D
             MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI2-M
             MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI2-D
             IF    FLG-INGAI          =   "1"
                   MOVE  HKN-HON-GAIMGAIJGNGAK
                                      TO  MAX-KIN-KOHI2-M
             ELSE
                   MOVE  HKN-HON-GAIMNAIJGNGAK
                                      TO  MAX-KIN-KOHI2-M
             END-IF
           END-IF.
      *
           IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" ) OR
                (FLG-KOUREI-KBN               =       "1" ))
           AND ( COMB-KOH2HKNNUM(1:1)        >=       "1"  )
           AND ( COMB-KOH2HKNNUM(1:1)        <=       "8"  )
           AND ( LNK-SYU-SRYYMD(1:6)         <     "200610")
              IF  (WK-1001-PREFNUM       NOT   =
                                    20 AND 15 AND 22 AND 18 AND 03 )
              AND (MAX-KIN-KOHI2-M             >   12000           )
                IF    WK-1001-PREFNUM       =   07
                AND   COMB-KOH2HKNNUM       =  "246"
                AND   LNK-SYU-SRYYMD(1:6)  >=  "200604"
                     CONTINUE
                ELSE
                     MOVE    12000   TO    MAX-KIN-KOHI2-M
                END-IF
              END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  11   ) AND
               ( COMB-KOH2HKNNUM(2:2)    = "41"  )
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI2-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI2-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI2-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI2-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  22             ) AND
               ( COMB-KOH2HKNNUM         = "184" OR "185"  )
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI2-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI2-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI2-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI2-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  13             ) AND
               ( COMB-KOH2HKNNUM         = "180"           )
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI2-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI2-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI2-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI2-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  33   ) AND
               ( COMB-KOH2HKNNUM(2:2)    = "80"  ) AND
               ( LNK-SYU-SRYYMD(1:6)  >= "200610")
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI2-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI2-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI2-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI2-M
               END-IF
             END-IF
           END-IF.
      *
       2104-KOH2-KAISU-EXT.
           EXIT.
      ******************************************************************
      *        ¸øÈñ£³¡Ê¶â³Û¡Ë¼è¹þ¤ß½èÍý                                *
      ******************************************************************
       2105-KOH3-KAISU-SEC           SECTION.
      *
           IF    COMB-KOH3HKNNUM(1:1)   >=  "1" AND <=  "8"
             IF  WK-1001-CHIHOUNUMKBN       =   "1"
               EVALUATE   FLG-SANSHOU2
                 WHEN   "1"
                    MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                    MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                    MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                    MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                    MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                    IF    FLG-INGAI          =   "1"
                          MOVE  HKN-HON-GAIMGAIJGNGAK
                                             TO  MAX-KIN-KOHI3-M
                    ELSE
                          MOVE  HKN-HON-GAIMNAIJGNGAK
                                             TO  MAX-KIN-KOHI3-M
                    END-IF
                 WHEN   "2"
                    IF   HKN-TSY-GAIFTNKBN      =  "0"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI3-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI3-M
                      END-IF
                    ELSE
                      MOVE  HKN-TSY-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                      MOVE  HKN-TSY-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                      MOVE  HKN-TSY-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                      MOVE  HKN-TSY-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                      MOVE  HKN-TSY-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-TSY-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI3-M
                      ELSE
                            MOVE  HKN-TSY-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI3-M
                      END-IF
                    END-IF
                 WHEN   "3"
                    IF  HKN-TNK-GAIFTNKBN       =  "0"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI3-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI3-M
                      END-IF
                    ELSE
                      MOVE  HKN-TNK-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                      MOVE  HKN-TNK-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                      MOVE  HKN-TNK-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                      MOVE  HKN-TNK-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                      MOVE  HKN-TNK-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-TNK-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI3-M
                      ELSE
                            MOVE  HKN-TNK-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI3-M
                      END-IF
                    END-IF
               END-EVALUATE
             ELSE
               IF (( WK-1001-PREFNUM   =  28                      ) AND
                   ( COMB-KOH3HKNNUM   = "241" OR "180" OR
                                         "182" OR "143" OR
                                         "185" OR "127"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200507"          ))
               OR (( WK-1001-PREFNUM   =  11                      ) AND
                   ( COMB-KOH3HKNNUM   = "344"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200507"          ))
               OR (( WK-1001-PREFNUM   =  05                      ) AND
                   ( COMB-KOH3HKNNUM   = "374" OR "474"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200508"          ))
               OR (( WK-1001-PREFNUM   =  32                      ) AND
                   ( COMB-KOH3HKNNUM   = "191" OR "391"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200510"          ))
               OR (( WK-1001-PREFNUM   =  28                      ) AND
                   ( COMB-KOH3HKNNUM   = "100"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200604"          ))
               OR (( WK-1001-PREFNUM   =  33                      ) AND
                   ( COMB-KOH3HKNNUM   = "141" OR "285" OR
                                         "485" OR "180" OR "186"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200610"          ))
               OR (( WK-1001-PREFNUM   =  19                      ) AND
                   ( COMB-KOH3HKNNUM   = "141"                    ))
               OR (( WK-1001-PREFNUM   =  23                      ) AND
                   ( COMB-KOH3HKNNUM   = "141"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200610"          ))
                 EVALUATE   FLG-SANSHOU2
                   WHEN   "1"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI3-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI3-M
                      END-IF
                   WHEN   "2"
                      IF   HKN-TSY-GAIFTNKBN      =  "0"
                        MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                        MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                        MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                        MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                        MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-HON-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI3-M
                        ELSE
                              MOVE  HKN-HON-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI3-M
                        END-IF
                      ELSE
                        MOVE  HKN-TSY-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                        MOVE  HKN-TSY-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                        MOVE  HKN-TSY-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                        MOVE  HKN-TSY-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                        MOVE  HKN-TSY-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-TSY-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI3-M
                        ELSE
                              MOVE  HKN-TSY-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI3-M
                        END-IF
                      END-IF
                   WHEN   "3"
                      IF  HKN-TNK-GAIFTNKBN       =  "0"
                        MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                        MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                        MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                        MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                        MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-HON-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI3-M
                        ELSE
                              MOVE  HKN-HON-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI3-M
                        END-IF
                      ELSE
                        MOVE  HKN-TNK-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                        MOVE  HKN-TNK-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                        MOVE  HKN-TNK-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                        MOVE  HKN-TNK-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                        MOVE  HKN-TNK-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-TNK-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI3-M
                        ELSE
                              MOVE  HKN-TNK-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI3-M
                        END-IF
                      END-IF
                 END-EVALUATE
               ELSE
                 MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
                 MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
                 MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
                 MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
                 MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
                 IF    FLG-INGAI          =   "1"
                       MOVE  HKN-HON-GAIMGAIJGNGAK
                                          TO  MAX-KIN-KOHI3-M
                 ELSE
                       MOVE  HKN-HON-GAIMNAIJGNGAK
                                          TO  MAX-KIN-KOHI3-M
                 END-IF
               END-IF
             END-IF
           ELSE
             MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI3-K
             MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI3-K
             MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI3-D
             MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI3-M
             MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI3-D
             IF    FLG-INGAI          =   "1"
                   MOVE  HKN-HON-GAIMGAIJGNGAK
                                      TO  MAX-KIN-KOHI3-M
             ELSE
                   MOVE  HKN-HON-GAIMNAIJGNGAK
                                      TO  MAX-KIN-KOHI3-M
             END-IF
           END-IF.
      *
           IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" ) OR
                (FLG-KOUREI-KBN               =       "1" ))
           AND ( COMB-KOH3HKNNUM(1:1)        >=       "1"  )
           AND ( COMB-KOH3HKNNUM(1:1)        <=       "8"  )
           AND ( LNK-SYU-SRYYMD(1:6)         <     "200610")
              IF  (WK-1001-PREFNUM       NOT   =
                                    20 AND 15 AND 22 AND 18 AND 03 )
              AND (MAX-KIN-KOHI3-M             >   12000           )
                IF    WK-1001-PREFNUM       =   07
                AND   COMB-KOH3HKNNUM       =  "246"
                AND   LNK-SYU-SRYYMD(1:6)  >=  "200604"
                     CONTINUE
                ELSE
                     MOVE    12000   TO    MAX-KIN-KOHI3-M
                END-IF
              END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  11   ) AND
               ( COMB-KOH3HKNNUM(2:2)    = "41"  )
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI3-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI3-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI3-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI3-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  22             ) AND
               ( COMB-KOH3HKNNUM         = "184" OR "185"  )
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI3-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI3-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI3-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI3-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  13             ) AND
               ( COMB-KOH3HKNNUM         = "180"           )
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI3-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI3-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI3-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI3-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  33   ) AND
               ( COMB-KOH3HKNNUM(2:2)    = "80"  ) AND
               ( LNK-SYU-SRYYMD(1:6)  >= "200610")
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972")
               IF   MAX-KIN-KOHI3-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI3-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH4HKNNUM         = "974")
               IF   MAX-KIN-KOHI3-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI3-M
               END-IF
             END-IF
           END-IF.
      *
       2105-KOH3-KAISU-EXT.
           EXIT.
      ******************************************************************
      *        ¸øÈñ£´¡Ê¶â³Û¡Ë¼è¹þ¤ß½èÍý                                *
      ******************************************************************
       2106-KOH4-KAISU-SEC           SECTION.
      *
           IF    COMB-KOH4HKNNUM(1:1)   >=  "1" AND <=  "8"
             IF  WK-1001-CHIHOUNUMKBN       =   "1"
               EVALUATE   FLG-SANSHOU2
                 WHEN   "1"
                    MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                    MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                    MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                    MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                    MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                    IF    FLG-INGAI          =   "1"
                          MOVE  HKN-HON-GAIMGAIJGNGAK
                                             TO  MAX-KIN-KOHI4-M
                    ELSE
                          MOVE  HKN-HON-GAIMNAIJGNGAK
                                             TO  MAX-KIN-KOHI4-M
                    END-IF
                 WHEN   "2"
                    IF   HKN-TSY-GAIFTNKBN      =  "0"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI4-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI4-M
                      END-IF
                    ELSE
                      MOVE  HKN-TSY-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                      MOVE  HKN-TSY-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                      MOVE  HKN-TSY-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                      MOVE  HKN-TSY-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                      MOVE  HKN-TSY-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-TSY-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI4-M
                      ELSE
                            MOVE  HKN-TSY-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI4-M
                      END-IF
                    END-IF
                 WHEN   "3"
                    IF  HKN-TNK-GAIFTNKBN       =  "0"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI4-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI4-M
                      END-IF
                    ELSE
                      MOVE  HKN-TNK-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                      MOVE  HKN-TNK-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                      MOVE  HKN-TNK-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                      MOVE  HKN-TNK-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                      MOVE  HKN-TNK-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-TNK-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI4-M
                      ELSE
                            MOVE  HKN-TNK-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI4-M
                      END-IF
                    END-IF
               END-EVALUATE
             ELSE
               IF (( WK-1001-PREFNUM   =  28                      ) AND
                   ( COMB-KOH4HKNNUM   = "241" OR "180" OR
                                         "182" OR "143" OR
                                         "185" OR "127"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200507"          ))
               OR (( WK-1001-PREFNUM   =  11                      ) AND
                   ( COMB-KOH4HKNNUM   = "344"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200507"          ))
               OR (( WK-1001-PREFNUM   =  05                      ) AND
                   ( COMB-KOH4HKNNUM   = "374" OR "474"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200508"          ))
               OR (( WK-1001-PREFNUM   =  32                      ) AND
                   ( COMB-KOH4HKNNUM   = "191" OR "391"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200510"          ))
               OR (( WK-1001-PREFNUM   =  28                      ) AND
                   ( COMB-KOH4HKNNUM   = "100"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200604"          ))
               OR (( WK-1001-PREFNUM   =  33                      ) AND
                   ( COMB-KOH4HKNNUM   = "141" OR "285" OR
                                         "485" OR "180" OR "186"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200610"          ))
               OR (( WK-1001-PREFNUM   =  19                      ) AND
                   ( COMB-KOH4HKNNUM   = "141"                    ))
               OR (( WK-1001-PREFNUM   =  23                      ) AND
                   ( COMB-KOH4HKNNUM   = "141"                    ) AND
                   ( LNK-SYU-SRYYMD(1:6)   >=   "200610"          ))
                 EVALUATE   FLG-SANSHOU2
                   WHEN   "1"
                      MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                      MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                      MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                      MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                      MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                      IF    FLG-INGAI          =   "1"
                            MOVE  HKN-HON-GAIMGAIJGNGAK
                                               TO  MAX-KIN-KOHI4-M
                      ELSE
                            MOVE  HKN-HON-GAIMNAIJGNGAK
                                               TO  MAX-KIN-KOHI4-M
                      END-IF
                   WHEN   "2"
                      IF   HKN-TSY-GAIFTNKBN      =  "0"
                        MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                        MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                        MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                        MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                        MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-HON-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI4-M
                        ELSE
                              MOVE  HKN-HON-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI4-M
                        END-IF
                      ELSE
                        MOVE  HKN-TSY-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                        MOVE  HKN-TSY-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                        MOVE  HKN-TSY-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                        MOVE  HKN-TSY-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                        MOVE  HKN-TSY-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-TSY-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI4-M
                        ELSE
                              MOVE  HKN-TSY-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI4-M
                        END-IF
                      END-IF
                   WHEN   "3"
                      IF  HKN-TNK-GAIFTNKBN       =  "0"
                        MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                        MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                        MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                        MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                        MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-HON-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI4-M
                        ELSE
                              MOVE  HKN-HON-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI4-M
                        END-IF
                      ELSE
                        MOVE  HKN-TNK-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                        MOVE  HKN-TNK-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                        MOVE  HKN-TNK-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                        MOVE  HKN-TNK-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                        MOVE  HKN-TNK-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                        IF    FLG-INGAI          =   "1"
                              MOVE  HKN-TNK-GAIMGAIJGNGAK
                                                 TO  MAX-KIN-KOHI4-M
                        ELSE
                              MOVE  HKN-TNK-GAIMNAIJGNGAK
                                                 TO  MAX-KIN-KOHI4-M
                        END-IF
                      END-IF
                 END-EVALUATE
               ELSE
                 MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
                 MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
                 MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
                 MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
                 MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
                 IF    FLG-INGAI          =   "1"
                       MOVE  HKN-HON-GAIMGAIJGNGAK
                                          TO  MAX-KIN-KOHI4-M
                 ELSE
                       MOVE  HKN-HON-GAIMNAIJGNGAK
                                          TO  MAX-KIN-KOHI4-M
                 END-IF
               END-IF
             END-IF
           ELSE
             MOVE  HKN-HON-GAIKKOTGAK TO  TEI-KIN-KOHI4-K
             MOVE  HKN-HON-GAIKJGNGAK TO  MAX-KIN-KOHI4-K
             MOVE  HKN-HON-GAIDJGNCNT TO  MAX-KAI-KOHI4-D
             MOVE  HKN-HON-GAIMJGNCNT TO  MAX-KAI-KOHI4-M
             MOVE  HKN-HON-GAIDJGNGAK TO  MAX-KIN-KOHI4-D
             IF    FLG-INGAI          =   "1"
                   MOVE  HKN-HON-GAIMGAIJGNGAK
                                      TO  MAX-KIN-KOHI4-M
             ELSE
                   MOVE  HKN-HON-GAIMNAIJGNGAK
                                      TO  MAX-KIN-KOHI4-M
             END-IF
           END-IF.
      *
           IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" ) OR
                (FLG-KOUREI-KBN               =       "1" ))
           AND ( COMB-KOH4HKNNUM(1:1)        >=       "1"  )
           AND ( COMB-KOH4HKNNUM(1:1)        <=       "8"  )
           AND ( LNK-SYU-SRYYMD(1:6)         <     "200610")
              IF  (WK-1001-PREFNUM       NOT   =
                                    20 AND 15 AND 22 AND 18 AND 03 )
              AND (MAX-KIN-KOHI4-M             >   12000           )
                IF    WK-1001-PREFNUM       =   07
                AND   COMB-KOH4HKNNUM       =  "246"
                AND   LNK-SYU-SRYYMD(1:6)  >=  "200604"
                     CONTINUE
                ELSE
                     MOVE    12000   TO    MAX-KIN-KOHI4-M
                END-IF
              END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  11   ) AND
               ( COMB-KOH4HKNNUM(2:2)    = "41"  )
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972")
               IF   MAX-KIN-KOHI4-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI4-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974")
               IF   MAX-KIN-KOHI4-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI4-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  22             ) AND
               ( COMB-KOH4HKNNUM         = "184" OR "185"  )
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972")
               IF   MAX-KIN-KOHI4-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI4-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974")
               IF   MAX-KIN-KOHI4-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI4-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  13             ) AND
               ( COMB-KOH4HKNNUM         = "180"           )
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972")
               IF   MAX-KIN-KOHI4-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI4-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974")
               IF   MAX-KIN-KOHI4-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI4-M
               END-IF
             END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  33   ) AND
               ( COMB-KOH4HKNNUM(2:2)    = "80"  ) AND
               ( LNK-SYU-SRYYMD(1:6)  >= "200610")
             IF (COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972")
               IF   MAX-KIN-KOHI4-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI4-M
               END-IF
             END-IF
             IF (COMB-KOH1HKNNUM         = "974") OR
                (COMB-KOH2HKNNUM         = "974") OR
                (COMB-KOH3HKNNUM         = "974")
               IF   MAX-KIN-KOHI4-M   >     20000
                     MOVE    20000   TO    MAX-KIN-KOHI4-M
               END-IF
             END-IF
           END-IF.
      *
       2106-KOH4-KAISU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ´µ¼Ô¸øÈñÉéÃ´¡Ê¶â³Û¡Ë¼è¹þ¤ß½èÍý                                *
      ******************************************************************
       2107-KOHFTN-KINGAKU-SEC           SECTION.
      *
           INITIALIZE                   WK-JIRITU-TBL.
           INITIALIZE                   WK-CHIHOU-TAFTNRUIKEI-TBL.
      *
           PERFORM    VARYING   KOH-IDX    FROM   1   BY   1
                      UNTIL    (KOH-IDX    >      4)
      *
             IF ( COMB-KOHHKNNUM(KOH-IDX)      = "015" OR "016" 
                                                       OR "012"      )
      *
             OR ((COMB-KOHHKNNUM(KOH-IDX)      = "051"   )  AND
                 (LNK-SYU-SRYYMD(1:6)         >= "200310")           )
      *
             OR ((WK-1001-PREFNUM              =  13            )  AND
                 (COMB-KOHHKNNUM(KOH-IDX)      = "151" OR "451" )  AND
                 (LNK-SYU-SRYYMD(1:6)         >= "200310")           )
      *
             OR ((WK-1001-PREFNUM              =  28     )  AND
                 (COMB-KOHHKNNUM(KOH-IDX)      = "186"   )  AND
                 (LNK-SYU-SRYYMD(1:6)         >= "200310"))
      *
             OR ((COMB-KOHHKNNUM(KOH-IDX)      = "052"   )  AND
                 (LNK-SYU-SRYYMD(1:6)         >= "200504")           )
      *
             OR ((COMB-KOHHKNNUM(KOH-IDX)      = "021"   )  AND
                 (LNK-SYU-SRYYMD(1:6)         >= "200604")           )
      *
             OR ((COMB-KOHHKNNUM(KOH-IDX)      = "024" OR "079" )  AND
                 (LNK-SYU-SRYYMD(1:6)         >= "200610")           )
      *
             OR ((WK-1001-PREFNUM              =  07     )  AND
                 (COMB-KOHHKNNUM(KOH-IDX)      = "281"   )  AND
                 (LNK-SYU-SRYYMD(1:6)         >= "200702"))
      *
      ***    OR ( COMB-KOHHKNNUM(KOH-IDX)(1:1) = "1" OR "2" OR "3" OR 
      ***                                        "4" OR "5" OR "6" OR
      ***                                        "7" OR "8"          )
      *
                MOVE       ZERO           TO  WK-GAIJGNGAK
      *
      *         ¸øÈñ·î¾å¸Â³Û¼èÆÀ
                INITIALIZE                    LNK-ORCSJGNGET-REC
                MOVE  LNK-SYU-HOSPID      TO  LNK-ORCSJGNGET-HOSPID
                MOVE  LNK-SYU-NYUGAIKBN   TO  LNK-ORCSJGNGET-NYUGAIKBN
                MOVE  LNK-SYU-PTID        TO  LNK-ORCSJGNGET-PTID
                MOVE  COMB-KOHHKNNUM(KOH-IDX)
                                          TO  LNK-ORCSJGNGET-KOHHKNNUM
                MOVE  COMB-KOHID(KOH-IDX) TO  LNK-ORCSJGNGET-KOHID
                MOVE  LNK-SYU-SRYYMD(1:6) TO  LNK-ORCSJGNGET-SRYYM
      *
                CALL    "ORCSJGNGET"      USING
                                          LNK-ORCSJGNGET-REC
      *
                IF    LNK-ORCSJGNGET-RC   =   ZERO
                   MOVE    LNK-ORCSJGNGET-GAIJGNGAK
                                          TO  WK-GAIJGNGAK
                   EVALUATE   KOH-IDX
                      WHEN  1
                        MOVE   WK-GAIJGNGAK        TO   MAX-KIN-KOHI1-M
                        MOVE     "1"   TO    COMB-KOH1-GAIJGNKBN
                        IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1" ) OR
                            (FLG-KOUREI-KBN               =   "1" )
                            IF  COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF   (COMB-HKNNUM  NOT   =   SPACE)
                              AND  (MAX-KIN-KOHI1-M    >   8000 )
                                MOVE     8000    TO   MAX-KIN-KOHI1-M
                              END-IF
                            ELSE
                              IF  ((COMB-KOHHKNNUM(KOH-IDX) =
                                           "015" OR "016" OR "021" ) AND
                                   (LNK-SYU-SRYYMD(1:6) >= "200604"))
                              OR  ((COMB-KOHHKNNUM(KOH-IDX) =
                                           "024" OR "079"          ) AND
                                   (LNK-SYU-SRYYMD(1:6) >= "200610"))
                                 CONTINUE
                              ELSE
                                 IF    MAX-KIN-KOHI1-M    >
                                                      WK-KOH-GENDOGAKU
                                   MOVE  WK-KOH-GENDOGAKU
                                                 TO   MAX-KIN-KOHI1-M
                                 END-IF
                              END-IF
                            END-IF
                            IF    FLG-CHIHOU(1)      =  "1"
                               MOVE   SPACE    TO   FLG-CHIHOU(1)
                            END-IF
                        END-IF
                        IF   FLG-GENBUTU70   NOT =   ZERO
                            IF    COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF  FLG-KOHYONKAIME = 1
                                 MOVE  24600   TO   WK-GEN70-GENDOGAKU2
                              ELSE
                                 MOVE  35400   TO   WK-GEN70-GENDOGAKU2
                              END-IF
                              IF  MAX-KIN-KOHI1-M > WK-GEN70-GENDOGAKU2
                                MOVE WK-GEN70-GENDOGAKU2
                                     TO MAX-KIN-KOHI1-M
                              END-IF
                            ELSE
                              IF  COMB-KOHHKNNUM(KOH-IDX) =
                                           "015" OR "016" OR "021"
                                                 OR "024" OR "079"
                                 CONTINUE
                              ELSE
                                 IF  MAX-KIN-KOHI1-M   >
                                                  WK-GEN70-GENDOGAKU
                                   MOVE  WK-GEN70-GENDOGAKU
                                      TO MAX-KIN-KOHI1-M
                                 END-IF
                              END-IF
                            END-IF
                        END-IF
                        IF  (COMB-KOH1HKNNUM = "015" OR "016" OR "021")
                        AND (LNK-SYU-SRYYMD(1:6)         >= "200604"  )
                            IF  MAX-KIN-KOHI1-M = 9999999 OR 999999
                              MOVE  1  TO WK-JIRITU-CYUKANSOU(KOH-IDX)
                              MOVE  ZERO
                                       TO WK-JIRITU-TAFTNRUIKEI(KOH-IDX)
                            ELSE
                              MOVE  2  TO WK-JIRITU-CYUKANSOU(KOH-IDX)
                              MOVE  LNK-ORCSJGNGET-TAFTNRUIKEI
                                       TO WK-JIRITU-TAFTNRUIKEI(KOH-IDX)
                            END-IF
                        END-IF
                        IF  (WK-1001-PREFNUM      =  07       )
                        AND (COMB-KOH1HKNNUM      = "281"     )
                        AND (LNK-SYU-SRYYMD(1:6) >= "200702"  )
                              MOVE  LNK-ORCSJGNGET-TAFTNRUIKEI
                                   TO WK-CHIHOU-TAFTNRUIKEI(KOH-IDX)
                        END-IF
                        IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1" ) OR
                            (FLG-KOUREI-KBN               =   "1" )
                         IF  (COMB-KOH1HKNNUM = "015" OR "016" OR "021")
                         AND (LNK-SYU-SRYYMD(1:6)         >= "200604"  )
                         AND (WK-JIRITU-CYUKANSOU(KOH-IDX) =  1        )
                            IF    MAX-KIN-KOHI1-M    >  WK-KOH-GENDOGAKU
                                MOVE  WK-KOH-GENDOGAKU
                                                   TO   MAX-KIN-KOHI1-M
                            END-IF
                         END-IF
                        END-IF
                        IF   FLG-GENBUTU70   NOT =   ZERO
                         IF  (COMB-KOH1HKNNUM = "015" OR "016" OR "021")
                         AND (WK-JIRITU-CYUKANSOU(KOH-IDX) =  1        )
                            IF    MAX-KIN-KOHI1-M  >  WK-GEN70-GENDOGAKU
                              MOVE    WK-GEN70-GENDOGAKU
                                      TO MAX-KIN-KOHI1-M
                              EVALUATE  WK-GEN70-GENDOGAKU
                               WHEN       80100
                                 MOVE  WK-ZENGAKU TO WK-KOUREI-IRYOHI70K
                                 MOVE  2          TO FLG-IRYOHI70K
                              END-EVALUATE
                            END-IF
                         END-IF
                        END-IF
                        IF  ( COMB-KOH1HKNNUM  = "015" OR "016") AND
                            ((COMB-KOH2HKNNUM  = "972") OR
                             (COMB-KOH3HKNNUM  = "972") OR
                             (COMB-KOH4HKNNUM  = "972"))
                          IF  (LNK-SYU-SRYYMD(1:6)   >= "200604")
                          AND (WK-JIRITU-CYUKANSOU(KOH-IDX) =  2)
                           CONTINUE
                          ELSE
                           IF   MAX-KIN-KOHI1-M   >     10000
                              MOVE    10000   TO    MAX-KIN-KOHI1-M
                           END-IF
                          END-IF
                        END-IF
                        IF  ( COMB-KOH1HKNNUM  = "015" OR "016") AND
                            ((COMB-KOH2HKNNUM  = "974") OR
                             (COMB-KOH3HKNNUM  = "974") OR
                             (COMB-KOH4HKNNUM  = "974"))
                          IF  (LNK-SYU-SRYYMD(1:6)   >= "200604")
                          AND (WK-JIRITU-CYUKANSOU(KOH-IDX) =  2)
                           CONTINUE
                          ELSE
                           IF   MAX-KIN-KOHI1-M   >     20000
                              MOVE    20000   TO    MAX-KIN-KOHI1-M
                           END-IF
                          END-IF
                        END-IF
                        IF  (COMB-KOH1HKNNUM = "015" OR "016" OR "021")
                        AND (LNK-SYU-SRYYMD(1:6)         >= "200604"  )
                            IF  MAX-KIN-KOHI1-M = 9999999 OR 999999
                              IF  LNK-SYU-SRYYMD(1:6)  >= "200610"
                                MOVE  80100   TO   MAX-KIN-KOHI1-M
                                MOVE  WK-ZENGAKU  TO  WK-IRYOHI
                                MOVE  1           TO  FLG-IRYOHI
                              ELSE
                                MOVE  72300   TO   MAX-KIN-KOHI1-M
                                MOVE  WK-ZENGAKU  TO  WK-IRYOHI
                                MOVE  1           TO  FLG-IRYOHI
                              END-IF
                            END-IF
                        END-IF
                        IF  (WK-1001-PREFNUM =  28                    )
                        AND (COMB-KOH1HKNNUM = "015" OR "016" OR "021")
                        AND (LNK-SYU-SRYYMD(1:6)         >= "200604"  )
                          IF   COMB-KOH2HKNNUM =  "100" OR "200"
                            MOVE  TEI-KIN-KOHI2-K  TO  TEI-KIN-KOHI1-K
                            MOVE  MAX-KIN-KOHI2-K  TO  MAX-KIN-KOHI1-K
                            MOVE  MAX-KAI-KOHI2-D  TO  MAX-KAI-KOHI1-D
                            MOVE  MAX-KAI-KOHI2-M  TO  MAX-KAI-KOHI1-M
                            MOVE  MAX-KIN-KOHI2-D  TO  MAX-KIN-KOHI1-D
                            IF   COMB-KOH2HKNNUM  =  "200"
                              MOVE   ZERO          TO  MAX-KIN-KOHI1-M
                            ELSE
                              MOVE MAX-KIN-KOHI2-M TO  MAX-KIN-KOHI1-M
                            END-IF
                            MOVE  ZERO           TO  WK-IRYOHI
                            MOVE  ZERO           TO  FLG-IRYOHI
                          END-IF
                          IF   COMB-KOH3HKNNUM =  "100" OR "200"
                            MOVE  TEI-KIN-KOHI3-K  TO  TEI-KIN-KOHI1-K
                            MOVE  MAX-KIN-KOHI3-K  TO  MAX-KIN-KOHI1-K
                            MOVE  MAX-KAI-KOHI3-D  TO  MAX-KAI-KOHI1-D
                            MOVE  MAX-KAI-KOHI3-M  TO  MAX-KAI-KOHI1-M
                            MOVE  MAX-KIN-KOHI3-D  TO  MAX-KIN-KOHI1-D
                            IF   COMB-KOH3HKNNUM  =  "200"
                              MOVE   ZERO          TO  MAX-KIN-KOHI1-M
                            ELSE
                              MOVE MAX-KIN-KOHI3-M TO  MAX-KIN-KOHI1-M
                            END-IF
                            MOVE  ZERO           TO  WK-IRYOHI
                            MOVE  ZERO           TO  FLG-IRYOHI
                          END-IF
                          IF   COMB-KOH4HKNNUM =  "100" OR "200"
                            MOVE  TEI-KIN-KOHI4-K  TO  TEI-KIN-KOHI1-K
                            MOVE  MAX-KIN-KOHI4-K  TO  MAX-KIN-KOHI1-K
                            MOVE  MAX-KAI-KOHI4-D  TO  MAX-KAI-KOHI1-D
                            MOVE  MAX-KAI-KOHI4-M  TO  MAX-KAI-KOHI1-M
                            MOVE  MAX-KIN-KOHI4-D  TO  MAX-KIN-KOHI1-D
                            IF   COMB-KOH4HKNNUM  =  "200"
                              MOVE   ZERO          TO  MAX-KIN-KOHI1-M
                            ELSE
                              MOVE MAX-KIN-KOHI4-M TO  MAX-KIN-KOHI1-M
                            END-IF
                            MOVE  ZERO           TO  WK-IRYOHI
                            MOVE  ZERO           TO  FLG-IRYOHI
                          END-IF
                        END-IF
                      WHEN  2
                        MOVE   WK-GAIJGNGAK        TO   MAX-KIN-KOHI2-M
                        MOVE     "1"   TO    COMB-KOH2-GAIJGNKBN
                        IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1" ) OR
                            (FLG-KOUREI-KBN               =   "1" )
                            IF  COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF   (COMB-HKNNUM  NOT   =   SPACE)
                              AND  (MAX-KIN-KOHI2-M    >   8000 )
                                MOVE     8000    TO   MAX-KIN-KOHI2-M
                              END-IF
                            ELSE
                              IF  ((COMB-KOHHKNNUM(KOH-IDX) =
                                           "015" OR "016" OR "021" ) AND
                                   (LNK-SYU-SRYYMD(1:6) >= "200604"))
                              OR  ((COMB-KOHHKNNUM(KOH-IDX) =
                                           "024" OR "079"          ) AND
                                   (LNK-SYU-SRYYMD(1:6) >= "200610"))
                                 CONTINUE
                              ELSE
                                 IF    MAX-KIN-KOHI2-M    >
                                                      WK-KOH-GENDOGAKU
                                   MOVE  WK-KOH-GENDOGAKU
                                                 TO   MAX-KIN-KOHI2-M
                                 END-IF
                              END-IF
                            END-IF
                            IF    FLG-CHIHOU(2)      =  "1"
                               MOVE   SPACE    TO   FLG-CHIHOU(2)
                            END-IF
                        END-IF
                        IF   FLG-GENBUTU70   NOT =   ZERO
                            IF    COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF  FLG-KOHYONKAIME = 1
                                 MOVE  24600   TO   WK-GEN70-GENDOGAKU2
                              ELSE
                                 MOVE  35400   TO   WK-GEN70-GENDOGAKU2
                              END-IF
                              IF  MAX-KIN-KOHI2-M > WK-GEN70-GENDOGAKU2
                                MOVE WK-GEN70-GENDOGAKU2
                                     TO MAX-KIN-KOHI2-M
                              END-IF
                            ELSE
                              IF  COMB-KOHHKNNUM(KOH-IDX) =
                                           "015" OR "016" OR "021"
                                                 OR "024" OR "079"
                                 CONTINUE
                              ELSE
                                 IF  MAX-KIN-KOHI2-M   >
                                                  WK-GEN70-GENDOGAKU
                                   MOVE  WK-GEN70-GENDOGAKU
                                      TO MAX-KIN-KOHI2-M
                                 END-IF
                              END-IF
                            END-IF
                        END-IF
                        IF  (COMB-KOH2HKNNUM = "015" OR "016" OR "021")
                        AND (LNK-SYU-SRYYMD(1:6)         >= "200604"  )
                            IF  MAX-KIN-KOHI2-M = 9999999 OR 999999
                              MOVE  1  TO WK-JIRITU-CYUKANSOU(KOH-IDX)
                              MOVE  ZERO
                                       TO WK-JIRITU-TAFTNRUIKEI(KOH-IDX)
                            ELSE
                              MOVE  2  TO WK-JIRITU-CYUKANSOU(KOH-IDX)
                              MOVE  LNK-ORCSJGNGET-TAFTNRUIKEI
                                       TO WK-JIRITU-TAFTNRUIKEI(KOH-IDX)
                            END-IF
                        END-IF
                        IF  (WK-1001-PREFNUM      =  07       )
                        AND (COMB-KOH2HKNNUM      = "281"     )
                        AND (LNK-SYU-SRYYMD(1:6) >= "200702"  )
                              MOVE  LNK-ORCSJGNGET-TAFTNRUIKEI
                                   TO WK-CHIHOU-TAFTNRUIKEI(KOH-IDX)
                        END-IF
                        IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1" ) OR
                            (FLG-KOUREI-KBN               =   "1" )
                         IF  (COMB-KOH2HKNNUM = "015" OR "016" OR "021")
                         AND (LNK-SYU-SRYYMD(1:6)         >= "200604"  )
                         AND (WK-JIRITU-CYUKANSOU(KOH-IDX) =  1        )
                            IF    MAX-KIN-KOHI2-M    >  WK-KOH-GENDOGAKU
                                MOVE  WK-KOH-GENDOGAKU
                                                   TO   MAX-KIN-KOHI2-M
                            END-IF
                         END-IF
                        END-IF
                        IF  ( COMB-KOH2HKNNUM  = "015" OR "016") AND
                            ((COMB-KOH1HKNNUM  = "972") OR
                             (COMB-KOH3HKNNUM  = "972") OR
                             (COMB-KOH4HKNNUM  = "972"))
                          IF  (LNK-SYU-SRYYMD(1:6)   >= "200604")
                          AND (WK-JIRITU-CYUKANSOU(KOH-IDX) =  2)
                           CONTINUE
                          ELSE
                           IF   MAX-KIN-KOHI2-M   >     10000
                              MOVE    10000   TO    MAX-KIN-KOHI2-M
                           END-IF
                          END-IF
                        END-IF
                        IF  ( COMB-KOH2HKNNUM  = "015" OR "016") AND
                            ((COMB-KOH1HKNNUM  = "974") OR
                             (COMB-KOH3HKNNUM  = "974") OR
                             (COMB-KOH4HKNNUM  = "974"))
                          IF  (LNK-SYU-SRYYMD(1:6)   >= "200604")
                          AND (WK-JIRITU-CYUKANSOU(KOH-IDX) =  2)
                           CONTINUE
                          ELSE
                           IF   MAX-KIN-KOHI2-M   >     20000
                              MOVE    20000   TO    MAX-KIN-KOHI2-M
                           END-IF
                          END-IF
                        END-IF
                        IF  (COMB-KOH2HKNNUM = "015" OR "016" OR "021")
                        AND (LNK-SYU-SRYYMD(1:6)         >= "200604"  )
                            IF  MAX-KIN-KOHI2-M = 9999999 OR 999999
                              IF  LNK-SYU-SRYYMD(1:6)  >= "200610"
                                MOVE  80100   TO   MAX-KIN-KOHI2-M
                              ELSE
                                MOVE  72300   TO   MAX-KIN-KOHI2-M
                              END-IF
                            END-IF
                        END-IF
                        IF  (WK-1001-PREFNUM =  28                    )
                        AND (COMB-KOH2HKNNUM = "015" OR "016" OR "021")
                        AND (LNK-SYU-SRYYMD(1:6)         >= "200604"  )
                          IF   COMB-KOH3HKNNUM =  "100" OR "200"
                            MOVE  TEI-KIN-KOHI3-K  TO  TEI-KIN-KOHI2-K
                            MOVE  MAX-KIN-KOHI3-K  TO  MAX-KIN-KOHI2-K
                            MOVE  MAX-KAI-KOHI3-D  TO  MAX-KAI-KOHI2-D
                            MOVE  MAX-KAI-KOHI3-M  TO  MAX-KAI-KOHI2-M
                            MOVE  MAX-KIN-KOHI3-D  TO  MAX-KIN-KOHI2-D
                            IF   COMB-KOH3HKNNUM  =  "200"
                              MOVE   ZERO          TO  MAX-KIN-KOHI2-M
                            ELSE
                              MOVE MAX-KIN-KOHI3-M TO  MAX-KIN-KOHI2-M
                            END-IF
                            MOVE  ZERO           TO  WK-IRYOHI
                            MOVE  ZERO           TO  FLG-IRYOHI
                          END-IF
                          IF   COMB-KOH4HKNNUM =  "100" OR "200"
                            MOVE  TEI-KIN-KOHI4-K  TO  TEI-KIN-KOHI2-K
                            MOVE  MAX-KIN-KOHI4-K  TO  MAX-KIN-KOHI2-K
                            MOVE  MAX-KAI-KOHI4-D  TO  MAX-KAI-KOHI2-D
                            MOVE  MAX-KAI-KOHI4-M  TO  MAX-KAI-KOHI2-M
                            MOVE  MAX-KIN-KOHI4-D  TO  MAX-KIN-KOHI2-D
                            IF   COMB-KOH4HKNNUM  =  "200"
                              MOVE   ZERO          TO  MAX-KIN-KOHI2-M
                            ELSE
                              MOVE MAX-KIN-KOHI4-M TO  MAX-KIN-KOHI2-M
                            END-IF
                            MOVE  ZERO           TO  WK-IRYOHI
                            MOVE  ZERO           TO  FLG-IRYOHI
                          END-IF
                        END-IF
                      WHEN  3
                        MOVE   WK-GAIJGNGAK        TO   MAX-KIN-KOHI3-M
                        MOVE     "1"   TO    COMB-KOH3-GAIJGNKBN
                        IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1" ) OR
                            (FLG-KOUREI-KBN               =   "1" )
                            IF  COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF   (COMB-HKNNUM  NOT   =   SPACE)
                              AND  (MAX-KIN-KOHI3-M    >   8000 )
                                MOVE     8000    TO   MAX-KIN-KOHI3-M
                              END-IF
                            ELSE
                              IF  ((COMB-KOHHKNNUM(KOH-IDX) =
                                           "015" OR "016" OR "021" ) AND
                                   (LNK-SYU-SRYYMD(1:6) >= "200604"))
                              OR  ((COMB-KOHHKNNUM(KOH-IDX) =
                                           "024" OR "079"          ) AND
                                   (LNK-SYU-SRYYMD(1:6) >= "200610"))
                                 CONTINUE
                              ELSE
                                 IF    MAX-KIN-KOHI3-M    >
                                                      WK-KOH-GENDOGAKU
                                   MOVE  WK-KOH-GENDOGAKU
                                                 TO   MAX-KIN-KOHI3-M
                                 END-IF
                              END-IF
                            END-IF
                            IF    FLG-CHIHOU(3)      =  "1"
                               MOVE   SPACE    TO   FLG-CHIHOU(3)
                            END-IF
                        END-IF
                        IF   FLG-GENBUTU70   NOT =   ZERO
                            IF    COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF  FLG-KOHYONKAIME = 1
                                 MOVE  24600   TO   WK-GEN70-GENDOGAKU2
                              ELSE
                                 MOVE  35400   TO   WK-GEN70-GENDOGAKU2
                              END-IF
                              IF  MAX-KIN-KOHI3-M > WK-GEN70-GENDOGAKU2
                                MOVE WK-GEN70-GENDOGAKU2
                                     TO MAX-KIN-KOHI3-M
                              END-IF
                            ELSE
                              IF  COMB-KOHHKNNUM(KOH-IDX) =
                                           "015" OR "016" OR "021"
                                                 OR "024" OR "079"
                                 CONTINUE
                              ELSE
                                 IF  MAX-KIN-KOHI3-M   >
                                                  WK-GEN70-GENDOGAKU
                                   MOVE  WK-GEN70-GENDOGAKU
                                      TO MAX-KIN-KOHI3-M
                                 END-IF
                              END-IF
                            END-IF
                        END-IF
                        IF  (WK-1001-PREFNUM      =  07       )
                        AND (COMB-KOH3HKNNUM      = "281"     )
                        AND (LNK-SYU-SRYYMD(1:6) >= "200702"  )
                              MOVE  LNK-ORCSJGNGET-TAFTNRUIKEI
                                   TO WK-CHIHOU-TAFTNRUIKEI(KOH-IDX)
                        END-IF
                      WHEN  4
                        MOVE   WK-GAIJGNGAK        TO   MAX-KIN-KOHI4-M
                        MOVE     "1"   TO    COMB-KOH4-GAIJGNKBN
                        IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1" ) OR
                            (FLG-KOUREI-KBN               =   "1" )
                            IF  COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF   (COMB-HKNNUM  NOT   =   SPACE)
                              AND  (MAX-KIN-KOHI4-M    >   8000 )
                                MOVE     8000    TO   MAX-KIN-KOHI4-M
                              END-IF
                            ELSE
                              IF  ((COMB-KOHHKNNUM(KOH-IDX) =
                                           "015" OR "016" OR "021" ) AND
                                   (LNK-SYU-SRYYMD(1:6) >= "200604"))
                              OR  ((COMB-KOHHKNNUM(KOH-IDX) =
                                           "024" OR "079"          ) AND
                                   (LNK-SYU-SRYYMD(1:6) >= "200610"))
                                 CONTINUE
                              ELSE
                                 IF    MAX-KIN-KOHI4-M    >
                                                      WK-KOH-GENDOGAKU
                                   MOVE  WK-KOH-GENDOGAKU
                                                 TO   MAX-KIN-KOHI4-M
                                 END-IF
                              END-IF
                            END-IF
                            IF    FLG-CHIHOU(4)      =  "1"
                               MOVE   SPACE    TO   FLG-CHIHOU(4)
                            END-IF
                        END-IF
                        IF   FLG-GENBUTU70   NOT =   ZERO
                            IF    COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF  FLG-KOHYONKAIME = 1
                                 MOVE  24600   TO   WK-GEN70-GENDOGAKU2
                              ELSE
                                 MOVE  35400   TO   WK-GEN70-GENDOGAKU2
                              END-IF
                              IF  MAX-KIN-KOHI4-M > WK-GEN70-GENDOGAKU2
                                MOVE WK-GEN70-GENDOGAKU2
                                     TO MAX-KIN-KOHI4-M
                              END-IF
                            ELSE
                              IF  COMB-KOHHKNNUM(KOH-IDX) =
                                           "015" OR "016" OR "021"
                                                 OR "024" OR "079"
                                 CONTINUE
                              ELSE
                                 IF  MAX-KIN-KOHI4-M   >
                                                  WK-GEN70-GENDOGAKU
                                   MOVE  WK-GEN70-GENDOGAKU
                                      TO MAX-KIN-KOHI4-M
                                 END-IF
                              END-IF
                            END-IF
                        END-IF
                        IF  (WK-1001-PREFNUM      =  07       )
                        AND (COMB-KOH4HKNNUM      = "281"     )
                        AND (LNK-SYU-SRYYMD(1:6) >= "200702"  )
                              MOVE  LNK-ORCSJGNGET-TAFTNRUIKEI
                                   TO WK-CHIHOU-TAFTNRUIKEI(KOH-IDX)
                        END-IF
                   END-EVALUATE
                END-IF
      *
             END-IF
      *
           END-PERFORM.
      *
       2107-KOHFTN-KINGAKU-EXT.
           EXIT.
      *
      ******************************************************************
      *        £´£±Ï·¿Í¾å¸Â³ÛÊÔ½¸½èÍý
      ******************************************************************
       2108-ROUJIN41-JYOGEN-HENSYU-SEC           SECTION.
      *
           IF      FLG-GENBUTU70   NOT =   ZERO
      *      £´£±Ï·¿Í
             IF  (WK-1001-PREFNUM  =  14                         ) AND
                 (COMB-KOH1HKNNUM  =  "141" OR "142" OR "143"    )
                MOVE   WK-ZENGAKU   TO    WK-KOUREI-IRYOHI70K
                IF     COMB-KOH1HKNNUM  =  "141"
                   MOVE    11       TO    FLG-IRYOHI70K
                ELSE
                   MOVE    12       TO    FLG-IRYOHI70K
                END-IF
             END-IF
           ELSE
      *      £´£±Ï·¿Í
             IF  (WK-1001-PREFNUM  =  14                         ) AND
                 (COMB-KOH1HKNNUM  =  "141" OR "142" OR "143"    ) AND
                 (LNK-SYU-SRYYMD(1:6)   >=   "200610"            )
                MOVE   WK-ZENGAKU   TO    WK-IRYOHI
                IF     COMB-KOH1HKNNUM  =  "141"
                   MOVE     1       TO    FLG-IRYOHI2
                ELSE
                   MOVE     2       TO    FLG-IRYOHI2
                END-IF
             END-IF
           END-IF
      *
           .
      *
       2108-ROUJIN41-JYOGEN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ´µ¼Ô¸øÈñÉéÃ´¡Ê¸º³Û¡Ê³ä¹ç¡¦¶â³Û¡Ë¡Ë¼è¹þ¤ß½èÍý                                *
      ******************************************************************
       2109-KOHFTN-KINGAKU-GEN-SEC           SECTION.
      *
           IF   WK-GENGAKUKBN   =   1   OR   2
      *
                MOVE       ZERO           TO  WK-GENGAKU-RITU
                                              WK-GENGAKU-KIN
      *
      *         ¸øÈñ·î¾å¸Â³Û¼èÆÀ
                INITIALIZE                    LNK-ORCSJGNGET-REC
                MOVE  LNK-SYU-HOSPID      TO  LNK-ORCSJGNGET-HOSPID
                MOVE  LNK-SYU-NYUGAIKBN   TO  LNK-ORCSJGNGET-NYUGAIKBN
                MOVE  LNK-SYU-PTID        TO  LNK-ORCSJGNGET-PTID
                EVALUATE   WK-GENGAKUKBN
                  WHEN         1
                     MOVE    "960"        TO  LNK-ORCSJGNGET-KOHHKNNUM
                  WHEN         2
                     MOVE    "961"        TO  LNK-ORCSJGNGET-KOHHKNNUM
                END-EVALUATE
                MOVE  WK-GENGAKU-KOHID    TO  LNK-ORCSJGNGET-KOHID
                MOVE  LNK-SYU-SRYYMD(1:6) TO  LNK-ORCSJGNGET-SRYYM
      *
                CALL    "ORCSJGNGET"      USING
                                          LNK-ORCSJGNGET-REC
      *
                IF    LNK-ORCSJGNGET-RC   =   ZERO
                    EVALUATE   WK-GENGAKUKBN
                      WHEN          1
                         MOVE    LNK-ORCSJGNGET-GAIJGNGAK
                                          TO  WK-GENGAKU-RITU
                         IF   WK-GENGAKU-RITU  >  100
                             MOVE   100   TO  WK-GENGAKU-RITU
                         END-IF
                      WHEN          2
                         MOVE    LNK-ORCSJGNGET-GAIJGNGAK
                                          TO  WK-GENGAKU-KIN
                    END-EVALUATE
                END-IF
      *
           END-IF
      *
           IF   (LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" )
      ***> OR   (FLG-KOUREI-KBN               =       "1" )
      *
              IF   COMB-KOH1HKNNUM          =  "027"
                 IF   MAX-KIN-KOHI1-M   NOT =  ZERO
                   EVALUATE   WK-GENGAKUKBN
                     WHEN          1
                        IF  WK-GENGAKU-RITU = 100
                          MOVE   ZERO   TO   MAX-KIN-KOHI1-D
                                             MAX-KIN-KOHI1-M
                        ELSE
                          COMPUTE  WK-GENGAKU-RITUX =
                                   100 - WK-GENGAKU-RITU
                          COMPUTE  MAX-KIN-KOHI1-D     =
                                   MAX-KIN-KOHI1-D     *
                                   WK-GENGAKU-RITUX    /
                                   100
                          COMPUTE  MAX-KIN-KOHI1-M     =
                                   MAX-KIN-KOHI1-M     *
                                   WK-GENGAKU-RITUX    /
                                   100
                        END-IF
                     WHEN          2
                        CONTINUE
                     WHEN          3
                     WHEN          4
                        MOVE   ZERO   TO   MAX-KIN-KOHI1-D
                                           MAX-KIN-KOHI1-M
                   END-EVALUATE
                   COMPUTE  MAX-KIN-KOHI1-D =
                            MAX-KIN-KOHI1-D +  5
                   COMPUTE  MAX-KIN-KOHI1-D =
                            MAX-KIN-KOHI1-D /  10
                   COMPUTE  MAX-KIN-KOHI1-D =
                            MAX-KIN-KOHI1-D *  10
                   COMPUTE  MAX-KIN-KOHI1-M =
                            MAX-KIN-KOHI1-M +  5
                   COMPUTE  MAX-KIN-KOHI1-M =
                            MAX-KIN-KOHI1-M /  10
                   COMPUTE  MAX-KIN-KOHI1-M =
                            MAX-KIN-KOHI1-M *  10
                 END-IF
              ELSE
                 IF   MAX-KIN-SYU-M     NOT =  ZERO
                   EVALUATE   WK-GENGAKUKBN
                     WHEN          1
                        IF  WK-GENGAKU-RITU = 100
                          MOVE   ZERO   TO   MAX-KIN-SYU-M
                        ELSE
                          COMPUTE  WK-GENGAKU-RITUX =
                                   100 - WK-GENGAKU-RITU
                          COMPUTE  MAX-KIN-SYU-M       =
                                   MAX-KIN-SYU-M       *
                                   WK-GENGAKU-RITUX    /
                                   100
                        END-IF
                     WHEN          2
                        CONTINUE
                     WHEN          3
                     WHEN          4
                        MOVE   ZERO   TO   MAX-KIN-SYU-M
                   END-EVALUATE
                   COMPUTE  MAX-KIN-SYU-M =
                            MAX-KIN-SYU-M +  5
                   COMPUTE  MAX-KIN-SYU-M =
                            MAX-KIN-SYU-M /  10
                   COMPUTE  MAX-KIN-SYU-M =
                            MAX-KIN-SYU-M *  10
                 END-IF
              END-IF
      *
           END-IF
      *
           IF  FLG-GENBUTU70   NOT =   ZERO
      *
             IF   MAX-KIN-SYU-M     NOT =  ZERO
               IF  FLG-IRYOHI70  = ZERO
                  EVALUATE   WK-GENGAKUKBN
                    WHEN          1
                       IF  WK-GENGAKU-RITU = 100
                         MOVE   ZERO   TO   MAX-KIN-SYU-M
                       ELSE
                         COMPUTE  WK-GENGAKU-RITUX =
                                  100 - WK-GENGAKU-RITU
                         COMPUTE  MAX-KIN-SYU-M       =
                                  MAX-KIN-SYU-M       *
                                  WK-GENGAKU-RITUX    /
                                  100
                       END-IF
                    WHEN          2
                       CONTINUE
                    WHEN          3
                    WHEN          4
                       MOVE   ZERO   TO   MAX-KIN-SYU-M
                  END-EVALUATE
                  COMPUTE  MAX-KIN-SYU-M =
                           MAX-KIN-SYU-M +  5
                  COMPUTE  MAX-KIN-SYU-M =
                           MAX-KIN-SYU-M /  10
                  COMPUTE  MAX-KIN-SYU-M =
                           MAX-KIN-SYU-M *  10
               END-IF
             END-IF
      *
           END-IF
      *
           .
      *
       2109-KOHFTN-KINGAKU-GEN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ´µ¼Ô¸øÈñÉéÃ´¡Ê¹â³Û°ÑÇ¤Ê§¤¤¶â³Û¼è¤ê¹þ¤ß¡Ë¼è¹þ¤ß½èÍý
      ******************************************************************
       2110-KOHININBARAI-KINGAKU-SEC           SECTION.
      *
      *    ¸øÈñ·î¾å¸Â³Û¼èÆÀ
           INITIALIZE                    LNK-ORCSJGNGET-REC
           MOVE  LNK-SYU-HOSPID      TO  LNK-ORCSJGNGET-HOSPID
           MOVE  LNK-SYU-NYUGAIKBN   TO  LNK-ORCSJGNGET-NYUGAIKBN
           MOVE  LNK-SYU-PTID        TO  LNK-ORCSJGNGET-PTID
           MOVE    "964"             TO  LNK-ORCSJGNGET-KOHHKNNUM
           MOVE  WK-KOHININBARAI-KOHID
                                     TO  LNK-ORCSJGNGET-KOHID
           MOVE  LNK-SYU-SRYYMD(1:6) TO  LNK-ORCSJGNGET-SRYYM
      *
           CALL    "ORCSJGNGET"      USING
                                     LNK-ORCSJGNGET-REC
      *
           IF    LNK-ORCSJGNGET-RC   =   ZERO
              IF      LNK-ORCSJGNGET-GAIJGNGAK  =  ZERO
                 MOVE    ZERO        TO  FLG-KOHININBARAI
              ELSE
                 IF   COMB-KOH1HKNNUM      =  "027"
                   MOVE    "1"       TO  COMB-KOH1-GAIJGNKBN
                   MOVE    LNK-ORCSJGNGET-GAIJGNGAK
                                     TO  MAX-KIN-KOHI1-D
                                         MAX-KIN-KOHI1-M
                 ELSE
                   MOVE    LNK-ORCSJGNGET-GAIJGNGAK
                                     TO  MAX-KIN-SYU-M
                 END-IF
                 MOVE    1           TO  FLG-KOUGAKUCYO-ININ
              END-IF
           ELSE
                 MOVE    ZERO        TO  FLG-KOHININBARAI
           END-IF
      *
           .
      *
       2110-KOHININBARAI-KINGAKU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¿ÇÎÅ·îÆâ¤Î¼ýÇ¼¥Þ¥¹¥¿¤«¤é¸øÈñËè¤Î»ÈÍÑ²ó¿ô¤È´µ¼ÔÉéÃ´¶â³Û¤Î
      *    Îß·×¤òÄ´ºº
      ******************************************************************
       220-SYUNOU-SEC           SECTION.
      *
           MOVE     ZERO        TO    FLG-NYUGAI.
           MOVE     ZERO        TO    FLG-TOKYO.
           MOVE     ZERO        TO    FLG-GEN70-TOKUSYU.
      *
           INITIALIZE                 WK-TAIHI-DENPNUMXX-OCC.
           MOVE     ZERO        TO    FLG-GRPNYURYOKU.
           MOVE     ZERO        TO    WK-CYOKI.
           MOVE     ZERO        TO    WK-CYOKI-ENTANI.
      *
           MOVE     ZERO        TO    WK-OITA-HKNFTNMONEY.
           MOVE     ZERO        TO    WK-OITA-CYOFTN.
      *
           MOVE     ZERO        TO    FLG-SYUKEI2.
      *
           IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1")
           AND (WK-1001-PREFNUM              =   13 )
           AND (LNK-SYU-SRYYMD(1:6)      >= "200610")
              IF  (LNK-SYU-SYUHKNNUM    NOT = SPACE          )
              AND (LNK-SYU-KOH1HKNNUM       = "027"          )
              AND (LNK-SYU-KOH2HKNNUM       = "182"          )
              AND (LNK-SYU-KOH3HKNNUM   NOT = "972" AND "974")
              AND (LNK-SYU-KOH4HKNNUM   NOT = "972" AND "974")
                  MOVE     2      TO   FLG-TOKYO
              END-IF
              IF  (LNK-SYU-SYUHKNNUM    NOT = SPACE          )
              AND (LNK-SYU-KOH1HKNNUM       = "182"          )
              AND (LNK-SYU-KOH2HKNNUM   NOT = "972" AND "974")
              AND (LNK-SYU-KOH3HKNNUM   NOT = "972" AND "974")
              AND (LNK-SYU-KOH4HKNNUM   NOT = "972" AND "974")
                  MOVE     1      TO   FLG-TOKYO
              END-IF
           END-IF.
           IF   FLG-GENBUTU70         NOT =  ZERO
              IF   WK-1001-PREFNUM        =   13
                IF  (LNK-SYU-SYUHKNNUM    NOT = SPACE          )
                AND (LNK-SYU-KOH1HKNNUM       = "182"          )
                AND (LNK-SYU-KOH2HKNNUM   NOT = "972" AND "974")
                AND (LNK-SYU-KOH3HKNNUM   NOT = "972" AND "974")
                AND (LNK-SYU-KOH4HKNNUM   NOT = "972" AND "974")
                  MOVE     1      TO   FLG-TOKYO
                END-IF
              END-IF
              IF   WK-1001-PREFNUM        =   07
                IF  (LNK-SYU-SYUHKNNUM    NOT = SPACE          )
                AND (LNK-SYU-KOH1HKNNUM       = "245" OR  "246")
                AND (LNK-SYU-KOH2HKNNUM   NOT = "972" AND "974")
                AND (LNK-SYU-KOH3HKNNUM   NOT = "972" AND "974")
                AND (LNK-SYU-KOH4HKNNUM   NOT = "972" AND "974")
                  MOVE     1      TO   FLG-GEN70-TOKUSYU
                END-IF
              END-IF
              IF   WK-1001-PREFNUM        =   11
                IF  (LNK-SYU-SYUHKNNUM    NOT = SPACE          )
                AND (LNK-SYU-KOH1HKNNUM       = "342" OR  "343"
                                             OR "181" OR  "182"
                                             OR "183" OR  "281")
                AND (LNK-SYU-KOH2HKNNUM   NOT = "972" AND "974")
                AND (LNK-SYU-KOH3HKNNUM   NOT = "972" AND "974")
                AND (LNK-SYU-KOH4HKNNUM   NOT = "972" AND "974")
                  MOVE     1      TO   FLG-GEN70-TOKUSYU
                END-IF
                IF  (LNK-SYU-SYUHKNNUM    NOT = SPACE          )
                AND (LNK-SYU-KOH1HKNNUM       = "344"          )
                AND (LNK-SYU-KOH2HKNNUM   NOT = "972" AND "974")
                AND (LNK-SYU-KOH3HKNNUM   NOT = "972" AND "974")
                AND (LNK-SYU-KOH4HKNNUM   NOT = "972" AND "974")
                AND (FLG-SANSHOU2             =  "2"  OR   "3" )
                  MOVE     1      TO   FLG-GEN70-TOKUSYU
                END-IF
              END-IF
           END-IF.
      *
           IF  (COMB-KOH1-GAIJGNKBN        =       "1")      OR
               (COMB-KOH2-GAIJGNKBN        =       "1")      OR
               (COMB-KOH3-GAIJGNKBN        =       "1")      OR
               (COMB-KOH4-GAIJGNKBN        =       "1")      OR
               (LNK-S01-KOUREI-ZAITAKU-KBN =       "1")      OR
               (FLG-KOUREI-KBN             =       "1")      OR
               (FLG-KOHININBARAI           =        1 )      OR
               (FLG-GENBUTU70          NOT =      ZERO)
      *
               IF   WK-1001-PREFNUM       =     32
               AND  LNK-SYU-SRYYMD(1:6)   <    "200510"
                    IF    LNK-SYU-KOH1HKNNUM = "190" OR "191"
                        MOVE     1    TO     FLG-NYUGAI
                    END-IF
                    IF    LNK-SYU-KOH2HKNNUM = "190" OR "191"
                        MOVE     2    TO     FLG-NYUGAI
                    END-IF
                    IF    LNK-SYU-KOH3HKNNUM = "190" OR "191"
                        MOVE     3    TO     FLG-NYUGAI
                    END-IF
                    IF    LNK-SYU-KOH4HKNNUM = "190" OR "191"
                        MOVE     4    TO     FLG-NYUGAI
                    END-IF
               END-IF
               IF   LNK-SYU-SRYYMD(1:6)   >=   "200604"
                    IF   (LNK-SYU-KOH1HKNNUM  = "015" OR "016" OR "021")
                    AND  (WK-JIRITU-CYUKANSOU(1)   =  2                )
                        MOVE     1    TO     FLG-NYUGAI
                        MOVE  WK-JIRITU-TAFTNRUIKEI(1) TO FTN-KOHI1-M
                                                          FTN-KOHI1-M2
                                                          FTN-KOHI1-M3
                    END-IF
                    IF   (LNK-SYU-KOH2HKNNUM  = "015" OR "016" OR "021")
                    AND  (WK-JIRITU-CYUKANSOU(2)   =  2                )
                        MOVE     2    TO     FLG-NYUGAI
                        MOVE  WK-JIRITU-TAFTNRUIKEI(2) TO FTN-KOHI2-M
                                                          FTN-KOHI2-M2
                                                          FTN-KOHI2-M3
                    END-IF
               END-IF
               IF   LNK-SYU-SRYYMD(1:6)   >=   "200610"
                    IF   LNK-SYU-KOH1HKNNUM  = "024" OR "079"
                        MOVE     1    TO     FLG-NYUGAI
                    END-IF
                    IF   LNK-SYU-KOH2HKNNUM  = "024" OR "079"
                        MOVE     2    TO     FLG-NYUGAI
                    END-IF
               END-IF
               IF   WK-1001-PREFNUM       =    07
               AND  LNK-SYU-SRYYMD(1:6)  >=   "200702"
                    IF   LNK-SYU-KOH1HKNNUM  = "281"
                        MOVE     1    TO     FLG-NYUGAI
                        MOVE  WK-CHIHOU-TAFTNRUIKEI(1) TO FTN-KOHI1-M
                                                          FTN-KOHI1-M2
                                                          FTN-KOHI1-M3
                    END-IF
                    IF   LNK-SYU-KOH2HKNNUM  = "281"
                        MOVE     2    TO     FLG-NYUGAI
                        MOVE  WK-CHIHOU-TAFTNRUIKEI(2) TO FTN-KOHI2-M
                                                          FTN-KOHI2-M2
                                                          FTN-KOHI2-M3
                    END-IF
                    IF   LNK-SYU-KOH3HKNNUM  = "281"
                        MOVE     3    TO     FLG-NYUGAI
                        MOVE  WK-CHIHOU-TAFTNRUIKEI(3) TO FTN-KOHI3-M
                                                          FTN-KOHI3-M2
                                                          FTN-KOHI3-M3
                    END-IF
                    IF   LNK-SYU-KOH4HKNNUM  = "281"
                        MOVE     4    TO     FLG-NYUGAI
                        MOVE  WK-CHIHOU-TAFTNRUIKEI(4) TO FTN-KOHI4-M
                                                          FTN-KOHI4-M2
                                                          FTN-KOHI4-M3
                    END-IF
               END-IF
      *
               IF    FLG-NYUGAI    NOT =   ZERO
                 INITIALIZE                         SYU-SYUNOU-REC
                 MOVE    LNK-SYU-HOSPID       TO    SYU-HOSPID
                 MOVE    LNK-SYU-PTID         TO    SYU-PTID
                 MOVE    LNK-SYU-SRYYMD(1:6)  TO    SYU-SRYYMD(1:6)
                 MOVE    "__"                 TO    SYU-SRYYMD(7:2)
                 MOVE    SYU-SYUNOU-REC       TO    MCPDATA-REC
                 MOVE    "DBSELECT"           TO    MCP-FUNC
                 MOVE    "SYUNOU-KEY30"       TO    ORC-DBPATH
                 CALL    "ORCMCPSUB"          USING   MCPAREA
                                                      ORCMCPAREA
                                                      MCPDATA-REC
                 IF      MCP-RC             =       ZERO
                     MOVE    "SYUNOU-KEY30" TO      ORC-DBPATH
                     PERFORM  SYUNOU-READ-SEC
                     PERFORM  UNTIL  (FLG-SYUNOU   =    "1")
                       IF   SYU-DENPJTIKBN  NOT =  "3"
                               PERFORM     2202-SYUNOU-COUNT-SEC
                       END-IF
                       MOVE     "SYUNOU-KEY30"   TO   ORC-DBPATH
                       PERFORM   SYUNOU-READ-SEC
                     END-PERFORM
                     MOVE        LNK-SYU-SRYYMD(7:2)   TO   IDX2X
                     MOVE        KOHI1-TBL(IDX2) TO    KAISU-KOHI1-D
                     MOVE        KOHI2-TBL(IDX2) TO    KAISU-KOHI2-D
                     MOVE        KOHI3-TBL(IDX2) TO    KAISU-KOHI3-D
                     MOVE        KOHI4-TBL(IDX2) TO    KAISU-KOHI4-D
                     MOVE        KOHI1-TBL(32)   TO    KAISU-KOHI1-M
                     MOVE        KOHI2-TBL(32)   TO    KAISU-KOHI2-M
                     MOVE        KOHI3-TBL(32)   TO    KAISU-KOHI3-M
                     MOVE        KOHI4-TBL(32)   TO    KAISU-KOHI4-M
                 ELSE
                     MOVE    12              TO        LNK-S01-RC
                 END-IF
      *
                 MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
                 MOVE    "SYUNOU-KEY30"          TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"              USING
                                                  MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
               ELSE
                 INITIALIZE                         SYU-SYUNOU-REC
                 MOVE    LNK-SYU-HOSPID       TO    SYU-HOSPID
                 MOVE    LNK-SYU-NYUGAIKBN    TO    SYU-NYUGAIKBN
                 MOVE    LNK-SYU-PTID         TO    SYU-PTID
                 MOVE    LNK-SYU-SRYYMD(1:6)  TO    SYU-SRYYMD(1:6)
                 MOVE    "__"                 TO    SYU-SRYYMD(7:2)
                 MOVE    SYU-SYUNOU-REC       TO    MCPDATA-REC
                 MOVE    "DBSELECT"           TO    MCP-FUNC
                 MOVE    "SYUNOU-KEY26"       TO    ORC-DBPATH
                 CALL    "ORCMCPSUB"          USING   MCPAREA
                                                      ORCMCPAREA
                                                      MCPDATA-REC
                 IF      MCP-RC             =       ZERO
                     MOVE    "SYUNOU-KEY26" TO      ORC-DBPATH
                     PERFORM  SYUNOU-READ-SEC
                     PERFORM  UNTIL  (FLG-SYUNOU   =    "1")
                       IF   SYU-DENPJTIKBN  NOT =  "3"
                               PERFORM     2202-SYUNOU-COUNT-SEC
                       END-IF
                       MOVE     "SYUNOU-KEY26"   TO   ORC-DBPATH
                       PERFORM   SYUNOU-READ-SEC
                     END-PERFORM
                     MOVE        LNK-SYU-SRYYMD(7:2)   TO   IDX2X
                     MOVE        KOHI1-TBL(IDX2) TO    KAISU-KOHI1-D
                     MOVE        KOHI2-TBL(IDX2) TO    KAISU-KOHI2-D
                     MOVE        KOHI3-TBL(IDX2) TO    KAISU-KOHI3-D
                     MOVE        KOHI4-TBL(IDX2) TO    KAISU-KOHI4-D
                     MOVE        KOHI1-TBL(32)   TO    KAISU-KOHI1-M
                     MOVE        KOHI2-TBL(32)   TO    KAISU-KOHI2-M
                     MOVE        KOHI3-TBL(32)   TO    KAISU-KOHI3-M
                     MOVE        KOHI4-TBL(32)   TO    KAISU-KOHI4-M
                 ELSE
                     MOVE    12              TO        LNK-S01-RC
                 END-IF
      *
                 MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
                 MOVE    "SYUNOU-KEY26"          TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"              USING
                                                  MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
               END-IF
      *
               PERFORM    VARYING   IDZ    FROM   1   BY   1
                           UNTIL  (IDZ    >    15)
                             OR   (LNK-WRK-SYU-SRYYMD(IDZ) =  SPACE)
      *
                      MOVE    1              TO  FLG-GRPNYURYOKU
      *
                      MOVE    LNK-WRK-SYUNOU-REC(IDZ)
                                             TO  SYU-SYUNOU-REC
                      PERFORM        2202-SYUNOU-COUNT-SEC
      *
               END-PERFORM
      *
           END-IF.
      *
           IF     FLG-GENBUTU70   NOT =   ZERO
              IF     FLG-IRYOHI70     =  1 OR 3
                IF  WK-KOUREI-IRYOHI70  >  500000
                   COMPUTE WK-KOUREI-IRYOHI70X =
                        (( WK-KOUREI-IRYOHI70 - 500000 ) * 0.01 ) + 0.5
                   COMPUTE  MAX-KIN-SYU-M    =   MAX-KIN-SYU-M 
                                             +   WK-KOUREI-IRYOHI70X
                   MOVE    1    TO    FLG-KOUGAKUCYO-GEN70
                END-IF
              END-IF
              IF     FLG-IRYOHI70     =  2 OR 4 OR 6
                IF  WK-KOUREI-IRYOHI70  >  267000
                   COMPUTE WK-KOUREI-IRYOHI70X =
                        (( WK-KOUREI-IRYOHI70 - 267000 ) * 0.01 ) + 0.5
                   COMPUTE  MAX-KIN-SYU-M    =   MAX-KIN-SYU-M 
                                             +   WK-KOUREI-IRYOHI70X
                   MOVE    1    TO    FLG-KOUGAKUCYO-GEN70
                END-IF
              END-IF
              IF     FLG-IRYOHI70   NOT =  ZERO
                IF   WK-GENGAKUKBN  NOT =  ZERO
                     PERFORM   2203-KOHFTN-KINGAKU-GEN2-SEC
                END-IF
              END-IF
      *       ¼«Î©»Ù±çÅùÍÑ
              IF     FLG-IRYOHI70K    =  1
                IF  WK-KOUREI-IRYOHI70K >  267000
                   COMPUTE WK-KOUREI-IRYOHI70X =
                        (( WK-KOUREI-IRYOHI70K - 267000 ) * 0.01 ) + 0.5
                   COMPUTE  MAX-KIN-KOHI1-M  =   MAX-KIN-KOHI1-M
                                             +   WK-KOUREI-IRYOHI70X
                   IF   WK-KOUREI-IRYOHI70K   >  1935690
                      MOVE    1    TO    FLG-KOUGAKUCYO-GEN70K
                   END-IF
                END-IF
              END-IF
              IF     FLG-IRYOHI70K    =  2
                IF  WK-KOUREI-IRYOHI70K >  267000
                   COMPUTE WK-KOUREI-IRYOHI70X =
                        (( WK-KOUREI-IRYOHI70K - 267000 ) * 0.01 ) + 0.5
                   COMPUTE  MAX-KIN-KOHI1-M  =   MAX-KIN-KOHI1-M
                                             +   WK-KOUREI-IRYOHI70X
                   IF   WK-KOUREI-IRYOHI70K   >   860340
                      MOVE    1    TO    FLG-KOUGAKUCYO-GEN70K
                   END-IF
                END-IF
              END-IF
      *       £´£±Ï·¿ÍÍÑ
              IF     FLG-IRYOHI70K    =  11
                IF  WK-KOUREI-IRYOHI70K >  801000
                   COMPUTE WK-KOUREI-IRYOHI70X =
                        (( WK-KOUREI-IRYOHI70K - 267000 ) * 0.01 ) + 0.5
                   COMPUTE  MAX-KIN-KOHI1-M  =   MAX-KIN-KOHI1-M
                                             +   WK-KOUREI-IRYOHI70X
                   IF   WK-KOUREI-IRYOHI70K   >   860340
                      MOVE    1    TO    FLG-KOUGAKUCYO-GEN70K
                   END-IF
                END-IF
              END-IF
              IF     FLG-IRYOHI70K    =  12
                IF  WK-KOUREI-IRYOHI70K >  267000
                   COMPUTE WK-KOUREI-IRYOHI70X =
                        (( WK-KOUREI-IRYOHI70K - 267000 ) * 0.01 ) + 0.5
                   COMPUTE  MAX-KIN-KOHI1-M  =   MAX-KIN-KOHI1-M
                                             +   WK-KOUREI-IRYOHI70X
                   MOVE    1    TO    FLG-KOUGAKUCYO-GEN70K
                END-IF
              END-IF
           ELSE
      *       ¼«Î©»Ù±çÍÑ
              IF    LNK-SYU-SRYYMD(1:6)  >= "200610"
                IF     FLG-IRYOHI      =      1
                  IF  WK-IRYOHI  >  267000
                     COMPUTE WK-IRYOHI2 = 
                           (( WK-IRYOHI  -  267000 ) * 0.01 ) + 0.5
                     COMPUTE  MAX-KIN-KOHI1-M  =   MAX-KIN-KOHI1-M 
                                               +   WK-IRYOHI2
                     IF   WK-IRYOHI   >   860340
                        MOVE    1    TO    FLG-KOUGAKUCYO-JIRITU
                     END-IF
                  END-IF
                END-IF
                IF     FLG-IRYOHI2     =      1
                  IF  WK-IRYOHI  >  801000
                     COMPUTE WK-IRYOHI2 = 
                           (( WK-IRYOHI  -  267000 ) * 0.01 ) + 0.5
                     COMPUTE  MAX-KIN-KOHI1-M  =   MAX-KIN-KOHI1-M 
                                               +   WK-IRYOHI2
                     IF   WK-IRYOHI   >   860340
                        MOVE    1    TO    FLG-KOUGAKUCYO2
                     END-IF
                  END-IF
                END-IF
                IF     FLG-IRYOHI2     =      2
                  IF  WK-IRYOHI  >  267000
                     COMPUTE WK-IRYOHI2 = 
                           (( WK-IRYOHI  -  267000 ) * 0.01 ) + 0.5
                     COMPUTE  MAX-KIN-KOHI1-M  =   MAX-KIN-KOHI1-M 
                                               +   WK-IRYOHI2
                     MOVE    1    TO    FLG-KOUGAKUCYO2
                  END-IF
                END-IF
              ELSE
                IF     FLG-IRYOHI      =      1
                  IF  WK-IRYOHI  >  241000
                     COMPUTE WK-IRYOHI2 = 
                           (( WK-IRYOHI  -  241000 ) * 0.01 ) + 0.5
                     COMPUTE  MAX-KIN-KOHI1-M  =   MAX-KIN-KOHI1-M 
                                               +   WK-IRYOHI2
                     IF   WK-IRYOHI   >   776540
                        MOVE    1    TO    FLG-KOUGAKUCYO-JIRITU
                     END-IF
                  END-IF
                END-IF
              END-IF
           END-IF.
      *
       220-SYUNOU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¿ÇÎÅ·î¤Î¼ýÇ¼¥Þ¥¹¥¿¤«¤é¸øÈñ¤Î²ó¿ô¡Ê¶â³Û¡Ë¥«¥¦¥ó¥È
      ******************************************************************
       2202-SYUNOU-COUNT-SEC          SECTION.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¤Î¸¡º÷
           INITIALIZE                         WRK-HKNCOMBI-REC.
           MOVE    SYU-HOSPID         TO      WRK-COMB-HOSPID.
           MOVE    SYU-PTID           TO      WRK-COMB-PTID.
           MOVE    SYU-HKNCOMBINUM    TO      WRK-COMB-HKNCOMBINUM.
           MOVE    WRK-HKNCOMBI-REC   TO      MCPDATA-REC.
           MOVE    "DBSELECT"         TO      MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"     TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING   MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC.
           IF      MCP-RC               =       ZERO
               MOVE    "HKNCOMBI-KEY" TO      ORC-DBPATH 
               PERFORM   HKNCOMBI-READ2-SEC
               IF      FLG-HKNCOMBI     =       1
                   DISPLAY "HKNCOMBI DBFETCH ERR"
                   MOVE     10          TO      LNK-S01-RC
                   MOVE    "DBCLOSECURSOR"  TO      MCP-FUNC
                   MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
                   CALL    "ORCMCPSUB"      USING   MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                   GO   TO              2202-SYUNOU-COUNT-EXT
               END-IF
           ELSE
               MOVE    1                TO      FLG-HKNCOMBI
               DISPLAY "HKNCOMBI DBSELECT ERR"
               MOVE     10              TO      LNK-S01-RC
               MOVE    "DBCLOSECURSOR"  TO      MCP-FUNC
               MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
               CALL    "ORCMCPSUB"      USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               GO   TO                  2202-SYUNOU-COUNT-EXT
           END-IF.
           MOVE    "DBCLOSECURSOR"      TO      MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"       TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC.
      *
      *    ÆÃÄê¼À´µ
           IF    LNK-SYU-SRYYMD(1:6)         >= "200310"
      *
              IF     SYU-KOH1HKNNUM  =  "051" 
                IF   LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH1ID    TO  WRK-COMB-KOH1ID
                END-IF
                IF   LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH2ID    TO  WRK-COMB-KOH1ID
                END-IF
              END-IF
              IF     SYU-KOH2HKNNUM  =  "051" 
                IF   LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM
                   MOVE   COMB-KOH1ID    TO  WRK-COMB-KOH2ID
                END-IF
                IF   LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM
                   MOVE   COMB-KOH2ID    TO  WRK-COMB-KOH2ID
                END-IF
              END-IF
      *
           END-IF.
      *
      *    ¾®»ùÆÃÄê
           IF    LNK-SYU-SRYYMD(1:6)         >= "200504"
      *
              IF     SYU-KOH1HKNNUM  =  "052"
                IF   LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH1ID    TO  WRK-COMB-KOH1ID
                END-IF
                IF   LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH2ID    TO  WRK-COMB-KOH1ID
                END-IF
              END-IF
              IF     SYU-KOH2HKNNUM  =  "052"
                IF   LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM
                   MOVE   COMB-KOH1ID    TO  WRK-COMB-KOH2ID
                END-IF
                IF   LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM
                   MOVE   COMB-KOH2ID    TO  WRK-COMB-KOH2ID
                END-IF
              END-IF
      *
           END-IF.
      *
      *    °ñ¾ë¸©¡Ê£±£´£³¡¢£±£´£µ¡Ë
           IF   WK-1001-PREFNUM          =  08
              IF     SYU-KOH1HKNNUM  =  "143" OR "145"
                                     OR "181" OR "182" OR "189"
                                     OR "187" OR "188"
                IF   LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH1ID    TO  WRK-COMB-KOH1ID
                END-IF
                IF   LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH2ID    TO  WRK-COMB-KOH1ID
                END-IF
                IF   LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH3ID    TO  WRK-COMB-KOH1ID
                END-IF
                IF   LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH4ID    TO  WRK-COMB-KOH1ID
                END-IF
              END-IF
              IF     SYU-KOH2HKNNUM  =  "143" OR "145"
                                     OR "181" OR "182" OR "189"
                                     OR "187" OR "188"
                IF   LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM
                   MOVE   COMB-KOH1ID    TO  WRK-COMB-KOH2ID
                END-IF
                IF   LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM
                   MOVE   COMB-KOH2ID    TO  WRK-COMB-KOH2ID
                END-IF
                IF   LNK-SYU-KOH3HKNNUM  =  SYU-KOH2HKNNUM
                   MOVE   COMB-KOH3ID    TO  WRK-COMB-KOH2ID
                END-IF
                IF   LNK-SYU-KOH4HKNNUM  =  SYU-KOH2HKNNUM
                   MOVE   COMB-KOH4ID    TO  WRK-COMB-KOH2ID
                END-IF
              END-IF
              IF     SYU-KOH3HKNNUM  =  "143" OR "145"
                                     OR "181" OR "182" OR "189"
                                     OR "187" OR "188"
                IF   LNK-SYU-KOH1HKNNUM  =  SYU-KOH3HKNNUM
                   MOVE   COMB-KOH1ID    TO  WRK-COMB-KOH3ID
                END-IF
                IF   LNK-SYU-KOH2HKNNUM  =  SYU-KOH3HKNNUM
                   MOVE   COMB-KOH2ID    TO  WRK-COMB-KOH3ID
                END-IF
                IF   LNK-SYU-KOH3HKNNUM  =  SYU-KOH3HKNNUM
                   MOVE   COMB-KOH3ID    TO  WRK-COMB-KOH3ID
                END-IF
                IF   LNK-SYU-KOH4HKNNUM  =  SYU-KOH3HKNNUM
                   MOVE   COMB-KOH4ID    TO  WRK-COMB-KOH3ID
                END-IF
              END-IF
              IF     SYU-KOH4HKNNUM  =  "143" OR "145"
                                     OR "181" OR "182" OR "189"
                                     OR "187" OR "188"
                IF   LNK-SYU-KOH1HKNNUM  =  SYU-KOH4HKNNUM
                   MOVE   COMB-KOH1ID    TO  WRK-COMB-KOH4ID
                END-IF
                IF   LNK-SYU-KOH2HKNNUM  =  SYU-KOH4HKNNUM
                   MOVE   COMB-KOH2ID    TO  WRK-COMB-KOH4ID
                END-IF
                IF   LNK-SYU-KOH3HKNNUM  =  SYU-KOH4HKNNUM
                   MOVE   COMB-KOH3ID    TO  WRK-COMB-KOH4ID
                END-IF
                IF   LNK-SYU-KOH4HKNNUM  =  SYU-KOH4HKNNUM
                   MOVE   COMB-KOH4ID    TO  WRK-COMB-KOH4ID
                END-IF
              END-IF
           END-IF.
      *
      *    ¼«Î©»Ù±ç¡ÊÀº¿ÀÄÌ±¡¡¢¹¹À¸¡¢°éÀ®¡Ë
           IF    LNK-SYU-SRYYMD(1:6)         >= "200604"
      *
              MOVE  LNK-SYU-SRYYMD(1:6)  TO  WK-JIRITU-SRYYMD(1:6)
              MOVE  "01"                 TO  WK-JIRITU-SRYYMD(7:2)
      *
              IF     SYU-KOH1HKNNUM  =  "015" OR "016" OR "021"
                IF   (LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM  )
                AND  (COMB-KOH1ID     NOT =  WRK-COMB-KOH1ID )
                AND  (WK-JIRITU-CYUKANSOU(1)   =  2          )
                AND  (SYU-NYNSHOKAISU          =  ZERO       )
                AND  (COMB-TEKSTYMD      <=  WK-JIRITU-SRYYMD)
                AND  (WRK-COMB-TEKSTYMD  <=  WK-JIRITU-SRYYMD)
                   MOVE   COMB-KOH1ID    TO  WRK-COMB-KOH1ID
                END-IF
                IF   (LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM  )
                AND  (COMB-KOH2ID     NOT =  WRK-COMB-KOH1ID )
                AND  (WK-JIRITU-CYUKANSOU(2)   =  2          )
                AND  (SYU-NYNSHOKAISU          =  ZERO       )
                AND  (COMB-TEKSTYMD      <=  WK-JIRITU-SRYYMD)
                AND  (WRK-COMB-TEKSTYMD  <=  WK-JIRITU-SRYYMD)
                   MOVE   COMB-KOH2ID    TO  WRK-COMB-KOH1ID
                END-IF
              END-IF
              IF     SYU-KOH2HKNNUM  =  "015" OR "016" OR "021"
                IF   (LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM  )
                AND  (COMB-KOH1ID     NOT =  WRK-COMB-KOH2ID )
                AND  (WK-JIRITU-CYUKANSOU(1)   =  2          )
                AND  (SYU-NYNSHOKAISU          =  ZERO       )
                AND  (COMB-TEKSTYMD      <=  WK-JIRITU-SRYYMD)
                AND  (WRK-COMB-TEKSTYMD  <=  WK-JIRITU-SRYYMD)
                   MOVE   COMB-KOH1ID    TO  WRK-COMB-KOH2ID
                END-IF
                IF   (LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM  )
                AND  (COMB-KOH2ID     NOT =  WRK-COMB-KOH2ID )
                AND  (WK-JIRITU-CYUKANSOU(2)   =  2          )
                AND  (SYU-NYNSHOKAISU          =  ZERO       )
                AND  (COMB-TEKSTYMD      <=  WK-JIRITU-SRYYMD)
                AND  (WRK-COMB-TEKSTYMD  <=  WK-JIRITU-SRYYMD)
                   MOVE   COMB-KOH2ID    TO  WRK-COMB-KOH2ID
                END-IF
              END-IF
      *
           END-IF.
      *
           IF  (WK-1001-PREFNUM          =  44      ) AND
               (LNK-SYU-SRYYMD(1:6)     >= "200610" )
              IF  ((LNK-SYU-KOH1HKNNUM  = "183" OR "283") OR
                   (LNK-SYU-KOH2HKNNUM  = "183" OR "283") OR
                   (LNK-SYU-KOH3HKNNUM  = "183" OR "283") OR
                   (LNK-SYU-KOH4HKNNUM  = "183" OR "283"))
              AND ((SYU-KOH1HKNNUM      = "183" OR "283") OR
                   (SYU-KOH2HKNNUM      = "183" OR "283") OR
                   (SYU-KOH3HKNNUM      = "183" OR "283") OR
                   (SYU-KOH4HKNNUM      = "183" OR "283"))
                COMPUTE   SYU-KOH-FTN         =  SYU-KOH-FTN
                                              -  SYU-JYO-COMPFTN
                COMPUTE   SYU-KOH-FTN-ENTANI  =  SYU-KOH-FTN-ENTANI
                                              -  SYU-JYO-COMPFTN
              END-IF
           END-IF.
      *
      *Ê¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ
      *¹âÎð¼Ô¤ÎºßÂðÁí¹ç¡¦ºßÂðËö´ü»»Äê´µ¼Ô
      *
           MOVE     ZERO     TO     FLG-ZAITAKU.                         
           MOVE     ZERO     TO     FLG-SYUKEI.
      *
           IF   SYU-NYUGAIKBN  =  "2"
             IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1")
             OR  (FLG-SYUHEN                   =   "1")
             OR  (FLG-KOHININBARAI             =    1 )
             OR  (FLG-GENBUTU70            NOT =  ZERO)
      *
               IF ( SYU-SYUHKNNUM       =  "971" OR "973" OR
                                           "068" OR "069" )
               OR ( SYU-SYUHKNNUM(1:2)  =  "98"           )
      *
                 CONTINUE
      *
               ELSE
      *
                 IF COMB-KOH1HKNNUM      =    "027"
      *               COMPUTE  WK-SYU-SYUCOMPFTN  =                     
      *                                 SYU-KOH1COMPFTN    +   5        
      *               COMPUTE  WK-SYU-SYUCOMPFTN  =                     
      *                                 WK-SYU-SYUCOMPFTN  /  10        
      *               COMPUTE  WK-SYU-SYUCOMPFTN  =                     
      *                                 WK-SYU-SYUCOMPFTN  *  10        
                      MOVE     SYU-KOH1COMPFTN  TO  WK-SYU-SYUCOMPFTN
      ***
                      IF   FLG-SYUHEN   =   "1"
                        IF  ((SYU-KOH2HKNNUM     NOT = SPACE)              
                        OR   (SYU-KOH3HKNNUM     NOT = SPACE)              
                        OR   (SYU-KOH4HKNNUM     NOT = SPACE))
                        AND ( SYU-KOH1HKNNUM         = "027" )             
                          IF   LNK-SYU-SRYYMD(7:2)   =  SYU-SRYYMD(7:2)
                            ADD   WK-SYU-SYUCOMPFTN TO  FTN-KOHI1-D-HEN  
                            COMPUTE WK-ENTANI2 =
                                         SYU-KOH1COMPFTN-ENTANI + 0.9
                            ADD   WK-ENTANI2 TO  FTN-KOHI1-D-HEN-ENTANI
                          END-IF                                          
                          ADD   WK-SYU-SYUCOMPFTN   TO  FTN-KOHI1-M-HEN  
                          COMPUTE WK-ENTANI2 =
                                         SYU-KOH1COMPFTN-ENTANI + 0.9
                          ADD   WK-ENTANI2   TO  FTN-KOHI1-M-HEN-ENTANI
                        END-IF
                      END-IF
      ***
                      IF    SYU-KOH1HKNNUM               =  "027"
                       IF   LNK-S01-KOUREI-ZAITAKU-KBN   =   "1"
                        IF  (SYU-KOH2HKNNUM     NOT = SPACE)              
                        OR  (SYU-KOH3HKNNUM     NOT = SPACE)              
                        OR  (SYU-KOH4HKNNUM     NOT = SPACE)              
                          MOVE     SYU-KOH-FTN  TO  WK-SYU-SYUCOMPFTN     
                        END-IF                                            
                        IF     LNK-SYU-SRYYMD(7:2)   =  SYU-SRYYMD(7:2)  
                          ADD   WK-SYU-SYUCOMPFTN TO   FTN-KOHI1-D      
                        END-IF                                          
                        ADD   WK-SYU-SYUCOMPFTN   TO   FTN-KOHI1-M      
      *
                        COMPUTE WK-ENTANI = SYU-KOH1COMPFTN-ENTANI + 0.9
                        IF  (SYU-KOH2HKNNUM     NOT = SPACE)
                        OR  (SYU-KOH3HKNNUM     NOT = SPACE)
                        OR  (SYU-KOH4HKNNUM     NOT = SPACE)
                          COMPUTE WK-ENTANI = SYU-KOH-FTN-ENTANI + 0.9
                        END-IF
                        IF     LNK-SYU-SRYYMD(7:2)   =  SYU-SRYYMD(7:2)
                          ADD   WK-ENTANI     TO   FTN-KOHI1-D-ENTANI
                        END-IF
                        ADD   WK-ENTANI       TO   FTN-KOHI1-M-ENTANI
                       END-IF
                       IF   FLG-KOHININBARAI  =    1
                        IF  (SYU-KOH2HKNNUM   =  SPACE)
                        AND (SYU-KOH3HKNNUM   =  SPACE)
                        AND (SYU-KOH4HKNNUM   =  SPACE)
                          IF  LNK-SYU-SRYYMD(7:2)  =  SYU-SRYYMD(7:2)
                            ADD   WK-SYU-SYUCOMPFTN TO   FTN-KOHI1-D
                          END-IF
                          ADD   WK-SYU-SYUCOMPFTN   TO   FTN-KOHI1-M
      *
                          COMPUTE WK-ENTANI =
                                  SYU-KOH1COMPFTN-ENTANI + 0.9
                          IF  LNK-SYU-SRYYMD(7:2)  =  SYU-SRYYMD(7:2)
                            ADD   WK-ENTANI     TO   FTN-KOHI1-D-ENTANI
                          END-IF
                          ADD   WK-ENTANI       TO   FTN-KOHI1-M-ENTANI
                        END-IF
                       END-IF
                       MOVE  1                     TO   FLG-ZAITAKU      
                      END-IF
                 ELSE                                                   
      *               COMPUTE  WK-SYU-SYUCOMPFTN  =                     
      *                                 SYU-SYUCOMPFTN     +   5        
      *               COMPUTE  WK-SYU-SYUCOMPFTN  =                     
      *                                 WK-SYU-SYUCOMPFTN  /  10        
      *               COMPUTE  WK-SYU-SYUCOMPFTN  =                     
      *                                 WK-SYU-SYUCOMPFTN  *  10        
                      MOVE     SYU-SYUCOMPFTN   TO  WK-SYU-SYUCOMPFTN
      ***
                      IF   FLG-SYUHEN   =   "1"
                        IF  ((SYU-KOH1HKNNUM     NOT = SPACE)              
                        OR   (SYU-KOH2HKNNUM     NOT = SPACE)              
                        OR   (SYU-KOH3HKNNUM     NOT = SPACE)              
                        OR   (SYU-KOH4HKNNUM     NOT = SPACE))
                        AND ( SYU-SYUHKNNUM      NOT = SPACE )
                          ADD   WK-SYU-SYUCOMPFTN   TO   FTN-SYU-M-HEN  
                          COMPUTE WK-ENTANI2 =
                                         SYU-SYUCOMPFTN-ENTANI + 0.9
                          ADD   WK-ENTANI2   TO   FTN-SYU-M-HEN-ENTANI
                        END-IF
                      END-IF
      ***
                      IF   (SYU-SYUHKNNUM      NOT = SPACE)
                      AND  (COMB-HKNNUM        NOT = SPACE)
                       IF   LNK-S01-KOUREI-ZAITAKU-KBN   =   "1"
                        IF  (SYU-KOH1HKNNUM     NOT = SPACE)              
                        OR  (SYU-KOH2HKNNUM     NOT = SPACE)              
                        OR  (SYU-KOH3HKNNUM     NOT = SPACE)              
                        OR  (SYU-KOH4HKNNUM     NOT = SPACE)              
                          MOVE     SYU-KOH-FTN  TO  WK-SYU-SYUCOMPFTN     
                        END-IF                                            
                        ADD   WK-SYU-SYUCOMPFTN TO  FTN-SYU-M           
      *
                        COMPUTE WK-ENTANI = SYU-SYUCOMPFTN-ENTANI + 0.9
                        IF  (SYU-KOH1HKNNUM     NOT = SPACE)
                        OR  (SYU-KOH2HKNNUM     NOT = SPACE)
                        OR  (SYU-KOH3HKNNUM     NOT = SPACE)
                        OR  (SYU-KOH4HKNNUM     NOT = SPACE)
                          COMPUTE WK-ENTANI = SYU-KOH-FTN-ENTANI + 0.9
                        END-IF
                        ADD   WK-ENTANI        TO  FTN-SYU-M-ENTANI
                       END-IF
                       IF   FLG-KOHININBARAI  =    1
                        IF  (SYU-KOH1HKNNUM   =  SPACE)
                        AND (SYU-KOH2HKNNUM   =  SPACE)
                        AND (SYU-KOH3HKNNUM   =  SPACE)
                        AND (SYU-KOH4HKNNUM   =  SPACE)
                          ADD   WK-SYU-SYUCOMPFTN TO  FTN-SYU-M
      *
                          COMPUTE WK-ENTANI =
                                  SYU-SYUCOMPFTN-ENTANI + 0.9
                          ADD   WK-ENTANI         TO  FTN-SYU-M-ENTANI
                        END-IF
                       END-IF
                       IF  FLG-GENBUTU70    NOT =  ZERO
                        IF  (SYU-KOH1HKNNUM     NOT = SPACE)
                        OR  (SYU-KOH2HKNNUM     NOT = SPACE)
                        OR  (SYU-KOH3HKNNUM     NOT = SPACE)
                        OR  (SYU-KOH4HKNNUM     NOT = SPACE)
                         MOVE  SYU-KOH-FTN      TO  WK-SYU-SYUCOMPFTN
                         COMPUTE WK-ENTANI = SYU-KOH-FTN-ENTANI + 0.9
                        ELSE
                         IF  (SYU-KOH1HKNNUM     =  SPACE)
                         AND (SYU-KOH2HKNNUM     =  SPACE)
                         AND (SYU-KOH3HKNNUM     =  SPACE)
                         AND (SYU-KOH4HKNNUM     =  SPACE)
                           COMPUTE WK-ENTANI =
                                          SYU-SYUCOMPFTN-ENTANI + 0.9
      ****                 IF  ( COMB-KOH1HKNNUM        =    SPACE )
      ****                 AND ( COMB-KOH2HKNNUM        =    SPACE )
      ****                 AND ( COMB-KOH3HKNNUM        =    SPACE )
      ****                 AND ( COMB-KOH4HKNNUM        =    SPACE )
      ****                 AND ( FLG-IRYOHI70           =    1 OR 2)
      ****                    COMPUTE   WK-KOUREI-IRYOHI70  =
      ****                              WK-KOUREI-IRYOHI70  +
      ****                             (SYU-HKNTEN (16) * 10)
      ****                 END-IF
                         END-IF
                        END-IF
                        ADD   WK-SYU-SYUCOMPFTN     TO   FTN-SYU-M
                        ADD   WK-ENTANI       TO   FTN-SYU-M-ENTANI
                        IF  ( COMB-KOH1HKNNUM        =    SPACE )
                        AND ( COMB-KOH2HKNNUM        =    SPACE )
                        AND ( COMB-KOH3HKNNUM        =    SPACE )
                        AND ( COMB-KOH4HKNNUM        =    SPACE )
                        AND ( FLG-IRYOHI70           =    1 OR 2)
                           COMPUTE   WK-KOUREI-IRYOHI70  =
                                     WK-KOUREI-IRYOHI70  +
                                    (SYU-HKNTEN (16) * 10)
                        END-IF
                       END-IF
                      END-IF
                 END-IF                                                 
      *
               END-IF
             END-IF
      *
             IF (((COMB-KOH1HKNNUM     = "972") OR
                  (COMB-KOH2HKNNUM     = "972") OR
                  (COMB-KOH3HKNNUM     = "972") OR
                  (COMB-KOH4HKNNUM     = "972")) AND
                 ((SYU-KOH1HKNNUM      = "972") OR
                  (SYU-KOH2HKNNUM      = "972") OR
                  (SYU-KOH3HKNNUM      = "972") OR
                  (SYU-KOH4HKNNUM      = "972")))
             OR (((COMB-KOH1HKNNUM     = "974") OR
                  (COMB-KOH2HKNNUM     = "974") OR
                  (COMB-KOH3HKNNUM     = "974") OR
                  (COMB-KOH4HKNNUM     = "974")) AND
                 ((SYU-KOH1HKNNUM      = "974") OR
                  (SYU-KOH2HKNNUM      = "974") OR
                  (SYU-KOH3HKNNUM      = "974") OR
                  (SYU-KOH4HKNNUM      = "974")))
      *
                   COMPUTE WK-CYOKI        =  WK-CYOKI
                                           +  SYU-KOH-FTN
                   COMPUTE WK-CYOKI-ENTANI =  WK-CYOKI-ENTANI
                                           + (SYU-KOH-FTN-ENTANI + 0.9)
      *
                   IF  (LNK-SYU-SRYYMD(1:6) >=  "200604"       )
                   AND (SYU-KOH1HKNNUM   NOT =  "015" AND "016"
                                                      AND "024")
                   AND (SYU-KOH2HKNNUM   NOT =  "015" AND "016"
                                                      AND "024")
                      ADD   SYU-KOH-FTN  TO  FTN-CYOKI-M-JI
                      ADD   SYU-KOH-FTN  TO  FTN-CYOKI-M2-JI
                      COMPUTE  WK-ENTANI =  SYU-KOH-FTN-ENTANI + 0.9
                      ADD   WK-ENTANI    TO  FTN-CYOKI-M3-JI
                   END-IF
      *
             END-IF
      *
           END-IF.
      *
      *    ¸øÈñ£±¥«¥¦¥ó¥È
           IF ((COMB-KOH1HKNNUM      NOT =   SPACE    ) AND
               (COMB-KOH1-GAIJGNKBN      =   "1"      ) AND
               (FLG-ZAITAKU              =   ZERO     ))
           OR ( FLG-IRYOHI70       =  3 OR 4 OR 6      )
             IF  ( SYU-NYUGAIKBN              =   "2"   )
             OR  ((SYU-NYUGAIKBN              =   "1"  ) AND
                  (FLG-NYUGAI                 =    1   ))
               IF   ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM ) AND
                     (COMB-KOH1ID         =  WRK-COMB-KOH1ID))
               OR   ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM ) AND
                     (COMB-KOH1ID         =  WRK-COMB-KOH2ID))
               OR   ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH3HKNNUM ) AND
                     (COMB-KOH1ID         =  WRK-COMB-KOH3ID))
               OR   ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH4HKNNUM ) AND
                     (COMB-KOH1ID         =  WRK-COMB-KOH4ID))
      *
                   IF  FLG-GENBUTU70    NOT =  ZERO
                     IF   FLG-IRYOHI70  =  3 OR 4 OR 6
                       COMPUTE  WK-KOUREI-IRYOHI70  =
                                WK-KOUREI-IRYOHI70  +
                               (SYU-HKNTEN (16) * 10)
                       MOVE       1    TO      FLG-SYUKEI
                       IF ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM ) AND
                           (COMB-KOH1ID         =  WRK-COMB-KOH2ID))
                       OR ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH3HKNNUM ) AND
                           (COMB-KOH1ID         =  WRK-COMB-KOH3ID))
                       OR ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH4HKNNUM ) AND
                           (COMB-KOH1ID         =  WRK-COMB-KOH4ID))
                          MOVE    1    TO      FLG-SYUKEI2
                       END-IF
                     END-IF
                     IF   FLG-IRYOHI70K NOT = ZERO
                       COMPUTE  WK-KOUREI-IRYOHI70K =
                                WK-KOUREI-IRYOHI70K +
                               (SYU-HKNTEN (16) * 10)
                     END-IF
                   ELSE
                     IF    (FLG-IRYOHI      =   1       )
                     OR    (FLG-IRYOHI2     =   1  OR  2)
                       COMPUTE  WK-IRYOHI   =  WK-IRYOHI  +
                                              (SYU-HKNTEN (16) * 10)
                     END-IF
                   END-IF
      *
                   MOVE   SYU-SRYYMD(7:2)     TO   IDX2X
                   MOVE   1                   TO   IDX3
                   IF   (MAX-KIN-KOHI1-D    NOT =  ZERO)
                   OR   (MAX-KIN-KOHI1-M    NOT =  ZERO)
                       PERFORM   22021-KAISU-COUNT-SEC
                   ELSE
                       PERFORM   22022-KAISU-COUNT2-SEC
                   END-IF
      *
                   IF ((WK-1001-PREFNUM          =  07      ) AND
                       (LNK-SYU-KOH1HKNNUM       = "245"    ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200412" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH1HKNNUM  = "343" OR "342") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200507" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH1HKNNUM       = "344"    ) AND
                       (FLG-SANSHOU2        =   "2" OR "3"  ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200507" ))
                   OR ((WK-1001-PREFNUM          =  07      ) AND
                       (LNK-SYU-KOH1HKNNUM       = "246"    ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200604" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH1HKNNUM  = "181" OR "182"
                                           OR "183" OR "281") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200704" ))
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM
                         IF   FLG-GEN70-TOKUSYU  =  1
                           COMPUTE  WK-KEISAN           =
                                    SYU-JYO-HKNFTNMONEY -
                                    SYU-KOH1HKNFTNMONEY
                           ADD      WK-KEISAN   TO   FTN-SYU-M
                           COMPUTE  WK-TOKUSYU  = SYU-JYO-COMPFTN-ENTANI
                                                + 0.9
                           COMPUTE  WK-TOKUSYU2 = SYU-KOH1COMPFTN-ENTANI
                                                + 0.9
                           COMPUTE  WK-KEISAN   =
                                    WK-TOKUSYU  - WK-TOKUSYU2
                           ADD      WK-KEISAN   TO   FTN-SYU-M-ENTANI
                         END-IF
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH1HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH1COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM
                         IF   FLG-GEN70-TOKUSYU  =  1
                           COMPUTE  WK-KEISAN           =
                                    SYU-JYO-HKNFTNMONEY -
                                    SYU-KOH2HKNFTNMONEY
                           ADD      WK-KEISAN   TO   FTN-SYU-M
                           COMPUTE  WK-TOKUSYU  = SYU-JYO-COMPFTN-ENTANI
                                                + 0.9
                           COMPUTE  WK-TOKUSYU2 = SYU-KOH2COMPFTN-ENTANI
                                                + 0.9
                           COMPUTE  WK-KEISAN   =
                                    WK-TOKUSYU  - WK-TOKUSYU2
                           ADD      WK-KEISAN   TO   FTN-SYU-M-ENTANI
                         END-IF
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH2HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH2COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH3HKNNUM
                         IF   FLG-GEN70-TOKUSYU  =  1
                           COMPUTE  WK-KEISAN           =
                                    SYU-JYO-HKNFTNMONEY -
                                    SYU-KOH3HKNFTNMONEY
                           ADD      WK-KEISAN   TO   FTN-SYU-M
                           COMPUTE  WK-TOKUSYU  = SYU-JYO-COMPFTN-ENTANI
                                                + 0.9
                           COMPUTE  WK-TOKUSYU2 = SYU-KOH3COMPFTN-ENTANI
                                                + 0.9
                           COMPUTE  WK-KEISAN   =
                                    WK-TOKUSYU  - WK-TOKUSYU2
                           ADD      WK-KEISAN   TO   FTN-SYU-M-ENTANI
                         END-IF
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH3HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH3COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH4HKNNUM
                         IF   FLG-GEN70-TOKUSYU  =  1
                           COMPUTE  WK-KEISAN           =
                                    SYU-JYO-HKNFTNMONEY -
                                    SYU-KOH4HKNFTNMONEY
                           ADD      WK-KEISAN   TO   FTN-SYU-M
                           COMPUTE  WK-TOKUSYU  = SYU-JYO-COMPFTN-ENTANI
                                                + 0.9
                           COMPUTE  WK-TOKUSYU2 = SYU-KOH4COMPFTN-ENTANI
                                                + 0.9
                           COMPUTE  WK-KEISAN   =
                                    WK-TOKUSYU  - WK-TOKUSYU2
                           ADD      WK-KEISAN   TO   FTN-SYU-M-ENTANI
                         END-IF
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH4HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH4COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH4COMPFTN-ENTANI
                      END-IF
                   END-IF
      *
                   IF ((WK-1001-PREFNUM         =  33      ) AND
                       (LNK-SYU-KOH1HKNNUM      = "285"    ) AND
                       (LNK-SYU-SRYYMD(1:6)    >= "200110" ) AND
                       (LNK-SYU-SRYYMD(1:6)    <= "200609" ))
                   OR ((WK-1001-PREFNUM         =  13      ) AND
                       (LNK-SYU-KOH1HKNNUM      = "182"    ) AND
                       (LNK-SYU-SRYYMD(1:6)    >= "200610" ))
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH1HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH1HKNFTNMONEY
                         IF   FLG-TOKYO  =  1
                            ADD  SYU-KOH1HKNFTNMONEY  TO   FTN-SYU-M
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH1COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH1COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH1COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH1COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                         IF   FLG-TOKYO  =  1
                            ADD  SYU-KOH1COMPFTN-ENTANI
                                         TO   FTN-SYU-M-ENTANI
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH2HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH2HKNFTNMONEY
                         IF   FLG-TOKYO  =  1
                            ADD  SYU-KOH2HKNFTNMONEY  TO   FTN-SYU-M
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH2COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH2COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH2COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH2COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                         IF   FLG-TOKYO  =  1
                            ADD  SYU-KOH2COMPFTN-ENTANI
                                         TO   FTN-SYU-M-ENTANI
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH3HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH3HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH3HKNFTNMONEY
                         IF   FLG-TOKYO  =  1
                            ADD  SYU-KOH3HKNFTNMONEY  TO   FTN-SYU-M
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH3COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH3COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH3COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH3COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                         IF   FLG-TOKYO  =  1
                            ADD  SYU-KOH3COMPFTN-ENTANI
                                         TO   FTN-SYU-M-ENTANI
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH4HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH4HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH4HKNFTNMONEY
                         IF   FLG-TOKYO  =  1
                            ADD  SYU-KOH4HKNFTNMONEY  TO   FTN-SYU-M
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH4COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH4COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH4COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH4COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                         IF   FLG-TOKYO  =  1
                            ADD  SYU-KOH4COMPFTN-ENTANI
                                         TO   FTN-SYU-M-ENTANI
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4COMPFTN-ENTANI
                        END-IF
                      END-IF
                   END-IF
      *
                   IF  (WK-1001-PREFNUM          =  44      ) AND
                       (LNK-SYU-KOH1HKNNUM  = "183" OR "283") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200610" )
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM
                                               OR  SYU-KOH2HKNNUM
                                               OR  SYU-KOH3HKNNUM
                                               OR  SYU-KOH4HKNNUM
                         ADD   SYU-JYO-HKNFTNMONEY
                                        TO  WK-OITA-HKNFTNMONEY
                         ADD   SYU-JYO-COMPFTN
                                        TO  WK-OITA-CYOFTN
                      END-IF
                   END-IF
      *
                   IF        LNK-SYU-SRYYMD(7:2)    =  SYU-SRYYMD(7:2)
                       EVALUATE  TRUE
                           WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH1HKNNUM)
                                 ADD   SYU-KOH1HKNFTNMONEY
                                                    TO  FTN-KOHI1-D
                                 IF  SYU-KOH1YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI1-D2 =              
                                            FTN-KOHI1-D2     +          
                                            SYU-KOH1COMPFTN  +          
                                            SYU-KOH1COMPYKZFTN          
                                   COMPUTE  FTN-KOHI1-D3 =              
                                            FTN-KOHI1-D3            +   
                                            SYU-KOH1COMPFTN-ENTANI  +   
                                            SYU-KOH1COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH1COMPFTN                
                                                    TO  FTN-KOHI1-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI1-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH1HKNFTNMONEY
                                                      TO  FTN-KOHI1-D-K
                                   IF  SYU-KOH1YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI1-D2-K =            
                                              FTN-KOHI1-D2-K   +          
                                              SYU-KOH1COMPFTN  +          
                                              SYU-KOH1COMPYKZFTN          
                                     COMPUTE  FTN-KOHI1-D3-K =          
                                              FTN-KOHI1-D3-K          + 
                                              SYU-KOH1COMPFTN-ENTANI  + 
                                              SYU-KOH1COMPYKZFTN      + 
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH1COMPFTN                
                                                      TO  FTN-KOHI1-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI1-D3-K 
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH2HKNNUM)
                                 ADD   SYU-KOH2HKNFTNMONEY
                                                    TO  FTN-KOHI1-D
                                 IF  SYU-KOH2YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI1-D2 =              
                                            FTN-KOHI1-D2     +          
                                            SYU-KOH2COMPFTN  +          
                                            SYU-KOH2COMPYKZFTN          
                                   COMPUTE  FTN-KOHI1-D3 =              
                                            FTN-KOHI1-D3            +   
                                            SYU-KOH2COMPFTN-ENTANI  +   
                                            SYU-KOH2COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH2COMPFTN                
                                                    TO  FTN-KOHI1-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI1-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH2HKNFTNMONEY
                                                      TO  FTN-KOHI1-D-K
                                   IF  SYU-KOH2YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI1-D2-K =            
                                              FTN-KOHI1-D2-K   +          
                                              SYU-KOH2COMPFTN  +          
                                              SYU-KOH2COMPYKZFTN          
                                     COMPUTE  FTN-KOHI1-D3-K =            
                                              FTN-KOHI1-D3-K          +   
                                              SYU-KOH2COMPFTN-ENTANI  +   
                                              SYU-KOH2COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH2COMPFTN                
                                                      TO  FTN-KOHI1-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI1-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH3HKNNUM)
                                 ADD   SYU-KOH3HKNFTNMONEY
                                                    TO  FTN-KOHI1-D
                                 IF  SYU-KOH3YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI1-D2 =              
                                            FTN-KOHI1-D2     +          
                                            SYU-KOH3COMPFTN  +          
                                            SYU-KOH3COMPYKZFTN          
                                   COMPUTE  FTN-KOHI1-D3 =              
                                            FTN-KOHI1-D3            +   
                                            SYU-KOH3COMPFTN-ENTANI  +   
                                            SYU-KOH3COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH3COMPFTN                
                                                    TO  FTN-KOHI1-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI1-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH3HKNFTNMONEY
                                                      TO  FTN-KOHI1-D-K
                                   IF  SYU-KOH3YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI1-D2-K =            
                                              FTN-KOHI1-D2-K   +          
                                              SYU-KOH3COMPFTN  +          
                                              SYU-KOH3COMPYKZFTN          
                                     COMPUTE  FTN-KOHI1-D3-K =          
                                              FTN-KOHI1-D3-K          +   
                                              SYU-KOH3COMPFTN-ENTANI  +   
                                              SYU-KOH3COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH3COMPFTN                
                                                      TO  FTN-KOHI1-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI1-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH4HKNNUM)
                                 ADD   SYU-KOH4HKNFTNMONEY
                                                    TO  FTN-KOHI1-D
                                 IF  SYU-KOH4YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI1-D2 =              
                                            FTN-KOHI1-D2     +          
                                            SYU-KOH4COMPFTN  +          
                                            SYU-KOH4COMPYKZFTN          
                                   COMPUTE  FTN-KOHI1-D3 =              
                                            FTN-KOHI1-D3            +   
                                            SYU-KOH4COMPFTN-ENTANI  +   
                                            SYU-KOH4COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH4COMPFTN                
                                                    TO  FTN-KOHI1-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI1-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH4HKNFTNMONEY
                                                      TO  FTN-KOHI1-D-K
                                   IF  SYU-KOH4YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI1-D2-K =            
                                              FTN-KOHI1-D2-K   +          
                                              SYU-KOH4COMPFTN  +          
                                              SYU-KOH4COMPYKZFTN          
                                     COMPUTE  FTN-KOHI1-D3-K =          
                                              FTN-KOHI1-D3-K          +   
                                              SYU-KOH4COMPFTN-ENTANI  +   
                                              SYU-KOH4COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH4COMPFTN                
                                                      TO  FTN-KOHI1-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI1-D3-K    
                                   END-IF                                
                                 END-IF
                       END-EVALUATE
                   END-IF
                   EVALUATE  TRUE
                       WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH1HKNNUM)
                             ADD   SYU-KOH1HKNFTNMONEY
                                                TO  FTN-KOHI1-M
                             IF  SYU-KOH1YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI1-M2 =                  
                                        FTN-KOHI1-M2     +              
                                        SYU-KOH1COMPFTN  +              
                                        SYU-KOH1COMPYKZFTN              
                               COMPUTE  FTN-KOHI1-M3 =                  
                                        FTN-KOHI1-M3            +       
                                        SYU-KOH1COMPFTN-ENTANI  +       
                                        SYU-KOH1COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH1COMPFTN                    
                                                TO  FTN-KOHI1-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI1-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200604")
                             AND (LNK-SYU-KOH1HKNNUM    =
                                            "015" OR "016" OR "021")
                             AND (WK-JIRITU-CYUKANSOU(1)   =  2    )
                             AND (SYU-NYUGAIKBN            = "2"   )
                                ADD   SYU-KOH1HKNFTNMONEY
                                                   TO  FTN-KOHI1-M-JI
                                ADD   SYU-KOH1COMPFTN
                                                   TO  FTN-KOHI1-M2-JI
                                COMPUTE WK-ENTANI =
                                          SYU-KOH1COMPFTN-ENTANI + 0.9
                                ADD   WK-ENTANI    TO  FTN-KOHI1-M3-JI
                             END-IF
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200610")
                             AND (LNK-SYU-KOH1HKNNUM    =
                                            "024" OR "079"         )
                             AND (SYU-NYUGAIKBN            = "2"   )
                                ADD   SYU-KOH1HKNFTNMONEY
                                                   TO  FTN-KOHI1-M-JI
                                ADD   SYU-KOH1COMPFTN
                                                   TO  FTN-KOHI1-M2-JI
                                COMPUTE WK-ENTANI =
                                          SYU-KOH1COMPFTN-ENTANI + 0.9
                                ADD   WK-ENTANI    TO  FTN-KOHI1-M3-JI
                             END-IF
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH1HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH1SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH1SKJ-FTN
                                                TO  FTN-KOHI1-M
                                                    FTN-KOHI1-M2
                                                    FTN-KOHI1-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH2HKNNUM)
                             ADD   SYU-KOH2HKNFTNMONEY
                                                TO  FTN-KOHI1-M
                             IF  SYU-KOH2YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI1-M2 =                  
                                        FTN-KOHI1-M2     +              
                                        SYU-KOH2COMPFTN  +              
                                        SYU-KOH2COMPYKZFTN              
                               COMPUTE  FTN-KOHI1-M3 =                  
                                        FTN-KOHI1-M3            +       
                                        SYU-KOH2COMPFTN-ENTANI  +       
                                        SYU-KOH2COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH2COMPFTN                    
                                                TO  FTN-KOHI1-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI1-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200604")
                             AND (LNK-SYU-KOH1HKNNUM    =
                                            "015" OR "016" OR "021")
                             AND (WK-JIRITU-CYUKANSOU(1)   =  2    )
                             AND (SYU-NYUGAIKBN            = "2"   )
                                ADD   SYU-KOH2HKNFTNMONEY
                                                   TO  FTN-KOHI1-M-JI
                                ADD   SYU-KOH2COMPFTN
                                                   TO  FTN-KOHI1-M2-JI
                                COMPUTE WK-ENTANI =
                                          SYU-KOH2COMPFTN-ENTANI + 0.9
                                ADD   WK-ENTANI    TO  FTN-KOHI1-M3-JI
                             END-IF
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200610")
                             AND (LNK-SYU-KOH1HKNNUM    =
                                            "024" OR "079"         )
                             AND (SYU-NYUGAIKBN            = "2"   )
                                ADD   SYU-KOH2HKNFTNMONEY
                                                   TO  FTN-KOHI1-M-JI
                                ADD   SYU-KOH2COMPFTN
                                                   TO  FTN-KOHI1-M2-JI
                                COMPUTE WK-ENTANI =
                                          SYU-KOH2COMPFTN-ENTANI + 0.9
                                ADD   WK-ENTANI    TO  FTN-KOHI1-M3-JI
                             END-IF
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH1HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH2SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH2SKJ-FTN
                                                TO  FTN-KOHI1-M
                                                    FTN-KOHI1-M2
                                                    FTN-KOHI1-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH3HKNNUM)
                             ADD   SYU-KOH3HKNFTNMONEY
                                                TO  FTN-KOHI1-M
                             IF  SYU-KOH3YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI1-M2 =                  
                                        FTN-KOHI1-M2     +              
                                        SYU-KOH3COMPFTN  +              
                                        SYU-KOH3COMPYKZFTN              
                               COMPUTE  FTN-KOHI1-M3 =                  
                                        FTN-KOHI1-M3            +       
                                        SYU-KOH3COMPFTN-ENTANI  +       
                                        SYU-KOH3COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH3COMPFTN                    
                                                TO  FTN-KOHI1-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI1-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH1HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH3SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH3SKJ-FTN
                                                TO  FTN-KOHI1-M
                                                    FTN-KOHI1-M2
                                                    FTN-KOHI1-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH4HKNNUM)
                             ADD   SYU-KOH4HKNFTNMONEY
                                                TO  FTN-KOHI1-M
                             IF  SYU-KOH4YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI1-M2 =                  
                                        FTN-KOHI1-M2     +              
                                        SYU-KOH4COMPFTN  +              
                                        SYU-KOH4COMPYKZFTN              
                               COMPUTE  FTN-KOHI1-M3 =                  
                                        FTN-KOHI1-M3            +       
                                        SYU-KOH4COMPFTN-ENTANI  +       
                                        SYU-KOH4COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH4COMPFTN                    
                                                TO  FTN-KOHI1-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI1-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH1HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH4SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH4SKJ-FTN
                                                TO  FTN-KOHI1-M
                                                    FTN-KOHI1-M2
                                                    FTN-KOHI1-M3
                             END-IF
                   END-EVALUATE
               END-IF
             END-IF
           END-IF.
      *
      *    ¸øÈñ£²¥«¥¦¥ó¥È
           IF  (COMB-KOH2HKNNUM               NOT = SPACE)   AND
               (COMB-KOH2-GAIJGNKBN           =     "1")     
             IF  ( SYU-NYUGAIKBN              =   "2"   )
             OR  ((SYU-NYUGAIKBN              =   "1"  ) AND
                  (FLG-NYUGAI                 =    2   ))
               IF   ((LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM ) AND
                     (COMB-KOH2ID         =  WRK-COMB-KOH1ID))
               OR   ((LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM ) AND
                     (COMB-KOH2ID         =  WRK-COMB-KOH2ID))
               OR   ((LNK-SYU-KOH2HKNNUM  =  SYU-KOH3HKNNUM ) AND
                     (COMB-KOH2ID         =  WRK-COMB-KOH3ID))
               OR   ((LNK-SYU-KOH2HKNNUM  =  SYU-KOH4HKNNUM ) AND
                     (COMB-KOH2ID         =  WRK-COMB-KOH4ID))
      *
                   IF  FLG-GENBUTU70    NOT =  ZERO
                     IF   (FLG-IRYOHI70       =  6            )
                     AND  (FLG-SYUKEI         =  ZERO         )
                       IF  ((WK-1001-PREFNUM    =  07           ) AND
                            (LNK-SYU-KOH2HKNNUM = "245" OR "246"))
                       OR  ((WK-1001-PREFNUM    =  11           ) AND
                            (LNK-SYU-KOH2HKNNUM = "342" OR "343"
                                               OR "181" OR "182"
                                               OR "183" OR "281"))
                       OR  ((WK-1001-PREFNUM    =  11           ) AND
                            (LNK-SYU-KOH2HKNNUM = "344"         ) AND
                            (FLG-SANSHOU2       = "2" OR "3"    ))
                         COMPUTE  WK-KOUREI-IRYOHI70  =
                                  WK-KOUREI-IRYOHI70  +
                                 (SYU-HKNTEN (16) * 10)
                       END-IF
                     END-IF
                   END-IF
      *
                   MOVE   SYU-SRYYMD(7:2)     TO    IDX2X
                   MOVE   2                   TO    IDX3
                   IF   (MAX-KIN-KOHI2-D    NOT =  ZERO)
                   OR   (MAX-KIN-KOHI2-M    NOT =  ZERO)
                       PERFORM   22021-KAISU-COUNT-SEC
                   ELSE
                       PERFORM   22022-KAISU-COUNT2-SEC
                   END-IF
      *
                   IF ((WK-1001-PREFNUM          =  07      ) AND
                       (LNK-SYU-KOH2HKNNUM       = "245"    ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200412" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH2HKNNUM  = "343" OR "342") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200507" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH2HKNNUM       = "344"    ) AND
                       (FLG-SANSHOU2        =   "2" OR "3"  ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200507" ))
                   OR ((WK-1001-PREFNUM          =  07      ) AND
                       (LNK-SYU-KOH2HKNNUM       = "246"    ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200604" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH2HKNNUM  = "181" OR "182"
                                           OR "183" OR "281") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200704" ))
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH1HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH1COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH2HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH2COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH3HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH3HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH3COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH4HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH4HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH4COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH4COMPFTN-ENTANI
                      END-IF
                   END-IF
      *
                   IF ((WK-1001-PREFNUM         =  33      ) AND
                       (LNK-SYU-KOH2HKNNUM      = "285"    ) AND
                       (LNK-SYU-SRYYMD(1:6)    >= "200110" ) AND
                       (LNK-SYU-SRYYMD(1:6)    <= "200609" ))
                   OR ((WK-1001-PREFNUM         =  13      ) AND
                       (LNK-SYU-KOH2HKNNUM      = "182"    ) AND
                       (LNK-SYU-SRYYMD(1:6)    >= "200610" ))
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH1HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH1HKNFTNMONEY
                         IF   FLG-TOKYO  =  2
                            ADD  SYU-KOH1HKNFTNMONEY  TO   FTN-KOHI1-M
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH1COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH1COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH1COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH1COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                         IF   FLG-TOKYO  =  2
                            ADD  SYU-KOH1COMPFTN-ENTANI
                                         TO   FTN-KOHI1-M-ENTANI
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH2HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH2HKNFTNMONEY
                         IF   FLG-TOKYO  =  2
                            ADD  SYU-KOH2HKNFTNMONEY  TO   FTN-KOHI1-M
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH2COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH2COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH2COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH2COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                         IF   FLG-TOKYO  =  2
                            ADD  SYU-KOH2COMPFTN-ENTANI
                                         TO   FTN-KOHI1-M-ENTANI
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH3HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH3HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH3HKNFTNMONEY
                         IF   FLG-TOKYO  =  2
                            ADD  SYU-KOH3HKNFTNMONEY  TO   FTN-KOHI1-M
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH3COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH3COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH3COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH3COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                         IF   FLG-TOKYO  =  2
                            ADD  SYU-KOH3COMPFTN-ENTANI
                                         TO   FTN-KOHI1-M-ENTANI
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH4HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH4HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH4HKNFTNMONEY
                         IF   FLG-TOKYO  =  2
                            ADD  SYU-KOH4HKNFTNMONEY  TO   FTN-KOHI1-M
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH4COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH4COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH4COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH4COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                         IF   FLG-TOKYO  =  2
                            ADD  SYU-KOH4COMPFTN-ENTANI
                                         TO   FTN-KOHI1-M-ENTANI
                         END-IF
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4COMPFTN-ENTANI
                        END-IF
                      END-IF
                   END-IF
      *
                   IF (( WK-1001-PREFNUM         =  27        ) AND
                       ( LNK-SYU-KOH2HKNNUM  = "972" OR "974" ) AND
                       ( COMB-KOH1HKNNUM     = "180" OR "182"
                                            OR "186" OR "188" ) AND
                       ( LNK-S01-SAIKEISAN-KBN   =   "1"      ) AND
                       ( LNK-SYU-DENPNUM     NOT =  ZERO      ) AND
                       ( LNK-SYU-DENPNUM      <  SYU-DENPNUM  ))
                   OR (( WK-1001-PREFNUM         =  34        ) AND
                       ( LNK-SYU-KOH2HKNNUM  = "972" OR "974" ) AND
                       ( COMB-KOH1HKNNUM     = "191" OR "291" ) AND
                       ( LNK-S01-SAIKEISAN-KBN   =   "1"      ) AND
                       ( LNK-SYU-DENPNUM     NOT =  ZERO      ) AND
                       ( LNK-SYU-DENPNUM      <  SYU-DENPNUM  ))
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM
                         MOVE     ZERO   TO   SYU-KOH1HKNFTNMONEY
                                              SYU-KOH1COMPFTN
                                              SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM
                         MOVE     ZERO   TO   SYU-KOH2HKNFTNMONEY
                                              SYU-KOH2COMPFTN
                                              SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH3HKNNUM
                         MOVE     ZERO   TO   SYU-KOH3HKNFTNMONEY
                                              SYU-KOH3COMPFTN
                                              SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH4HKNNUM
                         MOVE     ZERO   TO   SYU-KOH4HKNFTNMONEY
                                              SYU-KOH4COMPFTN
                                              SYU-KOH4COMPFTN-ENTANI
                      END-IF
                   END-IF
      *
                   IF  (WK-1001-PREFNUM          =  44      ) AND
                       (LNK-SYU-KOH2HKNNUM  = "183" OR "283") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200610" )
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM
                                               OR  SYU-KOH2HKNNUM
                                               OR  SYU-KOH3HKNNUM
                                               OR  SYU-KOH4HKNNUM
                         ADD   SYU-JYO-HKNFTNMONEY
                                        TO  WK-OITA-HKNFTNMONEY
                         ADD   SYU-JYO-COMPFTN
                                        TO  WK-OITA-CYOFTN
                      END-IF
                   END-IF
      *
                   IF        LNK-SYU-SRYYMD(7:2) =  SYU-SRYYMD(7:2)
                       EVALUATE  TRUE
                           WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH1HKNNUM)
                                 ADD   SYU-KOH1HKNFTNMONEY
                                                    TO  FTN-KOHI2-D
                                 IF  SYU-KOH1YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI2-D2 =              
                                            FTN-KOHI2-D2     +          
                                            SYU-KOH1COMPFTN  +          
                                            SYU-KOH1COMPYKZFTN          
                                   COMPUTE  FTN-KOHI2-D3 =              
                                            FTN-KOHI2-D3            +   
                                            SYU-KOH1COMPFTN-ENTANI  +   
                                            SYU-KOH1COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH1COMPFTN                
                                                    TO  FTN-KOHI2-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI2-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH1HKNFTNMONEY
                                                      TO  FTN-KOHI2-D-K
                                   IF  SYU-KOH1YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI2-D2-K =            
                                              FTN-KOHI2-D2-K   +          
                                              SYU-KOH1COMPFTN  +          
                                              SYU-KOH1COMPYKZFTN          
                                     COMPUTE  FTN-KOHI2-D3-K =          
                                              FTN-KOHI2-D3-K          +   
                                              SYU-KOH1COMPFTN-ENTANI  +   
                                              SYU-KOH1COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH1COMPFTN                
                                                      TO  FTN-KOHI2-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI2-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH2HKNNUM)
                                 ADD   SYU-KOH2HKNFTNMONEY
                                                    TO  FTN-KOHI2-D
                                 IF  SYU-KOH2YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI2-D2 =              
                                            FTN-KOHI2-D2     +          
                                            SYU-KOH2COMPFTN  +          
                                            SYU-KOH2COMPYKZFTN          
                                   COMPUTE  FTN-KOHI2-D3 =              
                                            FTN-KOHI2-D3            +   
                                            SYU-KOH2COMPFTN-ENTANI  +   
                                            SYU-KOH2COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH2COMPFTN                
                                                    TO  FTN-KOHI2-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI2-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH2HKNFTNMONEY
                                                      TO  FTN-KOHI2-D-K
                                   IF  SYU-KOH2YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI2-D2-K =            
                                              FTN-KOHI2-D2-K   +          
                                              SYU-KOH2COMPFTN  +          
                                              SYU-KOH2COMPYKZFTN          
                                     COMPUTE  FTN-KOHI2-D3-K =          
                                              FTN-KOHI2-D3-K          +   
                                              SYU-KOH2COMPFTN-ENTANI  +   
                                              SYU-KOH2COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH2COMPFTN                
                                                      TO  FTN-KOHI2-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI2-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH3HKNNUM)
                                 ADD   SYU-KOH3HKNFTNMONEY
                                                    TO  FTN-KOHI2-D
                                 IF  SYU-KOH3YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI2-D2 =              
                                            FTN-KOHI2-D2     +          
                                            SYU-KOH3COMPFTN  +          
                                            SYU-KOH3COMPYKZFTN          
                                   COMPUTE  FTN-KOHI2-D3 =              
                                            FTN-KOHI2-D3            +   
                                            SYU-KOH3COMPFTN-ENTANI  +   
                                            SYU-KOH3COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH3COMPFTN                
                                                    TO  FTN-KOHI2-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI2-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH3HKNFTNMONEY
                                                      TO  FTN-KOHI2-D-K
                                   IF  SYU-KOH3YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI2-D2-K =            
                                              FTN-KOHI2-D2-K   +          
                                              SYU-KOH3COMPFTN  +          
                                              SYU-KOH3COMPYKZFTN          
                                     COMPUTE  FTN-KOHI2-D3-K =          
                                              FTN-KOHI2-D3-K          +   
                                              SYU-KOH3COMPFTN-ENTANI  +   
                                              SYU-KOH3COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH3COMPFTN                
                                                      TO  FTN-KOHI2-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI2-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH4HKNNUM)
                                 ADD   SYU-KOH4HKNFTNMONEY
                                                    TO  FTN-KOHI2-D
                                 IF  SYU-KOH4YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI2-D2 =              
                                            FTN-KOHI2-D2     +          
                                            SYU-KOH4COMPFTN  +          
                                            SYU-KOH4COMPYKZFTN          
                                   COMPUTE  FTN-KOHI2-D3 =              
                                            FTN-KOHI2-D3            +   
                                            SYU-KOH4COMPFTN-ENTANI  +   
                                            SYU-KOH4COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH4COMPFTN                
                                                    TO  FTN-KOHI2-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI2-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH4HKNFTNMONEY
                                                      TO  FTN-KOHI2-D-K
                                   IF  SYU-KOH4YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI2-D2-K =            
                                              FTN-KOHI2-D2-K   +          
                                              SYU-KOH4COMPFTN  +          
                                              SYU-KOH4COMPYKZFTN          
                                     COMPUTE  FTN-KOHI2-D3-K =          
                                              FTN-KOHI2-D3-K          +   
                                              SYU-KOH4COMPFTN-ENTANI  +   
                                              SYU-KOH4COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH4COMPFTN                
                                                      TO  FTN-KOHI2-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI2-D3-K    
                                   END-IF                                
                                 END-IF
                       END-EVALUATE
                   END-IF
                   EVALUATE  TRUE
                       WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH1HKNNUM)
                             ADD   SYU-KOH1HKNFTNMONEY
                                                TO  FTN-KOHI2-M
                             IF  SYU-KOH1YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI2-M2 =                  
                                        FTN-KOHI2-M2     +              
                                        SYU-KOH1COMPFTN  +              
                                        SYU-KOH1COMPYKZFTN              
                               COMPUTE  FTN-KOHI2-M3 =                  
                                        FTN-KOHI2-M3            +       
                                        SYU-KOH1COMPFTN-ENTANI  +       
                                        SYU-KOH1COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH1COMPFTN                    
                                                TO  FTN-KOHI2-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI2-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200604")
                             AND (LNK-SYU-KOH2HKNNUM    =
                                            "015" OR "016" OR "021")
                             AND (WK-JIRITU-CYUKANSOU(2)   =  2    )
                             AND (SYU-NYUGAIKBN            = "2"   )
                                ADD   SYU-KOH1HKNFTNMONEY
                                                   TO  FTN-KOHI2-M-JI
                                ADD   SYU-KOH1COMPFTN
                                                   TO  FTN-KOHI2-M2-JI
                                COMPUTE WK-ENTANI =
                                          SYU-KOH1COMPFTN-ENTANI + 0.9
                                ADD   WK-ENTANI    TO  FTN-KOHI2-M3-JI
                             END-IF
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200610")
                             AND (LNK-SYU-KOH2HKNNUM    =
                                            "024" OR "079"         )
                             AND (SYU-NYUGAIKBN            = "2"   )
                                ADD   SYU-KOH1HKNFTNMONEY
                                                   TO  FTN-KOHI2-M-JI
                                ADD   SYU-KOH1COMPFTN
                                                   TO  FTN-KOHI2-M2-JI
                                COMPUTE WK-ENTANI =
                                          SYU-KOH1COMPFTN-ENTANI + 0.9
                                ADD   WK-ENTANI    TO  FTN-KOHI2-M3-JI
                             END-IF
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH2HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH1SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH1SKJ-FTN
                                                TO  FTN-KOHI2-M
                                                    FTN-KOHI2-M2
                                                    FTN-KOHI2-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH2HKNNUM)
                             ADD   SYU-KOH2HKNFTNMONEY
                                                TO  FTN-KOHI2-M
                             IF  SYU-KOH2YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI2-M2 =                  
                                        FTN-KOHI2-M2     +              
                                        SYU-KOH2COMPFTN  +              
                                        SYU-KOH2COMPYKZFTN              
                               COMPUTE  FTN-KOHI2-M3 =                  
                                        FTN-KOHI2-M3            +       
                                        SYU-KOH2COMPFTN-ENTANI  +       
                                        SYU-KOH2COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH2COMPFTN                    
                                                TO  FTN-KOHI2-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI2-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200604")
                             AND (LNK-SYU-KOH2HKNNUM    =
                                            "015" OR "016" OR "021")
                             AND (WK-JIRITU-CYUKANSOU(2)   =  2    )
                             AND (SYU-NYUGAIKBN            = "2"   )
                                ADD   SYU-KOH2HKNFTNMONEY
                                                   TO  FTN-KOHI2-M-JI
                                ADD   SYU-KOH2COMPFTN
                                                   TO  FTN-KOHI2-M2-JI
                                COMPUTE WK-ENTANI =
                                          SYU-KOH2COMPFTN-ENTANI + 0.9
                                ADD   WK-ENTANI    TO  FTN-KOHI2-M3-JI
                             END-IF
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200610")
                             AND (LNK-SYU-KOH2HKNNUM    =
                                            "024" OR "079"         )
                             AND (SYU-NYUGAIKBN            = "2"   )
                                ADD   SYU-KOH2HKNFTNMONEY
                                                   TO  FTN-KOHI2-M-JI
                                ADD   SYU-KOH2COMPFTN
                                                   TO  FTN-KOHI2-M2-JI
                                COMPUTE WK-ENTANI =
                                          SYU-KOH2COMPFTN-ENTANI + 0.9
                                ADD   WK-ENTANI    TO  FTN-KOHI2-M3-JI
                             END-IF
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH2HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH2SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH2SKJ-FTN
                                                TO  FTN-KOHI2-M
                                                    FTN-KOHI2-M2
                                                    FTN-KOHI2-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH3HKNNUM)
                             ADD   SYU-KOH3HKNFTNMONEY
                                                TO  FTN-KOHI2-M
                             IF  SYU-KOH3YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI2-M2 =                  
                                        FTN-KOHI2-M2     +              
                                        SYU-KOH3COMPFTN  +              
                                        SYU-KOH3COMPYKZFTN              
                               COMPUTE  FTN-KOHI2-M3 =                  
                                        FTN-KOHI2-M3            +       
                                        SYU-KOH3COMPFTN-ENTANI  +       
                                        SYU-KOH3COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH3COMPFTN                    
                                                TO  FTN-KOHI2-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI2-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH2HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH3SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH3SKJ-FTN
                                                TO  FTN-KOHI2-M
                                                    FTN-KOHI2-M2
                                                    FTN-KOHI2-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH4HKNNUM)
                             ADD   SYU-KOH4HKNFTNMONEY
                                                TO  FTN-KOHI2-M
                             IF  SYU-KOH4YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI2-M2 =                  
                                        FTN-KOHI2-M2     +              
                                        SYU-KOH4COMPFTN  +              
                                        SYU-KOH4COMPYKZFTN              
                               COMPUTE  FTN-KOHI2-M3 =                  
                                        FTN-KOHI2-M3            +       
                                        SYU-KOH4COMPFTN-ENTANI  +       
                                        SYU-KOH4COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH4COMPFTN                    
                                                TO  FTN-KOHI2-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI2-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH2HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH4SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH4SKJ-FTN
                                                TO  FTN-KOHI2-M
                                                    FTN-KOHI2-M2
                                                    FTN-KOHI2-M3
                             END-IF
                   END-EVALUATE
               END-IF
             END-IF
           END-IF.
      *    ¸øÈñ£³¥«¥¦¥ó¥È
           IF  (COMB-KOH3HKNNUM               NOT = SPACE)   AND
               (COMB-KOH3-GAIJGNKBN           =     "1")     
             IF  ( SYU-NYUGAIKBN              =   "2"   )
             OR  ((SYU-NYUGAIKBN              =   "1"  ) AND
                  (FLG-NYUGAI                 =    3   ))
               IF   ((LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM ) AND
                     (COMB-KOH3ID         =  WRK-COMB-KOH1ID))
               OR   ((LNK-SYU-KOH3HKNNUM  =  SYU-KOH2HKNNUM ) AND
                     (COMB-KOH3ID         =  WRK-COMB-KOH2ID))
               OR   ((LNK-SYU-KOH3HKNNUM  =  SYU-KOH3HKNNUM ) AND
                     (COMB-KOH3ID         =  WRK-COMB-KOH3ID))
               OR   ((LNK-SYU-KOH3HKNNUM  =  SYU-KOH4HKNNUM ) AND
                     (COMB-KOH3ID         =  WRK-COMB-KOH4ID))
      *
                   MOVE   SYU-SRYYMD(7:2)     TO    IDX2X
                   MOVE   3                   TO    IDX3
                   IF   (MAX-KIN-KOHI3-D    NOT =  ZERO)
                   OR   (MAX-KIN-KOHI3-M    NOT =  ZERO)
                       PERFORM   22021-KAISU-COUNT-SEC
                   ELSE
                       PERFORM   22022-KAISU-COUNT2-SEC
                   END-IF
      *
                   IF ((WK-1001-PREFNUM          =  07      ) AND
                       (LNK-SYU-KOH3HKNNUM       = "245"    ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200412" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH3HKNNUM  = "343" OR "342") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200507" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH3HKNNUM       = "344"    ) AND
                       (FLG-SANSHOU2        =   "2" OR "3"  ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200507" ))
                   OR ((WK-1001-PREFNUM          =  07      ) AND
                       (LNK-SYU-KOH3HKNNUM       = "246"    ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200604" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH3HKNNUM  = "181" OR "182"
                                           OR "183" OR "281") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200704" ))
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH1HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH1COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH2HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH2HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH2COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH3HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH3HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH3COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH4HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH4HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH4COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH4COMPFTN-ENTANI
                      END-IF
                   END-IF
      *
                   IF ((WK-1001-PREFNUM         =  33      ) AND
                       (LNK-SYU-KOH3HKNNUM      = "285"    ) AND
                       (LNK-SYU-SRYYMD(1:6)    >= "200110" ) AND
                       (LNK-SYU-SRYYMD(1:6)    <= "200609" ))
                   OR ((WK-1001-PREFNUM         =  13      ) AND
                       (LNK-SYU-KOH3HKNNUM      = "182"    ) AND
                       (LNK-SYU-SRYYMD(1:6)    >= "200610" ))
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH1HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH1HKNFTNMONEY
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH1COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH1COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH1COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH1COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH2HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH2HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH2HKNFTNMONEY
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH2COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH2COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH2COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH2COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH3HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH3HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH3HKNFTNMONEY
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH3COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH3COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH3COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH3COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH4HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH4HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH4HKNFTNMONEY
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH4COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH4COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH4COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH4COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4COMPFTN-ENTANI
                        END-IF
                      END-IF
                   END-IF
      *
                   IF (( WK-1001-PREFNUM         =  27        ) AND
                       ( LNK-SYU-KOH3HKNNUM  = "972" OR "974" ) AND
                       ((COMB-KOH1HKNNUM     = "180" OR "182"
                                            OR "186" OR "188") OR
                        (COMB-KOH2HKNNUM     = "180" OR "182"
                                            OR "186" OR "188")) AND
                       ( LNK-S01-SAIKEISAN-KBN   =   "1"      ) AND
                       ( LNK-SYU-DENPNUM     NOT =  ZERO      ) AND
                       ( LNK-SYU-DENPNUM      <  SYU-DENPNUM  ))
                   OR (( WK-1001-PREFNUM         =  34        ) AND
                       ( LNK-SYU-KOH3HKNNUM  = "972" OR "974" ) AND
                       ((COMB-KOH1HKNNUM     = "191" OR "291") OR
                        (COMB-KOH2HKNNUM     = "191" OR "291")) AND
                       ( LNK-S01-SAIKEISAN-KBN   =   "1"      ) AND
                       ( LNK-SYU-DENPNUM     NOT =  ZERO      ) AND
                       ( LNK-SYU-DENPNUM      <  SYU-DENPNUM  ))
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM
                         MOVE     ZERO   TO   SYU-KOH1HKNFTNMONEY
                                              SYU-KOH1COMPFTN
                                              SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH2HKNNUM
                         MOVE     ZERO   TO   SYU-KOH2HKNFTNMONEY
                                              SYU-KOH2COMPFTN
                                              SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH3HKNNUM
                         MOVE     ZERO   TO   SYU-KOH3HKNFTNMONEY
                                              SYU-KOH3COMPFTN
                                              SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH4HKNNUM
                         MOVE     ZERO   TO   SYU-KOH4HKNFTNMONEY
                                              SYU-KOH4COMPFTN
                                              SYU-KOH4COMPFTN-ENTANI
                      END-IF
                   END-IF
      *
                   IF  (WK-1001-PREFNUM          =  44      ) AND
                       (LNK-SYU-KOH3HKNNUM  = "183" OR "283") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200610" )
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM
                                               OR  SYU-KOH2HKNNUM
                                               OR  SYU-KOH3HKNNUM
                                               OR  SYU-KOH4HKNNUM
                         ADD   SYU-JYO-HKNFTNMONEY
                                        TO  WK-OITA-HKNFTNMONEY
                         ADD   SYU-JYO-COMPFTN
                                        TO  WK-OITA-CYOFTN
                      END-IF
                   END-IF
      *
                   IF        LNK-SYU-SRYYMD(7:2)   =   SYU-SRYYMD(7:2)
                       EVALUATE  TRUE
                           WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH1HKNNUM)
                                 ADD   SYU-KOH1HKNFTNMONEY
                                                    TO  FTN-KOHI3-D
                                 IF  SYU-KOH1YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI3-D2 =              
                                            FTN-KOHI3-D2     +          
                                            SYU-KOH1COMPFTN  +          
                                            SYU-KOH1COMPYKZFTN          
                                   COMPUTE  FTN-KOHI3-D3 =              
                                            FTN-KOHI3-D3            +   
                                            SYU-KOH1COMPFTN-ENTANI  +   
                                            SYU-KOH1COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH1COMPFTN                
                                                    TO  FTN-KOHI3-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI3-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH1HKNFTNMONEY
                                                      TO  FTN-KOHI3-D-K
                                   IF  SYU-KOH1YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI3-D2-K =            
                                              FTN-KOHI3-D2-K   +          
                                              SYU-KOH1COMPFTN  +          
                                              SYU-KOH1COMPYKZFTN          
                                     COMPUTE  FTN-KOHI3-D3-K =          
                                              FTN-KOHI3-D3-K          +   
                                              SYU-KOH1COMPFTN-ENTANI  +   
                                              SYU-KOH1COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH1COMPFTN                
                                                      TO  FTN-KOHI3-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI3-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH2HKNNUM)
                                 ADD   SYU-KOH2HKNFTNMONEY
                                                    TO  FTN-KOHI3-D
                                 IF  SYU-KOH2YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI3-D2 =              
                                            FTN-KOHI3-D2     +          
                                            SYU-KOH2COMPFTN  +          
                                            SYU-KOH2COMPYKZFTN          
                                   COMPUTE  FTN-KOHI3-D3 =              
                                            FTN-KOHI3-D3            +   
                                            SYU-KOH2COMPFTN-ENTANI  +   
                                            SYU-KOH2COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH2COMPFTN                
                                                    TO  FTN-KOHI3-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI3-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH2HKNFTNMONEY
                                                      TO  FTN-KOHI3-D-K
                                   IF  SYU-KOH2YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI3-D2-K =            
                                              FTN-KOHI3-D2-K   +          
                                              SYU-KOH2COMPFTN  +          
                                              SYU-KOH2COMPYKZFTN          
                                     COMPUTE  FTN-KOHI3-D3-K =          
                                              FTN-KOHI3-D3-K          +   
                                              SYU-KOH2COMPFTN-ENTANI  +   
                                              SYU-KOH2COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH2COMPFTN                
                                                      TO  FTN-KOHI3-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI3-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH3HKNNUM)
                                 ADD   SYU-KOH3HKNFTNMONEY
                                                    TO  FTN-KOHI3-D
                                 IF  SYU-KOH3YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI3-D2 =              
                                            FTN-KOHI3-D2     +          
                                            SYU-KOH3COMPFTN  +          
                                            SYU-KOH3COMPYKZFTN          
                                   COMPUTE  FTN-KOHI3-D3 =              
                                            FTN-KOHI3-D3            +   
                                            SYU-KOH3COMPFTN-ENTANI  +   
                                            SYU-KOH3COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH3COMPFTN                
                                                    TO  FTN-KOHI3-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI3-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH3HKNFTNMONEY
                                                      TO  FTN-KOHI3-D-K
                                   IF  SYU-KOH3YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI3-D2-K =            
                                              FTN-KOHI3-D2-K   +          
                                              SYU-KOH3COMPFTN  +          
                                              SYU-KOH3COMPYKZFTN          
                                     COMPUTE  FTN-KOHI3-D3-K =          
                                              FTN-KOHI3-D3-K          +   
                                              SYU-KOH3COMPFTN-ENTANI  +   
                                              SYU-KOH3COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH3COMPFTN                
                                                      TO  FTN-KOHI3-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI3-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH4HKNNUM)
                                 ADD   SYU-KOH4HKNFTNMONEY
                                                    TO  FTN-KOHI3-D
                                 IF  SYU-KOH4YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI3-D2 =              
                                            FTN-KOHI3-D2     +          
                                            SYU-KOH4COMPFTN  +          
                                            SYU-KOH4COMPYKZFTN          
                                   COMPUTE  FTN-KOHI3-D3 =              
                                            FTN-KOHI3-D3            +   
                                            SYU-KOH4COMPFTN-ENTANI  +   
                                            SYU-KOH4COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH4COMPFTN                
                                                    TO  FTN-KOHI3-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI3-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH4HKNFTNMONEY
                                                      TO  FTN-KOHI3-D-K
                                   IF  SYU-KOH4YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI3-D2-K =            
                                              FTN-KOHI3-D2-K   +          
                                              SYU-KOH4COMPFTN  +          
                                              SYU-KOH4COMPYKZFTN          
                                     COMPUTE  FTN-KOHI3-D3-K =          
                                              FTN-KOHI3-D3-K          +   
                                              SYU-KOH4COMPFTN-ENTANI  +   
                                              SYU-KOH4COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH4COMPFTN                
                                                      TO  FTN-KOHI3-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI3-D3-K    
                                   END-IF                                
                                 END-IF
                       END-EVALUATE
                   END-IF
                   EVALUATE  TRUE
                       WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH1HKNNUM)
                             ADD   SYU-KOH1HKNFTNMONEY
                                                TO  FTN-KOHI3-M
                             IF  SYU-KOH1YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI3-M2 =                  
                                        FTN-KOHI3-M2     +              
                                        SYU-KOH1COMPFTN  +              
                                        SYU-KOH1COMPYKZFTN              
                               COMPUTE  FTN-KOHI3-M3 =                  
                                        FTN-KOHI3-M3            +       
                                        SYU-KOH1COMPFTN-ENTANI  +       
                                        SYU-KOH1COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH1COMPFTN                    
                                                TO  FTN-KOHI3-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI3-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH3HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH1SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH1SKJ-FTN
                                                TO  FTN-KOHI3-M
                                                    FTN-KOHI3-M2
                                                    FTN-KOHI3-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH2HKNNUM)
                             ADD   SYU-KOH2HKNFTNMONEY
                                                TO  FTN-KOHI3-M
                             IF  SYU-KOH2YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI3-M2 =                  
                                        FTN-KOHI3-M2     +              
                                        SYU-KOH2COMPFTN  +              
                                        SYU-KOH2COMPYKZFTN              
                               COMPUTE  FTN-KOHI3-M3 =                  
                                        FTN-KOHI3-M3            +       
                                        SYU-KOH2COMPFTN-ENTANI  +       
                                        SYU-KOH2COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH2COMPFTN                    
                                                TO  FTN-KOHI3-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI3-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH3HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH2SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH2SKJ-FTN
                                                TO  FTN-KOHI3-M
                                                    FTN-KOHI3-M2
                                                    FTN-KOHI3-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH3HKNNUM)
                             ADD   SYU-KOH3HKNFTNMONEY
                                                TO  FTN-KOHI3-M
                             IF  SYU-KOH3YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI3-M2 =                  
                                        FTN-KOHI3-M2     +              
                                        SYU-KOH3COMPFTN  +              
                                        SYU-KOH3COMPYKZFTN              
                               COMPUTE  FTN-KOHI3-M3 =                  
                                        FTN-KOHI3-M3            +       
                                        SYU-KOH3COMPFTN-ENTANI  +       
                                        SYU-KOH3COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH3COMPFTN                    
                                                TO  FTN-KOHI3-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI3-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH3HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH3SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH3SKJ-FTN
                                                TO  FTN-KOHI3-M
                                                    FTN-KOHI3-M2
                                                    FTN-KOHI3-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH4HKNNUM)
                             ADD   SYU-KOH4HKNFTNMONEY
                                                TO  FTN-KOHI3-M
                             IF  SYU-KOH4YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI3-M2 =                  
                                        FTN-KOHI3-M2     +              
                                        SYU-KOH4COMPFTN  +              
                                        SYU-KOH4COMPYKZFTN              
                               COMPUTE  FTN-KOHI3-M3 =                  
                                        FTN-KOHI3-M3            +       
                                        SYU-KOH4COMPFTN-ENTANI  +       
                                        SYU-KOH4COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH4COMPFTN                    
                                                TO  FTN-KOHI3-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI3-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH3HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH4SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH4SKJ-FTN
                                                TO  FTN-KOHI3-M
                                                    FTN-KOHI3-M2
                                                    FTN-KOHI3-M3
                             END-IF
                   END-EVALUATE
               END-IF
             END-IF
           END-IF.
      *    ¸øÈñ£´¥«¥¦¥ó¥È
           IF  (COMB-KOH4HKNNUM               NOT = SPACE)   AND
               (COMB-KOH4-GAIJGNKBN           =     "1")     
             IF  ( SYU-NYUGAIKBN              =   "2"   )
             OR  ((SYU-NYUGAIKBN              =   "1"  ) AND
                  (FLG-NYUGAI                 =    4   ))
               IF   ((LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM ) AND
                     (COMB-KOH4ID         =  WRK-COMB-KOH1ID))
               OR   ((LNK-SYU-KOH4HKNNUM  =  SYU-KOH2HKNNUM ) AND
                     (COMB-KOH4ID         =  WRK-COMB-KOH2ID))
               OR   ((LNK-SYU-KOH4HKNNUM  =  SYU-KOH3HKNNUM ) AND
                     (COMB-KOH4ID         =  WRK-COMB-KOH3ID))
               OR   ((LNK-SYU-KOH4HKNNUM  =  SYU-KOH4HKNNUM ) AND
                     (COMB-KOH4ID         =  WRK-COMB-KOH4ID))
      *
                   MOVE   SYU-SRYYMD(7:2)     TO    IDX2X
                   MOVE   4                   TO    IDX3
                   IF   (MAX-KIN-KOHI4-D    NOT =  ZERO)
                   OR   (MAX-KIN-KOHI4-M    NOT =  ZERO)
                       PERFORM   22021-KAISU-COUNT-SEC
                   ELSE
                       PERFORM   22022-KAISU-COUNT2-SEC
                   END-IF
      *
                   IF ((WK-1001-PREFNUM          =  07      ) AND
                       (LNK-SYU-KOH4HKNNUM       = "245"    ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200412" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH4HKNNUM  = "343" OR "342") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200507" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH4HKNNUM       = "344"    ) AND
                       (FLG-SANSHOU2        =   "2" OR "3"  ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200507" ))
                   OR ((WK-1001-PREFNUM          =  07      ) AND
                       (LNK-SYU-KOH4HKNNUM       = "246"    ) AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200604" ))
                   OR ((WK-1001-PREFNUM          =  11      ) AND
                       (LNK-SYU-KOH4HKNNUM  = "181" OR "182"
                                           OR "183" OR "281") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200704" ))
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH1HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH1COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH2HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH2HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH2COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH3HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH3HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH3COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH4HKNNUM
                         MOVE   SYU-JYO-HKNFTNMONEY
                                              TO  SYU-KOH4HKNFTNMONEY
                         MOVE   SYU-JYO-COMPFTN
                                              TO  SYU-KOH4COMPFTN
                         MOVE   SYU-JYO-COMPFTN-ENTANI
                                              TO  SYU-KOH4COMPFTN-ENTANI
                      END-IF
                   END-IF
      *
                   IF ((WK-1001-PREFNUM         =  33      ) AND
                       (LNK-SYU-KOH4HKNNUM      = "285"    ) AND
                       (LNK-SYU-SRYYMD(1:6)    >= "200110" ) AND
                       (LNK-SYU-SRYYMD(1:6)    <= "200609" ))
                   OR ((WK-1001-PREFNUM         =  13      ) AND
                       (LNK-SYU-KOH4HKNNUM      = "182"    ) AND
                       (LNK-SYU-SRYYMD(1:6)    >= "200610" ))
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH1HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH1HKNFTNMONEY
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH1COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH1COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH1COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH1COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH1COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH2HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH2HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH2HKNFTNMONEY
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH2COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH2COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH2COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH2COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH2COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH3HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH3HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH3HKNFTNMONEY
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH3COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH3COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH3COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH3COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH3COMPFTN-ENTANI
                        END-IF
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH4HKNNUM
                        IF  SYU-JYO-HKNFTNMONEY NOT = ZERO
                         COMPUTE  SYU-KOH4HKNFTNMONEY =
                                  SYU-JYO-HKNFTNMONEY -
                                  SYU-KOH4HKNFTNMONEY
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4HKNFTNMONEY
                        END-IF
                        IF  SYU-JYO-COMPFTN     NOT = ZERO
                         COMPUTE  SYU-KOH4COMPFTN     =
                                  SYU-JYO-COMPFTN     -
                                  SYU-KOH4COMPFTN
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4COMPFTN
                        END-IF
                        IF  SYU-JYO-COMPFTN-ENTANI  NOT = ZERO
                         COMPUTE  WK-TOKUSYU   = SYU-JYO-COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH4COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH4COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                        ELSE
                         MOVE  ZERO  TO  SYU-KOH4COMPFTN-ENTANI
                        END-IF
                      END-IF
                   END-IF
      *
                   IF (( WK-1001-PREFNUM         =  27        ) AND
                       ( LNK-SYU-KOH4HKNNUM  = "972" OR "974" ) AND
                       ((COMB-KOH1HKNNUM     = "180" OR "182"
                                            OR "186" OR "188") OR
                        (COMB-KOH2HKNNUM     = "180" OR "182"
                                            OR "186" OR "188") OR
                        (COMB-KOH3HKNNUM     = "180" OR "182"
                                            OR "186" OR "188")) AND
                       ( LNK-S01-SAIKEISAN-KBN   =   "1"      ) AND
                       ( LNK-SYU-DENPNUM     NOT =  ZERO      ) AND
                       ( LNK-SYU-DENPNUM      <  SYU-DENPNUM  ))
                   OR (( WK-1001-PREFNUM         =  34        ) AND
                       ( LNK-SYU-KOH4HKNNUM  = "972" OR "974" ) AND
                       ((COMB-KOH1HKNNUM     = "191" OR "291") OR
                        (COMB-KOH2HKNNUM     = "191" OR "291") OR
                        (COMB-KOH3HKNNUM     = "191" OR "291")) AND
                       ( LNK-S01-SAIKEISAN-KBN   =   "1"      ) AND
                       ( LNK-SYU-DENPNUM     NOT =  ZERO      ) AND
                       ( LNK-SYU-DENPNUM      <  SYU-DENPNUM  ))
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM
                         MOVE     ZERO   TO   SYU-KOH1HKNFTNMONEY
                                              SYU-KOH1COMPFTN
                                              SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH2HKNNUM
                         MOVE     ZERO   TO   SYU-KOH2HKNFTNMONEY
                                              SYU-KOH2COMPFTN
                                              SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH3HKNNUM
                         MOVE     ZERO   TO   SYU-KOH3HKNFTNMONEY
                                              SYU-KOH3COMPFTN
                                              SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH4HKNNUM
                         MOVE     ZERO   TO   SYU-KOH4HKNFTNMONEY
                                              SYU-KOH4COMPFTN
                                              SYU-KOH4COMPFTN-ENTANI
                      END-IF
                   END-IF
      *
                   IF  (WK-1001-PREFNUM          =  44      ) AND
                       (LNK-SYU-KOH4HKNNUM  = "183" OR "283") AND
                       (LNK-SYU-SRYYMD(1:6)     >= "200610" )
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM
                                               OR  SYU-KOH2HKNNUM
                                               OR  SYU-KOH3HKNNUM
                                               OR  SYU-KOH4HKNNUM
                         ADD   SYU-JYO-HKNFTNMONEY
                                        TO  WK-OITA-HKNFTNMONEY
                         ADD   SYU-JYO-COMPFTN
                                        TO  WK-OITA-CYOFTN
                      END-IF
                   END-IF
      *
                   IF        LNK-SYU-SRYYMD(7:2)   =  SYU-SRYYMD(7:2)
                       EVALUATE  TRUE
                           WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH1HKNNUM)
                                 ADD   SYU-KOH1HKNFTNMONEY
                                                    TO  FTN-KOHI4-D
                                 IF  SYU-KOH1YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI4-D2 =              
                                            FTN-KOHI4-D2     +          
                                            SYU-KOH1COMPFTN  +          
                                            SYU-KOH1COMPYKZFTN          
                                   COMPUTE  FTN-KOHI4-D3 =              
                                            FTN-KOHI4-D3            +   
                                            SYU-KOH1COMPFTN-ENTANI  +   
                                            SYU-KOH1COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH1COMPFTN                
                                                    TO  FTN-KOHI4-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI4-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH1HKNFTNMONEY
                                                      TO  FTN-KOHI4-D-K
                                   IF  SYU-KOH1YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI4-D2-K =            
                                              FTN-KOHI4-D2-K   +          
                                              SYU-KOH1COMPFTN  +          
                                              SYU-KOH1COMPYKZFTN          
                                     COMPUTE  FTN-KOHI4-D3-K =          
                                              FTN-KOHI4-D3-K          +   
                                              SYU-KOH1COMPFTN-ENTANI  +   
                                              SYU-KOH1COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH1COMPFTN                
                                                      TO  FTN-KOHI4-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI4-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH2HKNNUM)
                                 ADD   SYU-KOH2HKNFTNMONEY
                                                    TO  FTN-KOHI4-D
                                 IF  SYU-KOH2YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI4-D2 =              
                                            FTN-KOHI4-D2     +          
                                            SYU-KOH2COMPFTN  +          
                                            SYU-KOH2COMPYKZFTN          
                                   COMPUTE  FTN-KOHI4-D3 =              
                                            FTN-KOHI4-D3            +   
                                            SYU-KOH2COMPFTN-ENTANI  +   
                                            SYU-KOH2COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH2COMPFTN                
                                                    TO  FTN-KOHI4-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI4-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH2HKNFTNMONEY
                                                      TO  FTN-KOHI4-D-K
                                   IF  SYU-KOH2YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI4-D2-K =            
                                              FTN-KOHI4-D2-K   +          
                                              SYU-KOH2COMPFTN  +          
                                              SYU-KOH2COMPYKZFTN          
                                     COMPUTE  FTN-KOHI4-D3-K =          
                                              FTN-KOHI4-D3-K          +   
                                              SYU-KOH2COMPFTN-ENTANI  +   
                                              SYU-KOH2COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH2COMPFTN                
                                                      TO  FTN-KOHI4-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI4-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH3HKNNUM)
                                 ADD   SYU-KOH3HKNFTNMONEY
                                                    TO  FTN-KOHI4-D
                                 IF  SYU-KOH3YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI4-D2 =              
                                            FTN-KOHI4-D2     +          
                                            SYU-KOH3COMPFTN  +          
                                            SYU-KOH3COMPYKZFTN          
                                   COMPUTE  FTN-KOHI4-D3 =              
                                            FTN-KOHI4-D3            +   
                                            SYU-KOH3COMPFTN-ENTANI  +   
                                            SYU-KOH3COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH3COMPFTN                
                                                    TO  FTN-KOHI4-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI4-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH3HKNFTNMONEY
                                                      TO  FTN-KOHI4-D-K
                                   IF  SYU-KOH3YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI4-D2-K =            
                                              FTN-KOHI4-D2-K   +          
                                              SYU-KOH3COMPFTN  +          
                                              SYU-KOH3COMPYKZFTN          
                                     COMPUTE  FTN-KOHI4-D3-K =          
                                              FTN-KOHI4-D3-K          +   
                                              SYU-KOH3COMPFTN-ENTANI  +   
                                              SYU-KOH3COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH3COMPFTN                
                                                      TO  FTN-KOHI4-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI4-D3-K    
                                   END-IF                                
                                 END-IF
                           WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH4HKNNUM)
                                 ADD   SYU-KOH4HKNFTNMONEY
                                                    TO  FTN-KOHI4-D
                                 IF  SYU-KOH4YKZFTNKBN   =    "2"       
                                   COMPUTE  FTN-KOHI4-D2 =              
                                            FTN-KOHI4-D2     +          
                                            SYU-KOH4COMPFTN  +          
                                            SYU-KOH4COMPYKZFTN          
                                   COMPUTE  FTN-KOHI4-D3 =              
                                            FTN-KOHI4-D3            +   
                                            SYU-KOH4COMPFTN-ENTANI  +   
                                            SYU-KOH4COMPYKZFTN      +   
                                            0.9
                                 ELSE                                   
                                   ADD   SYU-KOH4COMPFTN                
                                                    TO  FTN-KOHI4-D2    
                                   COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                                   ADD   WK-ENTANI  TO  FTN-KOHI4-D3    
                                 END-IF                                 
                                 IF  ((LNK-SYU-GRP-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-GRP-DENPNUM
                                                 =  SYU-GRP-DENPNUM))
                                 OR  ((LNK-SYU-FUKU-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-FUKU-DENPNUM
                                                 =  SYU-FUKU-DENPNUM))
                                 OR  ((LNK-SYU-DOUJI-DENPNUM
                                                       NOT =  ZERO ) AND
                                      (LNK-SYU-DOUJI-DENPNUM
                                                 =  SYU-DOUJI-DENPNUM))
                                 OR  ( FLG-GRPNYURYOKU     =  1       )
                                   ADD   SYU-KOH4HKNFTNMONEY
                                                      TO  FTN-KOHI4-D-K
                                   IF  SYU-KOH4YKZFTNKBN   =    "2"       
                                     COMPUTE  FTN-KOHI4-D2-K =            
                                              FTN-KOHI4-D2-K   +          
                                              SYU-KOH4COMPFTN  +          
                                              SYU-KOH4COMPYKZFTN          
                                     COMPUTE  FTN-KOHI4-D3-K =            
                                              FTN-KOHI4-D3-K          +   
                                              SYU-KOH4COMPFTN-ENTANI  +   
                                              SYU-KOH4COMPYKZFTN      +   
                                              0.9
                                   ELSE                                   
                                     ADD   SYU-KOH4COMPFTN                
                                                      TO  FTN-KOHI4-D2-K    
                                     COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                                     ADD   WK-ENTANI  TO  FTN-KOHI4-D3-K    
                                   END-IF                                
                                 END-IF
                       END-EVALUATE
                   END-IF
                   EVALUATE  TRUE
                       WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH1HKNNUM)
                             ADD   SYU-KOH1HKNFTNMONEY
                                                TO  FTN-KOHI4-M
                             IF  SYU-KOH1YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI4-M2 =                  
                                        FTN-KOHI4-M2     +              
                                        SYU-KOH1COMPFTN  +              
                                        SYU-KOH1COMPYKZFTN              
                               COMPUTE  FTN-KOHI4-M3 =                  
                                        FTN-KOHI4-M3            +       
                                        SYU-KOH1COMPFTN-ENTANI  +       
                                        SYU-KOH1COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH1COMPFTN                    
                                                TO  FTN-KOHI4-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI4-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH4HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH1SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH1SKJ-FTN
                                                TO  FTN-KOHI4-M
                                                    FTN-KOHI4-M2
                                                    FTN-KOHI4-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH2HKNNUM)
                             ADD   SYU-KOH2HKNFTNMONEY
                                                TO  FTN-KOHI4-M
                             IF  SYU-KOH2YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI4-M2 =                  
                                        FTN-KOHI4-M2     +              
                                        SYU-KOH2COMPFTN  +              
                                        SYU-KOH2COMPYKZFTN              
                               COMPUTE  FTN-KOHI4-M3 =                  
                                        FTN-KOHI4-M3            +       
                                        SYU-KOH2COMPFTN-ENTANI  +       
                                        SYU-KOH2COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH2COMPFTN                    
                                                TO  FTN-KOHI4-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI4-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH4HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH2SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH2SKJ-FTN
                                                TO  FTN-KOHI4-M
                                                    FTN-KOHI4-M2
                                                    FTN-KOHI4-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH3HKNNUM)
                             ADD   SYU-KOH3HKNFTNMONEY
                                                TO  FTN-KOHI4-M
                             IF  SYU-KOH3YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI4-M2 =                  
                                        FTN-KOHI4-M2     +              
                                        SYU-KOH3COMPFTN  +              
                                        SYU-KOH3COMPYKZFTN              
                               COMPUTE  FTN-KOHI4-M3 =                  
                                        FTN-KOHI4-M3            +       
                                        SYU-KOH3COMPFTN-ENTANI  +       
                                        SYU-KOH3COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH3COMPFTN                    
                                                TO  FTN-KOHI4-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI4-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH4HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH3SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH3SKJ-FTN
                                                TO  FTN-KOHI4-M
                                                    FTN-KOHI4-M2
                                                    FTN-KOHI4-M3
                             END-IF
                       WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH4HKNNUM)
                             ADD   SYU-KOH4HKNFTNMONEY
                                                TO  FTN-KOHI4-M
                             IF  SYU-KOH4YKZFTNKBN   =    "2"           
                               COMPUTE  FTN-KOHI4-M2 =                  
                                        FTN-KOHI4-M2     +              
                                        SYU-KOH4COMPFTN  +              
                                        SYU-KOH4COMPYKZFTN              
                               COMPUTE  FTN-KOHI4-M3 =                  
                                        FTN-KOHI4-M3            +       
                                        SYU-KOH4COMPFTN-ENTANI  +       
                                        SYU-KOH4COMPYKZFTN      +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH4COMPFTN                    
                                                TO  FTN-KOHI4-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI4-M3        
                             END-IF                                     
                             IF  (LNK-SYU-SRYYMD(1:6)  >=  "200702")
                             AND (WK-1001-PREFNUM       =   07     )
                             AND (LNK-SYU-KOH4HKNNUM    =  "281"   )
                             AND (SYU-NYUGAIKBN         =  "1"     )
                             AND (SYU-KOH4SKJ-FTNKBN    =  "2"     )
                                ADD   SYU-KOH4SKJ-FTN
                                                TO  FTN-KOHI4-M
                                                    FTN-KOHI4-M2
                                                    FTN-KOHI4-M3
                             END-IF
                   END-EVALUATE
               END-IF
             END-IF
           END-IF.
      *
       2202-SYUNOU-COUNT-EXT.
           EXIT.
      *
      ******************************************************************
      *    ²ó¿ô¥«¥¦¥ó¥È
      ******************************************************************
       22021-KAISU-COUNT-SEC           SECTION.
      *
           MOVE     ZERO     TO     FLG-NOCOUNT.
           IF   FLG-GRPNYURYOKU   =   ZERO
               PERFORM    22023-KAISU-SELECT-SEC
           END-IF.
      *
           IF     FLG-NOCOUNT          =  ZERO
           AND    FLG-GRPNYURYOKU      =  ZERO
      *
             IF    KOHI-TBL(IDX2 IDX3)  =  ZERO
             AND   SYU-HKNTEN(16)   NOT =  ZERO
      *
               IF   SYU-GRP-DENPNUM   NOT  =  ZERO
      *
                 IF  (WK-TAIHI-DENPNUMXX(IDX3) =  SYU-GRP-DENPNUM)
                 AND (WK-TAIHI-DENPNUMXX(IDX3)   NOT = ZERO      )
                     CONTINUE
                 ELSE
                     ADD   1    TO   KOHI-TBL(IDX2 IDX3)
                     IF   SYU-SRYYMD  NOT =  LNK-SYU-SRYYMD
                         ADD   1    TO   KOHI-TBL(32 IDX3)
                     END-IF
                     MOVE  SYU-GRP-DENPNUM
                                TO   WK-TAIHI-DENPNUMXX(IDX3)
                 END-IF
      *
               ELSE
      *          ¤Þ¤È¤áÆþÎÏ°Ê³°¤Ç¡¢·ÑÂ³Åù¤ÇÆþÎÏ¤·¤¿¤â¤Î¤Ï¥«¥¦¥ó¥È¤·¤Ê¤¤
                 IF  ((SYU-DOUJI-DENPNUM      NOT =  ZERO)  AND
                      (LNK-SYU-DOUJI-DENPNUM  NOT =  ZERO)  AND
                      (SYU-DOUJI-DENPNUM =
                                    LNK-SYU-DOUJI-DENPNUM))
                 OR  ((SYU-FUKU-DENPNUM       NOT =  ZERO)  AND
                      (LNK-SYU-FUKU-DENPNUM   NOT =  ZERO)  AND
                      (SYU-FUKU-DENPNUM  =
                                    LNK-SYU-FUKU-DENPNUM ))
                   CONTINUE
                 ELSE
                   IF  (SYU-DOUJI-DENPNUM      NOT =  ZERO)
                   OR  (SYU-FUKU-DENPNUM       NOT =  ZERO)
                         IF  (SYU-DOUJI-DENPNUM      NOT =  ZERO   )
                           IF  (WK-TAIHI-DENPNUMXX(IDX3)     =
                                                SYU-DOUJI-DENPNUM  )
                           AND (WK-TAIHI-DENPNUMXX(IDX3) NOT = ZERO)
                               CONTINUE
                           ELSE
                               ADD   1    TO  KOHI-TBL(IDX2 IDX3)
                               IF   SYU-SRYYMD  NOT =  LNK-SYU-SRYYMD
                                   ADD   1    TO   KOHI-TBL(32 IDX3)
                               END-IF
                               MOVE  SYU-DOUJI-DENPNUM
                                          TO  WK-TAIHI-DENPNUMXX(IDX3)
                           END-IF
                         END-IF
                         IF  (SYU-FUKU-DENPNUM       NOT =  ZERO   )
                           IF  (WK-TAIHI-DENPNUMXX(IDX3)     =
                                                SYU-FUKU-DENPNUM   )
                           AND (WK-TAIHI-DENPNUMXX(IDX3) NOT = ZERO)
                               CONTINUE
                           ELSE
                               ADD   1    TO  KOHI-TBL(IDX2 IDX3)
                               IF   SYU-SRYYMD  NOT =  LNK-SYU-SRYYMD
                                   ADD   1    TO   KOHI-TBL(32 IDX3)
                               END-IF
                               MOVE  SYU-FUKU-DENPNUM
                                          TO  WK-TAIHI-DENPNUMXX(IDX3)
                           END-IF
                         END-IF
                   ELSE
                               ADD   1    TO   KOHI-TBL(IDX2 IDX3)
                               IF   SYU-SRYYMD  NOT =  LNK-SYU-SRYYMD
                                   ADD   1    TO   KOHI-TBL(32 IDX3)
                               END-IF
                   END-IF
      *
                 END-IF
      *
               END-IF
      *
             ELSE
      *
               IF  SYU-HKNTEN(16)   NOT =  ZERO
      *
                 IF   SYU-GRP-DENPNUM   NOT  =  ZERO
      *
                   IF  (WK-TAIHI-DENPNUMXX(IDX3) =  SYU-GRP-DENPNUM)
                   AND (WK-TAIHI-DENPNUMXX(IDX3)   NOT = ZERO      )
                       CONTINUE
                   ELSE
                       ADD   1    TO   KOHI-TBL(IDX2 IDX3)
                       MOVE  SYU-GRP-DENPNUM
                                  TO   WK-TAIHI-DENPNUMXX(IDX3)
                   END-IF
      *
                 ELSE
      *            ¤Þ¤È¤áÆþÎÏ°Ê³°¤Ç¡¢·ÑÂ³Åù¤ÇÆþÎÏ¤·¤¿¤â¤Î¤Ï¥«¥¦¥ó¥È¤·¤Ê¤¤
                   IF  ((SYU-DOUJI-DENPNUM      NOT =  ZERO)  AND
                        (LNK-SYU-DOUJI-DENPNUM  NOT =  ZERO)  AND
                        (SYU-DOUJI-DENPNUM =
                                      LNK-SYU-DOUJI-DENPNUM))
                   OR  ((SYU-FUKU-DENPNUM       NOT =  ZERO)  AND
                        (LNK-SYU-FUKU-DENPNUM   NOT =  ZERO)  AND
                        (SYU-FUKU-DENPNUM  =
                                      LNK-SYU-FUKU-DENPNUM ))
                     CONTINUE
                   ELSE
                     IF  (SYU-DOUJI-DENPNUM      NOT =  ZERO)
                     OR  (SYU-FUKU-DENPNUM       NOT =  ZERO)
                           IF  (SYU-DOUJI-DENPNUM      NOT =  ZERO   )
                             IF  (WK-TAIHI-DENPNUMXX(IDX3)     =
                                                  SYU-DOUJI-DENPNUM  )
                             AND (WK-TAIHI-DENPNUMXX(IDX3) NOT = ZERO)
                                 CONTINUE
                             ELSE
                                 ADD   1    TO  KOHI-TBL(IDX2 IDX3)
                                 MOVE  SYU-DOUJI-DENPNUM
                                            TO  WK-TAIHI-DENPNUMXX(IDX3)
                             END-IF
                           END-IF
                           IF  (SYU-FUKU-DENPNUM       NOT =  ZERO   )
                             IF  (WK-TAIHI-DENPNUMXX(IDX3)     =
                                                  SYU-FUKU-DENPNUM   )
                             AND (WK-TAIHI-DENPNUMXX(IDX3) NOT = ZERO)
                                 CONTINUE
                             ELSE
                                 ADD   1    TO  KOHI-TBL(IDX2 IDX3)
                                 MOVE  SYU-FUKU-DENPNUM
                                            TO  WK-TAIHI-DENPNUMXX(IDX3)
                             END-IF
                           END-IF
                     ELSE
                                 ADD   1    TO   KOHI-TBL(IDX2 IDX3)
                     END-IF
      *
                   END-IF
      *
                 END-IF
      *
               END-IF
      *
             END-IF
      *
           END-IF.
      *
       22021-KAISU-COUNT-EXT.
           EXIT.
      *
      ******************************************************************
      *    ²ó¿ô¥«¥¦¥ó¥ÈÈ½Äê£²
      ******************************************************************
       22022-KAISU-COUNT2-SEC           SECTION.
      *
           MOVE     ZERO     TO     FLG-NOCOUNT.
           IF   FLG-GRPNYURYOKU   =   ZERO
               PERFORM    22023-KAISU-SELECT-SEC
           END-IF.
      *
           IF     FLG-NOCOUNT          =  ZERO
           AND    FLG-GRPNYURYOKU      =  ZERO
      *
             IF  SYU-HKNTEN(16)   NOT =  ZERO
      *
               IF   SYU-GRP-DENPNUM   NOT  =  ZERO
      *
                 IF  (WK-TAIHI-DENPNUMXX(IDX3) =  SYU-GRP-DENPNUM)
                 AND (WK-TAIHI-DENPNUMXX(IDX3)   NOT = ZERO      )
                     CONTINUE
                 ELSE
                     ADD   1    TO   KOHI-TBL(IDX2 IDX3)
                     ADD   1    TO   KOHI-TBL(32 IDX3)
                     MOVE  SYU-GRP-DENPNUM
                                TO   WK-TAIHI-DENPNUMXX(IDX3)
                 END-IF
      *
               ELSE
      *          ¤Þ¤È¤áÆþÎÏ°Ê³°¤Ç¡¢·ÑÂ³Åù¤ÇÆþÎÏ¤·¤¿¤â¤Î¤Ï¥«¥¦¥ó¥È¤·¤Ê¤¤
                 IF  ((SYU-DOUJI-DENPNUM      NOT =  ZERO)  AND
                      (LNK-SYU-DOUJI-DENPNUM  NOT =  ZERO)  AND
                      (SYU-DOUJI-DENPNUM =
                                    LNK-SYU-DOUJI-DENPNUM))
                 OR  ((SYU-FUKU-DENPNUM       NOT =  ZERO)  AND
                      (LNK-SYU-FUKU-DENPNUM   NOT =  ZERO)  AND
                      (SYU-FUKU-DENPNUM  =
                                    LNK-SYU-FUKU-DENPNUM ))
                   CONTINUE
                 ELSE
                   IF  (SYU-DOUJI-DENPNUM      NOT =  ZERO)
                   OR  (SYU-FUKU-DENPNUM       NOT =  ZERO)
                         IF  (SYU-DOUJI-DENPNUM      NOT =  ZERO   )
                           IF  (WK-TAIHI-DENPNUMXX(IDX3)     =
                                                SYU-DOUJI-DENPNUM  )
                           AND (WK-TAIHI-DENPNUMXX(IDX3) NOT = ZERO)
                               CONTINUE
                           ELSE
                               ADD   1    TO  KOHI-TBL(IDX2 IDX3)
                               ADD   1    TO  KOHI-TBL(32 IDX3)
                               MOVE  SYU-DOUJI-DENPNUM
                                          TO  WK-TAIHI-DENPNUMXX(IDX3)
                           END-IF
                         END-IF
                         IF  (SYU-FUKU-DENPNUM       NOT =  ZERO   )
                           IF  (WK-TAIHI-DENPNUMXX(IDX3)     =
                                                SYU-FUKU-DENPNUM   )
                           AND (WK-TAIHI-DENPNUMXX(IDX3) NOT = ZERO)
                               CONTINUE
                           ELSE
                               ADD   1    TO  KOHI-TBL(IDX2 IDX3)
                               ADD   1    TO  KOHI-TBL(32 IDX3)
                               MOVE  SYU-FUKU-DENPNUM
                                          TO  WK-TAIHI-DENPNUMXX(IDX3)
                           END-IF
                         END-IF
                   ELSE
                               ADD   1    TO   KOHI-TBL(IDX2 IDX3)
                               ADD   1    TO   KOHI-TBL(32 IDX3)
                   END-IF
      *
                 END-IF
      *
               END-IF
      *
             END-IF
      *
           END-IF.
      *
       22022-KAISU-COUNT2-EXT.
           EXIT.
      *
      ******************************************************************
      *    ²ó¿ô¥«¥¦¥ó¥ÈÈ½Äê
      ******************************************************************
       22023-KAISU-SELECT-SEC           SECTION.
      *
           EVALUATE  IDX3
               WHEN    1 
                  EVALUATE  TRUE
                       WHEN  LNK-SYU-KOH1HKNNUM = SYU-KOH1HKNNUM
                         IF  SYU-KOH1HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH1HKNNUM = SYU-KOH2HKNNUM
                         IF  SYU-KOH2HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH1HKNNUM = SYU-KOH3HKNNUM
                         IF  SYU-KOH3HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH1HKNNUM = SYU-KOH4HKNNUM
                         IF  SYU-KOH4HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                  END-EVALUATE
               WHEN    2 
                  EVALUATE  TRUE
                       WHEN  LNK-SYU-KOH2HKNNUM = SYU-KOH1HKNNUM
                         IF  SYU-KOH1HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH2HKNNUM = SYU-KOH2HKNNUM
                         IF  SYU-KOH2HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH2HKNNUM = SYU-KOH3HKNNUM
                         IF  SYU-KOH3HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH2HKNNUM = SYU-KOH4HKNNUM
                         IF  SYU-KOH4HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                  END-EVALUATE
               WHEN    3 
                  EVALUATE  TRUE
                       WHEN  LNK-SYU-KOH3HKNNUM = SYU-KOH1HKNNUM
                         IF  SYU-KOH1HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH3HKNNUM = SYU-KOH2HKNNUM
                         IF  SYU-KOH2HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH3HKNNUM = SYU-KOH3HKNNUM
                         IF  SYU-KOH3HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH3HKNNUM = SYU-KOH4HKNNUM
                         IF  SYU-KOH4HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                  END-EVALUATE
               WHEN    4 
                  EVALUATE  TRUE
                       WHEN  LNK-SYU-KOH4HKNNUM = SYU-KOH1HKNNUM
                         IF  SYU-KOH1HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH4HKNNUM = SYU-KOH2HKNNUM
                         IF  SYU-KOH2HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH4HKNNUM = SYU-KOH3HKNNUM
                         IF  SYU-KOH3HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                       WHEN  LNK-SYU-KOH4HKNNUM = SYU-KOH4HKNNUM
                         IF  SYU-KOH4HKNFTNMONEY = ZERO
                             MOVE   1    TO    FLG-NOCOUNT
                         END-IF
                  END-EVALUATE
           END-EVALUATE.
      *
       22023-KAISU-SELECT-EXT.
           EXIT.
      *
      ******************************************************************
      *        ´µ¼Ô¸øÈñÉéÃ´¡Ê¸º³Û¡Ê³ä¹ç¡¦¶â³Û¡Ë¡Ë¼è¹þ¤ß½èÍý            *
      ******************************************************************
       2203-KOHFTN-KINGAKU-GEN2-SEC           SECTION.
      *
           IF  FLG-GENBUTU70   NOT =   ZERO
             IF   MAX-KIN-SYU-M     NOT =  ZERO
                  MOVE     ZERO   TO   FLG-KOUGAKUCYO-GEN70
                  EVALUATE   WK-GENGAKUKBN
                    WHEN          1
                       IF  WK-GENGAKU-RITU = 100
                         MOVE   ZERO   TO   MAX-KIN-SYU-M
                       ELSE
                         COMPUTE  WK-GENGAKU-RITUX =
                                  100 - WK-GENGAKU-RITU
                         COMPUTE  MAX-KIN-SYU-M       =
                                  MAX-KIN-SYU-M       *
                                  WK-GENGAKU-RITUX    /
                                  100
                       END-IF
                    WHEN          2
                       CONTINUE
                    WHEN          3
                    WHEN          4
                       MOVE   ZERO   TO   MAX-KIN-SYU-M
                  END-EVALUATE
                  COMPUTE  MAX-KIN-SYU-M =
                           MAX-KIN-SYU-M +  5
                  COMPUTE  MAX-KIN-SYU-M =
                           MAX-KIN-SYU-M /  10
                  COMPUTE  MAX-KIN-SYU-M =
                           MAX-KIN-SYU-M *  10
             END-IF
           END-IF
      *
           .
      *
       2203-KOHFTN-KINGAKU-GEN2-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÊÝ¸±¡¦¸øÈñ¤ÎÉéÃ´¶â·×»»
      ******************************************************************
       230-FTNKIN-SEC           SECTION.
      *
      *    »»Äê²ÄÇ½¥Õ¥é¥°¤Î½é´ü²½
           MOVE     SPACE                  TO     FLG-SANTEI1
                                                  FLG-SANTEI2
                                                  FLG-SANTEI3
                                                  FLG-SANTEI4.
      *
      *    ²áµî¤Î´µ¼ÔÉéÃ´²ó¿ô¤È¾å¸Â²ó¿ô¤ÎÈæ³Ó
           IF       MAX-KAI-KOHI1-D        NOT =  ZERO
               IF   KAISU-KOHI1-D          >=     MAX-KAI-KOHI1-D
                    MOVE     "1"           TO     FLG-SANTEI1
               END-IF
           END-IF.
           IF       MAX-KAI-KOHI2-D        NOT =  ZERO
               IF   KAISU-KOHI2-D          >=     MAX-KAI-KOHI2-D
                    MOVE     "1"           TO     FLG-SANTEI2
               END-IF
           END-IF.
           IF       MAX-KAI-KOHI3-D        NOT =  ZERO
               IF   KAISU-KOHI3-D          >=     MAX-KAI-KOHI3-D
                    MOVE     "1"           TO     FLG-SANTEI3
               END-IF
           END-IF.
           IF       MAX-KAI-KOHI4-D        NOT =  ZERO
               IF   KAISU-KOHI4-D          >=     MAX-KAI-KOHI4-D
                    MOVE     "1"           TO     FLG-SANTEI4
               END-IF
           END-IF.
           IF       MAX-KAI-KOHI1-M        NOT =  ZERO
               IF   KAISU-KOHI1-M          >=     MAX-KAI-KOHI1-M
                    MOVE     "1"           TO     FLG-SANTEI1
               END-IF
           END-IF.
           IF       MAX-KAI-KOHI2-M        NOT =  ZERO
               IF   KAISU-KOHI2-M          >=     MAX-KAI-KOHI2-M
                    MOVE     "1"           TO     FLG-SANTEI2
               END-IF
           END-IF.
           IF       MAX-KAI-KOHI3-M        NOT =  ZERO
               IF   KAISU-KOHI3-M          >=     MAX-KAI-KOHI3-M
                    MOVE     "1"           TO     FLG-SANTEI3
               END-IF
           END-IF.
           IF       MAX-KAI-KOHI4-M        NOT =  ZERO
               IF   KAISU-KOHI4-M          >=     MAX-KAI-KOHI4-M
                    MOVE     "1"           TO     FLG-SANTEI4
               END-IF
           END-IF.
      *
      *    ²áµî¤Î´µ¼ÔÊÝ¸±ÉéÃ´Îß·×³Û¤È¾å¸Â¶â³Û¤ÎÈæ³Ó
      *
      ***     IF       MAX-KIN-KOHI1-D        NOT =  ZERO
      ***         IF   FTN-KOHI1-D            >=     MAX-KIN-KOHI1-D
      ***              MOVE     "1"           TO     FLG-SANTEI1
      ***         END-IF
      ***     END-IF.
      ***     IF       MAX-KIN-KOHI2-D        NOT =  ZERO
      ***         IF   FTN-KOHI2-D            >=     MAX-KIN-KOHI2-D
      ***              MOVE     "1"           TO     FLG-SANTEI2
      ***         END-IF
      ***     END-IF.
      ***     IF       MAX-KIN-KOHI3-D        NOT =  ZERO
      ***         IF   FTN-KOHI3-D            >=     MAX-KIN-KOHI3-D
      ***              MOVE     "1"           TO     FLG-SANTEI3
      ***         END-IF
      ***     END-IF.
      ***     IF       MAX-KIN-KOHI4-D        NOT =  ZERO
      ***         IF   FTN-KOHI4-D            >=     MAX-KIN-KOHI4-D
      ***              MOVE     "1"           TO     FLG-SANTEI4
      ***         END-IF
      ***     END-IF.
      *
      *----(01.00.23) LINE ADD START --------------------------
      *Ê¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ
      *¹âÎð¼Ô¤ÎºßÂðÁí¹ç¡¦ºßÂðËö´ü»»Äê´µ¼Ô
      *
           IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1")
           OR  (FLG-SYUHEN                   =   "1")
           OR  (FLG-KOHININBARAI             =    1 )
           OR  (FLG-GENBUTU70            NOT =  ZERO)
               IF  MAX-KIN-SYU-M           NOT =  ZERO
                   IF  FTN-SYU-M           >=     MAX-KIN-SYU-M
                       MOVE  "1"           TO     FLG-SANTEI-SYU
                   END-IF
               END-IF
           END-IF.
      *----(01.00.23) LINE ADD END   --------------------------
      ***     IF       MAX-KIN-KOHI1-M        NOT =  ZERO
      ***         IF   FTN-KOHI1-M            >=     MAX-KIN-KOHI1-M
      ***              MOVE     "1"           TO     FLG-SANTEI1
      ***         END-IF
      ***     END-IF.
      ***     IF       MAX-KIN-KOHI2-M        NOT =  ZERO
      ***         IF   FTN-KOHI2-M            >=     MAX-KIN-KOHI2-M
      ***              MOVE     "1"           TO     FLG-SANTEI2
      ***         END-IF
      ***     END-IF.
      ***     IF       MAX-KIN-KOHI3-M        NOT =  ZERO
      ***         IF   FTN-KOHI3-M            >=     MAX-KIN-KOHI3-M
      ***              MOVE     "1"           TO     FLG-SANTEI3
      ***         END-IF
      ***     END-IF.
      ***     IF       MAX-KIN-KOHI4-M        NOT =  ZERO
      ***         IF   FTN-KOHI4-M            >=     MAX-KIN-KOHI4-M
      ***              MOVE     "1"           TO     FLG-SANTEI4
      ***         END-IF
      ***     END-IF.
      *
      *    ÊÝ¸±¡Ê¸øÈñ¡Ë¥Æ¡¼¥Ö¥ë¤Î´µ¼ÔÉéÃ´¾å¸Â³Û¤ÈÅö·î¤Î´µ¼ÔÉéÃ´Îß·×³Û
      *    ¤«¤é»»Äê²ÄÇ½»Ä³Û¤ò»»½Ð¤¹¤ë
      *
           IF  FLG-SANTEI1               =     "1"
               MOVE     ZERO             TO    ZAN-KOHI1-D
                                               ZAN-KOHI1-D2             
                                               ZAN-KOHI1-D3             
           ELSE
               IF (COMB-KOH1HKNNUM           NOT = SPACE)    AND
                  (COMB-KOH1-GAIJGNKBN       =     "1")
                   IF       MAX-KIN-KOHI1-D       NOT = ZERO
                       IF   FTN-KOHI1-D           >=  MAX-KIN-KOHI1-D
                            MOVE     ZERO         TO  ZAN-KOHI1-D
                       ELSE
                            COMPUTE  ZAN-KOHI1-D  =   MAX-KIN-KOHI1-D -
                                                      FTN-KOHI1-D
                       END-IF
                       IF  COMB-KOH1HKNNUM    =   "027"                 
                            MOVE     ZERO         TO  ZAN-KOHI1-D2      
                       ELSE                                             
                         IF   FTN-KOHI1-D2        >=  MAX-KIN-KOHI1-D   
                            MOVE     ZERO         TO  ZAN-KOHI1-D2      
                         ELSE                                           
                            COMPUTE  ZAN-KOHI1-D2 =   MAX-KIN-KOHI1-D - 
                                                      FTN-KOHI1-D2      
                         END-IF                                         
                       END-IF                                           
                       IF  COMB-KOH1HKNNUM    =   "027"                 
                            MOVE     ZERO         TO  ZAN-KOHI1-D3      
                       ELSE                                             
                         IF   FTN-KOHI1-D3        >=  MAX-KIN-KOHI1-D   
                            MOVE     ZERO         TO  ZAN-KOHI1-D3      
                         ELSE                                           
                            COMPUTE  ZAN-KOHI1-D3 =   MAX-KIN-KOHI1-D - 
                                                      FTN-KOHI1-D3      
                         END-IF                                         
                       END-IF                       
                   END-IF
               END-IF
           END-IF.
           IF  FLG-SANTEI2               =     "1"
               MOVE     ZERO             TO    ZAN-KOHI2-D
                                               ZAN-KOHI2-D2             
                                               ZAN-KOHI2-D3             
           ELSE
               IF (COMB-KOH2HKNNUM           NOT = SPACE)    AND
                  (COMB-KOH2-GAIJGNKBN       =     "1")
                   IF       MAX-KIN-KOHI2-D       NOT = ZERO
                       IF   FTN-KOHI2-D           >=  MAX-KIN-KOHI2-D
                            MOVE     ZERO         TO  ZAN-KOHI2-D
                       ELSE
                            COMPUTE  ZAN-KOHI2-D  =   MAX-KIN-KOHI2-D -
                                                      FTN-KOHI2-D
                       END-IF
                       IF   FTN-KOHI2-D2          >=  MAX-KIN-KOHI2-D   
                            MOVE     ZERO         TO  ZAN-KOHI2-D2      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI2-D2 =   MAX-KIN-KOHI2-D - 
                                                      FTN-KOHI2-D2      
                       END-IF                                           
                       IF   FTN-KOHI2-D3          >=  MAX-KIN-KOHI2-D   
                            MOVE     ZERO         TO  ZAN-KOHI2-D3      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI2-D3 =   MAX-KIN-KOHI2-D - 
                                                      FTN-KOHI2-D3      
                       END-IF              
                   END-IF
               END-IF
           END-IF.
           IF  FLG-SANTEI3               =     "1"
               MOVE     ZERO             TO    ZAN-KOHI3-D
                                               ZAN-KOHI3-D2             
                                               ZAN-KOHI3-D3             
           ELSE
               IF (COMB-KOH3HKNNUM           NOT = SPACE)    AND
                  (COMB-KOH3-GAIJGNKBN       =     "1")
                   IF       MAX-KIN-KOHI3-D       NOT = ZERO
                       IF   FTN-KOHI3-D           >=  MAX-KIN-KOHI3-D
                            MOVE     ZERO         TO  ZAN-KOHI3-D
                       ELSE
                            COMPUTE  ZAN-KOHI3-D  =   MAX-KIN-KOHI3-D -
                                                      FTN-KOHI3-D
                       END-IF
                       IF   FTN-KOHI3-D2          >=  MAX-KIN-KOHI3-D   
                            MOVE     ZERO         TO  ZAN-KOHI3-D2      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI3-D2 =   MAX-KIN-KOHI3-D - 
                                                      FTN-KOHI3-D2      
                       END-IF                                           
                       IF   FTN-KOHI3-D3          >=  MAX-KIN-KOHI3-D   
                            MOVE     ZERO         TO  ZAN-KOHI3-D3      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI3-D3 =   MAX-KIN-KOHI3-D - 
                                                      FTN-KOHI3-D3      
                       END-IF   
                   END-IF
               END-IF
           END-IF.
           IF  FLG-SANTEI4               =     "1"
               MOVE     ZERO             TO    ZAN-KOHI4-D
                                               ZAN-KOHI4-D2             
                                               ZAN-KOHI4-D3             
           ELSE
               IF (COMB-KOH4HKNNUM           NOT = SPACE)    AND
                  (COMB-KOH4-GAIJGNKBN       =     "1")
                   IF       MAX-KIN-KOHI4-D       NOT = ZERO
                       IF   FTN-KOHI4-D           >=  MAX-KIN-KOHI4-D
                            MOVE     ZERO         TO  ZAN-KOHI4-D
                       ELSE
                            COMPUTE  ZAN-KOHI4-D  =   MAX-KIN-KOHI4-D -
                                                      FTN-KOHI4-D
                       END-IF
                       IF   FTN-KOHI4-D2          >=  MAX-KIN-KOHI4-D   
                            MOVE     ZERO         TO  ZAN-KOHI4-D2      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI4-D2 =   MAX-KIN-KOHI4-D - 
                                                      FTN-KOHI4-D2      
                       END-IF                                           
                       IF   FTN-KOHI4-D3          >=  MAX-KIN-KOHI4-D   
                            MOVE     ZERO         TO  ZAN-KOHI4-D3      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI4-D3 =   MAX-KIN-KOHI4-D - 
                                                      FTN-KOHI4-D3      
                       END-IF   
                   END-IF
               END-IF
           END-IF.
      *
      *Ê¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ
      *¹âÎð¼Ô¤ÎºßÂðÁí¹ç¡¦ºßÂðËö´ü»»Äê´µ¼Ô
      *
           IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1")
           OR  (FLG-SYUHEN                   =   "1")
           OR  (FLG-KOHININBARAI             =    1 )
           OR  (FLG-GENBUTU70            NOT =  ZERO)
               IF  FLG-SANTEI-SYU            =   "1"                  
                       MOVE  ZERO        TO      ZAN-SYU-M            
               ELSE                                                   
                   IF  FTN-SYU-M         >=    MAX-KIN-SYU-M          
                       MOVE  ZERO        TO      ZAN-SYU-M
                   ELSE
                       COMPUTE  ZAN-SYU-M  =   MAX-KIN-SYU-M  -
                                               FTN-SYU-M
                   END-IF
               END-IF
           END-IF.
      *
           IF  FLG-SANTEI1               =     "1"
               MOVE     ZERO             TO    ZAN-KOHI1-M
                                               ZAN-KOHI1-M2             
                                               ZAN-KOHI1-M3             
           ELSE
               IF (COMB-KOH1HKNNUM           NOT = SPACE)    AND
                  (COMB-KOH1-GAIJGNKBN       =     "1")
                   IF       MAX-KIN-KOHI1-M       NOT = ZERO
                       IF   FTN-KOHI1-M           >=  MAX-KIN-KOHI1-M
                            MOVE     ZERO         TO  ZAN-KOHI1-M
                       ELSE
                            COMPUTE  ZAN-KOHI1-M  =   MAX-KIN-KOHI1-M -
                                                      FTN-KOHI1-M
                       END-IF
                       IF  COMB-KOH1HKNNUM    =   "027"                 
                            MOVE     ZERO         TO  ZAN-KOHI1-M2      
                       ELSE                                             
                         IF   FTN-KOHI1-M2        >=  MAX-KIN-KOHI1-M   
                            MOVE     ZERO         TO  ZAN-KOHI1-M2      
                         ELSE                                           
                            COMPUTE  ZAN-KOHI1-M2 =   MAX-KIN-KOHI1-M - 
                                                      FTN-KOHI1-M2      
                         END-IF                                         
                       END-IF                                           
                       IF  COMB-KOH1HKNNUM    =   "027"                 
                            MOVE     ZERO         TO  ZAN-KOHI1-M3      
                       ELSE                                             
                         IF   FTN-KOHI1-M3        >=  MAX-KIN-KOHI1-M   
                            MOVE     ZERO         TO  ZAN-KOHI1-M3      
                         ELSE                                           
                            COMPUTE  ZAN-KOHI1-M3 =   MAX-KIN-KOHI1-M - 
                                                      FTN-KOHI1-M3      
                         END-IF                                         
                       END-IF       
                   END-IF
               END-IF
           END-IF.
           IF  FLG-SANTEI2               =     "1"
               MOVE     ZERO             TO    ZAN-KOHI2-M
                                               ZAN-KOHI2-M2             
                                               ZAN-KOHI2-M3             
           ELSE
               IF (COMB-KOH2HKNNUM           NOT = SPACE)    AND
                  (COMB-KOH2-GAIJGNKBN       =     "1")
                   IF       MAX-KIN-KOHI2-M       NOT = ZERO
                       IF   FTN-KOHI2-M           >=  MAX-KIN-KOHI2-M
                            MOVE     ZERO         TO  ZAN-KOHI2-M
                       ELSE
                            COMPUTE  ZAN-KOHI2-M  =   MAX-KIN-KOHI2-M -
                                                      FTN-KOHI2-M
                       END-IF
                       IF   FTN-KOHI2-M2          >=  MAX-KIN-KOHI2-M   
                            MOVE     ZERO         TO  ZAN-KOHI2-M2      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI2-M2 =   MAX-KIN-KOHI2-M - 
                                                      FTN-KOHI2-M2      
                       END-IF                                           
                       IF   FTN-KOHI2-M3          >=  MAX-KIN-KOHI2-M   
                            MOVE     ZERO         TO  ZAN-KOHI2-M3      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI2-M3 =   MAX-KIN-KOHI2-M - 
                                                      FTN-KOHI2-M3      
                       END-IF   
                   END-IF
               END-IF
           END-IF.
           IF  FLG-SANTEI3               =     "1"
               MOVE     ZERO             TO    ZAN-KOHI3-M
                                               ZAN-KOHI3-M2             
                                               ZAN-KOHI3-M3             
           ELSE
               IF (COMB-KOH3HKNNUM           NOT = SPACE)    AND
                  (COMB-KOH3-GAIJGNKBN       =     "1")
                   IF       MAX-KIN-KOHI3-M       NOT = ZERO
                       IF   FTN-KOHI3-M           >=  MAX-KIN-KOHI3-M
                            MOVE     ZERO         TO  ZAN-KOHI3-M
                       ELSE
                            COMPUTE  ZAN-KOHI3-M  =   MAX-KIN-KOHI3-M -
                                                      FTN-KOHI3-M
                       END-IF
                       IF   FTN-KOHI3-M2          >=  MAX-KIN-KOHI3-M   
                            MOVE     ZERO         TO  ZAN-KOHI3-M2      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI3-M2 =   MAX-KIN-KOHI3-M - 
                                                      FTN-KOHI3-M2      
                       END-IF                                           
                       IF   FTN-KOHI3-M3          >=  MAX-KIN-KOHI3-M   
                            MOVE     ZERO         TO  ZAN-KOHI3-M3      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI3-M3 =   MAX-KIN-KOHI3-M - 
                                                      FTN-KOHI3-M3      
                       END-IF   
                   END-IF
               END-IF
           END-IF.
           IF  FLG-SANTEI4               =     "1"
               MOVE     ZERO             TO    ZAN-KOHI4-M
                                               ZAN-KOHI4-M2             
                                               ZAN-KOHI4-M3             
           ELSE
               IF (COMB-KOH4HKNNUM           NOT = SPACE)    AND
                  (COMB-KOH4-GAIJGNKBN       =     "1")
                   IF       MAX-KIN-KOHI4-M       NOT = ZERO
                       IF   FTN-KOHI4-M           >=  MAX-KIN-KOHI4-M
                            MOVE     ZERO         TO  ZAN-KOHI4-M
                       ELSE
                            COMPUTE  ZAN-KOHI4-M  =   MAX-KIN-KOHI4-M -
                                                      FTN-KOHI4-M
                       END-IF
                       IF   FTN-KOHI4-M2          >=  MAX-KIN-KOHI4-M   
                            MOVE     ZERO         TO  ZAN-KOHI4-M2      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI4-M2 =   MAX-KIN-KOHI4-M - 
                                                      FTN-KOHI4-M2      
                       END-IF                                           
                       IF   FTN-KOHI4-M3          >=  MAX-KIN-KOHI4-M   
                            MOVE     ZERO         TO  ZAN-KOHI4-M3      
                       ELSE                                             
                            COMPUTE  ZAN-KOHI4-M3 =   MAX-KIN-KOHI4-M - 
                                                      FTN-KOHI4-M3      
                       END-IF  
                   END-IF
               END-IF
           END-IF.
      *
      *    Æü¤Î´µ¼ÔÉéÃ´²ÄÇ½»Ä³Û¤È·î¤Î´µ¼ÔÉéÃ´²ÄÇ½»Ä³Û¤òÈæ³Ó¤·¤Æ
      *    ÃÍ¤Î¾®¤µ¤¤¤Û¤¦¤ò¥»¥Ã¥È¤¹¤ë
           IF (COMB-KOH1HKNNUM              NOT = SPACE)    AND
              (COMB-KOH1-GAIJGNKBN          =     "1")
               EVALUATE   TRUE
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)
                     IF    ZAN-KOHI1-D            >    ZAN-KOHI1-M
                           MOVE   ZAN-KOHI1-M     TO   ZAN-KOHI1
                     ELSE
                           MOVE   ZAN-KOHI1-D     TO   ZAN-KOHI1
                     END-IF
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI1-M   =       ZERO)
                           MOVE   ZAN-KOHI1-D     TO   ZAN-KOHI1
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)
                           MOVE   ZAN-KOHI1-M     TO   ZAN-KOHI1
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND
                         (MAX-KIN-KOHI1-M   =       ZERO)
                           MOVE   ZERO            TO   ZAN-KOHI1
               END-EVALUATE
               IF   (MAX-KIN-KOHI1-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI1-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI1   =   SPACE
                   IF    MAX-KIN-KOHI1-K  NOT =  ZERO
                          IF   FTN-KOHI1-D-K     >=  MAX-KIN-KOHI1-K
                            MOVE     ZERO         TO  ZAN-KOHI1
                          ELSE
                            COMPUTE  ZAN-KOHI1   =   MAX-KIN-KOHI1-K
                                                 -   FTN-KOHI1-D-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI1-K     TO   ZAN-KOHI1
                   ELSE
                     IF    TEI-KIN-KOHI1-K  NOT =  ZERO
                          IF   FTN-KOHI1-D-K     >=  TEI-KIN-KOHI1-K
                            MOVE     ZERO         TO  ZAN-KOHI1
                          ELSE
                            COMPUTE  ZAN-KOHI1   =   TEI-KIN-KOHI1-K
                                                 -   FTN-KOHI1-D-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI1-K     TO   ZAN-KOHI1
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *
               EVALUATE   TRUE                                          
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)               
                     IF    ZAN-KOHI1-D2           >    ZAN-KOHI1-M2     
                           MOVE   ZAN-KOHI1-M2    TO   ZAN-KOHI12       
                     ELSE                                               
                           MOVE   ZAN-KOHI1-D2    TO   ZAN-KOHI12       
                     END-IF                                             
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI1-M   =       ZERO)               
                           MOVE   ZAN-KOHI1-D2    TO   ZAN-KOHI12       
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)               
                           MOVE   ZAN-KOHI1-M2    TO   ZAN-KOHI12       
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI1-M   =       ZERO)               
                           MOVE   ZERO            TO   ZAN-KOHI12       
               END-EVALUATE                                             
               IF   (MAX-KIN-KOHI1-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI1-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI1   =   SPACE
                   IF    MAX-KIN-KOHI1-K  NOT =  ZERO
                          IF   FTN-KOHI1-D2-K    >=  MAX-KIN-KOHI1-K
                            MOVE     ZERO         TO  ZAN-KOHI12
                          ELSE
                            COMPUTE  ZAN-KOHI12  =   MAX-KIN-KOHI1-K
                                                 -   FTN-KOHI1-D2-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI1-K     TO   ZAN-KOHI12
                   ELSE
                     IF    TEI-KIN-KOHI1-K  NOT =  ZERO
                          IF   FTN-KOHI1-D2-K    >=  TEI-KIN-KOHI1-K
                            MOVE     ZERO         TO  ZAN-KOHI12
                          ELSE
                            COMPUTE  ZAN-KOHI12  =   TEI-KIN-KOHI1-K
                                                 -   FTN-KOHI1-D2-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI1-K     TO   ZAN-KOHI12
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *
               EVALUATE   TRUE                                          
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)               
                     IF    ZAN-KOHI1-D3           >    ZAN-KOHI1-M3     
                           MOVE   ZAN-KOHI1-M3    TO   ZAN-KOHI13       
                     ELSE                                               
                           MOVE   ZAN-KOHI1-D3    TO   ZAN-KOHI13       
                     END-IF                                             
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI1-M   =       ZERO)               
                           MOVE   ZAN-KOHI1-D3    TO   ZAN-KOHI13       
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)               
                           MOVE   ZAN-KOHI1-M3    TO   ZAN-KOHI13       
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI1-M   =       ZERO)               
                           MOVE   ZERO            TO   ZAN-KOHI13       
               END-EVALUATE                                             
               IF   (MAX-KIN-KOHI1-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI1-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI1   =   SPACE
                   IF    MAX-KIN-KOHI1-K  NOT =  ZERO
                          IF   FTN-KOHI1-D3-K    >=  MAX-KIN-KOHI1-K
                            MOVE     ZERO         TO  ZAN-KOHI13
                          ELSE
                            COMPUTE  ZAN-KOHI13  =   MAX-KIN-KOHI1-K
                                                 -   FTN-KOHI1-D3-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI1-K     TO   ZAN-KOHI13
                   ELSE
                     IF    TEI-KIN-KOHI1-K  NOT =  ZERO
                          IF   FTN-KOHI1-D3-K    >=  TEI-KIN-KOHI1-K
                            MOVE     ZERO         TO  ZAN-KOHI13
                          ELSE
                            COMPUTE  ZAN-KOHI13  =   TEI-KIN-KOHI1-K
                                                 -   FTN-KOHI1-D3-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI1-K     TO   ZAN-KOHI13
                     END-IF
                   END-IF
                 END-IF
               END-IF
      ****
               IF (( LNK-SYU-SRYYMD(1:6)   >=  "200604"       ) AND
                   ( COMB-KOH1HKNNUM = "015" OR "016" OR "021") AND
                   ( WK-JIRITU-CYUKANSOU(1) =   2             ))
               OR (( LNK-SYU-SRYYMD(1:6)   >=  "200610"       ) AND
                   ( COMB-KOH1HKNNUM = "024" OR "079"         ))
                   IF  ( COMB-KOH1HKNNUM  = "015" OR "016"
                                         OR "024" OR "079" )
                   AND ((COMB-KOH2HKNNUM  = "972") OR
                        (COMB-KOH3HKNNUM  = "972") OR
                        (COMB-KOH4HKNNUM  = "972"))
                   AND ( MAX-KIN-KOHI1-M  >  10000)
                      IF    FTN-KOHI1-M-JI   >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-KOHI1-M-JI
                      END-IF
                      IF   ZAN-KOHI1  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI1
                      END-IF
                      IF    FTN-KOHI1-M2-JI  >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-KOHI1-M2-JI
                      END-IF
                      IF   ZAN-KOHI12 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI12
                      END-IF
                      IF    FTN-KOHI1-M3-JI  >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-KOHI1-M3-JI
                      END-IF
                      IF   ZAN-KOHI13 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI13
                      END-IF
                   END-IF
                   IF  ( COMB-KOH1HKNNUM  = "015" OR "016"
                                         OR "024" OR "079" )
                   AND ((COMB-KOH2HKNNUM  = "974") OR
                        (COMB-KOH3HKNNUM  = "974") OR
                        (COMB-KOH4HKNNUM  = "974"))
                   AND ( MAX-KIN-KOHI1-M  >  20000)
                      IF    FTN-KOHI1-M-JI   >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-KOHI1-M-JI
                      END-IF
                      IF   ZAN-KOHI1  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI1
                      END-IF
                      IF    FTN-KOHI1-M2-JI  >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-KOHI1-M2-JI
                      END-IF
                      IF   ZAN-KOHI12 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI12
                      END-IF
                      IF    FTN-KOHI1-M3-JI  >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-KOHI1-M3-JI
                      END-IF
                      IF   ZAN-KOHI13 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI13
                      END-IF
                   END-IF
                   IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =   "1" ) OR
                        (FLG-KOUREI-KBN               =   "1" ))
                   AND ( MAX-KIN-KOHI1-M  >  WK-KOH-GENDOGAKU  )
                      IF    FTN-KOHI1-M-JI   >=   WK-KOH-GENDOGAKU
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = WK-KOH-GENDOGAKU
                                              - FTN-KOHI1-M-JI
                      END-IF
                      IF   ZAN-KOHI1  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI1
                      END-IF
                      IF    FTN-KOHI1-M2-JI  >=   WK-KOH-GENDOGAKU
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = WK-KOH-GENDOGAKU
                                              - FTN-KOHI1-M2-JI
                      END-IF
                      IF   ZAN-KOHI12 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI12
                      END-IF
                      IF    FTN-KOHI1-M3-JI  >=   WK-KOH-GENDOGAKU
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = WK-KOH-GENDOGAKU
                                              - FTN-KOHI1-M3-JI
                      END-IF
                      IF   ZAN-KOHI13 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI13
                      END-IF
                   END-IF
                   IF  ( FLG-GENBUTU70   NOT =   ZERO          )
                   AND ( MAX-KIN-KOHI1-M  >  WK-GEN70-GENDOGAKU)
                      IF    FTN-KOHI1-M-JI   >=   WK-GEN70-GENDOGAKU
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = WK-GEN70-GENDOGAKU
                                              - FTN-KOHI1-M-JI
                      END-IF
                      IF   ZAN-KOHI1  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI1
                      END-IF
                      IF    FTN-KOHI1-M2-JI  >=   WK-GEN70-GENDOGAKU
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = WK-GEN70-GENDOGAKU
                                              - FTN-KOHI1-M2-JI
                      END-IF
                      IF   ZAN-KOHI12 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI12
                      END-IF
                      IF    FTN-KOHI1-M3-JI  >=   WK-GEN70-GENDOGAKU
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = WK-GEN70-GENDOGAKU
                                              - FTN-KOHI1-M3-JI
                      END-IF
                      IF   ZAN-KOHI13 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI13
                      END-IF
                   END-IF
               END-IF
      *+++
               IF (( LNK-SYU-SRYYMD(1:6)   >=  "200604") AND
                   ( COMB-KOH1HKNNUM  = "015" OR "016" ) AND
                   ((COMB-KOH2HKNNUM  = "972") OR
                    (COMB-KOH3HKNNUM  = "972") OR
                    (COMB-KOH4HKNNUM  = "972"))         )
               OR (( LNK-SYU-SRYYMD(1:6)   >=  "200610") AND
                   ( COMB-KOH1HKNNUM  = "024"          ) AND
                   ((COMB-KOH2HKNNUM  = "972") OR
                    (COMB-KOH3HKNNUM  = "972") OR
                    (COMB-KOH4HKNNUM  = "972"))         )
                      IF    FTN-CYOKI-M-JI   >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-CYOKI-M-JI
                      END-IF
                      IF   ZAN-KOHI1  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI1
                      END-IF
                      IF    FTN-CYOKI-M2-JI  >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-CYOKI-M2-JI
                      END-IF
                      IF   ZAN-KOHI12 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI12
                      END-IF
                      IF    FTN-CYOKI-M3-JI  >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-CYOKI-M3-JI
                      END-IF
                      IF   ZAN-KOHI13 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI13
                      END-IF
               END-IF
               IF (( LNK-SYU-SRYYMD(1:6)   >=  "200604") AND
                   ( COMB-KOH1HKNNUM  = "015" OR "016" ) AND
                   ((COMB-KOH2HKNNUM  = "974") OR
                    (COMB-KOH3HKNNUM  = "974") OR
                    (COMB-KOH4HKNNUM  = "974"))         )
               OR (( LNK-SYU-SRYYMD(1:6)   >=  "200610") AND
                   ( COMB-KOH1HKNNUM  = "024"          ) AND
                   ((COMB-KOH2HKNNUM  = "974") OR
                    (COMB-KOH3HKNNUM  = "974") OR
                    (COMB-KOH4HKNNUM  = "974"))         )
                      IF    FTN-CYOKI-M-JI   >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-CYOKI-M-JI
                      END-IF
                      IF   ZAN-KOHI1  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI1
                      END-IF
                      IF    FTN-CYOKI-M2-JI  >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-CYOKI-M2-JI
                      END-IF
                      IF   ZAN-KOHI12 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI12
                      END-IF
                      IF    FTN-CYOKI-M3-JI  >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-CYOKI-M3-JI
                      END-IF
                      IF   ZAN-KOHI13 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI13
                      END-IF
               END-IF
      ****
           END-IF.
      *
           IF (COMB-KOH2HKNNUM              NOT = SPACE)    AND
              (COMB-KOH2-GAIJGNKBN          =     "1")
               EVALUATE   TRUE
                   WHEN  (MAX-KIN-KOHI2-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI2-M   NOT =   ZERO)
                     IF    ZAN-KOHI2-D            >    ZAN-KOHI2-M
                           MOVE   ZAN-KOHI2-M     TO   ZAN-KOHI2
                     ELSE
                           MOVE   ZAN-KOHI2-D     TO   ZAN-KOHI2
                     END-IF
                   WHEN  (MAX-KIN-KOHI2-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI2-M   =       ZERO)
                           MOVE   ZAN-KOHI2-D     TO   ZAN-KOHI2
                   WHEN  (MAX-KIN-KOHI2-D   =       ZERO)   AND
                         (MAX-KIN-KOHI2-M   NOT =   ZERO)
                           MOVE   ZAN-KOHI2-M     TO   ZAN-KOHI2
                   WHEN  (MAX-KIN-KOHI2-D   =       ZERO)   AND
                         (MAX-KIN-KOHI2-M   =       ZERO)
                           MOVE   ZERO            TO   ZAN-KOHI2
               END-EVALUATE
               IF   (MAX-KIN-KOHI2-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI2-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI2   =   SPACE
                   IF    MAX-KIN-KOHI2-K  NOT =  ZERO
                          IF   FTN-KOHI2-D-K     >=  MAX-KIN-KOHI2-K
                            MOVE     ZERO         TO  ZAN-KOHI2
                          ELSE
                            COMPUTE  ZAN-KOHI2   =   MAX-KIN-KOHI2-K
                                                 -   FTN-KOHI2-D-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI2-K     TO   ZAN-KOHI2
                   ELSE
                     IF    TEI-KIN-KOHI2-K  NOT =  ZERO
                          IF   FTN-KOHI2-D-K     >=  TEI-KIN-KOHI2-K
                            MOVE     ZERO         TO  ZAN-KOHI2
                          ELSE
                            COMPUTE  ZAN-KOHI2   =   TEI-KIN-KOHI2-K
                                                 -   FTN-KOHI2-D-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI2-K     TO   ZAN-KOHI2
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *
               EVALUATE   TRUE                                          
                   WHEN  (MAX-KIN-KOHI2-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI2-M   NOT =   ZERO)               
                     IF    ZAN-KOHI2-D2           >    ZAN-KOHI2-M2     
                           MOVE   ZAN-KOHI2-M2    TO   ZAN-KOHI22       
                     ELSE                                               
                           MOVE   ZAN-KOHI2-D2    TO   ZAN-KOHI22       
                     END-IF                                             
                   WHEN  (MAX-KIN-KOHI2-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI2-M   =       ZERO)               
                           MOVE   ZAN-KOHI2-D2    TO   ZAN-KOHI22       
                   WHEN  (MAX-KIN-KOHI2-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI2-M   NOT =   ZERO)               
                           MOVE   ZAN-KOHI2-M2    TO   ZAN-KOHI22       
                   WHEN  (MAX-KIN-KOHI2-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI2-M   =       ZERO)               
                           MOVE   ZERO            TO   ZAN-KOHI22       
               END-EVALUATE                                             
               IF   (MAX-KIN-KOHI2-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI2-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI2   =   SPACE
                   IF    MAX-KIN-KOHI2-K  NOT =  ZERO
                          IF   FTN-KOHI2-D2-K    >=  MAX-KIN-KOHI2-K
                            MOVE     ZERO         TO  ZAN-KOHI22
                          ELSE
                            COMPUTE  ZAN-KOHI22  =   MAX-KIN-KOHI2-K
                                                 -   FTN-KOHI2-D2-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI2-K     TO   ZAN-KOHI22
                   ELSE
                     IF    TEI-KIN-KOHI2-K  NOT =  ZERO
                          IF   FTN-KOHI2-D2-K    >=  TEI-KIN-KOHI2-K
                            MOVE     ZERO         TO  ZAN-KOHI22
                          ELSE
                            COMPUTE  ZAN-KOHI22  =   TEI-KIN-KOHI2-K
                                                 -   FTN-KOHI2-D2-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI2-K     TO   ZAN-KOHI22
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *
               EVALUATE   TRUE                                          
                   WHEN  (MAX-KIN-KOHI2-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI2-M   NOT =   ZERO)               
                     IF    ZAN-KOHI2-D3           >    ZAN-KOHI2-M3     
                           MOVE   ZAN-KOHI2-M3    TO   ZAN-KOHI23       
                     ELSE                                               
                           MOVE   ZAN-KOHI2-D3    TO   ZAN-KOHI23       
                     END-IF                                             
                   WHEN  (MAX-KIN-KOHI2-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI2-M   =       ZERO)               
                           MOVE   ZAN-KOHI2-D3    TO   ZAN-KOHI23       
                   WHEN  (MAX-KIN-KOHI2-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI2-M   NOT =   ZERO)               
                           MOVE   ZAN-KOHI2-M3    TO   ZAN-KOHI23       
                   WHEN  (MAX-KIN-KOHI2-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI2-M   =       ZERO)               
                           MOVE   ZERO            TO   ZAN-KOHI23       
               END-EVALUATE                                             
               IF   (MAX-KIN-KOHI2-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI2-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI2   =   SPACE
                   IF    MAX-KIN-KOHI2-K  NOT =  ZERO
                          IF   FTN-KOHI2-D3-K    >=  MAX-KIN-KOHI2-K
                            MOVE     ZERO         TO  ZAN-KOHI23
                          ELSE
                            COMPUTE  ZAN-KOHI23  =   MAX-KIN-KOHI2-K
                                                 -   FTN-KOHI2-D3-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI2-K     TO   ZAN-KOHI23
                   ELSE
                     IF    TEI-KIN-KOHI2-K  NOT =  ZERO
                          IF   FTN-KOHI2-D3-K    >=  TEI-KIN-KOHI2-K
                            MOVE     ZERO         TO  ZAN-KOHI23
                          ELSE
                            COMPUTE  ZAN-KOHI23  =   TEI-KIN-KOHI2-K
                                                 -   FTN-KOHI2-D3-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI2-K     TO   ZAN-KOHI23
                     END-IF
                   END-IF
                 END-IF
               END-IF
      ****
               IF (( LNK-SYU-SRYYMD(1:6)   >=  "200604"       ) AND
                   ( COMB-KOH2HKNNUM = "015" OR "016" OR "021") AND
                   ( WK-JIRITU-CYUKANSOU(2) =   2             ))
               OR (( LNK-SYU-SRYYMD(1:6)   >=  "200610"       ) AND
                   ( COMB-KOH2HKNNUM = "024" OR "079"         ))
                   IF  ( COMB-KOH2HKNNUM  = "015" OR "016"
                                         OR "024" OR "079" )
                   AND ((COMB-KOH1HKNNUM  = "972") OR
                        (COMB-KOH3HKNNUM  = "972") OR
                        (COMB-KOH4HKNNUM  = "972"))
                   AND ( MAX-KIN-KOHI2-M  >  10000)
                      IF    FTN-KOHI2-M-JI   >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-KOHI2-M-JI
                      END-IF
                      IF   ZAN-KOHI2  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI2
                      END-IF
                      IF    FTN-KOHI2-M2-JI  >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-KOHI2-M2-JI
                      END-IF
                      IF   ZAN-KOHI22 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI22
                      END-IF
                      IF    FTN-KOHI2-M3-JI  >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-KOHI2-M3-JI
                      END-IF
                      IF   ZAN-KOHI23 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI23
                      END-IF
                   END-IF
                   IF  ( COMB-KOH2HKNNUM  = "015" OR "016"
                                         OR "024" OR "079" )
                   AND ((COMB-KOH1HKNNUM  = "974") OR
                        (COMB-KOH3HKNNUM  = "974") OR
                        (COMB-KOH4HKNNUM  = "974"))
                   AND ( MAX-KIN-KOHI2-M  >  20000)
                      IF    FTN-KOHI2-M-JI   >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-KOHI2-M-JI
                      END-IF
                      IF   ZAN-KOHI2  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI2
                      END-IF
                      IF    FTN-KOHI2-M2-JI  >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-KOHI2-M2-JI
                      END-IF
                      IF   ZAN-KOHI22 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI22
                      END-IF
                      IF    FTN-KOHI2-M3-JI  >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-KOHI2-M3-JI
                      END-IF
                      IF   ZAN-KOHI23 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI23
                      END-IF
                   END-IF
                   IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =   "1" ) OR
                        (FLG-KOUREI-KBN               =   "1" ))
                   AND ( MAX-KIN-KOHI2-M  >  WK-KOH-GENDOGAKU  )
                      IF    FTN-KOHI2-M-JI   >=   WK-KOH-GENDOGAKU
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = WK-KOH-GENDOGAKU
                                              - FTN-KOHI2-M-JI
                      END-IF
                      IF   ZAN-KOHI2  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI2
                      END-IF
                      IF    FTN-KOHI2-M2-JI  >=   WK-KOH-GENDOGAKU
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = WK-KOH-GENDOGAKU
                                              - FTN-KOHI2-M2-JI
                      END-IF
                      IF   ZAN-KOHI22 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI22
                      END-IF
                      IF    FTN-KOHI2-M3-JI  >=   WK-KOH-GENDOGAKU
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = WK-KOH-GENDOGAKU
                                              - FTN-KOHI2-M3-JI
                      END-IF
                      IF   ZAN-KOHI23 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI23
                      END-IF
                   END-IF
               END-IF
      *+++
               IF (( LNK-SYU-SRYYMD(1:6)   >=  "200604") AND
                   ( COMB-KOH2HKNNUM  = "015" OR "016" ) AND
                   ((COMB-KOH1HKNNUM  = "972") OR
                    (COMB-KOH3HKNNUM  = "972") OR
                    (COMB-KOH4HKNNUM  = "972"))         )
               OR (( LNK-SYU-SRYYMD(1:6)   >=  "200610") AND
                   ( COMB-KOH2HKNNUM  = "024"          ) AND
                   ((COMB-KOH1HKNNUM  = "972") OR
                    (COMB-KOH3HKNNUM  = "972") OR
                    (COMB-KOH4HKNNUM  = "972"))         )
                      IF    FTN-CYOKI-M-JI   >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-CYOKI-M-JI
                      END-IF
                      IF   ZAN-KOHI2  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI2
                      END-IF
                      IF    FTN-CYOKI-M2-JI  >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-CYOKI-M2-JI
                      END-IF
                      IF   ZAN-KOHI22 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI22
                      END-IF
                      IF    FTN-CYOKI-M3-JI  >=   10000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 10000 - FTN-CYOKI-M3-JI
                      END-IF
                      IF   ZAN-KOHI23 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI23
                      END-IF
               END-IF
               IF (( LNK-SYU-SRYYMD(1:6)   >=  "200604") AND
                   ( COMB-KOH2HKNNUM  = "015" OR "016" ) AND
                   ((COMB-KOH1HKNNUM  = "974") OR
                    (COMB-KOH3HKNNUM  = "974") OR
                    (COMB-KOH4HKNNUM  = "974"))         )
               OR (( LNK-SYU-SRYYMD(1:6)   >=  "200610") AND
                   ( COMB-KOH2HKNNUM  = "024"          ) AND
                   ((COMB-KOH1HKNNUM  = "974") OR
                    (COMB-KOH3HKNNUM  = "974") OR
                    (COMB-KOH4HKNNUM  = "974"))         )
                      IF    FTN-CYOKI-M-JI   >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-CYOKI-M-JI
                      END-IF
                      IF   ZAN-KOHI2  >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI2
                      END-IF
                      IF    FTN-CYOKI-M2-JI  >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-CYOKI-M2-JI
                      END-IF
                      IF   ZAN-KOHI22 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI22
                      END-IF
                      IF    FTN-CYOKI-M3-JI  >=   20000
                        MOVE    ZERO    TO   WK-ZAN-JIRITU
                      ELSE
                        COMPUTE WK-ZAN-JIRITU = 20000 - FTN-CYOKI-M3-JI
                      END-IF
                      IF   ZAN-KOHI23 >  WK-ZAN-JIRITU
                        MOVE   WK-ZAN-JIRITU  TO  ZAN-KOHI23
                      END-IF
               END-IF
      ****
           END-IF.
      *
           IF (COMB-KOH3HKNNUM              NOT = SPACE)    AND
              (COMB-KOH3-GAIJGNKBN          =     "1")
               EVALUATE   TRUE
                   WHEN  (MAX-KIN-KOHI3-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI3-M   NOT =   ZERO)
                     IF    ZAN-KOHI3-D            >    ZAN-KOHI3-M
                           MOVE   ZAN-KOHI3-M     TO   ZAN-KOHI3
                     ELSE
                           MOVE   ZAN-KOHI3-D     TO   ZAN-KOHI3
                     END-IF
                   WHEN  (MAX-KIN-KOHI3-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI3-M   =       ZERO)
                           MOVE   ZAN-KOHI3-D     TO   ZAN-KOHI3
                   WHEN  (MAX-KIN-KOHI3-D   =       ZERO)   AND
                         (MAX-KIN-KOHI3-M   NOT =   ZERO)
                           MOVE   ZAN-KOHI3-M     TO   ZAN-KOHI3
                   WHEN  (MAX-KIN-KOHI3-D   =       ZERO)   AND
                         (MAX-KIN-KOHI3-M   =       ZERO)
                           MOVE   ZERO            TO   ZAN-KOHI3
               END-EVALUATE
               IF   (MAX-KIN-KOHI3-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI3-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI3   =   SPACE
                   IF    MAX-KIN-KOHI3-K  NOT =  ZERO
                          IF   FTN-KOHI3-D-K     >=  MAX-KIN-KOHI3-K
                            MOVE     ZERO         TO  ZAN-KOHI3
                          ELSE
                            COMPUTE  ZAN-KOHI3   =   MAX-KIN-KOHI3-K
                                                 -   FTN-KOHI3-D-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI3-K     TO   ZAN-KOHI3
                   ELSE
                     IF    TEI-KIN-KOHI3-K  NOT =  ZERO
                          IF   FTN-KOHI3-D-K     >=  TEI-KIN-KOHI3-K
                            MOVE     ZERO         TO  ZAN-KOHI3
                          ELSE
                            COMPUTE  ZAN-KOHI3   =   TEI-KIN-KOHI3-K
                                                 -   FTN-KOHI3-D-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI3-K     TO   ZAN-KOHI3
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *
               EVALUATE   TRUE                                          
                   WHEN  (MAX-KIN-KOHI3-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI3-M   NOT =   ZERO)               
                     IF    ZAN-KOHI3-D2           >    ZAN-KOHI3-M2     
                           MOVE   ZAN-KOHI3-M2    TO   ZAN-KOHI32       
                     ELSE                                               
                           MOVE   ZAN-KOHI3-D2    TO   ZAN-KOHI32       
                     END-IF                                             
                   WHEN  (MAX-KIN-KOHI3-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI3-M   =       ZERO)               
                           MOVE   ZAN-KOHI3-D2    TO   ZAN-KOHI32       
                   WHEN  (MAX-KIN-KOHI3-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI3-M   NOT =   ZERO)               
                           MOVE   ZAN-KOHI3-M2    TO   ZAN-KOHI32       
                   WHEN  (MAX-KIN-KOHI3-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI3-M   =       ZERO)               
                           MOVE   ZERO            TO   ZAN-KOHI32       
               END-EVALUATE                                             
               IF   (MAX-KIN-KOHI3-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI3-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI3   =   SPACE
                   IF    MAX-KIN-KOHI3-K  NOT =  ZERO
                          IF   FTN-KOHI3-D2-K    >=  MAX-KIN-KOHI3-K
                            MOVE     ZERO         TO  ZAN-KOHI32
                          ELSE
                            COMPUTE  ZAN-KOHI32  =   MAX-KIN-KOHI3-K
                                                 -   FTN-KOHI3-D2-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI3-K     TO   ZAN-KOHI32
                   ELSE
                     IF    TEI-KIN-KOHI3-K  NOT =  ZERO
                          IF   FTN-KOHI3-D2-K    >=  TEI-KIN-KOHI3-K
                            MOVE     ZERO         TO  ZAN-KOHI32
                          ELSE
                            COMPUTE  ZAN-KOHI32  =   TEI-KIN-KOHI3-K
                                                 -   FTN-KOHI3-D2-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI3-K     TO   ZAN-KOHI32
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *
               EVALUATE   TRUE                                          
                   WHEN  (MAX-KIN-KOHI3-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI3-M   NOT =   ZERO)               
                     IF    ZAN-KOHI3-D3           >    ZAN-KOHI3-M3     
                           MOVE   ZAN-KOHI3-M3    TO   ZAN-KOHI33       
                     ELSE                                               
                           MOVE   ZAN-KOHI3-D3    TO   ZAN-KOHI33       
                     END-IF                                             
                   WHEN  (MAX-KIN-KOHI3-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI3-M   =       ZERO)               
                           MOVE   ZAN-KOHI3-D3    TO   ZAN-KOHI33       
                   WHEN  (MAX-KIN-KOHI3-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI3-M   NOT =   ZERO)               
                           MOVE   ZAN-KOHI3-M3    TO   ZAN-KOHI33       
                   WHEN  (MAX-KIN-KOHI3-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI3-M   =       ZERO)               
                           MOVE   ZERO            TO   ZAN-KOHI33       
               END-EVALUATE                                             
               IF   (MAX-KIN-KOHI3-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI3-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI3   =   SPACE
                   IF    MAX-KIN-KOHI3-K  NOT =  ZERO
                          IF   FTN-KOHI3-D3-K    >=  MAX-KIN-KOHI3-K
                            MOVE     ZERO         TO  ZAN-KOHI33
                          ELSE
                            COMPUTE  ZAN-KOHI33  =   MAX-KIN-KOHI3-K
                                                 -   FTN-KOHI3-D3-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI3-K     TO   ZAN-KOHI33
                   ELSE
                     IF    TEI-KIN-KOHI3-K  NOT =  ZERO
                          IF   FTN-KOHI3-D3-K    >=  TEI-KIN-KOHI3-K
                            MOVE     ZERO         TO  ZAN-KOHI33
                          ELSE
                            COMPUTE  ZAN-KOHI33  =   TEI-KIN-KOHI3-K
                                                 -   FTN-KOHI3-D3-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI3-K     TO   ZAN-KOHI33
                     END-IF
                   END-IF
                 END-IF
               END-IF
           END-IF.
      *
           IF (COMB-KOH4HKNNUM              NOT = SPACE)    AND
              (COMB-KOH4-GAIJGNKBN          =     "1")
               EVALUATE   TRUE
                   WHEN  (MAX-KIN-KOHI4-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI4-M   NOT =   ZERO)
                     IF    ZAN-KOHI4-D            >    ZAN-KOHI4-M
                           MOVE   ZAN-KOHI4-M     TO   ZAN-KOHI4
                     ELSE
                           MOVE   ZAN-KOHI4-D     TO   ZAN-KOHI4
                     END-IF
                   WHEN  (MAX-KIN-KOHI4-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI4-M   =       ZERO)
                           MOVE   ZAN-KOHI4-D     TO   ZAN-KOHI4
                   WHEN  (MAX-KIN-KOHI4-D   =       ZERO)   AND
                         (MAX-KIN-KOHI4-M   NOT =   ZERO)
                           MOVE   ZAN-KOHI4-M     TO   ZAN-KOHI4
                   WHEN  (MAX-KIN-KOHI4-D   =       ZERO)   AND
                         (MAX-KIN-KOHI4-M   =       ZERO)
                           MOVE   ZERO            TO   ZAN-KOHI4
               END-EVALUATE
               IF   (MAX-KIN-KOHI4-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI4-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI4   =   SPACE
                   IF    MAX-KIN-KOHI4-K  NOT =  ZERO
                          IF   FTN-KOHI4-D-K     >=  MAX-KIN-KOHI4-K
                            MOVE     ZERO         TO  ZAN-KOHI4
                          ELSE
                            COMPUTE  ZAN-KOHI4   =   MAX-KIN-KOHI4-K
                                                 -   FTN-KOHI4-D-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI4-K     TO   ZAN-KOHI4
                   ELSE
                     IF    TEI-KIN-KOHI4-K  NOT =  ZERO
                          IF   FTN-KOHI4-D-K     >=  TEI-KIN-KOHI4-K
                            MOVE     ZERO         TO  ZAN-KOHI4
                          ELSE
                            COMPUTE  ZAN-KOHI4   =   TEI-KIN-KOHI4-K
                                                 -   FTN-KOHI4-D-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI4-K     TO   ZAN-KOHI4
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *
               EVALUATE   TRUE                                          
                   WHEN  (MAX-KIN-KOHI4-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI4-M   NOT =   ZERO)               
                     IF    ZAN-KOHI4-D2           >    ZAN-KOHI4-M2     
                           MOVE   ZAN-KOHI4-M2    TO   ZAN-KOHI42       
                     ELSE                                               
                           MOVE   ZAN-KOHI4-D2    TO   ZAN-KOHI42       
                     END-IF                                             
                   WHEN  (MAX-KIN-KOHI4-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI4-M   =       ZERO)               
                           MOVE   ZAN-KOHI4-D2    TO   ZAN-KOHI42       
                   WHEN  (MAX-KIN-KOHI4-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI4-M   NOT =   ZERO)               
                           MOVE   ZAN-KOHI4-M2    TO   ZAN-KOHI42       
                   WHEN  (MAX-KIN-KOHI4-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI4-M   =       ZERO)               
                           MOVE   ZERO            TO   ZAN-KOHI42       
               END-EVALUATE                                             
               IF   (MAX-KIN-KOHI4-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI4-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI4   =   SPACE
                   IF    MAX-KIN-KOHI4-K  NOT =  ZERO
                          IF   FTN-KOHI4-D2-K    >=  MAX-KIN-KOHI4-K
                            MOVE     ZERO         TO  ZAN-KOHI42
                          ELSE
                            COMPUTE  ZAN-KOHI42  =   MAX-KIN-KOHI4-K
                                                 -   FTN-KOHI4-D2-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI4-K     TO   ZAN-KOHI42
                   ELSE
                     IF    TEI-KIN-KOHI4-K  NOT =  ZERO
                          IF   FTN-KOHI4-D2-K    >=  TEI-KIN-KOHI4-K
                            MOVE     ZERO         TO  ZAN-KOHI42
                          ELSE
                            COMPUTE  ZAN-KOHI42  =   TEI-KIN-KOHI4-K
                                                 -   FTN-KOHI4-D2-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI4-K     TO   ZAN-KOHI42
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *
               EVALUATE   TRUE                                          
                   WHEN  (MAX-KIN-KOHI4-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI4-M   NOT =   ZERO)               
                     IF    ZAN-KOHI4-D3           >    ZAN-KOHI4-M3     
                           MOVE   ZAN-KOHI4-M3    TO   ZAN-KOHI43       
                     ELSE                                               
                           MOVE   ZAN-KOHI4-D3    TO   ZAN-KOHI43       
                     END-IF                                             
                   WHEN  (MAX-KIN-KOHI4-D   NOT =   ZERO)   AND         
                         (MAX-KIN-KOHI4-M   =       ZERO)               
                           MOVE   ZAN-KOHI4-D3    TO   ZAN-KOHI43       
                   WHEN  (MAX-KIN-KOHI4-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI4-M   NOT =   ZERO)               
                           MOVE   ZAN-KOHI4-M3    TO   ZAN-KOHI43       
                   WHEN  (MAX-KIN-KOHI4-D   =       ZERO)   AND         
                         (MAX-KIN-KOHI4-M   =       ZERO)               
                           MOVE   ZERO            TO   ZAN-KOHI43       
               END-EVALUATE                                             
               IF   (MAX-KIN-KOHI4-D    NOT =  ZERO)
               OR   (MAX-KIN-KOHI4-M    NOT =  ZERO)
                    CONTINUE
               ELSE
                 IF    FLG-SANTEI4   =   SPACE
                   IF    MAX-KIN-KOHI4-K  NOT =  ZERO
                          IF   FTN-KOHI4-D3-K    >=  MAX-KIN-KOHI4-K
                            MOVE     ZERO         TO  ZAN-KOHI43
                          ELSE
                            COMPUTE  ZAN-KOHI43  =   MAX-KIN-KOHI4-K
                                                 -   FTN-KOHI4-D3-K
                          END-IF
      ***                 MOVE    MAX-KIN-KOHI4-K     TO   ZAN-KOHI43
                   ELSE
                     IF    TEI-KIN-KOHI4-K  NOT =  ZERO
                          IF   FTN-KOHI4-D3-K    >=  TEI-KIN-KOHI4-K
                            MOVE     ZERO         TO  ZAN-KOHI43
                          ELSE
                            COMPUTE  ZAN-KOHI43  =   TEI-KIN-KOHI4-K
                                                 -   FTN-KOHI4-D3-K
                          END-IF
      ***                 MOVE    TEI-KIN-KOHI4-K     TO   ZAN-KOHI43
                     END-IF
                   END-IF
                 END-IF
               END-IF
           END-IF.
      *
      *    ÂçºåÉÜÃÏÊý¸øÈñ¡ÜÄ¹´ü¤ÎÃÏÊý¸øÈñ»Ä³ÛÃÖ¤­´¹¤¨
           PERFORM   2308-ZANOKIKAE-HENSYU-SEC.
      *
      *    ¼çÊÝ¸±ÉéÃ´¶â³Û¤Î¸øÈñ¸ÂÅÙ³ÛÂÐ±þÍÑ¡¡»Ä¶â³Û·×»»
           IF      FLG-SYUHEN   =   "1"
                PERFORM    2301-SYUHEN-ZANKIN-KEISAN-SEC
           END-IF.
      *    ¼çÊÝ¸±ÉéÃ´¶â³Û¤Î±ßÃ±°Ì¡¡»Ä¶â³Û·×»»
           IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1")
           OR  (FLG-SYUHEN                   =   "1")
           OR  (FLG-KOHININBARAI             =    1 )
           OR  (FLG-GENBUTU70            NOT =  ZERO)
                PERFORM    2302-SYUENTANI-ZANKIN-KEISAN-SEC
           END-IF.
      *
      *    ÊÝ¸±¡¦¸øÈñËè¤Î´µ¼ÔÉéÃ´¶â³Û¤Î·×»»
           MOVE     ZERO     TO    WK-KOH-FTNRATE.
           IF    ((COMB-KOH1-HKNKOHSBTKBN    =   "3")    AND
                  (COMB-KOH1HKNNUM           =   "027")) OR 
                 ((COMB-KOH2-HKNKOHSBTKBN    =   "3")    AND
                  (COMB-KOH2HKNNUM           =   "027")) OR 
                 ((COMB-KOH3-HKNKOHSBTKBN    =   "3")    AND
                  (COMB-KOH3HKNNUM           =   "027")) OR 
                 ((COMB-KOH4-HKNKOHSBTKBN    =   "3")    AND
                  (COMB-KOH4HKNNUM           =   "027")) 
                  CONTINUE
           ELSE
               IF    COMB-HKNNUM             NOT =       SPACE
                  PERFORM    2303-SYUHOKEN-KEISAN-SEC
               END-IF
           END-IF.
      *
           MOVE     ZERO     TO     WK-TOKUTEI-COMPFTN3.
           MOVE     SPACE    TO     FLG-KUMAMOTO.
           INITIALIZE               WK-TAIHI-RECEFTNDSPKBN-OCC.
      *
           INITIALIZE               WK-SYOUKAN-FLG-TAIHI-AREA.
      *
           MOVE    ZERO      TO     WK-TOKUSYU-COMPFTN.
           MOVE    9999999   TO     WK-TOKUSYU-COMPFTN2.
           MOVE    ZERO      TO     WK-TOKUSYU-ENTANI.
      *
           MOVE    9999999   TO     WK-MIYAGI-COMPFTN.
           MOVE    9999999   TO     WK-SEISIN-COMPFTN.
      *
           MOVE    ZERO      TO     WK-CYOKI-ZAN.
           MOVE    ZERO      TO     WK-CYOKI-ZAN-ENTANI.
           IF   (COMB-KOH1HKNNUM     = "972") OR
                (COMB-KOH2HKNNUM     = "972") OR
                (COMB-KOH3HKNNUM     = "972") OR
                (COMB-KOH4HKNNUM     = "972")
                 IF   WK-CYOKI         >   10000
                    MOVE   ZERO   TO   WK-CYOKI-ZAN
                 ELSE
                    COMPUTE WK-CYOKI-ZAN  =  10000
                                          -  WK-CYOKI
                 END-IF
                 IF   WK-CYOKI-ENTANI  >   10000
                    MOVE   ZERO   TO   WK-CYOKI-ZAN-ENTANI
                 ELSE
                    COMPUTE WK-CYOKI-ZAN-ENTANI =  10000
                                                -  WK-CYOKI-ENTANI
                 END-IF
           END-IF.
           IF   (COMB-KOH1HKNNUM     = "974") OR
                (COMB-KOH2HKNNUM     = "974") OR
                (COMB-KOH3HKNNUM     = "974") OR
                (COMB-KOH4HKNNUM     = "974")
                 IF   WK-CYOKI         >   20000
                    MOVE   ZERO   TO   WK-CYOKI-ZAN
                 ELSE
                    COMPUTE WK-CYOKI-ZAN  =  20000
                                          -  WK-CYOKI
                 END-IF
                 IF   WK-CYOKI-ENTANI  >   20000
                    MOVE   ZERO   TO   WK-CYOKI-ZAN-ENTANI
                 ELSE
                    COMPUTE WK-CYOKI-ZAN-ENTANI =  20000
                                                -  WK-CYOKI-ENTANI
                 END-IF
           END-IF.
      *
           PERFORM    VARYING   KOH-IDX    FROM   1   BY   1
                      UNTIL    (KOH-IDX    >      4)
               IF   COMB-KOHHKNNUM(KOH-IDX)  NOT =  SPACE
                    PERFORM    2304-KOH-HKNREAD-SEC
                    IF   COMB-KOH-GAIJGNKBN(KOH-IDX)  =  "1"
                         PERFORM   2305-KOH-KEISAN-SEC
                    ELSE
                         PERFORM   2306-KOH-KEISAN2-SEC
                    END-IF
               END-IF
           END-PERFORM.
      *
      *     ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â³Û¤«¤éºÇ¾®¤ÎÃÍ¤òÁªÂò¥»¥Ã¥È
            PERFORM   2307-FTNKIN-HIKAKU-SEC.
      *
       230-FTNKIN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÂçºåÉÜÃÏÊý¸øÈñ¡ÜÄ¹´ü¤ÎÃÏÊý¸øÈñ»Ä³ÛÃÖ¤­´¹¤¨
      ******************************************************************
       2308-ZANOKIKAE-HENSYU-SEC           SECTION.
      *
           IF     WK-1001-PREFNUM       =   27
             IF   COMB-KOH1HKNNUM       =  "180" OR "182" OR
                                           "186" OR "188"
                  PERFORM    23081-ZANOKIKAE-HENSYU1-SEC
             END-IF
             IF   COMB-KOH2HKNNUM       =  "180" OR "182" OR
                                           "186" OR "188"
                  PERFORM    23082-ZANOKIKAE-HENSYU2-SEC
             END-IF
             IF   COMB-KOH3HKNNUM       =  "180" OR "182" OR
                                           "186" OR "188"
                  PERFORM    23083-ZANOKIKAE-HENSYU3-SEC
             END-IF
             IF   COMB-KOH4HKNNUM       =  "180" OR "182" OR
                                           "186" OR "188"
                  PERFORM    23084-ZANOKIKAE-HENSYU4-SEC
             END-IF
           END-IF
      *
           IF     WK-1001-PREFNUM       =   34
             IF   COMB-KOH1HKNNUM       =  "191" OR "291"
                  PERFORM    23081-ZANOKIKAE-HENSYU1-SEC
             END-IF
             IF   COMB-KOH2HKNNUM       =  "191" OR "291"
                  PERFORM    23082-ZANOKIKAE-HENSYU2-SEC
             END-IF
             IF   COMB-KOH3HKNNUM       =  "191" OR "291"
                  PERFORM    23083-ZANOKIKAE-HENSYU3-SEC
             END-IF
             IF   COMB-KOH4HKNNUM       =  "191" OR "291"
                  PERFORM    23084-ZANOKIKAE-HENSYU4-SEC
             END-IF
           END-IF
      *
           .
      *
       2308-ZANOKIKAE-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÃÖ¤­´¹¤¨ÊÔ½¸£±
      ******************************************************************
       23081-ZANOKIKAE-HENSYU1-SEC           SECTION.
      *
           IF   COMB-KOH2HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI1  >  ZAN-KOHI2
                 MOVE   ZAN-KOHI2   TO  ZAN-KOHI1
              END-IF
              IF   ZAN-KOHI12 >  ZAN-KOHI22
                 MOVE   ZAN-KOHI22  TO  ZAN-KOHI12
              END-IF
              IF   ZAN-KOHI13 >  ZAN-KOHI23
                 MOVE   ZAN-KOHI23  TO  ZAN-KOHI13
              END-IF
           END-IF
           IF   COMB-KOH3HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI1  >  ZAN-KOHI3
                 MOVE   ZAN-KOHI3   TO  ZAN-KOHI1
              END-IF
              IF   ZAN-KOHI12 >  ZAN-KOHI32
                 MOVE   ZAN-KOHI32  TO  ZAN-KOHI12
              END-IF
              IF   ZAN-KOHI13 >  ZAN-KOHI33
                 MOVE   ZAN-KOHI33  TO  ZAN-KOHI13
              END-IF
           END-IF
           IF   COMB-KOH4HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI1  >  ZAN-KOHI4
                 MOVE   ZAN-KOHI4   TO  ZAN-KOHI1
              END-IF
              IF   ZAN-KOHI12 >  ZAN-KOHI42
                 MOVE   ZAN-KOHI42  TO  ZAN-KOHI12
              END-IF
              IF   ZAN-KOHI13 >  ZAN-KOHI43
                 MOVE   ZAN-KOHI43  TO  ZAN-KOHI13
              END-IF
           END-IF
      *
           .
      *
       23081-ZANOKIKAE-HENSYU1-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÃÖ¤­´¹¤¨ÊÔ½¸£²
      ******************************************************************
       23082-ZANOKIKAE-HENSYU2-SEC           SECTION.
      *
           IF   COMB-KOH1HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI2  >  ZAN-KOHI1
                 MOVE   ZAN-KOHI1   TO  ZAN-KOHI2
              END-IF
              IF   ZAN-KOHI22 >  ZAN-KOHI12
                 MOVE   ZAN-KOHI12  TO  ZAN-KOHI22
              END-IF
              IF   ZAN-KOHI23 >  ZAN-KOHI13
                 MOVE   ZAN-KOHI13  TO  ZAN-KOHI23
              END-IF
           END-IF
           IF   COMB-KOH3HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI2  >  ZAN-KOHI3
                 MOVE   ZAN-KOHI3   TO  ZAN-KOHI2
              END-IF
              IF   ZAN-KOHI22 >  ZAN-KOHI32
                 MOVE   ZAN-KOHI32  TO  ZAN-KOHI22
              END-IF
              IF   ZAN-KOHI23 >  ZAN-KOHI33
                 MOVE   ZAN-KOHI33  TO  ZAN-KOHI23
              END-IF
           END-IF
           IF   COMB-KOH4HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI2  >  ZAN-KOHI4
                 MOVE   ZAN-KOHI4   TO  ZAN-KOHI2
              END-IF
              IF   ZAN-KOHI22 >  ZAN-KOHI42
                 MOVE   ZAN-KOHI42  TO  ZAN-KOHI22
              END-IF
              IF   ZAN-KOHI23 >  ZAN-KOHI43
                 MOVE   ZAN-KOHI43  TO  ZAN-KOHI23
              END-IF
           END-IF
      *
           .
      *
       23082-ZANOKIKAE-HENSYU2-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÃÖ¤­´¹¤¨ÊÔ½¸£³
      ******************************************************************
       23083-ZANOKIKAE-HENSYU3-SEC           SECTION.
      *
           IF   COMB-KOH1HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI3  >  ZAN-KOHI1
                 MOVE   ZAN-KOHI1   TO  ZAN-KOHI3
              END-IF
              IF   ZAN-KOHI32 >  ZAN-KOHI12
                 MOVE   ZAN-KOHI12  TO  ZAN-KOHI32
              END-IF
              IF   ZAN-KOHI33 >  ZAN-KOHI13
                 MOVE   ZAN-KOHI13  TO  ZAN-KOHI33
              END-IF
           END-IF
           IF   COMB-KOH2HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI3  >  ZAN-KOHI2
                 MOVE   ZAN-KOHI2   TO  ZAN-KOHI3
              END-IF
              IF   ZAN-KOHI32 >  ZAN-KOHI22
                 MOVE   ZAN-KOHI22  TO  ZAN-KOHI32
              END-IF
              IF   ZAN-KOHI33 >  ZAN-KOHI23
                 MOVE   ZAN-KOHI23  TO  ZAN-KOHI33
              END-IF
           END-IF
           IF   COMB-KOH4HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI3  >  ZAN-KOHI4
                 MOVE   ZAN-KOHI4   TO  ZAN-KOHI3
              END-IF
              IF   ZAN-KOHI32 >  ZAN-KOHI42
                 MOVE   ZAN-KOHI42  TO  ZAN-KOHI32
              END-IF
              IF   ZAN-KOHI33 >  ZAN-KOHI43
                 MOVE   ZAN-KOHI43  TO  ZAN-KOHI33
              END-IF
           END-IF
      *
           .
      *
       23083-ZANOKIKAE-HENSYU3-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÃÖ¤­´¹¤¨ÊÔ½¸£´
      ******************************************************************
       23084-ZANOKIKAE-HENSYU4-SEC           SECTION.
      *
           IF   COMB-KOH1HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI4  >  ZAN-KOHI1
                 MOVE   ZAN-KOHI1   TO  ZAN-KOHI4
              END-IF
              IF   ZAN-KOHI42 >  ZAN-KOHI12
                 MOVE   ZAN-KOHI12  TO  ZAN-KOHI42
              END-IF
              IF   ZAN-KOHI43 >  ZAN-KOHI13
                 MOVE   ZAN-KOHI13  TO  ZAN-KOHI43
              END-IF
           END-IF
           IF   COMB-KOH2HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI4  >  ZAN-KOHI2
                 MOVE   ZAN-KOHI2   TO  ZAN-KOHI4
              END-IF
              IF   ZAN-KOHI42 >  ZAN-KOHI22
                 MOVE   ZAN-KOHI22  TO  ZAN-KOHI42
              END-IF
              IF   ZAN-KOHI43 >  ZAN-KOHI23
                 MOVE   ZAN-KOHI23  TO  ZAN-KOHI43
              END-IF
           END-IF
           IF   COMB-KOH3HKNNUM  =  "972" OR "974"
              IF   ZAN-KOHI4  >  ZAN-KOHI3
                 MOVE   ZAN-KOHI3   TO  ZAN-KOHI4
              END-IF
              IF   ZAN-KOHI42 >  ZAN-KOHI32
                 MOVE   ZAN-KOHI32  TO  ZAN-KOHI42
              END-IF
              IF   ZAN-KOHI43 >  ZAN-KOHI33
                 MOVE   ZAN-KOHI33  TO  ZAN-KOHI43
              END-IF
           END-IF
      *
           .
      *
       23084-ZANOKIKAE-HENSYU4-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¼çÊÝ¸±ÉéÃ´¶â³Û¤Î¸øÈñ¸ÂÅÙ³ÛÂÐ±þÍÑ¡¡»Ä¶â³Û·×»»
      ******************************************************************
       2301-SYUHEN-ZANKIN-KEISAN-SEC           SECTION.
      *
      *    »»Äê²ÄÇ½¥Õ¥é¥°¤Î½é´ü²½
           MOVE     SPACE        TO     FLG-SANTEI1-HEN
                                        FLG-SANTEI-SYU-HEN
                                        FLG-SANTEI1-HEN-ENTANI
                                        FLG-SANTEI-SYU-HEN-ENTANI.
      *
      *    ²áµî¤Î´µ¼ÔÊÝ¸±ÉéÃ´Îß·×³Û¤È¾å¸Â¶â³Û¤ÎÈæ³Ó
      *
           IF   COMB-KOH1HKNNUM      =    "027"
             IF       MAX-KIN-KOHI1-D        NOT =  ZERO
               IF   FTN-KOHI1-D-HEN        >=     MAX-KIN-KOHI1-D
                       MOVE     "1"       TO     FLG-SANTEI1-HEN
               END-IF
               IF   FTN-KOHI1-D-HEN-ENTANI >=     MAX-KIN-KOHI1-D
                       MOVE     "1"       TO     FLG-SANTEI1-HEN-ENTANI
               END-IF
             END-IF
             IF       MAX-KIN-KOHI1-M        NOT =  ZERO
               IF   FTN-KOHI1-M-HEN        >=     MAX-KIN-KOHI1-M
                       MOVE     "1"       TO     FLG-SANTEI1-HEN
               END-IF
               IF   FTN-KOHI1-M-HEN-ENTANI >=     MAX-KIN-KOHI1-M
                       MOVE     "1"       TO     FLG-SANTEI1-HEN-ENTANI
               END-IF
             END-IF
           END-IF.
      *
           IF         MAX-KIN-SYU-M          NOT =  ZERO
               IF  FTN-SYU-M-HEN           >=     MAX-KIN-SYU-M
                      MOVE  "1"           TO     FLG-SANTEI-SYU-HEN
               END-IF
               IF  FTN-SYU-M-HEN-ENTANI    >=     MAX-KIN-SYU-M
                      MOVE  "1"    TO     FLG-SANTEI-SYU-HEN-ENTANI
               END-IF
           END-IF.
      *
      *    ÊÝ¸±¡Ê¸øÈñ¡Ë¥Æ¡¼¥Ö¥ë¤Î´µ¼ÔÉéÃ´¾å¸Â³Û¤ÈÅö·î¤Î´µ¼ÔÉéÃ´Îß·×³Û
      *    ¤«¤é»»Äê²ÄÇ½»Ä³Û¤ò»»½Ð¤¹¤ë
      *
           IF  FLG-SANTEI1-HEN           =     "1"
               MOVE     ZERO             TO    ZAN-KOHI1-D-HEN
                                               ZAN-KOHI1-M-HEN
           ELSE
               IF  COMB-KOH1HKNNUM           =   "027"
                   IF       MAX-KIN-KOHI1-D       NOT = ZERO
                       COMPUTE  ZAN-KOHI1-D-HEN  =   MAX-KIN-KOHI1-D
                                                 -   FTN-KOHI1-D-HEN
                   END-IF
                   IF       MAX-KIN-KOHI1-M       NOT = ZERO
                       COMPUTE  ZAN-KOHI1-M-HEN  =   MAX-KIN-KOHI1-M
                                                 -   FTN-KOHI1-M-HEN
                   END-IF
               END-IF
           END-IF.
           IF  FLG-SANTEI1-HEN-ENTANI    =     "1"
               MOVE     ZERO             TO    ZAN-KOHI1-D-HEN-ENTANI
                                               ZAN-KOHI1-M-HEN-ENTANI
           ELSE
               IF  COMB-KOH1HKNNUM           =   "027"
                   IF       MAX-KIN-KOHI1-D       NOT = ZERO
                       COMPUTE  ZAN-KOHI1-D-HEN-ENTANI
                                            =   MAX-KIN-KOHI1-D
                                            -   FTN-KOHI1-D-HEN-ENTANI
                   END-IF
                   IF       MAX-KIN-KOHI1-M       NOT = ZERO
                       COMPUTE  ZAN-KOHI1-M-HEN-ENTANI
                                            =   MAX-KIN-KOHI1-M
                                            -   FTN-KOHI1-M-HEN-ENTANI
                   END-IF
               END-IF
           END-IF.
      *
           IF  FLG-SANTEI-SYU-HEN        =     "1"
               MOVE     ZERO             TO    ZAN-SYU-M-HEN
           ELSE
               IF       MAX-KIN-SYU-M    NOT = ZERO
                     COMPUTE  ZAN-SYU-M-HEN  =   MAX-KIN-SYU-M
                                             -   FTN-SYU-M-HEN
               END-IF
           END-IF.
           IF  FLG-SANTEI-SYU-HEN-ENTANI =     "1"
               MOVE     ZERO             TO    ZAN-SYU-M-HEN-ENTANI
           ELSE
               IF       MAX-KIN-SYU-M    NOT = ZERO
                     COMPUTE  ZAN-SYU-M-HEN-ENTANI
                                             =   MAX-KIN-SYU-M
                                             -   FTN-SYU-M-HEN-ENTANI
               END-IF
           END-IF.
      *
           IF  COMB-KOH1HKNNUM           =   "027"
      *
               EVALUATE   TRUE
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)
                     IF    ZAN-KOHI1-D-HEN    >    ZAN-KOHI1-M-HEN
                           MOVE   ZAN-KOHI1-M-HEN     TO   ZAN-KOHI1-HEN
                     ELSE
                           MOVE   ZAN-KOHI1-D-HEN     TO   ZAN-KOHI1-HEN
                     END-IF
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI1-M   =       ZERO)
                           MOVE   ZAN-KOHI1-D-HEN     TO   ZAN-KOHI1-HEN
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)
                           MOVE   ZAN-KOHI1-M-HEN     TO   ZAN-KOHI1-HEN
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND
                         (MAX-KIN-KOHI1-M   =       ZERO)
                           MOVE   ZERO                TO   ZAN-KOHI1-HEN
               END-EVALUATE
      *
               EVALUATE   TRUE
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)
                     IF    ZAN-KOHI1-D-HEN-ENTANI    >
                                          ZAN-KOHI1-M-HEN-ENTANI
                           MOVE   ZAN-KOHI1-M-HEN-ENTANI
                                        TO   ZAN-KOHI1-HEN-ENTANI
                     ELSE
                           MOVE   ZAN-KOHI1-D-HEN-ENTANI
                                        TO   ZAN-KOHI1-HEN-ENTANI
                     END-IF
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI1-M   =       ZERO)
                           MOVE   ZAN-KOHI1-D-HEN-ENTANI
                                        TO   ZAN-KOHI1-HEN-ENTANI
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)
                           MOVE   ZAN-KOHI1-M-HEN-ENTANI
                                        TO   ZAN-KOHI1-HEN-ENTANI
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND
                         (MAX-KIN-KOHI1-M   =       ZERO)
                           MOVE   ZERO  TO   ZAN-KOHI1-HEN-ENTANI
               END-EVALUATE
      *
           END-IF.
      *
       2301-SYUHEN-ZANKIN-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¼çÊÝ¸±ÉéÃ´¶â³Û¤Î±ßÃ±°Ì¡¡»Ä¶â³Û·×»»
      ******************************************************************
       2302-SYUENTANI-ZANKIN-KEISAN-SEC           SECTION.
      *
      *    »»Äê²ÄÇ½¥Õ¥é¥°¤Î½é´ü²½
           MOVE     SPACE                  TO     FLG-SANTEI1-ENTANI
                                                  FLG-SANTEI-SYU-ENTANI.
      *
      *    ²áµî¤Î´µ¼ÔÊÝ¸±ÉéÃ´Îß·×³Û¤È¾å¸Â¶â³Û¤ÎÈæ³Ó
      *
           IF   COMB-KOH1HKNNUM      =    "027"
             IF       MAX-KIN-KOHI1-D        NOT =  ZERO
               IF   FTN-KOHI1-D-ENTANI     >=     MAX-KIN-KOHI1-D
                       MOVE     "1"       TO     FLG-SANTEI1-ENTANI
               END-IF
             END-IF
             IF       MAX-KIN-KOHI1-M        NOT =  ZERO
               IF   FTN-KOHI1-M-ENTANI     >=     MAX-KIN-KOHI1-M
                       MOVE     "1"       TO     FLG-SANTEI1-ENTANI
               END-IF
             END-IF
           END-IF.
      *
           IF         MAX-KIN-SYU-M          NOT =  ZERO
               IF  FTN-SYU-M-ENTANI        >=     MAX-KIN-SYU-M
                      MOVE  "1"           TO     FLG-SANTEI-SYU-ENTANI
               END-IF
           END-IF.
      *
      *    ÊÝ¸±¡Ê¸øÈñ¡Ë¥Æ¡¼¥Ö¥ë¤Î´µ¼ÔÉéÃ´¾å¸Â³Û¤ÈÅö·î¤Î´µ¼ÔÉéÃ´Îß·×³Û
      *    ¤«¤é»»Äê²ÄÇ½»Ä³Û¤ò»»½Ð¤¹¤ë
      *
           IF  FLG-SANTEI1-ENTANI          =     "1"
               MOVE     ZERO             TO    ZAN-KOHI1-D-ENTANI
                                               ZAN-KOHI1-M-ENTANI
           ELSE
               IF  COMB-KOH1HKNNUM           =   "027"
                   IF       MAX-KIN-KOHI1-D       NOT = ZERO
                     COMPUTE  ZAN-KOHI1-D-ENTANI =   MAX-KIN-KOHI1-D
                                                 -   FTN-KOHI1-D-ENTANI
                   END-IF
                   IF       MAX-KIN-KOHI1-M       NOT = ZERO
                     COMPUTE  ZAN-KOHI1-M-ENTANI =   MAX-KIN-KOHI1-M
                                                 -   FTN-KOHI1-M-ENTANI
                   END-IF
               END-IF
           END-IF.
      *
           IF  FLG-SANTEI-SYU-ENTANI         =     "1"
               MOVE     ZERO             TO    ZAN-SYU-M-ENTANI
           ELSE
               IF       MAX-KIN-SYU-M    NOT = ZERO
                     COMPUTE  ZAN-SYU-M-ENTANI  =   MAX-KIN-SYU-M
                                                -   FTN-SYU-M-ENTANI
               END-IF
           END-IF.
      *
           IF  COMB-KOH1HKNNUM           =   "027"
      *
               EVALUATE   TRUE
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)
                     IF    ZAN-KOHI1-D-ENTANI  >    ZAN-KOHI1-M-ENTANI
                        MOVE   ZAN-KOHI1-M-ENTANI  TO  ZAN-KOHI1-ENTANI
                     ELSE
                        MOVE   ZAN-KOHI1-D-ENTANI  TO  ZAN-KOHI1-ENTANI
                     END-IF
                   WHEN  (MAX-KIN-KOHI1-D   NOT =   ZERO)   AND
                         (MAX-KIN-KOHI1-M   =       ZERO)
                        MOVE   ZAN-KOHI1-D-ENTANI  TO   ZAN-KOHI1-ENTANI
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND
                         (MAX-KIN-KOHI1-M   NOT =   ZERO)
                        MOVE   ZAN-KOHI1-M-ENTANI  TO   ZAN-KOHI1-ENTANI
                   WHEN  (MAX-KIN-KOHI1-D   =       ZERO)   AND
                         (MAX-KIN-KOHI1-M   =       ZERO)
                        MOVE   ZERO                TO   ZAN-KOHI1-ENTANI
               END-EVALUATE
      *
           END-IF.
      *
       2302-SYUENTANI-ZANKIN-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¼çÊÝ¸±¤Î´µ¼ÔÉéÃ´¶â³Û·×»»                                *
      ******************************************************************
       2303-SYUHOKEN-KEISAN-SEC           SECTION.
      *
      *    ³ºÅöÊÝ¸±ÈÖ¹æ¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
           INITIALIZE                      HKNNUM-REC.
           MOVE   COMB-HOSPID        TO    HKN-HOSPID.
           MOVE   COMB-HKNNUM        TO    HKN-HKNNUM.
           MOVE   "00"               TO    WK-PAYKBN.
           MOVE   HKNNUM-REC         TO    MCPDATA-REC.
           MOVE   "DBSELECT"         TO    MCP-FUNC.
           MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH.
           CALL   "ORCMCPSUB"         USING MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC.
           IF      MCP-RC            =     ZERO
               MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
               PERFORM  HKNNUM-READ-SEC
               PERFORM  UNTIL
                 (FLG-HKNNUM     =    1)                       OR
                ((HKN-TEKSTYMD(1:6)   <=  LNK-SYU-SRYYMD(1:6)) AND
                 (LNK-SYU-SRYYMD(1:6) <=  HKN-TEKEDYMD(1:6))   AND
                 (WK-PAYKBN            =  HKN-PAYKBN))
                   PERFORM   HKNNUM-READ-SEC
               END-PERFORM
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥Æ¡¼¥Ö¥ë¤ÎÊä½õ¶èÊ¬¤«¤é´µ¼Ô¤ÎÉéÃ´³ä¹ç¤òÊÝ¸±ÈÖ¹æ
      *    ¥Æ¡¼¥Ö¥ë¤ËºÆ¥»¥Ã¥È¡Ê¹ñÊÝ¤Î¤ß¡Ë
           IF    COMB-HKNNUM           =    "060"
               EVALUATE   COMB-HOJOKBN
                   WHEN   "0"
                          MOVE   ZERO      TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
                   WHEN   "1"
                   WHEN   "4"
                          MOVE   10        TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
                   WHEN   "2"
                   WHEN   "5"
                          MOVE   20        TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
                   WHEN   "3"
                   WHEN   "6"
                          MOVE   30        TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
               END-EVALUATE
      *
      *    ·òÊÝ¤Î¹âÎð¼Ô¤Ë¤Ä¤¤¤Æ¤âÊä½õ¶èÊ¬¤è¤êÉéÃ´³ä¹ç¥»¥Ã¥È
           ELSE
               EVALUATE   COMB-HOJOKBN
                   WHEN   "9"
                          MOVE   10        TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
                   WHEN   "8"
                          MOVE   20        TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
                   WHEN   "7"
                          MOVE   30        TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
               END-EVALUATE
      *
           END-IF.
      *
      *    ²¬»³¸©À¾°ÀÁÒÂ¼Âà¿¦¼Ô¹ñÊÝ¡ÊÁ°´ü¹âÎð¼Ô¤ò½ü¤¯¡Ë
           IF  (FLG-TOKUREI-HKNJYA    =     1  )
           AND (COMB-HOJOKBN          =   SPACE)
                 MOVE   20      TO   HKN-HON-GAIKFTNRATE
                                     HKN-KZK-GAIKFTNRATE
                                     HKN-TSY-GAIKFTNRATE
                                     HKN-TNK-GAIKFTNRATE
           END-IF.
      *
      *    Ç¯Îð£³ºÐÌ¤Ëþ
           IF      LNK-SYU-SRYYMD       >=      "20021001"
               INITIALIZE                           PTINF-REC
               MOVE    LNK-SYU-HOSPID       TO      PTINF-HOSPID
               MOVE    LNK-SYU-PTID         TO      PTINF-PTID
               MOVE    PTINF-REC            TO      MCPDATA-REC
               MOVE    "DBSELECT"           TO      MCP-FUNC
               MOVE    "PTINF-KEY"          TO      ORC-DBPATH
               CALL    "ORCMCPSUB"           USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
               IF      MCP-RC               =       ZERO
                   MOVE    "PTINF-KEY"      TO      ORC-DBPATH
                   PERFORM  PTINF-READ-SEC
                   IF       MCP-RC          =       ZERO
                        MOVE   PTINF-BIRTHDAY   TO  WRK-YMDS1
                        MOVE   LNK-SYU-SRYYMD   TO  WRK-YMDS2
                        MOVE   ZERO             TO  WRK-NENREI
                        COMPUTE  WRK-NENREI     =   WRK-YYS2
                                                -   WRK-YYS1
                        IF   WRK-YMDS1(5:4)     >   WRK-YMDS2(5:4)
                             ADD   -1           TO  WRK-NENREI
                        END-IF
                        IF   WRK-NENREI         =   3
                        AND  WRK-YMDS1(5:4)    <=   WRK-YMDS2(5:4)
                            IF    WRK-YMDS1(5:2)  = WRK-YMDS2(5:2)
                                  IF   WRK-YMDS1(7:2)  NOT = "01"
                                       ADD   -1     TO  WRK-NENREI
                                  END-IF
                            END-IF
                        END-IF
                   ELSE
                        DISPLAY  "PTINF FETCH ERR"
                        MOVE     ZERO   TO      WRK-NENREI
                   END-IF
               ELSE
                   DISPLAY   "PTINF SELECT ERR"
                   MOVE     ZERO   TO      WRK-NENREI
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "PTINF-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
      *
               IF      WRK-NENREI         <     3
                   IF   COMB-HKNNUM       =     "060"
                       IF   COMB-HOJOKBN  =     "0"  OR  "1"
                                                     OR  "4"
                           CONTINUE
                       ELSE
                           MOVE   20      TO   HKN-HON-GAIKFTNRATE
                                               HKN-KZK-GAIKFTNRATE
                                               HKN-TSY-GAIKFTNRATE
                                               HKN-TNK-GAIKFTNRATE
                       END-IF
                   ELSE
                       IF    HKN-HKNKOHSBTKBN  =  "1" 
                           MOVE   20      TO   HKN-HON-GAIKFTNRATE
                                               HKN-KZK-GAIKFTNRATE
                                               HKN-TSY-GAIKFTNRATE
                                               HKN-TNK-GAIKFTNRATE
                       END-IF 
                   END-IF
               END-IF
           END-IF.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥Æ¡¼¥Ö¥ë¤ÎÊä½õ¶èÊ¬¤«¤é´µ¼Ô¤ÎÉéÃ´³ä¹ç¤òÊÝ¸±ÈÖ¹æ
      *    ¥Æ¡¼¥Ö¥ë¤ËºÆ¥»¥Ã¥È¡ÊÁ¥°÷ÊÝ¸±»þ¡Ë
           IF    COMB-HKNNUM                =    "002"
           AND   LNK-S01-SENINFLG(IDY)  NOT =    "1"  
               EVALUATE   COMB-HOJOKBN
                   WHEN   "1"
                   WHEN   "2"
                   WHEN   "3"
                   WHEN   "A"
                   WHEN   "B"
                   WHEN   "C"
                   WHEN   "D"
                   WHEN   "E"
                   WHEN   "F"
                   WHEN   "G"
                   WHEN   "H"
                   WHEN   "I"
                          MOVE   ZERO      TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
               END-EVALUATE
           END-IF.
      *    ÊÝ¸±ÁÈ¹ç¤»¥Æ¡¼¥Ö¥ë¤ÎÊä½õ¶èÊ¬¤«¤é´µ¼Ô¤ÎÉéÃ´³ä¹ç¤òÊÝ¸±ÈÖ¹æ
      *    ¥Æ¡¼¥Ö¥ë¤ËºÆ¥»¥Ã¥È¡Ê¶¦ºÑÁÈ¹çÊÝ¸±»þ¡Ë
           IF    COMB-HKNNUM    =    "031" OR "032" OR "033" OR "034"
               EVALUATE   COMB-HOJOKBN
                   WHEN   "2"
                   WHEN   "B"
                   WHEN   "E"  
                   WHEN   "H"
                          MOVE   ZERO      TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
               END-EVALUATE
           END-IF.
      *
      *    ÆÃÊÌÎÅÍÜÈñ
           IF    COMB-HKNNUM    =    "068" OR "069"
                          MOVE   100       TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
           END-IF.
      *
      *    ´µ¼ÔÉéÃ´¶â»»½ÐÁ°¤Î£±£°£°¡óÉéÃ´³Û»»½Ð
           IF     WK-HKNKBN         =   "1"
           OR     COMB-HKNNUM(1:1)  =   "0"
                 MOVE    10    TO      HKN-TENTNK
           ELSE
             IF   HKN-TENTNK   =   ZERO
                 MOVE    10    TO      HKN-TENTNK
             END-IF
           END-IF.
           MOVE      ZERO      TO      WK-ZENGAKU.
           PERFORM   VARYING   IDX9    FROM    1   BY  1
                     UNTIL     IDX9       >   15
               COMPUTE WK-ZENGAKU  =   WK-ZENGAKU  +
                            (LNK-SYU-HKNTEN (IDX9) *  HKN-TENTNK)
           END-PERFORM.
      *
           IF    COMB-SYU-HKNKOHSBTKBN       =   "8"
           AND   HKN-TENTNK              NOT =   10
               IF   HKN-HON-GAIKFTNRATE     NOT =   ZERO
                   MOVE     100       TO   HKN-HON-GAIKFTNRATE
               END-IF
               IF   HKN-KZK-GAIKFTNRATE     NOT =   ZERO
                   MOVE     100       TO   HKN-KZK-GAIKFTNRATE
               END-IF
           END-IF.
      *
      *    ¡Ê£×£Ë¡Ý£È£Ë£Î£Ë£Â£Î¡¡¶õÇò¡§°åÊÝ¡¡£±¡§Ï«ºÒ¡¡£²¡§¼«ÇåÀÕ¡Ë
           EVALUATE   WK-HKNKBN
             WHEN   SPACE
               IF   COMB-HONKZKKBN        =   "1"
                        MOVE  HKN-HON-GAIKFTNRATE  TO  WK-KOH-FTNRATE
                        IF    HKN-HON-GAIKFTNRATE     NOT =    ZERO
                              COMPUTE   WK-FUTANWARI  =
                                        HKN-HON-GAIKFTNRATE  /  100
                              COMPUTE   LNK-SYU-SYUCOMPFTN  =
                                        WK-ZENGAKU   *  WK-FUTANWARI
                              COMPUTE   LNK-SYU-SYUCOMPFTN-ENTANI  =
                                        WK-ZENGAKU   *  WK-FUTANWARI
                              IF   LNK-S01-SENINFLG(IDY)    =  "1"      
                                IF LNK-SYU-SYUCOMPFTN       >  200      
                                   MOVE   200  TO
                                           LNK-SYU-SYUCOMPFTN
                                           LNK-SYU-SYUCOMPFTN-ENTANI
                                END-IF                                  
                              END-IF                                    
                              IF  (COMB-SYU-HKNKOHSBTKBN     = "8" )
                              AND (WK-1001-JIHIPOSITION  NOT = "2" )
                              AND (COMB-HOJOKBN              = "1" )
                              AND (LNK-SYU-SRYYMD(1:6)  >= "200704")
                                 COMPUTE  LNK-SYU-SYUCOMPFTN =
                                          LNK-SYU-SYUCOMPFTN +
                                          WK-SKYMONEY-ZEI
                                 COMPUTE  LNK-SYU-SYUCOMPFTN-ENTANI =
                                          LNK-SYU-SYUCOMPFTN-ENTANI +
                                          WK-SKYMONEY-ZEI
                              END-IF
      *                       ¸º³Û
                              EVALUATE  WK-GENGAKUKBN
                                WHEN        1
                                  IF  WK-GENGAKU-RITU = 100
                                   MOVE  ZERO  TO  WK-GENGAKU-GAK
                                   MOVE  ZERO  TO  WK-GENGAKU-GAK-ENTANI
                                  ELSE
                                   COMPUTE  WK-GENGAKU-RITUX =
                                            100 - WK-GENGAKU-RITU
                                   COMPUTE  WK-GENGAKU-GAK      =
                                            LNK-SYU-SYUCOMPFTN  *
                                            WK-GENGAKU-RITUX    /
                                            100
                                   COMPUTE  WK-GENGAKU-GAK-ENTANI     =
                                            LNK-SYU-SYUCOMPFTN-ENTANI *
                                            WK-GENGAKU-RITUX          /
                                            100
                                  END-IF
                                WHEN        2
                                  CONTINUE
                                WHEN        3
                                WHEN        4
                                  MOVE  ZERO  TO  WK-GENGAKU-GAK
                                  MOVE  ZERO  TO  WK-GENGAKU-GAK-ENTANI
                              END-EVALUATE
                              IF  WK-GENGAKUKBN  NOT =  ZERO
                                  MOVE  WK-GENGAKU-GAK
                                          TO  LNK-SYU-SYUCOMPFTN
                                  MOVE  WK-GENGAKU-GAK-ENTANI
                                          TO  LNK-SYU-SYUCOMPFTN-ENTANI
                              END-IF
                              MOVE      LNK-SYU-SYUCOMPFTN
                                                 TO WK-TOKUTEI-COMPFTN2
                              IF   COMB-SYU-HKNKOHSBTKBN  =  "8"
                              AND  WK-1001-JIHIPOSITION   =  "3"
                              AND  LNK-SYU-SRYYMD(1:6)   >=  "200604"
                                 CONTINUE
                              ELSE
                                 COMPUTE   LNK-SYU-SYUCOMPFTN =
                                           LNK-SYU-SYUCOMPFTN  +  5
                                 COMPUTE   WK-FTN      =
                                           LNK-SYU-SYUCOMPFTN  /  10
                                 COMPUTE   LNK-SYU-SYUCOMPFTN =
                                           WK-FTN   *  10
                              END-IF
                        END-IF
                        MOVE  HKN-HON-GAIYKZFTNKBN
                                             TO   LNK-SYU-SYUYKZFTNKBN
                        IF    HKN-HON-GAIYKZFTNKBN  NOT =  "0"
                            MOVE  YAKFTN-SOGOKEI
                                             TO   LNK-SYU-SYUCOMPYKZFTN
                            IF    HKN-HON-GAIYKZFTNKBN  =  "1"
                                  MOVE  YAKFTN-SOGOKEI
                                             TO   WK-SYU-YAKFTN
                            END-IF
                        END-IF
      *
      *Ê¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ
      *¹âÎð¼Ô¤ÎºßÂðÁí¹ç¡¦ºßÂðËö´ü»»Äê´µ¼Ô
      *
                        IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1")
                        OR  (FLG-SYUHEN                   =   "1")
                        OR  (FLG-KOHININBARAI             =    1 )
                        OR  (FLG-GENBUTU70            NOT =  ZERO)
                            IF  FLG-SYUHEN   =   "1"
                              IF  ZAN-SYU-M-HEN  <  LNK-SYU-SYUCOMPFTN 
                                MOVE ZAN-SYU-M-HEN
                                                 TO  WK-HEN-SYUCOMPFTN 
                              ELSE                                     
                                MOVE LNK-SYU-SYUCOMPFTN                
                                                 TO  WK-HEN-SYUCOMPFTN 
                              END-IF
                              IF  ZAN-SYU-M-HEN-ENTANI
                                          <  LNK-SYU-SYUCOMPFTN-ENTANI
                                MOVE ZAN-SYU-M-HEN-ENTANI
                                         TO  WK-HEN-SYUCOMPFTN-ENTANI
                              ELSE
                                MOVE LNK-SYU-SYUCOMPFTN-ENTANI
                                         TO  WK-HEN-SYUCOMPFTN-ENTANI
                              END-IF
                            END-IF
                            IF  ZAN-SYU-M     <=    WK-TOKUTEI-COMPFTN2 
                                MOVE ZAN-SYU-M   TO  LNK-SYU-SYUCOMPFTN
                                                     WK-TOKUTEI-COMPFTN 
                            ELSE                                        
                                MOVE WK-TOKUTEI-COMPFTN2                
                                                 TO  WK-TOKUTEI-COMPFTN 
                                IF    FLG-KOUGAKUCYO-ININ  =    1
                                   MOVE   ZERO  TO  FLG-KOUGAKUCYO-ININ
                                END-IF
                                IF    FLG-KOUGAKUCYO-GEN70 =    1
                                   MOVE   ZERO  TO  FLG-KOUGAKUCYO-GEN70
                                END-IF
                            END-IF
                            IF  ZAN-SYU-M-ENTANI
                                              <     WK-TOKUTEI-COMPFTN2
                                MOVE  ZAN-SYU-M-ENTANI
                                          TO  LNK-SYU-SYUCOMPFTN-ENTANI
                            END-IF
                        ELSE
                                MOVE WK-TOKUTEI-COMPFTN2                
                                                 TO  WK-TOKUTEI-COMPFTN 
                        END-IF
                        COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
                                  LNK-SYU-SYUCOMPFTN     +
                                  LNK-SYU-SYUCOMPYKZFTN
               ELSE
                        MOVE  HKN-KZK-GAIKFTNRATE  TO  WK-KOH-FTNRATE
                        IF    HKN-KZK-GAIKFTNRATE     NOT =    ZERO
                              COMPUTE   WK-FUTANWARI  =
                                        HKN-KZK-GAIKFTNRATE  /  100
                              COMPUTE   LNK-SYU-SYUCOMPFTN  =
                                        WK-ZENGAKU   *  WK-FUTANWARI
                              COMPUTE   LNK-SYU-SYUCOMPFTN-ENTANI  =
                                        WK-ZENGAKU   *  WK-FUTANWARI
                              IF  (COMB-SYU-HKNKOHSBTKBN     = "8" )
                              AND (WK-1001-JIHIPOSITION  NOT = "2" )
                              AND (COMB-HOJOKBN              = "1" )
                              AND (LNK-SYU-SRYYMD(1:6)  >= "200704")
                                 COMPUTE  LNK-SYU-SYUCOMPFTN =
                                          LNK-SYU-SYUCOMPFTN +
                                          WK-SKYMONEY-ZEI
                                 COMPUTE  LNK-SYU-SYUCOMPFTN-ENTANI =
                                          LNK-SYU-SYUCOMPFTN-ENTANI +
                                          WK-SKYMONEY-ZEI
                              END-IF
      *                       ¸º³Û
                              EVALUATE  WK-GENGAKUKBN
                                WHEN        1
                                  IF  WK-GENGAKU-RITU = 100
                                   MOVE  ZERO  TO  WK-GENGAKU-GAK
                                   MOVE  ZERO  TO  WK-GENGAKU-GAK-ENTANI
                                  ELSE
                                   COMPUTE  WK-GENGAKU-RITUX =
                                            100 - WK-GENGAKU-RITU
                                   COMPUTE  WK-GENGAKU-GAK      =
                                            LNK-SYU-SYUCOMPFTN  *
                                            WK-GENGAKU-RITUX    /
                                            100
                                   COMPUTE  WK-GENGAKU-GAK-ENTANI     =
                                            LNK-SYU-SYUCOMPFTN-ENTANI *
                                            WK-GENGAKU-RITUX          /
                                            100
                                  END-IF
                                WHEN        2
                                  CONTINUE
                                WHEN        3
                                WHEN        4
                                  MOVE  ZERO  TO  WK-GENGAKU-GAK
                                  MOVE  ZERO  TO  WK-GENGAKU-GAK-ENTANI
                              END-EVALUATE
                              IF  WK-GENGAKUKBN  NOT =  ZERO
                                  MOVE  WK-GENGAKU-GAK
                                          TO  LNK-SYU-SYUCOMPFTN
                                  MOVE  WK-GENGAKU-GAK-ENTANI
                                          TO  LNK-SYU-SYUCOMPFTN-ENTANI
                              END-IF
                              MOVE      LNK-SYU-SYUCOMPFTN
                                                 TO WK-TOKUTEI-COMPFTN2
                              IF   COMB-SYU-HKNKOHSBTKBN  =  "8"
                              AND  WK-1001-JIHIPOSITION   =  "3"
                              AND  LNK-SYU-SRYYMD(1:6)   >=  "200604"
                                 CONTINUE
                              ELSE
                                 COMPUTE   LNK-SYU-SYUCOMPFTN =
                                           LNK-SYU-SYUCOMPFTN  +  5
                                 COMPUTE   WK-FTN      =
                                           LNK-SYU-SYUCOMPFTN  /  10
                                 COMPUTE   LNK-SYU-SYUCOMPFTN =
                                           WK-FTN   *  10
                              END-IF
                        END-IF
                        MOVE  HKN-KZK-GAIYKZFTNKBN
                                             TO   LNK-SYU-SYUYKZFTNKBN
                        IF    HKN-KZK-GAIYKZFTNKBN  NOT =  "0"
                            MOVE  YAKFTN-SOGOKEI
                                             TO   LNK-SYU-SYUCOMPYKZFTN
                            IF    HKN-KZK-GAIYKZFTNKBN  =  "1"
                                  MOVE  YAKFTN-SOGOKEI
                                             TO   WK-SYU-YAKFTN
                            END-IF
                        END-IF
      *
      *Ê¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ
      *¹âÎð¼Ô¤ÎºßÂðÁí¹ç¡¦ºßÂðËö´ü»»Äê´µ¼Ô
      *
                        IF  (LNK-S01-KOUREI-ZAITAKU-KBN   =   "1")
                        OR  (FLG-SYUHEN                   =   "1")
                        OR  (FLG-KOHININBARAI             =    1 )
                        OR  (FLG-GENBUTU70            NOT =  ZERO)
                            IF  FLG-SYUHEN   =   "1"
                              IF  ZAN-SYU-M-HEN  <  LNK-SYU-SYUCOMPFTN 
                                MOVE ZAN-SYU-M-HEN
                                                 TO  WK-HEN-SYUCOMPFTN 
                              ELSE                                     
                                MOVE LNK-SYU-SYUCOMPFTN                
                                                 TO  WK-HEN-SYUCOMPFTN 
                              END-IF
                              IF  ZAN-SYU-M-HEN-ENTANI
                                          <  LNK-SYU-SYUCOMPFTN-ENTANI
                                MOVE ZAN-SYU-M-HEN-ENTANI
                                         TO  WK-HEN-SYUCOMPFTN-ENTANI
                              ELSE
                                MOVE LNK-SYU-SYUCOMPFTN-ENTANI
                                         TO  WK-HEN-SYUCOMPFTN-ENTANI
                              END-IF
                            END-IF
                            IF  ZAN-SYU-M     <=    WK-TOKUTEI-COMPFTN2 
                                MOVE ZAN-SYU-M   TO  LNK-SYU-SYUCOMPFTN
                                                     WK-TOKUTEI-COMPFTN 
                            ELSE                                        
                                MOVE WK-TOKUTEI-COMPFTN2                
                                                 TO  WK-TOKUTEI-COMPFTN 
                                IF    FLG-KOUGAKUCYO-ININ  =    1
                                   MOVE   ZERO  TO  FLG-KOUGAKUCYO-ININ
                                END-IF
                                IF    FLG-KOUGAKUCYO-GEN70 =    1
                                   MOVE   ZERO  TO  FLG-KOUGAKUCYO-GEN70
                                END-IF
                            END-IF
                            IF  ZAN-SYU-M-ENTANI
                                              <     WK-TOKUTEI-COMPFTN2
                                MOVE  ZAN-SYU-M-ENTANI
                                          TO  LNK-SYU-SYUCOMPFTN-ENTANI
                            END-IF
                        ELSE
                                MOVE WK-TOKUTEI-COMPFTN2                
                                                 TO  WK-TOKUTEI-COMPFTN 
                        END-IF
                        COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
                                  LNK-SYU-SYUCOMPFTN     +
                                  LNK-SYU-SYUCOMPYKZFTN
               END-IF
             WHEN   "1"
      *        ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£´£°£°£±¡Ë¤«¤éÅÀ¿ôÃ±²Á¤Î¼èÆÀ
               INITIALIZE                         SYS-4001-REC
               MOVE    "4001"               TO    SYS-4001-KANRICD
               MOVE    "*"                  TO    SYS-4001-KBNCD
               MOVE    LNK-SYU-SRYYMD       TO    ORC-DBYMD
               MOVE    SYS-4001-REC         TO    MCPDATA-REC
               MOVE    "DBSELECT"           TO    MCP-FUNC
               MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH
               CALL    "ORCMCPSUB"    USING       MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
               IF      MCP-RC               =     ZERO
                   MOVE     "SYSKANRI-KEY"  TO    ORC-DBPATH
                   PERFORM   SYSKANRI-READ-SEC
                   IF    MCP-RC             =     ZERO
      *
                     IF    SYS-4001-KSNRATE1(1:3)   =  SPACE
                        IF    SYS-4001-KSNRATE(1:2)   =    SPACE
                            MOVE     100        TO   WK-4001-KSNRATE9
                            COMPUTE  WK-4001-KSNRATE   =
                                           WK-4001-KSNRATE9  /  100
                        ELSE
                            COMPUTE  WK-4001-KSNRATE9  =
                                           SYS-4001-KSNRATE  +  100
                            COMPUTE  WK-4001-KSNRATE   =
                                           WK-4001-KSNRATE9  /  100
                        END-IF
                     ELSE
                       MOVE SYS-4001-KSNRATE1   TO   WK-4001-KSNRATE9
                       COMPUTE  WK-4001-KSNRATE   =
                                           WK-4001-KSNRATE9  /  100
                     END-IF
                     MOVE   SYS-4001-TAXKBN     TO   WK-4001-TAXKBN
                     MOVE   SYS-4001-TAXKBN-KZI TO   WK-4001-TAXKBN-KZI
                     IF     SYS-4001-JIBAIKBN       =  SPACE
                        MOVE   "1"                 TO   WK-4001-JIBAIKBN
                     ELSE
                        MOVE   SYS-4001-JIBAIKBN   TO   WK-4001-JIBAIKBN
                     END-IF
                     IF     SYS-4001-SYUTENTANKA-X  =  SPACE
                        MOVE  ZERO   TO  WK-4001-SYUTENTANKA
                     ELSE
                        MOVE  SYS-4001-SYUTENTANKA
                                     TO  WK-4001-SYUTENTANKA
                     END-IF
                     IF     SYS-4001-YKZTENTANKA-X  =  SPACE
                        MOVE  ZERO   TO  WK-4001-YKZTENTANKA
                     ELSE
                        MOVE  SYS-4001-YKZTENTANKA
                                     TO  WK-4001-YKZTENTANKA
                     END-IF
                     IF     LNK-SYU-SYUHKNNUM       =  "971" OR "973"   
                       IF   LNK-SYU-PTFTNRATE       =   100             
                         MOVE     100      TO   HKN-HON-GAIKFTNRATE     
                                                HKN-KZK-GAIKFTNRATE     
                                                HKN-TSY-GAIKFTNRATE     
                                                HKN-TNK-GAIKFTNRATE     
                       ELSE                                             
                         MOVE     ZERO     TO   HKN-HON-GAIKFTNRATE     
                                                HKN-KZK-GAIKFTNRATE     
                                                HKN-TSY-GAIKFTNRATE     
                                                HKN-TNK-GAIKFTNRATE     
                       END-IF                                           
                     END-IF                                             
                     IF     LNK-SYU-SYUHKNNUM       =  "971"
                       EVALUATE  SYS-4001-TENKBN
                           WHEN  "1"
                               COMPUTE  WK-ZENGAKU  =  
                                       (WK-ZENGAKU  / 10 * 11.5)
                           WHEN  "2"
                               COMPUTE  WK-ZENGAKU  =  
                                       (WK-ZENGAKU  / 10 * 12.0)
                       END-EVALUATE
                     ELSE
                       IF  WK-4001-JIBAIKBN  =  "1"
                         EVALUATE  SYS-4001-TENKBN
                           WHEN  "1"
                              COMPUTE  WK-ZENGAKU  =  WK-ZENGAKU  /  10  
                              MOVE  LNK-S01-SYUTEN(IDY)
                                                TO  WK-ZENGAKU-SYU
                              COMPUTE  WK-ZENGAKU-YAK
                                                  = WK-ZENGAKU 
                                                  - LNK-S01-SYUTEN(IDY)
                              COMPUTE  WK-ZENGAKU-SYU =                 
                               (WK-ZENGAKU-SYU * 11.5 * WK-4001-KSNRATE)
                                                      + 0.5             
                              COMPUTE  WK-ZENGAKU-YAK =                 
                                (WK-ZENGAKU-YAK * 11.5)                 
                                                      + 0.5             
                              COMPUTE  WK-ZENGAKU     = WK-ZENGAKU-SYU
                                                      + WK-ZENGAKU-YAK
                           WHEN  "2"
                              COMPUTE  WK-ZENGAKU  =  WK-ZENGAKU  /  10  
                              MOVE  LNK-S01-SYUTEN(IDY)
                                                TO  WK-ZENGAKU-SYU
                              COMPUTE  WK-ZENGAKU-YAK
                                                  = WK-ZENGAKU 
                                                  - LNK-S01-SYUTEN(IDY)
                              COMPUTE  WK-ZENGAKU-SYU =                 
                               (WK-ZENGAKU-SYU * 12.0 * WK-4001-KSNRATE)
                                                      + 0.5             
                              COMPUTE  WK-ZENGAKU-YAK =                 
                                (WK-ZENGAKU-YAK * 12.0)                 
                                                      + 0.5             
                              COMPUTE  WK-ZENGAKU     = WK-ZENGAKU-SYU
                                                      + WK-ZENGAKU-YAK
                         END-EVALUATE
                       ELSE
                         COMPUTE  WK-ZENGAKU  =  WK-ZENGAKU  /  10
                         MOVE  LNK-S01-SYUTEN(IDY)
                                           TO  WK-ZENGAKU-SYU
                         COMPUTE  WK-ZENGAKU-YAK
                                             = WK-ZENGAKU
                                             - LNK-S01-SYUTEN(IDY)
                         COMPUTE  WK-ZENGAKU-SYU =
                                 (WK-ZENGAKU-SYU * WK-4001-SYUTENTANKA)
                                                 + 0.5
                         COMPUTE  WK-ZENGAKU-YAK =
                                 (WK-ZENGAKU-YAK * WK-4001-YKZTENTANKA)
                                                 + 0.5
                         COMPUTE  WK-ZENGAKU     = WK-ZENGAKU-SYU
                                                 + WK-ZENGAKU-YAK
                       END-IF
                     END-IF 
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
      *
               MOVE      WK-ZENGAKU    TO    LNK-SYU-RSIJIBAI-SKYMONEY
      *
               COMPUTE   WK-FUTANWARI  =
                         HKN-HON-GAIKFTNRATE  /  100
               COMPUTE   LNK-SYU-SYUCOMPFTN  =
                         WK-ZENGAKU   *  WK-FUTANWARI
      *         COMPUTE   LNK-SYU-SYUCOMPFTN =
      *                   LNK-SYU-SYUCOMPFTN  +  5
      *         COMPUTE   WK-FTN      =
      *                   LNK-SYU-SYUCOMPFTN  /  10
      *         COMPUTE   LNK-SYU-SYUCOMPFTN =
      *                   WK-FTN   *  10
               MOVE  HKN-HON-GAIYKZFTNKBN
                                   TO   LNK-SYU-SYUYKZFTNKBN
               IF    HKN-HON-GAIYKZFTNKBN  NOT =  "0"
                     MOVE  YAKFTN-SOGOKEI
                                   TO   LNK-SYU-SYUCOMPYKZFTN
                  IF    HKN-HON-GAIYKZFTNKBN  =  "1"
                     MOVE  YAKFTN-SOGOKEI
                                   TO   WK-SYU-YAKFTN
                  END-IF
               END-IF
               COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
                         LNK-SYU-SYUCOMPFTN     +
                         LNK-SYU-SYUCOMPYKZFTN
           END-EVALUATE.
      *
       2303-SYUHOKEN-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñ¤ÎÊÝ¸±ÈÖ¹æ¥Æ¡¼¥Ö¥ë¸¡º÷                              *
      ******************************************************************
       2304-KOH-HKNREAD-SEC           SECTION.
      *
      *    ³ºÅöÊÝ¸±ÈÖ¹æ¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
           INITIALIZE                      HKNNUM-REC.
           MOVE   COMB-HOSPID        TO    HKN-HOSPID.
           MOVE   COMB-KOHHKNNUM(KOH-IDX)
                                     TO    HKN-HKNNUM.
      *
           IF     COMB-KOHHKNNUM(KOH-IDX)  =     "027"
               IF   LNK-SYU-SRYYMD    >=   "20021001"
                  MOVE     COMB-KOH1PAYKBN     TO  WK-PAYKBN
               ELSE
                  MOVE     WK-1001-HOSPSBT     TO  WK-PAYKBN(1:1)
                  MOVE     WK-1001-ROUPAYKBN   TO  WK-PAYKBN(2:1)
               END-IF
           ELSE
                   MOVE     "00"         TO    WK-PAYKBN
           END-IF.
      *
           MOVE   HKNNUM-REC         TO    MCPDATA-REC.
           MOVE   "DBSELECT"         TO    MCP-FUNC.
           MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH.
           CALL   "ORCMCPSUB"         USING MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC.
           IF      MCP-RC            =     ZERO
               MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
               PERFORM  HKNNUM-READ-SEC
               PERFORM  UNTIL
                 (FLG-HKNNUM     =    1)                       OR
                ((HKN-TEKSTYMD(1:6)   <=  LNK-SYU-SRYYMD(1:6)) AND
                 (LNK-SYU-SRYYMD(1:6) <=  HKN-TEKEDYMD(1:6))   AND
                 (WK-PAYKBN            =  HKN-PAYKBN))
                   PERFORM   HKNNUM-READ-SEC
               END-PERFORM
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
      *    ´µ¼ÔÉéÃ´¶â»»½ÐÁ°¤Î£±£°£°¡óÉéÃ´³Û»»½Ð
           IF    (COMB-KOHHKNNUM(KOH-IDX)(1:1)   =   "0"           )
           OR    (COMB-KOHHKNNUM(KOH-IDX)        =   "972" OR "974")
                 MOVE    10    TO      HKN-TENTNK
           ELSE
             IF   HKN-TENTNK   =   ZERO
                 MOVE    10    TO      HKN-TENTNK
             END-IF
           END-IF.
           MOVE      ZERO      TO      WK-ZENGAKU.
           PERFORM   VARYING   IDX9    FROM    1   BY  1
                     UNTIL     IDX9       >   15
               COMPUTE WK-ZENGAKU  =   WK-ZENGAKU  +
                            (LNK-SYU-HKNTEN (IDX9) *  HKN-TENTNK)
           END-PERFORM.
      *
           MOVE       ZERO                TO   WK-GAIKFTNRATE.
           MOVE       SPACE               TO   WK-GAIYKZFTNKBN.
           MOVE       ZERO                TO   WK-GAIKKOTGAK.
      *
           IF    COMB-KOHHKNNUM(KOH-IDX)(1:1)   >=  "1" AND <=  "8"
             IF  WK-1001-CHIHOUNUMKBN       =   "1"
               EVALUATE   FLG-SANSHOU2
                 WHEN   "1"
                      MOVE    HKN-HON-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                      MOVE    HKN-HON-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                      MOVE    HKN-HON-GAIKKOTGAK   TO   WK-GAIKKOTGAK
                 WHEN   "2"
                    IF   HKN-TSY-GAIFTNKBN      =  "0"
                      MOVE    HKN-HON-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                      MOVE    HKN-HON-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                      MOVE    HKN-HON-GAIKKOTGAK   TO   WK-GAIKKOTGAK
                    ELSE
                      MOVE    HKN-TSY-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                      MOVE    HKN-TSY-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                      MOVE    HKN-TSY-GAIKKOTGAK   TO   WK-GAIKKOTGAK
                    END-IF
                 WHEN   "3"
                    IF  HKN-TNK-GAIFTNKBN       =  "0"
                      MOVE    HKN-HON-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                      MOVE    HKN-HON-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                      MOVE    HKN-HON-GAIKKOTGAK   TO   WK-GAIKKOTGAK
                    ELSE
                      MOVE    HKN-TNK-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                      MOVE    HKN-TNK-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                      MOVE    HKN-TNK-GAIKKOTGAK   TO   WK-GAIKKOTGAK
                    END-IF
               END-EVALUATE
             ELSE
               IF (( WK-1001-PREFNUM   =  28                    ) AND
                   ( COMB-KOHHKNNUM(KOH-IDX)  =
                                  "241" OR "180" OR "182" OR
                                  "143" OR "185" OR "127"       ) AND
                   ( LNK-SYU-SRYYMD(1:6)     >= "200507"        ))
               OR (( WK-1001-PREFNUM   =  11                    ) AND
                   ( COMB-KOHHKNNUM(KOH-IDX)  = "344"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)     >= "200507"        ))
               OR (( WK-1001-PREFNUM   =  05                    ) AND
                   ( COMB-KOHHKNNUM(KOH-IDX)  = "374" OR "474"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)     >= "200508"        ))
               OR (( WK-1001-PREFNUM   =  32                    ) AND
                   ( COMB-KOHHKNNUM(KOH-IDX)  = "191" OR "391"  ) AND
                   ( LNK-SYU-SRYYMD(1:6)     >= "200510"        ))
               OR (( WK-1001-PREFNUM   =  28                    ) AND
                   ( COMB-KOHHKNNUM(KOH-IDX)  = "100"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)     >= "200604"        ))
               OR (( WK-1001-PREFNUM   =  33                    ) AND
                   ( COMB-KOHHKNNUM(KOH-IDX)  =
                                  "141" OR "285" OR "485" OR
                                  "180" OR "186"                ) AND
                   ( LNK-SYU-SRYYMD(1:6)     >= "200610"        ))
               OR (( WK-1001-PREFNUM   =  19                    ) AND
                   ( COMB-KOHHKNNUM(KOH-IDX)  = "141"           ))
               OR (( WK-1001-PREFNUM   =  23                    ) AND
                   ( COMB-KOHHKNNUM(KOH-IDX)  = "141"           ) AND
                   ( LNK-SYU-SRYYMD(1:6)     >= "200610"        ))
                 EVALUATE   FLG-SANSHOU2
                   WHEN   "1"
                      MOVE    HKN-HON-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                      MOVE    HKN-HON-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                      MOVE    HKN-HON-GAIKKOTGAK   TO   WK-GAIKKOTGAK
                   WHEN   "2"
                    IF   HKN-TSY-GAIFTNKBN      =  "0"
                      MOVE    HKN-HON-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                      MOVE    HKN-HON-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                      MOVE    HKN-HON-GAIKKOTGAK   TO   WK-GAIKKOTGAK
                    ELSE
                      MOVE    HKN-TSY-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                      MOVE    HKN-TSY-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                      MOVE    HKN-TSY-GAIKKOTGAK   TO   WK-GAIKKOTGAK
                    END-IF
                   WHEN   "3"
                    IF  HKN-TNK-GAIFTNKBN       =  "0"
                      MOVE    HKN-HON-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                      MOVE    HKN-HON-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                      MOVE    HKN-HON-GAIKKOTGAK   TO   WK-GAIKKOTGAK
                    ELSE
                      MOVE    HKN-TNK-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                      MOVE    HKN-TNK-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                      MOVE    HKN-TNK-GAIKKOTGAK   TO   WK-GAIKKOTGAK
                    END-IF
                 END-EVALUATE
               ELSE
                 MOVE    HKN-HON-GAIKFTNRATE  TO   WK-GAIKFTNRATE
                 MOVE    HKN-HON-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
                 MOVE    HKN-HON-GAIKKOTGAK   TO   WK-GAIKKOTGAK
               END-IF
             END-IF
           ELSE
             MOVE    HKN-HON-GAIKFTNRATE  TO   WK-GAIKFTNRATE
             MOVE    HKN-HON-GAIYKZFTNKBN TO   WK-GAIYKZFTNKBN
             MOVE    HKN-HON-GAIKKOTGAK   TO   WK-GAIKKOTGAK
           END-IF.
      *
           IF   COMB-KOHHKNNUM(KOH-IDX)  =  "027"
             MOVE    HKN-HON-GAIKFTNRATE  TO   WK-KOH-FTNRATE
             MOVE    "1"                  TO   HKN-RECEFTNDSPKBN
           END-IF.
      *
           IF   COMB-KOHHKNNUM(KOH-IDX)  =  "010" OR "021"
              IF    (COMB-KOHHKNNUM(KOH-IDX)  =  "021"   )
              AND   (LNK-SYU-SRYYMD(1:6)     >=  "200604")
                 CONTINUE
              ELSE
                 IF   FLG-SEISINJYO  =  1
                   MOVE     ZERO     TO    WK-GAIKFTNRATE
                 END-IF
              END-IF
           END-IF.
      *
      *    Ê¡²¬¸©¤ÎÃÏÊý¸øÈñÂÐ±þ¡Ê¸øÈñÉÕ²Ã¾ðÊó¤Î¼èÆÀ¡Ë
           MOVE     SPACE     TO     WK-KOH-FTNKBN.
           IF     (WK-1001-PREFNUM           =       40)    AND
                  (COMB-KOHHKNNUM(KOH-IDX)(1:1)  >= "1")    AND
                  (COMB-KOHHKNNUM(KOH-IDX)(1:1)  <= "8")
               PERFORM     23041-KOHPLUS-FUKUOKA-SEC
           END-IF.
      *
           IF   FLG-CHIHOU(KOH-IDX)  =  "1"
               IF    WK-GAIKFTNRATE  =  ZERO
                    MOVE    SPACE   TO  COMB-KOH-GAIJGNKBN(KOH-IDX)
               END-IF
           END-IF.
      *
           IF    ((WK-1001-PREFNUM         =  34           )  AND
                  (COMB-KOHHKNNUM(KOH-IDX) = "390"         )  AND
                  (LNK-SYU-SRYYMD(1:6)    >= "200410"      ))
           OR    ((WK-1001-PREFNUM         =  01           ) AND
                  (COMB-KOHHKNNUM(KOH-IDX) = "142" OR "143"
                                                   OR "144") AND
                  (LNK-SYU-SRYYMD(1:6)    >= "200410"      ))
           OR    ((WK-1001-PREFNUM         =  01           ) AND
                  (COMB-KOHHKNNUM(KOH-IDX) = "190" OR "291"
                                                   OR "292") AND
                  (LNK-SYU-SRYYMD(1:6)    >= "200708"      ))
               IF    LNK-S01-SYOSINFLG  =  SPACE
                    MOVE    SPACE   TO   COMB-KOH-GAIJGNKBN(KOH-IDX)
                    MOVE    ZERO    TO   WK-GAIKFTNRATE
                    MOVE    "0"     TO   WK-GAIYKZFTNKBN
                    MOVE    ZERO    TO   WK-GAIKKOTGAK
               END-IF
           END-IF.
      *
           IF     (WK-1001-PREFNUM         =  06           ) AND
                  (COMB-KOHHKNNUM(KOH-IDX) = "182" OR "183")
               IF    LNK-S01-SINSATUFLG =  SPACE
                    MOVE    SPACE   TO   COMB-KOH-GAIJGNKBN(KOH-IDX)
                    MOVE    ZERO    TO   WK-GAIKFTNRATE
                    MOVE    "0"     TO   WK-GAIYKZFTNKBN
                    MOVE    ZERO    TO   WK-GAIKKOTGAK
               END-IF
           END-IF.
      *
           IF     (WK-1001-PREFNUM         =  01           ) AND
                  (COMB-KOHHKNNUM(KOH-IDX) = "121"         ) AND
                  (LNK-SYU-SRYYMD(1:6)    >= "200604"      )
                    MOVE    SPACE   TO   COMB-KOH-GAIJGNKBN(KOH-IDX)
                    MOVE    ZERO    TO   WK-GAIKFTNRATE
                    MOVE    "0"     TO   WK-GAIYKZFTNKBN
                    MOVE    ZERO    TO   WK-GAIKKOTGAK
           END-IF.
      *
           MOVE      ZERO      TO        FLG-MIYAGI.
           IF     (WK-1001-PREFNUM         =  04           ) AND
                  (COMB-KOHHKNNUM(KOH-IDX) = "183" OR "383" 
                                                   OR "483") AND
                  (LNK-SYU-SRYYMD(1:6)    >= "200510"      )
      *
                INITIALIZE                       PTKOHINF-REC
                MOVE    LNK-SYU-HOSPID       TO  PTKOH-HOSPID
                MOVE    LNK-SYU-PTID         TO  PTKOH-PTID
                MOVE    COMB-KOHHKNNUM(KOH-IDX)
                                             TO  PTKOH-KOHNUM
                MOVE    LNK-SYU-SRYYMD       TO  PTKOH-TEKSTYMD
                MOVE    LNK-SYU-SRYYMD       TO  PTKOH-TEKEDYMD
                MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                MOVE    "DBSELECT"           TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY6"      TO  ORC-DBPATH
                CALL    "ORCMCPSUB"          USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
                IF      MCP-RC               =       ZERO
                    MOVE    "PTKOHINF-KEY6"  TO  ORC-DBPATH
                    PERFORM  PTKOHINF-READ-SEC
                    PERFORM  UNTIL   FLG-PTKOHINF    =   1
                        IF   PTKOH-KOHID  =  COMB-KOHID(KOH-IDX)
                           EVALUATE  PTKOH-FTNJANUM(1:7)
                             WHEN    "8304102"
                               MOVE    1    TO  FLG-MIYAGI
                             WHEN    "8304125"
                              IF  LNK-SYU-SRYYMD(1:6) >= "200610"
                               CONTINUE
                              ELSE
                               MOVE    2    TO  FLG-MIYAGI
                              END-IF
                           END-EVALUATE
                           MOVE    1        TO  FLG-PTKOHINF
                        ELSE
                           MOVE    "PTKOHINF-KEY6"  TO  ORC-DBPATH
                           PERFORM  PTKOHINF-READ-SEC
                        END-IF
                    END-PERFORM
                END-IF
                MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
                MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
                CALL    "ORCMCPSUB"         USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
           END-IF.
      *
           IF  (COMB-KOHHKNNUM(KOH-IDX)(1:2)  = "99"  ) AND
               (WK-GAIKFTNRATE                =  ZERO )
                    MOVE    WK-KOH-FTNRATE   TO   WK-GAIKFTNRATE
           END-IF.
      *
           IF  (LNK-SYU-SRYYMD(1:6)     >= "200604"               )
           AND (COMB-KOHHKNNUM(KOH-IDX)  = "015" OR "016" OR "021")
           AND (WK-JIRITU-CYUKANSOU(KOH-IDX)   =  1               )
                   MOVE    1       TO  LNK-SYU-NYNSHOKAISU
           END-IF.
      *
           IF  ( FLG-GENBUTU70               NOT =     ZERO )
           AND ( COMB-KOHHKNNUM(KOH-IDX)(1:1)   >=     "1"  )
           AND ( COMB-KOHHKNNUM(KOH-IDX)(1:1)   <=     "8"  )
           AND ( COMB-KOH-GAIJGNKBN(KOH-IDX)     =   SPACE  )
           AND ( WK-GAIKFTNRATE                  =     100
                                                OR      30  )
                 MOVE    "1"    TO    FLG-SYOUKAN(KOH-IDX)
           END-IF.
      *
           IF      FLG-GENBUTU70           NOT =  ZERO
             IF (( WK-1001-PREFNUM             =   13            ) AND
                 ( COMB-KOHHKNNUM(KOH-IDX)     =  "182"          ))
             OR (( WK-1001-PREFNUM             =   07            ) AND
                 ( COMB-KOHHKNNUM(KOH-IDX)     =  "245" OR "246" ))
             OR (( WK-1001-PREFNUM             =   11            ) AND
                 ( COMB-KOHHKNNUM(KOH-IDX)     =  "342" OR "343"
                                               OR "181" OR "182"
                                               OR "183" OR "281" ))
             OR (( WK-1001-PREFNUM             =   11            ) AND
                 ( COMB-KOHHKNNUM(KOH-IDX)     =  "344"          ) AND
                 ( FLG-SANSHOU2                =  "2" OR "3"     ))
                 MOVE    "1"    TO    FLG-SYOUKAN(KOH-IDX)
             END-IF
           END-IF.
      *
       2304-KOH-HKNREAD-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñÉÕ²Ã¾ðÊó¤Î¼èÆÀ¡ÊÊ¡²¬¸©ÂÐ±þ¡Ë                  *
      ******************************************************************
       23041-KOHPLUS-FUKUOKA-SEC           SECTION.
      *
           INITIALIZE                  ORCSKOHPLUS-AREA
           MOVE    LNK-SYU-HOSPID  TO  LNK-SKOHPLUS-HOSPID
           MOVE    WK-1001-PREFNUM TO  LNK-SKOHPLUS-PREFNUM
           MOVE    LNK-SYU-PTID    TO  LNK-SKOHPLUS-PTID
           MOVE    LNK-SYU-HKNCOMBINUM
                                   TO  LNK-SKOHPLUS-HKNCOMBI
           MOVE    LNK-SYU-SRYYMD  TO  LNK-SKOHPLUS-SRYYMD
           CALL    "ORCSKOHPLUS"       USING
                                       ORCSKOHPLUS-AREA
           IF      LNK-SKOHPLUS-RC =   ZERO
               MOVE   LNK-SKOHPLUS-FTNKBN    TO   WK-KOH-FTNKBN
               MOVE   WK-KOH-FTNRATE         TO   WK-GAIKFTNRATE
               IF     WK-GAIKFTNRATE  =  ZERO
                      MOVE    SPACE   TO  COMB-KOH-GAIJGNKBN(KOH-IDX)
               END-IF
           END-IF
      *
           .
      *
       23041-KOHPLUS-FUKUOKA-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñ¤Î´µ¼ÔÉéÃ´¶â³Û·×»»¡Ê¾å¸Â³Û¤¢¤ê¡Ë                    *
      ******************************************************************
       2305-KOH-KEISAN-SEC           SECTION.
      *
      *    ÉéÃ´¶â³Û¤Î·×»»
           IF  WK-GAIKFTNRATE     NOT =    ZERO
      *
               COMPUTE   WK-FUTANWARI  =
                              WK-GAIKFTNRATE  /  100
               IF    WK-KOH-FTNKBN   NOT =  SPACE
                 EVALUATE  WK-KOH-FTNKBN
                    WHEN  "1"
                      COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                                LNK-SYU-KYUFUGAI-GOKEI-TEN
                                        *  HKN-TENTNK
                                        *  WK-FUTANWARI
                      COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                                LNK-SYU-KYUFUGAI-GOKEI-TEN
                                        *  HKN-TENTNK
                                        *  WK-FUTANWARI
                    WHEN  "2"
                      MOVE    ZERO   TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                      MOVE    ZERO   TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                      MOVE    ZERO   TO   LNK-SYU-KYUFUGAI-SHOSHIN-TEN
                                          LNK-SYU-KYUFUGAI-SIDOU-TEN
                                          LNK-SYU-KYUFUGAI-OSIN1-TEN
                                          LNK-SYU-KYUFUGAI-OSIN2-TEN
                                          LNK-SYU-KYUFUGAI-OSIN3-TEN
                                          LNK-SYU-KYUFUGAI-GOKEI-TEN
                 END-EVALUATE
               ELSE
                 COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                           WK-ZENGAKU   *  WK-FUTANWARI
                 COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                           WK-ZENGAKU   *  WK-FUTANWARI
                 IF  (WK-1001-PREFNUM             =  01           )
                 AND (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "642" OR "643"
                                                 OR "644" OR "743")
                 AND (LNK-S01-SYOSINFLG       NOT =  SPACE        )
                     IF   LNK-SYU-KOH-COMPFTN(KOH-IDX)         <   580
                        MOVE  ZERO  TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     ELSE
                        COMPUTE  LNK-SYU-KOH-COMPFTN(KOH-IDX) =
                                 LNK-SYU-KOH-COMPFTN(KOH-IDX) -
                                 580
                     END-IF
                     IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  <   580
                        MOVE  ZERO  TO
                                 LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     ELSE
                        COMPUTE  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX) =
                                 LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX) -
                                 580
                     END-IF
                 END-IF
               END-IF
               IF   LNK-SYU-KOH-HKNNUM(KOH-IDX)  =      "027"           
      *             ¸º³Û
                    EVALUATE  WK-GENGAKUKBN
                      WHEN        1
                        IF  WK-GENGAKU-RITU = 100
                         MOVE  ZERO  TO  WK-GENGAKU-GAK
                         MOVE  ZERO  TO  WK-GENGAKU-GAK-ENTANI
                        ELSE
                         COMPUTE  WK-GENGAKU-RITUX =
                                  100 - WK-GENGAKU-RITU
                         COMPUTE  WK-GENGAKU-GAK               =
                                  LNK-SYU-KOH-COMPFTN(KOH-IDX) *
                                  WK-GENGAKU-RITUX             /
                                  100
                         COMPUTE  WK-GENGAKU-GAK-ENTANI               =
                                  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX) *
                                  WK-GENGAKU-RITUX                    /
                                  100
                        END-IF
                      WHEN        2
                        CONTINUE
                      WHEN        3
                      WHEN        4
                        MOVE  ZERO  TO  WK-GENGAKU-GAK
                        MOVE  ZERO  TO  WK-GENGAKU-GAK-ENTANI
                    END-EVALUATE
                    IF  WK-GENGAKUKBN  NOT =  ZERO
                        MOVE  WK-GENGAKU-GAK
                               TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                        MOVE  WK-GENGAKU-GAK-ENTANI
                               TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                    END-IF
                    MOVE   LNK-SYU-KOH-COMPFTN(KOH-IDX)                 
                                            TO WK-TOKUTEI-COMPFTN3      
               END-IF                                                   
               IF ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"         ) OR
                  ( HKN-RECEFTNDSPKBN            =  "2"           ) OR
                  ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"      ) AND
                    (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "015" OR "016") )
                   MOVE    "1"   TO    WK-TAIHI-RECEFTNDSPKBN(KOH-IDX)
                   IF    WK-TOKUTEI-COMPFTN   NOT =    ZERO
                       MOVE  WK-TOKUTEI-COMPFTN
                                        TO  LNK-SYU-SYUCOMPFTN
                   END-IF
                   IF    LNK-SYU-KOH-HKNNUM(1)    =    "027"
                     IF  WK-TOKUTEI-COMPFTN3  NOT =    ZERO
                       MOVE  WK-TOKUTEI-COMPFTN3
                                        TO  LNK-SYU-KOH-COMPFTN(1)
                     END-IF
                   END-IF
               ELSE
                   COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                             LNK-SYU-KOH-COMPFTN(KOH-IDX)  +  5
                   COMPUTE   WK-FTN      =
                             LNK-SYU-KOH-COMPFTN(KOH-IDX)  / 10
                   COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                             WK-FTN   *  10
               END-IF
      *
               IF  ( FLG-SYUHEN                   =   "1" )
               AND ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "027")
                  IF    LNK-SYU-KOH-COMPFTN(KOH-IDX)   >
                                            ZAN-KOHI1-HEN
                     MOVE   ZAN-KOHI1-HEN   TO   WK-HEN-KOH1COMPFTN 
                  ELSE
                     MOVE   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                                            TO   WK-HEN-KOH1COMPFTN 
                  END-IF
                  IF    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)   >
                                            ZAN-KOHI1-HEN-ENTANI
                     MOVE   ZAN-KOHI1-HEN-ENTANI
                                        TO   WK-HEN-KOH1COMPFTN-ENTANI
                  ELSE
                     MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                        TO   WK-HEN-KOH1COMPFTN-ENTANI
                  END-IF
               END-IF
               IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =   "1" )  OR         
                    (FLG-SYUHEN                   =   "1" )  OR
                    (FLG-KOHININBARAI             =    1  ))            
               AND ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "027" )            
                 IF    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                     >      ZAN-KOHI1-ENTANI            
                     MOVE   ZAN-KOHI1-ENTANI       TO                   
                                    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX) 
                 END-IF                                                 
               ELSE                                                     
                 IF    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                     >      ZAN-KOHI-T3(KOH-IDX)        
                     MOVE   ZAN-KOHI-T3(KOH-IDX)   TO                   
                                    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX) 
                 END-IF                                                 
               END-IF                                                   
               PERFORM    23053-KOHHI-HIKAKU3-SEC                             
               IF ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"         ) OR
                  ( HKN-RECEFTNDSPKBN            =  "2"           ) OR
                  ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"      ) AND
                    (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "015" OR "016") )
                 IF    LNK-SYU-KOH-COMPFTN(KOH-IDX)   >                 
                                            ZAN-KOHI-T2(KOH-IDX)        
                     MOVE   ZAN-KOHI-T2(KOH-IDX)   TO                   
                                          WK-KOH-COMPFTN(KOH-IDX)       
                     PERFORM    23052-KOHHI-HIKAKU2-SEC                       
                 ELSE
                     MOVE   LNK-SYU-KOH-COMPFTN(KOH-IDX)   TO
                                          WK-KOH-COMPFTN(KOH-IDX)       
                     PERFORM    23052-KOHHI-HIKAKU2-SEC                       
                 END-IF                                                 
               END-IF                                                   
      *
               IF    LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1)  =  "0"
                 MOVE   ZERO   TO   WK-MIYAGI-COMPFTN
                 IF  LNK-SYU-KOH-HKNNUM(KOH-IDX)  = "021" OR
                                                    "015" OR "016" OR
                                                    "024" OR "079"
                    MOVE  WK-KOH-COMPFTN(KOH-IDX) TO  WK-MIYAGI-COMPFTN
                 END-IF
               END-IF
               IF    LNK-SYU-KOH-HKNNUM(KOH-IDX)       =  "021"
                    MOVE  WK-KOH-COMPFTN(KOH-IDX) TO  WK-SEISIN-COMPFTN
               END-IF
      *
               IF    LNK-SYU-KOH-COMPFTN(KOH-IDX)   >
                                            ZAN-KOHI-T(KOH-IDX)
                     MOVE   ZAN-KOHI-T(KOH-IDX)   TO
                                          LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     MOVE   ZERO        TO  ZAN-KOHI-T(KOH-IDX)
                     IF   LNK-SYU-KOH-HKNNUM(KOH-IDX)  =      "027"     
                          MOVE   LNK-SYU-KOH-COMPFTN(KOH-IDX)           
                                            TO WK-TOKUTEI-COMPFTN3      
                     END-IF                                             
               ELSE
                     COMPUTE  ZAN-KOHI-T(KOH-IDX)    =
                              ZAN-KOHI-T(KOH-IDX)    -
                              LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     IF   LNK-SYU-KOH-HKNNUM(KOH-IDX)  =      "027"
                        IF    FLG-KOUGAKUCYO-ININ  =    1
                              MOVE   ZERO  TO  FLG-KOUGAKUCYO-ININ
                        END-IF
                     END-IF 
               END-IF
               PERFORM    23051-KOHHI-HIKAKU-SEC                              
               MOVE  WK-GAIYKZFTNKBN
                                TO   LNK-SYU-KOH-YKZFTNKBN(KOH-IDX)
               IF    ZAN-KOHI-T(KOH-IDX)   NOT =   ZERO                 
                   COMPUTE   ZAN-KOHI-T(KOH-IDX) =                      
                             ZAN-KOHI-T(KOH-IDX)  +  5                  
                   COMPUTE   WK-FTN      =                              
                             ZAN-KOHI-T(KOH-IDX)  /  10                 
                   COMPUTE   ZAN-KOHI-T(KOH-IDX) =                      
                             WK-FTN   *  10                             
               END-IF                                                   
               EVALUATE   WK-GAIYKZFTNKBN
                   WHEN   "0"
                       MOVE  ZERO  TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                   WHEN   "1"
                       MOVE  YAKFTN-SOGOKEI   TO
                                LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                                WK-KOH-YAKFTN-T(KOH-IDX)
                   WHEN   "2"
                       IF    YAKFTN-SOGOKEI  >  ZAN-KOHI-T(KOH-IDX)
                             MOVE   ZAN-KOHI-T(KOH-IDX)  TO  
                                       LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                                       WK-KOH-YAKFTN-T(KOH-IDX)
                             MOVE   ZERO    TO
                                            ZAN-KOHI-T(KOH-IDX)
                       ELSE
                             MOVE  YAKFTN-SOGOKEI  TO
                                       LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                                       WK-KOH-YAKFTN-T(KOH-IDX)
                             COMPUTE  ZAN-KOHI-T(KOH-IDX)   =
                                      ZAN-KOHI-T(KOH-IDX)   -
                                      LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                       END-IF
                   WHEN   "3"
                       MOVE  YAKFTN-SOGOKEI TO  LNK-SYU-KOH-COMPYKZFTN
                                                            (KOH-IDX)
               END-EVALUATE
               COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                         LNK-SYU-KOH-COMPFTN(KOH-IDX)     +
                         LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
           ELSE
               IF ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"         ) OR
                  ( HKN-RECEFTNDSPKBN            =  "2"           ) OR
                  ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"      ) AND
                    (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "015" OR "016") )
                   MOVE    "1"   TO    WK-TAIHI-RECEFTNDSPKBN(KOH-IDX)
                   IF    WK-TOKUTEI-COMPFTN   NOT =    ZERO
                       MOVE  WK-TOKUTEI-COMPFTN  TO  LNK-SYU-SYUCOMPFTN
                   END-IF
               END-IF
               IF  (( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"       ) OR
                    ( HKN-RECEFTNDSPKBN            =  "2"         ) OR
                    ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"    ) AND
                      (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  
                                                    "015" OR "016") ))
               AND (  LNK-SYU-KOH-HKNNUM(1)        =  "027"          )
                   IF    WK-TOKUTEI-COMPFTN3   NOT =    ZERO            
                       MOVE   WK-TOKUTEI-COMPFTN3                       
                                            TO  LNK-SYU-KOH-COMPFTN(1)  
                   END-IF                                               
               END-IF                                                   
      *
      *        ÆÃ¼ì·×»»¡Ê¸ÂÅÙ¤òÄ¶¤¨¤¿¾ì¹ç¡¢´µ¼ÔÉéÃ´È¯À¸¡ÊÁ´³Û¡Ë¡Ë
      *********¡ÊÊ¡Åç¸©¡Ë
      *********¡Êºë¶Ì¸©¡Ë
               IF ((WK-1001-PREFNUM             =  07      ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "245"    ) AND
                   (LNK-SYU-SRYYMD(1:6)        >= "200412" ))
               OR ((WK-1001-PREFNUM             =  11      ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "343" OR
                                                  "342"    ) AND
                   (LNK-SYU-SRYYMD(1:6)        >= "200507" ))
               OR ((WK-1001-PREFNUM             =  11      ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "344"    ) AND
                   (FLG-SANSHOU2           =   "2" OR "3"  ) AND
                   (LNK-SYU-SRYYMD(1:6)        >= "200507" ))
               OR ((WK-1001-PREFNUM             =  07      ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "246"    ) AND
                   (LNK-SYU-SRYYMD(1:6)        >= "200604" ))
               OR ((WK-1001-PREFNUM             =  11      ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "181" OR
                                                  "182" OR
                                                  "183" OR
                                                  "281"    ) AND
                   (LNK-SYU-SRYYMD(1:6)        >= "200704" ))
                  MOVE     "1"     TO    LNK-S01-TOKUSYU-SAI-KBN
                  PERFORM   23054-TOKUSYU-SANTEI-SEC
                  GO    TO  2305-KOH-KEISAN-EXT
               END-IF
      *        ÆÃ¼ì·×»»¡Ê¸ÂÅÙ¤òÄ¶¤¨¤¿¾ì¹ç¡¢´µ¼ÔÉéÃ´È¯À¸¡ÊÄ¶¤¨¤¿³Û¡Ë¡Ë
      *********¡Ê²¬»³¸©¡Ë
      *********¡ÊÅìµþÅÔ¡Ë
               IF ((WK-1001-PREFNUM             =  33      ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "285"    ) AND
                   (LNK-SYU-SRYYMD(1:6)        >= "200110" ) AND
                   (LNK-SYU-SRYYMD(1:6)        <= "200609" ))
               OR ((WK-1001-PREFNUM             =  13      ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "182"    ) AND
                   (LNK-SYU-SRYYMD(1:6)        >= "200610" ))
                  PERFORM   23055-TOKUSYU-SANTEI2-SEC
                  GO    TO  2305-KOH-KEISAN-EXT
               END-IF
      *        ÆÃ¼ì·×»»¡Ê£²³äËô¤Ï£³³ä¤Î³Û¤ò»»½Ð¸å¡¢È¾³Û¤ò´µ¼ÔÉéÃ´¤È¤¹¤ë¡Ë
      *********¡Ê½©ÅÄ¸©¡Ë
               IF  (WK-1001-PREFNUM             =  05             ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "374" OR "474"  ) AND
                   (LNK-SYU-SRYYMD(1:6)        >= "200508" )
                  PERFORM   23056-TOKUSYU-SANTEI3-SEC
                  GO    TO  2305-KOH-KEISAN-EXT
               END-IF
      **
               IF    LNK-SYU-SYUCOMPFTN-ENTANI   NOT =    ZERO
                       IF    LNK-SYU-SYUCOMPFTN-ENTANI
                                                 >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE   ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
               ELSE
                 IF    LNK-SYU-KOH-COMPFTN-ENTANI(1)   NOT =    ZERO
                       IF    LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                                 >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE   ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                 ELSE
                       IF    WK-ZENGAKU   >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE   ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE   WK-ZENGAKU
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                 END-IF
               END-IF
               PERFORM    23053-KOHHI-HIKAKU3-SEC
      **
               IF ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"         ) OR
                  ( HKN-RECEFTNDSPKBN            =  "2"           ) OR
                  ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"      ) AND
                    (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "015" OR "016") )
                IF    LNK-SYU-SYUCOMPFTN   NOT =    ZERO
                       IF    LNK-SYU-SYUCOMPFTN  >  ZAN-KOHI-T2(KOH-IDX)
                           MOVE   ZAN-KOHI-T2(KOH-IDX)
                                            TO  WK-KOH-COMPFTN(KOH-IDX)
                           PERFORM    23052-KOHHI-HIKAKU2-SEC
                       ELSE
                           MOVE  LNK-SYU-SYUCOMPFTN                         
                                            TO  WK-KOH-COMPFTN(KOH-IDX)
                           PERFORM    23052-KOHHI-HIKAKU2-SEC
                       END-IF 
                ELSE
                 IF    LNK-SYU-KOH-COMPFTN(1)   NOT =    ZERO
                       IF LNK-SYU-KOH-COMPFTN(1) >  ZAN-KOHI-T2(KOH-IDX)
                          MOVE   ZAN-KOHI-T2(KOH-IDX)
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23052-KOHHI-HIKAKU2-SEC
                       ELSE
                          MOVE   LNK-SYU-KOH-COMPFTN(1)  
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23052-KOHHI-HIKAKU2-SEC
                       END-IF
                 ELSE
                       IF WK-ZENGAKU    >    ZAN-KOHI-T2(KOH-IDX)
                          MOVE   ZAN-KOHI-T2(KOH-IDX)
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23052-KOHHI-HIKAKU2-SEC
                       ELSE
                          MOVE   WK-ZENGAKU   
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23052-KOHHI-HIKAKU2-SEC
                       END-IF
                 END-IF
                END-IF
               END-IF                                                   
      *
               IF    LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1)  =  "0"
                 MOVE   ZERO   TO   WK-MIYAGI-COMPFTN
                 IF  LNK-SYU-KOH-HKNNUM(KOH-IDX)  = "015" OR "016" OR
                                                    "051" OR "052"
                    MOVE  WK-KOH-COMPFTN(KOH-IDX) TO  WK-MIYAGI-COMPFTN
                 END-IF
               END-IF
      **
               IF    LNK-SYU-SYUCOMPFTN   NOT =    ZERO
                       IF    LNK-SYU-SYUCOMPFTN  >  ZAN-KOHI-T(KOH-IDX)  
                           MOVE   ZAN-KOHI-T(KOH-IDX)
                                            TO  LNK-SYU-KOH-COMPFTN
                                                         (KOH-IDX)
                           MOVE   ZERO      TO  ZAN-KOHI-T(KOH-IDX)
                           PERFORM    23051-KOHHI-HIKAKU-SEC
                       ELSE
                           MOVE  LNK-SYU-SYUCOMPFTN                         
                                            TO  LNK-SYU-KOH-COMPFTN
                                                         (KOH-IDX)
                           COMPUTE    ZAN-KOHI-T(KOH-IDX)  =
                                      ZAN-KOHI-T(KOH-IDX)  -
                                      LNK-SYU-KOH-COMPFTN(KOH-IDX)
                           PERFORM    23051-KOHHI-HIKAKU-SEC
                       END-IF
               ELSE
                 IF    LNK-SYU-KOH-COMPFTN(1)   NOT =    ZERO
                       IF LNK-SYU-KOH-COMPFTN(1) >  ZAN-KOHI-T(KOH-IDX)
                          MOVE   ZAN-KOHI-T(KOH-IDX)
                                       TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                          MOVE   ZERO  TO  ZAN-KOHI-T(KOH-IDX)
                          PERFORM    23051-KOHHI-HIKAKU-SEC
                       ELSE
                          MOVE   LNK-SYU-KOH-COMPFTN(1)  
                                              TO  LNK-SYU-KOH-COMPFTN
                                                            (KOH-IDX)
                          PERFORM    23051-KOHHI-HIKAKU-SEC
                          COMPUTE   ZAN-KOHI-T(KOH-IDX)  =
                                    ZAN-KOHI-T(KOH-IDX)  -
                                    LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                 ELSE
                       IF WK-ZENGAKU    >    ZAN-KOHI-T(KOH-IDX)
                          MOVE   ZAN-KOHI-T(KOH-IDX)
                                       TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                          MOVE   ZERO  TO  ZAN-KOHI-T(KOH-IDX)
                          PERFORM    23051-KOHHI-HIKAKU-SEC
                       ELSE
                          MOVE   WK-ZENGAKU   TO  LNK-SYU-KOH-COMPFTN
                                                            (KOH-IDX)
                          PERFORM    23051-KOHHI-HIKAKU-SEC
                          COMPUTE   ZAN-KOHI-T(KOH-IDX)  =
                                    ZAN-KOHI-T(KOH-IDX)  -
                                    LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                 END-IF
               END-IF
               MOVE  WK-GAIYKZFTNKBN   TO  LNK-SYU-KOH-YKZFTNKBN
                                                        (KOH-IDX)
               IF    ZAN-KOHI-T(KOH-IDX)   NOT =   ZERO
                   COMPUTE   ZAN-KOHI-T(KOH-IDX) =
                             ZAN-KOHI-T(KOH-IDX)  +  5
                   COMPUTE   WK-FTN      =
                             ZAN-KOHI-T(KOH-IDX)  /  10
                   COMPUTE   ZAN-KOHI-T(KOH-IDX) =
                             WK-FTN   *  10
               END-IF
               EVALUATE   WK-GAIYKZFTNKBN
                   WHEN   "0"
                       MOVE  ZERO  TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                   WHEN   "1"
                       MOVE  YAKFTN-SOGOKEI
                                   TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                                       WK-KOH-YAKFTN-T(KOH-IDX)
                   WHEN   "2"
                       IF    YAKFTN-SOGOKEI  >  ZAN-KOHI-T(KOH-IDX)
                             MOVE   ZAN-KOHI-T(KOH-IDX)  
                                   TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                                       WK-KOH-YAKFTN-T(KOH-IDX)
                             MOVE   ZERO    TO  ZAN-KOHI-T(KOH-IDX)
                       ELSE
                             MOVE  YAKFTN-SOGOKEI  TO
                                       LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                                       WK-KOH-YAKFTN-T(KOH-IDX)
                             COMPUTE  ZAN-KOHI-T(KOH-IDX)   =
                                      ZAN-KOHI-T(KOH-IDX)   -
                                      LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                       END-IF
                   WHEN   "3"
                       MOVE  YAKFTN-SOGOKEI
                                   TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
               END-EVALUATE
               COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                         LNK-SYU-KOH-COMPFTN(KOH-IDX)     +
                         LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
               IF   LNK-SYU-KOH-HKNNUM(KOH-IDX)  =   "051"
               AND  LNK-SYU-SRYYMD(1:6)          <  "200310"
                    MOVE  YAKFTN-SOGOKEI   TO
                                       LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                    COMPUTE   WK-FTN  =
                              LNK-SYU-KOH-COMPFTN(KOH-IDX)  +
                              WK-KOH-YAKFTN-T(KOH-IDX)
                    IF        WK-FTN    >  1000
                        MOVE  1000   TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                        MOVE  ZERO   TO   WK-KOH-YAKFTN-T(KOH-IDX)
                    ELSE
                        MOVE  WK-FTN TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                        MOVE  ZERO   TO   WK-KOH-YAKFTN-T(KOH-IDX)
                    END-IF
               END-IF
           END-IF.
      *
       2305-KOH-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñ¤Î´µ¼ÔÉéÃ´¶â³Û·×»»¡Ê¾å¸Â³Û¤Ê¤·¡Ë                  *
      ******************************************************************
       2306-KOH-KEISAN2-SEC           SECTION.
      *
      *    ÉéÃ´¶â³Û¤Î·×»»
           IF  WK-GAIKFTNRATE     NOT =    ZERO
      *
               COMPUTE   WK-FUTANWARI  =
                              WK-GAIKFTNRATE  /  100
               IF    WK-KOH-FTNKBN   NOT =  SPACE
                 EVALUATE  WK-KOH-FTNKBN
                    WHEN  "1"
                      COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                                LNK-SYU-KYUFUGAI-GOKEI-TEN
                                        *  HKN-TENTNK
                                        *  WK-FUTANWARI
                      COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                                LNK-SYU-KYUFUGAI-GOKEI-TEN
                                        *  HKN-TENTNK
                                        *  WK-FUTANWARI
                    WHEN  "2"
                      MOVE    ZERO   TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                      MOVE    ZERO   TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                      MOVE    ZERO   TO   LNK-SYU-KYUFUGAI-SHOSHIN-TEN
                                          LNK-SYU-KYUFUGAI-SIDOU-TEN
                                          LNK-SYU-KYUFUGAI-OSIN1-TEN
                                          LNK-SYU-KYUFUGAI-OSIN2-TEN
                                          LNK-SYU-KYUFUGAI-OSIN3-TEN
                                          LNK-SYU-KYUFUGAI-GOKEI-TEN
                 END-EVALUATE
               ELSE
                 COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                           WK-ZENGAKU   *  WK-FUTANWARI
                 COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                           WK-ZENGAKU   *  WK-FUTANWARI
                 IF  (WK-1001-PREFNUM             =  01           )
                 AND (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "642" OR "643"
                                                 OR "644" OR "743")
                 AND (LNK-S01-SYOSINFLG       NOT =  SPACE        )
                     IF   LNK-SYU-KOH-COMPFTN(KOH-IDX)         <   580
                        MOVE  ZERO  TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     ELSE
                        COMPUTE  LNK-SYU-KOH-COMPFTN(KOH-IDX) =
                                 LNK-SYU-KOH-COMPFTN(KOH-IDX) -
                                 580
                     END-IF
                     IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  <   580
                        MOVE  ZERO  TO
                                 LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     ELSE
                        COMPUTE  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX) =
                                 LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX) -
                                 580
                     END-IF
                 END-IF
               END-IF
      *
               IF    LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1)  =  "0"
                 MOVE   ZERO   TO   WK-MIYAGI-COMPFTN
                 IF  LNK-SYU-KOH-HKNNUM(KOH-IDX)  = "010" OR "021"
      ***                                       OR  "015" OR "016"
                    MOVE  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                                            TO  WK-MIYAGI-COMPFTN
                 END-IF
               END-IF
      *
               IF   LNK-SYU-KOH-HKNNUM(KOH-IDX)  =      "027"           
      *             ¸º³Û
                    EVALUATE  WK-GENGAKUKBN
                      WHEN        1
                        IF  WK-GENGAKU-RITU = 100
                         MOVE  ZERO  TO  WK-GENGAKU-GAK
                         MOVE  ZERO  TO  WK-GENGAKU-GAK-ENTANI
                        ELSE
                         COMPUTE  WK-GENGAKU-RITUX =
                                  100 - WK-GENGAKU-RITU
                         COMPUTE  WK-GENGAKU-GAK               =
                                  LNK-SYU-KOH-COMPFTN(KOH-IDX) *
                                  WK-GENGAKU-RITUX             /
                                  100
                         COMPUTE  WK-GENGAKU-GAK-ENTANI               =
                                  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX) *
                                  WK-GENGAKU-RITUX                    /
                                  100
                        END-IF
                      WHEN        2
                        CONTINUE
                      WHEN        3
                      WHEN        4
                        MOVE  ZERO  TO  WK-GENGAKU-GAK
                        MOVE  ZERO  TO  WK-GENGAKU-GAK-ENTANI
                    END-EVALUATE
                    IF  WK-GENGAKUKBN  NOT =  ZERO
                        MOVE  WK-GENGAKU-GAK
                               TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                        MOVE  WK-GENGAKU-GAK-ENTANI
                               TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                    END-IF
                    MOVE   LNK-SYU-KOH-COMPFTN(KOH-IDX)                 
                                            TO WK-TOKUTEI-COMPFTN3      
               END-IF                                         
               IF ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"         ) OR
                  ( HKN-RECEFTNDSPKBN            =  "2"           ) OR
                  ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"      ) AND
                    (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "015" OR "016") )
                   MOVE    "1"   TO    WK-TAIHI-RECEFTNDSPKBN(KOH-IDX)
                   IF    WK-TOKUTEI-COMPFTN   NOT =    ZERO
                       MOVE  WK-TOKUTEI-COMPFTN
                                        TO  LNK-SYU-SYUCOMPFTN
                   END-IF
                   IF    LNK-SYU-KOH-HKNNUM(1)    =    "027"
                     IF  WK-TOKUTEI-COMPFTN3  NOT =    ZERO
                       MOVE  WK-TOKUTEI-COMPFTN3
                                        TO  LNK-SYU-KOH-COMPFTN(1)
                     END-IF
                   END-IF
               ELSE
                   COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX) =
                             LNK-SYU-KOH-COMPFTN(KOH-IDX)  +  5
                   COMPUTE   WK-FTN      =
                             LNK-SYU-KOH-COMPFTN(KOH-IDX)  /  10
                   COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX) =  WK-FTN
                                                              *  10
               END-IF
               MOVE  WK-GAIYKZFTNKBN TO  LNK-SYU-KOH-YKZFTNKBN(KOH-IDX)
           ELSE
      *
             IF  ((WK-1001-PREFNUM         =  43           ) AND
                  (COMB-KOHHKNNUM(KOH-IDX) = "143" OR "342"))
             OR  ( FLG-MIYAGI   NOT =  ZERO                 )
             OR  ((WK-1001-PREFNUM         =  01           ) AND
                  (COMB-KOHHKNNUM(KOH-IDX) = "121"         ) AND
                  (LNK-SYU-SRYYMD(1:6)    >= "200604"      ))
      *
               IF (WK-1001-PREFNUM         =  43           ) AND
                  (COMB-KOHHKNNUM(KOH-IDX) = "143" OR "342")
                  MOVE  "1"  TO  FLG-KUMAMOTO
                  MOVE  "1"  TO  WK-TAIHI-RECEFTNDSPKBN(KOH-IDX)
                  PERFORM   23061-KUMAMOTO-KOHI-SEC
               END-IF
               IF  FLG-MIYAGI   NOT =  ZERO
                  PERFORM   23062-MIYAGI-KOHI-SEC
               END-IF
               IF (WK-1001-PREFNUM         =  01           ) AND
                  (COMB-KOHHKNNUM(KOH-IDX) = "121"         ) AND
                  (LNK-SYU-SRYYMD(1:6)    >= "200604"      )
                  PERFORM   23063-HOKAIDO-KOHI-SEC
               END-IF
      *
             ELSE
      *
               IF    WK-GAIKKOTGAK  NOT =  ZERO
      *
                 MOVE  WK-GAIKKOTGAK TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                                  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
      *
                 IF ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"       ) OR
                    ( HKN-RECEFTNDSPKBN            =  "2"         ) OR
                    ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"    ) AND
                      (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  
                                                  "015" OR "016"  ) )
                   MOVE    "1"   TO    WK-TAIHI-RECEFTNDSPKBN(KOH-IDX)
                   IF    WK-TOKUTEI-COMPFTN   NOT =    ZERO
                       MOVE  WK-TOKUTEI-COMPFTN  TO  LNK-SYU-SYUCOMPFTN
                   END-IF
                 END-IF
                 IF  (( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"     ) OR
                      ( HKN-RECEFTNDSPKBN            =  "2"       ) OR
                      ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"  ) AND
                        (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  
                                                    "015" OR "016") ))
                 AND (  LNK-SYU-KOH-HKNNUM(1)        =  "027"        )
                   IF    WK-TOKUTEI-COMPFTN3   NOT =    ZERO            
                       MOVE   WK-TOKUTEI-COMPFTN3                       
                                            TO  LNK-SYU-KOH-COMPFTN(1)  
                   END-IF                                               
                 END-IF
      *
                 IF    LNK-SYU-SYUCOMPFTN   NOT =    ZERO
                   IF  LNK-SYU-KOH-COMPFTN(KOH-IDX)  
                                       >  LNK-SYU-SYUCOMPFTN      
                      MOVE  LNK-SYU-SYUCOMPFTN
                                       TO LNK-SYU-KOH-COMPFTN(KOH-IDX)  
                   END-IF                     
                 END-IF
      *
                 IF    LNK-SYU-SYUCOMPFTN-ENTANI   NOT =    ZERO        
                   IF  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)              
                                       >  LNK-SYU-SYUCOMPFTN-ENTANI     
                      MOVE  LNK-SYU-SYUCOMPFTN-ENTANI                   
                                TO LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  
                   END-IF                                               
                 END-IF                                                 
      *
                 MOVE  WK-GAIYKZFTNKBN TO LNK-SYU-KOH-YKZFTNKBN(KOH-IDX)
      *
               END-IF
      *
             END-IF
      *
           END-IF.
           PERFORM    23053-KOHHI-HIKAKU3-SEC.
           PERFORM    23051-KOHHI-HIKAKU-SEC.
           EVALUATE   WK-GAIYKZFTNKBN
               WHEN   "0"
                   MOVE  ZERO  TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
               WHEN   "1"
                   MOVE  YAKFTN-SOGOKEI
                               TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                                   WK-KOH-YAKFTN-T(KOH-IDX)
               WHEN   "2"
                   MOVE  YAKFTN-SOGOKEI
                               TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
                                   WK-KOH-YAKFTN-T(KOH-IDX)
               WHEN   "3"
                   MOVE  YAKFTN-SOGOKEI
                               TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
           END-EVALUATE.
           COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                     LNK-SYU-KOH-COMPFTN(KOH-IDX)     +
                     LNK-SYU-KOH-COMPYKZFTN(KOH-IDX).
      *
       2306-KOH-KEISAN2-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      ******************************************************************
       23051-KOHHI-HIKAKU-SEC           SECTION.
      *
           IF    LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "972" OR "974"
             IF  LNK-SYU-KOH-HKNNUM(1)       =  "027"
               IF  LNK-SYU-KOH-COMPFTN(1) < LNK-SYU-KOH-COMPFTN(KOH-IDX)
                   MOVE  LNK-SYU-KOH-COMPFTN(1)
                                        TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
               END-IF
             END-IF
           ELSE
             EVALUATE    KOH-IDX
               WHEN    2
                   IF  LNK-SYU-KOH-COMPFTN(1) < LNK-SYU-KOH-COMPFTN(2)
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  LNK-SYU-KOH-COMPFTN(1)
                                            TO  LNK-SYU-KOH-COMPFTN(2)
                   END-IF
               WHEN    3
                   IF  LNK-SYU-KOH-COMPFTN(2) < LNK-SYU-KOH-COMPFTN(3)
                       MOVE  LNK-SYU-KOH-COMPFTN(2)
                                            TO  LNK-SYU-KOH-COMPFTN(3)
                   END-IF
               WHEN    4
                   IF  LNK-SYU-KOH-COMPFTN(3) < LNK-SYU-KOH-COMPFTN(4)
                       MOVE  LNK-SYU-KOH-COMPFTN(3)
                                            TO  LNK-SYU-KOH-COMPFTN(4)
                   END-IF
             END-EVALUATE
           END-IF.
      *
       23051-KOHHI-HIKAKU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *        ¡Ê¥ì¥»µ­ºÜ¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢ÍÑ¡Ë
      ******************************************************************
       23052-KOHHI-HIKAKU2-SEC           SECTION.                             
      *
           IF    LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "972" OR "974"
             IF  LNK-SYU-KOH-HKNNUM(1)       =  "027"
               IF  WK-KOH-COMPFTN(1)  =  ALL"9"
                 IF  LNK-SYU-KOH-COMPFTN(1) < WK-KOH-COMPFTN(KOH-IDX)
                     MOVE  LNK-SYU-KOH-COMPFTN(1)
                                          TO  WK-KOH-COMPFTN(KOH-IDX)
                 END-IF
               ELSE
                 IF  WK-KOH-COMPFTN(1)      < WK-KOH-COMPFTN(KOH-IDX)
                     MOVE  WK-KOH-COMPFTN(1)
                                          TO  WK-KOH-COMPFTN(KOH-IDX)
                 END-IF
               END-IF
             END-IF
           ELSE
             EVALUATE    KOH-IDX
               WHEN    2
                 IF  WK-KOH-COMPFTN(1)  =  ALL"9"
                   IF  LNK-SYU-KOH-COMPFTN(1) < WK-KOH-COMPFTN(2)
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  LNK-SYU-KOH-COMPFTN(1)
                                            TO  WK-KOH-COMPFTN(2)
                   END-IF
                 ELSE
                   IF  WK-KOH-COMPFTN(1)      < WK-KOH-COMPFTN(2)
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  WK-KOH-COMPFTN(1)
                                            TO  WK-KOH-COMPFTN(2)
                   END-IF
                 END-IF
               WHEN    3
                 IF  WK-KOH-COMPFTN(2)  =  ALL"9"
                   IF  LNK-SYU-KOH-COMPFTN(2) < WK-KOH-COMPFTN(3)
                       MOVE  LNK-SYU-KOH-COMPFTN(2)
                                            TO  WK-KOH-COMPFTN(3)
                   END-IF
                 ELSE
                   IF  WK-KOH-COMPFTN(2)      < WK-KOH-COMPFTN(3)
                       MOVE  WK-KOH-COMPFTN(2)
                                            TO  WK-KOH-COMPFTN(3)
                   END-IF
                 END-IF
               WHEN    4
                 IF  WK-KOH-COMPFTN(3)  =  ALL"9"
                   IF  LNK-SYU-KOH-COMPFTN(3) < WK-KOH-COMPFTN(4)
                       MOVE  LNK-SYU-KOH-COMPFTN(3)
                                            TO  WK-KOH-COMPFTN(4)
                   END-IF
                 ELSE
                   IF  WK-KOH-COMPFTN(3)      < WK-KOH-COMPFTN(4)
                       MOVE  WK-KOH-COMPFTN(3)
                                            TO  WK-KOH-COMPFTN(4)
                   END-IF
                 END-IF
             END-EVALUATE
           END-IF.
      *
       23052-KOHHI-HIKAKU2-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *        ¡Ê±ßÃ±°Ì¡Ë
      ******************************************************************
       23053-KOHHI-HIKAKU3-SEC           SECTION.
      *
           IF    LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "972" OR "974"
             IF  LNK-SYU-KOH-HKNNUM(1)       =  "027"
               IF  LNK-SYU-KOH-COMPFTN-ENTANI(1) 
                               <     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(1) 
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
               END-IF
             END-IF
           ELSE
             EVALUATE    KOH-IDX
               WHEN    2
                   IF  LNK-SYU-KOH-COMPFTN-ENTANI(1) 
                                   <     LNK-SYU-KOH-COMPFTN-ENTANI(2)
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(1) 
                                     TO  LNK-SYU-KOH-COMPFTN-ENTANI(2)
                   END-IF
               WHEN    3
                   IF  LNK-SYU-KOH-COMPFTN-ENTANI(2) 
                                   <     LNK-SYU-KOH-COMPFTN-ENTANI(3)
                       MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(2) 
                                     TO  LNK-SYU-KOH-COMPFTN-ENTANI(3)
                   END-IF
               WHEN    4
                   IF  LNK-SYU-KOH-COMPFTN-ENTANI(3) 
                                   <     LNK-SYU-KOH-COMPFTN-ENTANI(4)
                       MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(3) 
                                     TO  LNK-SYU-KOH-COMPFTN-ENTANI(4)
                   END-IF
             END-EVALUATE
           END-IF.
      *
       23053-KOHHI-HIKAKU3-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ÆÃ¼ì·×»»¡Ê¸ÂÅÙ¤òÄ¶¤¨¤¿¾ì¹ç¡¢´µ¼ÔÉéÃ´È¯À¸¡ÊÁ´³Û¡Ë¡Ë
      ******************************************************************
       23054-TOKUSYU-SANTEI-SEC           SECTION.
      *
           IF        LNK-SYU-SYUCOMPFTN-ENTANI   NOT =    ZERO
           OR        FLG-GENBUTU70               NOT =    ZERO
                   IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                                 TO  WK-TAIHI-TOKU1
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI13
                          MOVE   ZAN-KOHI13   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI23
                          MOVE   ZAN-KOHI23   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI33
                          MOVE   ZAN-KOHI33   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI43
                          MOVE   ZAN-KOHI43   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF     WK-TAIHI-TOKU1   >=  ZAN-KOHI-T3(KOH-IDX)
                         MOVE   WK-TAIHI-TOKU1  TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     ELSE
                         MOVE   ZERO    TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     END-IF
                   ELSE
                     IF    LNK-SYU-SYUCOMPFTN-ENTANI
                                               >=  ZAN-KOHI-T3(KOH-IDX)
                         MOVE   LNK-SYU-SYUCOMPFTN-ENTANI  TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     ELSE
                         MOVE   ZERO    TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     END-IF
                   END-IF
                   PERFORM    23053-KOHHI-HIKAKU3-SEC
                   MOVE       LNK-SYU-SYUCOMPFTN-ENTANI  TO
                                                 WK-TOKUSYU-ENTANI
      *
                   IF   KOH-IDX    >     1
                      IF  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                             >=  ZAN-KOHI-T3(KOH-IDX)
                          CONTINUE
                      ELSE
                          MOVE   ZERO   TO
                                 LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                      END-IF
                   END-IF
           ELSE
               IF    LNK-SYU-KOH-COMPFTN-ENTANI(1)   NOT =    ZERO
                   IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                         TO  WK-TAIHI-TOKU1
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI13
                          MOVE   ZAN-KOHI13   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI23
                          MOVE   ZAN-KOHI23   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI33
                          MOVE   ZAN-KOHI33   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI43
                          MOVE   ZAN-KOHI43   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF     WK-TAIHI-TOKU1  >=  ZAN-KOHI-T3(KOH-IDX)
                         MOVE   WK-TAIHI-TOKU1  TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     ELSE
                         MOVE  ZERO    TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     END-IF
                   ELSE
                     IF    LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                              >=  ZAN-KOHI-T3(KOH-IDX)
                        MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(1)  TO
                                  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     ELSE
                        MOVE   ZERO    TO
                                  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     END-IF
                   END-IF
                   PERFORM    23053-KOHHI-HIKAKU3-SEC
                   MOVE       LNK-SYU-KOH-COMPFTN-ENTANI(1)  TO
                                                  WK-TOKUSYU-ENTANI
      *
                   IF   KOH-IDX    >     1
                      IF  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                             >=  ZAN-KOHI-T3(KOH-IDX)
                          CONTINUE
                      ELSE
                          MOVE   ZERO   TO
                                 LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                      END-IF
                   END-IF
               ELSE
                   IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     MOVE   WK-ZENGAKU   TO   WK-TAIHI-TOKU1
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI13
                          MOVE   ZAN-KOHI13   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI23
                          MOVE   ZAN-KOHI23   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI33
                          MOVE   ZAN-KOHI33   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU1    >   ZAN-KOHI43
                          MOVE   ZAN-KOHI43   TO  WK-TAIHI-TOKU1
                        END-IF
                     END-IF
                     IF    WK-TAIHI-TOKU1    >=  ZAN-KOHI-T3(KOH-IDX)
                         MOVE   WK-TAIHI-TOKU1  TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     ELSE
                         MOVE   ZERO    TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     END-IF
                   ELSE
                     IF    WK-ZENGAKU   >=  ZAN-KOHI-T3(KOH-IDX)
                        MOVE   WK-ZENGAKU  TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     ELSE
                        MOVE   ZERO    TO
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                     END-IF
                   END-IF
                   PERFORM    23053-KOHHI-HIKAKU3-SEC
                   MOVE       WK-ZENGAKU  TO      WK-TOKUSYU-ENTANI
      *
                   IF   KOH-IDX    >     1
                      IF  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                             >=  ZAN-KOHI-T3(KOH-IDX)
                          CONTINUE
                      ELSE
                          MOVE   ZERO   TO
                                 LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                      END-IF
                   END-IF
               END-IF
           END-IF
      **
           IF    HKN-RECEFTNDSPKBN      =  "2"
              IF    LNK-SYU-SYUCOMPFTN   NOT =    ZERO
              OR    FLG-GENBUTU70        NOT =    ZERO
                    IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                        (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                        (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                        (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                      MOVE   LNK-SYU-SYUCOMPFTN  TO  WK-TAIHI-TOKU2
                      IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI12
                          MOVE   ZAN-KOHI12   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI22
                          MOVE   ZAN-KOHI22   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI32
                          MOVE   ZAN-KOHI32   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI42
                          MOVE   ZAN-KOHI42   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF  WK-TAIHI-TOKU2  >=  ZAN-KOHI-T2(KOH-IDX)
                         MOVE   WK-TAIHI-TOKU2  TO
                                           WK-KOH-COMPFTN(KOH-IDX)
                      ELSE
                         MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                      END-IF
                    ELSE
                      IF  LNK-SYU-SYUCOMPFTN  >=  ZAN-KOHI-T2(KOH-IDX)
                         MOVE   LNK-SYU-SYUCOMPFTN  TO
                                           WK-KOH-COMPFTN(KOH-IDX)
                      ELSE
                         MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                      END-IF
                    END-IF
                    PERFORM   23052-KOHHI-HIKAKU2-SEC
                    MOVE      LNK-SYU-SYUCOMPFTN  TO
                                         WK-TOKUSYU-COMPFTN2
      *
                    IF   KOH-IDX    >     1
                      IF WK-KOH-COMPFTN(KOH-IDX) >= ZAN-KOHI-T2(KOH-IDX)
                          CONTINUE
                      ELSE
                          MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                      END-IF
                    END-IF
              ELSE
                IF  LNK-SYU-KOH-COMPFTN(1)   NOT =    ZERO
                    IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                        (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                        (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                        (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                      MOVE   LNK-SYU-KOH-COMPFTN(1)  TO  WK-TAIHI-TOKU2
                      IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI12
                          MOVE   ZAN-KOHI12   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI22
                          MOVE   ZAN-KOHI22   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI32
                          MOVE   ZAN-KOHI32   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI42
                          MOVE   ZAN-KOHI42   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF  WK-TAIHI-TOKU2  >=  ZAN-KOHI-T2(KOH-IDX)
                         MOVE   WK-TAIHI-TOKU2  TO
                                           WK-KOH-COMPFTN(KOH-IDX)
                      ELSE
                         MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                      END-IF
                    ELSE
                      IF  LNK-SYU-KOH-COMPFTN(1)
                                          >=  ZAN-KOHI-T2(KOH-IDX)
                         MOVE   LNK-SYU-KOH-COMPFTN(1)  TO
                                           WK-KOH-COMPFTN(KOH-IDX)
                      ELSE
                         MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                      END-IF
                    END-IF
                    PERFORM   23052-KOHHI-HIKAKU2-SEC
                    MOVE      LNK-SYU-KOH-COMPFTN(1)  TO
                                         WK-TOKUSYU-COMPFTN2
      *
                    IF   KOH-IDX    >     1
                      IF WK-KOH-COMPFTN(KOH-IDX) >= ZAN-KOHI-T2(KOH-IDX)
                          CONTINUE
                      ELSE
                          MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                      END-IF
                    END-IF
                ELSE
                    IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                        (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                        (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                        (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                      MOVE   WK-ZENGAKU  TO   WK-TAIHI-TOKU2
                      IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI12
                          MOVE   ZAN-KOHI12   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI22
                          MOVE   ZAN-KOHI22   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI32
                          MOVE   ZAN-KOHI32   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                        IF   WK-TAIHI-TOKU2    >   ZAN-KOHI42
                          MOVE   ZAN-KOHI42   TO  WK-TAIHI-TOKU2
                        END-IF
                      END-IF
                      IF  WK-TAIHI-TOKU2  >=  ZAN-KOHI-T2(KOH-IDX)
                         MOVE   WK-TAIHI-TOKU2  TO
                                           WK-KOH-COMPFTN(KOH-IDX)
                      ELSE
                         MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                      END-IF
                    ELSE
                      IF  WK-ZENGAKU    >=    ZAN-KOHI-T2(KOH-IDX)
                         MOVE   WK-ZENGAKU  TO
                                           WK-KOH-COMPFTN(KOH-IDX)
                      ELSE
                         MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                      END-IF
                    END-IF
                    PERFORM   23052-KOHHI-HIKAKU2-SEC
                    MOVE      WK-ZENGAKU  TO   WK-TOKUSYU-COMPFTN2
      *
                    IF   KOH-IDX    >     1
                      IF WK-KOH-COMPFTN(KOH-IDX) >= ZAN-KOHI-T2(KOH-IDX)
                          CONTINUE
                      ELSE
                          MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                      END-IF
                    END-IF
                END-IF
              END-IF
           END-IF
      **
           IF    LNK-SYU-SYUCOMPFTN   NOT =    ZERO
           OR    FLG-GENBUTU70        NOT =    ZERO
                  IF   FLG-KOUGAKUCYO-GEN70   =   1
                    MOVE  LNK-SYU-SYUCOMPFTN  TO  WK-TOKUSYU
                  ELSE
                    COMPUTE  WK-TOKUSYU =  LNK-SYU-SYUCOMPFTN  +   5
                    COMPUTE  WK-TOKUSYU =  WK-TOKUSYU          /  10
                    COMPUTE  WK-TOKUSYU =  WK-TOKUSYU          *  10
                  END-IF
      *
                  IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI1
                          MOVE   ZAN-KOHI1   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI2
                          MOVE   ZAN-KOHI2   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI3
                          MOVE   ZAN-KOHI3   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI4
                          MOVE   ZAN-KOHI4   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF  WK-TOKUSYU   >=  ZAN-KOHI-T(KOH-IDX)
                         MOVE   WK-TOKUSYU          TO
                                           LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     ELSE
                         MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     END-IF
                  ELSE
                     IF  WK-TOKUSYU   >=  ZAN-KOHI-T(KOH-IDX)
                         MOVE   LNK-SYU-SYUCOMPFTN  TO
                                           LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     ELSE
                         MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     END-IF
                  END-IF
                  PERFORM    23051-KOHHI-HIKAKU-SEC
                  MOVE       LNK-SYU-SYUCOMPFTN  TO  WK-TOKUSYU-COMPFTN
      *
                  IF   KOH-IDX    >     1
                      COMPUTE  WK-TOKUSYU =
                                  LNK-SYU-KOH-COMPFTN(KOH-IDX)  +    5
                      COMPUTE  WK-TOKUSYU  =
                                  WK-TOKUSYU                    /   10
                      COMPUTE  WK-TOKUSYU  =
                                  WK-TOKUSYU                    *   10
                      IF  WK-TOKUSYU   >=  ZAN-KOHI-T(KOH-IDX)
                          CONTINUE
                      ELSE
                          MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                      END-IF
                  END-IF
           ELSE
              IF  LNK-SYU-KOH-COMPFTN(1)   NOT =    ZERO
                  COMPUTE  WK-TOKUSYU =
                                    LNK-SYU-KOH-COMPFTN(1) +    5
                  COMPUTE  WK-TOKUSYU  =
                                    WK-TOKUSYU             /   10
                  COMPUTE  WK-TOKUSYU  =
                                    WK-TOKUSYU             *   10
      *
                  IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI1
                          MOVE   ZAN-KOHI1   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI2
                          MOVE   ZAN-KOHI2   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI3
                          MOVE   ZAN-KOHI3   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI4
                          MOVE   ZAN-KOHI4   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF  WK-TOKUSYU  >=  ZAN-KOHI-T(KOH-IDX)
                         MOVE   WK-TOKUSYU          TO
                                           LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     ELSE
                         MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     END-IF
                  ELSE
                     IF  WK-TOKUSYU  >=  ZAN-KOHI-T(KOH-IDX)
                         MOVE   LNK-SYU-KOH-COMPFTN(1)  TO
                                           LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     ELSE
                         MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     END-IF
                  END-IF
                  PERFORM    23051-KOHHI-HIKAKU-SEC
                  MOVE       LNK-SYU-KOH-COMPFTN(1)  TO
                                        WK-TOKUSYU-COMPFTN
      *
                  IF   KOH-IDX    >     1
                      COMPUTE  WK-TOKUSYU =
                                  LNK-SYU-KOH-COMPFTN(KOH-IDX)  +    5
                      COMPUTE  WK-TOKUSYU  =
                                  WK-TOKUSYU                    /   10
                      COMPUTE  WK-TOKUSYU  =
                                  WK-TOKUSYU                    *   10
                      IF  WK-TOKUSYU   >=  ZAN-KOHI-T(KOH-IDX)
                          CONTINUE
                      ELSE
                          MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                      END-IF
                  END-IF
              ELSE
                  COMPUTE  WK-TOKUSYU =
                                    WK-ZENGAKU             +    5
                  COMPUTE  WK-TOKUSYU  =
                                    WK-TOKUSYU             /   10
                  COMPUTE  WK-TOKUSYU  =
                                    WK-TOKUSYU             *   10
      *
                  IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI1
                          MOVE   ZAN-KOHI1   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI2
                          MOVE   ZAN-KOHI2   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI3
                          MOVE   ZAN-KOHI3   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   WK-TOKUSYU    >   ZAN-KOHI4
                          MOVE   ZAN-KOHI4   TO  WK-TOKUSYU
                       END-IF
                     END-IF
                     IF  WK-TOKUSYU    >=    ZAN-KOHI-T(KOH-IDX)
                         MOVE   WK-TOKUSYU          TO
                                           LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     ELSE
                         MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     END-IF
                  ELSE
                     IF  WK-TOKUSYU    >=    ZAN-KOHI-T(KOH-IDX)
                         MOVE   WK-ZENGAKU  TO
                                           LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     ELSE
                         MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                     END-IF
                  END-IF
                  PERFORM    23051-KOHHI-HIKAKU-SEC
                  MOVE       WK-ZENGAKU  TO   WK-TOKUSYU-COMPFTN
      *
                  IF   KOH-IDX    >     1
                      COMPUTE  WK-TOKUSYU =
                                  LNK-SYU-KOH-COMPFTN(KOH-IDX)  +    5
                      COMPUTE  WK-TOKUSYU  =
                                  WK-TOKUSYU                    /   10
                      COMPUTE  WK-TOKUSYU  =
                                  WK-TOKUSYU                    *   10
                      IF  WK-TOKUSYU   >=  ZAN-KOHI-T(KOH-IDX)
                          CONTINUE
                      ELSE
                          MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                      END-IF
                  END-IF
              END-IF
           END-IF
      **
           MOVE  WK-GAIYKZFTNKBN   TO  LNK-SYU-KOH-YKZFTNKBN(KOH-IDX)
           MOVE  ZERO              TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
      *
           COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                     LNK-SYU-KOH-COMPFTN(KOH-IDX)     +
                     LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
      *
      *    ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
           EVALUATE    KOH-IDX
               WHEN    2
                   IF  LNK-SYU-KOH-COMPFTN(1) < WK-TOKUSYU-COMPFTN
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  LNK-SYU-KOH-COMPFTN(1)
                                            TO  WK-TOKUSYU-COMPFTN
                   END-IF
               WHEN    3
                   IF  LNK-SYU-KOH-COMPFTN(2) < WK-TOKUSYU-COMPFTN
                       MOVE  LNK-SYU-KOH-COMPFTN(2)
                                            TO  WK-TOKUSYU-COMPFTN
                   END-IF
               WHEN    4
                   IF  LNK-SYU-KOH-COMPFTN(3) < WK-TOKUSYU-COMPFTN
                       MOVE  LNK-SYU-KOH-COMPFTN(3)
                                            TO  WK-TOKUSYU-COMPFTN
                   END-IF
           END-EVALUATE
      *
      *    ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *    ¡Ê¥ì¥»µ­ºÜ¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢ÍÑ¡Ë
           IF    HKN-RECEFTNDSPKBN      =  "2"
             EVALUATE    KOH-IDX
               WHEN    2
                 IF  WK-KOH-COMPFTN(1)  =  ALL"9"
                   IF  LNK-SYU-KOH-COMPFTN(1) < WK-TOKUSYU-COMPFTN2
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  LNK-SYU-KOH-COMPFTN(1)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 ELSE
                   IF  WK-KOH-COMPFTN(1)      < WK-TOKUSYU-COMPFTN2
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  WK-KOH-COMPFTN(1)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 END-IF
               WHEN    3
                 IF  WK-KOH-COMPFTN(2)  =  ALL"9"
                   IF  LNK-SYU-KOH-COMPFTN(2) < WK-TOKUSYU-COMPFTN2
                       MOVE  LNK-SYU-KOH-COMPFTN(2)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 ELSE
                   IF  WK-KOH-COMPFTN(2)      < WK-TOKUSYU-COMPFTN2
                       MOVE  WK-KOH-COMPFTN(2)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 END-IF
               WHEN    4
                 IF  WK-KOH-COMPFTN(3)  =  ALL"9"
                   IF  LNK-SYU-KOH-COMPFTN(3) < WK-TOKUSYU-COMPFTN2
                       MOVE  LNK-SYU-KOH-COMPFTN(3)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 ELSE
                   IF  WK-KOH-COMPFTN(3)      < WK-TOKUSYU-COMPFTN2
                       MOVE  WK-KOH-COMPFTN(3)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 END-IF
             END-EVALUATE
           END-IF
      *
      *    ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *    ¡Ê±ßÃ±°Ì¡Ë
           EVALUATE    KOH-IDX
               WHEN    2
                   IF  LNK-SYU-KOH-COMPFTN-ENTANI(1) 
                                   <     WK-TOKUSYU-ENTANI
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                     TO  WK-TOKUSYU-ENTANI
                   END-IF
               WHEN    3
                   IF  LNK-SYU-KOH-COMPFTN-ENTANI(2) 
                                   <     WK-TOKUSYU-ENTANI
                       MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(2)
                                     TO  WK-TOKUSYU-ENTANI
                   END-IF
               WHEN    4
                   IF  LNK-SYU-KOH-COMPFTN-ENTANI(3) 
                                   <     WK-TOKUSYU-ENTANI
                       MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(3) 
                                     TO  WK-TOKUSYU-ENTANI
                   END-IF
           END-EVALUATE
      *
      *    ¤¤¤ï¤­»Ô½Å¿´
      *
           IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" ) OR
                (FLG-KOUREI-KBN               =       "1" ))
           AND ( COMB-HKNNUM            NOT   =     SPACE  )
      *
              MOVE   ZERO   TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                                 LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                 WK-TOKUSYU-COMPFTN
                                 WK-TOKUSYU-ENTANI
      *
              IF    HKN-RECEFTNDSPKBN    =  "2"
                   MOVE   ZERO   TO    WK-KOH-COMPFTN(KOH-IDX)
                                       WK-TOKUSYU-COMPFTN2
              END-IF
      *
              COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                        LNK-SYU-KOH-COMPFTN(KOH-IDX)     +
                        LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
      *
           END-IF
      *
           .
      *
       23054-TOKUSYU-SANTEI-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ÆÃ¼ì·×»»¡Ê¸ÂÅÙ¤òÄ¶¤¨¤¿¾ì¹ç¡¢´µ¼ÔÉéÃ´È¯À¸¡ÊÄ¶¤¨¤¿³Û¡Ë¡Ë
      ******************************************************************
       23055-TOKUSYU-SANTEI2-SEC           SECTION.
      *
           IF        LNK-SYU-SYUCOMPFTN-ENTANI   NOT =    ZERO
           OR        FLG-GENBUTU70               NOT =    ZERO
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI  TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   PERFORM    23053-KOHHI-HIKAKU3-SEC
      *
                   IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI13
                          MOVE   ZAN-KOHI13
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI23
                          MOVE   ZAN-KOHI23
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI33
                          MOVE   ZAN-KOHI33
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI43
                          MOVE   ZAN-KOHI43
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                   END-IF
      *
                   IF    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                             >   ZAN-KOHI-T3(KOH-IDX)
                       COMPUTE  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                                LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  -
                                ZAN-KOHI-T3(KOH-IDX)
                   ELSE
                       MOVE   ZERO  TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   END-IF
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI  TO
                                                 WK-TOKUSYU-ENTANI
           ELSE
               IF    LNK-SYU-KOH-COMPFTN-ENTANI(1)   NOT =    ZERO
                   MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(1)  TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   PERFORM    23053-KOHHI-HIKAKU3-SEC
      *
                   IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI13
                          MOVE   ZAN-KOHI13
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI23
                          MOVE   ZAN-KOHI23
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI33
                          MOVE   ZAN-KOHI33
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI43
                          MOVE   ZAN-KOHI43
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                   END-IF
      *
                   IF    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                             >   ZAN-KOHI-T3(KOH-IDX)
                       COMPUTE  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                                LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  -
                                ZAN-KOHI-T3(KOH-IDX)
                   ELSE
                       MOVE   ZERO  TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   END-IF
                   MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(1)  TO
                                                  WK-TOKUSYU-ENTANI
               ELSE
                   MOVE   WK-ZENGAKU  TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   PERFORM    23053-KOHHI-HIKAKU3-SEC
      *
                   IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI13
                          MOVE   ZAN-KOHI13
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI23
                          MOVE   ZAN-KOHI23
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI33
                          MOVE   ZAN-KOHI33
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                                  >  ZAN-KOHI43
                          MOVE   ZAN-KOHI43
                              TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                   END-IF
      *
                   IF    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                             >   ZAN-KOHI-T3(KOH-IDX)
                       COMPUTE  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                                LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  -
                                ZAN-KOHI-T3(KOH-IDX)
                   ELSE
                       MOVE   ZERO  TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   END-IF
                   MOVE   WK-ZENGAKU   TO   WK-TOKUSYU-ENTANI
               END-IF
           END-IF
      **
           IF    HKN-RECEFTNDSPKBN      =  "2"
              IF    LNK-SYU-SYUCOMPFTN   NOT =    ZERO
              OR    FLG-GENBUTU70        NOT =    ZERO
                   MOVE  LNK-SYU-SYUCOMPFTN  TO  WK-KOH-COMPFTN(KOH-IDX)
                   PERFORM    23052-KOHHI-HIKAKU2-SEC
      *
                   IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI12
                          MOVE   ZAN-KOHI12
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI22
                          MOVE   ZAN-KOHI22
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI32
                          MOVE   ZAN-KOHI32
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI42
                          MOVE   ZAN-KOHI42
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                   END-IF
      *
                   IF    WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI-T2(KOH-IDX)
                       COMPUTE  WK-KOH-COMPFTN(KOH-IDX)  =
                                WK-KOH-COMPFTN(KOH-IDX)  -
                                ZAN-KOHI-T2(KOH-IDX)
                   ELSE
                       MOVE   ZERO    TO   WK-KOH-COMPFTN(KOH-IDX)
                   END-IF
                   MOVE   LNK-SYU-SYUCOMPFTN  TO  WK-TOKUSYU-COMPFTN2
              ELSE
                IF  LNK-SYU-KOH-COMPFTN(1)   NOT =    ZERO
                   MOVE  LNK-SYU-KOH-COMPFTN(1) TO
                                                 WK-KOH-COMPFTN(KOH-IDX)
                   PERFORM    23052-KOHHI-HIKAKU2-SEC
      *
                   IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI12
                          MOVE   ZAN-KOHI12
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI22
                          MOVE   ZAN-KOHI22
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI32
                          MOVE   ZAN-KOHI32
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI42
                          MOVE   ZAN-KOHI42
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                   END-IF
      *
                   IF    WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI-T2(KOH-IDX)
                       COMPUTE  WK-KOH-COMPFTN(KOH-IDX)  =
                                WK-KOH-COMPFTN(KOH-IDX)  -
                                ZAN-KOHI-T2(KOH-IDX)
                   ELSE
                       MOVE   ZERO    TO   WK-KOH-COMPFTN(KOH-IDX)
                   END-IF
                   MOVE   LNK-SYU-KOH-COMPFTN(1) TO  WK-TOKUSYU-COMPFTN2
                ELSE
                   MOVE   WK-ZENGAKU  TO   WK-KOH-COMPFTN(KOH-IDX)
                   PERFORM    23052-KOHHI-HIKAKU2-SEC
      *
                   IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                       (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI12
                          MOVE   ZAN-KOHI12
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI22
                          MOVE   ZAN-KOHI22
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI32
                          MOVE   ZAN-KOHI32
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI42
                          MOVE   ZAN-KOHI42
                                     TO  WK-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                   END-IF
      *
                   IF    WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI-T2(KOH-IDX)
                       COMPUTE  WK-KOH-COMPFTN(KOH-IDX)  =
                                WK-KOH-COMPFTN(KOH-IDX)  -
                                ZAN-KOHI-T2(KOH-IDX)
                   ELSE
                       MOVE   ZERO    TO   WK-KOH-COMPFTN(KOH-IDX)
                   END-IF
                   MOVE   WK-ZENGAKU  TO   WK-TOKUSYU-COMPFTN2
                END-IF
              END-IF
           END-IF
      **
           IF    LNK-SYU-SYUCOMPFTN   NOT =    ZERO
           OR    FLG-GENBUTU70        NOT =    ZERO
                  IF   FLG-KOUGAKUCYO-GEN70   =   1
                    MOVE  LNK-SYU-SYUCOMPFTN  TO  WK-TOKUSYU
                  ELSE
                    COMPUTE  WK-TOKUSYU =  LNK-SYU-SYUCOMPFTN  +   5
                    COMPUTE  WK-TOKUSYU =  WK-TOKUSYU          /  10
                    COMPUTE  WK-TOKUSYU =  WK-TOKUSYU          *  10
                  END-IF
      *
                  MOVE  WK-TOKUSYU  TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  PERFORM    23051-KOHHI-HIKAKU-SEC
      *
                  IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI1
                          MOVE   ZAN-KOHI1
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI2
                          MOVE   ZAN-KOHI2
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI3
                          MOVE   ZAN-KOHI3
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI4
                          MOVE   ZAN-KOHI4
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                  END-IF
      *
                  IF    LNK-SYU-KOH-COMPFTN(KOH-IDX)  >
                                            ZAN-KOHI-T(KOH-IDX)
                      COMPUTE  LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                               LNK-SYU-KOH-COMPFTN(KOH-IDX)  -
                               ZAN-KOHI-T(KOH-IDX)
                  ELSE
                      MOVE   ZERO    TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  END-IF
                  MOVE   LNK-SYU-SYUCOMPFTN  TO  WK-TOKUSYU-COMPFTN
           ELSE
              IF  LNK-SYU-KOH-COMPFTN(1)   NOT =    ZERO
                  COMPUTE  WK-TOKUSYU =  LNK-SYU-KOH-COMPFTN(1)  +   5
                  COMPUTE  WK-TOKUSYU =  WK-TOKUSYU              /  10
                  COMPUTE  WK-TOKUSYU =  WK-TOKUSYU              *  10
      *
                  MOVE  WK-TOKUSYU  TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  PERFORM    23051-KOHHI-HIKAKU-SEC
      *
                  IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI1
                          MOVE   ZAN-KOHI1
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI2
                          MOVE   ZAN-KOHI2
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI3
                          MOVE   ZAN-KOHI3
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI4
                          MOVE   ZAN-KOHI4
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                  END-IF
      *
                  IF    LNK-SYU-KOH-COMPFTN(KOH-IDX)  >
                                            ZAN-KOHI-T(KOH-IDX)
                      COMPUTE  LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                               LNK-SYU-KOH-COMPFTN(KOH-IDX)  -
                               ZAN-KOHI-T(KOH-IDX)
                  ELSE
                      MOVE   ZERO    TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  END-IF
                  MOVE   LNK-SYU-KOH-COMPFTN(1) TO  WK-TOKUSYU-COMPFTN
              ELSE
                  COMPUTE  WK-TOKUSYU =  WK-ZENGAKU  +   5
                  COMPUTE  WK-TOKUSYU =  WK-TOKUSYU  /  10
                  COMPUTE  WK-TOKUSYU =  WK-TOKUSYU  *  10
      *
                  MOVE  WK-TOKUSYU  TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  PERFORM    23051-KOHHI-HIKAKU-SEC
      *
                  IF  (LNK-SYU-KOH-HKNNUM(1)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(2)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(3)  =  "972" OR "974") OR
                      (LNK-SYU-KOH-HKNNUM(4)  =  "972" OR "974")
                     IF   LNK-SYU-KOH-HKNNUM(1) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI1
                          MOVE   ZAN-KOHI1
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(2) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI2
                          MOVE   ZAN-KOHI2
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(3) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI3
                          MOVE   ZAN-KOHI3
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYU-KOH-HKNNUM(4) =  "972" OR "974"
                       IF   LNK-SYU-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI4
                          MOVE   ZAN-KOHI4
                                     TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     END-IF
                  END-IF
      *
                  IF    LNK-SYU-KOH-COMPFTN(KOH-IDX)  >
                                            ZAN-KOHI-T(KOH-IDX)
                      COMPUTE  LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                               LNK-SYU-KOH-COMPFTN(KOH-IDX)  -
                               ZAN-KOHI-T(KOH-IDX)
                  ELSE
                      MOVE   ZERO    TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  END-IF
                  MOVE   WK-ZENGAKU  TO   WK-TOKUSYU-COMPFTN
              END-IF
           END-IF
      **
           MOVE  WK-GAIYKZFTNKBN   TO  LNK-SYU-KOH-YKZFTNKBN(KOH-IDX)
           MOVE  ZERO              TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
      *
           COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                     LNK-SYU-KOH-COMPFTN(KOH-IDX)     +
                     LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
      *
      *    ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
           EVALUATE    KOH-IDX
               WHEN    2
                   IF  LNK-SYU-KOH-COMPFTN(1) < WK-TOKUSYU-COMPFTN
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  LNK-SYU-KOH-COMPFTN(1)
                                            TO  WK-TOKUSYU-COMPFTN
                   END-IF
               WHEN    3
                   IF  LNK-SYU-KOH-COMPFTN(2) < WK-TOKUSYU-COMPFTN
                       MOVE  LNK-SYU-KOH-COMPFTN(2)
                                            TO  WK-TOKUSYU-COMPFTN
                   END-IF
               WHEN    4
                   IF  LNK-SYU-KOH-COMPFTN(3) < WK-TOKUSYU-COMPFTN
                       MOVE  LNK-SYU-KOH-COMPFTN(3)
                                            TO  WK-TOKUSYU-COMPFTN
                   END-IF
           END-EVALUATE
      *
      *    ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *    ¡Ê¥ì¥»µ­ºÜ¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢ÍÑ¡Ë
           IF    HKN-RECEFTNDSPKBN      =  "2"
             EVALUATE    KOH-IDX
               WHEN    2
                 IF  WK-KOH-COMPFTN(1)  =  ALL"9"
                   IF  LNK-SYU-KOH-COMPFTN(1) < WK-TOKUSYU-COMPFTN2
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  LNK-SYU-KOH-COMPFTN(1)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 ELSE
                   IF  WK-KOH-COMPFTN(1)      < WK-TOKUSYU-COMPFTN2
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  WK-KOH-COMPFTN(1)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 END-IF
               WHEN    3
                 IF  WK-KOH-COMPFTN(2)  =  ALL"9"
                   IF  LNK-SYU-KOH-COMPFTN(2) < WK-TOKUSYU-COMPFTN2
                       MOVE  LNK-SYU-KOH-COMPFTN(2)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 ELSE
                   IF  WK-KOH-COMPFTN(2)      < WK-TOKUSYU-COMPFTN2
                       MOVE  WK-KOH-COMPFTN(2)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 END-IF
               WHEN    4
                 IF  WK-KOH-COMPFTN(3)  =  ALL"9"
                   IF  LNK-SYU-KOH-COMPFTN(3) < WK-TOKUSYU-COMPFTN2
                       MOVE  LNK-SYU-KOH-COMPFTN(3)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 ELSE
                   IF  WK-KOH-COMPFTN(3)      < WK-TOKUSYU-COMPFTN2
                       MOVE  WK-KOH-COMPFTN(3)
                                            TO  WK-TOKUSYU-COMPFTN2
                   END-IF
                 END-IF
             END-EVALUATE
           END-IF
      *
      *    ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *    ¡Ê±ßÃ±°Ì¡Ë
           EVALUATE    KOH-IDX
               WHEN    2
                   IF  LNK-SYU-KOH-COMPFTN-ENTANI(1) 
                                   <     WK-TOKUSYU-ENTANI
                   AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                       MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                     TO  WK-TOKUSYU-ENTANI
                   END-IF
               WHEN    3
                   IF  LNK-SYU-KOH-COMPFTN-ENTANI(2) 
                                   <     WK-TOKUSYU-ENTANI
                       MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(2)
                                     TO  WK-TOKUSYU-ENTANI
                   END-IF
               WHEN    4
                   IF  LNK-SYU-KOH-COMPFTN-ENTANI(3) 
                                   <     WK-TOKUSYU-ENTANI
                       MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(3) 
                                     TO  WK-TOKUSYU-ENTANI
                   END-IF
           END-EVALUATE
      *
           .
      *
       23055-TOKUSYU-SANTEI2-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ÆÃ¼ì·×»»¡Ê£²³äËô¤Ï£³³ä¤Î³Û¤ò»»½Ð¸å¡¢È¾³Û¤ò´µ¼ÔÉéÃ´¤È¤¹¤ë¡Ë
      ******************************************************************
       23056-TOKUSYU-SANTEI3-SEC           SECTION.
      *
           IF     LNK-SYU-SYUCOMPFTN-ENTANI       NOT =    ZERO
           OR     FLG-GENBUTU70                   NOT =    ZERO
                  COMPUTE     WK-HANGAKU-ENTANI     =
                              LNK-SYU-SYUCOMPFTN-ENTANI      *  0.5
           ELSE
             IF   LNK-SYU-KOH-COMPFTN-ENTANI(1)   NOT =    ZERO
                    COMPUTE   WK-HANGAKU-ENTANI     =
                              LNK-SYU-KOH-COMPFTN-ENTANI(1)  *  0.5
             ELSE
                    COMPUTE   WK-HANGAKU-ENTANI     =
                              WK-ZENGAKU  *  0.5
             END-IF
           END-IF
      *
           IF    WK-HANGAKU-ENTANI  >  ZAN-KOHI-T3(KOH-IDX)
               MOVE   ZAN-KOHI-T3(KOH-IDX)
                     TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
           ELSE
               MOVE   WK-HANGAKU-ENTANI
                     TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
           END-IF
      *
           PERFORM    23053-KOHHI-HIKAKU3-SEC
      *
      *
           IF     HKN-RECEFTNDSPKBN          =  "2"
      *
              IF      LNK-SYU-SYUCOMPFTN       NOT =   ZERO
              OR      FLG-GENBUTU70            NOT =   ZERO
                      COMPUTE   WK-HANGAKU-COMPFTN   =
                                LNK-SYU-SYUCOMPFTN      *  0.5
              ELSE
                IF    LNK-SYU-KOH-COMPFTN(1)   NOT =   ZERO
                      COMPUTE   WK-HANGAKU-COMPFTN   =
                                LNK-SYU-KOH-COMPFTN(1)  *  0.5
                ELSE
                      COMPUTE   WK-HANGAKU-COMPFTN   =
                                WK-ZENGAKU  *  0.5
                END-IF
              END-IF
      *
              IF    WK-HANGAKU-COMPFTN  >  ZAN-KOHI-T2(KOH-IDX)
                  MOVE   ZAN-KOHI-T2(KOH-IDX)
                                   TO  WK-KOH-COMPFTN(KOH-IDX)
              ELSE
                  MOVE  WK-HANGAKU-COMPFTN
                                   TO  WK-KOH-COMPFTN(KOH-IDX)
              END-IF
      *
              PERFORM    23052-KOHHI-HIKAKU2-SEC
      *
           END-IF
      *
      *
      ***  IF      LNK-SYU-SYUCOMPFTN       NOT =   ZERO
           IF      WK-TOKUTEI-COMPFTN       NOT =   ZERO
           OR      FLG-GENBUTU70            NOT =   ZERO
                   COMPUTE   WK-HANGAKU-COMPFTN   =
      ***                    LNK-SYU-SYUCOMPFTN      *  0.5
                             WK-TOKUTEI-COMPFTN      *  0.5
           ELSE
      ***    IF    LNK-SYU-KOH-COMPFTN(1)   NOT =   ZERO
             IF    WK-TOKUTEI-COMPFTN3      NOT =   ZERO
                   COMPUTE   WK-HANGAKU-COMPFTN   =
      ***                    LNK-SYU-KOH-COMPFTN(1)  *  0.5
                             WK-TOKUTEI-COMPFTN3     *  0.5
             ELSE
                   COMPUTE   WK-HANGAKU-COMPFTN   =
                             WK-ZENGAKU  *  0.5
             END-IF
           END-IF
      *
           IF    WK-HANGAKU-COMPFTN  >  ZAN-KOHI-T(KOH-IDX)
               MOVE   ZAN-KOHI-T(KOH-IDX)
                             TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
           ELSE
               MOVE  WK-HANGAKU-COMPFTN
                             TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
           END-IF
      *
           PERFORM    23051-KOHHI-HIKAKU-SEC
      *
      *
           MOVE  WK-GAIYKZFTNKBN   TO  LNK-SYU-KOH-YKZFTNKBN(KOH-IDX)
           MOVE  ZERO              TO  LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
      *
           COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                     LNK-SYU-KOH-COMPFTN(KOH-IDX)     +
                     LNK-SYU-KOH-COMPYKZFTN(KOH-IDX)
      *
           .
       23056-TOKUSYU-SANTEI3-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ·§ËÜ¸©ÃÏÊý¸øÈñ¡Ê£±¡¿£³¡Ë·×»»
      ******************************************************************
       23061-KUMAMOTO-KOHI-SEC           SECTION.
      *
      *    ÉéÃ´¶â³Û¤Î·×»»
      *
           IF     LNK-SYU-SYUCOMPFTN  NOT  =  ZERO
           OR     FLG-GENBUTU70       NOT  =  ZERO
                  COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                            LNK-SYU-SYUCOMPFTN            +  5
                  COMPUTE   WK-FTN  =  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                                    /  10
                  COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =  WK-FTN
                                                          *  10
           ELSE
             IF   (LNK-SYU-KOH-HKNNUM(1)       =   "027")
             AND  (LNK-SYU-KOH-COMPFTN(1)  NOT =   ZERO )
                  COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                            LNK-SYU-KOH-COMPFTN(1)        +  5
                  COMPUTE   WK-FTN  =  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                                    /  10
                  COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =  WK-FTN
                                                          *  10
             ELSE
                  MOVE    ZERO    TO    LNK-SYU-KOH-COMPFTN(KOH-IDX)
             END-IF
           END-IF
      *
           IF     LNK-SYU-KOH-COMPFTN(KOH-IDX)   NOT  =  ZERO
                  MOVE      LNK-SYU-KOH-COMPFTN(KOH-IDX)  TO  WK-FTN
      *
                  COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                            ( WK-FTN  /  3 ) + 0.9
      *
                  COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                            ( WK-FTN  /  3 )
           END-IF
      *
           MOVE  WK-GAIYKZFTNKBN TO  LNK-SYU-KOH-YKZFTNKBN(KOH-IDX)
      *
           .
      *
       23061-KUMAMOTO-KOHI-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        µÜ¾ë¸©ÃÏÊý¸øÈñ·×»»¡ÊÆýÍÄ»ù¡Ë
      ******************************************************************
       23062-MIYAGI-KOHI-SEC           SECTION.
      *
           IF   WK-TOKUTEI-COMPFTN   NOT =    ZERO
           OR   FLG-GENBUTU70        NOT =    ZERO
              EVALUATE    FLG-MIYAGI
                WHEN         1
                    IF   LNK-SYU-SRYYMD(1:6)   <=    "200603"
                       MOVE   0.25    TO     WK-KEISU
                    END-IF
                    IF ( LNK-SYU-SRYYMD(1:6)    >=   "200604" ) AND
                       ( LNK-SYU-SRYYMD(1:6)    <=   "200703" )
                       MOVE   0.50    TO     WK-KEISU
                    END-IF
                    IF ( LNK-SYU-SRYYMD(1:6)    >=   "200704" ) AND
                       ( LNK-SYU-SRYYMD(1:6)    <=   "200803" )
                       MOVE   0.75    TO     WK-KEISU
                    END-IF
                    IF   LNK-SYU-SRYYMD(1:6)    >=   "200804"
                       MOVE   1.00    TO     WK-KEISU
                    END-IF
                WHEN         2
                    MOVE      0.50    TO     WK-KEISU
              END-EVALUATE
              IF    WK-MIYAGI-COMPFTN  NOT =  9999999
              AND   WK-TOKUTEI-COMPFTN  >  WK-MIYAGI-COMPFTN
                COMPUTE   WK-JYOSEIGAKU       =
                          WK-MIYAGI-COMPFTN   *  WK-KEISU
                COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                          WK-MIYAGI-COMPFTN   -  WK-JYOSEIGAKU
                COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                          WK-MIYAGI-COMPFTN   -  WK-JYOSEIGAKU
              ELSE
                COMPUTE   WK-JYOSEIGAKU       =
                          WK-TOKUTEI-COMPFTN  *  WK-KEISU
                COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                          WK-TOKUTEI-COMPFTN  -  WK-JYOSEIGAKU
                COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                          WK-TOKUTEI-COMPFTN  -  WK-JYOSEIGAKU
              END-IF
           ELSE
              MOVE     ZERO    TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
              MOVE     ZERO    TO   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
           END-IF
      *
           IF   HKN-RECEFTNDSPKBN   =   "2"
                MOVE   "1"   TO   WK-TAIHI-RECEFTNDSPKBN(KOH-IDX)
                IF    WK-TOKUTEI-COMPFTN   NOT =    ZERO
                    MOVE  WK-TOKUTEI-COMPFTN
                                     TO  LNK-SYU-SYUCOMPFTN
                END-IF
           ELSE
                COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX) =
                          LNK-SYU-KOH-COMPFTN(KOH-IDX) +  5
                COMPUTE   WK-FTN      =
                          LNK-SYU-KOH-COMPFTN(KOH-IDX) /  10
                COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX) =  WK-FTN
                                                       *  10
           END-IF
      *
           MOVE  WK-GAIYKZFTNKBN   TO   LNK-SYU-KOH-YKZFTNKBN(KOH-IDX)
      *
           .
      *
       23062-MIYAGI-KOHI-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ËÌ³¤Æ»ÃÏÊý¸øÈñ·×»»¡ÊÀº¿ÀÄÌ±¡Êä½õ¡Ë
      ******************************************************************
       23063-HOKAIDO-KOHI-SEC           SECTION.
      *
           IF      WK-TOKUTEI-COMPFTN   NOT =    ZERO
           OR      FLG-GENBUTU70        NOT =    ZERO
               IF    WK-SEISIN-COMPFTN  NOT =  9999999
               AND   WK-TOKUTEI-COMPFTN  >  WK-SEISIN-COMPFTN
                COMPUTE   WK-JYOSEIGAKU       =
                          WK-SEISIN-COMPFTN   *  0.5
                COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                          WK-SEISIN-COMPFTN   -  WK-JYOSEIGAKU
                COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                          WK-SEISIN-COMPFTN   -  WK-JYOSEIGAKU
               ELSE
                COMPUTE   WK-JYOSEIGAKU       =
                          WK-TOKUTEI-COMPFTN  *  0.5
                COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                          WK-TOKUTEI-COMPFTN  -  WK-JYOSEIGAKU
                COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                          WK-TOKUTEI-COMPFTN  -  WK-JYOSEIGAKU
               END-IF
           ELSE
             IF    WK-TOKUTEI-COMPFTN3  NOT =    ZERO
               IF    WK-SEISIN-COMPFTN  NOT =  9999999
               AND   WK-TOKUTEI-COMPFTN3 >  WK-SEISIN-COMPFTN
                COMPUTE   WK-JYOSEIGAKU       =
                          WK-SEISIN-COMPFTN   *  0.5
                COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                          WK-SEISIN-COMPFTN   -  WK-JYOSEIGAKU
                COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                          WK-SEISIN-COMPFTN   -  WK-JYOSEIGAKU
               ELSE
                COMPUTE   WK-JYOSEIGAKU       =
                          WK-TOKUTEI-COMPFTN3 *  0.5
                COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                          WK-TOKUTEI-COMPFTN3 -  WK-JYOSEIGAKU
                COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                          WK-TOKUTEI-COMPFTN3 -  WK-JYOSEIGAKU
               END-IF
             ELSE
               MOVE  ZERO  TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
               MOVE  ZERO  TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
             END-IF
           END-IF
      *
           IF   HKN-RECEFTNDSPKBN   =   "2"
                MOVE   "1"   TO   WK-TAIHI-RECEFTNDSPKBN(KOH-IDX)
                IF    WK-TOKUTEI-COMPFTN   NOT =    ZERO
                    MOVE  WK-TOKUTEI-COMPFTN
                                     TO  LNK-SYU-SYUCOMPFTN
                END-IF
                IF    LNK-SYU-KOH-HKNNUM(1)    =    "027"
                  IF  WK-TOKUTEI-COMPFTN3  NOT =    ZERO
                    MOVE  WK-TOKUTEI-COMPFTN3
                                     TO  LNK-SYU-KOH-COMPFTN(1)
                  END-IF
                END-IF
           ELSE
                COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX) =
                          LNK-SYU-KOH-COMPFTN(KOH-IDX) +  5
                COMPUTE   WK-FTN      =
                          LNK-SYU-KOH-COMPFTN(KOH-IDX) /  10
                COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX) =  WK-FTN
                                                       *  10
           END-IF
      *
           MOVE  WK-GAIYKZFTNKBN   TO   LNK-SYU-KOH-YKZFTNKBN(KOH-IDX)
      *
           .
      *
       23063-HOKAIDO-KOHI-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤«¤éºÇ¾®¤ÎÃÍ¤òÁªÂò¥»¥Ã¥È
      ******************************************************************
       2307-FTNKIN-HIKAKU-SEC         SECTION.
      *
           IF   COMB-KOH1HKNNUM        NOT   =   "027" 
           AND  COMB-HKNNUM            NOT   =   SPACE
               IF   LNK-SYU-KOH1HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH1COMPFTN-ENTANI  
                                 >   LNK-SYU-SYUCOMPFTN-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                TO  LNK-SYU-KOH1COMPFTN-ENTANI
               END-IF
               IF   LNK-SYU-KOH2HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH2COMPFTN-ENTANI
                                 >   LNK-SYU-SYUCOMPFTN-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                TO  LNK-SYU-KOH2COMPFTN-ENTANI
               END-IF
               IF   LNK-SYU-KOH3HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH3COMPFTN-ENTANI
                                 >   LNK-SYU-SYUCOMPFTN-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                TO  LNK-SYU-KOH3COMPFTN-ENTANI
               END-IF
               IF   LNK-SYU-KOH4HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH4COMPFTN-ENTANI
                                 >   LNK-SYU-SYUCOMPFTN-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                TO  LNK-SYU-KOH4COMPFTN-ENTANI
               END-IF
           END-IF.
      *
           IF   COMB-KOH1HKNNUM        NOT   =   "027" 
           AND  COMB-HKNNUM            NOT   =   SPACE
               IF   LNK-SYU-KOH1HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH1COMPFTN      >   LNK-SYU-SYUCOMPFTN
                   MOVE   LNK-SYU-SYUCOMPFTN TO  LNK-SYU-KOH1COMPFTN
                   COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
                             LNK-SYU-KOH1COMPFTN     +
                             LNK-SYU-KOH1COMPYKZFTN
                   IF    WK-KOH-COMPFTN(1)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO   WK-KOH-COMPFTN(1)
                   END-IF
               END-IF
               IF   LNK-SYU-KOH2HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH2COMPFTN      >   LNK-SYU-SYUCOMPFTN
                   MOVE   LNK-SYU-SYUCOMPFTN TO  LNK-SYU-KOH2COMPFTN
                   COMPUTE   LNK-SYU-KOH2COMPTOTAL   =
                             LNK-SYU-KOH2COMPFTN     +
                             LNK-SYU-KOH2COMPYKZFTN
                   IF    WK-KOH-COMPFTN(2)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO   WK-KOH-COMPFTN(2)
                   END-IF
               END-IF
               IF   LNK-SYU-KOH3HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH3COMPFTN      >   LNK-SYU-SYUCOMPFTN
                   MOVE   LNK-SYU-SYUCOMPFTN TO  LNK-SYU-KOH3COMPFTN
                   COMPUTE   LNK-SYU-KOH3COMPTOTAL   =
                             LNK-SYU-KOH3COMPFTN     +
                             LNK-SYU-KOH3COMPYKZFTN
                   IF    WK-KOH-COMPFTN(3)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO   WK-KOH-COMPFTN(3)
                   END-IF
               END-IF
               IF   LNK-SYU-KOH4HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH4COMPFTN      >   LNK-SYU-SYUCOMPFTN
                   MOVE   LNK-SYU-SYUCOMPFTN TO  LNK-SYU-KOH4COMPFTN
                   COMPUTE   LNK-SYU-KOH4COMPTOTAL   =
                             LNK-SYU-KOH4COMPFTN     +
                             LNK-SYU-KOH4COMPYKZFTN
                   IF    WK-KOH-COMPFTN(4)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO   WK-KOH-COMPFTN(4)
                   END-IF
               END-IF
           END-IF.
      *
           IF   (COMB-KOH1HKNNUM     = "972" OR "974") OR
                (COMB-KOH2HKNNUM     = "972" OR "974") OR
                (COMB-KOH3HKNNUM     = "972" OR "974") OR
                (COMB-KOH4HKNNUM     = "972" OR "974")
              IF  ((COMB-KOH1HKNNUM     = "972" OR "974" ) AND
                   (COMB-KOH2HKNNUM     =  SPACE         ) AND
                   (COMB-KOH3HKNNUM     =  SPACE         ) AND
                   (COMB-KOH4HKNNUM     =  SPACE         ))
              OR  ((COMB-KOH1HKNNUM     = "027"          ) AND
                   (COMB-KOH2HKNNUM     = "972" OR "974" ) AND
                   (COMB-KOH3HKNNUM     =  SPACE         ) AND
                   (COMB-KOH4HKNNUM     =  SPACE         ))
                 PERFORM   23072-FTNKIN-HIKAKU-TUJYO-SEC
              ELSE
                 PERFORM   23071-FTNKIN-HIKAKU-CYOKI-SEC
              END-IF
           ELSE
                 PERFORM   23072-FTNKIN-HIKAKU-TUJYO-SEC
           END-IF.
      *
           IF   ( LNK-SYU-SRYYMD(1:6)   >= "200604")
           AND  ((COMB-KOH1HKNNUM        =  "021" ) OR
                 (COMB-KOH2HKNNUM        =  "021" ))
           AND  ( FLG-SEISINJYO          =   1     )
              MOVE     ZERO     TO    WK-KNJFTNKIN
              MOVE     ZERO     TO    WK-KNJFTNKIN-ENTANI
           END-IF.
      *
      *    ¼ýÇ¼¥Æ¡¼¥Ö¥ë¤ÎÀÁµá³Û¤ò¥»¥Ã¥È
           MOVE    WK-KNJFTNKIN             TO  LNK-SYU-SKYMONEY.
      *
      *    ÂçÊ¬¸©ÆýÍÄ»ùÆÃ¼ì·×»»
           PERFORM   23074-OITA-TOKUSYU-KEISAN-SEC.
      *
      *    ¼ýÇ¼¥Æ¡¼¥Ö¥ë¤Î¸øÈñËè¤Î´µ¼ÔÉéÃ´¶â³Û¤ò¥»¥Ã¥È¤¹¤ë
           IF   (COMB-KOH1HKNNUM         NOT = SPACE)   AND
                (COMB-KOH1-GAIJGNKBN     =     "1")
             IF  ((LNK-S01-KOUREI-ZAITAKU-KBN   =       "1" ) OR
                  (FLG-KOUREI-KBN               =       "1" ))
             AND ( COMB-KOH1HKNNUM              =       "027")
                  CONTINUE
             ELSE
               IF  (( FLG-KOUGAKUCYO2    NOT =    ZERO  ) AND
                    ( FLG-IRYOHI2            =  1  OR 2 ))
               OR  (  FLG-KOUGAKUCYO-JIRITU
                                         NOT =    ZERO  )
               OR  (  FLG-KOUGAKUCYO-GEN70K
                                         NOT =    ZERO  )
               OR  (( FLG-KOUGAKUCYO-GEN70
                                         NOT =    ZERO  ) AND
                    ( FLG-IRYOHI70       =   3  OR 4    ) AND
                    ( FLG-SYOUKAN(1)         =    "1"   ))
                    MOVE     LNK-SYU-KOH1COMPFTN  TO
                                          LNK-SYU-KOH1HKNFTNMONEY
                    IF     LNK-SYU-KOH1YKZFTNKBN   =    "2"
                           COMPUTE  LNK-SYU-KOH1HKNFTNMONEY  =
                                    LNK-SYU-KOH1HKNFTNMONEY  +
                                    LNK-SYU-KOH1COMPYKZFTN
                    END-IF
               ELSE
                    COMPUTE  LNK-SYU-KOH1HKNFTNMONEY  =                  
                                    LNK-SYU-KOH1COMPFTN     +   5        
                    COMPUTE  LNK-SYU-KOH1HKNFTNMONEY  =                  
                                    LNK-SYU-KOH1HKNFTNMONEY /  10        
                    COMPUTE  LNK-SYU-KOH1HKNFTNMONEY  =                  
                                    LNK-SYU-KOH1HKNFTNMONEY *  10        
                    IF     LNK-SYU-KOH1YKZFTNKBN   =    "2"
                           COMPUTE  LNK-SYU-KOH1HKNFTNMONEY  =
                                    LNK-SYU-KOH1HKNFTNMONEY  +
                                    LNK-SYU-KOH1COMPYKZFTN
                    END-IF
               END-IF
             END-IF
           END-IF.
           IF   (COMB-KOH2HKNNUM        NOT = SPACE)   AND
                (COMB-KOH2-GAIJGNKBN    =     "1")
                    COMPUTE  LNK-SYU-KOH2HKNFTNMONEY  =                  
                                    LNK-SYU-KOH2COMPFTN     +   5        
                    COMPUTE  LNK-SYU-KOH2HKNFTNMONEY  =                  
                                    LNK-SYU-KOH2HKNFTNMONEY /  10        
                    COMPUTE  LNK-SYU-KOH2HKNFTNMONEY  =                  
                                    LNK-SYU-KOH2HKNFTNMONEY *  10        
                    IF     LNK-SYU-KOH2YKZFTNKBN   =    "2"
                           COMPUTE  LNK-SYU-KOH2HKNFTNMONEY  =
                                    LNK-SYU-KOH2HKNFTNMONEY  +
                                    LNK-SYU-KOH2COMPYKZFTN
                    END-IF
           END-IF.
           IF   (COMB-KOH3HKNNUM        NOT = SPACE)   AND
                (COMB-KOH3-GAIJGNKBN    =     "1")
                    COMPUTE  LNK-SYU-KOH3HKNFTNMONEY  =                  
                                    LNK-SYU-KOH3COMPFTN     +   5        
                    COMPUTE  LNK-SYU-KOH3HKNFTNMONEY  =                  
                                    LNK-SYU-KOH3HKNFTNMONEY /  10        
                    COMPUTE  LNK-SYU-KOH3HKNFTNMONEY  =                  
                                    LNK-SYU-KOH3HKNFTNMONEY *  10        
                    IF     LNK-SYU-KOH3YKZFTNKBN   =    "2"
                           COMPUTE  LNK-SYU-KOH3HKNFTNMONEY  =
                                    LNK-SYU-KOH3HKNFTNMONEY  +
                                    LNK-SYU-KOH3COMPYKZFTN
                    END-IF
           END-IF.
           IF   (COMB-KOH4HKNNUM        NOT = SPACE)   AND
                (COMB-KOH4-GAIJGNKBN    =     "1")
                    COMPUTE  LNK-SYU-KOH4HKNFTNMONEY  =                  
                                    LNK-SYU-KOH4COMPFTN     +   5        
                    COMPUTE  LNK-SYU-KOH4HKNFTNMONEY  =                  
                                    LNK-SYU-KOH4HKNFTNMONEY /  10        
                    COMPUTE  LNK-SYU-KOH4HKNFTNMONEY  =                  
                                    LNK-SYU-KOH4HKNFTNMONEY *  10        
                    IF     LNK-SYU-KOH4YKZFTNKBN   =    "2"
                           COMPUTE  LNK-SYU-KOH4HKNFTNMONEY  =
                                    LNK-SYU-KOH4HKNFTNMONEY  +
                                    LNK-SYU-KOH4COMPYKZFTN
                    END-IF
           END-IF.
      *
      *    ¥ì¥»µ­ºÜ¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢¤«¤é¼ýÇ¼¹àÌÜ¤ØÊÔ½¸
      *
           IF    COMB-KOH1HKNNUM         NOT = SPACE
             IF    WK-KOH-COMPFTN(1)  =  ALL"9"                           
                IF   WK-TAIHI-RECEFTNDSPKBN(1) = SPACE
                AND  COMB-KOH1HKNNUM       NOT = "027"
                  IF  (( FLG-KOUGAKUCYO2    NOT =    ZERO  ) AND
                       ( FLG-IRYOHI2            =  1  OR 2 ))
                  OR  (  FLG-KOUGAKUCYO-JIRITU
                                            NOT =    ZERO  )
                  OR  (  FLG-KOUGAKUCYO-GEN70K
                                            NOT =    ZERO  )
                  OR  (( FLG-KOUGAKUCYO-GEN70
                                            NOT =    ZERO  ) AND
                       ( FLG-IRYOHI70       =   3  OR 4    ) AND
                       ( FLG-SYOUKAN(1)         =    "1"   ))
                    CONTINUE
                  ELSE
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH1COMPTOTAL   =
                             LNK-SYU-KOH1COMPFTN     +
                             LNK-SYU-KOH1COMPYKZFTN
                  END-IF
                END-IF
             ELSE                                                         
                MOVE  WK-KOH-COMPFTN(1)  TO   LNK-SYU-KOH1COMPFTN       
                COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
                          LNK-SYU-KOH1COMPFTN     +
                          LNK-SYU-KOH1COMPYKZFTN
             END-IF
           END-IF.                                                      
      *
           IF    COMB-KOH2HKNNUM         NOT = SPACE
             IF    WK-KOH-COMPFTN(2)  =  ALL"9"                           
                IF   WK-TAIHI-RECEFTNDSPKBN(2) = SPACE
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH2COMPTOTAL   =
                             LNK-SYU-KOH2COMPFTN     +
                             LNK-SYU-KOH2COMPYKZFTN
                END-IF
             ELSE                                                         
                MOVE  WK-KOH-COMPFTN(2)  TO   LNK-SYU-KOH2COMPFTN       
                COMPUTE   LNK-SYU-KOH2COMPTOTAL   =
                          LNK-SYU-KOH2COMPFTN     +
                          LNK-SYU-KOH2COMPYKZFTN
             END-IF
           END-IF.                                                      
      *
           IF    COMB-KOH3HKNNUM         NOT = SPACE
             IF    WK-KOH-COMPFTN(3)  =  ALL"9"                           
                IF   WK-TAIHI-RECEFTNDSPKBN(3) = SPACE
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH3COMPTOTAL   =
                             LNK-SYU-KOH3COMPFTN     +
                             LNK-SYU-KOH3COMPYKZFTN
                END-IF
             ELSE                                                         
                MOVE  WK-KOH-COMPFTN(3)  TO   LNK-SYU-KOH3COMPFTN       
                COMPUTE   LNK-SYU-KOH3COMPTOTAL   =
                          LNK-SYU-KOH3COMPFTN     +
                          LNK-SYU-KOH3COMPYKZFTN
             END-IF
           END-IF.                                                      
      *
           IF    COMB-KOH4HKNNUM         NOT = SPACE
             IF    WK-KOH-COMPFTN(4)  =  ALL"9"                           
                IF   WK-TAIHI-RECEFTNDSPKBN(4) = SPACE
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH4COMPTOTAL   =
                             LNK-SYU-KOH4COMPFTN     +
                             LNK-SYU-KOH4COMPYKZFTN
                END-IF
             ELSE                                                         
                MOVE  WK-KOH-COMPFTN(4)  TO   LNK-SYU-KOH4COMPFTN       
                COMPUTE   LNK-SYU-KOH4COMPTOTAL   =
                          LNK-SYU-KOH4COMPFTN     +
                          LNK-SYU-KOH4COMPYKZFTN
             END-IF
           END-IF.                                                      
      *
      *    ÆÃ¼ì·×»»ÍÑ¶â³ÛÂàÈòÊÔ½¸
           PERFORM   23073-TOKUSYU-HENSYU-SEC.
      *
      *    ¼çÊÝ¸±¡¦Ï·¿ÍÊÝ¸±¤Î¶â³ÛÃÖ¤­´¹¤¨
           IF   COMB-HKNNUM            NOT =   SPACE
           AND  LNK-SYU-SYUCOMPFTN     NOT =    ZERO
           AND  FLG-KOUGAKUCYO-ININ    NOT =     1
           AND  COMB-SYU-HKNKOHSBTKBN  NOT =    "8"
           AND  FLG-KOUGAKUCYO-GEN70   NOT =     1
      *
                 COMPUTE   LNK-SYU-SYUCOMPFTN  =
                           LNK-SYU-SYUCOMPFTN  +  5
                 COMPUTE   WK-FTN      =
                           LNK-SYU-SYUCOMPFTN  / 10
                 COMPUTE   LNK-SYU-SYUCOMPFTN  =
                           WK-FTN   *  10
                 COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
                           LNK-SYU-SYUCOMPFTN     +
                           LNK-SYU-SYUCOMPYKZFTN
      *
           END-IF.
      *
           IF   COMB-KOH1HKNNUM            =   "027"
           AND  LNK-SYU-KOH1COMPFTN    NOT =    ZERO
           AND  FLG-KOUGAKUCYO-ININ    NOT =     1
      *
                 COMPUTE   LNK-SYU-KOH1COMPFTN  =
                           LNK-SYU-KOH1COMPFTN  +  5
                 COMPUTE   WK-FTN      =
                           LNK-SYU-KOH1COMPFTN  / 10
                 COMPUTE   LNK-SYU-KOH1COMPFTN  =
                           WK-FTN   *  10
                 COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
                           LNK-SYU-KOH1COMPFTN     +
                           LNK-SYU-KOH1COMPYKZFTN
      *
           END-IF.
      *
      *    £´£±Ï·¿ÍÍÑ
           IF   FLG-KOUGAKUCYO2        NOT =    ZERO
              IF   (FLG-IRYOHI2 = 1 OR 2)  AND (FLG-HIKAKU = 1)
                  CONTINUE
              ELSE
                  MOVE     ZERO      TO    FLG-KOUGAKUCYO2
              END-IF
           END-IF.
           IF   FLG-KOUGAKUCYO-JIRITU  NOT =    ZERO
              IF   FLG-HIKAKU     =     1
                  CONTINUE
              ELSE
                  MOVE     ZERO      TO    FLG-KOUGAKUCYO-JIRITU
              END-IF
           END-IF.
      *
           IF   FLG-KOUGAKUCYO-GEN70    =    1
              EVALUATE  FLG-IRYOHI70
                WHEN      1
                WHEN      2
                    CONTINUE
                WHEN      3
                WHEN      4
                  IF  (FLG-SYOUKAN(1) =    "1")
                  AND (FLG-HIKAKU     =     1 )
                    CONTINUE
                  ELSE
                    MOVE    ZERO   TO    FLG-KOUGAKUCYO-GEN70
                  END-IF
                WHEN      6
                    MOVE    ZERO   TO    FLG-KOUGAKUCYO-GEN70
              END-EVALUATE
           END-IF.
           IF   FLG-KOUGAKUCYO-GEN70K   =    1
               IF   FLG-HIKAKU     =     1
                    CONTINUE
               ELSE
                    MOVE    ZERO   TO    FLG-KOUGAKUCYO-GEN70K
               END-IF
           END-IF.
      *
      ***  IF   FLG-SYUHEN                 =   "1"
      ***  AND  LNK-SYU-SRYYMD(1:6)        <   "200704"
      *
      ***     IF   COMB-HKNNUM            NOT =   SPACE
      ***     AND  LNK-SYU-SYUCOMPFTN     NOT =   WK-HEN-SYUCOMPFTN
      *
      ***        IF   COMB-KOH1-GAIJGNKBN     =     "1"
      ***           IF   LNK-SYU-KOH1HKNFTNMONEY  >  WK-HEN-SYUCOMPFTN
      ***              MOVE  LNK-SYU-KOH1HKNFTNMONEY
      ***                                        TO  LNK-SYU-SYUCOMPFTN
      ***              COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
      ***                        LNK-SYU-SYUCOMPFTN     +
      ***                        LNK-SYU-SYUCOMPYKZFTN
      ***           ELSE
      ***              MOVE  WK-HEN-SYUCOMPFTN   TO  LNK-SYU-SYUCOMPFTN
      ***              COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
      ***                        LNK-SYU-SYUCOMPFTN     +
      ***                        LNK-SYU-SYUCOMPYKZFTN
      ***           END-IF
      ***        ELSE
      ***           IF   LNK-SYU-KOH1COMPFTN  >  WK-HEN-SYUCOMPFTN
      ***              COMPUTE   LNK-SYU-SYUCOMPFTN   =
      ***                        LNK-SYU-KOH1COMPFTN  +   5
      ***              COMPUTE   WK-FTN               =
      ***                        LNK-SYU-SYUCOMPFTN   /  10
      ***              COMPUTE   LNK-SYU-SYUCOMPFTN   =
      ***                        WK-FTN               *  10
      ***              COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
      ***                        LNK-SYU-SYUCOMPFTN     +
      ***                        LNK-SYU-SYUCOMPYKZFTN
      ***           ELSE
      ***              MOVE  WK-HEN-SYUCOMPFTN   TO  LNK-SYU-SYUCOMPFTN
      ***              COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
      ***                        LNK-SYU-SYUCOMPFTN     +
      ***                        LNK-SYU-SYUCOMPYKZFTN
      ***           END-IF
      ***        END-IF
      *
      ***     END-IF
      *
      ***     IF   COMB-HKNNUM       NOT =   SPACE
      ***     AND  LNK-SYU-SYUCOMPFTN-ENTANI
      ***                            NOT =   WK-HEN-SYUCOMPFTN-ENTANI
      *
      ***        IF   LNK-SYU-KOH1COMPFTN-ENTANI  >
      ***                                 WK-HEN-SYUCOMPFTN-ENTANI
      ***           MOVE   LNK-SYU-KOH1COMPFTN-ENTANI
      ***                       TO   LNK-SYU-SYUCOMPFTN-ENTANI
      ***        ELSE
      ***           MOVE   WK-HEN-SYUCOMPFTN-ENTANI
      ***                       TO   LNK-SYU-SYUCOMPFTN-ENTANI
      ***        END-IF
      *
      ***     END-IF
      *
      ***     IF   COMB-KOH1HKNNUM            =   "027"
      ***     AND  LNK-SYU-KOH1COMPFTN    NOT =   WK-HEN-KOH1COMPFTN
      *
      ***        IF   COMB-KOH2-GAIJGNKBN     =     "1"
      ***           IF   LNK-SYU-KOH2HKNFTNMONEY  >  WK-HEN-KOH1COMPFTN
      ***              MOVE  LNK-SYU-KOH2HKNFTNMONEY
      ***                                        TO  LNK-SYU-KOH1COMPFTN
      ***              COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
      ***                        LNK-SYU-KOH1COMPFTN     +
      ***                        LNK-SYU-KOH1COMPYKZFTN
      ***           ELSE
      ***              MOVE  WK-HEN-KOH1COMPFTN  TO  LNK-SYU-KOH1COMPFTN
      ***              COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
      ***                        LNK-SYU-KOH1COMPFTN     +
      ***                        LNK-SYU-KOH1COMPYKZFTN
      ***           END-IF
      ***        ELSE
      ***           IF   LNK-SYU-KOH2COMPFTN  >  WK-HEN-KOH1COMPFTN
      ***              COMPUTE   LNK-SYU-KOH1COMPFTN  =
      ***                        LNK-SYU-KOH2COMPFTN  +   5
      ***              COMPUTE   WK-FTN               =
      ***                        LNK-SYU-KOH1COMPFTN  /  10
      ***              COMPUTE   LNK-SYU-KOH1COMPFTN  =
      ***                        WK-FTN               *  10
      ***              COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
      ***                        LNK-SYU-KOH1COMPFTN     +
      ***                        LNK-SYU-KOH1COMPYKZFTN
      ***           ELSE
      ***              MOVE  WK-HEN-KOH1COMPFTN  TO  LNK-SYU-KOH1COMPFTN
      ***              COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
      ***                        LNK-SYU-KOH1COMPFTN     +
      ***                        LNK-SYU-KOH1COMPYKZFTN
      ***           END-IF
      ***        END-IF
      *
      ***     END-IF
      *
      ***     IF   COMB-KOH1HKNNUM            =   "027"
      ***     AND  LNK-SYU-KOH1COMPFTN-ENTANI
      ***                           NOT =   WK-HEN-KOH1COMPFTN-ENTANI
      *
      ***        IF   LNK-SYU-KOH2COMPFTN-ENTANI  >
      ***                                 WK-HEN-KOH1COMPFTN-ENTANI
      ***           MOVE   LNK-SYU-KOH2COMPFTN-ENTANI
      ***                       TO   LNK-SYU-KOH1COMPFTN-ENTANI
      ***        ELSE
      ***           MOVE   WK-HEN-KOH1COMPFTN-ENTANI
      ***                       TO   LNK-SYU-KOH1COMPFTN-ENTANI
      ***        END-IF
      *
      ***     END-IF
      *
      ***  END-IF.
      *
       2307-FTNKIN-HIKAKU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤«¤éºÇ¾®¤ÎÃÍ¤òÁªÂò¥»¥Ã¥È
      ******************************************************************
       23071-FTNKIN-HIKAKU-CYOKI-SEC         SECTION.
      *
      *    ÀÁµá³ÛÈæ³Ó
           MOVE    ZERO               TO    WK-KNJFTNKIN-ENTANI
      *
      *    ¸øÈñ£±¤Î¤ß
      ***  IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
      ***     (COMB-KOH2HKNNUM              =     SPACE)   AND
      ***     (COMB-KOH3HKNNUM              =     SPACE)   AND
      ***     (COMB-KOH4HKNNUM              =     SPACE)
      ***      MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
      ***                             TO    WK-KNJFTNKIN-ENTANI
      ***  END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              =     SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE    LNK-SYU-KOH1COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
           END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE    LNK-SYU-KOH1COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               IF  LNK-SYU-KOH2COMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-KOH2COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
           END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³¤È¸øÈñ£´
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              NOT = SPACE)
               MOVE    LNK-SYU-KOH1COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               IF  LNK-SYU-KOH2COMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-KOH2COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
               IF  LNK-SYU-KOH3COMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-KOH3COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
           END-IF
      *
           IF   COMB-KOH1HKNNUM        NOT   =   "027" 
           AND  COMB-HKNNUM            NOT   =   SPACE
               IF  LNK-SYU-SYUCOMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
           END-IF
      *
           IF    WK-CYOKI-ZAN-ENTANI  <  WK-KNJFTNKIN-ENTANI
               MOVE   WK-CYOKI-ZAN-ENTANI   TO   WK-KNJFTNKIN-ENTANI
           END-IF
      *
      ****************************
      *
           MOVE    ZERO               TO    FLG-HIKAKU
      *    ÀÁµá³ÛÈæ³Ó
           MOVE    ZERO               TO    WK-KNJFTNKIN
      *
      *    ¸øÈñ£±¤Î¤ß
      ***  IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
      ***     (COMB-KOH2HKNNUM              =     SPACE)   AND
      ***     (COMB-KOH3HKNNUM              =     SPACE)   AND
      ***     (COMB-KOH4HKNNUM              =     SPACE)
      ***      MOVE   LNK-SYU-SYUCOMPFTN    TO    WK-KNJFTNKIN
      ***      MOVE   WK-KOH1-YAKFTN        TO    LNK-SYU-YKZ-FTN
      ***  END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              =     SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE    1                    TO    FLG-HIKAKU
               MOVE    LNK-SYU-KOH1COMPFTN  TO    WK-KNJFTNKIN
               MOVE    WK-KOH1-YAKFTN       TO    LNK-SYU-YKZ-FTN
               IF  WK-KOH2-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH2-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
           END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE    1                    TO    FLG-HIKAKU
               MOVE    LNK-SYU-KOH1COMPFTN  TO    WK-KNJFTNKIN
               IF  LNK-SYU-KOH2COMPFTN      <=    WK-KNJFTNKIN
                   MOVE   2                       TO  FLG-HIKAKU
                   MOVE   LNK-SYU-KOH2COMPFTN     TO  WK-KNJFTNKIN
               END-IF
               MOVE    WK-KOH1-YAKFTN       TO    LNK-SYU-YKZ-FTN
               IF  WK-KOH2-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH2-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
               IF  WK-KOH3-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH3-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
           END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³¤È¸øÈñ£´
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              NOT = SPACE)
               MOVE    1                    TO    FLG-HIKAKU
               MOVE    LNK-SYU-KOH1COMPFTN  TO    WK-KNJFTNKIN
               IF  LNK-SYU-KOH2COMPFTN      <=    WK-KNJFTNKIN
                   MOVE   2                   TO  FLG-HIKAKU
                   MOVE   LNK-SYU-KOH2COMPFTN TO  WK-KNJFTNKIN
               END-IF
               IF  LNK-SYU-KOH3COMPFTN      <=    WK-KNJFTNKIN
                   MOVE   3                   TO  FLG-HIKAKU
                   MOVE   LNK-SYU-KOH3COMPFTN TO  WK-KNJFTNKIN
               END-IF
               MOVE    WK-KOH1-YAKFTN       TO    LNK-SYU-YKZ-FTN
               IF  WK-KOH2-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH2-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
               IF  WK-KOH3-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH3-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
               IF  WK-KOH4-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH4-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
           END-IF
      *
           IF   COMB-KOH1HKNNUM        NOT   =   "027" 
           AND  COMB-HKNNUM            NOT   =   SPACE
               IF  LNK-SYU-SYUCOMPFTN       <     WK-KNJFTNKIN
                   MOVE   ZERO                  TO   FLG-HIKAKU
                   MOVE   LNK-SYU-SYUCOMPFTN    TO   WK-KNJFTNKIN
               END-IF
               IF  LNK-SYU-SYUCOMPYKZFTN    <     LNK-SYU-YKZ-FTN
                   MOVE   LNK-SYU-SYUCOMPYKZFTN TO   LNK-SYU-YKZ-FTN
               END-IF
           END-IF
      *
           IF    WK-CYOKI-ZAN  <  WK-KNJFTNKIN
               MOVE     ZERO            TO     FLG-HIKAKU
               MOVE     WK-CYOKI-ZAN    TO     WK-KNJFTNKIN
           END-IF
      *
           .
      *
       23071-FTNKIN-HIKAKU-CYOKI-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤«¤éºÇ¾®¤ÎÃÍ¤òÁªÂò¥»¥Ã¥È
      ******************************************************************
       23072-FTNKIN-HIKAKU-TUJYO-SEC         SECTION.
      *
      *    ÀÁµá³ÛÈæ³Ó
           MOVE    ZERO               TO    WK-KNJFTNKIN-ENTANI
      *
      *    ¼çÊÝ¸±¤Î¤ß
           IF (COMB-KOH1HKNNUM              =    SPACE)    AND
              (COMB-KOH2HKNNUM              =    SPACE)    AND
              (COMB-KOH3HKNNUM              =    SPACE)    AND
              (COMB-KOH4HKNNUM              =    SPACE)
               MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                      TO   WK-KNJFTNKIN-ENTANI
           END-IF
      *    ¸øÈñ£±¤Î¤ß
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              =     SPACE)   AND
              (COMB-KOH3HKNNUM              =     SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE   LNK-SYU-KOH1COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
           END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              =     SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE    LNK-SYU-KOH1COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               IF  LNK-SYU-KOH2COMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-KOH2COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
           END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE    LNK-SYU-KOH1COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               IF  LNK-SYU-KOH2COMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-KOH2COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
               IF  LNK-SYU-KOH3COMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-KOH3COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
           END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³¤È¸øÈñ£´
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              NOT = SPACE)
               MOVE    LNK-SYU-KOH1COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               IF  LNK-SYU-KOH2COMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-KOH2COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
               IF  LNK-SYU-KOH3COMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-KOH3COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
               IF  LNK-SYU-KOH4COMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-KOH4COMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
           END-IF
      *
           IF   COMB-KOH1HKNNUM        NOT   =   "027" 
           AND  COMB-HKNNUM            NOT   =   SPACE
               IF  LNK-SYU-SYUCOMPFTN-ENTANI
                                      <     WK-KNJFTNKIN-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                      TO    WK-KNJFTNKIN-ENTANI
               END-IF
           END-IF
      *
      ****************************
      *
           MOVE    ZERO               TO    FLG-HIKAKU
      *    ÀÁµá³ÛÈæ³Ó
           MOVE    ZERO               TO    WK-KNJFTNKIN
      *    ¼çÊÝ¸±¤Î¤ß
           IF (COMB-KOH1HKNNUM              =    SPACE)    AND
              (COMB-KOH2HKNNUM              =    SPACE)    AND
              (COMB-KOH3HKNNUM              =    SPACE)    AND
              (COMB-KOH4HKNNUM              =    SPACE)
               MOVE   ZERO                  TO   FLG-HIKAKU
               MOVE   LNK-SYU-SYUCOMPFTN    TO   WK-KNJFTNKIN
               MOVE   LNK-SYU-SYUCOMPYKZFTN TO   LNK-SYU-YKZ-FTN
           END-IF
      *    ¸øÈñ£±¤Î¤ß
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              =     SPACE)   AND
              (COMB-KOH3HKNNUM              =     SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE   1                     TO    FLG-HIKAKU
               MOVE   LNK-SYU-KOH1COMPFTN   TO    WK-KNJFTNKIN
               MOVE   WK-KOH1-YAKFTN        TO    LNK-SYU-YKZ-FTN
           END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              =     SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE    1                    TO    FLG-HIKAKU
               MOVE    LNK-SYU-KOH1COMPFTN  TO    WK-KNJFTNKIN
               IF  LNK-SYU-KOH2COMPFTN      <=    WK-KNJFTNKIN
                   MOVE   2                   TO  FLG-HIKAKU
                   MOVE   LNK-SYU-KOH2COMPFTN TO  WK-KNJFTNKIN
               END-IF
               MOVE    WK-KOH1-YAKFTN       TO    LNK-SYU-YKZ-FTN
               IF  WK-KOH2-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH2-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
           END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE    1                    TO    FLG-HIKAKU
               MOVE    LNK-SYU-KOH1COMPFTN  TO    WK-KNJFTNKIN
               IF  LNK-SYU-KOH2COMPFTN      <=    WK-KNJFTNKIN
                   MOVE   2                       TO  FLG-HIKAKU
                   MOVE   LNK-SYU-KOH2COMPFTN     TO  WK-KNJFTNKIN
               END-IF
               IF  LNK-SYU-KOH3COMPFTN      <=    WK-KNJFTNKIN
                   MOVE   3                   TO  FLG-HIKAKU
                   MOVE   LNK-SYU-KOH3COMPFTN TO  WK-KNJFTNKIN
               END-IF
               MOVE    WK-KOH1-YAKFTN       TO    LNK-SYU-YKZ-FTN
               IF  WK-KOH2-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH2-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
               IF  WK-KOH3-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH3-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
           END-IF
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³¤È¸øÈñ£´
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              NOT = SPACE)
               MOVE    1                    TO    FLG-HIKAKU
               MOVE    LNK-SYU-KOH1COMPFTN  TO    WK-KNJFTNKIN
               IF  LNK-SYU-KOH2COMPFTN      <=    WK-KNJFTNKIN
                   MOVE   2                   TO  FLG-HIKAKU
                   MOVE   LNK-SYU-KOH2COMPFTN TO  WK-KNJFTNKIN
               END-IF
               IF  LNK-SYU-KOH3COMPFTN      <=    WK-KNJFTNKIN
                   MOVE   3                   TO  FLG-HIKAKU
                   MOVE   LNK-SYU-KOH3COMPFTN TO  WK-KNJFTNKIN
               END-IF
               IF  LNK-SYU-KOH4COMPFTN      <=    WK-KNJFTNKIN
                   MOVE   4                   TO  FLG-HIKAKU
                   MOVE   LNK-SYU-KOH4COMPFTN TO  WK-KNJFTNKIN
               END-IF
               MOVE    WK-KOH1-YAKFTN       TO    LNK-SYU-YKZ-FTN
               IF  WK-KOH2-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH2-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
               IF  WK-KOH3-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH3-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
               IF  WK-KOH4-YAKFTN           <     LNK-SYU-YKZ-FTN
                   MOVE   WK-KOH4-YAKFTN    TO    LNK-SYU-YKZ-FTN
               END-IF
           END-IF
      *
           IF   COMB-KOH1HKNNUM        NOT   =   "027" 
           AND  COMB-HKNNUM            NOT   =   SPACE
               IF  LNK-SYU-SYUCOMPFTN       <     WK-KNJFTNKIN
                   MOVE   ZERO                  TO   FLG-HIKAKU
                   MOVE   LNK-SYU-SYUCOMPFTN    TO   WK-KNJFTNKIN
               END-IF
               IF  LNK-SYU-SYUCOMPYKZFTN    <     LNK-SYU-YKZ-FTN
                   MOVE   LNK-SYU-SYUCOMPYKZFTN TO   LNK-SYU-YKZ-FTN
               END-IF
           END-IF
      *
           .
      *
       23072-FTNKIN-HIKAKU-TUJYO-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÆÃ¼ì·×»»ÍÑ¶â³ÛÂàÈòÊÔ½¸
      ******************************************************************
       23073-TOKUSYU-HENSYU-SEC         SECTION.
      *
           IF  ((WK-1001-PREFNUM             =  07      ) AND
                (LNK-SYU-SRYYMD(1:6)        >= "200412" ) AND
                ((LNK-SYU-KOH1HKNNUM         = "245"   ) OR
                 (LNK-SYU-KOH2HKNNUM         = "245"   ) OR
                 (LNK-SYU-KOH3HKNNUM         = "245"   ) OR
                 (LNK-SYU-KOH4HKNNUM         = "245"   )))
           OR  ((WK-1001-PREFNUM             =  07      ) AND
                (LNK-SYU-SRYYMD(1:6)        >= "200604" ) AND
                ((LNK-SYU-KOH1HKNNUM         = "246"   ) OR
                 (LNK-SYU-KOH2HKNNUM         = "246"   ) OR
                 (LNK-SYU-KOH3HKNNUM         = "246"   ) OR
                 (LNK-SYU-KOH4HKNNUM         = "246"   )))
      *
      ***
                    IF   LNK-SYU-KOH1HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(1) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(1)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(1) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH2HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(2) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(2)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(2) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(2)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH3HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(3) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(3)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(3) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(3)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH4HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(4) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(4)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(4) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(4)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
      ***
      *
                    IF  ( FLG-KOUGAKUCYO-GEN70  NOT =    ZERO     )
                    AND ( FLG-IRYOHI70          =   3  OR  4      )
                    AND ( LNK-SYU-KOH1HKNNUM    =  "245" OR "246" )
                      MOVE     WK-TOKUSYU-COMPFTN   TO
                                         LNK-SYU-JYO-HKNFTNMONEY
                    ELSE
                      COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    WK-TOKUSYU-COMPFTN        +    5
                      COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    LNK-SYU-JYO-HKNFTNMONEY   /   10
                      COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    LNK-SYU-JYO-HKNFTNMONEY   *   10
                    END-IF
      *
                    MOVE  WK-TOKUSYU-COMPFTN   TO   LNK-SYU-JYO-COMPFTN
                    IF    WK-TOKUSYU-COMPFTN2  =  9999999
                       IF ((LNK-SYU-KOH1HKNNUM    = "245" OR "246") AND
                           (WK-TAIHI-RECEFTNDSPKBN(1) = SPACE     ))
                       OR ((LNK-SYU-KOH2HKNNUM    = "245" OR "246") AND
                           (WK-TAIHI-RECEFTNDSPKBN(2) = SPACE     ))
                       OR ((LNK-SYU-KOH3HKNNUM    = "245" OR "246") AND
                           (WK-TAIHI-RECEFTNDSPKBN(3) = SPACE     ))
                       OR ((LNK-SYU-KOH4HKNNUM    = "245" OR "246") AND
                           (WK-TAIHI-RECEFTNDSPKBN(4) = SPACE     ))
                         IF  ( FLG-KOUGAKUCYO-GEN70  NOT =    ZERO     )
                         AND ( FLG-IRYOHI70          =   3  OR  4      )
                         AND ( LNK-SYU-KOH1HKNNUM    =  "245" OR "246" )
                           CONTINUE
                         ELSE
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       +    5
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       /   10
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       *   10
                         END-IF
                       END-IF
                    ELSE
                       MOVE  WK-TOKUSYU-COMPFTN2  TO
                                        LNK-SYU-JYO-COMPFTN
                    END-IF
      *
                    MOVE     WK-TOKUSYU-ENTANI    TO
                                        LNK-SYU-JYO-COMPFTN-ENTANI
      *
           END-IF
      *
           IF  (WK-1001-PREFNUM             =  33      )
           AND (LNK-SYU-SRYYMD(1:6)        >= "200110" )
           AND (LNK-SYU-SRYYMD(1:6)        <= "200609" )
           AND ((LNK-SYU-KOH1HKNNUM         = "285"   ) OR
                (LNK-SYU-KOH2HKNNUM         = "285"   ) OR
                (LNK-SYU-KOH3HKNNUM         = "285"   ) OR
                (LNK-SYU-KOH4HKNNUM         = "285"   ))
      *
      ***
                    IF   LNK-SYU-KOH1HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(1) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(1)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(1) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH2HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(2) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(2)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(2) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(2)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH3HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(3) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(3)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(3) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(3)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH4HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(4) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(4)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(4) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(4)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
      ***
      *
                    COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    WK-TOKUSYU-COMPFTN        +    5
                    COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    LNK-SYU-JYO-HKNFTNMONEY   /   10
                    COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    LNK-SYU-JYO-HKNFTNMONEY   *   10
      *
                    MOVE  WK-TOKUSYU-COMPFTN   TO   LNK-SYU-JYO-COMPFTN
                    IF    WK-TOKUSYU-COMPFTN2  =  9999999
                       IF ((LNK-SYU-KOH1HKNNUM        = "285") AND
                           (WK-TAIHI-RECEFTNDSPKBN(1) = SPACE))
                       OR ((LNK-SYU-KOH2HKNNUM        = "285") AND
                           (WK-TAIHI-RECEFTNDSPKBN(2) = SPACE))
                       OR ((LNK-SYU-KOH3HKNNUM        = "285") AND
                           (WK-TAIHI-RECEFTNDSPKBN(3) = SPACE))
                       OR ((LNK-SYU-KOH4HKNNUM        = "285") AND
                           (WK-TAIHI-RECEFTNDSPKBN(4) = SPACE))
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       +    5
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       /   10
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       *   10
                       END-IF
                    ELSE
                       MOVE  WK-TOKUSYU-COMPFTN2  TO
                                        LNK-SYU-JYO-COMPFTN
                    END-IF
      *
                    MOVE     WK-TOKUSYU-ENTANI    TO
                                        LNK-SYU-JYO-COMPFTN-ENTANI
      *
           END-IF
      *
           IF  ((WK-1001-PREFNUM             =  11      )  AND
                (LNK-SYU-SRYYMD(1:6)        >= "200507" )  AND
                ((LNK-SYU-KOH1HKNNUM   = "343" OR "342") OR
                 (LNK-SYU-KOH2HKNNUM   = "343" OR "342") OR
                 (LNK-SYU-KOH3HKNNUM   = "343" OR "342") OR
                 (LNK-SYU-KOH4HKNNUM   = "343" OR "342")))
           OR  ((WK-1001-PREFNUM             =  11      )  AND
                (LNK-SYU-SRYYMD(1:6)        >= "200507" )  AND
                (FLG-SANSHOU2          =  "2"  OR "3"   )  AND
                ((LNK-SYU-KOH1HKNNUM   = "344"         ) OR
                 (LNK-SYU-KOH2HKNNUM   = "344"         ) OR
                 (LNK-SYU-KOH3HKNNUM   = "344"         ) OR
                 (LNK-SYU-KOH4HKNNUM   = "344"         )))
           OR  ((WK-1001-PREFNUM             =  11      )  AND
                (LNK-SYU-SRYYMD(1:6)        >= "200704" )  AND
                ((LNK-SYU-KOH1HKNNUM   = "181" OR "182"
                                      OR "183" OR "281") OR
                 (LNK-SYU-KOH2HKNNUM   = "181" OR "182"
                                      OR "183" OR "281") OR
                 (LNK-SYU-KOH3HKNNUM   = "181" OR "182"
                                      OR "183" OR "281") OR
                 (LNK-SYU-KOH4HKNNUM   = "181" OR "182"
                                      OR "183" OR "281")))
      *
      ***
                    IF   LNK-SYU-KOH1HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(1) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(1)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(1) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH2HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(2) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(2)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(2) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(2)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH3HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(3) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(3)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(3) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(3)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH4HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(4) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(4)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(4) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(4)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
      ***
      *
                    IF  ( FLG-KOUGAKUCYO-GEN70  NOT =    ZERO     )
                    AND ( FLG-IRYOHI70          =   3  OR  4      )
                    AND ( LNK-SYU-KOH1HKNNUM    =  "342" OR "343"
                                                OR "344" OR "181"
                                                OR "182" OR "183"
                                                OR "281"          )
                      MOVE     WK-TOKUSYU-COMPFTN   TO
                                         LNK-SYU-JYO-HKNFTNMONEY
                    ELSE
                      COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    WK-TOKUSYU-COMPFTN        +    5
                      COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    LNK-SYU-JYO-HKNFTNMONEY   /   10
                      COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    LNK-SYU-JYO-HKNFTNMONEY   *   10
                    END-IF
      *
                    MOVE  WK-TOKUSYU-COMPFTN   TO   LNK-SYU-JYO-COMPFTN
                    IF    WK-TOKUSYU-COMPFTN2  =  9999999
                       IF ((LNK-SYU-KOH1HKNNUM   = "343" OR "342"
                                                OR "344" OR "181"
                                                OR "182" OR "183"
                                                OR "281"         ) AND
                           (WK-TAIHI-RECEFTNDSPKBN(1) = SPACE    ))
                       OR ((LNK-SYU-KOH2HKNNUM   = "343" OR "342"
                                                OR "344" OR "181"
                                                OR "182" OR "183"
                                                OR "281"         ) AND
                           (WK-TAIHI-RECEFTNDSPKBN(2) = SPACE    ))
                       OR ((LNK-SYU-KOH3HKNNUM   = "343" OR "342"
                                                OR "344" OR "181"
                                                OR "182" OR "183"
                                                OR "281"         ) AND
                           (WK-TAIHI-RECEFTNDSPKBN(3) = SPACE    ))
                       OR ((LNK-SYU-KOH4HKNNUM   = "343" OR "342"
                                                OR "344" OR "181"
                                                OR "182" OR "183"
                                                OR "281"         ) AND
                           (WK-TAIHI-RECEFTNDSPKBN(4) = SPACE    ))
                         IF  ( FLG-KOUGAKUCYO-GEN70  NOT =    ZERO     )
                         AND ( FLG-IRYOHI70          =   3  OR  4      )
                         AND ( LNK-SYU-KOH1HKNNUM    =  "342" OR "343"
                                                     OR "344" OR "181"
                                                     OR "182" OR "183"
                                                     OR "281"          )
                           CONTINUE
                         ELSE
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       +    5
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       /   10
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       *   10
                         END-IF
                       END-IF
                    ELSE
                       MOVE  WK-TOKUSYU-COMPFTN2  TO
                                        LNK-SYU-JYO-COMPFTN
                    END-IF
      *
                    MOVE     WK-TOKUSYU-ENTANI    TO
                                        LNK-SYU-JYO-COMPFTN-ENTANI
      *
           END-IF
      *
           IF  (WK-1001-PREFNUM             =  13      )
           AND (LNK-SYU-SRYYMD(1:6)        >= "200610" )
           AND ((LNK-SYU-KOH1HKNNUM         = "182"   ) OR
                (LNK-SYU-KOH2HKNNUM         = "182"   ) OR
                (LNK-SYU-KOH3HKNNUM         = "182"   ) OR
                (LNK-SYU-KOH4HKNNUM         = "182"   ))
      *
      ***
                    IF   LNK-SYU-KOH1HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(1) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(1)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(1) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH2HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(2) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(2)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(2) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(2)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH3HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(3) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(3)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(3) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(3)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
                    IF   LNK-SYU-KOH4HKNNUM   = "972" OR "974"
                       IF  LNK-SYU-KOH-COMPFTN(4) < WK-TOKUSYU-COMPFTN
                          MOVE  LNK-SYU-KOH-COMPFTN(4)
                                            TO  WK-TOKUSYU-COMPFTN
                          MOVE  9999999     TO  WK-TOKUSYU-COMPFTN2
                       END-IF
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(4) 
                                                  < WK-TOKUSYU-ENTANI
                          MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(4)
                                            TO  WK-TOKUSYU-ENTANI
                       END-IF
                    END-IF
      ***
      *
                    IF  ( FLG-KOUGAKUCYO-GEN70  NOT =    ZERO     )
                    AND ( FLG-IRYOHI70          =   3  OR  4      )
                    AND ( LNK-SYU-KOH1HKNNUM    =  "182"          )
                      MOVE     WK-TOKUSYU-COMPFTN   TO
                                         LNK-SYU-JYO-HKNFTNMONEY
                    ELSE
                      COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    WK-TOKUSYU-COMPFTN        +    5
                      COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    LNK-SYU-JYO-HKNFTNMONEY   /   10
                      COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                                    LNK-SYU-JYO-HKNFTNMONEY   *   10
                    END-IF
      *
                    MOVE  WK-TOKUSYU-COMPFTN   TO   LNK-SYU-JYO-COMPFTN
                    IF    WK-TOKUSYU-COMPFTN2  =  9999999
                       IF ((LNK-SYU-KOH1HKNNUM        = "182") AND
                           (WK-TAIHI-RECEFTNDSPKBN(1) = SPACE))
                       OR ((LNK-SYU-KOH2HKNNUM        = "182") AND
                           (WK-TAIHI-RECEFTNDSPKBN(2) = SPACE))
                       OR ((LNK-SYU-KOH3HKNNUM        = "182") AND
                           (WK-TAIHI-RECEFTNDSPKBN(3) = SPACE))
                       OR ((LNK-SYU-KOH4HKNNUM        = "182") AND
                           (WK-TAIHI-RECEFTNDSPKBN(4) = SPACE))
                         IF  ( FLG-KOUGAKUCYO-GEN70  NOT =    ZERO     )
                         AND ( FLG-IRYOHI70          =   3  OR  4      )
                         AND ( LNK-SYU-KOH1HKNNUM    =  "182"          )
                           CONTINUE
                         ELSE
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       +    5
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       /   10
                           COMPUTE  LNK-SYU-JYO-COMPFTN  =
                                    LNK-SYU-JYO-COMPFTN       *   10
                         END-IF
                       END-IF
                    ELSE
                       MOVE  WK-TOKUSYU-COMPFTN2  TO
                                        LNK-SYU-JYO-COMPFTN
                    END-IF
      *
                    MOVE     WK-TOKUSYU-ENTANI    TO
                                        LNK-SYU-JYO-COMPFTN-ENTANI
      *
           END-IF
      *
           .
      *
       23073-TOKUSYU-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÂçÊ¬¸©ÆýÍÄ»ùÆÃ¼ì·×»»
      ******************************************************************
       23074-OITA-TOKUSYU-KEISAN-SEC         SECTION.
      *
           IF  (WK-1001-PREFNUM             =  44      )
           AND (LNK-SYU-SRYYMD(1:6)        >= "200610" )
           AND ((LNK-SYU-KOH1HKNNUM   = "183" OR "283" ) OR
                (LNK-SYU-KOH2HKNNUM   = "183" OR "283" ) OR
                (LNK-SYU-KOH3HKNNUM   = "183" OR "283" ) OR
                (LNK-SYU-KOH4HKNNUM   = "183" OR "283" ))
      *
             IF   LNK-SYU-KOH1HKNNUM  = "183" OR "283"
               MOVE  LNK-SYU-SYUCOMPFTN     TO  LNK-SYU-JYO-HKNFTNMONEY
             END-IF
             IF   LNK-SYU-KOH2HKNNUM  = "183" OR "283"
               MOVE  LNK-SYU-KOH-COMPFTN(1) TO  LNK-SYU-JYO-HKNFTNMONEY
             END-IF
             IF   LNK-SYU-KOH3HKNNUM  = "183" OR "283"
               MOVE  LNK-SYU-KOH-COMPFTN(2) TO  LNK-SYU-JYO-HKNFTNMONEY
             END-IF
             IF   LNK-SYU-KOH4HKNNUM  = "183" OR "283"
               MOVE  LNK-SYU-KOH-COMPFTN(3) TO  LNK-SYU-JYO-HKNFTNMONEY
             END-IF
      *
             COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                      LNK-SYU-JYO-HKNFTNMONEY  +    5
             COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                      LNK-SYU-JYO-HKNFTNMONEY  /   10
             COMPUTE  LNK-SYU-JYO-HKNFTNMONEY  =
                      LNK-SYU-JYO-HKNFTNMONEY  *   10
      *
             IF     FLG-GENBUTU70    NOT =    ZERO
                IF  (FLG-IRYOHI70       =    3   OR   4  )
                AND (LNK-SYU-KOH1HKNNUM =  "183" OR "283")
                AND (FLG-SYUKEI2        =   ZERO         )
                  COMPUTE WK-FUTANWARI   = WK-KOH-FTNRATE  /  100
                  COMPUTE WK-OITA-KEISAN = WK-KOUREI-IRYOHI70
                                         * WK-FUTANWARI
                  COMPUTE  WK-OITA-KEISAN  =
                           WK-OITA-KEISAN  +    5
                  COMPUTE  WK-OITA-KEISAN  =
                           WK-OITA-KEISAN  /   10
                  COMPUTE  WK-OITA-KEISAN  =
                           WK-OITA-KEISAN  *   10
                  IF      MAX-KIN-SYU-M  > WK-OITA-KEISAN
                    MOVE   WK-OITA-KEISAN  TO  WK-OITA-KEISAN2
                  ELSE
                    MOVE   MAX-KIN-SYU-M   TO  WK-OITA-KEISAN2
                  END-IF
                  IF   WK-OITA-KEISAN2  >  80100
                    COMPUTE  WK-OITA-CYOFTN-KON  =  WK-OITA-KEISAN2
                                                 -  80100
                    IF  WK-OITA-CYOFTN-KON   >  WK-OITA-CYOFTN
                       COMPUTE WK-OITA-CYOFTN-KON  = WK-OITA-CYOFTN-KON
                                                   - WK-OITA-CYOFTN
                    ELSE
                       MOVE    ZERO    TO    WK-OITA-CYOFTN-KON
                    END-IF
                  ELSE
                    MOVE       ZERO    TO    WK-OITA-CYOFTN-KON
                  END-IF
                ELSE
                  MOVE         ZERO    TO    WK-OITA-CYOFTN-KON
                END-IF
                MOVE           ZERO    TO    LNK-SYU-JYO-HKNFTNMONEY
             ELSE
                IF (LNK-SYU-KOH1HKNNUM   = "972" OR "974" ) OR
                   (LNK-SYU-KOH2HKNNUM   = "972" OR "974" ) OR
                   (LNK-SYU-KOH3HKNNUM   = "972" OR "974" ) OR
                   (LNK-SYU-KOH4HKNNUM   = "972" OR "974" )
                  MOVE         ZERO    TO    LNK-SYU-JYO-HKNFTNMONEY
                  MOVE         ZERO    TO    WK-OITA-CYOFTN-KON
                ELSE
                  COMPUTE  WK-OITA-HKNFTNMONEY = WK-OITA-HKNFTNMONEY
                                               + LNK-SYU-JYO-HKNFTNMONEY
                  IF       WK-OITA-HKNFTNMONEY > 80100
                    COMPUTE  WK-OITA-CYOFTN-KON  =  WK-OITA-HKNFTNMONEY
                                                 -  80100
                    IF  WK-OITA-CYOFTN-KON   >  WK-OITA-CYOFTN
                       COMPUTE WK-OITA-CYOFTN-KON  = WK-OITA-CYOFTN-KON
                                                   - WK-OITA-CYOFTN
                    ELSE
                       MOVE    ZERO    TO    WK-OITA-CYOFTN-KON
                    END-IF
                  ELSE
                    MOVE       ZERO    TO    WK-OITA-CYOFTN-KON
                  END-IF
                END-IF
             END-IF
      *
             MOVE    WK-OITA-CYOFTN-KON  TO  LNK-SYU-JYO-COMPFTN
      *
      *      ÀÁµá³Û¤Ë²Ã»»
             COMPUTE  LNK-SYU-SKYMONEY  =
                      LNK-SYU-SKYMONEY  +    5
             COMPUTE  LNK-SYU-SKYMONEY  =
                      LNK-SYU-SKYMONEY  /   10
             COMPUTE  LNK-SYU-SKYMONEY  =
                      LNK-SYU-SKYMONEY  *   10
             ADD     WK-OITA-CYOFTN-KON  TO  LNK-SYU-SKYMONEY
                                             WK-KNJFTNKIN-ENTANI
             MOVE    1                   TO  FLG-KOUGAKUCYO-OITA
      *
           END-IF
      *
           .
      *
       23074-OITA-TOKUSYU-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÌôºÞ°ìÉôÉéÃ´¶â¤Î»»Äê½èÍý                                *
      ******************************************************************
       YAKFTN-SANTEI-SEC              SECTION.
      *
           MOVE      SPACE       TO      YAKFTN-AREA.
           INITIALIZE                    YAKFTN-AREA.
      *
      *    ÌôºÞ°ìÉôÉéÃ´¶âÄ§¼ý²ÄÇ½¤ÎÈ½Äê
           PERFORM YAKFTN-HANTEI-SEC.
      *
           IF      FLG-YAKFTN    =    SPACE
      *
      *      ÌôºÞ°ìÉôÉéÃ´¶â¥Æ¡¼¥Ö¥ë¤Î¥»¥Ã¥È
             PERFORM        YAKFTN-TABLE-SET-SEC
      *
      *      ÌôºÞ°ìÉôÉéÃ´¶â¥Æ¡¼¥Ö¥ë¤òºÆ¸¡º÷¤·¤ÆÆ±°ìÌôºÞ¤Î¼ïÎà¿ô¤ò¸º»»
             PERFORM YAKFTN-KAISUU-HENSYU-SEC
      *
      *      ÌôºÞ°ìÉôÉéÃ´¶â¥Æ¡¼¥Ö¥ë¤òºÆ¸¡º÷¤·¤ÆºÞÈÖ¹æÊÌ¤ÎÌôºÞ¿ô¤ò·×»»
             PERFORM YAKFTN-SAI-KEISAN-SEC
      *
      *      ºÞÈÖ¹æÊÌ¤ÎÌôºÞ¿ô¤è¤êÌôºÞ°ìÉôÉéÃ´¶â¤ò·×»»
             PERFORM YAKFTN-KINGAKU-KEISAN-SEC
      *
      *      ÊÝ¸±ÀÚ¤êÂØ¤¨»þ¤Î·ÑÂ³¿ÇÎÅÆþÎÏ¤¬¤µ¤ì¤¿¾ì¹ç¤ÎÁ°ÆþÎÏÊ¬
      *      ¤ÎÌôºÞ°ìÉôÉéÃ´¶â¤Î»»½Ð
             IF      LNK-SYU-CONTKBN     =    "2"
                 PERFORM YAKFTN-CONT-SEC
             END-IF
      *
      *      ÌôºÞ°ìÉôÉéÃ´¶â¤Î¾å¸Â³Û¤ò¼çÊÝ¸±¤ÎÉéÃ´¶â³Û¤Þ¤Ç¤È¤¹¤ë
             PERFORM YAKFTN-JOUGEN-HANTEI-SEC
           END-IF.
      *
       YAKFTN-SANTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *     Ç¯Îð¡¢ÊÝ¸±ÆâÍÆ¤«¤éÌôºÞ°ìÉôÉéÃ´¶â»»Äê²ÄÇ½´µ¼Ô¤Ç¤¢¤ë¤«È½Äê   *
      ******************************************************************
       YAKFTN-HANTEI-SEC           SECTION.
      *
           MOVE    SPACE                TO      FLG-YAKFTN.
      *
      *    Ç¯Îð£¶ºÍÌ¤Ëþ
           INITIALIZE                           PTINF-REC.
           MOVE    LNK-SYU-HOSPID       TO      PTINF-HOSPID.
           MOVE    LNK-SYU-PTID         TO      PTINF-PTID.
           MOVE    PTINF-REC            TO      MCPDATA-REC.
           MOVE    "DBSELECT"           TO      MCP-FUNC.
           MOVE    "PTINF-KEY"          TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC.
           IF      MCP-RC               =       ZERO
               MOVE    "PTINF-KEY"      TO      ORC-DBPATH
               PERFORM  PTINF-READ-SEC
               IF       MCP-RC          =       ZERO
                    MOVE   PTINF-BIRTHDAY   TO  WRK-YMDS1
                    MOVE   LNK-SYU-SRYYMD   TO  WRK-YMDS2
                    MOVE   ZERO             TO  WRK-NENREI
                    COMPUTE  WRK-NENREI     =   WRK-YYS2
                                            -   WRK-YYS1
                    IF   WRK-YMDS1(5:4)     >   WRK-YMDS2(5:4)
                         ADD   -1           TO  WRK-NENREI
                    END-IF
                    IF   WRK-NENREI         =   70 
                    AND  WRK-YMDS1(5:4)    <=   WRK-YMDS2(5:4)
                            IF    WRK-YMDS1(5:2)  = WRK-YMDS2(5:2)
                                  IF   WRK-YMDS1(7:2)  NOT = "01"
                                       ADD   -1     TO  WRK-NENREI
                                  END-IF
                            END-IF
                    END-IF
               ELSE
                    DISPLAY  "PTINF FETCH ERR"
                    MOVE     ZERO   TO      WRK-NENREI
               END-IF
           ELSE
               DISPLAY   "PTINF SELECT ERR"
               MOVE     ZERO   TO      WRK-NENREI
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "PTINF-KEY"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
      *     IF      WRK-NENREI         <     6
      *         MOVE    "1"            TO      FLG-YAKFTN
      *     END-IF.
           IF     (WRK-NENREI         <     6)   OR
                  (WRK-NENREI         >=    70)
               MOVE    "1"            TO      FLG-YAKFTN
           END-IF.
      *
      *    Á¥°÷ÊÝ¸±¤Î³ºÅö¼Ô¡§¿¦Ì³¾å
      *                    ¡§²¼Á¥¸å£³¥ö·î°ÊÆâ
      *                    ¡§ÄÌ¶ÐºÒ³²
           IF     (COMB-HKNNUM        =     "002")   AND
                  (COMB-HOJOKBN       =  "1"  OR  "2"  OR  "3"  OR
                                         "A"  OR  "B"  OR  "C"  OR
                                         "D"  OR  "E"  OR  "F"    )
               MOVE    "1"            TO      FLG-YAKFTN
           END-IF.
      *
      *    ¶¦ºÑÁÈ¹çÊÝ¸±¤Î³ºÅö¼Ô¡§²¼Á¥¸å£³¥ö·î°ÊÆâ
           IF     (COMB-HKNNUM  = "031" OR "032" OR "033" OR "034") AND
                  (COMB-HOJOKBN =  "2"  OR  "B"  OR  "E"          )
               MOVE    "1"            TO      FLG-YAKFTN
           END-IF.
      *
      *    Ï·¿ÍÊÝ¸±¼õµë¼Ô¡Ê£¶£µºÍ°Ê¾å´Þ¤à¡Ë
           IF     COMB-KOH1HKNNUM  = "027"
               MOVE    "1"            TO      FLG-YAKFTN
           END-IF.
      *
       YAKFTN-HANTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *     °ú¤­ÅÏ¤·¥Ñ¥é¥á¥¿¤«¤éÌôºÞ°ìÉôÉéÃ´¶â¥Æ¡¼¥Ö¥ë¤Ø¤Î¥»¥Ã¥È½èÍý   *
      ******************************************************************
       YAKFTN-TABLE-SET-SEC           SECTION.
      *
           MOVE    ZERO                 TO      Y-IDX3.
           PERFORM VARYING     Y-IDX    FROM    1   BY  1
                   UNTIL      (Y-IDX        >       3)
             PERFORM VARYING     Y-IDX2    FROM    1   BY  1
                     UNTIL    (Y-IDX2       >       40)
               IF    (LNK-S01-SRYKBN(Y-IDX Y-IDX2)   NOT =  SPACE)
               AND   (LNK-S01-SRYCD (Y-IDX Y-IDX2) (1:1) =  "6"  )
                   ADD        1     TO    Y-IDX3
                   MOVE  LNK-S01-SRYKBN (Y-IDX Y-IDX2)
                                         TO    LNK-S02-SRYKBN (Y-IDX3)
                   MOVE  LNK-S01-ZAIBAN (Y-IDX Y-IDX2)
                                         TO    LNK-S02-ZAIBAN (Y-IDX3)
                   MOVE  LNK-S01-SRYCD  (Y-IDX Y-IDX2)
                                         TO    LNK-S02-SRYCD  (Y-IDX3)
                   MOVE  LNK-S01-SURYOU (Y-IDX Y-IDX2)
                                         TO    LNK-S02-SURYOU (Y-IDX3)
                   MOVE  LNK-S01-KAISUU (Y-IDX Y-IDX2)
                                         TO    LNK-S02-KAISUU (Y-IDX3)
                   MOVE  LNK-S01-TENSUU (Y-IDX Y-IDX2)
                                         TO    LNK-S02-TENSUU (Y-IDX3)
               END-IF
             END-PERFORM
           END-PERFORM.
      *
           PERFORM VARYING     Y-IDX    FROM    1   BY  1
                   UNTIL      (LNK-S02-SRYKBN(Y-IDX)    =   SPACE) OR
                              (Y-IDX                    >   40   )
               MOVE  LNK-S02-SRYKBN (Y-IDX) TO YAKFTN-SRYKBN (Y-IDX)
               MOVE  LNK-S02-ZAIBAN (Y-IDX) TO YAKFTN-ZAIBAN (Y-IDX)
               MOVE  LNK-S02-SRYCD  (Y-IDX) TO YAKFTN-SRYCD  (Y-IDX)
               MOVE  LNK-S02-TENSUU (Y-IDX) TO YAKFTN-TENSUU (Y-IDX)
               IF    LNK-S02-SRYKBN(Y-IDX)  =  "22"
                     MOVE    1              TO LNK-S02-KAISUU(Y-IDX)
               END-IF
               MOVE  LNK-S02-KAISUU (Y-IDX) TO YAKFTN-MAX    (Y-IDX)
               MOVE  LNK-S02-KAISUU (Y-IDX) TO Y-IDX3
               PERFORM VARYING     Y-IDX2   FROM    1   BY  1
                       UNTIL       Y-IDX2   >       Y-IDX3
                    MOVE    1      TO    YAKFTN-KAISUU   (Y-IDX Y-IDX2)
              END-PERFORM
           END-PERFORM.
      *
      *    ÌôºÞ°ìÉôÉéÃ´¶â¤Î¾å¸Â³ÛÈ½ÄêÍÑ¤Ë¿ÇÎÅ¶èÊ¬ÊÌ¤ÎÌôºÞÅÀ¿ô¤ò¼èÆÀ
           PERFORM VARYING     Y-IDX    FROM    1   BY  1
                   UNTIL      (Y-IDX                    >   40   )  OR
                              (LNK-S02-SRYKBN(Y-IDX)    =   SPACE)
               PERFORM   VARYING   Y-IDX2    FROM   1   BY   1
                         UNTIL     Y-IDX2    >          40
                        IF   LNK-S02-ZAIBAN(Y-IDX)  =  
                                              WK-TOYAKU-ZAIBAN(Y-IDX2)
                             MOVE   41      TO   Y-IDX2
                        ELSE
                          IF   WK-TOYAKU-ZAIBAN(Y-IDX2)    =   ZERO
                              MOVE   LNK-S02-SRYKBN(Y-IDX) TO
                                              WK-TOYAKU-SRYKBN(Y-IDX2)
                              MOVE   LNK-S02-ZAIBAN(Y-IDX) TO
                                              WK-TOYAKU-ZAIBAN(Y-IDX2)
                              IF     LNK-S02-SRYKBN(Y-IDX)  =  "21" OR
                                                               "22"
                                  COMPUTE    WK-TOYAKU-ZAITEN(Y-IDX2)
                                          =  LNK-S02-TENSUU(Y-IDX)
                                          *  LNK-S02-KAISUU(Y-IDX)
                              ELSE
                                  MOVE   LNK-S02-TENSUU(Y-IDX) TO
                                              WK-TOYAKU-ZAITEN(Y-IDX2)
                              END-IF
                              MOVE   41       TO   Y-IDX2
                          END-IF
                        END-IF
               END-PERFORM
           END-PERFORM.
           PERFORM    VARYING    Y-IDX   FROM  1   BY   1
                      UNTIL     (Y-IDX       >   40)    OR
                                (WK-TOYAKU-SRYKBN(Y-IDX)   =    SPACE)
                  EVALUATE    WK-TOYAKU-SRYKBN(Y-IDX)
                      WHEN    "21"
                          ADD   WK-TOYAKU-ZAITEN(Y-IDX)    TO
                                                    WK-TOYAKU-TENSU(1)
                      WHEN    "22"
                          ADD   WK-TOYAKU-ZAITEN(Y-IDX)    TO
                                                    WK-TOYAKU-TENSU(2)
                      WHEN    "23"
                          ADD   WK-TOYAKU-ZAITEN(Y-IDX)    TO
                                                    WK-TOYAKU-TENSU(3)
                  END-EVALUATE
           END-PERFORM.
           COMPUTE    WK-TOYAKU-TENSU(1)  =  WK-TOYAKU-TENSU(1) *  10.
           COMPUTE    WK-TOYAKU-TENSU(2)  =  WK-TOYAKU-TENSU(2) *  10.
           COMPUTE    WK-TOYAKU-TENSU(3)  =  WK-TOYAKU-TENSU(3) *  10.
      *
       YAKFTN-TABLE-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÌôºÞ°ìÉôÉéÃ´¶â¥Æ¡¼¥Ö¥ë¤òºÆ¸¡º÷¤·¤ÆÆ±°ìÌôºÞ¤Î¼ïÎà¿ô¤ò·×»»    *
      ******************************************************************
       YAKFTN-KAISUU-HENSYU-SEC          SECTION.
      *
      *        ¿ÇÎÅ¶èÊ¬¡¦¿ÇÎÅ¥³¡¼¥É¤ÇÊÂ¤ÓÂØ¤¨
      *
           MOVE   YAKFTN-TABLE      TO    YAKFTN-TABLE2.
           PERFORM VARYING     Y-IDX   FROM    1   BY  1
                   UNTIL      (YAKFTN-SRYKBN2(Y-IDX)    =   SPACE)  OR
                              (Y-IDX                   >   39   )
               COMPUTE Y-IDX2  =   Y-IDX       +   1
               PERFORM VARYING     Y-IDX3  FROM    Y-IDX2  BY  1
                       UNTIL      (YAKFTN-SRYKBN2(Y-IDX3) = SPACE)  OR
                                  (Y-IDX3              >   40   )
                   IF      YAKFTN-KEY2 (Y-IDX) > YAKFTN-KEY2 (Y-IDX3)
                       MOVE    YAKFTN-TBL-OCC2(Y-IDX)   TO
                                               WK-SRT-YAKFTN-TABLE
                       MOVE    YAKFTN-TBL-OCC2(Y-IDX3)  TO
                                               YAKFTN-TBL-OCC2(Y-IDX)
                       MOVE    WK-SRT-YAKFTN-TABLE     TO
                                               YAKFTN-TBL-OCC2(Y-IDX3)
                   END-IF
               END-PERFORM
           END-PERFORM.       
           MOVE   YAKFTN-TABLE2     TO    YAKFTN-TABLE.
      *
      *//     PERFORM VARYING     Y-IDX   FROM    1   BY  1
      *//             UNTIL      (YAKFTN-SRYKBN(Y-IDX)    =   SPACE)  OR
      *//                        (Y-IDX                   >   39   )
      *//         COMPUTE Y-IDX2  =   Y-IDX       +   1
      *//        PERFORM VARYING     Y-IDX3  FROM    Y-IDX2  BY  1
      *//                 UNTIL      (YAKFTN-SRYKBN(Y-IDX3) = SPACE)  OR
      *//                            (Y-IDX3              >   40   )
      *//             IF      YAKFTN-KEY (Y-IDX) > YAKFTN-KEY (Y-IDX3)
      *//                 MOVE    YAKFTN-TBL-OCC(Y-IDX)   TO
      *//                                         WK-SRT-YAKFTN-TABLE
      *//                 MOVE    YAKFTN-TBL-OCC(Y-IDX3)  TO
      *//                                         YAKFTN-TBL-OCC(Y-IDX)
      *//                 MOVE    WK-SRT-YAKFTN-TABLE     TO
      *//                                         YAKFTN-TBL-OCC(Y-IDX3)
      *//            END-IF
      *//         END-PERFORM
      *//     END-PERFORM.       
      *
      *
      *     PERFORM   VARYING  DSP-IDX   FROM   1   BY   1
      *                UNTIL  DSP-IDX   >   10
      *         DISPLAY  "YAKFTN-SRYCD(DSP-IDX)  = "
      *                                   YAKFTN-SRYCD(DSP-IDX)
      *         DISPLAY  "YAKFTN-ZAIBAN(DSP-IDX) = "
      *                                   YAKFTN-ZAIBAN(DSP-IDX)
      *         DISPLAY  "YAKFTN-TENSUU(DSP-IDX) = "
      *                                   YAKFTN-TENSUU(DSP-IDX)
      *       PERFORM    VARYING   DSP-IDX2   FROM  1  BY  1
      *                  UNTIL     DSP-IDX2   >   5  
      *         DISPLAY "YAKFTN-KAISUU(" DSP-IDX ":" DSP-IDX2 ") = "
      *                           YAKFTN-KAISUU(DSP-IDX DSP-IDX2)
      *       END-PERFORM
      *     END-PERFORM.                             
      *               
      *
      *        ¿ÇÎÅ¶èÊ¬¡¦¿ÇÎÅ¥³¡¼¥ÉÊÌ¤ËÉþÍÑ´ü´Ö¤ÎÊÔ½¸
           MOVE    ZERO                 TO  WK-KAISUU-TABLE
           MOVE    1                    TO  Y-IDX
           PERFORM UNTIL      (YAKFTN-SRYKBN(Y-IDX)   =   SPACE)   OR
                              (Y-IDX                   >   40  )
               MOVE    YAKFTN-KEY   (Y-IDX)     TO  WK-KEY-SRY
               MOVE    YAKFTN-KAISUU-TABLE(Y-IDX)
                                                TO  WK-KAISUU-TABLE
               MOVE    YAKFTN-TENSUU(Y-IDX)
                                                TO  WK-YAKFTN-TENSUU
               ADD     1                        TO  Y-IDX
               PERFORM UNTIL      (YAKFTN-SRYKBN(Y-IDX)  =  SPACE) OR
                                  (Y-IDX                 >  40   ) OR
                                  (WK-KEY-SRY              NOT =
                                                  YAKFTN-KEY (Y-IDX))
      *
                   PERFORM VARYING Y-IDX3  FROM    1   BY  1
                           UNTIL   Y-IDX3  >       YAKFTN-MAX (Y-IDX)
                       IF      WK-KAISUU    (Y-IDX3)   =   1
                             AND  YAKFTN-TENSUU(Y-IDX)  >  20   AND
                                  WK-YAKFTN-TENSUU      >  20
                           MOVE  ZERO  TO  YAKFTN-KAISUU(Y-IDX Y-IDX3)
                       END-IF
      *                 IF  YAKFTN-TENSUU (Y-IDX)  <=   20
      *                     MOVE  1     TO  YAKFTN-KAISUU(Y-IDX Y-IDX3)
      *                 END-IF
                   END-PERFORM
      *
                   PERFORM VARYING Y-IDX3  FROM    1   BY  1
                           UNTIL   Y-IDX3  > YAKFTN-MAX (Y-IDX)
                       IF      YAKFTN-KAISUU(Y-IDX Y-IDX3) =  1
                           MOVE    1       TO  WK-KAISUU   (Y-IDX3)
                       END-IF
                   END-PERFORM
      *
                   MOVE    YAKFTN-TENSUU(Y-IDX)    TO  WK-YAKFTN-TENSUU
                   ADD     1                       TO  Y-IDX
              END-PERFORM
      *
           END-PERFORM.       
      *
      *
      *     PERFORM   VARYING  DSP-IDX   FROM   1   BY   1
      *                UNTIL  DSP-IDX   >   10
      *         DISPLAY  "YAKFTN-SRYCD(DSP-IDX)  = "
      *                                   YAKFTN-SRYCD(DSP-IDX)
      *         DISPLAY  "YAKFTN-ZAIBAN(DSP-IDX) = "
      *                                   YAKFTN-ZAIBAN(DSP-IDX)
      *         DISPLAY  "YAKFTN-TENSUU(DSP-IDX) = "
      *                                   YAKFTN-TENSUU(DSP-IDX)
      *       PERFORM    VARYING   DSP-IDX2   FROM  1  BY  1
      *                  UNTIL     DSP-IDX2   >   5  
      *         DISPLAY "YAKFTN-KAISUU(" DSP-IDX ":" DSP-IDX2 ") = "
      *                           YAKFTN-KAISUU(DSP-IDX DSP-IDX2)
      *       END-PERFORM
      *     END-PERFORM.                             
      *               
      *
       YAKFTN-KAISUU-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÌôºÞ°ìÉôÉéÃ´¶â¥Æ¡¼¥Ö¥ë¤òºÆ¸¡º÷¤·¤ÆºÞÈÖ¹æÊÌ¤ÎÌôºÞ¿ô¤ò·×»»    *
      ******************************************************************
       YAKFTN-SAI-KEISAN-SEC                SECTION.
      *
      *        ºÞÈÖ¹æ¤ÇÊÂ¤ÓÂØ¤¨
           PERFORM VARYING     Y-IDX   FROM    1   BY  1
                   UNTIL      (YAKFTN-SRYKBN(Y-IDX)  =  SPACE) OR
                              (Y-IDX                 >  39   )
               COMPUTE Y-IDX2  =   Y-IDX       +   1
               PERFORM VARYING     Y-IDX3  FROM    Y-IDX2  BY  1
                       UNTIL      (YAKFTN-SRYKBN(Y-IDX3) = SPACE)  OR
                                  (Y-IDX3                >   40 )
                   IF     YAKFTN-ZAIBAN(Y-IDX) > YAKFTN-ZAIBAN(Y-IDX3)
                       MOVE    YAKFTN-TBL-OCC(Y-IDX)  TO
                                                WK-SRT-YAKFTN-TABLE
                       MOVE    YAKFTN-TBL-OCC(Y-IDX3) TO
                                                YAKFTN-TBL-OCC(Y-IDX)
                       MOVE    WK-SRT-YAKFTN-TABLE    TO
                                                YAKFTN-TBL-OCC(Y-IDX3)
                   END-IF
               END-PERFORM
           END-PERFORM.       
      *
      *
      *
      *     PERFORM   VARYING  DSP-IDX   FROM   1   BY   1
      *                UNTIL  DSP-IDX   >   10
      *         DISPLAY  "YAKFTN-SRYCD(DSP-IDX)  = "
      *                                   YAKFTN-SRYCD(DSP-IDX)
      *         DISPLAY  "YAKFTN-ZAIBAN(DSP-IDX) = "
      *                                   YAKFTN-ZAIBAN(DSP-IDX)
      *         DISPLAY  "YAKFTN-TENSUU(DSP-IDX) = "
      *                                   YAKFTN-TENSUU(DSP-IDX)
      *       PERFORM    VARYING   DSP-IDX2   FROM  1  BY  1
      *                  UNTIL     DSP-IDX2   >   5  
      *         DISPLAY "YAKFTN-KAISUU(" DSP-IDX ":" DSP-IDX2 ") = "
      *                           YAKFTN-KAISUU(DSP-IDX DSP-IDX2)
      *       END-PERFORM
      *     END-PERFORM.                             
      *               
      *        ºÞÈÖ¹æÊÌ¤ËÌôºÞ¿ô¤ò·×»»
      *
           MOVE    ZERO                 TO  FLG-ZAIGATAKBN
      *
           MOVE    1                    TO  Y-IDX
           MOVE    ZERO                 TO  Y-IDX4
           PERFORM UNTIL      (YAKFTN-SRYKBN(Y-IDX)   =   SPACE)   OR
                              (Y-IDX                   >   40  )
               ADD     1                        TO  Y-IDX4
               MOVE    YAKFTN-SRYKBN  (Y-IDX)   TO
                                           YAKFTN-ZAI-SRYKBN  (Y-IDX4)
               MOVE    YAKFTN-ZAIBAN  (Y-IDX)   TO
                                           YAKFTN-ZAI-ZAIBAN  (Y-IDX4)
               MOVE    YAKFTN-SRYCD   (Y-IDX)   TO
                                           YAKFTN-ZAI-SRYCD   (Y-IDX4)
               MOVE    YAKFTN-TENSUU  (Y-IDX)   TO
                                           YAKFTN-ZAI-TENSUU  (Y-IDX4)
               MOVE    YAKFTN-ZAIBAN  (Y-IDX)   TO
                                           WK-KEY-ZAIBAN
               MOVE    ZERO                     TO
                                           WK-ZAI-KAISUU-TABLE
      *
               MOVE    ZERO                 TO  FLG-ZAIGATAKBN
      *
              PERFORM UNTIL  (YAKFTN-SRYKBN(Y-IDX)  =  SPACE)   OR
                             (Y-IDX                 >  40   )   OR
                             (WK-KEY-ZAIBAN   NOT =
                                                YAKFTN-ZAIBAN(Y-IDX))
      *
                 MOVE    ZERO                    TO  FLG-ADDKBN
      *
      *        ¡ÊÆâÉþÌôºÞ¤Îº®¹ç¡Ë
                 IF      YAKFTN-SRYKBN (Y-IDX)   =   "21"
                     INITIALIZE                      TNS-TENSU-REC
                     MOVE    YAKFTN-SRYCD (Y-IDX)    TO  TNS-SRYCD
                     MOVE    LNK-SYU-SRYYMD          TO  ORC-DBYMD
                     PERFORM TENSU-READ-SEC
                     IF      TNS-ZAIGATAKBN          =   "1" OR
                                                         "2" OR
                                                         "3"
                        IF      FLG-ZAIGATAKBN       =   1
                            MOVE    1                TO  FLG-ADDKBN
                        END-IF
                        MOVE    1                    TO  FLG-ZAIGATAKBN
                     END-IF        
                 END-IF
                 IF     FLG-ADDKBN              =   ZERO
                    PERFORM VARYING Y-IDX3  FROM    1   BY  1
                            UNTIL   Y-IDX3  >       YAKFTN-MAX (Y-IDX)
                        IF      YAKFTN-KAISUU(Y-IDX Y-IDX3)   >   0
                            ADD     YAKFTN-KAISUU(Y-IDX Y-IDX3)
                                            TO  WK-ZAI-KAISUU (Y-IDX3)
                            IF      Y-IDX3    >  YAKFTN-ZAI-MAX(Y-IDX4)
                                MOVE Y-IDX3  TO  YAKFTN-ZAI-MAX(Y-IDX4)
                            END-IF
                        END-IF
                    END-PERFORM
                END-IF    
      *
                ADD     1                       TO  Y-IDX      
              END-PERFORM
              MOVE    WK-ZAI-KAISUU-TABLE     TO
                                      YAKFTN-ZAI-KAISUU-TABLE (Y-IDX4)
           END-PERFORM.
      *
      *     DISPLAY "*** ZAIBANGTOU BETSU YAKUZAISUU KEISANGO  ***".
      *
      *     PERFORM   VARYING  DSP-IDX   FROM   1   BY   1
      *                UNTIL  DSP-IDX   >   10
      *         DISPLAY  "YAKFTN-ZAI-SRYCD(DSP-IDX)  = "
      *                                   YAKFTN-ZAI-SRYCD(DSP-IDX)
      *         DISPLAY  "YAKFTN-ZAI-ZAIBAN(DSP-IDX) = "
      *                                   YAKFTN-ZAI-ZAIBAN(DSP-IDX)
      *         DISPLAY  "YAKFTN-ZAI-TENSUU(DSP-IDX) = "
      *                                   YAKFTN-ZAI-TENSUU(DSP-IDX)
      *       PERFORM    VARYING   DSP-IDX2   FROM  1  BY  1
      *                  UNTIL     DSP-IDX2   >   5  
      *         DISPLAY "YAKFTN-ZAI-KAISUU(" DSP-IDX ":" DSP-IDX2 ") = "
      *                           YAKFTN-ZAI-KAISUU(DSP-IDX DSP-IDX2)
      *       END-PERFORM
      *     END-PERFORM.                             
      *               
      *
           PERFORM VARYING     Y-IDX   FROM    1   BY  1
                   UNTIL   (YAKFTN-ZAI-SRYKBN(Y-IDX)   =   SPACE)  OR
                           (Y-IDX                      >   40  )
      *        ¡Ê£²£°ÅÀ°Ê²¼ÂÐ±þ¡Ë
               IF      YAKFTN-ZAI-TENSUU (Y-IDX)   <=  20
                   PERFORM VARYING Y-IDX2  FROM    1   BY  1
                           UNTIL   Y-IDX2  >     YAKFTN-ZAI-MAX(Y-IDX)
                       IF      YAKFTN-ZAI-KAISUU (Y-IDX Y-IDX2) >   0
                           MOVE    1               TO
                                     YAKFTN-ZAI-KAISUU (Y-IDX Y-IDX2)
                       END-IF
                   END-PERFORM
               END-IF
      *        ¡Ê³°ÍÑÌô¤Îº®¹ç¡Ë
               IF      YAKFTN-ZAI-SRYKBN (Y-IDX)   =   "23"
                   PERFORM VARYING Y-IDX2  FROM    1   BY  1
                           UNTIL   Y-IDX2  >    YAKFTN-ZAI-MAX(Y-IDX)
                       IF      YAKFTN-ZAI-KAISUU (Y-IDX Y-IDX2) >   0
                           MOVE    1               TO
                                     YAKFTN-ZAI-KAISUU (Y-IDX Y-IDX2)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.
      *
       YAKFTN-SAI-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÌôºÞ°ìÉôÉéÃ´¶â¥Æ¡¼¥Ö¥ë¤òºÆ¸¡º÷¤·¤ÆºÞÈÖ¹æÊÌ¤ÎÌôºÞ¿ô¤ò·×»»    *
      ******************************************************************
       YAKFTN-KINGAKU-KEISAN-SEC                SECTION.
      *
      *----(01.00.01) LINE MOD START -------------------------------
      *
      *    MOVE    ZERO                 TO  YAKFTN-SOGOKEI
      *
      *    PERFORM VARYING     Y-IDX   FROM    1   BY  1
      *          UNTIL      (YAKFTN-ZAI-SRYKBN(Y-IDX)   =   SPACE)   OR
      *                       (Y-IDX                      >   40  )
      *      PERFORM VARYING Y-IDX2  FROM    1   BY  1
      *              UNTIL   Y-IDX2  >       YAKFTN-ZAI-MAX(Y-IDX)
      *          IF      YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2)    >   0
      *              EVALUATE    TRUE
      *                  WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =   "21" AND
      *                        YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2) =   1
      *                      MOVE    ZERO             TO  WK-FUTAN
      *                  WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =   "21" AND
      *                        YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2) = 2 OR 3
      *                      MOVE    30               TO  WK-FUTAN
      *                  WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =   "21" AND
      *                      YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2) =  4 OR 5
      *                      MOVE    60               TO  WK-FUTAN
      *                  WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =   "21" AND
      *                        YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2) >  5
      *                      MOVE    100              TO  WK-FUTAN
      *                  WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =   "22" 
      *                      COMPUTE  WK-FUTAN    =   
      *                        YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2)  *   10
      *                  WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =   "23" AND
      *                        YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2) =   1
      *	                     MOVE     50              TO  WK-FUTAN    
      *                  WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =   "23" AND
      *                        YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2) =   2
      *                     MOVE     100              TO  WK-FUTAN
      *                  WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =  "23" AND
      *                        YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2) >   2
      *                     MOVE     150              TO  WK-FUTAN
      *              END-EVALUATE
      *              ADD     WK-FUTAN             TO  YAKFTN-SOGOKEI
      *          END-IF
      *      END-PERFORM
      *    END-PERFORM.
      *
      *
           MOVE    ZERO                 TO  YAKFTN-SOGOKEI
      *
           PERFORM VARYING     Y-IDX   FROM    1   BY  1
                 UNTIL      (YAKFTN-ZAI-SRYKBN(Y-IDX)   =   SPACE)   OR
                              (Y-IDX                      >   40  )
             PERFORM VARYING Y-IDX2  FROM    1   BY  1
                     UNTIL   Y-IDX2  >       YAKFTN-ZAI-MAX(Y-IDX)
                 IF      YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2)    >   0
                     EVALUATE    TRUE
                         WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =   "21"
                               ADD  YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2) TO
                                    YAKFTN-TOTAL-KAISUU(1 Y-IDX2)
                         WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =   "22"
                               ADD  YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2) TO
                                    YAKFTN-TOTAL-KAISUU(2 Y-IDX2)
                         WHEN  YAKFTN-ZAI-SRYKBN(Y-IDX)    =   "23"
                               ADD  YAKFTN-ZAI-KAISUU(Y-IDX Y-IDX2) TO
                                    YAKFTN-TOTAL-KAISUU(3 Y-IDX2)
                     END-EVALUATE
                 END-IF
             END-PERFORM
           END-PERFORM.
      *
           PERFORM VARYING     Y-IDX   FROM    1   BY  1
                   UNTIL     (Y-IDX               >   3  )
             PERFORM VARYING Y-IDX2  FROM    1   BY  1
                     UNTIL    (Y-IDX2              >       180)
                 IF      YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2)    >   0
                     EVALUATE    TRUE
                         WHEN  Y-IDX       =     1     AND
                               YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2) =   1
                             MOVE    ZERO             TO  WK-FUTAN
                         WHEN  Y-IDX       =     1     AND
                               YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2) = 2
                             MOVE    30               TO  WK-FUTAN
                         WHEN  Y-IDX       =     1     AND
                               YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2) = 3
                             MOVE    30               TO  WK-FUTAN
                         WHEN  Y-IDX       =     1     AND
                             YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2) =   4
                             MOVE    60               TO  WK-FUTAN
                         WHEN  Y-IDX       =     1     AND
                             YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2) =   5
                             MOVE    60               TO  WK-FUTAN
                         WHEN  Y-IDX       =     1     AND
                               YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2) >  5
                             MOVE    100              TO  WK-FUTAN
                         WHEN  Y-IDX       =     2 
                             COMPUTE  WK-FUTAN    =   
                               YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2)  *  10
                         WHEN  Y-IDX       =     3     AND
                               YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2) =   1
      	                     MOVE     50              TO  WK-FUTAN    
                         WHEN  Y-IDX       =     3     AND
                               YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2) =   2
                            MOVE     100              TO  WK-FUTAN
                         WHEN  Y-IDX       =     3     AND
                               YAKFTN-TOTAL-KAISUU(Y-IDX Y-IDX2) >   2
                            MOVE     150              TO  WK-FUTAN
                     END-EVALUATE
                     EVALUATE    Y-IDX
                         WHEN    1
                                ADD    WK-FUTAN   TO  WK-NAI-FUTAN
                         WHEN    2
                                ADD    WK-FUTAN   TO  WK-TON-FUTAN
                         WHEN    3
                                ADD    WK-FUTAN   TO  WK-GAI-FUTAN
                     END-EVALUATE
      ***           ADD     WK-FUTAN             TO  YAKFTN-SOGOKEI
                 END-IF
             END-PERFORM
           END-PERFORM.
      *
      **     IF    WK-TOYAKU-TENSU(1)     <   WK-NAI-FUTAN
      **          MOVE    WK-TOYAKU-TENSU(1)   TO   WK-NAI-FUTAN
      **     END-IF. 
      **     IF    WK-TOYAKU-TENSU(2)     <   WK-TON-FUTAN
      **           MOVE    WK-TOYAKU-TENSU(2)   TO   WK-TON-FUTAN
      **     END-IF. 
      **     IF    WK-TOYAKU-TENSU(3)     <   WK-GAI-FUTAN
      **           MOVE    WK-TOYAKU-TENSU(3)   TO   WK-GAI-FUTAN
      **     END-IF. 
           COMPUTE   YAKFTN-SOGOKEI   =   WK-NAI-FUTAN   +
                                          WK-TON-FUTAN   +
                                          WK-GAI-FUTAN.
           COMPUTE   WK-TOYAKU-TENSU(4)   =    WK-TOYAKU-TENSU(1)    +
                                               WK-TOYAKU-TENSU(2)    +
                                               WK-TOYAKU-TENSU(3).
           IF        YAKFTN-SOGOKEI       >    WK-TOYAKU-TENSU(4)
               MOVE  WK-TOYAKU-TENSU(4)   TO   YAKFTN-SOGOKEI
           END-IF.
      *      
       YAKFTN-KINGAKU-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÊÝ¸±ÀÚ¤êÂØ¤¨»þ¤Î·ÑÂ³¿ÇÎÅÆþÎÏ¤¬¤µ¤ì¤¿¾ì¹ç¤ÎÁ°ÆþÎÏÊ¬
      *    ¤ÎÌôºÞ°ìÉôÉéÃ´¶â¤Î»»½Ð¡Ê¼ýÇ¼¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
      ******************************************************************
       YAKFTN-CONT-SEC                SECTION.
      *
           MOVE    LNK-SYU-HOSPID       TO     SYU-HOSPID.
           MOVE    LNK-SYU-NYUGAIKBN    TO     SYU-NYUGAIKBN.
           MOVE    LNK-SYU-PTID         TO     SYU-PTID.
           MOVE    LNK-SYU-DOUJI-DENPNUM TO    SYU-DOUJI-DENPNUM.
           MOVE    SYU-SYUNOU-REC       TO     MCPDATA-REC.
           MOVE    "DBSELECT"           TO     MCP-FUNC.
           MOVE    "SYUNOU-KEY3"        TO     ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING  MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC.
           IF      MCP-RC               =      ZERO
               MOVE    "SYUNOU-KEY3"    TO     ORC-DBPATH
               PERFORM  SYUNOU-READ-SEC
               PERFORM  UNTIL  (FLG-SYUNOU   =    "1")
                 DISPLAY  "LNK-SYU-DENPNUM = " LNK-SYU-DENPNUM
                 IF    LNK-SYU-DENPNUM   NOT =    1
                   IF  LNK-SYU-DENPNUM   >   SYU-DENPNUM
                     IF  WK-CONT-YAKFTNKIN1    =    ZERO
                         MOVE   SYU-RESE-YKZ-FTN
                                          TO     WK-CONT-YAKFTNKIN1
                     ELSE
                         MOVE   SYU-RESE-YKZ-FTN
                                          TO     WK-CONT-YAKFTNKIN2
                     END-IF
                   END-IF
                   MOVE      "SYUNOU-KEY3"  TO     ORC-DBPATH
                   PERFORM    SYUNOU-READ-SEC
                 ELSE
                   IF  WK-CONT-YAKFTNKIN1    =    ZERO
                       MOVE   SYU-RESE-YKZ-FTN
                                        TO     WK-CONT-YAKFTNKIN1
                   ELSE
                       MOVE   SYU-RESE-YKZ-FTN
                                        TO     WK-CONT-YAKFTNKIN2
                   END-IF
                   MOVE      "SYUNOU-KEY3"  TO     ORC-DBPATH
                   PERFORM   SYUNOU-READ-SEC
                 END-IF
               END-PERFORM
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "SYUNOU-KEY3"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *    ¥È¡¼¥¿¥ë¤ÎÌôºÞ°ìÉôÉéÃ´¶â¤«¤éÁ°¿ÇÎÅ¤ÎÌôºÞ°ìÉôÉéÃ´¶â¤ò¸º»»
           COMPUTE   YAKFTN-SOGOKEI   =   YAKFTN-SOGOKEI
                                      -   WK-CONT-YAKFTNKIN1
                                      -   WK-CONT-YAKFTNKIN2.
      *
       YAKFTN-CONT-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÌôºÞ°ìÉôÉéÃ´¶â¤Î¾å¸Â³Û¤ò¼çÊÝ¸±¤ÎÊÝ¸±ÉéÃ´¶â³Û¤Þ¤Ç¤È¤¹¤ë      *
      ******************************************************************
       YAKFTN-JOUGEN-HANTEI-SEC                SECTION.
      *
           MOVE    ZERO                 TO  YAKFTN-JOUGEN
      *
           IF    (COMB-KOH1-HKNKOHSBTKBN   =   "3")   OR
                 (COMB-KOH2-HKNKOHSBTKBN   =   "3")   OR
                 (COMB-KOH3-HKNKOHSBTKBN   =   "3")   OR
                 (COMB-KOH4-HKNKOHSBTKBN   =   "3")
               CONTINUE
           ELSE
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-HKNNUM        TO    HKN-HKNNUM
               MOVE   "00"               TO    WK-PAYKBN
               MOVE   HKNNUM-REC         TO    MCPDATA-REC
               MOVE   "DBSELECT"         TO    MCP-FUNC
               MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
               CALL   "ORCMCPSUB"         USING MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC            =     ZERO
                   MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   PERFORM  UNTIL 
                     (FLG-HKNNUM     =    1)                       OR
                    ((HKN-TEKSTYMD(1:6)   <=  LNK-SYU-SRYYMD(1:6)) AND
                     (LNK-SYU-SRYYMD(1:6) <=  HKN-TEKEDYMD(1:6))   AND
                     (WK-PAYKBN            =  HKN-PAYKBN))
                       PERFORM   HKNNUM-READ-SEC
                   END-PERFORM
                   IF     FLG-HKNNUM     =       0
                       PERFORM   YAKFTN-JOUGEN-SANTEI-SEC
                       IF    YAKFTN-SOGOKEI     >=   YAKFTN-JOUGEN
                           MOVE    YAKFTN-JOUGEN   TO  YAKFTN-SOGOKEI
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
               MOVE    "HKNNUM-KEY2"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"        USING
                                          MCPAREA
                                          ORCMCPAREA
                                          MCPDATA-REC
      *
           END-IF.
      *
       YAKFTN-JOUGEN-HANTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÌôºÞ°ìÉôÉéÃ´¶â¤Î¾å¸Â³Û¤ò»»Äê¤¹¤ë                            *
      ******************************************************************
       YAKFTN-JOUGEN-SANTEI-SEC                SECTION.
      *
           MOVE    ZERO             TO    YAKFTN-JOUGEN.
      *    ´µ¼ÔÉéÃ´¶â»»½ÐÁ°¤Î£±£°£°¡óÉéÃ´³Û»»½Ð
           MOVE      ZERO         TO      WK-ZENGAKU.
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   15
               COMPUTE WK-ZENGAKU  =   WK-ZENGAKU  +
                                      (LNK-SYU-HKNTEN (IDX) * 10)
           END-PERFORM.
      *    ÊÝ¸±ÁÈ¹ç¤»¥Æ¡¼¥Ö¥ë¤ÎÊä½õ¶èÊ¬¤«¤éÊÝ¸±¤ÎÉéÃ´³ä¹ç¤òÊÝ¸±ÈÖ¹æ
      *    ¥Æ¡¼¥Ö¥ë¤ËºÆ¥»¥Ã¥È
           IF    COMB-HKNNUM           =    "060"
               EVALUATE   COMB-HOJOKBN
                   WHEN   "0"
                          MOVE   100       TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
                   WHEN   "1"
                   WHEN   "4"
                          MOVE   90        TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
                   WHEN   "2"
                   WHEN   "5"
                          MOVE   80        TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
                   WHEN   "3"
                   WHEN   "6"
                          MOVE   70        TO   HKN-HON-GAIKFTNRATE
                                                HKN-KZK-GAIKFTNRATE
                                                HKN-TSY-GAIKFTNRATE
                                                HKN-TNK-GAIKFTNRATE
               END-EVALUATE
           ELSE
               COMPUTE   HKN-HON-GAIKFTNRATE
                         =  100  -  HKN-HON-GAIKFTNRATE
               COMPUTE   HKN-KZK-GAIKFTNRATE
                         =  100  -  HKN-KZK-GAIKFTNRATE
               COMPUTE   HKN-TSY-GAIKFTNRATE
                         =  100  -  HKN-TSY-GAIKFTNRATE
               COMPUTE   HKN-TNK-GAIKFTNRATE
                         =  100  -  HKN-TNK-GAIKFTNRATE
           END-IF.
      *
           EVALUATE    FLG-SANSHOU
               WHEN    "1"
                   IF   COMB-HONKZKKBN    =   "1"
                       IF   HKN-HON-GAIKFTNRATE  NOT =   ZERO
                            COMPUTE    WK-FUTANWARI  =
                                           HKN-HON-GAIKFTNRATE  /  100
                            COMPUTE    YAKFTN-JOUGEN    =
                                           WK-ZENGAKU * WK-FUTANWARI
                       END-IF
                   ELSE
                       IF   HKN-KZK-GAIKFTNRATE  NOT =   ZERO
                            COMPUTE    WK-FUTANWARI  =
                                           HKN-KZK-GAIKFTNRATE  /  100
                            COMPUTE    YAKFTN-JOUGEN    =
                                           WK-ZENGAKU * WK-FUTANWARI
                       END-IF
                   END-IF
               WHEN    "2"
                   IF   HKN-TSY-GAIKFTNRATE     NOT =   ZERO
                        COMPUTE    WK-FUTANWARI  =
                                       HKN-TSY-GAIKFTNRATE
                                                      /  100
                        COMPUTE    YAKFTN-JOUGEN    =
                                       WK-ZENGAKU * WK-FUTANWARI
                   END-IF
               WHEN    "3"
                   IF   HKN-TNK-GAIKFTNRATE     NOT =   ZERO
                        COMPUTE    WK-FUTANWARI  =
                                       HKN-TNK-GAIKFTNRATE
                                                      /  100
                        COMPUTE    YAKFTN-JOUGEN    =
                                       WK-ZENGAKU * WK-FUTANWARI
                   END-IF
           END-EVALUATE.
      *
       YAKFTN-JOUGEN-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¯¶âÍúÎòÆÉ¹þ
      *****************************************************************
       TNKRRK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNKRRK-REC
               MOVE    ZERO            TO  FLG-TNKRRK
           ELSE
               INITIALIZE                  TNKRRK-REC
               MOVE    1               TO  FLG-TNKRRK
           END-IF
      *
           .
       TNKRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Äã½êÆÀ¼ÔÍúÎòÆÉ¹þ
      *****************************************************************
       TSYRRK-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TSYRRK-REC
               MOVE    ZERO            TO  FLG-TSYRRK
           ELSE
               INITIALIZE                  TSYRRK-REC
               MOVE    1               TO  FLG-TSYRRK
           END-IF
      *
           .
       TSYRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       HKNCOMBI-READ2-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WRK-HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  WRK-HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       HKNCOMBI-READ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SRYACCT-READ-SEC         SECTION.
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
       SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SRYACT-READ-SEC         SECTION.
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
       SRYACT-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤êÏ·¿ÍÊÝ¸±¤Î»ÙÊ§ÊýË¡¤ò¼èÆÀ        *
      ******************************************************************
       SYS1001-READ-SEC           SECTION.
      *
           INITIALIZE                         SYS-1001-REC.
           MOVE    "1001"               TO    SYS-1001-KANRICD.
           MOVE    "*"                  TO    SYS-1001-KBNCD.
           MOVE    LNK-SYU-SRYYMD       TO    ORC-DBYMD.
           MOVE    SYS-1001-REC         TO    MCPDATA-REC.
           MOVE    "DBSELECT"           TO    MCP-FUNC.
           MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH.
           CALL    "ORCMCPSUB"    USING        MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC.
           IF      MCP-RC               =     ZERO
               MOVE     "SYSKANRI-KEY"  TO    ORC-DBPATH
               PERFORM   SYSKANRI-READ-SEC
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
       SYS1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
                                           SYS-1026-REC
                                           SYS-1027-REC
                                           SYS-1037-REC
                                           SYS-4001-REC
                                           SYS-4003-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
                                           SYS-1026-REC
                                           SYS-1027-REC
                                           SYS-1037-REC
                                           SYS-4001-REC
                                           SYS-4003-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SYUNOU-READ-SEC          SECTION.
      *
       SYUNOU-READ-100.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYU-SYUNOU-REC
               IF    ((LNK-S01-SAIKEISAN-KBN    =   "1"           ) AND
                      (LNK-SYU-DENPNUM      NOT =  ZERO           ) AND
                      (LNK-SYU-DENPNUM          =  SYU-DENPNUM    ))
               OR    ((LNK-S01-SAIKEISAN-KBN    =   "1"           ) AND
                      (LNK-SYU-GRP-DENPNUM  NOT =  ZERO           ) AND
                      (LNK-SYU-GRP-DENPNUM      =  SYU-GRP-DENPNUM))
                       GO    TO    SYUNOU-READ-100
               ELSE
                       MOVE    ZERO    TO  FLG-SYUNOU
               END-IF
           ELSE
               INITIALIZE                  SYU-SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF.
      *
       SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¾ðÊóÆÉ¹þ
      *****************************************************************
       PTINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊóÆÉ¹þ
      *****************************************************************
       PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÊÝ¸±¾ðÊóÆÉ¹þ
      *****************************************************************
       PTHKNINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       TENSU-READ-EXT.
           EXIT.
