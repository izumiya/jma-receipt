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
       PROGRAM-ID.             ORCR0630.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È¡ÊÆþ±¡¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¿ÇÎÅ¼ÂÆü¿ôÊÔ½¸
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/12/11    NaCl¡ÝÌçÏÆ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    03.03.07  Ì¤¼Â»Ü¸º»»¤ÎÎ¬¾ÎÄÉ²Ã
      *  01.00.02    NACL-ÌçÏÆ    03.03.10  ¿©»ö¶èÊ¬¡É£´¡É¤ÎÂÐ±þÄÉ²Ã
      *  01.00.03    NACL-ÌçÏÆ    03.03.12  ÁªÄêÆþ±¡ÎÁÂÐ±þ
      *  01.00.04    NACL-ÌçÏÆ    03.03.20  Æþ±¡ÎÁÆü¿ô·×»»½¤Àµ
      *  01.00.05    NACL-ÌçÏÆ    03.04.08  ´Ç¸îÇÛÃÖÅ¬¹ç¤ÎÎ¬¾ÎÄÉ²ÃÂÐ±þ
      *  01.00.06    NACL-ÌçÏÆ    03.04.10  ´Ç¸îÇÛÃÖÅ¬¹ç¤Î³°ÇñÂÐ±þ
      *  01.00.07    NACL-Æ£¸¶    03/04/21  ÃÏÊý¸øÈñ¥Ç¡¼¥¿¤Î¤È¤­ÉÔÌÀÊ¬¤Ï
      *                                     ½èÍý¤ÎÂÐ¾Ý¤È¤·¤Ê¤¤
      *  01.00.08    NACL-ÌçÏÆ    03/06/06  ÆÃÄêÆþ±¡ÎÁ¤ÎÎ¬¾Î¤¬Ìµ¤¤¾ì¹ç¤Î
      *                                     ÂÐ±þ
      *  01.00.09    NACL-ÌçÏÆ    03/06/19  ¼ÂÆü¿ô¥Æ¡¼¥Ö¥ëÊÔ½¸¤ÎÄÉ²Ã
      *  01.00.10    NACL-ÌçÏÆ    04/01/14  ¿ÇÎÅ¹Ô°Ù¤ÎÊñ³çÊ¬¤Ï½ü¤¯
      *                                     SRYACCT7¢ª33
      *  01.00.11    NACL-ÌçÏÆ    04/02/27  Ì¤¼Â»Ü¸º»»·îÅÓÃæÊÑ¹¹ÂÐ±þ
      *  01.00.12    NACL-ÌçÏÆ    04/03/17  °¦É²¸©ÃÏÊý¸øÈñ¡§Ê¡»ã¥ì¥»ÂÐ±þ
      *  01.00.13    NACL-ÌçÏÆ    04/03/18  Í­¾²¿ÇÎÅ½ê£±·²Æþ±¡´ðËÜÎÁ£±
      *                                     ¡Ê°å»ÕÇÛÃÖÅ¬¹ç¡Ë¤Î³°Çñ»þ
      *                                     ¤Î´ðËÜÎÁ¼èÆÀÂÐ±þÅù
      *  01.00.14    NACL-ÌçÏÆ    04/04/23  ÁªÄêÆþ±¡»þ¤ÎÂ¾°åÎÅµ¡´Ø¼õ¿Ç¤Î
      *                                     »»Äê½¤Àµ
      *  01.00.15    NACL-ÌçÏÆ    04/08/04  Æ±°ìÊÝ¸±ÈÖ¹æÂÐ±þ
      *  Ãæ»ß½èÍýÄÉ²Ã
      *  01.01.01    NACL-Æ£¸¶    04/10/07  Ãæ»ß½èÍý¤ÎÄÉ²Ã
      *  01.01.02    NACL-¾®Æ¦Âô  04/12/10  Æþ±¡Î¬¾Î¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß¥ï¡¼¥¯
      *                                     ÂÐ±þ
      *  01.01.03    NACL-Æ£¸¶    04/11/01  µ­ºÜ¤Ê¤·¤Î¸øÈñ¤Î½èÍý
      *  01.01.04    NACL-ÌçÏÆ    05/03/03  ¿©»ö¤Ë´Ø¤¹¤ëÊÔ½¸½¤Àµ
      *
      *  02.06.01    NACL-ÌçÏÆ    05/05/16  ÎÅÍÜÃ´Åö¼êÅö¡ÊËÌ³¤Æ»¡ËÂÐ±þ
      *  02.06.02    NACL-ÃÝÅÄ    05/05/30  realtime preview ½èÍýÄÉ²Ã
      *  02.06.03    NACL-¾®Æ¦Âô  05/09/16  °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤Î³°Çñ·×»»¤ò½¤Àµ
      *                                     ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *  02.06.04    NACL-ÌçÏÆ    06/01/05  ¥·¥¹´É¡Ê£µ£°£°£°¡Ë¤Î¿©»ö¾ðÊó¼è
      *                                     ÆÀ½¤Àµ
      *  02.06.05    NACL-ÌçÏÆ    06/01/20  É¸·çÂÐ±þ
      *  02.09.01    NACL-ÌçÏÆ    06/04/13  £²£°£°£¶Ç¯£´·î²þÀµÂÐ±þ
      *  02.09.02    NACL-ÌçÏÆ    06/05/16  Ï·¿ÍÆÃÄêÆþ±¡´ðËÜÎÁ¤Î¿ÇÎÅ¶èÊ¬
      *                                     ÊÔ½¸½¤Àµ
      *  02.09.03    NACL-ÌçÏÆ    06/05/17  Î¬¾ÎÊÔ½¸(190123250Åù)
      *  02.09.04    NACL-ÌçÏÆ    06/05/23  ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ½¤Àµ
      *  03.01.01    NACL-ÌçÏÆ    06/06/28  ¼ÂÆü¿ô¸º»»¥³¡¼¥ÉÂÐ±þ
      *  03.02.01    NACL-ÌçÏÆ    06/08/04  £¹£°Æþ±¡Íó¤ÎÆü¿ôÊÔ½¸½¤Àµ
      *  03.05.01    NACL-¾®Æ¦Âô  07/06/01  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.02    NACL-ÌçÏÆ    07/06/27  ¼ÂÆü¿ô¥Æ¡¼¥Ö¥ëÊÔ½¸½¤Àµ
      *  03.05.03    NACL-ÌçÏÆ    07/07/17  ¸ÄÊÌ»þ¤Î¥·¥¹¥Æ¥à´ÉÍýÊÔ½¸½¤Àµ
      *  04.01.01    NACL-ÌçÏÆ    07/12/19  Æþ±¡ÎÁÌ¤»»ÄêÆüÂÐ±þ
      *  04.01.02    NACL-ÌçÏÆ    08/02/04  É¸·ç³°Çñ·×»»½¤Àµ
      *  04.02.01    NACL-ÌçÏÆ    08/05/08  ÅÀÅ©£³£³£²¤Î¼êµ»ÎÁÊÔ½¸
      *  04.02.02    NACL-ÌçÏÆ    08/06/17  Æþ±¡ÎÁÌ¤»»ÄêÆüÂÐ±þ
      *  04.02.03    NACL-ÌçÏÆ    08/09/24  Æþ±¡Î¬¾ÎÊÔ½¸ÄÉ²Ã
      *  04.03.01    NACL-ÌçÏÆ    09/02/09  ¸øÈñÊ¬ÅÀ¿ôÊÔ½¸½¤Àµ
      *  04.03.02    NACL-ÌçÏÆ    09/03/03  °¦É²¸©ÃÏÊý¸øÈñÂÐ±þ
      *
      *  04.04.01    NACL-ÌçÏÆ    09/01/23  Æþ±¡ÎÁ»»ÄêÊÝ¸±°Ê³°¤Î¿ÇÎÅÊ¬¼ÂÆü¿ô»»ÄêÂÐ±þ
      *  04.04.02    NACL-ÌçÏÆ    09/05/11  °¦É²¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.04.03    NACL-ÌçÏÆ    09/09/04  ¾®Æþ´ÉÎ¬¾Î½¤Àµ
      *  04.04.04    NACL-ÌçÏÆ    10/03/23  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£µ¤ÎÂÐ±þ¡Ë
      *  04.04.05    NACL-ÌçÏÆ    10/04/13  Ê¬ÅÀÊÔ½¸
      *  04.04.06    NACL-ÌçÏÆ    10/05/21  ÉéÃ´¶èÊ¬ÊÔ½¸ÂÐ±þ
      *  04.04.07    NACL-ÌçÏÆ    10/06/01  Â¾°åÎÅµ¡´Ø¼õ¿ÇÂÐ±þ
      *  04.04.08    NACL-ÌçÏÆ    10/08/09  Â¾°å¼õ¿Ç¡Ê³°Çñ£´¡Ë¤ÎÁªÄê»þÂÐ±þ
      *
      *  04.05.01    NACL-ÌçÏÆ    10/01/12  ¼£¸³ÂÐ±þ
      *  04.05.02    NACL-ÌçÏÆ    10/02/08  ÃÏÊý¸øÈñ¤ÎÁ´¹ñ¸øÈñÊ»ÍÑÊ¬ÀÁµáÅÀ
      *                                     ½¸·×ÂÐ±þ
      *  04.05.03    NACL-ÌçÏÆ    10/09/06  ÃÏÊý¸øÈñ¤ÎÁ´¹ñ¸øÈñÊ»ÍÑÊ¬ÀÁµáÅÀ
      *                                     ½¸·×ÂÐ±þ¡Ê¥ì¥»ÅÅÉéÃ´¶èÊ¬ÂÐ±þ¡Ë
      *  04.05.04    NACL-ÌçÏÆ    11/02/25  ÊÝ¸±ÊÌÀÁµá¥Á¥§¥Ã¥¯É½¤ÎËÜÂÎ¼è¹þ¤ß
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE62-FILE.
       01  RECE62-REC. 
           COPY    "CPRCF062.INC".
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
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPERRFL.INC"
                                   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE62                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).
           03  FLG-SHOSAISIN-GEN                       PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-NYUINACT                            PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-RECE62                              PIC 9(01).
           03  FLG-NYUKHN                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-NYURYAKUSHO                         PIC 9(01).
           03  FLG-GAIHAKU                             PIC 9(01).
           03  FLG-MIJISI-GENSAN                       PIC 9(01).       
           03  FLG-MIJISI-INNAI                        PIC 9(01).       
           03  FLG-MIJISI-IRYO                         PIC 9(01).       
           03  FLG-MIJISI-JYOKU                        PIC 9(01).       
           03  FLG-SENTEICDCHG                         PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-NYURYAKUWRK                         PIC 9(01).
           03  FLG-NISSUSET                            PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-TBL-OCC                             OCCURS  2.
               05  CNT-IN                              PIC 9(06).
               05  CNT-MOD                             PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDZ                                     PIC 9(03).
           03  IDX1                                    PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(02).
           03  HC-IDX                                  PIC 9(03).
           03  KH-IDX                                  PIC 9(03).
           03  NYUINACT-IDX                            PIC 9(03).
           03  NYUINACT-IDX2                           PIC 9(03).
           03  ACCT-IDX                                PIC 9(03).
           03  SRYACT-IDX                              PIC 9(03).
           03  ID1                                     PIC 9(03).
           03  GENSAN-IDX                              PIC 9(03).
           03  GENSAN-IDX2                             PIC 9(03).
           03  IDV                                     PIC 9(02).
           03  TENTEK-IDX                              PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
           03  IDX11                                   PIC 9(03).
      *
      *    ¼ÂÆü¿ô¥«¥¦¥ó¥ÈÍÑÎÎ°è
       01  TBL-JNISSU.
           03  TBL-JNISSU-HKN       OCCURS   5.
               05  TBL-JNISSU-DAY                      PIC X(01)
                                                       OCCURS   31.
      *
      *    ¼ÂÆü¿ô¥«¥¦¥ó¥ÈÍÑÎÎ°è£²
       01  TBL-JNISSU2.
           03  TBL-JNISSU2-HKN                         OCCURS   5.
               05  TBL-JNISSU2-MON                     PIC 9(02).
               05  TBL-JNISSU2-DAY                     PIC X(01)
                                                       OCCURS   31.
      *
      *    ¼ÂÆü¿ô¸º¥«¥¦¥ó¥ÈÍÑÎÎ°è
       01  TBL-JNISSU-GEN.
           03  TBL-JNISSU-HKN-GEN   OCCURS   5.
               05  TBL-JNISSU-DAY-GEN                  PIC X(01)
                                                       OCCURS   31.
      *
      *    ¼ÂÆü¿ô¸º¥«¥¦¥ó¥ÈÍÑÎÎ°è¡Ê¤Þ¤È¤á¡Ë
       01  TBL-JNISSU-MATOME.
           03  TBL-JNISSU-HKN-MATOME                   OCCURS   31.
               05  TBL-JNISSU-DAY-M                    PIC X(01).
      *
      *    Ì¤¼Â»Ü¸º»»ÂàÈòÍÑ¥Õ¥é¥°¥Æ¡¼¥Ö¥ë
       01  TBL-GENSAN.
           03  TBL-GENSAN-INF       OCCURS  31.
               05  TBL-INNAI                           PIC X(01).
               05  TBL-IRYO                            PIC X(01).
               05  TBL-JYOKU                           PIC X(01).
      *
      *    Ì¤¼Â»Ü¸º»»ÂàÈòÍÑÅÀ¿ô¥Æ¡¼¥Ö¥ë
       01  TBL-GENSAN2.
           03  TBL-GENSAN2-INF      OCCURS  31.
               05  TBL-MIJISI-TENSU                    PIC 9(09).
      *
      *    °åÎÅµ¡´Ø¡ÊÆþ±¡´ðËÜ¡Ë¥«¥¦¥ó¥È
       01  WRK-SYS5000-COUNT                           PIC 9(02).
      *
      *    ¹ç·×ÅÀ¿ô¡Ê¿ÇÎÅ¹Ô°Ù¡Ë½¸·×ÍÑÎÎ°è
       01  TBL-TOTALTEN.
           03  TBL-TOTALTEN-HKN     OCCURS   5.
               05  TBL-TOTALTEN-ACCT                   PIC S9(08).
      *    ¹ç·×ÅÀ¿ô¡Ê¿ÇÎÅ¹Ô°Ù¡Ë½¸·×ÍÑÎÎ°è
      *    ¡Ê¤Ö¤óÅÀ¡¦¸øÈñÊ¬Î¥¶èÊ¬ÊÔ½¸ÍÑ¡Ë
       01  TBL-TOTALTEN2.
           03  TBL-TOTALTEN2-HKN    OCCURS   5.
               05  TBL-TOTALTEN2-ACCT                  PIC S9(08).
      *
       01  TBL-FUKUSHI.
           03  TBL-FUKUSHI-OCC                         OCCURS  4.
               05  TBL-FUKUSHIFLG                      PIC 9(01).
               05  TBL-FUKUSHI-DAYFLG                  PIC 9(01).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY1-NYUTENTTLKBN               PIC 9(03).
               05  WRK-DAY1-SENTEI                     PIC 9(04).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY41                           PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
               05  WRK-DAY6                            PIC 9(04).
               05  WRK-DAY6-ZAITEN                     PIC 9(08).
               05  WRK-DAY7                            PIC 9(04).
               05  WRK-DAY7-ZAITEN                     PIC 9(08).
               05  WRK-ACCT90                          PIC 9(08).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-IN                                  PIC X(01).
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
           03  WRK-SYS-SRYYMD                          PIC X(08).
           03  WRK-SRYYM-5000                          PIC X(06).
      *
           03  WRK-PREFNUM                             PIC X(02).
      *
           03  WRK-HYOKETU-TEN                         PIC 9(09)V99.
      *
           03  WRK-NYUINF-TABLE.
               05  WRK-NYUINF-TBL                      OCCURS  10.
                   07  WRK-NYUKHNKEI                   PIC 9(09)
                                                           OCCURS 5.
                   07  WRK-NYUTOKKEI                   PIC 9(09)
                                                           OCCURS 5.
               05  WRK-NYUINF-TBL2                     OCCURS   5.
                   07  WRK-NYUKHNKEI2                  PIC S9(09).
                   07  WRK-NYUTOKKEI2                  PIC S9(09).
      *
      *    Æþ±¡Î¬¾ÎÂàÈò¥ï¡¼¥¯
           03  WRK-NYURYAKU-OCC                       OCCURS  200.
               05  WRK-NYURYAKU-SRYCD                 PIC X(09).
               05  WRK-NYURYAKU-YUKOSTYMD             PIC X(09).
               05  WRK-NYURYAKU-YUKOEDYMD             PIC X(09).
               05  WRK-NYURYAKU-DATA                  PIC X(400).
           03  WRK-NYUYAKU-CD                         PIC X(09).
           03  WRK-NYURYAKU-IDX                       PIC 9(03).
           03  WRK-NYURYAKU-MAX                       PIC 9(03).
      *
           03  WRK-NYUINDAY-TANKI2-TBL.
               05  WRK-NYUINDAY-TANKI2-KBN            PIC X(01)
                                                      OCCURS  31.
      *
       01  WRK-TENTEK.
           03  WRK-TENTEK-SYUTEN                       PIC 9(07).
           03  WRK-TENTEK-ZAIKAISU                     PIC 9(04).
           03  WRK-TENTEK-TABLE.
               05  WRK-TENTEK-TBL                      OCCURS  10.
                   07  WRK-TENTEK-YKZTEN               PIC 9(07).
                   07  WRK-TENTEK-KIZAITEN             PIC 9(07).
           03  WRK-TENTEK-FLG-TABLE.
               05  WRK-TENTEK-FLG-TBL                  OCCURS  10.
                   07  WRK-TENTEK-FLG                  PIC 9(01)
                                                           OCCURS 5.
      *
       01  WRK-CONS-AREA.
      *    ¸ÇÄêÃ¼Ëö£É£Ä
           03  WRK-CONS-TERMID     PIC X(64)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *    Æþ±¡Î¬¾ÎÆÉ¤ß¹þ¤ß¥ï¡¼¥¯ÍÑ
           03  WRK-NYURYAKU-CONST-MAX             PIC 9(03) VALUE 200.
      *
           03  WRK-RECEERR                             PIC X(200). 
      *
      *    Æþ±¡ÅÀ¿ô»»ÄêÍÑ¥ï¡¼¥¯¥¨¥ê¥¢
       01  WRK-NYUIN-TENSU-AREA.
           03  WRK-KHNTEN                              PIC 9(08). 
           03  WRK-SENTEI-KHNTEN                       PIC 9(08).
           03  WRK-SENTEI-ZAITEN                       PIC 9(08).
           03  WRK-KSNTEN                              PIC S9(08).
           03  WRK-NYUTENTTLKBN                        PIC 9(03).
           03  WRK-NYUKHNRYO                           PIC 9(09).
           03  WRK-NYUTOKRYO                           PIC 9(09).
           03  WRK-SHOKUJIRYOYO-OCC.
               05  WRK-SHOKUJIRYOYO                    PIC 9(01)
                                                       OCCURS 31.
           03  WRK-TOKUKANRIKASAN-OCC.
               05  WRK-TOKUKANRIKASAN                  PIC 9(01)
                                                       OCCURS 31.
           03  WRK-SRYCD                               PIC X(09).
           03  WRK-RYAKUSHO                            PIC X(14).
           03  WRK-SRYYMD                              PIC X(08).       
           03  WRK-SRYYMD-TAIHI                        PIC X(08).   
           03  WRK-SENTEI-KEISU                        PIC 9V99.    
           03  WRK-GAIHAKU-SRYCD                       PIC X(09).
           03  WRK-GAIHAKU-KHNTEN                      PIC 9(08).
      *
      *
      *    °åÎÅµ¡´Ø¡Ê´ðËÜ¾ðÊó¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *
      *    °åÎÅµ¡´Ø¡ÊÆþ±¡´ðËÜ¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC     OCCURS  100.
               05  TBL-SYS5000                         PIC X(610).
      *
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK5000.INC".
      *
           COPY    "CPSK2010.INC".
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    Æþ±¡²ñ·×
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù
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
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    Æþ±¡Î¬¾Î
       01  NYURYAKUSHO-REC.
           COPY    "CPNYUINRYAKUSHO.INC".
      *
      *    ÁªÄêÆþ±¡ÎÁ
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ¿ô»úÊÑ´¹ÎÎ°è
           COPY    "CPORCSNUM.INC".
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *    Æþ±¡ÅÀÅ©°ì³ç¹¹¿·¥µ¥Ö
           COPY    "CPORCSTENTEK.INC".
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
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
                                       WRK-AREA
                                       SPA-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        £Ä£Â¥ª¡¼¥×¥ó½èÍý
               PERFORM 100-DBOPEN-SEC
      *
      *        ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0630"      TO  JOB-PGID
               MOVE    "¿ÇÎÅ¼ÂÆü¿ôÊÔ½¸"
                                       TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¾ðÊóÊÔ½¸½èÍý
           IF      FLG-END             =   ZERO
               PERFORM 1001-SYSKANRI-HENSYU-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¾ðÊóÊÔ½¸½èÍý
      ******************************************************************
       1001-SYSKANRI-HENSYU-SEC          SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       °åÎÅµ¡´Ø¾ðÊó
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
              MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key10"             TO  MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key10"         TO  MCP-PATHNAME
                  PERFORM  900-SYSKANRI-READ-SEC
      *
                  IF      FLG-SYSKANRI     =   ZERO
                     MOVE   SYS-1001-PREFNUM  TO  WRK-PREFNUM
                  ELSE
                     MOVE  "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                          TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                  END-IF    
              ELSE
                  MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key10"             TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key8"          TO  MCP-PATHNAME
                  PERFORM  900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key8"          TO  MCP-PATHNAME
                    PERFORM  900-SYSKANRI-READ-SEC
      *
                  END-PERFORM
              ELSE
                  MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *
             MOVE     WRK-PARA-SRYYM     TO  WRK-SRYYM-5000
             PERFORM   10012-SYS-5000-CHK-SEC
      *      °åÎÅµ¡´Ø¸º»»¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
             INITIALIZE                   WRK-SHOKUJIRYOYO-OCC
                                          WRK-TOKUKANRIKASAN-OCC
      *
             MOVE     ZERO               TO  FLG-MIJISI-INNAI
                                             FLG-MIJISI-IRYO
                                             FLG-MIJISI-JYOKU
      *
             MOVE     SPACE              TO  TBL-GENSAN
             INITIALIZE                      TBL-GENSAN
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
             MOVE    "01"                TO  SYS-STYUKYMD (7:2)
             MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"  TO  MCP-TABLE
                 MOVE    "key10"         TO  MCP-PATHNAME
                 PERFORM  900-SYSKANRI-READ-SEC
                 IF      FLG-SYSKANRI        =   ZERO
                   PERFORM  10011-SYSKAN-HENSYU-SEC
                   IF        WRK-SYS5000-COUNT    =     ZERO
                      CONTINUE
                   ELSE
                      PERFORM  10013-SYS-5000-GET-SEC
                   END-IF
                 ELSE
                   MOVE "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                         TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                 END-IF
             ELSE
                  MOVE "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                         TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
             END-IF
      *
              MOVE    "tbl_syskanri"     TO  MCP-TABLE
              MOVE    "key10"            TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
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
             MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"  TO  MCP-TABLE
                 MOVE    "key8"          TO  MCP-PATHNAME
                 PERFORM  900-SYSKANRI-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                           OR    (SYSKAN-IDX   >=  100)
                   ADD     1            TO  SYSKAN-IDX
                   MOVE   SYS-5000-REC  TO  TBL-SYS5000(SYSKAN-IDX)
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key8"          TO  MCP-PATHNAME
                   PERFORM  900-SYSKANRI-READ-SEC
                 END-PERFORM
             ELSE
                  MOVE "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                         TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
             END-IF
      *
              MOVE    "tbl_syskanri"     TO  MCP-TABLE
              MOVE    "key8"             TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       1001-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤ËÊ£¿ô¤Î°åÎÅ´ü´Ö¾ðÊó¤¬¤Ê¤¤¤«¸¡º÷¡Ê°åÎÅµ¡´Ø¸º»»¤Î³ÎÇ§¡Ë  *
      ******************************************************************
       10012-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE     ZERO               TO  WRK-SYS5000-COUNT
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1     TO        WRK-SYS5000-COUNT
                   MOVE    "tbl_syskanri"   TO  MCP-TABLE
                   MOVE    "key11"          TO  MCP-PATHNAME
                   PERFORM  900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key11"            TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       10012-SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤Î°åÎÅ´ü´Ö¾ðÊó¼èÆÀ¡Ê°åÎÅµ¡´Ø¸º»»¾ðÊó¡Ë                  *
      ******************************************************************
       10013-SYS-5000-GET-SEC                 SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
                 PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                            UNTIL     GENSAN-IDX   >   4
                  IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"    
                    EVALUATE  GENSAN-IDX
                      WHEN  2
                        MOVE    1     TO    FLG-MIJISI-INNAI    
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  "1"   TO    TBL-INNAI(GENSAN-IDX2)
                        END-PERFORM
                      WHEN  3
                        MOVE    1     TO    FLG-MIJISI-IRYO     
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  "1"   TO    TBL-IRYO(GENSAN-IDX2)
                        END-PERFORM
                      WHEN  4
                        MOVE    1     TO    FLG-MIJISI-JYOKU    
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  "1"   TO    TBL-JYOKU(GENSAN-IDX2)
                        END-PERFORM
                    END-EVALUATE
                  ELSE
                    EVALUATE  GENSAN-IDX
                      WHEN  2
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  SPACE   TO    TBL-INNAI(GENSAN-IDX2)
                        END-PERFORM
                      WHEN  3
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  SPACE   TO    TBL-IRYO(GENSAN-IDX2)
                        END-PERFORM
                      WHEN  4
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  SPACE   TO    TBL-JYOKU(GENSAN-IDX2)
                        END-PERFORM
                    END-EVALUATE
                  END-IF
                 END-PERFORM
      *
                 PERFORM    VARYING  IDX3  FROM  IDV  BY  1
                             UNTIL   IDX3    >   31
                      EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *                Æþ±¡¿©»öÎÅÍÜ¡Ê£±¡Ë
                       WHEN         "197000110"
                          MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX3)
                          EVALUATE  SYS-5000-SHOKUKBN(1)
                            WHEN     "1"
                              MOVE  ZERO  TO   WRK-TOKUKANRIKASAN(IDX3)
                            WHEN     "2"
                              MOVE    1   TO   WRK-TOKUKANRIKASAN(IDX3)
                          END-EVALUATE
      *                Æþ±¡¿©»öÎÅÍÜ¡Ê£²¡Ë
                       WHEN         "197000710"
                          MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX3)
                          MOVE    ZERO    TO   WRK-TOKUKANRIKASAN(IDX3)
                      END-EVALUATE
                 END-PERFORM
      *
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key11"             TO  MCP-PATHNAME
                 PERFORM  900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key11"            TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       10013-SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥«¥ó¤ÎÊÔ½¸
      *****************************************************************
       10011-SYSKAN-HENSYU-SEC                SECTION.
      *
           PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                    UNTIL     GENSAN-IDX   >   4
                IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"    
                    EVALUATE  GENSAN-IDX
                        WHEN  2
                            MOVE    1     TO    FLG-MIJISI-INNAI    
                            PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                              MOVE  "1"   TO    TBL-INNAI(GENSAN-IDX2)
                            END-PERFORM
                        WHEN  3
                            MOVE    1     TO    FLG-MIJISI-IRYO     
                            PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                              MOVE  "1"   TO    TBL-IRYO(GENSAN-IDX2)
                            END-PERFORM
                        WHEN  4
                            MOVE    1     TO    FLG-MIJISI-JYOKU    
                            PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                              MOVE  "1"   TO    TBL-JYOKU(GENSAN-IDX2)
                            END-PERFORM
                    END-EVALUATE
                END-IF
           END-PERFORM
      *
           PERFORM   VARYING   IDX3   FROM  1  BY  1
                      UNTIL    IDX3    >   31
              EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *         Æþ±¡¿©»öÎÅÍÜ¡Ê£±¡Ë
                WHEN         "197000110"
                    MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX3)
                    EVALUATE  SYS-5000-SHOKUKBN(1)
                      WHEN     "1"
                        MOVE  ZERO  TO   WRK-TOKUKANRIKASAN(IDX3)
                      WHEN     "2"
                        MOVE    1   TO   WRK-TOKUKANRIKASAN(IDX3)
                    END-EVALUATE
      *         Æþ±¡¿©»öÎÅÍÜ¡Ê£²¡Ë
                WHEN         "197000710"
                    MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX3)
              END-EVALUATE
           END-PERFORM
      *
           .
       10011-SYSKAN-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       2
                   OR      FLG-END     =   1
      *
               IF      IDZ             =   1               
      *    ¥ì¥»¥×¥È½èÍý
                   MOVE    "RECE620"           TO  RECE62PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE62PARA-TERMID
               ELSE
      *    ÀÁµá½ñ½èÍý
                   MOVE    "RECE62X"           TO  RECE62PARA-FILE-ID
      *->          MOVE    WRK-CONS-TERMID     TO  RECE62PARA-TERMID
                   IF      WRK-PARA-SHELLID    =   "RECEPTX"
                       MOVE    WRK-PARA-TERMID     TO  RECE62PARA-TERMID
                   ELSE
                       MOVE    WRK-CONS-TERMID     TO  RECE62PARA-TERMID
                   END-IF
               END-IF
               MOVE    WRK-PARA-HOSPNUM    TO   RECE62PARA-HOSPNUM
      *
               PERFORM 2001-HENSYU-SEC
               IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    1           TO  FLG-END
               END-IF
           END-PERFORM 
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       2001-HENSYU-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE62
      *
           OPEN    I-O                 RECE62-FILE
      *
           PERFORM    900-RECE62-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE62  =   1
                                       OR      FLG-END     =   1
               PERFORM     20011-HENSYU-SEC
      *
               PERFORM     900-RECE62-READ-SEC
      *
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           CLOSE   RECE62-FILE
           .
       2001-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ÊÔ½¸½èÍý
      *****************************************************************
       20011-HENSYU-SEC                   SECTION.
      *
      *    ÃÏÊý¸øÈñ¥Ç¡¼¥¿¤Î¤È¤­ÉÔÌÀÊ¬¤Ï½èÍý¤ÎÂÐ¾Ý¤È¤·¤Ê¤¤
           IF    ( IDZ                  =   2    )
           AND   ( RECE62-RECESYUBETU   =   ZERO )
               GO  TO  20011-HENSYU-EXT
           END-IF
      *
           MOVE       SPACE             TO     TBL-JNISSU
           INITIALIZE                          TBL-JNISSU
           MOVE       SPACE             TO     TBL-JNISSU2
           INITIALIZE                          TBL-JNISSU2
           MOVE       SPACE             TO     TBL-JNISSU-GEN
           INITIALIZE                          TBL-JNISSU-GEN
           MOVE       SPACE             TO     TBL-JNISSU-MATOME
           INITIALIZE                          TBL-JNISSU-MATOME
      *
           MOVE       SPACE             TO     TBL-TOTALTEN
           INITIALIZE                          TBL-TOTALTEN
           MOVE       SPACE             TO     TBL-TOTALTEN2
           INITIALIZE                          TBL-TOTALTEN2
           MOVE       SPACE             TO     WRK-NYUINF-TABLE
           INITIALIZE                          WRK-NYUINF-TABLE
      *
           MOVE       SPACE             TO     WRK-DAY-TABLE
           INITIALIZE                          WRK-DAY-TABLE
      *
           MOVE       SPACE             TO     TBL-GENSAN2
           INITIALIZE                          TBL-GENSAN2
      *
           MOVE       ZERO              TO     FLG-MIJISI-GENSAN        
      *
           INITIALIZE                          TBL-FUKUSHI
      *
           PERFORM     200118-SYUKEIKBN-HANBETU-SEC
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ
           MOVE       SPACE             TO     WRK-NYUINDAY-TANKI2-TBL
           INITIALIZE                          WRK-NYUINDAY-TANKI2-TBL
           INITIALIZE                   ORCSNYUINDAY-AREA
           MOVE    RECE62-HOSPNUM   TO  LNK-NYUINDAY-HOSPNUM
           MOVE    RECE62-PTID      TO  LNK-NYUINDAY-PTID
           MOVE    RECE62-SRYYM     TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"   USING   ORCSNYUINDAY-AREA
                                            SPA-AREA
           IF      LNK-NYUINDAY-RC     =   ZERO
               MOVE   LNK-NYUINDAY-TBL  TO  RECE62-NYUINDAY-TBL
               MOVE   LNK-NYUINDAY-TANKI2-TBL
                                        TO  WRK-NYUINDAY-TANKI2-TBL
           END-IF
      *
      ***  PERFORM    VARYING    IDX    FROM   1   BY   1
      ***             UNTIL     (IDX      >   10)  OR
      ***                       (RECE62-SRYKA(IDX)   =   SPACE)
               PERFORM    200111-RECE62-HENSYU-SEC
               PERFORM    200112-RECE62-HENSYU2-SEC
               PERFORM    200113-RECE62-HENSYU3-SEC
               IF   RECE62-SRYYM  >= "200604"
                 CONTINUE
               ELSE
                 PERFORM  200114-RECE62-HENSYU4-SEC
               END-IF
               PERFORM    200115-RECE62-HENSYU9-SEC
      ***  END-PERFORM
      *
           PERFORM     200116-JNISSU-SET-SEC
      *
           PERFORM     200117-RECE62-REWRITE-SEC
           .
       20011-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎÊÔ½¸¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       200111-RECE62-HENSYU-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  OR  "2"  OR  
                                                "4"  OR  "5"  OR
                                                "6"  OR  "7"
                       PERFORM    2001111-NYUIN-DAY-TAIHI-SEC
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
                 MOVE    "key7"                TO      MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE    "key7"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                          UNTIL   IDY     >       31
      *
              PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL   IDX1    >       10
                          OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                 IF        WRK-DAY5(IDY)   =   RECE62-HKNCOMBI (IDX1)
                      MOVE    ZERO            TO    FLG-SHOSAISIN
                      IF   WRK-DAY1(IDY)   NOT =    ZERO    
                               MOVE    1      TO    FLG-SHOSAISIN
                      END-IF
      *
                      PERFORM   500-KUMIAWASE-HANBETU-SEC
      *
                      PERFORM   2001112-RECE62-TENSOU11-SEC
                      MOVE    10              TO    IDX1
                 END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key33"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key33"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF     (ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1))
                       AND    (ACCT-ZAIKBN    NOT = 1 AND 2          )
      *
                           PERFORM    500-KUMIAWASE-HANBETU-SEC
      *
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                                MOVE    ZERO     TO    FLG-SHOSAISIN
                                                       FLG-SHOSAISIN-GEN
      ***                       IF   ACCT-SRYKBN  =  "11"  OR
      ***                                            "13"  OR  "14"
      ***                           PERFORM 2001113-SHOSAISIN-HANTEI-SEC
      ***                       END-IF
                                IF   ACCT-SRYKBN  =  "95"  OR  "96"
                                    CONTINUE
                                ELSE
                                    MOVE    1    TO    FLG-SHOSAISIN
                                END-IF
                                PERFORM   2001114-RECE62-TENSOU12-SEC
                           END-IF
                           IF   (ACCT-ZAITEN    =        ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                                MOVE    ZERO     TO    FLG-SHOSAISIN
                                                       FLG-SHOSAISIN-GEN
                                IF   ACCT-SRYKBN  =  "11"  OR  "99" 
                                    PERFORM 2001113-SHOSAISIN-HANTEI-SEC
                                END-IF
                                PERFORM   2001114-RECE62-TENSOU12-SEC
                           END-IF
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                                IF   ACCT-SRYKBN  =  "90" OR "92"   
                                    PERFORM   2001115-NYUINRYO-TAIHI-SEC
                                END-IF
                           END-IF
                           MOVE    10              TO    IDX1
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryacct"     TO  MCP-TABLE
                   MOVE    "key33"           TO  MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"    TO  MCP-TABLE
           MOVE    "key33"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-RECE62-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ²ñ·×¥Õ¥é¥°ÂàÈò½èÍý                                          *
      ******************************************************************
       2001111-NYUIN-DAY-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO 
      *
               IF     NACCT-ZAISKBKBN    =    "1"  
                  IF     NACCT-SRYCDTOTAL    =    099999916
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY1-SENTEI(IDY)
                  ELSE
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY1(IDY) 
                    IF  ( NACCT-SRYKBN     = "90"      )
                    OR  ( NACCT-SRYCDTOTAL = 190799410 )
                    OR  ( NACCT-SRYCDTOTAL = 190799510 )
                       MOVE   903  TO  WRK-DAY1-NYUTENTTLKBN(IDY)       
                    ELSE                                                
                       MOVE   920  TO  WRK-DAY1-NYUTENTTLKBN(IDY)       
                    END-IF
                  END-IF
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "2"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY2(IDY) 
               END-IF 
      *
               IF   RECE62-SRYYM  >= "200604"
                 CONTINUE
               ELSE
                 IF     NACCT-ZAISKBKBN    =    "4"
                   IF    NACCT-SRYCDTOTAL  =    197000570 
                      MOVE   NACCT-DAY(IDY)   TO  WRK-DAY41(IDY)
                   ELSE
                      MOVE   NACCT-DAY(IDY)   TO  WRK-DAY4(IDY)
                   END-IF
                 END-IF
               END-IF
      *
               IF     NACCT-ZAISKBKBN    =    "5"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY5(IDY) 
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "6"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY6(IDY) 
                    IF  NACCT-SRYCDTOTAL  =  190080470   OR             
                                             190799670   OR             
                                             190080570   OR             
                                             190799770   OR             
                                             190111690   OR             
                                             190809090   OR             
                                             190111790   OR             
                                             190809190                  
                      CONTINUE                                          
                    ELSE                                                
                      ADD    NACCT-ZAITEN   TO  WRK-DAY6-ZAITEN(IDY)
                    END-IF
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "7"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY7(IDY) 
                    ADD    NACCT-ZAITEN     TO  WRK-DAY7-ZAITEN(IDY)
               END-IF 
      *
             END-IF
      *
           END-PERFORM.
      *
       2001111-NYUIN-DAY-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2001112-RECE62-TENSOU11-SEC                 SECTION.
      *
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
               IF   FLG-SHOSAISIN        =       1
                       MOVE    "1"   TO   TBL-JNISSU-DAY(1 IDY)
               END-IF
           END-IF
      *
           MOVE       IDX1           TO   HC-IDX
      *
           MOVE       ZERO           TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE   ZERO               TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                   IF   FLG-SHOSAISIN      =    1
                      IF   TBL-FUKUSHI-DAYFLG(IDX2) NOT = ZERO
                         CONTINUE
                      ELSE
                         MOVE    "1"   TO   TBL-JNISSU-DAY(KH-IDX IDY)
                      END-IF
                      MOVE    "1"      TO   TBL-JNISSU2-DAY(KH-IDX IDY)
                   END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       2001112-RECE62-TENSOU11-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Æ¡¼¥Ö¥ë¤ÎÆâÍÆ¤«¤é¿ÇÎÅ¹Ô°Ù¥Æ¡¼¥Ö¥ë¤òÆÉ¤ß¹þ¤ß¡¢
      *    ¿ÇÎÅ¼ÂÆü¿ô¤Î¥«¥¦¥ó¥È¤ò¹Ô¤¨¤ëºÞ¤Ç¤¢¤ë¤«¤ÎÈ½Äê
      *****************************************************************
       2001113-SHOSAISIN-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
      ***  MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)
      ****                                               =   SPACE)  OR
      ****                            (FLG-SHOSAISIN     =    1)
      **               MOVE    SPACE         TO   TNS-TENSU-REC
      **               INITIALIZE                 TNS-TENSU-REC
      **               MOVE    SRY-SRYCD(SRYACT-IDX) TO   TNS-SRYCD
      **               MOVE    ACCT-SRYYM         TO  TNS-YUKOSTYMD(1:6)
      **               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
      **               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
      **               PERFORM  900-TENSU-READ-SEC
      **               IF       FLG-TENSU      =    ZERO
      **                 IF ( (TNS-JITUDAY    =     2)    AND
      **                      (TNS-DAYCNT     =     1   OR   2) ) OR
      **                    (  SRY-SRYCD(SRYACT-IDX) =  "099409901" OR
      **                                                "099999902" OR
      **                                                "099110001" OR
      **                                                "099120001" OR
      **                                                "111011810")
      **                     MOVE    1      TO    FLG-SHOSAISIN
      **                 END-IF
      **                 IF    SRY-SRYCD(SRYACT-IDX) =  "099409903"
      **                     MOVE    1      TO    FLG-SHOSAISIN-GEN
      **                 END-IF
      **               END-IF
                       IF  SRY-SRYCD(SRYACT-IDX) =  "099409901" OR
                                                    "099999902" OR
                                                    "099110001" OR
                                                    "099120001"
                             MOVE    1       TO   FLG-SHOSAISIN
                       END-IF
                       IF  SRY-SRYCD(SRYACT-IDX) =  "099409903"
                             MOVE    1       TO   FLG-SHOSAISIN-GEN
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO   MCP-TABLE
                   MOVE    "key9"            TO   MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO  MCP-TABLE
           MOVE    "key9"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       SHOSAISHIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2001114-RECE62-TENSOU12-SEC                 SECTION.
      *
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
               IF   FLG-SHOSAISIN        =       1
                    PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                               UNTIL    ACCT-IDX   >      31
                           IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                               MOVE    "1"       TO
                                             TBL-JNISSU-DAY(1 ACCT-IDX)
                           END-IF
                    END-PERFORM
               END-IF
               IF   FLG-SHOSAISIN-GEN    =       1
                    PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                               UNTIL    ACCT-IDX   >      31
                           IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                               MOVE    "1"       TO
                                         TBL-JNISSU-DAY-GEN(1 ACCT-IDX)
                           END-IF
                    END-PERFORM
               END-IF
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE       ZERO           TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                   IF   FLG-SHOSAISIN      =    1
                      PERFORM    VARYING  ACCT-IDX   FROM   1   BY   1
                                 UNTIL    ACCT-IDX   >      31
                             IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                               IF   TBL-FUKUSHI-DAYFLG(IDX2) NOT = ZERO
                                 CONTINUE
                               ELSE
                                 MOVE    "1"       TO
                                      TBL-JNISSU-DAY(KH-IDX ACCT-IDX)
                               END-IF
                               MOVE    "1"       TO
                                      TBL-JNISSU2-DAY(KH-IDX ACCT-IDX)
                             END-IF
                      END-PERFORM
                   END-IF
                   IF   FLG-SHOSAISIN-GEN  =    1
                      PERFORM    VARYING  ACCT-IDX   FROM   1   BY   1
                                 UNTIL    ACCT-IDX   >      31
                             IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                                 MOVE    "1"       TO
                                  TBL-JNISSU-DAY-GEN(KH-IDX ACCT-IDX)
                             END-IF
                      END-PERFORM
                   END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       2001114-RECE62-TENSOU12-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¤ÎÆþ±¡ÎÁÅù¤ÎÂàÈò½èÍý
      *****************************************************************
       2001115-NYUINRYO-TAIHI-SEC                 SECTION.
      *
           IF       (ACCT-SRYCDTOTAL    =    190080470
                                        OR   190799670
                                        OR   190080570
                                        OR   190799770
                                        OR   190111690
                                        OR   190809090
                                        OR   190111790
                                        OR   190809190)
           OR       (ACCT-ZAIKBN        =    2        )
                CONTINUE
           ELSE
                PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                                 UNTIL    ACCT-IDX   >      31
                   IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                         COMPUTE  WRK-ACCT90(ACCT-IDX) = 
                                  WRK-ACCT90(ACCT-IDX) +
                                 (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                   END-IF
                END-PERFORM
           END-IF
      *
           .
       2001115-NYUINRYO-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎÊÔ½¸¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       200112-RECE62-HENSYU2-SEC                SECTION.
      *
           PERFORM      2001121-MIJISI-GET-SEC
      *
           PERFORM      2001122-SENTEI-KEISU-GET-SEC
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN          =    "1"  
                 AND    NACCT-SRYCDTOTAL   NOT   =    099999916
      *
      *            Æþ±¡´ðËÜÎÁ¤È²Ã¸º»»ÅÀ¿ô¤Î¼èÆÀ
                   PERFORM   2001123-NYUINKHN-GET-SEC
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      MOVE     ZERO    TO   FLG-NISSUSET
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                         IF  WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                              IF   NACCT-ZAITEN    NOT =    ZERO  
                              AND  NACCT-DAY(IDY)  NOT =    ZERO  
                                   PERFORM 500-KUMIAWASE-HANBETU-SEC
                                   PERFORM 2001124-RECE62-TENSOU21-SEC
                              END-IF
                              MOVE    10        TO     IDX1
                         END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN    =    "6"  
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                            UNTIL   IDX1    >       10
                            OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                       IF   (WRK-DAY1(IDY)   NOT  =  ZERO  )
                       OR   (RECE62-NYUINDAY-KBN(IDY) = "1")
                         IF   WRK-DAY5(IDY)   =  RECE62-HKNCOMBI (IDX1)
                              IF   (NACCT-ZAITEN    NOT =    ZERO)  
                              AND  (NACCT-DAY(IDY)  NOT =    ZERO)  
                              AND  (WRK-DAY2(IDY)  =    ZERO  OR  4
                                                              OR  5)
                                   PERFORM 500-KUMIAWASE-HANBETU-SEC
                                   PERFORM 2001125-RECE62-TENSOU22-SEC
                              END-IF
                              MOVE    10   TO     IDX1
                         END-IF
                       ELSE
                         MOVE    10        TO     IDX1
                       END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key7"              TO  MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key33"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key33"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1)
                           IF   (ACCT-ZAITEN    NOT =    ZERO)
                           AND  (ACCT-ZAIKAISU  NOT =    ZERO)
                                IF   ACCT-SRYKBN  =  "90" OR "92"  
                                    PERFORM 500-KUMIAWASE-HANBETU-SEC
                                    PERFORM 2001126-RECE62-TENSOU23-SEC
                                END-IF
                           END-IF
                           MOVE    10            TO    IDX1
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryacct"     TO   MCP-TABLE
                   MOVE    "key33"           TO   MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200112-RECE62-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ì¤¼Â»Ü¸º»»³Û¼èÆÀ
      *****************************************************************
       2001121-MIJISI-GET-SEC                 SECTION.
      *
           IF   FLG-MIJISI-INNAI  =   1
      *
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
      ***      IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190080570"   TO   TNS-SRYCD
      ***      ELSE
      ***           MOVE    "190799770"   TO   TNS-SRYCD
      ***      END-IF
               MOVE    RECE62-SRYYM       TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
               PERFORM  900-TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                  PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                     IF  TBL-INNAI(GENSAN-IDX2) =  "1"
                          ADD   TNS-TEN   TO    
                                    TBL-MIJISI-TENSU(GENSAN-IDX2)
                     END-IF
                  END-PERFORM
               END-IF
      *
           END-IF
      *
           IF   FLG-MIJISI-IRYO   =   1
      *
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
      ***      IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190111690"   TO   TNS-SRYCD
      ***      ELSE
      ***           MOVE    "190809090"   TO   TNS-SRYCD
      ***      END-IF
               MOVE    RECE62-SRYYM       TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
               PERFORM  900-TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                  PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                     IF  TBL-IRYO(GENSAN-IDX2) =  "1"
                          ADD   TNS-TEN   TO    
                                    TBL-MIJISI-TENSU(GENSAN-IDX2)
                     END-IF
                  END-PERFORM
               END-IF
           END-IF
      *
           IF   FLG-MIJISI-JYOKU  =   1
      *
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
      ***      IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190111790"   TO   TNS-SRYCD
      ***      ELSE
      ***           MOVE    "190809190"   TO   TNS-SRYCD
      ***      END-IF
               MOVE    RECE62-SRYYM       TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
               PERFORM  900-TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                  PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                     IF  TBL-JYOKU(GENSAN-IDX2) =  "1"
                          ADD   TNS-TEN   TO    
                                    TBL-MIJISI-TENSU(GENSAN-IDX2)
                     END-IF
                  END-PERFORM
               END-IF
      *
           END-IF
      *
           .
       2001121-MIJISI-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    ÁªÄê³°Çñ»þ¤Î·¸¿ô¼èÆÀ
      **************************************************************
       2001122-SENTEI-KEISU-GET-SEC           SECTION.
      *
           MOVE      ZERO           TO     WRK-SENTEI-KEISU
      *
           IF   RECE62-SRYYM    <=    "200303"
               MOVE   0.1           TO     WRK-SENTEI-KEISU
           END-IF
      *
           IF ( RECE62-SRYYM    >=    "200304" )   AND
              ( RECE62-SRYYM    <=    "200403" )
               MOVE   0.05           TO    WRK-SENTEI-KEISU
           END-IF
      *
           IF   RECE62-SRYYM    >=    "200404"
               MOVE   ZERO          TO     WRK-SENTEI-KEISU
           END-IF
      *
           .
      *
       2001122-SENTEI-KEISU-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2001124-RECE62-TENSOU21-SEC                 SECTION.
      *
      *    Æþ±¡´ðËÜÎÁ¤È²Ã¸º»»ÅÀ¿ô¤Î·×»»
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
                      MOVE    1          TO        KH-IDX
                      PERFORM      20011241-NYUINACCT-GET-SEC
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE        ZERO          TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                      PERFORM      20011241-NYUINACCT-GET-SEC
                 END-IF
               END-IF  
           END-PERFORM
           .
       2001124-RECE62-TENSOU21-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡´ðËÜÎÁ¤È²Ã¸º»»ÅÀ¿ô¤Î·×»»
      **************************************************************
       20011241-NYUINACCT-GET-SEC           SECTION.
      *
      *    Æþ±¡ÎÁ¤È³°Çñ¤òÈæ³Ó¤·¤ÆÅÀ¿ô»»Äê
      *
           EVALUATE   WRK-NYUTENTTLKBN
               WHEN   903
                  MOVE      ZERO   TO  WRK-NYUKHNRYO
                  IF    WRK-DAY1-SENTEI(IDY)   NOT  =  ZERO
                    EVALUATE   WRK-DAY2(IDY)
                       WHEN   ZERO
                         MOVE     WRK-SENTEI-ZAITEN  TO  
                                       WRK-NYUKHNRYO
                       WHEN   1
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                       WHEN   3
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                       WHEN   4
                        IF   RECE62-SRYYM    >=    "201004"
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-SENTEI-KHNTEN  *  0.3)
                                                     +  0.5
                         COMPUTE  WRK-NYUKHNRYO =
                                  WRK-NYUKHNRYO +
                                  WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUKHNRYO  =
                                  (WRK-NYUKHNRYO  -
                                   (WRK-NYUKHNRYO *
                                    (WRK-HYOKETU-TEN / 100)))
                                                  + 0.5
                         END-IF
                        ELSE
                         COMPUTE  WRK-NYUKHNRYO =
      ****                       (WRK-KHNTEN         *  0.15)
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                        END-IF
                       WHEN   5
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-SENTEI-KHNTEN  *  0.7)
                                                     +  0.5
                         COMPUTE  WRK-NYUKHNRYO =
                                  WRK-NYUKHNRYO +
                                  WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUKHNRYO  =
                                  (WRK-NYUKHNRYO  -
                                   (WRK-NYUKHNRYO *
                                    (WRK-HYOKETU-TEN / 100)))
                                                  + 0.5
                         END-IF
                    END-EVALUATE
                  ELSE
                    EVALUATE   WRK-DAY2(IDY)
                       WHEN   ZERO
                         MOVE     NACCT-ZAITEN   TO  
                                       WRK-NYUKHNRYO
                       WHEN   1
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN    *  0.15)
                                                +  0.5
                       WHEN   2
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN    *  0.3)
                                                +  0.5
                       WHEN   4
                         IF   RECE62-SRYYM    >=    "200404"
                            COMPUTE  WRK-NYUKHNRYO =
                                    (WRK-KHNTEN    *  0.3)
                                                   +  0.5
                         ELSE
                            COMPUTE  WRK-NYUKHNRYO =
                                    (WRK-KHNTEN    *  0.15)
                                                   +  0.5
                         END-IF
                         COMPUTE  WRK-NYUKHNRYO =
                                  WRK-NYUKHNRYO +
                                  WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUKHNRYO  =
                                  (WRK-NYUKHNRYO  -
                                   (WRK-NYUKHNRYO *
                                    (WRK-HYOKETU-TEN / 100)))
                                                  + 0.5
                         END-IF
                       WHEN   5
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN    *  0.7)
                                                +  0.5
                         COMPUTE  WRK-NYUKHNRYO =
                                  WRK-NYUKHNRYO +
                                  WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUKHNRYO  =
                                  (WRK-NYUKHNRYO  -
                                   (WRK-NYUKHNRYO *
                                    (WRK-HYOKETU-TEN / 100)))
                                                  + 0.5
                         END-IF
                    END-EVALUATE
                  END-IF
                  EVALUATE   WRK-DAY2(IDY)
                     WHEN   ZERO
                         IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                         AND    KH-IDX                >   1
                            CONTINUE
                         ELSE
                            COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                                     WRK-NYUTOKKEI2(KH-IDX)  -
                                     TBL-MIJISI-TENSU(IDY)
                         END-IF
                         COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                                  RECE62-NYUTOKKEI2(KH-IDX)  -
                                  TBL-MIJISI-TENSU(IDY)
                         MOVE     1      TO    FLG-MIJISI-GENSAN        
                  END-EVALUATE
                  IF   WRK-NYUKHNRYO  NOT = ZERO
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                                UNTIL   ID1                   > 10
                                  OR    RECE62-NYUKHNNISSU(ID1)
                                                              = ZERO
                                  OR    FLG-ARI               = 1
                      IF  RECE62-NYUKHNRYO(ID1)  =  WRK-NYUKHNRYO
      ****                  IF   KH-IDX =  1
      ****                  OR   RECE62-HKNNUM  =  SPACE
                            IF   FLG-NISSUSET   =  ZERO
                              ADD   1   TO   RECE62-NYUKHNNISSU(ID1)
                              MOVE  1   TO   FLG-NISSUSET
                            END-IF
                            IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                            AND    KH-IDX                >   1
                               CONTINUE
                            ELSE
                               ADD   WRK-NYUKHNRYO
                                      TO   WRK-NYUKHNKEI(ID1 KH-IDX)
                            END-IF
                            ADD   WRK-NYUKHNRYO
                                      TO   RECE62-NYUKHNKEI(ID1 KH-IDX)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE62-NYUKHNNISSU(ID1)
                      MOVE       1           TO  FLG-NISSUSET
                      MOVE   WRK-NYUKHNRYO   TO  RECE62-NYUKHNRYO(ID1)
                      IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                      AND    KH-IDX                >   1
                         CONTINUE
                      ELSE
                         ADD    WRK-NYUKHNRYO   
                                       TO  WRK-NYUKHNKEI(ID1 KH-IDX)
                      END-IF
                      ADD    WRK-NYUKHNRYO   
                                       TO  RECE62-NYUKHNKEI(ID1 KH-IDX)
                    END-IF
                  END-IF
               WHEN   920
                  MOVE      ZERO   TO  WRK-NYUTOKRYO
                  MOVE      ZERO   TO  FLG-GAIHAKU
                  IF    WRK-DAY1-SENTEI(IDY)   NOT  =  ZERO
                    EVALUATE   WRK-DAY2(IDY)
                       WHEN   ZERO
                         MOVE     WRK-SENTEI-ZAITEN   TO  
                                       WRK-NYUTOKRYO
                       WHEN   1
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   3
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   4
                        IF   RECE62-SRYYM    >=    "201004"
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-SENTEI-KHNTEN  *  0.3)
                                                     +  0.5
                         COMPUTE  WRK-NYUTOKRYO =
                                  WRK-NYUTOKRYO +
                                  WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUTOKRYO  =
                                  (WRK-NYUTOKRYO  -
                                   (WRK-NYUTOKRYO *
                                    (WRK-HYOKETU-TEN / 100)))
                                                  + 0.5
                         END-IF
                         MOVE   1   TO   FLG-GAIHAKU
                        ELSE
                         COMPUTE  WRK-NYUTOKRYO =
      ****                       (WRK-KHNTEN         *  0.15)
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                        END-IF
                       WHEN   5
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-SENTEI-KHNTEN  *  0.7)
                                                     +  0.5
                         COMPUTE  WRK-NYUTOKRYO =
                                  WRK-NYUTOKRYO +
                                  WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUTOKRYO  =
                                  (WRK-NYUTOKRYO  -
                                   (WRK-NYUTOKRYO *
                                    (WRK-HYOKETU-TEN / 100)))
                                                  + 0.5
                         END-IF
                         MOVE   1   TO   FLG-GAIHAKU
                    END-EVALUATE
                  ELSE
                    EVALUATE   WRK-DAY2(IDY)
                       WHEN   ZERO
                         MOVE     NACCT-ZAITEN   TO  
                                       WRK-NYUTOKRYO
                       WHEN   1
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN    *  0.15)
                                                +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   2
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN    *  0.3)
                                                +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   4
                         IF   RECE62-SRYYM    >=    "200404"
                            COMPUTE  WRK-NYUTOKRYO =
                                    (WRK-KHNTEN    *  0.3)
                                                   +  0.5
                         ELSE
                            COMPUTE  WRK-NYUTOKRYO =
                                    (WRK-KHNTEN    *  0.15)
                                                   +  0.5
                         END-IF
                         COMPUTE  WRK-NYUTOKRYO =
                                  WRK-NYUTOKRYO +
                                  WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUTOKRYO  =
                                  (WRK-NYUTOKRYO  -
                                   (WRK-NYUTOKRYO *
                                    (WRK-HYOKETU-TEN / 100)))
                                                  + 0.5
                         END-IF
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   5
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN    *  0.7)
                                                +  0.5
                         COMPUTE  WRK-NYUTOKRYO =
                                  WRK-NYUTOKRYO +
                                  WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUTOKRYO  =
                                  (WRK-NYUTOKRYO  -
                                   (WRK-NYUTOKRYO *
                                    (WRK-HYOKETU-TEN / 100)))
                                                  + 0.5
                         END-IF
                         MOVE   1   TO   FLG-GAIHAKU
                    END-EVALUATE
                  END-IF
                  EVALUATE   WRK-DAY2(IDY)
                     WHEN   ZERO
                         IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                         AND    KH-IDX                >   1
                            CONTINUE
                         ELSE
                            COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                                     WRK-NYUTOKKEI2(KH-IDX)  -
                                     TBL-MIJISI-TENSU(IDY)
                         END-IF
                         COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                                  RECE62-NYUTOKKEI2(KH-IDX)  -
                                  TBL-MIJISI-TENSU(IDY)
                         MOVE     1      TO    FLG-MIJISI-GENSAN        
                  END-EVALUATE
                  IF  (WRK-DAY6(IDY)          NOT =  ZERO)
                  AND (WRK-DAY6-ZAITEN(IDY)   NOT =  ZERO)
                  AND (WRK-DAY2(IDY)              =  ZERO  OR  4
                                                           OR  5)
                      COMPUTE  WRK-NYUTOKRYO = WRK-NYUTOKRYO
                                             + WRK-DAY6-ZAITEN(IDY)
                  END-IF
                  IF  WRK-ACCT90(IDY)        NOT =  ZERO
                      COMPUTE  WRK-NYUTOKRYO = WRK-NYUTOKRYO
                                             + WRK-ACCT90(IDY)
                  END-IF
                  IF   WRK-NYUTOKRYO  NOT = ZERO
                   IF     FLG-GAIHAKU  =  ZERO
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 10    )  
                          OR  ( RECE62-NYUTOKNISSU(ID1)      = ZERO  )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE62-NYUTOKRYO-RYAKU(ID1)  =  WRK-RYAKUSHO
      ****                  IF   KH-IDX =  1
      ****                  OR   RECE62-HKNNUM  =  SPACE
                            IF   FLG-NISSUSET   =  ZERO
                              ADD   1   TO   RECE62-NYUTOKNISSU(ID1)
                              MOVE  1   TO   FLG-NISSUSET
                            END-IF
                            IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                            AND    KH-IDX                >   1
                               CONTINUE
                            ELSE
                               ADD   WRK-NYUTOKRYO
                                      TO   WRK-NYUTOKKEI(ID1 KH-IDX)
                            END-IF
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE62-NYUTOKKEI(ID1 KH-IDX)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE62-NYUTOKNISSU(ID1)
                      MOVE       1           TO  FLG-NISSUSET
                      MOVE   WRK-RYAKUSHO    TO  RECE62-NYUTOKRYO-RYAKU
                                                                   (ID1)
      ***             Î¬¾Î¤¬¶õÇò¤Î¾ì¹ç¤Î°Ù¤Î½èÃÖ
                      MOVE   999999999       TO  RECE62-NYUTOKRYO(ID1)
      *
                      IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                      AND    KH-IDX                >   1
                         CONTINUE
                      ELSE
                         ADD    WRK-NYUTOKRYO   
                                       TO  WRK-NYUTOKKEI(ID1 KH-IDX)
                      END-IF
                      ADD    WRK-NYUTOKRYO   
                                       TO  RECE62-NYUTOKKEI(ID1 KH-IDX)
                    END-IF
                   ELSE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 10    )  
                          OR  ( RECE62-NYUTOKNISSU(ID1)      = ZERO  )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE62-NYUTOKRYO(ID1)  =  WRK-NYUTOKRYO
      ****                  IF   KH-IDX =  1
      ****                  OR   RECE62-HKNNUM  =  SPACE
                            IF   FLG-NISSUSET   =  ZERO
                              ADD   1   TO   RECE62-NYUTOKNISSU(ID1)
                              MOVE  1   TO   FLG-NISSUSET
                            END-IF
                            IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                            AND    KH-IDX                >   1
                               CONTINUE
                            ELSE
                               ADD   WRK-NYUTOKRYO
                                      TO   WRK-NYUTOKKEI(ID1 KH-IDX)
                            END-IF
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE62-NYUTOKKEI(ID1 KH-IDX)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE62-NYUTOKNISSU(ID1)
                      MOVE       1           TO  FLG-NISSUSET
      ***             ÅÀ¿ô¤¬¥¼¥í¤Î¾ì¹ç¤Î°Ù¤Î½èÃÖ
                      MOVE   "£¹£¹£¹£¹£¹£¹£¹"
                                             TO  RECE62-NYUTOKRYO-RYAKU
                                                                   (ID1)
      *
                      MOVE   WRK-NYUTOKRYO   TO  RECE62-NYUTOKRYO(ID1)
                      IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                      AND    KH-IDX                >   1
                         CONTINUE
                      ELSE
                         ADD    WRK-NYUTOKRYO   
                                       TO  WRK-NYUTOKKEI(ID1 KH-IDX)
                      END-IF
                      ADD    WRK-NYUTOKRYO   
                                       TO  RECE62-NYUTOKKEI(ID1 KH-IDX)
                    END-IF
                   END-IF
                  END-IF
           END-EVALUATE
      *
           .
       20011241-NYUINACCT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2001125-RECE62-TENSOU22-SEC                 SECTION.
      *
      *    Æþ±¡´ðËÜÎÁ²Ã¸º»»ÅÀ¿ô¤Î·×»»
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
                      MOVE    1          TO        KH-IDX
                      PERFORM      20011251-NYUINACCT-GET2-SEC
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE        ZERO          TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                      PERFORM      20011251-NYUINACCT-GET2-SEC
                 END-IF
               END-IF  
           END-PERFORM
           .
       2001125-RECE62-TENSOU22-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡´ðËÜÎÁ²Ã¸º»»ÅÀ¿ô¤Î·×»»
      **************************************************************
       20011251-NYUINACCT-GET2-SEC           SECTION.
      *
           IF  NACCT-SRYCDTOTAL       =  190080470   OR                 
                                         190799670   OR                 
                                         190080570   OR                 
                                         190799770   OR                 
                                         190111690   OR                 
                                         190809090   OR                 
                                         190111790   OR                 
                                         190809190                      
              CONTINUE                                                  
           ELSE                                                         
      *
              EVALUATE   WRK-DAY1-NYUTENTTLKBN(IDY)
                 WHEN   903
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUKHNKEI2(KH-IDX)  =
                                WRK-NYUKHNKEI2(KH-IDX)  +  NACCT-ZAITEN
                    END-IF
                    COMPUTE  RECE62-NYUKHNKEI2(KH-IDX)  =
                             RECE62-NYUKHNKEI2(KH-IDX)  +  NACCT-ZAITEN
              END-EVALUATE
      *
           END-IF
      *
           IF RECE62-NYUINDAY-KBN(IDY) = "1"
      ***      IF  WRK-NYUINDAY-TANKI2-KBN(IDY) NOT = SPACE
                 IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                 AND    KH-IDX                >   1
                    CONTINUE
                 ELSE
                    COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                             WRK-NYUTOKKEI2(KH-IDX)  +  NACCT-ZAITEN
                 END-IF
                 COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                          RECE62-NYUTOKKEI2(KH-IDX)  +  NACCT-ZAITEN
      ***      ELSE
      ***        IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
      ***        AND    KH-IDX                >   1
      ***           CONTINUE
      ***        ELSE
      ***           COMPUTE  WRK-NYUKHNKEI2(KH-IDX)  =
      ***                    WRK-NYUKHNKEI2(KH-IDX)  +  NACCT-ZAITEN
      ***        END-IF
      ***        COMPUTE  RECE62-NYUKHNKEI2(KH-IDX)  =
      ***                 RECE62-NYUKHNKEI2(KH-IDX)  +  NACCT-ZAITEN
      ***      END-IF
           END-IF
      *
           .
       20011251-NYUINACCT-GET2-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡´ðËÜÎÁ¤È²Ã¸º»»ÅÀ¿ô¤Î¼èÆÀ
      **************************************************************
       2001123-NYUINKHN-GET-SEC           SECTION.
      *
           IF  ( NACCT-SRYKBN     = "90"      )
           OR  ( NACCT-SRYCDTOTAL = 190799410 )
           OR  ( NACCT-SRYCDTOTAL = 190799510 )
                 MOVE   903   TO  WRK-NYUTENTTLKBN                      
           ELSE                                                         
                 MOVE   920   TO  WRK-NYUTENTTLKBN                      
           END-IF                                                       
      *
           MOVE    ZERO               TO   WRK-KHNTEN
           MOVE    SPACE              TO   WRK-RYAKUSHO
           MOVE    ZERO               TO   NYUINACT-IDX2
           MOVE    ZERO               TO   WRK-KSNTEN
           MOVE    ZERO               TO   WRK-SENTEI-KHNTEN
           MOVE    ZERO               TO   WRK-SENTEI-ZAITEN
           MOVE    SPACE              TO   WRK-GAIHAKU-SRYCD
           MOVE    ZERO               TO   WRK-GAIHAKU-KHNTEN
           MOVE    ZERO               TO   WRK-HYOKETU-TEN
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =    ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM 900-NYUINACT-READ-SEC
           ELSE
               MOVE    1              TO   FLG-NYUINACT
           END-IF
      *
           IF      FLG-NYUINACT        =   ZERO
      *
               MOVE      ZERO          TO  FLG-NYUKHN  
               PERFORM   VARYING  NYUINACT-IDX  FROM 1  BY  1
                 UNTIL  (FLG-NYUKHN         =   1)    OR
                        (NYUINACT-IDX       >   5)    OR
                        (NSRY-SRYCD(NYUINACT-IDX)  =  SPACE)
                     MOVE    SPACE         TO   TNS-TENSU-REC
                     INITIALIZE                 TNS-TENSU-REC
                     MOVE    NSRY-SRYCD(NYUINACT-IDX) TO   TNS-SRYCD
                     MOVE    NACCT-SRYYM        TO  TNS-YUKOSTYMD(1:6)
                     MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
                     MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
                     PERFORM  900-TENSU-READ-SEC
                     IF       FLG-TENSU      =    ZERO
                         IF   TNS-DATAKBN    =    1
                              MOVE   TNS-TEN   TO   WRK-KHNTEN
      *                       ½é´ü²Ã»»¡ÊµÚ¤ÓÄ¹´ü¸º»»¡Ë¤ÎÅÀ¿ô¼èÆÀ
      ***                     COMPUTE   WRK-KSNTEN  =  NACCT-ZAITEN
      ***                                           -  WRK-KHNTEN
      *                       ´Ç¸îÈæÎ¨´ð½àÌ¤Ã£»þ¤ÎÆþ±¡´ðËÜÅÀ¿ô¤Î¼èÆÀ
                              PERFORM  20011231-GAIHAKU-NYUINKHN-SET-SEC
                              IF   WRK-GAIHAKU-KHNTEN  NOT =  ZERO
                                 MOVE   WRK-GAIHAKU-KHNTEN
                                               TO   WRK-KHNTEN
                              END-IF
                              MOVE   NYUINACT-IDX
                                               TO   NYUINACT-IDX2
      ***                     MOVE   1         TO   FLG-NYUKHN
                         ELSE
                              EVALUATE  TNS-TENSIKIBETU
                                WHEN     3
                                    MOVE  TNS-TEN  TO  WRK-KSNTEN
                                WHEN     8
                                    COMPUTE  WRK-KSNTEN = TNS-TEN * -1
                                WHEN     6
                                    MOVE  TNS-TEN  TO  WRK-HYOKETU-TEN
                              END-EVALUATE
                         END-IF
                     END-IF                  
               END-PERFORM
      *
               IF    WRK-NYUTENTTLKBN     =   920  
               AND   NYUINACT-IDX2    NOT =   ZERO
      *
                 MOVE    SPACE         TO   NYURYAKUSHO-REC
                 INITIALIZE                 NYURYAKUSHO-REC
                 MOVE    NSRY-SRYCD(NYUINACT-IDX2) 
                                       TO   NYURYAKUSHO-SRYCD
                 MOVE    NACCT-SRYYM   TO   NYURYAKUSHO-YUKOSTYMD(1:6)
                 MOVE    "01"          TO   NYURYAKUSHO-YUKOSTYMD(7:2)
                 MOVE    NYURYAKUSHO-YUKOSTYMD
                                       TO   NYURYAKUSHO-YUKOEDYMD
                 PERFORM  900-NYURYAKUSHO-READ-SEC
                 IF       FLG-NYURYAKUSHO    =    ZERO
                   MOVE     NYURYAKUSHO-RYAKUSHO
                                       TO  WRK-RYAKUSHO
      *            Àº¿À²ÊµßµÞÆþ±¡ÎÁ£±¤ÎÎ¬¾Îµ­ºÜ
                   IF    (RECE62-SRYYM              >=  "200804")  AND
                         (NSRY-SRYCD(NYUINACT-IDX2)  =  "190121010" OR
                                                        "190111410")
                       MOVE   "Àºµß£±" TO  WRK-RYAKUSHO
                   END-IF
                 ELSE
      *            Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²¤ÎÎ¬¾Îµ­ºÜ
                   IF    (RECE62-SRYYM              >=  "200804")  AND
                         (NSRY-SRYCD(NYUINACT-IDX2)  =  "190130010" OR
                                                        "190130110")
                       MOVE   "Àºµß£²" TO  WRK-RYAKUSHO
                   END-IF
                 END-IF                  
      *
               END-IF 
      *
      *        ÁªÄêÆþ±¡ÎÁ¤Î»»Äê
      *
               IF    NYUINACT-IDX2          NOT   =   ZERO
      *
                 MOVE    SPACE                TO  SENTEICDCHG-REC
                 INITIALIZE                       SENTEICDCHG-REC
                 MOVE    NSRY-SRYCD(NYUINACT-IDX2)
                                              TO  SENCHG-IPNSRYCD
                 MOVE    SENTEICDCHG-REC      TO  MCPDATA-REC
                 MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
                 MOVE    "key2"              TO  MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF      MCP-RC             =    ZERO
                     MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
                     MOVE    "key2"              TO  MCP-PATHNAME
                     PERFORM  900-SENTEICDCHG-READ-SEC
                 ELSE
                     MOVE    1                TO  FLG-SENTEICDCHG
                 END-IF
      *
                 IF  FLG-SENTEICDCHG   =   ZERO
      *
                    MOVE    SPACE               TO    TNS-TENSU-REC
                    INITIALIZE                        TNS-TENSU-REC
                    MOVE    SENCHG-SENTEISRYCD  TO    TNS-SRYCD
                    MOVE    NACCT-SRYYM         TO    TNS-YUKOSTYMD(1:6)
                    MOVE    "01"                TO    TNS-YUKOSTYMD(7:2)
                    MOVE    TNS-YUKOSTYMD       TO    TNS-YUKOEDYMD
                    PERFORM  900-TENSU-READ-SEC
                    IF       FLG-TENSU      =    ZERO
                       MOVE     TNS-TEN         TO    WRK-SENTEI-KHNTEN
                       COMPUTE  WRK-SENTEI-ZAITEN  =  WRK-SENTEI-KHNTEN
                                                   +  WRK-KSNTEN
                       IF     WRK-HYOKETU-TEN    NOT =   ZERO
      *                  É¸·ç¡§¡ó¸º»»
                         COMPUTE    WRK-SENTEI-ZAITEN  =
                                  ( WRK-SENTEI-ZAITEN  -
                                   (WRK-SENTEI-ZAITEN  *
                                    (WRK-HYOKETU-TEN   /  100)))  +  0.5
                       END-IF
                    END-IF  
      *
                 END-IF
      *
                 MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
                 MOVE    "key2"              TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
      *
               END-IF
      *
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      ***  IF     WRK-HYOKETU-TEN    NOT =   ZERO
      *      É¸·ç¡§¡ó¸º»»
      ***    COMPUTE    WRK-KHNTEN  =
      ***             ( WRK-KHNTEN  -
      ***              (WRK-KHNTEN  *
      ***               (WRK-HYOKETU-TEN   /  100)))  +  0.5
      ***  END-IF
      *
           .
       2001123-NYUINKHN-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ
      **************************************************************
       20011231-GAIHAKU-NYUINKHN-SET-SEC           SECTION.
      *
           EVALUATE  TNS-SRYCD
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
           IF   (RECE62-SRYYM      >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
               MOVE    WRK-GAIHAKU-SRYCD  TO  TNS-SRYCD
               MOVE    NACCT-SRYYM        TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
               PERFORM  900-TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                        MOVE   TNS-TEN    TO  WRK-GAIHAKU-KHNTEN
               END-IF                  
           END-IF
      *
           .
       20011231-GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2001126-RECE62-TENSOU23-SEC                 SECTION.
      *
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
                 MOVE    1          TO        KH-IDX
                 PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                            UNTIL    ACCT-IDX   >      31
                        IF  ((WRK-DAY1(ACCT-IDX)   NOT =  ZERO   ) OR
                             (RECE62-NYUINDAY-KBN(ACCT-IDX) = "1"))
                        AND ( WRK-DAY5(ACCT-IDX) =
                                           RECE62-HKNCOMBI (IDX1) )
                        AND ( ACCT-DAY(1 ACCT-IDX) NOT =  ZERO    )
                            PERFORM      20011261-SRYACCT-GET-SEC
                        END-IF
                 END-PERFORM
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE        ZERO          TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                     PERFORM    VARYING  ACCT-IDX   FROM   1   BY   1
                                UNTIL    ACCT-IDX   >      31
                          IF  ((WRK-DAY1(ACCT-IDX)   NOT =  ZERO   ) OR
                               (RECE62-NYUINDAY-KBN(ACCT-IDX) = "1"))
                          AND ( WRK-DAY5(ACCT-IDX) =
                                           RECE62-HKNCOMBI (IDX1)   )
                          AND ( ACCT-DAY(1 ACCT-IDX) NOT =  ZERO    )
                              PERFORM      20011261-SRYACCT-GET-SEC
                          END-IF
                     END-PERFORM
                 END-IF
               END-IF  
           END-PERFORM
      *
           .
       2001126-RECE62-TENSOU23-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ²ñ·×¤ÎÆþ±¡´ðËÜÎÁ²Ã¸º»»ÅÀ¿ô¤Î·×»»
      **************************************************************
       20011261-SRYACCT-GET-SEC           SECTION.
      *
           EVALUATE   WRK-DAY1-NYUTENTTLKBN(ACCT-IDX)
              WHEN   903
                 IF       (ACCT-SRYCDTOTAL    =    190080470
                                              OR   190799670
                                              OR   190080570
                                              OR   190799770
                                              OR   190111690
                                              OR   190809090
                                              OR   190111790
                                              OR   190809190)
                 OR       (ACCT-ZAIKBN        =    2        )
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                                WRK-NYUTOKKEI2(KH-IDX)  -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                             RECE62-NYUTOKKEI2(KH-IDX)  -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                 ELSE
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUKHNKEI2(KH-IDX)  =
                                WRK-NYUKHNKEI2(KH-IDX)  +
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    COMPUTE  RECE62-NYUKHNKEI2(KH-IDX)  =
                             RECE62-NYUKHNKEI2(KH-IDX)  +
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                 END-IF
              WHEN   920
                 IF       (ACCT-SRYCDTOTAL    =    190080470
                                              OR   190799670
                                              OR   190080570
                                              OR   190799770
                                              OR   190111690
                                              OR   190809090
                                              OR   190111790
                                              OR   190809190)
                 OR       (ACCT-ZAIKBN        =    2        )
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                                WRK-NYUTOKKEI2(KH-IDX)  -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                             RECE62-NYUTOKKEI2(KH-IDX)  -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                 END-IF
           END-EVALUATE
      *
           IF    RECE62-NYUINDAY-KBN(ACCT-IDX) = "1"
      ***    IF  WRK-NYUINDAY-TANKI2-KBN(ACCT-IDX) NOT = SPACE
              IF ACCT-ZAIKBN = 2
               IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
               AND    KH-IDX                >   1
                  CONTINUE
               ELSE
                  COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                           WRK-NYUTOKKEI2(KH-IDX)  -
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
               END-IF
               COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                        RECE62-NYUTOKKEI2(KH-IDX)  -
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
              ELSE
               IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
               AND    KH-IDX                >   1
                  CONTINUE
               ELSE
                  COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                           WRK-NYUTOKKEI2(KH-IDX)  +
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
               END-IF
               COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                        RECE62-NYUTOKKEI2(KH-IDX)  +
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
              END-IF
      ***    ELSE
      ***     IF ACCT-ZAIKBN = 2
      ***      IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
      ***      AND    KH-IDX                >   1
      ***         CONTINUE
      ***      ELSE
      ***         COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
      ***                  WRK-NYUTOKKEI2(KH-IDX)  -
      ***                  (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
      ***      END-IF
      ***      COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
      ***               RECE62-NYUTOKKEI2(KH-IDX)  -
      ***                  (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
      ***     ELSE
      ***      IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
      ***      AND    KH-IDX                >   1
      ***         CONTINUE
      ***      ELSE
      ***         COMPUTE  WRK-NYUKHNKEI2(KH-IDX)  =
      ***                  WRK-NYUKHNKEI2(KH-IDX)  +
      ***                  (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
      ***      END-IF
      ***      COMPUTE  RECE62-NYUKHNKEI2(KH-IDX)  =
      ***               RECE62-NYUKHNKEI2(KH-IDX)  +
      ***                  (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
      ***     END-IF
      ***    END-IF
           END-IF
      *
           .
       20011261-SRYACCT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎÊÔ½¸¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       200113-RECE62-HENSYU3-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN          =    "1"  
                 AND    NACCT-SRYCDTOTAL   NOT   =    099999916
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                         IF  WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                              IF   WRK-DAY1-NYUTENTTLKBN(IDY) = 903
                              AND  NACCT-ZAITEN    NOT =    ZERO  
                              AND  NACCT-DAY(IDY)  NOT =    ZERO  
                                   PERFORM   2001131-NYUINKHN-GET3-SEC
                                   MOVE    31    TO    IDY
                              END-IF
                              MOVE    10    TO    IDX1
                         END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key7"              TO  MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO  MCP-TABLE
               MOVE    "key7"            TO  MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "6"  
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                       IF   (WRK-DAY1(IDY)   NOT  =  ZERO  )
                       OR   (RECE62-NYUINDAY-KBN(IDY) = "1")
                         IF   WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                              IF   (NACCT-ZAITEN    NOT =    ZERO)  
                              AND  (NACCT-DAY(IDY)  NOT =    ZERO)  
                              AND  (WRK-DAY2(IDY)  =    ZERO  OR  4
                                                              OR  5)
                                   PERFORM   2001131-NYUINKHN-GET3-SEC
                                   MOVE    31    TO    IDY
                              END-IF
                              MOVE    10    TO    IDX1
                         END-IF
                       ELSE
                         MOVE     10        TO    IDX1
                       END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
                 MOVE    "key7"               TO  MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key33"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key33"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF     (ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1))
                       AND    (ACCT-ZAIKBN    NOT = 1 AND 2          )
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                             IF   ACCT-SRYKBN  =  "90" OR "92"  
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                  UNTIL    ACCT-IDX   >   31
                                IF  ((WRK-DAY1(ACCT-IDX) NOT = ZERO ) OR
                                     (RECE62-NYUINDAY-KBN(ACCT-IDX)
                                                               = "1"))
                                AND ( WRK-DAY5(ACCT-IDX) =
                                           RECE62-HKNCOMBI (IDX1)    )
                                AND ( ACCT-DAY(1 ACCT-IDX) NOT = ZERO)
                                        PERFORM 2001132-ACCTKHN-GET3-SEC
                                        MOVE  31  TO  ACCT-IDX
                                END-IF
                               END-PERFORM
                             END-IF
                           END-IF
                           MOVE    10    TO    IDX1
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryacct"     TO      MCP-TABLE
                   MOVE    "key33"           TO      MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key33"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    Ì¤¼Â»Ü¸º»»¡Ê¥·¥¹¥Æ¥à´ÉÍýÀßÄêÊ¬¡Ë
      *
           IF   FLG-MIJISI-GENSAN   =   1
      *
               IF   FLG-MIJISI-INNAI   =   1
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190080570"    TO   WRK-SRYCD
      ***        ELSE
      ***           MOVE    "190799770"    TO   WRK-SRYCD
      ***        END-IF
                 MOVE    WRK-SRYYMD-TAIHI  TO   WRK-SRYYMD
      *
                 PERFORM   500-RYAKUSYO-HENSYU-SEC
               END-IF
      *
               IF   FLG-MIJISI-IRYO    =   1
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190111690"    TO   WRK-SRYCD
      ***        ELSE
      ***           MOVE    "190809090"    TO   WRK-SRYCD
      ***        END-IF
                 MOVE    WRK-SRYYMD-TAIHI  TO   WRK-SRYYMD
      *
                 PERFORM   500-RYAKUSYO-HENSYU-SEC
               END-IF
      *
               IF   FLG-MIJISI-JYOKU   =   1
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190111790"    TO   WRK-SRYCD
      ***        ELSE
      ***           MOVE    "190809190"    TO   WRK-SRYCD
      ***        END-IF
                 MOVE    WRK-SRYYMD-TAIHI  TO   WRK-SRYYMD
      *
                 PERFORM   500-RYAKUSYO-HENSYU-SEC
               END-IF
      *
           END-IF
      *
           .
       200113-RECE62-HENSYU3-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡Î¬¾Î¤Î¼èÆÀ
      **************************************************************
       2001131-NYUINKHN-GET3-SEC           SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinact"   TO   MCP-TABLE
               MOVE    "key9"           TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =    "1")
      *
                 PERFORM   VARYING  NYUINACT-IDX  FROM 1  BY  1
                      UNTIL  (NYUINACT-IDX       >   5)    OR
                             (NSRY-SRYCD(NYUINACT-IDX)  =  SPACE)
      *
                       MOVE    NSRY-SRYCD(NYUINACT-IDX) 
                                              TO  WRK-SRYCD
                       MOVE    NACCT-SRYYM    TO  WRK-SRYYMD-TAIHI(1:6)
                       MOVE    "01"           TO  WRK-SRYYMD-TAIHI(7:2)
                       MOVE    NACCT-SRYYM    TO  WRK-SRYYMD(1:6)
                       MOVE    "01"           TO  WRK-SRYYMD(7:2)
      *
                       PERFORM   500-RYAKUSYO-HENSYU-SEC
      *
                 END-PERFORM
      *
                 MOVE    "tbl_nyuinact"     TO   MCP-TABLE
                 MOVE    "key9"             TO   MCP-PATHNAME
                 PERFORM        900-NYUINACT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001131-NYUINKHN-GET3-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡Î¬¾Î¤Î¼èÆÀ¡Ê¿ÇÎÅ²ñ·×¡Ë
      **************************************************************
       2001132-ACCTKHN-GET3-SEC           SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
      ***  MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)
      *
                       MOVE    SRY-SRYCD(SRYACT-IDX)
                                                  TO  WRK-SRYCD
                       MOVE    ACCT-SRYYM         TO  WRK-SRYYMD(1:6)
                       MOVE    "01"               TO  WRK-SRYYMD(7:2)
      *
                       PERFORM   500-RYAKUSYO-HENSYU-SEC
      *
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO   MCP-TABLE
                   MOVE    "key9"            TO   MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001132-ACCTKHN-GET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎÊÔ½¸¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       200114-RECE62-HENSYU4-SEC                SECTION.
      *
      **   IF    WRK-SHOKUJIRYOYO   NOT =  ZERO
      *       ¿©»öÎ¬¾Î¤Î¼èÆÀ
              PERFORM  VARYING  IDY    FROM    1   BY  1
                             UNTIL   IDY     >       31
      *
                 PERFORM  VARYING  IDX1    FROM    1   BY  1
                                UNTIL   IDX1    >       10
                                OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                  IF  WRK-DAY1(IDY) NOT =  ZERO 
                    IF   WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                         IF   (WRK-DAY4(IDY)   NOT =    ZERO)  
                         AND  (WRK-DAY2(IDY)  =    ZERO  OR  4
                                                         OR  5)
                         AND  (WRK-SHOKUJIRYOYO(IDY)  NOT = ZERO)
                              PERFORM   201141-NYUINKHN-GET4-SEC
                         END-IF
                         MOVE    10    TO    IDX1
                    END-IF
                  ELSE
                    MOVE     10        TO    IDX1
                  END-IF
      *
                 END-PERFORM
      *
              END-PERFORM
      **   END-IF
      *
           .
       200114-RECE62-HENSYU4-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎ¬¾Î¤Î¼èÆÀ
      **************************************************************
       201141-NYUINKHN-GET4-SEC           SECTION.
      *
           EVALUATE   WRK-DAY4(IDY)
             WHEN  1
                EVALUATE   WRK-SHOKUJIRYOYO(IDY)
                  WHEN  1
                     MOVE    "197000110"  TO  WRK-SRYCD
                     PERFORM  2011411-NYUINKHN-SET4-SEC
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        MOVE    "197000270"  TO  WRK-SRYCD
                        PERFORM  2011411-NYUINKHN-SET4-SEC
                     END-IF
                     IF    WRK-DAY41(IDY)  =  1
                        MOVE    "197000570"  TO  WRK-SRYCD
                        PERFORM  2011411-NYUINKHN-SET4-SEC
                     END-IF
                  WHEN  2
                     MOVE    "197000710"  TO  WRK-SRYCD
                     PERFORM  2011411-NYUINKHN-SET4-SEC
                END-EVALUATE
             WHEN  2
             WHEN  3
             WHEN  4
                     MOVE    "197000110"  TO  WRK-SRYCD
                     PERFORM  2011411-NYUINKHN-SET4-SEC
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        MOVE    "197000270"  TO  WRK-SRYCD
                        PERFORM  2011411-NYUINKHN-SET4-SEC
                     END-IF
                     IF    WRK-DAY41(IDY)  =  1
                        MOVE    "197000570"  TO  WRK-SRYCD
                        PERFORM  2011411-NYUINKHN-SET4-SEC
                     END-IF
           END-EVALUATE
      *
           .
       201141-NYUINKHN-GET4-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎ¬¾Î¤Î¥»¥Ã¥È
      **************************************************************
       2011411-NYUINKHN-SET4-SEC           SECTION.
      *
           MOVE    SPACE          TO   NYURYAKUSHO-REC
           INITIALIZE                  NYURYAKUSHO-REC
           MOVE    WRK-SRYCD      TO   NYURYAKUSHO-SRYCD
           MOVE    RECE62-SRYYM   TO   NYURYAKUSHO-YUKOSTYMD(1:6)
           MOVE    "01"           TO   NYURYAKUSHO-YUKOSTYMD(7:2)
           MOVE    NYURYAKUSHO-YUKOSTYMD
                                  TO   NYURYAKUSHO-YUKOEDYMD
           PERFORM  900-NYURYAKUSHO-READ-SEC
           IF       FLG-NYURYAKUSHO    =    ZERO
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 3
                         OR    RECE62-SHOKUJISYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE62-SHOKUJISYUBETU(ID1)  
                               =  NYURYAKUSHO-RYAKUSHO
                        MOVE  1   TO   FLG-ARI
                    END-IF
                END-PERFORM  
                IF  FLG-ARI  =  ZERO  
                    MOVE   NYURYAKUSHO-RYAKUSHO
                             TO   RECE62-SHOKUJISYUBETU(ID1)
                END-IF
           END-IF                  
      *
           .
       2011411-NYUINKHN-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎÊÔ½¸¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       200115-RECE62-HENSYU9-SEC                SECTION.
      *
      *     ÅÀÅ©ÌôºÞ¡Ê´ïºà¡Ë¤Þ¤È¤á½èÍý
           PERFORM    2001151-TENTEK-MATOME-SEC
      *
           INITIALIZE    WRK-TENTEK-FLG-TABLE
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key33"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key33"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1)
                           MOVE   IDX1          TO   TENTEK-IDX
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                               PERFORM   500-KUMIAWASE-HANBETU-SEC
                               PERFORM   2001152-RECE62-TENSOU9-SEC
                           END-IF
                           MOVE   10            TO   IDX1
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryacct"        TO      MCP-TABLE
                   MOVE    "key33"              TO      MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key33"              TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    ÎÅÍÜÃ´Åö¼êÅö½¸·×
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
      *
             IF   (WRK-DAY1(IDY)   NOT  =  ZERO  )
             OR   (RECE62-NYUINDAY-KBN(IDY) = "1")
      *
                PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL   IDX1     >     10
                           OR     RECE62-HKNCOMBI (IDX1)  =   ZERO
      *
                   IF   WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                      IF  (WRK-DAY7(IDY)          NOT =  ZERO)
                      AND (WRK-DAY7-ZAITEN(IDY)   NOT =  ZERO)
                      AND (WRK-DAY2(IDY)              =  ZERO  OR  4
                                                               OR  5)
                           PERFORM 500-KUMIAWASE-HANBETU-SEC
                           PERFORM 2001153-RECE62-TENSOU9-RYOYO-SEC
                      END-IF
                      MOVE    10    TO     IDX1
                   END-IF
      *
                END-PERFORM
      *
             END-IF
      *
           END-PERFORM
      *
           .
       200115-RECE62-HENSYU9-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2001152-RECE62-TENSOU9-SEC                 SECTION.
      *
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
               IF        ACCT-SRYKBN   =    "90" OR "92" OR "95"  
                                                 OR "96" OR "97"
                   CONTINUE
               ELSE
                 IF       (ACCT-SRYCDTOTAL    =    630010002)
      *                                       OR   190080470
      *                                       OR   190799670
      *                                       OR   190080570
      *                                       OR   190799770
      *                                       OR   190111690
      *                                       OR   190809090
      *                                       OR   190111790
      *                                       OR   190809190
                 OR       (ACCT-ZAIKBN        =    2        )
                   COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                             TBL-TOTALTEN-ACCT(1) -
                             (ACCT-ZAITEN  *  
                              ACCT-ZAIKAISU)
                   COMPUTE   TBL-TOTALTEN2-ACCT(1) =  
                             TBL-TOTALTEN2-ACCT(1) -
                             (ACCT-ZAITEN  *  
                              ACCT-ZAIKAISU)
                 ELSE
                   IF        WRK-TENTEK-FLG(TENTEK-IDX 1)  =   ZERO
                       COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                                 TBL-TOTALTEN-ACCT(1) +
                                 WRK-TENTEK-YKZTEN(TENTEK-IDX) +
                                 WRK-TENTEK-KIZAITEN(TENTEK-IDX)
                       COMPUTE   TBL-TOTALTEN2-ACCT(1) =  
                                 TBL-TOTALTEN2-ACCT(1) +
                                 WRK-TENTEK-YKZTEN(TENTEK-IDX) +
                                 WRK-TENTEK-KIZAITEN(TENTEK-IDX)
                       MOVE  1        TO   WRK-TENTEK-FLG(TENTEK-IDX 1)
                   END-IF
                   IF        ACCT-SRYKBN     =    "33"
                       PERFORM   20011521-TENTEK-SYUTEN-SEC
                       COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                                 TBL-TOTALTEN-ACCT(1) +
                                 WRK-TENTEK-SYUTEN
                       COMPUTE   TBL-TOTALTEN2-ACCT(1) =  
                                 TBL-TOTALTEN2-ACCT(1) +
                                 WRK-TENTEK-SYUTEN
                   ELSE
                       COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                                 TBL-TOTALTEN-ACCT(1) +
                                 (ACCT-ZAITEN  *  
                                  ACCT-ZAIKAISU)
                       COMPUTE   TBL-TOTALTEN2-ACCT(1) =  
                                 TBL-TOTALTEN2-ACCT(1) +
                                 (ACCT-ZAITEN  *  
                                  ACCT-ZAIKAISU)
                   END-IF
                 END-IF
               END-IF
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
             UNTIL   (IDX2      >       4)
               MOVE       ZERO           TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                   IF        ACCT-SRYKBN   =    "90" OR "92" OR "95"  
                                                     OR "96" OR "97"
                       CONTINUE
                   ELSE
                     IF       (ACCT-SRYCDTOTAL    =    630010002)
      *                                           OR   190080470
      *                                           OR   190799670
      *                                           OR   190080570
      *                                           OR   190799770
      *                                           OR   190111690
      *                                           OR   190809090
      *                                           OR   190111790
      *                                           OR   190809190
                     OR       (ACCT-ZAIKBN        =    2        )
                       IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                          CONTINUE
                       ELSE
                          COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                   TBL-TOTALTEN-ACCT(KH-IDX) -
                                   (ACCT-ZAITEN * 
                                    ACCT-ZAIKAISU)
                       END-IF
                       COMPUTE  TBL-TOTALTEN2-ACCT(KH-IDX) =
                                TBL-TOTALTEN2-ACCT(KH-IDX) -
                                (ACCT-ZAITEN * 
                                 ACCT-ZAIKAISU)
                     ELSE
                       IF        WRK-TENTEK-FLG(TENTEK-IDX KH-IDX)
                                                             =   ZERO
                           IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                              CONTINUE
                           ELSE
                              COMPUTE   TBL-TOTALTEN-ACCT(KH-IDX) =  
                                        TBL-TOTALTEN-ACCT(KH-IDX) +
                                        WRK-TENTEK-YKZTEN(TENTEK-IDX) +
                                        WRK-TENTEK-KIZAITEN(TENTEK-IDX)
                           END-IF
                           COMPUTE   TBL-TOTALTEN2-ACCT(KH-IDX) =  
                                     TBL-TOTALTEN2-ACCT(KH-IDX) +
                                     WRK-TENTEK-YKZTEN(TENTEK-IDX) +
                                     WRK-TENTEK-KIZAITEN(TENTEK-IDX)
                           MOVE  1       TO   WRK-TENTEK-FLG
                                                   (TENTEK-IDX KH-IDX)
                       END-IF
                       IF        ACCT-SRYKBN     =    "33"
                           PERFORM   20011521-TENTEK-SYUTEN-SEC
                           IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                              CONTINUE
                           ELSE
                              COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                       TBL-TOTALTEN-ACCT(KH-IDX) +
                                       WRK-TENTEK-SYUTEN
                           END-IF
                           COMPUTE  TBL-TOTALTEN2-ACCT(KH-IDX) =
                                    TBL-TOTALTEN2-ACCT(KH-IDX) +
                                    WRK-TENTEK-SYUTEN
                       ELSE
                           IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                              CONTINUE
                           ELSE
                              COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                       TBL-TOTALTEN-ACCT(KH-IDX) +
                                      (ACCT-ZAITEN * 
                                       ACCT-ZAIKAISU)
                           END-IF
                           COMPUTE  TBL-TOTALTEN2-ACCT(KH-IDX) =
                                    TBL-TOTALTEN2-ACCT(KH-IDX) +
                                   (ACCT-ZAITEN * 
                                    ACCT-ZAIKAISU)
                       END-IF
                     END-IF
                   END-IF
                 END-IF
      *
               END-IF  
           END-PERFORM
      *
           .
       2001152-RECE62-TENSOU9-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀÅ©Ãí¼Í¤ÎÌôºÞ¡Ê´ïºà¡ËÅÀ¿ô»»Äê
      *****************************************************************
       2001151-TENTEK-MATOME-SEC                    SECTION.
      *
           INITIALIZE                  WRK-TENTEK-TABLE
      *
           PERFORM     VARYING   TENTEK-IDX  FROM  1  BY  1
                         UNTIL  (TENTEK-IDX  >  10)    OR
                                (RECE62-HKNCOMBI(TENTEK-IDX) = ZERO)
               INITIALIZE                  ORCSTENTEKAREA
               MOVE    "2"                 TO  ORCSTEN-KBN
               MOVE    RECE62-PTID        TO  ORCSTEN-PTID
               MOVE    RECE62-SRYYM       TO  ORCSTEN-SRYYM
               MOVE    RECE62-HKNCOMBI(TENTEK-IDX)
                                          TO  ORCSTEN-HKNCOMBI
               MOVE    RECE62-HOSPNUM     TO  SPA-HOSPNUM
               CALL    "ORCSTENTEK"        USING
                                           SPA-AREA
                                           ORCSTENTEKAREA
      *        ÌôºÞÅÀ¿ô¤È´ïºàÅÀ¿ô
               PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL  IDX1     >       31
      *
                        IF  ORCSTEN-ZAITEN(IDX1)  NOT =  ZERO
                            ADD    ORCSTEN-ZAITEN(IDX1)
                                   TO  WRK-TENTEK-YKZTEN(TENTEK-IDX)
                        END-IF
               END-PERFORM
      *
               PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL  IDX1     >       50
                           OR    ORCSTEN-KIZ-ZAITEN(IDX1)  =  ZERO
      *
                        MOVE    ZERO   TO   WRK-TENTEK-ZAIKAISU
                        PERFORM  VARYING  IDX2    FROM    1   BY  1
                              UNTIL   IDX2     >       31
                            ADD    ORCSTEN-KIZ-DAY(IDX1 IDX2)
                                   TO  WRK-TENTEK-ZAIKAISU
                        END-PERFORM
      *
                        COMPUTE  WRK-TENTEK-KIZAITEN(TENTEK-IDX)  =
                                 WRK-TENTEK-KIZAITEN(TENTEK-IDX)  +
                                 ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                                   WRK-TENTEK-ZAIKAISU ) 
      *
               END-PERFORM
           END-PERFORM
      *
           .
       2001151-TENTEK-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀÅ©Ãí¼Í¤Î¼êµ»ÅÀ¿ô»»Äê
      *****************************************************************
       20011521-TENTEK-SYUTEN-SEC                    SECTION.
      *
      *¿ÇÎÅ¹Ô°Ù¥Æ¡¼¥Ö¥ë¤«¤é¿ÇÎÅ¼ïÊÌ¶èÊ¬¤Î¼èÆÀ
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM       TO   SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
           MOVE    ACCT-PTID          TO   SRY-PTID
      ***  MOVE    ACCT-SRYKA         TO   SRY-SRYKA
           MOVE    ACCT-SRYYM         TO   SRY-SRYYM
           MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
           MOVE    SRYACT-REC         TO   MCPDATA-REC
           MOVE    "tbl_sryact"       TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =    ZERO
                 MOVE    "tbl_sryact"     TO   MCP-TABLE
                 MOVE    "key9"           TO   MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
           ELSE
                 MOVE    1                TO   FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                   TO   WRK-TENTEK-SYUTEN
      *
           IF   FLG-SRYACT   =    ZERO
                EVALUATE  SRY-SRYSYUKBN
                    WHEN  "330"
                    WHEN  "331"
                    WHEN  "332"
                       COMPUTE WRK-TENTEK-SYUTEN = WRK-TENTEK-SYUTEN +
                               (ACCT-SYUTEN1 * ACCT-ZAIKAISU)
                    WHEN  "340"
                       COMPUTE WRK-TENTEK-SYUTEN = WRK-TENTEK-SYUTEN +
                               (ACCT-ZAITEN * ACCT-ZAIKAISU)
                END-EVALUATE
           END-IF
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       20011521-TENTEK-SYUTEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎÅÍÜÃ´Åö¼êÅöÅ¾Á÷
      *****************************************************************
       2001153-RECE62-TENSOU9-RYOYO-SEC               SECTION.
      *
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
      *
                 COMPUTE  TBL-TOTALTEN-ACCT(1) =
                          TBL-TOTALTEN-ACCT(1) +
                          WRK-DAY7-ZAITEN(IDY)
                 COMPUTE  TBL-TOTALTEN2-ACCT(1) =
                          TBL-TOTALTEN2-ACCT(1) +
                          WRK-DAY7-ZAITEN(IDY)
      *
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                       UNTIL   (IDX2      >       4)
               MOVE        ZERO          TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                     IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                        CONTINUE
                     ELSE
                        COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                 TBL-TOTALTEN-ACCT(KH-IDX) +
                                 WRK-DAY7-ZAITEN(IDY)
                     END-IF
                     COMPUTE  TBL-TOTALTEN2-ACCT(KH-IDX) =
                              TBL-TOTALTEN2-ACCT(KH-IDX) +
                              WRK-DAY7-ZAITEN(IDY)
                 END-IF
               END-IF
           END-PERFORM
      *
           .
       2001153-RECE62-TENSOU9-RYOYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ÂÆü¿ô¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       200116-JNISSU-SET-SEC                    SECTION.
      *
      *¼ÂÆü¿ô¤«¤é¸º¤¹¤ë
           PERFORM    VARYING    IDX    FROM   1   BY   1
                       UNTIL     IDX    >      5
             PERFORM    VARYING    IDY    FROM    1   BY   1
                         UNTIL     IDY    >      31
                IF  (TBL-JNISSU-DAY(IDX IDY)      NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(IDX IDY)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU-DAY(IDX IDY)
                END-IF
             END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   3   BY   1
                       UNTIL     IDX    >      5
             PERFORM    VARYING    IDY    FROM    1   BY   1
                         UNTIL     IDY    >      31
                IF  (TBL-JNISSU2-DAY(IDX IDY)     NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(IDX IDY)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU2-DAY(IDX IDY)
                END-IF
             END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                       UNTIL     IDX    >      5
             PERFORM    VARYING    IDY    FROM    1   BY   1
                         UNTIL     IDY    >      31
                IF   TBL-JNISSU-DAY(IDX IDY)      NOT =  SPACE
                    MOVE   "1"   TO  TBL-JNISSU-DAY-M(IDY)
                END-IF
             END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      5
               PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     31
                    IF    TBL-JNISSU-DAY(IDX IDY)  NOT =  SPACE
                          ADD    1      TO    RECE62-JNISSU(IDX)
                    END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   3   BY   1
                      UNTIL      IDX    >      5
               PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     31
                    IF    TBL-JNISSU2-DAY(IDX IDY) NOT =  SPACE
                          ADD    1      TO    TBL-JNISSU2-MON(IDX)
                    END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                      UNTIL      IDY    >     31
                    IF    TBL-JNISSU-DAY-M(IDY) NOT =  SPACE
                          ADD    1      TO    RECE62-SKYIRYO-NISSU
                    END-IF
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      5
               PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     31
                    IF    TBL-JNISSU-DAY(IDX IDY)  NOT =  SPACE
                          MOVE   "1"    TO    RECE62-JNISSUTBL(IDY)
                    END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      10
               PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     5
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             WRK-NYUKHNKEI(IDX IDY)
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             WRK-NYUTOKKEI(IDX IDY)
      *
                    COMPUTE  RECE62-HENTOTALTEN(IDY)  = 
                             RECE62-HENTOTALTEN(IDY)  + 
                             RECE62-NYUKHNKEI(IDX IDY)
                    COMPUTE  RECE62-HENTOTALTEN(IDY)  = 
                             RECE62-HENTOTALTEN(IDY)  + 
                             RECE62-NYUTOKKEI(IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     5
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             WRK-NYUKHNKEI2(IDY)
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             WRK-NYUTOKKEI2(IDY)
      *
                    COMPUTE  RECE62-HENTOTALTEN(IDY)  = 
                             RECE62-HENTOTALTEN(IDY)  + 
                             RECE62-NYUKHNKEI2(IDY)
                    COMPUTE  RECE62-HENTOTALTEN(IDY)  = 
                             RECE62-HENTOTALTEN(IDY)  + 
                             RECE62-NYUTOKKEI2(IDY)
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     5
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             TBL-TOTALTEN-ACCT(IDY)
                    COMPUTE  RECE62-HENTOTALTEN(IDY)  = 
                             RECE62-HENTOTALTEN(IDY)  + 
                             TBL-TOTALTEN2-ACCT(IDY)
           END-PERFORM
      *
           IF   RECE62-SRYYM    >=    "200804"
             CONTINUE
           ELSE
             PERFORM    VARYING    IDX    FROM   1   BY   1
                        UNTIL      IDX    >      10
               PERFORM    VARYING    IDY    FROM    2   BY   1
                          UNTIL      IDY    >     5
                 IF  ( RECE62-HENTOTALTEN(IDY) NOT =  ZERO       )
                 AND ( RECE62-HENTOTALTEN(1)   NOT =  
                                         RECE62-HENTOTALTEN(IDY) )
                    CONTINUE
                 ELSE
                    MOVE    ZERO    TO   RECE62-NYUKHNKEI(IDX IDY)
                    MOVE    ZERO    TO   RECE62-NYUTOKKEI(IDX IDY)
                 END-IF
               END-PERFORM
             END-PERFORM
      *
             PERFORM    VARYING    IDY    FROM    2   BY   1
                        UNTIL      IDY    >     5
                 IF  ( RECE62-HENTOTALTEN(IDY) NOT =  ZERO       )
                 AND ( RECE62-HENTOTALTEN(1)   NOT =  
                                         RECE62-HENTOTALTEN(IDY) )
                    CONTINUE
                 ELSE
                    MOVE    ZERO    TO   RECE62-NYUKHNKEI2(IDY)
                    MOVE    ZERO    TO   RECE62-NYUTOKKEI2(IDY)
                 END-IF
             END-PERFORM
           END-IF
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      10
                 IF   RECE62-NYUTOKRYO(IDX)  =  999999999
                    MOVE    ZERO    TO   RECE62-NYUTOKRYO(IDX)
                 END-IF
                 IF   RECE62-NYUTOKRYO-RYAKU(IDX)  
                                             =  "£¹£¹£¹£¹£¹£¹£¹"
                    MOVE    SPACE   TO   RECE62-NYUTOKRYO-RYAKU(IDX)
                 END-IF
           END-PERFORM
      *
      ***
      *
           IF    RECE62-FUKUSHIFLG(3)  =  "2"
              IF  (RECE62-KOHNUM(2)(1:1) >= "1" AND <= "8")
              AND (RECE62-TOTALTEN(3)     =  ZERO         )
                 MOVE  RECE62-HENTOTALTEN(3)  TO  RECE62-TOTALTEN(3)
              END-IF
              IF  (RECE62-KOHNUM(2)(1:1) >= "1" AND <= "8")
              AND (RECE62-JNISSU(3)       =  ZERO         )
                 MOVE  TBL-JNISSU2-MON(3)     TO  RECE62-JNISSU(3)
              END-IF
           END-IF
           IF    RECE62-FUKUSHIFLG(4)  =  "2"
              IF  (RECE62-KOHNUM(3)(1:1) >= "1" AND <= "8")
              AND (RECE62-TOTALTEN(4)     =  ZERO         )
                 MOVE  RECE62-HENTOTALTEN(4)  TO  RECE62-TOTALTEN(4)
              END-IF
              IF  (RECE62-KOHNUM(3)(1:1) >= "1" AND <= "8")
              AND (RECE62-JNISSU(4)       =  ZERO         )
                 MOVE  TBL-JNISSU2-MON(4)     TO  RECE62-JNISSU(4)
              END-IF
           END-IF
           IF    RECE62-FUKUSHIFLG(5)  =  "2"
              IF  (RECE62-KOHNUM(4)(1:1) >= "1" AND <= "8")
              AND (RECE62-TOTALTEN(5)     =  ZERO         )
                 MOVE  RECE62-HENTOTALTEN(5)  TO  RECE62-TOTALTEN(5)
              END-IF
              IF  (RECE62-KOHNUM(4)(1:1) >= "1" AND <= "8")
              AND (RECE62-JNISSU(5)       =  ZERO         )
                 MOVE  TBL-JNISSU2-MON(5)     TO  RECE62-JNISSU(5)
              END-IF
           END-IF
      *
           .
       200116-JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿¹¹¿·½èÍý¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       200117-RECE62-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE62-REC
      *
           ADD     1                   TO  CNT-MOD (IDZ)
      *
           .
       200117-RECE62-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á´¹ñ¸øÈñÊ»ÍÑÊ¬ÀÁµáÅÀ½¸·×È½ÊÌ
      *****************************************************************
       200118-SYUKEIKBN-HANBETU-SEC      SECTION.
      *
           IF    IDZ    =    1
      *
             PERFORM  VARYING  IDX11   FROM    1   BY  1
                       UNTIL  (IDX11                   >   10 )
                         OR   (RECE62-HKNCOMBI(IDX11)  =  ZERO)
      *
              IF ((RECE62-HKNNUMHC(IDX11)     NOT = SPACE          ) AND
                  (RECE62-KOHNUMHC(IDX11 1)(1:1)  = "0"            ) AND
                  (RECE62-KOHNUMHC(IDX11 2)(1:1) >= "1" AND <= "8" ))
              OR ((WRK-PREFNUM                    = "13"           ) AND
                  (RECE62-HKNNUMHC(IDX11)     NOT = SPACE          ) AND
                  (RECE62-KOHNUMHC(IDX11 1)(1:1) >= "1" AND <= "8" ) AND
                  (RECE62-KOHNUMHC(IDX11 2)(1:1) >= "1" AND <= "8" ))
              OR ((WRK-PREFNUM                    = "23"           ) AND
                  (RECE62-SRYYM                  >= "200910"       ) AND
                  (RECE62-HKNNUMHC(IDX11)     NOT = SPACE          ) AND
                  (RECE62-KOHNUMHC(IDX11 1)       = "191"          ) AND
                  (RECE62-KOHNUMHC(IDX11 2)(1:1) >= "1" AND <= "8" ))
      *
                IF ((RECE62-KOHNUMHC(IDX11 2) =  RECE62-KOHNUM(2)) AND
                    (RECE62-KOHIDHC(IDX11 2)  =  RECE62-KOHID(2) ))
                OR ((RECE62-KOHNUMHC(IDX11 2) =  RECE62-KOHNUM(3)) AND
                    (RECE62-KOHIDHC(IDX11 2)  =  RECE62-KOHID(3) ))
                OR ((RECE62-KOHNUMHC(IDX11 2) =  RECE62-KOHNUM(4)) AND
                    (RECE62-KOHIDHC(IDX11 2)  =  RECE62-KOHID(4) ))
      *
                  IF    RECE62-SRYYM  >=  "200804"
                    INITIALIZE                   SYSKANRI-REC
                    MOVE    "2010"           TO  SYS-KANRICD
                    MOVE    RECE62-KOHNUMHC(IDX11 2)
                                             TO  SYS-KBNCD
                    MOVE    RECE62-SRYYM     TO  SYS-STYUKYMD (1:6)
                    MOVE    "01"             TO  SYS-STYUKYMD (7:2)
                    MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
                    MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                    MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                    MOVE    "tbl_syskanri"   TO  MCP-TABLE
                    MOVE    "key10"          TO  MCP-PATHNAME
                    PERFORM   900-DBSELECT-SEC
                    IF      MCP-RC           =   ZERO
                      MOVE  "tbl_syskanri"   TO  MCP-TABLE
                      MOVE  "key10"          TO  MCP-PATHNAME
                      PERFORM 900-SYSKANRI-READ-SEC
                    ELSE
                      INITIALIZE                 SYS-2010-REC
                    END-IF
                    PERFORM   2001181-SYUKEIKBN-HENSYU-SEC
                    MOVE    "tbl_syskanri"   TO  MCP-TABLE
                    MOVE    "key10"          TO  MCP-PATHNAME
                    PERFORM   900-CLOSE-SEC
                  ELSE
                    EVALUATE   WRK-PREFNUM
                      WHEN     "32"
                        IF  RECE62-SRYYM  >=  "200510"
                          MOVE "2" TO RECE62-SYUKEIKBN(IDX11 2)
                          MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                        END-IF
                      WHEN     "13"
                      WHEN     "26"
                          MOVE "2" TO RECE62-SYUKEIKBN(IDX11 2)
                          MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                      WHEN     "08"
                        IF  RECE62-SRYYM  >=  "200607"
                          MOVE "2" TO RECE62-SYUKEIKBN(IDX11 2)
                          MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                        END-IF
                      WHEN     "38"
                          MOVE "1" TO RECE62-SYUKEIKBN(IDX11 2)
                          MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                    END-EVALUATE
                  END-IF
      *
                  IF ((WRK-PREFNUM               =  "13"    ) AND
                      (RECE62-KOHNUMHC(IDX11 2)  =  "151"   ) AND
                      (RECE62-SRYYM             >=  "200905"))
                  OR ((WRK-PREFNUM               =  "23"    ) AND
                      (RECE62-KOHNUMHC(IDX11 2)  =  "191"   ) AND
                      (RECE62-SRYYM             >=  "200910"))
                          MOVE "0" TO RECE62-SYUKEIKBN(IDX11 2)
                          MOVE "0" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                  END-IF
      *
                END-IF
      *
              END-IF
      *
              IF ((RECE62-HKNNUMHC(IDX11)    = SPACE   ) AND
                  (RECE62-KOHNUMHC(IDX11 1)  = "079"   ) AND
                  (RECE62-KOHNUMHC(IDX11 2)  = "012"   ) AND
                  (RECE62-SRYYM             >= "200804"))
                 MOVE     1     TO    RECE62-FTNKBNFLG(IDX11 2)
              END-IF
      *
             END-PERFORM
      *
           END-IF
      *
           .
       200118-SYUKEIKBN-HANBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á´¹ñ¸øÈñÊ»ÍÑÊ¬ÀÁµáÅÀ½¸·×È½ÊÌÊÔ½¸
      *****************************************************************
       2001181-SYUKEIKBN-HENSYU-SEC      SECTION.
      *
           EVALUATE  SYS-2010-SYUKEIKBN
             WHEN      SPACE
                 EVALUATE   WRK-PREFNUM
                   WHEN     "32"
                   WHEN     "13"
                   WHEN     "26"
                   WHEN     "08"
                       MOVE "2" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                   WHEN     "36"
                     IF  RECE62-SRYYM  >=  "200805"
                       MOVE "2" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "0" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                     END-IF
                   WHEN     "10"
                       MOVE "2" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "4" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                   WHEN     "30"
                     IF  RECE62-SRYYM  >=  "200904"
                       MOVE "1" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                     END-IF
                   WHEN     "38"
                       MOVE "1" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                 END-EVALUATE
             WHEN      "0"
             WHEN      "1"
             WHEN      "2"
             WHEN      "3"
             WHEN      "4"
                 MOVE   SYS-2010-SYUKEIKBN
                                    TO   RECE62-SYUKEIKBN(IDX11 2)
                 MOVE   SYS-2010-SYUKEI-TAIKBN
                                    TO   RECE62-SYUKEI-TAIKBN(IDX11 2)
           END-EVALUATE
      *
           .
       2001181-SYUKEIKBN-HENSYU-EXT.
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
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
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
           DISPLAY "RECE620 IN  :" CNT-IN   (1) 
           DISPLAY "        MOD :" CNT-MOD  (1) 
           DISPLAY "RECE62X IN  :" CNT-IN   (2) 
           DISPLAY "        MOD :" CNT-MOD  (2) 
           DISPLAY "*** ORCR0630 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡Î¬¾Î¤ÎÊÔ½¸
      *****************************************************************
       500-RYAKUSYO-HENSYU-SEC                SECTION.
      *
           MOVE    SPACE         TO   NYURYAKUSHO-REC
           INITIALIZE                 NYURYAKUSHO-REC
      *
           MOVE    WRK-SRYCD     TO   NYURYAKUSHO-SRYCD
           MOVE    WRK-SRYYMD    TO   NYURYAKUSHO-YUKOSTYMD
                                      NYURYAKUSHO-YUKOEDYMD
           PERFORM  900-NYURYAKUSHO-READ-SEC
           IF       FLG-NYURYAKUSHO    =    ZERO
              MOVE     ZERO     TO    FLG-ARI
              PERFORM  VARYING  ID1    FROM    1   BY  1
                    UNTIL   ID1                   > 20
                      OR    RECE62-NYUSYUBETU(ID1) = SPACE
                      OR    FLG-ARI                = 1 
                 IF  RECE62-NYUSYUBETU(ID1)  
                            =  NYURYAKUSHO-RYAKUSHO
                     MOVE  1   TO   FLG-ARI
                 END-IF
              END-PERFORM  
              IF  FLG-ARI  =  ZERO  
                     MOVE   NYURYAKUSHO-RYAKUSHO
                               TO   RECE62-NYUSYUBETU(ID1)
              END-IF
      *
              IF   RECE62-SRYYM  >= "200604"
                CONTINUE
              ELSE
                IF   WRK-SRYCD  =  "190109670"  OR  "190808870"
      *
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 20
                         OR    RECE62-NYUSYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE62-NYUSYUBETU(ID1)(1:4)   =   "Í­´Ç"
                        MOVE  1   TO   FLG-ARI
                    END-IF
                 END-PERFORM  
                 IF  FLG-ARI  =  ZERO  
                        MOVE   "Í­´Ç"
                                  TO   RECE62-NYUSYUBETU(ID1)(1:4)
                 END-IF
      *
                END-IF
                IF   WRK-SRYCD  =  "190117070"  OR  "190813270"
      *
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 20
                         OR    RECE62-NYUSYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE62-NYUSYUBETU(ID1)(1:4)   =   "Í­°å"
                        MOVE  1   TO   FLG-ARI
                    END-IF
                 END-PERFORM  
                 IF  FLG-ARI  =  ZERO  
                        MOVE   "Í­°å"
                                  TO   RECE62-NYUSYUBETU(ID1)(1:4)
                 END-IF
      *
                END-IF
                IF   WRK-SRYCD  =  "190117670"  OR  "190813870"
      *
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 20
                         OR    RECE62-NYUSYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE62-NYUSYUBETU(ID1)(1:6)   =   "Í­´Ç°å"
                        MOVE  1   TO   FLG-ARI
                    END-IF
                 END-PERFORM  
                 IF  FLG-ARI  =  ZERO  
                        MOVE   "Í­´Ç°å"
                                  TO   RECE62-NYUSYUBETU(ID1)(1:6)
                 END-IF
      *
                END-IF
              END-IF
           ELSE
              IF   ( RECE62-SRYYM  >= "200604"    )
              AND  ( WRK-SRYCD      = "190123250" OR
                                      "190123350" OR
                                      "190123450" OR
                                      "190123550" )
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                      UNTIL   ID1                   > 20
                        OR    RECE62-NYUSYUBETU(ID1) = SPACE
                        OR    FLG-ARI                = 1 
                   EVALUATE   WRK-SRYCD
                     WHEN     "190123250"
                      IF  RECE62-NYUSYUBETU(ID1)(1:8) = "°ìÈÌ·Ð²á"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190123350"
                      IF  RECE62-NYUSYUBETU(ID1)(1:8) = "·ë³Ë·Ð²á"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190123450"
                      IF  RECE62-NYUSYUBETU(ID1)(1:8) = "Àº¿À·Ð²á"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190123550"
                      IF  RECE62-NYUSYUBETU(ID1)(1:8) = "¾ã³²·Ð²á"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                   END-EVALUATE
                END-PERFORM  
                IF  FLG-ARI  =  ZERO
                   EVALUATE   WRK-SRYCD
                     WHEN     "190123250"
                      MOVE   "°ìÈÌ·Ð²á"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190123350"
                      MOVE   "·ë³Ë·Ð²á"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190123450"
                      MOVE   "Àº¿À·Ð²á"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190123550"
                      MOVE   "¾ã³²·Ð²á"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:8)
                   END-EVALUATE
                END-IF
              END-IF
              IF   ( RECE62-SRYYM  >= "200807"    )
              AND  ( WRK-SRYCD      = "190130810" OR
                                      "190131010" OR
                                      "190131110" )
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                      UNTIL   ID1                   > 20
                        OR    RECE62-NYUSYUBETU(ID1) = SPACE
                        OR    FLG-ARI                = 1 
                   EVALUATE   WRK-SRYCD
                     WHEN     "190130810"
                      IF  RECE62-NYUSYUBETU(ID1)(1:8) = "°ìÈÌ£±£°"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190131010"
                      IF  RECE62-NYUSYUBETU(ID1)(1:6) = "·ë£±£°"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190131110"
                      IF  RECE62-NYUSYUBETU(ID1)(1:6) = "Àì£±£°"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                   END-EVALUATE
                END-PERFORM  
                IF  FLG-ARI  =  ZERO
                   EVALUATE   WRK-SRYCD
                     WHEN     "190130810"
                      MOVE   "°ìÈÌ£±£°"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190131010"
                      MOVE   "·ë£±£°"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:6)
                     WHEN     "190131110"
                      MOVE   "Àì£±£°"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:6)
                   END-EVALUATE
                END-IF
              END-IF
           END-IF
      *
           .
       500-RYAKUSYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁÈ¤ß¹ç¤ï¤»È½ÊÌ¡ÊÊ¡»ãÍÑ¡Ë¼çÊÝ¸±¡ÜÁ´¹ñ¸øÈñ¡ÜÃÏÊý¸øÈñ
      *****************************************************************
       500-KUMIAWASE-HANBETU-SEC               SECTION.
      *
           INITIALIZE                 TBL-FUKUSHI
      *
           IF  RECE62-SYUKEIKBN(IDX1 2) = "0" OR SPACE
             CONTINUE
           ELSE
             EVALUATE  RECE62-SYUKEI-TAIKBN(IDX1 2)
               WHEN     "0"
                   PERFORM  5001-KUMIAWASE-HENSYU-SEC
               WHEN     "1"
                 IF   RECE62-TEISYUTUSAKI  =   2 OR 6
                   PERFORM  5001-KUMIAWASE-HENSYU-SEC
                 END-IF
               WHEN     "2"
                 IF   RECE62-TEISYUTUSAKI  =   1
                   PERFORM  5001-KUMIAWASE-HENSYU-SEC
                 END-IF
               WHEN     "3"
                 IF   RECE62-TEISYUTUSAKI  =   2
                   PERFORM  5001-KUMIAWASE-HENSYU-SEC
                 END-IF
               WHEN     "4"
                 IF   RECE62-TEISYUTUSAKI  =   6
                   PERFORM  5001-KUMIAWASE-HENSYU-SEC
                 END-IF
             END-EVALUATE
           END-IF
      *
           .
       500-KUMIAWASE-HANBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁÈ¤ß¹ç¤ï¤»È½ÊÌÊÔ½¸
      *****************************************************************
       5001-KUMIAWASE-HENSYU-SEC               SECTION.
      *
           EVALUATE  RECE62-SYUKEIKBN(IDX1 2)
              WHEN      "1"
              WHEN      "3"
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(2)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(2)
                     MOVE  "1"  TO      RECE62-FUKUSHIFLG(3)
                     MOVE   1   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "3"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(3)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(3)
                     MOVE  "1"  TO      RECE62-FUKUSHIFLG(4)
                     MOVE   1   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "3"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(4)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(4)
                     MOVE  "1"  TO      RECE62-FUKUSHIFLG(5)
                     MOVE   1   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "3"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
              WHEN      "2"
              WHEN      "4"
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(2)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(2)
                     MOVE  "2"  TO      RECE62-FUKUSHIFLG(3)
                     MOVE   2   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "4"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(3)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(3)
                     MOVE  "2"  TO      RECE62-FUKUSHIFLG(4)
                     MOVE   2   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "4"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(4)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(4)
                     MOVE  "2"  TO      RECE62-FUKUSHIFLG(5)
                     MOVE   2   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "4"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
           END-EVALUATE
      *
           IF   TBL-FUKUSHIFLG(2)   NOT = ZERO
             EVALUATE   WRK-PREFNUM
               WHEN    "38"
                  IF   RECE62-TEISYUTUSAKI  =   2 OR 6
                     MOVE   1    TO     TBL-FUKUSHI-DAYFLG(2)
                  END-IF
               WHEN    "36"
                  IF  (RECE62-KOHNUMHC(IDX1 2) = "245"   )
                  AND (RECE62-KOHNUMHC(IDX1 3) = "748"   )
                  AND (RECE62-SRYYM           >= "200805")
                     IF   RECE62-KOHNUMHC(IDX1 3) =  RECE62-KOHNUM(2)
                     AND  RECE62-KOHIDHC(IDX1 3)  =  RECE62-KOHID(2)
                        EVALUATE  TBL-FUKUSHIFLG(2)
                          WHEN      1
                            MOVE    "1"   TO  RECE62-FUKUSHIFLG(3)
                          WHEN      2
                            MOVE    "2"   TO  RECE62-FUKUSHIFLG(3)
                        END-EVALUATE
                        MOVE   TBL-FUKUSHIFLG(2)
                                          TO  TBL-FUKUSHIFLG(3)
                        MOVE   RECE62-FTNKBNFLG(IDX1 2)
                                          TO  RECE62-FTNKBNFLG(IDX1 3)
                     END-IF
                     IF   RECE62-KOHNUMHC(IDX1 3) =  RECE62-KOHNUM(3)
                     AND  RECE62-KOHIDHC(IDX1 3)  =  RECE62-KOHID(3)
                        EVALUATE  TBL-FUKUSHIFLG(2)
                          WHEN      1
                            MOVE    "1"   TO  RECE62-FUKUSHIFLG(4)
                          WHEN      2
                            MOVE    "2"   TO  RECE62-FUKUSHIFLG(4)
                        END-EVALUATE
                        MOVE   TBL-FUKUSHIFLG(2)
                                          TO  TBL-FUKUSHIFLG(3)
                        MOVE   RECE62-FTNKBNFLG(IDX1 2)
                                          TO  RECE62-FTNKBNFLG(IDX1 3)
                     END-IF
                     IF   RECE62-KOHNUMHC(IDX1 3) =  RECE62-KOHNUM(4)
                     AND  RECE62-KOHIDHC(IDX1 3)  =  RECE62-KOHID(4)
                        EVALUATE  TBL-FUKUSHIFLG(2)
                          WHEN      1
                            MOVE    "1"   TO  RECE62-FUKUSHIFLG(5)
                          WHEN      2
                            MOVE    "2"   TO  RECE62-FUKUSHIFLG(5)
                        END-EVALUATE
                        MOVE   TBL-FUKUSHIFLG(2)
                                          TO  TBL-FUKUSHIFLG(3)
                        MOVE   RECE62-FTNKBNFLG(IDX1 2)
                                          TO  RECE62-FTNKBNFLG(IDX1 3)
                     END-IF
                  END-IF
             END-EVALUATE
           END-IF
      *
           .
       5001-KUMIAWASE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-NYUINACCT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
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
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×ÆÉ¹þ
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
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
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
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
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    Æþ±¡Î¬¾Î¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       900-NYURYAKUSHO-READ-SEC        SECTION.
      *
           MOVE     ZERO       TO    FLG-NYURYAKUSHO
      *
           PERFORM  910-NYURYAKUWRK-CHK-SEC
      *
           IF      FLG-NYURYAKUWRK     =   ZERO
               MOVE    NYURYAKUSHO-REC     TO  MCPDATA-REC
               MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM   900-DBFETCH-SEC
                   IF      MCP-RC               =   ZERO
                       MOVE    MCPDATA-REC      TO  NYURYAKUSHO-REC
                       MOVE    ZERO             TO  FLG-NYURYAKUSHO
                       PERFORM  910-NYURYAKUWRK-HEN-SEC
                   ELSE
                       MOVE    1                TO  FLG-NYURYAKUSHO
                       INITIALIZE                   NYURYAKUSHO-REC
                   END-IF
               ELSE
                   MOVE    1                    TO  FLG-NYURYAKUSHO
                   INITIALIZE                       NYURYAKUSHO-REC
               END-IF
      *
               MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       900-NYURYAKUSHO-READ-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡Î¬¾Î¥Þ¥¹¥¿¡¼ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       910-NYURYAKUWRK-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-NYURYAKUWRK
      *
           PERFORM VARYING WRK-NYURYAKU-IDX  FROM    1   BY  1
             UNTIL ( WRK-NYURYAKU-IDX  >   WRK-NYURYAKU-CONST-MAX ) OR
                   ( FLG-NYURYAKUWRK   =     1 )      OR
                   ( WRK-NYURYAKU-IDX  >   200 )
               IF    ( WRK-NYURYAKU-SRYCD (WRK-NYURYAKU-IDX)
                                       =   NYURYAKUSHO-SRYCD )
                 AND ( WRK-NYURYAKU-YUKOSTYMD (WRK-NYURYAKU-IDX)
                                       <=  NYURYAKUSHO-YUKOSTYMD)
                 AND ( WRK-NYURYAKU-YUKOEDYMD (WRK-NYURYAKU-IDX)
                                       >=  NYURYAKUSHO-YUKOSTYMD)
                   MOVE    1           TO  FLG-NYURYAKUWRK
                   MOVE    WRK-NYURYAKU-DATA (WRK-NYURYAKU-IDX)
                                       TO  NYURYAKUSHO-REC
               END-IF
           END-PERFORM
      *
           .
       910-NYURYAKUWRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡Î¬¾Î¥Þ¥¹¥¿¡¼ÂàÈò¥ï¡¼¥¯ÊÔ½¸½èÍý
      *****************************************************************
       910-NYURYAKUWRK-HEN-SEC               SECTION.
      *
           IF    ( WRK-NYURYAKU-CONST-MAX  >   WRK-NYURYAKU-MAX )
               COMPUTE WRK-NYURYAKU-MAX    =  WRK-NYURYAKU-MAX   +   1
               MOVE    NYURYAKUSHO-SRYCD   TO  WRK-NYURYAKU-SRYCD
                                                   (WRK-NYURYAKU-MAX)
               MOVE    NYURYAKUSHO-YUKOSTYMD
                                           TO  WRK-NYURYAKU-YUKOSTYMD
                                                   (WRK-NYURYAKU-MAX)
               MOVE    NYURYAKUSHO-YUKOEDYMD
                                           TO  WRK-NYURYAKU-YUKOEDYMD
                                                   (WRK-NYURYAKU-MAX)
               MOVE    NYURYAKUSHO-REC     TO  WRK-NYURYAKU-DATA
                                                   (WRK-NYURYAKU-MAX)
           END-IF
      *
           .
       910-NYURYAKUWRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁªÄêÎÅÍÜ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SENTEICDCHG-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
               MOVE    ZERO            TO  FLG-SENTEICDCHG
           ELSE
               INITIALIZE                  SENTEICDCHG-REC
               MOVE    1               TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       900-SENTEICDCHG-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß½èÍý
      **************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                                           SYS-1001-REC
                                           SYS-5000-REC
                                           SYS-2010-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
                                           SYS-1001-REC
                                           SYS-5000-REC
                                           SYS-2010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ãæ´Ö¥Õ¥¡¥¤¥ë£Ò£Å£Á£Ä½èÍý
      *****************************************************************
       900-RECE62-READ-SEC            SECTION.
      *
           READ    RECE62-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE62
               NOT  AT  END
                   ADD     1     TO    CNT-IN (IDZ)
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE62 " IDZ " PTNUM=" RECE62-PTNUM     
      *
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       900-RECE62-READ-EXT.
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
      *        °åÎÅµ¡´Ø£É£ÄÊÔ½¸
               MOVE  ZERO           TO  FLG-SET
               MOVE  RECE62-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                     UNTIL  ( SYSKAN-IDX              >  100  )
                      OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                      OR    ( FLG-SET                 =   1   )
      *
                 MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                 IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                 AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                    MOVE     1               TO    FLG-SET
                    MOVE   SYS-1001-PREFNUM  TO    WRK-PREFNUM
                 END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                  STRING
                    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£¿ÇÎÅÇ¯·î¡á"
                                               DELIMITED  BY  SIZE 
                       RECE62-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1              TO  FLG-RECE62
                  GO  TO  910-SYSKANRI-HENSYU-EXT
               END-IF
      *
               MOVE      RECE62-SRYYM   TO  WRK-SRYYM-5000
               PERFORM   10012-SYS-5000-CHK-SEC
      *        °åÎÅµ¡´Ø¸º»»¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
               INITIALIZE                 WRK-SHOKUJIRYOYO-OCC
                                          WRK-TOKUKANRIKASAN-OCC
      *
               MOVE     ZERO      TO  FLG-MIJISI-INNAI
                                      FLG-MIJISI-IRYO
                                      FLG-MIJISI-JYOKU
      *
               MOVE     SPACE     TO  TBL-GENSAN
               INITIALIZE             TBL-GENSAN
      *
               MOVE  ZERO         TO  FLG-SET
               MOVE  RECE62-SRYYM TO  WRK-SYS-SRYYMD(1:6)
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
                     PERFORM  10011-SYSKAN-HENSYU-SEC
                     IF        WRK-SYS5000-COUNT    =     ZERO
                       CONTINUE
                     ELSE
                       PERFORM  10013-SYS-5000-GET-SEC
                     END-IF
                  END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                  STRING
              "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£¿ÇÎÅÇ¯·î¡á"
                                               DELIMITED  BY  SIZE 
                       RECE62-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1              TO  FLG-RECE62
                  GO  TO  910-SYSKANRI-HENSYU-EXT
               END-IF
           END-IF
      *
           .
      *
       910-SYSKANRI-HENSYU-EXT.
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
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
