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
       PROGRAM-ID.         ORCS02.
      ****************************************************************
      *    OS           LINUX                                         
      *                                                               
      *    PROGRAM      ORCS02  /  ÉéÃ´¶â³Û»»Äê¥µ¥Ö                   
      *                            ¡ÊÆþ±¡ÍÑ¡Ë                         
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/10/22    NACL¡ÝÌçÏÆ    ¿·µ¬ºîÀ®
      ****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    03/03/07  Ï·¿Í¸ÂÅÙ³Û¤Î½¸·×ÊÑ¹¹
      *  01.00.02    NACL-ÌçÏÆ    03/03/10  ¿©»ö¶èÊ¬¡É£´¡É¤ÎÂÐ±þÄÉ²Ã
      *  01.00.03    NACL-ÌçÏÆ    03/03/25  ¹¹Àµ¡¦°éÀ®Åù¤Î´µ¼Ô¸øÈñÉéÃ´ÂÐ±þ
      *  01.00.04    NACL-ÌçÏÆ    03/04/16  Á¥°÷ÊÝ¸±¡ÊÈïÊÝ¸±¼Ô¡Ë¡ÜÄÌ¶ÐºÒ³²
      *                                     ¡Ü½é¿Ç»þ¡¡ÂÐ±þ
      *  01.00.05    NACL-ÌçÏÆ    03/05/07  ÃÏÊýÏ·¿Í¤ÎÉéÃ´³Û¤Î»Í¼Î¸ÞÆþ¤ÎÀßÄê
      *                                     ¤òÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¤ÎÀßÄê¤Ç¹Ô¤¦¡£
      *  01.00.06    NACL-ÌçÏÆ    03/05/08  Á¥°÷ÊÝ¸±¡Ê¿¦Ì³¾å¡Ë¤Î¿©»öÉéÃ´Ìµ¤·
      *                                     ¤Ë½¤Àµ
      *  01.00.07    NACL-ÌçÏÆ    03/05/21  ¸ºÌÈ¤ÎÃ¼¿ôÀßÄêÂÐ±þ
      *  01.00.08    NACL-ÌçÏÆ    03/06/06  ¾ÃÈñÀÇÃ¼¿ôÂÐ±þ
      *  01.00.09    NACL-ÌçÏÆ    03/07/01  ¥ì¥»µ­ºÜÍÑ°ìÉôÉéÃ´¶âÊÔ½¸½¤Àµ
      *  01.00.10    NACL-ÌçÏÆ    03/07/24  Ï«ºÒÈó»ØÄê°åÎÅµ¡´ØÂÐ±þ
      *                                     ¼«ÇåÀÕ´µ¼ÔÀÁµá£°¡óÂÐ±þ
      *                                     ¼«ÇåÀÕÊ¸½ñÎÁ¾ÃÈñÀÇÂÐ±þ
      *                                     ¼«ÇåÀÕ¤ÎÎÅÍÜÃ´Åö¼êÅöÂÐ±þ
      *                                     Ï«ºÒ¼«ÇåÀÕ¿©»öÎÅÍÜÈñ·×»»ÂÐ±þ
      *  01.00.11    NACL-ÌçÏÆ    03/08/27  ÆÃÄê¼À´µ²þÀµÂÐ±þ
      *  01.00.12    NACL-ÌçÏÆ    03/10/02  ÅìµþÅÔÃÏÊý¸øÈñÂÐ±þ
      *  01.00.13    NACL-ÌçÏÆ    03/12/02  ÆüÊÌ¼ýÇ¼¥Þ¥¹¥¿¹¹¿·Åù
      *  01.00.14    NACL-ÌçÏÆ    04/01/14  ¿ÇÎÅ¹Ô°Ù¤ÎÊñ³çÊ¬¤Ï½ü¤¯
      *                                     SRYACCT7¢ª33
      *  01.00.15    NACL-ÌçÏÆ    04/01/15  ¾ÃÈñÀÇ¹àÌÜÅùÄÉ²Ã¹àÌÜÊÔ½¸
      *  01.00.16    NACL-ÌçÏÆ    04/02/06  ¼«ÇåÀÕ¤Î¿©»öÎÅÍÜÈñ¤òÏ«ºÒ
      *                                     ¤Î£±¡¥£²ÇÜ¤Ç·×»»¤¹¤ë
      *  01.00.17    NACL-ÌçÏÆ    04/02/10  ¿©»öÉéÃ´³Û½õÀ®ÂÐ±þ
      *  01.00.18    NACL-ÌçÏÆ    04/02/10  À¸ÊÝ¤Î¿©»öÉéÃ´·×»»½¤Àµ
      *  01.00.19    NACL-ÌçÏÆ    04/02/10  À¸ÊÝ¤Î¸øÈñÉéÃ´¸ÂÅÙ³ÛÀßÄê
      *  01.00.20    NACL-ÌçÏÆ    04/03/12  ¸øÈñÉéÃ´±ßÃ±°ÌÊÔ½¸
      *  01.00.21    NACL-ÌçÏÆ    04/04/06  ¹âÎð¼Ô¥Õ¥é¥°ÊÔ½¸½¤Àµ
      *  01.00.22    NACL-ÌçÏÆ    04/07/27  ¹ñÊÝÉéÃ´³ä¹çÊÔ½¸½¤Àµ
      *  01.00.23    NACL-ÌçÏÆ    04/08/04  Æ±°ìÊÝ¸±ÈÖ¹æÂÐ±þ
      *  01.00.24    NACL-ÌçÏÆ    04/08/20  Ê¼¸Ë¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.00.25    NACL-ÌçÏÆ    04/09/07  £±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç
      *                                     ¤Î½¤Àµ
      *  01.00.26    NACL-ÌçÏÆ    04/09/07  ËÌ³¤Æ»ÃÏÊý¸øÈñÂÐ±þ
      *  01.00.27    NACL-ÌçÏÆ    04/09/08  À¸ÊÝÃ±ÆÈ¤Î¸ÂÅÙ³Û½¤Àµ
      *  01.00.28    NACL-ÌçÏÆ    04/09/10  ·§ËÜ¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.00.29    NACL-ÌçÏÆ    04/09/24  ÀÁµá³ÛÃ¼¿ô½èÍýÂÐ±þ
      *  01.00.30    NACL-ÌçÏÆ    04/10/01  ¼«ÇåÀÕÆÃÄê´ïºàÅù¾ÃÈñÀÇÂÐ±þ
      *  01.00.31    NACL-ÌçÏÆ    04/10/21  ¸øÈñ»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç¤Î½¤Àµ
      *  01.00.32    NACL-ÌçÏÆ    04/11/12  Ê¡²¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.00.33    NACL-ÌçÏÆ    04/11/15  ¸øÈñ·î¾å¸Â³Û¼èÆÀ½¤Àµ
      *  01.00.34    NACL-ÌçÏÆ    04/11/24  ºë¶Ì¸©£´£±Ï·¿Í¤¬Ä¹´ü¤ÈÊ»ÍÑ¤Î
      *                                     ¾ì¹ç¡¢£±£°£°£°£°±ß¾å¸Â¤È¤¹¤ë
      *  01.00.35    NACL-ÌçÏÆ    04/11/25  ÂçºåÉÜ£´£±Ï·¿Í¾å¸Â³ÛÊÔ½¸
      *  01.00.36    NACL-ÌçÏÆ    04/11/25  Ê¡Åç¸©ÆýÍÄ»ùÆÃ¼ì·×»»ÂÐ±þ
      *  01.00.37    NACL-ÌçÏÆ    05/01/14  ²¬»³¸©ÆýÍÄ»ùÆÃ¼ì·×»»ÂÐ±þ
      *  01.00.38    NACL-ÌçÏÆ    05/01/25  ËÌ³¤Æ»ÃÏÊý¸øÈñ½¤Àµ
      *  01.00.39    NACL-ÌçÏÆ    05/03/03  ¿©»ö¤Ë´Ø¤¹¤ëÊÔ½¸½¤Àµ
      *  01.00.40    NACL-ÌçÏÆ    05/03/04  ÀÅ²¬¸©Êì»Ò¾ã³²¤¬Ä¹´ü¤ÈÊ»ÍÑ¤Î
      *                                     ¾ì¹ç¡¢£±£°£°£°£°±ß¾å¸Â¤È¤¹¤ë
      *  01.00.41    NACL-ÌçÏÆ    05/03/07  ËÌ³¤Æ»¡¢Âçºå¡¢¹­Åç£´£±Ï·¿Í¤Î
      *                                     £·£²£³£°£°Ä¶¡¢Äã½êÆÀÄãÇ¯¶âÂÐ±þ
      *  01.00.42    NACL-ÌçÏÆ    05/03/23  ÊÝ¸±ÁÈ¹ç¤»¤Î¾å¸Â¶èÊ¬ÊÔ½¸
      *  01.01.01    NACL-ÌçÏÆ    05/03/24  ¾®»ùÆÃÄê¾å¸ÂÂÐ±þ
      *  01.01.02    NACL-ÌçÏÆ    05/03/24  ¿·³ã¸©ÃÏÊý¸øÈñ¡§Æü¸ÇÄê³ÛÂÐ±þ
      *  01.01.03    NACL-ÌçÏÆ    05/04/13  ÀÅ²¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.01.04    NACL-ÌçÏÆ    05/05/06  ¸øÈñ°ìÉôÉéÃ´¶â¤È¿©»öÉéÃ´¤ò
      *                                     ÊÌ¡¹¤ËÈæ³Ó¤·¡¢¤½¤ì¤¾¤ìÃÍ¤Î¾®¤µ
      *                                     ¤¤¤â¤Î¤ò´µ¼ÔÉéÃ´¤È¤¹¤ë
      ****************************************************************
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
       01  KEISAN-AREA.
           03  WK-KNJFTNKIN            PIC 9(10).
           03  WK-KNJFTNKIN-ENTANI     PIC 9(08)V9(01).
           03  WK-ENTANI               PIC 9(08).
           03  WK-ZENGAKU              PIC 9(10).
           03  WK-ZENGAKU-SYU          PIC 9(10).
           03  WK-ZENGAKU-YAK          PIC 9(10).
           03  WK-FUTANWARI            PIC 9V99.
           03  WK-SEIKYUGK             PIC 9(10).
           03  WK-NYUKFTNRATE          PIC 9(03).
           03  WK-NYUSKJJYOGAK         PIC 9(06).
           03  WK-NYUSKJJYOGAK2        PIC 9(09).
           03  WK-TOKUTEI-COMPFTN      PIC 9(08).
           03  WK-TOKUTEI-COMPFTN2     PIC 9(08).
           03  WK-TOKUTEI-COMPFTN3     PIC 9(08).
       01  FLG-AREA.
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-TNKRRK              PIC 9(01).
           03  FLG-TSYRRK              PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-GENMEN              PIC 9(01).
           03  FLG-SANSHOU2            PIC X(01).
           03  FLG-SANSHOU3            PIC X(01).
           03  FLG-SANTEI1             PIC X(01).
           03  FLG-SANTEI2             PIC X(01).
           03  FLG-SANTEI3             PIC X(01).
           03  FLG-SANTEI4             PIC X(01).
           03  FLG-NG                  PIC X(01).
           03  FLG-HIKAKU              PIC 9(01).
           03  FLG-KUMAMOTO            PIC X(01).
           03  FLG-TAIGAI              PIC X(01).
           03  FLG-HIKOTEI             PIC 9(01).
      *
       01  WRK-AREA.
           03  WK-PAYKBN               PIC X(02).
           03  WK-1001-HOSPID          PIC X(24).
      *
      *    Äã½êÆÀ£±¤ÎÄ¹´üÆþ±¡Ç§ÄêÇ¯·îÆü
           03  WRK-SKJGNSTYMD-S        PIC X(08).
           03  WRK-SKJGNSTYMD-E        PIC X(08).
           03  WRK-SKJGNSTYMD2.
               05  WRK-SKJGNSTYYMM2    PIC 9(06).
               05  WRK-SKJGNSTDD2      PIC 9(02).
      *
           03  WK-NYUJGNKBN            PIC X(01).
           03  WK-NYUJGNGAK            PIC 9(07).
      *
           03  WK-DAYTAIHI-FTNMONEY    PIC 9(07).
           03  WK-DAYTAIHI-COMPFTN     PIC 9(07).
           03  WK-DAYTAIHI-COMPFTNWK   PIC 9(07).
      *
      *    ÊÝ¸±¶èÊ¬¡Ê¶õÇò¡§°åÊÝ¡¡£±¡§Ï«ºÒ¡¦¼«Çå¡Ë
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
           03  WK-TAIHI-RECEFTNDSPKBN-OCC.
               05  WK-TAIHI-RECEFTNDSPKBN
                                       PIC X(1)    OCCURS   4.
      *    Ê¡²¬¸©ÃÏÊý¸øÈñÉéÃ´¶èÊ¬¡¦¼çÊÝ¸±¤ÎÉéÃ´³ä¹ç
           03  WK-KOH-FTNKBN           PIC X(1).
           03  WK-KOH-FTNRATE          PIC 9(3).
      *
           03  WK-TAIHI-NYUJGNGAK-OCC.
               05  WK-TAIHI-NYUJGNGAK  PIC 9(7)    OCCURS   4.
      *
           03  WK-TAIHI-SDAY-COMPFTN   PIC 9(7).
           03  WK-TAIHI-SDAY-COMPFTNWK PIC 9(7).
           03  WK-TAIHI-SDAY-FTNMONEY  PIC 9(7).
      *
      *****Ï«ºÒ¡¦¼«ÇåÀÕ
      *    ¼«ÇåÀÕ¡Ê²Ã»»Î¨¡Ë
           03  WK-4001-KSNRATE         PIC 9(2).
      *    ¼«ÇåÀÕÊ¸½ñÎÁ¾ÃÈñÀÇ
           03  WK-4001-TAXKBN          PIC X(1).
      *    ÅÀ¿ôÃ±²Á¶èÊ¬
           03  WK-4001-TENKBN          PIC X(1).
      *    ¼«ÇåÀÕ¿©»öÎÅÍÜÈñ¶èÊ¬
           03  WK-4001-SKJKBN          PIC X(1).
      *    ¼«ÇåÀÕÆÃÄê´ïºàÅù¾ÃÈñÀÇ
           03  WK-4001-TAXKBN-KZI      PIC X(1).
      *
      *    ¼«ÇåÀÕÆÃÄê´ïºàÅù¾ÃÈñÀÇÂÐ¾Ý³°¥³¡¼¥É
           03  WK-4003-TAIGAI.
               05  WK-4003-TAIGAI-OCC      OCCURS  10.
                 06  WK-4003-TAIGAI-SRYCD  PIC  X(09).
      *
      *    ¼«ÇåÀÕ¤½¤ÎÂ¾¥³¡¼¥É¡Ê£°£¹£µ£¹£±¡¥£¹£²¡¥£¹£³¡Ë¤Î¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÅù
           03  WK-SONOTA91.
               05  WK-SONOTA91-OCC         OCCURS  50.
                 06  WK-SONOTA91-SRYCD     PIC  X(09).
                 06  WK-SONOTA91-ZAIKAISU  PIC  9(07).
                 06  WK-SONOTA91-TANKA     PIC  9(07).
           03  WK-SONOTA92.
               05  WK-SONOTA92-OCC         OCCURS  50.
                 06  WK-SONOTA92-SRYCD     PIC  X(09).
                 06  WK-SONOTA92-ZAIKAISU  PIC  9(07).
                 06  WK-SONOTA92-TANKA     PIC  9(07).
           03  WK-SONOTA93.
               05  WK-SONOTA93-OCC         OCCURS  50.
                 06  WK-SONOTA93-SRYCD     PIC  X(09).
                 06  WK-SONOTA93-ZAIKAISU  PIC  9(07).
                 06  WK-SONOTA93-TANKA     PIC  9(07).
      *
           03  WK-ZAN-KOHI-D           PIC 9(05).
           03  WK-DAY-NYUKFTNRATE      PIC 9(03).
           03  WK-DAY-COMPFTN          PIC 9(05).
           03  WK-RYOYO                PIC 9(07).
           03  WK-KSNRATE9             PIC 9(03).
           03  WK-KSNRATE              PIC 9V99. 
           03  WK-BRMSGK-TAXKBN        PIC X(01).
           03  WK-KEISAN               PIC 9(09).
           03  WK-KEISAN1              PIC 9(09).
           03  WK-KEISAN2              PIC 9(09).
           03  WK-KEISAN3              PIC 9(09).
           03  WK-KEISAN4              PIC 9(09).
           03  WK-KEISAN9              PIC 9(09).
           03  WK-KEISAN-SKJ           PIC 9(09).
           03  WK-KEISAN1-SKJ          PIC 9(09).
           03  WK-KEISAN2-SKJ          PIC 9(09).
           03  WK-KEISAN3-SKJ          PIC 9(09).
           03  WK-KEISAN4-SKJ          PIC 9(09).
           03  WK-KEISAN9-SKJ          PIC 9(09).
           03  WK-KEISAN-ENTANI        PIC 9(08)V9(01).
           03  WK-KEISAN1-ENTANI       PIC 9(08)V9(01).
           03  WK-KEISAN2-ENTANI       PIC 9(08)V9(01).
           03  WK-KEISAN3-ENTANI       PIC 9(08)V9(01).
           03  WK-KEISAN4-ENTANI       PIC 9(08)V9(01).
           03  WK-KEISAN9-ENTANI       PIC 9(08)V9(01).
      *    03  WK-NISSU1               PIC 9(05).
      *    03  WK-NISSU2               PIC 9(05).
      *
      *    ÆÃ¼ì·×»»ÍÑ
           03  WK-TOKUSYU-COMPFTN      PIC 9(07).
           03  WK-TOKUSYU-COMPFTN2     PIC 9(07).
           03  WK-TOKUSYU-ENTANI       PIC 9(07)V9(01).
           03  WK-TOKUSYU              PIC 9(07).
           03  WK-TOKUSYU2             PIC 9(07).
      *
      *    ¸øÈñ¤Î´µ¼ÔÉéÃ´¾å¸Â¶â³Û¤ÎÂàÈòÍÑ¥¨¥ê¥¢
       01  HOKEN-MAX-AREA.
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
      *
      *    ¸øÈñ¤Î»ÈÍÑ²ó¿ô¥«¥¦¥ó¥ÈÍÑ¥¨¥ê¥¢¡ÊÆüÃ±°Ì¡Ë
       01  HOKEN-KAISU-TABLE.
           03  HOKEN-KAISU-TABLE1.
             04 HOKEN-KAISU-TBL        OCCURS  32.
               05  KOHI1-TBL           PIC 9(02).
               05  KOHI2-TBL           PIC 9(02).
               05  KOHI3-TBL           PIC 9(02).
               05  KOHI4-TBL           PIC 9(02).
           03  HOKEN-KAISU-TBL-R   REDEFINES   HOKEN-KAISU-TABLE1
                                       OCCURS  32.
               05  KOHI-TBL            PIC 9(02)   OCCURS   4.
      *
      *    ¸øÈñ¤Î´µ¼ÔÉéÃ´ºÑ¤ß¶â³Û¤Î²Ã»»ÍÑ¥¨¥ê¥¢
       01  HOKEN-FUTAN-AREA.
           03  FTN-KOHI1-D             PIC 9(05)   OCCURS  31.
           03  FTN-KOHI2-D             PIC 9(05)   OCCURS  31.
           03  FTN-KOHI3-D             PIC 9(05)   OCCURS  31.
           03  FTN-KOHI4-D             PIC 9(05)   OCCURS  31.
           03  FTN-KOHI1-M             PIC 9(08).
           03  FTN-KOHI2-M             PIC 9(08).
           03  FTN-KOHI3-M             PIC 9(08).
           03  FTN-KOHI4-M             PIC 9(08).
      *
      *    ¸øÈñ¤Î¥ì¥»¥×¥ÈÉéÃ´¶èÊ¬»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç,
      *    ¥ì¥»µ­ºÜ¶â³Û¤Î²Ã»»ÍÑ¥¨¥ê¥¢
       01  HOKEN-FUTAN-AREA2.                                           
           03  FTN-KOHI1-D2            PIC 9(05)   OCCURS  31.
           03  FTN-KOHI2-D2            PIC 9(05)   OCCURS  31.
           03  FTN-KOHI3-D2            PIC 9(05)   OCCURS  31.
           03  FTN-KOHI4-D2            PIC 9(05)   OCCURS  31.
           03  FTN-KOHI1-M2            PIC 9(08).                       
           03  FTN-KOHI2-M2            PIC 9(08).                       
           03  FTN-KOHI3-M2            PIC 9(08).                       
           03  FTN-KOHI4-M2            PIC 9(08).                       
      *
      *    ¸øÈñ¤ÎÉéÃ´¶â³Û±ßÃ±°ÌÍÑ                                       
       01  HOKEN-FUTAN-AREA3.
           03  FTN-KOHI1-D3            PIC 9(05)   OCCURS  31.
           03  FTN-KOHI2-D3            PIC 9(05)   OCCURS  31.
           03  FTN-KOHI3-D3            PIC 9(05)   OCCURS  31.
           03  FTN-KOHI4-D3            PIC 9(05)   OCCURS  31.
           03  FTN-KOHI1-M3            PIC 9(08).
           03  FTN-KOHI2-M3            PIC 9(08).
           03  FTN-KOHI3-M3            PIC 9(08).
           03  FTN-KOHI4-M3            PIC 9(08).
      *
      *    ¸øÈñ¤Î´µ¼ÔÉéÃ´²ÄÇ½¤Ê»Ä³Û»»ÄêÍÑ¥¨¥ê¥¢
       01  HOKEN-ZAN-AREA.
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
      *    ¸øÈñ¤Î´µ¼ÔÉéÃ´²ÄÇ½¤Ê»Ä³Û»»ÄêÍÑ¥¨¥ê¥¢¡ÊÆüÃ±°Ì¡Ë
       01  HOKEN-ZAN-D-AREA.
           03  HOKEN-ZAN-D-TABLE1.
             04  HOKEN-ZAN-D-TBL       OCCURS  31.
               05  ZAN-KOHI1-D         PIC 9(05).
               05  ZAN-KOHI2-D         PIC 9(05).
               05  ZAN-KOHI3-D         PIC 9(05).
               05  ZAN-KOHI4-D         PIC 9(05).
           03  HOKEN-ZAN-D-TBL-R   REDEFINES   HOKEN-ZAN-D-TABLE1
                                       OCCURS  31.
               05  ZAN-KOHI-D          PIC 9(05)   OCCURS   4.
      *
      *    ¸øÈñ¤Î¥ì¥»¥×¥ÈÉéÃ´¶èÊ¬»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç,
      *    ¥ì¥»µ­ºÜ²ÄÇ½¤Ê»Ä³Û»»ÄêÍÑ¥¨¥ê¥¢
       01  HOKEN-ZAN-AREA2.                                             
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
      *    ¸øÈñ¤Î¥ì¥»¥×¥ÈÉéÃ´¶èÊ¬»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç,
      *    ¥ì¥»µ­ºÜ²ÄÇ½¤Ê»Ä³Û»»ÄêÍÑ¥¨¥ê¥¢¡ÊÆüÃ±°Ì¡Ë
       01  HOKEN-ZAN-D2-AREA.
           03  HOKEN-ZAN-D2-TABLE1.
             04  HOKEN-ZAN-D2-TBL       OCCURS  31.
               05  ZAN-KOHI1-D2         PIC 9(05).
               05  ZAN-KOHI2-D2         PIC 9(05).
               05  ZAN-KOHI3-D2         PIC 9(05).
               05  ZAN-KOHI4-D2         PIC 9(05).
           03  HOKEN-ZAN-D2-TBL-R   REDEFINES   HOKEN-ZAN-D2-TABLE1
                                        OCCURS  31.
               05  ZAN-KOHI-D2          PIC 9(05)   OCCURS   4.
      *
      *    ¸øÈñ¤ÎÉéÃ´¶â³Û±ßÃ±°ÌÍÑ                                       
       01  HOKEN-ZAN-AREA3.
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
      *    ¸øÈñ¤ÎÉéÃ´¶â³Û±ßÃ±°ÌÍÑ                                       
      *    ¡ÊÆüÃ±°Ì¡Ë
       01  HOKEN-ZAN-D3-AREA.
           03  HOKEN-ZAN-D3-TABLE1.
             04  HOKEN-ZAN-D3-TBL       OCCURS  31.
               05  ZAN-KOHI1-D3         PIC 9(05).
               05  ZAN-KOHI2-D3         PIC 9(05).
               05  ZAN-KOHI3-D3         PIC 9(05).
               05  ZAN-KOHI4-D3         PIC 9(05).
           03  HOKEN-ZAN-D3-TBL-R   REDEFINES   HOKEN-ZAN-D3-TABLE1
                                        OCCURS  31.
               05  ZAN-KOHI-D3          PIC 9(05)   OCCURS   4.
      *
      *    ¸øÈñ¤Î¥ì¥»¥×¥ÈÉéÃ´¶èÊ¬»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç,
      *    ¥ì¥»µ­ºÜ¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢
       01  WK-KOH-COMPFTN-AREA.                                         
           03  WK-KOH-COMPFTN          PIC 9(07)                        
                                        OCCURS  4.                      
      *
      *    ÆüÃ±°Ì¤Î¿©»öÉéÃ´¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢
       01  WK-DAY-SKJFTN-AREA.
           03  WK-DAY-SKJFTN           PIC 9(05)
                                        OCCURS  31.
      *
      *    ¸øÈñ¤ÎÆüÃ±°Ì»»Äê¥Õ¥é¥°
       01  WK-DAYINFFLG-AREA.
           03  WK-DAYINFFLG            PIC 9(01)
                                        OCCURS  4.
      *
      *    ¸øÈñ¤Î¥ì¥»¥×¥ÈÉéÃ´¶èÊ¬»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç,
      *    ¥ì¥»µ­ºÜ¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢¡ÊÆüÃ±°Ì¡Ë
       01  WK-DAY-KOH-COMPFTN-AREA.
           03  WK-DAY-KOH-COMPFTN      PIC 9(07)
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
      *    Åº¤¨»úÎÎ°è
       01  IDX-AREA.
           03  IDX                     PIC  9(02)  VALUE  ZERO.
           03  IDX2                    PIC  9(02)  VALUE  ZERO.
           03  IDX4                    PIC  9(03).
           03  IDX5                    PIC  9(03).
           03  IDX6                    PIC  9(03).
           03  IDX7                    PIC  9(03).
           03  IDX8                    PIC  9(03).
           03  IDX-ST                  PIC  9(02)  VALUE  ZERO.
           03  IDX-ED                  PIC  9(02)  VALUE  ZERO.
           03  IDY                     PIC  9(02)  VALUE  ZERO.
           03  IDZ                     PIC  9(02)  VALUE  ZERO.
           03  HKNNYU-IDX              PIC  9(02)  VALUE  ZERO.
           03  KOH-IDX                 PIC  9(02)  VALUE  ZERO.
           03  KOH-IDX2                PIC  9(02)  VALUE  ZERO.
           03  KOH-IDX3                PIC  9(02)  VALUE  ZERO.
      *
      *    ¹âÎð¼Ô¤ÎÉéÃ´¾å¸Â»»Äê¥¨¥ê¥¢
       01  WK-SYUHKN-TAIHI-AREA.
           03  MAX-KIN-SYU-M           PIC 9(08).
           03  FTN-SYU-M               PIC 9(08).
           03  ZAN-SYU-M               PIC 9(08).
           03  FLG-SANTEI-SYU          PIC X(01).
           03  FLG-KOUREI              PIC 9(01).
           03  FLG-IRYOHI              PIC 9(01).
           03  FLG-IRYOHI2             PIC 9(01).
           03  FLG-KOUGAKUCYO          PIC 9(01).
           03  FLG-KOUGAKUCYO2         PIC 9(01).
           03  FLG-SKJ                 PIC 9(01).
           03  WK-SYU-SYUCOMPFTN       PIC 9(08).
           03  WK-KOUREI-IRYOHI        PIC 9(08).
           03  WK-KOUREI-IRYOHI2       PIC 9(08).
           03  FTN-KOH-XXX             PIC 9(08).                       
           03  FTN-KOH-XXX-ENTANI      PIC 9(08).                       
           03  FTN-KOH-YYY             PIC 9(08).                       
           03  FTN-KOH-YYY-ENTANI      PIC 9(08).                       
           03  FTN-KOH-ZZZ             PIC 9(08).                       
           03  FTN-KOH-ZZZ-ENTANI      PIC 9(08)V9(01).                 
      *
           03  FTN-KOHI1-M-ENTANI      PIC 9(08).                       
           03  ZAN-KOHI1-ENTANI        PIC 9(08).                       
           03  FTN-SYU-M-ENTANI        PIC 9(08).                       
           03  ZAN-SYU-M-ENTANI        PIC 9(08).                       
           03  FLG-SANTEI1-ENTANI      PIC X(01).                       
           03  FLG-SANTEI-SYU-ENTANI   PIC X(01).                       
      *
           03  FLG-ROUJINHOKEN         PIC X(01).
      *
      *    ¿©»öÉéÃ´»»Äê¥¨¥ê¥¢
       01  WK-SHOKUJI-TAIHI-AREA.
           03  WK-SHOKUJIRYOYO         PIC 9(01).
           03  WK-TOKUKANRIKASAN       PIC 9(01).
           03  WK-SHOKUJI-NISSU        PIC 9(02).
           03  WK-SHOKUJI-RYOYOHI      PIC 9(07).
           03  WK-SHOKUJI-RYOYOHI95    PIC 9(07).
           03  WK-SHOKUJI-RYOYOHI96    PIC 9(07).
           03  WK-SHOKUJI-JIHI         PIC 9(07).
           03  WK-SHOKUJI-FTN          PIC 9(07).
           03  WK-SHOKUJI-FTN95        PIC 9(07).
           03  WK-SHOKUJI-FTN96        PIC 9(07).
           03  WK-NYUSKJRYYKBN         PIC X(01).
           03  WK-NISSU-SKJ            PIC 9(02).
           03  WK-KNJFTNKIN-SKJ        PIC 9(07).
           03  WK-RYOYOHI-SKJ          PIC 9(07).
           03  WK-SRYCD                PIC X(09).
      *    03  WK-TSUSAN-NISSU         PIC 9(04).
      *    03  WK-SEIKYUKIKAN-NISSU    PIC 9(04).
      *    ¿©Æ²²Ã»»ÂàÈò¥Æ¡¼¥Ö¥ë
       01  WK-SHOKUDOKASAN-TABLE.
           03  WK-SHOKUDOKASAN-T       OCCURS   31.
               05  WK-SHOKUDOKASAN     PIC 9(04).
      *    ³°ÇñÂàÈò¥Æ¡¼¥Ö¥ë
       01  WK-GAIHAKU-TABLE.
           03  WK-GAIHAKU-T            OCCURS   31.
               05  WK-GAIHAKU          PIC 9(04).
      *    Ï«ºÒÆþ±¡Ãæ¤ÎÆÃÊÌ¿©²Ã»»ÂàÈò¥Æ¡¼¥Ö¥ë
       01  WK-ROSAITOKU-TABLE.
           03  WK-ROSAITOKU-T          OCCURS   31.
               05  WK-ROSAITOKU95      PIC 9(04).
               05  WK-ROSAITOKU96      PIC 9(04).
               05  WK-ROSAITOKU97      PIC 9(04).
      *
      *ÃÏÊý¸øÈñÍÑ¥Õ¥é¥°¡Ê¸øÈñ¸ÂÅÙ³Û·èÄêÍÑ¡Ë
       01  WK-CHIHOU-FLG-TAIHI-AREA.
           03  FLG-CHIHOU              PIC X(01)  OCCURS  4.
      *
      *¼çÊÝ¸±ÉéÃ´¶â³Û¤Î¸øÈñ¸ÂÅÙ³ÛÂÐ±þÍÑ¥¨¥ê¥¢
       01  WK-SYUHEN-TAIHI-AREA.
           03  FLG-SYUHEN              PIC X(01).
      *
           03  FLG-SANTEI1-HEN         PIC X(01).
           03  FLG-SANTEI-SYU-HEN      PIC X(01).
      *
           03  FTN-KOHI1-M-HEN         PIC 9(08).
           03  ZAN-KOHI1-HEN           PIC 9(08).
      *
           03  FTN-SYU-M-HEN           PIC 9(08).
           03  ZAN-SYU-M-HEN           PIC 9(08).
      *
           03  WK-HEN-SYUCOMPFTN       PIC 9(07).
           03  WK-HEN-KOH1COMPFTN      PIC 9(07).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼
       01  SYU-SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ÆüÊÌ¼ýÇ¼¥Þ¥¹¥¿
       01  SYU-SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
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
      *
      *    Æþ±¡²ñ·×
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSK1001.INC".
           COPY    "CPSK1026.INC".
           COPY    "CPSK1027.INC".
           COPY    "CPSK1037.INC".
           COPY    "CPSK4001.INC".
           COPY    "CPSK4003.INC".
           COPY    "CPSK5000.INC".
      *
      *    ´µ¼Ô¾ðÊó
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *    ¸øÈñ·î¾å¸Â³Û¼èÆÀ¥µ¥Ö
           COPY    "CPORCSJGNGET.INC".
      *    ¸øÈñÉÕ²Ã¾ðÊó¼èÆÀ¥µ¥Ö
           COPY    "CPORCSKOHPLUS.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      /
      ***
       LINKAGE             SECTION.
           COPY  "MCPAREA". 
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-WRK-SYUNOU-REC-T.
         02 LNK-WRK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"  
                               REPLACING  //SYU-// BY //LNK-WRK-SYU-//.
       01  LNK-SYUDAY-REC.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
       01  LNK-WRK-SYUDAY-REC-T.
         02 LNK-WRK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"
                             REPLACING  //SDAY-// BY //LNK-WRK-SDAY-//.
      /
       PROCEDURE           DIVISION  USING
                                     MCPAREA
                                     LNK-ORCS02-REC
                                     LNK-SYUNOU-REC
                                     LNK-WRK-SYUNOU-REC-T
                                     LNK-SYUDAY-REC
                                     LNK-WRK-SYUDAY-REC-T.
      ******************************************************************
      *        °ÍÍêÀ©¸æ½èÍý
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
      **   MOVE    ZERO            TO  LNK-S02-RC.
           MOVE    SPACE           TO  KEISAN-AREA.
           INITIALIZE                  KEISAN-AREA.
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
           MOVE    SPACE           TO  LNK-SYU-SYUYKZFTNKBN.
           PERFORM   VARYING   IDX   FROM   1    BY   1
             UNTIL   IDX   >   4
                  MOVE    ZERO     TO  LNK-SYU-KOH-HKNFTNMONEY(IDX)
                                       LNK-SYU-KOH-COMPFTN(IDX)
                                       LNK-SYU-KOH-COMPFTN-ENTANI(IDX)
                                       LNK-SYU-KOH-COMPYKZFTN(IDX)
                                       LNK-SYU-KOH-COMPTOTAL(IDX)
                  MOVE    SPACE    TO  LNK-SYU-KOH-YKZFTNKBN(IDX)
           END-PERFORM.
           MOVE    ZERO            TO  LNK-SYU-RESE-YKZ-FTN.
      *
           PERFORM   VARYING   IDX   FROM   1    BY   1
             UNTIL   IDX   >   7
                  MOVE    ZERO     TO  LNK-SYU-SHOKUJI(IDX)
                                       LNK-SYU-SHOKUJI-NISSU(IDX)
           END-PERFORM.              
           MOVE           ZERO     TO  LNK-SYU-SYUSKJ-NISSU
                                       LNK-SYU-SYUSKJ-RYOYOHI
                                       LNK-SYU-SYUSKJ-FTN
                                       LNK-SYU-SYUSKJ-FTN-RECE.
           MOVE           SPACE    TO  LNK-SYU-SYUSKJ-FTNKBN.
           PERFORM   VARYING   IDX   FROM   1    BY   1
             UNTIL   IDX   >   4
                  MOVE    ZERO     TO  LNK-SYU-SKJ-NISSU(IDX)
                                       LNK-SYU-SKJ-RYOYOHI(IDX)
                                       LNK-SYU-SKJ-FTN(IDX)
                                       LNK-SYU-SKJ-FTN-RECE(IDX)
                  MOVE    SPACE    TO  LNK-SYU-SKJ-FTNKBN(IDX)
           END-PERFORM.
           MOVE           ZERO     TO  LNK-SYU-RYOYOHI-SKJ.
           MOVE           ZERO     TO  LNK-SYU-SKYMONEY-SKJ.
           MOVE           ZERO     TO  LNK-SYU-SKYMONEY-SKJ-TAX.
           MOVE           ZERO     TO  LNK-SYU-SKYMONEY-SKJ-KEI.
           MOVE           ZERO     TO  LNK-SYU-RYOYOHI-SKJ-JIHI.
           MOVE           ZERO     TO  LNK-SYU-SKYMONEY-SKJ-JIHI.
           MOVE           ZERO     TO  LNK-SYU-SKYMONEY-SKJ-JIHI-TAX.
           MOVE           ZERO     TO  LNK-SYU-SKYMONEY-SKJ-JIHI-KEI.
      *
           MOVE    ZERO            TO  LNK-SYU-TGMONEY-TAX-SAI
                                       LNK-SYU-RSI-TAX-SAI
                                       LNK-SYU-RSI-TOTAL
                                       LNK-SYU-RSIJIBAI-SKYMONEY
                                       LNK-SYU-SKYMONEY-TAX-SAI
                                       LNK-SYU-RMSAGAKU-TAX-SAI.
           PERFORM   VARYING   IDX   FROM   1    BY   1
             UNTIL   IDX   >   3
                  MOVE    ZERO     TO  LNK-SYU-SKJ-FTNGAKU(IDX)
                                       LNK-SYU-SKJ-FTNDAY(IDX)
           END-PERFORM.
           MOVE    SPACE           TO  LNK-SYU-DAYINFFLG.
           MOVE    SPACE           TO  LNK-SYU-DISCOUNT-KBN
                                       LNK-SYU-DISCOUNT-BODY
                                       LNK-SYU-DISCOUNT-RATEKBN
                                       LNK-SYU-DISCOUNT-TEIRITU.
           MOVE    ZERO            TO  LNK-SYU-DISCOUNT-RATE.
      *
      *    ÀÁµá¶â³ÛÃ¼¿ô¶èÊ¬¡¢¾ÃÈñÀÇÃ¼¿ô¶èÊ¬¤Î¼è¤ê¹þ¤ß
           INITIALIZE                         SYS-1001-REC.
           MOVE    "1001"               TO    SYS-1001-KANRICD.
           MOVE    "*"                  TO    SYS-1001-KBNCD.
           MOVE    LNK-SYU-SRYYMD       TO    ORC-DBYMD.
           MOVE    SYS-1001-REC         TO    MCPDATA-REC.
           MOVE    "DBSELECT"           TO    MCP-FUNC.
           MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH.
           CALL    "ORCMCPSUB"    USING       MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC.
           IF      MCP-RC               =     ZERO
               MOVE     "SYSKANRI-KEY"  TO    ORC-DBPATH
               PERFORM   SYSKANRI-READ-SEC
               MOVE      SYS-1001-DISCOUNT-HASU-KBN  
                                        TO    WK-HASU1001-KBN
               MOVE      SYS-1001-DISCOUNT-HASU-KBN2 
                                        TO    WK-HASU1001-KBN2
               MOVE      SYS-1001-TAX-HASU-KBN       
                                        TO    WK-TAX1001-KBN
               MOVE      SYS-1001-PREFNUM
                                        TO    WK-1001-PREFNUM
               MOVE      SYS-1001-HOSPID
                                        TO    WK-1001-HOSPID
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£´£°£°£±¡Ë¤«¤éÅÀ¿ôÃ±²ÁÅù¤Î¼èÆÀ
           MOVE     ZERO      TO    WK-4001-KSNRATE.
           MOVE     SPACE     TO    WK-4001-TAXKBN.
           MOVE     SPACE     TO    WK-4001-TENKBN.
           MOVE     SPACE     TO    WK-4001-SKJKBN.
           MOVE     SPACE     TO    WK-4001-TAXKBN-KZI.
           IF   LNK-SYU-SYUHKNNUM         =    "971" OR "973"
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
                   MOVE   SYS-4001-KSNRATE  TO    WK-4001-KSNRATE
                   MOVE   SYS-4001-TAXKBN   TO    WK-4001-TAXKBN
                   MOVE   SYS-4001-TENKBN   TO    WK-4001-TENKBN
                   MOVE   SYS-4001-SKJKBN   TO    WK-4001-SKJKBN
                   MOVE   SYS-4001-TAXKBN-KZI
                                            TO    WK-4001-TAXKBN-KZI
               END-IF
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           END-IF.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¤Î»²¾ÈÉôÊ¬³ÎÄê
           PERFORM   100-KAKUTEI-SEC.
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
                   MOVE    "DBCLOSECURSOR"  TO      MCP-FUNC
                   MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
                   CALL    "ORCMCPSUB"       USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
                   DISPLAY "HKNCOMBI DBFETCH ERR"
                   MOVE     10          TO      LNK-S02-RC
                   GO   TO              MAIN-EXT
               END-IF
           ELSE
               MOVE    "DBCLOSECURSOR"  TO      MCP-FUNC
               MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
               CALL    "ORCMCPSUB"       USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
               MOVE    1                TO      FLG-HKNCOMBI
               DISPLAY "HKNCOMBI DBSELECT ERR"
               MOVE     10              TO      LNK-S02-RC
               GO   TO                  MAIN-EXT
           END-IF.
           MOVE    "DBCLOSECURSOR"      TO      MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"       TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¤Î¾å¸Â¶èÊ¬ÊÔ½¸
           PERFORM   105-JYOGENKBN-HENSYU-SEC.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¤«¤é¼çÊÝ¸±¤ÎÊÝ¸±¼ïÎà¤òÈ½Äê
      *    ¡Ê¶õÇò¡§°åÊÝ¡¢£±¡§Ï«ºÒ¡¦¼«Çå¡Ë
           MOVE     SPACE               TO       WK-HKNKBN.
           EVALUATE    COMB-SYU-HKNKOHSBTKBN
               WHEN    "4"
                       MOVE    "1"      TO       WK-HKNKBN
           END-EVALUATE.
      *
      *    ´µ¼ÔÉéÃ´³Û¤Î»»Äê
           PERFORM       200-HKNFTN-SANTEI-SEC.
      *
      *    »»½ÐÀÁµá³Û¤Î£±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ¡ÊÌôºÞ°ìÉôÉéÃ´¶â¤â´Þ¤à¡Ë
      *    ¢¨Ï«ºÒÊÝ¸±¤Î¾ì¹ç¤Ï»Í¼Î¸ÞÆþ¤ò¹Ô¤ï¤Ê¤¤
      *    ¢¨Ï·¿Í£²³ä¤Ç¸ÂÅÙ¤¬£·£²£³£°£°±ß¡Ü¡÷±ß¤Î¾ì¹ç¤Ï»Í¼Î¸ÞÆþ¤ò¹Ô¤ï¤Ê¤¤
           IF     (LNK-SYU-SKYMONEY      NOT =   ZERO)   
               IF   WK-HKNKBN                =   "1"
               OR   FLG-KOUGAKUCYO           =    1 
               OR   FLG-KOUGAKUCYO2          =    1 
               OR   FLG-KUMAMOTO             =   "1"
                   CONTINUE
               ELSE
                   COMPUTE  LNK-SYU-SKYMONEY = LNK-SYU-SKYMONEY  +  5
                   COMPUTE  WK-SEIKYUGK      = LNK-SYU-SKYMONEY  /  10
                   COMPUTE  LNK-SYU-SKYMONEY = WK-SEIKYUGK       *  10
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
                       MOVE   WK-KNJFTNKIN-ENTANI
                                          TO   LNK-SYU-KOH-FTN-ENTANI
                  END-IF
               ELSE
      *            ¡Ê¸øÈñ¡Ë
                       MOVE   LNK-SYU-SKYMONEY   TO   LNK-SYU-KOH-FTN
                       MOVE   WK-KNJFTNKIN-ENTANI
                                          TO   LNK-SYU-KOH-FTN-ENTANI
               END-IF
           END-IF.
      *
      *    ¼«Èñ¶â³Û¤Î¾ÃÈñÀÇ·×»»
           COMPUTE   LNK-SYU-JIHI-TOTAL =  (LNK-SYU-JIHI-TAXNASI(1)  +
                                            LNK-SYU-JIHI-TAXNASI(2)  +
                                            LNK-SYU-JIHI-TAXNASI(3)  +
                                            LNK-SYU-JIHI-TAXNASI(4)  +
                                            LNK-SYU-JIHI-TAXNASI(5)).
           COMPUTE   LNK-SYU-JIHI-TOTAL-TAX = (LNK-SYU-JIHI-TAXARI(1) +
                                               LNK-SYU-JIHI-TAXARI(2) +
                                               LNK-SYU-JIHI-TAXARI(3) +
                                               LNK-SYU-JIHI-TAXARI(4) +
                                               LNK-SYU-JIHI-TAXARI(5)).
      *
           IF  ( COMB-SYU-HKNKOHSBTKBN  =  "8" ) AND
               ( COMB-HOJOKBN           =  "1" )
            CONTINUE
           ELSE
            IF    WK-TAX1001-KBN  =  "1"
              COMPUTE LNK-SYU-JIHI-TAX = (LNK-SYU-JIHI-TOTAL-TAX * 0.05)
                                       +  0.5
            ELSE
              COMPUTE LNK-SYU-JIHI-TAX =  LNK-SYU-JIHI-TOTAL-TAX * 0.05
            END-IF
           END-IF.
      *
      *    ÊÝ¸±Å¬ÍÑ³°¶â³Û¤Î²Ã»»
           MOVE       ZERO              TO  LNK-SYU-TGMONEY(12).
           MOVE       ZERO              TO  LNK-SYU-TGMONEY-TAX(12).
           PERFORM    VARYING   HKNNYU-IDX   FROM   1   BY   1
                      UNTIL     HKNNYU-IDX   >     11
                      ADD       LNK-SYU-TGMONEY(HKNNYU-IDX)
                                            TO  LNK-SYU-TGMONEY(12)
                      ADD       LNK-SYU-TGMONEY-TAX(HKNNYU-IDX)
                                        TO  LNK-SYU-TGMONEY-TAX(12)
           END-PERFORM.
      *
           IF  ( COMB-SYU-HKNKOHSBTKBN  =  "8" ) AND
               ( COMB-HOJOKBN           =  "1" )
             CONTINUE
           ELSE
             IF    WK-TAX1001-KBN  =  "1"
               COMPUTE   LNK-SYU-TGMONEY-TAX-SAI   = 
                       ( LNK-SYU-TGMONEY-TAX(12) * 0.05 )  +  0.5
               COMPUTE   LNK-SYU-TGMONEY-TAX(12)   = 
                       ( LNK-SYU-TGMONEY-TAX(12) * 1.05 )  +  0.5
             ELSE
               COMPUTE   LNK-SYU-TGMONEY-TAX-SAI   = 
                         LNK-SYU-TGMONEY-TAX(12) * 0.05
               COMPUTE   LNK-SYU-TGMONEY-TAX(12)   = 
                         LNK-SYU-TGMONEY-TAX(12) * 1.05          
             END-IF
           END-IF.
      *
      *    ¼¼ÎÁº¹³Û¾ÃÈñÀÇ·×»»
      *
           IF  ( COMB-SYU-HKNKOHSBTKBN  =  "8" ) AND
               ( COMB-HOJOKBN           =  "1" )
             CONTINUE
           ELSE
             IF   WK-BRMSGK-TAXKBN    =  "1"
                IF    WK-TAX1001-KBN  =  "1"
                  COMPUTE   LNK-SYU-RMSAGAKU-TAX-SAI   = 
                          ( LNK-SYU-RMSAGAKU * 0.05 )  +  0.5
                  COMPUTE   LNK-SYU-RMSAGAKU   = 
                          ( LNK-SYU-RMSAGAKU * 1.05 )  +  0.5
                ELSE
                  COMPUTE   LNK-SYU-RMSAGAKU-TAX-SAI   = 
                            LNK-SYU-RMSAGAKU * 0.05
                  COMPUTE   LNK-SYU-RMSAGAKU   = 
                            LNK-SYU-RMSAGAKU * 1.05
                END-IF
             END-IF
           END-IF.
      *
      *    ÀÁµá¶â³Û¤Î·×»»¡ÊÄ´À°¶â´Þ¤Þ¤º¡Ë
           COMPUTE    LNK-SYU-SKYMONEY  =   
                                 LNK-SYU-SKYMONEY              +
                                 LNK-SYU-RMSAGAKU              +
                                 LNK-SYU-SKYMONEY-SKJ-KEI      +
                                 LNK-SYU-SKYMONEY-SKJ-JIHI-KEI +
                                 LNK-SYU-TGMONEY(12)           +
                                 LNK-SYU-TGMONEY-TAX(12)       +
                                 LNK-SYU-JIHI-TOTAL            +
                                 LNK-SYU-JIHI-TOTAL-TAX        +
                                 LNK-SYU-JIHI-TAX.
      *
           IF   WK-HKNKBN  =   "1"
                  COMPUTE    LNK-SYU-RSIJIBAI-SKYMONEY     =
                             LNK-SYU-RSIJIBAI-SKYMONEY     +
                             LNK-SYU-RMSAGAKU              +
      *****                  LNK-SYU-SKYMONEY-SKJ-KEI      +
                             LNK-SYU-SKYMONEY-SKJ-JIHI-KEI +
                             LNK-SYU-TGMONEY(12)           +
                             LNK-SYU-TGMONEY-TAX(12)       +
                             LNK-SYU-JIHI-TOTAL            +
                             LNK-SYU-JIHI-TOTAL-TAX        +
                             LNK-SYU-JIHI-TAX
           END-IF.
      *
      *    ¼«ÈñÊÝ¸±¤ÎÊä½õ¶èÊ¬¤¬²ÝÀÇ¤Î¾ì¹ç¡¢¾ÃÈñÀÇ·×»»¤ò¹Ô¤¦
           IF  ( COMB-SYU-HKNKOHSBTKBN  =  "8" ) AND
               ( COMB-HOJOKBN           =  "1" )
      *
              IF    WK-TAX1001-KBN  =  "1"
                 COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI  = 
                           ( LNK-SYU-SKYMONEY  *  0.05 ) + 0.5
                 COMPUTE  LNK-SYU-SKYMONEY   =  
                           ( LNK-SYU-SKYMONEY  *  1.05 ) + 0.5
              ELSE
                 COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI  = 
                             LNK-SYU-SKYMONEY  *  0.05
                 COMPUTE  LNK-SYU-SKYMONEY   =  
                             LNK-SYU-SKYMONEY  *  1.05
              END-IF
      *
           ELSE
      *
              COMPUTE  LNK-SYU-SKYMONEY-TAX-SAI       =
                       LNK-SYU-JIHI-TAX               +
                       LNK-SYU-TGMONEY-TAX-SAI        +
                       LNK-SYU-RMSAGAKU-TAX-SAI       +
                       LNK-SYU-SKYMONEY-SKJ-TAX       +
                       LNK-SYU-SKYMONEY-SKJ-JIHI-TAX
      *
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
                 COMPUTE    LNK-SYU-SKYMONEY  =   
                            LNK-SYU-SKYMONEY              +
                            LNK-SYU-RMSAGAKU              +
                            LNK-SYU-SKYMONEY-SKJ-JIHI-KEI +
                            LNK-SYU-TGMONEY(12)           +
                            LNK-SYU-TGMONEY-TAX(12)       +
                            LNK-SYU-JIHI-TOTAL            +
                            LNK-SYU-JIHI-TOTAL-TAX        +
                            LNK-SYU-JIHI-TAX
      *
      **         COMPUTE    LNK-SYU-RSI-TOTAL =   
      **                                    LNK-SYU-RSISHOSHIN-MONEY +  
      **                                    LNK-SYU-RSISAISHIN-MONEY +  
      **                                    LNK-SYU-RSISDO-MONEY     +  
      **                                    LNK-SYU-RSIETC-MONEY        
           END-IF.
      *    ¡ãÈóÏ«ºÒ»ØÄê¡Ê£±£°£°¡ó¡Ë¡ä
           IF   (WK-HKNKBN               =    "1"  ) AND
                (LNK-SYU-SYUHKNNUM       =    "971") AND
                (LNK-SYU-PTFTNRATE       =     100 )
                 COMPUTE    LNK-SYU-SKYMONEY  =
                            LNK-SYU-SKYMONEY              +
                            LNK-SYU-RSISHOSHIN-MONEY      +
                            LNK-SYU-RSISAISHIN-MONEY      +
                            LNK-SYU-RSISDO-MONEY          +
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
      *    ¼«ÇåÀÕÊÝ¸±¤Î¾ì¹ç¡¢Ï«ºÒ±ß·ú¤Æ¹àÌÜ¤Ø¤Î²Ã»»Î¨·×»»ÄÉ²Ã
      *    ¡ã´µ¼ÔÀÁµá¤¹¤ë¡Ê£±£°£°¡ó¡Ë¤Î¾ì¹ç¡ä
           IF   (WK-HKNKBN               =    "1")   AND
                (LNK-SYU-SYUHKNNUM       =    "973") AND
                (LNK-SYU-PTFTNRATE       =     100 )
                 COMPUTE  LNK-SYU-RSISHOSHIN-MONEY  =
                         (LNK-SYU-RSISHOSHIN-MONEY  *  WK-KSNRATE)
                                                    +  0.5
                 COMPUTE  LNK-SYU-RSISAISHIN-MONEY  =
                         (LNK-SYU-RSISAISHIN-MONEY  *  WK-KSNRATE)
                                                    +  0.5
      *          COMPUTE  LNK-SYU-RSISDO-MONEY      =
      *                   LNK-SYU-RSISDO-MONEY      *  WK-KSNRATE
      *
      *          ¼«ÇåÀÕ¤½¤ÎÂ¾¥³¡¼¥É¾ðÊó¤ÎÊÔ½¸¡Ê£°£¹£µ£¹£±¡¥£¹£²¡¥£¹£³¡Ë
                 PERFORM   500-JIBAI-SONOTA-HENSYU-SEC
      *
      ***        IF   WK-4001-TAXKBN   =   "2"
      ***          IF    WK-TAX1001-KBN  =  "1"
      ***            COMPUTE  LNK-SYU-RSI-TAX-SAI       =               
      ***                     LNK-SYU-RSI-TAX-SAI       +               
      ***                   ((LNK-S02-SINDAN  *  0.05)  +  0.5)         
      ***            COMPUTE  LNK-SYU-RSI-TAX-SAI       =               
      ***                     LNK-SYU-RSI-TAX-SAI       +               
      ***                   ((LNK-S02-MEISAI  *  0.05)  +  0.5)         
      *
      ***            COMPUTE  LNK-SYU-RSIETC-MONEY      =
      ***                     LNK-SYU-RSIETC-MONEY      +
      ***                   ((LNK-S02-SINDAN  *  0.05)  +  0.5)
      ***            COMPUTE  LNK-SYU-RSIETC-MONEY      =
      ***                     LNK-SYU-RSIETC-MONEY      +
      ***                   ((LNK-S02-MEISAI  *  0.05)  +  0.5)
      ***          ELSE
      ***            COMPUTE  LNK-SYU-RSI-TAX-SAI       =               
      ***                     LNK-SYU-RSI-TAX-SAI       +               
      ***                    (LNK-S02-SINDAN  *  0.05)                  
      ***            COMPUTE  LNK-SYU-RSI-TAX-SAI       =               
      ***                     LNK-SYU-RSI-TAX-SAI       +               
      ***                    (LNK-S02-MEISAI  *  0.05)                  
      *
      ***            COMPUTE  LNK-SYU-RSIETC-MONEY      =
      ***                     LNK-SYU-RSIETC-MONEY      +
      ***                    (LNK-S02-SINDAN  *  0.05)
      ***            COMPUTE  LNK-SYU-RSIETC-MONEY      =
      ***                     LNK-SYU-RSIETC-MONEY      +
      ***                     (LNK-S02-MEISAI  *  0.05)
      ***          END-IF
      ***        END-IF
      *
                 COMPUTE    LNK-SYU-SKYMONEY  =   
                            LNK-SYU-SKYMONEY         +
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
                 COMPUTE    LNK-SYU-SKYMONEY-TAX-SAI  =
                            LNK-SYU-SKYMONEY-TAX-SAI  +
                            LNK-SYU-RSI-TAX-SAI        
           END-IF.
      *    ¡ã´µ¼ÔÀÁµá¤·¤Ê¤¤¡Ê£°¡ó¡Ë¤Î¾ì¹ç¡ä
           IF   (WK-HKNKBN               =    "1")   AND
                (LNK-SYU-SYUHKNNUM       =    "973") AND
                (LNK-SYU-PTFTNRATE       =    ZERO )
                 COMPUTE  LNK-SYU-RSISHOSHIN-MONEY  =
                         (LNK-SYU-RSISHOSHIN-MONEY  *  WK-KSNRATE)
                                                    +  0.5
                 COMPUTE  LNK-SYU-RSISAISHIN-MONEY  =
                         (LNK-SYU-RSISAISHIN-MONEY  *  WK-KSNRATE)
                                                    +  0.5
      *          COMPUTE  LNK-SYU-RSISDO-MONEY      =
      *                   LNK-SYU-RSISDO-MONEY      *  WK-KSNRATE
      *
      *          ¼«ÇåÀÕ¤½¤ÎÂ¾¥³¡¼¥É¾ðÊó¤ÎÊÔ½¸¡Ê£°£¹£µ£¹£±¡¥£¹£²¡¥£¹£³¡Ë
                 PERFORM   500-JIBAI-SONOTA-HENSYU-SEC
      *
      ***        IF   WK-4001-TAXKBN   =   "2"
      ***          IF    WK-TAX1001-KBN  =  "1"
      ***            COMPUTE  LNK-SYU-RSI-TAX-SAI       =               
      ***                     LNK-SYU-RSI-TAX-SAI       +               
      ***                   ((LNK-S02-SINDAN  *  0.05)  +  0.5)         
      ***            COMPUTE  LNK-SYU-RSI-TAX-SAI       =               
      ***                     LNK-SYU-RSI-TAX-SAI       +               
      ***                   ((LNK-S02-MEISAI  *  0.05)  +  0.5)         
      *
      ***            COMPUTE  LNK-SYU-RSIETC-MONEY      =
      ***                     LNK-SYU-RSIETC-MONEY      +
      ***                   ((LNK-S02-SINDAN  *  0.05)  +  0.5)
      ***            COMPUTE  LNK-SYU-RSIETC-MONEY      =
      ***                     LNK-SYU-RSIETC-MONEY      +
      ***                   ((LNK-S02-MEISAI  *  0.05)  +  0.5)
      ***          ELSE
      ***            COMPUTE  LNK-SYU-RSI-TAX-SAI       =               
      ***                     LNK-SYU-RSI-TAX-SAI       +               
      ***                    (LNK-S02-SINDAN  *  0.05)                  
      ***            COMPUTE  LNK-SYU-RSI-TAX-SAI       =               
      ***                     LNK-SYU-RSI-TAX-SAI       +               
      ***                    (LNK-S02-MEISAI  *  0.05)                  
      *
      ***            COMPUTE  LNK-SYU-RSIETC-MONEY      =
      ***                     LNK-SYU-RSIETC-MONEY      +
      ***                    (LNK-S02-SINDAN  *  0.05)
      ***            COMPUTE  LNK-SYU-RSIETC-MONEY      =
      ***                     LNK-SYU-RSIETC-MONEY      +
      ***                    (LNK-S02-MEISAI  *  0.05)
      ***          END-IF
      ***        END-IF
      *
                 MOVE    ZERO      TO       LNK-SYU-SKYMONEY
                 COMPUTE    LNK-SYU-SKYMONEY  =
                            LNK-SYU-SKYMONEY              +
                            LNK-SYU-RMSAGAKU              +
                            LNK-SYU-SKYMONEY-SKJ-JIHI-KEI +
                            LNK-SYU-TGMONEY(12)           +
                            LNK-SYU-TGMONEY-TAX(12)       +
                            LNK-SYU-JIHI-TOTAL            +
                            LNK-SYU-JIHI-TOTAL-TAX        +
                            LNK-SYU-JIHI-TAX
      *
      **         COMPUTE    LNK-SYU-RSI-TOTAL =   
      **                                    LNK-SYU-RSISHOSHIN-MONEY +  
      **                                    LNK-SYU-RSISAISHIN-MONEY +  
      **                                    LNK-SYU-RSISDO-MONEY     +  
      **                                    LNK-SYU-RSIETC-MONEY        
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
                       IF  SYS-1037-PGID-NYU NOT = SPACE
                          CALL   SYS-1037-PGID-NYU  USING   MCPAREA
                                                    LNK-SYUNOU-REC
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
      *    ÀÁµá¶â³Û¤Î·×»»¡ÊÄ´À°¶â¤Î²Ã¸º»»¡Ë
           COMPUTE    LNK-SYU-SKYMONEY  =   LNK-SYU-SKYMONEY    +
                                            LNK-SYU-CHOSEI.
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
                                    MOVE    ZERO   TO   LNK-SYU-SKYMONEY
                               ELSE
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
                      UNTIL   (IDX4                       >    50 )
                       OR     (LNK-S02-SONOTA-SRYCD(IDX4) =  SPACE)
                 EVALUATE      LNK-S02-SONOTA-SRYCD(IDX4)(1:5)
                    WHEN   "09591"
                       ADD     1     TO   IDX5
                       MOVE    LNK-S02-SONOTA-SRYCD(IDX4)
                                     TO   WK-SONOTA91-SRYCD(IDX5)
                       MOVE    LNK-S02-SONOTA-ZAIKAISU(IDX4)
                                     TO   WK-SONOTA91-ZAIKAISU(IDX5)
                       MOVE    LNK-S02-SONOTA-TANKA(IDX4)
                                     TO   WK-SONOTA91-TANKA(IDX5)
                    WHEN   "09592"
                       ADD     1     TO   IDX6
                       MOVE    LNK-S02-SONOTA-SRYCD(IDX4)
                                     TO   WK-SONOTA92-SRYCD(IDX6)
                       MOVE    LNK-S02-SONOTA-ZAIKAISU(IDX4)
                                     TO   WK-SONOTA92-ZAIKAISU(IDX6)
                       MOVE    LNK-S02-SONOTA-TANKA(IDX4)
                                     TO   WK-SONOTA92-TANKA(IDX6)
                    WHEN   "09593"
                       MOVE     SPACE    TO    FLG-TAIGAI
                       PERFORM  VARYING  IDX8    FROM   1   BY   1
                            UNTIL  (IDX8                       >   10  )
                              OR   (WK-4003-TAIGAI-SRYCD(IDX8) =  SPACE)
                              OR   (FLG-TAIGAI                 =  "1"  )
                         IF   WK-4003-TAIGAI-SRYCD(IDX8)  =
                                         LNK-S02-SONOTA-SRYCD(IDX4)
                              MOVE    "1"     TO    FLG-TAIGAI
                         END-IF
                       END-PERFORM
                       IF   FLG-TAIGAI  =  SPACE
                          ADD     1     TO   IDX7
                          MOVE    LNK-S02-SONOTA-SRYCD(IDX4)
                                        TO   WK-SONOTA93-SRYCD(IDX7)
                          MOVE    LNK-S02-SONOTA-ZAIKAISU(IDX4)
                                        TO   WK-SONOTA93-ZAIKAISU(IDX7)
                          MOVE    LNK-S02-SONOTA-TANKA(IDX4)
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
                            UNTIL   (IDX4                    >    50 )
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
      *
                 END-PERFORM
      *
                 PERFORM   VARYING   IDX4    FROM   1   BY   1
                            UNTIL   (IDX4                    >    50 )
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
      *
                 END-PERFORM
      *
              ELSE
      *
                 PERFORM   VARYING   IDX4    FROM   1   BY   1
                            UNTIL   (IDX4                    >    50 )
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
      *
                 END-PERFORM
      *
                 PERFORM   VARYING   IDX4    FROM   1   BY   1
                            UNTIL   (IDX4                    >    50 )
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
                            UNTIL   (IDX4                    >    50 )
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
      *
                 END-PERFORM
      *
              ELSE
      *
                 PERFORM   VARYING   IDX4    FROM   1   BY   1
                            UNTIL   (IDX4                    >    50 )
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
           MOVE    SPACE                TO      FLG-SANSHOU2.
           MOVE    SPACE                TO      FLG-SANSHOU3.     
           MOVE    SPACE                TO      WRK-SKJGNSTYMD-S
           MOVE    SPACE                TO      WRK-SKJGNSTYMD-E
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
                          (FLG-TNKRRK     =    1)    OR
                         ((TNKRRK-STYMD     <=   LNK-SYU-SRYYMD  )  AND
                          (LNK-SYU-SRYYMD   <=   TNKRRK-EDYMD))
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
                       (FLG-TSYRRK      =    1)    OR
                      ((TSYRRK-NINSTYMD   <=   LNK-SYU-SRYYMD  )  AND
                       (LNK-SYU-SRYYMD    <=   TSYRRK-NINEDYMD))
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
      *
                       MOVE     TSYRRK-SKJGNSTYMD
                                        TO       WRK-SKJGNSTYMD-S
                       MOVE     TSYRRK-NINEDYMD
                                        TO       WRK-SKJGNSTYMD-E
      *
               WHEN    0                ALSO     0                 
               WHEN    1                ALSO     0
                       MOVE     "1"     TO       FLG-SANSHOU3      
           END-EVALUATE.                                    
      *
       100-KAKUTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¤Î¾å¸Â¶èÊ¬ÊÔ½¸                                *
      ******************************************************************
       105-JYOGENKBN-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE    TO   COMB-KOH1-NYUJGNKBN.
           MOVE    SPACE    TO   COMB-KOH2-NYUJGNKBN.
           MOVE    SPACE    TO   COMB-KOH3-NYUJGNKBN.
           MOVE    SPACE    TO   COMB-KOH4-NYUJGNKBN.
      *
      *    ¸øÈñ£±¤Î¾å¸Â¶èÊ¬ÊÔ½¸
           IF  COMB-KOH1HKNNUM           NOT =  SPACE
      *
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-KOH1HKNNUM    TO    HKN-HKNNUM
               IF     COMB-KOH1HKNNUM    =     "027"
                       MOVE     COMB-KOH1PAYKBN
                                         TO    WK-PAYKBN
               ELSE
                       MOVE     "00"     TO    WK-PAYKBN
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
                  ((HKN-TEKSTYMD(1:6)     <=  LNK-SYU-SRYYMD(1:6)  ) AND
                   (LNK-SYU-SRYYMD(1:6)   <=  HKN-TEKEDYMD(1:6))     AND
                   (WK-PAYKBN            =  HKN-PAYKBN))
                     PERFORM   HKNNUM-READ-SEC
                 END-PERFORM
                 IF     FLG-HKNNUM         =       0
                        PERFORM   1051-JYOGENKBN-CHECK-SEC
                        MOVE   WK-NYUJGNKBN  TO  COMB-KOH1-NYUJGNKBN
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
                  ((HKN-TEKSTYMD(1:6)     <= LNK-SYU-SRYYMD(1:6)  ) AND
                   (LNK-SYU-SRYYMD(1:6)   <= HKN-TEKEDYMD(1:6))     AND
                   (WK-PAYKBN            = HKN-PAYKBN))
                     PERFORM   HKNNUM-READ-SEC
                 END-PERFORM
                 IF     FLG-HKNNUM         =       0
                        PERFORM   1051-JYOGENKBN-CHECK-SEC
                        MOVE   WK-NYUJGNKBN  TO  COMB-KOH2-NYUJGNKBN
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
                  ((HKN-TEKSTYMD(1:6)     <= LNK-SYU-SRYYMD(1:6)  ) AND
                   (LNK-SYU-SRYYMD(1:6)   <= HKN-TEKEDYMD(1:6))     AND
                   (WK-PAYKBN            = HKN-PAYKBN))
                     PERFORM   HKNNUM-READ-SEC
                 END-PERFORM
                 IF     FLG-HKNNUM         =       0
                        PERFORM   1051-JYOGENKBN-CHECK-SEC
                        MOVE   WK-NYUJGNKBN  TO  COMB-KOH3-NYUJGNKBN
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
                  ((HKN-TEKSTYMD(1:6)     <= LNK-SYU-SRYYMD(1:6)  ) AND
                   (LNK-SYU-SRYYMD(1:6)   <= HKN-TEKEDYMD(1:6))     AND
                   (WK-PAYKBN            = HKN-PAYKBN))
                     PERFORM   HKNNUM-READ-SEC
                 END-PERFORM
                 IF     FLG-HKNNUM         =       0
                        PERFORM   1051-JYOGENKBN-CHECK-SEC
                        MOVE   WK-NYUJGNKBN  TO  COMB-KOH4-NYUJGNKBN
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
           MOVE     SPACE     TO    WK-NYUJGNKBN.
      *
           IF  ( WK-1001-PREFNUM         =  01  OR  27  OR 34  ) AND
               ( HKN-HKNNUM              = "141"               )
              EVALUATE   FLG-SANSHOU2
                  WHEN   "1"
                      IF   HKN-HON-NYUMJGNGAK  NOT = ZERO
                      OR   HKN-HON-NYUMJGNCNT  NOT = ZERO
                      OR   HKN-HON-NYUDJGNGAK  NOT = ZERO
                             MOVE     "1"      TO     WK-NYUJGNKBN
                      END-IF
                  WHEN   "2"
                      IF   HKN-TSY-NYUMJGNGAK  NOT = ZERO
                      OR   HKN-TSY-NYUMJGNCNT  NOT = ZERO
                      OR   HKN-TSY-NYUDJGNGAK  NOT = ZERO
                             MOVE     "1"      TO     WK-NYUJGNKBN
                      END-IF
                  WHEN   "3"
                      IF   HKN-TNK-NYUMJGNGAK  NOT = ZERO
                      OR   HKN-TNK-NYUMJGNCNT  NOT = ZERO
                      OR   HKN-TNK-NYUDJGNGAK  NOT = ZERO
                             MOVE     "1"      TO     WK-NYUJGNKBN
                      END-IF
              END-EVALUATE
           ELSE
      ***     EVALUATE   FLG-SANSHOU2
      ***         WHEN   "1"
                      IF   HKN-HON-NYUMJGNGAK  NOT = ZERO
                      OR   HKN-HON-NYUMJGNCNT  NOT = ZERO
                      OR   HKN-HON-NYUDJGNGAK  NOT = ZERO
                             MOVE     "1"      TO     WK-NYUJGNKBN
                      END-IF
      ***         WHEN   "2"
      ***             IF   HKN-TSY-NYUMJGNGAK  NOT = ZERO
      ***             OR   HKN-TSY-NYUMJGNCNT  NOT = ZERO
      ***             OR   HKN-TSY-NYUDJGNGAK  NOT = ZERO
      ***                    MOVE     "1"      TO     WK-NYUJGNKBN
      ***             END-IF
      ***         WHEN   "3"
      ***             IF   HKN-TNK-NYUMJGNGAK  NOT = ZERO
      ***             OR   HKN-TNK-NYUMJGNCNT  NOT = ZERO
      ***             OR   HKN-TNK-NYUDJGNGAK  NOT = ZERO
      ***                    MOVE     "1"      TO     WK-NYUJGNKBN
      ***             END-IF
      ***     END-EVALUATE
           END-IF.
      *
           IF     HKN-HKNNUM   =     "027"
                      IF   HKN-HON-NYUMJGNGAK  NOT = ZERO
                      OR   HKN-HON-NYUMJGNCNT  NOT = ZERO
                      OR   HKN-HON-NYUDJGNGAK  NOT = ZERO
                             MOVE     "1"      TO     WK-NYUJGNKBN
                      ELSE
                             MOVE    SPACE     TO     WK-NYUJGNKBN
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
           MOVE    SPACE                 TO    HOKEN-MAX-AREA
                                               HOKEN-KAISU-TABLE
                                               HOKEN-FUTAN-AREA
                                               HOKEN-FUTAN-AREA2
                                               HOKEN-FUTAN-AREA3
                                               HOKEN-ZAN-AREA
                                               HOKEN-ZAN-AREA2
                                               HOKEN-ZAN-AREA3
                                               HOKEN-ZAN-D-AREA
                                               HOKEN-ZAN-D2-AREA
                                               HOKEN-ZAN-D3-AREA.
           INITIALIZE                          HOKEN-MAX-AREA
                                               HOKEN-KAISU-TABLE
                                               HOKEN-FUTAN-AREA
                                               HOKEN-FUTAN-AREA2
                                               HOKEN-FUTAN-AREA3
                                               HOKEN-ZAN-AREA
                                               HOKEN-ZAN-AREA2
                                               HOKEN-ZAN-AREA3
                                               HOKEN-ZAN-D-AREA
                                               HOKEN-ZAN-D2-AREA
                                               HOKEN-ZAN-D3-AREA.
           MOVE    ALL"9"                TO    WK-KOH-COMPFTN-AREA
                                               WK-DAY-KOH-COMPFTN-AREA.
      *
      *    ¼«ÇåÀÕ¡¦ÎÅÍÜÃ´Åö¼êÅöÍÑÆÃÎã½èÍý
      *    ¡Ê¼«ÇåÀÕ¤Î¾ì¹ç¡¢£±£°ÅÀ¤ò£±£²£°±ß¤ËÆÉ¤ßÂØ¤¨¤ë¡Ë
      *    ¡ÊÎÅÍÜÃ´Åö¼êÅö¡§£±·î£±²ó¤Î»»Äê¡Ë
           IF    LNK-SYU-SYUHKNNUM       =    "973"
           AND   LNK-S02-RYOYO       NOT =    ZERO
               COMPUTE  LNK-SYU-HKNTEN (10)  =  LNK-SYU-HKNTEN (10)
                                             -  LNK-S02-RYOYO
               COMPUTE  LNK-SYU-HKNTEN (12)  =  LNK-SYU-HKNTEN (12)
                                             -  LNK-S02-RYOYO
      *
               COMPUTE  WK-RYOYO  =  LNK-S02-RYOYO  /   10
      *
               COMPUTE  LNK-SYU-RSIETC-MONEY =  LNK-SYU-RSIETC-MONEY
                                             +  ( 120 *  WK-RYOYO )
           END-IF.
      *
      *    ´µ¼ÔÉéÃ´¶â»»½ÐÁ°¤Î£±£°£°¡óÉéÃ´³Û»»½Ð
           MOVE      ZERO         TO      WK-ZENGAKU.
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   11
               COMPUTE WK-ZENGAKU  =   WK-ZENGAKU  +
                                      (LNK-SYU-HKNTEN (IDX) * 10)
           END-PERFORM.
      *
      *
           INITIALIZE    WK-SYUHKN-TAIHI-AREA.
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
           END-IF.
      *
           IF  LNK-S02-KOUREI-NYUIN-KBN     =   SPACE   
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
                   DISPLAY   "PTINF SELECT ERR"
      *
                   INITIALIZE                   PTINF-REC
               END-IF
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "PTINF-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               INITIALIZE                    ORCSAGECHKAREA
               MOVE    LNK-SYU-HOSPID    TO  AGECHK-HOSPID
               MOVE    LNK-SYU-PTID      TO  AGECHK-PTID
               MOVE    LNK-SYU-SRYYMD    TO  AGECHK-SRYYMD
               MOVE    PTINF-BIRTHDAY    TO  AGECHK-BIRTHDAY
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
               IF      AGECHK-RC       =   ZERO
                   IF  ( AGECHK-NENREI   >=  70      ) OR
                       ( AGECHK-NENREI   >=  65  AND
                         COMB-KOH1HKNNUM  = "027"    ) OR
                       ( AGECHK-NENREI   >=  65  AND
                         FLG-ROUJINHOKEN  = "1"      )
                       MOVE  "1"     TO  LNK-S02-KOUREI-NYUIN-KBN
                   END-IF
               ELSE
                   DISPLAY  "PTINF AGE-CHK ERR " LNK-SYU-PTID
      *
                   MOVE  SPACE       TO  LNK-S02-KOUREI-NYUIN-KBN
               END-IF
           END-IF.       
           IF   LNK-S02-KOUREI-NYUIN-KBN   =  "1"
               IF  COMB-SYU-HKNKOHSBTKBN   =   "8"
                   MOVE  SPACE       TO  LNK-S02-KOUREI-NYUIN-KBN
               END-IF
               IF  COMB-HKNNUM  =  "971"  OR  "973"
                   MOVE  SPACE       TO  LNK-S02-KOUREI-NYUIN-KBN
               END-IF
           END-IF.                                               
      *
      *    Ï«ºÒ¡¦¼«ÇåÀÕ¤ÎÉéÃ´³ä¹ç¤òµá¤á¤ë
      *
      *    IF   WK-HKNKBN              =   "1"
      *
               INITIALIZE                           PTINF-REC.
               MOVE    LNK-SYU-HOSPID       TO      PTINF-HOSPID.
               MOVE    LNK-SYU-PTID         TO      PTINF-PTID.
               MOVE    PTINF-REC            TO      MCPDATA-REC.
               MOVE    "DBSELECT"           TO      MCP-FUNC.
               MOVE    "PTINF-KEY"          TO      ORC-DBPATH.
               CALL    "ORCMCPSUB"          USING   MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC.
               IF      MCP-RC               =       ZERO
                   MOVE    "PTINF-KEY"      TO      ORC-DBPATH
                   PERFORM  PTINF-READ-SEC
               ELSE
                   DISPLAY   "PTINF SELECT ERR"
      *
                   INITIALIZE                   PTINF-REC
               END-IF.
      *
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
               MOVE    "PTINF-KEY"          TO  ORC-DBPATH.
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC.
      *
               INITIALIZE                  ORCSAGECHKAREA.
               MOVE    LNK-SYU-HOSPID  TO  AGECHK-HOSPID.
               MOVE    LNK-SYU-PTID    TO  AGECHK-PTID.
               MOVE    LNK-SYU-SRYYMD  TO  AGECHK-SRYYMD.
               MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY.
               MOVE    LNK-SYU-HKNCOMBINUM 
                                       TO  AGECHK-HKNCOMBINUM.
               MOVE    LNK-SYU-NYUGAIKBN
                                       TO  AGECHK-NYUGAIKBN
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA.
               IF      AGECHK-RC       =   ZERO
                   MOVE   AGECHK-FTNRATE  TO  LNK-SYU-PTFTNRATE
               ELSE
                   DISPLAY  "PTINF AGE-CHK ERR " LNK-SYU-PTID
               END-IF.
      *
      *    END-IF.
      *
      *    ÊÝ¸±ÈÖ¹æ¥Æ¡¼¥Ö¥ë¤«¤é¸øÈñ¤ÎÉéÃ´¾å¸Â¶â³Û¤Î¼èÆÀ
           PERFORM         210-HOKEN-KAISU-SEC.
      *
      *    ¿ÇÎÅ·îÆâ¤Î¼ýÇ¼¥Þ¥¹¥¿¤«¤é¸øÈñËè¤Î´µ¼ÔÉéÃ´¶â³Û¤Î
      *    Îß·×¤òÄ´ºº
           PERFORM         220-SYUNOU-SEC.
      *
      *    ÊÝ¸±¡¦¸øÈñ¤ÎÉéÃ´¶â·×»»
           PERFORM         230-FTNKIN-SEC.
      *
       200-HKNFTN-SANTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÊÝ¸±¡¦¸øÈñ¤ÎÀßÄê¾å¸Â¶â³Û¤Î¼èÆÀ¡¡¡¡¡¡¡¡¡¡                *
      ******************************************************************
       210-HOKEN-KAISU-SEC           SECTION.
      *
           INITIALIZE         WK-CHIHOU-FLG-TAIHI-AREA.
      *
           INITIALIZE         WK-SYUHEN-TAIHI-AREA.
      *
      *    *¸øÈñ£±¤Î¾å¸Â¶â³Û¼è¹þ¤ß
           IF (COMB-KOH1HKNNUM           NOT =  SPACE)    AND
              (COMB-KOH1-NYUJGNKBN       =     "1")
               INITIALIZE                      HKNNUM-REC
               MOVE   COMB-HOSPID        TO    HKN-HOSPID
               MOVE   COMB-KOH1HKNNUM    TO    HKN-HKNNUM
               IF     COMB-KOH1HKNNUM    =     "027"
                       MOVE     COMB-KOH1PAYKBN
                                         TO    WK-PAYKBN
               ELSE
                       MOVE     "00"     TO    WK-PAYKBN
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
                  ((HKN-TEKSTYMD(1:6)     <=  LNK-SYU-SRYYMD(1:6)  ) AND
                   (LNK-SYU-SRYYMD(1:6)   <=  HKN-TEKEDYMD(1:6))     AND
                   (WK-PAYKBN            =  HKN-PAYKBN))
                     PERFORM   HKNNUM-READ-SEC
                 END-PERFORM
                 IF     FLG-HKNNUM         =       0
                        PERFORM   2103-KOH1-KAISU-SEC
                 END-IF
               END-IF
           ELSE
      *
               IF  (LNK-S02-KOUREI-NYUIN-KBN  =   "1" )
               AND (COMB-KOH1HKNNUM           =  "010")
                    MOVE   40200       TO   MAX-KIN-KOHI1-M
                    MOVE    "1"        TO   COMB-KOH1-NYUJGNKBN
               END-IF
      *
               IF  ( LNK-S02-KOUREI-NYUIN-KBN     =       "1"  )
               AND ( COMB-KOH1HKNNUM(1:1)        >=       "1"  )
               AND ( COMB-KOH1HKNNUM(1:1)        <=       "8"  )
                 IF   WK-1001-PREFNUM       NOT   =     20 AND 22
                    MOVE   40200   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-NYUJGNKBN
                    MOVE    "1"    TO    FLG-CHIHOU(1)
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  11   ) AND
                   ( COMB-KOH1HKNNUM(2:2)    = "41"  ) AND
                   ((COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972"))
                    MOVE   10000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-NYUJGNKBN
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  22             ) AND
                   ( COMB-KOH1HKNNUM         = "184" OR "185"  ) AND
                   ((COMB-KOH2HKNNUM         = "972"          ) OR
                    (COMB-KOH3HKNNUM         = "972"          ) OR
                    (COMB-KOH4HKNNUM         = "972"          ))
                    MOVE   10000   TO    MAX-KIN-KOHI1-M
                    MOVE    "1"    TO    COMB-KOH1-NYUJGNKBN
               END-IF
      *
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
      *    *¸øÈñ£²¤Î¾å¸Â¶â³Û¼è¹þ¤ß
           IF (COMB-KOH2HKNNUM           NOT = SPACE)    AND
              (COMB-KOH2-NYUJGNKBN       =     "1")
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
                  ((HKN-TEKSTYMD(1:6)     <= LNK-SYU-SRYYMD(1:6)  ) AND
                   (LNK-SYU-SRYYMD(1:6)   <= HKN-TEKEDYMD(1:6))     AND
                   (WK-PAYKBN            = HKN-PAYKBN))
                     PERFORM   HKNNUM-READ-SEC
                 END-PERFORM
                 IF     FLG-HKNNUM         =       0
                        PERFORM   2104-KOH2-KAISU-SEC
                 END-IF
               END-IF
           ELSE
      *
               IF  (LNK-S02-KOUREI-NYUIN-KBN  =   "1" )
               AND (COMB-KOH2HKNNUM           =  "010")
                    MOVE   40200       TO   MAX-KIN-KOHI2-M
                    MOVE    "1"        TO   COMB-KOH2-NYUJGNKBN
               END-IF
      *
               IF  ( LNK-S02-KOUREI-NYUIN-KBN     =       "1"  )
               AND ( COMB-KOH2HKNNUM(1:1)        >=       "1"  )
               AND ( COMB-KOH2HKNNUM(1:1)        <=       "8"  )
                 IF   WK-1001-PREFNUM       NOT   =     20 AND 22
                    MOVE   40200   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-NYUJGNKBN
                    MOVE    "1"    TO    FLG-CHIHOU(2)
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  11   ) AND
                   ( COMB-KOH2HKNNUM(2:2)    = "41"  ) AND
                   ((COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972"))
                    MOVE   10000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-NYUJGNKBN
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  22             ) AND
                   ( COMB-KOH2HKNNUM         = "184" OR "185"  ) AND
                   ((COMB-KOH1HKNNUM         = "972"          ) OR
                    (COMB-KOH3HKNNUM         = "972"          ) OR
                    (COMB-KOH4HKNNUM         = "972"          ))
                    MOVE   10000   TO    MAX-KIN-KOHI2-M
                    MOVE    "1"    TO    COMB-KOH2-NYUJGNKBN
               END-IF
      *
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
      *    *¸øÈñ£³¤Î¾å¸Â¶â³Û¼è¹þ¤ß
           IF (COMB-KOH3HKNNUM           NOT = SPACE)    AND
              (COMB-KOH3-NYUJGNKBN       =     "1")
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
                  ((HKN-TEKSTYMD(1:6)     <= LNK-SYU-SRYYMD(1:6)  ) AND
                   (LNK-SYU-SRYYMD(1:6)   <= HKN-TEKEDYMD(1:6))     AND
                   (WK-PAYKBN            = HKN-PAYKBN))
                     PERFORM   HKNNUM-READ-SEC
                 END-PERFORM
                 IF     FLG-HKNNUM         =       0
                        PERFORM   2105-KOH3-KAISU-SEC
                 END-IF
               END-IF
           ELSE
      *
               IF  (LNK-S02-KOUREI-NYUIN-KBN  =   "1" )
               AND (COMB-KOH3HKNNUM           =  "010")
                    MOVE   40200       TO   MAX-KIN-KOHI3-M
                    MOVE    "1"        TO   COMB-KOH3-NYUJGNKBN
               END-IF
      *
               IF  ( LNK-S02-KOUREI-NYUIN-KBN     =       "1"  )
               AND ( COMB-KOH3HKNNUM(1:1)        >=       "1"  )
               AND ( COMB-KOH3HKNNUM(1:1)        <=       "8"  )
                 IF   WK-1001-PREFNUM       NOT   =     20 AND 22
                    MOVE   40200   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-NYUJGNKBN
                    MOVE    "1"    TO    FLG-CHIHOU(3)
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  11   ) AND
                   ( COMB-KOH3HKNNUM(2:2)    = "41"  ) AND
                   ((COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH4HKNNUM         = "972"))
                    MOVE   10000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-NYUJGNKBN
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  22             ) AND
                   ( COMB-KOH3HKNNUM         = "184" OR "185"  ) AND
                   ((COMB-KOH1HKNNUM         = "972"          ) OR
                    (COMB-KOH2HKNNUM         = "972"          ) OR
                    (COMB-KOH4HKNNUM         = "972"          ))
                    MOVE   10000   TO    MAX-KIN-KOHI3-M
                    MOVE    "1"    TO    COMB-KOH3-NYUJGNKBN
               END-IF
      *
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
      *    *¸øÈñ£´¤Î¾å¸Â¶â³Û¼è¹þ¤ß
           IF (COMB-KOH4HKNNUM           NOT = SPACE)    AND
              (COMB-KOH4-NYUJGNKBN       =     "1")
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
                  ((HKN-TEKSTYMD(1:6)     <= LNK-SYU-SRYYMD(1:6)  ) AND
                   (LNK-SYU-SRYYMD(1:6)   <= HKN-TEKEDYMD(1:6))     AND
                   (WK-PAYKBN            = HKN-PAYKBN))
                     PERFORM   HKNNUM-READ-SEC
                 END-PERFORM
                 IF     FLG-HKNNUM         =       0
                        PERFORM   2106-KOH4-KAISU-SEC
                 END-IF
               END-IF
           ELSE
      *
               IF  (LNK-S02-KOUREI-NYUIN-KBN  =   "1" )
               AND (COMB-KOH4HKNNUM           =  "010")
                    MOVE   40200       TO   MAX-KIN-KOHI4-M
                    MOVE    "1"        TO   COMB-KOH4-NYUJGNKBN
               END-IF
      *
               IF  ( LNK-S02-KOUREI-NYUIN-KBN     =       "1"  )
               AND ( COMB-KOH4HKNNUM(1:1)        >=       "1"  )
               AND ( COMB-KOH4HKNNUM(1:1)        <=       "8"  )
                 IF   WK-1001-PREFNUM       NOT   =     20 AND 22
                    MOVE   40200   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-NYUJGNKBN
                    MOVE    "1"    TO    FLG-CHIHOU(4)
                 END-IF
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  11   ) AND
                   ( COMB-KOH4HKNNUM(2:2)    = "41"  ) AND
                   ((COMB-KOH1HKNNUM         = "972") OR
                    (COMB-KOH2HKNNUM         = "972") OR
                    (COMB-KOH3HKNNUM         = "972"))
                    MOVE   10000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-NYUJGNKBN
               END-IF
      *
               IF  ( WK-1001-PREFNUM         =  22             ) AND
                   ( COMB-KOH4HKNNUM         = "184" OR "185"  ) AND
                   ((COMB-KOH1HKNNUM         = "972"          ) OR
                    (COMB-KOH2HKNNUM         = "972"          ) OR
                    (COMB-KOH3HKNNUM         = "972"          ))
                    MOVE   10000   TO    MAX-KIN-KOHI4-M
                    MOVE    "1"    TO    COMB-KOH4-NYUJGNKBN
               END-IF
      *
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "HKNNUM-KEY2"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
      *    ´µ¼Ô¸øÈñÉéÃ´¡Ê¶â³Û¡Ë¼è¤ê¹þ¤ß
           IF    LNK-SYU-SRYYMD(1:6)   <   "200304"
               CONTINUE
           ELSE
               PERFORM   2107-KOHFTN-KINGAKU-SEC
           END-IF.
      *
      *
           IF  LNK-S02-KOUREI-NYUIN-KBN   =   "1"
               IF  COMB-KOH1-HKNKOHSBTKBN   =   "3"
                   EVALUATE   COMB-KOH1PAYKBN
                       WHEN   "01"
                           MOVE  "1"          TO   COMB-KOH1-NYUJGNKBN
                           IF  FLG-SANSHOU3     =   SPACE               
                               MOVE   40200   TO   MAX-KIN-KOHI1-M
                           ELSE
                             IF  FLG-SANSHOU3     =   "1"
                               MOVE   15000   TO   MAX-KIN-KOHI1-M
      *                        MOVE   1       TO   FLG-SKJ
                             ELSE
                               MOVE   24600   TO   MAX-KIN-KOHI1-M
      *                        MOVE   2       TO   FLG-SKJ
                             END-IF
                           END-IF
                       WHEN   "02"
                           MOVE  "1"          TO   COMB-KOH1-NYUJGNKBN
                           MOVE   72300       TO   MAX-KIN-KOHI1-M
                           IF  (COMB-KOH2HKNNUM    =  SPACE)              
                           AND (COMB-KOH3HKNNUM    =  SPACE)
                           AND (COMB-KOH4HKNNUM    =  SPACE)
                             MOVE   WK-ZENGAKU  TO   WK-KOUREI-IRYOHI
                             MOVE   1           TO   FLG-IRYOHI
                           END-IF
                   END-EVALUATE
               ELSE
                   EVALUATE   COMB-HOJOKBN
                       WHEN   "1"
                       WHEN   "9"
                           IF  FLG-SANSHOU3      =   SPACE              
                               MOVE   40200   TO   MAX-KIN-SYU-M
                           ELSE
                             IF  FLG-SANSHOU3     =   "1"
                               MOVE   15000   TO   MAX-KIN-SYU-M
      *                        MOVE   1       TO   FLG-SKJ
                             ELSE
                               MOVE   24600   TO   MAX-KIN-SYU-M
      *                        MOVE   2       TO   FLG-SKJ
                             END-IF
                           END-IF
                       WHEN   "2"
                       WHEN   "8"
                           MOVE   72300       TO   MAX-KIN-SYU-M
                           IF  (COMB-KOH1HKNNUM    =  SPACE)              
                           AND (COMB-KOH2HKNNUM    =  SPACE)
                           AND (COMB-KOH3HKNNUM    =  SPACE)
                           AND (COMB-KOH4HKNNUM    =  SPACE)
                             MOVE   WK-ZENGAKU  TO   WK-KOUREI-IRYOHI
                             MOVE   2           TO   FLG-IRYOHI
                           END-IF
                   END-EVALUATE
               END-IF
           END-IF.
      *
           IF  ( LNK-S02-KOUREI-NYUIN-KBN     =      "1"   )
           AND ( COMB-HKNNUM            NOT   =     SPACE  )
      *
                IF   COMB-KOH1HKNNUM      =    "027"
                   IF  ( COMB-KOH2HKNNUM(1:1)  >=     "1" )
                   AND ( COMB-KOH2HKNNUM(1:1)  <=     "8" )
                   AND ( COMB-KOH3HKNNUM        =    SPACE)
                   AND ( COMB-KOH4HKNNUM        =    SPACE)
                          MOVE    "1"    TO   FLG-SYUHEN
                   ELSE
                    IF   ( COMB-KOH2HKNNUM   =   "012" )
                    OR   ( COMB-KOH3HKNNUM   =   "012" )
                    OR   ( COMB-KOH4HKNNUM   =   "012" )
                        MOVE    "1"    TO   FLG-SYUHEN
                        IF   MAX-KIN-KOHI1-M   >  15000
                            MOVE   15000   TO   MAX-KIN-KOHI1-M
                        END-IF
                    ELSE
                     IF   ( COMB-KOH2HKNNUM  =   "972" )
                     OR   ( COMB-KOH3HKNNUM  =   "972" )
                     OR   ( COMB-KOH4HKNNUM  =   "972" )
                        MOVE    "1"    TO   FLG-SYUHEN
                        IF   MAX-KIN-KOHI1-M   >  10000
                            MOVE   10000   TO   MAX-KIN-KOHI1-M
                        END-IF
                     ELSE
                      IF  (COMB-KOH2HKNNUM  NOT = SPACE)
                      OR  (COMB-KOH3HKNNUM  NOT = SPACE)
                      OR  (COMB-KOH4HKNNUM  NOT = SPACE)
                        MOVE    "1"    TO   FLG-SYUHEN
                        IF   MAX-KIN-KOHI1-M   >  40200
                            MOVE   40200   TO   MAX-KIN-KOHI1-M
                        END-IF
                      END-IF
                     END-IF
                    END-IF
                   END-IF
                ELSE
                   IF  ( COMB-KOH1HKNNUM(1:1)  >=     "1" )
                   AND ( COMB-KOH1HKNNUM(1:1)  <=     "8" )
                   AND ( COMB-KOH2HKNNUM        =    SPACE)
                   AND ( COMB-KOH3HKNNUM        =    SPACE)
                   AND ( COMB-KOH4HKNNUM        =    SPACE)
                          MOVE    "1"    TO   FLG-SYUHEN
                   ELSE
                    IF   ( COMB-KOH1HKNNUM   =   "012" )
                    OR   ( COMB-KOH2HKNNUM   =   "012" )
                    OR   ( COMB-KOH3HKNNUM   =   "012" )
                    OR   ( COMB-KOH4HKNNUM   =   "012" )
                        MOVE    "1"    TO   FLG-SYUHEN
                        IF   MAX-KIN-SYU-M   >  15000
                            MOVE   15000   TO   MAX-KIN-SYU-M
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
                      IF  (COMB-KOH1HKNNUM  NOT = SPACE)
                      OR  (COMB-KOH2HKNNUM  NOT = SPACE)
                      OR  (COMB-KOH3HKNNUM  NOT = SPACE)
                      OR  (COMB-KOH4HKNNUM  NOT = SPACE)
                        MOVE    "1"    TO   FLG-SYUHEN
                        IF   MAX-KIN-SYU-M   >  40200
                            MOVE   40200   TO   MAX-KIN-SYU-M
                        END-IF
                      END-IF
                     END-IF
                    END-IF
                   END-IF
                END-IF
      *
           END-IF.
      *
      *    £´£±Ï·¿Í¾å¸Â³ÛÊÔ½¸½èÍý
           PERFORM    2108-ROUJIN41-JYOGEN-HENSYU-SEC.
      *
           IF  ( FLG-SANSHOU3        =   SPACE          )
           OR  ( LNK-SYU-SYUHKNNUM   =   "971" OR "973" )
                 CONTINUE
           ELSE
                 IF  FLG-SANSHOU3     =   "1"
                      MOVE   1       TO   FLG-SKJ
                 ELSE
                      MOVE   2       TO   FLG-SKJ
                 END-IF
           END-IF.
      *
       210-HOKEN-KAISU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñ£±¡Ê¶â³Û¡Ë¼è¹þ¤ß½èÍý                                *
      ******************************************************************
       2103-KOH1-KAISU-SEC           SECTION.
      *
           IF  ( WK-1001-PREFNUM         =  01  OR  27  OR 34  ) AND
               ( COMB-KOH1HKNNUM         = "141"               )
              EVALUATE   FLG-SANSHOU2
                  WHEN   "1"
                      MOVE  HKN-HON-NYUMJGNCNT TO  MAX-KAI-KOHI1-M
                      MOVE  HKN-HON-NYUDJGNGAK TO  MAX-KIN-KOHI1-D
                      MOVE  HKN-HON-NYUMJGNGAK TO  MAX-KIN-KOHI1-M
                  WHEN   "2"
                      MOVE  HKN-TSY-NYUMJGNCNT TO  MAX-KAI-KOHI1-M
                      MOVE  HKN-TSY-NYUDJGNGAK TO  MAX-KIN-KOHI1-D
                      MOVE  HKN-TSY-NYUMJGNGAK TO  MAX-KIN-KOHI1-M
                  WHEN   "3"
                      MOVE  HKN-TNK-NYUMJGNCNT TO  MAX-KAI-KOHI1-M
                      MOVE  HKN-TNK-NYUDJGNGAK TO  MAX-KIN-KOHI1-D
                      MOVE  HKN-TNK-NYUMJGNGAK TO  MAX-KIN-KOHI1-M
              END-EVALUATE
           ELSE
      ***     EVALUATE   FLG-SANSHOU2
      ***         WHEN   "1"
                      MOVE  HKN-HON-NYUMJGNCNT TO  MAX-KAI-KOHI1-M
                      MOVE  HKN-HON-NYUDJGNGAK TO  MAX-KIN-KOHI1-D
                      MOVE  HKN-HON-NYUMJGNGAK TO  MAX-KIN-KOHI1-M
      ***         WHEN   "2"
      ***             MOVE  HKN-TSY-NYUMJGNCNT TO  MAX-KAI-KOHI1-M
      ***             MOVE  HKN-TSY-NYUDJGNGAK TO  MAX-KIN-KOHI1-D
      ***             MOVE  HKN-TSY-NYUMJGNGAK TO  MAX-KIN-KOHI1-M
      ***         WHEN   "3"
      ***             MOVE  HKN-TNK-NYUMJGNCNT TO  MAX-KAI-KOHI1-M
      ***             MOVE  HKN-TNK-NYUDJGNGAK TO  MAX-KIN-KOHI1-D
      ***             MOVE  HKN-TNK-NYUMJGNGAK TO  MAX-KIN-KOHI1-M
      ***     END-EVALUATE
           END-IF.
           IF    COMB-KOH1HKNNUM   =   "027"
                      MOVE  HKN-HON-NYUMJGNCNT TO  MAX-KAI-KOHI1-M
                      MOVE  HKN-HON-NYUDJGNGAK TO  MAX-KIN-KOHI1-D
                      MOVE  HKN-HON-NYUMJGNGAK TO  MAX-KIN-KOHI1-M
           END-IF.
      *
           IF  ( LNK-S02-KOUREI-NYUIN-KBN     =       "1"  )
           AND ( COMB-KOH1HKNNUM(1:1)        >=       "1"  )
           AND ( COMB-KOH1HKNNUM(1:1)        <=       "8"  )
              IF  (WK-1001-PREFNUM       NOT   =     20 AND 22) 
              AND (MAX-KIN-KOHI1-M             >  40200       )
                     MOVE    40200   TO    MAX-KIN-KOHI1-M
              END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  11   ) AND
               ( COMB-KOH1HKNNUM(2:2)    = "41"  ) AND
               ((COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972"))
               IF   MAX-KIN-KOHI1-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI1-M
               END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  22             ) AND
               ( COMB-KOH1HKNNUM         = "184" OR "185"  ) AND
               ((COMB-KOH2HKNNUM         = "972"          ) OR
                (COMB-KOH3HKNNUM         = "972"          ) OR
                (COMB-KOH4HKNNUM         = "972"          ))
               IF   MAX-KIN-KOHI1-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI1-M
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
           IF  ( WK-1001-PREFNUM         =  01  OR  27  OR 34  ) AND
               ( COMB-KOH2HKNNUM         = "141"               )
              EVALUATE   FLG-SANSHOU2
                  WHEN   "1"
                      MOVE  HKN-HON-NYUMJGNCNT TO  MAX-KAI-KOHI2-M
                      MOVE  HKN-HON-NYUDJGNGAK TO  MAX-KIN-KOHI2-D
                      MOVE  HKN-HON-NYUMJGNGAK TO  MAX-KIN-KOHI2-M
                  WHEN   "2"
                      MOVE  HKN-TSY-NYUMJGNCNT TO  MAX-KAI-KOHI2-M
                      MOVE  HKN-TSY-NYUDJGNGAK TO  MAX-KIN-KOHI2-D
                      MOVE  HKN-TSY-NYUMJGNGAK TO  MAX-KIN-KOHI2-M
                  WHEN   "3"
                      MOVE  HKN-TNK-NYUMJGNCNT TO  MAX-KAI-KOHI2-M
                      MOVE  HKN-TNK-NYUDJGNGAK TO  MAX-KIN-KOHI2-D
                      MOVE  HKN-TNK-NYUMJGNGAK TO  MAX-KIN-KOHI2-M
              END-EVALUATE
           ELSE
      ***     EVALUATE   FLG-SANSHOU2
      ***         WHEN   "1"
                      MOVE  HKN-HON-NYUMJGNCNT TO  MAX-KAI-KOHI2-M
                      MOVE  HKN-HON-NYUDJGNGAK TO  MAX-KIN-KOHI2-D
                      MOVE  HKN-HON-NYUMJGNGAK TO  MAX-KIN-KOHI2-M
      ***         WHEN   "2"
      ***             MOVE  HKN-TSY-NYUMJGNCNT TO  MAX-KAI-KOHI2-M
      ***             MOVE  HKN-TSY-NYUDJGNGAK TO  MAX-KIN-KOHI2-D
      ***             MOVE  HKN-TSY-NYUMJGNGAK TO  MAX-KIN-KOHI2-M
      ***         WHEN   "3"
      ***             MOVE  HKN-TNK-NYUMJGNCNT TO  MAX-KAI-KOHI2-M
      ***             MOVE  HKN-TNK-NYUDJGNGAK TO  MAX-KIN-KOHI2-D
      ***             MOVE  HKN-TNK-NYUMJGNGAK TO  MAX-KIN-KOHI2-M
      ***     END-EVALUATE
           END-IF.
      *
           IF  ( LNK-S02-KOUREI-NYUIN-KBN     =       "1"  )
           AND ( COMB-KOH2HKNNUM(1:1)        >=       "1"  )
           AND ( COMB-KOH2HKNNUM(1:1)        <=       "8"  )
              IF  (WK-1001-PREFNUM       NOT   =     20 AND 22) 
              AND (MAX-KIN-KOHI2-M             >  40200       )
                     MOVE    40200   TO    MAX-KIN-KOHI2-M
              END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  11   ) AND
               ( COMB-KOH2HKNNUM(2:2)    = "41"  ) AND
               ((COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972"))
               IF   MAX-KIN-KOHI2-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI2-M
               END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  22             ) AND
               ( COMB-KOH2HKNNUM         = "184" OR "185"  ) AND
               ((COMB-KOH1HKNNUM         = "972"          ) OR
                (COMB-KOH3HKNNUM         = "972"          ) OR
                (COMB-KOH4HKNNUM         = "972"          ))
               IF   MAX-KIN-KOHI2-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI2-M
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
           IF  ( WK-1001-PREFNUM         =  01  OR  27  OR 34  ) AND
               ( COMB-KOH3HKNNUM         = "141"               )
              EVALUATE   FLG-SANSHOU2
                  WHEN   "1"
                      MOVE  HKN-HON-NYUMJGNCNT TO  MAX-KAI-KOHI3-M
                      MOVE  HKN-HON-NYUDJGNGAK TO  MAX-KIN-KOHI3-D
                      MOVE  HKN-HON-NYUMJGNGAK TO  MAX-KIN-KOHI3-M
                  WHEN   "2"
                      MOVE  HKN-TSY-NYUMJGNCNT TO  MAX-KAI-KOHI3-M
                      MOVE  HKN-TSY-NYUDJGNGAK TO  MAX-KIN-KOHI3-D
                      MOVE  HKN-TSY-NYUMJGNGAK TO  MAX-KIN-KOHI3-M
                  WHEN   "3"
                      MOVE  HKN-TNK-NYUMJGNCNT TO  MAX-KAI-KOHI3-M
                      MOVE  HKN-TNK-NYUDJGNGAK TO  MAX-KIN-KOHI3-D
                      MOVE  HKN-TNK-NYUMJGNGAK TO  MAX-KIN-KOHI3-M
              END-EVALUATE
           ELSE
      ***     EVALUATE   FLG-SANSHOU2
      ***         WHEN   "1"
                      MOVE  HKN-HON-NYUMJGNCNT TO  MAX-KAI-KOHI3-M
                      MOVE  HKN-HON-NYUDJGNGAK TO  MAX-KIN-KOHI3-D
                      MOVE  HKN-HON-NYUMJGNGAK TO  MAX-KIN-KOHI3-M
      ***         WHEN   "2"
      ***             MOVE  HKN-TSY-NYUMJGNCNT TO  MAX-KAI-KOHI3-M
      ***             MOVE  HKN-TSY-NYUDJGNGAK TO  MAX-KIN-KOHI3-D
      ***             MOVE  HKN-TSY-NYUMJGNGAK TO  MAX-KIN-KOHI3-M
      ***         WHEN   "3"
      ***             MOVE  HKN-TNK-NYUMJGNCNT TO  MAX-KAI-KOHI3-M
      ***             MOVE  HKN-TNK-NYUDJGNGAK TO  MAX-KIN-KOHI3-D
      ***             MOVE  HKN-TNK-NYUMJGNGAK TO  MAX-KIN-KOHI3-M
      ***     END-EVALUATE
           END-IF.
      *
           IF  ( LNK-S02-KOUREI-NYUIN-KBN     =       "1"  )
           AND ( COMB-KOH3HKNNUM(1:1)        >=       "1"  )
           AND ( COMB-KOH3HKNNUM(1:1)        <=       "8"  )
              IF  (WK-1001-PREFNUM       NOT   =     20 AND 22) 
              AND (MAX-KIN-KOHI3-M             >  40200       )
                     MOVE    40200   TO    MAX-KIN-KOHI3-M
              END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  11   ) AND
               ( COMB-KOH3HKNNUM(2:2)    = "41"  ) AND
               ((COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH4HKNNUM         = "972"))
               IF   MAX-KIN-KOHI3-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI3-M
               END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  22             ) AND
               ( COMB-KOH3HKNNUM         = "184" OR "185"  ) AND
               ((COMB-KOH1HKNNUM         = "972"          ) OR
                (COMB-KOH2HKNNUM         = "972"          ) OR
                (COMB-KOH4HKNNUM         = "972"          ))
               IF   MAX-KIN-KOHI3-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI3-M
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
           IF  ( WK-1001-PREFNUM         =  01  OR  27  OR 34  ) AND
               ( COMB-KOH4HKNNUM         = "141"               )
              EVALUATE   FLG-SANSHOU2
                  WHEN   "1"
                      MOVE  HKN-HON-NYUMJGNCNT TO  MAX-KAI-KOHI4-M
                      MOVE  HKN-HON-NYUDJGNGAK TO  MAX-KIN-KOHI4-D
                      MOVE  HKN-HON-NYUMJGNGAK TO  MAX-KIN-KOHI4-M
                  WHEN   "2"
                      MOVE  HKN-TSY-NYUMJGNCNT TO  MAX-KAI-KOHI4-M
                      MOVE  HKN-TSY-NYUDJGNGAK TO  MAX-KIN-KOHI4-D
                      MOVE  HKN-TSY-NYUMJGNGAK TO  MAX-KIN-KOHI4-M
                  WHEN   "3"
                      MOVE  HKN-TNK-NYUMJGNCNT TO  MAX-KAI-KOHI4-M
                      MOVE  HKN-TNK-NYUDJGNGAK TO  MAX-KIN-KOHI4-D
                      MOVE  HKN-TNK-NYUMJGNGAK TO  MAX-KIN-KOHI4-M
              END-EVALUATE
           ELSE
      ***     EVALUATE   FLG-SANSHOU2
      ***         WHEN   "1"
                      MOVE  HKN-HON-NYUMJGNCNT TO  MAX-KAI-KOHI4-M
                      MOVE  HKN-HON-NYUDJGNGAK TO  MAX-KIN-KOHI4-D
                      MOVE  HKN-HON-NYUMJGNGAK TO  MAX-KIN-KOHI4-M
      ***         WHEN   "2"
      ***             MOVE  HKN-TSY-NYUMJGNCNT TO  MAX-KAI-KOHI4-M
      ***             MOVE  HKN-TSY-NYUDJGNGAK TO  MAX-KIN-KOHI4-D
      ***             MOVE  HKN-TSY-NYUMJGNGAK TO  MAX-KIN-KOHI4-M
      ***         WHEN   "3"
      ***             MOVE  HKN-TNK-NYUMJGNCNT TO  MAX-KAI-KOHI4-M
      ***             MOVE  HKN-TNK-NYUDJGNGAK TO  MAX-KIN-KOHI4-D
      ***             MOVE  HKN-TNK-NYUMJGNGAK TO  MAX-KIN-KOHI4-M
      ***     END-EVALUATE
           END-IF.
      *
           IF  ( LNK-S02-KOUREI-NYUIN-KBN     =       "1"  )
           AND ( COMB-KOH4HKNNUM(1:1)        >=       "1"  )
           AND ( COMB-KOH4HKNNUM(1:1)        <=       "8"  )
              IF  (WK-1001-PREFNUM       NOT   =     20 AND 22) 
              AND (MAX-KIN-KOHI4-M             >  40200       )
                     MOVE    40200   TO    MAX-KIN-KOHI4-M
              END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  11   ) AND
               ( COMB-KOH4HKNNUM(2:2)    = "41"  ) AND
               ((COMB-KOH1HKNNUM         = "972") OR
                (COMB-KOH2HKNNUM         = "972") OR
                (COMB-KOH3HKNNUM         = "972"))
               IF   MAX-KIN-KOHI4-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI4-M
               END-IF
           END-IF.
      *
           IF  ( WK-1001-PREFNUM         =  22             ) AND
               ( COMB-KOH4HKNNUM         = "184" OR "185"  ) AND
               ((COMB-KOH1HKNNUM         = "972"          ) OR
                (COMB-KOH2HKNNUM         = "972"          ) OR
                (COMB-KOH3HKNNUM         = "972"          ))
               IF   MAX-KIN-KOHI4-M   >     10000
                     MOVE    10000   TO    MAX-KIN-KOHI4-M
               END-IF
           END-IF.
      *
       2106-KOH4-KAISU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ´µ¼Ô¸øÈñÉéÃ´¡Ê¶â³Û¡Ë¼è¹þ¤ß½èÍý                          *
      ******************************************************************
       2107-KOHFTN-KINGAKU-SEC           SECTION.
      *
           INITIALIZE                   WK-TAIHI-NYUJGNGAK-OCC.
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
      ***    OR ( COMB-KOHHKNNUM(KOH-IDX)(1:1) = "1" OR "2" OR "3" OR 
      ***                                        "4" OR "5" OR "6" OR
      ***                                        "7" OR "8"          )
      *
                MOVE       ZERO           TO  WK-NYUJGNGAK
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
                MOVE  LNK-SYU-SKYEDYMD    TO  LNK-ORCSJGNGET-SKYEDYMD
      *
                CALL    "ORCSJGNGET"       USING
                                           LNK-ORCSJGNGET-REC
      *
                IF    LNK-ORCSJGNGET-RC   =   ZERO
                     MOVE    LNK-ORCSJGNGET-NYUJGNGAK
                                          TO  WK-NYUJGNGAK
                     EVALUATE   KOH-IDX
                      WHEN  1
                        MOVE   WK-NYUJGNGAK    TO   MAX-KIN-KOHI1-M
                        MOVE   WK-NYUJGNGAK    TO
                                            WK-TAIHI-NYUJGNGAK(KOH-IDX)
                        MOVE     "1"   TO     COMB-KOH1-NYUJGNKBN
                        IF   LNK-S02-KOUREI-NYUIN-KBN  =    "1" 
                          IF  ( WK-1001-PREFNUM        =     20 OR 22)
                          AND ( COMB-KOH1HKNNUM(1:1)   >=   "1"      )
                          AND ( COMB-KOH1HKNNUM(1:1)   <=   "8"      )
                            CONTINUE
                          ELSE
                            IF  COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF   (COMB-HKNNUM  NOT   =   SPACE)
                              AND  (MAX-KIN-KOHI1-M    >   15000)
                                MOVE    15000   TO   MAX-KIN-KOHI1-M
                                MOVE    15000   TO
                                             WK-TAIHI-NYUJGNGAK(KOH-IDX)
                              END-IF
                            ELSE
                              IF    MAX-KIN-KOHI1-M    >  40200
                                MOVE    40200   TO   MAX-KIN-KOHI1-M
                                MOVE    40200   TO
                                             WK-TAIHI-NYUJGNGAK(KOH-IDX)
                              END-IF
                            END-IF
                            IF    FLG-CHIHOU(1)      =  "1"
                               MOVE   SPACE    TO   FLG-CHIHOU(1)
                            END-IF
                          END-IF
                        END-IF
                      WHEN  2
                        MOVE   WK-NYUJGNGAK    TO   MAX-KIN-KOHI2-M
                        MOVE   WK-NYUJGNGAK    TO
                                            WK-TAIHI-NYUJGNGAK(KOH-IDX)
                        MOVE     "1"   TO     COMB-KOH2-NYUJGNKBN
                        IF   LNK-S02-KOUREI-NYUIN-KBN  =    "1" 
                          IF  ( WK-1001-PREFNUM        =     20 OR 22)
                          AND ( COMB-KOH2HKNNUM(1:1)   >=   "1"      )
                          AND ( COMB-KOH2HKNNUM(1:1)   <=   "8"      )
                            CONTINUE
                          ELSE
                            IF  COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF   (COMB-HKNNUM  NOT   =   SPACE)
                              AND  (MAX-KIN-KOHI2-M    >   15000)
                                MOVE    15000   TO   MAX-KIN-KOHI2-M
                                MOVE    15000   TO
                                             WK-TAIHI-NYUJGNGAK(KOH-IDX)
                              END-IF
                            ELSE
                              IF    MAX-KIN-KOHI2-M    >  40200
                                MOVE    40200   TO   MAX-KIN-KOHI2-M
                                MOVE    40200   TO
                                             WK-TAIHI-NYUJGNGAK(KOH-IDX)
                              END-IF
                            END-IF
                            IF    FLG-CHIHOU(2)      =  "1"
                               MOVE   SPACE    TO   FLG-CHIHOU(2)
                            END-IF
                          END-IF
                        END-IF
                      WHEN  3
                        MOVE   WK-NYUJGNGAK    TO   MAX-KIN-KOHI3-M
                        MOVE   WK-NYUJGNGAK    TO
                                            WK-TAIHI-NYUJGNGAK(KOH-IDX)
                        MOVE     "1"   TO     COMB-KOH3-NYUJGNKBN
                        IF   LNK-S02-KOUREI-NYUIN-KBN  =    "1" 
                          IF  ( WK-1001-PREFNUM        =     20 OR 22)
                          AND ( COMB-KOH3HKNNUM(1:1)   >=   "1"      )
                          AND ( COMB-KOH3HKNNUM(1:1)   <=   "8"      )
                            CONTINUE
                          ELSE
                            IF  COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF   (COMB-HKNNUM  NOT   =   SPACE)
                              AND  (MAX-KIN-KOHI3-M    >   15000)
                                MOVE    15000   TO   MAX-KIN-KOHI3-M
                                MOVE    15000   TO
                                             WK-TAIHI-NYUJGNGAK(KOH-IDX)
                              END-IF
                            ELSE
                              IF    MAX-KIN-KOHI3-M    >  40200
                                MOVE    40200   TO   MAX-KIN-KOHI3-M
                                MOVE    40200   TO
                                             WK-TAIHI-NYUJGNGAK(KOH-IDX)
                              END-IF
                            END-IF
                            IF    FLG-CHIHOU(3)      =  "1"
                               MOVE   SPACE    TO   FLG-CHIHOU(3)
                            END-IF
                          END-IF
                        END-IF
                      WHEN  4
                        MOVE   WK-NYUJGNGAK    TO   MAX-KIN-KOHI4-M
                        MOVE   WK-NYUJGNGAK    TO
                                            WK-TAIHI-NYUJGNGAK(KOH-IDX)
                        MOVE     "1"   TO     COMB-KOH4-NYUJGNKBN
                        IF   LNK-S02-KOUREI-NYUIN-KBN  =    "1" 
                          IF  ( WK-1001-PREFNUM        =     20 OR 22)
                          AND ( COMB-KOH4HKNNUM(1:1)   >=   "1"      )
                          AND ( COMB-KOH4HKNNUM(1:1)   <=   "8"      )
                            CONTINUE
                          ELSE
                            IF  COMB-KOHHKNNUM(KOH-IDX) = "012"
                              IF   (COMB-HKNNUM  NOT   =   SPACE)
                              AND  (MAX-KIN-KOHI4-M    >   15000)
                                MOVE    15000   TO   MAX-KIN-KOHI4-M
                                MOVE    15000   TO
                                             WK-TAIHI-NYUJGNGAK(KOH-IDX)
                              END-IF
                            ELSE
                              IF    MAX-KIN-KOHI4-M    >  40200
                                MOVE    40200   TO   MAX-KIN-KOHI4-M
                                MOVE    40200   TO
                                             WK-TAIHI-NYUJGNGAK(KOH-IDX)
                              END-IF
                            END-IF
                            IF    FLG-CHIHOU(4)      =  "1"
                               MOVE   SPACE    TO   FLG-CHIHOU(4)
                            END-IF
                          END-IF
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
           IF   WK-1001-PREFNUM            =   01  OR  27  OR  34
      *
              IF   COMB-KOH1HKNNUM         =  "241"
                      MOVE  "1"          TO   COMB-KOH1-NYUJGNKBN
                      MOVE   72300       TO   MAX-KIN-KOHI1-M
                      MOVE   WK-ZENGAKU  TO   WK-KOUREI-IRYOHI
                      MOVE   1           TO   FLG-IRYOHI2
              END-IF
      *
              IF   COMB-KOH2HKNNUM         =  "241"
                      MOVE  "1"          TO   COMB-KOH2-NYUJGNKBN
                      MOVE   72300       TO   MAX-KIN-KOHI2-M
                      MOVE   WK-ZENGAKU  TO   WK-KOUREI-IRYOHI
                      MOVE   2           TO   FLG-IRYOHI2
              END-IF
      *
              IF   COMB-KOH3HKNNUM         =  "241"
                      MOVE  "1"          TO   COMB-KOH3-NYUJGNKBN
                      MOVE   72300       TO   MAX-KIN-KOHI3-M
                      MOVE   WK-ZENGAKU  TO   WK-KOUREI-IRYOHI
                      MOVE   3           TO   FLG-IRYOHI2
              END-IF
      *
              IF   COMB-KOH4HKNNUM         =  "241"
                      MOVE  "1"          TO   COMB-KOH4-NYUJGNKBN
                      MOVE   72300       TO   MAX-KIN-KOHI4-M
                      MOVE   WK-ZENGAKU  TO   WK-KOUREI-IRYOHI
                      MOVE   4           TO   FLG-IRYOHI2
              END-IF
      *
           END-IF
      *
           .
      *
       2108-ROUJIN41-JYOGEN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¿ÇÎÅ·îÆâ¤Î¼ýÇ¼¥Þ¥¹¥¿¤«¤é¸øÈñËè¤Î´µ¼ÔÉéÃ´¶â³Û¤Î
      *    Îß·×¤òÄ´ºº
      ******************************************************************
       220-SYUNOU-SEC           SECTION.
      *
           IF  (COMB-KOH1-NYUJGNKBN       =       "1")      OR
               (COMB-KOH2-NYUJGNKBN       =       "1")      OR
               (COMB-KOH3-NYUJGNKBN       =       "1")      OR
               (COMB-KOH4-NYUJGNKBN       =       "1")      OR
               (LNK-S02-KOUREI-NYUIN-KBN  =       "1")
      *
               IF   LNK-S02-TEIKI-KBN     =   "X"
                 CONTINUE
               ELSE
                 INITIALIZE                         SYU-SYUNOU-REC
                 MOVE    LNK-SYU-HOSPID     TO      SYU-HOSPID
                 MOVE    LNK-SYU-NYUGAIKBN  TO      SYU-NYUGAIKBN
                 MOVE    LNK-SYU-PTID       TO      SYU-PTID
                 MOVE    LNK-SYU-SRYYMD(1:6)  TO    SYU-SRYYMD(1:6)
                 MOVE    "__"                 TO    SYU-SRYYMD(7:2)
                 MOVE    SYU-SYUNOU-REC     TO      MCPDATA-REC
                 MOVE    "DBSELECT"         TO      MCP-FUNC
                 MOVE    "SYUNOU-KEY14"     TO      ORC-DBPATH
                 CALL    "ORCMCPSUB"         USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
                 IF      MCP-RC             =       ZERO
                   MOVE    "SYUNOU-KEY14"   TO      ORC-DBPATH
                   PERFORM  SYUNOU-READ-SEC
                   PERFORM  UNTIL  (FLG-SYUNOU   =    "1")
                     IF  SYU-DENPJTIKBN    NOT =  "3"
                      IF   LNK-SYU-SKYSTYMD    > SYU-SKYSTYMD
                             IF   SYU-DAYINFFLG  =  "1"
                                PERFORM     SYUDAY-READ-SEC
                             ELSE
                                INITIALIZE  SYU-SYUDAY-REC
                             END-IF
                             PERFORM        2201-SYUNOU-COUNT-SEC
                      END-IF
                     END-IF
                     MOVE           "SYUNOU-KEY14"   TO   ORC-DBPATH
                     PERFORM        SYUNOU-READ-SEC
                   END-PERFORM
                 ELSE
                   MOVE    12              TO        LNK-S02-RC
                 END-IF
      *
                 MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
                 MOVE    "SYUNOU-KEY14"          TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"              USING
                                                  MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
               END-IF
      *
               PERFORM    VARYING   IDZ    FROM   1   BY   1
                           UNTIL  (IDZ    >    10)
                             OR   (LNK-WRK-SYU-SKYSTYMD(IDZ) =  SPACE)
      *
                  IF  LNK-SYU-SKYSTYMD(1:6) = 
                                   LNK-WRK-SYU-SKYSTYMD(IDZ)(1:6) 
                      MOVE    LNK-WRK-SYUNOU-REC(IDZ)  
                                             TO  SYU-SYUNOU-REC
                      IF   SYU-DAYINFFLG  =  "1"
                         MOVE    LNK-WRK-SYUDAY-REC(IDZ)
                                             TO  SYU-SYUDAY-REC
                      ELSE
                         INITIALIZE  SYU-SYUDAY-REC
                      END-IF
                      PERFORM        2201-SYUNOU-COUNT-SEC
                  END-IF
      *
               END-PERFORM
      *
           END-IF.
      *
           IF     FLG-IRYOHI     =      1
             IF  WK-KOUREI-IRYOHI  >  361500
                COMPUTE WK-KOUREI-IRYOHI2 = 
                      (( WK-KOUREI-IRYOHI  -  361500 ) * 0.01 ) + 0.5
                COMPUTE  MAX-KIN-KOHI1-M  =   MAX-KIN-KOHI1-M 
                                          +   WK-KOUREI-IRYOHI2
                MOVE    1    TO    FLG-KOUGAKUCYO
             END-IF
           END-IF.
           IF     FLG-IRYOHI     =      2
             IF  WK-KOUREI-IRYOHI  >  361500
                COMPUTE WK-KOUREI-IRYOHI2 = 
                      (( WK-KOUREI-IRYOHI  -  361500 ) * 0.01 ) + 0.5
                COMPUTE  MAX-KIN-SYU-M    =   MAX-KIN-SYU-M 
                                          +   WK-KOUREI-IRYOHI2
                MOVE    1    TO    FLG-KOUGAKUCYO
             END-IF
           END-IF.
      *
      *    £´£±Ï·¿ÍÍÑ
           IF     FLG-IRYOHI2     =      1
             IF  WK-KOUREI-IRYOHI  >  361500
                COMPUTE WK-KOUREI-IRYOHI2 = 
                      (( WK-KOUREI-IRYOHI  -  361500 ) * 0.01 ) + 0.5
                COMPUTE  MAX-KIN-KOHI1-M  =   MAX-KIN-KOHI1-M 
                                          +   WK-KOUREI-IRYOHI2
                MOVE    1    TO    FLG-KOUGAKUCYO2
             END-IF
           END-IF.
           IF     FLG-IRYOHI2     =      2
             IF  WK-KOUREI-IRYOHI  >  361500
                COMPUTE WK-KOUREI-IRYOHI2 = 
                      (( WK-KOUREI-IRYOHI  -  361500 ) * 0.01 ) + 0.5
                COMPUTE  MAX-KIN-KOHI2-M  =   MAX-KIN-KOHI2-M 
                                          +   WK-KOUREI-IRYOHI2
                MOVE    1    TO    FLG-KOUGAKUCYO2
             END-IF
           END-IF.
           IF     FLG-IRYOHI2     =      3
             IF  WK-KOUREI-IRYOHI  >  361500
                COMPUTE WK-KOUREI-IRYOHI2 = 
                      (( WK-KOUREI-IRYOHI  -  361500 ) * 0.01 ) + 0.5
                COMPUTE  MAX-KIN-KOHI3-M  =   MAX-KIN-KOHI3-M 
                                          +   WK-KOUREI-IRYOHI2
                MOVE    1    TO    FLG-KOUGAKUCYO2
             END-IF
           END-IF.
           IF     FLG-IRYOHI2     =      4
             IF  WK-KOUREI-IRYOHI  >  361500
                COMPUTE WK-KOUREI-IRYOHI2 = 
                      (( WK-KOUREI-IRYOHI  -  361500 ) * 0.01 ) + 0.5
                COMPUTE  MAX-KIN-KOHI4-M  =   MAX-KIN-KOHI4-M 
                                          +   WK-KOUREI-IRYOHI2
                MOVE    1    TO    FLG-KOUGAKUCYO2
             END-IF
           END-IF.
      *
       220-SYUNOU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¿ÇÎÅ·î¤Î¼ýÇ¼¥Þ¥¹¥¿¤«¤é¸øÈñ¤Î¶â³Û¥«¥¦¥ó¥È
      ******************************************************************
       2201-SYUNOU-COUNT-SEC          SECTION.
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
                   MOVE    "DBCLOSECURSOR"  TO      MCP-FUNC
                   MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
                   CALL    "ORCMCPSUB"       USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
                   DISPLAY "HKNCOMBI DBFETCH ERR"
                   MOVE     10          TO      LNK-S02-RC
                   GO   TO              2201-SYUNOU-COUNT-EXT
               END-IF
           ELSE
               MOVE    "DBCLOSECURSOR"  TO      MCP-FUNC
               MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
               CALL    "ORCMCPSUB"       USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
               MOVE    1                TO      FLG-HKNCOMBI
               DISPLAY "HKNCOMBI DBSELECT ERR"
               MOVE     10              TO      LNK-S02-RC
               GO   TO                  2201-SYUNOU-COUNT-EXT
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
                IF   LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH3ID    TO  WRK-COMB-KOH1ID
                END-IF
                IF   LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH4ID    TO  WRK-COMB-KOH1ID
                END-IF
              END-IF
              IF     SYU-KOH2HKNNUM  =  "051" 
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
              IF     SYU-KOH3HKNNUM  =  "051" 
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
              IF     SYU-KOH4HKNNUM  =  "051" 
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
                IF   LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH3ID    TO  WRK-COMB-KOH1ID
                END-IF
                IF   LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM
                   MOVE   COMB-KOH4ID    TO  WRK-COMB-KOH1ID
                END-IF
              END-IF
              IF     SYU-KOH2HKNNUM  =  "052"
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
              IF     SYU-KOH3HKNNUM  =  "052"
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
              IF     SYU-KOH4HKNNUM  =  "052"
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
      *
           END-IF.
      *
           MOVE     ZERO     TO     FLG-KOUREI.                         
      *
           IF  LNK-S02-KOUREI-NYUIN-KBN   =   "1"
               IF ( SYU-SYUHKNNUM       =  "971" OR "973" )
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
      ***
                      IF   FLG-SYUHEN   =   "1"
                        IF  ((SYU-KOH2HKNNUM     NOT = SPACE)              
                        OR   (SYU-KOH3HKNNUM     NOT = SPACE)
                        OR   (SYU-KOH4HKNNUM     NOT = SPACE))
                        AND ( SYU-KOH1HKNNUM         = "027" )
                          ADD   SYU-KOH1COMPFTN   TO   FTN-KOHI1-M-HEN  
                        END-IF
                      END-IF
      ***
                      IF    SYU-KOH1HKNNUM         = "027"
                       IF  (SYU-KOH2HKNNUM     NOT = SPACE)              
                       OR  (SYU-KOH3HKNNUM     NOT = SPACE)
                       OR  (SYU-KOH4HKNNUM     NOT = SPACE)
                        MOVE     SYU-KOH-FTN  TO  WK-SYU-SYUCOMPFTN   
      *
                        IF  FTN-KOH-XXX  <  40200                       
                            ADD  WK-SYU-SYUCOMPFTN  TO  FTN-KOH-XXX       
                            IF   FTN-KOH-XXX     >   40200                
                               MOVE   40200   TO   FTN-KOH-XXX            
                            END-IF                                        
                        END-IF
      *
                        COMPUTE WK-ENTANI = SYU-KOH-FTN-ENTANI + 0.9
      *
                        IF  FTN-KOH-XXX-ENTANI  <  40200
                            ADD  WK-ENTANI  TO  FTN-KOH-XXX-ENTANI
                            IF   FTN-KOH-XXX-ENTANI  >   40200
                               MOVE   40200   TO   FTN-KOH-XXX-ENTANI
                            END-IF
                        END-IF
                       ELSE
                        IF  (SYU-KOH2HKNNUM     =  SPACE)              
                        AND (SYU-KOH3HKNNUM     =  SPACE)
                        AND (SYU-KOH4HKNNUM     =  SPACE)
                          MOVE   SYU-KOH1COMPFTN
                                            TO   WK-SYU-SYUCOMPFTN
      *
                          IF  ( COMB-KOH2HKNNUM        =    SPACE)
                          AND ( COMB-KOH3HKNNUM        =    SPACE)
                          AND ( COMB-KOH4HKNNUM        =    SPACE)
                             COMPUTE   WK-KOUREI-IRYOHI  =   
                                       WK-KOUREI-IRYOHI  +  
                                      (SYU-HKNTEN (12) * 10)
                          END-IF
      *
                          COMPUTE WK-ENTANI =
                                 SYU-KOH1COMPFTN-ENTANI + 0.9
                        END-IF
                       END-IF
                       ADD   WK-SYU-SYUCOMPFTN     TO   FTN-KOHI1-M      
                       ADD   WK-ENTANI       TO   FTN-KOHI1-M-ENTANI
      *
                       MOVE  1                     TO   FLG-KOUREI       
                      END-IF
                 ELSE                                                   
      *               COMPUTE  WK-SYU-SYUCOMPFTN  =                     
      *                                 SYU-SYUCOMPFTN     +   5        
      *               COMPUTE  WK-SYU-SYUCOMPFTN  =                     
      *                                 WK-SYU-SYUCOMPFTN  /  10        
      *               COMPUTE  WK-SYU-SYUCOMPFTN  =                     
      *                                 WK-SYU-SYUCOMPFTN  *  10        
      ***
                      IF   FLG-SYUHEN   =   "1"
                        IF  ((SYU-KOH1HKNNUM     NOT = SPACE)              
                        OR   (SYU-KOH2HKNNUM     NOT = SPACE)
                        OR   (SYU-KOH3HKNNUM     NOT = SPACE)
                        OR   (SYU-KOH4HKNNUM     NOT = SPACE))
                        AND ( SYU-SYUHKNNUM      NOT = SPACE )
                          ADD   SYU-SYUCOMPFTN    TO   FTN-SYU-M-HEN  
                        END-IF
                      END-IF
      ***
                      IF   (SYU-SYUHKNNUM      NOT = SPACE)
                      AND  (COMB-HKNNUM        NOT = SPACE)
                       IF  (SYU-KOH1HKNNUM     NOT = SPACE)              
                       OR  (SYU-KOH2HKNNUM     NOT = SPACE)
                       OR  (SYU-KOH3HKNNUM     NOT = SPACE)
                       OR  (SYU-KOH4HKNNUM     NOT = SPACE)
                        MOVE     SYU-KOH-FTN  TO  WK-SYU-SYUCOMPFTN 
      *
                        IF  FTN-KOH-XXX    <  40200                      
                            ADD  WK-SYU-SYUCOMPFTN  TO  FTN-KOH-XXX       
                            IF   FTN-KOH-XXX     >   40200                
                               MOVE   40200   TO   FTN-KOH-XXX            
                            END-IF                                        
                        END-IF
      *
                        COMPUTE WK-ENTANI = SYU-KOH-FTN-ENTANI + 0.9
      *
                        IF  FTN-KOH-XXX-ENTANI    <  40200
                            ADD  WK-ENTANI  TO  FTN-KOH-XXX-ENTANI
                            IF   FTN-KOH-XXX-ENTANI     >   40200
                               MOVE   40200   TO   FTN-KOH-XXX-ENTANI
                            END-IF
                        END-IF
                       ELSE
                        IF  (SYU-KOH1HKNNUM     =  SPACE)              
                        AND (SYU-KOH2HKNNUM     =  SPACE)
                        AND (SYU-KOH3HKNNUM     =  SPACE)
                        AND (SYU-KOH4HKNNUM     =  SPACE)
                          MOVE   SYU-SYUCOMPFTN
                                            TO   WK-SYU-SYUCOMPFTN
      *
                          IF  ( COMB-KOH1HKNNUM        =    SPACE)
                          AND ( COMB-KOH2HKNNUM        =    SPACE)
                          AND ( COMB-KOH3HKNNUM        =    SPACE)
                          AND ( COMB-KOH4HKNNUM        =    SPACE)
                             COMPUTE   WK-KOUREI-IRYOHI  =   
                                       WK-KOUREI-IRYOHI  +  
                                      (SYU-HKNTEN (12) * 10)
                          END-IF
      *
                          COMPUTE WK-ENTANI =
                                 SYU-SYUCOMPFTN-ENTANI + 0.9
                        END-IF
                       END-IF
                       ADD   WK-SYU-SYUCOMPFTN     TO   FTN-SYU-M        
                       ADD   WK-ENTANI       TO   FTN-SYU-M-ENTANI
                      END-IF
                 END-IF                                                 
      *
               END-IF
           END-IF.
      *
      *    ¸øÈñ£±¥«¥¦¥ó¥È
           IF  (COMB-KOH1HKNNUM               NOT =   SPACE)  AND
               (COMB-KOH1-NYUJGNKBN           =       "1"  )  AND
               (FLG-KOUREI                    =       ZERO )            
               IF   ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM ) AND
                     (COMB-KOH1ID         =  WRK-COMB-KOH1ID))
               OR   ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM ) AND
                     (COMB-KOH1ID         =  WRK-COMB-KOH2ID))
               OR   ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH3HKNNUM ) AND
                     (COMB-KOH1ID         =  WRK-COMB-KOH3ID))
               OR   ((LNK-SYU-KOH1HKNNUM  =  SYU-KOH4HKNNUM ) AND
                     (COMB-KOH1ID         =  WRK-COMB-KOH4ID))
      *
                 IF     FLG-IRYOHI2     =      1
                   COMPUTE  WK-KOUREI-IRYOHI  =  WK-KOUREI-IRYOHI  +  
                                                (SYU-HKNTEN (12) * 10)
                 END-IF
      *
                 IF  (WK-1001-PREFNUM         =  07      ) AND
                     (LNK-SYU-KOH1HKNNUM      = "245"    ) AND
                     (LNK-SYU-SRYYMD(1:6)    >= "200412" )
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH1HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH1COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH2HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH2COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH3HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH3HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH3COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH4HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH4HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH4COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH4COMPFTN-ENTANI
                      END-IF
                 END-IF
      *
                 IF  (WK-1001-PREFNUM         =  33      ) AND
                     (LNK-SYU-KOH1HKNNUM      = "285"    ) AND
                     (LNK-SYU-SRYYMD(1:6)    >= "200110" )
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH1HKNNUM
                         COMPUTE  SYU-KOH1HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH1HKNFTNMONEY
                         COMPUTE  SYU-KOH1COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH1COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH1COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH1COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH2HKNNUM
                         COMPUTE  SYU-KOH2HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH2HKNFTNMONEY
                         COMPUTE  SYU-KOH2COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH2COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH2COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH2COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH3HKNNUM
                         COMPUTE  SYU-KOH3HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH3HKNFTNMONEY
                         COMPUTE  SYU-KOH3COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH3COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH3COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH3COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH1HKNNUM  =  SYU-KOH4HKNNUM
                         COMPUTE  SYU-KOH4HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH4HKNFTNMONEY
                         COMPUTE  SYU-KOH4COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH4COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH4COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH4COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                 END-IF
      *
                 IF       MAX-KIN-KOHI1-D       NOT = ZERO
                 AND      MAX-KIN-KOHI1-M           = ZERO
                   PERFORM   VARYING  IDX2  FROM  1  BY  1
                              UNTIL   IDX2   >   31
                     IF   SYU-DAY(IDX2)  =  "1" OR "2"
                        EVALUATE  TRUE
                           WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH1HKNNUM)
                             IF  KOHI1-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 1)     =  1
                                 ADD     1      TO    KOHI1-TBL(IDX2)
                                 ADD     1      TO    KOHI1-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 1)
                                          TO  FTN-KOHI1-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 1)
                                          TO  FTN-KOHI1-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 1)
                                          TO  FTN-KOHI1-D3(IDX2)
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 1)
                                           TO  FTN-KOHI1-D(IDX2)
                                               FTN-KOHI1-D2(IDX2)
                                               FTN-KOHI1-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH2HKNNUM)
                             IF  KOHI1-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 2)     =  1
                                 ADD     1      TO    KOHI1-TBL(IDX2)
                                 ADD     1      TO    KOHI1-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 2)
                                          TO  FTN-KOHI1-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 2)
                                          TO  FTN-KOHI1-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 2)
                                          TO  FTN-KOHI1-D3(IDX2)
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 2)
                                           TO  FTN-KOHI1-D(IDX2)
                                               FTN-KOHI1-D2(IDX2)
                                               FTN-KOHI1-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH3HKNNUM)
                             IF  KOHI1-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 3)     =  1
                                 ADD     1      TO    KOHI1-TBL(IDX2)
                                 ADD     1      TO    KOHI1-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 3)
                                          TO  FTN-KOHI1-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 3)
                                          TO  FTN-KOHI1-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 3)
                                          TO  FTN-KOHI1-D3(IDX2)
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 3)
                                           TO  FTN-KOHI1-D(IDX2)
                                               FTN-KOHI1-D2(IDX2)
                                               FTN-KOHI1-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH4HKNNUM)
                             IF  KOHI1-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 4)     =  1
                                 ADD     1      TO    KOHI1-TBL(IDX2)
                                 ADD     1      TO    KOHI1-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 4)
                                          TO  FTN-KOHI1-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 4)
                                          TO  FTN-KOHI1-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 4)
                                          TO  FTN-KOHI1-D3(IDX2)
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 4)
                                           TO  FTN-KOHI1-D(IDX2)
                                               FTN-KOHI1-D2(IDX2)
                                               FTN-KOHI1-D3(IDX2)
                             END-IF
                        END-EVALUATE
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 EVALUATE  TRUE
                       WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH1HKNNUM)
                             ADD   SYU-KOH1HKNFTNMONEY
                                                TO  FTN-KOHI1-M
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH1SKJ-FTN
                                                TO  FTN-KOHI1-M
                             END-IF
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI1-M2 =                  
                                        FTN-KOHI1-M2     +              
                                        SYU-KOH1COMPFTN  +              
                                        SYU-KOH1SKJ-FTN                 
                               COMPUTE  FTN-KOHI1-M3 =                  
                                        FTN-KOHI1-M3            +       
                                        SYU-KOH1COMPFTN-ENTANI  +       
                                        SYU-KOH1SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH1COMPFTN                    
                                                TO  FTN-KOHI1-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI1-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH2HKNNUM)
                             ADD   SYU-KOH2HKNFTNMONEY
                                                TO  FTN-KOHI1-M
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH2SKJ-FTN
                                                TO  FTN-KOHI1-M
                             END-IF
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI1-M2 =                  
                                        FTN-KOHI1-M2     +              
                                        SYU-KOH2COMPFTN  +              
                                        SYU-KOH2SKJ-FTN                 
                               COMPUTE  FTN-KOHI1-M3 =                  
                                        FTN-KOHI1-M3            +       
                                        SYU-KOH2COMPFTN-ENTANI  +       
                                        SYU-KOH2SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH2COMPFTN                    
                                                TO  FTN-KOHI1-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI1-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH3HKNNUM)
                             ADD   SYU-KOH3HKNFTNMONEY
                                                TO  FTN-KOHI1-M
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH3SKJ-FTN
                                                TO  FTN-KOHI1-M
                             END-IF
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI1-M2 =                  
                                        FTN-KOHI1-M2     +              
                                        SYU-KOH3COMPFTN  +              
                                        SYU-KOH3SKJ-FTN                 
                               COMPUTE  FTN-KOHI1-M3 =                  
                                        FTN-KOHI1-M3            +       
                                        SYU-KOH3COMPFTN-ENTANI  +       
                                        SYU-KOH3SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH3COMPFTN                    
                                                TO  FTN-KOHI1-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI1-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH1HKNNUM = SYU-KOH4HKNNUM)
                             ADD   SYU-KOH4HKNFTNMONEY
                                                TO  FTN-KOHI1-M
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH4SKJ-FTN
                                                TO  FTN-KOHI1-M
                             END-IF
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI1-M2 =                  
                                        FTN-KOHI1-M2     +              
                                        SYU-KOH4COMPFTN  +              
                                        SYU-KOH4SKJ-FTN                 
                               COMPUTE  FTN-KOHI1-M3 =                  
                                        FTN-KOHI1-M3            +       
                                        SYU-KOH4COMPFTN-ENTANI  +       
                                        SYU-KOH4SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH4COMPFTN                    
                                                TO  FTN-KOHI1-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI1-M3        
                             END-IF                                     
                 END-EVALUATE
               END-IF
           END-IF.
      *    ¸øÈñ£²¥«¥¦¥ó¥È
           IF  (COMB-KOH2HKNNUM               NOT = SPACE)   AND
               (COMB-KOH2-NYUJGNKBN           =     "1")     
               IF   ((LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM ) AND
                     (COMB-KOH2ID         =  WRK-COMB-KOH1ID))
               OR   ((LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM ) AND
                     (COMB-KOH2ID         =  WRK-COMB-KOH2ID))
               OR   ((LNK-SYU-KOH2HKNNUM  =  SYU-KOH3HKNNUM ) AND
                     (COMB-KOH2ID         =  WRK-COMB-KOH3ID))
               OR   ((LNK-SYU-KOH2HKNNUM  =  SYU-KOH4HKNNUM ) AND
                     (COMB-KOH2ID         =  WRK-COMB-KOH4ID))
      *
                 IF     FLG-IRYOHI2     =      2
                   COMPUTE  WK-KOUREI-IRYOHI  =  WK-KOUREI-IRYOHI  +  
                                                (SYU-HKNTEN (12) * 10)
                 END-IF
      *
                 IF  (WK-1001-PREFNUM         =  07      ) AND
                     (LNK-SYU-KOH2HKNNUM      = "245"    ) AND
                     (LNK-SYU-SRYYMD(1:6)    >= "200412" )
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH1HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH1COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH2HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH2COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH3HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH3HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH3COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH4HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH4HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH4COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH4COMPFTN-ENTANI
                      END-IF
                 END-IF
      *
                 IF  (WK-1001-PREFNUM         =  33      ) AND
                     (LNK-SYU-KOH2HKNNUM      = "285"    ) AND
                     (LNK-SYU-SRYYMD(1:6)    >= "200110" )
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH1HKNNUM
                         COMPUTE  SYU-KOH1HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH1HKNFTNMONEY
                         COMPUTE  SYU-KOH1COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH1COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH1COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH1COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH2HKNNUM
                         COMPUTE  SYU-KOH2HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH2HKNFTNMONEY
                         COMPUTE  SYU-KOH2COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH2COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH2COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH2COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH3HKNNUM
                         COMPUTE  SYU-KOH3HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH3HKNFTNMONEY
                         COMPUTE  SYU-KOH3COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH3COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH3COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH3COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH2HKNNUM  =  SYU-KOH4HKNNUM
                         COMPUTE  SYU-KOH4HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH4HKNFTNMONEY
                         COMPUTE  SYU-KOH4COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH4COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH4COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH4COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                 END-IF
      *
                 IF       MAX-KIN-KOHI2-D       NOT = ZERO
                 AND      MAX-KIN-KOHI2-M           = ZERO
                   PERFORM   VARYING  IDX2  FROM  1  BY  1
                              UNTIL   IDX2   >   31
                     IF   SYU-DAY(IDX2)  =  "1" OR "2"
                        EVALUATE  TRUE
                           WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH1HKNNUM)
                             IF  KOHI2-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 1)     =  1
                                 ADD     1      TO    KOHI2-TBL(IDX2)
                                 ADD     1      TO    KOHI2-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 1)
                                           TO  FTN-KOHI2-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 1)
                                           TO  FTN-KOHI2-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 1)
                                           TO  FTN-KOHI2-D3(IDX2)
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 1)
                                           TO  FTN-KOHI2-D(IDX2)
                                               FTN-KOHI2-D2(IDX2)
                                               FTN-KOHI2-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH2HKNNUM)
                             IF  KOHI2-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 2)     =  1
                                 ADD     1      TO    KOHI2-TBL(IDX2)
                                 ADD     1      TO    KOHI2-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 2)
                                           TO  FTN-KOHI2-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 2)
                                           TO  FTN-KOHI2-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 2)
                                           TO  FTN-KOHI2-D3(IDX2)
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 2)
                                           TO  FTN-KOHI2-D(IDX2)
                                               FTN-KOHI2-D2(IDX2)
                                               FTN-KOHI2-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH3HKNNUM)
                             IF  KOHI2-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 3)     =  1
                                 ADD     1      TO    KOHI2-TBL(IDX2)
                                 ADD     1      TO    KOHI2-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 3)
                                           TO  FTN-KOHI2-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 3)
                                           TO  FTN-KOHI2-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 3)
                                           TO  FTN-KOHI2-D3(IDX2)
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 3)
                                           TO  FTN-KOHI2-D(IDX2)
                                               FTN-KOHI2-D2(IDX2)
                                               FTN-KOHI2-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH4HKNNUM)
                             IF  KOHI2-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 4)     =  1
                                 ADD     1      TO    KOHI2-TBL(IDX2)
                                 ADD     1      TO    KOHI2-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 4)
                                           TO  FTN-KOHI2-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 4)
                                           TO  FTN-KOHI2-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 4)
                                           TO  FTN-KOHI2-D3(IDX2)
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 4)
                                           TO  FTN-KOHI2-D(IDX2)
                                               FTN-KOHI2-D2(IDX2)
                                               FTN-KOHI2-D3(IDX2)
                             END-IF
                        END-EVALUATE
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 EVALUATE  TRUE
                       WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH1HKNNUM)
                             ADD   SYU-KOH1HKNFTNMONEY
                                                TO  FTN-KOHI2-M
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH1SKJ-FTN
                                                TO  FTN-KOHI2-M
                             END-IF
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI2-M2 =                  
                                        FTN-KOHI2-M2     +              
                                        SYU-KOH1COMPFTN  +              
                                        SYU-KOH1SKJ-FTN                 
                               COMPUTE  FTN-KOHI2-M3 =                  
                                        FTN-KOHI2-M3            +       
                                        SYU-KOH1COMPFTN-ENTANI  +       
                                        SYU-KOH1SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH1COMPFTN                    
                                                TO  FTN-KOHI2-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI2-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH2HKNNUM)
                             ADD   SYU-KOH2HKNFTNMONEY
                                                TO  FTN-KOHI2-M
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH2SKJ-FTN
                                                TO  FTN-KOHI2-M
                             END-IF
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI2-M2 =                  
                                        FTN-KOHI2-M2     +              
                                        SYU-KOH2COMPFTN  +              
                                        SYU-KOH2SKJ-FTN                 
                               COMPUTE  FTN-KOHI2-M3 =                  
                                        FTN-KOHI2-M3            +       
                                        SYU-KOH2COMPFTN-ENTANI  +       
                                        SYU-KOH2SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH2COMPFTN                    
                                                TO  FTN-KOHI2-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI2-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH3HKNNUM)
                             ADD   SYU-KOH3HKNFTNMONEY
                                                TO  FTN-KOHI2-M
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH3SKJ-FTN
                                                TO  FTN-KOHI2-M
                             END-IF
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI2-M2 =                  
                                        FTN-KOHI2-M2     +              
                                        SYU-KOH3COMPFTN  +              
                                        SYU-KOH3SKJ-FTN                 
                               COMPUTE  FTN-KOHI2-M3 =                  
                                        FTN-KOHI2-M3            +       
                                        SYU-KOH3COMPFTN-ENTANI  +       
                                        SYU-KOH3SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH3COMPFTN                    
                                                TO  FTN-KOHI2-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI2-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH2HKNNUM = SYU-KOH4HKNNUM)
                             ADD   SYU-KOH4HKNFTNMONEY
                                                TO  FTN-KOHI2-M
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH4SKJ-FTN
                                                TO  FTN-KOHI2-M
                             END-IF
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI2-M2 =                  
                                        FTN-KOHI2-M2     +              
                                        SYU-KOH4COMPFTN  +              
                                        SYU-KOH4SKJ-FTN                 
                               COMPUTE  FTN-KOHI2-M3 =                  
                                        FTN-KOHI2-M3            +       
                                        SYU-KOH4COMPFTN-ENTANI  +       
                                        SYU-KOH4SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH4COMPFTN                    
                                                TO  FTN-KOHI2-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI2-M3        
                             END-IF                                     
                 END-EVALUATE
               END-IF
           END-IF.
      *    ¸øÈñ£³¥«¥¦¥ó¥È
           IF  (COMB-KOH3HKNNUM               NOT = SPACE)   AND
               (COMB-KOH3-NYUJGNKBN           =     "1")     
               IF   ((LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM ) AND
                     (COMB-KOH3ID         =  WRK-COMB-KOH1ID))
               OR   ((LNK-SYU-KOH3HKNNUM  =  SYU-KOH2HKNNUM ) AND
                     (COMB-KOH3ID         =  WRK-COMB-KOH2ID))
               OR   ((LNK-SYU-KOH3HKNNUM  =  SYU-KOH3HKNNUM ) AND
                     (COMB-KOH3ID         =  WRK-COMB-KOH3ID))
               OR   ((LNK-SYU-KOH3HKNNUM  =  SYU-KOH4HKNNUM ) AND
                     (COMB-KOH3ID         =  WRK-COMB-KOH4ID))
      *
                 IF     FLG-IRYOHI2     =      3
                   COMPUTE  WK-KOUREI-IRYOHI  =  WK-KOUREI-IRYOHI  +  
                                                (SYU-HKNTEN (12) * 10)
                 END-IF
      *
                 IF  (WK-1001-PREFNUM         =  07      ) AND
                     (LNK-SYU-KOH3HKNNUM      = "245"    ) AND
                     (LNK-SYU-SRYYMD(1:6)    >= "200412" )
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH1HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH1COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH2HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH2HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH2COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH3HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH3HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH3COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH4HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH4HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH4COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH4COMPFTN-ENTANI
                      END-IF
                 END-IF
      *
                 IF  (WK-1001-PREFNUM         =  33      ) AND
                     (LNK-SYU-KOH3HKNNUM      = "285"    ) AND
                     (LNK-SYU-SRYYMD(1:6)    >= "200110" )
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH1HKNNUM
                         COMPUTE  SYU-KOH1HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH1HKNFTNMONEY
                         COMPUTE  SYU-KOH1COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH1COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH1COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH1COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH2HKNNUM
                         COMPUTE  SYU-KOH2HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH2HKNFTNMONEY
                         COMPUTE  SYU-KOH2COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH2COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH2COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH2COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH3HKNNUM
                         COMPUTE  SYU-KOH3HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH3HKNFTNMONEY
                         COMPUTE  SYU-KOH3COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH3COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH3COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH3COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH3HKNNUM  =  SYU-KOH4HKNNUM
                         COMPUTE  SYU-KOH4HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH4HKNFTNMONEY
                         COMPUTE  SYU-KOH4COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH4COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH4COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH4COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                 END-IF
      *
                 IF       MAX-KIN-KOHI3-D       NOT = ZERO
                 AND      MAX-KIN-KOHI3-M           = ZERO
                   PERFORM   VARYING  IDX2  FROM  1  BY  1
                              UNTIL   IDX2   >   31
                     IF   SYU-DAY(IDX2)  =  "1" OR "2"
                        EVALUATE  TRUE
                           WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH1HKNNUM)
                             IF  KOHI3-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 1)     =  1
                                 ADD     1      TO    KOHI3-TBL(IDX2)
                                 ADD     1      TO    KOHI3-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 1)
                                           TO  FTN-KOHI3-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 1)
                                           TO  FTN-KOHI3-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 1)
                                           TO  FTN-KOHI3-D3(IDX2)
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 1)
                                           TO  FTN-KOHI3-D(IDX2)
                                               FTN-KOHI3-D2(IDX2)
                                               FTN-KOHI3-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH2HKNNUM)
                             IF  KOHI3-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 2)     =  1
                                 ADD     1      TO    KOHI3-TBL(IDX2)
                                 ADD     1      TO    KOHI3-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 2)
                                           TO  FTN-KOHI3-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 2)
                                           TO  FTN-KOHI3-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 2)
                                           TO  FTN-KOHI3-D3(IDX2)
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 2)
                                           TO  FTN-KOHI3-D(IDX2)
                                               FTN-KOHI3-D2(IDX2)
                                               FTN-KOHI3-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH3HKNNUM)
                             IF  KOHI3-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 3)     =  1
                                 ADD     1      TO    KOHI3-TBL(IDX2)
                                 ADD     1      TO    KOHI3-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 3)
                                           TO  FTN-KOHI3-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 3)
                                           TO  FTN-KOHI3-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 3)
                                           TO  FTN-KOHI3-D3(IDX2)
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 3)
                                           TO  FTN-KOHI3-D(IDX2)
                                               FTN-KOHI3-D2(IDX2)
                                               FTN-KOHI3-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH4HKNNUM)
                             IF  KOHI3-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 4)     =  1
                                 ADD     1      TO    KOHI3-TBL(IDX2)
                                 ADD     1      TO    KOHI3-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 4)
                                           TO  FTN-KOHI3-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 4)
                                           TO  FTN-KOHI3-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 4)
                                           TO  FTN-KOHI3-D3(IDX2)
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 4)
                                           TO  FTN-KOHI3-D(IDX2)
                                               FTN-KOHI3-D2(IDX2)
                                               FTN-KOHI3-D3(IDX2)
                             END-IF
                        END-EVALUATE
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 EVALUATE  TRUE
                       WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH1HKNNUM)
                             ADD   SYU-KOH1HKNFTNMONEY
                                                TO  FTN-KOHI3-M
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH1SKJ-FTN
                                                TO  FTN-KOHI3-M
                             END-IF
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI3-M2 =                  
                                        FTN-KOHI3-M2     +              
                                        SYU-KOH1COMPFTN  +              
                                        SYU-KOH1SKJ-FTN                 
                               COMPUTE  FTN-KOHI3-M3 =                  
                                        FTN-KOHI3-M3            +       
                                        SYU-KOH1COMPFTN-ENTANI  +       
                                        SYU-KOH1SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH1COMPFTN                    
                                                TO  FTN-KOHI3-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI3-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH2HKNNUM)
                             ADD   SYU-KOH2HKNFTNMONEY
                                                TO  FTN-KOHI3-M
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH2SKJ-FTN
                                                TO  FTN-KOHI3-M
                             END-IF
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI3-M2 =                  
                                        FTN-KOHI3-M2     +              
                                        SYU-KOH2COMPFTN  +              
                                        SYU-KOH2SKJ-FTN                 
                               COMPUTE  FTN-KOHI3-M3 =                  
                                        FTN-KOHI3-M3            +       
                                        SYU-KOH2COMPFTN-ENTANI  +       
                                        SYU-KOH2SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH2COMPFTN                    
                                                TO  FTN-KOHI3-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI3-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH3HKNNUM)
                             ADD   SYU-KOH3HKNFTNMONEY
                                                TO  FTN-KOHI3-M
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH3SKJ-FTN
                                                TO  FTN-KOHI3-M
                             END-IF
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI3-M2 =                  
                                        FTN-KOHI3-M2     +              
                                        SYU-KOH3COMPFTN  +              
                                        SYU-KOH3SKJ-FTN                 
                               COMPUTE  FTN-KOHI3-M3 =                  
                                        FTN-KOHI3-M3            +       
                                        SYU-KOH3COMPFTN-ENTANI  +       
                                        SYU-KOH3SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH3COMPFTN                    
                                                TO  FTN-KOHI3-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI3-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH3HKNNUM = SYU-KOH4HKNNUM)
                             ADD   SYU-KOH4HKNFTNMONEY
                                                TO  FTN-KOHI3-M
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH4SKJ-FTN
                                                TO  FTN-KOHI3-M
                             END-IF
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI3-M2 =                  
                                        FTN-KOHI3-M2     +              
                                        SYU-KOH4COMPFTN  +              
                                        SYU-KOH4SKJ-FTN                 
                               COMPUTE  FTN-KOHI3-M3 =                  
                                        FTN-KOHI3-M3            +       
                                        SYU-KOH4COMPFTN-ENTANI  +       
                                        SYU-KOH4SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH4COMPFTN                    
                                                TO  FTN-KOHI3-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI3-M3        
                             END-IF                                     
                 END-EVALUATE
               END-IF
           END-IF.
      *    ¸øÈñ£´¥«¥¦¥ó¥È
           IF  (COMB-KOH4HKNNUM               NOT = SPACE)   AND
               (COMB-KOH4-NYUJGNKBN           =     "1")     
               IF   ((LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM ) AND
                     (COMB-KOH4ID         =  WRK-COMB-KOH1ID))
               OR   ((LNK-SYU-KOH4HKNNUM  =  SYU-KOH2HKNNUM ) AND
                     (COMB-KOH4ID         =  WRK-COMB-KOH2ID))
               OR   ((LNK-SYU-KOH4HKNNUM  =  SYU-KOH3HKNNUM ) AND
                     (COMB-KOH4ID         =  WRK-COMB-KOH3ID))
               OR   ((LNK-SYU-KOH4HKNNUM  =  SYU-KOH4HKNNUM ) AND
                     (COMB-KOH4ID         =  WRK-COMB-KOH4ID))
      *
                 IF     FLG-IRYOHI2     =      4
                   COMPUTE  WK-KOUREI-IRYOHI  =  WK-KOUREI-IRYOHI  +  
                                                (SYU-HKNTEN (12) * 10)
                 END-IF
      *
                 IF  (WK-1001-PREFNUM         =  07      ) AND
                     (LNK-SYU-KOH4HKNNUM      = "245"    ) AND
                     (LNK-SYU-SRYYMD(1:6)    >= "200412" )
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH1HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH1COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH1COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH2HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH2HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH2COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH2COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH3HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH3HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH3COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH3COMPFTN-ENTANI
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH4HKNNUM
                         MOVE   SYU-SHOKUJI5  TO  SYU-KOH4HKNFTNMONEY
                         MOVE   SYU-SHOKUJI6  TO  SYU-KOH4COMPFTN
                         MOVE   SYU-SHOKUJI7  TO  SYU-KOH4COMPFTN-ENTANI
                      END-IF
                 END-IF
      *
                 IF  (WK-1001-PREFNUM         =  33      ) AND
                     (LNK-SYU-KOH4HKNNUM      = "285"    ) AND
                     (LNK-SYU-SRYYMD(1:6)    >= "200110" )
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH1HKNNUM
                         COMPUTE  SYU-KOH1HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH1HKNFTNMONEY
                         COMPUTE  SYU-KOH1COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH1COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH1COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH1COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH2HKNNUM
                         COMPUTE  SYU-KOH2HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH2HKNFTNMONEY
                         COMPUTE  SYU-KOH2COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH2COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH2COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH2COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH3HKNNUM
                         COMPUTE  SYU-KOH3HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH3HKNFTNMONEY
                         COMPUTE  SYU-KOH3COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH3COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH3COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH3COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                      IF    LNK-SYU-KOH4HKNNUM  =  SYU-KOH4HKNNUM
                         COMPUTE  SYU-KOH4HKNFTNMONEY =
                                  SYU-SHOKUJI5 - SYU-KOH4HKNFTNMONEY
                         COMPUTE  SYU-KOH4COMPFTN     =
                                  SYU-SHOKUJI6 - SYU-KOH4COMPFTN
                         COMPUTE  WK-TOKUSYU   = SYU-SHOKUJI7
                                               + 0.9
                         COMPUTE  WK-TOKUSYU2  = SYU-KOH4COMPFTN-ENTANI
                                               + 0.9
                         COMPUTE  SYU-KOH4COMPFTN-ENTANI =
                                  WK-TOKUSYU   - WK-TOKUSYU2
                      END-IF
                 END-IF
      *
                 IF       MAX-KIN-KOHI4-D       NOT = ZERO
                 AND      MAX-KIN-KOHI4-M           = ZERO
                   PERFORM   VARYING  IDX2  FROM  1  BY  1
                              UNTIL   IDX2   >   31
                     IF   SYU-DAY(IDX2)  =  "1" OR "2"
                        EVALUATE  TRUE
                           WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH1HKNNUM)
                             IF  KOHI4-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 1)     =  1
                                 ADD     1      TO    KOHI4-TBL(IDX2)
                                 ADD     1      TO    KOHI4-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 1)
                                           TO  FTN-KOHI4-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 1)
                                           TO  FTN-KOHI4-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 1)
                                           TO  FTN-KOHI4-D3(IDX2)
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 1)
                                           TO  FTN-KOHI4-D(IDX2)
                                               FTN-KOHI4-D2(IDX2)
                                               FTN-KOHI4-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH2HKNNUM)
                             IF  KOHI4-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 2)     =  1
                                 ADD     1      TO    KOHI4-TBL(IDX2)
                                 ADD     1      TO    KOHI4-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 2)
                                           TO  FTN-KOHI4-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 2)
                                           TO  FTN-KOHI4-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 2)
                                           TO  FTN-KOHI4-D3(IDX2)
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 2)
                                           TO  FTN-KOHI4-D(IDX2)
                                               FTN-KOHI4-D2(IDX2)
                                               FTN-KOHI4-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH3HKNNUM)
                             IF  KOHI4-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 3)     =  1
                                 ADD     1      TO    KOHI4-TBL(IDX2)
                                 ADD     1      TO    KOHI4-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 3)
                                           TO  FTN-KOHI4-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 3)
                                           TO  FTN-KOHI4-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 3)
                                           TO  FTN-KOHI4-D3(IDX2)
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 3)
                                           TO  FTN-KOHI4-D(IDX2)
                                               FTN-KOHI4-D2(IDX2)
                                               FTN-KOHI4-D3(IDX2)
                             END-IF
                           WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH4HKNNUM)
                             IF  KOHI4-TBL(IDX2)            =  ZERO
                             AND SDAY-SANTEIFLG(IDX2 4)     =  1
                                 ADD     1      TO    KOHI4-TBL(IDX2)
                                 ADD     1      TO    KOHI4-TBL(32)
                             END-IF
                             ADD   SDAY-FTNMONEY(IDX2 4)
                                           TO  FTN-KOHI4-D(IDX2)
                             ADD   SDAY-COMPFTN(IDX2 4)
                                           TO  FTN-KOHI4-D2(IDX2)
                             ADD   SDAY-COMPFTNWK(IDX2 4)
                                           TO  FTN-KOHI4-D3(IDX2)
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"
                                 ADD  SDAY-SKJFTN(IDX2 4)
                                           TO  FTN-KOHI4-D(IDX2)
                                               FTN-KOHI4-D2(IDX2)
                                               FTN-KOHI4-D3(IDX2)
                             END-IF
                        END-EVALUATE
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 EVALUATE  TRUE
                       WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH1HKNNUM)
                             ADD   SYU-KOH1HKNFTNMONEY
                                                TO  FTN-KOHI4-M
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH1SKJ-FTN
                                                TO  FTN-KOHI4-M
                             END-IF
                             IF    SYU-KOH1SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI4-M2 =                  
                                        FTN-KOHI4-M2     +              
                                        SYU-KOH1COMPFTN  +              
                                        SYU-KOH1SKJ-FTN                 
                               COMPUTE  FTN-KOHI4-M3 =                  
                                        FTN-KOHI4-M3            +       
                                        SYU-KOH1COMPFTN-ENTANI  +       
                                        SYU-KOH1SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH1COMPFTN                    
                                                TO  FTN-KOHI4-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH1COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI4-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH2HKNNUM)
                             ADD   SYU-KOH2HKNFTNMONEY
                                                TO  FTN-KOHI4-M
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH2SKJ-FTN
                                                TO  FTN-KOHI4-M
                             END-IF
                             IF    SYU-KOH2SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI4-M2 =                  
                                        FTN-KOHI4-M2     +              
                                        SYU-KOH2COMPFTN  +              
                                        SYU-KOH2SKJ-FTN                 
                               COMPUTE  FTN-KOHI4-M3 =                  
                                        FTN-KOHI4-M3            +       
                                        SYU-KOH2COMPFTN-ENTANI  +       
                                        SYU-KOH2SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH2COMPFTN                    
                                                TO  FTN-KOHI4-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH2COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI4-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH3HKNNUM)
                             ADD   SYU-KOH3HKNFTNMONEY
                                                TO  FTN-KOHI4-M
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH3SKJ-FTN
                                                TO  FTN-KOHI4-M
                             END-IF
                             IF    SYU-KOH3SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI4-M2 =                  
                                        FTN-KOHI4-M2     +              
                                        SYU-KOH3COMPFTN  +              
                                        SYU-KOH3SKJ-FTN                 
                               COMPUTE  FTN-KOHI4-M3 =                  
                                        FTN-KOHI4-M3            +       
                                        SYU-KOH3COMPFTN-ENTANI  +       
                                        SYU-KOH3SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH3COMPFTN                    
                                                TO  FTN-KOHI4-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH3COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI4-M3        
                             END-IF                                     
                       WHEN  (LNK-SYU-KOH4HKNNUM = SYU-KOH4HKNNUM)
                             ADD   SYU-KOH4HKNFTNMONEY
                                                TO  FTN-KOHI4-M
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"
                                ADD   SYU-KOH4SKJ-FTN
                                                TO  FTN-KOHI4-M
                             END-IF
                             IF    SYU-KOH4SKJ-FTNKBN  =  "2"           
                               COMPUTE  FTN-KOHI4-M2 =                  
                                        FTN-KOHI4-M2     +              
                                        SYU-KOH4COMPFTN  +              
                                        SYU-KOH4SKJ-FTN                 
                               COMPUTE  FTN-KOHI4-M3 =                  
                                        FTN-KOHI4-M3            +       
                                        SYU-KOH4COMPFTN-ENTANI  +       
                                        SYU-KOH4SKJ-FTN         +       
                                        0.9
                             ELSE                                       
                               ADD   SYU-KOH4COMPFTN                    
                                                TO  FTN-KOHI4-M2        
                               COMPUTE WK-ENTANI =
                                           SYU-KOH4COMPFTN-ENTANI + 0.9
                               ADD   WK-ENTANI  TO  FTN-KOHI4-M3        
                             END-IF                                     
                 END-EVALUATE
               END-IF
           END-IF.
      *
       2201-SYUNOU-COUNT-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÊÝ¸±¡¦¸øÈñ¤ÎÉéÃ´¶â·×»»
      ******************************************************************
       230-FTNKIN-SEC           SECTION.
      *
      *    £±ÆüÃ±°Ì¤Î»»Äê²ÄÇ½»Ä³Û¤ò»»½Ð¤¹¤ë
      *
           PERFORM        2301-DAY-ZAN-HENSYU-SEC.
      *
      *    »»Äê²ÄÇ½¥Õ¥é¥°¤Î½é´ü²½
           MOVE     SPACE                  TO     FLG-SANTEI1
                                                  FLG-SANTEI2
                                                  FLG-SANTEI3
                                                  FLG-SANTEI4.
      *
      *    ²áµî¤Î´µ¼ÔÊÝ¸±ÉéÃ´Îß·×³Û¤È¾å¸Â¶â³Û¤ÎÈæ³Ó
      *
           IF   LNK-S02-KOUREI-NYUIN-KBN   =   "1"
               IF  MAX-KIN-SYU-M           NOT =  ZERO
                   IF  FTN-SYU-M           >=     MAX-KIN-SYU-M
                       MOVE  "1"           TO     FLG-SANTEI-SYU
                   END-IF
               END-IF
           END-IF.
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
           IF   LNK-S02-KOUREI-NYUIN-KBN   =   "1"
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
           IF  FLG-SANTEI1               =     "1"
               MOVE     ZERO             TO    ZAN-KOHI1-M
                                               ZAN-KOHI1-M2             
                                               ZAN-KOHI1-M3             
           ELSE
               IF (COMB-KOH1HKNNUM           NOT = SPACE)    AND
                  (COMB-KOH1-NYUJGNKBN       =     "1")
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
                  (COMB-KOH2-NYUJGNKBN       =     "1")
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
                  (COMB-KOH3-NYUJGNKBN       =     "1")
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
                  (COMB-KOH4-NYUJGNKBN       =     "1")
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
      *    ·î¤Î´µ¼ÔÉéÃ´²ÄÇ½»Ä³Û¤ò¥»¥Ã¥È¤¹¤ë
           IF (COMB-KOH1HKNNUM              NOT = SPACE)    AND
              (COMB-KOH1-NYUJGNKBN          =     "1")
                   MOVE   ZAN-KOHI1-M     TO   ZAN-KOHI1
                   MOVE   ZAN-KOHI1-M2    TO   ZAN-KOHI12               
                   MOVE   ZAN-KOHI1-M3    TO   ZAN-KOHI13               
           END-IF.
      *
           IF (COMB-KOH2HKNNUM              NOT = SPACE)    AND
              (COMB-KOH2-NYUJGNKBN          =     "1")
                   MOVE   ZAN-KOHI2-M     TO   ZAN-KOHI2
                   MOVE   ZAN-KOHI2-M2    TO   ZAN-KOHI22               
                   MOVE   ZAN-KOHI2-M3    TO   ZAN-KOHI23               
           END-IF.
      *
           IF (COMB-KOH3HKNNUM              NOT = SPACE)    AND
              (COMB-KOH3-NYUJGNKBN          =     "1")
                   MOVE   ZAN-KOHI3-M     TO   ZAN-KOHI3
                   MOVE   ZAN-KOHI3-M2    TO   ZAN-KOHI32               
                   MOVE   ZAN-KOHI3-M3    TO   ZAN-KOHI33               
           END-IF.
      *
           IF (COMB-KOH4HKNNUM              NOT = SPACE)    AND
              (COMB-KOH4-NYUJGNKBN          =     "1")
                   MOVE   ZAN-KOHI4-M     TO   ZAN-KOHI4
                   MOVE   ZAN-KOHI4-M2    TO   ZAN-KOHI42               
                   MOVE   ZAN-KOHI4-M3    TO   ZAN-KOHI43               
           END-IF.
      *
      *    ¼çÊÝ¸±ÉéÃ´¶â³Û¤Î¸øÈñ¸ÂÅÙ³ÛÂÐ±þÍÑ¡¡»Ä¶â³Û·×»»
           IF      FLG-SYUHEN   =   "1"
                PERFORM    2302-SYUHEN-ZANKIN-KEISAN-SEC
           END-IF.
      *    ¼çÊÝ¸±ÉéÃ´¶â³Û¤Î±ßÃ±°Ì¡¡»Ä¶â³Û·×»»                           
           IF   LNK-S02-KOUREI-NYUIN-KBN   =   "1"
                PERFORM    2303-SYUENTANI-ZANKIN-KEISAN-SEC
           END-IF.
      *
      *    ¿©»öÎÅÍÜÈñÅù¤Î·×»»
           PERFORM       2304-SHOKUJI-KEISAN-SEC.
      *
      *    ÊÝ¸±¡¦¸øÈñËè¤Î´µ¼ÔÉéÃ´¶â³Û¤Î·×»»
           MOVE     ZERO       TO       WK-DAY-NYUKFTNRATE.
           MOVE     ZERO       TO       WK-KOH-FTNRATE.
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
                  PERFORM    2305-SYUHOKEN-KEISAN-SEC
               END-IF
           END-IF.
      *
           MOVE     ZERO     TO     WK-TOKUTEI-COMPFTN3.
           MOVE     SPACE    TO     FLG-KUMAMOTO.
           INITIALIZE               WK-TAIHI-RECEFTNDSPKBN-OCC.
      *
           MOVE    ZERO      TO     WK-TOKUSYU-COMPFTN.
           MOVE    9999999   TO     WK-TOKUSYU-COMPFTN2.
           MOVE    ZERO      TO     WK-TOKUSYU-ENTANI.
      *
           PERFORM    VARYING   KOH-IDX    FROM   1   BY   1
                      UNTIL    (KOH-IDX    >      4)
               IF   COMB-KOHHKNNUM(KOH-IDX)  NOT =  SPACE
                    PERFORM    2306-KOH-HKNREAD-SEC
                    IF   FLG-CHIHOU(KOH-IDX)  =  "1"
                      IF    WK-NYUKFTNRATE  =  ZERO
                          MOVE  SPACE  TO  COMB-KOH-NYUJGNKBN(KOH-IDX)
                      END-IF
                    END-IF
                    IF   COMB-KOH-NYUJGNKBN(KOH-IDX)  =  "1"
                         PERFORM   2307-KOH-KEISAN-SEC
                    ELSE
                         PERFORM   2308-KOH-KEISAN2-SEC
                    END-IF
               END-IF 
           END-PERFORM.
      *
      *     ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â³Û¤«¤éºÇ¾®¤ÎÃÍ¤òÁªÂò¥»¥Ã¥È
            PERFORM   2309-FTNKIN-HIKAKU-SEC.
      *
       230-FTNKIN-EXT.
           EXIT.
      *
      ******************************************************************
      *        £±ÆüÃ±°Ì¤Î»»Äê²ÄÇ½»Ä³Û¤ò»»½Ð¤¹¤ë
      ******************************************************************
       2301-DAY-ZAN-HENSYU-SEC           SECTION.
      *
           INITIALIZE     WK-DAYINFFLG-AREA.
      *
           IF (COMB-KOH1HKNNUM           NOT = SPACE)    AND
              (COMB-KOH1-NYUJGNKBN       =     "1")
               IF       MAX-KIN-KOHI1-D       NOT = ZERO
               AND      MAX-KIN-KOHI1-M           = ZERO
                  PERFORM    VARYING   IDX2   FROM   1   BY   1
                              UNTIL    IDX2     >    31
                     IF   ( LNK-SYU-DAY(IDX2)          =  "1" OR "2" )
                     AND  ( LNK-SDAY-HKNTEN(IDX2)      NOT =  ZERO   )
                       IF   MAX-KAI-KOHI1-M       NOT = ZERO
                          IF  KOHI1-TBL(IDX2)  NOT = ZERO
                             MOVE     SPACE    TO    FLG-NG
                             MOVE      9       TO    KOHI1-TBL(IDX2)
                          ELSE
                             IF   KOHI1-TBL(32)  >=  MAX-KAI-KOHI1-M
                               MOVE   "1"      TO    FLG-NG
                             ELSE
                               MOVE   SPACE    TO    FLG-NG
                               MOVE    9       TO    KOHI1-TBL(IDX2)
                               ADD     1       TO    KOHI1-TBL(32)
                             END-IF
                          END-IF
                       ELSE
                          MOVE        SPACE    TO    FLG-NG
                          MOVE         9       TO    KOHI1-TBL(IDX2)
                       END-IF
                       IF     FLG-NG   =    SPACE  
                         MOVE        1         TO    WK-DAYINFFLG(1)
                         IF   FTN-KOHI1-D(IDX2)     >=  MAX-KIN-KOHI1-D
                              MOVE     ZERO    TO  ZAN-KOHI1-D(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI1-D(IDX2) =
                                       MAX-KIN-KOHI1-D   -
                                       FTN-KOHI1-D(IDX2)
                         END-IF
                         IF   FTN-KOHI1-D2(IDX2)  >=  MAX-KIN-KOHI1-D
                              MOVE     ZERO    TO  ZAN-KOHI1-D2(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI1-D2(IDX2) =
                                       MAX-KIN-KOHI1-D    -
                                       FTN-KOHI1-D2(IDX2)
                         END-IF
                         IF   FTN-KOHI1-D3(IDX2)  >=  MAX-KIN-KOHI1-D
                              MOVE     ZERO    TO  ZAN-KOHI1-D3(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI1-D3(IDX2) =
                                       MAX-KIN-KOHI1-D    -
                                       FTN-KOHI1-D3(IDX2)
                         END-IF
                       END-IF
                     END-IF
                  END-PERFORM
               END-IF
           END-IF.
      *
           IF (COMB-KOH2HKNNUM           NOT = SPACE)    AND
              (COMB-KOH2-NYUJGNKBN       =     "1")
               IF       MAX-KIN-KOHI2-D       NOT = ZERO
               AND      MAX-KIN-KOHI2-M           = ZERO
                  PERFORM    VARYING   IDX2   FROM   1   BY   1
                              UNTIL    IDX2     >    31
                     IF   ( LNK-SYU-DAY(IDX2)          =  "1" OR "2" )
                     AND  ( LNK-SDAY-HKNTEN(IDX2)      NOT =  ZERO   )
                       IF   MAX-KAI-KOHI2-M       NOT = ZERO
                          IF  KOHI2-TBL(IDX2)  NOT = ZERO
                             MOVE     SPACE    TO    FLG-NG
                             MOVE      9       TO    KOHI2-TBL(IDX2)
                          ELSE
                             IF   KOHI2-TBL(32)  >=  MAX-KAI-KOHI2-M
                               MOVE   "1"      TO    FLG-NG
                             ELSE
                               MOVE   SPACE    TO    FLG-NG
                               MOVE    9       TO    KOHI2-TBL(IDX2)
                               ADD     1       TO    KOHI2-TBL(32)
                             END-IF
                          END-IF
                       ELSE
                          MOVE        SPACE    TO    FLG-NG
                          MOVE         9       TO    KOHI2-TBL(IDX2)
                       END-IF
                       IF     FLG-NG   =    SPACE  
                         MOVE        1         TO    WK-DAYINFFLG(2)
                         IF   FTN-KOHI2-D(IDX2)     >=  MAX-KIN-KOHI2-D
                              MOVE     ZERO    TO  ZAN-KOHI2-D(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI2-D(IDX2) =
                                       MAX-KIN-KOHI2-D   -
                                       FTN-KOHI2-D(IDX2)
                         END-IF
                         IF   FTN-KOHI2-D2(IDX2)  >=  MAX-KIN-KOHI2-D
                              MOVE     ZERO    TO  ZAN-KOHI2-D2(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI2-D2(IDX2) =
                                       MAX-KIN-KOHI2-D    -
                                       FTN-KOHI2-D2(IDX2)
                         END-IF
                         IF   FTN-KOHI2-D3(IDX2)  >=  MAX-KIN-KOHI2-D
                              MOVE     ZERO    TO  ZAN-KOHI2-D3(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI2-D3(IDX2) =
                                       MAX-KIN-KOHI2-D    -
                                       FTN-KOHI2-D3(IDX2)
                         END-IF
                       END-IF
                     END-IF
                  END-PERFORM
               END-IF
           END-IF.
      *
           IF (COMB-KOH3HKNNUM           NOT = SPACE)    AND
              (COMB-KOH3-NYUJGNKBN       =     "1")
               IF       MAX-KIN-KOHI3-D       NOT = ZERO
               AND      MAX-KIN-KOHI3-M           = ZERO
                  PERFORM    VARYING   IDX2   FROM   1   BY   1
                              UNTIL    IDX2     >    31
                     IF   ( LNK-SYU-DAY(IDX2)          =  "1" OR "2" )
                     AND  ( LNK-SDAY-HKNTEN(IDX2)      NOT =  ZERO   )
                       IF   MAX-KAI-KOHI3-M       NOT = ZERO
                          IF  KOHI3-TBL(IDX2)  NOT = ZERO
                             MOVE     SPACE    TO    FLG-NG
                             MOVE      9       TO    KOHI3-TBL(IDX2)
                          ELSE
                             IF   KOHI3-TBL(32)  >=  MAX-KAI-KOHI3-M
                               MOVE   "1"      TO    FLG-NG
                             ELSE
                               MOVE   SPACE    TO    FLG-NG
                               MOVE    9       TO    KOHI3-TBL(IDX2)
                               ADD     1       TO    KOHI3-TBL(32)
                             END-IF
                          END-IF
                       ELSE
                          MOVE        SPACE    TO    FLG-NG
                          MOVE         9       TO    KOHI3-TBL(IDX2)
                       END-IF
                       IF     FLG-NG   =    SPACE  
                         MOVE        1         TO    WK-DAYINFFLG(3)
                         IF   FTN-KOHI3-D(IDX2)     >=  MAX-KIN-KOHI3-D
                              MOVE     ZERO    TO  ZAN-KOHI3-D(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI3-D(IDX2) =
                                       MAX-KIN-KOHI3-D   -
                                       FTN-KOHI3-D(IDX2)
                         END-IF
                         IF   FTN-KOHI3-D2(IDX2)  >=  MAX-KIN-KOHI3-D
                              MOVE     ZERO    TO  ZAN-KOHI3-D2(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI3-D2(IDX2) =
                                       MAX-KIN-KOHI3-D    -
                                       FTN-KOHI3-D2(IDX2)
                         END-IF
                         IF   FTN-KOHI3-D3(IDX2)  >=  MAX-KIN-KOHI3-D
                              MOVE     ZERO    TO  ZAN-KOHI3-D3(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI3-D3(IDX2) =
                                       MAX-KIN-KOHI3-D    -
                                       FTN-KOHI3-D3(IDX2)
                         END-IF
                       END-IF
                     END-IF
                  END-PERFORM
               END-IF
           END-IF.
      *
           IF (COMB-KOH4HKNNUM           NOT = SPACE)    AND
              (COMB-KOH4-NYUJGNKBN       =     "1")
               IF       MAX-KIN-KOHI4-D       NOT = ZERO
               AND      MAX-KIN-KOHI4-M           = ZERO
                  PERFORM    VARYING   IDX2   FROM   1   BY   1
                              UNTIL    IDX2     >    31
                     IF   ( LNK-SYU-DAY(IDX2)          =  "1" OR "2" )
                     AND  ( LNK-SDAY-HKNTEN(IDX2)      NOT =  ZERO   )
                       IF   MAX-KAI-KOHI4-M       NOT = ZERO
                          IF  KOHI4-TBL(IDX2)  NOT = ZERO
                             MOVE     SPACE    TO    FLG-NG
                             MOVE      9       TO    KOHI4-TBL(IDX2)
                          ELSE
                             IF   KOHI4-TBL(32)  >=  MAX-KAI-KOHI4-M
                               MOVE   "1"      TO    FLG-NG
                             ELSE
                               MOVE   SPACE    TO    FLG-NG
                               MOVE    9       TO    KOHI4-TBL(IDX2)
                               ADD     1       TO    KOHI4-TBL(32)
                             END-IF
                          END-IF
                       ELSE
                          MOVE        SPACE    TO    FLG-NG
                          MOVE         9       TO    KOHI4-TBL(IDX2)
                       END-IF
                       IF     FLG-NG   =    SPACE  
                         MOVE        1         TO    WK-DAYINFFLG(4)
                         IF   FTN-KOHI4-D(IDX2)     >=  MAX-KIN-KOHI4-D
                              MOVE     ZERO    TO  ZAN-KOHI4-D(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI4-D(IDX2) =
                                       MAX-KIN-KOHI4-D   -
                                       FTN-KOHI4-D(IDX2)
                         END-IF
                         IF   FTN-KOHI4-D2(IDX2)  >=  MAX-KIN-KOHI4-D
                              MOVE     ZERO    TO  ZAN-KOHI4-D2(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI4-D2(IDX2) =
                                       MAX-KIN-KOHI4-D    -
                                       FTN-KOHI4-D2(IDX2)
                         END-IF
                         IF   FTN-KOHI4-D3(IDX2)  >=  MAX-KIN-KOHI4-D
                              MOVE     ZERO    TO  ZAN-KOHI4-D3(IDX2)
                         ELSE
                              COMPUTE  ZAN-KOHI4-D3(IDX2) =
                                       MAX-KIN-KOHI4-D    -
                                       FTN-KOHI4-D3(IDX2)
                         END-IF
                       END-IF
                     END-IF
                  END-PERFORM
               END-IF
           END-IF.
      *
       2301-DAY-ZAN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¼çÊÝ¸±ÉéÃ´¶â³Û¤Î¸øÈñ¸ÂÅÙ³ÛÂÐ±þÍÑ¡¡»Ä¶â³Û·×»»
      ******************************************************************
       2302-SYUHEN-ZANKIN-KEISAN-SEC           SECTION.
      *
      *    »»Äê²ÄÇ½¥Õ¥é¥°¤Î½é´ü²½
           MOVE     SPACE                  TO     FLG-SANTEI1-HEN
                                                  FLG-SANTEI-SYU-HEN.
      *
      *    ²áµî¤Î´µ¼ÔÊÝ¸±ÉéÃ´Îß·×³Û¤È¾å¸Â¶â³Û¤ÎÈæ³Ó
      *
           IF   COMB-KOH1HKNNUM      =    "027"
             IF       MAX-KIN-KOHI1-M        NOT =  ZERO
               IF   FTN-KOHI1-M-HEN        >=     MAX-KIN-KOHI1-M
                       MOVE     "1"       TO     FLG-SANTEI1-HEN
               END-IF
             END-IF
           END-IF.
      *
           IF         MAX-KIN-SYU-M          NOT =  ZERO
               IF  FTN-SYU-M-HEN           >=     MAX-KIN-SYU-M
                      MOVE  "1"           TO     FLG-SANTEI-SYU-HEN
               END-IF
           END-IF.
      *
      *    ÊÝ¸±¡Ê¸øÈñ¡Ë¥Æ¡¼¥Ö¥ë¤Î´µ¼ÔÉéÃ´¾å¸Â³Û¤ÈÅö·î¤Î´µ¼ÔÉéÃ´Îß·×³Û
      *    ¤«¤é»»Äê²ÄÇ½»Ä³Û¤ò»»½Ð¤¹¤ë
      *
           IF  FLG-SANTEI1-HEN           =     "1"
               MOVE     ZERO             TO    ZAN-KOHI1-HEN
           ELSE
               IF  COMB-KOH1HKNNUM           =   "027"
                   IF       MAX-KIN-KOHI1-M       NOT = ZERO
                       COMPUTE  ZAN-KOHI1-HEN    =   MAX-KIN-KOHI1-M
                                                 -   FTN-KOHI1-M-HEN
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
      *
       2302-SYUHEN-ZANKIN-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¼çÊÝ¸±ÉéÃ´¶â³Û¤Î±ßÃ±°Ì¡¡»Ä¶â³Û·×»»
      ******************************************************************
       2303-SYUENTANI-ZANKIN-KEISAN-SEC           SECTION.              
      *
      *    »»Äê²ÄÇ½¥Õ¥é¥°¤Î½é´ü²½
           MOVE     SPACE                  TO     FLG-SANTEI1-ENTANI
                                                  FLG-SANTEI-SYU-ENTANI.
      *
      *    ²áµî¤Î´µ¼ÔÊÝ¸±ÉéÃ´Îß·×³Û¤È¾å¸Â¶â³Û¤ÎÈæ³Ó
      *
           IF   COMB-KOH1HKNNUM      =    "027"
             IF       MAX-KIN-KOHI1-M        NOT =  ZERO
               IF   FTN-KOHI1-M-ENTANI        >=     MAX-KIN-KOHI1-M
                       MOVE     "1"       TO     FLG-SANTEI1-ENTANI
               END-IF
             END-IF
           END-IF.
      *
           IF         MAX-KIN-SYU-M          NOT =  ZERO
               IF  FTN-SYU-M-ENTANI           >=     MAX-KIN-SYU-M
                      MOVE  "1"           TO     FLG-SANTEI-SYU-ENTANI
               END-IF
           END-IF.
      *
      *    ÊÝ¸±¡Ê¸øÈñ¡Ë¥Æ¡¼¥Ö¥ë¤Î´µ¼ÔÉéÃ´¾å¸Â³Û¤ÈÅö·î¤Î´µ¼ÔÉéÃ´Îß·×³Û
      *    ¤«¤é»»Äê²ÄÇ½»Ä³Û¤ò»»½Ð¤¹¤ë
      *
           IF  FLG-SANTEI1-ENTANI           =     "1"
               MOVE     ZERO             TO    ZAN-KOHI1-ENTANI
           ELSE
               IF  COMB-KOH1HKNNUM           =   "027"
                   IF       MAX-KIN-KOHI1-M       NOT = ZERO
                       COMPUTE  ZAN-KOHI1-ENTANI =   MAX-KIN-KOHI1-M
                                                 -   FTN-KOHI1-M-ENTANI
                   END-IF
               END-IF
           END-IF.
      *
           IF  FLG-SANTEI-SYU-ENTANI        =     "1"
               MOVE     ZERO             TO    ZAN-SYU-M-ENTANI
           ELSE
               IF       MAX-KIN-SYU-M    NOT = ZERO
                     COMPUTE  ZAN-SYU-M-ENTANI  =   MAX-KIN-SYU-M
                                                -   FTN-SYU-M-ENTANI
               END-IF
           END-IF.
      *
       2303-SYUENTANI-ZANKIN-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¥·¥¹´É¡¦Æþ±¡²ñ·×¤«¤é¿©»öÎÅÍÜÈñÅù¤Î·×»»¤ò¹Ô¤¦            *
      ******************************************************************
       2304-SHOKUJI-KEISAN-SEC         SECTION.
      *
           INITIALIZE    WK-SHOKUJI-TAIHI-AREA 
                         WK-SHOKUDOKASAN-TABLE 
                         WK-GAIHAKU-TABLE
                         WK-ROSAITOKU-TABLE
                         WK-DAY-SKJFTN-AREA.
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£µ£°£°£°¡Ë¤«¤é¿©»ö¾ðÊó¼èÆÀ
           INITIALIZE                         SYS-5000-REC.
           MOVE    "5000"               TO    SYS-5000-KANRICD.
           MOVE    "*"                  TO    SYS-5000-KBNCD.
           MOVE    LNK-SYU-SRYYMD       TO    ORC-DBYMD.
           MOVE    SYS-5000-REC         TO    MCPDATA-REC.
           MOVE    "DBSELECT"           TO    MCP-FUNC.
           MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH.
           CALL    "ORCMCPSUB"    USING       MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC.
           IF      MCP-RC               =     ZERO
                   MOVE     "SYSKANRI-KEY"  TO    ORC-DBPATH
                   PERFORM   SYSKANRI-READ-SEC
                   IF    MCP-RC             =     ZERO
      *
                     EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *                Æþ±¡¿©»öÎÅÍÜ¡Ê£±¡Ë
                       WHEN         "197000110"
                          MOVE     1         TO   WK-SHOKUJIRYOYO
                          EVALUATE  SYS-5000-SHOKUKBN(1)
                            WHEN     "1"
                              MOVE  ZERO     TO   WK-TOKUKANRIKASAN
                            WHEN     "2"
                              MOVE    1      TO   WK-TOKUKANRIKASAN
                          END-EVALUATE
      *                Æþ±¡¿©»öÎÅÍÜ¡Ê£²¡Ë
                       WHEN         "197000710"
                          MOVE     2         TO   WK-SHOKUJIRYOYO
                     END-EVALUATE
      *
                     MOVE   SYS-5000-BRMSGK-TAXKBN
                                             TO   WK-BRMSGK-TAXKBN
      *
                   END-IF 
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
      *
      *    MOVE     ZERO                TO  WK-NISSU1
      *                                     WK-NISSU2.
      *
      *    INITIALIZE                   STS-AREA-DAY.
      *    INITIALIZE                   LNK-DAY5-AREA.
      *    MOVE    "51"                 TO  LNK-DAY5-IRAI.
      *    MOVE    LNK-S02-SKYSTYMD     TO  LNK-DAY5-START.
      *    MOVE    LNK-SYU-SRYYMD       TO  LNK-DAY5-END.
      *    CALL    "ORCSDAY"            USING   STS-AREA-DAY
      *                                         LNK-DAY5-AREA.
      *    IF      ( STS-DAY-RC1    =   ZERO )
      *        MOVE    LNK-DAY5-NISSU1  TO  WK-NISSU1
      *        MOVE    LNK-DAY5-NISSU2  TO  WK-NISSU2
      *    END-IF.
      *
      *
           INITIALIZE                            NYUINACCT-REC.
           MOVE    LNK-SYU-HOSPID        TO      NACCT-HOSPID.
           MOVE    LNK-SYU-NYUGAIKBN     TO      NACCT-NYUGAIKBN.
           MOVE    LNK-SYU-PTID          TO      NACCT-PTID.
      ***  MOVE    "00"                  TO      NACCT-SRYKA.
           MOVE    LNK-SYU-SRYYMD(1:6)   TO      NACCT-SRYYM.
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC.
           MOVE    "DBSELECT"            TO      MCP-FUNC.
           MOVE    "NYUINACCT-KEY7"      TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC.
           IF      MCP-RC             =       ZERO
      *
               MOVE    "NYUINACCT-KEY7"  TO      ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-SRYCDTOTAL    =    197000570   
                             PERFORM        23041-SHOKUDOKASAN-TAIHI-SEC
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN     =    "2"  
                             PERFORM        23042-GAIHAKU-TAIHI-SEC
                 END-IF
      *
                 MOVE          "NYUINACCT-KEY7"  TO   ORC-DBPATH
                 PERFORM        NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY7"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC.
      *
      *
           INITIALIZE                            SRYACCT-REC.
           MOVE    LNK-SYU-HOSPID        TO      ACCT-HOSPID.
           MOVE    LNK-SYU-NYUGAIKBN     TO      ACCT-NYUGAIKBN.
           MOVE    LNK-SYU-PTID          TO      ACCT-PTID.
           MOVE    LNK-SYU-SRYYMD(1:6)   TO      ACCT-SRYYM.
           MOVE    SRYACCT-REC           TO      MCPDATA-REC.
           MOVE    "DBSELECT"            TO      MCP-FUNC.
           MOVE    "SRYACCT-KEY33"       TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC.
           IF      MCP-RC             =       ZERO
      *
               MOVE    "SRYACCT-KEY33"   TO      ORC-DBPATH
               PERFORM  SRYACCT-READ-SEC
               PERFORM  UNTIL  (FLG-SRYACCT    =    "1")
      *
                 IF     LNK-SYU-HKNCOMBINUM  =    ACCT-HKNCOMBI
                   IF     ACCT-SRYCDTOTAL    =    197001210  OR
                                                  197000470
                             PERFORM        23043-ROSAITOKU-TAIHI-SEC
                   END-IF
                 END-IF
      *
                 MOVE     "SRYACCT-KEY33"  TO   ORC-DBPATH
                 PERFORM   SRYACCT-READ-SEC
               END-PERFORM
      *
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC.
           MOVE    "SRYACCT-KEY33"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC.
      *
      *
           IF    WK-SHOKUJIRYOYO   NOT =  ZERO
             INITIALIZE                            NYUINACCT-REC
             MOVE    LNK-SYU-HOSPID        TO      NACCT-HOSPID
             MOVE    LNK-SYU-NYUGAIKBN     TO      NACCT-NYUGAIKBN
             MOVE    LNK-SYU-PTID          TO      NACCT-PTID
      ***    MOVE    "00"                  TO      NACCT-SRYKA
             MOVE    LNK-SYU-SRYYMD(1:6)   TO      NACCT-SRYYM
             MOVE    NYUINACCT-REC         TO      MCPDATA-REC
             MOVE    "DBSELECT"            TO      MCP-FUNC
             MOVE    "NYUINACCT-KEY7"      TO      ORC-DBPATH
             CALL    "ORCMCPSUB"           USING   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
             IF      MCP-RC             =       ZERO
      *
                 MOVE    "NYUINACCT-KEY7"  TO      ORC-DBPATH
                 PERFORM  NYUINACCT-READ-SEC
                 PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                   IF     NACCT-SRYCDTOTAL    =    099999913   
                               PERFORM        23044-SHOKUJI-COUNT-SEC
                   END-IF
      * 
                   MOVE          "NYUINACCT-KEY7"  TO   ORC-DBPATH
                   PERFORM        NYUINACCT-READ-SEC
                 END-PERFORM
      *
             END-IF
      *
             MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
             MOVE    "NYUINACCT-KEY7"        TO  ORC-DBPATH
             CALL    "ORCMCPSUB"             USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           END-IF.
      *
           IF   LNK-SYU-SHOKUJI1-NISSU      =  ZERO
           AND  LNK-SYU-SHOKUJI2-NISSU  NOT =  ZERO
      *
             MOVE  LNK-SYU-SHOKUJI2-NISSU  TO  LNK-SYU-SHOKUJI1-NISSU
             MOVE  LNK-SYU-SHOKUJI2        TO  LNK-SYU-SHOKUJI1
      *
             MOVE  ZERO                    TO  LNK-SYU-SHOKUJI2-NISSU
             MOVE  ZERO                    TO  LNK-SYU-SHOKUJI2
      *
           END-IF.
      *
       2304-SHOKUJI-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¿©Æ²²Ã»»ÂàÈò                                            *
      ******************************************************************
       23041-SHOKUDOKASAN-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO 
                    MOVE   NACCT-DAY(IDY)  TO   WK-SHOKUDOKASAN(IDY) 
             END-IF
      *
           END-PERFORM.
      *
       23041-SHOKUDOKASAN-TAIHI-EXT.
           EXIT.
      *
      ******************************************************************
      *        ³°ÇñÂàÈò                                            *
      ******************************************************************
       23042-GAIHAKU-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO 
                    MOVE   NACCT-DAY(IDY)  TO   WK-GAIHAKU(IDY) 
             END-IF
      *
           END-PERFORM.
      *
       23042-GAIHAKU-TAIHI-EXT.
           EXIT.
      *
      ******************************************************************
      *        Ï«ºÒÆþ±¡Ãæ¤ÎÆÃÊÌ¿©²Ã»»ÂàÈò                              *
      ******************************************************************
       23043-ROSAITOKU-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
      *
             IF   ACCT-SRYKBN   =   "95"  
                IF       ACCT-DAY(1 IDY)   NOT =  ZERO 
                    MOVE   ACCT-DAY(1 IDY)  TO   WK-ROSAITOKU95(IDY) 
                END-IF
             END-IF
      *
             IF   ACCT-SRYKBN   =   "96"  
                IF       ACCT-DAY(1 IDY)   NOT =  ZERO 
                    MOVE   ACCT-DAY(1 IDY)  TO   WK-ROSAITOKU96(IDY) 
                END-IF
             END-IF
      *
             IF   ACCT-SRYKBN   =   "97"
                IF       ACCT-DAY(1 IDY)   NOT =  ZERO 
                    MOVE   ACCT-DAY(1 IDY)  TO   WK-ROSAITOKU97(IDY) 
                END-IF
             END-IF
      *
           END-PERFORM.
      *
       23043-ROSAITOKU-TAIHI-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¿©»öÎÅÍÜÈñÅù¤Î·×»»                                       *
      ******************************************************************
       23044-SHOKUJI-COUNT-SEC         SECTION.
      *
      *    ADD        WK-NISSU1   TO    WK-SEIKYUKIKAN-NISSU. 
      *
           MOVE       LNK-SYU-SKYSTYMD(7:2)  TO   IDX-ST.
           MOVE       LNK-SYU-SKYEDYMD(7:2)  TO   IDX-ED.
      *
           PERFORM    VARYING   IDY   FROM   IDX-ST   BY   1
                                    UNTIL    IDY     >    IDX-ED
      *
      *      COMPUTE  WK-SEIKYUKIKAN-NISSU  =  WK-SEIKYUKIKAN-NISSU 
      *                                     +  1
      *
             MOVE     ZERO     TO      WK-SHOKUJI-JIHI
      *
             IF       ( NACCT-DAY(IDY)   NOT =  ZERO )
             AND      ( LNK-SYU-DAY(IDY)     =  "1"  )
             AND      ( WK-GAIHAKU(IDY)      =  ZERO  OR  4 )
      *
              IF  WK-SHOKUDOKASAN(IDY)  =  ZERO
      *
                EVALUATE   NACCT-DAY(IDY)
                  WHEN  1
                      EVALUATE   WK-SHOKUJIRYOYO
                        WHEN  1
      *
                          ADD       1    TO   WK-SHOKUJI-NISSU
                                              LNK-SYU-SHOKUJI1-NISSU
      *                   Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                          MOVE  "197000110"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI1
                          END-IF
      *                   ÆÃÊÌ´ÉÍý²Ã»»
                          IF    WK-TOKUKANRIKASAN  =  1
                            MOVE  "197000270"    TO   WK-SRYCD
                            PERFORM    TENSU-TBL-SEC
                            IF       FLG-TENSU      =    ZERO
                               ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                      WK-SHOKUJI-JIHI
                               ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI1
                            END-IF
                          END-IF
                        WHEN  2
      *
                          ADD       1    TO   WK-SHOKUJI-NISSU
                                              LNK-SYU-SHOKUJI1-NISSU
      *                   Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                          MOVE  "197000710"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI1
                          END-IF
                      END-EVALUATE
                  WHEN  2
      *
                          ADD       1    TO   WK-SHOKUJI-NISSU
                                              LNK-SYU-SHOKUJI1-NISSU
                                              LNK-SYU-SHOKUJI3-NISSU
      *                   Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                          MOVE  "197000110"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI1
                          END-IF
      *                   ÆÃÊÌ´ÉÍý²Ã»»
                          IF    WK-TOKUKANRIKASAN  =  1
                            MOVE  "197000270"    TO   WK-SRYCD
                            PERFORM    TENSU-TBL-SEC
                            IF       FLG-TENSU      =    ZERO
                               ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                      WK-SHOKUJI-JIHI
                               ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI1
                            END-IF
                          END-IF
      *                   ÆÃÊÌ¿©²Ã»»
                          MOVE  "197000470"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI3
                          END-IF
                  WHEN  3
      *
                          ADD       1    TO   WK-SHOKUJI-NISSU
                                              LNK-SYU-SHOKUJI1-NISSU
                                              LNK-SYU-SHOKUJI4-NISSU
      *                   Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                          MOVE  "197000110"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI1
                          END-IF
      *                   ÆÃÊÌ´ÉÍý²Ã»»
                          IF    WK-TOKUKANRIKASAN  =  1
                            MOVE  "197000270"    TO   WK-SRYCD
                            PERFORM    TENSU-TBL-SEC
                            IF       FLG-TENSU      =    ZERO
                               ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                      WK-SHOKUJI-JIHI
                               ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI1
                            END-IF
                          END-IF
      *                   Ê£¿ô¸¥Î©²Ã»»
                          MOVE  "197000670"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI4
                          END-IF
                  WHEN  4
      *
                          ADD       1    TO   WK-SHOKUJI-NISSU
                                              LNK-SYU-SHOKUJI1-NISSU
                                              LNK-SYU-SHOKUJI3-NISSU
                                              LNK-SYU-SHOKUJI4-NISSU
      *                   Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                          MOVE  "197000110"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI1
                          END-IF
      *                   ÆÃÊÌ´ÉÍý²Ã»»
                          IF    WK-TOKUKANRIKASAN  =  1
                            MOVE  "197000270"    TO   WK-SRYCD
                            PERFORM    TENSU-TBL-SEC
                            IF       FLG-TENSU      =    ZERO
                               ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                      WK-SHOKUJI-JIHI
                               ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI1
                            END-IF
                          END-IF
      *                   ÆÃÊÌ¿©²Ã»»
                          MOVE  "197000470"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI3
                          END-IF
      *                   Ê£¿ô¸¥Î©²Ã»»
                          MOVE  "197000670"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI4
                          END-IF
                END-EVALUATE
      *
              ELSE
      *
                EVALUATE   NACCT-DAY(IDY)
                  WHEN  1
                      EVALUATE   WK-SHOKUJIRYOYO
                        WHEN  1
      *
                          ADD       1    TO   WK-SHOKUJI-NISSU
                                              LNK-SYU-SHOKUJI2-NISSU
      *                   Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                          MOVE  "197000110"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI2
                          END-IF
      *                   ÆÃÊÌ´ÉÍý²Ã»»
                          IF    WK-TOKUKANRIKASAN  =  1
                            MOVE  "197000270"    TO   WK-SRYCD
                            PERFORM    TENSU-TBL-SEC
                            IF       FLG-TENSU      =    ZERO
                               ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                      WK-SHOKUJI-JIHI
                               ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI2
                            END-IF
                          END-IF
      *                   ¿©Æ²²Ã»»
                          MOVE  "197000570"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI2
                          END-IF
                        WHEN  2
      *
                          ADD       1    TO   WK-SHOKUJI-NISSU
                                              LNK-SYU-SHOKUJI1-NISSU
      *                   Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                          MOVE  "197000710"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI1
                          END-IF
                      END-EVALUATE
                  WHEN  2
      *
                          ADD       1    TO   WK-SHOKUJI-NISSU
                                              LNK-SYU-SHOKUJI2-NISSU
                                              LNK-SYU-SHOKUJI3-NISSU
      *                   Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                          MOVE  "197000110"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI2
                          END-IF
      *                   ÆÃÊÌ´ÉÍý²Ã»»
                          IF    WK-TOKUKANRIKASAN  =  1
                            MOVE  "197000270"    TO   WK-SRYCD
                            PERFORM    TENSU-TBL-SEC
                            IF       FLG-TENSU      =    ZERO
                               ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                      WK-SHOKUJI-JIHI
                               ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI2
                            END-IF
                          END-IF
      *                   ¿©Æ²²Ã»»
                          MOVE  "197000570"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI2
                          END-IF
      *                   ÆÃÊÌ¿©²Ã»»
                          MOVE  "197000470"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI3
                          END-IF
                  WHEN  3
      *
                          ADD       1    TO   WK-SHOKUJI-NISSU
                                              LNK-SYU-SHOKUJI2-NISSU
                                              LNK-SYU-SHOKUJI4-NISSU
      *                   Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                          MOVE  "197000110"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI2
                          END-IF
      *                   ÆÃÊÌ´ÉÍý²Ã»»
                          IF    WK-TOKUKANRIKASAN  =  1
                            MOVE  "197000270"    TO   WK-SRYCD
                            PERFORM    TENSU-TBL-SEC
                            IF       FLG-TENSU      =    ZERO
                               ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                      WK-SHOKUJI-JIHI
                               ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI2
                            END-IF
                          END-IF
      *                   ¿©Æ²²Ã»»
                          MOVE  "197000570"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI2
                          END-IF
      *                   Ê£¿ô¸¥Î©²Ã»»
                          MOVE  "197000670"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI4
                          END-IF
                  WHEN  4
      *
                          ADD       1    TO   WK-SHOKUJI-NISSU
                                              LNK-SYU-SHOKUJI2-NISSU
                                              LNK-SYU-SHOKUJI3-NISSU
                                              LNK-SYU-SHOKUJI4-NISSU
      *                   Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                          MOVE  "197000110"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI2
                          END-IF
      *                   ÆÃÊÌ´ÉÍý²Ã»»
                          IF    WK-TOKUKANRIKASAN  =  1
                            MOVE  "197000270"    TO   WK-SRYCD
                            PERFORM    TENSU-TBL-SEC
                            IF       FLG-TENSU      =    ZERO
                               ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                      WK-SHOKUJI-JIHI
                               ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI2
                            END-IF
                          END-IF
      *                   ¿©Æ²²Ã»»
                          MOVE  "197000570"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             ADD     WK-KEISAN TO   LNK-SYU-SHOKUJI2
                          END-IF
      *                   ÆÃÊÌ¿©²Ã»»
                          MOVE  "197000470"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI3
                          END-IF
      *                   Ê£¿ô¸¥Î©²Ã»»
                          MOVE  "197000670"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     WK-KEISAN TO   WK-SHOKUJI-RYOYOHI
                                                    WK-SHOKUJI-JIHI
                             MOVE    WK-KEISAN TO   LNK-SYU-SHOKUJI4
                          END-IF
                END-EVALUATE
      *
              END-IF
      *
              IF   ( COMB-SYU-HKNKOHSBTKBN   =   "8"            )
              OR   ( COMB-HKNNUM             =  "971"  OR  "973")
                ADD   WK-SHOKUJI-JIHI    TO   WK-SHOKUJI-FTN
              ELSE
                EVALUATE   FLG-SKJ
                  WHEN  0
      *                   É¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                          MOVE  "197000810"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     TNS-TEN   TO   WK-SHOKUJI-FTN
                             MOVE    TNS-TEN   TO   WK-DAY-SKJFTN(IDY)
                          END-IF
                  WHEN  1
      *                   É¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                          MOVE  "197001110"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     TNS-TEN   TO   WK-SHOKUJI-FTN
                             MOVE    TNS-TEN   TO   WK-DAY-SKJFTN(IDY)
                          END-IF
                  WHEN  2
      *             COMPUTE   WK-TSUSAN-NISSU  =  
      *                                 LNK-S02-NYUIN-TSUSAN-NISSU  +
      *                                 WK-SEIKYUKIKAN-NISSU
      *             IF        WK-TSUSAN-NISSU  <=  90
      *                   ADD     650    TO   WK-SHOKUJI-FTN
      *             ELSE
      *                   ADD     500    TO   WK-SHOKUJI-FTN
      *             END-IF
                    IF    WRK-SKJGNSTYMD-S   NOT  =   SPACE
                       MOVE   LNK-SYU-SRYYMD(1:6) 
                                         TO  WRK-SKJGNSTYYMM2
                       MOVE   IDY        TO  WRK-SKJGNSTDD2
                       IF   WRK-SKJGNSTYMD-S  <=  WRK-SKJGNSTYMD2
                       AND  WRK-SKJGNSTYMD-E  >=  WRK-SKJGNSTYMD2 
      *                   É¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                          MOVE  "197001010"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     TNS-TEN   TO   WK-SHOKUJI-FTN
                             MOVE    TNS-TEN   TO   WK-DAY-SKJFTN(IDY)
                          END-IF
                       ELSE
      *                   É¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                          MOVE  "197000910"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     TNS-TEN   TO   WK-SHOKUJI-FTN
                             MOVE    TNS-TEN   TO   WK-DAY-SKJFTN(IDY)
                          END-IF
                       END-IF
                    ELSE
      *                   É¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                          MOVE  "197000910"    TO   WK-SRYCD
                          PERFORM    TENSU-TBL-SEC
                          IF       FLG-TENSU      =    ZERO
                             ADD     TNS-TEN   TO   WK-SHOKUJI-FTN
                             MOVE    TNS-TEN   TO   WK-DAY-SKJFTN(IDY)
                          END-IF
                    END-IF    
                END-EVALUATE
              END-IF
      *
             END-IF
      *
             IF       ( WK-ROSAITOKU95(IDY)  NOT =  ZERO       )
      **>    AND      ( LNK-SYU-DAY(IDY)         =  "1" OR "2" )
             AND      ( LNK-SYU-DAY(IDY)         =  "2"        )
      *
      *         É¸½àÉéÃ´³Û¡ÊÏ«ºÒÆþ±¡Ãæ¡Ë
                MOVE  "197001210"    TO   WK-SRYCD
                PERFORM    TENSU-TBL-SEC
                IF       FLG-TENSU      =    ZERO
                   ADD     TNS-TEN   TO   WK-SHOKUJI-RYOYOHI95
                                          WK-SHOKUJI-FTN95
                END-IF
      *
             END-IF
      *
             IF       ( WK-ROSAITOKU96(IDY)  NOT =  ZERO       )
      **>    AND      ( LNK-SYU-DAY(IDY)         =  "1" OR "2" )
             AND      ( LNK-SYU-DAY(IDY)         =  "2"        )
      *
      *         É¸½àÉéÃ´³Û¡ÊÏ«ºÒÆþ±¡Ãæ¡Ë
                MOVE  "197001210"    TO   WK-SRYCD
                PERFORM    TENSU-TBL-SEC
                IF       FLG-TENSU      =    ZERO
                   ADD     TNS-TEN   TO   WK-SHOKUJI-RYOYOHI96
                                          WK-SHOKUJI-FTN96
                END-IF
      *
             END-IF
      *
             MOVE     ZERO     TO      WK-SHOKUJI-JIHI
      *
             IF       ( WK-ROSAITOKU97(IDY)  NOT =  ZERO       )
      **>    AND      ( LNK-SYU-DAY(IDY)         =  "1" OR "2" )
             AND      ( LNK-SYU-DAY(IDY)         =  "2"        )
      *
                ADD       1    TO   WK-SHOKUJI-NISSU
                                    LNK-SYU-SHOKUJI3-NISSU
      *         É¸½àÉéÃ´³Û¡ÊÏ«ºÒÆþ±¡Ãæ¡Ë
                MOVE  "197001210"    TO   WK-SRYCD
                PERFORM    TENSU-TBL-SEC
                IF       FLG-TENSU      =    ZERO
                   ADD     TNS-TEN   TO   WK-SHOKUJI-RYOYOHI
                                          WK-SHOKUJI-JIHI
                   MOVE    TNS-TEN   TO   LNK-SYU-SHOKUJI3
                END-IF
      *
                IF  COMB-SYU-HKNKOHSBTKBN   =   "8"
                  ADD   WK-SHOKUJI-JIHI    TO   WK-SHOKUJI-FTN
                END-IF
      *
             END-IF
      *
           END-PERFORM.
      *
       23044-SHOKUJI-COUNT-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¼çÊÝ¸±¤Î´µ¼ÔÉéÃ´¶â³Û·×»»                                *
      ******************************************************************
       2305-SYUHOKEN-KEISAN-SEC           SECTION.
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
                ((HKN-TEKSTYMD(1:6)     <=  LNK-SYU-SRYYMD(1:6)  ) AND
                 (LNK-SYU-SRYYMD(1:6)   <=  HKN-TEKEDYMD(1:6))     AND
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
                   WHEN   "4"
                          MOVE   ZERO      TO   HKN-HON-NYUKFTNRATE
                                                HKN-KZK-NYUKFTNRATE
                                                HKN-TSY-NYUKFTNRATE
                                                HKN-TNK-NYUKFTNRATE
                   WHEN   "1"
                   WHEN   "5"
                          MOVE   10        TO   HKN-HON-NYUKFTNRATE
                                                HKN-KZK-NYUKFTNRATE
                                                HKN-TSY-NYUKFTNRATE
                                                HKN-TNK-NYUKFTNRATE
                   WHEN   "2"
                   WHEN   "6"
                          MOVE   20        TO   HKN-HON-NYUKFTNRATE
                                                HKN-KZK-NYUKFTNRATE
                                                HKN-TSY-NYUKFTNRATE
                                                HKN-TNK-NYUKFTNRATE
                   WHEN   "3"
                          MOVE   30        TO   HKN-HON-NYUKFTNRATE
                                                HKN-KZK-NYUKFTNRATE
                                                HKN-TSY-NYUKFTNRATE
                                                HKN-TNK-NYUKFTNRATE
               END-EVALUATE
      *    ·òÊÝ¤Î¹âÎð¼Ô¤Ë¤Ä¤¤¤Æ¤âÊä½õ¶èÊ¬¤è¤êÉéÃ´³ä¹ç¥»¥Ã¥È
           ELSE
               EVALUATE   COMB-HOJOKBN
                   WHEN   "9"
                          MOVE   10        TO   HKN-HON-NYUKFTNRATE
                                                HKN-KZK-NYUKFTNRATE
                                                HKN-TSY-NYUKFTNRATE
                                                HKN-TNK-NYUKFTNRATE
                   WHEN   "8"
                          MOVE   20        TO   HKN-HON-NYUKFTNRATE
                                                HKN-KZK-NYUKFTNRATE
                                                HKN-TSY-NYUKFTNRATE
                                                HKN-TNK-NYUKFTNRATE
               END-EVALUATE
           END-IF.
      *
      *    Ç¯Îð£³ºÐÌ¤Ëþ
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
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      WRK-NENREI         <     3
               IF   COMB-HKNNUM       =     "060"
                   IF   COMB-HOJOKBN  =     "0"  OR  "1"
                                       OR   "4"  OR  "5"
                       CONTINUE
                   ELSE
                       MOVE   20      TO   HKN-HON-NYUKFTNRATE
                                           HKN-KZK-NYUKFTNRATE
                                           HKN-TSY-NYUKFTNRATE
                                           HKN-TNK-NYUKFTNRATE
                   END-IF
               ELSE
                   IF    HKN-HKNKOHSBTKBN  =  "1" 
                       MOVE   20      TO   HKN-HON-NYUKFTNRATE
                                           HKN-KZK-NYUKFTNRATE
                                           HKN-TSY-NYUKFTNRATE
                                           HKN-TNK-NYUKFTNRATE
                   END-IF 
               END-IF
           END-IF.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥Æ¡¼¥Ö¥ë¤ÎÊä½õ¶èÊ¬¤«¤é´µ¼Ô¤ÎÉéÃ´³ä¹ç¤òÊÝ¸±ÈÖ¹æ
      *    ¥Æ¡¼¥Ö¥ë¤ËºÆ¥»¥Ã¥È¡ÊÁ¥°÷ÊÝ¸±»þ¡Ë
           IF    COMB-HKNNUM           =    "002"
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
                       IF   LNK-S02-SENINFLG  NOT =    "1"
                          MOVE   ZERO      TO   HKN-HON-NYUKFTNRATE
                                                HKN-KZK-NYUKFTNRATE
                                                HKN-TSY-NYUKFTNRATE
                                                HKN-TNK-NYUKFTNRATE
                       END-IF
      *
                       MOVE      "0"       TO   HKN-HON-NYUSKJRYYKBN
                                                HKN-KZK-NYUSKJRYYKBN
                                                HKN-TSY-NYUSKJRYYKBN
                                                HKN-TNK-NYUSKJRYYKBN
               END-EVALUATE
           END-IF.
      *    ÊÝ¸±ÁÈ¹ç¤»¥Æ¡¼¥Ö¥ë¤ÎÊä½õ¶èÊ¬¤«¤é´µ¼Ô¤ÎÉéÃ´³ä¹ç¤òÊÝ¸±ÈÖ¹æ
      *    ¥Æ¡¼¥Ö¥ë¤ËºÆ¥»¥Ã¥È¡Ê¶¦ºÑÁÈ¹çÊÝ¸±»þ¡Ë
           IF    COMB-HKNNUM    =    "031" OR "032" OR "033" OR "034"
               EVALUATE   COMB-HOJOKBN
                   WHEN   "2"
                   WHEN   "B"
                   WHEN   "E"
                          MOVE   ZERO      TO   HKN-HON-NYUKFTNRATE
                                                HKN-KZK-NYUKFTNRATE
                                                HKN-TSY-NYUKFTNRATE
                                                HKN-TNK-NYUKFTNRATE
      *
                          MOVE   "0"       TO   HKN-HON-NYUSKJRYYKBN
                                                HKN-KZK-NYUSKJRYYKBN
                                                HKN-TSY-NYUSKJRYYKBN
                                                HKN-TNK-NYUSKJRYYKBN
               END-EVALUATE
           END-IF.
      *
      *    ¡Ê£×£Ë¡Ý£È£Ë£Î£Ë£Â£Î¡¡¶õÇò¡§°åÊÝ¡¡£±¡§Ï«ºÒ¡¦¼«Çå¡Ë
           EVALUATE   WK-HKNKBN
             WHEN   SPACE
               IF   COMB-HONKZKKBN        =   "1"
                        IF    HKN-HON-NYUKFTNRATE     NOT =    ZERO
                              MOVE   HKN-HON-NYUKFTNRATE
                                           TO   WK-DAY-NYUKFTNRATE
                              MOVE   HKN-HON-NYUKFTNRATE
                                           TO   WK-KOH-FTNRATE
                              COMPUTE   WK-FUTANWARI  =
                                        HKN-HON-NYUKFTNRATE  /  100
                              COMPUTE   LNK-SYU-SYUCOMPFTN  =
                                        WK-ZENGAKU   *  WK-FUTANWARI
                              COMPUTE   LNK-SYU-SYUCOMPFTN-ENTANI  =    
                                        WK-ZENGAKU   *  WK-FUTANWARI    
                              IF   LNK-S02-SENINFLG    =  "1"           
                                IF LNK-SYU-SYUCOMPFTN  >  200           
                                   MOVE   200  TO  LNK-SYU-SYUCOMPFTN   
                                            LNK-SYU-SYUCOMPFTN-ENTANI   
                                END-IF                                  
                              END-IF                                    
                              MOVE      LNK-SYU-SYUCOMPFTN
                                                 TO WK-TOKUTEI-COMPFTN2
                              COMPUTE   LNK-SYU-SYUCOMPFTN =
                                        LNK-SYU-SYUCOMPFTN  +  5
                              COMPUTE   WK-FTN      =
                                        LNK-SYU-SYUCOMPFTN  /  10
                              COMPUTE   LNK-SYU-SYUCOMPFTN =
                                        WK-FTN   *  10
                        END-IF
      *
                        IF   LNK-S02-KOUREI-NYUIN-KBN   =   "1"
                            IF  FLG-SYUHEN   =   "1"
                              IF  ZAN-SYU-M-HEN  <  LNK-SYU-SYUCOMPFTN 
                                MOVE ZAN-SYU-M-HEN
                                                 TO  WK-HEN-SYUCOMPFTN 
                              ELSE                                     
                                MOVE LNK-SYU-SYUCOMPFTN                
                                                 TO  WK-HEN-SYUCOMPFTN 
                              END-IF
                            END-IF
                            IF  ZAN-SYU-M     <     WK-TOKUTEI-COMPFTN2 
                                MOVE ZAN-SYU-M   TO  LNK-SYU-SYUCOMPFTN
                                                     WK-TOKUTEI-COMPFTN 
                            ELSE                                        
                                MOVE WK-TOKUTEI-COMPFTN2                
                                                 TO  WK-TOKUTEI-COMPFTN 
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
                                  LNK-SYU-SYUCOMPFTN     
                        MOVE  HKN-HON-NYUSKJRYYKBN
                                             TO   LNK-SYU-SYUSKJ-FTNKBN
                        MOVE  WK-SHOKUJI-NISSU
                                             TO   LNK-SYU-SYUSKJ-NISSU
                        MOVE  WK-SHOKUJI-RYOYOHI
                                             TO   LNK-SYU-SYUSKJ-RYOYOHI
                        IF    HKN-HON-NYUSKJRYYKBN  NOT =  "0"
                            IF    HKN-HON-NYUSKJRYYKBN  NOT =  "3"
                              MOVE  WK-SHOKUJI-FTN
                                             TO   LNK-SYU-SYUSKJ-FTN
                            END-IF
                            MOVE  WK-SHOKUJI-FTN
                                            TO   LNK-SYU-SYUSKJ-FTN-RECE
                        END-IF
               ELSE
                        IF    HKN-KZK-NYUKFTNRATE     NOT =    ZERO
                              MOVE   HKN-KZK-NYUKFTNRATE
                                           TO   WK-DAY-NYUKFTNRATE
                              MOVE   HKN-KZK-NYUKFTNRATE
                                           TO   WK-KOH-FTNRATE
                              COMPUTE   WK-FUTANWARI  =
                                        HKN-KZK-NYUKFTNRATE  /  100
                              COMPUTE   LNK-SYU-SYUCOMPFTN  =
                                        WK-ZENGAKU   *  WK-FUTANWARI
                              COMPUTE   LNK-SYU-SYUCOMPFTN-ENTANI  =    
                                        WK-ZENGAKU   *  WK-FUTANWARI    
                              MOVE      LNK-SYU-SYUCOMPFTN
                                                 TO WK-TOKUTEI-COMPFTN2
                              COMPUTE   LNK-SYU-SYUCOMPFTN =
                                        LNK-SYU-SYUCOMPFTN  +  5
                              COMPUTE   WK-FTN      =
                                        LNK-SYU-SYUCOMPFTN  /  10
                              COMPUTE   LNK-SYU-SYUCOMPFTN =
                                        WK-FTN   *  10
                        END-IF
      *
                        IF   LNK-S02-KOUREI-NYUIN-KBN   =   "1"
                            IF  FLG-SYUHEN   =   "1"
                              IF  ZAN-SYU-M-HEN  <  LNK-SYU-SYUCOMPFTN 
                                MOVE ZAN-SYU-M-HEN
                                                 TO  WK-HEN-SYUCOMPFTN 
                              ELSE                                     
                                MOVE LNK-SYU-SYUCOMPFTN                
                                                 TO  WK-HEN-SYUCOMPFTN 
                              END-IF
                            END-IF
                            IF  ZAN-SYU-M     <     WK-TOKUTEI-COMPFTN2 
                                MOVE ZAN-SYU-M   TO  LNK-SYU-SYUCOMPFTN
                                                     WK-TOKUTEI-COMPFTN 
                            ELSE                                        
                                MOVE WK-TOKUTEI-COMPFTN2                
                                                 TO  WK-TOKUTEI-COMPFTN 
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
                                  LNK-SYU-SYUCOMPFTN     
                        MOVE  HKN-KZK-NYUSKJRYYKBN
                                             TO   LNK-SYU-SYUSKJ-FTNKBN
                        MOVE  WK-SHOKUJI-NISSU
                                             TO   LNK-SYU-SYUSKJ-NISSU
                        MOVE  WK-SHOKUJI-RYOYOHI
                                             TO   LNK-SYU-SYUSKJ-RYOYOHI
                        IF    HKN-KZK-NYUSKJRYYKBN  NOT =  "0"
                            IF    HKN-KZK-NYUSKJRYYKBN  NOT =  "3"
                              MOVE  WK-SHOKUJI-FTN
                                             TO   LNK-SYU-SYUSKJ-FTN
                            END-IF
                            MOVE  WK-SHOKUJI-FTN
                                            TO   LNK-SYU-SYUSKJ-FTN-RECE
                        END-IF
               END-IF
             WHEN   "1"
               IF    WK-4001-KSNRATE(1:2)   =    SPACE
                      MOVE     100        TO   WK-KSNRATE9
                      COMPUTE  WK-KSNRATE   =  WK-KSNRATE9  /  100
               ELSE
                      COMPUTE  WK-KSNRATE9  =  WK-4001-KSNRATE
                                            +  100 
                      COMPUTE  WK-KSNRATE   =  WK-KSNRATE9  /  100
               END-IF  
               IF     LNK-SYU-SYUHKNNUM       =  "971" OR "973"
                 IF   LNK-SYU-PTFTNRATE       =   100
                   MOVE     100      TO   HKN-HON-NYUKFTNRATE
                                          HKN-KZK-NYUKFTNRATE
                                          HKN-TSY-NYUKFTNRATE
                                          HKN-TNK-NYUKFTNRATE
                   MOVE     "1"      TO   HKN-HON-NYUSKJRYYKBN
                                          HKN-KZK-NYUSKJRYYKBN
                                          HKN-TSY-NYUSKJRYYKBN
                                          HKN-TNK-NYUSKJRYYKBN
                 ELSE
                   MOVE     ZERO     TO   HKN-HON-NYUKFTNRATE
                                          HKN-KZK-NYUKFTNRATE
                                          HKN-TSY-NYUKFTNRATE
                                          HKN-TNK-NYUKFTNRATE
                   MOVE     "0"      TO   HKN-HON-NYUSKJRYYKBN
                                          HKN-KZK-NYUSKJRYYKBN
                                          HKN-TSY-NYUSKJRYYKBN
                                          HKN-TNK-NYUSKJRYYKBN
                 END-IF
               END-IF
               IF     LNK-SYU-SYUHKNNUM       =  "971"
                 EVALUATE  WK-4001-TENKBN
                     WHEN  "1"
                         COMPUTE  WK-ZENGAKU  =  
                                 (WK-ZENGAKU  / 10 * 11.5)
                     WHEN  "2"
                         COMPUTE  WK-ZENGAKU  =  
                                 (WK-ZENGAKU  / 10 * 12.0)
                 END-EVALUATE
               ELSE
                 EVALUATE  WK-4001-TENKBN
                     WHEN  "1"
                        COMPUTE  WK-ZENGAKU  =  WK-ZENGAKU  /  10  
                        MOVE  LNK-S02-SYUTEN  TO  WK-ZENGAKU-SYU
                        COMPUTE  WK-ZENGAKU-YAK = WK-ZENGAKU 
                                                - LNK-S02-SYUTEN
                        COMPUTE  WK-ZENGAKU-SYU =
                          (WK-ZENGAKU-SYU * 11.5 * WK-KSNRATE)
                                                 + 0.5
                        COMPUTE  WK-ZENGAKU-YAK =
                          (WK-ZENGAKU-YAK * 11.5)
                                                 + 0.5
                        COMPUTE  WK-ZENGAKU     = WK-ZENGAKU-SYU
                                                + WK-ZENGAKU-YAK
                     WHEN  "2"
                        COMPUTE  WK-ZENGAKU  =  WK-ZENGAKU  /  10  
                        MOVE  LNK-S02-SYUTEN  TO  WK-ZENGAKU-SYU
                        COMPUTE  WK-ZENGAKU-YAK = WK-ZENGAKU 
                                                - LNK-S02-SYUTEN
                        COMPUTE  WK-ZENGAKU-SYU =
                          (WK-ZENGAKU-SYU * 12.0 * WK-KSNRATE)
                                                 + 0.5
                        COMPUTE  WK-ZENGAKU-YAK =
                          (WK-ZENGAKU-YAK * 12.0)
                                                 + 0.5
                        COMPUTE  WK-ZENGAKU     = WK-ZENGAKU-SYU
                                                + WK-ZENGAKU-YAK
                 END-EVALUATE
               END-IF 
      *
               COMPUTE   LNK-SYU-RSIJIBAI-SKYMONEY  =
                         WK-ZENGAKU                 +
                         WK-SHOKUJI-FTN
      *
               COMPUTE   WK-FUTANWARI  =
                         HKN-HON-NYUKFTNRATE  /  100
               COMPUTE   LNK-SYU-SYUCOMPFTN  =
                         WK-ZENGAKU   *  WK-FUTANWARI
      *         COMPUTE   LNK-SYU-SYUCOMPFTN =
      *                   LNK-SYU-SYUCOMPFTN  +  5
      *         COMPUTE   WK-FTN      =
      *                   LNK-SYU-SYUCOMPFTN  /  10
      *         COMPUTE   LNK-SYU-SYUCOMPFTN =
      *                   WK-FTN   *  10
               COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
                         LNK-SYU-SYUCOMPFTN     
               MOVE  HKN-HON-NYUSKJRYYKBN
                                        TO   LNK-SYU-SYUSKJ-FTNKBN
               MOVE  WK-SHOKUJI-NISSU
                                        TO   LNK-SYU-SYUSKJ-NISSU
               MOVE  WK-SHOKUJI-RYOYOHI
                                        TO   LNK-SYU-SYUSKJ-RYOYOHI
               IF    HKN-HON-NYUSKJRYYKBN  NOT =  "0"
                       IF    HKN-HON-NYUSKJRYYKBN  NOT =  "3"
                         MOVE  WK-SHOKUJI-FTN
                                        TO   LNK-SYU-SYUSKJ-FTN
                       END-IF
                       MOVE  WK-SHOKUJI-FTN
                                        TO   LNK-SYU-SYUSKJ-FTN-RECE
               END-IF
           END-EVALUATE.
      *
       2305-SYUHOKEN-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñ¤ÎÊÝ¸±ÈÖ¹æ¥Æ¡¼¥Ö¥ë¸¡º÷                              *
      ******************************************************************
       2306-KOH-HKNREAD-SEC           SECTION.
      *
      *    ³ºÅöÊÝ¸±ÈÖ¹æ¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
           INITIALIZE                      HKNNUM-REC.
           MOVE   COMB-HOSPID        TO    HKN-HOSPID.
           MOVE   COMB-KOHHKNNUM(KOH-IDX)
                                     TO    HKN-HKNNUM.
           IF     COMB-KOHHKNNUM(KOH-IDX)  =     "027"
                  MOVE     COMB-KOH1PAYKBN
                                     TO    WK-PAYKBN
           ELSE
                  MOVE     "00"      TO    WK-PAYKBN
           END-IF.
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
                ((HKN-TEKSTYMD(1:6)     <=  LNK-SYU-SRYYMD(1:6)  ) AND
                 (LNK-SYU-SRYYMD(1:6)   <=  HKN-TEKEDYMD(1:6))     AND
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
           MOVE       ZERO                TO   WK-NYUKFTNRATE.
           MOVE       SPACE               TO   WK-NYUSKJRYYKBN.
           MOVE       ZERO                TO   WK-NYUSKJJYOGAK.
      *
           IF  ( WK-1001-PREFNUM         =  01  OR  27  OR 34  ) AND
               ( COMB-KOHHKNNUM(KOH-IDX) = "141"               )
             EVALUATE   FLG-SANSHOU2
               WHEN   "1"
                   MOVE    HKN-HON-NYUKFTNRATE  TO   WK-NYUKFTNRATE
                   MOVE    HKN-HON-NYUSKJRYYKBN TO   WK-NYUSKJRYYKBN
                   MOVE    HKN-HON-NYUSKJJYOGAK TO   WK-NYUSKJJYOGAK
               WHEN   "2"
                   MOVE    HKN-TSY-NYUKFTNRATE  TO   WK-NYUKFTNRATE
                   MOVE    HKN-TSY-NYUSKJRYYKBN TO   WK-NYUSKJRYYKBN
                   MOVE    HKN-TSY-NYUSKJJYOGAK TO   WK-NYUSKJJYOGAK
               WHEN   "3"
                   MOVE    HKN-TNK-NYUKFTNRATE  TO   WK-NYUKFTNRATE
                   MOVE    HKN-TNK-NYUSKJRYYKBN TO   WK-NYUSKJRYYKBN
                   MOVE    HKN-TNK-NYUSKJJYOGAK TO   WK-NYUSKJJYOGAK
             END-EVALUATE
           ELSE
      ***    EVALUATE   FLG-SANSHOU2
      ***      WHEN   "1"
                   MOVE    HKN-HON-NYUKFTNRATE  TO   WK-NYUKFTNRATE
                   MOVE    HKN-HON-NYUSKJRYYKBN TO   WK-NYUSKJRYYKBN
                   MOVE    HKN-HON-NYUSKJJYOGAK TO   WK-NYUSKJJYOGAK
                   IF  (LNK-SYU-SRYYMD(1:6)      >=  "200304"      )
                   AND (COMB-KOHHKNNUM(KOH-IDX)   =  "015" OR "016")
                       MOVE    "2"              TO   WK-NYUSKJRYYKBN
                   END-IF
                   IF  (COMB-KOHHKNNUM(KOH-IDX)   =  "012"         )
                       MOVE    "3"              TO   WK-NYUSKJRYYKBN
                   END-IF
                   IF  (LNK-SYU-SRYYMD(1:6)      >=  "200310"      )
                   AND (COMB-KOHHKNNUM(KOH-IDX)   =  "051"         )
                       MOVE    "2"              TO   WK-NYUSKJRYYKBN
                   END-IF
                   IF  (LNK-SYU-SRYYMD(1:6)      >=  "200310"      )
                   AND (WK-1001-PREFNUM           =   13           )
                   AND (COMB-KOHHKNNUM(KOH-IDX)   =  "151" OR "451")
                       MOVE    "2"              TO   WK-NYUSKJRYYKBN
                   END-IF
                   IF  (LNK-SYU-SRYYMD(1:6)      >=  "200310"      )
                   AND (WK-1001-PREFNUM           =   28           )
                   AND (COMB-KOHHKNNUM(KOH-IDX)   =  "186"         )
                       MOVE    "2"              TO   WK-NYUSKJRYYKBN
                   END-IF
      ***      WHEN   "2"
      ***          MOVE    HKN-TSY-NYUKFTNRATE  TO   WK-NYUKFTNRATE
      ***          MOVE    HKN-TSY-NYUSKJRYYKBN TO   WK-NYUSKJRYYKBN
      ***          MOVE    HKN-TSY-NYUSKJJYOGAK TO   WK-NYUSKJJYOGAK
      ***          IF  (LNK-SYU-SRYYMD(1:6)      >=  "200304"      )
      ***          AND (COMB-KOHHKNNUM(KOH-IDX)   =  "015" OR "016")
      ***              MOVE    "2"              TO   WK-NYUSKJRYYKBN
      ***          END-IF
      ***          IF  (COMB-KOHHKNNUM(KOH-IDX)   =  "012"         )
      ***              MOVE    "3"              TO   WK-NYUSKJRYYKBN
      ***          END-IF
      ***          IF  (LNK-SYU-SRYYMD(1:6)      >=  "200310"      )
      ***          AND (COMB-KOHHKNNUM(KOH-IDX)   =  "051"         )
      ***              MOVE    "2"              TO   WK-NYUSKJRYYKBN
      ***          END-IF
      ***          IF  (LNK-SYU-SRYYMD(1:6)      >=  "200310"      )
      ***          AND (WK-1001-PREFNUM           =   13           )
      ***          AND (COMB-KOHHKNNUM(KOH-IDX)   =  "151" OR "451")
      ***              MOVE    "2"              TO   WK-NYUSKJRYYKBN
      ***          END-IF
      ***          IF  (LNK-SYU-SRYYMD(1:6)      >=  "200310"      )
      ***          AND (WK-1001-PREFNUM           =   28           )
      ***          AND (COMB-KOHHKNNUM(KOH-IDX)   =  "186"         )
      ***              MOVE    "2"              TO   WK-NYUSKJRYYKBN
      ***          END-IF
      ***      WHEN   "3"
      ***          MOVE    HKN-TNK-NYUKFTNRATE  TO   WK-NYUKFTNRATE
      ***          MOVE    HKN-TNK-NYUSKJRYYKBN TO   WK-NYUSKJRYYKBN
      ***          MOVE    HKN-TNK-NYUSKJJYOGAK TO   WK-NYUSKJJYOGAK
      ***          IF  (LNK-SYU-SRYYMD(1:6)      >=  "200304"      )
      ***          AND (COMB-KOHHKNNUM(KOH-IDX)   =  "015" OR "016")
      ***              MOVE    "2"              TO   WK-NYUSKJRYYKBN
      ***          END-IF
      ***          IF  (COMB-KOHHKNNUM(KOH-IDX)   =  "012"         )
      ***              MOVE    "3"              TO   WK-NYUSKJRYYKBN
      ***          END-IF
      ***          IF  (LNK-SYU-SRYYMD(1:6)      >=  "200310"      )
      ***          AND (COMB-KOHHKNNUM(KOH-IDX)   =  "051"         )
      ***              MOVE    "2"              TO   WK-NYUSKJRYYKBN
      ***          END-IF
      ***          IF  (LNK-SYU-SRYYMD(1:6)      >=  "200310"      )
      ***          AND (WK-1001-PREFNUM           =   13           )
      ***          AND (COMB-KOHHKNNUM(KOH-IDX)   =  "151" OR "451")
      ***              MOVE    "2"              TO   WK-NYUSKJRYYKBN
      ***          END-IF
      ***          IF  (LNK-SYU-SRYYMD(1:6)      >=  "200310"      )
      ***          AND (WK-1001-PREFNUM           =   28           )
      ***          AND (COMB-KOHHKNNUM(KOH-IDX)   =  "186"         )
      ***              MOVE    "2"              TO   WK-NYUSKJRYYKBN
      ***          END-IF
      ***    END-EVALUATE
           END-IF.
      *
           IF   COMB-KOHHKNNUM(KOH-IDX)  =  "027"
                   MOVE    HKN-HON-NYUKFTNRATE  TO   WK-NYUKFTNRATE
                   MOVE    HKN-HON-NYUSKJRYYKBN TO   WK-NYUSKJRYYKBN
                   MOVE    HKN-HON-NYUSKJJYOGAK TO   WK-NYUSKJJYOGAK
      *
                   MOVE    HKN-HON-NYUKFTNRATE  TO   WK-KOH-FTNRATE
                   MOVE    "1"                  TO   HKN-RECEFTNDSPKBN
           END-IF.
      *
      *    Ê¡²¬¸©¤ÎÃÏÊý¸øÈñÂÐ±þ¡Ê¸øÈñÉÕ²Ã¾ðÊó¤Î¼èÆÀ¡Ë
           MOVE     SPACE     TO     WK-KOH-FTNKBN.
           IF  (WK-1001-PREFNUM           =       40)    AND
               (COMB-KOHHKNNUM(KOH-IDX)(1:1)  >= "1")    AND
               (COMB-KOHHKNNUM(KOH-IDX)(1:1)  <= "8")
               PERFORM     23061-KOHPLUS-FUKUOKA-SEC
           END-IF.
      *
           IF  (WK-1001-PREFNUM         =  01                    ) AND
               (COMB-KOHHKNNUM(KOH-IDX) = "142" OR "143" OR "144") AND
               (LNK-SYU-SRYYMD(1:6)    >= "200410"               )
               IF    LNK-S02-SYOSINFLG  =  SPACE
                    MOVE    SPACE   TO   COMB-KOH-NYUJGNKBN(KOH-IDX)
                    MOVE    ZERO    TO   WK-NYUKFTNRATE
               END-IF
           END-IF.
      *
           IF  (WK-1001-PREFNUM         =  01                    ) AND
               (COMB-KOHHKNNUM(KOH-IDX) = "342" OR "443" OR "444") AND
               (WK-NYUKFTNRATE          =  ZERO                  ) AND
               (LNK-SYU-SRYYMD(1:6)    >= "200410"               )
               IF    LNK-S02-SYOSINFLG  =  SPACE
                    MOVE    SPACE   TO   COMB-KOH-NYUJGNKBN(KOH-IDX)
                    MOVE    ZERO    TO   WK-NYUKFTNRATE
               END-IF
           END-IF.
      *
       2306-KOH-HKNREAD-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñÉÕ²Ã¾ðÊó¤Î¼èÆÀ¡ÊÊ¡²¬¸©ÂÐ±þ¡Ë                  *
      ******************************************************************
       23061-KOHPLUS-FUKUOKA-SEC           SECTION.
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
               MOVE   LNK-SKOHPLUS-NYUSKJRYYKBN
                                             TO   WK-NYUSKJRYYKBN
               MOVE   WK-KOH-FTNRATE         TO   WK-NYUKFTNRATE
               IF     WK-NYUKFTNRATE  =  ZERO
                      MOVE    SPACE   TO  COMB-KOH-NYUJGNKBN(KOH-IDX)
               END-IF
           END-IF
      *
           .
      *
       23061-KOHPLUS-FUKUOKA-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñ¤Î´µ¼ÔÉéÃ´¶â³Û·×»»¡Ê¾å¸Â³Û¤¢¤ê¡Ë                    *
      ******************************************************************
       2307-KOH-KEISAN-SEC           SECTION.
      *
      *    ÆüÊÌ¼ýÇ¼»»Äê
           IF   WK-DAYINFFLG(KOH-IDX)  =    1
               PERFORM    23071-DAYINF-SANTEI-SEC
               GO    TO   2307-KOH-KEISAN-EXT
           END-IF.
      *
      *    ÉéÃ´¶â³Û¤Î·×»»
           IF  WK-NYUKFTNRATE     NOT =    ZERO
      *
               IF   WK-DAY-NYUKFTNRATE  =  ZERO
                    MOVE      WK-NYUKFTNRATE   TO   WK-DAY-NYUKFTNRATE
               ELSE
                 IF  WK-DAY-NYUKFTNRATE  > WK-NYUKFTNRATE
                    MOVE      WK-NYUKFTNRATE   TO   WK-DAY-NYUKFTNRATE
                 END-IF
               END-IF
      *
               COMPUTE   WK-FUTANWARI  =
                              WK-NYUKFTNRATE  /  100
               IF    WK-KOH-FTNKBN   NOT =  SPACE
                 EVALUATE  WK-KOH-FTNKBN
                    WHEN  "1"
                      COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                                LNK-SYU-KYUFUGAI-GOKEI-TEN
                                        *  10
                                        *  WK-FUTANWARI
                      COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                                LNK-SYU-KYUFUGAI-GOKEI-TEN
                                        *  10
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
               END-IF
      *
               IF   LNK-SYU-KOH-HKNNUM(KOH-IDX)  =      "027"           
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
      *********    CONTINUE
               ELSE
                   COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                             LNK-SYU-KOH-COMPFTN(KOH-IDX)  +  5
                   COMPUTE   WK-FTN      =
                             LNK-SYU-KOH-COMPFTN(KOH-IDX)  / 10
                   COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                             WK-FTN   *  10
               END-IF
               IF   LNK-S02-KOUREI-NYUIN-KBN     =   "1"                
                IF  (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027"       )
                AND ( (WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                     ((WK-1001-PREFNUM      =  20 OR  22 ) AND
                      (LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) = "0" OR "9" )))
                    COMPUTE  FTN-KOH-YYY  =  40200  -  FTN-KOH-XXX        
                    IF  LNK-SYU-KOH-COMPFTN(KOH-IDX)  >  FTN-KOH-YYY      
                      MOVE  FTN-KOH-YYY TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)  
                    END-IF
      *
                    COMPUTE  FTN-KOH-YYY-ENTANI                         
                                      =  40200  -  FTN-KOH-XXX-ENTANI   
                    IF    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                       >    FTN-KOH-YYY-ENTANI          
                      MOVE  FTN-KOH-YYY-ENTANI TO                       
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                    END-IF                                              
                END-IF
               END-IF                                              
               IF  ( FLG-SYUHEN                   =   "1" )
               AND ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "027")
                  IF    LNK-SYU-KOH-COMPFTN(KOH-IDX)   >
                                            ZAN-KOHI1-HEN
                     MOVE   ZAN-KOHI1-HEN   TO   WK-HEN-KOH1COMPFTN 
                  ELSE
                     MOVE   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                                            TO   WK-HEN-KOH1COMPFTN 
                  END-IF
               END-IF
               IF  ( LNK-S02-KOUREI-NYUIN-KBN     =   "1"  )            
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
               PERFORM    23075-KOHHI-HIKAKU4-SEC                             
               IF ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"         ) OR
                  ( HKN-RECEFTNDSPKBN            =  "2"           ) OR
                  ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"      ) AND
                    (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "015" OR "016") )
                 IF    LNK-SYU-KOH-COMPFTN(KOH-IDX)   >                 
                                            ZAN-KOHI-T2(KOH-IDX)        
                     MOVE   ZAN-KOHI-T2(KOH-IDX)   TO                   
                                          WK-KOH-COMPFTN(KOH-IDX)       
                     PERFORM    23073-KOHHI-HIKAKU2-SEC                       
                 END-IF                                                 
               END-IF                                                   
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
               END-IF
               PERFORM    23072-KOHHI-HIKAKU-SEC                              
               COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                         LNK-SYU-KOH-COMPFTN(KOH-IDX)     
               IF    ZAN-KOHI-T(KOH-IDX)   NOT =   ZERO
                   COMPUTE   ZAN-KOHI-T(KOH-IDX) =
                             ZAN-KOHI-T(KOH-IDX)  +  5
                   COMPUTE   WK-FTN      =
                             ZAN-KOHI-T(KOH-IDX)  /  10
                   COMPUTE   ZAN-KOHI-T(KOH-IDX) =
                             WK-FTN   *  10
               END-IF
               MOVE  WK-NYUSKJRYYKBN    TO LNK-SYU-SKJ-FTNKBN(KOH-IDX)
               MOVE  WK-SHOKUJI-NISSU   TO LNK-SYU-SKJ-NISSU(KOH-IDX)
               MOVE  WK-SHOKUJI-RYOYOHI TO LNK-SYU-SKJ-RYOYOHI(KOH-IDX)
               EVALUATE   WK-NYUSKJRYYKBN
                   WHEN   "1"
                   WHEN   "2"
                   WHEN   "4"
                       MOVE  WK-SHOKUJI-FTN
                                      TO   LNK-SYU-SKJ-FTN(KOH-IDX)
                                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
                       IF   WK-NYUSKJJYOGAK  NOT =  ZERO
                          COMPUTE   WK-NYUSKJJYOGAK2  =
                                    WK-NYUSKJJYOGAK * WK-SHOKUJI-NISSU
                          IF   LNK-SYU-SKJ-FTN(KOH-IDX)
                                              >   WK-NYUSKJJYOGAK2
                             COMPUTE   LNK-SYU-SKJ-FTN(KOH-IDX) =
                                       LNK-SYU-SKJ-FTN(KOH-IDX) -
                                       WK-NYUSKJJYOGAK2
                          ELSE
                             MOVE   ZERO   TO  
                                      LNK-SYU-SKJ-FTN(KOH-IDX) 
                          END-IF
      *****               IF   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****                                   >   WK-NYUSKJJYOGAK2
      *****                  COMPUTE   LNK-SYU-SKJ-FTN-RECE(KOH-IDX) =
      *****                            LNK-SYU-SKJ-FTN-RECE(KOH-IDX) -
      *****                            WK-NYUSKJJYOGAK2
      *****               ELSE
      *****                  MOVE   ZERO   TO
      *****                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****               END-IF
                       END-IF
                       IF   LNK-SYU-SKJ-FTNKBN(KOH-IDX)  =  "2"
                         IF    LNK-SYU-SKJ-FTN(KOH-IDX)   >
                                              ZAN-KOHI-T(KOH-IDX)
                            MOVE   ZAN-KOHI-T(KOH-IDX)   TO
                                        LNK-SYU-SKJ-FTN(KOH-IDX)
      *                                 LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
                            MOVE   ZERO      TO  ZAN-KOHI-T(KOH-IDX)
                         ELSE
                            COMPUTE  ZAN-KOHI-T(KOH-IDX)    =
                                     ZAN-KOHI-T(KOH-IDX)    -
                                     LNK-SYU-SKJ-FTN(KOH-IDX)
                         END-IF
                       END-IF
                   WHEN   "3"
                       MOVE  WK-SHOKUJI-FTN
                                      TO   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****            IF   WK-NYUSKJJYOGAK  NOT =  ZERO
      *****               COMPUTE   WK-NYUSKJJYOGAK2  =
      *****                         WK-NYUSKJJYOGAK * WK-SHOKUJI-NISSU
      *****               IF   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****                                   >   WK-NYUSKJJYOGAK2
      *****                  COMPUTE   LNK-SYU-SKJ-FTN-RECE(KOH-IDX) =
      *****                            LNK-SYU-SKJ-FTN-RECE(KOH-IDX) -
      *****                            WK-NYUSKJJYOGAK2
      *****               ELSE
      *****                  MOVE   ZERO   TO
      *****                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****               END-IF
      *****            END-IF
               END-EVALUATE
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
               IF  (WK-1001-PREFNUM             =  07      ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "245"    ) AND
                   (LNK-SYU-SRYYMD(1:6)        >= "200412" )
                  PERFORM   23076-TOKUSYU-SANTEI-SEC
                  GO    TO  2307-KOH-KEISAN-EXT
               END-IF
      *        ÆÃ¼ì·×»»¡Ê¸ÂÅÙ¤òÄ¶¤¨¤¿¾ì¹ç¡¢´µ¼ÔÉéÃ´È¯À¸¡ÊÄ¶¤¨¤¿³Û¡Ë¡Ë
      *********¡Ê²¬»³¸©¡Ë
               IF  (WK-1001-PREFNUM             =  33      ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX) = "285"    ) AND
                   (LNK-SYU-SRYYMD(1:6)        >= "200110" )
                  PERFORM   23077-TOKUSYU-SANTEI2-SEC
                  GO    TO  2307-KOH-KEISAN-EXT
               END-IF
      **
               IF    LNK-SYU-SYUCOMPFTN-ENTANI   NOT =    ZERO          
                   IF  (  LNK-S02-KOUREI-NYUIN-KBN         =   "1"  )
                   AND (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027" )
                   AND ( ( WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                        (( WK-1001-PREFNUM      =  20 OR  22 ) AND
                         ( LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) 
                                                     = "0" OR "9" )))
                     IF  ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "012"  )
                     AND ( LNK-SYU-SYUHKNNUM            =  SPACE  )
                       IF    LNK-SYU-SYUCOMPFTN-ENTANI
                                                 >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE   ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     ELSE
                       COMPUTE  FTN-KOH-YYY-ENTANI
                                      =  40200  -  FTN-KOH-XXX-ENTANI
                       IF  LNK-SYU-SYUCOMPFTN-ENTANI
                                                  >   FTN-KOH-YYY-ENTANI
                         MOVE  FTN-KOH-YYY-ENTANI TO  FTN-KOH-ZZZ-ENTANI
                       ELSE
                         MOVE  LNK-SYU-SYUCOMPFTN-ENTANI
                                                  TO  FTN-KOH-ZZZ-ENTANI
                       END-IF
                       IF    FTN-KOH-ZZZ-ENTANI  >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE  ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE  FTN-KOH-ZZZ-ENTANI
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                   ELSE                                   
                       IF    LNK-SYU-SYUCOMPFTN-ENTANI
                                                 >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE   ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                   END-IF
               ELSE
                 IF    LNK-SYU-KOH-COMPFTN-ENTANI(1)   NOT =    ZERO
                   IF  (  LNK-S02-KOUREI-NYUIN-KBN         =   "1"  )
                   AND (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027" )
                   AND ( ( WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                        (( WK-1001-PREFNUM      =  20 OR  22 ) AND
                         ( LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) 
                                                     = "0" OR "9" )))
                     IF  ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "012"  )
                     AND ( LNK-SYU-SYUHKNNUM            =  SPACE  )
                       IF    LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                                 >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE   ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     ELSE
                       COMPUTE  FTN-KOH-YYY-ENTANI
                                      =  40200  -  FTN-KOH-XXX-ENTANI
                       IF  LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                                  >   FTN-KOH-YYY-ENTANI
                         MOVE  FTN-KOH-YYY-ENTANI TO  FTN-KOH-ZZZ-ENTANI
                       ELSE
                         MOVE  LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                                  TO  FTN-KOH-ZZZ-ENTANI
                       END-IF
                       IF    FTN-KOH-ZZZ-ENTANI  >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE  ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE  FTN-KOH-ZZZ-ENTANI
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     END-IF
                   ELSE                                                 
                       IF    LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                                 >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE   ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                   END-IF
                 ELSE
                   IF  (  LNK-S02-KOUREI-NYUIN-KBN         =   "1"  )
                   AND (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027" )
                   AND ( ( WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                        (( WK-1001-PREFNUM      =  20 OR  22 ) AND
                         ( LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) 
                                                     = "0" OR "9" )))
                     IF  ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "012"  )
                     AND ( LNK-SYU-SYUHKNNUM            =  SPACE  )
                       IF    WK-ZENGAKU   >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE   ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE   WK-ZENGAKU
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
                     ELSE
                       COMPUTE  FTN-KOH-YYY-ENTANI
                                      =  40200  -  FTN-KOH-XXX-ENTANI
                       IF  WK-ZENGAKU   >   FTN-KOH-YYY-ENTANI
                         MOVE  FTN-KOH-YYY-ENTANI TO  FTN-KOH-ZZZ-ENTANI
                       ELSE
                         MOVE  WK-ZENGAKU         TO  FTN-KOH-ZZZ-ENTANI
                       END-IF
                       IF    FTN-KOH-ZZZ-ENTANI  >  ZAN-KOHI-T3(KOH-IDX)
                           MOVE  ZAN-KOHI-T3(KOH-IDX)
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       ELSE
                           MOVE  FTN-KOH-ZZZ-ENTANI
                                 TO  LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                       END-IF
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
               END-IF                                                   
               PERFORM    23075-KOHHI-HIKAKU4-SEC                             
      **
               IF ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"         ) OR
                  ( HKN-RECEFTNDSPKBN            =  "2"           ) OR
                  ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"      ) AND
                    (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "015" OR "016") )
                IF    LNK-SYU-SYUCOMPFTN   NOT =    ZERO
                   IF  (  LNK-S02-KOUREI-NYUIN-KBN         =   "1"  )
                   AND (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027" )
                   AND ( ( WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                        (( WK-1001-PREFNUM      =  20 OR  22 ) AND
                         ( LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) 
                                                     = "0" OR "9" )))
                     IF  ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "012"  )
                     AND ( LNK-SYU-SYUHKNNUM            =  SPACE  )
                       IF   LNK-SYU-SYUCOMPFTN   >  ZAN-KOHI-T2(KOH-IDX)
                           MOVE   ZAN-KOHI-T2(KOH-IDX)
                                            TO  WK-KOH-COMPFTN(KOH-IDX)
                           PERFORM    23073-KOHHI-HIKAKU2-SEC
                       ELSE
                           MOVE  LNK-SYU-SYUCOMPFTN
                                            TO  WK-KOH-COMPFTN(KOH-IDX)
                           PERFORM    23073-KOHHI-HIKAKU2-SEC
                       END-IF
                     ELSE
                       COMPUTE  FTN-KOH-YYY  =  40200  -  FTN-KOH-XXX   
                       IF  LNK-SYU-SYUCOMPFTN  >  FTN-KOH-YYY           
                         MOVE  FTN-KOH-YYY         TO  FTN-KOH-ZZZ      
                       ELSE                                             
                         MOVE  LNK-SYU-SYUCOMPFTN  TO  FTN-KOH-ZZZ      
                       END-IF                                           
                       IF    FTN-KOH-ZZZ      >    ZAN-KOHI-T2(KOH-IDX)  
                           MOVE  ZAN-KOHI-T2(KOH-IDX)                    
                                            TO  WK-KOH-COMPFTN(KOH-IDX)
                           PERFORM    23073-KOHHI-HIKAKU2-SEC                  
                       ELSE                                             
                           MOVE  FTN-KOH-ZZZ                            
                                            TO  WK-KOH-COMPFTN(KOH-IDX) 
                           PERFORM    23073-KOHHI-HIKAKU2-SEC                  
                       END-IF                                           
                     END-IF
                   ELSE                                   
                       IF   LNK-SYU-SYUCOMPFTN   >  ZAN-KOHI-T2(KOH-IDX)
                           MOVE   ZAN-KOHI-T2(KOH-IDX)
                                            TO  WK-KOH-COMPFTN(KOH-IDX)
                           PERFORM    23073-KOHHI-HIKAKU2-SEC
                       ELSE
                           MOVE  LNK-SYU-SYUCOMPFTN
                                            TO  WK-KOH-COMPFTN(KOH-IDX)
                           PERFORM    23073-KOHHI-HIKAKU2-SEC
                       END-IF
                   END-IF
                ELSE
                 IF    LNK-SYU-KOH-COMPFTN(1)   NOT =    ZERO
                   IF  (  LNK-S02-KOUREI-NYUIN-KBN         =   "1"  )
                   AND (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027" )
                   AND ( ( WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                        (( WK-1001-PREFNUM      =  20 OR  22 ) AND
                         ( LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) 
                                                     = "0" OR "9" )))
                     IF  ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "012"  )
                     AND ( LNK-SYU-SYUHKNNUM            =  SPACE  )
                       IF    LNK-SYU-KOH-COMPFTN(1)
                                     >    ZAN-KOHI-T2(KOH-IDX)
                          MOVE   ZAN-KOHI-T2(KOH-IDX)
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC
                       ELSE
                          MOVE   LNK-SYU-KOH-COMPFTN(1)
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC
                       END-IF
                     ELSE
                       COMPUTE  FTN-KOH-YYY  =  40200  -  FTN-KOH-XXX   
                       IF  LNK-SYU-KOH-COMPFTN(1) >  FTN-KOH-YYY        
                         MOVE  FTN-KOH-YYY            TO  FTN-KOH-ZZZ   
                       ELSE                                             
                         MOVE  LNK-SYU-KOH-COMPFTN(1) TO  FTN-KOH-ZZZ   
                       END-IF                                           
                       IF FTN-KOH-ZZZ   >    ZAN-KOHI-T2(KOH-IDX)        
                          MOVE   ZAN-KOHI-T2(KOH-IDX)                    
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC                   
                       ELSE                                             
                          MOVE   FTN-KOH-ZZZ  
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC                   
                       END-IF                                           
                     END-IF
                   ELSE                                                 
                       IF    LNK-SYU-KOH-COMPFTN(1)
                                     >    ZAN-KOHI-T2(KOH-IDX)
                          MOVE   ZAN-KOHI-T2(KOH-IDX)
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC
                       ELSE
                          MOVE   LNK-SYU-KOH-COMPFTN(1)
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC
                       END-IF
                   END-IF
                 ELSE
                   IF  (  LNK-S02-KOUREI-NYUIN-KBN         =   "1"  )
                   AND (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027" )
                   AND ( ( WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                        (( WK-1001-PREFNUM      =  20 OR  22 ) AND
                         ( LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) 
                                                     = "0" OR "9" )))
                     IF  ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "012"  )
                     AND ( LNK-SYU-SYUHKNNUM            =  SPACE  )
                       IF    WK-ZENGAKU   >    ZAN-KOHI-T2(KOH-IDX)
                          MOVE   ZAN-KOHI-T2(KOH-IDX)
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC
                       ELSE
                          MOVE   WK-ZENGAKU   
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC
                       END-IF
                     ELSE
                       COMPUTE  FTN-KOH-YYY  =  40200  -  FTN-KOH-XXX   
                       IF  WK-ZENGAKU  >  FTN-KOH-YYY        
                         MOVE  FTN-KOH-YYY   TO  FTN-KOH-ZZZ   
                       ELSE                                             
                         MOVE  WK-ZENGAKU    TO  FTN-KOH-ZZZ   
                       END-IF                                           
                       IF FTN-KOH-ZZZ   >    ZAN-KOHI-T2(KOH-IDX)        
                          MOVE   ZAN-KOHI-T2(KOH-IDX)                    
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC                   
                       ELSE                                             
                          MOVE   FTN-KOH-ZZZ  
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC                   
                       END-IF                                           
                     END-IF
                   ELSE                                                 
                       IF    WK-ZENGAKU   >    ZAN-KOHI-T2(KOH-IDX)
                          MOVE   ZAN-KOHI-T2(KOH-IDX)
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC
                       ELSE
                          MOVE   WK-ZENGAKU   
                                       TO  WK-KOH-COMPFTN(KOH-IDX)
                          PERFORM    23073-KOHHI-HIKAKU2-SEC
                       END-IF
                   END-IF
                 END-IF
                END-IF
               END-IF                                                   
      **
               IF    LNK-SYU-SYUCOMPFTN   NOT =    ZERO
                   IF  (  LNK-S02-KOUREI-NYUIN-KBN         =   "1"  )
                   AND (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027" )
                   AND ( ( WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                        (( WK-1001-PREFNUM      =  20 OR  22 ) AND
                         ( LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) 
                                                     = "0" OR "9" )))
                     IF  ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "012"  )
                     AND ( LNK-SYU-SYUHKNNUM            =  SPACE  )
                       IF   LNK-SYU-SYUCOMPFTN   >  ZAN-KOHI-T(KOH-IDX)
                           MOVE   ZAN-KOHI-T(KOH-IDX)
                                            TO  LNK-SYU-KOH-COMPFTN
                                                         (KOH-IDX)
                           MOVE   ZERO      TO  ZAN-KOHI-T(KOH-IDX)
                           PERFORM    23072-KOHHI-HIKAKU-SEC
                       ELSE
                           MOVE  LNK-SYU-SYUCOMPFTN
                                            TO  LNK-SYU-KOH-COMPFTN
                                                         (KOH-IDX)
                           COMPUTE    ZAN-KOHI-T(KOH-IDX)  =
                                      ZAN-KOHI-T(KOH-IDX)  -
                                      LNK-SYU-KOH-COMPFTN(KOH-IDX)
                           PERFORM    23072-KOHHI-HIKAKU-SEC
                       END-IF
                     ELSE
                       COMPUTE  FTN-KOH-YYY  =  40200  -  FTN-KOH-XXX   
                       IF  LNK-SYU-SYUCOMPFTN  >  FTN-KOH-YYY           
                         MOVE  FTN-KOH-YYY         TO  FTN-KOH-ZZZ      
                       ELSE                                             
                         MOVE  LNK-SYU-SYUCOMPFTN  TO  FTN-KOH-ZZZ      
                       END-IF                                           
                       IF    FTN-KOH-ZZZ      >    ZAN-KOHI-T(KOH-IDX)  
                           MOVE  ZAN-KOHI-T(KOH-IDX)                    
                                            TO  LNK-SYU-KOH-COMPFTN     
                                                         (KOH-IDX)      
                           MOVE   ZERO      TO  ZAN-KOHI-T(KOH-IDX)     
                           PERFORM    23072-KOHHI-HIKAKU-SEC                  
                       ELSE                                             
                           MOVE  FTN-KOH-ZZZ                            
                                            TO  LNK-SYU-KOH-COMPFTN     
                                                         (KOH-IDX)      
                           COMPUTE    ZAN-KOHI-T(KOH-IDX)  =            
                                      ZAN-KOHI-T(KOH-IDX)  -            
                                      LNK-SYU-KOH-COMPFTN(KOH-IDX)      
                           PERFORM    23072-KOHHI-HIKAKU-SEC                  
                       END-IF                                           
                     END-IF
                   ELSE                                   
                       IF   LNK-SYU-SYUCOMPFTN   >  ZAN-KOHI-T(KOH-IDX)
                           MOVE   ZAN-KOHI-T(KOH-IDX)
                                            TO  LNK-SYU-KOH-COMPFTN
                                                         (KOH-IDX)
                           MOVE   ZERO      TO  ZAN-KOHI-T(KOH-IDX)
                           PERFORM    23072-KOHHI-HIKAKU-SEC
                       ELSE
                           MOVE  LNK-SYU-SYUCOMPFTN
                                            TO  LNK-SYU-KOH-COMPFTN
                                                         (KOH-IDX)
                           COMPUTE    ZAN-KOHI-T(KOH-IDX)  =
                                      ZAN-KOHI-T(KOH-IDX)  -
                                      LNK-SYU-KOH-COMPFTN(KOH-IDX)
                           PERFORM    23072-KOHHI-HIKAKU-SEC
                       END-IF
                   END-IF
               ELSE
                 IF    LNK-SYU-KOH-COMPFTN(1)   NOT =    ZERO
                   IF  (  LNK-S02-KOUREI-NYUIN-KBN         =   "1"  )
                   AND (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027" )
                   AND ( ( WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                        (( WK-1001-PREFNUM      =  20 OR  22 ) AND
                         ( LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) 
                                                     = "0" OR "9" )))
                     IF  ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "012"  )
                     AND ( LNK-SYU-SYUHKNNUM            =  SPACE  )
                       IF    LNK-SYU-KOH-COMPFTN(1)
                                     >    ZAN-KOHI-T(KOH-IDX)
                          MOVE   ZAN-KOHI-T(KOH-IDX)
                                       TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                          MOVE   ZERO  TO  ZAN-KOHI-T(KOH-IDX)
                          PERFORM    23072-KOHHI-HIKAKU-SEC
                       ELSE
                          MOVE   LNK-SYU-KOH-COMPFTN(1)
                                              TO  LNK-SYU-KOH-COMPFTN
                                                            (KOH-IDX)
                          PERFORM    23072-KOHHI-HIKAKU-SEC
                          COMPUTE   ZAN-KOHI-T(KOH-IDX)  =
                                    ZAN-KOHI-T(KOH-IDX)  -
                                    LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     ELSE
                       COMPUTE  FTN-KOH-YYY  =  40200  -  FTN-KOH-XXX   
                       IF  LNK-SYU-KOH-COMPFTN(1) >  FTN-KOH-YYY        
                         MOVE  FTN-KOH-YYY            TO  FTN-KOH-ZZZ   
                       ELSE                                             
                         MOVE  LNK-SYU-KOH-COMPFTN(1) TO  FTN-KOH-ZZZ   
                       END-IF                                           
                       IF FTN-KOH-ZZZ   >    ZAN-KOHI-T(KOH-IDX)        
                          MOVE   ZAN-KOHI-T(KOH-IDX)                    
                                       TO  LNK-SYU-KOH-COMPFTN(KOH-IDX) 
                          MOVE   ZERO  TO  ZAN-KOHI-T(KOH-IDX)          
                          PERFORM    23072-KOHHI-HIKAKU-SEC                   
                       ELSE                                             
                          MOVE   FTN-KOH-ZZZ  TO  LNK-SYU-KOH-COMPFTN   
                                                            (KOH-IDX)   
                          PERFORM    23072-KOHHI-HIKAKU-SEC                   
                          COMPUTE   ZAN-KOHI-T(KOH-IDX)  =              
                                    ZAN-KOHI-T(KOH-IDX)  -              
                                    LNK-SYU-KOH-COMPFTN(KOH-IDX)        
                       END-IF                                           
                     END-IF
                   ELSE                                                 
                       IF    LNK-SYU-KOH-COMPFTN(1)
                                     >    ZAN-KOHI-T(KOH-IDX)
                          MOVE   ZAN-KOHI-T(KOH-IDX)
                                       TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                          MOVE   ZERO  TO  ZAN-KOHI-T(KOH-IDX)
                          PERFORM    23072-KOHHI-HIKAKU-SEC
                       ELSE
                          MOVE   LNK-SYU-KOH-COMPFTN(1)
                                              TO  LNK-SYU-KOH-COMPFTN
                                                            (KOH-IDX)
                          PERFORM    23072-KOHHI-HIKAKU-SEC
                          COMPUTE   ZAN-KOHI-T(KOH-IDX)  =
                                    ZAN-KOHI-T(KOH-IDX)  -
                                    LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                   END-IF
                 ELSE
      *            MOVE   ZERO  TO  ZAN-KOHI-T(KOH-IDX)
                   IF  (  LNK-S02-KOUREI-NYUIN-KBN         =   "1"  )
                   AND (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027" )
                   AND ( ( WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                        (( WK-1001-PREFNUM      =  20 OR  22 ) AND
                         ( LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) 
                                                     = "0" OR "9" )))
                     IF  ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "012"  )
                     AND ( LNK-SYU-SYUHKNNUM            =  SPACE  )
                       IF    WK-ZENGAKU   >    ZAN-KOHI-T(KOH-IDX)
                          MOVE   ZAN-KOHI-T(KOH-IDX)
                                       TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                          MOVE   ZERO  TO  ZAN-KOHI-T(KOH-IDX)
                          PERFORM    23072-KOHHI-HIKAKU-SEC
                       ELSE
                          MOVE   WK-ZENGAKU   TO  LNK-SYU-KOH-COMPFTN
                                                            (KOH-IDX)
                          PERFORM    23072-KOHHI-HIKAKU-SEC
                          COMPUTE   ZAN-KOHI-T(KOH-IDX)  =
                                    ZAN-KOHI-T(KOH-IDX)  -
                                    LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                     ELSE
                       COMPUTE  FTN-KOH-YYY  =  40200  -  FTN-KOH-XXX   
                       IF  WK-ZENGAKU  >  FTN-KOH-YYY        
                         MOVE  FTN-KOH-YYY   TO  FTN-KOH-ZZZ   
                       ELSE                                             
                         MOVE  WK-ZENGAKU    TO  FTN-KOH-ZZZ   
                       END-IF                                           
                       IF FTN-KOH-ZZZ   >    ZAN-KOHI-T(KOH-IDX)        
                          MOVE   ZAN-KOHI-T(KOH-IDX)                    
                                       TO  LNK-SYU-KOH-COMPFTN(KOH-IDX) 
                          MOVE   ZERO  TO  ZAN-KOHI-T(KOH-IDX)          
                          PERFORM    23072-KOHHI-HIKAKU-SEC                   
                       ELSE                                             
                          MOVE   FTN-KOH-ZZZ  TO  LNK-SYU-KOH-COMPFTN   
                                                            (KOH-IDX)   
                          PERFORM    23072-KOHHI-HIKAKU-SEC                   
                          COMPUTE   ZAN-KOHI-T(KOH-IDX)  =              
                                    ZAN-KOHI-T(KOH-IDX)  -              
                                    LNK-SYU-KOH-COMPFTN(KOH-IDX)        
                       END-IF                                           
                     END-IF
                   ELSE                                                 
                       IF    WK-ZENGAKU   >    ZAN-KOHI-T(KOH-IDX)
                          MOVE   ZAN-KOHI-T(KOH-IDX)
                                       TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                          MOVE   ZERO  TO  ZAN-KOHI-T(KOH-IDX)
                          PERFORM    23072-KOHHI-HIKAKU-SEC
                       ELSE
                          MOVE   WK-ZENGAKU   TO  LNK-SYU-KOH-COMPFTN
                                                            (KOH-IDX)
                          PERFORM    23072-KOHHI-HIKAKU-SEC
                          COMPUTE   ZAN-KOHI-T(KOH-IDX)  =
                                    ZAN-KOHI-T(KOH-IDX)  -
                                    LNK-SYU-KOH-COMPFTN(KOH-IDX)
                       END-IF
                   END-IF
                 END-IF
               END-IF
               COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                         LNK-SYU-KOH-COMPFTN(KOH-IDX)     
               IF    ZAN-KOHI-T(KOH-IDX)   NOT =   ZERO
                   COMPUTE   ZAN-KOHI-T(KOH-IDX) =
                             ZAN-KOHI-T(KOH-IDX)  +  5
                   COMPUTE   WK-FTN      =
                             ZAN-KOHI-T(KOH-IDX)  /  10
                   COMPUTE   ZAN-KOHI-T(KOH-IDX) =
                             WK-FTN   *  10
               END-IF
               MOVE  WK-NYUSKJRYYKBN    TO LNK-SYU-SKJ-FTNKBN(KOH-IDX)
               MOVE  WK-SHOKUJI-NISSU   TO LNK-SYU-SKJ-NISSU(KOH-IDX)
               MOVE  WK-SHOKUJI-RYOYOHI TO LNK-SYU-SKJ-RYOYOHI(KOH-IDX)
               EVALUATE   WK-NYUSKJRYYKBN
                   WHEN   "1"
                   WHEN   "2"
                   WHEN   "4"
                       MOVE  WK-SHOKUJI-FTN
                                      TO   LNK-SYU-SKJ-FTN(KOH-IDX)
                                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
                       IF   WK-NYUSKJJYOGAK  NOT =  ZERO
                          COMPUTE   WK-NYUSKJJYOGAK2  =
                                    WK-NYUSKJJYOGAK * WK-SHOKUJI-NISSU
                          IF   LNK-SYU-SKJ-FTN(KOH-IDX)
                                              >   WK-NYUSKJJYOGAK2
                             COMPUTE   LNK-SYU-SKJ-FTN(KOH-IDX) =
                                       LNK-SYU-SKJ-FTN(KOH-IDX) -
                                       WK-NYUSKJJYOGAK2
                          ELSE
                             MOVE   ZERO   TO  
                                      LNK-SYU-SKJ-FTN(KOH-IDX) 
                          END-IF
      *****               IF   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****                                   >   WK-NYUSKJJYOGAK2
      *****                  COMPUTE   LNK-SYU-SKJ-FTN-RECE(KOH-IDX) =
      *****                            LNK-SYU-SKJ-FTN-RECE(KOH-IDX) -
      *****                            WK-NYUSKJJYOGAK2
      *****               ELSE
      *****                  MOVE   ZERO   TO
      *****                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****               END-IF
                       END-IF
                       IF   LNK-SYU-SKJ-FTNKBN(KOH-IDX)  =  "2"
                         IF    LNK-SYU-SKJ-FTN(KOH-IDX)   >
                                              ZAN-KOHI-T(KOH-IDX)
                            MOVE   ZAN-KOHI-T(KOH-IDX)   TO
                                        LNK-SYU-SKJ-FTN(KOH-IDX)
      *                                 LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
                            MOVE   ZERO      TO  ZAN-KOHI-T(KOH-IDX)
                         ELSE
                            COMPUTE  ZAN-KOHI-T(KOH-IDX)    =
                                     ZAN-KOHI-T(KOH-IDX)    -
                                     LNK-SYU-SKJ-FTN(KOH-IDX)
                         END-IF
                       END-IF
                   WHEN   "3"
                       MOVE  WK-SHOKUJI-FTN
                                      TO   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****            IF   WK-NYUSKJJYOGAK  NOT =  ZERO
      *****               COMPUTE   WK-NYUSKJJYOGAK2  =
      *****                         WK-NYUSKJJYOGAK * WK-SHOKUJI-NISSU
      *****               IF   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****                                   >   WK-NYUSKJJYOGAK2
      *****                  COMPUTE   LNK-SYU-SKJ-FTN-RECE(KOH-IDX) =
      *****                            LNK-SYU-SKJ-FTN-RECE(KOH-IDX) -
      *****                            WK-NYUSKJJYOGAK2
      *****               ELSE
      *****                  MOVE   ZERO   TO
      *****                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****               END-IF
      *****            END-IF
               END-EVALUATE
           END-IF.
      *
       2307-KOH-KEISAN-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñ¤Î´µ¼ÔÉéÃ´¶â³Û·×»»¡Ê¾å¸Â³Û¤Ê¤·¡Ë                  *
      ******************************************************************
       2308-KOH-KEISAN2-SEC           SECTION.
      *
      *    ÉéÃ´¶â³Û¤Î·×»»
           IF  WK-NYUKFTNRATE     NOT =    ZERO
      *
               IF   WK-DAY-NYUKFTNRATE  =  ZERO
                    MOVE      WK-NYUKFTNRATE   TO   WK-DAY-NYUKFTNRATE
               ELSE
                 IF  WK-DAY-NYUKFTNRATE  > WK-NYUKFTNRATE
                    MOVE      WK-NYUKFTNRATE   TO   WK-DAY-NYUKFTNRATE
                 END-IF
               END-IF
      *
               COMPUTE   WK-FUTANWARI  =
                              WK-NYUKFTNRATE  /  100
               IF    WK-KOH-FTNKBN   NOT =  SPACE
                 EVALUATE  WK-KOH-FTNKBN
                    WHEN  "1"
                      COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                                LNK-SYU-KYUFUGAI-GOKEI-TEN
                                        *  10
                                        *  WK-FUTANWARI
                      COMPUTE   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  =
                                LNK-SYU-KYUFUGAI-GOKEI-TEN
                                        *  10
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
               END-IF
      *
               IF   LNK-SYU-KOH-HKNNUM(KOH-IDX)  =      "027"           
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
      *********    CONTINUE
               ELSE
                   COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX) =
                             LNK-SYU-KOH-COMPFTN(KOH-IDX)  +  5
                   COMPUTE   WK-FTN      =
                             LNK-SYU-KOH-COMPFTN(KOH-IDX)  /  10
                   COMPUTE   LNK-SYU-KOH-COMPFTN(KOH-IDX) =  WK-FTN
                                                              *  10
               END-IF
               IF   LNK-S02-KOUREI-NYUIN-KBN     =   "1"                
                IF  (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027"       )
                AND ( (WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                     ((WK-1001-PREFNUM      =  20 OR  22 ) AND
                      (LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) = "0" OR "9" )))
                  COMPUTE  FTN-KOH-YYY  =  40200  -  FTN-KOH-XXX        
                  IF  LNK-SYU-KOH-COMPFTN(KOH-IDX)  >  FTN-KOH-YYY      
                    MOVE  FTN-KOH-YYY TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)  
                  END-IF                                                
      *
                  COMPUTE  FTN-KOH-YYY-ENTANI                           
                                    =  40200  -  FTN-KOH-XXX-ENTANI     
                  IF    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                     >    FTN-KOH-YYY-ENTANI            
                    MOVE  FTN-KOH-YYY-ENTANI TO                         
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  
                  END-IF                                                
                END-IF                                                  
               END-IF                                              
           ELSE
               IF   (WK-1001-PREFNUM         =  43           ) AND
                    (COMB-KOHHKNNUM(KOH-IDX) = "143" OR "342")
      *
                   MOVE    "1"   TO    FLG-KUMAMOTO
                   MOVE    "1"   TO    WK-TAIHI-RECEFTNDSPKBN(KOH-IDX)
                   PERFORM   23081-KUMAMOTO-KOHI-SEC
      *
               END-IF
           END-IF.
           PERFORM    23075-KOHHI-HIKAKU4-SEC.                                
           PERFORM    23072-KOHHI-HIKAKU-SEC.
           COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                     LNK-SYU-KOH-COMPFTN(KOH-IDX).     
           MOVE  WK-NYUSKJRYYKBN    TO  LNK-SYU-SKJ-FTNKBN(KOH-IDX)
           MOVE  WK-SHOKUJI-NISSU   TO  LNK-SYU-SKJ-NISSU(KOH-IDX)
           MOVE  WK-SHOKUJI-RYOYOHI TO  LNK-SYU-SKJ-RYOYOHI(KOH-IDX)
           EVALUATE   WK-NYUSKJRYYKBN
                   WHEN   "1"
                   WHEN   "2"
                   WHEN   "4"
                       MOVE  WK-SHOKUJI-FTN
                                      TO   LNK-SYU-SKJ-FTN(KOH-IDX)
                                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
                       IF   WK-NYUSKJJYOGAK  NOT =  ZERO
                          COMPUTE   WK-NYUSKJJYOGAK2  =
                                    WK-NYUSKJJYOGAK * WK-SHOKUJI-NISSU
                          IF   LNK-SYU-SKJ-FTN(KOH-IDX)
                                              >   WK-NYUSKJJYOGAK2
                             COMPUTE   LNK-SYU-SKJ-FTN(KOH-IDX) =
                                       LNK-SYU-SKJ-FTN(KOH-IDX) -
                                       WK-NYUSKJJYOGAK2
                          ELSE
                             MOVE   ZERO   TO  
                                      LNK-SYU-SKJ-FTN(KOH-IDX) 
                          END-IF
      *****               IF   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****                                   >   WK-NYUSKJJYOGAK2
      *****                  COMPUTE   LNK-SYU-SKJ-FTN-RECE(KOH-IDX) =
      *****                            LNK-SYU-SKJ-FTN-RECE(KOH-IDX) -
      *****                            WK-NYUSKJJYOGAK2
      *****               ELSE
      *****                  MOVE   ZERO   TO
      *****                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****               END-IF
                       END-IF
                       IF   LNK-SYU-SKJ-FTNKBN(KOH-IDX)  =  "2"
                         IF    LNK-SYU-SKJ-FTN(KOH-IDX)   >
                                              ZAN-KOHI-T(KOH-IDX)
                            MOVE   ZAN-KOHI-T(KOH-IDX)   TO
                                        LNK-SYU-SKJ-FTN(KOH-IDX)
                            MOVE   ZERO      TO  ZAN-KOHI-T(KOH-IDX)
                         ELSE
                            COMPUTE  ZAN-KOHI-T(KOH-IDX)    =
                                     ZAN-KOHI-T(KOH-IDX)    -
                                     LNK-SYU-SKJ-FTN(KOH-IDX)
                         END-IF
                       END-IF
                   WHEN   "3"
                       MOVE  WK-SHOKUJI-FTN
                                      TO   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****            IF   WK-NYUSKJJYOGAK  NOT =  ZERO
      *****               COMPUTE   WK-NYUSKJJYOGAK2  =
      *****                         WK-NYUSKJJYOGAK * WK-SHOKUJI-NISSU
      *****               IF   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****                                   >   WK-NYUSKJJYOGAK2
      *****                  COMPUTE   LNK-SYU-SKJ-FTN-RECE(KOH-IDX) =
      *****                            LNK-SYU-SKJ-FTN-RECE(KOH-IDX) -
      *****                            WK-NYUSKJJYOGAK2
      *****               ELSE
      *****                  MOVE   ZERO   TO
      *****                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
      *****               END-IF
      *****            END-IF
           END-EVALUATE.
      *     
       2308-KOH-KEISAN2-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      ******************************************************************
       23072-KOHHI-HIKAKU-SEC           SECTION.
      *
           IF    LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "972"
             CONTINUE
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
       23072-KOHHI-HIKAKU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *        ¡Ê¥ì¥»µ­ºÜ¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢ÍÑ¡Ë
      ******************************************************************
       23073-KOHHI-HIKAKU2-SEC           SECTION.                             
      *
           IF    LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "972"
             CONTINUE
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
       23073-KOHHI-HIKAKU2-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *        ¡Ê¥ì¥»µ­ºÜ¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢ÍÑ¡Ë¡ÊÆüÃ±°Ì¡Ë
      ******************************************************************
       23074-KOHHI-HIKAKU3-SEC           SECTION.
      *
           EVALUATE    KOH-IDX
               WHEN    2
                 IF  WK-KOH-COMPFTN(1)  =  ALL"9"
                  IF  WK-DAY-KOH-COMPFTN(1)  =  ALL"9"
                    IF  LNK-SYU-KOH-COMPFTN(1) < WK-DAY-KOH-COMPFTN(2)
                    AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                        MOVE  LNK-SYU-KOH-COMPFTN(1)
                                             TO  WK-DAY-KOH-COMPFTN(2)
                    END-IF
                  ELSE
                    IF  WK-DAY-KOH-COMPFTN(1)  < WK-DAY-KOH-COMPFTN(2)
                    AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                        MOVE  WK-DAY-KOH-COMPFTN(1)
                                             TO  WK-DAY-KOH-COMPFTN(2)
                    END-IF
                  END-IF
                 ELSE
                  IF  WK-DAY-KOH-COMPFTN(1)  =  ALL"9"
                    IF  WK-KOH-COMPFTN(1)      < WK-DAY-KOH-COMPFTN(2)
                    AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                        MOVE  WK-KOH-COMPFTN(1)
                                             TO  WK-DAY-KOH-COMPFTN(2)
                    END-IF
                  ELSE
                    IF  WK-DAY-KOH-COMPFTN(1)  < WK-DAY-KOH-COMPFTN(2)
                    AND LNK-SYU-KOH1HKNNUM   NOT =  SPACE
                        MOVE  WK-DAY-KOH-COMPFTN(1)
                                             TO  WK-DAY-KOH-COMPFTN(2)
                    END-IF
                  END-IF
                 END-IF
               WHEN    3
                 IF  WK-KOH-COMPFTN(2)  =  ALL"9"
                  IF  WK-DAY-KOH-COMPFTN(2)  =  ALL"9"
                    IF  LNK-SYU-KOH-COMPFTN(2) < WK-DAY-KOH-COMPFTN(3)
                    AND LNK-SYU-KOH2HKNNUM   NOT =  SPACE
                        MOVE  LNK-SYU-KOH-COMPFTN(2)
                                             TO  WK-DAY-KOH-COMPFTN(3)
                    END-IF
                  ELSE
                    IF  WK-DAY-KOH-COMPFTN(2)  < WK-DAY-KOH-COMPFTN(3)
                    AND LNK-SYU-KOH2HKNNUM   NOT =  SPACE
                        MOVE  WK-DAY-KOH-COMPFTN(2)
                                             TO  WK-DAY-KOH-COMPFTN(3)
                    END-IF
                  END-IF
                 ELSE
                  IF  WK-DAY-KOH-COMPFTN(2)  =  ALL"9"
                    IF  WK-KOH-COMPFTN(2)      < WK-DAY-KOH-COMPFTN(3)
                    AND LNK-SYU-KOH2HKNNUM   NOT =  SPACE
                        MOVE  WK-KOH-COMPFTN(2)
                                             TO  WK-DAY-KOH-COMPFTN(3)
                    END-IF
                  ELSE
                    IF  WK-DAY-KOH-COMPFTN(2)  < WK-DAY-KOH-COMPFTN(3)
                    AND LNK-SYU-KOH2HKNNUM   NOT =  SPACE
                        MOVE  WK-DAY-KOH-COMPFTN(2)
                                             TO  WK-DAY-KOH-COMPFTN(3)
                    END-IF
                  END-IF
                 END-IF
               WHEN    4
                 IF  WK-KOH-COMPFTN(3)  =  ALL"9"
                  IF  WK-DAY-KOH-COMPFTN(3)  =  ALL"9"
                    IF  LNK-SYU-KOH-COMPFTN(3) < WK-DAY-KOH-COMPFTN(4)
                    AND LNK-SYU-KOH3HKNNUM   NOT =  SPACE
                        MOVE  LNK-SYU-KOH-COMPFTN(3)
                                             TO  WK-DAY-KOH-COMPFTN(4)
                    END-IF
                  ELSE
                    IF  WK-DAY-KOH-COMPFTN(3)  < WK-DAY-KOH-COMPFTN(4)
                    AND LNK-SYU-KOH3HKNNUM   NOT =  SPACE
                        MOVE  WK-DAY-KOH-COMPFTN(3)
                                             TO  WK-DAY-KOH-COMPFTN(4)
                    END-IF
                  END-IF
                 ELSE
                  IF  WK-DAY-KOH-COMPFTN(3)  =  ALL"9"
                    IF  WK-KOH-COMPFTN(3)      < WK-DAY-KOH-COMPFTN(4)
                    AND LNK-SYU-KOH3HKNNUM   NOT =  SPACE
                        MOVE  WK-KOH-COMPFTN(3)
                                             TO  WK-DAY-KOH-COMPFTN(4)
                    END-IF
                  ELSE
                    IF  WK-DAY-KOH-COMPFTN(3)  < WK-DAY-KOH-COMPFTN(4)
                    AND LNK-SYU-KOH3HKNNUM   NOT =  SPACE
                        MOVE  WK-DAY-KOH-COMPFTN(3)
                                             TO  WK-DAY-KOH-COMPFTN(4)
                    END-IF
                  END-IF
                 END-IF
           END-EVALUATE.
      *
       23074-KOHHI-HIKAKU3-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *        ¡Ê±ßÃ±°Ì¡Ë
      ******************************************************************
       23075-KOHHI-HIKAKU4-SEC           SECTION.                             
      *
           IF    LNK-SYU-KOH-HKNNUM(KOH-IDX) =  "972"
             CONTINUE
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
       23075-KOHHI-HIKAKU4-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ÆüÊÌ¼ýÇ¼»»Äê
      ******************************************************************
       23071-DAYINF-SANTEI-SEC           SECTION.
      *
           MOVE       ZERO        TO       FLG-HIKOTEI.
           IF      WK-1001-PREFNUM       =   15
              IF   LNK-SYU-KOH-HKNNUM(KOH-IDX)  =
                      "161" OR "261" OR "361" OR "461" OR "561" OR
                      "163" OR "263" OR "165" OR "265" OR
                      "190" OR "290" OR
                      "164" OR "264" OR "364" OR "464" OR "564"
                 MOVE         1      TO      FLG-HIKOTEI
                 IF ((LNK-SYU-KOH-HKNNUM(1)  =  "027"     ) AND
                     (KOH-IDX                =   2        ))
                 OR ( KOH-IDX                =   1         )
      *
                    MOVE      2      TO      FLG-HIKOTEI
      *
                 END-IF
              END-IF
           END-IF.
      *
           MOVE    "1"    TO   LNK-SDAY-DAYINFFLG.
      *
           MOVE    ZERO   TO   WK-DAY-KOH-COMPFTN(KOH-IDX).
      *
           IF  WK-NYUKFTNRATE    NOT =  ZERO
              IF       WK-DAY-NYUKFTNRATE  =  ZERO
                    MOVE      WK-NYUKFTNRATE   TO   WK-DAY-NYUKFTNRATE
              ELSE
                 IF  WK-DAY-NYUKFTNRATE  > WK-NYUKFTNRATE
                    MOVE      WK-NYUKFTNRATE   TO   WK-DAY-NYUKFTNRATE
                 END-IF
              END-IF
           END-IF.
      *
           COMPUTE   WK-FUTANWARI  =
                                WK-DAY-NYUKFTNRATE  /  100.
      *
           PERFORM    VARYING   IDX2   FROM   1   BY   1
                         UNTIL    IDX2     >    31
      *
               IF    KOHI-TBL(IDX2 KOH-IDX)  =   9
      *
                 MOVE   99999    TO   WK-DAY-COMPFTN
      *
                 MOVE      1     TO    LNK-SDAY-SANTEIFLG(IDX2 KOH-IDX)
      *
                 MOVE   ZERO     TO    WK-TAIHI-SDAY-COMPFTN
                                       WK-TAIHI-SDAY-COMPFTNWK
                                       WK-TAIHI-SDAY-FTNMONEY
      *
                 IF     WK-DAY-NYUKFTNRATE    NOT =    ZERO
                    COMPUTE   WK-TAIHI-SDAY-COMPFTN =
                              LNK-SDAY-HKNTEN(IDX2) *
                              10                    *
                              WK-FUTANWARI
                    COMPUTE   WK-TAIHI-SDAY-COMPFTNWK =
                             (LNK-SDAY-HKNTEN(IDX2)   *
                              10                      *
                              WK-FUTANWARI         )  + 0.9
                 ELSE
                    COMPUTE   WK-TAIHI-SDAY-COMPFTN =
                              LNK-SDAY-HKNTEN(IDX2) *
                              10                     
                    COMPUTE   WK-TAIHI-SDAY-COMPFTNWK  =
                              LNK-SDAY-HKNTEN(IDX2)    *
                              10                     
                 END-IF
                 IF   ((KOH-IDX                =   1    ) AND
                       (LNK-SYU-SYUHKNNUM  NOT =   SPACE) AND
                       (LNK-SYU-SYUCOMPFTN     =   ZERO ))
                 OR   ((KOH-IDX                >   1    ) AND
                       (LNK-SYU-KOH-HKNNUM(KOH-IDX - 1)
                                           NOT =   SPACE) AND
                       (LNK-SYU-KOH-COMPFTN(KOH-IDX - 1)
                                               =   ZERO ))
                 OR   ((KOH-IDX                >   1    ) AND
                       (LNK-SYU-SYUHKNNUM  NOT =   SPACE) AND
                       (LNK-SYU-KOH-HKNNUM(1)
                                           NOT =   "027") AND
                       (LNK-SYU-SYUCOMPFTN     =   ZERO ))
                       MOVE   ZERO   TO  WK-TAIHI-SDAY-COMPFTN
                                         WK-TAIHI-SDAY-COMPFTNWK
                 END-IF
      *
                 IF ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"       ) OR
                    ( HKN-RECEFTNDSPKBN            =  "2"         ) OR
                    ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"    ) AND
                      (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  
                                                  "015" OR "016"  ) )
                     MOVE  "1"   TO    WK-TAIHI-RECEFTNDSPKBN(KOH-IDX)
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
      ***********    CONTINUE
                 ELSE
                     COMPUTE   WK-TAIHI-SDAY-COMPFTN  =
                               WK-TAIHI-SDAY-COMPFTN  +  5
                     COMPUTE   WK-FTN      =
                               WK-TAIHI-SDAY-COMPFTN  / 10
                     COMPUTE   WK-TAIHI-SDAY-COMPFTN  =
                               WK-FTN   *  10
                 END-IF
      *
                 IF      WK-TAIHI-SDAY-COMPFTNWK
                                          >   ZAN-KOHI-D3(IDX2 KOH-IDX)
                       MOVE   ZAN-KOHI-D3(IDX2 KOH-IDX)  TO
                                       WK-TAIHI-SDAY-COMPFTNWK
                 END-IF
      *
                 IF ( LNK-SYU-KOH-HKNNUM(KOH-IDX)  =  "051"       ) OR
                    ( HKN-RECEFTNDSPKBN            =  "2"         ) OR
                    ( (LNK-SYU-SRYYMD(1:6)        >=  "200404"    ) AND
                      (LNK-SYU-KOH-HKNNUM(KOH-IDX) =  
                                                  "015" OR "016"  ) )
                   IF    WK-TAIHI-SDAY-COMPFTN
                                          >   ZAN-KOHI-D2(IDX2 KOH-IDX)
                       MOVE   ZAN-KOHI-D2(IDX2 KOH-IDX)  TO
                                     WK-DAY-COMPFTN
                   END-IF
                 END-IF
      *
                 MOVE    ZAN-KOHI-D(IDX2 KOH-IDX)   TO   WK-ZAN-KOHI-D
      *
                 IF      WK-TAIHI-SDAY-COMPFTN
                                          >   ZAN-KOHI-D(IDX2 KOH-IDX)
                       MOVE   ZAN-KOHI-D(IDX2 KOH-IDX)   TO
                                    WK-TAIHI-SDAY-COMPFTN
                       MOVE   ZERO        TO  ZAN-KOHI-D(IDX2 KOH-IDX)
                 ELSE
                       COMPUTE  ZAN-KOHI-D(IDX2 KOH-IDX)    =
                                ZAN-KOHI-D(IDX2 KOH-IDX)    -
                                WK-TAIHI-SDAY-COMPFTN
                 END-IF
      *
                 IF    ZAN-KOHI-D(IDX2 KOH-IDX)   NOT =   ZERO
                     COMPUTE   ZAN-KOHI-D(IDX2 KOH-IDX) =
                               ZAN-KOHI-D(IDX2 KOH-IDX) +  5
                     COMPUTE   WK-FTN      =
                               ZAN-KOHI-D(IDX2 KOH-IDX) /  10
                     COMPUTE   ZAN-KOHI-D(IDX2 KOH-IDX) =
                               WK-FTN   *  10
                 END-IF
      *
                 IF   ( FLG-HIKOTEI            NOT =  ZERO )
                 AND  ( WK-TAIHI-SDAY-COMPFTN  NOT =  ZERO )
                    MOVE   ZAN-KOHI-D3(IDX2 KOH-IDX)  TO
                                 WK-TAIHI-SDAY-COMPFTNWK
                    MOVE   ZAN-KOHI-D2(IDX2 KOH-IDX)  TO
                                 WK-TAIHI-SDAY-COMPFTN
                    MOVE   WK-ZAN-KOHI-D              TO
                                 WK-TAIHI-SDAY-FTNMONEY
                    MOVE   ZERO        TO   ZAN-KOHI-D(IDX2 KOH-IDX)
                 ELSE
                    MOVE      WK-TAIHI-SDAY-COMPFTN
                                  TO   WK-TAIHI-SDAY-FTNMONEY
                    COMPUTE   WK-TAIHI-SDAY-FTNMONEY  =
                              WK-TAIHI-SDAY-FTNMONEY  +  5
                    COMPUTE   WK-FTN      =
                              WK-TAIHI-SDAY-FTNMONEY  / 10
                    COMPUTE   WK-TAIHI-SDAY-FTNMONEY  =
                              WK-FTN   *  10
      *
                    IF    WK-DAY-COMPFTN    =  ALL"9"
                          CONTINUE
                    ELSE
                          MOVE  WK-DAY-COMPFTN     TO   
                                     WK-TAIHI-SDAY-COMPFTN
                    END-IF
                 END-IF
      *
                 MOVE  WK-TAIHI-SDAY-COMPFTN  
                                   TO  LNK-SDAY-COMPFTN(IDX2 KOH-IDX)
                 MOVE  WK-TAIHI-SDAY-FTNMONEY
                                   TO  LNK-SDAY-FTNMONEY(IDX2 KOH-IDX)
                 MOVE  WK-TAIHI-SDAY-COMPFTNWK
                                   TO  LNK-SDAY-COMPFTNWK(IDX2 KOH-IDX)
      *
                 ADD   LNK-SDAY-COMPFTN(IDX2 KOH-IDX)  TO
                                 WK-DAY-KOH-COMPFTN(KOH-IDX)
      *
      *          ¼ýÇ¼¹àÌÜ
                 ADD   LNK-SDAY-FTNMONEY(IDX2 KOH-IDX)
                            TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
      *
                 ADD   LNK-SDAY-COMPFTNWK(IDX2 KOH-IDX)                 
                            TO   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
      *
               END-IF
      *
               EVALUATE   WK-NYUSKJRYYKBN
                     WHEN   "1"
                     WHEN   "2"
                     WHEN   "4"
                         MOVE  WK-DAY-SKJFTN(IDX2)
                                  TO   LNK-SDAY-SKJFTN(IDX2 KOH-IDX)
      *
                         IF   WK-NYUSKJJYOGAK  NOT =  ZERO
                            IF   WK-DAY-SKJFTN(IDX2)
                                              >   WK-NYUSKJJYOGAK
                               COMPUTE   LNK-SDAY-SKJFTN(IDX2 KOH-IDX) =
                                         LNK-SDAY-SKJFTN(IDX2 KOH-IDX) -
                                         WK-NYUSKJJYOGAK
                            ELSE
                               MOVE   ZERO   TO   
                                         LNK-SDAY-SKJFTN(IDX2 KOH-IDX)
                            END-IF
                         END-IF
                         IF   WK-NYUSKJRYYKBN  =  "2"
                           IF    LNK-SDAY-SKJFTN(IDX2 KOH-IDX)
                                         >   ZAN-KOHI-D(IDX2 KOH-IDX)
                              MOVE   ZAN-KOHI-D(IDX2 KOH-IDX)   TO
                                       LNK-SDAY-SKJFTN(IDX2 KOH-IDX)
                              MOVE   ZERO   TO  ZAN-KOHI-D(IDX2 KOH-IDX)
                           ELSE
                              COMPUTE  ZAN-KOHI-D(IDX2 KOH-IDX)   =
                                       ZAN-KOHI-D(IDX2 KOH-IDX)   -
                                       LNK-SDAY-SKJFTN(IDX2 KOH-IDX)
                           END-IF
                         END-IF
      *                  ¼ýÇ¼¹àÌÜ
                         ADD   LNK-SDAY-SKJFTN(IDX2 KOH-IDX)
                                  TO   LNK-SYU-SKJ-FTN(KOH-IDX)
                         ADD   WK-DAY-SKJFTN(IDX2)
                                  TO   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
                     WHEN   "3"
      *                  ¼ýÇ¼¹àÌÜ
                         ADD   WK-DAY-SKJFTN(IDX2)
                                  TO   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
               END-EVALUATE
      *
           END-PERFORM.
      *
      *    ¼ýÇ¼¹àÌÜ
      *
           MOVE     "1"             TO   LNK-SYU-DAYINFFLG.
      *
           MOVE  WK-NYUSKJRYYKBN    TO LNK-SYU-SKJ-FTNKBN(KOH-IDX).
           MOVE  WK-SHOKUJI-NISSU   TO LNK-SYU-SKJ-NISSU(KOH-IDX).
           MOVE  WK-SHOKUJI-RYOYOHI TO LNK-SYU-SKJ-RYOYOHI(KOH-IDX).
      *
           IF   LNK-S02-KOUREI-NYUIN-KBN     =   "1"                
                IF  (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027"       )
                AND ( (WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                     ((WK-1001-PREFNUM      =  20 OR  22 ) AND
                      (LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) = "0" OR "9" )))
                    COMPUTE  FTN-KOH-YYY  =  40200  -  FTN-KOH-XXX        
                    IF  LNK-SYU-KOH-COMPFTN(KOH-IDX)  >  FTN-KOH-YYY      
                      MOVE  FTN-KOH-YYY TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)  
                    END-IF
                    IF  WK-DAY-KOH-COMPFTN(KOH-IDX)   >  FTN-KOH-YYY      
                      MOVE  FTN-KOH-YYY TO  WK-DAY-KOH-COMPFTN(KOH-IDX)   
                    END-IF
      *
                    COMPUTE  FTN-KOH-YYY-ENTANI                         
                                    =  40200  -  FTN-KOH-XXX-ENTANI     
                    IF    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                       >    FTN-KOH-YYY-ENTANI          
                      MOVE  FTN-KOH-YYY-ENTANI TO                       
                                   LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)  
                    END-IF                                              
                END-IF
           END-IF.
      *
      ***     IF     FLG-HIKOTEI    =      2
      ***         IF  (LNK-SYU-KOH-HKNNUM(1)  =  "027"     ) AND
      ***             (KOH-IDX                =   2        )
      ***             IF  WK-DAY-KOH-COMPFTN(2) > LNK-SYU-KOH-COMPFTN(1)
      ***                 MOVE   WK-DAY-KOH-COMPFTN(2)
      ***                            TO  LNK-SYU-KOH-COMPFTN(1)
      ***                                LNK-SYU-KOH-COMPTOTAL(1)
      ***             END-IF
      ***             IF  LNK-SYU-KOH-COMPFTN-ENTANI(2)
      ***                                > LNK-SYU-KOH-COMPFTN-ENTANI(1)
      ***                 MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(2)
      ***                            TO  LNK-SYU-KOH-COMPFTN-ENTANI(1)
      ***             END-IF
      ***         END-IF
      ***         IF   KOH-IDX                =   1
      ***             IF    LNK-SYU-SYUHKNNUM  NOT =   SPACE
      ***               IF  WK-DAY-KOH-COMPFTN(1)  >  LNK-SYU-SYUCOMPFTN
      ***                 MOVE   WK-DAY-KOH-COMPFTN(1)
      ***                            TO  LNK-SYU-SYUCOMPFTN
      ***                                LNK-SYU-SYUCOMPTOTAL
      ***               END-IF
      ***               IF  LNK-SYU-KOH-COMPFTN-ENTANI(1)
      ***                                > LNK-SYU-SYUCOMPFTN-ENTANI
      ***                 MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(1)
      ***                            TO  LNK-SYU-SYUCOMPFTN-ENTANI
      ***               END-IF
      ***             END-IF
      ***         END-IF
      ***     END-IF.
      *
           PERFORM    23072-KOHHI-HIKAKU-SEC.
           PERFORM    23074-KOHHI-HIKAKU3-SEC.
           PERFORM    23075-KOHHI-HIKAKU4-SEC.                                
      *
           MOVE  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                                    TO LNK-SYU-KOH-COMPTOTAL(KOH-IDX).
      *
       23071-DAYINF-SANTEI-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ÆÃ¼ì·×»»¡Ê¸ÂÅÙ¤òÄ¶¤¨¤¿¾ì¹ç¡¢´µ¼ÔÉéÃ´È¯À¸¡ÊÁ´³Û¡Ë¡Ë
      ******************************************************************
       23076-TOKUSYU-SANTEI-SEC           SECTION.
      *
           IF    LNK-SYU-SYUCOMPFTN-ENTANI   NOT =    ZERO
                   IF    LNK-SYU-SYUCOMPFTN-ENTANI
                                             >=  ZAN-KOHI-T3(KOH-IDX)
                       MOVE   LNK-SYU-SYUCOMPFTN-ENTANI  TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   ELSE
                       MOVE   ZERO    TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   END-IF
                   PERFORM    23075-KOHHI-HIKAKU4-SEC
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
                   IF    LNK-SYU-KOH-COMPFTN-ENTANI(1)
                                             >=  ZAN-KOHI-T3(KOH-IDX)
                       MOVE   LNK-SYU-KOH-COMPFTN-ENTANI(1)  TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   ELSE
                       MOVE   ZERO    TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   END-IF
                   PERFORM    23075-KOHHI-HIKAKU4-SEC
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
                   IF    WK-ZENGAKU   >=  ZAN-KOHI-T3(KOH-IDX)
                       MOVE   WK-ZENGAKU  TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   ELSE
                       MOVE   ZERO    TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   END-IF
                   PERFORM    23075-KOHHI-HIKAKU4-SEC
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
                    IF  LNK-SYU-SYUCOMPFTN  >=  ZAN-KOHI-T2(KOH-IDX)
                       MOVE   LNK-SYU-SYUCOMPFTN  TO
                                         WK-KOH-COMPFTN(KOH-IDX)
                    ELSE
                       MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                    END-IF
                    PERFORM   23073-KOHHI-HIKAKU2-SEC
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
                    IF  LNK-SYU-KOH-COMPFTN(1)  >=  ZAN-KOHI-T2(KOH-IDX)
                       MOVE   LNK-SYU-KOH-COMPFTN(1)  TO
                                         WK-KOH-COMPFTN(KOH-IDX)
                    ELSE
                       MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                    END-IF
                    PERFORM   23073-KOHHI-HIKAKU2-SEC
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
                    IF  WK-ZENGAKU    >=    ZAN-KOHI-T2(KOH-IDX)
                       MOVE   WK-ZENGAKU  TO
                                         WK-KOH-COMPFTN(KOH-IDX)
                    ELSE
                       MOVE   ZERO   TO  WK-KOH-COMPFTN(KOH-IDX)
                    END-IF
                    PERFORM   23073-KOHHI-HIKAKU2-SEC
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
                  COMPUTE  WK-TOKUSYU =
                                    LNK-SYU-SYUCOMPFTN     +    5
                  COMPUTE  WK-TOKUSYU  =
                                    WK-TOKUSYU             /   10
                  COMPUTE  WK-TOKUSYU  =
                                    WK-TOKUSYU             *   10
      *
                  IF  WK-TOKUSYU   >=  ZAN-KOHI-T(KOH-IDX)
                      MOVE   LNK-SYU-SYUCOMPFTN  TO
                                        LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  ELSE
                      MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  END-IF
                  PERFORM    23072-KOHHI-HIKAKU-SEC
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
                  IF  WK-TOKUSYU  >=  ZAN-KOHI-T(KOH-IDX)
                      MOVE   LNK-SYU-KOH-COMPFTN(1)  TO
                                        LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  ELSE
                      MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  END-IF
                  PERFORM    23072-KOHHI-HIKAKU-SEC
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
                  IF  WK-TOKUSYU    >=    ZAN-KOHI-T(KOH-IDX)
                      MOVE   WK-ZENGAKU  TO
                                        LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  ELSE
                      MOVE   ZERO   TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  END-IF
                  PERFORM    23072-KOHHI-HIKAKU-SEC
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
           COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                     LNK-SYU-KOH-COMPFTN(KOH-IDX)
      *
           MOVE  WK-NYUSKJRYYKBN    TO LNK-SYU-SKJ-FTNKBN(KOH-IDX)
           MOVE  WK-SHOKUJI-NISSU   TO LNK-SYU-SKJ-NISSU(KOH-IDX)
           MOVE  WK-SHOKUJI-RYOYOHI TO LNK-SYU-SKJ-RYOYOHI(KOH-IDX)
      *
           EVALUATE   WK-NYUSKJRYYKBN
                WHEN   "1"
                WHEN   "2"
                WHEN   "4"
                       MOVE  WK-SHOKUJI-FTN
                                      TO   LNK-SYU-SKJ-FTN(KOH-IDX)
                                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
                       IF   WK-NYUSKJJYOGAK  NOT =  ZERO
                          COMPUTE   WK-NYUSKJJYOGAK2  =
                                    WK-NYUSKJJYOGAK * WK-SHOKUJI-NISSU
                          IF   LNK-SYU-SKJ-FTN(KOH-IDX)
                                              >   WK-NYUSKJJYOGAK2
                             COMPUTE   LNK-SYU-SKJ-FTN(KOH-IDX) =
                                       LNK-SYU-SKJ-FTN(KOH-IDX) -
                                       WK-NYUSKJJYOGAK2
                          ELSE
                             MOVE   ZERO   TO  
                                      LNK-SYU-SKJ-FTN(KOH-IDX) 
                          END-IF
                       END-IF
      ****             IF   LNK-SYU-SKJ-FTNKBN(KOH-IDX)  =  "2"
      ****               IF    LNK-SYU-SKJ-FTN(KOH-IDX)   >
      ****                                    ZAN-KOHI-T(KOH-IDX)
      ****                  MOVE   ZAN-KOHI-T(KOH-IDX)   TO
      ****                              LNK-SYU-SKJ-FTN(KOH-IDX)
      ****                  MOVE   ZERO      TO  ZAN-KOHI-T(KOH-IDX)
      ****               ELSE
      ****                  COMPUTE  ZAN-KOHI-T(KOH-IDX)    =
      ****                           ZAN-KOHI-T(KOH-IDX)    -
      ****                           LNK-SYU-SKJ-FTN(KOH-IDX)
      ****               END-IF
      ****             END-IF
                WHEN   "3"
                       MOVE  WK-SHOKUJI-FTN
                                      TO   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
           END-EVALUATE
      *
      *    ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *
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
      *
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
      *
      *    ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *    ¡Ê±ßÃ±°Ì¡Ë
      *
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
       23076-TOKUSYU-SANTEI-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ÆÃ¼ì·×»»¡Ê¸ÂÅÙ¤òÄ¶¤¨¤¿¾ì¹ç¡¢´µ¼ÔÉéÃ´È¯À¸¡ÊÄ¶¤¨¤¿³Û¡Ë¡Ë
      ******************************************************************
       23077-TOKUSYU-SANTEI2-SEC           SECTION.
      *
           IF    LNK-SYU-SYUCOMPFTN-ENTANI   NOT =    ZERO
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI  TO
                                     LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                   PERFORM    23075-KOHHI-HIKAKU4-SEC
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
                   PERFORM    23075-KOHHI-HIKAKU4-SEC
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
                   PERFORM    23075-KOHHI-HIKAKU4-SEC
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
                   MOVE  LNK-SYU-SYUCOMPFTN  TO  WK-KOH-COMPFTN(KOH-IDX)
                   PERFORM    23073-KOHHI-HIKAKU2-SEC
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
                   PERFORM    23073-KOHHI-HIKAKU2-SEC
                   IF    WK-KOH-COMPFTN(KOH-IDX) >  ZAN-KOHI-T2(KOH-IDX)
                       COMPUTE  WK-KOH-COMPFTN(KOH-IDX)  =
                                WK-KOH-COMPFTN(KOH-IDX)  -
                                ZAN-KOHI-T2(KOH-IDX)
                   ELSE
                       MOVE   ZERO    TO   WK-KOH-COMPFTN(KOH-IDX)
                   END-IF
                   MOVE   LNK-SYU-KOH-COMPFTN(1)  TO
                                           WK-TOKUSYU-COMPFTN2
                ELSE
                   MOVE  WK-ZENGAKU  TO    WK-KOH-COMPFTN(KOH-IDX)
                   PERFORM    23073-KOHHI-HIKAKU2-SEC
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
                  COMPUTE  WK-TOKUSYU =  LNK-SYU-SYUCOMPFTN  +   5
                  COMPUTE  WK-TOKUSYU =  WK-TOKUSYU          /  10
                  COMPUTE  WK-TOKUSYU =  WK-TOKUSYU          *  10
      *
                  MOVE  WK-TOKUSYU  TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  PERFORM    23072-KOHHI-HIKAKU-SEC
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
                  PERFORM    23072-KOHHI-HIKAKU-SEC
                  IF    LNK-SYU-KOH-COMPFTN(KOH-IDX)  >
                                            ZAN-KOHI-T(KOH-IDX)
                      COMPUTE  LNK-SYU-KOH-COMPFTN(KOH-IDX)  =
                               LNK-SYU-KOH-COMPFTN(KOH-IDX)  -
                               ZAN-KOHI-T(KOH-IDX)
                  ELSE
                      MOVE   ZERO    TO   LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  END-IF
                  MOVE   LNK-SYU-KOH-COMPFTN(1)  TO  WK-TOKUSYU-COMPFTN
              ELSE
                  COMPUTE  WK-TOKUSYU =  WK-ZENGAKU  +   5
                  COMPUTE  WK-TOKUSYU =  WK-TOKUSYU  /  10
                  COMPUTE  WK-TOKUSYU =  WK-TOKUSYU  *  10
      *
                  MOVE  WK-TOKUSYU  TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
                  PERFORM    23072-KOHHI-HIKAKU-SEC
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
           COMPUTE   LNK-SYU-KOH-COMPTOTAL(KOH-IDX)   =
                     LNK-SYU-KOH-COMPFTN(KOH-IDX)
      *
           MOVE  WK-NYUSKJRYYKBN    TO LNK-SYU-SKJ-FTNKBN(KOH-IDX)
           MOVE  WK-SHOKUJI-NISSU   TO LNK-SYU-SKJ-NISSU(KOH-IDX)
           MOVE  WK-SHOKUJI-RYOYOHI TO LNK-SYU-SKJ-RYOYOHI(KOH-IDX)
      *
           EVALUATE   WK-NYUSKJRYYKBN
                WHEN   "1"
                WHEN   "2"
                WHEN   "4"
                       MOVE  WK-SHOKUJI-FTN
                                      TO   LNK-SYU-SKJ-FTN(KOH-IDX)
                                           LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
                       IF   WK-NYUSKJJYOGAK  NOT =  ZERO
                          COMPUTE   WK-NYUSKJJYOGAK2  =
                                    WK-NYUSKJJYOGAK * WK-SHOKUJI-NISSU
                          IF   LNK-SYU-SKJ-FTN(KOH-IDX)
                                              >   WK-NYUSKJJYOGAK2
                             COMPUTE   LNK-SYU-SKJ-FTN(KOH-IDX) =
                                       LNK-SYU-SKJ-FTN(KOH-IDX) -
                                       WK-NYUSKJJYOGAK2
                          ELSE
                             MOVE   ZERO   TO  
                                      LNK-SYU-SKJ-FTN(KOH-IDX) 
                          END-IF
                       END-IF
      ****             IF   LNK-SYU-SKJ-FTNKBN(KOH-IDX)  =  "2"
      ****               IF    LNK-SYU-SKJ-FTN(KOH-IDX)   >
      ****                                    ZAN-KOHI-T(KOH-IDX)
      ****                  MOVE   ZAN-KOHI-T(KOH-IDX)   TO
      ****                              LNK-SYU-SKJ-FTN(KOH-IDX)
      ****                  MOVE   ZERO      TO  ZAN-KOHI-T(KOH-IDX)
      ****               ELSE
      ****                  COMPUTE  ZAN-KOHI-T(KOH-IDX)    =
      ****                           ZAN-KOHI-T(KOH-IDX)    -
      ****                           LNK-SYU-SKJ-FTN(KOH-IDX)
      ****               END-IF
      ****             END-IF
                WHEN   "3"
                       MOVE  WK-SHOKUJI-FTN
                                      TO   LNK-SYU-SKJ-FTN-RECE(KOH-IDX)
           END-EVALUATE
      *
      *    ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *
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
      *
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
      *
      *    ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤òÈæ³Ó
      *    ¡Ê±ßÃ±°Ì¡Ë
      *
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
       23077-TOKUSYU-SANTEI2-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ·§ËÜ¸©ÃÏÊý¸øÈñ¡Ê£±¡¿£³¡Ë·×»»
      ******************************************************************
       23081-KUMAMOTO-KOHI-SEC           SECTION.
      *
      *    ÉéÃ´¶â³Û¤Î·×»»
      *
           IF     LNK-SYU-SYUCOMPFTN  NOT  =  ZERO
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
           IF   LNK-S02-KOUREI-NYUIN-KBN     =   "1"
             IF  (  LNK-SYU-KOH-HKNNUM(KOH-IDX)  NOT =  "027"       )
             AND ( (WK-1001-PREFNUM  NOT =  20 AND 22 ) OR
                  ((WK-1001-PREFNUM      =  20 OR  22 ) AND
                   (LNK-SYU-KOH-HKNNUM(KOH-IDX)(1:1) = "0" OR "9" )))
               COMPUTE  FTN-KOH-YYY  =  40200  -  FTN-KOH-XXX
               IF  LNK-SYU-KOH-COMPFTN(KOH-IDX)  >  FTN-KOH-YYY
                 MOVE  FTN-KOH-YYY TO  LNK-SYU-KOH-COMPFTN(KOH-IDX)
               END-IF
      *
               COMPUTE  FTN-KOH-YYY-ENTANI
                                 =  40200  -  FTN-KOH-XXX-ENTANI
               IF    LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
                                  >    FTN-KOH-YYY-ENTANI
                 MOVE  FTN-KOH-YYY-ENTANI TO
                                LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX)
               END-IF
             END-IF
           END-IF
      *
           .
      *
       23081-KUMAMOTO-KOHI-EXT.
           EXIT.                                                        
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤«¤éºÇ¾®¤ÎÃÍ¤òÁªÂò¥»¥Ã¥È
      ******************************************************************
       2309-FTNKIN-HIKAKU-SEC         SECTION.
      *
           PERFORM   23091-FTNKIN-HIKAKU-ENTANI-SEC.                     
      *
           IF   COMB-KOH1HKNNUM        NOT   =   "027" 
           AND  COMB-HKNNUM            NOT   =   SPACE 
               IF   LNK-SYU-KOH1HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH1COMPFTN      >     LNK-SYU-SYUCOMPFTN
                   MOVE   LNK-SYU-SYUCOMPFTN  TO  LNK-SYU-KOH1COMPFTN
                   MOVE   LNK-SYU-KOH1COMPFTN TO  LNK-SYU-KOH1COMPTOTAL
                   IF    WK-KOH-COMPFTN(1)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO  WK-KOH-COMPFTN(1)
                   END-IF
                   IF    WK-DAY-KOH-COMPFTN(1)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO  WK-DAY-KOH-COMPFTN(1)
                   END-IF
               END-IF
               IF   LNK-SYU-KOH2HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH2COMPFTN      >     LNK-SYU-SYUCOMPFTN
                   MOVE   LNK-SYU-SYUCOMPFTN  TO  LNK-SYU-KOH2COMPFTN
                   MOVE   LNK-SYU-KOH2COMPFTN TO  LNK-SYU-KOH2COMPTOTAL
                   IF    WK-KOH-COMPFTN(2)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO  WK-KOH-COMPFTN(2)
                   END-IF
                   IF    WK-DAY-KOH-COMPFTN(2)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO  WK-DAY-KOH-COMPFTN(2)
                   END-IF
               END-IF
               IF   LNK-SYU-KOH3HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH3COMPFTN      >     LNK-SYU-SYUCOMPFTN
                   MOVE   LNK-SYU-SYUCOMPFTN  TO  LNK-SYU-KOH3COMPFTN
                   MOVE   LNK-SYU-KOH3COMPFTN TO  LNK-SYU-KOH3COMPTOTAL
                   IF    WK-KOH-COMPFTN(3)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO  WK-KOH-COMPFTN(3)
                   END-IF
                   IF    WK-DAY-KOH-COMPFTN(3)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO  WK-DAY-KOH-COMPFTN(3)
                   END-IF
               END-IF
               IF   LNK-SYU-KOH4HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH4COMPFTN      >     LNK-SYU-SYUCOMPFTN
                   MOVE   LNK-SYU-SYUCOMPFTN  TO  LNK-SYU-KOH4COMPFTN
                   MOVE   LNK-SYU-KOH4COMPFTN TO  LNK-SYU-KOH4COMPTOTAL
                   IF    WK-KOH-COMPFTN(4)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO  WK-KOH-COMPFTN(4)
                   END-IF
                   IF    WK-DAY-KOH-COMPFTN(4)  =  ALL"9"
                       CONTINUE
                   ELSE
                      MOVE  LNK-SYU-SYUCOMPFTN TO  WK-DAY-KOH-COMPFTN(4)
                   END-IF
               END-IF
           END-IF.
      *
      *    ÀÁµá³ÛÈæ³Ó
           MOVE    ZERO               TO    WK-KEISAN
                                            WK-KEISAN1
                                            WK-KEISAN2
                                            WK-KEISAN3
                                            WK-KEISAN4
                                            WK-KEISAN9.
           MOVE    ZERO               TO    WK-KEISAN-SKJ
                                            WK-KEISAN1-SKJ
                                            WK-KEISAN2-SKJ
                                            WK-KEISAN3-SKJ
                                            WK-KEISAN4-SKJ
                                            WK-KEISAN9-SKJ.
      *
           MOVE    ZERO               TO    FLG-HIKAKU.
      *
           MOVE    ZERO               TO    WK-KNJFTNKIN
                                            WK-KNJFTNKIN-SKJ.
      *    ¼çÊÝ¸±¤Î¤ß
           IF (COMB-KOH1HKNNUM              =    SPACE)    AND
              (COMB-KOH2HKNNUM              =    SPACE)    AND
              (COMB-KOH3HKNNUM              =    SPACE)    AND
              (COMB-KOH4HKNNUM              =    SPACE)
               MOVE     ZERO           TO  FLG-HIKAKU
      ***>     COMPUTE  WK-KEISAN9  =  LNK-SYU-SYUCOMPFTN  +
      ***>                             LNK-SYU-SYUSKJ-FTN
               MOVE   LNK-SYU-SYUCOMPFTN      TO   WK-KEISAN9
               MOVE   LNK-SYU-SYUSKJ-FTN      TO   WK-KEISAN9-SKJ
               MOVE   LNK-SYU-SYUCOMPFTN      TO   WK-KNJFTNKIN
               MOVE   LNK-SYU-SYUSKJ-FTN      TO   WK-KNJFTNKIN-SKJ 
           END-IF.
      *    ¸øÈñ£±¤Î¤ß
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              =     SPACE)   AND
              (COMB-KOH3HKNNUM              =     SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
               MOVE      1             TO  FLG-HIKAKU
      ***>     COMPUTE  WK-KEISAN9  =  LNK-SYU-KOH1COMPFTN  +
      ***>                             LNK-SYU-KOH1SKJ-FTN
               MOVE   LNK-SYU-KOH1COMPFTN     TO   WK-KEISAN9
               MOVE   LNK-SYU-KOH1SKJ-FTN     TO   WK-KEISAN9-SKJ
               MOVE   LNK-SYU-KOH1COMPFTN     TO   WK-KNJFTNKIN
               MOVE   LNK-SYU-KOH1SKJ-FTN     TO   WK-KNJFTNKIN-SKJ 
           END-IF.
      *    ¸øÈñ£±¤È¸øÈñ£²
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              =     SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
      *
      ***>     COMPUTE  WK-KEISAN1  =  LNK-SYU-KOH1COMPFTN  +
      ***>                             LNK-SYU-KOH1SKJ-FTN
      ***>     COMPUTE  WK-KEISAN2  =  LNK-SYU-KOH2COMPFTN  +
      ***>                             LNK-SYU-KOH2SKJ-FTN
               MOVE   LNK-SYU-KOH1COMPFTN     TO   WK-KEISAN1
               MOVE   LNK-SYU-KOH1SKJ-FTN     TO   WK-KEISAN1-SKJ
               MOVE   LNK-SYU-KOH2COMPFTN     TO   WK-KEISAN2
               MOVE   LNK-SYU-KOH2SKJ-FTN     TO   WK-KEISAN2-SKJ
               IF    WK-KEISAN1  <  WK-KEISAN2
                   MOVE      1                     TO  FLG-HIKAKU
                   MOVE    WK-KEISAN1              TO  WK-KEISAN9
                   MOVE    LNK-SYU-KOH1COMPFTN     TO  WK-KNJFTNKIN
      ***>         MOVE    LNK-SYU-KOH1SKJ-FTN     TO  WK-KNJFTNKIN-SKJ
               ELSE
                   MOVE      2                     TO  FLG-HIKAKU
                   MOVE    WK-KEISAN2              TO  WK-KEISAN9
                   MOVE    LNK-SYU-KOH2COMPFTN     TO  WK-KNJFTNKIN
      ***>         MOVE    LNK-SYU-KOH2SKJ-FTN     TO  WK-KNJFTNKIN-SKJ
               END-IF
               IF    WK-KEISAN1-SKJ  <  WK-KEISAN2-SKJ
                   MOVE    WK-KEISAN1-SKJ          TO  WK-KEISAN9-SKJ
                   MOVE    LNK-SYU-KOH1SKJ-FTN     TO  WK-KNJFTNKIN-SKJ
               ELSE
                   MOVE    WK-KEISAN2-SKJ          TO  WK-KEISAN9-SKJ
                   MOVE    LNK-SYU-KOH2SKJ-FTN     TO  WK-KNJFTNKIN-SKJ
               END-IF
           END-IF.
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
      *
      ***>     COMPUTE  WK-KEISAN1  =  LNK-SYU-KOH1COMPFTN  +
      ***>                             LNK-SYU-KOH1SKJ-FTN
      ***>     COMPUTE  WK-KEISAN2  =  LNK-SYU-KOH2COMPFTN  +
      ***>                             LNK-SYU-KOH2SKJ-FTN
      ***>     COMPUTE  WK-KEISAN3  =  LNK-SYU-KOH3COMPFTN  +
      ***>                             LNK-SYU-KOH3SKJ-FTN
               MOVE   LNK-SYU-KOH1COMPFTN     TO   WK-KEISAN1
               MOVE   LNK-SYU-KOH1SKJ-FTN     TO   WK-KEISAN1-SKJ
               MOVE   LNK-SYU-KOH2COMPFTN     TO   WK-KEISAN2
               MOVE   LNK-SYU-KOH2SKJ-FTN     TO   WK-KEISAN2-SKJ
               MOVE   LNK-SYU-KOH3COMPFTN     TO   WK-KEISAN3
               MOVE   LNK-SYU-KOH3SKJ-FTN     TO   WK-KEISAN3-SKJ
               IF    WK-KEISAN1  <  WK-KEISAN2
                   IF    WK-KEISAN1  <  WK-KEISAN3
                      MOVE      1                  TO  FLG-HIKAKU
                      MOVE    WK-KEISAN1           TO  WK-KEISAN9
                      MOVE    LNK-SYU-KOH1COMPFTN  TO  WK-KNJFTNKIN
      ***>            MOVE    LNK-SYU-KOH1SKJ-FTN  TO  WK-KNJFTNKIN-SKJ
                   ELSE
                      MOVE      3                  TO  FLG-HIKAKU
                      MOVE    WK-KEISAN3           TO  WK-KEISAN9
                      MOVE    LNK-SYU-KOH3COMPFTN  TO  WK-KNJFTNKIN
      ***>            MOVE    LNK-SYU-KOH3SKJ-FTN  TO  WK-KNJFTNKIN-SKJ 
                   END-IF
               ELSE
                   IF    WK-KEISAN2  <  WK-KEISAN3
                      MOVE      2                  TO  FLG-HIKAKU
                      MOVE    WK-KEISAN2           TO  WK-KEISAN9
                      MOVE    LNK-SYU-KOH2COMPFTN  TO  WK-KNJFTNKIN
      ***>            MOVE    LNK-SYU-KOH2SKJ-FTN  TO  WK-KNJFTNKIN-SKJ 
                   ELSE
                      MOVE      3                  TO  FLG-HIKAKU
                      MOVE    WK-KEISAN3           TO  WK-KEISAN9
                      MOVE    LNK-SYU-KOH3COMPFTN  TO  WK-KNJFTNKIN
      ***>            MOVE    LNK-SYU-KOH3SKJ-FTN  TO  WK-KNJFTNKIN-SKJ 
                   END-IF
               END-IF
               IF    WK-KEISAN1-SKJ  <  WK-KEISAN2-SKJ
                   IF    WK-KEISAN1-SKJ  <  WK-KEISAN3-SKJ
                      MOVE    WK-KEISAN1-SKJ       TO  WK-KEISAN9-SKJ
                      MOVE    LNK-SYU-KOH1SKJ-FTN  TO  WK-KNJFTNKIN-SKJ
                   ELSE
                      MOVE    WK-KEISAN3-SKJ       TO  WK-KEISAN9-SKJ
                      MOVE    LNK-SYU-KOH3SKJ-FTN  TO  WK-KNJFTNKIN-SKJ 
                   END-IF
               ELSE
                   IF    WK-KEISAN2-SKJ  <  WK-KEISAN3-SKJ
                      MOVE    WK-KEISAN2-SKJ       TO  WK-KEISAN9-SKJ
                      MOVE    LNK-SYU-KOH2SKJ-FTN  TO  WK-KNJFTNKIN-SKJ 
                   ELSE
                      MOVE    WK-KEISAN3-SKJ       TO  WK-KEISAN9-SKJ
                      MOVE    LNK-SYU-KOH3SKJ-FTN  TO  WK-KNJFTNKIN-SKJ 
                   END-IF
               END-IF
           END-IF.
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³¤È¸øÈñ£´
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              NOT = SPACE)
      *
      ***>     COMPUTE  WK-KEISAN1  =  LNK-SYU-KOH1COMPFTN  +
      ***>                             LNK-SYU-KOH1SKJ-FTN
      ***>     COMPUTE  WK-KEISAN2  =  LNK-SYU-KOH2COMPFTN  +
      ***>                             LNK-SYU-KOH2SKJ-FTN
      ***>     COMPUTE  WK-KEISAN3  =  LNK-SYU-KOH3COMPFTN  +
      ***>                             LNK-SYU-KOH3SKJ-FTN
      ***>     COMPUTE  WK-KEISAN4  =  LNK-SYU-KOH4COMPFTN  +
      ***>                             LNK-SYU-KOH4SKJ-FTN
               MOVE   LNK-SYU-KOH1COMPFTN     TO   WK-KEISAN1
               MOVE   LNK-SYU-KOH1SKJ-FTN     TO   WK-KEISAN1-SKJ
               MOVE   LNK-SYU-KOH2COMPFTN     TO   WK-KEISAN2
               MOVE   LNK-SYU-KOH2SKJ-FTN     TO   WK-KEISAN2-SKJ
               MOVE   LNK-SYU-KOH3COMPFTN     TO   WK-KEISAN3
               MOVE   LNK-SYU-KOH3SKJ-FTN     TO   WK-KEISAN3-SKJ
               MOVE   LNK-SYU-KOH4COMPFTN     TO   WK-KEISAN4
               MOVE   LNK-SYU-KOH4SKJ-FTN     TO   WK-KEISAN4-SKJ
               IF    WK-KEISAN1  <  WK-KEISAN2
                   IF    WK-KEISAN1  <  WK-KEISAN3
                      IF    WK-KEISAN1  <  WK-KEISAN4
                        MOVE      1                 TO  FLG-HIKAKU
                        MOVE    WK-KEISAN1          TO  WK-KEISAN9
                        MOVE    LNK-SYU-KOH1COMPFTN TO  WK-KNJFTNKIN
      ***>              MOVE    LNK-SYU-KOH1SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      ELSE
                        MOVE      4                 TO  FLG-HIKAKU
                        MOVE    WK-KEISAN4          TO  WK-KEISAN9
                        MOVE    LNK-SYU-KOH4COMPFTN TO  WK-KNJFTNKIN
      ***>              MOVE    LNK-SYU-KOH4SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      END-IF
                   ELSE
                      IF    WK-KEISAN3  <  WK-KEISAN4
                        MOVE      3                 TO  FLG-HIKAKU
                        MOVE    WK-KEISAN3          TO  WK-KEISAN9
                        MOVE    LNK-SYU-KOH3COMPFTN TO  WK-KNJFTNKIN
      ***>              MOVE    LNK-SYU-KOH3SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      ELSE
                        MOVE      4                 TO  FLG-HIKAKU
                        MOVE    WK-KEISAN4          TO  WK-KEISAN9
                        MOVE    LNK-SYU-KOH4COMPFTN TO  WK-KNJFTNKIN
      ***>              MOVE    LNK-SYU-KOH4SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      END-IF
                   END-IF
               ELSE
                   IF    WK-KEISAN2  <  WK-KEISAN3
                      IF    WK-KEISAN2  <  WK-KEISAN4
                        MOVE      2                 TO  FLG-HIKAKU
                        MOVE    WK-KEISAN2          TO  WK-KEISAN9
                        MOVE    LNK-SYU-KOH2COMPFTN TO  WK-KNJFTNKIN
     ***>               MOVE    LNK-SYU-KOH2SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      ELSE
                        MOVE      4                 TO  FLG-HIKAKU
                        MOVE    WK-KEISAN4          TO  WK-KEISAN9
                        MOVE    LNK-SYU-KOH4COMPFTN TO  WK-KNJFTNKIN
     ***>               MOVE    LNK-SYU-KOH4SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      END-IF
                   ELSE
                      IF    WK-KEISAN3  <  WK-KEISAN4
                        MOVE      3                 TO  FLG-HIKAKU
                        MOVE    WK-KEISAN3          TO  WK-KEISAN9
                        MOVE    LNK-SYU-KOH3COMPFTN TO  WK-KNJFTNKIN
     ***>               MOVE    LNK-SYU-KOH3SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      ELSE
                        MOVE      4                 TO  FLG-HIKAKU
                        MOVE    WK-KEISAN4          TO  WK-KEISAN9
                        MOVE    LNK-SYU-KOH4COMPFTN TO  WK-KNJFTNKIN
     ***>               MOVE    LNK-SYU-KOH4SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      END-IF
                   END-IF
               END-IF
               IF    WK-KEISAN1-SKJ  <  WK-KEISAN2-SKJ
                   IF    WK-KEISAN1-SKJ  <  WK-KEISAN3-SKJ
                      IF    WK-KEISAN1-SKJ  <  WK-KEISAN4-SKJ
                        MOVE    WK-KEISAN1-SKJ      TO  WK-KEISAN9-SKJ
                        MOVE    LNK-SYU-KOH1SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      ELSE
                        MOVE    WK-KEISAN4-SKJ      TO  WK-KEISAN9-SKJ
                        MOVE    LNK-SYU-KOH4SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      END-IF
                   ELSE
                      IF    WK-KEISAN3-SKJ  <  WK-KEISAN4-SKJ
                        MOVE    WK-KEISAN3-SKJ      TO  WK-KEISAN9-SKJ
                        MOVE    LNK-SYU-KOH3SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      ELSE
                        MOVE    WK-KEISAN4-SKJ      TO  WK-KEISAN9-SKJ
                        MOVE    LNK-SYU-KOH4SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      END-IF
                   END-IF
               ELSE
                   IF    WK-KEISAN2-SKJ  <  WK-KEISAN3-SKJ
                      IF    WK-KEISAN2-SKJ  <  WK-KEISAN4-SKJ
                        MOVE    WK-KEISAN2-SKJ      TO  WK-KEISAN9-SKJ
                        MOVE    LNK-SYU-KOH2SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      ELSE
                        MOVE    WK-KEISAN4-SKJ      TO  WK-KEISAN9-SKJ
                        MOVE    LNK-SYU-KOH4SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      END-IF
                   ELSE
                      IF    WK-KEISAN3-SKJ  <  WK-KEISAN4-SKJ
                        MOVE    WK-KEISAN3-SKJ      TO  WK-KEISAN9-SKJ
                        MOVE    LNK-SYU-KOH3SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      ELSE
                        MOVE    WK-KEISAN4-SKJ      TO  WK-KEISAN9-SKJ
                        MOVE    LNK-SYU-KOH4SKJ-FTN TO  WK-KNJFTNKIN-SKJ 
                      END-IF
                   END-IF
               END-IF
           END-IF.
      *
           IF   COMB-KOH1HKNNUM        NOT   =   "027" 
           AND  COMB-HKNNUM            NOT   =   SPACE 
      *
      ***>     COMPUTE  WK-KEISAN   =  LNK-SYU-SYUCOMPFTN  +
      ***>                             LNK-SYU-SYUSKJ-FTN
               MOVE   LNK-SYU-SYUCOMPFTN      TO   WK-KEISAN
               MOVE   LNK-SYU-SYUSKJ-FTN      TO   WK-KEISAN-SKJ
               IF   WK-KEISAN  <  WK-KEISAN9
                   MOVE    ZERO                 TO   FLG-HIKAKU
                   MOVE    LNK-SYU-SYUCOMPFTN   TO   WK-KNJFTNKIN
      ***>         MOVE    LNK-SYU-SYUSKJ-FTN   TO   WK-KNJFTNKIN-SKJ 
               END-IF
               IF   WK-KEISAN-SKJ  <  WK-KEISAN9-SKJ
                   MOVE    LNK-SYU-SYUSKJ-FTN   TO   WK-KNJFTNKIN-SKJ 
               END-IF
           END-IF.
      *
      *¿©»öÎÅÍÜÈñÈæ³Ó
           IF  ( COMB-KOH1HKNNUM          =   "027" )  OR
               ( COMB-HKNNUM              =   SPACE ) 
             MOVE        LNK-SYU-KOH1SKJ-RYOYOHI TO  WK-RYOYOHI-SKJ 
             IF   LNK-SYU-KOH2HKNNUM   NOT =  SPACE
               IF  LNK-SYU-KOH2SKJ-RYOYOHI <  WK-RYOYOHI-SKJ
                   MOVE  LNK-SYU-KOH2SKJ-RYOYOHI TO  WK-RYOYOHI-SKJ
               END-IF
             END-IF
             IF   LNK-SYU-KOH3HKNNUM   NOT =  SPACE
               IF  LNK-SYU-KOH3SKJ-RYOYOHI <  WK-RYOYOHI-SKJ
                   MOVE  LNK-SYU-KOH3SKJ-RYOYOHI TO  WK-RYOYOHI-SKJ          
               END-IF
             END-IF
             IF   LNK-SYU-KOH4HKNNUM   NOT =  SPACE
               IF  LNK-SYU-KOH4SKJ-RYOYOHI <  WK-RYOYOHI-SKJ
                   MOVE  LNK-SYU-KOH4SKJ-RYOYOHI TO  WK-RYOYOHI-SKJ          
               END-IF
             END-IF
           ELSE
             MOVE        LNK-SYU-SYUSKJ-RYOYOHI  TO  WK-RYOYOHI-SKJ 
             IF   LNK-SYU-KOH1HKNNUM   NOT =  SPACE
               IF  LNK-SYU-KOH1SKJ-RYOYOHI <  WK-RYOYOHI-SKJ
                   MOVE  LNK-SYU-KOH1SKJ-RYOYOHI TO  WK-RYOYOHI-SKJ
               END-IF 
             END-IF
             IF   LNK-SYU-KOH2HKNNUM   NOT =  SPACE
               IF  LNK-SYU-KOH2SKJ-RYOYOHI <  WK-RYOYOHI-SKJ
                   MOVE  LNK-SYU-KOH2SKJ-RYOYOHI TO  WK-RYOYOHI-SKJ
               END-IF
             END-IF
             IF   LNK-SYU-KOH3HKNNUM   NOT =  SPACE
               IF  LNK-SYU-KOH3SKJ-RYOYOHI <  WK-RYOYOHI-SKJ
                   MOVE  LNK-SYU-KOH3SKJ-RYOYOHI TO  WK-RYOYOHI-SKJ
               END-IF
             END-IF 
             IF   LNK-SYU-KOH4HKNNUM   NOT =  SPACE
               IF  LNK-SYU-KOH4SKJ-RYOYOHI <  WK-RYOYOHI-SKJ
                   MOVE  LNK-SYU-KOH4SKJ-RYOYOHI TO  WK-RYOYOHI-SKJ
               END-IF
             END-IF
           END-IF.
      *
      *    ¼ýÇ¼¥Æ¡¼¥Ö¥ë¤ÎÀÁµá³Û¤ò¥»¥Ã¥È
           MOVE    WK-KNJFTNKIN             TO  LNK-SYU-SKYMONEY.
      *    ¿©»öÎÅÍÜÈñ¡¦¿©»öÉéÃ´³Û¤ò¥»¥Ã¥È
           MOVE    WK-RYOYOHI-SKJ           TO  LNK-SYU-RYOYOHI-SKJ.
           MOVE    WK-KNJFTNKIN-SKJ         TO  LNK-SYU-SKYMONEY-SKJ.
           IF   LNK-SYU-SYUSKJ-FTNKBN  =  "4"
             IF    COMB-SYU-HKNKOHSBTKBN  =  "8" 
               IF    COMB-HOJOKBN           =  "1" 
                 CONTINUE
               ELSE
                 IF    WK-TAX1001-KBN  =  "1"
                    COMPUTE LNK-SYU-SKYMONEY-SKJ-TAX   =
                          ( LNK-SYU-SKYMONEY-SKJ  *   0.05 )  +  0.5
                 ELSE
                    COMPUTE LNK-SYU-SKYMONEY-SKJ-TAX   =
                            LNK-SYU-SKYMONEY-SKJ  *   0.05          
                 END-IF
               END-IF
             END-IF
           END-IF.
           COMPUTE  LNK-SYU-SKYMONEY-SKJ-KEI = LNK-SYU-SKYMONEY-SKJ
                                             + LNK-SYU-SKYMONEY-SKJ-TAX.
      *    ÆÃÄê¼À´µ¤¬¤¢¤ëÊý¤ÎÆÃÎã½èÍý
           IF  ((LNK-SYU-KOH1HKNNUM    =  "051") AND (FLG-HIKAKU = 1))
           OR  ((LNK-SYU-KOH2HKNNUM    =  "051") AND (FLG-HIKAKU = 2))
           OR  ((LNK-SYU-KOH3HKNNUM    =  "051") AND (FLG-HIKAKU = 3))
           OR  ((LNK-SYU-KOH4HKNNUM    =  "051") AND (FLG-HIKAKU = 4))
             IF    LNK-SYU-SRYYMD(1:6)   <  "200310"
               COMPUTE   WK-FTN   =   LNK-SYU-SKYMONEY  
                                  +   LNK-SYU-SKYMONEY-SKJ-KEI
               COMPUTE   WK-FTN   =   WK-FTN     +   5
               COMPUTE   WK-FTN   =   WK-FTN     /  10
               COMPUTE   WK-FTN   =   WK-FTN     *  10
               IF        WK-FTN    >=  14000
                 COMPUTE  LNK-SYU-SKYMONEY  =  LNK-SYU-SKYMONEY    
                                            +  LNK-SYU-SKYMONEY-SKJ-KEI
                 MOVE      ZERO            TO  LNK-SYU-SKYMONEY-SKJ
                                               LNK-SYU-SKYMONEY-SKJ-TAX
                                               LNK-SYU-SKYMONEY-SKJ-KEI
               END-IF
             ELSE
               COMPUTE   WK-FTN   =   LNK-SYU-SKYMONEY  
                                  +   LNK-SYU-SKYMONEY-SKJ-KEI
               COMPUTE   WK-FTN   =   WK-FTN     +   5
               COMPUTE   WK-FTN   =   WK-FTN     /  10
               COMPUTE   WK-FTN   =   WK-FTN     *  10
               IF        WK-FTN    >=  WK-TAIHI-NYUJGNGAK(FLG-HIKAKU)
                 COMPUTE  LNK-SYU-SKYMONEY  =  LNK-SYU-SKYMONEY    
                                            +  LNK-SYU-SKYMONEY-SKJ-KEI
                 MOVE      ZERO            TO  LNK-SYU-SKYMONEY-SKJ
                                               LNK-SYU-SKYMONEY-SKJ-TAX
                                               LNK-SYU-SKYMONEY-SKJ-KEI
               END-IF
             END-IF
           END-IF.
           IF    LNK-SYU-SRYYMD(1:6)   <   "200304"
               CONTINUE
           ELSE
      *      ¹¹Àµ¤¬¤¢¤ëÊý¤ÎÆÃÎã½èÍý
             IF ((LNK-SYU-KOH1HKNNUM    =  "015") AND (FLG-HIKAKU = 1))
             OR ((LNK-SYU-KOH2HKNNUM    =  "015") AND (FLG-HIKAKU = 2))
             OR ((LNK-SYU-KOH3HKNNUM    =  "015") AND (FLG-HIKAKU = 3))
             OR ((LNK-SYU-KOH4HKNNUM    =  "015") AND (FLG-HIKAKU = 4))
               COMPUTE   WK-FTN   =   LNK-SYU-SKYMONEY  
                                  +   LNK-SYU-SKYMONEY-SKJ-KEI
               COMPUTE   WK-FTN   =   WK-FTN     +   5
               COMPUTE   WK-FTN   =   WK-FTN     /  10
               COMPUTE   WK-FTN   =   WK-FTN     *  10
               IF        WK-FTN    >=  WK-TAIHI-NYUJGNGAK(FLG-HIKAKU)
                 COMPUTE  LNK-SYU-SKYMONEY  =  LNK-SYU-SKYMONEY    
                                            +  LNK-SYU-SKYMONEY-SKJ-KEI
                 MOVE      ZERO            TO  LNK-SYU-SKYMONEY-SKJ
                                               LNK-SYU-SKYMONEY-SKJ-TAX
                                               LNK-SYU-SKYMONEY-SKJ-KEI
               END-IF
             END-IF
      *      °éÀ®¤¬¤¢¤ëÊý¤ÎÆÃÎã½èÍý
             IF ((LNK-SYU-KOH1HKNNUM    =  "016") AND (FLG-HIKAKU = 1))
             OR ((LNK-SYU-KOH2HKNNUM    =  "016") AND (FLG-HIKAKU = 2))
             OR ((LNK-SYU-KOH3HKNNUM    =  "016") AND (FLG-HIKAKU = 3))
             OR ((LNK-SYU-KOH4HKNNUM    =  "016") AND (FLG-HIKAKU = 4))
               COMPUTE   WK-FTN   =   LNK-SYU-SKYMONEY  
                                  +   LNK-SYU-SKYMONEY-SKJ-KEI
               COMPUTE   WK-FTN   =   WK-FTN     +   5
               COMPUTE   WK-FTN   =   WK-FTN     /  10
               COMPUTE   WK-FTN   =   WK-FTN     *  10
               IF        WK-FTN    >=  WK-TAIHI-NYUJGNGAK(FLG-HIKAKU)
                 COMPUTE  LNK-SYU-SKYMONEY  =  LNK-SYU-SKYMONEY    
                                            +  LNK-SYU-SKYMONEY-SKJ-KEI
                 MOVE      ZERO            TO  LNK-SYU-SKYMONEY-SKJ
                                               LNK-SYU-SKYMONEY-SKJ-TAX
                                               LNK-SYU-SKYMONEY-SKJ-KEI
               END-IF
             END-IF
           END-IF.
      *
           IF    LNK-SYU-SRYYMD(1:6)   <   "200310"
               CONTINUE
           ELSE
      *      ÅìµþÅÔÃÏÊý¸øÈñ£±£µ£±¤¬¤¢¤ëÊý¤ÎÆÃÎã½èÍý
             IF   WK-1001-PREFNUM    =   13
               IF ((LNK-SYU-KOH1HKNNUM = "151" OR "451") 
                                              AND (FLG-HIKAKU = 1))
               OR ((LNK-SYU-KOH2HKNNUM = "151" OR "451")
                                              AND (FLG-HIKAKU = 2))
               OR ((LNK-SYU-KOH3HKNNUM = "151" OR "451")
                                              AND (FLG-HIKAKU = 3))
               OR ((LNK-SYU-KOH4HKNNUM = "151" OR "451")
                                              AND (FLG-HIKAKU = 4))
                COMPUTE   WK-FTN   =   LNK-SYU-SKYMONEY  
                                   +   LNK-SYU-SKYMONEY-SKJ-KEI
                COMPUTE   WK-FTN   =   WK-FTN     +   5
                COMPUTE   WK-FTN   =   WK-FTN     /  10
                COMPUTE   WK-FTN   =   WK-FTN     *  10
                IF        WK-FTN    >=  WK-TAIHI-NYUJGNGAK(FLG-HIKAKU)
                  COMPUTE  LNK-SYU-SKYMONEY  =  LNK-SYU-SKYMONEY    
                                             +  LNK-SYU-SKYMONEY-SKJ-KEI
                  MOVE      ZERO            TO  LNK-SYU-SKYMONEY-SKJ
                                                LNK-SYU-SKYMONEY-SKJ-TAX
                                                LNK-SYU-SKYMONEY-SKJ-KEI
                END-IF
               END-IF
             END-IF
           END-IF.
      *
           IF    LNK-SYU-SRYYMD(1:6)   <   "200310"
               CONTINUE
           ELSE
      *      Ê¼¸Ë¸©ÃÏÊý¸øÈñ£±£¸£¶¤¬¤¢¤ëÊý¤ÎÆÃÎã½èÍý
             IF   WK-1001-PREFNUM    =   28
               IF ((LNK-SYU-KOH1HKNNUM = "186") AND (FLG-HIKAKU = 1))
               OR ((LNK-SYU-KOH2HKNNUM = "186") AND (FLG-HIKAKU = 2))
               OR ((LNK-SYU-KOH3HKNNUM = "186") AND (FLG-HIKAKU = 3))
               OR ((LNK-SYU-KOH4HKNNUM = "186") AND (FLG-HIKAKU = 4))
                COMPUTE   WK-FTN   =   LNK-SYU-SKYMONEY  
                                   +   LNK-SYU-SKYMONEY-SKJ-KEI
                COMPUTE   WK-FTN   =   WK-FTN     +   5
                COMPUTE   WK-FTN   =   WK-FTN     /  10
                COMPUTE   WK-FTN   =   WK-FTN     *  10
                IF        WK-FTN    >=  WK-TAIHI-NYUJGNGAK(FLG-HIKAKU)
                  COMPUTE  LNK-SYU-SKYMONEY  =  LNK-SYU-SKYMONEY    
                                             +  LNK-SYU-SKYMONEY-SKJ-KEI
                  MOVE      ZERO            TO  LNK-SYU-SKYMONEY-SKJ
                                                LNK-SYU-SKYMONEY-SKJ-TAX
                                                LNK-SYU-SKYMONEY-SKJ-KEI
                END-IF
               END-IF
             END-IF
           END-IF.
      *
           IF    LNK-SYU-SRYYMD(1:6)   <   "200504"
               CONTINUE
           ELSE
      *      ¾®»ùÆÃÄê¤¬¤¢¤ëÊý¤ÎÆÃÎã½èÍý
             IF ((LNK-SYU-KOH1HKNNUM    =  "052") AND (FLG-HIKAKU = 1))
             OR ((LNK-SYU-KOH2HKNNUM    =  "052") AND (FLG-HIKAKU = 2))
             OR ((LNK-SYU-KOH3HKNNUM    =  "052") AND (FLG-HIKAKU = 3))
             OR ((LNK-SYU-KOH4HKNNUM    =  "052") AND (FLG-HIKAKU = 4))
               COMPUTE   WK-FTN   =   LNK-SYU-SKYMONEY  
                                  +   LNK-SYU-SKYMONEY-SKJ-KEI
               COMPUTE   WK-FTN   =   WK-FTN     +   5
               COMPUTE   WK-FTN   =   WK-FTN     /  10
               COMPUTE   WK-FTN   =   WK-FTN     *  10
               IF        WK-FTN    >=  WK-TAIHI-NYUJGNGAK(FLG-HIKAKU)
                 COMPUTE  LNK-SYU-SKYMONEY  =  LNK-SYU-SKYMONEY    
                                            +  LNK-SYU-SKYMONEY-SKJ-KEI
                 MOVE      ZERO            TO  LNK-SYU-SKYMONEY-SKJ
                                               LNK-SYU-SKYMONEY-SKJ-TAX
                                               LNK-SYU-SKYMONEY-SKJ-KEI
               END-IF
             END-IF
           END-IF.
      *
           COMPUTE  LNK-SYU-RYOYOHI-SKJ-JIHI  = WK-SHOKUJI-RYOYOHI95
                                              + WK-SHOKUJI-RYOYOHI96.
           COMPUTE  LNK-SYU-SKYMONEY-SKJ-JIHI = WK-SHOKUJI-FTN95
                                              + WK-SHOKUJI-FTN96.
           IF  ( COMB-SYU-HKNKOHSBTKBN  =  "8" ) AND
               ( COMB-HOJOKBN           =  "1" )
             CONTINUE
           ELSE
             IF    WK-TAX1001-KBN  =  "1"
                 COMPUTE  LNK-SYU-SKYMONEY-SKJ-JIHI-TAX  =
                        ( WK-SHOKUJI-FTN96  *  0.05 )  +  0.5
             ELSE
                 COMPUTE  LNK-SYU-SKYMONEY-SKJ-JIHI-TAX  =
                          WK-SHOKUJI-FTN96  *  0.05          
             END-IF
           END-IF.
           COMPUTE  LNK-SYU-SKYMONEY-SKJ-JIHI-KEI
                                      = LNK-SYU-SKYMONEY-SKJ-JIHI
                                      + LNK-SYU-SKYMONEY-SKJ-JIHI-TAX.
      *
      *    ¼ýÇ¼¥Æ¡¼¥Ö¥ë¤Î¸øÈñËè¤Î´µ¼ÔÉéÃ´¶â³Û¤ò¥»¥Ã¥È¤¹¤ë
           IF   (COMB-KOH1HKNNUM         NOT = SPACE)   AND
                (COMB-KOH1-NYUJGNKBN     =     "1")
             IF  ( LNK-S02-KOUREI-NYUIN-KBN     =        "1"  )
             AND ( COMB-KOH1HKNNUM              =       "027" )
                  CONTINUE
             ELSE
                    COMPUTE  LNK-SYU-KOH1HKNFTNMONEY  =                  
                                    LNK-SYU-KOH1COMPFTN     +   5        
                    COMPUTE  LNK-SYU-KOH1HKNFTNMONEY  =                  
                                    LNK-SYU-KOH1HKNFTNMONEY /  10        
                    COMPUTE  LNK-SYU-KOH1HKNFTNMONEY  =                  
                                    LNK-SYU-KOH1HKNFTNMONEY *  10        
             END-IF
           END-IF.
           IF   (COMB-KOH2HKNNUM        NOT = SPACE)   AND
                (COMB-KOH2-NYUJGNKBN    =     "1")
                    COMPUTE  LNK-SYU-KOH2HKNFTNMONEY  =                  
                                    LNK-SYU-KOH2COMPFTN     +   5        
                    COMPUTE  LNK-SYU-KOH2HKNFTNMONEY  =                  
                                    LNK-SYU-KOH2HKNFTNMONEY /  10        
                    COMPUTE  LNK-SYU-KOH2HKNFTNMONEY  =                  
                                    LNK-SYU-KOH2HKNFTNMONEY *  10        
           END-IF.
           IF   (COMB-KOH3HKNNUM        NOT = SPACE)   AND
                (COMB-KOH3-NYUJGNKBN    =     "1")
                    COMPUTE  LNK-SYU-KOH3HKNFTNMONEY  =                  
                                    LNK-SYU-KOH3COMPFTN     +   5        
                    COMPUTE  LNK-SYU-KOH3HKNFTNMONEY  =                  
                                    LNK-SYU-KOH3HKNFTNMONEY /  10        
                    COMPUTE  LNK-SYU-KOH3HKNFTNMONEY  =                  
                                    LNK-SYU-KOH3HKNFTNMONEY *  10        
           END-IF.
           IF   (COMB-KOH4HKNNUM        NOT = SPACE)   AND
                (COMB-KOH4-NYUJGNKBN    =     "1")
                    COMPUTE  LNK-SYU-KOH4HKNFTNMONEY  =                  
                                    LNK-SYU-KOH4COMPFTN     +   5        
                    COMPUTE  LNK-SYU-KOH4HKNFTNMONEY  =                  
                                    LNK-SYU-KOH4HKNFTNMONEY /  10        
                    COMPUTE  LNK-SYU-KOH4HKNFTNMONEY  =                  
                                    LNK-SYU-KOH4HKNFTNMONEY *  10        
           END-IF.
      *
      *    ¥ì¥»µ­ºÜ¶â³ÛÂàÈòÍÑ¥¨¥ê¥¢¤«¤é¼ýÇ¼¹àÌÜ¤ØÊÔ½¸
      *
           IF    WK-KOH-COMPFTN(1)  =  ALL"9"                           
      ****             CONTINUE                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(1) = SPACE
                AND  COMB-KOH1HKNNUM       NOT = "027"
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH1COMPTOTAL   =
                             LNK-SYU-KOH1COMPFTN
                END-IF
           ELSE                                                         
                MOVE  WK-KOH-COMPFTN(1)  TO   LNK-SYU-KOH1COMPFTN       
                COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
                          LNK-SYU-KOH1COMPFTN     
           END-IF.                                                      
           IF    WK-DAY-KOH-COMPFTN(1)  =  ALL"9"
      ****             CONTINUE                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(1) = SPACE
                AND  COMB-KOH1HKNNUM       NOT = "027"
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH1COMPTOTAL   =
                             LNK-SYU-KOH1COMPFTN
                END-IF
           ELSE                                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(1) = SPACE
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    WK-DAY-KOH-COMPFTN(1)   +   5
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH1COMPFTN  =
                                    LNK-SYU-KOH1COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH1COMPTOTAL   =
                             LNK-SYU-KOH1COMPFTN
                ELSE
                    MOVE     WK-DAY-KOH-COMPFTN(1)
                                         TO   LNK-SYU-KOH1COMPFTN
                    COMPUTE  LNK-SYU-KOH1COMPTOTAL   =
                             LNK-SYU-KOH1COMPFTN
                END-IF
      ****      MOVE  WK-DAY-KOH-COMPFTN(1)  TO   LNK-SYU-KOH1COMPFTN
      ****      COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
      ****                LNK-SYU-KOH1COMPFTN
           END-IF.                   
      *
           IF    WK-KOH-COMPFTN(2)  =  ALL"9"                           
      ****             CONTINUE                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(2) = SPACE
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH2COMPTOTAL   =
                             LNK-SYU-KOH2COMPFTN
                END-IF
           ELSE                                                         
                MOVE  WK-KOH-COMPFTN(2)  TO   LNK-SYU-KOH2COMPFTN       
                COMPUTE   LNK-SYU-KOH2COMPTOTAL   =
                          LNK-SYU-KOH2COMPFTN     
           END-IF.                                                      
           IF    WK-DAY-KOH-COMPFTN(2)  =  ALL"9"
      ****             CONTINUE                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(2) = SPACE
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH2COMPTOTAL   =
                             LNK-SYU-KOH2COMPFTN
                END-IF
           ELSE                                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(2) = SPACE
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    WK-DAY-KOH-COMPFTN(2)   +   5
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH2COMPFTN  =
                                    LNK-SYU-KOH2COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH2COMPTOTAL   =
                             LNK-SYU-KOH2COMPFTN
                ELSE
                    MOVE     WK-DAY-KOH-COMPFTN(2)
                                         TO   LNK-SYU-KOH2COMPFTN
                    COMPUTE  LNK-SYU-KOH2COMPTOTAL   =
                             LNK-SYU-KOH2COMPFTN
                END-IF
      ****      MOVE  WK-DAY-KOH-COMPFTN(2)  TO   LNK-SYU-KOH2COMPFTN
      ****      COMPUTE   LNK-SYU-KOH2COMPTOTAL   =
      ****                LNK-SYU-KOH2COMPFTN
           END-IF.                   
      *
           IF    WK-KOH-COMPFTN(3)  =  ALL"9"                           
      ****             CONTINUE                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(3) = SPACE
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH3COMPTOTAL   =
                             LNK-SYU-KOH3COMPFTN
                END-IF
           ELSE                                                         
                MOVE  WK-KOH-COMPFTN(3)  TO   LNK-SYU-KOH3COMPFTN       
                COMPUTE   LNK-SYU-KOH3COMPTOTAL   =
                          LNK-SYU-KOH3COMPFTN     
           END-IF.                                                      
           IF    WK-DAY-KOH-COMPFTN(3)  =  ALL"9"
      ****             CONTINUE                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(3) = SPACE
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH3COMPTOTAL   =
                             LNK-SYU-KOH3COMPFTN
                END-IF
           ELSE                                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(3) = SPACE
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    WK-DAY-KOH-COMPFTN(3)   +   5
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH3COMPFTN  =
                                    LNK-SYU-KOH3COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH3COMPTOTAL   =
                             LNK-SYU-KOH3COMPFTN
                ELSE
                    MOVE     WK-DAY-KOH-COMPFTN(3)
                                         TO   LNK-SYU-KOH3COMPFTN
                    COMPUTE  LNK-SYU-KOH3COMPTOTAL   =
                             LNK-SYU-KOH3COMPFTN
                END-IF
      ****      MOVE  WK-DAY-KOH-COMPFTN(3)  TO   LNK-SYU-KOH3COMPFTN
      ****      COMPUTE   LNK-SYU-KOH3COMPTOTAL   =
      ****                LNK-SYU-KOH3COMPFTN
           END-IF.                   
      *
           IF    WK-KOH-COMPFTN(4)  =  ALL"9"                           
      ****             CONTINUE                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(4) = SPACE
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH4COMPTOTAL   =
                             LNK-SYU-KOH4COMPFTN
                END-IF
           ELSE                                                         
                MOVE  WK-KOH-COMPFTN(4)  TO   LNK-SYU-KOH4COMPFTN       
                COMPUTE   LNK-SYU-KOH4COMPTOTAL   =
                          LNK-SYU-KOH4COMPFTN     
           END-IF.                                                      
           IF    WK-DAY-KOH-COMPFTN(4)  =  ALL"9"
      ****             CONTINUE                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(4) = SPACE
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     +   5
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH4COMPTOTAL   =
                             LNK-SYU-KOH4COMPFTN
                END-IF
           ELSE                                                         
                IF   WK-TAIHI-RECEFTNDSPKBN(4) = SPACE
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    WK-DAY-KOH-COMPFTN(4)   +   5
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     /  10
                    COMPUTE  LNK-SYU-KOH4COMPFTN  =
                                    LNK-SYU-KOH4COMPFTN     *  10
                    COMPUTE  LNK-SYU-KOH4COMPTOTAL   =
                             LNK-SYU-KOH4COMPFTN
                ELSE
                    MOVE     WK-DAY-KOH-COMPFTN(4)
                                         TO   LNK-SYU-KOH4COMPFTN
                    COMPUTE  LNK-SYU-KOH4COMPTOTAL   =
                             LNK-SYU-KOH4COMPFTN
                END-IF
      ****      MOVE  WK-DAY-KOH-COMPFTN(4)  TO   LNK-SYU-KOH4COMPFTN
      ****      COMPUTE   LNK-SYU-KOH4COMPTOTAL   =
      ****                LNK-SYU-KOH4COMPFTN
           END-IF.                   
      *
      *    ÆÃ¼ì·×»»ÍÑ¶â³ÛÂàÈòÊÔ½¸
           PERFORM   23092-TOKUSYU-HENSYU-SEC.
      *
      *    ¼çÊÝ¸±¡¦Ï·¿ÍÊÝ¸±¤Î¶â³ÛÃÖ¤­´¹¤¨
           IF   COMB-HKNNUM            NOT =   SPACE
           AND  LNK-SYU-SYUCOMPFTN     NOT =    ZERO
           AND  FLG-KOUGAKUCYO         NOT =     1  
      *
                 COMPUTE   LNK-SYU-SYUCOMPFTN  =
                           LNK-SYU-SYUCOMPFTN  +  5
                 COMPUTE   WK-FTN      =
                           LNK-SYU-SYUCOMPFTN  / 10
                 COMPUTE   LNK-SYU-SYUCOMPFTN  =
                           WK-FTN   *  10
      *
           END-IF.
      *
           IF   COMB-KOH1HKNNUM            =   "027"
           AND  LNK-SYU-KOH1COMPFTN    NOT =    ZERO
           AND  FLG-KOUGAKUCYO         NOT =     1  
      *
                 COMPUTE   LNK-SYU-KOH1COMPFTN  =
                           LNK-SYU-KOH1COMPFTN  +  5
                 COMPUTE   WK-FTN      =
                           LNK-SYU-KOH1COMPFTN  / 10
                 COMPUTE   LNK-SYU-KOH1COMPFTN  =
                           WK-FTN   *  10
      *
           END-IF.
      *
      *    £´£±Ï·¿ÍÍÑ
           IF   FLG-KOUGAKUCYO2        NOT =    ZERO
              IF ((FLG-IRYOHI2 = 1) AND (FLG-HIKAKU = 1))
              OR ((FLG-IRYOHI2 = 2) AND (FLG-HIKAKU = 2))
              OR ((FLG-IRYOHI2 = 3) AND (FLG-HIKAKU = 3))
              OR ((FLG-IRYOHI2 = 4) AND (FLG-HIKAKU = 4))
                IF   LNK-SYU-SKYMONEY   >   72300
                  CONTINUE
                ELSE
                  MOVE    ZERO     TO    FLG-KOUGAKUCYO2
                END-IF
              ELSE
                  MOVE    ZERO     TO    FLG-KOUGAKUCYO2
              END-IF
           END-IF.
      *
      *    ÆüÊÌ¥Ç¡¼¥¿ÃÖ¤­´¹¤¨ÊÔ½¸½èÍý
           PERFORM   23093-DAYDATA-OKIKAE-HENSYU-SEC.
      *
           IF   FLG-SYUHEN                 =   "1"
           AND  LNK-S02-TEIKI-KBN          =   "X"
      *
              IF   COMB-HKNNUM            NOT =   SPACE
              AND  LNK-SYU-SYUCOMPFTN     NOT =   WK-HEN-SYUCOMPFTN
      *
                 IF   COMB-KOH1-NYUJGNKBN     =     "1"
                    IF   LNK-SYU-KOH1HKNFTNMONEY  >  WK-HEN-SYUCOMPFTN
                       MOVE  LNK-SYU-KOH1HKNFTNMONEY
                                                 TO  LNK-SYU-SYUCOMPFTN
                       COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
                                 LNK-SYU-SYUCOMPFTN
                    ELSE
                       MOVE  WK-HEN-SYUCOMPFTN   TO  LNK-SYU-SYUCOMPFTN
                       COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
                                 LNK-SYU-SYUCOMPFTN
                    END-IF
                 ELSE
                    IF   LNK-SYU-KOH1COMPFTN  >  WK-HEN-SYUCOMPFTN
                       COMPUTE   LNK-SYU-SYUCOMPFTN   =
                                 LNK-SYU-KOH1COMPFTN  +   5
                       COMPUTE   WK-FTN               =
                                 LNK-SYU-SYUCOMPFTN   /  10
                       COMPUTE   LNK-SYU-SYUCOMPFTN   =
                                 WK-FTN               *  10
                       COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
                                 LNK-SYU-SYUCOMPFTN
                    ELSE
                       MOVE  WK-HEN-SYUCOMPFTN   TO  LNK-SYU-SYUCOMPFTN
                       COMPUTE   LNK-SYU-SYUCOMPTOTAL   =
                                 LNK-SYU-SYUCOMPFTN
                    END-IF
                 END-IF
      *
              END-IF
      *
              IF   COMB-KOH1HKNNUM            =   "027"
              AND  LNK-SYU-KOH1COMPFTN    NOT =   WK-HEN-KOH1COMPFTN
      *
                 IF   COMB-KOH2-NYUJGNKBN     =     "1"
                    IF   LNK-SYU-KOH2HKNFTNMONEY  >  WK-HEN-KOH1COMPFTN
                       MOVE  LNK-SYU-KOH2HKNFTNMONEY
                                                 TO  LNK-SYU-KOH1COMPFTN
                       COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
                                 LNK-SYU-KOH1COMPFTN
                    ELSE
                       MOVE  WK-HEN-KOH1COMPFTN  TO  LNK-SYU-KOH1COMPFTN
                       COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
                                 LNK-SYU-KOH1COMPFTN
                    END-IF
                 ELSE
                    IF   LNK-SYU-KOH2COMPFTN  >  WK-HEN-KOH1COMPFTN
                       COMPUTE   LNK-SYU-KOH1COMPFTN  =
                                 LNK-SYU-KOH2COMPFTN  +   5
                       COMPUTE   WK-FTN               =
                                 LNK-SYU-KOH1COMPFTN  /  10
                       COMPUTE   LNK-SYU-KOH1COMPFTN  =
                                 WK-FTN               *  10
                       COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
                                 LNK-SYU-KOH1COMPFTN
                    ELSE
                       MOVE  WK-HEN-KOH1COMPFTN  TO  LNK-SYU-KOH1COMPFTN
                       COMPUTE   LNK-SYU-KOH1COMPTOTAL   =
                                 LNK-SYU-KOH1COMPFTN
                    END-IF
                 END-IF
      *
              END-IF
      *
           END-IF.
      *
       2309-FTNKIN-HIKAKU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ¸øÈñÃ±°Ì¤Ç·×»»¤·¤¿´µ¼ÔÉéÃ´¶â¤«¤éºÇ¾®¤ÎÃÍ¤òÁªÂò¥»¥Ã¥È
      *        ¡Ê±ßÃ±°Ì¡Ë
      ******************************************************************
       23091-FTNKIN-HIKAKU-ENTANI-SEC         SECTION.                   
      *
           IF   COMB-KOH1HKNNUM        NOT   =   "027" 
           AND  COMB-HKNNUM            NOT   =   SPACE 
               IF   LNK-SYU-KOH1HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH1COMPFTN-ENTANI  >
                                        LNK-SYU-SYUCOMPFTN-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI  
                                    TO  LNK-SYU-KOH1COMPFTN-ENTANI
               END-IF
               IF   LNK-SYU-KOH2HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH2COMPFTN-ENTANI  >
                                        LNK-SYU-SYUCOMPFTN-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                    TO  LNK-SYU-KOH2COMPFTN-ENTANI
               END-IF
               IF   LNK-SYU-KOH3HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH3COMPFTN-ENTANI  >
                                        LNK-SYU-SYUCOMPFTN-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                    TO  LNK-SYU-KOH3COMPFTN-ENTANI
               END-IF
               IF   LNK-SYU-KOH4HKNNUM   NOT =  SPACE
               AND  LNK-SYU-KOH4COMPFTN-ENTANI  >
                                        LNK-SYU-SYUCOMPFTN-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                    TO  LNK-SYU-KOH4COMPFTN-ENTANI
               END-IF
           END-IF.
      *
      *    ÀÁµá³ÛÈæ³Ó
           MOVE    ZERO               TO    WK-KEISAN-ENTANI
                                            WK-KEISAN1-ENTANI
                                            WK-KEISAN2-ENTANI
                                            WK-KEISAN3-ENTANI
                                            WK-KEISAN4-ENTANI
                                            WK-KEISAN9-ENTANI.
      *
           MOVE    ZERO               TO    WK-KNJFTNKIN-ENTANI.
      *
      *    ¼çÊÝ¸±¤Î¤ß
           IF (COMB-KOH1HKNNUM              =    SPACE)    AND
              (COMB-KOH2HKNNUM              =    SPACE)    AND
              (COMB-KOH3HKNNUM              =    SPACE)    AND
              (COMB-KOH4HKNNUM              =    SPACE)
      ***>     COMPUTE  WK-KEISAN9-ENTANI
      ***>                          =  LNK-SYU-SYUCOMPFTN-ENTANI  +
      ***>                             LNK-SYU-SYUSKJ-FTN
               MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                            TO   WK-KEISAN9-ENTANI
               MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                            TO   WK-KNJFTNKIN-ENTANI
           END-IF.
      *    ¸øÈñ£±¤Î¤ß
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              =     SPACE)   AND
              (COMB-KOH3HKNNUM              =     SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
      ***>     COMPUTE  WK-KEISAN9-ENTANI
      ***>                          =  LNK-SYU-KOH1COMPFTN-ENTANI +
      ***>                             LNK-SYU-KOH1SKJ-FTN
               MOVE   LNK-SYU-KOH1COMPFTN-ENTANI
                                            TO   WK-KEISAN9-ENTANI
               MOVE   LNK-SYU-KOH1COMPFTN-ENTANI
                                            TO   WK-KNJFTNKIN-ENTANI
           END-IF.
      *    ¸øÈñ£±¤È¸øÈñ£²
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              =     SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
      *
      ***>     COMPUTE  WK-KEISAN1-ENTANI
      ***>                          =  LNK-SYU-KOH1COMPFTN-ENTANI +
      ***>                             LNK-SYU-KOH1SKJ-FTN
      ***>     COMPUTE  WK-KEISAN2-ENTANI
      ***>                          =  LNK-SYU-KOH2COMPFTN-ENTANI +
      ***>                             LNK-SYU-KOH2SKJ-FTN
               MOVE   LNK-SYU-KOH1COMPFTN-ENTANI
                                            TO   WK-KEISAN1-ENTANI
               MOVE   LNK-SYU-KOH2COMPFTN-ENTANI
                                            TO   WK-KEISAN2-ENTANI
               IF    WK-KEISAN1-ENTANI  <  WK-KEISAN2-ENTANI
                   MOVE    WK-KEISAN1-ENTANI  TO  WK-KEISAN9-ENTANI
                   MOVE    LNK-SYU-KOH1COMPFTN-ENTANI
                                          TO  WK-KNJFTNKIN-ENTANI
               ELSE
                   MOVE    WK-KEISAN2-ENTANI  TO  WK-KEISAN9-ENTANI
                   MOVE    LNK-SYU-KOH2COMPFTN-ENTANI
                                          TO  WK-KNJFTNKIN-ENTANI
               END-IF
           END-IF.
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              =     SPACE)
      *
      ***>     COMPUTE  WK-KEISAN1-ENTANI
      ***>                          =  LNK-SYU-KOH1COMPFTN-ENTANI  +
      ***>                             LNK-SYU-KOH1SKJ-FTN
      ***>     COMPUTE  WK-KEISAN2-ENTANI
      ***>                          =  LNK-SYU-KOH2COMPFTN-ENTANI  +
      ***>                             LNK-SYU-KOH2SKJ-FTN
      ***>     COMPUTE  WK-KEISAN3-ENTANI
      ***>                          =  LNK-SYU-KOH3COMPFTN-ENTANI  +
      ***>                             LNK-SYU-KOH3SKJ-FTN
               MOVE   LNK-SYU-KOH1COMPFTN-ENTANI
                                            TO   WK-KEISAN1-ENTANI
               MOVE   LNK-SYU-KOH2COMPFTN-ENTANI
                                            TO   WK-KEISAN2-ENTANI
               MOVE   LNK-SYU-KOH3COMPFTN-ENTANI
                                            TO   WK-KEISAN3-ENTANI
               IF    WK-KEISAN1-ENTANI  <  WK-KEISAN2-ENTANI
                   IF    WK-KEISAN1-ENTANI  <  WK-KEISAN3-ENTANI
                      MOVE    WK-KEISAN1-ENTANI TO  WK-KEISAN9-ENTANI
                      MOVE    LNK-SYU-KOH1COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                   ELSE
                      MOVE    WK-KEISAN3-ENTANI TO  WK-KEISAN9-ENTANI
                      MOVE    LNK-SYU-KOH3COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                   END-IF
               ELSE
                   IF    WK-KEISAN2-ENTANI  <  WK-KEISAN3-ENTANI
                      MOVE    WK-KEISAN2-ENTANI TO  WK-KEISAN9-ENTANI
                      MOVE    LNK-SYU-KOH2COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                   ELSE
                      MOVE    WK-KEISAN3-ENTANI TO  WK-KEISAN9-ENTANI
                      MOVE    LNK-SYU-KOH3COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                   END-IF
               END-IF
           END-IF.
      *    ¸øÈñ£±¤È¸øÈñ£²¤È¸øÈñ£³¤È¸øÈñ£´
           IF (COMB-KOH1HKNNUM              NOT = SPACE)   AND
              (COMB-KOH2HKNNUM              NOT = SPACE)   AND
              (COMB-KOH3HKNNUM              NOT = SPACE)   AND
              (COMB-KOH4HKNNUM              NOT = SPACE)
      *
      ***>     COMPUTE  WK-KEISAN1-ENTANI
      ***>                          =  LNK-SYU-KOH1COMPFTN-ENTANI  +
      ***>                             LNK-SYU-KOH1SKJ-FTN
      ***>     COMPUTE  WK-KEISAN2-ENTANI
      ***>                          =  LNK-SYU-KOH2COMPFTN-ENTANI  +
      ***>                             LNK-SYU-KOH2SKJ-FTN
      ***>     COMPUTE  WK-KEISAN3-ENTANI
      ***>                          =  LNK-SYU-KOH3COMPFTN-ENTANI  +
      ***>                             LNK-SYU-KOH3SKJ-FTN
      ***>     COMPUTE  WK-KEISAN4-ENTANI
      ***>                          =  LNK-SYU-KOH4COMPFTN-ENTANI  +
      ***>                             LNK-SYU-KOH4SKJ-FTN
               MOVE   LNK-SYU-KOH1COMPFTN-ENTANI
                                            TO   WK-KEISAN1-ENTANI
               MOVE   LNK-SYU-KOH2COMPFTN-ENTANI
                                            TO   WK-KEISAN2-ENTANI
               MOVE   LNK-SYU-KOH3COMPFTN-ENTANI
                                            TO   WK-KEISAN3-ENTANI
               MOVE   LNK-SYU-KOH4COMPFTN-ENTANI
                                            TO   WK-KEISAN4-ENTANI
               IF    WK-KEISAN1-ENTANI  <  WK-KEISAN2-ENTANI
                   IF    WK-KEISAN1-ENTANI  <  WK-KEISAN3-ENTANI
                      IF    WK-KEISAN1-ENTANI  <  WK-KEISAN4-ENTANI
                        MOVE    WK-KEISAN1-ENTANI TO  WK-KEISAN9-ENTANI
                        MOVE    LNK-SYU-KOH1COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                      ELSE
                        MOVE    WK-KEISAN4-ENTANI TO  WK-KEISAN9-ENTANI
                        MOVE    LNK-SYU-KOH4COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                      END-IF
                   ELSE
                      IF    WK-KEISAN3-ENTANI  <  WK-KEISAN4-ENTANI
                        MOVE    WK-KEISAN3-ENTANI TO  WK-KEISAN9-ENTANI
                        MOVE    LNK-SYU-KOH3COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                      ELSE
                        MOVE    WK-KEISAN4-ENTANI TO  WK-KEISAN9-ENTANI
                        MOVE    LNK-SYU-KOH4COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                      END-IF
                   END-IF
               ELSE
                   IF    WK-KEISAN2-ENTANI  <  WK-KEISAN3-ENTANI
                      IF    WK-KEISAN2-ENTANI  <  WK-KEISAN4-ENTANI
                        MOVE    WK-KEISAN2-ENTANI TO  WK-KEISAN9-ENTANI
                        MOVE    LNK-SYU-KOH2COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                      ELSE
                        MOVE    WK-KEISAN4-ENTANI TO  WK-KEISAN9-ENTANI
                        MOVE    LNK-SYU-KOH4COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                      END-IF
                   ELSE
                      IF    WK-KEISAN3-ENTANI  <  WK-KEISAN4-ENTANI
                        MOVE    WK-KEISAN3-ENTANI TO  WK-KEISAN9-ENTANI
                        MOVE    LNK-SYU-KOH3COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                      ELSE
                        MOVE    WK-KEISAN4-ENTANI TO  WK-KEISAN9-ENTANI
                        MOVE    LNK-SYU-KOH4COMPFTN-ENTANI
                                           TO  WK-KNJFTNKIN-ENTANI
                      END-IF
                   END-IF
               END-IF
           END-IF.
      *
           IF   COMB-KOH1HKNNUM        NOT   =   "027" 
           AND  COMB-HKNNUM            NOT   =   SPACE 
      *
      ***>     COMPUTE  WK-KEISAN-ENTANI
      ***>                          =  LNK-SYU-SYUCOMPFTN-ENTANI  +
      ***>                             LNK-SYU-SYUSKJ-FTN
               MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                            TO   WK-KEISAN-ENTANI
               IF   WK-KEISAN-ENTANI  <  WK-KEISAN9-ENTANI
                   MOVE   LNK-SYU-SYUCOMPFTN-ENTANI
                                  TO   WK-KNJFTNKIN-ENTANI
               END-IF
           END-IF.
      *
       23091-FTNKIN-HIKAKU-ENTANI-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÆÃ¼ì·×»»ÍÑ¶â³ÛÂàÈòÊÔ½¸
      ******************************************************************
       23092-TOKUSYU-HENSYU-SEC         SECTION.
      *
           IF  (WK-1001-PREFNUM             =  07      )
           AND (LNK-SYU-SRYYMD(1:6)        >= "200412" )
           AND ((LNK-SYU-KOH1HKNNUM         = "245"   ) OR
                (LNK-SYU-KOH2HKNNUM         = "245"   ) OR
                (LNK-SYU-KOH3HKNNUM         = "245"   ) OR
                (LNK-SYU-KOH4HKNNUM         = "245"   ))
      *
                    COMPUTE  LNK-SYU-SHOKUJI5  =
                                    WK-TOKUSYU-COMPFTN     +    5
                    COMPUTE  LNK-SYU-SHOKUJI5  =
                                    LNK-SYU-SHOKUJI5       /   10
                    COMPUTE  LNK-SYU-SHOKUJI5  =
                                    LNK-SYU-SHOKUJI5       *   10
      *
                    MOVE  WK-TOKUSYU-COMPFTN   TO   LNK-SYU-SHOKUJI6
                    IF    WK-TOKUSYU-COMPFTN2  =  9999999
                       IF ((LNK-SYU-KOH1HKNNUM        = "245") AND
                           (WK-TAIHI-RECEFTNDSPKBN(1) = SPACE))
                       OR ((LNK-SYU-KOH2HKNNUM        = "245") AND
                           (WK-TAIHI-RECEFTNDSPKBN(2) = SPACE))
                       OR ((LNK-SYU-KOH3HKNNUM        = "245") AND
                           (WK-TAIHI-RECEFTNDSPKBN(3) = SPACE))
                       OR ((LNK-SYU-KOH4HKNNUM        = "245") AND
                           (WK-TAIHI-RECEFTNDSPKBN(4) = SPACE))
                           COMPUTE  LNK-SYU-SHOKUJI6  =
                                    LNK-SYU-SHOKUJI6       +    5
                           COMPUTE  LNK-SYU-SHOKUJI6  =
                                    LNK-SYU-SHOKUJI6       /   10
                           COMPUTE  LNK-SYU-SHOKUJI6  =
                                    LNK-SYU-SHOKUJI6       *   10
                       END-IF
                    ELSE
                       MOVE  WK-TOKUSYU-COMPFTN2  TO   LNK-SYU-SHOKUJI6
                    END-IF
      *
      *****         MOVE     WK-TOKUSYU-ENTANI    TO   LNK-SYU-SHOKUJI7
                    COMPUTE  LNK-SYU-SHOKUJI7  =
                                            WK-TOKUSYU-ENTANI  + 0.9
      *
           END-IF
      *
           IF  (WK-1001-PREFNUM             =  33      )
           AND (LNK-SYU-SRYYMD(1:6)        >= "200110" )
           AND ((LNK-SYU-KOH1HKNNUM         = "285"   ) OR
                (LNK-SYU-KOH2HKNNUM         = "285"   ) OR
                (LNK-SYU-KOH3HKNNUM         = "285"   ) OR
                (LNK-SYU-KOH4HKNNUM         = "285"   ))
      *
                    COMPUTE  LNK-SYU-SHOKUJI5  =
                                    WK-TOKUSYU-COMPFTN     +    5
                    COMPUTE  LNK-SYU-SHOKUJI5  =
                                    LNK-SYU-SHOKUJI5       /   10
                    COMPUTE  LNK-SYU-SHOKUJI5  =
                                    LNK-SYU-SHOKUJI5       *   10
      *
                    MOVE  WK-TOKUSYU-COMPFTN   TO   LNK-SYU-SHOKUJI6
                    IF    WK-TOKUSYU-COMPFTN2  =  9999999
                       IF ((LNK-SYU-KOH1HKNNUM        = "285") AND
                           (WK-TAIHI-RECEFTNDSPKBN(1) = SPACE))
                       OR ((LNK-SYU-KOH2HKNNUM        = "285") AND
                           (WK-TAIHI-RECEFTNDSPKBN(2) = SPACE))
                       OR ((LNK-SYU-KOH3HKNNUM        = "285") AND
                           (WK-TAIHI-RECEFTNDSPKBN(3) = SPACE))
                       OR ((LNK-SYU-KOH4HKNNUM        = "285") AND
                           (WK-TAIHI-RECEFTNDSPKBN(4) = SPACE))
                           COMPUTE  LNK-SYU-SHOKUJI6  =
                                    LNK-SYU-SHOKUJI6       +    5
                           COMPUTE  LNK-SYU-SHOKUJI6  =
                                    LNK-SYU-SHOKUJI6       /   10
                           COMPUTE  LNK-SYU-SHOKUJI6  =
                                    LNK-SYU-SHOKUJI6       *   10
                       END-IF
                    ELSE
                       MOVE  WK-TOKUSYU-COMPFTN2  TO   LNK-SYU-SHOKUJI6
                    END-IF
      *
      *****         MOVE     WK-TOKUSYU-ENTANI    TO   LNK-SYU-SHOKUJI7
                    COMPUTE  LNK-SYU-SHOKUJI7  =
                                            WK-TOKUSYU-ENTANI  + 0.9
      *
           END-IF
      *
           .
      *
       23092-TOKUSYU-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *        ÆüÊÌ¥Ç¡¼¥¿ÃÖ¤­´¹¤¨ÊÔ½¸½èÍý
      ******************************************************************
       23093-DAYDATA-OKIKAE-HENSYU-SEC         SECTION.
      *
           PERFORM    VARYING   KOH-IDX2   FROM   1   BY   1
                       UNTIL    KOH-IDX2    >     4
      *
             IF   WK-DAYINFFLG(KOH-IDX2)   =    1
      *
               MOVE  LNK-SYU-KOH-HKNFTNMONEY(KOH-IDX2)
                                  TO   WK-DAYTAIHI-FTNMONEY
               MOVE  LNK-SYU-KOH-COMPFTN(KOH-IDX2)
                                  TO   WK-DAYTAIHI-COMPFTN
               COMPUTE  WK-DAYTAIHI-COMPFTNWK =
                        LNK-SYU-KOH-COMPFTN-ENTANI(KOH-IDX2) + 0.9
      *
               PERFORM    VARYING   KOH-IDX3   FROM   1   BY   1
                           UNTIL    KOH-IDX3    >    31
      *
                   IF    KOHI-TBL(KOH-IDX3 KOH-IDX2)  =   9
      *
                       IF  LNK-SDAY-FTNMONEY(KOH-IDX3 KOH-IDX2)
                                        <=    WK-DAYTAIHI-FTNMONEY
                         COMPUTE WK-DAYTAIHI-FTNMONEY  =
                                 WK-DAYTAIHI-FTNMONEY  -
                                 LNK-SDAY-FTNMONEY(KOH-IDX3 KOH-IDX2)
                       ELSE
                         MOVE    WK-DAYTAIHI-FTNMONEY  TO
                                 LNK-SDAY-FTNMONEY(KOH-IDX3 KOH-IDX2)
                         MOVE    ZERO    TO   WK-DAYTAIHI-FTNMONEY
                       END-IF
      *
                       IF  LNK-SDAY-COMPFTN(KOH-IDX3 KOH-IDX2)
                                        <=    WK-DAYTAIHI-COMPFTN
                         COMPUTE WK-DAYTAIHI-COMPFTN  =
                                 WK-DAYTAIHI-COMPFTN  -
                                 LNK-SDAY-COMPFTN(KOH-IDX3 KOH-IDX2)
                       ELSE
                         MOVE    WK-DAYTAIHI-COMPFTN   TO
                                 LNK-SDAY-COMPFTN(KOH-IDX3 KOH-IDX2)
                         MOVE    ZERO    TO   WK-DAYTAIHI-COMPFTN
                       END-IF
      *
                       IF  LNK-SDAY-COMPFTNWK(KOH-IDX3 KOH-IDX2)
                                        <=    WK-DAYTAIHI-COMPFTNWK
                         COMPUTE WK-DAYTAIHI-COMPFTNWK  =
                                 WK-DAYTAIHI-COMPFTNWK  -
                                 LNK-SDAY-COMPFTNWK(KOH-IDX3 KOH-IDX2)
                       ELSE
                         MOVE    WK-DAYTAIHI-COMPFTNWK   TO  
                                 LNK-SDAY-COMPFTNWK(KOH-IDX3 KOH-IDX2)
                         MOVE    ZERO    TO   WK-DAYTAIHI-COMPFTNWK
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
      *
             END-IF
      *
           END-PERFORM.
      *
       23093-DAYDATA-OKIKAE-HENSYU-EXT.
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
                                           SYS-5000-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
                                           SYS-1026-REC
                                           SYS-1027-REC
                                           SYS-1037-REC
                                           SYS-4001-REC
                                           SYS-4003-REC
                                           SYS-5000-REC
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
      **>  IF  LNK-S02-KOUREI-NYUIN-KBN   =   "1"
               MOVE    "DBFETCH"           TO  MCP-FUNC.
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC.
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYU-SYUNOU-REC
                   IF     (LNK-S02-SAIKEISAN-KBN    =   "1")    AND
                          (LNK-S02-SAIKEISAN-DENPYO =   SYU-DENPNUM)
                           GO    TO    SYUNOU-READ-100
                   ELSE
                           MOVE    ZERO    TO  FLG-SYUNOU
                   END-IF
               ELSE
                   INITIALIZE                  SYU-SYUNOU-REC
                   MOVE    1               TO  FLG-SYUNOU
               END-IF.
      **>  ELSE
      **>      MOVE    "DBFETCH"           TO  MCP-FUNC
      **>      CALL    "ORCMCPSUB"          USING
      **>                                   MCPAREA
      **>                                   ORCMCPAREA
      **>                                   MCPDATA-REC
      **>      IF      MCP-RC              =   ZERO
      **>          MOVE    MCPDATA-REC     TO  SYU-SYUNOU-REC
      **>          IF     (LNK-S02-SAIKEISAN-KBN    =   "1")    AND
      **>                 (LNK-S02-SAIKEISAN-DENPYO <=   SYU-DENPNUM)
      **>                  GO    TO    SYUNOU-READ-100
      **>          ELSE
      **>                  MOVE    ZERO    TO  FLG-SYUNOU
      **>          END-IF
      **>      ELSE
      **>          INITIALIZE                  SYU-SYUNOU-REC
      **>          MOVE    1               TO  FLG-SYUNOU
      **>      END-IF
      **>  END-IF.
      *
       SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ÆüÊÌ¼ýÇ¼¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       SYUDAY-READ-SEC          SECTION.
      *
           INITIALIZE                         SYU-SYUDAY-REC.
           MOVE    SYU-HOSPID         TO      SDAY-HOSPID.
           MOVE    SYU-NYUGAIKBN      TO      SDAY-NYUGAIKBN.
           MOVE    SYU-PTID           TO      SDAY-PTID.
           MOVE    SYU-DENPNUM        TO      SDAY-DENPNUM.
           MOVE    SYU-SYUDAY-REC     TO      MCPDATA-REC.
           MOVE    "DBSELECT"         TO      MCP-FUNC.
           MOVE    "SYUDAY-KEY"       TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"        USING   MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC.
           IF      MCP-RC             =       ZERO
               MOVE    "SYUDAY-KEY"   TO      ORC-DBPATH
               MOVE    "DBFETCH"      TO      MCP-FUNC
               CALL    "ORCMCPSUB"    USING   MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
               IF      MCP-RC              =   ZERO
                  MOVE   MCPDATA-REC  TO  SYU-SYUDAY-REC
               ELSE
                  INITIALIZE              SYU-SYUDAY-REC
               END-IF
           ELSE
                  INITIALIZE              SYU-SYUDAY-REC
           END-IF.
      *
           MOVE    "DBCLOSECURSOR"    TO      MCP-FUNC.
           MOVE    "SYUDAY-KEY"       TO      ORC-DBPATH.
           CALL    "ORCMCPSUB"        USING   MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC.
      *
       SYUDAY-READ-EXT.
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
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×ÆÉ¹þ
      *****************************************************************
       SRYACCT-READ-SEC           SECTION.
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
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
           MOVE    WK-SRYCD             TO      TNS-SRYCD
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    LNK-SYU-SRYYMD       TO      ORC-DBYMD
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "TENSU-KEY"          TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "TENSU-KEY"      TO      ORC-DBPATH
               PERFORM  TENSU-READ-SEC
           ELSE
               MOVE     1               TO      FLG-TENSU
           END-IF
      *
           IF     FLG-TENSU    =    ZERO
              IF    LNK-SYU-SYUHKNNUM   =   "971" OR "973"
                 COMPUTE   WK-KEISAN    =   TNS-TEN    *  1.2
                 COMPUTE   WK-KEISAN    =   WK-KEISAN  +   5
                 COMPUTE   WK-KEISAN    =   WK-KEISAN  /  10
                 COMPUTE   WK-KEISAN    =   WK-KEISAN  *  10
                 IF    LNK-SYU-SYUHKNNUM   =   "973"
                 AND   WK-4001-SKJKBN      =   "2"
                    COMPUTE   WK-KEISAN    =   WK-KEISAN  *  1.2
                 END-IF
              ELSE
                 MOVE      TNS-TEN     TO     WK-KEISAN
              END-IF
           ELSE
                 MOVE      ZERO        TO     WK-KEISAN
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO      MCP-FUNC
           MOVE    "TENSU-KEY"          TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ôÆÉ¹þ
      *****************************************************************
       TENSU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
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
       TENSU-READ-EXT.
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
