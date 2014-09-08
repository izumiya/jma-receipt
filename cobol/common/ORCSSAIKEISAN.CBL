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
       PROGRAM-ID.         ORCSSAIKEISAN.
      ****************************************************************
      *    OS           LINUX                                         
      *                                                               
      *    PROGRAM      ORCSSAIKEISAN  /  ÉéÃ´¶âºÆ·×»»¥µ¥Ö            
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  04/07/16    NACL¡ÝÌçÏÆ    ¿·µ¬ºîÀ®
      ****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    04/10/01  ¼«ÇåÀÕÆÃÄê´ïºàÅù¾ÃÈñÀÇÂÐ±þ
      *  01.00.02    NACL-ÌçÏÆ    04/12/01  ¼ýÇ¼³°Íè°ì³çºÆ·×»»ÂÐ±þ
      *  01.00.03    NACL-ÌçÏÆ    04/12/17  ¶»Éô¸ÇÄêÂÓ²Ã»»¡¢ÁÏ³°¸ÇÄê´ï
      *                                     »ÈÍÑ²Ã»»¤òÌôºÞÅù¤Ø
      *  01.00.04    NACL-ÌçÏÆ    05/01/27  ¹ø¡¦¶»Éô¸ÇÄêÂÓ²Ã»»¡¢ÁÏ³°¸ÇÄê´ï
      *                                     »ÈÍÑ²Ã»»¤Îµ»½Ñ¡¦ÌôºÞÅù¤ÎÁªÂòÂÐ±þ
      *  01.00.05    NACL-¾®Æ¦Âô  05/07/15  ¼«Èñ£±£°¹àÌÜÂÐ±þ
      *  01.00.06    NACL-ÌçÏÆ    05/07/19  ¼ýÇ¼¹àÌÜÄÉ²Ã¤ËÈ¼¤¦½¤Àµ
      *  01.00.07    NACL-ÌçÏÆ    06/03/30  Ï«ºÒÆ±Æü½é¿ÇÂÐ±þ
      *  03.01.01    NACL-ÌçÏÆ    06/07/06  ÎÎ¼ý½ñÂÐ±þ
      *  03.02.01    NACL-ÌçÏÆ    06/09/05  ·ò¹¯ÊÝ¸±Ë¡£±£°·î²þÀµÂÐ±þ
      *  03.04.01    NACL-ÌçÏÆ    06/12/01  ¼«ÇåÀÕ·òÊÝ½àµòÂÐ±þ
      *  03.04.02    NACL-ÌçÏÆ    07/01/22  ¼ýÇ¼ÌÀºÙÊÔ½¸½¤Àµ
      *  03.04.03    NACL-Â¿¡¹Ç¼  07/03/06  Êñ³ç¿ÇÎÅºÞÈ½ÄêÄÉ²Ã
      *  03.04.04    NACL-ÌçÏÆ    07/05/11  Ï«ºÒ¡¦¼«ÇåÀÕÃ±²Á£±£°±ßÌ¤ËþÂÐ±þ
      *  03.04.05    NACL-ÌçÏÆ    07/06/08  ½é¿ÇÎÁ»»Äê»þ¤Î¾ò·ïÄÉ²Ã
      *  03.05.00    NACL-Â¿¡¹Ç¼  07/04/XX  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.01.01    NACL-ÌçÏÆ    07/10/25  ¸øÌ³ºÒ³²ÂÐ±þ
      *  04.01.02    NACL-ÌçÏÆ    07/12/07  ¸ø³²ÂÐ±þ
      *  04.02.01    NACL-ÌçÏÆ    08/02/29  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *  04.04.01    NACL-ÌçÏÆ    10/03/31  ²þÀµÂÐ±þ¡ÊðôÉô¸ÇÄêÂÓ²Ã»»¤ÎÂÐ±þ¡Ë
      *  04.05.01    NACL-ÌçÏÆ    09/06/16  °ì³çºÆ·×»»ÊýË¡ÁªÂòµ¡Ç½ÂÐ±þ
      *  04.05.02    NACL-ÌçÏÆ    10/01/19  ¼£¸³ÂÐ±þ
      *  04.05.03    NACL-ÌçÏÆ    10/01/26  ÅìµþÅÔ¥Þ¥ë»ÒÄÉ²ÃÂÐ±þ
      *  04.05.04    NACL-ÌçÏÆ    11/02/28  Ä¹ºê¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.05.05    NACL-ÌçÏÆ    12/03/09  ²þÀµÂÐ±þ¡§³°Íè¸½ÊªµëÉÕÂÐ±þ
      *  04.05.06    NACL-ÌçÏÆ    12/03/30  ²þÀµÂÐ±þ¡§Ï«ºÒºÆ¿Ç¡ÊÆ±°ìÆü£²²ÊÌÜ¡Ë
      *
      *  04.06.01    NACL-ÌçÏÆ    10/09/02  ½Ð»º°é»ù°ì»þ¶âÄ¾ÀÜ»ÙÊ§À©ÅÙ¡Ê³°Íè¡Ë
      *  04.06.02    NACL-ÌçÏÆ    10/11/08  ¼çÊÝ¸±¡Ü¥Þ¥ëÄ¹¤ÎÀÁµáÂÐ±þ
      *  04.06.03    NACL-ÌçÏÆ    10/12/06  Ï«ºÒ¡¦¼«ÇåÀÕ¤Î¶â³Û¥Õ¥ê¡¼ÆþÎÏ
      *                                     ¥³¡¼¥ÉÂÐ±þ
      *  04.06.04    NACL-ÌçÏÆ    13/02/18  Ï«ºÒ¥ì¥»ÅÅÂÐ±þ
      *                                     ¡Ê¿·¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÂÐ±þ¡Ë
      *  04.06.05    NACL-ÌçÏÆ    13/02/27  Ê¿À®£²£µÇ¯£´·î²þÀµÂÐ±þ
      *  04.06.06    NACL-ÌçÏÆ    13/09/25  Ï«ºÒ¥Þ¥¹¥¿ÄÉ²ÃÂÐ±þ
      *                                     ¡ÊÅÅÏÃºÆ¿ÇÎÁ¡¡Åù¡Ë
      *
      *  04.07.01    NACL-ÌçÏÆ    13/07/10  ¼«ÇåÀÕ¡ÊÂè»°¼Ô¹Ô°Ù¡ËÂÐ±þ
      *  04.07.02    NACL-ÌçÏÆ    14/02/13  Ï«ºÒ¡¦¼«ÇåÀÕ¤Î»Ü½Ñ²Ã»»ÂÐ±þ
      *  04.07.03    NACL-ÌçÏÆ    14/03/01  Ï«ºÒ¥Þ¥¹¥¿ÄÉ²ÃÂÐ±þ
      *  04.07.00    NACL-Â¿¡¹Ç¼  14/09/04  ¥Þ¥¤¥Ê¥¹ºÞÅÀ¿ôÈ½ÄêÂÐ±þÂ¾
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
      *    Åº¤¨»úÎÎ°è
       01  IDX-AREA.
           03  IDX1                        PIC 9(03).
           03  IDX2                        PIC 9(03).
           03  IDX3                        PIC 9(03).
           03  IDX4                        PIC 9(03).
           03  IDX5                        PIC 9(03).
           03  IDY                         PIC 9(03).
           03  IDY2                        PIC 9(03).
           03  IDZ                         PIC 9(03).
           03  IDW                         PIC 9(03).
           03  SAI-IDX                     PIC 9(03).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-SYUTOTAL                PIC 9(01).
           03  FLG-JYURRK                  PIC 9(01).
           03  FLG-HKNKUMI                 PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-SYUMEI                  PIC 9(01).
           03  FLG-NG                      PIC 9(01).
           03  FLG-SENIN                   PIC 9(01).
           03  FLG-SENIN-SYOSIN            PIC 9(01).
           03  FLG-NOGRP                   PIC 9(01).
           03  FLG-OK                      PIC 9(01).
           03  FLG-TAISYO                  PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-GENBUTU                 PIC 9(01).
           03  FLG-GENBUTU2                PIC 9(01).
           03  FLG-GENBUTU3                PIC 9(01).
      *
      *    ¥ï¡¼¥¯ÎÎ°è
       01  WRK-AREA.
           03  WRK-SEIKYUGK-TBL.
              04  WRK-SEIKYUGK-TBL-OCC     OCCURS  15.
                 05  WRK-MAE-SEIKYUGK      PIC 9(07).
                 05  WRK-ATO-SEIKYUGK      PIC 9(07).
           03  WRK-GRP-DENPNUM             PIC 9(07).
           03  WRK-TAIHI-DENPNUM           PIC 9(07).
           03  WRK-TAIHI-SYUHKNNUM         PIC X(03).
           03  WRK-TAIHI-HKNCOMBINUM       PIC X(04).
           03  WRK-GRPCNT                  PIC 9(02).
           03  WRK-SAIKEISAN-YMD.
                 05  WRK-SAIKEISAN-YY      PIC 9(04).
                 05  WRK-SAIKEISAN-MM      PIC 9(02).
                 05  WRK-SAIKEISAN-DD      PIC 9(02).
           03  WRK-KAGRPFLG                PIC X(01).
           03  WRK-TYK-HKNTEN              PIC S9(07).
           03  WRK-TYK-HKNTEN2             PIC  9(07).
           03  WRK-SKYMONEY-TBL.
              04  WRK-SKYMONEY-TBL-OCC     OCCURS  15.
                 05  WRK-SKYMONEY          PIC 9(07).
                 05  WRK-GRP-SGKMONEY      PIC S9(07).
           03  WRK-RUI-SKYMONEY            PIC 9(07).
           03  WRK-KEISAN                  PIC S9(07).
           03  WRK-DOUJI-SRYKA-TBL.
              04  WRK-DOUJI-SRYKA-TBL-OCC  OCCURS  15.
                 05  WRK-DOUJI-SRYKA       PIC X(02)
                                           OCCURS  10.
           03  WRK-4001-KOTEIKBN           PIC X(01).
      *
           03  WRK-SYURRKNUM               PIC S9(09)  BINARY.
           03  WRK-SYUEDANUM               PIC S9(09)  BINARY.
           03  WRK-SYURRKUPDKBN            PIC X(01).
           03  WRK-FUTANWARI               PIC 9V99.
           03  WRK-KEISAN2                 PIC 9(09).
      *
      *    ºÞÈÖ¹æÅùÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
       01  ZAIBAN-TABLE.
           03  ZAIBAN-TBL                  OCCURS 100.
                 05  TBL-SRYKBN            PIC  X(02).
                 05  TBL-ZAIBAN            PIC  9(08).
                 05  TBL-ZAIKAISU          PIC  9(07).
                 05  TBL-ZAITEN            PIC  S9(08).
                 05  TBL-SYUTEN            PIC  9(08).
                 05  TBL-GENTEN            PIC  9(08).
                 05  TBL-ZAIKBN            PIC  9(03).
      *
      *    ÊÝ¸±Å¬ÍÑ³°¶â³ÛÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
       01  HKNTEKGAI-TABLE.
           03  HKNTEKGAI-TBL               OCCURS 16.
                 05  TBL-TGMONEY           PIC  9(08).
                 05  TBL-TGMONEY-TAX       PIC  9(08).
       01  JIHI-TABLE.
           03  JIHI-TBL                    OCCURS 10.
                 05  TBL-JIHI-TAXNASI      PIC  9(08).
                 05  TBL-JIHI-TAXARI       PIC  9(08).
      *
      *    Ï«ºÒ¾ðÊóÍÑ¥Æ¡¼¥Ö¥ë
       01  RSI-TABLE.
           03  WK-RSISHOSHIN-TENSU         PIC  9(08).
           03  WK-RSISAISHIN-TENSU         PIC  9(08).
           03  WK-RSISDO-TENSU             PIC  9(08).
           03  WK-RSIETC-TENSU             PIC  9(08).
      *
           03  WK-RSISHOSHIN               PIC  9(08).
           03  WK-RSISAISHIN               PIC  9(08).
           03  WK-RSISDO                   PIC  9(08).
           03  WK-RSIETC                   PIC  9(08).
           03  WK-RSIETC2                  PIC  9(08).
      *
           03  WK-SINDAN                   PIC  9(08).
           03  WK-MEISAI                   PIC  9(08).
           03  WK-KYUKYU                   PIC  9(08).
      *
           03  WK-YOUBUKASAN               PIC  9(08).
           03  WK-KYOBUKASAN               PIC  9(08).
           03  WK-KEIBUKASAN               PIC  9(08).
           03  WK-SOGAIKASAN               PIC  9(08).
           03  WK-RYOYOTEATE               PIC  9(08).
      *
           03  WK-SYUTEN-KEI               PIC  9(08).
           03  WRK-TEN                     PIC  9(08).
      *
           03  WK-KOGAI-TEN                PIC  9(08).
      *
           03  WRK-TEN-KG                  PIC  9(09).
           03  WRK-TEN-KG2                 PIC  9(09).
      *
      *    ¥·¥¹¥Æ¥àÎÎ°è
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY               PIC 9(04).
               05  WRK-SYSMM               PIC 9(02).
               05  WRK-SYSDD               PIC 9(02).
      *
      *    ½é¿ÇÎÁ»»Äê¥Õ¥é¥°ÍÑ¥Æ¡¼¥Ö¥ë
       01  TBL-SYOSIN-AREA.
           03  FILLER          PIC X(09)   VALUE    "111003610".
           03  FILLER          PIC X(09)   VALUE    "111700210".
           03  FILLER          PIC X(09)   VALUE    "111000110".
           03  FILLER          PIC X(09)   VALUE    "111700110".
           03  FILLER          PIC X(09)   VALUE    "113003510".
           03  FILLER          PIC X(09)   VALUE    "113003710".
           03  FILLER          PIC X(09)   VALUE    "099110001".
           03  FILLER          PIC X(09)   VALUE    "111012510".
      *
       01  TBL-SYOSIN-R        REDEFINES   TBL-SYOSIN-AREA.
           03  TBL-SYOSIN      PIC X(09)   OCCURS    8.
      *
       01  TBL-SYOSIN-MAX      PIC 9(02)   VALUE     8.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ¥ï¡¼¥¯¼ýÇ¼¥Þ¥¹¥¿¡¼¡Ê¹ç·×ÍÑ¡Ë
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC" REPLACING  //SYU-// BY //WRK-SYU-//.
      *
      *    ¥ï¡¼¥¯¼ýÇ¼¥Þ¥¹¥¿¡¼¡Ê¸ºÅÀÍÑ¡Ë
       01  WRK-SYUNOUG-REC.
           COPY    "CPSYUNOU.INC" REPLACING  //SYU-// BY //WRK-SYUG-//.
      *
      *    ¥ï¡¼¥¯¼ýÇ¼¥Þ¥¹¥¿¡¼¡ÊÂàÈòÍÑ¡Ë
       01  WRK-SYUNOU-TAIHI-REC.
           COPY    "CPSYUNOU.INC" REPLACING  //SYU-// BY //WRK-SYUT-//.
      *
      *    ¼ýÇ¼ÌÀºÙ¥Þ¥¹¥¿¡¼
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    ¼ýÇ¼¹ç·×¥Þ¥¹¥¿
       01  SYUTTL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡Ý
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ²ñ·×ÉÕ²Ã¾ðÊó¥Þ¥¹¥¿¡Ý
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡Ý
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ¼õ¿ÇÍúÎò
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡Ý
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSK1038.INC".
      *
           COPY    "CPSK4001.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    Ê¡²¬¸©ÃÏÊý¸øÈñµëÉÕÂÐ¾Ý³°ÅÀ¿ô¼èÆÀ¥µ¥Ö
           COPY    "CPORCSSYU40.INC".
      *
      *    ÆüÊó¥­¡¼¼èÆÀ¥µ¥Ö
           COPY "CPORCSDAILYKEY.INC".
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ÉéÃ´¶â³Û·×»»ÍÑ¥Ñ¥é¥á¥¿
           COPY    "CPORCS01.INC".
      *
      *    ¼ýÇ¼¹¹¿·º¹³Û¼èÆÀ
           COPY    "CPORCSS002.INC".
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    ¥ï¡¼¥¯ÉéÃ´¶â³Û·×»»ÍÑ¥Ñ¥é¥á¥¿¡Ê¹ç·×ÍÑ¡Ë
      ***  COPY    "CPORCS01.INC" REPLACING  //LNK-// BY //WRK-//.
      *
      *    ¥ï¡¼¥¯¼ýÇ¼¥Þ¥¹¥¿¡¼
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 15.
           COPY    "CPSYUNOU.INC" REPLACING  //SYU-// BY //LNK-SYU-//.
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      /
      ***
       LINKAGE             SECTION.
           COPY  "COMMON-SPA".
           COPY  "CPORCSSAIKEISAN.INC".
      /
       PROCEDURE           DIVISION  USING
                                     SPA-AREA
                                     LNK-ORCSSAIKEISAN-REC.
      ******************************************************************
      *        °ÍÍêÀ©¸æ½èÍý
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
      *
      *    ¼ýÇ¼¥Ç¡¼¥¿¤ò¥Æ¡¼¥Ö¥ë¤ËÊÔ½¸
      *
           INITIALIZE                  LNK-ORCS01-REC
           INITIALIZE                  LNK-SYUNOU-REC-T
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           INITIALIZE                          SYUNOU-REC
           MOVE    LNK-ORCSSKS-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    LNK-ORCSSKS-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    LNK-ORCSSKS-PTID        TO  SYU-PTID
           MOVE    LNK-ORCSSKS-DENPNUM     TO  SYU-DENPNUM
           MOVE    SYUNOU-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_syunou"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING  MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
           IF      MCP-RC     =       ZERO
                     MOVE    "tbl_syunou"  TO  MCP-TABLE
                     MOVE    "key"         TO  MCP-PATHNAME
                     PERFORM  900-SYUNOU-READ-SEC
           ELSE
                     MOVE     1            TO  FLG-SYUNOU
           END-IF
           MOVE     "tbl_syunou"           TO  MCP-TABLE
           MOVE     "key"                  TO  MCP-PATHNAME
           PERFORM   DB-CLOSE-SEC
      *
           IF      FLG-SYUNOU          =   1
               MOVE    "0060"         TO  SPA-ERRCD
               GO       TO                MAIN-EXT
           ELSE
      *
             IF    SYU-DENPJTIKBN      =   "3"
               MOVE    "0061"         TO  SPA-ERRCD
               GO       TO                MAIN-EXT
             ELSE
      *
               MOVE   SYU-GRP-DENPNUM     TO      WRK-GRP-DENPNUM
               MOVE   SYU-SRYYMD          TO      WRK-SAIKEISAN-YMD
      *
               PERFORM  900-SYS1038-READ-SEC
               IF       SYS-1038-KAGRPFLG  =  SPACE
                   MOVE    "0"            TO      SYS-1038-KAGRPFLG
               END-IF
               MOVE     SYS-1038-KAGRPFLG TO      WRK-KAGRPFLG
                                                  LNK-S01-KAGRPFLG
      *
               MOVE     SPACE   TO   WRK-4001-KOTEIKBN
               IF   SYU-SYUHKNNUM   =   "971" OR "973"
                 PERFORM  900-SYS4001-READ-SEC
                 MOVE     SYS-4001-KOTEIKBN   TO  WRK-4001-KOTEIKBN
               END-IF
      *
               MOVE     SYUNOU-REC        TO      WRK-SYUNOU-TAIHI-REC
      *
               IF  (LNK-ORCSSKS-IKTGENKEIFLG = "1" )
               AND (WRK-GRP-DENPNUM      NOT = ZERO)
                 PERFORM  4407-GRP-DATA-CHECK-SEC
               END-IF
               IF   FLG-GENBUTU  =  1
                 MOVE  ZERO               TO      WRK-GRP-DENPNUM
               END-IF
      *
               IF   WRK-GRP-DENPNUM   =   ZERO
                 ADD     1                TO      IDY
                 MOVE    WRK-SYUNOU-TAIHI-REC
                                          TO      LNK-SYUNOU-REC(IDY)
                 MOVE    WRK-SYUT-SKYMONEY
                                          TO      WRK-MAE-SEIKYUGK(IDY)
                 MOVE    1                TO      FLG-NOGRP
               ELSE
                 INITIALIZE                       SYUNOU-REC
                 MOVE    LNK-ORCSSKS-HOSPNUM
                                          TO      SYU-HOSPNUM
                 MOVE    LNK-ORCSSKS-NYUGAIKBN
                                          TO      SYU-NYUGAIKBN
                 MOVE    LNK-ORCSSKS-PTID TO      SYU-PTID
                 MOVE    WRK-GRP-DENPNUM  TO      SYU-GRP-DENPNUM
                 MOVE    SYUNOU-REC       TO      MCPDATA-REC
                 MOVE    "DBSELECT"       TO      MCP-FUNC
                 MOVE    "tbl_syunou"     TO      MCP-TABLE
                 MOVE    "key27"          TO      MCP-PATHNAME
                 CALL    "ORCDBMAIN"      USING   MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
                 IF      MCP-RC             =       ZERO
                     MOVE    "tbl_syunou" TO      MCP-TABLE
                     MOVE    "key27"      TO      MCP-PATHNAME
                     PERFORM  900-SYUNOU-READ-SEC
                     PERFORM  UNTIL  (FLG-SYUNOU   =    1)
                              OR     (SPA-ERRCD    NOT =   SPACE)
                       IF   SYU-DENPJTIKBN  NOT =  "3"
                          IF   WRK-TAIHI-HKNCOMBINUM  NOT =  SPACE
                          AND  WRK-TAIHI-HKNCOMBINUM
                                           NOT =   SYU-HKNCOMBINUM
                            IF   WRK-GRPCNT  >  1
                                PERFORM   4400-SYUTOTAL-FIND-SEC
                                IF   FLG-NG   =   1
                                    MOVE    "0062"   TO  SPA-ERRCD
                                END-IF
                            ELSE
                              IF   WRK-GRPCNT  =  1
                                PERFORM   4400-SYUTOTAL-FIND2-SEC
                                IF   FLG-NG   =   1
                                    MOVE    "0062"   TO  SPA-ERRCD
                                END-IF
                              END-IF
                            END-IF
                            MOVE   ZERO      TO  WRK-GRPCNT
                          END-IF
                          IF      SPA-ERRCD  =   SPACE
                              ADD   1            TO  IDY
                              MOVE  SYUNOU-REC   TO  LNK-SYUNOU-REC(IDY)
                              MOVE  SYU-SKYMONEY
                                             TO  WRK-MAE-SEIKYUGK(IDY)
                              MOVE  SYU-HKNCOMBINUM
                                             TO  WRK-TAIHI-HKNCOMBINUM
                              MOVE  SYU-FUKU-DENPNUM
                                             TO  WRK-TAIHI-DENPNUM
                              MOVE  SYU-SYUHKNNUM
                                             TO  WRK-TAIHI-SYUHKNNUM
                              ADD   1            TO  WRK-GRPCNT
                          END-IF
                       END-IF
                       MOVE    "tbl_syunou"  TO  MCP-TABLE
                       MOVE    "key27"       TO  MCP-PATHNAME
                       PERFORM  900-SYUNOU-READ-SEC
                     END-PERFORM
                 END-IF
                 MOVE     "tbl_syunou"       TO  MCP-TABLE
                 MOVE     "key27"            TO  MCP-PATHNAME
                 PERFORM   DB-CLOSE-SEC
      *
                 IF      SPA-ERRCD     NOT =   SPACE
                     GO       TO          MAIN-EXT
                 END-IF
                 IF   WRK-GRPCNT  >  1
                       PERFORM   4400-SYUTOTAL-FIND-SEC
                       IF   FLG-NG   =   1
                           MOVE    "0062"   TO  SPA-ERRCD
                           GO       TO          MAIN-EXT
                       END-IF
                 ELSE
                    IF   WRK-GRPCNT  =  1
                       PERFORM   4400-SYUTOTAL-FIND2-SEC
                       IF   FLG-NG   =   1
                           MOVE    "0062"   TO  SPA-ERRCD
                           GO       TO          MAIN-EXT
                       END-IF
                    END-IF
                 END-IF
               END-IF
      *
               IF    IDY   =   ZERO
                   MOVE    "0060"         TO  SPA-ERRCD
                   GO       TO                MAIN-EXT
               END-IF
      *
             END-IF
      *
           END-IF
      *
      *    ¼ýÇ¼¥Ç¡¼¥¿¤ÎºÆÊÔ½¸
      *
           INITIALIZE                  WRK-SYUNOU-REC
           INITIALIZE                  WRK-SYUNOUG-REC
      ***  INITIALIZE                  WRK-ORCS01-REC
           MOVE       SPACE     TO     WRK-TAIHI-HKNCOMBINUM
      *
           PERFORM    VARYING   IDY    FROM   1   BY   1
                              UNTIL  (IDY                 >    15 )
                                OR   (LNK-SYU-SRYYMD(IDY) =  SPACE)
      *
               INITIALIZE             WRK-SYUNOUG-REC
      *
      *        ¼ýÇ¼¹àÌÜ½é´ü²½
      *
               MOVE       SPACE   TO  LNK-SYU-SKY-NAIYOU(IDY)
               INITIALIZE             LNK-SYU-SKY-NAIYOU(IDY)
               PERFORM  VARYING  IDX3  FROM    1    BY    1
                         UNTIL   IDX3    >     10
                   MOVE    ZERO   TO  LNK-SYU-JIHI-TAXNASI(IDY IDX3)
                   MOVE    ZERO   TO  LNK-SYU-JIHI-TAXARI (IDY IDX3)
               END-PERFORM
               MOVE        ZERO   TO  LNK-SYU-JIHI-TOTAL(IDY)
                                      LNK-SYU-JIHI-TOTAL-TAX(IDY)
                                      LNK-SYU-JIHI-TAX(IDY)
                                      LNK-SYU-SKYMONEY(IDY)
                                      LNK-SYU-RSISHOSHIN-MONEY(IDY)
                                      LNK-SYU-RSISAISHIN-MONEY(IDY)
                                      LNK-SYU-RSISDO-MONEY(IDY)
                                      LNK-SYU-RSIETC-MONEY(IDY)
                                      LNK-SYU-SHOHOU-SAI(IDY)
               MOVE       SPACE   TO  LNK-SYU-KYUFUGAI(IDY)
               INITIALIZE             LNK-SYU-KYUFUGAI(IDY)
      *
      *
               IF   LNK-SYU-SRYKA(IDY)  NOT =  "00"
      *
      *          ¼õ¿ÇÍúÎò¤«¤é£É£Î£Ä£Å£Ø¼èÆÀ
      *
                 MOVE    ZERO                   TO  SAI-IDX
                 INITIALIZE                         JYURRK-REC
                 MOVE    LNK-SYU-HOSPNUM(IDY)   TO  JYURRK-HOSPNUM
                 MOVE    LNK-SYU-PTID(IDY)      TO  JYURRK-PTID
                 MOVE    LNK-SYU-NYUGAIKBN(IDY) TO  JYURRK-NYUGAIKBN
                 MOVE    LNK-SYU-DENPNUM(IDY)   TO  JYURRK-DENPNUM
                 MOVE    JYURRK-REC             TO  MCPDATA-REC
                 MOVE    "DBSELECT"             TO  MCP-FUNC
                 MOVE    "tbl_jyurrk"           TO  MCP-TABLE
                 MOVE    "key6"                 TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"            USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                 IF      MCP-RC              =   ZERO
                    MOVE       "tbl_jyurrk"     TO  MCP-TABLE
                    MOVE       "key6"           TO  MCP-PATHNAME
                    PERFORM     900-JYURRK-READ-SEC
      *H24.5
      *             ÊÝ¸±ÁÈ¹ç¤»È½ÄêÄÉ²Ã
                    PERFORM UNTIL   (FLG-JYURRK     =   1   )
                            OR      (JYURRK-HKNCOMBINUM 
                                         =   LNK-SYU-HKNCOMBINUM(IDY))
                         PERFORM     900-JYURRK-READ-SEC
                    END-PERFORM
      *H24.5
                    IF   FLG-JYURRK   =    ZERO
                        COMPUTE   SAI-IDX  =  JYURRK-RENNUM  +  1
                        IF    SAI-IDX   >   4
                            MOVE      4         TO  SAI-IDX
                        END-IF
                        IF    JYURRK-SRYKBN1 = "01"
                            MOVE   "1"   TO   LNK-S01-SINSATUFLG
                        END-IF
                    END-IF
                 END-IF
                 MOVE     "tbl_jyurrk"          TO  MCP-TABLE
                 MOVE     "key6"                TO  MCP-PATHNAME
                 PERFORM   DB-CLOSE-SEC
      *
                 IF    SAI-IDX   =   ZERO
                   MOVE    "0063"         TO  SPA-ERRCD
                   GO       TO                MAIN-EXT
                 END-IF
      *
                 IF    LNK-SYU-SYUHKNNUM(IDY)   =   "971" OR "973"
      *
                   INITIALIZE                        PTINF-REC
                   MOVE    LNK-SYU-HOSPNUM(IDY) TO   PTINF-HOSPNUM
                   MOVE    LNK-SYU-PTID(IDY)    TO   PTINF-PTID
                   MOVE    PTINF-REC            TO   MCPDATA-REC
                   MOVE    "DBSELECT"           TO   MCP-FUNC
                   MOVE    "tbl_ptinf"          TO   MCP-TABLE
                   MOVE    "key"                TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"          USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                   IF      MCP-RC               =    ZERO
                       MOVE    "tbl_ptinf"      TO   MCP-TABLE
                       MOVE    "key"            TO   MCP-PATHNAME
                       PERFORM  900-PTINF-READ-SEC
                   ELSE
                       INITIALIZE      PTINF-REC
                   END-IF
                   MOVE    "tbl_ptinf"          TO   MCP-TABLE
                   MOVE    "key"                TO   MCP-PATHNAME
                   PERFORM  DB-CLOSE-SEC
      *
                   INITIALIZE                        ORCSAGECHKAREA
                   MOVE    LNK-SYU-HOSPNUM(IDY) TO   AGECHK-HOSPNUM
                   MOVE    LNK-SYU-PTID(IDY)    TO   AGECHK-PTID
                   MOVE    LNK-SYU-SRYYMD(IDY)  TO   AGECHK-SRYYMD
                   MOVE    PTINF-BIRTHDAY       TO   AGECHK-BIRTHDAY
                   MOVE    LNK-SYU-HKNCOMBINUM(IDY)
                                                TO   AGECHK-HKNCOMBINUM
                   MOVE    LNK-SYU-NYUGAIKBN(IDY)
                                                TO   AGECHK-NYUGAIKBN
                   CALL    "ORCSAGECHK"         USING
                                                ORCSAGECHKAREA
                                                SPA-AREA
                   IF      AGECHK-RC            =    ZERO
                       CONTINUE
                   ELSE
                       INITIALIZE    ORCSAGECHKAREA
                   END-IF
      *
                   EVALUATE  LNK-SYU-SYUHKNNUM(IDY)
                     WHEN      "971"
                       EVALUATE  AGECHK-HKNKBN
      *                  Ï«ºÒ½àµò
                         WHEN      1
                           IF  JYURRK-HKNKBN  =  "5"
                             MOVE    "0062"         TO  SPA-ERRCD
                             GO       TO                MAIN-EXT
                           END-IF
      *                  ·òÊÝ½àµò
                         WHEN      5
                           IF  JYURRK-HKNKBN  =  "1"
                             MOVE    "0062"         TO  SPA-ERRCD
                             GO       TO                MAIN-EXT
                           END-IF
                       END-EVALUATE
                     WHEN      "973"
                       EVALUATE  AGECHK-HKNKBN
      *                  Ï«ºÒ½àµò
                         WHEN      2
                           IF  JYURRK-HKNKBN  =  "4"
                             MOVE    "0062"         TO  SPA-ERRCD
                             GO       TO                MAIN-EXT
                           END-IF
      *                  ·òÊÝ½àµò
                         WHEN      4
                           IF  JYURRK-HKNKBN  =  "2"
                             MOVE    "0062"         TO  SPA-ERRCD
                             GO       TO                MAIN-EXT
                           END-IF
                       END-EVALUATE
                   END-EVALUATE
      *
                 END-IF
      *
      *          Á¥°÷ÄÌ¶ÐºÒ³²¤ÎÈ½ÊÌ
      *
                 MOVE    ZERO                TO  FLG-SENIN                    
                 INITIALIZE                      HKNCOMBI-REC 
                 MOVE    LNK-SYU-HOSPNUM(IDY)
                                             TO  COMB-HOSPNUM
                 MOVE    LNK-SYU-PTID(IDY)   TO  COMB-PTID
                 MOVE    LNK-SYU-HKNCOMBINUM(IDY)
                                             TO  COMB-HKNCOMBINUM 
                 MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                 MOVE    "DBSELECT"          TO  MCP-FUNC
                 MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                 MOVE    "key"               TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
                 IF      MCP-RC              =   ZERO
                     MOVE    "tbl_hkncombi"  TO  MCP-TABLE
                     MOVE    "key"           TO  MCP-PATHNAME
                     PERFORM  900-HKNCOMBI-READ-SEC                           
                 ELSE                                                         
                     MOVE    1               TO  FLG-HKNKUMI                  
                 END-IF                                                       
                 MOVE     "tbl_hkncombi"     TO  MCP-TABLE
                 MOVE     "key"              TO  MCP-PATHNAME
                 PERFORM   DB-CLOSE-SEC
      *
                 IF     FLG-HKNKUMI         =   ZERO                          
                    IF    COMB-HKNNUM           =    "002"                    
                        EVALUATE   COMB-HOJOKBN                               
                          WHEN   "3"                                          
                          WHEN   "C"                                          
                          WHEN   "F"                                          
                          WHEN   "I"                                          
                              MOVE     1     TO     FLG-SENIN                 
                        END-EVALUATE                                          
                    END-IF                                                    
                 END-IF                                                       
      *
      *          ¥ï¡¼¥¯¥Æ¡¼¥Ö¥ë½é´ü²½
      *
                 MOVE    SPACE               TO     ZAIBAN-TABLE
                 INITIALIZE                         ZAIBAN-TABLE
                 MOVE    SPACE               TO     HKNTEKGAI-TABLE
                 INITIALIZE                         HKNTEKGAI-TABLE
                 MOVE    SPACE               TO     JIHI-TABLE
                 INITIALIZE                         JIHI-TABLE
                 MOVE    SPACE               TO     RSI-TABLE
                 INITIALIZE                         RSI-TABLE
      *
      *          ¿ÇÎÅÎÅ²ñ·×¥Þ¥¹¥¿¤«¤éºÞÈÖ¹æÅùÂàÈòÍÑ¥Æ¡¼¥Ö¥ë¤Ø
      *
                 INITIALIZE                      SRYACCT-REC
                 MOVE    LNK-SYU-HOSPNUM(IDY)
                                             TO  ACCT-HOSPNUM
                 MOVE    LNK-SYU-NYUGAIKBN(IDY)
                                             TO  ACCT-NYUGAIKBN
                 MOVE    LNK-SYU-PTID(IDY)   TO  ACCT-PTID
                 MOVE    LNK-SYU-SRYKA(IDY)  TO  ACCT-SRYKA
                 MOVE    LNK-SYU-SRYYMD(IDY)(1:6)
                                             TO  ACCT-SRYYM
                 MOVE    LNK-SYU-HKNCOMBINUM(IDY)
                                             TO  ACCT-HKNCOMBI
                 MOVE    SRYACCT-REC         TO  MCPDATA-REC
                 MOVE    "DBSELECT"          TO  MCP-FUNC
                 MOVE    "tbl_sryacct"       TO  MCP-TABLE
                 MOVE    "key24"             TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
                 IF      MCP-RC              =   ZERO
                     MOVE    "tbl_sryacct"   TO  MCP-TABLE
                     MOVE    "key24"         TO  MCP-PATHNAME
                     PERFORM  900-SRYACCT-READ-SEC
                 ELSE
                     MOVE    1               TO  FLG-SRYACCT
                 END-IF
      *
                 MOVE    1                   TO  IDX1
      *
                 PERFORM      4401-SRYACCT-SET-SEC
                     UNTIL   (FLG-SRYACCT    =       1      ) OR
                             (IDX1           >       100    )   
      *
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key24"            TO  MCP-PATHNAME
                 PERFORM   DB-CLOSE-SEC
      *
      *          ¼ýÇ¼¹àÌÜÊÔ½¸
      *
                 PERFORM      4402-SYUNOU-HENSYU-SEC
      *
      *          Ê¡²¬ÂÐ±þ
      *
                 IF      SPA-PREFNUM         =   40
                     INITIALIZE                    ORCSSYU40AREA
                     MOVE    "2"               TO  ORCS40-KBN
                     MOVE    LNK-SYU-PTID(IDY) TO  SPA-PTID
                     MOVE    "2"               TO  SPA-NYUGAIKBN
                     MOVE    LNK-SYU-DENPNUM(IDY)
                                               TO  ORCS40-DENPNUM
                     CALL    "ORCSSYU40"     USING   SPA-AREA
                                                     ORCSSYU40AREA
                                                     SYUNOU-REC
                     PERFORM    VARYING   IDX3   FROM  1   BY  1
                                UNTIL     IDX3   >     6
                         MOVE   SYU-KYUFUGAI-TEN(IDX3)  TO
                                          LNK-SYU-KYUFUGAI-TEN(IDY IDX3)
                     END-PERFORM
                 END-IF
      *
      *          ¹ç·×¥Ç¡¼¥¿¤ÎºÆÊÔ½¸ÍÑÂàÈò
      *
                 IF   WRK-TAIHI-HKNCOMBINUM  NOT =  SPACE
                 AND  WRK-TAIHI-HKNCOMBINUM
                                     NOT =   LNK-SYU-HKNCOMBINUM(IDY)
                       INITIALIZE       WRK-SYUNOU-REC
      ***              INITIALIZE       WRK-ORCS01-REC
                 END-IF
      *
                 PERFORM   4403-GOKEI-TAIHI-SEC
      *
                 MOVE    LNK-SYU-HKNCOMBINUM(IDY)
                                         TO  WRK-TAIHI-HKNCOMBINUM
      *
               ELSE
      *
      *          ¹ç·×¥Ç¡¼¥¿¤ÎºÆÊÔ½¸
      *
                 PERFORM   4404-GOKEI-HENSYU-SEC
      *
               END-IF
      *
           END-PERFORM
      *
      *    ´µ¼ÔÉéÃ´³Û·×»»½èÍý
      *
           MOVE    "1"           TO  LNK-S01-SAIKEISAN-KBN
           MOVE    LNK-ORCSSKS-IKTSORTKBN
                                 TO  LNK-S01-IKTSORTKBN
           MOVE    LNK-ORCSSKS-IKTKEISANKBN
                                 TO  LNK-S01-IKTKEISANKBN
           MOVE    LNK-ORCSSKS-BUNBENJIHIKBN
                                 TO  LNK-S01-BUNBENJIHIKBN
           MOVE    LNK-ORCSSKS-IKTGENKEIFLG
                                 TO  LNK-S01-IKTGENKEIFLG
           IF   FLG-GENBUTU  =  1
             MOVE    "1"         TO  LNK-S01-IKTGENMKJFLG
           END-IF
      *
           CALL    "ORCSGAIFTN"        USING
                                       MCPAREA
                                       LNK-ORCS01-REC
                                       LNK-SYUNOU-REC-T
                                       SPA-AREA
      *
      *    ¤Þ¤È¤áº¹³Û»»½Ð¤ÈÀÁµá³Û»»½Ð
      *
           INITIALIZE                  WRK-SKYMONEY-TBL
           MOVE       ZERO      TO     WRK-RUI-SKYMONEY
           MOVE       SPACE     TO     WRK-TAIHI-HKNCOMBINUM
           PERFORM    VARYING   IDY    FROM   1   BY   1
                              UNTIL  (IDY                 >    15 )
                                OR   (LNK-SYU-SRYYMD(IDY) =  SPACE)
      *
             IF   LNK-SYU-SRYKA(IDY)  NOT =  "00"
      *
                 IF   WRK-TAIHI-HKNCOMBINUM  NOT =  SPACE
                 AND  WRK-TAIHI-HKNCOMBINUM
                                     NOT =   LNK-SYU-HKNCOMBINUM(IDY)
                       MOVE    ZERO    TO    WRK-RUI-SKYMONEY
                 END-IF
      *
                 MOVE    LNK-SYU-SKYMONEY(IDY)   TO   WRK-SKYMONEY(IDY)
                 ADD     LNK-SYU-SKYMONEY(IDY)   TO   WRK-RUI-SKYMONEY
      *
                 MOVE    LNK-SYU-HKNCOMBINUM(IDY)
                                       TO    WRK-TAIHI-HKNCOMBINUM
      *
             ELSE
      *
                IF     WRK-KAGRPFLG     =   "0"
      *
                   MOVE     LNK-SYU-SKYMONEY(IDY) TO   WRK-SKYMONEY(IDY)
                   COMPUTE  WRK-GRP-SGKMONEY(IDY)  =   WRK-SKYMONEY(IDY)
                                                   -   WRK-RUI-SKYMONEY
      *
                   MOVE      ZERO      TO      FLG-OK
                   COMPUTE   IDW    =  IDY  -  1
                   PERFORM   VARYING   IDZ   FROM    IDW   BY   -1
                              UNTIL   (IDZ       <       1)
                               OR     (FLG-OK    =       1)
                       COMPUTE  WRK-KEISAN  =  WRK-SKYMONEY(IDZ)
                                            +  WRK-GRP-SGKMONEY(IDY)
                       IF       WRK-KEISAN  <   0
                           CONTINUE
                       ELSE
                           MOVE     WRK-GRP-SGKMONEY(IDY)
                                              TO   WRK-GRP-SGKMONEY(IDZ)
                           MOVE     WRK-KEISAN    TO   WRK-SKYMONEY(IDZ)
                           MOVE     1         TO   FLG-OK
                       END-IF
                   END-PERFORM
      *
                END-IF
      *
             END-IF
      *
           END-PERFORM
      *
      *    ¼ýÇ¼¥Ç¡¼¥¿¹¹¿·Åù
      *
           MOVE       ZERO      TO     IDY2
      *
           PERFORM    VARYING   IDY    FROM   1   BY   1
                              UNTIL  (IDY                 >    15 )
                                OR   (LNK-SYU-SRYYMD(IDY) =  SPACE)
                                OR   (SPA-ERRCD       NOT =  SPACE)
      *
             IF   LNK-SYU-SRYKA(IDY)  NOT =  "00"
      *
      *          ¤Þ¤È¤áº¹³Û¤ÈÀÁµá³Û¤ÎÃÖ¤­´¹¤¨
      *
                 MOVE   WRK-SKYMONEY(IDY) TO   LNK-SYU-SKYMONEY(IDY)
                 MOVE   WRK-GRP-SGKMONEY(IDY)
                                          TO   LNK-SYU-GRP-SGKMONEY(IDY)
      *
      *
                 MOVE   LNK-SYU-SKYMONEY(IDY)
                                          TO   WRK-ATO-SEIKYUGK(IDY)
      *
      *          ÅÁÉ¼¾õÂÖ¶èÊ¬¤ÎºÆ¥»¥Ã¥È
      *
                 IF     LNK-SYU-SKYMONEY(IDY)  =  ZERO
                         MOVE    SPACE    TO   LNK-SYU-DENPJTIKBN(IDY)
                 ELSE
                     IF    LNK-SYU-SKYMONEY(IDY)  >
                                       LNK-SYU-NYUKIN-TOTAL(IDY)
                         MOVE    "2"      TO   LNK-SYU-DENPJTIKBN(IDY)
                     ELSE
                         MOVE    "1"      TO   LNK-SYU-DENPJTIKBN(IDY)
                     END-IF
                 END-IF
      *
      *          ²ñ·×ÊÑ¹¹¶èÊ¬¤Î½é´ü²½
      *
                 MOVE     SPACE           TO   LNK-SYU-ACCT-UPDKBN(IDY)
      *
                 PERFORM  800-MCNDATE-SEC
                 MOVE     SMCNDATE-YMD    TO   LNK-SYU-UPYMD(IDY)
                 MOVE     SMCNDATE-HMS    TO   LNK-SYU-UPHMS(IDY)
      *
      *          ¼ýÇ¼¥Þ¥¹¥¿¤Î¹¹¿·
      *
                 MOVE    LNK-SYUNOU-REC(IDY) TO  SYUNOU-REC
      ****
                 INITIALIZE                   SS002-AREA
                 CALL    "ORCSS002"   USING   SS002-AREA
                                              SYUNOU-REC
                                              SPA-AREA
                 MOVE    SS002-SYURRKNUM     TO  WRK-SYURRKNUM
                 MOVE    SS002-SYUEDANUM     TO  WRK-SYUEDANUM
                 MOVE    SS002-SYURRKUPDKBN  TO  WRK-SYURRKUPDKBN
      ****
                 MOVE    SYUNOU-REC          TO  MCPDATA-REC
                 MOVE    "DBUPDATE"          TO  MCP-FUNC
                 MOVE    "tbl_syunou"        TO  MCP-TABLE
                 MOVE    "key"               TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"         USING  MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
      *
      *          ºÆ·×»»Ê¬¤Î¼ýÇ¼ÌÀºÙ¥Þ¥¹¥¿ºîÀ®
      *
                 INITIALIZE                       SYUMEI-REC
                 MOVE    LNK-SYU-HOSPNUM(IDY) TO  SMEI-HOSPNUM
                 MOVE    LNK-SYU-NYUGAIKBN(IDY)
                                              TO  SMEI-NYUGAIKBN
                 MOVE    LNK-SYU-PTID(IDY)    TO  SMEI-PTID
                 MOVE    LNK-SYU-DENPNUM(IDY) TO  SMEI-DENPNUM
                 MOVE    SYUMEI-REC           TO  MCPDATA-REC
                 MOVE    "DBSELECT"           TO  MCP-FUNC
                 MOVE    "tbl_syumei"         TO  MCP-TABLE
                 MOVE    "key2"               TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"          USING  MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
                 IF      MCP-RC              =    ZERO
                       MOVE    "tbl_syumei"   TO  MCP-TABLE
                       MOVE    "key2"         TO  MCP-PATHNAME
                       PERFORM  900-SYUMEI-READ-SEC
                 ELSE
                       MOVE    1                  TO  FLG-SYUMEI
                 END-IF
                 IF      FLG-SYUMEI    =     1
                       CONTINUE
                 ELSE
                       PERFORM   VARYING  IDX3  FROM   1  BY   1
                           UNTIL   (FLG-SYUMEI   =      1 )
                               MOVE    "tbl_syumei"   TO  MCP-TABLE
                               MOVE    "key2"         TO  MCP-PATHNAME
                               PERFORM  900-SYUMEI-READ-SEC
                       END-PERFORM
                       IF  LNK-ORCSSKS-BUNBENJIHIKBN = "1"
                         CONTINUE
                       ELSE
                         PERFORM    4405-SMEI-SAKUSEI-SEC
                       END-IF
                 END-IF
                 MOVE     "tbl_syumei"   TO  MCP-TABLE
                 MOVE     "key2"         TO  MCP-PATHNAME
                 PERFORM   DB-CLOSE-SEC
      *
                 ADD    1         TO   IDY2
                 MOVE   LNK-SYU-DENPNUM(IDY)
                                  TO   LNK-ORCSSKS-DENPNUM-END(IDY2)
      *
             ELSE
      *
                 IF     WRK-KAGRPFLG     =   "0"
      *
      *            ¤Þ¤È¤áº¹³Û¤ÈÀÁµá³Û¤ÎÃÖ¤­´¹¤¨
      *
                   MOVE   WRK-SKYMONEY(IDY)
                                      TO   LNK-SYU-SKYMONEY(IDY)
                   MOVE   WRK-GRP-SGKMONEY(IDY)
                                      TO   LNK-SYU-GRP-SGKMONEY(IDY)
      *
      *            ÅÁÉ¼¾õÂÖ¶èÊ¬¤ÎºÆ¥»¥Ã¥È
      *
                   IF     LNK-SYU-SKYMONEY(IDY)  =  ZERO
                           MOVE    SPACE    TO   LNK-SYU-DENPJTIKBN(IDY)
                   ELSE
                       IF    LNK-SYU-SKYMONEY(IDY)  >
                                         LNK-SYU-NYUKIN-TOTAL(IDY)
                           MOVE    "2"      TO   LNK-SYU-DENPJTIKBN(IDY)
                       ELSE
                           MOVE    "1"      TO   LNK-SYU-DENPJTIKBN(IDY)
                       END-IF
                   END-IF
      *
                   PERFORM  800-MCNDATE-SEC
                   MOVE     SMCNDATE-YMD    TO   LNK-SYU-UPYMD(IDY)
                   MOVE     SMCNDATE-HMS    TO   LNK-SYU-UPHMS(IDY)
      *
      *            ¼ýÇ¼¹ç·×¥Þ¥¹¥¿¤Î¹¹¿·
      *
                   IF  LNK-ORCSSKS-BUNBENJIHIKBN = "1"
                     CONTINUE
                   ELSE
                     INITIALIZE                      SYUTTL-REC
                     PERFORM   4406-GOKEI-OKIKAE-SEC
                     MOVE    SYUTTL-REC          TO  MCPDATA-REC
                     MOVE    "DBUPDATE"          TO  MCP-FUNC
                     MOVE    "tbl_syutotal"      TO  MCP-TABLE
                     MOVE    "key"               TO  MCP-PATHNAME
                     CALL    "ORCDBMAIN"         USING  MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                   END-IF
      *
                 END-IF
      *
             END-IF
      *
           END-PERFORM
      *
           .
      *
       MAIN-EXT.
           EXIT PROGRAM
           STOP RUN.
      *
      ******************************************************************
      *    ¼ýÇ¼¹ç·×¥Þ¥¹¥¿¤¬¤¢¤ë¤«
      ******************************************************************
       4400-SYUTOTAL-FIND-SEC           SECTION.
      *
           MOVE       ZERO        TO      FLG-NG
      *
           IF    WRK-KAGRPFLG     =   "0"
             PERFORM   900-SYUTOTAL-READ-SEC
             IF    FLG-SYUTOTAL   =   ZERO
                IF  SYUTTL-HKNCOMBINUM  =  WRK-TAIHI-HKNCOMBINUM
                  ADD     1           TO  IDY
                  PERFORM    44001-SYUTOTAL-SET-SEC
      ***         MOVE    ZERO        TO  WRK-MAE-SEIKYUGK(IDY)
                ELSE
                  MOVE    1           TO  FLG-NG
                END-IF
             ELSE
                  MOVE    1           TO  FLG-NG
             END-IF
           ELSE
             PERFORM   900-SYUTOTAL-READ-SEC
             IF    FLG-SYUTOTAL  =   1
                  ADD   1                 TO   IDY
                  MOVE  LNK-ORCSSKS-HOSPNUM
                                          TO   LNK-SYU-HOSPNUM(IDY)
                  MOVE  LNK-ORCSSKS-NYUGAIKBN
                                          TO   LNK-SYU-NYUGAIKBN(IDY)
                  MOVE  LNK-ORCSSKS-PTID  TO   LNK-SYU-PTID(IDY)
                  MOVE  ZERO              TO   LNK-SYU-DENPNUM(IDY)
      *
                  MOVE  WRK-SAIKEISAN-YMD TO   LNK-SYU-SRYYMD(IDY)
                  MOVE  "00"              TO   LNK-SYU-SRYKA(IDY)
                  MOVE  WRK-TAIHI-HKNCOMBINUM
                                          TO   LNK-SYU-HKNCOMBINUM(IDY)
                  MOVE  WRK-TAIHI-SYUHKNNUM
                                          TO   LNK-SYU-SYUHKNNUM(IDY)
      *
      ***         MOVE  ZERO              TO   WRK-MAE-SEIKYUGK(IDY)
             ELSE
                  MOVE     1              TO   FLG-NG
             END-IF
           END-IF
           .
      *
       4400-SYUTOTAL-FIND-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¼ýÇ¼¹ç·×¥Þ¥¹¥¿¤¬¤¢¤ë¤«
      ******************************************************************
       4400-SYUTOTAL-FIND2-SEC           SECTION.
      *
           MOVE       ZERO        TO      FLG-NG
      *
           PERFORM   900-SYUTOTAL-READ-SEC
           IF    FLG-SYUTOTAL   =   ZERO
                  MOVE    1       TO      FLG-NG
           END-IF
      *
           .
      *
       4400-SYUTOTAL-FIND2-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¼ýÇ¼¹ç·×¥Þ¥¹¥¿¹àÌÜ¤ò¼ýÇ¼¥Þ¥¹¥¿¹àÌÜ¤Ø¥»¥Ã¥È
      ******************************************************************
       44001-SYUTOTAL-SET-SEC           SECTION.
      *£Ë£Å£Ù
           MOVE     SYUTTL-KEY            TO  LNK-SYU-KEY(IDY)
      *¿ÇÎÅ²Ê
           MOVE     SYUTTL-SRYKA          TO  LNK-SYU-SRYKA(IDY)
      *Æ±»þÆþÎÏ¿ÇÎÅ²Ê
           PERFORM   VARYING   IDX3   FROM   1    BY    1
                      UNTIL    IDX3    >     10
              MOVE  SYUTTL-DOUJI-SRYKA(IDX3)
                                          TO  WRK-DOUJI-SRYKA(IDY IDX3)
           END-PERFORM
      *¿ÇÎÅÆü
           MOVE     SYUTTL-SRYYMD         TO  LNK-SYU-SRYYMD(IDY)
      *ÅÁÉ¼¾õÂÖ¶èÊ¬
           MOVE     SYUTTL-DENPJTIKBN     TO  LNK-SYU-DENPJTIKBN(IDY)
      *ÅÁÉ¼ºîÀ®¶èÊ¬
           MOVE     SYUTTL-CREATEKBN      TO  LNK-SYU-CREATEKBN(IDY)
      *ÅÁÉ¼È¯¹ÔÆü
           MOVE     SYUTTL-DENPPRTYMD     TO  LNK-SYU-DENPPRTYMD(IDY)
      *ÀÁµá´ü´Ö
           MOVE     SYUTTL-SKY-KIKAN      TO  LNK-SYU-SKY-KIKAN(IDY)
      *ÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ
           MOVE     SYUTTL-HKNCOMBINUM    TO  LNK-SYU-HKNCOMBINUM(IDY)
      *¼çÊÝ¸±¡ÝÉéÃ´¾ðÊó
           MOVE     SYUTTL-SYUHKNNUM      TO  LNK-SYU-SYUHKNNUM(IDY)
      *
           MOVE     SYUTTL-SYUHKNFTNMONEY
                                      TO  LNK-SYU-SYUHKNFTNMONEY(IDY)
      *
           MOVE     SYUTTL-SYUCOMPFTN     TO  LNK-SYU-SYUCOMPFTN(IDY)
      *
           MOVE     SYUTTL-SYUCOMPFTN-ENTANI
                                      TO  LNK-SYU-SYUCOMPFTN-ENTANI(IDY)
      *
           MOVE     SYUTTL-SYUCOMPYKZFTN  TO  LNK-SYU-SYUCOMPYKZFTN(IDY)
      *
           MOVE     SYUTTL-SYUCOMPTOTAL   TO  LNK-SYU-SYUCOMPTOTAL(IDY)
      *
           MOVE     SYUTTL-SYUYKZFTNKBN   TO  LNK-SYU-SYUYKZFTNKBN(IDY)
      *¸øÈñ¡ÝÉéÃ´¾ðÊó
           MOVE     SYUTTL-KOH-AREA       TO  LNK-SYU-KOH-AREA(IDY)
      *´µ¼ÔÉéÃ´Î¨
           MOVE     SYUTTL-PTFTNRATE      TO  LNK-SYU-PTFTNRATE(IDY)
      *ÀÁµá½ñºîÀ®¶èÊ¬
           MOVE     SYUTTL-SKYKUKBN       TO  LNK-SYU-SKYKUKBN(IDY)
      *ÀÁµáÆâÍÆ
           MOVE     SYUTTL-SKY-NAIYOU     TO  LNK-SYU-SKY-NAIYOU(IDY)
      *½èÊý¤»¤óÎÁºÆ·Ç
           MOVE     SYUTTL-SHOHOU-SAI     TO  LNK-SYU-SHOHOU-SAI(IDY)
      *ÊÝ¸±Å¬ÍÑ³°¶â³Û¡Ý¾ÃÈñÀÇ¡ÊºÆ·Ç¡Ë
           MOVE     SYUTTL-TGMONEY-TAX-SAI
                                      TO  LNK-SYU-TGMONEY-TAX-SAI(IDY)
      *¼«Èñ£±¤«¤é£µ
           MOVE     SYUTTL-JIHI-NAIYOU    TO  LNK-SYU-JIHI-NAIYOU(IDY)
      *¼«Èñ¾®·×¡Ê¾ÃÈñÀÇ¤Ê¤·¡Ë
           MOVE     SYUTTL-JIHI-TOTAL     TO  LNK-SYU-JIHI-TOTAL(IDY)
      *¼«Èñ¾®·×¡Ê¾ÃÈñÀÇ¤¢¤ê¡Ë
           MOVE     SYUTTL-JIHI-TOTAL-TAX
                                      TO  LNK-SYU-JIHI-TOTAL-TAX(IDY)
      *¼«Èñ¶â³Û¾ÃÈñÀÇ
           MOVE     SYUTTL-JIHI-TAX       TO  LNK-SYU-JIHI-TAX(IDY)
      *Ï·¿Í°ìÉôÉéÃ´¶â
           MOVE     SYUTTL-RJN-FTN        TO  LNK-SYU-RJN-FTN(IDY)
      *¸øÈñ°ìÉôÉéÃ´¶â
           MOVE     SYUTTL-KOH-FTN        TO  LNK-SYU-KOH-FTN(IDY)
      *¸øÈñ°ìÉôÉéÃ´¶â¡Ê±ßÃ±°Ì¡Ë
           MOVE     SYUTTL-KOH-FTN-ENTANI
                                      TO  LNK-SYU-KOH-FTN-ENTANI(IDY)
      *Ä´À°¶â
           MOVE     SYUTTL-CHOSEI         TO  LNK-SYU-CHOSEI(IDY)
      *¤Þ¤È¤áÆþÎÏÅÁÉ¼ÈÖ¹æ
           MOVE     WRK-GRP-DENPNUM       TO  LNK-SYU-GRP-DENPNUM(IDY)
      *¤Þ¤È¤áÆþÎÏº¹³Û
           MOVE     SYUTTL-GRP-SGKMONEY   TO  LNK-SYU-GRP-SGKMONEY(IDY)
      *ÊÝ¸±Å¬ÍÑ¶â³Û
           MOVE     SYUTTL-HKNTEKMONEY    TO  LNK-SYU-HKNTEKMONEY(IDY)
      *ÉÂ±¡¸ºÌÈ»öÍ³¶èÊ¬
           MOVE     SYUTTL-DISCOUNT-KBN   TO  LNK-SYU-DISCOUNT-KBN(IDY)
      *ÉÂ±¡¸ºÌÈ»öÍ³
           MOVE     SYUTTL-DISCOUNT-BODY  TO  LNK-SYU-DISCOUNT-BODY(IDY)
      *³ä°úÎ¨¶èÊ¬
           MOVE     SYUTTL-DISCOUNT-RATEKBN
                                      TO  LNK-SYU-DISCOUNT-RATEKBN(IDY)
      *ÄêÎ¨Äê³Û¶èÊ¬
           MOVE     SYUTTL-DISCOUNT-TEIRITU
                                      TO  LNK-SYU-DISCOUNT-TEIRITU(IDY)
      *³ä°úÎ¨
           MOVE     SYUTTL-DISCOUNT-RATE  TO  LNK-SYU-DISCOUNT-RATE(IDY)
      *¸ºÌÈ¶â³Û
           MOVE     SYUTTL-DISCOUNT-MONEY
                                      TO  LNK-SYU-DISCOUNT-MONEY(IDY)
      *Ï«ºÒÊÝ¸±¡Ý½é¿ÇÎÁ
           MOVE     SYUTTL-RSISHOSHIN-MONEY
                                      TO  LNK-SYU-RSISHOSHIN-MONEY(IDY)
      *Ï«ºÒÊÝ¸±¡ÝºÆ¿ÇÎÁ
           MOVE     SYUTTL-RSISAISHIN-MONEY
                                      TO  LNK-SYU-RSISAISHIN-MONEY(IDY)
      *Ï«ºÒÊÝ¸±¡Ý»ØÆ³ÎÁ
           MOVE     SYUTTL-RSISDO-MONEY   TO  LNK-SYU-RSISDO-MONEY(IDY)
      *Ï«ºÒÊÝ¸±¡Ý¤½¤ÎÂ¾
           MOVE     SYUTTL-RSIETC-MONEY   TO  LNK-SYU-RSIETC-MONEY(IDY)
      *Ï«ºÒÊÝ¸±¡Ý¾ÃÈñÀÇ¡ÊºÆ·Ç¡Ë
           MOVE     SYUTTL-RSI-TAX-SAI    TO  LNK-SYU-RSI-TAX-SAI(IDY)
      *Ï«ºÒÊÝ¸±¡Ý´µ¼ÔÀÁµáÊ¬±ß·ú¤Æ¹ç·×³Û
           MOVE     SYUTTL-RSI-TOTAL      TO  LNK-SYU-RSI-TOTAL(IDY)
      *Ï«ºÒ¼«Çå¡ÝÀÁµá¶â³Û¡ÊÆü·×É½µ­ºÜÍÑ¡Ë
           MOVE     SYUTTL-RSIJIBAI-SKYMONEY
                                      TO  LNK-SYU-RSIJIBAI-SKYMONEY(IDY)
      *ÀÁµá¶â³Û¡Ý¾ÃÈñÀÇ¡ÊºÆ·Ç¡Ë
           MOVE     SYUTTL-SKYMONEY-TAX-SAI
                                      TO  LNK-SYU-SKYMONEY-TAX-SAI(IDY)
      *ÀÁµá¶â³Û
           MOVE     SYUTTL-SKYMONEY       TO  LNK-SYU-SKYMONEY(IDY)
      *Æþ¶â¹ç·×³Û
           MOVE     SYUTTL-NYUKIN-TOTAL   TO  LNK-SYU-NYUKIN-TOTAL(IDY)
      *Æþ¶â²ó¿ô
           MOVE     SYUTTL-NYUKIN-KAISU   TO  LNK-SYU-NYUKIN-KAISU(IDY)
      *Ì¤¼ýÍýÍ³
           MOVE     SYUTTL-MISYURIYU      TO  LNK-SYU-MISYURIYU(IDY)
      *ºßÁí¿Ç¶èÊ¬
           MOVE     SYUTTL-ZAITAKU        TO  LNK-SYU-ZAITAKU(IDY)
      *¿ÇÎÅ¶èÊ¬ÊÌµëÉÕ³°ÅÀ¿ô
           MOVE     SYUTTL-KYUFUGAI       TO  LNK-SYU-KYUFUGAI(IDY)
      *ÊÝ¸±Å¬ÍÑ¶èÊ¬
           MOVE     SYUTTL-HKNTEKKBN      TO  LNK-SYU-HKNTEKKBN(IDY)
      *Ã¼Ëö£É£Ä
           MOVE     SYUTTL-TERMID         TO  LNK-SYU-TERMID(IDY)
      *¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
           MOVE     SYUTTL-OPID           TO  LNK-SYU-OPID(IDY)
      *ºîÀ®Ç¯·îÆü
           MOVE     SYUTTL-CREYMD         TO  LNK-SYU-CREYMD(IDY)
      *¹¹¿·Ç¯·îÆü
           MOVE     SYUTTL-UPYMD          TO  LNK-SYU-UPYMD(IDY)
      *¹¹¿·»þ´Ö
           MOVE     SYUTTL-UPHMS          TO  LNK-SYU-UPHMS(IDY)
      *
           .
      *
       44001-SYUTOTAL-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅÎÅ²ñ·×¥Þ¥¹¥¿¤«¤éºÞÈÖ¹æÅùÂàÈòÍÑ¥Æ¡¼¥Ö¥ë¤Ø
      **************************************************************
       4401-SRYACCT-SET-SEC           SECTION.
      *
           MOVE      ZERO    TO     FLG-TAISYO
      *
           IF    ((LNK-SYU-SYUHKNNUM(IDY)  =   "971" OR "973"    ) OR
                  (LNK-SYU-KOH1HKNNUM(IDY) =   "970"             ))
           AND   ( ACCT-SRYKBN             =   "80"               )
           AND   ( ACCT-ZAITEN             =    ZERO              )
           AND   ( ACCT-DAY(SAI-IDX  WRK-SAIKEISAN-DD) NOT = ZERO )
           AND   ( ACCT-JIHOKBN            =    SPACE             )
      *
              PERFORM  44011-SRYACCTPLUS-KENSAKU-SEC
      *
           END-IF
      *
           IF    (LNK-SYU-SYUHKNNUM(IDY)
                                      NOT =   "971" AND "973"
                                                    AND "975"   )
           AND   (ACCT-SRYKBN             =   "11"              )
           AND   (ACCT-ZAITEN             =    ZERO             )
           AND   (ACCT-DAY(SAI-IDX  WRK-SAIKEISAN-DD) NOT = ZERO)
           AND   (ACCT-JIHOKBN            =    SPACE            )
      *
              PERFORM  44012-SRYACT-KENSAKU-SEC
      *
           END-IF
      *
           IF    (LNK-SYU-SYUHKNNUM(IDY)
                                      NOT =   "971" AND "973"
                                                    AND "975"   )
           AND   (ACCT-SRYKBN             =   "99"              )
           AND   (ACCT-ZAITEN             =    ZERO             )
           AND   (ACCT-DAY(SAI-IDX  WRK-SAIKEISAN-DD) NOT = ZERO)
           AND   (ACCT-JIHOKBN            =    SPACE            )
           AND   (LNK-S01-SINSATUFLG2     =    SPACE            )
      *
              PERFORM  44013-SRYACT-KENSAKU2-SEC
      *
           END-IF
      *
           IF    (LNK-SYU-SYUHKNNUM(IDY)
                                      NOT =   "971" AND "973"
                                                    AND "975"   )
           AND   (ACCT-SRYKBN             =   "99"              )
           AND   (ACCT-ZAITEN             =    ZERO             )
           AND   (ACCT-DAY(SAI-IDX  WRK-SAIKEISAN-DD) NOT = ZERO)
           AND   (ACCT-JIHOKBN            =    SPACE            )
           AND   (LNK-S01-JIKANGAI        =    SPACE            )
      *
              PERFORM  44014-SRYACT-KENSAKU3-SEC
      *
           END-IF
      *
           IF   ((ACCT-ZAITEN       NOT = ZERO                  ) AND
                 (ACCT-DAY(SAI-IDX  WRK-SAIKEISAN-DD) NOT = ZERO) AND
                 (ACCT-JIHOKBN          = SPACE                 ))
           OR   ( FLG-TAISYO            =   1                    )
      *
      *H26.10
      *       ¿ÇÎÅ¹Ô°ÙÉí²Ã¾ðÊó¤«¤é¥Þ¥¤¥Ê¥¹ÅÀ¿ôºÞ¤ÎÈ½Äê
              IF      ACCT-ZAIREQFLG     =   "1"
      *           ÉÕ²Ã¾ðÊó¤¢¤ê
                  PERFORM  44011-SRYACCTPLUS-KENSAKU-SEC
              ELSE
                  MOVE     ZERO          TO  ACCTP-PLUSKBN
              END-IF
      *
              MOVE    ACCT-SRYKBN    TO  TBL-SRYKBN(IDX1)
              MOVE    ACCT-ZAINUM    TO  TBL-ZAIBAN(IDX1)
              MOVE    ACCT-DAY(SAI-IDX  WRK-SAIKEISAN-DD)
                                     TO  TBL-ZAIKAISU(IDX1)
              IF     ACCT-ZAIKBN       =    2
                   MOVE  ACCT-ZAITEN TO  TBL-GENTEN(IDX1)
                   MOVE  ACCT-ZAIKBN TO  TBL-ZAIKBN(IDX1)
              ELSE
      *H26.10   ¥Þ¥¤¥Ê¥¹È½Äê
                IF   ACCTP-PLUSKBN     =    1
                   COMPUTE   TBL-ZAITEN(IDX1)  =  TBL-ZAITEN(IDX1) +
                                                (ACCT-ZAITEN * -1)
                ELSE
                   MOVE  ACCT-ZAITEN TO  TBL-ZAITEN(IDX1)
                END-IF
              END-IF
              IF   (   ACCT-SRYKBN       =   "95"  OR  "96"   )         
              OR   (   ACCT-ZAIKBN       =    2               )
      *H26.10   ¥Þ¥¤¥Ê¥¹È½Äê
              OR   (   ACCTP-PLUSKBN     =    1               )
                 CONTINUE                                               
              ELSE                                                      
                 COMPUTE  TBL-SYUTEN(IDX1)  =  ACCT-SYUTEN1
                                            *  TBL-ZAIKAISU(IDX1)
                 IF  (LNK-SYU-SYUHKNNUM(IDY)  =   "973" OR "975")
                 AND (ACCT-SRYKBN             =   "70"          )
                     COMPUTE  TBL-SYUTEN(IDX1) =
                              TBL-SYUTEN(IDX1) -
                             (ACCT-SYUTEN2 * TBL-ZAIKAISU(IDX1))
                 END-IF
              END-IF
              ADD     1              TO  IDX1
      *
           END-IF
      *
           MOVE    "tbl_sryacct"     TO  MCP-TABLE
           MOVE    "key24"           TO  MCP-PATHNAME
           PERFORM  900-SRYACCT-READ-SEC
           .
      *
       4401-SRYACCT-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ²ñ·×ÉÕ²Ã¾ðÊó¸¡º÷
      **************************************************************
       44011-SRYACCTPLUS-KENSAKU-SEC           SECTION.
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
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING  MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_sryacctplus"  TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC    TO  SRYACCTPLUS-REC
                   IF   ACCTP-RSIKINGAKU  NOT = ZERO
                      MOVE    1   TO   FLG-TAISYO
                   END-IF
               END-IF
           END-IF
      *
           MOVE     "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE     "key"              TO   MCP-PATHNAME
           PERFORM   DB-CLOSE-SEC
      *
           .
      *
       44011-SRYACCTPLUS-KENSAKU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¾ðÊó¸¡º÷
      **************************************************************
       44012-SRYACT-KENSAKU-SEC           SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    "099110001"          TO      SRY-SRYCD(1)
           MOVE    "099110001"          TO      SRY-SRYCD(2)
           MOVE    "099110001"          TO      SRY-SRYCD(3)
           MOVE    "099110001"          TO      SRY-SRYCD(4)
           MOVE    "099110001"          TO      SRY-SRYCD(5)
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key8"               TO      MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"        TO      MCP-FUNC
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key8"           TO      MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE   MCPDATA-REC   TO      SRYACT-REC
                   MOVE   1             TO      FLG-TAISYO
               END-IF
           END-IF
      *
           MOVE     "tbl_sryact"        TO      MCP-TABLE
           MOVE     "key8"              TO      MCP-PATHNAME
           PERFORM   DB-CLOSE-SEC
      *
           .
      *
       44012-SRYACT-KENSAKU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¾ðÊó¸¡º÷
      **************************************************************
       44013-SRYACT-KENSAKU2-SEC           SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    "099409905"          TO      SRY-SRYCD(1)
           MOVE    "099409905"          TO      SRY-SRYCD(2)
           MOVE    "099409905"          TO      SRY-SRYCD(3)
           MOVE    "099409905"          TO      SRY-SRYCD(4)
           MOVE    "099409905"          TO      SRY-SRYCD(5)
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key8"               TO      MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"        TO      MCP-FUNC
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key8"           TO      MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE   MCPDATA-REC   TO      SRYACT-REC
                   MOVE   "1"           TO      LNK-S01-SINSATUFLG2
               END-IF
           END-IF
      *
           MOVE     "tbl_sryact"        TO      MCP-TABLE
           MOVE     "key8"              TO      MCP-PATHNAME
           PERFORM   DB-CLOSE-SEC
      *
           .
      *
       44013-SRYACT-KENSAKU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¾ðÊó¸¡º÷
      **************************************************************
       44014-SRYACT-KENSAKU3-SEC           SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    "099409907"          TO      SRY-SRYCD(1)
           MOVE    "099409907"          TO      SRY-SRYCD(2)
           MOVE    "099409907"          TO      SRY-SRYCD(3)
           MOVE    "099409907"          TO      SRY-SRYCD(4)
           MOVE    "099409907"          TO      SRY-SRYCD(5)
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key8"               TO      MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"        TO      MCP-FUNC
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key8"           TO      MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE   MCPDATA-REC   TO      SRYACT-REC
                   MOVE   "1"           TO      LNK-S01-JIKANGAI
               END-IF
           END-IF
      *
           MOVE     "tbl_sryact"        TO      MCP-TABLE
           MOVE     "key8"              TO      MCP-PATHNAME
           PERFORM   DB-CLOSE-SEC
      *
           .
      *
       44014-SRYACT-KENSAKU3-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¼ýÇ¼¹àÌÜÊÔ½¸
      ******************************************************************
       4402-SYUNOU-HENSYU-SEC           SECTION.
      *
           MOVE      ZERO     TO     FLG-SENIN-SYOSIN
      *
           MOVE      ZERO     TO     WRK-TYK-HKNTEN
           MOVE      ZERO     TO     WRK-TYK-HKNTEN2
           MOVE      ZERO     TO     IDX2
           MOVE      ZERO     TO     IDX5
      *
           PERFORM   VARYING      IDX1    FROM    1    BY    1
                      UNTIL     (IDX1               >   100  )
                       OR       (TBL-SRYKBN(IDX1)   =   SPACE)
      *
               EVALUATE    TBL-SRYKBN  (IDX1)
      *            ¿ÇÎÅ
                   WHEN    "11"
                    COMPUTE LNK-SYU-HKNTEN(IDY 01) =
                            LNK-SYU-HKNTEN(IDY 01) +
                           (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
                    IF   (TBL-SRYKBN(IDX1)  =  "11")
                    AND  (FLG-SENIN         =    1 )
                         MOVE    1     TO     FLG-SENIN-SYOSIN
                    END-IF
                   WHEN    "12"
                    IF   TBL-ZAIKBN(IDX1) =  2
                      COMPUTE WRK-SYUG-HKNTEN(01) =
                              WRK-SYUG-HKNTEN(01) +
                             (TBL-GENTEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
                    ELSE
                      COMPUTE LNK-SYU-HKNTEN(IDY 01) =
                              LNK-SYU-HKNTEN(IDY 01) +
                             (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
                    END-IF
      *            »ØÆ³
                   WHEN    "13"
                    IF   TBL-ZAIKBN(IDX1) =  2
                      COMPUTE WRK-SYUG-HKNTEN(02) =
                              WRK-SYUG-HKNTEN(02) +
                             (TBL-GENTEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
                    ELSE
                      COMPUTE LNK-SYU-HKNTEN(IDY 02) =
                              LNK-SYU-HKNTEN(IDY 02) +
                             (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
                    END-IF
      *            ºßÂð
                   WHEN    "14"
                    IF   TBL-ZAIKBN(IDX1) =  2
                      COMPUTE WRK-SYUG-HKNTEN(03) =
                              WRK-SYUG-HKNTEN(03) +
                             (TBL-GENTEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
                    ELSE
                      COMPUTE LNK-SYU-HKNTEN(IDY 03) =
                              LNK-SYU-HKNTEN(IDY 03) +
                             (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
                    END-IF
      *            ÅêÌô
                   WHEN    "21"   THRU  "27"
                    COMPUTE WRK-TYK-HKNTEN  =
                            WRK-TYK-HKNTEN  +
                           (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *            Ãí¼Í
                   WHEN    "31"
                   WHEN    "32"
                   WHEN    "33"
                    COMPUTE LNK-SYU-HKNTEN(IDY 05) =
                            LNK-SYU-HKNTEN(IDY 05) +
                           (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *            ½èÃÖ
                   WHEN    "40"
                    COMPUTE LNK-SYU-HKNTEN(IDY 06) =
                            LNK-SYU-HKNTEN(IDY 06) +
                           (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *            ¼ê½Ñ
                   WHEN    "50"
                    COMPUTE LNK-SYU-HKNTEN(IDY 07) =
                            LNK-SYU-HKNTEN(IDY 07) +
                           (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *            Ëã¿ì
                   WHEN    "54"
                    COMPUTE LNK-SYU-HKNTEN(IDY 08) =
                            LNK-SYU-HKNTEN(IDY 08) +
                           (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *            ¸¡ºº
                   WHEN    "60"
                    COMPUTE LNK-SYU-HKNTEN(IDY 09) =
                            LNK-SYU-HKNTEN(IDY 09) +
                           (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *            ÉÂÍý¿ÇÃÇ
                   WHEN    "64"
                    COMPUTE LNK-SYU-HKNTEN(IDY 14) =
                            LNK-SYU-HKNTEN(IDY 14) +
                           (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *            £ØÀþ
                   WHEN    "70"
                    COMPUTE LNK-SYU-HKNTEN(IDY 10) =
                            LNK-SYU-HKNTEN(IDY 10) +
                           (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *            ¤½¤ÎÂ¾
                   WHEN    "80"
                    INITIALIZE                       SRYACT-REC
                    MOVE    LNK-SYU-HOSPNUM(IDY)
                                                TO   SRY-HOSPNUM
                    MOVE    LNK-SYU-NYUGAIKBN(IDY)
                                                TO   SRY-NYUGAIKBN
                    MOVE    LNK-SYU-PTID(IDY)   TO   SRY-PTID
                    MOVE    LNK-SYU-SRYKA(IDY)  TO   SRY-SRYKA
                    MOVE    LNK-SYU-SRYYMD(IDY)(1:6)
                                                TO   SRY-SRYYM
                    MOVE    TBL-ZAIBAN(IDX1)    TO   SRY-ZAINUM
                    MOVE    SRYACT-REC          TO   MCPDATA-REC
                    MOVE    "DBSELECT"          TO   MCP-FUNC
                    MOVE    "tbl_sryact"        TO   MCP-TABLE
                    MOVE    "key2"              TO   MCP-PATHNAME
                    CALL    "ORCDBMAIN"         USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                    IF      MCP-RC             =       ZERO
                      MOVE     "tbl_sryact"     TO   MCP-TABLE
                      MOVE     "key2"           TO   MCP-PATHNAME
                      PERFORM   900-SRYACT-READ-SEC
                    ELSE
                      MOVE    1                 TO   FLG-SRYACT
                    END-IF
                    IF      FLG-SRYACT          =   ZERO
                      EVALUATE   SRY-SRYSYUKBN
      *                ¥ê¥Ï
                       WHEN         "800"
                       WHEN         "809"
      ***              WHEN         "810"
                        COMPUTE LNK-SYU-HKNTEN(IDY 11) =
                                LNK-SYU-HKNTEN(IDY 11) +
                              (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *                ½èÊý¤»¤óÎÁ
                       WHEN         "820"
                         COMPUTE WRK-TYK-HKNTEN2 =
                                 WRK-TYK-HKNTEN2 +
                              (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *                Àº¿À²ÊÀìÌçÎÅË¡
                       WHEN         "830"
                        COMPUTE LNK-SYU-HKNTEN(IDY 12) =
                                LNK-SYU-HKNTEN(IDY 12) +
                              (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *                Êü¼ÍÀþ
                       WHEN         "840"
                        COMPUTE LNK-SYU-HKNTEN(IDY 13) =
                                LNK-SYU-HKNTEN(IDY 13) +
                              (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *                ÎÅÍÜÃ´Åö¼êÅö
                       WHEN         "850"
                        COMPUTE LNK-SYU-HKNTEN(IDY 16) =
                                LNK-SYU-HKNTEN(IDY 16) +
                              (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
      *                Æþ±¡ÎÁ
                       WHEN         "890"
                        COMPUTE LNK-SYU-HKNTEN(IDY 15) =
                                LNK-SYU-HKNTEN(IDY 15) +
                              (TBL-ZAITEN(IDX1)  *  TBL-ZAIKAISU(IDX1))
                      END-EVALUATE
                    END-IF
                    MOVE     "tbl_sryact"     TO   MCP-TABLE
                    MOVE     "key2"           TO   MCP-PATHNAME
                    PERFORM   DB-CLOSE-SEC
               END-EVALUATE
      *
               IF    (IDY                 =     1  )
               AND   (LNK-S01-SYOSINFLG   =   SPACE)
                   IF  LNK-SYU-SYUHKNNUM(IDY)   =   "971"  OR  "973"
                                                           OR  "975"
                      CONTINUE
                   ELSE
                      PERFORM  44025-SYOSINFLG-SET-SEC
                   END-IF
               END-IF
      *
               IF    FLG-NOGRP          =     1
      *
                 IF  TBL-SRYKBN(IDX1)   =   "21"   OR  "22"  OR  "23"
      *
      *            ÌôºÞ¥³¡¼¥ÉÅù¤Î¥»¥Ã¥È
                   INITIALIZE                       SRYACT-REC
                   MOVE    LNK-SYU-HOSPNUM(IDY)
                                               TO   SRY-HOSPNUM
                   MOVE    LNK-SYU-NYUGAIKBN(IDY)
                                               TO   SRY-NYUGAIKBN
                   MOVE    LNK-SYU-PTID(IDY)   TO   SRY-PTID
                   MOVE    LNK-SYU-SRYKA(IDY)  TO   SRY-SRYKA
                   MOVE    LNK-SYU-SRYYMD(IDY)(1:6)
                                               TO   SRY-SRYYM
                   MOVE    TBL-ZAIBAN(IDX1)    TO   SRY-ZAINUM
                   MOVE    SRYACT-REC          TO   MCPDATA-REC
                   MOVE    "DBSELECT"          TO   MCP-FUNC
                   MOVE    "tbl_sryact"        TO   MCP-TABLE
                   MOVE    "key2"              TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC             =       ZERO
                     MOVE     "tbl_sryact"     TO   MCP-TABLE
                     MOVE     "key2"           TO   MCP-PATHNAME
                     PERFORM   900-SRYACT-READ-SEC
                     PERFORM  UNTIL  (FLG-SRYACT   =    1)
                          EVALUATE   LNK-SYU-INGAISHOHOKBN(IDY)
                               WHEN   "0"
                                   EVALUATE   SRY-SRYSYUKBN
                                       WHEN   "210"
                                       WHEN   "220"
                                       WHEN   "230"
                                       WHEN   "290"
                                       WHEN   "211"
                                       WHEN   "221"
                                       WHEN   "231"
                                       WHEN   "291"
      *H26.10
                                       WHEN   "293"
                                       WHEN   "294"
                                       WHEN   "   "
                                          PERFORM  44021-YAKUZAI-SET-SEC
                                   END-EVALUATE       
                               WHEN   "1"
                                   EVALUATE   SRY-SRYSYUKBN
                                       WHEN   "211"
                                       WHEN   "221"
                                       WHEN   "231"
                                       WHEN   "291"
      *H26.10
                                       WHEN   "294"
                                          PERFORM  44021-YAKUZAI-SET-SEC
                                   END-EVALUATE       
                          END-EVALUATE
                          MOVE     "tbl_sryact"     TO   MCP-TABLE
                          MOVE     "key2"           TO   MCP-PATHNAME
                          PERFORM   900-SRYACT-READ-SEC
                     END-PERFORM
                   END-IF
                   MOVE     "tbl_sryact"     TO   MCP-TABLE
                   MOVE     "key2"           TO   MCP-PATHNAME
                   PERFORM   DB-CLOSE-SEC
      *
                 END-IF
      *
               END-IF
      *
               IF  TBL-SRYKBN(IDX1)       =   "95"   OR  "96"
                   INITIALIZE                        SRYACT-REC
                   MOVE    LNK-SYU-HOSPNUM(IDY) TO   SRY-HOSPNUM
                   MOVE    LNK-SYU-NYUGAIKBN(IDY)
                                                TO   SRY-NYUGAIKBN
                   MOVE    LNK-SYU-PTID(IDY)    TO   SRY-PTID
                   MOVE    LNK-SYU-SRYKA(IDY)   TO   SRY-SRYKA
                   MOVE    LNK-SYU-SRYYMD(IDY)(1:6)
                                                TO   SRY-SRYYM
                   MOVE    TBL-ZAIBAN(IDX1)     TO   SRY-ZAINUM
                   MOVE    SRYACT-REC           TO   MCPDATA-REC
                   MOVE    "DBSELECT"           TO   MCP-FUNC
                   MOVE    "tbl_sryact"         TO   MCP-TABLE
                   MOVE    "key2"               TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"          USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                   IF      MCP-RC             =       ZERO
                     MOVE     "tbl_sryact"      TO   MCP-TABLE
                     MOVE     "key2"            TO   MCP-PATHNAME
                     PERFORM   900-SRYACT-READ-SEC
                     PERFORM  UNTIL  (FLG-SRYACT   =    1)
      *                   ÊÝ¸±Å¬ÍÑ³°¤Î¥Æ¡¼¥Ö¥ë¥»¥Ã¥È
                          PERFORM    44022-HKNTEKGAI-TBL-SEC
      *
                          MOVE     "tbl_sryact" TO   MCP-TABLE
                          MOVE     "key2"       TO   MCP-PATHNAME
                          PERFORM   900-SRYACT-READ-SEC
                     END-PERFORM
                   END-IF
                   MOVE     "tbl_sryact"     TO   MCP-TABLE
                   MOVE     "key2"           TO   MCP-PATHNAME
                   PERFORM   DB-CLOSE-SEC
               END-IF
      *
               IF  (LNK-SYU-SYUHKNNUM(IDY)   =   "971"  OR  "973"
                                                        OR  "975")
               OR  (LNK-SYU-KOH1HKNNUM(IDY)  =   "970"           )
                 IF  ( TBL-SRYKBN(IDX1)         =   "95" OR "96")
                 OR  ( TBL-ZAIKBN(IDX1)         =    2          )
                 OR  ((LNK-SYU-KOH1HKNNUM(IDY)  =   "970"      ) AND
                      (TBL-SRYKBN(IDX1)     NOT =   "80"       ))
                   CONTINUE                                            
                 ELSE                                                  
                   INITIALIZE                        SRYACT-REC
                   MOVE    LNK-SYU-HOSPNUM(IDY) TO   SRY-HOSPNUM
                   MOVE    LNK-SYU-NYUGAIKBN(IDY)
                                                TO   SRY-NYUGAIKBN
                   MOVE    LNK-SYU-PTID(IDY)    TO   SRY-PTID
                   MOVE    LNK-SYU-SRYKA(IDY)   TO   SRY-SRYKA
                   MOVE    LNK-SYU-SRYYMD(IDY)(1:6)
                                                TO   SRY-SRYYM
                   MOVE    TBL-ZAIBAN(IDX1)     TO   SRY-ZAINUM
                   MOVE    SRYACT-REC           TO   MCPDATA-REC
                   MOVE    "DBSELECT"           TO   MCP-FUNC
                   MOVE    "tbl_sryact"         TO   MCP-TABLE
                   MOVE    "key2"               TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"          USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                   IF      MCP-RC             =       ZERO
                     MOVE     "tbl_sryact"      TO   MCP-TABLE
                     MOVE     "key2"            TO   MCP-PATHNAME
                     PERFORM   900-SRYACT-READ-SEC
                     PERFORM  UNTIL  (FLG-SRYACT   =    1)
      *                   ÉéÃ´¶â³Û¤Î¼êµ»ÅÀ»»½ÐÅù
                          IF   LNK-SYU-KOH1HKNNUM(IDY)  =   "970"
                             PERFORM    44026-SYUTEN-KEISAN2-SEC
                          ELSE
                             PERFORM    44023-SYUTEN-KEISAN-SEC
                          END-IF
      *
                          MOVE     "tbl_sryact" TO   MCP-TABLE
                          MOVE     "key2"       TO   MCP-PATHNAME
                          PERFORM   900-SRYACT-READ-SEC
                     END-PERFORM
                   END-IF
                   MOVE     "tbl_sryact"        TO   MCP-TABLE
                   MOVE     "key2"              TO   MCP-PATHNAME
                   PERFORM   DB-CLOSE-SEC
                 END-IF                                                
               END-IF                                                  
      *
           END-PERFORM
      *
      *    Ï«ºÒ¼«ÇåÀÕÊÝ¸±½èÍý
           IF  LNK-SYU-SYUHKNNUM(IDY)       =   "971"  OR "973"
             COMPUTE  LNK-SYU-RSISHOSHIN-MONEY(IDY)  =
                      WK-RSISHOSHIN   *   10
             IF       LNK-SYU-RSISHOSHIN-MONEY(IDY)     NOT =    ZERO
                   MOVE    ZERO           TO  WK-RSISHOSHIN-TENSU
                   COMPUTE WK-RSISHOSHIN-TENSU  =
                           LNK-SYU-RSISHOSHIN-MONEY(IDY)  /   10
                   COMPUTE LNK-SYU-HKNTEN(IDY 01)  =
                           LNK-SYU-HKNTEN(IDY 01)  -
                           WK-RSISHOSHIN-TENSU
             END-IF
             COMPUTE  LNK-SYU-RSISAISHIN-MONEY(IDY)  =
                      WK-RSISAISHIN   *   10
             IF       LNK-SYU-RSISAISHIN-MONEY(IDY)     NOT =    ZERO
                   MOVE    ZERO           TO  WK-RSISAISHIN-TENSU
                   COMPUTE WK-RSISAISHIN-TENSU  =
                           LNK-SYU-RSISAISHIN-MONEY(IDY)  /   10
                   COMPUTE LNK-SYU-HKNTEN(IDY 01)  =
                           LNK-SYU-HKNTEN(IDY 01)  -
                           WK-RSISAISHIN-TENSU
             END-IF
             COMPUTE  LNK-SYU-RSISDO-MONEY(IDY)      =
                      WK-RSISDO       *   10
             IF       LNK-SYU-RSISDO-MONEY(IDY)         NOT =    ZERO
                   MOVE    ZERO           TO  WK-RSISDO-TENSU
                   COMPUTE WK-RSISDO-TENSU   =
                           LNK-SYU-RSISDO-MONEY(IDY)      /   10
                   COMPUTE LNK-SYU-HKNTEN(IDY 02)   =
                           LNK-SYU-HKNTEN(IDY 02)   -
                           WK-RSISDO-TENSU
             END-IF
             COMPUTE  LNK-SYU-RSIETC-MONEY(IDY)      =
                      WK-RSIETC       *   10
             IF       LNK-SYU-RSIETC-MONEY(IDY)         NOT =    ZERO
                   MOVE    ZERO           TO  WK-RSIETC-TENSU
                   COMPUTE WK-RSIETC-TENSU   =
                           LNK-SYU-RSIETC-MONEY(IDY)      /   10
                   COMPUTE LNK-SYU-HKNTEN(IDY 11)   =
                           LNK-SYU-HKNTEN(IDY 11)   -
                           WK-RSIETC-TENSU
             END-IF
             MOVE     WK-RSIETC2    TO   LNK-SYU-RSIETC-MONEY(IDY)
           END-IF
      *
           IF  LNK-SYU-KOH1HKNNUM(IDY)      =   "970"
             COMPUTE  LNK-SYU-RSIETC-MONEY(IDY) = WK-RSIETC * 10
             IF   LNK-SYU-RSIETC-MONEY(IDY) NOT = ZERO
               MOVE   ZERO        TO  WK-RSIETC-TENSU
               COMPUTE WK-RSIETC-TENSU = LNK-SYU-RSIETC-MONEY(IDY) / 10
               COMPUTE LNK-SYU-HKNTEN(IDY 11) = LNK-SYU-HKNTEN(IDY 11)
                                              - WK-RSIETC-TENSU
             END-IF
             MOVE     WK-RSIETC2  TO  LNK-SYU-RSIETC-MONEY(IDY)
           END-IF
      *
           IF   LNK-SYU-HKNTEN(IDY 01) >= WRK-SYUG-HKNTEN(01)
              COMPUTE  LNK-SYU-HKNTEN(IDY 01) =  LNK-SYU-HKNTEN(IDY 01)
                                              -  WRK-SYUG-HKNTEN(01)
           ELSE
              MOVE     ZERO    TO    LNK-SYU-HKNTEN(IDY 01)
           END-IF
           IF   LNK-SYU-HKNTEN(IDY 02) >= WRK-SYUG-HKNTEN(02)
              COMPUTE  LNK-SYU-HKNTEN(IDY 02) =  LNK-SYU-HKNTEN(IDY 02)
                                              -  WRK-SYUG-HKNTEN(02)
           ELSE
              MOVE     ZERO    TO    LNK-SYU-HKNTEN(IDY 02)
           END-IF
           IF   LNK-SYU-HKNTEN(IDY 03) >= WRK-SYUG-HKNTEN(03)
              COMPUTE  LNK-SYU-HKNTEN(IDY 03) =  LNK-SYU-HKNTEN(IDY 03)
                                              -  WRK-SYUG-HKNTEN(03)
           ELSE
              MOVE     ZERO    TO    LNK-SYU-HKNTEN(IDY 03)
           END-IF
      *
      *    ¼«ÇåÀÕÊÝ¸±½èÍý
           IF  LNK-SYU-SYUHKNNUM(IDY)  =   "973"
      *
             COMPUTE   WK-SYUTEN-KEI  =  WK-SYUTEN-KEI
                                      -  WRK-SYUG-HKNTEN(01)
                                      -  WRK-SYUG-HKNTEN(02)
                                      -  WRK-SYUG-HKNTEN(03)
      *
             COMPUTE   LNK-S01-SYUTEN(IDY)  =   WK-SYUTEN-KEI
                                            -   WK-RSISHOSHIN
                                            -   WK-RSISAISHIN
                                            -   WK-RSISDO
                                            -   WK-RSIETC
                                            -   WK-YOUBUKASAN
                                            -   WK-KYOBUKASAN
                                            -   WK-KEIBUKASAN
                                            -   WK-SOGAIKASAN
                                            -   WK-RYOYOTEATE
      *
             MOVE      WK-SINDAN       TO   LNK-S01-SINDAN(IDY)
             MOVE      WK-MEISAI       TO   LNK-S01-MEISAI(IDY)
      *
             MOVE      WK-RYOYOTEATE   TO   LNK-S01-RYOYO(IDY)
      *
             MOVE      WK-KYUKYU       TO   LNK-S01-KYUKYU(IDY)
      *
           END-IF
      *
      *    ¸ø³²½èÍý
           IF  LNK-SYU-SYUHKNNUM(IDY)  =   "975"
      *
             COMPUTE   WK-SYUTEN-KEI  =  WK-SYUTEN-KEI
                                      -  WRK-SYUG-HKNTEN(01)
                                      -  WRK-SYUG-HKNTEN(02)
                                      -  WRK-SYUG-HKNTEN(03)
      *
             COMPUTE   LNK-S01-SYUTEN(IDY)  =   WK-SYUTEN-KEI
                                            -   WK-KOGAI-TEN
      *
           END-IF
      *
           IF      FLG-SENIN-SYOSIN   =   1
                 MOVE     "1"          TO   LNK-S01-SENINFLG(IDY)
           END-IF
      *
           MOVE        WRK-TYK-HKNTEN  TO   LNK-SYU-HKNTEN(IDY 04)
           ADD         WRK-TYK-HKNTEN2 TO   LNK-SYU-HKNTEN(IDY 04)
           MOVE        WRK-TYK-HKNTEN2 TO   LNK-SYU-SHOHOU-SAI(IDY)
      *
      *    ÊÝ¸±Å¬ÍÑ³°¶â³Û¤Î½¸·×¥Æ¡¼¥Ö¥ë¤«¤é¼ýÇ¼¥Æ¡¼¥Ö¥ë¤Ø¤Î¥»¥Ã¥È
           PERFORM     44024-HKNTEKGAI-SET-SEC
      *
           PERFORM   VARYING   IDX3     FROM    1   BY  1
                      UNTIL    IDX3      >     16
               COMPUTE   LNK-SYU-HKNTEN(IDY 17)  =
                         LNK-SYU-HKNTEN(IDY 17)  +
                         LNK-SYU-HKNTEN(IDY IDX3)
           END-PERFORM
           .
      *
       4402-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ÌôºÞ¥³¡¼¥ÉÅù¤Î¥»¥Ã¥È
      **************************************************************
       44021-YAKUZAI-SET-SEC           SECTION.
      *
           PERFORM     VARYING    IDX3    FROM    1     BY    1
                         UNTIL   (IDX3              >   5    )  OR
                                 (SRY-SRYCD(IDX3)   =   SPACE)
               IF     (SRY-AUTOKBN(IDX3)    =   SPACE)  AND
                      (SRY-SRYCD(IDX3)(1:1) =   "6"  )
                   ADD     1                  TO  IDX2
                   MOVE    SRY-SRYKBN         TO  LNK-S01-SRYKBN(1 IDX2)
                   MOVE    SRY-ZAINUM         TO  LNK-S01-ZAIBAN(1 IDX2)
                   MOVE    SRY-SRYCD(IDX3)    TO  LNK-S01-SRYCD (1 IDX2)
                   MOVE    SRY-SRYSURYO(IDX3) TO  LNK-S01-SURYOU(1 IDX2)
                   MOVE    TBL-ZAIKAISU(IDX1) TO  LNK-S01-KAISUU(1 IDX2)
                   MOVE    TBL-ZAITEN(IDX1)   TO  LNK-S01-TENSUU(1 IDX2)
               END-IF
           END-PERFORM
           .
      *
       44021-YAKUZAI-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    ÊÝ¸±Å¬ÍÑ³°¤Î¥Æ¡¼¥Ö¥ë¥»¥Ã¥È
      **************************************************************
       44022-HKNTEKGAI-TBL-SEC          SECTION.
      *
           EVALUATE    SRY-SRYKBN
               WHEN    "95"
                   PERFORM     VARYING    IDX3    FROM    1     BY    1
                                 UNTIL   (IDX3         >   5   )   OR
       	                                 (SRY-SRYCD(IDX3)  =   SPACE)
                       IF    SRY-SRYCD (IDX3) (1:3)   =   "095"
                                                      OR  "096"
                           IF    SRY-SRYCD (IDX3) (4:2)    =   "00"
                                                      OR       "91"
                                                      OR       "92"
                                                      OR       "93"
                                                      OR       "94"
                               INITIALIZE    TNS-TENSU-REC
                               MOVE    SRY-SRYCD(IDX3)
                                                 TO   TNS-SRYCD
                               MOVE    LNK-SYU-HOSPNUM(IDY)
                                                 TO   TNS-HOSPNUM
                               MOVE    LNK-SYU-SRYYMD(IDY)
                                                 TO   TNS-YUKOSTYMD
                               MOVE    LNK-SYU-SRYYMD(IDY)
                                                 TO   TNS-YUKOEDYMD
                               PERFORM    900-TENSU-READ-SEC
                               IF      FLG-TENSU   =    ZERO
                                   EVALUATE    TNS-GAITENTTLKBN
                                       WHEN    1
                                          COMPUTE  TBL-JIHI-TAXNASI(1)
                                               =   TBL-JIHI-TAXNASI(1)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    2
                                          COMPUTE  TBL-JIHI-TAXNASI(2)
                                               =   TBL-JIHI-TAXNASI(2)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    3
                                          COMPUTE  TBL-JIHI-TAXNASI(3)
                                               =   TBL-JIHI-TAXNASI(3)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    4
                                          COMPUTE  TBL-JIHI-TAXNASI(4)
                                               =   TBL-JIHI-TAXNASI(4)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    5
                                          COMPUTE  TBL-JIHI-TAXNASI(5)
                                               =   TBL-JIHI-TAXNASI(5)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    6
                                          COMPUTE  TBL-JIHI-TAXNASI(6)
                                               =   TBL-JIHI-TAXNASI(6)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    7
                                          COMPUTE  TBL-JIHI-TAXNASI(7)
                                               =   TBL-JIHI-TAXNASI(7)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    8
                                          COMPUTE  TBL-JIHI-TAXNASI(8)
                                               =   TBL-JIHI-TAXNASI(8)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    9
                                          COMPUTE  TBL-JIHI-TAXNASI(9)
                                               =   TBL-JIHI-TAXNASI(9)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    10
                                          COMPUTE  TBL-JIHI-TAXNASI(10)
                                               =   TBL-JIHI-TAXNASI(10)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                   END-EVALUATE
                               END-IF
                           ELSE
                               EVALUATE   SRY-SRYCD (IDX3) (4:2)
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY(1)
                                            =   TBL-TGMONEY(1)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY(2)
                                            =   TBL-TGMONEY(2)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY(3)
                                            =   TBL-TGMONEY(3)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY(4)
                                            =   TBL-TGMONEY(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY(5)
                                            =   TBL-TGMONEY(5)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY(6)
                                            =   TBL-TGMONEY(6)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "50"
                                       COMPUTE  TBL-TGMONEY(7)
                                            =   TBL-TGMONEY(7)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY(8)
                                            =   TBL-TGMONEY(8)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY(9)
                                            =   TBL-TGMONEY(9)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "64"
                                       COMPUTE  TBL-TGMONEY(14)
                                            =   TBL-TGMONEY(14)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY(10)
                                            =   TBL-TGMONEY(10)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "80"
      ***                          WHEN   "81"
                                       COMPUTE  TBL-TGMONEY(11)
                                            =   TBL-TGMONEY(11)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "82"
                                       COMPUTE  TBL-TGMONEY(4)
                                            =   TBL-TGMONEY(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "83"
                                       COMPUTE  TBL-TGMONEY(12)
                                            =   TBL-TGMONEY(12)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "84"
                                       COMPUTE  TBL-TGMONEY(13)
                                            =   TBL-TGMONEY(13)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "85"
                                       COMPUTE  TBL-TGMONEY(16)
                                            =   TBL-TGMONEY(16)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "89"
                                       COMPUTE  TBL-TGMONEY(15)
                                            =   TBL-TGMONEY(15)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   OTHER
                                       COMPUTE  TBL-TGMONEY(11)
                                            =   TBL-TGMONEY(11)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                               END-EVALUATE
                           END-IF
                       ELSE
                           INITIALIZE    TNS-TENSU-REC
                           MOVE    SRY-SRYCD(IDX3) TO   TNS-SRYCD
                           MOVE    LNK-SYU-HOSPNUM(IDY)
                                                   TO   TNS-HOSPNUM
                           MOVE    LNK-SYU-SRYYMD(IDY)
                                                   TO   TNS-YUKOSTYMD
                           MOVE    LNK-SYU-SRYYMD(IDY)
                                                   TO   TNS-YUKOEDYMD
                           PERFORM    900-TENSU-READ-SEC
                           IF      FLG-TENSU   =    ZERO
                               EVALUATE   TNS-SRYKBN
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY(1)
                                            =   TBL-TGMONEY(1)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY(2)
                                            =   TBL-TGMONEY(2)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY(3)
                                            =   TBL-TGMONEY(3)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY(4)
                                            =   TBL-TGMONEY(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY(5)
                                            =   TBL-TGMONEY(5)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY(6)
                                            =   TBL-TGMONEY(6)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "50"
                                       COMPUTE  TBL-TGMONEY(7)
                                            =   TBL-TGMONEY(7)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY(8)
                                            =   TBL-TGMONEY(8)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY(9)
                                            =   TBL-TGMONEY(9)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "64"
                                       COMPUTE  TBL-TGMONEY(14)
                                            =   TBL-TGMONEY(14)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY(10)
                                            =   TBL-TGMONEY(10)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "80"
                                     EVALUATE   TNS-SRYSYUKBN
                                      WHEN       "800"
      ***                             WHEN       "810"
                                       COMPUTE  TBL-TGMONEY(11)
                                            =   TBL-TGMONEY(11)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                      WHEN       "820"
                                       COMPUTE  TBL-TGMONEY(4)
                                            =   TBL-TGMONEY(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                      WHEN       "830"
                                       COMPUTE  TBL-TGMONEY(12)
                                            =   TBL-TGMONEY(12)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                      WHEN       "840"
                                       COMPUTE  TBL-TGMONEY(13)
                                            =   TBL-TGMONEY(13)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                      WHEN       "850"
                                       COMPUTE  TBL-TGMONEY(16)
                                            =   TBL-TGMONEY(16)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                      WHEN       "890"
                                       COMPUTE  TBL-TGMONEY(15)
                                            =   TBL-TGMONEY(15)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                     END-EVALUATE
                                   WHEN   OTHER
                                     IF   TNS-SRYCD(1:1)    =   "6"
                                       IF    TNS-YKZKBN     =    4
                                          COMPUTE  TBL-TGMONEY(5)
                                               =   TBL-TGMONEY(5)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       ELSE
                                          COMPUTE  TBL-TGMONEY(4)
                                               =   TBL-TGMONEY(4)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       END-IF
                                     END-IF
                                     IF   TNS-SRYCD(1:1)    =   "7"
                                     OR   TNS-SRYCD(1:3)    =   "059"
                                       COMPUTE  TBL-TGMONEY(11)
                                            =   TBL-TGMONEY(11)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                     END-IF
                               END-EVALUATE
                           END-IF
                       END-IF
                   END-PERFORM
               WHEN    "96"
                   PERFORM     VARYING    IDX3    FROM   1    BY   1
                                 UNTIL   (IDX3        >   5    )   OR
                                         (SRY-SRYCD(IDX3)  =   SPACE)
                       IF    SRY-SRYCD (IDX3) (1:3)   =   "095"
                                                      OR  "096"
                           IF    SRY-SRYCD (IDX3) (4:2)    =   "00"
                                                      OR       "91"
                                                      OR       "92"
                                                      OR       "93"
                                                      OR       "94"
                               INITIALIZE    TNS-TENSU-REC
                               MOVE    SRY-SRYCD(IDX3)
                                                 TO   TNS-SRYCD
                               MOVE    LNK-SYU-HOSPNUM(IDY)
                                                 TO   TNS-HOSPNUM
                               MOVE    LNK-SYU-SRYYMD(IDY)
                                                 TO   TNS-YUKOSTYMD
                               MOVE    LNK-SYU-SRYYMD(IDY)
                                                 TO   TNS-YUKOEDYMD
                               PERFORM    900-TENSU-READ-SEC
                               IF      FLG-TENSU   =    ZERO
                                   EVALUATE    TNS-GAITENTTLKBN
                                       WHEN    1
                                          COMPUTE  TBL-JIHI-TAXARI(1)
                                               =   TBL-JIHI-TAXARI(1)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    2
                                          COMPUTE  TBL-JIHI-TAXARI(2)
                                               =   TBL-JIHI-TAXARI(2)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    3
                                          COMPUTE  TBL-JIHI-TAXARI(3)
                                               =   TBL-JIHI-TAXARI(3)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    4
                                          COMPUTE  TBL-JIHI-TAXARI(4)
                                               =   TBL-JIHI-TAXARI(4)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    5
                                          COMPUTE  TBL-JIHI-TAXARI(5)
                                               =   TBL-JIHI-TAXARI(5)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    6
                                          COMPUTE  TBL-JIHI-TAXARI(6)
                                               =   TBL-JIHI-TAXARI(6)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    7
                                          COMPUTE  TBL-JIHI-TAXARI(7)
                                               =   TBL-JIHI-TAXARI(7)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    8
                                          COMPUTE  TBL-JIHI-TAXARI(8)
                                               =   TBL-JIHI-TAXARI(8)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    9
                                          COMPUTE  TBL-JIHI-TAXARI(9)
                                               =   TBL-JIHI-TAXARI(9)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    10
                                          COMPUTE  TBL-JIHI-TAXARI(10)
                                               =   TBL-JIHI-TAXARI(10)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                   END-EVALUATE
                               END-IF
                           ELSE
                               EVALUATE   SRY-SRYCD (IDX3) (4:2)
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY-TAX(1)
                                            =   TBL-TGMONEY-TAX(1)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY-TAX(2)
                                            =   TBL-TGMONEY-TAX(2)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY-TAX(3)
                                            =   TBL-TGMONEY-TAX(3)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY-TAX(4)
                                            =   TBL-TGMONEY-TAX(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY-TAX(5)
                                            =   TBL-TGMONEY-TAX(5)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY-TAX(6)
                                            =   TBL-TGMONEY-TAX(6)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "50"
                                       COMPUTE  TBL-TGMONEY-TAX(7)
                                            =   TBL-TGMONEY-TAX(7)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY-TAX(8)
                                            =   TBL-TGMONEY-TAX(8)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY-TAX(9)
                                            =   TBL-TGMONEY-TAX(9)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "64"
                                       COMPUTE  TBL-TGMONEY-TAX(14)
                                            =   TBL-TGMONEY-TAX(14)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY-TAX(10)
                                            =   TBL-TGMONEY-TAX(10)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "80"
      ***                          WHEN   "81"
                                       COMPUTE  TBL-TGMONEY-TAX(11)
                                            =   TBL-TGMONEY-TAX(11)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "82"
                                       COMPUTE  TBL-TGMONEY-TAX(4)
                                            =   TBL-TGMONEY-TAX(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "83"
                                       COMPUTE  TBL-TGMONEY-TAX(12)
                                            =   TBL-TGMONEY-TAX(12)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "84"
                                       COMPUTE  TBL-TGMONEY-TAX(13)
                                            =   TBL-TGMONEY-TAX(13)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "85"
                                       COMPUTE  TBL-TGMONEY-TAX(16)
                                            =   TBL-TGMONEY-TAX(16)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "89"
                                       COMPUTE  TBL-TGMONEY-TAX(15)
                                            =   TBL-TGMONEY-TAX(15)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   OTHER
                                       COMPUTE  TBL-TGMONEY-TAX(11)
                                            =   TBL-TGMONEY-TAX(11)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                               END-EVALUATE
                           END-IF
                       ELSE
                           INITIALIZE    TNS-TENSU-REC
                           MOVE    SRY-SRYCD(IDX3) TO   TNS-SRYCD
                           MOVE    LNK-SYU-HOSPNUM(IDY)
                                                   TO   TNS-HOSPNUM
                           MOVE    LNK-SYU-SRYYMD(IDY)
                                                   TO   TNS-YUKOSTYMD
                           MOVE    LNK-SYU-SRYYMD(IDY)
                                                   TO   TNS-YUKOEDYMD
                           PERFORM    900-TENSU-READ-SEC
                           IF      FLG-TENSU   =    ZERO
                               EVALUATE   TNS-SRYKBN
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY-TAX(1)
                                            =   TBL-TGMONEY-TAX(1)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY-TAX(2)
                                            =   TBL-TGMONEY-TAX(2)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY-TAX(3)
                                            =   TBL-TGMONEY-TAX(3)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY-TAX(4)
                                            =   TBL-TGMONEY-TAX(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY-TAX(5)
                                            =   TBL-TGMONEY-TAX(5)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY-TAX(6)
                                            =   TBL-TGMONEY-TAX(6)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "50"
                                       COMPUTE  TBL-TGMONEY-TAX(7)
                                            =   TBL-TGMONEY-TAX(7)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY-TAX(8)
                                            =   TBL-TGMONEY-TAX(8)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY-TAX(9)
                                            =   TBL-TGMONEY-TAX(9)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "64"
                                       COMPUTE  TBL-TGMONEY-TAX(14)
                                            =   TBL-TGMONEY-TAX(14)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY-TAX(10)
                                            =   TBL-TGMONEY-TAX(10)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "80"
                                    EVALUATE   TNS-SRYSYUKBN
                                      WHEN       "800"
      ***                             WHEN       "810"
                                       COMPUTE  TBL-TGMONEY-TAX(11)
                                            =   TBL-TGMONEY-TAX(11)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                      WHEN       "820"
                                       COMPUTE  TBL-TGMONEY-TAX(4)
                                            =   TBL-TGMONEY-TAX(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                      WHEN       "830"
                                       COMPUTE  TBL-TGMONEY-TAX(12)
                                            =   TBL-TGMONEY-TAX(12)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                      WHEN       "840"
                                       COMPUTE  TBL-TGMONEY-TAX(13)
                                            =   TBL-TGMONEY-TAX(13)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                      WHEN       "850"
                                       COMPUTE  TBL-TGMONEY-TAX(16)
                                            =   TBL-TGMONEY-TAX(16)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                      WHEN       "890"
                                       COMPUTE  TBL-TGMONEY-TAX(15)
                                            =   TBL-TGMONEY-TAX(15)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                    END-EVALUATE
                                   WHEN   OTHER
                                     IF   TNS-SRYCD(1:1)    =   "6"
                                       IF    TNS-YKZKBN     =    4
                                          COMPUTE  TBL-TGMONEY-TAX(5)
                                               =   TBL-TGMONEY-TAX(5)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       ELSE
                                          COMPUTE  TBL-TGMONEY-TAX(4)
                                               =   TBL-TGMONEY-TAX(4)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       END-IF
                                     END-IF
                                     IF   TNS-SRYCD(1:1)    =   "7"
                                     OR   TNS-SRYCD(1:3)    =   "059"
                                       COMPUTE  TBL-TGMONEY-TAX(11)
                                            =   TBL-TGMONEY-TAX(11)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                     END-IF
                               END-EVALUATE
                           END-IF
                       END-IF
                   END-PERFORM
           END-EVALUATE
           .
      *
       44022-HKNTEKGAI-TBL-EXT.
           EXIT.
      *
      **************************************************************
      *    ÉéÃ´¶â³Û¤Î¼êµ»ÅÀ»»½ÐÅù
      **************************************************************
       44023-SYUTEN-KEISAN-SEC          SECTION.
      *
           IF    SRY-RENNUM     =     1                                
      *                                                                
               ADD  TBL-SYUTEN(IDX1)    TO  WK-SYUTEN-KEI              
      *                                                                
           END-IF                                                     
      *                                                                
           PERFORM   VARYING   IDX3   FROM   1   BY   1                
                       UNTIL    (IDX3            >       5)  OR        
                                (SRY-SRYCD(IDX3) =   SPACE)            
      *                                                                
                    INITIALIZE    TNS-TENSU-REC
                    MOVE    SRY-SRYCD(IDX3)      TO   TNS-SRYCD
                    MOVE    LNK-SYU-HOSPNUM(IDY) TO   TNS-HOSPNUM
                    MOVE    LNK-SYU-SRYYMD(IDY)  TO   TNS-YUKOSTYMD
                    MOVE    LNK-SYU-SRYYMD(IDY)  TO   TNS-YUKOEDYMD
                    PERFORM    900-TENSU-READ-SEC
                    IF      FLG-TENSU   =    ZERO
      *                                                     
                      IF  LNK-SYU-SYUHKNNUM(IDY)  =  "971" OR "973"
      *
                         COMPUTE  WRK-TEN     =   TNS-TEN  /  10       
      *                                                                
                         IF   LNK-SYU-SRYYMD(IDY)(1:6)  >=   "201307"
                           IF  SRY-SRYCD(IDX3) =
                               "101110010" OR "101110040"
                             COMPUTE WK-RSISHOSHIN = WK-RSISHOSHIN +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                           END-IF
                           IF  SRY-SRYCD(IDX3) = "101800890"
                             COMPUTE WK-RSIETC  = WK-RSIETC +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                             COMPUTE WK-RSIETC2 = WK-RSIETC2 +
                                    (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                           END-IF
                           IF  SRY-SRYCD(IDX3) =
                               "101120010" OR "101120040" OR
                               "101120050" OR "101120060" OR
                               "101120070" OR "101120080"
                             COMPUTE WK-RSISAISHIN = WK-RSISAISHIN +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                           END-IF
                           IF  SRY-SRYCD(IDX3) =  "101130190"
                             COMPUTE WK-RSISDO = WK-RSISDO +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                           END-IF
                           IF  SRY-SRYCD(IDX3) =
                               "101800870" OR "101800470" OR
                               "101800480" OR "101800490" OR
                               "101800500" OR "101800510" OR
                               "101800520" OR "101800530" OR
                               "101800540" OR "101800960" OR
                               "101800970" OR "101800980" OR
                               "101801040"
                             COMPUTE WK-RSIETC  = WK-RSIETC +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                             COMPUTE WK-RSIETC2 = WK-RSIETC2 +
                                    (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                           END-IF
                           EVALUATE  SRY-SRYCD(IDX3)
                            WHEN    "101801010"  THRU  "101801020"
                             COMPUTE WK-RSIETC  = WK-RSIETC +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                             COMPUTE WK-RSIETC2 = WK-RSIETC2 +
                                    (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
      *
                             COMPUTE WRK-TEN-KG     = WRK-TEN
                             COMPUTE WRK-TEN-KG2    = TNS-TEN
                           END-EVALUATE
                           IF  SRY-SRYCD(IDX3) = "101801030"
                             COMPUTE WRK-FUTANWARI  = TNS-TEN / 100
      *
                             COMPUTE WRK-KEISAN2    =
                                    (WRK-TEN-KG     * WRK-FUTANWARI)
                                                    + 0.5
                             COMPUTE WRK-KEISAN2    =
                                    (WRK-KEISAN2    *
                                                TBL-ZAIKAISU (IDX1))
                             COMPUTE WK-RSIETC      =
                                     WK-RSIETC      + WRK-KEISAN2
      *
                             COMPUTE WRK-KEISAN2    =
                                    (WRK-TEN-KG2    * WRK-FUTANWARI)
                             COMPUTE WRK-KEISAN2    =
                                    (WRK-KEISAN2    *
                                                TBL-ZAIKAISU (IDX1))
                             COMPUTE WK-RSIETC2     =
                                     WK-RSIETC2     + WRK-KEISAN2
                           END-IF
                         ELSE
                           IF  SRY-SRYCD(IDX3) =
                               "101110010" OR "101110030" OR
                               "101110040"
                             COMPUTE WK-RSISHOSHIN = WK-RSISHOSHIN +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                           END-IF
                           IF  SRY-SRYCD(IDX3) =
                               "101120030" OR "101120040" OR
                               "101120050" OR "101120060" OR
                               "101120100"
                             COMPUTE WK-RSISAISHIN = WK-RSISAISHIN +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                           END-IF
                           IF ( SRY-SRYCD(IDX3) =
                                "101130020" OR "101130030" OR
                                "101130040" OR "101130050" OR
                                "101130060" OR "101130070" OR
                                "101130080" OR "101130090" OR
                                "101130100"                      )
                           OR ((LNK-SYU-SRYYMD(IDY)(1:6)
                                                   <=   "200803") AND
                               (SRY-SRYCD(IDX3) =  "101130010"  ))
                             COMPUTE WK-RSISDO = WK-RSISDO +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                           END-IF
                         END-IF
                         IF    SRY-SRYCD (IDX3)(1:5)   =   "09591"     
                                                     OR    "09592"     
                                                     OR    "09593"     
                           IF  SRY-SRYSYUKBN = "809"
                             COMPUTE WRK-TEN    = SRY-JIHIMONEY(IDX3)
                                                / 10
                             COMPUTE WK-RSIETC  = WK-RSIETC +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                             COMPUTE WK-RSIETC2 = WK-RSIETC2 +
                                    (SRY-JIHIMONEY(IDX3)
                                              *   TBL-ZAIKAISU (IDX1))
                           ELSE
                             COMPUTE WK-RSIETC  = WK-RSIETC +
                                    (WRK-TEN  *   TBL-ZAIKAISU (IDX1))
                             COMPUTE WK-RSIETC2 = WK-RSIETC2 +
                                    (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                           END-IF
      *
      *                    ¼«ÇåÀÕÊÝ¸±½èÍý
                           IF    LNK-SYU-SYUHKNNUM(IDY)  =   "973"
                              IF   IDX5  >= 20
                                DISPLAY "TABLE OVER"
                              ELSE
                                ADD    1   TO  IDX5
                                MOVE   SRY-SRYCD(IDX3)
                                   TO  LNK-S01-SONOTA-SRYCD(IDY IDX5)
                                MOVE   TBL-ZAIKAISU (IDX1)
                                   TO  LNK-S01-SONOTA-ZAIKAISU(IDY IDX5)
                                IF  SRY-SRYSYUKBN = "809"
                                  MOVE SRY-JIHIMONEY(IDX3)
                                   TO  LNK-S01-SONOTA-TANKA(IDY IDX5)
                                ELSE
                                  MOVE TNS-TEN
                                   TO  LNK-S01-SONOTA-TANKA(IDY IDX5)
                                END-IF
                              END-IF
                            END-IF
      *
                         END-IF                                        
                         IF    SRY-SRYCD (IDX3)(1:5)   =   "09591"     
                          IF  SRY-SRYSYUKBN = "809"
                           COMPUTE WK-SINDAN = WK-SINDAN +             
                                  (SRY-JIHIMONEY(IDX3)
                                            *   TBL-ZAIKAISU (IDX1))
                          ELSE
                           COMPUTE WK-SINDAN = WK-SINDAN +             
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))   
                          END-IF
                         END-IF                                        
                         IF    SRY-SRYCD (IDX3)(1:5)   =   "09592"     
                          IF  SRY-SRYSYUKBN = "809"
                           COMPUTE WK-MEISAI = WK-MEISAI +
                                  (SRY-JIHIMONEY(IDX3)
                                            *   TBL-ZAIKAISU (IDX1))
                          ELSE
                           COMPUTE WK-MEISAI = WK-MEISAI +             
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))   
                          END-IF
                         END-IF                                        
      *
                         IF    SRY-SRYCD (IDX3)  =   "140037490"
                          IF  WRK-4001-KOTEIKBN  =  SPACE OR "2"
                           COMPUTE WK-YOUBUKASAN = WK-YOUBUKASAN +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                          END-IF
                         END-IF
                         IF    SRY-SRYCD (IDX3)  =   "140040110"
                          IF  WRK-4001-KOTEIKBN  =  SPACE OR "2"
                           COMPUTE WK-KYOBUKASAN = WK-KYOBUKASAN +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                          END-IF
                         END-IF
                         IF    SRY-SRYCD (IDX3)  =   "140052610"
                          IF  WRK-4001-KOTEIKBN  =  SPACE OR "2"
                           COMPUTE WK-KEIBUKASAN = WK-KEIBUKASAN +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                          END-IF
                         END-IF
                         IF    SRY-SRYCD (IDX3)  =   "150266970"
                          IF  WRK-4001-KOTEIKBN  =  SPACE OR "2"
                           COMPUTE WK-SOGAIKASAN = WK-SOGAIKASAN +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                          END-IF
                         END-IF
      *
                         IF    SRY-SRYCD (IDX3)  =   "199000610"
                           COMPUTE WK-RYOYOTEATE = WK-RYOYOTEATE +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                         END-IF
      *
                         IF    LNK-SYU-SYUHKNNUM(IDY)  =  "973"
                         AND   LNK-SYU-SRYYMD(IDY)(1:6)  >=   "201307"
                         AND   SRY-SRYCD (IDX3)  =   "101800890"
                           COMPUTE WK-KYUKYU = WK-KYUKYU +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                         END-IF
                      ELSE
                         IF    SRY-SRYCD (IDX3)  =  "102130010"
                                                OR  "102130020"
                                                OR  "102400010"
                           COMPUTE WK-KOGAI-TEN = WK-KOGAI-TEN +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                         END-IF
                      END-IF
      *                                                                
                    END-IF                                             
      *                                                                
           END-PERFORM
           .
      *
       44023-SYUTEN-KEISAN-EXT.
           EXIT.
      *
      **************************************************************
      *    ÉéÃ´¶â³Û¤Î¼êµ»ÅÀ»»½ÐÅù¡ÊÂè»°¼Ô¹Ô°Ù¡Ë
      **************************************************************
       44026-SYUTEN-KEISAN2-SEC          SECTION.
      *
           PERFORM   VARYING   IDX3   FROM   1   BY   1
                      UNTIL   (IDX3            >       5)
                        OR    (SRY-SRYCD(IDX3) =   SPACE)
      *
             IF  SRY-SRYCD(IDX3)(1:5) = "09591" OR "09592" OR "09593"
      *
               INITIALIZE    TNS-TENSU-REC
               MOVE    SRY-SRYCD(IDX3)      TO   TNS-SRYCD
               MOVE    LNK-SYU-HOSPNUM(IDY) TO   TNS-HOSPNUM
               MOVE    LNK-SYU-SRYYMD(IDY)  TO   TNS-YUKOSTYMD
               MOVE    LNK-SYU-SRYYMD(IDY)  TO   TNS-YUKOEDYMD
               PERFORM    900-TENSU-READ-SEC
               IF      FLG-TENSU   =    ZERO
      *
                 COMPUTE  WRK-TEN  =    TNS-TEN  /  10
      *
                 IF  SRY-SRYSYUKBN = "809"
                   COMPUTE WRK-TEN    = SRY-JIHIMONEY(IDX3) / 10
                   COMPUTE WK-RSIETC  = WK-RSIETC  +
                          (WRK-TEN    * TBL-ZAIKAISU(IDX1))
                   COMPUTE WK-RSIETC2 = WK-RSIETC2 +
                          (SRY-JIHIMONEY(IDX3)
                                      * TBL-ZAIKAISU(IDX1))
                 ELSE
                   COMPUTE WK-RSIETC  = WK-RSIETC  +
                          (WRK-TEN    * TBL-ZAIKAISU(IDX1))
                   COMPUTE WK-RSIETC2 = WK-RSIETC2 +
                          (TNS-TEN    * TBL-ZAIKAISU(IDX1))
                 END-IF
      *
                 IF   IDX5  >= 20
                   DISPLAY "TABLE OVER"
                 ELSE
                   ADD    1   TO  IDX5
                   MOVE   SRY-SRYCD(IDX3)
                              TO  LNK-S01-SONOTA-SRYCD(IDY IDX5)
                   MOVE   TBL-ZAIKAISU(IDX1)
                              TO  LNK-S01-SONOTA-ZAIKAISU(IDY IDX5)
                   IF  SRY-SRYSYUKBN = "809"
                      MOVE SRY-JIHIMONEY(IDX3)
                              TO  LNK-S01-SONOTA-TANKA(IDY IDX5)
                   ELSE
                      MOVE TNS-TEN
                              TO  LNK-S01-SONOTA-TANKA(IDY IDX5)
                   END-IF
                 END-IF
      *
               END-IF
      *
             END-IF
      *
           END-PERFORM
           .
      *
       44026-SYUTEN-KEISAN2-EXT.
           EXIT.
      *
      **************************************************************
      *    ÊÝ¸±Å¬ÍÑ³°¶â³Û¤Î½¸·×¥Æ¡¼¥Ö¥ë¤«¤é¼ýÇ¼¥Æ¡¼¥Ö¥ë¤Ø¤Î¥»¥Ã¥È
      **************************************************************
       44024-HKNTEKGAI-SET-SEC          SECTION.
      *
      *  ÊÝ¸±Å¬ÍÑ³°
           PERFORM     VARYING   IDX3    FROM   1    BY    1
                         UNTIL   IDX3    >      16
               MOVE   TBL-TGMONEY(IDX3)
                                 TO  LNK-SYU-TGMONEY(IDY IDX3)
               MOVE   TBL-TGMONEY-TAX(IDX3)
                                 TO  LNK-SYU-TGMONEY-TAX(IDY IDX3)
           END-PERFORM
      *
      *  ¼«Èñ£±¤«¤é£±£°
           PERFORM     VARYING   IDX3    FROM    1    BY    1
                         UNTIL   IDX3    >     10
              MOVE   TBL-JIHI-TAXNASI(IDX3)
                                 TO  LNK-SYU-JIHI-TAXNASI(IDY IDX3)
              MOVE   TBL-JIHI-TAXARI(IDX3)
                                 TO  LNK-SYU-JIHI-TAXARI(IDY IDX3)
           END-PERFORM
           .
      *
       44024-HKNTEKGAI-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    ½é¿ÇÎÁ»»Äê¥Õ¥é¥°¡ÊÃÏÊý¸øÈñÍÑ¡Ë¤Î¥»¥Ã¥È
      **************************************************************
       44025-SYOSINFLG-SET-SEC          SECTION.
      *
           IF  TBL-SRYKBN(IDX1)   =   "11"  OR  "13"
      *
               PERFORM   VARYING   IDX4   FROM    1   BY  1
                          UNTIL  ( IDX4    >   TBL-SYOSIN-MAX  )
                           OR    ( LNK-S01-SYOSINFLG   =   "1" )
      *
                   INITIALIZE                        SRYACT-REC
                   MOVE    LNK-SYU-HOSPNUM(IDY) TO   SRY-HOSPNUM
                   MOVE    LNK-SYU-NYUGAIKBN(IDY)
                                                TO   SRY-NYUGAIKBN
                   MOVE    LNK-SYU-PTID(IDY)    TO   SRY-PTID
                   MOVE    LNK-SYU-SRYYMD(IDY)(1:6)
                                                TO   SRY-SRYYM
                   MOVE    TBL-ZAIBAN(IDX1)     TO   SRY-ZAINUM
      *
                   MOVE    TBL-SYOSIN(IDX4)     TO   SRY-SRYCD (1)
                   MOVE    TBL-SYOSIN(IDX4)     TO   SRY-SRYCD (2)
                   MOVE    TBL-SYOSIN(IDX4)     TO   SRY-SRYCD (3)
                   MOVE    TBL-SYOSIN(IDX4)     TO   SRY-SRYCD (4)
                   MOVE    TBL-SYOSIN(IDX4)     TO   SRY-SRYCD (5)
                   MOVE    SRYACT-REC           TO   MCPDATA-REC
                   MOVE    "DBSELECT"           TO   MCP-FUNC
                   MOVE    "tbl_sryact"         TO   MCP-TABLE
                   MOVE    "key8"               TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"          USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                   IF      MCP-RC             =       ZERO
                     MOVE     "tbl_sryact"      TO   MCP-TABLE
                     MOVE     "key8"            TO   MCP-PATHNAME
                     PERFORM   900-SRYACT-READ-SEC
                     IF     FLG-SRYACT   =    ZERO
                           MOVE    "1"          TO   LNK-S01-SYOSINFLG
                     END-IF
                   END-IF
      *
                   MOVE    "tbl_sryact"         TO      MCP-TABLE
                   MOVE    "key8"               TO      MCP-PATHNAME
                   PERFORM  DB-CLOSE-SEC
      *
               END-PERFORM
      *
           END-IF
      *
           .
      *
       44025-SYOSINFLG-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¹ç·×¥Ç¡¼¥¿¤ÎºÆÊÔ½¸ÍÑÂàÈò
      ******************************************************************
       4403-GOKEI-TAIHI-SEC           SECTION.
      *
           PERFORM   VARYING   IDX3     FROM    1   BY  1
                      UNTIL    IDX3      >     17
               COMPUTE   WRK-SYU-HKNTEN(IDX3) =
                         WRK-SYU-HKNTEN(IDX3) +
                         LNK-SYU-HKNTEN(IDY IDX3)
           END-PERFORM
           PERFORM   VARYING   IDX3     FROM    1   BY  1
                      UNTIL    IDX3      >     16
               COMPUTE   WRK-SYU-TGMONEY(IDX3) =
                         WRK-SYU-TGMONEY(IDX3) +
                         LNK-SYU-TGMONEY(IDY IDX3)
               COMPUTE   WRK-SYU-TGMONEY-TAX(IDX3) =
                         WRK-SYU-TGMONEY-TAX(IDX3) +
                         LNK-SYU-TGMONEY-TAX(IDY IDX3)
           END-PERFORM
           PERFORM   VARYING   IDX3     FROM    1   BY  1
                      UNTIL    IDX3      >      10
               COMPUTE   WRK-SYU-JIHI-TAXNASI(IDX3) =
                         WRK-SYU-JIHI-TAXNASI(IDX3) +
                         LNK-SYU-JIHI-TAXNASI(IDY IDX3)
               COMPUTE   WRK-SYU-JIHI-TAXARI(IDX3) =
                         WRK-SYU-JIHI-TAXARI(IDX3) +
                         LNK-SYU-JIHI-TAXARI(IDY IDX3)
           END-PERFORM
           COMPUTE       WRK-SYU-RSISHOSHIN-MONEY =
                         WRK-SYU-RSISHOSHIN-MONEY +
                         LNK-SYU-RSISHOSHIN-MONEY(IDY)
           COMPUTE       WRK-SYU-RSISAISHIN-MONEY =
                         WRK-SYU-RSISAISHIN-MONEY +
                         LNK-SYU-RSISAISHIN-MONEY(IDY)
           COMPUTE       WRK-SYU-RSISDO-MONEY =
                         WRK-SYU-RSISDO-MONEY +
                         LNK-SYU-RSISDO-MONEY(IDY)
           COMPUTE       WRK-SYU-RSIETC-MONEY =
                         WRK-SYU-RSIETC-MONEY +
                         LNK-SYU-RSIETC-MONEY(IDY)
           PERFORM   VARYING   IDX3     FROM    1   BY  1
                            UNTIL    IDX3      >      6
               COMPUTE   WRK-SYU-KYUFUGAI-TEN(IDX3) =
                         WRK-SYU-KYUFUGAI-TEN(IDX3) +
                         LNK-SYU-KYUFUGAI-TEN(IDY IDX3)
           END-PERFORM
           COMPUTE       WRK-SYU-SHOHOU-SAI =
                         WRK-SYU-SHOHOU-SAI +
                         LNK-SYU-SHOHOU-SAI(IDY)
      *
           .
      *
       4403-GOKEI-TAIHI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¹ç·×¥Ç¡¼¥¿¤ÎºÆÊÔ½¸
      ******************************************************************
       4404-GOKEI-HENSYU-SEC           SECTION.
      *
           PERFORM   VARYING   IDX3     FROM    1   BY  1
                      UNTIL    IDX3      >     17
               MOVE      WRK-SYU-HKNTEN(IDX3)
                             TO    LNK-SYU-HKNTEN(IDY IDX3)
           END-PERFORM
           PERFORM   VARYING   IDX3     FROM    1   BY  1
                      UNTIL    IDX3      >     16
               MOVE      WRK-SYU-TGMONEY(IDX3)
                             TO    LNK-SYU-TGMONEY(IDY IDX3)
               MOVE      WRK-SYU-TGMONEY-TAX(IDX3)
                             TO    LNK-SYU-TGMONEY-TAX(IDY IDX3)
           END-PERFORM
           PERFORM   VARYING   IDX3     FROM    1   BY  1
                      UNTIL    IDX3      >      10
               MOVE      WRK-SYU-JIHI-TAXNASI(IDX3)
                             TO    LNK-SYU-JIHI-TAXNASI(IDY IDX3)
               MOVE      WRK-SYU-JIHI-TAXARI(IDX3)
                             TO    LNK-SYU-JIHI-TAXARI(IDY IDX3)
           END-PERFORM
           MOVE    WRK-SYU-RSISHOSHIN-MONEY
                             TO    LNK-SYU-RSISHOSHIN-MONEY(IDY)
           MOVE    WRK-SYU-RSISAISHIN-MONEY
                             TO    LNK-SYU-RSISAISHIN-MONEY(IDY)
           MOVE    WRK-SYU-RSISDO-MONEY
                             TO    LNK-SYU-RSISDO-MONEY(IDY)
           MOVE    WRK-SYU-RSIETC-MONEY
                             TO    LNK-SYU-RSIETC-MONEY(IDY)
           PERFORM   VARYING   IDX3     FROM    1   BY  1
                      UNTIL    IDX3      >      6
               MOVE      WRK-SYU-KYUFUGAI-TEN(IDX3)
                             TO    LNK-SYU-KYUFUGAI-TEN(IDY IDX3)
           END-PERFORM
           MOVE    WRK-SYU-SHOHOU-SAI
                             TO    LNK-SYU-SHOHOU-SAI(IDY)
      *
           .
      *
       4404-GOKEI-HENSYU-EXT.
           EXIT.
      *
      *************************************************************
      *    ¼ýÇ¼ÌÀºÙ¥ì¥³¡¼¥ÉºîÀ®
      *************************************************************
       4405-SMEI-SAKUSEI-SEC           SECTION.
      *
           MOVE    SPACE                TO  SYUMEI-REC
           INITIALIZE                       SYUMEI-REC
      *
           MOVE    LNK-SYU-HOSPNUM(IDY) TO  SMEI-HOSPNUM
           MOVE    LNK-SYU-NYUGAIKBN(IDY)
                                        TO  SMEI-NYUGAIKBN
           MOVE    LNK-SYU-PTID(IDY)    TO  SMEI-PTID
      *ÅÁÉ¼ÈÖ¹æ
           MOVE    LNK-SYU-DENPNUM(IDY) TO  SMEI-DENPNUM
      *ÅÁÉ¼ÈÖ¹æ»ÞÈÖ
           MOVE    IDX3                 TO  SMEI-DENPEDANUM
      *Æþ¶âÏ¢ÈÖ
           MOVE    ZERO                 TO  SMEI-NYUKINRENNUM
      *¿ÇÎÅ²Ê
           MOVE    LNK-SYU-SRYKA(IDY)   TO  SMEI-SRYKA
      *ÅÁÉ¼¾õÂÖ¶èÊ¬
           MOVE    SPACE                TO  SMEI-MEISAIJOUTAIKBN
      *ÀÁµáÈ¯¹ÔÆü¡ÊÈ¯¹ÔÆü¤ÎÀ¾Îñ¡Ë
           MOVE    SPACE                TO  SMEI-SKYPRINTYMD
      *ÀÁµáºÆÈ¯¹ÔÆü
           MOVE    SPACE                TO  SMEI-SKYREPRINTYMD
      *ÎÎ¼ý½ñÈ¯¹ÔÆü
           MOVE    SPACE                TO  SMEI-RYOSYUPRINTYMD
      *ÎÎ¼ý½ñºÆÈ¯¹ÔÆü
           MOVE    SPACE                TO  SMEI-RYOSYUREPRINTYMD
      *ÀÁµá¶â³Û
           IF  WRK-ATO-SEIKYUGK(IDY)    NOT =   WRK-MAE-SEIKYUGK(IDY)
               COMPUTE   SMEI-SKYMONEY  =   WRK-ATO-SEIKYUGK(IDY)  -
                                            WRK-MAE-SEIKYUGK(IDY)
           ELSE
               MOVE    ZERO             TO  SMEI-SKYMONEY
           END-IF
      *ÆþÊÖ¶â¶èÊ¬
           MOVE    SPACE                TO  SMEI-NYUHEN-KBN
      *ÆþÊÖ¶â³Û
           MOVE    ZERO                 TO  SMEI-NYUHEN-MONEY
      *ÆþÊÖ¶âÆü
           MOVE    SPA-SYSYMD           TO  SMEI-NYUHEN-YMD
      *Æþ¶âÊýË¡
           MOVE    "00"                 TO  SMEI-NYUKIN-HOHO
      *¾õÂÖ¶èÊ¬
           MOVE    "3"                  TO  SMEI-JOUTAIKBN
      *
           MOVE    WRK-SYURRKNUM        TO  SMEI-SYURRKNUM
           MOVE    WRK-SYUEDANUM        TO  SMEI-SYUEDANUM
           MOVE    WRK-SYURRKUPDKBN     TO  SMEI-SYURRKUPDKBN
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF       MCP-RC             =   ZERO
             CONTINUE
           ELSE
             MOVE     "0070"           TO  SPA-ERRCD
           END-IF
           .
      *
       4405-SMEI-SAKUSEI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¼ýÇ¼¹ç·×¥Þ¥¹¥¿¤Î¹àÌÜÃÖ¤­´¹¤¨
      ******************************************************************
       4406-GOKEI-OKIKAE-SEC           SECTION.
      *£Ë£Å£Ù
           MOVE    LNK-SYU-KEY(IDY)            TO  SYUTTL-KEY
      *¿ÇÎÅ²Ê
           MOVE    LNK-SYU-SRYKA(IDY)          TO  SYUTTL-SRYKA
      *Æ±»þÆþÎÏ¿ÇÎÅ²Ê
           PERFORM   VARYING   IDX3   FROM   1    BY    1
                      UNTIL    IDX3    >     10
              MOVE   WRK-DOUJI-SRYKA(IDY IDX3)
                                           TO  SYUTTL-DOUJI-SRYKA(IDX3)
           END-PERFORM
      *¿ÇÎÅÆü
           MOVE    LNK-SYU-SRYYMD(IDY)         TO  SYUTTL-SRYYMD
      *ÅÁÉ¼¾õÂÖ¶èÊ¬
           MOVE    LNK-SYU-DENPJTIKBN(IDY)     TO  SYUTTL-DENPJTIKBN
      *ÅÁÉ¼ºîÀ®¶èÊ¬
           MOVE    LNK-SYU-CREATEKBN(IDY)      TO  SYUTTL-CREATEKBN
      *ÅÁÉ¼È¯¹ÔÆü
           MOVE    LNK-SYU-DENPPRTYMD(IDY)     TO  SYUTTL-DENPPRTYMD
      *ÀÁµá´ü´Ö
           MOVE    LNK-SYU-SKY-KIKAN(IDY)      TO  SYUTTL-SKY-KIKAN
      *ÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ
           MOVE    LNK-SYU-HKNCOMBINUM(IDY)    TO  SYUTTL-HKNCOMBINUM
      *¼çÊÝ¸±¡ÝÉéÃ´¾ðÊó
           MOVE    LNK-SYU-SYUHKNNUM(IDY)      TO  SYUTTL-SYUHKNNUM
      *
           MOVE    LNK-SYU-SYUHKNFTNMONEY(IDY) TO  SYUTTL-SYUHKNFTNMONEY
      *
           MOVE    LNK-SYU-SYUCOMPFTN(IDY)     TO  SYUTTL-SYUCOMPFTN
      *
           MOVE    LNK-SYU-SYUCOMPFTN-ENTANI(IDY)
                                           TO  SYUTTL-SYUCOMPFTN-ENTANI
      *
           MOVE    LNK-SYU-SYUCOMPYKZFTN(IDY)  TO  SYUTTL-SYUCOMPYKZFTN
      *
           MOVE    LNK-SYU-SYUCOMPTOTAL(IDY)   TO  SYUTTL-SYUCOMPTOTAL
      *
           MOVE    LNK-SYU-SYUYKZFTNKBN(IDY)   TO  SYUTTL-SYUYKZFTNKBN
      *¸øÈñ¡ÝÉéÃ´¾ðÊó
           MOVE    LNK-SYU-KOH-AREA(IDY)       TO  SYUTTL-KOH-AREA
      *´µ¼ÔÉéÃ´Î¨
           MOVE    LNK-SYU-PTFTNRATE(IDY)      TO  SYUTTL-PTFTNRATE
      *ÀÁµá½ñºîÀ®¶èÊ¬
           MOVE    LNK-SYU-SKYKUKBN(IDY)       TO  SYUTTL-SKYKUKBN
      *ÀÁµáÆâÍÆ
           MOVE    LNK-SYU-SKY-NAIYOU(IDY)     TO  SYUTTL-SKY-NAIYOU
      *½èÊý¤»¤óÎÁºÆ·Ç
           MOVE    LNK-SYU-SHOHOU-SAI(IDY)     TO  SYUTTL-SHOHOU-SAI
      *ÊÝ¸±Å¬ÍÑ³°¶â³Û¡Ý¾ÃÈñÀÇ¡ÊºÆ·Ç¡Ë
           MOVE    LNK-SYU-TGMONEY-TAX-SAI(IDY)
                                           TO  SYUTTL-TGMONEY-TAX-SAI
      *¼«Èñ£±¤«¤é£µ
           MOVE    LNK-SYU-JIHI-NAIYOU(IDY)    TO  SYUTTL-JIHI-NAIYOU
      *¼«Èñ¾®·×¡Ê¾ÃÈñÀÇ¤Ê¤·¡Ë
           MOVE    LNK-SYU-JIHI-TOTAL(IDY)     TO  SYUTTL-JIHI-TOTAL
      *¼«Èñ¾®·×¡Ê¾ÃÈñÀÇ¤¢¤ê¡Ë
           MOVE    LNK-SYU-JIHI-TOTAL-TAX(IDY) TO  SYUTTL-JIHI-TOTAL-TAX
      *¼«Èñ¶â³Û¾ÃÈñÀÇ
           MOVE    LNK-SYU-JIHI-TAX(IDY)       TO  SYUTTL-JIHI-TAX
      *Ï·¿Í°ìÉôÉéÃ´¶â
           MOVE    LNK-SYU-RJN-FTN(IDY)        TO  SYUTTL-RJN-FTN
      *¸øÈñ°ìÉôÉéÃ´¶â
           MOVE    LNK-SYU-KOH-FTN(IDY)        TO  SYUTTL-KOH-FTN
      *¸øÈñ°ìÉôÉéÃ´¶â¡Ê±ßÃ±°Ì¡Ë
           MOVE    LNK-SYU-KOH-FTN-ENTANI(IDY) TO  SYUTTL-KOH-FTN-ENTANI
      *Ä´À°¶â
           MOVE    LNK-SYU-CHOSEI(IDY)         TO  SYUTTL-CHOSEI
      *¤Þ¤È¤áÆþÎÏº¹³Û
           MOVE    LNK-SYU-GRP-SGKMONEY(IDY)   TO  SYUTTL-GRP-SGKMONEY
      *ÊÝ¸±Å¬ÍÑ¶â³Û
           MOVE    LNK-SYU-HKNTEKMONEY(IDY)    TO  SYUTTL-HKNTEKMONEY
      *ÉÂ±¡¸ºÌÈ»öÍ³¶èÊ¬
           MOVE    LNK-SYU-DISCOUNT-KBN(IDY)   TO  SYUTTL-DISCOUNT-KBN
      *ÉÂ±¡¸ºÌÈ»öÍ³
           MOVE    LNK-SYU-DISCOUNT-BODY(IDY)  TO  SYUTTL-DISCOUNT-BODY
      *³ä°úÎ¨¶èÊ¬
           MOVE    LNK-SYU-DISCOUNT-RATEKBN(IDY)
                                           TO  SYUTTL-DISCOUNT-RATEKBN
      *ÄêÎ¨Äê³Û¶èÊ¬
           MOVE    LNK-SYU-DISCOUNT-TEIRITU(IDY)
                                           TO  SYUTTL-DISCOUNT-TEIRITU
      *³ä°úÎ¨
           MOVE    LNK-SYU-DISCOUNT-RATE(IDY)  TO  SYUTTL-DISCOUNT-RATE
      *¸ºÌÈ¶â³Û
           MOVE    LNK-SYU-DISCOUNT-MONEY(IDY) TO  SYUTTL-DISCOUNT-MONEY
      *Ï«ºÒÊÝ¸±¡Ý½é¿ÇÎÁ
           MOVE    LNK-SYU-RSISHOSHIN-MONEY(IDY)
                                           TO  SYUTTL-RSISHOSHIN-MONEY
      *Ï«ºÒÊÝ¸±¡ÝºÆ¿ÇÎÁ
           MOVE    LNK-SYU-RSISAISHIN-MONEY(IDY)
                                           TO  SYUTTL-RSISAISHIN-MONEY
      *Ï«ºÒÊÝ¸±¡Ý»ØÆ³ÎÁ
           MOVE    LNK-SYU-RSISDO-MONEY(IDY)   TO  SYUTTL-RSISDO-MONEY
      *Ï«ºÒÊÝ¸±¡Ý¤½¤ÎÂ¾
           MOVE    LNK-SYU-RSIETC-MONEY(IDY)   TO  SYUTTL-RSIETC-MONEY
      *Ï«ºÒÊÝ¸±¡Ý¾ÃÈñÀÇ¡ÊºÆ·Ç¡Ë
           MOVE    LNK-SYU-RSI-TAX-SAI(IDY)    TO  SYUTTL-RSI-TAX-SAI
      *Ï«ºÒÊÝ¸±¡Ý´µ¼ÔÀÁµáÊ¬±ß·ú¤Æ¹ç·×³Û
           MOVE    LNK-SYU-RSI-TOTAL(IDY)      TO  SYUTTL-RSI-TOTAL
      *Ï«ºÒ¼«Çå¡ÝÀÁµá¶â³Û¡ÊÆü·×É½µ­ºÜÍÑ¡Ë
           MOVE    LNK-SYU-RSIJIBAI-SKYMONEY(IDY)
                                           TO  SYUTTL-RSIJIBAI-SKYMONEY
      *ÀÁµá¶â³Û¡Ý¾ÃÈñÀÇ¡ÊºÆ·Ç¡Ë
           MOVE    LNK-SYU-SKYMONEY-TAX-SAI(IDY)
                                           TO  SYUTTL-SKYMONEY-TAX-SAI
      *ÀÁµá¶â³Û
           MOVE    LNK-SYU-SKYMONEY(IDY)       TO  SYUTTL-SKYMONEY
      *Æþ¶â¹ç·×³Û
           MOVE    LNK-SYU-NYUKIN-TOTAL(IDY)   TO  SYUTTL-NYUKIN-TOTAL
      *Æþ¶â²ó¿ô
           MOVE    LNK-SYU-NYUKIN-KAISU(IDY)   TO  SYUTTL-NYUKIN-KAISU
      *Ì¤¼ýÍýÍ³
           MOVE    LNK-SYU-MISYURIYU(IDY)      TO  SYUTTL-MISYURIYU
      *ºßÁí¿Ç¶èÊ¬
           MOVE    LNK-SYU-ZAITAKU(IDY)        TO  SYUTTL-ZAITAKU
      *¿ÇÎÅ¶èÊ¬ÊÌµëÉÕ³°ÅÀ¿ô
           MOVE    LNK-SYU-KYUFUGAI(IDY)       TO  SYUTTL-KYUFUGAI
      *ÊÝ¸±Å¬ÍÑ¶èÊ¬
           MOVE    LNK-SYU-HKNTEKKBN(IDY)      TO  SYUTTL-HKNTEKKBN
      *Ã¼Ëö£É£Ä
           MOVE    LNK-SYU-TERMID(IDY)         TO  SYUTTL-TERMID
      *¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
           MOVE    LNK-SYU-OPID(IDY)           TO  SYUTTL-OPID
      *ºîÀ®Ç¯·îÆü
           MOVE    LNK-SYU-CREYMD(IDY)         TO  SYUTTL-CREYMD
      *¹¹¿·Ç¯·îÆü
           MOVE    LNK-SYU-UPYMD(IDY)          TO  SYUTTL-UPYMD
      *¹¹¿·»þ´Ö
           MOVE    LNK-SYU-UPHMS(IDY)          TO  SYUTTL-UPHMS
      *
           .
      *
       4406-GOKEI-OKIKAE-EXT.
           EXIT.
      *
      ******************************************************************
      *    Ê£¿ô²Ê¤Þ¤È¤áÆþÎÏÊ¬¤Î¥Á¥§¥Ã¥¯
      ******************************************************************
       4407-GRP-DATA-CHECK-SEC           SECTION.
      *
           INITIALIZE                         SYUNOU-REC
           MOVE    LNK-ORCSSKS-HOSPNUM    TO  SYU-HOSPNUM
           MOVE    LNK-ORCSSKS-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE    LNK-ORCSSKS-PTID       TO  SYU-PTID
           MOVE    WRK-GRP-DENPNUM        TO  SYU-GRP-DENPNUM
           MOVE    SYUNOU-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key27"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE    "tbl_syunou"        TO  MCP-TABLE
              MOVE    "key27"             TO  MCP-PATHNAME
              PERFORM  900-SYUNOU-READ-SEC
              PERFORM  UNTIL   FLG-SYUNOU   =    1
                IF   SYU-DENPJTIKBN  NOT =  "3"
                  IF   SYU-SAIKEISANKBN    =  "1"
                     MOVE    1     TO  FLG-GENBUTU2
                  END-IF
                  IF  (SYU-SYUHKNNUM   NOT = SPACE)
                  AND (SYU-KOH1HKNNUM  NOT = SPACE)
                     MOVE    1     TO  FLG-GENBUTU3
                  END-IF
                END-IF
                MOVE    "tbl_syunou"      TO  MCP-TABLE
                MOVE    "key27"           TO  MCP-PATHNAME
                PERFORM  900-SYUNOU-READ-SEC
              END-PERFORM
           END-IF
           MOVE       "tbl_syunou"        TO  MCP-TABLE
           MOVE       "key27"             TO  MCP-PATHNAME
           PERFORM     DB-CLOSE-SEC
      *
           IF   FLG-GENBUTU2  =   1
           AND  FLG-GENBUTU3  =   1
              MOVE    1     TO   FLG-GENBUTU
           END-IF
      *
           .
      *
       4407-GRP-DATA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆüÊó¥­¡¼¼èÆÀ¥µ¥Ö
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
                                            SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
           EXIT.
      *****************************************************************
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SYUNOU-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC      TO   SYUNOU-REC
               MOVE    ZERO             TO   FLG-SYUNOU
           ELSE
               INITIALIZE                    SYUNOU-REC
               MOVE    1                TO   FLG-SYUNOU
           END-IF
      *
           .
      *
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¹ç·×¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       900-SYUTOTAL-READ-SEC          SECTION.
      *
           INITIALIZE                             SYUTTL-REC
           MOVE    LNK-ORCSSKS-HOSPNUM    TO      SYUTTL-HOSPNUM
           MOVE    LNK-ORCSSKS-NYUGAIKBN  TO      SYUTTL-NYUGAIKBN
           MOVE    LNK-ORCSSKS-PTID       TO      SYUTTL-PTID
           MOVE    WRK-TAIHI-DENPNUM      TO      SYUTTL-DENPNUM
           MOVE    SYUTTL-REC             TO      MCPDATA-REC
           MOVE    "DBSELECT"             TO      MCP-FUNC
           MOVE    "tbl_syutotal"         TO      MCP-TABLE
           MOVE    "key"                  TO      MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING   MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
           IF      MCP-RC             =       ZERO
              MOVE      "tbl_syutotal"    TO      MCP-TABLE
              MOVE      "key"             TO      MCP-PATHNAME
              PERFORM    901-SYUTOTAL-FETCH-SEC
           ELSE
              MOVE       1               TO  FLG-SYUTOTAL
           END-IF
           MOVE      "tbl_syutotal"       TO      MCP-TABLE
           MOVE      "key"                TO      MCP-PATHNAME
           PERFORM    DB-CLOSE-SEC
      *
           .
      *
       900-SYUTOTAL-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¹ç·×¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       901-SYUTOTAL-FETCH-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUTTL-REC
               MOVE    ZERO            TO  FLG-SYUTOTAL
           ELSE
               INITIALIZE                  SYUTTL-REC
               MOVE    1               TO  FLG-SYUTOTAL
           END-IF
      *
           .
      *
       901-SYUTOTAL-FETCH-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý£±£°£³£¸ÆÉ¹þ
      ******************************************************************
       900-SYS1038-READ-SEC           SECTION.
      *
           INITIALIZE                         SYS-1038-REC
           MOVE    "1038"               TO    SYS-1038-KANRICD
           MOVE    "*"                  TO    SYS-1038-KBNCD
           MOVE    LNK-ORCSSKS-HOSPNUM  TO    SYS-1038-HOSPNUM
           MOVE    WRK-SAIKEISAN-YMD    TO    SYS-1038-STYUKYMD
           MOVE    WRK-SAIKEISAN-YMD    TO    SYS-1038-EDYUKYMD
           MOVE    SYS-1038-REC         TO    MCPDATA-REC
           MOVE    "DBSELECT"           TO    MCP-FUNC
           MOVE    "tbl_syskanri"       TO    MCP-TABLE
           MOVE    "key10"              TO    MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =     ZERO
               MOVE     "tbl_syskanri"  TO    MCP-TABLE
               MOVE     "key10"         TO    MCP-PATHNAME
               PERFORM   901-SYSKANRI-READ-SEC
           END-IF
      *
           MOVE     "tbl_syskanri"      TO    MCP-TABLE
           MOVE     "key10"             TO    MCP-PATHNAME
           PERFORM   DB-CLOSE-SEC
      *
           .
      *
       900-SYS1038-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý£´£°£°£±ÆÉ¹þ
      ******************************************************************
       900-SYS4001-READ-SEC           SECTION.
      *
           INITIALIZE                         SYS-4001-REC
           MOVE    "4001"               TO    SYS-4001-KANRICD
           MOVE    "*"                  TO    SYS-4001-KBNCD
           MOVE    LNK-ORCSSKS-HOSPNUM  TO    SYS-4001-HOSPNUM
           MOVE    WRK-SAIKEISAN-YMD    TO    SYS-4001-STYUKYMD
           MOVE    WRK-SAIKEISAN-YMD    TO    SYS-4001-EDYUKYMD
           MOVE    SYS-4001-REC         TO    MCPDATA-REC
           MOVE    "DBSELECT"           TO    MCP-FUNC
           MOVE    "tbl_syskanri"       TO    MCP-TABLE
           MOVE    "key10"              TO    MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =     ZERO
               MOVE     "tbl_syskanri"  TO    MCP-TABLE
               MOVE     "key10"         TO    MCP-PATHNAME
               PERFORM   901-SYSKANRI-READ-SEC
           END-IF
      *
           MOVE     "tbl_syskanri"      TO    MCP-TABLE
           MOVE     "key10"             TO    MCP-PATHNAME
           PERFORM   DB-CLOSE-SEC
      *
           .
      *
       900-SYS4001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍýÆÉ¹þ
      *****************************************************************
       901-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1038-REC
                                           SYS-4001-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1038-REC
                                           SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       901-SYSKANRI-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    ¼õ¿ÇÍúÎò¥Þ¥¹¥¿¡¼ÆÉ¹þ
      **************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ
      **************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNKUMI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNKUMI
           END-IF
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      **************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      **************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      **************************************************************
       900-SRYACT-READ-SEC         SECTION.
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
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
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
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE     "tbl_tensu"            TO  MCP-TABLE
           MOVE     "key"                  TO  MCP-PATHNAME
           PERFORM   DB-CLOSE-SEC
      *
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    ¼ýÇ¼ÌÀºÙÆÉ¹þ
      **************************************************************
       900-SYUMEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
               MOVE    ZERO            TO  FLG-SYUMEI
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¾ðÊóÆÉ¹þ
      *****************************************************************
       900-PTINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       DB-CLOSE-EXT.
           EXIT.
      *
