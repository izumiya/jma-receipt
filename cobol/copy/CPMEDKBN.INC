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
      *****************************************************************
      *    °åÎÅ¶èÊ¬¾ðÊó          COPY (CPMEDKBN.INC)                  *
      *****************************************************************
      *****************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
       01  MEDH20-AREA.
           03  MEDH20-VAL.
               05  MEDH20-01FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-01FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-01FL01    PIC X(09)    VALUE  "820000060".
               05  MEDH20-01FL02    PIC X(09)    VALUE  "820000096".
               05  MEDH20-01FL03    PIC X(01)    VALUE  "3".
               05  MEDH20-01FL04    PIC 9(03)    VALUE  7.
               05  MEDH20-01FL05    PIC 9(01)    VALUE  0.
               05  MEDH20-01FL06    PIC 9(04)    VALUE  2004.
               05  MEDH20-01FL07    PIC X(04)    VALUE  "£°£±".
               05  MEDH20-01FL08    PIC X(06)    VALUE  "¡¡£±¡¡".
               05  MEDH20-01FL09    PIC X(100)   VALUE  
           "£²£´»þ´Ö»ýÂ³¤·¤ÆÅÀÅ©¤ò¼Â»Ü".
               05  MEDH20-02FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-02FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-02FL01    PIC X(09)    VALUE  "820000076".
               05  MEDH20-02FL02    PIC X(09)    VALUE  "820000097".
               05  MEDH20-02FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-02FL04    PIC 9(03)    VALUE 14.
               05  MEDH20-02FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-02FL06    PIC 9(04)    VALUE  2020.
               05  MEDH20-02FL07    PIC X(04)    VALUE  "£°£²".
               05  MEDH20-02FL08    PIC X(06)    VALUE  "¡¡£²¡¡".
               05  MEDH20-02FL09    PIC X(100)   VALUE  
           "Ç¢Ï©´¶À÷¾É¤ËÂÐ¤¹¤ë¼£ÎÅ¤ò¼Â»Ü".
               05  MEDH20-03FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-03FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-03FL01    PIC X(09)    VALUE  "820000077".
               05  MEDH20-03FL02    PIC X(09)    VALUE  "820000098".
               05  MEDH20-03FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-03FL04    PIC 9(03)    VALUE 30.
               05  MEDH20-03FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-03FL06    PIC 9(04)    VALUE  2021.
               05  MEDH20-03FL07    PIC X(04)    VALUE  "£°£³".
               05  MEDH20-03FL08    PIC X(06)    VALUE  "¡¡£³¡¡".
               05  MEDH20-03FL09    PIC X(100)   VALUE  
           "½ýÉÂÅù¤Ë¤è¤ê¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó¤¬É¬Í×¤Ê¾õÂÖ".
               05  MEDH20-04FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-04FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-04FL01    PIC X(09)    VALUE  "820000078".
               05  MEDH20-04FL02    PIC X(09)    VALUE  "820000099".
               05  MEDH20-04FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-04FL04    PIC 9(03)    VALUE 7.
               05  MEDH20-04FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-04FL06    PIC 9(04)    VALUE  2022.
               05  MEDH20-04FL07    PIC X(04)    VALUE  "£°£´".
               05  MEDH20-04FL08    PIC X(06)    VALUE  "¡¡£´¡¡".
               05  MEDH20-04FL09    PIC X(100)   VALUE  
           "Ã¦¿å¤ËÂÐ¤¹¤ë¼£ÎÅ¤ò¼Â»Ü¡¢¤«¤Ä¡¢È¯Ç®¤òÈ¼¤¦¾õÂÖ".
               05  MEDH20-05FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-05FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-05FL01    PIC X(09)    VALUE  "820000079".
               05  MEDH20-05FL02    PIC X(09)    VALUE  "820000100".
               05  MEDH20-05FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-05FL04    PIC 9(03)    VALUE 7.
               05  MEDH20-05FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-05FL06    PIC 9(04)    VALUE  2023.
               05  MEDH20-05FL07    PIC X(04)    VALUE  "£°£µ".
               05  MEDH20-05FL08    PIC X(06)    VALUE  "¡¡£µ¡¡".
               05  MEDH20-05FL09    PIC X(100)   VALUE  
           "¾Ã²½´ÉÅù¤ÎÂÎÆâ¤«¤é¤Î½Ð·ì¤¬È¿Éü·ÑÂ³".
               05  MEDH20-06FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-06FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-06FL01    PIC X(09)    VALUE  "820000080".
               05  MEDH20-06FL02    PIC X(09)    VALUE  "820000101".
               05  MEDH20-06FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-06FL04    PIC 9(03)    VALUE 3.
               05  MEDH20-06FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-06FL06    PIC 9(04)    VALUE  2024.
               05  MEDH20-06FL07    PIC X(04)    VALUE  "£°£¶".
               05  MEDH20-06FL08    PIC X(06)    VALUE  "¡¡£¶¡¡".
               05  MEDH20-06FL09    PIC X(100)   VALUE  
           "ÉÑ²ó¤ÎÓÒÅÇ¤ËÂÐ¤¹¤ë¼£ÎÅ¤ò¼Â»Ü¡¢¤«¤Ä¡¢È¯Ç®¤òÈ¼¤¦¾õÂÖ".
               05  MEDH20-07FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-07FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-07FL01    PIC X(09)    VALUE  "820000083".
               05  MEDH20-07FL02    PIC X(09)    VALUE  "820000102".
               05  MEDH20-07FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-07FL04    PIC 9(03)    VALUE 7.
               05  MEDH20-07FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-07FL06    PIC 9(04)    VALUE  2027.
               05  MEDH20-07FL07    PIC X(04)    VALUE  "£°£·".
               05  MEDH20-07FL08    PIC X(06)    VALUE  "¡¡£·¡¡".
               05  MEDH20-07FL09    PIC X(100)   VALUE  
           "¤»¤óÌÑ¤ËÂÐ¤¹¤ë¼£ÎÅ¤ò¼Â»Ü".
               05  MEDH20-08FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-08FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-08FL01    PIC X(09)    VALUE  "820000087".
               05  MEDH20-08FL02    PIC X(09)    VALUE  "820000103".
               05  MEDH20-08FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-08FL04    PIC 9(03)    VALUE 7.
               05  MEDH20-08FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-08FL06    PIC 9(04)    VALUE  2031.
               05  MEDH20-08FL07    PIC X(04)    VALUE  "£°£¸".
               05  MEDH20-08FL08    PIC X(06)    VALUE  "¡¡£¸¡¡".
               05  MEDH20-08FL09    PIC X(100)   VALUE  
           "·ÐÉ¡°ß´É¤ä·ÐÄ²±ÉÍÜ¤¬¹Ô¤ï¤ì¤Æ¤ª¤ê¡¢È¯Ç®Ëô¤ÏÓÒÅÇ¤òÈ¼¤¦¾õÂÖ".
               05  MEDH20-09FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-09FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-09FL01    PIC X(09)    VALUE  "820000090".
               05  MEDH20-09FL02    PIC X(09)    VALUE  "820000104".
               05  MEDH20-09FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-09FL04    PIC 9(03)    VALUE 3.
               05  MEDH20-09FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-09FL06    PIC 9(04)    VALUE  2034.
               05  MEDH20-09FL07    PIC X(04)    VALUE  "£°£¹".
               05  MEDH20-09FL08    PIC X(06)    VALUE  "¡¡£¹¡¡".
               05  MEDH20-09FL09    PIC X(100)   VALUE  
           "ÉÑ²ó¤Î·ìÅü¸¡ºº¤ò¼Â»Ü".
               05  MEDH20-10FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-10FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-10FL01    PIC X(09)    VALUE  "820000057".
               05  MEDH20-10FL02    PIC X(09)    VALUE  "820000105".
               05  MEDH20-10FL03    PIC X(01)    VALUE  "3".
               05  MEDH20-10FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-10FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-10FL06    PIC 9(04)    VALUE  2001.
               05  MEDH20-10FL07    PIC X(04)    VALUE  "£±£°".
               05  MEDH20-10FL08    PIC X(06)    VALUE  "£±£°¡¡".
               05  MEDH20-10FL09    PIC X(100)   VALUE  
           "¥¹¥â¥ó".
               05  MEDH20-11FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-11FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-11FL01    PIC X(09)    VALUE  "820000066".
               05  MEDH20-11FL02    PIC X(09)    VALUE  "820000106".
               05  MEDH20-11FL03    PIC X(01)    VALUE  "3".
               05  MEDH20-11FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-11FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-11FL06    PIC 9(04)    VALUE  2010.
               05  MEDH20-11FL07    PIC X(04)    VALUE  "£±£±".
               05  MEDH20-11FL08    PIC X(06)    VALUE  "£±£±¡¡".
               05  MEDH20-11FL09    PIC X(100)   VALUE  
         "Ãí£±¡§½ÅÅÙ¤Î»èÂÎÉÔ¼«Í³»ù¡Ê¼Ô¡ËÅù¡Ê¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁ¡ËÅù".
               05  MEDH20-12FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-12FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-12FL01    PIC X(09)    VALUE  "820000058".
               05  MEDH20-12FL02    PIC X(09)    VALUE  "820000107".
               05  MEDH20-12FL03    PIC X(01)    VALUE  "3".
               05  MEDH20-12FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-12FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-12FL06    PIC 9(04)    VALUE  2002.
               05  MEDH20-12FL07    PIC X(04)    VALUE  "£±£²".
               05  MEDH20-12FL08    PIC X(06)    VALUE  "£±£²¡¡".
               05  MEDH20-12FL09    PIC X(100)   VALUE  
           "°å»ÕµÚ¤Ó´Ç¸î¿¦°÷¤Ë¤è¤ê¡¢¾ï»þ¡¢´Æ»ëµÚ¤Ó´ÉÍý¤ò¼Â»Ü".
               05  MEDH20-13FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-13FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-13FL01    PIC X(09)    VALUE  "820000059".
               05  MEDH20-13FL02    PIC X(09)    VALUE  "820000108".
               05  MEDH20-13FL03    PIC X(01)    VALUE  "3".
               05  MEDH20-13FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-13FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-13FL06    PIC 9(04)    VALUE  2003.
               05  MEDH20-13FL07    PIC X(04)    VALUE  "£±£³".
               05  MEDH20-13FL08    PIC X(06)    VALUE  "£±£³¡¡".
               05  MEDH20-13FL09    PIC X(100)   VALUE  
           "Ãæ¿´ÀÅÌ®±ÉÍÜ¤ò¼Â»Ü".
               05  MEDH20-14FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-14FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-14FL01    PIC X(09)    VALUE  "820000061".
               05  MEDH20-14FL02    PIC X(09)    VALUE  "820000109".
               05  MEDH20-14FL03    PIC X(01)    VALUE  "3".
               05  MEDH20-14FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-14FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-14FL06    PIC 9(04)    VALUE  2005.
               05  MEDH20-14FL07    PIC X(04)    VALUE  "£±£´".
               05  MEDH20-14FL08    PIC X(06)    VALUE  "£±£´¡¡".
               05  MEDH20-14FL09    PIC X(100)   VALUE  
           "¿Í¹©¸ÆµÛ´ï¤ò»ÈÍÑ".
               05  MEDH20-15FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-15FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-15FL01    PIC X(09)    VALUE  "820000062".
               05  MEDH20-15FL02    PIC X(09)    VALUE  "820000110".
               05  MEDH20-15FL03    PIC X(01)    VALUE  "3".
               05  MEDH20-15FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-15FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-15FL06    PIC 9(04)    VALUE  2006.
               05  MEDH20-15FL07    PIC X(04)    VALUE  "£±£µ".
               05  MEDH20-15FL08    PIC X(06)    VALUE  "£±£µ¡¡".
               05  MEDH20-15FL09    PIC X(100)   VALUE  
           "¥É¥ì¡¼¥óË¡Ëô¤Ï¶»¹Ð¼ã¤·¤¯¤ÏÊ¢¹Ð¤ÎÀö¾ô¤ò¼Â»Ü".
               05  MEDH20-16FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-16FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-16FL01    PIC X(09)    VALUE  "820000063".
               05  MEDH20-16FL02    PIC X(09)    VALUE  "820000111".
               05  MEDH20-16FL03    PIC X(01)    VALUE  "3".
               05  MEDH20-16FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-16FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-16FL06    PIC 9(04)    VALUE  2007.
               05  MEDH20-16FL07    PIC X(04)    VALUE  "£±£¶".
               05  MEDH20-16FL08    PIC X(06)    VALUE  "£±£¶¡¡".
               05  MEDH20-16FL09    PIC X(100)   VALUE  
           "µ¤´ÉÀÚ³«Ëô¤Ïµ¤´ÉÆâÁÞ´É¤¬¹Ô¤ï¤ì¤Æ¤ª¤ê¡¢¤«¤Ä¡¢È¯Ç®¤òÈ¼¤¦¾õÂÖ".
               05  MEDH20-17FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-17FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-17FL01    PIC X(09)    VALUE  "820000064".
               05  MEDH20-17FL02    PIC X(09)    VALUE  "820000112".
               05  MEDH20-17FL03    PIC X(01)    VALUE  "3".
               05  MEDH20-17FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-17FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-17FL06    PIC 9(04)    VALUE  2008.
               05  MEDH20-17FL07    PIC X(04)    VALUE  "£±£·".
               05  MEDH20-17FL08    PIC X(06)    VALUE  "£±£·¡¡".
               05  MEDH20-17FL09    PIC X(100)   VALUE  
           "»ÀÁÇÎÅË¡¤ò¼Â»Ü".
               05  MEDH20-18FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-18FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-18FL01    PIC X(09)    VALUE  "820000065".
               05  MEDH20-18FL02    PIC X(09)    VALUE  "820000113".
               05  MEDH20-18FL03    PIC X(01)    VALUE  "3".
               05  MEDH20-18FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-18FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-18FL06    PIC 9(04)    VALUE  2009.
               05  MEDH20-18FL07    PIC X(04)    VALUE  "£±£¸".
               05  MEDH20-18FL08    PIC X(06)    VALUE  "£±£¸¡¡".
               05  MEDH20-18FL09    PIC X(100)   VALUE  
           "´¶À÷¾É¤Î¼£ÎÅ¤ÎÉ¬Í×À­¤«¤é³ÖÎ¥¼¼¤Ç¤Î´ÉÍý¤ò¼Â»Ü".
               05  MEDH20-19FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-19FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-19FL01    PIC X(09)    VALUE  "820000067".
               05  MEDH20-19FL02    PIC X(09)    VALUE  "820000114".
               05  MEDH20-19FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-19FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-19FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-19FL06    PIC 9(04)    VALUE  2011.
               05  MEDH20-19FL07    PIC X(04)    VALUE  "£±£¹".
               05  MEDH20-19FL08    PIC X(06)    VALUE  "£±£¹¡¡".
               05  MEDH20-19FL09    PIC X(100)   VALUE  
           "¶Ú¥¸¥¹¥È¥í¥Õ¥£¡¼¾É".
               05  MEDH20-20FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-20FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-20FL01    PIC X(09)    VALUE  "820000068".
               05  MEDH20-20FL02    PIC X(09)    VALUE  "820000115".
               05  MEDH20-20FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-20FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-20FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-20FL06    PIC 9(04)    VALUE  2012.
               05  MEDH20-20FL07    PIC X(04)    VALUE  "£²£°".
               05  MEDH20-20FL08    PIC X(06)    VALUE  "£²£°¡¡".
               05  MEDH20-20FL09    PIC X(100)   VALUE  
           "Â¿È¯À­¹Å²½¾É".
               05  MEDH20-21FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-21FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-21FL01    PIC X(09)    VALUE  "820000069".
               05  MEDH20-21FL02    PIC X(09)    VALUE  "820000116".
               05  MEDH20-21FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-21FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-21FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-21FL06    PIC 9(04)    VALUE  2013.
               05  MEDH20-21FL07    PIC X(04)    VALUE  "£²£±".
               05  MEDH20-21FL08    PIC X(06)    VALUE  "£²£±¡¡".
               05  MEDH20-21FL09    PIC X(100)   VALUE  
           "¶Ú°à½ÌÀ­Â¦º÷¹Å²½¾É".
               05  MEDH20-22FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-22FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-22FL01    PIC X(09)    VALUE  "820000070".
               05  MEDH20-22FL02    PIC X(09)    VALUE  "820000117".
               05  MEDH20-22FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-22FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-22FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-22FL06    PIC 9(04)    VALUE  2014.
               05  MEDH20-22FL07    PIC X(04)    VALUE  "£²£²".
               05  MEDH20-22FL08    PIC X(06)    VALUE  "£²£²¡¡".
               05  MEDH20-22FL09    PIC X(100)   VALUE  
           "¥Ñ¡¼¥­¥ó¥½¥óÉÂ´ØÏ¢¼À´µ".
               05  MEDH20-23FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-23FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-23FL01    PIC X(09)    VALUE  "820000071".
               05  MEDH20-23FL02    PIC X(09)    VALUE  "820000118".
               05  MEDH20-23FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-23FL04    PIC 9(03)    VALUE  0.
               05  MEDH20-23FL05    PIC 9(01)    VALUE  1.
               05  MEDH20-23FL06    PIC 9(04)    VALUE  2015.
               05  MEDH20-23FL07    PIC X(04)    VALUE  "£²£³".
               05  MEDH20-23FL08    PIC X(06)    VALUE  "£²£³¡¡".
               05  MEDH20-23FL09    PIC X(100)   VALUE  
           "¤½¤ÎÂ¾¤ÎÆñÉÂ¡Ê¥¹¥â¥óµÚ¤Ó£²£°¡Á£²£²¤Þ¤Ç¤ò½ü¤¯¡£¡Ë".
               05  MEDH20-24FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-24FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-24FL01    PIC X(09)    VALUE  "820000072".
               05  MEDH20-24FL02    PIC X(09)    VALUE  "820000119".
               05  MEDH20-24FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-24FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-24FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-24FL06    PIC 9(04)    VALUE  2016.
               05  MEDH20-24FL07    PIC X(04)    VALUE  "£²£´".
               05  MEDH20-24FL08    PIC X(06)    VALUE  "£²£´¡¡".
               05  MEDH20-24FL09    PIC X(100)   VALUE  
           "ÀÔ¿ñÂ»½ý".
               05  MEDH20-25FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-25FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-25FL01    PIC X(09)    VALUE  "820000073".
               05  MEDH20-25FL02    PIC X(09)    VALUE  "820000120".
               05  MEDH20-25FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-25FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-25FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-25FL06    PIC 9(04)    VALUE  2017.
               05  MEDH20-25FL07    PIC X(04)    VALUE  "£²£µ".
               05  MEDH20-25FL08    PIC X(06)    VALUE  "£²£µ¡¡".
               05  MEDH20-25FL09    PIC X(100)   VALUE  
           "ËýÀ­ÊÄºÉÀ­ÇÙ¼À´µ".
               05  MEDH20-26FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-26FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-26FL01    PIC X(09)    VALUE  "820000086".
               05  MEDH20-26FL02    PIC X(09)    VALUE  "820000121".
               05  MEDH20-26FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-26FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-26FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-26FL06    PIC 9(04)    VALUE  2030.
               05  MEDH20-26FL07    PIC X(04)    VALUE  "£²£¶".
               05  MEDH20-26FL08    PIC X(06)    VALUE  "£²£¶¡¡".
               05  MEDH20-26FL09    PIC X(100)   VALUE  
           "¿Í¹©¿ÕÂ¡¡¢»ýÂ³´Ë½ù¼°·ì±ÕßÉ²á¡¢Ê¢ËìÞõÎ®Ëô¤Ï·ìÞù¸ò´¹¤ò¼Â»Ü".
               05  MEDH20-27FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-27FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-27FL01    PIC X(09)    VALUE  "820000092".
               05  MEDH20-27FL02    PIC X(09)    VALUE  "820000122".
               05  MEDH20-27FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-27FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-27FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-27FL06    PIC 9(04)    VALUE  2036.
               05  MEDH20-27FL07    PIC X(04)    VALUE  "£²£·".
               05  MEDH20-27FL08    PIC X(06)    VALUE  "£²£·¡¡".
               05  MEDH20-27FL09    PIC X(100)   VALUE  
         "Ãí£²¡§½ÅÅÙ¤Î»èÂÎÉÔ¼«Í³»ù¡Ê¼Ô¡ËÅù¡Ê¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁ¡ËÅù".
               05  MEDH20-28FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-28FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-28FL01    PIC X(09)    VALUE  "820000093".
               05  MEDH20-28FL02    PIC X(09)    VALUE  "820000123".
               05  MEDH20-28FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-28FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-28FL05    PIC 9(01)    VALUE 1.
               05  MEDH20-28FL06    PIC 9(04)    VALUE  2037.
               05  MEDH20-28FL07    PIC X(04)    VALUE  "£²£¸".
               05  MEDH20-28FL08    PIC X(06)    VALUE  "£²£¸¡¡".
               05  MEDH20-28FL09    PIC X(100)   VALUE  
           "´ðËÜ¿ÇÎÅÎÁ¤Î»ÜÀß´ð½àÅù¤ÎÊÌÉ½Âè¸Þ¤Î»°¤Î»°¤Î´µ¼Ô".
               05  MEDH20-29FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-29FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-29FL01    PIC X(09)    VALUE  "820000074".
               05  MEDH20-29FL02    PIC X(09)    VALUE  "820000124".
               05  MEDH20-29FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-29FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-29FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-29FL06    PIC 9(04)    VALUE  2018.
               05  MEDH20-29FL07    PIC X(04)    VALUE  "£²£¹".
               05  MEDH20-29FL08    PIC X(06)    VALUE  "£²£¹¡¡".
               05  MEDH20-29FL09    PIC X(100)   VALUE  
           "°­À­¼ðáç".
               05  MEDH20-30FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-30FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-30FL01    PIC X(09)    VALUE  "820000075".
               05  MEDH20-30FL02    PIC X(09)    VALUE  "820000125".
               05  MEDH20-30FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-30FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-30FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-30FL06    PIC 9(04)    VALUE  2019.
               05  MEDH20-30FL07    PIC X(04)    VALUE  "£³£°".
               05  MEDH20-30FL08    PIC X(06)    VALUE  "£³£°¡¡".
               05  MEDH20-30FL09    PIC X(100)   VALUE  
           "ÇÙ±ê¤ËÂÐ¤¹¤ë¼£ÎÅ¤ò¼Â»Ü".
               05  MEDH20-31FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-31FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-31FL01    PIC X(09)    VALUE  "820000081".
               05  MEDH20-31FL02    PIC X(09)    VALUE  "820000126".
               05  MEDH20-31FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-31FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-31FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-31FL06    PIC 9(04)    VALUE  2025.
               05  MEDH20-31FL07    PIC X(04)    VALUE  "£³£±".
               05  MEDH20-31FL08    PIC X(06)    VALUE  "£³£±¡¡".
               05  MEDH20-31FL09    PIC X(100)   VALUE  
           "êóáì¤ËÂÐ¤¹¤ë¼£ÎÅ¤ò¼Â»Ü".
               05  MEDH20-32FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-32FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-32FL01    PIC X(09)    VALUE  "820000082".
               05  MEDH20-32FL02    PIC X(09)    VALUE  "820000127".
               05  MEDH20-32FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-32FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-32FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-32FL06    PIC 9(04)    VALUE  2026.
               05  MEDH20-32FL07    PIC X(04)    VALUE  "£³£²".
               05  MEDH20-32FL08    PIC X(06)    VALUE  "£³£²¡¡".
               05  MEDH20-32FL09    PIC X(100)   VALUE  
           "Ëö¾¿½Û´Ä¾ã³²¤Ë¤è¤ë²¼»èËöÃ¼¤Î³«ÊüÁÏ¤Î¼£ÎÅ¤ò¼Â»Ü".
               05  MEDH20-33FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-33FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-33FL01    PIC X(09)    VALUE  "820000084".
               05  MEDH20-33FL02    PIC X(09)    VALUE  "820000128".
               05  MEDH20-33FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-33FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-33FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-33FL06    PIC 9(04)    VALUE  2028.
               05  MEDH20-33FL07    PIC X(04)    VALUE  "£³£³".
               05  MEDH20-33FL08    PIC X(06)    VALUE  "£³£³¡¡".
               05  MEDH20-33FL09    PIC X(100)   VALUE  
           "¤¦¤Ä¾É¾õ¤ËÂÐ¤¹¤ë¼£ÎÅ¤ò¼Â»Ü".
               05  MEDH20-34FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-34FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-34FL01    PIC X(09)    VALUE  "820000085".
               05  MEDH20-34FL02    PIC X(09)    VALUE  "820000129".
               05  MEDH20-34FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-34FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-34FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-34FL06    PIC 9(04)    VALUE  2029.
               05  MEDH20-34FL07    PIC X(04)    VALUE  "£³£´".
               05  MEDH20-34FL08    PIC X(06)    VALUE  "£³£´¡¡".
               05  MEDH20-34FL09    PIC X(100)   VALUE  
           "Â¾¼Ô¤ËÂÐ¤¹¤ëË½¹Ô¤¬ËèÆüÇ§¤á¤é¤ì¤ë¾õÂÖ".
               05  MEDH20-35FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-35FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-35FL01    PIC X(09)    VALUE  "820000088".
               05  MEDH20-35FL02    PIC X(09)    VALUE  "820000130".
               05  MEDH20-35FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-35FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-35FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-35FL06    PIC 9(04)    VALUE  2032.
               05  MEDH20-35FL07    PIC X(04)    VALUE  "£³£µ".
               05  MEDH20-35FL08    PIC X(06)    VALUE  "£³£µ¡¡".
               05  MEDH20-35FL09    PIC X(100)   VALUE  
           "£±Æü£¸²ó°Ê¾å¤ÎÓ½áâµÛ°ú¤ò¼Â»Ü".
               05  MEDH20-36FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-36FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-36FL01    PIC X(09)    VALUE  "820000089".
               05  MEDH20-36FL02    PIC X(09)    VALUE  "820000131".
               05  MEDH20-36FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-36FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-36FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-36FL06    PIC 9(04)    VALUE  2033.
               05  MEDH20-36FL07    PIC X(04)    VALUE  "£³£¶".
               05  MEDH20-36FL08    PIC X(06)    VALUE  "£³£¶¡¡".
               05  MEDH20-36FL09    PIC X(100)   VALUE  
           "µ¤´ÉÀÚ³«Ëô¤Ïµ¤´ÉÆâÁÞ´É¤¬¹Ô¤ï¤ì¤Æ¤¤¤ë¾õÂÖ".
               05  MEDH20-37FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-37FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH20-37FL01    PIC X(09)    VALUE  "820000091".
               05  MEDH20-37FL02    PIC X(09)    VALUE  "820000132".
               05  MEDH20-37FL03    PIC X(01)    VALUE  "2".
               05  MEDH20-37FL04    PIC 9(03)    VALUE 0.
               05  MEDH20-37FL05    PIC 9(01)    VALUE 0.
               05  MEDH20-37FL06    PIC 9(04)    VALUE  2035.
               05  MEDH20-37FL07    PIC X(04)    VALUE  "£³£·".
               05  MEDH20-37FL08    PIC X(06)    VALUE  "£³£·¡¡".
               05  MEDH20-37FL09    PIC X(100)   VALUE  
           "ÁÏ½ý¡¢ÈéÉæÄÙáçËô¤ÏËªÁã±ê¡¢Ç¿Åù¤Î´¶À÷¾É¤ËÂÐ¤¹¤ë¼£ÎÅ¤ò¼Â»Ü".
               05  MEDH20-91FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-91FLE     PIC X(08)    VALUE  "20120331".
               05  MEDH22-91FL01    PIC X(09)    VALUE  "000000000".
               05  MEDH22-91FL02    PIC X(09)    VALUE  "820000158".
               05  MEDH22-91FL03    PIC X(01)    VALUE  "0".
               05  MEDH22-91FL04    PIC 9(03)    VALUE 0.
               05  MEDH22-91FL05    PIC 9(01)    VALUE 0.
               05  MEDH22-91FL06    PIC 9(04)    VALUE  2091.
               05  MEDH22-91FL07    PIC X(04)    VALUE  "£¹£±".
               05  MEDH22-91FL08    PIC X(06)    VALUE  "£¹£±¡¡".
               05  MEDH22-91FL09    PIC X(100)   VALUE  
           "êóáì¤ËÂÐ¤¹¤ë¼£ÎÅ¤ò¼Â»Ü¤·¤Æ¤¤¤ë¾õÂÖ¡Ê£³£±¤Î¾ì¹ç¤ò½ü¤¯¡Ë".
               05  MEDH20-92FLS     PIC X(08)    VALUE  "00000000".
               05  MEDH20-92FLE     PIC X(08)    VALUE  "20120331".
               05  MEDH22-92FL01    PIC X(09)    VALUE  "000000000".
               05  MEDH22-92FL02    PIC X(09)    VALUE  "820000159".
               05  MEDH22-92FL03    PIC X(01)    VALUE  "0".
               05  MEDH22-92FL04    PIC 9(03)    VALUE 0.
               05  MEDH22-92FL05    PIC 9(01)    VALUE 0.
               05  MEDH22-92FL06    PIC 9(04)    VALUE  2092.
               05  MEDH22-92FL07    PIC X(04)    VALUE  "£¹£²".
               05  MEDH22-92FL08    PIC X(06)    VALUE  "£¹£²¡¡".
               05  MEDH22-92FL09    PIC X(100)   VALUE  
           "¿ÈÂÎÍÞÀ©¤ò¼Â»Ü¤·¤Æ¤¤¤ë".
               05  MEDH24-91FLS     PIC X(08)    VALUE  "20120401".
               05  MEDH24-91FLE     PIC X(08)    VALUE  "99999999".
               05  MEDH24-91FL01    PIC X(09)    VALUE  "820000159".
               05  MEDH24-91FL02    PIC X(09)    VALUE  "820000160".
               05  MEDH24-91FL03    PIC X(01)    VALUE  "0".
               05  MEDH24-91FL04    PIC 9(03)    VALUE 0.
               05  MEDH24-91FL05    PIC 9(01)    VALUE 0.
               05  MEDH24-91FL06    PIC 9(04)    VALUE  2092.
               05  MEDH24-91FL07    PIC X(04)    VALUE  "£¹£±".
               05  MEDH24-91FL08    PIC X(06)    VALUE  "£¹£±¡¡".
               05  MEDH24-91FL09    PIC X(100)   VALUE  
           "¿ÈÂÎÍÞÀ©¤ò¼Â»Ü¤·¤Æ¤¤¤ë".
           03  MEDH20-R    REDEFINES   MEDH20-VAL.
               05  MEDH20-OCC          OCCURS  40.
                   07  MEDH20-STYMD.
                       09  MEDH20-STYM PIC X(06).
                       09  MEDH20-STDD PIC 9(02).
                   07  MEDH20-EDYMD.
                       09  MEDH20-EDYM PIC X(06).
                       09  MEDH20-EDDD PIC 9(02).
                   07  MEDH20-CDL      PIC X(09).
                   07  MEDH20-SRYCDL   PIC X(09).
                   07  MEDH20-MEDKBN   PIC X(01).
                   07  MEDH20-LIMIT    PIC 9(03).
                   07  MEDH20-ALLKBN   PIC 9(01).
                   07  MEDH20-ZAINUM   PIC 9(04).
                   07  MEDH20-NUML     PIC X(04).
                   07  MEDH20-MEIL     PIC X(106).
       01  MEDH20-MAX                  PIC 9(03)     VALUE  40.
