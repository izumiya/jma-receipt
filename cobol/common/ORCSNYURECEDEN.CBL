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
       PROGRAM-ID.                 ORCSNYURECEDEN.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥ì¥»ÅÅÍÑÆþ±¡²ñ·×¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  04/05/18    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-Æ£¸¶    04/10/19  ¥³¥á¥ó¥ÈËä¤á¹þ¤ßÊ¸»ú¤ÎÊÑ¹¹
      *  01.00.02    NACL-¾®Æ¦Âô  05/06/14  Äã½êÆÀ´µ¼Ô¤Î¿©»öÉ¸½àÉéÃ´³Û¤Î
      *                                     È½ÄêÊýË¡¤òÊÑ¹¹¡Ê£±ÆüÍ­¸ú¤ÇÈ½Äê¡Ë
      *  01.00.03    NACL-¾®Æ¦Âô  05/09/16  °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤Î³°Çñ·×»»¤ò½¤Àµ
      *                                     ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *  01.00.04    NACL-¾®Æ¦Âô  06/01/06  ¿©»ö¤ÎÆÃÊÌ´ÉÍý²Ã»»¤ò·îÅÓÃæ¤ÇÊÑ¹¹
      *                                     ¤·¤¿¾ì¹ç¤ÎÂÐ½èÄÉ²Ã
      *Ê¿À®£±£¸Ç¯£´·î²þÄêÂÐ±þ
      *  01.00.05    NACL-¾®Æ¦Âô  06/03/22  ÆÃÄêÆþ±¡ÎÁÄÉ²ÃÊ¬¡ÊÇ¾Â´Ãæ¥±¥¢¥æ¥Ë¡Ë
      *                                     ³°ÇñÃÖ¤­´¹¤¨½èÍý
      *  01.00.06    NACL-¾®Æ¦Âô  06/04/05  MONFUNCÂÐ±þ
      *  01.00.07    NACL-¾®Æ¦Âô  06/04/19  ¿©»öµ­Ï¿ÊýË¡¤ÎÊÑ¹¹ÂÐ±þ
      *  01.00.08    NACL-¾®Æ¦Âô  06/05/23  ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ½¤Àµ
      *  01.00.09    NACL-¾®Æ¦Âô  06/09/08  É¸·çÂÐ±þ
      *  01.00.10    NACL-¾®Æ¦Âô  06/09/13  Äê¿ôÄ¶²áÂÐ±þ
      *  01.00.11    NACL-¾®Æ¦Âô  06/10/06  ÎÅÍÜÉÂ¾²¥ì¥»¥³¥á¥ó¥ÈÂÐ±þ
      *  01.00.12    NACL-¾®Æ¦Âô  06/10/11  Æþ±¡»þÀ¸³èÎÅÍÜÈñÂÐ±þ
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
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-ROUSAI                  PIC 9(01).
           03  FLG-RYAKU                   PIC 9(01).
           03  FLG-OK                      PIC 9(01).
           03  FLG-KSN                     PIC 9(01).
           03  FLG-SENTEI-SET              PIC 9(01).
           03  FLG-SENTEICDCHG             PIC 9(01).
           03  FLG-TSYRRK                  PIC 9(01).
           03  FLG-TNKRRK                  PIC 9(01).
           03  FLG-TENSUWRK                PIC 9(01).
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDX5                        PIC 9(04).
           03  IDX6                        PIC 9(04).
           03  IDX7                        PIC 9(04).
           03  KSN-IDX                     PIC 9(04).
           03  GENSAN-IDX                  PIC 9(04).
           03  GENSAN-IDX2                 PIC 9(04).
           03  IDV                         PIC 9(04).
           03  TSY90-IDX                   PIC 9(02).
           03  ETC-IDX                     PIC 9(04).
           03  IDX-TSY90                   PIC 9(04).
           03  IDX-ADL                     PIC 9(04).
           03  MATOME-IDX                  PIC 9(04).
           03  CIDX                        PIC 9(04).
           03  CIDX2                       PIC 9(04).
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
       01  CONSTANT-AREA.
      *    Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»
           03  CONS-KEIKAKU-GENSAN1        PIC X(09) VALUE "190080470".
           03  CONS-KEIKAKU-GENSAN2        PIC X(09) VALUE "190799670".
      *    ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»
           03  CONS-INNAI-GENSAN1          PIC X(09) VALUE "190080570".
           03  CONS-INNAI-GENSAN2          PIC X(09) VALUE "190799770".
      *    °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»
           03  CONS-ANZEN-GENSAN1          PIC X(09) VALUE "190111690".
           03  CONS-ANZEN-GENSAN2          PIC X(09) VALUE "190809090".
      *    êóáìÂÐºöÌ¤¼Â»Ü¸º»»
           03  CONS-JYOKU-GENSAN1          PIC X(09) VALUE "190111790".
           03  CONS-JYOKU-GENSAN2          PIC X(09) VALUE "190809190".
      *    Æü¾ïÀ¸³è¾ã³²²Ã»»
           03  CONS-NICHIJOU-KSN1          PIC X(09) VALUE "190109170".
           03  CONS-NICHIJOU-KSN2          PIC X(09) VALUE "190807570".
      *    Ç§ÃÎ¾É²Ã»»
           03  CONS-CHIHOU-KSN1            PIC X(09) VALUE "190109270".
           03  CONS-CHIHOU-KSN2            PIC X(09) VALUE "190808670".
      *    êóáì´µ¼Ô´ÉÍý²Ã»»
           03  CONS-JYOKU-KSN1             PIC X(09) VALUE "190116990".
           03  CONS-JYOKU-KSN2             PIC X(09) VALUE "190813390".
      *    ÆÃÄê¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
           03  CONS-TOKUSEIYAKU-KSN        PIC X(09) VALUE "190117570".
      *    ¤½¤ÎÂ¾¿ÇÎÅ¹Ô°Ù¤ÇÆþÎÏ¤·¤¿Æþ±¡ÎÁ²Ã»»
           03  CONS-ETCNYUIN-TBL.
             05  CONS-ETCNYUIN-KSN         PIC X(09)  OCCURS 10.
      *    ÅÀ¿ô¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß¥ï¡¼¥¯ÍÑ
           03  TTNS-CONST-MAX              PIC 9(03) VALUE 200.
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
           03  WRK-MM-X                    PIC X(02).
           03  WRK-MM-9     REDEFINES   WRK-MM-X
                                           PIC 9(02).
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-KSNKBN                  PIC 9(03).
      *
           03  WRK-SYS5000-COUNT           PIC 9(02).
      *
           03  WRK-GENSAN2-MIJISI          PIC 9(01).
           03  WRK-GENSAN3-MIJISI          PIC 9(01).
           03  WRK-GENSAN4-MIJISI          PIC 9(01).
      *
      *    Æþ±¡²ñ·×¥ï¡¼¥¯
           03  WRK-NYUIN-TABLE.
               05  WRK-NYUIN-TBL           OCCURS  31.
                   07  WRK-HKNCOMBI-DAY    PIC 9(03).
                   07  WRK-SRYKBN          PIC X(02).
                   07  WRK-NYUINKHN        PIC X(09).
                   07  WRK-SHOKIKSN        PIC X(09).
                   07  WRK-HYOUKETSU       PIC X(09).
                   07  WRK-TEISU           PIC X(09).
                   07  WRK-GAIHAKU         PIC X(09).
                   07  WRK-NYUKHNKSNKBN    PIC 9(03).
                   07  WRK-GAIHAKU-KBN     PIC 9(01).
      *
      *    Æþ±¡²ñ·×ÊÂ¤ÓÂØ¤¨¥ï¡¼¥¯
           03  WRK-NYUIN-SORT-TABLE.
               05  WRK-NYUIN-SORT-TBL      OCCURS  31.
                   07  WRK-SO-MEISAISU     PIC 9(02).
                   07  WRK-SO-HKNCOMBI-DAY PIC 9(03).
                   07  WRK-SO-SRYKBN       PIC X(02).
                   07  WRK-SO-SRYCD        PIC X(09)  OCCURS 20.
                   07  WRK-SO-SRYCDTOTAL   PIC 9(14).
                   07  WRK-SO-TEN          PIC 9(07).
                   07  WRK-SO-KAISU        PIC 9(02).
                   07  WRK-SO-ADL-TEN-MOJI PIC X(04).
                   07  WRK-SO-GAIHAKU-TBL.
                     09  WRK-SO-GAIHAKU-KBN  PIC 9(01)  OCCURS 31.
                   07  WRK-SO-IRYOADL-DAY  PIC 9(02).
      *
      *    Æ±°ìºÞ¤Þ¤È¤áÍÑÈæ³Ó¥­¡¼
           03  WRK-OLD-KEY.
               05  WRK-OLD-MEISAISU        PIC 9(02).
               05  WRK-OLD-HKNCOMBI-DAY    PIC 9(03).
               05  WRK-OLD-SRYCDTOTAL      PIC 9(14).
               05  WRK-OLD-TEN             PIC 9(07).
           03  WRK-NEW-KEY.
               05  WRK-NEW-MEISAISU        PIC 9(02).
               05  WRK-NEW-HKNCOMBI-DAY    PIC 9(03).
               05  WRK-NEW-SRYCDTOTAL      PIC 9(14).
               05  WRK-NEW-TEN             PIC 9(07).
      *
      *    ÎÅÍÜÉÂ¾²¥³¥á¥ó¥ÈÊÂ¤ÓÂØ¤¨¥ï¡¼¥¯
           03  WRK-IRYOADL-TABLE.
               05  WRK-IRYOADL-SORT-TBL      OCCURS  31.
                   07  WRK-IRYOADL-DAY          PIC 9(02).
                   07  WRK-IRYOADL-MEISAISU     PIC 9(02).
                   07  WRK-IRYOADL-SRYCD        PIC X(09)  OCCURS 30.
                   07  WRK-IRYOADL-SRYCDTOTAL   PIC 9(14).
                   07  WRK-IRYOADL-ADL-SRYCD    PIC X(09).
                   07  WRK-IRYOADL-TEN          PIC 9(07).
                   07  WRK-IRYOADL-TEN-MOJI     PIC X(04).
                   07  WRK-IRYOADL-GAITOU-TBL.
                     09  WRK-IRYOADL-GAITOU-KBN   PIC 9(01)  OCCURS 31.
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
      *    Ç§ÃÎ¾É²Ã»»ÂàÈò
           03  WRK-CHIHOU-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-CHIHOU-KSN2             PIC 9(03) OCCURS 31.
      *
      *    êóáì´µ¼Ô´ÉÍý²Ã»»
           03  WRK-JYOKU-KSN1              PIC 9(03) OCCURS 31.
           03  WRK-JYOKU-KSN2              PIC 9(03) OCCURS 31.
      *
      *    ÆÃÄê¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
           03  WRK-TOKUSEIYAKU-KSN         PIC 9(03) OCCURS 31.
      *
      *    ¤½¤ÎÂ¾¿ÇÎÅ¹Ô°Ù¤ÇÆþÎÏ¤·¤¿Æþ±¡ÎÁ²Ã»»
           03  WRK-ETCNYUIN-TBL            OCCURS  10.
             05  WRK-ETCNYUIN-KSN          PIC 9(03) OCCURS 31.
      *
      *    ¿ÇÎÅ¥³¡¼¥É·×½¸·×ÍÑ
           03  WRK-SRYCD-X                 PIC X(09).
           03  WRK-SRYCD-9    REDEFINES    WRK-SRYCD-X
                                           PIC 9(09).
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¿©»öÂàÈòÎÎ°è
           03  WRK-SYS5000-SKJ             OCCURS  5.
               05  WRK-SYS5000-SHOKUCD     PIC X(09) OCCURS 31.
      *    ¿©»ö¥ï¡¼¥¯
           03  WRK-SKJ-TABLE.
               05  WRK-SKJ-TBL           OCCURS  31.
                   07  WRK-SKJ-HKNCOMBI-DAY    PIC 9(03).
                   07  WRK-SKJ-GRP.
                       09  WRK-SKJ-RYOYO       PIC X(09).
                       09  WRK-SKJ-TOKU-KANRI  PIC X(09).
                       09  WRK-SKJ-SHOKUDOU    PIC X(09).
                   07  WRK-SKJ-GRPR    REDEFINES  WRK-SKJ-GRP.
                       09  WRK-SKJ-SRYCD       PIC X(09)
                                               OCCURS  3.
                   07  WRK-SKJ-SRYCDTOTAL      PIC 9(14).
                   07  WRK-SKJ-KAISU           PIC 9(02).
                   07  WRK-SKJ-MONEY           PIC 9(07).
      *    ¿©»ö¥ï¡¼¥¯£²¡ÊÊ£¿ô¸¥Î©²Ã»»¡Ë
           03  WRK-SKJ2-TABLE.
               05  WRK-SKJ2-TBL          OCCURS  31.
                   07  WRK-SKJ2-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ2-FUKUSU         PIC X(09).
                   07  WRK-SKJ2-KAISU          PIC 9(02).
      *    ¿©»ö¥ï¡¼¥¯£³¡ÊÆÃÊÌ¿©²Ã»»¡Ë
           03  WRK-SKJ3-TABLE.
               05  WRK-SKJ3-TBL          OCCURS  31.
                   07  WRK-SKJ3-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ3-TOKU-SHOKU     PIC X(09).
                   07  WRK-SKJ3-KAISU          PIC 9(02).
      *    ¿©»ö¥ï¡¼¥¯£´¡Ê¿©»öÎÅÍÜÈñ¡Ë¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
           03  WRK-SKJ4-TABLE.
               05  WRK-SKJ4-TBL          OCCURS  93.
                   07  WRK-SKJ4-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ4-RYOYO          PIC X(09).
                   07  WRK-SKJ4-KAISU          PIC 9(02).
      *    ¿©»ö¥ï¡¼¥¯£µ¡ÊÆÃÊÌ¿©²Ã»»¡Ë¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
           03  WRK-SKJ5-TABLE.
               05  WRK-SKJ5-TBL          OCCURS  93.
                   07  WRK-SKJ5-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ5-TOKU-SHOKU     PIC X(09).
                   07  WRK-SKJ5-KAISU          PIC 9(02).
      *    ¿©»ö¥ï¡¼¥¯£¶¡Ê¿©Æ²²Ã»»¡Ë¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
           03  WRK-SKJ6-TABLE.
               05  WRK-SKJ6-TBL          OCCURS  31.
                   07  WRK-SKJ6-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ6-SHOKUDOU       PIC X(09).
                   07  WRK-SKJ6-KAISU          PIC 9(02).
      *    ¿©»ö¥ï¡¼¥¯£·
           03  WRK-SKJ7-TABLE.
               05  WRK-SKJ7-TBL           OCCURS  31.
                   07  WRK-SKJ7-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ7-GRP.
                       09  WRK-SKJ7-RYOYO       PIC X(09).
                       09  WRK-SKJ7-RYOYO-SU    PIC 9(01).
                       09  WRK-SKJ7-SHOKUDOU    PIC X(09).
                       09  WRK-SKJ7-TOKU-SHOKU  PIC X(09).
                       09  WRK-SKJ7-TOKU-SHOKU-SU PIC 9(01).
                       09  WRK-SKJ7-TOKU-SHOKU-KAISU1 PIC 9(02).
                       09  WRK-SKJ7-TOKU-SHOKU-KAISU2 PIC 9(02).
                       09  WRK-SKJ7-TOKU-SHOKU-KAISU3 PIC 9(02).
                       09  WRK-SKJ7-SKJFTN      PIC X(09).
                       09  WRK-SKJ7-SKJFTN-SU   PIC 9(01).
                   07  WRK-SKJ7-SRYCDTOTAL      PIC 9(14).
                   07  WRK-SKJ7-KAISU           PIC 9(02).
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¥ï¡¼¥¯
           03  WRK-KANKYO-TABLE.
               05  WRK-KANKYO-TBL           OCCURS  31.
                   07  WRK-KANKYO-HKNCOMBI-DAY  PIC 9(03).
                   07  WRK-KANKYO-GRP.
                       09  WRK-KANKYO-RYOYO     PIC X(09).
                       09  WRK-KANKYO-FTN       PIC X(09).
                   07  WRK-KANKYO-SRYCDTOTAL    PIC 9(14).
                   07  WRK-KANKYO-KAISU         PIC 9(02).
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯
           03  WRK-TENSU-OCC                   OCCURS  200.
               05  WRK-TENSU-SRYCD             PIC X(09).
               05  WRK-TENSU-DATA              PIC X(2000).
           03  WRK-TENSU-CD                    PIC X(09).
           03  TTNS-IDX                        PIC 9(03).
           03  TTNS-MAX                        PIC 9(03).
      *
      *    ¿©»öÉéÃ´¥ï¡¼¥¯
           03  WRK-SKJFTN-TABLE.
               05  WRK-SKJFTN-TBL        OCCURS  31.
                   07  WRK-SKJFTN-HKNCOMBI-DAY PIC 9(03).
                   07  WRK-SKJFTN-SRYCD        PIC X(09).
                   07  WRK-SKJFTN-KAISU        PIC 9(02).
      *
      *    ¿©»öÉéÃ´¥ï¡¼¥¯¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
           03  WRK-SKJFTN2-TABLE.
               05  WRK-SKJFTN2-TBL       OCCURS  93.
                   07  WRK-SKJFTN2-HKNCOMBI-DAY PIC 9(03).
                   07  WRK-SKJFTN2-SRYCD        PIC X(09).
                   07  WRK-SKJFTN2-KAISU        PIC 9(02).
      *
      *    ÁªÄêÎÅÍÜÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
           03  TABLE-SENTEI.
               05  TBL-SENTEI                  PIC 9(01)   OCCURS  31.
      *
      *    ³°Çñ¥³¥á¥ó¥ÈÍÑÆüÉÕÂàÈòÎÎ°è
           03  WRK-GAIHAKU-DAY-TABLE.
               05  WRK-GAIHAKU-DAY             OCCURS  31.
                   07  WRK-GAIHAKU-DAY-ST      PIC  9(02).
                   07  WRK-GAIHAKU-DAY-ED      PIC  9(02).
      *
      *    ÎÅÍÜ¥³¥á¥ó¥ÈÍÑÆüÉÕÂàÈòÎÎ°è
           03  WRK-RYOYO-DAY-TABLE.
               05  WRK-RYOYO-DAY               OCCURS  31.
                   07  WRK-RYOYO-DAY-ST        PIC  9(02).
                   07  WRK-RYOYO-DAY-ED        PIC  9(02).
      *
      *    ÆüÉÕÁ´³ÑÊÑ´¹ÎÎ°è
       01  TBL-DAY-AREA.
           03  FILLER              PIC X(04)  VALUE "£°£±".
           03  FILLER              PIC X(04)  VALUE "£°£²".
           03  FILLER              PIC X(04)  VALUE "£°£³".
           03  FILLER              PIC X(04)  VALUE "£°£´".
           03  FILLER              PIC X(04)  VALUE "£°£µ".
           03  FILLER              PIC X(04)  VALUE "£°£¶".
           03  FILLER              PIC X(04)  VALUE "£°£·".
           03  FILLER              PIC X(04)  VALUE "£°£¸".
           03  FILLER              PIC X(04)  VALUE "£°£¹".
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
      *    ÁªÄêÎÅÍÜÈñ
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    Äã½êÆÀ
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    Ç¯¶â
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
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
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¼èÆÀ¥µ¥Ö
           COPY    "CPORCSRYOCOM.INC".
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYULIFE.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *    Æþ±¡²ñ·×ºîÀ®ÍÑ¥Ñ¥é¥á¥¿
           COPY    "CPORCSNYURECEDEN.INC".
      *
       PROCEDURE                   DIVISION    USING
      *     SPA-AREA
           ORCSNYURECEDEN-AREA
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
           INITIALIZE                  LNK-NYURECEDEN-HENKYAKU-AREA
           INITIALIZE                  CONS-ETCNYUIN-TBL
           MOVE    ZERO                TO  LNK-NYURECEDEN-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-NYURECEDEN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ¼ç½èÍý
           IF      LNK-NYURECEDEN-RC      =    ZERO
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
      *    »»ÄêÇ¯·î¥Á¥§¥Ã¥¯
           IF    ( LNK-NYURECEDEN-SANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-NYURECEDEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                      PTINF-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  PTINF-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-NYURECEDEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ÊÝ¸±¶èÊ¬¥Á¥§¥Ã¥¯
           IF     LNK-NYURECEDEN-HKNKBN    =   "1"  OR  "2"
               CONTINUE
           ELSE
               MOVE    3               TO  LNK-NYURECEDEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    Æþ±¡Ãæ¤Î¥Á¥§¥Ã¥¯¡ÊÆþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷¡Ë
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    4               TO  LNK-NYURECEDEN-RC
           ELSE
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    4              TO  LNK-NYURECEDEN-RC
               ELSE
                    PERFORM    VARYING   IDX1   FROM   1   BY   1
                               UNTIL     IDX1   >      31
                        MOVE   NACCT-DAY (IDX1) TO 
                                            WRK-HKNCOMBI-DAY(IDX1)
                    END-PERFORM
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *¥ì¥»ÅÅÂÐ¾ÝÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ¤Î¼èÆÀ
           PERFORM    VARYING   IDX   FROM   1   BY   1
                      UNTIL      IDX  >    31
               IF     WRK-HKNCOMBI-DAY(IDX)   NOT =   ZERO
                   INITIALIZE                          HKNCOMBI-REC
                   MOVE    LNK-NYURECEDEN-HOSPID   TO  COMB-HOSPID
                   MOVE    LNK-NYURECEDEN-PTID     TO  COMB-PTID
                   MOVE    WRK-HKNCOMBI-DAY(IDX)   TO  COMB-HKNCOMBINUM
                   MOVE    "tbl_hkncombi"          TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
                   PERFORM 910-HKNCOMBI-SELECT-SEC
                   IF      FLG-HKNCOMBI            =   ZERO
                       MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM   910-HKNCOMBI-READ-SEC
                       IF   FLG-HKNCOMBI         =  ZERO
                            IF    ( COMB-HKNNUM   = "971" OR "973")
                                            OR
                                  ( COMB-HKNNUM(1:2)  =    "98")
                                MOVE   ZERO   TO  WRK-HKNCOMBI-DAY(IDX)
                            ELSE
                                IF     LNK-NYURECEDEN-HKNKBN  = "1"
                                    IF   COMB-HKNNUM = "060"  OR  "067"
                                        MOVE   ZERO  TO  
                                                  WRK-HKNCOMBI-DAY(IDX)
                                    END-IF
                                ELSE
                                    IF   COMB-HKNNUM = "060"  OR  "067"
                                        CONTINUE
                                    ELSE
                                        MOVE   ZERO  TO  
                                                  WRK-HKNCOMBI-DAY(IDX)
                                    END-IF
                                END-IF
                            END-IF
                       ELSE
                           MOVE    5          TO  LNK-NYURECEDEN-RC
                       END-IF
      *                ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
                       MOVE    "tbl_hkncombi"          TO  MCP-TABLE
                       MOVE    "key"                   TO  MCP-PATHNAME
                       PERFORM   910-DB-CLOSE-SEC
                   ELSE
      *                ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
                       MOVE    "tbl_hkncombi"          TO  MCP-TABLE
                       MOVE    "key"                   TO  MCP-PATHNAME
                       PERFORM   910-DB-CLOSE-SEC
                       MOVE    5              TO  LNK-NYURECEDEN-RC
                   END-IF
               END-IF
           END-PERFORM
      *
      *Æþ±¡ÎÁ¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                ³ºÅöÆþ±¡¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß
                       PERFORM    210-NYUINACT-GET-SEC
      *                Æþ±¡´ðËÜÎÁ¤È½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
                               IF  (NSRY-SRYCD(2) NOT =  SPACE)  AND
                                   (NSRY-SRYCD(2) NOT = "190107890" AND
                                                        "190107990" AND
                                                        "190108590" AND
                                                        "190108690" AND
                                                        "190107690" AND
                                                        "190107790" AND
                                                        "190122990" AND
                                                        "190123090")
                                   MOVE   NSRY-SRYCD(2)  TO
                                                   WRK-SHOKIKSN(IDX1)
                               END-IF
      *                        É¸·ç¥³¡¼¥É
                               IF  (NSRY-SRYCD(2)  NOT =   SPACE)  AND
                                   (NSRY-SRYCD(2)  =   "190107890" OR
                                                       "190107990" OR
                                                       "190108590" OR
                                                       "190108690")
                                   MOVE   NSRY-SRYCD(2)  TO
                                                   WRK-HYOUKETSU(IDX1)
                               END-IF
      *                        É¸·ç¥³¡¼¥É
                               IF  (NSRY-SRYCD(3)  NOT =   SPACE)  AND
                                   (NSRY-SRYCD(3)  =   "190107890" OR
                                                       "190107990" OR
                                                       "190108590" OR
                                                       "190108690")
                                   MOVE   NSRY-SRYCD(3)  TO
                                                   WRK-HYOUKETSU(IDX1)
                               END-IF
      *                        Äê¿ôÄ¶²á¥³¡¼¥É
                               IF  (NSRY-SRYCD(2)  NOT =   SPACE)  AND
                                   (NSRY-SRYCD(2)  =   "190107690" OR
                                                       "190107790" OR
                                                       "190122990" OR
                                                       "190123090")
                                   MOVE   NSRY-SRYCD(2)  TO
                                                   WRK-TEISU(IDX1)
                               END-IF
      *                        Äê¿ôÄ¶²á¥³¡¼¥É
                               IF  (NSRY-SRYCD(3)  NOT =   SPACE)  AND
                                   (NSRY-SRYCD(3)  =   "190107690" OR
                                                       "190107790" OR
                                                       "190122990" OR
                                                       "190123090")
                                   MOVE   NSRY-SRYCD(3)  TO
                                                   WRK-TEISU(IDX1)
                               END-IF
      *                        ¿ÇÎÅ¶èÊ¬
                               MOVE    NACCT-SRYKBN
                                                TO  WRK-SRYKBN(IDX1)
                               IF   FLG-RYAKU   =   ZERO
      *
      *                           ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬
      *                           ¤ÎÂàÈò
                                  PERFORM   210-GAIHAKU-MAE-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                               MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
      *
                               MOVE    1    TO   FLG-OK
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
               IF   FLG-OK     =   ZERO
                   MOVE     3      TO      LNK-NYURECEDEN-RC
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM   910-DB-CLOSE-SEC
                   GO   TO    200-MAIN-EXT
               END-IF
           ELSE
               MOVE     3      TO      LNK-NYURECEDEN-RC
      *        Æþ±¡²ñ·×¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key17"                 TO  MCP-PATHNAME
               PERFORM   910-DB-CLOSE-SEC
               GO   TO    200-MAIN-EXT
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Æþ±¡»þ¤Î³°Çñ¼èÆÀ
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ³°Çñ¶èÊ¬¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *ÁªÄêÎÅÍÜ¾ðÊó¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-SENTEI-SET
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key22"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key22"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ÁªÄêÆþ±¡¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                           MOVE   NACCT-DAY(IDX1)
                                              TO  TBL-SENTEI (IDX1)
                           MOVE     1         TO  FLG-SENTEI-SET 
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key22"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key22"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
           PERFORM   210-RJN-TAISYO-CHK-SEC
      *
      *·îÆâ¤ËÊ£¿ô¤ÎÆþ±¡´ðËÜ¾ðÊó¡Ê£µ£°£°£°¡Ë¤¬¤Ê¤¤¤«¸¡º÷
      *¤³¤ÎÂÐ±þ¤ÏÆÃÊÌ´ÉÍý²Ã»»ÍÑ¤Î°Ù¡¢Ê¿À®£±£¸Ç¯£³·îËø¤È¤¹¤ë
           IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
               PERFORM   210-SYS-5000-CHK-SEC
           END-IF
      *
      *°åÎÅµ¡´Ø¸º»»¾ðÊó¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
      *        ¸º»»¾ðÊó¤Î¼èÆÀ¤ÏÊ¿À®£±£¸Ç¯£³·îËø¤È¤¹¤ë
               IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
                  PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                        UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                                IF   FLG-RJN    =    ZERO
                                    MOVE  "190080570"  TO  WRK-GENSAN2
                                ELSE
                                    MOVE  "190799770"  TO  WRK-GENSAN2
                                END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-INNAI(GENSAN-IDX2)
                                END-PERFORM
                            WHEN  3
                                IF   FLG-RJN    =    ZERO
                                    MOVE  "190111690"  TO  WRK-GENSAN3
                                ELSE
                                    MOVE  "190809090"  TO  WRK-GENSAN3
                                END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-IRYO(GENSAN-IDX2)
                                END-PERFORM
                            WHEN  4
                                IF   FLG-RJN    =    ZERO
                                    MOVE  "190111790"  TO  WRK-GENSAN4
                                ELSE
                                    MOVE  "190809190"  TO  WRK-GENSAN4
                                END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-JYOKU(GENSAN-IDX2)
                                END-PERFORM
                        END-EVALUATE
                    END-IF
                  END-PERFORM
               END-IF
      *        ¿©»ö¾ðÊó¤ÎÂàÈò
               PERFORM    VARYING   IDX  FROM  1  BY  1
                          UNTIL     IDX   >    31
      *            Æþ±¡»þ¿©»öÎÅÍÜÈñ
                   IF     SYS-5000-SHOKUJIRYOYOCD  NOT =  SPACE
                       MOVE  SYS-5000-SHOKUJIRYOYOCD
                                          TO WRK-SYS5000-SHOKUCD(1 IDX)
                   END-IF
      *            ÆÃÊÌ´ÉÍý²Ã»»
      *            Ê¿À®£±£¸Ç¯£³·îËø¤È¤¹¤ë
                   IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
                       IF     SYS-5000-SHOKUCD(1)  NOT =  SPACE
                           IF  SYS-5000-SHOKUKBN(1) =     "2"
                               MOVE  SYS-5000-SHOKUCD(1)
                                          TO WRK-SYS5000-SHOKUCD(2 IDX)
                           END-IF
                       END-IF
                   END-IF
      *             ¿©Æ²²Ã»»¡ÊÆþ±¡²ñ·×¤«¤é¤Î¼èÆÀ¤ËÊÑ¹¹¡Ë
      *             IF     SYS-5000-SHOKUCD(2)  NOT =  SPACE
      *                 IF  SYS-5000-SHOKUKBN(2) =     "2"
      *                     MOVE  SYS-5000-SHOKUCD(2)
      *                                    TO WRK-SYS5000-SHOKUCD(3 IDX)
      *                 END-IF
      *             END-IF
               END-PERFORM
           END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *¤³¤ÎÂÐ±þ¤ÏÆÃÊÌ´ÉÍý²Ã»»ÍÑ¤Î°Ù¡¢Ê¿À®£±£¸Ç¯£³·îËø¤È¤¹¤ë
           IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
               IF        WRK-SYS5000-COUNT    =     ZERO
                     CONTINUE
               ELSE
                     PERFORM  210-SYS-5000-GET-SEC
               END-IF
           END-IF
      *
      *¿©Æ²²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                 IF   NACCT-SRYCDTOTAL     =   197000570
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)
                       AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO  OR  4)
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                           MOVE  "197000570"
                                       TO WRK-SYS5000-SHOKUCD(3 IDX1)
      
                       END-IF
                   END-PERFORM
                 END-IF
                 MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                 MOVE    "key17"                 TO  MCP-PATHNAME
                 PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Æþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
                       IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                       AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO  OR  4)
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                           IF   FLG-KSN   =  ZERO
                               ADD     1    TO    KSN-IDX
                               MOVE    1    TO    FLG-KSN
                           END-IF
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-NYUKSN(KSN-IDX)
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-NYUKSN-ZAITEN(KSN-IDX)
                           MOVE   NACCT-DAY(IDX1)
                                        TO WRK-NYUKSN-DAY(KSN-IDX IDX1)
      *
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *Æþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡Ê¿ÇÎÅ²ñ·×¤è¤ê¡Ë
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  ACCT-HOSPID
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-NYURECEDEN-PTID     TO  ACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  ACCT-SRYYM
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key30"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
                     IF   ACCT-SRYCDTOTAL(6:9)  =   
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
                                        OR  CONS-CHIHOU-KSN2
                                        OR  CONS-JYOKU-KSN1
                                        OR  CONS-JYOKU-KSN2
                                        OR  CONS-TOKUSEIYAKU-KSN
      *                Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                           AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4)
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
      *                         Ç§ÃÎ¾É²Ã»»
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
      *                         ÆÃÄê¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
                                WHEN  CONS-TOKUSEIYAKU-KSN
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-TOKUSEIYAKU-KSN(IDX1)
                              END-EVALUATE
      *
                           END-IF
                       END-PERFORM
                     ELSE
      *                ¤½¤ÎÂ¾Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
      *                ¿ÇÎÅ²ñ·×¤«¤é¿ÇÎÅ¹Ô°Ù¤ò¸¡º÷¤·¡¢Æþ±¡ÎÁ²Ã»»¤Î¿ÇÎÅ
      *                ¥³¡¼¥É¤òÆÃÄê¤¹¤ë
                       IF         ACCT-MEISAISU    =      1
                             CONTINUE
                       ELSE
                          INITIALIZE      SRYACT-REC
                          MOVE    ACCT-HOSPID    TO   SRY-HOSPID
                          MOVE    ACCT-NYUGAIKBN TO   SRY-NYUGAIKBN
                          MOVE    ACCT-PTID      TO   SRY-PTID
                          MOVE    ACCT-SRYKA     TO   SRY-SRYKA
                          MOVE    ACCT-SRYYM     TO   SRY-SRYYM
                          MOVE    ACCT-ZAINUM    TO   SRY-ZAINUM
                          MOVE    1              TO   SRY-RENNUM
                          MOVE    "tbl_sryact"   TO  MCP-TABLE
                          MOVE    "key"          TO  MCP-PATHNAME
                          PERFORM 910-SRYACT-SELECT-SEC
                          IF      FLG-SRYACT     =    ZERO
                              MOVE     "tbl_sryact"   TO  MCP-TABLE
                              MOVE     "key"          TO  MCP-PATHNAME
                              PERFORM   910-SRYACT-READ-SEC
                              IF  FLG-SRYACT    =    ZERO
                                  MOVE  SRY-SRYCD(1)   TO  
                                                  ACCT-SRYCDTOTAL(6:9)   
                              END-IF
                          END-IF
      *                   ¿ÇÎÅ¹Ô°Ù¥¯¥í¡¼¥º
                          MOVE     "tbl_sryact"   TO  MCP-TABLE
                          MOVE     "key"          TO  MCP-PATHNAME
                          PERFORM   910-DB-CLOSE-SEC
                       END-IF
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                           AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4)
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                              EVALUATE   ACCT-SRYCDTOTAL(6:9)
                                WHEN  CONS-ETCNYUIN-KSN(1)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(1 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(2)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(2 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(3)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(3 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(4)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(4 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(5)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(5 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(6)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(6 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(7)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(7 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(8)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(8 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(9)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(9 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(10)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(10 IDX1)
                                WHEN  OTHER
                                  ADD     1     TO   ETC-IDX
                                  MOVE    ACCT-SRYCDTOTAL(6:9) TO  
                                           CONS-ETCNYUIN-KSN(ETC-IDX)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(ETC-IDX
                                                                 IDX1)
                              END-EVALUATE
      *
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key30"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_sryacct"   TO  MCP-TABLE
           MOVE     "key30"          TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    ¼èÆÀ¤·¤¿Æþ±¡¾ðÊó¤«¤éÆüÃ±°Ì¤ÎÆþ±¡²ñ·×¾ðÊó¤òºÆ¹½ÃÛ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ¿ÇÎÅ¶èÊ¬
               MOVE   WRK-SRYKBN(IDX1)  TO  WRK-SO-SRYKBN(IDX1)
      *        ÊÝ¸±ÁÈ¹ç¤»
               MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                        TO  WRK-SO-HKNCOMBI-DAY(IDX1)
               MOVE     ZERO                TO  WRK-MEISAISU
      *        Æþ±¡´ðËÜÎÁ
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-NYUINKHN(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   IF     FLG-SENTEI-SET    NOT =   ZERO
                       IF   TBL-SENTEI(IDX1)    =   1
                           MOVE    ZERO    TO  FLG-SENTEICDCHG
                           INITIALIZE  SENTEICDCHG-REC
                           MOVE    WRK-NYUINKHN(IDX1)
                                           TO  SENCHG-IPNSRYCD
                           MOVE     "tbl_senteicdchg"  TO  MCP-TABLE
                           MOVE     "key2"             TO  MCP-PATHNAME
                           PERFORM   910-SENTEICDCHG-SELECT-SEC
                           IF      FLG-SENTEICDCHG  =    ZERO
                               MOVE     "tbl_senteicdchg"
                                                      TO  MCP-TABLE
                               MOVE     "key2"        TO  MCP-PATHNAME
                               PERFORM  910-SENTEICDCHG-READ-SEC
                               IF    FLG-SENTEICDCHG  =   ZERO
                                   MOVE  SENCHG-SENTEISRYCD
                                                TO  WRK-SO-SRYCD
                                                   (IDX1 WRK-MEISAISU)
                               END-IF
                          END-IF
                          MOVE     "tbl_senteicdchg"   TO  MCP-TABLE
                          MOVE     "key2"              TO  MCP-PATHNAME
                         PERFORM   910-DB-CLOSE-SEC
                       END-IF
                   END-IF
               END-IF
      *
      *        ½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë
               IF      (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        É¸·ç
               IF      WRK-HYOUKETSU(IDX1)   NOT =         SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOUKETSU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        Äê¿ôÄ¶²á
               IF      WRK-TEISU(IDX1)      NOT =         SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-TEISU(IDX1)  TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        ³°Çñ
               IF         WRK-GAIHAKU(IDX1)   NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE   WRK-GAIHAKU-KBN(IDX1)
                                            TO  WRK-SO-GAIHAKU-KBN
                                                           (IDX1 IDX1)
               END-IF
      *        Æþ±¡²ñ·×²Ã»»¾ðÊó
               PERFORM   VARYING   KSN-IDX   FROM   1  BY   1
                         UNTIL (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
                               (KSN-IDX    >      20)
                  IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD   1               TO  WRK-MEISAISU
                      MOVE  WRK-NYUKSN(KSN-IDX)
                                 TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                  END-IF
               END-PERFORM
      *        Æü¾ïÀ¸³è¾ã³²²Ã»»
               IF         WRK-NICHIJOU-KSN1(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-NICHIJOU-KSN1  TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-NICHIJOU-KSN2(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-NICHIJOU-KSN2  TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ç§ÃÎ¾É²Ã»»
               IF         WRK-CHIHOU-KSN1(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-CHIHOU-KSN1     TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-CHIHOU-KSN2(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-CHIHOU-KSN2     TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáì´µ¼Ô´ÉÍý²Ã»»
               IF         WRK-JYOKU-KSN1(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-KSN1      TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-JYOKU-KSN2(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-KSN2      TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÆÃÄê¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
               IF         WRK-TOKUSEIYAKU-KSN(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-TOKUSEIYAKU-KSN TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¤½¤ÎÂ¾Æþ±¡ÎÁ²Ã»»
               PERFORM    VARYING   ETC-IDX   FROM  1  BY  1
                          UNTIL     ETC-IDX  >  10
                   IF  CONS-ETCNYUIN-KSN(ETC-IDX)  NOT =  SPACE
                     IF   WRK-ETCNYUIN-KSN(ETC-IDX IDX1) NOT =   ZERO
                       ADD      1                  TO  WRK-MEISAISU
                       MOVE   CONS-ETCNYUIN-KSN(ETC-IDX)
                                   TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                     END-IF
                   END-IF
               END-PERFORM
      *        Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»
               IF         WRK-KEIKAKU-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KEIKAKU-GENSAN1 TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-KEIKAKU-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KEIKAKU-GENSAN2 TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»
               IF         WRK-INNAI-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-INNAI-GENSAN1   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-INNAI-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-INNAI-GENSAN2   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»
               IF         WRK-ANZEN-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-ANZEN-GENSAN1   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-ANZEN-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-ANZEN-GENSAN2   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáìÂÐºöÌ¤¼Â»Ü¸º»»
               IF         WRK-JYOKU-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-GENSAN1   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-JYOKU-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-GENSAN2   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¡Ë
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-INNAI(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN2       TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN2-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-IRYO(IDX1)   NOT =      SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN3       TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN3-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-JYOKU(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN4       TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN4-MIJISI
               END-IF
      *        ÁªÄêÆþ±¡»þ¤Î³°Çñ
               IF   (TBL-SENTEI(IDX1)    =         1)   AND
                    (WRK-GAIHAKU-KBN(IDX1)     NOT =  ZERO)
                   EVALUATE   WRK-GAIHAKU-KBN(IDX1)
      *                ³°Çñ
                       WHEN   3
                           INITIALIZE   WRK-NYUIN-SORT-TBL(IDX1)
      *                Â¾°åÎÅµ¡´Ø¼õ¿Ç
                       WHEN   4
                           INITIALIZE   WRK-NYUIN-SORT-TBL(IDX1)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿Æþ±¡²ñ·×¾ðÊó¤«¤éÅÀ¿ô¤Î»»Äê
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
               PERFORM   VARYING   IDX2    FROM   1  BY   1
                         UNTIL (WRK-SO-SRYCD(IDX1 IDX2) = SPACE)
                            OR (IDX2    >      20)
                   PERFORM    210-TENSU-KEISAN-SEC
               END-PERFORM
           END-PERFORM
      *
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     31)
      **          DISPLAY  "IDX1 = " IDX1
      **          DISPLAY  "WRK-SO-HKNCOMBI-DAY = " WRK-SO-HKNCOMBI-DAY
      **                                                         (IDX1)
      **          DISPLAY  "WRK-SO-SRYKBN   = " WRK-SO-SRYKBN(IDX1)
      **          DISPLAY  "WRK-SO-MEISAISU = " WRK-SO-MEISAISU(IDX1)
      **          DISPLAY  "WRK-SO-SRYCD(1) = " WRK-SO-SRYCD(IDX1 1)
      **          DISPLAY  "WRK-SO-SRYCD(2) = " WRK-SO-SRYCD(IDX1 2)
      **          DISPLAY  "WRK-SO-SRYCD(3) = " WRK-SO-SRYCD(IDX1 3)
      **          DISPLAY  "WRK-SO-SRYCD(4) = " WRK-SO-SRYCD(IDX1 4)
      **          DISPLAY  "WRK-SO-SRYCD(5) = " WRK-SO-SRYCD(IDX1 5)
      **          DISPLAY  "WRK-SO-SRYCDTOTAL = " WRK-SO-SRYCDTOTAL(IDX1)
      **          DISPLAY  "WRK-SO-TEN      = " WRK-SO-TEN(IDX1)
      **          DISPLAY  "WRK-SO-KAISU    = " WRK-SO-KAISU(IDX1)
      **     END-PERFORM
      *
      *    °åÎÅ¶èÊ¬¡¢£Á£Ä£ÌÅÀ¿ô¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          ORCSRYOCOM-AREA
           MOVE    LNK-NYURECEDEN-HOSPID   TO  LNK-RYOCOM-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-RYOCOM-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-RYOCOM-SANTEIYM
           CALL    "ORCSRYOCOM"     USING  ORCSRYOCOM-AREA
           IF      LNK-RYOCOM-RC           NOT =  ZERO
               DISPLAY  "LNK-RYOCOM-RC = " LNK-RYOCOM-RC
           END-IF
      *
      *     PERFORM   VARYING  IDX1   FROM  1   BY   1
      *               UNTIL    IDX1    >   31
      *       DISPLAY "IDX1 = " IDX1
      *       DISPLAY "LNK-RYOCOM-NYUINKBN = " LNK-RYOCOM-NYUINKBN(IDX1)
      *       DISPLAY "LNK-RYOCOM-MEISAISU = " LNK-RYOCOM-MEISAISU(IDX1)
      *       DISPLAY "LNK-RYOCOM-SRYCDTOTAL = " 
      *                                     LNK-RYOCOM-SRYCDTOTAL(IDX1)
      *       DISPLAY "LNK-RYOCOM-SRYCDTOTAL2 = " 
      *                                     LNK-RYOCOM-SRYCDTOTAL2(IDX1)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 1) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 1)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 2) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 2)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 3) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 3)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 4) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 4)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 5) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 5)
      *       DISPLAY "LNK-RYOCOM-ADL-SRYCD (IDX1) = "
      *                             LNK-RYOCOM-ADL-SRYCD(IDX1)
      *       DISPLAY "LNK-RYOCOM-ADL-TEN (IDX1) = "
      *                             LNK-RYOCOM-ADL-TEN(IDX1)
      *       DISPLAY "LNK-RYOCOM-ADL-TEN-MOJI (IDX1) = "
      *                             LNK-RYOCOM-ADL-TEN-MOJI(IDX1)
      *     END-PERFORM
      *
      *
           MOVE      ZERO      TO   MATOME-IDX
      *
      *    ºÆ¹½ÃÛ¤·¤¿Æþ±¡²ñ·×¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)
                   IF    (LNK-RYOCOM-NYUINKBN(IDX1)   =   "1")   AND
                         (LNK-RYOCOM-MEISAISU(IDX1)   NOT =  ZERO)
                       ADD   1              TO  MATOME-IDX
                       MOVE  MATOME-IDX     TO  WRK-IRYOADL-DAY(IDX1)
                       MOVE  MATOME-IDX     TO  WRK-SO-IRYOADL-DAY(IDX1)
                   END-IF
                   ADD   1                  TO  WRK-SO-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SO-MEISAISU(IDX3)  =
                                             WRK-SO-MEISAISU(IDX1)) AND
                             (WRK-SO-HKNCOMBI-DAY(IDX3)  =
                                         WRK-SO-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SO-SRYCDTOTAL(IDX3)  =
                                         WRK-SO-SRYCDTOTAL(IDX1))   AND
                             (WRK-SO-TEN(IDX3)  =
                                         WRK-SO-TEN(IDX1))
                             IF (LNK-RYOCOM-NYUINKBN(IDX3) = "1")   AND
                                (LNK-RYOCOM-MEISAISU(IDX3) NOT =  ZERO)
                                 MOVE  MATOME-IDX    TO  
                                                 WRK-IRYOADL-DAY(IDX3)
                             END-IF
                             ADD     1    TO    WRK-SO-KAISU(IDX1)
                             IF  WRK-SO-GAIHAKU-KBN(IDX3 IDX3)
                                                             NOT = ZERO
                                 MOVE  WRK-SO-GAIHAKU-KBN(IDX3 IDX3)
                                          TO  WRK-SO-GAIHAKU-KBN
                                                            (IDX1 IDX3)
                             END-IF
                             INITIALIZE   WRK-NYUIN-SORT-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¤ÎÆ±°ìºÞ¤Þ¤È¤á½èÍý
           IF    (MATOME-IDX     NOT =   ZERO)   AND
                 (LNK-RYOCOM-RC  =       ZERO)
      *      ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¼èÆÀ¥µ¥Ö¤ÎÊÖµÑ·ë²Ì¤ò¤Þ¤È¤á¥ï¡¼¥¯¤ËÅ¾Á÷
             PERFORM      VARYING    IDX1   FROM   1  BY  1
               UNTIL      IDX1   >   31
                 IF   (LNK-RYOCOM-NYUINKBN(IDX1)  NOT =   ZERO)   AND
                      (LNK-RYOCOM-MEISAISU(IDX1)  NOT =   ZERO)
                     MOVE   LNK-RYOCOM-MEISAISU(IDX1)
                                   TO  WRK-IRYOADL-MEISAISU(IDX1)
                     PERFORM  VARYING  IDX2  FROM  1  BY  1
                       UNTIL  IDX2 >  LNK-RYOCOM-MEISAISU(IDX1)
                         MOVE  LNK-RYOCOM-IRYO-SRYCD(IDX1 IDX2)
                                     TO  WRK-IRYOADL-SRYCD(IDX1 IDX2)
                     END-PERFORM
                     MOVE   LNK-RYOCOM-SRYCDTOTAL2(IDX1)
                                   TO  WRK-IRYOADL-SRYCDTOTAL(IDX1)
                     MOVE   LNK-RYOCOM-ADL-SRYCD(IDX1)
                                   TO  WRK-IRYOADL-ADL-SRYCD(IDX1)
                     MOVE   LNK-RYOCOM-ADL-TEN(IDX1)
                                   TO  WRK-IRYOADL-TEN(IDX1)
                     MOVE   LNK-RYOCOM-ADL-TEN-MOJI(IDX1)
                                   TO  WRK-IRYOADL-TEN-MOJI(IDX1)
                 END-IF
             END-PERFORM
      *
      *      ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¤Þ¤È¤á
             PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF   (WRK-IRYOADL-DAY(IDX1)  NOT =   ZERO)   AND
                    (WRK-IRYOADL-MEISAISU(IDX1)  NOT =   ZERO)
                   MOVE       1   TO  WRK-IRYOADL-GAITOU-KBN(IDX1 IDX1)
                   MOVE       ZERO      TO  IDX2
                   COMPUTE    IDX2      =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-IRYOADL-DAY(IDX3)  =
                                         WRK-IRYOADL-DAY(IDX1)) AND
                             (WRK-IRYOADL-MEISAISU(IDX3)  =
                                         WRK-IRYOADL-MEISAISU(IDX1)) AND
                             (WRK-IRYOADL-SRYCDTOTAL(IDX3)  =
                                       WRK-IRYOADL-SRYCDTOTAL(IDX1)) AND
                             (WRK-IRYOADL-TEN(IDX3)  =
                                         WRK-IRYOADL-TEN(IDX1))
                             MOVE      1    TO
                                       WRK-IRYOADL-GAITOU-KBN(IDX1 IDX3)
                             INITIALIZE   WRK-IRYOADL-SORT-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
             END-PERFORM
           END-IF
      *    ¤Þ¤È¤á·ë²Ì³ÎÇ§
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-IRYOADL-DAY(IDX1) = "
      *                    WRK-IRYOADL-DAY(IDX1)
      *          DISPLAY  "WRK-IRYOADL-MEISAISU(IDX1) = "
      *                    WRK-IRYOADL-MEISAISU(IDX1)
      *          DISPLAY  "WRK-IRYOADL-SRYCDTOTAL(IDX1) = "
      *                    WRK-IRYOADL-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-IRYOADL-TEN(IDX1) = "
      *                    WRK-IRYOADL-TEN(IDX1)
      *          DISPLAY  "WRK-IRYOADL-GAITOU-TBL(IDX1) = "
      *                    WRK-IRYOADL-GAITOU-TBL(IDX1)
      *     END-PERFORM
      *
      *    Æþ±¡²ñ·×¤Þ¤È¤á·ë²Ì¤ÈÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¤Î¥ê¥ó¥¯¤ò³ÎÇ§
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-IRYOADL-DAY(IDX1) = "
      *                    WRK-IRYOADL-DAY(IDX1)
      *          DISPLAY  "WRK-SO-IRYOADL-DAY(IDX1) = "
      *                    WRK-SO-IRYOADL-DAY(IDX1)
      *     END-PERFORM
      *
      *   ·ë²ÌÉ½¼¨
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-SO-HKNCOMBI-DAY = " WRK-SO-HKNCOMBI-DAY
      *                                                         (IDX1)
      *          DISPLAY  "WRK-SO-SRYKBN   = " WRK-SO-SRYKBN(IDX1)
      *          DISPLAY  "WRK-SO-MEISAISU = " WRK-SO-MEISAISU(IDX1)
      *          DISPLAY  "WRK-SO-SRYCD(1) = " WRK-SO-SRYCD(IDX1 1)
      *          DISPLAY  "WRK-SO-SRYCD(2) = " WRK-SO-SRYCD(IDX1 2)
      *          DISPLAY  "WRK-SO-SRYCD(3) = " WRK-SO-SRYCD(IDX1 3)
      *          DISPLAY  "WRK-SO-SRYCD(4) = " WRK-SO-SRYCD(IDX1 4)
      *          DISPLAY  "WRK-SO-SRYCD(5) = " WRK-SO-SRYCD(IDX1 5)
      *          DISPLAY  "WRK-SO-SRYCDTOTAL = " WRK-SO-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-SO-TEN      = " WRK-SO-TEN(IDX1)
      *          DISPLAY  "WRK-SO-KAISU    = " WRK-SO-KAISU(IDX1)
      *          PERFORM   VARYING  IDX2  FROM  1  BY  1
      *                    UNTIL  IDX2  >  31
      *                     DISPLAY  "IDX2 = " IDX2
      *                     DISPLAY  "WRK-SO-GAIHAKU-KBN = " 
      *                               WRK-SO-GAIHAKU-KBN(IDX1 IDX2)
      *          END-PERFORM
      *     END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È¡ÊÆþ±¡ÎÁ¡¡¿ÇÎÅ¶èÊ¬£¹£°¡Ë
           MOVE       ZERO      TO     IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)   AND
                    (WRK-SO-SRYKBN(IDX1)    =       "90")
                     ADD    1      TO    IDX2
                     MOVE  WRK-SO-HKNCOMBI-DAY(IDX1) TO
                                     LNK-NYURECEDEN-HKNCOMBI(1 IDX2)
                     MOVE  WRK-SO-MEISAISU(IDX1)   TO
                                     LNK-NYURECEDEN-MEISAISU(1 IDX2)
                     PERFORM   VARYING   IDX3   FROM   1   BY  1
                               UNTIL (WRK-SO-SRYCD(IDX1 IDX3) = SPACE)
                                  OR (IDX3   >   20)
                         MOVE   "SI"   TO
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                         IF    IDX3   =    1
                               MOVE   WRK-SO-SRYKBN(IDX1)  TO   
                                    LNK-NYURECEDEN-SRYSKB(1 IDX2 IDX3)
                         END-IF
                         MOVE  WRK-SO-SRYCD(IDX1 IDX3) TO
                                    LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                         MOVE  ZERO     TO    IDX4
                         COMPUTE   IDX4 =    IDX3   +   1
                         IF    WRK-SO-SRYCD(IDX1 IDX4)    =   SPACE
                               MOVE   WRK-SO-TEN(IDX1)   TO
                                      LNK-NYURECEDEN-TEN(1 IDX2 IDX3)
                               MOVE   WRK-SO-KAISU(IDX1)   TO
                                     LNK-NYURECEDEN-KAISU(1 IDX2 IDX3)
                         END-IF
                     END-PERFORM
      *
      *              ³°Çñ¥³¥á¥ó¥È¥»¥Ã¥È½èÍý
                     INITIALIZE    WRK-GAIHAKU-DAY-TABLE
                     IF   WRK-SO-GAIHAKU-TBL(IDX1)   =  ALL"0"
                         CONTINUE
                     ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-SO-GAIHAKU-KBN(IDX1 IDX5)  =
                                                         1   OR  2
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-SO-GAIHAKU-KBN(IDX1 IDX7) =
                                                         1   OR  2
                                        MOVE  IDX5  TO
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
                       PERFORM   VARYING  IDX5  FROM  1  BY  1
                       UNTIL   (IDX5  >  31)   OR
                               (WRK-GAIHAKU-DAY-ST(IDX5) =  ZERO)
                           ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           IF   WRK-GAIHAKU-DAY-ST(IDX5) = 
                                               WRK-GAIHAKU-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "840000015"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "840000016"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-GAIHAKU-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (5:4)
                           END-IF
                           ADD    1     TO   IDX3
                       END-PERFORM
                     END-IF
      *
                     IF  LNK-RYOCOM-RC  =       ZERO
      *              ÎÅÍÜ¥ì¥»¥×¥È¥³¥á¥ó¥È¥»¥Ã¥È½èÍý
      *              ·îÆü¥»¥Ã¥È
                     PERFORM   VARYING  CIDX   FROM   1  BY  1
                       UNTIL   CIDX     >   31
                      IF   WRK-SO-IRYOADL-DAY(IDX1)  =
                                         WRK-IRYOADL-DAY(CIDX)
                       INITIALIZE    WRK-RYOYO-DAY-TABLE
                       IF   WRK-IRYOADL-GAITOU-TBL(CIDX)   =  ALL"0"
                         CONTINUE
                       ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-IRYOADL-GAITOU-KBN(CIDX IDX5)
                                                          =       1
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-RYOYO-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-RYOYO-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-IRYOADL-GAITOU-KBN(CIDX IDX7)
                                                           =     1
                                        MOVE  IDX5  TO
                                              WRK-RYOYO-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-RYOYO-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-RYOYO-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
                         PERFORM   VARYING  IDX5  FROM  1  BY  1
                         UNTIL   (IDX5  >  31)   OR
                               (WRK-RYOYO-DAY-ST(IDX5) =  ZERO)
                           IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 100
                             ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(1 IDX2)
                             IF   WRK-RYOYO-DAY-ST(IDX5) = 
                                               WRK-RYOYO-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "810000001"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   "¡¡¡¡·î¡¡¡¡Æü"  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                MOVE   LNK-NYURECEDEN-SANTEIYM(5:2)
                                                       TO  WRK-MM-X
                                MOVE   WRK-MM-9        TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (7:4)
                             ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "810000001"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   "¡¡¡¡·î¡¡¡¡Æü¡Á¡¡¡¡·î¡¡¡¡Æü"  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                MOVE   LNK-NYURECEDEN-SANTEIYM(5:2)
                                                       TO  WRK-MM-X
                                MOVE   WRK-MM-9        TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (15:4)
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (7:4)
                                MOVE   WRK-RYOYO-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (21:4)
                             END-IF
                             ADD    1     TO   IDX3
                           END-IF
                         END-PERFORM
      *
      *                  °åÎÅ¶èÊ¬¥³¥á¥ó¥È
                         PERFORM   VARYING  IDX5  FROM  1  BY  1
                         UNTIL   (IDX5  >  30)   OR
                           (WRK-IRYOADL-SRYCD(CIDX IDX5) =  SPACE)
                           IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 100
                             ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                             MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                             MOVE  WRK-IRYOADL-SRYCD(CIDX IDX5)  TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                             ADD    1     TO   IDX3
                           END-IF
                         END-PERFORM
      *
      *                  £Á£Ä£Ì¥³¥á¥ó¥È
                         IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 100
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000103"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN(CIDX)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN(CIDX) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                         END-IF
      *
                       END-IF
                      END-IF
                     END-PERFORM
      *
                     END-IF
      *
               END-IF
           END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È¡ÊÆÃÄêÆþ±¡ÎÁ¡¡¿ÇÎÅ¶èÊ¬£¹£²¡Ë
           MOVE      ZERO     TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)   AND
                    (WRK-SO-SRYKBN(IDX1)    =       "92")
                     ADD    1      TO    IDX2
                     MOVE  WRK-SO-HKNCOMBI-DAY(IDX1) TO
                                     LNK-NYURECEDEN-HKNCOMBI(2 IDX2)
                     MOVE  WRK-SO-MEISAISU(IDX1)   TO
                                     LNK-NYURECEDEN-MEISAISU(2 IDX2)
                     PERFORM   VARYING   IDX3   FROM   1   BY  1
                               UNTIL (WRK-SO-SRYCD(IDX1 IDX3) = SPACE)
                                  OR (IDX3   >   20)
                         MOVE   "SI"   TO   
                                    LNK-NYURECEDEN-RECSKB(2 IDX2 IDX3)
                         IF    IDX3   =    1
                               MOVE   WRK-SO-SRYKBN(IDX1)  TO   
                                    LNK-NYURECEDEN-SRYSKB(2 IDX2 IDX3)
                         END-IF
                         MOVE  WRK-SO-SRYCD(IDX1 IDX3) TO
                                    LNK-NYURECEDEN-SRYCD(2 IDX2 IDX3)
                         MOVE  ZERO     TO    IDX4
                         COMPUTE   IDX4 =    IDX3   +   1
                         IF    WRK-SO-SRYCD(IDX1 IDX4)    =    SPACE
                               MOVE   WRK-SO-TEN(IDX1)   TO
                                      LNK-NYURECEDEN-TEN(2 IDX2 IDX3)
                               MOVE   WRK-SO-KAISU(IDX1)   TO
                                     LNK-NYURECEDEN-KAISU(2 IDX2 IDX3)
                         END-IF
                     END-PERFORM
      *              ³°Çñ¥³¥á¥ó¥È¥»¥Ã¥È½èÍý
                     INITIALIZE    WRK-GAIHAKU-DAY-TABLE
                     IF   WRK-SO-GAIHAKU-TBL(IDX1)   =  ALL"0"
                         CONTINUE
                     ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-SO-GAIHAKU-KBN(IDX1 IDX5)  =
                                                         1   OR  2
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-SO-GAIHAKU-KBN(IDX1 IDX7) =
                                                         1   OR  2
                                        MOVE  IDX5  TO
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
                       PERFORM   VARYING  IDX5  FROM  1  BY  1
                       UNTIL   (IDX5  >  31)   OR
                               (WRK-GAIHAKU-DAY-ST(IDX5) =  ZERO)
                           ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(2 IDX2)
                           IF   WRK-GAIHAKU-DAY-ST(IDX5) = 
                                               WRK-GAIHAKU-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(2 IDX2 IDX3)
                                MOVE  "840000015"  TO
                                      LNK-NYURECEDEN-SRYCD(2 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(2 IDX2 IDX3)
                           ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(2 IDX2 IDX3)
                                MOVE  "840000016"  TO
                                      LNK-NYURECEDEN-SRYCD(2 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(2 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-GAIHAKU-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(2 IDX2 IDX3)
                                                          (5:4)
                           END-IF
                           ADD    1     TO   IDX3
                       END-PERFORM
                     END-IF
                END-IF
           END-PERFORM
      *
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸
           IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
               PERFORM   2101-SHOKUJI-HENSHU1-SEC
           ELSE
               PERFORM   2103-SHOKUJI-HENSHU3-SEC
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¤ÎÊÔ½¸
           IF  (LNK-NYURECEDEN-SANTEIYM  >=  "200610")  AND
               (LNK-NYULIFE-RC           =    ZERO)
               PERFORM   2104-KANKYO-HENSHU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¼èÆÀ
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID        TO  NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
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
           INITIALIZE                      TNS-TENSU-REC
           MOVE    NSRY-SRYCD(1)       TO  TNS-SRYCD
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-NYUKHNKSNKBN    TO  WRK-KSNKBN
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
           MOVE   NACCT-DAY(IDX1)     TO    WRK-GAIHAKU-KBN(IDX1)
      *
           EVALUATE   WRK-SRYKBN(IDX1)
              WHEN     "90"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107290"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107390"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                      PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                 END-EVALUATE
              WHEN     "92"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107490"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107590"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                      PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                 END-EVALUATE
           END-EVALUATE
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
               WHEN   201  THRU  219
               WHEN   221  THRU  222
               WHEN   224
               WHEN   227  THRU  228
               WHEN   230  THRU  233
                  MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
               WHEN   013  THRU  021
               WHEN   101  THRU  109
               WHEN   147  THRU  152
                  MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
               WHEN   075  THRU  080
                  MOVE   "190108990"    TO   WRK-GAIHAKU(IDX1)
               WHEN   085  THRU  086
               WHEN   132  THRU  133
               WHEN   153  THRU  158
                  MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
      *
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
      *****************************************************************
       210-RJN-TAISYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-RJN
      *
      *    Æþ±¡´µ¼Ô¤ÎÏ·¿ÍÊÝ¸±È½Äê
           MOVE    ZERO                TO   FLG-HKNCOMBI
           INITIALIZE                       HKNCOMBI-REC
           MOVE    LNK-NYURECEDEN-HOSPID TO  COMB-HOSPID
           MOVE    LNK-NYURECEDEN-PTID  TO   COMB-PTID
           PERFORM   VARYING   IDX   FROM  1  BY  1
                     UNTIL     (WRK-HKNCOMBI-DAY(IDX)  NOT =  ZERO) OR
                               (IDX     >   31)
               IF   WRK-HKNCOMBI-DAY(IDX)   NOT =   ZERO
                  MOVE    WRK-HKNCOMBI-DAY(IDX)
                                       TO   COMB-HKNCOMBINUM
               END-IF
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI        =    ZERO
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    Æþ±¡´µ¼Ô¤ÎÇ¯Îð·×»»
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    LNK-NYURECEDEN-HOSPID
                                       TO  AGECHK-HOSPID
           MOVE    LNK-NYURECEDEN-PTID
                                       TO  AGECHK-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
           IF    ( AGECHK-RC           =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
      *    Ï·¿ÍÂÐ¾Ý´µ¼ÔÈ½Äê
           IF    ((FLG-HKNCOMBI        =   ZERO)    AND
                  (COMB-KOH1HKNNUM     =   "027"))   OR
                 ( AGECHK-NENREI      >=    75)      OR
                 ((AGECHK-NENREI      >=    70)     AND
                  (PTINF-BIRTHDAY     <=   "19320930"))
               MOVE    1               TO  FLG-RJN
           END-IF
      *
           .
       210-RJN-TAISYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô·×»»½èÍý
      *****************************************************************
       210-TENSU-KEISAN-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           IF     (IDX2                   =     1  )
           AND    (WRK-GAIHAKU(IDX1)   NOT =   SPACE)
               PERFORM    2101-GAIHAKU-NYUINKHN-SET-SEC
               IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
                  MOVE    WRK-GAIHAKU-SRYCD   TO  TNS-SRYCD
               ELSE
                  MOVE    WRK-SO-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
               END-IF
           ELSE
                  MOVE    WRK-SO-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
           END-IF
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD           TO  TNS-YUKOEDYMD
           MOVE    TNS-SRYCD    TO   WRK-TENSU-CD
           PERFORM 910-TENSUWRK-CHK-SEC
           IF      FLG-TENSUWRK   =    ZERO
               PERFORM 910-TENSU-READ-SEC
               PERFORM 910-TTNS-HEN-SEC
           END-IF
      *
      *    ÅÀ¿ô»»Äê
           EVALUATE    TNS-TENSIKIBETU
      *        ÅÀ¿ô¡Ê¥×¥é¥¹¡Ë
               WHEN    3
                   ADD   TNS-TEN     TO  WRK-SO-TEN(IDX1)
      *        ¡ó²Ã»»
               WHEN    5
                   COMPUTE    WRK-SO-TEN(IDX1)  =
                              WRK-SO-TEN(IDX1)  +
                            ((WRK-SO-TEN(IDX1)  *
                              (TNS-TEN   /  100))  + 0.5)
      *        ¡ó¸º»»
               WHEN    6
                   COMPUTE    WRK-SO-TEN(IDX1)  =
                            ( WRK-SO-TEN(IDX1)  -
                             (WRK-SO-TEN(IDX1)  *
                             (TNS-TEN   /  100)))        +  0.5
      *        ÅÀ¿ô¡Ê¥Þ¥¤¥Ê¥¹¡Ë
               WHEN    8
                   COMPUTE  WRK-SO-TEN(IDX1)  =
                            WRK-SO-TEN(IDX1)  -  TNS-TEN
      *        WHEN  OTHER
      *            ADD   TNS-TEN     TO  WRK-SO-TEN(IDX1)
           END-EVALUATE
      *
      *    ¿ÇÎÅ¥³¡¼¥É·×
           MOVE    TNS-SRYCD          TO  WRK-SRYCD-X
           COMPUTE WRK-SO-SRYCDTOTAL(IDX1)  =
                   WRK-SO-SRYCDTOTAL(IDX1)  +   WRK-SRYCD-9
           ADD      1          TO  WRK-SO-MEISAISU(IDX1)
      *
           .
       210-TENSU-KEISAN-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ
      **************************************************************
       2101-GAIHAKU-NYUINKHN-SET-SEC           SECTION.
      *
           MOVE    SPACE    TO   WRK-GAIHAKU-SRYCD
      *
           EVALUATE  WRK-SO-SRYCD(IDX1 IDX2)
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
           IF   (LNK-NYURECEDEN-SANTEIYM  >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    WRK-GAIHAKU-SRYCD   NOT =   SPACE
               MOVE   WRK-GAIHAKU-SRYCD   TO   WRK-SO-SRYCD(IDX1 IDX2)
           END-IF
      *
           .
       2101-GAIHAKU-NYUINKHN-SET-EXT.
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
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1     TO        WRK-SYS5000-COUNT
                   MOVE    "tbl_syskanri"    TO  MCP-TABLE
                   MOVE    "key11"           TO  MCP-PATHNAME
                   PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
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
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
      *          ¿©»ö¤ÎÆÃÊÌ´ÉÍý²Ã»»ÂàÈò
                 IF     SYS-5000-SHOKUCD(1)  NOT =  SPACE
                    IF  SYS-5000-SHOKUKBN(1) =     "2"
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                            MOVE  SYS-5000-SHOKUCD(1)
                                  TO WRK-SYS5000-SHOKUCD(2 GENSAN-IDX2)
                        END-PERFORM
                    ELSE
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                            MOVE  SPACE
                                  TO WRK-SYS5000-SHOKUCD(2 GENSAN-IDX2)
                        END-PERFORM
                    END-IF
                 ELSE
                    PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                              UNTIL     GENSAN-IDX2   >   31
                        MOVE SPACE TO WRK-SYS5000-SHOKUCD(2 GENSAN-IDX2)
                    END-PERFORM
                 END-IF
                 PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                            UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              IF   FLG-RJN    =    ZERO
                                  MOVE  "190080570"  TO  WRK-GENSAN2
                              ELSE
                                  MOVE  "190799770"  TO  WRK-GENSAN2
                              END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-INNAI(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  3
                              IF   FLG-RJN    =    ZERO
                                  MOVE  "190111690"  TO  WRK-GENSAN3
                              ELSE
                                  MOVE  "190809090"  TO  WRK-GENSAN3
                              END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-IRYO(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  4
                              IF   FLG-RJN    =    ZERO
                                  MOVE  "190111790"  TO  WRK-GENSAN4
                              ELSE
                                  MOVE  "190809190"  TO  WRK-GENSAN4
                              END-IF
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
                 MOVE    "tbl_syskanri"    TO  MCP-TABLE
                 MOVE    "key11"           TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       210-SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸¡ÊÊ¿À®£±£¸Ç¯£³·îËø¡Ë
      *****************************************************************
       2101-SHOKUJI-HENSHU1-SEC        SECTION.
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key28"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                         (WRK-NYUINKHN(IDX1) NOT =  SPACE)    AND
                         (WRK-GAIHAKU-KBN(IDX1)  =  ZERO  OR  4)
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ-HKNCOMBI-DAY(IDX1)
                                           WRK-SKJ2-HKNCOMBI-DAY(IDX1)
                                           WRK-SKJ3-HKNCOMBI-DAY(IDX1)
                                         WRK-SKJFTN-HKNCOMBI-DAY(IDX1)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN-SRYCD(IDX1)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                         TO  WRK-SKJ3-TOKU-SHOKU(IDX1)
                               END-IF
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN-SRYCD(IDX1)
                               END-IF
                           WHEN   3
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000670"
                                          TO  WRK-SKJ2-FUKUSU(IDX1)
                               END-IF
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN-SRYCD(IDX1)
                               END-IF
                           WHEN   4
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                         TO  WRK-SKJ3-TOKU-SHOKU(IDX1)
                                   MOVE   "197000670"
                                          TO  WRK-SKJ2-FUKUSU(IDX1)
                               END-IF
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN-SRYCD(IDX1)
                               END-IF
                       END-EVALUATE
      *                ÆÃÊÌ´ÉÍý²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(2 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(2 IDX1)
                                          TO  WRK-SKJ-TOKU-KANRI(IDX1)
                           ADD   200          TO  WRK-SKJ-MONEY(IDX1)
                       END-IF
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ-SHOKUDOU(IDX1)
                           ADD    50          TO  WRK-SKJ-MONEY(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *Äã½êÆÀ¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TSYRRK-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  TSYRRK-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  TSYRRK-PTID
           MOVE    "tbl_tsyrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TSYRRK-SELECT-SEC
           IF      FLG-TSYRRK              =   ZERO
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TSYRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TSYRRK          =   1                ) OR
                 (( TSYRRK-NINSTYMD(1:6) <= LNK-NYURECEDEN-SANTEIYM AND
                    LNK-NYURECEDEN-SANTEIYM <= TSYRRK-NINEDYMD(1:6) ))
      *
                   MOVE    "tbl_tsyrrk"     TO  MCP-TABLE
                   MOVE    "key3"           TO  MCP-PATHNAME
                   PERFORM 910-TSYRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TSYRRK              =   ZERO
              IF  (LNK-NYURECEDEN-SANTEIYM   =  TSYRRK-NINSTYMD(1:6))
              AND (TSYRRK-NINSTYMD(7:2)      NOT =  "01")
                CONTINUE
              ELSE
                PERFORM   VARYING   IDX1   FROM   1  BY  1
                          UNTIL     IDX1   >    31
                    IF    WRK-SKJ-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197000910"
                                        TO  WRK-SKJFTN-SRYCD(IDX1)
                    END-IF
                END-PERFORM
              END-IF
           END-IF
      *
           MOVE     "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE     "key3"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *£¹£°Æü±Û¤¨¡ÊÄã½êÆÀ¾ðÊó¡Ë
           IF  (TSYRRK-SKJGNSTYMD(1:6)  <= LNK-NYURECEDEN-SANTEIYM) AND
               (TSYRRK-SKJGNSTYMD       NOT = SPACE)
             IF (LNK-NYURECEDEN-SANTEIYM  = TSYRRK-SKJGNSTYMD(1:6)) AND
                (TSYRRK-SKJGNSTYMD(7:2)   NOT = "01")
               CONTINUE
             ELSE
               IF  TSYRRK-SKJGNSTYMD(1:6)  <  LNK-NYURECEDEN-SANTEIYM
                   MOVE   1             TO   TSY90-IDX
               ELSE
                   MOVE   TSYRRK-SKJGNSTYMD(7:2)   TO  TSY90-IDX
               END-IF
               PERFORM   VARYING   IDX1   FROM   TSY90-IDX  BY  1
                          UNTIL     IDX1   >    31
                    IF    WRK-SKJ-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN-SRYCD(IDX1)
                    END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *ÄãÇ¯¶â¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TNKRRK-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  TNKRRK-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  TNKRRK-PTID
           MOVE    "tbl_tnkrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TNKRRK-SELECT-SEC
           IF      FLG-TNKRRK              =   ZERO
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TNKRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TNKRRK          =   1                ) OR
                 (( TNKRRK-STYMD(1:6) <= LNK-NYURECEDEN-SANTEIYM AND
                    LNK-NYURECEDEN-SANTEIYM <= TNKRRK-EDYMD(1:6) ))
      *
                   MOVE    "tbl_tnkrrk"       TO  MCP-TABLE
                   MOVE    "key3"             TO  MCP-PATHNAME
                   PERFORM 910-TNKRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TNKRRK              =   ZERO
              IF  (LNK-NYURECEDEN-SANTEIYM   =  TNKRRK-STYMD(1:6))
              AND (TNKRRK-STYMD(7:2)     NOT =  "01")
                CONTINUE
              ELSE
                PERFORM   VARYING   IDX1   FROM   1  BY  1
                          UNTIL     IDX1   >    31
                    IF    WRK-SKJ-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001110"
                                        TO  WRK-SKJFTN-SRYCD(IDX1)
                    END-IF
                END-PERFORM
              END-IF
           END-IF
      *
           MOVE     "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE     "key3"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    ¿©»ö¤Î¿ÇÎÅ¥³¡¼¥É·×
           PERFORM    VARYING   IDX1    FROM  1  BY  1
                      UNTIL     IDX1    >   31
               PERFORM   VARYING  IDX2  FROM  1  BY  1
                         UNTIL    IDX2    >   3
                   IF   WRK-SKJ-SRYCD(IDX1 IDX2)  NOT =  SPACE
                       MOVE    WRK-SKJ-SRYCD(IDX1 IDX2) TO WRK-SRYCD-X
                       COMPUTE WRK-SKJ-SRYCDTOTAL(IDX1)  =
                               WRK-SKJ-SRYCDTOTAL(IDX1)  + WRK-SRYCD-9
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key28"                     TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    ºÆ¹½ÃÛ¤·¤¿¿©»ö¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
      *    ¿©»ö¥ï¡¼¥¯
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-SKJ-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ-HKNCOMBI-DAY(IDX3)  =
                                       WRK-SKJ-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ-SRYCDTOTAL(IDX3)  =
                                       WRK-SKJ-SRYCDTOTAL(IDX1))
                             ADD     1    TO    WRK-SKJ-KAISU(IDX1)
                             INITIALIZE   WRK-SKJ-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *    ¿©»ö¥ï¡¼¥¯£²¡ÊÊ£¿ô¸¥Î©²Ã»»¡Ë
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ2-FUKUSU(IDX1)  NOT =   SPACE
                   ADD   1                  TO  WRK-SKJ2-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ2-HKNCOMBI-DAY(IDX3)  =
                                      WRK-SKJ2-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ2-FUKUSU(IDX3)  =
                                       WRK-SKJ2-FUKUSU(IDX1))
                             ADD     1    TO    WRK-SKJ2-KAISU(IDX1)
                             INITIALIZE   WRK-SKJ2-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *    ¿©»ö¥ï¡¼¥¯£³¡ÊÆÃÊÌ¿©²Ã»»¡Ë
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ3-TOKU-SHOKU(IDX1)  NOT =   SPACE
                   ADD   1                  TO  WRK-SKJ3-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ3-HKNCOMBI-DAY(IDX3)  =
                                      WRK-SKJ3-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ3-TOKU-SHOKU(IDX3)  =
                                       WRK-SKJ3-TOKU-SHOKU(IDX1))
                             ADD     1    TO    WRK-SKJ3-KAISU(IDX1)
                             INITIALIZE   WRK-SKJ3-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *    ¿©»öÉéÃ´¥ï¡¼¥¯
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJFTN-SRYCD(IDX1)  NOT =   SPACE
                   ADD   1                  TO  WRK-SKJFTN-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJFTN-HKNCOMBI-DAY(IDX3)  =
                                    WRK-SKJFTN-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJFTN-SRYCD(IDX3)  =
                                       WRK-SKJFTN-SRYCD(IDX1))
                             ADD     1    TO    WRK-SKJFTN-KAISU(IDX1)
                             INITIALIZE   WRK-SKJFTN-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *   ·ë²ÌÉ½¼¨
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     31)
      **          DISPLAY  "IDX1 = " IDX1
      **          DISPLAY  "WRK-SKJ-HKNCOMBI-DAY = " 
      **                    WRK-SKJ-HKNCOMBI-DAY(IDX1)
      **          DISPLAY  "WRK-SKJ-SRYCD(1) = " WRK-SKJ-SRYCD(IDX1 1)
      **          DISPLAY  "WRK-SKJ-SRYCD(2) = " WRK-SKJ-SRYCD(IDX1 2)
      **          DISPLAY  "WRK-SKJ-SRYCD(3) = " WRK-SKJ-SRYCD(IDX1 3)
      **          DISPLAY  "WRK-SKJ-SRYCDTOTAL = "
      **                                      WRK-SKJ-SRYCDTOTAL(IDX1)
      **          DISPLAY  "WRK-SKJ-MONEY    = " WRK-SKJ-MONEY(IDX1)
      **          DISPLAY  "WRK-SKJ-KAISU    = " WRK-SKJ-KAISU(IDX1)
      **     END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJ-SRYCD(IDX1 1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    PERFORM    VARYING    IDX3   FROM  1   BY  1
                               UNTIL      IDX3   >    3
                        IF   WRK-SKJ-SRYCD(IDX1 IDX3)  NOT =  SPACE
                            ADD     1     TO    IDX4
                            MOVE   "SI"   TO
                                  LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                            IF     IDX4    =   1
                                MOVE   "97"   TO
                                  LNK-NYURECEDEN-SRYSKB(3 IDX2 IDX4)
                            END-IF
                            MOVE   WRK-SKJ-SRYCD(IDX1 IDX3)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        END-IF
                    END-PERFORM
                    MOVE  WRK-SKJ-MONEY(IDX1)  TO
                                LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                    MOVE  WRK-SKJ-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                END-IF
           END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡ÊÆÃÊÌ¿©²Ã»»¡Ë
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJ3-TOKU-SHOKU(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ3-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   "SI"   TO
                                  LNK-NYURECEDEN-RECSKB(3 IDX2 1)
                    MOVE   SPACE   TO
                                  LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
                    MOVE   WRK-SKJ3-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 1)
                    MOVE  350  TO
                                LNK-NYURECEDEN-TEN(3 IDX2 1)
                    MOVE  WRK-SKJ3-KAISU(IDX1)  TO
                                  LNK-NYURECEDEN-KAISU(3 IDX2 1)
                    MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                END-IF
           END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡ÊÊ£¿ô¸¥Î©²Ã»»¡Ë
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJ2-FUKUSU(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ2-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   "SI"   TO
                                  LNK-NYURECEDEN-RECSKB(3 IDX2 1)
                    MOVE   SPACE   TO
                                  LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
                    MOVE   WRK-SKJ2-FUKUSU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 1)
                    MOVE   50  TO
                                LNK-NYURECEDEN-TEN(3 IDX2 1)
                    MOVE  WRK-SKJ2-KAISU(IDX1)  TO
                                  LNK-NYURECEDEN-KAISU(3 IDX2 1)
                    MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                END-IF
           END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡Ê¿©»öÉéÃ´¡Ë
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJFTN-SRYCD(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJFTN-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   "SI"   TO
                                  LNK-NYURECEDEN-RECSKB(3 IDX2 1)
                    MOVE   "97"   TO
                                  LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
                    MOVE   WRK-SKJFTN-SRYCD(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 1)
                    EVALUATE   WRK-SKJFTN-SRYCD(IDX1)
                        WHEN   "197000810"
                            MOVE  780  TO 
                                        LNK-NYURECEDEN-TEN(3 IDX2 1)
                        WHEN   "197000910"
                            MOVE  650  TO 
                                        LNK-NYURECEDEN-TEN(3 IDX2 1)
                        WHEN   "197001010"
                            MOVE  500  TO 
                                        LNK-NYURECEDEN-TEN(3 IDX2 1)
                        WHEN   "197001110"
                            MOVE  300  TO 
                                        LNK-NYURECEDEN-TEN(3 IDX2 1)
                    END-EVALUATE
                    MOVE  WRK-SKJFTN-KAISU(IDX1)  TO
                                  LNK-NYURECEDEN-KAISU(3 IDX2 1)
                    MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                END-IF
           END-PERFORM
      *
           .
       2101-SHOKUJI-HENSHU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
      *****************************************************************
       2102-SHOKUJI-HENSHU2-SEC        SECTION.
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÄ«¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key30"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                         (WRK-NYUINKHN(IDX1) NOT =  SPACE)    AND
                         (WRK-GAIHAKU-KBN(IDX1)  =  ZERO  OR  4)
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX1)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX1)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key30"                     TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÃë¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key31"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key31"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX2   =    IDX1   +   31
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                         (WRK-NYUINKHN(IDX1) NOT =  SPACE)    AND
                         (WRK-GAIHAKU-KBN(IDX1)  =  ZERO  OR  4)
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX2)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX2)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX2)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
                       END-EVALUATE
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key31"                     TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÍ¼¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key32"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX3   =    IDX1   +   62
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                         (WRK-NYUINKHN(IDX1) NOT =  SPACE)    AND
                         (WRK-GAIHAKU-KBN(IDX1)  =  ZERO  OR  4)
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX3)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX3)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                            TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX3)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
                       END-EVALUATE
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key32"                     TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Äã½êÆÀ¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TSYRRK-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  TSYRRK-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  TSYRRK-PTID
           MOVE    "tbl_tsyrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TSYRRK-SELECT-SEC
           IF      FLG-TSYRRK              =   ZERO
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TSYRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TSYRRK          =   1                ) OR
                 (( TSYRRK-NINSTYMD(1:6) <= LNK-NYURECEDEN-SANTEIYM AND
                    LNK-NYURECEDEN-SANTEIYM <= TSYRRK-NINEDYMD(1:6) ))
      *
                   MOVE    "tbl_tsyrrk"     TO  MCP-TABLE
                   MOVE    "key3"           TO  MCP-PATHNAME
                   PERFORM 910-TSYRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TSYRRK              =   ZERO
              IF  (LNK-NYURECEDEN-SANTEIYM   =  TSYRRK-NINSTYMD(1:6))
              AND (TSYRRK-NINSTYMD(7:2)      NOT =  "01")
                CONTINUE
              ELSE
                PERFORM   VARYING   IDX1   FROM   1  BY  1
                          UNTIL     IDX1   >    93
                    IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197000910"
                                        TO  WRK-SKJFTN2-SRYCD(IDX1)
                    END-IF
                END-PERFORM
              END-IF
           END-IF
      *
           MOVE     "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE     "key3"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *£¹£°Æü±Û¤¨¡ÊÄã½êÆÀ¾ðÊó¡Ë
           IF  (TSYRRK-SKJGNSTYMD(1:6)  <= LNK-NYURECEDEN-SANTEIYM) AND
               (TSYRRK-SKJGNSTYMD       NOT = SPACE)
             IF (LNK-NYURECEDEN-SANTEIYM  = TSYRRK-SKJGNSTYMD(1:6)) AND
                (TSYRRK-SKJGNSTYMD(7:2)   NOT = "01")
               CONTINUE
             ELSE
               IF  TSYRRK-SKJGNSTYMD(1:6)  <  LNK-NYURECEDEN-SANTEIYM
                   MOVE   1             TO   TSY90-IDX
               ELSE
                   MOVE   TSYRRK-SKJGNSTYMD(7:2)   TO  TSY90-IDX
               END-IF
               PERFORM   VARYING   IDX1   FROM   TSY90-IDX  BY  1
                          UNTIL     IDX1   >    31
                    IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX1)
                        COMPUTE   IDX-TSY90  =  IDX1  +  31
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX-TSY90)
                        COMPUTE   IDX-TSY90  =  IDX1  +  62
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX-TSY90)
                    END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *ÄãÇ¯¶â¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TNKRRK-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  TNKRRK-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  TNKRRK-PTID
           MOVE    "tbl_tnkrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TNKRRK-SELECT-SEC
           IF      FLG-TNKRRK              =   ZERO
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TNKRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TNKRRK          =   1                ) OR
                 (( TNKRRK-STYMD(1:6) <= LNK-NYURECEDEN-SANTEIYM AND
                    LNK-NYURECEDEN-SANTEIYM <= TNKRRK-EDYMD(1:6) ))
      *
                   MOVE    "tbl_tnkrrk"       TO  MCP-TABLE
                   MOVE    "key3"             TO  MCP-PATHNAME
                   PERFORM 910-TNKRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TNKRRK              =   ZERO
              IF  (LNK-NYURECEDEN-SANTEIYM   =  TNKRRK-STYMD(1:6))
              AND (TNKRRK-STYMD(7:2)     NOT =  "01")
                CONTINUE
              ELSE
                PERFORM   VARYING   IDX1   FROM   1  BY  1
                          UNTIL     IDX1   >    93
                    IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001110"
                                        TO  WRK-SKJFTN2-SRYCD(IDX1)
                    END-IF
                END-PERFORM
              END-IF
           END-IF
      *
           MOVE     "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE     "key3"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    ¿©»ö¥ï¡¼¥¯£·¤ÎÊÔ½¸¡ÊÆ±°ìºÞ¤Þ¤È¤áÍÑ¡Ë
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ¿©»öÎÅÍÜÈñ¡ÊÄ«¡Ë
               IF    WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÄ«¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX1)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÄ«¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»öÎÅÍÜÈñ¡ÊÃë¡Ë
               MOVE      ZERO          TO  IDX2
               COMPUTE   IDX2     =    IDX1    +   31
               IF    WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX2)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÃë¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX2)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÃë¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX2)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»öÎÅÍÜÈñ¡ÊÍ¼¡Ë
               MOVE      ZERO          TO  IDX3
               COMPUTE   IDX3     =    IDX1    +   62
               IF    WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX3)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÍ¼¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX3)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÍ¼¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX3)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»ö²ó¿ô
      **         IF    (WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE)
      **             MOVE      1         TO   WRK-SKJ7-KAISU(IDX1)
      **         END-IF
      *        ¿©Æ²²Ã»»
               IF    WRK-SKJ6-SHOKUDOU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ6-SHOKUDOU(IDX1)
                                       TO  WRK-SKJ7-SHOKUDOU(IDX1)
               END-IF
           END-PERFORM
      *
      *    ¿©»ö¤Î¿ÇÎÅ¥³¡¼¥É·×
           PERFORM    VARYING   IDX1    FROM  1  BY  1
                      UNTIL     IDX1    >   31
               IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-RYOYO(IDX1)       TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-SHOKUDOU(IDX1)    TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-TOKU-SHOKU(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SKJFTN(IDX1)      NOT =  SPACE
                   MOVE    WRK-SKJ7-SKJFTN(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               ADD   WRK-SKJ7-RYOYO-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               ADD   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               ADD   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
           END-PERFORM
      *
      *     PERFORM      VARYING    IDX1   FROM   1  BY  1
      *                  UNTIL      IDX1   >   31
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
      *     END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿¿©»ö¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
      *    ¿©»ö¥ï¡¼¥¯
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ7-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-SKJ7-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ7-HKNCOMBI-DAY(IDX3)  =
                                       WRK-SKJ7-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ7-SRYCDTOTAL(IDX3)  =
                                       WRK-SKJ7-SRYCDTOTAL(IDX1))
                             ADD     1    TO    WRK-SKJ7-KAISU(IDX1)
                             INITIALIZE   WRK-SKJ7-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
                IF    WRK-SKJ7-RYOYO(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(3 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                    END-IF
                    MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   50     TO
                                 LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         EVALUATE  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                             WHEN  1
                                 MOVE   76     TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                             WHEN  2
                                 MOVE   152    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                             WHEN  3
                                 MOVE   228    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         END-EVALUATE
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
                            WHEN   "197000810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
                        END-EVALUATE
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                END-IF
           END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
      **     MOVE       ZERO    TO   IDX2
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡Ê¿©»öÎÅÍÜ¡Ë
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     93)
      **          IF    WRK-SKJ4-RYOYO(IDX1)   NOT =  SPACE
      **              ADD    1     TO    IDX2
      **              MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1) TO
      **                              LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
      **              MOVE   "SI"   TO
      **                            LNK-NYURECEDEN-RECSKB(3 IDX2 1)
      **              MOVE   "97"   TO
      **                            LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
      **              MOVE   WRK-SKJ4-RYOYO(IDX1)  TO
      **                            LNK-NYURECEDEN-SRYCD(3 IDX2 1)
      **              EVALUATE  WRK-SKJ4-RYOYO(IDX1)
      **                 WHEN   "197000110"
      **                      MOVE  640  TO  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **                 WHEN   "197000710"
      **                      MOVE  506  TO  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **              END-EVALUATE
      **              MOVE  WRK-SKJ4-KAISU(IDX1)  TO
      **                            LNK-NYURECEDEN-KAISU(3 IDX2 1)
      **              MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
      **          END-IF
      **     END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡ÊÆÃÊÌ¿©²Ã»»¡Ë
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     93)
      **          IF    WRK-SKJ5-TOKU-SHOKU(IDX1)   NOT =  SPACE
      **              ADD    1     TO    IDX2
      **              MOVE  WRK-SKJ5-HKNCOMBI-DAY(IDX1) TO
      **                              LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
      **              MOVE   "SI"   TO
      **                            LNK-NYURECEDEN-RECSKB(3 IDX2 1)
      **              MOVE   SPACE   TO
      **                            LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
      **              MOVE   WRK-SKJ5-TOKU-SHOKU(IDX1)  TO
      **                            LNK-NYURECEDEN-SRYCD(3 IDX2 1)
      **              MOVE  76  TO
      **                          LNK-NYURECEDEN-TEN(3 IDX2 1)
      **              MOVE  WRK-SKJ5-KAISU(IDX1)  TO
      **                            LNK-NYURECEDEN-KAISU(3 IDX2 1)
      **              MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
      **          END-IF
      **     END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡Ê¿©Æ²²Ã»»¡Ë
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     31)
      **          IF    WRK-SKJ6-SHOKUDOU(IDX1)   NOT =  SPACE
      **              ADD    1     TO    IDX2
      **             MOVE  WRK-SKJ6-HKNCOMBI-DAY(IDX1) TO
      **                              LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
      **              MOVE   "SI"   TO
      **                            LNK-NYURECEDEN-RECSKB(3 IDX2 1)
      **              MOVE   SPACE   TO
      **                            LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
      **              MOVE   WRK-SKJ6-SHOKUDOU(IDX1)  TO
      **                            LNK-NYURECEDEN-SRYCD(3 IDX2 1)
      **              MOVE  50  TO
      **                          LNK-NYURECEDEN-TEN(3 IDX2 1)
      **              MOVE  WRK-SKJ6-KAISU(IDX1)  TO
      **                            LNK-NYURECEDEN-KAISU(3 IDX2 1)
      **              MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
      **          END-IF
      **     END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡Ê¿©»öÉéÃ´¡Ë
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     93)
      **          IF    WRK-SKJFTN2-SRYCD(IDX1)   NOT =  SPACE
      **              ADD    1     TO    IDX2
      **              MOVE  WRK-SKJFTN2-HKNCOMBI-DAY(IDX1) TO
      **                              LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
      **              MOVE   "SI"   TO
      **                            LNK-NYURECEDEN-RECSKB(3 IDX2 1)
      **              MOVE   "97"   TO
      **                            LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
      **              MOVE   WRK-SKJFTN2-SRYCD(IDX1)  TO
      **                            LNK-NYURECEDEN-SRYCD(3 IDX2 1)
      **              EVALUATE   WRK-SKJFTN2-SRYCD(IDX1)
      **                  WHEN   "197000810"
      **                      MOVE  260  TO 
      **                                  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **                  WHEN   "197000910"
      **                      MOVE  210  TO 
      **                                  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **                  WHEN   "197001010"
      **                      MOVE  160  TO 
      **                                  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **                  WHEN   "197001110"
      **                      MOVE  100  TO 
      **                                  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **              END-EVALUATE
      **              MOVE  WRK-SKJFTN2-KAISU(IDX1)  TO
      **                            LNK-NYURECEDEN-KAISU(3 IDX2 1)
      **              MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
      **          END-IF
      **     END-PERFORM
      *
           .
       2102-SHOKUJI-HENSHU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡¡¿©»öÎÅÍÜÈñ¿©¿ô¥Ù¡¼¥¹¡Ë
      *****************************************************************
       2103-SHOKUJI-HENSHU3-SEC        SECTION.
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÄ«¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key30"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                         (WRK-NYUINKHN(IDX1) NOT =  SPACE)    AND
                         (WRK-GAIHAKU-KBN(IDX1)  =  ZERO  OR  4)
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX1)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX1)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key30"                     TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÃë¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key31"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key31"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX2   =    IDX1   +   31
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                         (WRK-NYUINKHN(IDX1) NOT =  SPACE)    AND
                         (WRK-GAIHAKU-KBN(IDX1)  =  ZERO  OR  4)
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX2)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX2)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX2)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
                       END-EVALUATE
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key31"                     TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÍ¼¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  NACCT-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key32"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX3   =    IDX1   +   62
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                         (WRK-NYUINKHN(IDX1) NOT =  SPACE)    AND
                         (WRK-GAIHAKU-KBN(IDX1)  =  ZERO  OR  4)
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX3)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX3)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                            TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX3)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
                       END-EVALUATE
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key32"                     TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Äã½êÆÀ¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TSYRRK-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  TSYRRK-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  TSYRRK-PTID
           MOVE    "tbl_tsyrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TSYRRK-SELECT-SEC
           IF      FLG-TSYRRK              =   ZERO
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TSYRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TSYRRK          =   1                ) OR
                 (( TSYRRK-NINSTYMD(1:6) <= LNK-NYURECEDEN-SANTEIYM AND
                    LNK-NYURECEDEN-SANTEIYM <= TSYRRK-NINEDYMD(1:6) ))
      *
                   MOVE    "tbl_tsyrrk"     TO  MCP-TABLE
                   MOVE    "key3"           TO  MCP-PATHNAME
                   PERFORM 910-TSYRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TSYRRK              =   ZERO
              IF  (LNK-NYURECEDEN-SANTEIYM   =  TSYRRK-NINSTYMD(1:6))
              AND (TSYRRK-NINSTYMD(7:2)      NOT =  "01")
                CONTINUE
              ELSE
                PERFORM   VARYING   IDX1   FROM   1  BY  1
                          UNTIL     IDX1   >    93
                    IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197000910"
                                        TO  WRK-SKJFTN2-SRYCD(IDX1)
                    END-IF
                END-PERFORM
              END-IF
           END-IF
      *
           MOVE     "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE     "key3"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *£¹£°Æü±Û¤¨¡ÊÄã½êÆÀ¾ðÊó¡Ë
           IF  (TSYRRK-SKJGNSTYMD(1:6)  <= LNK-NYURECEDEN-SANTEIYM) AND
               (TSYRRK-SKJGNSTYMD       NOT = SPACE)
             IF (LNK-NYURECEDEN-SANTEIYM  = TSYRRK-SKJGNSTYMD(1:6)) AND
                (TSYRRK-SKJGNSTYMD(7:2)   NOT = "01")
               CONTINUE
             ELSE
               IF  TSYRRK-SKJGNSTYMD(1:6)  <  LNK-NYURECEDEN-SANTEIYM
                   MOVE   1             TO   TSY90-IDX
               ELSE
                   MOVE   TSYRRK-SKJGNSTYMD(7:2)   TO  TSY90-IDX
               END-IF
               PERFORM   VARYING   IDX1   FROM   TSY90-IDX  BY  1
                          UNTIL     IDX1   >    31
                    IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX1)
                    END-IF
                    COMPUTE   IDX-TSY90  =  IDX1  +  31
                    IF    WRK-SKJ4-RYOYO(IDX-TSY90)  NOT =  SPACE
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX-TSY90)
                    END-IF
                    COMPUTE   IDX-TSY90  =  IDX1  +  62
                    IF    WRK-SKJ4-RYOYO(IDX-TSY90)  NOT =  SPACE
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX-TSY90)
                    END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *ÄãÇ¯¶â¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TNKRRK-REC
           MOVE    LNK-NYURECEDEN-HOSPID   TO  TNKRRK-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  TNKRRK-PTID
           MOVE    "tbl_tnkrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TNKRRK-SELECT-SEC
           IF      FLG-TNKRRK              =   ZERO
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TNKRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TNKRRK          =   1                ) OR
                 (( TNKRRK-STYMD(1:6) <= LNK-NYURECEDEN-SANTEIYM AND
                    LNK-NYURECEDEN-SANTEIYM <= TNKRRK-EDYMD(1:6) ))
      *
                   MOVE    "tbl_tnkrrk"       TO  MCP-TABLE
                   MOVE    "key3"             TO  MCP-PATHNAME
                   PERFORM 910-TNKRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TNKRRK              =   ZERO
              IF  (LNK-NYURECEDEN-SANTEIYM   =  TNKRRK-STYMD(1:6))
              AND (TNKRRK-STYMD(7:2)     NOT =  "01")
                CONTINUE
              ELSE
                PERFORM   VARYING   IDX1   FROM   1  BY  1
                          UNTIL     IDX1   >    93
                    IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001110"
                                        TO  WRK-SKJFTN2-SRYCD(IDX1)
                    END-IF
                END-PERFORM
              END-IF
           END-IF
      *
           MOVE     "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE     "key3"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤Î¾ðÊó¼èÆÀ
           INITIALIZE                          ORCSNYULIFE-AREA
           MOVE    LNK-NYURECEDEN-HOSPID   TO  LNK-NYULIFE-HOSPID
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-NYULIFE-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-NYULIFE-SANTEIYM
           CALL    "ORCSNYULIFE"     USING  ORCSNYULIFE-AREA
           IF      LNK-NYULIFE-RC          NOT =  ZERO
               DISPLAY  "LNK-NYULIFE-RC = " LNK-NYULIFE-RC
           END-IF
      *
      *    ¿©»ö¥ï¡¼¥¯£·¤ÎÊÔ½¸¡ÊÆ±°ìºÞ¤Þ¤È¤áÍÑ¡Ë
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ¿©»öÎÅÍÜÈñ¡ÊÄ«¡Ë
               IF    WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJ-SRYCD(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÄ«¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX1)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  "197001570"
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÄ«¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»öÎÅÍÜÈñ¡ÊÃë¡Ë
               MOVE      ZERO          TO  IDX2
               COMPUTE   IDX2     =    IDX1    +   31
               IF    WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX2)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJ-SRYCD(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÃë¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX2)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  "197001570"
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÃë¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX2)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»öÎÅÍÜÈñ¡ÊÍ¼¡Ë
               MOVE      ZERO          TO  IDX3
               COMPUTE   IDX3     =    IDX1    +   62
               IF    WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX3)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJ-SRYCD(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÍ¼¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX3)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  "197001570"
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÍ¼¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX3)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»ö²ó¿ô
      **         IF    (WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE)
      **             MOVE      1         TO   WRK-SKJ7-KAISU(IDX1)
      **         END-IF
      *        ¿©Æ²²Ã»»
               IF    WRK-SKJ6-SHOKUDOU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ6-SHOKUDOU(IDX1)
                                       TO  WRK-SKJ7-SHOKUDOU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  "197001670"
                                       TO  WRK-SKJ7-SHOKUDOU(IDX1)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    ¿©»ö¤Î¿ÇÎÅ¥³¡¼¥É·×
           PERFORM    VARYING   IDX1    FROM  1  BY  1
                      UNTIL     IDX1    >   31
               IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-RYOYO(IDX1)       TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-SHOKUDOU(IDX1)    TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
      **         IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
      **             MOVE    WRK-SKJ7-TOKU-SHOKU(IDX1)  TO WRK-SRYCD-X
      **             COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
      **                     WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
      **         END-IF
               IF   WRK-SKJ7-SKJFTN(IDX1)      NOT =  SPACE
                   MOVE    WRK-SKJ7-SKJFTN(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               ADD   WRK-SKJ7-RYOYO-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
      **         ADD   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      **                               TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               EVALUATE    WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                   WHEN    1
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
                   WHEN    2
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
                   WHEN    3
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
               END-EVALUATE
               ADD   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
           END-PERFORM
      *
      *     DISPLAY "WRK-SKJ7  MATOME BEFORE"
      *     PERFORM      VARYING    IDX1   FROM   1  BY  1
      *                  UNTIL      IDX1   >   31
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
      *          DISPLAY "WRK-SKJ7-SRYCDTOTAL =(IDX1) = "
      *                   WRK-SKJ7-SRYCDTOTAL (IDX1)
      *     END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿¿©»ö¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
      *    ¿©»ö¥ï¡¼¥¯
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ7-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-SKJ7-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ7-HKNCOMBI-DAY(IDX3)  =
                                       WRK-SKJ7-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ7-SRYCDTOTAL(IDX3)  =
                                       WRK-SKJ7-SRYCDTOTAL(IDX1))
                             IF   WRK-SKJ7-TOKU-SHOKU(IDX3) NOT = SPACE
                                 MOVE  WRK-SKJ7-TOKU-SHOKU(IDX3)  TO
                                           WRK-SKJ7-TOKU-SHOKU(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
                             END-IF
                             ADD     1    TO    WRK-SKJ7-KAISU(IDX1)
                             INITIALIZE   WRK-SKJ7-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           MOVE       ZERO    TO   IDX3
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
      *          DISPLAY "WRK-SKJ7  MATOME AFTER"
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
              IF    WRK-SKJ7-RYOYO(IDX1)   NOT =  SPACE
                IF (WRK-SKJ7-RYOYO(IDX1)   NOT = "197001310")  AND
                   (WRK-SKJ7-RYOYO(IDX1)   NOT = "197001710")
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(3 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                    END-IF
                    MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001310"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   554     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1108     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1662     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   420     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE   840     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1260     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   50     TO
                                 LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   1    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   76     TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   2    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   152    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   3    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   228    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197000810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  460  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  920  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE 1380  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  840  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE 1260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
                            WHEN   "197002210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197002310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
                            WHEN   "197002410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
                            WHEN   "197002510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                            WHEN   "197002710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°Æü°Ê²¼¡Ë¡Ë
                            WHEN   "197002810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°ÆüÄ¶¡Ë¡Ë
                            WHEN   "197002910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                            WHEN   "197003010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                ELSE
      *             Æþ±¡»þÀ¸³èÎÅÍÜ¤Î¿©»öÎÅÍÜ¤ÏÅº»ú£´¤ÎÎÎ°è¤ËÊÖµÑ
                    ADD    1     TO    IDX3
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(4 IDX3)
                    MOVE   ZERO     TO    IDX5
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                        IF     IDX5    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(4 IDX3 IDX5)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                    END-IF
                    MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001310"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   554     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1108     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1662     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   420     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE   840     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1260     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   50     TO
                                 LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   1    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   76     TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   2    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   152    TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   3    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   228    TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197000810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  460  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  920  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1380  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  840  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
                            WHEN   "197002210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197002310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
                            WHEN   "197002410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
                            WHEN   "197002510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                            WHEN   "197002710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°Æü°Ê²¼¡Ë¡Ë
                            WHEN   "197002810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°ÆüÄ¶¡Ë¡Ë
                            WHEN   "197002910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                            WHEN   "197003010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    END-IF
                    MOVE  IDX5  TO  LNK-NYURECEDEN-MEISAISU(4 IDX3)
                END-IF
              END-IF
           END-PERFORM
      *
           .
       2103-SHOKUJI-HENSHU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¡ÊÊ¿À®£±£¸Ç¯£±£°·î°Ê¹ß¡Ë
      *****************************************************************
       2104-KANKYO-HENSHU-SEC        SECTION.
      *
           PERFORM   VARYING   IDX1    FROM   1   BY   1
             UNTIL   IDX1      >       31
               IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)            AND
                     (LNK-NYULIFE-KAN-SRYCD(IDX1)    NOT =  SPACE)  AND
                     (WRK-NYUINKHN(IDX1) NOT =  SPACE)              AND
                     (WRK-GAIHAKU-KBN(IDX1)  =  ZERO  OR  4)
                   MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                     TO  WRK-KANKYO-HKNCOMBI-DAY(IDX1)
                   MOVE   LNK-NYULIFE-KAN-SRYCD(IDX1)
                                     TO  WRK-KANKYO-RYOYO(IDX1)
                   MOVE   LNK-NYULIFE-KANFTN-SRYCD(IDX1)
                                     TO  WRK-KANKYO-FTN(IDX1)
                   IF   LNK-NYULIFE-KAN-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-KAN-SRYCD(IDX1) TO WRK-SRYCD-X
                       COMPUTE WRK-KANKYO-SRYCDTOTAL(IDX1)  =
                           WRK-KANKYO-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
                   END-IF
                   IF   LNK-NYULIFE-KANFTN-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-KANFTN-SRYCD(IDX1)
                                                        TO WRK-SRYCD-X
                       COMPUTE WRK-KANKYO-SRYCDTOTAL(IDX1)  =
                           WRK-KANKYO-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
                   END-IF
               END-IF
           END-PERFORM
      *
      *
      *    Æ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-KANKYO-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-KANKYO-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-KANKYO-HKNCOMBI-DAY(IDX3)  =
                                     WRK-KANKYO-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-KANKYO-SRYCDTOTAL(IDX3)  =
                                       WRK-KANKYO-SRYCDTOTAL(IDX1))
                             ADD     1    TO    WRK-KANKYO-KAISU(IDX1)
                             INITIALIZE   WRK-KANKYO-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î´Ä¶­ÎÅÍÜ¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-KANKYO-RYOYO(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-KANKYO-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(5 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-KANKYO-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(5 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(5 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-KANKYO-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(5 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-KANKYO-RYOYO(IDX1)
                      WHEN  "197001410"
                          MOVE   398     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                      WHEN  "197001810"
                          MOVE   398     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                    END-EVALUATE
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    IF   WRK-KANKYO-FTN(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(5 IDX2 IDX4)
                         MOVE   WRK-KANKYO-FTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(5 IDX2 IDX4)
                         MOVE   320     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                         MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    END-IF
      *
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(5 IDX2)
                END-IF
           END-PERFORM
      *
           .
       2104-KANKYO-HENSHU-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       910-TENSUWRK-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSUWRK
      *
           PERFORM VARYING TTNS-IDX      FROM    1   BY  1
                   UNTIL ( TTNS-IDX      >   TTNS-MAX ) OR
                         ( FLG-TENSUWRK    =   1 )      OR
                         ( TTNS-IDX      >   200 )
               IF    ( WRK-TENSU-SRYCD (TTNS-IDX)
                                       =   WRK-TENSU-CD )
                   MOVE    1           TO  FLG-TENSUWRK
                   MOVE    WRK-TENSU-DATA (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       910-TENSUWRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯ÊÔ½¸½èÍý
      *****************************************************************
       910-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  WRK-TENSU-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  WRK-TENSU-DATA
                                                   (TTNS-MAX)
           END-IF
      *
           .
       910-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
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
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
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
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
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
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
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
      *    ÁªÄêÆþ±¡ÎÁ¿¶ÂØ¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SENTEICDCHG-SELECT-SEC        SECTION.
      *
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SENTEICDCHG
           ELSE
               MOVE    1                  TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       910-SENTEICDCHG-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁªÄêÆþ±¡ÎÁ¿¶ÂØ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SENTEICDCHG-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SENTEICDCHG-REC
               MOVE    ZERO               TO  FLG-SENTEICDCHG
           ELSE
               INITIALIZE                     SENTEICDCHG-REC
               MOVE    1                  TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       910-SENTEICDCHG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Äã½êÆÀ¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-TSYRRK-SELECT-SEC        SECTION.
      *
           MOVE    TSYRRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-TSYRRK
           ELSE
               MOVE    1                  TO  FLG-TSYRRK
           END-IF
      *
           .
       910-TSYRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Äã½êÆÀ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-TSYRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  TSYRRK-REC
               MOVE    ZERO               TO  FLG-TSYRRK
           ELSE
               INITIALIZE                     TSYRRK-REC
               MOVE    1                  TO  FLG-TSYRRK
           END-IF
      *
           .
       910-TSYRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÄãÇ¯¶â¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-TNKRRK-SELECT-SEC        SECTION.
      *
           MOVE    TNKRRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-TNKRRK
           ELSE
               MOVE    1                  TO  FLG-TNKRRK
           END-IF
      *
           .
       910-TNKRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÄãÇ¯¶â¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-TNKRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  TNKRRK-REC
               MOVE    ZERO               TO  FLG-TNKRRK
           ELSE
               INITIALIZE                     TNKRRK-REC
               MOVE    1                  TO  FLG-TNKRRK
           END-IF
      *
           .
       910-TNKRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
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
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
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
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
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
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
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
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SRYACT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SRYACT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
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
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
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
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
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
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "MONFUNC"            USING
                                            MCPAREA
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
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
