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
       PROGRAM-ID.             ORCR0640.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È¡ÊÆþ±¡¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : 
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/12/11    NaCl¡ÝÌçÏÆ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    03.03.05  ÅÀÅ©ÊÔ½¸ÄÉ²Ã
      *  01.00.02    NACL-ÌçÏÆ    03.03.10  ¿©»ö¶èÊ¬¡É£´¡É¤ÎÂÐ±þÄÉ²Ã
      *  01.00.03    NACL-ÌçÏÆ    03.03.12  ÁªÄêÆþ±¡ÎÁÂÐ±þ
      *  01.00.04    NACL-Æ£¸¶    03.04.09  ÆÃÄê´ïºà¡Ê²èÁü¿ÇÃÇ¤Î¥Õ¥£¥ë¥à°Ê³°¡Ë
      *                                     ¤ÎÃ±²Á¤òÉ½¼¨¤¹¤ë¤è¤¦¤Ë½¤Àµ
      *  01.00.05    NACL-ÌçÏÆ    03.04.10  ´Ç¸îÇÛÃÖÅ¬¹ç¤Î³°ÇñÂÐ±þ
      *  01.00.06    NACL-ÌçÏÆ    03.04.16  Á¥°÷ÊÝ¸±¡ÊÈïÊÝ¸±¼Ô¡Ë¡ÜÄÌ¶ÐºÒ³²
      *                                     ¡Ü½é¿Ç»þ¡¡ÂÐ±þ
      *  01.00.07    NACL-ÌçÏÆ    03.04.17  ·ë³Ë¡¢Àº¿À¤Î°ìÉôÉéÃ´³ÛÊÔ½¸½¤Àµ
      *  01.00.08    NACL-Æ£¸¶    03/04/21  ÃÏÊý¸øÈñ¥Ç¡¼¥¿¤Î¤È¤­ÉÔÌÀÊ¬¤Ï
      *                                     ½èÍý¤ÎÂÐ¾Ý¤È¤·¤Ê¤¤
      *  01.00.09    NACL-ÌçÏÆ    03/04/24  °ìÉôÉéÃ´¶â¡Ê¡Ë¤ÎÊÔ½¸½¤Àµ
      *  01.00.10    NACL-ÌçÏÆ    03/05/08  Ì¾¾ÎÍ­¸ú·å¤Î¥»¥Ã¥È
      *  01.00.11    NACL-ÌçÏÆ    03/05/22  »ÄÎÌÇÑ´þ¤Î¿ôÎÌ¥»¥Ã¥È
      *  01.00.12    NACL-ÌçÏÆ    03/05/23  °ìÉôÉéÃ´¶âµ­ºÜ½¤Àµ
      *  01.00.13    NACL-ÌçÏÆ    03/05/28  Ï·¿ÍÀº¿ÀÉÂÅïÅù¤Î¿ÇÎÅ¹Ô°Ù¥³¡¼¥É
      *                                     ÃÖ¤­´¹¤¨
      *  01.00.14    NACL-ÌçÏÆ    03/06/11  ÁªÄêÎÅÍÜ¤ÎÂ¾°åÎÅµ¡´Ø¼õ¿Ç¤Î
      *                                     ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É½¤Àµ
      *  01.00.15    NACL-ÌçÏÆ    03/08/20  ¸øÈñ¼ÂÆü¿ô¥Æ¡¼¥Ö¥ë¾ðÊó¤Î¥»¥Ã¥È
      *  01.00.16    NACL-ÌçÏÆ    03/08/27  ÆÃÄê¼À´µ²þÀµÂÐ±þ
      *  01.00.17    NACL-ÌçÏÆ    03/09/18  °ìÉôÉéÃ´¶â³ÛÊÔ½¸½¤Àµ
      *  01.00.18    NACL-ÌçÏÆ    03/09/24  ¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  01.00.19    NACL-ÌçÏÆ    03/09/29  Êñ³çÂÐ¾Ý¸¡ºº¾ðÊó¤Î¥»¥Ã¥È
      *                                     ¸¡ººÅù¼Â»ÜÈ½ÃÇ¾ðÊó¤Î¥»¥Ã¥È
      *                                     ³Æ¼ïÈÖ¹æÅù¾ðÊó¤Î¥»¥Ã¥È
      *  01.00.20    NACL-ÌçÏÆ    03/10/02  ÅìµþÅÔÃÏÊý¸øÈñÂÐ±þ
      *****************************************************************
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
           SELECT  RECE64-FILE     ASSIGN  RECE64PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE64-KEY
                                   FILE    STATUS  IS  STS-RECE64.
           SELECT  RECE644-FILE    ASSIGN  RECE64PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE644-KEY
                                   FILE    STATUS  IS  STS-RECE64.
           SELECT  RECE65-FILE     ASSIGN  RECE65PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE65-KEY
                                   FILE    STATUS  IS  STS-RECE65.
           SELECT  RECE655-FILE    ASSIGN  RECE65PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE655-KEY
                                   FILE    STATUS  IS  STS-RECE65.
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
       FD  RECE64-FILE.
       01  RECE64-REC. 
           COPY    "CPRCF064.INC".
      *
       FD  RECE644-FILE.
       01  RECE644-REC. 
           COPY    "CPRCF064.INC"  REPLACING  //RECE64//
                                   BY         //RECE644//.
      *
       FD  RECE65-FILE.
       01  RECE65-REC. 
           COPY    "CPRCF065.INC".
      *
       FD  RECE655-FILE.
       01  RECE655-REC. 
           COPY    "CPRCF065.INC"  REPLACING  //RECE65//
                                   BY         //RECE655//.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     ¥ì¥»¥×¥ÈÌÀºÙ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE62//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE64//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE65//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE62                              PIC X(02).
           03  STS-RECE64                              PIC X(02).
           03  STS-RECE65                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-RECE62                              PIC 9(01).
           03  FLG-KANJI                               PIC 9(01).
           03  FLG-SP                                  PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-NYUINACT                            PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-DAY21                               PIC 9(01).
           03  FLG-DAY21-SENTEI                        PIC 9(01).       
           03  FLG-DAY22                               PIC 9(01).
           03  FLG-DAY24                               PIC 9(01).
           03  FLG-NYUKHN                              PIC 9(01).
           03  FLG-MIJISI                              PIC 9(01).
           03  FLG-MIJISI-SENTEI                       PIC 9(01).       
           03  FLG-MIJISI2                             PIC 9(01).
           03  FLG-TENTEKI                             PIC 9(01).
           03  FLG-WRITE65                             PIC 9(01).
           03  FLG-SIKIBETU                            PIC 9(01).
           03  FLG-SENTEICDCHG                         PIC 9(01).
           03  FLG-PTKOHFTN                            PIC 9(01).
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-RECE65-ZAN                          PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-CHIHOU                              PIC 9(01).
           03  FLG-OKIKAE                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
      *
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-COMMIT                           PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-TBL-OCC                             OCCURS  2.
               05  CNT-OUT3                            PIC 9(06).
           03  CNT-DAY21                               PIC 9(06).
           03  CNT-DAY21-SENTEI                        PIC 9(06).       
           03  CNT-DAY22                               PIC 9(06).
           03  CNT-DAY24                               PIC 9(06).
           03  CNT-ZAIREN                              PIC 9(02).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX1                                    PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX5                                    PIC 9(03).
           03  IDX6                                    PIC 9(03).
           03  IDX7                                    PIC 9(02).
           03  IDY                                     PIC 9(03).
           03  IDZ                                     PIC 9(03).
           03  IDW                                     PIC 9(03).
           03  ID1                                     PIC 9(03).
           03  ID2                                     PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  HC-IDX                                  PIC 9(03).
           03  KH-IDX                                  PIC 9(03).
           03  BUN-IDX                                 PIC 9(03).
           03  BUN-IDX2                                PIC 9(03).
           03  BUN-IDX3                                PIC 9(03).
           03  BUN-IDX4                                PIC 9(03).
           03  SYUKEI-IDX                              PIC 9(03).
           03  SYUKEI-IDX2                             PIC 9(03).
           03  SYUKEI-IDX3                             PIC 9(03).
           03  SYUKEI-IDX4                             PIC 9(03).
           03  GENSAN-IDX                              PIC 9(03).
           03  NYUINACT-IDX                            PIC 9(03).
           03  KOHFTN-IDX                              PIC 9(03).
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
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¸¡º÷ÍÑÂÐ¾ÝÆüÎÎ°è
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY                            PIC 9(02).
           03  WK-TAIHI-FLG                            PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM                      PIC 9(06).
               05  WK-TAIHI-SRYD                       PIC 9(02).
      *
       01  WRK-AREA-SAIKEI.
           03  WRK-KOH-FTN                             PIC 9(09).
           03  WRK-KOH-GAI                             PIC 9(09).
           03  FTN-KOH-XXX                             PIC 9(09).       
           03  FTN-KOH-YYY                             PIC 9(09).       
           03  FLG-KOH                                 PIC 9(01).
           03  WRK-FTNKEI                              PIC 9(09).
           03  WRK-FTNKEI2                             PIC 9(09).
           03  WRK-FTNKEI3                             PIC 9(09).
           03  WRK-KINGAKU                             PIC 9(09).
           03  WRK-KINGAKU2                            PIC 9(09).
           03  WRK-KINGAKU3                            PIC 9(09).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY1-SENTEI                     PIC 9(04).       
               05  WRK-DAY1-SENTEI-KHNTEN              PIC 9(08).       
               05  WRK-DAY1-SENTEI-ZAITEN              PIC 9(08).       
               05  WRK-DAY1-SENTEI-SRYCD               PIC 9(09).       
               05  WRK-DAY1-SENTEI-SRYCD2              PIC 9(09).       
               05  WRK-DAY1-NYUTENTTLKBN               PIC 9(03).
               05  WRK-DAY1-KHNTEN                     PIC 9(08).
               05  WRK-DAY1-NYUKHNKSNKBN               PIC 9(03).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
      *
       01  WRK-DAY-TABLE2.
           03  WRK-DAY-T2                              OCCURS   31.
               05  WRK-DAY21                           PIC 9(04).
               05  WRK-DAY21-SENTEI                    PIC 9(04).       
               05  WRK-DAY22                           PIC 9(04).
               05  WRK-DAY22-NYUTENTTLKBN              PIC 9(03).
               05  WRK-DAY22-NYUKHNRYO                 PIC 9(08).
               05  WRK-DAY22-NYUKHNKSNKBN              PIC 9(03).
               05  WRK-DAY24                           PIC 9(04).
      *
       01  WRK-GAI-TABLE.
           03  WRK-GAI-TBL                             OCCURS   10.
             04  WRK-GAI-T                             OCCURS   10.
               05  WRK-GAI-DAY                         PIC 9(02).
               05  WRK-GAI-ZAITEN                      PIC 9(08).
               05  WRK-GAI-ZAIKAISU                    PIC 9(04).
               05  WRK-GAI-KAISU-T.
                06  WRK-GAI-KAISU-OCC                  OCCURS   31.
                 07  WRK-GAI-KAISU                     PIC 9(01).
      *
       01  WRK-GAI-TABLE2.
           03  WRK-GAI-T2                              OCCURS   20.
               05  WRK-GAI2-DAY                        PIC 9(02).
               05  WRK-GAI2-SRYCD                      PIC 9(09).
               05  WRK-GAI2-ZAITEN                     PIC 9(08).
               05  WRK-GAI2-ZAIKAISU                   PIC 9(04).
               05  WRK-GAI2-SRYKBNFLG                  PIC 9(01).
               05  WRK-GAI2-KAISU-T.
                06  WRK-GAI2-KAISU-OCC                 OCCURS   31.
                 07  WRK-GAI2-KAISU                    PIC 9(01).
      *
       01  WRK-GAI-DATA                                PIC X(55).
      *
       01  WRK-SKJ-TABLE.
           03  WRK-SKJ-T                               OCCURS   31.
               05  WRK-SKJ-ZAITEN                      PIC 9(08).
               05  WRK-SKJ-ZAIKAISU                    PIC 9(04).
               05  WRK-SKJ-SRYCD-T.
                06  WRK-SKJ-SRYCD-OCC                  OCCURS    5.
                 07  WRK-SKJ-SRYCD                     PIC 9(09).
      *
       01  WRK-SKJ-TABLE2.
           03  WRK-SKJ-T2                              OCCURS   31.
               05  WRK-SKJ2-ZAITEN                     PIC 9(08).
               05  WRK-SKJ2-ZAIKAISU                   PIC 9(04).
               05  WRK-SKJ2-MEISAISU                   PIC 9(04).
               05  WRK-SKJ2-SRYCDTOTAL                 PIC 9(14).
               05  WRK-SKJ2-SRYCD-T.
                06  WRK-SKJ2-SRYCD-OCC                 OCCURS    5.
                 07  WRK-SKJ2-SRYCD                    PIC 9(09).
               05  WRK-SKJ2-KAISU-T.
                06  WRK-SKJ2-KAISU-OCC                 OCCURS   31.
                 07  WRK-SKJ2-KAISU                    PIC 9(01).
      *
       01  WRK-MIJ-TABLE.
           03  WRK-MIJ-T                               OCCURS   10.
             04  WRK-MIJ-HKNCOMBI                      PIC 9(04).
             04  WRK-MIJ-T2                            OCCURS    4.
               05  WRK-MIJ-CNT90                       PIC 9(02).
               05  WRK-MIJ-CNT92                       PIC 9(02).
               05  WRK-MIJ-T3                          OCCURS   31.
                   07  WRK-MIJ-DAY90                   PIC 9(04).
                   07  WRK-MIJ-DAY92                   PIC 9(04).
      *
       01  WRK-ORCSTEN-TABLE.
           03  WRK-ORCSTEN-T                           OCCURS  100.
               05  WRK-ORCSTEN-SRYCD                   PIC X(09).
               05  WRK-ORCSTEN-SURYO                   PIC 9(05)V9(03).
           03  WRK-ORCSTEN-T2                          OCCURS   31.
               05  WRK-ORCSTEN-DAY                     PIC 9(03).
           03  WRK-ORCSTEN-ZAIKAISU                    PIC 9(04).
      *
      *    ¥·¥¹¥«¥ó¡Ê£µ£°£°£°¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC     OCCURS  10.
               05  TBL-SYS5000                         PIC X(610).
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
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-ZENKAKU                             PIC X(80).
           03  WRK-NUM                                 PIC X(80).
           03  WRK-NUM-X                               PIC X(10). 
           03  WRK-STIDX                               PIC 9(02).
           03  WRK-EDIDX                               PIC 9(02).
           03  WRK-SRYCD                               PIC 9(09).
           03  WRK-ZAINUM                              PIC 9(08).
           03  WRK-ZAINUM-CNT                          PIC 9(03).
           03  WRK-RENNUM                              PIC 9(02).
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
      *
           03  WRK-SRYSURYO                            PIC 9(05)V9(03).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
      *
           03  WRK-PREFNUM                             PIC X(02).
      *
           03  WRK-CHIHOU-NUM                          PIC X(03).
      *
           03  WRK-RECEERR                             PIC X(200).
      *
           03  WRK-NYUJGNGAK                           PIC 9(07).
      *
           03  WRK-TOKKI                               PIC X(02).
           03  TOKKI-IDX                               PIC 9(02). 
      *
           03  WRK-KHNTEN                              PIC 9(08).
           03  WRK-KSNTEN                              PIC S9(08).
           03  WRK-SENTEI-KHNTEN                       PIC 9(08).
           03  WRK-SENTEI-ZAITEN                       PIC 9(08).
           03  WRK-SENTEI-ZAITEN-W                     PIC 9(08).       
           03  WRK-SENTEI-SRYCD                        PIC 9(09).       
           03  WRK-SENTEI-SRYCD2                       PIC 9(09).       
           03  WRK-SENTEI-SRYCD-W                      PIC 9(09).       
           03  WRK-SENTEI-SRYCD2-W                     PIC 9(09).       
           03  WRK-SENTEI-KEISU                        PIC 9V99.
           03  WRK-GAIHAKU-SRYCD                       PIC X(09).
           03  WRK-GAIHAKU-KHNTEN                      PIC 9(08).
           03  WRK-NYUKHNKSNKBN                        PIC 9(03).
           03  WRK-SHOKUJIRYOYO                        PIC 9(01).
           03  WRK-TOKUKANRIKASAN                      PIC 9(01).
           03  WRK-SYS-MIJISI-T.
             04  WRK-SYS-MIJISI-OCC                    OCCURS  4.
               05  WRK-SYS-MIJISI                      PIC 9(01).
               05  WRK-SYS-MIJISI-ZAITEN               PIC 9(08).
      *
           03  WRK-SKJ-FTN-T.
             04  WRK-SKJ-FTN-OCC                       OCCURS  5.
               05  WRK-SKJ-FTN                         PIC 9(07).
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
           03  CONS-YAKUKAN-COMMENT  PIC X(09)  VALUE  "120001610".
      *    Æþ±¡±ÉÍÜ¿©»ö»ØÆ³ÎÁ
           03  CONS-NYUEIYO-COMMENT  PIC X(09)  VALUE  "113003310".
      *    Âà±¡Á°Ë¬Ìä»ØÆ³ÎÁ
           03  CONS-HOUMON-COMMENT1  PIC X(09)  VALUE  "190030310".
      *    ¡ÊÏ·¡ËÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ
           03  CONS-HOUMON-COMMENT2  PIC X(09)  VALUE  "190723610".
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ
           03  CONS-SYOBYO-COMMENT   PIC X(09)  VALUE  "180000710".
      *    ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ
           03  CONS-RYOYO-COMMENT    PIC X(09)  VALUE  "113004310".
      *    Âà±¡»þÉþÌô»ØÆ³²Ã»»
           03  CONS-TAIIN-COMMENT    PIC X(09)  VALUE  "113006270".
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ
           03  CONS-HOUSIJI-COMMENT  PIC X(09)  VALUE  "114008010".
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
      *    Àº¿À²ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ
           03  CONS-HOUSEI-COMMENT   PIC X(09)  VALUE  "180018310".
      *    Àº¿À²Ê³ÖÎ¥ÉÂ¼¼´ÉÍý²Ã»»
           03  CONS-KAKURI-COMMENT   PIC X(09)  VALUE  "190107070".
      *
      *****»ÀÁÇ 
      *****COPY    "CMSANSOTBL.INC".
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
      *
       01  WRK-CONS-AREA.    
      *    ¸ÇÄêÃ¼Ëö£É£Ä
           03  WRK-CONS-TERMID     PIC X(64)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
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
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
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
      *    
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK5000.INC".
      *    
      *
      *    ´µ¼Ô¥³¥á¥ó¥È
       01  PTCOM-REC.
       COPY    "CPPTCOM.INC".
      *
      *    ÁªÄêÆþ±¡ÎÁ
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    ´µ¼Ô¸øÈñÉéÃ´
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
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
           INITIALIZE                  SPA-AREA
                                       WRK-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCR0640"      TO  JOB-PGID
           MOVE    "¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *                             
      *    °åÎÅµ¡´Ø¾ðÊó
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               PERFORM 960-KANRIMST-READ-SEC
      *
               IF      FLG-SYSKANRI     =   ZERO
                  MOVE   MCPDATA-REC   TO  SYS-1001-REC
                  MOVE   SYS-1001-PREFNUM  TO  WRK-PREFNUM
               ELSE
                  MOVE  "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  GO  TO  100-INIT-EXT
               END-IF    
           ELSE
               MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE   "SYSKANRI-KEY"       TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *      ¿©»ö¡¦Ì¤¼Â»Ü¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
             MOVE     ZERO            TO  WRK-SHOKUJIRYOYO
                                          WRK-TOKUKANRIKASAN
      *
             INITIALIZE                   WRK-SYS-MIJISI-T
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
             MOVE    "01"                TO  ORC-DBYMD (7:2)
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
             CALL    "ORCMCPSUB"             USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
             IF      MCP-RC              =   ZERO
                 MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
                 PERFORM 960-KANRIMST-READ-SEC
                 MOVE    MCPDATA-REC     TO  SYS-5000-REC
      *
                 PERFORM  SYSKAN-HENSYU-SEC
      *
             END-IF
      *
             MOVE   "SYSKANRI-KEY"       TO  WK-PATH
             PERFORM   CSR-CLOSE-SEC
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
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
             CALL    "ORCMCPSUB"             USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
             IF      MCP-RC              =   ZERO
                 MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                 PERFORM 960-KANRIMST-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
      *
                   MOVE    MCPDATA-REC  TO  SYS-5000-REC
                   ADD     1            TO  SYSKAN-IDX
                   MOVE   SYS-5000-REC  TO  TBL-SYS5000(SYSKAN-IDX)
      *
                   MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                   PERFORM 960-KANRIMST-READ-SEC
      *
                 END-PERFORM
      *
             END-IF
      *
             MOVE   "SYSKANRI-KEY8"     TO  WK-PATH
             PERFORM   CSR-CLOSE-SEC
      *
           END-IF
      *
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥«¥ó¤ÎÊÔ½¸
      *****************************************************************
       SYSKAN-HENSYU-SEC                SECTION.
      *
           PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                    UNTIL     GENSAN-IDX   >   4
                IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"    
                    MOVE   1   TO   WRK-SYS-MIJISI(GENSAN-IDX)
                END-IF
           END-PERFORM
      *
           IF  SYS-5000-SHOKUJIRYOYOCD   =  "197000110"
                  MOVE     1         TO   WRK-SHOKUJIRYOYO
                  IF  SYS-5000-SHOKUKBN(1) =  "1"
                       MOVE  ZERO    TO   WRK-TOKUKANRIKASAN
                  ELSE
                       MOVE  1       TO   WRK-TOKUKANRIKASAN
                  END-IF
           ELSE
                  MOVE     2         TO   WRK-SHOKUJIRYOYO
           END-IF
      *
           .
       SYSKAN-HENSYU-EXT.
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
                   MOVE    "RECE640"           TO  RECE64PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE64PARA-TERMID
                   MOVE    "RECE650"           TO  RECE65PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE65PARA-TERMID
               ELSE
      *    ÀÁµá½ñ½èÍý
                   MOVE    "RECE62X"           TO  RECE62PARA-FILE-ID
                   MOVE    WRK-CONS-TERMID     TO  RECE62PARA-TERMID
               END-IF
      *
               PERFORM 200-HENSYU-SEC
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
       200-HENSYU-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE62
      *
           IF      IDZ             =   1               
      *    ¥ì¥»¥×¥È½èÍý
               OPEN    OUTPUT              RECE644-FILE
               CLOSE                       RECE644-FILE
      *
               OPEN    OUTPUT              RECE655-FILE
               CLOSE                       RECE655-FILE
      *
               OPEN    OUTPUT              RECE64-FILE
      ***                                  RECE65-FILE
               OPEN    I-O                 RECE65-FILE
           END-IF
      *
           OPEN    I-O                 RECE62-FILE
      *
           PERFORM    RECE62-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE62  =   1
               PERFORM     210-HENSYU-SEC
      *
               PERFORM     RECE62-READ-SEC
      	       IF  CNT-IN-COMMIT   =   300
      	           MOVE  ZERO          TO      CNT-IN-COMMIT
                   PERFORM 900-DBCOMMIT-SEC
                   PERFORM 900-DBSTART-SEC
               END-IF
           END-PERFORM
      *
           CLOSE   RECE62-FILE
      *
           IF      IDZ             =   1               
      *    ¥ì¥»¥×¥È½èÍý
               CLOSE   RECE64-FILE
                       RECE65-FILE
           END-IF
           .
       200-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ÊÔ½¸½èÍý
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
      *    ÃÏÊý¸øÈñ¥Ç¡¼¥¿¤Î¤È¤­ÉÔÌÀÊ¬¤Ï½èÍý¤ÎÂÐ¾Ý¤È¤·¤Ê¤¤
           IF      IDZ                  =   2
           AND     RECE62-RECESYUBETU   =   ZERO
               GO  TO  210-HENSYU-EXT
           END-IF
      *
           INITIALIZE          WRK-AREA-SAIKEI
      *
           INITIALIZE          WRK-DAY-TABLE
                               WRK-MIJ-TABLE
      *
      ***  PERFORM    VARYING    IDX    FROM   1   BY   1
      ***             UNTIL     (IDX      >   10)  OR
      ***                       (RECE62-SRYKA(IDX)   =   SPACE)
               PERFORM    RECE64-HENSYU-SEC
               PERFORM    RECE64-HENSYU2-SEC
               PERFORM    RECE64-HENSYU3-SEC
               PERFORM    FTN-SET-SEC
      ***  END-PERFORM
           PERFORM     SAIKEI-KEISAN-SEC
      *
           PERFORM     RECE62-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY7"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY7"   TO      ORC-DBPATH
               PERFORM  SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1)
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
      *
                               MOVE   ZERO   TO   WRK-SRYSURYO
                               MOVE   ZERO   TO   WRK-SRYSURYO2
                               MOVE   ZERO   TO   WRK-SRYSURYO3
                               MOVE   ZERO   TO   WRK-TANICD
                               MOVE   ZERO   TO   WRK-TANIMOJISU
                               MOVE   SPACE  TO   WRK-TANINAME
      *
                               MOVE   ZERO   TO   FLG-WRITE65
                               PERFORM   RECE64-TENSOU-SEC
                               PERFORM   RECE65-HENSYU-SEC
                               IF    FLG-WRITE65  =   1
                                 PERFORM RECE64-WRITE-SEC
                               END-IF
                           ELSE
                             IF   ACCT-ZAIKAISU  NOT =    ZERO
                               IF   ACCT-SRYKBN  =  "11"  OR
                                                    "33"  OR  "99" 
                                 IF  ACCT-SRYCDTOTAL = CONS-TOKKI1 OR
                                                       CONS-TOKKI2 OR
                                                       CONS-TOKKI3 OR
                                                       CONS-TOKKI4 OR
                                                       CONS-TOKKI5 OR
                                                       CONS-TOKKI7 OR
                                                       CONS-TOKKI8 OR
                                                       CONS-TOKKI9 OR
                                                       CONS-TOKKI10 OR
                                                       CONS-TOKKI11 OR
                                                       CONS-TOKKI12
                                      PERFORM   TOKKI-HENSYU-SEC
                                 ELSE
      *
                                      MOVE   ZERO   TO   WRK-SRYSURYO
                                      MOVE   ZERO   TO   WRK-SRYSURYO2
                                      MOVE   ZERO   TO   WRK-SRYSURYO3
                                      MOVE   ZERO   TO   WRK-TANICD
                                      MOVE   ZERO   TO   WRK-TANIMOJISU
                                      MOVE   SPACE  TO   WRK-TANINAME
      *
                                      MOVE   ZERO   TO   FLG-WRITE65
                                      PERFORM   RECE64-TENSOU-SEC
                                      PERFORM   RECE65-HENSYU-SEC
                                      IF    FLG-WRITE65  =   1
                                        PERFORM RECE64-WRITE-SEC
                                      END-IF
                                 END-IF
                               END-IF
                             END-IF
                           END-IF
                           MOVE    10                TO  IDX1
                       END-IF
                   END-PERFORM          
                   MOVE     "SRYACCT-KEY7"   TO   ORC-DBPATH
                   PERFORM   SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SRYACCT SELECT ERR" 
           END-IF
      *
           MOVE   "SRYACCT-KEY7"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       RECE64-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPID         TO      NACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
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
                 IF     NACCT-ZAISKBKBN    =    "1"  
                   IF     NACCT-SRYCDTOTAL   NOT =    099999916         
                       PERFORM      NYUINKHN-GET-SEC
                   END-IF                                               
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO 
                        IF     NACCT-SRYCDTOTAL    =    099999916       
                           MOVE   NACCT-DAY(IDY)                        
                                           TO   WRK-DAY1-SENTEI(IDY)    
                        ELSE                                            
                           MOVE   NACCT-DAY(IDY)  TO   WRK-DAY1(IDY) 
                           IF  NACCT-SRYKBN  =  "90"                    
                              MOVE  903  TO  WRK-DAY1-NYUTENTTLKBN(IDY) 
                           ELSE                                         
                              MOVE  920  TO  WRK-DAY1-NYUTENTTLKBN(IDY) 
                           END-IF                                       
                           MOVE   WRK-KHNTEN
                                         TO  WRK-DAY1-KHNTEN(IDY) 
                           MOVE   WRK-SENTEI-KHNTEN                     
                                         TO   WRK-DAY1-SENTEI-KHNTEN    
                                                               (IDY)    
                           MOVE   WRK-SENTEI-ZAITEN                     
                                         TO   WRK-DAY1-SENTEI-ZAITEN    
                                                               (IDY)    
                           MOVE   WRK-SENTEI-SRYCD                      
                                         TO   WRK-DAY1-SENTEI-SRYCD     
                                                               (IDY)    
                           MOVE   WRK-SENTEI-SRYCD2                     
                                         TO   WRK-DAY1-SENTEI-SRYCD2    
                                                               (IDY)    
                           MOVE   WRK-NYUKHNKSNKBN                      
                                      TO  WRK-DAY1-NYUKHNKSNKBN(IDY)    
                        END-IF                                          
                     END-IF
                   END-PERFORM
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
                 MOVE          "NYUINACCT-KEY7"  TO   ORC-DBPATH
                 PERFORM        NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE   "NYUINACCT-KEY7"         TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *
           PERFORM      SENTEI-KEISU-GET-SEC
      *
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPID         TO      NACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "DBSELECT"            TO      MCP-FUNC
           MOVE    "NYUINACCT-KEY7"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
      *
               MOVE    "NYUINACCT-KEY7"  TO      ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1") 
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  OR  "2"  OR
                                                "4"  OR  "6"
      *
                   MOVE     ZERO       TO    CNT-ZAIREN
      *
                   PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                      INITIALIZE          WRK-DAY-TABLE2
                                          WRK-GAI-TABLE
                                          WRK-GAI-TABLE2
                                          WRK-SKJ-TABLE
                                          WRK-SKJ-TABLE2
                      MOVE     ZERO       TO    CNT-DAY21
                                                CNT-DAY21-SENTEI        
                                                CNT-DAY22
                                                CNT-DAY24
                      MOVE     ZERO       TO    FLG-DAY21
                                                FLG-DAY21-SENTEI        
                                                FLG-DAY22
                                                FLG-DAY24
                                                FLG-MIJISI
                                                FLG-MIJISI-SENTEI       
                      MOVE     ZERO       TO    WRK-SENTEI-ZAITEN-W     
                                                WRK-SENTEI-SRYCD-W      
                                                WRK-SENTEI-SRYCD2-W     
      *
                      PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                         IF  RECE62-HKNCOMBI (IDX1)  =  WRK-DAY5(IDY)
                            IF   NACCT-ZAITEN    NOT =  ZERO
                            AND  NACCT-DAY(IDY)  NOT =  ZERO
                            AND  NACCT-ZAISKBKBN NOT =  "4"
                              IF NACCT-ZAISKBKBN     =  "1"
                                 IF  WRK-DAY2(IDY) =  ZERO
                                  IF  WRK-DAY1-SENTEI(IDY) =  ZERO      
                                     MOVE   NACCT-DAY(IDY)  
                                            TO   WRK-DAY21(IDY) 
                                     ADD    1   TO   CNT-DAY21
                                     MOVE   1   TO   FLG-DAY21
                                     MOVE   1   TO   FLG-MIJISI
                                  ELSE                                  
                                     MOVE   NACCT-DAY(IDY)              
                                            TO   WRK-DAY21-SENTEI(IDY)  
                                     MOVE   WRK-DAY1-SENTEI-ZAITEN(IDY) 
                                            TO   WRK-SENTEI-ZAITEN-W    
                                     MOVE   WRK-DAY1-SENTEI-SRYCD(IDY)  
                                            TO   WRK-SENTEI-SRYCD-W     
                                     MOVE   WRK-DAY1-SENTEI-SRYCD2(IDY) 
                                            TO   WRK-SENTEI-SRYCD2-W    
                                     ADD    1   TO   CNT-DAY21-SENTEI   
                                     MOVE   1   TO   FLG-DAY21-SENTEI   
                                     MOVE   1   TO   FLG-MIJISI-SENTEI  
                                  END-IF                                
                                 END-IF
                              ELSE
                                 IF  (WRK-DAY1(IDY)   NOT  =  ZERO)
                                 AND (WRK-DAY2(IDY) =  ZERO  OR  4)
                                    MOVE   NACCT-DAY(IDY)  
                                               TO   WRK-DAY21(IDY) 
                                    ADD    1     TO   CNT-DAY21
                                    MOVE   1     TO   FLG-DAY21
                                 END-IF
                              END-IF
                            END-IF
      *                     *³°Çñ
                            IF  (WRK-DAY1(IDY)   NOT  =  ZERO)
                            AND (NACCT-ZAITEN         =  ZERO)
                            AND (NACCT-ZAISKBKBN      =  "2" )
                            AND (NACCT-DAY(IDY)  NOT  =  ZERO)
                               MOVE   NACCT-DAY(IDY)  
                                            TO   WRK-DAY22(IDY) 
                               MOVE   WRK-DAY1-NYUTENTTLKBN(IDY)
                                        TO  WRK-DAY22-NYUTENTTLKBN(IDY)
                               MOVE   WRK-DAY1-NYUKHNKSNKBN(IDY)        
                                        TO  WRK-DAY22-NYUKHNKSNKBN(IDY) 
                               PERFORM      NYUINACCT-GET-SEC
                               ADD    1     TO   CNT-DAY22
                               MOVE   1     TO   FLG-DAY22
                            END-IF
      *                     *¿©»ö
                            IF  (WRK-DAY1(IDY)   NOT  =  ZERO)
                            AND (NACCT-ZAITEN         =  ZERO)
                            AND (NACCT-ZAISKBKBN      =  "4" )
                            AND (NACCT-DAY(IDY)  NOT  =  ZERO)
                               IF  WRK-DAY2(IDY) =  ZERO  OR  4
                                  MOVE   NACCT-DAY(IDY)  
                                            TO   WRK-DAY24(IDY) 
                                  PERFORM      NYUINACCT-SYU2-SEC
                                  ADD    1     TO   CNT-DAY24
                                  MOVE   1     TO   FLG-DAY24
                               END-IF
                            END-IF
                         END-IF
      *
                      END-PERFORM
      *
                      IF   FLG-DAY21    =   1
                         ADD    1     TO   CNT-ZAIREN
                         PERFORM   RECE64-TENSOU2-SEC
                         PERFORM   RECE65-HENSYU2-SEC
                         PERFORM   RECE64-WRITE-SEC
                         IF    FLG-MIJISI  =  1  
                            PERFORM VARYING SYUKEI-IDX3 FROM 1 BY 1
                               UNTIL  SYUKEI-IDX3           > 4
      *
                             IF    WRK-SYS-MIJISI(SYUKEI-IDX3) = 1
                               PERFORM  VARYING  IDY  FROM  1  BY  1
                                      UNTIL   IDY     >       31
                                IF   WRK-DAY21(IDY)  NOT =  ZERO
                                  EVALUATE WRK-DAY1-NYUTENTTLKBN(IDY)
                                    WHEN   903
                                      MOVE   RECE62-HKNCOMBI (IDX1)
                                        TO   WRK-MIJ-HKNCOMBI(IDX1)
                                      MOVE   WRK-DAY21(IDY)  
                                        TO   WRK-MIJ-DAY90
                                             (IDX1 SYUKEI-IDX3 IDY) 
                                    WHEN   920
                                      MOVE   RECE62-HKNCOMBI (IDX1)
                                        TO   WRK-MIJ-HKNCOMBI(IDX1)
                                      MOVE   WRK-DAY21(IDY)  
                                        TO   WRK-MIJ-DAY92
                                             (IDX1 SYUKEI-IDX3 IDY)
                                  END-EVALUATE
                                END-IF
                               END-PERFORM
                             END-IF
      *
                            END-PERFORM
                         END-IF
                      END-IF
      *
                      IF   FLG-DAY21-SENTEI    =   1
                         ADD    1     TO   CNT-ZAIREN
                         PERFORM   RECE64-TENSOU2-SEN-SEC
                         PERFORM   RECE65-HENSYU2-SEN-SEC
                         PERFORM   RECE64-WRITE-SEC
                         IF    FLG-MIJISI-SENTEI  =  1  
                            PERFORM VARYING SYUKEI-IDX3 FROM 1 BY 1
                               UNTIL  SYUKEI-IDX3           > 4
      *
                             IF    WRK-SYS-MIJISI(SYUKEI-IDX3) = 1
                               PERFORM  VARYING  IDY  FROM  1  BY  1
                                      UNTIL   IDY     >       31
                                IF  WRK-DAY21-SENTEI(IDY)  NOT =  ZERO
                                  EVALUATE WRK-DAY1-NYUTENTTLKBN(IDY)
                                    WHEN   903
                                      MOVE   RECE62-HKNCOMBI (IDX1)
                                        TO   WRK-MIJ-HKNCOMBI(IDX1)
                                      MOVE   WRK-DAY21-SENTEI(IDY)  
                                        TO   WRK-MIJ-DAY90
                                             (IDX1 SYUKEI-IDX3 IDY) 
                                    WHEN   920
                                      MOVE   RECE62-HKNCOMBI (IDX1)
                                        TO   WRK-MIJ-HKNCOMBI(IDX1)
                                      MOVE   WRK-DAY21-SENTEI(IDY)  
                                        TO   WRK-MIJ-DAY92
                                             (IDX1 SYUKEI-IDX3 IDY)
                                  END-EVALUATE
                                END-IF
                               END-PERFORM
                             END-IF
      *
                            END-PERFORM
                         END-IF
                      END-IF
      *
                      IF   FLG-DAY22    =   1
                         PERFORM    NYUINACCT-HEN-SEC
                         ADD    1     TO   CNT-ZAIREN
                         PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                            UNTIL  SYUKEI-IDX3           > 20
                             OR    WRK-GAI2-DAY(SYUKEI-IDX3) = ZERO
      *
                             PERFORM   RECE64-TENSOU2-GAI-SEC
                             PERFORM   RECE65-HENSYU2-GAI-SEC
                             PERFORM   RECE64-WRITE-SEC
      *
                         END-PERFORM
                      END-IF
      *
                      IF   FLG-DAY24    =   1
                         PERFORM    NYUINACCT-HEN2-SEC
                         ADD    1     TO   CNT-ZAIREN
                         PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                            UNTIL  SYUKEI-IDX3           >  31
                             OR    WRK-SKJ2-ZAITEN(SYUKEI-IDX3) = ZERO
      *
                             PERFORM   RECE64-TENSOU2-SKJ-SEC
                             PERFORM   RECE65-HENSYU2-SKJ-SEC
                             PERFORM   RECE64-WRITE-SEC
      *
                         END-PERFORM
                      END-IF
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE          "NYUINACCT-KEY7"  TO   ORC-DBPATH
                 PERFORM        NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE   "NYUINACCT-KEY7"         TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *
      *    Ì¤¼Â»Ü¸º»»
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1         >  10
      *
             IF    WRK-MIJ-HKNCOMBI(IDX1)  NOT =  ZERO 
      *
               PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                      UNTIL  SYUKEI-IDX3  >   4
      *
                 PERFORM   VARYING   IDY   FROM   1   BY   1
                         UNTIL   IDY    >  31
                    IF   WRK-MIJ-DAY90(IDX1 SYUKEI-IDX3 IDY)  
                                                  NOT =  ZERO
                        ADD   1   TO  WRK-MIJ-CNT90(IDX1 SYUKEI-IDX3)
                    END-IF
                    IF   WRK-MIJ-DAY92(IDX1 SYUKEI-IDX3 IDY)  
                                                  NOT =  ZERO
                        ADD   1   TO  WRK-MIJ-CNT92(IDX1 SYUKEI-IDX3)
                    END-IF
                 END-PERFORM
      *
               END-PERFORM
      *
             END-IF
      *
           END-PERFORM
      *
      *
           PERFORM      MIJISI-GET-SEC
      *
           MOVE     ZERO       TO    CNT-ZAIREN
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1         >  10
      *
             IF    WRK-MIJ-HKNCOMBI(IDX1)  NOT =  ZERO  
      *
               ADD       1      TO     CNT-ZAIREN
      *
               PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                      UNTIL  SYUKEI-IDX3  >   4
      *
                 IF   WRK-MIJ-CNT90(IDX1 SYUKEI-IDX3)  NOT =  ZERO
                       MOVE      1      TO     FLG-MIJISI2
                       PERFORM   RECE64-TENSOU2-MIJ-SEC
                       PERFORM   RECE65-TENSOU2-MIJ-SEC
                       PERFORM   RECE65-WRITE-SEC
                       PERFORM   RECE64-WRITE-SEC
                 END-IF
      *
               END-PERFORM
      *
               PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                    UNTIL  SYUKEI-IDX3  >   4
      *
                 IF   WRK-MIJ-CNT92(IDX1 SYUKEI-IDX3)  NOT =  ZERO
                       MOVE      2      TO     FLG-MIJISI2
                       PERFORM   RECE64-TENSOU2-MIJ-SEC
                       PERFORM   RECE65-TENSOU2-MIJ-SEC
                       PERFORM   RECE65-WRITE-SEC
                       PERFORM   RECE64-WRITE-SEC
                 END-IF
      *
               END-PERFORM
      *
             END-IF
      *
           END-PERFORM
      *
           .
       RECE64-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-HENSYU3-SEC                SECTION.
      *
      *    ÅÀÅ©¤Î·×»»
      *
           MOVE    ZERO                TO  CNT-ZAIREN
      *
           MOVE    RECE62-HOSPID       TO  SPA-HOSPID
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO
      *
              ADD      1                  TO  CNT-ZAIREN
      *
              PERFORM   RECE62-HENSYU3-IDX-SEC
      *
              INITIALIZE                  ORCSTENTEKAREA
              MOVE    "2"                 TO  ORCSTEN-KBN
              MOVE    RECE62-PTID         TO  ORCSTEN-PTID
              MOVE    RECE62-SRYYM        TO  ORCSTEN-SRYYM
              MOVE    RECE62-HKNCOMBI (IDX1)
                                          TO  ORCSTEN-HKNCOMBI
              CALL    "ORCSTENTEK"        USING
                                          SPA-AREA
                                          ORCSTENTEKAREA
      *
      *       IF      ORCSTEN-ERRCD       =   ZERO
      *
      *           ÌôºÞ
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                              UNTIL   IDY     >       31
      *
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
                       END-PERFORM
      *
                       PERFORM   RECE64-TENSOU3-SEC
      *
                       MOVE      100  TO   IDX5
                       PERFORM   RECE65-TENSOU3-SEC
                       PERFORM   RECE62-HENSYU3-SEC
      *
                       PERFORM   RECE64-WRITE-SEC
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
                       MOVE   2      TO   FLG-SIKIBETU
      *
                       INITIALIZE         WRK-ORCSTEN-TABLE
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       10
                          MOVE  ORCSTEN-KIZ-SRYCD(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SRYCD(IDX5)
                          MOVE  ORCSTEN-KIZ-SURYO(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SURYO(IDX5)
                       END-PERFORM
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       31
                          MOVE  ORCSTEN-KIZ-DAY  (IDY IDX5)  TO  
                                      WRK-ORCSTEN-DAY  (IDX5)
                          ADD   ORCSTEN-KIZ-DAY  (IDY IDX5)  TO
                                      WRK-ORCSTEN-ZAIKAISU
                       END-PERFORM
      *
                       PERFORM   RECE64-TENSOU3-SEC
      *
                       MOVE      10   TO   IDX5
                       PERFORM   RECE65-TENSOU3-SEC
                       PERFORM   RECE62-HENSYU3-SEC
      *
                       PERFORM   RECE64-WRITE-SEC
      *
                  END-PERFORM
      *
      *       END-IF
      *
           END-PERFORM
      *
           .
       RECE64-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆÃµ­»ö¹à¤ÎÊÔ½¸
      *****************************************************************
       TOKKI-HENSYU-SEC                 SECTION.
      *
           MOVE     SPACE            TO      WRK-TOKKI
      *
           EVALUATE    ACCT-SRYCDTOTAL
               WHEN    CONS-TOKKI1
                   MOVE    "01"      TO      WRK-TOKKI
               WHEN    CONS-TOKKI2
                   MOVE    "02"      TO      WRK-TOKKI
               WHEN    CONS-TOKKI3
                   MOVE    "03"      TO      WRK-TOKKI
               WHEN    CONS-TOKKI4
                   MOVE    "04"      TO      WRK-TOKKI
               WHEN    CONS-TOKKI5
                   MOVE    "05"      TO      WRK-TOKKI
               WHEN    CONS-TOKKI7
                   MOVE    "07"      TO      WRK-TOKKI
               WHEN    CONS-TOKKI8
                   MOVE    "08"      TO      WRK-TOKKI
               WHEN    CONS-TOKKI9
                   MOVE    "09"      TO      WRK-TOKKI
               WHEN    CONS-TOKKI10
                   MOVE    "10"      TO      WRK-TOKKI
               WHEN    CONS-TOKKI11
                   MOVE    "11"      TO      WRK-TOKKI
               WHEN    CONS-TOKKI12
                   MOVE    "12"      TO      WRK-TOKKI
           END-EVALUATE
      *
      *    ´û¤ËÆ±¤¸ÆÃµ­»ö¹à¥³¡¼¥É¤¬ÊÔ½¸ºÑ¤ß¤Î¤È¤­¤Ï¥»¥Ã¥È¤·¤Ê¤¤
           PERFORM VARYING TOKKI-IDX  FROM    1   BY  1
                   UNTIL ( TOKKI-IDX       >   5     )
               IF      RECE62-TOKKI(TOKKI-IDX)   =   SPACE
                   MOVE    WRK-TOKKI       TO  RECE62-TOKKI (TOKKI-IDX)
                   MOVE    5               TO  TOKKI-IDX
               ELSE
                   IF      WRK-TOKKI     =   RECE62-TOKKI   (TOKKI-IDX)
                       MOVE    5                 TO  TOKKI-IDX
                   END-IF 
               END-IF
           END-PERFORM
           .
       TOKKI-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ÁªÄê³°Çñ»þ¤Î·¸¿ô¼èÆÀ
      **************************************************************
       SENTEI-KEISU-GET-SEC           SECTION.
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
       SENTEI-KEISU-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡´ðËÜÎÁ¤Î¼èÆÀ
      **************************************************************
       NYUINKHN-GET-SEC           SECTION.
      *
           MOVE    ZERO               TO   WRK-KHNTEN
                                           WRK-NYUKHNKSNKBN    
                                           WRK-KSNTEN
      *
           MOVE    ZERO               TO   WRK-SENTEI-KHNTEN
                                           WRK-SENTEI-ZAITEN
                                           WRK-SENTEI-SRYCD
                                           WRK-SENTEI-SRYCD2
                                           NYUINACT-IDX
      *
           MOVE    SPACE              TO   WRK-GAIHAKU-SRYCD
           MOVE    ZERO               TO   WRK-GAIHAKU-KHNTEN
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID       TO   NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "NYUINACT-KEY9"    TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC             =    ZERO
               MOVE    "NYUINACT-KEY9"     TO   ORC-DBPATH
               PERFORM NYUINACT-READ-SEC
           ELSE
               MOVE    1              TO   FLG-NYUINACT
           END-IF
           IF      FLG-NYUINACT        =   ZERO
               MOVE      ZERO         TO   FLG-NYUKHN  
               PERFORM   VARYING  IDX2  FROM 1  BY  1
                 UNTIL  (FLG-NYUKHN         =      1)    OR
                        (IDX2               >      5)    OR
                        (NSRY-SRYCD(IDX2)   =  SPACE)
                     MOVE    NSRY-SRYCD(IDX2)    TO  WRK-SRYCD
                     MOVE    NACCT-SRYYM         TO  WK-TAIHI-SRYYM
                     MOVE    "01"                TO  WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF   FLG-TENSU      =    ZERO
                       IF   TNS-DATAKBN    =    1
                          MOVE   TNS-TEN   TO   WRK-KHNTEN
      *                   ½é´ü²Ã»»¡ÊµÚ¤ÓÄ¹´ü¸º»»¡Ë¤ÎÅÀ¿ô¼èÆÀ
                          COMPUTE   WRK-KSNTEN  =   NACCT-ZAITEN
                                                -   WRK-KHNTEN
                          MOVE   TNS-NYUKHNKSNKBN                  
                                           TO   WRK-NYUKHNKSNKBN   
                          MOVE   NSRY-SRYCD(IDX2)
                                           TO   WRK-SENTEI-SRYCD
      *                   ´Ç¸îÈæÎ¨´ð½àÌ¤Ã£»þ¤ÎÆþ±¡´ðËÜÅÀ¿ô¤Î¼èÆÀ
                          PERFORM  GAIHAKU-NYUINKHN-SET-SEC
                          IF   WRK-GAIHAKU-KHNTEN  NOT =  ZERO
                                 MOVE   WRK-GAIHAKU-KHNTEN
                                               TO   WRK-KHNTEN
                          END-IF     
                          MOVE   IDX2      TO   NYUINACT-IDX
                          MOVE   1         TO   FLG-NYUKHN
                       END-IF
                     END-IF                  
               END-PERFORM
      *
               IF    NYUINACT-IDX    NOT   =   ZERO
      *
                 MOVE    SPACE                TO  SENTEICDCHG-REC
                 INITIALIZE                       SENTEICDCHG-REC
                 MOVE    NSRY-SRYCD(NYUINACT-IDX)
                                              TO  SENCHG-IPNSRYCD
                 MOVE    SENTEICDCHG-REC      TO  MCPDATA-REC
                 MOVE    "DBSELECT"           TO  MCP-FUNC
                 MOVE    "SENTEICDCHG-KEY2"   TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"         USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
                 IF      MCP-RC             =    ZERO
                     MOVE  "SENTEICDCHG-KEY2" TO  ORC-DBPATH
                     PERFORM  SENTEICDCHG-READ-SEC
                 ELSE
                     MOVE    1                TO  FLG-SENTEICDCHG
                 END-IF
      *
                 IF  FLG-SENTEICDCHG   =   ZERO
      *
                    MOVE     SENCHG-SENTEISRYCD  TO   WRK-SRYCD
                    MOVE     NACCT-SRYYM         TO   WK-TAIHI-SRYYM
                    MOVE     "01"                TO   WK-TAIHI-SRYD
                    PERFORM  TENSU-TBL-SEC
                    IF       FLG-TENSU      =    ZERO
                       MOVE     TNS-TEN          TO   WRK-SENTEI-KHNTEN
                       COMPUTE  WRK-SENTEI-ZAITEN  =  WRK-SENTEI-KHNTEN
                                                   +  WRK-KSNTEN
                       MOVE  SENCHG-SENTEISRYCD  TO   WRK-SENTEI-SRYCD2
                    END-IF  
      *
                 END-IF
      *
                 MOVE   "SENTEICDCHG-KEY2"       TO  WK-PATH
                 PERFORM   CSR-CLOSE-SEC
      *
               END-IF
      *
           END-IF
      *
           MOVE   "NYUINACT-KEY9"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       NYUINKHN-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ
      **************************************************************
       GAIHAKU-NYUINKHN-SET-SEC           SECTION.
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
           END-EVALUATE
      *
           IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
               MOVE    WRK-GAIHAKU-SRYCD   TO  WRK-SRYCD
               MOVE    NACCT-SRYYM         TO  WK-TAIHI-SRYYM
               MOVE    "01"                TO  WK-TAIHI-SRYD
               PERFORM    TENSU-TBL-SEC
               IF   FLG-TENSU      =    ZERO
                     MOVE     TNS-TEN      TO  WRK-GAIHAKU-KHNTEN
               END-IF                  
           END-IF
      *
           .
       GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ¤ÎÆþ±¡´ðËÜÎÁ·×»»
      **************************************************************
       NYUINACCT-GET-SEC           SECTION.
      *
      *    Æþ±¡ÎÁ¤È³°Çñ¤òÈæ³Ó¤·¤ÆÅÀ¿ô»»Äê
      *
           EVALUATE   WRK-DAY22-NYUTENTTLKBN(IDY)
               WHEN   903
                  IF    WRK-DAY1-SENTEI(IDY)   NOT  =  ZERO
                    EVALUATE   WRK-DAY22(IDY)
                     WHEN   1
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)
                                              *  WRK-SENTEI-KEISU)
                                                        +  0.5
                         MOVE    1     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                     WHEN   3
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  
                                              *  WRK-SENTEI-KEISU)
                                                        +  0.5
                         MOVE    3     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                     WHEN   4
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
      ****                       (WRK-DAY1-KHNTEN(IDY)  *  0.15)
                                 (WRK-DAY1-KHNTEN(IDY)  
                                              *  WRK-SENTEI-KEISU)
                                                        +  0.5
                         MOVE    5     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                    END-EVALUATE
                  ELSE
                    EVALUATE   WRK-DAY22(IDY)
                     WHEN   1
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.15)
                                                        +  0.5
                         MOVE    1     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                     WHEN   2
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.3)
                                                        +  0.5
                         MOVE    2     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                     WHEN   4
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.15)
                                                        +  0.5
                         MOVE    4     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                    END-EVALUATE
                  END-IF
               WHEN   920
                  IF    WRK-DAY1-SENTEI(IDY)   NOT  =  ZERO
                    EVALUATE   WRK-DAY22(IDY)
                     WHEN   1
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)
                                              *  WRK-SENTEI-KEISU)
                                                        +  0.5
                         MOVE    6     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                     WHEN   3
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  
                                              *  WRK-SENTEI-KEISU)
                                                        +  0.5
                         MOVE    8     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                     WHEN   4
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
      ****                       (WRK-DAY1-KHNTEN(IDY)  *  0.15)
                                 (WRK-DAY1-KHNTEN(IDY)  
                                              *  WRK-SENTEI-KEISU)
                                                        +  0.5
                         MOVE   10     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                    END-EVALUATE
                  ELSE
                    EVALUATE   WRK-DAY22(IDY)
                     WHEN   1
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.15)
                                                        +  0.5
                         MOVE    6     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                     WHEN   2
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.3)
                                                        +  0.5
                         MOVE    7     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                     WHEN   4
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.15)
                                                        +  0.5
                         MOVE    9     TO    SYUKEI-IDX
                         PERFORM    NYUINACCT-SYU-SEC
                    END-EVALUATE
                  END-IF
           END-EVALUATE
      *
           .
       NYUINACCT-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ¤ÎÆþ±¡´ðËÜÎÁ½¸·×
      **************************************************************
       NYUINACCT-SYU-SEC           SECTION.
      *
           MOVE     ZERO     TO    FLG-ARI
           PERFORM  VARYING  SYUKEI-IDX2    FROM    1   BY  1
                  UNTIL  SYUKEI-IDX2           > 10
                   OR    WRK-GAI-DAY(SYUKEI-IDX SYUKEI-IDX2) = ZERO
                   OR    FLG-ARI               = 1
              IF  WRK-GAI-ZAITEN(SYUKEI-IDX SYUKEI-IDX2)
                                            =  WRK-DAY22-NYUKHNRYO(IDY)
                 ADD   1  TO   WRK-GAI-ZAIKAISU(SYUKEI-IDX SYUKEI-IDX2)
                 MOVE  1  TO                                            
                           WRK-GAI-KAISU   (SYUKEI-IDX SYUKEI-IDX2 IDY) 
                 MOVE  1  TO   FLG-ARI
              END-IF
           END-PERFORM  
           IF  FLG-ARI  =  ZERO  
                 MOVE  IDY 
                          TO   WRK-GAI-DAY     (SYUKEI-IDX SYUKEI-IDX2)
                 MOVE  WRK-DAY22-NYUKHNRYO(IDY)
                          TO   WRK-GAI-ZAITEN  (SYUKEI-IDX SYUKEI-IDX2) 
                 ADD   1  TO   WRK-GAI-ZAIKAISU(SYUKEI-IDX SYUKEI-IDX2)
                 MOVE  1  TO                                            
                           WRK-GAI-KAISU   (SYUKEI-IDX SYUKEI-IDX2 IDY) 
           END-IF
      *
           .
      *
       NYUINACCT-SYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ¤ÎÆþ±¡´ðËÜÎÁÊÔ½¸
      **************************************************************
       NYUINACCT-HEN-SEC           SECTION.
      *
           MOVE     ZERO     TO     SYUKEI-IDX3
      *
           PERFORM    VARYING  SYUKEI-IDX     FROM    1   BY  1
                  UNTIL  SYUKEI-IDX            > 10
      *
             PERFORM  VARYING  SYUKEI-IDX2    FROM    1   BY  1
                  UNTIL  SYUKEI-IDX2           > 10
                   OR    WRK-GAI-DAY(SYUKEI-IDX SYUKEI-IDX2) = ZERO
      *
               ADD      1      TO     SYUKEI-IDX3
      *
               MOVE   WRK-GAI-DAY(SYUKEI-IDX SYUKEI-IDX2)
                                    TO   WRK-GAI2-DAY     (SYUKEI-IDX3)
               EVALUATE  SYUKEI-IDX
                  WHEN     1
                     MOVE   "190107290" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     2
                     MOVE   "190107390" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     3
                     MOVE   "190111590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     4
                     PERFORM  NYUINACCT-HEN-TAKIKAN-SEC
                  WHEN     5
                     PERFORM  NYUINACCT-HEN-TAKIKAN2-SEC
                  WHEN     6
                     MOVE   "190107490" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     7
                     MOVE   "190107590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     8
                     MOVE   "190111590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     9
                     PERFORM  NYUINACCT-HEN-TAKIKAN-SEC
                  WHEN    10
                     PERFORM  NYUINACCT-HEN-TAKIKAN2-SEC
               END-EVALUATE
               MOVE   WRK-GAI-ZAITEN(SYUKEI-IDX SYUKEI-IDX2)
                                    TO   WRK-GAI2-ZAITEN  (SYUKEI-IDX3)
               MOVE   WRK-GAI-ZAIKAISU(SYUKEI-IDX SYUKEI-IDX2)
                                    TO   WRK-GAI2-ZAIKAISU(SYUKEI-IDX3)
               EVALUATE  SYUKEI-IDX
                  WHEN     1
                  WHEN     2
                  WHEN     3
                  WHEN     4
                  WHEN     5
                     MOVE    1      TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                  WHEN     6
                  WHEN     7
                  WHEN     8
                  WHEN     9
                  WHEN    10
                     MOVE    2      TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
               END-EVALUATE
               MOVE  WRK-GAI-KAISU-T(SYUKEI-IDX SYUKEI-IDX2)            
                                    TO   WRK-GAI2-KAISU-T(SYUKEI-IDX3)  
      *
             END-PERFORM
      *
           END-PERFORM  
      *
      *    ÆüÉÕ½ç¤ËÊÂ¤ÓÂØ¤¨
      *
           PERFORM    VARYING   SYUKEI-IDX     FROM    1   BY  1
                  UNTIL  ( SYUKEI-IDX                   >   20  )
                    OR   ( WRK-GAI2-DAY(SYUKEI-IDX)     =  ZERO )
      *
             COMPUTE    SYUKEI-IDX2  =  SYUKEI-IDX  +  1
      *
             PERFORM  VARYING  SYUKEI-IDX3  FROM  SYUKEI-IDX2  BY  1
                        UNTIL  ( SYUKEI-IDX3                >   20  )
                          OR   ( WRK-GAI2-DAY(SYUKEI-IDX3)  =  ZERO )
      *
                IF       WRK-GAI2-DAY(SYUKEI-IDX)  >
                                           WRK-GAI2-DAY(SYUKEI-IDX3)
                   MOVE  WRK-GAI-T2(SYUKEI-IDX)  
                                           TO  WRK-GAI-DATA
                   MOVE  WRK-GAI-T2(SYUKEI-IDX3)  
                                           TO  WRK-GAI-T2(SYUKEI-IDX)
                   MOVE  WRK-GAI-DATA      TO  WRK-GAI-T2(SYUKEI-IDX3)
                END-IF
      *
             END-PERFORM
      *
           END-PERFORM
      *
           .
      *
       NYUINACCT-HEN-EXT.
           EXIT.
      *
      **************************************************************
      *    Â¾°åÎÅµ¡´Ø¼õ¿Ç»þ¤Î¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ
      **************************************************************
       NYUINACCT-HEN-TAKIKAN-SEC           SECTION.
      *
           MOVE   WRK-GAI2-DAY(SYUKEI-IDX3)   TO   SYUKEI-IDX4
      *
           EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
               WHEN   099  THRU  100 
               WHEN   201  THRU  219
               WHEN   221  THRU  222
               WHEN   224
               WHEN   227  THRU  228
                  MOVE   "190108790" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
               WHEN   013  THRU  021
               WHEN   101  THRU  109
                  MOVE   "190108890" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
               WHEN   075  THRU  080
                  MOVE   "190108990" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
               WHEN   085  THRU  086
               WHEN   132  THRU  133
                  MOVE   "190109090" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
           END-EVALUATE
           .
      *
       NYUINACCT-HEN-TAKIKAN-EXT.
           EXIT.
      *
      **************************************************************
      *    Â¾°åÎÅµ¡´Ø¼õ¿Ç»þ¤Î¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ¡ÊÁªÄêÎÅÍÜ¡Ë
      **************************************************************
       NYUINACCT-HEN-TAKIKAN2-SEC           SECTION.
      *
           MOVE   WRK-GAI2-DAY(SYUKEI-IDX3)   TO   SYUKEI-IDX4
      *
           EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
               WHEN   099  THRU  100 
               WHEN   201  THRU  219
               WHEN   221  THRU  222
               WHEN   224
               WHEN   227  THRU  228
                  MOVE   "190116590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
               WHEN   013  THRU  021
               WHEN   101  THRU  109
                  MOVE   "190116690" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
               WHEN   075  THRU  080
                  MOVE   "190116890" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
               WHEN   085  THRU  086
               WHEN   132  THRU  133
                  MOVE   "190116790" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
           END-EVALUATE
           .
      *
       NYUINACCT-HEN-TAKIKAN2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î½¸·×
      **************************************************************
       NYUINACCT-SYU2-SEC           SECTION.
      *
           EVALUATE   WRK-DAY24(IDY)
              WHEN  1
                EVALUATE   WRK-SHOKUJIRYOYO
                  WHEN  1
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD(IDY 1)
                     MOVE  "197000110"    TO   WRK-SRYCD
                     MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                     END-IF
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN  =  1
                        MOVE  "197000270"    TO   WRK-SKJ-SRYCD(IDY 2)
                        MOVE  "197000270"    TO   WRK-SRYCD
                        MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                        END-IF
                     END-IF
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        MOVE  "197000570"    TO   WRK-SKJ-SRYCD(IDY 4)
                        MOVE  "197000570"    TO   WRK-SRYCD
                        MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                        END-IF
                     END-IF
      *
                  WHEN  2
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     MOVE  "197000710"    TO   WRK-SKJ-SRYCD(IDY 1)
                     MOVE  "197000710"    TO   WRK-SRYCD
                     MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                     END-IF
                END-EVALUATE
              WHEN  2
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD(IDY 1)
                     MOVE  "197000110"    TO   WRK-SRYCD
                     MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                     END-IF
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN  =  1
                        MOVE  "197000270"    TO   WRK-SKJ-SRYCD(IDY 2)
                        MOVE  "197000270"    TO   WRK-SRYCD
                        MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                        END-IF
                     END-IF
      *              ÆÃÊÌ¿©²Ã»»
                     MOVE  "197000470"    TO   WRK-SKJ-SRYCD(IDY 3)
                     MOVE  "197000470"    TO   WRK-SRYCD
                     MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                     END-IF
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        MOVE  "197000570"    TO   WRK-SKJ-SRYCD(IDY 4)
                        MOVE  "197000570"    TO   WRK-SRYCD
                        MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                        END-IF
                     END-IF
              WHEN  3
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD(IDY 1)
                     MOVE  "197000110"    TO   WRK-SRYCD
                     MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                     END-IF
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN  =  1
                        MOVE  "197000270"    TO   WRK-SKJ-SRYCD(IDY 2)
                        MOVE  "197000270"    TO   WRK-SRYCD
                        MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                        END-IF
                     END-IF
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        MOVE  "197000570"    TO   WRK-SKJ-SRYCD(IDY 4)
                        MOVE  "197000570"    TO   WRK-SRYCD
                        MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                        END-IF
                     END-IF
      *              Ê£¿ô¸¥Î©²Ã»»
                     MOVE  "197000670"    TO   WRK-SKJ-SRYCD(IDY 5)
                     MOVE  "197000670"    TO   WRK-SRYCD
                     MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                     END-IF
              WHEN  4
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD(IDY 1)
                     MOVE  "197000110"    TO   WRK-SRYCD
                     MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                     END-IF
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN  =  1
                        MOVE  "197000270"    TO   WRK-SKJ-SRYCD(IDY 2)
                        MOVE  "197000270"    TO   WRK-SRYCD
                        MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                        END-IF
                     END-IF
      *              ÆÃÊÌ¿©²Ã»»
                     MOVE  "197000470"    TO   WRK-SKJ-SRYCD(IDY 3)
                     MOVE  "197000470"    TO   WRK-SRYCD
                     MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                     END-IF
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        MOVE  "197000570"    TO   WRK-SKJ-SRYCD(IDY 4)
                        MOVE  "197000570"    TO   WRK-SRYCD
                        MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                        END-IF
                     END-IF
      *              Ê£¿ô¸¥Î©²Ã»»
                     MOVE  "197000670"    TO   WRK-SKJ-SRYCD(IDY 5)
                     MOVE  "197000670"    TO   WRK-SRYCD
                     MOVE   RECE62-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        ADD     TNS-TEN   TO   WRK-SKJ-ZAITEN(IDY)
                     END-IF
           END-EVALUATE
      *
           .
       NYUINACCT-SYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤ÎÊÔ½¸
      **************************************************************
       NYUINACCT-HEN2-SEC           SECTION.
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
                            UNTIL  SYUKEI-IDX3     >   5
                        IF  WRK-SKJ-SRYCD(SYUKEI-IDX SYUKEI-IDX3) 
                                             NOT =   ZERO
                           ADD   1  TO  WRK-SKJ2-MEISAISU(SYUKEI-IDX2)
                           ADD   WRK-SKJ-SRYCD(SYUKEI-IDX SYUKEI-IDX3)
                                    TO  WRK-SKJ2-SRYCDTOTAL(SYUKEI-IDX2)
                        END-IF
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
       NYUINACCT-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ì¤¼Â»Ü¸º»»³Û¼èÆÀ
      *****************************************************************
       MIJISI-GET-SEC                 SECTION.
      *
           PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                        UNTIL     GENSAN-IDX   >   4
                MOVE   ZERO   TO  WRK-SYS-MIJISI-ZAITEN(GENSAN-IDX)
           END-PERFORM
      *
           PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                        UNTIL     GENSAN-IDX   >   4
      *
              IF    WRK-SYS-MIJISI(GENSAN-IDX)  =   1    
                 EVALUATE    GENSAN-IDX
                   WHEN   2
      *
      ***            IF    RECE62-RJNHKNNUM    =    SPACE
                       MOVE    "190080570"   TO   WRK-SRYCD
      ***            ELSE
      ***              MOVE    "190799770"   TO   WRK-SRYCD
      ***            END-IF
                     MOVE    RECE62-SRYYM    TO   WK-TAIHI-SRYYM
                     MOVE    "01"            TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        MOVE    TNS-TEN   TO   WRK-SYS-MIJISI-ZAITEN
                                                      (GENSAN-IDX)
                     END-IF
      *
                   WHEN   3
      *
      ***            IF    RECE62-RJNHKNNUM    =    SPACE
                       MOVE    "190111690"   TO   WRK-SRYCD
      ***            ELSE
      ***              MOVE    "190809090"   TO   WRK-SRYCD
      ***            END-IF
                     MOVE    RECE62-SRYYM    TO   WK-TAIHI-SRYYM
                     MOVE    "01"            TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        MOVE    TNS-TEN   TO   WRK-SYS-MIJISI-ZAITEN
                                                      (GENSAN-IDX)
                     END-IF
      *
                   WHEN   4
      *
      ***            IF    RECE62-RJNHKNNUM    =    SPACE
                       MOVE    "190111790"   TO   WRK-SRYCD
      ***            ELSE
      ***              MOVE    "190809190"   TO   WRK-SRYCD
      ***            END-IF
                     MOVE    RECE62-SRYYM    TO   WK-TAIHI-SRYYM
                     MOVE    "01"            TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        MOVE    TNS-TEN   TO   WRK-SYS-MIJISI-ZAITEN
                                                      (GENSAN-IDX)
                     END-IF
      *
                 END-EVALUATE
              END-IF
      *
           END-PERFORM
      *
           .
       MIJISI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE64-REC
           INITIALIZE                           RECE64-REC
           MOVE    RECE62-HOSPID        TO      RECE64-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE64-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE64-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE64-PTID
           MOVE    RECE62-RECEKA        TO      RECE64-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE64-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE64-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE64-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE64-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE64-TEKSTYMD
           MOVE    ZERO                 TO      RECE64-KOHBUNRI
           MOVE    ACCT-SRYKBN          TO      RECE64-SRYKBN
           IF      ACCT-SRYKBN = "90" OR "92"
                 MOVE    "6"            TO      RECE64-ZAISKBKBN
           ELSE 
              IF   ACCT-SRYKBN = "97"
                 MOVE    "4"            TO      RECE64-ZAISKBKBN
              END-IF
           END-IF
           MOVE    ACCT-ZAINUM          TO      RECE64-ZAINUM
           MOVE    ZERO                 TO      RECE64-ZAIREN
           MOVE    RECE62-PTNUM         TO      RECE64-PTNUM
      *
           MOVE    ACCT-SRYKA           TO      RECE64-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE64-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE64-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE64-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE64-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE64-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE64-MEISAISU
           MOVE    ACCT-ZAIKAISU        TO      RECE64-ZAIKAISU
           MOVE    ACCT-SYUTEN1         TO      RECE64-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE64-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE64-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE64-KIZAITEN
           MOVE    ACCT-DRCD            TO      RECE64-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE64-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE64-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX3   FROM    1    BY   1
                       UNTIL     IDX3   >       4
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE64-KAISU (IDX3 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (ACCT-DAY(1 IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
           MOVE      ZERO     TO    FLG-OKIKAE
           IF   ( RECE62-AGE       >=  75        )
           OR   ( RECE62-BIRTHDAY  <  "19321001" )
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL    ( IDX2        >    31 )
                             OR     ( FLG-OKIKAE  =     1 )
                  IF  ( RECE62-BYOTOSYUBETU(IDX2) =  "02" OR "03" )
                  AND ( RECE64-KAISU(1 IDX2)  NOT =  ZERO         )
                        MOVE   1   TO   FLG-OKIKAE
                  END-IF    
               END-PERFORM
           END-IF
      *
           .
       RECE64-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE64-REC
           INITIALIZE                           RECE64-REC
           MOVE    RECE62-HOSPID        TO      RECE64-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE64-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE64-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE64-PTID
           MOVE    RECE62-RECEKA        TO      RECE64-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE64-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE64-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE64-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE64-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE64-TEKSTYMD
           MOVE    ZERO                 TO      RECE64-KOHBUNRI
           MOVE    NACCT-SRYKBN         TO      RECE64-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE64-ZAISKBKBN
      *
           MOVE    ZERO                 TO      WRK-ZAINUM-CNT
           IF      NACCT-ZAISKBKBN      =  "1"
             PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                            OR       WRK-ZAINUM-CNT  NOT =  ZERO
                 IF  WRK-DAY21(IDX2)   NOT =   ZERO
                     MOVE    IDX2       TO      WRK-ZAINUM-CNT
                 END-IF
             END-PERFORM
             COMPUTE WRK-ZAINUM  = 99999800  +  WRK-ZAINUM-CNT
             MOVE    WRK-ZAINUM         TO      RECE64-ZAINUM
           ELSE
             MOVE    NACCT-ZAINUM       TO      RECE64-ZAINUM
           END-IF
      *
           MOVE    CNT-ZAIREN           TO      RECE64-ZAIREN
           MOVE    RECE62-PTNUM         TO      RECE64-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE64-SRYKA
           MOVE    RECE62-HKNCOMBI(IDX1)
                                        TO      RECE64-HKNCOMBI
           MOVE    SPACE                TO      RECE64-JIHOKBN
           MOVE    NACCT-ZAITEN         TO      RECE64-ZAITEN
           MOVE    NACCT-MEISAISU       TO      RECE64-MEISAISU
           MOVE    CNT-DAY21            TO      RECE64-ZAIKAISU
           MOVE    NACCT-SYUTEN1        TO      RECE64-SYUTEN1
           MOVE    NACCT-SRYCDTOTAL     TO      RECE64-SRYCDTOTAL
           MOVE    NACCT-SURYOUTOTAL    TO      RECE64-SURYOUTOTAL
           MOVE    NACCT-SYUTEN2        TO      RECE64-SYUTEN2
           MOVE    NACCT-YKZTEN         TO      RECE64-YKZTEN
           MOVE    NACCT-KIZAITEN       TO      RECE64-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE64-DRCD
           MOVE    SPACE                TO      RECE64-ZAIREQFLG
           MOVE    ZERO                 TO      RECE64-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-DAY21(IDX2)  TO
                                            RECE64-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)   AND
                          (WRK-DAY21(IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       RECE64-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-TENSOU2-SEN-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE64-REC
           INITIALIZE                           RECE64-REC
           MOVE    RECE62-HOSPID        TO      RECE64-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE64-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE64-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE64-PTID
           MOVE    RECE62-RECEKA        TO      RECE64-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE64-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE64-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE64-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE64-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE64-TEKSTYMD
           MOVE    ZERO                 TO      RECE64-KOHBUNRI
           MOVE    NACCT-SRYKBN         TO      RECE64-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE64-ZAISKBKBN
      *
           MOVE        ZERO             TO      WRK-ZAINUM-CNT
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                            OR       WRK-ZAINUM-CNT  NOT =  ZERO
               IF  WRK-DAY21-SENTEI(IDX2)   NOT =   ZERO
                   MOVE    IDX2         TO      WRK-ZAINUM-CNT
               END-IF
           END-PERFORM
           COMPUTE WRK-ZAINUM   =  99999800  +  WRK-ZAINUM-CNT
           MOVE    WRK-ZAINUM           TO      RECE64-ZAINUM
      *
           MOVE    CNT-ZAIREN           TO      RECE64-ZAIREN
           MOVE    RECE62-PTNUM         TO      RECE64-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE64-SRYKA
           MOVE    RECE62-HKNCOMBI(IDX1)
                                        TO      RECE64-HKNCOMBI
           MOVE    SPACE                TO      RECE64-JIHOKBN
           MOVE    WRK-SENTEI-ZAITEN-W  TO      RECE64-ZAITEN
           MOVE    NACCT-MEISAISU       TO      RECE64-MEISAISU
           MOVE    CNT-DAY21-SENTEI     TO      RECE64-ZAIKAISU
           MOVE    WRK-SENTEI-ZAITEN-W  TO      RECE64-SYUTEN1
           COMPUTE RECE64-SRYCDTOTAL  =  NACCT-SRYCDTOTAL
                                      -  WRK-SENTEI-SRYCD-W
                                      +  WRK-SENTEI-SRYCD2-W
           MOVE    NACCT-SURYOUTOTAL    TO      RECE64-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE64-SYUTEN2
           MOVE    ZERO                 TO      RECE64-YKZTEN
           MOVE    ZERO                 TO      RECE64-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE64-DRCD
           MOVE    SPACE                TO      RECE64-ZAIREQFLG
           MOVE    ZERO                 TO      RECE64-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-DAY21-SENTEI(IDX2)  TO
                                            RECE64-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)   AND
                          (WRK-DAY21-SENTEI(IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       RECE64-TENSOU2-SEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-TENSOU2-GAI-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE64-REC
           INITIALIZE                           RECE64-REC
           MOVE    RECE62-HOSPID        TO      RECE64-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE64-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE64-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE64-PTID
           MOVE    RECE62-RECEKA        TO      RECE64-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE64-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE64-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE64-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE64-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE64-TEKSTYMD
           MOVE    ZERO                 TO      RECE64-KOHBUNRI
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1
              MOVE    "90"              TO      RECE64-SRYKBN
           ELSE
              MOVE    "92"              TO      RECE64-SRYKBN
           END-IF 
           MOVE    NACCT-ZAISKBKBN      TO      RECE64-ZAISKBKBN
           COMPUTE WRK-ZAINUM    =  99999900  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE64-ZAINUM
           MOVE    CNT-ZAIREN           TO      RECE64-ZAIREN
           MOVE    RECE62-PTNUM         TO      RECE64-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE64-SRYKA
           MOVE    RECE62-HKNCOMBI(IDX1)
                                        TO      RECE64-HKNCOMBI
           MOVE    SPACE                TO      RECE64-JIHOKBN
           MOVE    WRK-GAI2-ZAITEN(SYUKEI-IDX3) 
                                        TO      RECE64-ZAITEN
           MOVE    1                    TO      RECE64-MEISAISU
           MOVE    WRK-GAI2-ZAIKAISU(SYUKEI-IDX3)
                                        TO      RECE64-ZAIKAISU
           MOVE    WRK-GAI2-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE64-SYUTEN1
           MOVE    WRK-GAI2-SRYCD(SYUKEI-IDX3)
                                        TO      RECE64-SRYCDTOTAL
           MOVE    1                    TO      RECE64-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE64-SYUTEN2
           MOVE    ZERO                 TO      RECE64-YKZTEN
           MOVE    ZERO                 TO      RECE64-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE64-DRCD
           MOVE    SPACE                TO      RECE64-ZAIREQFLG
           MOVE    ZERO                 TO      RECE64-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-GAI2-KAISU(SYUKEI-IDX3 IDX2)             
                                        TO    RECE64-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)  
                   AND    (WRK-GAI2-KAISU(SYUKEI-IDX3 IDX2)             
                                                      NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       RECE64-TENSOU2-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-TENSOU2-SKJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE64-REC
           INITIALIZE                           RECE64-REC
           MOVE    RECE62-HOSPID        TO      RECE64-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE64-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE64-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE64-PTID
           MOVE    RECE62-RECEKA        TO      RECE64-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE64-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE64-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE64-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE64-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE64-TEKSTYMD
           MOVE    ZERO                 TO      RECE64-KOHBUNRI
           MOVE    NACCT-SRYKBN         TO      RECE64-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE64-ZAISKBKBN
           COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE64-ZAINUM
           MOVE    CNT-ZAIREN           TO      RECE64-ZAIREN
           MOVE    RECE62-PTNUM         TO      RECE64-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE64-SRYKA
           MOVE    RECE62-HKNCOMBI(IDX1)
                                        TO      RECE64-HKNCOMBI
           MOVE    SPACE                TO      RECE64-JIHOKBN
           MOVE    WRK-SKJ2-ZAITEN(SYUKEI-IDX3) 
                                        TO      RECE64-ZAITEN
           MOVE    WRK-SKJ2-MEISAISU(SYUKEI-IDX3)
                                        TO      RECE64-MEISAISU
           MOVE    WRK-SKJ2-ZAIKAISU(SYUKEI-IDX3)
                                        TO      RECE64-ZAIKAISU
           MOVE    WRK-SKJ2-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE64-SYUTEN1
           MOVE    WRK-SKJ2-SRYCDTOTAL(SYUKEI-IDX3)
                                        TO      RECE64-SRYCDTOTAL
           MOVE    WRK-SKJ2-MEISAISU(SYUKEI-IDX3)
                                        TO      RECE64-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE64-SYUTEN2
           MOVE    ZERO                 TO      RECE64-YKZTEN
           MOVE    ZERO                 TO      RECE64-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE64-DRCD
           MOVE    SPACE                TO      RECE64-ZAIREQFLG
           MOVE    ZERO                 TO      RECE64-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-SKJ2-KAISU(SYUKEI-IDX3 IDX2)             
                                        TO   RECE64-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)
                   AND    (WRK-SKJ2-KAISU(SYUKEI-IDX3 IDX2)             
                                                     NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       RECE64-TENSOU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-TENSOU2-MIJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE64-REC
           INITIALIZE                           RECE64-REC
           MOVE    RECE62-HOSPID        TO      RECE64-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE64-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE64-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE64-PTID
           MOVE    RECE62-RECEKA        TO      RECE64-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE64-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE64-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE64-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE64-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE64-TEKSTYMD
           MOVE    ZERO                 TO      RECE64-KOHBUNRI
           IF      FLG-MIJISI2   =   1
              MOVE    "90"              TO      RECE64-SRYKBN
           ELSE
              MOVE    "92"              TO      RECE64-SRYKBN
           END-IF 
           MOVE    "6"                  TO      RECE64-ZAISKBKBN
           IF      FLG-MIJISI2   =   1
             COMPUTE WRK-ZAINUM    =  99999960  +  SYUKEI-IDX3
           ELSE
             COMPUTE WRK-ZAINUM    =  99999970  +  SYUKEI-IDX3
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE64-ZAINUM
           MOVE    CNT-ZAIREN           TO      RECE64-ZAIREN
           MOVE    RECE62-PTNUM         TO      RECE64-PTNUM
      *
           MOVE    "00"                 TO      RECE64-SRYKA
           MOVE    WRK-MIJ-HKNCOMBI(IDX1)
                                        TO      RECE64-HKNCOMBI
           MOVE    SPACE                TO      RECE64-JIHOKBN
           EVALUATE   SYUKEI-IDX3
               WHEN  2
                 MOVE    WRK-SYS-MIJISI-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE64-ZAITEN
                 MOVE    1              TO      RECE64-MEISAISU
                 IF      FLG-MIJISI2   =   1
                    MOVE    WRK-MIJ-CNT90(IDX1 2)
                                        TO      RECE64-ZAIKAISU
                 ELSE
                    MOVE    WRK-MIJ-CNT92(IDX1 2)
                                        TO      RECE64-ZAIKAISU
                 END-IF
                 MOVE    WRK-SYS-MIJISI-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE64-SYUTEN1
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    190080570   TO      RECE64-SRYCDTOTAL
      ***        ELSE
      ***           MOVE    190799770   TO      RECE64-SRYCDTOTAL
      ***        END-IF
               WHEN  3
                 MOVE    WRK-SYS-MIJISI-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE64-ZAITEN
                 MOVE    1              TO      RECE64-MEISAISU
                 IF      FLG-MIJISI2   =   1
                    MOVE    WRK-MIJ-CNT90(IDX1 3)
                                        TO      RECE64-ZAIKAISU
                 ELSE
                    MOVE    WRK-MIJ-CNT92(IDX1 3)
                                        TO      RECE64-ZAIKAISU
                 END-IF
                 MOVE    WRK-SYS-MIJISI-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE64-SYUTEN1
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    190111690   TO      RECE64-SRYCDTOTAL
      ***        ELSE
      ***           MOVE    190809090   TO      RECE64-SRYCDTOTAL
      ***        END-IF
               WHEN  4
                 MOVE    WRK-SYS-MIJISI-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE64-ZAITEN
                 MOVE    1              TO      RECE64-MEISAISU
                 IF      FLG-MIJISI2   =   1
                    MOVE    WRK-MIJ-CNT90(IDX1 4)
                                        TO      RECE64-ZAIKAISU
                 ELSE
                    MOVE    WRK-MIJ-CNT92(IDX1 4)
                                        TO      RECE64-ZAIKAISU
                 END-IF
                 MOVE    WRK-SYS-MIJISI-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE64-SYUTEN1
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    190111790   TO      RECE64-SRYCDTOTAL
      ***        ELSE
      ***           MOVE    190809190   TO      RECE64-SRYCDTOTAL
      ***        END-IF
           END-EVALUATE
           MOVE    1                    TO      RECE64-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE64-SYUTEN2
           MOVE    ZERO                 TO      RECE64-YKZTEN
           MOVE    ZERO                 TO      RECE64-KIZAITEN
           MOVE    SPACE                TO      RECE64-DRCD
           MOVE    SPACE                TO      RECE64-ZAIREQFLG
           MOVE    ZERO                 TO      RECE64-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           IF      FLG-MIJISI2   =   1
              PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-MIJ-DAY90(IDX1 SYUKEI-IDX3 IDX2)
                                      TO      RECE64-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)   
                   AND    (WRK-MIJ-DAY90(IDX1 SYUKEI-IDX3 IDX2)
                                              NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
              END-PERFORM
           ELSE
              PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-MIJ-DAY92(IDX1 SYUKEI-IDX3 IDX2)
                                      TO      RECE64-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)   
                   AND    (WRK-MIJ-DAY92(IDX1 SYUKEI-IDX3 IDX2)
                                              NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
              END-PERFORM
           END-IF
      *
           .
       RECE64-TENSOU2-MIJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE64-REC
           INITIALIZE                           RECE64-REC
           MOVE    RECE62-HOSPID        TO      RECE64-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE64-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE64-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE64-PTID
           MOVE    RECE62-RECEKA        TO      RECE64-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE64-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE64-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE64-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE64-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE64-TEKSTYMD
           MOVE    ZERO                 TO      RECE64-KOHBUNRI
           MOVE    "33"                 TO      RECE64-SRYKBN
           MOVE    SPACE                TO      RECE64-ZAISKBKBN
           IF    FLG-SIKIBETU  =  1
             COMPUTE WRK-ZAINUM    =  99999700  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99999740  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE64-ZAINUM
           MOVE    CNT-ZAIREN           TO      RECE64-ZAIREN
           MOVE    RECE62-PTNUM         TO      RECE64-PTNUM
      *
           MOVE    "00"                 TO      RECE64-SRYKA
           MOVE    RECE62-HKNCOMBI (IDX1)
                                        TO      RECE64-HKNCOMBI
           MOVE    SPACE                TO      RECE64-JIHOKBN
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE64-ZAITEN
           ELSE
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)  
                                        TO      RECE64-ZAITEN
           END-IF
           MOVE    ZERO                 TO      RECE64-SRYCDTOTAL
           MOVE    ZERO                 TO      RECE64-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE64-MEISAISU
           IF    FLG-SIKIBETU  =  1
             MOVE  1                    TO      RECE64-ZAIKAISU
           ELSE
             MOVE  WRK-ORCSTEN-ZAIKAISU TO      RECE64-ZAIKAISU
           END-IF
           MOVE    ZERO                 TO      RECE64-SYUTEN1
           MOVE    ZERO                 TO      RECE64-SYUTEN2
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE64-YKZTEN
             MOVE  ZERO                 TO      RECE64-KIZAITEN
           ELSE
             MOVE  ZERO                 TO      RECE64-YKZTEN
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)
                                        TO      RECE64-KIZAITEN
           END-IF
           MOVE    SPACE                TO      RECE64-DRCD
           MOVE    SPACE                TO      RECE64-ZAIREQFLG
           MOVE    ZERO                 TO      RECE64-TEIGENRATE
      *
           IF    FLG-SIKIBETU  =  1
             MOVE    1                  TO      RECE64-KAISU (1 IDY)
             MOVE    IDY                TO      WK-TAIHI-DAY
           ELSE
             MOVE        ZERO           TO      WK-TAIHI-DAY
             MOVE        ZERO           TO      WK-TAIHI-FLG
             PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-ORCSTEN-DAY(IDX2)  TO
                                            RECE64-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (WRK-ORCSTEN-DAY(IDX2)  NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
             END-PERFORM
           END-IF
           .
       RECE64-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-HENSYU-SEC               SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
      ***  MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY9"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY9"    TO      ORC-DBPATH
               PERFORM  SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE    ZERO         TO      FLG-ZANHAIKI
                   MOVE    ZERO         TO      WRK-RENNUM
                   MOVE    ZERO         TO      FLG-TENTEKI
                   PERFORM   RECE65-TENSOU-SEC
                   IF   FLG-TENTEKI  =  ZERO
                     PERFORM RECE62-HENSYU-SEC
                     PERFORM RECE65-WRITE-SEC
                     MOVE  1            TO      FLG-WRITE65
                   END-IF
                   IF   FLG-ZANHAIKI  =  1
                      PERFORM  RECE65-REWRITE-SEC
                   END-IF
                   MOVE     "SRYACT-KEY9"    TO   ORC-DBPATH
                   PERFORM   SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE   "SRYACT-KEY9"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       RECE65-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-HENSYU2-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID       TO   NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "NYUINACT-KEY9"    TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC               =       ZERO
      *
               MOVE    "NYUINACT-KEY9"  TO      ORC-DBPATH
               PERFORM  NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =    "1")
                   PERFORM   RECE65-TENSOU2-SEC
                   PERFORM   RECE65-WRITE-SEC
                   MOVE          "NYUINACT-KEY9"  TO   ORC-DBPATH
                   PERFORM        NYUINACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE   "NYUINACT-KEY9"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       RECE65-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-HENSYU2-SEN-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID       TO   NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "NYUINACT-KEY9"    TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC               =       ZERO
      *
               MOVE    "NYUINACT-KEY9"  TO      ORC-DBPATH
               PERFORM  NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =    "1")
                   PERFORM   RECE65-TENSOU2-SEN-SEC
                   PERFORM   RECE65-WRITE-SEC
                   MOVE          "NYUINACT-KEY9"  TO   ORC-DBPATH
                   PERFORM        NYUINACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE   "NYUINACT-KEY9"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       RECE65-HENSYU2-SEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-HENSYU2-GAI-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID       TO   NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "NYUINACT-KEY9"    TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "NYUINACT-KEY9"  TO      ORC-DBPATH
               PERFORM  NYUINACT-READ-SEC
           ELSE
               MOVE     1               TO      FLG-NYUINACT
           END-IF
           IF  FLG-NYUINACT   =   ZERO
               PERFORM   RECE65-TENSOU2-GAI-SEC
               PERFORM   RECE65-WRITE-SEC
           END-IF
      *
           MOVE   "NYUINACT-KEY9"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       RECE65-HENSYU2-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-HENSYU2-SKJ-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID       TO   NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "NYUINACT-KEY9"    TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "NYUINACT-KEY9"  TO      ORC-DBPATH
               PERFORM  NYUINACT-READ-SEC
           ELSE
               MOVE     1               TO      FLG-NYUINACT
           END-IF
           IF  FLG-NYUINACT   =   ZERO
               PERFORM   RECE65-TENSOU2-SKJ-SEC
               PERFORM   RECE65-WRITE-SEC
           END-IF
      *
           MOVE   "NYUINACT-KEY9"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       RECE65-HENSYU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE65-REC
           INITIALIZE                           RECE65-REC
           MOVE    RECE62-HOSPID        TO      RECE65-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE65-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE65-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE65-PTID
           MOVE    RECE62-RECEKA        TO      RECE65-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE65-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE65-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE65-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE65-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE65-TEKSTYMD
           MOVE    ZERO                 TO      RECE65-KOHBUNRI
           MOVE    SRY-ZAINUM           TO      RECE65-ZAINUM
           MOVE    ZERO                 TO      RECE65-ZAIREN
           MOVE    SRY-RENNUM           TO      RECE65-RENNUM
           MOVE    RECE62-PTNUM         TO      RECE65-PTNUM
      *
           MOVE    SRY-SRYKA            TO      RECE65-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE65-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE65-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE65-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
      *      Ï·¿ÍÀº¿ÀÉÂÅïÅù¤Î¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÃÖ¤­´¹¤¨
             IF   FLG-OKIKAE   =   1
                EVALUATE  SRY-SRYCD(IDX2)
      *           ·ì±Õ³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160061810"
                      MOVE  "160703610"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE64-SRYCDTOTAL  =  RECE64-SRYCDTOTAL
                                                  +  641800
      *           ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»¡Ê£±¡Ë
                  WHEN  "160170170"
                      MOVE  "160710070"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE64-SRYCDTOTAL  =  RECE64-SRYCDTOTAL
                                                  +  539900
      *           ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»¡Ê£²¡Ë
                  WHEN  "160161610"
                      MOVE  "160709310"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE64-SRYCDTOTAL  =  RECE64-SRYCDTOTAL
                                                  +  547700
      *           À¸²½³ØÅª¸¡ºº¡Ê£±¡ËÈ½ÃÇÎÁ
                  WHEN  "160061910"
                      MOVE  "160703710"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE64-SRYCDTOTAL  =  RECE64-SRYCDTOTAL
                                                  +  641800
      *           À¸²½³ØÅª¸¡ºº¡Ê£²¡ËÈ½ÃÇÎÁ
                  WHEN  "160062010"
                      MOVE  "160703810"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE64-SRYCDTOTAL  =  RECE64-SRYCDTOTAL
                                                  +  641800
      *           Ç¢¡¦ÊµÊØÅù¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160061710"
                      MOVE  "160703510"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE64-SRYCDTOTAL  =  RECE64-SRYCDTOTAL
                                                  +  641800
      *           ÈùÀ¸Êª³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160062210"
                      MOVE  "160704010"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE64-SRYCDTOTAL  =  RECE64-SRYCDTOTAL
                                                  +  641800
      *           ÌÈ±Ö³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160062110"
                      MOVE  "160703910"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE64-SRYCDTOTAL  =  RECE64-SRYCDTOTAL
                                                  +  641800
                END-EVALUATE
             END-IF
      *
      ***>   IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR  
      ***>                                        "6" OR  "7" OR
      ***>                                        "8"
      *
              IF  (  SRY-SRYSYUKBN            = "330" OR "331"      
                                                      OR "350"     )
              AND (( SRY-SRYCD(IDX2)(1:3)     = "059"             ) OR  
                   ( SRY-SRYCD(IDX2)(1:1)     = "7"               ) OR  
                   ( SRY-SRYCD(IDX2)          = CONS-ZANHAIKI     ) OR  
                   ( SRY-SRYCD(IDX2)(1:1)     = "6"               ))    
      *
               MOVE    1                  TO    FLG-TENTEKI             
      *
              ELSE                                                      
      *
               ADD     1                  TO    IDX4    
      *
               MOVE    SRY-SRYCD(IDX2)    TO    RECE65-SRYCD(IDX4)
      *
               IF      SRY-SRYCD(IDX2)(1:1)  =  "S"
                  COMPUTE   RECE64-MEISAISU  =  RECE64-MEISAISU
                                             -  1
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE65-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE65-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE65-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE65-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE65-INPUTNUM(IDX4)
               MOVE    SRY-SRYCD(IDX2)    TO    WRK-SRYCD
               MOVE    RECE65-SRYYM       TO    WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY       TO    WK-TAIHI-SRYD
               PERFORM    TENSU-TBL-SEC
               IF      TNS-SRYKBN         =     "80"
                   IF  TNS-NYUTENTTLKBN   =     130
                       MOVE   "13"        TO    RECE64-SRYKBN
                                                RECE65-SRYKBN
                   END-IF
               END-IF
      *
               PERFORM  RECE65-TENSOU-COMCNT-HEN-SEC
      *
               IF  SRY-SRYCD(IDX2)        =     CONS-ZANHAIKI           
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.999                   
                  COMPUTE RECE65-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE65-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE65-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE65-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE65-TANINAME(IDX4)   
                    IF    RECE65-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE65-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE65-SRYSURYO(IDX4 - 1) 
                    END-IF
                  END-IF                                                
               ELSE                                                     
                  MOVE    SRY-SRYSURYO(IDX2)                            
                                          TO    WRK-SRYSURYO            
                  MOVE    TNS-TANICD      TO    RECE65-TANICD(IDX4)
                                                WRK-TANICD              
                  MOVE    TNS-TANIMOJISU  TO    RECE65-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE65-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
               IF      TNS-TENSIKIBETU    =     1   OR   3   OR  8
                   MOVE  TNS-TENSIKIBETU  TO   RECE65-TENSIKIBETU(IDX4)
                   MOVE  TNS-TEN          TO    RECE65-TEN(IDX4)
               END-IF
               MOVE    TNS-NYUTENTTLKBN   TO    RECE65-TENTTLKBN(IDX4)
               IF      SRY-SRYCD    (IDX2)(1:1)     =   "8"
                  MOVE ZERO               TO    RECE65-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE65-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO    RECE65-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE65-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE65-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE65-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE65-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE65-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE65-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE65-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE65-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE65-KOUHYOJYUNNUM(IDX4)
               IF      SRY-SRYCD    (IDX2)(1:1)     =   "8"
               AND     SRY-INPUTNUM (IDX2)      NOT =   ZERO
                   MOVE    SRY-HOSPID           TO  PTCOM-HOSPID
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT   TO    RECE65-NAME(IDX4)
      *
                   MOVE   "1"                  TO    RECE64-COMCDFLG
               ELSE
                   MOVE    TNS-NAME            TO    RECE65-NAME(IDX4)
               END-IF
      *
               IF  RECE65-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   200   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE65-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE65-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
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
      *********        SET     TBL-SANSO           TO  1
      *                SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
      *                     AT  END
      *                         MOVE    ZERO            TO  FLG-SANSO-OK
      *                     WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
      *                                                          (TBL-SANSO)
      *                         MOVE    1               TO  FLG-SANSO-OK
      *                END-SEARCH
      *********        IF      FLG-SANSO-OK        =   "1"
                     IF      TNS-TEN         =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM RECE65-TENSOU-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE65-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE65-NAME(IDX4)(IDW:)   
                       COMPUTE RECE65-YUKOKETA(IDX4)
                                               =   RECE65-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF
              END-IF
      ***>   END-IF  
           END-PERFORM
      *
           IF    IDX4   =   ZERO
               MOVE    1     TO    FLG-TENTEKI
           END-IF
      *
           .
       RECE65-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤ÎÃ±²ÁÊÔ½¸½èÍý
      *****************************************************************
       RECE65-TENSOU-TANKA-HEN-SEC     SECTION.
      *
      ***  IF      TNS-TEN         =   ZERO
      ***      GO  TO  RECE65-TENSOU-TANKA-HEN-EXT  
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
       RECE65-TENSOU-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       RECE65-TENSOU-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE64-SRYKBN      =     "80"
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
                                            "180025010"   
                        MOVE   "1"    TO    RECE62-COMCNTFLG
                        MOVE   "1"    TO    RECE64-RIHABIRI-COMMENT
              END-IF
           END-IF
      *
      *
           IF      RECE64-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     CONS-YAKUKAN-COMMENT
                   MOVE   "1"   TO    RECE64-FLG-YAKUKAN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-NYUEIYO-COMMENT
                   MOVE   "1"   TO    RECE64-FLG-NYUEIYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUMON-COMMENT1  OR
                                            CONS-HOUMON-COMMENT2
                   MOVE   "1"   TO    RECE64-FLG-HOUMON-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO-COMMENT
                   MOVE   "1"   TO    RECE64-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO-COMMENT
                   MOVE   "1"   TO    RECE64-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-TAIIN-COMMENT
                   MOVE   "1"   TO    RECE64-FLG-TAIIN-COMMENT
              END-IF
           END-IF
      *
           IF      RECE64-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI-COMMENT
                   MOVE   "1"   TO    RECE64-FLG-HOUSIJI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE64-SRYKBN      =     "50"
              IF   RECE64-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE64-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE64-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SHUJUTU-COMMENT
                   MOVE   "1"   TO    RECE64-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF      RECE64-SRYKBN      =     "70"
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
                   MOVE   "1"   TO    RECE64-FLG-CTMRI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE64-SRYKBN      =     "80"
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSEI-COMMENT
                   MOVE   "1"   TO    RECE64-FLG-HOUSEI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE64-SRYKBN      =     "90"  OR  "92"
              IF   SRY-SRYCD(IDX2)    =     CONS-KAKURI-COMMENT
                   MOVE   "1"   TO    RECE64-FLG-KAKURI-COMMENT
              END-IF
           END-IF
      *
           .
       RECE65-TENSOU-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE65-REC
           INITIALIZE                           RECE65-REC
           MOVE    RECE62-HOSPID        TO      RECE65-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE65-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE65-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE65-PTID
           MOVE    RECE62-RECEKA        TO      RECE65-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE65-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE65-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE65-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE65-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE65-TEKSTYMD
           MOVE    ZERO                 TO      RECE65-KOHBUNRI
      *
           IF      WRK-ZAINUM-CNT    NOT =   ZERO
             COMPUTE WRK-ZAINUM   =  99999800  +  WRK-ZAINUM-CNT
             MOVE    WRK-ZAINUM         TO      RECE65-ZAINUM
           ELSE
             MOVE    NSRY-ZAINUM        TO      RECE65-ZAINUM
           END-IF
      *
           MOVE    CNT-ZAIREN           TO      RECE65-ZAIREN
           MOVE    NSRY-RENNUM          TO      RECE65-RENNUM
           MOVE    RECE62-PTNUM         TO      RECE65-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE65-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE65-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE65-SRYSYUKBN
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE65-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (NSRY-SRYCD(IDX2)  =   SPACE)
             IF      NSRY-SRYCD(IDX2)(1:1)    =   "0"  OR  "1" OR  
                                                  "8"
               ADD     1                   TO  IDX4    
               MOVE    NSRY-SRYCD(IDX2)    TO  RECE65-SRYCD(IDX4)
               MOVE    NSRY-SRYSURYO(IDX2) TO  RECE65-SRYSURYO(IDX4)
               MOVE    NSRY-SRYKAISU(IDX2) TO  RECE65-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE65-MEISKYFLG(IDX4)
               MOVE    NSRY-AUTOKBN(IDX2)  TO  RECE65-AUTOKBN(IDX4)
               MOVE    NSRY-INPUTNUM(IDX2) TO  RECE65-INPUTNUM(IDX4)
               MOVE    NSRY-SRYCD(IDX2)    TO  WRK-SRYCD
               MOVE    RECE65-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM    TENSU-TBL-SEC
               MOVE    TNS-TANICD          TO  RECE65-TANICD(IDX4)
               MOVE    TNS-TANIMOJISU      TO  RECE65-TANIMOJISU(IDX4)
               MOVE    TNS-TANINAME        TO  RECE65-TANINAME(IDX4)
               IF      TNS-TENSIKIBETU    =     1   OR   3  OR  8
                   MOVE  TNS-TENSIKIBETU   TO  RECE65-TENSIKIBETU(IDX4)
                   MOVE  TNS-TEN           TO  RECE65-TEN(IDX4)
               END-IF
               MOVE    TNS-NYUTENTTLKBN    TO  RECE65-TENTTLKBN(IDX4)
               MOVE    TNS-YUKOKETA        TO  RECE65-YUKOKETA(IDX4)
               MOVE    TNS-DATAKBN         TO  RECE65-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN       TO  RECE65-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN       TO  RECE65-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN       TO  RECE65-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO       TO  RECE65-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU        TO  RECE65-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE65-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE65-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE65-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE65-KOUHYOJYUNNUM(IDX4)
               IF      NSRY-SRYCD    (IDX2)(1:1)     =   "8"
               AND     NSRY-INPUTNUM (IDX2)      NOT =   ZERO
                   MOVE    NSRY-HOSPID           TO  PTCOM-HOSPID
                   MOVE    NSRY-PTID             TO  PTCOM-PTID
                   MOVE    NSRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    NSRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    NSRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT     TO  RECE65-NAME(IDX4)
               ELSE
                   MOVE    TNS-NAME              TO  RECE65-NAME(IDX4)
               END-IF
      *
             END-IF  
           END-PERFORM
      *
           .
       RECE65-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-TENSOU2-SEN-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE65-REC
           INITIALIZE                           RECE65-REC
           MOVE    RECE62-HOSPID        TO      RECE65-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE65-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE65-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE65-PTID
           MOVE    RECE62-RECEKA        TO      RECE65-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE65-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE65-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE65-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE65-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE65-TEKSTYMD
           MOVE    ZERO                 TO      RECE65-KOHBUNRI
           COMPUTE WRK-ZAINUM   =  99999800  +  WRK-ZAINUM-CNT
           MOVE    WRK-ZAINUM           TO      RECE65-ZAINUM
           MOVE    CNT-ZAIREN           TO      RECE65-ZAIREN
           MOVE    NSRY-RENNUM          TO      RECE65-RENNUM
           MOVE    RECE62-PTNUM         TO      RECE65-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE65-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE65-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE65-SRYSYUKBN
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE65-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (NSRY-SRYCD(IDX2)  =   SPACE)
             IF      NSRY-SRYCD(IDX2)(1:1)    =   "0"  OR  "1" OR  
                                                  "8"
               ADD     1                   TO  IDX4    
               IF    IDX2  =  1
                 MOVE    WRK-SENTEI-SRYCD2-W
                                           TO  RECE65-SRYCD(IDX4)
               ELSE
                 MOVE    NSRY-SRYCD(IDX2)  TO  RECE65-SRYCD(IDX4)
               END-IF
               MOVE    NSRY-SRYSURYO(IDX2) TO  RECE65-SRYSURYO(IDX4)
               MOVE    NSRY-SRYKAISU(IDX2) TO  RECE65-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE65-MEISKYFLG(IDX4)
               MOVE    NSRY-AUTOKBN(IDX2)  TO  RECE65-AUTOKBN(IDX4)
               MOVE    NSRY-INPUTNUM(IDX2) TO  RECE65-INPUTNUM(IDX4)
               IF    IDX2  =  1
                 MOVE    WRK-SENTEI-SRYCD2-W
                                           TO  WRK-SRYCD
               ELSE
                 MOVE    NSRY-SRYCD(IDX2)  TO  WRK-SRYCD
               END-IF
               MOVE    RECE65-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM    TENSU-TBL-SEC
               MOVE    TNS-TANICD          TO  RECE65-TANICD(IDX4)
               MOVE    TNS-TANIMOJISU      TO  RECE65-TANIMOJISU(IDX4)
               MOVE    TNS-TANINAME        TO  RECE65-TANINAME(IDX4)
               IF      TNS-TENSIKIBETU    =     1   OR   3  OR  8
                   MOVE  TNS-TENSIKIBETU   TO  RECE65-TENSIKIBETU(IDX4)
                   MOVE  TNS-TEN           TO  RECE65-TEN(IDX4)
               END-IF
               MOVE    TNS-NYUTENTTLKBN    TO  RECE65-TENTTLKBN(IDX4)
               MOVE    TNS-YUKOKETA        TO  RECE65-YUKOKETA(IDX4)
               MOVE    TNS-DATAKBN         TO  RECE65-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN       TO  RECE65-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN       TO  RECE65-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN       TO  RECE65-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO       TO  RECE65-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU        TO  RECE65-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE65-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE65-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE65-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE65-KOUHYOJYUNNUM(IDX4)
               IF      NSRY-SRYCD    (IDX2)(1:1)     =   "8"
               AND     NSRY-INPUTNUM (IDX2)      NOT =   ZERO
                   MOVE    NSRY-HOSPID           TO  PTCOM-HOSPID
                   MOVE    NSRY-PTID             TO  PTCOM-PTID
                   MOVE    NSRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    NSRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    NSRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT     TO  RECE65-NAME(IDX4)
               ELSE
                   MOVE    TNS-NAME              TO  RECE65-NAME(IDX4)
               END-IF
      *
             END-IF  
           END-PERFORM
      *
           .
       RECE65-TENSOU2-SEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-TENSOU2-GAI-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE65-REC
           INITIALIZE                           RECE65-REC
           MOVE    RECE62-HOSPID        TO      RECE65-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE65-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE65-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE65-PTID
           MOVE    RECE62-RECEKA        TO      RECE65-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE65-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE65-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE65-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE65-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE65-TEKSTYMD
           MOVE    ZERO                 TO      RECE65-KOHBUNRI
           COMPUTE WRK-ZAINUM    =  99999900  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE65-ZAINUM
           MOVE    CNT-ZAIREN           TO      RECE65-ZAIREN
           MOVE    NSRY-RENNUM          TO      RECE65-RENNUM
           MOVE    RECE62-PTNUM         TO      RECE65-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE65-SRYKA
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1
              MOVE    "90"              TO      RECE65-SRYKBN
           ELSE
              MOVE    "92"              TO      RECE65-SRYKBN
           END-IF 
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1
              MOVE    "900"             TO      RECE65-SRYSYUKBN
           ELSE
              MOVE    "920"             TO      RECE65-SRYSYUKBN
           END-IF 
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE65-JIHIMONEY
      *
           MOVE    WRK-GAI2-SRYCD(SYUKEI-IDX3)
                                        TO  RECE65-SRYCD(1)
           MOVE    1                    TO  RECE65-SRYSURYO(1)
           MOVE    ZERO                 TO  RECE65-SRYKAISU(1)
           MOVE    SPACE                TO  RECE65-MEISKYFLG(1)
           MOVE    SPACE                TO  RECE65-AUTOKBN(1)
           MOVE    ZERO                 TO  RECE65-INPUTNUM(1)
           MOVE    WRK-GAI2-SRYCD(SYUKEI-IDX3)
                                        TO  WRK-SRYCD
           MOVE    RECE65-SRYYM         TO  WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
           PERFORM    TENSU-TBL-SEC
           MOVE    TNS-TANICD          TO  RECE65-TANICD(1)
           MOVE    TNS-TANIMOJISU      TO  RECE65-TANIMOJISU(1)
           MOVE    TNS-TANINAME        TO  RECE65-TANINAME(1)
           IF      TNS-TENSIKIBETU    =     1   OR   3  OR  8
               MOVE  TNS-TENSIKIBETU   TO  RECE65-TENSIKIBETU(1)
               MOVE  TNS-TEN           TO  RECE65-TEN(1)
           END-IF
           MOVE    TNS-NYUTENTTLKBN    TO  RECE65-TENTTLKBN(1)
           MOVE    TNS-YUKOKETA        TO  RECE65-YUKOKETA(1)
           MOVE    TNS-DATAKBN         TO  RECE65-DATAKBN (1)
           MOVE    TNS-HOUKNSKBN       TO  RECE65-HOUKNSKBN(1)
           MOVE    TNS-KNSJISKBN       TO  RECE65-KNSJISKBN(1)
           MOVE    TNS-CDKBN-KBN       TO  RECE65-CDKBN-KBN(1)
           MOVE    TNS-CDKBN-SYO       TO  RECE65-CDKBN-SYO(1)
           MOVE    TNS-CDKBN-BU        TO  RECE65-CDKBN-BU(1)
           MOVE    TNS-CDKBN-KBNNUM   
                                  TO RECE65-CDKBN-KBNNUM(1)
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                  TO RECE65-CDKBN-KBNNUM-EDA(1)
           MOVE    TNS-CDKBN-KOUBAN
                                  TO RECE65-CDKBN-KOUBAN(1)
           MOVE    TNS-KOUHYOJYUNNUM  
                                  TO RECE65-KOUHYOJYUNNUM(1)
           MOVE    TNS-NAME            TO  RECE65-NAME(1)
      *
           .
       RECE65-TENSOU2-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-TENSOU2-SKJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE65-REC
           INITIALIZE                           RECE65-REC
           MOVE    RECE62-HOSPID        TO      RECE65-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE65-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE65-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE65-PTID
           MOVE    RECE62-RECEKA        TO      RECE65-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE65-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE65-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE65-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE65-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE65-TEKSTYMD
           MOVE    ZERO                 TO      RECE65-KOHBUNRI
           COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE65-ZAINUM
           MOVE    CNT-ZAIREN           TO      RECE65-ZAIREN
           MOVE    NSRY-RENNUM          TO      RECE65-RENNUM
           MOVE    RECE62-PTNUM         TO      RECE65-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE65-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE65-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE65-SRYSYUKBN
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE65-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    
      *
              IF   WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)  NOT =   ZERO
      *
                ADD     1                    TO    IDX4  
                MOVE    WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)
                                             TO  RECE65-SRYCD(IDX4)
                MOVE    1                    TO  RECE65-SRYSURYO(IDX4)
                MOVE    ZERO                 TO  RECE65-SRYKAISU(IDX4)
                MOVE    SPACE                TO  RECE65-MEISKYFLG(IDX4)
                MOVE    SPACE                TO  RECE65-AUTOKBN(IDX4)
                MOVE    ZERO                 TO  RECE65-INPUTNUM(IDX4)
                MOVE    WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)
                                             TO  WRK-SRYCD
                MOVE    RECE65-SRYYM         TO  WK-TAIHI-SRYYM
                MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
                PERFORM    TENSU-TBL-SEC
                MOVE    TNS-TANICD          TO  RECE65-TANICD(IDX4)
                MOVE    TNS-TANIMOJISU      TO  RECE65-TANIMOJISU(IDX4)
                MOVE    TNS-TANINAME        TO  RECE65-TANINAME(IDX4)
                IF      TNS-TENSIKIBETU    =     1   OR   3  OR  8
                    MOVE  TNS-TENSIKIBETU   TO  RECE65-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN           TO  RECE65-TEN(IDX4)
                END-IF
                MOVE    TNS-NYUTENTTLKBN    TO  RECE65-TENTTLKBN(IDX4)
                MOVE    TNS-YUKOKETA        TO  RECE65-YUKOKETA(IDX4)
                MOVE    TNS-DATAKBN         TO  RECE65-DATAKBN (IDX4)
                MOVE    TNS-HOUKNSKBN       TO  RECE65-HOUKNSKBN(IDX4)
                MOVE    TNS-KNSJISKBN       TO  RECE65-KNSJISKBN(IDX4)
                MOVE    TNS-CDKBN-KBN       TO  RECE65-CDKBN-KBN(IDX4)
                MOVE    TNS-CDKBN-SYO       TO  RECE65-CDKBN-SYO(IDX4)
                MOVE    TNS-CDKBN-BU        TO  RECE65-CDKBN-BU(IDX4)
                MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE65-CDKBN-KBNNUM(IDX4)
                MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE65-CDKBN-KBNNUM-EDA(IDX4)
                MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE65-CDKBN-KOUBAN(IDX4)
                MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE65-KOUHYOJYUNNUM(IDX4)
                MOVE    TNS-NAME            TO  RECE65-NAME(IDX4)
      *
              END-IF
      *
           END-PERFORM
      *
           .
       RECE65-TENSOU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-TENSOU2-MIJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE65-REC
           INITIALIZE                           RECE65-REC
           MOVE    RECE62-HOSPID        TO      RECE65-HOSPID
           MOVE    RECE62-SRYYM         TO      RECE65-SRYYM
           MOVE    RECE62-NYUGAIKBN     TO      RECE65-NYUGAIKBN
           MOVE    RECE62-PTID          TO      RECE65-PTID
           MOVE    RECE62-RECEKA        TO      RECE65-RECEKA
           MOVE    RECE62-TEISYUTUSAKI  TO      RECE65-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU   TO      RECE65-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY  TO      RECE65-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY   TO      RECE65-HOJOKBN
           MOVE    RECE62-TEKSTYMD      TO      RECE65-TEKSTYMD
           MOVE    ZERO                 TO      RECE65-KOHBUNRI
           IF      FLG-MIJISI2   =   1
             COMPUTE WRK-ZAINUM    =  99999960  +  SYUKEI-IDX3
           ELSE
             COMPUTE WRK-ZAINUM    =  99999970  +  SYUKEI-IDX3
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE65-ZAINUM
           MOVE    CNT-ZAIREN           TO      RECE65-ZAIREN
           MOVE    1                    TO      RECE65-RENNUM
           MOVE    RECE62-PTNUM         TO      RECE65-PTNUM
      *
           MOVE    "00"                 TO      RECE65-SRYKA
           IF      FLG-MIJISI2   =   1
              MOVE    "90"              TO      RECE65-SRYKBN
           ELSE
              MOVE    "92"              TO      RECE65-SRYKBN
           END-IF 
           IF      FLG-MIJISI2   =   1
              MOVE    "900"             TO      RECE65-SRYSYUKBN
           ELSE
              MOVE    "920"             TO      RECE65-SRYSYUKBN
           END-IF 
           MOVE    ZERO                 TO      RECE65-JIHIMONEY
      *
           EVALUATE   SYUKEI-IDX3
               WHEN  2
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    190080570   TO      RECE65-SRYCD(1)
      ***        ELSE
      ***           MOVE    190799770   TO      RECE65-SRYCD(1)
      ***        END-IF
               WHEN  3
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    190111690   TO      RECE65-SRYCD(1)
      ***        ELSE
      ***           MOVE    190809090   TO      RECE65-SRYCD(1)
      ***        END-IF
               WHEN  4
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    190111790   TO      RECE65-SRYCD(1)
      ***        ELSE
      ***           MOVE    190809190   TO      RECE65-SRYCD(1)
      ***        END-IF
           END-EVALUATE
      *
           MOVE    1                    TO  RECE65-SRYSURYO(1)
           MOVE    ZERO                 TO  RECE65-SRYKAISU(1)
           MOVE    SPACE                TO  RECE65-MEISKYFLG(1)
           MOVE    SPACE                TO  RECE65-AUTOKBN(1)
           MOVE    ZERO                 TO  RECE65-INPUTNUM(1)
           MOVE    RECE65-SRYCD(1)      TO  WRK-SRYCD
           MOVE    RECE65-SRYYM         TO  WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
           PERFORM    TENSU-TBL-SEC
           MOVE    TNS-TANICD          TO  RECE65-TANICD(1)
           MOVE    TNS-TANIMOJISU      TO  RECE65-TANIMOJISU(1)
           MOVE    TNS-TANINAME        TO  RECE65-TANINAME(1)
           IF      TNS-TENSIKIBETU    =     1   OR   3  OR  8
               MOVE  TNS-TENSIKIBETU   TO  RECE65-TENSIKIBETU(1)
               MOVE  TNS-TEN           TO  RECE65-TEN(1)
           END-IF
           MOVE    TNS-NYUTENTTLKBN    TO  RECE65-TENTTLKBN(1)
           MOVE    TNS-YUKOKETA        TO  RECE65-YUKOKETA(1)
           MOVE    TNS-DATAKBN         TO  RECE65-DATAKBN (1)
           MOVE    TNS-HOUKNSKBN       TO  RECE65-HOUKNSKBN(1)
           MOVE    TNS-KNSJISKBN       TO  RECE65-KNSJISKBN(1)
           MOVE    TNS-CDKBN-KBN       TO  RECE65-CDKBN-KBN(1)
           MOVE    TNS-CDKBN-SYO       TO  RECE65-CDKBN-SYO(1)
           MOVE    TNS-CDKBN-BU        TO  RECE65-CDKBN-BU(1)
           MOVE    TNS-CDKBN-KBNNUM   
                                       TO  RECE65-CDKBN-KBNNUM(1)
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  RECE65-CDKBN-KBNNUM-EDA(1)
           MOVE    TNS-CDKBN-KOUBAN
                                       TO  RECE65-CDKBN-KOUBAN(1)
           MOVE    TNS-KOUHYOJYUNNUM  
                                       TO  RECE65-KOUHYOJYUNNUM(1)
           MOVE    TNS-NAME            TO  RECE65-NAME(1)
      *
           .
       RECE65-TENSOU2-MIJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-TENSOU3-SEC                SECTION.
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
              MOVE    SPACE                TO   RECE65-REC
              INITIALIZE                        RECE65-REC
              MOVE    RECE62-HOSPID        TO   RECE65-HOSPID
              MOVE    RECE62-SRYYM         TO   RECE65-SRYYM
              MOVE    RECE62-NYUGAIKBN     TO   RECE65-NYUGAIKBN
              MOVE    RECE62-PTID          TO   RECE65-PTID
              MOVE    RECE62-RECEKA        TO   RECE65-RECEKA
              MOVE    RECE62-TEISYUTUSAKI  TO   RECE65-TEISYUTUSAKI
              MOVE    RECE62-RECESYUBETU   TO   RECE65-RECESYUBETU
              MOVE    RECE62-HKNJANUM-KEY  TO   RECE65-HKNJANUM
              MOVE    RECE62-HOJOKBN-KEY   TO   RECE65-HOJOKBN
              MOVE    RECE62-TEKSTYMD      TO   RECE65-TEKSTYMD
              MOVE    ZERO                 TO   RECE65-KOHBUNRI
              IF    FLG-SIKIBETU  =  1
                COMPUTE WRK-ZAINUM    =  99999700  +  IDY
              ELSE
                COMPUTE WRK-ZAINUM    =  99999740  +  IDY
              END-IF
              MOVE    WRK-ZAINUM           TO   RECE65-ZAINUM
              MOVE    CNT-ZAIREN           TO   RECE65-ZAIREN
              ADD     1                    TO   WRK-RENNUM
              MOVE    WRK-RENNUM           TO   RECE65-RENNUM
              MOVE    RECE62-PTNUM         TO   RECE65-PTNUM
      *
              MOVE    "00"                 TO   RECE65-SRYKA
              MOVE    "33"                 TO   RECE65-SRYKBN
              MOVE    "330"                TO   RECE65-SRYSYUKBN
              MOVE    ZERO                 TO   RECE65-JIHIMONEY
      *
              MOVE    ZERO                 TO      IDX4
              PERFORM
                     UNTIL  (IDX4      >=   5  )    OR
                            (IDX2      >   IDX5)    OR
                            (WRK-ORCSTEN-SRYCD(IDX2)  =   SPACE)
      *
                IF   WRK-ORCSTEN-SRYCD(IDX2)(1:1) =   "0" OR  "6" OR  
                                                      "7" 
      *
                  ADD   1                   TO  IDX4    
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  RECE65-SRYCD(IDX4)
                  MOVE  WRK-ORCSTEN-SURYO(IDX2) 
                                            TO  RECE65-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE65-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE65-MEISKYFLG(IDX4)
                  MOVE  SPACE               TO  RECE65-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE65-INPUTNUM(IDX4)
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  WRK-SRYCD
                  MOVE  RECE65-SRYYM        TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM    TENSU-TBL-SEC
                  IF  WRK-ORCSTEN-SRYCD(IDX2)  =  CONS-ZANHAIKI
                      MOVE   WRK-TANICD     TO  RECE65-TANICD(IDX4)
                      MOVE   WRK-TANIMOJISU TO  RECE65-TANIMOJISU(IDX4)
                      MOVE   WRK-TANINAME   TO  RECE65-TANINAME(IDX4)
                  ELSE
                      MOVE  TNS-TANICD      TO  RECE65-TANICD(IDX4)
                                                WRK-TANICD
                      MOVE  TNS-TANIMOJISU  TO  RECE65-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU
                      MOVE  TNS-TANINAME    TO  RECE65-TANINAME(IDX4)
                                                WRK-TANINAME
                  END-IF
                  IF    TNS-TENSIKIBETU    =    1   OR   3   OR  8
                    MOVE  TNS-TENSIKIBETU   TO  RECE65-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN           TO  RECE65-TEN(IDX4)
                  END-IF
                  MOVE  TNS-NYUTENTTLKBN    TO  RECE65-TENTTLKBN(IDX4)
                  MOVE  TNS-YUKOKETA        TO  RECE65-YUKOKETA(IDX4)
                  MOVE  TNS-DATAKBN         TO  RECE65-DATAKBN (IDX4)
                  MOVE  TNS-HOUKNSKBN       TO  RECE65-HOUKNSKBN(IDX4)
                  MOVE  TNS-KNSJISKBN       TO  RECE65-KNSJISKBN(IDX4)
                  MOVE  TNS-CDKBN-KBN       TO  RECE65-CDKBN-KBN(IDX4)
                  MOVE  TNS-CDKBN-SYO       TO  RECE65-CDKBN-SYO(IDX4)
                  MOVE  TNS-CDKBN-BU        TO  RECE65-CDKBN-BU(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM   
                                      TO RECE65-CDKBN-KBNNUM(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO RECE65-CDKBN-KBNNUM-EDA(IDX4)
                  MOVE  TNS-CDKBN-KOUBAN
                                      TO RECE65-CDKBN-KOUBAN(IDX4)
                  MOVE  TNS-KOUHYOJYUNNUM  
                                      TO RECE65-KOUHYOJYUNNUM(IDX4)
                  MOVE  TNS-NAME            TO  RECE65-NAME(IDX4)
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2)  TO  WRK-SRYCD
                  COMPUTE  RECE64-SRYCDTOTAL  =  RECE64-SRYCDTOTAL
                                              +  WRK-SRYCD
                  COMPUTE  RECE64-SURYOUTOTAL =  RECE64-SURYOUTOTAL
                                              +  WRK-ORCSTEN-SURYO(IDX2)
                  COMPUTE  RECE64-MEISAISU    =  RECE64-MEISAISU
                                              +  1
      *
                END-IF  
      *
                ADD        1                TO  IDX2
      *
              END-PERFORM
      *
              PERFORM   RECE65-WRITE-SEC
      *
           END-PERFORM
      *
           .
       RECE65-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       RECE62-HENSYU-SEC               SECTION.
      *
      *    ¸øÈñ¤ÎÊ¬Î¥É½¼¨ÍÑ¥¤¥ó¥Ç¥Ã¥¯¥¹¤Î³ÎÄê
           MOVE          ZERO       TO     BUN-IDX
           IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
           MOVE          1          TO     BUN-IDX
           END-IF
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹£²
           MOVE          ZERO       TO     BUN-IDX2
           IF  RECE62-TOTALTEN(2)       =      ZERO
               CONTINUE
           ELSE
               IF (RECE62-TOTALTEN(2)   NOT =  ZERO)              AND
                  (RECE62-TOTALTEN(1)   NOT =  RECE62-TOTALTEN(2))
                   MOVE       IDX1              TO   HC-IDX
                   PERFORM    VARYING   IDX2    FROM   1   BY   1
                      UNTIL   (IDX2      >       4)     OR
                              (RECE62-KOHNUMHC(HC-IDX IDX2)  =  SPACE)
                      IF   RECE62-KOHNUMHC(HC-IDX IDX2) =
                                                   RECE62-KOHNUM(1)
                           MOVE      2       TO     BUN-IDX2
                      END-IF
                   END-PERFORM
               END-IF
           END-IF
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹£³
           MOVE          ZERO       TO     BUN-IDX3
           IF  RECE62-TOTALTEN(3)       =      ZERO
               CONTINUE
           ELSE
               IF (RECE62-TOTALTEN(3)   NOT =  ZERO)              AND
                  (RECE62-TOTALTEN(1)   NOT =  RECE62-TOTALTEN(3))
                   MOVE       IDX1              TO   HC-IDX
                   PERFORM    VARYING   IDX2    FROM   1   BY   1
                      UNTIL   (IDX2      >       4)     OR
                              (RECE62-KOHNUMHC(HC-IDX IDX2)  =  SPACE)
                      IF   RECE62-KOHNUMHC(HC-IDX IDX2) =
                                                   RECE62-KOHNUM(2)
                           MOVE      3       TO     BUN-IDX3
                      END-IF
                   END-PERFORM
               END-IF
           END-IF
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹£´
           MOVE          ZERO       TO     BUN-IDX4
           IF  RECE62-TOTALTEN(4)       =      ZERO
               CONTINUE
           ELSE
               IF (RECE62-TOTALTEN(4)   NOT =  ZERO)              AND
                  (RECE62-TOTALTEN(1)   NOT =  RECE62-TOTALTEN(4))
                   MOVE       IDX1              TO   HC-IDX
                   PERFORM    VARYING   IDX2    FROM   1   BY   1
                      UNTIL   (IDX2      >       4)     OR
                              (RECE62-KOHNUMHC(HC-IDX IDX2)  =  SPACE)
                      IF   RECE62-KOHNUMHC(HC-IDX IDX2) =
                                                   RECE62-KOHNUM(3)
                           MOVE      4       TO     BUN-IDX4
                      END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF    SRY-SRYKBN     =  "40"  OR  "50"  OR
                                               "60"  OR  "70"  OR
                                               "80"  OR  "54"
                                MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                                COMPUTE   WK-YKZTEN   =
                                                      ACCT-KIZAITEN  +
                                                      ACCT-YKZTEN
                       END-IF
                   ELSE
                       IF   SRY-SRYKBN   =  "21"  OR  "22"  OR  "23" OR
                                            "31"  OR  "32"  OR  "33" OR
                                            "40"  OR  "50"  OR  "60" OR
                                            "70"  OR  "80"  OR  "54" OR
                                            "14"  OR  "13"
                           MOVE  ZERO          TO   WK-ZAIKAISU
                           MOVE  ZERO          TO   WK-ZAITEN
                           MOVE  ZERO          TO   WK-SYUTEN
                           MOVE  ZERO          TO   WK-KIZAITEN
                           MOVE  ZERO          TO   WK-YKZTEN
                       END-IF
                   END-IF
               ELSE
                     MOVE    ZERO     TO    WK-ZAIKAISU
                     MOVE    ZERO     TO    WK-ZAITEN
                     MOVE    ZERO     TO    WK-SYUTEN
                     MOVE    ZERO     TO    WK-KIZAITEN
                     MOVE    ZERO     TO    WK-YKZTEN
               END-IF
               IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                    (ACCT-ZAIKAISU  NOT =    ZERO)
                     PERFORM   RECE-LEFT-SET-SEC
               END-IF
               IF    SRY-SRYKBN   =   "21"   OR   "22"   OR   "23"   OR
                                      "31"   OR   "32"   OR   "33"   OR
                                      "40"   OR   "50"   OR   "60"   OR
                                      "70"   OR   "54"   OR   "14"   OR
                                      "13"
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
           .                                           
      *
       RECE62-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       RECE62-HENSYU3-IDX-SEC               SECTION.
      *
      *    ¸øÈñ¤ÎÊ¬Î¥É½¼¨ÍÑ¥¤¥ó¥Ç¥Ã¥¯¥¹¤Î³ÎÄê
           MOVE          ZERO       TO     BUN-IDX
           IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
           MOVE          1          TO     BUN-IDX
           END-IF
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹£²
           MOVE          ZERO       TO     BUN-IDX2
           IF  RECE62-TOTALTEN(2)       =      ZERO
               CONTINUE
           ELSE
               IF (RECE62-TOTALTEN(2)   NOT =  ZERO)              AND
                  (RECE62-TOTALTEN(1)   NOT =  RECE62-TOTALTEN(2))
                   MOVE       IDX1              TO   HC-IDX
                   PERFORM    VARYING   IDX2    FROM   1   BY   1
                      UNTIL   (IDX2      >       4)     OR
                              (RECE62-KOHNUMHC(HC-IDX IDX2)  =  SPACE)
                      IF   RECE62-KOHNUMHC(HC-IDX IDX2) =
                                                   RECE62-KOHNUM(1)
                           MOVE      2       TO     BUN-IDX2
                      END-IF
                   END-PERFORM
               END-IF
           END-IF
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹£³
           MOVE          ZERO       TO     BUN-IDX3
           IF  RECE62-TOTALTEN(3)       =      ZERO
               CONTINUE
           ELSE
               IF (RECE62-TOTALTEN(3)   NOT =  ZERO)              AND
                  (RECE62-TOTALTEN(1)   NOT =  RECE62-TOTALTEN(3))
                   MOVE       IDX1              TO   HC-IDX
                   PERFORM    VARYING   IDX2    FROM   1   BY   1
                      UNTIL   (IDX2      >       4)     OR
                              (RECE62-KOHNUMHC(HC-IDX IDX2)  =  SPACE)
                      IF   RECE62-KOHNUMHC(HC-IDX IDX2) =
                                                   RECE62-KOHNUM(2)
                           MOVE      3       TO     BUN-IDX3
                      END-IF
                   END-PERFORM
               END-IF
           END-IF
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹£´
           MOVE          ZERO       TO     BUN-IDX4
           IF  RECE62-TOTALTEN(4)       =      ZERO
               CONTINUE
           ELSE
               IF (RECE62-TOTALTEN(4)   NOT =  ZERO)              AND
                  (RECE62-TOTALTEN(1)   NOT =  RECE62-TOTALTEN(4))
                   MOVE       IDX1              TO   HC-IDX
                   PERFORM    VARYING   IDX2    FROM   1   BY   1
                      UNTIL   (IDX2      >       4)     OR
                              (RECE62-KOHNUMHC(HC-IDX IDX2)  =  SPACE)
                      IF   RECE62-KOHNUMHC(HC-IDX IDX2) =
                                                   RECE62-KOHNUM(3)
                           MOVE      4       TO     BUN-IDX4
                      END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .                                           
      *
       RECE62-HENSYU3-IDX-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       RECE62-HENSYU3-SEC               SECTION.
      *
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
           ADD   RECE64-ZAIKAISU    TO   RECE62-KAISU(12)
      *
           IF    BUN-IDX        =   1
                 COMPUTE   RECE62-TENSU(12 BUN-IDX)  =
                           RECE62-TENSU(12 BUN-IDX)  +
                           (RECE64-ZAITEN  *  RECE64-ZAIKAISU)
           END-IF
           IF    BUN-IDX2       =   2
                 COMPUTE   RECE62-TENSU(12 BUN-IDX2)  =
                           RECE62-TENSU(12 BUN-IDX2)  +
                           (RECE64-ZAITEN  *  RECE64-ZAIKAISU)
           END-IF
           IF    BUN-IDX3       =   3
                 COMPUTE   RECE62-TENSU(12 BUN-IDX3)  =
                           RECE62-TENSU(12 BUN-IDX3)  +
                           (RECE64-ZAITEN  *  RECE64-ZAIKAISU)
           END-IF
           IF    BUN-IDX4       =   4
                 COMPUTE   RECE62-TENSU(12 BUN-IDX4)  =
                           RECE62-TENSU(12 BUN-IDX4)  +
                           (RECE64-ZAITEN  *  RECE64-ZAIKAISU)
           END-IF
      *
           .                                           
      *
       RECE62-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       RECE-LEFT-SET-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE65-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   TNS-CHUKSNTUBAN   =   "0"
                        ADD   WK-ZAIKAISU  TO   RECE62-KAISU(1)
                   END-IF
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                             MOVE  "1"     TO   RECE62-SHOSHINTIME
                         WHEN    4
                             MOVE  "2"     TO   RECE62-SHOSHINTIME
                         WHEN    3
                             MOVE  "3"     TO   RECE62-SHOSHINTIME
                     END-EVALUATE
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE    RECE62-TENSU(1 BUN-IDX)  =
                                  RECE62-TENSU(1 BUN-IDX)  +
                                 (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE    RECE62-TENSU(1 BUN-IDX2)  =
                                  RECE62-TENSU(1 BUN-IDX2)  +
                                 (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE    RECE62-TENSU(1 BUN-IDX3)  =
                                  RECE62-TENSU(1 BUN-IDX3)  +
                                 (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE    RECE62-TENSU(1 BUN-IDX4)  =
                                  RECE62-TENSU(1 BUN-IDX4)  +
                                 (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF    BUN-IDX        =   1
                       COMPUTE    RECE62-TENSU(2 BUN-IDX)  =
                                  RECE62-TENSU(2 BUN-IDX)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE    RECE62-TENSU(2 BUN-IDX2)  =
                                  RECE62-TENSU(2 BUN-IDX2)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE    RECE62-TENSU(2 BUN-IDX3)  =
                                  RECE62-TENSU(2 BUN-IDX3)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE    RECE62-TENSU(2 BUN-IDX4)  =
                                  RECE62-TENSU(2 BUN-IDX4)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                   IF    BUN-IDX        =   1
                       COMPUTE    RECE62-TENSU(3 BUN-IDX)  =
                                  RECE62-TENSU(3 BUN-IDX)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE    RECE62-TENSU(3 BUN-IDX2)  =
                                  RECE62-TENSU(3 BUN-IDX2)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE    RECE62-TENSU(3 BUN-IDX3)  =
                                  RECE62-TENSU(3 BUN-IDX3)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE    RECE62-TENSU(3 BUN-IDX4)  =
                                  RECE62-TENSU(3 BUN-IDX4)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF     SRY-SRYCD(IDX3) (1:1)   =  "6" OR "S" OR "8"
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE62-TENSU(4 BUN-IDX)  =
                                     RECE62-TENSU(4 BUN-IDX)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE62-TENSU(4 BUN-IDX2)  =
                                     RECE62-TENSU(4 BUN-IDX2)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE62-TENSU(4 BUN-IDX3)  =
                                     RECE62-TENSU(4 BUN-IDX3)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE62-TENSU(4 BUN-IDX4)  =
                                     RECE62-TENSU(4 BUN-IDX4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE62-KAISU(4)
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE62-TENSU(4 BUN-IDX)  =
                                     RECE62-TENSU(4 BUN-IDX)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE62-TENSU(4 BUN-IDX2)  =
                                     RECE62-TENSU(4 BUN-IDX2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE62-TENSU(4 BUN-IDX3)  =
                                     RECE62-TENSU(4 BUN-IDX3)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE62-TENSU(4 BUN-IDX4)  =
                                     RECE62-TENSU(4 BUN-IDX4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                        ADD   WK-ZAIKAISU       TO   RECE62-KAISU(4)
                        IF    BUN-IDX        =   1
                            COMPUTE   RECE62-TENSU(4 BUN-IDX)  =
                                      RECE62-TENSU(4 BUN-IDX)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                        IF    BUN-IDX2       =   2
                            COMPUTE   RECE62-TENSU(4 BUN-IDX2)  =
                                      RECE62-TENSU(4 BUN-IDX2)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                        IF    BUN-IDX3       =   3
                            COMPUTE   RECE62-TENSU(4 BUN-IDX3)  =
                                      RECE62-TENSU(4 BUN-IDX3)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                        IF    BUN-IDX4       =   4
                            COMPUTE   RECE62-TENSU(4 BUN-IDX4)  =
                                      RECE62-TENSU(4 BUN-IDX4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE62-KAISU(5)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(5 BUN-IDX)  =
                                 RECE62-TENSU(5 BUN-IDX)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(5 BUN-IDX2)  =
                                 RECE62-TENSU(5 BUN-IDX2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(5 BUN-IDX3)  =
                                 RECE62-TENSU(5 BUN-IDX3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(5 BUN-IDX4)  =
                                 RECE62-TENSU(5 BUN-IDX4)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE62-KAISU(6)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(6 BUN-IDX)  =
                                 RECE62-TENSU(6 BUN-IDX)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(6 BUN-IDX2)  =
                                 RECE62-TENSU(6 BUN-IDX2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(6 BUN-IDX3)  =
                                 RECE62-TENSU(6 BUN-IDX3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(6 BUN-IDX4)  =
                                 RECE62-TENSU(6 BUN-IDX4)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   ADD    WK-ZAIKAISU     TO    RECE62-KAISU(7)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(7 BUN-IDX)  =
                                 RECE62-TENSU(7 BUN-IDX)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(7 BUN-IDX2)  =
                                 RECE62-TENSU(7 BUN-IDX2)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(7 BUN-IDX3)  =
                                 RECE62-TENSU(7 BUN-IDX3)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(7 BUN-IDX4)  =
                                 RECE62-TENSU(7 BUN-IDX4)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   ADD    WK-ZAIKAISU     TO   RECE62-KAISU(8)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(8 BUN-IDX)  =
                                 RECE62-TENSU(8 BUN-IDX)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(8 BUN-IDX2)  =
                                 RECE62-TENSU(8 BUN-IDX2)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(8 BUN-IDX3)  =
                                 RECE62-TENSU(8 BUN-IDX3)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(8 BUN-IDX4)  =
                                 RECE62-TENSU(8 BUN-IDX4)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(9 BUN-IDX)  =
                                 RECE62-TENSU(9 BUN-IDX)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(9 BUN-IDX2)  =
                                 RECE62-TENSU(9 BUN-IDX2)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(9 BUN-IDX3)  =
                                 RECE62-TENSU(9 BUN-IDX3)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(9 BUN-IDX4)  =
                                 RECE62-TENSU(9 BUN-IDX4)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE62-KAISU(10)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(10 BUN-IDX)  =
                                 RECE62-TENSU(10 BUN-IDX)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(10 BUN-IDX2)  =
                                 RECE62-TENSU(10 BUN-IDX2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(10 BUN-IDX3)  =
                                 RECE62-TENSU(10 BUN-IDX3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(10 BUN-IDX4)  =
                                 RECE62-TENSU(10 BUN-IDX4)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE62-KAISU(11)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(11 BUN-IDX)  =
                                 RECE62-TENSU(11 BUN-IDX)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(11 BUN-IDX2)  =
                                 RECE62-TENSU(11 BUN-IDX2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(11 BUN-IDX3)  =
                                 RECE62-TENSU(11 BUN-IDX3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(11 BUN-IDX4)  =
                                 RECE62-TENSU(11 BUN-IDX4)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE62-KAISU(12)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(12 BUN-IDX)  =
                                 RECE62-TENSU(12 BUN-IDX)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(12 BUN-IDX2)  =
                                 RECE62-TENSU(12 BUN-IDX2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(12 BUN-IDX3)  =
                                 RECE62-TENSU(12 BUN-IDX3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(12 BUN-IDX4)  =
                                 RECE62-TENSU(12 BUN-IDX4)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE62-KAISU(13)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(13 BUN-IDX)  =
                                 RECE62-TENSU(13 BUN-IDX)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(13 BUN-IDX2)  =
                                 RECE62-TENSU(13 BUN-IDX2)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(13 BUN-IDX3)  =
                                 RECE62-TENSU(13 BUN-IDX3)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(13 BUN-IDX4)  =
                                 RECE62-TENSU(13 BUN-IDX4)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(14 BUN-IDX)  =
                                 RECE62-TENSU(14 BUN-IDX)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(14 BUN-IDX2)  =
                                 RECE62-TENSU(14 BUN-IDX2)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(14 BUN-IDX3)  =
                                 RECE62-TENSU(14 BUN-IDX3)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(14 BUN-IDX4)  =
                                 RECE62-TENSU(14 BUN-IDX4)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE62-KAISU(15)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(15 BUN-IDX)  =
                                 RECE62-TENSU(15 BUN-IDX)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(15 BUN-IDX2)  =
                                 RECE62-TENSU(15 BUN-IDX2)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(15 BUN-IDX3)  =
                                 RECE62-TENSU(15 BUN-IDX3)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(15 BUN-IDX4)  =
                                 RECE62-TENSU(15 BUN-IDX4)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(16 BUN-IDX)  =
                                 RECE62-TENSU(16 BUN-IDX)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(16 BUN-IDX2)  =
                                 RECE62-TENSU(16 BUN-IDX2)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(16 BUN-IDX3)  =
                                 RECE62-TENSU(16 BUN-IDX3)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(16 BUN-IDX4)  =
                                 RECE62-TENSU(16 BUN-IDX4)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE62-KAISU(17)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(17 BUN-IDX)  =
                                 RECE62-TENSU(17 BUN-IDX)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(17 BUN-IDX2)  =
                                 RECE62-TENSU(17 BUN-IDX2)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(17 BUN-IDX3)  =
                                 RECE62-TENSU(17 BUN-IDX3)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(17 BUN-IDX4)  =
                                 RECE62-TENSU(17 BUN-IDX4)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    TNS-KNSJISKBN    =   2
                         COMPUTE   RECE62-KAISU(17)  =
                                   RECE62-KAISU(17)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   CONS-SAIKETSU1  OR
                                              CONS-SAIKETSU2  OR
                                              CONS-SAIKETSU3
                         IF   RECE62-KAISU(17)  NOT =  ZERO
                              COMPUTE   RECE62-KAISU(17)  =
                                        RECE62-KAISU(17)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(18 BUN-IDX)  =
                                 RECE62-TENSU(18 BUN-IDX)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(18 BUN-IDX2)  =
                                 RECE62-TENSU(18 BUN-IDX2)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(18 BUN-IDX3)  =
                                 RECE62-TENSU(18 BUN-IDX3)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(18 BUN-IDX4)  =
                                 RECE62-TENSU(18 BUN-IDX4)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE62-KAISU(19)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(19 BUN-IDX)  =
                                 RECE62-TENSU(19 BUN-IDX)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(19 BUN-IDX2)  =
                                 RECE62-TENSU(19 BUN-IDX2)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(19 BUN-IDX3)  =
                                 RECE62-TENSU(19 BUN-IDX3)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(19 BUN-IDX4)  =
                                 RECE62-TENSU(19 BUN-IDX4)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(20 BUN-IDX)  =
                                 RECE62-TENSU(20 BUN-IDX)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(20 BUN-IDX2)  =
                                 RECE62-TENSU(20 BUN-IDX2)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(20 BUN-IDX3)  =
                                 RECE62-TENSU(20 BUN-IDX3)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(20 BUN-IDX4)  =
                                 RECE62-TENSU(20 BUN-IDX4)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                   IF    BUN-IDX        =   1
                         IF    TNS-NYUTENTTLKBN   =    130
                           COMPUTE   RECE62-TENSU(2 BUN-IDX)  =
                                     RECE62-TENSU(2 BUN-IDX)  +
                                    (TNS-TEN   *   ACCT-ZAIKAISU)
                         ELSE  
                           COMPUTE   RECE62-TENSU(21 BUN-IDX)  =
                                     RECE62-TENSU(21 BUN-IDX)  +
                                    (WK-SYUTEN   *   ACCT-ZAIKAISU)
                         END-IF
                   END-IF
                   IF    BUN-IDX2       =   2
                         IF    TNS-NYUTENTTLKBN   =    130
                           COMPUTE   RECE62-TENSU(2 BUN-IDX2)  =
                                     RECE62-TENSU(2 BUN-IDX2)  +
                                    (TNS-TEN   *   ACCT-ZAIKAISU)
                         ELSE  
                           COMPUTE   RECE62-TENSU(21 BUN-IDX2)  =
                                     RECE62-TENSU(21 BUN-IDX2)  +
                                    (WK-SYUTEN   *   ACCT-ZAIKAISU)
                         END-IF
                   END-IF
                   IF    BUN-IDX3       =   3
                         IF    TNS-NYUTENTTLKBN   =    130
                           COMPUTE   RECE62-TENSU(2 BUN-IDX3)  =
                                     RECE62-TENSU(2 BUN-IDX3)  +
                                    (TNS-TEN   *   ACCT-ZAIKAISU)
                         ELSE  
                           COMPUTE   RECE62-TENSU(21 BUN-IDX3)  =
                                     RECE62-TENSU(21 BUN-IDX3)  +
                                    (WK-SYUTEN   *   ACCT-ZAIKAISU)
                         END-IF
                   END-IF
                   IF    BUN-IDX4       =   4
                         IF    TNS-NYUTENTTLKBN   =    130
                           COMPUTE   RECE62-TENSU(2 BUN-IDX4)  =
                                     RECE62-TENSU(2 BUN-IDX4)  +
                                    (TNS-TEN   *   ACCT-ZAIKAISU)
                         ELSE
                           COMPUTE   RECE62-TENSU(21 BUN-IDX4)  =
                                     RECE62-TENSU(21 BUN-IDX4)  +
                                    (WK-SYUTEN   *   ACCT-ZAIKAISU)
                         END-IF
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE62-TENSU(22 BUN-IDX)  =
                                 RECE62-TENSU(22 BUN-IDX)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE62-TENSU(22 BUN-IDX2)  =
                                 RECE62-TENSU(22 BUN-IDX2)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE62-TENSU(22 BUN-IDX3)  =
                                 RECE62-TENSU(22 BUN-IDX3)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE62-TENSU(22 BUN-IDX4)  =
                                 RECE62-TENSU(22 BUN-IDX4)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
           END-EVALUATE
           .
      *
       RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
           MOVE    WRK-SRYCD            TO      TNS-SRYCD
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    WK-TAIHI-SRYYMD      TO      ORC-DBYMD
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "TENSU-KEY"          TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "TENSU-KEY"      TO      ORC-DBPATH
               PERFORM  TENSU-READ-SEC
               IF       FLG-TENSU       =       1
                   DISPLAY   "TENSU FETCH ERR"  
               END-IF
           ELSE
               INITIALIZE       TNS-TENSU-REC
               MOVE    1   TO   FLG-TENSU
               DISPLAY   "TENSU SELECT ERR" 
           END-IF
      *
           MOVE   "TENSU-KEY"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Ø¤ÎÉéÃ´¶â¥»¥Ã¥È¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       FTN-SET-SEC                SECTION.
      *
           INITIALIZE                           WRK-SKJ-FTN-T
      *
           INITIALIZE                           SYUNOU-REC
           MOVE    RECE62-HOSPID        TO      SYU-HOSPID
           MOVE    RECE62-NYUGAIKBN     TO      SYU-NYUGAIKBN
           MOVE    RECE62-PTID          TO      SYU-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      SYU-SRYKA
           MOVE    RECE62-SRYYM         TO      SYU-SRYYMD(1:6)
           MOVE    "__"                 TO      SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SYUNOU-KEY17"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SYUNOU-KEY17"   TO      ORC-DBPATH
               PERFORM  SYUNOU-READ-SEC
               PERFORM  UNTIL
                  (FLG-SYUNOU             =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)   =   ZERO 
                       IF      SYU-HKNCOMBINUM   =
                                                RECE62-HKNCOMBI (IDX1)
                           IF  SYU-DENPJTIKBN    NOT =  "3"
                               PERFORM   RECE62-HENSYU-SEC3
                           END-IF
                           MOVE    10              TO  IDX1
                       END-IF
                   END-PERFORM          
                   MOVE     "SYUNOU-KEY17"  TO   ORC-DBPATH
                   PERFORM   SYUNOU-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SYUNOU SELECT ERR" 
           END-IF
      *
           MOVE   "SYUNOU-KEY17"     TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    ¿©»öÉéÃ´´Þ¤à¾å¸Â³Û¤Î¤¢¤ë¸øÈñ¤Î°ìÉôÉéÃ´¶â³ÛÊÔ½¸
      *
           PERFORM         KOH-FUTAN-SET-SEC
      *
           .
       FTN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡ÝÉéÃ´¶â¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       RECE62-HENSYU-SEC3               SECTION.
      *
           MOVE     ZERO     TO    FLG-CHIHOU
           MOVE     SPACE    TO    WRK-CHIHOU-NUM
      *
           PERFORM  VARYING  ID1    FROM    1   BY  1
                         UNTIL   ID1   >   2
      *
            IF  SYU-SHOKUJI(ID1)   =   ZERO
              CONTINUE
            ELSE 
              MOVE     ZERO     TO    FLG-ARI
              PERFORM  VARYING  ID2    FROM    1   BY  1
                         UNTIL   ID2                       > 2
                           OR    RECE62-SHOKUJIKHNRYO(ID2) = ZERO
                           OR    FLG-ARI                   = 1
                  IF  SYU-SHOKUJI(ID1)   =   RECE62-SHOKUJIKHNRYO(ID2)
                        ADD   SYU-SHOKUJI-NISSU(ID1)  TO  
                                       RECE62-SHOKUJIKHNNISSU(ID2)
                        MOVE  1   TO   FLG-ARI
                  END-IF
              END-PERFORM  
              IF  FLG-ARI  =  ZERO  
                  ADD   SYU-SHOKUJI-NISSU(ID1)  TO  
                                       RECE62-SHOKUJIKHNNISSU(ID2)
                  MOVE  SYU-SHOKUJI(ID1)        TO  
                                       RECE62-SHOKUJIKHNRYO(ID2)
              END-IF
            END-IF
      *
           END-PERFORM
      *
           PERFORM  VARYING  ID1  FROM   3  BY   1
                      UNTIL  ID1  >   4
      *
              IF      SYU-SHOKUJI(ID1)  NOT =  ZERO
                   ADD   SYU-SHOKUJI-NISSU(ID1)  TO  
                                       RECE62-SHOKUJIKHNNISSU(ID1)
                   MOVE  SYU-SHOKUJI(ID1)        TO  
                                       RECE62-SHOKUJIKHNRYO(ID1)
              END-IF
      *
           END-PERFORM
      *
      *++++
      *
           IF      SYU-SYUSKJ-RYOYOHI  NOT =  ZERO
                   ADD   SYU-SYUSKJ-NISSU     TO  
                                       RECE62-SHOKUJINISSU(1)
                   ADD   SYU-SYUSKJ-RYOYOHI   TO
                                       RECE62-SHOKUJIRYOYOHI(1)
                   ADD   SYU-SYUSKJ-FTN-RECE  TO
                                       RECE62-SHOKUJIFTN(1)
                   IF    SYU-SKYMONEY-SKJ-TAX  NOT = ZERO
                      ADD   SYU-SKYMONEY-SKJ-TAX   TO
                                       RECE62-SHOKUJIFTN(1)
                   END-IF 
           END-IF
           IF      RECE62-RJNHKNNUM    NOT =  SPACE
             IF      SYU-SKJ-RYOYOHI(1)  NOT =  ZERO
                   ADD   SYU-SKJ-NISSU(1)     TO  
                                       RECE62-SHOKUJINISSU(1)
                   ADD   SYU-SKJ-RYOYOHI(1)   TO
                                       RECE62-SHOKUJIRYOYOHI(1)
                   ADD   SYU-SKJ-FTN-RECE(1)  TO
                                       RECE62-SHOKUJIFTN(1)
      **>          ADD   SYU-SKJ-FTN-RECE(1)  TO
      **>                              RECE62-RJNNYUFTN
             END-IF
           END-IF
      *
      *++++
      *
           IF      RECE62-SRYYM      >=  "200210"
             IF    RECE62-KOUREI  NOT =    SPACE
                PERFORM     HOKEN-FUTAN-HENSYU-SEC
             ELSE                                                       
      *         Á¥°÷ÊÝ¸±¡ÊÈïÊÝ¸±¼Ô¡Ë¡ÜÄÌ¶ÐºÒ³²
                IF    RECE62-SENIN-TUUSAI     =    "1"
                   ADD    SYU-SYUCOMPFTN   TO   RECE62-FTNMONEY(1)
                END-IF
             END-IF
           ELSE
             IF    RECE62-RJNHKNNUM    NOT =    SPACE
                ADD    SYU-KOH1COMPFTN  TO   RECE62-FTNMONEY(1)
                ADD    SYU-KOH1COMPFTN  TO   RECE62-RJNNYUFTN
             END-IF
      *      Á¥°÷ÊÝ¸±¡ÊÈïÊÝ¸±¼Ô¡Ë¡ÜÄÌ¶ÐºÒ³²
             IF    RECE62-SENIN-TUUSAI     =    "1"
                ADD    SYU-SYUCOMPFTN   TO   RECE62-FTNMONEY(1)
             END-IF
           END-IF
      *
           MOVE    IDX1            TO  HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
             UNTIL    IDX2      >       4
              IF      RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                      MOVE    2          TO        KH-IDX
                      EVALUATE  TRUE
                          WHEN  RECE62-KOHNUM(1)  =  SYU-KOH-HKNNUM(1)
                                MOVE     1    TO   IDX3
                          WHEN  RECE62-KOHNUM(1)  =  SYU-KOH-HKNNUM(2)
                                MOVE     2    TO   IDX3
                          WHEN  RECE62-KOHNUM(1)  =  SYU-KOH-HKNNUM(3)
                                MOVE     3    TO   IDX3
                          WHEN  RECE62-KOHNUM(1)  =  SYU-KOH-HKNNUM(4)
                                MOVE     4    TO   IDX3
                      END-EVALUATE
      *
                      PERFORM   KOHJNISSUTBL-SET-SEC
      *
                      ADD      1          TO   RECE62-KOHKENSU(1)
                      IF    (RECE62-FTNMONEY(1)  =  ZERO)
                      AND   (RECE62-KOHNUM(1)    =  "010" OR "021")
                            CONTINUE
                      ELSE
                            ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                         RECE62-FTNMONEY(KH-IDX)
                            ADD   SYU-SKJ-FTN(IDX3)      TO
                                         WRK-SKJ-FTN(KH-IDX)
                      END-IF
      *
                      ADD   SYU-SKJ-NISSU(IDX3)    TO
                                         RECE62-SHOKUJINISSU(KH-IDX)
                      ADD   SYU-SKJ-RYOYOHI(IDX3)  TO
                                         RECE62-SHOKUJIRYOYOHI(KH-IDX)
                      ADD   SYU-SKJ-FTN-RECE(IDX3) TO
                                         RECE62-SHOKUJIFTN(KH-IDX)
      *
                      IF   RECE62-KOHNUM(1) =  "051"
                      AND  FLG-KOH      NOT =   2
                            MOVE     1   TO   FLG-KOH
                      END-IF
                      IF   RECE62-KOHNUM(1) =  "010"  OR  "021" 
                            MOVE     2   TO   FLG-KOH
                      END-IF 
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                      MOVE    3          TO        KH-IDX
                      EVALUATE  TRUE
                          WHEN  RECE62-KOHNUM(2)  =  SYU-KOH-HKNNUM(1)
                                MOVE     1    TO   IDX3
                          WHEN  RECE62-KOHNUM(2)  =  SYU-KOH-HKNNUM(2)
                                MOVE     2    TO   IDX3
                          WHEN  RECE62-KOHNUM(2)  =  SYU-KOH-HKNNUM(3)
                                MOVE     3    TO   IDX3
                          WHEN  RECE62-KOHNUM(2)  =  SYU-KOH-HKNNUM(4)
                                MOVE     4    TO   IDX3
                      END-EVALUATE
      *
                      PERFORM   KOHJNISSUTBL-SET-SEC
      *
                      ADD      1          TO   RECE62-KOHKENSU(2)
                      IF    (RECE62-FTNMONEY(1)  =  ZERO)
                      AND   (RECE62-KOHNUM(2)    =  "010" OR "021")
                            CONTINUE
                      ELSE
                            ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                         RECE62-FTNMONEY(KH-IDX)
                            ADD   SYU-SKJ-FTN(IDX3)      TO
                                         WRK-SKJ-FTN(KH-IDX)
                      END-IF
      *
                      ADD   SYU-SKJ-NISSU(IDX3)    TO
                                         RECE62-SHOKUJINISSU(KH-IDX)
                      ADD   SYU-SKJ-RYOYOHI(IDX3)  TO
                                         RECE62-SHOKUJIRYOYOHI(KH-IDX)
                      ADD   SYU-SKJ-FTN-RECE(IDX3) TO
                                         RECE62-SHOKUJIFTN(KH-IDX)
      *
                      IF   RECE62-KOHNUM(2) =  "051"
                      AND  FLG-KOH      NOT =   2
                            MOVE     1   TO   FLG-KOH
                      END-IF
                      IF   RECE62-KOHNUM(2) =  "010"  OR  "021" 
                            MOVE     2   TO   FLG-KOH
                      END-IF 
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                      MOVE    4          TO        KH-IDX
                      EVALUATE  TRUE
                          WHEN  RECE62-KOHNUM(3)  =  SYU-KOH-HKNNUM(1)
                                MOVE     1    TO   IDX3
                          WHEN  RECE62-KOHNUM(3)  =  SYU-KOH-HKNNUM(2)
                                MOVE     2    TO   IDX3
                          WHEN  RECE62-KOHNUM(3)  =  SYU-KOH-HKNNUM(3)
                                MOVE     3    TO   IDX3
                          WHEN  RECE62-KOHNUM(3)  =  SYU-KOH-HKNNUM(4)
                                MOVE     4    TO   IDX3
                      END-EVALUATE
      *
                      PERFORM   KOHJNISSUTBL-SET-SEC
      *
                      ADD      1          TO   RECE62-KOHKENSU(3)
                      IF    (RECE62-FTNMONEY(1)  =  ZERO)
                      AND   (RECE62-KOHNUM(3)    =  "010" OR "021")
                            CONTINUE
                      ELSE
                            ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                         RECE62-FTNMONEY(KH-IDX)
                            ADD   SYU-SKJ-FTN(IDX3)      TO
                                         WRK-SKJ-FTN(KH-IDX)
                      END-IF
      *
                      ADD   SYU-SKJ-NISSU(IDX3)    TO
                                         RECE62-SHOKUJINISSU(KH-IDX)
                      ADD   SYU-SKJ-RYOYOHI(IDX3)  TO
                                         RECE62-SHOKUJIRYOYOHI(KH-IDX)
                      ADD   SYU-SKJ-FTN-RECE(IDX3) TO
                                         RECE62-SHOKUJIFTN(KH-IDX)
      *
                      IF   RECE62-KOHNUM(3) =  "051"
                      AND  FLG-KOH      NOT =   2
                            MOVE     1   TO   FLG-KOH
                      END-IF
                      IF   RECE62-KOHNUM(3) =  "010"  OR  "021" 
                            MOVE     2   TO   FLG-KOH
                      END-IF 
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                      MOVE    5          TO        KH-IDX
                      EVALUATE  TRUE
                          WHEN  RECE62-KOHNUM(4)  =  SYU-KOH-HKNNUM(1)
                                MOVE     1    TO   IDX3
                          WHEN  RECE62-KOHNUM(4)  =  SYU-KOH-HKNNUM(2)
                                MOVE     2    TO   IDX3
                          WHEN  RECE62-KOHNUM(4)  =  SYU-KOH-HKNNUM(3)
                                MOVE     3    TO   IDX3
                          WHEN  RECE62-KOHNUM(4)  =  SYU-KOH-HKNNUM(4)
                                MOVE     4    TO   IDX3
                      END-EVALUATE
      *
                      PERFORM   KOHJNISSUTBL-SET-SEC
      *
                      ADD      1          TO   RECE62-KOHKENSU(4)
                      IF    (RECE62-FTNMONEY(1)  =  ZERO)
                      AND   (RECE62-KOHNUM(4)    =  "010" OR "021")
                            CONTINUE
                      ELSE
                            ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                         RECE62-FTNMONEY(KH-IDX)
                            ADD   SYU-SKJ-FTN(IDX3)      TO
                                         WRK-SKJ-FTN(KH-IDX)
                      END-IF
      *
                      ADD   SYU-SKJ-NISSU(IDX3)    TO
                                         RECE62-SHOKUJINISSU(KH-IDX)
                      ADD   SYU-SKJ-RYOYOHI(IDX3)  TO
                                         RECE62-SHOKUJIRYOYOHI(KH-IDX)
                      ADD   SYU-SKJ-FTN-RECE(IDX3) TO
                                         RECE62-SHOKUJIFTN(KH-IDX)
      *
                      IF   RECE62-KOHNUM(4) =  "051"
                      AND  FLG-KOH      NOT =   2
                            MOVE     1   TO   FLG-KOH
                      END-IF
                      IF   RECE62-KOHNUM(4) =  "010"  OR  "021" 
                            MOVE     2   TO   FLG-KOH
                      END-IF 
                 END-IF
              END-IF
           END-PERFORM
           .
       RECE62-HENSYU-EXT3.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±Ê¬ÉéÃ´¶â³ÛÊÔ½¸
      *****************************************************************
       HOKEN-FUTAN-HENSYU-SEC               SECTION.
      *
      ***  IF      RECE62-SRYYM      <    "200310"
      *
                COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                                    * 10
                                    * RECE62-KYURATE
                                    / 100
                COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
                COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
                COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
      *
                IF   ( SYU-KOH-FTN         =   ZERO   )                 
                AND  ( SYU-KOH-HKNNUM(1)   =   SPACE OR "027"  )        
                AND  ( SYU-KOH-HKNNUM(2)   =   SPACE  )                 
                AND  ( SYU-KOH-HKNNUM(3)   =   SPACE  )                 
                AND  ( SYU-KOH-HKNNUM(4)   =   SPACE  )                 
                    IF    RECE62-RJNHKNNUM    NOT =    SPACE            
                      ADD    SYU-KOH1COMPFTN  TO   RECE62-FTNMONEY(1)   
                                                   RECE62-RJNNYUFTN     
                                                   WRK-KOH-GAI          
                    ELSE                                                
                      ADD    SYU-SYUCOMPFTN   TO   RECE62-FTNMONEY(1)   
                                                   WRK-KOH-GAI          
                    END-IF                                              
                ELSE
                  IF   ( SYU-KOH-HKNNUM(1)   =   "012" )
                  OR   ( SYU-KOH-HKNNUM(2)   =   "012" )
                  OR   ( SYU-KOH-HKNNUM(3)   =   "012" )
                  OR   ( SYU-KOH-HKNNUM(4)   =   "012" )
                      MOVE    15000     TO   WRK-KINGAKU
                      PERFORM   ICHIBUFTN-KOH-SET-SEC
                  ELSE
                    IF   ( RECE62-TOKKI(1)     =   "02"  )
                    OR   ( RECE62-TOKKI(2)     =   "02"  )
                    OR   ( RECE62-TOKKI(3)     =   "02"  )
                    OR   ( RECE62-TOKKI(4)     =   "02"  )
                    OR   ( RECE62-TOKKI(5)     =   "02"  )
                      MOVE    10000     TO   WRK-KINGAKU
                      PERFORM   ICHIBUFTN-KOH-SET-SEC
                    ELSE
                      PERFORM         CHIHOU-KOH-HANBETU-SEC
                      IF   (  RECE62-TEISYOTOKU  NOT =  SPACE  )
                      AND  (  FLG-CHIHOU             =  1      )
                        EVALUATE   RECE62-TEISYOTOKU
                          WHEN  "1"
                            MOVE    15000     TO   WRK-KINGAKU
                            PERFORM   ICHIBUFTN-KOH-SET-SEC
                          WHEN  "2"
                          WHEN  "3"
                            MOVE    24600     TO   WRK-KINGAKU
                            PERFORM   ICHIBUFTN-KOH-SET-SEC
                        END-EVALUATE
                      ELSE
                        IF   (  RECE62-KYURATE         =  20     )
                        AND  (  FLG-CHIHOU             =  1      )
                           EVALUATE  TRUE
                             WHEN  RECE62-KOHNUM(1)  =  WRK-CHIHOU-NUM
                                COMPUTE  WRK-KINGAKU2  =
                                      RECE62-TOTALTEN(2) *  10
                             WHEN  RECE62-KOHNUM(2)  =  WRK-CHIHOU-NUM
                                COMPUTE  WRK-KINGAKU2  =
                                      RECE62-TOTALTEN(3) *  10
                             WHEN  RECE62-KOHNUM(3)  =  WRK-CHIHOU-NUM
                                COMPUTE  WRK-KINGAKU2  =
                                      RECE62-TOTALTEN(4) *  10
                             WHEN  RECE62-KOHNUM(4)  =  WRK-CHIHOU-NUM
                                COMPUTE  WRK-KINGAKU2  =
                                      RECE62-TOTALTEN(5) *  10
                           END-EVALUATE
      ***                  IF   WRK-KINGAKU2  >  361500
      ***                     COMPUTE  WRK-KINGAKU3 =
      ***                     (( WRK-KINGAKU2 -  361500 ) * 0.01 ) + 0.5
      ***                     COMPUTE  WRK-KINGAKU  =   72300
      ***                                           +   WRK-KINGAKU3
      ***                  ELSE
                              MOVE    72300  TO   WRK-KINGAKU
      ***                  END-IF                     
                           PERFORM   ICHIBUFTN-KOH-SET-SEC
                        ELSE
                           MOVE    40200     TO   WRK-KINGAKU
                           PERFORM   ICHIBUFTN-KOH-SET-SEC
                        END-IF
                      END-IF
                    END-IF
                  END-IF                                              
                END-IF                                                  
      *
                ADD   SYU-KOH-FTN   TO   WRK-KOH-FTN
      *
      ***  ELSE
      *         ¼ýÇ¼¹àÌÜ¤«¤éÊÔ½¸¤¹¤ë¤è¤¦¤ËÊÑ¹¹
      ***          IF   ( SYU-KOH-FTN         =   ZERO   )                 
      ***          AND  ( SYU-KOH-HKNNUM(1)   =   SPACE OR "027"  )        
      ***          AND  ( SYU-KOH-HKNNUM(2)   =   SPACE  )                 
      ***          AND  ( SYU-KOH-HKNNUM(3)   =   SPACE  )                 
      ***          AND  ( SYU-KOH-HKNNUM(4)   =   SPACE  )                 
      ***              IF    RECE62-RJNHKNNUM    NOT =    SPACE            
      ***                ADD    SYU-KOH1COMPFTN  TO   RECE62-FTNMONEY(1)   
      ***                                             RECE62-RJNNYUFTN     
      ***                                             WRK-KOH-GAI          
      ***              ELSE                                                
      ***                ADD    SYU-SYUCOMPFTN   TO   RECE62-FTNMONEY(1)   
      ***                                             WRK-KOH-GAI          
      ***              END-IF                                              
      ***          ELSE
      ***              IF    RECE62-RJNHKNNUM    NOT =    SPACE            
      ***                ADD    SYU-KOH1COMPFTN  TO   RECE62-FTNMONEY(1)   
      ***                                             RECE62-RJNNYUFTN     
      ***                                             FTN-KOH-XXX          
      ***              ELSE                                                
      ***                ADD    SYU-SYUCOMPFTN   TO   RECE62-FTNMONEY(1)   
      ***                                             FTN-KOH-XXX          
      ***              END-IF
      ***          END-IF                                                  
      *
      ***          ADD   SYU-KOH-FTN   TO   WRK-KOH-FTN
      *
      ***  END-IF
      *
           .
       HOKEN-FUTAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¼ÂÆü¿ô¥Æ¡¼¥Ö¥ë¾ðÊó¤Î¥»¥Ã¥È
      *****************************************************************
       KOHJNISSUTBL-SET-SEC               SECTION.
      *
           EVALUATE   WRK-PREFNUM
      *
             WHEN    "38"
      *
               IF   SYU-KOH-HKNNUM(IDX3)(1:1) = "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6" OR
                                                "7" OR "8"
                  PERFORM    VARYING   IDX7    FROM   1   BY   1
                               UNTIL   IDX7      >      31
                    IF   SYU-DAY(IDX7)       NOT =  SPACE
                    AND  RECE62-JNISSUTBL(IDX7)  =   "1"
                       MOVE   "1"   TO   RECE62-KOHJNISSUTBL(IDX7)
                    END-IF
                  END-PERFORM
               END-IF
      *
           END-EVALUATE
      *
           .
       KOHJNISSUTBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»öÉéÃ´´Þ¤à¾å¸Â³Û¤Î¤¢¤ë¸øÈñ¤Î°ìÉôÉéÃ´¶â³ÛÊÔ½¸
      *****************************************************************
       KOH-FUTAN-SET-SEC               SECTION.
      *
           PERFORM    VARYING   KOHFTN-IDX   FROM   1   BY   1
                  UNTIL    KOHFTN-IDX    >    4
      *
             IF   (RECE62-KOHNUM(KOHFTN-IDX) =  "051"   )
             AND  (RECE62-SRYYM              <  "200310")
      *
                COMPUTE  WRK-FTNKEI2 = RECE62-FTNMONEY(KOHFTN-IDX + 1)
                                     + WRK-SKJ-FTN(KOHFTN-IDX + 1)
                COMPUTE  WRK-FTNKEI2 = WRK-FTNKEI2 +  5
                COMPUTE  WRK-FTNKEI2 = WRK-FTNKEI2 / 10
                COMPUTE  WRK-FTNKEI2 = WRK-FTNKEI2 * 10
                MOVE     RECE62-FTNMONEY(KOHFTN-IDX + 1)  
                                   TO  WRK-FTNKEI3
                COMPUTE  WRK-FTNKEI3 = WRK-FTNKEI3 +  5
                COMPUTE  WRK-FTNKEI3 = WRK-FTNKEI3 / 10
                COMPUTE  WRK-FTNKEI3 = WRK-FTNKEI3 * 10
      *
                IF  WRK-FTNKEI3  =  14000
                    MOVE  WRK-FTNKEI3  
                                   TO  RECE62-FTNMONEY(KOHFTN-IDX + 1)
                END-IF
                IF  WRK-FTNKEI3  <  14000
                AND WRK-FTNKEI2  =  14000
                    MOVE  WRK-FTNKEI2  
                                   TO  RECE62-FTNMONEY(KOHFTN-IDX + 1)
                END-IF
      *
             END-IF
      *
             IF  ((RECE62-KOHNUM(KOHFTN-IDX) =  "012" OR "015" 
                                                      OR "016")   AND
                  (RECE62-SRYYM             >=  "200304"      ))
             OR  ((RECE62-KOHNUM(KOHFTN-IDX) =  "051"         )   AND
                  (RECE62-SRYYM             >=  "200310"      ))
             OR  ((WRK-PREFNUM               =  "13"          )   AND
                  (RECE62-KOHNUM(KOHFTN-IDX) =  "151" OR "451")   AND
                  (RECE62-SRYYM             >=  "200310"      ))
      *
                COMPUTE  WRK-FTNKEI2 = RECE62-FTNMONEY(KOHFTN-IDX + 1)
                                     + WRK-SKJ-FTN(KOHFTN-IDX + 1)
                COMPUTE  WRK-FTNKEI2 = WRK-FTNKEI2 +  5
                COMPUTE  WRK-FTNKEI2 = WRK-FTNKEI2 / 10
                COMPUTE  WRK-FTNKEI2 = WRK-FTNKEI2 * 10
                MOVE     RECE62-FTNMONEY(KOHFTN-IDX + 1)  
                                   TO  WRK-FTNKEI3
                COMPUTE  WRK-FTNKEI3 = WRK-FTNKEI3 +  5
                COMPUTE  WRK-FTNKEI3 = WRK-FTNKEI3 / 10
                COMPUTE  WRK-FTNKEI3 = WRK-FTNKEI3 * 10
      *
                PERFORM  KOHFTN-KINGAKU-SEC
      *
                IF  WRK-NYUJGNGAK  NOT =  ZERO
                  IF  WRK-FTNKEI3  =  WRK-NYUJGNGAK
                    MOVE  WRK-FTNKEI3  
                                   TO  RECE62-FTNMONEY(KOHFTN-IDX + 1)
                  END-IF
                  IF  WRK-FTNKEI3  <  WRK-NYUJGNGAK
                  AND WRK-FTNKEI2  =  WRK-NYUJGNGAK
                    MOVE  WRK-FTNKEI2  
                                   TO  RECE62-FTNMONEY(KOHFTN-IDX + 1)
                  END-IF
                END-IF
      *
             END-IF
      *
           END-PERFORM
      *
           .
       KOH-FUTAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñÉéÃ´¡Ê¶â³Û¡Ë¼è¹þ¤ß½èÍý
      *****************************************************************
       KOHFTN-KINGAKU-SEC               SECTION.
      *
           MOVE      ZERO                  TO      WRK-NYUJGNGAK
      *
           INITIALIZE                              PTKOHFTN-REC
           MOVE    RECE62-HOSPID           TO      PTKOHFTN-HOSPID
           MOVE    RECE62-PTID             TO      PTKOHFTN-PTID
           MOVE    RECE62-KOHID(KOHFTN-IDX)   TO   PTKOHFTN-KOHID
           MOVE    PTKOHFTN-REC            TO      MCPDATA-REC
           MOVE    "DBSELECT"              TO      MCP-FUNC
           MOVE    "PTKOHFTN-KEY4"         TO      ORC-DBPATH
           CALL    "ORCMCPSUB"             USING   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
           IF      MCP-RC               =       ZERO
             MOVE    "PTKOHFTN-KEY4"     TO      ORC-DBPATH
             PERFORM  PTKOHFTN-READ-SEC
             PERFORM  UNTIL
                 (FLG-PTKOHFTN       =   1)    OR
                ((PTKOHFTN-TEKSTYMD(1:6) <=   RECE62-SRYYM   )  AND
                 (RECE62-SRYYM           <=   PTKOHFTN-TEKEDYMD(1:6)))
               PERFORM   PTKOHFTN-READ-SEC
             END-PERFORM
           ELSE
             MOVE    1                   TO      FLG-PTKOHFTN
           END-IF
      *
           IF   FLG-PTKOHFTN  =  ZERO
      *
             MOVE   PTKOHFTN-NYUJGNGAK   TO      WRK-NYUJGNGAK
      *
           END-IF
      *
           MOVE    "DBCLOSE"             TO  MCP-FUNC
           MOVE    "PTKOHFTN-KEY4"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC
      *
           .
       KOHFTN-KINGAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±Íó¤Î°ìÉôÉéÃ´¶â³Û¥»¥Ã¥È¡Ê¸øÈñ´Þ¤à¾ì¹ç¡Ë
      *****************************************************************
       ICHIBUFTN-KOH-SET-SEC               SECTION.
      *
           IF  FTN-KOH-XXX  >   WRK-KINGAKU
                MOVE   WRK-KINGAKU    TO   FTN-KOH-XXX
           END-IF
           COMPUTE  FTN-KOH-YYY  =  WRK-KINGAKU  -  FTN-KOH-XXX
           IF  WRK-FTNKEI  >  FTN-KOH-YYY
                ADD      FTN-KOH-YYY  TO  RECE62-FTNMONEY(1)
                                          FTN-KOH-XXX
                IF    RECE62-RJNHKNNUM    NOT =    SPACE
                    ADD  FTN-KOH-YYY  TO  RECE62-RJNNYUFTN
                END-IF
           ELSE
                ADD      WRK-FTNKEI   TO  RECE62-FTNMONEY(1)
                                          FTN-KOH-XXX
                IF    RECE62-RJNHKNNUM    NOT =    SPACE
                    ADD  WRK-FTNKEI   TO  RECE62-RJNNYUFTN
                END-IF
           END-IF
      *
           .
       ICHIBUFTN-KOH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼çÊÝ¸±¡ÜÃÏÊý¸øÈñ¡¡Ëô¤Ï¡¡¼çÊÝ¸±¡ÜÏ·¿Í¡ÜÃÏÊý¸øÈñ
      *    ¤ÎÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¤«¤É¤¦¤«È½ÊÌ
      *****************************************************************
       CHIHOU-KOH-HANBETU-SEC               SECTION.
      *
           IF  (( SYU-KOH-HKNNUM(1)       =   "027"               )  AND
                ( SYU-KOH-HKNNUM(2)(1:1)  =   "1" OR "2" OR "3" OR
                                              "4" OR "5" OR "6" OR
                                              "7" OR "8"          )  AND
                ( SYU-KOH-HKNNUM(3)       =   SPACE               )  AND
                ( SYU-KOH-HKNNUM(4)       =   SPACE               ))
           OR  (( SYU-KOH-HKNNUM(1)(1:1)  =   "1" OR "2" OR "3" OR
                                              "4" OR "5" OR "6" OR
                                              "7" OR "8"          )  AND
                ( SYU-KOH-HKNNUM(2)       =   SPACE               )  AND
                ( SYU-KOH-HKNNUM(3)       =   SPACE               )  AND
                ( SYU-KOH-HKNNUM(4)       =   SPACE               ))
      *
                MOVE     1       TO      FLG-CHIHOU
      *
                IF    SYU-KOH-HKNNUM(1)   =   "027"
                    MOVE     SYU-KOH-HKNNUM(2)   TO   WRK-CHIHOU-NUM
                ELSE
                    MOVE     SYU-KOH-HKNNUM(1)   TO   WRK-CHIHOU-NUM
                END-IF
      *
           END-IF
      *
           .
       CHIHOU-KOH-HANBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    °ìÉôÉéÃ´¶âºÆ·Ç¤Î·×»»½èÍý
      *****************************************************************
       SAIKEI-KEISAN-SEC               SECTION.
      *
           IF   RECE62-KOUREI  =  SPACE
                  MOVE     ZERO      TO     RECE62-KOUREI-SAIKEI
           ELSE
              IF   FTN-KOH-XXX   =   ZERO        
      *       OR   WRK-KOH-FTN   =   ZERO
                  MOVE     ZERO      TO     RECE62-KOUREI-SAIKEI
              ELSE 
                  IF  FLG-KOH     =   ZERO
                     COMPUTE   RECE62-KOUREI-SAIKEI  =  
                               FTN-KOH-XXX
                     IF   FTN-KOH-XXX   <=   WRK-KOH-FTN 
                           MOVE  ZERO   TO   RECE62-KOUREI-SAIKEI
                     END-IF
                     IF   FTN-KOH-XXX    =   RECE62-FTNMONEY(1)
                           MOVE  ZERO   TO   RECE62-KOUREI-SAIKEI
                     END-IF
                  ELSE
                     IF  FLG-KOH  =   1
                        COMPUTE   RECE62-KOUREI-SAIKEI  =  
                                  FTN-KOH-XXX
                     END-IF
                     IF  FLG-KOH  =   2
                        ADD   WRK-KOH-GAI   TO  RECE62-FTNMONEY(2)
                        COMPUTE   RECE62-KOUREI-SAIKEI  =  
                                  FTN-KOH-XXX
                     END-IF
                     IF   FTN-KOH-XXX   <=   WRK-KOH-FTN 
                           MOVE  ZERO   TO   RECE62-KOUREI-SAIKEI
                     END-IF
                     IF   FTN-KOH-XXX    =   RECE62-FTNMONEY(1)
                           MOVE  ZERO   TO   RECE62-KOUREI-SAIKEI
                     END-IF
                  END-IF
              END-IF
           END-IF
      *
           .
       SAIKEI-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE64-WRITE-SEC                SECTION.
      *
           IF      IDZ             =   1               
      *    ¥ì¥»¥×¥È½èÍý
               WRITE   RECE64-REC
      *
               ADD     1                   TO  CNT-OUT1
           END-IF
      *
           .
       RECE64-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-WRITE-SEC                SECTION.
      *
           IF      IDZ             =   1               
      *    ¥ì¥»¥×¥È½èÍý
               WRITE   RECE65-REC
      *
               ADD     1                   TO  CNT-OUT2
           END-IF 
      *
           .
       RECE65-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE65-REWRITE-SEC              SECTION.                        
      *
           IF      IDZ             =   1               
      *    ¥ì¥»¥×¥È½èÍý
      *
              MOVE    ZERO                TO  FLG-REWRITE
      *
              INITIALIZE                      RECE65-REC
              MOVE    RECE64-HOSPID       TO  RECE65-HOSPID
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
                        MOVE    1         TO  FLG-RECE65-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE65-ZAN
                        READ    RECE65-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE65-ZAN
                        END-READ
              END-START
              PERFORM   UNTIL
                  (RECE64-HOSPID       NOT =  RECE65-HOSPID)      OR
                  (RECE64-SRYYM        NOT =  RECE65-SRYYM)       OR
                  (RECE64-NYUGAIKBN    NOT =  RECE65-NYUGAIKBN)   OR
                  (RECE64-PTID         NOT =  RECE65-PTID)        OR
                  (RECE64-RECEKA       NOT =  RECE65-RECEKA)      OR
                  (RECE64-TEISYUTUSAKI NOT =  RECE65-TEISYUTUSAKI) OR
                  (RECE64-RECESYUBETU  NOT =  RECE65-RECESYUBETU)  OR
                  (RECE64-HKNJANUM     NOT =  RECE65-HKNJANUM)    OR
                  (RECE64-HOJOKBN      NOT =  RECE65-HOJOKBN )    OR
                  (RECE64-TEKSTYMD     NOT =  RECE65-TEKSTYMD)    OR
                  (RECE64-ZAINUM       NOT =  RECE65-ZAINUM)      OR
                  (RECE64-ZAIREN       NOT =  RECE65-ZAIREN)      OR
                  (FLG-RECE65-ZAN            =      1        )    OR
                  (FLG-REWRITE               =      1        )
                   IF    RECE65-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      MOVE    WRK-SRYSURYO3  TO  RECE65-SRYSURYO(5)
                      REWRITE   RECE65-REC
                   END-IF
                   READ    RECE65-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE65-ZAN
                   END-READ
              END-PERFORM
      *
           END-IF 
      *
           .
       RECE65-REWRITE-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿¹¹¿·½èÍý¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       RECE62-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE62-REC
      *
           ADD     1                   TO  CNT-OUT3 (IDZ)
      *
           .
       RECE62-WRITE-EXT.
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
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
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
      **     DISPLAY "RECE62-IN  :" CNT-IN   
      **     DISPLAY "RECE62-MOD :" CNT-OUT3  
      **     DISPLAY "RECE64-OUT :" CNT-OUT1 
      **     DISPLAY "RECE65-OUT :" CNT-OUT2  
             DISPLAY "*** ORCR0640 END ***"
      **     ACCEPT  WRK-IN FROM CONSOLE 
           .
       300-TERM-EXT.
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
                       ELSE
                           MOVE    ZERO            TO  WRK-NUM
                           MOVE    WRK-EDIDX       TO  IDX
                           MOVE    10              TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    IDX3                TO  IDY     
               ELSE
      *        È¾³Ñ¿ô»ú¤ËÊÑ´¹¡Ê¿ô»ú°Ê³°¤Î¤È¤­¤Ï¥¼¥í¡Ë
                   INSPECT WRK-ZENKAKU(WRK-STIDX:)  REPLACING   ALL " "
                                                                BY  "0"
                   IF      WRK-ZENKAKU(WRK-STIDX:)      NOT NUMERIC
                       MOVE    ZERO                TO  WRK-NUM
                       MOVE    10                  TO  IDY   
                   ELSE
                       COMPUTE IDY     =   WRK-EDIDX   -
                                           WRK-STIDX   +   1
                       MOVE    WRK-ZENKAKU (WRK-STIDX:IDY) TO  WRK-NUM
                   END-IF     
               END-IF
           END-IF
      *
           IF      IDY                 >   10
               MOVE    1                   TO  IDX
           ELSE
               COMPUTE IDX     =   11      -   IDY
           END-IF
           MOVE    ZERO                TO  WRK-NUM-X
           MOVE    WRK-NUM (1:IDY)     TO  WRK-NUM-X (IDX:)
           MOVE    SPACE               TO  WRK-NUM
           MOVE    WRK-NUM-X           TO  WRK-NUM (1:10)
           MOVE    10                  TO  IDY
       
      *
           .
       800-NUM-ZENKAKU-HENKAN-EXT.
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
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       SRYACT-READ-SEC           SECTION.
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
      **************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      **************************************************************
       NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       NYUINACT-ERAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ôÆÉ¹þ
      *****************************************************************
       TENSU-READ-SEC           SECTION.
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
       TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÆÉ¹þ
      *****************************************************************
       SYUNOU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁªÄêÎÅÍÜ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SENTEICDCHG-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
               MOVE    ZERO            TO  FLG-SENTEICDCHG
           ELSE
               INITIALIZE                  SENTEICDCHG-REC
               MOVE    1               TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       SENTEICDCHG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñÉéÃ´¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       PTKOHFTN-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHFTN-REC
               MOVE    ZERO            TO  FLG-PTKOHFTN
           ELSE
               INITIALIZE                  PTKOHFTN-REC
               MOVE    1               TO  FLG-PTKOHFTN
           END-IF
      *
           .
       PTKOHFTN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥³¥á¥ó¥È¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
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
               CALL    "ORCMCPSUB"          USING
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       950-PTCOM-READ-EXT.
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
                                       MCPDATA-REC
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
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
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ¥«¡¼¥½¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       CSR-CLOSE-SEC            SECTION.
      *
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSE"           TO  MCP-FUNC
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
      *    Ãæ´Ö¥Õ¥¡¥¤¥ë£Ò£Å£Á£Ä½èÍý
      *****************************************************************
       RECE62-READ-SEC            SECTION.
      *
           READ    RECE62-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE62
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                   IF   WRK-PARA-SRYYM    NOT =  ZERO
                      CONTINUE
                   ELSE
      *               ¿©»ö¡¦Ì¤¼Â»Ü¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
                      MOVE     ZERO      TO  WRK-SHOKUJIRYOYO
                                             WRK-TOKUKANRIKASAN
      *
                      INITIALIZE             WRK-SYS-MIJISI-T
      *
                      MOVE  ZERO         TO  FLG-SET
                      MOVE  RECE62-SRYYM TO  WRK-SYS-SRYYMD(1:6)
                      MOVE  "01"         TO  WRK-SYS-SRYYMD(7:2)
                      PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                           UNTIL  ( SYSKAN-IDX              >  10   )
                            OR    ( TBL-SYS5000(SYSKAN-IDX) = SPACE )
                            OR    ( FLG-SET                 =   1   )
      *
                       MOVE  TBL-SYS5000(SYSKAN-IDX)  TO  SYS-5000-REC
                       IF   ( SYS-5000-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                       AND  ( WRK-SYS-SRYYMD     <=  SYS-5000-EDYUKYMD )
                          MOVE     1       TO    FLG-SET
                          PERFORM  SYSKAN-HENSYU-SEC
                       END-IF
      *
                      END-PERFORM
                   END-IF
           END-READ
           .
       RECE62-READ-EXT.
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
