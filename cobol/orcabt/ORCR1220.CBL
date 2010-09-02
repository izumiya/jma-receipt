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
       PROGRAM-ID.             ORCR1220.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¸ø³²¥ì¥»¥×¥È¡ÊÆþ±¡¡Ë
      *                      ¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  07/11/16    NACL¡ÝÆ£¸¶    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  04.02.01    NACL-ÌçÏÆ    08/03/10  ¡û¡û¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ¤Î¥Õ¥é¥°ÊÔ½¸½¤Àµ
      *  04.02.02    NACL-ÌçÏÆ    08/03/24  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *                                     ¡Ê³°Çñ¡§Â¾°åÎÅµ¡´Ø¼õ¿Ç»þ¤Î¿ÇÎÅ¹Ô°Ù
      *                                             ¥³¡¼¥É¼èÆÀ¡Ë
      *                                     ¡ÊÆÃµ­»ö¹àÄÉ²ÃÂÐ±þ¡§£²£°¡Ë
      *                                     ¡ÊÌôºÞ´ÉÍý»ØÆ³ÎÁ¥³¡¼¥ÉÄÉ²Ã¡Ë
      *                                     ¡ÊÇ¾·ì´É¥ê¥Ï¡Ê£³¡Ë¥³¡¼¥ÉÄÉ²Ã¡Ë
      *                                     ¡Ê²ðÃ£¸£°úÈ¯¾ÉÆüµ­ºÜÇÑ»ß¡Ë
      *                                     ¡Ê½é¿Ç¡ÊÌë´Ö¡¦ÁáÄ«Åù¡Ë²Ã»»ÄÉ²Ã¡Ë
      *                                     ¡Ê¿ÇÎÅ¶èÊ¬£¶£´¡§ÉÂÍý¿ÇÃÇÂÐ±þ¡Ë
      *                                     ¡Êºß»ÙÉÂ¼«Æ°µ­ºÜÂÐ±þ¡Ë
      *  04.02.03    NACL-ÌçÏÆ    08/04/03  ¡û¡û¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ¤Î¥Õ¥é¥°ÊÔ½¸½¤Àµ
      *  04.02.04    NACL-ÌçÏÆ    08/06/13  Âà±¡Á°Ë¬Ìä»ØÆ³ÎÁ¡¢Àº¿À²ÊÂà±¡Á°Ë¬Ìä
      *                                     »ØÆ³ÎÁ¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *
      *  04.03.01    NACL-ÌçÏÆ    08/07/08  ¿Ç¶è£±£´¤Î¥³¥á¥ó¥È¤Î¤ß¤ÎºÞ
      *                                     ÊÔ½¸ÂÐ±þ
      *  04.03.02    NACL-ÌçÏÆ    08/07/14  ²¾¥ì¥»ºîÀ®¡Ê±¡³°½èÊý´Þ¤à¡ËÂÐ±þ
      *                                     ÅêÌôºÞÅÀ£°¡Ê±¡³°Åù¡Ëµ­ºÜ
      *  04.03.03    NACL-ÌçÏÆ    08/08/19  Æþ±¡ÎÁÌ¤»»Äê»þ¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  04.03.04    NACL-ÌçÏÆ    08/12/01  ºßÂð¡Ê±¡³°Ê¬¡ËÊÔ½¸½¤Àµ
      *  04.03.05    NACL-ÌçÏÆ    08/12/04  ²¾¼ýÇ¼¥Á¥§¥Ã¥¯½èÍýÄÉ²Ã
      *  04.03.06    NACL-ÌçÏÆ    08/12/16  Ê¿À®£²£±Ç¯£±·î²þÀµÂÐ±þ
      *
      *  04.04.01    NACL-ÌçÏÆ    08/12/25  Êñ³çÂÐ±þ
      *  04.04.02    NACL-ÌçÏÆ    09/02/04  ¸¡ºº¤Î¥³¥á¥ó¥È¼«Æ°µ­ºÜÂÐ±þ
      *  04.04.03    NACL-ÌçÏÆ    09/04/13  Ê¿À®£²£±Ç¯£µ·î²þÀµÂÐ±þ
      *  04.04.04    NACL-ÌçÏÆ    09/06/23  ÅÀÅ©´ïºà¾¦ÉÊÌ¾µ­ºÜ½¤Àµ
      *  04.04.05    NACL-ÌçÏÆ    09/11/05  ¹âÎð¼ÔÂ¾ÊÝ¸±Æþ±¡ÂÐ±þ
      *  04.04.06    NACL-ÌçÏÆ    09/12/04  ºÞ¤Þ¤È¤á¹àÌÜÃÍ¤Î·×»»½¤Àµ
      *  04.04.07    NACL-ÌçÏÆ    09/12/09  ½ÐÍè¹â»»Äê¡ÊºÞ¡Ë¤ÎÂÐ±þ
      *  04.04.08    NACL-ÌçÏÆ    10/01/13  ºÞ¤Þ¤È¤á¹àÌÜÃÍ¤Î·×»»½¤Àµ
      *  04.04.09    NACL-ÌçÏÆ    10/03/23  ²þÀµÂÐ±þ
      *                                     ¡Ê³°Çñ¶èÊ¬£µ¤ÎÂÐ±þ¡Ë
      *                                     ¡Ê³°Çñ¶èÊ¬£´¤Î¾ò·ïÄÉ²Ã¡Ë
      *                                     ¡Ê³°Çñ¶èÊ¬£´ÆÃÄêÆþ±¡´ðËÜÎÁ¤ÎÂÐ±þ¡Ë
      *  04.04.10    NACL-ÌçÏÆ    10/03/25  ²þÀµÂÐ±þ
      *                                     ¡ÊÍ½Ìó¥³¡¼¥ÉÆÉ¤ßÈô¤Ð¤·ÊÔ½¸¡Ë
      *  04.04.11    NACL-ÌçÏÆ    10/03/29  ²þÀµÂÐ±þ
      *                                     ¡Ê²èÁü¿ÇÃÇ£²²óÌÜ°Ê¹ß¸º»»ÊÔ½¸¡Ë
      *                                     ¡Ê¥³¡¼¥ÉÄÉ²Ã¡Ë
      *  04.04.12    NACL-ÌçÏÆ    10/04/06  ²þÀµÂÐ±þ
      *                                     ¡Ê¥³¥á¥ó¥È¼«Æ°µ­ºÜ¡Ë
      *  04.04.13    NACL-ÌçÏÆ    10/05/14  Æþ±¡ÎÁ²Ã»»ºÞÈÖ¹æÊÑ´¹
      *  04.04.14    NACL-ÌçÏÆ    10/05/21  ³°Çñ»þ¤ÎÆþ±¡ÎÁ²Ã»»ÊÔ½¸½¤Àµ
      *  04.04.15    NACL-ÌçÏÆ    10/06/01  Â¾°åÎÅµ¡´Ø¼õ¿ÇÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE200-FILE     ASSIGN  RECE200PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE200-KEY
                                   FILE    STATUS  IS  STS-RECE200.
           SELECT  RECE220-FILE     ASSIGN  RECE220PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE220-KEY
                                   FILE    STATUS  IS  STS-RECE220.
           SELECT  RECE230-FILE     ASSIGN  RECE230PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE230-KEY
                                   FILE    STATUS  IS  STS-RECE230.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE200-FILE.
       01  RECE200-REC. 
           COPY    "CPRCF200.INC".
      *
       FD  RECE220-FILE.
       01  RECE220-REC. 
           COPY    "CPRCF220.INC".
      *
       FD  RECE230-FILE.
       01  RECE230-REC. 
           COPY    "CPRCF230.INC".
      *
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     ¥ì¥»¥×¥ÈÌÀºÙ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE200//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE220//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE230//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPERRFL.INC"
                                   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE200         PIC X(02).
           03  STS-RECE220         PIC X(02).
           03  STS-RECE230         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE200         PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-DAY21           PIC 9(01).
           03  FLG-DAY22           PIC 9(01).
           03  FLG-NYUKHN          PIC 9(01).
           03  FLG-TENTEKI         PIC 9(01).
           03  FLG-WRITE230        PIC 9(01).
           03  FLG-WRITE230X       PIC 9(01).
           03  FLG-SIKIBETU        PIC 9(01).
           03  FLG-ZANHAIKI        PIC 9(01).
           03  FLG-FORMAL-KISAI    PIC 9(01).
           03  FLG-RECE230-ZAN     PIC 9(01).
           03  FLG-REWRITE         PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-NYUINRYO        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-HOUSIJI-COM     PIC 9(01).
           03  FLG-OSIN-COM        PIC 9(01).
           03  FLG-SYOBYO-COM      PIC 9(01).
           03  FLG-RYOYO-COM       PIC 9(01).
           03  FLG-SYODOU-COM      PIC 9(01).
           03  FLG-KENSA-COM       PIC 9(01).
           03  FLG-RINYAK          PIC 9(01).
           03  FLG-TENTEKIMATOME   PIC 9(01).
           03  FLG-TAHOKEN         PIC 9(01).
           03  FLG-KHNRYO-SKJ      PIC 9(01).
           03  FLG-HOUSEI-COM      PIC 9(01).
           03  FLG-HOUMON-COM      PIC 9(01).
           03  FLG-SYUNOU-ARI      PIC 9(01).
           03  FLG-FORMALNAME      PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-IN-COMMIT       PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-OUT1            PIC 9(06).
           03  CNT-OUT2            PIC 9(06).
           03  CNT-IN              PIC 9(06).
           03  CNT-OUT3            PIC 9(06).
           03  CNT-DAY21           PIC 9(06).
           03  CNT-DAY22           PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(03).
           03  IDX2                PIC 9(03).
           03  IDX3                PIC 9(03).
           03  IDX4                PIC 9(03).
           03  IDX5                PIC 9(03).
           03  IDX6                PIC 9(03).
           03  IDX8                PIC 9(02).
           03  IDX9                PIC 9(02).
           03  IDXA                PIC 9(01).
           03  IDXX                PIC 9(02).
           03  IDY                 PIC 9(03).
           03  IDY3                PIC 9(03).
           03  IDW                 PIC 9(03).
           03  IDW2                PIC 9(03).
           03  IDW3                PIC 9(03).
           03  IDW4                PIC 9(03).
           03  IDW5                PIC 9(03).
           03  IDW6                PIC 9(03).
           03  ID1                 PIC 9(03).
           03  ID4                 PIC 9(03).
           03  NUM-IDX             PIC 9(01).
           03  SYUKEI-IDX          PIC 9(03).
           03  SYUKEI-IDX2         PIC 9(03).
           03  SYUKEI-IDX3         PIC 9(03).
           03  SYUKEI-IDX4         PIC 9(03).
           03  SYUKEI-IDX5         PIC 9(03).
           03  IDV                 PIC 9(02).
           03  IDV2                PIC 9(03).
           03  IDU                 PIC 9(03).
           03  SYSKAN-IDX          PIC 9(03).
           03  SYSKAN-IDX2         PIC 9(03).
           03  FUKA-IDX            PIC 9(03).
      *
      *    ºÞ¾ðÊóÂàÈòÎÎ°è
       01  WK-ZAIKAISU             PIC S9(07).
       01  WK-ZAITEN               PIC S9(08).
       01  WK-SYUTEN               PIC S9(08).
       01  WK-KIZAITEN             PIC S9(08).
       01  WK-YKZTEN               PIC S9(08).
      *
      *    ¥¨¥é¡¼¥á¥Ã¥»¡¼¥¸ÊÔ½¸ÎÎ°è
       01  ERR-EDIT-AREA.
           03  ERR-PTNUM           PIC X(20).
           03  ERR-ERRMSG          PIC X(80).
           03  ERR-HEN-ERRMSG      PIC X(200).
      *
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¸¡º÷ÍÑÂÐ¾ÝÆüÎÎ°è
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY        PIC 9(02).
           03  WK-TAIHI-FLG        PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM  PIC 9(06).
               05  WK-TAIHI-SRYD   PIC 9(02).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY1-HYOKETU-SRYCD              PIC 9(09).
               05  WRK-DAY1-HYOKETU-TEN                PIC 9(09)V99.
               05  WRK-DAY1-KSN-SRYCD                  PIC 9(09).
               05  WRK-DAY1-NYUTENTTLKBN               PIC 9(03).
               05  WRK-DAY1-KHNTEN                     PIC 9(08).
               05  WRK-DAY1-KSNTEN                     PIC S9(08).
               05  WRK-DAY1-NYUKHNKSNKBN               PIC 9(03).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
      *
       01  WRK-DAY-TABLE2.
           03  WRK-DAY-T2                              OCCURS   31.
               05  WRK-DAY21                           PIC 9(04).
               05  WRK-DAY22                           PIC 9(04).
               05  WRK-DAY22-NYUTENTTLKBN              PIC 9(03).
               05  WRK-DAY22-NYUKHNRYO                 PIC 9(08).
               05  WRK-DAY22-NYUKHNKSNKBN              PIC 9(03).
               05  WRK-DAY24                           PIC 9(04).
      *
       01  WRK-GAI-TABLE.
      ***  03  WRK-GAI-TBL                             OCCURS   10.
           03  WRK-GAI-TBL                             OCCURS   14.
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
               05  WRK-GAI2-SRYCD.
                   07  WRK-GAI2-SRYCD-9                PIC 9(09).
               05  WRK-GAI2-ZAITEN                     PIC 9(08).
               05  WRK-GAI2-ZAIKAISU                   PIC 9(04).
               05  WRK-GAI2-SRYKBNFLG                  PIC 9(01).
               05  WRK-GAI2-KAISU-T.
                06  WRK-GAI2-KAISU-OCC                 OCCURS   31.
                 07  WRK-GAI2-KAISU                    PIC 9(01).
      *
       01  WRK-GAI-DATA                                PIC X(55).
      *
       01  WRK-SKJ200604-TABLE.
           03  WRK-SKJ200604-T                         OCCURS   31.
               05  WRK-SKJ200604-KBN                   PIC 9(01).
               05  WRK-SKJ200604-SRYCDTOTAL            PIC 9(14).
               05  WRK-SKJ200604-MEISAISU              PIC 9(02).
               05  WRK-SKJ200604-SURYOUTOTAL           PIC 9(02).
               05  WRK-SKJ200604-T3                    OCCURS    3.
                   07  WRK-SKJ200604-DAY               PIC 9(04).
               05  WRK-SKJ200604-DAY-KAN               PIC 9(01).
      *
       01  WRK-NYULIFE-TABLE.
           03  WRK-NYULIFE-AREA                        OCCURS   31.
               05  WRK-NYULIFE-KBN                     PIC X(01).
               05  WRK-NYULIFE-SKJFTN-SRYCD            PIC X(09).
               05  WRK-NYULIFE-SKJFTN-MONEY            PIC 9(07).
               05  WRK-NYULIFE-KANFTN-SRYCD            PIC X(09).
               05  WRK-NYULIFE-KANFTN-MONEY            PIC 9(07).
      *
       01  WRK-TENTEKI2-TABLE.
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
               05  WRK-ORCSTEN-SURYO                   PIC 9(05)V9(03).
               05  WRK-ORCSTEN-COM-MEI                 PIC X(80).
           03  WRK-ORCSTEN-T2                          OCCURS   31.
               05  WRK-ORCSTEN-DAY                     PIC 9(03).
           03  WRK-ORCSTEN-ZAIKAISU                    PIC 9(04).
      *
      *    »ÜÀß´ð½à¾ðÊóÂàÈòÍÑÎÎ°è
       01  TBL-SYS1006-T.
           03  TBL-SYS1006-OCC                         OCCURS  100.
               05  TBL-SYS10061                        PIC X(610).
               05  TBL-SYS10062                        PIC X(610).
      *
      *    °åÎÅµ¡´Ø¡ÊÆþ±¡´ðËÜ¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC     OCCURS  100.
               05  TBL-SYS5000                         PIC X(610).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM  PIC X(06).
               05  WRK-PARA-TERMID PIC X(16).
               05  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-JOBID  PIC 9(07).
               05  WRK-PARA-SHELLID
                                   PIC X(08).
           03  WRK-PARA-PRTKBN     PIC X(01).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
           03  WRK-SYS-SRYYMD      PIC X(08).
      *
           03  WRK-ZENKAKU         PIC X(80).
           03  WRK-NUM.
               05  WRK-NUM-X       PIC X(10).
               05  FILLER          PIC X(70).
           03  WRK-NUM-9           PIC 9(10). 
           03  WRK-STIDX           PIC 9(02).
           03  WRK-EDIDX           PIC 9(02).
           03  WRK-SRYCD-X.
               05  WRK-SRYCD       PIC 9(09).
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-ZAINUM-CNT      PIC 9(03).
           03  WRK-ZAINUM-CNT2     PIC 9(03).
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-RENNUM2         PIC 9(02).
           03  WRK-TANICD          PIC 9(03).
           03  WRK-TANIMOJISU      PIC 9(01).
           03  WRK-TANINAME        PIC X(24).
      *
           03  WRK-SRYSURYO        PIC 9(05)V9(03).
           03  WRK-SRYSURYO2       PIC 9(05).
           03  WRK-SRYSURYO3       PIC 9(05).
      *
           03  WRK-FILMTEN         PIC 9(07)V9(02).
           03  WRK-FILMTEN2        PIC 9(07)V9(02).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-SRYYM-5000      PIC X(06).
      *
           03  WRK-SYS5000-COUNT   PIC 9(02).
      *
           03  WRK-HYOKETU-TEN     PIC 9(09)V99.
           03  WRK-HYOKETU-SRYCD   PIC 9(09).
           03  WRK-KSN-SRYCD       PIC 9(09).
      *
           03  WRK-NYUTENTTLKBN    PIC 9(03).
      *
           03  WRK-KHNTEN          PIC 9(08).
           03  WRK-KSNTEN          PIC S9(08).
           03  WRK-GAIHAKU-SRYCD   PIC X(09).
           03  WRK-GAIHAKU-KHNTEN  PIC 9(08).
           03  WRK-NYUKHNKSNKBN    PIC 9(03).
      *
           03  WRK-SHOKUJIRYOYO-OCC.
               05  WRK-SHOKUJIRYOYO   PIC 9(01)
                                      OCCURS 31.
      *
           03  WRK-SISETU613       PIC 9(01).
           03  WRK-SISETU695       PIC 9(01).
      *
      *    ¿ô»úÈ¾³ÑÁ´³ÑÊÑ´¹ÍÑ
           03  WRK-CHAR-IDX PIC 9(02).
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
           03  WRK-TENTEKI2-SRYCD-X.
               05  WRK-TENTEKI2-SRYCD          PIC  X(09)
                                               OCCURS    5.
      *
           03  WRK-SYOKUJIKEI                  PIC  9(07).
      *
           03  WRK-STR                         PIC  X(200).
           03  WRK-LEN                         PIC  9(05).
           03  WRK-SHO                         PIC  9(05).
           03  WRK-AMARI                       PIC  9(05).
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
      *
      *    ÉÂ¾²¿ôÊÔ½¸¥Õ¥é¥°ÊÔ½¸¤Î°Ù¤Î¥³¡¼¥É¡ÊÆÃÄê¼À´µÎÅÍÜ»ØÆ³ÎÁÅù¡Ë
           03  CONS-SIDORYO1       PIC X(09) VALUE "113001810".
           03  CONS-SIDORYO2       PIC X(09) VALUE "113001910".
           03  CONS-SIDORYO3       PIC X(09) VALUE "113002010".
           03  CONS-SIDORYO4       PIC X(09) VALUE "113700610".
           03  CONS-SIDORYO5       PIC X(09) VALUE "113700710".
           03  CONS-SIDORYO6       PIC X(09) VALUE "113700810".
      *
      *    ºß»Ù±ç¥Õ¥é¥°ÊÔ½¸¤Î°Ù¤Î¥³¡¼¥É
           03  CONS-ZAISIEN1       PIC X(09) VALUE "114011570".
           03  CONS-ZAISIEN2       PIC X(09) VALUE "114011670".
           03  CONS-ZAISIEN3       PIC X(09) VALUE "114011770".
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
      *    ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ
           03  CONS-HINKAI-COMMENT   PIC X(09)  VALUE  "114009410".
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
           03  CONS-SYODOU-COMMENT   PIC X(09)  VALUE  "111011810".
      *    Ê£½é»»Äê¿ÇÎÅ²Ê¡§
           03  CONS-SYODOU2-COMMENT  PIC X(09)  VALUE  "830000024".
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
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE8-COMMENT   PIC X(09)  VALUE  "180032410".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE9-COMMENT   PIC X(09)  VALUE  "180032510".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE10-COMMENT  PIC X(09)  VALUE  "180032610".
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
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-UNDOKI7-COMMENT  PIC X(09)  VALUE  "180032710".
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
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯
           COPY    "CPTTNS.INC".
      *
       01  WRK-RECE200-REC. 
           COPY    "CPRCF200.INC"  REPLACING  //RECE200//
                                   BY         //WRK-RECE200//.
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
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK200501.INC".
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
           COPY    "CPORCSKANACHK.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *    ¿ôÃÍÊÑ´¹¥µ¥Ö
           COPY    "CPORCSNUM.INC".
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *    Æþ±¡ÅÀÅ©°ì³ç¹¹¿·¥µ¥Ö
           COPY    "CPORCSTENTEK.INC".
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
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
                                       WRK-AREA
                                       TTNS-AREA
                                       FLG-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-PRTKBN
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
               MOVE    "ORCR1220"      TO  JOB-PGID
               MOVE    "¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×"
                                       TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
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
                MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key10"             TO  MCP-PATHNAME
                PERFORM   900-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key10"         TO  MCP-PATHNAME
                    PERFORM 960-KANRIMST-READ-SEC
                    IF      FLG-SYSKANRI     =   ZERO
                       MOVE   MCPDATA-REC   TO  SYS-1006-REC
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
                END-IF
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key10"             TO  MCP-PATHNAME
                PERFORM   900-CLOSE-SEC
              END-PERFORM
           ELSE
              MOVE     SPACE              TO  TBL-SYS1006-T
              INITIALIZE                      TBL-SYS1006-T
      *
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    2
                 MOVE     ZERO            TO  SYSKAN-IDX2
                 INITIALIZE                   SYSKANRI-REC
                 MOVE    "1006"           TO  SYS-KANRICD
                 MOVE    IDXA             TO  SYS-KBNCD
                 MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                 MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key8"           TO  MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF      MCP-RC           =   ZERO
                     MOVE    "tbl_syskanri"   TO  MCP-TABLE
                     MOVE    "key8"           TO  MCP-PATHNAME
                     PERFORM  960-KANRIMST-READ-SEC
                     PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                               OR    (SYSKAN-IDX2  >=  100)
      *
                       MOVE    MCPDATA-REC   TO  SYS-1006-REC
                       EVALUATE  IDXA
                         WHEN     1
                           ADD     1     TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                         TO  TBL-SYS10061(SYSKAN-IDX2)
                         WHEN     2
                           ADD     1     TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                         TO  TBL-SYS10062(SYSKAN-IDX2)
                       END-EVALUATE
      *
                       MOVE    "tbl_syskanri"   TO  MCP-TABLE
                       MOVE    "key8"           TO  MCP-PATHNAME
                       PERFORM  960-KANRIMST-READ-SEC
      *
                     END-PERFORM
                 END-IF
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key8"           TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
              END-PERFORM
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *
             MOVE      WRK-PARA-SRYYM    TO  WRK-SRYYM-5000
             PERFORM   1001-SYS-5000-CHK-SEC
      *      ¿©»ö¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
             INITIALIZE                   WRK-SHOKUJIRYOYO-OCC
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
                 PERFORM  960-KANRIMST-READ-SEC
                 IF      FLG-SYSKANRI        =   ZERO
                   MOVE     MCPDATA-REC  TO  SYS-5000-REC
                   PERFORM  1002-SYSKAN-HENSYU-SEC
                   IF        WRK-SYS5000-COUNT    =     ZERO
                      CONTINUE
                   ELSE
                      PERFORM  1003-SYS-5000-GET-SEC
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
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key10"           TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
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
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"  TO  MCP-TABLE
                 MOVE    "key8"          TO  MCP-PATHNAME
                 PERFORM 960-KANRIMST-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                           OR    (SYSKAN-IDX   >=  100)
      *
                   MOVE    MCPDATA-REC  TO  SYS-5000-REC
                   ADD     1            TO  SYSKAN-IDX
                   MOVE   SYS-5000-REC  TO  TBL-SYS5000(SYSKAN-IDX)
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key8"          TO  MCP-PATHNAME
                   PERFORM 960-KANRIMST-READ-SEC
      *
                 END-PERFORM
             ELSE
                  MOVE "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                         TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
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
           MOVE    "tbl_syskanri"   TO  MCP-TABLE
           MOVE    "key10"          TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI     =   ZERO
                   MOVE   MCPDATA-REC  TO  SYS-200501-REC
               ELSE
                   INITIALIZE              SYS-200501-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-200501-REC
           END-IF
           IF      SYS-200501-COMMENT-NYUINRYO-KSIKBN
                                    =   SPACE
               MOVE    "1"   TO  SYS-200501-COMMENT-NYUINRYO-KSIKBN
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           MOVE    "RECE700"           TO  RECE200PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE200PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE200PARA-HOSPNUM
           MOVE    "RECE720"           TO  RECE220PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE220PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE220PARA-HOSPNUM
           MOVE    "RECE730"           TO  RECE230PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE230PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE230PARA-HOSPNUM
      *
           OPEN    OUTPUT              RECE220-FILE
           CLOSE                       RECE220-FILE
           OPEN    OUTPUT              RECE230-FILE
           CLOSE                       RECE230-FILE
      *
           OPEN    I-O                 RECE200-FILE
           OPEN    OUTPUT              RECE220-FILE
           OPEN    I-O                 RECE230-FILE
      *
           PERFORM RECE200-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤ËÊ£¿ô¤Î°åÎÅ´ü´Ö¾ðÊó¤¬¤Ê¤¤¤«¸¡º÷¡Ê°åÎÅµ¡´Ø¸º»»¤Î³ÎÇ§¡Ë  *
      ******************************************************************
       1001-SYS-5000-CHK-SEC                 SECTION.
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
               PERFORM 960-KANRIMST-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1             TO  WRK-SYS5000-COUNT
                   MOVE   "tbl_syskanri" TO  MCP-TABLE
                   MOVE   "key11"        TO  MCP-PATHNAME
                   PERFORM 960-KANRIMST-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
      *
       1001-SYS-5000-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥«¥ó¤ÎÊÔ½¸
      *****************************************************************
       1002-SYSKAN-HENSYU-SEC                SECTION.
      *
           PERFORM   VARYING   IDX8   FROM  1  BY  1
                      UNTIL    IDX8    >   31
              EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *         Æþ±¡¿©»öÎÅÍÜ¡Ê£±¡Ë
                WHEN         "197000110"
                    MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX8)
      *         Æþ±¡¿©»öÎÅÍÜ¡Ê£²¡Ë
                WHEN         "197000710"
                    MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX8)
              END-EVALUATE
           END-PERFORM
      *
           .
       1002-SYSKAN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤Î°åÎÅ´ü´Ö¾ðÊó¼èÆÀ¡Ê°åÎÅµ¡´Ø¸º»»¾ðÊó¡Ë                  *
      ******************************************************************
       1003-SYS-5000-GET-SEC                 SECTION.
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
               PERFORM 960-KANRIMST-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   MCPDATA-REC  TO  SYS-5000-REC
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
      *
                 PERFORM    VARYING  IDX8  FROM  IDV  BY  1
                             UNTIL   IDX8    >   31
                      EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *                Æþ±¡¿©»öÎÅÍÜ¡Ê£±¡Ë
                       WHEN         "197000110"
                          MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX8)
      *                Æþ±¡¿©»öÎÅÍÜ¡Ê£²¡Ë
                       WHEN         "197000710"
                          MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX8)
                      END-EVALUATE
                 END-PERFORM
      *
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key11"          TO  MCP-PATHNAME
                 PERFORM 960-KANRIMST-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
      *
       1003-SYS-5000-GET-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM             UNTIL   FLG-RECE200 =   1
                               OR      FLG-END     =   1
      *      ÊÔ½¸½èÍý
             PERFORM 210-HENSYU-SEC
      *
             IF   FLG-RECE200  =  ZERO
               PERFORM RECE200-READ-SEC
               IF      CNT-IN-COMMIT   =   300
                   MOVE  ZERO              TO  CNT-IN-COMMIT
                   PERFORM 900-DBCOMMIT-SEC
                   PERFORM 900-DBSTART-SEC
               END-IF
      *
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
             END-IF
           END-PERFORM
      *
           MOVE    1                   TO  FLG-END
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
                               WRK-SKJ200604-TABLE
                               WRK-NYULIFE-TABLE
                               WRK-TENTEKI2-TABLE
      *
           PERFORM    2111-TENTEKIMATOME-HENSYU-SEC
      *
           PERFORM    2101-RECE220-HENSYU-SEC
           PERFORM    2102-RECE220-HENSYU2-SEC
           PERFORM    2103-RECE220-HENSYU3-SEC
           PERFORM    2104-FTN-SET-SEC
           PERFORM    2105-GOKEI-KEISAN-SEC
      *
           PERFORM    RECE200-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀÅ©¼êµ»²Ã»»¤Þ¤È¤áÊÔ½¸
      *****************************************************************
       2111-TENTEKIMATOME-HENSYU-SEC      SECTION.
      *
           INITIALIZE                   ORCSTENTEK2AREA
           MOVE    RECE200-HOSPNUM  TO  LNK-ORCSTEN2-HOSPNUM
           MOVE    RECE200-PTID     TO  LNK-ORCSTEN2-PTID
           MOVE    RECE200-SRYYM    TO  LNK-ORCSTEN2-SRYYM
           MOVE    RECE200-HKNCOMBI TO  LNK-ORCSTEN2-HKNCOMBI
           CALL    "ORCSTENTEK2"        USING
                                        ORCSTENTEK2AREA
                                        SPA-AREA
           IF      LNK-ORCSTEN2-RC  =   ZERO
               IF     (LNK-ORCSTEN2-TENTEK-ERRCD  = ZERO)
               OR     (LNK-ORCSTEN2-CHUJOU-ERRCD  = ZERO)
                   MOVE  LNK-ORCSTEN2-OUT-AREA
                                    TO  WRK-TENTEKI2-HENKYAKU-AREA
               END-IF
           END-IF
           .
       2111-TENTEKIMATOME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2101-RECE220-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE200-HOSPNUM      TO      ACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN    TO      ACCT-NYUGAIKBN
           MOVE    RECE200-PTID         TO      ACCT-PTID
           MOVE    RECE200-SRYYM        TO      ACCT-SRYYM
           MOVE    RECE200-HKNCOMBI     TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key22"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key22"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
      *
                   MOVE   ZERO          TO  FLG-TENTEKIMATOME
                   IF    ACCT-SRYKBN  =  "33"
                      PERFORM   21015-TENTEKIMATOME-CHECK-SEC
                   END-IF
      *
                   MOVE   ZERO          TO  FLG-RINYAK
      *
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)    AND
                        (FLG-TENTEKIMATOME  =    ZERO)
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
                                         FLG-KENSA-COM
      *
                      MOVE   ZERO   TO   WRK-FILMTEN
      *
                      MOVE   ZERO   TO   FLG-WRITE230
                      MOVE   ZERO   TO   FLG-TENTEKI
                      PERFORM   21011-RECE220-TENSOU-SEC
                      PERFORM   21012-RECE230-HENSYU-SEC
                      IF    FLG-WRITE230  =   1
                        PERFORM RECE220-WRITE-SEC
                      END-IF
                   ELSE
                      IF  (ACCT-ZAIKAISU  NOT =    ZERO)   AND
                          (ACCT-JIHOKBN       =   SPACE)   AND
                          (FLG-TENTEKIMATOME  =    ZERO)
                         IF ( ACCT-SRYKBN  =  "11" OR
                                              "33" OR "99" OR
                                              "90" OR "92"  )
                         OR ((RECE200-SRYYM >= "200804"    ) AND
                             (ACCT-SRYKBN  =  "14"         ))
                         OR ((RECE200-SRYYM >= "200804"    ) AND
                             (ACCT-SRYKBN  =  "21" OR "22" OR
                                              "23"         ) AND
                             (WRK-PARA-PRTKBN  =  "2"      ))
                            IF  (ACCT-SRYCDTOTAL(1:12)  =
                                               "000000999901")
                            AND (ACCT-MEISAISU      =   1    )
                                 CONTINUE
                            ELSE
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
                                                    FLG-KENSA-COM
      *
                                 MOVE   ZERO   TO   WRK-FILMTEN
      *
                                 MOVE   ZERO   TO   FLG-WRITE230
                                 MOVE   ZERO   TO   FLG-TENTEKI
                                 PERFORM   21011-RECE220-TENSOU-SEC
                                 PERFORM   21012-RECE230-HENSYU-SEC
                                 IF    FLG-WRITE230  =   1
                                   PERFORM RECE220-WRITE-SEC
                                 END-IF
                            END-IF
                         ELSE
                            PERFORM   21014-SINRYO-CHECK-SEC
                            IF   FLG-RINYAK  NOT =  ZERO
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
                                                    FLG-KENSA-COM
      *
                                 MOVE   ZERO   TO   WRK-FILMTEN
      *
                                 MOVE   ZERO   TO   FLG-WRITE230
                                 MOVE   ZERO   TO   FLG-TENTEKI
                                 PERFORM   21011-RECE220-TENSOU-SEC
                                 PERFORM   21012-RECE230-HENSYU-SEC
                                 IF    FLG-WRITE230  =   1
                                   PERFORM RECE220-WRITE-SEC
                                 END-IF
                            END-IF
                         END-IF
                      END-IF
                   END-IF
                   MOVE    "tbl_sryacct"     TO      MCP-TABLE
                   MOVE    "key22"           TO      MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SRYACCT SELECT ERR"
           END-IF
      *
           MOVE    "tbl_sryacct"      TO      MCP-TABLE
           MOVE    "key22"            TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2101-RECE220-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       21011-RECE220-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE220-REC
           INITIALIZE                           RECE220-REC
           MOVE    RECE200-KEY          TO      RECE220-KEY1
           MOVE    ACCT-SRYKBN          TO      RECE220-SRYKBN
           IF      ACCT-SRYKBN = "90" OR "92"
                 MOVE    "6"            TO      RECE220-ZAISKBKBN
           ELSE 
              IF   ACCT-SRYKBN = "97"
                 MOVE    "4"            TO      RECE220-ZAISKBKBN
              END-IF
           END-IF
           MOVE    ACCT-ZAINUM          TO      RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO      RECE220-PTNUM
      *
           MOVE    ACCT-SRYKA           TO      RECE220-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE220-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE220-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE220-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE220-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE220-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE220-MEISAISU
           MOVE    ACCT-ZAIKAISU        TO      RECE220-ZAIKAISU
           MOVE    ACCT-SYUTEN1         TO      RECE220-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE220-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE220-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE220-KIZAITEN
           IF      ACCT-SRYKBN    =  "70"
             COMPUTE  RECE220-SYUTEN1    =      RECE220-SYUTEN1
                                         -      RECE220-SYUTEN2
             COMPUTE  RECE220-KIZAITEN   =      RECE220-KIZAITEN
                                         +      RECE220-SYUTEN2
           END-IF
           MOVE    ACCT-DRCD            TO      RECE220-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE220-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE220-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX3   FROM    1    BY   1
                       UNTIL     IDX3   >       4
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >      31
                   MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE220-KAISU (IDX3 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (ACCT-DAY(1 IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF    FLG-RINYAK    NOT =    ZERO
                MOVE    FLG-RINYAK      TO      RECE220-SAFLG
           END-IF
      *
           .
       21011-RECE220-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       21012-RECE230-HENSYU-SEC               SECTION.
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
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE    ZERO         TO      FLG-ZANHAIKI
                                                FLG-FORMAL-KISAI
                   MOVE    ZERO         TO      WRK-RENNUM
                                                WRK-RENNUM2
                   MOVE    ZERO         TO      FLG-WRITE230X
                   PERFORM   210121-RECE230-TENSOU-SEC
                   IF   FLG-TENTEKI   =  ZERO
                   AND  FLG-WRITE230X =   1
                     PERFORM 210122-RECE200-HENSYU-SEC
                     PERFORM RECE230-WRITE-SEC
                     MOVE  1            TO      FLG-WRITE230
                   END-IF
                   IF   FLG-ZANHAIKI  =  1
                      PERFORM  RECE230-REWRITE-SEC
                   END-IF
                   IF  FLG-FORMAL-KISAI  =  1
                      PERFORM  RECE230-REWRITE2-SEC
                   END-IF
                   MOVE    "tbl_sryact"      TO      MCP-TABLE
                   MOVE    "key9"            TO      MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"      TO      MCP-TABLE
           MOVE    "key9"            TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           IF     WRK-FILMTEN  NOT =  ZERO
                COMPUTE    RECE220-SYUTEN2  =  WRK-FILMTEN + 0.5
           END-IF
      *
           .
       21012-RECE230-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       210121-RECE230-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE230-REC
           INITIALIZE                           RECE230-REC
           MOVE    RECE200-KEY          TO      RECE230-KEY1
           MOVE    SRY-ZAINUM           TO      RECE230-ZAINUM
           MOVE    SRY-RENNUM           TO      RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO      RECE230-PTNUM
      *
           MOVE    SRY-SRYKA            TO      RECE230-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE230-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE230-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE230-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
            MOVE   SRY-SRYCD(IDX2)      TO      WRK-SRYCD
            MOVE   RECE230-SRYYM        TO      WK-TAIHI-SRYYM
            MOVE   WK-TAIHI-DAY         TO      WK-TAIHI-SRYD
            PERFORM    900-TENSU-TBL-SEC
      *
            IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE220-MEISAISU  =  RECE220-MEISAISU
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
            OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"       )
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
                  COMPUTE   RECE220-MEISAISU    =  RECE220-MEISAISU
                                                -  1
                  MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
                  COMPUTE   RECE220-SRYCDTOTAL  =  RECE220-SRYCDTOTAL
                                                -  WRK-SRYCD
                  COMPUTE   RECE220-SURYOUTOTAL =  RECE220-SURYOUTOTAL
                                                -  1
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
              OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"       )
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
      *
               CONTINUE
      *
              ELSE
      *
               ADD     1                  TO    IDX4
      *
               MOVE    SRY-SRYCD(IDX2)    TO    RECE230-SRYCD(IDX4)
      *
               IF      SRY-SRYKBN         =     "13"
                   IF   SRY-SRYCD(IDX2)   =     CONS-SIDORYO1    OR
                                                CONS-SIDORYO2    OR
                                                CONS-SIDORYO3    OR
                                                CONS-SIDORYO4    OR
                                                CONS-SIDORYO5    OR
                                                CONS-SIDORYO6
                        MOVE   "1"        TO    RECE200-BEDSUFLG
                   END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)      =    CONS-GAIJINKEN
                  MOVE   "1"              TO    RECE200-KENTAIKASANFLG
                  MOVE   99               TO    RECE220-KNSJISGRPKBN
               END-IF
               IF     RECE220-KNSJISGRPKBN  =    ZERO
                 IF    TNS-KNSJISGRPKBN = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                  MOVE  TNS-KNSJISGRPKBN  TO    RECE220-KNSJISGRPKBN
                 END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN         =    "60" OR "64" )
               AND   ( TNS-KNSJISKBN      =     2           )
                   MOVE    TNS-KNSJISKBN        TO  RECE220-KNSJISKBN
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "14" OR "21" OR "22" OR "23" )
               AND   ( RECE220-ZAITEN    =     ZERO                 )
               AND   ( RECE220-SAFLG     =     SPACE                )
                  IF   SRY-SRYCD(IDX2)   =     "820000046"
                     MOVE     "1"         TO    RECE220-SAFLG
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "21" OR "22" OR "23" )
               AND   ( RECE220-ZAITEN     =     ZERO        )
                  IF     RECE220-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE220-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                      IF   RECE220-TOYAKUFLG = SPACE
                        MOVE    "2"       TO    RECE220-TOYAKUFLG
                      END-IF
                    ELSE
                        MOVE    "1"       TO    RECE220-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN        =     "14" )
               AND   ( RECE220-ZAITEN    =     ZERO )
                  IF     RECE220-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE220-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "148" OR "149"
                          EVALUATE  WRK-PARA-PRTKBN
                           WHEN      "1"
                             MOVE   "9"  TO    RECE220-TOYAKUFLG
                           WHEN      "2"
                             IF   RECE220-TOYAKUFLG = SPACE
                               MOVE "2"  TO    RECE220-TOYAKUFLG
                             END-IF
                          END-EVALUATE
                        END-IF
                    ELSE
                        EVALUATE  WRK-PARA-PRTKBN
                          WHEN      "1"
                            MOVE  "9"    TO    RECE220-TOYAKUFLG
                          WHEN      "2"
                            MOVE  "1"    TO    RECE220-TOYAKUFLG
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
                       COMPUTE  RECE220-RUIKEITOTAL  =
                                RECE220-RUIKEITOTAL  +
                              ( WRK-FUKA-VALUE    * WRK-SRYCD)
                       COMPUTE  WRK-FUKA-KINGAKU    =
                                WRK-FUKA-KINGAKU    +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-SRYSURYO(IDX2))
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE220-RUIKEITOTAL  =
                            RECE220-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *          END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2)  TO   RECE230-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2)  TO   RECE230-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE230-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)   TO   RECE230-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2)  TO   RECE230-INPUTNUM(IDX4)
      *
               IF      SRY-SRYKBN = "40" OR "50" OR "54" OR "60" OR "64"
                  IF   SRY-SRYCD(IDX2)(1:1)  =  "7"
                  AND  TNS-DATAKBN           =  "3"
                         COMPUTE WRK-FILMTEN2   =
                                (TNS-TEN  * SRY-SRYSURYO(IDX2) / 10)
                         IF   TNS-JGNTEN  NOT =  ZERO
                         AND  TNS-JGNTEN   <  WRK-FILMTEN2
                             COMPUTE  WRK-FILMTEN  =
                                      WRK-FILMTEN  +
                                      TNS-JGNTEN
                         ELSE
                             COMPUTE  WRK-FILMTEN  =
                                      WRK-FILMTEN  +
                                      WRK-FILMTEN2
                         END-IF
                  END-IF
               END-IF
      *
               IF      TNS-SRYKBN         =     "80"
                   IF  TNS-NYUTENTTLKBN   =     130
                       MOVE   "13"        TO    RECE220-SRYKBN
                                                RECE230-SRYKBN
                   END-IF
               END-IF
      *
               PERFORM  2101211-RECE230-TENSOU-COMCNT-HEN-SEC
      *
               IF  SRY-SRYCD(IDX2)        =     CONS-ZANHAIKI
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO
                                          +     0.999
                  COMPUTE RECE230-SRYSURYO(IDX4)
                                          =     WRK-SRYSURYO2
                                          -     WRK-SRYSURYO
                  IF  RECE230-SRYSURYO(IDX4)  NOT = ZERO
                    MOVE  WRK-TANICD      TO    RECE230-TANICD(IDX4)
                    MOVE  WRK-TANIMOJISU  TO    RECE230-TANIMOJISU(IDX4)
                    MOVE  WRK-TANINAME    TO    RECE230-TANINAME(IDX4)
                    IF    RECE230-RENNUM  >  1
                    AND   IDX4            =  1
                      MOVE    1           TO    FLG-ZANHAIKI
                      COMPUTE  WRK-RENNUM  =  RECE230-RENNUM  -  1
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3
                    ELSE
                      MOVE WRK-SRYSURYO2  TO  RECE230-SRYSURYO(IDX4 - 1)
                    END-IF
                  END-IF
               ELSE
                  MOVE    SRY-SRYSURYO(IDX2)
                                          TO  WRK-SRYSURYO
                  MOVE    TNS-TANICD      TO  RECE230-TANICD(IDX4)
                                              WRK-TANICD
                  MOVE    TNS-TANIMOJISU  TO  RECE230-TANIMOJISU(IDX4)
                                              WRK-TANIMOJISU
                  MOVE    TNS-TANINAME    TO  RECE230-TANINAME(IDX4)
                                              WRK-TANINAME
               END-IF
               MOVE    TNS-TENSIKIBETU    TO  RECE230-TENSIKIBETU(IDX4)
               MOVE    TNS-TEN            TO  RECE230-TEN(IDX4)
               MOVE    TNS-NYUTENTTLKBN   TO  RECE230-TENTTLKBN(IDX4)
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO  RECE230-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO  RECE230-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO  RECE230-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO  RECE230-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO  RECE230-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO  RECE230-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO  RECE230-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO  RECE230-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM
                                      TO RECE230-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE230-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE230-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM
                                      TO RECE230-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-SRYSYUKBN  TO RECE230-SRYSYUKBN-X  (IDX4)
               MOVE    TNS-KOKUJISKBKBN1
                                      TO RECE230-KOKUJISKBKBN1(IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE230-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE230-CHUKSNCD       (IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE230-CHUKSNTUBAN    (IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO RECE230-KENTAICOMMENT  (IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE230-KNSJISGRPKBN   (IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO RECE230-TOKUKIZAISBT1  (IDX4)
               MOVE    TNS-SANSOKBN   TO RECE230-SANSOKBN       (IDX4)
      *
               IF    (TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490")
               AND   (RECE200-SRYYM  <=  "201003"   )
                      MOVE   202      TO  RECE230-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE230-CDKBN-KOUBAN(IDX4)
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
                      MOVE   203      TO  RECE230-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE230-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               IF    (TNS-SRYCD       =  "170022290")
               AND   (RECE200-SRYYM  >=  "201004"   )
                      MOVE   "1"      TO  RECE220-GAZOGEN
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
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT   TO   RECE230-NAME(IDX4)
                                                    RECE230-INPUTCOMENT
                                                                (IDX4)
      *
                   IF      SRY-SRYCD (IDX2)(1:1)
                                                =   "8"
                       IF      PTCOM-INPUTCHI1     =   SPACE
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           MOVE    TNS-NAME        TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-MAX     >   ZERO )
                               COMPUTE IDXX    =   KANACHK-MAX +   1
                               MOVE    PTCOM-INPUTCOMENT (IDXX:)
                                                       TO
                                           RECE230-INPUTCOMENT   (IDX4)
                           ELSE
                               MOVE    PTCOM-INPUTCOMENT
                                                       TO
                                           RECE230-INPUTCOMENT   (IDX4)
                           END-IF
                       ELSE
                           INITIALIZE              ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           STRING  PTCOM-INPUTCHI1 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI2 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI3 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI4 DELIMITED  BY  SPACE
                                   INTO   KANACHK-MAE-INPUT
                           END-STRING
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF      KANACHK-RC      =   ZERO
                               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO
                                           RECE230-INPUTCOMENT   (IDX4)
                           END-IF
                       END-IF
                   END-IF
      *
      ***          MOVE   "1"                  TO    RECE220-COMCDFLG
               ELSE
                   MOVE    TNS-NAME        TO    RECE230-NAME(IDX4)
                                                 RECE230-INPUTCOMENT
                                                             (IDX4)
                   MOVE    TNS-YUKOKETA    TO    RECE230-YUKOKETA(IDX4)
               END-IF
      *
               IF    (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                        MOVE     "1"       TO    RECE220-COMCDFLG
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
                      IF  RECE230-NAME(IDX4)(IDV2:2)   =  "Æü"
                        MOVE   SPACE  TO  RECE220-FLG-KENSA-COMMENT
                        MOVE     1    TO  FLG-KENSA-COM
                      END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)(1:3)     =   "058"
                 MOVE  TNS-FORMALYUKOKETA TO  RECE230-YUKOKETA(IDX4)
                 MOVE  TNS-FORMALNAME     TO  RECE230-NAME(IDX4)
               END-IF
               IF  (SRY-SRYCD(IDX2)(1:1)    =   "7")
               AND (SRY-MEISKYFLG(IDX2)     =   "1")
                 IF   (SRY-SRYKBN  =   "70")
                 AND  (TNS-DATAKBN =   "3" )
                    IF    RECE230-RENNUM  >  1
                    AND   IDX4            =  1
                      MOVE    1       TO  FLG-FORMAL-KISAI
                      COMPUTE  WRK-RENNUM2 =  RECE230-RENNUM  -  1
                    ELSE
                      MOVE   "1"      TO  RECE230-RECEPTFLG(IDX4 - 1)
                    END-IF
                 ELSE
                    MOVE  "1"         TO  RECE230-RECEHENFLG(IDX4)
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
                  MOVE     "1"          TO  RECE230-RECEPTFLG(IDX4)
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
                    MOVE  SPACE TO RECE230-NAME(IDX4 - 1)
                    COMPUTE   IDW4  =  IDW3  -  14
                    MOVE  TNS-NAME(15:IDW4)
                                TO RECE230-NAME(IDX4 - 1)(1:IDW4)
                    COMPUTE   IDW5  =  IDW4  +   1
                    MOVE  "¤Ë¤Æ¿Ç»¡ÎÁ»»ÄêºÑ"
                                TO RECE230-NAME(IDX4 - 1)(IDW5:16)
                    COMPUTE   RECE230-YUKOKETA(IDX4 - 1) =
                              RECE230-YUKOKETA(IDX4 - 1) -
                              3                          +
                              IDW6
                  END-IF
               END-IF
      *
               IF  RECE230-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   200   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE230-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE230-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
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
                       PERFORM 2101212-RECE230-TENSOU-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE230-YUKOKETA(IDX4)  *  2
                                                   +   3
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE230-NAME(IDX4)(IDW:)
                       COMPUTE RECE230-YUKOKETA(IDX4)
                                           =   RECE230-YUKOKETA(IDX4)
                                               +   1
                                               +
                                              (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF
              END-IF
             END-IF
            END-IF  
           END-PERFORM
      *
           IF    IDX4    NOT =   ZERO
               MOVE    1     TO    FLG-WRITE230X
           END-IF
      *
           .
       210121-RECE230-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       2101211-RECE230-TENSOU-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE220-SRYKBN      =     "11"
      *       ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
              IF   SRY-SRYCD(IDX2)    =     CONS-SYODOU-COMMENT
              AND  FLG-SYODOU-COM     =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-SYODOU-COMMENT
                   MOVE   SRY-SRYKA   TO    RECE220-SYODOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYODOU2-COMMENT
                   MOVE  SPACE  TO    RECE220-FLG-SYODOU-COMMENT
                   MOVE  SPACE  TO    RECE220-SYODOU-SRYKA(1)
                   MOVE    1    TO    FLG-SYODOU-COM
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "40"
      *       ²ðÃ£¸£°ú¡ÊÈ¯¾ÉÆüµ­ºÜ¡Ë
              IF   RECE200-SRYYM      >=    "200604"
              AND  RECE200-SRYYM      <=    "200803"
                IF   SRY-SRYCD(IDX2)  =     CONS-KAITATU-COMMENT
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-KAITATU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE200-SRYYM     >=    "201004"
      *         ¿Í¹©¿ÕÂ¡Åù
                IF   SRY-SRYCD(IDX2)  =     CONS-JINZOU1-COMMENT
                                        OR  CONS-JINZOU2-COMMENT
                                        OR  CONS-JINZOU3-COMMENT
                                        OR  CONS-JINZOU4-COMMENT
                                        OR  CONS-JINZOU5-COMMENT
                                        OR  CONS-JINZOU6-COMMENT
                                        OR  CONS-JINZOU7-COMMENT
                                        OR  CONS-JINZOU8-COMMENT
                        MOVE   "1"    TO    RECE220-FLG-JINZOU-COMMENT
                END-IF
      *         ¶É½ê±¢°µÊÄº¿½èÃÖ
                IF   SRY-SRYCD(IDX2)  =     CONS-KYOKUIN1-COMMENT
                                        OR  CONS-KYOKUIN2-COMMENT
                                        OR  CONS-KYOKUIN3-COMMENT
                                        OR  CONS-KYOKUIN4-COMMENT
                        MOVE   "1"    TO    RECE220-FLG-KYOKUIN-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "80"
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
      ****              MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-RIHABIRI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SINDAI1-COMMENT  OR
                                            CONS-SINDAI2-COMMENT  OR
                                            CONS-SINDAI3-COMMENT  OR
                                            CONS-SINDAI4-COMMENT  OR
                                            CONS-KOKYUKI1-COMMENT OR
                                            CONS-KOKYUKI2-COMMENT OR
                                            CONS-KOKYUKI3-COMMENT OR
                                            CONS-KOKYUKI4-COMMENT OR
                                            CONS-NOUKE1-COMMENT   OR
                                            CONS-NOUKE2-COMMENT   OR
                                            CONS-NOUKE3-COMMENT   OR
                                            CONS-NOUKE4-COMMENT   OR
                                            CONS-NOUKE7-COMMENT   OR
                                            CONS-NOUKE8-COMMENT   OR
                                            CONS-NOUKE9-COMMENT   OR
                                            CONS-NOUKE10-COMMENT  OR
                                            CONS-UNDOKI1-COMMENT  OR
                                            CONS-UNDOKI2-COMMENT  OR
                                            CONS-UNDOKI3-COMMENT  OR
                                            CONS-UNDOKI4-COMMENT  OR
                                            CONS-UNDOKI7-COMMENT  OR
                                            CONS-SYOGAI1-COMMENT  OR
                                            CONS-SYOGAI2-COMMENT  OR
                                            CONS-SYOGAI3-COMMENT  OR
                                            CONS-GANKNJ-COMMENT
      ****              MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-RIHABIRI-COMMENT
              END-IF
              EVALUATE      SRY-SRYCD(IDX2)
                WHEN     CONS-SINDAI5-COMMENT
                        MOVE   "2"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     CONS-SINDAI6-COMMENT
                        MOVE   "3"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     CONS-KOKYUKI5-COMMENT
                        MOVE   "4"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     CONS-KOKYUKI6-COMMENT
                        MOVE   "5"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     CONS-NOUKE5-COMMENT
                        MOVE   "6"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     CONS-NOUKE6-COMMENT
                        MOVE   "7"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     CONS-UNDOKI5-COMMENT
                        MOVE   "8"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     CONS-UNDOKI6-COMMENT
                        MOVE   "9"    TO    RECE220-RIHABIRI-COMMENT
              END-EVALUATE
      *       ¥ê¥Ï¥Ó¥ê¡ÊÈ¯¾ÉÆüµ­ºÜ¡Ë
              IF   RECE200-SRYYM      >=    "200604"
                IF   SRY-SRYCD(IDX2)  =     CONS-SINDAI1-COMMENT
                                      OR    CONS-SINDAI2-COMMENT
                                      OR    CONS-SINDAI3-COMMENT
                                      OR    CONS-SINDAI4-COMMENT
                                      OR    CONS-SINDAI5-COMMENT
                                      OR    CONS-SINDAI6-COMMENT
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-SINDAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-KOKYUKI1-COMMENT
                                      OR    CONS-KOKYUKI2-COMMENT
                                      OR    CONS-KOKYUKI3-COMMENT
                                      OR    CONS-KOKYUKI4-COMMENT
                                      OR    CONS-KOKYUKI5-COMMENT
                                      OR    CONS-KOKYUKI6-COMMENT
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-KOKYUKI-COMMENT
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
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-NOUKE-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-UNDOKI1-COMMENT
                                      OR    CONS-UNDOKI2-COMMENT
                                      OR    CONS-UNDOKI3-COMMENT
                                      OR    CONS-UNDOKI4-COMMENT
                                      OR    CONS-UNDOKI5-COMMENT
                                      OR    CONS-UNDOKI6-COMMENT
                                      OR    CONS-UNDOKI7-COMMENT
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-UNDOKI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-NANBYO-COMMENT
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-NANBYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-SYOGAI1-COMMENT
                                      OR    CONS-SYOGAI2-COMMENT
                                      OR    CONS-SYOGAI3-COMMENT
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-SYOGAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-SESSYOKU-COMMENT
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-SESSYOKU-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     CONS-YAKUKAN-COMMENT1 OR
                                            CONS-YAKUKAN-COMMENT2 OR
                                            CONS-YAKUKAN-COMMENT3
                   MOVE   "1"   TO    RECE220-FLG-YAKUKAN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-NYUEIYO-COMMENT
                   MOVE   "1"   TO    RECE220-FLG-NYUEIYO-COMMENT
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     CONS-HOUMON-COMMENT1  OR
                                            CONS-HOUMON-COMMENT2)
              AND (FLG-HOUMON-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE220-FLG-HOUMON-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUMON-COMMENT3
                   MOVE  SPACE  TO    RECE220-FLG-HOUMON-COMMENT
                   MOVE    1    TO    FLG-HOUMON-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO-COMMENT
              AND  FLG-SYOBYO-COM     =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO2-COMMENT
                                      OR    CONS-SYOBYO3-COMMENT
                   MOVE  SPACE  TO    RECE220-FLG-SYOBYO-COMMENT
                   MOVE    1    TO    FLG-SYOBYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO-COMMENT
              AND  FLG-RYOYO-COM      =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO2-COMMENT
                   MOVE  SPACE  TO    RECE220-FLG-RYOYO-COMMENT
                   MOVE    1    TO    FLG-RYOYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-TAIIN-COMMENT
                   MOVE   "1"   TO    RECE220-FLG-TAIIN-COMMENT
              END-IF
      *
              IF   RECE200-SRYYM     >=    "201004"
      *         ²ð¸î»Ù±çÏ¢·È»ØÆ³ÎÁ
                IF   SRY-SRYCD(IDX2)  =     CONS-KAISIEN-COMMENT
                        MOVE   "1"    TO    RECE220-FLG-KAISIEN-COMMENT
                END-IF
      *         ½¸ÃÄ±ÉÍÜ¿©»ö»ØÆ³ÎÁ
                IF   SRY-SRYCD(IDX2)  =     CONS-SYUEIYO-COMMENT
                        MOVE   "1"    TO    RECE220-FLG-SYUEIYO-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI-COMMENT
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI2-COMMENT
                   MOVE  SPACE  TO    RECE220-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-HINKAI-COMMENT
                   MOVE   "1"   TO    RECE220-FLG-HINKAI-COMMENT
              END-IF
      *
      ***     IF  (SRY-SRYCD(IDX2)    =     CONS-OSIN-COMMENT
      ***                             OR    CONS-OSIN2-COMMENT)
      ***     AND (FLG-OSIN-COM       =     ZERO              )
      ***          MOVE   "1"   TO    RECE220-FLG-OSIN-COMMENT
      ***     END-IF
      ***     IF   SRY-SRYCD(IDX2)    =     CONS-OSIN3-COMMENT
      ***          MOVE  SPACE  TO    RECE220-FLG-OSIN-COMMENT
      ***          MOVE    1    TO    FLG-OSIN-COM
      ***     END-IF
      *
              IF     WRK-SISETU613      =      1
                IF     RECE200-SRYYM    >=     "200604"
                  IF   SRY-SRYCD(IDX2)  =     CONS-ZAISIEN1    OR
                                              CONS-ZAISIEN2    OR
                                              CONS-ZAISIEN3
                       MOVE   "1"   TO    RECE200-COMCNTFLG
                       MOVE   "1"   TO    RECE220-FLG-ZAISIEN-COMMENT
                  END-IF
                END-IF
              ELSE
                IF   WRK-SISETU695      =      1
                  IF   RECE200-SRYYM    >=     "200804"
                    IF SRY-SRYCD(IDX2)  =     CONS-ZAISIEN1    OR
                                              CONS-ZAISIEN2    OR
                                              CONS-ZAISIEN3
                       MOVE   "1"   TO    RECE200-COMCNTFLG
                       MOVE   "2"   TO    RECE220-FLG-ZAISIEN-COMMENT
                    END-IF
                  END-IF
                END-IF
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "50"
              IF   RECE220-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE220-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SHUJUTU-COMMENT
                   MOVE   "1"   TO    RECE220-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF   RECE200-SRYYM     >=    "201004"
            IF     RECE220-SRYKBN     =     "54"
              IF   RECE220-SYUTEN1    =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-MASUI-COMMENT
              END-IF
            END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "70"
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
              AND (RECE200-SRYYM      <     "201004"            )
                   MOVE   "1"   TO    RECE220-FLG-CTMRI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     CONS-CTMRI-COMMENT1  OR
                                            CONS-CTMRI-COMMENT2  OR
                                            CONS-CTMRI-COMMENT4  OR
                                            CONS-CTMRI-COMMENT6  OR
                                            CONS-CTMRI-COMMENT7  OR
                                            CONS-CTMRI-COMMENT18)
              AND (RECE200-SRYYM      >=    "201004"            )
                   MOVE   "1"   TO    RECE220-FLG-CTMRI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "80"
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSEI-COMMENT
              AND  FLG-HOUSEI-COM     =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-HOUSEI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSEI-COMMENT2
                   MOVE  SPACE  TO    RECE220-FLG-HOUSEI-COMMENT
                   MOVE    1    TO    FLG-HOUSEI-COM
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "90"  OR  "92"
              IF   SRY-SRYCD(IDX2)    =     CONS-KAKURI-COMMENT
                   MOVE   "1"   TO    RECE220-FLG-KAKURI-COMMENT
              END-IF
      *
              IF   RECE200-SRYYM     >=    "201004"
      *         ±ÉÍÜ¥µ¥Ý¡¼¥È¥Á¡¼¥à²Ã»»
                IF   SRY-SRYCD(IDX2)  =     CONS-EISAPO-COMMENT
                        MOVE   "1"    TO    RECE220-FLG-EISAPO-COMMENT
                END-IF
      *         ¸ÆµÛ¥±¥¢¥Á¡¼¥à²Ã»»
                IF   SRY-SRYCD(IDX2)  =     CONS-KOKKEA-COMMENT
                        MOVE   "1"    TO    RECE220-FLG-KOKKEA-COMMENT
                END-IF
      *         ½Å¾É¼Ô²Ã»»¡ÊÀº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ¡Ë
                IF   SRY-SRYCD(IDX2)  =     CONS-JYUSYO-COMMENT
                        MOVE   "1"    TO    RECE220-FLG-JYUSYO-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "60" OR "64"
              IF   (SRY-SRYCD(IDX2)   =     CONS-KENSA1-COMMENT
                                      OR    CONS-KENSA2-COMMENT
                                      OR    CONS-KENSA3-COMMENT)
              AND  (FLG-KENSA-COM     =     ZERO               )
                   MOVE   "1"   TO    RECE220-FLG-KENSA-COMMENT
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN   =  "14" OR "40" OR "50" OR "54"
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
                   MOVE   "1"   TO    RECE220-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           .
       2101211-RECE230-TENSOU-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤ÎÃ±²ÁÊÔ½¸½èÍý
      *****************************************************************
       2101212-RECE230-TENSOU-TANKA-HEN-SEC     SECTION.
      *
      ***  IF      TNS-TEN         =   ZERO
      ***      GO  TO  2101212-RECE230-TENSOU-TANKA-HEN-EXT
      ***  END-IF
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
       2101212-RECE230-TENSOU-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       210122-RECE200-HENSYU-SEC               SECTION.
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF    SRY-SRYKBN  =  "31" OR "32" OR "33" OR
                                            "40" OR "50" OR "54" OR
                                            "60" OR "70" OR "80" OR
                                            "14" OR "64"
                           MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                           COMPUTE  WK-YKZTEN  =  ACCT-KIZAITEN
                                               +  ACCT-YKZTEN
                           IF  SRY-SRYKBN  =  "70"
                             COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                 -  ACCT-SYUTEN2
                             COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                 +  ACCT-SYUTEN2
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
                     PERFORM   2101221-RECE-LEFT-SET-SEC
               END-IF
               IF    SRY-SRYKBN   =   "21"   OR   "22"   OR   "23"   OR
                                      "31"   OR   "32"   OR   "33"   OR
                                      "40"   OR   "50"   OR   "54"   OR
                                      "60"   OR   "70"   OR   "13"   OR
                                      "14"   OR   "64"
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
           .
      *
       210122-RECE200-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2101221-RECE-LEFT-SET-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE230-SRYYM       TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   TNS-CHUKSNTUBAN    =   "0"
                        ADD   WK-ZAIKAISU      TO   RECE200-KAISU (1)
                   END-IF
      *************£±¡§»þ´Ö³°¡¡£²¡§¿¼Ìë¡¡£³¡§µÙÆü¡¡£´¡§»þ´Ö³°¡¦¿¼Ìë
      *************£µ¡§»þ´Ö³°¡¦µÙÆü¡¡£¶¡§µÙÆü¡¦¿¼Ìë
      *************£·¡§»þ´Ö³°¡¦µÙÆü¡¦¿¼Ìë
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             EVALUATE   RECE200-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "1"     TO   RECE200-SHOSHINTIME
                               WHEN   "1"
                                  CONTINUE
                               WHEN   "2"
                                  MOVE  "4"     TO   RECE200-SHOSHINTIME
                               WHEN   "3"
                                  MOVE  "5"     TO   RECE200-SHOSHINTIME
                               WHEN   "4"
                                  CONTINUE
                               WHEN   "5"
                                  CONTINUE
                               WHEN   "6"
                                  MOVE  "7"     TO   RECE200-SHOSHINTIME
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                         WHEN    4
                             EVALUATE   RECE200-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "2"     TO   RECE200-SHOSHINTIME
                               WHEN   "1"
                                  MOVE  "4"     TO   RECE200-SHOSHINTIME
                               WHEN   "2"
                                  CONTINUE
                               WHEN   "3"
                                  MOVE  "6"     TO   RECE200-SHOSHINTIME
                               WHEN   "4"
                                  CONTINUE
                               WHEN   "5"
                                  MOVE  "7"     TO   RECE200-SHOSHINTIME
                               WHEN   "6"
                                  CONTINUE
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                         WHEN    3
                             EVALUATE   RECE200-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "3"     TO   RECE200-SHOSHINTIME
                               WHEN   "1"
                                  MOVE  "5"     TO   RECE200-SHOSHINTIME
                               WHEN   "2"
                                  MOVE  "6"     TO   RECE200-SHOSHINTIME
                               WHEN   "3"
                                  CONTINUE
                               WHEN   "4"
                                  MOVE  "7"     TO   RECE200-SHOSHINTIME
                               WHEN   "5"
                                  CONTINUE
                               WHEN   "6"
                                  CONTINUE
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                     END-EVALUATE
                   END-IF
                       COMPUTE    RECE200-TENSU(1 1)  =
                                  RECE200-TENSU(1 1)  +
                                 (TNS-TEN     *   ACCT-ZAIKAISU)
      *    °å³Ø´ÉÍý¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                       COMPUTE    RECE200-TENSU(2 1)  =
                                  RECE200-TENSU(2 1)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                       COMPUTE    RECE200-TENSU(3 1)  =
                                  RECE200-TENSU(3 1)  +
                                 (WK-SYUTEN   *   WK-ZAIKAISU)
                       COMPUTE    RECE200-TENSU(3 2)  =
                                  RECE200-TENSU(3 2)  +
                                 (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)     =    CONS-YAKTEIGEN
                           COMPUTE   RECE200-TENSU(4 2)  =
                                     RECE200-TENSU(4 2)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE200-KAISU(4)
                           COMPUTE   RECE200-TENSU(4 2)  =
                                     RECE200-TENSU(4 2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                           ADD   WK-ZAIKAISU   TO   RECE200-KAISU(4)
                           COMPUTE   RECE200-TENSU(4 2)  =
                                     RECE200-TENSU(4 2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE200-KAISU(5)
                   COMPUTE   RECE200-TENSU(5 2)  =
                             RECE200-TENSU(5 2)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE200-KAISU(6)
                   COMPUTE   RECE200-TENSU(6 2)  =
                             RECE200-TENSU(6 2)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE200-KAISU(7)
                   END-IF
                   COMPUTE   RECE200-TENSU(7 1)  =
                             RECE200-TENSU(7 1)  +
                            (TNS-TEN     *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE200-KAISU(8)
                   END-IF
                   COMPUTE   RECE200-TENSU(8 1)  =
                             RECE200-TENSU(8 1)  +
                            (TNS-TEN     *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE200-TENSU(9 1)  =
                             RECE200-TENSU(9 1)  +
                            (TNS-TEN     *   ACCT-ZAIKAISU)
      *    Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE200-KAISU(10)
                   COMPUTE   RECE200-TENSU(10 1)  =
                             RECE200-TENSU(10 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE200-TENSU(11 2)  =
                             RECE200-TENSU(11 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE200-KAISU(12)
                   END-IF
                   COMPUTE   RECE200-TENSU(12 1)  =
                             RECE200-TENSU(12 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE200-TENSU(13 2)  =
                             RECE200-TENSU(13 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE200-KAISU(14)
                   END-IF
                   COMPUTE   RECE200-TENSU(14 1)  =
                             RECE200-TENSU(14 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE200-TENSU(15 2)  =
                             RECE200-TENSU(15 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE200-KAISU(16)
                   END-IF
                   COMPUTE   RECE200-TENSU(16 1)  =
                             RECE200-TENSU(16 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                         COMPUTE   RECE200-KAISU(16)  =
                                   RECE200-KAISU(16)  -
                                   WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   CONS-SAIKETSU1  OR
                                              CONS-SAIKETSU2  OR
                                              CONS-SAIKETSU3
                         IF   RECE200-KAISU(16)  NOT =  ZERO
                              COMPUTE   RECE200-KAISU(16)  =
                                        RECE200-KAISU(16)  -
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE200-TENSU(17 2)  =
                             RECE200-TENSU(17 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE200-KAISU(18)
                   END-IF
                   COMPUTE   RECE200-TENSU(18 1)  =
                             RECE200-TENSU(18 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE200-TENSU(19 2)  =
                             RECE200-TENSU(19 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                   IF    TNS-NYUTENTTLKBN   =    130
                     COMPUTE   RECE200-TENSU(2 1)  =
                               RECE200-TENSU(2 1)  +
                              (TNS-TEN     *   ACCT-ZAIKAISU)
                   ELSE
                     COMPUTE   RECE200-TENSU(20 1)  =
                               RECE200-TENSU(20 1)  +
                              (WK-SYUTEN   *   ACCT-ZAIKAISU)
                   END-IF
                   COMPUTE     RECE200-TENSU(21 2)  =
                               RECE200-TENSU(21 2)  +
                              (WK-YKZTEN   *   WK-ZAIKAISU)
           END-EVALUATE
           .
      *
       2101221-RECE-LEFT-SET-EXT.
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
              IF  ( IDU  =  1                                  )
              OR  ((IDU  =  2) AND (ACCT-SRYKBN = "60" OR "64"))
              OR  ((IDU  =  3) AND (ACCT-SRYKBN = "80"        ))
              OR  ((IDU  =  4) AND (ACCT-SRYKBN = "80"        ))
      *
                 INITIALIZE                  SRYACT-REC
                 MOVE RECE200-HOSPNUM    TO  SRY-HOSPNUM
                 MOVE RECE200-NYUGAIKBN  TO  SRY-NYUGAIKBN
                 MOVE RECE200-PTID       TO  SRY-PTID
                 MOVE RECE200-SRYYM      TO  SRY-SRYYM
                 MOVE ACCT-ZAINUM        TO  SRY-ZAINUM
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(1)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(2)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(3)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(4)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(5)
                 MOVE SRYACT-REC         TO  MCPDATA-REC
                 MOVE "tbl_sryact"       TO  MCP-TABLE
                 MOVE "key8"             TO  MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF      MCP-RC        =  ZERO
                     MOVE "tbl_sryact"   TO  MCP-TABLE
                     MOVE "key8"         TO  MCP-PATHNAME
                     PERFORM   900-SRYACT-READ-SEC
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
                 MOVE "tbl_sryact"       TO  MCP-TABLE
                 MOVE "key8"             TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
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
      *    ÅÀÅ©¼êµ»²Ã»»¤Þ¤È¤áÂÐ¾Ý¤«¥Á¥§¥Ã¥¯
      *****************************************************************
       21015-TENTEKIMATOME-CHECK-SEC                 SECTION.
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
       21015-TENTEKIMATOME-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2102-RECE220-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE200-HOSPNUM       TO      NACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN     TO      NACCT-NYUGAIKBN
           MOVE    RECE200-PTID          TO      NACCT-PTID
           MOVE    RECE200-SRYYM         TO      NACCT-SRYYM
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
                 IF     NACCT-ZAISKBKBN    =    "1"  
                   IF     NACCT-SRYCDTOTAL   NOT =    099999916         
                       PERFORM      210201-NYUINKHN-GET-SEC
                   END-IF                                               
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                               UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO 
                        IF     NACCT-SRYCDTOTAL    =    099999916       
                           CONTINUE
                        ELSE                                            
                           MOVE   NACCT-DAY(IDY)  TO   WRK-DAY1(IDY) 
                           IF (NACCT-SRYKBN     =  "90"       )
                           OR (NACCT-SRYCDTOTAL =   190799410 )
                           OR (NACCT-SRYCDTOTAL =   190799510 )
                              MOVE  903  TO  WRK-DAY1-NYUTENTTLKBN(IDY) 
                           ELSE                                         
                              MOVE  920  TO  WRK-DAY1-NYUTENTTLKBN(IDY) 
                           END-IF                                       
                           MOVE   WRK-KHNTEN
                                         TO  WRK-DAY1-KHNTEN(IDY) 
                           MOVE   WRK-KSNTEN
                                         TO  WRK-DAY1-KSNTEN(IDY)
                           MOVE   WRK-NYUKHNKSNKBN                      
                                         TO  WRK-DAY1-NYUKHNKSNKBN(IDY)
                           MOVE   WRK-HYOKETU-SRYCD
                                         TO  WRK-DAY1-HYOKETU-SRYCD
                                                               (IDY)
                           MOVE   WRK-KSN-SRYCD
                                         TO  WRK-DAY1-KSN-SRYCD
                                                               (IDY)
                           MOVE   WRK-HYOKETU-TEN
                                         TO  WRK-DAY1-HYOKETU-TEN
                                                               (IDY)
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
                 MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
                 MOVE    "key7"                TO      MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
             IF    WRK-DAY2(IDY)  =  4  OR  5
                CONTINUE
             ELSE
                MOVE   ZERO    TO   WRK-DAY1-KSN-SRYCD(IDY)
                MOVE   ZERO    TO   WRK-DAY1-HYOKETU-SRYCD(IDY)
             END-IF
           END-PERFORM
      *
      *
           MOVE    ZERO    TO    WRK-ZAINUM-CNT2
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE200-HOSPNUM       TO      NACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN     TO      NACCT-NYUGAIKBN
           MOVE    RECE200-PTID          TO      NACCT-PTID
           MOVE    RECE200-SRYYM         TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1") 
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  OR  "2"  OR
                                                "4"  OR  "6"  OR
                                                "7"
      *
                      IF   NACCT-ZAISKBKBN  = "6" OR "7"
                         ADD    1         TO    WRK-ZAINUM-CNT2
                      END-IF
      *
                      INITIALIZE          WRK-DAY-TABLE2
                                          WRK-GAI-TABLE
                                          WRK-GAI-TABLE2
                      MOVE     ZERO       TO    CNT-DAY21
                                                CNT-DAY22
                      MOVE     ZERO       TO    FLG-DAY21
                                                FLG-DAY22
                      MOVE     ZERO       TO    WRK-NYUTENTTLKBN
      *
                      PERFORM  VARYING  IDY    FROM    1   BY  1
                                UNTIL   IDY     >     31
      *
                         IF  RECE200-HKNCOMBI    =  WRK-DAY5(IDY)
                            IF   NACCT-ZAITEN    NOT =  ZERO
                            AND  NACCT-DAY(IDY)  NOT =  ZERO
                            AND  NACCT-ZAISKBKBN NOT =  "4"
                              IF NACCT-ZAISKBKBN     =  "1"
                               IF  WRK-DAY2(IDY) =  ZERO
                                    MOVE   NACCT-DAY(IDY)  
                                           TO   WRK-DAY21(IDY) 
                                    ADD    1   TO   CNT-DAY21
                                    MOVE   1   TO   FLG-DAY21
                               END-IF
                              ELSE
                               IF  ((WRK-DAY1(IDY)   NOT  =  ZERO   ) OR
                                    (RECE200-NYUINDAY-KBN(IDY) = "1"))
                               AND ( WRK-DAY2(IDY) =  ZERO  OR  4
                                                            OR  5    )
                                    MOVE   NACCT-DAY(IDY)  
                                           TO   WRK-DAY21(IDY) 
                                    MOVE   WRK-DAY1-NYUTENTTLKBN(IDY)
                                           TO   WRK-NYUTENTTLKBN
                                    ADD    1   TO   CNT-DAY21
                                    MOVE   1   TO   FLG-DAY21
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
                               PERFORM      210203-NYUINACCT-GET-SEC
                            END-IF
      *                     *¿©»ö
                            IF  ((WRK-DAY1(IDY)   NOT  =  ZERO   ) OR
                                 (RECE200-NYUINDAY-KBN(IDY) = "1"))
                            AND ( NACCT-ZAITEN         =  ZERO    )
                            AND ( NACCT-ZAISKBKBN      =  "4"     )
                            AND ( NACCT-DAY(IDY)  NOT  =  ZERO    )
                            AND ( WRK-SHOKUJIRYOYO(IDY)
                                                  NOT  =  ZERO    )
                            AND ( WRK-DAY2(IDY)    = ZERO  OR  4
                                                           OR  5  )
                            AND ( RECE200-SRYYM   >= "200604"     )
                            AND ( NACCT-SRYCDTOTAL = 099999918
                                                 OR  099999919
                                                 OR  099999920    )
                               MOVE   NACCT-DAY(IDY)
                                            TO   WRK-DAY24(IDY)
                               PERFORM      210204-NYUINACCT-SYU2-SEC
                            END-IF
                            IF    ( RECE200-SRYYM       >= "200610" )
                            AND   ((WRK-DAY1(IDY)   NOT  =  ZERO   ) OR
                                   (RECE200-NYUINDAY-KBN(IDY) = "1"))
                            AND   ( NACCT-ZAITEN         =  ZERO    )
                            AND   ( NACCT-ZAISKBKBN      =  "4"     )
      ***                   AND   ( NACCT-DAY(IDY)  NOT  =  ZERO    )
                            AND   ( WRK-SHOKUJIRYOYO(IDY)
                                                    NOT  =  ZERO    )
                            AND   ( NACCT-SRYCDTOTAL = 099999918
                                                   OR  099999919
                                                   OR  099999920    )
                            AND   ( WRK-DAY2(IDY) =  ZERO  OR  4
                                                           OR  5    )
                               PERFORM   210218-NYUINACCT-SYU2-KAN-SEC
                            END-IF
                         END-IF
      *
                      END-PERFORM
      *
                      IF   FLG-DAY21    =   1
                         PERFORM   210205-RECE220-TENSOU2-SEC
                         PERFORM   210206-RECE230-HENSYU2-SEC
                         PERFORM   RECE220-WRITE-SEC
                      END-IF
      *
                      IF   FLG-DAY22    =   1
                         PERFORM   210209-NYUINACCT-HEN-SEC
                         PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                            UNTIL  SYUKEI-IDX3           > 20
                             OR    WRK-GAI2-DAY(SYUKEI-IDX3) = ZERO
                           PERFORM   210210-RECE220-TENSOU2-GAI-SEC
                           PERFORM   210211-RECE230-HENSYU2-GAI-SEC
                           PERFORM   RECE220-WRITE-SEC
                         END-PERFORM
                      END-IF
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
                 MOVE    "key7"                TO      MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ
           IF    RECE200-SRYYM   >=  "200610"
             INITIALIZE                    ORCSNYULIFE-AREA
             MOVE    RECE200-HOSPNUM   TO  LNK-NYULIFE-HOSPNUM
             MOVE    RECE200-PTID      TO  LNK-NYULIFE-PTID
             MOVE    RECE200-SRYYM     TO  LNK-NYULIFE-SANTEIYM
             CALL    "ORCSNYULIFE"         USING
                                           ORCSNYULIFE-AREA
                                           SPA-AREA
             IF      LNK-NYULIFE-RC   =   ZERO
               PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                         UNTIL   SYUKEI-IDX3   >   31
                 MOVE  LNK-NYULIFE-KBN(SYUKEI-IDX3)
                             TO  WRK-NYULIFE-KBN(SYUKEI-IDX3)
                 MOVE  LNK-NYULIFE-SKJFTN-SRYCD(SYUKEI-IDX3)
                             TO  WRK-NYULIFE-SKJFTN-SRYCD(SYUKEI-IDX3)
                 MOVE  LNK-NYULIFE-SKJFTN-MONEY(SYUKEI-IDX3)
                             TO  WRK-NYULIFE-SKJFTN-MONEY(SYUKEI-IDX3)
                 MOVE  LNK-NYULIFE-KANFTN-SRYCD(SYUKEI-IDX3)
                             TO  WRK-NYULIFE-KANFTN-SRYCD(SYUKEI-IDX3)
                 MOVE  LNK-NYULIFE-KANFTN-MONEY(SYUKEI-IDX3)
                             TO  WRK-NYULIFE-KANFTN-MONEY(SYUKEI-IDX3)
               END-PERFORM
             END-IF
           END-IF
      *
      *    £²£°£°£¶Ç¯£´·î°Ê¹ß¤Î¿©»öÎÅÍÜ
           IF   RECE200-SRYYM   >=  "200604"
      *
              PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                        UNTIL   SYUKEI-IDX3    >  31
      *
                 IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                                                    NOT =  ZERO
                    PERFORM 210213-RECE220-TENSOU2-SKJ200604-SEC
                    PERFORM 210214-RECE230-HENSYU2-SKJ200604-SEC
                    PERFORM RECE230-WRITE-SEC
                    PERFORM RECE220-WRITE-SEC
                 END-IF
      *
                 IF   WRK-SKJ200604-DAY-KAN(SYUKEI-IDX3)
                                                    NOT =  ZERO
                 AND  WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                    PERFORM 210219-RECE220-TENSOU2-SKJ200610-KAN-SEC
                    PERFORM 210220-RECE230-HENSYU2-SKJ200610-KAN-SEC
                    PERFORM RECE230-WRITE-SEC
                    PERFORM RECE220-WRITE-SEC
                 END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       2102-RECE220-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡´ðËÜÎÁ¤Î¼èÆÀ
      **************************************************************
       210201-NYUINKHN-GET-SEC           SECTION.
      *
           MOVE    ZERO               TO   WRK-KHNTEN
                                           WRK-NYUKHNKSNKBN
                                           WRK-KSNTEN
                                           WRK-KSN-SRYCD
      *
           MOVE    SPACE              TO   WRK-GAIHAKU-SRYCD
           MOVE    ZERO               TO   WRK-GAIHAKU-KHNTEN
      *
           MOVE    ZERO               TO   WRK-HYOKETU-TEN
                                           WRK-HYOKETU-SRYCD
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
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =    ZERO
               MOVE    "tbl_nyuinact"      TO      MCP-TABLE
               MOVE    "key9"              TO      MCP-PATHNAME
               PERFORM 900-NYUINACT-READ-SEC
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
                     MOVE    1                   TO  WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF   FLG-TENSU      =    ZERO
                       IF   TNS-DATAKBN    =    1
                          MOVE   TNS-TEN   TO   WRK-KHNTEN
                          MOVE   TNS-NYUKHNKSNKBN
                                           TO   WRK-NYUKHNKSNKBN
      *                   ´Ç¸îÈæÎ¨´ð½àÌ¤Ã£»þ¤ÎÆþ±¡´ðËÜÅÀ¿ô¤Î¼èÆÀ
                          PERFORM  2102011-GAIHAKU-NYUINKHN-SET-SEC
                          IF   WRK-GAIHAKU-KHNTEN  NOT =  ZERO
                                 MOVE   WRK-GAIHAKU-KHNTEN
                                               TO   WRK-KHNTEN
                          END-IF
      ***                 MOVE   1         TO   FLG-NYUKHN
                       ELSE
                          EVALUATE  TNS-TENSIKIBETU
                            WHEN     3
                                MOVE  TNS-TEN  TO  WRK-KSNTEN
                                MOVE  NSRY-SRYCD(IDX2)
                                               TO  WRK-KSN-SRYCD
                            WHEN     8
                                COMPUTE  WRK-KSNTEN = TNS-TEN * -1
                                MOVE  NSRY-SRYCD(IDX2)
                                               TO  WRK-KSN-SRYCD
                            WHEN     6
                                MOVE  TNS-TEN  TO  WRK-HYOKETU-TEN
                                MOVE  NSRY-SRYCD(IDX2)
                                               TO  WRK-HYOKETU-SRYCD
                          END-EVALUATE
                       END-IF
                     END-IF
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO      MCP-TABLE
           MOVE    "key9"              TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
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
       210201-NYUINKHN-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ
      **************************************************************
       2102011-GAIHAKU-NYUINKHN-SET-SEC           SECTION.
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
           IF   (RECE200-SRYYM      >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
               MOVE    WRK-GAIHAKU-SRYCD   TO  WRK-SRYCD
               MOVE    NACCT-SRYYM         TO  WK-TAIHI-SRYYM
               MOVE    1                   TO  WK-TAIHI-SRYD
               PERFORM    900-TENSU-TBL-SEC
               IF   FLG-TENSU      =    ZERO
                     MOVE     TNS-TEN      TO  WRK-GAIHAKU-KHNTEN
               END-IF
           END-IF
      *
           .
       2102011-GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ¤ÎÆþ±¡´ðËÜÎÁ·×»»
      **************************************************************
       210203-NYUINACCT-GET-SEC           SECTION.
      *
      *    Æþ±¡ÎÁ¤È³°Çñ¤òÈæ³Ó¤·¤ÆÅÀ¿ô»»Äê
      *
           EVALUATE   WRK-DAY22-NYUTENTTLKBN(IDY)
               WHEN   903
                    EVALUATE   WRK-DAY22(IDY)
                     WHEN   1
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.15)
                                                        +  0.5
                         MOVE    1     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   2
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.3)
                                                        +  0.5
                         MOVE    2     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   4
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.3)
                                                        +  0.5
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                  WRK-DAY22-NYUKHNRYO(IDY) +
                                  WRK-DAY1-KSNTEN(IDY)
                         IF  WRK-DAY1-HYOKETU-TEN(IDY)  NOT =  ZERO
                           COMPUTE WRK-DAY22-NYUKHNRYO(IDY)    =
                                  (WRK-DAY22-NYUKHNRYO(IDY)    -
                                   (WRK-DAY22-NYUKHNRYO(IDY)   *
                                    (WRK-DAY1-HYOKETU-TEN(IDY) / 100)))
                                                               + 0.5
                         END-IF
                         MOVE    4     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   5
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.7)
                                                        +  0.5
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                  WRK-DAY22-NYUKHNRYO(IDY) +
                                  WRK-DAY1-KSNTEN(IDY)
                         IF  WRK-DAY1-HYOKETU-TEN(IDY)  NOT =  ZERO
                           COMPUTE WRK-DAY22-NYUKHNRYO(IDY)    =
                                  (WRK-DAY22-NYUKHNRYO(IDY)    -
                                   (WRK-DAY22-NYUKHNRYO(IDY)   *
                                    (WRK-DAY1-HYOKETU-TEN(IDY) / 100)))
                                                               + 0.5
                         END-IF
                         MOVE    11    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                    END-EVALUATE
               WHEN   920
                    EVALUATE   WRK-DAY22(IDY)
                     WHEN   1
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.15)
                                                        +  0.5
                         MOVE    6     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   2
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.3)
                                                        +  0.5
                         MOVE    7     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   4
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.3)
                                                        +  0.5
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                  WRK-DAY22-NYUKHNRYO(IDY) +
                                  WRK-DAY1-KSNTEN(IDY)
                         IF  WRK-DAY1-HYOKETU-TEN(IDY)  NOT =  ZERO
                           COMPUTE WRK-DAY22-NYUKHNRYO(IDY)    =
                                  (WRK-DAY22-NYUKHNRYO(IDY)    -
                                   (WRK-DAY22-NYUKHNRYO(IDY)   *
                                    (WRK-DAY1-HYOKETU-TEN(IDY) / 100)))
                                                               + 0.5
                         END-IF
                         MOVE    9     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   5
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.7)
                                                        +  0.5
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                  WRK-DAY22-NYUKHNRYO(IDY) +
                                  WRK-DAY1-KSNTEN(IDY)
                         IF  WRK-DAY1-HYOKETU-TEN(IDY)  NOT =  ZERO
                           COMPUTE WRK-DAY22-NYUKHNRYO(IDY)    =
                                  (WRK-DAY22-NYUKHNRYO(IDY)    -
                                   (WRK-DAY22-NYUKHNRYO(IDY)   *
                                    (WRK-DAY1-HYOKETU-TEN(IDY) / 100)))
                                                               + 0.5
                         END-IF
                         MOVE    13    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                    END-EVALUATE
           END-EVALUATE
      *
           .
       210203-NYUINACCT-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ¤ÎÆþ±¡´ðËÜÎÁ½¸·×
      **************************************************************
       2102031-NYUINACCT-SYU-SEC           SECTION.
      *
           IF  ( WRK-DAY22-NYUKHNRYO(IDY)  NOT =  ZERO     )
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
             ADD    1     TO   CNT-DAY22
             MOVE   1     TO   FLG-DAY22
      *
           END-IF
      *
           .
      *
       2102031-NYUINACCT-SYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î½¸·×
      **************************************************************
       210204-NYUINACCT-SYU2-SEC           SECTION.
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
           .
       210204-NYUINACCT-SYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î½¸·×¡Ê´Ä¶­¡Ë
      **************************************************************
       210218-NYUINACCT-SYU2-KAN-SEC           SECTION.
      *
           EVALUATE   WRK-SHOKUJIRYOYO(IDY)
             WHEN  1
               MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
               MOVE    1       TO  WRK-SKJ200604-DAY-KAN(IDY)
             WHEN  2
               MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
               MOVE    1       TO  WRK-SKJ200604-DAY-KAN(IDY)
           END-EVALUATE
      *
           .
       210218-NYUINACCT-SYU2-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       210205-RECE220-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE220-REC
           INITIALIZE                           RECE220-REC
           MOVE    RECE200-KEY          TO      RECE220-KEY1
           MOVE    NACCT-SRYKBN         TO      RECE220-SRYKBN
           IF      NACCT-ZAISKBKBN      =       "1"
             IF  (NACCT-SRYCDTOTAL =   190799410 )
             OR  (NACCT-SRYCDTOTAL =   190799510 )
                MOVE    "90"            TO      RECE220-SRYKBN
             END-IF
           END-IF
           IF      NACCT-ZAISKBKBN      =       "6"
             EVALUATE  WRK-NYUTENTTLKBN
               WHEN      903
                MOVE    "90"            TO      RECE220-SRYKBN
               WHEN      920
                MOVE    "92"            TO      RECE220-SRYKBN
             END-EVALUATE
           END-IF
           MOVE    NACCT-ZAISKBKBN      TO      RECE220-ZAISKBKBN
           IF      NACCT-ZAISKBKBN      =       "7"
                MOVE     "80"           TO      RECE220-SRYKBN
                MOVE    SPACE           TO      RECE220-ZAISKBKBN
           END-IF
      *
           MOVE    ZERO                 TO      WRK-ZAINUM-CNT
           EVALUATE  NACCT-ZAISKBKBN
            WHEN    "1"
             PERFORM     VARYING   IDX2   FROM    1    BY   1
                          UNTIL    IDX2    >     31
                            OR     WRK-ZAINUM-CNT  NOT =  ZERO
                 IF  WRK-DAY21(IDX2)   NOT =   ZERO
                     MOVE    IDX2       TO      WRK-ZAINUM-CNT
                 END-IF
             END-PERFORM
             COMPUTE WRK-ZAINUM  = 99999800  +  WRK-ZAINUM-CNT
             MOVE    WRK-ZAINUM         TO      RECE220-ZAINUM
            WHEN    "6"
            WHEN    "7"
             COMPUTE WRK-ZAINUM  = 99993000  +  WRK-ZAINUM-CNT2
             MOVE    WRK-ZAINUM         TO      RECE220-ZAINUM
            WHEN    OTHER
             MOVE    NACCT-ZAINUM       TO      RECE220-ZAINUM
           END-EVALUATE
      *
           MOVE    RECE200-PTNUM        TO      RECE220-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE220-SRYKA
           MOVE    RECE200-HKNCOMBI     TO      RECE220-HKNCOMBI
           MOVE    SPACE                TO      RECE220-JIHOKBN
           MOVE    NACCT-ZAITEN         TO      RECE220-ZAITEN
           MOVE    NACCT-MEISAISU       TO      RECE220-MEISAISU
           MOVE    CNT-DAY21            TO      RECE220-ZAIKAISU
           MOVE    NACCT-SYUTEN1        TO      RECE220-SYUTEN1
           MOVE    NACCT-SRYCDTOTAL     TO      RECE220-SRYCDTOTAL
           IF      NACCT-ZAISKBKBN      =       "7"
                MOVE    199000510       TO      RECE220-SRYCDTOTAL
           END-IF
           MOVE    NACCT-SURYOUTOTAL    TO      RECE220-SURYOUTOTAL
           MOVE    NACCT-SYUTEN2        TO      RECE220-SYUTEN2
           MOVE    NACCT-YKZTEN         TO      RECE220-YKZTEN
           MOVE    NACCT-KIZAITEN       TO      RECE220-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE220-DRCD
           MOVE    SPACE                TO      RECE220-ZAIREQFLG
           MOVE    ZERO                 TO      RECE220-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                        UNTIL    IDX2   >       31
                   MOVE    WRK-DAY21(IDX2)  TO
                                            RECE220-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)   AND
                          (WRK-DAY21(IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF
           END-PERFORM
           .
       210205-RECE220-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       210206-RECE230-HENSYU2-SEC               SECTION.
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
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
      *
               MOVE    "tbl_nyuinact"   TO   MCP-TABLE
               MOVE    "key9"           TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =    "1")
                   PERFORM   2102061-RECE230-TENSOU2-SEC
                   IF      NACCT-ZAISKBKBN      =       "7"
                      PERFORM   2102062-RECE200-HENSYU2-SEC
                   END-IF
                   PERFORM   RECE230-WRITE-SEC
                   MOVE    "tbl_nyuinact"     TO   MCP-TABLE
                   MOVE    "key9"             TO   MCP-PATHNAME
                   PERFORM        900-NYUINACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210206-RECE230-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2102061-RECE230-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE230-REC
           INITIALIZE                           RECE230-REC
           MOVE    RECE200-KEY          TO      RECE230-KEY1
      *
           EVALUATE  NACCT-ZAISKBKBN
            WHEN    "1"
             COMPUTE WRK-ZAINUM   =  99999800  +  WRK-ZAINUM-CNT
             MOVE    WRK-ZAINUM         TO      RECE230-ZAINUM
            WHEN    "6"
            WHEN    "7"
             COMPUTE WRK-ZAINUM   =  99993000  +  WRK-ZAINUM-CNT2
             MOVE    WRK-ZAINUM         TO      RECE230-ZAINUM
            WHEN    OTHER
             MOVE    NSRY-ZAINUM        TO      RECE230-ZAINUM
           END-EVALUATE
      *
           MOVE    NSRY-RENNUM          TO      RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO      RECE230-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE230-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE230-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE230-SRYSYUKBN
           IF      NACCT-ZAISKBKBN      =       "1"
             IF  (NACCT-SRYCDTOTAL =   190799410 )
             OR  (NACCT-SRYCDTOTAL =   190799510 )
                MOVE    "90"            TO      RECE230-SRYKBN
                MOVE    "900"           TO      RECE230-SRYSYUKBN
             END-IF
           END-IF
           IF      NACCT-ZAISKBKBN      =       "6"
             EVALUATE  WRK-NYUTENTTLKBN
               WHEN      903
                MOVE    "90"            TO      RECE230-SRYKBN
                MOVE    "900"           TO      RECE230-SRYSYUKBN
               WHEN      920
                MOVE    "92"            TO      RECE230-SRYKBN
                MOVE    "920"           TO      RECE230-SRYSYUKBN
             END-EVALUATE
           END-IF
           IF      NACCT-ZAISKBKBN      =       "7"
                MOVE     "80"           TO      RECE230-SRYKBN
                MOVE     "800"          TO      RECE230-SRYSYUKBN
           END-IF
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE230-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (NSRY-SRYCD(IDX2)  =   SPACE)
             IF      NSRY-SRYCD(IDX2)(1:1)    =   "0"  OR  "1" OR
                                                  "8"
               ADD     1                   TO  IDX4
               MOVE    NSRY-SRYCD(IDX2)    TO  RECE230-SRYCD(IDX4)
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     "199000510"     TO  RECE230-SRYCD(IDX4)
               END-IF
               MOVE    NSRY-SRYSURYO(IDX2) TO  RECE230-SRYSURYO(IDX4)
               MOVE    NSRY-SRYKAISU(IDX2) TO  RECE230-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE230-MEISKYFLG(IDX4)
               MOVE    NSRY-AUTOKBN(IDX2)  TO  RECE230-AUTOKBN(IDX4)
               MOVE    NSRY-INPUTNUM(IDX2) TO  RECE230-INPUTNUM(IDX4)
               MOVE    NSRY-SRYCD(IDX2)    TO  WRK-SRYCD
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     199000510       TO  WRK-SRYCD
               END-IF
               MOVE    RECE230-SRYYM       TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM    900-TENSU-TBL-SEC
               MOVE    TNS-TANICD          TO  RECE230-TANICD(IDX4)
               MOVE    TNS-TANIMOJISU      TO  RECE230-TANIMOJISU(IDX4)
               MOVE    TNS-TANINAME        TO  RECE230-TANINAME(IDX4)
               MOVE    TNS-TENSIKIBETU     TO  RECE230-TENSIKIBETU(IDX4)
               MOVE    TNS-TEN             TO  RECE230-TEN(IDX4)
               MOVE    TNS-NYUTENTTLKBN    TO  RECE230-TENTTLKBN(IDX4)
               MOVE    TNS-YUKOKETA        TO  RECE230-YUKOKETA(IDX4)
               MOVE    TNS-DATAKBN         TO  RECE230-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN       TO  RECE230-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN       TO  RECE230-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN       TO  RECE230-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO       TO  RECE230-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU        TO  RECE230-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM
                                      TO RECE230-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE230-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE230-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM
                                      TO RECE230-KOUHYOJYUNNUM(IDX4)
      *
               MOVE    TNS-SRYSYUKBN  TO RECE230-SRYSYUKBN-X(IDX4)
               MOVE    TNS-KOKUJISKBKBN1
                                      TO RECE230-KOKUJISKBKBN1(IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE230-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE230-CHUKSNCD(IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE230-CHUKSNTUBAN(IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO RECE230-KENTAICOMMENT(IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE230-KNSJISGRPKBN(IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO RECE230-TOKUKIZAISBT1(IDX4)
               MOVE    TNS-SANSOKBN   TO RECE230-SANSOKBN     (IDX4)
      *
               IF    ((NSRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (NSRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( NSRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    NSRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    NSRY-PTID             TO  PTCOM-PTID
                   MOVE    NSRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    NSRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    NSRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT     TO  RECE230-NAME(IDX4)
               ELSE
                   MOVE    TNS-NAME              TO  RECE230-NAME(IDX4)
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
                   COMPUTE  RECE220-RUIKEITOTAL  =
                            RECE220-RUIKEITOTAL  +
                          ( WRK-FUKA-VALUE    * WRK-SRYCD)
                   COMPUTE  WRK-FUKA-KINGAKU    =
                            WRK-FUKA-KINGAKU    +
                          ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                              * NSRY-SRYSURYO(IDX2))
                 END-IF
               END-PERFORM
               COMPUTE  RECE220-RUIKEITOTAL  =
                        RECE220-RUIKEITOTAL  +
                        WRK-FUKA-KINGAKU
      *
             END-IF
           END-PERFORM
      *
           .
       2102061-RECE230-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡ÊÎÅÍÜÃ´Åö¼êÅö¡Ë
      *****************************************************************
       2102062-RECE200-HENSYU2-SEC                SECTION.
      *
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
           COMPUTE   RECE200-TENSU(20 1)  =
                     RECE200-TENSU(20 1)  +
                    (NACCT-ZAITEN   *   CNT-DAY21)
      *
           .
       2102062-RECE200-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ¤ÎÆþ±¡´ðËÜÎÁÊÔ½¸
      **************************************************************
       210209-NYUINACCT-HEN-SEC           SECTION.
      *
           MOVE     ZERO     TO     SYUKEI-IDX3
      *
           PERFORM    VARYING  SYUKEI-IDX     FROM    1   BY  1
      ***         UNTIL  SYUKEI-IDX            > 10
                  UNTIL  SYUKEI-IDX            > 14
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
                  WHEN     4
                     PERFORM  2102091-NYUINACCT-HEN-TAKIKAN-SEC
                  WHEN     6
                     MOVE   "190107490" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     7
                     MOVE   "190107590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     9
                     PERFORM  2102091-NYUINACCT-HEN-TAKIKAN-SEC
                  WHEN    11
                     PERFORM  2102091-NYUINACCT-HEN-TAKIKAN-SEC
                  WHEN    13
                     PERFORM  2102091-NYUINACCT-HEN-TAKIKAN-SEC
               END-EVALUATE
               MOVE   WRK-GAI-ZAITEN(SYUKEI-IDX SYUKEI-IDX2)
                                    TO   WRK-GAI2-ZAITEN  (SYUKEI-IDX3)
               MOVE   WRK-GAI-ZAIKAISU(SYUKEI-IDX SYUKEI-IDX2)
                                    TO   WRK-GAI2-ZAIKAISU(SYUKEI-IDX3)
               EVALUATE  SYUKEI-IDX
                  WHEN     1
                  WHEN     2
                  WHEN     4
                  WHEN    11
                     MOVE    1      TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     MOVE   WRK-GAI2-DAY(SYUKEI-IDX3)  TO   SYUKEI-IDX4
                     IF WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)      NOT = ZERO
                        MOVE    3   TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     END-IF
                     IF WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)  NOT = ZERO
                        MOVE    3   TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     END-IF
                  WHEN     6
                  WHEN     7
                  WHEN     9
                  WHEN    13
                     MOVE    2      TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     MOVE   WRK-GAI2-DAY(SYUKEI-IDX3)  TO   SYUKEI-IDX4
                     IF WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)      NOT = ZERO
                        MOVE    4   TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     END-IF
                     IF WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)  NOT = ZERO
                        MOVE    4   TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     END-IF
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
       210209-NYUINACCT-HEN-EXT.
           EXIT.
      *
      **************************************************************
      *    Â¾°åÎÅµ¡´Ø¼õ¿Ç»þ¤Î¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ
      **************************************************************
       2102091-NYUINACCT-HEN-TAKIKAN-SEC           SECTION.
      *
           MOVE   WRK-GAI2-DAY(SYUKEI-IDX3)   TO   SYUKEI-IDX4
      *
           EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
               WHEN   099  THRU  100
                 IF   RECE200-SRYYM  >=  "201004"
                   EVALUATE   SYUKEI-IDX
                    WHEN     4
                    WHEN     9
                     MOVE "190108890" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    11
                    WHEN    13
                     MOVE "190142190" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 END-IF
               WHEN   201  THRU  219
               WHEN   221  THRU  222
               WHEN   224
               WHEN   227  THRU  229
               WHEN   230  THRU  239
               WHEN   241  THRU  249
                 EVALUATE   SYUKEI-IDX
                   WHEN     4
                   WHEN     9
                     MOVE "190108790" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   WHEN    11
                   WHEN    13
                     MOVE "190142090" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
               WHEN   013  THRU  021
               WHEN   101  THRU  109
               WHEN   147  THRU  152
               WHEN   173  THRU  190
                 EVALUATE   SYUKEI-IDX
                   WHEN     4
                   WHEN     9
                     MOVE "190108890" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   WHEN    11
                   WHEN    13
                     MOVE "190142190" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
               WHEN   075  THRU  080
                 EVALUATE   SYUKEI-IDX
                   WHEN     4
                   WHEN     9
                     MOVE "190108990" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
               WHEN   085  THRU  086
               WHEN   132  THRU  133
               WHEN   153  THRU  158
                 EVALUATE   SYUKEI-IDX
                   WHEN     4
                   WHEN     9
                     MOVE "190109090" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   WHEN    11
                   WHEN    13
                     MOVE "190142290" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
               WHEN   226
               WHEN   240
                 CONTINUE
               WHEN   OTHER
                 EVALUATE   SYUKEI-IDX
                   WHEN    11
                   WHEN    13
                     MOVE "190141590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
           END-EVALUATE
           .
      *
       2102091-NYUINACCT-HEN-TAKIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       210210-RECE220-TENSOU2-GAI-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE220-REC
           INITIALIZE                           RECE220-REC
           MOVE    RECE200-KEY          TO      RECE220-KEY1
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1 OR 3
              MOVE    "90"              TO      RECE220-SRYKBN
           ELSE
              MOVE    "92"              TO      RECE220-SRYKBN
           END-IF
           MOVE    NACCT-ZAISKBKBN      TO      RECE220-ZAISKBKBN
           COMPUTE WRK-ZAINUM    =  99999900  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO      RECE220-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE220-SRYKA
           MOVE    RECE200-HKNCOMBI     TO      RECE220-HKNCOMBI
           MOVE    SPACE                TO      RECE220-JIHOKBN
           MOVE    WRK-GAI2-ZAITEN(SYUKEI-IDX3) 
                                        TO      RECE220-ZAITEN
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1 OR 2
              MOVE    1                 TO      RECE220-MEISAISU
              MOVE    WRK-GAI2-SRYCD-9 (SYUKEI-IDX3)
                                        TO      RECE220-SRYCDTOTAL
              MOVE    1                 TO      RECE220-SURYOUTOTAL
           ELSE
              MOVE    1                 TO      RECE220-MEISAISU
              MOVE    WRK-GAI2-SRYCD-9 (SYUKEI-IDX3)
                                        TO      RECE220-SRYCDTOTAL
              MOVE    1                 TO      RECE220-SURYOUTOTAL
              MOVE    WRK-GAI2-DAY(SYUKEI-IDX3)  TO   SYUKEI-IDX4
              IF WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)      NOT = ZERO
                ADD   1   TO   RECE220-MEISAISU
                ADD   WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)
                          TO   RECE220-SRYCDTOTAL
                ADD   1   TO   RECE220-SURYOUTOTAL
              END-IF
              IF WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)  NOT = ZERO
                ADD   1   TO   RECE220-MEISAISU
                ADD   WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)
                          TO   RECE220-SRYCDTOTAL
                ADD   1   TO   RECE220-SURYOUTOTAL
              END-IF
           END-IF
           MOVE    ZERO                 TO      RECE220-RUIKEITOTAL
           MOVE    WRK-GAI2-ZAIKAISU(SYUKEI-IDX3)
                                        TO      RECE220-ZAIKAISU
           MOVE    WRK-GAI2-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE220-SYUTEN1
           MOVE    ZERO                 TO      RECE220-SYUTEN2
           MOVE    ZERO                 TO      RECE220-YKZTEN
           MOVE    ZERO                 TO      RECE220-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE220-DRCD
           MOVE    SPACE                TO      RECE220-ZAIREQFLG
           MOVE    ZERO                 TO      RECE220-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                        UNTIL    IDX2   >       31
                   MOVE    WRK-GAI2-KAISU(SYUKEI-IDX3 IDX2)
                                        TO    RECE220-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)
                   AND    (WRK-GAI2-KAISU(SYUKEI-IDX3 IDX2)
                                                      NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF
           END-PERFORM
           .
       210210-RECE220-TENSOU2-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       210211-RECE230-HENSYU2-GAI-SEC               SECTION.
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
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
           ELSE
               MOVE     1               TO      FLG-NYUINACT
           END-IF
           IF  FLG-NYUINACT   =   ZERO
               PERFORM   2102111-RECE230-TENSOU2-GAI-SEC
               PERFORM   RECE230-WRITE-SEC
           END-IF
      *
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210211-RECE230-HENSYU2-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2102111-RECE230-TENSOU2-GAI-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE230-REC
           INITIALIZE                           RECE230-REC
           MOVE    RECE200-KEY          TO      RECE230-KEY1
           COMPUTE WRK-ZAINUM    =  99999900  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE230-ZAINUM
           MOVE    NSRY-RENNUM          TO      RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO      RECE230-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE230-SRYKA
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1 OR 3
              MOVE    "90"              TO      RECE230-SRYKBN
           ELSE
              MOVE    "92"              TO      RECE230-SRYKBN
           END-IF 
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1 OR 3
              MOVE    "900"             TO      RECE230-SRYSYUKBN
           ELSE
              MOVE    "920"             TO      RECE230-SRYSYUKBN
           END-IF 
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE230-JIHIMONEY
      *
           MOVE    WRK-GAI2-SRYCD(SYUKEI-IDX3)
                                        TO  RECE230-SRYCD(1)
           MOVE    1                    TO  RECE230-SRYSURYO(1)
           MOVE    ZERO                 TO  RECE230-SRYKAISU(1)
           MOVE    SPACE                TO  RECE230-MEISKYFLG(1)
           MOVE    SPACE                TO  RECE230-AUTOKBN(1)
           MOVE    ZERO                 TO  RECE230-INPUTNUM(1)
           MOVE    WRK-GAI2-SRYCD(SYUKEI-IDX3)
                                        TO  WRK-SRYCD
           MOVE    RECE230-SRYYM        TO  WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
           PERFORM    900-TENSU-TBL-SEC
           MOVE    TNS-TANICD          TO  RECE230-TANICD(1)
           MOVE    TNS-TANIMOJISU      TO  RECE230-TANIMOJISU(1)
           MOVE    TNS-TANINAME        TO  RECE230-TANINAME(1)
           MOVE    TNS-TENSIKIBETU     TO  RECE230-TENSIKIBETU(1)
           MOVE    TNS-TEN             TO  RECE230-TEN(1)
           MOVE    TNS-NYUTENTTLKBN    TO  RECE230-TENTTLKBN(1)
           MOVE    TNS-YUKOKETA        TO  RECE230-YUKOKETA(1)
           MOVE    TNS-DATAKBN         TO  RECE230-DATAKBN (1)
           MOVE    TNS-HOUKNSKBN       TO  RECE230-HOUKNSKBN(1)
           MOVE    TNS-KNSJISKBN       TO  RECE230-KNSJISKBN(1)
           MOVE    TNS-CDKBN-KBN       TO  RECE230-CDKBN-KBN(1)
           MOVE    TNS-CDKBN-SYO       TO  RECE230-CDKBN-SYO(1)
           MOVE    TNS-CDKBN-BU        TO  RECE230-CDKBN-BU(1)
           MOVE    TNS-CDKBN-KBNNUM
                                  TO RECE230-CDKBN-KBNNUM(1)
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                  TO RECE230-CDKBN-KBNNUM-EDA(1)
           MOVE    TNS-CDKBN-KOUBAN
                                  TO RECE230-CDKBN-KOUBAN(1)
           MOVE    TNS-KOUHYOJYUNNUM
                                  TO RECE230-KOUHYOJYUNNUM(1)
      *
           MOVE    TNS-SRYSYUKBN  TO RECE230-SRYSYUKBN-X(1)
           MOVE    TNS-KOKUJISKBKBN1
                                  TO RECE230-KOKUJISKBKBN1(1)
           MOVE    TNS-KZMCOMPSIKIBETU
                                  TO RECE230-KZMCOMPSIKIBETU(1)
           MOVE    TNS-CHUKSNCD   TO RECE230-CHUKSNCD(1)
           MOVE    TNS-CHUKSNTUBAN
                                  TO RECE230-CHUKSNTUBAN(1)
           MOVE    TNS-KENTAICOMMENT
                                  TO RECE230-KENTAICOMMENT(1)
           MOVE    TNS-KNSJISGRPKBN
                                  TO RECE230-KNSJISGRPKBN(1)
           MOVE    TNS-TOKUKIZAISBT1
                                  TO RECE230-TOKUKIZAISBT1(1)
           MOVE    TNS-SANSOKBN   TO RECE230-SANSOKBN(1)
      *
           MOVE    TNS-NAME       TO RECE230-NAME(1)
      *
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   3 OR 4
      *
            MOVE    1     TO     SYUKEI-IDX5
            MOVE   WRK-GAI2-DAY(SYUKEI-IDX3)  TO  SYUKEI-IDX4
            IF WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)      NOT = ZERO
              ADD   1     TO     SYUKEI-IDX5
              MOVE  WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)
                                     TO RECE230-SRYCD(SYUKEI-IDX5)
              MOVE  1                TO RECE230-SRYSURYO(SYUKEI-IDX5)
              MOVE  ZERO             TO RECE230-SRYKAISU(SYUKEI-IDX5)
              MOVE  SPACE            TO RECE230-MEISKYFLG(SYUKEI-IDX5)
              MOVE  SPACE            TO RECE230-AUTOKBN(SYUKEI-IDX5)
              MOVE  ZERO             TO RECE230-INPUTNUM(SYUKEI-IDX5)
              MOVE  WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)
                                     TO WRK-SRYCD
              MOVE  RECE230-SRYYM    TO WK-TAIHI-SRYYM
              MOVE  WK-TAIHI-DAY     TO WK-TAIHI-SRYD
              PERFORM    900-TENSU-TBL-SEC
              MOVE  TNS-TANICD       TO RECE230-TANICD(SYUKEI-IDX5)
              MOVE  TNS-TANIMOJISU   TO RECE230-TANIMOJISU(SYUKEI-IDX5)
              MOVE  TNS-TANINAME     TO RECE230-TANINAME(SYUKEI-IDX5)
              MOVE  TNS-TENSIKIBETU  TO RECE230-TENSIKIBETU(SYUKEI-IDX5)
              MOVE  TNS-TEN          TO RECE230-TEN(SYUKEI-IDX5)
              MOVE  TNS-NYUTENTTLKBN TO RECE230-TENTTLKBN(SYUKEI-IDX5)
              MOVE  TNS-YUKOKETA     TO RECE230-YUKOKETA(SYUKEI-IDX5)
              MOVE  TNS-DATAKBN      TO RECE230-DATAKBN (SYUKEI-IDX5)
              MOVE  TNS-HOUKNSKBN    TO RECE230-HOUKNSKBN(SYUKEI-IDX5)
              MOVE  TNS-KNSJISKBN    TO RECE230-KNSJISKBN(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-KBN    TO RECE230-CDKBN-KBN(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-SYO    TO RECE230-CDKBN-SYO(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-BU     TO RECE230-CDKBN-BU(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-KBNNUM
                                TO RECE230-CDKBN-KBNNUM(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-KBNNUM-EDA
                                TO RECE230-CDKBN-KBNNUM-EDA(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-KOUBAN
                                TO RECE230-CDKBN-KOUBAN(SYUKEI-IDX5)
              MOVE  TNS-KOUHYOJYUNNUM
                                TO RECE230-KOUHYOJYUNNUM(SYUKEI-IDX5)
      *
              MOVE  TNS-SRYSYUKBN
                                TO RECE230-SRYSYUKBN-X(SYUKEI-IDX5)
              MOVE  TNS-KOKUJISKBKBN1
                                TO RECE230-KOKUJISKBKBN1(SYUKEI-IDX5)
              MOVE  TNS-KZMCOMPSIKIBETU
                                TO RECE230-KZMCOMPSIKIBETU(SYUKEI-IDX5)
              MOVE  TNS-CHUKSNCD
                                TO RECE230-CHUKSNCD(SYUKEI-IDX5)
              MOVE  TNS-CHUKSNTUBAN
                                TO RECE230-CHUKSNTUBAN(SYUKEI-IDX5)
              MOVE  TNS-KENTAICOMMENT
                                TO RECE230-KENTAICOMMENT(SYUKEI-IDX5)
              MOVE  TNS-KNSJISGRPKBN
                                TO RECE230-KNSJISGRPKBN(SYUKEI-IDX5)
              MOVE  TNS-TOKUKIZAISBT1
                                TO RECE230-TOKUKIZAISBT1(SYUKEI-IDX5)
              MOVE  TNS-SANSOKBN
                                TO RECE230-SANSOKBN(SYUKEI-IDX5)
      *
              MOVE  TNS-NAME    TO RECE230-NAME(SYUKEI-IDX5)
            END-IF
      *
            IF WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)  NOT = ZERO
              ADD   1     TO     SYUKEI-IDX5
              MOVE  WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)
                                     TO RECE230-SRYCD(SYUKEI-IDX5)
              MOVE  1                TO RECE230-SRYSURYO(SYUKEI-IDX5)
              MOVE  ZERO             TO RECE230-SRYKAISU(SYUKEI-IDX5)
              MOVE  SPACE            TO RECE230-MEISKYFLG(SYUKEI-IDX5)
              MOVE  SPACE            TO RECE230-AUTOKBN(SYUKEI-IDX5)
              MOVE  ZERO             TO RECE230-INPUTNUM(SYUKEI-IDX5)
              MOVE  WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)
                                     TO WRK-SRYCD
              MOVE  RECE230-SRYYM    TO WK-TAIHI-SRYYM
              MOVE  WK-TAIHI-DAY     TO WK-TAIHI-SRYD
              PERFORM    900-TENSU-TBL-SEC
              MOVE  TNS-TANICD       TO RECE230-TANICD(SYUKEI-IDX5)
              MOVE  TNS-TANIMOJISU   TO RECE230-TANIMOJISU(SYUKEI-IDX5)
              MOVE  TNS-TANINAME     TO RECE230-TANINAME(SYUKEI-IDX5)
              MOVE  TNS-TENSIKIBETU  TO RECE230-TENSIKIBETU(SYUKEI-IDX5)
              MOVE  TNS-TEN          TO RECE230-TEN(SYUKEI-IDX5)
              MOVE  TNS-NYUTENTTLKBN TO RECE230-TENTTLKBN(SYUKEI-IDX5)
              MOVE  TNS-YUKOKETA     TO RECE230-YUKOKETA(SYUKEI-IDX5)
              MOVE  TNS-DATAKBN      TO RECE230-DATAKBN (SYUKEI-IDX5)
              MOVE  TNS-HOUKNSKBN    TO RECE230-HOUKNSKBN(SYUKEI-IDX5)
              MOVE  TNS-KNSJISKBN    TO RECE230-KNSJISKBN(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-KBN    TO RECE230-CDKBN-KBN(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-SYO    TO RECE230-CDKBN-SYO(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-BU     TO RECE230-CDKBN-BU(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-KBNNUM
                                TO RECE230-CDKBN-KBNNUM(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-KBNNUM-EDA
                                TO RECE230-CDKBN-KBNNUM-EDA(SYUKEI-IDX5)
              MOVE  TNS-CDKBN-KOUBAN
                                TO RECE230-CDKBN-KOUBAN(SYUKEI-IDX5)
              MOVE  TNS-KOUHYOJYUNNUM
                                TO RECE230-KOUHYOJYUNNUM(SYUKEI-IDX5)
      *
              MOVE  TNS-SRYSYUKBN
                                TO RECE230-SRYSYUKBN-X(SYUKEI-IDX5)
              MOVE  TNS-KOKUJISKBKBN1
                                TO RECE230-KOKUJISKBKBN1(SYUKEI-IDX5)
              MOVE  TNS-KZMCOMPSIKIBETU
                                TO RECE230-KZMCOMPSIKIBETU(SYUKEI-IDX5)
              MOVE  TNS-CHUKSNCD
                                TO RECE230-CHUKSNCD(SYUKEI-IDX5)
              MOVE  TNS-CHUKSNTUBAN
                                TO RECE230-CHUKSNTUBAN(SYUKEI-IDX5)
              MOVE  TNS-KENTAICOMMENT
                                TO RECE230-KENTAICOMMENT(SYUKEI-IDX5)
              MOVE  TNS-KNSJISGRPKBN
                                TO RECE230-KNSJISGRPKBN(SYUKEI-IDX5)
              MOVE  TNS-TOKUKIZAISBT1
                                TO RECE230-TOKUKIZAISBT1(SYUKEI-IDX5)
              MOVE  TNS-SANSOKBN
                                TO RECE230-SANSOKBN(SYUKEI-IDX5)
      *
              MOVE  TNS-NAME    TO RECE230-NAME(SYUKEI-IDX5)
            END-IF
      *
           END-IF
      *
           .
       2102111-RECE230-TENSOU2-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       210213-RECE220-TENSOU2-SKJ200604-SEC               SECTION.
      *
           ADD      1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
           IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
              ADD   1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
           END-IF
           IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
              ADD   1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
           END-IF
      *
           ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
           ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 2)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
           ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 3)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
      *
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
             EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
              WHEN      1
                ADD     197001310
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
                  ADD   197001570
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
                  ADD   197001670
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
              WHEN      2
                ADD     197001710
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
             END-EVALUATE
           ELSE
             EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
              WHEN      1
                ADD     197000110
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
                  ADD   197000470
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
                  ADD   197000570
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
              WHEN      2
                ADD     197000710
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
             END-EVALUATE
           END-IF
      *
      *
           MOVE    SPACE                TO  RECE220-REC
           INITIALIZE                       RECE220-REC
           MOVE    RECE200-KEY          TO  RECE220-KEY1
           MOVE    "97"                 TO  RECE220-SRYKBN
           MOVE    "4"                  TO  RECE220-ZAISKBKBN
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                COMPUTE WRK-ZAINUM  =  99996100  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE220-ZAINUM
           ELSE
                COMPUTE WRK-ZAINUM  =  99996000  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE220-ZAINUM
           END-IF
      ***  COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
      ***  MOVE    WRK-ZAINUM           TO  RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO  RECE220-PTNUM
      *
           MOVE    SPACE                TO  RECE220-SRYKA
           MOVE    RECE200-HKNCOMBI     TO  RECE220-HKNCOMBI
           MOVE    SPACE                TO  RECE220-JIHOKBN
      *
           MOVE    WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                                        TO  RECE220-SRYCDTOTAL
           MOVE    WRK-SKJ200604-MEISAISU   (SYUKEI-IDX3)
                                        TO  RECE220-MEISAISU
           MOVE    1                    TO  RECE220-ZAIKAISU
           MOVE    WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                                        TO  RECE220-SURYOUTOTAL
           MOVE    ZERO                 TO  RECE220-RUIKEITOTAL
           MOVE    ZERO                 TO  RECE220-SYUTEN2
           MOVE    ZERO                 TO  RECE220-YKZTEN
           MOVE    ZERO                 TO  RECE220-KIZAITEN
           MOVE    SPACE                TO  RECE220-DRCD
           MOVE    SPACE                TO  RECE220-ZAIREQFLG
           MOVE    ZERO                 TO  RECE220-TEIGENRATE
           MOVE    1                    TO  RECE220-KAISU(1 SYUKEI-IDX3)
           MOVE    SYUKEI-IDX3          TO  WK-TAIHI-DAY
           .
       210213-RECE220-TENSOU2-SKJ200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê´Ä¶­¡Ë¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       210219-RECE220-TENSOU2-SKJ200610-KAN-SEC               SECTION.
      *
           MOVE    SPACE                TO  RECE220-REC
           INITIALIZE                       RECE220-REC
           MOVE    RECE200-KEY          TO  RECE220-KEY1
           MOVE    "97"                 TO  RECE220-SRYKBN
           MOVE    "4"                  TO  RECE220-ZAISKBKBN
           COMPUTE WRK-ZAINUM  =  99996300  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO  RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO  RECE220-PTNUM
      *
           MOVE    SPACE                TO  RECE220-SRYKA
           MOVE    RECE200-HKNCOMBI     TO  RECE220-HKNCOMBI
           MOVE    SPACE                TO  RECE220-JIHOKBN
      *
           EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
             WHEN      1
                MOVE    197001410       TO  RECE220-SRYCDTOTAL
             WHEN      2
                MOVE    197001810       TO  RECE220-SRYCDTOTAL
           END-EVALUATE
           MOVE    1                    TO  RECE220-MEISAISU
           MOVE    1                    TO  RECE220-ZAIKAISU
           MOVE    1                    TO  RECE220-SURYOUTOTAL
           MOVE    ZERO                 TO  RECE220-RUIKEITOTAL
           MOVE    ZERO                 TO  RECE220-SYUTEN2
           MOVE    ZERO                 TO  RECE220-YKZTEN
           MOVE    ZERO                 TO  RECE220-KIZAITEN
           MOVE    SPACE                TO  RECE220-DRCD
           MOVE    SPACE                TO  RECE220-ZAIREQFLG
           MOVE    ZERO                 TO  RECE220-TEIGENRATE
           MOVE    1                    TO  RECE220-KAISU(1 SYUKEI-IDX3)
           MOVE    SYUKEI-IDX3          TO  WK-TAIHI-DAY
           .
       210219-RECE220-TENSOU2-SKJ200610-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       210214-RECE230-HENSYU2-SKJ200604-SEC               SECTION.
      *
           MOVE    SPACE                TO   RECE230-REC
           INITIALIZE                        RECE230-REC
           MOVE    RECE200-KEY          TO   RECE230-KEY1
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                COMPUTE WRK-ZAINUM  =  99996100  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO   RECE230-ZAINUM
           ELSE
                COMPUTE WRK-ZAINUM  =  99996000  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO   RECE230-ZAINUM
           END-IF
      ***  COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
      ***  MOVE    WRK-ZAINUM           TO   RECE230-ZAINUM
           MOVE    1                    TO   RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO   RECE230-PTNUM
      *
           MOVE    SPACE                TO   RECE230-SRYKA
           MOVE    "97"                 TO   RECE230-SRYKBN
           MOVE    "970"                TO   RECE230-SRYSYUKBN
           MOVE    ZERO                 TO   RECE230-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL     IDX2     >     3
      *
             IF  WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX2)  NOT =  ZERO
      *
               ADD     1                    TO  IDX4
               IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
                 WHEN      1
                   EVALUATE    IDX2
                     WHEN       1
                         MOVE  "197001310"  TO  RECE230-SRYCD(IDX4)
                     WHEN       2
                         MOVE  "197001570"  TO  RECE230-SRYCD(IDX4)
                     WHEN       3
                         MOVE  "197001670"  TO  RECE230-SRYCD(IDX4)
                   END-EVALUATE
                 WHEN      2
                         MOVE  "197001710"  TO  RECE230-SRYCD(IDX4)
                END-EVALUATE
               ELSE
                EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
                 WHEN      1
                   EVALUATE    IDX2
                     WHEN       1
                         MOVE  "197000110"  TO  RECE230-SRYCD(IDX4)
                     WHEN       2
                         MOVE  "197000470"  TO  RECE230-SRYCD(IDX4)
                     WHEN       3
                         MOVE  "197000570"  TO  RECE230-SRYCD(IDX4)
                   END-EVALUATE
                 WHEN      2
                         MOVE  "197000710"  TO  RECE230-SRYCD(IDX4)
                END-EVALUATE
               END-IF
               MOVE    WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX2)
                                            TO  RECE230-SRYSURYO(IDX4)
               MOVE    ZERO                 TO  RECE230-SRYKAISU(IDX4)
               MOVE    SPACE                TO  RECE230-MEISKYFLG(IDX4)
               MOVE    SPACE                TO  RECE230-AUTOKBN(IDX4)
               MOVE    ZERO                 TO  RECE230-INPUTNUM(IDX4)
      *
               MOVE    RECE230-SRYCD(IDX4)  TO  WRK-SRYCD
               MOVE    RECE230-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
               PERFORM 900-TENSU-TBL-SEC
               MOVE    TNS-TANICD           TO  RECE230-TANICD(IDX4)
               MOVE    TNS-TANIMOJISU       TO  RECE230-TANIMOJISU(IDX4)
               MOVE    TNS-TANINAME         TO  RECE230-TANINAME(IDX4)
               MOVE    TNS-TENSIKIBETU      TO  RECE230-TENSIKIBETU
                                                                  (IDX4)
               MOVE    TNS-TEN              TO  RECE230-TEN(IDX4)
               MOVE    TNS-NYUTENTTLKBN     TO  RECE230-TENTTLKBN(IDX4)
               MOVE    TNS-YUKOKETA         TO  RECE230-YUKOKETA(IDX4)
               MOVE    TNS-DATAKBN          TO  RECE230-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN        TO  RECE230-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN        TO  RECE230-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN        TO  RECE230-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO        TO  RECE230-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU         TO  RECE230-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM     TO
                                          RECE230-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA TO
                                          RECE230-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN     TO
                                          RECE230-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM    TO
                                          RECE230-KOUHYOJYUNNUM(IDX4)
      *
               MOVE    TNS-SRYSYUKBN  TO  RECE230-SRYSYUKBN-X(IDX4)
               MOVE    TNS-KOKUJISKBKBN1
                                      TO  RECE230-KOKUJISKBKBN1(IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO  RECE230-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-CHUKSNCD   TO  RECE230-CHUKSNCD(IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO  RECE230-CHUKSNTUBAN(IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO  RECE230-KENTAICOMMENT(IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO  RECE230-KNSJISGRPKBN(IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO  RECE230-TOKUKIZAISBT1(IDX4)
               MOVE    TNS-SANSOKBN   TO  RECE230-SANSOKBN     (IDX4)
      *
               MOVE    TNS-NAME       TO  RECE230-NAME(IDX4)
      *
               COMPUTE RECE220-ZAITEN =
                       RECE220-ZAITEN +
                  (TNS-TEN * WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX2))
               COMPUTE RECE220-SYUTEN1 =
                       RECE220-SYUTEN1 +
                  (TNS-TEN * WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX2))
             END-IF
      *
           END-PERFORM
      *
           .
       210214-RECE230-HENSYU2-SKJ200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê´Ä¶­¡Ë¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       210220-RECE230-HENSYU2-SKJ200610-KAN-SEC               SECTION.
      *
           MOVE    SPACE                TO   RECE230-REC
           INITIALIZE                        RECE230-REC
           MOVE    RECE200-KEY          TO   RECE230-KEY1
           COMPUTE WRK-ZAINUM   =  99996300  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO   RECE230-ZAINUM
           MOVE    1                    TO   RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO   RECE230-PTNUM
      *
           MOVE    SPACE                TO   RECE230-SRYKA
           MOVE    "97"                 TO   RECE230-SRYKBN
           MOVE    "970"                TO   RECE230-SRYSYUKBN
           MOVE    ZERO                 TO   RECE230-JIHIMONEY
      *
      *
           EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
             WHEN      1
                 MOVE  "197001410"      TO  RECE230-SRYCD(1)
             WHEN      2
                 MOVE  "197001810"      TO  RECE230-SRYCD(1)
           END-EVALUATE
           MOVE    1                    TO  RECE230-SRYSURYO(1)
           MOVE    ZERO                 TO  RECE230-SRYKAISU(1)
           MOVE    SPACE                TO  RECE230-MEISKYFLG(1)
           MOVE    SPACE                TO  RECE230-AUTOKBN(1)
           MOVE    ZERO                 TO  RECE230-INPUTNUM(1)
      *
           MOVE    RECE230-SRYCD(1)     TO  WRK-SRYCD
           MOVE    RECE230-SRYYM        TO  WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
           PERFORM 900-TENSU-TBL-SEC
           MOVE    TNS-TANICD           TO  RECE230-TANICD(1)
           MOVE    TNS-TANIMOJISU       TO  RECE230-TANIMOJISU(1)
           MOVE    TNS-TANINAME         TO  RECE230-TANINAME(1)
           MOVE    TNS-TENSIKIBETU      TO  RECE230-TENSIKIBETU(1)
           MOVE    TNS-TEN              TO  RECE230-TEN(1)
           MOVE    TNS-NYUTENTTLKBN     TO  RECE230-TENTTLKBN(1)
           MOVE    TNS-YUKOKETA         TO  RECE230-YUKOKETA(1)
           MOVE    TNS-DATAKBN          TO  RECE230-DATAKBN (1)
           MOVE    TNS-HOUKNSKBN        TO  RECE230-HOUKNSKBN(1)
           MOVE    TNS-KNSJISKBN        TO  RECE230-KNSJISKBN(1)
           MOVE    TNS-CDKBN-KBN        TO  RECE230-CDKBN-KBN(1)
           MOVE    TNS-CDKBN-SYO        TO  RECE230-CDKBN-SYO(1)
           MOVE    TNS-CDKBN-BU         TO  RECE230-CDKBN-BU(1)
           MOVE    TNS-CDKBN-KBNNUM     TO  RECE230-CDKBN-KBNNUM(1)
           MOVE    TNS-CDKBN-KBNNUM-EDA TO  RECE230-CDKBN-KBNNUM-EDA(1)
           MOVE    TNS-CDKBN-KOUBAN     TO  RECE230-CDKBN-KOUBAN(1)
           MOVE    TNS-KOUHYOJYUNNUM    TO  RECE230-KOUHYOJYUNNUM(1)
      *
           MOVE    TNS-SRYSYUKBN        TO  RECE230-SRYSYUKBN-X(1)
           MOVE    TNS-KOKUJISKBKBN1    TO  RECE230-KOKUJISKBKBN1(1)
           MOVE    TNS-KZMCOMPSIKIBETU  TO  RECE230-KZMCOMPSIKIBETU(1)
           MOVE    TNS-CHUKSNCD         TO  RECE230-CHUKSNCD(1)
           MOVE    TNS-CHUKSNTUBAN      TO  RECE230-CHUKSNTUBAN(1)
           MOVE    TNS-KENTAICOMMENT    TO  RECE230-KENTAICOMMENT(1)
           MOVE    TNS-KNSJISGRPKBN     TO  RECE230-KNSJISGRPKBN(1)
           MOVE    TNS-TOKUKIZAISBT1    TO  RECE230-TOKUKIZAISBT1(1)
           MOVE    TNS-SANSOKBN         TO  RECE230-SANSOKBN(1)
      *
           MOVE    TNS-NAME             TO  RECE230-NAME(1)
      *
           MOVE    TNS-TEN              TO  RECE220-ZAITEN
           MOVE    TNS-TEN              TO  RECE220-SYUTEN1
      *
           .
       210220-RECE230-HENSYU2-SKJ200610-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2103-RECE220-HENSYU3-SEC                SECTION.
      *
      *    ÅÀÅ©¤Î·×»»
      *
           MOVE    RECE200-HOSPNUM      TO  SPA-HOSPNUM
      *
           INITIALIZE                      ORCSTENTEKAREA
           MOVE    "2"                 TO  ORCSTEN-KBN
           MOVE    RECE200-PTID        TO  ORCSTEN-PTID
           MOVE    RECE200-SRYYM       TO  ORCSTEN-SRYYM
           MOVE    RECE200-HKNCOMBI    TO  ORCSTEN-HKNCOMBI
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *
      *    IF      ORCSTEN-ERRCD       =   ZERO
      *
      *           ÌôºÞ
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                            UNTIL   IDY     >     31
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
                          PERFORM  VARYING  IDX9    FROM    1   BY  1
                            UNTIL  IDX9     >      5
                              OR   ORCSTEN-COM-SRYCD(IDY IDX5 IDX9)
                                                              = SPACE
                             ADD   1   TO   IDX6
                             MOVE  "810000001"
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  1   TO   WRK-ORCSTEN-SURYO(IDX6)
                             MOVE  ORCSTEN-COM-MEI(IDY IDX5 IDX9)
                                       TO   WRK-ORCSTEN-COM-MEI(IDX6)
                          END-PERFORM
                       END-PERFORM
      *
                       PERFORM   21032-RECE220-TENSOU3-SEC
      *
                       MOVE      350  TO   IDX5
                       PERFORM   21033-RECE230-TENSOU3-SEC
                       PERFORM   21034-RECE200-HENSYU3-SEC
      *
                       PERFORM   RECE220-WRITE-SEC
      *
                    END-IF
      *
                  END-PERFORM
      *
      *           ´ïºà
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                            UNTIL   IDY     >     50
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
                          PERFORM  VARYING  IDX9    FROM    1   BY  1
                            UNTIL  IDX9     >      5
                              OR   ORCSTEN-KIZ-COM-SRYCD(IDY IDX5 IDX9)
                                                              = SPACE
                             ADD   1   TO   IDX6
                             MOVE  "810000001"
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  1   TO   WRK-ORCSTEN-SURYO(IDX6)
                             MOVE  ORCSTEN-KIZ-COM-MEI(IDY IDX5 IDX9)
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
                       PERFORM   21032-RECE220-TENSOU3-SEC
      *
                       MOVE      60   TO   IDX5
                       PERFORM   21033-RECE230-TENSOU3-SEC
                       PERFORM   21034-RECE200-HENSYU3-SEC
      *
                       PERFORM   RECE220-WRITE-SEC
      *
                  END-PERFORM
      *
      *    END-IF
      *
      ***
      *
      *    ÅÀÅ©¼êµ»²Ã»»
      *
           IF   WRK-TENTEKI2-T-ERRCD     =  ZERO
                 PERFORM  VARYING  IDY    FROM    1   BY  1
                           UNTIL   IDY     >     31
                    IF  WRK-TENTEKI2-T-ZAITEN(IDY)  NOT =  ZERO
                       MOVE      3   TO   FLG-SIKIBETU
                       PERFORM   21032X-RECE220-TENSOU3-SEC
                       MOVE      WRK-TENTEKI2-T-SRYCD-X(IDY)
                                     TO   WRK-TENTEKI2-SRYCD-X
                       PERFORM   21033X-RECE230-TENSOU3-SEC
                       PERFORM   21034-RECE200-HENSYU3-SEC
                       PERFORM   RECE220-WRITE-SEC
                    END-IF
                 END-PERFORM
           END-IF
      *
           IF   WRK-TENTEKI2-C-ERRCD     =  ZERO
                 PERFORM  VARYING  IDY    FROM    1   BY  1
                           UNTIL   IDY     >     31
                    IF  WRK-TENTEKI2-C-ZAITEN(IDY)  NOT =  ZERO
                       MOVE      4   TO   FLG-SIKIBETU
                       PERFORM   21032X-RECE220-TENSOU3-SEC
                       MOVE      WRK-TENTEKI2-C-SRYCD-X(IDY)
                                     TO   WRK-TENTEKI2-SRYCD-X
                       PERFORM   21033X-RECE230-TENSOU3-SEC
                       PERFORM   21034-RECE200-HENSYU3-SEC
                       PERFORM   RECE220-WRITE-SEC
                    END-IF
                 END-PERFORM
           END-IF
      *
           .
       2103-RECE220-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       21032-RECE220-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE220-REC
           INITIALIZE                           RECE220-REC
           MOVE    RECE200-KEY          TO      RECE220-KEY1
           MOVE    "33"                 TO      RECE220-SRYKBN
           MOVE    SPACE                TO      RECE220-ZAISKBKBN
           IF    FLG-SIKIBETU  =  1
             COMPUTE WRK-ZAINUM    =  99997000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99997400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO      RECE220-PTNUM
      *
           MOVE    "00"                 TO      RECE220-SRYKA
           MOVE    RECE200-HKNCOMBI     TO      RECE220-HKNCOMBI
           MOVE    SPACE                TO      RECE220-JIHOKBN
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE220-ZAITEN
           ELSE
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)
                                        TO      RECE220-ZAITEN
           END-IF
           MOVE    ZERO                 TO      RECE220-SRYCDTOTAL
           MOVE    ZERO                 TO      RECE220-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE220-MEISAISU
           IF    FLG-SIKIBETU  =  1
             MOVE  1                    TO      RECE220-ZAIKAISU
           ELSE
             MOVE  WRK-ORCSTEN-ZAIKAISU TO      RECE220-ZAIKAISU
           END-IF
           MOVE    ZERO                 TO      RECE220-SYUTEN1
           MOVE    ZERO                 TO      RECE220-SYUTEN2
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE220-YKZTEN
             MOVE  ZERO                 TO      RECE220-KIZAITEN
           ELSE
             MOVE  ZERO                 TO      RECE220-YKZTEN
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)
                                        TO      RECE220-KIZAITEN
           END-IF
           MOVE    SPACE                TO      RECE220-DRCD
           MOVE    SPACE                TO      RECE220-ZAIREQFLG
           MOVE    ZERO                 TO      RECE220-TEIGENRATE
      *
           IF    FLG-SIKIBETU  =  1
             MOVE    1                  TO      RECE220-KAISU (1 IDY)
             MOVE    IDY                TO      WK-TAIHI-DAY
           ELSE
             MOVE        ZERO           TO      WK-TAIHI-DAY
             MOVE        ZERO           TO      WK-TAIHI-FLG
             PERFORM     VARYING   IDX2   FROM    1    BY   1
                          UNTIL    IDX2     >    31
                   MOVE    WRK-ORCSTEN-DAY(IDX2)  TO
                                            RECE220-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (WRK-ORCSTEN-DAY(IDX2)  NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF
             END-PERFORM
           END-IF
           .
       21032-RECE220-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       21032X-RECE220-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE220-REC
           INITIALIZE                           RECE220-REC
           MOVE    RECE200-KEY          TO      RECE220-KEY1
           MOVE    "33"                 TO      RECE220-SRYKBN
           MOVE    SPACE                TO      RECE220-ZAISKBKBN
           IF    FLG-SIKIBETU  =  3
             COMPUTE WRK-ZAINUM    =  99995000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99995400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO      RECE220-PTNUM
      *
           MOVE    "00"                 TO      RECE220-SRYKA
           MOVE    RECE200-HKNCOMBI     TO      RECE220-HKNCOMBI
           MOVE    SPACE                TO      RECE220-JIHOKBN
      *
           IF    FLG-SIKIBETU  =  3
             MOVE  WRK-TENTEKI2-T-ZAITEN(IDY)
                                        TO      RECE220-ZAITEN
                                                RECE220-SYUTEN1
             MOVE  WRK-TENTEKI2-T-MEISAISU(IDY)
                                        TO      RECE220-MEISAISU
                                                RECE220-SURYOUTOTAL
             MOVE  WRK-TENTEKI2-T-SRYCDTOTAL(IDY)
                                        TO      RECE220-SRYCDTOTAL
           ELSE
             MOVE  WRK-TENTEKI2-C-ZAITEN(IDY)
                                        TO      RECE220-ZAITEN
                                                RECE220-SYUTEN1
             MOVE  WRK-TENTEKI2-C-MEISAISU(IDY)
                                        TO      RECE220-MEISAISU
                                                RECE220-SURYOUTOTAL
             MOVE  WRK-TENTEKI2-C-SRYCDTOTAL(IDY)
                                        TO      RECE220-SRYCDTOTAL
           END-IF
           MOVE    1                    TO      RECE220-ZAIKAISU
      *
           MOVE    ZERO                 TO      RECE220-SYUTEN2
           MOVE    ZERO                 TO      RECE220-YKZTEN
           MOVE    ZERO                 TO      RECE220-KIZAITEN
           MOVE    SPACE                TO      RECE220-DRCD
           MOVE    SPACE                TO      RECE220-ZAIREQFLG
           MOVE    ZERO                 TO      RECE220-TEIGENRATE
      *
           MOVE    1                    TO      RECE220-KAISU (1 IDY)
           MOVE    IDY                  TO      WK-TAIHI-DAY
           .
       21032X-RECE220-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       21033-RECE230-TENSOU3-SEC                SECTION.
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
              MOVE    SPACE                TO   RECE230-REC
              INITIALIZE                        RECE230-REC
              MOVE    RECE200-KEY          TO   RECE230-KEY1
              IF    FLG-SIKIBETU  =  1
                COMPUTE WRK-ZAINUM    =  99997000  +  IDY
              ELSE
                COMPUTE WRK-ZAINUM    =  99997400  +  IDY
              END-IF
              MOVE    WRK-ZAINUM           TO   RECE230-ZAINUM
              ADD     1                    TO   WRK-RENNUM
              MOVE    WRK-RENNUM           TO   RECE230-RENNUM
              MOVE    RECE200-PTNUM        TO   RECE230-PTNUM
      *
              MOVE    "00"                 TO   RECE230-SRYKA
              MOVE    "33"                 TO   RECE230-SRYKBN
              MOVE    "330"                TO   RECE230-SRYSYUKBN
              MOVE    ZERO                 TO   RECE230-JIHIMONEY
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
                                            TO  RECE230-SRYCD(IDX4)
                  MOVE  WRK-ORCSTEN-SURYO(IDX2) 
                                            TO  RECE230-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE230-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE230-MEISKYFLG(IDX4)
                  MOVE  SPACE               TO  RECE230-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE230-INPUTNUM(IDX4)
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  WRK-SRYCD
                  MOVE  RECE230-SRYYM       TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM    900-TENSU-TBL-SEC
                  IF  WRK-ORCSTEN-SRYCD(IDX2)  =  CONS-ZANHAIKI
                      MOVE   WRK-TANICD     TO  RECE230-TANICD(IDX4)
                      MOVE   WRK-TANIMOJISU TO  RECE230-TANIMOJISU(IDX4)
                      MOVE   WRK-TANINAME   TO  RECE230-TANINAME(IDX4)
                  ELSE
                      MOVE  TNS-TANICD      TO  RECE230-TANICD(IDX4)
                                                WRK-TANICD
                      MOVE  TNS-TANIMOJISU  TO  RECE230-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU
                      MOVE  TNS-TANINAME    TO  RECE230-TANINAME(IDX4)
                                                WRK-TANINAME
                  END-IF
                  MOVE  TNS-TENSIKIBETU     TO  RECE230-TENSIKIBETU
                                                                 (IDX4)
                  MOVE  TNS-TEN             TO  RECE230-TEN(IDX4)
                  MOVE  TNS-NYUTENTTLKBN    TO  RECE230-TENTTLKBN(IDX4)
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                     MOVE   ZERO            TO  RECE230-YUKOKETA(IDX4)
                  ELSE
                     MOVE   TNS-YUKOKETA    TO  RECE230-YUKOKETA(IDX4)
                  END-IF
                  MOVE  TNS-DATAKBN         TO  RECE230-DATAKBN (IDX4)
                  MOVE  TNS-HOUKNSKBN       TO  RECE230-HOUKNSKBN(IDX4)
                  MOVE  TNS-KNSJISKBN       TO  RECE230-KNSJISKBN(IDX4)
                  MOVE  TNS-CDKBN-KBN       TO  RECE230-CDKBN-KBN(IDX4)
                  MOVE  TNS-CDKBN-SYO       TO  RECE230-CDKBN-SYO(IDX4)
                  MOVE  TNS-CDKBN-BU        TO  RECE230-CDKBN-BU(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM
                                      TO RECE230-CDKBN-KBNNUM(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO RECE230-CDKBN-KBNNUM-EDA(IDX4)
                  MOVE  TNS-CDKBN-KOUBAN
                                      TO RECE230-CDKBN-KOUBAN(IDX4)
                  MOVE  TNS-KOUHYOJYUNNUM
                                      TO RECE230-KOUHYOJYUNNUM(IDX4)
      *
                  MOVE  TNS-SRYSYUKBN TO RECE230-SRYSYUKBN-X(IDX4)
                  MOVE  TNS-KOKUJISKBKBN1
                                      TO RECE230-KOKUJISKBKBN1(IDX4)
                  MOVE  TNS-KZMCOMPSIKIBETU
                                      TO RECE230-KZMCOMPSIKIBETU(IDX4)
                  MOVE  TNS-CHUKSNCD  TO RECE230-CHUKSNCD(IDX4)
                  MOVE  TNS-CHUKSNTUBAN
                                      TO RECE230-CHUKSNTUBAN(IDX4)
                  MOVE  TNS-KENTAICOMMENT
                                      TO RECE230-KENTAICOMMENT(IDX4)
                  MOVE  TNS-KNSJISGRPKBN
                                      TO RECE230-KNSJISGRPKBN(IDX4)
                  MOVE  TNS-TOKUKIZAISBT1
                                      TO RECE230-TOKUKIZAISBT1(IDX4)
                  MOVE  TNS-SANSOKBN  TO RECE230-SANSOKBN     (IDX4)
      *
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                     MOVE   WRK-ORCSTEN-COM-MEI(IDX2)
                                      TO RECE230-NAME(IDX4)
                  ELSE
                     MOVE   TNS-NAME  TO RECE230-NAME(IDX4)
                  END-IF
      *
                  IF   WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "058"
                    MOVE  TNS-FORMALYUKOKETA TO  RECE230-YUKOKETA(IDX4)
                    MOVE  TNS-FORMALNAME     TO  RECE230-NAME(IDX4)
                    MOVE  1                  TO  FLG-FORMALNAME
                  END-IF
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "7")
                  AND (FLG-FORMALNAME                =    1 )
                    MOVE  "1"             TO  RECE230-RECEHENFLG(IDX4)
                    MOVE  ZERO            TO  FLG-FORMALNAME
                  END-IF
      *
                  IF   RECE230-YUKOKETA(IDX4)  =  ZERO
                     PERFORM   VARYING   IDW   FROM   200   BY  -1
                                UNTIL    IDW   <   1
                       IF   RECE230-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE230-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                     END-PERFORM
                  END-IF
      *
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                        MOVE     "1"             TO  RECE220-COMCDFLG
                  END-IF
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2)  TO  WRK-SRYCD
                  COMPUTE RECE220-SRYCDTOTAL  =  RECE220-SRYCDTOTAL
                                              +  WRK-SRYCD
                  COMPUTE RECE220-SURYOUTOTAL =  RECE220-SURYOUTOTAL
                                              +  WRK-ORCSTEN-SURYO(IDX2)
                  COMPUTE RECE220-MEISAISU    =  RECE220-MEISAISU
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
                      COMPUTE  RECE220-RUIKEITOTAL  =
                               RECE220-RUIKEITOTAL  +
                             ( WRK-FUKA-VALUE    * WRK-SRYCD)
                      COMPUTE  WRK-FUKA-KINGAKU    =
                               WRK-FUKA-KINGAKU    +
                             ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                   * WRK-ORCSTEN-SURYO(IDX2))
                    END-IF
                  END-PERFORM
                  COMPUTE  RECE220-RUIKEITOTAL  =
                           RECE220-RUIKEITOTAL  +
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
                        PERFORM 2101212-RECE230-TENSOU-TANKA-HEN-SEC
                        COMPUTE IDW =   RECE230-YUKOKETA(IDX4)  *  2
                                    +   3
                        MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                   TO   RECE230-NAME(IDX4)(IDW:)
                        COMPUTE RECE230-YUKOKETA(IDX4)  =
                                RECE230-YUKOKETA(IDX4)  +
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
              PERFORM   RECE230-WRITE-SEC
      *
           END-PERFORM
      *
           .
       21033-RECE230-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       21033X-RECE230-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO   RECE230-REC
           INITIALIZE                        RECE230-REC
           MOVE    RECE200-KEY          TO   RECE230-KEY1
           IF    FLG-SIKIBETU  =  3
             COMPUTE WRK-ZAINUM    =  99995000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99995400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO   RECE230-ZAINUM
           MOVE    1                    TO   RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO   RECE230-PTNUM
      *
           MOVE    "00"                 TO   RECE230-SRYKA
           MOVE    "33"                 TO   RECE230-SRYKBN
           IF    FLG-SIKIBETU  =  3
              MOVE    "330"             TO   RECE230-SRYSYUKBN
           ELSE
              MOVE    "340"             TO   RECE230-SRYSYUKBN
           END-IF
           MOVE    ZERO                 TO   RECE230-JIHIMONEY
      *
           MOVE    ZERO                 TO   IDX4
           PERFORM   VARYING   IDX2   FROM    1    BY   1
                      UNTIL   (IDX2                      >     5  )
                        OR    (WRK-TENTEKI2-SRYCD(IDX2)  =   SPACE)
      *
                  ADD   1                   TO  IDX4
      *
                  MOVE  WRK-TENTEKI2-SRYCD(IDX2)
                                            TO  RECE230-SRYCD(IDX4)
                  MOVE  1                   TO  RECE230-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE230-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE230-MEISKYFLG(IDX4)
                  MOVE  SPACE               TO  RECE230-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE230-INPUTNUM(IDX4)
                  MOVE  WRK-TENTEKI2-SRYCD(IDX2)
                                            TO  WRK-SRYCD
                  MOVE  RECE230-SRYYM       TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM    900-TENSU-TBL-SEC
                  MOVE  TNS-TANICD          TO  RECE230-TANICD(IDX4)
                  MOVE  TNS-TANIMOJISU      TO  RECE230-TANIMOJISU(IDX4)
                  MOVE  TNS-TANINAME        TO  RECE230-TANINAME(IDX4)
                  MOVE  TNS-TENSIKIBETU     TO  RECE230-TENSIKIBETU
                                                                (IDX4)
                  MOVE  TNS-TEN             TO  RECE230-TEN(IDX4)
                  MOVE  TNS-NYUTENTTLKBN    TO  RECE230-TENTTLKBN(IDX4)
                  MOVE  TNS-YUKOKETA        TO  RECE230-YUKOKETA(IDX4)
                  MOVE  TNS-DATAKBN         TO  RECE230-DATAKBN (IDX4)
                  MOVE  TNS-HOUKNSKBN       TO  RECE230-HOUKNSKBN(IDX4)
                  MOVE  TNS-KNSJISKBN       TO  RECE230-KNSJISKBN(IDX4)
                  MOVE  TNS-CDKBN-KBN       TO  RECE230-CDKBN-KBN(IDX4)
                  MOVE  TNS-CDKBN-SYO       TO  RECE230-CDKBN-SYO(IDX4)
                  MOVE  TNS-CDKBN-BU        TO  RECE230-CDKBN-BU(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM
                                      TO RECE230-CDKBN-KBNNUM(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO RECE230-CDKBN-KBNNUM-EDA(IDX4)
                  MOVE  TNS-CDKBN-KOUBAN
                                      TO RECE230-CDKBN-KOUBAN(IDX4)
                  MOVE  TNS-KOUHYOJYUNNUM
                                      TO RECE230-KOUHYOJYUNNUM(IDX4)
      *
                  MOVE  TNS-SRYSYUKBN TO RECE230-SRYSYUKBN-X(IDX4)
                  MOVE  TNS-KOKUJISKBKBN1
                                      TO RECE230-KOKUJISKBKBN1(IDX4)
                  MOVE  TNS-KZMCOMPSIKIBETU
                                      TO RECE230-KZMCOMPSIKIBETU(IDX4)
                  MOVE  TNS-CHUKSNCD  TO RECE230-CHUKSNCD(IDX4)
                  MOVE  TNS-CHUKSNTUBAN
                                      TO RECE230-CHUKSNTUBAN(IDX4)
                  MOVE  TNS-KENTAICOMMENT
                                      TO RECE230-KENTAICOMMENT(IDX4)
                  MOVE  TNS-KNSJISGRPKBN
                                      TO RECE230-KNSJISGRPKBN(IDX4)
                  MOVE  TNS-TOKUKIZAISBT1
                                      TO RECE230-TOKUKIZAISBT1(IDX4)
                  MOVE  TNS-SANSOKBN  TO RECE230-SANSOKBN     (IDX4)
      *
                  MOVE  TNS-NAME      TO RECE230-NAME(IDX4)
      *
                  MOVE  ZERO          TO WRK-FUKA-KINGAKU
                  PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                            UNTIL   FUKA-IDX   >    9
                    MOVE   WRK-TENTEKI2-SRYCD(IDX2)(FUKA-IDX:1)
                                            TO  WRK-FUKA-VALUE
                    IF  WRK-FUKA-VALUE  =  ZERO
                      CONTINUE
                    ELSE
                      COMPUTE  RECE220-RUIKEITOTAL =
                               RECE220-RUIKEITOTAL +
                             ( WRK-FUKA-VALUE    * WRK-SRYCD)
                      COMPUTE  WRK-FUKA-KINGAKU    =
                               WRK-FUKA-KINGAKU    +
                             ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                 * 1)
                    END-IF
                  END-PERFORM
                  COMPUTE  RECE220-RUIKEITOTAL  =
                           RECE220-RUIKEITOTAL  +
                           WRK-FUKA-KINGAKU
      *
           END-PERFORM
      *
           IF     IDX4    NOT =  ZERO
              PERFORM   RECE230-WRITE-SEC
           END-IF
      *
           .
       21033X-RECE230-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       21034-RECE200-HENSYU3-SEC               SECTION.
      *
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
           ADD   RECE220-ZAIKAISU    TO   RECE200-KAISU(10)
      *
           COMPUTE   RECE200-TENSU(10 1)  =
                     RECE200-TENSU(10 1)  +
                    (RECE220-SYUTEN1   *  RECE220-ZAIKAISU)
           COMPUTE   RECE200-TENSU(11 2)  =
                     RECE200-TENSU(11 2)  +
                    (RECE220-YKZTEN    *  RECE220-ZAIKAISU)
           COMPUTE   RECE200-TENSU(11 2)  =
                     RECE200-TENSU(11 2)  +
                    (RECE220-KIZAITEN  *  RECE220-ZAIKAISU)
      *
           .
      *
       21034-RECE200-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Ø¤ÎÉéÃ´¶â¥»¥Ã¥È¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2104-FTN-SET-SEC                SECTION.
      *
           MOVE    ZERO                 TO      FLG-KHNRYO-SKJ
      *
           MOVE    ZERO                 TO      FLG-NYUINRYO
      *
           MOVE    ZERO                 TO      FLG-SYUNOU-ARI
      *
           INITIALIZE                           SYUNOU-REC
           MOVE    RECE200-HOSPNUM      TO      SYU-HOSPNUM
           MOVE    RECE200-NYUGAIKBN    TO      SYU-NYUGAIKBN
           MOVE    RECE200-PTID         TO      SYU-PTID
           MOVE    RECE200-SRYYM        TO      SYU-SRYYMD(1:6)
           MOVE    "__"                 TO      SYU-SRYYMD(7:2)
           MOVE    RECE200-HKNCOMBI     TO      SYU-HKNCOMBINUM
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key19"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_syunou"     TO      MCP-TABLE
               MOVE    "key19"          TO      MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
               PERFORM  UNTIL
                  (FLG-SYUNOU             =    1)
      *
                   IF  SYU-DENPJTIKBN    NOT =  "3"
                       PERFORM   21044-RECE200-FTN-HENSYU-SEC
                       MOVE   1   TO   FLG-SYUNOU-ARI
                   END-IF
      *
                   MOVE    "tbl_syunou" TO      MCP-TABLE
                   MOVE    "key19"      TO      MCP-PATHNAME
                   PERFORM  900-SYUNOU-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SYUNOU SELECT ERR"
           END-IF
      *
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key19"              TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           IF   FLG-SYUNOU-ARI  =  ZERO
              DISPLAY "<SYUNOU NG STOP>"
              MOVE    "²¾¼ýÇ¼¥Ç¡¼¥¿¤¬¤¢¤ê¤Þ¤»¤ó"  TO  ERR-ERRMSG
              PERFORM 600-ERRMSG-EDIT-SEC
              MOVE    ERR-HEN-ERRMSG    TO  WRK-RECEERR
              PERFORM 500-ERR-HENSYU-SEC
              MOVE    1                 TO  FLG-RECE200
           END-IF
      *
           .
       2104-FTN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡ÝÉéÃ´¶â¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       21044-RECE200-FTN-HENSYU-SEC               SECTION.
      *
           IF   RECE200-NYUINRYOFLG       =   "1"
           AND  FLG-NYUINRYO              =   ZERO
               PERFORM         210441-NYUINRYOFLG-SET-SEC
           END-IF
      *
           PERFORM             210445-SKJ-KINGAKU-HENSYU-SEC
      *
           .
       21044-RECE200-FTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ¤ò»»Äê¤·¤Æ¤¤¤ë¾ì¹ç¤ÎÊÝ¸±·èÄê
      *****************************************************************
       210441-NYUINRYOFLG-SET-SEC               SECTION.
      *
           MOVE     SPACE    TO    RECE200-NYUINRYOFLG
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
      *
             IF      SYU-DAY(IDY)    =    "2"
      *
                IF         WRK-DAY5(IDY)      NOT =  ZERO
                AND        WRK-DAY1(IDY)      NOT =  ZERO
                   PERFORM  2104411-NYUINRYOFLG-HENSYU-SEC
                ELSE
                   IF      WRK-DAY5(IDY)               NOT =  ZERO
                   AND     RECE200-NYUINDAY-KBN(IDY)   NOT =  "1"
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE200-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "1"     TO   RECE200-NYUINRYOFLG
                       WHEN     "2"
                          MOVE    "6"     TO   RECE200-NYUINRYOFLG
                       WHEN     "3"
                          PERFORM  2104411-NYUINRYOFLG-HENSYU-SEC
                     END-EVALUATE
                   END-IF
                   IF      WRK-DAY5(IDY)               NOT =  ZERO
                   AND     RECE200-NYUINDAY-KBN(IDY)       =  "1"
                   AND     RECE200-SRYYM                  >=  "200904"
                          PERFORM  2104411-NYUINRYOFLG-HENSYU-SEC
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
       210441-NYUINRYOFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÊÔ½¸
      *****************************************************************
       2104411-NYUINRYOFLG-HENSYU-SEC               SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    RECE200-HOSPNUM TO  COMB-HOSPNUM
           MOVE    RECE200-PTID    TO  COMB-PTID
           MOVE    WRK-DAY5(IDY)   TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 900-HKNCOMBI-INV-SEC
           IF      FLG-HKNCOMBI    =   ZERO
              IF   COMB-HKNNUM(1:2)  =  "98"
              OR   COMB-HKNNUM       =  "971"
              OR   COMB-HKNNUM       =  "973"
              OR   COMB-HKNNUM       =  "975"
                IF   COMB-HKNNUM(1:2)  =  "98"
                   MOVE    "1"   TO   RECE200-NYUINRYOFLG
                END-IF
                IF   COMB-HKNNUM       =  "971"
                   MOVE    "2"   TO   RECE200-NYUINRYOFLG
                END-IF
                IF   COMB-HKNNUM       =  "973"
                   MOVE    "3"   TO   RECE200-NYUINRYOFLG
                END-IF
                IF   COMB-HKNNUM       =  "975"
                   CONTINUE
                END-IF
              ELSE
                EVALUATE  COMB-HKNNUM
                  WHEN     "060"
                  WHEN     "067"
                  WHEN     "068"
                  WHEN     "069"
                    MOVE   "4"   TO   RECE200-NYUINRYOFLG
                  WHEN     "039"
                  WHEN     "040"
                    MOVE   "8"   TO   RECE200-NYUINRYOFLG
                  WHEN      OTHER
                    MOVE   "5"   TO   RECE200-NYUINRYOFLG
                END-EVALUATE
              END-IF
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2104411-NYUINRYOFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¶â³ÛÊÔ½¸
      *****************************************************************
       210445-SKJ-KINGAKU-HENSYU-SEC               SECTION.
      *
           IF    RECE200-SRYYM  >= "200610"
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >    10
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                   CONTINUE
               ELSE
                   EVALUATE    ID1
                     WHEN       1
                     WHEN       5
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(1)
                      IF   FLG-KHNRYO-SKJ   =   ZERO
                        IF   RECE200-SHOKUJIKHNRYO(1)     =  ZERO
                          MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE200-SHOKUJIKHNRYO(1)
                        ELSE
                          IF RECE200-SHOKUJIKHNRYO(1) NOT =
                                                SYU-SHOKUJI(ID1)
                            MOVE   ZERO  TO  RECE200-SHOKUJIKHNRYO(1)
                            MOVE   1     TO  FLG-KHNRYO-SKJ
                          END-IF
                        END-IF
                      END-IF
                      MOVE  "£±"   TO  RECE200-SHOKUJISYUBETU-KHN
                     WHEN       2
                     WHEN       6
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(1)
                      IF   FLG-KHNRYO-SKJ   =   ZERO
                        IF   RECE200-SHOKUJIKHNRYO(1)     =  ZERO
                          MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE200-SHOKUJIKHNRYO(1)
                        ELSE
                          IF RECE200-SHOKUJIKHNRYO(1) NOT =
                                                SYU-SHOKUJI(ID1)
                            MOVE   ZERO  TO  RECE200-SHOKUJIKHNRYO(1)
                            MOVE   1     TO  FLG-KHNRYO-SKJ
                          END-IF
                        END-IF
                      END-IF
                      MOVE  "£²"   TO  RECE200-SHOKUJISYUBETU-KHN
                     WHEN       3
                     WHEN       7
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(2)
                      MOVE  "ÆÃÊÌ" TO  RECE200-SHOKUJISYUBETU(1)
                     WHEN       4
                     WHEN       8
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(3)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(3)
                      MOVE  "¿©Æ²" TO  RECE200-SHOKUJISYUBETU(2)
                     WHEN       9
                     WHEN      10
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(4)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(4)
                      MOVE  "´Ä¶­" TO  RECE200-SHOKUJISYUBETU(3)
                   END-EVALUATE
               END-IF
      *
             END-PERFORM
      *
           END-IF
      *
           IF    RECE200-SRYYM  >= "200604"
           AND   RECE200-SRYYM  <  "200610"
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >     4
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                 CONTINUE
               ELSE
                 EVALUATE  ID1
                   WHEN       1
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(1)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(1)
                     MOVE  "£±"   TO  RECE200-SHOKUJISYUBETU-KHN
                   WHEN       2
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(1)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(1)
                     MOVE  "£²"   TO  RECE200-SHOKUJISYUBETU-KHN
                   WHEN       3
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(2)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(2)
                     MOVE  "ÆÃÊÌ" TO  RECE200-SHOKUJISYUBETU(1)
                   WHEN       4
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(3)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(3)
                     MOVE  "¿©Æ²" TO  RECE200-SHOKUJISYUBETU(2)
                 END-EVALUATE
               END-IF
      *
             END-PERFORM
      *
           END-IF
      *
           .
       210445-SKJ-KINGAKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¹ç·×·×»»
      *****************************************************************
       2105-GOKEI-KEISAN-SEC                SECTION.
      *
      *    ¾®·×
           PERFORM VARYING IDX    FROM    1   BY  1
                   UNTIL   IDX    >      21
               ADD    RECE200-TENSU(IDX 1)   TO   RECE200-SYOUKEI(1)
               ADD    RECE200-TENSU(IDX 2)   TO   RECE200-SYOUKEI(2)
           END-PERFORM
      *
           PERFORM VARYING IDX    FROM    1   BY  1
                   UNTIL   IDX    >      10
               ADD    RECE200-NYUKHNKEI(IDX) TO   RECE200-SYOUKEI(1)
               ADD    RECE200-NYUTOKKEI(IDX) TO   RECE200-SYOUKEI(1)
           END-PERFORM
           ADD        RECE200-NYUKHNKEI2     TO   RECE200-SYOUKEI(1)
           ADD        RECE200-NYUTOKKEI2     TO   RECE200-SYOUKEI(1)
      *
           PERFORM VARYING IDX    FROM    1   BY  1
                   UNTIL   IDX    >       3
               ADD    RECE200-KOGAISIDOKEI(IDX)
                                             TO   RECE200-SYOUKEI(2)
           END-PERFORM
           ADD        RECE200-SEIKANKEI      TO   RECE200-SYOUKEI(2)
      *
           PERFORM VARYING IDX    FROM    1   BY  1
                   UNTIL   IDX    >       4
               IF      RECE200-SHOKUJIKHNRYO (IDX)  >   ZERO
                   COMPUTE WRK-SYOKUJIKEI = RECE200-SHOKUJIKHNRYO  (IDX)
                                          * RECE200-SHOKUJIKHNNISSU(IDX)
                   ADD     WRK-SYOKUJIKEI    TO   RECE200-SYOUKEI(3)
               END-IF
           END-PERFORM
      *
      *    ¹ç·×¡Ê£±£²±ß¡ß¼êµ»ÅÀ¡Ë
           COMPUTE    RECE200-TOTALTEN(1)  =  RECE200-SYOUKEI(1)
                                           *  12
      *    ¹ç·×¡Ê£±£°±ß¡ßÌôºÞÅùÅÀ¡Ë
           COMPUTE    RECE200-TOTALTEN(2)  =  RECE200-SYOUKEI(2)
                                           *  10
      *    ¹ç·×¡Ê£±¡¥£²¡ß¿©»ö¡Ë
           COMPUTE    RECE200-TOTALTEN(3)  =  RECE200-SYOUKEI(3)
                                           *  1.2
      *    Áí¹ç·×
           COMPUTE    RECE200-TOTALTEN(4)  =  RECE200-TOTALTEN(1)
                                           +  RECE200-TOTALTEN(2)
                                           +  RECE200-TOTALTEN(3)
      *
           .
       2105-GOKEI-KEISAN-EXT.
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
           MOVE    ZERO                TO  WRK-NUM-9
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
      *****************ELSE
      *****************    MOVE    ZERO            TO  WRK-NUM
      *****************    MOVE    WRK-EDIDX       TO  IDX
      *****************    MOVE    10              TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    IDX3                TO  IDY     
               ELSE
      *        È¾³Ñ¿ô»ú¤ËÊÑ´¹¡Ê¿ô»ú°Ê³°¤Î¤È¤­¤Ï¥¼¥í¡Ë
                   INSPECT WRK-ZENKAKU(WRK-STIDX:)  REPLACING   ALL " "
                                                                BY  "0"
                   IF      WRK-ZENKAKU(WRK-STIDX:)     NOT NUMERIC
                       PERFORM VARYING IDX     FROM    WRK-STIDX  BY  1
                               UNTIL   IDX     >       WRK-EDIDX
                           IF      WRK-ZENKAKU (IDX:1) >=  "0" AND
                                                       <=  "9"
                               ADD     1           TO  IDX3
                               MOVE    WRK-ZENKAKU (IDX:1)
                                                   TO  WRK-NUM (IDX3:1)
                           END-IF
                       END-PERFORM    
                       MOVE    IDX3                TO  IDY     
                   ELSE
                       COMPUTE IDY     =   WRK-EDIDX   -
                                           WRK-STIDX   +   1
                       MOVE    WRK-ZENKAKU (WRK-STIDX:IDY) TO  WRK-NUM
                   END-IF     
               END-IF
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-NUM-X           TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF      SNUM-RC             =   ZERO
               MOVE    SNUM-OUTNUM         TO  WRK-NUM-9
           ELSE
               MOVE    ZERO                TO  WRK-NUM-9 
           END-IF
      *
           .
       800-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           MOVE    ZERO                     TO      FLG-TENSU
           MOVE    WRK-SRYCD                TO      TTNS-CHK-SRYCD
           MOVE    WK-TAIHI-SRYYMD          TO      TTNS-CHK-YMD
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯
           PERFORM 9001-TTNS-CHK-SEC
      *
      *
      *    ÂàÈò¥ï¡¼¥¯¤ËÂ¸ºß¤·¤Ê¤¤¾ì¹ç¤Ï£Ä£Â¤ò¸¡º÷
           IF    ( TTNS-FLG            =   ZERO )
      *
               INITIALIZE                           TNS-TENSU-REC
               MOVE    WRK-SRYCD            TO      TNS-SRYCD
               MOVE    SPA-HOSPNUM          TO      TNS-HOSPNUM
               MOVE    WK-TAIHI-SRYYMD      TO      TNS-YUKOSTYMD
                                                    TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
               MOVE    "tbl_tensu"          TO      MCP-TABLE
               MOVE    "key"                TO      MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC               =       ZERO
                   MOVE    "tbl_tensu"      TO      MCP-TABLE
                   MOVE    "key"            TO      MCP-PATHNAME
                   PERFORM  910-TENSU-READ-SEC
                   IF       FLG-TENSU       =       1
                       DISPLAY   "TENSU FETCH ERR"  
                   ELSE
      *                ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¤ËÊÔ½¸¤¹¤ë
                       PERFORM 9001-TTNS-HEN-SEC
                   END-IF
               ELSE
                   INITIALIZE       TNS-TENSU-REC
                   MOVE    1   TO   FLG-TENSU
                   DISPLAY   "TENSU SELECT ERR" 
               END-IF
      *
               MOVE    "tbl_tensu"          TO      MCP-TABLE
               MOVE    "key"                TO      MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
      *
           END-IF
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯ÊÔ½¸½èÍý
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF      TNS-TANIMOJISU    =   ZERO
           AND     TNS-TANINAME  NOT =   SPACE
               PERFORM   VARYING   IDW2   FROM   24   BY  -1
                          UNTIL    IDW2    <      1
                  IF   TNS-TANINAME(IDW2:1)  NOT =   SPACE
                      COMPUTE  TNS-TANIMOJISU   =   IDW2   /   2
                      MOVE    1     TO    IDW2
                  END-IF
               END-PERFORM
           END-IF
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ôÆÉ¹þ
      *****************************************************************
       910-TENSU-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
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
               IF        ACCT-SRYKBN        =    "95"   OR  "96"
                   GO  TO  900-SRYACCT-READ-SEC
               ELSE
                   MOVE    ZERO            TO  FLG-SRYACCT
               END-IF
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
      *    ¼ýÇ¼ÆÉ¹þ
      *****************************************************************
       900-SYUNOU-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
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
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1               TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                       TO  FLG-HKNCOMBI
               INITIALIZE                          HKNCOMBI-REC
           END-IF
      *
           .
       900-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
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
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ãæ´Ö¥Õ¥¡¥¤¥ë£Ò£Å£Á£Ä½èÍý
      *****************************************************************
       RECE200-READ-SEC            SECTION.
      *
           READ    RECE200-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE200
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE200 "  " PTNUM=" RECE200-PTNUM
                                       " (PTID=" RECE200-PTID ")"
      *
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       RECE200-READ-EXT.
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
      *        »ÜÀß´ð½à¾ðÊó
               MOVE  ZERO          TO  FLG-SET
               MOVE  RECE200-SRYYM TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"          TO  WRK-SYS-SRYYMD(7:2)
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
               MOVE      RECE200-SRYYM   TO  WRK-SRYYM-5000
               PERFORM   1001-SYS-5000-CHK-SEC
      *        ¿©»ö¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
               INITIALIZE             WRK-SHOKUJIRYOYO-OCC
      *
               MOVE  ZERO          TO  FLG-SET
               MOVE  RECE200-SRYYM TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"          TO  WRK-SYS-SRYYMD(7:2)
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS5000(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                  MOVE  TBL-SYS5000(SYSKAN-IDX)  TO  SYS-5000-REC
                  IF   ( SYS-5000-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                  AND  ( WRK-SYS-SRYYMD     <=  SYS-5000-EDYUKYMD )
                      MOVE     1       TO    FLG-SET
                      PERFORM  1002-SYSKAN-HENSYU-SEC
                      IF        WRK-SYS5000-COUNT    =     ZERO
                          CONTINUE
                      ELSE
                          PERFORM  1003-SYS-5000-GET-SEC
                      END-IF
                  END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                  STRING
              "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£¿ÇÎÅÇ¯·î¡á"
                                               DELIMITED  BY  SIZE 
                       RECE200-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1              TO  FLG-RECE200
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
      *    ¥Ç¡¼¥¿¹¹¿·½èÍý¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       RECE200-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE200-REC
      *
           ADD     1                   TO  CNT-OUT3
      *
           .
       RECE200-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE220-WRITE-SEC                SECTION.
      *
           WRITE   RECE220-REC
           ADD     1                   TO  CNT-OUT1
           .
       RECE220-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE230-WRITE-SEC                SECTION.
      *
           WRITE   RECE230-REC
           ADD     1                   TO  CNT-OUT2
           .
       RECE230-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE230-REWRITE-SEC              SECTION.                        
      *
              MOVE    ZERO                 TO  FLG-REWRITE
      *
              INITIALIZE                       RECE230-REC
              MOVE    RECE220-KEY1         TO  RECE230-KEY1
              MOVE    RECE220-ZAINUM       TO  RECE230-ZAINUM
              START   RECE230-FILE
                    KEY IS    >=   RECE230-KEY
                    INVALID
                        MOVE    1          TO  FLG-RECE230-ZAN
                    NOT INVALID
                        MOVE    ZERO       TO  FLG-RECE230-ZAN
                        READ    RECE230-FILE   NEXT
                          AT  END
                            MOVE    1      TO  FLG-RECE230-ZAN
                        END-READ
              END-START
              PERFORM   UNTIL
                  (RECE220-KEY1         NOT =  RECE230-KEY1  ) OR
                  (RECE220-ZAINUM       NOT =  RECE230-ZAINUM) OR
                  (FLG-RECE230-ZAN          =      1         ) OR
                  (FLG-REWRITE              =      1         )
                   IF    RECE230-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      MOVE    WRK-SRYSURYO3  TO  RECE230-SRYSURYO(5)
                      REWRITE   RECE230-REC
                   END-IF
                   READ    RECE230-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE230-ZAN
                   END-READ
              END-PERFORM
           .
       RECE230-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE230-REWRITE2-SEC              SECTION.
      *
              MOVE    ZERO                 TO  FLG-REWRITE
      *
              INITIALIZE                       RECE230-REC
              MOVE    RECE220-KEY1         TO  RECE230-KEY1
              MOVE    RECE220-ZAINUM       TO  RECE230-ZAINUM
              START   RECE230-FILE
                    KEY IS    >=   RECE230-KEY
                    INVALID
                        MOVE    1          TO  FLG-RECE230-ZAN
                    NOT INVALID
                        MOVE    ZERO       TO  FLG-RECE230-ZAN
                        READ    RECE230-FILE   NEXT
                          AT  END
                            MOVE    1      TO  FLG-RECE230-ZAN
                        END-READ
              END-START
              PERFORM   UNTIL
                  (RECE220-KEY1         NOT =  RECE230-KEY1  ) OR
                  (RECE220-ZAINUM       NOT =  RECE230-ZAINUM) OR
                  (FLG-RECE230-ZAN          =      1         ) OR
                  (FLG-REWRITE              =      1         )
                   IF    RECE230-RENNUM   =   WRK-RENNUM2
                      MOVE    1              TO  FLG-REWRITE
                      MOVE   "1"             TO  RECE230-RECEPTFLG(5)
                      REWRITE   RECE230-REC
                   END-IF
                   READ    RECE230-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE230-ZAN
                   END-READ
              END-PERFORM
      *
           .
       RECE230-REWRITE2-EXT.
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
      *    ¥¨¥é¡¼¥á¥Ã¥»¡¼¥¸ÊÔ½¸½èÍý
      *****************************************************************
       600-ERRMSG-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
      *
           MOVE    RECE200-PTNUM       TO  ERR-PTNUM
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
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE200-FILE
                   RECE220-FILE
                   RECE230-FILE
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
           DISPLAY "RECE200  CNT-IN  :" CNT-IN
           DISPLAY "RECE220  CNT-OUT :" CNT-OUT1
           DISPLAY "RECE230  CNT-OUT :" CNT-OUT2
           DISPLAY "RECE200  CNT-OUT :" CNT-OUT3
           DISPLAY "*** ORCR1220 END ***"
           .
       300-TERM-EXT.
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
      *
      *****************************************************************
      *    £Ä£Â¡¡£Ã£Ï£Í£Í£É£Ô½èÍý
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
      *
