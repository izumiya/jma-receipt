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
       PROGRAM-ID.             ORCR0870.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥ÈÆþ±¡¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ·î¼¡¶ÈÌ³¡¡Ï«ºÒ¼«ÇåÀÕÌÀºÙ½ñ¥Õ¥¡¥¤¥ëºîÀ®
      *                      ¥³¥á¥ó¥ÈÊÔ½¸¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/06/01    NACL-ÌçÏÆ¡¡    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    04/01/14  ¿ÇÎÅ¹Ô°Ù¤ÎÊñ³çÊ¬¤Ï½ü¤¯
      *                                     SRYACCT5¢ª31
      *  £²£°£°£´Ç¯£´·î²þÀµÂÐ±þ
      *  01.01.01    NACL-ÌçÏÆ    04.04.01  ¥ê¥Ï¸À¸ìÄ°³ÐÎÅË¡¡Ê£³¡ËÄÉ²ÃÂÐ±þ
      *  01.01.02    NACL-ÌçÏÆ    04.04.05  ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤Î
      *                                     ¥³¥á¥ó¥ÈÂÐ±þ
      *  01.01.03    NACL-ÌçÏÆ    04.04.15  Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥È
      *                                     ÂÐ±þ
      *  01.01.04    NACL-ÌçÏÆ    04.06.07  ¥³¥Ô¡¼¶ç£°£¸£µÊÑ¹¹¤ËÈ¼¤¦½¤Àµ
      *  Ãæ»ß½èÍýÄÉ²Ã
      *  01.02.01    NACL-Æ£¸¶    04/10/13  Ãæ»ß½èÍý¤ÎÄÉ²Ã
      *  01.02.02    NACL-ÌçÏÆ    04/12/09  Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»ÆüÂÐ±þ
      *  01.02.03    NACL-ÌçÏÆ    05/01/18  »ÀÁÇ·×»»¼°¥³¥á¥ó¥ÈÂÐ±þ
      *  01.02.04    NACL-ÌçÏÆ    05/05/10  Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÉÂÌ¾µ­ºÜÂÐ±þ
      *  01.02.05    NACL-ÌçÏÆ    05/08/22  ¥ê¥Ï¥Ó¥ê¼Â»Ü²ó¿ôº¹¤·¹þ¤ß½¤ÀµÂÐ±þ
      *  01.02.06    NACL-ÌçÏÆ    05/08/31  ¼«ÇåÀÕ½¾ÍèÍÍ¼°ÂÐ±þ
      *  01.02.07    NACL-ÌçÏÆ    05/11/01  ¡Ê¸¡Áª¡ËÅù¥³¥á¥ó¥ÈÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *
           SELECT  RECE84-FILE     ASSIGN  RECE84PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE84-KEY
                                   FILE    STATUS  IS  STS-RECE84.
      *
           SELECT  RECE85-FILE     ASSIGN  RECE85PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE85-KEY
                                   FILE    STATUS  IS  STS-RECE85.
      *
           SELECT  RECE841-FILE    ASSIGN  RECE841PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE841-KEY
                                   FILE    STATUS  IS  STS-RECE841.
           SELECT  RECE8441-FILE   ASSIGN  RECE841PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8441-KEY
                                   FILE    STATUS  IS  STS-RECE841.
      *
           SELECT  RECE851-FILE    ASSIGN  RECE851PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE851-KEY
                                   FILE    STATUS  IS  STS-RECE851.
           SELECT  RECE8551-FILE   ASSIGN  RECE851PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8551-KEY
                                   FILE    STATUS  IS  STS-RECE851.
      *¥ï¡¼¥¯
           SELECT  RECE849-FILE    ASSIGN  RECE849PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE849-KEY
                                   FILE    STATUS  IS  STS-RECE849.
           SELECT  RECE8449-FILE   ASSIGN  RECE849PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8449-KEY
                                   FILE    STATUS  IS  STS-RECE849.
      *
           SELECT  RECE859-FILE    ASSIGN  RECE859PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE859-KEY
                                   FILE    STATUS  IS  STS-RECE859.
           SELECT  RECE8559-FILE   ASSIGN  RECE859PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8559-KEY
                                   FILE    STATUS  IS  STS-RECE859.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC.
           COPY    "CPRCF082.INC".
      *
       FD  RECE84-FILE.
       01  RECE84-REC.
           COPY    "CPRCF084.INC".
      *
       FD  RECE85-FILE.
       01  RECE85-REC.
           COPY    "CPRCF085.INC".
      *
       FD  RECE841-FILE.
       01  RECE841-REC. 
           COPY    "CPRCF084.INC"  REPLACING   //RECE84//
                                   BY          //RECE841//.
      *
       FD  RECE851-FILE.
       01  RECE851-REC. 
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //RECE851//.
      *
       FD  RECE8441-FILE.
       01  RECE8441-REC. 
           COPY    "CPRCF084.INC"  REPLACING   //RECE84//
                                   BY          //RECE8441//.
      *
       FD  RECE8551-FILE.
       01  RECE8551-REC. 
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //RECE8551//.
      *¥ï¡¼¥¯
       FD  RECE849-FILE.
       01  RECE849-REC. 
           COPY    "CPRCF084.INC"  REPLACING   //RECE84//
                                   BY          //RECE849//.
      *
       FD  RECE859-FILE.
       01  RECE859-REC. 
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //RECE859//.
      *
       FD  RECE8449-FILE.
       01  RECE8449-REC. 
           COPY    "CPRCF084.INC"  REPLACING   //RECE84//
                                   BY          //RECE8449//.
      *
       FD  RECE8559-FILE.
       01  RECE8559-REC. 
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //RECE8559//.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     ¥ì¥»¥×¥ÈÌÀºÙ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE82//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE84//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE85//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE841//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE851//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE849//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE859//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE82          PIC X(02).
           03  STS-RECE84          PIC X(02).
           03  STS-RECE841         PIC X(02).
           03  STS-RECE85          PIC X(02).
           03  STS-RECE851         PIC X(02).
           03  STS-RECE849         PIC X(02).
           03  STS-RECE859         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE84          PIC 9(01).
           03  FLG-RECE849         PIC 9(01).
           03  FLG-RECE85          PIC 9(01).
           03  FLG-RECE859         PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
      *
           03  FLG-YAKUKAN-COMMENT PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-WRKRECE85       PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-NYU-RENZOKU     PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-KYUSEI-OK       PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-RECE82          PIC 9(06).
           03  CNT-RECE84          PIC 9(06).
           03  CNT-RECE85          PIC 9(06).
           03  CNT-RECE849         PIC 9(06).
           03  CNT-RECE859         PIC 9(06).
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
           03  IDX12               PIC 9(04).
           03  IDX13               PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  DAY-IDX3            PIC 9(01).
           03  IDX-RECE85          PIC 9(04).
           03  IDV                 PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-RECEKBN            PIC  X(01).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
      *
           03  WRK-JYUNBAN         PIC 9(04).
           03  WRK-GYOSUU          PIC 9(04).
           03  WRK-GYOSUU2         PIC 9(04).
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-CNT-MEISAI      PIC 9(04).
           03  WRK-KETA            PIC 9(04).
           03  WRK-KETA-TAIHI      PIC 9(04).
           03  WRK-KETA2           PIC 9(04).
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
           03  WRK-HENSYU-KAISU    PIC X(02).
           03  WRK-HENSYU-KAISU2   PIC X(04).
      *
           03  WRK-HENSYU-KASANRITU   PIC X(01).
           03  WRK-HENSYU-KASANRITU2  PIC X(02).
      *
           03  WRK-TUKI-YAKUKAN    PIC 9(02).
           03  WRK-DAY-YAKUKAN     PIC 9(02).
      *
           03  TBL-SANTEI-DAY.
               05  TB-SANTEI-DAY   PIC 9(02)   OCCURS  31.
      *
           03  TBL-NYU-DAY.
               05  TBL-NYU-DAY-OCC             OCCURS  31.
                 07  TBL-NYU-DAY-ST  PIC 9(02).
                 07  TBL-NYU-DAY-ED  PIC 9(02).
                 07  TBL-NYU-DAY-CD  PIC X(09).
      *
           03  WRK-ZENYYMM           PIC 9(06).
      *
           03  WRK-KEISAN            PIC 9(09).
           03  WRK-KEISAN2           PIC 9(03).
           03  WRK-KEISAN3           PIC 9(09)V9(03).
           03  WRK-KEISAN-TAIHI      PIC X(09).
      *
           03  WRK-KYUSEI-NYUGAIKBN  PIC X(01).
           03  WRK-KYUSEI-SRYKA      PIC X(02).
           03  WRK-KYUSEI-ZAINUM     PIC 9(08).
      *
       01  WRK-RECE84-REC. 
           COPY    "CPRCF084.INC"  REPLACING   //RECE84//
                                   BY          //WRK-RECE84//.
      *
       01  WRK-RECE85-REC. 
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //WRK-RECE85//.
      *
       01  WRK-RECE85XX-REC.
         02 WRK-RECE85XX-TBL       OCCURS 50.
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //WRK-RECE85XX//.
      *
       01  WRK-FREE-AREA.
           03  WRK-FREEINF                     OCCURS 49.         
               05  WK-FREE-YUKOKETA          PIC  9(02).
               05  WK-FREE-NAME              PIC  X(200).
      *
       01  WRK-FREE-AREA2.                                              
           03  WRK-FREEINF2                    OCCURS 50.               
               05  WK-FREE-YUKOKETA2         PIC  9(02).                
               05  WK-FREE-NAME2             PIC  X(200).               
      *
       01  WRK-FREE-AREA3.
           03  WRK-FREEINF3.
               05  WK-FREE-NAME3             PIC  X(1000).
               05  WK-FREE-NAME3-TAIHI       PIC  X(1000).
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
           03  CONS-YAKUKAN-COMMENT  PIC X(09) VALUE "120001610".
      *    »»ÄêÆü¥³¥á¥ó¥È
           03  CONS-SANTEI-COMMENT1  PIC X(09) VALUE "840000058".
           03  CONS-SANTEI-COMMENT2  PIC X(09) VALUE "840000065".
      *    ºßÂð·ì±ÕÆ©ÀÏ»ØÆ³´ÉÍýÎÁ
           03  CONS-TOSEKI           PIC X(09) VALUE "114009310".
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü
           03  CONS-KYUSEI           PIC X(09) VALUE "099800101".
      *
      *    »ÀÁÇ¡Ê¶â³Û¡¦¿ôÎÌ¡Ë¾ðÊó
       01  TBL-SANSO-INF.
           03  TBL-SANSO-INF2       OCCURS    10.
               05  TBL-SANSO-TEN          PIC 9(09)V9(02).
               05  TBL-SANSO-SRYSURYO     PIC 9(05)V9(03).
           03  TBL-SANSO-INF3.
               05  TBL-KASAN-SRYSURYO     PIC 9(05)V9(03).
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
      *    ´µ¼Ô¥³¥á¥ó¥È
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
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
      *    Á´³Ñ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSKANACHK.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
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
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END  =  1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
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
      *
           MOVE    COMMAND-PARAM   TO  WRK-PARA
      *
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
      *
           PERFORM 110-FILE-OPEN-SEC
           PERFORM 120-DBOPEN-SEC
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCR0870"      TO  JOB-PGID
           MOVE    "¥³¥á¥ó¥ÈÊÔ½¸¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë"
                                   TO  JOB-SHELLMSG   
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
      *    ½èÍýÃæ»ßÀßÄê½èÍý
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           PERFORM 990-RECE82-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
      *
           MOVE    "RECE84W"           TO  RECE84PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84PARA-TERMID
      *
           MOVE    "RECE850"           TO  RECE85PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE85PARA-TERMID
      *
           MOVE    "RECE84A"           TO  RECE841PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE841PARA-TERMID
      *
           MOVE    "RECE85A"           TO  RECE851PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE851PARA-TERMID
      *
           MOVE    "RECE84Y"           TO  RECE849PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE849PARA-TERMID
      *
           MOVE    "RECE85Y"           TO  RECE859PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE859PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    INPUT               RECE82-FILE
      *
           OPEN    INPUT               RECE84-FILE
      *
           OPEN    INPUT               RECE85-FILE
      *
           OPEN    OUTPUT              RECE8441-FILE
           CLOSE                       RECE8441-FILE
      *
           OPEN    OUTPUT              RECE8551-FILE
           CLOSE                       RECE8551-FILE
      *
           OPEN    OUTPUT              RECE8449-FILE
           CLOSE                       RECE8449-FILE
      *
           OPEN    OUTPUT              RECE8559-FILE
           CLOSE                       RECE8559-FILE
      *
           OPEN    OUTPUT              RECE841-FILE
      *
           OPEN    OUTPUT              RECE851-FILE
      *
           OPEN    I-O                 RECE849-FILE
      *
           OPEN    I-O                 RECE859-FILE
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
      **** IF   RECE82-COMCNTFLG   =   "1"
      ****   INITIALIZE                      RECE84-REC
      ****   MOVE    RECE82-HOSPID       TO  RECE84-HOSPID
      ****   MOVE    RECE82-SRYYM        TO  RECE84-SRYYM
      ****   MOVE    RECE82-NYUGAIKBN    TO  RECE84-NYUGAIKBN
      ****   MOVE    RECE82-PTID         TO  RECE84-PTID
      ****   MOVE    RECE82-RECEKA       TO  RECE84-RECEKA
      ****   MOVE    RECE82-HKNID        TO  RECE84-HKNID
      ****   MOVE    RECE82-TEKSTYMD     TO  RECE84-TEKSTYMD
      ****   MOVE    ZERO                TO  RECE84-ZAINUM
      *
      ****   START   RECE84-FILE
      ****       KEY IS    >=   RECE84-KEY
      ****       INVALID
      ****           MOVE    1           TO  FLG-RECE84
      ****       NOT INVALID
      ****           PERFORM 990-RECE84-NEXT-SEC
      ****   END-START
      ****   PERFORM
      ****     UNTIL  ( FLG-RECE84           =   1                   )
      ****     OR     ( RECE82-HOSPID    NOT =   RECE84-HOSPID       )
      ****     OR     ( RECE82-SRYYM     NOT =   RECE84-SRYYM        )
      ****     OR     ( RECE82-NYUGAIKBN NOT =   RECE84-NYUGAIKBN    )
      ****     OR     ( RECE82-PTID      NOT =   RECE84-PTID         )
      ****     OR     ( RECE82-RECEKA    NOT =   RECE84-RECEKA       )
      ****     OR     ( RECE82-HKNID     NOT =   RECE84-HKNID        )
      ****     OR     ( RECE82-TEKSTYMD  NOT =   RECE84-TEKSTYMD     )
      *
      ****     IF   RECE82-HKNKBN    NOT  =  "4"                        
      ****        EVALUATE      RECE84-SRYKBN
      *+++
      *+++
      ****        END-EVALUATE
      ****     ELSE                                                     
      ****        EVALUATE      RECE84-SRYKBN
      *+++
      *+++
      ****        END-EVALUATE                                          
      ****     END-IF                                                   
      ****     PERFORM 990-RECE84-NEXT-SEC
      ****   END-PERFORM
      **** END-IF
      *
      *    Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ¤ß¤Î¥³¥á¥ó¥È
      *
           IF   RECE82-NYUINRYOFLG   NOT =   SPACE
      *
              PERFORM   2001-NYUINRYO-COMMENT-SEC
      *
           END-IF
      *
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥È
           PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
      *
      *    ¼«ÇåÀÕ½¾ÍèÍÍ¼°¡§²Ã»»Î¨ 
           IF   RECE82-HKNKBN     =   "4"
           AND  WRK-PARA-RECEKBN  =   "5"
                PERFORM    2001-KASANRITU-COMMENT-SEC
           END-IF
      *
           INITIALIZE                      RECE84-REC
           MOVE    RECE82-HOSPID       TO  RECE84-HOSPID
           MOVE    RECE82-SRYYM        TO  RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE84-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE84-PTID
           MOVE    RECE82-RECEKA       TO  RECE84-RECEKA
           MOVE    RECE82-HKNID        TO  RECE84-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE84-TEKSTYMD
           MOVE    ZERO                TO  RECE84-ZAINUM
      *
           START   RECE84-FILE
               KEY IS    >=   RECE84-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE84
               NOT INVALID
                   PERFORM 990-RECE84-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE84           =   1                   )
               OR     ( RECE82-HOSPID    NOT =   RECE84-HOSPID       )
               OR     ( RECE82-SRYYM     NOT =   RECE84-SRYYM        )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE84-NYUGAIKBN    )
               OR     ( RECE82-PTID      NOT =   RECE84-PTID         )
               OR     ( RECE82-RECEKA    NOT =   RECE84-RECEKA       )
               OR     ( RECE82-HKNID     NOT =   RECE84-HKNID        )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE84-TEKSTYMD     )
      *
               IF   RECE82-HKNKBN    NOT  =  "4"                        
                 EVALUATE      RECE84-SRYKBN                   
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
      *
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE84-WRITE-SEC
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
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "50"
      *            ¡Ê¼ê½ÑÆü¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "60"
      *            ¡Ê¡Ê¸¡Áª¡ËÆü¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-KENSEN-HANTEI-SEC
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
                        IF  RECE84-RIHABIRI-COMMENT     =  "1"
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                        END-IF
                      END-IF
      *            ¡Ê¡Ê¥ê¥ÏÁª¡ËÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2001-RIHASEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "90"                                       
                   WHEN      "92"
                      EVALUATE    RECE84-ZAISKBKBN
                        WHEN   "2"
                          PERFORM   2001-NYU-COMMENT-SEC
                        WHEN   "6"
                          MOVE   ZERO   TO  FLG-COMMENT
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2001-KAKURI-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2001-CHIRYO-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2099-RECE84-WRITE-SEC
                          END-IF
                        WHEN   OTHER
                         PERFORM    2099-RECE84-WRITE-SEC
                      END-EVALUATE
                   WHEN   OTHER
                         PERFORM    2099-RECE84-WRITE-SEC
                 END-EVALUATE
               ELSE                                                     
                 EVALUATE      RECE84-SRYKBN                            
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
      *
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "40"          
                      MOVE   ZERO   TO  FLG-COMMENT
      *            ¡ÊÀº¿À²ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUMONSIDO-SEISIN-HANTEI-SEC
                      END-IF
      *            ¡Ê¥ê¥Ï¥Ó¥ê¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        IF  RECE84-RIHABIRI-COMMENT     =  "1"
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                        END-IF
                      END-IF
      *            ¡Ê¡Ê¥ê¥ÏÁª¡ËÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2001-RIHASEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "50"                                       
      *            ¡Ê¼ê½ÑÆü¤Î¥»¥Ã¥È¡Ë                                   
                      PERFORM    2001-SHUJUTSU-HANTEI-SEC               
                   WHEN      "60"
      *            ¡Ê¡Ê¸¡Áª¡ËÆü¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-KENSEN-HANTEI-SEC
                   WHEN      "70"
      *            ¡Ê£Ã£Ô¡¦£Í£Ò£É¼Â»ÜÆü¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-CTMRI-HANTEI-SEC
                   WHEN      "80"
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
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "90"                                       
                   WHEN      "92"
                      EVALUATE    RECE84-ZAISKBKBN
                        WHEN   "2"
                          PERFORM   2001-NYU-COMMENT-SEC
                        WHEN   "6"
                          MOVE   ZERO   TO  FLG-COMMENT
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2001-KAKURI-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2001-CHIRYO-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2099-RECE84-WRITE-SEC
                          END-IF
                        WHEN   OTHER
                          PERFORM   2099-RECE84-WRITE-SEC
                      END-EVALUATE
                   WHEN   OTHER                                         
                         PERFORM    2099-RECE84-WRITE-SEC
                 END-EVALUATE                                           
               END-IF                                                   
      *
               PERFORM 990-RECE84-NEXT-SEC
      *
           END-PERFORM
      *
      *****º¹¹þÊÔ½¸£²********************
           PERFORM     250-SASHIKOMI2-SEC
      ***********************************
      *
           PERFORM 990-RECE82-READ-SEC
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
           INITIALIZE                      RECE849-REC
           MOVE    RECE82-HOSPID       TO  RECE849-HOSPID
           MOVE    RECE82-SRYYM        TO  RECE849-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE849-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE849-PTID
           MOVE    RECE82-RECEKA       TO  RECE849-RECEKA
           MOVE    RECE82-HKNID        TO  RECE849-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE849-TEKSTYMD
           MOVE    99999999            TO  RECE849-ZAINUM
           MOVE    "90"                TO  RECE849-SRYKBN
      *
           MOVE    1                   TO  RECE849-MEISAISU
      *
           WRITE   RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           INITIALIZE                      RECE859-REC
           MOVE    RECE82-HOSPID       TO  RECE859-HOSPID
           MOVE    RECE82-SRYYM        TO  RECE859-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE859-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE859-PTID
           MOVE    RECE82-RECEKA       TO  RECE859-RECEKA
           MOVE    RECE82-HKNID        TO  RECE859-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE859-TEKSTYMD
           MOVE    99999999            TO  RECE859-ZAINUM
           MOVE    "90"                TO  RECE859-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE85XX-REC
           INITIALIZE                      WRK-RECE85XX-REC
           MOVE    "810000001"         TO  WRK-RECE85XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE85XX-SRYSURYO(1 1)
           EVALUATE   RECE82-NYUINRYOFLG
             WHEN     "1"
               MOVE     10             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "¼«Èñ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "2"
               MOVE     10             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "Ï«ºÒ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "3"
               MOVE     11             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "¼«ÇåÀÕ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "4"
               MOVE     10             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "¹ñÊÝ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "5"
               MOVE     10             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "¼ÒÊÝ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE85XX-NAME(1 1)
           END-EVALUATE
           MOVE     "1"                TO  WRK-RECE85XX-SIKFLG(1 1)
      *
           MOVE    WRK-RECE85XX-SRYACT-TBL(1 1)
                                       TO  RECE859-SRYACT-TBL(1)
           WRITE   RECE859-REC
           ADD     1                   TO  CNT-RECE859
      *
           .
       2001-NYUINRYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î£±£¸£°Æü°ÊÆâ¤«È½Äê
      *****************************************************************
       2001-KYUSEIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     WRK-KYUSEIYMD
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                  SANTEI-REC
           MOVE    RECE82-HOSPID       TO  SANTEI-HOSPID
           MOVE    RECE82-PTID         TO  SANTEI-PTID
           MOVE    CONS-KYUSEI         TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE82-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
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
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE82-SRYYM
                         AND  RECE82-SRYYM        <=  WRK-AFTER180(1:6)
                               MOVE    1    TO   FLG-KYUSEI-COMMENT
                               MOVE  WRK-FSANTEIYMD
                                            TO   WRK-KYUSEIYMD
                         END-IF
      *
                     END-IF
      *
                   END-PERFORM
      *
                   MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
                   PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF      FLG-KYUSEI-COMMENT    =      1
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               PERFORM    20011-KYUSEIHASSYO-COMMENT-SEC
           END-IF
      *
           .
       2001-KYUSEIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¸¡º÷
      *****************************************************************
       20011-KYUSEIHASSYO-KENSAKU-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-KYUSEI-OK
           MOVE    SPACE                 TO  WRK-KYUSEI-NYUGAIKBN
           MOVE    SPACE                 TO  WRK-KYUSEI-SRYKA
           MOVE    ZERO                  TO  WRK-KYUSEI-ZAINUM
      *
           MOVE    ZERO                  TO  IDZ
           INITIALIZE                        WRK-FREE-AREA
      *
           INITIALIZE                        SRYACT-REC
           MOVE    RECE82-HOSPID         TO  SRY-HOSPID
           MOVE    RECE82-PTID           TO  SRY-PTID
           MOVE    WRK-KYUSEIYMD(1:6)    TO  SRY-SRYYM
           MOVE    CONS-KYUSEI           TO  SRY-SRYCD (1)
           MOVE    CONS-KYUSEI           TO  SRY-SRYCD (2)
           MOVE    CONS-KYUSEI           TO  SRY-SRYCD (3)
           MOVE    CONS-KYUSEI           TO  SRY-SRYCD (4)
           MOVE    CONS-KYUSEI           TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "SRYACT-KEY12"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY12"    TO  ORC-DBPATH
               PERFORM  900-SRYACT-READ-NEXT-SEC
               PERFORM  UNTIL   FLG-SRYACT     =   1
                         OR     FLG-KYUSEI-OK  =   1
                 INITIALIZE                     SRYACCT-REC
                 MOVE    SRY-HOSPID       TO    ACCT-HOSPID
                 MOVE    SRY-NYUGAIKBN    TO    ACCT-NYUGAIKBN
                 MOVE    SRY-PTID         TO    ACCT-PTID
                 MOVE    SRY-SRYKA        TO    ACCT-SRYKA
                 MOVE    SRY-SRYYM        TO    ACCT-SRYYM
                 MOVE    SRY-ZAINUM       TO    ACCT-ZAINUM
                 MOVE    SRYACCT-REC      TO    MCPDATA-REC
                 MOVE    "DBSELECT"       TO    MCP-FUNC
                 MOVE    "SRYACCT-KEY31"  TO    ORC-DBPATH
                 CALL    "ORCMCPSUB"      USING
                                          MCPAREA
                                          ORCMCPAREA
                                          MCPDATA-REC
                 IF     MCP-RC            =  ZERO
                    MOVE    "DBFETCH"        TO   MCP-FUNC
                    MOVE    "SRYACCT-KEY31"  TO   ORC-DBPATH
                    CALL    "ORCMCPSUB"           USING
                                                  MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
                    IF      MCP-RC        =       ZERO
                        MOVE    MCPDATA-REC  TO  SRYACCT-REC
      *
                        MOVE    WRK-KYUSEIYMD(7:2)  TO  DAY-IDX2
                        IF  ( ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO )
                        AND ( ACCT-HKNCOMBI  =  RECE82-HKNCOMBI )
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
                 INITIALIZE                     MCPAREA
                 MOVE    "DBCLOSECURSOR"   TO   MCP-FUNC
                 MOVE    "SRYACCT-KEY31"   TO   ORC-DBPATH
                 CALL    "ORCMCPSUB"       USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
                 MOVE     "SRYACT-KEY12"   TO   ORC-DBPATH
                 PERFORM   900-SRYACT-READ-NEXT-SEC
               END-PERFORM
           END-IF
           INITIALIZE                        MCPAREA
           MOVE    "DBCLOSECURSOR"   TO      MCP-FUNC
           MOVE    "SRYACT-KEY12"    TO      ORC-DBPATH
           CALL    "ORCMCPSUB"       USING
                                     MCPAREA
                                     ORCMCPAREA
                                     MCPDATA-REC
      ***
           IF      WRK-KYUSEI-ZAINUM    NOT =    ZERO
              INITIALIZE                           SRYACT-REC
              MOVE    RECE82-HOSPID          TO    SRY-HOSPID
              MOVE    WRK-KYUSEI-NYUGAIKBN   TO    SRY-NYUGAIKBN
              MOVE    RECE82-PTID            TO    SRY-PTID
              MOVE    WRK-KYUSEI-SRYKA       TO    SRY-SRYKA
              MOVE    WRK-KYUSEIYMD(1:6)     TO    SRY-SRYYM
              MOVE    WRK-KYUSEI-ZAINUM      TO    SRY-ZAINUM
              MOVE    SRYACT-REC             TO    MCPDATA-REC
              MOVE    "DBSELECT"             TO    MCP-FUNC
              MOVE    "SRYACT-KEY2"          TO    ORC-DBPATH
              CALL    "ORCMCPSUB"            USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
              IF     MCP-RC            =  ZERO
                 MOVE    "SRYACT-KEY2"       TO    ORC-DBPATH
                 PERFORM  900-SRYACT-READ-NEXT-SEC
                 PERFORM  UNTIL   FLG-SRYACT     =   1
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12             >    5   )  OR
                                    (SRY-SRYCD(IDX12)  =   SPACE)
                      IF   (SRY-SRYCD(IDX12)(1:1)     =   "8"  )  OR
                           (SRY-SRYCD(IDX12)(1:3)     =   "008")
                       IF   SRY-INPUTNUM(IDX12)   NOT =   ZERO
                         INITIALIZE                      PTCOM-REC
                         MOVE    SRY-HOSPID          TO  PTCOM-HOSPID
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
                       END-IF
                      END-IF
                   END-PERFORM
                   MOVE     "SRYACT-KEY2"    TO    ORC-DBPATH
                   PERFORM   900-SRYACT-READ-NEXT-SEC
                 END-PERFORM
              END-IF
              INITIALIZE                     MCPAREA
              MOVE    "DBCLOSECURSOR"   TO   MCP-FUNC
              MOVE    "SRYACT-KEY2"     TO   ORC-DBPATH
              CALL    "ORCMCPSUB"       USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           MOVE    ZERO         TO  IDX-RECE85
           MOVE    SPACE        TO  WRK-RECE85XX-REC
           INITIALIZE               WRK-RECE85XX-REC
      *
           ADD     1            TO  IDX-RECE85
           MOVE    "810000001"  TO  WRK-RECE85XX-SRYCD(IDX-RECE85 1)
           MOVE    ZERO         TO  WRK-RECE85XX-SRYSURYO(IDX-RECE85 1)
           MOVE    9            TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
           MOVE    "È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü"
                                TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
           MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                              TO  WRK-RECE85XX-NAME(IDX-RECE85 1)(7:4)
           MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                              TO  WRK-RECE85XX-NAME(IDX-RECE85 1)(13:4)
           MOVE    "1"        TO  WRK-RECE85XX-SIKFLG(IDX-RECE85 1)
      *
           PERFORM   VARYING   IDX12   FROM    1   BY  1
                      UNTIL  ( IDX12               >   49    )
                        OR   ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1        TO  IDX-RECE85
                MOVE   "810000001"
                                TO  WRK-RECE85XX-SRYCD(IDX-RECE85 1)
                MOVE   ZERO     TO  WRK-RECE85XX-SRYSURYO(IDX-RECE85 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                                TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                MOVE   WK-FREE-NAME(IDX12)
                                TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                MOVE   "1"      TO  WRK-RECE85XX-SIKFLG(IDX-RECE85 1)
           END-PERFORM
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE85
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE85
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE85  =    1
                INITIALIZE                      RECE859-REC
                MOVE    RECE82-HOSPID       TO  RECE859-HOSPID
                MOVE    RECE82-SRYYM        TO  RECE859-SRYYM
                MOVE    RECE82-NYUGAIKBN    TO  RECE859-NYUGAIKBN
                MOVE    RECE82-PTID         TO  RECE859-PTID
                MOVE    RECE82-RECEKA       TO  RECE859-RECEKA
                MOVE    RECE82-HKNID        TO  RECE859-HKNID
                MOVE    RECE82-TEKSTYMD     TO  RECE859-TEKSTYMD
                MOVE    99999996            TO  RECE859-ZAINUM
                MOVE    "99"                TO  RECE859-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE859-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE85   =    1)
                   ADD         1            TO  IDX-RECE85
                   IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = SPACE
                       MOVE    1            TO  FLG-WRKRECE85
                   ELSE
                       MOVE  WRK-RECE85XX-SRYACT-TBL (IDX-RECE85 1)
                                        TO   RECE859-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE859-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE859-REC
                   ADD     1            TO   CNT-RECE859
                END-IF
           END-PERFORM
      *
      *
           INITIALIZE                      RECE849-REC
           MOVE    RECE82-HOSPID       TO  RECE849-HOSPID
           MOVE    RECE82-SRYYM        TO  RECE849-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE849-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE849-PTID
           MOVE    RECE82-RECEKA       TO  RECE849-RECEKA
           MOVE    RECE82-HKNID        TO  RECE849-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE849-TEKSTYMD
           MOVE    99999996            TO  RECE849-ZAINUM
           MOVE    "99"                TO  RECE849-SRYKBN
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE849-MEISAISU
      *
           WRITE   RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           .
       20011-KYUSEIHASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕ½¾ÍèÍÍ¼°¡§²Ã»»Î¨ ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-KASANRITU-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE849-REC
           MOVE    RECE82-HOSPID       TO  RECE849-HOSPID
           MOVE    RECE82-SRYYM        TO  RECE849-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE849-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE849-PTID
           MOVE    RECE82-RECEKA       TO  RECE849-RECEKA
           MOVE    RECE82-HKNID        TO  RECE849-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE849-TEKSTYMD
           MOVE    99999998            TO  RECE849-ZAINUM
           MOVE    "99"                TO  RECE849-SRYKBN
           MOVE    1                   TO  RECE849-MEISAISU
      *
           WRITE   RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           INITIALIZE                      RECE859-REC
           MOVE    RECE82-HOSPID       TO  RECE859-HOSPID
           MOVE    RECE82-SRYYM        TO  RECE859-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE859-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE859-PTID
           MOVE    RECE82-RECEKA       TO  RECE859-RECEKA
           MOVE    RECE82-HKNID        TO  RECE859-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE859-TEKSTYMD
           MOVE    99999998            TO  RECE859-ZAINUM
           MOVE    "99"                TO  RECE859-SRYKBN
      *    ²Ã»»Î¨¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE85XX-REC
           INITIALIZE                      WRK-RECE85XX-REC
           MOVE   "810000001"          TO  WRK-RECE85XX-SRYCD(1 1)
           MOVE    ZERO                TO  WRK-RECE85XX-SRYSURYO(1 1)
           MOVE    10                  TO  WRK-RECE85XX-YUKOKETA(1 1)
           MOVE   "²Ã»»Î¨¡Ê¡¡¡¥¡¡¡¡¡Ë¡ó"
                                       TO  WRK-RECE85XX-NAME(1 1)
      *
           MOVE    RECE82-KSNRATE(1:1) TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE85XX-NAME(1 1)(9:2)
           MOVE    RECE82-KSNRATE(2:1) TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE85XX-NAME(1 1)(13:2)
           MOVE    RECE82-KSNRATE(3:1) TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE85XX-NAME(1 1)(15:2)
      *
           MOVE     "1"                TO  WRK-RECE85XX-SIKFLG(1 1)
           MOVE    WRK-RECE85XX-SRYACT-TBL(1 1)
                                       TO  RECE859-SRYACT-TBL(1)
           WRITE   RECE859-REC
           ADD     1                   TO  CNT-RECE859
      *
           .
       2001-KASANRITU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²Ã»»Î¨ÊÔ½¸
      *****************************************************************
       20011-KASANRITU-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-HENSYU-KASANRITU
           MOVE    SPACE                TO  WRK-HENSYU-KASANRITU2
      *
           MOVE    DAY-IDX3             TO  WRK-HENSYU-KASANRITU
      *
           INITIALIZE                       ORCSKANACHKAREA
           MOVE    "2"                  TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KASANRITU TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:2)
                                        TO  WRK-HENSYU-KASANRITU2
           END-IF
      *
           .
       20011-KASANRITU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-YAKUKANSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-YAKUKAN-COMMENT   =    "1"
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
           IF      RECE84-FLG-NYUEIYO-COMMENT   =    "1"
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
           IF      RECE84-FLG-HOUMON-COMMENT   =    "1"
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
           IF      RECE84-FLG-SYOBYO-COMMENT   =    "1"
               MOVE    4                 TO  FLG-COMMENT
               PERFORM 20011-SHUJUTSU-ETC2-COMMENT-SEC
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
           IF      RECE84-FLG-RYOYO-COMMENT    =    "1"
               MOVE    4                 TO     FLG-COMMENT
               PERFORM 20011-SHUJUTSU-ETC2-COMMENT-SEC
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
           IF      RECE84-FLG-TAIIN-COMMENT   =    "1"
      *
             MOVE      ZERO            TO      FLG-YAKUKAN-COMMENT
      *
             INITIALIZE                        SRYACT-REC
             MOVE    RECE84-HOSPID     TO      SRY-HOSPID
             MOVE    RECE84-NYUGAIKBN  TO      SRY-NYUGAIKBN
             MOVE    RECE84-PTID       TO      SRY-PTID
             MOVE    RECE84-SRYYM      TO      SRY-SRYYM
             MOVE    CONS-YAKUKAN-COMMENT  TO  SRY-SRYCD (1)
             MOVE    CONS-YAKUKAN-COMMENT  TO  SRY-SRYCD (2)
             MOVE    CONS-YAKUKAN-COMMENT  TO  SRY-SRYCD (3)
             MOVE    CONS-YAKUKAN-COMMENT  TO  SRY-SRYCD (4)
             MOVE    CONS-YAKUKAN-COMMENT  TO  SRY-SRYCD (5)
      *
             MOVE    SRYACT-REC        TO      MCPDATA-REC
             MOVE    "DBSELECT"        TO      MCP-FUNC
             MOVE    "SRYACT-KEY7"     TO      ORC-DBPATH
             CALL    "ORCMCPSUB"       USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
             IF      MCP-RC               =       ZERO
                 MOVE    "SRYACT-KEY7"   TO      ORC-DBPATH
                 PERFORM  900-SRYACT-READ-NEXT-SEC
                 IF      FLG-SRYACT  =  ZERO
                     MOVE    1         TO      FLG-YAKUKAN-COMMENT
                 END-IF
             END-IF
      *
             MOVE    "DBCLOSECURSOR"   TO      MCP-FUNC
             MOVE    "SRYACT-KEY7"     TO      ORC-DBPATH
             CALL    "ORCMCPSUB"       USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
             IF      FLG-YAKUKAN-COMMENT   =   ZERO
      *
               MOVE      ZERO            TO      WRK-ZENYYMM
      *
               MOVE      ZERO            TO      FLG-YAKUKAN-COMMENT
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    RECE84-SRYYM        TO  LNK-DAY6-KIJUN(1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN(7:2)
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE     -1                 TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-ZENYYMM
      *
               INITIALIZE                        SRYACT-REC
               MOVE    RECE84-HOSPID     TO      SRY-HOSPID
               MOVE    RECE84-NYUGAIKBN  TO      SRY-NYUGAIKBN
               MOVE    RECE84-PTID       TO      SRY-PTID
               MOVE    WRK-ZENYYMM       TO      SRY-SRYYM
               MOVE    CONS-YAKUKAN-COMMENT  TO  SRY-SRYCD (1)
               MOVE    CONS-YAKUKAN-COMMENT  TO  SRY-SRYCD (2)
               MOVE    CONS-YAKUKAN-COMMENT  TO  SRY-SRYCD (3)
               MOVE    CONS-YAKUKAN-COMMENT  TO  SRY-SRYCD (4)
               MOVE    CONS-YAKUKAN-COMMENT  TO  SRY-SRYCD (5)
      *
               MOVE    SRYACT-REC        TO      MCPDATA-REC
               MOVE    "DBSELECT"        TO      MCP-FUNC
               MOVE    "SRYACT-KEY7"     TO      ORC-DBPATH
               CALL    "ORCMCPSUB"       USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC
      *
               IF      MCP-RC               =       ZERO
                   MOVE    "SRYACT-KEY7"   TO      ORC-DBPATH
                   PERFORM  900-SRYACT-READ-NEXT-SEC
                   PERFORM  UNTIL   FLG-SRYACT           =  1
      *
                     INITIALIZE                     SRYACCT-REC
                     MOVE    SRY-HOSPID     TO      ACCT-HOSPID
                     MOVE    SRY-NYUGAIKBN  TO      ACCT-NYUGAIKBN
                     MOVE    SRY-PTID       TO      ACCT-PTID
                     MOVE    SRY-SRYKA      TO      ACCT-SRYKA
                     MOVE    SRY-SRYYM      TO      ACCT-SRYYM
                     MOVE    SRY-ZAINUM     TO      ACCT-ZAINUM
      *
                     MOVE    SRYACCT-REC    TO      MCPDATA-REC
                     MOVE    "DBSELECT"     TO      MCP-FUNC
                     MOVE    "SRYACCT-KEY31"  TO    ORC-DBPATH
                     CALL    "ORCMCPSUB"       USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
      *
                     IF  MCP-RC                =  ZERO
                         MOVE    "DBFETCH"      TO      MCP-FUNC
                         MOVE    "SRYACCT-KEY31"  TO    ORC-DBPATH
                         CALL    "ORCMCPSUB"           USING
                                                       MCPAREA
                                                       ORCMCPAREA
                                                       MCPDATA-REC
                         IF      MCP-RC        =       ZERO
      *
                           MOVE    MCPDATA-REC  TO  SRYACCT-REC
      *
                           PERFORM    VARYING    IDX  FROM   1   BY   1
                                UNTIL      IDX      >   31
                             IF    ACCT-DAY(1 IDX)  NOT =  ZERO
                                IF    IDX   >   WRK-DAY-YAKUKAN
                                    MOVE  IDX   TO  WRK-DAY-YAKUKAN
                                END-IF
                             END-IF
                           END-PERFORM
      *
                         END-IF
                     END-IF
      *
                     MOVE    "DBCLOSECURSOR"   TO      MCP-FUNC
                     MOVE    "SRYACCT-KEY31"   TO      ORC-DBPATH
                     CALL    "ORCMCPSUB"       USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
      *
                     MOVE     "SRYACT-KEY7"   TO   ORC-DBPATH
                     PERFORM   900-SRYACT-READ-NEXT-SEC
      *
                   END-PERFORM
               END-IF
      *
               MOVE    "DBCLOSECURSOR"   TO      MCP-FUNC
               MOVE    "SRYACT-KEY7"     TO      ORC-DBPATH
               CALL    "ORCMCPSUB"       USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC
      *
             END-IF
      *
           END-IF
      *
           IF      WRK-DAY-YAKUKAN    NOT   =   ZERO
                MOVE       99             TO   FLG-COMMENT
                MOVE    WRK-ZENYYMM(5:2)  TO   WRK-TUKI-YAKUKAN
                PERFORM    20011-TAIIN-COMMENT-SEC
           END-IF
      *
           .
       2001-TAIIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOUSIJI-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-HOUSIJI-COMMENT   =    "1"
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
           IF   RECE84-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE84-KAISU(1 IDX3)  NOT =  ZERO
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
             MOVE    RECE84-HOSPID       TO  SANTEI-HOSPID
             MOVE    RECE84-PTID         TO  SANTEI-PTID
             MOVE    CONS-TOSEKI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
             CALL    "ORCMCPSUB"         USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC
             IF      MCP-RC              =   ZERO
                    MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
                    PERFORM        900-SANTEI-READ-SEC
                    PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                     ( FLG-HINKAI-COMMENT =   1 )
      *
                       MOVE   SANTEI-FSANTEIYMD  TO  WRK-FSANTEIYMD
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
                       MOVE    RECE84-SRYYM    TO  WRK-HINKAIYMD(1:6)
                       MOVE    WRK-DAY         TO  WRK-HINKAIYMD(7:2)
      *
                       IF     WRK-DAY      NOT  =  ZERO
                       AND    WRK-FSANTEIYMD   <=  WRK-HINKAIYMD
                       AND    WRK-HINKAIYMD    <   WRK-AFTER2
                             MOVE    1    TO   FLG-HINKAI-COMMENT
                             MOVE  WRK-FSANTEIYMD
                                          TO   WRK-TOSEKIYMD
                       END-IF
      * 
                       MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
                       PERFORM        900-SANTEI-READ-SEC
                    END-PERFORM
             END-IF
      *
             MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
             MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
             CALL    "ORCMCPSUB"         USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC
      *
             IF      FLG-HINKAI-COMMENT    =      1
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
             PERFORM   200111-RECE85-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE85
             MOVE    SPACE                TO  WRK-RECE85XX-REC
             INITIALIZE                       WRK-RECE85XX-REC
      *      ¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE84-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    16     TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "½é²ó»ØÆ³´ÉÍý»»ÄêÆü¡¡¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(21:4)
             MOVE    WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(27:4)
             MOVE    "1"    TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HINKAI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆü¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SHUJUTSU-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-SHUJUTU-COMMENT   =    SPACE
               MOVE       1       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE84-WRITE-SEC
           END-IF
      *
           .
       2001-SHUJUTSU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ê¸¡Áª¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-KENSEN-HANTEI-SEC          SECTION.
      *
           IF     RECE84-SAFLG   =    "2"
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE84-WRITE-SEC
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
           PERFORM   200111-RECE85-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
      *
           PERFORM
             UNTIL  ( FLG-RECE85           =   1                   )
             OR     ( RECE84-HOSPID    NOT =   RECE85-HOSPID       )
             OR     ( RECE84-SRYYM     NOT =   RECE85-SRYYM        )
             OR     ( RECE84-NYUGAIKBN NOT =   RECE85-NYUGAIKBN    )
             OR     ( RECE84-PTID      NOT =   RECE85-PTID         )
             OR     ( RECE84-RECEKA    NOT =   RECE85-RECEKA       )
             OR     ( RECE84-HKNID     NOT =   RECE85-HKNID        )
             OR     ( RECE84-TEKSTYMD  NOT =   RECE85-TEKSTYMD     )
             OR     ( RECE84-ZAINUM    NOT =   RECE85-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE85-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE85
                  MOVE   RECE85-SRYACT-TBL(IDX)
                             TO   WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
      *++++++
                  IF    IDX-RECE85  =  RECE84-MEISAISU
      *
                    IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = "810000001"
                    OR  WRK-RECE85XX-SRYCD(IDX-RECE85 1)(1:3)  = "008"
      *
                        COMPUTE   IDV  =
                         (WRK-RECE85XX-YUKOKETA(IDX-RECE85 1) * 2) + 1
      *
                        IF   WRK-RECE85XX-YUKOKETA(IDX-RECE85 1) <=  96
                          MOVE    "¡¡¡ß"   TO
                           WRK-RECE85XX-NAME(IDX-RECE85 1)(IDV:4)
                        END-IF
      *
                        COMPUTE   IDV  =  IDV  +  4
      *
                        MOVE    RECE84-ZAIKAISU   TO   DAY-IDX2
                        PERFORM 20011-KAISU-HENSYU-SEC
                        IF   WRK-RECE85XX-YUKOKETA(IDX-RECE85 1) <=  96
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE85XX-NAME(IDX-RECE85 1)(IDV:4)
                          COMPUTE WRK-RECE85XX-YUKOKETA(IDX-RECE85 1) =
                                  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1) +
                                  4
                        END-IF
      *
                    END-IF
      *
                  END-IF
      *++++++
             END-PERFORM 
      *
             PERFORM 990-RECE85-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¼ê½ÑÆü¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE85
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE85
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE85  =    1
      *
                  INITIALIZE                        RECE859-REC
                  MOVE    WRK-RECE85-KEY       TO   RECE859-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE859-RENNUM
                  MOVE    WRK-RECE85-PTNUM     TO   RECE859-PTNUM
                  MOVE    WRK-RECE85-SRYKA     TO   RECE859-SRYKA
                  MOVE    WRK-RECE85-SRYKBN    TO   RECE859-SRYKBN
                  MOVE    WRK-RECE85-SRYSYUKBN TO   RECE859-SRYSYUKBN
                  MOVE    WRK-RECE85-JIHIMONEY TO   RECE859-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE85   =   1)
                     ADD         1             TO    IDX-RECE85
                     IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE85
                     ELSE
                         MOVE  WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
                                          TO   RECE859-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE85XX-SIKFLG(IDX-RECE85 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE859-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE859-REC
                     ADD     1            TO   CNT-RECE859
                  END-IF
           END-PERFORM
      *
           MOVE    RECE84-REC        TO      RECE849-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE849-MEISAISU
           WRITE   RECE849-REC
           ADD     1                 TO      CNT-RECE849
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
      *    £Ã£Ô¡¦£Í£Ò£É¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-CTMRI-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-CTMRI-COMMENT   =    "1"
                MOVE       2       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE84-WRITE-SEC
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
           IF      RECE84-FLG-HOUSEI-COMMENT   =    "1"
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
           MOVE       RECE84-REC       TO  RECE849-REC
           ADD     1                   TO  RECE849-MEISAISU
           WRITE      RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPID       TO  RECE85-HOSPID
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID        TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                 KEY IS    >=   RECE85-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE85
                 NOT INVALID
                     PERFORM 990-RECE85-NEXT-SEC
           END-START
      *
           MOVE    RECE85-REC           TO  RECE859-REC
           MOVE    RECE85-SRYACT-TBL(1) TO  RECE859-SRYACT-TBL(1)
           MOVE    RECE85-SRYACT-TBL(2) TO  RECE859-SRYACT-TBL(3)
           MOVE    RECE85-SRYACT-TBL(3) TO  RECE859-SRYACT-TBL(4)
           MOVE    RECE85-SRYACT-TBL(4) TO  RECE859-SRYACT-TBL(5)
     *
           MOVE    SPACE                TO  RECE859-SRYACT-TBL(2)
           INITIALIZE                       RECE859-SRYACT-TBL(2)
           MOVE    "810000001"          TO  RECE859-SRYCD(2)
           MOVE    ZERO                 TO  RECE859-SRYSURYO(2)
           MOVE    16                   TO  RECE859-YUKOKETA(2)
           MOVE    "ÌôºÞ´ÉÍý»ØÆ³ÎÁ»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü"
                                        TO  RECE859-NAME(2)
           MOVE     WRK-TUKI-YAKUKAN    TO  DAY-IDX
           MOVE     TBL-DAY(DAY-IDX)    TO  RECE859-NAME(2)(21:4)
           MOVE     WRK-DAY-YAKUKAN     TO  DAY-IDX
           MOVE     TBL-DAY(DAY-IDX)    TO  RECE859-NAME(2)(27:4)
           MOVE     "1"                 TO  RECE859-SIKFLG(2)
      *
           WRITE    RECE859-REC
           ADD      1                   TO  CNT-RECE859
      *
           .
       20011-TAIIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥³¥á¥ó¥ÈÊÔ½¸¡Êº¹¹þ¡§¾å¡Ë
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
           PERFORM  200111-RECE85-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1       TO  IDX-RECE85
                MOVE   "810000001"
                               TO  WRK-RECE85XX-SRYCD(IDX-RECE85 1)
                MOVE   ZERO    TO  WRK-RECE85XX-SRYSURYO(IDX-RECE85 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                               TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                MOVE   WK-FREE-NAME2(IDX)
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                MOVE    "1"    TO  WRK-RECE85XX-SIKFLG(IDX-RECE85 1)
           END-PERFORM 
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥³¥á¥ó¥ÈÊÔ½¸£²¡Êº¹¹þ¡§²¼¡Ë
      *****************************************************************
       20011-SHUJUTSU-ETC2-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM   200111-FREE-COMMENT2-SEC                              
      *
           PERFORM   200111-RECE85-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
      *
           MOVE    RECE84-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                              TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO   TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"   TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM 
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
           MOVE    ZERO          TO  DAY-IDX
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
               IF    RECE84-KAISU(1 IDX)  NOT =  ZERO
                   ADD   1     TO   DAY-IDX
                   MOVE  IDX   TO  TB-SANTEI-DAY(DAY-IDX)
               END-IF
           END-PERFORM
           .
       200111-SANTEI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆüÉÕ¤ÎÊÔ½¸
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
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "¸òÉÕ"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
           END-EVALUATE
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
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "¡¡"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
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
      *    ÆüÉÕ¤ÎÊÔ½¸¡Ê²þ¹Ô¡Ë
      *****************************************************************
       2001111-FREE-COMMENT2-KAIGYO-SEC          SECTION.                  
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA2(IDZ)
           MOVE    ALL "¡¡"        TO  WK-FREE-NAME2(IDZ)(1:WRK-KETA)
      *
           .
       2001111-FREE-COMMENT2-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-RIHA-COMMENT-SEC          SECTION.
      *
             PERFORM   200111-RECE85-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE85
             MOVE    SPACE                TO  WRK-RECE85XX-REC
             INITIALIZE                       WRK-RECE85XX-REC
      *      ¥ê¥Ï¥Ó¥ê¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE84-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7      TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "¼Â»ÜÆü¿ô¡¡¡¡Æü"
                            TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
             MOVE    "1"    TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             PERFORM  VARYING   IDX  FROM  1  BY  1
                       UNTIL    IDX   >   31
                 IF   RECE84-KAISU(1 IDX)  NOT = ZERO
                        ADD      1        TO  CNT-RIHA
                 END-IF
             END-PERFORM
             IF      CNT-RIHA             NOT =    ZERO
               MOVE    CNT-RIHA             TO  DAY-IDX
               MOVE    TBL-DAY(DAY-IDX)
                          TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(9:4)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       2001-RIHA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ê¥ê¥ÏÁª¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-RIHASEN-HANTEI-SEC          SECTION.
      *
           IF     RECE84-SAFLG   =    "2"
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
           PERFORM     20011-NYU-DAY-SEC
      *
           PERFORM     200111-RECE85-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
      *
           MOVE    RECE84-MEISAISU      TO  MAX-MEISAISU
      *
      *    »»ÄêÆü¥³¥á¥ó¥È¤ÎÁÞÆþ
           PERFORM  VARYING      IDX    FROM  1   BY   1
                    UNTIL       (IDX    >     31)      OR
                                (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
      ***      MOVE    TBL-NYU-DAY-CD(IDX)
               MOVE    "810000001"
                             TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
               MOVE    ZERO  TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
               IF    TBL-NYU-DAY-CD(IDX)  =  CONS-SANTEI-COMMENT1
                 MOVE    4   TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "¡¡¡¡¡¡Æü"
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(3:4)
               ELSE
                 MOVE    8   TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "¡¡¡¡¡¡Æü¡Á¡¡¡¡Æü"
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX) 
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(3:4)
                 MOVE    TBL-NYU-DAY-ED(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(11:4)
               END-IF
               MOVE    "1"   TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
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
                IF    RECE84-KAISU(1 IDX)   =  ZERO
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
           .
       20011-NYU-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    Àº¿À²Ê³ÖÎ¥ÉÂ¼¼´ÉÍý²Ã»»¡ÊÆþ±¡ÎÁ²Ã»»¡Ë¤¬¤¢¤ë¤«È½Äê
      *    ¡Ê¿ÇÎÅ¹Ô°ÙÆþÎÏ¡Ë
      *****************************************************************
       2001-KAKURI-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-KAKURI-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-KAKURI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï«ºÒ¼£ÎÅ·×²è²Ã»»¡ÊÆþ±¡ÎÁ²Ã»»¡Ë¤¬¤¢¤ë¤«È½Äê
      *    ¡Ê¿ÇÎÅ¹Ô°ÙÆþÎÏ¡Ë
      *****************************************************************
       2001-CHIRYO-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-CHIRYO-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   20011-CHIRYO-COMMENT-SEC
           END-IF
      *
           .
       2001-CHIRYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï«ºÒ¼£ÎÅ·×²è²Ã»»¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-CHIRYO-COMMENT-SEC          SECTION.
      *
           PERFORM    200111-SANTEI-DAY-SEC
      *
           MOVE       RECE84-REC       TO  RECE849-REC
           ADD     1                   TO  RECE849-MEISAISU
           WRITE      RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPID       TO  RECE85-HOSPID
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID        TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                 KEY IS    >=   RECE85-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE85
                 NOT INVALID
                     PERFORM 990-RECE85-NEXT-SEC
           END-START
      *
           MOVE    RECE85-REC           TO  RECE859-REC
           MOVE    RECE85-SRYACT-TBL(1) TO  RECE859-SRYACT-TBL(1)
           MOVE    RECE85-SRYACT-TBL(2) TO  RECE859-SRYACT-TBL(3)
           MOVE    RECE85-SRYACT-TBL(3) TO  RECE859-SRYACT-TBL(4)
           MOVE    RECE85-SRYACT-TBL(4) TO  RECE859-SRYACT-TBL(5)
      *
           MOVE    SPACE                TO  RECE859-SRYACT-TBL(2)
           INITIALIZE                       RECE859-SRYACT-TBL(2)
           MOVE    "810000001"          TO  RECE859-SRYCD(2)
           MOVE    ZERO                 TO  RECE859-SRYSURYO(2)
           MOVE    9                    TO  RECE859-YUKOKETA(2)
           MOVE    "»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü" TO  RECE859-NAME(2)
           MOVE     RECE82-SRYYM(5:2)   TO  DAY-IDX2
           MOVE     TBL-DAY(DAY-IDX2)   TO  RECE859-NAME(2)(7:4)
           MOVE     TB-SANTEI-DAY(1)    TO  DAY-IDX2
           MOVE     TBL-DAY(DAY-IDX2)   TO  RECE859-NAME(2)(13:4)
           MOVE     "1"                 TO  RECE859-SIKFLG(2)
      *
           WRITE    RECE859-REC
           ADD      1                   TO  CNT-RECE859
      *
           .
       20011-CHIRYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
      *****************************************************************
       2099-RECE84-WRITE-SEC      SECTION.
      *
           MOVE    RECE84-REC      TO  RECE849-REC
           WRITE   RECE849-REC
           ADD     1               TO  CNT-RECE849
      *
           PERFORM 20991-RECE85-TENSOU-SEC
           .
       2099-RECE84-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£µ½ÐÎÏ½èÍý
      *****************************************************************
       20991-RECE85-TENSOU-SEC     SECTION.
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPID       TO  RECE85-HOSPID
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID        TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
               KEY IS    >=   RECE85-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE85
               NOT INVALID
                   PERFORM 990-RECE85-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE85           =   1                   )
               OR     ( RECE84-HOSPID    NOT =   RECE85-HOSPID       )
               OR     ( RECE84-SRYYM     NOT =   RECE85-SRYYM        )
               OR     ( RECE84-NYUGAIKBN NOT =   RECE85-NYUGAIKBN    )
               OR     ( RECE84-PTID      NOT =   RECE85-PTID         )
               OR     ( RECE84-RECEKA    NOT =   RECE85-RECEKA       )
               OR     ( RECE84-HKNID     NOT =   RECE85-HKNID        )
               OR     ( RECE84-TEKSTYMD  NOT =   RECE85-TEKSTYMD     )
               OR     ( RECE84-ZAINUM    NOT =   RECE85-ZAINUM       )
      *
      *
               MOVE          RECE85-REC       TO      RECE859-REC
               WRITE         RECE859-REC
               ADD     1                 TO  CNT-RECE859
      *
               PERFORM 990-RECE85-NEXT-SEC
      *
           END-PERFORM
      *
           .
       20991-RECE85-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡ÂàÈò½èÍý
      *****************************************************************
       200111-RECE85-TAIHI-SEC     SECTION.
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPID       TO  RECE85-HOSPID
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID        TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                 KEY IS    >=   RECE85-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE85
                 NOT INVALID
                     PERFORM 990-RECE85-NEXT-SEC
                     MOVE    RECE85-REC  TO  WRK-RECE85-REC
           END-START
      *
           .
       200111-RECE85-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       200111-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE85           =   1                   )
             OR     ( RECE84-HOSPID    NOT =   RECE85-HOSPID       )
             OR     ( RECE84-SRYYM     NOT =   RECE85-SRYYM        )
             OR     ( RECE84-NYUGAIKBN NOT =   RECE85-NYUGAIKBN    )
             OR     ( RECE84-PTID      NOT =   RECE85-PTID         )
             OR     ( RECE84-RECEKA    NOT =   RECE85-RECEKA       )
             OR     ( RECE84-HKNID     NOT =   RECE85-HKNID        )
             OR     ( RECE84-TEKSTYMD  NOT =   RECE85-TEKSTYMD     )
             OR     ( RECE84-ZAINUM    NOT =   RECE85-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE85-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE85
                  MOVE   RECE85-SRYACT-TBL(IDX)
                             TO   WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
             END-PERFORM 
      *
             PERFORM 990-RECE85-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¼ê½ÑÆü¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE85
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE85
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE85  =    1
      *
                  INITIALIZE                        RECE859-REC
                  MOVE    WRK-RECE85-KEY       TO   RECE859-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE859-RENNUM
                  MOVE    WRK-RECE85-PTNUM     TO   RECE859-PTNUM
                  MOVE    WRK-RECE85-SRYKA     TO   RECE859-SRYKA
                  MOVE    WRK-RECE85-SRYKBN    TO   RECE859-SRYKBN
                  MOVE    WRK-RECE85-SRYSYUKBN TO   RECE859-SRYSYUKBN
                  MOVE    WRK-RECE85-JIHIMONEY TO   RECE859-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE85   =   1)
                     ADD         1             TO    IDX-RECE85
                     IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE85
                     ELSE
                         MOVE  WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
                                          TO   RECE859-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE85XX-SIKFLG(IDX-RECE85 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE859-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE859-REC
                     ADD     1            TO   CNT-RECE859
                  END-IF
           END-PERFORM
      *
           MOVE    RECE84-REC        TO      RECE849-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE849-MEISAISU
           WRITE   RECE849-REC
           ADD     1                 TO      CNT-RECE849
      *
           .
       200111-SRYACTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    º¹¹þÊÔ½¸£²
      *****************************************************************
       250-SASHIKOMI2-SEC     SECTION.
      *
      *    ¥Ñ¥é¥á¡¼¥¿£¸£´£¹¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           INITIALIZE                      RECE849-REC
           MOVE    RECE82-HOSPID       TO  RECE849-HOSPID
           MOVE    RECE82-SRYYM        TO  RECE849-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE849-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE849-PTID
           MOVE    RECE82-RECEKA       TO  RECE849-RECEKA
           MOVE    RECE82-HKNID        TO  RECE849-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE849-TEKSTYMD
           MOVE    ZERO                TO  RECE849-ZAINUM
      *
           START   RECE849-FILE
               KEY IS    >=   RECE849-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE849
               NOT INVALID
                   PERFORM 990-RECE849-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE849          =   1                    )
               OR     ( RECE82-HOSPID    NOT =   RECE849-HOSPID       )
               OR     ( RECE82-SRYYM     NOT =   RECE849-SRYYM        )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE849-NYUGAIKBN    )
               OR     ( RECE82-PTID      NOT =   RECE849-PTID         )
               OR     ( RECE82-RECEKA    NOT =   RECE849-RECEKA       )
               OR     ( RECE82-HKNID     NOT =   RECE849-HKNID        )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE849-TEKSTYMD     )
      *
               MOVE   ZERO   TO  FLG-COMMENT
      *        ¡Ê»ÀÁÇ·×»»¼°¤Î¥³¥á¥ó¥È¤Î¥»¥Ã¥È¡Ë
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2501-SANSO-HANTEI-SEC
               END-IF
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2599-RECE849-WRITE-SEC
               END-IF
      *
               PERFORM 990-RECE849-NEXT-SEC
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
           IF     RECE849-FLG-SANSO-COMMENT   =    "1"
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
           INITIALIZE                       RECE859-REC
           MOVE    RECE849-HOSPID       TO  RECE859-HOSPID
           MOVE    RECE849-SRYYM        TO  RECE859-SRYYM
           MOVE    RECE849-NYUGAIKBN    TO  RECE859-NYUGAIKBN
           MOVE    RECE849-PTID         TO  RECE859-PTID
           MOVE    RECE849-RECEKA       TO  RECE859-RECEKA
           MOVE    RECE849-HKNID        TO  RECE859-HKNID
           MOVE    RECE849-TEKSTYMD     TO  RECE859-TEKSTYMD
           MOVE    RECE849-ZAINUM       TO  RECE859-ZAINUM
           START   RECE859-FILE
               KEY IS    >=   RECE859-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE859
               NOT INVALID
                   PERFORM 990-RECE859-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE859           =   1                 )
               OR     ( RECE849-HOSPID    NOT =   RECE859-HOSPID    )
               OR     ( RECE849-SRYYM     NOT =   RECE859-SRYYM     )
               OR     ( RECE849-NYUGAIKBN NOT =   RECE859-NYUGAIKBN )
               OR     ( RECE849-PTID      NOT =   RECE859-PTID      )
               OR     ( RECE849-RECEKA    NOT =   RECE859-RECEKA    )
               OR     ( RECE849-HKNID     NOT =   RECE859-HKNID     )
               OR     ( RECE849-TEKSTYMD  NOT =   RECE859-TEKSTYMD  )
               OR     ( RECE849-ZAINUM    NOT =   RECE859-ZAINUM    )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE859-SRYCD(IDX) =   SPACE )
      *
                  SET     TBL-SANSO    TO     1
                  SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                       AT  END
                           MOVE    ZERO    TO   FLG-SANSO-OK
                       WHEN   RECE859-SRYCD(IDX) = TBL-SANSO-SRYCD
                                                       (TBL-SANSO)
                           MOVE    1       TO   FLG-SANSO-OK
                  END-SEARCH
                  IF      FLG-SANSO-OK        =    1
                     ADD      1             TO  IDX4
                     MOVE  RECE859-TEN(IDX) TO  TBL-SANSO-TEN(IDX4)
                     MOVE  RECE859-SRYSURYO(IDX)
                                            TO  TBL-SANSO-SRYSURYO
                                                             (IDX4)
                  END-IF
                  IF    RECE859-SRYCD(IDX) = "770030070"
                     IF   TBL-KASAN-SRYSURYO  =  ZERO
                        MOVE   RECE859-SRYSURYO(IDX)
                                     TO  TBL-KASAN-SRYSURYO
                     ELSE
                        COMPUTE   TBL-KASAN-SRYSURYO  =  
                                  TBL-KASAN-SRYSURYO  *  
                                  RECE859-SRYSURYO(IDX)  
                     END-IF
                  END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE859-NEXT-SEC
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
           PERFORM   250111-SANSO-FREE-COMMENT-SEC
      *
           PERFORM   250111-RECE859-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£¸£µ£¹¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
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
           MOVE    ZERO        TO    WRK-KEISAN3
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
                COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
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
                COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
                                      * TBL-KASAN-SRYSURYO)
             END-IF
      *
           END-PERFORM
      *
           IF   IDX  =  2
              MOVE    "¡à£±£°¡á"  TO  WK-FREE-NAME3(WRK-KETA:8)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  8
      *
              COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
              COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
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
                  COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
                  COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
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
      *    £¸£µ£¹¡¡ÂàÈò½èÍý
      *****************************************************************
       250111-RECE859-TAIHI-SEC     SECTION.
      *
           INITIALIZE                       RECE859-REC
           MOVE    RECE849-HOSPID       TO  RECE859-HOSPID
           MOVE    RECE849-SRYYM        TO  RECE859-SRYYM
           MOVE    RECE849-NYUGAIKBN    TO  RECE859-NYUGAIKBN
           MOVE    RECE849-PTID         TO  RECE859-PTID
           MOVE    RECE849-RECEKA       TO  RECE859-RECEKA
           MOVE    RECE849-HKNID        TO  RECE859-HKNID
           MOVE    RECE849-TEKSTYMD     TO  RECE859-TEKSTYMD
           MOVE    RECE849-ZAINUM       TO  RECE859-ZAINUM
           START   RECE859-FILE
                 KEY IS    >=   RECE859-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE859
                 NOT INVALID
                     PERFORM 990-RECE859-NEXT-SEC
                     MOVE   RECE859-REC TO  WRK-RECE85-REC
           END-START
      *
           .
       250111-RECE859-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £¸£µ£¹¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       250111-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE859           =   1                    )
             OR     ( RECE849-HOSPID    NOT =   RECE859-HOSPID       )
             OR     ( RECE849-SRYYM     NOT =   RECE859-SRYYM        )
             OR     ( RECE849-NYUGAIKBN NOT =   RECE859-NYUGAIKBN    )
             OR     ( RECE849-PTID      NOT =   RECE859-PTID         )
             OR     ( RECE849-RECEKA    NOT =   RECE859-RECEKA       )
             OR     ( RECE849-HKNID     NOT =   RECE859-HKNID        )
             OR     ( RECE849-TEKSTYMD  NOT =   RECE859-TEKSTYMD     )
             OR     ( RECE849-ZAINUM    NOT =   RECE859-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX                >   5     )
                       OR      ( RECE859-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE85
                  MOVE   RECE859-SRYACT-TBL(IDX)
                             TO   WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
      *
                  IF   RECE859-SRYCD(IDX) = "770020070" OR "770030070"
                       MOVE   IDX-RECE85   TO   WRK-JYUNBAN
                  END-IF
      *
             END-PERFORM 
      *
             PERFORM 990-RECE859-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-GYOSUU   NOT =  ZERO
      *
      *      ÊäÀµÎ¨Ëô¤Ï¹âµ¤°µ»ÀÁÇ²Ã»»°Ê¹ß¤ÎÌÀºÙ¤ò¤º¤é¤¹
             IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE85   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  WRK-GYOSUU
                   MOVE   WRK-RECE85XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE85XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE85XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
             END-IF
      *
      *      ·×»»¼°¤Îº¹¤·¹þ¤ß
             IF   WRK-JYUNBAN  NOT =  ZERO
                 MOVE      WRK-JYUNBAN    TO     MAX-MEISAISU
             ELSE
                 MOVE      IDX-RECE85     TO     MAX-MEISAISU
             END-IF
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   50    )
                         OR    ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD    1      TO  MAX-MEISAISU
                MOVE   "810000001"
                              TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO   TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"   TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
      *
             END-PERFORM
      *
           END-IF
      *
      *    ¼ê½ÑÆü¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE85
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE85
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE85  =    1
      *
                  INITIALIZE                        RECE851-REC
                  MOVE    WRK-RECE85-KEY       TO   RECE851-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE851-RENNUM
                  MOVE    WRK-RECE85-PTNUM     TO   RECE851-PTNUM
                  MOVE    WRK-RECE85-SRYKA     TO   RECE851-SRYKA
                  MOVE    WRK-RECE85-SRYKBN    TO   RECE851-SRYKBN
                  MOVE    WRK-RECE85-SRYSYUKBN TO   RECE851-SRYSYUKBN
                  MOVE    WRK-RECE85-JIHIMONEY TO   RECE851-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE85   =   1)
                     ADD         1             TO    IDX-RECE85
                     IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE85
                     ELSE
                         MOVE  WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
                                          TO   RECE851-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE85XX-SIKFLG(IDX-RECE85 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE851-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE851-REC
                     ADD     1            TO   CNT-RECE85
                  END-IF
           END-PERFORM
      *
           MOVE    RECE849-REC       TO      RECE841-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE841-MEISAISU
           WRITE   RECE841-REC
           ADD     1                 TO      CNT-RECE84
      *
           .
       250111-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë£¸£´£¹½ÐÎÏ½èÍý
      *****************************************************************
       2599-RECE849-WRITE-SEC      SECTION.
      *
           MOVE    RECE849-REC      TO  RECE841-REC
           WRITE   RECE841-REC
           ADD     1                TO  CNT-RECE84
      *
           PERFORM 25991-RECE859-TENSOU-SEC
           .
       2599-RECE849-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£¸£µ£¹½ÐÎÏ½èÍý
      *****************************************************************
       25991-RECE859-TENSOU-SEC     SECTION.
      *
           INITIALIZE                       RECE859-REC
           MOVE    RECE849-HOSPID       TO  RECE859-HOSPID
           MOVE    RECE849-SRYYM        TO  RECE859-SRYYM
           MOVE    RECE849-NYUGAIKBN    TO  RECE859-NYUGAIKBN
           MOVE    RECE849-PTID         TO  RECE859-PTID
           MOVE    RECE849-RECEKA       TO  RECE859-RECEKA
           MOVE    RECE849-HKNID        TO  RECE859-HKNID
           MOVE    RECE849-TEKSTYMD     TO  RECE859-TEKSTYMD
           MOVE    RECE849-ZAINUM       TO  RECE859-ZAINUM
           START   RECE859-FILE
               KEY IS    >=   RECE859-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE859
               NOT INVALID
                   PERFORM 990-RECE859-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE859           =   1                    )
               OR     ( RECE849-HOSPID    NOT =   RECE859-HOSPID       )
               OR     ( RECE849-SRYYM     NOT =   RECE859-SRYYM        )
               OR     ( RECE849-NYUGAIKBN NOT =   RECE859-NYUGAIKBN    )
               OR     ( RECE849-PTID      NOT =   RECE859-PTID         )
               OR     ( RECE849-RECEKA    NOT =   RECE859-RECEKA       )
               OR     ( RECE849-HKNID     NOT =   RECE859-HKNID        )
               OR     ( RECE849-TEKSTYMD  NOT =   RECE859-TEKSTYMD     )
               OR     ( RECE849-ZAINUM    NOT =   RECE859-ZAINUM       )
      *
               MOVE    RECE859-REC       TO   RECE851-REC
               WRITE   RECE851-REC
               ADD     1                 TO   CNT-RECE85
      *
               PERFORM 990-RECE859-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25991-RECE859-TENSOU-EXT.
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
      *    ½èÍýÃæ»ß½èÍý
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
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
      *            ¥¸¥ç¥Ö´ÉÍý½ªÎ»½èÍý
                   MOVE    "CAN"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
                   MOVE    WRK-PARA-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "8888"          TO  JOB-ERRCD
                   MOVE    2               TO  JOB-STOPFLG
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
               END-IF
      *                             
               MOVE    1                   TO  FLG-END
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
           DISPLAY "RECE82  CNT-IN  :" CNT-RECE82  
           DISPLAY "RECE849 CNT-WK  :" CNT-RECE849 
           DISPLAY "RECE859 CNT-WK  :" CNT-RECE859 
           DISPLAY "RECE84  CNT-OUT :" CNT-RECE84  
           DISPLAY "RECE85  CNT-OUT :" CNT-RECE85  
           DISPLAY "*** ORCR0870 END ***"
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
           CLOSE                       RECE82-FILE
      *
           CLOSE                       RECE84-FILE
      *
           CLOSE                       RECE85-FILE
      *
           CLOSE                       RECE841-FILE     
           CLOSE                       RECE851-FILE     
      *
           CLOSE                       RECE849-FILE
           CLOSE                       RECE859-FILE
      *
           .
       310-FILE-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿½ç¸¡º÷½èÍý
      *****************************************************************
       990-RECE82-READ-SEC           SECTION.
      *
           READ    RECE82-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE82
                   ADD     1           TO  CNT-IN-COMMIT
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
           END-READ
      *
           .
       990-RECE82-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE84-NEXT-SEC           SECTION.
      *
           READ    RECE84-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE84
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE84
           END-READ
      *
           .
       990-RECE84-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£¸£´£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE849-NEXT-SEC           SECTION.
      *
           READ    RECE849-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE849
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE849
           END-READ
      *
           .
       990-RECE849-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE85-NEXT-SEC           SECTION.
      *
           READ    RECE85-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE85
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE85
           END-READ
      *
           .
       990-RECE85-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£¸£µ£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE859-NEXT-SEC           SECTION.
      *
           READ    RECE859-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE859
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE859
           END-READ
      *
           .
       990-RECE859-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"        TO      MCP-FUNC
           MOVE    "SRYACT-KEY8"     TO      ORC-DBPATH
           CALL    "ORCMCPSUB"       USING
                                     MCPAREA
                                     ORCMCPAREA
                                     MCPDATA-REC
      *
           IF  MCP-RC                =       ZERO
               MOVE    "DBFETCH"     TO      MCP-FUNC
               MOVE    "SRYACT-KEY8" TO      ORC-DBPATH
               CALL    "ORCMCPSUB"           USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
               IF      MCP-RC        =       ZERO
                   MOVE    MCPDATA-REC         TO  SRYACT-REC
                   MOVE    ZERO                TO  FLG-SRYACT
               ELSE
                   MOVE    1                   TO  FLG-SRYACT
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
           MOVE    "SRYACT-KEY8"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"       USING
                                     MCPAREA
                                     ORCMCPAREA
                                     MCPDATA-REC
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       900-SRYACT-READ-NEXT-SEC           SECTION.
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
       900-SRYACT-READ-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÎò¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
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
      *    ´µ¼Ô¥³¥á¥ó¥È¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-PTCOM-READ-SEC           SECTION.
      *
           MOVE     PTCOM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £Ä£Â¡¡¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       120-DBOPEN-EXT.
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
