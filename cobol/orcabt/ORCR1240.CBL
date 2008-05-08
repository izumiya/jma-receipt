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
       PROGRAM-ID.             ORCR1240.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¸ø³²¥ì¥»¥×¥È¡ÊÆþ±¡¡Ë
      *                      ¥ì¥»¥×¥È¥³¥á¥ó¥ÈÊÔ½¸
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  07/11/16    NACL¡ÝÆ£¸¶    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  04.02.01    NACL-ÌçÏÆ    08/03/24  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *                                     ¡ÊÌôºÞ´ÉÍý»ØÆ³ÎÁ¥³¡¼¥ÉÄÉ²Ã¡Ë
      *                                     ¡Ê¿ÇÎÅ¶èÊ¬£¶£´¡§ÉÂÍý¿ÇÃÇÂÐ±þ¡Ë
      *                                     ¡Êºß»ÙÉÂ¼«Æ°µ­ºÜÂÐ±þ¡Ë
      *  04.02.02    NACL-ÌçÏÆ    08/04/03  ¡û¡û¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑÊÔ½¸½¤Àµ
      *  04.02.03    NACL-¾®Æ¦Âô  08/04/08  °åÎÅ¶èÊ¬¥³¥á¥ó¥È²þÄêÂÐ±þ
      *  04.02.04    NACL-ÌçÏÆ    08/05/07  Âà±¡»þÉþÌô»ØÆ³²Ã»»»»ÄêÆüµ­ºÜ½¤Àµ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
           SELECT  RECE200-FILE    ASSIGN  RECE200PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE200-KEY
                                   FILE    STATUS  IS  STS-RECE200.
      *
           SELECT  RECE220-FILE    ASSIGN  RECE220PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE220-KEY
                                   FILE    STATUS  IS  STS-RECE220.
      *
           SELECT  RECE230-FILE    ASSIGN  RECE230PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE230-KEY
                                   FILE    STATUS  IS  STS-RECE230.
      *    ¡Ê£Ï£Õ£Ô¡Ë
           SELECT  RECE2201-FILE   ASSIGN  RECE2201PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE2201-KEY
                                   FILE    STATUS  IS  STS-RECE2201.
      *
           SELECT  RECE2301-FILE   ASSIGN  RECE2301PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE2301-KEY
                                   FILE    STATUS  IS  STS-RECE2301.
      *    ¡Ê¥ï¡¼¥¯¡Ë
           SELECT  RECE2209-FILE   ASSIGN  RECE2209PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE2209-KEY
                                   FILE    STATUS  IS  STS-RECE2209.
      *
           SELECT  RECE2309-FILE   ASSIGN  RECE2309PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE2309-KEY
                                   FILE    STATUS  IS  STS-RECE2309.
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
       FD  RECE2201-FILE.
       01  RECE2201-REC. 
           COPY    "CPRCF220.INC"  REPLACING   //RECE220//
                                   BY          //RECE2201//.
      *
       FD  RECE2301-FILE.
       01  RECE2301-REC. 
           COPY    "CPRCF230.INC"  REPLACING   //RECE230//
                                   BY          //RECE2301//.
      *¥ï¡¼¥¯
       FD  RECE2209-FILE.
       01  RECE2209-REC. 
           COPY    "CPRCF220.INC"  REPLACING   //RECE220//
                                   BY          //RECE2209//.
      *
       FD  RECE2309-FILE.
       01  RECE2309-REC. 
           COPY    "CPRCF230.INC"  REPLACING   //RECE230//
                                   BY          //RECE2309//.
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
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE2201//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE2301//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE2209//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE2309//.
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
           03  STS-RECE2201        PIC X(02).
           03  STS-RECE2301        PIC X(02).
           03  STS-RECE2209        PIC X(02).
           03  STS-RECE2309        PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE200         PIC 9(01).
           03  FLG-RECE220         PIC 9(01).
           03  FLG-RECE2209        PIC 9(01).
           03  FLG-RECE230         PIC 9(01).
           03  FLG-RECE2309        PIC 9(01).
           03  FLG-YAKUKAN-COMMENT PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT2 PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-WRKRECE230      PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-NYU-RENZOKU     PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-KYUSEI-OK       PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RIHA            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-RYOCOM          PIC 9(01).
           03  FLG-RYOCOM2         PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-COMOKIKAE       PIC 9(01).
           03  FLG-COMOKIKAE2      PIC 9(01).
           03  FLG-HENSYUKIN       PIC 9(01).
           03  FLG-HANTEI          PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-RECE200         PIC 9(06).
           03  CNT-RECE220         PIC 9(06).
           03  CNT-RECE230         PIC 9(06).
           03  CNT-RECE2209        PIC 9(06).
           03  CNT-RECE2309        PIC 9(06).
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
           03  IDX6                PIC 9(04).
           03  IDX12               PIC 9(04).
           03  IDX13               PIC 9(04).
           03  IDX14               PIC 9(04).
           03  IDX15               PIC 9(01).
           03  IDX16               PIC 9(02).
           03  IDX17               PIC 9(02).
           03  IDX18               PIC 9(02).
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-RECE230         PIC 9(04).
           03  IDV                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  RYO-IDX             PIC 9(03).
           03  RYO-IDX2            PIC 9(03).
           03  RYO-IDX3            PIC 9(03).
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
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
           03  WRK-PARA-HOSPNUM    PIC 9(02).
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
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-HENSYU-KAISU    PIC X(02).
           03  WRK-HENSYU-KAISU2   PIC X(04).
      *
           03  TBL-SANTEI-DAY.
               05  TB-SANTEI-DAY   PIC 9(02)   OCCURS  31.
      *
      *    ³ºÅö¿ÇÎÅ·îÊ¬
           03  TBL-RIHASANTEI-DAY.
               05  TB-RIHASANTEI-DAY PIC 9(01) OCCURS  31.
      *
      *    Ä¾¶áÊ¬
           03  WRK-RIHASANTEI-YMD.
               05  WRK-RIHASANTEI-YM PIC 9(06).
               05  WRK-RIHASANTEI-DD PIC 9(02).
      *
      *    Ä¾¶áÊ¬ÂàÈòÍÑ
           03  WRK-RIHASANTEI-YMD2   PIC X(08).
      *
      *    Á°·î°ÊÁ°¤ÎÄ¾¶á½ªÎ»Æü
           03  WRK-RIHASANTEI-END    PIC X(08).
      *
      *    £²£°£°£¶Ç¯£´·î£±Æü¤Ë»»ÄêÆü¤¬¤¢¤ë¤«
           03  WRK-RIHASANTEI41-FLG  PIC 9(01).
      *
           03  TBL-NYU-DAY.
               05  TBL-NYU-DAY-OCC             OCCURS  31.
                 07  TBL-NYU-DAY-ST  PIC 9(02).
                 07  TBL-NYU-DAY-ED  PIC 9(02).
                 07  TBL-NYU-DAY-CD  PIC X(09).
      *
           03  WRK-ZENYYMM           PIC 9(06).
      *
           03  WRK-TUKI-YAKUKAN      PIC 9(02).
           03  WRK-DAY-YAKUKAN       PIC 9(02).
      *
           03  WRK-KEISAN            PIC 9(09).
           03  WRK-KEISAN2           PIC 9(03).
           03  WRK-KEISAN3           PIC 9(09).
           03  WRK-KEISAN4           PIC 9(09).
           03  WRK-KEISAN-TAIHI      PIC X(09).
      *
           03  WRK-KYUSEI-NYUGAIKBN  PIC X(01).
           03  WRK-KYUSEI-SRYKA      PIC X(02).
           03  WRK-KYUSEI-ZAINUM     PIC 9(08).
      *
           03  WRK-HENYMD            PIC X(08).
           03  WRK-TAIHI-XX          PIC X(02).
      *
           03  WRK-HENSYU-KINGAKU    PIC X(09).
           03  WRK-HENSYU-KINGAKU2   PIC X(18).
           03  WRK-HENSYU-KINGAKU3   PIC X(18).
      *
           03  WRK-RIHAIGAKU-SRYCD           PIC X(09).
           03  WRK-RIHAIGAKU-SRYCD2          PIC X(09).
           03  WRK-RIHAIGAKU-FSANTEIYMD      PIC X(08).
           03  WRK-RIHAIGAKU-SANTEI-AREA.
               05  WRK-RIHAIGAKU-SANTEI-FLG  PIC X(01).
               05  WRK-RIHAIGAKU-SANTEI-DAY  PIC 9(03)
                                             OCCURS 31.
      *
           03  WRK-YAKUKAN-SRYCD     PIC X(09).
      *
       01  WRK-RECE220-REC. 
           COPY    "CPRCF220.INC"  REPLACING   //RECE220//
                                   BY          //WRK-RECE220//.
      *
       01  WRK-RECE230-REC. 
           COPY    "CPRCF230.INC"  REPLACING   //RECE230//
                                   BY          //WRK-RECE230//.
      *
       01  WRK-RECE230XX-REC.
         02 WRK-RECE230XX-TBL       OCCURS 50.
           COPY    "CPRCF230.INC"  REPLACING   //RECE230//
                                   BY          //WRK-RECE230XX//.
      *
       01  WRK-RECE230XX2-REC.
         02 WRK-RECE230XX2-TBL      OCCURS 100.
           COPY    "CPRCF230.INC"  REPLACING   //RECE230//
                                   BY          //WRK-RECE230XX2//.
      *
       01  WRK-FREE-AREA.
           03  WRK-FREEINF                     OCCURS 49.         
               05  WK-FREE-YUKOKETA          PIC  9(03).
               05  WK-FREE-NAME              PIC  X(200).
      *
       01  WRK-FREE-AREA2.                                              
           03  WRK-FREEINF2                    OCCURS 50.               
               05  WK-FREE-YUKOKETA2         PIC  9(03).                
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
           03  CONS-YAKUKAN-COMMENT PIC X(09)  VALUE "120001610".
      *    »»ÄêÆü¥³¥á¥ó¥È
           03  CONS-SANTEI-COMMENT1  PIC X(09) VALUE "840000058".
           03  CONS-SANTEI-COMMENT2  PIC X(09) VALUE "840000065".
      *    ºßÂð·ì±ÕÆ©ÀÏ»ØÆ³´ÉÍýÎÁ
           03  CONS-TOSEKI          PIC X(09)  VALUE "114009310".
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü
           03  CONS-KYUSEI          PIC X(09)  VALUE "099800101".
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»Æü
           03  CONS-KYUSEI-END      PIC X(09)  VALUE "099800102".
      *    ¿´Âç¥ê¥Ï³«»ÏÆü
           03  CONS-SINDAI          PIC X(09) VALUE "099800111".
      *    ¸ÆµÛ´ï¥ê¥Ï³«»ÏÆü
           03  CONS-KOKYUKI         PIC X(09) VALUE "099800141".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï³«»ÏÆü
           03  CONS-NOUKE           PIC X(09) VALUE "099800121".
      *    ±¿Æ°´ï¥ê¥Ï³«»ÏÆü
           03  CONS-UNDOKI          PIC X(09) VALUE "099800131".
      *    ÆñÉÂ´µ¼Ô¥ê¥Ï³«»ÏÆü
           03  CONS-NANBYO          PIC X(09) VALUE "099800161".
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï³«»ÏÆü
           03  CONS-SYOGAI          PIC X(09) VALUE "099800171".
      *    ÀÝ¿©ÎÅË¡³«»ÏÆü
           03  CONS-SESSYOKU        PIC X(09) VALUE "099800151".
      *
      *    ¿´Âç¥ê¥Ï½ªÎ»Æü
           03  CONS-SINDAI-E        PIC X(09) VALUE "099800112".
      *    ¸ÆµÛ´ï¥ê¥Ï½ªÎ»Æü
           03  CONS-KOKYUKI-E       PIC X(09) VALUE "099800142".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï½ªÎ»Æü
           03  CONS-NOUKE-E         PIC X(09) VALUE "099800122".
      *    ±¿Æ°´ï¥ê¥Ï½ªÎ»Æü
           03  CONS-UNDOKI-E        PIC X(09) VALUE "099800132".
      *    ÆñÉÂ´µ¼Ô¥ê¥Ï½ªÎ»Æü
           03  CONS-NANBYO-E        PIC X(09) VALUE "099800162".
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï½ªÎ»Æü
           03  CONS-SYOGAI-E        PIC X(09) VALUE "099800172".
      *    ÀÝ¿©ÎÅË¡½ªÎ»Æü
           03  CONS-SESSYOKU-E      PIC X(09) VALUE "099800152".
      *
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-SINDAI-IGAKU1   PIC X(09) VALUE "180029910".
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-SINDAI-IGAKU2   PIC X(09) VALUE "180030010".
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-KOKYUKI-IGAKU1  PIC X(09) VALUE "180030510".
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-KOKYUKI-IGAKU2  PIC X(09) VALUE "180030610".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-NOUKE-IGAKU1    PIC X(09) VALUE "180030110".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-NOUKE-IGAKU2    PIC X(09) VALUE "180030210".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-UNDOKI-IGAKU1   PIC X(09) VALUE "180030310".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-UNDOKI-IGAKU2   PIC X(09) VALUE "180030410".
      *
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¼Â»Ü
           03  CONS-SINDAI-DUMMY    PIC X(09) VALUE "099800311".
      *    ¸ÆµÛ´ï¥ê¥Ï¼Â»Ü
           03  CONS-KOKYUKI-DUMMY   PIC X(09) VALUE "099800341".
      *    Ç¾·ì´É¼À´µ¥ê¥Ï¼Â»Ü
           03  CONS-NOUKE-DUMMY     PIC X(09) VALUE "099800321".
      *    ±¿Æ°´ï¥ê¥Ï¼Â»Ü
           03  CONS-UNDOKI-DUMMY    PIC X(09) VALUE "099800331".
      *
      *    ÌôºÞ´ÉÍý»ØÆ³ÎÁ¥Æ¡¼¥Ö¥ë¡Ê£²£°£°£¸Ç¯£´·î¿ÇÎÅÊ¬¡Á¡Ë
       01  TBL-YAKUKAN-AREA.
           03  FILLER               PIC X(09) VALUE "113010710".
           03  FILLER               PIC X(09) VALUE "113010810".
           03  FILLER               PIC X(09) VALUE "120001610".
      *
       01  TBL-YAKUKAN-R       REDEFINES   TBL-YAKUKAN-AREA.
            03  TBL-YAKUKAN    PIC X(09)   OCCURS   3.
      *
       01  TBL-YAKUKAN-MAX     PIC 9(02)   VALUE    3.
      *
      *
       01  TBL-HASSYO-INF.
           03  TBL-HASSYO-KAITATU-COMMENT   PIC 9(01).
           03  TBL-HASSYO-SINDAI-COMMENT    PIC 9(01).
           03  TBL-HASSYO-KOKYUKI-COMMENT   PIC 9(01).
           03  TBL-HASSYO-NOUKE-COMMENT     PIC 9(01).
           03  TBL-HASSYO-UNDOKI-COMMENT    PIC 9(01).
           03  TBL-HASSYO-NANBYO-COMMENT    PIC 9(01).
           03  TBL-HASSYO-SYOGAI-COMMENT    PIC 9(01).
           03  TBL-HASSYO-SESSYOKU-COMMENT  PIC 9(01).
      *
       01  TBL-ZAISIEN-INF.
           03  TBL-ZAISIEN-COMMENT          PIC 9(01).
      *
      *    »ÀÁÇ¡Ê¶â³Û¡¦¿ôÎÌ¡Ë¾ðÊó
       01  TBL-SANSO-INF.
           03  TBL-SANSO-INF2       OCCURS    10.
               05  TBL-SANSO-TEN           PIC 9(09)V9(02).
               05  TBL-SANSO-SRYSURYO      PIC 9(05)V9(03).
           03  TBL-SANSO-INF3.
               05  TBL-KASAN-SRYSURYO      PIC 9(05)V9(03).
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¾ðÊó
       01  TBL-RYOCOM-INF.
           03  TBL-RYOCOM-INF2                 OCCURS  31.
               05  TBL-RYOCOM-NYUINKBN         PIC  X(01).
               05  TBL-RYOCOM-AREA.
                 06  TBL-RYOCOM-MEISAISU       PIC  9(03).
                 06  TBL-RYOCOM-SRYCDTOTAL     PIC  9(14).
                 06  TBL-RYOCOM-INF3           OCCURS  30.
                   07  TBL-RYOCOM-IRYO-SRYCD   PIC  X(09).
                 06  TBL-RYOCOM-ADL-SRYCD      PIC  X(09).
                 06  TBL-RYOCOM-ADL-TEN        PIC  9(02).
                 06  TBL-RYOCOM-ADL-TEN-MOJI   PIC  X(04).
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¾ðÊó¡Ê³ºÅöÊ¬¤Î¤Þ¤È¤á¡Ë
       01  TBL-RYOCOM2-INF.
           03  TBL-RYOCOM2-INF2                OCCURS  31.
               05  TBL-RYOCOM2-AREA.
                 06  TBL-RYOCOM2-MEISAISU      PIC  9(03).
                 06  TBL-RYOCOM2-SRYCDTOTAL    PIC  9(14).
                 06  TBL-RYOCOM2-INF3          OCCURS  30.
                   07  TBL-RYOCOM2-IRYO-SRYCD  PIC  X(09).
                 06  TBL-RYOCOM2-ADL-SRYCD     PIC  X(09).
                 06  TBL-RYOCOM2-ADL-TEN       PIC  9(02).
                 06  TBL-RYOCOM2-ADL-TEN-MOJI  PIC  X(04).
               05  TBL-RYOCOM2-INF4            OCCURS  31.
                 06  TBL-RYOCOM2-DAY           PIC  9(01).
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
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
      *    ¿ÇÎÅ²ÊÌÜ¾ðÊó
           COPY    "CPSK1005.INC".
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
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
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
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    Á´³Ñ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSKANACHK.INC".
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¼èÆÀ¥µ¥Ö
           COPY    "CPORCSRYOCOM.INC".
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
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END  =   1
      *
           PERFORM 300-END-SEC
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        £Ä£Â¥ª¡¼¥×¥ó½èÍý
               PERFORM 120-DBOPEN-SEC
      *
      *        ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR1240"      TO  JOB-PGID
               MOVE    "¥³¥á¥ó¥ÈÊÔ½¸"  TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           PERFORM 990-RECE200-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
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
           MOVE    "RECE722"           TO  RECE2201PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE2201PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE2201PARA-HOSPNUM
           MOVE    "RECE732"           TO  RECE2301PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE2301PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE2301PARA-HOSPNUM
      *
           MOVE    "RECE72Y"           TO  RECE2209PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE2209PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE2209PARA-HOSPNUM
           MOVE    "RECE73Y"           TO  RECE2309PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE2309PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE2309PARA-HOSPNUM
      *
           OPEN    INPUT               RECE200-FILE
           OPEN    INPUT               RECE220-FILE
           OPEN    INPUT               RECE230-FILE
           OPEN    OUTPUT              RECE2201-FILE
           OPEN    OUTPUT              RECE2301-FILE
      *
           OPEN    OUTPUT              RECE2209-FILE
           CLOSE                       RECE2209-FILE
           OPEN    OUTPUT              RECE2309-FILE
           CLOSE                       RECE2309-FILE
      *
           OPEN    I-O                 RECE2209-FILE
           OPEN    I-O                 RECE2309-FILE
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
           INITIALIZE                      TBL-HASSYO-INF
           INITIALIZE                      TBL-ZAISIEN-INF
           INITIALIZE                      TBL-RYOCOM-INF
      *
           IF   RECE200-COMCNTFLG   =   "1"
      *
             INITIALIZE                      RECE220-REC
             MOVE    RECE200-KEY         TO  RECE220-KEY1
             MOVE    ZERO                TO  RECE220-ZAINUM
      *
             START   RECE220-FILE
                 KEY IS    >=   RECE220-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE220
                 NOT INVALID
                     PERFORM 990-RECE220-NEXT-SEC
             END-START
             PERFORM
               UNTIL  ( FLG-RECE220          =   1            )
               OR     ( RECE200-KEY      NOT =   RECE220-KEY1 )
      *
               EVALUATE      RECE220-SRYKBN
                   WHEN      "14"
      *            ¡Êºß»Ù±ç¤Ë´Ø¤¹¤ë¿ÇÎÅ¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE220-FLG-ZAISIEN-COMMENT   =  "1" OR "2"
                        MOVE    RECE220-REC    TO   WRK-RECE220-REC
                        EVALUATE  RECE220-FLG-ZAISIEN-COMMENT
                          WHEN      "1"
                            MOVE  1  TO  TBL-ZAISIEN-COMMENT
                          WHEN      "2"
                            MOVE  2  TO  TBL-ZAISIEN-COMMENT
                        END-EVALUATE
                      END-IF
                   WHEN      "40"
      *            ¡Ê²ðÃ£¸£°ú¤¢¤ê¡Ë
                      IF  RECE220-KAITATU-COMMENT    =  "1"
                        MOVE    1    TO    TBL-HASSYO-KAITATU-COMMENT
                      END-IF
                   WHEN      "80"
      *            ¡Ê¥ê¥Ï¥Ó¥ê¤¢¤ê¡Ë
                      IF  RECE220-SINDAI-COMMENT     =  "1"
                        MOVE    1    TO    TBL-HASSYO-SINDAI-COMMENT
                      END-IF
                      IF  RECE220-KOKYUKI-COMMENT    =  "1"
                        MOVE    1    TO    TBL-HASSYO-KOKYUKI-COMMENT
                      END-IF
                      IF  RECE220-NOUKE-COMMENT      =  "1"
                        MOVE    1    TO    TBL-HASSYO-NOUKE-COMMENT
                      END-IF
                      IF  RECE220-UNDOKI-COMMENT     =  "1"
                        MOVE    1    TO    TBL-HASSYO-UNDOKI-COMMENT
                      END-IF
                      IF  RECE220-NANBYO-COMMENT     =  "1"
                        MOVE    1    TO    TBL-HASSYO-NANBYO-COMMENT
                      END-IF
                      IF  RECE220-SYOGAI-COMMENT     =  "1"
                        MOVE    1    TO    TBL-HASSYO-SYOGAI-COMMENT
                      END-IF
                      IF  RECE220-SESSYOKU-COMMENT   =  "1"
                        MOVE    1    TO    TBL-HASSYO-SESSYOKU-COMMENT
                      END-IF
               END-EVALUATE
      *
               PERFORM 990-RECE220-NEXT-SEC
      *
             END-PERFORM
      *
           END-IF
      *
      *    ¥Ñ¥é¥á¡¼¥¿£°£´¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           INITIALIZE                      RECE220-REC
           MOVE    RECE200-KEY         TO  RECE220-KEY1
           MOVE    ZERO                TO  RECE220-ZAINUM
      *
           START   RECE220-FILE
               KEY IS    >=   RECE220-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE220
               NOT INVALID
                   PERFORM 990-RECE220-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE220           =   1            )
               OR     ( RECE200-KEY       NOT =   RECE220-KEY1 )
      *
               EVALUATE      RECE220-SRYKBN
                   WHEN      "11"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            ¡Ê½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë»»Äê²Ê¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYODOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE220-TENSOU-SEC
                      END-IF
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
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE220-TENSOU-SEC
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
      *            ¡Ê±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-OSIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE220-TENSOU-SEC
                      END-IF
                   WHEN      "50"
      *            ¡Ê¼ê½ÑÆü¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "60"
                   WHEN      "64"
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
                        IF    RECE220-RIHABIRI-COMMENT  NOT = SPACE
                          IF  RECE220-RIHABIRI-COMMENT      =  "1"
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                          ELSE
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHAIGAKU-COMMENT-SEC
                          END-IF
                        END-IF
                      END-IF
      *            ¡Ê¡Ê¥ê¥ÏÁª¡ËÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM        2001-RIHASEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                             PERFORM   2099-RECE220-TENSOU-SEC
                      END-IF
                   WHEN      "90"                                       
                   WHEN      "92"
                      EVALUATE    RECE220-ZAISKBKBN
                        WHEN   "2"
                          PERFORM   2001-NYU-COMMENT-SEC
                        WHEN   "6"
                          PERFORM   2001-KAKURI-HANTEI-SEC
                        WHEN   OTHER
                          PERFORM   2099-RECE220-TENSOU-SEC
                      END-EVALUATE
                   WHEN   OTHER
                      PERFORM       2099-RECE220-TENSOU-SEC
               END-EVALUATE
      *
               PERFORM 990-RECE220-NEXT-SEC
      *
           END-PERFORM
      *
      *    Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ¤ß¤Î¥³¥á¥ó¥È
      *
           IF   RECE200-NYUINRYOFLG   NOT =   SPACE
               PERFORM   2001-NYUINRYO-COMMENT-SEC
           END-IF
      *
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥È
           PERFORM   2001-HASSYO-HANTEI-SEC
      *
      *    ºß»Ù±ç¥³¥á¥ó¥È
           IF  TBL-ZAISIEN-COMMENT   NOT =  ZERO
                  PERFORM    2001-ZAISIEN-COMMENT-SEC
           END-IF
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¼èÆÀ
      *
           MOVE      ZERO     TO      FLG-RYOCOM
      *
           IF    RECE200-SRYYM   >=  "200610"
             INITIALIZE                    ORCSRYOCOM-AREA
             MOVE    RECE200-HOSPNUM   TO  LNK-RYOCOM-HOSPNUM
             MOVE    RECE200-PTID      TO  LNK-RYOCOM-PTID
             MOVE    RECE200-SRYYM     TO  LNK-RYOCOM-SANTEIYM
             CALL    "ORCSRYOCOM"          USING
                                           ORCSRYOCOM-AREA
                                           SPA-AREA
             IF      LNK-RYOCOM-RC    =   ZERO
               MOVE       1            TO  FLG-RYOCOM
               PERFORM  VARYING  RYO-IDX  FROM  1  BY  1
                         UNTIL   RYO-IDX   >   31
                 MOVE  LNK-RYOCOM-NYUINKBN(RYO-IDX)
                                 TO  TBL-RYOCOM-NYUINKBN(RYO-IDX)
                 MOVE  LNK-RYOCOM-MEISAISU(RYO-IDX)
                                 TO  TBL-RYOCOM-MEISAISU(RYO-IDX)
                 MOVE  LNK-RYOCOM-SRYCDTOTAL(RYO-IDX)
                                 TO  TBL-RYOCOM-SRYCDTOTAL(RYO-IDX)
                 PERFORM  VARYING  RYO-IDX2  FROM  1  BY  1
                           UNTIL   RYO-IDX2   >   30
                   MOVE  LNK-RYOCOM-IRYO-SRYCD(RYO-IDX RYO-IDX2)
                           TO  TBL-RYOCOM-IRYO-SRYCD(RYO-IDX RYO-IDX2)
                 END-PERFORM
                 MOVE  LNK-RYOCOM-ADL-SRYCD(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-SRYCD(RYO-IDX)
                 MOVE  LNK-RYOCOM-ADL-TEN(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-TEN(RYO-IDX)
                 MOVE  LNK-RYOCOM-ADL-TEN-MOJI(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-TEN-MOJI(RYO-IDX)
               END-PERFORM
             END-IF
           END-IF
      *
      *****º¹¹þÊÔ½¸£²********************
           PERFORM     250-SASHIKOMI2-SEC
      ***********************************
      *
           PERFORM 990-RECE200-READ-SEC
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
           INITIALIZE                      RECE2209-REC
           MOVE    RECE200-KEY         TO  RECE2209-KEY1
           MOVE    99999999            TO  RECE2209-ZAINUM
           MOVE    "90"                TO  RECE2209-SRYKBN
      ***  MOVE    RECE200-HKNCOMBI    TO  RECE2209-HKNCOMBI
      *
           MOVE    1                   TO  RECE2209-MEISAISU
      *
           WRITE   RECE2209-REC
           ADD     1                   TO  CNT-RECE2209
      *
           INITIALIZE                      RECE2309-REC
           MOVE    RECE200-KEY         TO  RECE2309-KEY1
           MOVE    99999999            TO  RECE2309-ZAINUM
           MOVE    "90"                TO  RECE2309-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE230XX-REC
           INITIALIZE                      WRK-RECE230XX-REC
           MOVE    "810000001"         TO  WRK-RECE230XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE230XX-SRYSURYO(1 1)
           EVALUATE   RECE200-NYUINRYOFLG
             WHEN     "1"
               MOVE     10             TO  WRK-RECE230XX-YUKOKETA(1 1)
               MOVE    "¼«Èñ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE230XX-NAME(1 1)
             WHEN     "2"
               MOVE     10             TO  WRK-RECE230XX-YUKOKETA(1 1)
               MOVE    "Ï«ºÒ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE230XX-NAME(1 1)
             WHEN     "3"
               MOVE     11             TO  WRK-RECE230XX-YUKOKETA(1 1)
               MOVE    "¼«ÇåÀÕ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE230XX-NAME(1 1)
             WHEN     "4"
               MOVE     10             TO  WRK-RECE230XX-YUKOKETA(1 1)
               MOVE    "¹ñÊÝ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE230XX-NAME(1 1)
             WHEN     "5"
               MOVE     10             TO  WRK-RECE230XX-YUKOKETA(1 1)
               MOVE    "¼ÒÊÝ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE230XX-NAME(1 1)
             WHEN     "8"
               MOVE     10             TO  WRK-RECE230XX-YUKOKETA(1 1)
               MOVE    "¸åÊÝ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE230XX-NAME(1 1)
           END-EVALUATE
      *
           MOVE    WRK-RECE230XX-SRYACT-TBL(1 1)
                                       TO  RECE2309-SRYACT-TBL(1)
           WRITE   RECE2309-REC
           ADD     1                   TO  CNT-RECE2309
      *
           .
       2001-NYUINRYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºß»Ù±ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-ZAISIEN-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE2209-REC
           MOVE    RECE200-KEY         TO  RECE2209-KEY1
           MOVE    99999965            TO  RECE2209-ZAINUM
           MOVE    WRK-RECE220-SRYKBN  TO  RECE2209-SRYKBN
      ***  MOVE    RECE200-HKNCOMBI    TO  RECE2209-HKNCOMBI
      *
           MOVE    1                   TO  RECE2209-MEISAISU
      *
           WRITE   RECE2209-REC
           ADD     1                   TO  CNT-RECE2209
      *
           INITIALIZE                      RECE2309-REC
           MOVE    RECE200-KEY         TO  RECE2309-KEY1
           MOVE    99999965            TO  RECE2309-ZAINUM
           MOVE    WRK-RECE220-SRYKBN  TO  RECE2309-SRYKBN
      *    ºß»Ù±ç¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE230XX-REC
           INITIALIZE                      WRK-RECE230XX-REC
           MOVE    "810000001"         TO  WRK-RECE230XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE230XX-SRYSURYO(1 1)
           MOVE     3                  TO  WRK-RECE230XX-YUKOKETA(1 1)
           EVALUATE  TBL-ZAISIEN-COMMENT
             WHEN      1
               MOVE    "ºß»Ù±ç"        TO  WRK-RECE230XX-NAME(1 1)
             WHEN      2
               MOVE    "ºß»ÙÉÂ"        TO  WRK-RECE230XX-NAME(1 1)
           END-EVALUATE
      *
           MOVE    WRK-RECE230XX-SRYACT-TBL(1 1)
                                       TO  RECE2309-SRYACT-TBL(1)
           WRITE   RECE2309-REC
           ADD     1                   TO  CNT-RECE2309
      *
           .
       2001-ZAISIEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HASSYO-HANTEI-SEC          SECTION.
      *
           IF   TBL-HASSYO-KAITATU-COMMENT   =  1
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-KYUSEI     TO   WRK-SRYCD
             PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SINDAI-COMMENT    =  1
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-SINDAI     TO   WRK-SRYCD
             MOVE      CONS-SINDAI-E   TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-KOKYUKI-COMMENT   =  1
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-KOKYUKI    TO   WRK-SRYCD
             MOVE      CONS-KOKYUKI-E  TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-NOUKE-COMMENT     =  1
             MOVE      1               TO   FLG-COMOKIKAE
             MOVE      CONS-NOUKE      TO   WRK-SRYCD
             MOVE      CONS-NOUKE-E    TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-UNDOKI-COMMENT    =  1
             MOVE      1                TO   FLG-COMOKIKAE
             MOVE      CONS-UNDOKI      TO   WRK-SRYCD
             MOVE      CONS-UNDOKI-E    TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-NANBYO-COMMENT    =  1
             MOVE      1                TO   FLG-COMOKIKAE
             MOVE      CONS-NANBYO      TO   WRK-SRYCD
             MOVE      CONS-NANBYO-E    TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SYOGAI-COMMENT    =  1
             MOVE      ZERO             TO   FLG-COMOKIKAE
             MOVE      CONS-SYOGAI      TO   WRK-SRYCD
             MOVE      CONS-SYOGAI-E    TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SESSYOKU-COMMENT  =  1
             MOVE      ZERO               TO   FLG-COMOKIKAE
             MOVE      CONS-SESSYOKU      TO   WRK-SRYCD
             MOVE      CONS-SESSYOKU-E    TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      *
           .
       2001-HASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î£±£¸£°Æü°ÊÆâ¤«È½Äê
      *****************************************************************
       2001-KYUSEIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     ZERO     TO     WRK-KYUSEIYMD
      *
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                      SANTEI-REC
           MOVE    RECE200-HOSPNUM     TO  SANTEI-HOSPNUM
           MOVE    RECE200-PTID        TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE200-HKNCOMBI    TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"        TO      MCP-TABLE
                MOVE    "key3"              TO      MCP-PATHNAME
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
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE200-SRYYM
                         AND  RECE200-SRYYM       <=  WRK-AFTER180(1:6)
                               MOVE    1    TO   FLG-KYUSEI-COMMENT
                               MOVE  WRK-FSANTEIYMD
                                            TO   WRK-KYUSEIYMD
                         END-IF
      *
                     END-IF
      *
                   END-PERFORM
      *
                   MOVE    "tbl_santei"        TO      MCP-TABLE
                   MOVE    "key3"              TO      MCP-PATHNAME
                   PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO      MCP-TABLE
           MOVE    "key3"              TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»Æü
      *
           IF      FLG-KYUSEI-COMMENT    =      1
      *       »»ÄêÍúÎò¸¡º÷
              INITIALIZE                      SANTEI-REC
              MOVE    RECE200-HOSPNUM     TO  SANTEI-HOSPNUM
              MOVE    RECE200-PTID        TO  SANTEI-PTID
              MOVE    CONS-KYUSEI-END     TO  SANTEI-SRYCD
              MOVE    ZERO                TO  SANTEI-NYUGAIKBN
              MOVE    ZERO                TO  SANTEI-SRYKA
              MOVE    RECE200-HKNCOMBI    TO  SANTEI-HKNCOMBINUM
              MOVE    SANTEI-REC          TO  MCPDATA-REC
              MOVE    "tbl_santei"        TO  MCP-TABLE
              MOVE    "key3"              TO  MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
      *
                   MOVE    "tbl_santei"        TO      MCP-TABLE
                   MOVE    "key3"              TO      MCP-PATHNAME
                   PERFORM        900-SANTEI-READ-SEC
      *
                   PERFORM   UNTIL ( FLG-SANTEI          =   1         )
                              OR   ( SANTEI-SRYYM  < WRK-KYUSEIYMD(1:6))
                              OR   ( FLG-KYUSEI-COMMENT2 =   1         )
      *
                      PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL ( IDX3                  <    1      )
                              OR   ( FLG-KYUSEI-COMMENT2   =    1      )
      *
                        IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                           MOVE   SANTEI-SRYYM   TO  WRK-FSANTEIYMD(1:6)
                           MOVE   IDX3           TO  WRK-FSANTEIYMD(7:2)
      *
                           IF   WRK-KYUSEIYMD        <  WRK-FSANTEIYMD
                           AND  WRK-FSANTEIYMD(1:6)  <  RECE200-SRYYM
                                  MOVE    1    TO   FLG-KYUSEI-COMMENT2
                           END-IF
      *
                        END-IF
      *
                      END-PERFORM
      *
                      MOVE    "tbl_santei"        TO      MCP-TABLE
                      MOVE    "key3"              TO      MCP-PATHNAME
                      PERFORM        900-SANTEI-READ-SEC
      *
                   END-PERFORM
      *
              END-IF
      *
              MOVE    "tbl_santei"        TO      MCP-TABLE
              MOVE    "key3"              TO      MCP-PATHNAME
              PERFORM   900-CLOSE-SEC
           END-IF
      *
           IF      FLG-KYUSEI-COMMENT    =      1
           AND     FLG-KYUSEI-COMMENT2   =      ZERO
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               MOVE       WRK-KYUSEIYMD(7:2)  TO  IDX16
               PERFORM    20011-HASSYO-COMMENT-SEC
           END-IF
      *
           .
       2001-KYUSEIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆüÅù¤Î¸¡º÷
      *****************************************************************
       20011-KYUSEIHASSYO-KENSAKU-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-KYUSEI-OK
           MOVE    SPACE                 TO  WRK-KYUSEI-NYUGAIKBN
           MOVE    SPACE                 TO  WRK-KYUSEI-SRYKA
           MOVE    ZERO                  TO  WRK-KYUSEI-ZAINUM
           MOVE    ZERO                  TO  FLG-COMOKIKAE2
      *
           MOVE    ZERO                  TO  IDZ
           INITIALIZE                        WRK-FREE-AREA
      *++++
           PERFORM   VARYING   IDX15    FROM   1   BY   1
                      UNTIL   (IDX15          >    2)
                       OR     (FLG-KYUSEI-OK  =    1)
      *
           INITIALIZE                        SRYACT-REC
           MOVE    RECE200-HOSPNUM       TO  SRY-HOSPNUM
           MOVE    IDX15                 TO  SRY-NYUGAIKBN
           MOVE    RECE200-PTID          TO  SRY-PTID
           MOVE    WRK-KYUSEIYMD(1:6)    TO  SRY-SRYYM
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (1)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (2)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (3)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (4)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key7"                TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"      TO     MCP-TABLE
               MOVE    "key7"            TO     MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT     =   1
                         OR     FLG-KYUSEI-OK  =   1
                 INITIALIZE                     SRYACCT-REC
                 MOVE    SRY-HOSPNUM      TO    ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN    TO    ACCT-NYUGAIKBN
                 MOVE    SRY-PTID         TO    ACCT-PTID
                 MOVE    SRY-SRYKA        TO    ACCT-SRYKA
                 MOVE    SRY-SRYYM        TO    ACCT-SRYYM
                 MOVE    SRY-ZAINUM       TO    ACCT-ZAINUM
                 MOVE    SRYACCT-REC      TO    MCPDATA-REC
                 MOVE    "tbl_sryacct"    TO    MCP-TABLE
                 MOVE    "key31"          TO    MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF     MCP-RC            =  ZERO
                    MOVE    "tbl_sryacct"    TO    MCP-TABLE
                    MOVE    "key31"          TO    MCP-PATHNAME
                    PERFORM   900-DBFETCH-SEC
                    IF      MCP-RC        =       ZERO
                        MOVE    MCPDATA-REC  TO  SRYACCT-REC
                        MOVE    WRK-KYUSEIYMD(7:2)  TO  DAY-IDX2
                        IF  ( ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO )
                        AND ( ACCT-HKNCOMBI =  RECE200-HKNCOMBI )
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
                 MOVE    "tbl_sryacct"    TO    MCP-TABLE
                 MOVE    "key31"          TO    MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"          TO      MCP-TABLE
                 MOVE    "key7"                TO      MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_sryact"          TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           END-PERFORM
      *++++
           IF      WRK-KYUSEI-ZAINUM    NOT =    ZERO
              INITIALIZE                           SRYACT-REC
              MOVE    RECE200-HOSPNUM        TO    SRY-HOSPNUM
              MOVE    WRK-KYUSEI-NYUGAIKBN   TO    SRY-NYUGAIKBN
              MOVE    RECE200-PTID           TO    SRY-PTID
              MOVE    WRK-KYUSEI-SRYKA       TO    SRY-SRYKA
              MOVE    WRK-KYUSEIYMD(1:6)     TO    SRY-SRYYM
              MOVE    WRK-KYUSEI-ZAINUM      TO    SRY-ZAINUM
              MOVE    SRYACT-REC             TO    MCPDATA-REC
              MOVE    "tbl_sryact"           TO    MCP-TABLE
              MOVE    "key2"                 TO    MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF     MCP-RC            =  ZERO
                 MOVE    "tbl_sryact"        TO    MCP-TABLE
                 MOVE    "key2"              TO    MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
                 PERFORM  UNTIL   FLG-SRYACT     =   1
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12             >    5   )  OR
                                    (SRY-SRYCD(IDX12)  =   SPACE)
                      IF   (SRY-SRYCD(IDX12)(1:1)     =   "8"  )  OR
                           (SRY-SRYCD(IDX12)(1:3)     =   "008")
                       IF   SRY-INPUTNUM(IDX12)   NOT =   ZERO
                         INITIALIZE                      PTCOM-REC
                         MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
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
                       ELSE
                         PERFORM      900-TENSU-TBL-SEC
                         ADD     1           TO  IDZ
                         MOVE    TNS-NAME    TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          EVALUATE  SRY-SRYCD(IDX12)
                            WHEN     "840000042"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    1    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000101"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    2    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                          END-EVALUATE
                       END-IF
                      END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO    MCP-TABLE
                   MOVE    "key2"            TO    MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
                 END-PERFORM
              END-IF
              INITIALIZE                     MCPAREA
              MOVE    "tbl_sryact"      TO    MCP-TABLE
              MOVE    "key2"            TO    MCP-PATHNAME
              PERFORM   900-CLOSE-SEC
           END-IF
      *
           .
       20011-KYUSEIHASSYO-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-HASSYO-COMMENT-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO        TO  IDX-RECE230
           MOVE    SPACE       TO  WRK-RECE230XX-REC
           INITIALIZE              WRK-RECE230XX-REC
      *
           ADD     1           TO  IDX-RECE230
           MOVE    "810000001" TO  WRK-RECE230XX-SRYCD(IDX-RECE230 1)
           MOVE    ZERO        TO  WRK-RECE230XX-SRYSURYO(IDX-RECE230 1)
      *
           EVALUATE      WRK-SRYCD
             WHEN       CONS-KYUSEI
               MOVE    16   TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
               MOVE    "Ç¾·ì´É¼À´µµÞÀ­È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
               MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(21:4)
               MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(27:4)
               COMPUTE WRK-ZAINUM    =    99998000
                                     +   (IDX16 + 31)
               MOVE    "40" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI
               MOVE    20   TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
               MOVE    "¿´Âç·ì´É¼À´µ¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
               MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(29:4)
               MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(35:4)
               IF      FLG-RIHA      =  1
                 MOVE    99998201      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998200
                                       +   (IDX16 + 31)
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    18  TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                   MOVE    "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(25:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(31:4)
                 WHEN       1
                   MOVE    18  TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                   MOVE    "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(25:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(31:4)
                 WHEN       2
                   MOVE    20  TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                   MOVE    "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(29:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(35:4)
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 MOVE    99998401      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998400
                                       +   (IDX16 + 31)
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    15  TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                   MOVE    "±¿Æ°´ï¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(19:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(25:4)
                 WHEN       1
                   MOVE    15  TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                   MOVE    "±¿Æ°´ï¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(19:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(25:4)
                 WHEN       2
                   MOVE    17  TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                   MOVE    "±¿Æ°´ï¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(23:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(29:4)
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 MOVE    99998601      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998600
                                       +   (IDX16 + 31)
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI
               MOVE    17   TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
               MOVE    "¸ÆµÛ´ï¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
               MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(23:4)
               MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(29:4)
               IF      FLG-RIHA      =  1
                 MOVE    99998801      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998800
                                       +   (IDX16 + 31)
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SESSYOKU
               MOVE    18   TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
               MOVE    "ÀÝ¿©µ¡Ç½ÎÅË¡¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
               MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(25:4)
               MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(31:4)
               IF      FLG-RIHA      =  1
                 MOVE    99999001      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99999000
                                       +   (IDX16 + 31)
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    16  TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                   MOVE    "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(21:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(27:4)
                 WHEN       1
                   MOVE    16  TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                   MOVE    "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(21:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(27:4)
                 WHEN       2
                   MOVE    18  TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                   MOVE    "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(25:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(31:4)
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 MOVE    99999201      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99999200
                                       +   (IDX16 + 31)
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SYOGAI
               MOVE    18   TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
               MOVE    "¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
               MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(25:4)
               MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE230XX-NAME(IDX-RECE230 1)(31:4)
               IF      FLG-RIHA      =  1
                 MOVE    99999401      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99999400
                                       +   (IDX16 + 31)
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
           END-EVALUATE
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE230
                MOVE   "810000001"
                               TO  WRK-RECE230XX-SRYCD(IDX-RECE230 1)
                MOVE   ZERO    TO  WRK-RECE230XX-SRYSURYO(IDX-RECE230 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
           END-PERFORM
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE230
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE230
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE230  =    1
                INITIALIZE                       RECE2309-REC
                MOVE    RECE200-KEY          TO  RECE2309-KEY1
                MOVE    WRK-ZAINUM           TO  RECE2309-ZAINUM
                MOVE    WRK-SRYKBN           TO  RECE2309-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE2309-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE230  =    1)
                   ADD         1             TO    IDX-RECE230
                   IF  WRK-RECE230XX-SRYCD(IDX-RECE230 1) = SPACE
                       MOVE    1             TO    FLG-WRKRECE230
                   ELSE
                       MOVE  WRK-RECE230XX-SRYACT-TBL (IDX-RECE230 1)
                                        TO   RECE2309-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE2309-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE2309-REC
                   ADD     1            TO   CNT-RECE2309
                END-IF
           END-PERFORM
      *
      *
           INITIALIZE                      RECE2209-REC
           MOVE    RECE200-KEY         TO  RECE2209-KEY1
           MOVE    WRK-ZAINUM          TO  RECE2209-ZAINUM
           MOVE    WRK-SRYKBN          TO  RECE2209-SRYKBN
      ***  MOVE    RECE200-HKNCOMBI    TO  RECE2209-HKNCOMBI
           MOVE    WRK-CNT-MEISAI      TO  RECE2209-MEISAISU
      *
           WRITE   RECE2209-REC
           ADD     1                   TO  CNT-RECE2209
      *
           .
       20011-HASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥êÈ¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       20011-RIHABIRIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     SPACE    TO     WRK-RIHASANTEI-END
      *
      *    ¥ê¥Ï½ªÎ»Æü
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                      SANTEI-REC
           MOVE    RECE200-HOSPNUM     TO  SANTEI-HOSPNUM
           MOVE    RECE200-PTID        TO  SANTEI-PTID
           MOVE    WRK-SRYCD2          TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE200-HKNCOMBI    TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF    SANTEI-SRYYM  <  RECE200-SRYYM
                   PERFORM   VARYING   IDX3   FROM  31  BY  -1
                              UNTIL  ( IDX3                <   1 )
                                OR   ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                        MOVE  SANTEI-SRYYM  TO  WRK-RIHASANTEI-END(1:6)
                        MOVE  IDX3          TO  WRK-RIHASANTEI-END(7:2)
      *
                        MOVE  1             TO  FLG-KYUSEI-COMMENT2
      *
                     END-IF
      *
                   END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      ********
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     ZERO     TO     WRK-KYUSEIYMD
           INITIALIZE               TBL-RIHASANTEI-DAY
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD2
           MOVE     ZERO     TO     WRK-RIHASANTEI41-FLG
      *
      *    ¥ê¥Ï³«»ÏÆü
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                      SANTEI-REC
           MOVE    RECE200-HOSPNUM     TO  SANTEI-HOSPNUM
           MOVE    RECE200-PTID        TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE200-HKNCOMBI    TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE200-SRYYM
                  IF   SANTEI-SRYYM  =  RECE200-SRYYM
                   PERFORM  VARYING   IDX3   FROM   1  BY  1
                             UNTIL    IDX3    >    31
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
      ****               MOVE   SANTEI-SRYYM   TO  WRK-KYUSEIYMD(1:6)
      ****               MOVE   "00"           TO  WRK-KYUSEIYMD(7:2)
                         MOVE   1     TO  TB-RIHASANTEI-DAY(IDX3)
      *
                         MOVE   1     TO  FLG-KYUSEI-COMMENT
      *
                         IF    (SANTEI-SRYYM = "200604")
                         AND   (IDX3         =  1      )
                            MOVE    1    TO   WRK-RIHASANTEI41-FLG
                         END-IF
                         IF    (SANTEI-SRYYM  NOT = "200604")
                         AND   (IDX3              =  1      )
                            MOVE   1     TO   FLG-KYUSEI-COMMENT2
                         END-IF
      *
                     END-IF
      *
                   END-PERFORM
                  ELSE
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3                <   1 )
                               OR    ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
      ****               MOVE   SANTEI-SRYYM   TO  WRK-KYUSEIYMD(1:6)
      ****               MOVE   IDX3           TO  WRK-KYUSEIYMD(7:2)
      *
                       MOVE   SANTEI-SRYYM
                                        TO  WRK-RIHASANTEI-YMD2(1:6)
                       MOVE   IDX3      TO  WRK-RIHASANTEI-YMD2(7:2)
      *
                       IF  (WRK-RIHASANTEI-END  = SPACE              )
                       OR  (WRK-RIHASANTEI-END <= WRK-RIHASANTEI-YMD2)
      *
                         MOVE   SANTEI-SRYYM
                                          TO  WRK-RIHASANTEI-YMD(1:6)
                         MOVE   IDX3      TO  WRK-RIHASANTEI-YMD(7:2)
      *
                         MOVE   1         TO  FLG-KYUSEI-COMMENT
      *
                       END-IF
      *
                       MOVE     1         TO  FLG-KYUSEI-COMMENT2
      *
                       IF  (WRK-RIHASANTEI-END  = SPACE              )
                       OR  (WRK-RIHASANTEI-END <= WRK-RIHASANTEI-YMD2)
      *
                         IF    (SANTEI-SRYYM = "200604")
                         AND   (IDX3         =  1      )
                            MOVE   ZERO  TO   FLG-KYUSEI-COMMENT2
                         END-IF
                         IF     WRK-RIHASANTEI41-FLG  =  1
                            MOVE   ZERO  TO   TB-RIHASANTEI-DAY(1)
                         END-IF
      *
                       END-IF
      *
                     END-IF
      *
                   END-PERFORM
                  END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           IF      FLG-KYUSEI-COMMENT    =      1
               IF  WRK-RIHASANTEI-YMD  NOT = SPACE
                     MOVE   1      TO  FLG-RIHA
                     MOVE   WRK-RIHASANTEI-YMD
                                   TO  WRK-KYUSEIYMD
                     PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                     PERFORM    20011-HASSYO-COMMENT-SEC
               END-IF
               PERFORM   VARYING   IDX16    FROM   1   BY   1
                          UNTIL    IDX16      >   31
                 IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
                     MOVE   2      TO  FLG-RIHA
                     MOVE   RECE200-SRYYM
                                   TO  WRK-KYUSEIYMD(1:6)
                     MOVE   IDX16  TO  WRK-KYUSEIYMD(7:2)
                     PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                     PERFORM    20011-HASSYO-COMMENT-SEC
                 END-IF
               END-PERFORM
           END-IF
      *
           .
       20011-RIHABIRIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SYODOU-HANTEI-SEC          SECTION.
      *
           IF      RECE220-FLG-SYODOU-COMMENT   =    "1"
                MOVE       99       TO     FLG-COMMENT
                PERFORM    20011-SYODOU-COMMENT-SEC
           END-IF
      *
           .
       2001-SYODOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-SYODOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                       >   10  )
                       OR     (RECE220-SYODOU-SRYKA(IDX) =  SPACE)
      *
             ADD      1                  TO  IDZ
             MOVE    "Ê£½é»»Äê¿ÇÎÅ²Ê¡§"  TO  WK-FREE-NAME2(IDZ)(1:16)
      *      ¿ÇÎÅ²Ê¤Î¼èÆÀ
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE220-SYODOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE200-SRYYM       TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI      =   ZERO
                  MOVE  SYS-1005-SRYKANAME(1:40)
                                         TO  WK-FREE-NAME2(IDZ)(17:40)
               END-IF
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
             PERFORM   VARYING   IDW   FROM   200   BY  -1
                        UNTIL    IDW   <   1
                IF   WK-FREE-NAME2(IDZ)(IDW:1)  NOT =  SPACE
                     COMPUTE WK-FREE-YUKOKETA2(IDZ) =  IDW  /   2
                     MOVE    1   TO    IDW
                END-IF
             END-PERFORM
      *
           END-PERFORM
      *
           PERFORM  200111-RECE230-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE230
           MOVE    SPACE                TO  WRK-RECE230XX-REC
           INITIALIZE                       WRK-RECE230XX-REC
      *
           MOVE    RECE220-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE230XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE230XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE230XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SYODOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-YAKUKANSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE220-FLG-YAKUKAN-COMMENT   =    "1"
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
           IF      RECE220-FLG-NYUEIYO-COMMENT   =    "1"
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
           IF      RECE220-FLG-HOUMON-COMMENT   =    "1"
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
           IF      RECE220-FLG-SYOBYO-COMMENT   =    "1"
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
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
           IF      RECE220-FLG-RYOYO-COMMENT    =    "1"
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
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
           IF      RECE220-FLG-TAIIN-COMMENT   =    "1"
      *
             MOVE      ZERO            TO      FLG-YAKUKAN-COMMENT
      *
             IF    RECE200-SRYYM    >=  "200804"
               PERFORM   VARYING   IDX6   FROM    1   BY    1
                          UNTIL  ( IDX6   >   TBL-YAKUKAN-MAX       )
                            OR   ( FLG-YAKUKAN-COMMENT   =    1     )
                  MOVE   TBL-YAKUKAN(IDX6)     TO  WRK-YAKUKAN-SRYCD
                  PERFORM  2001X-TAIIN-HANTEI-TOUGETU-SEC
               END-PERFORM
             ELSE
                  MOVE   CONS-YAKUKAN-COMMENT  TO  WRK-YAKUKAN-SRYCD
                  PERFORM  2001X-TAIIN-HANTEI-TOUGETU-SEC
             END-IF
      *
             IF      FLG-YAKUKAN-COMMENT   =   ZERO
      *
               MOVE      ZERO            TO      WRK-ZENYYMM
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    RECE220-SRYYM       TO  LNK-DAY6-KIJUN(1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN(7:2)
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE     -1                 TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-ZENYYMM
      *
               IF    RECE200-SRYYM    >=  "200804"
                 PERFORM   VARYING   IDX6   FROM    1   BY    1
                            UNTIL    IDX6   >   TBL-YAKUKAN-MAX
                    MOVE   TBL-YAKUKAN(IDX6)     TO  WRK-YAKUKAN-SRYCD
                    PERFORM  2001Y-TAIIN-HANTEI-ZENGETU-SEC
                 END-PERFORM
               ELSE
                    MOVE   CONS-YAKUKAN-COMMENT  TO  WRK-YAKUKAN-SRYCD
                    PERFORM  2001Y-TAIIN-HANTEI-ZENGETU-SEC
               END-IF
      *
             END-IF
      *
             MOVE       99                TO     FLG-COMMENT
             IF    WRK-DAY-YAKUKAN    NOT   =   ZERO
                MOVE    2                 TO     FLG-HANTEI
                MOVE    WRK-ZENYYMM(5:2)  TO     WRK-TUKI-YAKUKAN
             ELSE
                MOVE    1                 TO     FLG-HANTEI
             END-IF
             PERFORM    20011-TAIIN-COMMENT-SEC
      *
           END-IF
      *
           .
       2001-TAIIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Åö·î¤ËÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤¢¤ë¤«¥Á¥§¥Ã¥¯
      *****************************************************************
       2001X-TAIIN-HANTEI-TOUGETU-SEC          SECTION.
      *
           INITIALIZE                    SRYACT-REC
           MOVE    RECE220-HOSPNUM   TO  SRY-HOSPNUM
           MOVE    RECE220-NYUGAIKBN TO  SRY-NYUGAIKBN
           MOVE    RECE220-PTID      TO  SRY-PTID
           MOVE    RECE220-SRYYM     TO  SRY-SRYYM
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (1)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (2)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (3)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (4)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
      *
           IF      MCP-RC               =       ZERO
             MOVE    "tbl_sryact"    TO  MCP-TABLE
             MOVE    "key7"          TO  MCP-PATHNAME
             PERFORM  900-SRYACT-READ-SEC
             IF      FLG-SRYACT  =  ZERO
                   MOVE    1         TO  FLG-YAKUKAN-COMMENT
             END-IF
           END-IF
      *
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2001X-TAIIN-HANTEI-TOUGETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á°·î¤ËÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤¢¤ë¤«¥Á¥§¥Ã¥¯
      *****************************************************************
       2001Y-TAIIN-HANTEI-ZENGETU-SEC          SECTION.
      *
           INITIALIZE                    SRYACT-REC
           MOVE    RECE220-HOSPNUM   TO  SRY-HOSPNUM
           MOVE    RECE220-NYUGAIKBN TO  SRY-NYUGAIKBN
           MOVE    RECE220-PTID      TO  SRY-PTID
           MOVE    WRK-ZENYYMM       TO  SRY-SRYYM
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (1)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (2)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (3)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (4)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
      *
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO   MCP-TABLE
               MOVE    "key7"           TO   MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
      *
               PERFORM  UNTIL   FLG-SRYACT   =  1
      *
                 INITIALIZE                  SRYACCT-REC
                 MOVE    SRY-HOSPNUM    TO   ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN  TO   ACCT-NYUGAIKBN
                 MOVE    SRY-PTID       TO   ACCT-PTID
                 MOVE    SRY-SRYKA      TO   ACCT-SRYKA
                 MOVE    SRY-SRYYM      TO   ACCT-SRYYM
                 MOVE    SRY-ZAINUM     TO   ACCT-ZAINUM
                 MOVE    SRYACCT-REC    TO   MCPDATA-REC
                 MOVE    "tbl_sryacct"  TO   MCP-TABLE
                 MOVE    "key31"        TO   MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
      *
                 IF  MCP-RC               =  ZERO
                     MOVE    "tbl_sryacct"  TO  MCP-TABLE
                     MOVE    "key31"        TO  MCP-PATHNAME
                     PERFORM   900-DBFETCH-SEC
                     IF      MCP-RC       =  ZERO
                       MOVE    MCPDATA-REC  TO  SRYACCT-REC
                       PERFORM   VARYING   IDX  FROM   1   BY   1
                                  UNTIL    IDX    >   31
                         IF    ACCT-DAY(1 IDX)  NOT =  ZERO
                            IF    IDX   >   WRK-DAY-YAKUKAN
                                MOVE  IDX   TO  WRK-DAY-YAKUKAN
                            END-IF
                         END-IF
                       END-PERFORM
                     END-IF
                 END-IF
      *
                 INITIALIZE                  MCPAREA
                 MOVE    "tbl_sryacct"  TO   MCP-TABLE
                 MOVE    "key31"        TO   MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"   TO   MCP-TABLE
                 MOVE    "key7"         TO   MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
      *
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"      TO      MCP-TABLE
           MOVE    "key7"            TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2001Y-TAIIN-HANTEI-ZENGETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOUSIJI-HANTEI-SEC          SECTION.
      *
           IF     RECE220-FLG-HOUSIJI-COMMENT   =    "1"
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
           IF   RECE220-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE220-KAISU(1 IDX3)  NOT =  ZERO
                        MOVE  IDX3  TO  WRK-DAY
                        MOVE  31    TO  IDX3
                 END-IF
             END-PERFORM
      *
             MOVE     ZERO     TO     FLG-HINKAI-COMMENT
             MOVE     ZERO     TO     WRK-TOSEKIYMD
      *
      *      »»ÄêÍúÎò¸¡º÷
             INITIALIZE                      SANTEI-REC
             MOVE    RECE220-HOSPNUM     TO  SANTEI-HOSPNUM
             MOVE    RECE220-PTID        TO  SANTEI-PTID
             MOVE    CONS-TOSEKI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                    MOVE    "tbl_santei"        TO  MCP-TABLE
                    MOVE    "key3"              TO  MCP-PATHNAME
                    PERFORM        900-SANTEI-READ-SEC
                    PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                     ( FLG-HINKAI-COMMENT =   1 )
      *
                      IF   SANTEI-SRYYM  <=  RECE220-SRYYM
                       MOVE   SANTEI-SRYYM     TO  WRK-FSANTEIYMD(1:6)
                       MOVE   SANTEI-MSANTEID  TO  WRK-FSANTEIYMD(7:2)
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
                       MOVE    RECE220-SRYYM   TO  WRK-HINKAIYMD(1:6)
                       MOVE    WRK-DAY         TO  WRK-HINKAIYMD(7:2)
      *
                       IF     WRK-DAY      NOT  =  ZERO
                       AND    WRK-FSANTEIYMD   <=  WRK-HINKAIYMD
                       AND    WRK-HINKAIYMD    <   WRK-AFTER2
                             MOVE    1    TO   FLG-HINKAI-COMMENT
                             MOVE  WRK-FSANTEIYMD
                                          TO   WRK-TOSEKIYMD
                       END-IF
                      END-IF
      * 
                      MOVE    "tbl_santei"     TO  MCP-TABLE
                      MOVE    "key3"           TO  MCP-PATHNAME
                      PERFORM        900-SANTEI-READ-SEC
                    END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
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
             PERFORM   200111-RECE230-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE230
             MOVE    SPACE                TO  WRK-RECE230XX-REC
             INITIALIZE                       WRK-RECE230XX-REC
      *      ¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE220-MEISAISU  +  1
      *
             MOVE    "810000001"
                           TO  WRK-RECE230XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO  TO  WRK-RECE230XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    16    TO  WRK-RECE230XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "½é²ó»ØÆ³´ÉÍý»»ÄêÆü¡¡¡¡¡¡·î¡¡¡¡Æü"
                           TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)(21:4)
             MOVE    WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)(27:4)
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HINKAI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-OSIN-HANTEI-SEC          SECTION.
      *
           IF     RECE220-FLG-OSIN-COMMENT   =    "1"
               MOVE       6       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-OSIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆü¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SHUJUTSU-HANTEI-SEC          SECTION.
      *
           IF     RECE220-FLG-SHUJUTU-COMMENT   =    SPACE
                MOVE       1       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE220-TENSOU-SEC
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
           IF     RECE220-SAFLG   =    "2"
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE220-TENSOU-SEC
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
           PERFORM   200111-RECE230-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE230
           MOVE    SPACE                TO  WRK-RECE230XX-REC
           INITIALIZE                       WRK-RECE230XX-REC
      *
           PERFORM
               UNTIL  ( FLG-RECE230           =   1               )
               OR     ( RECE220-KEY1      NOT =   RECE230-KEY1    )
               OR     ( RECE220-ZAINUM    NOT =   RECE230-ZAINUM  )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE230-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE230
                    MOVE   RECE230-SRYACT-TBL(IDX)
                           TO   WRK-RECE230XX-SRYACT-TBL(IDX-RECE230 1)
      *++++++
                    IF    IDX-RECE230  =  RECE220-MEISAISU
      *
                      IF  WRK-RECE230XX-SRYCD(IDX-RECE230 1)
                                          = "810000001"
                      OR  WRK-RECE230XX-SRYCD(IDX-RECE230 1)(1:3)
                                          = "008"
      *
                        COMPUTE   IDV  =
                         (WRK-RECE230XX-YUKOKETA(IDX-RECE230 1) * 2) + 1
      *
                        IF   WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                                                   <=  36
                          MOVE    "¡¡¡ß"   TO
                           WRK-RECE230XX-INPUTCOMENT(IDX-RECE230 1)
                                                               (IDV:4)
                        END-IF
                        IF   WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                                                   <=  96
                          MOVE    "¡¡¡ß"   TO
                           WRK-RECE230XX-NAME(IDX-RECE230 1)(IDV:4)
                        END-IF
      *
                        COMPUTE   IDV  =  IDV  +  4
      *
                        MOVE    RECE220-ZAIKAISU   TO   DAY-IDX2
                        PERFORM 20011-KAISU-HENSYU-SEC
                        IF   WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                                                   <=  36
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE230XX-INPUTCOMENT(IDX-RECE230 1)
                                                       (IDV:4)
                        END-IF
                        IF   WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                                                   <=  96
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE230XX-NAME(IDX-RECE230 1)(IDV:4)
                          COMPUTE
                             WRK-RECE230XX-YUKOKETA(IDX-RECE230 1) =
                             WRK-RECE230XX-YUKOKETA(IDX-RECE230 1) +
                                  4
                        END-IF
      *
                      END-IF
      *
                    END-IF
      *++++++
               END-PERFORM
      *
               PERFORM 990-RECE230-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE230
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE230
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE230  =    1
                    INITIALIZE                        RECE2309-REC
                    MOVE    WRK-RECE230-KEY       TO  RECE2309-KEY
                    ADD     1                     TO  WRK-RENNUM
                    MOVE    WRK-RENNUM            TO  RECE2309-RENNUM
                    MOVE    WRK-RECE230-PTNUM     TO  RECE2309-PTNUM
                    MOVE    WRK-RECE230-SRYKA     TO  RECE2309-SRYKA
                    MOVE    WRK-RECE230-SRYKBN    TO  RECE2309-SRYKBN
                    MOVE    WRK-RECE230-SRYSYUKBN TO  RECE2309-SRYSYUKBN
                    MOVE    WRK-RECE230-JIHIMONEY TO  RECE2309-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE230   =   1)
                       ADD         1             TO    IDX-RECE230
                       IF  WRK-RECE230XX-SRYCD(IDX-RECE230 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE230
                       ELSE
                           MOVE  WRK-RECE230XX-SRYACT-TBL(IDX-RECE230 1)
                                            TO  RECE2309-SRYACT-TBL(IDX)
                           ADD   1          TO  WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE2309-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE2309-REC
                       ADD     1            TO   CNT-RECE2309
                    END-IF
           END-PERFORM
      *
           MOVE    RECE220-REC       TO      RECE2209-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE2209-MEISAISU
           WRITE   RECE2209-REC
           ADD     1                 TO      CNT-RECE2209
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
           IF      RECE220-FLG-CTMRI-COMMENT   =    "1"
                MOVE       2       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE220-TENSOU-SEC
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
           IF      RECE220-FLG-HOUSEI-COMMENT   =    "1"
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
           MOVE       RECE220-REC      TO  RECE2209-REC
           EVALUATE   FLG-HANTEI
             WHEN       1
                ADD     1              TO  RECE2209-MEISAISU
             WHEN       2
                ADD     2              TO  RECE2209-MEISAISU
           END-EVALUATE
           WRITE      RECE2209-REC
           ADD     1                   TO  CNT-RECE2209
      *
           INITIALIZE                      RECE230-REC
           MOVE    RECE220-KEY1        TO  RECE230-KEY1
           MOVE    RECE220-ZAINUM      TO  RECE230-ZAINUM
           START   RECE230-FILE
                 KEY IS    >=   RECE230-KEY
                 INVALID
                     MOVE    1         TO  FLG-RECE230
                 NOT INVALID
                     PERFORM 990-RECE230-NEXT-SEC
           END-START
      *
           MOVE    RECE230-REC         TO  RECE2309-REC
           EVALUATE   FLG-HANTEI
             WHEN       1
               MOVE    RECE230-SRYACT-TBL(1) TO  RECE2309-SRYACT-TBL(1)
               MOVE    RECE230-SRYACT-TBL(2) TO  RECE2309-SRYACT-TBL(3)
               MOVE    RECE230-SRYACT-TBL(3) TO  RECE2309-SRYACT-TBL(4)
               MOVE    RECE230-SRYACT-TBL(4) TO  RECE2309-SRYACT-TBL(5)
      *
               MOVE    SPACE                TO  RECE2309-SRYACT-TBL(2)
               INITIALIZE                       RECE2309-SRYACT-TBL(2)
               MOVE    "810000001"          TO  RECE2309-SRYCD(2)
               MOVE    ZERO                 TO  RECE2309-SRYSURYO(2)
               PERFORM  200111-SANTEI-DAY-SEC
               PERFORM  200111-TAIIN-COMMENT-HENSYU-SEC
             WHEN       2
               MOVE    RECE230-SRYACT-TBL(1) TO  RECE2309-SRYACT-TBL(1)
               MOVE    RECE230-SRYACT-TBL(2) TO  RECE2309-SRYACT-TBL(4)
               MOVE    RECE230-SRYACT-TBL(3) TO  RECE2309-SRYACT-TBL(5)
      *
               MOVE    SPACE                TO  RECE2309-SRYACT-TBL(2)
               INITIALIZE                       RECE2309-SRYACT-TBL(2)
               MOVE    "810000001"          TO  RECE2309-SRYCD(2)
               MOVE    ZERO                 TO  RECE2309-SRYSURYO(2)
               PERFORM  200111-SANTEI-DAY-SEC
               PERFORM  200111-TAIIN-COMMENT-HENSYU-SEC
      *
               MOVE    SPACE                TO  RECE2309-SRYACT-TBL(3)
               INITIALIZE                       RECE2309-SRYACT-TBL(3)
               MOVE    "810000001"          TO  RECE2309-SRYCD(3)
               MOVE    ZERO                 TO  RECE2309-SRYSURYO(3)
               MOVE    16                   TO  RECE2309-YUKOKETA(3)
               MOVE    "ÌôºÞ´ÉÍý»ØÆ³ÎÁ»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü"
                                            TO  RECE2309-NAME(3)
               MOVE     WRK-TUKI-YAKUKAN    TO  DAY-IDX
               MOVE     TBL-DAY(DAY-IDX)    TO  RECE2309-NAME(3)(21:4)
               MOVE     WRK-DAY-YAKUKAN     TO  DAY-IDX
               MOVE     TBL-DAY(DAY-IDX)    TO  RECE2309-NAME(3)(27:4)
           END-EVALUATE
           WRITE    RECE2309-REC
           ADD      1                   TO  CNT-RECE2309
      *
           .
       20011-TAIIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÆüÊÔ½¸
      *****************************************************************
       200111-TAIIN-COMMENT-HENSYU-SEC          SECTION.
      *
           MOVE     TB-SANTEI-DAY(1)    TO  DAY-IDX
           IF       DAY-IDX  <  10
             MOVE    5                  TO  RECE2309-YUKOKETA(2)
             MOVE    "»»Äê¡¡¡¡Æü"       TO  RECE2309-NAME(2)(1:10)
             MOVE   TBL-DAY(DAY-IDX)    TO  RECE2309-NAME(2)(5:4)
             MOVE   11                  TO  IDX17
           ELSE
             MOVE    6                  TO  RECE2309-YUKOKETA(2)
             MOVE    "»»Äê¡¡¡¡¡¡Æü"     TO  RECE2309-NAME(2)(1:12)
             MOVE   TBL-DAY(DAY-IDX)    TO  RECE2309-NAME(2)(7:4)
             MOVE   13                  TO  IDX17
           END-IF
           IF       TB-SANTEI-DAY(2)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(2)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE2309-YUKOKETA(2)
               MOVE  "¡¢¡¡Æü"       TO  RECE2309-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE2309-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE2309-YUKOKETA(2)
               MOVE  "¡¢¡¡¡¡Æü"     TO  RECE2309-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE2309-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(3)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(3)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE2309-YUKOKETA(2)
               MOVE  "¡¢¡¡Æü"       TO  RECE2309-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE2309-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE2309-YUKOKETA(2)
               MOVE  "¡¢¡¡¡¡Æü"     TO  RECE2309-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE2309-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(4)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(4)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE2309-YUKOKETA(2)
               MOVE  "¡¢¡¡Æü"       TO  RECE2309-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE2309-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE2309-YUKOKETA(2)
               MOVE  "¡¢¡¡¡¡Æü"     TO  RECE2309-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE2309-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(5)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(5)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE2309-YUKOKETA(2)
               MOVE  "¡¢¡¡Æü"       TO  RECE2309-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE2309-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE2309-YUKOKETA(2)
               MOVE  "¡¢¡¡¡¡Æü"     TO  RECE2309-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE2309-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
      *
           .
       200111-TAIIN-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Êº¹¹þ¡§¾å¡Ë
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
           PERFORM  200111-RECE230-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE230
           MOVE    SPACE                TO  WRK-RECE230XX-REC
           INITIALIZE                       WRK-RECE230XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1     TO  IDX-RECE230
                MOVE   "810000001"
                             TO  WRK-RECE230XX-SRYCD(IDX-RECE230 1)
                MOVE   ZERO  TO  WRK-RECE230XX-SRYSURYO(IDX-RECE230 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE230XX-YUKOKETA(IDX-RECE230 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE230XX-NAME(IDX-RECE230 1)
           END-PERFORM 
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤Î¥³¥á¥ó¥ÈÊÔ½¸£²¡Êº¹¹þ¡§²¼¡Ë
      *****************************************************************
       20011-SHUJUTSU-ETC2-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  200111-FREE-COMMENT2-SEC                              
      *
           PERFORM  200111-RECE230-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE230
           MOVE    SPACE                TO  WRK-RECE230XX-REC
           INITIALIZE                       WRK-RECE230XX-REC
      *
           MOVE    RECE220-MEISAISU     TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                              TO  WRK-RECE230XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO   TO  WRK-RECE230XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE230XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)
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
           MOVE    ZERO           TO  DAY-IDX
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF    RECE220-KAISU(1 IDX)  NOT =  ZERO
                      ADD   1     TO  DAY-IDX
                      MOVE  IDX   TO  TB-SANTEI-DAY(DAY-IDX)
                END-IF
           END-PERFORM
      *
           .
       200111-SANTEI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤ÎÆüÉÕ¤ÎÊÔ½¸
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
                 MOVE     16         TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     8          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "¸òÉÕ¡¡¡¡Ç¯¡¡¡¡·î"
                                 TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
      *
                 INITIALIZE                      STS-AREA-DAY
                 INITIALIZE                      LNK-DAY2-AREA
                 MOVE    "21"                TO  LNK-DAY2-IRAI
                 MOVE    RECE200-SRYYM       TO  WRK-HENYMD(1:6)
                 MOVE    "01"                TO  WRK-HENYMD(7:2)
                 MOVE    WRK-HENYMD          TO  LNK-DAY2-YMD
                 CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                             LNK-DAY2-AREA
                 IF  STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(5:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(11:4)
                   END-IF
                 END-IF
              WHEN    6
                 MOVE     0          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     0          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
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
                       IF   FLG-COMMENT   =   4
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       ELSE
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    "¡¡"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       END-IF
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
      *    ¼ê½ÑÆüÅù¤ÎÆüÉÕ¤ÎÊÔ½¸¡Ê²þ¹Ô¡Ë
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
             PERFORM   200111-RECE230-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE230
             MOVE    SPACE                TO  WRK-RECE230XX-REC
             INITIALIZE                       WRK-RECE230XX-REC
      *      ¥ê¥Ï¥Ó¥ê¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE220-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE230XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE230XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7      TO  WRK-RECE230XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "¼Â»ÜÆü¿ô¡¡¡¡Æü"
                            TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             PERFORM  VARYING   IDX  FROM  1  BY  1
                       UNTIL    IDX   >   31
                 IF   RECE220-KAISU(1 IDX)  NOT = ZERO
                        ADD      1        TO  CNT-RIHA
                 END-IF
             END-PERFORM
             IF      CNT-RIHA             NOT =    ZERO
               MOVE    CNT-RIHA             TO  DAY-IDX
               MOVE    TBL-DAY(DAY-IDX)
                           TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)(9:4)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       2001-RIHA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê°å³Ø´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-RIHAIGAKU-COMMENT-SEC          SECTION.
      *
           EVALUATE      RECE220-RIHABIRI-COMMENT
              WHEN           "2"
                  MOVE   CONS-SINDAI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "3"
                  MOVE   CONS-SINDAI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "4"
                  MOVE   CONS-KOKYUKI-IGAKU1  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "5"
                  MOVE   CONS-KOKYUKI-IGAKU2  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "6"
                  MOVE   CONS-NOUKE-IGAKU1    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "7"
                  MOVE   CONS-NOUKE-IGAKU2    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "8"
                  MOVE   CONS-UNDOKI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "9"
                  MOVE   CONS-UNDOKI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
           END-EVALUATE
      ***
           MOVE          SPACE          TO  WRK-RIHAIGAKU-FSANTEIYMD
      *    °å³Ø´ÉÍý¡§»»ÄêÍúÎò¸¡º÷
           INITIALIZE                       SANTEI-REC
           MOVE    RECE220-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE220-PTID         TO  SANTEI-PTID
           MOVE    RECE220-SRYYM        TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD  TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    RECE220-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM  900-SANTEI-READ-SEC
             IF       FLG-SANTEI        =   ZERO
                MOVE  SANTEI-FSANTEIYMD TO  WRK-RIHAIGAKU-FSANTEIYMD
             END-IF
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      ***
           INITIALIZE      WRK-RIHAIGAKU-SANTEI-AREA
      *    ¥ê¥Ï¼Â»Ü¥À¥ß¡¼¡§»»ÄêÍúÎò¸¡º÷
           INITIALIZE                       SANTEI-REC
           MOVE    RECE220-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE220-PTID         TO  SANTEI-PTID
           MOVE    RECE220-SRYYM        TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD2 TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    RECE220-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM  900-SANTEI-READ-SEC
             IF       FLG-SANTEI        =   ZERO
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL   IDX    >  31
                 IF   SANTEI-DAY(IDX)   NOT =  ZERO
                    MOVE  "1"  TO  WRK-RIHAIGAKU-SANTEI-FLG
                    MOVE  SANTEI-DAY(IDX)
                               TO  WRK-RIHAIGAKU-SANTEI-DAY(IDX)
                 END-IF
               END-PERFORM
             END-IF
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      ***
      *
           PERFORM  200111-RECE230-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE230
           MOVE    SPACE                TO  WRK-RECE230XX-REC
           INITIALIZE                       WRK-RECE230XX-REC
      *
           COMPUTE  MAX-MEISAISU        =   RECE220-MEISAISU  +  1
      *
           MOVE    "810000001"
                         TO  WRK-RECE230XX-SRYCD(MAX-MEISAISU 1)
           MOVE    ZERO  TO  WRK-RECE230XX-SRYSURYO(MAX-MEISAISU 1)
           MOVE    9     TO  WRK-RECE230XX-YUKOKETA(MAX-MEISAISU 1)
           MOVE    "³«»Ï¡¡¡¡¡¡·î¡¡¡¡Æü"
                         TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)
           IF      WRK-RIHAIGAKU-FSANTEIYMD  NOT = SPACE
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)(7:4)
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)(13:4)
           END-IF
      *
           IF      WRK-RIHAIGAKU-SANTEI-FLG  NOT = SPACE
             PERFORM    20011-RIHAIGAKU-DUMMY-COMMENT-SEC
           ELSE
             PERFORM    200111-SRYACTINF-HENSYU-SEC
           END-IF
      *
           .
       2001-RIHAIGAKU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê°å³Ø´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Ê¥ê¥Ï¼Â»ÜÆü¡Ë
      *****************************************************************
       20011-RIHAIGAKU-DUMMY-COMMENT-SEC          SECTION.
      *
           INITIALIZE             TBL-SANTEI-DAY
      *
           MOVE     ZERO      TO   DAY-IDX
           PERFORM  VARYING  IDX  FROM   1   BY   1
                     UNTIL   IDX    >   31
             IF  WRK-RIHAIGAKU-SANTEI-DAY(IDX)  NOT = ZERO
                 ADD    1     TO   DAY-IDX
                 MOVE   IDX   TO   TB-SANTEI-DAY(DAY-IDX)
             END-IF
           END-PERFORM
      *
           MOVE     ZERO      TO   IDZ
           INITIALIZE         WRK-FREE-AREA2
      *
           MOVE     2         TO   FLG-COMMENT
           PERFORM  200111-FREE-COMMENT2-SEC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE230XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE230XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE230XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-RIHAIGAKU-DUMMY-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ê¥ê¥ÏÁª¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-RIHASEN-HANTEI-SEC          SECTION.
      *
           IF     RECE220-SAFLG   =    "2"
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
           PERFORM     200111-RECE230-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE230
           MOVE    SPACE                TO  WRK-RECE230XX-REC
           INITIALIZE                       WRK-RECE230XX-REC
      *
           MOVE    RECE220-MEISAISU      TO  MAX-MEISAISU
      *
      *    »»ÄêÆü¥³¥á¥ó¥È¤ÎÁÞÆþ
           PERFORM  VARYING      IDX    FROM  1   BY   1
                    UNTIL       (IDX    >     31)      OR
                                (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
      ***      MOVE    TBL-NYU-DAY-CD(IDX)
               MOVE    "810000001"
                             TO  WRK-RECE230XX-SRYCD(MAX-MEISAISU 1)
               MOVE    ZERO  TO  WRK-RECE230XX-SRYSURYO(MAX-MEISAISU 1)
               IF    TBL-NYU-DAY-CD(IDX)  =  CONS-SANTEI-COMMENT1
                 MOVE    3   TO  WRK-RECE230XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "¡¡¡¡Æü"
                             TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)(1:4)
               ELSE
                 MOVE    7   TO  WRK-RECE230XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "¡¡¡¡Æü¡Á¡¡¡¡Æü"
                             TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)(1:4)
                 MOVE    TBL-NYU-DAY-ED(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)(9:4)
               END-IF
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
                IF    RECE220-KAISU(1 IDX)   =  ZERO
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
                      UNTIL  (DAY-IDX                  >   31)  OR
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
      *    Àº¿À²Ê³ÖÎ¥ÉÂ¼¼´ÉÍý²Ã»»¡ÊÆþ±¡ÎÁ²Ã»»¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *    ¡Ê¿ÇÎÅ¹Ô°ÙÆþÎÏ¡Ë
      *****************************************************************
       2001-KAKURI-HANTEI-SEC          SECTION.
      *
           IF      RECE220-FLG-KAKURI-COMMENT    =    "1"
               PERFORM   2001-NYU-COMMENT-SEC
           ELSE
               PERFORM   2099-RECE220-TENSOU-SEC
           END-IF         
      *
           .
       2001-KAKURI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£´Å¾Á÷½èÍý
      *****************************************************************
       2099-RECE220-TENSOU-SEC     SECTION.
      *
           MOVE       RECE220-REC   TO   RECE2209-REC
           WRITE      RECE2209-REC
           ADD        1             TO   CNT-RECE2209
           PERFORM    20991-RECE230-TENSOU-SEC
      *
           .
       2099-RECE220-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£µÅ¾Á÷½èÍý
      *****************************************************************
       20991-RECE230-TENSOU-SEC     SECTION.
      *
           INITIALIZE                      RECE230-REC
           MOVE    RECE220-KEY1        TO  RECE230-KEY1
           MOVE    RECE220-ZAINUM      TO  RECE230-ZAINUM
           START   RECE230-FILE
               KEY IS    >=   RECE230-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE230
               NOT INVALID
                   PERFORM 990-RECE230-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE230           =   1              )
               OR     ( RECE220-KEY1      NOT =   RECE230-KEY1   )
               OR     ( RECE220-ZAINUM    NOT =   RECE230-ZAINUM )
      *
               MOVE    RECE230-REC       TO  RECE2309-REC
               WRITE   RECE2309-REC
               ADD     1                 TO  CNT-RECE2309
      *
               PERFORM 990-RECE230-NEXT-SEC
      *
           END-PERFORM
      *
           .
       20991-RECE230-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡ÂàÈò½èÍý
      *****************************************************************
       200111-RECE230-TAIHI-SEC     SECTION.
      *
           INITIALIZE                      RECE230-REC
           MOVE    RECE220-KEY1        TO  RECE230-KEY1
           MOVE    RECE220-ZAINUM      TO  RECE230-ZAINUM
           START   RECE230-FILE
                 KEY IS    >=   RECE230-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE230
                 NOT INVALID
                     PERFORM 990-RECE230-NEXT-SEC
                     MOVE    RECE230-REC  TO  WRK-RECE230-REC
           END-START
      *
           .
       200111-RECE230-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       200111-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE230           =   1               )
               OR     ( RECE220-KEY1      NOT =   RECE230-KEY1    )
               OR     ( RECE220-ZAINUM    NOT =   RECE230-ZAINUM  )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE230-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE230
                    MOVE   RECE230-SRYACT-TBL(IDX)
                           TO   WRK-RECE230XX-SRYACT-TBL(IDX-RECE230 1)
               END-PERFORM
      *
               PERFORM 990-RECE230-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE230
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE230
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE230  =    1
                    INITIALIZE                        RECE2309-REC
                    MOVE    WRK-RECE230-KEY       TO  RECE2309-KEY
                    ADD     1                     TO  WRK-RENNUM
                    MOVE    WRK-RENNUM            TO  RECE2309-RENNUM
                    MOVE    WRK-RECE230-PTNUM     TO  RECE2309-PTNUM
                    MOVE    WRK-RECE230-SRYKA     TO  RECE2309-SRYKA
                    MOVE    WRK-RECE230-SRYKBN    TO  RECE2309-SRYKBN
                    MOVE    WRK-RECE230-SRYSYUKBN TO  RECE2309-SRYSYUKBN
                    MOVE    WRK-RECE230-JIHIMONEY TO  RECE2309-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE230   =   1)
                       ADD         1             TO    IDX-RECE230
                       IF  WRK-RECE230XX-SRYCD(IDX-RECE230 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE230
                       ELSE
                           MOVE  WRK-RECE230XX-SRYACT-TBL(IDX-RECE230 1)
                                            TO  RECE2309-SRYACT-TBL(IDX)
                           ADD   1          TO  WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE2309-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE2309-REC
                       ADD     1            TO   CNT-RECE2309
                    END-IF
           END-PERFORM
      *
           MOVE    RECE220-REC       TO      RECE2209-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE2209-MEISAISU
           WRITE   RECE2209-REC
           ADD     1                 TO      CNT-RECE2209
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
      *    ¥Ñ¥é¥á¡¼¥¿£¶£´£¹¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           INITIALIZE                      RECE2209-REC
           MOVE    RECE200-KEY         TO  RECE2209-KEY1
           MOVE    ZERO                TO  RECE2209-ZAINUM
      *
           START   RECE2209-FILE
               KEY IS    >=   RECE2209-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE2209
               NOT INVALID
                   PERFORM 990-RECE2209-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE2209          =   1             )
               OR     ( RECE200-KEY       NOT =   RECE2209-KEY1 )
      *
               EVALUATE  RECE2209-SRYKBN
                 WHEN      "90"
                 WHEN      "92"
                    IF    FLG-RYOCOM  =  ZERO
                       PERFORM       2599-RECE2209-TENSOU-SEC
                    ELSE
                       EVALUATE  RECE2209-ZAISKBKBN
                         WHEN     "1"
                         WHEN     "2"
                           PERFORM   2502-RYOCOM-HANTEI-SEC
                         WHEN   OTHER
                           PERFORM   2599-RECE2209-TENSOU-SEC
                       END-EVALUATE
                    END-IF
                 WHEN      OTHER
                    MOVE   ZERO   TO  FLG-COMMENT
      *             ¡Ê»ÀÁÇ·×»»¼°¤Î¥³¥á¥ó¥È¤Î¥»¥Ã¥È¡Ë
                    IF   FLG-COMMENT  =  ZERO
                        PERFORM   2501-SANSO-HANTEI-SEC
                    END-IF
                    IF   FLG-COMMENT  =  ZERO
                        PERFORM   2599-RECE2209-TENSOU-SEC
                    END-IF
               END-EVALUATE
      *
               PERFORM 990-RECE2209-NEXT-SEC
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
           IF     RECE2209-FLG-SANSO-COMMENT   =    "1"
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
           INITIALIZE                       RECE2309-REC
           MOVE    RECE2209-KEY1        TO  RECE2309-KEY1
           MOVE    RECE2209-ZAINUM      TO  RECE2309-ZAINUM
           START   RECE2309-FILE
               KEY IS    >=   RECE2309-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE2309
               NOT INVALID
                   PERFORM 990-RECE2309-NEXT-SEC
           END-START
           PERFORM
               UNTIL ( FLG-RECE2309           =  1               )
               OR    ( RECE2209-KEY1      NOT =  RECE2309-KEY1   )
               OR    ( RECE2209-ZAINUM    NOT =  RECE2309-ZAINUM )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                 >   5     )
                           OR    ( RECE2309-SRYCD(IDX) =   SPACE )
      *
                  SET     TBL-SANSO    TO     1
                  SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                       AT  END
                           MOVE    ZERO    TO   FLG-SANSO-OK
                       WHEN   RECE2309-SRYCD(IDX) = TBL-SANSO-SRYCD
                                                       (TBL-SANSO)
                           MOVE    1       TO   FLG-SANSO-OK
                  END-SEARCH
                  IF      FLG-SANSO-OK        =    1
                     ADD      1              TO  IDX4
                     MOVE  RECE2309-TEN(IDX) TO  TBL-SANSO-TEN(IDX4)
                     MOVE  RECE2309-SRYSURYO(IDX)
                                             TO  TBL-SANSO-SRYSURYO
                                                              (IDX4)
                  END-IF
                  IF    RECE2309-SRYCD(IDX) = "770030070"
                     IF   TBL-KASAN-SRYSURYO  =  ZERO
                        MOVE   RECE2309-SRYSURYO(IDX)
                                     TO  TBL-KASAN-SRYSURYO
                     ELSE
                        COMPUTE   TBL-KASAN-SRYSURYO  =  
                                  TBL-KASAN-SRYSURYO  *  
                                  RECE2309-SRYSURYO(IDX)   
                     END-IF
                  END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE2309-NEXT-SEC
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
           PERFORM  250111-SANSO-FREE-COMMENT-SEC
      *
           PERFORM  250111-RECE2309-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£¶£µ£¹¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE230
           MOVE    SPACE                TO  WRK-RECE230XX-REC
           INITIALIZE                       WRK-RECE230XX-REC
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
           MOVE    ZERO        TO    WRK-KEISAN4
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
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                                                                + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
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
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
      ***                             * TBL-KASAN-SRYSURYO)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
                                      * TBL-KASAN-SRYSURYO)     + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             END-IF
      *
           END-PERFORM
      *
           IF   IDX  =  2
              MOVE    "¡à£±£°¡á"  TO  WK-FREE-NAME3(WRK-KETA:8)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  8
      *
      ***     COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***     COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
              COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
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
      ***         COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***         COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
                  COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
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
      *    £¶£µ£¹¡¡ÂàÈò½èÍý
      *****************************************************************
       250111-RECE2309-TAIHI-SEC     SECTION.
      *
           INITIALIZE                       RECE2309-REC
           MOVE    RECE2209-KEY1        TO  RECE2309-KEY1
           MOVE    RECE2209-ZAINUM      TO  RECE2309-ZAINUM
           START   RECE2309-FILE
                 KEY IS    >=   RECE2309-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE2309
                 NOT INVALID
                     PERFORM 990-RECE2309-NEXT-SEC
                     MOVE  RECE2309-REC TO  WRK-RECE230-REC
           END-START
      *
           .
       250111-RECE2309-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £¶£µ£¹¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       250111-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE2309           =   1               )
               OR     ( RECE2209-KEY1      NOT =   RECE2309-KEY1   )
               OR     ( RECE2209-ZAINUM    NOT =   RECE2309-ZAINUM )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                 >   5     )
                         OR      ( RECE2309-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE230
                    MOVE   RECE2309-SRYACT-TBL(IDX)
                             TO  WRK-RECE230XX-SRYACT-TBL(IDX-RECE230 1)
      *
                    IF   RECE2309-SRYCD(IDX) = "770020070" OR
                                               "770030070"
                         MOVE   IDX-RECE230   TO   WRK-JYUNBAN
                    END-IF
      *
               END-PERFORM
      *
               PERFORM 990-RECE2309-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-GYOSUU   NOT =  ZERO
      *
      *      ÊäÀµÎ¨Ëô¤Ï¹âµ¤°µ»ÀÁÇ²Ã»»°Ê¹ß¤ÎÌÀºÙ¤ò¤º¤é¤¹
             IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE230   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  WRK-GYOSUU
                   MOVE   WRK-RECE230XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE230XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE230XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
             END-IF
      *
      *      ·×»»¼°¤Îº¹¤·¹þ¤ß
             IF   WRK-JYUNBAN  NOT =  ZERO
                 MOVE      WRK-JYUNBAN    TO     MAX-MEISAISU
             ELSE
                 MOVE      IDX-RECE230    TO     MAX-MEISAISU
             END-IF
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   50    )
                         OR    ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD    1     TO  MAX-MEISAISU
                MOVE   "810000001"
                             TO  WRK-RECE230XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE230XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE230XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE230XX-NAME(MAX-MEISAISU 1)
      *
             END-PERFORM
      *
           END-IF
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE230
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE230
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE230  =    1
                    INITIALIZE                        RECE2301-REC
                    MOVE    WRK-RECE230-KEY       TO  RECE2301-KEY
                    ADD     1                     TO  WRK-RENNUM
                    MOVE    WRK-RENNUM            TO  RECE2301-RENNUM
                    MOVE    WRK-RECE230-PTNUM     TO  RECE2301-PTNUM
                    MOVE    WRK-RECE230-SRYKA     TO  RECE2301-SRYKA
                    MOVE    WRK-RECE230-SRYKBN    TO  RECE2301-SRYKBN
                    MOVE    WRK-RECE230-SRYSYUKBN TO  RECE2301-SRYSYUKBN
                    MOVE    WRK-RECE230-JIHIMONEY TO  RECE2301-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE230   =   1)
                       ADD         1             TO    IDX-RECE230
                       IF  WRK-RECE230XX-SRYCD(IDX-RECE230 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE230
                       ELSE
                           MOVE  WRK-RECE230XX-SRYACT-TBL(IDX-RECE230 1)
                                            TO  RECE2301-SRYACT-TBL(IDX)
                           ADD   1          TO  WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE2301-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE2301-REC
                       ADD     1            TO   CNT-RECE230
                    END-IF
           END-PERFORM
      *
           MOVE    RECE2209-REC      TO      RECE2201-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE2201-MEISAISU
           WRITE   RECE2201-REC
           ADD     1                 TO      CNT-RECE220
      *
           .
       250111-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎÅÍÜÉÂÅï¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2502-RYOCOM-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-RYOCOM2
           INITIALIZE               TBL-RYOCOM2-INF
      *
           PERFORM  VARYING  RYO-IDX  FROM  1  BY  1
                     UNTIL   RYO-IDX   >   31
      *
               IF    (RECE2209-KAISU(1 RYO-IDX)   NOT =  ZERO)
               AND   (TBL-RYOCOM-NYUINKBN(RYO-IDX)    =  "1" )
      *
                  MOVE     ZERO     TO    FLG-ARI
                  PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                            UNTIL  (RYO-IDX3   >   31)
                             OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3)
                                               = ZERO)
                             OR    (FLG-ARI    =    1)
                     IF  (TBL-RYOCOM2-MEISAISU  (RYO-IDX3) =
                                    TBL-RYOCOM-MEISAISU  (RYO-IDX))
                     AND (TBL-RYOCOM2-SRYCDTOTAL(RYO-IDX3) =
                                    TBL-RYOCOM-SRYCDTOTAL(RYO-IDX))
                     AND (TBL-RYOCOM2-ADL-TEN   (RYO-IDX3) =
                                    TBL-RYOCOM-ADL-TEN   (RYO-IDX))
                        MOVE   1   TO  TBL-RYOCOM2-DAY(RYO-IDX3 RYO-IDX)
                        MOVE   1   TO  FLG-ARI
                     END-IF
                  END-PERFORM
                  IF  FLG-ARI  =  ZERO
                     MOVE  TBL-RYOCOM-AREA(RYO-IDX)
                               TO  TBL-RYOCOM2-AREA(RYO-IDX3)
                     MOVE  1   TO  TBL-RYOCOM2-DAY (RYO-IDX3 RYO-IDX)
                  END-IF
      *
                  MOVE      1      TO     FLG-RYOCOM2
      *
               END-IF
      *
           END-PERFORM
      *
           IF  FLG-RYOCOM2   =   ZERO
              PERFORM   2599-RECE2209-TENSOU-SEC
           ELSE
              PERFORM   25021-RYOCOM-HENSYU-SEC
           END-IF
      *
           .
       2502-RYOCOM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎÅÍÜÉÂÅï¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       25021-RYOCOM-HENSYU-SEC          SECTION.
      *
           PERFORM  250111-RECE2309-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE230
           MOVE    SPACE                TO  WRK-RECE230XX2-REC
           INITIALIZE                       WRK-RECE230XX2-REC
      *
           MOVE    RECE2209-MEISAISU    TO  MAX-MEISAISU
      *
      *
           PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                     UNTIL  (RYO-IDX3   >   31                    )
                      OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3) = ZERO)
      *
              INITIALIZE                TBL-NYU-DAY
      *
              MOVE    ZERO          TO  DAY-IDX
              MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
              PERFORM    VARYING    IDX  FROM   1   BY   1
                          UNTIL    (IDX      >   31)  OR
                                   (DAY-IDX  >   31)
                IF    TBL-RYOCOM2-DAY(RYO-IDX3 IDX)   =  ZERO
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
              PERFORM  VARYING  DAY-IDX  FROM   1   BY   1
                        UNTIL  (DAY-IDX                  >   31)
                         OR    (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
                IF    TBL-NYU-DAY-ST(DAY-IDX) 
                                    =   TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  "840000059"
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                ELSE
                      MOVE  "810000001"
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                END-IF
              END-PERFORM
      *
      *       »»ÄêÆü¥³¥á¥ó¥È¤ÎÁÞÆþ
              PERFORM  VARYING   IDX    FROM  1   BY   1
                        UNTIL   (IDX                  >    31)
                         OR     (TBL-NYU-DAY-ST(IDX)  =  ZERO)
                 COMPUTE  MAX-MEISAISU   =   MAX-MEISAISU  +  1
                 MOVE  "810000001"
                             TO  WRK-RECE230XX2-SRYCD(MAX-MEISAISU 1)
                 MOVE   ZERO TO  WRK-RECE230XX2-SRYSURYO(MAX-MEISAISU 1)
                 IF    TBL-NYU-DAY-CD(IDX)  =  "840000059"
                   MOVE   6  TO  WRK-RECE230XX2-YUKOKETA(MAX-MEISAISU 1)
                   MOVE   "¡¡¡¡·î¡¡¡¡Æü"
                           TO  WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                   MOVE   RECE200-SRYYM(5:2)   TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE230XX2-NAME(MAX-MEISAISU 1)(1:4)
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE230XX2-NAME(MAX-MEISAISU 1)(7:4)
                 ELSE
                   MOVE   13 TO  WRK-RECE230XX2-YUKOKETA(MAX-MEISAISU 1)
                   MOVE   "¡¡¡¡·î¡¡¡¡Æü¡Á¡¡¡¡·î¡¡¡¡Æü"
                           TO  WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                   MOVE   RECE200-SRYYM(5:2)   TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE230XX2-NAME(MAX-MEISAISU 1)(1:4)
                               WRK-RECE230XX2-NAME(MAX-MEISAISU 1)(15:4)
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE230XX2-NAME(MAX-MEISAISU 1)(7:4)
                   MOVE   TBL-NYU-DAY-ED(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE230XX2-NAME(MAX-MEISAISU 1)(21:4)
                 END-IF
              END-PERFORM
      *
              PERFORM  VARYING  IDX  FROM   1   BY   1
                  UNTIL  (IDX                                  >  30  )
                   OR    (TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX) = SPACE)
      *
                   COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
                   MOVE "810000001"
                              TO WRK-RECE230XX2-SRYCD(MAX-MEISAISU 1)
                   MOVE  ZERO TO WRK-RECE230XX2-SRYSURYO(MAX-MEISAISU 1)
                   MOVE  4    TO WRK-RECE230XX2-YUKOKETA(MAX-MEISAISU 1)
                   EVALUATE  TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX)
                     WHEN      "820000057"
                     WHEN      "820000096"
                       MOVE "¡Ö£°£±¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000058"
                     WHEN      "820000097"
                       MOVE "¡Ö£°£²¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000059"
                     WHEN      "820000098"
                       MOVE "¡Ö£°£³¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000060"
                     WHEN      "820000099"
                       MOVE "¡Ö£°£´¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000061"
                     WHEN      "820000100"
                       MOVE "¡Ö£°£µ¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000062"
                     WHEN      "820000101"
                       MOVE "¡Ö£°£¶¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000063"
                     WHEN      "820000102"
                       MOVE "¡Ö£°£·¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000064"
                     WHEN      "820000103"
                       MOVE "¡Ö£°£¸¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000065"
                     WHEN      "820000104"
                       MOVE "¡Ö£°£¹¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000066"
                     WHEN      "820000105"
                       MOVE "¡Ö£±£°¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000067"
                     WHEN      "820000106"
                       MOVE "¡Ö£±£±¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000068"
                     WHEN      "820000107"
                       MOVE "¡Ö£±£²¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000069"
                     WHEN      "820000108"
                       MOVE "¡Ö£±£³¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000070"
                     WHEN      "820000109"
                       MOVE "¡Ö£±£´¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000071"
                     WHEN      "820000110"
                       MOVE "¡Ö£±£µ¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000072"
                     WHEN      "820000111"
                       MOVE "¡Ö£±£¶¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000073"
                     WHEN      "820000112"
                       MOVE "¡Ö£±£·¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000074"
                     WHEN      "820000113"
                       MOVE "¡Ö£±£¸¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000075"
                     WHEN      "820000114"
                       MOVE "¡Ö£±£¹¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000076"
                     WHEN      "820000115"
                       MOVE "¡Ö£²£°¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000077"
                     WHEN      "820000116"
                       MOVE "¡Ö£²£±¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000078"
                     WHEN      "820000117"
                       MOVE "¡Ö£²£²¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000079"
                     WHEN      "820000118"
                       MOVE "¡Ö£²£³¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000080"
                     WHEN      "820000119"
                       MOVE "¡Ö£²£´¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000081"
                     WHEN      "820000120"
                       MOVE "¡Ö£²£µ¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000082"
                     WHEN      "820000121"
                       MOVE "¡Ö£²£¶¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000083"
                     WHEN      "820000122"
                       MOVE "¡Ö£²£·¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000084"
                     WHEN      "820000123"
                       MOVE "¡Ö£²£¸¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000085"
                     WHEN      "820000124"
                       MOVE "¡Ö£²£¹¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000086"
                     WHEN      "820000125"
                       MOVE "¡Ö£³£°¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000087"
                     WHEN      "820000126"
                       MOVE "¡Ö£³£±¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000088"
                     WHEN      "820000127"
                       MOVE "¡Ö£³£²¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000089"
                     WHEN      "820000128"
                       MOVE "¡Ö£³£³¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000090"
                     WHEN      "820000129"
                       MOVE "¡Ö£³£´¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000091"
                     WHEN      "820000130"
                       MOVE "¡Ö£³£µ¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000092"
                     WHEN      "820000131"
                       MOVE "¡Ö£³£¶¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                     WHEN      "820000093"
                     WHEN      "820000132"
                       MOVE "¡Ö£³£·¡×"
                              TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                   END-EVALUATE
      *
              END-PERFORM
      *
              IF   TBL-RYOCOM2-ADL-SRYCD(RYO-IDX3)  NOT =  SPACE
                COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO WRK-RECE230XX2-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO WRK-RECE230XX2-SRYSURYO(MAX-MEISAISU 1)
                MOVE   7     TO WRK-RECE230XX2-YUKOKETA(MAX-MEISAISU 1)
                MOVE   "£Á£Ä£Ì¡¡¡¡¡¡ÅÀ"
                             TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)
                MOVE   TBL-RYOCOM2-ADL-TEN-MOJI(RYO-IDX3)
                             TO WRK-RECE230XX2-NAME(MAX-MEISAISU 1)(9:4)
              END-IF
      *
           END-PERFORM
      *
      *
           PERFORM    250211-SRYACTINF3-HENSYU-SEC
      *
           .
       25021-RYOCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £¶£µ£¹¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       250211-SRYACTINF3-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE2309           =   1               )
               OR     ( RECE2209-KEY1      NOT =   RECE2309-KEY1   )
               OR     ( RECE2209-ZAINUM    NOT =   RECE2309-ZAINUM )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                 >   5     )
                           OR    ( RECE2309-SRYCD(IDX) =   SPACE )
                    ADD    1    TO    IDX-RECE230
                    MOVE   RECE2309-SRYACT-TBL(IDX)
                           TO   WRK-RECE230XX2-SRYACT-TBL(IDX-RECE230 1)
               END-PERFORM
      *
               PERFORM 990-RECE2309-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE230
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE230
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE230  =    1
               INITIALIZE                        RECE2301-REC
               MOVE    WRK-RECE230-KEY       TO  RECE2301-KEY
               ADD     1                     TO  WRK-RENNUM
               MOVE    WRK-RENNUM            TO  RECE2301-RENNUM
               MOVE    WRK-RECE230-PTNUM     TO  RECE2301-PTNUM
               MOVE    WRK-RECE230-SRYKA     TO  RECE2301-SRYKA
               MOVE    WRK-RECE230-SRYKBN    TO  RECE2301-SRYKBN
               MOVE    WRK-RECE230-SRYSYUKBN TO  RECE2301-SRYSYUKBN
               MOVE    WRK-RECE230-JIHIMONEY TO  RECE2301-JIHIMONEY
               PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX             >   5)
                          OR     (FLG-WRKRECE230   =   1)
                   ADD        1             TO   IDX-RECE230
                   IF  WRK-RECE230XX2-SRYCD(IDX-RECE230 1) = SPACE
                      MOVE    1             TO    FLG-WRKRECE230
                   ELSE
                      MOVE  WRK-RECE230XX2-SRYACT-TBL(IDX-RECE230 1)
                                       TO   RECE2301-SRYACT-TBL(IDX)
                      ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
               END-PERFORM
               IF  RECE2301-SRYCD(1)  NOT =  SPACE
                  WRITE   RECE2301-REC
                  ADD     1            TO   CNT-RECE230
               END-IF
           END-PERFORM
      *
           MOVE    RECE2209-REC       TO      RECE2201-REC
           MOVE    WRK-CNT-MEISAI     TO      RECE2201-MEISAISU
      *
           MOVE    RECE2209-MEISAISU  TO      RECE2201-RYOTENKISAISU
      *
           WRITE   RECE2201-REC
           ADD     1                  TO      CNT-RECE220
      *
           .
       250211-SRYACTINF3-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£¶£´£¹Å¾Á÷½èÍý
      *****************************************************************
       2599-RECE2209-TENSOU-SEC     SECTION.
      *
           MOVE       RECE2209-REC   TO   RECE2201-REC
           WRITE      RECE2201-REC
           ADD        1              TO   CNT-RECE220
           PERFORM    25991-RECE2309-TENSOU-SEC
      *
           .
       2599-RECE2209-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£¶£µ£¹Å¾Á÷½èÍý
      *****************************************************************
       25991-RECE2309-TENSOU-SEC     SECTION.
      *
           INITIALIZE                       RECE2309-REC
           MOVE    RECE2209-KEY1        TO  RECE2309-KEY1
           MOVE    RECE2209-ZAINUM      TO  RECE2309-ZAINUM
           START   RECE2309-FILE
               KEY IS    >=   RECE2309-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE2309
               NOT INVALID
                   PERFORM 990-RECE2309-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE2309           =   1               )
               OR     ( RECE2209-KEY1      NOT =   RECE2309-KEY1   )
               OR     ( RECE2209-ZAINUM    NOT =   RECE2309-ZAINUM )
      *
               MOVE    RECE2309-REC      TO   RECE2301-REC
               WRITE   RECE2301-REC
               ADD     1                 TO   CNT-RECE230
      *
               PERFORM 990-RECE2309-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25991-RECE2309-TENSOU-EXT.
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
      *    ½ªÎ»¡¡¡¡½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ¥Õ¥¡¥¤¥ë£Ã£Ì£Ï£Ó£Å½èÍý
           PERFORM 310-FILE-CLOSE-SEC
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
           DISPLAY "RECE200  CNT-IN  :" CNT-RECE200 
           DISPLAY "RECE2209 CNT-WK  :" CNT-RECE2209
           DISPLAY "RECE2309 CNT-WK  :" CNT-RECE2309
           DISPLAY "RECE220  CNT-OUT :" CNT-RECE220 
           DISPLAY "RECE230  CNT-OUT :" CNT-RECE230 
           DISPLAY "*** ORCR1240 END ***"
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
           CLOSE                       RECE200-FILE
      *
           CLOSE                       RECE220-FILE
      *
           CLOSE                       RECE230-FILE
      *
           CLOSE                       RECE2201-FILE     
           CLOSE                       RECE2301-FILE     
      *
           CLOSE                       RECE2209-FILE
           CLOSE                       RECE2309-FILE
      *
           .
       310-FILE-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿½ç¸¡º÷½èÍý
      *****************************************************************
       990-RECE200-READ-SEC           SECTION.
      *
           READ    RECE200-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE200
                   ADD     1           TO  CNT-IN-COMMIT
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE200  PTNUM=" RECE200-PTNUM
      *
           END-READ
      *
           .
       990-RECE200-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE220-NEXT-SEC           SECTION.
      *
           READ    RECE220-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE220
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE220
           END-READ
      *
           .
       990-RECE220-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£¶£´£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE2209-NEXT-SEC           SECTION.
      *
           READ    RECE2209-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE2209
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE2209
           END-READ
      *
           .
       990-RECE2209-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE230-NEXT-SEC           SECTION.
      *
           READ    RECE230-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE230
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE230
           END-READ
      *
           .
       990-RECE230-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£¶£µ£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE2309-NEXT-SEC           SECTION.
      *
           READ    RECE2309-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE2309
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE2309
           END-READ
      *
           .
       990-RECE2309-NEXT-EXT.
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
      *    »»ÄêÎò¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-SANTEI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
           MOVE     PTCOM-REC          TO      MCPDATA-REC
           MOVE    "tbl_ptcom"         TO      MCP-TABLE
           MOVE    "key"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"       TO      MCP-TABLE
               MOVE    "key"             TO      MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
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
           MOVE    "tbl_ptcom"         TO      MCP-TABLE
           MOVE    "key"               TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                       TNS-TENSU-REC
           MOVE    SRY-SRYCD(IDX12) TO      TNS-SRYCD
           MOVE    SRY-SRYYM        TO      TNS-YUKOSTYMD(1:6)
           MOVE    "01"             TO      TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD    TO      TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM      TO      TNS-HOSPNUM
           MOVE    TNS-TENSU-REC    TO      MCPDATA-REC
           MOVE    "tbl_tensu"      TO      MCP-TABLE
           MOVE    "key"            TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_tensu"      TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                   MOVE    ZERO            TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           INITIALIZE                     MCPAREA
           MOVE    "tbl_tensu"      TO      MCP-TABLE
           MOVE    "key"            TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
       120-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
           .
       120-DBOPEN-EXT.
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
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
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
           PERFORM   900-ORCDBMAIN-SEC
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
           PERFORM   900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
