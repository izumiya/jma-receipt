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
       PROGRAM-ID.             ORCR0660.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È¡ÊÆþ±¡¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥ì¥»¥×¥È¥³¥á¥ó¥ÈÊÔ½¸
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/12/13    NaCl-ÌçÏÆ     ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    03.03.31  ÌôºÞ´ÉÍý»ØÆ³ÎÁ¤Î»»ÄêÆüÊÔ½¸
      *  01.00.02    NACL-ÌçÏÆ    03.04.04  Æþ±¡±ÉÍÜ¿©»ö»ØÆ³ÎÁ¤Î»»ÄêÆüÊÔ½¸
      *                                     Âà±¡Á°Ë¬Ìä»ØÆ³ÎÁ¤Î»»ÄêÆüÊÔ½¸
      *                                     Àº¿À²ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ¤Î»»ÄêÆüÊÔ½¸
      *                                     ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ¤Î¸òÉÕÆüÊÔ½¸
      *                                     ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ¤Î¸òÉÕÆüÊÔ½¸
      *  01.00.03    NACL-ÌçÏÆ    03.04.08  Âà±¡»þÉþÌô»ØÆ³²Ã»»»»Äê»þ¤Î
      *                                     Á°·îÌôºÞ´ÉÍý»ØÆ³ÎÁ¤Î»»ÄêÆüÊÔ½¸
      *  01.00.04    NACL-ÌçÏÆ    03.04.24  Í¢·ì¤Ï¥³¥á¥ó¥ÈÊÔ½¸ÂÐ¾Ý³°¤È¤¹¤ë
      *  01.00.05    NACL-ÌçÏÆ    03.05.28  ¼õ¿ÇÍúÎò¥­¡¼¤ÎÊÑ¹¹
      *  01.00.06    NACL-ÌçÏÆ    03.06.19  Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤ÎÊÔ½¸½¤Àµ
      *  01.00.07    NACL-ÌçÏÆ    03.08.20  ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *  01.00.08    NACL-ÌçÏÆ    03.09.24  ¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  01.00.09    NACL-ÌçÏÆ    03.09.29  ¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  01.00.10    NACL-ÌçÏÆ    03.12.15  ¼«Æ°ÊÔ½¸¥³¥á¥ó¥È¤Î¸øÈñÊ¬Î¥¶èÊ¬
      *                                     ¥»¥Ã¥È
      *  01.00.11    NACL-ÌçÏÆ    04.01.14  ¿ÇÎÅ¹Ô°Ù¤ÎÊñ³çÊ¬¤Ï½ü¤¯
      *                                     SRYACCT5¢ª31
      *  £²£°£°£´Ç¯£´·î²þÀµÂÐ±þ
      *  01.01.01    NACL-ÌçÏÆ    04.04.01  ¥ê¥Ï¸À¸ìÄ°³ÐÎÅË¡¡Ê£³¡ËÄÉ²ÃÂÐ±þ
      *  01.01.02    NACL-ÌçÏÆ    04.04.05  ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤Î
      *                                     ¥³¥á¥ó¥ÈÂÐ±þ
      *  01.01.03    NACL-ÌçÏÆ    04.04.15  Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥È
      *                                     ÂÐ±þ
      *  01.01.04    NACL-ÌçÏÆ    04.06.07  ¥³¥Ô¡¼¶ç£°£¶£µÊÑ¹¹¤ËÈ¼¤¦½¤Àµ
      *  01.01.05    NACL-ÌçÏÆ    04.06.08  ÁªÄê³°Çñ¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  Ãæ»ß½èÍýÄÉ²Ã
      *  01.02.01    NACL-Æ£¸¶    04/10/07  Ãæ»ß½èÍý¤ÎÄÉ²Ã
      *  01.02.02    NACL-ÌçÏÆ    04/12/09  Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»ÆüÂÐ±þ
      *  01.02.03    NACL-ÌçÏÆ    05/01/18  »ÀÁÇ·×»»¼°¥³¥á¥ó¥ÈÂÐ±þ
      *  01.02.04    NACL-ÌçÏÆ    05/11/01  ¡Ê¸¡Áª¡ËÅù¥³¥á¥ó¥ÈÂÐ±þ
      *  01.02.05    NACL-ÌçÏÆ    05/11/15  ½ýÉÂ¼êÅö¶â¡¢ÎÅÍÜÈñÆ±°Õ½ñ¤Î¼«Æ°
      *                                     µ­ºÜÊÔ½¸½¤Àµ
      *  01.02.06    NACL-ÌçÏÆ    05/12/20  º¹¤·¹þ¤ß¥³¥á¥ó¥È¤ÎÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
      *
           SELECT  RECE64-FILE     ASSIGN  RECE64PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE64-KEY
                                   FILE    STATUS  IS  STS-RECE64.
      *
           SELECT  RECE65-FILE     ASSIGN  RECE65PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE65-KEY
                                   FILE    STATUS  IS  STS-RECE65.
      *
           SELECT  RECE641-FILE    ASSIGN  RECE641PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE641-KEY
                                   FILE    STATUS  IS  STS-RECE641.
           SELECT  RECE6441-FILE   ASSIGN  RECE641PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE6441-KEY
                                   FILE    STATUS  IS  STS-RECE641.
      *
           SELECT  RECE651-FILE    ASSIGN  RECE651PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE651-KEY
                                   FILE    STATUS  IS  STS-RECE651.
           SELECT  RECE6551-FILE   ASSIGN  RECE651PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE6551-KEY
                                   FILE    STATUS  IS  STS-RECE651.
      *¥ï¡¼¥¯
           SELECT  RECE649-FILE    ASSIGN  RECE649PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE649-KEY
                                   FILE    STATUS  IS  STS-RECE649.
           SELECT  RECE6449-FILE   ASSIGN  RECE649PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE6449-KEY
                                   FILE    STATUS  IS  STS-RECE649.
      *
           SELECT  RECE659-FILE    ASSIGN  RECE659PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE659-KEY
                                   FILE    STATUS  IS  STS-RECE659.
           SELECT  RECE6559-FILE   ASSIGN  RECE659PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE6559-KEY
                                   FILE    STATUS  IS  STS-RECE659.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿
       FD  RECE62-FILE.
       01  RECE62-REC.
           COPY    "CPRCF062.INC".
      *
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿
       FD  RECE64-FILE.
       01  RECE64-REC.
           COPY    "CPRCF064.INC".
      *
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿
       FD  RECE65-FILE.
       01  RECE65-REC.
           COPY    "CPRCF065.INC".
      *
       FD  RECE641-FILE.
       01  RECE641-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //RECE641//.
      *
       FD  RECE651-FILE.
       01  RECE651-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //RECE651//.
      *
       FD  RECE6441-FILE.
       01  RECE6441-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //RECE6441//.
      *
       FD  RECE6551-FILE.
       01  RECE6551-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //RECE6551//.
      *¥ï¡¼¥¯
       FD  RECE649-FILE.
       01  RECE649-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //RECE649//.
      *
       FD  RECE659-FILE.
       01  RECE659-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //RECE659//.
      *
       FD  RECE6449-FILE.
       01  RECE6449-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //RECE6449//.
      *
       FD  RECE6559-FILE.
       01  RECE6559-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //RECE6559//.
      *
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
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE641//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE651//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE649//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE659//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE62          PIC X(02).
           03  STS-RECE64          PIC X(02).
           03  STS-RECE641         PIC X(02).
           03  STS-RECE65          PIC X(02).
           03  STS-RECE651         PIC X(02).
           03  STS-RECE649         PIC X(02).
           03  STS-RECE659         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE62          PIC 9(01).
           03  FLG-RECE64          PIC 9(01).
           03  FLG-RECE649         PIC 9(01).
           03  FLG-RECE65          PIC 9(01).
           03  FLG-RECE659         PIC 9(01).
           03  FLG-YAKUKAN-COMMENT PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT2 PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-WRKRECE65       PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-NYU-RENZOKU     PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-RECE62          PIC 9(06).
           03  CNT-RECE64          PIC 9(06).
           03  CNT-RECE65          PIC 9(06).
           03  CNT-RECE649         PIC 9(06).
           03  CNT-RECE659         PIC 9(06).
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
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-RECE65          PIC 9(04).
           03  IDV                 PIC 9(04).
      *     
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
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
           03  WRK-KOHBUNRI        PIC 9(01).
           03  WRK-KOHBUNRI-JIDO   PIC 9(01).
           03  WRK-HKNCOMBI-JIDO   PIC 9(04).
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
           03  WRK-TUKI-YAKUKAN      PIC 9(02).
           03  WRK-DAY-YAKUKAN       PIC 9(02).
      *
           03  WRK-KEISAN            PIC 9(09).
           03  WRK-KEISAN2           PIC 9(03).
           03  WRK-KEISAN3           PIC 9(09)V9(03).
           03  WRK-KEISAN-TAIHI      PIC X(09).
      *
           03  WRK-HENYMD            PIC X(08).
           03  WRK-TAIHI-XX          PIC X(02).
      *
       01  WRK-RECE64-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //WRK-RECE64//.
      *
       01  WRK-RECE65-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //WRK-RECE65//.
      *
       01  WRK-RECE65XX-REC.
         02 WRK-RECE65XX-TBL       OCCURS 50.
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //WRK-RECE65XX//.
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
      *Íý³ØÎÅË¡ºÞÈÖ¹æÂàÈòÎÎ°è
       01  WRK-TAIHI-ZAINUM.
           03  WRK-TAIHI-ZAINUM-T              OCCURS  5.
               05  WRK-RIGA1-SHU-ZAINUM      PIC  9(08).   
               05  WRK-RIGA1-KO-ZAINUM       PIC  9(08).   
               05  WRK-RIGA2-SHU-ZAINUM      PIC  9(08).   
               05  WRK-RIGA2-KO-ZAINUM       PIC  9(08).   
               05  WRK-RIGA3-SHU-ZAINUM      PIC  9(08).   
               05  WRK-RIGA3-KO-ZAINUM       PIC  9(08).   
               05  WRK-RIGA4-SHU-ZAINUM      PIC  9(08).   
               05  WRK-RIGA4-KO-ZAINUM       PIC  9(08).   
               05  WRK-SAGYO1-SHU-ZAINUM     PIC  9(08).   
               05  WRK-SAGYO2-SHU-ZAINUM     PIC  9(08).   
               05  WRK-SAGYO1-KO-ZAINUM      PIC  9(08).   
               05  WRK-SAGYO2-KO-ZAINUM      PIC  9(08).   
               05  WRK-GENGO1-SHU-ZAINUM     PIC  9(08).   
               05  WRK-GENGO1-KO-ZAINUM      PIC  9(08).   
               05  WRK-GENGO2-SHU-ZAINUM     PIC  9(08).   
               05  WRK-GENGO2-KO-ZAINUM      PIC  9(08).   
               05  WRK-GENGO3-SHU-ZAINUM     PIC  9(08).   
               05  WRK-GENGO3-KO-ZAINUM      PIC  9(08).   
      *
       01  WRK-TAIHI-ZAINUM-R     REDEFINES   WRK-TAIHI-ZAINUM.
           03  WRK-RIHA-ZAINUM-T               OCCURS  5.
               05  WRK-RIHA-ZAINUM   PIC 9(08)   OCCURS   18.           
      *
      *Íý³ØÎÅË¡¼Â»Ü¥«¥ì¥ó¥À¡¼
       01  WRK-RIGAKU-AREA.
           03  WRK-RIGAKU-T                    OCCURS  5.
             05  WRK-RIGA1-SHU                 PIC  X(01)  OCCURS 31.   
             05  WRK-RIGA1-KO                  PIC  X(01)  OCCURS 31.   
             05  WRK-RIGA2-SHU                 PIC  X(01)  OCCURS 31.   
             05  WRK-RIGA2-KO                  PIC  X(01)  OCCURS 31.   
             05  WRK-RIGA3-SHU                 PIC  X(01)  OCCURS 31.   
             05  WRK-RIGA3-KO                  PIC  X(01)  OCCURS 31.   
             05  WRK-RIGA4-SHU                 PIC  X(01)  OCCURS 31.   
             05  WRK-RIGA4-KO                  PIC  X(01)  OCCURS 31.   
             05  WRK-SAGYO1-SHU                PIC  X(01)  OCCURS 31.   
             05  WRK-SAGYO2-SHU                PIC  X(01)  OCCURS 31.   
             05  WRK-SAGYO1-KO                 PIC  X(01)  OCCURS 31.   
             05  WRK-SAGYO2-KO                 PIC  X(01)  OCCURS 31.   
             05  WRK-GENGO1-SHU                PIC  X(01)  OCCURS 31.   
             05  WRK-GENGO1-KO                 PIC  X(01)  OCCURS 31.   
             05  WRK-GENGO2-SHU                PIC  X(01)  OCCURS 31.   
             05  WRK-GENGO2-KO                 PIC  X(01)  OCCURS 31.   
             05  WRK-GENGO3-SHU                PIC  X(01)  OCCURS 31.   
             05  WRK-GENGO3-KO                 PIC  X(01)  OCCURS 31.   
      *
       01  WRK-RIGAKU-R        REDEFINES     WRK-RIGAKU-AREA.
           02  WRK-RIGAKU-OCC                  OCCURS    5.
             03  WRK-RIGAKU-OCC2               OCCURS   18.             
               04  WRK-RIGAKU-OCC3             OCCURS   31.
                 05  WRK-RIGAKU                PIC  X(01).
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
      *
      *   ¥ê¥Ï¥Ó¥ê¿ÇÎÅ¹Ô°Ù¥Æ¡¼¥Ö¥ë
       01  TBL-RIHA-AREA.
           03  FILLER          PIC X(09)   VALUE    "180706910".
           03  FILLER          PIC X(09)   VALUE    "180022210".
           03  FILLER          PIC X(09)   VALUE    "180706810".
           03  FILLER          PIC X(09)   VALUE    "180022110".
           03  FILLER          PIC X(09)   VALUE    "180707110".
           03  FILLER          PIC X(09)   VALUE    "180022410".
           03  FILLER          PIC X(09)   VALUE    "180707010".
           03  FILLER          PIC X(09)   VALUE    "180022310".
           03  FILLER          PIC X(09)   VALUE    "180707310".
           03  FILLER          PIC X(09)   VALUE    "180022610".
           03  FILLER          PIC X(09)   VALUE    "180707210".
           03  FILLER          PIC X(09)   VALUE    "180022510".
           03  FILLER          PIC X(09)   VALUE    "180707510".
           03  FILLER          PIC X(09)   VALUE    "180022810".
           03  FILLER          PIC X(09)   VALUE    "180707410".
           03  FILLER          PIC X(09)   VALUE    "180022710".
           03  FILLER          PIC X(09)   VALUE    "180708210".
           03  FILLER          PIC X(09)   VALUE    "180023710".
           03  FILLER          PIC X(09)   VALUE    "180708410".
           03  FILLER          PIC X(09)   VALUE    "180023910".
           03  FILLER          PIC X(09)   VALUE    "180708110".
           03  FILLER          PIC X(09)   VALUE    "180023610".
           03  FILLER          PIC X(09)   VALUE    "180708310".
           03  FILLER          PIC X(09)   VALUE    "180023810".
           03  FILLER          PIC X(09)   VALUE    "180024910".
           03  FILLER          PIC X(09)   VALUE    "180024810".
           03  FILLER          PIC X(09)   VALUE    "180025110".
           03  FILLER          PIC X(09)   VALUE    "180025010".
           03  FILLER          PIC X(09)   VALUE    "180025510".
           03  FILLER          PIC X(09)   VALUE    "180025410".
      *
       01  TBL-RIHA-R          REDEFINES   TBL-RIHA-AREA.
            03  TBL-RIHA       PIC X(09)   OCCURS   30.                 
      *
       01  TBL-RIHA-MAX        PIC 9(02)   VALUE    30.                 
      *
      *    »ÀÁÇ¡Ê¶â³Û¡¦¿ôÎÌ¡Ë¾ðÊó
       01  TBL-SANSO-INF.
           03  TBL-SANSO-INF2       OCCURS    10.
               05  TBL-SANSO-TEN           PIC 9(09)V9(02).
               05  TBL-SANSO-SRYSURYO      PIC 9(05)V9(03).
           03  TBL-SANSO-INF3.
               05  TBL-KASAN-SRYSURYO      PIC 9(05)V9(03).
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
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
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
                   UNTIL  FLG-END  =   1
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
           MOVE    "ORCR0660"      TO  JOB-PGID
           MOVE    "¥³¥á¥ó¥ÈÊÔ½¸"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *    ½èÍýÃæ»ßÀßÄê½èÍý
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           PERFORM 990-RECE62-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           MOVE    "RECE620"           TO  RECE62PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE62PARA-TERMID
      *
           MOVE    "RECE64W"           TO  RECE64PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE64PARA-TERMID
      *
           MOVE    "RECE650"           TO  RECE65PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE65PARA-TERMID
      *
           MOVE    "RECE641"           TO  RECE641PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE641PARA-TERMID
      *
           MOVE    "RECE651"           TO  RECE651PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE651PARA-TERMID
      *
           MOVE    "RECE64Y"           TO  RECE649PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE649PARA-TERMID
      *
           MOVE    "RECE65Y"           TO  RECE659PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE659PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    INPUT               RECE62-FILE
      *
           OPEN    INPUT               RECE64-FILE
      *
           OPEN    INPUT               RECE65-FILE
      *
           OPEN    OUTPUT              RECE6441-FILE
           CLOSE                       RECE6441-FILE
      *
           OPEN    OUTPUT              RECE6551-FILE
           CLOSE                       RECE6551-FILE
      *
           OPEN    OUTPUT              RECE6449-FILE
           CLOSE                       RECE6449-FILE
      *
           OPEN    OUTPUT              RECE6559-FILE
           CLOSE                       RECE6559-FILE
      *
           OPEN    OUTPUT              RECE641-FILE
      *
           OPEN    OUTPUT              RECE651-FILE
      *
           OPEN    I-O                 RECE649-FILE
      *
           OPEN    I-O                 RECE659-FILE
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
           INITIALIZE                      WRK-RIGAKU-AREA
           INITIALIZE                      WRK-TAIHI-ZAINUM
      *
           MOVE  RECE62-KOHBUNRI-MIN   TO  WRK-KOHBUNRI-JIDO
           MOVE  ZERO                  TO  WRK-HKNCOMBI-JIDO
      *
           IF   RECE62-COMCNTFLG   =   "1"
      *
             INITIALIZE                      RECE64-REC
             MOVE    RECE62-HOSPID       TO  RECE64-HOSPID
             MOVE    RECE62-SRYYM        TO  RECE64-SRYYM
             MOVE    RECE62-NYUGAIKBN    TO  RECE64-NYUGAIKBN
             MOVE    RECE62-PTID         TO  RECE64-PTID
             MOVE    RECE62-RECEKA       TO  RECE64-RECEKA
             MOVE    RECE62-TEISYUTUSAKI TO  RECE64-TEISYUTUSAKI
             MOVE    RECE62-RECESYUBETU  TO  RECE64-RECESYUBETU
             MOVE    RECE62-HKNJANUM-KEY TO  RECE64-HKNJANUM
             MOVE    RECE62-HOJOKBN-KEY  TO  RECE64-HOJOKBN
             MOVE    RECE62-TEKSTYMD     TO  RECE64-TEKSTYMD
             MOVE    ZERO                TO  RECE64-ZAINUM
      *
             START   RECE64-FILE
                 KEY IS    >=   RECE64-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE64
                 NOT INVALID
                     PERFORM 990-RECE64-NEXT-SEC
             END-START
             PERFORM
               UNTIL  ( FLG-RECE64           =   1                   )
               OR     ( RECE62-HOSPID    NOT =   RECE64-HOSPID       )
               OR     ( RECE62-SRYYM     NOT =   RECE64-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE64-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE64-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE64-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE64-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE64-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE64-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE64-HOJOKBN      )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE64-TEKSTYMD     )
      *
               EVALUATE      RECE64-SRYKBN
                   WHEN      "80"
      *            ¡Ê¥ê¥Ï¥Ó¥ê¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE64-RIHABIRI-COMMENT     =  "1"
                         PERFORM    2001-RIHA-COUNT-SEC
                      END-IF
               END-EVALUATE
      *
               PERFORM 990-RECE64-NEXT-SEC
      *
             END-PERFORM
      *
           END-IF
      *
      *    ¥Ñ¥é¥á¡¼¥¿£°£´¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           INITIALIZE                      RECE64-REC
           MOVE    RECE62-HOSPID       TO  RECE64-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE64-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE64-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE64-PTID
           MOVE    RECE62-RECEKA       TO  RECE64-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE64-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE64-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE64-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE64-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE64-TEKSTYMD
           MOVE    ZERO                TO  RECE64-ZAINUM
      *
           START   RECE64-FILE
               KEY IS    >=   RECE64-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE64
               NOT INVALID
                   PERFORM 990-RECE64-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE64           =   1                   )
               OR     ( RECE62-HOSPID    NOT =   RECE64-HOSPID       )
               OR     ( RECE62-SRYYM     NOT =   RECE64-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE64-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE64-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE64-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE64-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE64-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE64-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE64-HOJOKBN      )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE64-TEKSTYMD     )
      *
               IF   WRK-HKNCOMBI-JIDO   =   ZERO
                 IF    WRK-KOHBUNRI-JIDO   =   RECE64-KOHBUNRI
                    IF  (RECE64-SRYKBN    NOT =  "95"  AND  "96")
                    AND (RECE64-ZAITEN    NOT =   ZERO)
                    AND (RECE64-ZAIKAISU  NOT =   ZERO)
                       MOVE   RECE64-HKNCOMBI   TO   WRK-HKNCOMBI-JIDO
                    END-IF
                 END-IF
               END-IF
      *
               EVALUATE      RECE64-SRYKBN
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
                        PERFORM    2099-RECE64-TENSOU-SEC
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
                        PERFORM    2099-RECE64-TENSOU-SEC
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
                         COMPUTE  WRK-KOHBUNRI 
                                 =    RECE64-KOHBUNRI  +  1
                         MOVE    ZERO     TO    FLG-ARI
                         PERFORM   VARYING   IDX    FROM   1   BY   1
                              UNTIL   (IDX             >      18)       
                               OR     (FLG-ARI         =       1)
                           IF  RECE64-ZAINUM  =  
                                   WRK-RIHA-ZAINUM(WRK-KOHBUNRI IDX)
                               MOVE    1    TO   FLG-ARI 
                           END-IF
                         END-PERFORM
                         IF  FLG-ARI  =  1
                             MOVE      99   TO   FLG-COMMENT
                             PERFORM   2001-RIHA-COMMENT-SEC
                         END-IF
                      END-IF
      *            ¡Ê¡Ê¥ê¥ÏÁª¡ËÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM        2001-RIHASEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                             PERFORM   2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "90"                                       
                   WHEN      "92"
                      EVALUATE    RECE64-ZAISKBKBN
                        WHEN   "2"
                          PERFORM   2001-NYU-COMMENT-SEC
                        WHEN   "6"
                          PERFORM   2001-KAKURI-HANTEI-SEC
                        WHEN   OTHER
                          PERFORM   2099-RECE64-TENSOU-SEC
                      END-EVALUATE
                   WHEN   OTHER
                      PERFORM       2099-RECE64-TENSOU-SEC
               END-EVALUATE
      *
               PERFORM 990-RECE64-NEXT-SEC
      *
           END-PERFORM
      *
      *    Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ¤ß¤Î¥³¥á¥ó¥È
      *
           IF   RECE62-NYUINRYOFLG   NOT =   SPACE
      *
              IF    RECE62-RECESYUBETU  =  8888
                  CONTINUE
              ELSE
                  PERFORM   2001-NYUINRYO-COMMENT-SEC
              END-IF
      *
           END-IF
      *
      *    ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥È
      *
           IF   RECE62-KOHJNISSUTBLINF    =   ALL " "
              CONTINUE
           ELSE
              PERFORM   2001-KOHJYUSIN-COMMENT-SEC
           END-IF
      *
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥È
           PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
      *
      *****º¹¹þÊÔ½¸£²********************
           PERFORM     250-SASHIKOMI2-SEC
      ***********************************
      *
           PERFORM 990-RECE62-READ-SEC
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
      *    ¥ê¥Ï¥Ó¥ê¤Î¥«¥¦¥ó¥È
      *****************************************************************
       2001-RIHA-COUNT-SEC          SECTION.
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( IDX           >   TBL-RIHA-MAX )
      *
              INITIALIZE                        SRYACT-REC
              MOVE    RECE64-HOSPID     TO      SRY-HOSPID
              MOVE    RECE64-NYUGAIKBN  TO      SRY-NYUGAIKBN
              MOVE    RECE64-PTID       TO      SRY-PTID
              MOVE    RECE64-SRYYM      TO      SRY-SRYYM
              MOVE    RECE64-ZAINUM     TO      SRY-ZAINUM
              MOVE    TBL-RIHA(IDX)     TO      SRY-SRYCD (1)
              MOVE    TBL-RIHA(IDX)     TO      SRY-SRYCD (2)
              MOVE    TBL-RIHA(IDX)     TO      SRY-SRYCD (3)
              MOVE    TBL-RIHA(IDX)     TO      SRY-SRYCD (4)
              MOVE    TBL-RIHA(IDX)     TO      SRY-SRYCD (5)
      *
              MOVE    SRYACT-REC        TO      MCPDATA-REC
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
                     EVALUATE    IDX
                        WHEN   1  
                        WHEN   2
                           MOVE    1    TO    IDX2   
                        WHEN   3  
                        WHEN   4
                           MOVE    2    TO    IDX2   
                        WHEN   5  
                        WHEN   6
                           MOVE    3    TO    IDX2   
                        WHEN   7  
                        WHEN   8
                           MOVE    4    TO    IDX2   
                        WHEN   9  
                        WHEN  10
                           MOVE    5    TO    IDX2   
                        WHEN  11  
                        WHEN  12
                           MOVE    6    TO    IDX2   
                        WHEN  13  
                        WHEN  14
                           MOVE    7    TO    IDX2   
                        WHEN  15  
                        WHEN  16
                           MOVE    8    TO    IDX2   
                        WHEN  17  
                        WHEN  18
                           MOVE    9    TO    IDX2   
                        WHEN  19  
                        WHEN  20
                           MOVE   10    TO    IDX2   
                        WHEN  21  
                        WHEN  22
                           MOVE   11    TO    IDX2   
                        WHEN  23  
                        WHEN  24
                           MOVE   12    TO    IDX2   
                        WHEN  25  
                           MOVE   13    TO    IDX2
                        WHEN  26
                           MOVE   14    TO    IDX2   
                        WHEN  27  
                           MOVE   15    TO    IDX2
                        WHEN  28
                           MOVE   16    TO    IDX2   
                        WHEN  29                                        
                           MOVE   17    TO    IDX2                      
                        WHEN  30                                        
                           MOVE   18    TO    IDX2                      
                     END-EVALUATE
                     COMPUTE  WRK-KOHBUNRI 
                                 =    RECE64-KOHBUNRI  +  1
                     IF  RECE64-ZAINUM  >  
                                     WRK-RIHA-ZAINUM(WRK-KOHBUNRI IDX2)
                         MOVE  RECE64-ZAINUM  TO  
                                     WRK-RIHA-ZAINUM(WRK-KOHBUNRI IDX2)
                     END-IF
                     PERFORM  VARYING   IDX1  FROM  1  BY  1
                              UNTIL     IDX1  >   31
                         IF   RECE64-KAISU(1 IDX1)  NOT = ZERO
                              MOVE  "1"  TO  
                                    WRK-RIGAKU(WRK-KOHBUNRI IDX2 IDX1)
                         END-IF
                     END-PERFORM
                  END-IF
              END-IF
      *
              INITIALIZE                        MCPAREA
              MOVE    "DBCLOSECURSOR"   TO      MCP-FUNC
              MOVE    "SRYACT-KEY8"     TO      ORC-DBPATH
              CALL    "ORCMCPSUB"       USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           END-PERFORM
      *
           .
       2001-RIHA-COUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ¤ß¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-NYUINRYO-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPID       TO  RECE649-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    99999999            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "90"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           INITIALIZE                      RECE659-REC
           MOVE    RECE62-HOSPID       TO  RECE659-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE659-PTID
           MOVE    RECE62-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE659-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE659-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    99999999            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "90"                TO  RECE659-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "810000001"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           EVALUATE   RECE62-NYUINRYOFLG
             WHEN     "1"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "¼«Èñ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "2"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "Ï«ºÒ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "3"
               MOVE     11             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "¼«ÇåÀÕ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "6"
               MOVE     11             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
           END-EVALUATE
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                       TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                   TO  CNT-RECE659
      *
           .
       2001-NYUINRYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-KOHJYUSIN-COMMENT-SEC          SECTION.
      *
           INITIALIZE                TBL-NYU-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF   RECE62-KOHJNISSUTBL(IDX)   = SPACE
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
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           MOVE       5          TO   FLG-COMMENT
      *
           PERFORM  20011-FREE-COMMENT3-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                TO  IDX-RECE65
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1       TO  IDX-RECE65
                MOVE   "810000001"
                               TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                               TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME2(IDX)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM 
      *
      *    ¼õ¿ÇÆü¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                INITIALIZE                       RECE659-REC
                MOVE    RECE62-HOSPID        TO  RECE659-HOSPID
                MOVE    RECE62-SRYYM         TO  RECE659-SRYYM
                MOVE    RECE62-NYUGAIKBN     TO  RECE659-NYUGAIKBN
                MOVE    RECE62-PTID          TO  RECE659-PTID
                MOVE    RECE62-RECEKA        TO  RECE659-RECEKA
                MOVE    RECE62-TEISYUTUSAKI  TO  RECE659-TEISYUTUSAKI
                MOVE    RECE62-RECESYUBETU   TO  RECE659-RECESYUBETU
                MOVE    RECE62-HKNJANUM-KEY  TO  RECE659-HKNJANUM
                MOVE    RECE62-HOJOKBN-KEY   TO  RECE659-HOJOKBN
                MOVE    RECE62-TEKSTYMD      TO  RECE659-TEKSTYMD
                MOVE    99999980             TO  RECE659-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE659-KOHBUNRI
                MOVE    "99"                 TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE65   =   1)
                   ADD         1             TO    IDX-RECE65
                   IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPID       TO  RECE649-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    99999980            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       2001-KOHJYUSIN-COMMENT-EXT.
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
           INITIALIZE                  SANTEI-REC
           MOVE    RECE62-HOSPID       TO  SANTEI-HOSPID
           MOVE    RECE62-PTID         TO  SANTEI-PTID
           MOVE    CONS-KYUSEI         TO  SANTEI-SRYCD
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
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE62-SRYYM
                         AND  RECE62-SRYYM        <=  WRK-AFTER180(1:6)
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
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»Æü
      *
           IF      FLG-KYUSEI-COMMENT    =      1
      *       »»ÄêÍúÎò¸¡º÷
              INITIALIZE                  SANTEI-REC
              MOVE    RECE62-HOSPID       TO  SANTEI-HOSPID
              MOVE    RECE62-PTID         TO  SANTEI-PTID
              MOVE    CONS-KYUSEI-END     TO  SANTEI-SRYCD
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
      *
                   MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
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
                           AND  WRK-FSANTEIYMD(1:6)  <  RECE62-SRYYM
                                  MOVE    1    TO   FLG-KYUSEI-COMMENT2
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
           END-IF
      *
           IF      FLG-KYUSEI-COMMENT    =      1
           AND     FLG-KYUSEI-COMMENT2   =      ZERO
               PERFORM    20011-KYUSEIHASSYO-COMMENT-SEC
           END-IF
      *
           .
       2001-KYUSEIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-KYUSEIHASSYO-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPID       TO  RECE649-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    99999998            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           INITIALIZE                      RECE659-REC
           MOVE    RECE62-HOSPID       TO  RECE659-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE659-PTID
           MOVE    RECE62-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE659-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE659-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    99999998            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "99"                TO  RECE659-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
           MOVE    "810000001"          TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO                TO  WRK-RECE65XX-SRYSURYO(1 1)
           MOVE     9                   TO  WRK-RECE65XX-YUKOKETA(1 1)
           MOVE    "È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü" TO  WRK-RECE65XX-NAME(1 1)
           MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)    TO  WRK-RECE65XX-NAME(1 1)(7:4)
           MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)    TO  WRK-RECE65XX-NAME(1 1)(13:4)
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                        TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                    TO  CNT-RECE659
      *
           .
       20011-KYUSEIHASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-YAKUKANSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-YAKUKAN-COMMENT   =    "1"
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
           IF      RECE64-FLG-NYUEIYO-COMMENT   =    "1"
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
           IF      RECE64-FLG-HOUMON-COMMENT   =    "1"
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
           IF      RECE64-FLG-SYOBYO-COMMENT   =    "1"
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
           IF      RECE64-FLG-RYOYO-COMMENT    =    "1"
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
           IF      RECE64-FLG-TAIIN-COMMENT   =    "1"
      *
             MOVE      ZERO            TO      FLG-YAKUKAN-COMMENT
      *
             INITIALIZE                        SRYACT-REC
             MOVE    RECE64-HOSPID     TO      SRY-HOSPID
             MOVE    RECE64-NYUGAIKBN  TO      SRY-NYUGAIKBN
             MOVE    RECE64-PTID       TO      SRY-PTID
             MOVE    RECE64-SRYYM      TO      SRY-SRYYM
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
                 PERFORM  900-SRYACT-READ-SEC
                 IF      FLG-SRYACT  =  ZERO
                     MOVE    1         TO      FLG-YAKUKAN-COMMENT
                 END-IF
             END-IF
      *
             INITIALIZE                        MCPAREA
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
               MOVE    RECE64-SRYYM        TO  LNK-DAY6-KIJUN(1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN(7:2)
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE     -1                 TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-ZENYYMM
      *
               INITIALIZE                        SRYACT-REC
               MOVE    RECE64-HOSPID     TO      SRY-HOSPID
               MOVE    RECE64-NYUGAIKBN  TO      SRY-NYUGAIKBN
               MOVE    RECE64-PTID       TO      SRY-PTID
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
                   PERFORM  900-SRYACT-READ-SEC
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
                     INITIALIZE                        MCPAREA
                     MOVE    "DBCLOSECURSOR"   TO      MCP-FUNC
                     MOVE    "SRYACCT-KEY31"   TO      ORC-DBPATH
                     CALL    "ORCMCPSUB"       USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
      *
                     MOVE     "SRYACT-KEY7"   TO   ORC-DBPATH
                     PERFORM   900-SRYACT-READ-SEC
      *
                   END-PERFORM
               END-IF
      *
               INITIALIZE                        MCPAREA
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
           IF     RECE64-FLG-HOUSIJI-COMMENT   =    "1"
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
           IF   RECE64-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE64-KAISU(1 IDX3)  NOT =  ZERO
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
             MOVE    RECE64-HOSPID       TO  SANTEI-HOSPID
             MOVE    RECE64-PTID         TO  SANTEI-PTID
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
                       MOVE    RECE64-SRYYM    TO  WRK-HINKAIYMD(1:6)
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
             PERFORM   200111-RECE65-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE65
             MOVE    SPACE                TO  WRK-RECE65XX-REC
             INITIALIZE                       WRK-RECE65XX-REC
      *      ¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE64-MEISAISU  +  1
      *
             MOVE    "810000001"
                           TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    16    TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "½é²ó»ØÆ³´ÉÍý»»ÄêÆü¡¡¡¡¡¡·î¡¡¡¡Æü"
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(21:4)
             MOVE    WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(27:4)
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
           IF     RECE64-FLG-SHUJUTU-COMMENT   =    SPACE
                MOVE       1       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE64-TENSOU-SEC
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
           IF     RECE64-SAFLG   =    "2"
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE64-TENSOU-SEC
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
           PERFORM   200111-RECE65-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           PERFORM
               UNTIL  ( FLG-RECE65           =   1                   )
               OR     ( RECE64-HOSPID    NOT =   RECE65-HOSPID       )
               OR     ( RECE64-SRYYM     NOT =   RECE65-SRYYM        )
               OR     ( RECE64-NYUGAIKBN NOT =   RECE65-NYUGAIKBN    )
               OR     ( RECE64-PTID      NOT =   RECE65-PTID         )
               OR     ( RECE64-RECEKA    NOT =   RECE65-RECEKA       )
               OR     ( RECE64-TEISYUTUSAKI
                                         NOT =   RECE65-TEISYUTUSAKI )
               OR     ( RECE64-RECESYUBETU
                                         NOT =   RECE65-RECESYUBETU  )
               OR     ( RECE64-HKNJANUM  NOT =   RECE65-HKNJANUM     )
               OR     ( RECE64-HOJOKBN   NOT =   RECE65-HOJOKBN      )
               OR     ( RECE64-TEKSTYMD  NOT =   RECE65-TEKSTYMD     )
               OR     ( RECE64-ZAINUM    NOT =   RECE65-ZAINUM       )
               OR     ( RECE64-ZAIREN    NOT =   RECE65-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE65-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE65-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
      *++++++
                    IF    IDX-RECE65  =  RECE64-MEISAISU
      *
                      IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = "810000001"
                      OR  WRK-RECE65XX-SRYCD(IDX-RECE65 1)(1:3)  = "008"
      *
                        COMPUTE   IDV  =
                         (WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) * 2) + 1
      *
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  36
                          MOVE    "¡¡¡ß"   TO
                           WRK-RECE65XX-INPUTCOMENT(IDX-RECE65 1)(IDV:4)
                        END-IF
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  96
                          MOVE    "¡¡¡ß"   TO
                           WRK-RECE65XX-NAME(IDX-RECE65 1)(IDV:4)
                        END-IF
      *
                        COMPUTE   IDV  =  IDV  +  4
      *
                        MOVE    RECE64-ZAIKAISU   TO   DAY-IDX2
                        PERFORM 20011-KAISU-HENSYU-SEC
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  36
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE65XX-INPUTCOMENT(IDX-RECE65 1)(IDV:4)
                        END-IF
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  96
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE65XX-NAME(IDX-RECE65 1)(IDV:4)
                          COMPUTE WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) =
                                  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) +
                                  4
                        END-IF
      *
                      END-IF
      *
                    END-IF
      *++++++
               END-PERFORM
      *
               PERFORM 990-RECE65-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE659-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE659-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE659-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE659-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE659-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE659-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE659-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE659-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE659-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE659-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE659-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE659-REC
                       ADD     1            TO   CNT-RECE659
                    END-IF
           END-PERFORM
      *
           MOVE    RECE64-REC        TO      RECE649-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE649-MEISAISU
           WRITE   RECE649-REC
           ADD     1                 TO      CNT-RECE649
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
           IF      RECE64-FLG-CTMRI-COMMENT   =    "1"
                MOVE       2       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE64-TENSOU-SEC
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
           IF      RECE64-FLG-HOUSEI-COMMENT   =    "1"
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
           MOVE       RECE64-REC       TO  RECE649-REC
           ADD     1                   TO  RECE649-MEISAISU
           WRITE      RECE649-REC
           ADD     1                   TO  CNT-RECE649
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
                     MOVE    1         TO  FLG-RECE65
                 NOT INVALID
                     PERFORM 990-RECE65-NEXT-SEC
           END-START
      *
           MOVE    RECE65-REC           TO  RECE659-REC
           MOVE    RECE65-SRYACT-TBL(1) TO  RECE659-SRYACT-TBL(1)
           MOVE    RECE65-SRYACT-TBL(2) TO  RECE659-SRYACT-TBL(3)
           MOVE    RECE65-SRYACT-TBL(3) TO  RECE659-SRYACT-TBL(4)
           MOVE    RECE65-SRYACT-TBL(4) TO  RECE659-SRYACT-TBL(5)
      *
           MOVE    SPACE                TO  RECE659-SRYACT-TBL(2)
           INITIALIZE                       RECE659-SRYACT-TBL(2)
           MOVE    "810000001"          TO  RECE659-SRYCD(2)
           MOVE    ZERO                 TO  RECE659-SRYSURYO(2)
           MOVE    16                   TO  RECE659-YUKOKETA(2)
           MOVE    "ÌôºÞ´ÉÍý»ØÆ³ÎÁ»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü"
                                        TO  RECE659-NAME(2)
           MOVE     WRK-TUKI-YAKUKAN    TO  DAY-IDX
           MOVE     TBL-DAY(DAY-IDX)    TO  RECE659-NAME(2)(21:4)
           MOVE     WRK-DAY-YAKUKAN     TO  DAY-IDX
           MOVE     TBL-DAY(DAY-IDX)    TO  RECE659-NAME(2)(27:4)
           WRITE    RECE659-REC
           ADD      1                   TO  CNT-RECE659
      *
           .
       20011-TAIIN-COMMENT-EXT.
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
           PERFORM  200111-RECE65-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1     TO  IDX-RECE65
                MOVE   "810000001"
                             TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO  TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
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
           PERFORM  200111-RECE65-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                              TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO   TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
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
                IF    RECE64-KAISU(1 IDX)  NOT =  ZERO
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
                 MOVE    RECE62-SRYYM        TO  WRK-HENYMD(1:6)
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
      *    ¸øÈñ¼õ¿ÇÆü¤ÎÆüÉÕ¤ÎÊÔ½¸
      *****************************************************************
       20011-FREE-COMMENT3-SEC          SECTION.                         
      *
           ADD      1          TO    IDZ
      *
           EVALUATE   FLG-COMMENT
              WHEN    5
                 MOVE     3          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "Ê¡»ã"  TO    WK-FREE-NAME2(IDZ)(1:4)
           END-EVALUATE
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX                  >    31 )
                           OR     (TBL-NYU-DAY-ST(IDX)  =   ZERO)
      *
                 IF  TBL-NYU-DAY-CD(IDX)   =  CONS-SANTEI-COMMENT1
      *
                       MOVE   TBL-NYU-DAY-ST(IDX) TO  IDY
      *
                       IF  FLG-FIRST  =  ZERO
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    "¡¡"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         MOVE  1  TO  FLG-FIRST 
                       ELSE
                         IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                           PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                         ELSE
                           ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                           COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                           MOVE    "¡¢"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         END-IF
                       END-IF
      *
                       IF  TBL-NYU-DAY-ST(IDX)     <  10 
                          IF    WK-FREE-YUKOKETA2(IDZ)  =
                                                       24 OR 25
                            PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                          END-IF
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
                          IF    WK-FREE-YUKOKETA2(IDZ)  =
                                                 23 OR 24 OR 25
                            PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                          END-IF
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
      *
                 ELSE
      *
                       MOVE   TBL-NYU-DAY-ST(IDX) TO  IDY
                       MOVE   TBL-NYU-DAY-ED(IDX) TO  IDY2
      *
                       IF  FLG-FIRST  =  ZERO
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    "¡¡"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         MOVE  1  TO  FLG-FIRST 
                       ELSE
                         IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                           PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                         ELSE
                           ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                           COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                           MOVE    "¡¢"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         END-IF
                       END-IF
      *
                       IF  TBL-NYU-DAY-ST(IDX)     <  10 
                          IF  TBL-NYU-DAY-ED(IDX)  <  10
                            IF    WK-FREE-YUKOKETA2(IDZ)  =  
                                       21 OR 22 OR 23 OR 24 OR 25
                              PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                            END-IF
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    TBL-DAY(IDY)(3:2)     
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                            ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    "Æü¡Á"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                            MOVE    TBL-DAY(IDY2)(3:2)     
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    "Æü"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                          ELSE
                            IF    WK-FREE-YUKOKETA2(IDZ)  =  
                                 20 OR 21 OR 22 OR 23 OR 24 OR 25
                              PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                            END-IF
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    TBL-DAY(IDY)(3:2)     
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                            ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    "Æü¡Á"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                            ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                            MOVE    TBL-DAY(IDY2)
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                            MOVE    "Æü"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                          END-IF
                       ELSE
                          IF    WK-FREE-YUKOKETA2(IDZ)  =  
                               19 OR 20 OR 21 OR 22 OR 23 OR 24 OR 25
                              PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                          END-IF
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü¡Á"
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    TBL-DAY(IDY2)
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
      *
                 END-IF
      *
           END-PERFORM
           .
       20011-FREE-COMMENT3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¼õ¿ÇÆü¤ÎÆüÉÕ¤ÎÊÔ½¸¡Ê²þ¹Ô¡Ë
      *****************************************************************
       200111-FREE-COMMENT3-KAIGYO-SEC          SECTION.                  
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA2(IDZ)
           MOVE    ALL "¡¡"        TO  WK-FREE-NAME2(IDZ)(1:4)
      *
           .
       200111-FREE-COMMENT3-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-RIHA-COMMENT-SEC          SECTION.
      *
             PERFORM   200111-RECE65-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE65
             MOVE    SPACE                TO  WRK-RECE65XX-REC
             INITIALIZE                       WRK-RECE65XX-REC
      *      ¥ê¥Ï¥Ó¥ê¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE64-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7      TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "¼Â»ÜÆü¿ô¡¡¡¡Æü"
                            TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             EVALUATE   RECE64-ZAINUM
                 WHEN   WRK-RIGA1-SHU-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA1-SHU(WRK-KOHBUNRI IDX) 
                                                      NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA2-SHU-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA2-SHU(WRK-KOHBUNRI IDX) 
                                                      NOT = SPACE
                                ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA3-SHU-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA3-SHU(WRK-KOHBUNRI IDX) 
                                                      NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA4-SHU-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA4-SHU(WRK-KOHBUNRI IDX) 
                                                      NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA1-KO-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA1-KO(WRK-KOHBUNRI IDX) 
                                                     NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA2-KO-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA2-KO(WRK-KOHBUNRI IDX) 
                                                     NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA3-KO-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA3-KO(WRK-KOHBUNRI IDX) 
                                                     NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA4-KO-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA4-KO(WRK-KOHBUNRI IDX) 
                                                     NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-SAGYO1-SHU-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-SAGYO1-SHU(WRK-KOHBUNRI IDX) 
                                                       NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-SAGYO2-SHU-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-SAGYO2-SHU(WRK-KOHBUNRI IDX) 
                                                       NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-SAGYO1-KO-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-SAGYO1-KO(WRK-KOHBUNRI IDX) 
                                                      NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-SAGYO2-KO-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-SAGYO2-KO(WRK-KOHBUNRI IDX) 
                                                      NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO1-SHU-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO1-SHU(WRK-KOHBUNRI IDX) 
                                                       NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO2-SHU-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO2-SHU(WRK-KOHBUNRI IDX) 
                                                       NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO3-SHU-ZAINUM(WRK-KOHBUNRI)             
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO3-SHU(WRK-KOHBUNRI IDX) 
                                                       NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO1-KO-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO1-KO(WRK-KOHBUNRI IDX) 
                                                      NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO2-KO-ZAINUM(WRK-KOHBUNRI)
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO2-KO(WRK-KOHBUNRI IDX) 
                                                      NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO3-KO-ZAINUM(WRK-KOHBUNRI)              
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO3-KO(WRK-KOHBUNRI IDX) 
                                                      NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   OTHER
                        CONTINUE
             END-EVALUATE
             IF      CNT-RIHA             NOT =    ZERO
               MOVE    CNT-RIHA             TO  DAY-IDX
               MOVE    TBL-DAY(DAY-IDX)
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(9:4)
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
           IF     RECE64-SAFLG   =    "2"
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
           PERFORM     200111-RECE65-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *
      *    »»ÄêÆü¥³¥á¥ó¥È¤ÎÁÞÆþ
           PERFORM  VARYING      IDX    FROM  1   BY   1
                    UNTIL       (IDX    >     31)      OR
                                (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
      ***      MOVE    TBL-NYU-DAY-CD(IDX)
               MOVE    "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
               MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
               IF    TBL-NYU-DAY-CD(IDX)  =  CONS-SANTEI-COMMENT1
                 MOVE    4   TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "¡¡¡¡¡¡Æü"
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(3:4)
               ELSE
                 MOVE    8   TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "¡¡¡¡¡¡Æü¡Á¡¡¡¡Æü"
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(3:4)
                 MOVE    TBL-NYU-DAY-ED(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(11:4)
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
                IF    RECE64-KAISU(1 IDX)   =  ZERO
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
      *    Àº¿À²Ê³ÖÎ¥ÉÂ¼¼´ÉÍý²Ã»»¡ÊÆþ±¡ÎÁ²Ã»»¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *    ¡Ê¿ÇÎÅ¹Ô°ÙÆþÎÏ¡Ë
      *****************************************************************
       2001-KAKURI-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-KAKURI-COMMENT    =    "1"
               PERFORM   2001-NYU-COMMENT-SEC
           ELSE
               PERFORM   2099-RECE64-TENSOU-SEC
           END-IF         
      *
           .
       2001-KAKURI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£´Å¾Á÷½èÍý
      *****************************************************************
       2099-RECE64-TENSOU-SEC     SECTION.
      *
           MOVE       RECE64-REC    TO   RECE649-REC
           WRITE      RECE649-REC
           ADD        1             TO   CNT-RECE649
           PERFORM    20991-RECE65-TENSOU-SEC
      *
           .
       2099-RECE64-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£µÅ¾Á÷½èÍý
      *****************************************************************
       20991-RECE65-TENSOU-SEC     SECTION.
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
                   MOVE    1           TO  FLG-RECE65
               NOT INVALID
                   PERFORM 990-RECE65-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE65           =   1                   )
               OR     ( RECE64-HOSPID    NOT =   RECE65-HOSPID       )
               OR     ( RECE64-SRYYM     NOT =   RECE65-SRYYM        )
               OR     ( RECE64-NYUGAIKBN NOT =   RECE65-NYUGAIKBN    )
               OR     ( RECE64-PTID      NOT =   RECE65-PTID         )
               OR     ( RECE64-RECEKA    NOT =   RECE65-RECEKA       )
               OR     ( RECE64-TEISYUTUSAKI
                                         NOT =   RECE65-TEISYUTUSAKI )
               OR     ( RECE64-RECESYUBETU
                                         NOT =   RECE65-RECESYUBETU  )
               OR     ( RECE64-HKNJANUM  NOT =   RECE65-HKNJANUM     )
               OR     ( RECE64-HOJOKBN   NOT =   RECE65-HOJOKBN      )
               OR     ( RECE64-TEKSTYMD  NOT =   RECE65-TEKSTYMD     )
               OR     ( RECE64-ZAINUM    NOT =   RECE65-ZAINUM       )
               OR     ( RECE64-ZAIREN    NOT =   RECE65-ZAIREN       )
      *
      *
               MOVE          RECE65-REC       TO      RECE659-REC
               WRITE         RECE659-REC
               ADD     1                 TO  CNT-RECE659
      *
               PERFORM 990-RECE65-NEXT-SEC
      *
           END-PERFORM
      *
           .
       20991-RECE65-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡ÂàÈò½èÍý
      *****************************************************************
       200111-RECE65-TAIHI-SEC     SECTION.
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
                     MOVE    1           TO  FLG-RECE65
                 NOT INVALID
                     PERFORM 990-RECE65-NEXT-SEC
                     MOVE    RECE65-REC  TO  WRK-RECE65-REC
           END-START
      *
           .
       200111-RECE65-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       200111-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE65           =   1                   )
               OR     ( RECE64-HOSPID    NOT =   RECE65-HOSPID       )
               OR     ( RECE64-SRYYM     NOT =   RECE65-SRYYM        )
               OR     ( RECE64-NYUGAIKBN NOT =   RECE65-NYUGAIKBN    )
               OR     ( RECE64-PTID      NOT =   RECE65-PTID         )
               OR     ( RECE64-RECEKA    NOT =   RECE65-RECEKA       )
               OR     ( RECE64-TEISYUTUSAKI
                                         NOT =   RECE65-TEISYUTUSAKI )
               OR     ( RECE64-RECESYUBETU
                                         NOT =   RECE65-RECESYUBETU  )
               OR     ( RECE64-HKNJANUM  NOT =   RECE65-HKNJANUM     )
               OR     ( RECE64-HOJOKBN   NOT =   RECE65-HOJOKBN      )
               OR     ( RECE64-TEKSTYMD  NOT =   RECE65-TEKSTYMD     )
               OR     ( RECE64-ZAINUM    NOT =   RECE65-ZAINUM       )
               OR     ( RECE64-ZAIREN    NOT =   RECE65-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE65-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE65-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                    IF   RECE64-ZAITEN   =   ZERO
                       EVALUATE   WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                         WHEN   "190111590"
                             MOVE    "810000001"
                                TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                             MOVE    ZERO
                                TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                             MOVE    8
                                TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                             MOVE    "³°Çñ¡ÊÁªÄêÎÅÍÜ¡Ë"
                                TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                         WHEN   "190116590"
                             MOVE    "810000001"
                                TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                             MOVE    ZERO
                                TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                             MOVE    22
                                TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
       MOVE "ÆÃÄêÆþ±¡ÎÁ¸º»»¡ÊÂ¾°åÎÅµ¡´Ø¼õ¿Ç¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                                TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                         WHEN   "190116690"
                             MOVE    "810000001"
                                TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                             MOVE    ZERO
                                TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                             MOVE    26
                                TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
       MOVE "ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¸º»»¡ÊÂ¾°åÎÅµ¡´Ø¼õ¿Ç¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                                TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                         WHEN   "190116790"
                             MOVE    "810000001"
                                TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                             MOVE    ZERO
                                TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                             MOVE    29
                                TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
       MOVE "¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¸º»»¡ÊÂ¾°åÎÅµ¡´Ø¼õ¿Ç¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                                TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                       END-EVALUATE
                    END-IF
               END-PERFORM
      *
               PERFORM 990-RECE65-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE659-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE659-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE659-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE659-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE659-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE659-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE659-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE659-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE659-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE659-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE659-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE659-REC
                       ADD     1            TO   CNT-RECE659
                    END-IF
           END-PERFORM
      *
           MOVE    RECE64-REC        TO      RECE649-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE649-MEISAISU
           WRITE   RECE649-REC
           ADD     1                 TO      CNT-RECE649
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
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPID       TO  RECE649-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    ZERO                TO  RECE649-ZAINUM
      *
           START   RECE649-FILE
               KEY IS    >=   RECE649-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE649
               NOT INVALID
                   PERFORM 990-RECE649-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE649          =   1                    )
               OR     ( RECE62-HOSPID    NOT =   RECE649-HOSPID       )
               OR     ( RECE62-SRYYM     NOT =   RECE649-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE649-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE649-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE649-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE649-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE649-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE649-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE649-HOJOKBN      )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE649-TEKSTYMD     )
      *
               MOVE   ZERO   TO  FLG-COMMENT
      *        ¡Ê»ÀÁÇ·×»»¼°¤Î¥³¥á¥ó¥È¤Î¥»¥Ã¥È¡Ë
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2501-SANSO-HANTEI-SEC
               END-IF
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2599-RECE649-TENSOU-SEC
               END-IF
      *
               PERFORM 990-RECE649-NEXT-SEC
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
           IF     RECE649-FLG-SANSO-COMMENT   =    "1"
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
           INITIALIZE                       RECE659-REC
           MOVE    RECE649-HOSPID       TO  RECE659-HOSPID
           MOVE    RECE649-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE649-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE649-PTID         TO  RECE659-PTID
           MOVE    RECE649-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE649-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE649-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE649-HKNJANUM     TO  RECE659-HKNJANUM
           MOVE    RECE649-HOJOKBN      TO  RECE659-HOJOKBN
           MOVE    RECE649-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    RECE649-ZAINUM       TO  RECE659-ZAINUM
           MOVE    RECE649-ZAIREN       TO  RECE659-ZAIREN
           START   RECE659-FILE
               KEY IS    >=   RECE659-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE659
               NOT INVALID
                   PERFORM 990-RECE659-NEXT-SEC
           END-START
           PERFORM
               UNTIL ( FLG-RECE659           =  1                    )
               OR    ( RECE649-HOSPID    NOT =  RECE659-HOSPID       )
               OR    ( RECE649-SRYYM     NOT =  RECE659-SRYYM        )
               OR    ( RECE649-NYUGAIKBN NOT =  RECE659-NYUGAIKBN    )
               OR    ( RECE649-PTID      NOT =  RECE659-PTID         )
               OR    ( RECE649-RECEKA    NOT =  RECE659-RECEKA       )
               OR    ( RECE649-TEISYUTUSAKI
                                         NOT =  RECE659-TEISYUTUSAKI )
               OR    ( RECE649-RECESYUBETU
                                         NOT =  RECE659-RECESYUBETU  )
               OR    ( RECE649-HKNJANUM  NOT =  RECE659-HKNJANUM     )
               OR    ( RECE649-HOJOKBN   NOT =  RECE659-HOJOKBN      )
               OR    ( RECE649-TEKSTYMD  NOT =  RECE659-TEKSTYMD     )
               OR    ( RECE649-ZAINUM    NOT =  RECE659-ZAINUM       )
               OR    ( RECE649-ZAIREN    NOT =  RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE659-SRYCD(IDX) =   SPACE )
      *
                  SET     TBL-SANSO    TO     1
                  SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                       AT  END
                           MOVE    ZERO    TO   FLG-SANSO-OK
                       WHEN   RECE659-SRYCD(IDX) = TBL-SANSO-SRYCD
                                                       (TBL-SANSO)
                           MOVE    1       TO   FLG-SANSO-OK
                  END-SEARCH
                  IF      FLG-SANSO-OK        =    1
                     ADD      1             TO  IDX4
                     MOVE  RECE659-TEN(IDX) TO  TBL-SANSO-TEN(IDX4)
                     MOVE  RECE659-SRYSURYO(IDX)
                                            TO  TBL-SANSO-SRYSURYO
                                                             (IDX4)
                  END-IF
                  IF    RECE659-SRYCD(IDX) = "770030070"
                     IF   TBL-KASAN-SRYSURYO  =  ZERO
                        MOVE   RECE659-SRYSURYO(IDX)
                                     TO  TBL-KASAN-SRYSURYO
                     ELSE
                        COMPUTE   TBL-KASAN-SRYSURYO  =  
                                  TBL-KASAN-SRYSURYO  *  
                                  RECE659-SRYSURYO(IDX)   
                     END-IF
                  END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE659-NEXT-SEC
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
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£¶£µ£¹¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
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
      *    £¶£µ£¹¡¡ÂàÈò½èÍý
      *****************************************************************
       250111-RECE659-TAIHI-SEC     SECTION.
      *
           INITIALIZE                       RECE659-REC
           MOVE    RECE649-HOSPID       TO  RECE659-HOSPID
           MOVE    RECE649-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE649-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE649-PTID         TO  RECE659-PTID
           MOVE    RECE649-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE649-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE649-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE649-HKNJANUM     TO  RECE659-HKNJANUM
           MOVE    RECE649-HOJOKBN      TO  RECE659-HOJOKBN
           MOVE    RECE649-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    RECE649-ZAINUM       TO  RECE659-ZAINUM
           MOVE    RECE649-ZAIREN       TO  RECE659-ZAIREN
           START   RECE659-FILE
                 KEY IS    >=   RECE659-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE659
                 NOT INVALID
                     PERFORM 990-RECE659-NEXT-SEC
                     MOVE   RECE659-REC TO  WRK-RECE65-REC
           END-START
      *
           .
       250111-RECE659-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £¶£µ£¹¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       250111-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE659           =   1                    )
               OR     ( RECE649-HOSPID    NOT =   RECE659-HOSPID       )
               OR     ( RECE649-SRYYM     NOT =   RECE659-SRYYM        )
               OR     ( RECE649-NYUGAIKBN NOT =   RECE659-NYUGAIKBN    )
               OR     ( RECE649-PTID      NOT =   RECE659-PTID         )
               OR     ( RECE649-RECEKA    NOT =   RECE659-RECEKA       )
               OR     ( RECE649-TEISYUTUSAKI
                                          NOT =   RECE659-TEISYUTUSAKI )
               OR     ( RECE649-RECESYUBETU
                                          NOT =   RECE659-RECESYUBETU  )
               OR     ( RECE649-HKNJANUM  NOT =   RECE659-HKNJANUM     )
               OR     ( RECE649-HOJOKBN   NOT =   RECE659-HOJOKBN      )
               OR     ( RECE649-TEKSTYMD  NOT =   RECE659-TEKSTYMD     )
               OR     ( RECE649-ZAINUM    NOT =   RECE659-ZAINUM       )
               OR     ( RECE649-ZAIREN    NOT =   RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE659-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE659-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
      *
                    IF   RECE659-SRYCD(IDX) = "770020070" OR "770030070"
                         MOVE   IDX-RECE65   TO   WRK-JYUNBAN
                    END-IF
      *
               END-PERFORM
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-GYOSUU   NOT =  ZERO
      *
      *      ÊäÀµÎ¨Ëô¤Ï¹âµ¤°µ»ÀÁÇ²Ã»»°Ê¹ß¤ÎÌÀºÙ¤ò¤º¤é¤¹
             IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE65   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  WRK-GYOSUU
                   MOVE   WRK-RECE65XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE65XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE65XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
             END-IF
      *
      *      ·×»»¼°¤Îº¹¤·¹þ¤ß
             IF   WRK-JYUNBAN  NOT =  ZERO
                 MOVE      WRK-JYUNBAN    TO     MAX-MEISAISU
             ELSE
                 MOVE      IDX-RECE65     TO     MAX-MEISAISU
             END-IF
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   50    )
                         OR    ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD    1     TO  MAX-MEISAISU
                MOVE   "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                MOVE   "1"   TO  WRK-RECE65XX-RECEDENFLG(MAX-MEISAISU 1)
      *
             END-PERFORM
      *
           END-IF
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE651-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE651-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE651-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE651-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE651-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE651-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE651-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE651-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE651-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE651-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE651-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE651-REC
                       ADD     1            TO   CNT-RECE65
                    END-IF
           END-PERFORM
      *
           MOVE    RECE649-REC       TO      RECE641-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE641-MEISAISU
           WRITE   RECE641-REC
           ADD     1                 TO      CNT-RECE64
      *
           .
       250111-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£¶£´£¹Å¾Á÷½èÍý
      *****************************************************************
       2599-RECE649-TENSOU-SEC     SECTION.
      *
           MOVE       RECE649-REC    TO   RECE641-REC
           WRITE      RECE641-REC
           ADD        1              TO   CNT-RECE64
           PERFORM    25991-RECE659-TENSOU-SEC
      *
           .
       2599-RECE649-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£¶£µ£¹Å¾Á÷½èÍý
      *****************************************************************
       25991-RECE659-TENSOU-SEC     SECTION.
      *
           INITIALIZE                       RECE659-REC
           MOVE    RECE649-HOSPID       TO  RECE659-HOSPID
           MOVE    RECE649-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE649-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE649-PTID         TO  RECE659-PTID
           MOVE    RECE649-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE649-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE649-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE649-HKNJANUM     TO  RECE659-HKNJANUM
           MOVE    RECE649-HOJOKBN      TO  RECE659-HOJOKBN
           MOVE    RECE649-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    RECE649-ZAINUM       TO  RECE659-ZAINUM
           MOVE    RECE649-ZAIREN       TO  RECE659-ZAIREN
           START   RECE659-FILE
               KEY IS    >=   RECE659-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE659
               NOT INVALID
                   PERFORM 990-RECE659-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE659           =   1                    )
               OR     ( RECE649-HOSPID    NOT =   RECE659-HOSPID       )
               OR     ( RECE649-SRYYM     NOT =   RECE659-SRYYM        )
               OR     ( RECE649-NYUGAIKBN NOT =   RECE659-NYUGAIKBN    )
               OR     ( RECE649-PTID      NOT =   RECE659-PTID         )
               OR     ( RECE649-RECEKA    NOT =   RECE659-RECEKA       )
               OR     ( RECE649-TEISYUTUSAKI
                                          NOT =   RECE659-TEISYUTUSAKI )
               OR     ( RECE649-RECESYUBETU
                                          NOT =   RECE659-RECESYUBETU  )
               OR     ( RECE649-HKNJANUM  NOT =   RECE659-HKNJANUM     )
               OR     ( RECE649-HOJOKBN   NOT =   RECE659-HOJOKBN      )
               OR     ( RECE649-TEKSTYMD  NOT =   RECE659-TEKSTYMD     )
               OR     ( RECE649-ZAINUM    NOT =   RECE659-ZAINUM       )
               OR     ( RECE649-ZAIREN    NOT =   RECE659-ZAIREN       )
      *
               MOVE    RECE659-REC       TO   RECE651-REC
               WRITE   RECE651-REC
               ADD     1                 TO   CNT-RECE65
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25991-RECE659-TENSOU-EXT.
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
           DISPLAY "RECE62  CNT-IN  :" CNT-RECE62 
           DISPLAY "RECE649 CNT-WK  :" CNT-RECE649
           DISPLAY "RECE659 CNT-WK  :" CNT-RECE659
           DISPLAY "RECE64  CNT-OUT :" CNT-RECE64 
           DISPLAY "RECE65  CNT-OUT :" CNT-RECE65 
           DISPLAY "*** ORCR0660 END ***"
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
           CLOSE                       RECE62-FILE
      *
           CLOSE                       RECE64-FILE
      *
           CLOSE                       RECE65-FILE
      *
           CLOSE                       RECE641-FILE     
           CLOSE                       RECE651-FILE     
      *
           CLOSE                       RECE649-FILE
           CLOSE                       RECE659-FILE
      *
           .
       310-FILE-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿½ç¸¡º÷½èÍý
      *****************************************************************
       990-RECE62-READ-SEC           SECTION.
      *
           READ    RECE62-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE62
                   ADD     1           TO  CNT-IN-COMMIT
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE62  PTNUM=" RECE62-PTNUM
      *
           END-READ
      *
           .
       990-RECE62-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE64-NEXT-SEC           SECTION.
      *
           READ    RECE64-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE64
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE64
           END-READ
      *
           .
       990-RECE64-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£¶£´£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE649-NEXT-SEC           SECTION.
      *
           READ    RECE649-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE649
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE649
           END-READ
      *
           .
       990-RECE649-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE65-NEXT-SEC           SECTION.
      *
           READ    RECE65-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE65
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE65
           END-READ
      *
           .
       990-RECE65-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£¶£µ£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE659-NEXT-SEC           SECTION.
      *
           READ    RECE659-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE659
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE659
           END-READ
      *
           .
       990-RECE659-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
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
       900-SRYACT-READ-EXT.
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
      *    ¥«¡¼¥½¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       CSR-CLOSE-SEC            SECTION.
      *
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       CSR-CLOSE-EXT.
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
