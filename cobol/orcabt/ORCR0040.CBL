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
       PROGRAM-ID.             ORCR0040.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥ì¥»¥×¥È¥³¥á¥ó¥ÈÊÔ½¸
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  00/04/23    MCC-¾®Æ¦Âô    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    02.11.28  Æ±ÆüÊ£¿ôºÆ¿Ç¡¢ÅÅÏÃºÆ¿Ç¤Î
      *                                     ¥³¥á¥ó¥Èº¹¹þ¤ß
      *  01.00.02    NACL-ÌçÏÆ    02.11.29  Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î
      *                                     ¥³¥á¥ó¥Èº¹¹þ¤ß
      *  01.00.03    NACL-ÌçÏÆ    03.02.04  ºÞ¤Þ¤È¤áÂÐ±þÅù
      *  01.00.04    NACL-ÌçÏÆ    03.02.06  ¥³¥á¥ó¥È¥«¥¦¥ó¥È¥Õ¥é¥°¤Ç¤Î
      *                                     È½ÄêÄÉ²Ã
      *  01.00.05    NACL-Æ£¸¶    03.03.12  Êä½õ¶èÊ¬¤ÎÄÉ²Ã
      *  01.00.06    NACL-ÌçÏÆ    03.03.03  ºÞÈÖ¹æ·å½¤ÀµÂÐ±þ
      *  01.00.07    NACL-ÌçÏÆ    03.03.12  £Ã£Ô¡¦£Í£Ò£É¤Î¼Â»ÜÆüÄÉ²Ã
      *  01.00.08    NACL-ÌçÏÆ    03.04.04  ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ¤Î¸òÉÕÆüÊÔ½¸
      *                                     ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ¤Î¸òÉÕÆüÊÔ½¸
      *  01.00.09    NACL-ÌçÏÆ    03.04.24  Í¢·ì¤Ï¥³¥á¥ó¥ÈÊÔ½¸ÂÐ¾Ý³°¤È¤¹¤ë
      *  01.00.10    NACL-ÌçÏÆ    03.05.28  ¼õ¿ÇÍúÎò¥­¡¼¤ÎÊÑ¹¹
      *  01.00.11    NACL-ÌçÏÆ    03.06.19  Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤ÎÊÔ½¸½¤Àµ
      *  01.00.12    NACL-ÌçÏÆ    03.07.15  ÆýÍÄ»ù¡¦ÍÄ»ù¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *  01.00.13    NACL-ÌçÏÆ    03.08.20  ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *  01.00.14    NACL-ÌçÏÆ    03.09.24  ¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  01.00.15    NACL-ÌçÏÆ    03.09.29  ¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  01.00.16    NACL-ÌçÏÆ    03.12.15  ¼«Æ°ÊÔ½¸¥³¥á¥ó¥È¤Î¸øÈñÊ¬Î¥¶èÊ¬
      *                                     ¥»¥Ã¥È
      *  £²£°£°£´Ç¯£´·î²þÀµÂÐ±þ
      *  01.01.01    NACL-ÌçÏÆ    04.04.01  ¥ê¥Ï¸À¸ìÄ°³ÐÎÅË¡¡Ê£³¡ËÄÉ²ÃÂÐ±þ
      *  01.01.02    NACL-ÌçÏÆ    04.04.05  ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤Î
      *                                     ¥³¥á¥ó¥ÈÂÐ±þ
      *  01.01.03    NACL-ÌçÏÆ    04.04.10  ºßÂð´µ¼ÔË¬ÌäÅÀÅ©Ãí¼Í´ÉÍý»ØÆ³ÎÁ
      *                                     »»Äê»þ¤ÎÅÀÅ©Ãí¼Í¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  01.01.04    NACL-ÌçÏÆ    04.04.15  Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥È
      *                                     ÂÐ±þ
      *
      *  01.02.01    NACL-Æ£¸¶    04.05.26  ¥ì¥¤¥¢¥¦¥ÈÊÑ¹¹
      *                                     ¹ð¼¨Åù¼±ÊÌ¶èÊ¬£±
      *                                     ¤­¤¶¤ßÃÍ·×»»¼±ÊÌ
      *                                     Ãí²Ã»»¥³¡¼¥É¤ÎÄÉ²Ã  
      *  01.02.02    NACL-ÌçÏÆ    04.07.30  ÅÅÏÃºÆ¿Ç¡¢Æ±ÆüÊ£¿ôºÆ¿Ç¤Î¥³¥á¥ó¥È
      *                                     ½¤Àµ
      *  01.02.03    NACL-ÌçÏÆ    05.04.20  ÆâÉþÌôºÞÄþ¸ºÊÔ½¸ÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿
           SELECT  RECE02-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿¡Ê£É£Î¡Ë
           SELECT  RECE04-FILE     ASSIGN  RECE04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE04-KEY
                                   FILE    STATUS  IS  STS-RECE04.
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿¡Ê£É£Î¡Ë
           SELECT  RECE05-FILE     ASSIGN  RECE05PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE05-KEY
                                   FILE    STATUS  IS  STS-RECE05.
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿¡Ê£Ï£Õ£Ô¡Ë
           SELECT  RECE041-FILE    ASSIGN  RECE041PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE041-KEY
                                   FILE    STATUS  IS  STS-RECE041.
           SELECT  RECE0441-FILE   ASSIGN  RECE041PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE0441-KEY
                                   FILE    STATUS  IS  STS-RECE041.
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿¡Ê£Ï£Õ£Ô¡Ë
           SELECT  RECE051-FILE    ASSIGN  RECE051PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE051-KEY
                                   FILE    STATUS  IS  STS-RECE051.
           SELECT  RECE0551-FILE   ASSIGN  RECE051PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE0551-KEY
                                   FILE    STATUS  IS  STS-RECE051.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿
       FD  RECE02-FILE.
       01  RECE02-REC.
           COPY    "CPRCF002.INC".
      *
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿
       FD  RECE04-FILE.
       01  RECE04-REC.
           COPY    "CPRCF004.INC".
      *
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿
       FD  RECE05-FILE.
       01  RECE05-REC.
           COPY    "CPRCF005.INC".
      *
       FD  RECE041-FILE.
       01  RECE041-REC. 
           COPY    "CPRCF004.INC"  REPLACING   //RECE04//
                                   BY          //RECE041//.
      *
       FD  RECE051-FILE.
       01  RECE051-REC. 
           COPY    "CPRCF005.INC"  REPLACING   //RECE05//
                                   BY          //RECE051//.
      *
       FD  RECE0441-FILE.
       01  RECE0441-REC. 
           COPY    "CPRCF004.INC"  REPLACING   //RECE04//
                                   BY          //RECE0441//.
      *
       FD  RECE0551-FILE.
       01  RECE0551-REC. 
           COPY    "CPRCF005.INC"  REPLACING   //RECE05//
                                   BY          //RECE0551//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     ¥ì¥»¥×¥ÈÌÀºÙ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC".
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE04//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE05//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE041//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE051//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    ¥¹¥ÑÎÎ°è
       01  SPA2-AREA.
           03  SPA-STS              PIC 9(04).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE02          PIC X(02).
           03  STS-RECE04          PIC X(02).
           03  STS-RECE041         PIC X(02).
           03  STS-RECE05          PIC X(02).
           03  STS-RECE051         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE02          PIC 9(01).
           03  FLG-RECE04          PIC 9(01).
           03  FLG-RECE05          PIC 9(01).
           03  FLG-WRKRECE05       PIC 9(01).
           03  FLG-SHIDOU-COMMENT  PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-YAKUTEI         PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-RECE02          PIC 9(06).
           03  CNT-RECE04          PIC 9(06).
           03  CNT-RECE05          PIC 9(06).
           03  CNT-JINZOU          PIC 9(06).
           03  CNT-RIHA            PIC 9(06).
           03  CNT-JUSHIN          PIC 9(06).
           03  CNT-SANTEIKA        PIC 9(06).
           03  CNT-IN-COMMIT       PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(02).
           03  IDX6                PIC 9(04).
           03  IDX7                PIC 9(04).
           03  IDX8                PIC 9(04).
           03  IDX9                PIC 9(04).
           03  IDX10               PIC 9(04).
           03  IDX11               PIC 9(04).
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-RECE05          PIC 9(04).
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
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-CNT-MEISAI      PIC 9(04).
           03  WRK-KETA            PIC 9(04).
           03  WRK-KETA-TAIHI      PIC 9(04).
           03  WRK-KETA2           PIC 9(04).
           03  WRK-KOHBUNRI        PIC 9(01).
           03  WRK-KOHBUNRI-JIDO   PIC 9(01).
           03  WRK-ZAINUM          PIC 9(08).
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
               05  TB-SANTEI-DAY     PIC 9(02)   OCCURS  31.
      *
           03  WRK-HKNCOMBINUMX      PIC X(04).
           03  WRK-HKNCOMBINUM9      REDEFINES  WRK-HKNCOMBINUMX
                                     PIC 9(04).
      *
           03  WRK-SRYYMDX.
               05  WRK-SRYYM         PIC 9(06).
               05  WRK-SRYDD         PIC 9(02).
      *
       01  WRK-RECE04-REC. 
           COPY    "CPRCF004.INC"  REPLACING   //RECE04//
                                   BY          //WRK-RECE04//.
      *
       01  WRK-RECE05-REC. 
           COPY    "CPRCF005.INC"  REPLACING   //RECE05//
                                   BY          //WRK-RECE05//.
      *
       01  WRK-RECE05XX-REC.
         02 WRK-RECE05XX-TBL       OCCURS 50.
           COPY    "CPRCF005.INC"  REPLACING   //RECE05//
                                   BY          //WRK-RECE05XX//.
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
      *
      *¿Í¹©¿ÕÂ¡¼Â»Ü¥«¥ì¥ó¥À¡¼
       01  WRK-JINZOU-AREA.
           03  WRK-JINZOU-T                    OCCURS   5.
             05  WRK-JINZOU                    PIC  X(01)  OCCURS 31.   
             05  WRK-JINZOU-HKNCOMBI           PIC  9(04).
      *
      *ºÆ¿Ç£´²óÌÜ°Ê¹ß¼Â»Ü¥«¥ì¥ó¥À¡¼
       01  WRK-SAISHIN4-AREA.
           03  WRK-SAISHIN4-T                  OCCURS   5.
             05  WRK-SAISHIN4                  PIC  X(01)  OCCURS 31.   
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
      *    ËýÀ­¤È¤¦ÄË¼À´µ
           03  CONS-MANSEI          PIC X(09) VALUE "113006510".
      *    ºßÂð·ì±ÕÆ©ÀÏ»ØÆ³´ÉÍýÎÁ
           03  CONS-TOSEKI          PIC X(09) VALUE "114009310".
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü
           03  CONS-KYUSEI          PIC X(09) VALUE "099800101".
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
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤ÎÂàÈò¥Æ¡¼¥Ö¥ë
       01  TBL-JUSHIN-INF.
           03  TBL-JUSHIN-INF2      OCCURS    50.
               05  TBL-JUSHIN-SRYKA       PIC X(02).
               05  TBL-JUSHIN-SRYKANAME   PIC X(80).
               05  TBL-JUSHIN-ARI         PIC X(01).
               05  TBL-JUSHIN-DAY         PIC X(01)   OCCURS  31.
      *
       01  TBL-COUNT-INF.
           03  TBL-COUNT-INF2       OCCURS     5.
               05  CNT-DENWA              PIC 9(06).
               05  CNT-DOUJITU            PIC 9(06).
               05  TBL-COUNT-HKNCOMBI     PIC 9(04).
      *
      *    ÌôºÞÄþ¸º¾ðÊó
       01  TBL-YAKUTEI-INF.
           03  TBL-YT1-OCC                      OCCURS 100.
             05  TBL-YT1-KEY.
               07  TBL-YT1-KOHBUNRI             PIC 9(01).
               07  TBL-YT1-HKNCOMBI             PIC 9(04).
               07  TBL-YT1-SRYKA                PIC X(02).
             05  TBL-YT1-RECE04-REC             PIC X(250).
             05  TBL-YT1-HIKAKU.
               07  TBL-YT1-ZAITEN               PIC 9(08).
               07  TBL-YT1-SRYCDTOTAL           PIC 9(14).
               07  TBL-YT1-SURYOUTOTAL          PIC 9(07)V9(03).
               07  TBL-YT1-RUIKEITOTAL          PIC 9(15)V9(03).
               07  TBL-YT1-MEISAISU             PIC 9(04).
             05  TBL-YT1-RECE04-DAY-AREA.
               07  TBL-YT1-OCC2                 OCCURS  31.
                 09  TBL-YT1-OCC3               OCCURS   3.
                   11  TBL-YT1-KAISU            PIC 9(03).
             05  TBL-YT1-FLG                    PIC 9(01).
      *
      *    ÊÂ¤ÓÂØ¤¨ÍÑ
       01  WRK-YAKUTEI-DATA                     PIC X(591).
      *
       01  WRK-TAIHI-YT1-KEY                    PIC X(07).
       01  WRK-TAIHI-YT1-KEY2                   PIC 9(01).
       01  WRK-YAKUTEI-ZAINUM                   PIC 9(08).
       01  WRK-YAKUTEI-ZAINUM2                  PIC 9(08).
      *
      *    ÌôºÞÄþ¸º¾ðÊó¡Ê¥­¡¼Ã±°Ì¤ÎÊÔ½¸¥Æ¡¼¥Ö¥ë¡Ë
       01  TBL-YAKUTEI-INF2.
           03  TBL-YT2-DAY                    OCCURS      31.
             05  TBL-YT2X-KAIT                  OCCURS     3.
               06  TBL-YT2X1-OCC                  OCCURS  25.
                 07  TBL-YT2X1-RECE04-REC         PIC X(250).
                 07  TBL-YT2X1-KAISU              PIC 9(03).
               06  TBL-YT2X2-OCC                  OCCURS   3.
                 07  TBL-YT2X2-RECE04-REC         PIC X(250).
                 07  TBL-YT2X2-KAISU              PIC 9(03).
             05  TBL-YT2Y-KAIT                  OCCURS     3.
               06  TBL-YT2Y1-OCC                  OCCURS  26.
                 07  TBL-YT2Y1-HIKAKU.
                   09  TBL-YT2Y1-ZAITEN           PIC 9(08).
                   09  TBL-YT2Y1-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT2Y1-SURYOUTOTAL      PIC 9(07)V9(03).
                   09  TBL-YT2Y1-RUIKEITOTAL      PIC 9(15)V9(03).
                   09  TBL-YT2Y1-MEISAISU         PIC 9(04).
               06  TBL-YT2Y2-OCC                  OCCURS   4.
                 07  TBL-YT2Y2-HIKAKU.
                   09  TBL-YT2Y2-ZAITEN           PIC 9(08).
                   09  TBL-YT2Y2-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT2Y2-SURYOUTOTAL      PIC 9(07)V9(03).
                   09  TBL-YT2Y2-RUIKEITOTAL      PIC 9(15)V9(03).
                   09  TBL-YT2Y2-MEISAISU         PIC 9(04).
      *
      *    ÌôºÞÄþ¸º¾ðÊó¡Ê¤Þ¤È¤áÍÑ¤ÎÊÔ½¸¥Æ¡¼¥Ö¥ë¡Ë
       01  TBL-YAKUTEI-INF3.
           03  TBL-YT3-XXX                    OCCURS     100.
             05  TBL-YT3X-KAIT.
               06  TBL-YT3X1-OCC                  OCCURS  25.
                 07  TBL-YT3X1-RECE04-REC         PIC X(250).
                 07  TBL-YT3X1-KAISU              PIC 9(03).
               06  TBL-YT3X2-OCC                  OCCURS   3.
                 07  TBL-YT3X2-RECE04-REC         PIC X(250).
                 07  TBL-YT3X2-KAISU              PIC 9(03).
             05  TBL-YT3Y-KAIT.
               06  TBL-YT3Y1-OCC                  OCCURS  26.
                 07  TBL-YT3Y1-HIKAKU.
                   09  TBL-YT3Y1-ZAITEN           PIC 9(08).
                   09  TBL-YT3Y1-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT3Y1-SURYOUTOTAL      PIC 9(07)V9(03).
                   09  TBL-YT3Y1-RUIKEITOTAL      PIC 9(15)V9(03).
                   09  TBL-YT3Y1-MEISAISU         PIC 9(04).
               06  TBL-YT3Y2-OCC                  OCCURS   4.
                 07  TBL-YT3Y2-HIKAKU.
                   09  TBL-YT3Y2-ZAITEN           PIC 9(08).
                   09  TBL-YT3Y2-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT3Y2-SURYOUTOTAL      PIC 9(07)V9(03).
                   09  TBL-YT3Y2-RUIKEITOTAL      PIC 9(15)V9(03).
                   09  TBL-YT3Y2-MEISAISU         PIC 9(04).
      *
      *    ¥Ñ¥¹Ì¾ÎÎ°è
       01  WRK-PATH                        PIC X(64).
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
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    ¼õ¿ÇÍúÎò
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
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
                   UNTIL   SPA-STS  =   2
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
           INITIALIZE                  SPA2-AREA
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
           MOVE    "ORCR0040"      TO  JOB-PGID
           MOVE    "¥³¥á¥ó¥ÈÊÔ½¸"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           PERFORM 990-RECE02-READ-SEC
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
           MOVE    "RECE020"           TO  RECE01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
      *
           MOVE    "RECE04W"           TO  RECE04PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE04PARA-TERMID
      *
           MOVE    "RECE050"           TO  RECE05PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE05PARA-TERMID
      *
           MOVE    "RECE041"           TO  RECE041PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE041PARA-TERMID
      *
           MOVE    "RECE051"           TO  RECE051PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE051PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    INPUT               RECE02-FILE
      *
           OPEN    INPUT               RECE04-FILE
      *
           OPEN    INPUT               RECE05-FILE
      *
           OPEN    OUTPUT              RECE0441-FILE
           CLOSE                       RECE0441-FILE
      *
           OPEN    OUTPUT              RECE0551-FILE
           CLOSE                       RECE0551-FILE
      *
           OPEN    OUTPUT              RECE041-FILE
      *
           OPEN    OUTPUT              RECE051-FILE
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
           MOVE       ZERO             TO  CNT-SANTEIKA
           INITIALIZE                      TBL-COUNT-INF
           INITIALIZE                      WRK-SAISHIN4-AREA
           INITIALIZE                      WRK-JINZOU-AREA
           INITIALIZE                      WRK-RIGAKU-AREA
           INITIALIZE                      WRK-TAIHI-ZAINUM
      *
           INITIALIZE                      TBL-YAKUTEI-INF
      *
           MOVE       ZERO             TO  IDX6
      *
           MOVE  RECE02-KOHBUNRI-MIN   TO  WRK-KOHBUNRI-JIDO
      *
           IF   RECE02-COMCNTFLG   =   "1"
      *
             INITIALIZE                      RECE04-REC
             MOVE    RECE02-HOSPID       TO  RECE04-HOSPID
             MOVE    RECE02-SRYYM        TO  RECE04-SRYYM
             MOVE    RECE02-NYUGAIKBN    TO  RECE04-NYUGAIKBN
             MOVE    RECE02-PTID         TO  RECE04-PTID
             MOVE    RECE02-RECEKA       TO  RECE04-RECEKA
             MOVE    RECE02-TEISYUTUSAKI TO  RECE04-TEISYUTUSAKI
             MOVE    RECE02-RECESYUBETU  TO  RECE04-RECESYUBETU
             MOVE    RECE02-HKNJANUM-KEY TO  RECE04-HKNJANUM
             MOVE    RECE02-HOJOKBN-KEY  TO  RECE04-HOJOKBN
             MOVE    RECE02-TEKSTYMD     TO  RECE04-TEKSTYMD
             MOVE    ZERO                TO  RECE04-ZAINUM
      *
             START   RECE04-FILE
                 KEY IS    >=   RECE04-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE04
                 NOT INVALID
                     PERFORM 990-RECE04-NEXT-SEC
             END-START
             PERFORM
               UNTIL  ( FLG-RECE04           =   1                   )
               OR     ( RECE02-HOSPID    NOT =   RECE04-HOSPID       )
               OR     ( RECE02-SRYYM     NOT =   RECE04-SRYYM        )
               OR     ( RECE02-NYUGAIKBN NOT =   RECE04-NYUGAIKBN    )
               OR     ( RECE02-PTID      NOT =   RECE04-PTID         )
               OR     ( RECE02-RECEKA    NOT =   RECE04-RECEKA       )
               OR     ( RECE02-TEISYUTUSAKI
                                         NOT =   RECE04-TEISYUTUSAKI )
               OR     ( RECE02-RECESYUBETU
                                         NOT =   RECE04-RECESYUBETU  )
               OR     ( RECE02-HKNJANUM-KEY
                                         NOT =   RECE04-HKNJANUM     )
               OR     ( RECE02-HOJOKBN-KEY
                                         NOT =   RECE04-HOJOKBN      )
               OR     ( RECE02-TEKSTYMD  NOT =   RECE04-TEKSTYMD     )
      *
               EVALUATE      RECE04-SRYKBN
                   WHEN      "11"
      *            ¡Ê½é¿ÇÎÁ»»Äê²Ê¡§¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-SYOSANTEIKA-COMMENT  =  "1"
                             ADD   RECE04-ZAIKAISU   TO   CNT-SANTEIKA
                      END-IF
                   WHEN      "12"
      *            ¡ÊºÆ¿ÇÎÁ»»Äê²Ê¡§¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-SAISANTEIKA-COMMENT  =  "1"
                             ADD   RECE04-ZAIKAISU   TO   CNT-SANTEIKA
                      END-IF
      *
      *            ¡ÊÅÅÏÃºÆ¿Ç¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-DENWASAI-COMMENT     =  "1"
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        ADD   RECE04-ZAIKAISU  
                                   TO   CNT-DENWA(WRK-KOHBUNRI)
                        MOVE  RECE04-HKNCOMBI
                                   TO   TBL-COUNT-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
      *            ¡ÊÆ±ÆüºÆ¿Ç¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-DOUJITUSAI-COMMENT   =  "1"
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        ADD   RECE04-ZAIKAISU  
                                   TO   CNT-DOUJITU(WRK-KOHBUNRI)
                        MOVE  RECE04-HKNCOMBI
                                   TO   TBL-COUNT-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
      *
      *            ¡ÊºÆ¿Ç£´²óÌÜ¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-SAIJINKO-COMMENT     =  "1"
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        PERFORM  VARYING   IDX1  FROM  1  BY  1
                                 UNTIL     IDX1  >   31
                            IF   RECE04-KAISU(1 IDX1)  NOT = ZERO
                                MOVE  "1"  TO  
                                      WRK-SAISHIN4(WRK-KOHBUNRI IDX1)
                            END-IF
                        END-PERFORM
                      END-IF
                   WHEN      "40"
      *            ¡Ê¿Í¹©¿ÕÂ¡¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-JINKOJIN-COMMENT     =  "1"
                        MOVE    RECE04-REC    TO      WRK-RECE04-REC
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        PERFORM  VARYING   IDX1  FROM  1  BY  1
                                 UNTIL     IDX1  >   31
                            IF   RECE04-KAISU(1 IDX1)  NOT = ZERO
                                 MOVE  "1"  TO  
                                       WRK-JINZOU(WRK-KOHBUNRI IDX1)
                            END-IF
                        END-PERFORM
                        MOVE   RECE04-HKNCOMBI  TO
                                       WRK-JINZOU-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                   WHEN      "80"
      *            ¡Ê¥ê¥Ï¥Ó¥ê¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-RIHABIRI-COMMENT     =  "1"
                        PERFORM    2001-RIHA-COUNT-SEC
                      END-IF
               END-EVALUATE
      *
               PERFORM 990-RECE04-NEXT-SEC
      *
             END-PERFORM
      *
           END-IF
      *
      ******
      *
      *    ÆýÍÄ»ù¡¦ÍÄ»ù¤Î¥³¥á¥ó¥È
      *
           IF   RECE02-NYUYOJIFLG   NOT =   SPACE
      *
              PERFORM   2001-NYUYOJI-COMMENT-SEC
      *
           END-IF
      *
      *    ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥È
      *
           IF   RECE02-KOHJNISSUTBLINF    =   ALL " "
              CONTINUE
           ELSE
              PERFORM   2001-KOHJYUSIN-COMMENT-SEC
           END-IF
      *
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥È
           PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
      *
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç
      *
           IF   CNT-SANTEIKA   >  ZERO
      *
             MOVE       ZERO             TO  CNT-JUSHIN
             INITIALIZE                      TBL-JUSHIN-INF
      *      ¿ÇÎÅ²Ê¤Î¼èÆÀ
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    ZERO                TO  SYS-1005-STYUKYMD
             MOVE    ALL "9"             TO  SYS-1005-EDYUKYMD
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
             MOVE    RECE02-SRYYM        TO  ORC-DBYMD(1:6)
             MOVE    "01"                TO  ORC-DBYMD(7:2)
             CALL    "ORCMCPSUB"         USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
             IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH 
               PERFORM   SYSKANRI-READ-SEC
               PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX             >      50)
                            OR    (FLG-SYSKANRI    =       1)
                   MOVE    SYS-1005-KBNCD  TO TBL-JUSHIN-SRYKA(IDX)
                   MOVE    SYS-1005-SRYKANAME   
                                           TO TBL-JUSHIN-SRYKANAME(IDX)
      *            ¼õ¿ÇÍúÎò¥Æ¡¼¥Ö¥ë¤«¤é¿ÇÎÅ²ÊËè¤Î¼õ¿ÇÆü¤ò¸¡º÷¥»¥Ã¥È
                   PERFORM   2001-JUSHIN-KENSAKU-SEC
      *
                   MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH 
                   PERFORM   SYSKANRI-READ-SEC
               END-PERFORM
             END-IF
      *
             INITIALIZE                        MCPAREA
             MOVE    "DBCLOSE"         TO      MCP-FUNC
             MOVE    "SYSKANRI-KEY2"   TO      ORC-DBPATH
             CALL    "ORCMCPSUB"       USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
      *      Ê£¿ô¤Î¿ÇÎÅ²Ê¼õ¿Ç¤¬¤¢¤ë¤«È½Äê
             MOVE       ZERO       TO   IDZ  
             INITIALIZE            WRK-FREE-AREA
      *
             PERFORM   VARYING   IDX    FROM   1   BY   1
                            UNTIL   (IDX             >      50)
               IF   TBL-JUSHIN-ARI (IDX)  =  "1"
                   ADD       1     TO   CNT-JUSHIN
                   PERFORM  2001-FREE-COMMENT-SEC
               END-IF
             END-PERFORM
      *      Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
             IF    CNT-JUSHIN  >   1
                    PERFORM   2001-JUSHIN-COMMENT-SEC
             END-IF
      *
           END-IF
      *
      *    ÅÅÏÃºÆ¿Ç¡¦Æ±ÆüÊ£¿ôºÆ¿Ç
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX             >      5)
      *
              IF         CNT-DENWA(IDX)     >   ZERO
              AND        CNT-DOUJITU(IDX)   >   ZERO
                         PERFORM    2001-DENWA-DOUJITU-COMMENT-SEC
              ELSE
                  IF         CNT-DENWA(IDX)     >   ZERO
                  AND        CNT-DOUJITU(IDX)   =   ZERO
                         PERFORM    2001-DENWA-COMMENT-SEC
                  ELSE
                      IF         CNT-DENWA(IDX)     =   ZERO
                      AND        CNT-DOUJITU(IDX)   >   ZERO
                         PERFORM    2001-DOUJITU-COMMENT-SEC
                      END-IF
                  END-IF
              END-IF
      *
           END-PERFORM
      *
      *    ¿Í¹©¿ÕÂ¡¤ÎÈ½Äê
      *
           PERFORM    2001-JINZOU-HANTEI-SEC
      *
      *
      *    ¥Ñ¥é¥á¡¼¥¿£°£´¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           INITIALIZE                      RECE04-REC
           MOVE    RECE02-HOSPID       TO  RECE04-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE04-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE04-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE04-PTID
           MOVE    RECE02-RECEKA       TO  RECE04-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE04-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE04-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE04-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE04-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE04-TEKSTYMD
           MOVE    ZERO                TO  RECE04-ZAINUM
      *
           START   RECE04-FILE
               KEY IS    >=   RECE04-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   PERFORM 990-RECE04-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE04           =   1                   )
               OR     ( RECE02-HOSPID    NOT =   RECE04-HOSPID       )
               OR     ( RECE02-SRYYM     NOT =   RECE04-SRYYM        )
               OR     ( RECE02-NYUGAIKBN NOT =   RECE04-NYUGAIKBN    )
               OR     ( RECE02-PTID      NOT =   RECE04-PTID         )
               OR     ( RECE02-RECEKA    NOT =   RECE04-RECEKA       )
               OR     ( RECE02-TEISYUTUSAKI
                                         NOT =   RECE04-TEISYUTUSAKI )
               OR     ( RECE02-RECESYUBETU
                                         NOT =   RECE04-RECESYUBETU  )
               OR     ( RECE02-HKNJANUM-KEY
                                         NOT =   RECE04-HKNJANUM     )
               OR     ( RECE02-HOJOKBN-KEY
                                         NOT =   RECE04-HOJOKBN      )
               OR     ( RECE02-TEKSTYMD  NOT =   RECE04-TEKSTYMD     )
      *
               EVALUATE      RECE04-SRYKBN
                   WHEN      "13"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            ¡ÊËýÀ­áÖÄË¼À´µ´ÉÍýÎÁ»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                         PERFORM    2001-SHIDOU-HANTEI-SEC
                      END-IF
      *            ¡Ê½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ¸òÉÕÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOBYO-HANTEI-SEC
                      END-IF
      *            ¡ÊÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ¸òÉÕÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RYOYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RECE04-TENSOU-SEC
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
                        PERFORM    2001-RECE04-TENSOU-SEC
                      END-IF
                   WHEN      "21"
                      IF   RECE04-COMCDFLG   =   "2"
                        PERFORM    2001-YAKUTEI-HENSYU-SEC
                      ELSE
                        PERFORM    2001-RECE04-TENSOU-SEC
                      END-IF
                   WHEN      "33"
      *            ¡ÊºßÂð´µ¼ÔË¬ÌäÅÀÅ©Ãí¼Í´ÉÍý»ØÆ³ÎÁ»»Äê»þ¤ÎÅÀÅ©Ãí¼Í¤Î
      *              ¥³¥á¥ó¥È¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-HOUTEN-HANTEI-SEC
                   WHEN      "50"
      *            ¡Ê¼ê½ÑÆü¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "70"
      *            ¡Ê£Ã£Ô¡¦£Í£Ò£É¼Â»ÜÆü¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-CTMRI-HANTEI-SEC
                   WHEN      "80"
      *            ¡Ê¥ê¥Ï¥Ó¥ê¤Î¥»¥Ã¥È¡Ë
                      COMPUTE  WRK-KOHBUNRI 
                                 =    RECE04-KOHBUNRI  +  1
                      MOVE    ZERO     TO    FLG-ARI
                      PERFORM   VARYING   IDX    FROM   1   BY   1
                            UNTIL   (IDX             >      18)         
                             OR     (FLG-ARI         =       1)
                          IF  RECE04-ZAINUM  =  
                                   WRK-RIHA-ZAINUM(WRK-KOHBUNRI IDX)
                              MOVE    1    TO   FLG-ARI 
                          END-IF
                      END-PERFORM
                      IF  FLG-ARI  =  1
                          PERFORM   2001-RIHA-COMMENT-SEC
                      ELSE
                          PERFORM   2001-RECE04-TENSOU-SEC
                      END-IF
                   WHEN   OTHER
                      PERFORM    2001-RECE04-TENSOU-SEC
               END-EVALUATE
      *
               PERFORM 990-RECE04-NEXT-SEC
      *
           END-PERFORM
      *
      *****ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ëÊÔ½¸£²****
           PERFORM     240-YAKUTEI-HENSYU2-SEC
      *
           PERFORM 990-RECE02-READ-SEC
      *
           IF  CNT-IN-COMMIT   =    300
                 MOVE  ZERO          TO      CNT-IN-COMMIT
                 PERFORM 900-DBCOMMIT-SEC
                 PERFORM 900-DBSTART-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼õ¿ÇÍúÎò¥Æ¡¼¥Ö¥ë¤«¤é¿ÇÎÅ²ÊËè¤Î¼õ¿ÇÆü¤ò¸¡º÷¥»¥Ã¥È
      *****************************************************************
       2001-JUSHIN-KENSAKU-SEC             SECTION.
      *
      *    ¼õ¿ÇÎò¤Î¸¡º÷
           INITIALIZE                      JYURRK-REC
           MOVE    RECE02-HOSPID       TO  JYURRK-HOSPID
           MOVE    RECE02-PTID         TO  JYURRK-PTID
           MOVE    RECE02-NYUGAIKBN    TO  JYURRK-NYUGAIKBN
           MOVE    TBL-JUSHIN-SRYKA(IDX)
                                       TO  JYURRK-SRYKA
           MOVE    RECE02-SRYYM        TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    RECE02-SRYYM        TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY24"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING   MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY24"  TO  ORC-DBPATH 
               PERFORM   JYURRK-READ-SEC
               PERFORM   UNTIL   FLG-JYURRK   =    1
                   IF    JYURRK-HKNKBN    =       "0"
                      MOVE   JYURRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUMX
                      IF  WRK-HKNCOMBINUM9  =  RECE02-HKNCOMBI(1) OR
                                               RECE02-HKNCOMBI(2) OR
                                               RECE02-HKNCOMBI(3) OR
                                               RECE02-HKNCOMBI(4) OR
                                               RECE02-HKNCOMBI(5) OR
                                               RECE02-HKNCOMBI(6) OR
                                               RECE02-HKNCOMBI(7) OR
                                               RECE02-HKNCOMBI(8) OR
                                               RECE02-HKNCOMBI(9) OR
                                               RECE02-HKNCOMBI(10)
                            MOVE   JYURRK-SRYYMD   TO   WRK-SRYYMDX
                            IF   RECE02-JNISSUTBL(WRK-SRYDD) = SPACE
                               CONTINUE
                            ELSE
                               MOVE   "1"     TO    
                                        TBL-JUSHIN-ARI (IDX)
                               MOVE   "1"     TO    
                                        TBL-JUSHIN-DAY (IDX WRK-SRYDD)
                            END-IF
                      END-IF      
                   END-IF
      *
                   MOVE    "JYURRK-KEY24"  TO  ORC-DBPATH 
                   PERFORM   JYURRK-READ-SEC
               END-PERFORM
           END-IF
      *
           INITIALIZE                        MCPAREA
           MOVE    "DBCLOSE"         TO      MCP-FUNC
           MOVE    "JYURRK-KEY24"    TO      ORC-DBPATH
           CALL    "ORCMCPSUB"       USING
                                     MCPAREA
                                     ORCMCPAREA
                                     MCPDATA-REC
      *
           .
       2001-JUSHIN-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸
      *****************************************************************
       2001-FREE-COMMENT-SEC          SECTION.
      *
           ADD      1                      TO  IDZ
           MOVE     ZERO                   TO  WRK-KETA
                                               WRK-KETA-TAIHI
                                               WRK-KETA2
      *
           IF  TBL-JUSHIN-SRYKANAME(IDX)  NOT =  SPACE
             INITIALIZE                      ORCSKANACHKAREA
             MOVE    "2"                     TO  KANACHK-SYORI 
             MOVE  TBL-JUSHIN-SRYKANAME(IDX) TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
                 COMPUTE WRK-KETA          =   KANACHK-MAX
                 COMPUTE WRK-KETA-TAIHI    =   KANACHK-MAX
                 COMPUTE WRK-KETA2         =   KANACHK-MAX  /  2
                 MOVE    WRK-KETA2   TO  WK-FREE-YUKOKETA(IDZ)
                 MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                   TO  WK-FREE-NAME(IDZ)(1:KANACHK-MAX)
             END-IF    
           END-IF
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDY    FROM   1   BY   1
                          UNTIL   (IDY             >      31)
      *
             IF  TBL-JUSHIN-DAY(IDX IDY) =  "1"
                 IF  FLG-FIRST  =  ZERO
                    IF  IDY     <  10 
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "¡¡"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)(3:2)     
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    "Æü"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                    ELSE
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "¡¡"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)     
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                       MOVE    "Æü"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                    END-IF
                    MOVE      1              TO  FLG-FIRST
                 ELSE
                    IF  IDY     <  10 
                       IF    WK-FREE-YUKOKETA(IDZ)  =  25
                          PERFORM  2001-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "¡¢"
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "¡¢"
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA(IDZ)  =  24 OR 25
                          PERFORM  2001-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2) 
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2) 
                       END-IF
                    ELSE
                       IF    WK-FREE-YUKOKETA(IDZ)  =  25
                          PERFORM  2001-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "¡¢"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "¡¢"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA(IDZ)  =  23 OR 24 OR 25
                          PERFORM  2001-FREE-COMMENT-KAIGYO-SEC
                          ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       END-IF
                    END-IF
                 END-IF
             END-IF
      *
           END-PERFORM
           .
       2001-FREE-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¡Ê²þ¹Ô¡Ë
      *****************************************************************
       2001-FREE-COMMENT-KAIGYO-SEC          SECTION.
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA(IDZ)
           MOVE    ALL "¡¡"        TO  WK-FREE-NAME(IDZ)(1:WRK-KETA)
      *
           .
       2001-FREE-COMMENT-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-JUSHIN-COMMENT-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                TO  IDX-RECE05
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
      *
           ADD     1           TO  IDX-RECE05
           MOVE    "810000001" TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
           MOVE    ZERO        TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
           MOVE    7           TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
           MOVE    "Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   49    )
                         OR      ( WK-FREE-NAME(IDX) =   SPACE )
                ADD    1       TO  IDX-RECE05
                MOVE   "810000001"
                               TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO    TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA(IDX)
                               TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME(IDX)
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           END-PERFORM 
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                INITIALIZE                       RECE051-REC
                MOVE    RECE02-HOSPID        TO  RECE051-HOSPID
                MOVE    RECE02-SRYYM         TO  RECE051-SRYYM
                MOVE    RECE02-NYUGAIKBN     TO  RECE051-NYUGAIKBN
                MOVE    RECE02-PTID          TO  RECE051-PTID
                MOVE    RECE02-RECEKA        TO  RECE051-RECEKA
                MOVE    RECE02-TEISYUTUSAKI  TO  RECE051-TEISYUTUSAKI
                MOVE    RECE02-RECESYUBETU   TO  RECE051-RECESYUBETU
                MOVE    RECE02-HKNJANUM-KEY  TO  RECE051-HKNJANUM
                MOVE    RECE02-HOJOKBN-KEY   TO  RECE051-HOJOKBN
                MOVE    RECE02-TEKSTYMD      TO  RECE051-TEKSTYMD
      *         MOVE    ZERO                 TO  RECE051-ZAINUM
                MOVE    99999999             TO  RECE051-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE051-KOHBUNRI
                MOVE    "99"                 TO  RECE051-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE051-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE05   =   1)
                   ADD         1             TO    IDX-RECE05
                   IF  WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE
                       MOVE    1             TO    FLG-WRKRECE05
                   ELSE
                       MOVE  WRK-RECE05XX-SRYACT-TBL (IDX-RECE05 1)
                                        TO   RECE051-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                WRITE   RECE051-REC
                ADD     1                    TO  CNT-RECE05
           END-PERFORM     
      *
      *
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-HOSPID       TO  RECE041-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE041-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE041-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE041-PTID
           MOVE    RECE02-RECEKA       TO  RECE041-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE041-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE041-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE041-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE041-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE041-TEKSTYMD
      *    MOVE    ZERO                TO  RECE041-ZAINUM
           MOVE    99999999            TO  RECE041-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE041-KOHBUNRI
           MOVE    "99"                TO  RECE041-SRYKBN
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE041-MEISAISU
      *
           WRITE   RECE041-REC
           ADD     1                   TO  CNT-RECE04
      *
           .
       2001-JUSHIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÅÏÃºÆ¿Ç¡¦Æ±ÆüÊ£¿ôºÆ¿Ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-DENWA-DOUJITU-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-HOSPID       TO  RECE041-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE041-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE041-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE041-PTID
           MOVE    RECE02-RECEKA       TO  RECE041-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE041-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE041-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE041-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE041-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE041-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999990  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE041-ZAINUM
      *    MOVE    99999999            TO  RECE041-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE041-KOHBUNRI
           MOVE    "99"                TO  RECE041-SRYKBN
      *
           MOVE    2                   TO  RECE041-MEISAISU
      *
           MOVE    TBL-COUNT-HKNCOMBI(IDX)
                                       TO  RECE041-HKNCOMBI
      *
           WRITE   RECE041-REC
           ADD     1                   TO  CNT-RECE04
      *
           INITIALIZE                      RECE051-REC
           MOVE    RECE02-HOSPID       TO  RECE051-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE051-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE051-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE051-PTID
           MOVE    RECE02-RECEKA       TO  RECE051-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE051-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE051-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE051-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE051-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE051-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999990  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE051-ZAINUM
      *    MOVE    99999999            TO  RECE051-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE051-KOHBUNRI
           MOVE    "99"                TO  RECE051-SRYKBN
      *    ÅÅÏÃºÆ¿Ç¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
           MOVE    "810000001"          TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE05XX-SRYSURYO(1 1)
           MOVE    9                    TO  WRK-RECE05XX-YUKOKETA(1 1)
           MOVE    "ÅÅÏÃºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë" TO  WRK-RECE05XX-NAME(1 1)
      ***  MOVE    CNT-DENWA(IDX)       TO  DAY-IDX
      ***  MOVE    TBL-DAY(DAY-IDX)     TO  WRK-RECE05XX-NAME(1 1)(11:4)
      *
           MOVE    CNT-DENWA(IDX)       TO  DAY-IDX2
           PERFORM    20011-KAISU-HENSYU-SEC
           MOVE    WRK-HENSYU-KAISU2    TO  WRK-RECE05XX-NAME(1 1)(11:4)
      *
           MOVE    WRK-RECE05XX-SRYACT-TBL (1 1)
                                        TO  RECE051-SRYACT-TBL(1)
      *    Æ±ÆüÊ£¿ôºÆ¿Ç¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    "810000001"          TO  WRK-RECE05XX-SRYCD(2 1)
           MOVE    ZERO                 TO  WRK-RECE05XX-SRYSURYO(2 1)
           MOVE    11                   TO  WRK-RECE05XX-YUKOKETA(2 1)
           MOVE    "Æ±ÆüÊ£¿ôºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë"
                                        TO  WRK-RECE05XX-NAME(2 1)
      ***  MOVE    CNT-DOUJITU(IDX)     TO  DAY-IDX
      ***  MOVE    TBL-DAY(DAY-IDX)     TO  WRK-RECE05XX-NAME(2 1)(15:4)
      *
           MOVE    CNT-DOUJITU(IDX)     TO  DAY-IDX2
           PERFORM    20011-KAISU-HENSYU-SEC
           MOVE    WRK-HENSYU-KAISU2    TO  WRK-RECE05XX-NAME(2 1)(15:4)
      *
           MOVE    WRK-RECE05XX-SRYACT-TBL(2 1)
                                        TO  RECE051-SRYACT-TBL (2)
      *
           WRITE   RECE051-REC
           ADD     1                    TO  CNT-RECE05
      *
           .
       2001-DENWA-DOUJITU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÅÏÃºÆ¿Ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-DENWA-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-HOSPID       TO  RECE041-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE041-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE041-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE041-PTID
           MOVE    RECE02-RECEKA       TO  RECE041-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE041-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE041-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE041-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE041-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE041-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999990  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE041-ZAINUM
      *    MOVE    99999999            TO  RECE041-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE041-KOHBUNRI
           MOVE    "99"                TO  RECE041-SRYKBN
      *
           MOVE    1                   TO  RECE041-MEISAISU
      *
           MOVE    TBL-COUNT-HKNCOMBI(IDX)
                                       TO  RECE041-HKNCOMBI
      *
           WRITE   RECE041-REC
           ADD     1                   TO  CNT-RECE04
      *
           INITIALIZE                      RECE051-REC
           MOVE    RECE02-HOSPID       TO  RECE051-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE051-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE051-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE051-PTID
           MOVE    RECE02-RECEKA       TO  RECE051-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE051-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE051-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE051-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE051-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE051-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999990  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE051-ZAINUM
      *    MOVE    99999999            TO  RECE051-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE051-KOHBUNRI
           MOVE    "99"                TO  RECE051-SRYKBN
      *    ÅÅÏÃºÆ¿Ç¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
           MOVE    "810000001"          TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE05XX-SRYSURYO(1 1)
           MOVE    9                    TO  WRK-RECE05XX-YUKOKETA(1 1)
           MOVE    "ÅÅÏÃºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë" TO  WRK-RECE05XX-NAME(1 1)
      ***  MOVE    CNT-DENWA(IDX)       TO  DAY-IDX
      ***  MOVE    TBL-DAY(DAY-IDX)     TO  WRK-RECE05XX-NAME(1 1)(11:4)
      *
           MOVE    CNT-DENWA(IDX)       TO  DAY-IDX2
           PERFORM    20011-KAISU-HENSYU-SEC
           MOVE    WRK-HENSYU-KAISU2    TO  WRK-RECE05XX-NAME(1 1)(11:4)
      *
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                        TO  RECE051-SRYACT-TBL(1)
           WRITE   RECE051-REC
           ADD     1                    TO  CNT-RECE05
      *
           .
       2001-DENWA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æ±ÆüÊ£¿ôºÆ¿Ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-DOUJITU-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-HOSPID       TO  RECE041-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE041-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE041-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE041-PTID
           MOVE    RECE02-RECEKA       TO  RECE041-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE041-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE041-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE041-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE041-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE041-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999990  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE041-ZAINUM
      *    MOVE    99999999            TO  RECE041-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE041-KOHBUNRI
           MOVE    "99"                TO  RECE041-SRYKBN
      *
           MOVE    1                   TO  RECE041-MEISAISU
      *
           MOVE    TBL-COUNT-HKNCOMBI(IDX)
                                       TO  RECE041-HKNCOMBI
      *
           WRITE   RECE041-REC
           ADD     1                   TO  CNT-RECE04
      *
           INITIALIZE                      RECE051-REC
           MOVE    RECE02-HOSPID       TO  RECE051-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE051-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE051-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE051-PTID
           MOVE    RECE02-RECEKA       TO  RECE051-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE051-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE051-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE051-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE051-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE051-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999990  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE051-ZAINUM
      *    MOVE    99999999            TO  RECE051-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE051-KOHBUNRI
           MOVE    "99"                TO  RECE051-SRYKBN
      *    Æ±ÆüÊ£¿ôºÆ¿Ç¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
           MOVE    "810000001"          TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE05XX-SRYSURYO(1 1)
           MOVE    11                   TO  WRK-RECE05XX-YUKOKETA(1 1)
           MOVE    "Æ±ÆüÊ£¿ôºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë"
                                        TO  WRK-RECE05XX-NAME(1 1)
      ***  MOVE    CNT-DOUJITU(IDX)     TO  DAY-IDX
      ***  MOVE    TBL-DAY(DAY-IDX)     TO  WRK-RECE05XX-NAME(1 1)(15:4)
      *
           MOVE    CNT-DOUJITU(IDX)     TO  DAY-IDX2
           PERFORM    20011-KAISU-HENSYU-SEC
           MOVE    WRK-HENSYU-KAISU2    TO  WRK-RECE05XX-NAME(1 1)(15:4)
      *
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                        TO  RECE051-SRYACT-TBL(1)
           WRITE   RECE051-REC
           ADD     1                    TO  CNT-RECE05
      *
           .
       2001-DOUJITU-COMMENT-EXT.
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
      *    ¥ê¥Ï¥Ó¥ê¤Î¥«¥¦¥ó¥È
      *****************************************************************
       2001-RIHA-COUNT-SEC          SECTION.
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( IDX           >   TBL-RIHA-MAX )
      *
              INITIALIZE                        SRYACT-REC
              MOVE    RECE04-HOSPID     TO      SRY-HOSPID
              MOVE    RECE04-NYUGAIKBN  TO      SRY-NYUGAIKBN
              MOVE    RECE04-PTID       TO      SRY-PTID
              MOVE    RECE04-SRYYM      TO      SRY-SRYYM
              MOVE    RECE04-ZAINUM     TO      SRY-ZAINUM
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
                                 =    RECE04-KOHBUNRI  +  1
                     IF  RECE04-ZAINUM  >  
                                     WRK-RIHA-ZAINUM(WRK-KOHBUNRI IDX2)
                         MOVE  RECE04-ZAINUM  TO  
                                     WRK-RIHA-ZAINUM(WRK-KOHBUNRI IDX2)
                     END-IF
                     PERFORM  VARYING   IDX1  FROM  1  BY  1
                              UNTIL     IDX1  >   31
                         IF   RECE04-KAISU(1 IDX1)  NOT = ZERO
                              MOVE  "1"  TO  
                                    WRK-RIGAKU(WRK-KOHBUNRI IDX2 IDX1)
                         END-IF
                     END-PERFORM
                  END-IF
              END-IF
      *
              INITIALIZE                        MCPAREA
              MOVE    "DBCLOSE"         TO      MCP-FUNC
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
      *    ¿Í¹©¿ÕÂ¡¤ÎÈ½Äê
      *****************************************************************
       2001-JINZOU-HANTEI-SEC          SECTION.
      *
      *    ¿Í¹©¿ÕÂ¡¤ÈºÆ¿Ç¤ÎÆÍ¤­¹ç¤ï¤»
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( IDX           >   5  )
      *
              MOVE       ZERO             TO     CNT-JINZOU
              PERFORM    VARYING    IDY   FROM   1   BY   1
                      UNTIL      IDY   >    31
                  IF   (WRK-JINZOU(IDX IDY)    =   "1")  AND
                       (WRK-SAISHIN4(IDX IDY)  =   "1")
                       ADD     1           TO   CNT-JINZOU
                  END-IF
              END-PERFORM
      *
              IF      CNT-JINZOU        NOT =      ZERO
                  PERFORM    2001-JINZOU-COMMENT-SEC
              END-IF
      *
           END-PERFORM
      *
           .
       2001-JINZOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿Í¹©¿ÕÂ¡¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-JINZOU-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-HOSPID       TO  RECE041-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE041-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE041-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE041-PTID
           MOVE    RECE02-RECEKA       TO  RECE041-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE041-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE041-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE041-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE041-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE041-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999980  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE041-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE041-KOHBUNRI
           MOVE    WRK-RECE04-SRYKBN   TO  RECE041-SRYKBN
      *
           MOVE    1                   TO  RECE041-MEISAISU
      *
           MOVE    WRK-JINZOU-HKNCOMBI(IDX)
                                       TO  RECE041-HKNCOMBI
      *
           WRITE   RECE041-REC
           ADD     1                   TO  CNT-RECE04
      *
           INITIALIZE                      RECE051-REC
           MOVE    RECE02-HOSPID       TO  RECE051-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE051-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE051-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE051-PTID
           MOVE    RECE02-RECEKA       TO  RECE051-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE051-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE051-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE051-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE051-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE051-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999980  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE051-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE051-KOHBUNRI
           MOVE    WRK-RECE04-SRYKBN   TO  RECE051-SRYKBN
      *    ¿Í¹©¿ÕÂ¡¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
           MOVE    "810000001"          TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE05XX-SRYSURYO(1 1)
           MOVE    14                   TO  WRK-RECE05XX-YUKOKETA(1 1)
           MOVE    "ºÆ¿Ç£´²óÌÜ°Ê¹ß¼Â»Ü²ó¿ô¡¡¡¡²ó"
                                        TO  WRK-RECE05XX-NAME(1 1)
           MOVE    CNT-JINZOU           TO  DAY-IDX
           MOVE    TBL-DAY(DAY-IDX)     TO  WRK-RECE05XX-NAME(1 1)(23:4)
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                        TO  RECE051-SRYACT-TBL(1)
           WRITE   RECE051-REC
           ADD     1                    TO  CNT-RECE05
      *
           .
       2001-JINZOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆýÍÄ»ù¡¦ÍÄ»ù¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-NYUYOJI-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-HOSPID       TO  RECE041-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE041-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE041-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE041-PTID
           MOVE    RECE02-RECEKA       TO  RECE041-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE041-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE041-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE041-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE041-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE041-TEKSTYMD
           MOVE    99999990            TO  RECE041-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE041-KOHBUNRI
           MOVE    "99"                TO  RECE041-SRYKBN
      *
           MOVE    1                   TO  RECE041-MEISAISU
      *
           WRITE   RECE041-REC
           ADD     1                   TO  CNT-RECE04
      *
           INITIALIZE                      RECE051-REC
           MOVE    RECE02-HOSPID       TO  RECE051-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE051-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE051-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE051-PTID
           MOVE    RECE02-RECEKA       TO  RECE051-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE051-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE051-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE051-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE051-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE051-TEKSTYMD
           MOVE    99999990            TO  RECE051-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE051-KOHBUNRI
           MOVE    "99"                TO  RECE051-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
           EVALUATE  RECE02-NYUYOJIFLG
              WHEN   "1"
                 MOVE    "810000001"    TO  WRK-RECE05XX-SRYCD(1 1)
                 MOVE     ZERO          TO  WRK-RECE05XX-SRYSURYO(1 1)
                 MOVE     11            TO  WRK-RECE05XX-YUKOKETA(1 1)
                 MOVE    "¡¡¡¡·î¡¡¡¡Æü¤Þ¤ÇÆýÍÄ»ù" 
                                        TO  WRK-RECE05XX-NAME(1 1)
              WHEN   "2"
                 MOVE    "810000001"    TO  WRK-RECE05XX-SRYCD(1 1)
                 MOVE     ZERO          TO  WRK-RECE05XX-SRYSURYO(1 1)
                 MOVE     10            TO  WRK-RECE05XX-YUKOKETA(1 1)
                 MOVE    "¡¡¡¡·î¡¡¡¡Æü¤Þ¤ÇÍÄ»ù" 
                                        TO  WRK-RECE05XX-NAME(1 1)
           END-EVALUATE
      *
           MOVE    RECE02-BIRTHDAY(5:2) TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)    TO  WRK-RECE05XX-NAME(1 1)(1:4)
           MOVE    RECE02-BIRTHDAY(7:2) TO  DAY-IDX2
           COMPUTE    DAY-IDX2   =   DAY-IDX2  -   1
           MOVE    TBL-DAY(DAY-IDX2)    TO  WRK-RECE05XX-NAME(1 1)(7:4)
      *
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                        TO  RECE051-SRYACT-TBL(1)
           WRITE   RECE051-REC
           ADD     1                    TO  CNT-RECE05
      *
           .
       2001-NYUYOJI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-KOHJYUSIN-COMMENT-SEC          SECTION.
      *
           INITIALIZE                TBL-SANTEI-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF   RECE02-KOHJNISSUTBL(IDX)  NOT = SPACE
                      ADD   1     TO   DAY-IDX
                      MOVE  IDX   TO   TB-SANTEI-DAY(DAY-IDX)
                END-IF
           END-PERFORM
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           MOVE       5          TO   FLG-COMMENT
      *
           PERFORM  2001-FREE-COMMENT2-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                TO  IDX-RECE05
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1     TO  IDX-RECE05
                MOVE   "810000001"
                             TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO  TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           END-PERFORM 
      *
      *    ¼õ¿ÇÆü¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                INITIALIZE                       RECE051-REC
                MOVE    RECE02-HOSPID        TO  RECE051-HOSPID
                MOVE    RECE02-SRYYM         TO  RECE051-SRYYM
                MOVE    RECE02-NYUGAIKBN     TO  RECE051-NYUGAIKBN
                MOVE    RECE02-PTID          TO  RECE051-PTID
                MOVE    RECE02-RECEKA        TO  RECE051-RECEKA
                MOVE    RECE02-TEISYUTUSAKI  TO  RECE051-TEISYUTUSAKI
                MOVE    RECE02-RECESYUBETU   TO  RECE051-RECESYUBETU
                MOVE    RECE02-HKNJANUM-KEY  TO  RECE051-HKNJANUM
                MOVE    RECE02-HOJOKBN-KEY   TO  RECE051-HOJOKBN
                MOVE    RECE02-TEKSTYMD      TO  RECE051-TEKSTYMD
                MOVE    99999980             TO  RECE051-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE051-KOHBUNRI
                MOVE    "99"                 TO  RECE051-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE051-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE05   =   1)
                   ADD         1             TO    IDX-RECE05
                   IF  WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE
                       MOVE    1             TO    FLG-WRKRECE05
                   ELSE
                       MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                        TO   RECE051-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                WRITE   RECE051-REC
                ADD     1                    TO  CNT-RECE05
           END-PERFORM
      *
      *
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-HOSPID       TO  RECE041-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE041-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE041-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE041-PTID
           MOVE    RECE02-RECEKA       TO  RECE041-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE041-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE041-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE041-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE041-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE041-TEKSTYMD
           MOVE    99999980            TO  RECE041-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE041-KOHBUNRI
           MOVE    "99"                TO  RECE041-SRYKBN
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE041-MEISAISU
      *
           WRITE   RECE041-REC
           ADD     1                   TO  CNT-RECE04
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
           MOVE     ZERO     TO     WRK-KYUSEIYMD
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                  SANTEI-REC
           MOVE    RECE02-HOSPID       TO  SANTEI-HOSPID
           MOVE    RECE02-PTID         TO  SANTEI-PTID
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
                PERFORM        SANTEI-READ-SEC
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
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE02-SRYYM
                         AND  RECE02-SRYYM        <=  WRK-AFTER180(1:6)
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
                   PERFORM        SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF      FLG-KYUSEI-COMMENT    =      1
               PERFORM    2001-KYUSEIHASSYO-COMMENT-SEC
           END-IF
      *
           .
       2001-KYUSEIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-KYUSEIHASSYO-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-HOSPID       TO  RECE041-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE041-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE041-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE041-PTID
           MOVE    RECE02-RECEKA       TO  RECE041-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE041-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE041-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE041-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE041-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE041-TEKSTYMD
           MOVE    99999998            TO  RECE041-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE041-KOHBUNRI
           MOVE    "99"                TO  RECE041-SRYKBN
      *
           MOVE    1                   TO  RECE041-MEISAISU
      *
           WRITE   RECE041-REC
           ADD     1                   TO  CNT-RECE04
      *
           INITIALIZE                      RECE051-REC
           MOVE    RECE02-HOSPID       TO  RECE051-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE051-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE051-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE051-PTID
           MOVE    RECE02-RECEKA       TO  RECE051-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE051-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE051-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE051-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE051-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE051-TEKSTYMD
           MOVE    99999998            TO  RECE051-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE051-KOHBUNRI
           MOVE    "99"                TO  RECE051-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
           MOVE    "810000001"          TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE     ZERO                TO  WRK-RECE05XX-SRYSURYO(1 1)
           MOVE     9                   TO  WRK-RECE05XX-YUKOKETA(1 1)
           MOVE    "È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü" TO  WRK-RECE05XX-NAME(1 1)
           MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)    TO  WRK-RECE05XX-NAME(1 1)(7:4)
           MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)    TO  WRK-RECE05XX-NAME(1 1)(13:4)
      *
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                        TO  RECE051-SRYACT-TBL(1)
           WRITE   RECE051-REC
           ADD     1                    TO  CNT-RECE05
      *
           .
       2001-KYUSEIHASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ËýÀ­áÖÄË¼À´µ´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SHIDOU-HANTEI-SEC          SECTION.
      *
           IF   RECE04-FLG-SHIDOU-COMMENT   =    "1"
      *
             MOVE     ZERO     TO     FLG-SHIDOU-COMMENT
      *
      *      »»ÄêÍúÎò¸¡º÷
             INITIALIZE                  SANTEI-REC
             MOVE    RECE04-HOSPID       TO  SANTEI-HOSPID
             MOVE    RECE04-PTID         TO  SANTEI-PTID
             MOVE    RECE04-SRYYM        TO  SANTEI-SRYYM
             MOVE    CONS-MANSEI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
             CALL    "ORCMCPSUB"         USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC
             IF      MCP-RC              =   ZERO
                    MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
                    PERFORM        SANTEI-READ-SEC
                    PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                       IF     SANTEI-FSANTEIYMD(1:6)  =  RECE04-SRYYM   
                             MOVE    1    TO      FLG-SHIDOU-COMMENT
                       END-IF
      * 
                       MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
                       PERFORM        SANTEI-READ-SEC
                    END-PERFORM
             END-IF
      *
             MOVE    "DBCLOSE"           TO  MCP-FUNC
             MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
             CALL    "ORCMCPSUB"         USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC
      *
             IF      FLG-SHIDOU-COMMENT    =      1
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    2001-SHIDOU-COMMENT-SEC
             END-IF         
      *
           END-IF
      *
           .
       2001-SHIDOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ËýÀ­áÖÄË¼À´µ´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-SHIDOU-COMMENT-SEC          SECTION.
      *
           PERFORM   2001-SANTEI-DAY-SEC
      *
           MOVE       RECE04-REC    TO   RECE041-REC
           ADD     1                TO   RECE041-MEISAISU
           WRITE      RECE041-REC
           ADD     1                TO   CNT-RECE04
      *
           INITIALIZE                      RECE05-REC
           MOVE    RECE04-HOSPID       TO  RECE05-HOSPID
           MOVE    RECE04-SRYYM        TO  RECE05-SRYYM
           MOVE    RECE04-NYUGAIKBN    TO  RECE05-NYUGAIKBN
           MOVE    RECE04-PTID         TO  RECE05-PTID
           MOVE    RECE04-RECEKA       TO  RECE05-RECEKA
           MOVE    RECE04-TEISYUTUSAKI TO  RECE05-TEISYUTUSAKI
           MOVE    RECE04-RECESYUBETU  TO  RECE05-RECESYUBETU
           MOVE    RECE04-HKNJANUM     TO  RECE05-HKNJANUM
           MOVE    RECE04-HOJOKBN      TO  RECE05-HOJOKBN
           MOVE    RECE04-TEKSTYMD     TO  RECE05-TEKSTYMD
           MOVE    RECE04-ZAINUM       TO  RECE05-ZAINUM
           START   RECE05-FILE
                 KEY IS    >=   RECE05-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE05
                 NOT INVALID
                     PERFORM 990-RECE05-NEXT-SEC
           END-START
      *
           MOVE    RECE05-REC           TO  RECE051-REC
           MOVE    RECE05-SRYACT-TBL(1)  TO  RECE051-SRYACT-TBL(1)
           MOVE    RECE05-SRYACT-TBL(2)  TO  RECE051-SRYACT-TBL(3)
           MOVE    RECE05-SRYACT-TBL(3)  TO  RECE051-SRYACT-TBL(4)
           MOVE    RECE05-SRYACT-TBL(4)  TO  RECE051-SRYACT-TBL(5)
      *
           MOVE    SPACE                TO  RECE051-SRYACT-TBL(2)
           INITIALIZE                       RECE051-SRYACT-TBL(2)
           MOVE    "810000001"          TO  RECE051-SRYCD(2)
           MOVE    ZERO                 TO  RECE051-SRYSURYO(2)
           MOVE    15                   TO  RECE051-YUKOKETA(2)
           MOVE    "ËýÀ­áÖÄË¼À´µ´ÉÍý»»ÄêÆü¡¡¡¡¡¡Æü"
                                        TO  RECE051-NAME(2)
           MOVE     TB-SANTEI-DAY(1)    TO  DAY-IDX
           MOVE     TBL-DAY(DAY-IDX)    TO  RECE051-NAME(2)(25:4)
           WRITE    RECE051-REC
           ADD      1                    TO  CNT-RECE05
      *
           .
       2001-SHIDOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SYOBYO-HANTEI-SEC          SECTION.
      *
           IF      RECE04-FLG-SYOBYO-COMMENT   =    "1"
                MOVE       4       TO     FLG-COMMENT
                PERFORM    2001-SHUJUTSU-ETC2-COMMENT-SEC
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
           IF      RECE04-FLG-RYOYO-COMMENT    =    "1"
                MOVE       4       TO     FLG-COMMENT
                PERFORM    2001-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-RYOYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOUSIJI-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-HOUSIJI-COMMENT   =    "1"
               MOVE       3       TO     FLG-COMMENT
               PERFORM    2001-SHUJUTSU-ETC2-COMMENT-SEC
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
           IF   RECE04-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE04-KAISU(1 IDX3)  NOT =  ZERO
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
             MOVE    RECE04-HOSPID       TO  SANTEI-HOSPID
             MOVE    RECE04-PTID         TO  SANTEI-PTID
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
                    PERFORM        SANTEI-READ-SEC
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
                       MOVE    RECE04-SRYYM    TO  WRK-HINKAIYMD(1:6)
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
                       PERFORM        SANTEI-READ-SEC
                    END-PERFORM
             END-IF
      *
             MOVE    "DBCLOSE"           TO  MCP-FUNC
             MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
             CALL    "ORCMCPSUB"         USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC
      *
             IF      FLG-HINKAI-COMMENT    =      1
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    2001-HINKAI-COMMENT-SEC
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
       2001-HINKAI-COMMENT-SEC          SECTION.
      *
             PERFORM    2001-RECE05-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE05
             MOVE    SPACE                TO  WRK-RECE05XX-REC
             INITIALIZE                       WRK-RECE05XX-REC
      *      ¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE04-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    16     TO  WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "½é²ó»ØÆ³´ÉÍý»»ÄêÆü¡¡¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)(21:4)
             MOVE    WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)(27:4)
      *
             PERFORM    2001-SRYACTINF-HENSYU-SEC
      *
           .
       2001-HINKAI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼ÔË¬ÌäÅÀÅ©Ãí¼Í´ÉÍý»ØÆ³ÎÁ»»Äê»þ¤ÎÅÀÅ©Ãí¼Í¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOUTEN-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-HOUTEN-COMMENT   =    "1"
               PERFORM    2001-HOUTEN-COMMENT-SEC
           ELSE
               PERFORM    2001-RECE04-TENSOU-SEC
           END-IF
      *
           .
       2001-HOUTEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼ÔË¬ÌäÅÀÅ©Ãí¼Í´ÉÍý»ØÆ³ÎÁ»»Äê»þ¤ÎÅÀÅ©Ãí¼Í¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-HOUTEN-COMMENT-SEC          SECTION.
      *
             PERFORM    2001-RECE05-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE05
             MOVE    SPACE                TO  WRK-RECE05XX-REC
             INITIALIZE                       WRK-RECE05XX-REC
      *      ¥³¥á¥ó¥È¤ÎÁÞÆþ
             ADD     1                    TO  IDX-RECE05
      *
             MOVE    "810000001"
                            TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
             MOVE    ZERO   TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
             MOVE    4      TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
             MOVE    "¡ÊË¬ÅÀ¡Ë"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
      *
             PERFORM    2001-SRYACTINF-HENSYU-SEC
      *
           .
       2001-HOUTEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆü¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SHUJUTSU-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-SHUJUTU-COMMENT   =    SPACE
               MOVE       1       TO     FLG-COMMENT
               PERFORM    2001-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
               PERFORM    2001-RECE04-TENSOU-SEC
           END-IF
      *
           .
       2001-SHUJUTSU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ã£Ô¡¦£Í£Ò£É¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-CTMRI-HANTEI-SEC          SECTION.
      *
           IF      RECE04-FLG-CTMRI-COMMENT   =    "1"
                MOVE       2       TO     FLG-COMMENT
                PERFORM    2001-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2001-RECE04-TENSOU-SEC
           END-IF
      *
           .
       2001-CTMRI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Êº¹¹þ¡§¾å¡Ë
      *****************************************************************
       2001-SHUJUTSU-ETC-COMMENT-SEC          SECTION.
      *
           PERFORM   2001-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  2001-FREE-COMMENT2-SEC                              
      *
           PERFORM  2001-RECE05-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE05
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1                  TO  IDX-RECE05
                MOVE   "810000001"
                              TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO   TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           END-PERFORM 
      *
           PERFORM    2001-SRYACTINF-HENSYU-SEC
      *
           .
       2001-SHUJUTSU-ETC-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤Î¥³¥á¥ó¥ÈÊÔ½¸£²¡Êº¹¹þ¡§²¼¡Ë
      *****************************************************************
       2001-SHUJUTSU-ETC2-COMMENT-SEC          SECTION.
      *
           PERFORM   2001-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  2001-FREE-COMMENT2-SEC                              
      *
           PERFORM  2001-RECE05-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE05
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
      *
           MOVE    RECE04-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    2001-SRYACTINF-HENSYU-SEC
      *
           .
       2001-SHUJUTSU-ETC2-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÆü¤Î³ÎÄê
      *****************************************************************
       2001-SANTEI-DAY-SEC          SECTION.
      *
           INITIALIZE                TBL-SANTEI-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF   RECE04-KAISU(1 IDX)  NOT = ZERO
                      ADD   1     TO   DAY-IDX
                      MOVE  IDX   TO   TB-SANTEI-DAY(DAY-IDX)
                END-IF
           END-PERFORM
      *
           .
       2001-SANTEI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤ÎÆüÉÕ¤ÎÊÔ½¸
      *****************************************************************
       2001-FREE-COMMENT2-SEC          SECTION.                         
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
               WHEN    5
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "Ê¡»ã"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
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
                          PERFORM  2001-FREE-COMMENT2-KAIGYO-SEC
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
                          PERFORM  2001-FREE-COMMENT2-KAIGYO-SEC
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
                          PERFORM  2001-FREE-COMMENT2-KAIGYO-SEC
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
                          PERFORM  2001-FREE-COMMENT2-KAIGYO-SEC
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
       2001-FREE-COMMENT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤ÎÆüÉÕ¤ÎÊÔ½¸¡Ê²þ¹Ô¡Ë
      *****************************************************************
       2001-FREE-COMMENT2-KAIGYO-SEC          SECTION.                  
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA2(IDZ)
           MOVE    ALL "¡¡"        TO  WK-FREE-NAME2(IDZ)(1:WRK-KETA)
      *
           .
       2001-FREE-COMMENT2-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-RIHA-COMMENT-SEC          SECTION.
      *
             PERFORM  2001-RECE05-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE05
             MOVE    SPACE                TO  WRK-RECE05XX-REC
             INITIALIZE                       WRK-RECE05XX-REC
      *      ¥ê¥Ï¥Ó¥ê¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE04-MEISAISU  +  1
      *
             MOVE    "810000001"
                           TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO  TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7     TO  WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "¼Â»ÜÆü¿ô¡¡¡¡Æü"
                           TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             EVALUATE   RECE04-ZAINUM
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
                           TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)(9:4)
             END-IF
      *
             PERFORM    2001-SRYACTINF-HENSYU-SEC
      *
           .
       2001-RIHA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ëÊÔ½¸
      *****************************************************************
       2001-YAKUTEI-HENSYU-SEC          SECTION.
      *
           ADD       1        TO         IDX6
      *
           IF    IDX6    <=    100
             MOVE    RECE04-KOHBUNRI     TO   TBL-YT1-KOHBUNRI(IDX6)
             MOVE    RECE04-HKNCOMBI     TO   TBL-YT1-HKNCOMBI(IDX6)
             MOVE    RECE04-SRYKA        TO   TBL-YT1-SRYKA(IDX6)
      *
             MOVE    RECE04-REC          TO   TBL-YT1-RECE04-REC(IDX6)
      *
             MOVE    RECE04-ZAITEN       TO   TBL-YT1-ZAITEN(IDX6)
             MOVE    RECE04-SRYCDTOTAL   TO   TBL-YT1-SRYCDTOTAL(IDX6)
             MOVE    RECE04-SURYOUTOTAL  TO   TBL-YT1-SURYOUTOTAL(IDX6)
             MOVE    RECE04-RUIKEITOTAL  TO   TBL-YT1-RUIKEITOTAL(IDX6)
             MOVE    RECE04-MEISAISU     TO   TBL-YT1-MEISAISU(IDX6)
      *
             PERFORM   VARYING   IDX7    FROM   2   BY   1
                        UNTIL   (IDX7      >    4)
      *
               COMPUTE   IDX9    =   IDX7  -   1
               PERFORM   VARYING   IDX8    FROM   1   BY   1
                          UNTIL   (IDX8      >    31)
      *
                  MOVE    RECE04-KAISU(IDX7 IDX8)
                            TO    TBL-YT1-KAISU(IDX6 IDX8 IDX9)
      *
               END-PERFORM
      *
             END-PERFORM
      *
             IF      RECE04-SRYCDTOTAL  =   630010002
                  MOVE      2      TO     TBL-YT1-FLG(IDX6)
             ELSE
                  MOVE      1      TO     TBL-YT1-FLG(IDX6)
             END-IF
           ELSE
      *
             DISPLAY "TABLE OVER1"
      *
           END-IF
      *
           .
       2001-YAKUTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£´Å¾Á÷½èÍý
      *****************************************************************
       2001-RECE04-TENSOU-SEC     SECTION.
      *
           MOVE       RECE04-REC    TO   RECE041-REC
           WRITE      RECE041-REC
           ADD        1             TO   CNT-RECE04
           PERFORM    2001-RECE05-TENSOU-SEC
      *
           .
       2001-RECE04-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£µÅ¾Á÷½èÍý
      *****************************************************************
       2001-RECE05-TENSOU-SEC     SECTION.
      *
           INITIALIZE                      RECE05-REC
           MOVE    RECE04-HOSPID       TO  RECE05-HOSPID
           MOVE    RECE04-SRYYM        TO  RECE05-SRYYM
           MOVE    RECE04-NYUGAIKBN    TO  RECE05-NYUGAIKBN
           MOVE    RECE04-PTID         TO  RECE05-PTID
           MOVE    RECE04-RECEKA       TO  RECE05-RECEKA
           MOVE    RECE04-TEISYUTUSAKI TO  RECE05-TEISYUTUSAKI
           MOVE    RECE04-RECESYUBETU  TO  RECE05-RECESYUBETU
           MOVE    RECE04-HKNJANUM     TO  RECE05-HKNJANUM
           MOVE    RECE04-HOJOKBN      TO  RECE05-HOJOKBN
           MOVE    RECE04-TEKSTYMD     TO  RECE05-TEKSTYMD
           MOVE    RECE04-ZAINUM       TO  RECE05-ZAINUM
           START   RECE05-FILE
               KEY IS    >=   RECE05-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE05
               NOT INVALID
                   PERFORM 990-RECE05-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE05           =   1                   )
               OR     ( RECE04-HOSPID    NOT =   RECE05-HOSPID       )
               OR     ( RECE04-SRYYM     NOT =   RECE05-SRYYM        )
               OR     ( RECE04-NYUGAIKBN NOT =   RECE05-NYUGAIKBN    )
               OR     ( RECE04-PTID      NOT =   RECE05-PTID         )
               OR     ( RECE04-RECEKA    NOT =   RECE05-RECEKA       )
               OR     ( RECE04-TEISYUTUSAKI
                                         NOT =   RECE05-TEISYUTUSAKI )
               OR     ( RECE04-RECESYUBETU
                                         NOT =   RECE05-RECESYUBETU  )
               OR     ( RECE04-HKNJANUM  NOT =   RECE05-HKNJANUM     )
               OR     ( RECE04-HOJOKBN   NOT =   RECE05-HOJOKBN      )
               OR     ( RECE04-TEKSTYMD  NOT =   RECE05-TEKSTYMD     )
               OR     ( RECE04-ZAINUM    NOT =   RECE05-ZAINUM       )
      *
      *
               MOVE          RECE05-REC       TO      RECE051-REC
               WRITE         RECE051-REC
               ADD     1                 TO  CNT-RECE05
      *
               PERFORM 990-RECE05-NEXT-SEC
      *
           END-PERFORM
      *
           .
       2001-RECE05-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡ÂàÈò½èÍý
      *****************************************************************
       2001-RECE05-TAIHI-SEC     SECTION.
      *
           INITIALIZE                      RECE05-REC
           MOVE    RECE04-HOSPID       TO  RECE05-HOSPID
           MOVE    RECE04-SRYYM        TO  RECE05-SRYYM
           MOVE    RECE04-NYUGAIKBN    TO  RECE05-NYUGAIKBN
           MOVE    RECE04-PTID         TO  RECE05-PTID
           MOVE    RECE04-RECEKA       TO  RECE05-RECEKA
           MOVE    RECE04-TEISYUTUSAKI TO  RECE05-TEISYUTUSAKI
           MOVE    RECE04-RECESYUBETU  TO  RECE05-RECESYUBETU
           MOVE    RECE04-HKNJANUM     TO  RECE05-HKNJANUM
           MOVE    RECE04-HOJOKBN      TO  RECE05-HOJOKBN
           MOVE    RECE04-TEKSTYMD     TO  RECE05-TEKSTYMD
           MOVE    RECE04-ZAINUM       TO  RECE05-ZAINUM
           START   RECE05-FILE
                 KEY IS    >=   RECE05-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE05
                 NOT INVALID
                     PERFORM 990-RECE05-NEXT-SEC
                     MOVE    RECE05-REC  TO  WRK-RECE05-REC
           END-START
      *
           .
       2001-RECE05-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       2001-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE05           =   1                   )
               OR     ( RECE04-HOSPID    NOT =   RECE05-HOSPID       )
               OR     ( RECE04-SRYYM     NOT =   RECE05-SRYYM        )
               OR     ( RECE04-NYUGAIKBN NOT =   RECE05-NYUGAIKBN    )
               OR     ( RECE04-PTID      NOT =   RECE05-PTID         )
               OR     ( RECE04-RECEKA    NOT =   RECE05-RECEKA       )
               OR     ( RECE04-TEISYUTUSAKI
                                         NOT =   RECE05-TEISYUTUSAKI )
               OR     ( RECE04-RECESYUBETU
                                         NOT =   RECE05-RECESYUBETU  )
               OR     ( RECE04-HKNJANUM  NOT =   RECE05-HKNJANUM     )
               OR     ( RECE04-HOJOKBN   NOT =   RECE05-HOJOKBN      )
               OR     ( RECE04-TEKSTYMD  NOT =   RECE05-TEKSTYMD     )
               OR     ( RECE04-ZAINUM    NOT =   RECE05-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE05-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE05
                    MOVE   RECE05-SRYACT-TBL(IDX)
                             TO   WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
               END-PERFORM 
      *
               PERFORM 990-RECE05-NEXT-SEC
      *
           END-PERFORM
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                    INITIALIZE                        RECE051-REC
                    MOVE    WRK-RECE05-KEY       TO   RECE051-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE051-RENNUM
                    MOVE    WRK-RECE05-KOHBUNRI  TO   RECE051-KOHBUNRI
                    MOVE    WRK-RECE05-PTNUM     TO   RECE051-PTNUM
                    MOVE    WRK-RECE05-SRYKA     TO   RECE051-SRYKA
                    MOVE    WRK-RECE05-SRYKBN    TO   RECE051-SRYKBN
                    MOVE    WRK-RECE05-SRYSYUKBN TO   RECE051-SRYSYUKBN
                    MOVE    WRK-RECE05-JIHIMONEY TO   RECE051-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE05   =   1)
                       ADD         1             TO    IDX-RECE05
                       IF  WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE05
                       ELSE
                           MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                            TO   RECE051-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF  
                    END-PERFORM
                    WRITE   RECE051-REC
                    ADD     1                    TO  CNT-RECE05
           END-PERFORM
      *
           MOVE    RECE04-REC        TO      RECE041-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE041-MEISAISU
           WRITE   RECE041-REC
           ADD     1                 TO      CNT-RECE04
      *
           .
       2001-SRYACTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ëÊÔ½¸£²
      *****************************************************************
       240-YAKUTEI-HENSYU2-SEC          SECTION.
      *
      *    ÊÂ¤ÓÂØ¤¨
           PERFORM    VARYING   IDX6    FROM   1   BY   1
                       UNTIL   (IDX6                     >   100 )
                         OR    (TBL-YT1-RECE04-REC(IDX6) =  SPACE)
      *
              COMPUTE    IDX7  =  IDX6  +  1
      *
              PERFORM    VARYING   IDX8   FROM   IDX7   BY   1
                          UNTIL   (IDX8                     >   100 )
                            OR    (TBL-YT1-RECE04-REC(IDX8) =  SPACE)
      *
                IF       TBL-YT1-KEY(IDX6)  >   TBL-YT1-KEY(IDX8)
                   MOVE  TBL-YT1-OCC(IDX6)  TO  WRK-YAKUTEI-DATA
                   MOVE  TBL-YT1-OCC(IDX8)  TO  TBL-YT1-OCC(IDX6)
                   MOVE  WRK-YAKUTEI-DATA   TO  TBL-YT1-OCC(IDX8)
                END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *    ½ÐÎÏ½èÍý
           INITIALIZE                   TBL-YAKUTEI-INF2
           INITIALIZE                   TBL-YAKUTEI-INF3
           MOVE       SPACE     TO      WRK-TAIHI-YT1-KEY
           MOVE       ZERO      TO      WRK-TAIHI-YT1-KEY2
      *
           MOVE       ZERO      TO      IDX11
      *
           PERFORM    VARYING   IDX6    FROM   1   BY   1
                       UNTIL   (IDX6                     >   100 )
                         OR    (TBL-YT1-RECE04-REC(IDX6) =  SPACE)
      *
              IF     TBL-YT1-KEY(IDX6)   NOT =  WRK-TAIHI-YT1-KEY
      *
                 IF   WRK-TAIHI-YT1-KEY  NOT =  SPACE
                   PERFORM   2401-YAKUTEI-MATOME-SEC
                   IF  TBL-YT1-KOHBUNRI(IDX6)  NOT =  WRK-TAIHI-YT1-KEY2
                       PERFORM 2402-YAKUTEI-WRITE-SEC
                       INITIALIZE                  TBL-YAKUTEI-INF3
                   END-IF
                 END-IF
      *
                 INITIALIZE                        TBL-YAKUTEI-INF2
                 MOVE   TBL-YT1-KEY(IDX6)      TO  WRK-TAIHI-YT1-KEY
                 MOVE   TBL-YT1-KOHBUNRI(IDX6) TO  WRK-TAIHI-YT1-KEY2
      *
              END-IF
      *
              IF     TBL-YT1-FLG(IDX6)   =    1
      *
                   PERFORM   VARYING   IDX7    FROM   1   BY   1
                              UNTIL   (IDX7      >    31)
      *
                     PERFORM   VARYING   IDX8    FROM   1   BY   1
                                UNTIL   (IDX8      >    3)
      *
                        IF   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                                        NOT =  ZERO
                           PERFORM   VARYING   IDX9    FROM   1   BY   1
                            UNTIL (IDX9      >    25)
                              OR  (TBL-YT2X1-RECE04-REC(IDX7 IDX8 IDX9)
                                                              =  SPACE)
                                 CONTINUE
                           END-PERFORM
                           IF    IDX9   >   25
                             DISPLAY "TABLE OVER2"
                           ELSE
                             MOVE   TBL-YT1-RECE04-REC(IDX6)
                                TO  TBL-YT2X1-RECE04-REC(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X1-KAISU(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-HIKAKU(IDX6)
                                TO  TBL-YT2Y1-HIKAKU(IDX7 IDX8 IDX9)
      *
                             ADD    TBL-YT1-ZAITEN(IDX6)
                                TO  TBL-YT2Y1-ZAITEN(IDX7 IDX8 26)
                             ADD    TBL-YT1-SRYCDTOTAL(IDX6)
                                TO  TBL-YT2Y1-SRYCDTOTAL(IDX7 IDX8 26)
                             ADD    TBL-YT1-SURYOUTOTAL(IDX6)
                                TO  TBL-YT2Y1-SURYOUTOTAL(IDX7 IDX8 26)
                             ADD    TBL-YT1-RUIKEITOTAL(IDX6)
                                TO  TBL-YT2Y1-RUIKEITOTAL(IDX7 IDX8 26)
                             ADD    TBL-YT1-MEISAISU(IDX6)
                                TO  TBL-YT2Y1-MEISAISU(IDX7 IDX8 26)
      *
                           END-IF
                        END-IF
      *
                     END-PERFORM
      *
                   END-PERFORM
      *
              ELSE
      *
                   PERFORM   VARYING   IDX7    FROM   1   BY   1
                              UNTIL   (IDX7      >    31)
      *
                     PERFORM   VARYING   IDX8    FROM   1   BY   1
                                UNTIL   (IDX8      >    3)
      *
                        IF   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                                        NOT =  ZERO
                           PERFORM   VARYING   IDX9    FROM   1   BY   1
                            UNTIL (IDX9      >    3)
                              OR  (TBL-YT2X2-RECE04-REC(IDX7 IDX8 IDX9)
                                                              =  SPACE)
                                 CONTINUE
                           END-PERFORM
                           IF    IDX9   >   3
                             DISPLAY "TABLE OVER3"
                           ELSE
                             MOVE   TBL-YT1-RECE04-REC(IDX6)
                                TO  TBL-YT2X2-RECE04-REC(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X2-KAISU(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-HIKAKU(IDX6)
                                TO  TBL-YT2Y2-HIKAKU(IDX7 IDX8 IDX9)
      *
                             ADD    TBL-YT1-ZAITEN(IDX6)
                                TO  TBL-YT2Y2-ZAITEN(IDX7 IDX8 4)
                             ADD    TBL-YT1-SRYCDTOTAL(IDX6)
                                TO  TBL-YT2Y2-SRYCDTOTAL(IDX7 IDX8 4)
                             ADD    TBL-YT1-SURYOUTOTAL(IDX6)
                                TO  TBL-YT2Y2-SURYOUTOTAL(IDX7 IDX8 4)
                             ADD    TBL-YT1-RUIKEITOTAL(IDX6)
                                TO  TBL-YT2Y2-RUIKEITOTAL(IDX7 IDX8 4)
                             ADD    TBL-YT1-MEISAISU(IDX6)
                                TO  TBL-YT2Y2-MEISAISU(IDX7 IDX8 4)
      *
                           END-IF
                        END-IF
      *
                     END-PERFORM
      *
                   END-PERFORM
      *
              END-IF
      *
           END-PERFORM
      *
           IF   WRK-TAIHI-YT1-KEY  NOT =  SPACE
              PERFORM   2401-YAKUTEI-MATOME-SEC
              PERFORM   2402-YAKUTEI-WRITE-SEC
           END-IF
      *
           .
       240-YAKUTEI-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ë¥Õ¥¡¥¤¥ë¤Þ¤È¤á½èÍý
      *****************************************************************
       2401-YAKUTEI-MATOME-SEC          SECTION.
      *
           PERFORM   VARYING   IDX7    FROM   1   BY   1
                      UNTIL   (IDX7      >    31)
      *
              PERFORM   VARYING   IDX8    FROM   1   BY   1
                         UNTIL   (IDX8      >    3)
      *
                 MOVE     ZERO     TO    FLG-YAKUTEI
                 PERFORM    VARYING   IDX9    FROM   1   BY   1
                       UNTIL   (IDX9                          >   100 )
                         OR    (TBL-YT3X1-RECE04-REC(IDX9 1)  =  SPACE)
                         OR    (FLG-YAKUTEI                   =   1   )
      *
                    IF   (TBL-YT2Y1-HIKAKU(IDX7 IDX8 26) =
                                              TBL-YT3Y1-HIKAKU(IDX9 26))
                    AND  (TBL-YT2Y2-HIKAKU(IDX7 IDX8  4) =
                                              TBL-YT3Y2-HIKAKU(IDX9  4))
      *
                       PERFORM    VARYING   IDX10  FROM   1   BY   1
                         UNTIL (IDX10                         >    25 )
                           OR  (TBL-YT2X1-RECE04-REC(IDX7 IDX8 IDX10)
                                                              =  SPACE)
                         ADD    TBL-YT2X1-KAISU(IDX7 IDX8 IDX10)
                                    TO  TBL-YT3X1-KAISU(IDX9 IDX10)
                       END-PERFORM
                       PERFORM    VARYING   IDX10  FROM   1   BY   1
                         UNTIL (IDX10                         >     3 )
                           OR  (TBL-YT2X2-RECE04-REC(IDX7 IDX8 IDX10)
                                                              =  SPACE)
                         ADD    TBL-YT2X2-KAISU(IDX7 IDX8 IDX10)
                                    TO  TBL-YT3X2-KAISU(IDX9 IDX10)
                       END-PERFORM
      *
                       MOVE    1    TO    FLG-YAKUTEI
                    END-IF
      *
                 END-PERFORM
                 IF  FLG-YAKUTEI  =  ZERO
                    IF   IDX9    >    100
                       DISPLAY   "TABLE OVER4"
                    ELSE
                       MOVE   TBL-YT2X-KAIT(IDX7 IDX8)
                                      TO  TBL-YT3X-KAIT(IDX9)
                       MOVE   TBL-YT2Y-KAIT(IDX7 IDX8)
                                      TO  TBL-YT3Y-KAIT(IDX9)
                    END-IF
                 END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
           .
       2401-YAKUTEI-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ë¥Õ¥¡¥¤¥ë½ÐÎÏ
      *****************************************************************
       2402-YAKUTEI-WRITE-SEC          SECTION.
      *
           PERFORM   VARYING   IDX7    FROM   1   BY   1
                      UNTIL   (IDX7                          >   100 )
                        OR    (TBL-YT3X1-RECE04-REC(IDX7 1)  =  SPACE)
      *
                 PERFORM    VARYING   IDX8    FROM   1   BY   1
                             UNTIL   (IDX8                     >   25  )
                               OR    (TBL-YT3X1-RECE04-REC(IDX7 IDX8)
                                                               =  SPACE)
      *
                      INITIALIZE       RECE041-REC
                      MOVE   TBL-YT3X1-RECE04-REC(IDX7 IDX8)
                                  TO   RECE041-REC
                      MOVE   RECE041-ZAINUM  TO  WRK-YAKUTEI-ZAINUM
                      INITIALIZE       RECE041-DAY-AREA
                      MOVE   TBL-YT3X1-KAISU(IDX7 IDX8)
                                  TO   RECE041-ZAIKAISU
                      ADD       1             TO  IDX11
                      COMPUTE   WRK-ZAINUM    =  99990000  +  IDX11
                      MOVE      WRK-ZAINUM    TO  RECE041-ZAINUM
                      MOVE      RECE041-ZAINUM  TO  WRK-YAKUTEI-ZAINUM2
      *
                      WRITE     RECE041-REC
                      ADD       1            TO  CNT-RECE04
                      PERFORM   24021-YAKUTEI-RECE05-TENSOU-SEC
      *
                 END-PERFORM
      *
                 PERFORM    VARYING   IDX8    FROM   1   BY   1
                              UNTIL  (IDX8                     >    3  )
                                OR   (TBL-YT3X2-RECE04-REC(IDX7 IDX8)
                                                               =  SPACE)
      *
                      INITIALIZE       RECE041-REC
                      MOVE   TBL-YT3X2-RECE04-REC(IDX7 IDX8)
                                  TO   RECE041-REC
                      MOVE   RECE041-ZAINUM  TO  WRK-YAKUTEI-ZAINUM
                      INITIALIZE       RECE041-DAY-AREA
                      MOVE   TBL-YT3X2-KAISU(IDX7 IDX8)
                                  TO   RECE041-ZAIKAISU
                      ADD       1             TO  IDX11
                      COMPUTE   WRK-ZAINUM    =  99990000  +  IDX11
                      MOVE      WRK-ZAINUM    TO  RECE041-ZAINUM
                      MOVE      RECE041-ZAINUM  TO  WRK-YAKUTEI-ZAINUM2
      *
                      WRITE     RECE041-REC
                      ADD       1            TO  CNT-RECE04
                      PERFORM   24021-YAKUTEI-RECE05-TENSOU-SEC
      *
                 END-PERFORM
      *
           END-PERFORM
      *
           .
       2402-YAKUTEI-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ë¥Õ¥¡¥¤¥ë½ÐÎÏ¡Ê£°£µ¥Õ¥¡¥¤¥ë¡Ë
      *****************************************************************
       24021-YAKUTEI-RECE05-TENSOU-SEC          SECTION.
      *
           INITIALIZE                       RECE05-REC
           MOVE    RECE041-HOSPID       TO  RECE05-HOSPID
           MOVE    RECE041-SRYYM        TO  RECE05-SRYYM
           MOVE    RECE041-NYUGAIKBN    TO  RECE05-NYUGAIKBN
           MOVE    RECE041-PTID         TO  RECE05-PTID
           MOVE    RECE041-RECEKA       TO  RECE05-RECEKA
           MOVE    RECE041-TEISYUTUSAKI TO  RECE05-TEISYUTUSAKI
           MOVE    RECE041-RECESYUBETU  TO  RECE05-RECESYUBETU
           MOVE    RECE041-HKNJANUM     TO  RECE05-HKNJANUM
           MOVE    RECE041-HOJOKBN      TO  RECE05-HOJOKBN
           MOVE    RECE041-TEKSTYMD     TO  RECE05-TEKSTYMD
           MOVE    WRK-YAKUTEI-ZAINUM   TO  RECE05-ZAINUM
           START   RECE05-FILE
               KEY IS    >=   RECE05-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE05
               NOT INVALID
                   PERFORM 990-RECE05-NEXT-SEC
           END-START
           PERFORM
               UNTIL ( FLG-RECE05              =   1                   )
               OR    ( RECE041-HOSPID      NOT =   RECE05-HOSPID       )
               OR    ( RECE041-SRYYM       NOT =   RECE05-SRYYM        )
               OR    ( RECE041-NYUGAIKBN   NOT =   RECE05-NYUGAIKBN    )
               OR    ( RECE041-PTID        NOT =   RECE05-PTID         )
               OR    ( RECE041-RECEKA      NOT =   RECE05-RECEKA       )
               OR    ( RECE041-TEISYUTUSAKI
                                           NOT =   RECE05-TEISYUTUSAKI )
               OR    ( RECE041-RECESYUBETU
                                           NOT =   RECE05-RECESYUBETU  )
               OR    ( RECE041-HKNJANUM    NOT =   RECE05-HKNJANUM     )
               OR    ( RECE041-HOJOKBN     NOT =   RECE05-HOJOKBN      )
               OR    ( RECE041-TEKSTYMD    NOT =   RECE05-TEKSTYMD     )
               OR    ( WRK-YAKUTEI-ZAINUM  NOT =   RECE05-ZAINUM       )
      *
               MOVE     RECE05-REC            TO      RECE051-REC
               MOVE     WRK-YAKUTEI-ZAINUM2   TO      RECE051-ZAINUM
               WRITE    RECE051-REC
               ADD      1                     TO      CNT-RECE05
      *
               PERFORM 990-RECE05-NEXT-SEC
      *
           END-PERFORM
      *
           .
       24021-YAKUTEI-RECE05-TENSOU-EXT.
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
           DISPLAY "RECE02  CNT-IN  :" CNT-RECE02
           DISPLAY "RECE04  CNT-OUT :" CNT-RECE04 
           DISPLAY "RECE05  CNT-OUT :" CNT-RECE05 
           DISPLAY "*** ORCR0040 END ***"
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
           CLOSE                       RECE02-FILE
      *
           CLOSE                       RECE04-FILE
      *
           CLOSE                       RECE05-FILE
      *
           CLOSE                       RECE041-FILE     
           CLOSE                       RECE051-FILE     
      *
           .
       310-FILE-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿½ç¸¡º÷½èÍý
      *****************************************************************
       990-RECE02-READ-SEC           SECTION.
      *
           READ    RECE02-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
                   MOVE    2           TO  SPA-STS
               NOT AT  END
                   ADD     1           TO  CNT-RECE02
                   ADD     1           TO  CNT-IN-COMMIT
                   MOVE    ZERO        TO  FLG-END
      *
                   DISPLAY "RECE02  PTNUM=" RECE02-PTNUM
           END-READ
      *
           .
       990-RECE02-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE04-NEXT-SEC           SECTION.
      *
           READ    RECE04-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE04
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE04
           END-READ
      *
           .
       990-RECE04-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE05-NEXT-SEC           SECTION.
      *
           READ    RECE05-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE05
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE05
           END-READ
      *
           .
       990-RECE05-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÎò¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       SANTEI-READ-SEC         SECTION.
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
       SANTEI-READ-EXT.
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
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼õ¿ÇÍúÎòÆÉ¹þ
      *****************************************************************
       JYURRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥«¡¼¥½¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       CSR-CLOSE-SEC            SECTION.
      *
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSE"           TO  MCP-FUNC
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

