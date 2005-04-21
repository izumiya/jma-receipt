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
       PROGRAM-ID.             ORCR0430.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ·î¼¡¶ÈÌ³¡¡Ï«ºÒ¼«ÇåÀÕÌÀºÙ½ñ¥Õ¥¡¥¤¥ëºîÀ®
      *                      ¥³¥á¥ó¥ÈÊÔ½¸¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/07/01    MCC-ÌçÏÆ¡¡    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    02.09.18  ¼«ÇåÀÕÂÐ±þ¡¡¡¡¡¡¡¡¡¡¡¡
      *  01.00.02    NACL-ÌçÏÆ    02.11.29  Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î
      *                                     ¥³¥á¥ó¥Èº¹¹þ¤ß
      *  01.00.03    NACL-ÌçÏÆ    03.02.04  ºÞ¤Þ¤È¤áÂÐ±þÅù
      *  01.00.04    NACL-ÌçÏÆ    03.02.06  ¥³¥á¥ó¥È¥«¥¦¥ó¥È¥Õ¥é¥°¤Ç¤Î
      *                                     È½ÄêÄÉ²Ã
      *  01.00.05    NACL-ÌçÏÆ    03.03.03  ºÞÈÖ¹æ·å½¤ÀµÂÐ±þ
      *  01.00.06    NACL-ÌçÏÆ    03.03.12  £Ã£Ô¡¦£Í£Ò£É¤Î¼Â»ÜÆüÄÉ²Ã
      *  01.00.07    NACL-ÌçÏÆ    03.04.04  ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ¤Î¸òÉÕÆüÊÔ½¸
      *                                     ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ¤Î¸òÉÕÆüÊÔ½¸
      *  01.00.08    NACL-ÌçÏÆ    03.04.24  Í¢·ì¤Ï¥³¥á¥ó¥ÈÊÔ½¸ÂÐ¾Ý³°¤È¤¹¤ë
      *  01.00.09    NACL-ÌçÏÆ    03.05.28  ¼õ¿ÇÍúÎò¥­¡¼¤ÎÊÑ¹¹
      *  01.00.10    NACL-ÌçÏÆ    03.06.19  Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤ÎÊÔ½¸½¤Àµ
      *  01.00.11    NACL-ÌçÏÆ    03.09.24  ¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  01.00.12    NACL-ÌçÏÆ    03.09.29  ¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  £²£°£°£´Ç¯£´·î²þÀµÂÐ±þ
      *  01.01.01    NACL-ÌçÏÆ    04.04.01  ¥ê¥Ï¸À¸ìÄ°³ÐÎÅË¡¡Ê£³¡ËÄÉ²ÃÂÐ±þ
      *  01.01.02    NACL-ÌçÏÆ    04.04.05  ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤Î
      *                                     ¥³¥á¥ó¥ÈÂÐ±þ
      *  01.01.03    NACL-ÌçÏÆ    04.04.10  ºßÂð´µ¼ÔË¬ÌäÅÀÅ©Ãí¼Í´ÉÍý»ØÆ³ÎÁ
      *                                     »»Äê»þ¤ÎÅÀÅ©Ãí¼Í¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  01.01.04    NACL-ÌçÏÆ    04.04.15  Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥È
      *                                     ÂÐ±þ
      *  01.01.05    NACL-ÌçÏÆ    04.06.07  ¥³¥Ô¡¼¶ç£°£´£µÊÑ¹¹¤ËÈ¼¤¦½¤Àµ
      *  01.01.06    NACL-ÌçÏÆ    04.07.30  Æ±ÆüÊ£¿ôºÆ¿Ç¤Î¥³¥á¥ó¥È½¤Àµ
      *  Ãæ»ß½èÍýÄÉ²Ã
      *  01.02.01    NACL-Æ£¸¶    04/10/13  Ãæ»ß½èÍý¤ÎÄÉ²Ã
      *  01.02.02    NACL-ÌçÏÆ    04/12/09  Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»ÆüÂÐ±þ
      *  01.02.03    NACL-ÌçÏÆ    05/01/18  »ÀÁÇ·×»»¼°¥³¥á¥ó¥ÈÂÐ±þ
      *  01.02.04    NACL-ÌçÏÆ    05/04/20  ÆâÉþÌôºÞÄþ¸ºÊÔ½¸ÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
      *
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
      *
           SELECT  RECE441-FILE    ASSIGN  RECE441PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE441-KEY
                                   FILE    STATUS  IS  STS-RECE441.
           SELECT  RECE4441-FILE   ASSIGN  RECE441PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4441-KEY
                                   FILE    STATUS  IS  STS-RECE441.
      *
           SELECT  RECE451-FILE    ASSIGN  RECE451PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE451-KEY
                                   FILE    STATUS  IS  STS-RECE451.
           SELECT  RECE4551-FILE   ASSIGN  RECE451PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4551-KEY
                                   FILE    STATUS  IS  STS-RECE451.
      *¥ï¡¼¥¯
           SELECT  RECE449-FILE    ASSIGN  RECE449PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE449-KEY
                                   FILE    STATUS  IS  STS-RECE449.
           SELECT  RECE4449-FILE   ASSIGN  RECE449PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4449-KEY
                                   FILE    STATUS  IS  STS-RECE449.
      *
           SELECT  RECE459-FILE    ASSIGN  RECE459PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE459-KEY
                                   FILE    STATUS  IS  STS-RECE459.
           SELECT  RECE4559-FILE   ASSIGN  RECE459PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4559-KEY
                                   FILE    STATUS  IS  STS-RECE459.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
       FD  RECE44-FILE.
       01  RECE44-REC.
           COPY    "CPRCF044.INC".
      *
       FD  RECE45-FILE.
       01  RECE45-REC.
           COPY    "CPRCF045.INC".
      *
       FD  RECE441-FILE.
       01  RECE441-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //RECE441//.
      *
       FD  RECE451-FILE.
       01  RECE451-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE451//.
      *
       FD  RECE4441-FILE.
       01  RECE4441-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //RECE4441//.
      *
       FD  RECE4551-FILE.
       01  RECE4551-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE4551//.
      *¥ï¡¼¥¯
       FD  RECE449-FILE.
       01  RECE449-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //RECE449//.
      *
       FD  RECE459-FILE.
       01  RECE459-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE459//.
      *
       FD  RECE4449-FILE.
       01  RECE4449-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //RECE4449//.
      *
       FD  RECE4559-FILE.
       01  RECE4559-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE4559//.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     ¥ì¥»¥×¥ÈÌÀºÙ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE41//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE44//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE45//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE441//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE451//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE449//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE459//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE41          PIC X(02).
           03  STS-RECE44          PIC X(02).
           03  STS-RECE441         PIC X(02).
           03  STS-RECE45          PIC X(02).
           03  STS-RECE451         PIC X(02).
           03  STS-RECE449         PIC X(02).
           03  STS-RECE459         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE44          PIC 9(01).
           03  FLG-RECE449         PIC 9(01).
           03  FLG-RECE45          PIC 9(01).
           03  FLG-RECE459         PIC 9(01).
           03  FLG-WRKRECE45       PIC 9(01).
           03  FLG-SHIDOU-COMMENT  PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-YAKUTEI         PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE44          PIC 9(06).
           03  CNT-RECE45          PIC 9(06).
           03  CNT-RECE449         PIC 9(06).
           03  CNT-RECE459         PIC 9(06).
           03  CNT-RIHA            PIC 9(06).
           03  CNT-JUSHIN          PIC 9(06).
           03  CNT-SANTEIKA        PIC 9(06).
           03  CNT-DOUJITU         PIC 9(06).
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
           03  IDX7                PIC 9(04).
           03  IDX8                PIC 9(04).
           03  IDX9                PIC 9(04).
           03  IDX10               PIC 9(04).
           03  IDX11               PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  IDX-RECE45          PIC 9(04).
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
               05  TB-SANTEI-DAY   PIC 9(02)   OCCURS  31.
      *
           03  WRK-HKNCOMBINUMX      PIC X(04).
           03  WRK-HKNCOMBINUM9      REDEFINES  WRK-HKNCOMBINUMX
                                     PIC 9(04).
      *
           03  WRK-SRYYMDX.
               05  WRK-SRYYM         PIC 9(06).
               05  WRK-SRYDD         PIC 9(02).
      *
           03  WRK-KEISAN            PIC 9(09).
           03  WRK-KEISAN2           PIC 9(03).
           03  WRK-KEISAN3           PIC 9(09)V9(03).
           03  WRK-KEISAN-TAIHI      PIC X(09).
      *
       01  WRK-RECE44-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //WRK-RECE44//.
      *
       01  WRK-RECE45-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //WRK-RECE45//.
      *
       01  WRK-RECE45XX-REC.
         02 WRK-RECE45XX-TBL       OCCURS 50.
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //WRK-RECE45XX//.
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
      *Íý³ØÎÅË¡ºÞÈÖ¹æÂàÈòÎÎ°è
       01  WRK-TAIHI-ZAINUM.
           03  WRK-RIGA1-SHU-ZAINUM          PIC  9(08).   
           03  WRK-RIGA1-KO-ZAINUM           PIC  9(08).   
           03  WRK-RIGA2-SHU-ZAINUM          PIC  9(08).   
           03  WRK-RIGA2-KO-ZAINUM           PIC  9(08).   
           03  WRK-RIGA3-SHU-ZAINUM          PIC  9(08).   
           03  WRK-RIGA3-KO-ZAINUM           PIC  9(08).   
           03  WRK-RIGA4-SHU-ZAINUM          PIC  9(08).   
           03  WRK-RIGA4-KO-ZAINUM           PIC  9(08).   
           03  WRK-SAGYO1-SHU-ZAINUM         PIC  9(08).   
           03  WRK-SAGYO2-SHU-ZAINUM         PIC  9(08).   
           03  WRK-SAGYO1-KO-ZAINUM          PIC  9(08).   
           03  WRK-SAGYO2-KO-ZAINUM          PIC  9(08).   
           03  WRK-GENGO1-SHU-ZAINUM         PIC  9(08).   
           03  WRK-GENGO1-KO-ZAINUM          PIC  9(08).   
           03  WRK-GENGO2-SHU-ZAINUM         PIC  9(08).   
           03  WRK-GENGO2-KO-ZAINUM          PIC  9(08).   
           03  WRK-GENGO3-SHU-ZAINUM         PIC  9(08).   
           03  WRK-GENGO3-KO-ZAINUM          PIC  9(08).   
      *
       01  WRK-TAIHI-ZAINUM-R     REDEFINES   WRK-TAIHI-ZAINUM.
            03  WRK-RIHA-ZAINUM   PIC 9(08)   OCCURS   18.              
      *
      *Íý³ØÎÅË¡¼Â»Ü¥«¥ì¥ó¥À¡¼
       01  WRK-RIGAKU-AREA.
           03  WRK-RIGA1-SHU                 PIC  X(01)  OCCURS 31.   
           03  WRK-RIGA1-KO                  PIC  X(01)  OCCURS 31.   
           03  WRK-RIGA2-SHU                 PIC  X(01)  OCCURS 31.   
           03  WRK-RIGA2-KO                  PIC  X(01)  OCCURS 31.   
           03  WRK-RIGA3-SHU                 PIC  X(01)  OCCURS 31.   
           03  WRK-RIGA3-KO                  PIC  X(01)  OCCURS 31.   
           03  WRK-RIGA4-SHU                 PIC  X(01)  OCCURS 31.   
           03  WRK-RIGA4-KO                  PIC  X(01)  OCCURS 31.   
           03  WRK-SAGYO1-SHU                PIC  X(01)  OCCURS 31.   
           03  WRK-SAGYO2-SHU                PIC  X(01)  OCCURS 31.   
           03  WRK-SAGYO1-KO                 PIC  X(01)  OCCURS 31.   
           03  WRK-SAGYO2-KO                 PIC  X(01)  OCCURS 31.   
           03  WRK-GENGO1-SHU                PIC  X(01)  OCCURS 31.   
           03  WRK-GENGO1-KO                 PIC  X(01)  OCCURS 31.   
           03  WRK-GENGO2-SHU                PIC  X(01)  OCCURS 31.   
           03  WRK-GENGO2-KO                 PIC  X(01)  OCCURS 31.   
           03  WRK-GENGO3-SHU                PIC  X(01)  OCCURS 31.   
           03  WRK-GENGO3-KO                 PIC  X(01)  OCCURS 31.   
      *
       01  WRK-RIGAKU-R        REDEFINES     WRK-RIGAKU-AREA.
           02  WRK-RIGAKU-OCC                OCCURS   18.               
             03  WRK-RIGAKU-OCC2             OCCURS   31.
               04  WRK-RIGAKU                PIC  X(01).
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
      *    »ÀÁÇ¡Ê¶â³Û¡¦¿ôÎÌ¡Ë¾ðÊó
       01  TBL-SANSO-INF.
           03  TBL-SANSO-INF2       OCCURS    10.
               05  TBL-SANSO-TEN          PIC 9(09)V9(02).
               05  TBL-SANSO-SRYSURYO     PIC 9(05)V9(03).
           03  TBL-SANSO-INF3.
               05  TBL-KASAN-SRYSURYO     PIC 9(05)V9(03).
      *
      *    ÌôºÞÄþ¸º¾ðÊó
       01  TBL-YAKUTEI-INF.
           03  TBL-YT1-OCC                      OCCURS 100.
             05  TBL-YT1-KEY.
               07  TBL-YT1-SRYKA                PIC X(02).
             05  TBL-YT1-RECE44-REC             PIC X(250).
             05  TBL-YT1-HIKAKU.
               07  TBL-YT1-ZAITEN               PIC 9(08).
               07  TBL-YT1-SRYCDTOTAL           PIC 9(14).
               07  TBL-YT1-SURYOUTOTAL          PIC 9(07)V9(03).
               07  TBL-YT1-RUIKEITOTAL          PIC 9(15)V9(03).
               07  TBL-YT1-MEISAISU             PIC 9(04).
             05  TBL-YT1-RECE44-DAY-AREA.
               07  TBL-YT1-OCC2                 OCCURS  31.
                 09  TBL-YT1-OCC3               OCCURS   3.
                   11  TBL-YT1-KAISU            PIC 9(03).
             05  TBL-YT1-FLG                    PIC 9(01).
      *
      *    ÊÂ¤ÓÂØ¤¨ÍÑ
       01  WRK-YAKUTEI-DATA                     PIC X(586).
      *
       01  WRK-TAIHI-YT1-KEY                    PIC X(02).
       01  WRK-YAKUTEI-ZAINUM                   PIC 9(08).
       01  WRK-YAKUTEI-ZAINUM2                  PIC 9(08).
      *
      *    ÌôºÞÄþ¸º¾ðÊó¡Ê¥­¡¼Ã±°Ì¤ÎÊÔ½¸¥Æ¡¼¥Ö¥ë¡Ë
       01  TBL-YAKUTEI-INF2.
           03  TBL-YT2-DAY                    OCCURS      31.
             05  TBL-YT2X-KAIT                  OCCURS     3.
               06  TBL-YT2X1-OCC                  OCCURS  25.
                 07  TBL-YT2X1-RECE44-REC         PIC X(250).
                 07  TBL-YT2X1-KAISU              PIC 9(03).
               06  TBL-YT2X2-OCC                  OCCURS   3.
                 07  TBL-YT2X2-RECE44-REC         PIC X(250).
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
                 07  TBL-YT3X1-RECE44-REC         PIC X(250).
                 07  TBL-YT3X1-KAISU              PIC 9(03).
               06  TBL-YT3X2-OCC                  OCCURS   3.
                 07  TBL-YT3X2-RECE44-REC         PIC X(250).
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
           MOVE    "ORCR0430"      TO  JOB-PGID
           MOVE    "¥³¥á¥ó¥ÈÊÔ½¸¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë" 
                                   TO  JOB-SHELLMSG   
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *    ½èÍýÃæ»ßÀßÄê½èÍý
           PERFORM 500-CANCEL-HENSYU-SEC
      *    
           PERFORM 990-RECE41-READ-SEC
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
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
      *
           MOVE    "RECE44W"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
      *
           MOVE    "RECE450"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
      *
           MOVE    "RECE44A"           TO  RECE441PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE441PARA-TERMID
      *
           MOVE    "RECE45A"           TO  RECE451PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE451PARA-TERMID
      *
           MOVE    "RECE44Y"           TO  RECE449PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE449PARA-TERMID
      *
           MOVE    "RECE45Y"           TO  RECE459PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE459PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    INPUT               RECE41-FILE
      *
           OPEN    INPUT               RECE44-FILE
      *
           OPEN    INPUT               RECE45-FILE
      *
           OPEN    OUTPUT              RECE4441-FILE
           CLOSE                       RECE4441-FILE
      *
           OPEN    OUTPUT              RECE4551-FILE
           CLOSE                       RECE4551-FILE
      *
           OPEN    OUTPUT              RECE4449-FILE
           CLOSE                       RECE4449-FILE
      *
           OPEN    OUTPUT              RECE4559-FILE
           CLOSE                       RECE4559-FILE
      *
           OPEN    OUTPUT              RECE441-FILE
      *
           OPEN    OUTPUT              RECE451-FILE
      *
           OPEN    I-O                 RECE449-FILE
      *
           OPEN    I-O                 RECE459-FILE
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
                                           CNT-DOUJITU
           INITIALIZE                      WRK-RIGAKU-AREA
           INITIALIZE                      WRK-TAIHI-ZAINUM
      *
           INITIALIZE                      TBL-YAKUTEI-INF
      *
           MOVE       ZERO             TO  IDX6
      *
           IF   RECE41-COMCNTFLG   =   "1"
      *
             INITIALIZE                      RECE44-REC
             MOVE    RECE41-HOSPID       TO  RECE44-HOSPID
             MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
             MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
             MOVE    RECE41-PTID         TO  RECE44-PTID
             MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
             MOVE    RECE41-HKNID        TO  RECE44-HKNID
             MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
             MOVE    ZERO                TO  RECE44-ZAINUM
      *
             START   RECE44-FILE
                 KEY IS    >=   RECE44-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE44
                 NOT INVALID
                     PERFORM 990-RECE44-NEXT-SEC
             END-START
             PERFORM
               UNTIL  ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPID    NOT =   RECE44-HOSPID       )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
               IF   RECE41-HKNKBN    NOT  =  "4"                        
                  EVALUATE      RECE44-SRYKBN
                      WHEN      "11"
      *               ¡Ê½é¿ÇÎÁ»»Äê²Ê¡§¤Î¥«¥¦¥ó¥È¡Ë
                         IF  RECE44-SYOSANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
                      WHEN      "12"
      *               ¡ÊºÆ¿ÇÎÁ»»Äê²Ê¡§¤Î¥«¥¦¥ó¥È¡Ë
                         IF  RECE44-SAISANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
      *               ¡ÊÆ±ÆüºÆ¿Ç¤Î¥«¥¦¥ó¥È¡Ë
                         IF  RECE44-DOUJITUSAI-COMMENT   =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DOUJITU
                         END-IF
                      WHEN      "80"
      *               ¡Ê¥ê¥Ï¥Ó¥ê¤Î¥«¥¦¥ó¥È¡Ë
                         IF  RECE44-RIHABIRI-COMMENT     =  "1"
                            PERFORM    2001-RIHA-COUNT-SEC              
                         END-IF
                  END-EVALUATE
               ELSE                                                     
                  EVALUATE      RECE44-SRYKBN  
                      WHEN      "11"
      *               ¡Ê½é¿ÇÎÁ»»Äê²Ê¡§¤Î¥«¥¦¥ó¥È¡Ë
                         IF  RECE44-SYOSANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
                      WHEN      "12"
      *               ¡ÊºÆ¿ÇÎÁ»»Äê²Ê¡§¤Î¥«¥¦¥ó¥È¡Ë
                         IF  RECE44-SAISANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
      *               ¡ÊÆ±ÆüºÆ¿Ç¤Î¥«¥¦¥ó¥È¡Ë
                         IF  RECE44-DOUJITUSAI-COMMENT   =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DOUJITU
                         END-IF
                      WHEN      "40"                                    
      *               ¡Ê¥ê¥Ï¥Ó¥ê¤Î¥«¥¦¥ó¥È¡Ë                            
                         IF  RECE44-RIHABIRI-COMMENT     =  "1"
                            PERFORM    2001-RIHA-COUNT-SEC              
                         END-IF
                  END-EVALUATE                                          
               END-IF                                                   
      *
               PERFORM 990-RECE44-NEXT-SEC
      *
             END-PERFORM
      *
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
             MOVE    RECE41-SRYYM        TO  ORC-DBYMD(1:6)
             MOVE    "01"                TO  ORC-DBYMD(7:2)
             CALL    "ORCMCPSUB"         USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
             IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH 
               PERFORM   900-SYSKANRI-READ-SEC
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
                   PERFORM   900-SYSKANRI-READ-SEC
               END-PERFORM
             END-IF
      *
             INITIALIZE                        MCPAREA
             MOVE    "DBCLOSECURSOR"   TO      MCP-FUNC
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
      *    Æ±ÆüÊ£¿ôºÆ¿Ç
      *
           IF     CNT-DOUJITU     >   ZERO
                PERFORM    2001-DOUJITU-COMMENT-SEC
           END-IF
      *
      *    ¥Ñ¥é¥á¡¼¥¿£°£´¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-HOSPID       TO  RECE44-HOSPID
           MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE44-PTID
           MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
           MOVE    RECE41-HKNID        TO  RECE44-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
           MOVE    ZERO                TO  RECE44-ZAINUM
      *
           START   RECE44-FILE
               KEY IS    >=   RECE44-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE44
               NOT INVALID
                   PERFORM 990-RECE44-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPID    NOT =   RECE44-HOSPID       )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
               IF   RECE41-HKNKBN    NOT  =  "4"                        
                 EVALUATE      RECE44-SRYKBN                   
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
                        PERFORM    2099-RECE44-TENSOU-SEC
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
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "21"
                      IF   RECE44-COMCDFLG   =   "3"
                        PERFORM    2001-YAKUTEI-HENSYU-SEC
                      ELSE
                        PERFORM    2099-RECE44-TENSOU-SEC
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
                      MOVE    ZERO     TO    FLG-ARI
                      PERFORM   VARYING   IDX    FROM   1   BY   1
                            UNTIL   (IDX             >      18)         
                             OR     (FLG-ARI         =       1)
                          IF  RECE44-ZAINUM  =  WRK-RIHA-ZAINUM(IDX)
                              MOVE    1    TO   FLG-ARI 
                          END-IF
                      END-PERFORM
                      IF  FLG-ARI  =  1
                          PERFORM   2001-RIHA-COMMENT-SEC
                      ELSE
                          PERFORM   2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN   OTHER
                      PERFORM       2099-RECE44-TENSOU-SEC
                 END-EVALUATE
               ELSE                                                     
                 EVALUATE      RECE44-SRYKBN                            
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
                        PERFORM    2099-RECE44-TENSOU-SEC
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
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "21"
                      IF   RECE44-COMCDFLG   =   "3"
                        PERFORM    2001-YAKUTEI-HENSYU-SEC
                      ELSE
                        PERFORM    2099-RECE44-TENSOU-SEC
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
                   WHEN      "40"          
      *            ¡Ê¥ê¥Ï¥Ó¥ê¤Î¥»¥Ã¥È¡Ë
                      MOVE    ZERO     TO    FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        MOVE    ZERO     TO    FLG-ARI
                        PERFORM   VARYING   IDX    FROM   1   BY   1
                              UNTIL   (IDX             >      18)         
                               OR     (FLG-ARI         =       1)
                            IF  RECE44-ZAINUM  =  WRK-RIHA-ZAINUM(IDX)
                                MOVE    1    TO   FLG-ARI 
                            END-IF
                        END-PERFORM
                        IF  FLG-ARI  =  1
                            MOVE      99     TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                        END-IF
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN   OTHER                                         
                      PERFORM       2099-RECE44-TENSOU-SEC
                 END-EVALUATE                                           
               END-IF                                                   
      *
               PERFORM 990-RECE44-NEXT-SEC
      *
           END-PERFORM
      *
      *****ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ëÊÔ½¸£²****
           PERFORM     240-YAKUTEI-HENSYU2-SEC
      *
      *****º¹¹þÊÔ½¸£²********************
           PERFORM     250-SASHIKOMI2-SEC
      *
           PERFORM 990-RECE41-READ-SEC
      *
           IF  CNT-IN-COMMIT   =    300
                 MOVE  ZERO          TO      CNT-IN-COMMIT
                 PERFORM 900-DBCOMMIT-SEC
                 PERFORM 900-DBSTART-SEC
           END-IF
      *
      *    ½èÍýÃæ»ßÀßÄê½èÍý
           IF      CNT-IN-CANCEL   =   50
               MOVE    ZERO            TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
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
           MOVE    RECE41-HOSPID       TO  JYURRK-HOSPID
           MOVE    RECE41-PTID         TO  JYURRK-PTID
           MOVE    RECE41-NYUGAIKBN    TO  JYURRK-NYUGAIKBN
           MOVE    TBL-JUSHIN-SRYKA(IDX)
                                       TO  JYURRK-SRYKA
           MOVE    RECE41-SRYYM        TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    RECE41-SRYYM        TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY24"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING   MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY24"  TO  ORC-DBPATH 
               PERFORM   900-JYURRK-READ-SEC
               PERFORM   UNTIL   FLG-JYURRK   =    1
                  IF   RECE41-HKNKBN    NOT  =  "4" 
                   IF    JYURRK-HKNKBN    =       "1"
                      MOVE   JYURRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUMX
                      IF  WRK-HKNCOMBINUM9  =  RECE41-HKNCOMBI
                          IF  RECE41-HKNKBN   NOT =  "3"
                               MOVE   JYURRK-SRYYMD   TO   WRK-SRYYMDX
                               IF   RECE41-JNISSUTBL(WRK-SRYDD) = SPACE
                                  CONTINUE
                               ELSE
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-ARI (IDX)
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-DAY (IDX WRK-SRYDD)
                               END-IF
                          ELSE
                            IF  JYURRK-SRYYMD  =  RECE41-TEKSTYMD
                               MOVE   JYURRK-SRYYMD   TO   WRK-SRYYMDX
                               IF   RECE41-JNISSUTBL(WRK-SRYDD) = SPACE
                                  CONTINUE
                               ELSE
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-ARI (IDX)
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-DAY (IDX WRK-SRYDD)
                               END-IF
                            END-IF
                          END-IF
                      END-IF      
                   END-IF
                  ELSE
                   IF    JYURRK-HKNKBN    =       "2"
                      MOVE   JYURRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUMX
                      IF  WRK-HKNCOMBINUM9  =  RECE41-HKNCOMBI
                            MOVE   JYURRK-SRYYMD   TO   WRK-SRYYMDX
                            IF   RECE41-JNISSUTBL(WRK-SRYDD) = SPACE
                               CONTINUE
                            ELSE
                               MOVE   "1"     TO    
                                        TBL-JUSHIN-ARI (IDX)
                               MOVE   "1"     TO    
                                        TBL-JUSHIN-DAY (IDX WRK-SRYDD)
                            END-IF
                      END-IF      
                   END-IF
                  END-IF
      *
                  MOVE    "JYURRK-KEY24"  TO  ORC-DBPATH 
                  PERFORM   900-JYURRK-READ-SEC
               END-PERFORM
           END-IF
      *
           INITIALIZE                        MCPAREA
           MOVE    "DBCLOSECURSOR"   TO      MCP-FUNC
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
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
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
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
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
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
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
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
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
       20011-FREE-COMMENT-KAIGYO-SEC          SECTION.
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA(IDZ)
           MOVE    ALL "¡¡"        TO  WK-FREE-NAME(IDZ)(1:WRK-KETA)
      *
           .
       20011-FREE-COMMENT-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-JUSHIN-COMMENT-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                TO  IDX-RECE45
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *
           ADD     1            TO  IDX-RECE45
           MOVE    "810000001"  TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
           MOVE    ZERO         TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
           MOVE    7            TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
           MOVE    "Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç"
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
           MOVE    "1"          TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   49    )
                         OR      ( WK-FREE-NAME(IDX) =   SPACE )
                ADD    1        TO  IDX-RECE45
                MOVE   "810000001"
                                TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO     TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA(IDX)
                                TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME(IDX)
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE   "1"      TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-PERFORM 
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
                INITIALIZE                      RECE459-REC
                MOVE    RECE41-HOSPID       TO  RECE459-HOSPID
                MOVE    RECE41-SRYYM        TO  RECE459-SRYYM
                MOVE    RECE41-NYUGAIKBN    TO  RECE459-NYUGAIKBN
                MOVE    RECE41-PTID         TO  RECE459-PTID
                MOVE    RECE41-RECEKA       TO  RECE459-RECEKA
                MOVE    RECE41-HKNID        TO  RECE459-HKNID
                MOVE    RECE41-TEKSTYMD     TO  RECE459-TEKSTYMD
      *         MOVE    ZERO                TO  RECE459-ZAINUM
                MOVE    99999999            TO  RECE459-ZAINUM
                MOVE    "99"                TO  RECE459-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE459-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE45   =   1)
                   ADD         1            TO  IDX-RECE45
                   IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE
                       MOVE    1            TO  FLG-WRKRECE45
                   ELSE
                       MOVE  WRK-RECE45XX-SRYACT-TBL (IDX-RECE45 1)
                                        TO   RECE459-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                IF   RECE459-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE459-REC
                   ADD     1            TO   CNT-RECE459
                END-IF
           END-PERFORM     
      *
      *
           INITIALIZE                      RECE449-REC
           MOVE    RECE41-HOSPID       TO  RECE449-HOSPID
           MOVE    RECE41-SRYYM        TO  RECE449-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE449-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE449-PTID
           MOVE    RECE41-RECEKA       TO  RECE449-RECEKA
           MOVE    RECE41-HKNID        TO  RECE449-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE449-TEKSTYMD
      *    MOVE    ZERO                TO  RECE449-ZAINUM
           MOVE    99999999            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           .
       2001-JUSHIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æ±ÆüÊ£¿ôºÆ¿Ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-DOUJITU-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE449-REC
           MOVE    RECE41-HOSPID       TO  RECE449-HOSPID
           MOVE    RECE41-SRYYM        TO  RECE449-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE449-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE449-PTID
           MOVE    RECE41-RECEKA       TO  RECE449-RECEKA
           MOVE    RECE41-HKNID        TO  RECE449-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE449-TEKSTYMD
           MOVE    99999991            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           INITIALIZE                      RECE459-REC
           MOVE    RECE41-HOSPID       TO  RECE459-HOSPID
           MOVE    RECE41-SRYYM        TO  RECE459-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE459-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE459-PTID
           MOVE    RECE41-RECEKA       TO  RECE459-RECEKA
           MOVE    RECE41-HKNID        TO  RECE459-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE459-TEKSTYMD
           MOVE    99999991            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    Æ±ÆüÊ£¿ôºÆ¿Ç¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    11                   TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    "Æ±ÆüÊ£¿ôºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë"
                                        TO  WRK-RECE45XX-NAME(1 1)
      ***  MOVE    CNT-DOUJITU          TO  DAY-IDX
      ***  MOVE    TBL-DAY(DAY-IDX)     TO  WRK-RECE45XX-NAME(1 1)(15:4)
      *
           MOVE    CNT-DOUJITU          TO  DAY-IDX2
           PERFORM    20011-KAISU-HENSYU-SEC
           MOVE    WRK-HENSYU-KAISU2    TO  WRK-RECE45XX-NAME(1 1)(15:4)
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           WRITE   RECE459-REC
           ADD     1                    TO  CNT-RECE459
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
              MOVE    RECE44-HOSPID     TO      SRY-HOSPID
              MOVE    RECE44-NYUGAIKBN  TO      SRY-NYUGAIKBN
              MOVE    RECE44-PTID       TO      SRY-PTID
              MOVE    RECE44-SRYYM      TO      SRY-SRYYM
              MOVE    RECE44-ZAINUM     TO      SRY-ZAINUM
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
                     IF  RECE44-ZAINUM  >  WRK-RIHA-ZAINUM(IDX2)
                            MOVE  RECE44-ZAINUM  
                                          TO  WRK-RIHA-ZAINUM(IDX2)
                     END-IF
                     PERFORM  VARYING   IDX1  FROM  1  BY  1
                              UNTIL     IDX1  >   31
                            IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                                 MOVE  "1"  TO  WRK-RIGAKU(IDX2 IDX1)
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
           MOVE    RECE41-HOSPID       TO  SANTEI-HOSPID
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    CONS-KYUSEI         TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE41-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
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
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE41-SRYYM
                         AND  RECE41-SRYYM        <=  WRK-AFTER180(1:6)
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
           INITIALIZE                      RECE449-REC
           MOVE    RECE41-HOSPID       TO  RECE449-HOSPID
           MOVE    RECE41-SRYYM        TO  RECE449-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE449-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE449-PTID
           MOVE    RECE41-RECEKA       TO  RECE449-RECEKA
           MOVE    RECE41-HKNID        TO  RECE449-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE449-TEKSTYMD
           MOVE    99999998            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           INITIALIZE                      RECE459-REC
           MOVE    RECE41-HOSPID       TO  RECE459-HOSPID
           MOVE    RECE41-SRYYM        TO  RECE459-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE459-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE459-PTID
           MOVE    RECE41-RECEKA       TO  RECE459-RECEKA
           MOVE    RECE41-HKNID        TO  RECE459-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE459-TEKSTYMD
           MOVE    99999998            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE     9                   TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    "È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü" TO  WRK-RECE45XX-NAME(1 1)
           MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)    TO  WRK-RECE45XX-NAME(1 1)(7:4)
           MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)    TO  WRK-RECE45XX-NAME(1 1)(13:4)
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           WRITE   RECE459-REC
           ADD     1                    TO  CNT-RECE459
      *
           .
       20011-KYUSEIHASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ËýÀ­áÖÄË¼À´µ´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SHIDOU-HANTEI-SEC          SECTION.
      *
           IF   RECE44-FLG-SHIDOU-COMMENT   =    "1"
      *
             MOVE    ZERO              TO      FLG-SHIDOU-COMMENT
      *
      *      »»ÄêÍúÎò¸¡º÷
             INITIALIZE                  SANTEI-REC
             MOVE    RECE44-HOSPID       TO  SANTEI-HOSPID
             MOVE    RECE44-PTID         TO  SANTEI-PTID
             MOVE    RECE44-SRYYM        TO  SANTEI-SRYYM
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
                  PERFORM        900-SANTEI-READ-SEC
                  PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                     IF     SANTEI-FSANTEIYMD(1:6)  =  RECE44-SRYYM   
                           MOVE    1    TO      FLG-SHIDOU-COMMENT
                     END-IF
      * 
                     MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
                     PERFORM        900-SANTEI-READ-SEC
                  END-PERFORM
             END-IF
      *
             MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
             MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
             CALL    "ORCMCPSUB"         USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC
      *
             IF      FLG-SHIDOU-COMMENT    =      1
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-SHIDOU-COMMENT-SEC
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
       20011-SHIDOU-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       RECE44-REC    TO   RECE449-REC
           ADD        1             TO   RECE449-MEISAISU
           WRITE      RECE449-REC
           ADD        1             TO   CNT-RECE449
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPID       TO  RECE45-HOSPID
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE45
                 NOT INVALID
                     PERFORM 990-RECE45-NEXT-SEC
           END-START
      *
           MOVE    RECE45-REC           TO  RECE459-REC
           MOVE    RECE45-SRYACT-TBL(1) TO  RECE459-SRYACT-TBL(1)
           MOVE    RECE45-SRYACT-TBL(2) TO  RECE459-SRYACT-TBL(3)
           MOVE    RECE45-SRYACT-TBL(3) TO  RECE459-SRYACT-TBL(4)
           MOVE    RECE45-SRYACT-TBL(4) TO  RECE459-SRYACT-TBL(5)
      *
           MOVE    SPACE                TO  RECE459-SRYACT-TBL(2)
           INITIALIZE                       RECE459-SRYACT-TBL(2)
           MOVE    "810000001"          TO  RECE459-SRYCD(2)
           MOVE    ZERO                 TO  RECE459-SRYSURYO(2)
           MOVE    15                   TO  RECE459-YUKOKETA(2)
           MOVE    "ËýÀ­áÖÄË¼À´µ´ÉÍý»»ÄêÆü¡¡¡¡¡¡Æü"
                                        TO  RECE459-NAME(2)
           MOVE     TB-SANTEI-DAY(1)    TO  DAY-IDX
           MOVE     TBL-DAY(DAY-IDX)    TO  RECE459-NAME(2)(25:4)
      *
           MOVE     "1"                 TO  RECE459-SIKFLG(2)
      *
           WRITE    RECE459-REC
           ADD      1                   TO  CNT-RECE459
      *
           .
       20011-SHIDOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SYOBYO-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-SYOBYO-COMMENT   =    "1"
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
           IF      RECE44-FLG-RYOYO-COMMENT    =    "1"
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
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
           IF     RECE44-FLG-HOUSIJI-COMMENT   =    "1"
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
           IF   RECE44-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE44-KAISU(1 IDX3)  NOT =  ZERO
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
             MOVE    RECE44-HOSPID       TO  SANTEI-HOSPID
             MOVE    RECE44-PTID         TO  SANTEI-PTID
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
                       MOVE    RECE44-SRYYM    TO  WRK-HINKAIYMD(1:6)
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
             PERFORM    200111-RECE45-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE45
             MOVE    SPACE                TO  WRK-RECE45XX-REC
             INITIALIZE                       WRK-RECE45XX-REC
      *      ¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE44-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    16     TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "½é²ó»ØÆ³´ÉÍý»»ÄêÆü¡¡¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(21:4)
             MOVE    WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(27:4)
             MOVE    "1"    TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HINKAI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼ÔË¬ÌäÅÀÅ©Ãí¼Í´ÉÍý»ØÆ³ÎÁ»»Äê»þ¤ÎÅÀÅ©Ãí¼Í¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOUTEN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-HOUTEN-COMMENT   =    "1"
               PERFORM    20011-HOUTEN-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE44-TENSOU-SEC
           END-IF
      *
           .
       2001-HOUTEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼ÔË¬ÌäÅÀÅ©Ãí¼Í´ÉÍý»ØÆ³ÎÁ»»Äê»þ¤ÎÅÀÅ©Ãí¼Í¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-HOUTEN-COMMENT-SEC          SECTION.
      *
             PERFORM    200111-RECE45-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE45
             MOVE    SPACE                TO  WRK-RECE45XX-REC
             INITIALIZE                       WRK-RECE45XX-REC
      *      ¥³¥á¥ó¥È¤ÎÁÞÆþ
             ADD     1                    TO  IDX-RECE45
      *
             MOVE    "810000001"
                            TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
             MOVE    ZERO   TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
             MOVE    4      TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
             MOVE    "¡ÊË¬ÅÀ¡Ë"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
             MOVE    "1"    TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HOUTEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆü¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SHUJUTSU-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-SHUJUTU-COMMENT   =    SPACE
               MOVE       1       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE44-TENSOU-SEC
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
           IF      RECE44-FLG-CTMRI-COMMENT   =    "1"
                MOVE       2       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE44-TENSOU-SEC
           END-IF
      *
           .
       2001-CTMRI-HANTEI-EXT.
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
           PERFORM  200111-RECE45-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1                  TO  IDX-RECE45
                MOVE   "810000001"
                              TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO   TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE    "1"   TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
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
           PERFORM  200111-RECE45-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           MOVE    RECE44-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
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
                   IF    RECE44-KAISU(1 IDX)  NOT =  ZERO
                       ADD   1     TO   DAY-IDX
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
             PERFORM  200111-RECE45-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE45
             MOVE    SPACE                TO  WRK-RECE45XX-REC
             INITIALIZE                       WRK-RECE45XX-REC
      *      ¥ê¥Ï¥Ó¥ê¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE44-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7      TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "¼Â»ÜÆü¿ô¡¡¡¡Æü"
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
             MOVE    "1"    TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             EVALUATE   RECE44-ZAINUM
                 WHEN   WRK-RIGA1-SHU-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA1-SHU(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA2-SHU-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA2-SHU(IDX) NOT = SPACE
                                ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA3-SHU-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA3-SHU(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA4-SHU-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA4-SHU(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA1-KO-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA1-KO(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA2-KO-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA2-KO(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA3-KO-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA3-KO(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-RIGA4-KO-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-RIGA4-KO(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-SAGYO1-SHU-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-SAGYO1-SHU(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-SAGYO2-SHU-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-SAGYO2-SHU(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-SAGYO1-KO-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-SAGYO1-KO(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-SAGYO2-KO-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-SAGYO2-KO(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO1-SHU-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO1-SHU(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO2-SHU-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO2-SHU(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO3-SHU-ZAINUM                           
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO3-SHU(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO1-KO-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO1-KO(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO2-KO-ZAINUM
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO2-KO(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   WRK-GENGO3-KO-ZAINUM                            
                        PERFORM  VARYING  IDX   FROM  1  BY  1
                                 UNTIL    IDX   >   31
                            IF   WRK-GENGO3-KO(IDX) NOT = SPACE
                                 ADD      1     TO  CNT-RIHA
                            END-IF
                        END-PERFORM
                 WHEN   OTHER
                        CONTINUE
             END-EVALUATE
             IF      CNT-RIHA             NOT =    ZERO
               MOVE    CNT-RIHA             TO  DAY-IDX
               MOVE    TBL-DAY(DAY-IDX)
                           TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(9:4)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
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
             MOVE    RECE44-SRYKA        TO   TBL-YT1-SRYKA(IDX6)
      *
             MOVE    RECE44-REC          TO   TBL-YT1-RECE44-REC(IDX6)
      *
             MOVE    RECE44-ZAITEN       TO   TBL-YT1-ZAITEN(IDX6)
             MOVE    RECE44-SRYCDTOTAL   TO   TBL-YT1-SRYCDTOTAL(IDX6)
             MOVE    RECE44-SURYOUTOTAL  TO   TBL-YT1-SURYOUTOTAL(IDX6)
             MOVE    RECE44-RUIKEITOTAL  TO   TBL-YT1-RUIKEITOTAL(IDX6)
             MOVE    RECE44-MEISAISU     TO   TBL-YT1-MEISAISU(IDX6)
      *
             PERFORM   VARYING   IDX7    FROM   2   BY   1
                        UNTIL   (IDX7      >    4)
      *
               COMPUTE   IDX9    =   IDX7  -   1
               PERFORM   VARYING   IDX8    FROM   1   BY   1
                          UNTIL   (IDX8      >    31)
      *
                  MOVE    RECE44-KAISU(IDX7 IDX8)
                            TO    TBL-YT1-KAISU(IDX6 IDX8 IDX9)
      *
               END-PERFORM
      *
             END-PERFORM
      *
             IF      RECE44-SRYCDTOTAL  =   630010002
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
       2099-RECE44-TENSOU-SEC     SECTION.
      *
           MOVE       RECE44-REC    TO   RECE449-REC
           WRITE      RECE449-REC
           ADD        1             TO   CNT-RECE449
           PERFORM    20991-RECE45-TENSOU-SEC
      *
           .
       2099-RECE44-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£µÅ¾Á÷½èÍý
      *****************************************************************
       20991-RECE45-TENSOU-SEC     SECTION.
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPID       TO  RECE45-HOSPID
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE45           =   1                   )
               OR     ( RECE44-HOSPID    NOT =   RECE45-HOSPID       )
               OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
               OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
               OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
               OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
               OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
               OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
               OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
      *
               MOVE          RECE45-REC       TO      RECE459-REC
               WRITE         RECE459-REC
               ADD     1                 TO  CNT-RECE459
      *
               PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
           .
       20991-RECE45-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡ÂàÈò½èÍý
      *****************************************************************
       200111-RECE45-TAIHI-SEC     SECTION.
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPID       TO  RECE45-HOSPID
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE45
                 NOT INVALID
                     PERFORM 990-RECE45-NEXT-SEC
                     MOVE    RECE45-REC  TO  WRK-RECE45-REC
           END-START
      *
           .
       200111-RECE45-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       200111-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE45           =   1                   )
             OR     ( RECE44-HOSPID    NOT =   RECE45-HOSPID       )
             OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
             OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
             OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
             OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
             OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
             OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
             OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE45-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE45-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
             END-PERFORM 
      *
             PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¼ê½ÑÆü¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
      *
                  INITIALIZE                        RECE459-REC
                  MOVE    WRK-RECE45-KEY       TO   RECE459-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE459-RENNUM
                  MOVE    WRK-RECE45-PTNUM     TO   RECE459-PTNUM
                  MOVE    WRK-RECE45-SRYKA     TO   RECE459-SRYKA
                  MOVE    WRK-RECE45-SRYKBN    TO   RECE459-SRYKBN
                  MOVE    WRK-RECE45-SRYSYUKBN TO   RECE459-SRYSYUKBN
                  MOVE    WRK-RECE45-JIHIMONEY TO   RECE459-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE45   =   1)
                     ADD         1             TO    IDX-RECE45
                     IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE45
                     ELSE
                         MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                          TO   RECE459-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE45XX-SIKFLG(IDX-RECE45 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE459-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE459-REC
                     ADD     1            TO   CNT-RECE459
                  END-IF
           END-PERFORM     
      *
           MOVE    RECE44-REC        TO      RECE449-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE449-MEISAISU
           WRITE   RECE449-REC
           ADD     1                 TO      CNT-RECE449
      *
           .
       200111-SRYACTINF-HENSYU-EXT.
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
                         OR    (TBL-YT1-RECE44-REC(IDX6) =  SPACE)
      *
              COMPUTE    IDX7  =  IDX6  +  1
      *
              PERFORM    VARYING   IDX8   FROM   IDX7   BY   1
                          UNTIL   (IDX8                     >   100 )
                            OR    (TBL-YT1-RECE44-REC(IDX8) =  SPACE)
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
      *
           MOVE       ZERO      TO      IDX11
      *
           PERFORM    VARYING   IDX6    FROM   1   BY   1
                       UNTIL   (IDX6                     >   100 )
                         OR    (TBL-YT1-RECE44-REC(IDX6) =  SPACE)
      *
              IF     TBL-YT1-KEY(IDX6)   NOT =  WRK-TAIHI-YT1-KEY
      *
                 IF   WRK-TAIHI-YT1-KEY  NOT =  SPACE
                      PERFORM   2401-YAKUTEI-MATOME-SEC
                 END-IF
      *
                 INITIALIZE                        TBL-YAKUTEI-INF2
                 MOVE   TBL-YT1-KEY(IDX6)      TO  WRK-TAIHI-YT1-KEY
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
                              OR  (TBL-YT2X1-RECE44-REC(IDX7 IDX8 IDX9)
                                                              =  SPACE)
                                 CONTINUE
                           END-PERFORM
                           IF    IDX9   >   25
                             DISPLAY "TABLE OVER2"
                           ELSE
                             MOVE   TBL-YT1-RECE44-REC(IDX6)
                                TO  TBL-YT2X1-RECE44-REC(IDX7 IDX8 IDX9)
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
                              OR  (TBL-YT2X2-RECE44-REC(IDX7 IDX8 IDX9)
                                                              =  SPACE)
                                 CONTINUE
                           END-PERFORM
                           IF    IDX9   >   3
                             DISPLAY "TABLE OVER3"
                           ELSE
                             MOVE   TBL-YT1-RECE44-REC(IDX6)
                                TO  TBL-YT2X2-RECE44-REC(IDX7 IDX8 IDX9)
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
                         OR    (TBL-YT3X1-RECE44-REC(IDX9 1)  =  SPACE)
                         OR    (FLG-YAKUTEI                   =   1   )
      *
                    IF   (TBL-YT2Y1-HIKAKU(IDX7 IDX8 26) =
                                              TBL-YT3Y1-HIKAKU(IDX9 26))
                    AND  (TBL-YT2Y2-HIKAKU(IDX7 IDX8  4) =
                                              TBL-YT3Y2-HIKAKU(IDX9  4))
      *
                       PERFORM    VARYING   IDX10  FROM   1   BY   1
                         UNTIL (IDX10                         >    25 )
                           OR  (TBL-YT2X1-RECE44-REC(IDX7 IDX8 IDX10)
                                                              =  SPACE)
                         ADD    TBL-YT2X1-KAISU(IDX7 IDX8 IDX10)
                                    TO  TBL-YT3X1-KAISU(IDX9 IDX10)
                       END-PERFORM
                       PERFORM    VARYING   IDX10  FROM   1   BY   1
                         UNTIL (IDX10                         >     3 )
                           OR  (TBL-YT2X2-RECE44-REC(IDX7 IDX8 IDX10)
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
                        OR    (TBL-YT3X1-RECE44-REC(IDX7 1)  =  SPACE)
      *
                 PERFORM    VARYING   IDX8    FROM   1   BY   1
                             UNTIL   (IDX8                     >   25  )
                               OR    (TBL-YT3X1-RECE44-REC(IDX7 IDX8)
                                                               =  SPACE)
      *
                      INITIALIZE       RECE449-REC
                      MOVE   TBL-YT3X1-RECE44-REC(IDX7 IDX8)
                                  TO   RECE449-REC
                      MOVE   RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM
                      INITIALIZE       RECE449-DAY-AREA
                      MOVE   TBL-YT3X1-KAISU(IDX7 IDX8)
                                  TO   RECE449-ZAIKAISU
                      ADD       1             TO  IDX11
                      COMPUTE   WRK-ZAINUM    =  99990000  +  IDX11
                      MOVE      WRK-ZAINUM    TO  RECE449-ZAINUM
                      MOVE      RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM2
      *
                      WRITE     RECE449-REC
                      ADD       1            TO  CNT-RECE449
                      PERFORM   24021-YAKUTEI-RECE45-TENSOU-SEC
      *
                 END-PERFORM
      *
                 PERFORM    VARYING   IDX8    FROM   1   BY   1
                              UNTIL  (IDX8                     >    3  )
                                OR   (TBL-YT3X2-RECE44-REC(IDX7 IDX8)
                                                               =  SPACE)
      *
                      INITIALIZE       RECE449-REC
                      MOVE   TBL-YT3X2-RECE44-REC(IDX7 IDX8)
                                  TO   RECE449-REC
                      MOVE   RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM
                      INITIALIZE       RECE449-DAY-AREA
                      MOVE   TBL-YT3X2-KAISU(IDX7 IDX8)
                                  TO   RECE449-ZAIKAISU
                      ADD       1             TO  IDX11
                      COMPUTE   WRK-ZAINUM    =  99990000  +  IDX11
                      MOVE      WRK-ZAINUM    TO  RECE449-ZAINUM
                      MOVE      RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM2
      *
                      WRITE     RECE449-REC
                      ADD       1            TO  CNT-RECE449
                      PERFORM   24021-YAKUTEI-RECE45-TENSOU-SEC
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
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ë¥Õ¥¡¥¤¥ë½ÐÎÏ¡Ê£´£µ¥Õ¥¡¥¤¥ë¡Ë
      *****************************************************************
       24021-YAKUTEI-RECE45-TENSOU-SEC          SECTION.
      *
           INITIALIZE                       RECE45-REC
           MOVE    RECE449-HOSPID       TO  RECE45-HOSPID
           MOVE    RECE449-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE449-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE449-PTID         TO  RECE45-PTID
           MOVE    RECE449-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE449-HKNID        TO  RECE45-HKNID
           MOVE    RECE449-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    WRK-YAKUTEI-ZAINUM   TO  RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE45             =   1                  )
               OR     ( RECE449-HOSPID     NOT =   RECE45-HOSPID      )
               OR     ( RECE449-SRYYM      NOT =   RECE45-SRYYM       )
               OR     ( RECE449-NYUGAIKBN  NOT =   RECE45-NYUGAIKBN   )
               OR     ( RECE449-PTID       NOT =   RECE45-PTID        )
               OR     ( RECE449-RECEKA     NOT =   RECE45-RECEKA      )
               OR     ( RECE449-HKNID      NOT =   RECE45-HKNID       )
               OR     ( RECE449-TEKSTYMD   NOT =   RECE45-TEKSTYMD    )
               OR     ( WRK-YAKUTEI-ZAINUM NOT =   RECE45-ZAINUM      )
      *
               MOVE     RECE45-REC            TO      RECE459-REC
               MOVE     WRK-YAKUTEI-ZAINUM2   TO      RECE459-ZAINUM
               WRITE    RECE459-REC
               ADD      1                     TO      CNT-RECE459
      *
               PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
           .
       24021-YAKUTEI-RECE45-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    º¹¹þÊÔ½¸£²
      *****************************************************************
       250-SASHIKOMI2-SEC     SECTION.
      *
      *    ¥Ñ¥é¥á¡¼¥¿£´£´£¹¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           INITIALIZE                      RECE449-REC
           MOVE    RECE41-HOSPID       TO  RECE449-HOSPID
           MOVE    RECE41-SRYYM        TO  RECE449-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE449-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE449-PTID
           MOVE    RECE41-RECEKA       TO  RECE449-RECEKA
           MOVE    RECE41-HKNID        TO  RECE449-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE449-TEKSTYMD
           MOVE    ZERO                TO  RECE449-ZAINUM
      *
           START   RECE449-FILE
               KEY IS    >=   RECE449-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE449
               NOT INVALID
                   PERFORM 990-RECE449-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE449          =   1                    )
               OR     ( RECE41-HOSPID    NOT =   RECE449-HOSPID       )
               OR     ( RECE41-SRYYM     NOT =   RECE449-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE449-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE449-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE449-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE449-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE449-TEKSTYMD     )
      *
               MOVE   ZERO   TO  FLG-COMMENT
      *        ¡Ê»ÀÁÇ·×»»¼°¤Î¥³¥á¥ó¥È¤Î¥»¥Ã¥È¡Ë
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2501-SANSO-HANTEI-SEC
               END-IF
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2599-RECE449-TENSOU-SEC
               END-IF
      *
               PERFORM 990-RECE449-NEXT-SEC
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
           IF     RECE449-FLG-SANSO-COMMENT   =    "1"
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
           INITIALIZE                       RECE459-REC
           MOVE    RECE449-HOSPID       TO  RECE459-HOSPID
           MOVE    RECE449-SRYYM        TO  RECE459-SRYYM
           MOVE    RECE449-NYUGAIKBN    TO  RECE459-NYUGAIKBN
           MOVE    RECE449-PTID         TO  RECE459-PTID
           MOVE    RECE449-RECEKA       TO  RECE459-RECEKA
           MOVE    RECE449-HKNID        TO  RECE459-HKNID
           MOVE    RECE449-TEKSTYMD     TO  RECE459-TEKSTYMD
           MOVE    RECE449-ZAINUM       TO  RECE459-ZAINUM
           START   RECE459-FILE
               KEY IS    >=   RECE459-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE459
               NOT INVALID
                   PERFORM 990-RECE459-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE459           =   1                 )
               OR     ( RECE449-HOSPID    NOT =   RECE459-HOSPID    )
               OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM     )
               OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN )
               OR     ( RECE449-PTID      NOT =   RECE459-PTID      )
               OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA    )
               OR     ( RECE449-HKNID     NOT =   RECE459-HKNID     )
               OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD  )
               OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM    )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE459-SRYCD(IDX) =   SPACE )
      *
                  SET     TBL-SANSO    TO     1
                  SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                       AT  END
                           MOVE    ZERO    TO   FLG-SANSO-OK
                       WHEN   RECE459-SRYCD(IDX) = TBL-SANSO-SRYCD
                                                       (TBL-SANSO)
                           MOVE    1       TO   FLG-SANSO-OK
                  END-SEARCH
                  IF      FLG-SANSO-OK        =    1
                     ADD      1             TO  IDX4
                     MOVE  RECE459-TEN(IDX) TO  TBL-SANSO-TEN(IDX4)
                     MOVE  RECE459-SRYSURYO(IDX)
                                            TO  TBL-SANSO-SRYSURYO
                                                             (IDX4)
                  END-IF
                  IF    RECE459-SRYCD(IDX) = "770030070"
                     IF   TBL-KASAN-SRYSURYO  =  ZERO
                        MOVE   RECE459-SRYSURYO(IDX)
                                     TO  TBL-KASAN-SRYSURYO
                     ELSE
                        COMPUTE   TBL-KASAN-SRYSURYO  =  
                                  TBL-KASAN-SRYSURYO  *  
                                  RECE459-SRYSURYO(IDX)  
                     END-IF
                  END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE459-NEXT-SEC
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
           PERFORM  250111-RECE459-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£´£µ£¹¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
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
      *    £´£µ£¹¡¡ÂàÈò½èÍý
      *****************************************************************
       250111-RECE459-TAIHI-SEC     SECTION.
      *
           INITIALIZE                       RECE459-REC
           MOVE    RECE449-HOSPID       TO  RECE459-HOSPID
           MOVE    RECE449-SRYYM        TO  RECE459-SRYYM
           MOVE    RECE449-NYUGAIKBN    TO  RECE459-NYUGAIKBN
           MOVE    RECE449-PTID         TO  RECE459-PTID
           MOVE    RECE449-RECEKA       TO  RECE459-RECEKA
           MOVE    RECE449-HKNID        TO  RECE459-HKNID
           MOVE    RECE449-TEKSTYMD     TO  RECE459-TEKSTYMD
           MOVE    RECE449-ZAINUM       TO  RECE459-ZAINUM
           START   RECE459-FILE
                 KEY IS    >=   RECE459-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE459
                 NOT INVALID
                     PERFORM 990-RECE459-NEXT-SEC
                     MOVE  RECE459-REC  TO  WRK-RECE45-REC
           END-START
      *
           .
       250111-RECE459-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £´£µ£¹¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       250111-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE459           =   1                    )
             OR     ( RECE449-HOSPID    NOT =   RECE459-HOSPID       )
             OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM        )
             OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN    )
             OR     ( RECE449-PTID      NOT =   RECE459-PTID         )
             OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA       )
             OR     ( RECE449-HKNID     NOT =   RECE459-HKNID        )
             OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD     )
             OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX                >   5     )
                       OR      ( RECE459-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE459-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
      *
                  IF   RECE459-SRYCD(IDX) = "770020070" OR "770030070"
                       MOVE   IDX-RECE45   TO   WRK-JYUNBAN
                  END-IF
      *
             END-PERFORM 
      *
             PERFORM 990-RECE459-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-GYOSUU   NOT =  ZERO
      *
      *      ÊäÀµÎ¨Ëô¤Ï¹âµ¤°µ»ÀÁÇ²Ã»»°Ê¹ß¤ÎÌÀºÙ¤ò¤º¤é¤¹
             IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE45   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  WRK-GYOSUU
                   MOVE   WRK-RECE45XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE45XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE45XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
             END-IF
      *
      *      ·×»»¼°¤Îº¹¤·¹þ¤ß
             IF   WRK-JYUNBAN  NOT =  ZERO
                 MOVE      WRK-JYUNBAN    TO     MAX-MEISAISU
             ELSE
                 MOVE      IDX-RECE45     TO     MAX-MEISAISU
             END-IF
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   50    )
                         OR    ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD    1     TO  MAX-MEISAISU
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
      *
             END-PERFORM
      *
           END-IF
      *
      *    ¼ê½ÑÆü¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
      *
                  INITIALIZE                        RECE451-REC
                  MOVE    WRK-RECE45-KEY       TO   RECE451-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE451-RENNUM
                  MOVE    WRK-RECE45-PTNUM     TO   RECE451-PTNUM
                  MOVE    WRK-RECE45-SRYKA     TO   RECE451-SRYKA
                  MOVE    WRK-RECE45-SRYKBN    TO   RECE451-SRYKBN
                  MOVE    WRK-RECE45-SRYSYUKBN TO   RECE451-SRYSYUKBN
                  MOVE    WRK-RECE45-JIHIMONEY TO   RECE451-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE45   =   1)
                     ADD         1             TO    IDX-RECE45
                     IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE45
                     ELSE
                         MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                          TO   RECE451-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE45XX-SIKFLG(IDX-RECE45 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE451-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE451-REC
                     ADD     1            TO   CNT-RECE45
                  END-IF
           END-PERFORM     
      *
           MOVE    RECE449-REC       TO      RECE441-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE441-MEISAISU
           WRITE   RECE441-REC
           ADD     1                 TO      CNT-RECE44
      *
           .
       250111-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£´£´£¹Å¾Á÷½èÍý
      *****************************************************************
       2599-RECE449-TENSOU-SEC     SECTION.
      *
           MOVE       RECE449-REC    TO   RECE441-REC
           WRITE      RECE441-REC
           ADD        1              TO   CNT-RECE44
           PERFORM    25991-RECE459-TENSOU-SEC
      *
           .
       2599-RECE449-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£´£µ£¹Å¾Á÷½èÍý
      *****************************************************************
       25991-RECE459-TENSOU-SEC     SECTION.
      *
           INITIALIZE                       RECE459-REC
           MOVE    RECE449-HOSPID       TO  RECE459-HOSPID
           MOVE    RECE449-SRYYM        TO  RECE459-SRYYM
           MOVE    RECE449-NYUGAIKBN    TO  RECE459-NYUGAIKBN
           MOVE    RECE449-PTID         TO  RECE459-PTID
           MOVE    RECE449-RECEKA       TO  RECE459-RECEKA
           MOVE    RECE449-HKNID        TO  RECE459-HKNID
           MOVE    RECE449-TEKSTYMD     TO  RECE459-TEKSTYMD
           MOVE    RECE449-ZAINUM       TO  RECE459-ZAINUM
           START   RECE459-FILE
               KEY IS    >=   RECE459-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE459
               NOT INVALID
                   PERFORM 990-RECE459-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE459           =   1                    )
               OR     ( RECE449-HOSPID    NOT =   RECE459-HOSPID       )
               OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM        )
               OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN    )
               OR     ( RECE449-PTID      NOT =   RECE459-PTID         )
               OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA       )
               OR     ( RECE449-HKNID     NOT =   RECE459-HKNID        )
               OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD     )
               OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM       )
      *
               MOVE    RECE459-REC       TO   RECE451-REC
               WRITE   RECE451-REC
               ADD     1                 TO   CNT-RECE45
      *
               PERFORM 990-RECE459-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25991-RECE459-TENSOU-EXT.
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
           DISPLAY "RECE41  CNT-IN  :" CNT-RECE41  
           DISPLAY "RECE449 CNT-WK  :" CNT-RECE449 
           DISPLAY "RECE459 CNT-WK  :" CNT-RECE459 
           DISPLAY "RECE44  CNT-OUT :" CNT-RECE44  
           DISPLAY "RECE45  CNT-OUT :" CNT-RECE45  
           DISPLAY "*** ORCR0430 END ***"
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
           CLOSE                       RECE41-FILE
      *
           CLOSE                       RECE44-FILE
      *
           CLOSE                       RECE45-FILE
      *
           CLOSE                       RECE441-FILE     
           CLOSE                       RECE451-FILE     
      *
           CLOSE                       RECE449-FILE
           CLOSE                       RECE459-FILE
      *
           .
       310-FILE-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿½ç¸¡º÷½èÍý
      *****************************************************************
       990-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE41
                   ADD     1           TO  CNT-IN-COMMIT
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
           END-READ
      *
           .
       990-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE44-NEXT-SEC           SECTION.
      *
           READ    RECE44-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE44
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE44
           END-READ
      *
           .
       990-RECE44-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£´£´£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE449-NEXT-SEC           SECTION.
      *
           READ    RECE449-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE449
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE449
           END-READ
      *
           .
       990-RECE449-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE45-NEXT-SEC           SECTION.
      *
           READ    RECE45-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE45
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE45
           END-READ
      *
           .
       990-RECE45-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£´£µ£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE459-NEXT-SEC           SECTION.
      *
           READ    RECE459-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE459
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE459
           END-READ
      *
           .
       990-RECE459-NEXT-EXT.
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
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
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
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼õ¿ÇÍúÎòÆÉ¹þ
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
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
       900-JYURRK-READ-EXT.
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
