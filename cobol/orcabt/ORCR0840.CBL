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
       PROGRAM-ID.             ORCR0840.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥ÈÆþ±¡¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/06/01    NaCl¡ÝÌçÏÆ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01   NACL-ÌçÏÆ     04/01/06  ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¤Î£±·åÌÜ¤ÎÈ½Äê
      *                                     ½¤Àµ
      *  01.00.02    NACL-ÌçÏÆ    04.01.21  Ï«ºÒ¤ÎÆÃÄê´ïºà(09593)ÂÐ±þ
      *  01.00.03    NACL-ÌçÏÆ    04.01.21  Ï«ºÒ¡¦¼«ÇåÀÕ¤ÎÆþ±¡¼¼ÎÁ²Ã»»
      *                                     (09594)ÂÐ±þ
      *  01.00.04    NACL-ÌçÏÆ    04/01/28  ¿ÇÎÅ¶èÊ¬£¹£°¡¢£¹£²¤Î¥³¥á¥ó¥È
      *                                     ¤Î¤ß¤ÎºÞ¤â¥ì¥»µ­ºÜ¤ÎÂÐ¾Ý¤Ë¤¹¤ë
      *  01.00.05    NACL-ÌçÏÆ    04/02/05  ¿ÇÎÅ¥³¡¼¥É¡ö¿ôÎÌ¤ÎÎß·×ÊÔ½¸
      *  01.00.06    NACL-ÌçÏÆ    04/02/06  ¼«ÇåÀÕ¤Î¿©»öÎÅÍÜÈñ¤òÏ«ºÒ
      *                                     ¤Î£±¡¥£²ÇÜ¤Ç·×»»¤¹¤ë
      *  £²£°£°£´Ç¯£´·î²þÀµÂÐ±þ
      *  01.01.01    NACL-ÌçÏÆ    04/04/01  êóáì´µ¼Ô´ÉÍý²Ã»»¤ÎÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
           SELECT  RECE84-FILE     ASSIGN  RECE84PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE84-KEY
                                   FILE    STATUS  IS  STS-RECE84.
           SELECT  RECE844-FILE    ASSIGN  RECE84PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE844-KEY
                                   FILE    STATUS  IS  STS-RECE84.
           SELECT  RECE8A-FILE     ASSIGN  RECE8APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE8A-KEY
                                   FILE    STATUS  IS  STS-RECE8A.
           SELECT  RECE8A4-FILE    ASSIGN  RECE8APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8A4-KEY
                                   FILE    STATUS  IS  STS-RECE8A.
           SELECT  RECE85-FILE     ASSIGN  RECE85PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE85-KEY
                                   FILE    STATUS  IS  STS-RECE85.
           SELECT  RECE855-FILE    ASSIGN  RECE85PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE855-KEY
                                   FILE    STATUS  IS  STS-RECE85.
           SELECT  RECE8B-FILE     ASSIGN  RECE8BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE8B-KEY
                                   FILE    STATUS  IS  STS-RECE8B.
           SELECT  RECE8B5-FILE    ASSIGN  RECE8BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8B5-KEY
                                   FILE    STATUS  IS  STS-RECE8B.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC. 
           COPY    "CPRCF082.INC".
      *
       FD  RECE84-FILE.
       01  RECE84-REC. 
           COPY    "CPRCF084.INC".
      *
       FD  RECE844-FILE.
       01  RECE844-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE844//.
      *
       FD  RECE8A-FILE.
       01  RECE8A-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE8A//.
      *
       FD  RECE8A4-FILE.
       01  RECE8A4-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE8A4//.
      *
       FD  RECE85-FILE.
       01  RECE85-REC. 
           COPY    "CPRCF085.INC".
      *
       FD  RECE855-FILE.
       01  RECE855-REC. 
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //RECE855//.
      *
       FD  RECE8B-FILE.
       01  RECE8B-REC. 
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //RECE8B//.
      *
       FD  RECE8B5-FILE.
       01  RECE8B5-REC. 
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //RECE8B5//.
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
                                   BY         //RECE8A//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE85//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE8B//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE82                              PIC X(02).
           03  STS-RECE84                              PIC X(02).
           03  STS-RECE85                              PIC X(02).
           03  STS-RECE8A                              PIC X(02).
           03  STS-RECE8B                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-SP                                  PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-NYUINACT                            PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-DAY21                               PIC 9(01).
           03  FLG-DAY24                               PIC 9(01).
           03  FLG-TENTEKI                             PIC 9(01).
           03  FLG-WRITE85                             PIC 9(01).
           03  FLG-SIKIBETU                            PIC 9(01).
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-RECE85-ZAN                          PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-OKIKAE                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-HEYA                                PIC 9(01).
           03  FLG-KHN                                 PIC 9(01).
           03  FLG-NYUINRYO                            PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-HOUSIJI-COM                         PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-COMMIT                           PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-OUT3                                PIC 9(06).
           03  CNT-OUT4                                PIC 9(06).
           03  CNT-OUT5                                PIC 9(06).
           03  CNT-DAY21                               PIC 9(06).
           03  CNT-DAY24                               PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX5                                    PIC 9(03).
           03  IDX6                                    PIC 9(03).
           03  IDX7                                    PIC 9(03).
           03  IDX8                                    PIC 9(03).
           03  IDX9                                    PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDY2                                    PIC 9(03).
           03  IDZ                                     PIC 9(03).
           03  IDW                                     PIC 9(03).
           03  ID1                                     PIC 9(03).
           03  ID2                                     PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  SYUKEI-IDX                              PIC 9(03).
           03  SYUKEI-IDX2                             PIC 9(03).
           03  SYUKEI-IDX3                             PIC 9(03).
           03  SYUKEI-IDX4                             PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
      *
      *    ´ðËÜÅÀ¿ô¤ÎÈóÉ½¼¨È½ÄêÍÑ¥Õ¥é¥°¡Ê¼«ÇåÀÕÍÑ¡Ë
       01  FLG-KHNTEN-JI-AREA.                                          
           03  FLG-KHNTEN7-JI                          PIC 9(01).       
           03  FLG-KHNTEN8-JI                          PIC 9(01).       
           03  FLG-KHNTEN9-JI                          PIC 9(01).       
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
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
      *
       01  WRK-DAY-TABLE2.
           03  WRK-DAY-T2                              OCCURS   31.
               05  WRK-DAY21                           PIC 9(04).
               05  WRK-DAY24                           PIC 9(04).
      *
       01  WRK-SKJ-TABLE.
           03  WRK-SKJ-T                               OCCURS   31.
               05  WRK-SKJ-ZAITEN                      PIC 9(08).
               05  WRK-SKJ-ZAIKAISU                    PIC 9(04).
               05  WRK-SKJ-SRYCD-T.
                06  WRK-SKJ-SRYCD-OCC                  OCCURS   15.
                 07  WRK-SKJ-SRYCD                     PIC 9(09).
      *
       01  WRK-SKJ-TABLE2.
           03  WRK-SKJ-T2                              OCCURS   31.
               05  WRK-SKJ2-ZAITEN                     PIC 9(08).
               05  WRK-SKJ2-ZAIKAISU                   PIC 9(04).
               05  WRK-SKJ2-MEISAISU                   PIC 9(04).
               05  WRK-SKJ2-SRYCDTOTAL                 PIC 9(14).
               05  WRK-SKJ2-SRYCD-T.
                06  WRK-SKJ2-SRYCD-OCC                 OCCURS   15.
                 07  WRK-SKJ2-SRYCD                    PIC 9(09).
               05  WRK-SKJ2-KAISU-T.
                06  WRK-SKJ2-KAISU-OCC                 OCCURS   31.
                 07  WRK-SKJ2-KAISU                    PIC 9(01).
      *
       01  WRK-ORCSTEN-TABLE.
           03  WRK-ORCSTEN-T                           OCCURS  100.
               05  WRK-ORCSTEN-SRYCD                   PIC X(09).
               05  WRK-ORCSTEN-SURYO                   PIC 9(05)V9(03).
           03  WRK-ORCSTEN-T2                          OCCURS   31.
               05  WRK-ORCSTEN-DAY                     PIC 9(03).
           03  WRK-ORCSTEN-ZAIKAISU                    PIC 9(04).
      *
      *    °åÎÅµ¡´Ø¡ÊÆþ±¡´ðËÜ¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC     OCCURS  10.
               05  TBL-SYS5000                         PIC X(610).
      *    Ï«ºÒ¼«Çå°åÎÅµ¡´ØÂàÈòÍÑÎÎ°è
       01  TBL-SYS4001-T.
           03  TBL-SYS4001-OCC     OCCURS  10.
               05  TBL-SYS4001                         PIC X(610).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-TEN                                 PIC 9(09).
           03  WRK-MEISAI-CNT                          PIC 9(04).
           03  WRK-SRYCDTOTAL                          PIC 9(14).
           03  WRK-SURYOUTOTAL                         PIC 9(07)V9(03).
           03  WRK-KSNRATE                             PIC 9V99.    
           03  WRK-YOUBUKASAN                          PIC 9(07).
           03  WRK-RYOYOTEATE                          PIC 9(07).
           03  WRK-RYOYOTEATE2                         PIC 9(07).
      *
           03  WRK-HEYA-NINZU                          PIC 9(02).
      *
           03  WRK-SRYCD                               PIC 9(09).
           03  WRK-ZAINUM                              PIC 9(08).
           03  WRK-RENNUM                              PIC 9(02).
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
      *
           03  WRK-SRYSURYO                            PIC 9(05)V9(03).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
      *
           03  WRK-RECEERR                             PIC X(200).
      *
           03  WRK-SHOKUJIRYOYO                        PIC 9(01).
           03  WRK-TOKUKANRIKASAN                      PIC 9(01).
           03  WRK-4001-SKJKBN                         PIC X(01).
      *
           03  WRK-KEISAN                              PIC 9(09).
           03  WRK-KOYOMI-YMD                          PIC 9(08).
           03  WRK-KOYOMI2-YMD.
               05  WRK-KOYOMI2-YYMM                    PIC 9(06).
               05  WRK-KOYOMI2-DD                      PIC 9(02).
      *
           03  WRK-SROUNYUIN-SRYCDTOTAL                PIC 9(14).
           03  WRK-SROUNYUIN-MEISAISU                  PIC 9(07).
           03  WRK-SROUNYUIN-SRYCD                     PIC 9(09).
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
      *    ÆýÍÄ»ù²Ã»»
           03  CONS-NYUKSN1        PIC X(09) VALUE "111000370".
           03  CONS-NYUKSN2        PIC X(09) VALUE "111011470".
      *
      *    Ï«ºÒ¼«ÇåÀÕ´Ø·¸
           03  CONS-ROUSAI1    PIC X(09)   VALUE   "101110010".
           03  CONS-ROUSAI2    PIC X(09)   VALUE   "101110020".
           03  CONS-ROUSAI3    PIC X(09)   VALUE   "101130010".
           03  CONS-ROUSAI4    PIC X(09)   VALUE   "101130020".
           03  CONS-ROUSAI5    PIC X(09)   VALUE   "101130030".
           03  CONS-ROUSAI6    PIC X(09)   VALUE   "101130040".
           03  CONS-ROUSAI7    PIC X(09)   VALUE   "101130050".
           03  CONS-ROUSAI8    PIC X(09)   VALUE   "101130060".
           03  CONS-ROUSAI9    PIC X(09)   VALUE   "101130070".
           03  CONS-ROUSAI10   PIC X(09)   VALUE   "101130080".
           03  CONS-ROUSAI11   PIC X(09)   VALUE   "101130090".
           03  CONS-ROUSAI12   PIC X(09)   VALUE   "101130100".
           03  CONS-ROUSAI13   PIC X(09)   VALUE   "101910010".
           03  CONS-ROUSAI14   PIC X(09)   VALUE   "101910020".
           03  CONS-ROUSAI15   PIC X(09)   VALUE   "101910030".
           03  CONS-ROUSAI16   PIC X(09)   VALUE   "101910040".
           03  CONS-ROUSAI17   PIC X(09)   VALUE   "101910070".
           03  CONS-ROUSAI18   PIC X(09)   VALUE   "101910080".
           03  CONS-ROUSAI19   PIC X(09)   VALUE   "101910090".
           03  CONS-ROUSAI20   PIC X(09)   VALUE   "101910100".
      *
           03  CONS-ROUCOM1    PIC X(09)   VALUE   "840800001".
           03  CONS-ROUCOM2    PIC X(09)   VALUE   "840800002".
           03  CONS-ROUCOM3    PIC X(09)   VALUE   "820800001".
           03  CONS-ROUCOM4    PIC X(09)   VALUE   "820800002".
           03  CONS-ROUCOM5    PIC X(09)   VALUE   "820800003".
           03  CONS-ROUCOM6    PIC X(09)   VALUE   "820800004".
           03  CONS-ROUCOM7    PIC X(09)   VALUE   "830800001".
      *
      *    ¹øÉô¸ÇÄêÂÓ²Ã»»
           03  CONS-YOUBUKASAN PIC X(09)   VALUE   "140037490".
      *    ÎÅÍÜÃ´Åö¼êÅö¡ÊÆþ±¡¡Ë
           03  CONS-RYOYOTEATE PIC X(09)   VALUE   "199000510".
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
      *    Àº¿À²ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ
           03  CONS-HOUSEI-COMMENT   PIC X(09)  VALUE  "180018310".
      *    Àº¿À²Ê³ÖÎ¥ÉÂ¼¼´ÉÍý²Ã»»
           03  CONS-KAKURI-COMMENT   PIC X(09)  VALUE  "190107070".
      *    Ï«ºÒ¼£ÎÅ·×²è²Ã»»
           03  CONS-CHIRYO-COMMENT   PIC X(09)  VALUE  "101910050".
      *
      *    ÉÂ°áÂßÍ¿ÎÁ
           03  CONS-BYOITAIYO        PIC X(09)  VALUE  "101910060".
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
      *    Ï«ºÒ¼«Çå°åÎÅµ¡´Ø¾ðÊó
           COPY    "CPSK4001.INC".
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
      *    Ï«ºÒÆþ±¡²ñ·×ºîÀ®¥µ¥Ö
           COPY    "CPORCSROUNYUIN.INC".
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
           MOVE    "ORCR0840"      TO  JOB-PGID
           MOVE    "¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *      ¿©»ö¡¦Ì¤¼Â»Ü¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
             MOVE     ZERO            TO  WRK-SHOKUJIRYOYO
                                          WRK-TOKUKANRIKASAN
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
                 PERFORM 110-SYSKAN-HENSYU-SEC
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
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£´£°£°£±¡Ë¤«¤é¼«ÇåÀÕ¿©»ö
      *       ÎÅÍÜÈñ¶èÊ¬¤Î¼èÆÀ
              MOVE    SPACE                TO    WRK-4001-SKJKBN
      *
              INITIALIZE                         SYSKANRI-REC
              MOVE    "4001"               TO    SYS-KANRICD
              MOVE    "*"                  TO    SYS-KBNCD
              MOVE    WRK-PARA-SRYYM       TO    ORC-DBYMD (1:6)
              MOVE    "01"                 TO    ORC-DBYMD (7:2)
              MOVE    SYSKANRI-REC         TO    MCPDATA-REC
              MOVE    "DBSELECT"           TO    MCP-FUNC
              MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH
              CALL    "ORCMCPSUB"                USING
                                                 MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "SYSKANRI-KEY"   TO  ORC-DBPATH
                  PERFORM 960-KANRIMST-READ-SEC
                  MOVE    MCPDATA-REC      TO  SYS-4001-REC
                  MOVE    SYS-4001-SKJKBN  TO  WRK-4001-SKJKBN
              END-IF
      *
              MOVE   "SYSKANRI-KEY"        TO  WK-PATH
              PERFORM   CSR-CLOSE-SEC
           ELSE
              MOVE     ZERO                TO  SYSKAN-IDX
              MOVE     SPACE               TO  TBL-SYS4001-T
              INITIALIZE                       TBL-SYS4001-T
      *
              INITIALIZE                       SYSKANRI-REC
              MOVE    "4001"               TO  SYS-KANRICD
              MOVE    "*"                  TO  SYS-KBNCD
              MOVE    SYSKANRI-REC         TO  MCPDATA-REC
              MOVE    "DBSELECT"           TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"      TO  ORC-DBPATH
              CALL    "ORCMCPSUB"              USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
              IF      MCP-RC              =   ZERO
                 MOVE    "SYSKANRI-KEY8"   TO  ORC-DBPATH
                 PERFORM 960-KANRIMST-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
      *
                   MOVE    MCPDATA-REC     TO  SYS-4001-REC
                   ADD     1               TO  SYSKAN-IDX
                   MOVE   SYS-4001-REC     TO  TBL-SYS4001(SYSKAN-IDX)
      *
                   MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                   PERFORM 960-KANRIMST-READ-SEC
      *
                 END-PERFORM
              END-IF
      *
              MOVE   "SYSKANRI-KEY8"       TO  WK-PATH
              PERFORM   CSR-CLOSE-SEC
           END-IF
      *
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
           MOVE    "RECE840"           TO  RECE84PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84PARA-TERMID
           MOVE    "RECE850"           TO  RECE85PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE85PARA-TERMID
      *
           MOVE    "RECE841"           TO  RECE8APARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE8APARA-TERMID
           MOVE    "RECE851"           TO  RECE8BPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE8BPARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    OUTPUT              RECE844-FILE
           CLOSE                       RECE844-FILE
      *
           OPEN    OUTPUT              RECE855-FILE
           CLOSE                       RECE855-FILE
      *
           OPEN    OUTPUT              RECE8A4-FILE
           CLOSE                       RECE8A4-FILE
      *
           OPEN    OUTPUT              RECE8B5-FILE
           CLOSE                       RECE8B5-FILE
      *
           OPEN    OUTPUT              RECE84-FILE
           OPEN    I-O                 RECE85-FILE
      *
           OPEN    OUTPUT              RECE8A-FILE
                                       RECE8B-FILE
      *
           OPEN    I-O                 RECE82-FILE
      *
           PERFORM    RECE82-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥«¥ó¤ÎÊÔ½¸
      *****************************************************************
       110-SYSKAN-HENSYU-SEC                SECTION.
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
       110-SYSKAN-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 210-HENSYU-SEC
      *
           PERFORM RECE82-READ-SEC
      *
           IF  CNT-IN-COMMIT   =   300
               MOVE  ZERO          TO      CNT-IN-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               PERFORM 900-DBSTART-SEC
           END-IF
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
      *
           INITIALIZE          FLG-KHNTEN-JI-AREA
      *
           MOVE   ZERO   TO    FLG-HEYA
      *
           PERFORM    220-RECE84-HENSYU-SEC
           PERFORM    230-RECE84-HENSYU2-SEC
           PERFORM    240-RECE84-HENSYU3-SEC
           PERFORM    250-RECE84-HENSYU4-SEC
           PERFORM    260-FTN-SET-SEC
      *
           IF      RECE82-HKNKBN    =   "4"
             PERFORM  270-KHNTEN-JI-CLEAR-SEC
           END-IF                            
      *
           PERFORM    RECE82-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       220-RECE84-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE82-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE82-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE82-PTID          TO      ACCT-PTID
           MOVE    RECE82-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE82-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY22"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY22"  TO      ORC-DBPATH
               PERFORM  SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
      *          Ì¤¼Â»Ü¸º»»¡¦Æü¾ïÀ¸³è¾ã³²²Ã»»¡¦ÃÔÊò²Ã»»¡¦
      *          êóáì´µ¼Ô´ÉÍý²Ã»»¤ò½ü¤¯
                 IF   ACCT-SRYCDTOTAL  =  190080470  OR  190799670
                                      OR  190080570  OR  190799770
                                      OR  190111690  OR  190809090
                                      OR  190111790  OR  190809190
      *
                                      OR  190109170  OR  190807570
                                      OR  190109270  OR  190808670
                                      OR  190116990  OR  190813390
                   CONTINUE
                 ELSE
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)
      *
                       MOVE   ZERO   TO   WRK-TEN
                                          WRK-MEISAI-CNT
                                          WRK-SRYCDTOTAL
                                          WRK-SURYOUTOTAL
                                          WRK-YOUBUKASAN
                                          WRK-RYOYOTEATE
      *
                       MOVE   ZERO   TO   WRK-SRYSURYO
                       MOVE   ZERO   TO   WRK-SRYSURYO2
                       MOVE   ZERO   TO   WRK-SRYSURYO3
                       MOVE   ZERO   TO   WRK-TANICD
                       MOVE   ZERO   TO   WRK-TANIMOJISU
                       MOVE   SPACE  TO   WRK-TANINAME
                       MOVE   ZERO   TO   FLG-HOUSIJI-COM
      *
                       MOVE   ZERO   TO   FLG-WRITE85
                       PERFORM   2201-RECE84-TENSOU-SEC
                       PERFORM   2201-RECE85-HENSYU-SEC
                       IF    FLG-WRITE85  =   1
                           PERFORM RECE84-WRITE-SEC
                       END-IF
                   ELSE
                       IF   ACCT-ZAIKAISU  NOT =    ZERO
                            IF   ACCT-SRYKBN  =  "11"  OR
                                                 "33"  OR  "99" OR
                                                 "90"  OR  "92" 
      *
                                 MOVE   ZERO   TO   WRK-TEN
                                                    WRK-MEISAI-CNT
                                                    WRK-SRYCDTOTAL
                                                    WRK-SURYOUTOTAL
                                                    WRK-YOUBUKASAN
                                                    WRK-RYOYOTEATE
      *
                                 MOVE   ZERO   TO   WRK-SRYSURYO
                                 MOVE   ZERO   TO   WRK-SRYSURYO2
                                 MOVE   ZERO   TO   WRK-SRYSURYO3
                                 MOVE   ZERO   TO   WRK-TANICD
                                 MOVE   ZERO   TO   WRK-TANIMOJISU
                                 MOVE   SPACE  TO   WRK-TANINAME
                                 MOVE   ZERO   TO   FLG-HOUSIJI-COM
      *
                                 MOVE   ZERO   TO   FLG-WRITE85
                                 PERFORM   2201-RECE84-TENSOU-SEC
                                 PERFORM   2201-RECE85-HENSYU-SEC
                                 IF    FLG-WRITE85  =   1
                                    PERFORM RECE84-WRITE-SEC
                                 END-IF
                            END-IF
                       END-IF
                   END-IF
                 END-IF
                 MOVE     "SRYACCT-KEY22"  TO   ORC-DBPATH
                 PERFORM   SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE   "SRYACCT-KEY22"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       220-RECE84-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       230-RECE84-HENSYU2-SEC                SECTION.
      *
           MOVE       ZERO       TO       WRK-TEN
                                          WRK-MEISAI-CNT
                                          WRK-SRYCDTOTAL
                                          WRK-SURYOUTOTAL
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE82-HOSPID         TO      NACCT-HOSPID
           MOVE    RECE82-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE82-PTID           TO      NACCT-PTID
           MOVE    RECE82-SRYYM          TO      NACCT-SRYYM
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
                   IF     NACCT-SRYCDTOTAL    =    099999916
                       CONTINUE
                   ELSE
                     PERFORM    VARYING   IDY   FROM   1   BY   1
                                      UNTIL    IDY    >    31
                       IF   NACCT-DAY(IDY)   NOT =  ZERO 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY1(IDY) 
                       END-IF
                     END-PERFORM
                   END-IF
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
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE82-HOSPID         TO      NACCT-HOSPID
           MOVE    RECE82-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE82-PTID           TO      NACCT-PTID
           MOVE    RECE82-SRYYM          TO      NACCT-SRYYM
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
                 IF     NACCT-ZAISKBKBN    =     "4"  OR  "6"
      *
                      INITIALIZE          WRK-DAY-TABLE2
                                          WRK-SKJ-TABLE
                                          WRK-SKJ-TABLE2
                      MOVE     ZERO       TO    CNT-DAY21
                                                CNT-DAY24
                      MOVE     ZERO       TO    FLG-DAY21
                                                FLG-DAY24
      *
                      PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                         IF  RECE82-HKNCOMBI     =  WRK-DAY5(IDY)
                            IF  (WRK-DAY1(IDY)   NOT  =  ZERO)
                            AND (NACCT-ZAITEN    NOT  =  ZERO)
                            AND (NACCT-ZAISKBKBN NOT  =  "4" )
                            AND (NACCT-DAY(IDY)  NOT  =  ZERO)
                               IF  WRK-DAY2(IDY) =  ZERO  OR  4
                                    MOVE   NACCT-DAY(IDY)  
                                           TO   WRK-DAY21(IDY) 
                                    ADD    1   TO   CNT-DAY21
                                    MOVE   1   TO   FLG-DAY21
                               END-IF
                            END-IF
      *                     *¿©»ö
                            IF  (WRK-DAY1(IDY)   NOT  =  ZERO)
                            AND (NACCT-ZAITEN         =  ZERO)
                            AND (NACCT-ZAISKBKBN      =  "4" )
                            AND (NACCT-DAY(IDY)  NOT  =  ZERO)
                               IF  WRK-DAY2(IDY) =  ZERO  OR  4
                                  MOVE   NACCT-DAY(IDY)  
                                            TO   WRK-DAY24(IDY) 
                                  PERFORM      2301-NYUINACCT-SYU-SEC
                                  ADD    1     TO   CNT-DAY24
                                  MOVE   1     TO   FLG-DAY24
                               END-IF
                            END-IF
                         END-IF
      *
                      END-PERFORM
      *
                      IF   FLG-DAY21    =   1
                         PERFORM   2301-RECE84-TENSOU2-SEC
                         PERFORM   2301-RECE85-HENSYU2-SEC
                         PERFORM   RECE84-WRITE-SEC
                      END-IF
      *
                      IF   FLG-DAY24    =   1
                         PERFORM    2301-NYUINACCT-HEN-SEC
                         PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                            UNTIL  SYUKEI-IDX3           >  31
                             OR    WRK-SKJ2-ZAITEN(SYUKEI-IDX3) = ZERO
      *
                             PERFORM   2301-RECE84-TENSOU2-SKJ-SEC
                             PERFORM   2301-RECE85-HENSYU2-SKJ-SEC
                             PERFORM   RECE84-WRITE-SEC
      *
                         END-PERFORM
                      END-IF
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
           .
       230-RECE84-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       240-RECE84-HENSYU3-SEC                SECTION.
      *
      *    ÅÀÅ©¤Î·×»»
      *
           MOVE    RECE82-HOSPID       TO  SPA-HOSPID
      *
              INITIALIZE                  ORCSTENTEKAREA
              MOVE    "2"                 TO  ORCSTEN-KBN
              MOVE    RECE82-PTID         TO  ORCSTEN-PTID
              MOVE    RECE82-SRYYM        TO  ORCSTEN-SRYYM
              MOVE    RECE82-HKNCOMBI     TO  ORCSTEN-HKNCOMBI
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
                       PERFORM   2401-RECE84-TENSOU3-SEC
      *
                       MOVE      100  TO   IDX5
                       PERFORM   2401-RECE85-TENSOU3-SEC
                       PERFORM   2401-RECE82-HENSYU3-SEC
                       PERFORM   RECE84-WRITE-SEC
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
                       PERFORM   2401-RECE84-TENSOU3-SEC
      *
                       MOVE      10   TO   IDX5
                       PERFORM   2401-RECE85-TENSOU3-SEC
                       PERFORM   2401-RECE82-HENSYU3-SEC
                       PERFORM   RECE84-WRITE-SEC
      *
                  END-PERFORM
      *
      *       END-IF
      *
           .
       240-RECE84-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       250-RECE84-HENSYU4-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-KOYOMI-YMD
                                           WRK-KOYOMI2-YMD
                                           FLG-KHN
                                           WRK-SROUNYUIN-SRYCDTOTAL
                                           WRK-SROUNYUIN-MEISAISU
                                           WRK-SROUNYUIN-SRYCD
      *
      *    ½é²óÆþ±¡Ç¯·îÆü¤«¤é£²½µ´ÖÌÜ¤ò»»Äê
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    RECE82-NYUINYMD     TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    14                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      *
           MOVE    LNK-DAY6-KOYOMI     TO  WRK-KOYOMI-YMD
      *
      *
           INITIALIZE                      ORCSROUNYUIN-AREA
           MOVE    RECE82-HOSPID       TO  LNK-SROUNYUIN-HOSPID
           MOVE    RECE82-PTID         TO  LNK-SROUNYUIN-PTID
           MOVE    RECE82-SRYYM        TO  LNK-SROUNYUIN-SANTEIYM
           MOVE    RECE82-HKNCOMBI     TO  LNK-SROUNYUIN-HKNCOMBI
           MOVE    SPACE               TO  LNK-SROUNYUIN-RYAKUKBN
      *    Ï«ºÒ¾è¿ô¶èÊ¬¥»¥Ã¥È
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
      *
              MOVE    RECE82-SRYYM     TO  WRK-KOYOMI2-YYMM
              MOVE    IDY              TO  WRK-KOYOMI2-DD
              IF      WRK-KOYOMI2-YMD  <=  WRK-KOYOMI-YMD
                   MOVE    "1"    TO   LNK-SROUNYUIN-JOUSU-KBN(IDY)
              ELSE
                   MOVE    "2"    TO   LNK-SROUNYUIN-JOUSU-KBN(IDY)
              END-IF
           END-PERFORM
      *
           CALL    "ORCSROUNYUIN"      USING   MCPAREA
                                               ORCSROUNYUIN-AREA
      *
           IF   LNK-SROUNYUIN-RC   =   ZERO
      *
              PERFORM    VARYING   IDY   FROM   1   BY   1
                          UNTIL    IDY    >    31
      *
                IF   LNK-SROUNYUIN-NYUIN-FLG(IDY)    =  "1"
      *
                   MOVE     ZERO        TO    WRK-RENNUM
      *
                   IF   LNK-SROUNYUIN-GAIHAKU(IDY)  =  ZERO
      *
                     MOVE     1     TO    FLG-KHN
      *
                     PERFORM   2501-RECE84-TENSOU2-KHN-SEC
      *
                     MOVE     1     TO    SYUKEI-IDX
                     MOVE     5     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2501-RECE85-TENSOU2-KHN-SEC
                        PERFORM   RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE     6     TO    SYUKEI-IDX
                     MOVE    10     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2501-RECE85-TENSOU2-KHN-SEC
                        PERFORM   RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE    11     TO    SYUKEI-IDX
                     MOVE    15     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2501-RECE85-TENSOU2-KHN-SEC
                        PERFORM   RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE    16     TO    SYUKEI-IDX
                     MOVE    20     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2501-RECE85-TENSOU2-KHN-SEC
                        PERFORM   RECE85-WRITE-SEC
                     END-IF
      *
                     PERFORM   RECE84-WRITE-SEC
      *
                   ELSE
      *
                     MOVE     2     TO    FLG-KHN
      *
                     MOVE      LNK-SROUNYUIN-SRYCD(IDY 1)
                                        TO   WRK-SROUNYUIN-SRYCD
                     COMPUTE   WRK-SROUNYUIN-SRYCDTOTAL      =
                               LNK-SROUNYUIN-SRYCDTOTAL(IDY) -
                               WRK-SROUNYUIN-SRYCD
                     COMPUTE   WRK-SROUNYUIN-MEISAISU        =
                               LNK-SROUNYUIN-MEISAISU(IDY)   -
                               1
      *
                     PERFORM   2501-RECE84-TENSOU2-KHN-SEC
      *
                     MOVE     2     TO    SYUKEI-IDX
                     MOVE     6     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2501-RECE85-TENSOU2-KHN-SEC
                        PERFORM   RECE85-WRITE-SEC
                     END-IF
      *
                     PERFORM   RECE84-WRITE-SEC
      *
                   END-IF
      *
                END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       250-RECE84-HENSYU4-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î½¸·×
      **************************************************************
       2301-NYUINACCT-SYU-SEC           SECTION.
      *
           MOVE       ZERO                TO   IDY2
      *
           EVALUATE   WRK-DAY24(IDY)
              WHEN  1
                EVALUATE   WRK-SHOKUJIRYOYO
                  WHEN  1
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD(IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD(IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD(IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
                  WHEN  2
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000710"    TO   WRK-SKJ-SRYCD(IDY IDY2)
                     MOVE  "197000710"    TO   WRK-SRYCD
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
                END-EVALUATE
              WHEN  2
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD(IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD(IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ÆÃÊÌ¿©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000470"    TO   WRK-SKJ-SRYCD(IDY IDY2)
                     MOVE  "197000470"    TO   WRK-SRYCD
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD(IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
              WHEN  3
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD(IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD(IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD(IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              Ê£¿ô¸¥Î©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000670"    TO   WRK-SKJ-SRYCD(IDY IDY2)
                     MOVE  "197000670"    TO   WRK-SRYCD
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
              WHEN  4
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD(IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD(IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ÆÃÊÌ¿©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000470"    TO   WRK-SKJ-SRYCD(IDY IDY2)
                     MOVE  "197000470"    TO   WRK-SRYCD
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD(IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              Ê£¿ô¸¥Î©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000670"    TO   WRK-SKJ-SRYCD(IDY IDY2)
                     MOVE  "197000670"    TO   WRK-SRYCD
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23011-NYUINACCT-SYU-JYOSU-SEC
           END-EVALUATE
      *
           .
       2301-NYUINACCT-SYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î·×»»
      **************************************************************
       23011-NYUINACCT-SYU-KEISAN-SEC           SECTION.
      *
           COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
           IF      RECE82-HKNKBN    =   "4"
           AND     WRK-4001-SKJKBN  =   "2"
               COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  1.2
           END-IF
           ADD      WRK-KEISAN  TO   WRK-SKJ-ZAITEN(IDY)
      *
           .
       23011-NYUINACCT-SYU-KEISAN-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤ÎÏ«ºÒ¾è¿ô¥»¥Ã¥È
      **************************************************************
       23011-NYUINACCT-SYU-JYOSU-SEC           SECTION.
      *
           ADD    1             TO   IDY2
           MOVE  "101970010"    TO   WRK-SKJ-SRYCD(IDY IDY2)
           IF      RECE82-HKNKBN    =   "4"
           AND     WRK-4001-SKJKBN  =   "2"
      *       ¼«ÇåÀÕ¡Ê£±¡¥£²ÇÜ¡Ë
              ADD    1             TO   IDY2
      *       MOVE  "101970010"    TO   WRK-SKJ-SRYCD(IDY IDY2)
              MOVE  "099609901"    TO   WRK-SKJ-SRYCD(IDY IDY2)
           END-IF
      *
           .
       23011-NYUINACCT-SYU-JYOSU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤ÎÊÔ½¸
      **************************************************************
       2301-NYUINACCT-HEN-SEC           SECTION.
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
                        UNTIL ( SYUKEI-IDX3     >   15 )
                         OR   ( WRK-SKJ-SRYCD(SYUKEI-IDX SYUKEI-IDX3) 
                                                               =  ZERO)
      *
                        ADD   1  TO  WRK-SKJ2-MEISAISU(SYUKEI-IDX2)
                        ADD   WRK-SKJ-SRYCD(SYUKEI-IDX SYUKEI-IDX3)
                                 TO  WRK-SKJ2-SRYCDTOTAL(SYUKEI-IDX2)
      *
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
       2301-NYUINACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2201-RECE84-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPID        TO      RECE84-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           MOVE    ACCT-ZAINUM          TO      RECE84-ZAINUM
           MOVE    ACCT-SRYKBN          TO      RECE84-SRYKBN
           IF      ACCT-SRYKBN = "90" OR "92"
                 MOVE    "6"            TO      RECE84-ZAISKBKBN
           ELSE 
              IF   ACCT-SRYKBN = "97"
                 MOVE    "4"            TO      RECE84-ZAISKBKBN
              END-IF
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
           MOVE    ACCT-SRYKA           TO      RECE84-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE84-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE84-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE84-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE84-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE84-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE84-MEISAISU
           MOVE    ACCT-ZAIKAISU        TO      RECE84-ZAIKAISU
           MOVE    ACCT-SYUTEN1         TO      RECE84-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE84-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE84-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE84-KIZAITEN
           IF   RECE82-HKNKBN     =  "4"
              IF  ACCT-SRYKBN     =  "70"
                  COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                            -  RECE84-SYUTEN2
                  COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                            +  RECE84-SYUTEN2
              END-IF
           END-IF
           MOVE    ACCT-DRCD            TO      RECE84-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE84-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX3   FROM    1    BY   1
                       UNTIL     IDX3   >       4
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE84-KAISU (IDX3 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (ACCT-DAY(1 IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
           MOVE      ZERO     TO    FLG-OKIKAE
           IF   ( RECE82-AGE       >=  75        )
           OR   ( RECE82-BIRTHDAY  <  "19321001" )
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL    ( IDX2        >    31 )
                             OR     ( FLG-OKIKAE  =     1 )
                  IF  ( RECE82-BYOTOSYUBETU(IDX2) =  "02" OR "03" )
                  AND ( RECE84-KAISU(1 IDX2)  NOT =  ZERO         )
                        MOVE   1   TO   FLG-OKIKAE
                  END-IF    
               END-PERFORM
           END-IF
      *
           .
       2201-RECE84-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2301-RECE84-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPID        TO      RECE84-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
      *
           MOVE    NACCT-ZAINUM         TO      RECE84-ZAINUM
      *
           MOVE    NACCT-SRYKBN         TO      RECE84-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    NACCT-ZAITEN         TO      RECE84-ZAITEN
           MOVE    NACCT-SRYCDTOTAL     TO      RECE84-SRYCDTOTAL
           MOVE    NACCT-SURYOUTOTAL    TO      RECE84-SURYOUTOTAL
           MOVE    NACCT-MEISAISU       TO      RECE84-MEISAISU
           MOVE    CNT-DAY21            TO      RECE84-ZAIKAISU
           MOVE    NACCT-SYUTEN1        TO      RECE84-SYUTEN1
           MOVE    NACCT-SYUTEN2        TO      RECE84-SYUTEN2
           MOVE    NACCT-YKZTEN         TO      RECE84-YKZTEN
           MOVE    NACCT-KIZAITEN       TO      RECE84-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-DAY21(IDX2)  TO
                                            RECE84-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)   AND
                          (WRK-DAY21(IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       2301-RECE84-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2301-RECE84-TENSOU2-SKJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPID        TO      RECE84-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    NACCT-SRYKBN         TO      RECE84-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    WRK-SKJ2-ZAITEN(SYUKEI-IDX3) 
                                        TO      RECE84-ZAITEN
           MOVE    WRK-SKJ2-SRYCDTOTAL(SYUKEI-IDX3)
                                        TO      RECE84-SRYCDTOTAL
           MOVE    WRK-SKJ2-MEISAISU(SYUKEI-IDX3)
                                        TO      RECE84-SURYOUTOTAL
           MOVE    WRK-SKJ2-MEISAISU(SYUKEI-IDX3)
                                        TO      RECE84-MEISAISU
           MOVE    WRK-SKJ2-ZAIKAISU(SYUKEI-IDX3)
                                        TO      RECE84-ZAIKAISU
           MOVE    WRK-SKJ2-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-SKJ2-KAISU(SYUKEI-IDX3 IDX2)             
                                        TO   RECE84-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)
                   AND    (WRK-SKJ2-KAISU(SYUKEI-IDX3 IDX2)             
                                                     NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       2301-RECE84-TENSOU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2401-RECE84-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPID        TO      RECE84-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           IF    FLG-SIKIBETU  =  1
             COMPUTE WRK-ZAINUM    =  99999700  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99999740  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    "33"                 TO      RECE84-SRYKBN
           MOVE    SPACE                TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    "00"                 TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE84-ZAITEN
           ELSE
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)  
                                        TO      RECE84-ZAITEN
           END-IF
           MOVE    ZERO                 TO      RECE84-SRYCDTOTAL
           MOVE    ZERO                 TO      RECE84-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE84-MEISAISU
           IF    FLG-SIKIBETU  =  1
             MOVE  1                    TO      RECE84-ZAIKAISU
           ELSE
             MOVE  WRK-ORCSTEN-ZAIKAISU TO      RECE84-ZAIKAISU
           END-IF
           MOVE    ZERO                 TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE84-YKZTEN
             MOVE  ZERO                 TO      RECE84-KIZAITEN
           ELSE
             MOVE  ZERO                 TO      RECE84-YKZTEN
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)
                                        TO      RECE84-KIZAITEN
           END-IF
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           IF    FLG-SIKIBETU  =  1
             MOVE    1                  TO      RECE84-KAISU (1 IDY)
             MOVE    IDY                TO      WK-TAIHI-DAY
           ELSE
             MOVE        ZERO           TO      WK-TAIHI-DAY
             MOVE        ZERO           TO      WK-TAIHI-FLG
             PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-ORCSTEN-DAY(IDX2)  TO
                                            RECE84-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (WRK-ORCSTEN-DAY(IDX2)  NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
             END-PERFORM
           END-IF
           .
       2401-RECE84-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2501-RECE84-TENSOU2-KHN-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPID        TO      RECE84-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
      *
           COMPUTE WRK-ZAINUM  =  99999800  +  IDY
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
      *
           MOVE    LNK-SROUNYUIN-SRYKBN(IDY)
                                        TO      RECE84-SRYKBN
           IF      FLG-KHN  =  1
              MOVE    1                 TO      RECE84-ZAISKBKBN
           ELSE
              MOVE    2                 TO      RECE84-ZAISKBKBN
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    "00"                 TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    LNK-SROUNYUIN-ZAITEN(IDY)
                                        TO      RECE84-ZAITEN
           IF      FLG-KHN  =  1
              MOVE    LNK-SROUNYUIN-SRYCDTOTAL(IDY)
                                        TO      RECE84-SRYCDTOTAL
              MOVE    LNK-SROUNYUIN-MEISAISU(IDY)
                                        TO      RECE84-SURYOUTOTAL
              MOVE    LNK-SROUNYUIN-MEISAISU(IDY)
                                        TO      RECE84-MEISAISU
           ELSE
              MOVE    WRK-SROUNYUIN-SRYCDTOTAL
                                        TO      RECE84-SRYCDTOTAL
              MOVE    WRK-SROUNYUIN-MEISAISU
                                        TO      RECE84-SURYOUTOTAL
              MOVE    WRK-SROUNYUIN-MEISAISU
                                        TO      RECE84-MEISAISU
           END-IF
           MOVE    1                    TO      RECE84-ZAIKAISU
           MOVE    LNK-SROUNYUIN-ZAITEN(IDY)
                                        TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           MOVE    1                    TO      RECE84-KAISU (1 IDY)
           MOVE    IDY                  TO      WK-TAIHI-DAY
           .
       2501-RECE84-TENSOU2-KHN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2201-RECE85-HENSYU-SEC               SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
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
                   PERFORM   22011-RECE85-TENSOU-SEC
                   IF   FLG-TENTEKI  =  ZERO
                     PERFORM 22011-RECE82-HENSYU-SEC
                     PERFORM RECE85-WRITE-SEC
                     MOVE  1            TO      FLG-WRITE85
                   END-IF
                   IF   FLG-ZANHAIKI  =  1
                      PERFORM  RECE85-REWRITE-SEC
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
       2201-RECE85-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2301-RECE85-HENSYU2-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID       TO   NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
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
                   PERFORM   23011-RECE85-TENSOU2-SEC
                   PERFORM   RECE85-WRITE-SEC
                   MOVE          "NYUINACT-KEY9"  TO   ORC-DBPATH
                   PERFORM        NYUINACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE   "NYUINACT-KEY9"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       2301-RECE85-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2301-RECE85-HENSYU2-SKJ-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID       TO   NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
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
               MOVE      1      TO     IDX7
               MOVE      1      TO     IDX8
               MOVE      5      TO     IDX9
               PERFORM   23011-RECE85-TENSOU2-SKJ-SEC
               PERFORM   RECE85-WRITE-SEC
               IF   WRK-SKJ2-SRYCD(SYUKEI-IDX3 6)  NOT =   ZERO
                  MOVE      2      TO     IDX7
                  MOVE      6      TO     IDX8
                  MOVE     10      TO     IDX9
                  PERFORM   23011-RECE85-TENSOU2-SKJ-SEC
                  PERFORM   RECE85-WRITE-SEC
               END-IF
               IF   WRK-SKJ2-SRYCD(SYUKEI-IDX3 11)  NOT =   ZERO
                  MOVE      3      TO     IDX7
                  MOVE     11      TO     IDX8
                  MOVE     15      TO     IDX9
                  PERFORM   23011-RECE85-TENSOU2-SKJ-SEC
                  PERFORM   RECE85-WRITE-SEC
               END-IF
           END-IF
      *
           MOVE   "NYUINACT-KEY9"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       2301-RECE85-HENSYU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       22011-RECE85-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPID        TO      RECE85-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
           MOVE    SRY-ZAINUM           TO      RECE85-ZAINUM
           MOVE    SRY-RENNUM           TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    SRY-SRYKA            TO      RECE85-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE85-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE85-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE85-JIHIMONEY
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
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»¡Ê£±¡Ë
                  WHEN  "160170170"
                      MOVE  "160710070"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  539900
      *           ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»¡Ê£²¡Ë
                  WHEN  "160161610"
                      MOVE  "160709310"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  547700
      *           À¸²½³ØÅª¸¡ºº¡Ê£±¡ËÈ½ÃÇÎÁ
                  WHEN  "160061910"
                      MOVE  "160703710"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           À¸²½³ØÅª¸¡ºº¡Ê£²¡ËÈ½ÃÇÎÁ
                  WHEN  "160062010"
                      MOVE  "160703810"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           Ç¢¡¦ÊµÊØÅù¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160061710"
                      MOVE  "160703510"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ÈùÀ¸Êª³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160062210"
                      MOVE  "160704010"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ÌÈ±Ö³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160062110"
                      MOVE  "160703910"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
                END-EVALUATE
             END-IF
      *
      *
             IF     (SRY-SRYCD(IDX2)(1:1)  =  "S"  )
             OR     (SRY-SRYCD(IDX2)(1:3)  =  "001")
                  COMPUTE   RECE84-MEISAISU  =  RECE84-MEISAISU
                                             -  1
             END-IF
             IF      SRY-SRYCD(IDX2)(1:3)  =  "001"
                  MOVE    SRY-SRYCD(IDX2)   TO    WRK-SRYCD
                  COMPUTE   RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                               -  WRK-SRYCD
                  COMPUTE   RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                               -  1
             END-IF
      *
      *
             IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                  "6" OR  "7" OR
                                                  "8"
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
               MOVE    SRY-SRYCD(IDX2)    TO    RECE85-SRYCD(IDX4)
      *
               IF      SRY-SRYCD(IDX2)    =     CONS-BYOITAIYO
                  EVALUATE  RECE82-HOSPSBT
                     WHEN     "1"
                          MOVE     "3"    TO    RECE82-HOSPSBT
                     WHEN     "2"
                          MOVE     "4"    TO    RECE82-HOSPSBT
                  END-EVALUATE
               END-IF
      *
               IF     SRY-SRYCD(IDX2)  IS  NUMERIC
                 IF  SRY-SRYCD(IDX2)(1:3) NOT = "001"
                   MOVE    SRY-SRYCD(IDX2)  TO  WRK-SRYCD
                   COMPUTE  RECE84-RUIKEITOTAL  =
                            RECE84-RUIKEITOTAL  +
                           (WRK-SRYCD  *  SRY-SRYSURYO(IDX2))
                 END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE85-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE85-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE85-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE85-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE85-INPUTNUM(IDX4)
               MOVE    SRY-SRYCD(IDX2)    TO    WRK-SRYCD
               MOVE    RECE85-SRYYM       TO    WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY       TO    WK-TAIHI-SRYD
               PERFORM    TENSU-TBL-SEC
      *
               IF   RECE82-HKNKBN    NOT  =  "4"                        
                  IF      TNS-SRYKBN         =     "80"
                      IF  TNS-NYUTENTTLKBN   =     130
                          MOVE   "13"        TO    RECE84-SRYKBN
                                                   RECE85-SRYKBN
                      END-IF
                  END-IF
               ELSE                                                     
                  IF      SRY-SRYCD(IDX2)    =     CONS-YOUBUKASAN
                    COMPUTE  WRK-YOUBUKASAN   =  WRK-YOUBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                  END-IF
      *           ¼«ÇåÀÕ¡¦ÎÅÍÜÃ´Åö¼êÅöÍÑÆÃÎã½èÍý
      *           ¡Ê¼«ÇåÀÕ¤Î¾ì¹ç¡¢£±£°ÅÀ¤ò£±£²£°±ß¤ËÆÉ¤ßÂØ¤¨¤ë¡Ë
      *           ¡ÊÎÅÍÜÃ´Åö¼êÅö¡§£±·î£±²ó¤Î»»Äê¡Ë
                  IF      SRY-SRYCD(IDX2)    =     CONS-RYOYOTEATE
                    MOVE     120     TO     WRK-RYOYOTEATE2
                    COMPUTE  WRK-RYOYOTEATE   =  WRK-RYOYOTEATE
                                              +  WRK-RYOYOTEATE2
                  END-IF
      *
                  IF ( ACCT-SRYKBN          =  "80"               ) AND
                     ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592" )
                                MOVE   "95"   TO    RECE84-SRYKBN       
                                                    RECE85-SRYKBN       
                  ELSE
                   IF ( ACCT-SRYKBN          =  "80"              ) AND 
                      ( SRY-SRYCD(IDX2)(1:5) =  "09593" OR "09594")     
                                MOVE   "40"   TO    RECE84-SRYKBN       
                                                    RECE85-SRYKBN       
                   ELSE                                                 
                     IF      TNS-SRYKBN         =   "80"                
                        IF    TNS-NYUTENTTLKBN   =  130           
                            MOVE   "13"   TO  RECE84-SRYKBN       
                                              RECE85-SRYKBN       
                        ELSE                                      
                            MOVE   "40"   TO  RECE84-SRYKBN       
                                              RECE85-SRYKBN       
                        END-IF                                    
                     END-IF
                     IF      TNS-SRYKBN         =   "14"                
                            MOVE   "80"   TO  RECE84-SRYKBN       
                                              RECE85-SRYKBN       
                     END-IF
                   END-IF                                               
                  END-IF                                                
               END-IF                                                   
      *
               PERFORM  220111-RECE85-TENSOU-COMCNT-HEN-SEC
      *
               IF  SRY-SRYCD(IDX2)        =     CONS-ZANHAIKI           
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.999                   
                  COMPUTE RECE85-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE85-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE85-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE85-TANINAME(IDX4)   
                    IF    RECE85-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE85-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE85-SRYSURYO(IDX4 - 1) 
                    END-IF
                  END-IF                                                
               ELSE                                                     
                  MOVE    SRY-SRYSURYO(IDX2)                            
                                          TO    WRK-SRYSURYO            
                  MOVE    TNS-TANICD      TO    RECE85-TANICD(IDX4)
                                                WRK-TANICD              
                  MOVE    TNS-TANIMOJISU  TO    RECE85-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE85-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
               IF ( ACCT-SRYKBN          =  "80"              ) AND     
                  ( SRY-SRYCD(IDX2)(1:5) =  "09593" OR "09594")         
                    MOVE  1               TO  RECE85-TENSIKIBETU(IDX4)  
                    MOVE  TNS-TEN         TO  RECE85-TEN(IDX4)          
               ELSE                                                     
                 IF   RECE82-HKNKBN     =  "4"
                 AND  SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
                    MOVE  1               TO  RECE85-TENSIKIBETU(IDX4)  
                    MOVE  WRK-RYOYOTEATE2 TO  RECE85-TEN(IDX4)
                 ELSE
                  IF      TNS-TENSIKIBETU    =     1   OR   3   OR  8
                    MOVE  TNS-TENSIKIBETU TO  RECE85-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN         TO  RECE85-TEN(IDX4)
                  END-IF
                 END-IF
               END-IF                                                   
               MOVE    TNS-NYUTENTTLKBN   TO    RECE85-TENTTLKBN(IDX4)
               IF      SRY-SRYCD    (IDX2)(1:1)     =   "8"
                  MOVE ZERO               TO    RECE85-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE85-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO    RECE85-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE85-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE85-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE85-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE85-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE85-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE85-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE85-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE85-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE85-KOUHYOJYUNNUM(IDX4)
               IF      SRY-SRYCD    (IDX2)(1:1)     =   "8"
               AND     SRY-INPUTNUM (IDX2)      NOT =   ZERO
                   MOVE    SRY-HOSPID           TO  PTCOM-HOSPID
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT   TO    RECE85-NAME(IDX4)
      *
                   MOVE   "1"                  TO    RECE84-COMCDFLG
               ELSE
                   MOVE    TNS-NAME            TO    RECE85-NAME(IDX4)
               END-IF
      *
               IF  RECE85-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   200   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE85-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE85-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF (  SRY-SRYCD(IDX2) = CONS-ROUSAI1  OR
                                       CONS-ROUSAI2  OR
                                       CONS-ROUSAI3  OR
                                       CONS-ROUSAI4  OR
                                       CONS-ROUSAI5  OR
                                       CONS-ROUSAI6  OR
                                       CONS-ROUSAI7  OR
                                       CONS-ROUSAI8  OR
                                       CONS-ROUSAI9  OR
                                       CONS-ROUSAI10 OR
                                       CONS-ROUSAI11 OR
                                       CONS-ROUSAI12 OR
                                       CONS-ROUSAI13 OR
                                       CONS-ROUSAI14 OR
                                       CONS-ROUSAI15 OR
                                       CONS-ROUSAI16 OR
                                       CONS-ROUSAI17 OR
                                       CONS-ROUSAI18 OR
                                       CONS-ROUSAI19 OR
                                       CONS-ROUSAI20   )                
               OR (( ACCT-SRYKBN          =  "80"     )   AND           
                   ( SRY-SRYCD(IDX2)(1:5) =  "09593"
                                         OR  "09594"  ))                
      *
                     ADD      1          TO  WRK-MEISAI-CNT
                     COMPUTE  WRK-TEN    =   WRK-TEN   +
                                           ( TNS-TEN   /  10 ) 
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *
                     MOVE     "2"        TO    RECE85-SIKFLG(IDX4)
      *
               ELSE
      *
                  IF   RECE82-HKNKBN     =  "4"
                  AND  SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
      *
                     ADD      1          TO  WRK-MEISAI-CNT
                     COMPUTE  WRK-TEN    =   WRK-TEN   +
                                             TNS-TEN
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *
                     MOVE     "2"        TO  RECE85-SIKFLG(IDX4)
      *
                  ELSE
      *
                      IF  ( SRY-SRYCD(IDX2) = CONS-ROUCOM1  OR
                                              CONS-ROUCOM2  OR
                                              CONS-ROUCOM3  OR
                                              CONS-ROUCOM4  OR
                                              CONS-ROUCOM5  OR
                                              CONS-ROUCOM6  OR
                                              CONS-ROUCOM7      )
                      OR  ( SRY-SRYCD(1)    = CONS-ROUSAI2      )
                      OR (( ACCT-SRYKBN     = "11"              ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ) AND
                          ( SRY-SRYCD(IDX2) NOT = "830000020"   ))
      *
      **>             OR (( SRY-SRYCD(1)    = CONS-ROUSAI1      ) AND
      **>                 ( SRY-SRYCD(IDX2)(1:1)     =   "8"    ))
      *
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE85-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF    RECE84-COMCDFLG       =   "1" 
                         AND   SRY-SRYCD(IDX2)(1:1)  =   "8" 
                            MOVE  "2"  TO    RECE84-COMCDFLG
                         END-IF
                      ELSE
                         MOVE  "1"     TO    RECE85-SIKFLG(IDX4)
                      END-IF
      *
                  END-IF
      *
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
                     IF      TNS-TEN         =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM 220111-RECE85-TENSOU-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE85-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE85-NAME(IDX4)(IDW:)   
                       COMPUTE RECE85-YUKOKETA(IDX4)
                                               =   RECE85-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF
              END-IF
             END-IF  
           END-PERFORM
      *
           IF    IDX4   =   ZERO
               MOVE    1     TO    FLG-TENTEKI
           END-IF
      *
           .
       22011-RECE85-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤ÎÃ±²ÁÊÔ½¸½èÍý
      *****************************************************************
       220111-RECE85-TENSOU-TANKA-HEN-SEC     SECTION.
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
       220111-RECE85-TENSOU-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       220111-RECE85-TENSOU-COMCNT-HEN-SEC               SECTION.
      *
           IF     (RECE82-HKNKBN    NOT  =  "4"    AND
                   RECE84-SRYKBN         =  "80"       )
           OR     (RECE82-HKNKBN         =  "4"    AND
                   RECE84-SRYKBN         =  "40"       )
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
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     CONS-YAKUKAN-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-YAKUKAN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-NYUEIYO-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-NYUEIYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUMON-COMMENT1  OR
                                            CONS-HOUMON-COMMENT2
                   MOVE   "1"   TO    RECE84-FLG-HOUMON-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-TAIIN-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-TAIIN-COMMENT
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI-COMMENT
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI2-COMMENT
                   MOVE  SPACE  TO    RECE84-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "50"
              IF   RECE84-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SHUJUTU-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "70"
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
                   MOVE   "1"   TO    RECE84-FLG-CTMRI-COMMENT
              END-IF
           END-IF
      *
           IF     (RECE82-HKNKBN    NOT  =  "4"    AND
                   RECE84-SRYKBN         =  "80"       )
           OR     (RECE82-HKNKBN         =  "4"    AND
                   RECE84-SRYKBN         =  "40"       )
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSEI-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-HOUSEI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "90"  OR  "92"
              IF   SRY-SRYCD(IDX2)    =     CONS-KAKURI-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-KAKURI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-CHIRYO-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-CHIRYO-COMMENT
              END-IF
           END-IF
      *
           .
       220111-RECE85-TENSOU-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       23011-RECE85-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPID        TO      RECE85-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
      *
           MOVE    NSRY-ZAINUM          TO      RECE85-ZAINUM
      *
           MOVE    NSRY-RENNUM          TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE85-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE85-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE85-SRYSYUKBN
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (NSRY-SRYCD(IDX2)  =   SPACE)
             IF      NSRY-SRYCD(IDX2)(1:1)    =   "0"  OR  "1" OR  
                                                  "8"
               ADD     1                   TO  IDX4    
               MOVE    NSRY-SRYCD(IDX2)    TO  RECE85-SRYCD(IDX4)
               MOVE    NSRY-SRYSURYO(IDX2) TO  RECE85-SRYSURYO(IDX4)
               MOVE    NSRY-SRYKAISU(IDX2) TO  RECE85-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
               MOVE    NSRY-AUTOKBN(IDX2)  TO  RECE85-AUTOKBN(IDX4)
               MOVE    NSRY-INPUTNUM(IDX2) TO  RECE85-INPUTNUM(IDX4)
               MOVE    NSRY-SRYCD(IDX2)    TO  WRK-SRYCD
               MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM    TENSU-TBL-SEC
               MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
               MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
               MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
               IF      TNS-TENSIKIBETU    =     1   OR   3  OR  8
                   MOVE  TNS-TENSIKIBETU   TO  RECE85-TENSIKIBETU(IDX4)
                   MOVE  TNS-TEN           TO  RECE85-TEN(IDX4)
               END-IF
               MOVE    TNS-NYUTENTTLKBN   TO    RECE85-TENTTLKBN(IDX4)
               MOVE    TNS-YUKOKETA       TO    RECE85-YUKOKETA(IDX4)
               MOVE    TNS-DATAKBN        TO    RECE85-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE85-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE85-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE85-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE85-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE85-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE85-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE85-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE85-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE85-KOUHYOJYUNNUM(IDX4)
               IF      NSRY-SRYCD    (IDX2)(1:1)     =   "8"
               AND     NSRY-INPUTNUM (IDX2)      NOT =   ZERO
                   MOVE    NSRY-HOSPID           TO  PTCOM-HOSPID
                   MOVE    NSRY-PTID             TO  PTCOM-PTID
                   MOVE    NSRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    NSRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    NSRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT     TO  RECE85-NAME(IDX4)
               ELSE
                   MOVE    TNS-NAME              TO  RECE85-NAME(IDX4)
               END-IF
               MOVE     "1"                      TO  RECE85-SIKFLG(IDX4)
      *
             END-IF  
           END-PERFORM
      *
           .
       23011-RECE85-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       23011-RECE85-TENSOU2-SKJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPID        TO      RECE85-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID      TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE85-ZAINUM
           MOVE    IDX7                 TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE85-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE85-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE85-SRYSYUKBN
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    IDX8    BY   1
                  UNTIL  (IDX2      >    IDX9)    
                    OR   (WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)  =   ZERO)
      *
                ADD     1                    TO    IDX4  
                MOVE    WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)
                                             TO  RECE85-SRYCD(IDX4)
                MOVE    1                    TO  RECE85-SRYSURYO(IDX4)
                MOVE    ZERO                 TO  RECE85-SRYKAISU(IDX4)
                MOVE    SPACE                TO  RECE85-MEISKYFLG(IDX4)
                MOVE    SPACE                TO  RECE85-AUTOKBN(IDX4)
                MOVE    ZERO                 TO  RECE85-INPUTNUM(IDX4)
                MOVE    WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)
                                             TO  WRK-SRYCD
                MOVE    RECE85-SRYYM         TO  WK-TAIHI-SRYYM
                MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
                PERFORM    TENSU-TBL-SEC
                MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
                MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
                MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
                IF      TNS-TENSIKIBETU    =     1   OR   3  OR  8
                    MOVE  TNS-TENSIKIBETU   TO  RECE85-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN           TO  RECE85-TEN(IDX4)
                END-IF
                MOVE    TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
                MOVE    TNS-YUKOKETA        TO  RECE85-YUKOKETA(IDX4)
                MOVE    TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
                MOVE    TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
                MOVE    TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
                MOVE    TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
                MOVE    TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
                MOVE    TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
                MOVE    TNS-CDKBN-KBNNUM  
                                      TO  RECE85-CDKBN-KBNNUM(IDX4)
                MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
                MOVE    TNS-CDKBN-KOUBAN
                                      TO  RECE85-CDKBN-KOUBAN(IDX4)
                MOVE    TNS-KOUHYOJYUNNUM  
                                      TO  RECE85-KOUHYOJYUNNUM(IDX4)
                MOVE    TNS-NAME            TO  RECE85-NAME(IDX4)
                MOVE     "1"                TO  RECE85-SIKFLG(IDX4)
      *
           END-PERFORM
      *
           .
       23011-RECE85-TENSOU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2401-RECE85-TENSOU3-SEC                SECTION.
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
              MOVE    SPACE                TO   RECE85-REC
              INITIALIZE                        RECE85-REC
              MOVE    RECE82-HOSPID        TO   RECE85-HOSPID
              MOVE    RECE82-SRYYM         TO   RECE85-SRYYM
              MOVE    RECE82-NYUGAIKBN     TO   RECE85-NYUGAIKBN
              MOVE    RECE82-PTID          TO   RECE85-PTID
              MOVE    RECE82-RECEKA        TO   RECE85-RECEKA
              MOVE    RECE82-HKNID         TO   RECE85-HKNID
              MOVE    RECE82-TEKSTYMD      TO   RECE85-TEKSTYMD
              IF    FLG-SIKIBETU  =  1
                COMPUTE WRK-ZAINUM    =  99999700  +  IDY
              ELSE
                COMPUTE WRK-ZAINUM    =  99999740  +  IDY
              END-IF
              MOVE    WRK-ZAINUM           TO   RECE85-ZAINUM
              ADD     1                    TO   WRK-RENNUM
              MOVE    WRK-RENNUM           TO   RECE85-RENNUM
              MOVE    RECE82-PTNUM         TO   RECE85-PTNUM
      *
              MOVE    "00"                 TO   RECE85-SRYKA
              MOVE    "33"                 TO   RECE85-SRYKBN
              MOVE    "330"                TO   RECE85-SRYSYUKBN
              MOVE    ZERO                 TO   RECE85-JIHIMONEY
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
                                            TO  RECE85-SRYCD(IDX4)
                  MOVE  WRK-ORCSTEN-SURYO(IDX2) 
                                            TO  RECE85-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE85-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE85-MEISKYFLG(IDX4)
                  MOVE  SPACE               TO  RECE85-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE85-INPUTNUM(IDX4)
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  WRK-SRYCD
                  MOVE  RECE85-SRYYM        TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM    TENSU-TBL-SEC
                  IF  WRK-ORCSTEN-SRYCD(IDX2)  =  CONS-ZANHAIKI
                      MOVE   WRK-TANICD     TO  RECE85-TANICD(IDX4)
                      MOVE   WRK-TANIMOJISU TO  RECE85-TANIMOJISU(IDX4)
                      MOVE   WRK-TANINAME   TO  RECE85-TANINAME(IDX4)
                  ELSE
                      MOVE  TNS-TANICD      TO  RECE85-TANICD(IDX4)
                                                WRK-TANICD
                      MOVE  TNS-TANIMOJISU  TO  RECE85-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU
                      MOVE  TNS-TANINAME    TO  RECE85-TANINAME(IDX4)
                                                WRK-TANINAME
                  END-IF
                  IF    TNS-TENSIKIBETU    =    1   OR   3   OR  8
                    MOVE  TNS-TENSIKIBETU   TO  RECE85-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN           TO  RECE85-TEN(IDX4)
                  END-IF
                  MOVE  TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
                  MOVE  TNS-YUKOKETA        TO  RECE85-YUKOKETA(IDX4)
                  MOVE  TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
                  MOVE  TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
                  MOVE  TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
                  MOVE  TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
                  MOVE  TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
                  MOVE  TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM    
                                      TO  RECE85-CDKBN-KBNNUM(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
                  MOVE  TNS-CDKBN-KOUBAN
                                      TO  RECE85-CDKBN-KOUBAN(IDX4)
                  MOVE  TNS-KOUHYOJYUNNUM  
                                      TO  RECE85-KOUHYOJYUNNUM(IDX4)
                  MOVE  TNS-NAME            TO  RECE85-NAME(IDX4)
                  MOVE     "1"              TO  RECE85-SIKFLG(IDX4)
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2)  TO  WRK-SRYCD
                  COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                              +  WRK-SRYCD
                  COMPUTE  RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                              +  WRK-ORCSTEN-SURYO(IDX2)
                  COMPUTE  RECE84-MEISAISU    =  RECE84-MEISAISU
                                              +  1
      *
                END-IF  
      *
                ADD        1                TO  IDX2
      *
              END-PERFORM
      *
              PERFORM   RECE85-WRITE-SEC
      *
           END-PERFORM
      *
           .
       2401-RECE85-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2501-RECE85-TENSOU2-KHN-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPID        TO      RECE85-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
      *
           COMPUTE WRK-ZAINUM   =   99999800   +   IDY
           MOVE    WRK-ZAINUM           TO      RECE85-ZAINUM
      *
           ADD     1                    TO      WRK-RENNUM
           MOVE    WRK-RENNUM           TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    "00"                 TO      RECE85-SRYKA
           MOVE    LNK-SROUNYUIN-SRYKBN(IDY)
                                        TO      RECE85-SRYKBN
           IF      LNK-SROUNYUIN-SRYKBN(IDY) =  "90"
              MOVE    "900"             TO      RECE85-SRYSYUKBN
           ELSE
              MOVE    "920"             TO      RECE85-SRYSYUKBN
           END-IF 
           MOVE    ZERO                 TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM   SYUKEI-IDX   BY   1
               UNTIL (IDX2      >    SYUKEI-IDX2)
                OR   (LNK-SROUNYUIN-SRYCD(IDY IDX2)  =   SPACE)
      *
               ADD     1                   TO  IDX4    
               MOVE    LNK-SROUNYUIN-SRYCD(IDY IDX2)
                                           TO  RECE85-SRYCD(IDX4)
               MOVE    1                   TO  RECE85-SRYSURYO(IDX4)
               MOVE    ZERO                TO  RECE85-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
               MOVE    SPACE               TO  RECE85-AUTOKBN(IDX4)
               MOVE    ZERO                TO  RECE85-INPUTNUM(IDX4)
               MOVE    LNK-SROUNYUIN-SRYCD(IDY IDX2)
                                           TO  WRK-SRYCD
               MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM    TENSU-TBL-SEC
               MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
               MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
               MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
               IF      TNS-TENSIKIBETU    =     1   OR   3  OR  8
                   MOVE  TNS-TENSIKIBETU   TO  RECE85-TENSIKIBETU(IDX4)
                   MOVE  TNS-TEN           TO  RECE85-TEN(IDX4)
               END-IF
               MOVE    TNS-NYUTENTTLKBN   TO    RECE85-TENTTLKBN(IDX4)
               MOVE    TNS-YUKOKETA       TO    RECE85-YUKOKETA(IDX4)
               MOVE    TNS-DATAKBN        TO    RECE85-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE85-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE85-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE85-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE85-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE85-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE85-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE85-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE85-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE85-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-NAME           TO    RECE85-NAME(IDX4)
               MOVE     "1"               TO    RECE85-SIKFLG(IDX4)
      *
           END-PERFORM
      *
           .
       2501-RECE85-TENSOU2-KHN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       22011-RECE82-HENSYU-SEC               SECTION.
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF   RECE82-HKNKBN    NOT  =  "4"
                         IF ( ACCT-SRYKBN          =  "80"    ) AND   
                            ( SRY-SRYCD(IDX3)(1:5) =  "09593"
                                                   OR "09594" )       
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN  
                                  COMPUTE   WK-YKZTEN =                 
                                                      ACCT-KIZAITEN  +  
                                                      ACCT-YKZTEN       
                         ELSE
                           IF    SRY-SRYKBN     =  "40"  OR  "50"  OR
                                                   "60"  OR  "70"  OR
                                                   "80"  OR  "54"
                                MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                                COMPUTE   WK-YKZTEN   =
                                                      ACCT-KIZAITEN  +
                                                      ACCT-YKZTEN
                           END-IF
                         END-IF
                       ELSE
                         IF ( ACCT-SRYKBN          =  "80"    )   AND
                            ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                      "09592" )
                               CONTINUE
                         ELSE
                           IF ( ACCT-SRYKBN          =  "80"    ) AND   
                              ( SRY-SRYCD(IDX3)(1:5) =  "09593"
                                                     OR "09594" )       
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN  
                                  COMPUTE   WK-YKZTEN =                 
                                                      ACCT-KIZAITEN  +  
                                                      ACCT-YKZTEN       
                           ELSE                                         
                             IF    SRY-SRYKBN   =  "30"  OR  "31"  OR     
                                                   "32"  OR  "33"  OR     
                                                   "40"  OR  "50"  OR     
                                                   "54"  OR  "60"  OR     
                                                   "70"  OR  "80"         
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN  
                                  COMPUTE   WK-YKZTEN =                 
                                                      ACCT-KIZAITEN  +  
                                                      ACCT-YKZTEN       
                                  IF  SRY-SRYKBN  =  "70"
                                     COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                         -  ACCT-SYUTEN2
                                     COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                         +  ACCT-SYUTEN2
                                  END-IF
                                  IF  WRK-YOUBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-YOUBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-YOUBUKASAN
                                  END-IF
                             END-IF
                           END-IF                                       
                         END-IF
                       END-IF
                       IF    SRY-SRYKBN     =  "14"
                           MOVE    ACCT-SYUTEN1   TO   WK-SYUTEN
                           MOVE    ACCT-KIZAITEN  TO   WK-KIZAITEN
                           MOVE    ACCT-YKZTEN    TO   WK-YKZTEN
                           IF   RECE82-HKNKBN   =  "4"
                             IF  WRK-YOUBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-YOUBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-YOUBUKASAN
                             END-IF
                           END-IF
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
                  IF   RECE82-HKNKBN   NOT  =   "4"                     
                        PERFORM   220111-RECE-LEFT-SET-SEC
                  ELSE                                                  
                        PERFORM   220111-RECE-LEFT-SET2-SEC
                  END-IF                                                
               END-IF
               IF    SRY-SRYKBN   =   "21"   OR   "22"   OR   "23"   OR
                                      "31"   OR   "32"   OR   "33"   OR
                                      "40"   OR   "50"   OR   "60"   OR
                                      "70"   OR   "54"   OR   "14"
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
           .                                           
      *
       22011-RECE82-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2401-RECE82-HENSYU3-SEC               SECTION.
      *
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
           IF   RECE82-HKNKBN   NOT  =   "4"
             ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(12)
      *
             COMPUTE   RECE82-TENSU(12)  =
                       RECE82-TENSU(12)  +
                       (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
           ELSE
             ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(12)
      *
             COMPUTE   RECE82-TENSU-YAK(13)  =
                       RECE82-TENSU-YAK(13)  +
                       (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
           END-IF
           .                                           
      *
       2401-RECE82-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       220111-RECE-LEFT-SET-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)  =
                                RECE82-TENSU(1)  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1    
                        COMPUTE    RECE82-KINGAKU2(1)  =
                                   RECE82-KINGAKU2(1)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI2
                        COMPUTE    RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF   SRY-SRYCD(IDX3) =   CONS-ROUSAI3  OR
                                            CONS-ROUSAI4  OR
                                            CONS-ROUSAI5  OR
                                            CONS-ROUSAI6  OR
                                            CONS-ROUSAI7  OR
                                            CONS-ROUSAI8  OR
                                            CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12 
                         COMPUTE    RECE82-KINGAKU2(2)  =
                                    RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF                   
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                   COMPUTE    RECE82-TENSU(3)  =
                              RECE82-TENSU(3)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)   =  "6" OR "S" OR "8")
                   OR    (SRY-SRYCD(IDX3) (1:3)   =  "001"            )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU(4)  =
                                      RECE82-TENSU(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU(5)  =
                             RECE82-TENSU(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU(6)  =
                             RECE82-TENSU(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(7)
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(8)
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(10)
                   COMPUTE   RECE82-TENSU(10)  =
                             RECE82-TENSU(10)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(11)
                   COMPUTE   RECE82-TENSU(11)  =
                             RECE82-TENSU(11)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(13)
                   END-IF
                   COMPUTE   RECE82-TENSU(13)  =
                             RECE82-TENSU(13)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(14)  =
                             RECE82-TENSU(14)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(15)
                   END-IF
                   COMPUTE   RECE82-TENSU(15)  =
                             RECE82-TENSU(15)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(16)  =
                             RECE82-TENSU(16)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(17)
                   END-IF
                   COMPUTE   RECE82-TENSU(17)  =
                             RECE82-TENSU(17)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(17)  =
                                RECE82-KAISU(17)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   CONS-SAIKETSU1  OR
                                              CONS-SAIKETSU2  OR
                                              CONS-SAIKETSU3
                         IF   RECE82-KAISU(17)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(17)  =
                                        RECE82-KAISU(17)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(19)
                   END-IF
                   COMPUTE   RECE82-TENSU(19)  =
                             RECE82-TENSU(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI13  OR
                                            CONS-ROUSAI14  OR
                                            CONS-ROUSAI15  OR
                                            CONS-ROUSAI16  OR
                                            CONS-ROUSAI17  OR
                                            CONS-ROUSAI18  OR
                                            CONS-ROUSAI19  OR
                                            CONS-ROUSAI20 )
                   OR ((ACCT-SRYKBN          =  "80"     )  AND
                       (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                         COMPUTE    RECE82-KINGAKU2(2)  =
                                    RECE82-KINGAKU2(2)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   CONS-ROUSAI13
                              WHEN   CONS-ROUSAI14
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI15
                              WHEN   CONS-ROUSAI16
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI17
                              WHEN   CONS-ROUSAI18
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI19
                              WHEN   CONS-ROUSAI20
                                  MOVE    4     TO   WRK-HEYA-NINZU
                         END-EVALUATE
                         EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                         END-EVALUATE
                         IF    RECE82-HEYA-NINZU   =   ZERO
                         AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                         ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                         END-IF
                   ELSE
                     IF ( ACCT-SRYKBN          =  "80"    )  AND         
                        ( SRY-SRYCD(IDX3)(1:5) =  "09593" )              
                         COMPUTE   RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                     ELSE                
                       IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(2)  =
                                   RECE82-TENSU(2)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                       ELSE  
                         COMPUTE   RECE82-TENSU(21)  =
                                   RECE82-TENSU(21)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       COMPUTE       RECE82-TENSU(22)  =
                                     RECE82-TENSU(22)  +
                                    (WK-YKZTEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
           END-EVALUATE
           .
      *
       220111-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       220111-RECE-LEFT-SET2-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)  =
                                RECE82-TENSU(1)  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2
                             MOVE  "1"     TO   RECE82-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1    
                        COMPUTE    RECE82-KINGAKU2(1)  =
                                   RECE82-KINGAKU2(1)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI2
                        COMPUTE    RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF   SRY-SRYCD(IDX3) =   CONS-ROUSAI3  OR
                                            CONS-ROUSAI4  OR
                                            CONS-ROUSAI5  OR
                                            CONS-ROUSAI6  OR
                                            CONS-ROUSAI7  OR
                                            CONS-ROUSAI8  OR
                                            CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12 
                         COMPUTE    RECE82-KINGAKU3(3)  =
                                    RECE82-KINGAKU3(3)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                   END-IF                   
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                   COMPUTE    RECE82-TENSU(22)  =
                              RECE82-TENSU(22)  +
                             (WK-SYUTEN     *   WK-ZAIKAISU)
                   COMPUTE    RECE82-TENSU-YAK(22)  =
                              RECE82-TENSU-YAK(22)  +
                             (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                             (WK-YKZTEN     *   WK-ZAIKAISU)
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)   =  "6" OR "S" OR "8")
                   OR    (SRY-SRYCD(IDX3) (1:3)   =  "001"            )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                           COMPUTE   RECE82-TENSU-YAK(4)  =
                                     RECE82-TENSU-YAK(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU-YAK(4)  =
                                     RECE82-TENSU-YAK(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU-YAK(4)  =
                                      RECE82-TENSU-YAK(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU-YAK(5)  =
                             RECE82-TENSU-YAK(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU-YAK(6)  =
                             RECE82-TENSU-YAK(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   IF    RECE82-KHNTEN(7)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(7)
                   ELSE
                      IF   RECE82-KHNTEN(7) NOT = TNS-TEN
                           MOVE   ZERO    TO    RECE82-KHNTEN(7)
                           MOVE   1       TO    FLG-KHNTEN7-JI
                      END-IF
                   END-IF
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(7)
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF    RECE82-KHNTEN(8)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(8)
                   ELSE
                      IF   RECE82-KHNTEN(8) NOT = TNS-TEN
                           MOVE   ZERO    TO    RECE82-KHNTEN(8)
                           MOVE   1       TO    FLG-KHNTEN8-JI
                      END-IF
                   END-IF
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(8)
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   IF    RECE82-KHNTEN(9)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(9)
                   ELSE
                      IF   RECE82-KHNTEN(9) NOT = TNS-TEN
                           MOVE   ZERO    TO    RECE82-KHNTEN(9)
                           MOVE   1       TO    FLG-KHNTEN9-JI
                      END-IF
                   END-IF
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(9)
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(14)
                   END-IF
                   COMPUTE   RECE82-TENSU(14)  =
                             RECE82-TENSU(14)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(15)  =
                             RECE82-TENSU-YAK(15)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(16)
                   END-IF
                   COMPUTE   RECE82-TENSU(16)  =
                             RECE82-TENSU(16)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(17)  =
                             RECE82-TENSU-YAK(17)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(18)
                   END-IF
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(18)  =
                                RECE82-KAISU(18)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   CONS-SAIKETSU1  OR
                                              CONS-SAIKETSU2  OR
                                              CONS-SAIKETSU3
                         IF   RECE82-KAISU(18)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(18)  =
                                        RECE82-KAISU(18)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU-YAK(19)  =
                             RECE82-TENSU-YAK(19)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(20)
                   END-IF
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(21)  =
                             RECE82-TENSU-YAK(21)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                  IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI13  OR
                                           CONS-ROUSAI14  OR
                                           CONS-ROUSAI15  OR
                                           CONS-ROUSAI16  OR
                                           CONS-ROUSAI17  OR
                                           CONS-ROUSAI18  OR
                                           CONS-ROUSAI19  OR
                                           CONS-ROUSAI20 )
                  OR ((ACCT-SRYKBN          =  "80"     )  AND
                      (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                       COMPUTE    RECE82-KINGAKU3(1)  =
                                  RECE82-KINGAKU3(1)  +
                                 (TNS-TEN   *   ACCT-ZAIKAISU)
                       EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   CONS-ROUSAI13
                              WHEN   CONS-ROUSAI14
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI15
                              WHEN   CONS-ROUSAI16
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI17
                              WHEN   CONS-ROUSAI18
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI19
                              WHEN   CONS-ROUSAI20
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       IF    RECE82-HEYA-NINZU   =   ZERO
                       AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                       ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                       END-IF
                  ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                       CONTINUE
                   ELSE
                    IF ( ACCT-SRYKBN          =  "80"    )  AND         
                       ( SRY-SRYCD(IDX3)(1:5) =  "09593" )              
                       COMPUTE    RECE82-KINGAKU3(3)  =                 
                                  RECE82-KINGAKU3(3)  +                 
                                 (TNS-TEN   *   ACCT-ZAIKAISU)          
                    ELSE                                                
                     IF  SRY-SRYCD(IDX3)      =    CONS-RYOYOTEATE
                       COMPUTE    RECE82-KINGAKU3(3)  =                 
                                  RECE82-KINGAKU3(3)  +                 
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      IF    TNS-NYUTENTTLKBN   =    130
                            COMPUTE   RECE82-TENSU(2)  =
                                      RECE82-TENSU(2)  +
                                     (TNS-TEN   *   ACCT-ZAIKAISU)
                      ELSE
                        IF    WK-SYUTEN       NOT =   ZERO
                           ADD  WK-ZAIKAISU  TO  RECE82-KAISU(14)
                        END-IF
                        COMPUTE   RECE82-TENSU(14)  =
                                  RECE82-TENSU(14)  +
                                 (WK-SYUTEN   *   ACCT-ZAIKAISU)
                      END-IF
                      COMPUTE   RECE82-TENSU-YAK(15)  =
                                RECE82-TENSU-YAK(15)  +
                               (WK-YKZTEN   *   WK-ZAIKAISU)
                     END-IF
                    END-IF                                              
                   END-IF
                  END-IF
           END-EVALUATE
           .
      *
       220111-RECE-LEFT-SET2-EXT.
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
           ELSE
               INITIALIZE       TNS-TENSU-REC
               MOVE    1   TO   FLG-TENSU
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
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î´ðËÜÅÀ¿ôÉôÊ¬¤Î¥¯¥ê¥¢½èÍý¡Ê¼«ÇåÀÕ¡Ë
      *****************************************************************
       270-KHNTEN-JI-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN7-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(7)
           END-IF
           IF        FLG-KHNTEN8-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(8)
           END-IF
           IF        FLG-KHNTEN9-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(9)
           END-IF
           .
       270-KHNTEN-JI-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Ø¤ÎÉéÃ´¶â¥»¥Ã¥È¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       260-FTN-SET-SEC                SECTION.
      *
           MOVE    ZERO                 TO      FLG-NYUINRYO
      *
           INITIALIZE                           SYUNOU-REC
           MOVE    RECE82-HOSPID        TO      SYU-HOSPID
           MOVE    RECE82-NYUGAIKBN     TO      SYU-NYUGAIKBN
           MOVE    RECE82-PTID          TO      SYU-PTID
           MOVE    RECE82-SRYYM         TO      SYU-SRYYMD(1:6)
           MOVE    "__"                 TO      SYU-SRYYMD(7:2)
           MOVE    RECE82-HKNCOMBI      TO      SYU-HKNCOMBINUM
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SYUNOU-KEY19"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SYUNOU-KEY19"   TO      ORC-DBPATH
               PERFORM  SYUNOU-READ-SEC
               PERFORM  UNTIL
                  (FLG-SYUNOU             =    1)
                           IF  SYU-DENPJTIKBN    NOT =  "3"
                               PERFORM   2601-RECE82-FTN-HENSYU-SEC
                           END-IF
                   MOVE     "SYUNOU-KEY19"  TO   ORC-DBPATH
                   PERFORM   SYUNOU-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE   "SYUNOU-KEY19"     TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       260-FTN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡ÝÉéÃ´¶â¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2601-RECE82-FTN-HENSYU-SEC               SECTION.
      *
           IF   RECE82-NYUINRYOFLG       =   "1"
           AND  FLG-NYUINRYO             =   ZERO
               PERFORM         26011-NYUINRYOFLG-SET-SEC
           END-IF
      *
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
                           OR    RECE82-SHOKUJIKHNRYO(ID2) = ZERO
                           OR    FLG-ARI                   = 1
                  IF  SYU-SHOKUJI(ID1)   =   RECE82-SHOKUJIKHNRYO(ID2)
                        ADD   SYU-SHOKUJI-NISSU(ID1)  TO  
                                       RECE82-SHOKUJIKHNNISSU(ID2)
                        MOVE  1   TO   FLG-ARI
                  END-IF
              END-PERFORM  
              IF  FLG-ARI  =  ZERO  
                  ADD   SYU-SHOKUJI-NISSU(ID1)  TO  
                                       RECE82-SHOKUJIKHNNISSU(ID2)
                  MOVE  SYU-SHOKUJI(ID1)        TO  
                                       RECE82-SHOKUJIKHNRYO(ID2)
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
                                       RECE82-SHOKUJIKHNNISSU(ID1)
                   MOVE  SYU-SHOKUJI(ID1)        TO  
                                       RECE82-SHOKUJIKHNRYO(ID1)
              END-IF
      *
           END-PERFORM
      *
           IF      SYU-SYUSKJ-RYOYOHI  NOT =  ZERO
                   ADD   SYU-SYUSKJ-NISSU     TO  
                                       RECE82-SHOKUJITOTALDAY
                   ADD   SYU-SYUSKJ-RYOYOHI   TO
                                       RECE82-SHOKUJITOTALKIN
           END-IF
      *
           .
       2601-RECE82-FTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ¤ò»»Äê¤·¤Æ¤¤¤ë¾ì¹ç¤ÎÊÝ¸±·èÄê
      *****************************************************************
       26011-NYUINRYOFLG-SET-SEC               SECTION.
      *
      *    Æþ±¡´ðËÜÎÁÌµ¤·¡¢Ëô¤Ï¡¢ÁÈ¤ß¹ç¤ï¤»¡á¼«Èñ¡¦¡¦¡¦£±
      *    ÁÈ¤ß¹ç¤ï¤»¡áÏ«ºÒ                      ¡¦¡¦¡¦£²
      *    ÁÈ¤ß¹ç¤ï¤»¡á¼«ÇåÀÕ                    ¡¦¡¦¡¦£³
      *    ÁÈ¤ß¹ç¤ï¤»¡á¹ñÊÝ                      ¡¦¡¦¡¦£´
      *    ÁÈ¤ß¹ç¤ï¤»¡á¼ÒÊÝ                      ¡¦¡¦¡¦£µ
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                            UNTIL   IDY     >       31
      *
             IF      SYU-DAY(IDY)    =    "2"
      *
                IF         WRK-DAY5(IDY)      NOT =  ZERO
                AND        WRK-DAY1(IDY)      NOT =  ZERO
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    RECE82-HOSPID   TO  COMB-HOSPID
                   MOVE    RECE82-PTID     TO  COMB-PTID
                   MOVE    WRK-DAY5(IDY)   TO  COMB-HKNCOMBINUM
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                   PERFORM HKNCOMBI-INV-SEC
                   IF      FLG-HKNCOMBI    =   ZERO
                     IF   COMB-HKNNUM(1:2)  =  "98"
                     OR   COMB-HKNNUM       =  "971"
                     OR   COMB-HKNNUM       =  "973"
                       IF   COMB-HKNNUM(1:2)  =  "98"
                          MOVE    "1"   TO   RECE82-NYUINRYOFLG
                       END-IF
                       IF   COMB-HKNNUM       =  "971"
                          MOVE    "2"   TO   RECE82-NYUINRYOFLG
                       END-IF
                       IF   COMB-HKNNUM       =  "973"
                          MOVE    "3"   TO   RECE82-NYUINRYOFLG
                       END-IF
                     ELSE
                       IF   COMB-HKNNUM       =  "060" OR  "067"
                          MOVE    "4"   TO   RECE82-NYUINRYOFLG
                       ELSE
                          MOVE    "5"   TO   RECE82-NYUINRYOFLG
                       END-IF
                     END-IF
                   END-IF
                   MOVE    "DBCLOSE"           TO  MCP-FUNC
                   MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
                ELSE
                   IF      WRK-DAY5(IDY)      NOT =  ZERO
                          MOVE    "1"   TO   RECE82-NYUINRYOFLG
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
       26011-NYUINRYOFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE84-WRITE-SEC                SECTION.
      *
           MOVE     RECE84-REC      TO  RECE8A-REC
      *
           COMPUTE  RECE84-ZAITEN      =  RECE84-ZAITEN     -  
                                          WRK-TEN
           COMPUTE  RECE84-MEISAISU    =  RECE84-MEISAISU   -
                                          WRK-MEISAI-CNT
           COMPUTE  RECE84-SYUTEN1     =  RECE84-SYUTEN1    -
                                          WRK-TEN
           COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL -
                                          WRK-SRYCDTOTAL
           COMPUTE  RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL -
                                          WRK-SURYOUTOTAL     
      *
           MOVE     WRK-TEN         TO  RECE8A-ZAITEN
           MOVE     WRK-MEISAI-CNT  TO  RECE8A-MEISAISU
           MOVE     WRK-TEN         TO  RECE8A-SYUTEN1  
           MOVE     WRK-SRYCDTOTAL  TO  RECE8A-SRYCDTOTAL
           MOVE     WRK-SURYOUTOTAL TO  RECE8A-SURYOUTOTAL
      *
           WRITE   RECE84-REC
           ADD     1                TO  CNT-OUT1
      *
           WRITE   RECE8A-REC
           ADD     1                TO  CNT-OUT4
      *
           .
       RECE84-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE85-WRITE-SEC                SECTION.
      *
           MOVE    RECE85-REC          TO   RECE8B-REC      
      *
           WRITE   RECE85-REC
           ADD     1                   TO   CNT-OUT2
      *
           WRITE   RECE8B-REC
           ADD     1                   TO   CNT-OUT5
      *
           .
       RECE85-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE85-REWRITE-SEC              SECTION.                        
      *
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPID       TO  RECE85-HOSPID
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID     TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                    KEY IS    >=   RECE85-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
           END-START
           PERFORM   UNTIL
               (RECE84-HOSPID       NOT =  RECE85-HOSPID)      OR
               (RECE84-SRYYM        NOT =  RECE85-SRYYM)       OR
               (RECE84-NYUGAIKBN    NOT =  RECE85-NYUGAIKBN)   OR
               (RECE84-PTID         NOT =  RECE85-PTID)        OR
               (RECE84-RECEKA       NOT =  RECE85-RECEKA)      OR
               (RECE84-HKNID        NOT =  RECE85-HKNID)    OR
               (RECE84-TEKSTYMD     NOT =  RECE85-TEKSTYMD)    OR
               (RECE84-ZAINUM       NOT =  RECE85-ZAINUM)      OR
               (FLG-RECE85-ZAN            =      1        )    OR
               (FLG-REWRITE               =      1        )
                   IF    RECE85-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      MOVE    WRK-SRYSURYO3  TO  RECE85-SRYSURYO(5)
                      REWRITE   RECE85-REC
                   END-IF
                   READ    RECE85-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
           END-PERFORM
      *
           .
       RECE85-REWRITE-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿¹¹¿·½èÍý¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       RECE82-REWRITE-SEC                SECTION.
      *
           IF   RECE82-HKNKBN     =   "4"                               
      *
               COMPUTE   RECE82-TENSU(3)      =  RECE82-TENSU(1)        
                                              +  RECE82-TENSU(2)        
      *
               COMPUTE   RECE82-TENSU(10)     =  RECE82-TENSU(7)       
                                              +  RECE82-TENSU(8)       
                                              +  RECE82-TENSU(9)       
      *
               COMPUTE   RECE82-TENSU-YAK(11) =  RECE82-TENSU-YAK(4)    
                                              +  RECE82-TENSU-YAK(5)   
                                              +  RECE82-TENSU-YAK(6)   
      *
               COMPUTE   RECE82-TOTALKIN-ETC      =
                         RECE82-KINGAKU3-SYO      +
                         RECE82-SHOKUJITOTALKIN   +
                         RECE82-SINDAN-KIN        +
                         RECE82-MEISAI-KIN
      * 
           END-IF                                                       
      *
           REWRITE   RECE82-REC
      *
           ADD     1                   TO  CNT-OUT3
      *
           .
       RECE82-WRITE-EXT.
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
           CLOSE   RECE82-FILE
                   RECE84-FILE
                   RECE85-FILE
                   RECE8A-FILE
                   RECE8B-FILE
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
           DISPLAY "RECE82-IN  :" CNT-IN   
           DISPLAY "RECE82-MOD :" CNT-OUT3  
           DISPLAY "RECE84-OUT :" CNT-OUT1 
           DISPLAY "RECE85-OUT :" CNT-OUT2  
           DISPLAY "RECE8A-OUT :" CNT-OUT4 
           DISPLAY "RECE8B-OUT :" CNT-OUT5  
           DISPLAY "*** ORCR0840 END ***"
           .
       300-TERM-EXT.
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
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
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
       HKNCOMBI-INV-EXT.
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
       900-DBDISCONNECT-EXT.
           EXIT.
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
       RECE82-READ-SEC            SECTION.
      *
           READ    RECE82-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
                   IF   WRK-PARA-SRYYM    NOT =  ZERO
                      CONTINUE
                   ELSE
      *               ¿©»ö¡¦Ì¤¼Â»Ü¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
                      MOVE     ZERO      TO  WRK-SHOKUJIRYOYO
                                             WRK-TOKUKANRIKASAN
      *
                      MOVE  ZERO         TO  FLG-SET
                      MOVE  RECE82-SRYYM TO  WRK-SYS-SRYYMD(1:6)
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
                          PERFORM  110-SYSKAN-HENSYU-SEC
                       END-IF
      *
                      END-PERFORM
                   END-IF
      *
                   IF   WRK-PARA-SRYYM    NOT =  ZERO
                      CONTINUE
                   ELSE
      *               ¼«ÇåÀÕ¿©»öÎÅÍÜÈñ¶èÊ¬¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
                      MOVE    SPACE      TO  WRK-4001-SKJKBN
      *
                      MOVE  ZERO         TO  FLG-SET
                      MOVE  RECE82-SRYYM TO  WRK-SYS-SRYYMD(1:6)
                      MOVE  "01"         TO  WRK-SYS-SRYYMD(7:2)
                      PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                           UNTIL  ( SYSKAN-IDX              >  10   )
                            OR    ( TBL-SYS4001(SYSKAN-IDX) = SPACE )
                            OR    ( FLG-SET                 =   1   )
      *
                       MOVE  TBL-SYS4001(SYSKAN-IDX)  TO  SYS-4001-REC
                       IF   ( SYS-4001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                       AND  ( WRK-SYS-SRYYMD     <=  SYS-4001-EDYUKYMD )
                          MOVE     1               TO    FLG-SET
                          MOVE    SYS-4001-SKJKBN  TO    WRK-4001-SKJKBN
                       END-IF
      *
                      END-PERFORM
                   END-IF
           END-READ
           .
       RECE82-READ-EXT.
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
