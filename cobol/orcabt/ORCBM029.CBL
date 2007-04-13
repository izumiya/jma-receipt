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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBM029.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ·î¼¡Ä¢É¼
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¹ñÊÝ¡¦Áí³çÉ½¡Ê°å²Ê¡¦»õ²Ê¡Ë
      *                      £È£Ã£Í£²£Í¡¢£È£Ã£Í£²£Î
      *                      £²£°£°£³¡¥£´²þÀµÂÐ±þ
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/04/17    NACL-Æ£¸¶     ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-Æ£¸¶    03/05/19  ÂÐ¾ÝÊÝ¸±¼ÔÈÖ¹æ¤ÎÄÉ²Ã
      *  01.00.02    NACL-Æ£¸¶    03/07/10  ÆÃÄê¼À´µÍ­¤ê¡¦Ìµ¤·¤Î½¸·×¤ò
      *                                     Æ±¤¸Íó¤ËÊÔ½¸¤¹¤ë
      *  01.00.03    NACL-Æ£¸¶    03/07/21  ¸øÈñÍó¤ÎÆÃÄê¼À´µ¤ÎÌ¾¾Î¤ÎÊÑ¹¹
      *ÊÖÌáÊ¬ÂÐ±þ  
      *  01.01.01    NACL-Æ£¸¶    05/03/03  ÊÖÌáÊ¬ÂÐ±þ
      *
      *  02.07.01    NACL-Æ£¸¶    05/08/20  ÆÃÊÌÎÅÍÜÈñÂÐ±þ
      *
      *  £²£°£°£¶Ç¯£´·î²þÀµÂÐ±þ
      *  02.09.01    NACL-Æ£¸¶    06/04/13  £²£°£°£¶Ç¯£´·î²þÀµÂÐ±þ
      *                                     HCM2M02.red ¢ª HCM2W.red 
      *                                     HCM2N02.red ¢ª HCM2X.red 
      *
      *  03.03.01    NACL-Æ£¸¶    06/10/10  £²£°£°£¶Ç¯£±£°·î²þÀµÂÐ±þ
      *                                     HCM2W.red ¢ª HCM2WV01.red
      *                                     HCM2X.red ¢ª HCM2XV01.red 
      *  03.03.02    NACL-Æ£¸¶    06/11/08  À¸³èÎÅÍÜ¤Î¤ß¤Î¾ì¹ç¤Î½¸·×
      *  03.03.03    NACL-Æ£¸¶    07/04/13  °åÎÅµ¡´Ø¾ðÊó¼èÆÀ¤Î¾ò·ï¤ò½¤Àµ
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ÀÁµá£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ÀÁµá£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    ÀÁµá£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *****COPY    "HCM2M.INC".
      *****COPY    "HCM2MV01.INC"  REPLACING  //HCM2MV01//
      *****COPY    "HCM2MV02.INC"  REPLACING  //HCM2MV02//
      *****                        BY         //HCM2M//.
      *****COPY    "HCM2W.INC".
           COPY    "HCM2WV01.INC"  REPLACING  //HCM2WV01//
                                   BY         //HCM2W//.
      *****COPY    "HCM2N.INC".
      *****COPY    "HCM2NV01.INC"  REPLACING  //HCM2NV01//
      *****COPY    "HCM2NV02.INC"  REPLACING  //HCM2NV02//
      *****                        BY         //HCM2N//.
      *****COPY    "HCM2X.INC".
           COPY    "HCM2XV01.INC"  REPLACING  //HCM2XV01//
                                   BY         //HCM2X//.
      *
      *    ¥¹¥ÑÎÎ°è
       01  STS-AREA.
           03  STS-RECE10          PIC X(02). 
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-KOH-END         PIC 9(01).
           03  FLG-UPD             PIC 9(01).
      *     
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01). 
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01). 
           03  FLG-PTINF           PIC 9(01).
      *     
           03  FLG-PRTKBN          PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
      *    ¥Ñ¥é¥á¥¿ÎÎ°è
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-NYUGAIKBN  PIC X(01).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-SYOKBN1    PIC X(01).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *  
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-RECEERR         PIC X(200). 
      *
           03  WRK-PATH.
               05  FILLER          PIC S9(09)  BINARY.
               05  FILLER          PIC S9(09)  BINARY.
               05  FILLER          PIC S9(09)  BINARY.
      *
           03  WRK-INDEX           PIC 9(02).
           03  WRK-SYOKBN          PIC X(01).
      *
           03  WRK-PRTFLG          PIC 9(01).
      *
      *    ¿ô»ú¹àÌÜÊÔ½¸
           03  WRK-KOHNUM9         PIC ZZ9.
           03  WRK-NISSUZ          PIC ZZZZZZ.
           03  WRK-KENSUZ          PIC ZZZZZZ.
           03  WRK-TENSUZ          PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-FTNMONEYZ       PIC Z,ZZZ,ZZZ,ZZZ. 
           03  WRK-KINGAKZ         PIC Z,ZZZ,ZZZ,ZZZ. 
           03  WRK-FTNGAKZ         PIC Z,ZZZ,ZZZ,ZZZ. 
           03  WRK-PAGE            PIC ZZZ9.
      *
      *    ¸øÈñÌ¾¾ÎÊÔ½¸
           03  WRK-KOHKBN.
               05  WRK-KOHNUM        PIC X(03).
               05  WRK-HKNNAME       PIC X(20). 
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-PAGE              PIC 9(04).
           03  CNT-RECE10            PIC 9(06).
           03  CNT-UPDFILE           PIC 9(06).
      *
      *    ½¸·×ÎÎ°è
       01  SUM-AREA.
           03  SUM-TABLE.
      *        ¹ñÌ±·ò¹¯ÊÝ¸±¡¦Ï·¿ÍÊÝ·ò         
               05  SUM-TBL             OCCURS  11.
                   07  SUM-TBL-G       OCCURS  2.
                       09  SUM-KENSU           PIC  9(06).
                       09  SUM-NISSU           PIC  9(06).
                       09  SUM-TOTALTEN        PIC  9(10).
                       09  SUM-FTNMONEY        PIC  9(10).
                   07  SUM-SHOKUJIKENSU        PIC  9(06).
                   07  SUM-SHOKUJINISSU        PIC  9(06).
                   07  SUM-SHOKUJIRYOYOHI      PIC  9(10).
                   07  SUM-SHOKUJIFTN          PIC  9(10).
      *             
      *        ¸øÈñÉéÃ´ 
               05  SUM-KOH-TABLE. 
                   07  SUM-KOH-TBL     OCCURS  100.
                       09  SUM-KOH-KOHNUM      PIC  X(03).
                       09  SUM-TBL-G   OCCURS  2.
                           11  SUM-KOH-KENSU   PIC  9(06).
                           11  SUM-KOH-NISSU   PIC  9(06).
                           11  SUM-KOH-TOTALTEN
                                               PIC  9(10).
                           11  SUM-KOH-FTNMONEY
                                               PIC  9(10).
                       09  SUM-KOH-SHOKUJIKENSU
                                               PIC  9(06).
                       09  SUM-KOH-SHOKUJINISSU
                                               PIC  9(06).
                       09  SUM-KOH-SHOKUJIRYOYOHI
                                               PIC  9(10).
                       09  SUM-KOH-SHOKUJIFTN  PIC  9(10).
      *
      *    ¥­¡¼ÎÎ°è
       01  KEY-AREA                    VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-HKNJANUM      PIC X(08).
           03  KEY-OLD.
               05  KEY-O-HKNJANUM      PIC X(08). 
      *
      *
           COPY    "CPSHELLTBL.INC".
      *     
           COPY    "ORCA-DBPATH".
      *
      *    ¸ÇÄê¹àÌÜ
       01  WRK-CONS-AREA.
      *
      *    £²£°£°£³Ç¯£´·î²þÀµ
           03  WRK-CONS-SRYYM-200304   PIC X(06)   VALUE   "200304".
      *    £²£°£°£¶Ç¯£´·î²þÀµ
           03  WRK-CONS-SRYYM-200604   PIC X(06)   VALUE   "200604".
      *    £²£°£°£¶Ç¯£±£°·î²þÀµ
           03  WRK-CONS-SRYYM-200610   PIC X(06)   VALUE   "200610".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ´µ¼Ô
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÊÝ¸±¼Ô
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    ÊÝ¸±ÈÖ¹æ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    °åÎÅµ¡´Ø¾ðÊó¾ðÊó
           COPY    "CPSK1001.INC".
      *
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý½êºßÃÏ¡¢Ï¢ÍíÀè 
           COPY    "CPSK1002.INC".
      *
      *    ÀÁµá´ÉÍý
           COPY    "CPRCF010.INC".
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    °õºþ´ÉÍý
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    °õºþ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      * 
      ******************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    °õºþ£Ä£Â¹¹¿·¥µ¥Ö
           COPY    "CPORCSPRT.INC".          
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "MCPAREA".
      *
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
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
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
           INITIALIZE                  SUM-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID    
                                               LNK-PRTKANRI-PRTNM    
                                               WRK-PARA-HOSPID
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-SYOKBN1
           END-UNSTRING  
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBM029"      TO  JOB-PGID
           MOVE    "¹ñÊÝÁí³çÉ½¡Ê²þÀµÈÇ¡Ë"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECEUPD"       TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  RECEUPD-TERMID
      *
           MOVE    "RECEERR"       TO  RECEERR-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  RECEERR-TERMID
      *
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  WRK-SRYYMD (1:6)
           MOVE    "01"            TO  WRK-SRYYMD (7:2)
      *     
           MOVE    WRK-SRYYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)
                                   TO  WRK-SRYYMWH 
      *
           INITIALIZE              STS-AREA-DAY
           INITIALIZE              LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1     =   ZERO 
               MOVE    LNK-DAY2-EDTYMD3
                                       TO  WRK-SEIYMDWH
           END-IF
      *
      *    °åÎÅµ¡´Ø£É£ÄÊÔ½¸
           INITIALIZE              SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD
                                   TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE           TO  SYS-1001-HOSPNAME
               END-IF    
               IF      SYS-1001-KAISETUNAME
                                           =   LOW-VALUE
                   MOVE    SPACE           TO  SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý½êºßÃÏ
           INITIALIZE              SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1002-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1002-STYUKYMD (7:2)
           MOVE    SYS-1002-STYUKYMD
                                   TO  SYS-1002-EDYUKYMD
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
               IF      SYS-1002-ADRS   =   LOW-VALUE
                   MOVE    SPACE           TO  SYS-1002-ADRS
               END-IF    
           ELSE
               MOVE    "°åÎÅµ¡´Ø¾ðÊó¡Ê½êºßÃÏ¡Ë¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF 
      *
           OPEN    OUTPUT  UPD-FILE
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    ZERO
               WHEN    "2"
                   MOVE    "2"         TO  WRK-SYOKBN
                   PERFORM 2001-SYOKBN-SYUKEI-SEC
           END-EVALUATE
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    ZERO
               WHEN    "1"
                   MOVE    "1"         TO  WRK-SYOKBN
                   PERFORM 2001-SYOKBN-SYUKEI-SEC
           END-EVALUATE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½¸·×½èÍý
      *****************************************************************
       2001-SYOKBN-SYUKEI-SEC         SECTION.
      *
      *    1:-200603 2:200604-200609 3:200610-   
           PERFORM VARYING WRK-PRTFLG  FROM    1   BY  1
                   UNTIL   WRK-PRTFLG  >       3
      *
               IF    ( WRK-PRTFLG          =   3                     )
               AND   ( LNK-PRTKANRI-SRYYM  <   WRK-CONS-SRYYM-200610 )
                   CONTINUE
               ELSE
                 IF    ( WRK-PRTFLG          >=  2                     )
                 AND   ( LNK-PRTKANRI-SRYYM  <   WRK-CONS-SRYYM-200604 )
                     CONTINUE
                 ELSE
      *
      *            Æþ±¡³°¡Ê¤½¤ÎÂ¾¤Î¥ì¥»¡Ë¡¦Æþ±¡½¸·×
      *    
                   MOVE    LOW-VALUE           TO  KEY-AREA
                   MOVE    1                   TO  WRK-INDEX
      *     
                   EVALUATE    WRK-SYOKBN      ALSO    WRK-PARA-SYOKBN1
                       WHEN    "1"             ALSO    "1"
                           MOVE    PATH-TBL-SEIKYU-KEY34    TO  WRK-PATH
                       WHEN    "2"             ALSO    "1"
                           MOVE    PATH-TBL-SEIKYU-KEY33    TO  WRK-PATH
                       WHEN    "1"             ALSO    "2"
                           MOVE    PATH-TBL-SEIKYU-KEY40    TO  WRK-PATH
                       WHEN    "2"             ALSO    "2"
                           MOVE    PATH-TBL-SEIKYU-KEY39    TO  WRK-PATH
                   END-EVALUATE  
                   PERFORM 900-SEIKYU-SELECT-SEC
      *
                   PERFORM 200-HENSYU-SEC  UNTIL   FLG-RECE10   =   1 
                                           OR      FLG-ERR      =   1
      *
                   MOVE    WRK-PATH            TO  MCP-PATH
                   MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
                   CALL    "MCPSUB"            USING
                                               MCPAREA
                                               MCPDATA-REC
      *
      *            Æþ±¡³°½¸·×¡ÊºßÁí¿Ç¤Þ¤¿¤Ïºß°åÁí¤ò»»Äê¤·¤¿¥ì¥»¡Ë
      *            ÆÃÊÌÎÅÍÜÈñ¤Î¾ì¹ç¤ÏºßÁí¿Ç¤Ï¤Ê¤·
                   IF    ( FLG-ERR             =   ZERO )
                   AND   ( WRK-PARA-SYOKBN1    =   "1"  )
                       MOVE    LOW-VALUE           TO  KEY-AREA
                       MOVE    2                   TO  WRK-INDEX
      *         
                       EVALUATE    WRK-SYOKBN
                           WHEN    "1"
                               MOVE    PATH-TBL-SEIKYU-KEY36
                                                       TO  WRK-PATH
                           WHEN    "2"
                               MOVE    PATH-TBL-SEIKYU-KEY35
                                                       TO  WRK-PATH
                       END-EVALUATE  
                       PERFORM 900-SEIKYU-SELECT-SEC
      *
                       PERFORM 200-HENSYU-SEC  UNTIL   FLG-RECE10   =  1
                                               OR      FLG-ERR      =  1
      *
                       MOVE    WRK-PATH            TO  MCP-PATH
                       MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
                       CALL    "MCPSUB"            USING
                                                       MCPAREA
                                                       MCPDATA-REC
                   END-IF
                 END-IF
               END-IF
           END-PERFORM 
      *     
           MOVE    1                   TO  FLG-END
           .
       2001-SYOKBN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½¸·×½èÍý
      *****************************************************************
       200-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  SUM-AREA
      *
           MOVE    KEY-NEW             TO  KEY-OLD      
           PERFORM             UNTIL   FLG-END     =   1
                               OR      KEY-NEW NOT =   KEY-OLD
               PERFORM 2001-SYUKEI-SEC
           END-PERFORM
      *
          IF      FLG-UPD        =   1
          AND     FLG-ERR        =   ZERO 
               PERFORM 2002-PRINT-HENSYU-SEC
           END-IF
           .
       200-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±¼ÔÊÌ½¸·×½èÍý
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      * 
           MOVE    ZERO                    TO  IDX-AREA
                                               FLG-UPD
      *
           EVALUATE    RECE10-NYUGAIKBN
               WHEN    "1"
                   MOVE    1                 TO  IDX3
               WHEN    "2"
                   MOVE    2                 TO  IDX3
           END-EVALUATE           
      *
      *        Ï·¿ÍÊÝ·ò
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               EVALUATE    RECE10-KYURATE
                   WHEN    9
                       MOVE    10                  TO  IDX
                   WHEN    8
                   WHEN    7
                       MOVE    11                  TO  IDX
               END-EVALUATE        
           ELSE
      *        ¹ñÌ±·ò¹¯ÊÝ¸±
               EVALUATE    RECE10-HKNNUM-S
                       WHEN    "060"
                       WHEN    "068"
                           IF      RECE10-AGE          >=  70
                               EVALUATE    RECE10-KYURATE
                                   WHEN    9
                                       MOVE    1           TO  IDX
                                   WHEN    8
                                   WHEN    7
                                       MOVE    2           TO  IDX
                               END-EVALUATE        
                           ELSE
                               IF      RECE10-AGE      <   3
                                   MOVE    4               TO  IDX
                               ELSE                    
                                   MOVE    3               TO  IDX
                               END-IF
                           END-IF
                       WHEN    "067"
                       WHEN    "069"
                           IF      RECE10-AGE          >=  70
                               EVALUATE    RECE10-KYURATE
                                   WHEN    9
                                       MOVE    6           TO  IDX
                                   WHEN    8
                                   WHEN    7
                                       MOVE    7           TO  IDX
                               END-EVALUATE        
                           ELSE
                               IF      RECE10-AGE      <   3
                                   MOVE    9               TO  IDX
                               ELSE                    
                                   IF      RECE10-HONKZKKBN    =   "1"
                                       MOVE    5               TO  IDX
                                   ELSE
                                       MOVE    8               TO  IDX
                                   END-IF
                               END-IF
                           END-IF        
               END-EVALUATE
           END-IF
           PERFORM 20011-HKN-SYUKEI-SEC
      *
      *    ¸øÈñÉéÃ´Ê¬     
           IF      RECE10-KOHNUM (1)   NOT =   SPACE
               PERFORM 20012-KOH-SYUKEI-SEC
           END-IF
      *
      *    ½¸·×¤ÎÂÐ¾Ý¤Ë¤Ê¤é¤Ê¤«¤Ã¤¿¤È¤­                
           IF      FLG-UPD             =   1
               PERFORM 20013-UPD-FILE-HENSYU-SEC
      *
               MOVE    WRK-PATH            TO  MCP-PATH
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "½¸·×ÂÐ¾Ý³°¡¡´µ¼ÔÈÖ¹æ¡á"     DELIMITED  BY  SIZE
                       RECE10-PTNUM                DELIMITED  BY  SPACE
                       " ¿ÇÎÅÇ¯·î¡á"               DELIMITED  BY  SIZE
                       RECE10-SRYYM                DELIMITED  BY  SIZE
                       " ¥ì¥»¼ïÊÌ¡á"               DELIMITED  BY  SIZE
                       RECE10-RECESYUBETU          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING                                
               PERFORM 500-ERR-HENSYU-SEC
           END-IF 
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅÊÝ¸±¡¦Ï·¿ÍÊÝ·òÊ¬½¸·×½èÍý
      *****************************************************************
       20011-HKN-SYUKEI-SEC          SECTION.
      *
           IF      IDX             =   ZERO
           OR      IDX3            =   ZERO 
               GO  TO  20011-HKN-SYUKEI-EXT
           END-IF
      *
      *    °åÎÅÊÝ¸±¡¦Ï·¿ÍÊÝ·òÊ¬
           MOVE    1                         TO  IDX1
           MOVE    IDX                       TO  IDX2
      *    
      *    ·ï¿ô
           ADD     1               TO  SUM-KENSU (IDX2 IDX3)
      *
      *    ÎÅÍÜ¤ÎµëÉÕ 
           ADD     RECE10-JNISSU   (IDX1) TO 
                                   SUM-NISSU      (IDX2 IDX3)
           ADD     RECE10-TOTALTEN (IDX1) TO
                                   SUM-TOTALTEN   (IDX2 IDX3)
           ADD     RECE10-FTNMONEY (IDX1) TO
                                   SUM-FTNMONEY   (IDX2 IDX3)
      *    ¿©»öÎÅÍÜ
           IF      IDX3                   =   1 
              IF      RECE10-SHOKUJINISSU(IDX1)   >   ZERO
                  ADD     1              TO
                                  SUM-SHOKUJIKENSU    (IDX2)
                  ADD     RECE10-SHOKUJINISSU   (IDX1)
                                         TO
                                  SUM-SHOKUJINISSU    (IDX2)
               END-IF
               ADD     RECE10-SHOKUJIRYOYOHI (IDX1)
                                         TO
                                  SUM-SHOKUJIRYOYOHI  (IDX2)
               ADD     RECE10-SHOKUJIFTN     (IDX1)
                                         TO
                                  SUM-SHOKUJIFTN      (IDX2)
           END-IF    
      *
           MOVE    1               TO  FLG-UPD 
           .
       20011-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñÉéÃ´Ê¬½¸·×½èÍý
      *****************************************************************
       20012-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       4
                   OR  RECE10-KOHNUM (IDZ) =   SPACE
      *
      *        ÆÃÄê¼À´µÌµ¤·¤Î¤È¤­ 
               IF      RECE10-KOHNUM (IDZ) =   "091"
                   MOVE    "051"               TO  RECE10-KOHNUM (IDZ)
               END-IF
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2 >          100
                   IF      SUM-KOH-KOHNUM (IDX2) =   SPACE
                       MOVE    RECE10-KOHNUM (IDZ)     TO 
                                       SUM-KOH-KOHNUM (IDX2)
                   END-IF                                
                   IF      RECE10-KOHNUM (IDZ) 
                                   =   SUM-KOH-KOHNUM (IDX2)
                       PERFORM 200121-KOH-SYUKEI-01-SEC
                       MOVE    100             TO  IDX2
                   END-IF
               END-PERFORM
           END-PERFORM                 
           .
       20012-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ½¸·×½èÍý
      *****************************************************************
       200121-KOH-SYUKEI-01-SEC        SECTION.
      *
           COMPUTE IDY     =   IDZ     +   1
      *    
      *    ·ï¿ô
           ADD     1               TO  SUM-KOH-KENSU (IDX2 IDX3)
      *
      *    ÎÅÍÜ¤ÎµëÉÕ 
           ADD     RECE10-JNISSU   (IDY) TO 
                                   SUM-KOH-NISSU      (IDX2 IDX3)
           ADD     RECE10-TOTALTEN (IDY) TO
                                   SUM-KOH-TOTALTEN   (IDX2 IDX3)
           ADD     RECE10-FTNMONEY (IDY) TO
                                   SUM-KOH-FTNMONEY   (IDX2 IDX3)
      *    ¿©»öÎÅÍÜ
           IF      IDX3                   =   1 
              IF      RECE10-SHOKUJINISSU(IDY)   >   ZERO
                  ADD     1              TO
                                  SUM-KOH-SHOKUJIKENSU   (IDX2)
                  ADD     RECE10-SHOKUJINISSU   (IDY)
                                         TO
                                  SUM-KOH-SHOKUJINISSU   (IDX2)
               END-IF
               ADD     RECE10-SHOKUJIRYOYOHI (IDY)
                                         TO
                                  SUM-KOH-SHOKUJIRYOYOHI (IDX2)
               ADD     RECE10-SHOKUJIFTN     (IDY)
                                         TO
                                  SUM-KOH-SHOKUJIFTN     (IDX2)
           END-IF    
      *
           MOVE    1               TO  FLG-UPD 
           .
       200121-KOH-SYUKEI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
      *****************************************************************
       20013-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           WRITE   RECE10X-REC
           ADD     1                   TO  CNT-UPDFILE
           .
       20013-UPD-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
      *****************************************************************
       2002-PRINT-HENSYU-SEC                SECTION.
      *           
      *    Ä¢É¼ÊÔ½¸<¸«½Ð¤·¡¦£È£Ã£Í£²£Í>½èÍý
           PERFORM 20021-HEAD-HEN-01-SEC
      *    Ä¢É¼ÊÔ½¸<¸«½Ð¤·¡¦£È£Ã£Í£²£Î>½èÍý
           PERFORM 20022-HEAD-HEN-02-SEC
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ>½èÍý
           PERFORM 20023-BODY-HEN-SEC
      *    Ä¢É¼°õºþ½èÍý
           PERFORM 20024-PRINT-OUT-SEC
      *
           .
       2002-PRINT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¥Ø¥Ã¥À¡¼Éô>½èÍý
      *****************************************************************
       20021-HEAD-HEN-01-SEC          SECTION.
      *
           INITIALIZE                      HCM2W      
      *
           MOVE    WRK-SEIYMDWH        TO  HCM2W-SEIYMD
      *
           MOVE    WRK-SRYYMWH         TO  HCM2W-PRTYM
      *
      *    °åÎÅµ¡´Ø½»½ê
           MOVE    SYS-1002-ADRS       TO  HCM2W-ADRS
      *    °åÎÅµ¡´ØÌ¾
           MOVE    SYS-1001-HOSPNAME   TO  HCM2W-HOSPNAME
      *    
           MOVE    SYS-1001-KAISETUNAME
                                       TO  HCM2W-KAISETUNAME
      *    
           MOVE    SYS-1001-HOSPCDN    TO  HCM2W-HOSPCD
      *
           MOVE    SPACE               TO  HKNJAINF-REC
           INITIALIZE                      HKNJAINF-REC
           MOVE    SYS-1001-HOSPID     TO  HKNJA-HOSPID
           MOVE    KEY-O-HKNJANUM      TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           PERFORM 900-HKNJAINF-INV-SEC
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNJANAME     TO  HCM2W-HKNJANAME
           END-IF                                
           MOVE    KEY-O-HKNJANUM      TO  HCM2W-HKNJANUM
      *
           EVALUATE    WRK-INDEX
               WHEN    1
                   MOVE    "¤½¤ÎÂ¾¤Î¥ì¥»"      TO  HCM2W-TITLE  
               WHEN    2
                   IF      WRK-PRTFLG          =   1
                       MOVE    "ºßÁí¿Ç¤Þ¤¿¤Ïºß°åÁí¤ò»»Äê¤·¤¿¥ì¥»"
                                               TO  HCM2W-TITLE
                   ELSE   
                       MOVE    "ºß°åÁí´É¤Þ¤¿¤Ïºß°åÁí¤ò»»Äê¤·¤¿¥ì¥»"
                                               TO  HCM2W-TITLE
                   END-IF
           END-EVALUATE           
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "2"
                   MOVE    "¡ÊÊÖÌáÊ¬¡Ë"  TO  HCM2W-TITLE1
           END-EVALUATE           
      *
           EVALUATE    WRK-PARA-SYOKBN1
               WHEN    "2"
                   MOVE    "ÆÃÊÌÎÅÍÜÈñ"  TO  HCM2W-MOJI
           END-EVALUATE           
      *
           IF      WRK-PRTFLG          =   1
               MOVE    "Æü¿ô"              TO  HCM2W-TITLE2
           ELSE   
               MOVE    "²ó¿ô"              TO  HCM2W-TITLE2
           END-IF
      *
           EVALUATE    WRK-PRTFLG
               WHEN    1
               WHEN    2
                   MOVE    "¡¡¿©¡¡»ö¡¡ÎÅ¡¡ÍÜ"
                                         TO  HCM2W-TITLE3
                   MOVE    "È¬"          TO  HCM2W-TITLE4
                                             HCM2W-TITLE5
               WHEN    3
                   MOVE    "¿©»öÎÅÍÜ¡¦À¸³èÎÅÍÜ"
                                         TO  HCM2W-TITLE3
                   MOVE    "¼·"          TO  HCM2W-TITLE4
                                             HCM2W-TITLE5
           END-EVALUATE
      *
           .
       20021-HEAD-HEN-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¸«½Ð¤·¡¦£È£Ã£Í£²£Î>½èÍý
      *****************************************************************
       20022-HEAD-HEN-02-SEC          SECTION.
      *
           INITIALIZE                      HCM2X
      *    
           MOVE    SYS-1001-HOSPCDN    TO  HCM2X-HOSPCD 
      *
           EVALUATE    WRK-INDEX
               WHEN    1
                   MOVE    "¤½¤ÎÂ¾¤Î¥ì¥»"      TO  HCM2X-TITLE  
               WHEN    2
                   IF      WRK-PRTFLG          =   1
                       MOVE    "ºßÁí¿Ç¤Þ¤¿¤Ïºß°åÁí¤ò»»Äê¤·¤¿¥ì¥»"
                                               TO  HCM2X-TITLE
                   ELSE   
                       MOVE    "ºß°åÁí´É¤Þ¤¿¤Ïºß°åÁí¤ò»»Äê¤·¤¿¥ì¥»"
                                               TO  HCM2X-TITLE
                   END-IF
           END-EVALUATE   
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "2"
                   MOVE    "¡ÊÊÖÌáÊ¬¡Ë"  TO  HCM2X-TITLE1
           END-EVALUATE           
      *
           EVALUATE    WRK-PARA-SYOKBN1
               WHEN    "2"
                   MOVE    "ÆÃÊÌÎÅÍÜÈñ"  TO  HCM2X-MOJI
           END-EVALUATE           
      *
           IF      WRK-PRTFLG          =   1
               MOVE    "Æü¿ô"              TO  HCM2X-TITLE2 (1)
                                               HCM2X-TITLE2 (2)
           ELSE   
               MOVE    "²ó¿ô"              TO  HCM2X-TITLE2 (1)
                                               HCM2X-TITLE2 (2)
           END-IF
      *
           EVALUATE    WRK-PRTFLG
               WHEN    1
               WHEN    2
                   MOVE    "¡¡¿©¡¡»ö¡¡ÎÅ¡¡ÍÜ"
                                         TO  HCM2X-TITLE3
                                             HCM2X-TITLE4
                   MOVE    "È¬"          TO  HCM2X-TITLE5
               WHEN    3
                   MOVE    "¿©»öÎÅÍÜ¡¦À¸³èÎÅÍÜ"
                                         TO  HCM2X-TITLE3
                                             HCM2X-TITLE4
                   MOVE    "¼·"          TO  HCM2X-TITLE5
           END-EVALUATE
      *        
           MOVE    KEY-O-HKNJANUM      TO  HCM2X-HKNJANUM
      *
           .
       20022-HEAD-HEN-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>½èÍý
      *****************************************************************
       20023-BODY-HEN-SEC      SECTION.
      *
      *    ¹ñÌ±·ò¹¯ÊÝ¸±
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       9
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       2
                   MOVE    SUM-KENSU   (IDX IDY)
                                           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  HCM2W-KENSU (IDX IDY)
                   MOVE    SUM-NISSU   (IDX IDY)
                                           TO  WRK-NISSUZ
                   MOVE    WRK-NISSUZ      TO  HCM2W-NISSU (IDX IDY) 
                   MOVE    SUM-TOTALTEN(IDX IDY)
                                           TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ      TO  HCM2W-TENSU (IDX IDY)
                   IF    ( IDY             =   2                       )
                   AND   ( IDX             =   3   OR  4   OR  5   OR
                                               8   OR  9               )
                       CONTINUE
                   ELSE                            
                       MOVE    SUM-FTNMONEY    (IDX IDY)
                                               TO  WRK-FTNMONEYZ
                       MOVE    WRK-FTNMONEYZ   TO  HCM2W-ITBFTN(IDX IDY)
                   END-IF    
               END-PERFORM     
               MOVE    SUM-SHOKUJIKENSU(IDX)   TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM2W-SYOKENSU (IDX)
               MOVE    SUM-SHOKUJINISSU(IDX)   TO  WRK-NISSUZ
               MOVE    WRK-NISSUZ              TO  HCM2W-SYONISSU (IDX)
               MOVE    SUM-SHOKUJIRYOYOHI(IDX) TO  WRK-KINGAKZ
               MOVE    WRK-KINGAKZ             TO  HCM2W-SYOKINGAK(IDX)
               MOVE    SUM-SHOKUJIFTN   (IDX)  TO  WRK-FTNGAKZ
               MOVE    WRK-FTNGAKZ             TO  HCM2W-SYOFTNGAK(IDX)
           END-PERFORM
      *
      *    Ï·¿ÍÊÝ·ò
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING IDX FROM    10  BY  1
                   UNTIL   IDX >       11
               ADD     1                   TO  IDZ    
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       2
                   MOVE    SUM-KENSU   (IDX IDY)
                                           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  HCM2X-HKN-KENSU (IDZ IDY)
                   MOVE    SUM-NISSU   (IDX IDY)
                                           TO  WRK-NISSUZ 
                   MOVE    WRK-NISSUZ      TO  HCM2X-HKN-NISSU (IDZ IDY) 
                   MOVE    SUM-TOTALTEN(IDX IDY)
                                           TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ      TO  HCM2X-HKN-TENSU (IDZ IDY)
                   MOVE    SUM-FTNMONEY    (IDX IDY)
                                           TO  WRK-FTNMONEYZ
                   MOVE    WRK-FTNMONEYZ   TO  HCM2X-HKN-ITBFTN(IDZ IDY)
               END-PERFORM     
               MOVE    SUM-SHOKUJIKENSU(IDX)
                                           TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ          TO  HCM2X-HKN-SYOKENSU (IDZ)
               MOVE    SUM-SHOKUJINISSU(IDX)
                                           TO  WRK-NISSUZ
               MOVE    WRK-NISSUZ          TO  HCM2X-HKN-SYONISSU (IDZ)
               MOVE    SUM-SHOKUJIRYOYOHI(IDX)
                                           TO  WRK-KINGAKZ
               MOVE    WRK-KINGAKZ         TO  HCM2X-HKN-SYOKINGAK(IDZ)
               MOVE    SUM-SHOKUJIFTN   (IDX)
                                           TO  WRK-FTNGAKZ
               MOVE    WRK-FTNGAKZ         TO  HCM2X-HKN-SYOFTNGAK(IDZ)
           END-PERFORM
      *     
      *    Ä¢É¼°õºþ½èÍý¡Ê£±ÊÇÌÜ¡Ë
           MOVE    ZERO                TO  FLG-PRTKBN
           PERFORM 20024-PRINT-OUT-SEC
           MOVE    1                   TO  FLG-PRTKBN
      *    ¸øÈñÉéÃ´Ê¬ÊÔ½¸½èÍý
           MOVE    ZERO                TO  FLG-KOH-END
           MOVE    1                   TO  IDX2
      * 
           PERFORM 200231-KOH-HENSYU-SEC
                                   UNTIL   FLG-KOH-END   =   1
      *
           .
       20023-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¸øÈñÉéÃ´Ê¬>½èÍý
      *****************************************************************
      *    
       200231-KOH-HENSYU-SEC         SECTION.
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2         
                   OR      FLG-KOH-END     =   1
               IF      SUM-KOH-KOHNUM (IDX2)   =   SPACE
                   MOVE    1                   TO  FLG-KOH-END
               ELSE    
                   PERFORM 2002311-HKNNUM-HENSYU-SEC     
                   PERFORM VARYING IDZ FROM    1   BY  1
                           UNTIL   IDZ >       2
                       MOVE    SUM-KOH-KENSU   (IDX2 IDZ)
                                           TO  WRK-KENSUZ
                       MOVE    WRK-KENSUZ  TO  HCM2X-KOH-KENSU (IDY IDZ)
                       MOVE    SUM-KOH-NISSU   (IDX2 IDZ)
                                           TO  WRK-NISSUZ
                       MOVE    WRK-NISSUZ  TO  HCM2X-KOH-NISSU (IDY IDZ) 
                       MOVE    SUM-KOH-TOTALTEN(IDX2 IDZ)
                                           TO  WRK-TENSUZ
                       MOVE    WRK-TENSUZ  TO  HCM2X-KOH-TENSU (IDY IDZ)
                       MOVE    SUM-KOH-FTNMONEY(IDX2 IDZ)
                                           TO  WRK-FTNMONEYZ
                       MOVE    WRK-FTNMONEYZ
                                           TO  HCM2X-KOH-ITBFTN(IDY IDZ)
                   END-PERFORM     
                   MOVE    SUM-KOH-SHOKUJIKENSU (IDX2)
                                           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  HCM2X-KOH-SYOKENSU (IDY)
                   MOVE    SUM-KOH-SHOKUJINISSU (IDX2)
                                           TO  WRK-NISSUZ
                   MOVE    WRK-NISSUZ      TO  HCM2X-KOH-SYONISSU (IDY)
                   MOVE    SUM-KOH-SHOKUJIRYOYOHI (IDX2)
                                           TO  WRK-KINGAKZ
                   MOVE    WRK-KINGAKZ     TO  HCM2X-KOH-SYOKINGAK(IDY)
                   MOVE    SUM-KOH-SHOKUJIFTN    (IDX2)
                                           TO  WRK-FTNGAKZ
                   MOVE    WRK-FTNGAKZ     TO  HCM2X-KOH-SYOFTNGAK(IDY)
      *             
                   ADD     1               TO  IDX2   
               END-IF        
           END-PERFORM
      *
           IF      SUM-KOH-KOHNUM (IDX2)   =   SPACE
               MOVE    1                   TO  FLG-KOH-END
           ELSE    
      *        Ä¢É¼°õºþ½èÍý¡Ê£²ÊÇ°Ê¹ß¡Ë
               PERFORM 20024-PRINT-OUT-SEC
      *        Ä¢É¼ÊÔ½¸<¸«½Ð¤·>½èÍý
               PERFORM 20022-HEAD-HEN-02-SEC
           END-IF     
      *
           .
       200231-KOH-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñÊÝ¸±¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
      *    
       2002311-HKNNUM-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-KOHKBN
      *
           MOVE    SUM-KOH-KOHNUM (IDX2)
                                       TO  WRK-KOHNUM9
           MOVE    WRK-KOHNUM9         TO  HCM2X-KOH-NUM (IDY)
      *
      *    ÆÃÄê¼À´µ¤Î¤È¤­ 
           IF      SUM-KOH-KOHNUM (IDX2)    =   "051"
               MOVE    "ÆÃÄê¼À´µ"      TO   WRK-HKNNAME
           ELSE
      *        ÊÝ¸±ÈÖ¹æÆÉ¹þ¤ß  
               INITIALIZE                      HKNNUM-REC
               MOVE   SYS-1001-HOSPID      TO    HKN-HOSPID
               MOVE   SUM-KOH-KOHNUM (IDX2)    TO    HKN-HKNNUM
               MOVE   HKNNUM-REC           TO    MCPDATA-REC
               MOVE   "DBSELECT"           TO    MCP-FUNC
               MOVE   "HKNNUM-KEY2"        TO    ORC-DBPATH
               CALL   "ORCMCPSUB"           USING MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "HKNNUM-KEY2"       TO  ORC-DBPATH
                   PERFORM 900-HKNNUM-READ-SEC
               ELSE
                   MOVE    1                  TO  FLG-HKNNUM
               END-IF
      *
               PERFORM UNTIL       FLG-HKNNUM      =   1
                 IF      HKN-TEKSTYMD            <=  WRK-SRYYMD
                 AND     HKN-TEKEDYMD            >=  WRK-SRYYMD
                   IF     SUM-KOH-KOHNUM (IDX2)    =   "027"
                       IF      SYS-1001-HOSPSBT    =   HKN-PAYKBN (1:1)
                       AND     SYS-1001-ROUPAYKBN  =   HKN-PAYKBN (2:1)
                           MOVE    HKN-TANSEIDONAME    TO   WRK-HKNNAME
                           MOVE    1                   TO  FLG-HKNNUM   
                       END-IF
                   ELSE
                       MOVE    HKN-TANSEIDONAME    TO   WRK-HKNNAME
                       MOVE    1                   TO  FLG-HKNNUM   
                   END-IF     
                 END-IF
      *
                 IF      FLG-HKNNUM           =   ZERO
                     MOVE    "HKNNUM-KEY2"       TO  ORC-DBPATH
                     PERFORM 900-HKNNUM-READ-SEC
                 END-IF
               END-PERFORM
      *        ¥«¡¼¥½¥ë¥¯¥í¡¼¥¹
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               MOVE    "HKNNUM-KEY2"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
           END-IF
      *
           INSPECT WRK-HKNNAME      REPLACING   ALL "¡Ê" BY  "¡¦"
                                                ALL "¡Ë" BY  "¡¡"  
           MOVE    WRK-HKNNAME         TO  HCM2X-KOH-NAME(IDY)
           .
       2002311-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼°õºþ½èÍý
      *****************************************************************
       20024-PRINT-OUT-SEC                SECTION.
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-PAGE
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           EVALUATE    FLG-PRTKBN
               WHEN    ZERO 
                   MOVE    WRK-PAGE            TO  HCM2W-PAGE
      *************MOVE    "HCM2M.red"         TO  SPRT-PRTID
      *************MOVE    "HCM2MV01.red"      TO  SPRT-PRTID
      *************MOVE    "HCM2MV02.red"      TO  SPRT-PRTID
      *************MOVE    "HCM2W.red"         TO  SPRT-PRTID
                   MOVE    "HCM2WV01.red"      TO  SPRT-PRTID
                   MOVE    HCM2W               TO  SPRT-PRTDATA
               WHEN    1                            
                   MOVE    WRK-PAGE            TO  HCM2X-PAGE
      *************MOVE    "HCM2N.red"         TO  SPRT-PRTID
      *************MOVE    "HCM2NV01.red"      TO  SPRT-PRTID
      *************MOVE    "HCM2NV02.red"      TO  SPRT-PRTID
      *************MOVE    "HCM2X.red"         TO  SPRT-PRTID
                   MOVE    "HCM2XV01.red"      TO  SPRT-PRTID
                   MOVE    HCM2X               TO  SPRT-PRTDATA
           END-EVALUATE        
           EVALUATE    WRK-INDEX    ALSO    WRK-SYOKBN
                                            ALSO    WRK-PARA-SYOKBN1
               WHEN    1            ALSO    "1"     
                                            ALSO   "1"
                   MOVE    "¹ñÊÝÁí³çÉ½"        TO  SPRT-TITLE
               WHEN    2            ALSO    "1"
                                            ALSO   "1"
                   MOVE    "¹ñÊÝÁí³çÉ½¡Êºß°åÁíÅù¡Ë"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "2"
                                            ALSO   "1"
                   MOVE    "¹ñÊÝÁí³çÉ½¡ÎÊÖÌáÊ¬¡Ï" 
                                               TO  SPRT-TITLE
               WHEN    2            ALSO    "2"
                                            ALSO   "1"
                   MOVE    "¹ñÊÝÁí³çÉ½¡Êºß°åÁíÅù¡Ë¡ÎÊÖÌáÊ¬¡Ï"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "1"     
                                            ALSO   "2"
                   MOVE    "¹ñÊÝÁí³çÉ½¡ÎÆÃÊÌÎÅÍÜÈñ¡Ï"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "2"
                                            ALSO   "2"
                   MOVE    "¹ñÊÝÁí³çÉ½¡ÎÊÖÌáÊ¬¡Ï¡ÎÆÃÊÌÎÅÍÜÈñ¡Ï" 
                                               TO  SPRT-TITLE
           END-EVALUATE        
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *     
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "°õºþ£Ä£Â¤Ë¹¹¿·¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC                           
           END-IF                                                              
           .
       20024-PRINT-OUT-EXT.
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
                                           FLG-ERR
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
           CLOSE   UPD-FILE
      *
      *
           IF      CNT-RECE10            =   ZERO 
               CONTINUE
           ELSE
              IF      FLG-UPD        =   1
              AND     FLG-ERR        =   ZERO 
                   IF      CNT-UPDFILE    >   ZERO
                       PERFORM 310-SEIKYU-UPD-SEC
                   END-IF
               END-IF     
           END-IF    
      *
           DISPLAY "*** ORCBM029 IN  "  CNT-RECE10 
           DISPLAY "*** ORCBM029 END ***"
      *     
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT                        
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *                              
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *   ÀÁµá´ÉÍý£Ä£Â¹¹¿·½èÍý
      *****************************************************************
       310-SEIKYU-UPD-SEC              SECTION.
      *
           OPEN    INPUT    UPD-FILE
      *
           PERFORM 900-UPD-FILE-READ-SEC
           PERFORM             UNTIL   FLG-UPDFILE    =   1
               MOVE    RECE10X-KEY         TO  RECE10-KEY
               MOVE    RECE10-REC          TO  MCPDATA-REC
               PERFORM 910-SEIKYU-INV-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    "2"             TO  RECE10-SKYKBN
                   MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD TO  RECE10-UPDYMD  
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "SEIKYU-KEY"    TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"      USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       STRING "ÀÁµá´ÉÍý£Ä£Â¡¡¹¹¿·¥¨¥é¡¼¡¡"
                                               DELIMITED  BY  SIZE
                               RECE10-KEY      DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                       END-STRING                                
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                   STRING "ÀÁµá´ÉÍý£Ä£Â¡¡ÆÉ¹þ¤ß¹¹¿·¥¨¥é¡¼¡¡"
                                               DELIMITED  BY  SIZE
                           RECE10X-KEY         DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING                                
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-UPDFILE
               END-IF
           END-PERFORM  
      *
           CLOSE   UPD-FILE
           .
       310-SEIKYU-UPD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    À¾ÎñÆüËÜ¸ìÊÑ´¹½èÍý
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "¡¡"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá´ÉÍýÆÉ¹þ
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE10
      *     
      *    ÀÁµá´ÉÍý£Ä£Â¸¡º÷
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                      RECE10-REC
           MOVE    WRK-PARA-HOSPID     TO  RECE10-HOSPID
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
           EVALUATE    WRK-PRTFLG
               WHEN    "1"
                   MOVE    WRK-CONS-SRYYM-200304
                                           TO  RECE10-SRYYM
                   MOVE    WRK-CONS-SRYYM-200604
                                           TO  RECE10-XXSRYYM
               WHEN    "2"
                   MOVE    WRK-CONS-SRYYM-200604
                                           TO  RECE10-SRYYM
                   MOVE    WRK-CONS-SRYYM-200610
                                           TO  RECE10-XXSRYYM
               WHEN    "3"
                   MOVE    WRK-CONS-SRYYM-200610
                                           TO  RECE10-SRYYM
                   MOVE    999999          TO  RECE10-XXSRYYM
           END-EVALUATE
           IF      WRK-PARA-HKNJANUM   NOT =   ZERO
               MOVE    WRK-PARA-HKNJANUM   TO  RECE10-XXSTHKNJANUM
                                               RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           END-IF 
      *
      *###
           DISPLAY "SYOKBN=" WRK-SYOKBN "#" WRK-PARA-SYOKBN1
                   " SRYYM=" RECE10-SRYYM "-" RECE10-XXSRYYM 
      *###
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH            TO  MCP-PATH
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE10
               MOVE    HIGH-VALUE      TO  KEY-NEW
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    WRK-PATH            TO  MCP-PATH
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECE10
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
      *        ¥Æ¥¹¥È´µ¼Ô¤Î¤È¤­¤ÏÆÉ¤ßÈô¤Ð¤¹
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    RECE10-HOSPID       TO  PTINF-HOSPID
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF   
      *         
               MOVE    RECE10-HKNJANUM-S   TO  KEY-N-HKNJANUM
               ADD     1                   TO  CNT-RECE10   
      *         
               DISPLAY "PTNUM=" RECE10-PTNUM 
      *         
           ELSE
               MOVE    HIGH-VALUE      TO  KEY-NEW
               MOVE    1               TO  FLG-RECE10
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá´ÉÍý£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ëÆÉ¹þ
      *****************************************************************
       900-UPD-FILE-READ-SEC             SECTION.
      *
           READ    UPD-FILE
               AT  END
                   MOVE    1           TO  FLG-UPDFILE
               NOT AT  END
                   MOVE    ZERO        TO  FLG-UPDFILE
           END-READ
           .
       900-UPD-FILE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA-REC
               IF          MCP-RC      =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SEIKYU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SEIKYU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SEIKYU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¥­¡¼¡Ë
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNJAINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "HKNJAINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "HKNJAINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-HKNJAINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £Ä£Â¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
