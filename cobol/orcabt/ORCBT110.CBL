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
       PROGRAM-ID.             ORCBT110.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æü¼¡Åý·×
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æü¼¡Åý·×¥Ç¡¼¥¿ÍÑ´ðËÜ¾ðÊóºîÀ®¡Ê³°Íè¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  05/03/31    NACL¡ÝÂ¿¡¹Ç¼  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.03.01    NACL-Æ£¸¶    06/09/27  £²£°£°£¶Ç¯£±£°·î²þÀµÂÐ±þ
      *  03.05.01    NACL-Æ£¸¶    07/04/19  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.01.01    NACL-¾®Æ¦Âô  07/11/16  ¸øÌ³ºÒ³²¡¢¸ø³²¤Î¼ïÊÌÈ½ÄêÄÉ²Ã
      *  04.01.01    NACL-¾®Æ¦Âô  08/02/05  ¥ì¥»¥×¥È¼ïÊÌ¥»¥Ã¥È½¤Àµ
      *
      *  04.02.01    NACL-Æ£¸¶    08/04/10  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *
      *  04.04.01    NACL-Æ£¸¶    09/02/26  Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤Ë£·£µºÐÅþ
      *                                     Ã£»þ¤Ë³¤³°¤Ë½»½ê¤òÍ­¤¹¤ë¾ì¹ç
      *                                     ¤ÎÂÐ±þ
      *  04.04.03    NACL-Æ£¸¶    10/08/23  ¸å´ü¹âÎðÈó³ºÅöÂÐ±þ
      *                                     ¡Ê¹âÎð¼õµë¼ÔÉ¼¤¬¤Ê¤¤¾ì¹ç¡Ë
      * 
      *  04.05.01    NACL-¾®Æ¦Âô  10/01/07  ¼£¸³¤Î¥ì¥»¥×¥È¼ïÊÌÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    Æü¼¡Åý·×¥Ç¡¼¥¿
           SELECT  TOUKEI04-FILE   ASSIGN  TOUKEI04PARA
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-TOUKEI04.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  TOUKEIERR-FILE  ASSIGN  TOUKEIERR
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    Æü¼¡Åý·×¥Ç¡¼¥¿
       FD  TOUKEI04-FILE.
       01  TOUKEI04-REC. 
           COPY    "CPTCF004.INC".
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC           PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI04PARA//.
      *
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TOUKEIERR//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-TOUKEI04                    PIC X(02).
           03  STS-TOUKEIERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-HKNCOMBI                    PIC 9(01). 
           03  FLG-HKNNUM                      PIC 9(01). 
           03  FLG-TOUKEI04                    PIC 9(01).
           03  FLG-PTHKNINF                    PIC 9(01).
           03  FLG-PTKOHINF                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-KYUSEIRRK                   PIC 9(01).
           03  FLG-MONTHLYNUM                  PIC 9(01).
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-SYUNOU                      PIC 9(01).
           03  FLG-PTRSIINF                    PIC 9(01).
           03  FLG-ADRS                        PIC 9(01).
           03  FLG-JYURRK                      PIC 9(01).
      * 
           03  FLG-SYORI                       PIC 9(01).
           03  FLG-CHK-END                     PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-TOUKEI04                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                             PIC 9(03). 
           03  IDX1                            PIC 9(03). 
           03  IDX2                            PIC 9(03). 
           03  IDX3                            PIC 9(03). 
           03  IDX4                            PIC 9(03). 
           03  IDX5                            PIC 9(03). 
           03  IDY                             PIC 9(03). 
           03  IDZ                             PIC 9(03).
           03  IDXX                            PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                  PIC 9(10).
               05  KEY-N-DENPNUM               PIC 9(07).
           03  KEY-OLD.
               05  KEY-O-PTID                  PIC 9(10).
               05  KEY-O-DENPNUM               PIC 9(07).
      *
      *    °ì»þÎÎ°è
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(32).
               05  WRK-PARA-HOSPID             PIC X(24).
               05  WRK-PARA-PREFNUM            PIC X(02). 
               05  WRK-PARA-SRYYMD             PIC X(08).
               05  WRK-PARA-NYUGAIKBN          PIC X(01).
               05  WRK-PARA-DATAKBN            PIC X(01).
               05  WRK-PARA-HOUKATUKBN         PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
      *
           03  WRK-TOUKEIERR               PIC X(200). 
           03  WRK-ERRMSG                  PIC X(300).
      *
           03  WRK-MCP-TABLE               PIC X(64).
           03  WRK-MCP-PATHNAME            PIC X(64).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN              PIC X(01).
               05  WRK-TEISEIKBN           PIC X(01).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *    ´µ¼ÔËè
           03  WRK-HENSYU-AREA.
               05  WRK-KIHONKBN            PIC 9(01).
               05  WRK-JIKANKBN            PIC 9(01).
               05  WRK-SYOKAIKBN           PIC 9(01).
      *
           03  WRK-HKNNUM                  PIC X(03).
           03  WRK-PAYKBN                  PIC X(02).
      *
      *    ¸å´ü¹âÎð¼ÔÈó³ºÅö¥Õ¥é¥°
           03  WRK-KOUKI-FLG           PIC 9(01).
      *
           03  WRK-TEKSTYMD.
               05  WRK-TEKSTYYMM       PIC 9(06).
               05  WRK-TEKSTDD         PIC 9(02).
      *
      *    ¥ì¥»¥×¥È¼ïÊÌÈ½ÄêÍÑÎÎ°è
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO                               PIC X(01).
           03  WK-IHO                                  PIC X(01).
           03  WK-RJN                                  PIC X(01).
           03  WK-KOHI-CNT                             PIC 9(02).
           03  WK-HONKZKKBN                            PIC X(01).
           03  WRK-AGE                     PIC 9(03).
           03  WRK-SYUGAKUKBN              PIC 9(01).
      *
           03  WRK-HKNNUM-AREA.
               05  WRK-HKNNUM-TBL          OCCURS   4.
      *            Ë¡ÊÌÈÖ¹æ
                   07  WRK-HKN-HBTNUM                PIC  X(02).
      *            ¥ì¥»¥×¥ÈÀÁµá¶èÊ¬
                   07  WRK-HKN-RECESKYKBN            PIC  X(01).
      *            ÊÝ¸±ÈÖ¹æ
                   07  WRK-HKN-HKNNUM                PIC  X(03).
      *
           03  WRK-1033-TABLE.
               05  WRK-1033-TBL    OCCURS  5.
                   07  WRK-1033-HKNJANUM-G.
                       09  WRK-1033-HKNJANUM
                                   PIC X(08) OCCURS  20.
                   07  WRK-1033-STYUKYMD
                                   PIC X(08).
                   07  WRK-1033-EDYUKYMD 
                                   PIC X(08).
      *
      *    ¿ÇÎÅ²Ê¥Æ¡¼¥Ö¥ë
       01  TBL-SYS-1005-AREA.
           03  TBL-SYS-1005-REC        OCCURS  99.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-RECESRYKA       PIC X(02).
           03  IDX-SRYKA-MAX            PIC 9(04).
      *
           COPY    "MCPAREA".
      *
      *    Æü¼¡Åý·×¥Ç¡¼¥¿ÊÝÂ¸
       01  HZN-TOUKEI04-REC. 
           COPY    "CPTCF004.INC"  REPLACING  //TOUKEI04-//
                                   BY         //HZN-TOUKEI04-//.
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯
           COPY    "CPTTNS.INC".
      *
       01  WRK-CONS-AREA.
      *    ¼«ÈñÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    Ï«ºÒÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    ¼«ÇåÀÕÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *    ¸¶ÇúÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".       
      *    ¸ø³²ÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-KOUGAI-HKNNUM PIC X(03)    VALUE   "975".       
      *
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.      
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
      *    °åÎÅµ¡´Ø¾ðÊó
           COPY  "CPSK1001.INC".
      *
      *    ´µ¼ÔÈÖ¹æ¹½À®´ÉÍý¾ðÊó
           COPY    "CPSK1009.INC".
      *     
      *    ¸©Æâ°·¤¤ÊÝ¸±¼Ô¾ðÊó
           COPY    "CPSK1033.INC".
      *
      *    ¿ÇÎÅ²Ê¾ðÊó
           COPY    "CPSK1005.INC".
      *     
           COPY    "CPSK2005.INC".
      *
      *    ´µ¼ÔÅÐÏ¿µ¡Ç½À©¸æ¾ðÊó
           COPY  "CPSK1017.INC".
      *
      *    ¼õ¿ÇÍúÎò
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ÊÝ¸±¾ðÊó
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    ´µ¼Ô¾ðÊó
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ¼ýÇ¼¾ðÊó
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ´µ¼ÔÏ«ºÒÊÝ¸±¾ðÊó
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    ½»½ê¥Þ¥¹¥¿
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    Æþ±¡²ñ·×
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    Æþ±¡²ñ·×ÊÔ½¸¥µ¥Ö
           COPY    "CPORCHCM34S01.INC".
      *
      *    ¸øÈñÉÕ²Ã¾ðÊó¼èÆÀ¥µ¥Ö
           COPY    "CPORCSKOHPLUS.INC".
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ¿ô»úÊÑ´¹ÎÎ°è
           COPY    "CPORCSNUM.INC".
      *   ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *   ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    ¥ì¥»¥×¥È¼ïÊÌÊÖµÑ¥µ¥Ö
           COPY    "CPORCSSYUBETUGET.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  TTNS-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-HOSPID
                                               WRK-PARA-PREFNUM 
                                               WRK-PARA-SRYYMD
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-HOUKATUKBN 
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               TOUKEIERR
           END-UNSTRING 
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBT110"      TO  JOB-PGID
           MOVE    "Æü¼¡Åý·×¥Ç¡¼¥¿ºîÀ®¡Ê³°Íè¡Ë"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    ¿ÇÎÅÇ¯·îÊÔ½¸
           MOVE    WRK-PARA-SRYYMD     TO  WRK-SYMD
      *
      *    °åÎÅµ¡´Ø£É£ÄÊÔ½¸
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ´µ¼ÔÈÖ¹æ¹½À®´ÉÍý¾ðÊóÊÔ½¸
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1009-STYUKYMD
                                           SYS-1009-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "´µ¼ÔÈÖ¹æ¹½À®´ÉÍý¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *     
      *    ¥ì¥»¥×¥ÈÊÔ½¸¶èÊ¬¾ðÊó
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-SYMD            TO  SYS-2005-STYUKYMD
                                           SYS-2005-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE               TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-RJNGNBKKBN =   SPACE
               MOVE    ZERO                TO  SYS-2005-RJNGNBKKBN
           END-IF
      *
      *    ´µ¼ÔÅÐÏ¿µ¡Ç½À©¸æ¾ðÊó
           INITIALIZE                      SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1017-STYUKYMD
                                           SYS-1017-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1017-HOSPNUM
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1017-REC
           ELSE    
               INITIALIZE                      SYS-1017-REC
           END-IF
           IF      SYS-1017-SEIHO-JKYSNUM
                                       =   SPACE
               MOVE    ZERO                TO  SYS-1017-SEIHO-JKYSNUM
           END-IF
      * 
      *    ¸©Æâ°·¤¤ÊÝ¸±¼Ô¾ðÊó
           MOVE    SPACE           TO  WRK-1033-TABLE
      *     
           MOVE    SPACE           TO  SYS-1033-REC
           INITIALIZE                  SYS-1033-REC
           MOVE    "1033"          TO  SYS-1033-KANRICD
           MOVE    "*"             TO  SYS-1033-KBNCD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1033-HOSPNUM
           MOVE    SYS-1033-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >=  5
               MOVE    MCPDATA-REC         TO  SYS-1033-REC
               ADD     1                   TO  IDX
               MOVE    SYS-1033-STYUKYMD   TO  WRK-1033-STYUKYMD   (IDX)
               MOVE    SYS-1033-EDYUKYMD   TO  WRK-1033-EDYUKYMD   (IDX)
               MOVE    SYS-1033-TBL        TO  WRK-1033-HKNJANUM-G (IDX)
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *    ¿ÇÎÅ²Ê¥Æ¡¼¥Ö¥ë
           INITIALIZE                  TBL-SYS-1005-AREA
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SYMD            TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >=  99
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               ADD     1                   TO  IDX
               MOVE    SYS-1005-KBNCD(1:2)
                                           TO  TBL-SRYKA  (IDX)
               MOVE    SYS-1005-RECESRYKA  TO  TBL-RECESRYKA  (IDX)
               MOVE    IDX                 TO  IDX-SRYKA-MAX
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    "TOKEI110"          TO  TOUKEI04PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI04PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI04PARA-HOSPNUM
      *
           OPEN    OUTPUT              TOUKEI04-FILE
           CLOSE                       TOUKEI04-FILE
      *
           OPEN    OUTPUT              TOUKEI04-FILE
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ¼õ¿ÇÍúÎòÆÉ¤ß¹þ¤ß
           INITIALIZE                  JYURRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JYURRK-HOSPNUM
           MOVE    WRK-PARA-SRYYMD     TO  JYURRK-SRYYMD
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM UNTIL       FLG-JYURRK  =   1
      *
               PERFORM 2001-INIT-SYORI-SEC
               MOVE    ZERO                TO  FLG-SYORI
               PERFORM UNTIL    (FLG-JYURRK    =   1  )  OR
                                (KEY-N-PTID    NOT =   KEY-O-PTID)
                   PERFORM 2002-HENSYU-SYORI-SEC
               END-PERFORM
               IF      FLG-SYORI               =   1
                   PERFORM 2003-OUT-SYORI-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ´µ¼ÔËè¤ÎÊÔ½¸½èÍý
      *****************************************************************
       2001-INIT-SYORI-SEC              SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
      *    ¥Æ¥¹¥È´µ¼Ô¤Î¤È¤­¤ÏÆÉ¤ßÈô¤Ð¤¹
           MOVE    JYURRK-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    JYURRK-PTID         TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "JYURRK PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
      *    ¥Æ¥¹¥È´µ¼Ô¤Î»þÆÉ¤ßÈô¤Ð¤·
           IF     (FLG-PTINF           =   ZERO )  AND
                  (PTINF-TSTPTNUMKBN   =   "1"  )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM UNTIL ( FLG-JYURRK      =   1  )
                       OR    ( KEY-N-PTID  NOT =   KEY-O-PTID)
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key29"             TO  MCP-PATHNAME
                   PERFORM 900-JYURRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
           END-IF
      *
           .
       2001-INIT-SYORI-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ÊÔ½¸½èÍý
      *****************************************************************
       2002-HENSYU-SYORI-SEC              SECTION.
      *
      *    ´µ¼Ô¾ðÊóÊÔ½¸
           PERFORM 20021-PTINF-HENSYU-SEC
      *
           MOVE    ZERO                TO  FLG-SYORI
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM UNTIL ( FLG-JYURRK      =   1  )
                   OR    ( KEY-NEW     NOT =   KEY-OLD)
               IF      JYURRK-DENPNUM      =   ZERO
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key29"             TO  MCP-PATHNAME
                   PERFORM 900-JYURRK-READ-SEC
               ELSE
                   PERFORM 20022-SYORI-01-SEC
               END-IF
           END-PERFORM
      *
           .
       2002-HENSYU-SYORI-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ´µ¼Ô¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       20021-PTINF-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      HZN-TOUKEI04-REC
           MOVE    WRK-PARA-HOSPID     TO  HZN-TOUKEI04-HOSPID
      *    °åÎÅµ¡´Ø¥³¡¼¥É
           MOVE    SYS-1001-HOSPCD     TO  HZN-TOUKEI04-HOSPCD
      *    ¿ÇÎÅÇ¯·îÆü
           MOVE    WRK-PARA-SRYYMD     TO  HZN-TOUKEI04-SRYYMD
      *    Æþ³°¶èÊ¬
           MOVE    2                   TO  HZN-TOUKEI04-NYUGAIKBN
      *    ´µ¼ÔÈÖ¹æ
           INITIALIZE                  ORCSPTIDAREA
           MOVE    JYURRK-HOSPNUM  TO  SPTID-HOSPNUM
           MOVE    JYURRK-PTID     TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           MOVE    SPTID-PTNUM     TO  HZN-TOUKEI04-PTNUM
           MOVE    JYURRK-PTID     TO  HZN-TOUKEI04-PTID
      *
      *    ´µ¼Ô¾ðÊó
           MOVE    PTINF-KANANAME  TO  HZN-TOUKEI04-KANANAME
           MOVE    PTINF-NAME      TO  HZN-TOUKEI04-NAME
           MOVE    PTINF-SEX       TO  HZN-TOUKEI04-SEX
           MOVE    PTINF-BIRTHDAY  TO  HZN-TOUKEI04-BIRTHDAY
           MOVE    PTINF-HOME-POST TO  HZN-TOUKEI04-HOME-POST
           MOVE    PTINF-HOME-ADRS TO  HZN-TOUKEI04-HOME-ADRS
           MOVE    PTINF-HOME-TEL1 TO  HZN-TOUKEI04-HOME-TEL
           INITIALIZE                  ADRS-REC
           MOVE    PTINF-HOME-POST TO  ADRS-POST
grpsys     MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
           MOVE    ADRS-REC        TO  MCPDATA-REC
           PERFORM 900-ADRS-INV-SEC
           MOVE    ADRS-LPUBCD     TO  HZN-TOUKEI04-LPUBCD
      *
           .
       20021-PTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥­¡¼ËèÊÔ½¸½èÍý
      *****************************************************************
       20022-SYORI-01-SEC              SECTION.
      *
           MOVE   HZN-TOUKEI04-REC     TO  TOUKEI04-REC
      *    ¥ì¥³¡¼¥É¼±ÊÌ
           IF      JYURRK-GRP-DENPNUM  =   ZERO
               MOVE    1                   TO  TOUKEI04-RECSKB
               MOVE    ZERO                TO  WRK-KIHONKBN
               MOVE    ZERO                TO  WRK-JIKANKBN
           ELSE
               IF      JYURRK-GRP-RENNUM   =   1
                   MOVE    1                   TO  TOUKEI04-RECSKB
                   MOVE    ZERO                TO  WRK-KIHONKBN
                   MOVE    ZERO                TO  WRK-JIKANKBN
               ELSE
                   MOVE    2                   TO  TOUKEI04-RECSKB
               END-IF
           END-IF
           MOVE    ZERO                TO  WRK-SYOKAIKBN
      *    ¿ÇÎÅ²Ê
           MOVE    JYURRK-SRYKA        TO  TOUKEI04-SRYKA
           PERFORM 20023-SRYKA-HENSYU-SEC
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE    JYURRK-HKNCOMBINUM  TO  TOUKEI04-HKNCOMBINUM
           PERFORM 20024-HKNCOMBI-HENSYU-SEC
      *    ¥É¥¯¥¿¡¼
           MOVE    JYURRK-DRCD         TO  TOUKEI04-DRCD
      *    ÅÁÉ¼ÈÖ¹æ
           MOVE    JYURRK-DENPNUM      TO  TOUKEI04-DENPNUM
      *    ¼ýÇ¼¾ðÊó
           PERFORM 20026-SYUNOU-HENSYU-SEC
      *    ¼õ¿ÇÍúÎò¤ÎÆâÍÆÈ½Äê
           PERFORM UNTIL    (FLG-JYURRK    =   1  )  OR
                            (KEY-NEW   NOT =   KEY-OLD)
      *        ¿Ç»¡ÎÁ»»Äê¤¢¤ê
               IF      JYURRK-SRYKBN1      NOT =   SPACE
                   PERFORM 200227-SRYACCT-HEN-SEC
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-SEC
           END-PERFORM
      *
      *    ½ÐÎÏ½èÍý
           PERFORM 2003-OUT-SYORI-SEC
           .
       20022-SYORI-01-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¸¡º÷½èÍý
      *****************************************************************
       200227-SRYACCT-HEN-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15  )  OR
                              (JYURRK-ZAINUM(IDX)  =   ZERO )
      *
               MOVE    SPACE                   TO  SRYACT-REC
               INITIALIZE                      SRYACT-REC
               MOVE    JYURRK-HOSPNUM          TO  SRY-HOSPNUM
               MOVE    JYURRK-NYUGAIKBN        TO  SRY-NYUGAIKBN
               MOVE    JYURRK-PTID             TO  SRY-PTID
               MOVE    JYURRK-SRYKA            TO  SRY-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)      TO  SRY-SRYYM
               MOVE    JYURRK-ZAINUM(IDX)      TO  SRY-ZAINUM
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACT-REC
                   MOVE    1               TO  FLG-SRYACT
               END-IF
               MOVE    ZERO                   TO  FLG-CHK-END
               PERFORM UNTIL  (FLG-SRYACT     =   1  )  OR
                              (FLG-CHK-END    =   1  )
                   EVALUATE    SRY-SRYKBN
                      WHEN     "11"
      *                   ½é¿Ç
                          PERFORM 2002271-SYOSIN-CHK11-SEC
                      WHEN     "12"
      *                   ºÆ¿Ç
                          PERFORM 2002272-SYOSIN-CHK12-SEC
                      WHEN     "13"
      *                   »ØÆ³
                          PERFORM 2002273-SYOSIN-CHK13-SEC
                      WHEN     "14"
      *                   ±ý¿Ç
                          PERFORM 2002273-SYOSIN-CHK14-SEC
                      WHEN     OTHER
                          MOVE     1              TO   FLG-CHK-END
                   END-EVALUATE
      *
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACT-READ-SEC
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-PERFORM
           .
       200227-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿ÇÎÁÈ½Äê½èÍý
      *****************************************************************
       2002271-SYOSIN-CHK11-SEC              SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5  )  OR
                              (SRY-SRYCD (IDY) =   SPACE )
               IF       SRY-SRYCD(IDY)(1:1)    =   "1"
                   MOVE    SRY-SRYCD(IDY)      TO  TNS-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   IF      TNS-DATAKBN         =   "1"
                       MOVE    1                   TO  WRK-KIHONKBN
                   ELSE
                       IF      TNS-TIMEKSNKBN  NOT =   ZERO
      *                    »þ´Ö³°²Ã»»È½Äê
                           PERFORM 2002271-TIME-CHK-SEC
                       ELSE
      *                    ¾Ò²ð²Ã»»È½Äê
                           PERFORM 2002272-SYOKAI-CHK-SEC
                       END-IF
                   END-IF
               ELSE
                   IF  (SRY-SRYCD(IDY)         =   "099110001") OR
                       (SRY-SRYCD(IDY)         =   "830000020")
                       MOVE    1                   TO  WRK-KIHONKBN
                   END-IF
               END-IF
           END-PERFORM
           .
       2002271-SYOSIN-CHK11-EXT.
           EXIT.
      *****************************************************************
      *    »þ´Ö³°¶èÊ¬È½Äê½èÍý
      *****************************************************************
       2002271-TIME-CHK-SEC              SECTION.
      *
           EVALUATE    TNS-TIMEKSNKBN
               WHEN    1
      *            »þ´Ö³°
                   MOVE    2               TO  WRK-JIKANKBN
               WHEN    2
               WHEN    3
      *            µÙÆü
                   MOVE    3               TO  WRK-JIKANKBN
               WHEN    4
      *            ¿¼Ìë
                   MOVE    4               TO  WRK-JIKANKBN
               WHEN    5
      *            »þ´Ö³°ÆÃÎã
                   MOVE    2               TO  WRK-JIKANKBN
           END-EVALUATE
           .
       2002271-TIME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾Ò²ð²Ã»»È½Äê½èÍý
      *****************************************************************
       2002272-SYOKAI-CHK-SEC              SECTION.
      *
           EVALUATE    SRY-SRYCD(IDY)
      *        ½é¿Ç»þ¡Ê¿ÇÎÅ½ê¡Ë¾Ò²ð´µ¼Ô²Ã»»
               WHEN    "111003770"
      *        ¾Ò²ð´µ¼Ô²Ã»»£³
               WHEN    "111009970"
      *        ¾Ò²ð´µ¼Ô²Ã»»£´
               WHEN    "111010070"
      *        ¾Ò²ð´µ¼Ô²Ã»»£µ
               WHEN    "111010170"
      *        ¾Ò²ð´µ¼Ô²Ã»»£¶
               WHEN    "111010270"
      *        ¾Ò²ð´µ¼Ô²Ã»»£±
               WHEN    "111011270"
      *        ¾Ò²ð´µ¼Ô²Ã»»£²
               WHEN    "111011370"
                   MOVE    1               TO  WRK-SYOKAIKBN
      *        ¾®»ù²Ê³°Íè¿ÇÎÅÎÁ¡Ê½é¿Ç»þ¿ÇÎÅ½ê¤Î¾Ò²ð´µ¼Ô¡Ë²Ã»»
               WHEN    "111010370"
      *        ¾®»ù²Ê³°Íè¿ÇÎÅÎÁ¡Ê¾Ò²ð´µ¼Ô²Ã»»
               WHEN    "111010870"
               WHEN    "111010970"
               WHEN    "111011070"
               WHEN    "111011170"
               WHEN    "113005070"
               WHEN    "113005170"
                   MOVE    1               TO  WRK-SYOKAIKBN
      *        ¿ÇÎÅ¾ðÊóÄó¶¡ÎÁ¡Ê£Â¡ËµÕ¾Ò²ð²Ã»»         
               WHEN    "113006370"
      *        ¿ÇÎÅ¾ðÊóÄó¶¡ÎÁ¡Ê£Ã¡ËµÕ¾Ò²ð²Ã»»         
               WHEN    "113006470"
                   MOVE    1               TO  WRK-SYOKAIKBN
      *        ½é¿Ç¡Ê¿ÇÎÅ½ê¡Ë¾Ò²ð´µ¼Ô²Ã»»
               WHEN    "111700470"
               WHEN    "111702370"
               WHEN    "111702470"
               WHEN    "111702570"
               WHEN    "111702670"
               WHEN    "111702770"
               WHEN    "111702870"
                   MOVE    1               TO  WRK-SYOKAIKBN
           END-EVALUATE
           .
       2002272-SYOKAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÆ¿ÇÎÁÈ½Äê½èÍý
      *****************************************************************
       2002272-SYOSIN-CHK12-SEC              SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5  )  OR
                              (SRY-SRYCD (IDY) =   SPACE )
               EVALUATE    SRY-SRYCD(IDY)
      *            ºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë
                   WHEN    "112007410"
      *            ÅÅÏÃºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë
                   WHEN    "112007950"
      *            ºÆ¿ÇÎÁ¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë
                   WHEN    "112009210"
      *            ÅÅÏÃºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë
                   WHEN    "112009750"
                       MOVE    2                   TO  WRK-KIHONKBN
      *
      *            Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë
                   WHEN    "112008350"
      *            ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë
                   WHEN    "112008850"
      *            Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë
                   WHEN    "112010150"
      *            ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë
                   WHEN    "112010650"
                       MOVE    3                   TO  WRK-KIHONKBN
      *
      *            ºÆ¿ÇÎÁ¡ÊÏ·¿Í ÉÂ±¡¡Ë
                   WHEN    "112702310"
      *            ÅÅÏÃºÆ¿ÇÎÁ¡ÊÏ·¿Í ÉÂ±¡¡Ë
                   WHEN    "112702750"
      *            ºÆ¿ÇÎÁ¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë
                   WHEN    "112703710"
      *            ÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë
                   WHEN    "112704150"
                       MOVE    2                   TO  WRK-KIHONKBN
      *
      *            Æ±ÆüºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë
                   WHEN    "112703050"
      *            ÅÅÏÃÆ±ÆüºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë
                   WHEN    "112703450"
      *            Æ±ÆüºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë
                   WHEN    "112704450"
      *            ÅÅÏÃÆ±ÆüºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë
                   WHEN    "112704850"
                       MOVE    3                   TO  WRK-KIHONKBN
      *
      *            ³°Íè¿ÇÎÅÎÁ
                   WHEN    "112011310"
      *            ³°Íè¿ÇÎÅÎÁ¡ÊÏ·¿Í¡Ë
                   WHEN    "112705510"
                       MOVE    2                   TO  WRK-KIHONKBN
      *
      *            Æ±Æü³°Íè¿ÇÎÅÎÁ
                   WHEN    "112011710"
      *            Æ±Æü³°Íè¿ÇÎÅÎÁ¡ÊÏ·¿Í¡Ë
                   WHEN    "112705850"
                       MOVE    2                   TO  WRK-KIHONKBN
      *
      *            Ï«ºÒºÆ¿ÇÎÁ
                   WHEN    "101120040"
                       MOVE    2                   TO  WRK-KIHONKBN
      *
      *            Â¾²Ê¼õ¿Ç
                   WHEN    "830000021"
                       IF      JYURRK-GRP-RENNUM   =   1  OR
                                                       ZERO
                           MOVE    2               TO  WRK-KIHONKBN
                       END-IF
      *
                   WHEN    OTHER
                       IF       SRY-SRYCD(IDY)(1:1)    =   "1"
                           MOVE    SRY-SRYCD(IDY)      TO  TNS-SRYCD
                           PERFORM 900-TENSU-READ-SEC
                           IF     (TNS-DATAKBN         =   "2" ) AND
                                  (TNS-TIMEKSNKBN  NOT =   ZERO)
      *                        »þ´Ö³°È½Äê
                               PERFORM 2002271-TIME-CHK-SEC
                           END-IF
                       END-IF
               END-EVALUATE
           END-PERFORM
           .
       2002272-SYOSIN-CHK12-EXT.
           EXIT.
      *****************************************************************
      *    »ØÆ³ÎÁÈ½Äê½èÍý
      *****************************************************************
       2002273-SYOSIN-CHK13-SEC              SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5  )  OR
                              (SRY-SRYCD (IDY) =   SPACE )
               EVALUATE    SRY-SRYCD(IDY)
      *            ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(±¡³°½èÊý ½é¿Ç)
                   WHEN    "113003510"
      *            ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(±¡Æâ½èÊý ½é¿Ç)
                   WHEN    "113003710"
                       MOVE    1                   TO  WRK-KIHONKBN
      *
      *            ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(±¡³°½èÊý ºÆ¿Ç)
                   WHEN    "113003610"
      *            ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(±¡Æâ½èÊý ºÆ¿Ç½é¿Ç)
                   WHEN    "113003610"
                       MOVE    2                   TO  WRK-KIHONKBN
               END-EVALUATE
           END-PERFORM
           .
       2002273-SYOSIN-CHK13-EXT.
           EXIT.
      *****************************************************************
      *    ±ý¿ÇÈ½Äê½èÍý
      *****************************************************************
       2002273-SYOSIN-CHK14-SEC              SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5  )  OR
                              (SRY-SRYCD (IDY) =   SPACE )
               EVALUATE    SRY-SRYCD(IDY)
      *            ±ý¿Ç
                   WHEN    "114000110"
      *            ÆÃÊÌ±ý¿Ç
                   WHEN    "114001610"
      *            ºßÂð´µ¼ÔË¬Ìä¿Ç»¡ÎÁ
                   WHEN    "114001110"
      *                ±ý¿Ç¶èÊ¬
                       MOVE    1                   TO  TOUKEI04-OSINKBN
               END-EVALUATE
           END-PERFORM
           .
       2002273-SYOSIN-CHK14-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ÐÎÏ½èÍý
      *****************************************************************
       2003-OUT-SYORI-SEC              SECTION.
      *
      *    ´ðËÜÎÁ¶èÊ¬
           MOVE    WRK-KIHONKBN        TO  TOUKEI04-KIHONKBN
      *    »þ´Ö³°¶èÊ¬
           MOVE    WRK-JIKANKBN        TO  TOUKEI04-TIMEKBN
      *    ¾Ò²ð¶èÊ¬
           MOVE    WRK-SYOKAIKBN       TO  TOUKEI04-SHOUKAIKBN
      *
           WRITE   TOUKEI04-REC
      *
           IF      STS-TOUKEI04        =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-TOUKEIERR
               STRING "Æü¼¡Åý·×¥Ç¡¼¥¿ ½ñ¤­¹þ¤ß¥¨¥é¡¼ STS="
                                               DELIMITED  BY  SIZE
                       STS-TOUKEI04            DELIMITED  BY  SIZE
                                               INTO    WRK-TOUKEIERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-TOUKEI04
           .
       2003-OUT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÊÔ½¸½èÍý
      *****************************************************************
       20023-SRYKA-HENSYU-SEC              SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   IDX-SRYKA-MAX
               IF      JYURRK-SRYKA    =   TBL-SRYKA(IDX2)
                   MOVE    TBL-RECESRYKA (IDX2)
                                           TO  TOUKEI04-RECEKA
                   MOVE    IDX-SRYKA-MAX   TO  IDX2
               END-IF
           END-PERFORM
           .
       20023-SRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
      *****************************************************************
       20024-HKNCOMBI-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    JYURRK-HOSPNUM      TO  COMB-HOSPNUM
           MOVE    JYURRK-PTID         TO  COMB-PTID
           MOVE    JYURRK-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM 910-HKNCOMBI-INV-SEC
           IF      FLG-HKNCOMBI        =   1
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *    ¼çÊÝ¸±¾ðÊóÆÉ¤ß½Ð¤·
           INITIALIZE                      PTHKNINF-REC
           INITIALIZE                      PTRSIINF-REC
           IF      COMB-HKNID      NOT =   ZERO
               IF      COMB-HKNNUM         =   WRK-CONS-ROUSAI-HKNNUM
                                           OR  WRK-CONS-JIBAI-HKNNUM
      *            Ï«ºÒ¡¦¼«ÇåÀÕ
                   INITIALIZE                  PTRSIINF-REC
                   MOVE    COMB-HOSPNUM        TO  PTRSI-HOSPNUM
                   MOVE    COMB-HKNID          TO  PTRSI-HKNID
                   MOVE    COMB-PTID           TO  PTRSI-PTID
                   MOVE    PTRSIINF-REC        TO  MCPDATA-REC
                   PERFORM 900-PTRSIINF-INV-SEC
               ELSE
                   INITIALIZE                  PTHKNINF-REC
                   MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
                   MOVE    COMB-PTID           TO  PTHKN-PTID
                   MOVE    COMB-HKNID          TO  PTHKN-HKNID
                   MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                                                   PTHKN-TEKEDYMD
                   MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                   PERFORM 910-PTHKNINF-INV-SEC
               END-IF
           END-IF
      *    ÊÝ¸±¼ÔÌ¾
           MOVE    PTHKN-HKNJANUM      TO  TOUKEI04-HKNJANUM
      *    ¼çÊÝ¸±
           MOVE    COMB-HKNNUM         TO  TOUKEI04-HKNNUM
           MOVE    COMB-HKNID          TO  TOUKEI04-HKNID
           MOVE    COMB-HONKZKKBN      TO  TOUKEI04-HONKZKKBN
           MOVE    COMB-HOJOKBN        TO  TOUKEI04-HOJOKBN
           MOVE    COMB-CONTKBN        TO  TOUKEI04-CONTKBN
      *    ¸øÈñ£±
           MOVE    COMB-KOH1ID         TO  TOUKEI04-KOH1ID
           MOVE    COMB-KOH1HKNNUM     TO  TOUKEI04-KOH1HKNNUM
           MOVE    COMB-KOH1PAYKBN     TO  TOUKEI04-KOH1PAYKBN
      *    ¸øÈñ£²
           MOVE    COMB-KOH2ID         TO  TOUKEI04-KOH2ID
           MOVE    COMB-KOH2HKNNUM     TO  TOUKEI04-KOH2HKNNUM
           MOVE    COMB-KOH2PAYKBN     TO  TOUKEI04-KOH2PAYKBN
      *    ¸øÈñ£³
           MOVE    COMB-KOH3ID         TO  TOUKEI04-KOH3ID
           MOVE    COMB-KOH3HKNNUM     TO  TOUKEI04-KOH3HKNNUM
           MOVE    COMB-KOH3PAYKBN     TO  TOUKEI04-KOH3PAYKBN
      *    ¸øÈñ£´
           MOVE    COMB-KOH4ID         TO  TOUKEI04-KOH4ID
           MOVE    COMB-KOH4HKNNUM     TO  TOUKEI04-KOH4HKNNUM
           MOVE    COMB-KOH4PAYKBN     TO  TOUKEI04-KOH4PAYKBN
      *    Ï«ºÒ¡¦¼«ÇåÀÕ
           IF      COMB-HKNNUM         =   WRK-CONS-ROUSAI-HKNNUM
                                       OR  WRK-CONS-JIBAI-HKNNUM
      *        Ï«ºÒ¡ÝÏ«ºÒÊÝ¸±¶èÊ¬
               MOVE    PTRSI-HKNKBN        TO  TOUKEI04-RSIHKNKBN
      *        Ï«ºÒ¡Ý¼«ÇåÀÕÀÁµá¶èÊ¬
               MOVE   PTRSI-JIBAISEIKBN    TO  TOUKEI04-JIBAISEIKBN
           END-IF
      *    ¥ì¥»¥×¥È¼ïÊÌ
           IF      FLG-HKNCOMBI        =   ZERO
               PERFORM 20021-RECESYUBETU-SET-SEC
           END-IF
           .
       20024-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       20026-SYUNOU-HENSYU-SEC              SECTION.
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿¸¡º÷
           INITIALIZE                  SYUNOU-REC
           MOVE    JYURRK-HOSPNUM      TO  SYU-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  SYU-PTID
           MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC                =   ZERO
               MOVE    "tbl_syunou"          TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    ÅÁÉ¼¾õÂÖ¶èÊ¬
           MOVE    SYU-DENPJTIKBN      TO  TOUKEI04-DENPJTIKBN
      *    ÅÁÉ¼ºîÀ®¶èÊ¬
           MOVE    SYU-CREATEKBN       TO  TOUKEI04-CREATEKBN
      *    ÊÝ¸±ÅÀ¿ô
           MOVE    SYU-TOTAL-HKNTEN    TO  TOUKEI04-HKNTEN
      *    ÊÝ¸±ÉéÃ´¶â³Û
           COMPUTE TOUKEI04-HKNMONEY   =   SYU-TOTAL-MONEY
                                       +   SYU-RJN-FTN
                                       +   SYU-KOH-FTN
      *    ÊÝ¸±Å¬ÍÑ³°¶â³Û
           COMPUTE TOUKEI04-TGMONEY    =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       -   SYU-TGMONEY-TAX-SAI
      *    ÊÝ¸±Å¬ÍÑ³°¶â³Û¡Ê¾ÃÈñÀÇ¤¢¤ê¡Ë
           MOVE    SYU-TGMONEY-TAX-SAI
                                       TO  TOUKEI04-TGMONEY-TAX
      *    ¼«Èñ¶â³Û 
           COMPUTE TOUKEI04-JIHI       =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
      *    ¼«Èñ¶â³Û¾ÃÈñÀÇ
           MOVE    SYU-JIHI-TAX        TO  TOUKEI04-JIHI-TAX
      *    ÀÁµá¶â³Û
           MOVE    SYU-SKYMONEY        TO  TOUKEI04-SKYMONEY
      *    Ï«ºÒ¡¿¼«ÇåÀÕ¤¬¤¢¤ë¾ì¹ç¤ÎÀÁµá³Û
           IF      SYU-RSIJIBAI-SKYMONEY   NOT =  ZERO
               MOVE   SYU-RSIJIBAI-SKYMONEY  TO  TOUKEI04-SKYMONEY
           END-IF
           .
       20026-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È¼ïÊÌ¥»¥Ã¥È½èÍý
      *****************************************************************
       20021-RECESYUBETU-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WK-RECESYUBETU-AREA
           INITIALIZE                      WK-RECESYUBETU-AREA
      *
      *    Ï«ºÒ¡¦¼«ÇåÀÕ¤Î¤È¤­
           IF      TOUKEI04-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM
               EVALUATE    TOUKEI04-RSIHKNKBN
                                       ALSO    PTRSI-SAIGAIKBN
                                           ALSO    WRK-PARA-NYUGAIKBN
                   WHEN    "1"         ALSO    "1"     ALSO    "1"   
                       MOVE    7081        TO    TOUKEI04-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "1"
                       MOVE    7082        TO    TOUKEI04-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "1"
                       MOVE    7101        TO    TOUKEI04-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "1"
                       MOVE    7102        TO    TOUKEI04-RECESYUBETU
                   WHEN    "1"         ALSO    "1"     ALSO    "2"   
                       MOVE    7071        TO    TOUKEI04-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "2"
                       MOVE    7072        TO    TOUKEI04-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "2"
                       MOVE    7091        TO    TOUKEI04-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "2"
                       MOVE    7092        TO    TOUKEI04-RECESYUBETU
                   WHEN    "3"         ALSO    ANY     ALSO    ANY
                       MOVE    7021        TO    TOUKEI04-RECESYUBETU
                   WHEN    "4"         ALSO    ANY     ALSO    ANY
                       MOVE    7001        TO    TOUKEI04-RECESYUBETU
      *            ¸øÌ³ºÒ³²
                   WHEN    "5"         ALSO    ANY     ALSO    "1"
                       MOVE    7061        TO    TOUKEI04-RECESYUBETU
                   WHEN    "5"         ALSO    ANY     ALSO    "2"
                       MOVE    7051        TO    TOUKEI04-RECESYUBETU
               END-EVALUATE
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    ¼«Èñ¤Î¤È¤­
           IF     TOUKEI04-HKNNUM(1:2) =   "98"
               MOVE    8888                TO  TOUKEI04-RECESYUBETU
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    ¼£¸³¤Î¤È¤­
           IF     WRK-PARA-SRYYMD      >=  "20080401"
               IF    (TOUKEI04-HKNNUM  >=   "900")   AND
                     (TOUKEI04-HKNNUM  <=   "919")
                   MOVE    8888                TO  TOUKEI04-RECESYUBETU
                   GO  TO  20021-RECESYUBETU-SET-EXT
               END-IF
           END-IF
      *
      *    ¸ø³²ÊÝ¸±¡ÊÆþ±¡¡Ë¤Î¤È¤­
           IF     TOUKEI04-HKNNUM     =    WRK-CONS-KOUGAI-HKNNUM
               MOVE    7200                TO  TOUKEI04-RECESYUBETU
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    Ç¯Îð·×»»
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    TOUKEI04-PTID       TO  AGECHK-PTID
           MOVE    WRK-PARA-SRYYMD     TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-RC           =   ZERO
               MOVE    AGECHK-NENREI       TO  WRK-AGE
               MOVE    AGECHK-SYUGAKUKBN   TO  WRK-SYUGAKUKBN
           ELSE
               MOVE    ZERO                TO  WRK-AGE
                                               WRK-SYUGAKUKBN
           END-IF
      *
      *    ¸å´ü¹âÎðÈó³ºÅö¤ÎÍ­ÌµÀßÄê
           MOVE    ZERO            TO  WRK-KOUKI-FLG
      *
           IF    ( WRK-AGE               >=  70                    )
           AND   ( WRK-PARA-SRYYMD (1:6) >=  WRK-CONS-SRYYM-200804 )
               IF    ( COMB-HKNNUM       =   SPACE   OR  "040"   
                                           OR  "039"   OR  "067"
                                           OR  "069"             )
               OR    ( COMB-HKNNUM (1:2)   =   "98"    OR  "90"
                                           OR  "91"              )
                   CONTINUE
               ELSE
                   IF   (  COMB-HKNNUM         =   "060"   OR  "068" )
                   OR   (( COMB-HKNNUM     NOT =   "060"   AND "068" )
                           AND
                         ( COMB-HOJOKBN        =   SPACE             ))
                       MOVE    WRK-PARA-SRYYMD (1:6)   TO  WRK-TEKSTYYMM
                       IF      COMB-TEKSTYMD (1:6)  >=
                                               WRK-PARA-SRYYMD (1:6)
                            MOVE  COMB-TEKSTYMD (7:2)  TO  WRK-TEKSTDD
                       ELSE
                            MOVE  01                   TO  WRK-TEKSTDD
                       END-IF
      *
                       INITIALIZE                      PTKOHINF-REC
                       MOVE    WRK-PARA-HOSPNUM    TO  PTKOH-HOSPNUM
                       MOVE    COMB-PTID           TO  PTKOH-PTID
                       MOVE    "976"               TO  PTKOH-KOHNUM
                       MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key4"              TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                           MOVE    "key4"              TO  MCP-PATHNAME
                           PERFORM 900-PTKOHINF-READ-N-SEC
                           PERFORM UNTIL   FLG-PTKOHINF    =   1
                               IF    (PTKOH-TEKSTYMD   <=  WRK-TEKSTYMD)
                               AND   (WRK-TEKSTYMD     <=
                                                       PTKOH-TEKEDYMD  )
                                   MOVE    1           TO  WRK-KOUKI-FLG
                                   MOVE    1           TO  FLG-PTKOHINF
                               ELSE
                                   MOVE    "tbl_ptkohinf"
                                                    TO  MCP-TABLE
                                   MOVE    "key4"   TO  MCP-PATHNAME
                                   PERFORM  900-PTKOHINF-READ-N-SEC
                               END-IF
                           END-PERFORM
                       END-IF
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM  900-CLOSE-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      TOUKEI04-HKNNUM     NOT =   SPACE
               EVALUATE    TOUKEI04-HKNNUM
                   WHEN    "060"
                   WHEN    "067"
                   WHEN    "068"
                   WHEN    "069"
                       IF      TOUKEI04-HKNNUM =   "060"   OR   "068"
                           MOVE   "1"      TO  WK-KOKUHO
                       ELSE
                           MOVE   "2"      TO  WK-KOKUHO
                       END-IF
                   WHEN    "039"
                   WHEN    "040"
                       MOVE   "3"          TO  WK-KOKUHO
                   WHEN    OTHER
                       MOVE    "1"         TO  WK-IHO
               END-EVALUATE
           END-IF
      *
           INITIALIZE                  WRK-HKNNUM-AREA
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   4  )  OR
                              (COMB-KOHID(IDX)   =   ZERO)
               MOVE    COMB-KOHHKNNUM(IDX)     TO  WRK-HKNNUM
               MOVE    COMB-KOHPAYKBN(IDX)     TO  WRK-PAYKBN
               PERFORM 200211-HKNNUM-SET-SEC
               MOVE    HKN-HBTNUM          TO  WRK-HKN-HBTNUM(IDX)
               MOVE    HKN-RECESKYKBN      TO  WRK-HKN-RECESKYKBN(IDX)
               MOVE    HKN-HKNNUM          TO  WRK-HKN-HKNNUM(IDX)
           END-PERFORM
      *
           IF      WRK-PARA-SRYYMD(1:6)    <      "200210" 
               IF      COMB-KOH1HKNNUM     =   "027"
                   MOVE        "1"             TO  WK-RJN
               END-IF
           END-IF
      *
           MOVE    TOUKEI04-HONKZKKBN  TO  WK-HONKZKKBN
      *
           MOVE    ZERO                TO  WK-KOHI-CNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
      *        ¸øÈñ¿ô
               IF     (WRK-HKN-HKNNUM (IDX)   NOT =   SPACE) AND
                      (WRK-HKN-HKNNUM (IDX)   NOT =   "027") AND
                      (WRK-HKN-HKNNUM (IDX)   NOT =   "972") AND
                      (WRK-HKN-HKNNUM (IDX)   NOT =   "974")
                   ADD     1               TO  WK-KOHI-CNT
               END-IF
           END-PERFORM
      *
           IF  TOUKEI04-HKNNUM     NOT =   SPACE
               IF      WRK-PARA-SRYYMD(1:6)    >=  "200210"
      *            £³ºÐÌ¤Ëþ¤Î¾ì¹ç¡¢£×£Ë¡Ý£È£Ï£Î£Ë£Ú£Ë£Ë£Â£Î¤Ë"£³"¤ò¥»¥Ã¥È
                   IF      WRK-AGE             <     3
                       MOVE    "3"             TO   WK-HONKZKKBN
                   END-IF
               END-IF
      *        Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
               IF      WRK-PARA-SRYYMD(1:6)    >=  WRK-CONS-SRYYM-200804
                   IF      WRK-SYUGAKUKBN      =   1
                       MOVE   "3"              TO  WK-HONKZKKBN
                   END-IF
               END-IF
      * 
               IF      WRK-PARA-SRYYMD(1:6)    >=  WRK-CONS-SRYYM-200804
                   IF    ( COMB-HKNNUM     =   "002" )
                   AND  ( WRK-AGE          >=  75    )
                       EVALUATE    COMB-HOJOKBN
                           WHEN    "A"
                           WHEN    "G"
                               MOVE    "1"     TO  COMB-HOJOKBN
                           WHEN    "B"
                           WHEN    "H"
                               MOVE    "2"     TO  COMB-HOJOKBN
                           WHEN    "C"
                           WHEN    "I"
                               MOVE    "3"     TO  COMB-HOJOKBN
                       END-EVALUATE
                   END-IF
      *
                   EVALUATE    COMB-HKNNUM
                       WHEN    "002" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
      *********************IF  (WRK-AGE  >=   70)  AND
      *********************    (WRK-AGE  <=   74)
                           IF  (WRK-AGE  >=   70)
                           AND (WRK-KOUKI-FLG
                                         =    0 )
                               EVALUATE   COMB-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                   WHEN   "3"
                                   WHEN   "6"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    "039" 
                       WHEN    "040" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "1"
                                   MOVE   "9"   TO   WK-RJN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-RJN
                           END-EVALUATE
                       WHEN    "067" 
                       WHEN    "069" 
                           IF      WRK-AGE       >=   65
                               MOVE   "?"   TO   WK-HONKZKKBN
                           ELSE
                               EVALUATE   COMB-HOJOKBN
                                   WHEN   "9"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "8"
                                       MOVE   "?"   TO   WK-HONKZKKBN
                                   WHEN   "7"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF
                       WHEN    OTHER 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                     END-EVALUATE
      *
                   IF      COMB-KOH1HKNNUM      =    "027"
                       MOVE   "?"               TO   WK-RJN
                   END-IF
               END-IF
      *
               IF    ( WRK-PARA-SRYYMD(1:6)  >= "200610"              )
               AND   ( WRK-PARA-SRYYMD(1:6)  <  WRK-CONS-SRYYM-200804 )
                   EVALUATE    COMB-HKNNUM
                       WHEN    "002" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF  (WRK-AGE  >=   70)  AND
                               (WRK-AGE  <=   74)
                             EVALUATE    WRK-PARA-NYUGAIKBN
                               WHEN    "1"
                                   EVALUATE   COMB-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "5"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "6"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                               WHEN    "2"
                                   EVALUATE   COMB-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "4"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "5"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                       WHEN   "6"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                             END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      COMB-KOH1HKNNUM      =    "027"
                       EVALUATE   COMB-KOH1PAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN                     
                           WHEN   "02"
                               MOVE   "?"   TO   WK-RJN
                           WHEN   "03"
                               MOVE   "7"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
      *
               IF    ( WRK-PARA-SRYYMD(1:6)
                                           >=  "200210" )
               AND   ( WRK-PARA-SRYYMD(1:6)
                                           <=  "200609" )
      *
      *            £±³ä¡¢£²³äÉéÃ´¤Î¹âÎð¼Ô¤Î¾ì¹ç£×£Ë¡Ý£È£Ï£Î£Ë£Ú£Ë£Ë£Â£Î¤Ë
      *            "£¹"¤Þ¤¿¤Ï"£¸"¤ò¥»¥Ã¥È
      *            Ï·¿ÍÊÝ¸±¤Î¾ì¹ç¤Ï£×£Ë¡Ý£Ò£Ê£Î¤Ë¥»¥Ã¥È¤¹¤ë	
                   EVALUATE    COMB-HKNNUM
                       WHEN    "002" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF  (WRK-AGE  >=   70)  AND
                               (WRK-AGE  <=   74)
                               EVALUATE   COMB-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      COMB-KOH1HKNNUM      =    "027"
                       EVALUATE   COMB-KOH1PAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN                     
                           WHEN   "02"
                               MOVE   "8"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )
           AND   ( TOUKEI04-HONKZKKBN  =   "1"   )
               IF      TOUKEI04-HKNNUM =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF    
      *
           INITIALIZE                  ORCSSYUBETUGETAREA  
           MOVE    WK-KOKUHO           TO  LNK-SYUBETUGET-KOKUHO
           MOVE    WK-IHO              TO  LNK-SYUBETUGET-IHO
           MOVE    WK-RJN              TO  LNK-SYUBETUGET-RJN
           MOVE    WK-KOHI-CNT         TO  LNK-SYUBETUGET-KOHI-CNT
           MOVE    WK-HONKZKKBN        TO  LNK-SYUBETUGET-HONKZKKBN
           MOVE    TOUKEI04-NYUGAIKBN  TO  LNK-SYUBETUGET-NYUGAIKBN
           MOVE    WRK-PARA-SRYYMD(1:6)
                                       TO  LNK-SYUBETUGET-SRYYM
           MOVE    TOUKEI04-HKNNUM     TO  LNK-SYUBETUGET-HKNNUM
           CALL    "ORCSSYUBETUGET"    USING   ORCSSYUBETUGETAREA
      *
           MOVE    LNK-SYUBETUGET-RECESYUBETU
                                        TO  TOUKEI04-RECESYUBETU
      *
      *    DISPLAY "TOUKEI04-RECESYUBETU=" TOUKEI04-RECESYUBETU
           .
       20021-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿½èÍý
      *****************************************************************
       200211-HKNNUM-SET-SEC                SECTION.
      *
           INITIALIZE                      HKNNUM-REC
           MOVE    COMB-HOSPNUM        TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN   
           MOVE    WRK-SYMD            TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC                =   ZERO
               MOVE    "tbl_hknnum"            TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNNUM
                   MOVE    MCPDATA-REC         TO  HKNNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNNUM
                   INITIALIZE                      HKNNUM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
               INITIALIZE                      HKNNUM-REC
           END-IF
      *
           MOVE    "tbl_hknnum"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200211-HKNNUM-SET-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   TOUKEIERR-FILE
           IF      STS-TOUKEIERR         =   ZERO
               CLOSE   TOUKEIERR-FILE
           ELSE
               OPEN    OUTPUT              TOUKEIERR-FILE
      *
               MOVE    WRK-TOUKEIERR         TO  TOUKEIERR-REC
               WRITE   TOUKEIERR-REC
               CLOSE   TOUKEIERR-FILE
      *         
      *        ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-TOUKEIERR   TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼»þ½ªÎ»½èÍý
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBT110 "         DELIMITED  BY  SIZE
                   WRK-TOUKEIERR       DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   TOUKEI04-FILE
      *
           IF      CNT-TOUKEI04           =   ZERO  
               MOVE    "½èÍýÂÐ¾Ý¤Î¥Ç¡¼¥¿¤¬¤¢¤ê¤Þ¤»¤ó¤Ç¤·¤¿"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TOUKEI04  CNT-TOUKEI04:" CNT-TOUKEI04
           DISPLAY "*** ORCBT110 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¡£Ò£Å£Á£Ä
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
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
      *    ¼õ¿ÇÍúÎò¥Þ¥¹¥¿£Ò£Å£Á£Ä
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-JYURRK
               MOVE    MCPDATA-REC         TO  JYURRK-REC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-PTID         TO  KEY-N-PTID
               MOVE    JYURRK-DENPNUM      TO  KEY-N-DENPNUM
           END-IF
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ôÆÉ¹þ
      *****************************************************************
       900-TENSU-READ-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    WRK-PARA-SRYYMD     TO  TTNS-CHK-YMD
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    ÂàÈò¥ï¡¼¥¯¤ËÂ¸ºß¤·¤Ê¤¤¾ì¹ç¤Ï£Ä£Â¤ò¸¡º÷
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
grpsys         MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
grpsys             PERFORM 900-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                       MOVE    ZERO            TO  FLG-TENSU
      *
      *                ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¤ËÊÔ½¸¤¹¤ë
                       PERFORM 9001-TTNS-HEN-SEC
                   ELSE
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    1                   TO  FLG-TENSU
                   END-IF
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
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
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯ÊÔ½¸½èÍý
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
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
      *    ½»½ê¥Þ¥¹¥¿¸¡º÷
      *****************************************************************
       900-ADRS-INV-SEC            SECTION.
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ADRS-REC
                   MOVE    ZERO                TO  FLG-ADRS
               ELSE
                   INITIALIZE                      ADRS-REC
                   MOVE    1                   TO  FLG-ADRS
               END-IF
           ELSE
               INITIALIZE                  ADRS-REC
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
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
      *    ¼ýÇ¼ÆÉ¹þ½èÍý
      *****************************************************************
       980-SYUNOU-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÏ«ºÒÊÝ¸±¾ðÊó¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       900-PTRSIINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
                   IF      PTRSI-SAKUJOKBN     =   "1"
                       MOVE    1                   TO  FLG-PTRSIINF
                   END-IF
               ELSE
                    INITIALIZE                 PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTRSIINF-INV-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÊÝ¸±¾ðÊó¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key3"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"     TO  MCP-TABLE
               MOVE    "key3"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key3 "            TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊóÆÉ¹þ
      *****************************************************************
       900-PTKOHINF-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               IF      PTKOH-SAKJOKBN      =   "1"
                   GO  TO  900-PTKOHINF-READ-N-SEC
               END-IF
               MOVE    ZERO                TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ½èÍý
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
