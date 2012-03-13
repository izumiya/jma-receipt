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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSODRNACCT.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÅÅ»Ò¥«¥ë¥ÆÆþ±¡²ñ·×ºîÀ®
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  02.07.01    NACL-¾®Æ¦Âô  05.12.06  Æþ±¡ÅÐÏ¿»þ¤Î¿©»ö¤È¿©Æ²²Ã»»¤Î
      *                                     ¥«¥ì¥ó¥À¡¼¥¯¥ê¥¢ÂÐ±þ¤È¡¢Ä«¿©Í¥Àè
      *                                     ¤Î¾ò·ïÄÉ²Ã
      *  02.07.02    NACL-¾®Æ¦Âô  05.12.12  Å¾²Ê¡¢Å¾Åï¡¢Å¾¼¼ÂÐ±þ
      *  02.07.03    NACL-¾®Æ¦Âô  05.12.13  Æþ±¡¼è¾ÃÂÐ±þ
      *  03.05.00    NACL-ÂÀÅÄ    07/05/28  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.03.00    NACL-¾®Æ¦Âô  08/08/11  ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¤ò¥µ¥Ö¥×¥í¤ËÊÑ¹¹
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ÆþÂà±¡ÅÐÏ¿¶¦ÄÌÎÎ°è
           COPY    "I0COMMON-SPA".
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-ORCSDAY                 PIC 9(01).
           03  FLG-NYUKSN                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDX10                       PIC 9(05).
           03  IDX11                       PIC 9(05).
           03  IDX12                       PIC 9(05).
           03  IDX13                       PIC 9(05).
           03  IDX14                       PIC 9(05).
           03  IDX15                       PIC 9(05).
           03  IDX16                       PIC 9(05).
           03  START-IDX                   PIC 9(05).
           03  IDX-SKJ-TAIHI               PIC 9(02).
      *
      *    ¥·¥¹¥Æ¥àÎÎ°è
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-ERRCD                   PIC X(04).
           03  WRK-NAI-I01-IDOYMD          PIC X(08).
           03  WRK-NAI-I01-IDOYMD-MAE      PIC X(08).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-HENYMDG                 PIC X(09).
      *
           03  WRK-AGE-YY                  PIC S9(04).
           03  WRK-AGE-MM                  PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z               PIC ---9.
               05  WRK-AGE-MEI             PIC X(04).
           03  WRK-BIRTHDAY                PIC X(08).
           03  WRK-SAGAKU.
               05  WRK-SAGAKU-NUM          PIC ZZZZ9.
               05  WRK-SAGAKU-EN           PIC X(02).
           03  WRK-HKNCOMBI                PIC X(100).
           03  WRK-BRMNUM                  PIC X(06).
           03  WRK-BRMNUM-IN               PIC X(06).
           03  WRK-BRMNUM-EDT              PIC X(06).
           03  WRK-BRMNUM-OT               PIC X(06).
           03  WRK-NYUINKA-KEY             PIC X(02).
           03  WRK-BTUNUM-KEY              PIC X(02).
           03  WRK-BRMNUM-KEY              PIC X(08).
           03  WRK-HKNCOMBI-KEY            PIC X(04).
           03  WRK-TOKUTEI-KEY.
               05  WRK-TOKUTEI-KBN-KEY     PIC X(01).
               05  WRK-TOKUTEI-NYUIN-KEY   PIC X(02).
      *
           03  WRK-STYMD                       PIC X(08).
           03  WRK-EDYMD                       PIC X(08).
           03  WRK-LASTSRYYM                   PIC X(06).
           03  WRK-NISSU                       PIC 9(05).
           03  WRK-NISSU1                      PIC 9(05).
           03  WRK-NISSU2                      PIC 9(05).
           03  WRK-TSUSAN                      PIC 9(05).
           03  WRK-NYUINNISSU                  PIC 9(05).
           03  WRK-SANTEISTYMD                 PIC X(08).
           03  WRK-SENTEI-STYMD                PIC X(08).
           03  WRK-BIRTHDAY-15                 PIC X(08).
           03  WRK-BIRTHDAY-15R   REDEFINES    WRK-BIRTHDAY-15.
               05  WRK-BIRTHDAY-15Y            PIC 9(04).
               05  WRK-BIRTHDAY-15MD           PIC 9(04).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNCOMBINUM9   REDEFINES  WRK-HKNCOMBINUM
                                           PIC 9(04).
           03  WRK-SAGAKU-KBN              PIC X(02).
           03  WRK-SAGAKU-KBN9  REDEFINES  WRK-SAGAKU-KBN
                                           PIC 9(02).
           03  WRK-TAIHI-SRYKA             PIC X(02).
      *
      *    °åÎÅµ¡´ØÀßÄê¤ÎÆþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯ÍÑ
           03  WRK-NYUKHNKSN-SRYCD         PIC X(09)  OCCURS 20.
           03  WRK-NYUKSNKBN               PIC X(03).
           03  WRK-BTU-NYUKSNKBN           PIC X(03).
           03  WRK-BTU-CDKBN-KBNNUM        PIC 9(03).
           03  WRK-BTU-CDKBN-KBNNUM-EDA    PIC 9(02).
           03  WRK-KSN-NYUKSNKBN           PIC X(03).
           03  WRK-KSN-CDKBN-KBNNUM        PIC 9(03).
           03  WRK-KSN-CDKBN-KBNNUM-EDA    PIC 9(02).
           03  WRK-STYUKYMD                PIC X(08).
           03  WRK-TAIINYMD.
               05  WRK-TAIINYM             PIC X(06).
               05  WRK-TAIINDD             PIC 9(02).
           03  WRK-MCP-PUTTYPE             PIC X(08).
           03  WRK-ORCHCN01-SYORIKBN       PIC X(01).
           03  WRK-ORCHCN01-PRTKBN         PIC X(01).
           03  WRK-AGECHK-SRYYMD           PIC X(08).
           03  WRK-NACCT-SRYYM             PIC X(08).
           03  WRK-PRINT-PG                PIC X(20).
           03  WRK-ORCSHKN-HKNCOMBINUM     PIC X(04).
           03  WRK-TOKUBETSU-NYUIN         PIC X(01).
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»Î²Ã»»²Ã»»ÂàÈòÎÎ°è
           03  WRK-BTU-HIKSISUKBN          PIC X(01).
           03  WRK-BTU-KNGTEK-KBN          PIC X(01).
           03  WRK-BTU-DRTEK-KBN           PIC X(01).
      *    ²ñ·×ºîÀ®ÍÑÂàÈòÎÎ°è
           03  WRK-TAIHI-HKN               PIC 9(03).
           03  WRK-TAIHI-SHOKUJI           PIC 9(03).
           03  IDX-SHOKUJIX                PIC X(02).
           03  IDX-SHOKUJI9   REDEFINES    IDX-SHOKUJIX
                                           PIC 9(02).
           03  IDX-SKJ                     PIC 9(02).
           03  WRK-NYUINCHU-TAIINYMD       PIC X(08).
           03  WRK-ORCHCN01-HKNCOMBINUM    PIC 9(04).
           03  WRK-STIDX                   PIC 9(02).
           03  WRK-EDIDX                   PIC 9(02).
           03  WRK-MIN-YMD.
               05  WRK-MIN-YM              PIC X(06).
               05  WRK-MIN-DD              PIC 9(02).
      *    ÆÃÄêÆþ±¡ÎÁ¤Î²ñ·×ºîÀ®¸å¤Ë°ìÈÌ¤ÎÆþ±¡ÎÁ¤Ç²ñ·×¤òºîÀ®¤¹¤ë¤¿¤á¤Î
      *    ÎÎ°è
           03  IPN-NYUKAIKEI-SAKUSEI-FLG   PIC X(01).
           03  IPN-SANTEISTYMD             PIC X(08).
           03  WRK-IPN-HANTEI-YMD1.
               05  WRK-IPN-HANTEI-YM1.
                   07  WRK-IPN-HANTEI-Y1   PIC 9(04).
                   07  WRK-IPN-HANTEI-M1   PIC 9(02).
               05  WRK-IPN-HANTEI-D1       PIC 9(02).
           03  WRK-IPN-HANTEI-YMD2         PIC X(08).
      *    ÆÃÄêÆþ±¡ÎÁ¤Î²ñ·×ºîÀ®ÍÑ¤Ë¿ÇÎÅ¥³¡¼¥É¤òÂàÈò¤¹¤ëÎÎ°è
           03  WRK-TOKUTEI-SRYCD1          PIC X(09).
           03  WRK-TOKUTEI-SRYCD2          PIC X(09).
           03  WRK-TOKUTEI-SRYCD3          PIC X(09).
           03  WRK-TOKUTEI-SRYCD4          PIC X(09).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-SHOKAI-NYUINYMD-W       PIC X(10).
           03  WRK-CHK-SHONUM              PIC 9(03).
           03  WRK-I01-I04-PROCKBN         PIC X(01).
           03  WRK-FTN-NYUINYMD            PIC X(08).
           03  WRK-FTN-TAIINYMD            PIC X(08).
           03  WRK-FTN-RRKNUM              PIC 9(03).
           03  WRK-NYUREKILST-SEL          PIC 9(05).
      *
      *    Å¾²Ê¡¿Å¾Åï¡¿Å¾¼¼»þ¤Ë¿©»ö²ñ·×¤òÂàÈò¤¹¤ë¹àÌÜ¡ÊºÇÂç£²¥«·îÊ¬¡Ë
           03  WRK-TENROOM-SKJ-SRYYMX.
               05  WRK-TENROOM-SKJ-SRYYY9  PIC 9(04).
               05  WRK-TENROOM-SKJ-SRYMM9  PIC 9(02).
           03  WRK-TENROOM-SKJ-TABLE.
               05  WRK-TENROOM-SKJ-TBL     OCCURS  2.
                   07  WRK-TENROOM-SKJ-SRYYM
                                           PIC X(06).
                   07  WRK-TENROOM-SKJ-DD  PIC 9(02).
                   07  WRK-TENROOM-SKJ     PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ18   PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ19   PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ20   PIC 9(03)  OCCURS  31.
           03  WRK-NAI-NYUINYMD.
               05  WRK-NAI-NYUINYM         PIC X(06).
               05  WRK-NAI-NYUINDD         PIC 9(02).
      *
      *    ´µ¼ÔÇ¯Îð»»½ÐÍÑ¥¨¥ê¥¢
       01  WRK-NENREI-AREA.
           03  WRK-NENREI                  PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1                PIC 9(04).
               05  WRK-MMS1                PIC 9(02).
               05  WRK-DDS1                PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2                PIC 9(04).
               05  WRK-MMS2                PIC 9(02).
               05  WRK-DDS2                PIC 9(02).
      *
      *    Âà±¡Æü¤ÎÆ±ÆüºÆÆþ±¡ÍÑ¥ï¡¼¥¯ÎÎ°è
       01  WRK-DOU-AREA.
           03  WRK-DOU-NAI-I01-NYUINYMD      PIC X(08).
           03  WRK-DOU-PTNYUINRRK-TENNYUYMD  PIC X(08).
           03  WRK-DOU-TENNYUYMD             PIC X(08).
           03  FLG-JIINRRK                   PIC 9(01).
      *
      *    ¿©Æ²²Ã»»ÀßÄêÍÑ¤ÎÂàÈòÎÎ°è
       01   WRK-SHOKUDOU                   PIC 9(02).
      *
      *    ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ¡ÊÅ¾²Ê¡¢Å¾Åï¡Ë·×»»¥¨¥ê¥¢
       01  TN-IDX                          PIC 9(02).
       01  TN-IDX2                         PIC 9(02).
       01  WK-TOKUTEI-DAY                  PIC 9(04).
       01  WK-TOKUTEI-DAY2                 PIC 9(04).
      *
      *    ³°Çñ¿©»ö¥ª¡¼¥À¡¼¤ÎÆüÉÕÂàÈò
       01  WRK-STARTYMD.
           03  WRK-STARTYMD-YM.
               05  WRK-STARTYMD-Y          PIC 9(04).
               05  WRK-STARTYMD-M          PIC 9(02).
           03  WRK-STARTYMD-D              PIC 9(02).
       01  WRK-GETSUMATSU-YMD.
           03  WRK-GETSUMATSU-Y            PIC 9(04).
           03  WRK-GETSUMATSU-M            PIC 9(02).
           03  WRK-GETSUMATSU-D            PIC 9(02).
      *
       01  WRK-ROW.
           03  WRK-NYUREKILST-ROW          PIC S9(9)   BINARY.
      *
      *    ¥¹¥ÑÎÎ°è ¥ï¡¼¥¯
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WRK-SPA-//.
      *
      *    Æþ±¡ÍúÎò¥ï¡¼¥¯
       01  NRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING
                               //PTNYUINRRK-// BY //NRRK-//.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *    ¡Ê°åÎÅµ¡´Ø´ðËÜ¾ðÊó¡Ë
           COPY    "CPSK1001.INC".
      *    ¡Ê¿ÇÎÅ²ÊÆþ±¡²Ê¾ðÊó¡Ë
           COPY    "CPSK1005.INC".
      *    ¡Ê¿¦°÷¾ðÊó¡Ë
           COPY    "CPSK1010.INC".
      *
      *    Ä¢É¼¥×¥í¥°¥é¥àÌ¾
           COPY    "CPSK1035.INC".
      *    ¡Ê°åÎÅµ¡´Ø¾ðÊó¡Ë
           COPY    "CPSK5000.INC".
      *    ¡ÊÉÂÅï¾ðÊó¡Ë
           COPY    "CPSK5001.INC".
      *    ¡ÊÉÂ¼¼¾ðÊó¡Ë
           COPY    "CPSK5002.INC".
      *    ¡Ê¼¼ÎÁº¹³Û¡Ë
           COPY    "CPSK5113.INC".
      *    ¡ÊÆÃÄêÆþ±¡ÎÁ¡ÝÉÂÅï¡Ë
           COPY    "CPSK5110.INC".
      *    ¡ÊÆÃÄêÆþ±¡ÎÁ¡ÝÉÂ¼¼¡Ë
           COPY    "CPSK5111.INC".
      *    
           COPY    "CPSK5013.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ´µ¼ÔÈÖ¹æ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ´µ¼ÔÂ¾±¡ÎòÉÕ²Ã¾ðÊó
       01  PTTAINFUKA-REC.
           COPY    "CPPTTAINFUKA.INC".
      *
      *    Ï·¿Í°ìÈÌ¥³¡¼¥ÉÊÑ´¹
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    ÁªÄêÎÅÍÜÈñ
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    Æþ±¡²ñ·×
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    Æþ±¡ÅÐÏ¿¥µ¥Ö
           COPY    "CPI0SUB01.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTNUM.INC".
      *
      *    ¿ôÃÍ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSNUM.INC".
      *
      *    ½é´ü²Ã»»»»Äê¥µ¥Ö
           COPY    "CPORCSSYOKIKSN.INC".
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö
           COPY    "CPORCSNYUACCT.INC".
      *
      *    ÄÌ»»Æü¿ô¼èÆÀ¥µ¥Ö
           COPY "CPORCSTUSAN.INC".
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸¥µ¥Ö
           COPY    "CPORCSHKNSET.INC".
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¾È²ñ¥Æ¡¼¥Ö¥ë
           COPY  "CPORCSS010.INC".
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥µ¥Ö
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *    °åÎÅ´Ñ»¡Æþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥Ñ¥é¥á¥¿
           COPY  "CPORCSIRYOKNS.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *    Æþ±¡¥ª¡¼¥À¡¼²ñ·×ºîÀ®
           COPY    "CPORCSODRNACCT.INC".
      *    Æþ±¡ÍúÎò
       01  LNK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"   REPLACING //PTNYUINRRK-//
                                        BY        //LNK-PTNYUINRRK-//.
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SODRNACCT-AREA
                                               LNK-PTNYUINRRK-REC
                                               SPA-AREA.
      *****************************************************************
      *    ¼ç    ½è    Íý
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 1001-SPA-SET-SEC
      *
           DISPLAY "SODRNACCT-ORDERID = " SODRNACCT-ORDERID
           MOVE    SPACE           TO  WRK-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    SODRNACCT-ORDERID
      *       ¡ÊÆþ±¡¡Ë
               WHEN    "0101"
                   PERFORM   4801-NYUKAIKEI-EDIT-SEC
      *       ¡ÊÂà±¡¡Ë
               WHEN    "0102"
                   PERFORM   4801-TAIINKAIKEI-EDIT-SEC
      *       ¡ÊÅ¾²Ê¡¿Å¾Åï¡¿Å¾¼¼¡Ë
               WHEN    "0103"
               WHEN    "0104"
               WHEN    "0105"
                   PERFORM   4801-TENROOM-EDIT-SEC
                   IF    IPN-NYUKAIKEI-SAKUSEI-FLG   =  "1"
                       PERFORM   4801-TENROOM-EDIT-SEC
                   END-IF
      *       ¡ÊÆþ±¡¼è¾Ã¡Ë
               WHEN    "0107"
      *       ¡ÊÆþ±¡¼è¾Ã¡Ý²ñ·×´Þ¤à¡Ë
               WHEN    "0108"
                   PERFORM   4801-NYUKAIKEI-ERASE-SEC
      *       ¡Ê³°Çñ¡Ë
               WHEN    "0201"
               WHEN    "0202"
               WHEN    "0203"
               WHEN    "0204"
               WHEN    "0205"
                   PERFORM   4801-GAIHAKU-SEC
      *       ¡Ê¿©»ö¡Ë
               WHEN    "0301"
               WHEN    "0302"
               WHEN    "0303"
               WHEN    "0304"
               WHEN    "0305"
                   IF   SODRNACCT-STARTYMD(1:6)  >= "200604"
                       PERFORM   4801-SHOKUJI-200604-SEC
                   ELSE
                       PERFORM   4801-SHOKUJI-SEC
                   END-IF
           END-EVALUATE
      *
           PERFORM 4900-ORCSS010-SEC
      *
           MOVE      ZERO    TO   SODRNACCT-RC
           .
      *
       000-PROC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           MOVE    NACCT-SRYYM         TO  WRK-LASTSRYYM
           MOVE    LNK-PTNYUINRRK-REC  TO  NRRK-REC
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    SPA´ðËÜ¾ðÊóÀßÄê½èÍý
      *****************************************************************
       1001-SPA-SET-SEC                SECTION.
      *
      *    INITIALIZE                          SPA-AREA
      *
           INITIALIZE                          SYSKANRI-REC
           MOVE    "1001"                  TO  SYS-KANRICD
           MOVE    "*"                     TO  SYS-KBNCD
           MOVE    LNK-PTNYUINRRK-TENNYUYMD TO SYS-STYUKYMD
                                               SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC            TO  SYS-1001-REC
      *
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  SPA-HOSPNUM
           MOVE    SYS-1001-PREFNUM        TO  SPA-PREFNUM
           MOVE    "1"                     TO  SPA-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID     TO  SPA-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
           MOVE    PTINF-BIRTHDAY          TO  SPA-BIRTHDAY
      *
           .
      *
       1001-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-NYUKAIKEI-EDIT-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    ÄÌ»»Æü¿ô¡¢»»Äê³«»ÏÆü¤òµá¤á¤ë
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( WRK-ERRCD    NOT =  SPACE )
               GO  TO  4801-NYUKAIKEI-EDIT-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
           PERFORM  4801-BTUSEL-SEC
      *
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¤Î¾ì¹ç¤Ï´ü´Ö²Ã»»¤¬Â¸ºß¤¹¤ë¤¿¤á¡¢
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÎÅÍÜÉÂ¾²¤ÇÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤òÁê¸ß»»Äê¤¹¤ë¾ì¹ç¤â
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-NYUKAIKEI-EDIT-EXT
                   END-IF
               END-IF
           ELSE
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  WRK-ERRCD
                       GO  TO  4801-NYUKAIKEI-EDIT-EXT
                   END-IF
               ELSE
                   MOVE    "4018"      TO  WRK-ERRCD
                   GO  TO  4801-NYUKAIKEI-EDIT-EXT
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "1"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                            TO   LNK-SCNYUACCT-STSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    °åÎÅ´Ñ»¡Æþ±¡¥»¥Ã¥È
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     LNK-PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     LNK-PTNYUINRRK-TOKUTEI-KBN
                                             TO  LNK-IRYOKNS-KBN
           MOVE     LNK-PTNYUINRRK-TOKUTEI-NYUIN
                                             TO  LNK-IRYOKNS-NYUIN
           MOVE     LNK-PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
           DISPLAY  "==  ORCSIRYOKNS CALL ==="
           DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                        TO  LNK-SCNYUACCT-STSRYYMD
               MOVE   "99999999"        TO  LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    ÄÌ»»»»ÄêºÑÆü¿ô
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    ÆÃÄêÆþ±¡¤ÇÆþ±¡ÎÁ¤¬ÊÑ¹¹¤Ë¤Ê¤ë¥³¡¼¥É¤ÎÂÐ±þ
      *
      *    ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190774110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190774110"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    13                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190774210"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ¡ÊÀ¸³èÎÅÍÜ¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190125010"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190125010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    13                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190125110"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    Ç§ÃÎ¾ÉÉÂÅïÆþ±¡ÎÁ£±¡ÊÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç¤Î´ü´ÖÂÐ±þ¡Ë
           IF    LNK-PTNYUINRRK-TENNYUYMD  <=  "20100331"
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190739910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU03      <=  90
                     ADD    1           TO  TN-IDX
                     MOVE   "190739910" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Ç§ÃÎ¾ÉÉÂÅïÆþ±¡ÎÁ£±¤Î£¹£°Æü°ÊÆâ¤Î½ªÆü
      *              ¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU03
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190740010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
             END-IF
           END-IF
      *
      *    Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡ÊÊ¿À®£²£´Ç¯£³·î¤Þ¤Ç¤Î´ü´ÖÂÐ±þ¡Ë
           IF   (LNK-PTNYUINRRK-TENNYUYMD  >=  "20100401")   AND
                (LNK-PTNYUINRRK-TENNYUYMD  <=  "20120331")
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190739910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU03      <=  60
                     ADD    1           TO  TN-IDX
                     MOVE   "190739910" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¤Î£¶£°Æü°ÊÆâ¤Î½ªÆü
      *              ¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU03
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190740010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
             END-IF
           END-IF
      *
      *    Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡ÊÊ¿À®£²£´Ç¯£´·î¤«¤é¤Î´ü´ÖÂÐ±þ¡Ë
           IF    LNK-PTNYUINRRK-TENNYUYMD    >=    "20120401"
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190151610"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190151610"
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     29               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190739910"
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     30               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *
                 MOVE   "190740010"
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(3)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(3)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(3)
             END-IF
           END-IF
      *
      *    Ç§ÃÎ¾ÉÉÂÅïÆþ±¡ÎÁ£²¡ÊÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç¤Î´ü´ÖÂÐ±þ¡Ë
           IF    LNK-PTNYUINRRK-TENNYUYMD  <=  "20100331"
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190813410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU03      <=  90
                     ADD    1           TO  TN-IDX
                     MOVE   "190813410" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Ç§ÃÎ¾ÉÉÂÅïÆþ±¡ÎÁ£²¤Î£¹£°Æü°ÊÆâ¤Î½ªÆü
      *              ¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU03
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190813510"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
             END-IF
           END-IF
      *
      *    Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¡ÊÊ¿À®£²£´Ç¯£³·î¤Þ¤Ç¤Î´ü´ÖÂÐ±þ¡Ë
           IF   (LNK-PTNYUINRRK-TENNYUYMD  >=  "20100401")   AND
                (LNK-PTNYUINRRK-TENNYUYMD  <=  "20120331")
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190813410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU03      <=  60
                     ADD    1           TO  TN-IDX
                     MOVE   "190813410" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¤Î£¶£°Æü°ÊÆâ¤Î½ªÆü
      *              ¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU03
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190813510"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
             END-IF
           END-IF
      *
      *    Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¡ÊÊ¿À®£²£´Ç¯£´·î¤«¤é¤Î´ü´ÖÂÐ±þ¡Ë
           IF    LNK-PTNYUINRRK-TENNYUYMD    >=    "20120401"
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190151710"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190151710"
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     29               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190813410"
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     30               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *
                 MOVE   "190813510"
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(3)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(3)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(3)
             END-IF
           END-IF
      *
      *    ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡ÊÊ¿À®£²£´Ç¯£´·î¤«¤é¤Î´ü´ÖÂÐ±þ¡Ë
           IF    LNK-PTNYUINRRK-TENNYUYMD    >=    "20120401"
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190028910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190028910"
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     29               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190150510"
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     30               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *
                 MOVE   "190150610"
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(3)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(3)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(3)
             END-IF
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¤Ç£·Æü¤Þ¤Ç¡¢£±£´Æü¤Þ¤Ç¡¢¤½¤ì°Ê¹ß¤Ï°ìÈÌÆþ±¡ÎÁ¤ò»»Äê¤¹¤ë
      *    ¾ì¹ç¤Î¿ÇÎÅ¥³¡¼¥É¤ò¼èÆÀ
      *    ¢¨Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤ÎµßÌ¿µßµÞÆþ±¡ÎÁ¤Ç¤¢¤ì¤Ð¡¢¤³¤Î½èÍý¤Ï¥¹¥ë¡¼¤¹¤ë
           IF    (LNK-PTNYUINRRK-TENNYUYMD >=    "20080401")  AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190024510"  OR
                                                 "190110210"  OR
                                                 "190110310"  OR
                                                 "190024310"  OR
                                                 "190110410"  OR
                                                 "190110510")
               CONTINUE
           ELSE
             PERFORM    4801-TOKUTEI-NYUIN-7-14-SEC
      *
             IF  SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   WRK-TOKUTEI-SRYCD1  
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     6                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   WRK-TOKUTEI-SRYCD2
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     14               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
             END-IF
           END-IF
      *
      *    Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤ÏµßÌ¿µßµÞÆþ±¡ÎÁ¤ò£±Æü¡Á£³Æü¡¢£´Æü¡Á£·Æü¡¢
      *    £¸Æü¡Á£±£´Æü¤Ç¤ÎÅÀ¿ô¹ï¤ß¤È¤¹¤ë
           IF   ((LNK-PTNYUINRRK-TENNYUYMD >=    "20080401")  AND
                 (LNK-PTNYUINRRK-TENNYUYMD <=    "20100331")) AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190024510"  OR
                                                 "190110210"  OR
                                                 "190110310"  OR
                                                 "190024310"  OR
                                                 "190110410"  OR
                                                 "190110510")
             PERFORM    4801-TOKUTEI-NYUIN-3-7-14-SEC
      *
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   WRK-TOKUTEI-SRYCD1  
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     2                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   WRK-TOKUTEI-SRYCD2
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     4                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *
                 MOVE   WRK-TOKUTEI-SRYCD3
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(3)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(3)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(3)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     14               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
             END-IF
           END-IF
      *
      *    Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤Ï°Ê²¼ÆÃÄêÆþ±¡ÎÁ¤ò£±Æü¡Á£³Æü¡¢£´Æü¡Á£·Æü¡¢
      *    £¸Æü¡Á£±£´Æü¤Ç¤ÎÅÀ¿ô¹ï¤ß¤È¤¹¤ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
           IF    (LNK-PTNYUINRRK-TENNYUYMD >=    "20100401")  AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190024510"  OR
                                                 "190024310"  OR
                                                 "190138110"  OR
                                                 "190138710")
             PERFORM    4801-TOKUTEI-NYUIN2-3-7-14-SEC
      *
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   WRK-TOKUTEI-SRYCD1  
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     2                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   WRK-TOKUTEI-SRYCD2
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     4                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *
                 MOVE   WRK-TOKUTEI-SRYCD3
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(3)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(3)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(3)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     14               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
             END-IF
           END-IF
      *
      *    Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤Ï°Ê²¼ÆÃÄêÆþ±¡ÎÁ¤ò£±Æü¡Á£³Æü¡¢£´Æü¡Á£·Æü¡¢
      *    £¸Æü¡Á£¶£°Æü¤Ç¤ÎÅÀ¿ô¹ï¤ß¤È¤¹¤ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
           IF    (LNK-PTNYUINRRK-TENNYUYMD >=    "20100401")  AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190138410")
             PERFORM    4801-TOKUTEI-NYUIN3-3-7-60-SEC
      *
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   WRK-TOKUTEI-SRYCD1  
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     2                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   WRK-TOKUTEI-SRYCD2
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     4                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *
                 MOVE   WRK-TOKUTEI-SRYCD3
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(3)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(3)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    59                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(3)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     60               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(3)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
             END-IF
           END-IF
      *
      *    Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤Ï°Ê²¼ÆÃÄêÆþ±¡ÎÁ¤ò£±Æü¡Á£³Æü¡¢£´Æü¡Á£·Æü¡¢
      *    £¸Æü¡Á£±£´Æü¡¢£±£µ¡Á£¶£°Æü¤Ç¤ÎÅÀ¿ô¹ï¤ß¤È¤¹¤ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
           IF    (LNK-PTNYUINRRK-TENNYUYMD >=    "20100401")  AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190139010")
             PERFORM    4801-TOKUTEI-NYUIN4-3-7-14-60-SEC
      *
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   WRK-TOKUTEI-SRYCD1  
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     2                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   WRK-TOKUTEI-SRYCD2
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     4                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *
                 MOVE   WRK-TOKUTEI-SRYCD3
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(3)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(3)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     7                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(3)
      *
                 MOVE   WRK-TOKUTEI-SRYCD4
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(4)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(3)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(4)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    59                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(4)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     60               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(4)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
             END-IF
           END-IF
      *
      *    Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤Ï°Ê²¼ÆÃÄêÆþ±¡ÎÁ¤ò£±Æü¡Á£·Æü¡¢£¸Æü¡Á£¶£°Æü
      *    ¤Ç¤ÎÅÀ¿ô¹ï¤ß¤È¤¹¤ë
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
           IF    (LNK-PTNYUINRRK-TENNYUYMD >=    "20100401")  AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190140010")
             PERFORM    4801-TOKUTEI-NYUIN5-7-60-SEC
      *
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   WRK-TOKUTEI-SRYCD1  
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     6                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   WRK-TOKUTEI-SRYCD2
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
      *
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    59                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     60               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
             END-IF
           END-IF
      *
      *    ¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê£¶£°Æü¾å¸Â¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190024810"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190024810"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    59                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     60               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ¡Ê£²£±Æü¤ò¸ÂÅÙ¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190117310"
                 MOVE   SPACE       TO   IPN-NYUKAIKEI-SAKUSEI-FLG
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190117310"
                                    TO   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    20                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     21               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ¡Ê£±£´Æü¤ò¸ÂÅÙ¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190120810"
                 MOVE   SPACE       TO   IPN-NYUKAIKEI-SAKUSEI-FLG
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190120810"
                                    TO   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    13                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     14               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    Àº¿À²ÊµßµÞÆþ±¡ÎÁ
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121010"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190121010" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190111410"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190130010"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190130010" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190130110"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190130210"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190130210" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190130310"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190121110" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190066910"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121210"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190121210" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190067010"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê£¶£°Æü¾å¸Â¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190129910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190129910"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    59                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     60               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡Ê»ØÄêÃÏ°è¡Ë¡Ê£¶£°Æü¾å¸Â¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190150310"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190150310"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    59                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     60               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê»ØÄêÃÏ°è¡Ë¡Ê£¶£°Æü¾å¸Â¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190150410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190150410"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    59                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     60               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡Ë¡Ê£¶£°Æü¾å¸Â¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190152610"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190152610"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    59                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     60               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡¦¥ê¥Ï¥Ó¥ê»»Äê´µ¼Ô¡Ë¡Ê£¶£°Æü¾å¸Â¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190152710"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190152710"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    59                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     60               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *-------ÆÃÄêÆþ±¡ÂÐ±þ¤³¤³¤Þ¤Ç-----------------------------------
      *
      *    Æþ±¡ÎÁ²Ã»»¿ÇÎÅ¥³¡¼¥É¼è¤ê¹þ¤ß
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    ÆÃÄêÆþ±¡ÎÁ¶èÊ¬¤Î¥»¥Ã¥È
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        Ï·¿ÍÆÃÄêÆþ±¡´ðËÜÎÁµÚ¤ÓÊ¿À®£±£¸Ç¯£··î°Ê¹ß¤ÎÎÅÍÜÉÂÅï¡¢
      *        ÎÅÍÜÉÂ¾²¤Î¾ì¹ç¤ÏÆÃÄêÆþ±¡ÎÁ°·¤¤¤«¤é³°¤¹
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410" OR
                                                       "190799510" OR
                                                       "190121310" OR
                                                       "190121410" OR
                                                       "190121510" OR
                                                       "190121610" OR
                                                       "190121710" OR
                                                       "190121810" OR
                                                       "190122010" OR
                                                       "190122110" OR
                                                       "190122210" OR
                                                       "190122310" OR
                                                       "190122410" OR
                                                       "190121510" OR
                                                       "190123710" OR
                                                       "190123810" OR
                                                       "190123910" OR
                                                       "190124010" OR
                                                       "190124110" OR
                                                       "190124210" OR
                                                       "190124310" OR
                                                       "190124410" OR
                                                       "190124510" OR
                                                       "190124610" OR
                                                       "190124710" OR
                                                       "190124810" OR
                                                       "190131610" OR
                                                       "190131710" OR
                                                       "190131810" OR
                                                       "190131910" OR
                                                       "190132010" OR
                                                       "190132110" OR
                                                       "190132210" OR
                                                       "190132310" OR
                                                       "190132410" OR
                                                       "190132510" OR
                                                       "190132610" OR
                                                       "190132710" OR
                                                       "190132810" OR
                                                       "190132910" OR
                                                       "190133010" OR
                                                       "190133110" OR
                                                       "190133210" OR
                                                       "190133310" OR
                                                       "190133410" OR
                                                       "190133510" OR
                                                       "190133610" OR
                                                       "190133710" OR
                                                       "190133810" OR
                                                       "190133910" OR
                                                       "190134010" OR
                                                       "190134110"
                   MOVE  "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
               END-IF
           END-IF
      *    ³°Çñ
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    ¼¼ÎÁº¹³Û
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     LNK-PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   LNK-PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    ¿©»ö¶èÊ¬
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE   0             TO  LNK-SCNYUACCT-SHOKUJIKBN
      *    ¿©Æ²²Ã»»
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           PERFORM 4801-SENTEI-SET-SEC
           MOVE  LNK-PTNYUINRRK-HOSPNUM   TO  SPA-HOSPNUM
           MOVE  LNK-PTNYUINRRK-PTID     TO  SPA-PTID
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" )
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   WRK-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   LNK-PTNYUINRRK-TENNYUYMD
                   PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-NYUKAIKEI-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆÃÄêÆþ±¡ÎÁ¡Ê£·Æü¡¿£±£´Æü¡¿¤½¤ì°Ê¹ß¡Ë¤Î¿ÇÎÅ¥³¡¼¥É¥»¥Ã¥È
      *****************************************************************
       4801-TOKUTEI-NYUIN-7-14-SEC         SECTION.
      *
      *ÆÃÄêÆþ±¡ÎÁ¤Ç£·Æü¤Þ¤Ç¡¢£±£´Æü¤Þ¤Ç¡¢¤½¤ì°Ê¹ß¤Ï°ìÈÌÆþ±¡ÎÁ¤ò»»Äê¤¹¤ë
      *¾ì¹ç¤Î¿ÇÎÅ¥³¡¼¥É¤ò¼èÆÀ
      *
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
           MOVE      "999999999"  TO    WRK-TOKUTEI-SRYCD1
                                        WRK-TOKUTEI-SRYCD2
           EVALUATE   SYOKIKSN-IN-KHNSRYCD
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
               WHEN   "190024610"
                   MOVE    "190024610"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190074610"  TO  WRK-TOKUTEI-SRYCD2
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê½Å¾É¼ÔÅùÈæÎ¨´ð½àÌ¤Ëþ¡Ë
      *    ¡ÊÊ¿À®£²£²Ç¯²þÄê¤Ë¤è¤êÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±¤ËÊÑ¹¹¡Ë
               WHEN   "190116310"
                   MOVE    "190116310"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190116410"  TO  WRK-TOKUTEI-SRYCD2
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
               WHEN   "190139810"
                   MOVE    "190139810"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190139910"  TO  WRK-TOKUTEI-SRYCD2
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±
               WHEN   "190024510"
                   MOVE    "190024510"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190074510"  TO  WRK-TOKUTEI-SRYCD2
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê¹âÅÙ¡Ë
               WHEN   "190110210"
                   MOVE    "190110210"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190110610"  TO  WRK-TOKUTEI-SRYCD2
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê£Á¤Þ¤¿¤Ï¿·Àß¡Ë
               WHEN   "190110310"
                   MOVE    "190110310"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190110710"  TO  WRK-TOKUTEI-SRYCD2
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²
               WHEN   "190024310"
                   MOVE    "190024310"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190024410"  TO  WRK-TOKUTEI-SRYCD2
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê¹âÅÙ¡Ë
               WHEN   "190110410"
                   MOVE    "190110410"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190110810"  TO  WRK-TOKUTEI-SRYCD2
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê£Á¤Þ¤¿¤Ï¿·Àß¡Ë
               WHEN   "190110510"
                   MOVE    "190110510"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190110910"  TO  WRK-TOKUTEI-SRYCD2
      *    °ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
               WHEN   "190075510"
                   MOVE    "190075510"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190075610"  TO  WRK-TOKUTEI-SRYCD2
      *    ¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
               WHEN   "190149910"
                   MOVE    "190149910"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190150010"  TO  WRK-TOKUTEI-SRYCD2
           END-EVALUATE
           .
      *
       4801-TOKUTEI-NYUIN-7-14-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆÃÄêÆþ±¡ÎÁ¡Ê£³Æü¡¿£·Æü¡¿£±£´Æü¡¿¤½¤ì°Ê¹ß¡Ë¤Î¿ÇÎÅ¥³¡¼¥É¥»¥Ã¥È
      *****************************************************************
       4801-TOKUTEI-NYUIN-3-7-14-SEC         SECTION.
      *
      *ÆÃÄêÆþ±¡ÎÁ¤Ç£³Æü¤Þ¤Ç¡¢£·Æü¤Þ¤Ç¡¢£±£´Æü¤Þ¤Ç¡¢¤½¤ì°Ê¹ß¤Ï°ìÈÌÆþ±¡ÎÁ¤ò»»Äê¤¹¤ë
      *¾ì¹ç¤Î¿ÇÎÅ¥³¡¼¥É¤ò¼èÆÀ
      *
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
           MOVE      "999999999"  TO    WRK-TOKUTEI-SRYCD1
                                        WRK-TOKUTEI-SRYCD2
                                        WRK-TOKUTEI-SRYCD3
           EVALUATE   SYOKIKSN-IN-KHNSRYCD
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±
               WHEN   "190024510"
                   MOVE    "190024510"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190128610"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190074510"  TO  WRK-TOKUTEI-SRYCD3
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê¹âÅÙ¡Ë
               WHEN   "190110210"
                   MOVE    "190110210"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190129110"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190110610"  TO  WRK-TOKUTEI-SRYCD3
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê£Á¡Ë
               WHEN   "190110310"
                   MOVE    "190110310"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190128910"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190110710"  TO  WRK-TOKUTEI-SRYCD3
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²
               WHEN   "190024310"
                   MOVE    "190024310"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190128710"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190024410"  TO  WRK-TOKUTEI-SRYCD3
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê¹âÅÙ¡Ë
               WHEN   "190110410"
                   MOVE    "190110410"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190129210"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190110810"  TO  WRK-TOKUTEI-SRYCD3
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê£Á¡Ë
               WHEN   "190110510"
                   MOVE    "190110510"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190129010"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190110910"  TO  WRK-TOKUTEI-SRYCD3
           END-EVALUATE
           .
      *
       4801-TOKUTEI-NYUIN-3-7-14-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê¿À®£²£²Ç¯²þÄê
      *    ÆÃÄêÆþ±¡ÎÁ¡Ê£³Æü¡¿£·Æü¡¿£±£´Æü¡¿¤½¤ì°Ê¹ß¡Ë¤Î¿ÇÎÅ¥³¡¼¥É¥»¥Ã¥È
      *****************************************************************
       4801-TOKUTEI-NYUIN2-3-7-14-SEC         SECTION.
      *
      *ÆÃÄêÆþ±¡ÎÁ¤Ç£³Æü¤Þ¤Ç¡¢£·Æü¤Þ¤Ç¡¢£±£´Æü¤Þ¤Ç¡¢¤½¤ì°Ê¹ß¤Ï°ìÈÌÆþ±¡ÎÁ¤ò»»Äê¤¹¤ë
      *¾ì¹ç¤Î¿ÇÎÅ¥³¡¼¥É¤ò¼èÆÀ
      *
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
           MOVE      "999999999"  TO    WRK-TOKUTEI-SRYCD1
                                        WRK-TOKUTEI-SRYCD2
                                        WRK-TOKUTEI-SRYCD3
           EVALUATE   SYOKIKSN-IN-KHNSRYCD
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±
               WHEN   "190024510"
                   MOVE    "190024510"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190128610"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190074510"  TO  WRK-TOKUTEI-SRYCD3
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²
               WHEN   "190024310"
                   MOVE    "190024310"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190128710"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190024410"  TO  WRK-TOKUTEI-SRYCD3
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
               WHEN   "190138110"
                   MOVE    "190138110"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190138210"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190138310"  TO  WRK-TOKUTEI-SRYCD3
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
               WHEN   "190138710"
                   MOVE    "190138710"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190138810"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190138910"  TO  WRK-TOKUTEI-SRYCD3
           END-EVALUATE
           .
      *
       4801-TOKUTEI-NYUIN2-3-7-14-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê¿À®£²£²Ç¯²þÄê
      *    ÆÃÄêÆþ±¡ÎÁ¡Ê£³Æü¡¿£·Æü¡¿£¶£°Æü¡¿¤½¤ì°Ê¹ß¡Ë¤Î¿ÇÎÅ¥³¡¼¥É¥»¥Ã¥È
      *****************************************************************
       4801-TOKUTEI-NYUIN3-3-7-60-SEC         SECTION.
      *
      *ÆÃÄêÆþ±¡ÎÁ¤Ç£³Æü¤Þ¤Ç¡¢£·Æü¤Þ¤Ç¡¢£¶£°Æü¤Þ¤Ç¡¢¤½¤ì°Ê¹ß¤Ï°ìÈÌÆþ±¡ÎÁ¤ò»»Äê¤¹¤ë
      *¾ì¹ç¤Î¿ÇÎÅ¥³¡¼¥É¤ò¼èÆÀ
      *
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
           MOVE      "999999999"  TO    WRK-TOKUTEI-SRYCD1
                                        WRK-TOKUTEI-SRYCD2
                                        WRK-TOKUTEI-SRYCD3
           EVALUATE   SYOKIKSN-IN-KHNSRYCD
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
               WHEN   "190138410"
                   MOVE    "190138410"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190138510"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190138610"  TO  WRK-TOKUTEI-SRYCD3
           END-EVALUATE
           .
      *
       4801-TOKUTEI-NYUIN3-3-7-60-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê¿À®£²£²Ç¯²þÄê
      *    ÆÃÄêÆþ±¡ÎÁ¡Ê£³Æü¡¿£·Æü¡¿£±£´Æü¡¿£¶£°Æü¡¿¤½¤ì°Ê¹ß¡Ë¤Î¿ÇÎÅ¥³¡¼¥É¥»¥Ã¥È
      *****************************************************************
       4801-TOKUTEI-NYUIN4-3-7-14-60-SEC         SECTION.
      *
      *ÆÃÄêÆþ±¡ÎÁ¤Ç£³Æü¤Þ¤Ç¡¢£·Æü¤Þ¤Ç¡¢£±£´Æü¤Þ¤Ç¡¢£¶£°Æü¤Þ¤Ç¡¢
      *¤½¤ì°Ê¹ß¤Ï°ìÈÌÆþ±¡ÎÁ¤ò»»Äê¤¹¤ë¾ì¹ç¤Î¿ÇÎÅ¥³¡¼¥É¤ò¼èÆÀ
      *
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
           MOVE      "999999999"  TO    WRK-TOKUTEI-SRYCD1
                                        WRK-TOKUTEI-SRYCD2
                                        WRK-TOKUTEI-SRYCD3
                                        WRK-TOKUTEI-SRYCD4
           EVALUATE   SYOKIKSN-IN-KHNSRYCD
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
               WHEN   "190139010"
                   MOVE    "190139010"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190139110"  TO  WRK-TOKUTEI-SRYCD2
                   MOVE    "190139210"  TO  WRK-TOKUTEI-SRYCD3
                   MOVE    "190139310"  TO  WRK-TOKUTEI-SRYCD4
           END-EVALUATE
           .
      *
       4801-TOKUTEI-NYUIN4-3-7-14-60-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê¿À®£²£²Ç¯²þÄê
      *    ÆÃÄêÆþ±¡ÎÁ¡Ê£·Æü¡¿£¶£°Æü¡¿¤½¤ì°Ê¹ß¡Ë¤Î¿ÇÎÅ¥³¡¼¥É¥»¥Ã¥È
      *****************************************************************
       4801-TOKUTEI-NYUIN5-7-60-SEC         SECTION.
      *
      *ÆÃÄêÆþ±¡ÎÁ¤Ç£·Æü¤Þ¤Ç¡¢£¶£°Æü¤Þ¤Ç¡¢¤½¤ì°Ê¹ß¤Ï°ìÈÌÆþ±¡ÎÁ¤ò»»Äê¤¹¤ë
      *¾ì¹ç¤Î¿ÇÎÅ¥³¡¼¥É¤ò¼èÆÀ
      *
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
           MOVE      "999999999"  TO    WRK-TOKUTEI-SRYCD1
                                        WRK-TOKUTEI-SRYCD2
           EVALUATE   SYOKIKSN-IN-KHNSRYCD
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
               WHEN   "190140010"
                   MOVE    "190140010"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190140110"  TO  WRK-TOKUTEI-SRYCD2
           END-EVALUATE
           .
      *
       4801-TOKUTEI-NYUIN5-7-60-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»¤Î¼è¤ê¹þ¤ß½èÍý
      *****************************************************************
       4801-NYUKSN-GET-SEC         SECTION.
      *
           MOVE   ZERO             TO   IDX8
      *
      *°åÎÅµ¡´Ø²Ã»»¡Ê¸º»»¡Ë¾ðÊó
           INITIALIZE              SYSKANRI-REC
           MOVE    "5000"          TO  SYS-KANRICD
           EVALUATE   SODRNACCT-ORDERID(3:2)
               WHEN   "01"
               WHEN   "03"
               WHEN   "04"
               WHEN   "05"
                   MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
      **         WHEN   "08"
      **             MOVE   WRK-NAI-I01-IDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
      **         WHEN   "09"
      **             MOVE   SPA-NAI-I01-MAEIDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
               WHEN   OTHER
                   MOVE    SPA-SYSYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           END-EVALUATE
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                   TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¤Î°åÎÅµ¡´Ø¾ðÊó¤è¤ê¼èÆÀ
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF   MCP-RC             =    ZERO
               MOVE    MCPDATA-REC TO   SYS-5000-REC
      *        Æþ±¡ÎÁ²Ã»»
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (SYS-5000-KSNCD(IDX7)    =   SPACE)  OR
                        (IDX7                    >   20)
                    IF   SYS-5000-KSNKBN(IDX7)   =   "2"
                             ADD    1                TO  IDX8
                             MOVE   SYS-5000-KSNCD(IDX7)  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-PERFORM
      *        ÃÏ°è²Ã»»
               IF   SYS-5000-CHIIKIKSN    NOT =   SPACE
                    ADD    1        TO   IDX8
                    MOVE   SYS-5000-CHIIKIKSN   TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
           END-IF
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¥¯¥í¡¼¥º
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *ÉÂÅï²Ã»»¾ðÊó
           PERFORM     4801-BTUSEL-SEC
      *
           IF   FLG-SYSKANRI           =    ZERO
      *
      *        ´Ç¸îÊä½õ²Ã»»
               IF    SYS-5001-BTU-KNGHJOKSN   NOT =   SPACE
                   ADD      1          TO     IDX8
                   EVALUATE     SYS-5001-BTU-KNGHJOKSN
                       WHEN     "01"
                           MOVE  "190103070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "02"
                           MOVE  "190103470"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "03"
                           MOVE  "190103770"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "04"
                           MOVE  "190103970"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "05"
                           MOVE  "190104070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
      *                Ê¿À®£±£¸Ç¯£´·î²þÀµÂÐ±þ
                       WHEN     "06"
                           MOVE  "190103770"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "07"
                           MOVE  "190103870"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "08"
                           MOVE  "190103970"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "09"
                           MOVE  "190104070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                   END-EVALUATE
               END-IF
      *
      *        Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»
               IF    SYS-5001-BTU-YAKANKNGKSN   NOT =   SPACE
                   ADD      1          TO     IDX8
                   EVALUATE     SYS-5001-BTU-YAKANKNGKSN
                       WHEN     "01"
                           MOVE  "190109870"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "02"
                           MOVE  "190076070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "03"
                           MOVE  "190076170"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "04"
                           MOVE  "190076370"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "05"
                           MOVE  "190076470"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                   END-EVALUATE
               END-IF
      *
      *        ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ²Ã»»¡ÊÊÝ°é»Î£±¿Í°Ê¾å¡Ë
               MOVE  SPACE       TO   WRK-BTU-HIKSISUKBN
               IF    SYOKIKSN-IN-KHNSRYCD      =   "190129310"  OR
                                                   "190111110"  OR
                                                   "190111210"  OR
                                                   "190075810"  OR
                                                   "190140710"
                   IF    SYS-5001-BTU-HIKSISUKBN   =   "2"
                       MOVE   "2"    TO   WRK-BTU-HIKSISUKBN
                   END-IF
               END-IF
      *
      *      ÉÂÅïÆþ±¡ÎÁ²Ã»»
      *
      *      Ê¿À®£²£°Ç¯£³·î¤Þ¤Ç¤Î²Ã»»¥»¥Ã¥È½èÍý
             IF     LNK-PTNYUINRRK-TENNYUYMD      <=     "20080331"
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (IDX7                    >   14)
                   EVALUATE   IDX7
                       WHEN   3
                       WHEN   4
                       WHEN   5
                           CONTINUE
      *                »ùÆ¸»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»¡Ê£²£°ºÍÌ¤Ëþ¡Ë
                       WHEN   6
                           IF      SYS-5001-KSNKBN(IDX7)   =   "2"
                               PERFORM   4801-PT-AGE-SEC
                               IF      WRK-NENREI          <      20
                                   ADD    1                TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                               WRK-NYUKHNKSN-SRYCD(IDX8)
                               END-IF
                           END-IF
      *                ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
                       WHEN   13
                           MOVE  SPACE       TO   WRK-BTU-KNGTEK-KBN
                           IF    SYS-5001-KNGTEK-KBN   =   "2"
                               MOVE   "2"    TO   WRK-BTU-KNGTEK-KBN
                           END-IF
      *                Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
                       WHEN   14
                           MOVE  SPACE       TO   WRK-BTU-DRTEK-KBN
                           IF    SYS-5001-DRTEK-KBN   =   "2"
                               MOVE   "2"    TO   WRK-BTU-DRTEK-KBN
                           END-IF
                       WHEN   OTHER
                           IF     SYS-5001-KSNKBN(IDX7)    =   "2"
                               ADD    1                TO  IDX8
                               MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                           END-IF
                   END-EVALUATE
               END-PERFORM
             END-IF
      *
      *      Ê¿À®£²£°Ç¯£´·î¤«¤éÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç¤Î²Ã»»¥»¥Ã¥È½èÍý
             IF    (LNK-PTNYUINRRK-TENNYUYMD  >=     "20080401")    AND
                   (LNK-PTNYUINRRK-TENNYUYMD  <=     "20100331")
                 PERFORM   VARYING   IDX7   FROM   1  BY  1
                   UNTIL  (IDX7      >   14)
                     IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                         EVALUATE    SYS-5001-KSNCD(IDX7)
      *                      »ùÆ¸»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»¡Ê£²£°ºÍÌ¤Ëþ¡Ë
                             WHEN    "190110170"
                                PERFORM   4801-PT-AGE-SEC
                                IF      WRK-NENREI   <      20
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ê²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÆþ±¡ÎÁ£±¡Ë
                             WHEN    "190129870"
                                IF  SYOKIKSN-IN-KHNSRYCD = "190129410"
                                                        OR "190129510"  
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      °å»ÕÇÛÃÖÅ¬¹ç²Ã»»¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ë
                             WHEN    "190117070"
                             WHEN    "190126570"
                             WHEN    "190109670"
                             WHEN    "190126670"
                             WHEN    "190126770"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190097110"
                                                        OR "190119510"
                                                        OR "190119610"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
                             WHEN    "190126470"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190097110"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190097310"
                                                        OR "190119710"
                                                        OR "190119810"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
                             WHEN   OTHER
                                ADD    1                TO  IDX8
                                MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                         END-EVALUATE
                     END-IF
                 END-PERFORM
             END-IF
      *
      *      Ê¿À®£²£²Ç¯£´·î¤«¤éÊ¿À®£²£´Ç¯£³·î¤Þ¤Ç¤Î²Ã»»¥»¥Ã¥È½èÍý
             IF    (LNK-PTNYUINRRK-TENNYUYMD  >=     "20100401")    AND
                   (LNK-PTNYUINRRK-TENNYUYMD  <=     "20120331")
      *         ¡Ê¥·¥¹¥Æ¥à´ÉÍý¤ÎÉÂÅïÀßÄê¸«Ä¾¤·¤Ë¤è¤ë¤â¤Î¡Ë
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (IDX7      >   14)
                     IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                         EVALUATE    SYS-5001-KSNCD(IDX7)
      *                      »ùÆ¸»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»¡Ê£²£°ºÍÌ¤Ëþ¡Ë
                             WHEN    "190110170"
                                PERFORM   4801-PT-AGE-SEC
                                IF      WRK-NENREI   <      20
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ê²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÆþ±¡ÎÁ£±¡Ë
                             WHEN    "190129870"
                                IF  SYOKIKSN-IN-KHNSRYCD = "190129410"
                                                        OR "190129510"
                                                        OR "190141610"
                                                        OR "190141710"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
      *                            ´ð½àÌ¤Å¬¹ç¤Î¾ì¹ç¤Ï½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ê´ð½àÌ¤Å¬¹ç¡Ë
      *                            ¤ËÃÖ¤­´¹¤¨¤ë
                                   IF SYOKIKSN-IN-KHNSRYCD = "190129410"
                                                          OR "190129510"
                                     MOVE   "190142370"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
                                END-IF
      *                      °å»ÕÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      °å»ÕÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
                             WHEN    "190135570"
                             WHEN    "190117070"
                             WHEN    "190109670"
                             WHEN    "190126570"
                             WHEN    "190126770"
                             WHEN    "190126670"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¤È£³¡Ë
                             WHEN    "190126470"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                                        OR "190135110"
                                                        OR "190135210"
                                                        OR "190135310"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡Ê£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ë
                             WHEN    "190131570"
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
                                   IF SYOKIKSN-IN-KHNSRYCD = "190077410"
                                     ADD    1            TO  IDX8
                                     MOVE   "190131570"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
                                   IF SYOKIKSN-IN-KHNSRYCD = "190085910"
                                     ADD    1            TO  IDX8
                                     MOVE   "190134870"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
                                   IF SYOKIKSN-IN-KHNSRYCD = "190090210"
                                     ADD    1            TO  IDX8
                                     MOVE   "190134970"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                      µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»¡Ê²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ¡Ë
                             WHEN    "190140870"
                                IF  SYOKIKSN-IN-KHNSRYCD = "190141610"
                                                        OR "190141710"
                                                        OR "190141810"
                                                        OR "190141910"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
                             WHEN   OTHER
                                ADD    1                TO  IDX8
                                MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                         END-EVALUATE
                     END-IF
               END-PERFORM
             END-IF
      *
      *      Ê¿À®£²£´Ç¯£´·î¤«¤é¤Î²Ã»»¥»¥Ã¥È½èÍý
             IF     LNK-PTNYUINRRK-TENNYUYMD      >=     "20120401"
      *         ¡Ê¥·¥¹¥Æ¥à´ÉÍý¤ÎÉÂÅïÀßÄê¸«Ä¾¤·¤Ë¤è¤ë¤â¤Î¡Ë
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (IDX7      >   14)
                     IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                         EVALUATE    SYS-5001-KSNCD(IDX7)
      *                      °å»ÕÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      °å»ÕÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
                             WHEN    "190135570"
                             WHEN    "190117070"
                             WHEN    "190109670"
                             WHEN    "190126570"
                             WHEN    "190126770"
                             WHEN    "190126670"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¤È£³¡Ë
                             WHEN    "190126470"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                                        OR "190135110"
                                                        OR "190135210"
                                                        OR "190135310"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡Ê£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ë
                             WHEN    "190131570"
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
                                   IF SYOKIKSN-IN-KHNSRYCD = "190077510"
                                     ADD    1            TO  IDX8
                                     MOVE   "190131570"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
                                   IF SYOKIKSN-IN-KHNSRYCD = "190090310"
                                     ADD    1            TO  IDX8
                                     MOVE   "190134970"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ£±¡Ë
                                   IF SYOKIKSN-IN-KHNSRYCD = "190151910"
                                     ADD    1            TO  IDX8
                                     MOVE   "190152570"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                      µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»¡Ê²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ¡Ë
                             WHEN    "190140870"
                                IF  SYOKIKSN-IN-KHNSRYCD = "190141610"
                                                        OR "190141710"
                                                        OR "190141810"
                                                        OR "190141910"
                                                        OR "190150110"
                                                        OR "190150210"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      ´Ç¸îÉ¬Í×ÅÙ²Ã»»£±¡Ê£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ë
                             WHEN    "190143070"
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£±¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
                                IF SYOKIKSN-IN-KHNSRYCD = "190077410"
                                   ADD    1            TO  IDX8
                                   MOVE   "190143070"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£±¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
                                IF SYOKIKSN-IN-KHNSRYCD = "190085910"
                                   ADD    1            TO  IDX8
                                   MOVE   "190143970"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£±¡ÊÀìÌçÉÂ±¡¡Ë
                                IF SYOKIKSN-IN-KHNSRYCD = "190090210"
                                   ADD    1            TO  IDX8
                                   MOVE   "190144570"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      ´Ç¸îÉ¬Í×ÅÙ²Ã»»£²¡Ê£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ë
                             WHEN    "190143170"
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£²¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
                                IF SYOKIKSN-IN-KHNSRYCD = "190077410"
                                   ADD    1            TO  IDX8
                                   MOVE   "190143170"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£²¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
                                IF SYOKIKSN-IN-KHNSRYCD = "190085910"
                                   ADD    1            TO  IDX8
                                   MOVE   "190144070"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£²¡ÊÀìÌçÉÂ±¡¡Ë
                                IF SYOKIKSN-IN-KHNSRYCD = "190090210"
                                   ADD    1            TO  IDX8
                                   MOVE   "190144670"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
                             WHEN   OTHER
                                ADD    1                TO  IDX8
                                MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                         END-EVALUATE
                     END-IF
               END-PERFORM
             END-IF
      *
      *      Æþ±¡ÎÁ²Ã»»¡Ê¥æ¡¼¥¶¡¼ÀßÄê¡Ë
             PERFORM   VARYING   IDX7   FROM   1  BY  1
               UNTIL  (IDX7      >   10)
                   IF   SYS-5001-KSNCD2(IDX7)  NOT =   SPACE
                       ADD   1     TO    IDX8
                       MOVE   SYS-5001-KSNCD2(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                   END-IF
             END-PERFORM
      *
           END-IF
      *
      *ÉÂ¼¼Æþ±¡ÎÁ²Ã»»
           PERFORM     4801-BRMSEL-SEC
      *
           IF   FLG-SYSKANRI       =    ZERO
      *
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (SYS-5002-KSNCD(IDX7)    =   SPACE)  OR
                        (IDX7                    >   12)
                     IF   SYS-5002-KSNKBN(IDX7)   =   "2"
                          ADD    1                TO  IDX8
                          MOVE   SYS-5002-KSNCD(IDX7)  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                     END-IF
               END-PERFORM
      *
           END-IF
      *
      *    Æþ±¡ÎÁ¤ÈÆþ±¡ÎÁ²Ã»»¤ÎÊ»»»Äê¥Á¥§¥Ã¥¯
           MOVE    ZERO            TO     IDX9
           PERFORM    VARYING    IDX8   FROM   1   BY   1
                      UNTIL      (WRK-NYUKHNKSN-SRYCD(IDX8)  =  SPACE)
                         OR      (IDX8   >     20)
                         PERFORM   4801-NYUKSN-CHK-SEC
                         IF     FLG-NYUKSN       =   ZERO   
                             ADD    1                TO  IDX9
                             MOVE   WRK-NYUKHNKSN-SRYCD(IDX8)  TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
                         END-IF
           END-PERFORM
      *
      *    ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ²Ã»»¡ÊÊÝ°é»Î£±¿Í°Ê¾å¡Ë
           IF    (WRK-BTU-HIKSISUKBN   =   "2")  AND
                 (IDX9    <   20 )
               ADD    1            TO  IDX9
               MOVE  "190111370"   TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
           END-IF
      *
           IF     LNK-PTNYUINRRK-TENNYUYMD    <=     "20080331"
      *      ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
             IF    (WRK-BTU-KNGTEK-KBN   =   "2")  AND
                   (IDX9    <   20 )
                 ADD    1            TO  IDX9
                 MOVE  "190109670"   TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
             END-IF
      *
      *      Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
             IF    (WRK-BTU-DRTEK-KBN   =   "2")  AND
                   (IDX9    <   20 )
                 ADD    1            TO  IDX9
                 MOVE  "190117070"   TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
             END-IF
           END-IF
      *
           .
      *
       4801-NYUKSN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»¤Î»»Äê²Ä¡¦ÉÔ²Ä¥Á¥§¥Ã¥¯
      *****************************************************************
       4801-NYUKSN-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUKSN
      *
      **     EVALUATE   WRK-NYUKHNKSN-SRYCD(IDX8)
      *        ¿ÇÎÅÏ¿´ÉÍýÂÎÀ©²Ã»»
      **         WHEN   "190100370"
      **             IF   SODRNACCT-IDOKBN   =  "07" OR "08"
      **                  MOVE   1        TO     FLG-NYUKSN
      **             END-IF
      *            ¼«±¡¤Ç¤Î·ÑÂ³Æþ±¡¥Á¥§¥Ã¥¯
      **     END-EVALUATE
      *
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯¥Æ¡¼¥Ö¥ë¤Ë¤è¤ëÈ½Äê
           INITIALIZE  NYUKSNCHK-REC
      *
           PERFORM   48011-TENSU-SEL-SEC
           MOVE    WRK-BTU-NYUKSNKBN
                                   TO  NYUKSNCHK-NYUINKBN
           PERFORM   48012-TENSU-SEL-SEC
           IF      TNS-NYUKHNKSNKBN    NOT =   ZERO
               MOVE    WRK-KSN-NYUKSNKBN
                                       TO  NYUKSNCHK-KSNKBN
               MOVE    SYOKIKSN-IN-SANTEISTYMD TO  NYUKSNCHK-YUKOSTYMD
                                                   NYUKSNCHK-YUKOEDYMD
      *
               MOVE    "tbl_nyuksnchk" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    NYUKSNCHK-REC   TO  MCPDATA-REC
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  NYUKSNCHK-REC
                   IF    ( NYUKSNCHK-CHKKBN    =   "1" OR "2" )
                       CONTINUE
                   ELSE
                       MOVE   1        TO     FLG-NYUKSN
                   END-IF
               ELSE
                   MOVE   1        TO     FLG-NYUKSN
               END-IF
      *
      *
               MOVE    "tbl_nyuksnchk"      TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
      *    Ï·¿ÍÆÃÄêÆþ±¡´ðËÜÎÁ¤Î²Ã»»¥Á¥§¥Ã¥¯Ï³¤ìÂÐ±þ
           IF   (WRK-BTU-NYUKSNKBN   =  "099")  AND
                (WRK-KSN-NYUKSNKBN   =  "407"   OR
                                        "408"   OR
                                        "409"   OR
                                        "410"   OR
                                        "411"   OR
                                        "412")
               MOVE   ZERO     TO     FLG-NYUKSN
           END-IF
      *
      *    Ãí²Ã»»¥³¡¼¥É¤¬Æ±°ì¤Ç¤¢¤ë¤³¤È¤ÎÈ½Äê
           IF   WRK-KSN-CDKBN-KBNNUM    NOT =    ZERO
              IF  (WRK-BTU-CDKBN-KBNNUM  = WRK-KSN-CDKBN-KBNNUM)  AND
                  (WRK-BTU-CDKBN-KBNNUM-EDA = WRK-KSN-CDKBN-KBNNUM-EDA)
                  CONTINUE
              ELSE
                  MOVE    1     TO    FLG-NYUKSN
              END-IF
           END-IF
      *
           .
      *
       4801-NYUKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý¡ÊÆþ±¡ÎÁ¡Ë
      *****************************************************************
       48011-TENSU-SEL-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-NYUKSNKBN
           MOVE    ZERO            TO  WRK-BTU-CDKBN-KBNNUM
           MOVE    ZERO            TO  WRK-BTU-CDKBN-KBNNUM-EDA
           MOVE    SPACE           TO  WRK-TOKUBETSU-NYUIN
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE              TNS-TENSU-REC
      *
           IF      LNK-SCNYUACCT-NYUKHN-SRYCD(1) NOT = SPACE
               MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                   TO  TNS-SRYCD
           ELSE
               MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                   TO  TNS-SRYCD
           END-IF
           MOVE    SYOKIKSN-IN-SANTEISTYMD    TO  TNS-YUKOSTYMD
                                                  TNS-YUKOEDYMD
           MOVE    LNK-PTNYUINRRK-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¥¯¥í¡¼¥º
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-TENSU       NOT =   ZERO )
               GO  TO  48011-TENSU-SEL-EXT
           END-IF
      *
           MOVE    TNS-NYUKHNKSNKBN
                                    TO  WRK-BTU-NYUKSNKBN
           MOVE    TNS-CDKBN-KBNNUM TO  WRK-BTU-CDKBN-KBNNUM
           MOVE    TNS-CDKBN-KBNNUM-EDA TO WRK-BTU-CDKBN-KBNNUM-EDA
      *
      *    ÆÃÊÌÆþ±¡ÎÁ¤ÎÈ½Äê
           EVALUATE   TNS-NYUKHNKBN
               WHEN   73
               WHEN   74
               WHEN   75
               WHEN   76
               WHEN   77
               WHEN   78
               WHEN   79
               WHEN   83
               WHEN   84
                   MOVE    "1"     TO  WRK-TOKUBETSU-NYUIN
           END-EVALUATE
           IF  TNS-SRYCD      =    "190121810"
                   MOVE    "1"     TO  WRK-TOKUBETSU-NYUIN
           END-IF
           .
      *
       48011-TENSU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý¡ÊÆþ±¡ÎÁ²Ã»»¡Ë
      *****************************************************************
       48012-TENSU-SEL-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-NYUKSNKBN
           MOVE    ZERO            TO  WRK-KSN-CDKBN-KBNNUM
           MOVE    ZERO            TO  WRK-KSN-CDKBN-KBNNUM-EDA
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE              TNS-TENSU-REC
      *
      *    ÆÃÊÌÆþ±¡ÎÁ»»Äê»þ¤ËÍÄ»ù²Ã»»¤ÈÆýÍÄ»ù²Ã»»¤Î²Ã»»¥³¡¼¥ÉÃÖ¤­´¹¤¨
           IF      WRK-TOKUBETSU-NYUIN   =   "1"
               EVALUATE   WRK-NYUKHNKSN-SRYCD(IDX8)
                   WHEN   "190100470"
                       MOVE  "190100570"  TO  WRK-NYUKHNKSN-SRYCD(IDX8)
                   WHEN   "190100770"
                       MOVE  "190100870"  TO  WRK-NYUKHNKSN-SRYCD(IDX8)
               END-EVALUATE
           END-IF
      *
           MOVE    WRK-NYUKHNKSN-SRYCD(IDX8)
                                   TO  TNS-SRYCD
           MOVE    SYOKIKSN-IN-SANTEISTYMD    TO  TNS-YUKOSTYMD
                                                  TNS-YUKOEDYMD
           MOVE    LNK-PTNYUINRRK-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¥¯¥í¡¼¥º
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-TENSU       NOT =   ZERO )
               GO  TO  48012-TENSU-SEL-EXT
           END-IF
      *
           MOVE    TNS-NYUKHNKSNKBN
                                   TO  WRK-KSN-NYUKSNKBN
      *
      *    Æþ±¡ÎÁ¡Ê£Á£±£°£°ÈÖÂæ¡Ë¡¢ÆÃÄêÆþ±¡ÎÁ¡Ê£Á£³£°£°ÈÖÂæ¡Ë¤Ë´Þ¤Þ¤ì¤ë
      *    Ãí²Ã»»¤Î¤ß¤¬È½ÄêÂÐ¾Ý¤È¤Ê¤ë
           IF      (TNS-CDKBN-KBN       =   "A")     AND
                   (TNS-CDKBN-KBNNUM   >=   100)     AND
                   (TNS-CDKBN-KBNNUM   <=   199)
                MOVE  TNS-CDKBN-KBNNUM TO  WRK-KSN-CDKBN-KBNNUM
                MOVE  TNS-CDKBN-KBNNUM-EDA
                                       TO  WRK-KSN-CDKBN-KBNNUM-EDA
           END-IF
      *
           IF      (TNS-CDKBN-KBN       =   "A")     AND
                   (TNS-CDKBN-KBNNUM   >=   300)     AND
                   (TNS-CDKBN-KBNNUM   <=   399)
                MOVE  TNS-CDKBN-KBNNUM TO  WRK-KSN-CDKBN-KBNNUM
                MOVE  TNS-CDKBN-KBNNUM-EDA
                                       TO  WRK-KSN-CDKBN-KBNNUM-EDA
           END-IF
           .
      *
       48012-TENSU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡´µ¼Ô¤ÎÇ¯Îð»»½Ð
      *****************************************************************
       4801-PT-AGE-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC       =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF      FLG-PTINF           =   1
               MOVE    ZERO            TO  WRK-NENREI
           ELSE
               MOVE   PTINF-BIRTHDAY   TO  WRK-YMDS1
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  WRK-YMDS2
               MOVE   ZERO             TO  WRK-NENREI
               COMPUTE  WRK-NENREI     =   WRK-YYS2
                                       -   WRK-YYS1
               IF   WRK-YMDS1(5:4)     >   WRK-YMDS2(5:4)
                    ADD   -1           TO  WRK-NENREI
               END-IF
           END-IF
      *
           .
      *
       4801-PT-AGE-EXT.
           EXIT.
      *
      *****************************************************************
      *     ÁªÄêÎÅÍÜÈñ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ½èÍý
      *****************************************************************
       4801-SENTEI-SET-SEC         SECTION.
      *
           INITIALIZE  SENTEICDCHG-REC
           MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                   TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC TO  MCPDATA-REC
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC TO  SENTEICDCHG-REC
           ELSE
               INITIALIZE              SENTEICDCHG-REC
           END-IF
      *
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      **     MOVE    SENCHG-SENTEISRYCD
      **                             TO  LNK-SCNYUACCT-SENTEI-SRYCD
           IF      SENCHG-SENTEISRYCD    NOT =  SPACE
               MOVE    "099999916"   TO  LNK-SCNYUACCT-SENTEI-SRYCD
               MOVE    WRK-SENTEI-STYMD
                                     TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    £±£µºÍÌ¤Ëþ¤Î´µ¼Ô¤ÏÁªÄêÆþ±¡ÂÐ¾Ý³°
           DISPLAY "PTINF-BIRTHDAY = " PTINF-BIRTHDAY
           MOVE     PTINF-BIRTHDAY   TO  WRK-BIRTHDAY-15
           ADD      15               TO  WRK-BIRTHDAY-15Y
           DISPLAY  "WRK-SENTEI-STYMD = " WRK-SENTEI-STYMD
           DISPLAY  "WRK-BIRTHDAY-15 = " WRK-BIRTHDAY-15
           IF       WRK-SENTEI-STYMD <=  WRK-BIRTHDAY-15
               MOVE   WRK-BIRTHDAY-15  TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *
      *
           .
       4801-SENTEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÄÌ»»Æü¿ô¼èÆÀ½èÍý
      *****************************************************************
       4801-TSUSAN-SANTEI-GET-SEC      SECTION.
      *
           INITIALIZE                  WRK-TSUSAN
                                       WRK-SANTEISTYMD
      *
           INITIALIZE                  ORCSTUSANAREA
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID TO  STUSAN-PTID
           MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                   TO  STUSAN-KJNYMD
      *    ÆÃÄêÆþ±¡¤Ç¤Î»»ÄêºÑÆü¿ô¤ò¼èÆÀÍÑ
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê¹âÅÙ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê£Á¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê¹âÅÙ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê£Á¡Ë
      *    °ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
      *    ¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *    ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ
      *    Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡Ê»ØÄêÃÏ°è¡Ë
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê»ØÄêÃÏ°è¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
      *    ¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡Ë
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡¦¥ê¥Ï¥Ó¥ê»»Äê´µ¼Ô¡Ë
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               EVALUATE   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   WHEN   "03"
                       MOVE   "190024510"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190128610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190074510"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190714410"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190714510"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190714610"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "04"
                       MOVE   "190110210"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190129110"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190110610"  TO  STUSAN-TOKUTEI-SRYCD(3)
                   WHEN   "05"
                       MOVE   "190110310"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190128910"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190110710"  TO  STUSAN-TOKUTEI-SRYCD(3)
                   WHEN   "06"
                       MOVE   "190024310"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190128710"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190024410"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190714710"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190714810"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190714910"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "07"
                       MOVE   "190110410"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190129210"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190110810"  TO  STUSAN-TOKUTEI-SRYCD(3)
                   WHEN   "08"
                       MOVE   "190110510"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190129010"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190110910"  TO  STUSAN-TOKUTEI-SRYCD(3)
                   WHEN   "09"
                   WHEN   "16"
                       MOVE   "190024610"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190074610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190116310"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190116410"  TO  STUSAN-TOKUTEI-SRYCD(4)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190717210"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190717310"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "14"
                       MOVE   "190075510"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190075610"  TO  STUSAN-TOKUTEI-SRYCD(2)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190718710"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190718810"  TO  STUSAN-TOKUTEI-SRYCD(4)
                   WHEN   "13"
                       MOVE   "190024810"  TO  STUSAN-TOKUTEI-SRYCD(1)
                   WHEN   "17"
                       MOVE   "190117310"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190717810"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "18"
                       MOVE   "190117410"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190720110"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "19"
                       MOVE   "190120810"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190717910"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "54"
                       MOVE   "190129910"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190720210"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "55"
                       MOVE   "190138110"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190138210"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190138310"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190715010"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190715110"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190715210"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "56"
                       MOVE   "190138410"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190138510"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190138610"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190715310"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190715410"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190715510"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "57"
                       MOVE   "190138710"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190138810"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190138910"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190715610"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190715710"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190715810"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "58"
                       MOVE   "190139010"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190139110"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190139210"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190139310"  TO  STUSAN-TOKUTEI-SRYCD(4)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190715910"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190716010"  TO  STUSAN-TOKUTEI-SRYCD(6)
                       MOVE   "190716110"  TO  STUSAN-TOKUTEI-SRYCD(7)
                       MOVE   "190716210"  TO  STUSAN-TOKUTEI-SRYCD(8)
                   WHEN   "59"
                       MOVE   "190139810"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190139910"  TO  STUSAN-TOKUTEI-SRYCD(2)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190717410"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190717510"  TO  STUSAN-TOKUTEI-SRYCD(4)
                   WHEN   "60"
                       MOVE   "190140010"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190140110"  TO  STUSAN-TOKUTEI-SRYCD(2)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190717610"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190717710"  TO  STUSAN-TOKUTEI-SRYCD(4)
                   WHEN   "91"
                       MOVE   "190149910"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190150010"  TO  STUSAN-TOKUTEI-SRYCD(2)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190718010"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190718110"  TO  STUSAN-TOKUTEI-SRYCD(4)
                   WHEN   "92"
                       MOVE   "190150310"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190720310"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "93"
                       MOVE   "190150410"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190720410"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "95"
                       MOVE   "190152610"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190723110"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "96"
                       MOVE   "190152710"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190723210"  TO  STUSAN-TOKUTEI-SRYCD(2)
               END-EVALUATE
           END-IF
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡ÊµÞÀ­Áý°­¤ÎÅ¾Åï¡Ë
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¡ÊµÞÀ­Áý°­¤ÎÅ¾Åï¡Ë
      *    Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
      *    Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *    ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "2"
               EVALUATE   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   WHEN   "11"
                       MOVE   "190121110"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190721410"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "12"
                       MOVE   "190121210"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190721610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "24"
                       MOVE   "190121110"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190721410"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "25"
                       MOVE   "190121210"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190721610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "05"
                       MOVE   "190151610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190739910"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190740010"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190722310"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190722410"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190722510"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "15"
                       MOVE   "190151710"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190813410"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190813510"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190722610"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190722710"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190722810"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "09"
                       MOVE   "190028910"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190150510"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190150610"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                ¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
                       MOVE   "190720710"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190720810"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190720910"  TO  STUSAN-TOKUTEI-SRYCD(6)
               END-EVALUATE
           END-IF
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF    ( STUSAN-RC       NOT =   ZERO )
      *         MOVE    "4025"      TO  WRK-ERRCD
               DISPLAY  "WRK-ERRCD = " WRK-ERRCD
      *         GO  TO  4801-TSUSAN-SANTEI-GET-EXT
           END-IF
      *
           COMPUTE WRK-TSUSAN  =   STUSAN-NISSU03  -   1
           MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SANTEISTYMD
      *
           MOVE    WRK-SANTEISTYMD
                                   TO  WRK-YMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           IF    ( STUSAN-KJNYMD-CHK-FLG  =   ZERO )
               COMPUTE WRK-ZOGEN   =   180 -   STUSAN-NISSU04
           ELSE
               COMPUTE WRK-ZOGEN   =   180 - ( STUSAN-NISSU04  -   1 )
           END-IF
      *
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  WRK-SENTEI-STYMD
      *
           .
       4801-TSUSAN-SANTEI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂÅï¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       4801-BTUSEL-SEC             SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    LNK-PTNYUINRRK-BTUNUM
                                   TO  SYS-KBNCD
           EVALUATE   SODRNACCT-ORDERID(3:2)
               WHEN   "01"
               WHEN   "03"
               WHEN   "04"
               WHEN   "05"
                   MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
      **         WHEN   "08"
      **             MOVE   WRK-NAI-I01-IDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
      **         WHEN   "09"
      **             MOVE   SPA-NAI-I01-MAEIDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
               WHEN   OTHER
                   MOVE    SPA-SYSYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           END-EVALUATE
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                   TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO  SYS-5001-REC
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-BTUSEL-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂ¼¼¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       4801-BRMSEL-SEC             SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    LNK-PTNYUINRRK-BRMNUM
                                   TO  SYS-KBNCD
           EVALUATE   SODRNACCT-ORDERID(3:2)
               WHEN   "01"
               WHEN   "03"
               WHEN   "04"
               WHEN   "05"
                   MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
      **         WHEN   "08"
      **             MOVE   WRK-NAI-I01-IDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
      **         WHEN   "09"
      **             MOVE   SPA-NAI-I01-MAEIDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
               WHEN   OTHER
                   MOVE    SPA-SYSYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           END-EVALUATE
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                   TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO  SYS-5002-REC
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-BRMSEL-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÄêÆþ±¡ÎÁ¥»¥Ã¥È½èÍý
      *****************************************************************
       4801-TOKTEI-NYUIN-SET-SEC   SECTION.
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤ò¥µ¥Ö¥×¥í¤è¤ê¼èÆÀ
           INITIALIZE                           ORCSTOKNYUIN-AREA
           MOVE    LNK-PTNYUINRRK-TOKUTEI-KBN   TO  LNK-TOKNYUIN-KBN
           MOVE    LNK-PTNYUINRRK-TOKUTEI-NYUIN TO  LNK-TOKNYUIN-NYUIN
           MOVE    LNK-PTNYUINRRK-BTUNUM        TO  LNK-TOKNYUIN-BTUNUM
           MOVE    SYOKIKSN-IN-SANTEISTYMD(1:6)
                                            TO  LNK-TOKNYUIN-SANTEIYM
           CALL    "ORCSTOKNYUIN"      USING    ORCSTOKNYUIN-AREA
                                                SPA-AREA
           IF      LNK-TOKNYUIN-RC          =   ZERO
               MOVE   LNK-TOKNYUIN-SRYCD    TO  SYOKIKSN-IN-KHNSRYCD
           END-IF
      *
           .
       4801-TOKTEI-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ°ìÈÌ¥³¡¼¥É¤«¤éÏ·¿Í¥³¡¼¥É¤Ø¤ÎÃÖ¤­´¹¤¨½èÍý
      *****************************************************************
       4801-RJNCD-CHG-SEC   SECTION.
      *
      *    Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                   TO  WRK-AGECHK-SRYYMD
           PERFORM 4801-RJN-TAISYO-CHK-SEC
           IF    ( FLG-RJN         =   1 )    AND
                 ( COMB-HKNNUM     NOT =   "971" )  AND
                 ( COMB-HKNNUM     NOT =   "973" )
      *
      *        Ê¿À®£±£¸Ç¯£´·î²þÄêÂÐ±þ
      *        Ï·¿Í¥³¡¼¥ÉÃÖ¤­´¹¤¨¤ÏÎÅÍÜÉÂÅï¡¢ÎÅÍÜÉÂ¾²¤Î¤ß
               IF   SYOKIKSN-IN-SANTEISTYMD(1:6)  >=  "200604"
                   IF   SYOKIKSN-IN-KHNSRYCD  =  "190080610" OR
                                                 "190080710" OR
                                                 "190081310" OR
                                                 "190081410" OR
                                                 "190098210" OR
                                                 "190098310"
                       INITIALIZE              SRYCDCHG-REC
                       MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                       MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                       MOVE    "tbl_srycdchg"  TO MCP-TABLE
                       MOVE    "key2"          TO MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC 
                       IF    ( MCP-RC      =   ZERO )
                           MOVE    MCPDATA-REC
                                               TO   SRYCDCHG-REC
                           IF   CHG-RJNSRYCD   NOT =   SPACE
                                MOVE   CHG-RJNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                           END-IF
                       END-IF
      *
                       MOVE    "tbl_srycdchg"  TO MCP-TABLE
                       MOVE    "key2"          TO MCP-PATHNAME
                       PERFORM 910-DBCLOSECURSOR-SEC
                   END-IF
               ELSE
                   INITIALIZE              SRYCDCHG-REC
                   MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                   MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                   MOVE    "tbl_srycdchg"  TO MCP-TABLE
                   MOVE    "key2"          TO MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC 
                   IF    ( MCP-RC      =   ZERO )
                       MOVE    MCPDATA-REC
                                           TO   SRYCDCHG-REC
                       IF   CHG-RJNSRYCD   NOT =   SPACE
                            MOVE   CHG-RJNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_srycdchg"  TO MCP-TABLE
                   MOVE    "key2"          TO MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *
           .
       4801-RJNCD-CHG-EXT.
           EXIT.
      *****************************************************************
      *    Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
      *****************************************************************
       4801-RJN-TAISYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-RJN
      *
      *    Æþ±¡´µ¼Ô¤ÎÏ·¿ÍÊÝ¸±È½Äê
           MOVE    ZERO                TO   FLG-HKNCOMBI
           INITIALIZE              HKNCOMBI-REC
      *
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO   COMB-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID
                                       TO   COMB-PTID
           IF      SODRNACCT-ORDERID(3:2) =   "07"  OR  "09"
               MOVE    WRK-TAIHI-HKN   TO   COMB-HKNCOMBINUM
           ELSE
               MOVE    LNK-PTNYUINRRK-HKNCOMBINUM
                                       TO   COMB-HKNCOMBINUM
           END-IF
           MOVE    HKNCOMBI-REC        TO   MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC 
           IF    ( MCP-RC    =   ZERO )
               MOVE    MCPDATA-REC
                                       TO   HKNCOMBI-REC
           ELSE
               MOVE    1               TO   FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM  4801-PT-AGE-SEC
      *    Æþ±¡´µ¼Ô¤ÎÇ¯Îð·×»»
           INITIALIZE ORCSAGECHKAREA
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  AGECHK-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID
                                       TO  AGECHK-PTID
           MOVE    WRK-AGECHK-SRYYMD
                                       TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY
                                       TO  AGECHK-BIRTHDAY
           DISPLAY "AGECHK-SRYYMD" AGECHK-SRYYMD
           DISPLAY "AGECHK-BIRTHDAY" AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF    ( AGECHK-RC       =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
      *    Ï·¿ÍÂÐ¾Ý´µ¼ÔÈ½Äê
           IF    ((FLG-HKNCOMBI     =   ZERO)    AND
                  (COMB-KOH1HKNNUM  =   "027"))      OR
                 ( AGECHK-NENREI   >=    75)         OR
                 ((AGECHK-NENREI   >=    70)     AND
                  (PTINF-BIRTHDAY  <=   "19320930")) OR
                  (AGECHK-ROUJIN    =    1)
               MOVE    1            TO  FLG-RJN
           END-IF
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó¤«¤éÏ·¿ÍÊÝ·ò¤ÎÅÐÏ¿¥Á¥§¥Ã¥¯
           INITIALIZE                    PTKOHINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO   PTKOH-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID    TO   PTKOH-PTID
           MOVE    "027"            TO   PTKOH-KOHNUM
           MOVE    WRK-AGECHK-SRYYMD
                                    TO   PTKOH-TEKSTYMD
           MOVE    WRK-AGECHK-SRYYMD
                                    TO   PTKOH-TEKEDYMD
           MOVE    PTKOHINF-REC     TO   MCPDATA-REC
           MOVE    "tbl_ptkohinf"   TO   MCP-TABLE
           MOVE    "key6"           TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC 
           IF    ( MCP-RC    =   ZERO )
               MOVE    1            TO  FLG-RJN
           END-IF
           MOVE    "tbl_ptkohinf"   TO   MCP-TABLE
           MOVE    "key6"           TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-RJN-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¼èÆÀ½èÍý
      *****************************************************************
       490-HKNCOMBI-GET-SEC            SECTION.
      *
           INITIALIZE                  SPA-KYOTUKEY
                                       SPA-KYOTU-SET
                                       ORCSHKNSETAREA
      *
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID     TO  SPA-PTID
           MOVE    PTINF-BIRTHDAY
                                       TO  SPA-BIRTHDAY
           MOVE    "2"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
               GO  TO  490-HKNCOMBI-GET-EXT
           END-IF
      *
           .
       490-HKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
      *****************************************************************
       490-HKNCOMBI-EDIT-SEC           SECTION.
      *
           MOVE    SPACE                   TO  WRK-HKNCOMBI
           PERFORM VARYING IDX15   FROM 1  BY  1
                   UNTIL ( IDX15   >   ORCSHKN-HKN-MAX )
                     OR  ( IDX15   >   100 )
      *
               IF    ( ORCSHKN-GMN-HKNCOMBINUM (IDX15)
                                   =   WRK-ORCSHKN-HKNCOMBINUM )
                   MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX15)
                                           TO  WRK-HKNCOMBI
                   MOVE    ORCSHKN-HKN-MAX TO  IDX15
               END-IF
      *
           END-PERFORM
      *
           .
       490-HKNCOMBI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÆüÉÕ¥Á¥§¥Ã¥¯¡¦ÊÔ½¸½èÍý
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
           IF    ( WRK-YMD         =   SPACE )
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
      *    ÆüÉÕ²èÌÌ¥Á¥§¥Ã¥¯¥µ¥Ö
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  WRK-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´ü´Ö»»½Ð½èÍý
      *****************************************************************
       5002-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSDAY
                                           WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF      ( STS-DAY-RC1   NOT =   ZERO )
               MOVE    1           TO  FLG-ORCSDAY
               GO  TO              5002-KIKAN-CALC-EXT
           END-IF
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       5002-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    ¥«¥ì¥ó¥À¡¼½èÍý
      *****************************************************************
       5002-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       5002-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    Å¾²Ê¡¿Å¾Åï¡¿Å¾¼¼»þ¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-TENROOM-EDIT-SEC         SECTION.
      *
           MOVE      SPACE        TO    WRK-DOU-TENNYUYMD
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
           MOVE    SPACE   TO   SPA-ERRCD
      *    ÄÌ»»Æü¿ô¡¢»»Äê³«»ÏÆü¤òµá¤á¤ë
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-TENROOM-EDIT-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ»»Äê»þ¤ËÆÃÄêÆþ±¡¤Ç¤Î»»ÄêÆü¤¬£±£´Æü¤ò
      *    ·Ð²á¤·¤Æ¤¤¤¿¾ì¹ç¤Ï°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
      *    °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤âÆ±ÍÍ
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²
      *    °ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
      *    ¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "03" OR "04" OR "05" OR
                                                 "06" OR "07" OR "08" OR
                                                 "09" OR "14" OR "16" OR
                                                 "55" OR "57" OR "59" OR
                                                 "91"
                   IF   STUSAN-NISSU06   >=   14
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    ¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ»»Äê»þ¤ËÆÃÄêÆþ±¡¤Ç¤Î»»ÄêÆü¤¬£¶£°Æü¤ò
      *    ·Ð²á¤·¤Æ¤¤¤¿¾ì¹ç¤Ï°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "13"
                   IF   STUSAN-NISSU06   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ»»Äê»þ¤ËÆÃÄêÆþ±¡¤Ç¤Î»»ÄêÆü¤¬£²£±Æü¤ò
      *    ·Ð²á¤·¤Æ¤¤¤¿¾ì¹ç¤Ï°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "17"
                   IF   STUSAN-NISSU06   >=   21
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ»»Äê»þ¤ËÆÃÄêÆþ±¡¤Ç¤Î»»ÄêÆü¤¬£¹£°Æü¤ò
      *    ·Ð²á¤·¤Æ¤¤¤¿¾ì¹ç¤Ï°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"
                   IF   STUSAN-NISSU06   >=   90
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²»»Äê»þ¤ËÆÃÄêÆþ±¡¤Ç¤Î»»ÄêÆü¤¬£¶£°Æü¤ò
      *    ·Ð²á¤·¤Æ¤¤¤¿¾ì¹ç¤Ï°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "54"
                   IF   STUSAN-NISSU06   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡Ê»ØÄêÃÏ°è¡Ë»»Äê»þ¤ËÆÃÄêÆþ±¡¤Ç¤Î»»ÄêÆü¤¬£¶£°Æü¤ò
      *    ·Ð²á¤·¤Æ¤¤¤¿¾ì¹ç¤Ï°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "92"
                   IF   STUSAN-NISSU06   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê»ØÄêÃÏ°è¡Ë»»Äê»þ¤ËÆÃÄêÆþ±¡¤Ç¤Î»»ÄêÆü¤¬£¶£°Æü¤ò
      *    ·Ð²á¤·¤Æ¤¤¤¿¾ì¹ç¤Ï°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "93"
                   IF   STUSAN-NISSU06   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡Ë»»Äê»þ¤ËÆÃÄêÆþ±¡¤Ç¤Î
      *    »»ÄêÆü¤¬£¶£°Æü¤ò·Ð²á¤·¤Æ¤¤¤¿¾ì¹ç¤Ï°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "95"
                   IF   STUSAN-NISSU06   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡¦¥ê¥Ï¥Ó¥ê»»Äê´µ¼Ô¡Ë
      *    »»Äê»þ¤ËÆÃÄêÆþ±¡¤Ç¤Î»»ÄêÆü¤¬£¶£°Æü¤ò·Ð²á¤·¤Æ¤¤¤¿¾ì¹ç¤Ï°ìÈÌ¤Î
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "96"
                   IF   STUSAN-NISSU06   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë¤Î¾ì¹ç¤ÏºÇÄ¹£¶£°Æü´Ö¤Î»»Äê¤Î°Ù¡¢
      *    ¼¡·î¤Î²ñ·×ºîÀ®»þ¤Ë£¶£°Æü·Ð²á»þ¤ÏÌµ¾ò·ï¤Ë°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "56"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë¤Î¾ì¹ç¤ÏºÇÄ¹£¶£°Æü´Ö¤Î»»Äê¤Î°Ù¡¢
      *    ¼¡·î¤Î²ñ·×ºîÀ®»þ¤Ë£¶£°Æü·Ð²á»þ¤ÏÌµ¾ò·ï¤Ë°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "58"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë¤Î¾ì¹ç¤ÏºÇÄ¹£¶£°Æü´Ö¤Î»»Äê¤Î°Ù¡¢
      *    ¼¡·î¤Î²ñ·×ºîÀ®»þ¤Ë£¶£°Æü·Ð²á»þ¤ÏÌµ¾ò·ï¤Ë°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "60"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ»»Äê»þ¤ËÆÃÄêÆþ±¡¤Ç¤Î»»ÄêÆü¤¬£±£´Æü¤ò
      *    ·Ð²á¤·¤Æ¤¤¤¿¾ì¹ç¤Ï°ìÈÌ¤ÎÆþ±¡ÎÁ»»Äê¤È¤¹¤ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "19"
                   IF   STUSAN-NISSU07   >=   14
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¤Î¾ì¹ç¤Ï´ü´Ö²Ã»»¤¬Â¸ºß¤¹¤ë¤¿¤á¡¢
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÎÅÍÜÉÂ¾²¤ÇÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤òÁê¸ß»»Äê¤¹¤ë¾ì¹ç¤â
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4021"      TO  SPA-ERRCD
                       GO  TO  4801-TENROOM-EDIT-EXT
                   END-IF
               END-IF
           ELSE
               PERFORM   4801-BTUSEL-SEC
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4021"      TO  SPA-ERRCD
                       GO  TO  4801-TENROOM-EDIT-EXT
                   END-IF
               ELSE
                   MOVE    "4022"      TO  SPA-ERRCD
                   GO  TO  4801-TENROOM-EDIT-EXT
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®Á°¤Ë²ñ·×¥Ç¡¼¥¿¤Î¿©»ö¶èÊ¬¤òÂàÈò
           IF  LNK-PTNYUINRRK-TENNYUYMD(1:6)  >= "200604"
      *      ¡Ê°ÛÆ°·îÊ¬¡Ë
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
               UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                           TO  NACCT-SRYYM
                                               WRK-TENROOM-SKJ-SRYYMX
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          ¿©»ö¤òÂàÈò
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
               ELSE
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
                 MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(1)
               END-IF
      *        Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
      *      ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
                UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               IF   IDX-SKJ-TAIHI     =    1
                   IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                       MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
                   ELSE
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
                   END-IF
               END-IF
               MOVE    WRK-TENROOM-SKJ-SRYYMX
                                           TO  NACCT-SRYYM
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC      TO  NYUINACCT-REC
      *          ¿©»ö¤òÂàÈò
                 MOVE    NACCT-SRYYM       TO  WRK-TENROOM-SKJ-SRYYM(2)
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ18(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  2
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ19(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  3
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ20(2 IDX-SKJ)
                       END-PERFORM
                 END-EVALUATE
               ELSE
                   MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(2)
               END-IF
      *        Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
           ELSE
      *      ¡Ê°ÛÆ°·îÊ¬¡Ë
             INITIALIZE                      NYUINACCT-REC
             MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
             MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
             MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                         TO  NACCT-SRYYM
                                             WRK-TENROOM-SKJ-SRYYMX
      *
             MOVE    NYUINACCT-REC       TO  MCPDATA-REC
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM   910-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          ¿©»ö¤òÂàÈò
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   NACCT-DAY(IDX-SKJ)
                                         TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                 END-PERFORM
             ELSE
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   ZERO        TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                 END-PERFORM
             END-IF
      *      Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
      *      ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
             INITIALIZE                      NYUINACCT-REC
             MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
             MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
             IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                 ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                 MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
             ELSE
                 ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
             END-IF
             MOVE    WRK-TENROOM-SKJ-SRYYMX
                                         TO  NACCT-SRYYM
      *
             MOVE    NYUINACCT-REC       TO  MCPDATA-REC
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM   910-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          ¿©»ö¤òÂàÈò
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
                 PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   NACCT-DAY(IDX-SKJ)
                                         TO  WRK-TENROOM-SKJ(2 IDX-SKJ)
                 END-PERFORM
             ELSE
                 MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(2)
             END-IF
      *      Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE                     ORCSNYUACCT-AREA
           MOVE    "8"                TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE    PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                           TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                           TO   LNK-SCNYUACCT-STSRYYMD
           MOVE    LNK-PTNYUINRRK-TENNYUYMD
                           TO   LNK-SCNYUACCT-EDSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    °åÎÅ´Ñ»¡Æþ±¡¥»¥Ã¥È
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     LNK-PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     LNK-PTNYUINRRK-TOKUTEI-KBN
                                             TO  LNK-IRYOKNS-KBN
           MOVE     LNK-PTNYUINRRK-TOKUTEI-NYUIN
                                             TO  LNK-IRYOKNS-NYUIN
           MOVE     LNK-PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
           DISPLAY  "==  ORCSIRYOKNS CALL ==="
           DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                        TO  LNK-SCNYUACCT-STSRYYMD
               MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                        TO  LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    ÄÌ»»»»ÄêºÑÆü¿ô
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    ÆÃÄêÆþ±¡¤ÇÆþ±¡ÎÁ¤¬ÊÑ¹¹¤Ë¤Ê¤ë¥³¡¼¥É¤ÎÂÐ±þ
      *
      *    ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190774110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  14
                     ADD    1           TO  TN-IDX
                     MOVE   "190774110" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ¤Î£±£´Æü°ÊÆâ¤Î½ªÆü¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU01
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190774210"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ¡ÊÀ¸³èÎÅÍÜ¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190125010"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  14
                     ADD    1           TO  TN-IDX
                     MOVE   "190125010" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ¤Î£±£´Æü°ÊÆâ¤Î½ªÆü¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU01
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190125110"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡ÊÊ¿À®£²£´Ç¯£³·î¤Þ¤Ç¤Î´ü´ÖÂÐ±þ¡Ë
           IF   (LNK-PTNYUINRRK-TENNYUYMD     >=  "20100401")   AND
                (LNK-PTNYUINRRK-TENNYUYMD     <=  "20120331")
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190739910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU03      <=  60
                     ADD    1           TO  TN-IDX
                     MOVE   "190739910" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¤Î£¶£°Æü°ÊÆâ¤Î½ªÆü¤ò
      *              ºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU03
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190740010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
             END-IF
           END-IF
      *
      *    Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡ÊÊ¿À®£²£´Ç¯£´·î¤«¤é¤Î´ü´ÖÂÐ±þ¡Ë
           IF    LNK-PTNYUINRRK-TENNYUYMD     >=  "20120401"
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190151610"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU03      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190151610" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¤Î£³£°Æü°ÊÆâ¤Î½ªÆü
      *              ¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU03
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU03      <=  60
                     ADD    1           TO  TN-IDX
                     MOVE   "190739910" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¤Î£¶£°Æü°ÊÆâ¤Î³«»ÏÆü¥»¥Ã¥È
                     IF     TN-IDX        =      1
                         MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     END-IF
                     IF     TN-IDX       =      2
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                                TO  LNK-DAY6-KIJUN
                         MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                         MOVE     1             TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                        LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     END-IF
      *              Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¤Î£¶£°Æü°ÊÆâ¤Î½ªÆü
      *              ¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU03
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190740010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     IF   TN-IDX      =     3
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                            TO  LNK-DAY6-KIJUN
                         MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                         MOVE     1         TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN
                                   TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                           (TN-IDX)
                     ELSE
                         MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     END-IF
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
             END-IF
           END-IF
      *
      *    Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¡ÊÊ¿À®£²£´Ç¯£³·î¤Þ¤Ç¤Î´ü´ÖÂÐ±þ¡Ë
           IF   (LNK-PTNYUINRRK-TENNYUYMD     >=  "20100401")   AND
                (LNK-PTNYUINRRK-TENNYUYMD     <=  "20120331")
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190813410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU03      <=  60
                     ADD    1           TO  TN-IDX
                     MOVE   "190813410" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¤Î£¶£°Æü°ÊÆâ¤Î½ªÆü¤ò
      *              ºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU03
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190813510"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
             END-IF
           END-IF
      *
      *    Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¡ÊÊ¿À®£²£´Ç¯£´·î¤«¤é¤Î´ü´ÖÂÐ±þ¡Ë
           IF    LNK-PTNYUINRRK-TENNYUYMD     >=  "20120401"
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190151710"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU03      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190151710" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¤Î£³£°Æü°ÊÆâ¤Î½ªÆü
      *              ¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU03
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU03      <=  60
                     ADD    1           TO  TN-IDX
                     MOVE   "190813410" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¤Î£¶£°Æü°ÊÆâ¤Î³«»ÏÆü¥»¥Ã¥È
                     IF     TN-IDX        =      1
                         MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     END-IF
                     IF     TN-IDX       =      2
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                                TO  LNK-DAY6-KIJUN
                         MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                         MOVE     1             TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                        LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     END-IF
      *              Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¤Î£¶£°Æü°ÊÆâ¤Î½ªÆü
      *              ¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU03
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190813510"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     IF   TN-IDX      =     3
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                            TO  LNK-DAY6-KIJUN
                         MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                         MOVE     1         TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN
                                   TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                           (TN-IDX)
                     ELSE
                         MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     END-IF
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
             END-IF
           END-IF
      *
      *    ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡ÊÊ¿À®£²£´Ç¯£´·î¤«¤é¤Î´ü´ÖÂÐ±þ¡Ë
           IF    LNK-PTNYUINRRK-TENNYUYMD     >=  "20120401"
             IF  SYOKIKSN-IN-KHNSRYCD   =   "190028910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU03      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190028910" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ¤Î£³£°Æü°ÊÆâ¤Î½ªÆü
      *              ¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU03
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU03      <=  60
                     ADD    1           TO  TN-IDX
                     MOVE   "190150510" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ¤Î£¶£°Æü°ÊÆâ¤Î³«»ÏÆü¥»¥Ã¥È
                     IF     TN-IDX        =      1
                         MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     END-IF
                     IF     TN-IDX       =      2
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                                TO  LNK-DAY6-KIJUN
                         MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                         MOVE     1             TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                        LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     END-IF
      *              ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ¤Î£¶£°Æü°ÊÆâ¤Î½ªÆü
      *              ¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU03
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190150610"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     IF   TN-IDX      =     3
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                            TO  LNK-DAY6-KIJUN
                         MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                         MOVE     1         TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN
                                   TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                           (TN-IDX)
                     ELSE
                         MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     END-IF
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
             END-IF
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¤Ç£·Æü¤Þ¤Ç¡¢£±£´Æü¤Þ¤Ç¡¢¤½¤ì°Ê¹ß¤Ï°ìÈÌÆþ±¡ÎÁ¤ò»»Äê¤¹¤ë
      *    ¾ì¹ç¤Î¿ÇÎÅ¥³¡¼¥É¤ò¼èÆÀ
      *    ¢¨Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤ÎµßÌ¿µßµÞÆþ±¡ÎÁ¤Ç¤¢¤ì¤Ð¡¢¤³¤Î½èÍý¤Ï¥¹¥ë¡¼¤¹¤ë
           IF    (LNK-PTNYUINRRK-TENNYUYMD >=    "20080401")  AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190024510"  OR
                                                 "190110210"  OR
                                                 "190110310"  OR
                                                 "190024310"  OR
                                                 "190110410"  OR
                                                 "190110510")
               CONTINUE
           ELSE
             PERFORM    4801-TOKUTEI-NYUIN-7-14-SEC
      *
             MOVE      ZERO               TO  TN-IDX
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  7
                     ADD     1        TO     TN-IDX
                     COMPUTE  WK-TOKUTEI-DAY2 =  6   -  STUSAN-NISSU07
                     MOVE   WRK-TOKUTEI-SRYCD1  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD2  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1) NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD  TO
                                                    LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD  TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     MOVE    "99999999"        TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
             END-IF
           END-IF
      *
      *    Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤ÏµßÌ¿µßµÞÆþ±¡ÎÁ¤ò£±Æü¡Á£³Æü¡¢£´Æü¡Á£·Æü¡¢
      *    £¸Æü¡Á£±£´Æü¤Ç¤ÎÅÀ¿ô¹ï¤ß¤È¤¹¤ë
           IF   ((LNK-PTNYUINRRK-TENNYUYMD >=    "20080401")  AND
                 (LNK-PTNYUINRRK-TENNYUYMD <=    "20100331")) AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190024510"  OR
                                                 "190110210"  OR
                                                 "190110310"  OR
                                                 "190024310"  OR
                                                 "190110410"  OR
                                                 "190110510")
             PERFORM    4801-TOKUTEI-NYUIN-3-7-14-SEC
      *
             MOVE      ZERO               TO  TN-IDX
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          µßÌ¿µßµÞÆþ±¡ÎÁ¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  3
                     ADD     1        TO     TN-IDX
                     COMPUTE  WK-TOKUTEI-DAY2 =  2   -  STUSAN-NISSU07
                     MOVE   WRK-TOKUTEI-SRYCD1  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU07  <    7
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD2  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1) NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                                        LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     COMPUTE  WK-TOKUTEI-DAY2 =  6   -  STUSAN-NISSU07
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU07  <    14
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD3  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     COMPUTE   TN-IDX2   =   TN-IDX   -   1
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2) 
                                                         NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                                    LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     MOVE    "99999999"        TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
             END-IF
           END-IF
      *
      *    Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤Ï°Ê²¼ÆÃÄêÆþ±¡ÎÁ¤ò£±Æü¡Á£³Æü¡¢£´Æü¡Á£·Æü¡¢
      *    £¸Æü¡Á£±£´Æü¤Ç¤ÎÅÀ¿ô¹ï¤ß¤È¤¹¤ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
           IF    (LNK-PTNYUINRRK-TENNYUYMD >=    "20100401")  AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190024510"  OR
                                                 "190024310"  OR
                                                 "190138110"  OR
                                                 "190138710")
             PERFORM    4801-TOKUTEI-NYUIN2-3-7-14-SEC
      *
             MOVE      ZERO               TO  TN-IDX
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          µßÌ¿µßµÞÆþ±¡ÎÁ¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  3
                     ADD     1        TO     TN-IDX
                     COMPUTE  WK-TOKUTEI-DAY2 =  2   -  STUSAN-NISSU07
                     MOVE   WRK-TOKUTEI-SRYCD1  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU07  <    7
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD2  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1) NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                                     LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     COMPUTE  WK-TOKUTEI-DAY2 =  6   -  STUSAN-NISSU07
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU07  <    14
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD3  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     COMPUTE   TN-IDX2   =   TN-IDX   -   1
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2) 
                                                         NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD
                                                   TO  LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     MOVE    "99999999"        TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
             END-IF
           END-IF
      *
      *    Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤Ï°Ê²¼ÆÃÄêÆþ±¡ÎÁ¤ò£±Æü¡Á£³Æü¡¢£´Æü¡Á£·Æü¡¢
      *    £¸Æü¡Á£¶£°Æü¤Ç¤ÎÅÀ¿ô¹ï¤ß¤È¤¹¤ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
           IF    (LNK-PTNYUINRRK-TENNYUYMD >=    "20100401")  AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190138410")
             PERFORM    4801-TOKUTEI-NYUIN3-3-7-60-SEC
      *
             MOVE      ZERO               TO  TN-IDX
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          µßÌ¿µßµÞÆþ±¡ÎÁ¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  3
                     ADD     1        TO     TN-IDX
                     COMPUTE  WK-TOKUTEI-DAY2 =  2   -  STUSAN-NISSU07
                     MOVE   WRK-TOKUTEI-SRYCD1  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU07  <    7
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD2  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1) NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                                     LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     COMPUTE  WK-TOKUTEI-DAY2 =  6   -  STUSAN-NISSU07
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU07  <    60
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD3  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     COMPUTE   TN-IDX2   =   TN-IDX   -   1
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2) 
                                                         NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                                     LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     MOVE    "99999999"        TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
             END-IF
           END-IF
      *
      *    Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤Ï°Ê²¼ÆÃÄêÆþ±¡ÎÁ¤ò£±Æü¡Á£³Æü¡¢£´Æü¡Á£·Æü¡¢
      *    £¸Æü¡Á£±£´Æü¡¢£±£µÆü¡Á£¶£°Æü¤Ç¤ÎÅÀ¿ô¹ï¤ß¤È¤¹¤ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
           IF    (LNK-PTNYUINRRK-TENNYUYMD >=    "20100401")  AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190139010")
             PERFORM    4801-TOKUTEI-NYUIN4-3-7-14-60-SEC
      *
             MOVE      ZERO               TO  TN-IDX
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          µßÌ¿µßµÞÆþ±¡ÎÁ¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  3
                     ADD     1        TO     TN-IDX
                     COMPUTE  WK-TOKUTEI-DAY2 =  2   -  STUSAN-NISSU07
                     MOVE   WRK-TOKUTEI-SRYCD1  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU07  <    7
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD2  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1) NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                                        LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     COMPUTE  WK-TOKUTEI-DAY2 =  6   -  STUSAN-NISSU07
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU07  <    14
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD3  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     COMPUTE   TN-IDX2   =   TN-IDX   -   1
                     IF   (LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2)
                                                           NOT = SPACE)
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                                    LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     COMPUTE  WK-TOKUTEI-DAY2 =  13   -  STUSAN-NISSU07
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU07  <    60
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD4  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     COMPUTE   TN-IDX2   =   TN-IDX   -   1
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2) 
                                                         NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                                    LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     MOVE    "99999999"        TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
             END-IF
           END-IF
      *
      *    Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤Ï°Ê²¼ÆÃÄêÆþ±¡ÎÁ¤ò£±Æü¡Á£·Æü¡¢£¸Æü¡Á£¶£°Æü
      *    ¤Ç¤ÎÅÀ¿ô¹ï¤ß¤È¤¹¤ë
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
           IF    (LNK-PTNYUINRRK-TENNYUYMD >=    "20100401")  AND
                 (SYOKIKSN-IN-KHNSRYCD     =     "190140010")
             PERFORM    4801-TOKUTEI-NYUIN5-7-60-SEC
      *
             MOVE      ZERO               TO  TN-IDX
             IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  7
                     ADD     1        TO     TN-IDX
                     COMPUTE  WK-TOKUTEI-DAY2 =  6   -  STUSAN-NISSU07
                     MOVE   WRK-TOKUTEI-SRYCD1  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     STUSAN-NISSU07  <    60
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD2  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     COMPUTE   TN-IDX2   =   TN-IDX   -   1
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2) 
                                                         NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX2)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                                   LNK-DAY6-KIJUN
                         MOVE  LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     MOVE    "99999999"        TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
             END-IF
           END-IF
      *
      *    ¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190024810"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          ¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  60
                     MOVE   "190024810"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ¡Ê£²£±Æü¤ò¸ÂÅÙ¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190117310"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  21  - STUSAN-NISSU06
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     MOVE   "190117310"  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD  TO
                                 LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                                 LNK-DAY6-KIJUN
                     MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                     MOVE     WK-TOKUTEI-DAY
                                           TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ¡Ê£¹£°Æü¤ò¸ÂÅÙ¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190117410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  90
                     MOVE   "190117410"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190129910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  60
                     MOVE   "190129910"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡Ê»ØÄêÃÏ°è¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190150310"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡Ê»ØÄêÃÏ°è¡Ë¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  60
                     MOVE   "190150310"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê»ØÄêÃÏ°è¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190150410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê»ØÄêÃÏ°è¡Ë¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  60
                     MOVE   "190150410"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190152610"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡Ë¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  60
                     MOVE   "190152610"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡¦¥ê¥Ï¥Ó¥ê»»Äê´µ¼Ô¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190152710"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡¦¥ê¥Ï¥Ó¥ê»»Äê´µ¼Ô¡Ë
      *          ¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  60
                     MOVE   "190152710"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          ÆÃÄêÆþ±¡´ü´Ö¡Ê£¶£°Æü·Ð²á¡Ë¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡ÎÁºîÀ®¤Î
      *          ²ÄÈÝ¤òÈ½Äê
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          ·îËöÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ¡Ê£±£´Æü¤ò¸ÂÅÙ¡Ë
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190120810"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ¤Î»»Äê²ÄÇ½Æü¿ô¤ò·×»»
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU07
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     MOVE   "190120810"  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD  TO
                                 LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                                 LNK-DAY6-KIJUN
                     MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                     MOVE     WK-TOKUTEI-DAY
                                           TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          °ìÈÌÆþ±¡ÎÁ¤Î»»Äê³«»ÏÆü¤ò»»Äê
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    Àº¿À²ÊµßµÞÆþ±¡ÎÁ
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121010"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190121010" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Àº¿À²ÊµßµÞÆþ±¡ÎÁ¤Î£³£°Æü°ÊÆâ¤Î½ªÆü¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU01
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190111410"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190130010"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190130010" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²¤Î£³£°Æü°ÊÆâ¤Î½ªÆü¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU01
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190130110"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190130210"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190130210" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Àº¿À²ÊµßµÞÆþ±¡ÎÁ¤Î£³£°Æü°ÊÆâ¤Î½ªÆü¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU01
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190130310"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU07      <=  29
                     ADD    1           TO  TN-IDX
                     MOVE   "190121110" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¤Î£³£°Æü°ÊÆâ¤Î½ªÆü¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  29  - STUSAN-NISSU07
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190066910"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121210"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU07      <=  29
                     ADD    1           TO  TN-IDX
                     MOVE   "190121210" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¤Î£³£°Æü°ÊÆâ¤Î½ªÆü¤òºÆ·×»»
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  29  - STUSAN-NISSU07
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190067010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *-------ÆÃÄêÆþ±¡ÂÐ±þ¤³¤³¤Þ¤Ç-----------------------------------
      *
      *    Æþ±¡ÎÁ²Ã»»¿ÇÎÅ¥³¡¼¥É¼è¤ê¹þ¤ß
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    ÆÃÄêÆþ±¡ÎÁ¶èÊ¬¤Î¥»¥Ã¥È
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        Ï·¿ÍÆÃÄêÆþ±¡´ðËÜÎÁµÚ¤ÓÊ¿À®£±£¸Ç¯£··î°Ê¹ß¤ÎÎÅÍÜÉÂÅï¡¢
      *        ÎÅÍÜÉÂ¾²¤Î¾ì¹ç¤ÏÆÃÄêÆþ±¡ÎÁ°·¤¤¤«¤é³°¤¹
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410" OR
                                                       "190799510" OR
                                                       "190121310" OR
                                                       "190121410" OR
                                                       "190121510" OR
                                                       "190121610" OR
                                                       "190121710" OR
                                                       "190121810" OR
                                                       "190122010" OR
                                                       "190122110" OR
                                                       "190122210" OR
                                                       "190122310" OR
                                                       "190122410" OR
                                                       "190121510" OR
                                                       "190123710" OR
                                                       "190123810" OR
                                                       "190123910" OR
                                                       "190124010" OR
                                                       "190124110" OR
                                                       "190124210" OR
                                                       "190124310" OR
                                                       "190124410" OR
                                                       "190124510" OR
                                                       "190124610" OR
                                                       "190124710" OR
                                                       "190124810" OR
                                                       "190131610" OR
                                                       "190131710" OR
                                                       "190131810" OR
                                                       "190131910" OR
                                                       "190132010" OR
                                                       "190132110" OR
                                                       "190132210" OR
                                                       "190132310" OR
                                                       "190132410" OR
                                                       "190132510" OR
                                                       "190132610" OR
                                                       "190132710" OR
                                                       "190132810" OR
                                                       "190132910" OR
                                                       "190133010" OR
                                                       "190133110" OR
                                                       "190133210" OR
                                                       "190133310" OR
                                                       "190133410" OR
                                                       "190133510" OR
                                                       "190133610" OR
                                                       "190133710" OR
                                                       "190133810" OR
                                                       "190133910" OR
                                                       "190134010" OR
                                                       "190134110"
                   MOVE  "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
               END-IF
           END-IF
      *    ³°Çñ
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    ¼¼ÎÁº¹³Û
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     LNK-PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   LNK-PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    ¿©»ö¶èÊ¬
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           IF     WRK-TAIHI-SHOKUJI    NOT =   ZERO
                  MOVE   WRK-TAIHI-SHOKUJI
                                TO  LNK-SCNYUACCT-SHOKUJIKBN
           ELSE
                  MOVE   0      TO  LNK-SCNYUACCT-SHOKUJIKBN
           END-IF
      *    ¿©Æ²²Ã»»
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    Å¾²Ê¸µÆþ±¡²Ê
           MOVE   "00"          TO  LNK-SCNYUACCT-MOTO-NYUINKA
      **     MOVE   SPA-GMN-I01-NYUINKA-MOT (1 : 2)
      **                          TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           PERFORM 4801-SENTEI-SET-SEC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  SPA-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  SPA-PTID
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" )
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   LNK-PTNYUINRRK-TENNYUYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¸å¤ËÂàÈò¥Ç¡¼¥¿¤Î¿©»ö¶èÊ¬¤òÉü¸µ
           IF  LNK-PTNYUINRRK-TENNYUYMD(1:6)  >= "200604"
      *       ¡Ê°ÛÆ°·îÊ¬¡Ë
               IF   WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE
                PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                 INITIALIZE                      NYUINACCT-REC
                 MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                 MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                 MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                             TO  NACCT-SRYYM
      *
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-SELECT-SEC
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-READ-SEC
                 IF      FLG-NYUINACCT        =   ZERO
                     MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *              ¿©»ö¤òÉü¸µ
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                     EVALUATE  IDX-SKJ-TAIHI
                         WHEN  1
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ18(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  2
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ19(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  3
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ20(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                     END-EVALUATE
      *              ºÞ²ó¿ôºÆ¥»¥Ã¥È
                     MOVE  ZERO              TO  NACCT-ZAIKAISU
                     PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                              UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                     END-PERFORM   
                 END-IF
                 MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 MOVE     "key"              TO  MCP-PATHNAME
                 MOVE     "DBUPDATE"         TO  MCP-FUNC
                 CALL    "ORCDBMAIN"         USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *          Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM 910-DBCLOSECURSOR-SEC
                END-PERFORM
               END-IF
      *
      *       ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
               IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
      *
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *                ¿©»ö¤òÉü¸µ
                       EVALUATE  IDX-SKJ-TAIHI
                           WHEN  1
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ18(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  2
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ19(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  3
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ20(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                       END-EVALUATE
      *                ºÞ²ó¿ôºÆ¥»¥Ã¥È
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                           IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                           END-IF
                       END-PERFORM   
                   END-IF
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                   MOVE     "key"              TO  MCP-PATHNAME
                   MOVE     "DBUPDATE"         TO  MCP-FUNC
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
                   MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-PERFORM
               END-IF
           ELSE
      *       ¡Ê°ÛÆ°·îÊ¬¡Ë
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            ¿©»ö¤òÉü¸µ
                   MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                       TO  IDX-SHOKUJIX
                   PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                             UNTIL     IDX-SKJ   >   31
                         MOVE   WRK-TENROOM-SKJ(1 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO              TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key"              TO  MCP-PATHNAME
               MOVE     "DBUPDATE"         TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *        Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
      *       ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
               IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key28"             TO  MCP-PATHNAME
                   PERFORM   910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *                ¿©»ö¤òÉü¸µ
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   WRK-TENROOM-SKJ(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                       END-PERFORM
      *                ºÞ²ó¿ôºÆ¥»¥Ã¥È
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                           IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                           END-IF
                       END-PERFORM   
                   END-IF
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   MOVE     "DBUPDATE"         TO  MCP-FUNC
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key28"             TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *
           .
       4801-TENROOM-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¼è¾Ã¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-NYUKAIKEI-ERASE-SEC         SECTION.
      *
           DISPLAY "4801-NYUKAIKEI-ERASE-SEC"
           DISPLAY "PTNYUINRRK-PTID      = " LNK-PTNYUINRRK-PTID
           DISPLAY "PTNYUINRRK-NYUINYMD  = " LNK-PTNYUINRRK-NYUINYMD
           DISPLAY "PTNYUINRRK-TAIINYMD  = " LNK-PTNYUINRRK-TAIINYMD
           DISPLAY "PTNYUINRRK-HKNCOMBINUM  = "
                                         LNK-PTNYUINRRK-HKNCOMBINUM
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE            ORCSNYUACCT-AREA
           EVALUATE   SODRNACCT-ORDERID
               WHEN   "0107"
                      MOVE   "5"       TO   LNK-SCNYUACCT-SYORI-KBN
               WHEN   "0108"
                      MOVE   "6"       TO   LNK-SCNYUACCT-SYORI-KBN
           END-EVALUATE
           DISPLAY  "LNK-SCNYUACCT-SYORI-KBN = " 
                     LNK-SCNYUACCT-SYORI-KBN
           MOVE     LNK-PTNYUINRRK-HOSPNUM  TO  SPA-HOSPNUM
           MOVE     "1"                    TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                        TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9      TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
      *
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           .
       4801-NYUKAIKEI-ERASE-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âà±¡¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-TAIINKAIKEI-EDIT-SEC         SECTION.
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "2"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-HOSPNUM  TO  SPA-HOSPNUM
           MOVE     LNK-PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE     LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-SCNYUACCT-EDSRYYMD
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9     TO  LNK-SCNYUACCT-HKNCOMBINUM
      *     MOVE   SPA-SRYKA           TO  WRK-TAIHI-SRYKA
           MOVE   "00"                 TO  SPA-SRYKA
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
      *     MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           .
       4801-TAIINKAIKEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ³°Çñ¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-GAIHAKU-SEC         SECTION.
      *
      *    ³°Çñ¤Î¥»¥Ã¥È
           IF      LNK-PTNYUINRRK-TAIINYMD =   "99999999"
               PERFORM    4801-GAIHAKU-1-SEC
           ELSE
               PERFORM    4801-GAIHAKU-2-SEC
           END-IF
      *
           .
       4801-GAIHAKU-EXT.
           EXIT.
      *****************************************************************
      *    ³°Çñ¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÆþ±¡Ãæ¡Ë
      *****************************************************************
       4801-GAIHAKU-1-SEC         SECTION.
      *
      *    Æþ±¡Ãæ´µ¼Ô¤Î³°Çñ¥Ç¡¼¥¿¹¹¿·¤Ê¤Î¤Ç¡¢Æþ±¡²ñ·×¤¬Â¸ºß¤¹¤ë²ñ·×¥Ç¡¼¥¿¤Ë
      *    ¤Ä¤¤¤ÆÁ´¤Æ¹¹¿·¤ò¹Ô¤¦
      *
      *    ³°Çñ¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ³«»ÏÆü¤Î¥»¥Ã¥È
                   IF      NACCT-SRYYM    =   SODRNACCT-STARTYMD(1:6)
                       MOVE   WRK-STARTYMD-D   TO   START-IDX
                   ELSE
                       MOVE   1                TO   START-IDX
                   END-IF
      *            ·îËöÆü¤ò»»Äê
                   INITIALIZE                STS-AREA-DAY
                   INITIALIZE                LNK-DAY7-AREA
                   MOVE    "71"              TO  LNK-DAY7-IRAI
                   MOVE   NACCT-SRYYM        TO  LNK-DAY7-YM
                   CALL  "ORCSDAY"           USING STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE   LNK-DAY7-KOYOMI    TO    WRK-GETSUMATSU-YMD
      *            ³°Çñ¶èÊ¬¤Î¥»¥Ã¥È
                   PERFORM  VARYING IDX  FROM  START-IDX   BY 1
                                UNTIL   IDX  >    WRK-GETSUMATSU-D
                       EVALUATE   SODRNACCT-ORDERID(3:2)
                           WHEN   "01"
                             MOVE   1    TO   NACCT-DAY(IDX)
                           WHEN   "02"
                             MOVE   2    TO   NACCT-DAY(IDX)
                           WHEN   "03"
                             MOVE   3    TO   NACCT-DAY(IDX)
                           WHEN   "04"
                             MOVE   4    TO   NACCT-DAY(IDX)
                           WHEN   "05"
                             MOVE   0    TO   NACCT-DAY(IDX)
                       END-EVALUATE
                   END-PERFORM
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key17"             TO  MCP-PATHNAME
                   PERFORM   910-DBCLOSECURSOR-SEC
      *            ¼¡·î¤Î²ñ·×ÆÉ¤ß¹þ¤ß
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   IF      WRK-STARTYMD-M  =   12
                      ADD     1      TO    WRK-STARTYMD-Y
                      MOVE    1      TO    WRK-STARTYMD-M
                   ELSE
                      ADD     1      TO    WRK-STARTYMD-M
                   END-IF
                   MOVE    "2"                     TO  NACCT-ZAISKBKBN
                   MOVE    WRK-STARTYMD-YM         TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT    =   ZERO
                       MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                       MOVE    "key17"                 TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                   END-IF
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-GAIHAKU-1-EXT.
           EXIT.
      *****************************************************************
      *    ³°Çñ¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÂà±¡ºÑ¡Ë
      *****************************************************************
       4801-GAIHAKU-2-SEC         SECTION.
      *
      *    Âà±¡ºÑ´µ¼Ô¤Î³°Çñ¥Ç¡¼¥¿¹¹¿·¤Ê¤Î¤Ç¡¢Âà±¡Æü¤Þ¤Ç¤Î²ñ·×¥Ç¡¼¥¿¤Ë¤Ä¤¤¤Æ
      *    ¹¹¿·¤ò¹Ô¤¦
      *
      *    ³°Çñ¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)  OR
                      (NACCT-SRYYM    > LNK-PTNYUINRRK-TAIINYMD(1:6))
      *            ³«»ÏÆü¤Î¥»¥Ã¥È
                   IF      NACCT-SRYYM      =  SODRNACCT-STARTYMD(1:6)
                       MOVE   WRK-STARTYMD-D   TO   START-IDX
                   ELSE
                       MOVE   1                TO   START-IDX
                   END-IF
      *            ·îËöÆü¤ò»»Äê
                   INITIALIZE                STS-AREA-DAY
                   INITIALIZE                LNK-DAY7-AREA
                   MOVE    "71"              TO  LNK-DAY7-IRAI
                   MOVE   NACCT-SRYYM        TO  LNK-DAY7-YM
                   CALL  "ORCSDAY"           USING STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE   LNK-DAY7-KOYOMI    TO    WRK-GETSUMATSU-YMD
      *            Âà±¡·î¤Ë¤Ä¤¤¤Æ¤ÏÂà±¡Æü¤ò¥»¥Ã¥È
                   IF   NACCT-SRYYM    =  LNK-PTNYUINRRK-TAIINYMD(1:6)
                       MOVE    LNK-PTNYUINRRK-TAIINYMD
                                             TO    WRK-GETSUMATSU-YMD
                   END-IF
      *            ³°Çñ¶èÊ¬¤Î¥»¥Ã¥È
                   PERFORM  VARYING IDX  FROM  START-IDX   BY 1
                                UNTIL   IDX  >    WRK-GETSUMATSU-D
                       EVALUATE   SODRNACCT-ORDERID(3:2)
                           WHEN   "01"
                             MOVE   1    TO   NACCT-DAY(IDX)
                           WHEN   "02"
                             MOVE   2    TO   NACCT-DAY(IDX)
                           WHEN   "03"
                             MOVE   3    TO   NACCT-DAY(IDX)
                           WHEN   "04"
                             MOVE   4    TO   NACCT-DAY(IDX)
                           WHEN   "05"
                             MOVE   0    TO   NACCT-DAY(IDX)
                       END-EVALUATE
                   END-PERFORM
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE     "key17"                 TO  MCP-PATHNAME
                   PERFORM   910-DBCLOSECURSOR-SEC
      *            ¼¡·î¤Î²ñ·×ÆÉ¤ß¹þ¤ß
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   IF      WRK-STARTYMD-M  =   12
                      ADD     1      TO    WRK-STARTYMD-Y
                      MOVE    1      TO    WRK-STARTYMD-M
                   ELSE
                      ADD     1      TO    WRK-STARTYMD-M
                   END-IF
                   MOVE    "2"                     TO  NACCT-ZAISKBKBN
                   MOVE    WRK-STARTYMD-YM         TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT    =   ZERO
                       MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                       MOVE    "key17"                 TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                   END-IF
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-GAIHAKU-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÊ¿À®£±£¸Ç¯£³·î°ÊÁ°¡Ë
      *****************************************************************
       4801-SHOKUJI-SEC         SECTION.
      *
      *    ¿©»ö¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key28"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key28"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               IF        FLG-NYUINACCT     =   ZERO
                 IF   NACCT-SRYCDTOTAL  =  99999913
      *            ¿©»ö¶èÊ¬¤Î¥»¥Ã¥È
                   MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
                   IF   SODRNACCT-SKJTIME   =  "01"
                     EVALUATE   SODRNACCT-ORDERID(3:2)
                       WHEN   "01"
                           MOVE  1  TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "02"
                           MOVE  2  TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "03"
                           MOVE  3  TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "04"
                           MOVE   4     TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "05"
                           MOVE  0  TO   NACCT-DAY(WRK-STARTYMD-D)
                     END-EVALUATE
                   ELSE
                     EVALUATE   SODRNACCT-ORDERID(3:2)
                       WHEN   "01"
                           IF     NACCT-DAY(WRK-STARTYMD-D)  =  0
                               MOVE  1  TO   NACCT-DAY(WRK-STARTYMD-D)
                           END-IF
                       WHEN   "02"
                           IF     NACCT-DAY(WRK-STARTYMD-D) =  0    OR
                                                               1    OR
                                                               3
                               MOVE  2  TO   NACCT-DAY(WRK-STARTYMD-D)
                           END-IF
                       WHEN   "03"
                           IF     NACCT-DAY(WRK-STARTYMD-D) =  0  OR 1
                               MOVE  3  TO   NACCT-DAY(WRK-STARTYMD-D)
                           END-IF
                       WHEN   "04"
                           MOVE   4     TO   NACCT-DAY(WRK-STARTYMD-D)
                     END-EVALUATE
                   END-IF
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
      *            ¿©Æ²²Ã»»¤ÎÊÔ½¸ÍÑ¤ËÂàÈò
                   MOVE    NACCT-DAY(WRK-STARTYMD-D)  TO WRK-SHOKUDOU
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
                 END-IF
               END-IF
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key28"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
      *-------------------¿©Æ²²Ã»»¤³¤³¤«¤é----------------------------
      *    ¿©»ö¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                 IF   NACCT-SRYCDTOTAL  =  197000570
      *            ¿©Æ²²Ã»»¤Î¥»¥Ã¥È
                   MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
                   IF    WRK-SHOKUDOU    NOT =  ZERO
                         MOVE   1   TO   NACCT-DAY(WRK-STARTYMD-D)
                   ELSE
                         MOVE   0   TO   NACCT-DAY(WRK-STARTYMD-D)
                   END-IF
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
                 END-IF
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key17"             TO  MCP-PATHNAME
                 PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *-------------------¿©Æ²²Ã»»¤³¤³¤Þ¤Ç----------------------------
           .
       4801-SHOKUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
      *****************************************************************
       4801-SHOKUJI-200604-SEC         SECTION.
      *
      *    ¿©»ö¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           EVALUATE   SODRNACCT-SKJTIME
              WHEN    "01"
                  MOVE    "key30"          TO  MCP-PATHNAME
              WHEN    "02"
                  MOVE    "key31"          TO  MCP-PATHNAME
              WHEN    "03"
                  MOVE    "key32"          TO  MCP-PATHNAME
           END-EVALUATE
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               EVALUATE   SODRNACCT-SKJTIME
                   WHEN    "01"
                       MOVE   "key30"      TO  MCP-PATHNAME
                   WHEN    "02"
                       MOVE   "key31"      TO  MCP-PATHNAME
                   WHEN    "03"
                       MOVE   "key32"      TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM   910-NYUINACCT-READ-SEC
               IF        FLG-NYUINACCT     =   ZERO
      *            ¿©»ö¶èÊ¬¤Î¥»¥Ã¥È
      *             MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
      *             EVALUATE   SODRNACCT-ORDERID(3:2)
      *                 WHEN   "01"
      *                     MOVE   1     TO   NACCT-DAY(WRK-STARTYMD-D)
      *                 WHEN   "02"
      *                     MOVE   2     TO   NACCT-DAY(WRK-STARTYMD-D)
      *                 WHEN   "05"
      *                     MOVE   0     TO   NACCT-DAY(WRK-STARTYMD-D)
      *             END-EVALUATE
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   SODRNACCT-DAY(IDX-SKJ)   =   1
                           EVALUATE   SODRNACCT-ORDERID(3:2)
                               WHEN   "01"
                                   MOVE   1     TO   NACCT-DAY(IDX-SKJ)
                               WHEN   "02"
                                   MOVE   2     TO   NACCT-DAY(IDX-SKJ)
                               WHEN   "05"
                                   MOVE   0     TO   NACCT-DAY(IDX-SKJ)
                           END-EVALUATE
                       END-IF
                   END-PERFORM
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
      *            ¿©Æ²²Ã»»¤ÎÊÔ½¸ÍÑ¤ËÂàÈò
                   MOVE    NACCT-DAY(WRK-STARTYMD-D)  TO WRK-SHOKUDOU
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
               END-IF
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           EVALUATE   SODRNACCT-SKJTIME
              WHEN    "01"
                  MOVE    "key30"          TO  MCP-PATHNAME
              WHEN    "02"
                  MOVE    "key31"          TO  MCP-PATHNAME
              WHEN    "03"
                  MOVE    "key32"          TO  MCP-PATHNAME
           END-EVALUATE
           PERFORM   910-DBCLOSECURSOR-SEC
      *
      *-------------------¿©Æ²²Ã»»¤³¤³¤«¤é----------------------------
      *    ¿©»ö¤Î¥»¥Ã¥È
      *     INITIALIZE                          NYUINACCT-REC
      *     MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
      *     MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
      *     MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
      *     MOVE    "4"                     TO  NACCT-ZAISKBKBN
      *     MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
      *     MOVE    "key17"                 TO  MCP-PATHNAME
      *     PERFORM 910-NYUINACCT-SELECT-SEC
      *     IF      FLG-NYUINACCT           =   ZERO
      *         MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
      *         MOVE     "key17"            TO  MCP-PATHNAME
      *         PERFORM   910-NYUINACCT-READ-SEC
      *         PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *           IF   NACCT-SRYCDTOTAL  =  197000570
      *            ¿©Æ²²Ã»»¤Î¥»¥Ã¥È
      *             MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
      *             IF    WRK-SHOKUDOU    NOT =  ZERO
      *                   MOVE   1   TO   NACCT-DAY(WRK-STARTYMD-D)
      *             ELSE
      *                   MOVE   0   TO   NACCT-DAY(WRK-STARTYMD-D)
      *             END-IF
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
      *             MOVE  ZERO           TO  NACCT-ZAIKAISU
      *             PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
      *                      UNTIL     IDX-SKJ   >   31
      *                 IF   NACCT-DAY(IDX-SKJ)   NOT =  0
      *                     ADD   1        TO  NACCT-ZAIKAISU
      *                 END-IF
      *             END-PERFORM   
      **
      *             MOVE    NYUINACCT-REC   TO   MCPDATA-REC
      *             MOVE    "DBUPDATE"      TO   MCP-FUNC
      *             MOVE    "tbl_nyuinacct" TO   MCP-TABLE
      *             MOVE    "key"           TO   MCP-PATHNAME
      *             CALL    "ORCDBMAIN"         USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
      *                                         SPA-AREA
      *
      *           END-IF
      *           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
      *           MOVE    "key17"             TO  MCP-PATHNAME
      *           PERFORM 910-NYUINACCT-READ-SEC
      *         END-PERFORM
      *     END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
      *     MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
      *     MOVE     "key17"                 TO  MCP-PATHNAME
      *     PERFORM   910-DBCLOSECURSOR-SEC
      *-------------------¿©Æ²²Ã»»¤³¤³¤Þ¤Ç----------------------------
           .
       4801-SHOKUJI-200604-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¾È²ñ¥Æ¡¼¥Ö¥ëÊÔ½¸¥µ¥Ö¸Æ¤Ó½Ð¤·
      *****************************************************************
       4900-ORCSS010-SEC         SECTION.
      *
           MOVE    NRRK-TENNYUYMD  TO  WRK-STYMD
           MOVE    WRK-LASTSRYYM   TO  WRK-EDYMD
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           IF    ( WRK-EDYMD       =   SPACE )
            OR   ( NACCT-SRYYM >=  WRK-EDYMD (1:6) )
               MOVE    NACCT-SRYYM TO  WRK-EDYMD
           END-IF
      *
           IF    ( WRK-EDYMD       =   SPACE )
               DISPLAY "SELECT FAILED [NYUINACCT] PTID:"
                        LNK-PTNYUINRRK-PTID
               MOVE    1               TO  FLG-END
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "abort"         TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
           END-IF
      *
           PERFORM UNTIL ( WRK-STYMD (1:6) >   WRK-EDYMD (1:6))
                    OR   ( FLG-END     NOT =   ZERO )
      *
               INITIALIZE                      SS010-AREA
               MOVE    LNK-PTNYUINRRK-PTID TO  SS010-PTID
               MOVE    WRK-STYMD (1:6)     TO  SS010-SRYYM
               CALL    "ORCSS010"      USING   SS010-AREA
                                               SPA-AREA
               IF    ( SS010-RC        NOT =   ZERO )
                   MOVE    1           TO  FLG-END
               END-IF
      *
               MOVE    WRK-STYMD       TO  WRK-YMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE     1              TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-STYMD
      *
           END-PERFORM
      *
           .
      *
       4900-ORCSS010-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¾ðÊó¸¡º÷½èÍý(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC              SECTION.
      *
           PERFORM 910-DB-SELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DB-READ-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý
      *****************************************************************
       910-DB-SELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       910-DB-READ-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACCT-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       900-NYUINACCT-KEY19-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID TO  NACCT-PTID
           MOVE    "999999"            TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY19-SEL-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DBCLOSECURSOR-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
