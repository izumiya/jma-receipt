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
       PROGRAM-ID.             ORCSAGECHK.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç½èÍý
      *  ÊÖµÑ¥¨¥é¡¼¥³¡¼¥É  : 00:Àµ¾ï¡¡99:¥Ñ¥é¥á¡¼¥¿ÉÔÀµ¡¦ÂÐ¾Ý¥Ç¡¼¥¿¤Ê¤·
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/09/10    NACL- Â¿¡¹Ç¼  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 01.00.01     NACL-Â¿¡¹Ç¼  02/10/01  À¸³èÊÝ¸îÄÉ²Ã
      * 01.00.02     NACL-Â¿¡¹Ç¼  02/10/07  Á¥°÷ËÜ¿ÍÄÉ²Ã
      *                                     £·£µºÐ°Ê¾å¤ÎÏ·¿Í¸øÈñÍ­ÌµÄÉ²Ã
      * 01.00.03     NACL-Â¿¡¹Ç¼  02/11/21  ¶¦ºÑ²¼Á¥£³·îÄÉ²Ã
      * 01.00.04     NACL-Â¿¡¹Ç¼  03/02/05  ¸¶ÇúÃ±ÆÈÏ·¿ÍÈ½Äê¤ò¤·¤Ê¤¤
      * 01.00.05     NACL-Â¿¡¹Ç¼  03/03/06  £³ºÐÌ¤ËþÈ½Äê¤Ë¼«Èñ¡¦ÃÏ¸³¤ò¤Ï¤º¤¹
      * 01.00.06     NACL-Â¿¡¹Ç¼  03/04/14  Ï«ºÒ¤Î³ä¹çÊÑ¹¹ÄÉ²Ã
      * 01.00.07     NACL-Â¿¡¹Ç¼  03/05/12  ¼«ÇåÀÕ³ä¹çÊÑ¹¹ÄÉ²Ã
      * 01.00.08     NACL-Â¿¡¹Ç¼  03/09/03  ¸øÈñ´ü¸Â¥Á¥§¥Ã¥¯ÄÉ²Ã
      * 01.00.09     NACL-Â¿¡¹Ç¼  03/11/13  Ï·¿Í¶èÊ¬ÄÉ²Ã
      * 01.00.10     NACL-Â¿¡¹Ç¼  04/01/06  Êñ³ç¤Þ¤È¤áÆþÎÏÄÉ²Ã
      * 01.00.11     NACL-Â¿¡¹Ç¼  04/04/21  Ï·¿Í¶èÊ¬£²¡¢ÊÝ¸±¶èÊ¬ÄÉ²Ã
      *                                     ¼ÂÇ¯ÎðÄÉ²Ã
      * 01.00.12     NACL-Â¿¡¹Ç¼  05/09/15  ÃÏÊý¸øÈñÃ±ÆÈ¶èÊ¬ÄÉ²Ã
      * 01.00.13     NACL-Â¿¡¹Ç¼  05/09/22  Ä¹ºê¸©ÈïÇú¼Ô°åÎÅ¡Ê£±£¸£¶¡ËÄÉ²Ã
      * 01.00.14     NACL-Â¿¡¹Ç¼  05/10/04  ÆÃÊÌÎÅÍÜÈñ¡Ê£°£¶£¸¡ËÄÉ²Ã
      * 02.00.00     NACL-ÂÀÅÄ    05/10/25  MONFUNC ÂÐ±þ
      * 02.00.01     NACL-Â¿¡¹Ç¼  06/01/12  »ùÆ¸ÊÝ¸î¡Ê£°£µ£³¡ËÄÉ²Ã
      * 03.00.01     NACL-Â¿¡¹Ç¼  06/08/25  Ê¿À®£±£¸Ç¯£±£°·î²þÀµÂÐ±þ
      *                                     ¡ÊÏ·¿Í³ä¹çÊÑ¹¹¡Ë
      * 03.03.00     NACL-Â¿¡¹Ç¼  06/10/24  Àº¿ÀÆþ±¡Ã±ÆÈÄÉ²Ã
      * 03.04.00     NACL-Â¿¡¹Ç¼  06/12/01  ¼«ÇåÀÕ·òÊÝ½àµòÄÉ²Ã
      * 03.05.00     NACL-Â¿¡¹Ç¼  07/04/XX  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      * 04.01.00     NACL-Â¿¡¹Ç¼  07/10/19  ¸øÌ³ºÒ³²ÄÉ²Ã
      * 04.02.00     NACL-Â¿¡¹Ç¼  08/03/XX  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *                                     ¡Ê¸å´ü¹âÎð¼Ô¡¢½¢³ØÁ°È½Äê¡Ë
      *                                     ¡Ê»ÄÎ±Ë®¿Í¡Ë
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNNUM-OK       PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-HKN-ERR         PIC 9(01).
           03  FLG-KOHCHK          PIC 9(01).
           03  FLG-KOH186          PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-ERR2            PIC 9(01).
           03  FLG-KAKUNIN         PIC 9(01).
           03  FLG-CHK-OK          PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMMDD.
                   07  WRK-SMM             PIC 9(02).
                   07  WRK-SDD             PIC 9(02).
           03  WRK-BYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
           03  WRK-NENREI-YY           PIC 9(03).
           03  WRK-NENREI-MM           PIC 9(02).
           03  WRK-NENREI-DD           PIC 9(02).
      *    £·£°ºÐ³«»ÏÆü
           03  WRK-BYMD-70.
               05  WRK-B70-YY          PIC 9(04).
               05  WRK-B70-MM          PIC 9(02).
               05  WRK-B70-DD          PIC 9(02).
      *
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
      *
           03  WRK-ZZ9                 PIC ZZZ.
      *
           03  WRK-SEIDONAME           PIC X(20).
      *
           03  WRK-BIRTHDAY            PIC X(08).
           03  WRK-ROU-NEN             PIC 9(03).
      *
           03  WRK-N-STRYMD.
               05  WRK-N-SYY           PIC 9(04).
               05  WRK-N-SMD           PIC X(04).
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¶¦ÄÌÎÎ°è
           COPY    "MCPAREA".
      *
      *    ´ðËÜ¾ðÊó
           COPY    "CPSK1001.INC".
      *
      *    Ï«ºÒ°åÎÅµ¡´Ø¾ðÊó
           COPY    "CPSK4001.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡Ý
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *    ´µ¼ÔÏ«ºÒÊÝ¸±¾ðÊó
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSAGECHK.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSAGECHKAREA
           SPA-AREA.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  AGECHK-OUT-AREA
           INITIALIZE                  AGECHK-RC
      *
           IF     (AGECHK-HOSPNUM      =   ZERO    ) OR
                  (AGECHK-PTID         NOT NUMERIC )
               MOVE    99              TO  AGECHK-RC
           END-IF
      *    Ç¯Îð·×»»
           IF      AGECHK-RC           =   ZERO
               PERFORM 100-NENREI-SEC
           END-IF
      *
      *    ÊÝ¸±ÁÈ¹çÈÖ¹æ¤¬¤¢¤ë¤È¤­¡¢ÉéÃ´³ä¹ç¤ò»»Äê¤¹¤ë
           IF     (AGECHK-RC           =   ZERO  )
             AND  (AGECHK-HKNCOMBINUM  NOT =   SPACE)
               PERFORM 200-FTNRATE-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    Ç¯Îð·×»»½èÍý
      *****************************************************************
       100-NENREI-SEC                SECTION.
      *
      *    ´µ¼ÔÀ¸Ç¯·îÆü
           IF      AGECHK-BIRTHDAY     =   SPACE
      *        ´µ¼Ô¾ðÊó¸¡º÷
               INITIALIZE                      PTINF-REC
               MOVE    AGECHK-HOSPNUM      TO  PTINF-HOSPNUM
               MOVE    AGECHK-PTID         TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   1
                   MOVE    99              TO  AGECHK-RC
                   GO      TO      100-NENREI-EXT
               END-IF
               MOVE    PTINF-BIRTHDAY      TO  WRK-BYMD
               MOVE    PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
           ELSE
               MOVE    AGECHK-BIRTHDAY     TO  WRK-BYMD
               MOVE    AGECHK-BIRTHDAY     TO  WRK-BIRTHDAY
           END-IF
      *
      *    ÅöÆü¤ÎÇ¯Îð¤ò·×»»¤¹¤ë
           MOVE    AGECHK-SRYYMD       TO  WRK-SYMD
           COMPUTE WRK-NENREI-YY       =   WRK-SYY
                                       -   WRK-BYY
           IF      WRK-BYMD(5:4)       >   WRK-SYMD(5:4)
               COMPUTE WRK-NENREI-YY       =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM       =  (WRK-SMM   +  12 )
                                           -   WRK-BMM
           ELSE
               COMPUTE WRK-NENREI-MM       =   WRK-SMM
                                           -   WRK-BMM
           END-IF
           IF      WRK-BDD             >   WRK-SDD
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                       -   1
           END-IF
      *    Ç¯Îð´ü´Ö»»Äê
           IF      WRK-NENREI-MM       =   ZERO
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY5-AREA
               MOVE    "51"                TO  LNK-DAY5-IRAI
               MOVE    WRK-BIRTHDAY        TO  LNK-DAY5-START
               MOVE    AGECHK-SRYYMD       TO  LNK-DAY5-END
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY5-AREA
               IF      LNK-DAY5-NISSU2     <=  31
                   MOVE    LNK-DAY5-NISSU2     TO  WRK-NENREI-DD
                   MOVE    ZERO                TO  WRK-NENREI-MM
               END-IF
           END-IF
           MOVE    WRK-NENREI-YY       TO  AGECHK-NENREI2-YY
           MOVE    WRK-NENREI-MM       TO  AGECHK-NENREI2-MM
           MOVE    WRK-NENREI-DD       TO  AGECHK-NENREI2-DD
      *
      *ÊÝ¸±Ç¯Îð·×»»
      *    £±Æü°Ê³°¤ÎÃÂÀ¸Æü¤Î»þ¤Ï¡¢Íâ·î£±Æü¤ò´ð½à¤È¤¹¤ë
           IF      WRK-BDD             >   01
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF      WRK-BMM         >   12
                   COMPUTE WRK-BYY         =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01              TO  WRK-BDD
           END-IF
      *    £·£°ºÐÅþÃ£Æü¡ÊÁ°´ü¹âÎð¼Ô³«»ÏÆü¡Ë
           MOVE    WRK-BYMD            TO  WRK-BYMD-70
           COMPUTE WRK-B70-YY          =   WRK-B70-YY  +   70
      *
      *    ÊÝ¸±Ç¯Îð¤ò·×»»¤¹¤ë
           MOVE    AGECHK-SRYYMD       TO  WRK-SYMD
           COMPUTE WRK-NENREI-YY       =   WRK-SYY
                                       -   WRK-BYY
           IF      WRK-BYMD(5:4)       >   WRK-SYMD(5:4)
               COMPUTE WRK-NENREI-YY       =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM       =  (WRK-BMM   +  12 )
                                           -   WRK-SMM
           ELSE
               COMPUTE WRK-NENREI-MM       =   WRK-BMM
                                           -   WRK-SMM
           END-IF
           MOVE    WRK-NENREI-YY       TO  AGECHK-NENREI
      *
      *    £È£²£°¡¥£´²þÀµ
      *    ½¢³ØÁ°È½Äê
           IF     (AGECHK-SRYYMD       >=   "20080401" )  AND
                  (WRK-BIRTHDAY        >=   "20020402" )
               PERFORM 1001-SYUGAKU-NEN-SEC
           END-IF
      *
      *    Ç¯Îð¤Ç¤ÎÏ·¿ÍÈ½Äê
           IF     (AGECHK-SRYYMD       >=   "20080401" )
      *        £È£²£°¡¥£´¤«¤é
               PERFORM 1003-ROUJIN-CHK2008-SEC
           ELSE
      *        £È£²£°¡¥£´¤Þ¤Ç
               PERFORM 1002-ROUJIN-CHK2002-SEC
           END-IF
           .
       100-NENREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½¢³ØÁ°È½Äê½èÍý
      *****************************************************************
       1001-SYUGAKU-NEN-SEC                SECTION.
      *
      *    ¥·¥¹¥Æ¥àÆü¤Ç½¢³ØÁ°¤«¤ÎÈ½ÃÇ
           MOVE    AGECHK-SRYYMD       TO  WRK-SYMD
           IF      WRK-SMMDD           >=  "0401"
               COMPUTE WRK-N-SYY           =   WRK-SYY   -   6
           ELSE
               COMPUTE WRK-N-SYY           =   WRK-SYY   -   7
           END-IF
           MOVE    "0401"              TO  WRK-N-SMD
      *    ¥·¥¹¥Æ¥àÇ¯ÅÙ¡Ý£¶Ç¯¤Î£´¡¥£±°Ê¹ß¤ÎÃÂÀ¸Æü¤¬½¢³ØÁ°
           IF      WRK-BIRTHDAY        >   WRK-N-STRYMD
               MOVE    1               TO  AGECHK-SYUGAKUKBN
           END-IF
      *
           .
       1001-SYUGAKU-NEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¯Îð¤Ç¤ÎÏ·¿ÍÈ½Äê¡Ê£È£²£°¡¥£³¤Þ¤Ç¡Ë
      *****************************************************************
       1002-ROUJIN-CHK2002-SEC         SECTION.
      *
      *    £È£±£´¡¥£±£°°Ê¹ß£·£°ºÐ¤ò£·£µºÐ¤È¤¹¤ë
           IF     (AGECHK-SRYYMD       >=   "20021001" )  AND
                  (WRK-BIRTHDAY        >=   "19321001" )
               MOVE    75                   TO  WRK-ROU-NEN
           ELSE
      *    £È£±£´¡¥£±£°»þÅÀ¤Ç£·£°ºÐÌ¤Ëþ¤Î»þ
               MOVE    70                   TO  WRK-ROU-NEN
           END-IF
      *    Ï·¿ÍÇ¯Îð°Ê¾å¤ÏÏ·¿Í¤È¤¹¤ë¡Ê³ºÅö·î£±Æü¤Ë£·£°ºÐ¡¦£·£µºÐ¡Ë
      *    ÊÝ¸±Ç¯Îð·×»»È½Äê
           IF      AGECHK-NENREI       >=  WRK-ROU-NEN
               MOVE    1               TO  AGECHK-ROUJIN
           END-IF
      *    £¶£µºÐ°Ê¾å¤Î»þ¡¢Ï·¿Í¸øÈñ¤ò¤â¤Ã¤Æ¤¤¤ë¤«¤ÎÈ½Äê
           IF      AGECHK-NENREI       >=  65
               IF      AGECHK-ROUJIN       =   ZERO
                   IF      AGECHK-ROUJIN2      =   ZERO
                       PERFORM 20010-ROUJIN-CHK2-SEC
                   END-IF
               ELSE
                   MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
               END-IF
           END-IF
      *
           .
       1002-ROUJIN-CHK2002-EXT.
           EXIT.
      *****************************************************************
      *    Ç¯Îð¤Ç¤ÎÏ·¿ÍÈ½Äê¡Ê£È£²£°¡¥£´¤«¤é¡Ë
      *****************************************************************
       1003-ROUJIN-CHK2008-SEC         SECTION.
      *
      *    £·£µºÐ¤ò·Þ¤¨¤ëÆü
           MOVE    WRK-BIRTHDAY        TO  WRK-SYMD
           COMPUTE WRK-SYY             =   WRK-SYY     +   75
           IF      WRK-SYMD            <=  AGECHK-SRYYMD
               MOVE    1                   TO  AGECHK-ROUJIN
           END-IF
      *    £¶£µºÐ°Ê¾å¤Î»þ¡¢¸å´ü¹âÎð¼Ô¤ò¤â¤Ã¤Æ¤¤¤ë¤«¤ÎÈ½Äê
           IF      AGECHK-NENREI       >=  65
               IF      AGECHK-ROUJIN       =   ZERO
                   IF      AGECHK-ROUJIN2      =   ZERO
                       PERFORM 20011-ROUJIN-CHK3-SEC
                   END-IF
               ELSE
                   MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
               END-IF
           END-IF
           IF     (AGECHK-SRYYMD       >=   "20021001" )  AND
                  (WRK-BIRTHDAY        >=   "19321001" )
               MOVE    75                   TO  WRK-ROU-NEN
           END-IF
           .
       1003-ROUJIN-CHK2008-EXT.
           EXIT.
      *****************************************************************
      *    ÉéÃ´³ä¹ç¤ò»»Äê½èÍý
      *****************************************************************
       200-FTNRATE-SEC                SECTION.
      *
      *    Êñ³ç¤Þ¤È¤áÆþÎÏ
           IF      AGECHK-HKNCOMBINUM  =   "9999"
               PERFORM 2009-HOUKATUHKN-SEC
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK2
      *    ÊÝ¸±ÁÈ¹ç¥Þ¥¹¥¿¡¼ÊÔ½¸
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    AGECHK-HOSPNUM      TO  COMB-HOSPNUM
           MOVE    AGECHK-PTID         TO  COMB-PTID
           MOVE    AGECHK-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNCOMBI        =   1
               MOVE    99              TO  AGECHK-RC
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
           MOVE    ZERO               TO  AGECHK-HKNKBN
      *
      *    Ï«ºÒ¤Î»þ¡¢³ä¹ç¤ÎÊÑ¹¹
           IF      COMB-HKNNUM         =   "971"
               MOVE    1               TO  AGECHK-HKNKBN
      *        Ï«ºÒ¾ðÊó¸¡º÷
               PERFORM 2004-PTRSIINF-SELECT-SEC
               IF      PTRSI-HKNKBN        =   "5"
      *            ¸øÌ³ºÒ³²¤Î³ä¹çÊÑ¹¹
                   PERFORM 2003-RSIKOMU-FTNRATE-SEC
               ELSE
                   PERFORM 2001-RSI-FTNRATE-SEC
               END-IF
           END-IF
      *
      *    ¼«ÇåÀÕ¤Î»þ¡¢³ä¹ç¤ÎÊÑ¹¹
           IF      COMB-HKNNUM         =   "973"
               MOVE    2               TO  AGECHK-HKNKBN
               PERFORM 2002-JIBAI-FTNRATE-SEC
           END-IF
      *
      *    ¸ø³²
           IF      COMB-HKNNUM         =   "975"
               MOVE    6               TO  AGECHK-HKNKBN
           END-IF
      *
           IF      AGECHK-NYUGAIKBN    =   "1"
               MOVE    COMB-NYU-KFTNRATE   TO  AGECHK-FTNRATE
           ELSE
               MOVE    COMB-GAI-KFTNRATE   TO  AGECHK-FTNRATE
           END-IF
           IF      AGECHK-FTNRATE      =   ZERO
               MOVE    SPACE               TO  AGECHK-FTNRATEMEI
           ELSE
               MOVE    AGECHK-FTNRATE      TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  AGECHK-FTNRATEMEI(1:3)
               MOVE    "%"                 TO  AGECHK-FTNRATEMEI(4:)
           END-IF
      *    £È£±£´¡¥£±£°²þÀµ¸å
      *    £³ºÐÌ¤Ëþ¤Î»þ¡¢£²³ä¤ò¾å¸Â¤È¤¹¤ë
           IF    ((AGECHK-SRYYMD       >=  "20021001") AND
                  (AGECHK-NENREI       <   03        )) 
      *    £È£²£°¡¥£´ ²þÀµ
             OR  ((AGECHK-SRYYMD       >=  "20080401") AND
                  (AGECHK-SYUGAKUKBN   =   1         ))
      *        ¼çÊÝ¸±¤¬¼«Èñ°Ê³°¤Î»þ
               MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
               MOVE    ZERO                TO  WRK-PAYKBN
               PERFORM 3001-HKNNUM-CHK-SEC
      *        ¼«Èñ¡¦¼£¸³¡¦Ï«ºÒ¡¦¼«ÇåÀÕ
               IF     (HKN-HKNKOHSBTKBN    =   "8"  OR  "9" )  OR
                      (HKN-HKNKOHSBTKBN    =   "4"          )  OR
                      (COMB-HKNNUM         =   "971"  OR  "973")
      *                ÆÃÊÌÎÅÍÜÈñ
                   OR (COMB-HKNNUM         =   "068"  OR  "069")
                   CONTINUE
      *            ¼«Èñ
                   IF      HKN-HKNKOHSBTKBN    =   "8"
                       MOVE    3               TO  AGECHK-HKNKBN
                   END-IF
               ELSE
                   IF      AGECHK-FTNRATE      >   20
                       MOVE    20                  TO  AGECHK-FTNRATE
                       MOVE    " 20%"              TO  AGECHK-FTNRATEMEI
                   END-IF
               END-IF
           END-IF
      *
      *    Ï·¿ÍÈ½Äê
           IF      AGECHK-SRYYMD       <   "20080401"
               PERFORM 2009-ROUJIN-HANTEI-SEC
           ELSE
      *        £È£²£°¡¥£´¤«¤é¸å´ü¹âÎð¼Ô
               PERFORM 20010-ROUJIN-HANTEI2-SEC
           END-IF
      *
      *    ÃÏÊý¸øÈñÃ±ÆÈÈ½Äê
           PERFORM 2010-KOHONLY-CHK-SEC
      *
      *    £È£±£´¡¥£±£°²þÀµ¸å°Ê²¼¤Î¥Á¥§¥Ã¥¯¤ò¹Ô¤¦
           IF      AGECHK-SRYYMD       >=  "20021001"
               CONTINUE
           ELSE
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    £È£±£µ¡¥£´²þÀµ¸å°Ê²¼¤Î¥Á¥§¥Ã¥¯¤ò¹Ô¤¦
           IF      AGECHK-SRYYMD       >=  "20030401"
               PERFORM 400-PTHKNCHK-SEC
      *        ¸øÈñ¤Î´ü¸ÂÀÚ¤ì¥Á¥§¥Ã¥¯
               IF      AGECHK-HKNCHK       =   ZERO
                   PERFORM 410-PTKOHCHK-SEC
               END-IF
           END-IF
           IF      AGECHK-HKNCHK   NOT =   ZERO
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    Ä¹ºê¸©¤ÎÈïÇú¼Ô°åÎÅ¡Ê£±£¸£¶¡ËÃ±ÆÈ¤Ï¥Á¥§¥¯¤Ê¤·
           IF      FLG-KOH186          =   1
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    £·£°ºÐ°Ê¾å¤Î»þ¡¢Ï·¿Í¤Ç¤¢¤ë¤³¤È
           IF      AGECHK-NENREI       >=  70
               PERFORM 2000-RJNCHK-SYORI-SEC
           END-IF
      *    £È£±£µ¡¥£´²þÀµ¸å
           IF     (AGECHK-SRYYMD       >=  "20061001")  AND
                  (AGECHK-NENREI       >=  65   AND
                                       <   70        )
      *        £¶£µºÐ°Ê¾åÏ·¿Í¸øÈñ¤Î³ä¹çÈ½Äê
               PERFORM 2001-RJNCHK65-SYORI-SEC
           END-IF
      *
           .
       200-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    £¶£µºÐ°Ê¾åÏ·¿Í¸øÈñ¤Î³ä¹çÈ½Äê½èÍý¡Ê£È£±£¸¡¥£±£°¤«¤é¡Ë
      *****************************************************************
       2001-RJNCHK65-SYORI-SEC         SECTION.
      *
      *    Ï·¿Í¸øÈñ¤Ç¡¢»ÙÊ§¶èÊ¬¤¬¤¢¤ë»þ
           PERFORM 294-KOH027-18-CHK-SEC
      *    Ï·¿Í£²³ä¥¨¥é¡¼
           IF     (FLG-ERR2            =   1   )
               MOVE    3               TO  AGECHK-RJNCHK
           END-IF
           .
       2001-RJNCHK65-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï·¿ÍÀßÄêÍ­ÌµÈ½Äê½èÍý¡Ê£·£°ºÐ°Ê¾å¡Ë
      *****************************************************************
       2000-RJNCHK-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK2
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-ERR2
           MOVE    ZERO                TO  FLG-KAKUNIN
      *    ¼çÊÝ¸±È½Äê
           IF      COMB-HKNNUM     NOT =   SPACE
               MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
               MOVE    ZERO                TO  WRK-PAYKBN
               PERFORM 3001-HKNNUM-CHK-SEC
               IF      FLG-HKNNUM          =   ZERO
      *            Ï«ºÒ¡¦¼«Èñ¡¦¼£¸³
                   IF      HKN-HKNKOHSBTKBN    =   "4"  OR  "8"  OR  "9"
                       MOVE    3                   TO  FLG-OK
                   ELSE
                       IF      AGECHK-SRYYMD       >=  "20061001"
      *                    £È£±£¸¡¥£±£°²þÀµ°Ê¹ß¤Î¥Á¥§¥Ã¥¯
                           PERFORM 291-HOJOKBN-CHK18-SEC
                       ELSE
      *                    £È£±£´¡¥£±£°²þÀµ¤Î¥Á¥§¥Ã¥¯
                           PERFORM 290-HOJOKBN-CHK14-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    À¸³èÊÝ¸î¤Î¤ß¤Î»þ
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "012" )  OR
                  (COMB-KOH2HKNNUM     =   "012" )  OR
                  (COMB-KOH3HKNNUM     =   "012" )  OR
                  (COMB-KOH4HKNNUM     =   "012" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    »ÄÎ±Ë®¿ÍÅù¤Î¤ß¤Î»þ
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "025" )  OR
                  (COMB-KOH2HKNNUM     =   "025" )  OR
                  (COMB-KOH3HKNNUM     =   "025" )  OR
                  (COMB-KOH4HKNNUM     =   "025" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *
      *    ¸¶Çú¤Î¤ß¤Î»þ
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "018" )  OR
                  (COMB-KOH2HKNNUM     =   "018" )  OR
                  (COMB-KOH3HKNNUM     =   "018" )  OR
                  (COMB-KOH4HKNNUM     =   "018" ))
               MOVE    1               TO  FLG-OK2
           END-IF
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "019" )  OR
                  (COMB-KOH2HKNNUM     =   "019" )  OR
                  (COMB-KOH3HKNNUM     =   "019" )  OR
                  (COMB-KOH4HKNNUM     =   "019" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    ·ë£³£µ¤Î¤ß¤Î»þ
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "011" )  OR
                  (COMB-KOH2HKNNUM     =   "011" )  OR
                  (COMB-KOH3HKNNUM     =   "011" )  OR
                  (COMB-KOH4HKNNUM     =   "011" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    ¹¹Àµ¤Î¤ß»þ
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "015" )  OR
                  (COMB-KOH2HKNNUM     =   "015" )  OR
                  (COMB-KOH3HKNNUM     =   "015" )  OR
                  (COMB-KOH4HKNNUM     =   "015" ))
               IF      COMB-TEKSTYMD        >=   "20060401"
                   MOVE    1               TO  FLG-OK2
               END-IF
           END-IF
      *    Àº¿ÀÆþ±¡¤Î¤ß»þ
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "020" )  OR
                  (COMB-KOH2HKNNUM     =   "020" )  OR
                  (COMB-KOH3HKNNUM     =   "020" )  OR
                  (COMB-KOH4HKNNUM     =   "020" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    Àº¿ÀÄÌ±¡¤Î¤ß»þ
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "021" )  OR
                  (COMB-KOH2HKNNUM     =   "021" )  OR
                  (COMB-KOH3HKNNUM     =   "021" )  OR
                  (COMB-KOH4HKNNUM     =   "021" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    »ùÆ¸ÊÝ¸îÁ¼ÃÖµÚ¤ÓÃÎÅª¾ã³²¼ÔÊ¡»ã¤Î¤ß»þ
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "053" )  OR
                  (COMB-KOH2HKNNUM     =   "053" )  OR
                  (COMB-KOH3HKNNUM     =   "053" )  OR
                  (COMB-KOH4HKNNUM     =   "053" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    Ï·¿Í¸øÈñ¤Ç¡¢»ÙÊ§¶èÊ¬¤¬¤¢¤ë»þ
           IF      AGECHK-SRYYMD       >=  "20061001"
      *        £È£±£¸¡¥£±£°²þÀµ°Ê¹ß¤Î¥Á¥§¥Ã¥¯
               PERFORM 294-KOH027-18-CHK-SEC
           ELSE
      *        £È£±£´¡¥£±£°²þÀµ°Ê¹ß¤Î¥Á¥§¥Ã¥¯
               PERFORM 293-KOH027-14-CHK-SEC
           END-IF
      *
      *    ³Æ¸øÈñ¤¬Ï·¿Í¸øÈñ¤«¤ÎÈ½Äê
           IF      FLG-OK2             =   ZERO
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
      *            ¸øÈñ
                   IF      COMB-KOHHKNNUM (IDX)    NOT =   SPACE
                       MOVE    COMB-KOHHKNNUM (IDX)    TO  WRK-HKNNUM
                       MOVE    COMB-KOHPAYKBN (IDX)    TO  WRK-PAYKBN
                       PERFORM 3001-HKNNUM-CHK-SEC
                       IF      FLG-HKNNUM          =   ZERO
                           IF      HKN-HKNKOHSBTKBN    =   "3" OR "6"
                               PERFORM 2001-KOH-CHK-SEC
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Ï·¿Í¶èÊ¬¤Ê¤·
           IF     (FLG-OK              =   ZERO )  AND
                  (FLG-OK2             =   ZERO )
               MOVE    1               TO  AGECHK-RJNCHK
           END-IF
      *
           IF     (AGECHK-RJNCHK       =   ZERO )  AND
                  (FLG-OK          NOT =   3    )
      *    £·£µºÐ°Ê¾å¤Î»þ¡¢Ï·¿Í¸øÈñ¤¬¤¢¤ë¤³¤È
               IF      AGECHK-SRYYMD       <   "20080401"
                   IF     (AGECHK-NENREI       >=  WRK-ROU-NEN )  AND
                          (FLG-OK2             =   ZERO)
                       MOVE    2               TO  AGECHK-RJNCHK
                   END-IF
               ELSE
      *            £È£²£°¡¥£´¤«¤é
      *            £·£µºÐ°Ê¾å¤Ç¸å´ü¹âÎð¼Ô¤¬¤Ê¤¤»þ
                   IF     (AGECHK-ROUJIN       =   1   )  AND
                          (FLG-OK          NOT =   4   )  AND
                          (FLG-OK2             =   ZERO)
                       MOVE    4               TO  AGECHK-RJNCHK
                   END-IF
               END-IF
           END-IF
      *
      *    £È£±£¸¡¥£±£°¤«¤é¡¢Ï·¿Í£²³ä¥¨¥é¡¼
           IF    ((FLG-ERR             =   1   )  AND
                  (FLG-OK2             =   ZERO) )  OR
                  (FLG-ERR2            =   1   )
               IF      (AGECHK-RJNCHK   NOT =   4   )
                   MOVE    3               TO  AGECHK-RJNCHK
               END-IF
           END-IF
      *
      *    ¹ñÊÝ¤ÎÁ°´ü¹âÎð¼Ô°Ê¹ß¤Î³ÎÇ§Í­Ìµ
           IF    (FLG-KAKUNIN          =   1 ) AND
                 (FLG-OK2              =   ZERO)
               MOVE    1               TO  AGECHK-KAKUNINCHK
           END-IF
      *
           .
       2000-RJNCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï·¿Í¸øÈñ»ÙÊ§¶èÊ¬È½Äê½èÍý¡Ê£È£±£´¡¥£±£°°Ê¹ß¡Ë
      *****************************************************************
       293-KOH027-14-CHK-SEC         SECTION.
      *
           IF    ((COMB-KOH1HKNNUM     =   "027" )  AND
                  (COMB-KOH1PAYKBN     =   "01" OR "02" ))  OR
                 ((COMB-KOH2HKNNUM     =   "027" )  AND
                  (COMB-KOH2PAYKBN     =   "01" OR "02"  ))  OR
                 ((COMB-KOH3HKNNUM     =   "027" )  AND
                  (COMB-KOH3PAYKBN     =   "01" OR "02" ))  OR
                 ((COMB-KOH4HKNNUM     =   "027" )  AND
                  (COMB-KOH4PAYKBN     =   "01" OR "02" ))
               MOVE    1               TO  FLG-OK2
           END-IF
           .
       293-KOH027-14-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ï·¿Í¸øÈñ»ÙÊ§¶èÊ¬È½Äê½èÍý¡Ê£È£±£¸¡¥£±£°°Ê¹ß¡Ë
      *****************************************************************
       294-KOH027-18-CHK-SEC         SECTION.
      *
           IF    ((COMB-KOH1HKNNUM     =   "027" )  AND
                  (COMB-KOH1PAYKBN     =   "01" OR "03" ))  OR
                 ((COMB-KOH2HKNNUM     =   "027" )  AND
                  (COMB-KOH2PAYKBN     =   "01" OR "03"  ))  OR
                 ((COMB-KOH3HKNNUM     =   "027" )  AND
                  (COMB-KOH3PAYKBN     =   "01" OR "03" ))  OR
                 ((COMB-KOH4HKNNUM     =   "027" )  AND
                  (COMB-KOH4PAYKBN     =   "01" OR "03" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    £²³ä¤ÎÈ½Äê
           IF    ((COMB-KOH1HKNNUM     =   "027" )  AND
                  (COMB-KOH1PAYKBN     =   "02" ))  OR
                 ((COMB-KOH2HKNNUM     =   "027" )  AND
                  (COMB-KOH2PAYKBN     =   "02"  ))  OR
                 ((COMB-KOH3HKNNUM     =   "027" )  AND
                  (COMB-KOH3PAYKBN     =   "02" ))  OR
                 ((COMB-KOH4HKNNUM     =   "027" )  AND
                  (COMB-KOH4PAYKBN     =   "02" ))
               MOVE    1               TO  FLG-ERR2
           END-IF
           .
       294-KOH027-18-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¾ðÊó½èÍý
      *****************************************************************
       2001-KOH-CHK-SEC         SECTION.
      *
           MOVE    1                   TO  FLG-OK2
           .
       2001-KOH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    £È£±£´¡¥£±£°²þÀµ¤Î¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       290-HOJOKBN-CHK14-SEC         SECTION.
      *
      *    ÊÝ¸±¼ÔÊÔ½¸
           INITIALIZE                  PTHKNINF-REC
           MOVE    AGECHK-HOSPNUM      TO  PTHKN-HOSPNUM
           MOVE    AGECHK-PTID         TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTHKNINF        =   ZERO
               IF     (PTHKN-HOJOKBN       =   "8"  OR  "9") OR
                      ((COMB-HKNNUM        =   "060"
                                           OR  "068"       )  AND
                       (PTHKN-HOJOKBN      =   "1"  OR  "2"
                                           OR  "4"  OR  "5"))
                   MOVE    1               TO  FLG-OK
               END-IF
      *        Á¥°÷¤ÇÊä½õ¶èÊ¬¤Î¤¢¤ë»þ£Ï£Ë¤È¤¹¤ë
               IF     (COMB-HKNNUM         =   "002" ) AND
                      (PTHKN-HOJOKBN   NOT =   SPACE )
                   IF      PTHKN-HOJOKBN       >=  "A" AND
                                               <=  "F"
                       MOVE    2               TO  FLG-OK
                   ELSE
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
      *        ¶¦ºÑ¤ÇÏ·¿Í¶èÊ¬¤Î¤¢¤ë»þ£Ï£Ë¤È¤¹¤ë
               IF     (COMB-HKNNUM         =   "031"  OR
                                               "032"  OR
                                               "033"  OR
                                               "034" ) AND
                      (PTHKN-HOJOKBN       =   "B"  OR  "E")
                   MOVE    2               TO  FLG-OK
               END-IF
      *        ¹ñÊÝ¤ÇÏ·¿Í¶èÊ¬¤Î¤¢¤ë»þ£Ï£Ë¤È¤¹¤ë
      *            Á°´ü¹âÎð¼Ô³«»ÏÆü°Ê¹ß¤Ë³ÎÄê¤¬¤Ê¤¤»þ¡¢·Ù¹ð
               IF     (COMB-HKNNUM         =   "060"
                                           OR  "068"       )
                   IF     (PTHKN-TEKSTYMD      <   WRK-BYMD-70 )
                      AND (PTHKN-TEKEDYMD      >=  WRK-BYMD-70 )
                      AND (PTHKN-KAKUNINYMD    <   WRK-BYMD-70 )
                       MOVE    1               TO  FLG-KAKUNIN
                   END-IF
               END-IF
           END-IF
           .
       290-HOJOKBN-CHK14-EXT.
           EXIT.
      *
      *****************************************************************
      *    £È£±£¸¡¥£±£°²þÀµ¤Î¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       291-HOJOKBN-CHK18-SEC         SECTION.
      *
      *    ÊÝ¸±¼ÔÊÔ½¸
           INITIALIZE                  PTHKNINF-REC
           MOVE    AGECHK-HOSPNUM      TO  PTHKN-HOSPNUM
           MOVE    AGECHK-PTID         TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           EVALUATE    COMB-HKNNUM
               WHEN    "060"
               WHEN    "068"
      *            ¹ñÊÝ
      *            ÁÈ¹ç¹ñÊÝ°Ê³°¤Ï¡¢£²³ä¤Ï·Ù¹ð
                   IF      PTHKN-HKNJANUM (3:1)    =   "3"
                       MOVE    1               TO  FLG-OK
                   ELSE
                       MOVE    1               TO  FLG-OK
                       IF      PTHKN-HOJOKBN       =   "2"
                           MOVE    1               TO  FLG-ERR
                       END-IF
                   END-IF
      *            Á°´ü¹âÎð¼Ô³«»ÏÆü°Ê¹ß¤Ë³ÎÄê¤¬¤Ê¤¤»þ¡¢·Ù¹ð
                   IF     (PTHKN-TEKSTYMD      <   WRK-BYMD-70 )
                      AND (PTHKN-TEKEDYMD      >=  WRK-BYMD-70 )
                      AND (PTHKN-KAKUNINYMD    <   WRK-BYMD-70 )
                       MOVE    1               TO  FLG-KAKUNIN
                   END-IF
               WHEN    "002"
      *        Á¥°÷
                   IF      PTHKN-HOJOKBN       =   "1"   OR  "2"
                                               OR  "3"   OR  SPACE
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-OK
      *                £²³ä¤Ç·ÑÂ³°Ê³°¤Ï¥¨¥é¡¼
                       IF      PTHKN-HOJOKBN       =   "8"
                                                   OR  "D"
                                                   OR  "E"
                                                   OR  "F"
                           IF      PTHKN-CONTKBN   =   SPACE
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       END-IF
                   END-IF
      *            £È£²£°¡¥£´¤«¤éËÜ¿Í¤Ç¤¢¤ì¤Ð¸å´ü¹âÎð¼Ô
                   IF     (AGECHK-SRYYMD       >=  "20080401" ) AND
                          (AGECHK-ROUJIN       =   1          ) AND
                          (PTHKN-HONKZKKBN     =   "1"        ) AND
                          (PTHKN-HOJOKBN       >=  "A"  AND
                                               <=  "I"        )
                       MOVE    4               TO  FLG-OK
                   END-IF
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
      *            ¶¦ºÑ¤ÇÏ·¿Í¶èÊ¬¤Î¤¢¤ë»þ£Ï£Ë¤È¤¹¤ë
                   IF     PTHKN-HOJOKBN        =   SPACE
                                               OR  "2"
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-OK
      *                £²³ä¤Ç·ÑÂ³°Ê³°¤Ï¥¨¥é¡¼
                       IF      PTHKN-HOJOKBN       =   "8"
                                                   OR  "E"
                           IF      PTHKN-CONTKBN   =   SPACE
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       ELSE
                           MOVE    2               TO  FLG-OK
                       END-IF
                   END-IF
      *        £È£²£°¡¥£´¤«¤é¡Ê¸å´ü¹âÎð¼Ô¡Ë
               WHEN    "039"
               WHEN    "040"
                   MOVE    4                   TO  FLG-OK
               WHEN    OTHER
      *            ¤½¤ÎÂ¾
                   IF     PTHKN-HOJOKBN        =   "7"
                                               OR  "8"
                                               OR  "9"
                       MOVE    1               TO  FLG-OK
      *                £²³ä¤Ç·ÑÂ³°Ê³°¤Ï¥¨¥é¡¼
                       IF      PTHKN-HOJOKBN       =   "8"
                           IF      PTHKN-CONTKBN   =   SPACE
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
           .
       291-HOJOKBN-CHK18-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï«ºÒ¤Î»þ¡¢³ä¹ç¤ÎÊÑ¹¹½èÍý
      *****************************************************************
       2001-RSI-FTNRATE-SEC         SECTION.
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷¡ÊÏ«ºÒ¾ðÊó¡Ë
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    AGECHK-SRYYMD       TO  SYS-4001-STYUKYMD
                                           SYS-4001-EDYUKYMD
           MOVE    AGECHK-HOSPNUM      TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-4001-REC
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
      *
               IF      SYS-4001-RSIIRYOKBN     =   "1"
                   MOVE    ZERO                TO  COMB-GAI-KFTNRATE
                   MOVE    ZERO                TO  COMB-NYU-KFTNRATE
               ELSE
                   MOVE    100                 TO  COMB-GAI-KFTNRATE
                   MOVE    100                 TO  COMB-NYU-KFTNRATE
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-RSI-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕ³ä¹ç¤ÎÊÑ¹¹½èÍý
      *****************************************************************
       2002-JIBAI-FTNRATE-SEC         SECTION.
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó¸¡º÷
           PERFORM 2004-PTRSIINF-SELECT-SEC
      *    ¼«ÇåÀÕ´µ¼ÔÀÁµá¶èÊ¬
           IF     (FLG-PTRSIINF        =   ZERO )  AND
                  (PTRSI-HKNKBN        =   "4"  )
               EVALUATE    PTRSI-JIBAISEIKBN
                   WHEN    "1"
                       MOVE    100             TO  COMB-GAI-KFTNRATE
                       MOVE    100             TO  COMB-NYU-KFTNRATE
                   WHEN    "2"
                       MOVE    ZERO            TO  COMB-GAI-KFTNRATE
                       MOVE    ZERO            TO  COMB-NYU-KFTNRATE
               END-EVALUATE
           END-IF
      *
      *    ¼«ÇåÀÕ¶èÊ¬¤ÎÈ½Äê
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷¡ÊÏ«ºÒ¾ðÊó¡Ë
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    AGECHK-SRYYMD       TO  SYS-4001-STYUKYMD
                                           SYS-4001-EDYUKYMD
           MOVE    AGECHK-HOSPNUM      TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-4001-REC
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
      *        ·òÊÝ½àµò
               IF      SYS-4001-JIBAIKBN       =   "2"
                   MOVE    4               TO  AGECHK-HKNKBN
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
      *
       2002-JIBAI-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÌ³ºÒ³²¤Î³ä¹çÊÑ¹¹½èÍý
      *****************************************************************
       2003-RSIKOMU-FTNRATE-SEC         SECTION.
      *
      *    ´µ¼ÔÀÁµá¶èÊ¬
           EVALUATE    PTRSI-JIBAISEIKBN
               WHEN    "1"
                   MOVE    100             TO  COMB-GAI-KFTNRATE
                   MOVE    100             TO  COMB-NYU-KFTNRATE
               WHEN    "2"
                   MOVE    ZERO            TO  COMB-GAI-KFTNRATE
                   MOVE    ZERO            TO  COMB-NYU-KFTNRATE
           END-EVALUATE
      *
      *    ¸øÌ³ºÒ³²¶èÊ¬¤ÎÈ½Äê
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷¡ÊÏ«ºÒ¾ðÊó¡Ë
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    AGECHK-SRYYMD       TO  SYS-4001-STYUKYMD
                                           SYS-4001-EDYUKYMD
           MOVE    AGECHK-HOSPNUM      TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-4001-REC
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
      *        ·òÊÝ½àµò
      *        ¸øÌ³ºÒ³²¶èÊ¬
               IF      SYS-4001-KOMUKBN        =   "2"
                   MOVE    5               TO  AGECHK-HKNKBN
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2003-RSIKOMU-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï«ºÒÊÝ¸±¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       2004-PTRSIINF-SELECT-SEC         SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    COMB-HOSPNUM        TO  PTRSI-HOSPNUM
           MOVE    COMB-PTID           TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "tbl_ptrsiinf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2004-PTRSIINF-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    Ï·¿Í¡¦°ìÈÌÄê½èÍý
      *****************************************************************
       2009-ROUJIN-HANTEI-SEC         SECTION.
      *
           MOVE    ZERO                TO  AGECHK-ROUJIN2
      *    À¸³èÊÝ¸î¤Ç¡¢£¶£µºÐ°Ê¾å¤ÇÏ·¿Í¸øÈñ¤ò»ý¤Ã¤Æ¤¤¤ë¾ì¹ç¡¢Ï·¿Í
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "012" )  OR
                  (COMB-KOH2HKNNUM     =   "012" )  OR
                  (COMB-KOH3HKNNUM     =   "012" )  OR
                  (COMB-KOH4HKNNUM     =   "012" ))
               IF     (AGECHK-NENREI       >=  65  )  AND
                      (AGECHK-ROUJIN       =   ZERO)
                   PERFORM 2009-ROUJIN-CHK-SEC
               END-IF
           ELSE
      *        Ï·¿Í¸øÈñ¤òÁªÂò¤·¤Æ¤¤¤ë¾ì¹ç¡¢Ï·¿Í
               IF     (COMB-KOH1HKNNUM     =   "027" )  OR
                      (COMB-KOH2HKNNUM     =   "027" )  OR
                      (COMB-KOH3HKNNUM     =   "027" )  OR
                      (COMB-KOH4HKNNUM     =   "027" )
                   MOVE    1               TO  AGECHK-ROUJIN
               END-IF
           END-IF
      *    Àº¿ÀÄÌ±¡¡¦¹¹Àµ¤Ç£¶£µºÐ°Ê¾å¤ÇÏ·¿Í¸øÈñ¤ò»ý¤Ã¤Æ¤¤¤ë¾ì¹ç¡¢Ï·¿Í
           IF     (COMB-HKNNUM         =   SPACE )  AND
                  (AGECHK-NENREI       >=  65    )  AND
                  (AGECHK-ROUJIN       =   ZERO  )  AND
                  (AGECHK-SRYYMD       >=  "20060401" )
               IF     ((COMB-KOH1HKNNUM     =   "015" )  OR
                       (COMB-KOH2HKNNUM     =   "015" )  OR
                       (COMB-KOH3HKNNUM     =   "015" )  OR
                       (COMB-KOH4HKNNUM     =   "015" ))  OR
                      ((COMB-KOH1HKNNUM     =   "021" )  OR
                       (COMB-KOH2HKNNUM     =   "021" )  OR
                       (COMB-KOH3HKNNUM     =   "021" )  OR
                       (COMB-KOH4HKNNUM     =   "021" ))
      *            Àº¿ÀÆþ±¡
                 OR  ((COMB-KOH1HKNNUM     =   "020" )  OR
                      (COMB-KOH2HKNNUM     =   "020" )  OR
                      (COMB-KOH3HKNNUM     =   "020" )  OR
                      (COMB-KOH4HKNNUM     =   "020" ))
                   PERFORM 2009-ROUJIN-CHK-SEC
               END-IF
           END-IF
      *    £¶£µºÐ°Ê¾å¤Î»þ¡¢Ï·¿Í¸øÈñ¤ò¤â¤Ã¤Æ¤¤¤ë¤«¤ÎÈ½Äê
           IF      AGECHK-NENREI       >=  65
               IF      AGECHK-ROUJIN       =   ZERO
                   IF      AGECHK-ROUJIN2      =   ZERO
                       PERFORM 20010-ROUJIN-CHK2-SEC
                   END-IF
               ELSE
                   MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
               END-IF
           END-IF
      *
      *    Ï«ºÒ¡¦¼«ÇåÀÕ¡¦¸ø³²¤Î»þ¡¢¤¹¤Ù¤Æ°ìÈÌ¤Ø
           IF      COMB-HKNNUM         =   "971"
                                       OR  "973"
                                       OR  "975"
               MOVE    ZERO            TO  AGECHK-ROUJIN
           END-IF
      *
           .
       2009-ROUJIN-HANTEI-EXT.
           EXIT.
      *****************************************************************
      *    Ï·¿Í¡¦°ìÈÌÄê¡Ê£È£²£°¡¥£´¤«¤é¡Ë½èÍý
      *****************************************************************
       20010-ROUJIN-HANTEI2-SEC         SECTION.
      *
           MOVE    ZERO                TO  AGECHK-ROUJIN2
      *    ¸å´ü¹âÎð¼Ô¤òÁªÂò¤·¤Æ¤¤¤ë¾ì¹ç¡¢Ï·¿Í
           IF      COMB-HKNNUM         =   "039"
                                       OR  "040"
               MOVE    1               TO  AGECHK-ROUJIN
           END-IF
      *    £¶£µºÐ°Ê¾å¤Î»þ¡¢¸å´ü¹âÎð¼Ô¤ò¤â¤Ã¤Æ¤¤¤ë¤«¤ÎÈ½Äê
           IF      AGECHK-NENREI       >=  65
               IF      AGECHK-ROUJIN       =   ZERO
                   IF      AGECHK-ROUJIN2      =   ZERO
                       PERFORM 20011-ROUJIN-CHK3-SEC
                   END-IF
               ELSE
                   MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
               END-IF
           END-IF
      *    À¸³èÊÝ¸î¡¢¸øÈñÃ±ÆÈ¤Ç¸å´ü¸øÈñ¤ò»ý¤Ã¤Æ¤¤¤ë¾ì¹ç¡¢Ï·¿Í
           IF     (COMB-HKNNUM         =   SPACE )  AND
                  (AGECHK-NENREI       >=  65    )  AND
                  (AGECHK-ROUJIN       =   ZERO  )
               PERFORM 20012-KOH-ROJIN-SEC
           END-IF
      *    Ï«ºÒ¡¦¼«ÇåÀÕ¡¦¸ø³²¤Î»þ¡¢¤¹¤Ù¤Æ°ìÈÌ¤Ø
           IF      COMB-HKNNUM         =   "971"
                                       OR  "973"
                                       OR  "975"
               MOVE    ZERO            TO  AGECHK-ROUJIN
           END-IF
      *
           .
       20010-ROUJIN-HANTEI2-EXT.
           EXIT.
      *****************************************************************
      *    Ï·¿Í¸øÈñÈ½Äê½èÍý
      *****************************************************************
       2009-ROUJIN-CHK-SEC         SECTION.
      *
      *    ¸øÈñÊÔ½¸
           INITIALIZE                  PTKOHINF-REC
           MOVE    AGECHK-HOSPNUM      TO  PTKOH-HOSPNUM
           MOVE    AGECHK-PTID         TO  PTKOH-PTID
           MOVE    "027"               TO  PTKOH-KOHNUM
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKEDYMD
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTKOHINF        =   ZERO
               MOVE    1                   TO  AGECHK-ROUJIN
               MOVE    1                   TO  AGECHK-ROUJIN2
           END-IF
      *
           .
      *
       2009-ROUJIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï·¿Í¸øÈñÈ½Äê½èÍý
      *****************************************************************
       20010-ROUJIN-CHK2-SEC         SECTION.
      *
      *    ¸øÈñÊÔ½¸
           INITIALIZE                  PTKOHINF-REC
           MOVE    AGECHK-HOSPNUM      TO  PTKOH-HOSPNUM
           MOVE    AGECHK-PTID         TO  PTKOH-PTID
           MOVE    "027"               TO  PTKOH-KOHNUM
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKEDYMD
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTKOHINF        =   ZERO
               MOVE    1                   TO  AGECHK-ROUJIN2
           END-IF
      *
           .
      *
       20010-ROUJIN-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸å´ü¹âÎð¼ÔÈ½Äê½èÍý
      *****************************************************************
       20011-ROUJIN-CHK3-SEC         SECTION.
      *
      *    ¸å´ü¹âÎð¼Ô³ºÅö
           INITIALIZE                  PTHKNINF-REC
           MOVE    AGECHK-HOSPNUM      TO  PTHKN-HOSPNUM
           MOVE    AGECHK-PTID         TO  PTHKN-PTID
           MOVE    AGECHK-SRYYMD       TO  PTHKN-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  PTHKN-TEKEDYMD
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           PERFORM UNTIL      (FLG-PTHKNINF    =   1   )
                         OR   (AGECHK-ROUJIN2  =   1   )
               IF      PTHKN-SAKJOKBN      =   "1"
                   CONTINUE
               ELSE
                   IF      PTHKN-HKNNUM        =   "039"
                                               OR  "040"
                       MOVE    1               TO  AGECHK-ROUJIN2
                   END-IF
               END-IF
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ¸å´ü¹âÎð¼Ô³ºÅö
           IF      AGECHK-ROUJIN2      =   ZERO
               INITIALIZE                  PTKOHINF-REC
               MOVE    AGECHK-HOSPNUM      TO  PTKOH-HOSPNUM
               MOVE    AGECHK-PTID         TO  PTKOH-PTID
               MOVE    "968"               TO  PTKOH-KOHNUM
               MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKSTYMD
               MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKEDYMD
      *
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key6"              TO  MCP-PATHNAME
                   PERFORM 920-PTKOHINF-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF      FLG-PTKOHINF        =   ZERO
                   MOVE    1                   TO  AGECHK-ROUJIN2
               END-IF
           END-IF
           .
       20011-ROUJIN-CHK3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñÃ±ÆÈÏ·¿ÍÈ½Äê½èÍý
      *****************************************************************
       20012-KOH-ROJIN-SEC         SECTION.
      *
           IF     (COMB-HKNNUM         =   SPACE )
      *        À¸³èÊÝ¸î¡¦Àº¿ÀÄÌ±¡¡¦¹¹Àµ
               IF    ((COMB-KOH1HKNNUM     =   "012" )  OR
                      (COMB-KOH2HKNNUM     =   "012" )  OR
                      (COMB-KOH3HKNNUM     =   "012" ))
                 OR  ((COMB-KOH1HKNNUM     =   "015" )  OR
                      (COMB-KOH2HKNNUM     =   "015" )  OR
                      (COMB-KOH3HKNNUM     =   "015" )  OR
                      (COMB-KOH4HKNNUM     =   "015" ))
                 OR  ((COMB-KOH1HKNNUM     =   "021" )  OR
                      (COMB-KOH2HKNNUM     =   "021" )  OR
                      (COMB-KOH3HKNNUM     =   "021" )  OR
                      (COMB-KOH4HKNNUM     =   "021" ))
      *            Àº¿ÀÆþ±¡
                 OR  ((COMB-KOH1HKNNUM     =   "020" )  OR
                      (COMB-KOH2HKNNUM     =   "020" )  OR
                      (COMB-KOH3HKNNUM     =   "020" )  OR
                      (COMB-KOH4HKNNUM     =   "020" ))
      *            »ÄÎ±Ë®¿Í
                 OR  ((COMB-KOH1HKNNUM     =   "025" )  OR
                      (COMB-KOH2HKNNUM     =   "025" )  OR
                      (COMB-KOH3HKNNUM     =   "025" )  OR
                      (COMB-KOH4HKNNUM     =   "025" ))
                   MOVE    AGECHK-ROUJIN2      TO  AGECHK-ROUJIN
               END-IF
           END-IF
           .
       20012-KOH-ROJIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÃÏÊý¸øÈñÃ±ÆÈÈ½Äê½èÍý
      *****************************************************************
       2010-KOHONLY-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-KOH186
           MOVE    ZERO                TO  FLG-KOHCHK
      *
      *    Ä¹ºê¸©¤ÎÈïÇú¼Ô°åÎÅ¡Ê£±£¸£¶¡Ë¤ÏÃ±ÆÈ²ÄÇ½
           IF      (COMB-HKNNUM         =   SPACE )  AND
                   (COMB-KOH1HKNNUM     =   "186" )  AND
                   (COMB-KOH2HKNNUM     =   SPACE )  AND
                   (COMB-KOH3HKNNUM     =   SPACE )  AND
                   (COMB-KOH4HKNNUM     =   SPACE )
               PERFORM 20101-KOH186-CHK-SEC
           END-IF
           IF     (FLG-KOH186           =   ZERO  )  AND
                  (COMB-HKNNUM          =   SPACE )  AND
                  (COMB-KOH1HKNNUM NOT  =   SPACE )
               MOVE    ZERO             TO  FLG-CHK-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF    (COMB-KOHHKNNUM(IDX)  =   SPACE  )
                     OR  (COMB-KOHHKNNUM(IDX)  >=  "100"  AND
                                               <=  "899"  )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-CHK-OK
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   4   )
                             OR   (FLG-CHK-OK  =   1   )
      *            ¸øÈñ
                   IF      COMB-KOHHKNNUM (IDX)    NOT =   SPACE
                       MOVE    COMB-KOHHKNNUM (IDX)    TO  WRK-HKNNUM
                       MOVE    COMB-KOHPAYKBN (IDX)    TO  WRK-PAYKBN
                       PERFORM 3001-HKNNUM-CHK-SEC
                       IF     (FLG-HKNNUM          =   ZERO  )
                          AND (HKN-HKNKOHSBTKBN    NOT =   "6" )
      *                    Ã±ÆÈ¸øÈñ°Ê³°
                           MOVE    1               TO  FLG-KOHCHK
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           IF      FLG-KOHCHK          =   1
               MOVE    1                   TO  AGECHK-KOHCHK
           ELSE
               MOVE    ZERO                TO  AGECHK-KOHCHK
           END-IF
           .
      *
       2010-KOHONLY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¹ºê¸©¤ÎÈïÇú¼Ô°åÎÅ¡Ê£±£¸£¶¡Ë½èÍý
      *****************************************************************
       20101-KOH186-CHK-SEC         SECTION.
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷¡Ê´ðËÜ¾ðÊó¡Ë
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    AGECHK-SRYYMD       TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    AGECHK-HOSPNUM      TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1001-REC
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *        ¸©ÈÖ¹æ
               IF      SYS-1001-PREFNUM    =   "42"
                   MOVE    1                TO  FLG-KOH186
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20101-KOH186-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±¾ðÊó½èÍý
      *****************************************************************
       3001-HKNNUM-CHK-SEC         SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    AGECHK-HOSPNUM      TO  HKN-HOSPNUM
           MOVE    AGECHK-SRYYMD       TO  HKN-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  HKN-TEKEDYMD
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±¤ÎÀÚ¤êÂØ¤¨¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       400-PTHKNCHK-SEC         SECTION.
      *
           IF      COMB-HKNNUM     NOT =   SPACE
      *
               MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
               MOVE    ZERO                TO  WRK-PAYKBN
               PERFORM 4101-KIKANCHK-CHK-SEC
               IF      FLG-HKNNUM-OK       =   ZERO
                   MOVE    1               TO  AGECHK-HKNCHK
               END-IF
           END-IF
      *
           .
       400-PTHKNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¤ÎÀÚ¤êÂØ¤¨¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       410-PTKOHCHK-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4   )
                       OR     (AGECHK-HKNCHK   NOT =   ZERO )
      *        ¸øÈñ
               IF      COMB-KOHHKNNUM (IDX)    NOT =   SPACE
                   MOVE    COMB-KOHHKNNUM (IDX)    TO  WRK-HKNNUM
                   MOVE    COMB-KOHPAYKBN (IDX)    TO  WRK-PAYKBN
      *            ¸øÈñ¤Î´ü¸ÂÀÚ¤ì¥Á¥§¥Ã¥¯
                   PERFORM 4101-KIKANCHK-CHK-SEC
                   IF      FLG-HKNNUM-OK       =   ZERO
                       MOVE    2               TO  AGECHK-HKNCHK
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       410-PTKOHCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±¤ÎÀÚ¤êÂØ¤¨¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4101-KIKANCHK-CHK-SEC         SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    AGECHK-HOSPNUM      TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
           MOVE    AGECHK-SRYYMD       TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"    TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    ZERO                TO  FLG-HKNNUM-OK
           PERFORM
               UNTIL   (FLG-HKNNUM     =   1  )  OR
                       (FLG-HKNNUM-OK  =   1  )
      *
               IF     (HKN-TEKSTYMD        <=  COMB-TEKSTYMD ) AND
                      (HKN-TEKEDYMD        >=  COMB-TEKEDYMD )
      *            ÊÝ¸±´ü´ÖÂÐ¾Ý¤¢¤ê
                   MOVE    1                   TO  FLG-HKNNUM-OK
               END-IF
      *
               IF     (HKN-TEKSTYMD        <=  COMB-TEKSTYMD ) AND
                      (HKN-TEKEDYMD        <   COMB-TEKEDYMD ) AND
                      (HKN-TEKEDYMD        >=  AGECHK-SRYYMD )
      *            ¿ÇÎÅÆü¤ËÊÝ¸±´ü´ÖÂÐ¾Ý¤¢¤ê
                   MOVE    1                   TO  FLG-HKNNUM-OK
               END-IF
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4101-HKNNUM-KIKANCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¤Þ¤È¤áÆþÎÏ½èÍý
      *****************************************************************
       2009-HOUKATUHKN-SEC         SECTION.
      *
           MOVE    ZERO                TO  AGECHK-FTNRATE
           MOVE    SPACE               TO  AGECHK-FTNRATEMEI
      *
      *    Ï·¿ÍÈ½Äê
           IF     (AGECHK-NENREI       >=  65  )  AND
                  (AGECHK-ROUJIN       =   ZERO)
               PERFORM 2009-ROUJIN-CHK-SEC
           END-IF
      *
           .
       2009-HOUKATUHKN-EXT.
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
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
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
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¼¡¸¡º÷½èÍý
      ****************************************************************
       921-HKNNUM-NEXT-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       921-HKNNUM-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¥Þ¥¹¥¿¡¼ÆÉ¤ß¹þ¤ß
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÏ«ºÒÊÝ¸±¾ðÊóÆÉ¤ß¹þ¤ß
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊóÆÉ¤ß¹þ¤ß
      *****************************************************************
       920-PTKOHINF-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       920-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£ÂÆÉ¹þ¤ß½èÍý
      *****************************************************************
       920-DBFETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
