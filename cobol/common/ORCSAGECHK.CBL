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
      * 02.00.01     NACL-Â¿¡¹Ç¼  06/01/12  »ùÆ¸ÊÝ¸î¡Ê£°£µ£³¡ËÄÉ²Ã
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
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-BYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
           03  WRK-NENREI-YY           PIC 9(03).
           03  WRK-NENREI-MM           PIC 9(02).
           03  WRK-NENREI-DD           PIC 9(02).
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
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSAGECHK.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSAGECHKAREA.
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
           IF     (AGECHK-HOSPID       =   SPACE   ) OR
                  (AGECHK-PTID         NOT NUMERIC )
               MOVE    99              TO  AGECHK-RC
               GO      TO      000-PROC-EXT
           END-IF
      *    Ç¯Îð·×»»
           PERFORM 100-NENREI-SEC
           IF      AGECHK-RC       NOT =   ZERO
               GO      TO      000-PROC-EXT
           END-IF
      *
      *    ÊÝ¸±ÁÈ¹çÈÖ¹æ¤¬¤¢¤ë¤È¤­¡¢ÉéÃ´³ä¹ç¤ò»»Äê¤¹¤ë
           IF      AGECHK-HKNCOMBINUM  NOT =   SPACE
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
               MOVE    AGECHK-HOSPID       TO  PTINF-HOSPID
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
      *    Ç¯Îð¤Ç¤ÎÏ·¿ÍÈ½Äê
      *    £È£±£´¡¥£±£°°Ê¹ß£·£°ºÐ¤ò£·£µºÐ¤È¤¹¤ë
      *    £È£±£´¡¥£±£°»þÅÀ¤Ç£·£°ºÐÌ¤Ëþ¤Î»þ
           IF     (AGECHK-SRYYMD       >=   "20021001" )  AND
                  (WRK-BIRTHDAY        >=   "19321001" )
               MOVE    75                   TO  WRK-ROU-NEN
           ELSE
               MOVE    70                   TO  WRK-ROU-NEN
           END-IF
      *    £·£°ºÐ°Ê¾å¤ÏÏ·¿Í¤È¤¹¤ë¡Ê³ºÅö·î£±Æü¤Ë£·£°ºÐ¡Ë
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
       100-NENREI-EXT.
           EXIT.
      *
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
           MOVE    AGECHK-HOSPID       TO  COMB-HOSPID
           MOVE    AGECHK-PTID         TO  COMB-PTID
           MOVE    AGECHK-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
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
               PERFORM 2001-RSI-FTNRATE-SEC
               MOVE    1               TO  AGECHK-HKNKBN
           END-IF
      *
      *    ¼«ÇåÀÕ¤Î»þ¡¢³ä¹ç¤ÎÊÑ¹¹
           IF      COMB-HKNNUM         =   "973"
               PERFORM 2002-JIBAI-FTNRATE-SEC
               MOVE    2               TO  AGECHK-HKNKBN
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
           IF     (AGECHK-SRYYMD       >=  "20021001") AND
                  (AGECHK-NENREI       <   03        )
      *        ¼çÊÝ¸±¤¬¼«Èñ°Ê³°¤Î»þ
               MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
               MOVE    ZERO                TO  WRK-PAYKBN
               PERFORM 3001-HKNNUM-CHK-SEC
      *        ¼«Èñ¡¦¼£¸³¡¦Ï«ºÒ¡¦¼«ÇåÀÕ
               IF     (HKN-HKNKOHSBTKBN    =   "8"  OR  "9" )  OR
                      (COMB-HKNNUM         =   "971"  OR  "973")
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
           PERFORM 2009-ROUJIN-HANTEI-SEC
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
               IF      AGECHK-HKNCHK   NOT =   ZERO
                   GO      TO      200-FTNRATE-EXT
               END-IF
      *        ¸øÈñ¤Î´ü¸ÂÀÚ¤ì¥Á¥§¥Ã¥¯
               PERFORM 410-PTKOHCHK-SEC
               IF      AGECHK-HKNCHK   NOT =   ZERO
                   GO      TO      200-FTNRATE-EXT
               END-IF
           END-IF
      *
      *    £·£°ºÐ°Ê¾å¤Î»þ¡¢Ï·¿Í¤Ç¤¢¤ë¤³¤È
           IF      AGECHK-NENREI       <   70
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    Ä¹ºê¸©¤ÎÈïÇú¼Ô°åÎÅ¡Ê£±£¸£¶¡ËÃ±ÆÈ¤Ï¥Á¥§¥¯¤Ê¤·
           IF      FLG-KOH186          =   1
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    ¼çÊÝ¸±È½Äê
           MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
           MOVE    ZERO                TO  WRK-PAYKBN
           PERFORM 3001-HKNNUM-CHK-SEC
           IF      FLG-HKNNUM          =   ZERO
      *        Ï«ºÒ¡¦¼«Èñ¡¦¼£¸³
               IF      HKN-HKNKOHSBTKBN    =   "4"  OR  "8"  OR  "9"
                   MOVE    1                   TO  FLG-OK
                   GO      TO      200-FTNRATE-EXT
               END-IF
      *        ÊÝ¸±¼ÔÊÔ½¸
               INITIALIZE                  PTHKNINF-REC
               MOVE    AGECHK-HOSPID       TO  PTHKN-HOSPID
               MOVE    AGECHK-PTID         TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
      *
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
                   PERFORM 910-PTHKNINF-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
               IF      FLG-PTHKNINF        =   ZERO
                   IF     (PTHKN-HOJOKBN       =   "8"  OR  "9") OR
                          ((COMB-HKNNUM        =   "060"   )  AND
                           (PTHKN-HOJOKBN      =   "1"  OR  "2"
                                               OR  "4"  OR  "5"))
                       MOVE    1               TO  FLG-OK
                   END-IF
      *            Á¥°÷¤ÇÊä½õ¶èÊ¬¤Î¤¢¤ë»þ£Ï£Ë¤È¤¹¤ë
                   IF     (COMB-HKNNUM         =   "002" ) AND
                          (PTHKN-HOJOKBN   NOT =   SPACE )
                       IF      PTHKN-HOJOKBN       >=  "A" AND
                                                   <=  "F"
                           MOVE    2               TO  FLG-OK
                       ELSE
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-IF
      *            ¶¦ºÑ¤ÇÏ·¿Í¶èÊ¬¤Î¤¢¤ë»þ£Ï£Ë¤È¤¹¤ë
                   IF     (COMB-HKNNUM         =   "031"  OR
                                                   "032"  OR
                                                   "033"  OR
                                                   "034" ) AND
                          (PTHKN-HOJOKBN       =   "B"  OR  "E")
                       MOVE    2               TO  FLG-OK
                   END-IF
               END-IF
               MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
               PERFORM 990-DBCLOSE-SEC
           END-IF
      **** IF      FLG-OK                  =   1
      *        GO      TO      200-FTNRATE-EXT
      **** END-IF
      *
      *    À¸³èÊÝ¸î¤Î¤ß¤Î»þ
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "012" )  OR
                  (COMB-KOH2HKNNUM     =   "012" )  OR
                  (COMB-KOH3HKNNUM     =   "012" )  OR
                  (COMB-KOH4HKNNUM     =   "012" ))
               MOVE    1               TO  FLG-OK2
      *********GO      TO      200-FTNRATE-EXT
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
           IF    ((COMB-KOH1HKNNUM     =   "027" )  AND
                  (COMB-KOH1PAYKBN     =   "01" OR "02" ))  OR
                 ((COMB-KOH2HKNNUM     =   "027" )  AND
                  (COMB-KOH2PAYKBN     =   "01" OR "02"  ))  OR
                 ((COMB-KOH3HKNNUM     =   "027" )  AND
                  (COMB-KOH3PAYKBN     =   "01" OR "02" ))  OR
                 ((COMB-KOH4HKNNUM     =   "027" )  AND
                  (COMB-KOH4PAYKBN     =   "01" OR "02" ))
               MOVE    1               TO  FLG-OK2
           ELSE
      *        ¸øÈñ£±
               IF      COMB-KOH1HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH1HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH1PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "3" OR "6"
                           PERFORM 2001-KOH-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *        ¸øÈñ£²
               IF      COMB-KOH2HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH2HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH2PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "3" OR "6"
                           PERFORM 2001-KOH-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *        ¸øÈñ£³
               IF      COMB-KOH3HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH3HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH3PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "3" OR "6"
                           PERFORM 2001-KOH-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *        ¸øÈñ£´
               IF      COMB-KOH4HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH4HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH4PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "3" OR "6"
                           PERFORM 2001-KOH-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *
           END-IF
      *
           IF      FLG-OK              =   ZERO   AND
                   FLG-OK2             =   ZERO
               MOVE    1               TO  AGECHK-RJNCHK
               GO      TO      200-FTNRATE-EXT
           END-IF
      *    £·£µºÐ°Ê¾å¤Î»þ¡¢Ï·¿Í¸øÈñ¤¬¤¢¤ë¤³¤È
      **** IF     (AGECHK-NENREI       >=  75 )  AND
           IF     (AGECHK-NENREI       >=  WRK-ROU-NEN )  AND
                  (FLG-OK2             =   ZERO)
               MOVE    2               TO  AGECHK-RJNCHK
           END-IF
      *
      *
           .
       200-FTNRATE-EXT.
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
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    AGECHK-SRYYMD       TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
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
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
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
           INITIALIZE                      PTRSIINF-REC
           MOVE    COMB-HOSPID         TO  PTRSI-HOSPID
           MOVE    COMB-PTID           TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTRSIINF-KEY"     TO  ORC-DBPATH
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "PTRSIINF-KEY"     TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
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
           .
      *
       2002-JIBAI-FTNRATE-EXT.
           EXIT.
      *
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
      *    Ï«ºÒ¡¦¼«ÇåÀÕ¤Î»þ¡¢¤¹¤Ù¤Æ°ìÈÌ¤Ø
           IF      COMB-HKNNUM         =   "971"
                                       OR  "973"
               MOVE    ZERO            TO  AGECHK-ROUJIN
           END-IF
      *
           .
       2009-ROUJIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï·¿Í¸øÈñÈ½Äê½èÍý
      *****************************************************************
       2009-ROUJIN-CHK-SEC         SECTION.
      *
      *    ¸øÈñÊÔ½¸
           INITIALIZE                  PTKOHINF-REC
           MOVE    AGECHK-HOSPID       TO  PTKOH-HOSPID
           MOVE    AGECHK-PTID         TO  PTKOH-PTID
           MOVE    "027"               TO  PTKOH-KOHNUM
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKEDYMD
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
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
           MOVE    AGECHK-HOSPID       TO  PTKOH-HOSPID
           MOVE    AGECHK-PTID         TO  PTKOH-PTID
           MOVE    "027"               TO  PTKOH-KOHNUM
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKEDYMD
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
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
           IF      FLG-KOH186           =   ZERO
             IF      (COMB-HKNNUM         =   SPACE )  AND
                     (COMB-KOH1HKNNUM     >=  "100"  AND
                                          <=  "899"  )  AND
                    ((COMB-KOH2HKNNUM     =   SPACE  ) OR
                     (COMB-KOH2HKNNUM     >=  "100"  AND
                                          <=  "899"  )) AND
                     ((COMB-KOH3HKNNUM     =   SPACE  ) OR
                      (COMB-KOH3HKNNUM     >=  "100"  AND
                                           <=  "899"  )) AND
                     ((COMB-KOH4HKNNUM     =   SPACE  ) OR
                      (COMB-KOH4HKNNUM     >=  "100"  AND
                                           <=  "899"  ))
      *        ¸øÈñ£±
               IF      COMB-KOH1HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH1HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH1PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "6"
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-KOHCHK
                       END-IF
                   END-IF
               END-IF
      *        ¸øÈñ£²
               IF      COMB-KOH2HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH2HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH2PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "6"
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-KOHCHK
                       END-IF
                   END-IF
               END-IF
      *        ¸øÈñ£³
               IF      COMB-KOH3HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH3HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH3PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "6"
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-KOHCHK
                       END-IF
                   END-IF
               END-IF
      *        ¸øÈñ£´
               IF      COMB-KOH4HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH4HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH4PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "6"
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-KOHCHK
                       END-IF
                   END-IF
               END-IF
             END-IF
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
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    AGECHK-SRYYMD       TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
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
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
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
           MOVE    AGECHK-HOSPID       TO  HKN-HOSPID
           MOVE    AGECHK-SRYYMD       TO  HKN-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  HKN-TEKEDYMD
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    AGECHK-SRYYMD       TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
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
           IF      COMB-HKNNUM         =   SPACE
               GO      TO      400-PTHKNCHK-EXT
           END-IF
      *
           MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
           MOVE    ZERO                TO  WRK-PAYKBN
           PERFORM 4101-KIKANCHK-CHK-SEC
           IF      FLG-HKNNUM-OK       =   ZERO
               MOVE    1               TO  AGECHK-HKNCHK
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
           IF      COMB-KOH1HKNNUM     NOT =   SPACE
      *        ¸øÈñ¤Î´ü¸ÂÀÚ¤ì¥Á¥§¥Ã¥¯
               MOVE    COMB-KOH1HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH1PAYKBN     TO  WRK-PAYKBN
               PERFORM 4101-KIKANCHK-CHK-SEC
               IF      FLG-HKNNUM-OK       =   ZERO
                   MOVE    2               TO  AGECHK-HKNCHK
                   GO      TO      410-PTKOHCHK-EXT
               END-IF
           END-IF
      *
           IF      COMB-KOH2HKNNUM     NOT =   SPACE
      *        ¸øÈñ¤Î´ü¸ÂÀÚ¤ì¥Á¥§¥Ã¥¯
               MOVE    COMB-KOH2HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH2PAYKBN     TO  WRK-PAYKBN
               PERFORM 4101-KIKANCHK-CHK-SEC
               IF      FLG-HKNNUM-OK       =   ZERO
                   MOVE    2               TO  AGECHK-HKNCHK
                   GO      TO      410-PTKOHCHK-EXT
               END-IF
           END-IF
      *
           IF      COMB-KOH3HKNNUM     NOT =   SPACE
      *        ¸øÈñ¤Î´ü¸ÂÀÚ¤ì¥Á¥§¥Ã¥¯
               MOVE    COMB-KOH3HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH3PAYKBN     TO  WRK-PAYKBN
               PERFORM 4101-KIKANCHK-CHK-SEC
               IF      FLG-HKNNUM-OK       =   ZERO
                   MOVE    2               TO  AGECHK-HKNCHK
                   GO      TO      410-PTKOHCHK-EXT
               END-IF
           END-IF
      *
           IF      COMB-KOH4HKNNUM     NOT =   SPACE
      *        ¸øÈñ¤Î´ü¸ÂÀÚ¤ì¥Á¥§¥Ã¥¯
               MOVE    COMB-KOH4HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH4PAYKBN     TO  WRK-PAYKBN
               PERFORM 4101-KIKANCHK-CHK-SEC
               IF      FLG-HKNNUM-OK       =   ZERO
                   MOVE    2               TO  AGECHK-HKNCHK
                   GO      TO      410-PTKOHCHK-EXT
               END-IF
           END-IF
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
           MOVE    AGECHK-HOSPID       TO  HKN-HOSPID
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    AGECHK-SRYYMD       TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY7"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNNUM-KEY7"       TO  ORC-DBPATH
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
               MOVE    "HKNNUM-KEY7"       TO  ORC-DBPATH
               PERFORM 921-HKNNUM-NEXT-SEC
           END-PERFORM
           MOVE    "HKNNUM-KEY7"       TO  ORC-DBPATH
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
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           .
       921-HKNNUM-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¥Þ¥¹¥¿¡¼ÆÉ¤ß¹þ¤ß
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÏ«ºÒÊÝ¸±¾ðÊóÆÉ¤ß¹þ¤ß
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
