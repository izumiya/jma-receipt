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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSTOKNYUIN2.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÆÃÄêÆþ±¡ÎÁ»»Äê´ü´Ö¼èÆÀ¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  13/08/06    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
           03  FLG-PTCONF                  PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
      *  ¡Ê£°¡§¼£ÎÅ¼¼Ã±°Ì°Ê³°¤ÎÆÃÄêÆþ±¡ÎÁ¡¢£±¡§¼£ÎÅ¼¼Ã±°Ì¤ÎÆÃÄêÆþ±¡ÎÁ¡Ë
           03  FLG-ROOM                    PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  TN-IDX                      PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
           03  WRK-TOKNYUIN2-SRYCD         PIC X(09)  OCCURS 5.
           03  WRK-JGN-NISSU               PIC 9(05)  OCCURS 5.
           03  WRK-SRYCD-COUNT             PIC 9(02).
           03  WRK-TOKUTEI-DAY             PIC 9(03).
           03  WRK-TOKUTEI-DAY1            PIC 9(03).
           03  WRK-TOKUTEI-DAY2            PIC 9(03).
           03  WRK-TOKUTEI-DAY3            PIC 9(03).
           03  WRK-TOKUTEI-DAY4            PIC 9(03).
           03  WRK-TOKUTEI-DAY5            PIC 9(03).
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
      *
           03  WRK-WEIGHT                  PIC 9(04).
           03  WRK-TOKUTEI-ROOM-NISSU      PIC 9(04).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSK5001.INC".
      *
      *    ´µ¼Ô¸ÄÊÌÀßÄê
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ÄÌ»»Æü¿ô¼èÆÀ¥µ¥Ö
           COPY "CPORCSTUSAN.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ÆÃÄêÆþ±¡ÎÁ»»Äê´ü´Ö¼èÆÀ¥Ñ¥é¥á¥¿
           COPY    "CPORCSTOKNYUIN2.INC".
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTOKNYUIN2-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      LNK-TOKNYUIN2-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN2-RC
      *
           PERFORM    200-EDIT-SEC
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¥»¥Ã¥È
      *****************************************************************
       200-EDIT-SEC                    SECTION.
      *
           DISPLAY  "====  ORCSTOKNYUIN2 SET  ===="
           DISPLAY  "LNK-TOKNYUIN2-TOKUTEI-NYUIN  = " 
                     LNK-TOKNYUIN2-TOKUTEI-NYUIN
           DISPLAY  "LNK-TOKNYUIN2-IDOKBN    = " LNK-TOKNYUIN2-IDOKBN
           DISPLAY  "LNK-TOKNYUIN2-NYUINYMD  = " LNK-TOKNYUIN2-NYUINYMD
           DISPLAY  "LNK-TOKNYUIN2-SANTEIYMD = " LNK-TOKNYUIN2-SANTEIYMD
           DISPLAY  "LNK-TOKNYUIN2-NISSU08   = " LNK-TOKNYUIN2-NISSU08
      *
      *    ÆÃÊÌ½èÍý
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¤ÎµÞÀ­Áý°­¤Ë¤è¤ëÅ¾Åï
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121110")  AND
                (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "24")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
           END-IF
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¤ÎµÞÀ­Áý°­¤Ë¤è¤ëÅ¾Åï
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121210")  AND
                (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "25")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
           END-IF
      *
      *    ½ÐÀ¸»ùÂÎ½Å¤Î¼èÆÀ
           MOVE   SPACE                TO  PTCONF-REC
           INITIALIZE                      PTCONF-REC
           MOVE   LNK-TOKNYUIN2-HOSPNUM
                                       TO  PTCONF-HOSPNUM
           MOVE   LNK-TOKNYUIN2-PTID   TO  PTCONF-PTID
           MOVE   "BIRTHWEIGHT"        TO  PTCONF-CKEY
           PERFORM 900-PTCONF-READ-SEC
           IF     FLG-PTCONF           =   ZERO
               MOVE   PTCONF-CDATA(1:4)  TO  WRK-WEIGHT
           ELSE
               MOVE   9999               TO  WRK-WEIGHT
           END-IF
           DISPLAY  "ORCSTOKNYUIN2  WRK-WEIGHT = " WRK-WEIGHT
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤È»»Äê¾å¸ÂÆü¿ô¤Î¥»¥Ã¥È¡ÊÊ¿À®£±£¸Ç¯£³·î£³£±Æü°ÊÁ°¡Ë
           IF     LNK-TOKNYUIN2-SANTEIYMD   <=   "20060331"
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê¹âÅÙ¡Ë
              WHEN      "190110210"
                  MOVE  "190110210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê£Á¡Ë
              WHEN      "190110310"
                  MOVE  "190110310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê¹âÅÙ¡Ë
              WHEN      "190110410"
                  MOVE  "190110410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê£Á¡Ë
              WHEN      "190110510"
                  MOVE  "190110510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê½Å¾É¼ÔÅùÈæÎ¨´ð½àÌ¤Ëþ¡Ë
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
              WHEN      "190024610"
                  MOVE  "190024610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *          ¢¨¾åµ­Æþ±¡ÎÁ¤Ï½ÐÀ¸»þÂÎ½Å¤Ë¤è¤ê¾å¸ÂÆü¿ô¤¬ÊÑÆ°¤¹¤ë°Ù¡¢
      *            ¤³¤Î¥µ¥Ö¥×¥í¥°¥é¥à¤Ç¤ÏÂÐ±þ¤·¤Ê¤¤
      *      (A304)¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
              WHEN      "190024810"
                  MOVE  "190024810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)°ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308)²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ
              WHEN      "190075910"
                  MOVE  "190075910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  180            TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A316)¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤È»»Äê¾å¸ÂÆü¿ô¤Î¥»¥Ã¥È
      *    ¡ÊÊ¿À®£±£¸Ç¯£´·î£±Æü¤«¤éÊ¿À®£²£°Ç¯£³·î£³£±Æü¡Ë
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20060401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20080331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê¹âÅÙ¡Ë
              WHEN      "190110210"
                  MOVE  "190110210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê£Á¡Ë
              WHEN      "190110310"
                  MOVE  "190110310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê¹âÅÙ¡Ë
              WHEN      "190110410"
                  MOVE  "190110410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê£Á¡Ë
              WHEN      "190110510"
                  MOVE  "190110510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê½Å¾É¼ÔÅùÈæÎ¨´ð½àÅ¬¹ç¡Ë
              WHEN      "190024610"
                  MOVE  "190024610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *          ¢¨¾åµ­Æþ±¡ÎÁ¤Ï½ÐÀ¸»þÂÎ½Å¤Ë¤è¤ê¾å¸ÂÆü¿ô¤¬ÊÑÆ°¤¹¤ë°Ù¡¢
      *            ¤³¤Î¥µ¥Ö¥×¥í¥°¥é¥à¤Ç¤ÏÂÐ±þ¤·¤Ê¤¤
      *      (A304)¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
              WHEN      "190024810"
                  MOVE  "190024810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)°ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ¡ÊÀ¸³èÎÅÍÜ¡Ë
              WHEN      "190125010"
                  MOVE  "190125010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190125110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤È»»Äê¾å¸ÂÆü¿ô¤Î¥»¥Ã¥È
      *    ¡ÊÊ¿À®£²£°Ç¯£´·î£±Æü¤«¤éÊ¿À®£²£²Ç¯£³·î£³£±Æü¡Ë
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20080401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20100331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê½Å¾É¼ÔÅùÈæÎ¨´ð½àÅ¬¹ç¡Ë
              WHEN      "190024610"
                  MOVE  "190024610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *          ¢¨¾åµ­Æþ±¡ÎÁ¤Ï½ÐÀ¸»þÂÎ½Å¤Ë¤è¤ê¾å¸ÂÆü¿ô¤¬ÊÑÆ°¤¹¤ë°Ù¡¢
      *            ¤³¤Î¥µ¥Ö¥×¥í¥°¥é¥à¤Ç¤ÏÂÐ±þ¤·¤Ê¤¤
      *      (A304)¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
              WHEN      "190024810"
                  MOVE  "190024810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)°ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾ÉÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾ÉÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)¿ÇÎÅ½ê¸å´ü¹âÎð¼Ô°åÎÅ´ÉÍýÎÁ
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)¿ÇÎÅ½ê¸å´ü¹âÎð¼Ô°åÎÅ´ÉÍýÎÁ¡ÊÀ¸³èÎÅÍÜ¡Ë
              WHEN      "190125010"
                  MOVE  "190125010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190125110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤È»»Äê¾å¸ÂÆü¿ô¤Î¥»¥Ã¥È
      *    ¡ÊÊ¿À®£²£²Ç¯£´·î£±Æü¤«¤éÊ¿À®£²£´Ç¯£³·î£³£±Æü¡Ë
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20100401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20120331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡ÊÊìÂÎ¡¦ÂÛ»ù¡Ë
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê¿·À¸»ù¡Ë
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303-2)¿·À¸»ù¼£ÎÅ²óÉü¼¼Æþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)°ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤È»»Äê¾å¸ÂÆü¿ô¤Î¥»¥Ã¥È
      *    ¡ÊÊ¿À®£²£´Ç¯£´·î£±Æü¤«¤éÊ¿À®£²£¶Ç¯£³·î£³£±Æü¡Ë
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20120401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20140331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-4)¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡ÊÊìÂÎ¡¦ÂÛ»ù¡Ë
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê¿·À¸»ù¡Ë
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303-2)¿·À¸»ù¼£ÎÅ²óÉü¼¼Æþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)°ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡Ê»ØÄêÃÏ°è¡Ë
              WHEN      "190150310"
                  MOVE  "190150310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê»ØÄêÃÏ°è¡Ë
              WHEN      "190150410"
                  MOVE  "190150410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A310)´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A317)ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡Ë
              WHEN      "190152610"
                  MOVE  "190152610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A317)ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡¦¥ê¥Ï¥Ó¥ê»»Äê´µ¼Ô¡Ë
              WHEN      "190152710"
                  MOVE  "190152710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤È»»Äê¾å¸ÂÆü¿ô¤Î¥»¥Ã¥È¡ÊÊ¿À®£²£¶Ç¯£´·î£±Æü°Ê¹ß¡Ë
           IF     LNK-TOKNYUIN2-SANTEIYMD   >=   "20140401"
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±
              WHEN      "190174410"
                  MOVE  "190174410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
              WHEN      "190174610"
                  MOVE  "190174610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
              WHEN      "190174810"
                  MOVE  "190174810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£³
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£´¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ£±
              WHEN      "190175010"
                  MOVE  "190175010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ£²
              WHEN      "190175110"
                  MOVE  "190175110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±
      **          ÂÎ½Å£±¡¤£µ£°£°¥°¥é¥à°Ê¾å¤Ç¡¤¸üÀ¸Ï«Æ¯Âç¿Ã¤¬Äê¤á¤ë¼À´µ
      **          ¤ò¼çÉÂ¤È¤¹¤ë´µ¼Ô¤Ï£³£µÆü¸ÂÅÙ¤À¤¬¡¢¸½¾õ¤Ç¤ÏÈ½Äê¤Ç¤­¤Ê¤¤
      **          °Ù¡¢Ì¤ÂÐ±þ
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²
      **          ÂÎ½Å£±¡¤£µ£°£°¥°¥é¥à°Ê¾å¤Ç¡¤¸üÀ¸Ï«Æ¯Âç¿Ã¤¬Äê¤á¤ë¼À´µ
      **          ¤ò¼çÉÂ¤È¤¹¤ë´µ¼Ô¤Ï£³£µÆü¸ÂÅÙ¤À¤¬¡¢¸½¾õ¤Ç¤ÏÈ½Äê¤Ç¤­¤Ê¤¤
      **          °Ù¡¢Ì¤ÂÐ±þ
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡ÊÊìÂÎ¡¦ÂÛ»ù¡Ë
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê¿·À¸»ù¡Ë
      **          ÂÎ½Å£±¡¤£µ£°£°¥°¥é¥à°Ê¾å¤Ç¡¤¸üÀ¸Ï«Æ¯Âç¿Ã¤¬Äê¤á¤ë¼À´µ
      **          ¤ò¼çÉÂ¤È¤¹¤ë´µ¼Ô¤Ï£³£µÆü¸ÂÅÙ¤À¤¬¡¢¸½¾õ¤Ç¤ÏÈ½Äê¤Ç¤­¤Ê¤¤
      **          °Ù¡¢Ì¤ÂÐ±þ
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)¿·À¸»ù¼£ÎÅ²óÉü¼¼Æþ±¡°åÎÅ´ÉÍýÎÁ
      **          ÂÎ½Å£±¡¤£µ£°£°¥°¥é¥à°Ê¾å¤Ç¡¤¸üÀ¸Ï«Æ¯Âç¿Ã¤¬Äê¤á¤ë¼À´µ
      **          ¤ò¼çÉÂ¤È¤¹¤ë´µ¼Ô¤Ï£µ£°Æü¸ÂÅÙ¤À¤¬¡¢¸½¾õ¤Ç¤ÏÈ½Äê¤Ç¤­¤Ê¤¤
      **          °Ù¡¢Ì¤ÂÐ±þ
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A305)°ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡Ê»ØÄêÃÏ°è¡Ë
              WHEN      "190150310"
                  MOVE  "190150310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê»ØÄêÃÏ°è¡Ë
              WHEN      "190150410"
                  MOVE  "190150410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡£±
              WHEN      "190175310"
                  MOVE  "190175310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡£±¡ÊÀ¸³èÎÅÍÜ¡Ë
              WHEN      "190175410"
                  MOVE  "190175410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±
              WHEN      "190175510"
                  MOVE  "190175510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë
              WHEN      "190175610"
                  MOVE  "190175610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡£²
              WHEN      "190175710"
                  MOVE  "190175710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡£²¡ÊÀ¸³èÎÅÍÜ¡Ë
              WHEN      "190175810"
                  MOVE  "190175810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²
              WHEN      "190175910"
                  MOVE  "190175910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë
              WHEN      "190176010"
                  MOVE  "190176010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡£±¡ÊÆÃÄêÃÏ°è¡Ë
              WHEN      "190176110"
                  MOVE  "190176110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë
              WHEN      "190176210"
                  MOVE  "190176210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÆÃÄêÃÏ°è¡Ë
              WHEN      "190176310"
                  MOVE  "190176310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë
              WHEN      "190176410"
                  MOVE  "190176410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡£²¡ÊÆÃÄêÃÏ°è¡Ë
              WHEN      "190176510"
                  MOVE  "190176510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë
              WHEN      "190176610"
                  MOVE  "190176610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÆÃÄêÃÏ°è¡Ë
              WHEN      "190176710"
                  MOVE  "190176710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë
              WHEN      "190176810"
                  MOVE  "190176810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-3)Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡Ë
              WHEN      "190152610"
                  MOVE  "190152610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡¦¥ê¥Ï¥Ó¥ê»»Äê´µ¼Ô¡Ë
              WHEN      "190152710"
                  MOVE  "190152710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡ÊÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍý£±¡Ë
              WHEN      "190182310"
                  MOVE  "190182310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡ÊÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍý£²¡Ë
              WHEN      "190182410"
                  MOVE  "190182410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
             END-EVALUATE
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¤Ë¥Ò¥Ã¥È¤·¤¿¾ì¹ç¤Î¥Õ¥é¥°¥»¥Ã¥È
           IF        WRK-TOKNYUIN2-SRYCD(1)   NOT =   SPACE
               MOVE  1                 TO  FLG-HIT
           END-IF
      *
      *    »»Äê¾å¸ÂÆü¿ô¤ò¥×¥é¥¹·×»»¤Ç»ÈÍÑ¤¹¤ë°Ù¤Ë¥Þ¥¤¥Ê¥¹£±¤·¤Æ¤ª¤¯
           IF    (WRK-JGN-NISSU(1)     >=     1)   AND
                 (WRK-JGN-NISSU(1)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(1)  =   WRK-JGN-NISSU(1)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(2)     >=     1)   AND
                 (WRK-JGN-NISSU(2)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(2)  =   WRK-JGN-NISSU(2)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(3)     >=     1)   AND
                 (WRK-JGN-NISSU(3)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(3)  =   WRK-JGN-NISSU(3)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(4)     >=     1)   AND
                 (WRK-JGN-NISSU(4)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(4)  =   WRK-JGN-NISSU(4)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(5)     >=     1)   AND
                 (WRK-JGN-NISSU(5)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(5)  =   WRK-JGN-NISSU(5)   -  1
           END-IF
      *
      *    »»ÄêÆÃÄêÆþ±¡ÎÁ¤¬¼£ÎÅ¼¼Ã±°Ì°Ê³°¤Ç¤¢¤ë¾ì¹ç¡¢´û¤Ë»»ÄêºÑ¤ß¤ÎÆ±°ì
      *    ÆÃÄêÆþ±¡ÎÁ¤Ë¤Ä¤¤¤ÆÄÌ»»Æü¿ô¤ò»»½Ð¤¹¤ë
           IF   FLG-ROOM      =    0
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             IF      WRK-TOKNYUIN2-SRYCD(1)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(1)  TO  STUSAN-TOKUTEI-SRYCD(1)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(2)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(2)  TO  STUSAN-TOKUTEI-SRYCD(2)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(3)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(3)  TO  STUSAN-TOKUTEI-SRYCD(3)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(4)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(4)  TO  STUSAN-TOKUTEI-SRYCD(4)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(5)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(5)  TO  STUSAN-TOKUTEI-SRYCD(5)
             END-IF
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
           END-IF
      *
      *    »»ÄêÆÃÄêÆþ±¡ÎÁ¤¬¼£ÎÅ¼¼Ã±°Ì¤Ç¤¢¤ë¾ì¹ç¡¢´û¤Ë»»ÄêºÑ¤ß¤Î¼£ÎÅ¼¼
      *    Ã±°Ì¤ÎÆÃÄêÆþ±¡ÎÁ¤Ë¤Ä¤¤¤ÆÄÌ»»Æü¿ô¤ò»»½Ð¤¹¤ë
           IF   FLG-ROOM      =    1
             PERFORM    201-TOKUTEI-ROOM-NISSU-SEC
           END-IF
      *
      *    ÎòºîÀ®¤ÇÅÐÏ¿¤·¤¿Æþ±¡ÍúÎò¤Ë¤Ä¤¤¤Æ¡ÖÆÃÄêÆþ±¡¡ÝÄÌ»»ÂÐ¾ÝÆü¿ô¡×¤Î
      *    ÆþÎÏÊ¬Æü¿ô¤ò²Ã»»¤¹¤ë
           PERFORM    202-RRK-TOKUTEI-NISSU-SEC
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤¬£±¼ïÎà¤Î¾ì¹ç
           IF    WRK-SRYCD-COUNT       =   1
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¡Ê£±¡Ë
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              ÆÃÄêÆþ±¡³«»ÏÆü¡Ê£±¡Ë
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              ÆÃÄêÆþ±¡½ªÎ»Æü¡Ê£±¡Ë
                   IF    WRK-JGN-NISSU(1) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              °ìÈÌÆþ±¡²ñ·×ºîÀ®¥Õ¥é¥°¤ÈºîÀ®³«»ÏÆü¤Î¥»¥Ã¥È
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
      *
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤¬£²¼ïÎà¤Î¾ì¹ç
           IF    WRK-SRYCD-COUNT       =   2
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¡Ê£±¡Ë
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              ÆÃÄêÆþ±¡³«»ÏÆü¡Ê£±¡Ë
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              ÆÃÄêÆþ±¡½ªÎ»Æü¡Ê£±¡Ë
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¡Ê£²¡Ë
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ÆÃÄêÆþ±¡³«»ÏÆü¡Ê£²¡Ë
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            ÆÃÄêÆþ±¡½ªÎ»Æü¡Ê£²¡Ë
                   IF    WRK-JGN-NISSU(2) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              °ìÈÌÆþ±¡²ñ·×ºîÀ®¥Õ¥é¥°¤ÈºîÀ®³«»ÏÆü¤Î¥»¥Ã¥È
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤¬£³¼ïÎà¤Î¾ì¹ç
           IF    WRK-SRYCD-COUNT       =   3
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¡Ê£±¡Ë
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              ÆÃÄêÆþ±¡³«»ÏÆü¡Ê£±¡Ë
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              ÆÃÄêÆþ±¡½ªÎ»Æü¡Ê£±¡Ë
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¡Ê£²¡Ë
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ÆÃÄêÆþ±¡³«»ÏÆü¡Ê£²¡Ë
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            ÆÃÄêÆþ±¡½ªÎ»Æü¡Ê£²¡Ë
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1
      **             DISPLAY "=== KAKUNIN DISPLAY ==="
      **             DISPLAY  "WRK-JGN-NISSU(2) = " WRK-JGN-NISSU(2)
      **             DISPLAY  "LNK-TOKNYUIN2-NISSU08 = " 
      **                       LNK-TOKNYUIN2-NISSU08
      **             DISPLAY  "WRK-TOKUTEI-DAY1 = " WRK-TOKUTEI-DAY1
      **             DISPLAY  "WRK-TOKUTEI-DAY = " WRK-TOKUTEI-DAY
      **             DISPLAY "=== KAKUNIN DISPLAY END ==="
                   COMPUTE  WRK-TOKUTEI-DAY2 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(3)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¡Ê£³¡Ë
                   MOVE   WRK-TOKNYUIN2-SRYCD(3)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ÆÃÄêÆþ±¡³«»ÏÆü¡Ê£³¡Ë
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            ÆÃÄêÆþ±¡½ªÎ»Æü¡Ê£³¡Ë
                   IF    WRK-JGN-NISSU(3) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(3)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1  -
                                                 WRK-TOKUTEI-DAY2
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              °ìÈÌÆþ±¡²ñ·×ºîÀ®¥Õ¥é¥°¤ÈºîÀ®³«»ÏÆü¤Î¥»¥Ã¥È
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤¬£´¼ïÎà¤Î¾ì¹ç
           IF    WRK-SRYCD-COUNT       =   4
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¡Ê£±¡Ë
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              ÆÃÄêÆþ±¡³«»ÏÆü¡Ê£±¡Ë
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              ÆÃÄêÆþ±¡½ªÎ»Æü¡Ê£±¡Ë
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¡Ê£²¡Ë
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ÆÃÄêÆþ±¡³«»ÏÆü¡Ê£²¡Ë
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            ÆÃÄêÆþ±¡½ªÎ»Æü¡Ê£²¡Ë
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1
                   COMPUTE  WRK-TOKUTEI-DAY2 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(3)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¡Ê£³¡Ë
                   MOVE   WRK-TOKNYUIN2-SRYCD(3)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ÆÃÄêÆþ±¡³«»ÏÆü¡Ê£³¡Ë
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            ÆÃÄêÆþ±¡½ªÎ»Æü¡Ê£³¡Ë
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(3)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1  -
                                               WRK-TOKUTEI-DAY2
                   COMPUTE  WRK-TOKUTEI-DAY3 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                      TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(4)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¡Ê£´¡Ë
                   MOVE   WRK-TOKNYUIN2-SRYCD(4)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ÆÃÄêÆþ±¡³«»ÏÆü¡Ê£´¡Ë
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  4
                       MOVE    LNK-TOKNYUIN2-EDYMD (3)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            ÆÃÄêÆþ±¡½ªÎ»Æü¡Ê£´¡Ë
                   IF    WRK-JGN-NISSU(4) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(4)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1 -
                                                 WRK-TOKUTEI-DAY2 -
                                                 WRK-TOKUTEI-DAY3
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              °ìÈÌÆþ±¡²ñ·×ºîÀ®¥Õ¥é¥°¤ÈºîÀ®³«»ÏÆü¤Î¥»¥Ã¥È
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
           DISPLAY   "TN-IDX = " TN-IDX
      *    ÆÃÄêÆþ±¡´ü´Ö¤ò·Ð²á¤·¤¿ºÝ¤Î°ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
           EVALUATE   LNK-TOKNYUIN2-IDOKBN
               WHEN   "1"
                   MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                              TO WRK-IPN-HANTEI-YMD1
                   IF      WRK-IPN-HANTEI-M1   =   12
                         ADD    1     TO      WRK-IPN-HANTEI-Y1
                         MOVE   1     TO      WRK-IPN-HANTEI-M1
                   ELSE
                         ADD    1     TO      WRK-IPN-HANTEI-M1
                   END-IF
               WHEN   "2"
                   MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                              TO WRK-IPN-HANTEI-YMD1
           END-EVALUATE
      *    ·îËöÆü¤ò»»Äê
           INITIALIZE                STS-AREA-DAY
           INITIALIZE                LNK-DAY7-AREA
           MOVE    "71"              TO  LNK-DAY7-IRAI
           MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
           CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                             LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
      *    ÆÃÄêÆþ±¡ÎÁ¤Î½ªÎ»ÆüÉÕ¤ò¥»¥Ã¥È
           IF     TN-IDX      NOT =     ZERO
               MOVE    LNK-TOKNYUIN2-EDYMD(TN-IDX)
                                     TO      WRK-IPN-HANTEI-YMD2
           ELSE
               MOVE    "00000000"    TO      WRK-IPN-HANTEI-YMD2
           END-IF
      *    °ìÈÌÆþ±¡²ñ·×ºîÀ®ÍÑ¤Î¥Õ¥é¥°ÀßÄê
           DISPLAY "WRK-IPN-HANTEI-YMD1 = " WRK-IPN-HANTEI-YMD1
           DISPLAY "WRK-IPN-HANTEI-YMD2 = " WRK-IPN-HANTEI-YMD2
           IF    WRK-IPN-HANTEI-YMD1    >   WRK-IPN-HANTEI-YMD2
               MOVE    "1"        TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
               IF    WRK-IPN-HANTEI-YMD2   =   "00000000"
                   MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                  TO     LNK-TOKNYUIN2-IPN-KAIKEI-YMD
               END-IF
           ELSE
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
           IF        FLG-HIT    =    ZERO
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
      *    ÎÅÍÜÉÂÅï¡ÊÉÂ¾²¡Ë¤ÎÆþ±¡»þ¤ÏÆÃÄêÆþ±¡ÎÁ»»Äê¸å¤Î°ìÈÌÆþ±¡²ñ·×¤Ï
      *    ºîÀ®¤·¤Ê¤¤
           IF     LNK-TOKNYUIN2-TOKUBETSU-FLG(2)    =    "1"
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
           DISPLAY  "====  ORCSTOKNYUIN2 HENKYAKU  ===="
           DISPLAY  "LNK-TOKNYUIN2-SRYCD(1) = " LNK-TOKNYUIN2-SRYCD(1)
                 " " LNK-TOKNYUIN2-STYMD(1) " " LNK-TOKNYUIN2-EDYMD(1)
           DISPLAY  "LNK-TOKNYUIN2-SRYCD(2) = " LNK-TOKNYUIN2-SRYCD(2)
                 " " LNK-TOKNYUIN2-STYMD(2) " " LNK-TOKNYUIN2-EDYMD(2)
           DISPLAY  "LNK-TOKNYUIN2-SRYCD(3) = " LNK-TOKNYUIN2-SRYCD(3)
                 " " LNK-TOKNYUIN2-STYMD(3) " " LNK-TOKNYUIN2-EDYMD(3)
           DISPLAY  "LNK-TOKNYUIN2-SRYCD(4) = " LNK-TOKNYUIN2-SRYCD(4)
                 " " LNK-TOKNYUIN2-STYMD(4) " " LNK-TOKNYUIN2-EDYMD(4)
           DISPLAY  "LNK-TOKNYUIN2-SRYCD(5) = " LNK-TOKNYUIN2-SRYCD(5)
                 " " LNK-TOKNYUIN2-STYMD(5) " " LNK-TOKNYUIN2-EDYMD(5)
           DISPLAY  "LNK-TOKNYUIN2-IPN-KAIKEI-FLG = "
                     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
           DISPLAY  "LNK-TOKNYUIN2-IPN-KAIKEI-YMD = "
                     LNK-TOKNYUIN2-IPN-KAIKEI-YMD
      *
           IF   LNK-TOKNYUIN2-SRYCD(1)  =      SPACE
               MOVE    1                TO     LNK-TOKNYUIN2-RC
           END-IF
           IF     FLG-HIT     =    ZERO
                  MOVE    2             TO     LNK-TOKNYUIN2-RC
           END-IF
      *
      *
           .
       200-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼£ÎÅ¼¼Ã±°Ì¤ÎÆÃÄêÆþ±¡ÎÁÄÌ»»Æü¿ô¡ÊÊ¿À®£²£¶Ç¯£´·î£±Æü°Ê¹ß¡Ë
      *****************************************************************
       201-TOKUTEI-ROOM-NISSU-SEC                    SECTION.
      *
           MOVE   ZERO                  TO  WRK-TOKUTEI-ROOM-NISSU
      *
           IF    LNK-TOKNYUIN2-NISSU08   NOT =    ZERO
      *     (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£±
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190024510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190128610"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190074510"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£²
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190024310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190128710"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190024410"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190138110"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190138210"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190138310"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190138410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190138510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190138610"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190138710"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190138810"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190138910"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A300)µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190139010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190139110"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190139210"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190139310"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190174410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190174510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190174610"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190174710"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190174810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190174910"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£³
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190116310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190116410"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£´¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190139810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190139910"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190140010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190140110"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ£±
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190175010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ£²
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190175110"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301-2)¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190117310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301-3)Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190120810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301-4)¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190149910"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190150010"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190024710"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190140410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡ÊÊìÂÎ¡¦ÂÛ»ù¡Ë
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190066710"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê¿·À¸»ù¡Ë
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190066810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A303-2)¿·À¸»ù¼£ÎÅ²óÉü¼¼Æþ±¡°åÎÅ´ÉÍýÎÁ
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190140510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A305)°ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190075510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190075610"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
             MOVE    WRK-TOKUTEI-ROOM-NISSU TO  LNK-TOKNYUIN2-NISSU08
           END-IF
           .
       201-TOKUTEI-ROOM-NISSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎòºîÀ®¤ÇÅÐÏ¿¤·¤¿Æþ±¡ÍúÎò¤Ë¤Ä¤¤¤Æ¡ÖÆÃÄêÆþ±¡¡ÝÄÌ»»ÂÐ¾ÝÆü¿ô¡×¤Î
      *    ÆþÎÏÊ¬Æü¿ô¤ò²Ã»»¤¹¤ë
      *****************************************************************
       202-RRK-TOKUTEI-NISSU-SEC                    SECTION.
      *
           INITIALIZE                       PTNYUINRRK-REC
           MOVE    LNK-TOKNYUIN2-HOSPNUM
                                        TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID   TO  PTNYUINRRK-PTID
           MOVE    LNK-TOKNYUIN2-SHONUM TO  PTNYUINRRK-SHONUM
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key65"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
               MOVE    "key65"          TO  MCP-PATHNAME
               PERFORM 920-PTNYUINRRK-FETCH-SEC
               PERFORM    UNTIL   FLG-PTNYUINRRK  =   1
      *
                  IF      PTNYUINRRK-TOKU-TAISYONISSU  NOT =  ZERO
                      ADD PTNYUINRRK-TOKU-TAISYONISSU  TO
                          LNK-TOKNYUIN2-NISSU08
                  END-IF
      *
                  MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
                  MOVE    "key65"          TO  MCP-PATHNAME
                  PERFORM 920-PTNYUINRRK-FETCH-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"       TO   MCP-TABLE
           MOVE    "key65"                TO   MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       202-RRK-TOKUTEI-NISSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸ÄÊÌÀßÄê¥Æ¡¼¥Ö¥ëÆÉ¹þ½èÍý
      *****************************************************************
       900-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTCONF-REC
                   MOVE    ZERO        TO  FLG-PTCONF
               ELSE
                   INITIALIZE              PTCONF-REC
                   MOVE    1           TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTCONF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       920-PTNYUINRRK-FETCH-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF.
      *
       920-PTNYUINRRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
