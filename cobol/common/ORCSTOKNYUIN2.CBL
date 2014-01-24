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
           03  WRK-JGN-NISSU               PIC 9(03)  OCCURS 5.
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
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSK5001.INC".
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
           DISPLAY  "LNK-TOKNYUIN2-PTNYUINRRK-TOKUTEI-NYUIN  = " 
                     LNK-TOKNYUIN2-PTNYUINRRK-TOKUTEI-NYUIN
           DISPLAY  "LNK-TOKNYUIN2-IDOKBN    = " LNK-TOKNYUIN2-IDOKBN
           DISPLAY  "LNK-TOKNYUIN2-NYUINYMD  = " LNK-TOKNYUIN2-NYUINYMD
           DISPLAY  "LNK-TOKNYUIN2-SANTEIYMD = " LNK-TOKNYUIN2-SANTEIYMD
           DISPLAY  "LNK-TOKNYUIN2-NISSU08   = " LNK-TOKNYUIN2-NISSU08
      *
      *    ÆÃÊÌ½èÍý
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¤ÎµÞÀ­Áý°­¤Ë¤è¤ëÅ¾Åï
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121110")  AND
                (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-PTNYUINRRK-TOKUTEI-NYUIN   =   "24")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
           END-IF
      *    Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¤ÎµÞÀ­Áý°­¤Ë¤è¤ëÅ¾Åï
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121210")  AND
                (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-PTNYUINRRK-TOKUTEI-NYUIN   =   "25")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
           END-IF
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
                  MOVE  999            TO  WRK-JGN-NISSU(2)
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
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ¡ÊÀ¸³èÎÅÍÜ¡Ë
              WHEN      "190125010"
                  MOVE  "190125010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190125110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
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
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾ÉÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾ÉÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)¿ÇÎÅ½ê¸å´ü¹âÎð¼Ô°åÎÅ´ÉÍýÎÁ
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)¿ÇÎÅ½ê¸å´ü¹âÎð¼Ô°åÎÅ´ÉÍýÎÁ¡ÊÀ¸³èÎÅÍÜ¡Ë
              WHEN      "190125010"
                  MOVE  "190125010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190125110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
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
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *      (A303-2)¿·À¸»ù¼£ÎÅ²óÉü¼¼Æþ±¡°åÎÅ´ÉÍýÎÁ
      *          ¢¨¾åµ­Æþ±¡ÎÁ¤Ï½ÐÀ¸»þÂÎ½Å¤Ë¤è¤ê¾å¸ÂÆü¿ô¤¬ÊÑÆ°¤¹¤ë°Ù¡¢
      *            ¤³¤Î¥µ¥Ö¥×¥í¥°¥é¥à¤Ç¤ÏÂÐ±þ¤·¤Ê¤¤
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
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤È»»Äê¾å¸ÂÆü¿ô¤Î¥»¥Ã¥È¡ÊÊ¿À®£²£´Ç¯£´·î£±Æü°Ê¹ß¡Ë
           IF     LNK-TOKNYUIN2-SANTEIYMD   >=   "20120401"
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
      *      (A302)¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *      (A303)Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *      (A303-2)¿·À¸»ù¼£ÎÅ²óÉü¼¼Æþ±¡°åÎÅ´ÉÍýÎÁ
      *          ¢¨¾åµ­Æþ±¡ÎÁ¤Ï½ÐÀ¸»þÂÎ½Å¤Ë¤è¤ê¾å¸ÂÆü¿ô¤¬ÊÑÆ°¤¹¤ë°Ù¡¢
      *            ¤³¤Î¥µ¥Ö¥×¥í¥°¥é¥à¤Ç¤ÏÂÐ±þ¤·¤Ê¤¤
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
                  IF    LNK-TOKNYUIN2-TOKUBETSU-SYORI-FLG(1)  =  "1"
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
                  IF    LNK-TOKNYUIN2-TOKUBETSU-SYORI-FLG(1)  =  "1"
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
                  MOVE  999            TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  999            TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  999            TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A314)Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  999            TO  WRK-JGN-NISSU(3)
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
      *    ÆÃÄêÆþ±¡ÎÁ¤Ë¥Ò¥Ã¥È¤·¤¿¾ì¹ç¤Î¥Õ¥é¥°¥»¥Ã¥È
           IF        WRK-TOKNYUIN2-SRYCD(1)   NOT =   SPACE
               MOVE  1                 TO  FLG-HIT
           END-IF
      *
      *    »»Äê¾å¸ÂÆü¿ô¤ò¥×¥é¥¹·×»»¤Ç»ÈÍÑ¤¹¤ë°Ù¤Ë¥Þ¥¤¥Ê¥¹£±¤·¤Æ¤ª¤¯
           IF    (WRK-JGN-NISSU(1)     >=     1)   AND
                 (WRK-JGN-NISSU(1)     <=   998)
               COMPUTE  WRK-JGN-NISSU(1)  =   WRK-JGN-NISSU(1)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(2)     >=     1)   AND
                 (WRK-JGN-NISSU(2)     <=   998)
               COMPUTE  WRK-JGN-NISSU(2)  =   WRK-JGN-NISSU(2)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(3)     >=     1)   AND
                 (WRK-JGN-NISSU(3)     <=   998)
               COMPUTE  WRK-JGN-NISSU(3)  =   WRK-JGN-NISSU(3)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(4)     >=     1)   AND
                 (WRK-JGN-NISSU(4)     <=   998)
               COMPUTE  WRK-JGN-NISSU(4)  =   WRK-JGN-NISSU(4)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(5)     >=     1)   AND
                 (WRK-JGN-NISSU(5)     <=   998)
               COMPUTE  WRK-JGN-NISSU(5)  =   WRK-JGN-NISSU(5)   -  1
           END-IF
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
                   IF    WRK-JGN-NISSU(1) =   999
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
                   IF    WRK-JGN-NISSU(2) =   999
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
                   IF    WRK-JGN-NISSU(3) =   999
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
                   IF    WRK-JGN-NISSU(4) =   999
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
      *
           .
       200-EDIT-EXT.
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
