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
       PROGRAM-ID.                 ORCSAPIHKNALL.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¶¦ÄÌ¡Ê£Á£Ð£É¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Á´ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  17/04/25    NACL¡ÝÂ¿¡¹Ç¼  ¿·µ¬ºîÀ®
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
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-LABOR-SIO       PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK-OK          PIC 9(01).
           03  FLG-COMBIOK         PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDH-MAX             PIC 9(04).
           03  IDK-MAX             PIC 9(04).
           03  IDX-KOH             PIC 9(04).
           03  IDX-TBL             PIC 9(04).
           03  IDXH                PIC 9(04).
      *
           03  HZN-IDK             PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *
           03  WRK-HKNCOMBINUM         PIC X(04).
           03  WRK-HYOJIKBN            PIC X(01).
      *    É½¼¨ÍÑÊÝ¸±
           03  WRK-IDXH                PIC 9(04).
           03  WRK-IDXH2               PIC 9(04).
           03  WRK-CNT-HKN             PIC 9(04).
           03  WRK-CNT-HKN2            PIC 9(04).
      *
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HKNID               PIC 9(10).
           03  WRK-FTNJANUM            PIC X(08).
           03  WRK-JKSNUM              PIC X(20).
           03  WRK-PROVIDER            PIC X(20).
           03  WRK-KOHNUM              PIC X(03).
      *
           03  WRK-START-YM            PIC X(06).
           03  WRK-END-YM              PIC X(06).
      *
      *    ÉéÃ´Î¨¡¦³ÛÊÔ½¸
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(04).
           03  WRK-ZZ9                 PIC ZZZZZ9.
      *
           03  WRK-HZN-RATE            PIC 9(03).
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  TBL-HKNCOMBI-AREA.
           02  TBL-HKNCOMBI-REC        OCCURS   200.
           COPY    "CPHKNCOMBI.INC"    REPLACING
                                       //COMB-// BY //TBL-COMB-//.
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó
       01  TBL-PTHKNINF-AREA.
         02  TBL-PTHKNINF-REC           OCCURS  100.
           COPY    "CPPTHKNINF.INC"     REPLACING
                                        //PTHKN-// BY //TBL-PTHKN-//.
      *
      *    ¸øÈñ¥Þ¥¹¥¿¡Ê¥Æ¡¼¥Ö¥ë¡Ë
       01  TBL-PTKOHINF-AREA.
         02  TBL-PTKOHINF-REC           OCCURS  200.
           COPY    "CPPTKOHINF.INC"     REPLACING
                                        //PTKOH-// BY //TBL-PTKOH-//.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *    ¥Á¥§¥Ã¥¯µ¡Ç½À©¸æ
      *    COPY  "CPSK1008.INC".
      *
           COPY  "CPSK1010.INC".
      *
      *    Ï«ºÒ°åÎÅµ¡´Ø¾ðÊó
           COPY    "CPSK4001.INC".
      *
           COPY    "CPSK4002.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ¼õ¿ÇÍúÎò
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ´µ¼ÔÏ«ºÒÊÝ¸±¾ðÊó
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    ÊÝ¸±¾ðÊó
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    Ï«Æ¯´ð½à´ÆÆÄ½ð¥Þ¥¹¥¿
       01  LABOR-SIO-REC.
           COPY    "CPLABOR-SIO.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ¿ô»úÊÑ´¹ÎÎ°è
           COPY    "CPORCSNUM.INC".
      *
      *    Á´³Ñ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
      ***  COPY    "CPORCSKANACHK.INC".
      *    Ç¯Îð¡¦ÉéÃ´³ä¹ç¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *H27.10
      *    Êä½õ¶èÊ¬Ì¾¾ÎÊÔ½¸¥Ñ¥é¥á¥¿
           COPY    "CPORAPI012SUB1.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *****COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *    ÊÝ¸±ÁÈ¹ç¤»¥»¥Ã¥ÈÎÎ°è
           COPY    "CPORCSAPIHKNALL.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSAPIHKNALLAREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSAPIHKN-HKNCOMBI-AREA
           INITIALIZE                  ORCSAPIHKN-ERRCD
      *
      *    ½é´ü½èÍý
           PERFORM 100-INIT-SYORI-SEC
      *
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
           PERFORM 200-MAIN-SYORI-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SYORI-SEC            SECTION.
      *
      *    ³«»Ï¡¦½ªÎ»ÆüÀßÄê
           MOVE    SPACE               TO  WRK-START-YM
           MOVE    SPACE               TO  WRK-END-YM
           IF      ORCSAPIHKN-START-YM =   SPACE
               IF      ORCSAPIHKN-END-YM   NOT =   SPACE
                   MOVE    ALL "0"             TO  WRK-START-YM
               END-IF
           ELSE
               MOVE    ORCSAPIHKN-START-YM     TO  WRK-START-YM
           END-IF
           IF      ORCSAPIHKN-END-YM   =   SPACE
               IF      ORCSAPIHKN-START-YM NOT =   SPACE
                   MOVE    ALL "9"             TO  WRK-END-YM
               END-IF
           ELSE
               MOVE    ORCSAPIHKN-END-YM     TO  WRK-END-YM
           END-IF
      *
      *    ÊÝ¸±¾ðÊóÊÔ½¸
           INITIALIZE                  TBL-PTHKNINF-AREA
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  IDH-MAX 
           PERFORM     UNTIL   ( FLG-PTHKNINF  =   1 )  OR
                               ( IDH-MAX       >=  100 )
               IF     (PTHKN-SAKJOKBN      =   SPACE )
                   ADD     1                   TO  IDH-MAX 
                   MOVE    PTHKNINF-REC        TO  TBL-PTHKNINF-REC
                                                            (IDH-MAX)
               END-IF
      *
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ¸øÈñÊÔ½¸
           INITIALIZE                  TBL-PTKOHINF-AREA
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOH-PTID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    ZERO                TO  IDK-MAX 
           PERFORM     UNTIL   ( FLG-PTKOHINF  =   1 )  OR
                               ( IDK-MAX       >=  200 )
               IF     (PTKOH-SAKJOKBN      =   SPACE )
                   ADD     1                   TO  IDK-MAX 
                   MOVE    PTKOHINF-REC        TO  TBL-PTKOHINF-REC
                                                            (IDK-MAX)
               END-IF
      *
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
      *****************************************************************
       200-MAIN-SYORI-SEC            SECTION.
      *
      *
      *    ÊÝ¸±ÁÈ¹ç¥Þ¥¹¥¿¡¼ÊÔ½¸
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDXH
           MOVE    ZERO                TO  FLG-ERR
           PERFORM UNTIL   (FLG-HKNCOMBI   =   1   )
                       OR  (IDX-MAX        >   200 )
               MOVE    ZERO                TO  FLG-CHK-OK
               IF     (COMB-DELKBN         =   SPACE     ) AND
                      (COMB-HYOJIKBN   NOT =   "1"       )
                   MOVE    1                   TO  FLG-CHK-OK
      *            »ØÄêÆü¤Î¤ßÂÐ¾Ý
                   IF     (ORCSAPIHKN-KBN          =   "1"   )
                     AND  (ORCSAPIHKN-SYSYMD   NOT =   SPACE )
                       IF     (COMB-TEKSTYMD   <=  ORCSAPIHKN-SYSYMD )
                         AND  (COMB-TEKEDYMD   >=  ORCSAPIHKN-SYSYMD )
                           MOVE    1               TO  FLG-CHK-OK
                       ELSE
                           MOVE    ZERO            TO  FLG-CHK-OK
                       END-IF
                   END-IF
      *            ³«»ÏÇ¯·î¤¢¤ê
                   IF     (WRK-START-YM        =   SPACE )
                      AND (WRK-END-YM          =   SPACE )
                       CONTINUE
                   ELSE
                       IF     (COMB-TEKEDYMD(1:6)  <   WRK-START-YM)
                          OR  (COMB-TEKSTYMD(1:6)  >   WRK-END-YM  )
      *                    ÂÐ¾Ý³°
                           MOVE    ZERO            TO  FLG-CHK-OK
                       END-IF
                   END-IF
               END-IF
               IF      FLG-CHK-OK            =   1
                   ADD     1                 TO  IDX-MAX
                   IF      IDX-MAX           >   200
                       MOVE    1                 TO  FLG-ERR
                   ELSE
      *                ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
                       PERFORM 2001-HKNCOMBI-HENSYU-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ÂÐ¾Ý£²£°£°·ï°Ê¾å
           IF      FLG-ERR             =   1
               MOVE    1                   TO  ORCSAPIHKN-ERRCD
           END-IF
      *    ÂÐ¾Ý¤Ê¤·
           IF      IDXH                =   ZERO
               MOVE    2                   TO  ORCSAPIHKN-ERRCD
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
      *****************************************************************
       2001-HKNCOMBI-HENSYU-SEC             SECTION.
      *
           ADD     1                   TO  IDXH
           MOVE    IDXH                TO  ORCSAPIHKN-OT-MAX
      *
           MOVE    COMB-HKNCOMBINUM
                                       TO  ORCSAPIHKN-THKNCOMBI
                                                               (IDXH)
           MOVE    COMB-HKNNUM
                                       TO  ORCSAPIHKN-HKNNUM (IDXH)
      *    ÊÝ¸±£É£Ä
           MOVE    COMB-HKNID
                                       TO  ORCSAPIHKN-NAI-HKNID (IDXH)
      *    ¼çÊÝ¸±¡ÝÃ»½ÌÀ©ÅÙÌ¾
           MOVE    COMB-SYU-TANSEIDONAME
                                       TO  ORCSAPIHKN-HKNNUM-NAME
                                                                 (IDXH)
      *    ËÜ¿Í²ÈÂ²¶èÊ¬
           MOVE    COMB-HONKZKKBN
                                       TO  ORCSAPIHKN-HONKZKKBN(IDXH)
      *    Êä½õ¶èÊ¬
           MOVE    COMB-HOJOKBN
                                       TO  ORCSAPIHKN-HOJOKBN (IDXH)
           PERFORM 20011-HOJOKBN-NAME-HEN-SEC
      *
      *    ·ÑÂ³¶èÊ¬
           MOVE    COMB-CONTKBN        TO  ORCSAPIHKN-CONTKBN (IDXH)
      *    ÉéÃ´³ä¹ç
           IF     (ORCSAPIHKN-KBN          =   "1"   )
              AND (ORCSAPIHKN-SYSYMD   NOT =   SPACE )
      *        ½èÍýÆüÉÕ¤Ç¤ÎÉéÃ´³ä¹ç
               PERFORM 20016-AGEFTNRATE-SEC
           ELSE
      *        ÊÝ¸±ÁÈ¹ç¤»ÉéÃ´³ä¹ç
               MOVE    COMB-GAI-KFTNRATE   TO  WRK-HZN-RATE
               PERFORM 4001-FTNRATE-HEN-SEC
               MOVE    WRK-FTNRATEMEI
                                       TO  ORCSAPIHKN-COMB-RATEG(IDXH)
               MOVE    COMB-NYU-KFTNRATE   TO  WRK-HZN-RATE
               PERFORM 4001-FTNRATE-HEN-SEC
               MOVE    WRK-FTNRATEMEI
                                       TO  ORCSAPIHKN-COMB-RATEN(IDXH)
           END-IF
      *    Í­¸ú´ü´Ö
           MOVE    COMB-TEKSTYMD       TO  WRK-SYMD
           PERFORM 800-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE       TO  ORCSAPIHKN-COMB-TEKSTYMD(IDXH)
           MOVE    COMB-TEKEDYMD      TO  WRK-SYMD
           PERFORM 800-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE       TO  ORCSAPIHKN-COMB-TEKEDYMD(IDXH)
      *
           MOVE    COMB-TEKSTYMD      TO  ORCSAPIHKN-COMBN-TEKSTYMD
                                                                (IDXH)
           MOVE    COMB-TEKEDYMD      TO  ORCSAPIHKN-COMBN-TEKEDYMD
                                                                (IDXH)
      *    ÈóÉ½¼¨¶èÊ¬
           MOVE    COMB-HYOJIKBN      TO  ORCSAPIHKN-HYOJIKBN (IDXH)
      *
      *    ÊÝ¸±¾ðÊó
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH         >   IDH-MAX  )
               IF      COMB-HKNID          =   TBL-PTHKN-HKNID (IDH)
                   MOVE    TBL-PTHKN-HKNJANUM(IDH)
                                           TO  ORCSAPIHKN-HKNJANUM(IDXH)
                   MOVE    TBL-PTHKN-KIGO (IDH)
                                           TO  ORCSAPIHKN-KIGO (IDXH)
                   MOVE    TBL-PTHKN-NUM  (IDH)
                                           TO  ORCSAPIHKN-NUM  (IDXH)
                   MOVE    TBL-PTHKN-HIHKNJANAME(IDH)
                                           TO  ORCSAPIHKN-HIHKNJANAME
                                                                (IDXH)
      *            ÊÝ¸±Í­¸ú´ü´Ö
                   MOVE    TBL-PTHKN-TEKSTYMD (IDH) TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-TEKSTYMD(IDXH)
                   MOVE    TBL-PTHKN-TEKEDYMD (IDH) TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-TEKEDYMD(IDXH)
      *
                   MOVE    TBL-PTHKN-TEKSTYMD (IDH)
                                           TO  ORCSAPIHKN-NAI-TEKSTYMD
                                                               (IDXH)
                   MOVE    TBL-PTHKN-TEKEDYMD (IDH)
                                           TO  ORCSAPIHKN-NAI-TEKEDYMD
                                                               (IDXH)
      *            »ñ³Ê¼èÆÀÆü
                   IF      TBL-PTHKN-SKKGETYMD (IDH) NOT =   SPACE
                       MOVE    TBL-PTHKN-SKKGETYMD (IDH) TO  WRK-SYMD
                       PERFORM 800-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-SKKGETYMD
                                                                (IDXH)
                   END-IF
                   MOVE    TBL-PTHKN-SKKGETYMD (IDH)
                                           TO  ORCSAPIHKN-NAI-SKKGETYMD
                                                                (IDXH)
      *            ³ÎÇ§ÆüÉÕ
                   IF      TBL-PTHKN-KAKUNINYMD(IDH) NOT =   SPACE
                       MOVE    TBL-PTHKN-KAKUNINYMD(IDH) TO  WRK-SYMD
                       PERFORM 800-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KAKUNINYMD
                                                                (IDXH)
                   END-IF
                   MOVE    TBL-PTHKN-KAKUNINYMD (IDH)
                                           TO  ORCSAPIHKN-NAI-KAKUNINYMD
                                                                (IDXH)
      *
                   MOVE    IDH-MAX         TO  IDH
               END-IF
           END-PERFORM
      *    ¸øÈñ¾ðÊó
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ     >   4    )
                         OR   (COMB-KOHID (IDZ)    =   ZERO )
               PERFORM 20012-PTKOHINF-HEN-SEC
           END-PERFORM
      *
      *    Ï«ºÒ¡¦¼«ÇåÀÕ¡¦Âè»°¼Ô¹Ô°ÙÊÔ½¸
           IF     (COMB-HKNNUM         =   "971"  OR  "973" )
              OR  (COMB-KOHHKNNUM (1)  =   "970"  )
               PERFORM 20013-PTRSIINF-HEN-SEC
           END-IF
           .
       2001-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êä½õ¶èÊ¬Ì¾¾ÎÊÔ½¸
      *****************************************************************
       20011-HOJOKBN-NAME-HEN-SEC     SECTION.
      *
           INITIALIZE                  ORAPI012SUB1AREA
           MOVE    COMB-HKNNUM         TO  API012SUB1-HKNNUM
           MOVE    COMB-HOJOKBN        TO  API012SUB1-HOJOKBN
           MOVE    SPA-SYSYMD          TO  API012SUB1-SYSYMD
           CALL    "ORAPI012SUB1"          USING
                                           ORAPI012SUB1AREA
                                           SPA-AREA
           MOVE    API012SUB1-HOJOKBN-NAME
                                       TO  ORCSAPIHKN-HOJOKBN-NAME(IDXH)
      *
           .
       20011-HOJOKBN-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       20012-PTKOHINF-HEN-SEC           SECTION.
      *
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   IDK-MAX )
      *        ¸øÈñ¾ðÊó
               IF      COMB-KOHID (IDZ)    =   TBL-PTKOH-KOHID (IDH)
      *            ¸øÈñ£É£Ä
                   MOVE    TBL-PTKOH-KOHID (IDH)
                                           TO  ORCSAPIHKN-NAI-KOHID
                                                            (IDXH IDZ)
      *            ¸øÈñ¤Î¼ïÎà
                   MOVE    TBL-PTKOH-KOHNUM (IDH)
                                           TO  ORCSAPIHKN-KOHNUM
                                                            (IDXH IDZ)
      *            ¸øÈñ¤ÎÌ¾¾Î
                   MOVE    COMB-KOH-TANSEIDONAME (IDZ)
                                           TO  ORCSAPIHKN-KOHNUM-NAME
                                                            (IDXH IDZ)
      *            ÉéÃ´¼ÔÈÖ¹æ
                   MOVE    TBL-PTKOH-FTNJANUM (IDH)
                                           TO  ORCSAPIHKN-FTNJANUM
                                                            (IDXH IDZ)
      *            ¼õµë¼ÔÈÖ¹æ
                   MOVE    TBL-PTKOH-JKYSNUM (IDH)
                                           TO  ORCSAPIHKN-JKYSNUM
                                                            (IDXH IDZ)
      *            Í­¸ú³«»ÏÆü
                   MOVE    TBL-PTKOH-TEKSTYMD (IDH)    TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KOH-TEKSTYMD
                                                            (IDXH IDZ)
      *            Í­¸ú½ªÎ»Æü
                   MOVE    TBL-PTKOH-TEKEDYMD (IDH)    TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KOH-TEKEDYMD
                                                            (IDXH IDZ)
      *
      *            ÉéÃ´Î¨¡¦ÉéÃ´³ÛÊÔ½¸
                   PERFORM 400-KOHFTN-HENSYU-SEC
      *
                   MOVE    IDK-MAX             TO  IDH
               END-IF
           END-PERFORM
           .
       20012-PTKOHINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    Ï«ºÒ¡¦¼«ÇåÀÕ¡¦Âè»°¼Ô¹Ô°ÙÊÔ½¸½èÍý
      *****************************************************************
       20013-PTRSIINF-HEN-SEC                SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-PTID            TO  PTRSI-PTID
           IF      COMB-HKNNUM         =   "971"  OR  "973"
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
           ELSE
               MOVE    COMB-KOHID (1)      TO  PTRSI-KOHID
           END-IF
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
      *
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
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTRSIINF        =   1
               GO      TO    20013-PTRSIINF-HEN-EXT
           END-IF
      *    Ï«ºÒÊÝ¸±Ì¾¾Î
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
                   MOVE    "Ã»´üµëÉÕ"      TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
               WHEN    "2"
                   MOVE    "½ýÉÂÇ¯¶â"      TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
               WHEN    "3"
                   MOVE    "¥¢¥Õ¥¿¡¼¥±¥¢"  TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
               WHEN    "4"
                   MOVE    "¼«ÇåÀÕÊÝ¸±"    TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
               WHEN    "5"
                   MOVE    "¸øÌ³ºÒ³²"      TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
               WHEN    "6"
                   MOVE    "Âè»°¼Ô¹Ô°Ù"    TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
           END-EVALUATE
      *    ½ýÉÂ¤ÎÉô°Ì
           MOVE    PTRSI-COMMENT       TO  ORCSAPIHKN-DISEASE-LOCATION
                                                                 (IDXH)
      *    ½ýÉÂÇ¯·îÆü
           IF      PTRSI-SHOBYOYMD NOT =   SPACE
               MOVE    PTRSI-SHOBYOYMD     TO  WRK-SYMD
               PERFORM 800-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-DISEASE-DATE(IDXH)
           END-IF
      *
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
      *    Ï«Æ¯ÊÝ¸±ÈÖ¹æ
                   MOVE    PTRSI-KOFUNUM
                                       TO  ORCSAPIHKN-PENSIONCERTIFICATE
                                                                (IDXH)
               WHEN    "2"
      *    Ç¯¶â¾Ú½ñÈÖ¹æ
                   MOVE    PTRSI-KOFUNUM
                                       TO  ORCSAPIHKN-PENSIONCERTIFICATE
                                                                (IDXH)
               WHEN    "3"
      *    ·ò¹¯´ÉÍý¼êÄ¢ÈÖ¹æ
                   MOVE    PTRSI-KOFUNUM
                                       TO  ORCSAPIHKN-PENSIONCERTIFICATE
                                                                (IDXH)
           END-EVALUATE
      *    ºÒ³²¶èÊ¬
           MOVE    PTRSI-SAIGAIKBN     TO  ORCSAPIHKN-ACCIDENT-CLASS
                                                               (IDXH)
      *    ºÒ³²¶èÊ¬¡ÊÌ¾¾Î¡Ë
           EVALUATE    PTRSI-SAIGAIKBN
               WHEN    "1"
                   MOVE    "¶ÈÌ³Ãæ¤ÎºÒ³²"
                                   TO  ORCSAPIHKN-ACCIDENT-CLASS-NAME
                                                               (IDXH)
               WHEN    "2"
                   MOVE    "ÄÌ¶ÐÅÓ¾å¤ÎºÒ³²"
                                   TO  ORCSAPIHKN-ACCIDENT-CLASS-NAME
                                                               (IDXH)
           END-EVALUATE
      *
      *    ¿··ÑºÆÊÌ¶èÊ¬
           MOVE    PTRSI-SINKEI    TO  ORCSAPIHKN-SINKEI (IDXH)
           EVALUATE    PTRSI-SINKEI
               WHEN    "1"
                   MOVE    "½é¿Ç"      TO  ORCSAPIHKN-SINKEI-NAME(IDXH)
               WHEN    "3"
                   MOVE    "Å¾°å»Ï¿Ç"  TO  ORCSAPIHKN-SINKEI-NAME(IDXH)
               WHEN    "5"
                   MOVE    "·ÑÂ³"      TO  ORCSAPIHKN-SINKEI-NAME(IDXH)
               WHEN    "7"
                   MOVE    "ºÆÈ¯"      TO  ORCSAPIHKN-SINKEI-NAME(IDXH)
           END-EVALUATE
      *    Å¾µ¢»öÍ³
           MOVE    PTRSI-TENKI         TO  ORCSAPIHKN-TENKI (IDXH)
           EVALUATE    PTRSI-TENKI
               WHEN    "1"
                   MOVE    "¼£Ìþ"      TO  ORCSAPIHKN-TENKI-NAME(IDXH)
               WHEN    "3"
                   MOVE    "·ÑÂ³"      TO  ORCSAPIHKN-TENKI-NAME(IDXH)
               WHEN    "5"
                   MOVE    "Å¾°å"      TO  ORCSAPIHKN-TENKI-NAME(IDXH)
               WHEN    "7"
                   MOVE    "Ãæ»ß"      TO  ORCSAPIHKN-TENKI-NAME(IDXH)
               WHEN    "9"
                   MOVE    "»àË´"      TO  ORCSAPIHKN-TENKI-NAME(IDXH)
           END-EVALUATE
      *    »Í»èÆÃÎã¶èÊ¬
           MOVE    PTRSI-SISIKBN       TO  ORCSAPIHKN-SISIKBN (IDXH)
           EVALUATE    PTRSI-SISIKBN
               WHEN    "0"
                   MOVE    "¤Ê¤·"      TO  ORCSAPIHKN-SISIKBN-NAME(IDXH)
               WHEN    "1"
                   MOVE    "»Í»è"      TO  ORCSAPIHKN-SISIKBN-NAME(IDXH)
               WHEN    "2"
                   MOVE    "¼ê»Ø"      TO  ORCSAPIHKN-SISIKBN-NAME(IDXH)
           END-EVALUATE
      *    Ï«Æ¯´ð½à´ÆÆÄ½ð¥³¡¼¥É
           MOVE    PTRSI-SYOCD         TO  ORCSAPIHKN-LABOR-ST-CODE
                                                               (IDXH)
           IF      PTRSI-SYOCD     NOT =   SPACE
               PERFORM 200131-SYOCD-HEN-SEC
           END-IF
      *
      *    »ö¶È½êÌ¾¾Î
           MOVE    PTRSI-JIGYOUNAME    TO  ORCSAPIHKN-RSI-JIGYOUNAME
                                                                (IDXH)
      *    ½êºßÃÏÅÔÆ»ÉÜ¸©Ì¾
           MOVE    PTRSI-PREFNAME      TO  ORCSAPIHKN-RSI-PREFNAME
                                                               (IDXH)
      *    ½êºßÃÏÅÔÆ»ÉÜ¸©¶èÊ¬
           MOVE    PTRSI-PREFKBN       TO  ORCSAPIHKN-RSI-PREFKBN(IDXH)
           EVALUATE    PTRSI-PREFKBN
               WHEN    "1"
                   MOVE    "ÅÔ"        TO  ORCSAPIHKN-RSI-PREFKBNNAME
                                                               (IDXH)
               WHEN    "2"
                   MOVE    "Æ»"        TO  ORCSAPIHKN-RSI-PREFKBNNAME
                                                               (IDXH)
               WHEN    "3"
                   MOVE    "ÉÜ"        TO  ORCSAPIHKN-RSI-PREFKBNNAME
                                                               (IDXH)
               WHEN    "4"
                   MOVE    "¸©"        TO  ORCSAPIHKN-RSI-PREFKBNNAME
                                                               (IDXH)
           END-EVALUATE
      *    ½êºßÃÏ·´»Ô¶èÌ¾
           MOVE    PTRSI-CITYNAME      TO  ORCSAPIHKN-RSI-CITYNAME(IDXH)
      *    ½êºßÃÏ·´»Ô¶è¶èÊ¬
           MOVE    PTRSI-CITYKBN       TO  ORCSAPIHKN-RSI-CITYKBN(IDXH)
           EVALUATE    PTRSI-CITYKBN
               WHEN    "1"
                   MOVE    "·´"        TO  ORCSAPIHKN-RSI-CITYKBNNAME
                                                               (IDXH)
               WHEN    "2"
                   MOVE    "»Ô"        TO  ORCSAPIHKN-RSI-CITYKBNNAME
                                                               (IDXH)
               WHEN    "3"
                   MOVE    "¶è"        TO  ORCSAPIHKN-RSI-CITYKBNNAME
                                                               (IDXH)
           END-EVALUATE
      *    Ï«ºÒ¥ì¥»²ó¿ôµ­ºÜ ´ð½àÇ¯·î
           IF      PTRSI-KIJYUNYM  NOT =   SPACE
               MOVE    PTRSI-KIJYUNYM      TO  WRK-SYMD(1:6)
               PERFORM 800-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE(1:7)   TO  ORCSAPIHKN-KIJYUNYM
                                                               (IDXH)
           END-IF
      *    Ï«ºÒ¥ì¥»²ó¿ôµ­ºÜ ²ó¿ô
           IF      PTRSI-KAISU     NOT =   ZERO
               MOVE    PTRSI-KAISU     TO  ORCSAPIHKN-RESEPUTO-COUNT
                                                               (IDXH)
           END-IF
      *¼«ÇåÀÕ
      *    ¼«ÇåÀÕÊÝ¸±²ñ¼ÒÌ¾
           MOVE    PTRSI-HOKENNAME     TO  ORCSAPIHKN-RSI-HOKENNAME
                                                               (IDXH)
      *    ¼«ÇåÀÕ´µ¼ÔÀÁµá¶èÊ¬
           MOVE    PTRSI-JIBAISEIKBN   TO  ORCSAPIHKN-JIBAISEIKBN(IDXH)
           EVALUATE    PTRSI-JIBAISEIKBN
               WHEN    "1"
                   MOVE    "´µ¼ÔÀÁµá¤¹¤ë(100%)"
                                       TO  ORCSAPIHKN-JIBAISEIKBN-NAME
                                                            (IDXH)
               WHEN    "2"
                   MOVE    "´µ¼ÔÀÁµá¤·¤Ê¤¤(0%)"
                                       TO  ORCSAPIHKN-JIBAISEIKBN-NAME
                                                            (IDXH)
           END-EVALUATE
      *    ¼«ÇåÀÕÃ´Åö°å¥³¡¼¥É
           IF      PTRSI-DRCD      NOT =   SPACE
               MOVE    PTRSI-DRCD      TO  ORCSAPIHKN-RSI-DRCD (IDXH)
      *
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    "1010"              TO  SYS-KANRICD
               MOVE    PTRSI-RYOSTYMD      TO  SYS-STYUKYMD
               MOVE    PTRSI-RYOSTYMD      TO  SYS-EDYUKYMD
               MOVE    PTRSI-DRCD          TO  SYS-KBNCD
               PERFORM 900-SYSKANRI-KEYREAD-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC    TO  SYS-1010-REC
                   MOVE    SYS-1010-NAME   TO  ORCSAPIHKN-RSI-DRCD-NAME
                                                                (IDXH)
               END-IF
           END-IF
      *
      *¥¢¥Õ¥¿¡¼¥±¥¢
           EVALUATE    PTRSI-HKNKBN
           WHEN    "3"
      *        Â»½ý¶èÊ¬
               STRING  PTRSI-SONSHOUKBN
                       PTRSI-SONSHOUKBN-EDANUM
                                           DELIMITED  BY  SIZE
                                       INTO   ORCSAPIHKN-DAMAGE-CLASS
                                                                (IDXH)
               END-STRING
      *        Â»½ý¶èÊ¬Ì¾¾Î¼èÆÀ
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    "4002"              TO  SYS-KANRICD
               MOVE    PTRSI-RYOSTYMD      TO  SYS-STYUKYMD
               MOVE    PTRSI-RYOSTYMD      TO  SYS-EDYUKYMD
               MOVE    ORCSAPIHKN-DAMAGE-CLASS (IDXH)
                                           TO  SYS-KBNCD
               PERFORM 900-SYSKANRI-KEYREAD-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-4002-REC
                   MOVE    SYS-4002-SONSHOUNAME
                                       TO  ORCSAPIHKN-DAMAGE-CLASS-NAME
                                                                (IDXH)
               END-IF
      *    Âè»°¼Ô¹Ô°Ù
           WHEN    "6"
      *        ¸½Êª»Ùµë¶èÊ¬
               MOVE    PTRSI-DAISAN-GENBTKBN
                                       TO  ORCSAPIHKN-DS-GENBTKBN(IDXH)
               EVALUATE    PTRSI-DAISAN-GENBTKBN
               WHEN    "1"
                   MOVE    "ÂÐ¾Ý³°"
                                   TO  ORCSAPIHKN-DS-GENBTKBN-NAME(IDXH)
               WHEN    "2"
                   MOVE    "ÂÐ¾Ý"
                                   TO  ORCSAPIHKN-DS-GENBTKBN-NAME(IDXH)
               END-EVALUATE
      *        ÆÃµ­»ö¹à¶èÊ¬
               MOVE    PTRSI-DAISAN-TOKKIKBN
                                       TO  ORCSAPIHKN-DS-TOKKIKBN(IDXH)
               EVALUATE    PTRSI-DAISAN-TOKKIKBN
               WHEN    "1"
                   MOVE    "¡Ö£±£°Âè»°¡×µ­ºÜÍ­"
                                   TO  ORCSAPIHKN-DS-TOKKIKBN-NAME(IDXH)
               WHEN    "2"
                   MOVE    "¡Ö£±£°Âè»°¡×µ­ºÜÌµ"
                                   TO  ORCSAPIHKN-DS-TOKKIKBN-NAME(IDXH)
               END-EVALUATE
           END-EVALUATE
           .
       20013-PTRSIINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï«Æ¯´ð½à´ÆÆÄ½ðÌ¾¾ÎÊÔ½¸½èÍý
      *****************************************************************
       200131-SYOCD-HEN-SEC                SECTION.
      *
           INITIALIZE                      LABOR-SIO-REC
           MOVE    PTRSI-SYOCD         TO  LABOR-SIO-SYOCD
           MOVE    PTRSI-RYOSTYMD      TO  LABOR-SIO-YUKOSTYMD
           MOVE    PTRSI-RYOSTYMD      TO  LABOR-SIO-YUKOEDYMD
      *
           MOVE    LABOR-SIO-REC       TO  MCPDATA-REC
           MOVE    "tbl_labor_sio"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_labor_sio"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  LABOR-SIO-REC
                   MOVE    ZERO                TO  FLG-LABOR-SIO
               ELSE
                   MOVE    1                   TO  FLG-LABOR-SIO
                   INITIALIZE                  LABOR-SIO-REC
               END-IF
           ELSE
               INITIALIZE                  LABOR-SIO-REC
               MOVE    1               TO  FLG-LABOR-SIO
           END-IF
           MOVE    "tbl_labor_sio"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-LABOR-SIO       =   ZERO
               MOVE    LABOR-SIO-NAME      TO  ORCSAPIHKN-LABOR-ST-NAME
                                                             (IDXH)
           END-IF
           .
       200131-SYOCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»ÉéÃ´³ä¹çÊÔ½¸½èÍý
      *****************************************************************
       20016-AGEFTNRATE-SEC            SECTION.
      *
      *    Æþ±¡
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    ORCSAPIHKN-SYSYMD   TO  AGECHK-SRYYMD
           MOVE    ORCSAPIHKN-BIRTHDAY TO  AGECHK-BIRTHDAY
           MOVE    "1"                 TO  AGECHK-NYUGAIKBN
           MOVE    COMB-HKNCOMBINUM
                                       TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-FTNRATE      TO  WRK-HZN-RATE
      *    ÉéÃ´³ä¹ç·×»»
           PERFORM 4001-FTNRATE-HEN-SEC
           MOVE    WRK-FTNRATEMEI      TO  ORCSAPIHKN-COMB-RATEN(IDXH)
      *
      *    ³°Íè
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    ORCSAPIHKN-SYSYMD   TO  AGECHK-SRYYMD
           MOVE    ORCSAPIHKN-BIRTHDAY TO  AGECHK-BIRTHDAY
           MOVE    "2"                 TO  AGECHK-NYUGAIKBN
           MOVE    COMB-HKNCOMBINUM
                                       TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-FTNRATE      TO  WRK-HZN-RATE
           PERFORM 4001-FTNRATE-HEN-SEC
           MOVE    WRK-FTNRATEMEI      TO  ORCSAPIHKN-COMB-RATEG(IDXH)
      *
           .
       20016-AGEFTNRATE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ÉéÃ´Î¨ÊÔ½¸½èÍý
      *****************************************************************
       4001-FTNRATE-HEN-SEC        SECTION.
      *
           COMPUTE WRK-FTNRATE         =   WRK-HZN-RATE
                                       /   100
           MOVE    WRK-FTNRATE         TO  WRK-Z9
           MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
      *
           .
       4001-FTNRATE-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ÉéÃ´Î¨¡¦ÉéÃ´³ÛÊÔ½¸½èÍý
      *****************************************************************
       400-KOHFTN-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
      *    ÊÝ¸±ÈÖ¹æ¡Ê¸øÈñ¡Ë
           MOVE    TBL-PTKOH-KOHNUM (IDH)  TO  HKN-HKNNUM
      *    »ÙÊ§¶èÊ¬
           MOVE    TBL-PTKOH-PAYKBN (IDH)  TO  HKN-PAYKBN
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-HKNNUM-READ-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ËÜ¿Í¡Ý³°Íè¡Ý²ó¡ÝÉéÃ´³ä¹ç
           MOVE    HKN-HON-GAIKFTNRATE TO  WRK-HZN-RATE
           PERFORM 4001-FTNRATE-HEN-SEC
           MOVE    WRK-FTNRATEMEI      TO  ORCSAPIHKN-RATE-OUTPATIENT
                                                            (IDXH IDZ)
      *    ³°Íè¾å¸Â³Û
           IF      HKN-HON-GAIKKOTGAK  =   ZERO
      *        ËÜ¿Í¡Ý³°Íè¡ÝÆü¡Ý¾å¸Â³Û
               MOVE    HKN-HON-GAIDJGNGAK  TO  WRK-ZZ9
           ELSE
      *        ËÜ¿Í¡Ý³°Íè¡Ý²ó¡Ý¾å¸Â³Û
               MOVE    HKN-HON-GAIKKOTGAK  TO  WRK-ZZ9
           END-IF
           MOVE    WRK-ZZ9             TO  ORCSAPIHKN-MONEY-OUTPATIENT
                                                            (IDXH IDZ)
      *
      *    ËÜ¿Í¡ÝÆþ±¡¡Ý²ó¡ÝÉéÃ´³ä¹ç
           MOVE    HKN-HON-NYUKFTNRATE TO  WRK-HZN-RATE
           PERFORM 4001-FTNRATE-HEN-SEC
           MOVE    WRK-FTNRATEMEI      TO  ORCSAPIHKN-RATE-ADMISSION
                                                            (IDXH IDZ)
      *    ³°Íè¾å¸Â³Û
           IF      HKN-HON-NYUKKOTGAK  =   ZERO
      *        ËÜ¿Í¡ÝÆþ±¡¡ÝÆü¡Ý¾å¸Â³Û
               MOVE    HKN-HON-NYUDJGNGAK  TO  WRK-ZZ9
           ELSE
      *        ËÜ¿Í¡ÝÆþ±¡¡Ý²ó¡Ý¾å¸Â³Û
               MOVE    HKN-HON-NYUKKOTGAK  TO  WRK-ZZ9
           END-IF
           MOVE    WRK-ZZ9             TO  ORCSAPIHKN-MONEY-ADMISSION
                                                            (IDXH IDZ)
           .
       400-KOHFTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆüÉÕÊÔ½¸½èÍý
      *****************************************************************
       800-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    WRK-SYY             TO  WRK-HEN-YY
           MOVE    WRK-SMM             TO  WRK-HEN-MM
           MOVE    WRK-SDD             TO  WRK-HEN-DD
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   "99999999"
               MOVE    "12"                TO  WRK-HEN-MM
               MOVE    "31"                TO  WRK-HEN-DD
           END-IF
           .
       800-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¼ç¥­¡¼ÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       900-SYSKANRI-KEYREAD-SEC        SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *
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
      ****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¼¡¸¡º÷½èÍý
      ****************************************************************
       980-HKNNUM-READ-SEC              SECTION.
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
       980-HKNNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¥Þ¥¹¥¿¡¼ÆÉ¤ß¹þ¤ß
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
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
       920-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñÆÉ¤ß¹þ¤ß
      *****************************************************************
       920-PTKOHINF-READ-SEC         SECTION.
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
      *
       920-DBFETCH-EXT.
           EXIT.
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
      *****************************************************************
