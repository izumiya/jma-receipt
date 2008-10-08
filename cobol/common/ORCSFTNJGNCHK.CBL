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
       PROGRAM-ID.             ORCSFTNJGNCHK.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ´µ¼Ô¼«¸ÊÉéÃ´¶â¥ì¥»¥×¥Èµ­ºÜÈ½Äê
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    04/11/15  ¸øÈñ·î¾å¸Â³Û¼èÆÀ½¤Àµ
      *  01.00.02    NACL-ÌçÏÆ    04/11/30  Æþ±¡ÂÐ±þ
      *  01.00.03    NACL-ÌçÏÆ    05/06/01  Ä¹´ü¹â³ÛÎÅÍÜ¥ì¥»µ­ºÜÈ½ÄêÄÉ²Ã
      *  01.00.04    NACL-ÌçÏÆ    05/08/12  ¼¢²ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.00.05    NACL-ÌçÏÆ    05/10/04  Åçº¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  03.02.01    NACL-ÌçÏÆ    06/09/05  Ä¹´ü£¹£·£´ÂÐ±þ
      *  03.03.01    NACL-ÌçÏÆ    06/11/02  ¸º³Û¡¦ÌÈ½ü¡¦»ÙÊ§Í±Í½ÂÐ±þ
      *  03.04.01    NACL-ÌçÏÆ    07/02/14  Ê¡Åç¸©ÈÓ´ÛÂ¼ÂÐ±þ
      *  03.04.02    NACL-ÌçÏÆ    07/03/22  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *                                     ¡ÊÀî¸ý¡¦½ÕÆüÉô»Ô¡Ë
      *  03.04.03    NACL-ÌçÏÆ    07/06/04  Ä¹ºê»ÔÃÏÊý¸øÈñÂÐ±þ
      *
      *  03.05.01    NACL-Æ£¸¶    07/04/19  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.02    NACL-ÌçÏÆ    07/08/01  ½êÆÀ¾ðÊó¼è°·¤¤µ¡Ç½¶¯²½ÂÐ±þ
      *  03.05.03    NACL-ÌçÏÆ    07/08/01  ·îÅÓÃæ¤Î½êÆÀ¾ðÊóÊÑ¹¹ÂÐ±þ
      *  04.01.01    NACL-ÌçÏÆ    08/01/18  ºë¶Ì¸©ÀîÅçÄ®ÃÏÊý¸øÈñÂÐ±þ
      *  04.01.02    NACL-ÌçÏÆ    08/01/31  ¼¢²ì¸©£´£´£±ÄÉ²Ã¤ËÈ¼¤¦½¤Àµ
      *  04.02.01    NACL-ÌçÏÆ    08/03/19  À¸ÊÝ¤Î£·£¹Ê»ÍÑ»þ¤ÎÂÐ±þ
      *  04.02.02    NACL-ÌçÏÆ    08/04/24  ºë¶Ì¸©»ÖÌÚ»ÔÃÏÊý¸øÈñÂÐ±þ
      *  04.02.03    NACL-ÌçÏÆ    08/07/02  ÂçºåÉÜ£´£±Ï·¿Í¡Ü£±£¸£¸ÅùÂÐ±þ
      *  04.02.04    NACL-ÌçÏÆ    08/08/07  Àº¿ÀÄÌ±¡¤ÎÀÁµáÅÀÈ½ÄêÄÉ²Ã
      *  04.02.05    NACL-ÌçÏÆ    08/09/04  Ê¡²¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.02.06    NACL-ÌçÏÆ    08/10/02  ËÌ³¤Æ»ÆÃÄê¼À´µÂÐ±þ
      *  04.02.07    NACL-ÌçÏÆ    08/10/08  ÀÁµáÅÀÈ½ÄêÄÉ²Ã
      *****************************************************************
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-CHECK           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-GENMEN          PIC 9(01).
           03  FLG-DAYCHK          PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  SYOTOKU-IDX         PIC 9(02).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-HKNMST-JGNGAK  PIC 9(06).
           03  WRK-HKNMST-JGNGAK2 PIC 9(06).
           03  WRK-KOHFTN         PIC 9(10).
           03  WRK-SEIKYU         PIC 9(10).
           03  WRK-SKJ-FTN        PIC 9(10).
           03  WRK-PREFNUM        PIC 9(02).
      *
           03  WRK-TEISYOTOKU     PIC X(01).
           03  WRK-RECEJGNDSPKBN  PIC X(01).
      *
           03  WRK-HKNCOMBINUM-TBL.
               05  WRK-HKNCOMBINUM-OCC      OCCURS 50.
                 07  WRK-HKNCOMBINUM        PIC 9(04).
                 07  WRK-HKNCOMBINUM-FLG    PIC 9(01).
      *
      *    ¶¦ÄÌÎÎ°è
           COPY    "MCPAREA".
      *
      *    ´µ¼Ô¾ðÊó
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÊÝ¸±ÈÖ¹æ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ¼ýÇ¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSK1001.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ¸øÈñ·î¾å¸Â³Û¼èÆÀ¥µ¥Ö
           COPY    "CPORCSJGNGET.INC".
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    ´µ¼Ô½êÆÀ¾ðÊóÊÔ½¸¥µ¥Ö
           COPY    "CPORCSSYOTOKU.INC".
      *
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSFTNJGNCHK.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSFTNJGNCHKAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-FTNJGNCHK-RC
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM   100-PRM-CHK-SEC
           IF       (LNK-FTNJGNCHK-RC   NOT =   ZERO)
               GO   TO   000-PROC-EXT
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC      =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                      PTINF-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    10              TO  LNK-FTNJGNCHK-RC
           END-IF
      *
           IF    LNK-FTNJGNCHK-KOHNUM   = SPACE
               MOVE    10              TO  LNK-FTNJGNCHK-RC
           END-IF
      *
           IF    LNK-FTNJGNCHK-KOHID    =  ZERO
               MOVE    10              TO  LNK-FTNJGNCHK-RC
           END-IF
      *
      *    ¸©ÈÖ¹æ¼èÆÀ
      ***  INITIALIZE                      SYS-1001-REC
      ***  MOVE    "1001"              TO  SYS-1001-KANRICD
      ***  MOVE    "*"                 TO  SYS-1001-KBNCD
      ***  MOVE    LNK-FTNJGNCHK-HOSPNUM
      ***                              TO  SYS-1001-HOSPNUM
      ***  MOVE    LNK-FTNJGNCHK-SRYYM TO  SYS-1001-STYUKYMD(1:6)
      ***  MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
      ***  MOVE    LNK-FTNJGNCHK-SRYYM TO  SYS-1001-EDYUKYMD(1:6)
      ***  MOVE    "01"                TO  SYS-1001-EDYUKYMD(7:2)
      ***  MOVE    SYS-1001-REC        TO  MCPDATA-REC
      ***  MOVE    "DBSELECT"          TO  MCP-FUNC
      ***  MOVE    "tbl_syskanri"      TO  MCP-TABLE
      ***  MOVE    "key10"             TO  MCP-PATHNAME
      ***  CALL    "ORCDBMAIN"         USING   MCPAREA
      ***                                      MCPDATA-REC
      ***                                      SPA-AREA
      ***  IF      MCP-RC               =  ZERO
      ***      MOVE     "tbl_syskanri" TO  MCP-TABLE
      ***      MOVE     "key10"        TO  MCP-PATHNAME
      ***      PERFORM   SYSKANRI-READ-SEC
      ***      MOVE      SYS-1001-PREFNUM
      ***                              TO  WRK-PREFNUM
      ***  END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
      ***  MOVE     "tbl_syskanri"     TO    MCP-TABLE
      ***  MOVE     "key10"            TO    MCP-PATHNAME
      ***  PERFORM   910-DB-CLOSE-SEC
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¤«¤é¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬¤ÈÉéÃ´¾å¸Â³Û¤Î¼èÆÀ
           PERFORM    210-HKNNUM-GET-SEC
      *
           IF   FLG-HKNNUM     =    ZERO
             IF   (HKN-RECEJGNDSPKBN    =   "1"           )
             OR   (HKN-HKNNUM           =   "972" OR "974")
             OR   (WRK-RECEJGNDSPKBN    =   "1"           )
      *        ÉéÃ´¾å¸Â³Û¤Î¼èÆÀ
               PERFORM    210-PTKOHFTN-GET-SEC
             END-IF
      *      ÊÝ¸±¤Î¾å¸Â³Û¤È¼ýÇ¼¤ÎÉéÃ´³Û¤òÈæ³ÓÈ½Äê
             PERFORM      210-HANTEI-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¤«¤é¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬¤ÈÉéÃ´¾å¸Â³Û¤Î¼èÆÀ
      *****************************************************************
       210-HKNNUM-GET-SEC       SECTION.
      *
           MOVE        ZERO      TO     FLG-CHECK
           MOVE        ZERO      TO     FLG-OK
           MOVE        SPACE     TO     WRK-TEISYOTOKU
           MOVE        SPACE     TO     WRK-RECEJGNDSPKBN
      *
           IF  (LNK-FTNJGNCHK-PREFNUM =  32            ) AND
               (LNK-FTNJGNCHK-KOHNUM  = "191" OR "291"
                                     OR "190" OR "290" ) AND
               (LNK-FTNJGNCHK-SRYYM  >= "200510"       )
      *
               IF   LNK-FTNJGNCHK-KOHNUM  =  "191"
                     PERFORM    2101-SYOTOKU-HENSYU-SEC
               END-IF
               MOVE     "1"      TO     WRK-RECEJGNDSPKBN
      *
           END-IF
      *
           IF  (LNK-FTNJGNCHK-PREFNUM =  11            ) AND
               (LNK-FTNJGNCHK-KOHNUM  = "483"          ) AND
               (LNK-FTNJGNCHK-SRYYM  >= "200804"       )
      *
               PERFORM    2101-SYOTOKU-HENSYU-SEC
      *
           END-IF
      *
           IF  (LNK-FTNJGNCHK-PREFNUM =  27            ) AND
               (LNK-FTNJGNCHK-KOHNUM  = "141" OR "241" ) AND
               (LNK-FTNJGNCHK-SRYYM  >= "200804"       )
      *
               MOVE     "1"      TO     WRK-RECEJGNDSPKBN
      *
           END-IF
      *
           INITIALIZE                           HKNNUM-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM  TO    HKN-HOSPNUM
           MOVE    LNK-FTNJGNCHK-KOHNUM   TO    HKN-HKNNUM
           MOVE   "tbl_hknnum"            TO    MCP-TABLE
           MOVE   "key2"                  TO    MCP-PATHNAME
           PERFORM  910-HKNNUM-SELECT-SEC
           IF      FLG-HKNNUM           =       ZERO
               MOVE    "tbl_hknnum"       TO    MCP-TABLE
               MOVE    "key2"             TO    MCP-PATHNAME
               PERFORM  910-HKNNUM-READ-SEC
               PERFORM  UNTIL
                   ( FLG-HKNNUM          =   1   )   OR
                   ((HKN-TEKSTYMD(1:6)
                                  <=   LNK-FTNJGNCHK-SRYYM) AND
                    (LNK-FTNJGNCHK-SRYYM
                                  <=   HKN-TEKEDYMD(1:6)))
                      MOVE   "tbl_hknnum" TO    MCP-TABLE
                      MOVE   "key2"       TO    MCP-PATHNAME
                      PERFORM 910-HKNNUM-READ-SEC
               END-PERFORM
           END-IF
      *    ÊÝ¸±ÈÖ¹æ¥¯¥í¡¼¥º
           MOVE     "tbl_hknnum" TO    MCP-TABLE
           MOVE     "key2"       TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       210-HKNNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Äã½êÆÀ¡¦ÄãÇ¯¶â¾ðÊóÊÔ½¸
      *****************************************************************
       2101-SYOTOKU-HENSYU-SEC       SECTION.
      *
           MOVE       1       TO      SYOTOKU-IDX
      *
           IF   LNK-FTNJGNCHK-SRYYM   >=  "200604"
      *
              INITIALIZE                         PTKOHINF-REC
              MOVE    LNK-FTNJGNCHK-HOSPNUM  TO  PTKOH-HOSPNUM
              MOVE    LNK-FTNJGNCHK-PTID     TO  PTKOH-PTID
              MOVE    LNK-FTNJGNCHK-KOHID    TO  PTKOH-KOHID
              MOVE    PTKOHINF-REC           TO  MCPDATA-REC
              MOVE    "DBSELECT"             TO  MCP-FUNC
              MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
              MOVE    "key8"                 TO  MCP-PATHNAME
              CALL    "ORCDBMAIN"            USING  MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
              IF      MCP-RC   =    ZERO
                MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                MOVE    "key8"           TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
                IF       FLG-PTKOHINF    =   ZERO
                  IF   PTKOH-TEKSTYMD(1:6)  >=  LNK-FTNJGNCHK-SRYYM
                      MOVE   PTKOH-TEKSTYMD(7:2)  TO   SYOTOKU-IDX
                  END-IF
                END-IF
              END-IF
              MOVE     "tbl_ptkohinf"    TO   MCP-TABLE
              MOVE     "key8"            TO   MCP-PATHNAME
              PERFORM   910-DB-CLOSE-SEC
      *
           END-IF
      *
           INITIALIZE                       ORCSSYOTOKUAREA
           MOVE  LNK-FTNJGNCHK-HOSPNUM  TO  LNK-SYOTOKU-HOSPNUM
           MOVE  LNK-FTNJGNCHK-PTID     TO  LNK-SYOTOKU-PTID
           MOVE  LNK-FTNJGNCHK-SRYYM    TO  LNK-SYOTOKU-SRYYM
      *
           CALL    "ORCSSYOTOKU"   USING  ORCSSYOTOKUAREA
                                          SPA-AREA
      *
           IF    LNK-SYOTOKU-RC    =   ZERO
                 MOVE   LNK-SYOTOKU-CHIKBN(SYOTOKU-IDX)
                                        TO  WRK-TEISYOTOKU
           END-IF
      *
           .
       2101-SYOTOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñÉéÃ´¥Þ¥¹¥¿¤«¤éÉéÃ´¾å¸Â³Û¤Î¼èÆÀ
      *****************************************************************
       210-PTKOHFTN-GET-SEC       SECTION.
      *
           IF    LNK-FTNJGNCHK-NYUGAIKBN     =   "1"
               EVALUATE   WRK-TEISYOTOKU
                 WHEN     SPACE
                   MOVE   HKN-HON-NYUMJGNGAK     TO  WRK-HKNMST-JGNGAK
                   IF  (LNK-FTNJGNCHK-PREFNUM =  25            ) AND
                       (LNK-FTNJGNCHK-KOHNUM  = "140" OR "241" OR
                                                "243" OR "244" OR
                                                "245" OR "441" ) AND
                       (LNK-FTNJGNCHK-SRYYM  >= "200508"       )
                      MOVE   HKN-HON-NYUDJGNGAK  TO  WRK-HKNMST-JGNGAK
                   END-IF
                 WHEN     "1"
                 WHEN     "4"
                   MOVE   HKN-TNK-NYUMJGNGAK     TO  WRK-HKNMST-JGNGAK
                 WHEN     "2"
                 WHEN     "3"
                   MOVE   HKN-TSY-NYUMJGNGAK     TO  WRK-HKNMST-JGNGAK
               END-EVALUATE
           ELSE
               EVALUATE   WRK-TEISYOTOKU
                 WHEN     SPACE
                   IF    HKN-HON-GAIMNAIJGNGAK   NOT =    ZERO
                      MOVE HKN-HON-GAIMNAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
                   IF    HKN-HON-GAIMGAIJGNGAK   NOT =    ZERO
                      MOVE HKN-HON-GAIMGAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
                   IF  (LNK-FTNJGNCHK-PREFNUM    =  42     )
                   AND (LNK-FTNJGNCHK-KOHNUM     = "180"   )
                   AND (LNK-FTNJGNCHK-SRYYM     >= "200704")
                      MOVE HKN-HON-GAIDJGNGAK    TO WRK-HKNMST-JGNGAK2
                   END-IF
                 WHEN     "1"
                 WHEN     "4"
                   IF    HKN-TNK-GAIMNAIJGNGAK   NOT =    ZERO
                      MOVE HKN-TNK-GAIMNAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
                   IF    HKN-TNK-GAIMGAIJGNGAK   NOT =    ZERO
                      MOVE HKN-TNK-GAIMGAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
                 WHEN     "2"
                   IF    HKN-TSY-GAIMNAIJGNGAK   NOT =    ZERO
                      MOVE HKN-TSY-GAIMNAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
                   IF    HKN-TSY-GAIMGAIJGNGAK   NOT =    ZERO
                      MOVE HKN-TSY-GAIMGAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
               END-EVALUATE
           END-IF
           IF   (LNK-FTNJGNCHK-PREFNUM   =  11                     )
           AND  (LNK-FTNJGNCHK-KOHNUM    = "481" OR "482" OR "483" )
           AND  (LNK-FTNJGNCHK-NYUGAIKBN = "1"                     )
           AND  (LNK-FTNJGNCHK-SRYYM    >= "200804"                )
                      MOVE     999999            TO WRK-HKNMST-JGNGAK
           END-IF
      *
      *****
      *
           IF ((LNK-FTNJGNCHK-PREFNUM        =  13            )  AND
               (LNK-FTNJGNCHK-KOHNUM         = "151" OR "451" )  AND
               (LNK-FTNJGNCHK-SRYYM         >= "200310"))
      *
           OR ((LNK-FTNJGNCHK-PREFNUM        =  28     )  AND
               (LNK-FTNJGNCHK-KOHNUM         = "186"   )  AND
               (LNK-FTNJGNCHK-SRYYM         >= "200310"))
      *
           OR ((LNK-FTNJGNCHK-PREFNUM        =  07     )  AND
               (LNK-FTNJGNCHK-KOHNUM         = "281"   )  AND
               (LNK-FTNJGNCHK-SRYYM         >= "200702"))
      *
           OR ((LNK-FTNJGNCHK-PREFNUM        =  01     )  AND
               (LNK-FTNJGNCHK-KOHNUM         = "183"   )  AND
               (LNK-FTNJGNCHK-SRYYM         >= "200810"))
      *
      *         ¸øÈñ·î¾å¸Â³Û¼èÆÀ
                INITIALIZE                    LNK-ORCSJGNGET-REC
                MOVE  LNK-FTNJGNCHK-HOSPNUM
                                          TO  LNK-ORCSJGNGET-HOSPNUM
                MOVE  LNK-FTNJGNCHK-NYUGAIKBN
                                          TO  LNK-ORCSJGNGET-NYUGAIKBN
                MOVE  LNK-FTNJGNCHK-PTID  TO  LNK-ORCSJGNGET-PTID
                MOVE  LNK-FTNJGNCHK-KOHNUM
                                          TO  LNK-ORCSJGNGET-KOHHKNNUM
                MOVE  LNK-FTNJGNCHK-KOHID TO  LNK-ORCSJGNGET-KOHID
                MOVE  LNK-FTNJGNCHK-SRYYM TO  LNK-ORCSJGNGET-SRYYM
      *
                CALL    "ORCSJGNGET"      USING
                                          LNK-ORCSJGNGET-REC
                                          SPA-AREA
      *
                IF    LNK-ORCSJGNGET-RC   =   ZERO
                 IF    LNK-FTNJGNCHK-NYUGAIKBN     =   "1"
                   MOVE    LNK-ORCSJGNGET-NYUJGNGAK
                                          TO  WRK-HKNMST-JGNGAK
                 ELSE
                   MOVE    LNK-ORCSJGNGET-GAIJGNGAK
                                          TO  WRK-HKNMST-JGNGAK
                 END-IF
                END-IF
      *
           END-IF
           .
       210-PTKOHFTN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±¤Î¾å¸Â³Û¤È¼ýÇ¼¤ÎÉéÃ´³Û¤òÈæ³ÓÈ½Äê
      *****************************************************************
       210-HANTEI-SEC            SECTION.
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ßÂÐ¾Ý¤ÎÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ¼èÆÀ
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM   TO  COMB-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID      TO  COMB-PTID
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(1)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(2)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(3)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(4)
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            NOT =   ZERO
               MOVE    01                  TO  LNK-FTNJGNCHK-RC
               GO  TO  210-HANTEI-EXT
           ELSE
               MOVE   "tbl_hkncombi"       TO  MCP-TABLE
               MOVE   "key4"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
      *
               MOVE      ZERO      TO      IDX
               PERFORM
                 UNTIL   (IDX              >=    50 )
                   OR    (FLG-HKNCOMBI  NOT =   ZERO)
      *
                 IF    COMB-DELKBN          =   SPACE
                 AND   COMB-TEKSTYMD(1:6)  <=   LNK-FTNJGNCHK-SRYYM
                 AND   LNK-FTNJGNCHK-SRYYM <=   COMB-TEKEDYMD(1:6)
      *
                   ADD     1       TO      IDX
                   MOVE    COMB-HKNCOMBINUM  TO
                                        WRK-HKNCOMBINUM(IDX)
      *
                   IF    WRK-RECEJGNDSPKBN      =   "1"
                   AND   LNK-FTNJGNCHK-PREFNUM  =   27
      *
                     IF ((COMB-KOH1HKNNUM = LNK-FTNJGNCHK-KOHNUM) AND
                         (COMB-KOH2HKNNUM = "187" OR "188"
                                         OR "189" OR "190"      ))
                     OR ((COMB-KOH2HKNNUM = LNK-FTNJGNCHK-KOHNUM) AND
                         (COMB-KOH3HKNNUM = "187" OR "188"
                                         OR "189" OR "190"      ))
                     OR ((COMB-KOH1HKNNUM = "187" OR "188"
                                         OR "189" OR "190"      ) AND
                         (COMB-KOH2HKNNUM = LNK-FTNJGNCHK-KOHNUM))
                     OR ((COMB-KOH2HKNNUM = "187" OR "188"
                                         OR "189" OR "190"      ) AND
                         (COMB-KOH3HKNNUM = LNK-FTNJGNCHK-KOHNUM))
                         MOVE  1   TO   WRK-HKNCOMBINUM-FLG(IDX)
                     END-IF
      *
                   END-IF
      *
                   IF    WRK-RECEJGNDSPKBN      =   "1"
                   AND   LNK-FTNJGNCHK-PREFNUM  =   32
                   AND   FLG-CHECK              =   ZERO
      *
                     IF   COMB-HKNNUM     NOT =  SPACE
                       IF   COMB-KOH1HKNNUM   =  "027"
                          IF  COMB-KOH1PAYKBN  =  "01"
                             CONTINUE
                          ELSE
                             MOVE    1    TO   FLG-CHECK
                          END-IF
                       ELSE
                          EVALUATE  LNK-FTNJGNCHK-NYUGAIKBN
                            WHEN       "1"
                              IF  COMB-HOJOKBN  =  "1" OR "5" OR "9"
                                 CONTINUE
                              ELSE
                                 MOVE    1    TO   FLG-CHECK
                              END-IF
                            WHEN       "2"
                              IF  COMB-HOJOKBN  =  "1" OR "4" OR "9"
                                 CONTINUE
                              ELSE
                                 MOVE    1    TO   FLG-CHECK
                              END-IF
                          END-EVALUATE
                       END-IF
      *
                       IF    FLG-CHECK                =   ZERO
                       AND   LNK-FTNJGNCHK-KOHNUM     =  "191"
                       AND   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       AND   WRK-TEISYOTOKU           =   SPACE
                       AND   FLG-OK                   =   ZERO
                       AND   LNK-FTNJGNCHK-SRYYM      <  "200610"
      *
                         MOVE    1           TO  FLG-OK
      *
                         INITIALIZE              ORCSAGECHKAREA
                         MOVE    LNK-FTNJGNCHK-HOSPNUM
                                             TO  AGECHK-HOSPNUM
                         MOVE    LNK-FTNJGNCHK-PTID
                                             TO  AGECHK-PTID
                         MOVE    LNK-FTNJGNCHK-SRYYM
                                             TO  AGECHK-SRYYMD(1:6)
                         MOVE    "01"        TO  AGECHK-SRYYMD(7:2)
                         MOVE    PTINF-BIRTHDAY
                                             TO  AGECHK-BIRTHDAY
                         CALL    "ORCSAGECHK"    USING
                                                 ORCSAGECHKAREA
                                                 SPA-AREA
                         IF      AGECHK-RC       =   ZERO
                           IF  ( AGECHK-NENREI   >=  70      )
                           OR  ( AGECHK-NENREI   >=  65  AND
                                 COMB-KOH1HKNNUM  = "027"    )
                              MOVE    01      TO  LNK-FTNJGNCHK-RC
                              MOVE     1      TO  FLG-CHECK
                           END-IF
                         END-IF
                       END-IF
                     END-IF
      *
                   END-IF
      *
                 END-IF
      *
                 MOVE   "tbl_hkncombi"      TO  MCP-TABLE
                 MOVE   "key4"              TO  MCP-PATHNAME
                 PERFORM 910-HKNCOMBI-READ-SEC
      *
               END-PERFORM
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
           MOVE     "tbl_hkncombi"      TO  MCP-TABLE
           MOVE     "key4"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿¤«¤é³ºÅö¸øÈñ¤Î´µ¼ÔÉéÃ´³Û¥Á¥§¥Ã¥¯
           IF   WRK-RECEJGNDSPKBN =  "1"
           AND  FLG-CHECK         =   1
           AND  LNK-FTNJGNCHK-RC  =  ZERO
              MOVE     ZERO    TO    FLG-CHECK
              MOVE     SPACE   TO    WRK-RECEJGNDSPKBN
           END-IF
      *
           IF   FLG-CHECK  =  ZERO
              PERFORM      2101-SYUKEI-SEC
           END-IF
      *
           .
       210-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³ºÅö¸øÈñ¤Î´µ¼ÔÉéÃ´³Û½¸·×
      *****************************************************************
       2101-SYUKEI-SEC            SECTION.
      *
           INITIALIZE                          SYUNOU-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM   TO  SYU-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID      TO  SYU-PTID
           MOVE    LNK-FTNJGNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
           MOVE    LNK-FTNJGNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
           MOVE    "__"                    TO  SYU-SRYYMD(7:2)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(1)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(2)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(3)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(4)
           IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
              MOVE    "tbl_syunou"         TO  MCP-TABLE
              MOVE    "key33"              TO  MCP-PATHNAME
           ELSE
              MOVE    "tbl_syunou"         TO  MCP-TABLE
              MOVE    "key25"              TO  MCP-PATHNAME
           END-IF
           PERFORM 910-SYUNOU-SELECT-SEC
           IF      FLG-SYUNOU              NOT =   ZERO
               MOVE    01                  TO  LNK-FTNJGNCHK-RC
               GO  TO  2101-SYUKEI-EXT
           ELSE
               IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key33"          TO  MCP-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key25"          TO  MCP-PATHNAME
               END-IF
               PERFORM 910-SYUNOU-READ-SEC
               IF   FLG-SYUNOU             =   ZERO
                 IF   (HKN-RECEJGNDSPKBN    =   "1"           )
                 OR   (HKN-HKNNUM           =   "972" OR "974"
                                                      OR "012")
                 OR   (WRK-RECEJGNDSPKBN    =   "1"           )
                     PERFORM 21011-SYUKEI-HENSYU1-SEC
                 ELSE
                     PERFORM 21012-SYUKEI-HENSYU2-SEC
                 END-IF
               END-IF
           END-IF
      *    ¼ýÇ¼¥¯¥í¡¼¥º
           IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    "key33"          TO  MCP-PATHNAME
           ELSE
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    "key25"          TO  MCP-PATHNAME
           END-IF
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÔ½¸½èÍý£±
      *****************************************************************
       21011-SYUKEI-HENSYU1-SEC            SECTION.
      *
      *    ´µ¼ÔÉéÃ´¶â¤Î½¸·×
           MOVE      ZERO          TO      WRK-KOHFTN
           MOVE      ZERO          TO      WRK-SEIKYU
           MOVE      ZERO          TO      FLG-GENMEN
           MOVE      ZERO          TO      FLG-DAYCHK
           MOVE      ZERO          TO      WRK-SKJ-FTN
           PERFORM   UNTIL    FLG-SYUNOU  =   1
             MOVE    ZERO          TO      FLG-ARI
             PERFORM   VARYING   IDX1   FROM  1  BY  1
                        UNTIL   (IDX1                  >  50 )
                          OR    (WRK-HKNCOMBINUM(IDX1) = ZERO)
                          OR    (FLG-ARI               =   1 )
               IF   SYU-HKNCOMBINUM  =  WRK-HKNCOMBINUM(IDX1)
                  MOVE    1    TO    FLG-ARI
                  EVALUATE   LNK-FTNJGNCHK-KOHNUM
                    WHEN   SYU-KOH-HKNNUM(1)
                       ADD  SYU-KOH-HKNFTNMONEY(1) TO  WRK-KOHFTN
                    WHEN   SYU-KOH-HKNNUM(2)
                       ADD  SYU-KOH-HKNFTNMONEY(2) TO  WRK-KOHFTN
                    WHEN   SYU-KOH-HKNNUM(3)
                       ADD  SYU-KOH-HKNFTNMONEY(3) TO  WRK-KOHFTN
                    WHEN   SYU-KOH-HKNNUM(4)
                       ADD  SYU-KOH-HKNFTNMONEY(4) TO  WRK-KOHFTN
                  END-EVALUATE
                  ADD       SYU-TOTAL-HKNTEN       TO  WRK-SEIKYU
                  IF     SYU-YKZKENNUM  NOT = ZERO
                       MOVE    1    TO   FLG-GENMEN
                  END-IF
                  IF  (LNK-FTNJGNCHK-PREFNUM    =  42     )
                  AND (LNK-FTNJGNCHK-NYUGAIKBN  = "2"     )
                  AND (LNK-FTNJGNCHK-KOHNUM     = "180"   )
                  AND (LNK-FTNJGNCHK-SRYYM     >= "200704")
                    EVALUATE   LNK-FTNJGNCHK-KOHNUM
                      WHEN   SYU-KOH-HKNNUM(1)
                        IF SYU-KOH-HKNFTNMONEY(1) >= WRK-HKNMST-JGNGAK2
                             MOVE   1    TO   FLG-DAYCHK
                        END-IF
                      WHEN   SYU-KOH-HKNNUM(2)
                        IF SYU-KOH-HKNFTNMONEY(2) >= WRK-HKNMST-JGNGAK2
                             MOVE   1    TO   FLG-DAYCHK
                        END-IF
                      WHEN   SYU-KOH-HKNNUM(3)
                        IF SYU-KOH-HKNFTNMONEY(3) >= WRK-HKNMST-JGNGAK2
                             MOVE   1    TO   FLG-DAYCHK
                        END-IF
                      WHEN   SYU-KOH-HKNNUM(4)
                        IF SYU-KOH-HKNFTNMONEY(4) >= WRK-HKNMST-JGNGAK2
                             MOVE   1    TO   FLG-DAYCHK
                        END-IF
                    END-EVALUATE
                  END-IF
                  IF  (LNK-FTNJGNCHK-NYUGAIKBN  = "1"     )
                  AND (LNK-FTNJGNCHK-KOHNUM     = "012"   )
                  AND (LNK-FTNJGNCHK-SRYYM     >= "200804")
                    IF  (SYU-SYUHKNNUM     =   SPACE)
                    AND (SYU-KOH-HKNNUM(1) =  "079" )
                    AND (SYU-KOH-HKNNUM(2) =  "012" )
                        ADD  SYU-SKJ-FTN-RECE(1) TO  WRK-SKJ-FTN
                    END-IF
                  END-IF
                  IF     WRK-HKNCOMBINUM-FLG(IDX1) =  1
                        MOVE   01                TO  LNK-FTNJGNCHK-RC
                  END-IF
               END-IF
             END-PERFORM
             IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                MOVE    "tbl_syunou"     TO  MCP-TABLE
                MOVE    "key33"          TO  MCP-PATHNAME
             ELSE
                MOVE    "tbl_syunou"     TO  MCP-TABLE
                MOVE    "key25"          TO  MCP-PATHNAME
             END-IF
             PERFORM  910-SYUNOU-READ-SEC
           END-PERFORM
      *
           IF  ((LNK-FTNJGNCHK-PREFNUM    =  27           ) AND
                (LNK-FTNJGNCHK-KOHNUM     = "141" OR "241") AND
                (LNK-FTNJGNCHK-SRYYM     >= "200804"      ))
           OR  ((LNK-FTNJGNCHK-PREFNUM    =  40           ) AND
                (LNK-FTNJGNCHK-KOHNUM(1:1)
                                       >=  "1" AND <=  "8") AND
                (LNK-FTNJGNCHK-NYUGAIKBN  = "1"           ) AND
                (LNK-FTNJGNCHK-SRYYM     >= "200810"      ))
              MOVE   9999999999     TO  WRK-KOHFTN
           END-IF
      *
           IF  (LNK-FTNJGNCHK-PREFNUM    =  42     )
           AND (LNK-FTNJGNCHK-NYUGAIKBN  = "2"     )
           AND (LNK-FTNJGNCHK-KOHNUM     = "180"   )
           AND (LNK-FTNJGNCHK-SRYYM     >= "200704")
             IF   ( WRK-HKNMST-JGNGAK   >   WRK-KOHFTN )
             AND  ( FLG-DAYCHK          =   ZERO       )
                   MOVE   01        TO  LNK-FTNJGNCHK-RC
             END-IF
           ELSE
             IF ((LNK-FTNJGNCHK-PREFNUM  =  11     ) AND
                 (LNK-FTNJGNCHK-KOHNUM   =
                   "181" OR "182" OR "183" OR "281") AND
                 (LNK-FTNJGNCHK-SRYYM   >= "200704"))
             OR ((LNK-FTNJGNCHK-PREFNUM  =  11     ) AND
                 (LNK-FTNJGNCHK-KOHNUM   = "381"   ) AND
                 (LNK-FTNJGNCHK-SRYYM   >= "200801"))
             OR ((LNK-FTNJGNCHK-PREFNUM  =  11     ) AND
                 (LNK-FTNJGNCHK-KOHNUM   = "481" OR
                                           "482"   ) AND
                 (LNK-FTNJGNCHK-NYUGAIKBN = "2"    ) AND
                 (LNK-FTNJGNCHK-SRYYM   >= "200804"))
             OR ((LNK-FTNJGNCHK-PREFNUM  =  11     ) AND
                 (LNK-FTNJGNCHK-KOHNUM   = "483"   ) AND
                 (WRK-TEISYOTOKU     NOT =  SPACE  ) AND
                 (LNK-FTNJGNCHK-NYUGAIKBN = "2"    ) AND
                 (LNK-FTNJGNCHK-SRYYM   >= "200804"))
               IF   WRK-KOHFTN   NOT = ZERO
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
               END-IF
             ELSE
               EVALUATE   LNK-FTNJGNCHK-KOHNUM
                 WHEN       "012"
                   IF   WRK-SKJ-FTN  >=  14880
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                   END-IF
                 WHEN       "972"
                 WHEN       "974"
                   IF   FLG-GENMEN   =   1
                     CONTINUE
                   ELSE
                     IF   WRK-HKNMST-JGNGAK   >   WRK-KOHFTN
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                     END-IF
                   END-IF
                 WHEN       OTHER
                   IF     WRK-HKNMST-JGNGAK   >   WRK-KOHFTN
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                   END-IF
               END-EVALUATE
             END-IF
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC  =   ZERO
             IF  WRK-SEIKYU        =   ZERO
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
             END-IF
           END-IF
      *
           .
       21011-SYUKEI-HENSYU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÔ½¸½èÍý£²
      *****************************************************************
       21012-SYUKEI-HENSYU2-SEC            SECTION.
      *
           MOVE      ZERO       TO      WRK-SEIKYU
           PERFORM   UNTIL   (FLG-SYUNOU     =  1  )
                      OR     (WRK-SEIKYU NOT = ZERO)
             MOVE    ZERO       TO      FLG-ARI
             PERFORM   VARYING   IDX1   FROM  1  BY  1
                        UNTIL   (IDX1                  >  50 )
                          OR    (WRK-HKNCOMBINUM(IDX1) = ZERO)
                          OR    (FLG-ARI               =   1 )
               IF   SYU-HKNCOMBINUM  =  WRK-HKNCOMBINUM(IDX1)
                  MOVE       1             TO  FLG-ARI
                  ADD    SYU-TOTAL-HKNTEN  TO  WRK-SEIKYU
               END-IF
             END-PERFORM
      *
             IF  WRK-SEIKYU  =  ZERO
               IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key33"          TO  MCP-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key25"          TO  MCP-PATHNAME
               END-IF
               PERFORM  910-SYUNOU-READ-SEC
             END-IF
           END-PERFORM
      *
           IF    LNK-FTNJGNCHK-RC  =   ZERO
             IF  WRK-SEIKYU        =   ZERO
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
             END-IF
           END-IF
      *
           .
       21012-SYUKEI-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
               MOVE    "tbl_ptinf"      TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC           =   ZERO
                   MOVE    MCPDATA-REC  TO  PTINF-REC
                   MOVE    ZERO         TO  FLG-PTINF
               ELSE
                   INITIALIZE               PTINF-REC
                   MOVE    1            TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                   PTINF-REC
               MOVE    1                TO  FLG-PTINF
           END-IF
      *
           MOVE     "tbl_ptinf"         TO  MCP-TABLE
           MOVE     "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SYUNOU-SELECT-SEC        SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYUNOU
           ELSE
               MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-HKNNUM-SELECT-SEC        SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       910-HKNNUM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-HKNNUM-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       910-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊóÆÉ¹þ
      *****************************************************************
       910-PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
