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
       PROGRAM-ID.                 ORCSROUNYUIN.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Ï«ºÒÆþ±¡²ñ·×ºîÀ®¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/07/11    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    04/02/27  Ì¤¼Â»Ü¸º»»·îÅÓÃæÊÑ¹¹ÂÐ±þ
      *  01.00.02    NACL-¾®Æ¦Âô  04/03/11  Í­¾²¿ÇÎÅ½ê£±·²Æþ±¡´ðËÜÎÁ£±
      *                                     ¡Ê°å»ÕÇÛÃÖÅ¬¹ç¡Ë¤ÎÂÐ±þ
      *  01.00.03    NACL-ÌçÏÆ    04/03/17  Â¾°åÎÅµ¡´Ø¼õ¿Ç¤ÎÈ½ÄêÍÑ¥³¡¼¥É
      *                                     ¡ÊÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬¡ËÄÉ²Ã
      *  01.00.04    NACL-ÌçÏÆ    04/03/29  êóáì´µ¼Ô´ÉÍý²Ã»»¤ÎÂÐ±þ
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
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-RJN             PIC 9(01).
           03  FLG-ROUSAI          PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-RYAKU           PIC 9(01).
           03  FLG-NYURYAKUSHO     PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-KSN             PIC 9(01).
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  ID1                 PIC 9(04).
           03  KSN-IDX             PIC 9(04).
           03  GENSAN-IDX          PIC 9(04).
           03  GENSAN-IDX2         PIC 9(04).
           03  IDV                 PIC 9(02).
           03  SRYACT-IDX          PIC 9(04).
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
       01  CONSTANT-AREA.
      *    Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»
           03  CONS-KEIKAKU-GENSAN1        PIC X(09) VALUE "190080470".
           03  CONS-KEIKAKU-GENSAN2        PIC X(09) VALUE "190799670".
      *    ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»
           03  CONS-INNAI-GENSAN1          PIC X(09) VALUE "190080570".
           03  CONS-INNAI-GENSAN2          PIC X(09) VALUE "190799770".
      *    °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»
           03  CONS-ANZEN-GENSAN1          PIC X(09) VALUE "190111690".
           03  CONS-ANZEN-GENSAN2          PIC X(09) VALUE "190809090".
      *    êóáìÂÐºöÌ¤¼Â»Ü¸º»»
           03  CONS-JYOKU-GENSAN1          PIC X(09) VALUE "190111790".
           03  CONS-JYOKU-GENSAN2          PIC X(09) VALUE "190809190".
      *    Æü¾ïÀ¸³è¾ã³²²Ã»»
           03  CONS-NICHIJOU-KSN1          PIC X(09) VALUE "190109170".
           03  CONS-NICHIJOU-KSN2          PIC X(09) VALUE "190807570".
      *    ÃÔÊò²Ã»»
           03  CONS-CHIHOU-KSN1            PIC X(09) VALUE "190109270".
           03  CONS-CHIHOU-KSN2            PIC X(09) VALUE "190808670".
      *    êóáì´µ¼Ô´ÉÍý²Ã»»
           03  CONS-JYOKU-KSN1             PIC X(09) VALUE "190116990".
           03  CONS-JYOKU-KSN2             PIC X(09) VALUE "190813390".
      *
      *    Ï«ºÒ¼£ÎÅ·×²è²Ã»»
           03  CONS-ROSAICHIRYO            PIC X(09) VALUE "101910050".
      *    ÉÂ°áÂßÍ¿ÎÁ
           03  CONS-BYOUI                  PIC X(09) VALUE "101910060".
      *    ÆÃÊÌ´Æ»ëÎÁ
           03  CONS-TOKUKANSI              PIC X(09) VALUE "101920010".
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-RYAKU-SRYCD             PIC X(09).
           03  WRK-RYAKUSHO                PIC X(14).
           03  WRK-SRYKBN                  PIC X(02).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-ZAISKBKBN               PIC X(01).
      *
           03  WRK-SYS5000-COUNT           PIC 9(02).
      *
           03  WRK-GENSAN2-MIJISI          PIC 9(01).
           03  WRK-GENSAN3-MIJISI          PIC 9(01).
           03  WRK-GENSAN4-MIJISI          PIC 9(01).
      *
      *    Ï«ºÒÆþ±¡²ñ·×¥ï¡¼¥¯
           03  WRK-ROUSAI-TABLE.
               05  WRK-ROUSAI-TBL          OCCURS  31.
                   07  WRK-NYUINKHN        PIC X(09).
                   07  WRK-SHOKIKSN        PIC X(09).
                   07  WRK-GAIHAKU         PIC X(09).
                   07  WRK-NYUKHNKSNKBN    PIC 9(03).
      *
      *    Æþ±¡ÎÁ²Ã»»ÂàÈòÎÎ°è
           03  WRK-NYUKSN-TABLE.
               05  WRK-NYUKSN-TBL          OCCURS  20.
                   07  WRK-NYUKSN          PIC X(09).
                   07  WRK-NYUKSN-ZAITEN   PIC 9(08).
                   07  WRK-NYUKSN-DAY      PIC 9(03)  OCCURS 31.
      *
      *    Ì¤¼Â»Ü¸º»»ÂàÈòÍÑ¥Õ¥é¥°¥Æ¡¼¥Ö¥ë
           03  WRK-GENSAN-TABLE.
               05  WRK-GENSAN-INF          OCCURS  31.
                   07  WRK-GENSAN-INNAI    PIC X(01).
                   07  WRK-GENSAN-IRYO     PIC X(01).
                   07  WRK-GENSAN-JYOKU    PIC X(01).
      *
      *    ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-GENSAN2                 PIC X(09).
      *    °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»¥ï¡¼¥¯
           03  WRK-GENSAN3                 PIC X(09).
      *    êóáìÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-GENSAN4                 PIC X(09).
      *
      *    Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-KEIKAKU-GENSAN1         PIC 9(03) OCCURS 31.
           03  WRK-KEIKAKU-GENSAN2         PIC 9(03) OCCURS 31.
      *    ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-INNAI-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-INNAI-GENSAN2           PIC 9(03) OCCURS 31.
      *    °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»¥ï¡¼¥¯
           03  WRK-ANZEN-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-ANZEN-GENSAN2           PIC 9(03) OCCURS 31.
      *    êóáìÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-JYOKU-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-JYOKU-GENSAN2           PIC 9(03) OCCURS 31.
      *
      *    Æü¾ïÀ¸³è¾ã³²²Ã»»ÂàÈò
           03  WRK-NICHIJOU-KSN1           PIC 9(03) OCCURS 31.
           03  WRK-NICHIJOU-KSN2           PIC 9(03) OCCURS 31.
      *
      *    ÃÔÊò²Ã»»ÂàÈò
           03  WRK-CHIHOU-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-CHIHOU-KSN2             PIC 9(03) OCCURS 31.
      *
      *    êóáì´µ¼Ô´ÉÍý²Ã»»
           03  WRK-JYOKU-KSN1              PIC 9(03) OCCURS 31.
           03  WRK-JYOKU-KSN2              PIC 9(03) OCCURS 31.
      *
      *    ¿ÇÎÅ¥³¡¼¥É·×½¸·×ÍÑ
           03  WRK-SRYCD-X                 PIC X(09).
           03  WRK-SRYCD-9    REDEFINES    WRK-SRYCD-X
                                           PIC 9(09).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    Æþ±¡¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡Ý
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
           COPY    "CPTENSU.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡Ý
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    Æþ±¡Î¬¾Î
       01  NYURYAKUSHO-REC.
           COPY    "CPNYUINRYAKUSHO.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *    Ï«ºÒÆþ±¡²ñ·×ºîÀ®ÍÑ¥Ñ¥é¥á¥¿
           COPY    "CPORCSROUNYUIN.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSROUNYUIN-AREA
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
           INITIALIZE                  LNK-SROUNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-SROUNYUIN-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-SROUNYUIN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ¼ç½èÍý
           IF      LNK-SROUNYUIN-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
           IF    ( LNK-SROUNYUIN-SANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-SROUNYUIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                      PTINF-REC
           MOVE    LNK-SROUNYUIN-HOSPID    TO  PTINF-HOSPID
           MOVE    LNK-SROUNYUIN-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-SROUNYUIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    Ï«ºÒÆþ±¡Ãæ¤Î¥Á¥§¥Ã¥¯¡ÊÆþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷¡Ë
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPID    TO  NACCT-HOSPID
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    3               TO  LNK-SROUNYUIN-RC
               GO  TO  100-PRM-CHK-EXT
           ELSE
               MOVE    "NYUINACCT-KEY17"   TO  ORC-DBPATH
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    3               TO  LNK-SROUNYUIN-RC
                    GO  TO  100-PRM-CHK-EXT
               ELSE
      *            Ï«ºÒÊÝ¸±¤ÎÈ½Äê
                   MOVE      ZERO              TO  FLG-ROUSAI
                   PERFORM   VARYING   IDX1   FROM  1   BY  1
                             UNTIL    IDX1   >   31
                        IF   LNK-SROUNYUIN-HKNCOMBI  =
                                               NACCT-DAY(IDX1)
                             MOVE    1         TO  FLG-ROUSAI
                             MOVE   "1"        TO  
                                          LNK-SROUNYUIN-NYUIN-FLG(IDX1)
                        END-IF
                   END-PERFORM
                   IF   FLG-ROUSAI             =   ZERO
                        MOVE    3              TO  LNK-SROUNYUIN-RC
                   END-IF
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "NYUINACCT-KEY17"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *Ï«ºÒÆþ±¡ÎÁ¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPID    TO  NACCT-HOSPID
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "NYUINACCT-KEY17"  TO  ORC-DBPATH
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                   MOVE      SPACE    TO      WRK-RYAKUSHO
                                              WRK-SRYKBN
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                ³ºÅöÆþ±¡¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß
                       PERFORM    210-NYUINACT-GET-SEC
      *                Æþ±¡´ðËÜÎÁ¤È½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1")
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
                               IF   NSRY-SRYCD(2)  NOT =   SPACE
                                   MOVE   NSRY-SRYCD(2)  TO
                                                   WRK-SHOKIKSN(IDX1)
                               END-IF
      *
                               IF   FLG-RYAKU   =   ZERO
      *
                                  MOVE    NACCT-SRYKBN
                                                TO  WRK-SRYKBN
      *                           ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬
      *                           ¤ÎÂàÈò
                                  PERFORM   210-GAIHAKU-MAE-SEC
      *
                                  MOVE   NSRY-SRYCD(1)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   NACCT-ZAISKBKBN
                                                TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                               MOVE  WRK-SRYKBN
                                         TO  LNK-SROUNYUIN-SRYKBN(IDX1)
                               MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
      *
                               IF    WRK-RYAKUSHO   NOT =  SPACE
                                  MOVE  WRK-RYAKUSHO
                                      TO  LNK-SROUNYUIN-RYAKUSYO(IDX1)
                               END-IF
      *
                               MOVE    1    TO   FLG-OK
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
               IF   FLG-OK     =   ZERO
                   MOVE     3      TO      LNK-SROUNYUIN-RC
                   GO   TO    200-MAIN-EXT
               END-IF
           ELSE
               MOVE     3      TO      LNK-SROUNYUIN-RC
               GO   TO    200-MAIN-EXT
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "NYUINACCT-KEY17"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *Ï«ºÒÆþ±¡»þ¤Î³°Çñ¼èÆÀ
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPID    TO  NACCT-HOSPID
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "NYUINACCT-KEY17"  TO  ORC-DBPATH
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ³°Çñ¶èÊ¬¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "NYUINACCT-KEY17"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
           PERFORM   210-RJN-TAISYO-CHK-SEC
      *
      *·îÆâ¤ËÊ£¿ô¤ÎÆþ±¡´ðËÜ¾ðÊó¡Ê£µ£°£°£°¡Ë¤¬¤Ê¤¤¤«¸¡º÷
           PERFORM   210-SYS-5000-CHK-SEC
      *
      *°åÎÅµ¡´Ø¸º»»¾ðÊó¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  ORC-DBYMD(1:6)
           MOVE    "01"                TO  ORC-DBYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                        UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                                MOVE  "190080570"  TO  WRK-GENSAN2
      **                          IF   FLG-RJN    =    ZERO
      **                              MOVE  "190080570"  TO  WRK-GENSAN2
      **                          ELSE
      **                              MOVE  "190799770"  TO  WRK-GENSAN2
      **                          END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-INNAI(GENSAN-IDX2)
                                END-PERFORM
                            WHEN  3
                                MOVE  "190111690"  TO  WRK-GENSAN3
      **                          IF   FLG-RJN    =    ZERO
      **                              MOVE  "190111690"  TO  WRK-GENSAN3
      **                          ELSE
      **                              MOVE  "190809090"  TO  WRK-GENSAN3
      **                          END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-IRYO(GENSAN-IDX2)
                                END-PERFORM
                            WHEN  4
                                MOVE  "190111790"  TO  WRK-GENSAN4
      **                          IF   FLG-RJN    =    ZERO
      **                              MOVE  "190111790"  TO  WRK-GENSAN4
      **                          ELSE
      **                              MOVE  "190809190"  TO  WRK-GENSAN4
      **                          END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-JYOKU(GENSAN-IDX2)
                                END-PERFORM
                        END-EVALUATE
                    END-IF
               END-PERFORM
           END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
           MOVE    "SYSKANRI-KEY"          TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           IF        WRK-SYS5000-COUNT    =     ZERO
                 CONTINUE
           ELSE
                 PERFORM  210-SYS-5000-GET-SEC
           END-IF
      *
      *
      *Ï«ºÒÆþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPID    TO  NACCT-HOSPID
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "NYUINACCT-KEY17"  TO  ORC-DBPATH
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                   MOVE      ZERO     TO      FLG-KSN
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                       AND   (LNK-SROUNYUIN-GAIHAKU(IDX1)  
                                             =  ZERO  OR  4)
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                           IF   FLG-KSN   =  ZERO
                               ADD     1    TO    KSN-IDX
                               MOVE    1    TO    FLG-KSN
                           END-IF
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-NYUKSN(KSN-IDX)
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-NYUKSN-ZAITEN(KSN-IDX)
                           MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-NYUKSN-DAY(KSN-IDX IDX1)
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "NYUINACCT-KEY17"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *Ï«ºÒÆþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡Ê¿ÇÎÅ²ñ·×¤è¤ê¡Ë
           INITIALIZE                           SRYACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPID    TO   ACCT-HOSPID
           MOVE    1                       TO   ACCT-NYUGAIKBN
           MOVE    LNK-SROUNYUIN-PTID      TO   ACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO   ACCT-SRYYM
           MOVE    LNK-SROUNYUIN-HKNCOMBI  TO   ACCT-HKNCOMBI
           MOVE    "SRYACCT-KEY22"         TO   ORC-DBPATH
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT           =   ZERO
               MOVE     "SRYACCT-KEY22"    TO   ORC-DBPATH
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)
                   IF   ACCT-SRYKBN  =  "90" OR "92"   
                     IF   ACCT-SRYCDTOTAL(6:9)  =   
                                            CONS-KEIKAKU-GENSAN1
                                        OR  CONS-KEIKAKU-GENSAN2
                                        OR  CONS-INNAI-GENSAN1
                                        OR  CONS-INNAI-GENSAN2
                                        OR  CONS-ANZEN-GENSAN1
                                        OR  CONS-ANZEN-GENSAN2
                                        OR  CONS-JYOKU-GENSAN1
                                        OR  CONS-JYOKU-GENSAN2
                                        OR  CONS-NICHIJOU-KSN1
                                        OR  CONS-NICHIJOU-KSN2
                                        OR  CONS-CHIHOU-KSN1
                                        OR  CONS-CHIHOU-KSN2
                                        OR  CONS-JYOKU-KSN1
                                        OR  CONS-JYOKU-KSN2
      *                Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                           AND   (LNK-SROUNYUIN-GAIHAKU(IDX1)  
                                                        =  ZERO)
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                              EVALUATE   ACCT-SRYCDTOTAL(6:9)
      *                         Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»
                                WHEN  CONS-KEIKAKU-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KEIKAKU-GENSAN1(IDX1)
                                WHEN  CONS-KEIKAKU-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KEIKAKU-GENSAN2(IDX1)
      *                         ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»
                                WHEN  CONS-INNAI-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-INNAI-GENSAN1(IDX1)
                                WHEN  CONS-INNAI-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-INNAI-GENSAN2(IDX1)
      *                         °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»
                                WHEN  CONS-ANZEN-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ANZEN-GENSAN1(IDX1)
                                WHEN  CONS-ANZEN-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ANZEN-GENSAN2(IDX1)
      *                         êóáìÂÐºöÌ¤¼Â»Ü¸º»»
                                WHEN  CONS-JYOKU-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-GENSAN1(IDX1)
                                WHEN  CONS-JYOKU-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-GENSAN2(IDX1)
      *                         Æü¾ïÀ¸³è¾ã³²²Ã»»
                                WHEN  CONS-NICHIJOU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NICHIJOU-KSN1(IDX1)
                                WHEN  CONS-NICHIJOU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NICHIJOU-KSN2(IDX1)
      *                         ÃÔÊò²Ã»»
                                WHEN  CONS-CHIHOU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-CHIHOU-KSN1(IDX1)
                                WHEN  CONS-CHIHOU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-CHIHOU-KSN2(IDX1)
      *                         êóáì´µ¼Ô´ÉÍý²Ã»»
                                WHEN  CONS-JYOKU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-KSN1(IDX1)
                                WHEN  CONS-JYOKU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-KSN2(IDX1)
                              END-EVALUATE
      *
                              IF   FLG-RYAKU   =   ZERO
                                 MOVE   ACCT-SRYCDTOTAL(6:9)
                                               TO  WRK-RYAKU-SRYCD
                                 MOVE   "6"    TO  WRK-ZAISKBKBN
                                 PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                 MOVE    1     TO    FLG-RYAKU
                              END-IF
      *
                           END-IF
                       END-PERFORM
                     ELSE
                       IF   ACCT-SRYCDTOTAL(6:9)  =  CONS-ROSAICHIRYO
                                                 OR  CONS-BYOUI
                                                 OR  CONS-TOKUKANSI
      *                  Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                             AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  MOVE   ACCT-SRYCDTOTAL(6:9)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   "6"    TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                       ELSE
      *                  Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                             AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  PERFORM   210-SRYACT-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                       END-IF
                     END-IF
                   END-IF
                 END-IF
                 MOVE     "SRYACCT-KEY22"    TO   ORC-DBPATH
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
           MOVE    "SRYACCT-KEY22"      TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    ¼èÆÀ¤·¤¿Ï«ºÒÆþ±¡¾ðÊó¤«¤éÆüÃ±°Ì¤ÎÆþ±¡²ñ·×¾ðÊó¤òºÆ¹½ÃÛ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               MOVE     ZERO                TO  WRK-MEISAISU
      *        Æþ±¡´ðËÜÎÁ
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-NYUINKHN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ³°Çñ
               IF         WRK-GAIHAKU(IDX1)   NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Æü¾ïÀ¸³è¾ã³²²Ã»»
               IF         WRK-NICHIJOU-KSN1(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-NICHIJOU-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-NICHIJOU-KSN2(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-NICHIJOU-KSN2  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÔÊò²Ã»»
               IF         WRK-CHIHOU-KSN1(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-CHIHOU-KSN1     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-CHIHOU-KSN2(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-CHIHOU-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáì´µ¼Ô´ÉÍý²Ã»»
               IF         WRK-JYOKU-KSN1(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-KSN1      TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-JYOKU-KSN2(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-KSN2      TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»
               IF         WRK-KEIKAKU-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KEIKAKU-GENSAN1 TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-KEIKAKU-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KEIKAKU-GENSAN2 TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»
               IF         WRK-INNAI-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-INNAI-GENSAN1   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-INNAI-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-INNAI-GENSAN2   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»
               IF         WRK-ANZEN-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-ANZEN-GENSAN1   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-ANZEN-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-ANZEN-GENSAN2   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáìÂÐºöÌ¤¼Â»Ü¸º»»
               IF         WRK-JYOKU-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-GENSAN1   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-JYOKU-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-GENSAN2   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¡Ë
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-INNAI(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN2       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN2-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-IRYO(IDX1)   NOT =      SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN3       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN3-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-JYOKU(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN4       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN4-MIJISI
               END-IF
      *        Ï«ºÒ¾è¿ô¡ÊÆÃÄêÆþ±¡ÎÁ¤Î¾ì¹ç¤ÏÏ«ºÒ¾è¿ô¤Ê¤·¡Ë
               IF       LNK-SROUNYUIN-SRYKBN(IDX1)    =    "90"
                   EVALUATE  LNK-SROUNYUIN-JOUSU-KBN(IDX1)
                       WHEN  "1"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900010"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                       WHEN  "2"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900020"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                   END-EVALUATE
               END-IF
      *        ½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë
               IF      (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Æþ±¡ÎÁ²Ã»»¡ÊÆþ±¡²ñ·×¡Ë
      *         PERFORM   VARYING   KSN-IDX   FROM  1  BY  1
      *                   UNTIL    (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
      *                            (KSN-IDX             >  20)
      *            IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
      *                CONTINUE
      *            ELSE
      *                ADD     1           TO  WRK-MEISAISU
      *                MOVE WRK-NYUKSN(KSN-IDX)
      *                                    TO  LNK-SROUNYUIN-SRYCD
      *                                   (IDX1 WRK-MEISAISU)
      *            END-IF
      *         END-PERFORM
           END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿Ï«ºÒÆþ±¡²ñ·×¾ðÊó¤«¤éÅÀ¿ô¤Î»»Äê
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
               PERFORM   VARYING   IDX2    FROM   1  BY   1
                         UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                            OR (IDX2    >      20)
                   PERFORM    210-TENSU-KEISAN-SEC
               END-PERFORM
      *
               PERFORM   VARYING   KSN-IDX   FROM   1  BY   1
                         UNTIL (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
                               (KSN-IDX    >      20)
                  IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD  WRK-NYUKSN-ZAITEN(KSN-IDX)
                                 TO  LNK-SROUNYUIN-ZAITEN-NACCT(IDX1)
                  END-IF
               END-PERFORM
      *
               IF    LNK-SROUNYUIN-NYUIN-FLG(IDX1)      =   "1"
               AND   LNK-SROUNYUIN-ZAITEN(IDX1)         =  ZERO
                  MOVE     SPACE   TO   LNK-SROUNYUIN-NYUIN-FLG(IDX1)
               END-IF
           END-PERFORM
      *
      *
           IF  WRK-GENSAN2-MIJISI  =  1
                 MOVE    WRK-GENSAN2       TO  WRK-RYAKU-SRYCD
                 MOVE    "6"               TO  WRK-ZAISKBKBN
                 PERFORM   210-RYAKUSYO-HENSYU-SEC
           END-IF
      *
           IF  WRK-GENSAN3-MIJISI  =  1
                 MOVE    WRK-GENSAN3       TO  WRK-RYAKU-SRYCD
                 MOVE    "6"               TO  WRK-ZAISKBKBN
                 PERFORM   210-RYAKUSYO-HENSYU-SEC
           END-IF
      *
           IF  WRK-GENSAN4-MIJISI  =  1
                 MOVE    WRK-GENSAN4       TO  WRK-RYAKU-SRYCD
                 MOVE    "6"               TO  WRK-ZAISKBKBN
                 PERFORM   210-RYAKUSYO-HENSYU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¼èÆÀ
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID        TO  NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "NYUINACT-KEY9"     TO  ORC-DBPATH
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE     "NYUINACT-KEY9"    TO  ORC-DBPATH
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    Æþ±¡¹Ô°Ù¥¯¥í¡¼¥º
           MOVE    "NYUINACT-KEY9"     TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°ÇñÁ°½èÍý
      *****************************************************************
       210-GAIHAKU-MAE-SEC            SECTION.
      *
      *    ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬¤È¿ÇÎÅ¶èÊ¬¤ÎÂàÈò
           INITIALIZE                      TNS-TENSU-REC
           MOVE    NSRY-SRYCD(1)       TO  TNS-SRYCD
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  ORC-DBYMD(1:6)
           MOVE    "01"                TO  ORC-DBYMD(7:2)
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-NYUKHNKSNKBN    TO  WRK-KSNKBN
      *
           .
       210-GAIHAKU-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¿ÇÎÅ¥³¡¼¥É¼èÆÀ
      *****************************************************************
       210-GAIHAKU-SET-SEC            SECTION.
      *
           MOVE   NACCT-DAY(IDX1)     TO    LNK-SROUNYUIN-GAIHAKU(IDX1)
      *
           EVALUATE   LNK-SROUNYUIN-SRYKBN(IDX1)
              WHEN     "90"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107290"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107390"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                      PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                 END-EVALUATE
              WHEN     "92"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107490"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107590"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                      PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                 END-EVALUATE
           END-EVALUATE
      *
           .
       210-GAIHAKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¿ÇÎÅ¥³¡¼¥É¼èÆÀ
      *****************************************************************
       2101-GAIHAKU-TAKIKAN-SEC           SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *
               WHEN   099  THRU  100 
               WHEN   201  THRU  219
               WHEN   221  THRU  222
               WHEN   224
               WHEN   227  THRU  228
               WHEN   230  THRU  232
                  MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
               WHEN   013  THRU  021
               WHEN   101  THRU  109
                  MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
               WHEN   075  THRU  080
                  MOVE   "190108990"    TO   WRK-GAIHAKU(IDX1)
               WHEN   085  THRU  086
               WHEN   132  THRU  133
                  MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
      *
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
      *****************************************************************
       210-RJN-TAISYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-RJN
      *
      *    Æþ±¡´µ¼Ô¤ÎÏ·¿ÍÊÝ¸±È½Äê
           MOVE    ZERO                TO   FLG-HKNCOMBI
           INITIALIZE                       HKNCOMBI-REC
           MOVE    LNK-SROUNYUIN-HOSPID TO  COMB-HOSPID
           MOVE    LNK-SROUNYUIN-PTID  TO   COMB-PTID
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                       TO   COMB-HKNCOMBINUM
           MOVE    "HKNCOMBI-KEY"      TO   ORC-DBPATH
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI        =    ZERO
               MOVE     "HKNCOMBI-KEY" TO   ORC-DBPATH
               PERFORM   910-HKNCOMBI-READ-SEC
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
           MOVE    "HKNCOMBI-KEY"      TO   ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *    Æþ±¡´µ¼Ô¤ÎÇ¯Îð·×»»
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    LNK-SROUNYUIN-HOSPID
                                       TO  AGECHK-HOSPID
           MOVE    LNK-SROUNYUIN-PTID
                                       TO  AGECHK-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
           IF    ( AGECHK-RC           =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
      *    Ï·¿ÍÂÐ¾Ý´µ¼ÔÈ½Äê
           IF    ((FLG-HKNCOMBI        =   ZERO)    AND
                  (COMB-KOH1HKNNUM     =   "027"))   OR
                 ( AGECHK-NENREI      >=    75)      OR
                 ((AGECHK-NENREI      >=    70)     AND
                  (PTINF-BIRTHDAY     <=   "19320930"))
               MOVE    1               TO  FLG-RJN
           END-IF
      *
           .
       210-RJN-TAISYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô·×»»½èÍý
      *****************************************************************
       210-TENSU-KEISAN-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           IF     (IDX2                   =     1  )
           AND    (WRK-GAIHAKU(IDX1)   NOT =   SPACE)
               PERFORM    2101-GAIHAKU-NYUINKHN-SET-SEC
               IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
                  MOVE    WRK-GAIHAKU-SRYCD   TO  TNS-SRYCD
               ELSE
                  MOVE    LNK-SROUNYUIN-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
               END-IF
           ELSE
                  MOVE    LNK-SROUNYUIN-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
           END-IF
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  ORC-DBYMD(1:6)
           MOVE    "01"                    TO  ORC-DBYMD(7:2)
           PERFORM 910-TENSU-READ-SEC
      *
      *    ÅÀ¿ô»»Äê
           EVALUATE    TNS-TENSIKIBETU
      *        ÅÀ¿ô¡Ê¥×¥é¥¹¡Ë
               WHEN    3
                   ADD   TNS-TEN     TO  LNK-SROUNYUIN-ZAITEN(IDX1)
      *        ¡ó²Ã»»
               WHEN    5
                   COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                              LNK-SROUNYUIN-ZAITEN(IDX1)  +
                            ((LNK-SROUNYUIN-ZAITEN(IDX1)  *
                              (TNS-TEN   /  100))  + 0.5)
      *        ¡ó¸º»»
               WHEN    6
                   COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                            ( LNK-SROUNYUIN-ZAITEN(IDX1)  -
                             (LNK-SROUNYUIN-ZAITEN(IDX1)  *
                             (TNS-TEN   /  100)))        +  0.5
      *        ÅÀ¿ô¡Ê¥Þ¥¤¥Ê¥¹¡Ë
               WHEN    8
                   COMPUTE  LNK-SROUNYUIN-ZAITEN(IDX1)  =
                            LNK-SROUNYUIN-ZAITEN(IDX1)  -  TNS-TEN
      *        WHEN  OTHER
      *            ADD   TNS-TEN     TO  LNK-SROUNYUIN-ZAITEN(IDX1)
           END-EVALUATE
      *
      *    ¿ÇÎÅ¥³¡¼¥É·×
           MOVE    TNS-SRYCD          TO  WRK-SRYCD-X
           COMPUTE LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  =
                   LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  +   WRK-SRYCD-9
           ADD      1          TO  LNK-SROUNYUIN-MEISAISU(IDX1)
      *
           .
       210-TENSU-KEISAN-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ
      **************************************************************
       2101-GAIHAKU-NYUINKHN-SET-SEC           SECTION.
      *
           MOVE    SPACE    TO   WRK-GAIHAKU-SRYCD
      *
           EVALUATE  LNK-SROUNYUIN-SRYCD(IDX1 IDX2)
               WHEN  "190078210"
                     MOVE  "190077410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078310"
                     MOVE  "190077510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078410"
                     MOVE  "190077610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078510"
                     MOVE  "190077710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078610"
                     MOVE  "190077810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078710"
                     MOVE  "190077910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078810"
                     MOVE  "190078010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078910"
                     MOVE  "190078110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190082410"
                     MOVE  "190081710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082510"
                     MOVE  "190081810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082610"
                     MOVE  "190081910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082710"
                     MOVE  "190082010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082810"
                     MOVE  "190082110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082910"
                     MOVE  "190082210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190083010"
                     MOVE  "190082310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190084310"
                     MOVE  "190083610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084410"
                     MOVE  "190083710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084510"
                     MOVE  "190083810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084610"
                     MOVE  "190083910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084710"
                     MOVE  "190084010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084810"
                     MOVE  "190084110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084910"
                     MOVE  "190084210"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190087410"
                     MOVE  "190085910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087510"
                     MOVE  "190086010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087610"
                     MOVE  "190086110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087710"
                     MOVE  "190086210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087810"
                     MOVE  "190086310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087910"
                     MOVE  "190086410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088010"
                     MOVE  "190086510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088110"
                     MOVE  "190086610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088210"
                     MOVE  "190086710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088310"
                     MOVE  "190086810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088410"
                     MOVE  "190086910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088510"
                     MOVE  "190087010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088610"
                     MOVE  "190087110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088710"
                     MOVE  "190087210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088810"
                     MOVE  "190087310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190094710"
                     MOVE  "190093710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094810"
                     MOVE  "190093810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094910"
                     MOVE  "190093910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095010"
                     MOVE  "190094010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095110"
                     MOVE  "190094110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190797410"
                     MOVE  "190796610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797510"
                     MOVE  "190796710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797610"
                     MOVE  "190796810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797710"
                     MOVE  "190796910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797810"
                     MOVE  "190797010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797910"
                     MOVE  "190797110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798010"
                     MOVE  "190797210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798110"
                     MOVE  "190797310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190803510"
                     MOVE  "190802810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803610"
                     MOVE  "190802910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803710"
                     MOVE  "190803010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803810"
                     MOVE  "190803110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803910"
                     MOVE  "190803210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804010"
                     MOVE  "190803310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804110"
                     MOVE  "190803410"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190091210"
                     MOVE  "190090210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190091310"
                     MOVE  "190090310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190109670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190808870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117070"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813270"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           .
       2101-GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    Î¬¾Î¤ÎÊÔ½¸
      **************************************************************
       210-RYAKUSYO-HENSYU-SEC           SECTION.
      *
           IF   LNK-SROUNYUIN-RYAKUKBN   =   "1"
      *
              MOVE    SPACE         TO   NYURYAKUSHO-REC
              INITIALIZE                 NYURYAKUSHO-REC
      *
              MOVE    WRK-RYAKU-SRYCD          TO   NYURYAKUSHO-SRYCD
              MOVE    LNK-SROUNYUIN-SANTEIYM   TO   ORC-DBYMD(1:6)
              MOVE    "01"                     TO   ORC-DBYMD(7:2)
              PERFORM  910-NYURYAKUSHO-READ-SEC
      *
              IF       FLG-NYURYAKUSHO    =    ZERO
      *
                EVALUATE   WRK-ZAISKBKBN
      *
                 WHEN     1
      *
                  IF   WRK-SRYKBN  =  "90"
      *
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL   ID1                          > 20
                            OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                            OR    FLG-ARI                      = 1 
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)
                                           =  NYURYAKUSHO-RYAKUSHO
                           MOVE  1   TO   FLG-ARI
                       END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                           MOVE   NYURYAKUSHO-RYAKUSHO
                                     TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)
                    END-IF
      *
                    IF  WRK-RYAKU-SRYCD  =  "190109670" OR "190808870"
      *
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL   ID1                          > 20
                             OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                             OR    FLG-ARI                      = 1 
                         IF LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4) = "Í­´Ç"
                              MOVE  1   TO   FLG-ARI
                         END-IF
                       END-PERFORM  
                       IF  FLG-ARI  =  ZERO  
                            MOVE  "Í­´Ç"
                                TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4)
                       END-IF
      *
                    END-IF
                    IF  WRK-RYAKU-SRYCD  =  "190117070" OR "190813270"
      *
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL   ID1                          > 20
                             OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                             OR    FLG-ARI                      = 1 
                         IF LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4) = "Í­°å"
                              MOVE  1   TO   FLG-ARI
                         END-IF
                       END-PERFORM  
                       IF  FLG-ARI  =  ZERO  
                            MOVE  "Í­°å"
                                TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4)
                       END-IF
      *
                    END-IF
      *
                  ELSE
      *
                    MOVE    NYURYAKUSHO-RYAKUSHO   TO  WRK-RYAKUSHO
      *
                  END-IF
      *
                 WHEN   6
      *
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL   ID1                          > 20
                            OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                            OR    FLG-ARI                      = 1 
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)
                                           =  NYURYAKUSHO-RYAKUSHO
                           MOVE  1   TO   FLG-ARI
                       END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                           MOVE   NYURYAKUSHO-RYAKUSHO
                                     TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)
                    END-IF
      *
                END-EVALUATE
      *
              END-IF
      *
           END-IF
      *
           .
       210-RYAKUSYO-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¤ÎÆÉ¤ß¹þ¤ß½èÍý
      **************************************************************
       210-SRYACT-HENSYU-SEC           SECTION.
      *
           IF   LNK-SROUNYUIN-RYAKUKBN   =   "1"
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
              CALL    "ORCMCPSUB"          USING   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
              IF      MCP-RC               =       ZERO
                  MOVE    "SRYACT-KEY9"    TO      ORC-DBPATH
                  PERFORM  910-SRYACT-READ-SEC
                  PERFORM  UNTIL  FLG-SRYACT    =    1
                      PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                                UNTIL    (SRYACT-IDX    >   5)  OR
                                         (SRY-SRYCD(SRYACT-IDX)
                                                            =   SPACE)
      *
                          MOVE    SRY-SRYCD(SRYACT-IDX)
                                                  TO  WRK-RYAKU-SRYCD
                          MOVE    "6"             TO  WRK-ZAISKBKBN
      *
                          PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                      END-PERFORM
                      MOVE     "SRYACT-KEY9"    TO   ORC-DBPATH
                      PERFORM   910-SRYACT-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE     "SRYACT-KEY9"       TO  ORC-DBPATH
              PERFORM   910-DB-CLOSE-SEC
      *
           END-IF
      *
           .
       210-SRYACT-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤ËÊ£¿ô¤Î°åÎÅ´ü´Ö¾ðÊó¤¬¤Ê¤¤¤«¸¡º÷¡Ê°åÎÅµ¡´Ø¸º»»¤Î³ÎÇ§¡Ë  *
      ******************************************************************
       210-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SYS5000-COUNT
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  ORC-DBYMD(1:6)
           MOVE    "__"                TO  ORC-DBYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY11"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY11"  TO  ORC-DBPATH
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1     TO        WRK-SYS5000-COUNT
                   MOVE    "SYSKANRI-KEY11"  TO  ORC-DBPATH
                   PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "SYSKANRI-KEY11"    TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       210-SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤Î°åÎÅ´ü´Ö¾ðÊó¼èÆÀ¡Ê°åÎÅµ¡´Ø¸º»»¾ðÊó¡Ë                  *
      ******************************************************************
       210-SYS-5000-GET-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  ORC-DBYMD(1:6)
           MOVE    "__"                TO  ORC-DBYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY11"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY11"  TO  ORC-DBPATH
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
                 PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                            UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              MOVE  "190080570"  TO  WRK-GENSAN2
      **                        IF   FLG-RJN    =    ZERO
      **                            MOVE  "190080570"  TO  WRK-GENSAN2
      **                        ELSE
      **                            MOVE  "190799770"  TO  WRK-GENSAN2
      **                        END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-INNAI(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  3
                              MOVE  "190111690"  TO  WRK-GENSAN3
      **                        IF   FLG-RJN    =    ZERO
      **                            MOVE  "190111690"  TO  WRK-GENSAN3
      **                        ELSE
      **                            MOVE  "190809090"  TO  WRK-GENSAN3
      **                        END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-IRYO(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  4
                              MOVE  "190111790"  TO  WRK-GENSAN4
      **                        IF   FLG-RJN    =    ZERO
      **                            MOVE  "190111790"  TO  WRK-GENSAN4
      **                        ELSE
      **                            MOVE  "190809190"  TO  WRK-GENSAN4
      **                        END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-JYOKU(GENSAN-IDX2)
                              END-PERFORM
                        END-EVALUATE
                    ELSE
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-INNAI(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  3
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-IRYO(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  4
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-JYOKU(GENSAN-IDX2)
                              END-PERFORM
                        END-EVALUATE
                    END-IF
                 END-PERFORM
                 MOVE    "SYSKANRI-KEY11"  TO  ORC-DBPATH
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "SYSKANRI-KEY11"    TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       210-SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
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
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   INITIALIZE                     PTINF-REC
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                 PTINF-REC
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "PTINF-KEY"        TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SRYACCT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SRYACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       910-SRYACT-READ-SEC           SECTION.
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
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "TENSU-KEY"     TO   ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    Æþ±¡Î¬¾Î¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       910-NYURYAKUSHO-READ-SEC        SECTION.
      *
           MOVE    NYURYAKUSHO-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINRYAKUSHO-KEY" TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "NYUINRYAKUSHO-KEY" TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  NYURYAKUSHO-REC
                   MOVE    ZERO                TO  FLG-NYURYAKUSHO
               ELSE
                   MOVE    1                   TO  FLG-NYURYAKUSHO
                   INITIALIZE                  NYURYAKUSHO-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-NYURYAKUSHO
               INITIALIZE                  NYURYAKUSHO-REC
           END-IF
      *
           MOVE    "NYUINRYAKUSHO-KEY"     TO   ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-NYURYAKUSHO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSE"            TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
