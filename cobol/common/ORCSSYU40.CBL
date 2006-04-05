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
       PROGRAM-ID.                 ORCSSYU40.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¿ÇÎÅ¹Ô°ÙÆþÎÏ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Ê¡²¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *                      µëÉÕÂÐ¾Ý³°ÅÀ¿ôÊÔ½¸¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/08/25    NACL¡ÝÂ¿¡¹Ç¼  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-Â¿¡¹Ç¼  04/03/11  £È£±£¶¡¥£´²þÀµÂÐ±þ
      *  01.00.02    NACL-Â¿¡¹Ç¼  05/11/11  MONFUNC ÂÐ±þ Â¾
      *  01.00.03    NACL-Â¿¡¹Ç¼  06/03/20  £È£±£¸¡¥£´²þÀµÂÐ±þ
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
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
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYOSIN-ERR      PIC 9(01).
      *
           03  FLG-SYORI-END       PIC 9(01).
           03  FLG-SYONIKA         PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-D               PIC 9(04).
           03  IDX-D2              PIC 9(02).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-KASAN           PIC X(01).
      *    ¿ÇÎÅ¹Ô°Ù·×»»ÍÑ
           03  WRK-TENSU           PIC 9(08).
           03  WRK-ZAITEN          PIC 9(08).
      *
           03  WRK-SRYCD           PIC X(09).
           03  WRK-HEN-SRYCD       PIC X(09).
      *    ½é¿Ç»»ÄêÆü
           03  WRK-DD              PIC 9(02).
           03  WRK-DD-S            PIC 9(02).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYY           PIC 9(04).
               05  WRK-SRYMM           PIC 9(02).
               05  WRK-SRYDD           PIC 9(02).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-H-ZAINUM        PIC 9(08).
      *
      *    ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É°ìÍ÷
       01  TBL-SINSATU-AREA.
      *    ½é¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë
           03  FILLER              PIC X(13)   VALUE   "111000110 1 0".
      *    ½é¿ÇÎÁ¡ÊÏ·¿Í ÉÂ±¡¡Ë
           03  FILLER              PIC X(13)   VALUE   "111700110 1 0".
      *    ½é¿ÇÎÁ¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë
           03  FILLER              PIC X(13)   VALUE   "111003610 1 0".
      *    ½é¿ÇÎÁ¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë
           03  FILLER              PIC X(13)   VALUE   "111700210 1 0".
      *
      *    ±ý¿ÇÎÁ
           03  FILLER              PIC X(13)   VALUE   "114000110 1 0".
      *    ±ý¿Ç¡ÊÌë´Ö²Ã»»¡Ë
           03  FILLER              PIC X(13)   VALUE   "114000470 2 1".
      *    ±ý¿Ç¡Ê¿¼Ìë²Ã»»¡Ë
           03  FILLER              PIC X(13)   VALUE   "114000570 2 2".
      *    ±ý¿Ç¡Ê¶ÛµÞ²Ã»»¡Ë
           03  FILLER              PIC X(13)   VALUE   "114000370 2 2".
      *
      *    ÆÃÊÌ±ý¿ÇÎÁ
           03  FILLER              PIC X(13)   VALUE   "114001610 1 0".
      *    ÆÃÊÌ±ý¿Ç¡ÊÌë´Ö²Ã»»¡Ë
           03  FILLER              PIC X(13)   VALUE   "114001970 2 1".
      *    ÆÃÊÌ±ý¿Ç¡Ê¿¼Ìë²Ã»»¡Ë
           03  FILLER              PIC X(13)   VALUE   "114002070 2 2".
      *    ÆÃÊÌ±ý¿Ç¡Ê¶ÛµÞ²Ã»»¡Ë
           03  FILLER              PIC X(13)   VALUE   "114001870 2 2".
      *
      *
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(±¡³°½èÊý ½é¿Ç)
           03  FILLER              PIC X(13)   VALUE   "113003510 1 0".
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(±¡Æâ½èÊý ½é¿Ç)
           03  FILLER              PIC X(13)   VALUE   "113003710 1 0".
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(½é¿Ç»þ»þ´Ö³°²Ã»»)
           03  FILLER              PIC X(13)   VALUE   "111010470 2 1".
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(½é¿Ç»þµÙÆü²Ã»»)
           03  FILLER              PIC X(13)   VALUE   "111010570 2 2".
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(½é¿Ç»þ¿¼Ìë²Ã»»)
           03  FILLER              PIC X(13)   VALUE   "111010670 2 3".
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(½é¿Ç»þ»þ´Ö³°ÆÃÎã)
           03  FILLER              PIC X(13)   VALUE   "111010770 2 4".
      *
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(½é¿Ç»þÆýÍÄ»ùÌë´Ö¡Ë
           03  FILLER              PIC X(13)   VALUE   "113007070 2 5".
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(½é¿Ç»þÆýÍÄ»ùµÙÆü¡Ë
           03  FILLER              PIC X(13)   VALUE   "113007170 2 6".
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(½é¿Ç»þÆýÍÄ»ù¿¼Ìë¡Ë
           03  FILLER              PIC X(13)   VALUE   "113007270 2 7".
      *
       01  TBL-SINSATU-AREAR       REDEFINES   TBL-SINSATU-AREA.
      *****03  TBL-SINSATU-OCC         OCCURS  18    INDEXED  TBL-IDX.
           03  TBL-SINSATU-OCC         OCCURS  21    INDEXED  TBL-IDX.
      *        ¿ÇÎÅ¥³¡¼¥É
               05  TBL-SRYCD           PIC X(09).
      *
               05  TBL-YOBO1           PIC X(01).
      *        ¥Ç¡¼¥¿¶èÊ¬
               05  TBL-DATAKBN         PIC X(01).
      *
               05  TBL-YOBO2           PIC X(01).
      *        ±ý¿Ç²Ã»»¡Ê0:¤Ê¤·,1:Ìë´Ö¡¢2:¿¼Ìë¡¦¶ÛµÞ¡Ë
      *        »þ´Ö²Ã»»¡Ê0:¤Ê¤·,1:»þ´Ö³°¡¢2:µÙÆü¡¢3:¿¼Ìë¡¢4:ÆÃÎã
      *                 (5:ÆýÍÄ»ùÌë´Ö¡¢6:ÆýÍÄ»ùµÙÆü¡¢7:ÆýÍÄ»ù¿¼Ìë¡Ë
               05  TBL-KASAN           PIC X(01).
      *
      *
       01  TBL2-SYOKAI-AREA.
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(¾Ò²ð²Ã»»)
           03  FILLER              PIC X(09)   VALUE   "111010370".
      *    ½é¿Ç»þ(¾Ò²ð²Ã»»)
           03  FILLER              PIC X(09)   VALUE   "111003770".
      *    ¾Ò²ð²Ã»»£±
           03  FILLER              PIC X(09)   VALUE   "113005070".
           03  FILLER              PIC X(09)   VALUE   "111011270".
      *    ¾Ò²ð²Ã»»£²
           03  FILLER              PIC X(09)   VALUE   "113005170".
           03  FILLER              PIC X(09)   VALUE   "111011370".
      *    ¾Ò²ð²Ã»»£³
           03  FILLER              PIC X(09)   VALUE   "111010870".
           03  FILLER              PIC X(09)   VALUE   "111009970".
      *    ¾Ò²ð²Ã»»£´
           03  FILLER              PIC X(09)   VALUE   "111010970".
           03  FILLER              PIC X(09)   VALUE   "111010070".
      *    ¾Ò²ð²Ã»»£µ
           03  FILLER              PIC X(09)   VALUE   "111011070".
           03  FILLER              PIC X(09)   VALUE   "111010170".
      *    ¾Ò²ð²Ã»»£¶
           03  FILLER              PIC X(09)   VALUE   "111011170".
           03  FILLER              PIC X(09)   VALUE   "111010270".
      *
      *    °é»ù±ÉÍÜ»ØÆ³ÎÁ
           03  FILLER              PIC X(09)   VALUE   "099114001".
           03  FILLER              PIC X(09)   VALUE   "111000470".
      *
       01  TBL2-SYOKAI-AREAR       REDEFINES   TBL2-SYOKAI-AREA.
           03  TBL2-SYOKAI-OCC         OCCURS  8     INDEXED  TBL2-IDX.
      *        ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ¤Î²Ã»»
               05  TBL2-SRYCD-1        PIC X(09).
      *        ½é¿ÇÎÁ¤Î²Ã»»
               05  TBL2-SRYCD-2        PIC X(09).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡Ý
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¼õ¿ÇÍúÎò
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *    µëÉÕ³°ÅÀ¿ôÊÔ½¸¥Ñ¥é¥á¥¿ÎÎ°è
           COPY    "CPORCSSYU40.INC".
      *    ¼ýÇ¼¥Þ¥¹¥¿
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSSYU40AREA
           SYUNOU-REC
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
      *    ¿ÇÎÅ¶èÊ¬ÊÌµëÉÕ³°ÅÀ¿ô¥¯¥ê¥¢
           INITIALIZE                  SYU-KYUFUGAI
      *
           EVALUATE    ORCS40-KBN
               WHEN    "1"
      *            ¿ÇÎÅ¹Ô°Ù¤è¤ê
                   PERFORM 100-SRYKOU-GMN-SEC
               WHEN    "2"
      *            ¼ýÇ¼¤è¤ê
                   PERFORM 300-SYUNOU-GMN-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙºîÀ®½èÍý
      *****************************************************************
       100-SRYKOU-GMN-SEC           SECTION.
      *
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
      *    Åö·îºÇ½é¤Î½é¿ÇÎÁ»»ÄêÆü¤òÈ½Äê¤¹¤ë
           PERFORM 3000-SYOSIN-ALLCHK-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   5   )  OR
                              (FLG-SYOSIN-ERR  =   1   )   OR
                              (ORCS40-SRYKBN (IDX) =   SPACE)
               IF      ORCS40-SRYKBN (IDX)     =   "13"
                                               OR  "99"
      *            ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ»»Äê»þ¡¢½é¿ÇÎÁ·×»»
                   PERFORM 1001-SYOUNIKA-HEN-SEC
               ELSE
      *            ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ°Ê³°
                   PERFORM 1002-SRYCD-CHK-SEC
               END-IF
           END-PERFORM
      *
      *    ¹ç·×·×»»
           COMPUTE SYU-KYUFUGAI-GOKEI-TEN  =   SYU-KYUFUGAI-SHOSHIN-TEN
                                           +   SYU-KYUFUGAI-SIDOU-TEN
                                           +   SYU-KYUFUGAI-OSIN1-TEN
                                           +   SYU-KYUFUGAI-OSIN2-TEN
                                           +   SYU-KYUFUGAI-OSIN3-TEN
      *
      *TEST
      *    DISPLAY "SYU-KYUFUGAI-SHOSHIN-TEN:" SYU-KYUFUGAI-SHOSHIN-TEN
      *    DISPLAY "SYU-KYUFUGAI-SIDOU-TEN:" SYU-KYUFUGAI-SIDOU-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN1-TEN:" SYU-KYUFUGAI-OSIN1-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN2-TEN:" SYU-KYUFUGAI-OSIN2-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN3-TEN:" SYU-KYUFUGAI-OSIN3-TEN
      *TEST
           .
       100-SAKUSEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ°Ê³°½èÍý
      *****************************************************************
       1002-SRYCD-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  WRK-KASAN
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2            >   5   )  OR
                              (ORCS40-SRYCD (IDX IDX2)
                                               =   SPACE)
      *        ¥³¡¼¥É¥Á¥§¥Ã¥¯
               SET     TBL-IDX             TO  1
               SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    ZERO            TO  FLG-OK2
                   WHEN    ORCS40-SRYCD (IDX IDX2)
                                               =   TBL-SRYCD (TBL-IDX)
                       IF      WRK-KASAN           =   ZERO
                           MOVE    TBL-KASAN (TBL-IDX) TO  WRK-KASAN
                       END-IF
                       IF      TBL-DATAKBN (TBL-IDX)   =   "1"
                           MOVE    1               TO  FLG-OK
                           MOVE    IDX2            TO  IDX-SYU
                       END-IF
               END-SEARCH
           END-PERFORM
           IF      FLG-OK              =   ZERO
               GO      TO      1002-SRYCD-CHK-EXT
           END-IF
      *
      *
      *    ÂÐ¾Ý¤Î¿Ç»¡ÎÁ¤¢¤ê
           EVALUATE    ORCS40-SRYKBN (IDX)
               WHEN    "11"
      *            ½é¿ÇÎÁ¤Î»þ¡¢Åö·îºÇ½é¤«¤É¤¦¤«¤ÎÈ½Äê
                   IF     (WRK-DD              =   ZERO )  OR
                          (WRK-SRYDD           <=  WRK-DD)
      *                µëÉÕÂÐ¾Ý³°ÅÀ¿ô¡Ê½é¿Ç¡Ë
                       MOVE    ORCS40-ZAITEN (IDX)
                                           TO  SYU-KYUFUGAI-SHOSHIN-TEN
                   END-IF
               WHEN    "14"
                   EVALUATE    WRK-KASAN
                       WHEN    "0"
      *                µëÉÕÂÐ¾Ý³°ÅÀ¿ô¡Ê±ý¿Ç¡Ë
                           MOVE    ORCS40-ZAITEN (IDX)
                                           TO  SYU-KYUFUGAI-OSIN1-TEN
                       WHEN    "1"
      *                µëÉÕÂÐ¾Ý³°ÅÀ¿ô¡Ê±ý¿Ç¡¿Ìë´Ö¡Ë
                           MOVE    ORCS40-ZAITEN (IDX)
                                           TO  SYU-KYUFUGAI-OSIN2-TEN
                       WHEN    "2"
      *                µëÉÕÂÐ¾Ý³°ÅÀ¿ô¡Ê±ý¿Ç¡¿¿¼Ìë¡¿¶ÛµÞ¡Ë
                           MOVE    ORCS40-ZAITEN (IDX)
                                           TO  SYU-KYUFUGAI-OSIN3-TEN
                   END-EVALUATE
           END-EVALUATE
      *
           .
       1002-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ»»Äê½èÍý
      *****************************************************************
       1001-SYOUNIKA-HEN-SEC            SECTION.
      *
      *    Åö·îºÇ½é¤«¤É¤¦¤«¤ÎÈ½Äê
           IF     (WRK-DD              =   ZERO  )  OR
                  (WRK-SRYDD           <=  WRK-DD)
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-SYOSIN-ERR
               GO   TO      1001-SYOUNIKA-HEN-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  WRK-KASAN
           MOVE    SPACE               TO  WRK-HEN-SRYCD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2            >   5   )  OR
                              (ORCS40-SRYCD (IDX IDX2)
                                               =   SPACE)
      *        ¥³¡¼¥É¥Á¥§¥Ã¥¯
               SET     TBL-IDX             TO  1
               SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    ZERO            TO  FLG-OK2
                   WHEN    ORCS40-SRYCD (IDX IDX2)
                                               =   TBL-SRYCD (TBL-IDX)
                       IF      WRK-KASAN           =   ZERO
                           MOVE    TBL-KASAN (TBL-IDX) TO  WRK-KASAN
                       END-IF
                       IF      TBL-DATAKBN (TBL-IDX)   =   "1"
                           MOVE    1               TO  FLG-OK
                       END-IF
                       MOVE    1               TO  FLG-OK2
               END-SEARCH
      *
               IF      FLG-OK2                 =   ZERO
      *            ¾Ò²ð²Ã»»¤ÎÈ½Äê
                   SET     TBL2-IDX            TO  1
                   SEARCH  TBL2-SYOKAI-OCC     VARYING   TBL2-IDX
                       AT  END
                           MOVE    ZERO            TO  FLG-OK2
                       WHEN    ORCS40-SRYCD (IDX IDX2)
                                                   =   TBL2-SRYCD-1
                                                           (TBL2-IDX)
                            MOVE    TBL2-SRYCD-2 (TBL2-IDX)
                                                   TO  WRK-HEN-SRYCD
                   END-SEARCH
               END-IF
           END-PERFORM
      *    ÂÐ¾Ý³°
           IF     (FLG-OK              =   ZERO  )  AND
                  (WRK-HEN-SRYCD       =   SPACE )
               GO      TO      1001-SYOUNIKA-HEN-EXT
           END-IF
      *
      *    ½é¿ÇÎÁÅÀ¿ô¤òµëÉÕÂÐ¾Ý³°ÅÀ¿ô¤Ø
           IF      SPA-SRYYMD(1:6)     <   "200604"
               PERFORM 10013-SYOSIN-TEN-SEC
           ELSE
               PERFORM 10013-SYOSIN02-TEN-SEC
           END-IF
      *
           .
       1001-SYOUNIKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿ÇÎÁÅÀ¿ô¤òµëÉÕÂÐ¾Ý³°ÅÀ¿ôÀßÄê½èÍý
      *****************************************************************
       10013-SYOSIN-TEN-SEC            SECTION.
      *
      *
           MOVE    ZERO                TO  WRK-ZAITEN
      *    ¿ÇÎÅ¥³¡¼¥É¤«¤éÅÀ¿ô·×»»
           IF      FLG-OK              =   1
      *        ¿Ç»¡ÎÁ
               IF      SPA-HOSPSBT         =   "1"
      *            ÉÂ±¡
                   MOVE    "111000110"         TO  WRK-SRYCD
               ELSE
      *            ¿ÇÎÅ½ê
                   MOVE    "111003610"         TO  WRK-SRYCD
               END-IF
               PERFORM 10011-TENSU-SET-SEC
               ADD     WRK-TENSU               TO  WRK-ZAITEN
      *
      *        ÆýÍÄ»ù²Ã»»
               IF      WRK-KASAN           =   ZERO
                   MOVE    "111000370"         TO  WRK-SRYCD
               ELSE
                   MOVE    "111011470"         TO  WRK-SRYCD
               END-IF
               PERFORM 10011-TENSU-SET-SEC
               ADD     WRK-TENSU               TO  WRK-ZAITEN
      *
      *        »þ´Ö³°
               EVALUATE    WRK-KASAN
                   WHEN    "1"
      *                »þ´Ö³°
                       MOVE    "111000570"         TO  WRK-SRYCD
                   WHEN    "2"
      *                µÙÆü
                       MOVE    "111000670"         TO  WRK-SRYCD
                   WHEN    "3"
      *                ¿¼Ìë
                       MOVE    "111000770"         TO  WRK-SRYCD
                   WHEN    "4"
      *                »þ´Ö³°ÆÃÎãµÙÆü
                       MOVE    "111000870"         TO  WRK-SRYCD
                   WHEN    "5"
      *                ÆýÍÄ»ùÌë´Ö
                       MOVE    "111011570"         TO  WRK-SRYCD
                   WHEN    "6"
      *                ÆýÍÄ»ùµÙÆü
                       MOVE    "111011670"         TO  WRK-SRYCD
                   WHEN    "7"
      *                ÆýÍÄ»ù¿¼Ìë
                       MOVE    "111011770"         TO  WRK-SRYCD
                   WHEN    OTHER
      *                »þ´Ö³°¤Ê¤·
                       MOVE    SPACE               TO  WRK-SRYCD
               END-EVALUATE
               IF      WRK-SRYCD       NOT =   SPACE
                   PERFORM 10011-TENSU-SET-SEC
                   ADD     WRK-TENSU               TO  WRK-ZAITEN
               END-IF
           END-IF
      *
           IF      WRK-HEN-SRYCD   NOT =   SPACE
               MOVE    WRK-HEN-SRYCD       TO  WRK-SRYCD
               PERFORM 10011-TENSU-SET-SEC
               ADD     WRK-TENSU               TO  WRK-ZAITEN
           END-IF
      *
      *    µëÉÕÂÐ¾Ý³°ÅÀ¿ô¡Ê»ØÆ³¡Ë
           ADD     WRK-ZAITEN         TO  SYU-KYUFUGAI-SIDOU-TEN
      *
           .
       10013-SYOSIN-TEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿ÇÎÁÅÀ¿ô¤òµëÉÕÂÐ¾Ý³°ÅÀ¿ôÀßÄê½èÍý
      *****************************************************************
       10013-SYOSIN02-TEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAITEN
      *    ¿ÇÎÅ¥³¡¼¥É¤«¤éÅÀ¿ô·×»»
           IF      FLG-OK              =   1
      *        ÉÂ±¡
               MOVE    "111000110"         TO  WRK-SRYCD
               PERFORM 10011-TENSU-SET-SEC
               ADD     WRK-TENSU               TO  WRK-ZAITEN
      *
      *        ÆýÍÄ»ù²Ã»»
      *        »þ´Ö³°
               EVALUATE    WRK-KASAN
                   WHEN    "1"
      *                »þ´Ö³°
                       MOVE    "111011970"         TO  WRK-SRYCD
                   WHEN    "2"
      *                µÙÆü
                       MOVE    "111012070"         TO  WRK-SRYCD
                   WHEN    "3"
      *                ¿¼Ìë
                       MOVE    "111012170"         TO  WRK-SRYCD
                   WHEN    "4"
      *                »þ´Ö³°ÆÃÎãµÙÆü
                       MOVE    "111012270"         TO  WRK-SRYCD
                   WHEN    "5"
      *                ÆýÍÄ»ùÌë´Ö
                       MOVE    "111011570"         TO  WRK-SRYCD
                   WHEN    "6"
      *                ÆýÍÄ»ùµÙÆü
                       MOVE    "111011670"         TO  WRK-SRYCD
                   WHEN    "7"
      *                ÆýÍÄ»ù¿¼Ìë
                       MOVE    "111011770"         TO  WRK-SRYCD
                   WHEN    OTHER
      *                »þ´Ö³°¤Ê¤·
                       MOVE    "111000370"         TO  WRK-SRYCD
               END-EVALUATE
               IF      WRK-SRYCD       NOT =   SPACE
                   PERFORM 10011-TENSU-SET-SEC
                   ADD     WRK-TENSU               TO  WRK-ZAITEN
               END-IF
           END-IF
      *
           IF      WRK-HEN-SRYCD   NOT =   SPACE
               MOVE    WRK-HEN-SRYCD       TO  WRK-SRYCD
               PERFORM 10011-TENSU-SET-SEC
               ADD     WRK-TENSU               TO  WRK-ZAITEN
           END-IF
      *
      *    µëÉÕÂÐ¾Ý³°ÅÀ¿ô¡Ê»ØÆ³¡Ë
           ADD     WRK-ZAITEN         TO  SYU-KYUFUGAI-SIDOU-TEN
      *
           .
       10013-SYOSIN02-TEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÅÀ¿ôÀßÄê½èÍý
      *****************************************************************
       10011-TENSU-SET-SEC            SECTION.
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-TEN             TO  WRK-TENSU
           ELSE
               MOVE    ZERO                TO  WRK-TENSU
           END-IF
      *
           .
       10011-TENSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¤è¤êºîÀ®½èÍý
      *****************************************************************
       300-SYUNOU-GMN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOSIN-ERR
      *    ¼õ¿ÇÎò¤ò¸¡º÷¤¹¤ë
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    ORCS40-DENPNUM      TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *    ¿Ç»¡ÎÁ¤ËÆþÎÏ¤¬¤¢¤ë»þ¤Î¤ßÂÐ¾Ý¤È¤¹¤ë
           IF     (FLG-JYURRK          =   ZERO )  AND
                  (JYURRK-RENNUM       =   1    )  AND
                  (JYURRK-SRYKBN1  NOT =   SPACE)
               MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
      *        Åö·îºÇ½é¤Î½é¿ÇÎÁ»»ÄêÆü¤òÈ½Äê¤¹¤ë
               PERFORM 3000-SYOSIN-ALLCHK-SEC
      *****ELSE
      *********MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           PERFORM
                   UNTIL   (FLG-JYURRK     =   1 )  OR
                           (FLG-SYOSIN-ERR =   1 )
               PERFORM VARYING     IDX-Z   FROM    1  BY  1
                       UNTIL       IDX-Z   >   15
                   IF      JYURRK-ZAINUM (IDX-Z)  NOT =   ZERO
      *                ¿ÇÎÅ²ñ·×¼èÆÀ
                       PERFORM 3001-SRYACCT-CHK-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ¹ç·×·×»»
           COMPUTE SYU-KYUFUGAI-GOKEI-TEN  =   SYU-KYUFUGAI-SHOSHIN-TEN
                                           +   SYU-KYUFUGAI-SIDOU-TEN
                                           +   SYU-KYUFUGAI-OSIN1-TEN
                                           +   SYU-KYUFUGAI-OSIN2-TEN
                                           +   SYU-KYUFUGAI-OSIN3-TEN
      *
      *TEST
      *    DISPLAY "SYU-KYUFUGAI-SHOSHIN-TEN:" SYU-KYUFUGAI-SHOSHIN-TEN
      *    DISPLAY "SYU-KYUFUGAI-SIDOU-TEN:" SYU-KYUFUGAI-SIDOU-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN1-TEN:" SYU-KYUFUGAI-OSIN1-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN2-TEN:" SYU-KYUFUGAI-OSIN2-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN3-TEN:" SYU-KYUFUGAI-OSIN3-TEN
      *TEST
           .
       300-SYUNOU-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Åö·îºÇ½é¤Î½é¿ÇÎÁ»»ÄêÆü¤òÈ½Äê½èÍý
      *****************************************************************
       3000-SYOSIN-ALLCHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *    »»ÄêÍúÎò¸¡º÷
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYYMD (1:6)    TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
               UNTIL      (FLG-SANTEI      =   1  )  OR
                          (FLG-ERR         =   1  )
      *
               IF      SANTEI-MSANTEICNT   >   ZERO
      *            Åö·î»»ÄêÊ¬
                   PERFORM 30001-TOUGETU-CHK-SEC
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3000-SYOSIN-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Åö·î»»ÄêÊ¬¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       30001-TOUGETU-CHK-SEC            SECTION.
      *
           EVALUATE    SANTEI-SRYCD
      *        ½é¿ÇÎÁ
               WHEN    "111000110"
               WHEN    "111700110"
               WHEN    "111003610"
               WHEN    "111700210"
      *        ¾®»ù²Ê³°Íè¿ÇÎÅ²Ê
               WHEN    "113003510"
               WHEN    "113003710"
      *        ¥À¥ß¡¼
               WHEN    "099110001"
      *            ºÇ¾®¤Î»»ÄêÆüÊÔ½¸
                   IF     (WRK-DD              =   ZERO )  OR
                          (SANTEI-MSANTEID     <   WRK-DD)
                       MOVE    SANTEI-MSANTEID     TO  WRK-DD
                   END-IF
                   IF      WRK-DD              <   WRK-SRYDD
                        MOVE    1              TO  FLG-ERR
                   END-IF
           END-EVALUATE
      *
           .
       3000-SYOSIN-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       3001-SRYACCT-CHK-SEC            SECTION.
     *
           MOVE    ZERO                TO  FLG-SYORI-END
     *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM(IDX-Z)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL  (FLG-SRYACCT          =   1 )  OR
                      (FLG-SYORI-END        =   1 )
      *
               MOVE    JYURRK-SRYYMD(7:2)   TO  IDX-D2
               COMPUTE IDX-D           =   JYURRK-RENNUM   +   1
               IF     (ACCT-SRYKBN     =   "11"  OR  "13"  OR
                                           "14"  OR   "99"  ) AND
                      (ACCT-DAY (IDX-D IDX-D2)   >   ZERO   )
                   PERFORM 30011-SRYACT-SYORI-SEC
               END-IF
      *
               IF      FLG-SYORI-END       =   ZERO
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-SRYACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       30011-SRYACT-SYORI-SEC            SECTION.
     *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  WRK-KASAN
      *
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¤ß¹þ¤ß
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
                MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL       FLG-SRYACT      =   1
      *        ºÞÌÀºÙÊÔ½¸
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5        )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE)
      *            ¼êµ»ÎÁ¤Î»þ¡¢È½Äê¤¹¤ë
                   IF      SRY-SRYCD (IDZ)(1:1)    =   "1"
                       PERFORM 300111-TBL-CHK-SEC
                   END-IF
                   IF     (ACCT-SRYKBN             =   "99")  AND
                          (SRY-SRYCD (IDZ)         =   "099114001")
                       PERFORM 300111-TBL-CHK-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-OK              =   ZERO )  AND
                  (WRK-KASAN           =   SPACE)
               GO      TO      30011-SRYACT-SYORI-EXT
           ELSE
      *        ÂÐ¾Ý¤¬¤¢¤Ã¤¿»þ¡¢ÊÔ½¸¤¹¤ë
               PERFORM 30012-KYUFUGAI-HEN-SEC
           END-IF
      *
           .
       30011-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÂÐ¾Ý¿ÇÎÅ¥³¡¼¥É¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       300111-TBL-CHK-SEC            SECTION.
      *
      *    ¥³¡¼¥É¥Á¥§¥Ã¥¯
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
               AT  END
                   MOVE    ZERO            TO  FLG-OK2
               WHEN    SRY-SRYCD (IDZ)     =   TBL-SRYCD (TBL-IDX)
                   IF      WRK-KASAN           =   ZERO
                       MOVE    TBL-KASAN (TBL-IDX) TO  WRK-KASAN
                   END-IF
                   IF      TBL-DATAKBN (TBL-IDX)   =   "1"
                       MOVE    1               TO  FLG-OK
                       MOVE    TBL-IDX         TO  IDX-SYU
                   END-IF
                   MOVE    1               TO  FLG-OK2
           END-SEARCH
      *
           IF      FLG-OK2                 =   ZERO
      *        ¾Ò²ð²Ã»»¤ÎÈ½Äê
               SET     TBL2-IDX            TO  1
               SEARCH  TBL2-SYOKAI-OCC     VARYING   TBL2-IDX
                   AT  END
                       MOVE    ZERO            TO  FLG-OK2
                   WHEN    SRY-SRYCD (IDZ)     =   TBL2-SRYCD-1
                                                           (TBL2-IDX)
                        MOVE    TBL2-SRYCD-2 (TBL2-IDX)
                                                   TO  WRK-HEN-SRYCD
               END-SEARCH
           END-IF
      *
           .
       300111-TBL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    µëÉÕÂÐ¾Ý³°ÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       30012-KYUFUGAI-HEN-SEC            SECTION.
      *
      *    ÂÐ¾Ý¤Î¿Ç»¡ÎÁ¤¢¤ê
           EVALUATE    ACCT-SRYKBN  ALSO   FLG-OK
               WHEN    "11"         ALSO   1
                   MOVE    1                   TO  FLG-SYORI-END
      *            ½é¿ÇÎÁ¤Î»þ¡¢Åö·îºÇ½é¤«¤É¤¦¤«¤ÎÈ½Äê
                   IF     (WRK-DD              =   ZERO )  OR
                          (WRK-SRYDD           <=  WRK-DD)
      *                µëÉÕÂÐ¾Ý³°ÅÀ¿ô¡Ê½é¿Ç¡Ë
                       MOVE    ACCT-ZAITEN
                                           TO  SYU-KYUFUGAI-SHOSHIN-TEN
      *********    ELSE
      *********        MOVE    1           TO  FLG-SYOSIN-ERR
                   END-IF
               WHEN    "14"         ALSO   1
                   MOVE    1                   TO  FLG-SYORI-END
                   EVALUATE    WRK-KASAN
                       WHEN    "0"
      *                µëÉÕÂÐ¾Ý³°ÅÀ¿ô¡Ê±ý¿Ç¡Ë
                           MOVE    ACCT-ZAITEN
                                           TO  SYU-KYUFUGAI-OSIN1-TEN
                       WHEN    "1"
      *                µëÉÕÂÐ¾Ý³°ÅÀ¿ô¡Ê±ý¿Ç¡¿Ìë´Ö¡Ë
                           MOVE    ACCT-ZAITEN
                                           TO  SYU-KYUFUGAI-OSIN2-TEN
                       WHEN    "2"
      *                µëÉÕÂÐ¾Ý³°ÅÀ¿ô¡Ê±ý¿Ç¡¿¿¼Ìë¡¿¶ÛµÞ¡Ë
                           MOVE    ACCT-ZAITEN
                                           TO  SYU-KYUFUGAI-OSIN3-TEN
                   END-EVALUATE
               WHEN    "13"        ALSO    1
      *            ½é¿ÇÎÁ¤Î»þ¡¢Åö·îºÇ½é¤«¤É¤¦¤«¤ÎÈ½Äê
                   IF     (WRK-DD              =   ZERO )  OR
                          (WRK-SRYDD           <=  WRK-DD)
      *                ½é¿ÇÎÁÅÀ¿ô¤òµëÉÕÂÐ¾Ý³°ÅÀ¿ô¤Ø
                       PERFORM 10013-SYOSIN-TEN-SEC
                       MOVE    1                   TO  FLG-SYONIKA
                   ELSE
                       MOVE    1                   TO  FLG-SYORI-END
                   END-IF
               WHEN    "99"        ALSO    ANY
      *            ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ
      *            ½é¿ÇÎÁÅÀ¿ô¤òµëÉÕÂÐ¾Ý³°ÅÀ¿ô¤Ø
                   IF      FLG-SYONIKA         =   1
                       PERFORM 10013-SYOSIN-TEN-SEC
                   END-IF
           END-EVALUATE
      *
           .
       30012-KYUFUGAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    »»ÄêÎò¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       930-SRYACT-RAED-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-RAED-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¼õ¿ÇÍúÎò½çÆÉ¤ß
      *****************************************************************
       970-JYURRK-READ-SEC           SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
