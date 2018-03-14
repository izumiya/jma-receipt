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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSNINF03.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : APIÏ¢·ÈÍÑ¥â¥¸¥å¡¼¥ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æþ±¡¿©»ö¾ðÊó¼èÆÀ
      *  ´ÉÍý¼Ô            :
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  13/09/03    NACL¡ÝÂÀÅÄ¡¡¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXH                    PIC 9(05).
           03  IDXN                    PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-ZAISKBKBN           PIC X(01).
           03  WRK-NYUIN-DAY-G.
               05  WRK-NYUIN-DAY       PIC 9(01)
                                       OCCURS  31.
           03  WRK-NYUINRYO-DAY-G.
               05  WRK-NYUINRYO-DAY    PIC 9(01)
                                       OCCURS  31.
           03  WRK-PTSTATUS-DATA.
               05  WRK-PTSTATUS-N      PIC 9(02).
           03  WRK-DAY                 PIC 9(03).
           03  WRK-SKJ.
               05  WRK-SKJ-N           PIC 9(02).
           03  WRK-SKJ-NAME            PIC X(30).
           03  WRK-NUM7Z               PIC ZZZZZZ9.
           03  WRK-CALENDAR-MAX        PIC 9(03).
      *
       01  KEY-AREA.
           03  KEY-SRYYM               PIC X(06).
      *
       01  TCOMB-AREA.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT//
                                       BY          //TCOMB//.
      *
       01  TNACCT-AREA.
         02    TNACCT-OCC                OCCURS    4.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT//
                                       BY          //TNACCT//.
      *
       01  TGAIHAKU-AREA.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT//
                                       BY          //TGAIHAKU//.
      *
       01  CONST-AREA.
           03  CONST-H3004             PIC X(06)   VALUE "201804".
           03  CONST-H2804             PIC X(06)   VALUE "201604".
           03  CONST-H200401           PIC X(08)   VALUE "20080401".
           03  CONST-ASA               PIC 9(03)   VALUE 1.
           03  CONST-HIRU              PIC 9(03)   VALUE 2.
           03  CONST-YU                PIC 9(03)   VALUE 3.
           03  CONST-GAIHAKU           PIC 9(03)   VALUE 4.
           03  CONST-HKN-MAX           PIC 9(03)   VALUE 10.
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
           COPY    "CPORCSKANACHK.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *
      *    ÆþÂà±¡Æü¼èÆÀ¥µ¥Ö¥Ñ¥é¥á¥¿
           COPY    "CPORCSNTYMD.INC".
      *
      *    ²ÊÊÌ¡¦ÊÝ¸±ÊÌÆþ±¡²ñ·×ÊÔ½¸¥µ¥Ö
           COPY    "CPORCSS009.INC".
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥»¥Ã¥ÈÎÎ°è
           COPY    "CPORCSAPIHKN.INC".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSYSKANRI.INC".
      *    ¿¦°÷¾ðÊó
           COPY    "CPSK1010.INC".
      *    Æþ±¡´ðËÜ
           COPY    "CPSK5000.INC".
      *    ÉÂÅï
           COPY    "CPSK5001.INC".
      *    ÉÂ¼¼
           COPY    "CPSK5002.INC".
      *    ¿ÇÎÅ²Ê¾ðÊó
           COPY    "CPSK1005.INC".
      *    ¼¼ÎÁº¹³Û
           COPY    "CPSK5113.INC".
      *
      *    ´µ¼Ô¾ðÊó
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    Æþ±¡¿ÇÎÅ²ñ·×
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    ÊÝ¸±ÁÈ¹ç¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ï¢ÍíÎÎ°è
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPORCSNINF03.INC".
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SNINF03-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  TCOMB-AREA
           INITIALIZE                  TNACCT-AREA
           INITIALIZE                  TGAIHAKU-AREA
      *
      *    ½é´ü½èÍý
           PERFORM 100-INIT-SEC
      *
      *    ¼ç½èÍý
           IF    ( WRK-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    ÊÖµÑÎÎ°èÊÔ½¸
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    SNINF03-IN-SRYYM    TO  KEY-SRYYM
      *
           INITIALIZE                      ORCSNTYMDAREA
           MOVE    SPA-HOSPNUM         TO  SNTYMD-HOSPNUM
           MOVE    SPA-PTID            TO  SNTYMD-PTID
           MOVE    "2"                 TO  SNTYMD-KBN
           CALL    "ORCSNTYMD" USING   ORCSNTYMDAREA
                                           SPA-AREA
      *
           MOVE    KEY-SRYYM   TO  WRK-SRYYMD-YM
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               MOVE    IDXD    TO  WRK-SRYYMD-DD
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SNTYMD-MAX )
                   IF    ( WRK-SRYYMD  >=  SNTYMD-NYUINYMD (IDX0))
                    AND  ( WRK-SRYYMD  <=  SNTYMD-TAIINYMD (IDX0))
                       MOVE    1   TO  WRK-NYUIN-DAY (IDXD)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           MOVE    "5"                 TO  WRK-ZAISKBKBN
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
           MOVE    NYUINACCT-REC       TO  TCOMB-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ¼ç¡¡½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-PTINF-EDIT-SEC
      *
           PERFORM 2001-NACCT-EDIT-SEC
      *
           PERFORM 2001-HKN-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÊóÊÔ½¸½èÍý
      *****************************************************************
       2001-PTINF-EDIT-SEC                 SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPA-PTNUM           TO  SNINF03-PTNUM
           MOVE    PTINF-NAME          TO  SNINF03-NAME
           MOVE    PTINF-KANANAME      TO  SNINF03-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  SNINF03-BIRTHDAY
           MOVE    PTINF-SEX           TO  SNINF03-SEX
      *
          .
       2001-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       2001-NACCT-EDIT-SEC                 SECTION.
      *
           MOVE    ZERO            TO      IDXA
           PERFORM VARYING IDX0    FROM    SNTYMD-MAX   BY  -1
                   UNTIL ( IDXA    >   5 )
                    OR   ( IDX0    <   1)
      *
               IF    ( KEY-SRYYM   >=  SNTYMD-NYUINYMD (IDX0)(1:6))
                AND  ( KEY-SRYYM   <=  SNTYMD-TAIINYMD (IDX0)(1:6))
                   COMPUTE IDXA    =   IDXA    +   1
                   MOVE    SNTYMD-NYUINYMD (IDX0)
                                   TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE    TO  SNINF03-NYUINYMD (IDXA)
      *
                   MOVE    SNTYMD-TAIINYMD (IDX0)
                                       TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE    TO  SNINF03-TAIINYMD (IDXA)
               END-IF
      *
           END-PERFORM
      *
           MOVE    KEY-SRYYM           TO  WRK-SYMD
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE (1:7)      TO  SNINF03-SRYYM
      *
           PERFORM 2001-SKJ-GAIHAKU-EDIT-SEC
      *
           MOVE    ZERO            TO  IDXA
      *
           INITIALIZE                  SS009-AREA
           MOVE    "1"             TO  SS009-EXGAIHAKU
           CALL    "ORCSS009"  USING   SS009-AREA
                                       TCOMB-AREA
                                       SPA-AREA
           IF    ( SS009-RC        =   ZERO )
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   IF    ( WRK-NYUIN-DAY (IDXD)    >   ZERO )
                    AND  ( TCOMB-DAY     (IDXD)    >   ZERO )
                       PERFORM 20011-NACCT-EDIT-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
          .
       2001-NACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       20011-NACCT-EDIT-SEC                SECTION.
      *
           COMPUTE IDXA    =   IDXA    +   1
      *
           MOVE    IDXA            TO  WRK-CALENDAR-MAX
      *
           MOVE    KEY-SRYYM       TO  WRK-SRYYMD-YM
           MOVE    IDXD            TO  WRK-SRYYMD-DD
      *
           MOVE    WRK-SRYYMD      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  SNINF03-SRYYMD (IDXA)
      *
           MOVE    "¿ÇÎÅ²Ê"        TO  SNINF03-SRYKA-LBL  (IDXA)
           MOVE    SS009-SRYKA-DAY (IDXD)
                                   TO  SNINF03-SRYKA-DATA (IDXA)
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    SS009-SRYKA-DAY (IDXD)
                                   TO  SYS-KBNCD
           MOVE    WRK-SRYYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1005-REC
           MOVE    SYS-1005-SRYKANAME
                                   TO  SNINF03-SRYKA-NAME (IDXA)
      *
           MOVE    "ÉÂÅïÈÖ¹æ"      TO  SNINF03-BTUNUM-LBL (IDXA)
           MOVE    SS009-BRMNUM-DAY(IDXD)(1:2)
                                   TO  SNINF03-BTUNUM-DATA(IDXA)
      *
           MOVE    "ÉÂÅïÌ¾"        TO  SNINF03-BTUNAME-LBL(IDXA)
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SS009-BRMNUM-DAY(IDXD)(1:2)
                                   TO  SYS-KBNCD
           MOVE    WRK-SRYYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
           MOVE    SYS-5001-BTU-NAME
                                   TO  SNINF03-BTUNAME-DATA (IDXA)
      *
           MOVE    "ÉÂ¼¼ÈÖ¹æ"      TO  SNINF03-BRMNUM-LBL   (IDXA)
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    SS009-BRMNUM-DAY(IDXD)
                                   TO  SYS-KBNCD
           MOVE    WRK-SRYYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5002-REC
           MOVE    SYS-5002-BRM-NUM
                                   TO  SNINF03-BRMNUM-DATA  (IDXA)
      *
           MOVE    SS009-HKNCOMBI-DAY (IDXD)
                                   TO  SNINF03-HKNCOMBI (IDXA)
      *
           MOVE    "³°Çñ¡¦Â¾°åÎÅµ¡´Ø¼õ¿Ç¾ðÊóÅù"
                                   TO  SNINF03-PTSTATUS-LBL (IDXA)
           MOVE    TNACCT-DAY (CONST-GAIHAKU IDXD)
                                   TO  WRK-PTSTATUS-N
           MOVE    WRK-PTSTATUS-N  TO  SNINF03-PTSTATUS-DATA (IDXA)
      *
           EVALUATE    TRUE
           WHEN  ( KEY-SRYYM       >=  CONST-H2804 )
               EVALUATE    TNACCT-DAY (CONST-GAIHAKU IDXD)
               WHEN    ZERO
                   MOVE    "Æþ±¡Ãæ"    TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    1
                   MOVE    "³°Çñ"      TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    2
                   MOVE    "¼£ÎÅ¤Î°Ù¤Î³°Çñ"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    3
                   MOVE    "ÁªÄêÆþ±¡Ãæ¤Î³°Çñ"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    4
                   MOVE    "Â¾°åÎÅµ¡´Ø¼õ¿Ç£´£°¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    5
                   MOVE    "Â¾°åÎÅµ¡´Ø¼õ¿Ç£±£°¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    6
                   MOVE    "Â¾°åÎÅµ¡´Ø¼õ¿Ç£²£°¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    8
                   MOVE    "ÆÃÄê»þ´ÖÂà±¡¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    9
                   MOVE    "ÆÃÄêÍËÆüÆþÂà±¡¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    13
                   MOVE   "ÆÃÄêÍËÆüÆþÂà±¡¸º»»¡ÜÂ¾°åÎÅµ¡´Ø¼õ¿Ç£±£°¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               END-EVALUATE
      *
               IF  ( KEY-SRYYM         >=  CONST-H3004 )
                   EVALUATE    TNACCT-DAY (CONST-GAIHAKU IDXD)
                   WHEN    14
                       MOVE    "Â¾°åÎÅµ¡´Ø¼õ¿Ç£µ¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
                   WHEN    15
                       MOVE    "Â¾°åÎÅµ¡´Ø¼õ¿Ç£³£µ¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
                   WHEN    16
                       MOVE    "Â¾°åÎÅµ¡´Ø¼õ¿Ç£±£µ¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
                   WHEN    17
                       MOVE "ÆÃÄêÍËÆüÆþÂà±¡¸º»»¡ÜÂ¾°åÎÅµ¡´Ø¼õ¿Ç£µ¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
                   END-EVALUATE
               END-IF
           WHEN    OTHER
               EVALUATE    TNACCT-DAY (CONST-GAIHAKU IDXD)
               WHEN    ZERO
                   MOVE    "Æþ±¡Ãæ"    TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    1
                   MOVE    "³°Çñ"      TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    2
                   MOVE    "¼£ÎÅ¤Î°Ù¤Î³°Çñ"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    3
                   MOVE    "ÁªÄêÆþ±¡Ãæ¤Î³°Çñ"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    4
                   MOVE    "Â¾°åÎÅµ¡´Ø¼õ¿Ç£·£°¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    5
                   MOVE    "Â¾°åÎÅµ¡´Ø¼õ¿Ç£³£°¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    6
                   MOVE    "Â¾°åÎÅµ¡´Ø¼õ¿Ç£µ£µ¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    7
                   MOVE    "Â¾°åÎÅµ¡´Ø¼õ¿Ç£±£µ¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    8
                   MOVE    "ÆÃÄê»þ´ÖÂà±¡¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    9
                   MOVE    "ÆÃÄêÍËÆüÆþÂà±¡¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               WHEN    13
                   MOVE  "ÆÃÄêÍËÆüÆþÂà±¡¸º»»¡ÜÂ¾°åÎÅµ¡´Ø¼õ¿Ç£³£°¡ó¸º»»"
                                       TO  SNINF03-PTSTATUS-NAME (IDXA)
               END-EVALUATE
           END-EVALUATE
      *
           MOVE    "Ä«¿©"              TO  SNINF03-SKJ-ASA-LBL   (IDXA)
           MOVE    TNACCT-DAY (CONST-ASA IDXD)
                                       TO  WRK-SKJ-N
           MOVE    WRK-SKJ-N           TO  SNINF03-SKJ-ASA-DATA  (IDXA)
           MOVE    TNACCT-DAY (CONST-ASA IDXD)
                                       TO  WRK-DAY
           PERFORM 200111-SKJ-EDIT-SEC
           MOVE    WRK-SKJ-NAME        TO  SNINF03-SKJ-ASA-NAME  (IDXA)
      *
           MOVE    "Ãë¿©"              TO  SNINF03-SKJ-HIRU-LBL  (IDXA)
           MOVE    TNACCT-DAY (CONST-HIRU IDXD)
                                       TO  WRK-SKJ-N
           MOVE    WRK-SKJ-N           TO  SNINF03-SKJ-HIRU-DATA (IDXA)
           MOVE    TNACCT-DAY (CONST-HIRU IDXD)
                                       TO  WRK-DAY
           PERFORM 200111-SKJ-EDIT-SEC
           MOVE    WRK-SKJ-NAME        TO  SNINF03-SKJ-HIRU-NAME (IDXA)
      *
           MOVE    "Í¼¿©"              TO  SNINF03-SKJ-YU-LBL   (IDXA)
           MOVE    TNACCT-DAY (CONST-YU IDXD)
                                       TO  WRK-SKJ-N
           MOVE    WRK-SKJ-N           TO  SNINF03-SKJ-YU-DATA  (IDXA)
           MOVE    TNACCT-DAY (CONST-YU IDXD)
                                       TO  WRK-DAY
           PERFORM 200111-SKJ-EDIT-SEC
           MOVE    WRK-SKJ-NAME        TO  SNINF03-SKJ-YU-NAME  (IDXA)
      *
           MOVE    "¼¼ÎÁº¹³Û"          TO  SNINF03-SAGAKU-LBL   (IDXA)
           MOVE    "±ß"                TO  SNINF03-SAGAKU-NAME  (IDXA)
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5113"              TO  SYS-KANRICD
           MOVE    SS009-SAGAKUCD-DAY (IDXD)
                                       TO  SYS-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5113-REC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SYS-5113-BRM-SAGAKU-NM
                                       TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z       TO  SNINF03-SAGAKU-DATA (IDXA)
           ELSE
               MOVE    ZERO            TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z       TO  SNINF03-SAGAKU-DATA (IDXA)
           END-IF
      *
          .
       20011-NACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»ö³°Çñ¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       2001-SKJ-GAIHAKU-EDIT-SEC       SECTION.
      *
           PERFORM 900-NYUINACCT-KEY51-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               MOVE    ZERO                TO  IDXN
      *
               EVALUATE    NACCT-ZAISKBKBN
               WHEN    "2"
                   MOVE    CONST-GAIHAKU   TO  IDXN
               WHEN    "4"
                   EVALUATE    NACCT-SRYCDTOTAL
                   WHEN    099999918
                       MOVE    CONST-ASA   TO  IDXN
                   WHEN    099999919
                       MOVE    CONST-HIRU  TO  IDXN
                   WHEN    099999920
                       MOVE    CONST-YU    TO  IDXN
                   END-EVALUATE
               END-EVALUATE
      *
               IF    ( IDXN            >   ZERO )
                   MOVE    NYUINACCT-REC   TO  TNACCT-OCC (IDXN)
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY51-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key51"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    TNACCT-OCC (CONST-GAIHAKU)
                                   TO  TGAIHAKU-AREA
           MOVE    ZERO            TO  TNACCT-ZAIKAISU (CONST-ASA)
                                       TNACCT-ZAIKAISU (CONST-HIRU)
                                       TNACCT-ZAIKAISU (CONST-YU)
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    CONST-ASA   TO  IDXN
               PERFORM 20011-SKJ-GAIHAKU-EDIT-SEC
      *
               MOVE    CONST-HIRU  TO  IDXN
               PERFORM 20011-SKJ-GAIHAKU-EDIT-SEC
      *
               MOVE    CONST-YU    TO  IDXN
               PERFORM 20011-SKJ-GAIHAKU-EDIT-SEC
      *
           END-PERFORM
      *
          .
       2001-SKJ-GAIHAKU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»ö³°Çñ¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       20011-SKJ-GAIHAKU-EDIT-SEC          SECTION.
      *
           IF    ( TGAIHAKU-DAY (IDXD)     =   ZERO )
             OR  ( TGAIHAKU-DAY (IDXD)    >=   4 )
               IF    ( TNACCT-DAY (IDXN IDXD)      >   ZERO )
                   COMPUTE TNACCT-ZAIKAISU (IDXN)
                       =   TNACCT-ZAIKAISU (IDXN)  +   1
               END-IF
           ELSE
               MOVE    ZERO    TO  TNACCT-DAY (IDXN IDXD)
           END-IF
      *
          .
       20011-SKJ-GAIHAKU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»ö¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       200111-SKJ-EDIT-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-SKJ-NAME
      *
           IF    ( KEY-SRYYM       >=  CONST-H2804 )
               EVALUATE    WRK-DAY
               WHEN    ZERO
                   MOVE    "¿©»ö¤Ê¤·"  TO  WRK-SKJ-NAME
               WHEN    1
                   MOVE    "¿©»ö¤¢¤ê"  TO  WRK-SKJ-NAME
               WHEN    2
                   MOVE    "¿©»ö¤¢¤ê¡ÊÆÃÊÌ¿©¡Ë"
                                       TO  WRK-SKJ-NAME
               WHEN    3
                   MOVE    "¿©»ö¤¢¤ê¡ÊÎ®Æ°¿©¡Ë"
                                       TO  WRK-SKJ-NAME
               END-EVALUATE
           ELSE
               EVALUATE    WRK-DAY
               WHEN    ZERO
                   MOVE    "¿©»ö¤Ê¤·"  TO  WRK-SKJ-NAME
               WHEN    1
                   MOVE    "¿©»ö¤¢¤ê"  TO  WRK-SKJ-NAME
               WHEN    2
                   MOVE    "¿©»ö¤¢¤ê¡ÊÆÃÊÌ¿©¡Ë"
                                       TO  WRK-SKJ-NAME
               END-EVALUATE
           END-IF
      *
          .
       200111-SKJ-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       2001-HKN-EDIT-SEC               SECTION.
      *
           MOVE    ZERO                TO  IDXH
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   WRK-CALENDAR-MAX )
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-HKN-MAX )
                        OR   ( IDXH    >=  CONST-HKN-MAX )
      *
                   IF    ( IDX0        >   IDXH )
                       COMPUTE IDXH    =   IDXH    +   1
                       MOVE    SNINF03-HKNCOMBI (IDXA)
                                       TO  SNINF03-HKNCOMBI-KEY (IDXH)
                       MOVE    CONST-HKN-MAX
                                       TO  IDX0
                   ELSE
                       IF   (  SNINF03-HKNCOMBI (IDXA)
                                        =  SNINF03-HKNCOMBI-KEY (IDX0))
                           MOVE    CONST-HKN-MAX
                                       TO  IDX0
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    SPA-AREA    TO  TSPA-AREA
           MOVE    SPA-SRYYMD  TO  SPA-SYSYMD
           INITIALIZE              ORCSAPIHKNAREA
           MOVE    "2"         TO  ORCSAPIHKN-KBN2
           CALL    "ORCSAPIHKN"    USING
                                   SPA-AREA
                                   ORCSAPIHKNAREA
           MOVE    TSPA-AREA   TO  SPA-AREA
      *
           PERFORM VARYING IDXH    FROM    1   BY  1
                   UNTIL ( IDXH    >   CONST-HKN-MAX )
                    OR   ( SNINF03-HKNCOMBI-KEY (IDXH)
                                   =   SPACE )
               PERFORM VARYING IDXA    FROM    1   BY  1
                       UNTIL ( IDXA    >   20 )
                         OR  ( ORCSAPIHKN-THKNCOMBI (IDXA)
                                       =   SPACE  )
                   IF    ( SNINF03-HKNCOMBI-KEY (IDXH)
                                       =   ORCSAPIHKN-THKNCOMBI (IDXA))
                       PERFORM 20011-HKN-EDIT-SEC
                       MOVE    20      TO  IDXA
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
          .
       2001-HKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       20011-HKN-EDIT-SEC              SECTION.
      *
      *    ÊÝ¸±¤Î¼ïÎà
           MOVE    ORCSAPIHKN-HKNNUM (IDXA)
                                   TO  SNINF03-HKNNUM   (IDXH)
      *
      *    ÊÝ¸±¼ÔÈÖ¹æ
           MOVE    ORCSAPIHKN-HKNJANUM (IDXA)
                                   TO  SNINF03-HKNJANUM (IDXH)
      *
      *    ÊÝ¸±¤ÎÌ¾¾Î
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDXA)
                                   TO  SNINF03-HKNJANAME(IDXH)
      *
      *    µ­¹æ
           MOVE    ORCSAPIHKN-KIGO  (IDXA)
                                   TO  SNINF03-HKIGO (IDXH)
      *
      *    ÈÖ¹æ
           MOVE    ORCSAPIHKN-NUM  (IDXA)
                                   TO  SNINF03-HNUM  (IDXH)
      *
      *    ¸øÈñ¾ðÊó
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   4 )
      *
      *        ¸øÈñ¤Î¼ïÎà
               MOVE    ORCSAPIHKN-KOHNUM (IDXA IDXK)
                                   TO  SNINF03-KOHNUM  (IDXH IDXK)
      *        ¸øÈñ¤Î¼ïÎàÌ¾¾Î
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDXA IDXK)
                                   TO  SNINF03-KOHNAME (IDXH IDXK)
      *        ÉéÃ´¼ÔÈÖ¹æ
               MOVE    ORCSAPIHKN-FTNJANUM (IDXA IDXK)
                                   TO  SNINF03-FTNJANUM(IDXH IDXK)
      *        ¼õµë¼ÔÈÖ¹æ
               MOVE    ORCSAPIHKN-JKYSNUM (IDXA IDXK)
                                   TO  SNINF03-JKYSNUM (IDXH IDXK)
           END-PERFORM
      *
          .
       20011-HKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆüÉÕÊÔ½¸½èÍý
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    ÆüÉÕÊÔ½¸½èÍý
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    ÆüÉÕÊÔ½¸½èÍý
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    »þ¹ïÊÔ½¸½èÍý
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
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
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¾ðÊó¼èÆÀ½èÍý
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¸¡º÷½èÍý
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    KEY-SRYYM           TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    WRK-ZAISKBKBN       TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¸¡º÷½èÍý
      *****************************************************************
       901-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    KEY-SRYYM           TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    WRK-ZAISKBKBN       TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       901-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×FETCH½èÍý
      *****************************************************************
       901-NYUINACCT-KEY17-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       901-NYUINACCT-KEY17-FET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¸¡º÷½èÍý
      *****************************************************************
       900-NYUINACCT-KEY51-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    KEY-SRYYM           TO  NACCT-SRYYM
                                           NACCT-CREYMD
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key51"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY51-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×FETCH½èÍý
      *****************************************************************
       900-NYUINACCT-KEY51-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key51"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY51-FET-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
