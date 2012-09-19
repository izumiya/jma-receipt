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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCHCM34S01.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æþ±¡²ñ·×¥«¡¼¥É¾ðÊóÊÔ½¸¥µ¥Ö
      *                      ¡Ê²ñ·×¥«¡¼¥É¤Î¥µ¥Ö£Ð£Ç¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/08/18    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL¡ÝÂÀÅÄ   04/02/02  ¼¼ÎÁº¹³Û¡¦¿©»ö¤Î»»Äê¤ò
      *                                     »ÃÄêÅª¤Ë½¤Àµ
      *  01.00.02    NACL¡ÝÂÀÅÄ   04/03/03  °åÎÅµ¡´Ø¸º»»ÂÐ±þ
      *  01.00.03    NACL¡ÝÂÀÅÄ   04/03/11  Í­¾²¿ÇÎÅ½ê£±·²Æþ±¡´ðËÜÎÁ£±
      *                                     ¡Ê°å»ÕÇÛÃÖÅ¬¹ç¡ËÂÐ±þ
      *  01.00.04    NACL¡ÝÂÀÅÄ   04/08/23  Æþ±¡²ñ·×¼¼ÎÁº¹³Û¶èÊ¬
      *  00.00.05    NACL-¾®Æ¦Âô  05/09/16  °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤Î³°Çñ·×»»¤ò½¤Àµ
      *                                     ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *  01.00.06    NACL-¾®Æ¦Âô  06/03/22  ÆÃÄêÆþ±¡ÎÁÄÉ²ÃÊ¬¡ÊÇ¾Â´Ãæ¥±¥¢¥æ¥Ë¡Ë
      *                                     ³°ÇñÃÖ¤­´¹¤¨½èÍý
      *  01.00.07    NACL-¾®Æ¦Âô  06/05/23  ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ½¤Àµ
      *  03.01.01    NACL-ÌçÏÆ    06/06/30  ÎÅÍÜÉÂÅï¤ÎÂ¾°åÎÅµ¡´Ø¼õ¿ÇÂÐ±þ
      *  03.05.00    NACL-ÂÀÅÄ    07/05/28  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.01.00    NACL-ÂÀÅÄ    07/11/20  ¼¼ÎÁº¹³Û£¶·åÂÐ±þ
      *  04.01.00    NACL-ÂÀÅÄ    07/12/19  Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤ÎÈ½ÄêÄÉ²Ã
      *****************************************************************
      *
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
      *FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-OVER                        PIC 9(01).
           03  FLG-HIT                         PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-NYUINACT                    PIC 9(01).
           03  FLG-SENTEICDCHG                 PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-TAISYO                      PIC 9(01).
           03  FLG-NYUINARI                    PIC 9(01).
           03  FLG-KIKAN-G .
               05  FLG-KIKANDAY                PIC 9(01)
                                               OCCURS  31.
           03  FLG-NYUINDAY-G.
               05  FLG-NYUINDAY                PIC 9(01)
                                               OCCURS  31.
           03  FLG-NYUINRYODAY-G.
               05  FLG-NYUINRYODAY             PIC 9(01)
                                               OCCURS  31.
           03  FLG-SENTEIDAY-G.
               05  FLG-SENTEIDAY               PIC 9(01)
                                               OCCURS  31.
           03  FLG-TOKTEIDAY-G.
               05  FLG-TOKTEIDAY               PIC 9(01)
                                               OCCURS  31.
           03  FLG-GAIHAKUDAY-G.
               05  FLG-GAIHAKUDAY              PIC 9(01)
                                               OCCURS  31.
           03  FLG-SYOKUDODAY-G.
               05  FLG-SYOKUDODAY              PIC 9(01)
                                               OCCURS  31.
           03  FLG-NYUINGENDAY-G.
               05  FLG-NYUINGENDAY-OCC         OCCURS  4.
                   07  FLG-NYUINGENDAY         PIC 9(01)
                                               OCCURS  31.
           03  FLG-BRMSGKSANTEIDAY-G.
               05  FLG-BRMSGKSANTEIDAY         PIC 9(01)
                                               OCCURS  31.
           03  FLG-SANTEI                      PIC 9(01).
           03  FLG-CLEAR                       PIC 9(01).
           03  FLG-EXSYOKUJI                   PIC 9(01).
           03  FLG-SKJZAIPTN                   PIC 9(01).
           03  FLG-SKJFTN                      PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-SYS5000                     PIC 9(05).
           03  CNT-KANKYO-DAY-G.
               05  CNT-KANKYO-DAY-TOTAL        PIC 9(05).
               05  CNT-KANKYO-DAY              PIC 9(01)
                                               OCCURS  31.
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDX1                            PIC 9(05).
           03  IDX2                            PIC 9(05).
           03  IDX3                            PIC 9(05).
           03  IDX4                            PIC 9(05).
           03  IDX5                            PIC 9(05).
           03  IDX6                            PIC 9(05).
           03  IDX7                            PIC 9(05).
           03  IDX8                            PIC 9(05).
           03  IDX9                            PIC 9(05).
           03  IDXA                            PIC 9(05).
           03  IDXB                            PIC 9(05).
           03  IDXC                            PIC 9(05).
           03  IDXD                            PIC 9(05).
           03  IDXE                            PIC 9(05).
           03  IDXF                            PIC 9(05).
           03  IDXG                            PIC 9(05).
           03  IDXH                            PIC 9(05).
           03  IDX-KHN                         PIC 9(05).
           03  IDX-TAX                         PIC 9(05).
           03  IDX-SKJ                         PIC 9(05).
           03  IDX-WSYOKUJI                    PIC 9(05).
           03  IDX-KANKYO                      PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-YMD.
               05  WRK-YYMM                    PIC X(06).
               05  WRK-DD                      PIC 9(02).
           03  WRK-DBYMD                       PIC X(08).
           03  WRK-TNS-SRYCD                   PIC X(09).
           03  WRK-GAIHAKU-SRYCD               PIC X(09).
           03  WRK-GAIHAKU-KHNTEN              PIC 9(08).
           03  WRK-GAIHAKU-KSNTEN              PIC 9(08).
           03  WRK-NYUKHNKSNKBN                PIC 9(03).
           03  WRK-CHUKSNCD                    PIC X(04).
           03  WRK-NACCT-ZAISKBKBN             PIC X(01).
           03  WRK-KHNTEN                      PIC S9(08).
           03  WRK-KSNTEN                      PIC S9(08).
           03  WRK-KHNSRYCD                    PIC X(09).
           03  WRK-KSNSRYCD-G.
               05  WRK-KSNSRYCD-MAX            PIC 9(03).
               05  WRK-KSNSRYCD                PIC X(09)
                                               OCCURS   5.
           03  WRK-HYOKETU-TEN                 PIC S9(08).
           03  WRK-HYOKETU-SRYCD               PIC X(09).
           03  WRK-SENTEI-TEN                  PIC S9(08).
           03  WRK-SENTEI-SRYCD                PIC X(09).
           03  WRK-SENTEI-ZAITEN               PIC  9(08).
           03  WRK-SENTEI-KHNTEN               PIC 9(08).
           03  WRK-SENTEISRYCD                 PIC X(09).
           03  WRK-SRYCDTOTAL                  PIC 9(09).
           03  WRK-SENTEI-KEISU                PIC 9V99.
           03  WRK-BRMSAGAKU                   PIC 9(07).
           03  WRK-BRMSAGAKUCD                 PIC X(02).
           03  WRK-SKJ-TEN-G.
               05  WRK-SKJ-TEN-RYOYO1          PIC 9(08).
               05  WRK-SKJ-TEN-RYOYO2          PIC 9(08).
               05  WRK-SKJ-TEN-TOKKANRI        PIC 9(08).
               05  WRK-SKJ-TEN-TOKSYOKU        PIC 9(08).
               05  WRK-SKJ-TEN-SYOKUDO         PIC 9(08).
               05  WRK-SKJ-TEN-FKSKONDATE      PIC 9(08).
               03  WRK-SKJ-TEN-SKJFTN-IPN      PIC 9(08).
               03  WRK-SKJ-TEN-SKJFTN-TEI2     PIC 9(08).
               03  WRK-SKJ-TEN-SKJFTN-TEI2-90  PIC 9(08).
               03  WRK-SKJ-TEN-SKJFTN-TEI1     PIC 9(08).
           03  WRK-LIFE-TEN-G.
               05  WRK-LIFE-TEN-RYOYO1         PIC 9(08).
               05  WRK-LIFE-TEN-RYOYO2         PIC 9(08).
               05  WRK-LIFE-TEN-TOKSYOKU       PIC 9(08).
               05  WRK-LIFE-TEN-SYOKUDO        PIC 9(08).
               05  WRK-LIFE-TEN-KANKYO1        PIC 9(08).
               05  WRK-LIFE-TEN-KANKYO2        PIC 9(08).
           03  WRK-SYS-5113.
               05  WRK-SYS-5113-MAX            PIC 9(03).
               05  WRK-SYS-5113-OCC            OCCURS  30.
                   07  WRK-SYS-5113-STYUKYMD   PIC X(08).
                   07  WRK-SYS-5113-EDYUKYMD   PIC X(08).
                   07  WRK-SYS-5113-KBNCD      PIC 9(02).
                   07  WRK-SYS-5113-BRMSAGAKU  PIC 9(07).
           03  WRK-SRYCD-NUM                   PIC 9(09).
           03  WRK-SKJ-SRYCD-G.
               05  WRK-SKJ-ZAITEN              PIC 9(08).
               05  WRK-SKJ-SRYCDTOTAL          PIC 9(14).
               05  WRK-SKJ-SRYCD               PIC X(09)   OCCURS  5.
           03  WRK-SYS-MIJISI-G.
               05  WRK-SYS-MIJISI-OCC                      OCCURS  4.
                   07  WRK-SYS-MIJISI          PIC 9(01).
                   07  WRK-SYS-MIJISI-ZAITEN   PIC 9(08).
           03  WRK-SHOKUJIRYOYO                PIC 9(01).
           03  WRK-TOKUKANRIKASAN-G.
               05  WRK-TOKUKANRIKASAN          PIC 9(01)   OCCURS  31.
           03  WRK-SENTEI-PTFTN-G.
               05  WRK-SENTEI-PTFTN            PIC 9(08)   OCCURS  2.
           03  WRK-SENTEI-GAIHAKU1             PIC 9(08).
           03  WRK-SENTEI-GAIHAKU2             PIC 9(08).
           03  WRK-SENTEI-FTNKBN               PIC X(01).
           03  WRK-SENTEI-TANKA-X              PIC  X(02).
           03  WRK-SENTEI-TANKA-R              REDEFINES
               WRK-SENTEI-TANKA-X.
               05  WRK-SENTEI-TANKA            PIC  9(02).
           03  WRK-SENTEI-KINGAKU-X            PIC  X(04).
           03  WRK-SENTEI-KINGAKU-R            REDEFINES
               WRK-SENTEI-KINGAKU-X.
               05  WRK-SENTEI-KINGAKU          PIC  9(04).
           03  WRK-STIDX                       PIC  9(02).
           03  WRK-EDIDX                       PIC  9(02).
           03  WRK-SRYCD-G.
               05  WRK-SRYCD                   PIC  X(09).
               05  WRK-SRYCD-9                 REDEFINES WRK-SRYCD
                                               PIC  9(09).
           03  WRK-SRYYMD                      PIC  X(08).
           03  WRK-SKYSTYMD.
               05  WRK-SKYSTYM                 PIC  X(06).
               05  WRK-SKYSTDD                 PIC  9(02).
           03  WRK-SKYEDYMD.
               05  WRK-SKYEDYM                 PIC  X(06).
               05  WRK-SKYEDDD                 PIC  9(02).
           03  WRK-SKJCOMMENTCD                PIC  X(09).
           03  WRK-ZAI-G.
               05  WRK-ZAI-CNT                 PIC  9(08).
               05  WRK-ZAI-SRYCDTOTAL          PIC  9(14).
               05  WRK-ZAI-TEN                 PIC  9(08).
               05  WRK-ZAI-KHNSRYCD            PIC  X(09).
               05  WRK-ZAI-KHNTEN              PIC  9(08).
               05  WRK-ZAI-SRYCD               PIC  X(09)
                                               OCCURS  5.
           03  WRK-DOJITU-G.
               05  WRK-DOJITU-OCC              OCCURS  31.
                   07  WRK-DOJITUST            PIC  X(01).
                   07  WRK-DOJITUED            PIC  X(01).
      *
      *    Æþ±¡ÎÁ
       01  WNYUINRYO-AREA.
           03  WNYUINRYO-OCC1                  OCCURS 02.
               05  WNYUINRYO-OCC2-MAX          PIC 9(05).
               05  WNYUINRYO-OCC2              OCCURS 20.
                   07  WNYUINRYO-ZAINUM        PIC 9(08).
      *            Æþ±¡´ðËÜÎÁ¿ÇÎÅ¶èÊ¬
                   07  WNYUINRYO-SRYKBN        PIC X(02).
      *            ºÞÅÀ¿ô
                   07  WNYUINRYO-ZAITEN        PIC 9(08).
      *            ºÞ²ó¿ô
                   07  WNYUINRYO-ZAIKAISU      PIC S9(07).
      *            ¿ÇÎÅ¥³¡¼¥É
                   07  WNYUINRYO-SRYCD         PIC X(09)   OCCURS  5.
      *            ¿ÇÎÅ²ó¿ô¥Æ¡¼¥Ö¥ë
                   07  WNYUINRYO-DAY-TBL.
                       09  WNYUINRYO-DAY       PIC 9(03)   OCCURS  31.
      *
      *    ³°Çñ
       01  WGAIHAKU-AREA.
           03  WGAIHAKU-OCC1                   OCCURS 02.
               05  WGAIHAKU-OCC2-MAX           PIC 9(05).
               05  WGAIHAKU-OCC2               OCCURS 20.
      *            ¥½¡¼¥ÈÍÑÈÖ¹æ
                   07  WGAIHAKU-NUM            PIC 9(03).
      *                ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¹ç·×
                   07  WGAIHAKU-SRYCDTOTAL     PIC 9(14).
      *            Æþ±¡´ðËÜÎÁ¿ÇÎÅ¶èÊ¬
                   07  WGAIHAKU-SRYKBN         PIC X(02).
      *            ºÞÅÀ¿ô
                   07  WGAIHAKU-ZAITEN         PIC 9(08).
      *            ºÞ²ó¿ô
                   07  WGAIHAKU-ZAIKAISU       PIC 9(07).
      *            ¿ÇÎÅ¥³¡¼¥É
                   07  WGAIHAKU-SRYCD          PIC X(09)
                                               OCCURS 5.
      *            ¿ÇÎÅ²ó¿ô¥Æ¡¼¥Ö¥ë
                   07  WGAIHAKU-DAY-TBL.
                       09  WGAIHAKU-DAY        PIC 9(03)   OCCURS  31.
      *
      *    ¼¼ÎÁº¹³Û
       01  WBRMSAGAKU-AREA.
           03  WBRMSAGAKU-OCC1-MAX             PIC 9(05).
           03  WBRMSAGAKU-OCC1                 OCCURS 20.
      *        ¥½¡¼¥ÈÍÑÈÖ¹æ
               05  WBRMSAGAKU-NUM              PIC 9(03).
      *        Æþ±¡´ðËÜÎÁ¿ÇÎÅ¶èÊ¬
               05  WBRMSAGAKU-SRYKBN           PIC X(02).
      *        ºÞÅÀ¿ô
               05  WBRMSAGAKU-ZAITEN           PIC 9(08).
      *        ¼¼ÎÁº¹³Û¶èÊ¬¥³¡¼¥É
               05  WBRMSAGAKU-SAGAKUCD         PIC X(02).
      *        ºÞ²ó¿ô
               05  WBRMSAGAKU-ZAIKAISU         PIC 9(07).
      *        ¿ÇÎÅ¥³¡¼¥É
               05  WBRMSAGAKU-SRYCD            PIC X(09).
      *        ¿ÇÎÅ²ó¿ô¥Æ¡¼¥Ö¥ë
               05  WBRMSAGAKU-DAY-TBL.
                   07  WBRMSAGAKU-DAY          PIC 9(03)  OCCURS  31.
      *
      *    ¿©»ö
       01  WSYOKUJI-AREA.
           03  WSYOKUJI-OCC1-MAX               PIC 9(05).
           03  WSYOKUJI-OCC1                   OCCURS 50.
      *        ¥½¡¼¥ÈÍÑÈÖ¹æ
               05  WSYOKUJI-NUM                PIC 9(03).
      *        ´Ä¶­ÎÅÍÜºÞ¥Õ¥é¥°
               05  WSYOKUJI-KANKYO             PIC 9(03).
      *        ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¹ç·×
               05  WSYOKUJI-SRYCDTOTAL         PIC 9(14).
      *        ¿ôÎÌ¹ç·×
               05  WSYOKUJI-SURYOTOTAL         PIC 9(10).
      *        Æþ±¡´ðËÜÎÁ¿ÇÎÅ¶èÊ¬
               05  WSYOKUJI-SRYKBN             PIC X(02).
      *        ºÞÅÀ¿ô
               05  WSYOKUJI-ZAITEN             PIC 9(08).
      *        ºÞ²ó¿ô
               05  WSYOKUJI-ZAIKAISU           PIC 9(07).
      *        ÌÀºÙ¿ô
               05  WSYOKUJI-MEISAISU           PIC 9(08).
               05  WSYOKUJI-SRYCD-OCC          OCCURS  5.
      *            ¿ôÎÌ
                   07  WSYOKUJI-SURYO          PIC 9(05).
      *            ¿ÇÎÅ¥³¡¼¥É
                   07  WSYOKUJI-SRYCD          PIC X(09).
      *        ¿ÇÎÅ²ó¿ô¥Æ¡¼¥Ö¥ë
               05  WSYOKUJI-DAY-TBL.
                   07  WSYOKUJI-DAY            PIC 9(03)   OCCURS  31.
      *        À¸³èÎÅÍÜ¥Õ¥é¥°
               05  WSYOKUJI-LIFE               PIC 9(01).
      *
      *    ¿©»ö
       01  WSKJH1804-AREA.
      *    ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¹ç·×
           03  WSKJH1804-SRYCDTOTAL        PIC 9(14).
      *    ¿ôÎÌ¹ç·×
           03  WSKJH1804-SURYOTOTAL        PIC 9(10).
      *    ºÞÅÀ¿ô
           03  WSKJH1804-ZAITEN            PIC 9(08).
      *    ÌÀºÙ¿ô
           03  WSKJH1804-MEISAISU          PIC 9(08).
           03  WSKJH1804-SRYCD-CNT-G.
               05  WSKJH1804-SRYCD-CNT-VAL.
                   07  WSKJH1804-CNT-ASA       PIC 9(05).
                   07  WSKJH1804-CNT-HIRU      PIC 9(05).
                   07  WSKJH1804-CNT-YUU       PIC 9(05).
                   07  WSKJH1804-CNT-SKJRYOYO1 PIC 9(05).
                   07  WSKJH1804-CNT-SKJRYOYO2 PIC 9(05).
                   07  WSKJH1804-CNT-TOKSYOKU  PIC 9(05).
                   07  WSKJH1804-CNT-SYOKUDO   PIC 9(05).
                   07  WSKJH1804-CNT-SKJFTN    PIC 9(05).
                   07  WSKJH1804-CNT-KANFTN    PIC 9(05).
               05  WSKJH1804-SRYCD-CNT-OCC     REDEFINES
                   WSKJH1804-SRYCD-CNT-VAL     OCCURS  9.
                   07  WSKJH1804-SRYCD-CNT     PIC 9(05).
      *
      *    Æþ±¡ÎÁ²Ã»»
       01  WNYUINKSN-AREA.
           03  WNYUINKSN-OCC1                  OCCURS 02.
               05  WNYUINKSN-OCC2-MAX          PIC 9(05).
               05  WNYUINKSN-OCC2              OCCURS 50.
      *            É½ÍÑÈÖ¹æ¡Ê¥½¡¼¥ÈÍÑ¡Ë
                   07 WNYUINKSN-CDKBN-SYO      PIC 9(01).
                   07 WNYUINKSN-CDKBN-BU       PIC 9(02).
                   07 WNYUINKSN-CDKBN-KBN      PIC X(01).
                   07 WNYUINKSN-CDKBN-KBNNUM   PIC 9(03).
                   07 WNYUINKSN-CDKBN-KBNNUM-EDA
                                               PIC 9(02).
                   07 WNYUINKSN-CDKBN-KOUBAN   PIC 9(02).
                   07 WNYUINKSN-KOUHYOJYUNNUM
                                               PIC 9(09).
      *            Æþ±¡´ðËÜÎÁ¿ÇÎÅ¶èÊ¬
                   07  WNYUINKSN-SRYKBN        PIC X(02).
      *            ºÞÅÀ¿ô
                   07  WNYUINKSN-ZAITEN        PIC 9(08).
      *            ºÞ²ó¿ô
                   07  WNYUINKSN-ZAIKAISU      PIC 9(07).
      *            ¿ÇÎÅ¥³¡¼¥É
                   07  WNYUINKSN-SRYCD         PIC X(09).
      *            ¿ÇÎÅ²ó¿ô¥Æ¡¼¥Ö¥ë
                   07  WNYUINKSN-DAY-TBL.
                       09  WNYUINKSN-DAY       PIC  9(03)  OCCURS  31.
      *
      *    ÎÅÍÜÃ´Åö¼êÅö
       01  WRYYTAN-AREA.
      *    ¿ÇÎÅ¶èÊ¬
           03  WRYYTAN-SRYKBN                  PIC X(02).
      *    ºÞÅÀ¿ô
           03  WRYYTAN-ZAITEN                  PIC 9(08).
      *    ºÞ²ó¿ô
           03  WRYYTAN-ZAIKAISU                PIC 9(07).
      *    ¿ÇÎÅ¥³¡¼¥É
           03  WRYYTAN-SRYCD                   PIC X(09).
      *    ¿ÇÎÅ²ó¿ô¥Æ¡¼¥Ö¥ë
           03  WRYYTAN-DAY-TBL.
               05  WRYYTAN-DAY                 PIC 9(03)   OCCURS  31.
      *
      *    ¥½¡¼¥ÈÍÑÎÎ°è
       01  SRT-AREA.
           03  SRT-IDX1                        PIC 9(05).
           03  SRT-IDX2                        PIC 9(05).
           03  SRT-STIDX                       PIC 9(05).
           03  SRT-MAX                         PIC 9(05).
           03  SRT-OCC1                        OCCURS 101.
               05  SRT-KEY.
                   07  SRT-SRYKBN              PIC X(02).
                   07  SRT-PRIORITY            PIC 9(03).
                   07  SRT-PRIORITY2           PIC 9(03).
                   07  SRT-PRIORITY3           PIC 9(03).
                   07  SRT-CDKBN-SYO           PIC 9(01).
                   07  SRT-CDKBN-BU            PIC 9(02).
                   07  SRT-CDKBN-KBN           PIC X(01).
                   07  SRT-CDKBN-KBNNUM        PIC 9(03).
                   07  SRT-CDKBN-KBNNUM-EDA    PIC 9(02).
                   07  SRT-CDKBN-KOUBAN        PIC 9(02).
                   07  SRT-KOUHYOJYUNNUM       PIC 9(09).
               05  SRT-ZAISKBKBN               PIC X(01).
               05  SRT-ZAITEN                  PIC 9(08).
               05  SRT-ZAIKAISU                PIC 9(07).
               05  SRT-SAGAKUCD                PIC X(02).
               05  SRT-SRYCD-OCC               OCCURS  5.
                   07  SRT-SURYO               PIC 9(05).
                   07  SRT-SRYCD               PIC X(09).
               05  SRT-DAY-TBL.
                   07  SRT-DAY                 PIC 9(03)   OCCURS  31.
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
       01  CONST-AREA.
           03  CONST-H2404                 PIC X(06)   VALUE "201204".
           03  CONST-H2204                 PIC X(06)   VALUE "201004".
           03  CONST-H1904                 PIC X(06)   VALUE "200704".
           03  CONST-H1804                 PIC X(06)   VALUE "200604".
           03  CONST-H1810                 PIC X(06)   VALUE "200610".
           03  CONST-H1604                 PIC X(06)   VALUE "200404".
      *    Æþ±¡ÎÁÊÔ½¸ºî¶ÈÎÎ°èºÇÂç·ï¿ô
           03  CONST-WNYUINRYO-MAX             PIC 9(05)   VALUE 20.
      *    ³°ÇñÊÔ½¸ºî¶ÈÎÎ°èºÇÂç·ï¿ô
           03  CONST-WGAIHAKU-MAX              PIC 9(05)   VALUE 20.
      *    ¼¼ÎÁº¹³ÛÊÔ½¸ºî¶ÈÎÎ°èºÇÂç·ï¿ô
           03  CONST-WBRMSAGAKU-MAX            PIC 9(05)   VALUE 20.
      *    ¿©»öÊÔ½¸ºî¶ÈÎÎ°èºÇÂç·ï¿ô
           03  CONST-WSYOKUJI-MAX              PIC 9(05)   VALUE 50.
      *    Æþ±¡ÎÁ²Ã»»ÊÔ½¸ºî¶ÈÎÎ°èºÇÂç·ï¿ô
           03  CONST-WNYUINKSN-MAX             PIC 9(05)   VALUE 50.
      *    ÊÖµÑºÇÂç·ï¿ô
           03  CONST-HENKYAKU-MAX              PIC 9(05)   VALUE 100.
      *    ¥½¡¼¥Èºî¶ÈÎÎ°èºÇÂç·ï¿ô
           03  CONST-SRT-MAX                   PIC 9(05)   VALUE 100.
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
           03  CONST-SKJ-RYOYO1                PIC X(09)
                                                   VALUE "197000110".
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
           03  CONST-SKJ-RYOYO2                PIC X(09)
                                                   VALUE "197000710".
      *    ÆÃÊÌ´ÉÍý²Ã»»
           03  CONST-SKJ-TOKKANRI              PIC X(09)
                                                   VALUE "197000270".
      *    ÆÃÊÌ¿©²Ã»»
           03  CONST-SKJ-TOKSYOKU              PIC X(09)
                                                   VALUE "197000470".
      *    ¿©Æ²²Ã»»
           03  CONST-SKJ-SYOKUDO               PIC X(09)
                                                   VALUE "197000570".
      *    Ê£¿ô¸¥Î©²Ã»»
           03  CONST-SKJ-FKSKONDATE            PIC X(09)
                                                   VALUE "197000670".
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
           03  CONST-LIFE-RYOYO1               PIC X(09)
                                                   VALUE "197001310".
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë´Ä¶­ÎÅÍÜ
           03  CONST-LIFE-KANKYO1              PIC X(09)
                                                   VALUE "197001410".
      *    ÆÃÊÌ¿©²Ã»»¡ÊÀ¸³èÎÅÍÜ¡Ë
           03  CONST-LIFE-TOKSYOKU             PIC X(09)
                                                   VALUE "197001570".
      *    ¿©Æ²²Ã»»¡ÊÀ¸³èÎÅÍÜ¡Ë
           03  CONST-LIFE-SYOKUDO              PIC X(09)
                                                   VALUE "197001670".
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
           03  CONST-LIFE-RYOYO2               PIC X(09)
                                                   VALUE "197001710".
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë´Ä¶­ÎÅÍÜ
           03  CONST-LIFE-KANKYO2              PIC X(09)
                                                   VALUE "197001810".
      *    ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
           03  CONST-SKJFTN-IPN                PIC X(09)
                                                   VALUE "197000810".
      *    ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
           03  CONST-SKJFTN-TEI2               PIC X(09)
                                                   VALUE "197000910".
      *    ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
           03  CONST-SKJFTN-TEI2-90            PIC X(09)
                                                   VALUE "197001010".
      *    ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
           03  CONST-SKJFTN-TEI1               PIC X(09)
                                                   VALUE "197001110".
      *
           03  CONST-SKJ-COMMENT-G.
               05  CONST-SKJ-COMMENT-VAL.
      *    ¿©»öÄ«
                   07  CONST-SKJ-ASA           PIC 9(09)
                                               VALUE 99999918.
      *    ¿©»öÃë
                   07  CONST-SKJ-HIRU          PIC 9(09)
                                               VALUE 99999919.
      *    ¿©»öÍ¼
                   07  CONST-SKJ-YUU           PIC 9(09)
                                               VALUE 99999920.
               05  CONST-SKJ-COMMENT-R REDEFINES CONST-SKJ-COMMENT-VAL.
                   07  CONST-SKJ-COMMENT       PIC 9(09)
                                               OCCURS 3.
      *
           COPY    "CPSK5000.INC"  REPLACING   //SYS-5000//
                                   BY          //WKSYS-5000//.
      *
       01  WSKJNACCT-AREA.
           02  WSKJNACCT-SYOKUDO-NG            PIC X(01)
                                               OCCURS  31.
           02  WSKJNACCT-OCC                   OCCURS  3.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT-//
                                       BY          //WSKJNACCT-//.
      *
       01  TSKJFTN-AREA.
           03  TSKJFTN-OCC                     OCCURS  31.
               05  TSKJFTN-SRYCD               PIC X(09).
               05  TSKJFTN-MONEY               PIC 9(10).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK5000.INC".
      *
           COPY    "CPSK5113.INC".
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
      *    ÁªÄêÎÅÍÜÈñ
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
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
      *    ÉÂÅïÁªÄêÆþ±¡¼«¸ÊÉéÃ´¼èÆÀ¥µ¥Ö
           COPY    "CPORCSBTUSENTEI.INC".
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYULIFE.INC".
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    ´µ¼Ô½êÆÀ¾ðÊóÊÔ½¸¥µ¥Ö
           COPY    "CPORCSSYOTOKU.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                        SECTION.
      *
      *    Æþ±¡²ñ·×¥«¡¼¥É¾ðÊóÊÔ½¸¥µ¥Ö¥Ñ¥é¥á¥¿
           COPY    "CPORCHCM34S01.INC".
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           HCM34S01-AREA
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
           INITIALIZE                      CNT-AREA
           INITIALIZE                      WNYUINRYO-AREA
                                           WGAIHAKU-AREA
                                           WBRMSAGAKU-AREA
                                           WSYOKUJI-AREA
                                           WSKJH1804-AREA
                                           WNYUINKSN-AREA
                                           WRYYTAN-AREA
                                           WSKJNACCT-AREA
                                           TSKJFTN-AREA
      *
           INITIALIZE                      HCM34S01-HENKYAKU-AREA
           MOVE    ZERO                TO  HCM34S01-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
      *    ½é´ü½èÍý
           PERFORM 150-INIT-SEC
      *    ¼ç½èÍý
           PERFORM 200-MAIN-SEC
      *    ½ªÎ»½èÍý
           PERFORM 300-END-SEC
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
      *    »»ÄêÇ¯·î¤¬ÀßÄê¤µ¤ì¤Æ¤¤¤ë¤³¤È
           IF    ( HCM34S01-SANTEIYM       =   SPACE )
               MOVE    1                   TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                          PTINF-REC
           MOVE    HCM34S01-HOSPNUM        TO  PTINF-HOSPNUM
           MOVE    HCM34S01-PTID           TO  PTINF-PTID
           MOVE    PTINF-REC               TO  MCPDATA-REC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC                  NOT =   ZERO )
               MOVE    2                   TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    ³ºÅöÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¤ÎÆþ±¡²ñ·×¤¬Â¸ºß¤¹¤ë¤«¥Á¥§¥Ã¥¯
           INITIALIZE                          FLG-NYUINDAY-G
      *
           MOVE    "5"                     TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-NYUINACCT       NOT =   ZERO )
               MOVE    3                   TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    ZERO                    TO  FLG-NYUINARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   31 )
                IF   ( HCM34S01-HKNCOMBI   =   NACCT-DAY(IDX1) )
                   MOVE    1               TO  FLG-NYUINARI
                   IF    ( HCM34S01-SRYKAKBN   =   "1" )
                       IF    ( HCM34S01-SRYKADAY (IDX1)    =   1 )
                           MOVE    1       TO  FLG-NYUINDAY (IDX1)
                       END-IF
                   ELSE
                           MOVE    1       TO  FLG-NYUINDAY (IDX1)
                   END-IF
                END-IF
           END-PERFORM
      *
           MOVE    HCM34S01-SANTEIYM       TO  WRK-SKYSTYMD
                                               WRK-SKYEDYMD
           MOVE    1                       TO  WRK-SKYSTDD
           MOVE    31                      TO  WRK-SKYEDDD
      *    ´ü´Ö»ØÄê¶èÊ¬¤¬ÀßÄê¤µ¤ì¤Æ¤¤¤ë¾ì¹ç¡¢´ü´Ö¤¬ÀßÄê¤µ¤ì¤Æ¤¤¤ë¤³¤È
           IF    ( HCM34S01-KIKANKBN       =   "1" )
               IF    ( HCM34S01-SKYSTYMD   IS  NOT NUMERIC )
                   MOVE    12              TO  HCM34S01-RC
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
               IF    ( HCM34S01-SKYEDYMD   IS  NOT NUMERIC )
                   MOVE    13              TO  HCM34S01-RC
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
      *
               MOVE    HCM34S01-SKYSTYMD (7:2) TO  WRK-STIDX
               MOVE    HCM34S01-SKYEDYMD (7:2) TO  WRK-EDIDX
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   31 )
      *
                   IF    ( IDX1    >=  WRK-STIDX )
                    AND  ( IDX1    <=  WRK-EDIDX )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-NYUINDAY (IDX1)
                   END-IF
      *
               END-PERFORM
      *
               MOVE    HCM34S01-SKYSTYMD   TO  WRK-SKYSTYMD
               MOVE    HCM34S01-SKYEDYMD   TO  WRK-SKYEDYMD
      *
               IF    ( HCM34S01-DOJITUFLG-ST   =   "1" )
                   MOVE    2           TO  FLG-NYUINDAY(WRK-SKYSTDD)
                   MOVE    HCM34S01-TIMEKBN-ST
                                       TO  WRK-DOJITUST(WRK-SKYSTDD)
               END-IF
      *
               IF    ( HCM34S01-DOJITUFLG-ED   =   "1" )
                   MOVE    HCM34S01-TIMEKBN-ED
                                       TO  WRK-DOJITUED(WRK-SKYEDDD)
               END-IF
      *
           END-IF
      *
           MOVE    HCM34S01-SANTEIYM       TO  WRK-SRYYMD
           MOVE    "01"                    TO  WRK-SRYYMD (7:2)
      *
           IF    ( HCM34S01-SANTEIYM       >=  CONST-H1804 )
            AND  ( HCM34S01-EXSYOKUJI      =   "1" )
               MOVE    1                   TO  FLG-EXSYOKUJI
           END-IF
      *
           IF    ( HCM34S01-SANTEIYM       >=  CONST-H1804 )
            AND  ( HCM34S01-SKJFTN         =   "1" )
               MOVE    1                   TO  FLG-SKJFTN
           END-IF
      *
      *
           IF    ( HCM34S01-EXGAIHAKU      =   SPACE )
               MOVE       "1"              TO  HCM34S01-EXGAIHAKU
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¸¡º÷½èÍý¡Ê£Ë£Å£Ù£±£·¡Ë
      *****************************************************************
       1001-NYUINACCT-KEY17-SEL-SEC    SECTION.
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    HCM34S01-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    HCM34S01-PTID           TO  NACCT-PTID
           MOVE    HCM34S01-SANTEIYM       TO  NACCT-SRYYM
           MOVE    WRK-NACCT-ZAISKBKBN     TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1                   TO  FLG-NYUINACCT
           ELSE
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           END-IF
      *
           .
       1001-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×ÆÉ¤ß¹þ¤ß½èÍý¡Ê£Ë£Å£Ù£±£·¡Ë
      *****************************************************************
       1001-NYUINACCT-KEY17-READ-SEC    SECTION.
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACCT
           ELSE
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           END-IF
      *
           .
       1001-NYUINACCT-KEY17-READ-EXT.
           EXIT.
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       150-INIT-SEC                    SECTION.
      *
           INITIALIZE                      WRK-SYS-MIJISI-G
                                           WRK-SHOKUJIRYOYO
                                           WRK-TOKUKANRIKASAN-G
                                           WRK-SENTEI-PTFTN-G
                                           FLG-NYUINGENDAY-G
                                           FLG-BRMSGKSANTEIDAY-G
      *
      *    °åÎÅµ¡´Ø¾ðÊó¼èÆÀ½èÍý
           PERFORM 1501-SYS1001-GET-SEC
      *
      *    Æþ±¡´ðËÜ¾ðÊó¼èÆÀ
           PERFORM 1501-SYS5000-GET-SEC
      *
           IF    (   HCM34S01-SANTEIYM   >=  CONST-H1810 )
               INITIALIZE                      ORCSNYULIFE-AREA
               MOVE    HCM34S01-HOSPNUM     TO  LNK-NYULIFE-HOSPNUM
               MOVE    HCM34S01-PTID       TO  LNK-NYULIFE-PTID
               MOVE    HCM34S01-SANTEIYM   TO  LNK-NYULIFE-SANTEIYM
               CALL    "ORCSNYULIFE"       USING
                                           ORCSNYULIFE-AREA
                                           SPA-AREA
               IF    ( LNK-NYULIFE-RC  NOT =   ZERO )
      *            DISPLAY "ORCSNYULIFE ERR:" LNK-NYULIFE-RC
                   INITIALIZE              ORCSNYULIFE-AREA
               END-IF
               INITIALIZE                  ORCSSYOTOKUAREA
               MOVE    HCM34S01-HOSPNUM    TO  LNK-SYOTOKU-HOSPNUM
               MOVE    HCM34S01-PTID       TO  LNK-SYOTOKU-PTID
               MOVE    HCM34S01-SANTEIYM   TO  LNK-SYOTOKU-SRYYM
               CALL    "ORCSSYOTOKU"       USING
                                           ORCSSYOTOKUAREA
                                           SPA-AREA
               IF    ( LNK-SYOTOKU-RC  NOT =         ZERO )
      *             DISPLAY "ORCSSYTOKU RC = " LNK-SYOTOKU-RC
                    INITIALIZE                 ORCSSYOTOKUAREA
               END-IF
           ELSE
               INITIALIZE                      ORCSNYULIFE-AREA
               INITIALIZE                      ORCSSYOTOKUAREA
           END-IF
      *
      *
           INITIALIZE  ORCSNYUINDAY-AREA
           MOVE    HCM34S01-HOSPNUM    TO  LNK-NYUINDAY-HOSPNUM
           MOVE    HCM34S01-PTID       TO  LNK-NYUINDAY-PTID
           MOVE    HCM34S01-SANTEIYM   TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"          USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
           IF    ( LNK-NYUINDAY-RC     NOT =   ZERO )
      *            DISPLAY "ORCSNYUINDAY ERR:" LNK-NYUINDAY-RC
                   INITIALIZE              ORCSNYUINDAY-AREA
           END-IF
      *
           PERFORM VARYING     IDXD    FROM    WRK-SKYSTDD BY  1
                   UNTIL     ( IDXD    >       WRK-SKYEDDD )
      *
               IF    ( LNK-NYUINDAY-DOUJITSU-KBN(IDXD) = "1" )
                   IF    ( LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDXD)
                                               =  HCM34S01-HKNCOMBI )
                     AND ( LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDXD)
                                           NOT =  HCM34S01-HKNCOMBI )
                       MOVE    2           TO  FLG-NYUINDAY(IDXD)
                       MOVE    LNK-NYUINDAY-DOUJITSU-SKJKBN(IDXD)
                                           TO  WRK-DOJITUST (IDXD)
                   END-IF
      *
                   IF    ( LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDXD)
                                           NOT =  HCM34S01-HKNCOMBI )
                     AND ( LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDXD)
                                               =  HCM34S01-HKNCOMBI )
                       MOVE    LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDXD)
                                           TO  WRK-DOJITUED (IDXD)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       150-INIT-EXT.
           EXIT.
      *****************************************************************
      *    °åÎÅµ¡´Ø¾ðÊó¼èÆÀ½èÍý
      *****************************************************************
       1501-SYS1001-GET-SEC            SECTION.
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    HCM34S01-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               INITIALIZE  SYS-1001-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1501-SYS1001-GET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ðËÜ¾ðÊó¼èÆÀ
      *****************************************************************
       1501-SYS5000-GET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    ZERO                TO  CNT-SYS5000
      *
           INITIALIZE                      WKSYS-5000-REC
      *
           INITIALIZE                      SYS-5000-REC
           MOVE    HCM34S01-HOSPNUM    TO  SYS-5000-HOSPNUM
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*%"                TO  SYS-5000-KBNCD
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-5000-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               IF    ( HCM34S01-SANTEIYM  >=  SYS-5000-STYUKYMD (1:6))
                AND  ( HCM34S01-SANTEIYM  <=  SYS-5000-EDYUKYMD (1:6))
      *
                   PERFORM 1501-SYS5000-HEN-SEC
      *
                   MOVE    SYS-5000-REC
                                       TO  WKSYS-5000-REC
      *
               END-IF
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-5000-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    WKSYS-5000-REC      TO  SYS-5000-REC
      *
           .
       1501-SYS5000-GET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ðËÜ¾ðÊóÊÔ½¸
      *****************************************************************
       1501-SYS5000-HEN-SEC            SECTION.
      *
           COMPUTE CNT-SYS5000 =   CNT-SYS5000 +   1
      *
           IF    ( CNT-SYS5000         =   1 )
               IF    ( SYS-5000-SHOKUJIRYOYOCD     =   "197000110" )
                   MOVE    1               TO   WRK-SHOKUJIRYOYO
               ELSE
                   MOVE    2               TO   WRK-SHOKUJIRYOYO
               END-IF
           END-IF
      *
           MOVE    01                      TO   WRK-STIDX
           MOVE    31                      TO   WRK-EDIDX
      *
           IF    ( HCM34S01-SANTEIYM       =    SYS-5000-STYUKYMD (1:6))
               MOVE    SYS-5000-STYUKYMD (7:2)
                                           TO   WRK-STIDX
           END-IF
      *
           IF    ( HCM34S01-SANTEIYM       =    SYS-5000-EDYUKYMD (1:6))
               MOVE    SYS-5000-EDYUKYMD (7:2)
                                           TO   WRK-EDIDX
           END-IF
      *
           IF    ( SYS-5000-SHOKUJIRYOYOCD         =   "197000110" )
               IF    ( SYS-5000-SHOKUKBN(1)        =   "2" )
                   PERFORM VARYING IDX0    FROM    WRK-STIDX   BY  1
                           UNTIL ( IDX0        >   WRK-EDIDX )
                       MOVE    1           TO  WRK-TOKUKANRIKASAN (IDX0)
                   END-PERFORM
               END-IF
           END-IF
      *
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   4 )
      *
               IF   ( SYS-5000-GENKBN(IDX4)    =   "2" )
                   MOVE   1            TO  WRK-SYS-MIJISI(IDX4)
                   PERFORM VARYING IDX0    FROM    WRK-STIDX   BY  1
                           UNTIL ( IDX0        >   WRK-EDIDX )
      *
                       MOVE   1        TO  FLG-NYUINGENDAY (IDX4 IDX0)
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           IF   ( SYS-5000-NACCT-BRMSGKKBN     =   "2" )
               PERFORM VARYING IDX0    FROM    WRK-STIDX   BY  1
                           UNTIL ( IDX0        >   WRK-EDIDX )
      *
                   MOVE   1        TO  FLG-BRMSGKSANTEIDAY (IDX0)
      *
               END-PERFORM
           END-IF
      *
           .
       1501-SYS5000-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    ³°Çñ¥Á¥§¥Ã¥¯
           PERFORM 2001-GAIHAKU-CHK-SEC
      *
      *    Æþ±¡ÎÁ¥Á¥§¥Ã¥¯½èÍý
           PERFORM 2001-NYUINRYO-CHK-SEC
      *
      *    Æþ±¡ÎÁÊÔ½¸½èÍý¡Ê³°Çñ¤âÊÔ½¸¡Ë
           PERFORM 2001-NYUINRYO-HEN-SEC
      *
      *    ¼¼ÎÁº¹³ÛÊÔ½¸½èÍý
           PERFORM 2001-BRMSAGAKU-HEN-SEC
      *
      *    ¿©»öÊÔ½¸½èÍý
           IF    ( HCM34S01-SANTEIYM   >=  CONST-H1804 )
               PERFORM 2002-SYOKUJI-HEN-SEC
           ELSE
               PERFORM 2001-SYOKUJI-HEN-SEC
           END-IF
      *
      *    Æþ±¡ÎÁ²Ã»»ÊÔ½¸½èÍý
           PERFORM 2001-NYUINKSN-HEN-SEC
      *
      *    ÎÅÍÜÃ´ÅöÈñÊÔ½¸½èÍý
           PERFORM 2001-RYYTAN-HEN-SEC
      *
      *    ºÞ£Ó£Ï£Ò£Ô½èÍý
           PERFORM 2001-ZAISORT-SEC
      *
      *    ÊÖµÑÃÍÀßÄê½èÍý
           PERFORM 2001-HENKYAKU-SET-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-GAIHAKU-CHK-SEC            SECTION.
      *
           INITIALIZE                          FLG-GAIHAKUDAY-G
      *
           MOVE    "2"                     TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               PERFORM VARYING     IDX1    FROM    1   BY  1
                       UNTIL     ( IDX1    >   31 )
                   IF   ( NACCT-DAY(IDX1)  >   ZERO )
                       MOVE    NACCT-DAY(IDX1)
                                           TO  FLG-GAIHAKUDAY (IDX1)
                   END-IF
               END-PERFORM
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-GAIHAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-NYUINRYO-CHK-SEC           SECTION.
      *
           INITIALIZE                          FLG-SENTEIDAY-G
                                               FLG-NYUINRYODAY-G
                                               FLG-TOKTEIDAY-G
      *
           MOVE    "1"                     TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
      *        ÁªÄêÆþ±¡ÎÁ¤ò»»Äê¤·¤Æ¤¤¤¿Æü¤Ë¤Á¤òÂàÈò
               IF    ( NACCT-SRYCDTOTAL    =    099999916 )
                   IF    ( HCM34S01-HKNNUM =    "975" )
                       CONTINUE
                   ELSE
                       PERFORM VARYING     IDX1    FROM    1   BY  1
                               UNTIL     ( IDX1    >   31 )
                           IF   ( NACCT-DAY(IDX1)  >   ZERO )
                               MOVE    1       TO  FLG-SENTEIDAY (IDX1)
                           END-IF
                       END-PERFORM
                   END-IF
               ELSE
      *
      *            Æþ±¡ÎÁ¤ò»»Äê¤·¤Æ¤¤¤ëÆü¤Ë¤Á¤òÂàÈò
                   PERFORM VARYING     IDX1    FROM    1   BY  1
                           UNTIL     ( IDX1    >   31 )
      *
                       IF   ( NACCT-DAY(IDX1)  >   ZERO )
                           MOVE    1       TO  FLG-NYUINRYODAY (IDX1)
                       END-IF
      *
                   END-PERFORM
      *
      *            ÆÃÄêÆþ±¡ÎÁ¤ò»»Äê¤·¤Æ¤¤¤¿Æü¤Ë¤Á¤òÂàÈò
                   IF    ( NACCT-SRYKBN        =    "92" )
                       PERFORM VARYING     IDX1    FROM    1   BY  1
                               UNTIL     ( IDX1    >   31 )
                           IF   ( NACCT-DAY(IDX1)  >   ZERO )
                               MOVE    1   TO  FLG-TOKTEIDAY (IDX1)
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-NYUINRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁÊÔ½¸½èÍý
      *****************************************************************
       2001-NYUINRYO-HEN-SEC           SECTION.
      *
           MOVE    "1"                     TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF    ( NACCT-SRYCDTOTAL    =    099999916 )
                   CONTINUE
               ELSE
                   PERFORM 20011-NYUINRYO-HEN-SEC
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-NYUINRYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁÊÔ½¸½èÍý
      *****************************************************************
       20011-NYUINRYO-HEN-SEC          SECTION.
      *
      *    Æþ±¡ÎÁÊÔ½¸½èÍý
           PERFORM 200111-NYUINRYO-HEN-SEC
      *
      *    ÁªÄêÆþ±¡ÎÁÊÔ½¸½èÍý
           PERFORM 200111-SENTEI-HEN-SEC
      *
      *    ³°ÇñÊÔ½¸½èÍý
           PERFORM 200111-GAIHAKU-HEN-SEC
      *
      *    ÁªÄêÆþ±¡ÎÁ´µ¼Ô¼«¸ÊÉéÃ´ÊÔ½¸½èÍý
           PERFORM 200111-SENTEIPTFTN-HEN-SEC
      *
           .
       20011-NYUINRYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁÊÔ½¸½èÍý
      *****************************************************************
       200111-NYUINRYO-HEN-SEC         SECTION.
      *
           IF    ( NACCT-SRYKBN            =   "90" )
               MOVE    1               TO  IDXA
           ELSE
               MOVE    2               TO  IDXA
           END-IF
      *
           IF    ( WNYUINRYO-OCC2-MAX (IDXA)
                                           >=  CONST-WNYUINRYO-MAX )
               GO  TO  200111-NYUINRYO-HEN-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-TAISYO
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD        >   31 )
      *
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                AND  ( FLG-NYUINDAY (IDXD)     =   1 )
                AND  ( FLG-NYUINRYODAY (IDXD)  =   1 )
                AND  ( FLG-SENTEIDAY(IDXD)     =   ZERO )
                AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO )
                   IF    ( FLG-TAISYO      =   ZERO )
                       MOVE    1           TO  FLG-TAISYO
      *                Æþ±¡ÎÁ´ðËÜ¾ðÊóÀßÄê½èÍý
                       PERFORM 2001111-NYUINKHNINF-SET-SEC
                   END-IF
      *
                   MOVE    1               TO  WNYUINRYO-DAY
                                                       (IDXA IDXB IDXD)
      *
                   COMPUTE WNYUINRYO-ZAIKAISU (IDXA IDXB)
                       =   WNYUINRYO-ZAIKAISU (IDXA IDXB)  +   1
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-NYUINRYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ÁªÄêÆþ±¡ÎÁÊÔ½¸½èÍý
      *****************************************************************
       200111-SENTEI-HEN-SEC           SECTION.
      *
           IF    ( NACCT-SRYKBN            =   "90" )
               MOVE    1               TO  IDXA
           ELSE
               MOVE    2               TO  IDXA
           END-IF
      *
           IF    ( WNYUINRYO-OCC2-MAX (IDXA)
                                           >=  CONST-WNYUINRYO-MAX )
               GO  TO  200111-SENTEI-HEN-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-TAISYO
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD        >   31 )
      *
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                AND  ( FLG-NYUINDAY (IDXD)     =   1 )
                AND  ( FLG-NYUINRYODAY (IDXD)  =   1 )
                AND  ( FLG-SENTEIDAY(IDXD)     =   1 )
                AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO )
                   IF    ( FLG-TAISYO      =   ZERO )
                       MOVE    1           TO  FLG-TAISYO
      *                Æþ±¡ÎÁ´ðËÜ¾ðÊóÀßÄê½èÍý
                       PERFORM 2001111-NYUINKHNINF-SET-SEC
      *                ÁªÄêÆþ±¡ÎÁÃÖ¤­´¹¤¨½èÍý
                       PERFORM 2001111-SENTEI-CHG-SEC
                   END-IF
      *
                   MOVE    1               TO  WNYUINRYO-DAY
                                                       (IDXA IDXB IDXD)
      *
                   COMPUTE WNYUINRYO-ZAIKAISU (IDXA IDXB)
                       =   WNYUINRYO-ZAIKAISU (IDXA IDXB)  +   1
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-SENTEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ³°ÇñÊÔ½¸½èÍý
      *****************************************************************
       200111-GAIHAKU-HEN-SEC          SECTION.
      *
           IF    ( NACCT-SRYKBN            =   "90" )
               MOVE    1               TO  IDXA
           ELSE
               MOVE    2               TO  IDXA
           END-IF
      *
      *    ³°Çñ´ðËÜÅÀ¿ô¼èÆÀ
           PERFORM 200111-GAIHAKU-KHNTEN-GET-SEC
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD        >   31 )
      *
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                AND  ( FLG-NYUINDAY (IDXD)     =   1 )
                AND  ( FLG-NYUINRYODAY (IDXD)  =   1 )
                AND  ( FLG-GAIHAKUDAY(IDXD)    >   ZERO )
      *
      *            ³°Çñ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ½èÍý
                   PERFORM 200111-GAIHAKU-SRYCD-GET-SEC
      *
      *            ³°ÇñÅÀ¿ô¼èÆÀ½èÍý
                   PERFORM 200111-GAIHAKU-TENSU-GET-SEC
      *
      *            ³°Çñ´ðËÜ¾ðÊóÀßÄê½èÍý
                   PERFORM 200111-GAIHAKKHNINF-SET-SEC
                   IF    ( IDXB        >   ZERO )
                     AND ( FLG-OVER    =   ZERO )
                       MOVE    1       TO  WGAIHAKU-DAY
                                                   (IDXA IDXB IDXD)
                       COMPUTE WGAIHAKU-ZAIKAISU (IDXA IDXB)
                           =   WGAIHAKU-ZAIKAISU (IDXA IDXB)   +   1
                       IF    ( WGAIHAKU-NUM (IDXA IDXB)    =   ZERO )
                               MOVE    IDXD
                                       TO  WGAIHAKU-NUM (IDXA IDXB)
                       ELSE
                           IF    ( IDXD    <   WGAIHAKU-NUM (IDXA IDXB))
                               MOVE    IDXD
                                       TO  WGAIHAKU-NUM (IDXA IDXB)
                           END-IF
                       END-IF
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-GAIHAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ³°Çñ´ðËÜÅÀ¿ô¼èÆÀ½èÍý
      *****************************************************************
       200111-GAIHAKU-KHNTEN-GET-SEC   SECTION.
      *
      *    Æþ±¡´ðËÜÎÁ¤Î¥³¡¼¥É¤òÃµ¤¹
           MOVE    ZERO                TO  WRK-KHNTEN
           MOVE    ZERO                TO  WRK-NYUKHNKSNKBN
           MOVE    SPACE               TO  WRK-CHUKSNCD
           MOVE    SPACE               TO  WRK-KHNSRYCD
           MOVE    ZERO                TO  WRK-HYOKETU-TEN
           MOVE    SPACE               TO  WRK-HYOKETU-SRYCD
           MOVE    ZERO                TO  IDX-KHN
           MOVE    ZERO                TO  WRK-GAIHAKU-KSNTEN
           MOVE    SPACE               TO  WRK-SENTEI-SRYCD
           MOVE    ZERO                TO  WRK-SENTEI-TEN
           INITIALIZE                      WRK-KSNSRYCD-G
      *
           PERFORM 20011-NYUINACT-GET-SEC
           IF    ( FLG-NYUINACT        NOT =    ZERO )
               GO  TO  200111-GAIHAKU-KHNTEN-GET-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   5 )
               MOVE    NSRY-SRYCD (IDX2)
                                       TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   EVALUATE    TNS-DATAKBN
                   WHEN    1
                       MOVE    TNS-TEN             TO  WRK-KHNTEN
                       MOVE    TNS-NYUKHNKSNKBN    TO  WRK-NYUKHNKSNKBN
                       MOVE    TNS-CHUKSNCD        TO  WRK-CHUKSNCD
                       MOVE    IDX2                TO  IDX-KHN
                   WHEN    2
                       EVALUATE    TNS-TENSIKIBETU
                       WHEN    3
                       WHEN    8
                           IF    ( TNS-TENSIKIBETU =   3 )
                               COMPUTE WRK-GAIHAKU-KSNTEN
                                   =   WRK-GAIHAKU-KSNTEN
                                   +   TNS-TEN
                           ELSE
                               COMPUTE WRK-GAIHAKU-KSNTEN
                                   =   WRK-GAIHAKU-KSNTEN
                                   -   TNS-TEN
                           END-IF
                           IF    ( WRK-KSNSRYCD-MAX    >=  5 )
                               CONTINUE
                           ELSE
                               COMPUTE WRK-KSNSRYCD-MAX
                                   =   WRK-KSNSRYCD-MAX
                                   +   1
                               MOVE    TNS-SRYCD
                                           TO  WRK-KSNSRYCD
                                              (WRK-KSNSRYCD-MAX)
                           END-IF
                       WHEN    6
                           MOVE  TNS-SRYCD TO  WRK-HYOKETU-SRYCD
                           MOVE  TNS-TEN   TO  WRK-HYOKETU-TEN
                       END-EVALUATE
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           IF    ( IDX-KHN             =   ZERO )
               DISPLAY "KHNRYO NOT FOUND[" NACCT-PTID "]"
                                       "[" NACCT-ZAINUM "]"
      *        MOVE    5               TO  HCM34S01-RC
      *        PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           IF    ( IDX-KHN             =   ZERO )
               MOVE    SPACE           TO  WRK-TNS-SRYCD
                                           WRK-KHNSRYCD
           ELSE
               MOVE    NSRY-SRYCD (IDX-KHN)
                                       TO  WRK-TNS-SRYCD
                                           WRK-KHNSRYCD
           END-IF
      *
      *    ´Ç¸îÈæÎ¨´ð½àÌ¤Ã£¤Î¥Á¥§¥Ã¥¯
           IF    ( HCM34S01-SANTEIYM   >=  CONST-H2404 )
               CONTINUE
           ELSE
               PERFORM 2001111-KNGMITATU-CHK-SEC
               IF    ( WRK-GAIHAKU-KHNTEN  NOT =   ZERO )
                   MOVE    WRK-GAIHAKU-KHNTEN  TO  WRK-KHNTEN
                   MOVE    WRK-TNS-SRYCD       TO  WRK-KHNSRYCD
               END-IF
           END-IF
      *
      *    ÁªÄêÆþ±¡»þ¤Î¥³¡¼¥É¼èÆÀ
           PERFORM 900-SENTEICDCHG-KEY2-SEL-SEC
           IF    ( FLG-SENTEICDCHG     =   ZERO )
               MOVE    SENCHG-SENTEISRYCD
                                           TO  WRK-SENTEI-SRYCD
               MOVE    SENCHG-SENTEISRYCD  TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       NOT =   ZERO )
                   MOVE    14              TO  HCM34S01-RC
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
               MOVE    TNS-TEN             TO  WRK-SENTEI-TEN
           END-IF
      *
           .
       200111-GAIHAKU-KHNTEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÈæÎ¨´ð½àÌ¤Ã£¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001111-KNGMITATU-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-GAIHAKU-SRYCD
           MOVE    ZERO                TO  WRK-GAIHAKU-KHNTEN
      *
           EVALUATE  WRK-TNS-SRYCD
           WHEN  "190078210"
               MOVE  "190077410"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078310"
               MOVE  "190077510"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078410"
               MOVE  "190077610"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078510"
               MOVE  "190077710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078610"
               MOVE  "190077810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078710"
               MOVE  "190077910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078810"
               MOVE  "190078010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078910"
               MOVE  "190078110"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190082410"
               MOVE  "190081710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190082510"
               MOVE  "190081810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190082610"
               MOVE  "190081910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190082710"
               MOVE  "190082010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190082810"
               MOVE  "190082110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190082910"
               MOVE  "190082210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190083010"
               MOVE  "190082310"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190084310"
               MOVE  "190083610"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084410"
               MOVE  "190083710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084510"
               MOVE  "190083810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084610"
               MOVE  "190083910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084710"
               MOVE  "190084010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084810"
               MOVE  "190084110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084910"
               MOVE  "190084210"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190087410"
               MOVE  "190085910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190087510"
               MOVE  "190086010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190087610"
               MOVE  "190086110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190087710"
               MOVE  "190086210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190087810"
               MOVE  "190086310"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190087910"
               MOVE  "190086410"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088010"
               MOVE  "190086510"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088110"
               MOVE  "190086610"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088210"
               MOVE  "190086710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088310"
               MOVE  "190086810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088410"
               MOVE  "190086910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088510"
               MOVE  "190087010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088610"
               MOVE  "190087110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088710"
               MOVE  "190087210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088810"
               MOVE  "190087310"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190094710"
               MOVE  "190093710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190094810"
               MOVE  "190093810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190094910"
               MOVE  "190093910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190095010"
               MOVE  "190094010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190095110"
               MOVE  "190094110"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190797410"
               MOVE  "190796610"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190797510"
               MOVE  "190796710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190797610"
               MOVE  "190796810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190797710"
               MOVE  "190796910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190797810"
               MOVE  "190797010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190797910"
               MOVE  "190797110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190798010"
               MOVE  "190797210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190798110"
               MOVE  "190797310"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190803510"
               MOVE  "190802810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190803610"
               MOVE  "190802910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190803710"
               MOVE  "190803010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190803810"
               MOVE  "190803110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190803910"
               MOVE  "190803210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190804010"
               MOVE  "190803310"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190804110"
               MOVE  "190803410"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190091210"
               MOVE  "190090210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190091310"
               MOVE  "190090310"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190109670"
           WHEN  "190117070"
           WHEN  "190117670"
               MOVE  "190097010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190808870"
           WHEN  "190813270"
           WHEN  "190813870"
               MOVE  "190805710"       TO  WRK-GAIHAKU-SRYCD
      *    ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
           WHEN  "190774210"
                 MOVE  "190774110"     TO  WRK-GAIHAKU-SRYCD
      *    Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
           WHEN  "190740010"
                 MOVE  "190739910"     TO  WRK-GAIHAKU-SRYCD
      *    Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
           WHEN  "190813510"
                 MOVE  "190813410"     TO  WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           IF   (NACCT-SRYYM      >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    ( WRK-GAIHAKU-SRYCD   NOT =   SPACE )
               MOVE    WRK-GAIHAKU-SRYCD   TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   MOVE    TNS-TEN     TO  WRK-GAIHAKU-KHNTEN
               END-IF
           END-IF
      *
           .
       2001111-KNGMITATU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ³°Çñ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ½èÍý
      *****************************************************************
       200111-GAIHAKU-SRYCD-GET-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-GAIHAKU-SRYCD
           INITIALIZE                      WRK-ZAI-G
      *
           IF    ( NACCT-SRYKBN            =   "90" )
               EVALUATE    FLG-GAIHAKUDAY (IDXD)
               WHEN    1
                   MOVE    "190107290"     TO  WRK-GAIHAKU-SRYCD
               WHEN    2
                   MOVE    "190107390"     TO  WRK-GAIHAKU-SRYCD
               WHEN    3
                   IF    ( NACCT-SRYYM     >=  CONST-H1604 )
                       MOVE    "099999907" TO  WRK-GAIHAKU-SRYCD
                   ELSE
                       MOVE    "190111590" TO  WRK-GAIHAKU-SRYCD
                   END-IF
               WHEN    4
               WHEN    5
               WHEN    6
               WHEN    7
                   IF    ( FLG-SENTEIDAY(IDXD) =   ZERO )
                       PERFORM 2001111-TAKIKAN-SEC
                   ELSE
                       PERFORM 2001111-SENTEI-TAKIKAN-SEC
                   END-IF
               WHEN    8
               WHEN    9
                       PERFORM 2001111-GAIHAKU-GENSAN-SEC
               END-EVALUATE
           ELSE
               EVALUATE    FLG-GAIHAKUDAY (IDXD)
               WHEN    1
                   MOVE    "190107490"     TO  WRK-GAIHAKU-SRYCD
               WHEN    2
                   MOVE    "190107590"     TO  WRK-GAIHAKU-SRYCD
               WHEN    3
                   IF    ( NACCT-SRYYM     >=  CONST-H1604 )
                       MOVE    "099999907" TO  WRK-GAIHAKU-SRYCD
                   ELSE
                       MOVE    "190111590" TO  WRK-GAIHAKU-SRYCD
                   END-IF
               WHEN    4
               WHEN    5
               WHEN    6
               WHEN    7
                   IF    ( FLG-SENTEIDAY(IDXD) =   ZERO )
                       PERFORM 2001111-TAKIKAN-SEC
                   ELSE
                       PERFORM 2001111-SENTEI-TAKIKAN-SEC
                   END-IF
               WHEN    8
               WHEN    9
                       PERFORM 2001111-GAIHAKU-GENSAN-SEC
               END-EVALUATE
           END-IF
      *
           IF    ( FLG-SENTEIDAY(IDXD) =   ZERO )
               MOVE    WRK-KHNSRYCD        TO  WRK-ZAI-KHNSRYCD
               MOVE    WRK-KHNTEN          TO  WRK-ZAI-KHNTEN
           ELSE
               MOVE    WRK-SENTEI-SRYCD    TO  WRK-ZAI-KHNSRYCD
               MOVE    WRK-SENTEI-TEN      TO  WRK-ZAI-KHNTEN
           END-IF
           IF    ( WRK-ZAI-KHNSRYCD   IS  NUMERIC )
               MOVE    WRK-ZAI-KHNSRYCD    TO  WRK-SRYCD
               COMPUTE WRK-ZAI-SRYCDTOTAL
                   =   WRK-ZAI-SRYCDTOTAL
                   +   WRK-SRYCD-9
      *
               IF    ( HCM34S01-EXGAIHAKU =    "1" )
                   COMPUTE WRK-ZAI-CNT    =    WRK-ZAI-CNT +   1
                   MOVE    WRK-ZAI-KHNSRYCD
                                           TO  WRK-ZAI-SRYCD
                                                (WRK-ZAI-CNT)
               END-IF
      *
           END-IF
      *
           IF    ( WRK-GAIHAKU-SRYCD   IS  NUMERIC )
               MOVE    WRK-GAIHAKU-SRYCD   TO  WRK-SRYCD
               COMPUTE WRK-ZAI-SRYCDTOTAL
                   =   WRK-ZAI-SRYCDTOTAL
                   +   WRK-SRYCD-9
      *
               COMPUTE WRK-ZAI-CNT =   WRK-ZAI-CNT +   1
               MOVE    WRK-GAIHAKU-SRYCD   TO  WRK-ZAI-SRYCD
                                                (WRK-ZAI-CNT)
      *
           END-IF
      *
           IF    ( FLG-GAIHAKUDAY (IDXD)   =   4 OR 5 OR 6 OR 7
                                            OR 8 OR 9 )
      *
               PERFORM VARYING IDX8    FROM    1   BY  1
                       UNTIL ( IDX8    >   WRK-KSNSRYCD-MAX )
                   IF    ( WRK-KSNSRYCD (IDX8)   IS  NUMERIC )
                       MOVE    WRK-KSNSRYCD (IDX8)
                                       TO  WRK-SRYCD
                       COMPUTE WRK-ZAI-SRYCDTOTAL
                           =   WRK-ZAI-SRYCDTOTAL
                           +   WRK-SRYCD-9
      *
                       COMPUTE WRK-ZAI-CNT =   WRK-ZAI-CNT +   1
                       MOVE    WRK-KSNSRYCD (IDX8)
                                           TO  WRK-ZAI-SRYCD
                                                (WRK-ZAI-CNT)
      *
                   END-IF
               END-PERFORM
      *
               IF    ( WRK-HYOKETU-SRYCD       IS  NUMERIC )
                   MOVE    WRK-HYOKETU-SRYCD   TO  WRK-SRYCD
                   COMPUTE WRK-ZAI-SRYCDTOTAL
                       =   WRK-ZAI-SRYCDTOTAL
                       +   WRK-SRYCD-9
      *
                   COMPUTE WRK-ZAI-CNT =   WRK-ZAI-CNT +   1
                   MOVE    WRK-HYOKETU-SRYCD
                                           TO  WRK-ZAI-SRYCD
                                                (WRK-ZAI-CNT)
      *
               END-IF
           END-IF
      *
           .
       200111-GAIHAKU-SRYCD-GET-EXT.
           EXIT.
      *****************************************************************
      *    Â¾°åÎÅµ¡´Ø¼õ¿Ç»þ³°Çñ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ½èÍý
      *****************************************************************
       2001111-TAKIKAN-SEC             SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN
           WHEN    099 THRU    100
               IF    ( HCM34S01-SANTEIYM   >=  CONST-H2204 )
                    EVALUATE   FLG-GAIHAKUDAY (IDXD)
                    WHEN   4
                        MOVE   "190108890" TO  WRK-GAIHAKU-SRYCD
                    WHEN   5
                        MOVE   "190142190" TO  WRK-GAIHAKU-SRYCD
                    END-EVALUATE
               END-IF
           WHEN    201 THRU    219
           WHEN    221 THRU    222
           WHEN    224
           WHEN    227 THRU    229
           WHEN    230 THRU    239
           WHEN    241 THRU    249
           WHEN    250 THRU    255
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN   4
                   MOVE   "190108790" TO  WRK-GAIHAKU-SRYCD
               WHEN   5
                   MOVE   "190142090" TO  WRK-GAIHAKU-SRYCD
               WHEN   6
                   IF    ( WRK-NYUKHNKSNKBN
                       =   221 OR 232
                        OR 229 OR 238 OR 215 OR 216 OR 217 OR 239 )
                       MOVE   "190153190" TO   WRK-GAIHAKU-SRYCD
                   END-IF
               WHEN   7
                   IF    ( WRK-NYUKHNKSNKBN
                       =   221 OR 232 )
                      MOVE   "190153390"  TO   WRK-GAIHAKU-SRYCD
                   END-IF
               END-EVALUATE
           WHEN    013 THRU    021
           WHEN    101 THRU    109
           WHEN    147 THRU    152
           WHEN    173 THRU    190
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN   4
                   MOVE   "190108890"  TO  WRK-GAIHAKU-SRYCD
               WHEN   5
                   MOVE   "190142190"  TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN    075 THRU    080
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN    4
                   MOVE    "190108990" TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN    085 THRU    086
           WHEN    132 THRU    133
           WHEN    153 THRU    158
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN     4
                   MOVE   "190109090"  TO  WRK-GAIHAKU-SRYCD
               WHEN     5
                   MOVE   "190142290"  TO  WRK-GAIHAKU-SRYCD
               WHEN     7
                   IF    ( WRK-NYUKHNKSNKBN  >= 153 )
                     AND ( WRK-NYUKHNKSNKBN  <= 158 )
                       MOVE   "190153290"  TO  WRK-GAIHAKU-SRYCD
                   END-IF
               END-EVALUATE
           WHEN   226
           WHEN   240
               CONTINUE
           WHEN   031
           WHEN   033
           WHEN   034
           WHEN   035
           WHEN   038
           WHEN   170
           WHEN   191
      *
           WHEN   022
           WHEN   023
           WHEN   024
           WHEN   025
           WHEN   026
           WHEN   029
           WHEN   136
           WHEN   168
           WHEN   169
      *
           WHEN   081
           WHEN   083
           WHEN   172
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN     5
                   MOVE   "190141590"      TO  WRK-GAIHAKU-SRYCD
               WHEN     7
                   MOVE   "190153090"      TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
      *
           WHEN   OTHER
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN     5
                   MOVE   "190141590"  TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
           END-EVALUATE
      *
           .
       2001111-TAKIKAN-EXT.
           EXIT.
      *****************************************************************
      *    Â¾°åÎÅµ¡´Ø¼õ¿Ç»þ³°Çñ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ½èÍý¡ÊÁªÄêÆþ±¡»þ¡Ë
      *****************************************************************
       2001111-SENTEI-TAKIKAN-SEC      SECTION.
      *
           EVALUATE    WRK-NYUKHNKSNKBN
           WHEN    099 THRU  100
               IF    ( HCM34S01-SANTEIYM   >=  CONST-H2204 )
                    EVALUATE   FLG-GAIHAKUDAY (IDXD)
                    WHEN   4
                        MOVE   "190108890" TO  WRK-GAIHAKU-SRYCD
                    WHEN   5
                        MOVE   "190142190" TO  WRK-GAIHAKU-SRYCD
                    END-EVALUATE
               END-IF
           WHEN    201 THRU  219
           WHEN    221 THRU  222
           WHEN    224
           WHEN    227 THRU  229
           WHEN    230 THRU  239
           WHEN    241 THRU  249
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN    4
                   MOVE    "190116590"     TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN    013 THRU    021
           WHEN    101 THRU    109
           WHEN    147 THRU    152
           WHEN    173 THRU    190
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN    4
                   MOVE    "190116690"     TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN    075 THRU  080
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN    4
                   MOVE    "190116890" TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN    085 THRU    086
           WHEN    132 THRU    133
           WHEN    153 THRU    158
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN     4
                   MOVE   "190116790"  TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN   226
           WHEN   240
               CONTINUE
           WHEN   OTHER
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN     5
                   MOVE   "190141590"  TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
           END-EVALUATE
      *
           .
       2001111-SENTEI-TAKIKAN-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÄêÍËÆü¡¦ÆÃÄê»þ´Ö¸º»»¿ÇÎÅ¥³¡¼¥É¼èÆÀ
      *****************************************************************
       2001111-GAIHAKU-GENSAN-SEC      SECTION.
      *
           EVALUATE   FLG-GAIHAKUDAY (IDXD)
           WHEN     8
               EVALUATE   WRK-CHUKSNCD
               WHEN   "9017"
                   MOVE   "190161470"  TO  WRK-GAIHAKU-SRYCD
               WHEN   "9018"
                   MOVE   "190161570"  TO  WRK-GAIHAKU-SRYCD
               WHEN   "9026"
                   MOVE   "190161870"  TO  WRK-GAIHAKU-SRYCD
               WHEN   "9030"
                   MOVE   "190162070"  TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN     9
               EVALUATE   WRK-CHUKSNCD
               WHEN   "9017"
                   MOVE   "190161670"  TO  WRK-GAIHAKU-SRYCD
               WHEN   "9018"
                   MOVE   "190161770"  TO  WRK-GAIHAKU-SRYCD
               WHEN   "9026"
                   MOVE   "190161970"  TO  WRK-GAIHAKU-SRYCD
               WHEN   "9030"
                  MOVE    "190162170"  TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           END-EVALUATE
      *
           .
       2001111-GAIHAKU-GENSAN-EXT.
           EXIT.
      *****************************************************************
      *    ³°ÇñÅÀ¿ô¼èÆÀ½èÍý
      *****************************************************************
       200111-GAIHAKU-TENSU-GET-SEC    SECTION.
      *
           IF  ( FLG-SENTEIDAY (IDXD) NOT  =  ZERO )
      *
               PERFORM 200111-SENTEI-KEISU-GET-SEC
      *
               EVALUATE    FLG-GAIHAKUDAY(IDXD)
               WHEN    1
               WHEN    3
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   WRK-SENTEI-KEISU )  + 0.5
               WHEN    4
                   IF    ( NACCT-SRYYM     >=  CONST-H2204 )
                       COMPUTE WRK-ZAI-TEN
                           = ( WRK-ZAI-KHNTEN  *   0.3 )   +  0.5
                   END-IF
               WHEN    5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.7 )   +  0.5
               WHEN    8
               WHEN    9
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
               END-EVALUATE
      *
               IF    ( FLG-GAIHAKUDAY(IDXD)    =   4 OR 5 OR 8 OR 9 )
                   COMPUTE WRK-ZAI-TEN
                       =   WRK-ZAI-TEN
                       +   WRK-GAIHAKU-KSNTEN
                   COMPUTE WRK-ZAI-TEN
                       =   WRK-ZAI-TEN
                       - ( WRK-ZAI-TEN * ( WRK-HYOKETU-TEN / 100 ))
                       +   0.5
               END-IF
           ELSE
               EVALUATE  FLG-GAIHAKUDAY(IDXD)
               WHEN   1
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.15)   +  0.5
               WHEN   2
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.3 )   +  0.5
               WHEN   4
                   IF    ( NACCT-SRYYM     <=  "200403" )
                       COMPUTE WRK-ZAI-TEN
                           = ( WRK-ZAI-KHNTEN  *   0.15)   +  0.5
                   ELSE
                       COMPUTE WRK-ZAI-TEN
                           = ( WRK-ZAI-KHNTEN  *   0.3 )   +  0.5
                   END-IF
               WHEN   5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.7 )   +  0.5
               WHEN   6
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.45 )  +  0.5
               WHEN   7
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.85 )   +  0.5
               WHEN    8
               WHEN    9
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
               END-EVALUATE
      *
               IF    ( FLG-GAIHAKUDAY(IDXD)    =   4 OR 5 OR 6 OR 7
                                                OR 8 OR 9 )
                   COMPUTE WRK-ZAI-TEN
                       =   WRK-ZAI-TEN
                       +   WRK-GAIHAKU-KSNTEN
                   COMPUTE WRK-ZAI-TEN
                       =   WRK-ZAI-TEN
                       - ( WRK-ZAI-TEN * ( WRK-HYOKETU-TEN / 100 ))
                       +   0.5
               END-IF
           END-IF
      *
           .
       200111-GAIHAKU-TENSU-GET-EXT.
           EXIT.
      *****************************************************************
      *    ÁªÄê³°Çñ»þ¤Î·¸¿ô¥»¥Ã¥È½èÍý
      *****************************************************************
       200111-SENTEI-KEISU-GET-SEC     SECTION.
      *
           MOVE    ZERO                TO  WRK-SENTEI-KEISU
      *
      *    ÁªÄêÆþ±¡¤Î¾ì¹ç¤Î·¸¿ô¤òµá¤á¤Æ¤ª¤¯
           EVALUATE    TRUE
           WHEN (  NACCT-SRYYM         <=  "200303" )
               MOVE   0.1           TO     WRK-SENTEI-KEISU
           WHEN (  NACCT-SRYYM         >=    "200304" )
            AND (  NACCT-SRYYM         <=    "200403" )
               MOVE   0.05           TO    WRK-SENTEI-KEISU
           WHEN (  NACCT-SRYYM         >=    "200404" )
               MOVE   ZERO          TO     WRK-SENTEI-KEISU
           END-EVALUATE
           .
      *
       200111-SENTEI-KEISU-GET-EXT.
           EXIT.
      *****************************************************************
      *    ³°Çñ´ðËÜ¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       200111-GAIHAKKHNINF-SET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-OVER
           MOVE    ZERO                TO  IDXB
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WGAIHAKU-OCC2-MAX (IDXA) )
                     OR  ( IDXB    >   CONST-WGAIHAKU-MAX )
                     OR (( WRK-ZAI-SRYCDTOTAL
                                   =   WGAIHAKU-SRYCDTOTAL (IDXA IDXB)) 
                      AND( WRK-ZAI-TEN
                                   =   WGAIHAKU-ZAITEN(IDXA IDXB)))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF   (    IDXB  >   WGAIHAKU-OCC2-MAX (IDXA) )
               COMPUTE WGAIHAKU-OCC2-MAX (IDXA)
                   =   WGAIHAKU-OCC2-MAX (IDXA)    +   1
               IF    ( WGAIHAKU-OCC2-MAX (IDXA)
                                           >   CONST-WGAIHAKU-MAX )
                   MOVE    CONST-WGAIHAKU-MAX
                                       TO  WGAIHAKU-OCC2-MAX (IDXA)
                   MOVE    1           TO  FLG-OVER
               ELSE
                   MOVE    WGAIHAKU-OCC2-MAX (IDXA)    TO  IDXB
                   MOVE    NACCT-SRYKBN
                                       TO  WGAIHAKU-SRYKBN (IDXA IDXB)
                   MOVE    WRK-ZAI-TEN
                                       TO  WGAIHAKU-ZAITEN (IDXA IDXB)
                   MOVE    WRK-ZAI-SRYCDTOTAL
                                       TO  WGAIHAKU-SRYCDTOTAL
                                                           (IDXA IDXB)
      *
                   PERFORM VARYING IDX9    FROM    1   BY  1
                           UNTIL ( IDX9    >   WRK-ZAI-CNT )
      *
                       MOVE    WRK-ZAI-SRYCD (IDX9)
                                   TO  WGAIHAKU-SRYCD (IDXA IDXB IDX9)
      *
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       200111-GAIHAKKHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ´ðËÜ¾ðÊóÀßÄê½èÍý
      *****************************************************************
       2001111-NYUINKHNINF-SET-SEC     SECTION.
      *
           COMPUTE WNYUINRYO-OCC2-MAX (IDXA)
               =   WNYUINRYO-OCC2-MAX (IDXA)   +   1
      *
           MOVE    WNYUINRYO-OCC2-MAX (IDXA)   TO  IDXB
      *
           MOVE    NACCT-ZAINUM        TO  WNYUINRYO-ZAINUM
                                                           (IDXA IDXB)
           MOVE    NACCT-SRYKBN        TO  WNYUINRYO-SRYKBN
                                                           (IDXA IDXB)
           MOVE    NACCT-ZAITEN        TO  WNYUINRYO-ZAITEN
                                                           (IDXA IDXB)
           PERFORM 20011-NYUINACT-GET-SEC
           IF    ( FLG-NYUINACT        NOT =    ZERO )
               GO  TO  2001111-NYUINKHNINF-SET-EXT
           END-IF
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC                >   5 )
                    OR   ( NSRY-SRYCD (IDXC)   =   SPACE )
      *
               MOVE    NSRY-SRYCD (IDXC)
                                       TO  WNYUINRYO-SRYCD
                                                       (IDXA IDXB IDXC)
      *
           END-PERFORM
      *
           .
       2001111-NYUINKHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¼èÆÀ½èÍý
      *****************************************************************
       20011-NYUINACT-GET-SEC          SECTION.
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¸¡º÷½èÍý
           MOVE    ZERO                TO  FLG-NYUINACT
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM        TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACT
           ELSE
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           END-IF
      *
      *    Æþ±¡¹Ô°Ù¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-NYUINACT-GET-EXT.
           EXIT.
      *****************************************************************
      *    ÁªÄêÆþ±¡ÎÁÃÖ¤­´¹¤¨½èÍý
      *****************************************************************
       2001111-SENTEI-CHG-SEC          SECTION.
      *
           PERFORM 20011111-SENTEI-CHG-SEC
      *
           IF    ( WRK-HYOKETU-TEN NOT =   ZERO )
      *
      *        É¸·ç¡§¡ó¸º»»
               COMPUTE WRK-SENTEI-KHNTEN
                   =   WRK-SENTEI-KHNTEN
                   -   WRK-SENTEI-KHNTEN * ( WRK-HYOKETU-TEN / 100 )
                   +  0.5
      *
               COMPUTE WRK-SENTEI-ZAITEN
                   =   WRK-SENTEI-ZAITEN
                   -   WRK-SENTEI-ZAITEN * ( WRK-HYOKETU-TEN / 100 )
                   +  0.5
      *
               COMPUTE WRK-KHNTEN
                   =   WRK-KHNTEN
                   -   WRK-KHNTEN * ( WRK-HYOKETU-TEN / 100)
                   +  0.5
           END-IF
      *
           MOVE    WRK-SENTEISRYCD     TO  WNYUINRYO-SRYCD
                                                   (IDXA IDXB IDX-KHN)
      *
           MOVE    WRK-SENTEI-ZAITEN   TO  WNYUINRYO-ZAITEN (IDXA IDXB)
      *
           .
       2001111-SENTEI-CHG-EXT.
           EXIT.
      *****************************************************************
      *    ÁªÄêÆþ±¡ÎÁÃÖ¤­´¹¤¨½èÍý
      *****************************************************************
       20011111-SENTEI-CHG-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SENTEISRYCD
      *
           MOVE    ZERO                TO  WRK-KHNTEN
                                           WRK-KSNTEN
                                           WRK-SENTEI-ZAITEN
                                           WRK-SENTEI-KHNTEN
                                           WRK-HYOKETU-TEN
      *
      *    Æþ±¡´ðËÜÎÁ¤Î¥³¡¼¥É¤òÃµ¤¹
           MOVE    ZERO                TO  IDX-KHN
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   5 )
               MOVE    NSRY-SRYCD (IDXC)
                                       TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   IF    ( TNS-DATAKBN =   1 )
                       MOVE    TNS-TEN TO  WRK-KHNTEN
                       MOVE    IDXC    TO  IDX-KHN
                   ELSE
                       EVALUATE  TNS-TENSIKIBETU
                       WHEN     3
                           MOVE  TNS-TEN   TO  WRK-KSNTEN
                       WHEN     8
                           COMPUTE WRK-KSNTEN  =   TNS-TEN *   -1
                       WHEN     6
                           MOVE  TNS-TEN   TO  WRK-HYOKETU-TEN
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( IDX-KHN             =   ZERO )
               MOVE    9               TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ²Ã»»Ê¬¤ÎÅÀ¿ô¤òµá¤á¤ë
      *    COMPUTE WRK-KSNTEN  =   NACCT-ZAITEN    -   WRK-KHNTEN
      *
      *    ¾å¤Çµá¤á¤¿Æþ±¡´ðËÜÎÁ¤Î¥³¡¼¥É¤òÁªÄêÆþ±¡ÎÁ¤Î¥³¡¼¥É¤ËÃÖ¤­´¹¤¨¤ë
           PERFORM 900-SENTEICDCHG-KEY2-SEL-SEC
           IF    ( FLG-SENTEICDCHG NOT =   ZERO )
               MOVE    10              TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    SENCHG-SENTEISRYCD  TO  WRK-SENTEISRYCD
      *
      *    ÅÀ¿ô¤â²þ¤á¤Æµá¤á¤Ê¤ª¤¹
           MOVE    SENCHG-SENTEISRYCD  TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               MOVE    11              TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    TNS-TEN             TO  WRK-SENTEI-KHNTEN
           COMPUTE WRK-SENTEI-ZAITEN
               =   WRK-SENTEI-KHNTEN   +   WRK-KSNTEN
      *
           .
       20011111-SENTEI-CHG-EXT.
           EXIT.
      *****************************************************************
      *    ÁªÄêÆþ±¡ÎÁ´µ¼Ô¼«¸ÊÉéÃ´ÊÔ½¸½èÍý
      *****************************************************************
       200111-SENTEIPTFTN-HEN-SEC      SECTION.
      *
      *    Æþ±¡´µ¼Ô¤ÎÏ·¿ÍÈ½Äê
           INITIALIZE   ORCSAGECHKAREA
           MOVE    HCM34S01-HOSPNUM     TO  AGECHK-HOSPNUM
           MOVE    HCM34S01-PTID       TO  AGECHK-PTID
      *
           MOVE    HCM34S01-SANTEIYM   TO  WRK-YYMM
           MOVE    1                   TO  WRK-DD
           PERFORM VARYING IDXD    FROM    1   BY  1
               UNTIL ( IDXD        >   31 )
               IF    ( FLG-NYUINDAY (IDXD) =   1 )
                   MOVE    IDXD        TO  WRK-DD
                   MOVE    31          TO  IDXD
               END-IF
           END-PERFORM
           MOVE    WRK-YMD             TO  AGECHK-SRYYMD
      *
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      ( AGECHK-RC    =    ZERO )
               CONTINUE
           ELSE
               INITIALIZE    ORCSAGECHKAREA
           END-IF
      *
           MOVE    ZERO                TO  FLG-TAISYO
           PERFORM VARYING IDXD    FROM    1   BY  1
               UNTIL ( IDXD        >   31 )
      *
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                 AND ( FLG-NYUINDAY (IDXD)     =   1 )
                 AND ( FLG-NYUINRYODAY (IDXD)  =   1 )
                 AND ( FLG-SENTEIDAY(IDXD)     =   1 )
                   IF    ( FLG-TAISYO          =   ZERO )
                       MOVE    1       TO  FLG-TAISYO
                       PERFORM 2001111-SENTEI-FTN-TNS-SET-SEC
                   END-IF
      *
      *            ÁªÄêÆþ±¡ÎÁ´µ¼Ô¼«¸ÊÉéÃ´³ÛÊÔ½¸½èÍý
                   EVALUATE    SYS-5000-SENTEI-FTNKBN
                   WHEN    "1"
                   WHEN    "2"
                       MOVE    SYS-5000-SENTEI-FTNKBN
                                           TO  WRK-SENTEI-FTNKBN
                       IF    ( SYS-5000-SENTEI-TAXKBN      =   "1" )
                           MOVE    2       TO  IDX-TAX
                       ELSE
                           MOVE    1       TO  IDX-TAX
                       END-IF
                       MOVE    SYS-5000-SENTEI-TANKA-X
                                           TO  WRK-SENTEI-TANKA-X
                       IF  (SYS-5000-SENTEI-KINGAKU-X NOT = SPACE) AND
                           (SYS-5000-SENTEI-KINGAKU-X NOT = "0000")
                           MOVE    SYS-5000-SENTEI-KINGAKU
                                              TO  WRK-SENTEI-KINGAKU
                       ELSE
                         IF   AGECHK-ROUJIN2    =   0
                           IF  SYS-5000-SENTEI-IPN-KINGAKU-X  =  SPACE
                               MOVE   ZERO TO  WRK-SENTEI-KINGAKU
                           ELSE
                               MOVE   SYS-5000-SENTEI-IPN-KINGAKU
                                           TO  WRK-SENTEI-KINGAKU
                           END-IF
                         ELSE
                           IF  SYS-5000-SENTEI-RJN-KINGAKU-X  =  SPACE
                               MOVE   ZERO  TO  WRK-SENTEI-KINGAKU
                           ELSE
                               MOVE   SYS-5000-SENTEI-RJN-KINGAKU
                                                TO  WRK-SENTEI-KINGAKU
                           END-IF
                         END-IF
                       END-IF
                       PERFORM 2001111-SENTEI-PTFTN-SEC
                   WHEN    "3"
                       MOVE    SBTUSENTEI-DAY-FTNKBN   (IDXD)
                                           TO  WRK-SENTEI-FTNKBN
                       IF    ( SBTUSENTEI-DAY-TAXKBN   (IDXD)  =   "1" )
                           MOVE    2       TO  IDX-TAX
                       ELSE
                           MOVE    1       TO  IDX-TAX
                       END-IF
                       MOVE    SBTUSENTEI-DAY-TANKA    (IDXD)
                                           TO  WRK-SENTEI-TANKA
                       MOVE    SBTUSENTEI-DAY-KINGAKU  (IDXD)
                                           TO  WRK-SENTEI-KINGAKU
                       PERFORM 2001111-SENTEI-PTFTN-SEC
                   END-EVALUATE
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-SENTEIPTFTN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ÁªÄêÆþ±¡ÎÁ´µ¼Ô¼«¸ÊÉéÃ´ÅÀ¿ô¾ðÊóÀßÄê½èÍý
      *****************************************************************
       2001111-SENTEI-FTN-TNS-SET-SEC  SECTION.
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¼èÆÀ
           PERFORM 20011-NYUINACT-GET-SEC
           IF    ( FLG-NYUINACT        NOT =    ZERO )
               GO  TO  2001111-SENTEI-FTN-TNS-SET-EXT
           END-IF
      *
      *    ÁªÄêÆþ±¡ÎÁ¤ËÃÖ¤­´¹¤¨¡ÊWRK-KHNTEN¤â¼èÆÀ¡Ë
           PERFORM 20011111-SENTEI-CHG-SEC
      *
      *    ´Ç¸îÈæÎ¨´ð½àÌ¤Ã£¤Î¥Á¥§¥Ã¥¯
           PERFORM 2001111-KNGMITATU-CHK-SEC
           IF    ( WRK-GAIHAKU-KHNTEN  NOT =   ZERO )
               MOVE    WRK-GAIHAKU-KHNTEN  TO  WRK-KHNTEN
           END-IF
      *
      *    ÉÂÅïÀßÄê¤ÎÁªÄê¼«¸ÊÉéÃ´¾ðÊó¤Î¼èÆÀ
           IF    SYS-5000-SENTEI-FTNKBN   =   "3"
               INITIALIZE                  ORCSBTUSENTEIAREA
               MOVE    HCM34S01-HOSPNUM     TO  SBTUSENTEI-HOSPNUM
               MOVE    HCM34S01-PTID       TO  SBTUSENTEI-PTID
               MOVE    HCM34S01-SANTEIYM   TO  SBTUSENTEI-SRYYM
               CALL    "ORCSBTUSENTEI"     USING
                                           ORCSBTUSENTEIAREA
                                           SPA-AREA
           END-IF
      *
           .
       2001111-SENTEI-FTN-TNS-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÁªÄêÆþ±¡ÎÁ´µ¼Ô¼«¸ÊÉéÃ´ÀßÄê½èÍý
      *****************************************************************
       2001111-SENTEI-PTFTN-SEC        SECTION.
      *
           MOVE       ZERO             TO  WRK-SENTEI-GAIHAKU1
                                           WRK-SENTEI-GAIHAKU2
      *
           PERFORM     200111-SENTEI-KEISU-GET-SEC
      *
           EVALUATE    WRK-SENTEI-FTNKBN
      *    ÄêÎ¨ÀßÄê
           WHEN   "1"
               IF    ( WRK-SENTEI-TANKA-X      =   SPACE )
                   MOVE   10           TO  WRK-SENTEI-TANKA
               END-IF
      *
               EVALUATE    FLG-GAIHAKUDAY(IDXD)
               WHEN    ZERO
               WHEN    3
               WHEN    4
               WHEN    5
               WHEN    8
               WHEN    9
                   IF    ( NACCT-SRYYM     >=  CONST-H1604 )
                    AND  ( NACCT-SRYYM     <   CONST-H2204 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)    =   4 )
                       CONTINUE
                   ELSE
                       IF    ( IDX-TAX             =   1 )
                           COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                               =   WRK-SENTEI-PTFTN (IDX-TAX)
                               + ( WRK-KHNTEN  -   WRK-SENTEI-KHNTEN )
                               *   WRK-SENTEI-TANKA
                       ELSE
                           IF    ( SYS-1001-TAX-HASU-KBN   =   "1" )
                               COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                                   =   WRK-SENTEI-PTFTN (IDX-TAX)
                                   + ( WRK-KHNTEN - WRK-SENTEI-KHNTEN )
                                   *   WRK-SENTEI-TANKA
                                   *   1.05
                                   +   0.5
                           ELSE
                               COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                                   =   WRK-SENTEI-PTFTN (IDX-TAX)
                                   + ( WRK-KHNTEN - WRK-SENTEI-KHNTEN )
                                   *   WRK-SENTEI-TANKA
                                   *   1.05
                           END-IF
                       END-IF
                   END-IF
               END-EVALUATE
      *    Äê³ÛÀßÄê
           WHEN    "2"
      *
               IF    ( WRK-SENTEI-KINGAKU-X   =   SPACE )
                   MOVE    ZERO        TO  WRK-SENTEI-KINGAKU
               END-IF
      *
               EVALUATE   FLG-GAIHAKUDAY(IDXD)
               WHEN    ZERO
               WHEN    3
               WHEN    4
               WHEN    5
               WHEN    8
               WHEN    9
                   IF    ( IDX-TAX             =   1 )
                       COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                           =   WRK-SENTEI-PTFTN (IDX-TAX)
                           +   WRK-SENTEI-KINGAKU
                   ELSE
                       IF    ( SYS-1001-TAX-HASU-KBN   =   "1" )
                           COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                               =   WRK-SENTEI-PTFTN (IDX-TAX)
                               +   WRK-SENTEI-KINGAKU
                               *   1.05
                               +   0.5
                       ELSE
                           COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                               =   WRK-SENTEI-PTFTN (IDX-TAX)
                               +   WRK-SENTEI-KINGAKU
                               *   1.05
                       END-IF
                   END-IF
               END-EVALUATE
      *
           END-EVALUATE
      *
           .
       2001111-SENTEI-PTFTN-EXT.
           EXIT.
      *****************************************************************
      *    ¼¼ÎÁº¹³ÛÊÔ½¸½èÍý
      *****************************************************************
       2001-BRMSAGAKU-HEN-SEC          SECTION.
      *
      *    ¼¼ÎÁº¹³Û¾ðÊó¼èÆÀ½èÍý
           PERFORM 20011-SYS-5113-GET-SEC
      *
           MOVE    "3"                 TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-NYUINACCT       NOT =   ZERO )
               GO  TO  2001-BRMSAGAKU-HEN-EXT
           END-IF
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    ZERO                TO  FLG-SANTEI
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                AND  ( FLG-NYUINDAY (IDXD)     =   1 )
      *
                    IF    (  FLG-BRMSGKSANTEIDAY (IDXD)    =   1 )
                            MOVE   1       TO  FLG-SANTEI
                    ELSE
                        IF   ( FLG-NYUINRYODAY (IDXD)  =   1 )
                            MOVE   1       TO  FLG-SANTEI
                        END-IF
                    END-IF
                END-IF
      *
               IF    ( FLG-SANTEI          =   1 )
      *
      *            ¼¼ÎÁº¹³Û´ðËÜ¾ðÊóÀßÄê½èÍý
                   PERFORM 20011-BRMSAGAKU-KHNINF-SET-SEC
                   IF    ( IDXB        >   ZERO )
                     AND ( FLG-OVER    =   ZERO )
                       MOVE    1       TO  WBRMSAGAKU-DAY
                                                   (IDXB IDXD)
                       COMPUTE WBRMSAGAKU-ZAIKAISU (IDXB)
                           =   WBRMSAGAKU-ZAIKAISU (IDXB)   +   1
                       IF    ( WBRMSAGAKU-NUM (IDXB)    =   ZERO )
                               MOVE    IDXD
                                       TO  WBRMSAGAKU-NUM (IDXB)
                       ELSE
                           IF    ( IDXD    <   WBRMSAGAKU-NUM (IDXB))
                               MOVE    IDXD
                                       TO  WBRMSAGAKU-NUM (IDXB)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2001-BRMSAGAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¼¼ÎÁº¹³Û¾ðÊó¼èÆÀ½èÍý
      *****************************************************************
       20011-SYS-5113-GET-SEC          SECTION.
      *
           INITIALIZE                      WRK-SYS-5113
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-5113-REC
           MOVE    HCM34S01-HOSPNUM    TO  SYS-5113-HOSPNUM
           MOVE    "5113"              TO  SYS-5113-KANRICD
           MOVE    SYS-5113-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-5113-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( WRK-SYS-5113-MAX   >=   30 )
      *
               IF    ( SYS-5113-STYUKYMD (1:6)
                                       <=  HCM34S01-SANTEIYM )
                AND  ( SYS-5113-EDYUKYMD (1:6)
                                       >=  HCM34S01-SANTEIYM )
                   COMPUTE WRK-SYS-5113-MAX
                       =   WRK-SYS-5113-MAX    +   1
                   MOVE    WRK-SYS-5113-MAX    TO  IDX3
      *
                   MOVE    SYS-5113-STYUKYMD
                                       TO  WRK-SYS-5113-STYUKYMD (IDX3)
                   MOVE    SYS-5113-EDYUKYMD
                                       TO  WRK-SYS-5113-EDYUKYMD (IDX3)
                   MOVE    SYS-5113-KBNCD (1:2)
                                       TO  WRK-SYS-5113-KBNCD    (IDX3)
                   MOVE    SYS-5113-BRM-SAGAKU-NM
                                       TO  WRK-SYS-5113-BRMSAGAKU(IDX3)
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-5113-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-SYS-5113-GET-EXT.
           EXIT.
      *****************************************************************
      *    ¼¼ÎÁº¹³Û´ðËÜ¾ðÊóÀßÄê½èÍý
      *****************************************************************
       20011-BRMSAGAKU-KHNINF-SET-SEC  SECTION.
      *
           MOVE    NACCT-SRYYM         TO  WRK-YYMM
           MOVE    IDXD                TO  WRK-DD
           MOVE    ZERO                TO  WRK-BRMSAGAKU
           MOVE    ZERO                TO  FLG-OVER
           MOVE    ZERO                TO  IDXB
      *
           MOVE    ZERO                TO  FLG-HIT
           PERFORM VARYING IDX3    FROM    1   BY    1
                   UNTIL ( IDX3        >   WRK-SYS-5113-MAX )
                    OR   ( FLG-HIT NOT =   ZERO )
      *
               IF    ( WRK-SYS-5113-STYUKYMD (IDX3) <= WRK-YMD )
                AND  ( WRK-SYS-5113-EDYUKYMD (IDX3) >= WRK-YMD )
                AND  ( WRK-SYS-5113-KBNCD    (IDX3)  = NACCT-DAY (IDXD))
                   MOVE    1           TO  FLG-HIT
                   MOVE    WRK-SYS-5113-BRMSAGAKU(IDX3)
                                       TO  WRK-BRMSAGAKU
                   MOVE    WRK-SYS-5113-KBNCD    (IDX3)
                                       TO  WRK-BRMSAGAKUCD
                   IF    ( HCM34S01-SANTEIYM   >=  CONST-H1904 )
                       IF    ( SYS-5000-BRMSGK-TAXKBN  =   "1" )
                           COMPUTE WRK-BRMSAGAKU
                               =   WRK-BRMSAGAKU   *   1.05
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-HIT             =   ZERO )
               GO  TO  20011-BRMSAGAKU-KHNINF-SET-EXT
           END-IF
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WBRMSAGAKU-OCC1-MAX )
                     OR  ( IDXB    >   CONST-WBRMSAGAKU-MAX )
                     OR  ( WRK-BRMSAGAKUCD
                                   =   WBRMSAGAKU-SAGAKUCD(IDXB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF   (    IDXB  >   WBRMSAGAKU-OCC1-MAX )
      *
               COMPUTE WBRMSAGAKU-OCC1-MAX
                   =   WBRMSAGAKU-OCC1-MAX +   1
               IF    ( WBRMSAGAKU-OCC1-MAX >   CONST-WBRMSAGAKU-MAX )
                   MOVE    CONST-WBRMSAGAKU-MAX
                                       TO  WBRMSAGAKU-OCC1-MAX
                   MOVE    1           TO  FLG-OVER
               ELSE
                   MOVE    WBRMSAGAKU-OCC1-MAX TO  IDXB
                   MOVE    NACCT-SRYKBN 
                                       TO  WBRMSAGAKU-SRYKBN (IDXB)
                   MOVE    WRK-BRMSAGAKU
                                       TO  WBRMSAGAKU-ZAITEN (IDXB)
                   MOVE    WRK-BRMSAGAKUCD
                                       TO  WBRMSAGAKU-SAGAKUCD(IDXB)
                   MOVE    NACCT-SRYCDTOTAL
                                       TO  WRK-SRYCDTOTAL
                   MOVE    WRK-SRYCDTOTAL
                                       TO  WBRMSAGAKU-SRYCD  (IDXB)
               END-IF
           END-IF
      *
           .
       20011-BRMSAGAKU-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÊÔ½¸½èÍý
      *****************************************************************
       2001-SYOKUJI-HEN-SEC            SECTION.
      *
      *    ¿©»ö´ðËÜÅÀ¿ô¼èÆÀ½èÍý
           PERFORM 20011-SKJ-KHNTEN-GET-SEC
      *
      *    ¿©Æ²²Ã»»¥Á¥§¥Ã¥¯
           PERFORM 20011-SYOKUDOKSN-CHK-SEC
      *
           MOVE    "4"                 TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
      *
               IF    ( NACCT-SRYCDTOTAL    =   197000570 )
                   CONTINUE
               ELSE
      *            ¿©»öÊÔ½¸½èÍý
                   PERFORM 20011-SYOKUJI-HEN-SEC
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-SYOKUJI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö´ðËÜÅÀ¿ô¼èÆÀ½èÍý
      *****************************************************************
       20011-SKJ-KHNTEN-GET-SEC        SECTION.
      *
           INITIALIZE                  WRK-SKJ-TEN-G
      *
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
           MOVE    CONST-SKJ-RYOYO1    TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-RYOYO1
           END-IF
      *
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
           MOVE    CONST-SKJ-RYOYO2    TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-RYOYO2
           END-IF
      *
      *    ÆÃÊÌ´ÉÍý²Ã»»
           MOVE    CONST-SKJ-TOKKANRI  TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-TOKKANRI
           END-IF
      *
      *    ÆÃÊÌ¿©²Ã»»
           MOVE    CONST-SKJ-TOKSYOKU  TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-TOKSYOKU
           END-IF
      *
      *    ¿©Æ²²Ã»»
           MOVE    CONST-SKJ-SYOKUDO   TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-SYOKUDO
           END-IF
      *
      *    Ê£¿ô¸¥Î©²Ã»»
           MOVE    CONST-SKJ-FKSKONDATE
                                       TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-FKSKONDATE
           END-IF
      *
      *    ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
           MOVE    CONST-SKJFTN-IPN    TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-SKJFTN-IPN
           END-IF
      *
      *    ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
           MOVE    CONST-SKJFTN-TEI2    TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-SKJFTN-TEI2
           END-IF
      *
      *    ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
           MOVE    CONST-SKJFTN-TEI2-90 TO WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-SKJFTN-TEI2-90
           END-IF
      *
      *    ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
           MOVE    CONST-SKJFTN-TEI1   TO WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-SKJFTN-TEI1
           END-IF
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
             IF    ( HCM34S01-HKNNUM(1:2)  NOT =   "98" )
               IF    ( FLG-NYUINDAY (IDXD)     =   1 OR 2 )
                OR   ( LNK-NYUINDAY-KBN(IDXD)  =   "1" )
      *
                   EVALUATE    LNK-SYOTOKU-SKJKBN(IDXD)
      *            Äã½êÆÀ£²
                   WHEN    "2"
                       MOVE   CONST-SKJFTN-TEI2
                                         TO  TSKJFTN-SRYCD(IDXD)
                       MOVE   WRK-SKJ-TEN-SKJFTN-TEI2
                                         TO  TSKJFTN-MONEY(IDXD)
      *            Äã½êÆÀ£²¡Ê£¹£°Æü±Û¤¨¡Ë
                   WHEN    "3"
                       MOVE   CONST-SKJFTN-TEI2-90
                                         TO  TSKJFTN-SRYCD(IDXD)
                       MOVE   WRK-SKJ-TEN-SKJFTN-TEI2-90
                                         TO  TSKJFTN-MONEY(IDXD)
      *            Äã½êÆÀ£±
                   WHEN    "1"
                   WHEN    "4"
                       MOVE   CONST-SKJFTN-TEI1
                                         TO  TSKJFTN-SRYCD(IDXD)
                       MOVE   WRK-SKJ-TEN-SKJFTN-TEI1
                                         TO  TSKJFTN-MONEY(IDXD)
                   WHEN    OTHER
                       MOVE   CONST-SKJFTN-IPN
                                         TO  TSKJFTN-SRYCD(IDXD)
                       MOVE   WRK-SKJ-TEN-SKJFTN-IPN
                                         TO  TSKJFTN-MONEY(IDXD)
                   END-EVALUATE
               END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       20011-SKJ-KHNTEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    À¸³è´ðËÜÅÀ¿ô¼èÆÀ½èÍý
      *****************************************************************
       20011-LIFE-KHNTEN-GET-SEC        SECTION.
      *
           INITIALIZE                  WRK-LIFE-TEN-G
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
           MOVE    CONST-LIFE-RYOYO1   TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-LIFE-TEN-RYOYO1
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë´Ä¶­ÎÅÍÜ
           MOVE    CONST-LIFE-KANKYO1  TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-LIFE-TEN-KANKYO1
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
           MOVE    CONST-LIFE-RYOYO2   TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-LIFE-TEN-RYOYO2
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë´Ä¶­ÎÅÍÜ
           MOVE    CONST-LIFE-KANKYO2  TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-LIFE-TEN-KANKYO2
           END-IF
      *
      *    ÆÃÊÌ¿©²Ã»»
           MOVE    CONST-LIFE-TOKSYOKU TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-LIFE-TEN-TOKSYOKU
           END-IF
      *
      *    ¿©Æ²²Ã»»
           MOVE    CONST-LIFE-SYOKUDO  TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-LIFE-TEN-SYOKUDO
           END-IF
      *
           .
       20011-LIFE-KHNTEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©Æ²²Ã»»¥Á¥§¥Ã¥¯
      *****************************************************************
       20011-SYOKUDOKSN-CHK-SEC        SECTION.
      *
           INITIALIZE                          FLG-SYOKUDODAY-G
      *
           MOVE    "4"                     TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF   ( NACCT-SRYCDTOTAL =   197000570 )
                   PERFORM VARYING     IDX1    FROM    1   BY  1
                           UNTIL     ( IDX1    >   31 )
                       IF    ( NACCT-DAY(IDX1)  >   ZERO )
                           MOVE    1       TO  FLG-SYOKUDODAY (IDX1)
                       END-IF
                   END-PERFORM
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-SYOKUDOKSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÊÔ½¸½èÍý
      *****************************************************************
       20011-SYOKUJI-HEN-SEC           SECTION.
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    ZERO                TO  FLG-SANTEI
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                AND  ( FLG-NYUINDAY (IDXD)     =   1 )
                AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR 4 OR 5
                                                        OR 6 OR 7
                                                        OR 8 OR 9 )
                    IF   ( FLG-NYUINRYODAY (IDXD)  =   1 )
                     OR  ( LNK-NYUINDAY-KBN(IDXD)  =   "1" )
                        MOVE   1       TO  FLG-SANTEI
                    END-IF
               END-IF
      *
               IF    ( FLG-SANTEI          =   1 )
      *
      *            ¿©»ö¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ½èÍý
                   PERFORM 200111-SYOKUJI-SRYCD-GET-SEC
      *
      *            ¿©»ö´ðËÜ¾ðÊóÀßÄê½èÍý
                   PERFORM 200111-SYOKUJI-KHNINF-SET-SEC
                   IF    ( IDXB        >   ZERO )
                     AND ( FLG-OVER    =   ZERO )
                       MOVE    1       TO  WSYOKUJI-DAY
                                                   (IDXB IDXD)
                       COMPUTE WSYOKUJI-ZAIKAISU (IDXB)
                           =   WSYOKUJI-ZAIKAISU (IDXB)    +   1
                       IF    ( WSYOKUJI-NUM (IDXB)     =   ZERO )
                               MOVE    IDXD
                                       TO  WSYOKUJI-NUM (IDXB)
                       ELSE
                           IF    ( IDXD    <   WSYOKUJI-NUM (IDXB))
                               MOVE    IDXD
                                       TO  WSYOKUJI-NUM (IDXB)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20011-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ½èÍý
      *****************************************************************
       200111-SYOKUJI-SRYCD-GET-SEC    SECTION.
      *
      *
           INITIALIZE                      WRK-SKJ-SRYCD-G
           MOVE    ZERO                TO  IDX5
      *
           IF    ( FLG-EXSYOKUJI       =   ZERO )
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
      *
      *                    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                           PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *                    ÆÃÊÌ´ÉÍý²Ã»»
                           PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
      *
      *                    ¿©Æ²²Ã»»
                           PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                       WHEN  2
      *
      *                    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                           PERFORM 2001111-SKJ-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN  2
      *
      *                Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                       PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *                ÆÃÊÌ´ÉÍý²Ã»»
                       PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
      *
      *                ÆÃÊÌ¿©²Ã»»
                       PERFORM 2001111-SKJ-TOKSYOKU-SET-SEC
      *
      *                ¿©Æ²²Ã»»
                       PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                   WHEN  3
      *
      *                Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                       PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *                ÆÃÊÌ´ÉÍý²Ã»»
                       PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
      *
      *                ¿©Æ²²Ã»»
                       PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
      *
      *                Ê£¿ô¸¥Î©²Ã»»
                       PERFORM 2001111-SKJ-FKSKONDATE-SET-SEC
      *
                   WHEN  4
      *
      *                Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                       PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *                ÆÃÊÌ´ÉÍý²Ã»»
                       PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
      *
      *                ÆÃÊÌ¿©²Ã»»
                       PERFORM 2001111-SKJ-TOKSYOKU-SET-SEC
      *
      *                ¿©Æ²²Ã»»
                       PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
      *
      *                Ê£¿ô¸¥Î©²Ã»»
                       PERFORM 2001111-SKJ-FKSKONDATE-SET-SEC
      *
                   END-EVALUATE
               WHEN  2
                   PERFORM 2001111-SKJ-SKJFTN-SET-SEC
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
      *
                   IF    ( NACCT-DAY (IDXD)    =   1  OR 2 )
                       EVALUATE    WRK-SKJCOMMENTCD
                       WHEN    CONST-SKJ-ASA
                           PERFORM 2001111-SKJ-ASA-SET-SEC
                       WHEN    CONST-SKJ-HIRU
                           PERFORM 2001111-SKJ-HIRU-SET-SEC
                       WHEN    CONST-SKJ-YUU
                           PERFORM 2001111-SKJ-YUU-SET-SEC
                       END-EVALUATE
                   END-IF
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
      *
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
                           PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
                       WHEN    2
                           PERFORM 2001111-SKJ-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN    2
                       PERFORM 2001111-SKJ-RYOYO1-SET-SEC
                       PERFORM 2001111-SKJ-TOKSYOKU-SET-SEC
                   END-EVALUATE
               WHEN    1
                   IF    ( WRK-SHOKUJIRYOYO    =   1 )
                    AND  ( NACCT-DAY (IDXD)    =   1  OR 2 )
                       PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                   END-IF
               WHEN    2
                   PERFORM 2001111-SKJ-SKJFTN-SET-SEC
               END-EVALUATE
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL ( IDX5    >   5 )
               IF    ( WRK-SKJ-SRYCD (IDX5)    NOT =   SPACE )
                   MOVE    WRK-SKJ-SRYCD (IDX5)    TO  WRK-SRYCD-NUM
                   COMPUTE WRK-SKJ-SRYCDTOTAL  =   WRK-SKJ-SRYCDTOTAL
                                               +   WRK-SRYCD-NUM
               END-IF
           END-PERFORM
      *
           .
       200111-SYOKUJI-SRYCD-GET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÄ«¥³¥á¥ó¥ÈÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-ASA-SET-SEC         SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-ASA   TO  WRK-SKJ-SRYCD (IDX5)
           END-IF
      *
           .
       2001111-SKJ-ASA-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÃë¥³¥á¥ó¥ÈÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-HIRU-SET-SEC         SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-HIRU  TO  WRK-SKJ-SRYCD (IDX5)
           END-IF
      *
           .
       2001111-SKJ-HIRU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÍ¼¥³¥á¥ó¥ÈÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-YUU-SET-SEC         SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-YUU   TO  WRK-SKJ-SRYCD (IDX5)
           END-IF
      *
           .
       2001111-SKJ-YUU-SET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡ËÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-RYOYO1-SET-SEC      SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-RYOYO1    TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-RYOYO1
           END-IF
      *
           .
       2001111-SKJ-RYOYO1-SET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡ËÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-RYOYO2-SET-SEC      SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-RYOYO2    TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-RYOYO2
           END-IF
      *
           .
       2001111-SKJ-RYOYO2-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÊÌ´ÉÍý²Ã»»ÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-TOKKANRI-SET-SEC    SECTION.
      *
           IF    ( WRK-TOKUKANRIKASAN(IDXD)      =  1 )
               PERFORM 20011111-SKJ-TOKKANRI-SET-SEC
           END-IF
      *
           .
       2001111-SKJ-TOKKANRI-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÊÌ´ÉÍý²Ã»»ÀßÄê½èÍý
      *****************************************************************
       20011111-SKJ-TOKKANRI-SET-SEC   SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-TOKKANRI  TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-TOKKANRI
           END-IF
      *
           .
       20011111-SKJ-TOKKANRI-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÊÌ¿©²Ã»»ÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-TOKSYOKU-SET-SEC    SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-TOKSYOKU  TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-TOKSYOKU
           END-IF
      *
           .
       2001111-SKJ-TOKSYOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©Æ²²Ã»»ÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-SYOKUDO-SET-SEC     SECTION.
      *
           IF    ( FLG-SYOKUDODAY(IDXD)    NOT =   ZERO )
            AND  ( WSKJH1804-CNT-SYOKUDO       =   ZERO )
               PERFORM 20011111-SKJ-SYOKUDO-SET-SEC
           END-IF
      *
           .
       2001111-SKJ-SYOKUDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©Æ²²Ã»»ÀßÄê½èÍý
      *****************************************************************
       20011111-SKJ-SYOKUDO-SET-SEC     SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-SYOKUDO   TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-SYOKUDO
           END-IF
      *
           .
       20011111-SKJ-SYOKUDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ê£¿ô¸¥Î©²Ã»»ÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-FKSKONDATE-SET-SEC  SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-FKSKONDATE
                                           TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-FKSKONDATE
           END-IF
      *
           .
       2001111-SKJ-FKSKONDATE-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¿©»öÉéÃ´ÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-SKJFTN-SET-SEC      SECTION.
      *
           IF    ( TSKJFTN-SRYCD (IDXD) =   SPACE )
               CONTINUE
           ELSE
               IF    ( IDX5                <   5 )
                   COMPUTE IDX5    =   IDX5    +   1
                   MOVE    TSKJFTN-SRYCD (IDXD)
                                           TO  WRK-SKJ-SRYCD (IDX5)
      *
                   COMPUTE WRK-SKJ-ZAITEN
                       =   WRK-SKJ-ZAITEN
                       +   TSKJFTN-MONEY (IDXD)
               END-IF
           END-IF
      *
      *
           .
       2001111-SKJ-SKJFTN-SET-EXT.
           EXIT.
      *****************************************************************
      *    À¸³èÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ½èÍý
      *****************************************************************
       200111-SEIKATU-SRYCD-GET-SEC    SECTION.
      *
           INITIALIZE                      WRK-SKJ-SRYCD-G
           MOVE    ZERO                TO  IDX5
      *
           IF    ( FLG-EXSYOKUJI       =   ZERO )
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
      *
      *                    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë
                           PERFORM 2001111-LIFE-RYOYO1-SET-SEC
      *
      *                    ¿©Æ²²Ã»»
                           PERFORM 2001111-LIFE-SYOKUDO-SET-SEC
                       WHEN  2
      *
      *                    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë
                           PERFORM 2001111-LIFE-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN  2
      *
      *                Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë
                       PERFORM 2001111-LIFE-RYOYO1-SET-SEC
      *
      *                ÆÃÊÌ¿©²Ã»»
                       PERFORM 2001111-LIFE-TOKSYOKU-SET-SEC
      *
      *                ¿©Æ²²Ã»»
                       PERFORM 2001111-LIFE-SYOKUDO-SET-SEC
      *
                   END-EVALUATE
               WHEN    2
                   PERFORM 2001111-LIFE-SKJFTN-SET-SEC
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
      *
                   IF    ( NACCT-DAY (IDXD)    =   1  OR 2 )
                       EVALUATE    WRK-SKJCOMMENTCD
                       WHEN    CONST-SKJ-ASA
                           PERFORM 2001111-SKJ-ASA-SET-SEC
                       WHEN    CONST-SKJ-HIRU
                           PERFORM 2001111-SKJ-HIRU-SET-SEC
                       WHEN    CONST-SKJ-YUU
                           PERFORM 2001111-SKJ-YUU-SET-SEC
                       END-EVALUATE
                   END-IF
      *
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
      *
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
                           PERFORM 2001111-LIFE-RYOYO1-SET-SEC
      *
                       WHEN    2
                           PERFORM 2001111-LIFE-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN    2
                       PERFORM 2001111-LIFE-RYOYO1-SET-SEC
                       PERFORM 2001111-LIFE-TOKSYOKU-SET-SEC
                   END-EVALUATE
               WHEN    1
                   IF    ( WRK-SHOKUJIRYOYO    =   1 )
                    AND  ( NACCT-DAY (IDXD)    =   1  OR 2 )
                       PERFORM 2001111-LIFE-SYOKUDO-SET-SEC
                   END-IF
               WHEN    2
                   PERFORM 2001111-LIFE-SKJFTN-SET-SEC
               END-EVALUATE
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL ( IDX5    >   5 )
               IF    ( WRK-SKJ-SRYCD (IDX5)    NOT =   SPACE )
                   MOVE    WRK-SKJ-SRYCD (IDX5)    TO  WRK-SRYCD-NUM
                   COMPUTE WRK-SKJ-SRYCDTOTAL  =   WRK-SKJ-SRYCDTOTAL
                                               +   WRK-SRYCD-NUM
               END-IF
           END-PERFORM
      *
           .
       200111-SEIKATU-SRYCD-GET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡ËÀßÄê½èÍý
      *****************************************************************
       2001111-LIFE-RYOYO1-SET-SEC     SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-LIFE-RYOYO1   TO  WRK-SKJ-SRYCD (IDX5)
                                               WRK-SRYCD-NUM
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-LIFE-TEN-RYOYO1
           END-IF
      *
           .
       2001111-LIFE-RYOYO1-SET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡ËÀßÄê½èÍý
      *****************************************************************
       2001111-LIFE-RYOYO2-SET-SEC     SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-LIFE-RYOYO2   TO  WRK-SKJ-SRYCD (IDX5)
                                               WRK-SRYCD-NUM
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-LIFE-TEN-RYOYO2
           END-IF
      *
           .
       2001111-LIFE-RYOYO2-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÊÌ¿©²Ã»»ÀßÄê½èÍý
      *****************************************************************
       2001111-LIFE-TOKSYOKU-SET-SEC   SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-LIFE-TOKSYOKU TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-LIFE-TEN-TOKSYOKU
           END-IF
      *
           .
       2001111-LIFE-TOKSYOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©Æ²²Ã»»ÀßÄê½èÍý
      *****************************************************************
       2001111-LIFE-SYOKUDO-SET-SEC     SECTION.
      *
           IF    ( FLG-SYOKUDODAY(IDXD)    NOT =   ZERO )
            AND  ( WSKJH1804-CNT-SYOKUDO       =   ZERO )
               PERFORM 20011111-LIFE-SYOKUDO-SET-SEC
           END-IF
      *
           .
       2001111-LIFE-SYOKUDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©Æ²²Ã»»ÀßÄê½èÍý
      *****************************************************************
       20011111-LIFE-SYOKUDO-SET-SEC   SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-LIFE-SYOKUDO  TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-LIFE-TEN-SYOKUDO
           END-IF
      *
           .
       20011111-LIFE-SYOKUDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¿©»öÉéÃ´ÀßÄê½èÍý
      *****************************************************************
       2001111-LIFE-SKJFTN-SET-SEC     SECTION.
      *
           IF    ( HCM34S01-HKNNUM(1:2)            =   "98" )
             OR  ( LNK-NYULIFE-SKJFTN-SRYCD (IDXD) =   SPACE )
               CONTINUE
           ELSE
               IF    ( IDX5                <   5 )
                   COMPUTE IDX5    =   IDX5    +   1
                   MOVE    LNK-NYULIFE-SKJFTN-SRYCD (IDXD)
                                           TO  WRK-SKJ-SRYCD (IDX5)
      *
                   COMPUTE WRK-SKJ-ZAITEN
                       =   WRK-SKJ-ZAITEN
                       +   LNK-NYULIFE-SKJFTN-MONEY (IDXD)
               END-IF
           END-IF
      *
      *
           .
       2001111-LIFE-SKJFTN-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»ö´ðËÜ¾ðÊóÀßÄê½èÍý
      *****************************************************************
       200111-SYOKUJI-KHNINF-SET-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-OVER
           MOVE    ZERO                TO  IDXB
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WSYOKUJI-OCC1-MAX )
                     OR  ( IDXB    >   CONST-WSYOKUJI-MAX )
                     OR (( WRK-SKJ-SRYCDTOTAL
                                   =   WSYOKUJI-SRYCDTOTAL (IDXB))
                      AND( WRK-SKJ-ZAITEN
                                   =   WSYOKUJI-ZAITEN     (IDXB)))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF   (    IDXB  >   WSYOKUJI-OCC1-MAX )
               COMPUTE WSYOKUJI-OCC1-MAX
                   =   WSYOKUJI-OCC1-MAX   +   1
               IF    ( WSYOKUJI-OCC1-MAX   >   CONST-WSYOKUJI-MAX )
                   MOVE    CONST-WSYOKUJI-MAX  TO  WSYOKUJI-OCC1-MAX
                   MOVE    1                   TO  FLG-OVER
               ELSE
                   MOVE    WSYOKUJI-OCC1-MAX   TO  IDXB
                   MOVE    WRK-SKJ-SRYCDTOTAL
                                       TO  WSYOKUJI-SRYCDTOTAL (IDXB)
                   MOVE    NACCT-SRYKBN
                                       TO  WSYOKUJI-SRYKBN (IDXB)
                   MOVE    WRK-SKJ-ZAITEN
                                       TO  WSYOKUJI-ZAITEN (IDXB)
                   PERFORM VARYING IDXC    FROM    1   BY  1
                           UNTIL ( IDXC    >   5 )
                            OR   ( WRK-SKJ-SRYCD (IDXC)    =   SPACE )
                       MOVE    WRK-SKJ-SRYCD (IDXC)
                                       TO  WSYOKUJI-SRYCD  (IDXB IDXC)
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       200111-SYOKUJI-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÊÔ½¸½èÍý
      *****************************************************************
       2002-SYOKUJI-HEN-SEC            SECTION.
      *
      *    ¿©»ö´ðËÜÅÀ¿ô¼èÆÀ½èÍý
           PERFORM 20011-SKJ-KHNTEN-GET-SEC
      *
      *    À¸³è´ðËÜÅÀ¿ô¼èÆÀ½èÍý
           IF    ( HCM34S01-SANTEIYM   >=  CONST-H1810 )
               PERFORM 20011-LIFE-KHNTEN-GET-SEC
           ELSE
               INITIALIZE              WRK-LIFE-TEN-G
           END-IF
      *
           MOVE    "4"                 TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
      *
               IF    ( NACCT-SRYCDTOTAL    =   197000570 )
                   CONTINUE
               ELSE
      *
      *            ¿©»öÂàÈòÎÎ°èÊÔ½¸½èÍý
                   PERFORM 20021-WSKJNACCT-HEN-SEC
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    ¿©Æ²²Ã»»¥Á¥§¥Ã¥¯
           PERFORM 20021-SYOKUDOKSN-CHK-SEC
      *
           IF    ( FLG-EXSYOKUJI   =   ZERO )
               MOVE    ZERO            TO      FLG-SKJZAIPTN
               PERFORM 20021-SYOKUJI-HEN-SEC
               IF    ( FLG-SKJFTN  =   1 )
                   MOVE    2           TO      FLG-SKJZAIPTN
                   PERFORM 20021-SYOKUJI-HEN-SEC
               END-IF
           ELSE
               MOVE    ZERO            TO      FLG-SKJZAIPTN
               PERFORM 20021-SYOKUJI-HEN-SEC
               MOVE    1               TO      FLG-SKJZAIPTN
               PERFORM 20021-SYOKUJI-HEN-SEC
               IF    ( FLG-SKJFTN  =   1 )
                   MOVE    2           TO      FLG-SKJZAIPTN
                   PERFORM 20021-SYOKUJI-HEN-SEC
               END-IF
           END-IF
      *
           IF    ( HCM34S01-SANTEIYM   >=  CONST-H1810 )
            AND  ( WRK-SHOKUJIRYOYO     =  1 OR 2 )
            AND  ( CONST-WSYOKUJI-MAX  >   WSYOKUJI-OCC1-MAX )
      *            Æþ±¡»þÀ¸³èÎÅÍÜ´Ä¶­ÎÅÍÜÊÔ½¸½èÍý
                   PERFORM 20021-KANKYO-HEN-SEC
           END-IF
      *
           .
       2002-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»ö¿ÇÎÅ¥³¡¼¥É¹ç·×ÊÔ½¸½èÍý
      *****************************************************************
       20021-SKJ-SRYCDTTL-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDXG    FROM    1   BY  1
                   UNTIL ( IDXG    >   9 )
      *
               IF    ( WSKJH1804-SRYCD-CNT (IDXG)
                                   >   ZERO )
                   COMPUTE WSKJH1804-SURYOTOTAL
                       =   WSKJH1804-SURYOTOTAL
                       +   WSKJH1804-SRYCD-CNT (IDXG)
                   COMPUTE WSKJH1804-MEISAISU
                       =   WSKJH1804-MEISAISU
                       +   1
               END-IF
           END-PERFORM
      *
           .
       20021-SKJ-SRYCDTTL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»ö¿ÇÎÅ¥³¡¼¥É¹ç·×ÊÔ½¸½èÍý
      *****************************************************************
       20021-SYOKUJI-ZAI-EDIT-SEC      SECTION.
      *
           IF    ( WSKJH1804-ZAITEN    >   ZERO )
               PERFORM 20021-SYOKUJI-KHNINF-SET-SEC
               IF    ( IDXB        >   ZERO )
                 AND ( FLG-OVER    =   ZERO )
                   MOVE    1       TO  WSYOKUJI-DAY (IDXB IDXD)
                   COMPUTE WSYOKUJI-ZAIKAISU (IDXB)
                       =   WSYOKUJI-ZAIKAISU (IDXB)    +   1
               END-IF
           END-IF
      *
           .
       20021-SYOKUJI-ZAI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÂàÈòÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       20021-WSKJNACCT-HEN-SEC         SECTION.
      *
           MOVE        ZERO   TO   IDX-SKJ
           EVALUATE    NACCT-SRYCDTOTAL
           WHEN    CONST-SKJ-ASA
               MOVE    1       TO  IDX-SKJ
           WHEN    CONST-SKJ-HIRU
               MOVE    2       TO  IDX-SKJ
           WHEN    CONST-SKJ-YUU
               MOVE    3       TO  IDX-SKJ
           END-EVALUATE
      *
           IF    ( IDX-SKJ     >   ZERO )
      *
               MOVE    NYUINACCT-REC
                                   TO  WSKJNACCT-OCC      (IDX-SKJ)
               INITIALIZE              WSKJNACCT-DAY-AREA (IDX-SKJ)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
      *
                   MOVE    ZERO                TO  FLG-SANTEI
                   IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                    AND  ( FLG-NYUINDAY (IDXD)     =   1 OR 2 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR 4 OR 5
                                                            OR 6 OR 7
                                                            OR 8 OR 9 )
                        IF   ( FLG-NYUINRYODAY (IDXD)  =   1 )
                         OR  ( LNK-NYUINDAY-KBN(IDXD)  =   "1" )
                            MOVE   1       TO  FLG-SANTEI
                        END-IF
                   END-IF
      *
                   IF    ( FLG-SANTEI          =   1 )
                       MOVE    NACCT-DAY    (IDXD)
                                       TO  WSKJNACCT-DAY (IDX-SKJ IDXD)
      *
                       MOVE    ZERO    TO  FLG-CLEAR
                       IF    ( WRK-DOJITUST(IDXD)  NOT =  SPACE  )
                           EVALUATE    WRK-DOJITUST(IDXD)
                           WHEN    "0"
                                   MOVE    1   TO  FLG-CLEAR
                           WHEN    "1"
                               CONTINUE
                           WHEN    "2"
                               IF    ( IDX-SKJ <   2 )
                                   MOVE    1   TO  FLG-CLEAR
                               END-IF
                           WHEN    "3"
                               IF    ( IDX-SKJ <   3 )
                                   MOVE    1   TO  FLG-CLEAR
                               END-IF
                           END-EVALUATE
                       END-IF
      *
                       IF    ( FLG-CLEAR   =   1 )
                           MOVE    ZERO    TO  WSKJNACCT-DAY
                                                         (IDX-SKJ IDXD)
                           MOVE    "1"     TO  WSKJNACCT-SYOKUDO-NG
                                                         (IDXD)
                       END-IF
      *
                       MOVE    ZERO    TO  FLG-CLEAR
                       IF    ( WRK-DOJITUED(IDXD)  NOT =  SPACE  )
                           EVALUATE    WRK-DOJITUED(IDXD)
                           WHEN    "0"
                                   MOVE    1   TO  FLG-CLEAR
                           WHEN    "1"
                               IF    ( IDX-SKJ >   1 )
                                   MOVE    1   TO  FLG-CLEAR
                               END-IF
                           WHEN    "2"
                               IF    ( IDX-SKJ >   2 )
                                   MOVE    1   TO  FLG-CLEAR
                               END-IF
                           WHEN    "3"
                                   CONTINUE
                           END-EVALUATE
                       END-IF
                       IF    ( FLG-CLEAR   =   1 )
                           MOVE    ZERO    TO  WSKJNACCT-DAY
                                                         (IDX-SKJ IDXD)
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       20021-WSKJNACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¿©Æ²²Ã»»¥Á¥§¥Ã¥¯
      *****************************************************************
       20021-SYOKUDOKSN-CHK-SEC        SECTION.
      *
           PERFORM 20011-SYOKUDOKSN-CHK-SEC
      *
           PERFORM VARYING IDXD    FROM    1 BY  1
                   UNTIL ( IDXD    >       31 )
               IF    ( WSKJNACCT-SYOKUDO-NG (IDXD) =   "1" )
                   MOVE    ZERO    TO  FLG-SYOKUDODAY(IDXD)
               END-IF
           END-PERFORM
      *
           .
       20021-SYOKUDOKSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÊÔ½¸½èÍý
      *****************************************************************
       20021-SYOKUJI-HEN-SEC           SECTION.
      *
           IF    ( FLG-EXSYOKUJI   =   ZERO )
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   INITIALIZE              WSKJH1804-AREA
                   PERFORM VARYING IDX-SKJ FROM    1   BY  1
                           UNTIL ( IDX-SKJ >   3 )
      *
                       MOVE    CONST-SKJ-COMMENT (IDX-SKJ)
                                   TO      WRK-SKJCOMMENTCD
                       MOVE    WSKJNACCT-OCC (IDX-SKJ)
                                   TO      NYUINACCT-REC
      *
                       IF    ( NACCT-DAY    (IDXD)     >   ZERO )
      *
                           IF    ( LNK-NYULIFE-KBN(IDXD)   =   SPACE )
                               PERFORM 200211-SYOKUJI-HEN-SEC
                           ELSE
                               PERFORM 200212-SYOKUJI-HEN-SEC
                           END-IF
      *
                       END-IF
      *
      *
                   END-PERFORM
                   PERFORM 20021-SKJ-SRYCDTTL-HEN-SEC
                   PERFORM 20021-SYOKUJI-ZAI-EDIT-SEC
               END-PERFORM
           ELSE
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   PERFORM VARYING IDX-SKJ FROM    1   BY  1
                           UNTIL ( IDX-SKJ >   3 )
      *
                       INITIALIZE          WSKJH1804-AREA
                       MOVE    CONST-SKJ-COMMENT (IDX-SKJ)
                                   TO      WRK-SKJCOMMENTCD
                       MOVE    WSKJNACCT-OCC (IDX-SKJ)
                                   TO      NYUINACCT-REC
      *
                       IF    ( NACCT-DAY    (IDXD)     >   ZERO )
      *
                           IF    ( LNK-NYULIFE-KBN(IDXD)   =   SPACE )
                               PERFORM 200211-SYOKUJI-HEN-SEC
                           ELSE
                               PERFORM 200212-SYOKUJI-HEN-SEC
                           END-IF
      *
                       END-IF
      *
                       PERFORM 20021-SKJ-SRYCDTTL-HEN-SEC
                       PERFORM 20021-SYOKUJI-ZAI-EDIT-SEC
      *
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           .
       20021-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÊÔ½¸½èÍý¡Ê¿©»öÎÅÍÜ¡Ë
      *****************************************************************
       200211-SYOKUJI-HEN-SEC          SECTION.
      *
           PERFORM 200111-SYOKUJI-SRYCD-GET-SEC
      *
           COMPUTE WSKJH1804-SRYCDTOTAL
               =   WSKJH1804-SRYCDTOTAL
               +   WRK-SKJ-SRYCDTOTAL
      *
           COMPUTE WSKJH1804-ZAITEN
               =   WSKJH1804-ZAITEN
               +   WRK-SKJ-ZAITEN
      *
           PERFORM VARYING IDXF    FROM    1   BY  1
                           UNTIL ( IDXF    >       5 )
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-ASA )
                   COMPUTE WSKJH1804-CNT-ASA
                       =   WSKJH1804-CNT-ASA
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-HIRU )
                   COMPUTE WSKJH1804-CNT-HIRU
                       =   WSKJH1804-CNT-HIRU
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-YUU )
                   COMPUTE WSKJH1804-CNT-YUU
                       =   WSKJH1804-CNT-YUU
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-RYOYO1 )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO1
                       =   WSKJH1804-CNT-SKJRYOYO1
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-SKJ-RYOYO2 )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO2
                       =   WSKJH1804-CNT-SKJRYOYO2
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-SKJ-TOKSYOKU )
                   COMPUTE WSKJH1804-CNT-TOKSYOKU
                       =   WSKJH1804-CNT-TOKSYOKU
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-SKJ-SYOKUDO )
                   COMPUTE WSKJH1804-CNT-SYOKUDO
                       =   WSKJH1804-CNT-SYOKUDO
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                   NOT =   SPACE )
                   IF    ( WRK-SKJ-SRYCD (IDXF)
                                   =   TSKJFTN-SRYCD (IDXD))
                       COMPUTE WSKJH1804-CNT-SKJFTN
                           =   WSKJH1804-CNT-SKJFTN
                           +   1
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200211-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»öÊÔ½¸½èÍý¡ÊÀ¸³èÎÅÍÜ¡Ë
      *****************************************************************
       200212-SYOKUJI-HEN-SEC          SECTION.
      *
           PERFORM 200111-SEIKATU-SRYCD-GET-SEC
      *
           COMPUTE WSKJH1804-SRYCDTOTAL
               =   WSKJH1804-SRYCDTOTAL
               +   WRK-SKJ-SRYCDTOTAL
      *
           COMPUTE WSKJH1804-ZAITEN
               =   WSKJH1804-ZAITEN
               +   WRK-SKJ-ZAITEN
      *
           PERFORM VARYING IDXF    FROM    1   BY  1
                           UNTIL ( IDXF    >       5 )
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-ASA )
                   COMPUTE WSKJH1804-CNT-ASA
                       =   WSKJH1804-CNT-ASA
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-HIRU )
                   COMPUTE WSKJH1804-CNT-HIRU
                       =   WSKJH1804-CNT-HIRU
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-YUU )
                   COMPUTE WSKJH1804-CNT-YUU
                       =   WSKJH1804-CNT-YUU
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-LIFE-RYOYO1 )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO1
                       =   WSKJH1804-CNT-SKJRYOYO1
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-LIFE-RYOYO2 )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO2
                       =   WSKJH1804-CNT-SKJRYOYO2
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-LIFE-TOKSYOKU )
                   COMPUTE WSKJH1804-CNT-TOKSYOKU
                       =   WSKJH1804-CNT-TOKSYOKU
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-LIFE-SYOKUDO )
                   COMPUTE WSKJH1804-CNT-SYOKUDO
                       =   WSKJH1804-CNT-SYOKUDO
                       +   1
      *
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                   NOT =   SPACE )
                   IF    ( WRK-SKJ-SRYCD (IDXF)
                                   =   LNK-NYULIFE-SKJFTN-SRYCD (IDXD))
                       COMPUTE WSKJH1804-CNT-SKJFTN
                           =   WSKJH1804-CNT-SKJFTN
                           +   1
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200212-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»ö´ðËÜ¾ðÊóÀßÄê½èÍý
      *****************************************************************
       20021-SYOKUJI-KHNINF-SET-SEC    SECTION.
      *
           MOVE    ZERO                TO  IDX-WSYOKUJI
      *
           MOVE    ZERO                TO  IDXB
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WSYOKUJI-OCC1-MAX )
                     OR  ( IDXB    >   CONST-WSYOKUJI-MAX )
                     OR (( WSKJH1804-SRYCDTOTAL
                                   =   WSYOKUJI-SRYCDTOTAL (IDXB))
                      AND( WSKJH1804-SURYOTOTAL
                                   =   WSYOKUJI-SURYOTOTAL (IDXB))
                      AND( WSKJH1804-MEISAISU
                                   =   WSYOKUJI-MEISAISU   (IDXB))
                      AND( WSKJH1804-ZAITEN
                                   =   WSYOKUJI-ZAITEN     (IDXB)))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF   (    IDXB  >   WSYOKUJI-OCC1-MAX )
               COMPUTE WSYOKUJI-OCC1-MAX
                   =   WSYOKUJI-OCC1-MAX   +   1
               IF    ( WSYOKUJI-OCC1-MAX   >   CONST-WSYOKUJI-MAX )
                   MOVE    CONST-WSYOKUJI-MAX  TO  WSYOKUJI-OCC1-MAX
                   MOVE    1                   TO  FLG-OVER
               ELSE
                   MOVE    WSYOKUJI-OCC1-MAX   TO  IDX-WSYOKUJI
                   MOVE    WSYOKUJI-OCC1-MAX   TO  IDXB
                   MOVE    IDXB                TO  WSYOKUJI-NUM(IDXB)
                   MOVE    WSKJH1804-SRYCDTOTAL
                                           TO  WSYOKUJI-SRYCDTOTAL(IDXB)
                   MOVE    WSKJH1804-SURYOTOTAL
                                           TO  WSYOKUJI-SURYOTOTAL(IDXB)
                   MOVE    WSKJH1804-MEISAISU
                                           TO  WSYOKUJI-MEISAISU  (IDXB)
                   MOVE    WSKJH1804-ZAITEN
                                           TO  WSYOKUJI-ZAITEN (IDXB)
                   MOVE    "97"            TO  WSYOKUJI-SRYKBN (IDXB)
                   MOVE    ZERO            TO  IDXC
      *
                   IF    ( LNK-NYULIFE-KBN(IDXD)   =   SPACE )
                       PERFORM 200211-SYOKUJI-KHNINF-SET-SEC
                   ELSE
                       PERFORM 200212-SYOKUJI-KHNINF-SET-SEC
                   END-IF
      *
               END-IF
           END-IF
      *
           .
       20021-SYOKUJI-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»ö´ðËÜ¾ðÊóÀßÄê½èÍý¡Ê¿©»öÎÅÍÜ¡Ë
      *****************************************************************
       200211-SYOKUJI-KHNINF-SET-SEC   SECTION.
      *
           IF    ( WSKJH1804-CNT-ASA     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-ASA
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-ASA
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-HIRU    >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-HIRU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-HIRU
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-YUU     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-YUU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-YUU 
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-SKJRYOYO1     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO1
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-RYOYO1
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SKJRYOYO2     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO2
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-RYOYO2
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-TOKSYOKU      >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-TOKSYOKU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-TOKSYOKU
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SYOKUDO       >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SYOKUDO
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-SYOKUDO
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SKJFTN        >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJFTN
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    TSKJFTN-SRYCD (IDXD)
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           .
       200211-SYOKUJI-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»ö´ðËÜ¾ðÊóÀßÄê½èÍý¡ÊÀ¸³èÎÅÍÜ¡Ë
      *****************************************************************
       200212-SYOKUJI-KHNINF-SET-SEC   SECTION.
      *
           IF    ( WSKJH1804-CNT-ASA     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-ASA
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-ASA
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-HIRU    >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-HIRU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-HIRU
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-YUU     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-YUU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-YUU 
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-SKJRYOYO1     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO1
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-LIFE-RYOYO1
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SKJRYOYO2     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO2
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-LIFE-RYOYO2
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-TOKSYOKU      >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-TOKSYOKU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-LIFE-TOKSYOKU
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SYOKUDO       >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SYOKUDO
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-LIFE-SYOKUDO
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SKJFTN        >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJFTN
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    LNK-NYULIFE-SKJFTN-SRYCD (IDXD)
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-KANFTN        >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-KANFTN
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    LNK-NYULIFE-KANFTN-SRYCD (IDXD)
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           MOVE    1               TO  WSYOKUJI-LIFE (IDXB)
      *
           .
       200212-SYOKUJI-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡»þÀ¸³èÎÅÍÜ´Ä¶­ÎÅÍÜÊÔ½¸½èÍý
      *****************************************************************
       20021-KANKYO-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-KANKYO-DAY-G
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    ZERO                TO  FLG-SANTEI
               IF    ( FLG-NYUINDAY (IDXD)     =   1 )
                AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR 4 OR 5
                                                        OR 6 OR 7
                                                        OR 8 OR 9 )
                AND  ( LNK-NYULIFE-KBN(IDXD)   NOT =   SPACE )
                    IF   ( FLG-NYUINRYODAY (IDXD)  =   1 )
                     OR  ( LNK-NYUINDAY-KBN(IDXD)  =   "1" )
                       COMPUTE CNT-KANKYO-DAY-TOTAL
                           =   CNT-KANKYO-DAY-TOTAL
                           +   1
                       MOVE    1           TO  CNT-KANKYO-DAY (IDXD)
                    END-IF
               END-IF
           END-PERFORM
      *
           IF    ( CNT-KANKYO-DAY-TOTAL    >   ZERO )
               COMPUTE WSYOKUJI-OCC1-MAX
                   =   WSYOKUJI-OCC1-MAX
                   +   1
               MOVE    WSYOKUJI-OCC1-MAX   TO  IDXB
      *
               PERFORM VARYING IDXH    FROM    1   BY  1
                       UNTIL ( IDXH    >   WSYOKUJI-OCC1-MAX - 1 )
                   IF    ( WSYOKUJI-LIFE (IDXH)    =   1 )
                       IF    ( WSYOKUJI-NUM (IDXB)
                                       =   ZERO )
                        OR   ( WSYOKUJI-NUM (IDXH)
                                       >   WSYOKUJI-NUM (IDXB) )
                           MOVE    WSYOKUJI-NUM (IDXH)
                                       TO  WSYOKUJI-NUM (IDXB)
                       END-IF
                   END-IF
               END-PERFORM
               IF    ( WSYOKUJI-NUM (IDXB) =   ZERO )
                   MOVE    999             TO  WSYOKUJI-NUM (IDXB)
               END-IF
      *
               MOVE    1                   TO  WSYOKUJI-KANKYO (IDXB)
               IF    ( WRK-SHOKUJIRYOYO    =   1 )
                   MOVE    CONST-LIFE-KANKYO1
                                           TO  WRK-SRYCD
                   COMPUTE WSYOKUJI-SRYCDTOTAL (IDXB)
                       =   WSYOKUJI-SRYCDTOTAL (IDXB)
                       +   WRK-SRYCD-9
                   MOVE    WRK-LIFE-TEN-KANKYO1
                                           TO  WSYOKUJI-ZAITEN (IDXB)
                   MOVE    CONST-LIFE-KANKYO1
                                           TO  WSYOKUJI-SRYCD(IDXB 1)
               ELSE
                   MOVE    CONST-LIFE-KANKYO2
                                           TO  WRK-SRYCD
                   COMPUTE WSYOKUJI-SRYCDTOTAL (IDXB)
                       =   WSYOKUJI-SRYCDTOTAL (IDXB)
                       +   WRK-SRYCD-9
                   MOVE    WRK-LIFE-TEN-KANKYO2
                                           TO  WSYOKUJI-ZAITEN (IDXB)
                   MOVE    CONST-LIFE-KANKYO2
                                           TO  WSYOKUJI-SRYCD(IDXB 1)
               END-IF
               MOVE    "97"                TO  WSYOKUJI-SRYKBN (IDXB)
               MOVE    1                   TO  WSYOKUJI-SURYOTOTAL(IDXB)
               MOVE    CNT-KANKYO-DAY-TOTAL
                                           TO  WSYOKUJI-ZAIKAISU (IDXB)
               MOVE    1                   TO  WSYOKUJI-MEISAISU (IDXB)
               MOVE    1                   TO  WSYOKUJI-SURYO (IDXB 1)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    CNT-KANKYO-DAY (IDXD)
                                           TO  WSYOKUJI-DAY (IDXB IDXD)
               END-PERFORM
               MOVE    1                   TO  WSYOKUJI-LIFE(IDXB)
      *
               IF    ( HCM34S01-HKNNUM(1:2)            =   "98" )
                   CONTINUE
               ELSE
                   MOVE    IDXB                TO  IDX-KANKYO
                   PERFORM 200211-KANKYOFTN-HEN-SEC
               END-IF
      *
           END-IF
      *
           .
       20021-KANKYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡»þÀ¸³èÎÅÍÜ´Ä¶­ÎÅÍÜÉ¸½àÉéÃ´³ÛÊÔ½¸½èÍý
      *****************************************************************
       200211-KANKYOFTN-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
      *
               IF    ( CNT-KANKYO-DAY (IDXD)   >   ZERO )
                   IF    ( LNK-NYULIFE-KANFTN-SRYCD (IDXD)
                                           =   SPACE )
                       CONTINUE
                   ELSE
                       INITIALIZE          WSKJH1804-AREA
                       MOVE    LNK-NYULIFE-KANFTN-SRYCD (IDXD)
                                       TO  WRK-SRYCD-NUM
                       MOVE    WRK-SRYCD-NUM
                                       TO  WSKJH1804-SRYCDTOTAL
                       MOVE    1       TO  WSKJH1804-SURYOTOTAL
                       MOVE    LNK-NYULIFE-KANFTN-MONEY (IDXD)
                                       TO  WSKJH1804-ZAITEN
                       MOVE    1       TO  WSKJH1804-MEISAISU
                       MOVE    1       TO  WSKJH1804-CNT-KANFTN
                       PERFORM 20021-SYOKUJI-ZAI-EDIT-SEC
                       IF    ( IDX-WSYOKUJI    >   ZERO )
                           MOVE    WSYOKUJI-NUM (IDX-KANKYO)
                                       TO   WSYOKUJI-NUM (IDX-WSYOKUJI)
                           COMPUTE WSYOKUJI-KANKYO (IDX-WSYOKUJI)
                               =   100 +   IDXD
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       200211-KANKYOFTN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»ÊÔ½¸½èÍý
      *****************************************************************
       2001-NYUINKSN-HEN-SEC           SECTION.
      *
      *    ¸º»»¾ðÊóÊÔ½¸½èÍý
           PERFORM 20011-NYUINGEN-HEN-SEC
      *
      *    ²Ã»»¾ðÊóÊÔ½¸½èÍý
           PERFORM 20011-NYUINKSN-HEN-SEC
      *
           .
       2001-NYUINKSN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¸º»»¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       20011-NYUINGEN-HEN-SEC          SECTION.
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL ( IDX4    >   4 )
      *
               IF    ( WRK-SYS-MIJISI (IDX4)   =   1 )
                   MOVE    SPACE           TO  WRK-TNS-SRYCD
      *
                   EVALUATE    IDX4
                   WHEN    2
                       MOVE  "190080570"   TO  WRK-TNS-SRYCD
                   WHEN    3
                       MOVE  "190111690"   TO  WRK-TNS-SRYCD
                   WHEN    4
                       MOVE  "190111790"   TO  WRK-TNS-SRYCD
                   END-EVALUATE
      *
                   IF    ( WRK-TNS-SRYCD   NOT =   SPACE )
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       IF    ( FLG-TENSU       =   ZERO )
                           PERFORM 200111-NYUINGEN-HEN-SEC
                       END-IF
                   END-IF
      *
               END-IF
           END-PERFORM
      *
           .
       20011-NYUINGEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¸º»»¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       200111-NYUINGEN-HEN-SEC         SECTION.
      *
          MOVE    1                        TO  IDXA
          IF    ( WNYUINKSN-OCC2-MAX (IDXA)
                                   <   CONST-WNYUINKSN-MAX)
               MOVE    ZERO                TO FLG-TAISYO
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD        >   31 )
                   IF    ( FLG-NYUINDAY  (IDXD)        =   1  )
                    AND  ( FLG-NYUINRYODAY (IDXD)      =   1 )
                    AND  ( FLG-NYUINGENDAY (IDX4 IDXD) =   1 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)        =   ZERO )
                    AND  ( FLG-TOKTEIDAY (IDXD)        =   ZERO )
                       IF    ( FLG-TAISYO              =   ZERO )
                           MOVE    1       TO  FLG-TAISYO
                           PERFORM 2001111-KSN-KHNINF-SET-SEC
                       END-IF
                       MOVE    1           TO  WNYUINKSN-DAY
                                                       (IDXA IDXB IDXD)
                       COMPUTE WNYUINKSN-ZAIKAISU (IDXA IDXB)
                           =   WNYUINKSN-ZAIKAISU (IDXA IDXB)  +   1
                   END-IF
               END-PERFORM
           END-IF
      *
          MOVE    2                        TO  IDXA
          IF    ( WNYUINKSN-OCC2-MAX (IDXA)
                                   <   CONST-WNYUINKSN-MAX)
               MOVE    ZERO                TO FLG-TAISYO
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD        >   31 )
                   IF    ( FLG-NYUINDAY  (IDXD)        =   1  )
                    AND  ((FLG-NYUINRYODAY (IDXD)      =   1 )
                     OR  ( LNK-NYUINDAY-TANKI2-KBN(IDXD)   =   "1" ))
                    AND  ( FLG-NYUINGENDAY (IDX4 IDXD) =   1 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)        =   ZERO )
                    AND  ((FLG-TOKTEIDAY (IDXD)    NOT =   ZERO )
                     OR  ( LNK-NYUINDAY-TANKI2-KBN(IDXD)   =   "1" ))
                       IF    ( FLG-TAISYO              =   ZERO )
                           MOVE    1       TO  FLG-TAISYO
                           PERFORM 2001111-KSN-KHNINF-SET-SEC
                       END-IF
                       MOVE    1           TO  WNYUINKSN-DAY
                                                       (IDXA IDXB IDXD)
                       COMPUTE WNYUINKSN-ZAIKAISU (IDXA IDXB)
                           =   WNYUINKSN-ZAIKAISU (IDXA IDXB)  +   1
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-NYUINGEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ²Ã»»´ðËÜ¾ðÊóÀßÄê½èÍý
      *****************************************************************
       2001111-KSN-KHNINF-SET-SEC      SECTION.
      *
           COMPUTE WNYUINKSN-OCC2-MAX (IDXA)
               =   WNYUINKSN-OCC2-MAX (IDXA)   +   1
           MOVE    WNYUINKSN-OCC2-MAX (IDXA)   TO   IDXB
           MOVE    TNS-CDKBN-SYO       TO  WNYUINKSN-CDKBN-SYO
                                                           (IDXA IDXB)
           MOVE    TNS-CDKBN-BU        TO  WNYUINKSN-CDKBN-BU
                                                           (IDXA IDXB)
           MOVE    TNS-CDKBN-KBN       TO  WNYUINKSN-CDKBN-KBN
                                                           (IDXA IDXB)
           MOVE    TNS-CDKBN-KBNNUM    TO  WNYUINKSN-CDKBN-KBNNUM
                                                           (IDXA IDXB)
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  WNYUINKSN-CDKBN-KBNNUM-EDA
                                                           (IDXA IDXB)
           MOVE    TNS-CDKBN-KOUBAN    TO  WNYUINKSN-CDKBN-KOUBAN
                                                           (IDXA IDXB)
           MOVE    TNS-KOUHYOJYUNNUM   TO  WNYUINKSN-KOUHYOJYUNNUM
                                                           (IDXA IDXB)
           IF    ( IDXA                =   1 )
               MOVE    "90"            TO  WNYUINKSN-SRYKBN
                                                           (IDXA IDXB)
           ELSE
               MOVE    "92"            TO  WNYUINKSN-SRYKBN
                                                           (IDXA IDXB)
           END-IF
      *
           MOVE    TNS-TEN             TO  WNYUINKSN-ZAITEN
                                                           (IDXA IDXB)
      *
           MOVE    TNS-SRYCD           TO  WNYUINKSN-SRYCD
                                                           (IDXA IDXB)
      *
           .
       2001111-KSN-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    ²Ã»»¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       20011-NYUINKSN-HEN-SEC          SECTION.
      *
           MOVE    "6"                 TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               MOVE    NACCT-SRYCDTOTAL
                                       TO  WRK-SRYCDTOTAL
               MOVE    WRK-SRYCDTOTAL  TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   PERFORM 200111-NYUINKSN-HEN-SEC
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-NYUINKSN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ²Ã»»¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       200111-NYUINKSN-HEN-SEC          SECTION.
      *
          MOVE    1                        TO  IDXA
          IF    ( WNYUINKSN-OCC2-MAX (IDXA)
                                   <   CONST-WNYUINKSN-MAX)
               MOVE    ZERO                TO FLG-TAISYO
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD        >   31 )
                   IF    ( NACCT-DAY (IDXD)            >   ZERO )
                    AND  ( FLG-NYUINDAY  (IDXD)        =   1  )
                    AND  ( FLG-NYUINRYODAY (IDXD)      =   1 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR 4 OR 5
                                                            OR 6 OR 7
                                                            OR 8 OR 9 )
                    AND  ( FLG-TOKTEIDAY (IDXD)        =   ZERO )
                       IF    ( FLG-TAISYO              =   ZERO )
                           MOVE    1       TO  FLG-TAISYO
                           PERFORM 2001111-KSN-KHNINF-SET-SEC
                       END-IF
                       MOVE    1           TO  WNYUINKSN-DAY
                                                       (IDXA IDXB IDXD)
                       COMPUTE WNYUINKSN-ZAIKAISU (IDXA IDXB)
                           =   WNYUINKSN-ZAIKAISU (IDXA IDXB)  +   1
                   END-IF
               END-PERFORM
           END-IF
      *
          MOVE    2                        TO  IDXA
          IF    ( WNYUINKSN-OCC2-MAX (IDXA)
                                   <   CONST-WNYUINKSN-MAX)
               MOVE    ZERO                TO FLG-TAISYO
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD        >   31 )
                   IF    ( NACCT-DAY (IDXD)            >   ZERO )
                    AND  ( FLG-NYUINDAY  (IDXD)        =   1  )
                    AND  ((FLG-NYUINRYODAY (IDXD)      =   1 )
                      OR  ( LNK-NYUINDAY-TANKI2-KBN(IDXD)  =   "1" ))
                    AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR 4 OR 5
                                                            OR 6 OR 7
                                                            OR 8 OR 9 )
                    AND  ((FLG-TOKTEIDAY (IDXD)    NOT =   ZERO )
                      OR  ( LNK-NYUINDAY-TANKI2-KBN(IDXD)
                                                       =   "1" ))
                       IF    ( FLG-TAISYO              =   ZERO )
                           MOVE    1       TO  FLG-TAISYO
                           PERFORM 2001111-KSN-KHNINF-SET-SEC
                       END-IF
                       MOVE    1           TO  WNYUINKSN-DAY
                                                       (IDXA IDXB IDXD)
                       COMPUTE WNYUINKSN-ZAIKAISU (IDXA IDXB)
                           =   WNYUINKSN-ZAIKAISU (IDXA IDXB)  +   1
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-NYUINKSN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÃ´Åö¼êÅöÊÔ½¸½èÍý
      *****************************************************************
       2001-RYYTAN-HEN-SEC             SECTION.
      *
           MOVE    "7"                 TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-NYUINACCT       NOT =   ZERO )
               GO  TO  2001-RYYTAN-HEN-EXT
           END-IF
      *
           MOVE    "199000510"         TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       =   ZERO )
               PERFORM 20011-RYYTAN-HEN-SEC
           END-IF
      *
           .
       2001-RYYTAN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÃ´Åö¼êÅöÊÔ½¸½èÍý
      *****************************************************************
       20011-RYYTAN-HEN-SEC            SECTION.
      *
           MOVE    "80"            TO  WRYYTAN-SRYKBN
           MOVE    TNS-TEN         TO  WRYYTAN-ZAITEN
           MOVE    TNS-SRYCD       TO  WRYYTAN-SRYCD
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               IF    ( NACCT-DAY (IDXD)        >   ZERO )
                AND  ( FLG-NYUINDAY  (IDXD)    =   1  )
                AND  ((FLG-NYUINRYODAY (IDXD)      =   1 )
                  OR  ( LNK-NYUINDAY-KBN(IDXD)     =   "1" ))
                AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR 4 OR 5
                                                        OR 6 OR 7
                                                        OR 8 OR 9 )
                   MOVE    1       TO  WRYYTAN-DAY (IDXD)
                   COMPUTE WRYYTAN-ZAIKAISU
                       =   WRYYTAN-ZAIKAISU    +   1
               END-IF
      *
           END-PERFORM
      *
           .
       20011-RYYTAN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ºÞ£Ó£Ï£Ò£Ô½èÍý
      *****************************************************************
       2001-ZAISORT-SEC                SECTION.
      *
      *    £Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
           PERFORM 20011-SORTAREA-HEN-SEC
      *
      *    £Ó£Ï£Ò£Ôºî¶ÈÎÎ°è£Ó£Ï£Ò£Ô½èÍý
           PERFORM 20011-SORTAREA-SORT-SEC
      *
           .
       2001-ZAISORT-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       20011-SORTAREA-HEN-SEC          SECTION.
      *
           INITIALIZE                  SRT-AREA
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   2 )
      *
      *        Æþ±¡ÎÁ£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
               PERFORM 200111-NYUINRYO-SRT-SET-SEC
      *
      *        ³°Çñ£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
               PERFORM 200111-GAIHAKU-SRT-SET-SEC
      *
      *        Æþ±¡ÎÁ²Ã»»£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
               PERFORM 200111-NYUINKSN-SRT-SET-SEC
      *
           END-PERFORM
      *
      *    ¿©»ö£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
           PERFORM 200111-SYOKUJI-SRT-SET-SEC
      *
      *    ¼¼ÎÁº¹³Û£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
           PERFORM 200111-BRMSAGAKU-SRT-SET-SEC
      *
           IF    ( WRYYTAN-SRYKBN      NOT =   SPACE )
      *        ÎÅÍÜÃ´Åö¼êÅö£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
               PERFORM 200111-RYYTAN-SRT-SET-SEC
           END-IF
      *
           .
       20011-SORTAREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       200111-NYUINRYO-SRT-SET-SEC     SECTION.
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WNYUINRYO-OCC2-MAX (IDXA))
                    OR   ( SRT-MAX >=  CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    WNYUINRYO-SRYKBN (IDXA IDXB)
                                           TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    1                   TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    WNYUINRYO-ZAINUM (IDXA IDXB)
                                           TO  SRT-PRIORITY2(SRT-MAX)
      *
               MOVE    WNYUINRYO-ZAITEN (IDXA IDXB)
                                           TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WNYUINRYO-ZAIKAISU(IDXA IDXB)
                                           TO  SRT-ZAIKAISU (SRT-MAX)
               MOVE    "1"                 TO  SRT-ZAISKBKBN(SRT-MAX)
      *
               PERFORM VARYING IDXC    FROM    1   BY  1
                       UNTIL ( IDXC    >   5 )
                   MOVE    WNYUINRYO-SRYCD (IDXA IDXB IDXC)
                                           TO  SRT-SRYCD (SRT-MAX IDXC)
               END-PERFORM
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WNYUINRYO-DAY (IDXA IDXB IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-NYUINRYO-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    ³°Çñ£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       200111-GAIHAKU-SRT-SET-SEC      SECTION.
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WGAIHAKU-OCC2-MAX (IDXA))
                    OR   ( SRT-MAX >=  CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    WGAIHAKU-SRYKBN (IDXA IDXB)
                                           TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    2                   TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    WGAIHAKU-NUM (IDXA IDXB)
                                           TO  SRT-PRIORITY2(SRT-MAX)
      *
               MOVE    WGAIHAKU-ZAITEN (IDXA IDXB)
                                           TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WGAIHAKU-ZAIKAISU(IDXA IDXB)
                                           TO  SRT-ZAIKAISU (SRT-MAX)
      *
               MOVE    "2"                 TO  SRT-ZAISKBKBN(SRT-MAX)
      *
               PERFORM VARYING IDXC    FROM    1   BY  1
                       UNTIL ( IDXC    >   5 )
                   MOVE    WGAIHAKU-SRYCD (IDXA IDXB IDXC)
                                           TO  SRT-SRYCD (SRT-MAX IDXC)
               END-PERFORM
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WGAIHAKU-DAY (IDXA IDXB IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-GAIHAKU-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       200111-NYUINKSN-SRT-SET-SEC     SECTION.
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WNYUINKSN-OCC2-MAX (IDXA))
                    OR   ( SRT-MAX >=  CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    WNYUINKSN-SRYKBN (IDXA IDXB)
                                           TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    3                   TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    WNYUINKSN-CDKBN-SYO (IDXA IDXB)
                                           TO  SRT-CDKBN-SYO(SRT-MAX)
      *
               MOVE    WNYUINKSN-CDKBN-BU  (IDXA IDXB)
                                           TO  SRT-CDKBN-BU (SRT-MAX)
               MOVE    WNYUINKSN-CDKBN-KBN (IDXA IDXB)
                                           TO  SRT-CDKBN-KBN(SRT-MAX)
               MOVE    WNYUINKSN-CDKBN-KBNNUM (IDXA IDXB)
                                           TO  SRT-CDKBN-KBNNUM
                                                            (SRT-MAX)
               MOVE    WNYUINKSN-CDKBN-KBNNUM-EDA (IDXA IDXB)
                                           TO  SRT-CDKBN-KBNNUM-EDA
                                                            (SRT-MAX)
               MOVE    WNYUINKSN-CDKBN-KOUBAN (IDXA IDXB)
                                           TO  SRT-CDKBN-KOUBAN
                                                            (SRT-MAX)
               MOVE    WNYUINKSN-KOUHYOJYUNNUM (IDXA IDXB)
                                           TO  SRT-KOUHYOJYUNNUM
                                                            (SRT-MAX)
      *
               MOVE    WNYUINKSN-ZAITEN (IDXA IDXB)
                                           TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WNYUINKSN-ZAIKAISU (IDXA IDXB)
                                           TO  SRT-ZAIKAISU (SRT-MAX)
      *
               MOVE    "6"                 TO  SRT-ZAISKBKBN(SRT-MAX)
               MOVE    WNYUINKSN-SRYCD (IDXA IDXB)
                                           TO  SRT-SRYCD    (SRT-MAX 1)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WNYUINKSN-DAY (IDXA IDXB IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-NYUINKSN-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©»ö£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       200111-SYOKUJI-SRT-SET-SEC      SECTION.
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WSYOKUJI-OCC1-MAX )
                    OR   ( SRT-MAX >=  CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    WSYOKUJI-SRYKBN (IDXB)
                                           TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    1                   TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    WSYOKUJI-NUM (IDXB)
                                           TO  SRT-PRIORITY2(SRT-MAX)
               MOVE    WSYOKUJI-KANKYO (IDXB)
                                           TO  SRT-PRIORITY3(SRT-MAX)
      *
               MOVE    WSYOKUJI-ZAITEN (IDXB)
                                           TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WSYOKUJI-ZAIKAISU(IDXB)
                                           TO  SRT-ZAIKAISU (SRT-MAX)
      *
               MOVE    "4"                 TO  SRT-ZAISKBKBN(SRT-MAX)
               PERFORM VARYING IDXC    FROM    1   BY  1
                       UNTIL ( IDXC    >   5 )
                   MOVE    WSYOKUJI-SURYO (IDXB IDXC)
                                           TO  SRT-SURYO (SRT-MAX IDXC)
                   MOVE    WSYOKUJI-SRYCD (IDXB IDXC)
                                           TO  SRT-SRYCD (SRT-MAX IDXC)
               END-PERFORM
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WSYOKUJI-DAY (IDXB IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-SYOKUJI-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¼¼ÎÁº¹³Û£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       200111-BRMSAGAKU-SRT-SET-SEC    SECTION.
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WBRMSAGAKU-OCC1-MAX )
                    OR   ( SRT-MAX >=  CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    "95"                TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    1                   TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    WBRMSAGAKU-NUM (IDXB)
                                           TO  SRT-PRIORITY2(SRT-MAX)
      *
               MOVE    WBRMSAGAKU-ZAITEN (IDXB)
                                           TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WBRMSAGAKU-ZAIKAISU(IDXB)
                                           TO  SRT-ZAIKAISU (SRT-MAX)
      *
               MOVE    WBRMSAGAKU-SAGAKUCD(IDXB)
                                           TO  SRT-SAGAKUCD (SRT-MAX)
      *
               MOVE    "3"                 TO  SRT-ZAISKBKBN(SRT-MAX)
               MOVE    WBRMSAGAKU-SRYCD (IDXB)
                                           TO  SRT-SRYCD (SRT-MAX 1)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WBRMSAGAKU-DAY (IDXB IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-BRMSAGAKU-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÃ´Åö¼êÅö£Ó£Ï£Ò£Ôºî¶ÈÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       200111-RYYTAN-SRT-SET-SEC       SECTION.
      *
           IF    ( SRT-MAX     <   CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    WRYYTAN-SRYKBN      TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    ZERO                TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    ZERO                TO  SRT-PRIORITY2(SRT-MAX)
      *
               MOVE    WRYYTAN-ZAITEN      TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WRYYTAN-ZAIKAISU    TO  SRT-ZAIKAISU (SRT-MAX)
      *
               MOVE    "7"                 TO  SRT-ZAISKBKBN(SRT-MAX)
               MOVE    WRYYTAN-SRYCD
                                           TO  SRT-SRYCD (SRT-MAX 1)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WRYYTAN-DAY (IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-IF
      *
           .
       200111-RYYTAN-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ï£Ò£Ôºî¶ÈÎÎ°è£Ó£Ï£Ò£Ô½èÍý
      *****************************************************************
       20011-SORTAREA-SORT-SEC         SECTION.
      *
      *    ¥½¡¼¥ÈÍÑ¥ï¡¼¥¯¤Ë¤Ä¤¤¤ÆÊÂ¤ÓÂØ¤¨¤ò¹Ô¤¦
           PERFORM VARYING SRT-IDX1    FROM    2   BY  1
                   UNTIL ( SRT-IDX1    >   SRT-MAX )
               COMPUTE SRT-STIDX   =   SRT-IDX1    -   1
               MOVE    SRT-OCC1 (SRT-IDX1)
                                   TO  SRT-OCC1 (101)
               PERFORM VARYING SRT-IDX2    FROM    SRT-STIDX   BY  -1
                       UNTIL ( SRT-IDX2    <   1 )
                          OR ( SRT-KEY (101)
                                       >=  SRT-KEY (SRT-IDX2) )
                       MOVE    SRT-OCC1 (SRT-IDX2)
                                   TO  SRT-OCC1 (SRT-IDX2 + 1)
               END-PERFORM
               MOVE    SRT-OCC1 (101)
                                   TO  SRT-OCC1 (SRT-IDX2 + 1)
           END-PERFORM
      *
           .
       20011-SORTAREA-SORT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÖµÑÃÍÀßÄê½èÍý
      *****************************************************************
       2001-HENKYAKU-SET-SEC           SECTION.
      *
           COMPUTE HCM34S01-SENTEI-PTFTN
               =   WRK-SENTEI-PTFTN (1)
               +   WRK-SENTEI-PTFTN (2)
      *
           MOVE    ZERO                TO  HCM34S01-HENKYAKU-MAX
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   SRT-MAX )
                    OR   ( IDXB    >   CONST-HENKYAKU-MAX )
      *
               IF    ( SRT-ZAIKAISU (IDXB) NOT =   ZERO )
      *
                   COMPUTE HCM34S01-HENKYAKU-MAX
                       =   HCM34S01-HENKYAKU-MAX   +   1
      *
                   MOVE    HCM34S01-HENKYAKU-MAX   TO  IDXE
      *
                   MOVE    SRT-SRYKBN   (IDXB) TO  HCM34S01-SRYKBN
                                                              (IDXE)
                   IF    ( SRT-ZAISKBKBN(IDXB) =   "7" )
                       MOVE    "850"           TO  HCM34S01-SRYSYUKBN
                                                              (IDXE)
                   END-IF
                   MOVE    SRT-ZAITEN   (IDXB) TO  HCM34S01-ZAITEN
                                                              (IDXE)
                   MOVE    SRT-ZAIKAISU (IDXB) TO  HCM34S01-ZAIKAISU
                                                              (IDXE)
                   MOVE    SRT-ZAISKBKBN(IDXB) TO  HCM34S01-ZAISKBKBN
                                                              (IDXE)
                   MOVE    SRT-PRIORITY (IDXB) TO  HCM34S01-PRIORITY
                                                              (IDXE)
                   MOVE    SRT-PRIORITY2(IDXB) TO  HCM34S01-PRIORITY2
                                                              (IDXE)
                   MOVE    SRT-PRIORITY3(IDXB) TO  HCM34S01-PRIORITY3
                                                              (IDXE)
                   PERFORM VARYING IDXC    FROM    1   BY  1
                           UNTIL ( IDXC    >   5 )
                       IF    ( HCM34S01-SANTEIYM   >=  CONST-H1804 )
                        AND  ( SRT-SRYKBN (IDXB)   =   "97" )
                           MOVE    SRT-SURYO(IDXB IDXC)
                                               TO  HCM34S01-SURYO
                                                           (IDXE IDXC)
                       ELSE
                           MOVE    1           TO  HCM34S01-SURYO
                                                           (IDXE IDXC)
                       END-IF
                       MOVE    SRT-SRYCD(IDXB IDXC)
                                               TO  HCM34S01-SRYCD
                                                           (IDXE IDXC)
      *
                       MOVE    SRT-SRYCD(IDXB IDXC)
                                               TO  WRK-SRYCD
                       IF    ( WRK-SRYCD   NOT =   SPACE )
                           COMPUTE HCM34S01-MEISAISU (IDXE)
                               =   HCM34S01-MEISAISU (IDXE)
                               +   1
                           COMPUTE HCM34S01-SURYOUTOTAL (IDXE)
                               =   HCM34S01-SURYOUTOTAL (IDXE)
                               +   HCM34S01-SURYO (IDXE IDXC)
                           IF    ( WRK-SRYCD   IS  NUMERIC )
                               COMPUTE HCM34S01-SRYCDTOTAL (IDXE)
                                   =   HCM34S01-SRYCDTOTAL (IDXE)
                                   +   WRK-SRYCD-9
                               COMPUTE HCM34S01-RUIKEITOTAL(IDXE)
                                   =   HCM34S01-RUIKEITOTAL(IDXE)
                                   +   WRK-SRYCD-9 
                           END-IF
                       END-IF
      *
                   END-PERFORM
      *
                   PERFORM VARYING IDXD    FROM    1   BY  1
                           UNTIL ( IDXD    >   31 )
                       MOVE    SRT-DAY  (IDXB IDXD)
                                               TO  HCM34S01-DAY
                                                           (IDXE IDXD)
      *
                       IF    ( SRT-ZAISKBKBN(IDXB) =   "3" )
                           IF    ( SRT-DAY  (IDXB IDXD)    >   ZERO )
                               MOVE    SRT-SAGAKUCD (IDXB)
                                               TO  HCM34S01-SAGAKUCD
                                                           (IDXD)
                           END-IF
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       2001-HENKYAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    ZERO                TO  HCM34S01-RC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           INITIALIZE                      TNS-TENSU-REC
           MOVE    HCM34S01-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           ELSE
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ÁªÄêÎÅÍÜ¥³¡¼¥ÉÃÖ´¹¸¡º÷½èÍý
      *****************************************************************
       900-SENTEICDCHG-KEY2-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-SENTEICDCHG
      *
           INITIALIZE  SENTEICDCHG-REC
           MOVE    NSRY-SRYCD (IDX-KHN)
                                       TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
           ELSE
               MOVE    1               TO  FLG-SENTEICDCHG
               INITIALIZE                  SENTEICDCHG-REC
           END-IF
      *
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SENTEICDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ëÆÉ¹þ½èÍý
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DBCLOSECURSOR-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ¥×¥í¥°¥é¥à½ªÎ»½èÍý¡Ê¥¨¥é¡¼»þ¡Ë
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "ORCHCM34S01 ERRCD [" HCM34S01-RC "]"
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
