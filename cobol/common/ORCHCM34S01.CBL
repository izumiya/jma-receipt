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
           03  FLG-DBRC                        PIC 9(01).
           03  FLG-OVER                        PIC 9(01).
           03  FLG-HIT                         PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-NYUINACT                    PIC 9(01).
           03  FLG-SENTEICDCHG                 PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-TAISYO                      PIC 9(01).
           03  FLG-NYUINARI                    PIC 9(01).
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
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-SYS5000                     PIC 9(05).
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
           03  IDXA                            PIC 9(05).
           03  IDXB                            PIC 9(05).
           03  IDXC                            PIC 9(05).
           03  IDXD                            PIC 9(05).
           03  IDXE                            PIC 9(05).
           03  IDX-KHN                         PIC 9(05).
           03  IDX-TAX                         PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-DBPATH                      PIC X(100).
           03  WRK-YMD.
               05  WRK-YYMM                    PIC X(06).
               05  WRK-DD                      PIC 9(02).
           03  WRK-DBYMD                       PIC X(08).
           03  WRK-TNS-SRYCD                   PIC X(09).
           03  WRK-GAIHAKU-SRYCD               PIC X(09).
           03  WRK-GAIHAKU-KHNTEN              PIC 9(08).
           03  WRK-NYUKHNKSNKBN                PIC 9(03).
           03  WRK-NACCT-ZAISKBKBN             PIC X(01).
           03  WRK-GAIHAKU-TEN                 PIC 9(08).
           03  WRK-KHNTEN                      PIC S9(08).
           03  WRK-KSNTEN                      PIC S9(08).
           03  WRK-SENTEI-KHNTEN               PIC 9(08).
           03  WRK-SENTEISRYCD                 PIC X(09).
           03  WRK-SRYCDTOTAL                  PIC 9(09).
           03  WRK-SENTEI-KEISU                PIC 9V99.
           03  WRK-BRMSAGAKU                   PIC 9(05).
           03  WRK-SKJ-TEN-G.
               05  WRK-SKJ-TEN-RYOYO1          PIC 9(08).
               05  WRK-SKJ-TEN-RYOYO2          PIC 9(08).
               05  WRK-SKJ-TEN-TOKKANRI        PIC 9(08).
               05  WRK-SKJ-TEN-TOKSYOKU        PIC 9(08).
               05  WRK-SKJ-TEN-SYOKUDO         PIC 9(08).
               05  WRK-SKJ-TEN-FKSKONDATE      PIC 9(08).
           03  WRK-SYS-5113.
               05  WRK-SYS-5113-MAX            PIC 9(03).
               05  WRK-SYS-5113-OCC            OCCURS  30.
                   07  WRK-SYS-5113-STYUKYMD   PIC X(08).
                   07  WRK-SYS-5113-EDYUKYMD   PIC X(08).
                   07  WRK-SYS-5113-KBNCD      PIC 9(02).
                   07  WRK-SYS-5113-BRMSAGAKU  PIC 9(05).
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
           03  WRK-TOKUKANRIKASAN              PIC 9(01).
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
      *            Æþ±¡´ðËÜÎÁ¿ÇÎÅ¶èÊ¬
                   07  WGAIHAKU-SRYKBN         PIC X(02).
      *            ºÞÅÀ¿ô
                   07  WGAIHAKU-ZAITEN         PIC 9(08).
      *            ºÞ²ó¿ô
                   07  WGAIHAKU-ZAIKAISU       PIC 9(07).
      *            ¿ÇÎÅ¥³¡¼¥É
                   07  WGAIHAKU-SRYCD          PIC X(09).
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
           03  WSYOKUJI-OCC1                   OCCURS 20.
      *        ¥½¡¼¥ÈÍÑÈÖ¹æ
               05  WSYOKUJI-NUM                PIC 9(03).
      *        ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¹ç·×
               05  WSYOKUJI-SRYCDTOTAL         PIC 9(14).
      *        Æþ±¡´ðËÜÎÁ¿ÇÎÅ¶èÊ¬
               05  WSYOKUJI-SRYKBN             PIC X(02).
      *        ºÞÅÀ¿ô
               05  WSYOKUJI-ZAITEN             PIC 9(08).
      *        ºÞ²ó¿ô
               05  WSYOKUJI-ZAIKAISU           PIC 9(07).
      *        ¿ÇÎÅ¥³¡¼¥É
               05  WSYOKUJI-SRYCD              PIC X(09)   OCCURS  5.
      *        ¿ÇÎÅ²ó¿ô¥Æ¡¼¥Ö¥ë
               05  WSYOKUJI-DAY-TBL.
                   07  WSYOKUJI-DAY            PIC 9(03)   OCCURS  31.
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
               05  SRT-SRYCD                   PIC X(09)   OCCURS  5.
               05  SRT-DAY-TBL.
                   07  SRT-DAY                 PIC 9(03)   OCCURS  31.
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
       01  CONST-AREA.
      *    Æþ±¡ÎÁÊÔ½¸ºî¶ÈÎÎ°èºÇÂç·ï¿ô
           03  CONST-WNYUINRYO-MAX             PIC 9(05)   VALUE 20.
      *    ³°ÇñÊÔ½¸ºî¶ÈÎÎ°èºÇÂç·ï¿ô
           03  CONST-WGAIHAKU-MAX              PIC 9(05)   VALUE 20.
      *    ¼¼ÎÁº¹³ÛÊÔ½¸ºî¶ÈÎÎ°èºÇÂç·ï¿ô
           03  CONST-WBRMSAGAKU-MAX            PIC 9(05)   VALUE 20.
      *    ¿©»öÊÔ½¸ºî¶ÈÎÎ°èºÇÂç·ï¿ô
           03  CONST-WSYOKUJI-MAX              PIC 9(05)   VALUE 20.
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
      *
           COPY    "CPSK5000.INC"  REPLACING   //SYS-5000//
                                   BY          //WKSYS-5000//.
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
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
                                           WNYUINKSN-AREA
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
           MOVE    HCM34S01-HOSPID         TO  PTINF-HOSPID
           MOVE    HCM34S01-PTID           TO  PTINF-PTID
           MOVE    PTINF-REC               TO  MCPDATA-REC
           MOVE    "PTINF-KEY"             TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC                NOT =   ZERO )
               MOVE    2                   TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ³ºÅöÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¤ÎÆþ±¡²ñ·×¤¬Â¸ºß¤¹¤ë¤«¥Á¥§¥Ã¥¯
           INITIALIZE                          FLG-NYUINDAY-G
      *
           MOVE    "5"                     TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           MOVE    "NYUINACCT-KEY17"       TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
                   MOVE    1               TO  FLG-NYUINDAY (IDX1)
                END-IF
           END-PERFORM
      *
           IF    ( FLG-NYUINARI            =   ZERO )
               MOVE    4                   TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
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
           MOVE    HCM34S01-HOSPID         TO  NACCT-HOSPID
           MOVE    HCM34S01-PTID           TO  NACCT-PTID
           MOVE    HCM34S01-SANTEIYM       TO  NACCT-SRYYM
           MOVE    WRK-NACCT-ZAISKBKBN     TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY17"       TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            NOT =   ZERO )
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
           MOVE    "NYUINACCT-KEY17"   TO  WRK-DBPATH
           PERFORM 900-TBL-READ-SEC
           IF    ( FLG-DBRC            NOT =   ZERO )
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
                                           WRK-TOKUKANRIKASAN
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
           .
       150-INIT-EXT.
           EXIT.
      *****************************************************************
      *    °åÎÅµ¡´Ø¾ðÊó¼èÆÀ½èÍý
      *****************************************************************
       1501-SYS1001-GET-SEC            SECTION.
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    HCM34S01-SANTEIYM   TO  ORC-DBYMD
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               INITIALIZE  SYS-1001-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*"                 TO  SYS-5000-KBNCD
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY4"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
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
               MOVE    "SYSKANRI-KEY4" TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-5000-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SYSKANRI-KEY4"     TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
      *
               IF    ( SYS-5000-SHOKUJIRYOYOCD     =   "197000110" )
                   MOVE     1              TO   WRK-SHOKUJIRYOYO
                   IF    ( SYS-5000-SHOKUKBN(1)    =  "1" )
                       MOVE  ZERO          TO   WRK-TOKUKANRIKASAN
                   ELSE
                       MOVE  1             TO   WRK-TOKUKANRIKASAN
                   END-IF
               ELSE
                   MOVE      2             TO   WRK-SHOKUJIRYOYO
               END-IF
           END-IF
      *
      *    °åÎÅµ¡´Ø¸º»»¾ðÊóÊÔ½¸
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
           PERFORM 2001-SYOKUJI-HEN-SEC
      *
      *    Æþ±¡ÎÁ²Ã»»ÊÔ½¸½èÍý
           PERFORM 2001-NYUINKSN-HEN-SEC
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
           MOVE    "NYUINACCT-KEY17"       TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
                   PERFORM VARYING     IDX1    FROM    1   BY  1
                           UNTIL     ( IDX1    >   31 )
                       IF   ( NACCT-DAY(IDX1)  >   ZERO )
                           MOVE    1       TO  FLG-SENTEIDAY (IDX1)
                       END-IF
                   END-PERFORM
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
           MOVE    "NYUINACCT-KEY17"       TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
           MOVE    "NYUINACCT-KEY17"       TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
           PERFORM 20011-NYUINACT-GET-SEC
           IF    ( FLG-NYUINACT        NOT =    ZERO )
               GO  TO  200111-GAIHAKU-KHNTEN-GET-EXT
           END-IF
      *
      *    Æþ±¡´ðËÜÎÁ¤Î¥³¡¼¥É¤òÃµ¤¹
           MOVE    ZERO                TO  WRK-KHNTEN
           MOVE    ZERO                TO  IDX-KHN
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   5 )
                    OR   ( IDX-KHN NOT =   ZERO )
               MOVE    NSRY-SRYCD (IDX2)
                                       TO  WRK-TNS-SRYCD
               MOVE    NACCT-SRYYM     TO  WRK-DBYMD
               MOVE    "01"            TO  WRK-DBYMD (7:2)
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   IF    ( TNS-DATAKBN =   1 )
                       MOVE    TNS-TEN             TO  WRK-KHNTEN
                       MOVE    TNS-NYUKHNKSNKBN    TO  WRK-NYUKHNKSNKBN
                       MOVE    IDX2                TO  IDX-KHN
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( IDX-KHN             =   ZERO )
               MOVE    5               TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    NSRY-SRYCD (IDX-KHN)
                                       TO  WRK-TNS-SRYCD
      *
      *    ´Ç¸îÈæÎ¨´ð½àÌ¤Ã£¤Î¥Á¥§¥Ã¥¯
           PERFORM 2001111-KNGMITATU-CHK-SEC
           IF    ( WRK-GAIHAKU-KHNTEN  NOT =   ZERO )
               MOVE    WRK-GAIHAKU-KHNTEN  TO  WRK-KHNTEN
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
           IF    ( WRK-GAIHAKU-SRYCD   NOT =   SPACE )
               MOVE    WRK-GAIHAKU-SRYCD   TO  WRK-TNS-SRYCD
               MOVE    NACCT-SRYYM     TO  WRK-DBYMD
               MOVE    "01"            TO  WRK-DBYMD (7:2)
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
      *
           IF    ( NACCT-SRYKBN            =   "90" )
               EVALUATE    FLG-GAIHAKUDAY (IDXD)
               WHEN    1
                   MOVE    "190107290"     TO  WRK-GAIHAKU-SRYCD
               WHEN    2
                   MOVE    "190107390"     TO  WRK-GAIHAKU-SRYCD
               WHEN    3
                   MOVE    "190111590"     TO  WRK-GAIHAKU-SRYCD
               WHEN    4
                   IF    ( FLG-SENTEIDAY(IDXD) =   ZERO )
                       PERFORM 2001111-TAKIKAN-SEC
                   ELSE
                       PERFORM 2001111-SENTEI-TAKIKAN-SEC
                   END-IF
               END-EVALUATE
           ELSE
               EVALUATE    FLG-GAIHAKUDAY (IDXD)
               WHEN    1
                   MOVE    "190107490"     TO  WRK-GAIHAKU-SRYCD
               WHEN    2
                   MOVE    "190107590"     TO  WRK-GAIHAKU-SRYCD
               WHEN    3
                   MOVE    "190111590"     TO  WRK-GAIHAKU-SRYCD
               WHEN    4
                   IF    ( FLG-SENTEIDAY(IDXD) =   ZERO )
                       PERFORM 2001111-TAKIKAN-SEC
                   ELSE
                       PERFORM 2001111-SENTEI-TAKIKAN-SEC
                   END-IF
               END-EVALUATE
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
           WHEN    201 THRU    219
           WHEN    221 THRU    222
           WHEN    224
           WHEN    227 THRU    228
           WHEN    230 THRU    232
               MOVE    "190108790"     TO  WRK-GAIHAKU-SRYCD
           WHEN    013 THRU    021
           WHEN    101 THRU    109
               MOVE    "190108890"     TO  WRK-GAIHAKU-SRYCD
           WHEN    075 THRU    080
               MOVE    "190108990"     TO  WRK-GAIHAKU-SRYCD
           WHEN    085 THRU    086
           WHEN    132 THRU    133
               MOVE    "190109090"     TO  WRK-GAIHAKU-SRYCD
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
           WHEN    201 THRU  219
           WHEN    221 THRU  222
           WHEN    224
           WHEN    227 THRU  228
           WHEN    230 THRU  232
               MOVE    "190116590"     TO  WRK-GAIHAKU-SRYCD
           WHEN    013 THRU  021
           WHEN    101 THRU  109
               MOVE    "190116690"     TO  WRK-GAIHAKU-SRYCD
           WHEN    075 THRU  080
               MOVE    "190116890"     TO  WRK-GAIHAKU-SRYCD
           WHEN    085 THRU  086
           WHEN    132 THRU  133
               MOVE   "190116790"      TO  WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           .
       2001111-SENTEI-TAKIKAN-EXT.
           EXIT.
      *****************************************************************
      *    ³°ÇñÅÀ¿ô¼èÆÀ½èÍý
      *****************************************************************
       200111-GAIHAKU-TENSU-GET-SEC    SECTION.
      *
           MOVE    ZERO                TO  WRK-GAIHAKU-TEN
      *
           IF  ( FLG-SENTEIDAY (IDXD) NOT  =  ZERO )
      *
               PERFORM 200111-SENTEI-KEISU-GET-SEC
      *
               EVALUATE    FLG-GAIHAKUDAY(IDXD)
               WHEN    1
               WHEN    3
               WHEN    4
                   COMPUTE WRK-GAIHAKU-TEN
                       = ( WRK-KHNTEN  *   WRK-SENTEI-KEISU )  +  0.5
               END-EVALUATE
           ELSE
               EVALUATE  FLG-GAIHAKUDAY(IDXD)
               WHEN   1
                   COMPUTE WRK-GAIHAKU-TEN
                       = ( WRK-KHNTEN  *   0.15)   +  0.5
               WHEN   4
                   IF    ( NACCT-SRYYM     <=  "200403" )
                       COMPUTE WRK-GAIHAKU-TEN
                           = ( WRK-KHNTEN  *   0.15)   +  0.5
                   ELSE
                       COMPUTE WRK-GAIHAKU-TEN
                           = ( WRK-KHNTEN  *   0.3 )   +  0.5
                   END-IF
               WHEN   2
                   COMPUTE WRK-GAIHAKU-TEN
                       = ( WRK-KHNTEN  *   0.3 )   +  0.5
               END-EVALUATE
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
                     OR (( WRK-GAIHAKU-SRYCD
                                   =   WGAIHAKU-SRYCD (IDXA IDXB)) 
                      AND( WRK-GAIHAKU-TEN
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
                   MOVE    WRK-GAIHAKU-TEN
                                       TO  WGAIHAKU-ZAITEN (IDXA IDXB)
                   MOVE    WRK-GAIHAKU-SRYCD
                                       TO  WGAIHAKU-SRYCD  (IDXA IDXB)
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
           MOVE    NACCT-HOSPID        TO  NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "NYUINACT-KEY9"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF   (  FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACT
           ELSE
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           END-IF
      *
      *    Æþ±¡¹Ô°Ù¥¯¥í¡¼¥º
           MOVE    "NYUINACT-KEY9"     TO  ORC-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
           MOVE    WRK-SENTEISRYCD     TO  WNYUINRYO-SRYCD
                                                   (IDXA IDXB IDX-KHN)
      *
           COMPUTE WNYUINRYO-ZAITEN (IDXA IDXB)    =   WRK-SENTEI-KHNTEN
                                                   +   WRK-KSNTEN
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
                                           WRK-SENTEI-KHNTEN
      *
      *    Æþ±¡´ðËÜÎÁ¤Î¥³¡¼¥É¤òÃµ¤¹
           MOVE    ZERO                TO  IDX-KHN
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   5 )
                    OR   ( IDX-KHN NOT =   ZERO )
               MOVE    NSRY-SRYCD (IDXC)
                                       TO  WRK-TNS-SRYCD
               MOVE    NACCT-SRYYM     TO  WRK-DBYMD
               MOVE    "01"            TO  WRK-DBYMD (7:2)
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   IF    ( TNS-DATAKBN =   1 )
                       MOVE    TNS-TEN TO  WRK-KHNTEN
                       MOVE    IDXC    TO  IDX-KHN
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
           COMPUTE WRK-KSNTEN  =   NACCT-ZAITEN    -   WRK-KHNTEN
      *
      *    ¾å¤Çµá¤á¤¿Æþ±¡´ðËÜÎÁ¤Î¥³¡¼¥É¤òÁªÄêÆþ±¡ÎÁ¤Î¥³¡¼¥É¤ËÃÖ¤­´¹¤¨¤ë
           MOVE    ZERO                TO  FLG-SENTEICDCHG
           INITIALIZE  SENTEICDCHG-REC
           MOVE    NSRY-SRYCD (IDX-KHN)
                                       TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           MOVE    "SENTEICDCHG-KEY2"  TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-SENTEICDCHG
           ELSE
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
           END-IF
      *
           MOVE    "SENTEICDCHG-KEY2"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-SENTEICDCHG NOT =   ZERO )
               MOVE    10              TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    SENCHG-SENTEISRYCD  TO  WRK-SENTEISRYCD
      *
      *    ÅÀ¿ô¤â²þ¤á¤Æµá¤á¤Ê¤ª¤¹
           MOVE    SENCHG-SENTEISRYCD  TO  WRK-TNS-SRYCD
           MOVE    NACCT-SRYYM         TO  WRK-DBYMD
           MOVE    "01"                TO  WRK-DBYMD (7:2)
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               MOVE    11              TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    TNS-TEN             TO  WRK-SENTEI-KHNTEN
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
           MOVE    HCM34S01-HOSPID     TO  AGECHK-HOSPID
           MOVE    HCM34S01-PTID       TO  AGECHK-PTID
           MOVE    HCM34S01-SANTEIYM   TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
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
               MOVE    HCM34S01-HOSPID     TO  SBTUSENTEI-HOSPID
               MOVE    HCM34S01-PTID       TO  SBTUSENTEI-PTID
               MOVE    HCM34S01-SANTEIYM   TO  SBTUSENTEI-SRYYM
               CALL    "ORCSBTUSENTEI"     USING
                                           ORCSBTUSENTEIAREA
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
               WHEN   ZERO
                   COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                       =   WRK-SENTEI-PTFTN (IDX-TAX)
                       + ( WRK-KHNTEN  -   WRK-SENTEI-KHNTEN )
                       *   WRK-SENTEI-TANKA
      *        ³°Çñ¡Ê£±£µ¡ó¤ÈÁªÄê³°Çñ¤Îº¹³Û¡Ë
               WHEN    3
               WHEN    4
                   IF    ( FLG-GAIHAKUDAY(IDXD)    =   4 )
                    AND  ( HCM34S01-SANTEIYM      >=   "200404" )
                       COMPUTE WRK-SENTEI-GAIHAKU1
      *                    = ( WRK-KHNTEN  *   0.3 )
                           = ( WRK-KHNTEN  *   0.0 )
                           +   0.5
                   ELSE
                       COMPUTE WRK-SENTEI-GAIHAKU1
                           = ( WRK-KHNTEN  *   0.15 )
                           +   0.5
                   END-IF
                   COMPUTE WRK-SENTEI-GAIHAKU2
                       = ( WRK-KHNTEN  *   WRK-SENTEI-KEISU)
                       +   0.5
                   COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                       =   WRK-SENTEI-PTFTN (IDX-TAX)
                       + ( WRK-SENTEI-GAIHAKU1
                       -   WRK-SENTEI-GAIHAKU2 )
                       *   WRK-SENTEI-TANKA
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
                   COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                       =   WRK-SENTEI-PTFTN (IDX-TAX)
                       +   WRK-SENTEI-KINGAKU
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
           MOVE    "NYUINACCT-KEY17"       TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
           MOVE    "5113"              TO  SYS-5113-KANRICD
           MOVE    SYS-5113-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY5"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            NOT =   ZERO )
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
               MOVE    "SYSKANRI-KEY5" TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC            NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-5113-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SYSKANRI-KEY5"     TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
                     OR  ( WRK-BRMSAGAKU
                                   =   WBRMSAGAKU-ZAITEN(IDXB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF   (    IDXB  >   WBRMSAGAKU-OCC1-MAX )
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
           MOVE    "NYUINACCT-KEY17"       TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
           MOVE    HCM34S01-SANTEIYM   TO  WRK-DBYMD
           MOVE    "01"                TO  WRK-DBYMD (7:2)
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-RYOYO1
           END-IF
      *
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
           MOVE    CONST-SKJ-RYOYO2    TO  WRK-TNS-SRYCD
           MOVE    HCM34S01-SANTEIYM   TO  WRK-DBYMD
           MOVE    "01"                TO  WRK-DBYMD (7:2)
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-RYOYO2
           END-IF
      *
      *    ÆÃÊÌ´ÉÍý²Ã»»
           MOVE    CONST-SKJ-TOKKANRI  TO  WRK-TNS-SRYCD
           MOVE    HCM34S01-SANTEIYM   TO  WRK-DBYMD
           MOVE    "01"                TO  WRK-DBYMD (7:2)
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-TOKKANRI
           END-IF
      *
      *    ÆÃÊÌ¿©²Ã»»
           MOVE    CONST-SKJ-TOKSYOKU  TO  WRK-TNS-SRYCD
           MOVE    HCM34S01-SANTEIYM   TO  WRK-DBYMD
           MOVE    "01"                TO  WRK-DBYMD (7:2)
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-TOKSYOKU
           END-IF
      *
      *    ¿©Æ²²Ã»»
           MOVE    CONST-SKJ-SYOKUDO   TO  WRK-TNS-SRYCD
           MOVE    HCM34S01-SANTEIYM   TO  WRK-DBYMD
           MOVE    "01"                TO  WRK-DBYMD (7:2)
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-SYOKUDO
           END-IF
      *
      *    Ê£¿ô¸¥Î©²Ã»»
           MOVE    CONST-SKJ-FKSKONDATE
                                       TO  WRK-TNS-SRYCD
           MOVE    HCM34S01-SANTEIYM   TO  WRK-DBYMD
           MOVE    "01"                TO  WRK-DBYMD (7:2)
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-FKSKONDATE
           END-IF
      *
           .
       20011-SKJ-KHNTEN-GET-EXT.
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
           MOVE    "NYUINACCT-KEY17"       TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
                AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR 4 )
                    IF   ( FLG-NYUINRYODAY (IDXD)  =   1 )
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
           INITIALIZE                      WRK-SKJ-SRYCD-G
           MOVE    ZERO                TO  IDX5
      *
           EVALUATE    NACCT-DAY (IDXD)
           WHEN  1
               EVALUATE    WRK-SHOKUJIRYOYO
               WHEN    1
      *
      *            Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                   PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *            ÆÃÊÌ´ÉÍý²Ã»»
                   IF    ( WRK-TOKUKANRIKASAN  =  1 )
                       PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
                   END-IF
      *
      *            ¿©Æ²²Ã»»
                   IF    ( FLG-SYOKUDODAY(IDXD)    NOT =   ZERO )
                       PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                   END-IF
               WHEN  2
      *
      *            Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                   PERFORM 2001111-SKJ-RYOYO2-SET-SEC
               END-EVALUATE
           WHEN  2
      *
      *        Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
               PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *        ÆÃÊÌ´ÉÍý²Ã»»
               IF    ( WRK-TOKUKANRIKASAN  =  1 )
                   PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
               END-IF
      *
      *        ÆÃÊÌ¿©²Ã»»
               PERFORM 2001111-SKJ-TOKSYOKU-SET-SEC
      *
      *        ¿©Æ²²Ã»»
               IF    ( FLG-SYOKUDODAY(IDXD)    NOT =   ZERO )
                   PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
               END-IF
           WHEN  3
      *
      *        Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
               PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *        ÆÃÊÌ´ÉÍý²Ã»»
               IF    ( WRK-TOKUKANRIKASAN      =  1 )
                   PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
               END-IF
      *
      *        ¿©Æ²²Ã»»
               IF    ( FLG-SYOKUDODAY(IDXD)    NOT =   ZERO )
                   PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
               END-IF
      *
      *        Ê£¿ô¸¥Î©²Ã»»
               PERFORM 2001111-SKJ-FKSKONDATE-SET-SEC
      *
          WHEN  4
      *
      *        Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
               PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *        ÆÃÊÌ´ÉÍý²Ã»»
               IF    ( WRK-TOKUKANRIKASAN      =  1 )
                   PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
               END-IF
      *
      *        ÆÃÊÌ¿©²Ã»»
               PERFORM 2001111-SKJ-TOKSYOKU-SET-SEC
      *
      *        ¿©Æ²²Ã»»
               IF    ( FLG-SYOKUDODAY(IDXD)    NOT =   ZERO )
                   PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
               END-IF
      *
      *        Ê£¿ô¸¥Î©²Ã»»
               PERFORM 2001111-SKJ-FKSKONDATE-SET-SEC
      *
           END-EVALUATE
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
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡ËÀßÄê½èÍý
      *****************************************************************
       2001111-SKJ-RYOYO1-SET-SEC      SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-RYOYO1    TO  WRK-SKJ-SRYCD (IDX5)
                                               WRK-SRYCD-NUM
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
                                               WRK-SRYCD-NUM
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
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-TOKKANRI  TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-TOKKANRI
           END-IF
      *
           .
       2001111-SKJ-TOKKANRI-SET-EXT.
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
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-SYOKUDO   TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-SYOKUDO
           END-IF
      *
           .
       2001111-SKJ-SYOKUDO-SET-EXT.
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
                       MOVE    HCM34S01-SANTEIYM
                                           TO  WRK-DBYMD
                       MOVE    "01"        TO  WRK-DBYMD (7:2)
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
                    AND  ( FLG-NYUINRYODAY (IDXD)      =   1 )
                    AND  ( FLG-NYUINGENDAY (IDX4 IDXD) =   1 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)        =   ZERO )
                    AND  ( FLG-TOKTEIDAY (IDXD)    NOT =   ZERO )
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
               MOVE    HCM34S01-SANTEIYM
                                       TO  WRK-DBYMD
               MOVE    "01"            TO  WRK-DBYMD (7:2)
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   PERFORM 200111-NYUINKSN-HEN-SEC
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "NYUINACCT-KEY17"       TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
                    AND  ( FLG-GAIHAKUDAY(IDXD)        =   ZERO OR 4 )
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
                    AND  ( FLG-NYUINRYODAY (IDXD)      =   1 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)        =   ZERO OR 4 )
                    AND  ( FLG-TOKTEIDAY (IDXD)    NOT =   ZERO )
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
                    OR   ( SRT-MAX >   CONST-SRT-MAX )
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
                    OR   ( SRT-MAX >   CONST-SRT-MAX )
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
               MOVE    WGAIHAKU-SRYCD (IDXA IDXB)
                                           TO  SRT-SRYCD    (SRT-MAX 1)
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
                    OR   ( SRT-MAX >   CONST-SRT-MAX )
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
                    OR   ( SRT-MAX >   CONST-SRT-MAX )
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
                    OR   ( SRT-MAX >   CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    "99"                TO  SRT-SRYKBN   (SRT-MAX)
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
           IF    ( SYS-1001-TAX-HASU-KBN   =   "1" )
               COMPUTE HCM34S01-SENTEI-PTFTN
                   =   WRK-SENTEI-PTFTN (1)
                   +   WRK-SENTEI-PTFTN (2)
                   *   1.05
                   *   0.5
           ELSE
               COMPUTE HCM34S01-SENTEI-PTFTN
                   =   WRK-SENTEI-PTFTN (1)
                   +   WRK-SENTEI-PTFTN (2)
                   *   1.05
           END-IF
      *
           MOVE    ZERO                TO  HCM34S01-HENKYAKU-MAX
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   SRT-MAX )
                    OR   ( IDXB    >   CONST-HENKYAKU-MAX )
      *
               IF    ( SRT-ZAITEN   (IDXB) NOT =   ZERO )
                AND  ( SRT-ZAIKAISU (IDXB) NOT =   ZERO )
      *
                   COMPUTE HCM34S01-HENKYAKU-MAX
                       =   HCM34S01-HENKYAKU-MAX   +   1
      *
                   MOVE    HCM34S01-HENKYAKU-MAX   TO  IDXE
      *
                   MOVE    SRT-SRYKBN   (IDXB) TO  HCM34S01-SRYKBN
                                                              (IDXE)
                   MOVE    SRT-ZAITEN   (IDXB) TO  HCM34S01-ZAITEN
                                                              (IDXE)
                   MOVE    SRT-ZAIKAISU (IDXB) TO  HCM34S01-ZAIKAISU
                                                              (IDXE)
                   MOVE    SRT-ZAISKBKBN(IDXB) TO  HCM34S01-ZAISKBKBN
                                                              (IDXE)
                   PERFORM VARYING IDXC    FROM    1   BY  1
                           UNTIL ( IDXC    >   5 )
                       MOVE    SRT-SRYCD(IDXB IDXC)
                                               TO  HCM34S01-SRYCD
                                                           (IDXE IDXC)
                   END-PERFORM
      *
                   PERFORM VARYING IDXD    FROM    1   BY  1
                           UNTIL ( IDXD    >   31 )
                       MOVE    SRT-DAY  (IDXB IDXD)
                                               TO  HCM34S01-DAY
                                                           (IDXE IDXD)
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
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-DBYMD           TO  ORC-DBYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "TENSU-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           ELSE
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           END-IF
      *
           MOVE    "TENSU-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       900-TBL-SELECT-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               MOVE    WRK-DBPATH  TO  ORC-DBPATH
               MOVE    "DBFETCH"   TO  MCP-FUNC
               CALL    "ORCMCPSUB"     USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-DBRC
               END-IF
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ëÆÉ¹þ½èÍý
      *****************************************************************
       900-TBL-READ-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-TBL-CLOSE-EXT.
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
