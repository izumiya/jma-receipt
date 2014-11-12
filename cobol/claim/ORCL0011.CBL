      ******************************************************************
      * ORCA -- Online Receipt Computer Advantage
      *
      * Copyright (C)  2000-2001 JMA.         
      *
      * This module is part of ORCA.
      *
      *     ORCA is distributed in the hope that it will be useful, but
      * WITHOUT ANY WARRANTY.  No author or distributor accepts 
      * responsibility to anyone for the consequences of using it or for
      * whether it serves any particular purpose or works at all, unless
      * he says so in writing.
      * Refer to the GNU General Public License for full details. 
      *
      *     Everyone is granted permission to copy, modify and
      * redistribute ORCA, but only under the conditions described in
      * the GNU General Public License.  A copy of this license is
      * supposed to have been given to you along with ORCA so you can
      * know your rights and responsibilities.  It should be in a file
      * named COPYING.  Among other things, the copyright notice and 
      * this notice must be preserved on all copies. 
      ******************************************************************      *
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0011.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : 
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  00/12/01    MCC-Æ£¸¶      ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 01.00.01     NACL-Â¿¡¹Ç¼  03/05/23  £È£±£µÇ¯£¶·î²þÀµÂÐ±þ
      * 01.00.02     NACL-Â¿¡¹Ç¼  03/07/01  ³°Íè¿ÇÎÅÎÁÄÉ²Ã
      * 01.00.03     NACL-Â¿¡¹Ç¼  04/02/17  ¿ÇÎÅ²Ê¡Ç£Ø£Ø¡ÇÄÉ²Ã
      * 01.00.04     NACL-Â¿¡¹Ç¼  04/10/29  ÊÝ¸±ÁÈ¹ç¤»¤ÎÁªÂòÊ¬¤òºÇ½é¤Ø
      * 01.00.05     NACL-ÃÝÅÄ    05/03/02  ¸øÈñ½ÐÎÏ½çÊÑ¹¹
      * 01.00.06     NACL-Â¿¡¹Ç¼  06/03/20  £È£±£¸Ç¯£´·î²þÀµÂÐ±þ
      * 01.00.07     NACL-Â¿¡¹Ç¼  06/05/25  MONFUNCÂÐ±þ
      *
      * 03.03.01     NACL-Æ£¸¶    06/10/15  £²£°£°£¶Ç¯£±£°·î²þÀµÂÐ±þ
      *                                     ¸øÈñ¤ÎÄÉ²Ã
      * 03.03.02     NACL-Æ£¸¶    06/10/31  ÎÅÍÜ²ð¸î°åÎÅ£²£´¤ÎÄÉ²Ã
      * 03.05.00     NACL-Â¿¡¹Ç¼  07/05/09  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      * 03.05.00     NACL-Â¿¡¹Ç¼  07/05/25  UID¥Æ¡¼¥Ö¥ëÂÐ±þ
      * 03.05.01     NACL-Â¿¡¹Ç¼  07/08/10  ¼õÉÕ¥á¥âÄÉ²ÃÂÐ±þ
      * 04.03.00     NACL-Â¿¡¹Ç¼  08/06/11  ¥á¥âÅÐÏ¿¤ÎÁ÷¿®ÂÐ±þ
      * 04.04.00     NACL-ÃÝÅÄ¡¡  09/02/25  ¸ø³²ÂÐ±þ
      * 04.05.00     NACL-Â¿¡¹Ç¼  10/01/15  ¼£¸³¡¦³ÈÄ¥´Á»úÂÐ±þ
      * 04.05.00     NACL-Â¿¡¹Ç¼  10/03/XX  Ê¿À®£²£²Ç¯£´·î²þÀµÂÐ±þ
      * 04.07.00     NACL-Â¿¡¹Ç¼  14/11/XX  ¸øÈñSORTÇÑ»ßÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      **** SELECT  LF03-FILE       ASSIGN  "/var/tmp/LF03V01"
      *                            ORGANIZATION    IS  INDEXED
      *                            ACCESS  MODE    IS  DYNAMIC
      *                            RECORD  KEY     IS  LF03-KEY
      ***                          FILE    STATUS  IS  STS-LF03.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *FD  LF03-FILE.
      *01  LF03-REC. 
      *    COPY    "CPRLF003.INC".
      *
       WORKING-STORAGE             SECTION.
      *        
018000*    ¥Õ¥é¥°ÎÎ°è
      *01  STS-AREA.
      *    03  STS-LF03            PIC X(02).
      *
018000*    ¥Õ¥é¥°ÎÎ°è
018100 01  FLG-AREA.
           03  FLG-END             PIC 9(01).
018900     03  FLG-PTINF           PIC 9(01).
018900     03  FLG-PTKOHINF        PIC 9(01).
018900     03  FLG-PTHKNINF        PIC 9(01).
018900     03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      ***  03  FLG-LF03            PIC 9(01).
           03  FLG-CLMUID          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTMEMOINF       PIC 9(01).
           03  FLG-UID             PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY1                PIC 9(01).
           03  IDZ                 PIC 9(04).
      *
           03  IDX-U               PIC 9(04).
      *
           03  IDX-HZN             PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(04).
               05  WRK-WMM         PIC X(02).
               05  WRK-WDD         PIC X(02).
      *
           03  WRK-POST            PIC X(07).
           03  WRK-HENPOST         PIC X(08).
      *
           03  WRK-ZZZ             PIC Z99.
           03  WRK-999             PIC 9(03).
           03  WRK-SYOYMD          PIC X(08).
           03  WRK-SYOSIN          PIC 9(01).
           03  WRK-SYOSINKBN       PIC X(08).
      *
           03  WRK-SRYKA           PIC X(02).
      *
           03 TBL-MAX              PIC 9(01).
      *
      *    ¸øÈñÊÂÂØ¤¨ÍÑ¥ï¡¼¥¯
           03  WRK-KOHI-AREA.
      *    ÊÂ¤ÓÂØ¤¨Á°
               05  WRK-KOHI-TBL        OCCURS  4.
                   07  WRK-KOHID-O     PIC  9(10).
                   07  WRK-KOHHKNNUM-O PIC  X(03).
                   07  WRK-KOHMEI-O    PIC  X(50).
                   07  WRK-GAIJGNKBN-O PIC  X(01).
      *    ÊÂ¤ÓÂØ¤¨¸å
           03  WRK-KOHI-AREA-X.
               05  WRK-KOHI-TBL-X      OCCURS  4.
                   07  WRK-KOHID       PIC  9(10).
                   07  WRK-KOHHKNNUM   PIC  X(03).
                   07  WRK-KOHMEI      PIC  X(50).
                   07  WRK-GAIJGNKBN   PIC  X(01).
      *    £Ó£Ï£Ò£ÔÍÑ¥ï¡¼¥¯
           03  WRK-KOHI.
               05  WRK-KOHID-W         PIC  9(10).
               05  WRK-KOHHKNNUM-W     PIC  X(03).
               05  WRK-KOHMEI-W        PIC  X(50).
               05  WRK-GAIJGNKBN-W     PIC  X(01).
      *
      *    ¿Ç»¡ÎÁÍÑ
       01  WRK-SINSATU-AREA.
      *    ¼õ¿Ç²ó¿ô
           03  WRK-SINSATU-CNT         PIC 9(04).
      *    £±£µºÐÌ¤ËþºÆ¿ÇÎÁ¤¢¤ê
           03  WRK-15MIMAN-ARI         PIC 9(01).
           03  FLG-15MIMAN             PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *    ¾®»ù²Ê³°Íè¿ÇÎÅ²Ê»ÜÀßµ¬½à¤¢¤ê
           03  FLG-SYONIKA-ARI         PIC 9(01).
      *    ¾®»ù²Ê³°Íè¿ÇÎÅ²Ê»»Äê
           03  FLG-SYONIKA             PIC 9(01).
           03  FLG-SYOSIN              PIC 9(01).
      *    ¿Ç»¡ÎÁ
           03  FLG-SINSATU         PIC 9(01).
      *    Ï·¿Í
           03  FLG-ROUJIN         PIC 9(01).
      *    Ç¯ÎðÈ½ÄêÍÑ
           03  WRK-BIRTHDAY.
               05  WRK-BIRTH-YY    PIC 9(04).
               05  WRK-BIRTH-MM    PIC 9(02).
               05  WRK-BIRTH-DD    PIC 9(02).
      *    Îð·×»»ÎÎ°è
           03  WRK-NENREI.
               05  WRK-NENREI-YY         PIC 9(03).
               05  WRK-NENREI-MM         PIC 9(02).
               05  WRK-NENREI-DD         PIC 9(02).
      *
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
      *
           03  WRK-KAISU               PIC 9(04).
           03  WRK-HOSPSBT             PIC 9(01).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-KBN                 PIC X(01).
           03  WRK-KBN2                PIC X(01).
           03  FLG-ARI                 PIC 9(01).
      *
           03  FLG-ARIS                PIC 9(01).
      *
           03  WRK-LSTYMD              PIC X(08).
           03  WRK-LSTSRYKA            PIC X(02).
      * 
      *    ÉéÃ´Î¨
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(05).
           03  WRK-ZZ9                 PIC ZZZZ9.
      *
           03  WRK-C-UID               PIC X(36).
      *
      *    ´µ¼Ô¥á¥âÊ¬²ò
           03  WRK-MEMO                PIC X(1000).
           03  WRK-MEMO-G.
               05  WRK-MEMO-01         PIC X(1000).
               05  WRK-MEMO-02         PIC X(1000).
               05  WRK-MEMO-03         PIC X(1000).
               05  WRK-MEMO-04         PIC X(1000).
               05  WRK-MEMO-05         PIC X(1000).
               05  WRK-MEMO-OK         PIC X(1000).
      *
           03  WRK-KOUMOKU             PIC X(200).
      *
       01  WRK-SRT-AREA.
           03  WRK-SRT-TABLE.
               05  FILLER              PIC X(03)   VALUE   "027".  
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "024".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "079".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "066".  
               05  FILLER              PIC X(03)   VALUE   "012".
      *********05  FILLER              PIC X(03)   VALUE   "972".
      *********05  FILLER              PIC X(03)   VALUE   "974".
           03  WRK-SRT-TABLE-R         REDEFINES   WRK-SRT-TABLE.      
               05  WRK-SRT-KOHHKNNUM   PIC X(03)   OCCURS  24.
      *
           03  WRK-SRT-KOHHKNNUM-MAX   PIC 9(02)   VALUE   24.
      *  
       01  WRK-HENYMD.
           03  WRK-HENYY           PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "-".  
           03  WRK-HENMM           PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "-".
           03  WRK-HENDD           PIC X(02).
      *
      *    ¿Ç»¡ÎÁ¥Æ¡¼¥Ö¥ë
       01  TBL-SINSATU-AREA.
      *    ½é¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë
           03  FILLER              PIC X(14)   VALUE   "101F 111000110".
      *    ½é¿ÇÎÁ¡ÊÏ·¿Í ÉÂ±¡¡Ë
           03  FILLER              PIC X(14)   VALUE   "111F 111700110".
      *    ½é¿ÇÎÁ¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë
           03  FILLER              PIC X(14)   VALUE   "102F 111003610".
      *    ½é¿ÇÎÁ¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë
           03  FILLER              PIC X(14)   VALUE   "112F 111700210".
      *
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "101  112007410".
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "201  112007510".
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "401  112007610".
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë£±£µºÍÌ¤Ëþ
           03  FILLER              PIC X(14)   VALUE   "401 M112007710".
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "401 E112012310".
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë¿Í¹©¿ÕÂ¡
           03  FILLER              PIC X(14)   VALUE   "401 J112007810".
      *
      *    ÅÅÏÃºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "101T 112007950".
      *    ÅÅÏÃºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "201T 112008050".
      *    ÅÅÏÃºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "401T 112008150".
      *    ÅÅÏÃºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë£±£µºÍÌ¤Ëþ
           03  FILLER              PIC X(14)   VALUE   "401TM112008250".
      *    ÅÅÏÃºÆ¿ÇÎÁ¡Ê°ìÈÌ ÉÂ±¡¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "401TE112012450".
      *
      *    Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "101D 112008350".
      *    Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "201D 112008450".
      *    Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "401D 112008550".
      *    Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë£±£µºÍÌ¤Ëþ
           03  FILLER              PIC X(14)   VALUE   "401DM112008650".
      *    Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "401DE112008750".
      *
      *    ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "101S 112008850".
      *    ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "201S 112008950".
      *    ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "401S 112009050".
      *    ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë£±£µºÍÌ¤Ëþ
           03  FILLER              PIC X(14)   VALUE   "401SM112009150".
      *    ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ÉÂ±¡¡Ë¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "401SE112012550".
      *
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "102  112009210".
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "202  112009310".
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "402  112009410".
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£±£µºÍÌ¤Ëþ
           03  FILLER              PIC X(14)   VALUE   "402 M112009510".
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "402 E112012610".
      *    ºÆ¿ÇÎÁ¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë¿Í¹©¿ÕÂ¡
           03  FILLER              PIC X(14)   VALUE   "402 J112009610".
      *
      *    ÅÅÏÃºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "102T 112009750".
      *    ÅÅÏÃºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "202T 112009850".
      *    ÅÅÏÃºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "402T 112009950".
      *    ÅÅÏÃºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£±£µºÍÌ¤Ëþ
           03  FILLER              PIC X(14)   VALUE   "402TM112010050".
      *    ÅÅÏÃºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "402TE112012750".
      *
      *    Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "102D 112010150".
      *    Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "202D 112010250".
      *    Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "402D 112010350".
      *    Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£±£µºÍÌ¤Ëþ
           03  FILLER              PIC X(14)   VALUE   "402DM112010450".
      *    Æ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "402DE112010550".
      *
      *    ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "102S 112010650".
      *    ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "202S 112010750".
      *    ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "402S 112010850".
      *    ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë£±£µºÍÌ¤Ëþ
           03  FILLER              PIC X(14)   VALUE   "402SM112010950".
      *    ÅÅÏÃÆ±ÆüºÆ¿Ç¡Ê°ìÈÌ ¿ÇÎÅ½ê¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "402SE112012850".
      *
      *    ºÆ¿ÇÎÁ¡ÊÏ·¿Í ÉÂ±¡¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "111  112702310".
      *    ºÆ¿ÇÎÁ¡ÊÏ·¿Í ÉÂ±¡¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "211  112702410".
      *    ºÆ¿ÇÎÁ¡ÊÏ·¿Í ÉÂ±¡¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "411  112702510".
      *    ºÆ¿ÇÎÁ¡ÊÏ·¿Í ÉÂ±¡¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "411 E112706310".
      *    ºÆ¿ÇÎÁ¡ÊÏ·¿Í ÉÂ±¡¡Ë¿Í¹©¿ÕÂ¡
           03  FILLER              PIC X(14)   VALUE   "411 J112702610".
      *
      *    ÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "111T 112702750".
      *    ÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "211T 112702850".
      *    ÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "411T 112702950".
      *    ÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "411TE112706450".
      *
      *    Æ±ÆüºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "111D 112703050".
      *    Æ±ÆüºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "211D 112703150".
      *    Æ±ÆüºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "411D 112703250".
      *    Æ±ÆüºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "411DE112703350".
      *
      *    Æ±ÆüÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "111S 112703450".
      *    Æ±ÆüÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "211S 112703550".
      *    Æ±ÆüÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "411S 112703650".
      *    Æ±ÆüÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ÉÂ±¡¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "411SE112706550".
      *
      *    ºÆ¿ÇÎÁ¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "112  112703710".
      *    ºÆ¿ÇÎÁ¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "212  112703810".
      *    ºÆ¿ÇÎÁ¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "412  112703910".
      *    ºÆ¿ÇÎÁ¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "412 E112706610".
      *    ºÆ¿ÇÎÁ¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë¿Í¹©¿ÕÂ¡
           03  FILLER              PIC X(14)   VALUE   "412 J112704010".
      *
      *    ÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "112T 112704150".
      *    ÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "212T 112704250".
      *    ÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "412T 112704350".
      *    ÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "412TE112706750".
      *
      *    Æ±ÆüºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "112D 112704450".
      *    Æ±ÆüºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "212D 112704550".
      *    Æ±ÆüºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "412D 112704650".
      *    Æ±ÆüºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "412DE112704750".
      *
      *    Æ±ÆüÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£±²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "112S 112704850".
      *    Æ±ÆüÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£²¡¤£³²óÌÜ
           03  FILLER              PIC X(14)   VALUE   "212S 112704950".
      *    Æ±ÆüÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë£´²óÌÜ°Ê¹ß
           03  FILLER              PIC X(14)   VALUE   "412S 112705050".
      *    Æ±ÆüÅÅÏÃºÆ¿Ç¡ÊÏ·¿Í ¿ÇÎÅ½ê¡Ë¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô
           03  FILLER              PIC X(14)   VALUE   "412SE112706850".
      *
      *
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(±¡³°½èÊý ½é¿Ç)
           03  FILLER              PIC X(14)   VALUE   "100  113003510".
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(±¡Æâ½èÊý ½é¿Ç)
           03  FILLER              PIC X(14)   VALUE   "100  113003710".
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(±¡³°½èÊý ºÆ¿Ç)
           03  FILLER              PIC X(14)   VALUE   "200  113003610".
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ(±¡Æâ½èÊý ºÆ¿Ç)
           03  FILLER              PIC X(14)   VALUE   "200  113003810".
      *
      *    ³°Íè¿ÇÎÅÎÁ
           03  FILLER              PIC X(14)   VALUE   "501  112011310".
      *    Æ±Æü³°Íè¿ÇÎÅÎÁ
           03  FILLER              PIC X(14)   VALUE   "501D 112011710".
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÏ·¿Í¡Ë
           03  FILLER              PIC X(14)   VALUE   "511  112705510".
      *    Æ±Æü³°Íè¿ÇÎÅÎÁ¡ÊÏ·¿Í¡Ë
           03  FILLER              PIC X(14)   VALUE   "511D 112705850".
      *
      *
       01  TBL-SINSATU-AREAR       REDEFINES   TBL-SINSATU-AREA.
           03  TBL-SINSATU-OCC         OCCURS  88    INDEXED  TBL-IDX.
      *        ²ó¿ô¡Ê£µ¡§³°Íè¿ÇÎÅÎÁ¡Ë
               05  TBL-KAISU           PIC 9(01).
      *        Ï·¿Í°ìÈÌ¶èÊ¬¡Ê£°¡§°ìÈÌ¡¢£±¡§Ï·¿Í¡Ë
               05  TBL-ROUJIN          PIC 9(01).
      *        ÉÂ±¡¼ïÊÌ¡Ê£±¡§ÉÂ±¡¡¢£²¡§¿ÇÎÅ½ê¡Ë
               05  TBL-HOSPSBT         PIC 9(01).
      *        D:Æ±ÆüºÆ¿Ç¡¢T¡§ÅÅÏÃºÆ¿Ç¡¢S:Æ±ÆüÅÅÏÃºÆ¿Ç¡¢F:½é¿Ç
               05  TBL-KBN             PIC X(01).
      *        M:£±£µºÍÌ¤Ëþ¡¢E¡§¸üÀ¸¾Ê¤¬Äê¤á¤ë´µ¼Ô¡¢J:¿Í¹©¿ÕÂ¡
               05  TBL-KBN2            PIC X(01).
      *        ¿ÇÎÅ¥³¡¼¥É
               05  TBL-SRYCD           PIC X(09).
      *
      *01  TBL-SIN-MAX                 PIC 9(04)   VALUE   84.
       01  TBL-SIN-MAX                 PIC 9(04)   VALUE   88.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
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
      *    ¿ÇÎÅ²ÊÍúÎò
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK1001.INC".
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý½êºßÃÏ¡¢Ï¢ÍíÀè 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1010.INC".
      *
      *    £Ã£Ì£Á£É£ÍÀÜÂ³¾ðÊó
           COPY  "CPSK9000.INC".
      *
      *    ¼õ¿ÇÍúÎò
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡Ý
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ÊÝ¸±ÈÖ¹æ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    £Õ£É£Ä¥Æ¡¼¥Ö¥ë
      *01  CLMUID-REC.
           COPY    "CPCLMUID.INC".
      *
      *    ´µ¼Ô¥á¥â
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ¥¹¥ÑÎÎ°è
      **** COPY    "COMMON-SPA".
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "MCPAREA".
      **** COPY    "CPORCMCP.INC".
      *
           COPY    "MCPDATA.INC".
      *     
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *
      *    ÊÝ¸±Ç¯Îð¡¦ÉéÃ´³ä¹ç»»Äê¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
       01  HZN-LF01-REC. 
           COPY    "CPRLF001.INC"    REPLACING  //LF01-//
                                     BY         //HZN-LF01-//.
      *
      *    Á´³Ñ¥Á¥§¥Ã¥¯
           COPY    "CPORCXKANACONV.INC".
      *
      *    Á´³Ñ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSKANACHK.INC".
      *
      *    £Õ£É£Ä¼èÆÀÍÑ¥¨¥ê¥¢
       01 UIDIO-AREA.
           03  C-RET   PIC X(2).
           03  C-UID   PIC X(36).
       01  ST          PIC 9(2).
      * 
           COPY    "CPMML0031.INC".
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *01  LNK-PTID             PIC 9(10).
      *01  LNK-SRYKA            PIC X(02).
       01  LNK-TIME             PIC X(06).
      *01  LNK-HOSPID           PIC X(24).
      *01  LNK-SYSYMD           PIC 9(08).
      *01  LNK-DRCD             PIC X(05).
      *01  LNK-HKNCOMBI         PIC X(04).
       01  LNK-PARA-SINRYONAIYO PIC X(16).
       01  LNK-PARA-RENNUM      PIC X(02).
       01  LNK-PARA-MEMO        PIC X(02).
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       01  WRK-LF01-REC. 
           COPY    "CPRLF001.INC"    REPLACING  //LF01//
                                     BY         //WRK-LF01//.
      *
      ******************************************************************
      *
      *PROCEDURE                       DIVISION
      *                                USING   LNK-PTID
      *                                        LNK-SRYKA
      *                                        LNK-TIME
      *                                        LNK-HOSPID
      *                                        LNK-SYSYMD
      *                                        LNK-DRCD
      *                                        LNK-HKNCOMBI
      *                                        LNK-PARA-SINRYONAIYO
      ****                                     WRK-LF01-REC
       PROCEDURE                       DIVISION
                                       USING
           LNK-TIME
           LNK-PARA-SINRYONAIYO
           LNK-PARA-RENNUM
           LNK-PARA-MEMO
           SPA-AREA
           WRK-LF01-REC
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ¾®»ù²Ê³°Íè¿ÇÎÅ²ÊÊÔ½¸
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    "1"                 TO  SYS-1006-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1006-EDYUKYMD
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷¡Ê»ÜÀß´ð½à¡Ë
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 901-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                  SYS-1006-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  FLG-SYONIKA-ARI
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               IF      SYS-1006-SSTKIJUN (121) =   1
                   MOVE    1               TO  FLG-SYONIKA-ARI
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷¡Ê£Ã£Ì£Á£É£ÍÀÜÂ³¾ðÊó¡Ë
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                      SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9000-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  MCPDATA-REC
      *
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9000-REC
           ELSE
               INITIALIZE                      SYS-9000-REC
           END-IF
      *    ¥ì¥»ÅÅ¤Î²Ê¤ØÊÑ¹¹
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           IF     (SYS-9000-RECEDEN-FLG    =   "1" )  AND
                  (SPA-SRYKA           NOT =   "XX")
               PERFORM 1001-RECDEN-SRYKA-SEC
           END-IF
      *
      *    OPEN    I-O     LF03-FILE
      *
      *    INITIALIZE                  FLG-AREA
      *    MOVE    SPA-HOSPNUM     TO  LF03-HOSPNUM
      *    MOVE    SPA-PTID        TO  LF03-PTID
      *    MOVE    WRK-SRYKA       TO  LF03-SRYKA
      *
      *    DISPLAY "LD03 READ START WRK-SRYKA:"  WRK-SRYKA
      *
      *    PERFORM 900-LF03-READ-SEC
      *    IF      FLG-LF03        =   1
      *        DISPLAY "SPA-HOSPNUM INVALID"
      *        MOVE    SPACE           TO  LF03-REC
      *        INITIALIZE                  LF03-REC
      *        MOVE    SPA-HOSPNUM     TO  LF03-HOSPNUM
      *        MOVE    SPA-PTID        TO  LF03-PTID
      *        MOVE    WRK-SRYKA       TO  LF03-SRYKA
      *****END-IF
      *
      *    ¹¹¿·Æü¼èÆÀ
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    UID¸¡º÷
           INITIALIZE                      CLMUID-REC
           MOVE    SPA-HOSPNUM         TO  CLMUID-HOSPNUM
           MOVE    SPA-PTID            TO  CLMUID-PTID
           MOVE    WRK-SRYKA           TO  CLMUID-SRYKA
           PERFORM 903-CLMUID-READ-SEC
           IF      FLG-CLMUID          =   1
               INITIALIZE                      CLMUID-REC
               MOVE    SPA-HOSPNUM         TO  CLMUID-HOSPNUM
               MOVE    SPA-PTID            TO  CLMUID-PTID
               MOVE    WRK-SRYKA           TO  CLMUID-SRYKA
           END-IF
      *    Ê£¿ô¥Û¥¹¥È¤Î»þ¡¢UID¤ÏºÇ½é¤Î¤ßÀßÄê
           IF     (LNK-PARA-RENNUM     =   "01"  )
             OR   (FLG-CLMUID          =   1     )
               MOVE    ZERO                TO  FLG-UID
           ELSE
               MOVE    1                   TO  FLG-UID
           END-IF
      *    ¥á¥âÅÐÏ¿¤«¤é
           IF     LNK-PARA-MEMO        =   "@@"
               IF      FLG-CLMUID          =   ZERO
                   MOVE    1               TO  FLG-UID
               END-IF
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»ÅÅ¤Î²Ê¤ØÊÑ¹¹½èÍý
      *****************************************************************
       1001-RECDEN-SRYKA-SEC                SECTION.
      *
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *        ¥ì¥»ÅÅ¿ÇÎÅ²Ê
               IF      SYS-1005-RECESRYKA  =   SPACE
                   MOVE    "XX"                TO  WRK-SRYKA
               ELSE
                   MOVE    SYS-1005-RECESRYKA  TO  WRK-SRYKA
               END-IF
           END-IF
           .
       1001-RECDEN-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           DISPLAY "11 START"
           MOVE    SPACE           TO  WRK-LF01-REC
           INITIALIZE              WRK-LF01-REC
      *
      *    ´µ¼ÔÆÉ¤ß½Ð¤·
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SPA-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
      *    MOVE    SPA-PTID        TO  WRK-LF01-PTID1
      *                                WRK-LF01-PTID2
      *    ´µ¼ÔÈÖ¹æÊÑ´¹
           INITIALIZE                       ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    SPA-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           MOVE    SPTID-PTNUM     TO  WRK-LF01-PTID1
                                       WRK-LF01-PTID2
      *
           INITIALIZE              UIDIO-AREA
           PERFORM 500-UUID-SET-SEC
           DISPLAY "C-UID" C-UID 
           MOVE    C-UID           TO  WRK-LF01-UID1
                                       CLMUID-UID-NEW
           PERFORM 500-UUID-SET-SEC
           MOVE    C-UID           TO  WRK-LF01-UID3
      *
           MOVE    SPA-SYSYMD      TO  WRK-WYMD
           PERFORM 500-HIZUKE-HENSYU-SEC
           MOVE    WRK-HENYMD      TO  WRK-LF01-CONFIRMDATE1
      *                                WRK-LF01-CONFIRMDATE2
                                       WRK-LF01-CONFIRMDATE3
      *
           MOVE    WRK-HENYMD      TO  WRK-LF01-UKEYMD(1:10)
           MOVE    "T"             TO  WRK-LF01-UKEYMD(11:1)
           MOVE    LNK-TIME (1:2)  TO  WRK-LF01-UKEYMD(12:2)
           MOVE    ":"             TO  WRK-LF01-UKEYMD(14:1) 
           MOVE    LNK-TIME (3:2)  TO  WRK-LF01-UKEYMD(15:2)
           MOVE    ":"             TO  WRK-LF01-UKEYMD(17:1) 
           MOVE    LNK-TIME (5:2)  TO  WRK-LF01-UKEYMD(18:2)
      *
           MOVE    "facility"      TO  WRK-LF01-IDTYPE1
                                       WRK-LF01-IDTYPE2
      *
           IF      PTINF-KANANAME  NOT =   SPACE
               MOVE    "P"         TO  WRK-LF01-KANANAME-REPCODE
               MOVE    PTINF-KANANAME
                                   TO  WRK-LF01-KANANAME
           END-IF
      *
           MOVE    "I"             TO  WRK-LF01-NAME-REPCODE
      ***  MOVE    PTINF-NAME      TO  WRK-LF01-NAME
      *    ³ÈÄ¥´Á»úÊÑ´¹
           MOVE    PTINF-NAME      TO  WRK-KOUMOKU
           PERFORM 800-ORCSKANACHK-SEC
           MOVE    WRK-KOUMOKU     TO  WRK-LF01-NAME
      *
           MOVE    PTINF-BIRTHDAY  TO  WRK-WYMD
           PERFORM 500-HIZUKE-HENSYU-SEC
           MOVE    WRK-HENYMD      TO  WRK-LF01-BIRTHDAY
      *
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "male"      TO  WRK-LF01-SEX
               WHEN    "2"
                   MOVE    "female"    TO  WRK-LF01-SEX
               WHEN    " "
                   MOVE    "unknown"   TO  WRK-LF01-SEX
               WHEN    OTHER
                   MOVE    "other"     TO  WRK-LF01-SEX
           END-EVALUATE
      *
           IF      PTINF-HOME-POST     =   SPACE
           AND     PTINF-HOME-ADRS     =   SPACE
           AND     PTINF-HOME-BANTI    =   SPACE
               CONTINUE
           ELSE
               MOVE    PTINF-HOME-POST     TO  WRK-POST
               PERFORM 500-POST-HENSYU-SEC
               MOVE    WRK-HENPOST         TO  WRK-LF01-HOME-POST
               MOVE    "home"          TO  WRK-LF01-HOME-KREPCODE
               STRING  PTINF-HOME-ADRS     DELIMITED   BY  SPACE
                       "¡¡"                DELIMITED   BY  SIZE
                       PTINF-HOME-BANTI    DELIMITED   BY  SPACE
                                       INTO    WRK-LF01-HOME-ADRS
               END-STRING
           END-IF 
      *
      *
           IF      PTINF-RENRAKU-POST     =   SPACE
           AND     PTINF-RENRAKU-ADRS     =   SPACE
           AND     PTINF-RENRAKU-BANTI    =   SPACE
               CONTINUE
           ELSE
             MOVE    PTINF-RENRAKU-POST     TO  WRK-POST
             PERFORM 500-POST-HENSYU-SEC
             MOVE    WRK-HENPOST         TO  WRK-LF01-RENRAKU-POST
             MOVE    "home"          TO  WRK-LF01-RENRAKU-KREPCODE
             STRING  PTINF-RENRAKU-ADRS  DELIMITED   BY  SPACE
                     "¡¡"                DELIMITED   BY  SIZE
                     PTINF-RENRAKU-BANTI DELIMITED   BY  SPACE
                                     INTO    WRK-LF01-RENRAKU-ADRS
             END-STRING
           END-IF 
      *
           IF       PTINF-HOME-TEL1      NOT =  SPACE
               MOVE     "?????"          TO  WRK-LF01-PHONE-AREA
               MOVE     "?????"          TO  WRK-LF01-PHONE-CITY
               MOVE     "?????"          TO  WRK-LF01-PHONE-NUM
               MOVE     PTINF-HOME-TEL1  TO  WRK-LF01-PHONE-MEMO
           END-IF
      *
           IF      PTINF-OFFICE-POST     =   SPACE
           AND     PTINF-OFFICE-ADRS     =   SPACE
           AND     PTINF-OFFICE-BANTI    =   SPACE
               CONTINUE
           ELSE
             MOVE    PTINF-OFFICE-POST     TO  WRK-POST
             PERFORM 500-POST-HENSYU-SEC
             MOVE    WRK-HENPOST         TO  WRK-LF01-OFFICE-POST
             MOVE    "business"          TO  WRK-LF01-OFFICE-KREPCODE
             STRING  PTINF-OFFICE-ADRS   DELIMITED   BY  SPACE
                     "¡¡"                DELIMITED   BY  SIZE
                     PTINF-OFFICE-BANTI  DELIMITED   BY  SPACE
                                     INTO    WRK-LF01-OFFICE-ADRS
             END-STRING
             END-IF 
      *
      *
           IF      PTINF-KISEI-POST     =   SPACE
           AND     PTINF-KISEI-ADRS     =   SPACE
           AND     PTINF-KISEI-BANTI    =   SPACE
               CONTINUE
           ELSE
             MOVE    PTINF-KISEI-POST     TO  WRK-POST
             PERFORM 500-POST-HENSYU-SEC
             MOVE    WRK-HENPOST         TO  WRK-LF01-KISEI-POST
             MOVE    "home"          TO  WRK-LF01-KISEI-KREPCODE
             STRING  PTINF-KISEI-ADRS    DELIMITED   BY  SPACE
                     "¡¡"                DELIMITED   BY  SIZE
                     PTINF-KISEI-BANTI   DELIMITED   BY  SPACE
                                     INTO    WRK-LF01-KISEI-ADRS
             END-STRING
           END-IF
      *                
           IF      PTINF-DEATHKBN  =   "1"
               MOVE    "true"          TO  WRK-LF01-DEATHKBN
           ELSE
               MOVE    "false"         TO  WRK-LF01-DEATHKBN
           END-IF
      *
           MOVE    "regist"        TO  WRK-LF01-UKEYMD-KREPCODE
      *    ´µ¼ÔÅÐÏ¿¤«¤é
           IF      WRK-SRYKA       =   "XX"
               MOVE    "info"          TO  WRK-LF01-UKEYMD-KREPCODE
      *        ¿ÇÎÅ²ÊÀßÄê
               PERFORM 2009-SRYKA-SET-SEC
           END-IF
      *    ¥á¥â¤«¤é
           IF      LNK-PARA-MEMO       =   "@@"
               MOVE    "memo"          TO  WRK-LF01-UKEYMD-KREPCODE
           END-IF
      *
           MOVE    "false"         TO  WRK-LF01-NYUGAIKBN
      *
      *
      *    ¿ÇÎÅ²ÊÌ¾¾Î
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           IF      SPA-SRYKA           =   "XX"
               MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           ELSE
               MOVE    SPA-SRYKA           TO  SYS-1005-KBNCD
           END-IF
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME  TO  WRK-LF01-SRYNAME
                                           WRK-LF01-CRE001-DPERTNAME
                                           WRK-LF01-CRE301-DPERTNAME
               MOVE    WRK-SRYKA           TO  WRK-LF01-CRE001-DPERTID
                                           WRK-LF01-CRE301-DPERTID
               MOVE    "I"                 TO  WRK-LF01-SRYREP
           END-IF
      *
           MOVE    SPACE               TO  WRK-C-UID
      *
      *    ÊÝ¸±¾ðÊó  
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    SPA-PTID        TO  COMB-PTID
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-COMB-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-HZN
           MOVE    SPACE               TO   WRK-FTNRATEMEI
           PERFORM UNTIL       (IDX            =   20   )  OR
                               (FLG-HKNCOMBI   =   1    )
           DISPLAY "ÊÝ¸±¾ðÊó START"
112500         IF     (SPA-SYSYMD            >=  COMB-TEKSTYMD)  AND
112800                (SPA-SYSYMD            <=  COMB-TEKEDYMD)  AND
                      (COMB-DELKBN           =   SPACE        )
                   ADD     1                 TO  IDX
      *
      *            ÉéÃ´Î¨·×»»
                   INITIALIZE                  ORCSAGECHKAREA
                   MOVE    COMB-HOSPNUM        TO  AGECHK-HOSPNUM
                   MOVE    COMB-PTID           TO  AGECHK-PTID
                   MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
                   MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
                   MOVE    COMB-HKNCOMBINUM    TO  AGECHK-HKNCOMBINUM
                   CALL    "ORCSAGECHK"        USING
                                               ORCSAGECHKAREA
                                               SPA-AREA
      *            ÉéÃ´Î¨
                   IF      AGECHK-RC           =   ZERO
                       COMPUTE WRK-FTNRATE         =   AGECHK-FTNRATE
                                                   /   100
                   ELSE
                       COMPUTE WRK-FTNRATE         =   COMB-GAI-KFTNRATE
                                                   /   100
                   END-IF
                   MOVE    WRK-FTNRATE         TO   WRK-Z9
                   MOVE    WRK-Z9              TO   WRK-FTNRATEMEI
      *
      *   ¼çÊÝ¸±¾ðÊóÆÉ¤ß½Ð¤·
                   IF      COMB-HKNID          NOT =   ZERO
      *                ÉéÃ´Î¨
                       MOVE    WRK-FTNRATEMEI      TO
                                               WRK-LF01-OUTRATIO (IDX)
      *                £±²óÌÜ
                       IF      FLG-UID             =   ZERO
                           MOVE    COMB-HKNCOMBINUM    TO
                                               CLMUID-HKNCOMBI (IDX)
      *
                           INITIALIZE              UIDIO-AREA
                           PERFORM 500-UUID-SET-SEC
                           MOVE    C-UID               TO
                                               WRK-LF01-UID2    (IDX)
                                               CLMUID-UID       (IDX)
                       ELSE
      *                £²²óÌÜ¤«¤é
                           PERFORM 2004-UID-TBL-SEC
                       END-IF
      *
                       IF      SPA-HKNCOMBINUM     =   COMB-HKNCOMBINUM
                           MOVE    C-UID           TO  WRK-C-UID
                           MOVE    IDX             TO  IDX-HZN
                       END-IF
      *
                       MOVE    SPA-SYSYMD          TO  WRK-WYMD
                       PERFORM 500-HIZUKE-HENSYU-SEC
                       MOVE    WRK-HENYMD          TO  
                               WRK-LF01-CONFIRMDATE2(IDX)
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
                       MOVE    SPA-PTID            TO  PTHKN-PTID
                       MOVE    COMB-HKNID          TO  PTHKN-HKNID
                       MOVE    SPA-SYSYMD          TO  PTHKN-TEKSTYMD
                       MOVE    SPA-SYSYMD          TO  PTHKN-TEKEDYMD
                       MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                       PERFORM 910-PTHKNINF-INV-SEC
                       MOVE    PTHKN-HKNJANUM  
                               TO  WRK-LF01-HKNJANUM (IDX)
                       IF      PTHKN-KIGO          =   SPACE
                           MOVE    "µ­ºÜ¤Ê¤·"     
                               TO  WRK-LF01-KIGO     (IDX)
                       ELSE
                           MOVE    PTHKN-KIGO      
                               TO  WRK-LF01-KIGO     (IDX)
                       END-IF
                       IF      PTHKN-NUM           =   SPACE
                           MOVE    "µ­ºÜ¤Ê¤·"     
                               TO  WRK-LF01-NUM      (IDX)
                       ELSE
                           MOVE    PTHKN-NUM       
                               TO  WRK-LF01-NUM      (IDX)
                       END-IF
                       IF      PTHKN-HONKZKKBN =   "1"
                           MOVE    "true"      
                               TO  WRK-LF01-HONKZKKBN (IDX)
                       ELSE
                           MOVE    "false"     
                               TO  WRK-LF01-HONKZKKBN (IDX)
                       END-IF
                       MOVE    PTHKN-TEKSTYMD  TO  WRK-WYMD
                       PERFORM 500-HIZUKE-HENSYU-SEC
                       MOVE    WRK-HENYMD      
                               TO  WRK-LF01-TEKSTYMD (IDX)
                       MOVE    PTHKN-TEKEDYMD  TO  WRK-WYMD
                       PERFORM 500-HIZUKE-HENSYU-SEC
                       MOVE    WRK-HENYMD      
                               TO  WRK-LF01-TEKEDYMD (IDX)
                       PERFORM VARYING IDW FROM    1   BY  1
                               UNTIL   IDW >       MML0031-TMAX
                         IF    PTHKN-HKNNUM  =  MML0031-HOKEN (IDW)
                               MOVE    MML0031-NAME(IDW) 
                                       TO  WRK-LF01-HKNSBT  (IDX)
      *                        ÊÝ¸±¼ïÊÌ
      *                        PERFORM 2001-HKNNUM-SET-SEC
                               MOVE    99        TO  IDW
                         END-IF
                      END-PERFORM
      *                 
                      PERFORM 2001-HKNNUM-SET-SEC
                   ELSE
      *                ¼çÊÝ¸±¤Ê¤·
      *                £±²óÌÜ
                       IF      FLG-UID             =   ZERO
                           MOVE    COMB-HKNCOMBINUM    TO
                                               CLMUID-HKNCOMBI (IDX)
      *                 
                           INITIALIZE              UIDIO-AREA
                           PERFORM 500-UUID-SET-SEC
                           MOVE    C-UID               TO
                                               WRK-LF01-UID2    (IDX)
                                               CLMUID-UID       (IDX)
                       ELSE
      *                £²²óÌÜ¤«¤é
                           PERFORM 2004-UID-TBL-SEC
                       END-IF
      *
                       IF      SPA-HKNCOMBINUM     =   COMB-HKNCOMBINUM
                           MOVE    C-UID           TO  WRK-C-UID
                           MOVE    IDX             TO  IDX-HZN
                       END-IF
      *
                       MOVE    SPA-SYSYMD          TO  WRK-WYMD
                       PERFORM 500-HIZUKE-HENSYU-SEC
                       MOVE    WRK-HENYMD          TO  
                               WRK-LF01-CONFIRMDATE2(IDX)
                       MOVE    "9999" 
                                               TO  WRK-LF01-HKNJANUM
                                                                (IDX)
                       MOVE    "¸øÈñÃ±ÆÈ"      TO  WRK-LF01-HKNSBT
                                                                (IDX)
                       MOVE    "XX"            TO  WRK-LF01-HKNSBTCODE
                                                                 (IDX)
                       MOVE    "µ­ºÜ¤Ê¤·"      TO  WRK-LF01-KIGO (IDX)
                       MOVE    "µ­ºÜ¤Ê¤·"      TO  WRK-LF01-NUM  (IDX)
                       MOVE    "true"          TO  WRK-LF01-HONKZKKBN
                                                                 (IDX)
                       MOVE    COMB-TEKSTYMD   TO  WRK-WYMD
                       PERFORM 500-HIZUKE-HENSYU-SEC
                       MOVE    WRK-HENYMD      TO  WRK-LF01-TEKSTYMD
                                                               (IDX)
                       MOVE    COMB-TEKEDYMD   TO  WRK-WYMD
                       PERFORM 500-HIZUKE-HENSYU-SEC
                       MOVE    WRK-HENYMD      TO  WRK-LF01-TEKEDYMD
                                                               (IDX)
                   END-IF
      *
      *   ¸øÈñÆÉ¤ß½Ð¤·
                   IF      COMB-KOHHKNNUM (1)  NOT =   SPACE
                       PERFORM 20012-KOHID-HENSYU-SEC
                       PERFORM VARYING IDY1 FROM    1   BY  1
                               UNTIL   IDY1 >       4
                               OR      WRK-KOHID(IDY1)    =   ZERO
                           INITIALIZE              PTKOHINF-REC
                           MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
                           MOVE    SPA-PTID        TO  PTKOH-PTID
                           MOVE    WRK-KOHID(IDY1) TO  PTKOH-KOHID
                           MOVE    SPA-SYSYMD      TO  PTKOH-TEKSTYMD
                           MOVE    SPA-SYSYMD      TO  PTKOH-TEKEDYMD
                           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
                           PERFORM 910-PTKOHINF-INV-SEC
                           MOVE    IDY1            TO
                                           WRK-LF01-PRIORITY (IDX IDY1)
                     IF    PTKOH-FTNJANUM          =   SPACE
                           MOVE    "mikinyu"       TO
                                            WRK-LF01-FTNJANUM(IDX IDY1)
                     ELSE
                           MOVE    PTKOH-FTNJANUM  TO
                                            WRK-LF01-FTNJANUM(IDX IDY1)
                     END-IF
                     IF    PTKOH-JKYSNUM           =   SPACE
                           MOVE    "mikinyu"       TO
                                            WRK-LF01-JKYSNUM (IDX IDY1)
                     ELSE
                           MOVE    PTKOH-JKYSNUM   TO
                                            WRK-LF01-JKYSNUM (IDX IDY1)
                     END-IF
                           MOVE    PTKOH-TEKSTYMD  TO  WRK-WYMD
                           PERFORM 500-HIZUKE-HENSYU-SEC
                           MOVE    WRK-HENYMD      TO
                                        WRK-LF01-TTEKSTYMD (IDX IDY1)
                           MOVE    PTKOH-TEKEDYMD  TO  WRK-WYMD
                           PERFORM 500-HIZUKE-HENSYU-SEC
                           MOVE    WRK-HENYMD      TO
                                        WRK-LF01-TTEKEDYMD (IDX IDY1)
      *??
      *                    ÉéÃ´Î¨¡¦ÉéÃ´³ÛÊÔ½¸
                           PERFORM 400-KOHFTN-HENSYU-SEC
      *??
      *                    ¸øÈñÌ¾¾Î
                           MOVE    WRK-KOHMEI (IDY1)   TO
                                           WRK-LF01-KOHINAME (IDX IDY1)
      *
                       END-PERFORM    
                   END-IF
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-COMB-READ-SEC
           END-PERFORM
      *    ¥«¡¼¥½¥ë¥¯¥í¡¼¥¹
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    »ÈÍÑÊÝ¸± (claim:insuranceUid)
           MOVE    WRK-C-UID           TO  WRK-LF01-FIL1
           IF      IDX-HZN             >   ZERO
               PERFORM 2002-HKNCOMBI-LIST-SEC
           END-IF
      *
      *    ½é¿Ç¡¿ºÆ¿Ç
           MOVE    SPACE               TO  WRK-SYOYMD
           MOVE    ZERO                TO  WRK-SYOSIN 
           MOVE    SPACE               TO  WRK-LSTYMD
           MOVE    SPACE               TO  WRK-LSTSRYKA
      *
      *    ¿ÇÎÅ²ÊÎò¤ò¸¡º÷
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                  SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                  SRYKARRK-REC
           END-IF
           PERFORM UNTIL       FLG-SRYKARRK    =   1
      *
      *        ½é¿Ç»»ÄêÆü
               IF     (KARRK-SYOSINYMD2 NOT =  SPACE      ) AND
                      (KARRK-SYOSINYMD2    >=  WRK-SYOYMD )
                   MOVE    KARRK-SYOSINYMD2    TO  WRK-SYOYMD
               END-IF
      *        ºÇ½ª¼õ¿ÇÆü
               IF     (KARRK-LASTYMD   NOT =   SPACE      ) AND
                      (KARRK-LASTYMD       >=  WRK-LSTYMD )
                   MOVE    KARRK-LASTYMD       TO  WRK-LSTYMD
                   MOVE    KARRK-SRYKA         TO  WRK-LSTSRYKA
               END-IF
      *
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ½é¿ÇÎÁ»»ÄêÍ­Ìµ
           IF      WRK-SYOYMD          =   SPACE
      *            ½é¿Ç»»Äê
               MOVE    1               TO  WRK-SYOSIN
           END-IF
      *
      *    ½é¿ÇÎÁ»»ÄêÆü¤¬¿ÇÎÅÆü¤Î»þ¡¢ºÆ¿Ç¤È¤¹¤ë
           IF      WRK-SYOSIN          =   1
               IF      WRK-SYOYMD          =   SPA-SYSYMD
                   MOVE    ZERO            TO  WRK-SYOSIN
               END-IF
           END-IF
      *
           IF      WRK-SYOSIN         =   ZERO
      *
               MOVE    WRK-SYOYMD      TO  WRK-WYMD
               PERFORM 500-HIZUKE-HENSYU-SEC
               MOVE    WRK-HENYMD      TO  WRK-LF01-CONFIRMDATE4
           END-IF
      *
           MOVE    "tbl_tensu"         TO  WRK-LF01-ID
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý½êºßÃÏ¡¢Ï¢ÍíÀè
           MOVE    SPACE               TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1002-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1002-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *    °åÎÅµ¡´Ø£É£ÄÊÔ½¸
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
      ******** EVALUATE    SYS-1001-HOSPSBT    ALSO    WRK-SYOSIN
      *            WHEN    "1"                 ALSO    1
      *                MOVE    "111000110"     TO  WRK-LF01-IJICD
      *                MOVE    "110"           TO
      *                        WRK-LF01-BUNDLE-CLASSCODE    
      *            WHEN    "1"                 ALSO    ZERO
      *                MOVE    "112000110"     TO  WRK-LF01-IJICD    
      *                MOVE    "120"           TO
      *                        WRK-LF01-BUNDLE-CLASSCODE    
      *            WHEN    "2"                 ALSO    1
      *                MOVE    "111003610"     TO  WRK-LF01-IJICD    
      *                MOVE    "110"           TO
      *                        WRK-LF01-BUNDLE-CLASSCODE    
      *            WHEN    "2"                 ALSO    ZERO
      *                MOVE    "112000210"     TO  WRK-LF01-IJICD
      *                MOVE    "120"           TO
      *                        WRK-LF01-BUNDLE-CLASSCODE    
      *            WHEN    OTHER
      *                DISPLAY "*** ORCL0011 IJICD ERR HOSPSBT="
      *                        SYS-1001-HOSPSBT " SYOSIN=" WRK-SYOSIN 
      *********END-EVALUATE
      * ºîÀ®¼Ô »ÜÀß¾ðÊóÌ¾
      *        ³ÈÄ¥´Á»úÊÑ´¹
               MOVE    SYS-1001-HOSPNAME    TO  WRK-KOUMOKU
               PERFORM 800-ORCSKANACHK-SEC
               MOVE    WRK-KOUMOKU          TO  SYS-1001-HOSPNAME
      *
               MOVE  SYS-1001-HOSPNAME      TO  
                     WRK-LF01-CRE001-FACILITYNAME
                     WRK-LF01-CRE101-FACILITYNAME
                     WRK-LF01-CRE301-FACILITYNAME
      *´ÉÍý¼ÔÌ¾
      *        ³ÈÄ¥´Á»úÊÑ´¹
               MOVE    SYS-1001-KANRINAME   TO  WRK-KOUMOKU
               PERFORM 800-ORCSKANACHK-SEC
               MOVE    WRK-KOUMOKU          TO  SYS-1001-KANRINAME
      *
               MOVE  SYS-1001-KANRINAME     TO  WRK-LF01-CRE001-FULLNAME
                                                WRK-LF01-CRE101-FULLNAME
      *                                         WRK-LF01-CRE201-FULLNAME
                                                WRK-LF01-CRE301-FULLNAME
      *°åÎÅµ¡´Ø¥³¡¼¥É
               MOVE  SYS-1001-HOSPCD     TO  WRK-LF01-CRE001-PERSONALID
                                             WRK-LF01-CRE101-PERSONALID
                                             WRK-LF01-CRE301-PERSONALID
               MOVE  "doctor"               TO  WRK-LF01-CRE001-LICENSE
                                                WRK-LF01-CRE101-LICENSE
                                                WRK-LF01-CRE301-LICENSE
               MOVE  SYS-1001-HOSPID     TO  WRK-LF01-CRE001-FACILITYID
                                             WRK-LF01-CRE101-FACILITYID
                                             WRK-LF01-CRE301-FACILITYID
      *Í¹ÊØÈÖ¹æ
               MOVE  SYS-1002-POST(1:3)     TO  WRK-LF01-CRE001-ZIP(1:3)
                                                WRK-LF01-CRE101-ZIP(1:3)
                                                WRK-LF01-CRE301-ZIP(1:3)
               MOVE  "-"                    TO  WRK-LF01-CRE001-ZIP(4:1)
                                                WRK-LF01-CRE101-ZIP(4:1)
                                                WRK-LF01-CRE301-ZIP(4:1)
               MOVE  SYS-1002-POST(4:4)     TO  WRK-LF01-CRE001-ZIP(5:4)
                                                WRK-LF01-CRE101-ZIP(5:4)
                                                WRK-LF01-CRE301-ZIP(5:4)
      *½»½ê
               MOVE  SYS-1002-ADRS       TO  WRK-LF01-CRE001-ADRFULLNAME
                                             WRK-LF01-CRE101-ADRFULLNAME
                                             WRK-LF01-CRE301-ADRFULLNAME
      *        PERFORM UNTIL       (IDX            =   20   )
               PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       20
                 IF    WRK-LF01-HONKZKKBN(IDX) NOT =   SPACE 
                       MOVE  "MML0031"      
                             TO  WRK-LF01-HKNSBTTBL(IDX)
                       MOVE  SYS-1001-HOSPNAME      
                             TO  WRK-LF01-CRE201-FACILITYNAME(IDX)
                       MOVE  SYS-1001-KANRINAME 
                             TO  WRK-LF01-CRE201-FULLNAME(IDX)
                       MOVE  SYS-1001-HOSPCD        
                             TO  WRK-LF01-CRE201-PERSONALID(IDX)
                       MOVE  "doctor"               
                             TO  WRK-LF01-CRE201-LICENSE(IDX)
                       MOVE  SYS-1001-HOSPID         
                             TO  WRK-LF01-CRE201-FACILITYID(IDX)
                       MOVE  SYS-1005-SRYKANAME  
                             TO  WRK-LF01-CRE201-DPERTNAME(IDX)
                       MOVE  WRK-SRYKA           
                             TO  WRK-LF01-CRE201-DPERTID(IDX)
                       MOVE  SYS-1002-POST(1:3) 
                                            TO  WRK-LF01-CRE201-ZIP(IDX)
                                                                 (1:3)
                       MOVE  "-"            
                                            TO  WRK-LF01-CRE201-ZIP(IDX)
                                                                  (4:1)
                       MOVE  SYS-1002-POST(4:4)     
                                            TO  WRK-LF01-CRE201-ZIP(IDX)
                                                                   (5:4)
                       MOVE  SYS-1002-ADRS  
                                            TO  
                             WRK-LF01-CRE201-ADRFULLNAME(IDX)
                  END-IF
               END-PERFORM
           END-IF     
      *
      *    ¥É¥¯¥¿¡¼
           IF      SPA-DRCD            =   "@@@@@"
               CONTINUE
           ELSE
               PERFORM 2001-DRCD-NAME-SEC
               MOVE    SYS-1010-NAME   TO  WRK-LF01-CRE301-FULLNAME
               MOVE    SPA-DRCD        TO  WRK-LF01-CRE301-PERSONALID
           END-IF
      *    ¿Ç»¡ÎÁ¥³¡¼¥É¤ò·èÄê¤¹¤ë
           PERFORM 2020-SRYCD-SET-SEC
      *
           IF      WRK-LF01-IJICD       NOT =   SPACE
               INITIALIZE                       TNS-TENSU-REC
               MOVE    WRK-LF01-IJICD       TO  TNS-SRYCD
               PERFORM 900-TENSU-INV-SEC
               IF       FLG-TENSU       =       ZERO
                   MOVE    TNS-NAME        TO  WRK-LF01-HOSPSBT
      *            ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ¤ÇÆ±ÆüºÆ¿Ç¤Î»þ
                   IF     (WRK-LF01-IJICD(1:3) =   "113") AND
                          (WRK-LSTYMD          =   SPA-SYSYMD)
                       MOVE    "¾®»ù²Ê³°Íè¿Ç»¡ÎÁ¡¦Æ±ÆüºÆ¿Ç"
                                               TO  WRK-LF01-HOSPSBT
                   END-IF
               ELSE
                   DISPLAY "*** ORCL0011 TEN INVALID KEY=" 
                           WRK-LF01-IJICD 
               END-IF
           END-IF
      *
      *    ¿Ç»¡ÎÁ¥³¡¼¥É¤ò·èÄê¤¹¤ë
           MOVE    LNK-PARA-SINRYONAIYO
                                       TO  WRK-LF01-YYKMEMO
      *
      *    ¼õÉÕ¥á¥âÊÔ½¸
           PERFORM 2009-PTMEMOINF-SEC
      *
           MOVE    1                  TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    £²²óÌÜ¤«¤é¤ÎUIDÀßÄê½èÍý
      *****************************************************************
       2004-UID-TBL-SEC              SECTION.
      *
           PERFORM VARYING     IDX-U   FROM    1   BY  1
                   UNTIL       IDX-U   >   20
               IF      CLMUID-HKNCOMBI (IDX-U) =   COMB-HKNCOMBINUM
                   MOVE    CLMUID-UID  (IDX-U) TO  C-UID
                   MOVE    CLMUID-UID  (IDX-U) TO  WRK-LF01-UID2 (IDX)
                   MOVE    20                  TO  IDX-U
               END-IF
           END-PERFORM
           .
       2004-UID-TBL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¥É¥¯¥¿¡¼ÀßÄê½èÍý
      *****************************************************************
       2001-DRCD-NAME-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
      *
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
           ELSE
               INITIALIZE                      SYS-1010-REC
           END-IF
      *
      *    ³ÈÄ¥´Á»úÊÑ´¹
           MOVE    SYS-1010-NAME       TO  WRK-KOUMOKU
           PERFORM 800-ORCSKANACHK-SEC
           MOVE    WRK-KOUMOKU         TO  SYS-1010-NAME
           .
       2001-DRCD-NAME-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¿ÇÎÅ²ÊÀßÄê½èÍý
      *****************************************************************
       2009-SRYKA-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYKA
      *
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
                   MOVE    SYS-1005-KBNCD      TO  WRK-SRYKA
               ELSE
                   INITIALIZE                      SYS-1005-REC
               END-IF
           ELSE
               INITIALIZE                      SYS-1005-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2009-SRYKA-SET-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ÊÝ¸±¼ïÊÌÊÔ½¸½èÍý
      *****************************************************************
       2001-HKNNUM-SET-SEC              SECTION.
      *
           EVALUATE    PTHKN-HKNNUM 
               WHEN    "060"
      *            ¹ñÊÝ¤Î¾ì¹ç¤Ï¡¢¥¼¥í¤òÀßÄê
                   MOVE    "00"    TO  WRK-LF01-HKNSBTCODE (IDX)
               WHEN    "971"
      *            Ï«ºÒ
                   MOVE    "R1"    TO  WRK-LF01-HKNSBTCODE (IDX)
                   MOVE    "9999"  TO  WRK-LF01-HKNJANUM (IDX)
                   MOVE    "Ï«ºÒ"  TO  WRK-LF01-HKNSBT  (IDX)
               WHEN    "973"
      *            ¼«ÇåÀÕ
                   MOVE    "R3"    TO  WRK-LF01-HKNSBTCODE (IDX)
                   MOVE    "9999"  TO  WRK-LF01-HKNJANUM (IDX)
                   MOVE    "¼«ÇåÀÕ"    TO  WRK-LF01-HKNSBT  (IDX)
               WHEN    "975"
      *            ¸ø³²
                   MOVE    "K5"    TO  WRK-LF01-HKNSBTCODE (IDX)
                   MOVE    "9999"  TO  WRK-LF01-HKNJANUM (IDX)
                   MOVE    "¸ø³²"  TO  WRK-LF01-HKNSBT  (IDX)
               WHEN    OTHER
                   EVALUATE    PTHKN-HKNNUM(1:2)
                   WHEN    "98"
      *                ¼«Èñ
                       MOVE    "Z"         TO  WRK-LF01-HKNSBTCODE
                                                            (IDX)(1:1)
                       MOVE    PTHKN-HKNNUM(3:1)
                                           TO  WRK-LF01-HKNSBTCODE
                                                            (IDX)(2:1)
                       MOVE    "9999" 
                                           TO  WRK-LF01-HKNJANUM (IDX)
                       MOVE    "¼«Èñ" 
                                           TO  WRK-LF01-HKNSBT  (IDX)
                   WHEN    "90"
      *                ¼£¸³
                       MOVE    "A"         TO  WRK-LF01-HKNSBTCODE
                                                            (IDX)(1:1)
                       MOVE    PTHKN-HKNNUM(3:1)
                                           TO  WRK-LF01-HKNSBTCODE
                                                            (IDX)(2:1)
                       MOVE    "9999" 
                                           TO  WRK-LF01-HKNJANUM (IDX)
                       MOVE    "¼£¸³" 
                                           TO  WRK-LF01-HKNSBT  (IDX)
                   WHEN    "91"
      *                ¼£¸³
                       MOVE    "B"         TO  WRK-LF01-HKNSBTCODE
                                                            (IDX)(1:1)
                       MOVE    PTHKN-HKNNUM(3:1)
                                           TO  WRK-LF01-HKNSBTCODE
                                                            (IDX)(2:1)
                       MOVE    "9999" 
                                           TO  WRK-LF01-HKNJANUM (IDX)
                       MOVE    "¼£¸³" 
                                           TO  WRK-LF01-HKNSBT  (IDX)
                   WHEN    OTHER
      *                ¤½¤ÎÂ¾¡Ê²¼£²·å¡Ë
                       MOVE    PTHKN-HKNNUM    TO  WRK-999
                       MOVE    WRK-999         TO  WRK-ZZZ
                       MOVE    WRK-ZZZ(2:2)    TO  WRK-LF01-HKNSBTCODE
                                                                (IDX)
                   END-EVALUATE
           END-EVALUATE
      *
           .
       2001-HKNNUM-SET-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¸øÈñ¸ÄÊÌ½èÍý
      *****************************************************************
       20012-KOHID-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      WRK-KOHI-AREA
                                           WRK-KOHI-AREA-X
           MOVE    ZERO                    TO  TBL-MAX
      *
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
               MOVE    COMB-KOHID    (IDZ) TO  WRK-KOHID-O     (IDZ)
               MOVE    COMB-KOHHKNNUM(IDZ) TO  WRK-KOHHKNNUM-O (IDZ)
      *        ¸øÈñÌ¾¾Î
               MOVE    COMB-KOH-TANSEIDONAME(IDZ)
                                           TO  WRK-KOHMEI-O    (IDZ)
      *        ¸øÈñ¡Ý³°Íè¡Ý¾å¸Â¶èÊ¬
               MOVE    COMB-KOH-GAIJGNKBN (IDZ)
                                           TO  WRK-GAIJGNKBN-O (IDZ)
           END-PERFORM
      *
      *H26.11  ÊÂ¤ÓÂØ¤¨¤òÇÑ»ß¤¹¤ë
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ         >   4 )
               IF      WRK-KOHID-O (IDZ)   >   ZERO
                   ADD     1                   TO  TBL-MAX
                   MOVE    WRK-KOHI-TBL (IDZ)  TO  WRK-KOHI-TBL-X
                                                           (TBL-MAX)
               END-IF
           END-PERFORM
      *****
           GO  TO  20012-KOHID-HENSYU-EXT
      *
      *****
      *
      *    Í¥Àè½ç°Ì¤ËÊÂ¤ÙÂØ¤¨
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       WRK-SRT-KOHHKNNUM-MAX
                   OR      TBL-MAX     =   4 
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       4
                   IF      WRK-SRT-KOHHKNNUM   (IDZ)
                                   =   WRK-KOHHKNNUM-O (IDY)
                       ADD     1               TO  TBL-MAX
                       MOVE    WRK-KOHI-TBL    (IDY)   TO
                                           WRK-KOHI-TBL-X (TBL-MAX)
                       INITIALIZE          WRK-KOHI-TBL   (IDY)
                       MOVE    4               TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    ¤¹¤Ù¤ÆÊÂ¤ÙÂØ¤¨¤¿»þ 
           IF      TBL-MAX                 =   4
               GO  TO  20012-KOHID-HENSYU-EXT
           END-IF
      *
      *    ÃÏÊý¸øÈñ¤ÎÊÂ¤ÓÂØ¤¨
           PERFORM VARYING IDZ FROM    1    BY  1
                   UNTIL   IDZ >       3
               COMPUTE IDX1    =   IDZ     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       4
                   IF      WRK-KOHHKNNUM-O (IDZ) >   
                                           WRK-KOHHKNNUM-O (IDY)
                   OR      WRK-KOHHKNNUM-O (IDZ) =   SPACE 
                       MOVE    WRK-KOHI-TBL    (IDZ) TO  
                                           WRK-KOHI
                       MOVE    WRK-KOHI-TBL    (IDY) TO  
                                           WRK-KOHI-TBL(IDZ)
                       MOVE    WRK-KOHI              TO  
                                           WRK-KOHI-TBL(IDY)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    ÃÏÊý¸øÈñ¤ÎÄÉ²Ã
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ         >   4
                   OR      TBL-MAX     =   4 
               IF      WRK-KOHID-O (IDZ)   >   ZERO
                   ADD     1               TO  TBL-MAX
                   MOVE    WRK-KOHI-TBL (IDZ)
                           TO  WRK-KOHI-TBL-X (TBL-MAX)
               END-IF    
           END-PERFORM
076800     .
076900 20012-KOHID-HENSYU-EXT.
077000     EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥ê¥¹¥È½èÍý
      *****************************************************************
       2002-HKNCOMBI-LIST-SEC        SECTION.
      *
           INITIALIZE                  HZN-LF01-REC
           MOVE    1                   TO  IDX
           MOVE    WRK-LF01-TBL(IDX-HZN)   TO  HZN-LF01-TBL (1)
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   20  )  OR
                              (IDX     >=  20  )
               IF      IDX-HZN         NOT =   IDY
                   ADD     1               TO  IDX
                   MOVE    WRK-LF01-TBL(IDY)   TO  HZN-LF01-TBL (IDX)
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               MOVE    HZN-LF01-TBL(IDX)   TO  WRK-LF01-TBL (IDX)
           END-PERFORM
      *
           .
       2002-HKNCOMBI-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉéÃ´Î¨¡¦ÉéÃ´³ÛÊÔ½¸½èÍý
      *****************************************************************
       400-KOHFTN-HENSYU-SEC        SECTION.
      *
      *
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
      *    ÊÝ¸±ÈÖ¹æ¡Ê¸øÈñ¡Ë
           MOVE    PTKOH-KOHNUM        TO  HKN-HKNNUM
      *    »ÙÊ§¶èÊ¬
           MOVE    PTKOH-PAYKBN        TO  HKN-PAYKBN
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
           IF      WRK-GAIJGNKBN (IDY1)    =   SPACE
      *        ËÜ¿Í¡Ý³°Íè¡Ý²ó¡ÝÉéÃ´³ä¹ç
               COMPUTE WRK-FTNRATE         =   HKN-HON-GAIKFTNRATE
                                           /   100
               MOVE    WRK-FTNRATE         TO  WRK-Z9
               MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
               MOVE    WRK-FTNRATEMEI      TO  WRK-LF01-RATIO (IDX IDY1)
               MOVE    "ratio"             TO  WRK-LF01-RATIO-CODE
                                                              (IDX IDY1)
           ELSE
      *        ¾å¸Â³Û
               IF      HKN-HON-GAIKKOTGAK  =   ZERO
      *            ËÜ¿Í¡Ý³°Íè¡ÝÆü¡Ý¾å¸Â³Û
                   MOVE    HKN-HON-GAIDJGNGAK  TO  WRK-ZZ9
               ELSE
      *            ËÜ¿Í¡Ý³°Íè¡Ý²ó¡Ý¾å¸Â³Û
                   MOVE    HKN-HON-GAIKKOTGAK  TO  WRK-ZZ9
               END-IF
               MOVE    WRK-ZZ9             TO  WRK-LF01-RATIO (IDX IDY1)
               MOVE    "fix"               TO  WRK-LF01-RATIO-CODE
                                                              (IDX IDY1)
           END-IF
           .
       400-KOHFTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿Ç»¡ÎÁ¥³¡¼¥É¤ò·èÄê½èÍý
      *****************************************************************
       2020-SRYCD-SET-SEC        SECTION.
      *
      *    ¼õ¿ÇÍúÎò¤è¤ê¿Ç»¡²ó¿ô¤òµá¤á¤ë
           PERFORM 1000-SINSATU-KAISU-SEC
      *
           MOVE    ZERO                TO  FLG-ARIS
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ
           IF      (FLG-SYONIKA-ARI    =   1   )  AND
                  ((WRK-NENREI-YY      <   3   )  OR
                   (WRK-NENREI-YY      =   3   AND
                    WRK-NENREI-MM      =   ZERO ))
               PERFORM 20201-SYONIKA-HEN-SEC
               IF      FLG-ARIS        =   1
                   GO      TO      2020-SRYCD-SET-EXT
               END-IF
           END-IF
      *
      *    ¿Ç»¡ÎÁ¥³¡¼¥É¤ò·èÄê¤¹¤ë
           IF      WRK-SYOSIN          =   1
               MOVE    "11"                TO  WRK-SRYKBN
               MOVE    "F"                 TO  WRK-KBN
      *        ¿Ç»¡¥³¡¼¥É¤ò¥Æ¡¼¥Ö¥ë¤è¤ê·èÄê
               PERFORM 2001-SRYCD-TBL-SEC
               MOVE    "110"           TO
                       WRK-LF01-BUNDLE-CLASSCODE    
           ELSE
      *        Æ±ÆüºÆ¿ÇÎÁ»»Äê
               IF      WRK-LSTYMD          =   SPA-SYSYMD
                   MOVE    "12"                TO  WRK-SRYKBN
                   MOVE    "D"                 TO  WRK-KBN
      *            ¿Ç»¡¥³¡¼¥É¤ò¥Æ¡¼¥Ö¥ë¤è¤ê·èÄê
                   PERFORM 2001-SRYCD-TBL-SEC
               ELSE
      *        ºÆ¿ÇÎÁ»»Äê
                   MOVE    "12"                TO  WRK-SRYKBN
                   MOVE    SPACE               TO  WRK-KBN
      *            ¿Ç»¡¥³¡¼¥É¤ò¥Æ¡¼¥Ö¥ë¤è¤ê·èÄê
                   PERFORM 2001-SRYCD-TBL-SEC
               END-IF
               MOVE    "120"           TO
                       WRK-LF01-BUNDLE-CLASSCODE    
           END-IF
           .
       2020-SRYCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼õ¿ÇÍúÎò¤è¤ê¿Ç»¡²ó¿ô¤òµá¤á¤ë½èÍý
      *****************************************************************
       1000-SINSATU-KAISU-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SINSATU-CNT
           MOVE    ZERO                TO  FLG-15MIMAN
           MOVE    ZERO                TO  WRK-15MIMAN-ARI
      *
      *    Ç¯Îð·×»»
           MOVE    PTINF-BIRTHDAY      TO  WRK-YMDS1
           MOVE    SPA-SYSYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
           END-IF
      *
      *    £±£µºÐ¤Ë¤Ê¤ëÇ¯·îÆü¤ò·×»»
           MOVE    PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
           COMPUTE WRK-BIRTH-YY        =   WRK-BIRTH-YY    +  15
      *
      *    ¼õ¿ÇÍúÎò¤è¤ê¼õ¿Ç²ó¿ô¤òµá¤á¤ë
      *    ¼õ¿ÇÎò¤ò¸¡º÷¤¹¤ë¡Ê¿ÇÎÅ·î£±Æü¤«¤é¿ÇÎÅÆü¤Þ¤Ç¤ÎÈÏ°Ï¡Ë
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
      *
      *    £È£±£µÇ¯£¶·î¤è¤ê¤¹¤Ù¤Æ£±²óÌÜ¤È¤¹¤ë
           IF      SPA-SYSYMD          >=  "20030601"
               MOVE    SPA-SYSYMD          TO  JYURRK-SRYYMD
           ELSE
               MOVE    SPA-SYSYMD          TO  JYURRK-SRYYMD
               MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           END-IF
           MOVE    SPA-SYSYMD          TO  JYURRK-UPSRYYMD
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL    FLG-JYURRK     =   1
               IF      JYURRK-RENNUM       =   1
                   MOVE    ZERO            TO  FLG-SYONIKA
               END-IF
               MOVE    ZERO                TO  FLG-CHK
               IF      FLG-CHK             =   ZERO
      *            ¼õ¿ÇÍúÎò¤ËÆ±ÆüÂ¾²Ê»»Äê¤¬¤Ê¤¤¤³¤È
                   PERFORM 10001-DOUJI-CHK-SEC
               END-IF
      *
               IF     (JYURRK-EDANUM       =   1   )  AND
                      (JYURRK-DOUJI-RENNUM =   1  OR  ZERO )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-CHK
               END-IF
      *        ¼õ¿Ç²ó¿ô
               IF      FLG-CHK         =   ZERO
                   ADD     1               TO  WRK-SINSATU-CNT
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *!   £±£µºÐÌ¤ËþºÆ¿Ç²Ê»»ÄêÍ­¤ê
      *    IF     (WRK-NENREI-YY       =   15              )  AND
      *           (WRK-BIRTHDAY(1:6)   =   SPA-SYSYMD (1:6))
      *        IF      FLG-15MIMAN         =   1
      *            MOVE    1               TO  WRK-15MIMAN-ARI
      *        END-IF
      *!   END-IF
           .
       1000-SINSATU-KAISU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼õ¿ÇÍúÎò¤ËÆ±ÆüÂ¾²Ê»»Äê¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       10001-DOUJI-CHK-SEC                  SECTION.
      *
           IF      JYURRK-SRYKBN (01)      =   "01"
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-CHK
               GO      TO      10001-DOUJI-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-SINSATU
           MOVE    ZERO                TO  FLG-SYOSIN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15  )  OR
                              (FLG-SINSATU         =   1     )  OR
                              (JYURRK-ZAINUM (IDX) =   ZERO  )
      *
               MOVE    SPACE               TO  SRYACT-REC
               INITIALIZE                  SRYACT-REC
               MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
               MOVE    "2"                 TO  SRY-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  SRY-PTID
               MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
               MOVE    JYURRK-ZAINUM (IDX)
                                           TO  SRY-ZAINUM
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACT-READ-SEC
                ELSE
                   INITIALIZE                  SRYACT-REC
                   MOVE    1               TO  FLG-SRYACT
               END-IF
      *
               PERFORM
                       UNTIL  (FLG-SRYACT      =   1     )  OR
                              (FLG-SINSATU     =   1     )
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL   (IDY        >   5   )  OR
                                   (FLG-SINSATU          =   1     )  OR
                                   (SRY-SRYCD (IDY)      =   SPACE )
                       PERFORM 100011-IPN-DOUJI-CHK-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACT-READ-SEC
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
           END-PERFORM
      *
           IF    ((FLG-SINSATU         =   1   )  OR
                  (FLG-SYONIKA         =   1   ))  AND
                  (FLG-SYOSIN          =   ZERO )
               MOVE    ZERO            TO  FLG-CHK
           ELSE
               MOVE    1               TO  FLG-CHK
           END-IF
      *
      *    ½é¿ÇÎÁ»»Äê»þ¡¢²ó¿ô¤ò¥¯¥ê¥¢¤¹¤ë
           IF      FLG-SYOSIN          =   1
               MOVE    ZERO            TO  WRK-SINSATU-CNT
           END-IF
      *
           .
       10001-DOUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    °ìÈÌÆ±ÆüÂ¾²Ê»»Äê¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       100011-IPN-DOUJI-CHK-SEC        SECTION.
      *
      *    ¿Ç»¡ÎÁ¸¡º÷
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
               AT   END
                   MOVE    ZERO               TO  FLG-ARI
                   MOVE    SPACE              TO  WRK-KBN
                   MOVE    SPACE              TO  WRK-KBN2
               WHEN   SRY-SRYCD (IDY)     =   TBL-SRYCD (TBL-IDX)
                   MOVE    1                  TO  FLG-ARI
                   MOVE    TBL-KBN  (TBL-IDX) TO  WRK-KBN
                   MOVE    TBL-KBN2 (TBL-IDX) TO  WRK-KBN2
           END-SEARCH
      *
           IF      FLG-ARI         =   1
               MOVE    1               TO  FLG-SINSATU
      *        ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ¤¢¤ê
               IF      SRY-SRYCD(IDY)(1:3)     =   "113"
                   MOVE    1           TO  FLG-SYONIKA
               END-IF
      *!       £±£µºÐÌ¤Ëþ¤Î¼õ¿Ç¤¢¤ê
      *        IF      JYURRK-SRYYMD       <   WRK-BIRTHDAY
      *            MOVE    1               TO  FLG-15MIMAN
      *!       END-IF
           ELSE
               EVALUATE    SRY-SRYCD (IDY)
      *            ºßÂð´µ¼ÔË¬Ìä¿Ç»¡ÎÁ
                   WHEN    "114001110"
      *            ºßÂðËö´ü°åÎÅÁí¹ç¿Ç»¡ÎÁ
                   WHEN    "114007610"
                   WHEN    "114007710"
                       MOVE    1               TO  FLG-SINSATU
               END-EVALUATE
           END-IF
      *    ½é¿ÇÎÁ¤Ï»»Äê³°¤È¤¹¤ë
           IF     (SRY-SRYKBN          =   "11" )  OR
                  (SRY-SRYCD(IDY)      =   "113003510"  OR
                                           "113003710" )
               MOVE    1               TO  FLG-SYOSIN
           END-IF
      *
           .
       100011-IPN-DOUJI-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ¿Ç»¡ÎÁ¤ò¥Æ¡¼¥Ö¥ë¤è¤ê·èÄê½èÍý
      *****************************************************************
       2001-SRYCD-TBL-SEC              SECTION.
      *
           IF      WRK-SRYKBN          =   "11"
               MOVE    1               TO  WRK-KAISU
           ELSE
      ******   EVALUATE    WRK-SINSATU-CNT
      *            WHEN    0
      *                MOVE    1               TO  WRK-KAISU
      *            WHEN    1
      *            WHEN    2
      *                MOVE    2               TO  WRK-KAISU
      *            WHEN    OTHER
      *                MOVE    4               TO  WRK-KAISU
      ******** END-EVALUATE
      *        ÉÂ±¡¤Ç£²£°£°¾²°Ê¾å¤Ï³°Íè¿ÇÎÅÎÁ¤È¤¹¤ë
               IF      (SYS-1001-HOSPSBT       =   "1"  )  AND
                       (SYS-1001-BEDSUIPN      >=  200  )
                   MOVE    5               TO  WRK-KAISU
               ELSE
      *        °Ê³°¤Ï¤¹¤Ù¤Æ£±²óÌÜ¤È¤¹¤ë
                   MOVE    1               TO  WRK-KAISU
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-KBN2
      *!!! £±£µºÍÌ¤Ëþ
      *    IF    ((WRK-NENREI-YY       <   15 ) OR
      *           (WRK-15MIMAN-ARI     =   1  )) AND
      *          ( WRK-KAISU           =   4  )
      *        MOVE    4                   TO  WRK-KAISU
      *        MOVE    "M"                 TO  WRK-KBN2
      *!!! END-IF
      *    Ï·¿Í
           IF      WRK-NENREI-YY       >=  70
               MOVE    1               TO  FLG-ROUJIN
           ELSE
               MOVE    ZERO            TO  FLG-ROUJIN
           END-IF
      *£È£±£¸¡¿£´²þÀµÂÐ±þ
      *    ¤¹¤Ù¤Æ°ìÈÌ
           MOVE    SYS-1001-HOSPSBT    TO  WRK-HOSPSBT
           IF      SPA-SYSYMD(1:6)     >=  "200604"
               MOVE    ZERO                TO  FLG-ROUJIN
               IF      WRK-KBN             =   "F"
                   MOVE    1               TO  WRK-HOSPSBT
               END-IF
           END-IF
      *£È£²£²Ç¯£´·î²þÀµ
      *    ¤¹¤Ù¤ÆÉÂ±¡¡Ê¿ÇÎÅ½êÇÑ»ß¡Ë
           IF      SPA-SYSYMD(1:6)     >=  "201004"
               MOVE    1               TO  WRK-HOSPSBT
           END-IF
      *
      *    ¿Ç»¡ÎÁ¥»¥Ã¥È
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-LF01-IJICD
                       DISPLAY "*** ORCL0011 IJICD ERR HOSPSBT="
                               SYS-1001-HOSPSBT " SYOSIN=" WRK-SYOSIN 
                   WHEN   (WRK-KAISU           =   TBL-KAISU (TBL-IDX))
                     AND  (FLG-ROUJIN          =   TBL-ROUJIN(TBL-IDX))
                     AND  (WRK-HOSPSBT         =   TBL-HOSPSBT(TBL-IDX))
                     AND  (WRK-KBN             =   TBL-KBN    (TBL-IDX))
                     AND  (WRK-KBN2            =   TBL-KBN2   (TBL-IDX))
                     AND  (WRK-SRYKBN          =   TBL-SRYCD(TBL-IDX)
                                                                (2:2))
                       MOVE    TBL-SRYCD (TBL-IDX)
                                               TO  WRK-LF01-IJICD
           END-SEARCH
      *
           .
       2001-SRYCD-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁÊÑ´¹½èÍý
      *****************************************************************
       20201-SYONIKA-HEN-SEC        SECTION.
      *
      *    £³ºÐ¤Ë¤Ê¤ëÇ¯·îÆü¤ò·×»»
           MOVE    PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
           COMPUTE WRK-BIRTH-YY        =   WRK-BIRTH-YY    +  3
      *    £³ºÐÅþÃ£·î¤ËÅ¬ÍÑ
           IF      WRK-BIRTHDAY(1:6)   <   SPA-SYSYMD (1:6)
               GO      TO      20201-SYONIKA-HEN-EXT
           END-IF
      *
           IF      (WRK-NENREI-YY      =   3 )  AND
                   (WRK-NENREI-MM      =   ZERO)  AND
                   (PTINF-BIRTHDAY(7:2)    <=  SPA-SYSYMD(7:2))
      *        £³ºÐ¤ÎÃÂÀ¸Æü°ÊÁ°¤Ç¡¢Åö·î¤Ë¿Ç»¡¤¬¤¢¤Ã¤¿»þ
               IF      WRK-LSTYMD(1:6)     =   SPA-SYSYMD(1:6)
                   IF     (WRK-LSTYMD          =   SPA-SYSYMD) AND
                          (FLG-SYONIKA         =   ZERO      )
                       GO      TO      20201-SYONIKA-HEN-EXT
                   END-IF
               ELSE
                   GO      TO      20201-SYONIKA-HEN-EXT
               END-IF
           END-IF
           MOVE    1                   TO  FLG-ARIS
      *
      *    ¿Ç»¡ÎÁ¥³¡¼¥É¤ò·èÄê¤¹¤ë
           IF      WRK-SYOSIN          =   1
      *        ½é¿Ç
               IF      SYS-1001-SYOHOKBN    =   "0"
      *            ±¡Æâ
                   MOVE    "113003710"         TO  WRK-LF01-IJICD
               ELSE
      *            ±¡³°
                   MOVE    "113003510"         TO  WRK-LF01-IJICD
               END-IF
               MOVE    "130"           TO  WRK-LF01-BUNDLE-CLASSCODE
           ELSE
      *        ºÆ¿Ç
               IF      SYS-1001-SYOHOKBN    =   "0"
      *            ±¡Æâ
                   MOVE    "113003810"         TO  WRK-LF01-IJICD
               ELSE
      *            ±¡³°
                   MOVE    "113003610"         TO  WRK-LF01-IJICD
               END-IF
               MOVE    "130"           TO  WRK-LF01-BUNDLE-CLASSCODE
           END-IF
      *
           .
       20201-SYONIKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¼õÉÕ¥á¥âÊÔ½¸½èÍý
      *****************************************************************
        2009-PTMEMOINF-SEC        SECTION.
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    2                   TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-SYSYMD          TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    SPACE               TO  WRK-MEMO
           PERFORM UNTIL    FLG-PTMEMOINF  =   1
               IF      PTMEMOINF-SRYKA     =   "00"
                   IF      WRK-MEMO        =   SPACE
                       MOVE    PTMEMOINF-MEMO  TO  WRK-MEMO
                   END-IF
               ELSE
                   IF      PTMEMOINF-SRYKA     =   SPA-SRYKA
                       MOVE    PTMEMOINF-MEMO  TO  WRK-MEMO
                   END-IF
               END-IF
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-MEMO-G
           MOVE    WRK-MEMO            TO  WRK-MEMO-OK
           IF      WRK-MEMO        NOT =   SPACE
      *        ²þ¹Ô¤ò"@"¤È¤¹¤ë
               INSPECT     WRK-MEMO    REPLACING  ALL  X"0A"
                                                  BY   "/"
               UNSTRING    WRK-MEMO    DELIMITED BY "¡ö¡ö¡ö¡ö¡ö"
                     INTO  WRK-MEMO-01
                           WRK-MEMO-02
                           WRK-MEMO-03
                           WRK-MEMO-04
                           WRK-MEMO-05
               END-UNSTRING
               IF      WRK-MEMO-01     NOT =   SPACE
                   MOVE    WRK-MEMO-01     TO  WRK-MEMO-OK
               END-IF
               IF      WRK-MEMO-02     NOT =   SPACE
                   IF      WRK-MEMO-02(1:1)    =   "/"
                       MOVE    WRK-MEMO-02(2:) TO  WRK-MEMO-OK
                   ELSE
                       MOVE    WRK-MEMO-02     TO  WRK-MEMO-OK
                   END-IF
               END-IF
               IF      WRK-MEMO-03     NOT =   SPACE
                   IF      WRK-MEMO-03(1:1)    =   "/"
                       MOVE    WRK-MEMO-03(2:) TO  WRK-MEMO-OK
                   ELSE
                       MOVE    WRK-MEMO-03     TO  WRK-MEMO-OK
                   END-IF
               END-IF
               IF      WRK-MEMO-04     NOT =   SPACE
                   IF      WRK-MEMO-04(1:1)    =   "/"
                       MOVE    WRK-MEMO-04(2:) TO  WRK-MEMO-OK
                   ELSE
                       MOVE    WRK-MEMO-04     TO  WRK-MEMO-OK
                   END-IF
               END-IF
               IF      WRK-MEMO-05     NOT =   SPACE
                   IF      WRK-MEMO-05(1:1)    =   "/"
                       MOVE    WRK-MEMO-05(2:) TO  WRK-MEMO-OK
                   ELSE
                       MOVE    WRK-MEMO-05     TO  WRK-MEMO-OK
                   END-IF
               END-IF
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    1000                TO  KANACONV-LEN
               MOVE    1000                TO  KANACONV-STLEN
               MOVE    0                   TO  KANACONV-CONV-FLAG
               MOVE    0                   TO  KANACONV-CHAR-TYPE
               MOVE    WRK-MEMO-OK         TO  KANACONV-INDATA
               CALL    "kanaconv"          USING ORCXKANACONVAREA
               MOVE    KANACONV-OUTDATA    TO  WRK-MEMO-OK
           END-IF
      *    ¼õÉÕ¥á¥â
           MOVE    WRK-MEMO-OK         TO  WRK-LF01-UKE-MEMO
           .
        2009-PTMEMOINF-EXT.
           EXIT.
      *****************************************************************
      *    ÆüÉÕÊÑ´¹½èÍý
      *****************************************************************
       500-HIZUKE-HENSYU-SEC        SECTION.
      *
           IF      WRK-WYMD         =   SPACE
               MOVE    ALL "0"          TO  WRK-WYMD
           END-IF
      *
           IF      WRK-WYMD         =   ALL "9"
               MOVE    "99991231"       TO  WRK-WYMD
           END-IF
      *
           MOVE    WRK-WYY          TO  WRK-HENYY
           MOVE    WRK-WMM          TO  WRK-HENMM
           MOVE    WRK-WDD          TO  WRK-HENDD
           .
       500-HIZUKE-HENSYU--EXT.
           EXIT.
      *
      *****************************************************************
      *    £Õ£É£Ä¼èÆÀ½èÍý
      *****************************************************************
       500-UUID-SET-SEC             SECTION.
      *
           INITIALIZE     UIDIO-AREA.
           CALL    "orcuidset" USING ST UIDIO-AREA.
           DISPLAY "UUID CALL" UIDIO-AREA.
      *    WIN-EXEC GRAPH "DLL_orcuidset", UIDIO-AREA, " ", " ",
      *                                    UIDIO-AREA, ST
           .
       500-UUID-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Í¹ÊØÈÖ¹æÊÑ´¹½èÍý
      *****************************************************************
       500-POST-HENSYU-SEC        SECTION.
      *
           IF      WRK-POST        =   SPACE
               MOVE    SPACE           TO  WRK-HENPOST
           ELSE
               MOVE    WRK-POST(1:3)   TO  WRK-HENPOST (1:3)
               MOVE    "-"             TO  WRK-HENPOST (4:1)
               MOVE    WRK-POST(4:4)   TO  WRK-HENPOST (5:4)       
           END-IF
           .
       500-POST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á´³Ñ³ÈÄ¥´Á»úÊÑ´¹½èÍý
      *****************************************************************
       800-ORCSKANACHK-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-KOUMOKU         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-KOUMOKU
           .
       800-ORCSKANACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»¡¡¡¡½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      FLG-CLMUID          =   ZERO
      *        UID¹¹¿·½èÍý
               MOVE    SMCNDATE-YMD        TO  CLMUID-UPYMD
               MOVE    SMCNDATE-HMS        TO  CLMUID-UPHMS
      *
               MOVE    CLMUID-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_clm_uid"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   DISPLAY "ORCL0011 UPD CLMUID-KEY =>" CLMUID-KEY  "."
               END-IF
           ELSE
      *        UIDÅÐÏ¿½èÍý
               MOVE    SMCNDATE-YMD        TO  CLMUID-CREYMD
               MOVE    SMCNDATE-YMD        TO  CLMUID-UPYMD
               MOVE    SMCNDATE-HMS        TO  CLMUID-UPHMS
      *
               MOVE    CLMUID-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_clm_uid"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   DISPLAY "ORCL0011 INS CLMUID-KEY =>" CLMUID-KEY  "."
               END-IF
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ½èÍý
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÊÝ¸±¾ðÊó¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊó¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-PTKOHINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTKOHINF-INV-EXT.
           EXIT.
      *****************************************************************
      *    £Õ£É£Ä¥Ç¡¼¥¿ÆÉ¹þ½èÍý
      *****************************************************************
       903-CLMUID-READ-SEC         SECTION.
      *
           MOVE    CLMUID-REC          TO  MCPDATA-REC
           MOVE    "tbl_clm_uid"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_clm_uid"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  CLMUID-REC
                   MOVE    ZERO                TO  FLG-CLMUID
               ELSE
                   MOVE    1                   TO  FLG-CLMUID
                   INITIALIZE                  CLMUID-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-CLMUID
               INITIALIZE                  CLMUID-REC
           END-IF
      *
           MOVE    "tbl_clm_uid"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       903-CLMUID-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-COMB-READ-SEC         SECTION.
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
       900-COMB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ½èÍý
      *****************************************************************
       901-SYSKANRI-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       901-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¥­¡¼¡Ë
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÎò¥Ç¡¼¥¿¼ç¥­¡¼¸¡º÷
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ôÆÉ¹þ
      *****************************************************************
       900-TENSU-INV-SEC           SECTION.
      *
           MOVE    SPA-SYSYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SYSYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-TENSU
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               ELSE
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-TENSU-INV-EXT.
           EXIT.
      ******************************************************************
      *    ¼õ¿ÇÍúÎò¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¼¡¸¡º÷½èÍý
      *****************************************************************
       980-HKNNUM-READ-SEC             SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       980-HKNNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¥á¥âÆÉ¹þ½èÍý
      *****************************************************************
       910-PTMEMOINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTMEMOINF-REC
               MOVE    ZERO                TO  FLG-PTMEMOINF
           ELSE
               INITIALIZE                      PTMEMOINF-REC
               MOVE    1                   TO  FLG-PTMEMOINF
           END-IF
      *
           .
       910-PTMEMOINF-READ-EXT.
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
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
