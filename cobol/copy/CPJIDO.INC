      ******************************************************************
      *    CPJIDO ¡ÊXXXXXXXXXXXXXX)
      ******************************************************************
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  02.00.00    NACL-Â¿¡¹Ç¼  04/03/19  ²þÀµÂÐ±þ
      ************************************************************
       01  CPJIDOTBL.
           02  JIDOTBL-T.
             03  JIDOTBL-T1.
               05  FILLER              PIC  X(9)   VALUE   "180016410" .
               05  FILLER              PIC  X(9)   VALUE   "180001110" .
               05  FILLER              PIC  X(9)   VALUE   "630010002" .
               05  FILLER              PIC  X(9)   VALUE   "120000710" .
               05  FILLER              PIC  X(9)   VALUE   "120001010" .
               05  FILLER              PIC  X(9)   VALUE   "120002610" .
               05  FILLER              PIC  X(9)   VALUE   "120001210" .
               05  FILLER              PIC  X(9)   VALUE   "120000110" .
               05  FILLER              PIC  X(9)   VALUE   "120001310" .
               05  FILLER              PIC  X(9)   VALUE   "120001810" .
               05  FILLER              PIC  X(9)   VALUE   "120002370" .
               05  FILLER              PIC  X(9)   VALUE   "120002470" .
               05  FILLER              PIC  X(9)   VALUE   "113701310" .
               05  FILLER              PIC  X(9)   VALUE   "113701410" .
               05  FILLER              PIC  X(9)   VALUE   "113701310" .
               05  FILLER              PIC  X(9)   VALUE   "113701410" .
               05  FILLER              PIC  X(9)   VALUE   "820000047" .
               05  FILLER              PIC  X(9)   VALUE   "160061710" .
               05  FILLER              PIC  X(9)   VALUE   "160700510" .
               05  FILLER              PIC  X(9)   VALUE   "160061810" .
               05  FILLER              PIC  X(9)   VALUE   "160700610" .
               05  FILLER              PIC  X(9)   VALUE   "160061910" .
               05  FILLER              PIC  X(9)   VALUE   "160700710" .
               05  FILLER              PIC  X(9)   VALUE   "160062010" .
               05  FILLER              PIC  X(9)   VALUE   "160700810" .
               05  FILLER              PIC  X(9)   VALUE   "160062110" .
               05  FILLER              PIC  X(9)   VALUE   "160700910" .
               05  FILLER              PIC  X(9)   VALUE   "160062210" .
               05  FILLER              PIC  X(9)   VALUE   "160701010" .
               05  FILLER              PIC  X(9)   VALUE   "160062310" .
               05  FILLER              PIC  X(9)   VALUE   "160146910" .
               05  FILLER              PIC  X(9)   VALUE   "160147010" .
               05  FILLER              PIC  X(9)   VALUE   "160147610" .
               05  FILLER              PIC  X(9)   VALUE   "160147710" .
               05  FILLER              PIC  X(9)   VALUE   "160147910" .
      ******** Æ°Ì®·ìºÎ·ì
      *        05  FILLER              PIC  X(9)   VALUE   "160101210" .
      *        ÀÅÌ®·ìºÎ·ì
      *        05  FILLER              PIC  X(9)   VALUE   "160095710" .
      *        Ëõ¾ÃºÎ·ì
      ******** 05  FILLER              PIC  X(9)   VALUE   "160095810" .
      *£È£±£´¡¥£´²þÀµÊ¬
      *        ½èÊý¤»¤óÎÁ
               05  FILLER              PIC  X(9)   VALUE   "120002810" .
               05  FILLER              PIC  X(9)   VALUE   "120003010" .
               05  FILLER              PIC  X(9)   VALUE   "120002710" .
               05  FILLER              PIC  X(9)   VALUE   "120002910" .
      *
      *Æþ±¡Ê¬
               05  FILLER              PIC  X(9)   VALUE   "120001710" .
               05  FILLER              PIC  X(9)   VALUE   "120001110" .
               05  FILLER              PIC  X(9)   VALUE   "120000410" .
      *
           02  JIDOTBL-R  REDEFINES   JIDOTBL-T.
      *******03  JIDOTBL-TX           OCCURS  38.
      *******03  JIDOTBL-TX           OCCURS  42.
      ****** 03  JIDOTBL-TX           OCCURS  39.
             03  JIDOTBL-TX           OCCURS  42.
               05  JIDOTBL-CODE        PIC  X(9).
       01  JIDO-MAX                PIC 9(04)   VALUE   44.
      *
