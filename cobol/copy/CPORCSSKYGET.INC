      *******************************************************************
      * Project code name "ORCA"
      * ∆¸∞Â…∏Ω‡•Ï•ª•◊•»•Ω•’•»° JMA standard receipt software°À
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
      *******************************************************************
      *     ›∏±º‘…È√¥≈˘§Œ¿¡µ·≥€ºË§Íπ˛§ﬂ•µ•÷•—•È•·•ø
      *    COPY  (CPORCSSKYGET.INC)
      ************************************************************
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      ************************************************************
       01  ORCSSKYGETAREA.
      *
      *∆˛Œœ√Õ
      *  ∂Ë ¨
           03  LNK-SKYGET-KBN                           PIC X(01).
      *  ø«Œ≈«Ø∑Ó
           03  LNK-SKYGET-SRYYM                         PIC 9(06).
      *
      * ÷µ—√Õ
           03  LNK-SKYGET-OUT-AREA.
      *
      *  ¿¡µ·≥€° ∞ÂŒ≈»Ò°Àæ Û
               05  LNK-SKYGET-IRYO-INF.
      *    º¬∆¸øÙ
                   07  LNK-SKYGET-IRYO-NISSU            PIC 9(02).
      *    ¿¡µ·≈¿
                   07  LNK-SKYGET-IRYO-TOTALTEN         PIC 9(08).
      *    ¥µº‘…È√¥
                   07  LNK-SKYGET-IRYO-ICHIBUFTN        PIC 9(09).
      *     ›∏±º‘…È√¥
                   07  LNK-SKYGET-IRYO-HOKENJA          PIC 9(09).
      *    ∏¯»Ò…È√¥
                   07  LNK-SKYGET-IRYO-KOHFTN           PIC 9(09).
      *
      *  ¿¡µ·≥€° ø©ªˆ°Àæ Û
               05  LNK-SKYGET-SKJ-INF.
      *    ≤ÛøÙ
                   07  LNK-SKYGET-SKJ-KAISU             PIC 9(02).
      *    ¿¡µ·±ﬂ
                   07  LNK-SKYGET-SKJ-RYOYOHI           PIC 9(09).
      *    ¥µº‘…È√¥
                   07  LNK-SKYGET-SKJ-ICHIBUFTN         PIC 9(09).
      *     ›∏±º‘…È√¥
                   07  LNK-SKYGET-SKJ-HOKENJA           PIC 9(09).
      *    ∏¯»Ò…È√¥
                   07  LNK-SKYGET-SKJ-KOHFTN            PIC 9(09).
      *
      *  ∏¯»Òæ Û° •Ï•ª•◊•»¿¡µ· ¨°À
               05  LNK-SKYGET-KOH-INF.
                 06  LNK-SKYGET-KOH-OCC                 OCCURS 4.
                   07  LNK-SKYGET-KOH-HKNNUM            PIC X(03).
                   07  LNK-SKYGET-KOH-NISSU             PIC 9(02).
                   07  LNK-SKYGET-KOH-TOTALTEN          PIC 9(08).
                   07  LNK-SKYGET-KOH-FTNMONEY          PIC 9(09).
                   07  LNK-SKYGET-KOH-KAISU             PIC 9(02).
                   07  LNK-SKYGET-KOH-RYOYOHI           PIC 9(09).
                   07  LNK-SKYGET-KOH-SHOKUJIFTN        PIC 9(09).
      *
      *  ∏¯»Òæ Û° ∏¯»Ò¿¡µ·ΩÒ¿¡µ· ¨°À
               05  LNK-SKYGET-KOHETC-INF.
                 06  LNK-SKYGET-KOHETC-OCC              OCCURS 4.
                   07  LNK-SKYGET-KOHETC-HKNNUM         PIC X(03).
                   07  LNK-SKYGET-KOHETC-NISSU          PIC 9(02).
                   07  LNK-SKYGET-KOHETC-TOTALTEN       PIC 9(08).
                   07  LNK-SKYGET-KOHETC-FTNMONEY       PIC 9(09).
                   07  LNK-SKYGET-KOHETC-KAISU          PIC 9(02).
                   07  LNK-SKYGET-KOHETC-RYOYOHI        PIC 9(09).
                   07  LNK-SKYGET-KOHETC-SHOKUJIFTN     PIC 9(09).
      *
      * ÷µ—√Õ£≤
           03  LNK-SKYGET-OUT2-AREA.
      *      (1)º“ ›§Œº´»Ò ¨∞ ≤º
      *      (2)πÒ ›§Œº´»Ò ¨∞ ≤º
      *      (3)∏Â¥¸π‚Œ§Œº´»Ò ¨∞ ≤º
      *      (4)∆√ ÃŒ≈Õ‹»Ò
      *      (5)º´»Ò ›∏±
      *      (6)º£∏≥
      *      (7)œ´∫“
      *      (8)º´«Â¿’
      *      (9)∏¯≥≤
             04  LNK-SKYGET-OUT2-OCC                    OCCURS 9.
      *        (1)∆˛±°§Œ≤æº˝«º(2)≥∞ÕË(3)∆˛±°§ŒÀ‹º˝«º
               05  LNK-SKYGET-OUT2-OCC2                 OCCURS 3.
      *
      *  ∑ÔøÙ
                 06  LNK-SKYGET-KENSU                   PIC 9(06).
      *
      *  ¿¡µ·≥€° ∞ÂŒ≈»Ò°Àæ Û
                 06  LNK-SKYGET-IRYO2-INF.
      *    º¬∆¸øÙ
                   07  LNK-SKYGET-IRYO2-NISSU           PIC 9(06).
      *    ¿¡µ·≈¿
                   07  LNK-SKYGET-IRYO2-TOTALTEN        PIC 9(08).
      *    ¥µº‘…È√¥
                   07  LNK-SKYGET-IRYO2-ICHIBUFTN       PIC 9(09).
      *     ›∏±º‘…È√¥
                   07  LNK-SKYGET-IRYO2-HOKENJA         PIC 9(09).
      *    ∏¯»Ò…È√¥
                   07  LNK-SKYGET-IRYO2-KOHFTN          PIC 9(09).
      *
      *  ¿¡µ·≥€° ø©ªˆ°Àæ Û
                 06  LNK-SKYGET-SKJ2-INF.
      *    ≤ÛøÙ
                   07  LNK-SKYGET-SKJ2-KAISU            PIC 9(06).
      *    ¿¡µ·±ﬂ
                   07  LNK-SKYGET-SKJ2-RYOYOHI          PIC 9(09).
      *    ¥µº‘…È√¥
                   07  LNK-SKYGET-SKJ2-ICHIBUFTN        PIC 9(09).
      *     ›∏±º‘…È√¥
                   07  LNK-SKYGET-SKJ2-HOKENJA          PIC 9(09).
      *    ∏¯»Ò…È√¥
                   07  LNK-SKYGET-SKJ2-KOHFTN           PIC 9(09).
      *
      *  º´»Ò ¨
                 06  LNK-SKYGET-JIHI                    PIC 9(09).
      *  ∏∫Ã»∂‚≥€
                 06  LNK-SKYGET-DISCOUNT-MONEY          PIC S9(09).
      *  ƒ¥¿∞∂‚∑◊
                 06  LNK-SKYGET-CHOSEI                  PIC S9(09).
      *  ƒ¥¿∞∂‚£±
                 06  LNK-SKYGET-CHOSEI1                 PIC S9(09).
      *  ƒ¥¿∞∂‚£≤
                 06  LNK-SKYGET-CHOSEI2                 PIC S9(09).
      *  √ºøÙƒ¥¿∞∂‚≥€
                 06  LNK-SKYGET-HASU                    PIC S9(09).
      *
      *…¸µ¢•≥°º•…
           03  LNK-SKYGET-RC                            PIC 9(02).
