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
      ********************************************************************
      ************************************************************
      *    ºıø«Õ˙ŒÚ°°    °°°°°° COPY  (CPJYURRK.INC)    *
      ************************************************************
      ************************************************************
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      *  01.00.01    MCC-¬ø°π«º   02/04/10  œ´∫“ƒ…≤√
      *  01.00.02    NACL-¬ø°π«º  02/10/21  ∆˛≥∞∂Ë ¨ƒ…≤√
      ************************************************************
      *01  JYURRK-REC.
      *ºıø«Õ˙ŒÚ
           03  JYURRK-KEY.
      *  ∞ÂŒ≈µ°¥ÿ£…£ƒ
               05  JYURRK-HOSPID           PIC  X(24).
      *  ¥µº‘£…£ƒ
               05  JYURRK-PTID             PIC  9(10).
      *  ∆˛≥∞∂Ë ¨
               05  JYURRK-NYUGAIKBN        PIC  X(01).
      *  ø«Œ≈≤ 
               05  JYURRK-SRYKA            PIC  X(02).
      *  ø«Œ≈∆¸
               05  JYURRK-SRYYMD           PIC  X(08).
      *  œ¢»÷° £±§´§È§Œ∆±∆¸œ¢»÷°À
               05  JYURRK-RENNUM           PIC  9(01).
      *  ∆±ª˛ø«Œ≈œ¢»÷° £∞°ß∑—¬≥§ §∑°¢£±§´§È§Œœ¢»÷°ß∑—¬≥§¢§Í°À
               05  JYURRK-DOUJI-RENNUM     PIC  9(01).
      *  ≤Ò∑◊œ¢»÷° ≤Ò∑◊æ ÛΩ§¿µª˛§Œœ¢»÷°À
               05  JYURRK-KAIKEI-RENNUM    PIC  9(03).
      *  ªﬁ»÷° £±ø«Œ≈£±£µ∫ﬁ∞ æÂ§Œª˛§À£±§´§È§Œœ¢»÷°À
               05  JYURRK-EDANUM           PIC  9(01).
      *  ∫«Ω™∂Ë ¨
           03  JYURRK-LASTKBN              PIC X(01).
      *•…•Ø•ø°º
           03  JYURRK-DRCD                 PIC X(05).
      * ›∏±¡»πÁ»÷πÊ
           03  JYURRK-HKNCOMBINUM          PIC X(04).
      * ›∏±∂Ë ¨° £∞°ß∞Ï»Ã°¢£±°ßœ´∫“°¢£≤°ßº´«Â¿’°¢£≥°ßªı≤ °À
           03  JYURRK-HKNKBN               PIC X(01).
      *≈¡…º»÷πÊ
           03  JYURRK-DENPNUM              PIC 9(07).
      *ø«Œ≈∂Ë ¨
           03  JYURRK-SRYKBN-G.
      *  ø«Œ≈∂Ë ¨£±
             05  JYURRK-SRYKBN1            PIC  X(02).
      *  ø«Œ≈∂Ë ¨£≤
             05  JYURRK-SRYKBN2            PIC  X(02).
      *  ø«Œ≈∂Ë ¨£≥
             05  JYURRK-SRYKBN3            PIC  X(02).
      *  ø«Œ≈∂Ë ¨£¥
             05  JYURRK-SRYKBN4            PIC  X(02).
      *  ø«Œ≈∂Ë ¨£µ
             05  JYURRK-SRYKBN5            PIC  X(02).
      *  ø«Œ≈∂Ë ¨£∂
             05  JYURRK-SRYKBN6            PIC  X(02).
      *  ø«Œ≈∂Ë ¨£∑
             05  JYURRK-SRYKBN7            PIC  X(02).
      *  ø«Œ≈∂Ë ¨£∏
             05  JYURRK-SRYKBN8            PIC  X(02).
      *  ø«Œ≈∂Ë ¨£π
             05  JYURRK-SRYKBN9            PIC  X(02).
      *  ø«Œ≈∂Ë ¨£±£∞
             05  JYURRK-SRYKBN10           PIC  X(02).
      *  ø«Œ≈∂Ë ¨£±£±
             05  JYURRK-SRYKBN11           PIC  X(02).
      *
           03  JYURRK-SRYKBN-R         REDEFINES   JYURRK-SRYKBN-G.
      * ø«Œ≈∂Ë ¨
             05  JYURRK-SRYKBN             PIC  X(02)
                                           OCCURS   11.
      *∫ﬁ»÷πÊ
           03  JYURRK-ZAINUM-G.
      *  ∫ﬁ»÷πÊ£±
             05  JYURRK-ZAINUM1            PIC  9(08).
      *  ∫ﬁ»÷πÊ£≤
             05  JYURRK-ZAINUM2            PIC  9(08).
      *  ∫ﬁ»÷πÊ£≥
             05  JYURRK-ZAINUM3            PIC  9(08).
      *  ∫ﬁ»÷πÊ£¥
             05  JYURRK-ZAINUM4            PIC  9(08).
      *  ∫ﬁ»÷πÊ£µ
             05  JYURRK-ZAINUM5            PIC  9(08).
      *  ∫ﬁ»÷πÊ£∂
             05  JYURRK-ZAINUM6            PIC  9(08).
      *  ∫ﬁ»÷πÊ£∑
             05  JYURRK-ZAINUM7            PIC  9(08).
      *  ∫ﬁ»÷πÊ£∏
             05  JYURRK-ZAINUM8            PIC  9(08).
      *  ∫ﬁ»÷πÊ£π
             05  JYURRK-ZAINUM9            PIC  9(08).
      *  ∫ﬁ»÷πÊ£±£∞
             05  JYURRK-ZAINUM10           PIC  9(08).
      *  ∫ﬁ»÷πÊ£±£±
             05  JYURRK-ZAINUM11           PIC  9(08).
      *  ∫ﬁ»÷πÊ£±£≤
             05  JYURRK-ZAINUM12           PIC  9(08).
      *  ∫ﬁ»÷πÊ£±£≥
             05  JYURRK-ZAINUM13           PIC  9(08).
      *  ∫ﬁ»÷πÊ£±£¥
             05  JYURRK-ZAINUM14           PIC  9(08).
      *  ∫ﬁ»÷πÊ£±£µ
             05  JYURRK-ZAINUM15           PIC  9(08).
      *
           03  JYURRK-ZAINUM-R         REDEFINES   JYURRK-ZAINUM-G.
      *  ∫ﬁ»÷πÊ
             05  JYURRK-ZAINUM             PIC  9(08)
                                           OCCURS 15.
      *√ºÀˆ£…£ƒ
           03  JYURRK-TERMID               PIC  X(16).
      *•™•⁄°º•Ï°º•ø£…£ƒ
           03  JYURRK-OPID                 PIC  X(16).
      *∫Ó¿Æ«Ø∑Ó∆¸
           03  JYURRK-CREYMD               PIC  X(08).
      *ππø∑«Ø∑Ó∆¸
           03  JYURRK-UPYMD                PIC  X(08).
      *ππø∑ª˛¥÷
           03  JYURRK-UPHMS                PIC  X(06).
      *°°°°ºÁ•≠°ºππø∑Õ—π‡Ã‹
           03  JYURRK-UPKEY.
               05  JYURRK-UPHOSPID           PIC  X(24).
               05  JYURRK-UPPTID             PIC  9(10).
               05  JYURRK-UNYUGAIKBN         PIC  X(01).
               05  JYURRK-UPSRYKA            PIC  X(02).
               05  JYURRK-UPSRYYMD           PIC  X(08).
               05  JYURRK-UPRENNUM           PIC  9(01).
               05  JYURRK-UPDOUJI-RENNUM     PIC  9(01).
               05  JYURRK-UPKAIKEI-RENNUM    PIC  9(03).
               05  JYURRK-UPEDANUM           PIC  9(01).
