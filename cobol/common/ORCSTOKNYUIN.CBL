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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSTOKNYUIN.
      *****************************************************************
      *  •∑•π•∆•‡Ãæ        : £œ£“£√£¡
      *  •µ•÷•∑•π•∆•‡Ãæ    : ∆˛±°
      *  •≥•Û•›°º•Õ•Û•»Ãæ  : ∆√ƒÍ∆˛±°Œ¡•≥°º•…ºË∆¿•µ•÷
      *  ¥…Õ˝º‘            : 
      *  ∫Ó¿Æ∆¸…’    ∫Ó∂»º‘        µ≠Ω“
      *  08/08/08    NACL°›æÆ∆¶¬Ù  ø∑µ¨∫Ó¿Æ
      *****************************************************************
      *  •◊•Ì•∞•È•‡Ω§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
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
      *
      *****************************************************************
      *    •’•°•§•Î•Ï•§•¢•¶•»
      *****************************************************************
      *
      *
      *****************************************************************
      *    •µ•÷•◊•ÌÕ—°°ŒŒ∞Ë
      *****************************************************************
      *
      *
      *****************************************************************
      *    œ¢ÕÌ°°ŒŒ∞Ë
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…ºË∆¿•—•È•·•ø
           COPY    "CPORCSTOKNYUIN.INC".
      *
      *    •π•—ŒŒ∞Ë
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTOKNYUIN-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ºÁ°°°°ΩËÕ˝
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    …¬≈Ô
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                æÆª˘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                æÆª˘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                ∆√ºÏº¿¥µ…¬≈Ô∆˛±°Œ¡£±
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                ∆√ºÏº¿¥µ…¬≈Ô∆˛±°Œ¡£≤
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                «ß√Œæ……¬≈Ô∆˛±°Œ¡£±
                       MOVE   "190739910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                œ∑øÕ¿≠«ß√Œæ…º¿¥µŒ≈Õ‹…¬≈Ô∆˛±°Œ¡
                       MOVE   "190740110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                ¿∫ø¿Œ≈Õ‹…¬≈Ô∆˛±°Œ¡
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                ¿∫ø¿Œ≈Õ‹…¬≈Ô∆˛±°Œ¡£≤
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                ¥Àœ¬•±•¢…¬≈Ô∆˛±°Œ¡
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                ¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡
                       MOVE   "190111410"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                ¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£±
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "11"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                ¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±
                       MOVE   "190066910"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                ¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "12"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                ¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
                       MOVE   "190067010"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                ¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "13"
      *                ≤Û…¸¥¸•Í•œ•”•Í•∆°º•∑•Á•Û…¬≈Ô∆˛±°Œ¡
                       MOVE   "190075910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                æÆª˘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                «ß√Œæ……¬≈Ô∆˛±°Œ¡£≤
                       MOVE   "190813410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                ≤Û…¸¥¸•Í•œ•”•Í•∆°º•∑•Á•Û…¬≈Ô∆˛±°Œ¡° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190124910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                æÆª˘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                ≤Û…¸¥¸•Í•œ•”•Í•∆°º•∑•Á•Û…¬≈Ô∆˛±°Œ¡£±
                       MOVE   "190129410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                ≤Û…¸¥¸•Í•œ•”•Í•∆°º•∑•Á•Û…¬≈Ô∆˛±°Œ¡£±° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190129510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                ≤Û…¸¥¸•Í•œ•”•Í•∆°º•∑•Á•Û…¬≈Ô∆˛±°Œ¡£≤
                       MOVE   "190129610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                ≤Û…¸¥¸•Í•œ•”•Í•∆°º•∑•Á•Û…¬≈Ô∆˛±°Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190129710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                ¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£≤
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                ¿∫ø¿≤ µﬂµﬁ°¶πÁ ªæ…∆˛±°Œ¡
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                ¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±° µﬁ¿≠¡˝∞≠§Œ∫∆∆˛±°°À
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                ¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤° µﬁ¿≠¡˝∞≠§Œ∫∆∆˛±°°À
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    …¬ºº
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                ∆√ºÏº¿¥µ∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                ø«Œ≈ΩÍ∏Â¥¸π‚Œº‘∞ÂŒ≈¥…Õ˝Œ¡
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                µﬂÃøµﬂµﬁ∆˛±°Œ¡£±
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                µﬂÃøµﬂµﬁ∆˛±°Œ¡£±° π‚≈Ÿ°À
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                µﬂÃøµﬂµﬁ∆˛±°Œ¡£±° £¡°À
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤° π‚≈Ÿ°À
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤° £¡°À
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° Ω≈æ…º‘≈˘»ÊŒ®¥Ω‡≈¨πÁ°À
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                ¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°  Ï¬Œ°¶¬€ª˘°À
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                ¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° ø∑¿∏ª˘°À
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                ∞ÏŒ‡¥∂¿˜æ…¥µº‘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                œ∑øÕ∞Ï»Ã…¬≈Ô∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                •œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                ∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                «æ¬¥√Ê•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£±
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≤
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                ∏Â¥¸π‚Œº‘∆√ƒÍ∆˛±°¥À‹Œ¡
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                ø«Œ≈ΩÍ∏Â¥¸π‚Œº‘∞ÂŒ≈¥…Õ˝Œ¡° £≥£∞∆¸§Ú∑–≤·§∑§ §§∆¸§Œ∫∆∆˛±°°À
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                ∏Â¥¸π‚Œº‘∆√ƒÍ∆˛±°¥À‹Œ¡° ∏Â¥¸π‚Œº‘∆√ Ã∆˛±°¥À‹Œ¡ªªƒÍ¥µº‘°À
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡£¡
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡£¬
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡£√
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡£ƒ
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡£≈
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡° ∆√ Ã∆˛±°¥À‹Œ¡°À
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡£¡
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡£¬
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡£√
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡£ƒ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡£≈
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡° ∆√ Ã∆˛±°¥À‹Œ¡°À
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡£¡° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡£¬° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡£√° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡£ƒ° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡£≈° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡° ∆√ Ã∆˛±°¥À‹Œ¡°À° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡£¡° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡£¬° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡£√° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡£ƒ° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡£≈° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡° ∆√ Ã∆˛±°¥À‹Œ¡°À° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                ø«Œ≈ΩÍ∏Â¥¸π‚Œº‘∞ÂŒ≈¥…Õ˝Œ¡
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                ø«Œ≈ΩÍ∏Â¥¸π‚Œº‘∞ÂŒ≈¥…Õ˝Œ¡° £≥£∞∆¸Ã§∑–≤·§Œ∫∆∆˛±°°À° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≥
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≥° ¿∏≥ËŒ≈Õ‹°À
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                ∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
           IF    LNK-TOKNYUIN-SRYCD    =     SPACE
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
