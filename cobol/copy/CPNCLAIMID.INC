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
      ************************************************************
      *    Æþ±¡CLAIM¹àÌÜID  COPY (CPNCLAIMID.INC)                *
      ************************************************************
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ     ÆâÍÆ
      ************************************************************
       01  NCLAIMID-AREA.
           03  NCLAIMID-CLASS          PIC X(20)   VALUE   "Claim010".
           03  NCLAIMID-ORDER          PIC X(20)   VALUE   "Claim011".
           03  NCLAIMID-ITEM-VAL.
             05  NCLAIMID-NYUKBN-G.
               07  NCLAIMID-NYUKBN
                                PIC X(20)   VALUE   "Claim012".
               07  NCLAIMID-NYUKBNJMA
                                PIC X(05)   VALUE   "00001".
               07  NCLAIMID-NYUKBNMES
                                PIC X(100)  VALUE   "Æþ±¡¶èÊ¬".
             05  NCLAIMID-SHOKAI-G.
               07  NCLAIMID-SHOKAI
                                PIC X(20)   VALUE   "Claim013".
               07  NCLAIMID-SHOKAIJMA
                                PIC X(05)   VALUE   "00002".
               07  NCLAIMID-SHOKAIMES
                                PIC X(100)  VALUE   "·ÑÂ³Æþ±¡¶èÊ¬".
             05  NCLAIMID-BTUNUM-G.
               07  NCLAIMID-BTUNUM
                                PIC X(20)   VALUE   "Claim014".
               07  NCLAIMID-BTUNUMJMA
                                PIC X(05)   VALUE   "00003".
               07  NCLAIMID-BTUNUMMES
                                PIC X(100)  VALUE   "ÉÂÅï".
             05  NCLAIMID-BRMNUM-G.
               07  NCLAIMID-BRMNUM
                                PIC X(20)   VALUE   "Claim015".
               07  NCLAIMID-BRMNUMJMA
                                PIC X(05)   VALUE   "00004".
               07  NCLAIMID-BRMNUMMES
                                PIC X(100)  VALUE   "ÉÂ¼¼".
             05  NCLAIMID-SAGAKU-G.
               07  NCLAIMID-SAGAKU
                                PIC X(20)   VALUE   "Claim016".
               07  NCLAIMID-SAGAKUJMA
                                PIC X(05)   VALUE   "00005".
               07  NCLAIMID-SAGAKUMES
                                PIC X(100)  VALUE   "¼¼ÎÁº¹³Û".
             05  NCLAIMID-DR-G.
               07  NCLAIMID-DR
                                PIC X(20)   VALUE   "Claim017".
               07  NCLAIMID-DRJMA
                                PIC X(05)   VALUE   "00006".
               07  NCLAIMID-DRMES
                                PIC X(100)  VALUE   "Ã´Åö°å".
             05  NCLAIMID-TAIINCD-G.
               07  NCLAIMID-TAIINCD
                                PIC X(20)   VALUE   "MML0016".
               07  NCLAIMID-TAIINCDJMA
                                PIC X(05)   VALUE   "00007".
               07  NCLAIMID-TAIINCDMES
                                PIC X(100)  VALUE   "Âà±¡»öÍ³".
             05  NCLAIMID-TOKNYUIN-G.
               07  NCLAIMID-TOKNYUIN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-TOKNYUINJMA
                                PIC X(05)   VALUE  "00008".
               07  NCLAIMID-TOKNYUINMES
                                PIC X(100)  VALUE  "ÆÃÄêÆþ±¡ÎÁ".
             05  NCLAIMID-NYUINKA-G.
               07  NCLAIMID-NYUINKA
                                PIC X(20)   VALUE  "MML0028".
               07  NCLAIMID-NYUINKAJMA
                                PIC X(05)   VALUE  "00009".
               07  NCLAIMID-NYUINKAMES
                                PIC X(100)  VALUE  "Æþ±¡²Ê".
             05  NCLAIMID-HKNCOMBI-G.
               07  NCLAIMID-HKNCOMBI
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-HKNCOMBIJMA
                                PIC X(05)   VALUE  "00010".
               07  NCLAIMID-HKNCOMBIMES
                                PIC X(100)  VALUE  "ÊÝ¸±ÁÈ¹ç¤»".
             05  NCLAIMID-SKJTIME-G.
               07  NCLAIMID-SKJTIME
                                PIC X(20)   VALUE  "Claim018".
               07  NCLAIMID-SKJTIMEJMA
                                PIC X(05)   VALUE  "00011".
               07  NCLAIMID-SKJTIMEMES
                                PIC X(100)  VALUE  "¿©»ö»þ´Ö".
             05  NCLAIMID-NISSU-G.
               07  NCLAIMID-NISSU
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-NISSUJMA
                                PIC X(05)   VALUE  "00012".
               07  NCLAIMID-NISSUMES
                                PIC X(100)  VALUE  "°ì³çÆþÎÏ".
             05  NCLAIMID-TEIKIKBN-G.
               07  NCLAIMID-TEIKIKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-TEIKIKBNJMA
                                PIC X(05)   VALUE  "00013".
               07  NCLAIMID-TEIKIKBNMES
                                PIC X(100)  VALUE  "Äê´üÀÁµá¤Î»ØÄê".
             05  NCLAIMID-KAIGOKBN-G.
               07  NCLAIMID-KAIGOKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-KAIGOKBNJMA
                                PIC X(05)   VALUE  "00014".
               07  NCLAIMID-KAIGOKBNMES
                                PIC X(100)  VALUE  "²ð¸î¤«¤é¤Î°ÛÆ°".
             05  NCLAIMID-NYUINYMD-G.
               07  NCLAIMID-NYUINYMD
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-NYUINYMDJMA
                                PIC X(05)   VALUE  "00015".
               07  NCLAIMID-NYUINYMDMES
                                PIC X(100)  VALUE  "Æþ±¡Æü".
             05  NCLAIMID-GAIHAKUKBN-G.
               07  NCLAIMID-GAIHAKUKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-GAIHAKUKBNJMA
                                PIC X(05)   VALUE  "00016".
               07  NCLAIMID-GAIHAKUKBNMES
                                PIC X(100)  VALUE  "³°Çñ¶èÊ¬".
             05  NCLAIMID-SKJTYPE-G.
               07  NCLAIMID-SKJTYPE
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-SKJTYPEJMA
                                PIC X(05)   VALUE  "00017".
               07  NCLAIMID-SKJTYPEMES
                                PIC X(100)  VALUE  "¿©»ö¤Î¼ïÎà".
             05  NCLAIMID-MEDID-G.
               07  NCLAIMID-MEDID
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-MEDIDJMA
                                PIC X(05)   VALUE  "00018".
               07  NCLAIMID-MEDIDMES
                                PIC X(100)  VALUE  "°åÎÅ¶èÊ¬¹àÌÜÈÖ¹æ".
             05  NCLAIMID-MEDDATA-G.
               07  NCLAIMID-MEDDATA
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-MEDDATAJMA
                                PIC X(05)   VALUE  "00019".
               07  NCLAIMID-MEDDATAMES
                       PIC X(100)  VALUE  "°åÎÅ¶èÊ¬¹àÌÜ¿ÇÎÅÆü¤Î¾õÂÖ".
             05  NCLAIMID-MEDALL-G.
               07  NCLAIMID-MEDALL
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-MEDALLJMA
                                PIC X(05)   VALUE  "00020".
               07  NCLAIMID-MEDALLMES
                       PIC X(100)  VALUE  "°åÎÅ¶èÊ¬¹àÌÜÊ£¿ôÆüÀßÄê".
             05  NCLAIMID-ADLID-G.
               07  NCLAIMID-ADLID
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-ADLIDJMA
                                PIC X(05)   VALUE  "00021".
               07  NCLAIMID-ADLIDMES
                       PIC X(100)  VALUE  "£Á£Ä£Ì¹àÌÜÈÖ¹æ".
             05  NCLAIMID-ADLTEN-G.
               07  NCLAIMID-ADLTEN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-ADLTENJMA
                                PIC X(05)   VALUE  "00022".
               07  NCLAIMID-ADLTENMES
                       PIC X(100)  VALUE  "£Á£Ä£Ì¹àÌÜ¿ÇÎÅÆü¤ÎÅÀ¿ô".
             05  NCLAIMID-ADLALL-G.
               07  NCLAIMID-ADLALL
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-ADLALLJMA
                                PIC X(05)   VALUE  "00023".
               07  NCLAIMID-ADLALLMES
                       PIC X(100)  VALUE  "£Á£Ä£ÌÅÀ¿ôÊ£¿ôÆüÀßÄê".
             05  NCLAIMID-UNCHKECK-G.
               07  NCLAIMID-UNCHKECK
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-UNCHKECKJMA
                                PIC X(05)   VALUE  "99999".
               07  NCLAIMID-UNCHKECKMES
                                PIC X(100)  VALUE  "ÉÔÌÀ".
           03  NCLAIMID-ITEM-R          REDEFINES   NCLAIMID-ITEM-VAL.
             05  NCLAIMID-ITEM-OCC      OCCURS  24.
               07  NCLAIMID-ITEM        PIC X(20).
               07  NCLAIMID-ITEMJMA     PIC X(05).
               07  NCLAIMID-ITEMMES     PIC X(100).
           03  NCLAIMID-ITEM-MAX        PIC 9(03)   VALUE   24.
