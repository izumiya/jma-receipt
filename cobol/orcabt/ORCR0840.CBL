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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCR0840.
      *****************************************************************
      *  •∑•π•∆•‡Ãæ        : £œ£“£√£¡
      *  •µ•÷•∑•π•∆•‡Ãæ    : •Ï•ª•◊•»∆˛±°° œ´∫“°¶º´«Â¿’°À
      *  •≥•Û•›°º•Õ•Û•»Ãæ  : ø«Œ≈∂Ë ¨ Ã≈¿øÙΩ∏∑◊° œ´∫“°¶º´«Â¿’°À
      *  ¥…Õ˝º‘            : 
      *  ∫Ó¿Æ∆¸…’    ∫Ó∂»º‘        µ≠Ω“
      *  03/06/01    NaCl°›ÃÁœ∆    ø∑µ¨∫Ó¿Æ
      *****************************************************************
      *  •◊•Ì•∞•È•‡Ω§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      *  01.00.01   NACL-ÃÁœ∆     04/01/06  ø«Œ≈π‘∞Ÿ•≥°º•…§Œ£±∑ÂÃ‹§Œ»ΩƒÍ
      *                                     Ω§¿µ
      *  01.00.02    NACL-ÃÁœ∆    04.01.21  œ´∫“§Œ∆√ƒÍ¥Ô∫‡(09593)¬–±˛
      *  01.00.03    NACL-ÃÁœ∆    04.01.21  œ´∫“°¶º´«Â¿’§Œ∆˛±°ººŒ¡≤√ªª
      *                                     (09594)¬–±˛
      *  01.00.04    NACL-ÃÁœ∆    04/01/28  ø«Œ≈∂Ë ¨£π£∞°¢£π£≤§Œ•≥•·•Û•»
      *                                     §Œ§ﬂ§Œ∫ﬁ§‚•Ï•ªµ≠∫‹§Œ¬–æ›§À§π§Î
      *  01.00.05    NACL-ÃÁœ∆    04/02/05  ø«Œ≈•≥°º•…°ˆøÙŒÃ§ŒŒﬂ∑◊ ‘Ω∏
      *  01.00.06    NACL-ÃÁœ∆    04/02/06  º´«Â¿’§Œø©ªˆŒ≈Õ‹»Ò§Úœ´∫“
      *                                     §Œ£±°•£≤«‹§«∑◊ªª§π§Î
      *  £≤£∞£∞£¥«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  01.01.01    NACL-ÃÁœ∆    04/04/01  ÍÛ·Ï¥µº‘¥…Õ˝≤√ªª§Œ¬–±˛
      *  01.01.02    NACL-ÃÁœ∆    04/04/01  •Í•œ∏¿∏Ïƒ∞≥–Œ≈À°° £≥°Àƒ…≤√¬–±˛
      *  01.01.03    NACL-ÃÁœ∆    04/04/05  ∫ﬂ¬∑Ï±’∆©¿œ…—≤Ûªÿ∆≥¥…Õ˝Œ¡§Œ
      *                                     •≥•·•Û•»¬–±˛
      *  01.01.04    NACL-ÃÁœ∆    04/04/26  •Ê°º•∂°º•≥•·•Û•»ƒ…≤√¬–±˛
      *  01.01.05    NACL-ÃÁœ∆    04/05/20  ≈¿≈©§Œ¥Ô∫‡√±≤¡ ‘Ω∏
      *  01.01.06    NACL-æÆ∆¶¬Ù  04/06/08  £√£‘°¶£Õ£“£…° £≤≤ÛÃ‹∞ πﬂ°À§Œ
      *                                     ≈¶Õ◊ÕÛµ≠∫‹ΩÁΩ§¿µ
      *  01.01.07    NACL-ÃÁœ∆    04/07/28  ≈ÍÃÙŒ¡§Œ¥Ô∫‡≈¿§ŒΩ∏∑◊Ω§¿µ
      *  01.01.08    NACL-ÃÁœ∆    04/10/04  ≈¿≈©§Œø«Œ≈•≥°º•…°ˆøÙŒÃ§ŒŒﬂ∑◊ ‘Ω∏
      *  01.01.09    NACL-ÃÁœ∆    04/10/15  º´«Â¿’∆√ƒÍ¥Ô∫‡≈˘æ√»Ò¿«¬–±˛
      *  √ÊªﬂΩËÕ˝ƒ…≤√
      *  01.02.01    NACL-∆£∏∂    04/10/13  √ÊªﬂΩËÕ˝§Œƒ…≤√
      *  01.02.02    NACL-ÃÁœ∆    04/12/17  ∂ª…Ù∏«ƒÍ¬”≤√ªª°¢¡œ≥∞∏«ƒÍ¥Ô
      *                                     ª»Õ—≤√ªª§ÚÃÙ∫ﬁ≈˘§ÿ
      *  01.02.03    NACL-ÃÁœ∆    05/01/18  ª¿¡«∑◊ªªº∞•≥•·•Û•»¬–±˛
      *  01.02.04    NACL-ÃÁœ∆    05/01/26  •∑•π•∆•‡¥…Õ˝£¥£∞£∞£±§À¥ÿ§π§ÎΩ§¿µ
      *  01.02.05    NACL-ÃÁœ∆    05/01/27  π¯°¶∂ª…Ù∏«ƒÍ¬”≤√ªª°¢¡œ≥∞∏«ƒÍ¥Ô
      *                                     ª»Õ—≤√ªª§ŒµªΩ—°¶ÃÙ∫ﬁ≈˘§Œ¡™¬Ú¬–±˛
      *  01.02.06    NACL-ÃÁœ∆    05/01/28   ∏ΩÒŒ¡§Œ•≥•·•Û•»¬–±˛
      *  01.02.07    NACL-ÃÁœ∆    05/02/28  ° •µ°Àµ≠∫‹¬–±˛
      *  01.02.08    NACL-ÃÁœ∆    05/03/03  ∆√µ≠ªˆπ‡§Œ ‘Ω∏Ω§¿µ
      *  01.02.09    NACL-ÃÁœ∆    05/03/03  ø©ªˆ§À¥ÿ§π§Î ‘Ω∏Ω§¿µ
      *  01.02.10    NACL-ÃÁœ∆    05/05/16  Œ≈Õ‹√¥≈ˆºÍ≈ˆ° ÀÃ≥§∆ª°À¬–±˛
      *  01.02.11    NACL-ÃÁœ∆    05/05/19  ≈¿øÙ•ﬁ•π•ø§Œ√±∞Ã ∏ª˙øÙ§¨•º•Ì
      *                                     §ŒæÏπÁ§Œ ‘Ω∏
      *  01.02.12    NACL-æÆ∆¶¬Ù  05/05/20  ∆˛±°≈¿≈©ºÔ Ãƒ…≤√§À»º§¶Ω§¿µ
      *  01.02.13    NACL-ÃÁœ∆    05/07/15  º´«Â¿’ΩæÕËÕÕº∞¬–±˛
      *  01.02.14    NACL-ÃÁœ∆    05/08/22  •Í•œ•”•Íº¬ª‹≤ÛøÙ∫π§∑π˛§ﬂΩ§¿µ¬–±˛
      *  01.02.15    NACL-∆£∏∂    05/08/22  ∏°¬Œ∏°∫∫•≥•·•Û•»§Œƒ…≤√
      *  01.02.16    NACL-ÃÁœ∆    05/09/13  ∆˛±°≈¿≈©•≥•·•Û•»§À¥ÿ§π§ÎΩ§¿µ
      *  01.02.17    NACL-ÃÁœ∆    05/08/29  £∞£π£µ£π£≥§Œ•≥•·•Û•»¬–±˛
      *  01.02.18    NACL-ÃÁœ∆    05/10/06  ø«∂Ë£π£µ£π£∂¬–±˛
      *  01.02.19    NACL-æÆ∆¶¬Ù  05/10/28  ∆√µ≠ªˆπ‡°°¿Ëø °¢¿©ƒ∂°°¬–±˛
      *  01.02.20    NACL-ÃÁœ∆    05/11/01  ° ∏°¡™°À≈˘•≥•·•Û•»¬–±˛
      *  01.02.21    NACL-ÃÁœ∆    05/11/11  •≥•·•Û•»§¨§¢§ÎæÏπÁ§Œ ‘Ω∏Ω§¿µ
      *  01.02.22    NACL-ÃÁœ∆    05/11/15  Ω˝…¬ºÍ≈ˆ∂‚°¢Œ≈Õ‹»Ò∆±∞’ΩÒ§Œº´∆∞
      *                                     µ≠∫‹•’•È•∞ ‘Ω∏Ω§¿µ
      *  01.02.23    NACL-ÃÁœ∆    05/12/14  ΩÈø«ª˛¥÷∂Ë ¨ ‘Ω∏Ω§¿µ
      *  01.02.24    NACL-ÃÁœ∆    06/01/05  •∑•π¥…° £µ£∞£∞£∞°À§Œø©ªˆæ ÛºË
      *                                     ∆¿Ω§¿µ
      *  01.02.25    NACL-ÃÁœ∆    06/02/01  £∞£π£π£≤£∞£∞£∞§«ªœ§ﬁ§Î≈ÍÃÙÕ—
      *                                     •≥•·•Û•»§Œ ‘Ω∏¬–±˛
      *  01.02.26    NACL-ÃÁœ∆    06/02/07  ø©ªˆ¥À‹Œ¡æ Û° §Ω§Œ¬æ°À ‘Ω∏
      *
      *  £≤£∞£∞£∂«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  02.09.01    NACL-ÃÁœ∆    06/04/05  Œﬂ∑◊•»°º•ø•Î ‘Ω∏Ω§¿µ
      *  02.09.02    NACL-ÃÁœ∆    06/04/13  £≤£∞£∞£∂«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  02.09.03    NACL-∆£∏∂    06/04/14  §≠§∂§ﬂ√Õ∑◊ªªº± Ã§Œƒ…≤√
      *  02.09.04    NACL-ÃÁœ∆    06/04/20  ≥∞ÕËø◊¬Æ∏°¬Œ∏°∫∫≤√ªª ‘Ω∏
      *  02.09.05    NACL-ÃÁœ∆    06/05/09  Œﬂ∑◊•»°º•ø•Î ‘Ω∏Ω§¿µ
      *  02.09.06    NACL-ÃÁœ∆    06/06/02  ÍÛ·Ï¥µº‘¥…Õ˝≤√ªª ‘Ω∏¬–±˛
      *  02.09.07    NACL-ÃÁœ∆    06/06/05   £øÙ∞Âª’µ⁄§”¥«∏Ó«€√÷≈¨πÁ≤√ªª§»
      *                                     ¥«∏Ó«€√÷≈¨πÁ≤√ªª§Œ ‘Ω∏¬–±˛
      *  02.09.08    NACL-ÃÁœ∆    06/06/09  œ´∫“° ≤√ªª§ §∑°À§Ú∫ﬁ§À¥ﬁ§‡
      *                                     æÏπÁ§Œ ‘Ω∏Ω§¿µ° º´«Â¿’°À
      *  03.00.01    NACL-ÃÁœ∆    06/06/22  ±˝ø«°¢∫ﬂ¬¥µº‘À¨Ã‰ø«Œ≈Œ¡§Œ
      *                                     •≥•·•Û•»º´∆∞µ≠∫‹¬–±˛
      *  03.00.02    NACL-ÃÁœ∆    06/06/23  «ß√Œµ°«Ωæ„≥≤≤√ªª§Œ ‘Ω∏¬–±˛
      *  03.01.01    NACL-æÆ∆¶¬Ù  06.07.04  ∆˛±°Œ¡§À¥ﬁ§·§Î≤√ªª§¨•≥•·•Û•»
      *                                     §»∆±ª˛∆˛Œœ§µ§Ï§øæÏπÁ§Œ¬–±˛
      *  03.01.02    NACL-ÃÁœ∆    06/07/10  ∫ﬂªŸ±Á•≥•·•Û•»º´∆∞µ≠∫‹¬–±˛
      *  03.01.03    NACL-ÃÁœ∆    06/07/14  ø«Œ≈∂Ë ¨√÷§≠¥π§®•’•È•∞§Œ ‘Ω∏
      *  03.01.04    NACL-ÃÁœ∆    06/07/19  ø©ªˆπ‡Ã‹ ‘Ω∏Ω§¿µ
      *  03.02.01    NACL-ÃÁœ∆    06/08/10  π‚Œº‘æÂ∏¬≥€∑–≤·¡º√÷¬–±˛
      *  03.02.02    NACL-ÃÁœ∆    06/09/08  •’•§•Î•‡§Œ√±≤¡ ‘Ω∏Ω§¿µ
      *  03.03.01    NACL-ÃÁœ∆    06/09/27  ∆√µ≠ªˆπ‡£±£∂ƒπ£≤¬–±˛
      *  03.03.02    NACL-ÃÁœ∆    06/10/11  ∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò¬–±˛
      *  03.03.03    NACL-ÃÁœ∆    06/11/10  ±˝ø«§Œ•≥•·•Û•»º´∆∞µ≠∫‹æÚ∑ÔΩ§¿µ
      *  03.04.01    NACL-ÃÁœ∆    06/12/01  º´«Â¿’∑Ú ›Ω‡µÚ¬–±˛
      *  03.04.02    NACL-ÃÁœ∆    07/03/12   Ò≥Áø«Œ≈¬–±˛
      *  03.04.03    NACL-ÃÁœ∆    07/04/01  ∏Ω ™µÎ…’¬–±˛
      *  03.04.04    NACL-ÃÁœ∆    07/04/13  •Í•œ•”•Í¬–±˛
      *  03.04.05    NACL-ÃÁœ∆    07/05/11  œ´∫“°¶º´«Â¿’√±≤¡£±£∞±ﬂÃ§À˛¬–±˛
      *  03.04.06    NACL-ÃÁœ∆    07/05/15  ≈¿≈©§Œµ≠∫‹ ‘Ω∏Ω§¿µ
      *  03.04.07    NACL-ÃÁœ∆    07/06/07  ∆˛±°ººŒ¡≤√ªª§Œ•≥•·•Û•»¬–±˛
      *
      *  03.05.01    NACL-ÃÁœ∆    07/06/01  •∞•Î°º•◊ø«Œ≈¬–±˛
      *  03.05.02    NACL-∆£∏∂    07/06/01  realtime preview ΩËÕ˝ƒ…≤√
      *  03.05.03    NACL-ÃÁœ∆    07/06/26  º´«Â¿’ ø¿Æ£±£π«Ø£¥∑Ó≤˛¿µÕÕº∞¬–±˛
      *  03.05.04    NACL-ÃÁœ∆    07/07/17  ∏ƒ Ãª˛§Œ•∑•π•∆•‡¥…Õ˝ ‘Ω∏Ω§¿µ
      *  03.05.05    NACL-ÃÁœ∆    07/07/26  •Ï•ª§Àµ≠∫‹§∑§ §§•≥•·•Û•»¬–±˛
      *  03.05.06    NACL-ÃÁœ∆    07/09/11  ≈¿≈©ºÍµªΩ∏∑◊Ω§¿µ
      *  03.05.07    NACL-ÃÁœ∆    07/09/18  ≤Ë¡¸ø«√«§Œ≤√ªª° •«°º•ø∂Ë ¨°ßºÍµª°À
      *                                     §Œµ≠∫‹ΩÁ —ππ¬–±˛
      *  04.01.01    NACL-ÃÁœ∆    07/10/05  ∆˛±°≈¿≈©•≥•·•Û•»¬–±˛
      *  04.01.02    NACL-ÃÁœ∆    07/10/24  º´«Â¿’∆√ƒÍ¥Ô∫‡≈˘æ√»Ò¿«¬–æ›≥∞¿ﬂƒÍ
      *                                     π‡Ã‹øÙ≥»ƒ•¬–±˛
      *  04.01.03    NACL-ÃÁœ∆    07/11/01  ∏¯Ã≥∫“≥≤¬–±˛
      *  04.01.04    NACL-ÃÁœ∆    07/11/26  ∆√ƒÍ¥Ô∫‡æ¶… Ãæµ≠∫‹¬–±˛
      *  04.01.05    NACL-ÃÁœ∆    07/11/30  ¬æ ›∏±§À§∆ø«ª°Œ¡ªªƒÍ∫—§ŒÃææŒ√÷¥π
      *  04.01.06    NACL-ÃÁœ∆    07/12/19  ∆˛±°Œ¡Ã§ªªƒÍ∆¸¬–±˛
      *  04.01.07    NACL-ÃÁœ∆    08/01/15  ∏¯≥≤¬–±˛
      *  04.01.08    NACL-ÃÁœ∆    08/02/12  ΩË ˝Œ¡≈˘•´•¶•Û•»Ω§¿µ
      *  04.02.01    NACL-ÃÁœ∆    08/03/10  °˚°˚§À§∆∆˛±°Œ¡ªªƒÍ∫—§Œ•’•È•∞ ‘Ω∏Ω§¿µ
      *  04.02.02    NACL-ÃÁœ∆    08/03/24   ø¿Æ£≤£∞«Ø£¥∑Ó≤˛¿µ¬–±˛
      *                                     ° ∆√µ≠ªˆπ‡ƒ…≤√¬–±˛°ß£≤£∞°À
      *                                     ° ÃÙ∫ﬁ¥…Õ˝ªÿ∆≥Œ¡•≥°º•…ƒ…≤√°À
      *                                     ° «æ∑Ï¥…•Í•œ° £≥°À•≥°º•…ƒ…≤√°À
      *                                     ° ≤√£∏£∞˙»Øæ…∆¸µ≠∫‹«—ªﬂ°À
      *                                     ° ΩÈø«° ÃÎ¥÷°¶¡·ƒ´≈˘°À≤√ªªƒ…≤√°À
      *                                     ° ø«Œ≈∂Ë ¨£∂£¥°ß…¬Õ˝ø«√«¬–±˛°À
      *                                     ° ∫∆Ω¢œ´°ß±ﬂ —ππ¬–±˛°À
      *                                     ° •Í•œ°ß¿ÏÕ—•≥°º•…ƒ…≤√°À
      *                                     ° ∫ﬂªŸ…¬º´∆∞µ≠∫‹¬–±˛°À
      *  04.02.03    NACL-ÃÁœ∆    08/04/03  °˚°˚§À§∆∆˛±°Œ¡ªªƒÍ∫—§Œ•’•È•∞ ‘Ω∏Ω§¿µ
      *  04.02.04    NACL-ÃÁœ∆    08/04/30  ∆˛±°Œ¡§À¥ﬁ§·§Î≤√ªª¬–±˛
      *  04.02.05    NACL-ÃÁœ∆    08/06/13  ¬‡±°¡∞À¨Ã‰ªÿ∆≥Œ¡°¢¿∫ø¿≤ ¬‡±°¡∞À¨Ã‰
      *                                     ªÿ∆≥Œ¡§Œ•≥•·•Û•»¬–±˛
      *  04.02.06    NACL-ÃÁœ∆    08/06/20  ∫ﬂ¬ ‘Ω∏Ω§¿µ
      *
      *  04.03.01    NACL-ÃÁœ∆    08/07/08  ø«∂Ë£±£¥§Œ•≥•·•Û•»§Œ§ﬂ§Œ∫ﬁ
      *                                      ‘Ω∏¬–±˛
      *  04.03.02    NACL-ÃÁœ∆    08/07/11  ø«√«ΩÒÃ¿∫ŸΩÒŒ¡§Œ≈¶Õ◊ÕÛµ≠∫‹¬–±˛
      *  04.03.03    NACL-ÃÁœ∆    08/07/14  ≤æ•Ï•ª∫Ó¿Æ° ±°≥∞ΩË ˝¥ﬁ§‡°À¬–±˛
      *                                     ≈ÍÃÙ∫ﬁ≈¿£∞° ±°≥∞≈˘°Àµ≠∫‹
      *  04.03.04    NACL-ÃÁœ∆    08/08/19  ∆˛±°Œ¡Ã§ªªƒÍª˛§Œ•≥•·•Û•»¬–±˛
      *  04.03.05    NACL-ÃÁœ∆    08/09/25  º´«Â¿’£∞£π£µ£π£≥§ŒΩ∏∑◊¿Ë¬–±˛
      *  04.03.06    NACL-ÃÁœ∆    08/12/01  ∫ﬂ¬° ±°≥∞ ¨°À ‘Ω∏Ω§¿µ
      *  04.03.07    NACL-ÃÁœ∆    08/12/04  ≤æº˝«º•¡•ß•√•ØΩËÕ˝ƒ…≤√
      *  04.03.08    NACL-ÃÁœ∆    08/12/16   ø¿Æ£≤£±«Ø£±∑Ó≤˛¿µ¬–±˛
      *
      *  04.04.01    NACL-ÃÁœ∆    08/12/25   Ò≥Á¬–±˛
      *  04.04.02    NACL-ÃÁœ∆    09/02/04  ∏°∫∫§Œ•≥•·•Û•»º´∆∞µ≠∫‹¬–±˛
      *  04.04.03    NACL-ÃÁœ∆    09/04/13   ø¿Æ£≤£±«Ø£µ∑Ó≤˛¿µ¬–±˛
      *  04.04.04    NACL-ÃÁœ∆    09/06/23  ≈¿≈©¥Ô∫‡æ¶… Ãæµ≠∫‹Ω§¿µ
      *  04.04.05    NACL-ÃÁœ∆    09/11/05  π‚Œº‘¬æ ›∏±∆˛±°¬–±˛
      *  04.04.06    NACL-ÃÁœ∆    09/11/19  º´«Â¿’∑Ú ›Ω‡µÚŒ≈Õ‹√¥≈ˆºÍ≈ˆ¬–±˛
      *  04.04.07    NACL-ÃÁœ∆    09/12/04  ∫ﬁ§ﬁ§»§·π‡Ã‹√Õ§Œ∑◊ªªΩ§¿µ
      *  04.04.08    NACL-ÃÁœ∆    09/12/09  Ω–ÕËπ‚ªªƒÍ° ∫ﬁ°À§Œ¬–±˛
      *  04.04.09    NACL-ÃÁœ∆    10/01/13  ∫ﬁ§ﬁ§»§·π‡Ã‹√Õ§Œ∑◊ªªΩ§¿µ
      *  04.04.10    NACL-ÃÁœ∆    10/03/23  ≤˛¿µ¬–±˛° ≥∞«Ò∂Ë ¨£µ§Œ¬–±˛°À
      *                                             ° œ´∫“æËøÙ§À¥ÿ§Ô§Î≤√ªª¬–±˛°À
      *  04.04.11    NACL-ÃÁœ∆    10/03/25  ≤˛¿µ¬–±˛
      *                                     ° ÕΩÃÛ•≥°º•…∆…§ﬂ»Ù§–§∑ ‘Ω∏°À
      *  04.04.12    NACL-ÃÁœ∆    10/03/29  ≤˛¿µ¬–±˛
      *                                     ° ≤Ë¡¸ø«√«£≤≤ÛÃ‹∞ πﬂ∏∫ªª ‘Ω∏°À
      *                                     ° •≥°º•…ƒ…≤√°À
      *  04.04.13    NACL-ÃÁœ∆    10/03/31  ≤˛¿µ¬–±˛
      *                                     ° Ù…Ù∏«ƒÍ¬”≤√ªª§Œ¬–±˛°À
      *  04.04.14    NACL-ÃÁœ∆    10/04/02  ∆√ Ãœ´∫“…’≈∫¥«∏Ó§Œ¬–±˛
      *  04.04.15    NACL-ÃÁœ∆    10/04/06  ≤˛¿µ¬–±˛
      *                                     ° •≥•·•Û•»º´∆∞µ≠∫‹°À
      *  04.04.16    NACL-ÃÁœ∆    10/05/14  ∆˛±°Œ¡≤√ªª∫ﬁ»÷πÊ —¥π
      *  04.04.17    NACL-ÃÁœ∆    10/09/17  ≥∞«Òª˛§Œ≈¶Õ◊ÕÛµ≠∫‹Ω§¿µ
      *  04.04.18    NACL-ÃÁœ∆    10/12/10  ∫ﬁ§ﬁ§»§·π‡Ã‹√Õ§Œ∑◊ªªΩ§¿µ
      *
      *  04.05.01    NACL-∆£∏∂    09/07/14  æÆøÙ≈¿∞ ≤º£µ∑Â¬–±˛
      *  04.05.02    NACL-ÃÁœ∆    09/08/17  æÆøÙ≈¿∞ ≤º£µ∑Â¬–±˛
      *  04.05.03    NACL-ÃÁœ∆    09/09/24  •Ï•ª•◊•»µ≠∫‹§∑§ §§•≥•·•Û•»¬–±˛
      *  04.05.04    NACL-ÃÁœ∆    09/11/19  ÃÙ∫ﬁªªƒÍ§ §∑ª˛§Œ≈¶Õ◊ÕÛ∆±∞Ï∫ﬁ
      *                                     §ﬁ§»§·µ≠∫‹¬–±˛
      *  04.05.05    NACL-ÃÁœ∆    09/12/04  ø©ªˆ° ∆√ Ãø©°À§Œ¬æ ›∏±¬–±˛
      *  04.05.06    NACL-ÃÁœ∆    10/01/07  º£∏≥¬–±˛
      *  04.05.07    NACL-ÃÁœ∆    11/06/21  ≤Ë¡¸ø«√«°¶≈≈ª“≤Ë¡¸¥…Õ˝≤√ªª§Œ
      *                                     …ΩÕ—»÷πÊ ‘Ω∏
      *  04.05.08    NACL-ÃÁœ∆    11/11/04  µﬂµﬁ»¬¡˜ø«Œ≈Œ¡ªªƒÍ¬–±˛
      *  04.05.09    NACL-ÃÁœ∆    11/11/09  ∆±∆¸∫∆∆˛±°ø©ªˆ¬–±˛
      *  04.05.10    NACL-ÃÁœ∆    11/12/16  ¥Ô∫‡§Œ∂‚≥€§»√±∞Ã§Œ¬–±˛
      *  04.05.11    NACL-ÃÁœ∆    12/02/13  ≤ÛøÙÕÛ ‘Ω∏Ω§¿µ° ƒ¥∫ﬁ°¢À„∆«°¢ƒ¥¥°À
      *  04.05.12    NACL-ÃÁœ∆    12/04/02   ø¿Æ£≤£¥«Ø£¥∑Ó≤˛¿µ
      *                                     ≥∞«Ò∂Ë ¨£∂°•£∑§Œ¬–±˛
      *                                     °÷∫ﬂ¬°◊§Œ¬–±˛
      *                                     °÷ΩË√÷°◊§Œ¬–±˛
      *                                     °÷≤Ë¡¸ø«√«°◊§Œ¬–±˛
      *                                     °÷§Ω§Œ¬æ°◊§Œ¬–±˛
      *                                     °÷∆˛±°°◊§Œ¬–±˛
      *                                     •≥•·•Û•»∫ππ˛
      *  04.05.13    NACL-ÃÁœ∆    12/05/23  ∫∆Ω¢œ´Œ≈Õ‹ªÿ∆≥¥…Õ˝Œ¡§ŒΩ∏∑◊¿Ë —ππ
      *  04.05.14    NACL-ÃÁœ∆    12/05/23   ›∏±ÕÛ°›∞Ï…Ù…È√¥∂‚Ã§µ≠∫‹¬–±˛
      *  04.05.15    NACL-ÃÁœ∆    12/08/10  £∏£∞§Ω§Œ¬æÕÛ§Œ≈¿øÙΩ∏∑◊Ω§¿µ
      *  04.05.16    NACL-ÃÁœ∆    12/09/18  ≤˛¿µ¬–±˛° ≥∞«Ò∂Ë ¨£∏°§£π§Œ¬–±˛°À
      *  04.05.17    NACL-ÃÁœ∆    12/09/19   ›∏±ÕÛ°›∞Ï…Ù…È√¥∂‚ÕÛ∂‚≥€√÷¥π¬–±˛
      *  04.05.18    NACL-ÃÁœ∆    12/11/08  ∏¯»ÒÕÛ°›∞Ï…Ù…È√¥∂‚ÕÛ∂‚≥€√÷¥π¬–±˛
      *
      *  04.06.01    NACL-ÃÁœ∆    10/07/01  º´«Â¿’ ∏ΩÒŒ¡•Ï•ª•◊•»≈¶Õ◊ÕÛµ≠∫‹
      *                                     ° £±ƒÃª˛°À¬–±˛
      *  04.06.02    NACL-ÃÁœ∆    10/08/16  ∆˛±°§Œ≈¿≈©∆˛Œœ≤˛¡±¬–±˛
      *  04.06.03    NACL-ÃÁœ∆    10/12/06  œ´∫“°¶º´«Â¿’§Œ∂‚≥€•’•Í°º∆˛Œœ
      *                                     •≥°º•…¬–±˛
      *  04.06.04    NACL-ÃÁœ∆    12/10/15  ø«Œ≈π‘∞Ÿ∆˛Œœ§∑§ø•≥•·•Û•»§Œ§ﬂ§Œ∫ﬁ§Œ
      *                                     •Ï•ª•◊•»µ≠∫‹¬–±˛
      *  04.06.05    NACL-ÃÁœ∆    13/02/18  œ´∫“•Ï•ª≈≈¬–±˛
      *                                     ° ø∑ø«Œ≈π‘∞Ÿ•≥°º•…¬–±˛°À
      *  04.06.06    NACL-ÃÁœ∆    13/02/27   ø¿Æ£≤£µ«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  04.06.07    NACL-ÃÁœ∆    13/06/12  œ´∫“°¶º´«Â¿’§Œ
      *                                     ¥«∏ÓŒ¡≥‰¡˝≤√ªª¬–±˛
      *  04.06.08    NACL-ÃÁœ∆    13/07/26  ∂‚≥€ÕÛΩ∏∑◊ ¨§Œ•≥•·•Û•» ‘Ω∏Ω§¿µ
      *  04.06.09    NACL-ÃÁœ∆    13/11/07  ∏¯Ã≥∫“≥≤° œ´∫“Ω‡µÚ°À§Œ√œ∏¯∫“∆√ Ã
      *                                     ≤√ªª¬–±˛° µ˛≈‘…‹°À
      *
      *  04.07.01    NACL-ÃÁœ∆    12/08/28  ÃÙ∫ﬁŒ¡∏∫≈¿¬–±˛
      *  04.07.02    NACL-ÃÁœ∆    12/09/25  •≥•·•Û•»∫ππ˛¬–±˛° ∫ﬂ¬ª˝¬≥Õ€∞µ°À
      *  04.07.03    NACL-ÃÁœ∆    12/09/25  •≥•·•Û•»∫ππ˛ºË§Íªﬂ§·° ∫ﬂ¬ª˝¬≥Õ€∞µ°À
      *  04.07.04    NACL-ÃÁœ∆    13/01/21  œ´∫“•Ï•ª≈≈¬–±˛
      *  04.07.05    NACL-ÃÁœ∆    13/07/01  ∆˛±°Œ¡¬æ ›∏±ªªƒÍª˛•≥•·•Û•»µ≠∫‹¬–±˛
      *  04.07.06    NACL-ÃÁœ∆    13/08/20  £∞£π£µ£π£≥§«ªœ§ﬁ§Î•≥°º•…§ŒÕ≠Ãµ∂Ë ¨
      *                                      ‘Ω∏
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
           SELECT  RECE84-FILE     ASSIGN  RECE84PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE84-KEY
                                   FILE    STATUS  IS  STS-RECE84.
           SELECT  RECE8A-FILE     ASSIGN  RECE8APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE8A-KEY
                                   FILE    STATUS  IS  STS-RECE8A.
           SELECT  RECE85-FILE     ASSIGN  RECE85PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE85-KEY
                                   FILE    STATUS  IS  STS-RECE85.
           SELECT  RECE8B-FILE     ASSIGN  RECE8BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE8B-KEY
                                   FILE    STATUS  IS  STS-RECE8B.
      *    •Ï•ª≈≈Õ—
           SELECT  RECE84C-FILE    ASSIGN  RECE84CPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE84C-KEY
                                   FILE    STATUS  IS  STS-RECE84C.
           SELECT  RECE85C-FILE    ASSIGN  RECE85CPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE85C-KEY
                                   FILE    STATUS  IS  STS-RECE85C.
      *    •®•È°º•’•°•§•Î
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC. 
           COPY    "CPRCF082.INC".
      *
       FD  RECE84-FILE.
       01  RECE84-REC. 
           COPY    "CPRCF084.INC".
      *
       FD  RECE8A-FILE.
       01  RECE8A-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE8A//.
      *
       FD  RECE85-FILE.
       01  RECE85-REC. 
           COPY    "CPRCF085.INC".
      *
       FD  RECE8B-FILE.
       01  RECE8B-REC. 
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //RECE8B//.
      *
      *    •Ï•ª≈≈Õ—
       FD  RECE84C-FILE.
       01  RECE84C-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE84C//.
      *
       FD  RECE85C-FILE.
       01  RECE85C-REC. 
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //RECE85C//.
      *
      *    •®•È°º•’•°•§•Î
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE82//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE84//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE8A//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE85//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE8B//.
      *    •Ï•ª≈≈Õ—
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE84C//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE85C//.
      *    •®•È°º•’•°•§•Î ÃææŒŒŒ∞Ë 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    •’•È•∞ŒŒ∞Ë
       01  STS-AREA.
           03  STS-RECE82                              PIC X(02).
           03  STS-RECE84                              PIC X(02).
           03  STS-RECE85                              PIC X(02).
           03  STS-RECE8A                              PIC X(02).
           03  STS-RECE8B                              PIC X(02).
           03  STS-RECE84C                             PIC X(02).
           03  STS-RECE85C                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-SP                                  PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-NYUINACT                            PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-ARI2                                PIC 9(01).
           03  FLG-DAY21                               PIC 9(01).
           03  FLG-DAY24                               PIC 9(01).
           03  FLG-TENTEKI                             PIC 9(01).
           03  FLG-WRITE85                             PIC 9(01).
           03  FLG-WRITE85X                            PIC 9(01).
           03  FLG-SIKIBETU                            PIC 9(01).
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-FORMAL-KISAI                        PIC 9(01).
           03  FLG-RECE85-ZAN                          PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-OKIKAE                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-HEYA                                PIC 9(01).
           03  FLG-KHN                                 PIC 9(01).
           03  FLG-NYUINRYO                            PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-HOUSIJI-COM                         PIC 9(01).
           03  FLG-OSIN-COM                            PIC 9(01).
           03  FLG-SYOBYO-COM                          PIC 9(01).
           03  FLG-RYOYO-COM                           PIC 9(01).
           03  FLG-SYODOU-COM                          PIC 9(01).
           03  FLG-KENSA-COM                           PIC 9(01).
           03  FLG-TAIGAI                              PIC 9(01).
           03  FLG-RINYAK                              PIC 9(01).
           03  FLG-KHNRYO-SKJ                          PIC 9(01).
           03  FLG-TAISYO                              PIC 9(01).
           03  FLG-TENTEKIMATOME                       PIC 9(01).
           03  FLG-SRYACCTPLUS                         PIC 9(01).
           03  FLG-OSINKBN                             PIC 9(01).
           03  FLG-ZAITAKU                             PIC 9(01).
           03  FLG-TAHOKEN                             PIC 9(01).
           03  FLG-HOUSEI-COM                          PIC 9(01).
           03  FLG-HOUMON-COM                          PIC 9(01).
           03  FLG-SRYKBN-HEN                          PIC 9(01).
           03  FLG-SYUNOU-ARI                          PIC 9(01).
           03  FLG-JYOGAI                              PIC 9(01).
           03  FLG-DOJITU                              PIC 9(01).
           03  FLG-HENSYU-NG                           PIC 9(01).
           03  FLG-SETCOM                              PIC 9(01).
           03  FLG-HENSYU-OK                           PIC 9(01).
      *
      *    •´•¶•Û•»ŒŒ∞Ë
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-COMMIT                           PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-OUT3                                PIC 9(06).
           03  CNT-OUT4                                PIC 9(06).
           03  CNT-OUT5                                PIC 9(06).
           03  CNT-OUT6                                PIC 9(06).
           03  CNT-OUT7                                PIC 9(06).
           03  CNT-DAY21                               PIC 9(06).
           03  CNT-DAY24                               PIC 9(06).
      *
      *    ≈∫ª˙ŒŒ∞Ë
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX5                                    PIC 9(03).
           03  IDX6                                    PIC 9(03).
           03  IDX7                                    PIC 9(03).
           03  IDX8                                    PIC 9(03).
           03  IDX9                                    PIC 9(03).
           03  IDX10                                   PIC 9(03).
           03  IDX11                                   PIC 9(02).
           03  IDX12                                   PIC 9(02).
           03  IDX13                                   PIC 9(02).
           03  IDX14                                   PIC 9(02).
           03  IDXA                                    PIC 9(01).
           03  IDXB                                    PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDY2                                    PIC 9(03).
           03  IDY3                                    PIC 9(03).
           03  IDZ                                     PIC 9(03).
           03  IDW                                     PIC 9(03).
           03  IDW3                                    PIC 9(03).
           03  IDW4                                    PIC 9(03).
           03  IDW5                                    PIC 9(03).
           03  IDW6                                    PIC 9(03).
           03  ID1                                     PIC 9(03).
           03  ID2                                     PIC 9(03).
           03  ID3                                     PIC 9(03).
           03  ID4                                     PIC 9(03).
           03  IDU                                     PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  SYUKEI-IDX                              PIC 9(03).
           03  SYUKEI-IDX2                             PIC 9(03).
           03  SYUKEI-IDX3                             PIC 9(03).
           03  SYUKEI-IDX4                             PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
           03  SYSKAN-IDX2                             PIC 9(03).
           03  IDV                                     PIC 9(02).
           03  IDV2                                    PIC 9(03).
           03  FUKA-IDX                                PIC 9(03).
           03  IDYY                                    PIC 9(02).
           03  IDXX                                    PIC 9(02).
      *
      *    ¥À‹≈¿øÙ§Œ»Û…Ωº®»ΩƒÍÕ—•’•È•∞° º´«Â¿’Õ—°À
       01  FLG-KHNTEN-JI-AREA.                                          
           03  FLG-KHNTEN7-JI                          PIC 9(01).       
           03  FLG-KHNTEN8-JI                          PIC 9(01).       
           03  FLG-KHNTEN9-JI                          PIC 9(01).       
      *
      *    ∫ﬁæ Û¬‡»ÚŒŒ∞Ë
       01  WK-ZAIKAISU                                 PIC S9(07).
       01  WK-ZAITEN                                   PIC S9(08).
       01  WK-SYUTEN                                   PIC S9(08).
       01  WK-KIZAITEN                                 PIC S9(08).
       01  WK-YKZTEN                                   PIC S9(08).
       01  WK-YKZTEN2                                  PIC S9(08).
      *
      *    •®•È°º•·•√•ª°º•∏ ‘Ω∏ŒŒ∞Ë
       01  ERR-EDIT-AREA.
           03  ERR-PTNUM                               PIC X(20).
           03  ERR-ERRMSG                              PIC X(80).
           03  ERR-HEN-ERRMSG                          PIC X(200).
      *
      *    ≈¿øÙ•∆°º•÷•Î∏°∫˜Õ—¬–æ›∆¸ŒŒ∞Ë
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY                            PIC 9(02).
           03  WK-TAIHI-FLG                            PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM                      PIC 9(06).
               05  WK-TAIHI-SRYD                       PIC X(02).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY4X                           PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
      *
       01  WRK-DAY-TABLE2.
           03  WRK-DAY-T2                              OCCURS   31.
               05  WRK-DAY21                           PIC 9(04).
               05  WRK-DAY24                           PIC 9(04).
      *
       01  WRK-SKJ-TABLE.
           03  WRK-SKJ-T                               OCCURS   31.
               05  WRK-SKJ-ZAITEN                      PIC 9(08).
               05  WRK-SKJ-ZAIKAISU                    PIC 9(04).
               05  WRK-SKJ-SRYCD-T.
                06  WRK-SKJ-SRYCD-OCC                  OCCURS   15.
                 07  WRK-SKJ-SRYCD                     PIC 9(09).
      *
       01  WRK-SKJ-TABLE2.
           03  WRK-SKJ-T2                              OCCURS   31.
               05  WRK-SKJ2-ZAITEN                     PIC 9(08).
               05  WRK-SKJ2-ZAIKAISU                   PIC 9(04).
               05  WRK-SKJ2-MEISAISU                   PIC 9(04).
               05  WRK-SKJ2-SRYCDTOTAL                 PIC 9(14).
               05  WRK-SKJ2-SRYCD-T.
                06  WRK-SKJ2-SRYCD-OCC                 OCCURS   15.
                 07  WRK-SKJ2-SRYCD                    PIC 9(09).
               05  WRK-SKJ2-KAISU-T.
                06  WRK-SKJ2-KAISU-OCC                 OCCURS   31.
                 07  WRK-SKJ2-KAISU                    PIC 9(01).
      *
       01  WRK-SKJ200604-TABLE.
           03  WRK-SKJ200604-T                         OCCURS   31.
               05  WRK-SKJ200604-KBN                   PIC 9(01).
               05  WRK-SKJ200604-SRYCDTOTAL            PIC 9(14).
               05  WRK-SKJ200604-MEISAISU              PIC 9(02).
               05  WRK-SKJ200604-SURYOUTOTAL           PIC 9(02).
               05  WRK-SKJ200604-T2                    OCCURS    3.
                   07  WRK-SKJ200604-DAY               PIC 9(04).
               05  WRK-SKJ200604-T3                    OCCURS    9.
                   07  WRK-SKJ200604-SRYCD             PIC 9(09).
                   07  WRK-SKJ200604-SURYOU            PIC 9(02).
      *
               05  WRK-SKJ200604-SRYCDTOTAL-KAN        PIC 9(14).
               05  WRK-SKJ200604-MEISAISU-KAN          PIC 9(02).
               05  WRK-SKJ200604-SURYOUTOTAL-KAN       PIC 9(02).
               05  WRK-SKJ200604-DAY-KAN               PIC 9(01).
               05  WRK-SKJ200604-T4                    OCCURS    3.
                   07  WRK-SKJ200604-SRYCD-KAN         PIC 9(09).
                   07  WRK-SKJ200604-SURYOU-KAN        PIC 9(02).
      *
       01  WRK-SKJ-DOJITU-TABLE.
           03  WRK-SKJ-DOJITU-T                        OCCURS   31.
               05  WRK-SKJ-DOJITU-KBN                  PIC 9(01).
               05  WRK-SKJ-DOJITU-SHOKUDO              PIC 9(04).
               05  WRK-SKJ-DOJITU-ASA                  PIC 9(04).
               05  WRK-SKJ-DOJITU-HIRU                 PIC 9(04).
               05  WRK-SKJ-DOJITU-YORU                 PIC 9(04).
      *
       01  WRK-NYULIFE-TABLE.
           03  WRK-NYULIFE-AREA                        OCCURS   31.
               05  WRK-NYULIFE-KBN                     PIC X(01).
      *
       01  WRK-TENTEKI2-TABLE.
      *
           03  WRK-TENTEKI2-HENKYAKUARI                PIC 9(01).
      *
           03  WRK-TENTEKI2-HENKYAKU-AREA.
      *
               05  WRK-TENTEKI2-ZAINUM                 PIC 9(08)
                                                       OCCURS   20.
      *
               05  WRK-TENTEKI2-T-AREA                 OCCURS   31.
                 06  WRK-TENTEKI2-T-MEISAISU           PIC 9(03).
                 06  WRK-TENTEKI2-T-ZAITEN             PIC 9(07).
                 06  WRK-TENTEKI2-T-SRYCDTOTAL         PIC 9(13).
                 06  WRK-TENTEKI2-T-SRYCD-X.
                   07  WRK-TENTEKI2-T-SRYCD-X-OCC      OCCURS    5.
                     08  WRK-TENTEKI2-T-SRYCD            PIC X(09).
                     08  WRK-TENTEKI2-T-COM-MEISAISU     PIC 9(03).
                     08  WRK-TENTEKI2-T-COM-AREA       OCCURS    5.
                         10  WRK-TENTEKI2-T-COM-FLG      PIC X(01).
                         10  WRK-TENTEKI2-T-COM-SRYCD    PIC X(09).
                         10  WRK-TENTEKI2-T-COM-MOJI     PIC X(100).
               05  WRK-TENTEKI2-T-ERRCD                PIC 9(01).
      *
               05  WRK-TENTEKI2-C-AREA                 OCCURS   31.
                 06  WRK-TENTEKI2-C-MEISAISU           PIC 9(03).
                 06  WRK-TENTEKI2-C-ZAITEN             PIC 9(07).
                 06  WRK-TENTEKI2-C-SRYCDTOTAL         PIC 9(13).
                 06  WRK-TENTEKI2-C-SRYCD-X.
                   07  WRK-TENTEKI2-C-SRYCD-X-OCC      OCCURS    5.
                     08  WRK-TENTEKI2-C-SRYCD            PIC X(09).
                     08  WRK-TENTEKI2-C-COM-MEISAISU     PIC 9(03).
                     08  WRK-TENTEKI2-C-COM-AREA       OCCURS    5.
                         10  WRK-TENTEKI2-C-COM-FLG      PIC X(01).
                         10  WRK-TENTEKI2-C-COM-SRYCD    PIC X(09).
                         10  WRK-TENTEKI2-C-COM-MOJI     PIC X(100).
               05  WRK-TENTEKI2-C-ERRCD                PIC 9(01).
      *
       01  WRK-ORCSTEN-TABLE.
           03  WRK-ORCSTEN-T                           OCCURS  350.
               05  WRK-ORCSTEN-SRYCD                   PIC X(09).
               05  WRK-ORCSTEN-SURYO                   PIC 9(05)V9(05).
               05  WRK-ORCSTEN-COM-MEI                 PIC X(80).
           03  WRK-ORCSTEN-T2                          OCCURS   31.
               05  WRK-ORCSTEN-DAY                     PIC 9(03).
           03  WRK-ORCSTEN-ZAIKAISU                    PIC 9(04).
      *
       01  WRK-CHUKSNSRYCD-TABLE.
           03  WRK-CHUKSNSRYCD                         PIC X(09)
                                                       OCCURS   50.
      *
      *    ∞ÂŒ≈µ°¥ÿ° ∆˛±°¥À‹°À¬‡»ÚÕ—ŒŒ∞Ë
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC     OCCURS  100.
               05  TBL-SYS5000                         PIC X(610).
      *    ∞ÂŒ≈µ°¥ÿ° ¥À‹æ Û°À¬‡»ÚÕ—ŒŒ∞Ë
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *    ª‹¿ﬂ¥Ω‡æ Û¬‡»ÚÕ—ŒŒ∞Ë
       01  TBL-SYS1006-T.
           03  TBL-SYS1006-OCC                         OCCURS  100.
               05  TBL-SYS10062                        PIC X(610).
               05  TBL-SYS10067                        PIC X(610).
      *
      *    ºÍµª≈¿°¶ÃÙ∫ﬁ≈˘≈¿øÙÕ—ŒŒ∞Ë
       01  TBL-TENSUG-JI.
           03  TBL-TENSU-JI                            OCCURS   50.
               05  TBL-SYUTEN-JI                       PIC 9(09).
               05  TBL-YKZTEN-JI                       PIC 9(09).
               05  TBL-YKZTEN-JI-C                     PIC 9(09).
      *
      *    ∞Ïª˛ŒŒ∞Ë
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-RECEKBN                    PIC X(01).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-JIBAIPRTKBN                    PIC X(01).
           03  WRK-PARA-PRTKBN                         PIC X(01).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-SRYYM-5000                          PIC X(06).
           03  WRK-SYS5000-COUNT                       PIC 9(02).
      *
           03  WRK-TEN                                 PIC 9(09).
           03  WRK-TEN-KG                              PIC 9(09).
           03  WRK-TEN-KG2                             PIC 9(09).
           03  WRK-MEISAI-CNT                          PIC 9(04).
           03  WRK-SRYCDTOTAL                          PIC 9(14).
           03  WRK-SURYOUTOTAL                         PIC 9(07)V9(05).
           03  WRK-KSNRATE                             PIC 9V99.    
           03  WRK-YOUBUKASAN                          PIC 9(07).
           03  WRK-KYOBUKASAN                          PIC 9(07).
           03  WRK-KEIBUKASAN                          PIC 9(07).
           03  WRK-SOGAIKASAN                          PIC 9(07).
           03  WRK-RYOYOTEATE                          PIC 9(07).
           03  WRK-RYOYOTEATE2                         PIC 9(07).
           03  WRK-ZAI-KINGAKU                         PIC 9(09).
      *
           03  WRK-HEYA-NINZU                          PIC 9(02).
      *
           03  WRK-SRYCD-X.
               05  WRK-SRYCD                           PIC 9(09).
           03  WRK-ZAINUM                              PIC 9(08).
           03  WRK-ZAINUM-CNT2                         PIC 9(03).
           03  WRK-RENNUM                              PIC 9(02).
           03  WRK-RENNUM2                             PIC 9(02).
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
      *
           03  WRK-SRYSURYO                            PIC 9(05)V9(05).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
           03  WRK-KZI058-TEN                          PIC 9(09)V99.
           03  WRK-KZI058-TANICD                       PIC 9(03).
           03  WRK-KZI058-TANIMOJISU                   PIC 9(01).
           03  WRK-KZI058-TANINAME                     PIC X(24).
      *
           03  WRK-SHOKUJIRYOYO-OCC.
               05  WRK-SHOKUJIRYOYO                    PIC 9(01)
                                                       OCCURS 31.
           03  WRK-TOKUKANRIKASAN-OCC.
               05  WRK-TOKUKANRIKASAN                  PIC 9(01)
                                                       OCCURS 31.
      *
           03  WRK-KEISAN                              PIC 9(09).
           03  WRK-KEISAN2                             PIC 9(09).
           03  WRK-KEISAN-KG                           PIC 9(09).
           03  WRK-KEISAN-KG2                          PIC 9(09).
      *
           03  WRK-SROUNYUIN-SRYCDTOTAL                PIC 9(14).
           03  WRK-SROUNYUIN-MEISAISU                  PIC 9(07).
           03  WRK-SROUNYUIN-SRYCD                     PIC 9(09).
      *
           03  WRK-TEN-KZI                             PIC 9(09).
           03  WRK-TEN-BUN                             PIC 9(09).
      *
      *    º´«Â¿’∆√ƒÍ¥Ô∫‡≈˘§Œ≈¿øÙ¬‡»Ú
           03  WRK-JIBAIKZI-TNS-TAIHI-TABLE.
               05  WRK-JIBAIKZI-TNS-TAIHI-OCC          OCCURS  5.
                 06  WRK-JIBAIKZI-TNS                  PIC 9(09).
                 06  WRK-JIBAIKZI-SYUKEIKBN            PIC 9(01).
      *
           03  WRK-SYUTEN-JI                           PIC S9(09).
           03  WRK-YKZTEN-JI                           PIC 9(09).
           03  WRK-KINGAKU-JI                          PIC 9(09).
      *
      *    æ√»Ò¿«√ºøÙ∂Ë ¨
           03  WRK-TAX1001-KBN                         PIC X(01).
      *
           03  WRK-SISETU613                           PIC 9(01).
           03  WRK-SISETU695                           PIC 9(01).
           03  WRK-SISETU3055                          PIC 9(01).
           03  WRK-SISETU3056                          PIC 9(01).
           03  WRK-SISETU3057                          PIC 9(01).
           03  WRK-SISETU3058                          PIC 9(01).
           03  WRK-SISETU3168                          PIC 9(01).
           03  WRK-SISETU3169                          PIC 9(01).
      *
      *    øÙª˙»æ≥—¡¥≥— —¥πÕ—     
           03  WRK-CHAR-IDX                    PIC 9(02).
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(20).
               05  WRK-CHAR-N                  PIC X(40).
               05  WRK-CHAR-CNT                PIC 9(02).
               05  WRK-CHAR-N-CNT              PIC 9(02).
      *     
      *    √±≤¡ ‘Ω∏Õ—     
           03  WRK-TANKA                       PIC 9(09)V9(02).
           03  WRK-TANKA-R  REDEFINES  WRK-TANKA.
               05  WRK-TANKA-S1                PIC 9(09).
               05  WRK-TANKA-S2                PIC 9(02).
           03  WRK-TANKA-X.
               05  WRK-TANKA-Z1                PIC ZZZZZZZZ9.
               05  WRK-TANKA-Z2                PIC X(01).
               05  WRK-TANKA-Z3                PIC ZZ.
      *
      ***  03  WRK-FUKA-KINGAKU                PIC 9(09).
           03  WRK-FUKA-KINGAKU                PIC 9(13)V9(05).
           03  WRK-FUKA-VALUE                  PIC 9(01).
      *
           03  WRK-ZAITENNASHI                 PIC 9(09).
      *
           03  WRK-TENTEKI2-SRYCD-X.
               05  WRK-TENTEKI2-SRYCD-X-OCC        OCCURS    5.
                 06  WRK-TENTEKI2-SRYCD            PIC X(09).
                 06  WRK-TENTEKI2-COM-MEISAISU     PIC 9(03).
                 06  WRK-TENTEKI2-COM-AREA         OCCURS    5.
                     08  WRK-TENTEKI2-COM-FLG      PIC X(01).
                     08  WRK-TENTEKI2-COM-SRYCD    PIC X(09).
                     08  WRK-TENTEKI2-COM-MOJI     PIC X(100).
      *
           03  WRK-TENTEKI2-SRYCD2-X.
               05  WRK-TENTEKI2-SRYCD2-X-OCC       OCCURS  100.
                 06  WRK-TENTEKI2-SRYCD2           PIC X(09).
                 06  WRK-TENTEKI2-COM-FLG2         PIC X(01).
                 06  WRK-TENTEKI2-COM-MOJI2        PIC X(100).
      *
           03  WRK-STR                         PIC X(200).
           03  WRK-LEN                         PIC 9(05).
           03  WRK-SHO                         PIC 9(05).
           03  WRK-AMARI                       PIC 9(05).
      *
           03  WRK-GENTEN-YKZTEN                   PIC 9(08).
      *
      *    øÙŒÃ ‘Ω∏Õ—
           03  WRK-SURYOKETA                   PIC 9(02).
           03  WRK-SURYOKETA1                  PIC 9(02).
           03  WRK-SURYOKETA2                  PIC 9(02).
           03  WRK-SURYOST                     PIC 9(02).
           03  WRK-SURYO                       PIC 9(05)V9(05).
           03  WRK-SURYO-R  REDEFINES  WRK-SURYO.
               05  WRK-SURYO-S1                PIC 9(05).
               05  WRK-SURYO-S2                PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1                PIC ZZZZZ.
               05  WRK-SURYO-Z2                PIC X(01).
               05  WRK-SURYO-Z3                PIC ZZZZZ.
           03  WRK-SURYO-N.
               05  WRK-SURYO-N1                PIC X(10).
               05  WRK-SURYO-N2                PIC X(02).
               05  WRK-SURYO-N3                PIC X(10).
           03  WRK-SRYSURYOW                   PIC X(24).
      *
           03  WRK-FORMALNAME                  PIC X(300).
      *
           03  WRK-SETCOM-JIDO                 PIC 9(03).
           03  WRK-SETCOM-JIDO2                PIC 9(03).
      *
           03  WRK-FUTANWARI                   PIC 9V99.
      *
           03  WRK-CHECK-SRYCD                 PIC X(09).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    •®•È°º•’•°•§•ÎÃææŒŒŒ∞Ë 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
      *    ∏«ƒÍ√Õ
       01  CONST-AREA.
      *    •’•Í°º•≥•·•Û•»
           03  CONS-COMMENT    PIC X(09)   VALUE   "810000001".
      *    ÃÙ∫ﬁŒ¡ƒ˛∏∫° £π£∞°ø£±£∞£∞°À° ∆‚…˛ÃÙ°À
           03  CONS-YAKTEIGEN  PIC X(09)   VALUE   "630010002".
      *     ¥∫’° •≥•·•Û•»°À
           03  CONS-FUNSAI-COMMENT PIC X(09) VALUE "099209901".
      *    ∫Œ∑Ï° £¬°›£¡°À
           03  CONS-SAIKETSU1      PIC X(09) VALUE "160101210".
      *    ∫Œ∑Ï° £¬°›£÷°À
           03  CONS-SAIKETSU2      PIC X(09) VALUE "160095710".
      *    ∫Œ∑Ï° £¬°›£√°À
           03  CONS-SAIKETSU3      PIC X(09) VALUE "160095810".
      *    ∆˝Õƒª˘≤√ªª
           03  CONS-NYUKSN1        PIC X(09) VALUE "111000370".
           03  CONS-NYUKSN2        PIC X(09) VALUE "111011470".
           03  CONS-NYUKSN3        PIC X(09) VALUE "111011970".
           03  CONS-NYUKSN4        PIC X(09) VALUE "111012070".
           03  CONS-NYUKSN5        PIC X(09) VALUE "111012170".
           03  CONS-NYUKSN6        PIC X(09) VALUE "111012270".
           03  CONS-NYUKSN7        PIC X(09) VALUE "111011570".
           03  CONS-NYUKSN8        PIC X(09) VALUE "111011670".
           03  CONS-NYUKSN9        PIC X(09) VALUE "111011770".
      *
      *    œ´∫“º´«Â¿’¥ÿ∑∏
           03  CONS-ROUSAI1    PIC X(09)   VALUE   "101110010".
           03  CONS-ROUSAI2    PIC X(09)   VALUE   "101110020".
           03  CONS-ROUSAI3    PIC X(09)   VALUE   "101130010".
           03  CONS-ROUSAI4    PIC X(09)   VALUE   "101130020".
           03  CONS-ROUSAI5    PIC X(09)   VALUE   "101130030".
           03  CONS-ROUSAI6    PIC X(09)   VALUE   "101130040".
           03  CONS-ROUSAI7    PIC X(09)   VALUE   "101130050".
           03  CONS-ROUSAI8    PIC X(09)   VALUE   "101130060".
           03  CONS-ROUSAI9    PIC X(09)   VALUE   "101130070".
           03  CONS-ROUSAI10   PIC X(09)   VALUE   "101130080".
           03  CONS-ROUSAI11   PIC X(09)   VALUE   "101130090".
           03  CONS-ROUSAI12   PIC X(09)   VALUE   "101130100".
           03  CONS-ROUSAI13   PIC X(09)   VALUE   "101910010".
           03  CONS-ROUSAI14   PIC X(09)   VALUE   "101910020".
           03  CONS-ROUSAI15   PIC X(09)   VALUE   "101910030".
           03  CONS-ROUSAI16   PIC X(09)   VALUE   "101910040".
           03  CONS-ROUSAI17   PIC X(09)   VALUE   "101910070".
           03  CONS-ROUSAI18   PIC X(09)   VALUE   "101910080".
           03  CONS-ROUSAI19   PIC X(09)   VALUE   "101910090".
           03  CONS-ROUSAI20   PIC X(09)   VALUE   "101910100".
           03  CONS-ROUSAI21   PIC X(09)   VALUE   "101110040".
      *
           03  CONS-ROUSAI22   PIC X(09)   VALUE   "101800390".
           03  CONS-ROUSAI23   PIC X(09)   VALUE   "101800400".
           03  CONS-ROUSAI24   PIC X(09)   VALUE   "101800410".
           03  CONS-ROUSAI25   PIC X(09)   VALUE   "101800420".
           03  CONS-ROUSAI26   PIC X(09)   VALUE   "101800430".
           03  CONS-ROUSAI27   PIC X(09)   VALUE   "101800440".
           03  CONS-ROUSAI28   PIC X(09)   VALUE   "101800450".
           03  CONS-ROUSAI29   PIC X(09)   VALUE   "101800460".
      *
           03  CONS-ROUSAI30   PIC X(09)   VALUE   "101800870".
           03  CONS-ROUSAI31   PIC X(09)   VALUE   "101800470".
           03  CONS-ROUSAI32   PIC X(09)   VALUE   "101800480".
           03  CONS-ROUSAI33   PIC X(09)   VALUE   "101800490".
           03  CONS-ROUSAI34   PIC X(09)   VALUE   "101800500".
           03  CONS-ROUSAI35   PIC X(09)   VALUE   "101800510".
           03  CONS-ROUSAI36   PIC X(09)   VALUE   "101800520".
           03  CONS-ROUSAI37   PIC X(09)   VALUE   "101800530".
           03  CONS-ROUSAI38   PIC X(09)   VALUE   "101800540".
           03  CONS-ROUSAI39   PIC X(09)   VALUE   "101800960".
           03  CONS-ROUSAI40   PIC X(09)   VALUE   "101800970".
           03  CONS-ROUSAI41   PIC X(09)   VALUE   "101800980".
      *
           03  CONS-ROUSAI42   PIC X(09)   VALUE   "101800880".
      *
           03  CONS-ROUCOM1    PIC X(09)   VALUE   "840800001".
           03  CONS-ROUCOM2    PIC X(09)   VALUE   "840800002".
           03  CONS-ROUCOM3    PIC X(09)   VALUE   "820800001".
           03  CONS-ROUCOM4    PIC X(09)   VALUE   "820800002".
           03  CONS-ROUCOM5    PIC X(09)   VALUE   "820800003".
           03  CONS-ROUCOM6    PIC X(09)   VALUE   "820800004".
           03  CONS-ROUCOM7    PIC X(09)   VALUE   "830800001".
      *
      *    π¯…Ù∏«ƒÍ¬”≤√ªª
           03  CONS-YOUBUKASAN PIC X(09)   VALUE   "140037490".
      *    ∂ª…Ù∏«ƒÍ¬”≤√ªª
           03  CONS-KYOBUKASAN PIC X(09)   VALUE   "140040110".
      *    Ù…Ù∏«ƒÍ¬”≤√ªª
           03  CONS-KEIBUKASAN PIC X(09)   VALUE   "140052610".
      *    ¡œ≥∞∏«ƒÍ¥Ôª»Õ—≤√ªª
           03  CONS-SOGAIKASAN PIC X(09)   VALUE   "150266970".
      *    Œ≈Õ‹√¥≈ˆºÍ≈ˆ° ∆˛±°°À
           03  CONS-RYOYOTEATE PIC X(09)   VALUE   "199000510".
      *
      *    •’•£•Î•‡Œ¡° ∆˝Õƒª˘°À≤√ªª
           03  CONS-FILM-YOUJI-KASAN  
                                   PIC X(09) VALUE "799990070".
      *    ª¿¡« ‰¿µŒ®
           03  CONS-SANSO          PIC X(09) VALUE "770020070".
      *    π‚µ§∞µª¿¡«≤√ªª
           03  CONS-SANSO2         PIC X(09) VALUE "770030070".
      *    ªƒŒÃ«—¥˛
           03  CONS-ZANHAIKI       PIC X(09) VALUE "099309901".
      *
      *    ÃÙ∫ﬁ¥…Õ˝ªÿ∆≥Œ¡
           03  CONS-YAKUKAN-COMMENT1 PIC X(09)  VALUE  "113010710".
           03  CONS-YAKUKAN-COMMENT2 PIC X(09)  VALUE  "113010810".
           03  CONS-YAKUKAN-COMMENT3 PIC X(09)  VALUE  "120001610".
      *    ∆˛±°±…Õ‹ø©ªˆªÿ∆≥Œ¡
           03  CONS-NYUEIYO-COMMENT  PIC X(09)  VALUE  "113003310".
      *    ¬‡±°¡∞À¨Ã‰ªÿ∆≥Œ¡
           03  CONS-HOUMON-COMMENT1  PIC X(09)  VALUE  "190030310".
      *    ° œ∑°À¬‡±°¡∞À¨Ã‰ªÿ∆≥Œ¡
           03  CONS-HOUMON-COMMENT2  PIC X(09)  VALUE  "190723610".
      *    ¬‡±°¡∞À¨Ã‰ªÿ∆≥Œ¡ªªƒÍ∆¸
           03  CONS-HOUMON-COMMENT3  PIC X(09)  VALUE  "840000055".
      *    Ω˝…¬ºÍ≈ˆ∂‚∞’∏´ΩÒ∏Ú…’Œ¡
           03  CONS-SYOBYO-COMMENT   PIC X(09)  VALUE  "180000710".
      *    Ω˝…¬ºÍ≈ˆ∂‚∞’∏´ΩÒ∏Ú…’«Ø∑Ó∆¸
           03  CONS-SYOBYO2-COMMENT  PIC X(09)  VALUE  "840000011".
      *    ° ¡Í¬≥°ÀΩ˝…¬ºÍ≈ˆ∂‚∞’∏´ΩÒ∏Ú…’«Ø∑Ó∆¸
           03  CONS-SYOBYO3-COMMENT  PIC X(09)  VALUE  "840000012".
      *    Œ≈Õ‹»Ò∆±∞’ΩÒ∏Ú…’Œ¡
           03  CONS-RYOYO-COMMENT    PIC X(09)  VALUE  "113004310".
      *    Œ≈Õ‹»Ò∆±∞’ΩÒ∏Ú…’«Ø∑Ó∆¸
           03  CONS-RYOYO2-COMMENT   PIC X(09)  VALUE  "840000078".
      *    ¬‡±°ª˛…˛ÃÙªÿ∆≥≤√ªª
           03  CONS-TAIIN-COMMENT    PIC X(09)  VALUE  "113006270".
      *    À¨Ã‰¥«∏Óªÿº®Œ¡
           03  CONS-HOUSIJI-COMMENT  PIC X(09)  VALUE  "114008010".
      *    À¨Ã‰¥«∏Óªÿº®Œ¡ªªƒÍ∆¸
           03  CONS-HOUSIJI2-COMMENT PIC X(09)  VALUE  "840000073".
      *    ºÍΩ—∆¸•≥•·•Û•»
           03  CONS-SHUJUTU-COMMENT  PIC X(09)  VALUE  "840000042".
      *    ∆¨…Ù√±Ω„£√£‘ª£±∆
           03  CONS-CTMRI-COMMENT1   PIC X(09)  VALUE  "170011710".
      *    ∂Ì¥¥√±Ω„£√£‘ª£±∆
           03  CONS-CTMRI-COMMENT2   PIC X(09)  VALUE  "170011810".
      *    ªÕªË√±Ω„£√£‘ª£±∆
           03  CONS-CTMRI-COMMENT3   PIC X(09)  VALUE  "170011910".
      *    «æ¡Â£√£‘¬§±∆
           03  CONS-CTMRI-COMMENT4   PIC X(09)  VALUE  "170012110".
      *    Ù…Ù√±Ω„£√£‘ª£±∆
           03  CONS-CTMRI-COMMENT5   PIC X(09)  VALUE  "170015550".
      *    ∆¨…Ù√±Ω„£Õ£“£…ª£±∆
           03  CONS-CTMRI-COMMENT6   PIC X(09)  VALUE  "170015210".
      *    ∂Ì¥¥√±Ω„£Õ£“£…ª£±∆
           03  CONS-CTMRI-COMMENT7   PIC X(09)  VALUE  "170020110".
      *    ªÕªË√±Ω„£Õ£“£…ª£±∆
           03  CONS-CTMRI-COMMENT8   PIC X(09)  VALUE  "170022110".
      *    £√£‘°¢£Õ£“£…° ∆¨…Ù°À° £≤≤ÛÃ‹∞ πﬂ°À
           03  CONS-CTMRI-COMMENT9   PIC X(09)  VALUE  "170022290".
      *    £√£‘°¢£Õ£“£…° ∂Ì¥¥°À° £≤≤ÛÃ‹∞ πﬂ°À
           03  CONS-CTMRI-COMMENT10  PIC X(09)  VALUE  "170022390".
      *    £√£‘°¢£Õ£“£…° ªÕªË°À° £≤≤ÛÃ‹∞ πﬂ°À
           03  CONS-CTMRI-COMMENT11  PIC X(09)  VALUE  "170022490".
      *    ∆¨…Ù∆√ºÏ£√£‘ª£±∆
           03  CONS-CTMRI-COMMENT12  PIC X(09)  VALUE  "170023010".
      *    ∂Ì¥¥∆√ºÏ£√£‘ª£±∆
           03  CONS-CTMRI-COMMENT13  PIC X(09)  VALUE  "170023110".
      *    ªÕªË∆√ºÏ£√£‘ª£±∆
           03  CONS-CTMRI-COMMENT14  PIC X(09)  VALUE  "170023210".
      *    ∆¨…Ù∆√ºÏ£Õ£“£…ª£±∆
           03  CONS-CTMRI-COMMENT15  PIC X(09)  VALUE  "170023310".
      *    ∂Ì¥¥∆√ºÏ£Õ£“£…ª£±∆
           03  CONS-CTMRI-COMMENT16  PIC X(09)  VALUE  "170023410".
      *    ªÕªË∆√ºÏ£Õ£“£…ª£±∆
           03  CONS-CTMRI-COMMENT17  PIC X(09)  VALUE  "170023510".
      *    £√£‘ª£±∆° £±£∂ŒÛÃ§À˛•ﬁ•Î•¡•π•È•§•π∑øµ°¥Ô°À
           03  CONS-CTMRI-COMMENT18  PIC X(09)  VALUE  "170028610".
      *    £√£‘ª£±∆° £∂£¥ŒÛ∞ æÂ•ﬁ•Î•¡•π•È•§•π∑øµ°¥Ô°À
           03  CONS-CTMRI-COMMENT19  PIC X(09)  VALUE  "170033410".
      *    £Õ£“£…ª£±∆° £≥•∆•π•È∞ æÂ§Œµ°¥Ô°À
           03  CONS-CTMRI-COMMENT20  PIC X(09)  VALUE  "170033510".
      *    ¿∫ø¿≤ ¬‡±°¡∞À¨Ã‰ªÿ∆≥Œ¡
           03  CONS-HOUSEI-COMMENT   PIC X(09)  VALUE  "180018310".
      *    ¿∫ø¿≤ ¬‡±°¡∞À¨Ã‰ªÿ∆≥Œ¡ªªƒÍ∆¸
           03  CONS-HOUSEI-COMMENT2  PIC X(09)  VALUE  "840000079".
      *    ¿∫ø¿≤ ≥÷Œ•…¬ºº¥…Õ˝≤√ªª
           03  CONS-KAKURI-COMMENT   PIC X(09)  VALUE  "190107070".
      *    œ´∫“º£Œ≈∑◊≤Ë≤√ªª
           03  CONS-CHIRYO-COMMENT   PIC X(09)  VALUE  "101910050".
      *
      *    …¬∞·¬ﬂÕøŒ¡
           03  CONS-BYOITAIYO        PIC X(09)  VALUE  "101910060".
      *    ∫ﬂ¬∑Ï±’∆©¿œ…—≤Ûªÿ∆≥¥…Õ˝Œ¡
           03  CONS-HINKAI-COMMENT   PIC X(09)  VALUE  "114009410".
      *    ΩÈø«° ∆±∆¸ £øÙø«Œ≈≤ °À
           03  CONS-SYODOU-COMMENT   PIC X(09)  VALUE  "101110040".
      *     £ΩÈªªƒÍø«Œ≈≤ °ß
           03  CONS-SYODOU2-COMMENT  PIC X(09)  VALUE  "830000024".
      *    ΩÈø«° ∆±∆¸ £øÙø«Œ≈≤ °À
           03  CONS-SYODOU3-COMMENT  PIC X(09)  VALUE  "111011810".
      *    ΩÈø«Œ¡° ∆±∞Ï∆¸£≤≤ Ã‹°ßæ“≤æı§ §∑°À
           03  CONS-SYODOU4-COMMENT  PIC X(09)  VALUE  "111012610".
      *    ø¥¬Á∑Ï¥…º¿¥µ•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-SINDAI1-COMMENT  PIC X(09)  VALUE  "180027410".
      *    ø¥¬Á∑Ï¥…º¿¥µ•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-SINDAI2-COMMENT  PIC X(09)  VALUE  "180027510".
      **   ø¥¬Á∑Ï¥…º¿¥µ•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À° £±£≤£∞∆¸ƒ∂°À
           03  CONS-SINDAI3-COMMENT  PIC X(09)  VALUE  "180029110".
      **   ø¥¬Á∑Ï¥…º¿¥µ•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À° £±£≤£∞∆¸ƒ∂°À
           03  CONS-SINDAI4-COMMENT  PIC X(09)  VALUE  "180029210".
      **   ø¥¬Á∑Ï¥…º¿¥µ•Í•œ•”•Í•∆°º•∑•Á•Û∞Â≥ÿ¥…Õ˝Œ¡° £±°À
           03  CONS-SINDAI5-COMMENT  PIC X(09)  VALUE  "180029910".
      **   ø¥¬Á∑Ï¥…º¿¥µ•Í•œ•”•Í•∆°º•∑•Á•Û∞Â≥ÿ¥…Õ˝Œ¡° £≤°À
           03  CONS-SINDAI6-COMMENT  PIC X(09)  VALUE  "180030010".
      *!   ø¥¬Á∑Ï¥…º¿¥µ•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-SINDAI7-COMMENT  PIC X(09)  VALUE  "101800080".
      *!   ø¥¬Á∑Ï¥…º¿¥µ•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-SINDAI8-COMMENT  PIC X(09)  VALUE  "101800090".
      *#   ø¥¬Á∑Ï¥…º¿¥µ•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-SINDAI9-COMMENT  PIC X(09)  VALUE  "101800230".
      *#   ø¥¬Á∑Ï¥…º¿¥µ•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-SINDAI10-COMMENT PIC X(09)  VALUE  "101800240".
      *    ∏∆µ€¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-KOKYUKI1-COMMENT PIC X(09)  VALUE  "180028010".
      *    ∏∆µ€¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-KOKYUKI2-COMMENT PIC X(09)  VALUE  "180028110".
      **   ∏∆µ€¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À° £∏£∞∆¸ƒ∂°À
           03  CONS-KOKYUKI3-COMMENT PIC X(09)  VALUE  "180029710".
      **   ∏∆µ€¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À° £∏£∞∆¸ƒ∂°À
           03  CONS-KOKYUKI4-COMMENT PIC X(09)  VALUE  "180029810".
      **   ∏∆µ€¥Ô•Í•œ•”•Í•∆°º•∑•Á•Û∞Â≥ÿ¥…Õ˝Œ¡° £±°À
           03  CONS-KOKYUKI5-COMMENT PIC X(09)  VALUE  "180030510".
      **   ∏∆µ€¥Ô•Í•œ•”•Í•∆°º•∑•Á•Û∞Â≥ÿ¥…Õ˝Œ¡° £≤°À
           03  CONS-KOKYUKI6-COMMENT PIC X(09)  VALUE  "180030610".
      *!   ∏∆µ€¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-KOKYUKI7-COMMENT PIC X(09)  VALUE  "101800150".
      *!   ∏∆µ€¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-KOKYUKI8-COMMENT PIC X(09)  VALUE  "101800160".
      *#   ∏∆µ€¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-KOKYUKI9-COMMENT PIC X(09)  VALUE  "101800340".
      *#   ∏∆µ€¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-KOKYUKI10-COMMENT PIC X(09) VALUE  "101800350".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-NOUKE1-COMMENT   PIC X(09)  VALUE  "180027610".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-NOUKE2-COMMENT   PIC X(09)  VALUE  "180027710".
      **   «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À° £±£¥£∞∆¸ƒ∂°À
           03  CONS-NOUKE3-COMMENT   PIC X(09)  VALUE  "180029310".
      **   «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À° £±£¥£∞∆¸ƒ∂°À
           03  CONS-NOUKE4-COMMENT   PIC X(09)  VALUE  "180029410".
      **   «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•Û∞Â≥ÿ¥…Õ˝Œ¡° £±°À
           03  CONS-NOUKE5-COMMENT   PIC X(09)  VALUE  "180030110".
      **   «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•Û∞Â≥ÿ¥…Õ˝Œ¡° £≤°À
           03  CONS-NOUKE6-COMMENT   PIC X(09)  VALUE  "180030210".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≥°À
           03  CONS-NOUKE7-COMMENT   PIC X(09)  VALUE  "180030810".
      *!   «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-NOUKE8-COMMENT   PIC X(09)  VALUE  "101800100".
      *!   «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-NOUKE9-COMMENT   PIC X(09)  VALUE  "101800110".
      *!   «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≥°À
           03  CONS-NOUKE10-COMMENT  PIC X(09)  VALUE  "101800120".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À° «—Õ—æ…∏ı∑≤°À
           03  CONS-NOUKE11-COMMENT  PIC X(09)  VALUE  "180032410".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À° «—Õ—æ…∏ı∑≤°À
           03  CONS-NOUKE12-COMMENT  PIC X(09)  VALUE  "180032510".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≥°À° «—Õ—æ…∏ı∑≤°À
           03  CONS-NOUKE13-COMMENT  PIC X(09)  VALUE  "180032610".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À° Õ◊≤∏Ó°¶«—Õ—æ…∏ı∑≤∞ ≥∞°À
           03  CONS-NOUKE14-COMMENT  PIC X(09)  VALUE  "180033910".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À° Õ◊≤∏Ó°¶«—Õ—æ…∏ı∑≤°À
           03  CONS-NOUKE15-COMMENT  PIC X(09)  VALUE  "180034010".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À° Õ◊≤∏Ó°¶«—Õ—æ…∏ı∑≤∞ ≥∞°À
           03  CONS-NOUKE16-COMMENT  PIC X(09)  VALUE  "180034110".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À° Õ◊≤∏Ó°¶«—Õ—æ…∏ı∑≤°À
           03  CONS-NOUKE17-COMMENT  PIC X(09)  VALUE  "180034210".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≥°À° Õ◊≤∏Ó°¶«—Õ—æ…∏ı∑≤∞ ≥∞°À
           03  CONS-NOUKE18-COMMENT  PIC X(09)  VALUE  "180034310".
      *    «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≥°À° Õ◊≤∏Ó°¶«—Õ—æ…∏ı∑≤°À
           03  CONS-NOUKE19-COMMENT  PIC X(09)  VALUE  "180034410".
      *#   «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-NOUKE20-COMMENT  PIC X(09)  VALUE  "101800250".
      *#   «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-NOUKE21-COMMENT  PIC X(09)  VALUE  "101800260".
      *#   «æ∑Ï¥…º¿¥µ≈˘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≥°À
           03  CONS-NOUKE22-COMMENT  PIC X(09)  VALUE  "101800270".
      *    ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-UNDOKI1-COMMENT  PIC X(09)  VALUE  "180027810".
      *    ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-UNDOKI2-COMMENT  PIC X(09)  VALUE  "180027910".
      **   ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À° £±£≤£∞∆¸ƒ∂°À
           03  CONS-UNDOKI3-COMMENT  PIC X(09)  VALUE  "180029510".
      **   ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À° £±£≤£∞∆¸ƒ∂°À
           03  CONS-UNDOKI4-COMMENT  PIC X(09)  VALUE  "180029610".
      **   ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•Û∞Â≥ÿ¥…Õ˝Œ¡° £±°À
           03  CONS-UNDOKI5-COMMENT  PIC X(09)  VALUE  "180030310".
      **   ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•Û∞Â≥ÿ¥…Õ˝Œ¡° £≤°À
           03  CONS-UNDOKI6-COMMENT  PIC X(09)  VALUE  "180030410".
      *!   ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-UNDOKI7-COMMENT  PIC X(09)  VALUE  "101800130".
      *!   ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-UNDOKI8-COMMENT  PIC X(09)  VALUE  "101800140".
      *    ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-UNDOKI9-COMMENT  PIC X(09)  VALUE  "180032710".
      *!   ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-UNDOKI10-COMMENT PIC X(09)  VALUE  "101800125".
      *    ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À° Õ◊≤∏Ó°À
           03  CONS-UNDOKI11-COMMENT  PIC X(09)  VALUE  "180034510".
      *    ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À° Õ◊≤∏Ó°À
           03  CONS-UNDOKI12-COMMENT  PIC X(09)  VALUE  "180034610".
      *    ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≥°À° Õ◊≤∏Ó°À
           03  CONS-UNDOKI13-COMMENT PIC X(09)  VALUE  "180034710".
      *#   ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±°À
           03  CONS-UNDOKI14-COMMENT PIC X(09)  VALUE  "101800280".
      *#   ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≤°À
           03  CONS-UNDOKI15-COMMENT PIC X(09)  VALUE  "101800290".
      *#   ±ø∆∞¥Ô•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £≥°À
           03  CONS-UNDOKI16-COMMENT PIC X(09)  VALUE  "101800330".
      *    ∆Ò…¬¥µº‘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡
           03  CONS-NANBYO-COMMENT   PIC X(09)  VALUE  "180017910".
      *    æ„≥≤ª˘° º‘°À•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £∂∫–Ã§À˛°À
           03  CONS-SYOGAI1-COMMENT  PIC X(09)  VALUE  "180028210".
      *    æ„≥≤ª˘° º‘°À•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £∂∫–∞ æÂ£±£∏∫–Ã§À˛°À
           03  CONS-SYOGAI2-COMMENT  PIC X(09)  VALUE  "180028310".
      *    æ„≥≤ª˘° º‘°À•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡° £±£∏∫–∞ æÂ°À
           03  CONS-SYOGAI3-COMMENT  PIC X(09)  VALUE  "180028410".
      *    ¿›ø©µ°«ΩŒ≈À°
           03  CONS-SESSYOKU-COMMENT PIC X(09)  VALUE  "180016610".
      *    §¨§Û¥µº‘•Í•œ•”•Í•∆°º•∑•Á•ÛŒ¡
           03  CONS-GANKNJ-COMMENT   PIC X(09)  VALUE  "180033110".
      *    ≤√£∏£∞˙
           03  CONS-KAITATU-COMMENT  PIC X(09)  VALUE  "140048010".
      *    ±˝ø«
           03  CONS-OSIN-COMMENT     PIC X(09)  VALUE  "114000110".
      *    ∆√ Ã±˝ø«
           03  CONS-OSIN2-COMMENT    PIC X(09)  VALUE  "114001610".
      *    ±˝ø«∆¸•≥•·•Û•»
           03  CONS-OSIN3-COMMENT    PIC X(09)  VALUE  "840000039".
      *
      *    £¬£Œ£–¿∫Ã©
           03  CONS-KENSA1-COMMENT   PIC X(09)  VALUE  "160162350".
      *    £Œ£‘°›££Ú£Ô£¬£Œ£–¿∫Ã©
           03  CONS-KENSA2-COMMENT   PIC X(09)  VALUE  "160181250".
      *    £»£¡£Œ£–¿∫Ã©
           03  CONS-KENSA3-COMMENT   PIC X(09)  VALUE  "160116310".
      *
      *    ≤∏ÓªŸ±Áœ¢∑»ªÿ∆≥Œ¡
           03  CONS-KAISIEN-COMMENT  PIC X(09)  VALUE  "113011710".
      *    Ω∏√ƒ±…Õ‹ø©ªˆªÿ∆≥Œ¡
           03  CONS-SYUEIYO-COMMENT  PIC X(09)  VALUE  "113003410".
      *    øÕπ©ø’¬°° §Ω§Œ¬æ°À
           03  CONS-JINZOU1-COMMENT  PIC X(09)  VALUE  "140007710".
      *    øÕπ©ø’¬°° À˝¿≠∞›ª˝∆©¿œ°À° £¥ª˛¥÷Ã§À˛°À
           03  CONS-JINZOU2-COMMENT  PIC X(09)  VALUE  "140036710".
      *    øÕπ©ø’¬°° À˝¿≠∞›ª˝∆©¿œ°À° £¥ª˛¥÷∞ æÂ£µª˛¥÷Ã§À˛°À
           03  CONS-JINZOU3-COMMENT  PIC X(09)  VALUE  "140051010".
      *    øÕπ©ø’¬°° À˝¿≠∞›ª˝∆©¿œ°À° £µª˛¥÷∞ æÂ°À
           03  CONS-JINZOU4-COMMENT  PIC X(09)  VALUE  "140051110".
      *    œ¢¬≥∑»π‘º∞ ¢ÀÏ§´§ÛŒÆ
           03  CONS-JINZOU5-COMMENT  PIC X(09)  VALUE  "140008510".
      *    ª˝¬≥¥ÀΩ˘º∞∑Ï±’ﬂ…≤·
           03  CONS-JINZOU6-COMMENT  PIC X(09)  VALUE  "140029850".
      *    ∑Ïﬁ˘∏Ú¥πŒ≈À°
           03  CONS-JINZOU7-COMMENT  PIC X(09)  VALUE  "140008210".
      *    ∑ÏµÂ¿Æ ¨Ω¸µÓŒ≈À°
           03  CONS-JINZOU8-COMMENT  PIC X(09)  VALUE  "140039210".
      *    øÕπ©ø’¬°° À˝¿≠∞›ª˝∆©¿œﬂ…≤·°À°  £ª®°À
           03  CONS-JINZOU9-COMMENT  PIC X(09)  VALUE  "140052810".
      *    ∂…ΩÍ±¢∞µ ƒ∫øΩË√÷° §Ω§Œ¬æ°À
           03  CONS-KYOKUIN1-COMMENT PIC X(09)  VALUE  "140052410".
      *    ∂…ΩÍ±¢∞µ ƒ∫øΩË√÷° »Ô §∫‡≈Ω…’°À° £±£∞£∞£„£Ì£≤Ã§À˛°À
           03  CONS-KYOKUIN2-COMMENT PIC X(09)  VALUE  "140051810".
      *    ∂…ΩÍ±¢∞µ ƒ∫øΩË√÷° »Ô §∫‡≈Ω…’°À° £±£∞£∞£„£Ì£≤°¡£≤£∞£∞£„£Ì£≤Ã§À˛°À
           03  CONS-KYOKUIN3-COMMENT PIC X(09)  VALUE  "140051910".
      *    ∂…ΩÍ±¢∞µ ƒ∫øΩË√÷° »Ô §∫‡≈Ω…’°À° £≤£∞£∞£„£Ì£≤∞ æÂ°À
           03  CONS-KYOKUIN4-COMMENT PIC X(09)  VALUE  "140052010".
      *    ∂…ΩÍ±¢∞µ ƒ∫øΩË√÷ΩÈ≤Û≤√ªª° £±£∞£∞£„£Ì£≤Ã§À˛°À
           03  CONS-KYOKUIN5-COMMENT PIC X(09)  VALUE  "140052170".
      *    ∂…ΩÍ±¢∞µ ƒ∫øΩË√÷ΩÈ≤Û≤√ªª° £±£∞£∞£„£Ì£≤∞ æÂ£≤£∞£∞£„£Ì£≤Ã§À˛°À
           03  CONS-KYOKUIN6-COMMENT PIC X(09)  VALUE  "140052270".
      *    ∂…ΩÍ±¢∞µ ƒ∫øΩË√÷ΩÈ≤Û≤√ªª° £≤£∞£∞£„£Ì£≤∞ æÂ°À
           03  CONS-KYOKUIN7-COMMENT PIC X(09)  VALUE  "140052370".
      *    ±…Õ‹•µ•›°º•»•¡°º•‡≤√ªª
           03  CONS-EISAPO-COMMENT   PIC X(09)  VALUE  "190136810".
      *    ±…Õ‹•µ•›°º•»•¡°º•‡≤√ªª° ªÿƒÍ√œ∞Ë°À
           03  CONS-EISAPO-COMMENT2  PIC X(09)  VALUE  "190147110".
      *    ∏∆µ€•±•¢•¡°º•‡≤√ªª
           03  CONS-KOKKEA-COMMENT   PIC X(09)  VALUE  "190137910".
      *    Ω≈æ…º‘≤√ªª° ¿∫ø¿Œ≈Õ‹…¬≈Ô∆˛±°Œ¡°À
           03  CONS-JYUSYO-COMMENT   PIC X(09)  VALUE  "190141370".
      *    Ω≈æ…º‘≤√ªª£±
           03  CONS-JYUSYO-COMMENT2  PIC X(09)  VALUE  "190151470".
      *
      *    ≤∏Óø¶∞˜≈˘”Ω·‚µ€∞˙≈˘ªÿº®Œ¡
           03  CONS-KAISYOKU-COMMENT PIC X(09)  VALUE  "114020910".
      *    ¿∫ø¿≤ •∑•Á°º•»°¶•±•¢° ¬Áµ¨Ãœ°À° ∆˛±°√Ê§Œ¥µº‘°À
           03  CONS-SEIKEA1-COMMENT  PIC X(09)  VALUE  "180039030".
      *    ¿∫ø¿≤ •∑•Á°º•»°¶•±•¢° æÆµ¨Ãœ°À° ∆˛±°√Ê§Œ¥µº‘°À
           03  CONS-SEIKEA2-COMMENT  PIC X(09)  VALUE  "180036030".
      *    ¿∫ø¿≤ •∑•Á°º•»°¶•±•¢° æÆµ¨Ãœ°À° ¡·¥¸°¶∆˛±°√Ê§Œ¥µº‘°À
           03  CONS-SEIKEA3-COMMENT  PIC X(09)  VALUE  "180039130".
      *    ¿∫ø¿≤ •∑•Á°º•»°¶•±•¢° ¬Áµ¨Ãœ°À° ¡·¥¸°¶∆˛±°√Ê§Œ¥µº‘°À
           03  CONS-SEIKEA4-COMMENT  PIC X(09)  VALUE  "180039230".
      *    ¿∫ø¿≤ •«•§°¶•±•¢° ¬Áµ¨Ãœ°À° ∆˛±°√Ê§Œ¥µº‘°À
           03  CONS-SEIKEA5-COMMENT  PIC X(09)  VALUE  "180039330".
      *    ¿∫ø¿≤ •«•§°¶•±•¢° æÆµ¨Ãœ°À° ∆˛±°√Ê§Œ¥µº‘°À
           03  CONS-SEIKEA6-COMMENT  PIC X(09)  VALUE  "180036130".
      *    ¿∫ø¿≤ •«•§°¶•±•¢° æÆµ¨Ãœ°À° ¡·¥¸°¶∆˛±°√Ê§Œ¥µº‘°À
           03  CONS-SEIKEA7-COMMENT  PIC X(09)  VALUE  "180039430".
      *    ¿∫ø¿≤ •«•§°¶•±•¢° ¬Áµ¨Ãœ°À° ¡·¥¸°¶∆˛±°√Ê§Œ¥µº‘°À
           03  CONS-SEIKEA8-COMMENT  PIC X(09)  VALUE  "180039530".
      *    ¿∫ø¿≤ •Í•®•æ•Û•¡°º•‡≤√ªª
           03  CONS-SEIRIE-COMMENT   PIC X(09)  VALUE  "190147010".
      *    …¬≈ÔÃÙ∫ﬁ∂»Ã≥º¬ª‹≤√ªª
           03  CONS-BYOYAK-COMMENT   PIC X(09)  VALUE  "190149110".
      *
      *    ∫ﬂ¬ª˝¬≥Õ€∞µ∏∆µ€Œ≈À°ªÿ∆≥¥…Õ˝Œ¡
           03  CONS-YOUATU-COMMENT   PIC X(09)  VALUE  "114009710".
      *
      *    ≥∞ÕËø◊¬Æ∏°¬Œ∏°∫∫≤√ªª
           03  CONS-GAIJINKEN        PIC X(09)  VALUE  "160177770".
      *
      *    …¬æ≤øÙ ‘Ω∏•’•È•∞ ‘Ω∏§Œ∞Ÿ§Œ•≥°º•…° ∆√ƒÍº¿¥µŒ≈Õ‹ªÿ∆≥Œ¡≈˘°À
           03  CONS-SIDORYO1         PIC X(09)  VALUE  "113001810".
           03  CONS-SIDORYO2         PIC X(09)  VALUE  "113001910".
           03  CONS-SIDORYO3         PIC X(09)  VALUE  "113002010".
           03  CONS-SIDORYO4         PIC X(09)  VALUE  "113700610".
           03  CONS-SIDORYO5         PIC X(09)  VALUE  "113700710".
           03  CONS-SIDORYO6         PIC X(09)  VALUE  "113700810".
      *
      *    ∫ﬂªŸ±Á•’•È•∞ ‘Ω∏§Œ∞Ÿ§Œ•≥°º•…
           03  CONS-ZAISIEN1         PIC X(09)  VALUE  "114011570".
           03  CONS-ZAISIEN2         PIC X(09)  VALUE  "114011670".
           03  CONS-ZAISIEN3         PIC X(09)  VALUE  "114011770".
      *
      *    ∫ﬁ§Œø«Œ≈∆‚Õ∆•¡•ß•√•Ø•∆°º•÷•Î
       01  TBL-ZAICHECK-AREA.
      *    ° •µ°À
           03  FILLER          PIC X(09)   VALUE    "820000046".
      *    ° ∏°¡™°À
           03  FILLER          PIC X(09)   VALUE    "820000053".
      *    ° •Í•œ¡™°À
           03  FILLER          PIC X(09)   VALUE    "820000054".
      *    ° ¿∫¡™°À
           03  FILLER          PIC X(09)   VALUE    "820000055".
      *
       01  TBL-ZAICHECK-R      REDEFINES   TBL-ZAICHECK-AREA.
           03  TBL-ZAICHECK    PIC X(09)   OCCURS    4.
      *
       01  TBL-ZAICHECK-MAX    PIC 9(02)   VALUE     4.
      *
      *    ª¿¡« 
           COPY    "CMSANSOTBL.INC".
      *
      *   ∆¸À‹∏ÏøÙª˙
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "£±£≤£≥£¥£µ£∂£∑£∏£π£∞".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
       01  TBL-SUJI-AREA-X.
           03  FILLER          PIC X(10)   VALUE
              "1234567890".
       01  TBL-SUJI-X-R        REDEFINES   TBL-SUJI-AREA-X.
           03  TBL-SUJI-X      PIC X(01)   OCCURS   10.
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
       01  WRK-RECE82G-REC. 
           COPY    "CPRCF082.INC"  REPLACING  //RECE82//
                                   BY         //WRK-RECE82G//.
      *
       01  WRK-RECE82C-REC. 
           COPY    "CPRCF082.INC"  REPLACING  //RECE82//
                                   BY         //WRK-RECE82C//.
      *
      *****************************************************************
      *    •’•°•§•Î•Ï•§•¢•¶•»
      *****************************************************************
      *
      *    •∏•Á•÷¥…Õ˝•ﬁ•π•ø
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ø«Œ≈≤Ò∑◊
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ø«Œ≈≤Ò∑◊…’≤√æ Û•ﬁ•π•ø°›
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    ø«Œ≈π‘∞Ÿ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ∆˛±°≤Ò∑◊
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    ∆˛±°ø«Œ≈π‘∞Ÿ
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ≈¿øÙ
           COPY    "CPTENSU.INC".
      *
      *    º˝«º
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    •∑•π•∆•‡¥…Õ˝
           COPY    "CPSYSKANRI.INC".
      *    ∞ÂŒ≈µ°¥ÿæ Û°›¥À‹
           COPY    "CPSK1001.INC".
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK200501.INC".
      *
           COPY    "CPSK4003.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    ¥µº‘•≥•·•Û•»
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *     ›∏±¡»§ﬂπÁ§Ô§ª
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    •µ•÷•◊•ÌÕ—ŒŒ∞Ë
      *****************************************************************
      *
      *    »æ≥—•¡•ß•√•Ø•µ•÷
           COPY    "CPORCSKANACHK.INC".
      *
      *   ∆¸…’ —¥π•µ•÷
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ¥µº‘»÷πÊ —¥π•µ•÷
           COPY    "CPORCSPTID.INC".
      *    øÙ√Õ —¥π•µ•÷
           COPY    "CPORCSNUM.INC".
      *    •¡•ß•√•Ø•«•£•∏•√•»° •‚•∏•Â•È•π£±£±Õ—°ÀªªΩ–•µ•÷
           COPY    "CPORCCHKDGT11.INC".
      *   •∏•Á•÷¥…Õ˝£ƒ£¬¿©∏Ê•µ•÷
           COPY    "CPORCSJOBKANRI.INC".
      *    ∆˛±°≈¿≈©∞Ï≥Áππø∑•µ•÷
           COPY    "CPORCSTENTEK.INC".
      *    œ´∫“∆˛±°≤Ò∑◊∫Ó¿Æ•µ•÷
           COPY    "CPORCSROUNYUIN.INC".
      *    ∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€ºË∆¿•µ•÷
           COPY    "CPORCSNYULIFE.INC".
      *    ∆˛±°≈¿≈©ºÍµª≤√ªª§ﬁ§»§·•µ•÷
           COPY    "CPORCSTENTEK2.INC".
      *
      *    £ƒ£¬∏°∫˜
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    ºÁΩËÕ˝
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *
      *****************************************************************
      *    ΩÈ¥¸°°ΩËÕ˝
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
                                       WRK-AREA
                                       FLG-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
           MOVE    "RECE840"           TO  RECE84PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE84PARA-HOSPNUM
           MOVE    "RECE850"           TO  RECE85PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE85PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE85PARA-HOSPNUM
      *
           MOVE    "RECE841"           TO  RECE8APARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE8APARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE8APARA-HOSPNUM
           MOVE    "RECE851"           TO  RECE8BPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE8BPARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE8BPARA-HOSPNUM
      *
           MOVE    "RECE84C"           TO  RECE84CPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84CPARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE84CPARA-HOSPNUM
           MOVE    "RECE85C"           TO  RECE85CPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE85CPARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE85CPARA-HOSPNUM
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        •π•∆•√•◊¥…Õ˝≥´ªœΩËÕ˝
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0840"          TO  JOB-PGID
               MOVE    "ø«Œ≈∂Ë ¨ Ã≈¿øÙΩ∏∑◊° œ´∫“°¶º´«Â¿’°À"
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ΩËÕ˝√Êªﬂ¿ﬂƒÍΩËÕ˝
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       •∑•π•∆•‡¥…Õ˝•ﬁ•π•ø° £±£∞£∞£±°À§´§Èæ√»Ò¿«√ºøÙ∂Ë ¨
      *       §ŒºË∆¿
              MOVE    SPACE               TO  WRK-TAX1001-KBN
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                MOVE  MCPDATA-REC         TO  SYS-1001-REC
                MOVE  SYS-1001-TAX-HASU-KBN
                                          TO  WRK-TAX1001-KBN
              ELSE
                MOVE  "∞ÂŒ≈µ°¥ÿæ Û§¨ºË∆¿§«§≠§ﬁ§ª§Û§«§∑§ø°£"
                                          TO  WRK-RECEERR
                PERFORM 500-ERR-HENSYU-SEC
                MOVE    1                 TO  FLG-END
              END-IF
      *
      *       ª‹¿ﬂ¥Ω‡æ Û
              MOVE     ZERO     TO     WRK-SISETU613
              MOVE     ZERO     TO     WRK-SISETU695
              MOVE     ZERO     TO     WRK-SISETU3055
              MOVE     ZERO     TO     WRK-SISETU3056
              MOVE     ZERO     TO     WRK-SISETU3057
              MOVE     ZERO     TO     WRK-SISETU3058
              MOVE     ZERO     TO     WRK-SISETU3168
              MOVE     ZERO     TO     WRK-SISETU3169
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                INITIALIZE                      SYSKANRI-REC
                MOVE    "1006"              TO  SYS-KANRICD
                MOVE    IDXA                TO  SYS-KBNCD
                MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
                MOVE    "01"                TO  SYS-STYUKYMD (7:2)
                MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys          MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                PERFORM 910-SYSKANRI-INV-SEC
                IF      FLG-SYSKANRI     =   ZERO
                   MOVE   MCPDATA-REC       TO  SYS-1006-REC
                   EVALUATE  IDXA
                     WHEN     2
                       MOVE  SYS-1006-SSTKIJUN(113)
                                            TO  WRK-SISETU613
                       MOVE  SYS-1006-SSTKIJUN(195)
                                            TO  WRK-SISETU695
                     WHEN     7
                       MOVE  SYS-1006-SSTKIJUN(55)
                                            TO  WRK-SISETU3055
                       MOVE  SYS-1006-SSTKIJUN(56)
                                            TO  WRK-SISETU3056
                       MOVE  SYS-1006-SSTKIJUN(57)
                                            TO  WRK-SISETU3057
                       MOVE  SYS-1006-SSTKIJUN(58)
                                            TO  WRK-SISETU3058
                       MOVE  SYS-1006-SSTKIJUN(168)
                                            TO  WRK-SISETU3168
                       MOVE  SYS-1006-SSTKIJUN(169)
                                            TO  WRK-SISETU3169
                   END-EVALUATE
                END-IF
               END-IF
              END-PERFORM
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key8"          TO  MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
                  END-PERFORM
              ELSE
                  MOVE    "∞ÂŒ≈µ°¥ÿæ Û§¨ºË∆¿§«§≠§ﬁ§ª§Û§«§∑§ø°£"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              MOVE     SPACE              TO  TBL-SYS1006-T
              INITIALIZE                      TBL-SYS1006-T
      *
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                 MOVE     ZERO            TO  SYSKAN-IDX2
                 INITIALIZE                   SYSKANRI-REC
                 MOVE    "1006"           TO  SYS-KANRICD
                 MOVE    IDXA             TO  SYS-KBNCD
grpsys           MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                 MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key8"           TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC           =   ZERO
                     MOVE    "tbl_syskanri"   TO  MCP-TABLE
                     MOVE    "key8"           TO  MCP-PATHNAME
                     PERFORM  900-SYSKANRI-READ-SEC
                     PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                               OR    (SYSKAN-IDX2  >=  100)
      *
                       MOVE   MCPDATA-REC     TO  SYS-1006-REC
                       EVALUATE  IDXA
                         WHEN     2
                           ADD     1      TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                          TO  TBL-SYS10062(SYSKAN-IDX2)
                         WHEN     7
                           ADD     1      TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                          TO  TBL-SYS10067(SYSKAN-IDX2)
                       END-EVALUATE
                       MOVE    "tbl_syskanri" TO  MCP-TABLE
                       MOVE    "key8"         TO  MCP-PATHNAME
                       PERFORM  900-SYSKANRI-READ-SEC
                     END-PERFORM
                 END-IF
                 MOVE    "tbl_syskanri"       TO  MCP-TABLE
                 MOVE    "key8"               TO  MCP-PATHNAME
                 PERFORM  900-CLOSE-SEC
               END-IF
              END-PERFORM
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *
             MOVE      WRK-PARA-SRYYM    TO  WRK-SRYYM-5000
             PERFORM   120-SYS-5000-CHK-SEC
      *
      *      ø©ªˆ°¶Ã§º¬ª‹∏∫ªªæ Û° •∑•π•∆•‡¥…Õ˝•∆°º•÷•Î§Ë§Í°À
             INITIALIZE                   WRK-SHOKUJIRYOYO-OCC
                                          WRK-TOKUKANRIKASAN-OCC
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
             MOVE    "01"                TO  SYS-STYUKYMD (7:2)
             MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             PERFORM 910-SYSKANRI-INV-SEC
             IF      FLG-SYSKANRI        =   ZERO
               MOVE     MCPDATA-REC      TO  SYS-5000-REC
               PERFORM  110-SYSKAN-HENSYU-SEC
               IF        WRK-SYS5000-COUNT    =     ZERO
                  CONTINUE
               ELSE
                  PERFORM  130-SYS-5000-GET-SEC
               END-IF
             ELSE
               MOVE "∞ÂŒ≈µ°¥ÿæ Û°›∆˛±°¥À‹§¨ºË∆¿§«§≠§ﬁ§ª§Û§«§∑§ø°£"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1              TO  FLG-END
             END-IF
      *
           ELSE
      *
             MOVE     ZERO               TO  SYSKAN-IDX
             MOVE     SPACE              TO  TBL-SYS5000-T
             INITIALIZE                      TBL-SYS5000-T
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"  TO  MCP-TABLE
                 MOVE    "key8"          TO  MCP-PATHNAME
                 PERFORM 900-SYSKANRI-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                           OR    (SYSKAN-IDX   >=  100)
      *
                   MOVE    MCPDATA-REC  TO  SYS-5000-REC
                   ADD     1            TO  SYSKAN-IDX
                   MOVE   SYS-5000-REC  TO  TBL-SYS5000(SYSKAN-IDX)
      *
                   MOVE    "tbl_syskanri"   TO  MCP-TABLE
                   MOVE    "key8"           TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
                 END-PERFORM
             ELSE
                 MOVE "∞ÂŒ≈µ°¥ÿæ Û°›∆˛±°¥À‹§¨ºË∆¿§«§≠§ﬁ§ª§Û§«§∑§ø°£"
                                        TO  WRK-RECEERR
                 PERFORM 500-ERR-HENSYU-SEC
                 MOVE    1              TO  FLG-END
             END-IF
             MOVE    "tbl_syskanri"     TO  MCP-TABLE
             MOVE    "key8"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
           END-IF
      *
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "01"             TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
           MOVE    "01"             TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC     TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
                MOVE  MCPDATA-REC   TO  SYS-200501-REC
           ELSE
                INITIALIZE              SYS-200501-REC
           END-IF
           IF      SYS-200501-COMMENT-NYUINRYO-KSIKBN
                                    =   SPACE
               MOVE    "1"   TO  SYS-200501-COMMENT-NYUINRYO-KSIKBN
           END-IF
           IF      SYS-200501-COMMENT-NYUINRYO1-KSIKBN
                                    =   SPACE
               MOVE    "2"   TO  SYS-200501-COMMENT-NYUINRYO1-KSIKBN
           END-IF
           IF      SYS-200501-ROURECEKBN  =   SPACE
               MOVE    "0"   TO  SYS-200501-ROURECEKBN
           END-IF
      *
           OPEN    OUTPUT              RECE84-FILE
           CLOSE                       RECE84-FILE
      *
           OPEN    OUTPUT              RECE85-FILE
           CLOSE                       RECE85-FILE
      *
           OPEN    OUTPUT              RECE8A-FILE
           CLOSE                       RECE8A-FILE
      *
           OPEN    OUTPUT              RECE8B-FILE
           CLOSE                       RECE8B-FILE
      *
           OPEN    OUTPUT              RECE84C-FILE
           CLOSE                       RECE84C-FILE
      *
           OPEN    OUTPUT              RECE85C-FILE
           CLOSE                       RECE85C-FILE
      *
           OPEN    OUTPUT              RECE84-FILE
           OPEN    I-O                 RECE85-FILE
      *
           OPEN    OUTPUT              RECE8A-FILE
                                       RECE8B-FILE
      *
           OPEN    OUTPUT              RECE84C-FILE
           OPEN    I-O                 RECE85C-FILE
      *
           OPEN    I-O                 RECE82-FILE
           IF      STS-RECE82          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           PERFORM    900-RECE82-READ-SEC
      *
           IF  FLG-END =  ZERO
              INITIALIZE  WRK-CHUKSNSRYCD-TABLE
              IF  RECE82-SRYYM  >= "201004"
                 PERFORM   140-NYUINKASAN-JYOGAI-CYUSYUTU-SEC
              END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    •∑•π•´•Û§Œ ‘Ω∏
      *****************************************************************
       110-SYSKAN-HENSYU-SEC                SECTION.
      *
           PERFORM   VARYING   IDX11  FROM  1  BY  1
                      UNTIL    IDX11   >   31
              EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *         ∆˛±°ø©ªˆŒ≈Õ‹° £±°À
                WHEN         "197000110"
                    MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX11)
                    EVALUATE  SYS-5000-SHOKUKBN(1)
                      WHEN     "1"
                        MOVE  ZERO  TO   WRK-TOKUKANRIKASAN(IDX11)
                      WHEN     "2"
                        MOVE    1   TO   WRK-TOKUKANRIKASAN(IDX11)
                    END-EVALUATE
      *         ∆˛±°ø©ªˆŒ≈Õ‹° £≤°À
                WHEN         "197000710"
                    MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX11)
              END-EVALUATE
           END-PERFORM
      *
           .
       110-SYSKAN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ∑Ó∆‚§À £øÙ§Œ∞ÂŒ≈¥¸¥÷æ Û§¨§ §§§´∏°∫˜° ∞ÂŒ≈µ°¥ÿ∏∫ªª§Œ≥Œ«ß°À  *
      ******************************************************************
       120-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE     ZERO               TO  WRK-SYS5000-COUNT
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD (1:6)
           MOVE    "__"                TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   MOVE    MCPDATA-REC TO  SYS-5000-REC
                   ADD     1     TO        WRK-SYS5000-COUNT
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key11"         TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       120-SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    ∑Ó∆‚§Œ∞ÂŒ≈¥¸¥÷æ ÛºË∆¿° ∞ÂŒ≈µ°¥ÿ∏∫ªªæ Û°À                  *
      ******************************************************************
       130-SYS-5000-GET-SEC                 SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD (1:6)
           MOVE    "__"                TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   MCPDATA-REC  TO  SYS-5000-REC
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
                 PERFORM    VARYING  IDX11  FROM  IDV  BY  1
                             UNTIL   IDX11    >   31
                      EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *                ∆˛±°ø©ªˆŒ≈Õ‹° £±°À
                       WHEN         "197000110"
                          MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX11)
                          EVALUATE  SYS-5000-SHOKUKBN(1)
                            WHEN     "1"
                              MOVE  ZERO  TO   WRK-TOKUKANRIKASAN(IDX11)
                            WHEN     "2"
                              MOVE    1   TO   WRK-TOKUKANRIKASAN(IDX11)
                          END-EVALUATE
      *                ∆˛±°ø©ªˆŒ≈Õ‹° £≤°À
                       WHEN         "197000710"
                          MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX11)
                          MOVE    ZERO    TO   WRK-TOKUKANRIKASAN(IDX11)
                      END-EVALUATE
                 END-PERFORM
      *
                 MOVE    "tbl_syskanri"   TO   MCP-TABLE
                 MOVE    "key11"          TO   MCP-PATHNAME
                 PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"         TO   MCP-TABLE
           MOVE    "key11"                TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       130-SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∆˛±°Œ¡≤√ªªΩ¸≥∞•≥°º•…√ÍΩ–
      *****************************************************************
       140-NYUINKASAN-JYOGAI-CYUSYUTU-SEC      SECTION.
      *
           MOVE   ZERO               TO  IDX14
      *
           INITIALIZE                    TNS-TENSU-REC
           MOVE   SPA-HOSPNUM        TO  TNS-HOSPNUM
           MOVE   RECE82-SRYYM       TO  TNS-YUKOSTYMD(1:6)
           MOVE   "01"               TO  TNS-YUKOSTYMD(7:2)
           MOVE   RECE82-SRYYM       TO  TNS-YUKOEDYMD(1:6)
           MOVE   "01"               TO  TNS-YUKOEDYMD(7:2)
           MOVE   TNS-TENSU-REC      TO  MCPDATA-REC
           MOVE   "tbl_tensu"        TO  MCP-TABLE
           MOVE   "key30"            TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC            =   ZERO
              MOVE    "tbl_tensu"    TO  MCP-TABLE
              MOVE    "key30"        TO  MCP-PATHNAME
              PERFORM  900-TENSU-READ-SEC
              PERFORM  UNTIL  (FLG-TENSU  =     1)
                        OR    (IDX14     >=    50)
                 IF   TNS-SRYCD  =  "190134270"
                                 OR "190135470"
                                 OR "190135670"
      *
                                 OR "190142970"
                                 OR "190143370"
      *
                                 OR "190142870"
                                 OR "190143270"
                                 OR "190143470"
                                 OR "190144770"
                                 OR "190144870"
                                 OR "190144970"
                                 OR "190145070"
                                 OR "190145170"
                                 OR "190145270"
      *
                                 OR "190135070"
                   CONTINUE
                 ELSE
                   ADD    1          TO  IDX14
                   MOVE   TNS-SRYCD  TO  WRK-CHUKSNSRYCD(IDX14)
                 END-IF
      *
                 MOVE    "tbl_tensu" TO  MCP-TABLE
                 MOVE    "key30"     TO  MCP-PATHNAME
                 PERFORM  900-TENSU-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_tensu"       TO  MCP-TABLE
           MOVE    "key30"           TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       140-NYUINKASAN-JYOGAI-CYUSYUTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÁΩËÕ˝
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 210-HENSYU-SEC
      *
           IF   FLG-END = ZERO
             PERFORM 900-RECE82-READ-SEC
      *
             IF  CNT-IN-COMMIT   =   300
                 MOVE  ZERO          TO      CNT-IN-COMMIT
                 PERFORM 900-DBCOMMIT-SEC
                 PERFORM 900-DBSTART-SEC
             END-IF
      *
      *      ΩËÕ˝√Êªﬂ¿ﬂƒÍΩËÕ˝
             IF      CNT-IN-CANCEL       =   50
                 MOVE    ZERO                TO  CNT-IN-CANCEL
                 PERFORM 500-CANCEL-HENSYU-SEC
             END-IF
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *     ‘Ω∏ΩËÕ˝
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           INITIALIZE          WRK-DAY-TABLE
      *
           INITIALIZE          FLG-KHNTEN-JI-AREA
      *
           INITIALIZE          TBL-TENSUG-JI
      *
           INITIALIZE          WRK-SKJ200604-TABLE
      *
           INITIALIZE          WRK-NYULIFE-TABLE
      *
           INITIALIZE          WRK-TENTEKI2-TABLE
      *
           MOVE   ZERO   TO    FLG-HEYA
      *
           MOVE   ZERO   TO    WRK-KINGAKU-JI
      *
           MOVE   ZERO   TO    WRK-ZAITENNASHI
      *
           INITIALIZE          WRK-RECE82G-REC
           INITIALIZE          WRK-RECE82C-REC
      *
           PERFORM    290-TENTEKIMATOME-HENSYU-SEC
      *
           PERFORM    220-RECE84-HENSYU-SEC
           PERFORM    230-RECE84-HENSYU2-SEC
           PERFORM    240-RECE84-HENSYU3-SEC
           PERFORM    250-RECE84-HENSYU4-SEC
           PERFORM    260-FTN-SET-SEC
      *
           IF      RECE82-HKNKBN    =   "4"
             PERFORM  270-KHNTEN-JI-CLEAR-SEC
           END-IF                            
      *
           PERFORM    281-GENTEN-HENSYU-SEC
      *
           PERFORM    280-RECE82-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿≈©ºÍµª≤√ªª§ﬁ§»§· ‘Ω∏
      *****************************************************************
       290-TENTEKIMATOME-HENSYU-SEC      SECTION.
      *
           INITIALIZE                  ORCSTENTEK2AREA
           MOVE    RECE82-HOSPNUM  TO  LNK-ORCSTEN2-HOSPNUM
           MOVE    RECE82-PTID     TO  LNK-ORCSTEN2-PTID
           MOVE    RECE82-SRYYM    TO  LNK-ORCSTEN2-SRYYM
           MOVE    RECE82-HKNCOMBI TO  LNK-ORCSTEN2-HKNCOMBI
           CALL    "ORCSTENTEK2"  USING
                                  ORCSTENTEK2AREA
                                  SPA-AREA
           IF      LNK-ORCSTEN2-RC  =   ZERO
             IF   (LNK-ORCSTEN2-TENTEK-ERRCD  = ZERO)
             OR   (LNK-ORCSTEN2-CHUJOU-ERRCD  = ZERO)
               MOVE  1            TO  WRK-TENTEKI2-HENKYAKUARI
               MOVE  LNK-ORCSTEN2-OUT-AREA
                                  TO  WRK-TENTEKI2-HENKYAKU-AREA
             END-IF
           END-IF
      *
           .
       290-TENTEKIMATOME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£¥°À
      *****************************************************************
       220-RECE84-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE82-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE82-PTID          TO      ACCT-PTID
           MOVE    RECE82-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE82-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key22"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key22"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
      *
                MOVE   ZERO          TO  FLG-RINYAK
      *
                IF    ACCT-SRYKBN       =  "95"  OR  "96"
                 CONTINUE
                ELSE
      *
                 MOVE     ZERO    TO    FLG-JYOGAI
                 IF    ACCT-SRYKBN  =  "90" OR "92"
                   PERFORM  VARYING  ID4    FROM   1   BY   1
                             UNTIL  (ID4                  >   50  )
                               OR   (WRK-CHUKSNSRYCD(ID4) =  SPACE)
                               OR   (FLG-JYOGAI           =   1   )
                     IF  ACCT-SRYCDTOTAL(6:9) = WRK-CHUKSNSRYCD(ID4)
                        MOVE     1    TO   FLG-JYOGAI
                     END-IF
                   END-PERFORM
                 END-IF
      *
      *          Ã§º¬ª‹∏∫ªª°¶∆¸æÔ¿∏≥Ëæ„≥≤≤√ªª°¶√‘ Ú≤√ªª°¶
      *          ÍÛ·Ï¥µº‘¥…Õ˝≤√ªª§ÚΩ¸§Ø
                 IF ((ACCT-SRYCDTOTAL  =  190080470  OR  190799670
                                      OR  190080570  OR  190799770
                                      OR  190111690  OR  190809090
                                      OR  190111790  OR  190809190
                                      OR  190807570  OR  190808670
                                      OR  190116990  OR  190813390) AND
                     (RECE82-SRYYM     <= "200603"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190109170  OR  190109270) AND
                     (RECE82-SRYYM     <= "200606"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190117070  OR  190109670) AND
                     (RECE82-SRYYM     >= "200604"                ) AND
                     (RECE82-SRYYM     <= "200803"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190121970  OR  190122670) AND
                     (RECE82-SRYYM     >= "200607"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190119210               ) AND
                     (RECE82-SRYYM     >= "200804"                ))
                 OR ( FLG-JYOGAI       =  1                        )
                   CONTINUE
                 ELSE
      *
                   MOVE     ZERO    TO    FLG-TAISYO
                   IF   (ACCT-SRYKBN         =   "80" )
                   AND  (ACCT-ZAITEN         =   ZERO )
                   AND  (ACCT-ZAIKAISU   NOT =   ZERO )
                   AND  (ACCT-JIHOKBN        =   SPACE)
                     PERFORM  2204-SRYACCTPLUS-KENSAKU-SEC
                   END-IF
      *
                   MOVE     ZERO    TO    FLG-TENTEKIMATOME
                   IF    ACCT-SRYKBN  =  "33"
                     PERFORM  2205-TENTEKIMATOME-CHECK-SEC
                   END-IF
      *
                   IF  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                        (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                        (ACCT-JIHOKBN       =    SPACE) AND
                        (FLG-TENTEKIMATOME  =    ZERO ))
                   OR  ( FLG-TAISYO         =      1   )
      *
                       MOVE   ZERO   TO   WRK-TEN
                                          WRK-MEISAI-CNT
                                          WRK-SRYCDTOTAL
                                          WRK-SURYOUTOTAL
                                          WRK-YOUBUKASAN
                                          WRK-KYOBUKASAN
                                          WRK-KEIBUKASAN
                                          WRK-SOGAIKASAN
                                          WRK-RYOYOTEATE
                                          WRK-ZAI-KINGAKU
      *
                       MOVE   ZERO   TO   WRK-SRYSURYO
                       MOVE   ZERO   TO   WRK-SRYSURYO2
                       MOVE   ZERO   TO   WRK-SRYSURYO3
                       MOVE   ZERO   TO   WRK-TANICD
                       MOVE   ZERO   TO   WRK-TANIMOJISU
                       MOVE   SPACE  TO   WRK-TANINAME
                       MOVE   ZERO   TO   FLG-HOUSIJI-COM
                                          FLG-SYOBYO-COM
                                          FLG-RYOYO-COM
                                          FLG-SYODOU-COM
                                          FLG-OSIN-COM
                                          FLG-HOUSEI-COM
                                          FLG-HOUMON-COM
                                          FLG-TAHOKEN
                                          FLG-SRYKBN-HEN
                                          FLG-KENSA-COM
      *
                       MOVE   SPACE  TO   WRK-FORMALNAME
                       MOVE   ZERO   TO
                                     WRK-KZI058-TEN
                                     WRK-KZI058-TANICD
                                     WRK-KZI058-TANIMOJISU
                       MOVE   SPACE  TO
                                     WRK-KZI058-TANINAME
      *
                       MOVE   ZERO   TO   FLG-WRITE85
                       MOVE   ZERO   TO   FLG-TENTEKI
                       MOVE   ZERO   TO   FLG-HENSYU-NG
                       MOVE   ZERO   TO   FLG-HENSYU-OK
                       MOVE   ZERO   TO   FLG-SETCOM
                       MOVE   ZERO   TO   WRK-SETCOM-JIDO
                       MOVE   ZERO   TO   WRK-SETCOM-JIDO2
                       IF  (ACCT-SRYKBN     = "97"     )
                       AND (ACCT-SRYCDTOTAL = 197000470
                                           OR 197001570
                                           OR 197001210)
                         IF  (RECE82-SRYYM   >= "200604" )
                         AND (ACCT-SRYCDTOTAL = 197000470
                                             OR 197001570)
                           PERFORM   2201-RECE84-TENSOU-SEC
                           PERFORM   2202-RECE85-HENSYU-SEC
                           IF    FLG-WRITE85  =   1
                               PERFORM 900-RECE84-WRITE-SEC
                           END-IF
                         END-IF
                       ELSE
                         PERFORM   2201-RECE84-TENSOU-SEC
                         PERFORM   2202-RECE85-HENSYU-SEC
                         IF    FLG-WRITE85  =   1
                             PERFORM 900-RECE84-WRITE-SEC
                         END-IF
                       END-IF
                   ELSE
                       IF  (ACCT-ZAIKAISU  NOT =    ZERO) AND
                           (ACCT-JIHOKBN       =   SPACE) AND
                           (FLG-TENTEKIMATOME  =    ZERO)
                            IF ( ACCT-SRYKBN  NOT =
                                 "12" AND "25" AND "93" AND "98")
                            OR ( ACCT-ZAIKBN  =   1             )
                               IF  (ACCT-SRYCDTOTAL(1:12)  =
                                                  "000000999901")
                               AND (ACCT-MEISAISU      =   1    )
                                 CONTINUE
                               ELSE
                                 MOVE   ZERO   TO   WRK-TEN
                                                    WRK-MEISAI-CNT
                                                    WRK-SRYCDTOTAL
                                                    WRK-SURYOUTOTAL
                                                    WRK-YOUBUKASAN
                                                    WRK-KYOBUKASAN
                                                    WRK-KEIBUKASAN
                                                    WRK-SOGAIKASAN
                                                    WRK-RYOYOTEATE
                                                    WRK-ZAI-KINGAKU
      *
                                 MOVE   ZERO   TO   WRK-SRYSURYO
                                 MOVE   ZERO   TO   WRK-SRYSURYO2
                                 MOVE   ZERO   TO   WRK-SRYSURYO3
                                 MOVE   ZERO   TO   WRK-TANICD
                                 MOVE   ZERO   TO   WRK-TANIMOJISU
                                 MOVE   SPACE  TO   WRK-TANINAME
                                 MOVE   ZERO   TO   FLG-HOUSIJI-COM
                                                    FLG-SYOBYO-COM
                                                    FLG-RYOYO-COM
                                                    FLG-SYODOU-COM
                                                    FLG-OSIN-COM
                                                    FLG-HOUSEI-COM
                                                    FLG-HOUMON-COM
                                                    FLG-TAHOKEN
                                                    FLG-SRYKBN-HEN
                                                    FLG-KENSA-COM
      *
                                 MOVE   SPACE  TO   WRK-FORMALNAME
                                 MOVE   ZERO   TO
                                               WRK-KZI058-TEN
                                               WRK-KZI058-TANICD
                                               WRK-KZI058-TANIMOJISU
                                 MOVE   SPACE  TO
                                               WRK-KZI058-TANINAME
      *
                                 MOVE   ZERO   TO   FLG-WRITE85
                                 MOVE   ZERO   TO   FLG-TENTEKI
                                 MOVE   ZERO   TO   FLG-HENSYU-NG
                                 MOVE   ZERO   TO   FLG-HENSYU-OK
                                 MOVE   ZERO   TO   FLG-SETCOM
                                 MOVE   ZERO   TO   WRK-SETCOM-JIDO
                                 MOVE   ZERO   TO   WRK-SETCOM-JIDO2
                                 IF    ACCT-SRYKBN  =
                                       "60" OR "64" OR "80"
                                   PERFORM 2203-SINRYO-CHECK-SEC
                                 END-IF
                                 PERFORM   2201-RECE84-TENSOU-SEC
                                 PERFORM   2202-RECE85-HENSYU-SEC
                                 IF    FLG-WRITE85  =   1
                                    PERFORM 900-RECE84-WRITE-SEC
                                 END-IF
                               END-IF
                            END-IF
                       END-IF
                   END-IF
                 END-IF
                END-IF
                MOVE    "tbl_sryacct"    TO      MCP-TABLE
                MOVE    "key22"          TO      MCP-PATHNAME
                PERFORM  900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"         TO      MCP-TABLE
           MOVE    "key22"               TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       220-RECE84-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       2201-RECE84-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           MOVE    ACCT-ZAINUM          TO      RECE84-ZAINUM
           MOVE    ACCT-SRYKBN          TO      RECE84-SRYKBN
           IF      ACCT-SRYKBN = "90" OR "92"
                 MOVE    "6"            TO      RECE84-ZAISKBKBN
           ELSE 
              IF   ACCT-SRYKBN = "97"
      ***        MOVE    "4"            TO      RECE84-ZAISKBKBN
                 MOVE    "9"            TO      RECE84-ZAISKBKBN
                 IF   ACCT-ZAITEN  =  ZERO
                   MOVE  "A"            TO      RECE84-ZAISKBKBN
                 END-IF
              END-IF
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
           MOVE    ACCT-SRYKA           TO      RECE84-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE84-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE84-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE84-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE84-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE84-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE84-MEISAISU
           MOVE    ACCT-ZAIKAISU        TO      RECE84-ZAIKAISU
           MOVE    ACCT-SYUTEN1         TO      RECE84-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE84-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE84-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE84-KIZAITEN
           IF   RECE82-HKNKBN     =  "4"
              IF  ACCT-SRYKBN     =  "70"
                  COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                            -  RECE84-SYUTEN2
                  COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                            +  RECE84-SYUTEN2
              END-IF
           END-IF
           MOVE    ACCT-DRCD            TO      RECE84-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE84-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX3   FROM    1    BY   1
                       UNTIL     IDX3   >       4
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE84-KAISU (IDX3 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (ACCT-DAY(1 IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
           MOVE      ZERO     TO    FLG-OKIKAE
           IF   ( RECE82-AGE       >=  75        )
           OR   ( RECE82-BIRTHDAY  <  "19321001" )
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL    ( IDX2        >    31 )
                             OR     ( FLG-OKIKAE  =     1 )
                  IF  ( RECE82-BYOTOSYUBETU(IDX2) =  "02" OR "03" )
                  AND ( RECE84-KAISU(1 IDX2)  NOT =  ZERO         )
                        MOVE   1   TO   FLG-OKIKAE
                  END-IF    
               END-PERFORM
           END-IF
      *
           IF    FLG-RINYAK    NOT =    ZERO
                MOVE    FLG-RINYAK   TO    RECE84-SAFLG
           END-IF
      *
           .
       2201-RECE84-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£µ°À
      *****************************************************************
       2202-RECE85-HENSYU-SEC               SECTION.
      *
           MOVE    SPACE                TO      WRK-CHECK-SRYCD
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
      *
               IF    WRK-CHECK-SRYCD  =  SPACE
                 MOVE   SRY-SRYCD(1)    TO      WRK-CHECK-SRYCD
               END-IF
      *
               MOVE     ZERO    TO    FLG-JYOGAI
               IF    ACCT-SRYKBN  =  "90" OR "92"
                 PERFORM  VARYING  ID4    FROM   1   BY   1
                           UNTIL  (ID4                  >   50  )
                             OR   (WRK-CHUKSNSRYCD(ID4) =  SPACE)
                             OR   (FLG-JYOGAI           =   1   )
                   IF  SRY-SRYCD(1) = WRK-CHUKSNSRYCD(ID4)
                      MOVE     1    TO   FLG-JYOGAI
                   END-IF
                 END-PERFORM
               END-IF
      *
      *        œ´∫“∆˛±°§Œ≤√ªª¬–æ›•≥°º•…§Úø«Œ≈π‘∞Ÿ•∆°º•÷•Î§«»ΩƒÍ
               IF ((SRY-SRYCD(1)  =  "190080470"  OR  "190799670"
                                 OR  "190080570"  OR  "190799770"
                                 OR  "190111690"  OR  "190809090"
                                 OR  "190111790"  OR  "190809190"
                                 OR  "190807570"  OR  "190808670"
                                 OR  "190116990"  OR  "190813390") AND
                   (RECE82-SRYYM     <= "200603"                 ))
               OR ((SRY-SRYCD(1)  =  "190109170"  OR  "190109270") AND
                   (RECE82-SRYYM     <= "200606"                 ))
               OR ((SRY-SRYCD(1)  =  "190117070"  OR  "190109670") AND
                   (RECE82-SRYYM     >= "200604"                 ) AND
                   (RECE82-SRYYM     <= "200803"                 ))
               OR ((SRY-SRYCD(1)  =  "190121970"  OR  "190122670") AND
                   (RECE82-SRYYM     >= "200607"                 ))
               OR ((SRY-SRYCD(1)  =  "190119210"                 ) AND
                   (RECE82-SRYYM     >= "200804"                 ))
               OR ( FLG-JYOGAI    =   1                           )
                 MOVE   ZERO         TO   FLG-WRITE85
               ELSE
                 PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE    ZERO         TO   FLG-ZANHAIKI
                                             FLG-FORMAL-KISAI
                   MOVE    ZERO         TO   WRK-RENNUM
                                             WRK-RENNUM2
                   MOVE    ZERO         TO   FLG-WRITE85X
                   PERFORM   22021-RECE85-TENSOU-SEC
                   IF   FLG-TENTEKI  =  ZERO
                   AND  FLG-WRITE85X =   1
                     PERFORM 22022-RECE82-HENSYU-SEC
                     PERFORM 900-RECE85-WRITE-SEC
                     MOVE  1            TO   FLG-WRITE85
                   END-IF
                   IF   FLG-ZANHAIKI  =  1
                      PERFORM  900-RECE85-REWRITE-SEC
                   END-IF
                   IF  FLG-FORMAL-KISAI  =  1
                      PERFORM  900-RECE85-REWRITE2-SEC
                   END-IF
                   MOVE    "tbl_sryact" TO   MCP-TABLE
                   MOVE    "key9"       TO   MCP-PATHNAME
                   PERFORM  900-SRYACT-READ-SEC
                 END-PERFORM
               END-IF
           END-IF
           MOVE    "tbl_sryact"         TO   MCP-TABLE
           MOVE    "key9"               TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2202-RECE85-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       22021-RECE85-TENSOU-SEC                SECTION.
      *
           INITIALIZE          WRK-JIBAIKZI-TNS-TAIHI-TABLE
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
           MOVE    SRY-ZAINUM           TO      RECE85-ZAINUM
           MOVE    SRY-RENNUM           TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    SRY-SRYKA            TO      RECE85-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE85-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE85-SRYSYUKBN
                                                RECE84-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           MOVE    ZERO                 TO      FLG-OSINKBN
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
      *     œ∑øÕ¿∫ø¿…¬≈Ô≈˘§Œø«Œ≈π‘∞Ÿ•≥°º•…√÷§≠¥π§®
            IF   RECE82-SRYYM  >= "200604"
              CONTINUE
            ELSE
              IF   FLG-OKIKAE   =   1
                EVALUATE  SRY-SRYCD(IDX2)
      *           ∑Ï±’≥ÿ≈™∏°∫∫»Ω√«Œ¡
                  WHEN  "160061810"
                      MOVE  "160703610"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ∏°¬Œ∏°∫∫¥…Õ˝≤√ªª° £±°À
                  WHEN  "160170170"
                      MOVE  "160710070"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  539900
      *           ∏°¬Œ∏°∫∫¥…Õ˝≤√ªª° £≤°À
                  WHEN  "160161610"
                      MOVE  "160709310"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  547700
      *           ¿∏≤Ω≥ÿ≈™∏°∫∫° £±°À»Ω√«Œ¡
                  WHEN  "160061910"
                      MOVE  "160703710"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ¿∏≤Ω≥ÿ≈™∏°∫∫° £≤°À»Ω√«Œ¡
                  WHEN  "160062010"
                      MOVE  "160703810"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           «¢°¶ µ ÿ≈˘∏°∫∫»Ω√«Œ¡
                  WHEN  "160061710"
                      MOVE  "160703510"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           »˘¿∏ ™≥ÿ≈™∏°∫∫»Ω√«Œ¡
                  WHEN  "160062210"
                      MOVE  "160704010"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           Ã»±÷≥ÿ≈™∏°∫∫»Ω√«Œ¡
                  WHEN  "160062110"
                      MOVE  "160703910"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
                END-EVALUATE
              END-IF
            END-IF
      *
            MOVE    SRY-SRYCD(IDX2)    TO    WRK-SRYCD
            MOVE    RECE85-SRYYM       TO    WK-TAIHI-SRYYM
            MOVE    WK-TAIHI-DAY       TO    WK-TAIHI-SRYD
            PERFORM    900-TENSU-TBL-SEC
      *
            IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE84-MEISAISU  =  RECE84-MEISAISU
                                             -  1
            END-IF
      *
            IF       SRY-SRYCD(IDX2)(1:5)  =
                     "09960" OR "09970" OR "09980"
                  MOVE    1     TO    FLG-HENSYU-NG
            END-IF
      *
            IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
            OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
            OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                    (TNS-TEN               =  ZERO )       )
            OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
            OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                           OR "0086"       )
            OR     ((SRY-SRYSYUKBN  = "330" OR "331" OR
                                      "332" OR "350" OR
                                      "352"               ) AND
                    ((SRY-SRYCD(IDX2)(1:1) =   "8"       ) OR
                     (SRY-SRYCD(IDX2)(1:3) =   "008"     )) AND
                    ( ACCT-ZAITEN      NOT =   ZERO       ) AND
                    ( ACCT-SYUTEN1         =   ZERO       ) AND
                    ( ACCT-ZAIKAISU    NOT =   ZERO       ))
            OR     ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                   "60" OR "64" OR "70" OR
                                   "80"                   ) AND
                    (ACCT-ZAITEN       NOT =  ZERO        ) AND
                    (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                    (ACCT-JIHOKBN          =  SPACE       ) AND
                    (ACCT-YKZTEN           =  ZERO        ) AND
                    (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
            OR     ( FLG-HENSYU-NG     NOT =  ZERO         )
            OR     ( SRY-SRYCD(IDX2)       =
                     "099209902" OR "099209903" OR
                     "099209904" OR "099209905" OR
                     "099209906" OR "099209907" OR
                     "099209908" OR
                     "099999903" OR "099999904" OR
                     "099999905" OR "099999906" OR
                     "099999907" OR "099999908" OR
                     "099999909" OR "099999910" OR
                     "099999921" OR
                     "099830101"                           )
            OR     ( SRY-SRYCD(IDX2)       =  "099509901"  )
                  COMPUTE   RECE84-MEISAISU    =  RECE84-MEISAISU
                                               -  1
                  MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
                  COMPUTE   RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                               -  WRK-SRYCD
                  IF ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                     "60" OR "64" OR "70" OR
                                     "80"                   ) AND
                      (ACCT-ZAITEN       NOT =  ZERO        ) AND
                      (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                      (ACCT-JIHOKBN          =  SPACE       ) AND
                      (ACCT-YKZTEN           =  ZERO        ) AND
                      (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
                     COMPUTE   RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                                  -  SRY-SRYSURYO(IDX2)
                  ELSE
                     COMPUTE   RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                                  -  1
                  END-IF
            END-IF
      *
            IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                 "6" OR  "7" OR
                                                 "8"
      *
             IF  (  SRY-SRYSYUKBN            = "330" OR "331"  OR      
                                               "332" OR "350"  OR
                                               "352" )
             AND (( SRY-SRYCD(IDX2)(1:3)     = "059"             ) OR  
                  ( SRY-SRYCD(IDX2)(1:1)     = "7"               ) OR  
                  ( SRY-SRYCD(IDX2)          = CONS-ZANHAIKI     ) OR  
                  ( SRY-SRYCD(IDX2)(1:1)     = "6"               ))    
             AND (  ACCT-ZAITEN          NOT = ZERO  )
      *******AND (  ACCT-SYUTEN1             = ZERO  )
             AND (  ACCT-ZAIKAISU        NOT = ZERO  )
      *
              MOVE    1                  TO    FLG-TENTEKI             
      *
             ELSE                                                      
      *
              IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
              OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                      (TNS-TEN               =  ZERO )       )
              OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
              OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                             OR "0086"       )
              OR     ((SRY-SRYSYUKBN  = "330" OR "331" OR
                                        "332" OR "350" OR
                                        "352"               ) AND
                      ((SRY-SRYCD(IDX2)(1:1) =   "8"       ) OR
                       (SRY-SRYCD(IDX2)(1:3) =   "008"     )) AND
                      ( ACCT-ZAITEN      NOT =   ZERO       ) AND
                      ( ACCT-SYUTEN1         =   ZERO       ) AND
                      ( ACCT-ZAIKAISU    NOT =   ZERO       ))
              OR     ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                     "60" OR "64" OR "70" OR
                                     "80"                   ) AND
                      (ACCT-ZAITEN       NOT =  ZERO        ) AND
                      (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                      (ACCT-JIHOKBN          =  SPACE       ) AND
                      (ACCT-YKZTEN           =  ZERO        ) AND
                      (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
              OR     ( FLG-HENSYU-NG     NOT =  ZERO         )
              OR     ( SRY-SRYCD(IDX2)       =
                       "099209902" OR "099209903" OR
                       "099209904" OR "099209905" OR
                       "099209906" OR "099209907" OR
                       "099209908" OR
                       "099999903" OR "099999904" OR
                       "099999905" OR "099999906" OR
                       "099999907" OR "099999908" OR
                       "099999909" OR "099999910" OR
                       "099999921" OR
                       "099830101"                           )
              OR     ( SRY-SRYCD(IDX2)       =  "099509901"  )
      *
               CONTINUE
      *
              ELSE
      *
               ADD     1                  TO    IDX4    
      *
               MOVE    SRY-SRYCD(IDX2)    TO    RECE85-SRYCD(IDX4)
      *
               IF      SRY-SRYCD(IDX2)    =     CONS-BYOITAIYO
                  EVALUATE  RECE82-HOSPSBT
                     WHEN     "1"
                          MOVE     "3"    TO    RECE82-HOSPSBT
                     WHEN     "2"
                          MOVE     "4"    TO    RECE82-HOSPSBT
                  END-EVALUATE
               END-IF
      *
               IF      SRY-SRYKBN         =     "13"
                   IF   SRY-SRYCD(IDX2)   =     CONS-SIDORYO1    OR
                                                CONS-SIDORYO2    OR
                                                CONS-SIDORYO3    OR
                                                CONS-SIDORYO4    OR
                                                CONS-SIDORYO5    OR
                                                CONS-SIDORYO6
                        MOVE   "1"        TO    RECE82-BEDSUFLG
                   END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)      =    CONS-GAIJINKEN
                  MOVE   "1"              TO    RECE82-KENTAIKASANFLG
                  MOVE   99               TO    RECE84-KNSJISGRPKBN
               END-IF
               IF     RECE84-KNSJISGRPKBN  =    ZERO
                 IF    TNS-KNSJISGRPKBN = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                  MOVE  TNS-KNSJISGRPKBN  TO    RECE84-KNSJISGRPKBN
                 END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN         =     "60" OR "64" )
               AND   ( TNS-KNSJISKBN      =      2           )
                  MOVE  TNS-KNSJISKBN     TO    RECE84-KNSJISKBN
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "14" OR "21" OR "22" OR "23" )
               AND   ( RECE84-ZAITEN     =     ZERO                 )
               AND   ( RECE84-SAFLG      =     SPACE                )
               AND   ( ACCT-ZAIKBN   NOT =     1                    )
                  IF   SRY-SRYCD(IDX2)   =     "820000046"
                     MOVE     "1"         TO    RECE84-SAFLG
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "21" OR "22" OR "23" )
               AND   ( RECE84-ZAITEN     =     ZERO         )
               AND   ( ACCT-ZAIKBN   NOT =     1            )
                  IF     RECE84-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE84-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "212" OR "292" OR
                                               "222" OR "232"
                          IF   RECE84-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE84-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE84-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN        =     "14" )
               AND   ( RECE84-ZAITEN     =     ZERO )
               AND   ( ACCT-ZAIKBN   NOT =     1    )
                  IF     RECE84-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE84-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "148" OR "149"
                          IF   RECE84-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE84-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE84-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)  IS  NUMERIC
      *          IF  SRY-SRYCD(IDX2)(1:3) NOT = "001"
                   MOVE    SRY-SRYCD(IDX2)   TO  WRK-SRYCD
      *
                   MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                   PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                             UNTIL   FUKA-IDX   >    9
                     MOVE   SRY-SRYCD(IDX2)(FUKA-IDX:1)
                                             TO  WRK-FUKA-VALUE
                     IF  WRK-FUKA-VALUE  =  ZERO
                       CONTINUE
                     ELSE
                       COMPUTE  RECE84-RUIKEITOTAL  =
                                RECE84-RUIKEITOTAL  +
                              ( WRK-FUKA-VALUE    * WRK-SRYCD)
                       IF  (ACCT-SRYKBN          =  "80" )
                       AND (SRY-SRYCD(IDX2)(1:5) =
                            "09591" OR "09592" OR "09593"
                                               OR "09594")
                       AND (SRY-SRYSYUKBN        =  "809")
                        COMPUTE WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-JIHIMONEY(IDX2))
                       ELSE
                        COMPUTE WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-SRYSURYO(IDX2))
                       END-IF
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE84-RUIKEITOTAL  =
                            RECE84-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *
                   MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                   PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                             UNTIL   FUKA-IDX   >   10
                     MOVE   SRY-SRYSURYO(IDX2)(FUKA-IDX:1)
                                             TO  WRK-FUKA-VALUE
                     IF  WRK-FUKA-VALUE  =  ZERO
                       CONTINUE
                     ELSE
                       COMPUTE  WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX)
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE84-RUIKEITOTAL  =
                            RECE84-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *          END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE85-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE85-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE85-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE85-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE85-INPUTNUM(IDX4)
      *
               IF   RECE82-HKNKBN    NOT  =  "4"                        
                  IF ((SRY-SRYCD(IDX2)  =  CONS-ROUSAI3    ) AND
                      (RECE82-SRYYM     <  "200804"        ))
                  OR ((SRY-SRYCD(IDX2)  =
                       CONS-ROUSAI2  OR
                       CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                       CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                       CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                       CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                       CONS-ROUSAI12                       ) AND
                      (RECE82-SRYYM    <=  "201306"        ))
                  OR ((SRY-SRYCD(IDX2)  =
                       CONS-ROUSAI42 OR
                       CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                       CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                       CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                       CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                       CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                       CONS-ROUSAI40 OR  CONS-ROUSAI41     ) AND
                      (RECE82-SRYYM    >=  "201307"        ))
                          MOVE   "80"        TO    RECE84-SRYKBN
                                                   RECE85-SRYKBN
                  ELSE
                    IF      TNS-SRYKBN         =     "80"
                    OR      RECE84-SRYKBN      =     "80"
                      IF  TNS-NYUTENTTLKBN   =     130
                          MOVE   "13"        TO    RECE84-SRYKBN
                                                   RECE85-SRYKBN
                      END-IF
                    END-IF
                  END-IF
               ELSE                                                     
                  IF      SRY-SRYCD(IDX2)    =     CONS-YOUBUKASAN
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-YOUBUKASAN   =  WRK-YOUBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-KYOBUKASAN
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KYOBUKASAN   =  WRK-KYOBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-KEIBUKASAN
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KEIBUKASAN   =  WRK-KEIBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-SOGAIKASAN
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-SOGAIKASAN   =  WRK-SOGAIKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
      *           º´«Â¿’°¶Œ≈Õ‹√¥≈ˆºÍ≈ˆÕ—∆√Œ„ΩËÕ˝
      *           ° º´«Â¿’§ŒæÏπÁ°¢£±£∞≈¿§Ú£±£≤£∞±ﬂ§À∆…§ﬂ¬ÿ§®§Î°À
      *           ° Œ≈Õ‹√¥≈ˆºÍ≈ˆ°ß£±∑Ó£±≤Û§ŒªªƒÍ°À
                  IF      SRY-SRYCD(IDX2)    =     CONS-RYOYOTEATE
                    EVALUATE  RECE82-JIBAIKBN
                      WHEN      "1"
                        MOVE     120    TO     WRK-RYOYOTEATE2
                        COMPUTE  WRK-RYOYOTEATE   =
                                 WRK-RYOYOTEATE   +
                                 WRK-RYOYOTEATE2
                      WHEN      "2"
                        COMPUTE  WRK-RYOYOTEATE   =  WRK-RYOYOTEATE
                                                  +  TNS-TEN
                        COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                                  -  TNS-TEN
                        COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                                  +  TNS-TEN
                    END-EVALUATE
                  END-IF
      *
                  IF ( ACCT-SRYKBN          =  "80"               ) AND
                     ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592" )
      ***            EVALUATE  SRY-SRYCD(IDX2)(1:5)
      ***             WHEN      "09591"
      ***               IF  RECE82-SINDAN-TUU < 2
      ***                 MOVE   "95"   TO    RECE84-SRYKBN
      ***                                     RECE85-SRYKBN
      ***               END-IF
      ***             WHEN      "09592"
      ***               IF  RECE82-MEISAI-TUU < 2
      ***                 MOVE   "95"   TO    RECE84-SRYKBN
      ***                                     RECE85-SRYKBN
      ***               END-IF
      ***            END-EVALUATE
                     MOVE    1          TO    FLG-SRYKBN-HEN
                  ELSE
                   IF ( SRY-SRYCD(IDX2)(1:5) = "09593"       )
                   OR ( SRY-SRYCD(IDX2)(1:5) = "09594"       )
                   OR ((SRY-SRYCD(IDX2)  =  CONS-ROUSAI3    ) AND
                       (RECE82-SRYYM     <  "200804"        ))
                   OR ((RECE82-JIBAIKBN  =  "1"             ) AND
                       (SRY-SRYCD(IDX2)  =  CONS-RYOYOTEATE ))
                   OR ((SRY-SRYCD(IDX2)  =
                        CONS-ROUSAI2  OR
                        CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                        CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                        CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                        CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                        CONS-ROUSAI12 OR
                        CONS-ROUSAI13 OR  CONS-ROUSAI14 OR
                        CONS-ROUSAI15 OR  CONS-ROUSAI16 OR
                        CONS-ROUSAI17 OR  CONS-ROUSAI18 OR
                        CONS-ROUSAI19 OR  CONS-ROUSAI20     ) AND
                       (RECE82-SRYYM    <=  "201306"        ))
                   OR ((SRY-SRYCD(IDX2)  >= "101910110" AND
                                         <= "101910400"     ) AND
                       (RECE82-SRYYM    <=  "201306"        ))
                   OR ((SRY-SRYCD(IDX2)  =
                        CONS-ROUSAI42 OR
                        CONS-ROUSAI22 OR  CONS-ROUSAI23 OR
                        CONS-ROUSAI24 OR  CONS-ROUSAI25 OR
                        CONS-ROUSAI26 OR  CONS-ROUSAI27 OR
                        CONS-ROUSAI28 OR  CONS-ROUSAI29 OR
                        CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                        CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                        CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                        CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                        CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                        CONS-ROUSAI40 OR  CONS-ROUSAI41     ) AND
                       (RECE82-SRYYM    >=  "201307"        ))
                   OR ((SRY-SRYCD(IDX2)  >= "101800570" AND
                                         <= "101800860"     ) AND
                       (RECE82-SRYYM    >=  "201307"        ))
                   OR ((SRY-SRYCD(IDX2)   = "101800550" OR
                             "101800560" OR "101800990"     ) AND
                       (RECE82-SRYYM    >=  "201307"        ))
                     IF   SRY-SRYCD(IDX2)(1:5) =  "09593"
                       MOVE  TNS-KANANAMECHGKBN
                                      TO  WRK-JIBAIKZI-SYUKEIKBN(IDX2)
                                          RECE84-JIBAISYUKEIKBN
                       EVALUATE  TNS-KANANAMECHGKBN
                         WHEN      0
                         WHEN      4
                         WHEN      5
                            MOVE   "80"   TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                         WHEN      1
                            MOVE   "11"   TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                       END-EVALUATE
                     ELSE
                       MOVE   "80"        TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                     END-IF
                     MOVE      1          TO  FLG-SRYKBN-HEN
                   ELSE                                                 
                     IF     TNS-SRYKBN         =   "80"
                     OR     RECE84-SRYKBN      =   "80"
                       IF   TNS-NYUTENTTLKBN   =   130
                            MOVE   "13"   TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                       END-IF
                     END-IF
                     IF  (WRK-PARA-JIBAIPRTKBN =  "0" )
                     AND (FLG-SRYKBN-HEN       =  ZERO)
                       IF (( RECE84-SRYKBN     =  "80" ) AND
                           ( RECE84-OKIKAEFLG  =  SPACE))
                       OR (  RECE84-OKIKAEFLG  =  "1"   )
                        IF   SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
                            CONTINUE
                        ELSE
                            MOVE   "1"    TO  RECE84-OKIKAEFLG
                            MOVE   "40"   TO  RECE84-SRYKBN       
                                              RECE85-SRYKBN       
                        END-IF
                       END-IF
                     END-IF
                     IF (( RECE84-SRYKBN     =  "14" ) AND
                         ( RECE84-OKIKAEFLG  =  SPACE))
                     OR (  RECE84-OKIKAEFLG  =  "2"   )
                            MOVE   "2"    TO  RECE84-OKIKAEFLG
                            MOVE   "80"   TO  RECE84-SRYKBN       
                                              RECE85-SRYKBN       
                     END-IF
                   END-IF                                               
                  END-IF                                                
      *
               END-IF                                                   
      *
               PERFORM  220211-RECE85-TENSOU-COMCNT-HEN-SEC
      *
               IF  SRY-SRYCD(IDX2)        =     CONS-ZANHAIKI           
                IF   ACCT-ZAIKBN  =  1
                  MOVE    "1"             TO  RECE85-RECEDENFLG(IDX4)
                  MOVE    "1"             TO  RECE85-RECEPTFLG(IDX4)
                  MOVE    ZERO            TO  RECE85-SRYSURYO(IDX4)
                ELSE
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.99999                   
                  COMPUTE RECE85-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE85-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE85-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE85-TANINAME(IDX4)   
                    IF    RECE85-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE85-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE85-SRYSURYO(IDX4 - 1) 
                    END-IF
                  ELSE
                    MOVE  "1"             TO  RECE85-RECEDENFLG(IDX4)
                    MOVE  "1"             TO  RECE85-RECEPTFLG(IDX4)
                  END-IF                                                
                END-IF
               ELSE                                                     
                  IF      TNS-TANIMOJISU    =   ZERO
                  AND     TNS-TANINAME  NOT =   SPACE
                     PERFORM   VARYING   IDW   FROM   24   BY  -1
                                UNTIL    IDW    <      1
                        IF   TNS-TANINAME(IDW:1)  NOT =   SPACE
                            COMPUTE  TNS-TANIMOJISU   =   IDW   /   2
                            MOVE    1     TO    IDW
                        END-IF
                     END-PERFORM
                  END-IF
                  MOVE    SRY-SRYSURYO(IDX2)                            
                                          TO    WRK-SRYSURYO            
                  IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO    RECE85-TANICD(IDX4)
                                                WRK-TANICD
                  END-IF
                  MOVE    TNS-TANIMOJISU  TO    RECE85-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE85-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
               IF ( ACCT-SRYKBN          =  "80"              ) AND     
                  ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                         OR "09593" OR "09594")         
                    MOVE  1               TO  RECE85-TENSIKIBETU(IDX4)  
                    IF     (RECE82-HKNKBN        =  "4"               )
                    AND    (SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                                            OR "09593")
                      IF    SRY-SRYCD(IDX2)(1:5) =  "09593"
                        PERFORM   220212-JIBAI-KIZAI-TENSU-HENSYU-SEC
                        MOVE  WRK-TEN-KZI TO  RECE85-TEN(IDX4)
                        MOVE  WRK-TEN-KZI TO  WRK-JIBAIKZI-TNS(IDX2)
                      ELSE
                        PERFORM   220214-JIBAI-BUNSYO-TENSU-HENSYU-SEC
                        MOVE  WRK-TEN-BUN TO  RECE85-TEN(IDX4)
                        MOVE  WRK-TEN-BUN TO  WRK-JIBAIKZI-TNS(IDX2)
                      END-IF
                    ELSE
                      IF  SRY-SRYSYUKBN = "809"
                        MOVE   SRY-JIHIMONEY(IDX2)
                                          TO  RECE85-TEN(IDX4)
                        MOVE   SRY-JIHIMONEY(IDX2)
                                          TO  WRK-JIBAIKZI-TNS(IDX2)
                      ELSE
                        MOVE   TNS-TEN    TO  RECE85-TEN(IDX4)
                        MOVE   TNS-TEN    TO  WRK-JIBAIKZI-TNS(IDX2)
                      END-IF
                    END-IF
               ELSE                                                     
                 IF   RECE82-HKNKBN     =  "4"
                 AND  RECE82-JIBAIKBN   =  "1"
                 AND  SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
                    MOVE  1               TO  RECE85-TENSIKIBETU(IDX4)  
                    MOVE  WRK-RYOYOTEATE2 TO  RECE85-TEN(IDX4)
                 ELSE
                    MOVE  TNS-TENSIKIBETU TO  RECE85-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN         TO  RECE85-TEN(IDX4)
                 END-IF
               END-IF                                                   
               MOVE    TNS-NYUTENTTLKBN   TO    RECE85-TENTTLKBN(IDX4)
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO    RECE85-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE85-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO    RECE85-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE85-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE85-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE85-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE85-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE85-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE85-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE85-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE85-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE85-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-SRYSYUKBN  TO RECE85-SRYSYUKBN-X  (IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE85-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE85-KNSJISGRPKBN   (IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO RECE85-KENTAICOMMENT  (IDX4)
               MOVE    TNS-KOKUJISKBKBN1
                                      TO RECE85-KOKUJISKBKBN1  (IDX4)
               MOVE    TNS-KOKUJISKBKBN2
                                      TO RECE85-KOKUJISKBKBN2  (IDX4)
               MOVE    TNS-TUSOKUGAIKBN
                                      TO RECE85-TUSOKUGAIKBN   (IDX4)
               MOVE    TNS-TIMEKSNKBN TO RECE85-TIMEKSNKBN     (IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE85-CHUKSNCD       (IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE85-CHUKSNTUBAN    (IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO RECE85-TOKUKIZAISBT1  (IDX4)
               MOVE    TNS-SANSOKBN   TO RECE85-SANSOKBN       (IDX4)
               IF      TNS-SRYCD (1:2)   =   "84"
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       10
                       MOVE    TNS-SSTKIJUNCD (IDYY)
                                      TO RECE85-SSTKIJUNCD(IDX4 IDYY)
                   END-PERFORM
               END-IF
      *
               IF    (TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490")
               AND   (RECE82-SRYYM   <=  "201003"   )
                      MOVE   202      TO  RECE85-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE85-CDKBN-KOUBAN(IDX4)
               END-IF
               IF     TNS-SRYCD       =  "170016010"  OR
                                         "170020270"  OR  "170024470" OR
                                         "170024910"  OR  "170025010" OR
                                         "170025210"  OR  "170025310" OR
                                         "170025410"  OR  "170025510" OR
                                         "170025610"  OR  "170025710" OR
                                         "170025810"  OR  "170025910" OR
                                         "170026010"  OR  "170026110" OR
                                         "170026210"  OR  "170026310" OR
                                         "170026810"  OR
                                         "170000210"  OR  "170016910" OR
                                         "170017010"  OR  "170026710" OR
                                         "170028810"
                      MOVE   203      TO  RECE85-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE85-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               IF    (TNS-SRYCD       =  "170022290")
               AND   (RECE82-SRYYM   >=  "201004"   )
                      MOVE   "1"      TO  RECE84-GAZOGEN
               END-IF
      *
               IF    ((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (SRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 900-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT
                                           TO  RECE85-NAME(IDX4)
                                               RECE85-INPUTCOMENT(IDX4)
      *
                   IF      SRY-SRYCD (IDX2)(1:1)   =   "8"
                       IF      PTCOM-INPUTCHI1     =   SPACE
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           MOVE    TNS-NAME        TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-MAX     >   ZERO )
                               COMPUTE IDXX    =   KANACHK-MAX +   1
                               MOVE    PTCOM-INPUTCOMENT (IDXX:)
                                                       TO
                                           RECE85-INPUTCOMENT   (IDX4)
                           ELSE
                               MOVE    PTCOM-INPUTCOMENT
                                                       TO
                                           RECE85-INPUTCOMENT   (IDX4)
                           END-IF
                       ELSE
                           INITIALIZE              ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           STRING  PTCOM-INPUTCHI1 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI2 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI3 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI4 DELIMITED  BY  SPACE
                                   INTO   KANACHK-MAE-INPUT
                           END-STRING
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF      KANACHK-RC      =   ZERO
                               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO
                                           RECE85-INPUTCOMENT   (IDX4)
                           END-IF
                       END-IF
                   END-IF
      *
      ***          MOVE   "1"                  TO    RECE84-COMCDFLG
               ELSE
                   MOVE    TNS-NAME        TO  RECE85-NAME(IDX4)
                                               RECE85-INPUTCOMENT(IDX4)
               END-IF
      *
               IF  SRY-SRYCD(IDX2)        =     CONS-ZANHAIKI
                  IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO
                    PERFORM  220215-RECE85-TENSOU-SURYO-HEN-SEC
      *
                    MOVE    SPACE      TO  RECE85-NAME(IDX4)
                    MOVE    SPACE      TO  RECE85-INPUTCOMENT(IDX4)
                    STRING  TNS-NAME          DELIMITED  BY  " "
      ***                   "  "              DELIMITED  BY  SIZE
                            "°°"              DELIMITED  BY  SIZE
                            WRK-SRYSURYOW     DELIMITED  BY  " "
                            RECE85-TANINAME(IDX4)
                                              DELIMITED  BY  " "
                            INTO              RECE85-NAME(IDX4)
                    END-STRING
                    MOVE  RECE85-NAME(IDX4)
                                       TO  RECE85-INPUTCOMENT(IDX4)
      *
      ***           MOVE  "810000001"  TO  RECE85-SRYCD(IDX4)
                    MOVE  ZERO         TO  RECE85-SRYSURYO(IDX4)
                    MOVE  ZERO         TO  RECE85-TANICD(IDX4)
                    MOVE  ZERO         TO  RECE85-TANIMOJISU(IDX4)
                    MOVE  SPACE        TO  RECE85-TANINAME(IDX4)
                    MOVE  ZERO         TO  RECE85-YUKOKETA(IDX4)
                    MOVE  "2"          TO  RECE85-MEISKYFLG(IDX4)
                  END-IF
               END-IF
      *
               IF    (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                        MOVE     "1"       TO    RECE84-COMCDFLG
               END-IF
      *
               IF  ( SRY-SRYKBN     =  "60" OR "64" )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
               AND ( FLG-KENSA-COM  =   ZERO        )
                 IF  SRY-SRYCD(IDX2) = "840000085" OR "840000087"
                    CONTINUE
                 ELSE
                    PERFORM  VARYING  IDV2  FROM  1  BY  2
                              UNTIL  (IDV2           >   200)
                               OR    (FLG-KENSA-COM  =    1 )
                      IF  RECE85-NAME(IDX4)(IDV2:2)   =  "∆¸"
                        MOVE   SPACE  TO  RECE84-FLG-KENSA-COMMENT
                        MOVE     1    TO  FLG-KENSA-COM
                      END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)(1:3)     =   "058"
                 MOVE  TNS-FORMALYUKOKETA TO  RECE85-YUKOKETA(IDX4)
                 MOVE  TNS-FORMALNAME     TO  RECE85-NAME(IDX4)
                                              WRK-FORMALNAME
                 MOVE  "1"                TO  RECE85-RECEDENFLG(IDX4)
      *
                 MOVE     TNS-TEN         TO  WRK-KZI058-TEN
                 IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO  WRK-KZI058-TANICD
                    MOVE  TNS-TANIMOJISU  TO  WRK-KZI058-TANIMOJISU
                    MOVE  TNS-TANINAME    TO  WRK-KZI058-TANINAME
                 END-IF
                 MOVE     ZERO            TO  RECE85-TEN(IDX4)
                 MOVE     ZERO            TO  RECE85-TANICD(IDX4)
                 MOVE     ZERO            TO  RECE85-TANIMOJISU(IDX4)
                 MOVE     SPACE           TO  RECE85-TANINAME(IDX4)
               END-IF
               IF  (SRY-SRYCD(IDX2)(1:1)    =   "7")
               AND (SRY-MEISKYFLG(IDX2)     =   "1")
                 IF   (SRY-SRYKBN  =   "70")
                 AND  (TNS-DATAKBN =   "3" )
                    IF    RECE85-RENNUM  >  1
                    AND   IDX4           =  1
                      MOVE    1           TO  FLG-FORMAL-KISAI
                      COMPUTE  WRK-RENNUM2 =  RECE85-RENNUM  -  1
                    ELSE
                      MOVE  "1"           TO  RECE85-RECEPTFLG(IDX4 - 1)
                    END-IF
                 ELSE
                    MOVE  WRK-FORMALNAME  TO  RECE85-RECEDENNAME(IDX4)
                    MOVE  "1"             TO  RECE85-RECEHENFLG(IDX4)
                    IF    WRK-KZI058-TEN  NOT = ZERO
                       MOVE   WRK-KZI058-TEN
                                          TO  RECE85-TEN(IDX4)
                    END-IF
                    IF   (RECE85-TANICD(IDX4)    = ZERO)
                    AND  (WRK-KZI058-TANICD  NOT = ZERO)
                       MOVE   WRK-KZI058-TANICD
                                          TO  RECE85-TANICD(IDX4)
                       MOVE   WRK-KZI058-TANIMOJISU
                                          TO  RECE85-TANIMOJISU(IDX4)
                       MOVE   WRK-KZI058-TANINAME
                                          TO  RECE85-TANINAME(IDX4)
                    END-IF
                 END-IF
               END-IF
      *
               IF  (SRY-SRYKBN         =  "99"       )
               AND (SRY-RENNUM         =   1         )
               AND (IDX2               =   1         )
               AND (SRY-SRYCD(IDX2)    =  "099999902")
                    MOVE      1         TO  FLG-TAHOKEN
               END-IF
               IF  (FLG-TAHOKEN           =   1              )
               AND (SRY-RENNUM            =   1              )
               AND (IDX2                  =   2              )
               AND (SRY-SRYCD(IDX2)(1:4)  =  "0082"          )
               AND (TNS-NAME(1:14)        =  " ›∏±ÃææŒ —ππ°ß")
                  MOVE     "1"          TO  RECE85-RECEPTFLG(IDX4)
                  MOVE     "1"          TO  RECE85-RECEDENFLG(IDX4)
      *
                  MOVE      ZERO        TO  IDW3
                  PERFORM   VARYING   IDW   FROM   200   BY  -1
                             UNTIL    IDW     <    15
                       IF   TNS-NAME(IDW:1)  NOT =   SPACE
                           COMPUTE IDW6       =  (IDW / 2) - 7
                           MOVE    IDW        TO  IDW3
                           MOVE    1          TO  IDW
                       END-IF
                  END-PERFORM
                  IF     IDW3    NOT =  ZERO
                    MOVE  SPACE TO RECE85-NAME(IDX4 - 1)
                    COMPUTE   IDW4  =  IDW3  -  14
                    MOVE  TNS-NAME(15:IDW4)
                                TO RECE85-NAME(IDX4 - 1)(1:IDW4)
                    COMPUTE   IDW5  =  IDW4  +   1
                    MOVE  "§À§∆ø«ª°Œ¡ªªƒÍ∫—"
                                TO RECE85-NAME(IDX4 - 1)(IDW5:16)
                    COMPUTE   RECE85-YUKOKETA(IDX4 - 1) =
                              RECE85-YUKOKETA(IDX4 - 1) -
                              3                         +
                              IDW6
                  END-IF
               END-IF
      *
               IF  RECE85-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   200   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE85-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE85-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF (  SRY-SRYCD(IDX2) = CONS-ROUSAI1  OR
                                       CONS-ROUSAI21   )                
               OR (( ACCT-SRYKBN          =  "80"     )   AND           
                   ( SRY-SRYCD(IDX2)(1:5) =  "09591"
                                         OR  "09592"
                                         OR  "09593"
                                         OR  "09594"  ))                
               OR (( SRY-SRYCD(IDX2) = CONS-ROUSAI3   )   AND
                   ( RECE82-SRYYM    < "200804"       ))
               OR (( SRY-SRYCD(IDX2) =
                     CONS-ROUSAI2  OR
                     CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                     CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                     CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                     CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                     CONS-ROUSAI12 OR
                     CONS-ROUSAI13 OR  CONS-ROUSAI14 OR
                     CONS-ROUSAI15 OR  CONS-ROUSAI16 OR
                     CONS-ROUSAI17 OR  CONS-ROUSAI18 OR
                     CONS-ROUSAI19 OR  CONS-ROUSAI20  )   AND
                   ( RECE82-SRYYM   <= "201306"       ))
               OR (( SRY-SRYCD(IDX2) >= "101910110" AND
                                     <= "101910400"   )   AND
                   ( RECE82-SRYYM   <=  "201306"      ))
               OR (( SRY-SRYCD(IDX2) =
                     CONS-ROUSAI42 OR
                     CONS-ROUSAI22 OR  CONS-ROUSAI23 OR
                     CONS-ROUSAI24 OR  CONS-ROUSAI25 OR
                     CONS-ROUSAI26 OR  CONS-ROUSAI27 OR
                     CONS-ROUSAI28 OR  CONS-ROUSAI29 OR
                     CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                     CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                     CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                     CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                     CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                     CONS-ROUSAI40 OR  CONS-ROUSAI41  )   AND
                   ( RECE82-SRYYM   >= "201307"       ))
               OR (( SRY-SRYCD(IDX2) >= "101800570" AND
                                     <= "101800860"   )   AND
                   ( RECE82-SRYYM   >=  "201307"      ))
               OR (( SRY-SRYCD(IDX2)  = "101800550" OR
                         "101800560" OR "101800990"   )   AND
                   ( RECE82-SRYYM   >=  "201307"      ))
      *
                     ADD      1          TO  WRK-MEISAI-CNT
                     IF  SRY-SRYSYUKBN = "809"
                        COMPUTE  WRK-TEN = WRK-TEN +
                                          (SRY-JIHIMONEY(IDX2) / 10)
                     ELSE
                      IF SRY-SRYCD(IDX2)  =  "101800550" OR
                              "101800560" OR "101800990"
                        COMPUTE  WRK-FUTANWARI  = TNS-TEN / 100
                        COMPUTE  WRK-KEISAN-KG  =
                                (WRK-TEN-KG  * WRK-FUTANWARI) + 0.5
                        COMPUTE  WRK-TEN = WRK-TEN + WRK-KEISAN-KG
                        COMPUTE  WRK-KEISAN-KG2 =
                                (WRK-TEN-KG2 * WRK-FUTANWARI)
                      ELSE
                        COMPUTE  WRK-TEN = WRK-TEN +
                                          (TNS-TEN / 10)
                        COMPUTE  WRK-TEN-KG  = TNS-TEN / 10
                        COMPUTE  WRK-TEN-KG2 = TNS-TEN
                      END-IF
                     END-IF
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *
                     MOVE     "2"        TO    RECE85-SIKFLG(IDX4)
      *
                     IF  (ACCT-SRYKBN          = "80"              )
                     AND (SRY-SRYCD(IDX2)(1:5) =
                           "09591" OR "09592" OR "09593" OR "09594")
                       COMPUTE WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                               + WRK-JIBAIKZI-TNS(IDX2)
                     ELSE
                      IF  SRY-SRYCD(IDX2)  =  "101800550" OR
                               "101800560" OR "101800990"
                       COMPUTE WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                               + WRK-KEISAN-KG2
                       MOVE    WRK-KEISAN-KG2  TO  RECE85-TEN(IDX4)
                      ELSE
                       COMPUTE WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                               + TNS-TEN
                      END-IF
                     END-IF
      *
               ELSE
      *
                  IF   RECE82-HKNKBN     =  "4"
                  AND  RECE82-JIBAIKBN   =  "1"
                  AND  SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
      *
                     ADD      1          TO  WRK-MEISAI-CNT
                     COMPUTE  WRK-TEN    =   WRK-TEN   +
                                             TNS-TEN
      *
                     COMPUTE  WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                              + WRK-RYOYOTEATE2
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *
                     MOVE     "2"        TO  RECE85-SIKFLG(IDX4)
      *
                  ELSE
      *
                    IF  ((RECE82-HKNKBN      =   "4") AND
                         (RECE82-JIBAIKBN    =   "2"))
                    OR  ((RECE82-HKNKBN      =   "5") AND
                         (RECE82-KOMUKBN     =   "2"))
                      IF (( ACCT-SRYKBN          =  "80"    ) AND
                          ( WRK-CHECK-SRYCD(1:5) =  "09591"
                                                 OR "09592"
                                                 OR "09593"
                                                 OR "09594" ) AND
                          ( SRY-SRYCD(IDX2)(1:1) =   "8"    ))
                      OR (( ACCT-SRYKBN          =  "80"    ) AND
                          ( WRK-CHECK-SRYCD(1:5) =  "09591"
                                                 OR "09592"
                                                 OR "09593"
                                                 OR "09594" ) AND
                          ( SRY-SRYCD(IDX2)(1:3) =   "008"  ))
      *
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE85-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE84-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE84-COMCDFLG
                         END-IF
                      ELSE
                         MOVE  "1"     TO    RECE85-SIKFLG(IDX4)
                      END-IF
                    ELSE
                      IF (  SRY-SRYCD(IDX2) = CONS-ROUCOM1  OR
                                              CONS-ROUCOM2  OR
                                              CONS-ROUCOM3  OR
                                              CONS-ROUCOM4  OR
                                              CONS-ROUCOM5  OR
                                              CONS-ROUCOM6  OR
                                              CONS-ROUCOM7       )
                      OR (( ACCT-SRYKBN     = "11"              ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( SRY-SRYCD(IDX2) NOT = "830000020"   ) AND
                          ( ACCT-ZAIKBN     NOT =  1 AND 2      ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( WRK-CHECK-SRYCD =  CONS-ROUSAI3     ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM    <  "200804"         ))
                      OR (( ACCT-SRYKBN            =  "80"      ) AND
                          ( WRK-CHECK-SRYCD(1:5)   =  "09591"
                                                   OR "09592"
                                                   OR "09593"
                                                   OR "09594"   ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ))
                      OR (( WRK-CHECK-SRYCD = CONS-ROUSAI2      ) AND
                          ( RECE82-SRYYM   <=  "201306"         ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( WRK-CHECK-SRYCD =
                            CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                            CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                            CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                            CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                            CONS-ROUSAI12                       ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   <=  "201306"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD =
                            CONS-ROUSAI13 OR  CONS-ROUSAI14 OR
                            CONS-ROUSAI15 OR  CONS-ROUSAI16 OR
                            CONS-ROUSAI17 OR  CONS-ROUSAI18 OR
                            CONS-ROUSAI19 OR  CONS-ROUSAI20     ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   <=  "201306"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD >= "101910110" AND
                                            <= "101910400"      ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   <=  "201306"         ))
                      OR (( WRK-CHECK-SRYCD = CONS-ROUSAI42     ) AND
                          ( RECE82-SRYYM   >=  "201307"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD =
                            CONS-ROUSAI22 OR  CONS-ROUSAI23 OR
                            CONS-ROUSAI24 OR  CONS-ROUSAI25 OR
                            CONS-ROUSAI26 OR  CONS-ROUSAI27 OR
                            CONS-ROUSAI28 OR  CONS-ROUSAI29     ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   >=  "201307"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD =
                            CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                            CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                            CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                            CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                            CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                            CONS-ROUSAI40 OR  CONS-ROUSAI41     ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   >=  "201307"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD >= "101800570" AND
                                            <= "101800860"      ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   >=  "201307"         ))
                       IF   WRK-CHECK-SRYCD(1:6) = "101989"
                         MOVE  "1"     TO    RECE85-SIKFLG(IDX4)
                       ELSE
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE85-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE84-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE84-COMCDFLG
                         END-IF
                       END-IF
                      ELSE
                         MOVE  "1"     TO    RECE85-SIKFLG(IDX4)
                      END-IF
                    END-IF
      *
                  END-IF
      *
               END-IF
      *
      *        ∆√ƒÍ¥Ô∫‡° ≤Ë¡¸ø«√«§Œ•’•£•Î•‡∞ ≥∞°À§Œ§»§≠√±≤¡§Ú ‘Ω∏
               IF      SRY-SRYCD (IDX2) (1:1)  =   "7"
      ***          IF   (( SRY-SRYKBN               =   "70" )  AND
      ***                ( TNS-DATAKBN              =   "3"  ))
                   IF   (  TNS-DATAKBN              =   "3"   )
                   OR   (  SRY-SRYCD (IDX2)         =
                                    CONS-FILM-YOUJI-KASAN  OR  
                                    CONS-SANSO             OR
                                    CONS-SANSO2               )
                       CONTINUE
                   ELSE
                     IF      TNS-TEN         =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM 220213-RECE85-TENSOU-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE85-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE85-NAME(IDX4)(IDW:)   
                       COMPUTE RECE85-YUKOKETA(IDX4)
                                               =   RECE85-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF
      *
               IF    SRY-SRYKBN     =  "14"
      *
                 IF (SRY-SRYCD(IDX2) =  "114001110")
                 OR (SRY-SRYCD(IDX2) =  "114012910")
                 OR (SRY-SRYCD(IDX2) =  "114018010")
                    MOVE        9         TO    FLG-OSINKBN
                 END-IF
      *
                 EVALUATE    SRY-SRYCD(IDX2)
      *±˝ø«° ∆√ Ã±˝ø«°À
                   WHEN    "114000110"
                   WHEN    "114001610"
                   WHEN    "114000970"
                   WHEN    "114002470"
                   WHEN    "114002970"
                   IF  FLG-OSINKBN        NOT = 2  AND  3  AND  9
                       MOVE     1         TO    FLG-OSINKBN
                   END-IF
      *ÃÎ¥÷
                   WHEN    "114000470"
                   WHEN    "114001970"
                   WHEN    "114011670"
                   WHEN    "114011970"
                   WHEN    "114017570"
                   WHEN    "114017870"
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     2         TO    FLG-OSINKBN
                   END-IF
      *øºÃÎ°¶∂€µﬁ
                   WHEN    "114000370"
                   WHEN    "114000570"
                   WHEN    "114001870"
                   WHEN    "114002070"
                   WHEN    "114011570"
                   WHEN    "114011770"
                   WHEN    "114011870"
                   WHEN    "114012070"
                   WHEN    "114017470"
                   WHEN    "114017770"
                   WHEN    "114017670"
                   WHEN    "114017970"
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     3         TO    FLG-OSINKBN
                   END-IF
                 END-EVALUATE
               END-IF
      *
               MOVE    ACCT-ZAIKBN  TO  RECE84-ZAIKBN
      *
               IF      ACCT-ZAIKBN       =     1
                 MOVE   ZERO        TO  RECE85-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE85-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE85-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE85-TANINAME(IDX4)
                 IF  (SRY-SRYCD(IDX2)(1:1) = "6" OR "7")
                 OR  (SRY-SRYCD(IDX2)(1:3) = "059"     )
                 OR  (SRY-SRYCD(IDX2)(1:2) = "63"      )
                     MOVE  "1"          TO  RECE84-COMCDFLG
                     MOVE  "810000001"  TO  RECE85-SRYCD(IDX4)
                 END-IF
               END-IF
               IF      ACCT-ZAIKBN       =     2
                 MOVE   ZERO        TO  RECE85-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE85-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE85-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE85-TANINAME(IDX4)
               END-IF
      *
               PERFORM  220216-SETCOM-HENSYU-SEC
      *
      *        •Ï•ª≈≈° ≈¿≈©§Œµ≠œøΩÁ ¬§”¬ÿ§®Õ—°À
               IF      SRY-SRYKBN         =     "33"
               AND     FLG-HENSYU-OK      =     ZERO
                 PERFORM  220217-TENTEKI-CDKBN-KBNNUM-HENSYU-SEC
                 MOVE     1     TO    FLG-HENSYU-OK
               END-IF
      *
      *        £∞£π£µ£π£≥§«ªœ§ﬁ§Î•≥°º•…§¨§¢§ÎæÏπÁ°¢œ´∫“•Ï•ª≈≈§Œ∫Ó¿Æ¬–æ›≥∞
               IF    (RECE82-HKNKBN           =   "1" OR "2")
               AND   (RECE82-SRYYM           >=   "201307"  )
               AND   (RECE82-RSIIRYOKBN   NOT =   "0"       )
               AND   (SYS-200501-ROURECEKBN   =   "2" OR "3")
                 IF  (ACCT-SRYKBN          =  "80"   )
                 AND (SRY-SRYCD(IDX2)(1:5) =  "09593")
                    MOVE     1      TO  RECE82-ORIGINALCDKBN
                 END-IF
               END-IF
      *
              END-IF
             END-IF
            END-IF  
           END-PERFORM
      *
           IF    IDX4    NOT =   ZERO
               MOVE    1     TO    FLG-WRITE85X
           END-IF
      *
           .
       22021-RECE85-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»•≥•·•Û•»•’•È•∞ ‘Ω∏ΩËÕ˝
      *****************************************************************
       220211-RECE85-TENSOU-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE84-SRYKBN      =     "11"
      *       ΩÈø«° ∆±∆¸ £øÙø«Œ≈≤ °À
              IF  (SRY-SRYCD(IDX2)    =     CONS-SYODOU-COMMENT
                                      OR    CONS-SYODOU3-COMMENT
                                      OR    CONS-SYODOU4-COMMENT)
              AND (FLG-SYODOU-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE84-FLG-SYODOU-COMMENT
                   MOVE   SRY-SRYKA   TO    RECE84-SYODOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYODOU2-COMMENT
                   MOVE  SPACE  TO    RECE84-FLG-SYODOU-COMMENT
                   MOVE  SPACE  TO    RECE84-SYODOU-SRYKA(1)
                   MOVE    1    TO    FLG-SYODOU-COM
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "40"
      *       ≤√£∏£∞˙° »Øæ…∆¸µ≠∫‹°À
              IF   RECE82-SRYYM      >=    "200604"
              AND  RECE82-SRYYM      <=    "200803"
                IF   SRY-SRYCD(IDX2)  =     CONS-KAITATU-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-KAITATU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201004"
      *         øÕπ©ø’¬°≈˘
                IF   SRY-SRYCD(IDX2)  =     CONS-JINZOU1-COMMENT
                                        OR  CONS-JINZOU2-COMMENT
                                        OR  CONS-JINZOU3-COMMENT
                                        OR  CONS-JINZOU4-COMMENT
                                        OR  CONS-JINZOU5-COMMENT
                                        OR  CONS-JINZOU6-COMMENT
                                        OR  CONS-JINZOU7-COMMENT
                                        OR  CONS-JINZOU8-COMMENT
                                        OR  CONS-JINZOU9-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-JINZOU-COMMENT
                END-IF
      *         ∂…ΩÍ±¢∞µ ƒ∫øΩË√÷
                IF   RECE82-SRYYM      >=    "201204"
                  IF   SRY-SRYCD(IDX2)  =     CONS-KYOKUIN5-COMMENT
                                          OR  CONS-KYOKUIN6-COMMENT
                                          OR  CONS-KYOKUIN7-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-KYOKUIN-COMMENT
                  END-IF
                ELSE
                  IF   SRY-SRYCD(IDX2)  =     CONS-KYOKUIN1-COMMENT
                                          OR  CONS-KYOKUIN2-COMMENT
                                          OR  CONS-KYOKUIN3-COMMENT
                                          OR  CONS-KYOKUIN4-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-KYOKUIN-COMMENT
                  END-IF
                END-IF
              END-IF
           END-IF
      *
           IF    ((RECE82-HKNKBN    NOT  =  "4"         ) AND
                  (RECE84-SRYKBN         =  "80"        ))
           OR    ((RECE82-HKNKBN         =  "4"         ) AND
                  (RECE84-SRYKBN         =  "40" OR "80"))
      *       •Í•œ•”•Í
              IF   SRY-SRYCD(IDX2)    =     "180706910"   OR
                                            "180022210"   OR
                                            "180706810"   OR
                                            "180022110"   OR
                                            "180707110"   OR
                                            "180022410"   OR
                                            "180707010"   OR
                                            "180022310"   OR
                                            "180707310"   OR
                                            "180022610"   OR
                                            "180707210"   OR
                                            "180022510"   OR
                                            "180707510"   OR
                                            "180022810"   OR
                                            "180707410"   OR
                                            "180022710"   OR
                                            "180708210"   OR
                                            "180023710"   OR
                                            "180708410"   OR
                                            "180023910"   OR
                                            "180708110"   OR
                                            "180023610"   OR
                                            "180708310"   OR
                                            "180023810"   OR
                                            "180024910"   OR
                                            "180024810"   OR
                                            "180025110"   OR
                                            "180025010"   OR
                                            "180025510"   OR
                                            "180025410"
      ****              MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SINDAI1-COMMENT  OR
                                            CONS-SINDAI2-COMMENT  OR
                                            CONS-SINDAI3-COMMENT  OR
                                            CONS-SINDAI4-COMMENT  OR
                                            CONS-KOKYUKI1-COMMENT OR
                                            CONS-KOKYUKI2-COMMENT OR
                                            CONS-KOKYUKI3-COMMENT OR
                                            CONS-KOKYUKI4-COMMENT OR
                                            CONS-NOUKE1-COMMENT   OR
                                            CONS-NOUKE2-COMMENT   OR
                                            CONS-NOUKE3-COMMENT   OR
                                            CONS-NOUKE4-COMMENT   OR
                                            CONS-NOUKE7-COMMENT   OR
                                            CONS-NOUKE11-COMMENT  OR
                                            CONS-NOUKE12-COMMENT  OR
                                            CONS-NOUKE13-COMMENT  OR
                                            CONS-NOUKE14-COMMENT  OR
                                            CONS-NOUKE15-COMMENT  OR
                                            CONS-NOUKE16-COMMENT  OR
                                            CONS-NOUKE17-COMMENT  OR
                                            CONS-NOUKE18-COMMENT  OR
                                            CONS-NOUKE19-COMMENT  OR
                                            CONS-UNDOKI1-COMMENT  OR
                                            CONS-UNDOKI2-COMMENT  OR
                                            CONS-UNDOKI3-COMMENT  OR
                                            CONS-UNDOKI4-COMMENT  OR
                                            CONS-UNDOKI9-COMMENT  OR
                                            CONS-UNDOKI11-COMMENT OR
                                            CONS-UNDOKI12-COMMENT OR
                                            CONS-UNDOKI13-COMMENT OR
                                            CONS-SYOGAI1-COMMENT  OR
                                            CONS-SYOGAI2-COMMENT  OR
                                            CONS-SYOGAI3-COMMENT  OR
                                            CONS-GANKNJ-COMMENT
      ****              MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
              END-IF
              IF   RECE82-SRYYM      >=    "201307"
                IF   SRY-SRYCD(IDX2)  =     CONS-SINDAI9-COMMENT   OR
                                            CONS-SINDAI10-COMMENT  OR
                                            CONS-KOKYUKI9-COMMENT  OR
                                            CONS-KOKYUKI10-COMMENT OR
                                            CONS-NOUKE20-COMMENT   OR
                                            CONS-NOUKE21-COMMENT   OR
                                            CONS-NOUKE22-COMMENT   OR
                                            CONS-UNDOKI14-COMMENT  OR
                                            CONS-UNDOKI15-COMMENT  OR
                                            CONS-UNDOKI16-COMMENT
      ****              MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
                END-IF
              ELSE
                IF   SRY-SRYCD(IDX2)  =     CONS-SINDAI7-COMMENT   OR
                                            CONS-SINDAI8-COMMENT   OR
                                            CONS-KOKYUKI7-COMMENT  OR
                                            CONS-KOKYUKI8-COMMENT  OR
                                            CONS-NOUKE8-COMMENT    OR
                                            CONS-NOUKE9-COMMENT    OR
                                            CONS-NOUKE10-COMMENT   OR
                                            CONS-UNDOKI7-COMMENT   OR
                                            CONS-UNDOKI8-COMMENT   OR
                                            CONS-UNDOKI10-COMMENT
      ****              MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
                END-IF
              END-IF
              EVALUATE      SRY-SRYCD(IDX2)
                WHEN     CONS-SINDAI5-COMMENT
                        MOVE   "2"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-SINDAI6-COMMENT
                        MOVE   "3"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-KOKYUKI5-COMMENT
                        MOVE   "4"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-KOKYUKI6-COMMENT
                        MOVE   "5"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-NOUKE5-COMMENT
                        MOVE   "6"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-NOUKE6-COMMENT
                        MOVE   "7"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-UNDOKI5-COMMENT
                        MOVE   "8"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-UNDOKI6-COMMENT
                        MOVE   "9"    TO    RECE84-RIHABIRI-COMMENT
              END-EVALUATE
      *       •Í•œ•”•Í° »Øæ…∆¸µ≠∫‹°À
              IF   RECE82-SRYYM      >=    "200604"
                IF   SRY-SRYCD(IDX2)  =     CONS-SINDAI1-COMMENT
                                      OR    CONS-SINDAI2-COMMENT
                                      OR    CONS-SINDAI3-COMMENT
                                      OR    CONS-SINDAI4-COMMENT
                                      OR    CONS-SINDAI5-COMMENT
                                      OR    CONS-SINDAI6-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SINDAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-KOKYUKI1-COMMENT
                                      OR    CONS-KOKYUKI2-COMMENT
                                      OR    CONS-KOKYUKI3-COMMENT
                                      OR    CONS-KOKYUKI4-COMMENT
                                      OR    CONS-KOKYUKI5-COMMENT
                                      OR    CONS-KOKYUKI6-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-KOKYUKI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-NOUKE1-COMMENT
                                      OR    CONS-NOUKE2-COMMENT
                                      OR    CONS-NOUKE3-COMMENT
                                      OR    CONS-NOUKE4-COMMENT
                                      OR    CONS-NOUKE5-COMMENT
                                      OR    CONS-NOUKE6-COMMENT
                                      OR    CONS-NOUKE7-COMMENT
                                      OR    CONS-NOUKE11-COMMENT
                                      OR    CONS-NOUKE12-COMMENT
                                      OR    CONS-NOUKE13-COMMENT
                                      OR    CONS-NOUKE14-COMMENT
                                      OR    CONS-NOUKE15-COMMENT
                                      OR    CONS-NOUKE16-COMMENT
                                      OR    CONS-NOUKE17-COMMENT
                                      OR    CONS-NOUKE18-COMMENT
                                      OR    CONS-NOUKE19-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-NOUKE-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-UNDOKI1-COMMENT
                                      OR    CONS-UNDOKI2-COMMENT
                                      OR    CONS-UNDOKI3-COMMENT
                                      OR    CONS-UNDOKI4-COMMENT
                                      OR    CONS-UNDOKI5-COMMENT
                                      OR    CONS-UNDOKI6-COMMENT
                                      OR    CONS-UNDOKI9-COMMENT
                                      OR    CONS-UNDOKI11-COMMENT
                                      OR    CONS-UNDOKI12-COMMENT
                                      OR    CONS-UNDOKI13-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-UNDOKI-COMMENT
                END-IF
                IF   RECE82-SRYYM      >=    "201307"
                  IF   SRY-SRYCD(IDX2)  =   CONS-SINDAI9-COMMENT
                                      OR    CONS-SINDAI10-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SINDAI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   CONS-KOKYUKI9-COMMENT
                                      OR    CONS-KOKYUKI10-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-KOKYUKI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   CONS-NOUKE20-COMMENT
                                      OR    CONS-NOUKE21-COMMENT
                                      OR    CONS-NOUKE22-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-NOUKE-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   CONS-UNDOKI14-COMMENT
                                      OR    CONS-UNDOKI15-COMMENT
                                      OR    CONS-UNDOKI16-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-UNDOKI-COMMENT
                  END-IF
                ELSE
                  IF   SRY-SRYCD(IDX2)  =   CONS-SINDAI7-COMMENT
                                      OR    CONS-SINDAI8-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SINDAI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   CONS-KOKYUKI7-COMMENT
                                      OR    CONS-KOKYUKI8-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-KOKYUKI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   CONS-NOUKE8-COMMENT
                                      OR    CONS-NOUKE9-COMMENT
                                      OR    CONS-NOUKE10-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-NOUKE-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   CONS-UNDOKI7-COMMENT
                                      OR    CONS-UNDOKI8-COMMENT
                                      OR    CONS-UNDOKI10-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-UNDOKI-COMMENT
                  END-IF
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-NANBYO-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-NANBYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-SYOGAI1-COMMENT
                                      OR    CONS-SYOGAI2-COMMENT
                                      OR    CONS-SYOGAI3-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SYOGAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-SESSYOKU-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SESSYOKU-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     CONS-YAKUKAN-COMMENT1 OR
                                            CONS-YAKUKAN-COMMENT2 OR
                                            CONS-YAKUKAN-COMMENT3
                   MOVE   "1"   TO    RECE84-FLG-YAKUKAN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-NYUEIYO-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-NYUEIYO-COMMENT
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     CONS-HOUMON-COMMENT1  OR
                                            CONS-HOUMON-COMMENT2)
              AND (FLG-HOUMON-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE84-FLG-HOUMON-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUMON-COMMENT3
                   MOVE  SPACE  TO    RECE84-FLG-HOUMON-COMMENT
                   MOVE    1    TO    FLG-HOUMON-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO-COMMENT
              AND  FLG-SYOBYO-COM     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO2-COMMENT
                                      OR    CONS-SYOBYO3-COMMENT
                   MOVE  SPACE  TO    RECE84-FLG-SYOBYO-COMMENT
                   MOVE    1    TO    FLG-SYOBYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO-COMMENT
              AND  FLG-RYOYO-COM      =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO2-COMMENT
                   MOVE  SPACE  TO    RECE84-FLG-RYOYO-COMMENT
                   MOVE    1    TO    FLG-RYOYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-TAIIN-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-TAIIN-COMMENT
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201004"
      *         ≤∏ÓªŸ±Áœ¢∑»ªÿ∆≥Œ¡
                IF   SRY-SRYCD(IDX2)  =     CONS-KAISIEN-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-KAISIEN-COMMENT
                END-IF
      *         Ω∏√ƒ±…Õ‹ø©ªˆªÿ∆≥Œ¡
                IF   SRY-SRYCD(IDX2)  =     CONS-SYUEIYO-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-SYUEIYO-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF     (RECE82-HKNKBN    NOT  =  "4"    AND
                   RECE84-SRYKBN         =  "14"       )
           OR     (RECE82-HKNKBN         =  "4"    AND
                   RECE84-SRYKBN         =  "80"       )
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI-COMMENT
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI2-COMMENT
                   MOVE  SPACE  TO    RECE84-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-HINKAI-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-HINKAI-COMMENT
              END-IF
      *
      ***     IF  (SRY-SRYCD(IDX2)    =     CONS-OSIN-COMMENT
      ***                             OR    CONS-OSIN2-COMMENT)
      ***     AND (FLG-OSIN-COM       =     ZERO              )
      ***          MOVE   "1"   TO    RECE84-FLG-OSIN-COMMENT
      ***     END-IF
      ***     IF   SRY-SRYCD(IDX2)    =     CONS-OSIN3-COMMENT
      ***          MOVE  SPACE  TO    RECE84-FLG-OSIN-COMMENT
      ***          MOVE    1    TO    FLG-OSIN-COM
      ***     END-IF
      *
              IF   RECE82-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  CONS-OSIN-COMMENT
                                    OR  CONS-OSIN2-COMMENT
                   IF    (WRK-SISETU3168  =  1)
                   OR    (WRK-SISETU3055  =  1)
                   OR    (WRK-SISETU3056  =  1)
                       MOVE   "1"   TO    RECE82-COMCNTFLG
                       MOVE   "1"   TO    RECE84-FLG-ZAISIEN-COMMENT
                   END-IF
                   IF  RECE84-FLG-ZAISIEN-COMMENT = SPACE
                     IF  (WRK-SISETU3169  =  1)
                     OR  (WRK-SISETU3057  =  1)
                     OR  (WRK-SISETU3058  =  1)
                       MOVE   "1"   TO    RECE82-COMCNTFLG
                       MOVE   "2"   TO    RECE84-FLG-ZAISIEN-COMMENT
                     END-IF
                   END-IF
                END-IF
              ELSE
                IF   WRK-SISETU613   =  1
                  IF   RECE82-SRYYM   >=  "200604"
                      IF SRY-SRYCD(IDX2) =  CONS-ZAISIEN1       OR
                                            CONS-ZAISIEN2       OR
                                            CONS-ZAISIEN3
                        MOVE   "1"   TO    RECE82-COMCNTFLG
                        MOVE   "1"   TO    RECE84-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                ELSE
                  IF   WRK-SISETU695   =  1
                    IF   RECE82-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  CONS-ZAISIEN1       OR
                                            CONS-ZAISIEN2       OR
                                            CONS-ZAISIEN3
                        MOVE   "1"   TO    RECE82-COMCNTFLG
                        MOVE   "2"   TO    RECE84-FLG-ZAISIEN-COMMENT
                      END-IF
                    END-IF
                  END-IF
                END-IF
              END-IF
      *
              IF   RECE82-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  CONS-KAISYOKU-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-KAISYOKU-COMMENT
                END-IF
      ***       IF   SRY-SRYCD(IDX2) =  CONS-YOUATU-COMMENT
      ***          MOVE   "1"   TO    RECE84-FLG-YOUATU-COMMENT
      ***       END-IF
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "50"
              IF   RECE84-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SHUJUTU-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF   RECE82-SRYYM      >=    "201004"
            IF     RECE84-SRYKBN      =     "54"
              IF   RECE84-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-MASUI-COMMENT
              END-IF
            END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "70"
              IF  (SRY-SRYCD(IDX2)    =     CONS-CTMRI-COMMENT1  OR
                                            CONS-CTMRI-COMMENT2  OR
                                            CONS-CTMRI-COMMENT3  OR
                                            CONS-CTMRI-COMMENT4  OR
                                            CONS-CTMRI-COMMENT5  OR
                                            CONS-CTMRI-COMMENT6  OR
                                            CONS-CTMRI-COMMENT7  OR
                                            CONS-CTMRI-COMMENT8  OR
                                            CONS-CTMRI-COMMENT9  OR
                                            CONS-CTMRI-COMMENT10 OR
                                            CONS-CTMRI-COMMENT11 OR
                                            CONS-CTMRI-COMMENT12 OR
                                            CONS-CTMRI-COMMENT13 OR
                                            CONS-CTMRI-COMMENT14 OR
                                            CONS-CTMRI-COMMENT15 OR
                                            CONS-CTMRI-COMMENT16 OR
                                            CONS-CTMRI-COMMENT17)
              AND (RECE82-SRYYM       <     "201004"            )
                   MOVE   "1"   TO    RECE84-FLG-CTMRI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     CONS-CTMRI-COMMENT1  OR
                                            CONS-CTMRI-COMMENT2  OR
                                            CONS-CTMRI-COMMENT4  OR
                                            CONS-CTMRI-COMMENT6  OR
                                            CONS-CTMRI-COMMENT7  OR
                                            CONS-CTMRI-COMMENT18 OR
                                            CONS-CTMRI-COMMENT19 OR
                                            CONS-CTMRI-COMMENT20)
              AND (RECE82-SRYYM       >=    "201004"            )
                   MOVE   "1"   TO    RECE84-FLG-CTMRI-COMMENT
              END-IF
      *
              IF    SRY-SRYCD(IDX2)  =
                    "700010000" OR "700030000" OR "700050000" OR
                    "700080000" OR "700110000" OR "700140000" OR
                    "700290000" OR "700310000" OR "700330000" OR
                    "700710000" OR "700730000" OR "700750000" OR
                    "700780000" OR "700810000" OR "700840000" OR
                    "700870000" OR "700160000" OR "700180000" OR
                    "700190000" OR "700200000" OR "700210000" OR
                    "700220000" OR "700230000" OR "700240000" OR
                    "700250000" OR "700260000" OR "700270000" OR
                    "700280000" OR "700590000" OR "700600000" OR
                    "700630000" OR "700640000" OR "700650000" OR
                    "700680000" OR "729780000"
                IF    SRY-SRYKAISU(IDX2)  >   1
                   MOVE   "1"   TO    RECE84-FLG-FILM-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF    ((RECE82-HKNKBN    NOT  =  "4"         ) AND
                  (RECE84-SRYKBN         =  "80"        ))
           OR    ((RECE82-HKNKBN         =  "4"         ) AND
                  (RECE84-SRYKBN         =  "40" OR "80"))
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSEI-COMMENT
              AND  FLG-HOUSEI-COM     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-HOUSEI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSEI-COMMENT2
                   MOVE  SPACE  TO    RECE84-FLG-HOUSEI-COMMENT
                   MOVE    1    TO    FLG-HOUSEI-COM
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201204"
                IF   SRY-SRYCD(IDX2)  =     CONS-SEIKEA1-COMMENT
                                      OR    CONS-SEIKEA2-COMMENT
                                      OR    CONS-SEIKEA3-COMMENT
                                      OR    CONS-SEIKEA4-COMMENT
                                      OR    CONS-SEIKEA5-COMMENT
                                      OR    CONS-SEIKEA6-COMMENT
                                      OR    CONS-SEIKEA7-COMMENT
                                      OR    CONS-SEIKEA8-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-SEIKEA-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "90"  OR  "92"
              IF   SRY-SRYCD(IDX2)    =     CONS-KAKURI-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-KAKURI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-CHIRYO-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-CHIRYO-COMMENT
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201004"
      *         ±…Õ‹•µ•›°º•»•¡°º•‡≤√ªª
                IF   SRY-SRYCD(IDX2)  =     CONS-EISAPO-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-EISAPO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-EISAPO-COMMENT2
                        MOVE   "2"    TO    RECE84-FLG-EISAPO-COMMENT
                END-IF
      *         ∏∆µ€•±•¢•¡°º•‡≤√ªª
                IF   SRY-SRYCD(IDX2)  =     CONS-KOKKEA-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-KOKKEA-COMMENT
                END-IF
      *         Ω≈æ…º‘≤√ªª° ¿∫ø¿Œ≈Õ‹…¬≈Ô∆˛±°Œ¡°À
                IF   SRY-SRYCD(IDX2)  =     CONS-JYUSYO-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-JYUSYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-JYUSYO-COMMENT2
                        MOVE   "2"    TO    RECE84-FLG-JYUSYO-COMMENT
                END-IF
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201204"
      *         ¿∫ø¿≤ •Í•®•æ•Û•¡°º•‡≤√ªª
                IF   SRY-SRYCD(IDX2)  =     CONS-SEIRIE-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-SEIRIE-COMMENT
                END-IF
      *         …¬≈ÔÃÙ∫ﬁ∂»Ã≥º¬ª‹≤√ªª
                IF   SRY-SRYCD(IDX2)  =     CONS-BYOYAK-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-BYOYAK-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "60" OR "64"
              IF   (SRY-SRYCD(IDX2)   =     CONS-KENSA1-COMMENT
                                      OR    CONS-KENSA2-COMMENT
                                      OR    CONS-KENSA3-COMMENT)
              AND  (FLG-KENSA-COM     =     ZERO               )
                   MOVE   "1"   TO    RECE84-FLG-KENSA-COMMENT
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN   =  "14" OR "40" OR "50" OR "54"
                                   OR "60" OR "70" OR "80" OR "64"
              SET     TBL-SANSO    TO      1
              SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
                                                        (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
              END-SEARCH
              IF      FLG-SANSO-OK        =    1
                   MOVE   "1"   TO    RECE84-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           .
       220211-RECE85-TENSOU-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    º´«Â¿’∆√ƒÍ¥Ô∫‡≈¿øÙ ‘Ω∏ΩËÕ˝
      *****************************************************************
       220212-JIBAI-KIZAI-TENSU-HENSYU-SEC     SECTION.
      *
           MOVE     ZERO                   TO    WRK-TEN-KZI
           MOVE     ZERO                   TO    FLG-TAIGAI
      *
           IF   RECE82-TAXKBN-KZI   =   "2"
      *
      *       •∑•π•∆•‡¥…Õ˝•ﬁ•π•ø° £¥£∞£∞£≥°À§À§∆æ√»Ò¿«¬–æ›≥∞•≥°º•…§´»Ω Ã
              INITIALIZE                         SYSKANRI-REC
              MOVE    "4003"               TO    SYS-KANRICD
              MOVE    "*"                  TO    SYS-KBNCD
              MOVE    RECE82-SRYYM         TO    SYS-STYUKYMD (1:6)
              MOVE    "01"                 TO    SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD         TO    SYS-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM          TO    SYS-HOSPNUM
              MOVE    SYSKANRI-REC         TO    MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                MOVE      MCPDATA-REC      TO    SYS-4003-REC
                PERFORM   VARYING   IDX10    FROM   1   BY   1
                           UNTIL   (IDX10                 >    50 )
                            OR     (SYS-4003-SRYCD(IDX10) =  SPACE)
                            OR     (FLG-TAIGAI            =     1 )
                  IF   SYS-4003-SRYCD(IDX10)  =  SRY-SRYCD(IDX2)
                      MOVE     1           TO    FLG-TAIGAI
                  END-IF
                END-PERFORM
              END-IF
              IF      TNS-KANANAMECHGKBN  =  ZERO OR 5
                  CONTINUE
              ELSE
                  MOVE     1       TO    FLG-TAIGAI
              END-IF
      *
      *       ∆√ƒÍ¥Ô∫‡≈˘æ√»Ò¿«∑◊ªª
              IF   FLG-TAIGAI  =  ZERO
                   IF    WRK-TAX1001-KBN  =  "1"
                    IF  SRY-SRYSYUKBN = "809"
                      COMPUTE  WRK-TEN-KZI  =
                              (SRY-JIHIMONEY(IDX2)
                                        *  1.05)  +  0.5
                    ELSE
                      COMPUTE  WRK-TEN-KZI  =
                              (TNS-TEN  *  1.05)  +  0.5
                    END-IF
                   ELSE
                    IF  SRY-SRYSYUKBN = "809"
                      COMPUTE  WRK-TEN-KZI  =
                              (SRY-JIHIMONEY(IDX2)
                                        *  1.05)
                    ELSE
                      COMPUTE  WRK-TEN-KZI  =
                              (TNS-TEN  *  1.05)
                    END-IF
                   END-IF
              ELSE
                   IF  SRY-SRYSYUKBN = "809"
                      MOVE  SRY-JIHIMONEY(IDX2)  TO   WRK-TEN-KZI
                   ELSE
                      MOVE  TNS-TEN              TO   WRK-TEN-KZI
                   END-IF
              END-IF
           ELSE
              IF  SRY-SRYSYUKBN = "809"
                      MOVE  SRY-JIHIMONEY(IDX2)  TO   WRK-TEN-KZI
              ELSE
                      MOVE  TNS-TEN              TO   WRK-TEN-KZI
              END-IF
           END-IF
      *
           .
       220212-JIBAI-KIZAI-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ª¿¡«§Œ√±≤¡ ‘Ω∏ΩËÕ˝
      *****************************************************************
       220213-RECE85-TENSOU-TANKA-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-TANKA-X
      * 
      *    øÙª˙…Ωº®Õ— ‘Ω∏° »æ≥—°À
           MOVE    TNS-TEN         TO  WRK-TANKA
           MOVE    WRK-TANKA-S1    TO  WRK-TANKA-Z1
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDY3   FROM    2   BY  -1
                     UNTIL     IDY3           =   ZERO
             IF  WRK-TANKA-S2(IDY3:1) NOT =   ZERO
                 MOVE    1               TO  FLG-SP
             END-IF
             IF  FLG-SP                  =   1
                 MOVE    WRK-TANKA-S2(IDY3:1)
                                         TO  WRK-TANKA-Z3(IDY3:1)
             END-IF
           END-PERFORM
           IF  WRK-TANKA-Z3          NOT =   SPACE
               MOVE    "."               TO  WRK-TANKA-Z2
           END-IF
      *        
           INITIALIZE                      WRK-CHAR-HENKAN-AREA
           MOVE    WRK-TANKA-X         TO  WRK-CHAR-X
           MOVE    12                  TO  WRK-CHAR-CNT
           MOVE    1                   TO  WRK-CHAR-IDX
           MOVE    ZERO                TO  WRK-CHAR-N-CNT
      *     
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL       IDW     >   WRK-CHAR-CNT
               IF      WRK-CHAR-X (IDW:1)  =   SPACE
                   CONTINUE
               ELSE        
                   IF    WRK-CHAR-X (IDW:1)  IS  NUMERIC
                       IF      WRK-CHAR-X (IDW:1)  =   "0"
                           MOVE    TBL-SUJI (10)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE
                           MOVE    WRK-CHAR-X (IDW:1)  TO  NUM-IDX
                           MOVE    TBL-SUJI (NUM-IDX)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF                    
                   ELSE
                       IF    WRK-CHAR-X (IDW:1)  =   "."
                           MOVE    "°•"   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE    
                           MOVE    "  "   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF
                   END-IF    
                   ADD     2           TO  WRK-CHAR-IDX
                                           WRK-CHAR-N-CNT
               END-IF    
           END-PERFORM
      *
           IF      WRK-CHAR-N-CNT      >   ZERO
                   MOVE    "±ﬂ"        TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                   ADD     2           TO  WRK-CHAR-N-CNT
           END-IF                             
      *
           .
       220213-RECE85-TENSOU-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    º´«Â¿’ ∏ΩÒŒ¡≈¿øÙ ‘Ω∏ΩËÕ˝
      *****************************************************************
       220214-JIBAI-BUNSYO-TENSU-HENSYU-SEC     SECTION.
      *
           MOVE        ZERO          TO    WRK-TEN-BUN
      *
           IF   RECE82-TAXKBN   =   "2"
              IF    WRK-TAX1001-KBN  =  "1"
               IF  SRY-SRYSYUKBN = "809"
                  COMPUTE  WRK-TEN-BUN  =
                          (SRY-JIHIMONEY(IDX2)
                                    *  1.05)  +  0.5
               ELSE
                  COMPUTE  WRK-TEN-BUN  =
                          (TNS-TEN  *  1.05)  +  0.5
               END-IF
              ELSE
               IF  SRY-SRYSYUKBN = "809"
                  COMPUTE  WRK-TEN-BUN  =
                          (SRY-JIHIMONEY(IDX2)
                                    *  1.05)
               ELSE
                  COMPUTE  WRK-TEN-BUN  =
                          (TNS-TEN  *  1.05)
               END-IF
              END-IF
           ELSE
               IF  SRY-SRYSYUKBN = "809"
                  MOVE  SRY-JIHIMONEY(IDX2)  TO   WRK-TEN-BUN
               ELSE
                  MOVE  TNS-TEN              TO   WRK-TEN-BUN
               END-IF
           END-IF
      *
           .
       220214-JIBAI-BUNSYO-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ªƒŒÃ«—¥˛§ŒøÙŒÃ ‘Ω∏ΩËÕ˝
      *****************************************************************
       220215-RECE85-TENSOU-SURYO-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    ZERO            TO  WRK-SURYOST
           MOVE    ZERO            TO  WRK-SURYOKETA
           MOVE    ZERO            TO  WRK-SURYOKETA1
           MOVE    ZERO            TO  WRK-SURYOKETA2
           MOVE    SPACE           TO  WRK-SURYO-N
           MOVE    SPACE           TO  WRK-SRYSURYOW
      *
      *    øÙª˙…Ωº®Õ— ‘Ω∏° »æ≥—°À
           MOVE    RECE85-SRYSURYO(IDX4)  TO  WRK-SURYO
           MOVE    WRK-SURYO-S1           TO  WRK-SURYO-Z1
      *
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDW   FROM    5   BY  -1
                      UNTIL    IDW   =   ZERO
             IF  WRK-SURYO-S2(IDW:1) NOT =   ZERO
                 MOVE    1         TO  FLG-SP
             END-IF
             IF  FLG-SP  =   1
                 MOVE    WRK-SURYO-S2(IDW:1)
                                   TO  WRK-SURYO-Z3(IDW:1)
             END-IF
           END-PERFORM
           IF  WRK-SURYO-Z3   NOT =  SPACE
               MOVE    "."         TO  WRK-SURYO-Z2
           END-IF
      *    …Ωº® ∏ª˙øÙ•´•¶•Û•»
           PERFORM   VARYING   IDW   FROM    1   BY  1
                      UNTIL    IDW     >    11
             IF  WRK-SURYO-X(IDW:1)  NOT =  SPACE
                 IF  WRK-SURYOST           =   ZERO
                     MOVE    IDW   TO  WRK-SURYOST
                 END-IF
      *          øÙŒÃ∑ÂøÙ•´•¶•Û•»(£∂∑ÂÃ‹°ß•‘•Í•™•…)
                 ADD     1         TO  WRK-SURYOKETA
                 IF  IDW    <   6
                     ADD     1     TO  WRK-SURYOKETA1
                 END-IF
                 IF  IDW    >   6
                     ADD     1     TO  WRK-SURYOKETA2
                 END-IF
             END-IF
           END-PERFORM
           IF  WRK-SURYOST  =   6
               MOVE   "0"          TO  WRK-SURYO-Z1(5:1)
               MOVE    5           TO  WRK-SURYOST
               MOVE    1           TO  WRK-SURYOKETA1
               ADD     1           TO  WRK-SURYOKETA
           END-IF
      *    ¡¥≥— —¥π
           IF  WRK-SURYOKETA1   >   ZERO
               INITIALIZE              WRK-CHAR-HENKAN-AREA
               MOVE    WRK-SURYO-Z1(WRK-SURYOST:WRK-SURYOKETA1)
                                       TO  WRK-CHAR-X
               MOVE    WRK-SURYOKETA1  TO  WRK-CHAR-CNT
      *
               MOVE    1               TO  WRK-CHAR-IDX
               PERFORM  VARYING  IDW   FROM    1   BY  1
                         UNTIL   IDW     >   WRK-CHAR-CNT
                   IF    WRK-CHAR-X(IDW:1)  IS  NUMERIC
                     IF    WRK-CHAR-X(IDW:1)  =   "0"
                        MOVE  TBL-SUJI(10)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     ELSE
                        MOVE  WRK-CHAR-X(IDW:1)  TO  NUM-IDX
                        MOVE  TBL-SUJI(NUM-IDX)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     END-IF
                   ELSE
                       MOVE    "  "    TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                   END-IF
                   ADD     2           TO  WRK-CHAR-IDX
               END-PERFORM
      *
               MOVE    WRK-CHAR-N      TO  WRK-SURYO-N1
           END-IF
           IF  WRK-SURYOKETA2   >   ZERO
               MOVE    "°•"            TO  WRK-SURYO-N2
      *
               INITIALIZE              WRK-CHAR-HENKAN-AREA
               MOVE    WRK-SURYO-Z3(1:WRK-SURYOKETA2)
                                       TO  WRK-CHAR-X
               MOVE    WRK-SURYOKETA2  TO  WRK-CHAR-CNT
      *
               MOVE    1               TO  WRK-CHAR-IDX
               PERFORM  VARYING  IDW   FROM    1   BY  1
                         UNTIL   IDW     >   WRK-CHAR-CNT
                   IF    WRK-CHAR-X(IDW:1)  IS  NUMERIC
                     IF    WRK-CHAR-X(IDW:1)  =   "0"
                        MOVE  TBL-SUJI(10)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     ELSE
                        MOVE  WRK-CHAR-X(IDW:1)  TO  NUM-IDX
                        MOVE  TBL-SUJI(NUM-IDX)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     END-IF
                   ELSE
                       MOVE    "  "    TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                   END-IF
                   ADD     2           TO  WRK-CHAR-IDX
               END-PERFORM
      *
               MOVE    WRK-CHAR-N      TO  WRK-SURYO-N3
           END-IF
      *
           STRING      WRK-SURYO-N1    DELIMITED   BY  " "
                       WRK-SURYO-N2    DELIMITED   BY  " "
                       WRK-SURYO-N3    DELIMITED   BY  " "
                       INTO            WRK-SRYSURYOW
           END-STRING
      *
           .
       220215-RECE85-TENSOU-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ÿ∑∏•≥•·•Û•» ‘Ω∏
      *****************************************************************
       220216-SETCOM-HENSYU-SEC     SECTION.
      *
           IF   RECE85-MEISKYFLG(IDX4)  =  "2"
              IF   (FLG-SETCOM              =  1   )
              AND  (RECE85-RECEDENFLG(IDX4) = SPACE)
                 MOVE  "1"            TO  RECE84-SETCOMFLG
              ELSE
                 MOVE  SPACE          TO  RECE85-MEISKYFLG(IDX4)
              END-IF
           ELSE
              IF   (RECE85-SRYCD(IDX4)(1:1) = "1" OR "6" OR "7")
              OR   (RECE85-SRYCD(IDX4)(1:3) = "059"            )
                IF  RECE85-SRYCD(IDX4)(1:2) = "63"
                 CONTINUE
                ELSE
                 MOVE   1             TO  FLG-SETCOM
                END-IF
              END-IF
           END-IF
      *
      *****
      *
      *    ∆√ƒÍÃÙ∫ﬁº£Œ≈¥…Õ˝Œ¡
           IF    RECE85-SRYCD(IDX4)   = "113000410" OR "113000510" OR
                                        "180000110" OR "180000210"
                 MOVE   1             TO  WRK-SETCOM-JIDO
           END-IF
      *    ∆√ƒÍÃÙ∫ﬁº£Œ≈¥…Õ˝≤√ªª° ¬°¥Ô∞‹ø¢∑Ó§´§È£≥∑Ó°À
           IF    RECE85-SRYCD(IDX4)   = "113000670"
                 MOVE   1             TO  WRK-SETCOM-JIDO2
           END-IF
      *    ∞≠¿≠º·Á∆√∞€ ™º¡º£Œ≈¥…Õ˝Œ¡
           IF    RECE85-SRYCD(IDX4)   = "113001210" OR "113001310" OR
                                        "113002110"
                 MOVE   3             TO  WRK-SETCOM-JIDO
           END-IF
      *
      *    ΩÈø«° ∆±∆¸ £øÙø«Œ≈≤ °À
           IF    RECE85-SRYCD(IDX4)   = CONS-SYODOU3-COMMENT
                                     OR CONS-SYODOU4-COMMENT
                 MOVE   4             TO  WRK-SETCOM-JIDO
           END-IF
           IF    RECE85-SRYCD(IDX4)   = CONS-SYODOU-COMMENT
                 MOVE   204           TO  WRK-SETCOM-JIDO
           END-IF
      *    Ω˝…¬ºÍ≈ˆ∂‚∞’∏´ΩÒ∏Ú…’Œ¡
           IF    RECE85-SRYCD(IDX4)   = CONS-SYOBYO-COMMENT
                 MOVE   6             TO  WRK-SETCOM-JIDO
           END-IF
      *    Œ≈Õ‹»Ò∆±∞’ΩÒ∏Ú…’Œ¡
           IF    RECE85-SRYCD(IDX4)   = CONS-RYOYO-COMMENT
                 MOVE   7             TO  WRK-SETCOM-JIDO
           END-IF
      *    À¨Ã‰¥«∏Óªÿº®Œ¡
           IF    RECE85-SRYCD(IDX4)   = CONS-HOUSIJI-COMMENT
                 MOVE   11            TO  WRK-SETCOM-JIDO
           END-IF
      *    ¬‡±°¡∞À¨Ã‰ªÿ∆≥Œ¡
           IF    RECE85-SRYCD(IDX4)   = CONS-HOUMON-COMMENT1
                 MOVE   103           TO  WRK-SETCOM-JIDO
           END-IF
      *    ¿∫ø¿≤ ¬‡±°¡∞À¨Ã‰ªÿ∆≥Œ¡
           IF    RECE85-SRYCD(IDX4)   = CONS-HOUSEI-COMMENT
                 MOVE   105           TO  WRK-SETCOM-JIDO
           END-IF
      *
           IF  ((WRK-SETCOM-JIDO    =  1                        ) AND
                (RECE85-SRYCD(IDX4) = "830000014" OR "840000038"))
           OR  ((WRK-SETCOM-JIDO2   =  1                        ) AND
                (RECE85-SRYCD(IDX4) = "840000077"               ))
           OR  ((WRK-SETCOM-JIDO    =  3                        ) AND
                (RECE85-SRYCD(IDX4) = "830000015"               ))
           OR  ((WRK-SETCOM-JIDO    =  4                        ) AND
                (RECE85-SRYCD(IDX4) = CONS-SYODOU2-COMMENT      ))
           OR  ((WRK-SETCOM-JIDO    =  6                        ) AND
                (RECE85-SRYCD(IDX4) = CONS-SYOBYO2-COMMENT
                                   OR CONS-SYOBYO3-COMMENT      ))
           OR  ((WRK-SETCOM-JIDO    =  7                        ) AND
                (RECE85-SRYCD(IDX4) = CONS-RYOYO2-COMMENT       ))
           OR  ((WRK-SETCOM-JIDO    =  11                       ) AND
                (RECE85-SRYCD(IDX4) = CONS-HOUSIJI2-COMMENT     ))
           OR  ((WRK-SETCOM-JIDO    =  103                      ) AND
                (RECE85-SRYCD(IDX4) = CONS-HOUMON-COMMENT3      ))
           OR  ((WRK-SETCOM-JIDO    =  105                      ) AND
                (RECE85-SRYCD(IDX4) = CONS-HOUSEI-COMMENT2      ))
      *
                 MOVE  "1"              TO  RECE84-SETCOMFLG
                 MOVE  "2"              TO  RECE85-MEISKYFLG(IDX4)
                 MOVE  WRK-SETCOM-JIDO  TO  RECE85-SETCOMATFLG(IDX4)
                 IF  (WRK-SETCOM-JIDO2   =  1         )
                 AND (RECE85-SRYCD(IDX4) = "840000077")
                   MOVE    2            TO  RECE85-SETCOMATFLG(IDX4)
                 END-IF
      *
           END-IF
      *
           .
       220216-SETCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿≈©•≥°º•……ΩÕ—»÷πÊ°›∂Ë ¨»÷πÊ ‘Ω∏
      *****************************************************************
       220217-TENTEKI-CDKBN-KBNNUM-HENSYU-SEC     SECTION.
      *
      *    …ΩÕ—°›∂Ë ¨»÷πÊ
      *    £¥:≈¿≈©
      *    £µ°ß√Êø¥¿≈ÃÆ
      *    £∂°ß√Êø¥¿≈ÃÆ±…Õ‹
      *    £∑∞ æÂ°°§Ω§Œ¬æ√ÌºÕ
           IF     RECE85-CDKBN-KBNNUM(IDX4)   NOT =    ZERO
              IF     RECE85-CDKBN-KBNNUM(IDX4)  <=    6
                EVALUATE    RECE85-CDKBN-KBNNUM(IDX4)
                  WHEN     5
                    MOVE   6     TO   RECE84-CDKBN-KBNNUM
                  WHEN     6
                    MOVE   8     TO   RECE84-CDKBN-KBNNUM
                  WHEN  OTHER
                    MOVE   RECE85-CDKBN-KBNNUM(IDX4)
                                 TO   RECE84-CDKBN-KBNNUM
                END-EVALUATE
              ELSE
                MOVE   RECE85-CDKBN-KBNNUM(IDX4)
                                 TO   RECE84-CDKBN-KBNNUM
                ADD    10        TO   RECE84-CDKBN-KBNNUM
              END-IF
      *       ≈¿≈©√ÌºÕ° ∑Ïﬁ˘¿Æ ¨¿Ω∫ﬁ ∏ΩÒ°À≤√ªª
              IF     RECE85-SRYCD(IDX4)    =   "130010570"
                    MOVE   5     TO   RECE84-CDKBN-KBNNUM
              END-IF
      *       √Êø¥¿≈ÃÆ√ÌºÕ° ∑Ïﬁ˘¿Æ ¨¿Ω∫ﬁ ∏ΩÒ°À≤√ªª
              IF     RECE85-SRYCD(IDX4)    =   "130010670"
                    MOVE   7     TO   RECE84-CDKBN-KBNNUM
              END-IF
           ELSE
      *       ÃÙ∫ﬁ
              IF     RECE84-YKZTEN     NOT =   ZERO
                    MOVE   9     TO   RECE84-CDKBN-KBNNUM
              END-IF
      *       ¥Ô∫‡
              IF     RECE84-KIZAITEN   NOT =   ZERO
                    MOVE   10    TO   RECE84-CDKBN-KBNNUM
              END-IF
      *       •≥•·•Û•»
              IF    (RECE85-SRYCD(IDX4)(1:1)    =   "8"  )
              OR    (RECE85-SRYCD(IDX4)(1:3)    =   "008")
                    MOVE   999   TO   RECE84-CDKBN-KBNNUM
              END-IF
           END-IF
      *
           .
       220217-TENTEKI-CDKBN-KBNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜£≤° £∆£∞£∞£≤°À
      *****************************************************************
       22022-RECE82-HENSYU-SEC               SECTION.
      *
           MOVE      ZERO       TO     WK-ZAIKAISU
           MOVE      ZERO       TO     WK-ZAITEN
           MOVE      ZERO       TO     WK-SYUTEN
           MOVE      ZERO       TO     WK-KIZAITEN
           MOVE      ZERO       TO     WK-YKZTEN
           MOVE      ZERO       TO     WK-YKZTEN2
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF      SRY-SRYKBN   =  "31" OR "32" OR "33"
                                MOVE   ACCT-YKZTEN    TO   WK-YKZTEN2
                       END-IF
                       IF   RECE82-HKNKBN    NOT  =  "4"
                         IF    SRY-SRYKBN   =  "31" OR "32" OR "33" OR
                                               "40" OR "50" OR "54" OR
                                               "60" OR "70" OR "80" OR
                                               "64"
                                MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                                COMPUTE   WK-YKZTEN   =
                                                      ACCT-KIZAITEN  +
                                                      ACCT-YKZTEN
                         END-IF
                       ELSE
                         IF    SRY-SRYKBN   =  "31" OR "32" OR "33" OR
                                               "40" OR "50" OR "54" OR
                                               "60" OR "70" OR "80" OR
                                               "64"
                                MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN  
                                COMPUTE   WK-YKZTEN =                 
                                                      ACCT-KIZAITEN  +  
                                                      ACCT-YKZTEN       
                                IF  SRY-SRYKBN  =  "70"
                                     COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                         -  ACCT-SYUTEN2
                                     COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                         +  ACCT-SYUTEN2
                                END-IF
                                IF  WRK-YOUBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-YOUBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-YOUBUKASAN
                                END-IF
                                IF  WRK-KYOBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-KYOBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-KYOBUKASAN
                                END-IF
                                IF  WRK-KEIBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-KEIBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-KEIBUKASAN
                                END-IF
                                IF  WRK-SOGAIKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-SOGAIKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-SOGAIKASAN
                                END-IF
                                IF  WRK-RYOYOTEATE  NOT =  ZERO
                                 IF  RECE82-JIBAIKBN    =  "2"
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-RYOYOTEATE
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-RYOYOTEATE
                                 END-IF
                                END-IF
                         END-IF
                       END-IF
                       IF    SRY-SRYKBN     =  "14"
                           MOVE    ACCT-SYUTEN1   TO   WK-SYUTEN
                           MOVE    ACCT-KIZAITEN  TO   WK-KIZAITEN
                           MOVE    ACCT-YKZTEN    TO   WK-YKZTEN
                           IF   RECE82-HKNKBN   =  "4"
                             IF  WRK-YOUBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-YOUBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-YOUBUKASAN
                             END-IF
                             IF  WRK-KYOBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-KYOBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-KYOBUKASAN
                             END-IF
                             IF  WRK-KEIBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-KEIBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-KEIBUKASAN
                             END-IF
                             IF  WRK-SOGAIKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-SOGAIKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-SOGAIKASAN
                             END-IF
                           END-IF
                       END-IF
                   ELSE
                       IF   SRY-SRYKBN   =  "21"  OR  "22"  OR  "23" OR
                                            "31"  OR  "32"  OR  "33" OR
                                            "40"  OR  "50"  OR  "54" OR
                                            "60"  OR  "70"  OR  "80" OR
                                            "13"  OR  "14"  OR  "64"
                           MOVE  ZERO          TO   WK-ZAIKAISU
                           MOVE  ZERO          TO   WK-ZAITEN
                           MOVE  ZERO          TO   WK-SYUTEN
                           MOVE  ZERO          TO   WK-KIZAITEN
                           MOVE  ZERO          TO   WK-YKZTEN
                           MOVE  ZERO          TO   WK-YKZTEN2
                           MOVE  ZERO          TO   FLG-OSINKBN
                       END-IF
                   END-IF
               ELSE
                     MOVE    ZERO     TO    WK-ZAIKAISU
                     MOVE    ZERO     TO    WK-ZAITEN
                     MOVE    ZERO     TO    WK-SYUTEN
                     MOVE    ZERO     TO    WK-KIZAITEN
                     MOVE    ZERO     TO    WK-YKZTEN
                     MOVE    ZERO     TO    WK-YKZTEN2
                     MOVE    ZERO     TO    FLG-OSINKBN
               END-IF
               IF  ((ACCT-ZAITEN    NOT =    ZERO) AND
                    (ACCT-ZAIKAISU  NOT =    ZERO))
               OR  ( FLG-TAISYO         =     1   )
                  EVALUATE   RECE82-HKNKBN
                    WHEN       "1"
                    WHEN       "2"
                        PERFORM   220221-RECE-LEFT-SET-SEC
                    WHEN       "4"
                        EVALUATE  WRK-PARA-RECEKBN
                          WHEN      "4"
      ********************* ° ø∑ÕÕº∞°À
                            PERFORM   220222-RECE-LEFT-SET2-SEC
                          WHEN      "5"
      ********************* ° ΩæÕËÕÕº∞°À
                            PERFORM   220223-RECE-LEFT-SET3-SEC
                        END-EVALUATE
                    WHEN       "5"
                        PERFORM   220224-RECE-LEFT-SET4-SEC
                  END-EVALUATE
               END-IF
               IF  ( SRY-SRYKBN   =   "21"   OR   "22"   OR   "23"   OR
                                      "31"   OR   "32"   OR   "33"   OR
                                      "40"   OR   "50"   OR   "54"   OR
                                      "60"   OR   "70"   OR   "14"   OR
                                      "64" )
               OR  ((ACCT-ZAIKBN  =    2  ) AND
                    (SRY-SRYKBN   =   "13"))
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
           .                                           
      *
       22022-RECE82-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜£≤° £∆£∞£∞£≤°À
      *****************************************************************
       220221-RECE-LEFT-SET-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ΩÈø«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "11"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)  =
                                RECE82-TENSU(1)  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1  OR
                                          CONS-ROUSAI21
                        COMPUTE    RECE82-KINGAKU2(1)  =
                                   RECE82-KINGAKU2(1)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
                   IF  (SRY-SRYCD(IDX3) = CONS-ROUSAI2)
                   AND (RECE82-SRYYM   <=   "201306"  )
                        COMPUTE    RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
      *    ªÿ∆≥§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "13"
                   IF ((SRY-SRYCD(IDX3) =
                        CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                        CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                        CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                        CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                        CONS-ROUSAI12                       ) AND
                       (RECE82-SRYYM   <=   "201306"        ))
                   OR ((SRY-SRYCD(IDX3) =   CONS-ROUSAI3    ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         COMPUTE    RECE82-KINGAKU2(2)  =
                                    RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE82G-TENSU(2)  =
                                    WRK-RECE82G-TENSU(2)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
      *    ∫ﬂ¬§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE82G-TENSU(3)  =
                              WRK-RECE82G-TENSU(3)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   COMPUTE    RECE82-TENSU(3)  =
                              RECE82-TENSU(3)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-IF
      *    ≈ÍÃÙ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU(4)  =
                                      RECE82-TENSU(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU(5)  =
                             RECE82-TENSU(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU(6)  =
                             RECE82-TENSU(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ƒ¥∫ﬁ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "24"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(7)
                   END-IF
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    À„∆«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(8)
                   END-IF
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ƒ¥¥§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "27"
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    »È≤º∂⁄∆˘∆‚√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "31"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(10)
                   COMPUTE   RECE82-TENSU(10)  =
                             RECE82-TENSU(10)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU(10)  =
                             WRK-RECE82C-TENSU(10)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    ¿≈ÃÆ∆‚√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "32"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(11)
                   COMPUTE   RECE82-TENSU(11)  =
                             RECE82-TENSU(11)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU(11)  =
                             WRK-RECE82C-TENSU(11)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    §Ω§Œ¬æ√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "33"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE   WRK-RECE82G-TENSU(12)  =
                             WRK-RECE82G-TENSU(12)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU(12)  =
                             WRK-RECE82C-TENSU(12)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
      *    ΩË√÷§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(13)
                   END-IF
                   COMPUTE   RECE82-TENSU(13)  =
                             RECE82-TENSU(13)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(14)  =
                             RECE82-TENSU(14)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ºÍΩ—§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(15)
                   END-IF
                   COMPUTE   RECE82-TENSU(15)  =
                             RECE82-TENSU(15)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(16)  =
                             RECE82-TENSU(16)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ∏°∫∫§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(17)
                   END-IF
                   COMPUTE   RECE82-TENSU(17)  =
                             RECE82-TENSU(17)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(17)  =
                                RECE82-KAISU(17)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   CONS-SAIKETSU1  OR
                                              CONS-SAIKETSU2  OR
                                              CONS-SAIKETSU3
                         IF   RECE82-KAISU(17)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(17)  =
                                        RECE82-KAISU(17)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ≤Ë¡¸ø«√«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(19)
                   END-IF
                   COMPUTE   RECE82-TENSU(19)  =
                             RECE82-TENSU(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    §Ω§Œ¬æ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "80"
                 IF ((SRY-SRYCD(IDX3) =
                      CONS-ROUSAI42 OR
                      CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                      CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                      CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                      CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                      CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                      CONS-ROUSAI40 OR  CONS-ROUSAI41     ) AND
                     (RECE82-SRYYM    >=  "201307"        ))
                           COMPUTE RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (TNS-TEN  *   ACCT-ZAIKAISU)
                 ELSE
                   IF ((SRY-SRYCD(IDX3) =
                        CONS-ROUSAI13  OR  CONS-ROUSAI14  OR
                        CONS-ROUSAI15  OR  CONS-ROUSAI16  OR
                        CONS-ROUSAI17  OR  CONS-ROUSAI18  OR
                        CONS-ROUSAI19  OR  CONS-ROUSAI20 )  AND
                       (RECE82-SRYYM   <=   "201306"     ))
                   OR ((SRY-SRYCD(IDX3) =
                        CONS-ROUSAI22  OR  CONS-ROUSAI23  OR
                        CONS-ROUSAI24  OR  CONS-ROUSAI25  OR
                        CONS-ROUSAI26  OR  CONS-ROUSAI27  OR
                        CONS-ROUSAI28  OR  CONS-ROUSAI29 )  AND
                       (RECE82-SRYYM   >=   "201307"     ))
                   OR ((ACCT-SRYKBN          =  "80"     )  AND
                       (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                         IF   SRY-SRYCD(IDX3)(1:5) =  "09594"
                           COMPUTE RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (WRK-JIBAIKZI-TNS(IDX3)
                                            *   ACCT-ZAIKAISU)
                           IF   FLG-TAISYO  =   1
                            COMPUTE  WRK-ZAITENNASHI  =
                                     WRK-ZAITENNASHI  +
                                    (WRK-JIBAIKZI-TNS(IDX3)
                                            *   ACCT-ZAIKAISU)
                           END-IF
                         ELSE
                           COMPUTE RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (TNS-TEN  *   ACCT-ZAIKAISU)
                         END-IF
                         EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   CONS-ROUSAI13
                              WHEN   CONS-ROUSAI14
                              WHEN   CONS-ROUSAI22
                              WHEN   CONS-ROUSAI23
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI15
                              WHEN   CONS-ROUSAI16
                              WHEN   CONS-ROUSAI24
                              WHEN   CONS-ROUSAI25
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI17
                              WHEN   CONS-ROUSAI18
                              WHEN   CONS-ROUSAI26
                              WHEN   CONS-ROUSAI27
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI19
                              WHEN   CONS-ROUSAI20
                              WHEN   CONS-ROUSAI28
                              WHEN   CONS-ROUSAI29
                                  MOVE    4     TO   WRK-HEYA-NINZU
                         END-EVALUATE
                         EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                         END-EVALUATE
                         IF    RECE82-HEYA-NINZU   =   ZERO
                         AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                         ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                         END-IF
                   ELSE
                     IF ((ACCT-SRYKBN          =  "80"   )  AND
                         (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                     OR ((SRY-SRYCD(IDX3) >= "101910110" AND
                                          <= "101910400" )  AND
                         (RECE82-SRYYM   <=  "201306"    ))
                     OR ((SRY-SRYCD(IDX3) >= "101800570" AND
                                          <= "101800860" )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                     OR ((SRY-SRYCD(IDX3)  = "101800550" OR
                              "101800560" OR "101800990" )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                       IF SRY-SRYCD(IDX3)(1:5) =  "09593"
                         COMPUTE   RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (WRK-JIBAIKZI-TNS(IDX3)
                                               *   ACCT-ZAIKAISU)
                         IF   FLG-TAISYO  =   1
                          COMPUTE  WRK-ZAITENNASHI  =
                                   WRK-ZAITENNASHI  +
                                  (WRK-JIBAIKZI-TNS(IDX3)
                                               *   ACCT-ZAIKAISU)
                         END-IF
                       ELSE
                        IF  SRY-SRYCD(IDX3)  = "101800550" OR
                                "101800560" OR "101800990"
                         COMPUTE   WRK-FUTANWARI  = TNS-TEN / 100
                         COMPUTE   WRK-KEISAN-KG2 =
                                  (WRK-TEN-KG2 * WRK-FUTANWARI)
                         COMPUTE   RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (WRK-KEISAN-KG2 * ACCT-ZAIKAISU)
                        ELSE
                         COMPUTE   RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         COMPUTE   WRK-TEN-KG2 = TNS-TEN
                        END-IF
                       END-IF
                     ELSE                
                       COMPUTE     RECE82-TENSU(21) =
                                   RECE82-TENSU(21) +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(2)  =
                                   RECE82-TENSU(2)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         COMPUTE   RECE82-TENSU(21) =
                                   RECE82-TENSU(21) -
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                       END-IF
                       COMPUTE       RECE82-TENSU(22)  =
                                     RECE82-TENSU(22)  +
                                    (WK-YKZTEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
                 END-IF
           END-EVALUATE
           .
      *
       220221-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜£≤° £∆£∞£∞£≤°À
      *****************************************************************
       220222-RECE-LEFT-SET2-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ΩÈø«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "11"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)  =
                                RECE82-TENSU(1)  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2  OR
                                             CONS-NYUKSN3  OR
                                             CONS-NYUKSN4  OR
                                             CONS-NYUKSN5  OR
                                             CONS-NYUKSN6  OR
                                             CONS-NYUKSN7  OR
                                             CONS-NYUKSN8  OR
                                             CONS-NYUKSN9
                             MOVE  "1"     TO   RECE82-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1  OR
                                          CONS-ROUSAI21
                        COMPUTE    RECE82-KINGAKU2(1)  =
                                   RECE82-KINGAKU2(1)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
                   IF  (SRY-SRYCD(IDX3) = CONS-ROUSAI2)
                   AND (RECE82-SRYYM   <=   "201306"  )
                        COMPUTE    RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
      *    ªÿ∆≥§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "13"
                   IF ((SRY-SRYCD(IDX3) =
                        CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                        CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                        CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                        CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                        CONS-ROUSAI12                       ) AND
                       (RECE82-SRYYM   <=   "201306"        ))
                   OR ((SRY-SRYCD(IDX3) =   CONS-ROUSAI3    ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         COMPUTE    RECE82-KINGAKU3(3)  =
                                    RECE82-KINGAKU3(3)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE82G-TENSU(2)  =
                                    WRK-RECE82G-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
      *    ∫ﬂ¬§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE82G-TENSU(22) =
                              WRK-RECE82G-TENSU(22) +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   EVALUATE  WRK-PARA-JIBAIPRTKBN
                     WHEN      "0"
                       COMPUTE    RECE82-TENSU(22)  =
                                  RECE82-TENSU(22)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU-YAK(22)  =
                                  RECE82-TENSU-YAK(22)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                     WHEN      "1"
                       COMPUTE    RECE82-TENSU(22)  =
                                  RECE82-TENSU(22)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU-YAK(23)  =
                                  RECE82-TENSU-YAK(23)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                   END-EVALUATE
                 END-IF
      *    ≈ÍÃÙ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                           COMPUTE   RECE82-TENSU-YAK(4)  =
                                     RECE82-TENSU-YAK(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU-YAK(4)  =
                                     RECE82-TENSU-YAK(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU-YAK(4)  =
                                      RECE82-TENSU-YAK(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU-YAK(5)  =
                             RECE82-TENSU-YAK(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU-YAK(6)  =
                             RECE82-TENSU-YAK(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ƒ¥∫ﬁ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "24"
                   IF    RECE82-KHNTEN(7)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(7)
                   ELSE
                      IF   RECE82-KHNTEN(7) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(7)
                           MOVE   1       TO    FLG-KHNTEN7-JI
                      END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(7)
                   END-IF
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    À„∆«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "26"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                    IF    RECE82-KHNTEN(8)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(8)
                    ELSE
                      IF   RECE82-KHNTEN(8) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(8)
                           MOVE   1       TO    FLG-KHNTEN8-JI
                      END-IF
                    END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(8)
                   END-IF
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ƒ¥¥§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "27"
                   IF    RECE82-KHNTEN(9)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(9)
                   ELSE
                      IF   RECE82-KHNTEN(9) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(9)
                           MOVE   1       TO    FLG-KHNTEN9-JI
                      END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(9)
                   END-IF
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    »È≤º∂⁄∆˘∆‚√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU-YAK(13)  =
                             WRK-RECE82C-TENSU-YAK(13)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    ¿≈ÃÆ∆‚√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU-YAK(13)  =
                             WRK-RECE82C-TENSU-YAK(13)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    §Ω§Œ¬æ√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "33"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE   WRK-RECE82G-TENSU-YAK(13)  =
                             WRK-RECE82G-TENSU-YAK(13)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU-YAK(13)  =
                             WRK-RECE82C-TENSU-YAK(13)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
      *    ΩË√÷§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(14)
                   END-IF
                   COMPUTE   RECE82-TENSU(14)  =
                             RECE82-TENSU(14)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(15)  =
                             RECE82-TENSU-YAK(15)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ºÍΩ—§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(16)
                   END-IF
                   COMPUTE   RECE82-TENSU(16)  =
                             RECE82-TENSU(16)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(17)  =
                             RECE82-TENSU-YAK(17)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ∏°∫∫§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(18)
                   END-IF
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(18)  =
                                RECE82-KAISU(18)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   CONS-SAIKETSU1  OR
                                              CONS-SAIKETSU2  OR
                                              CONS-SAIKETSU3
                         IF   RECE82-KAISU(18)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(18)  =
                                        RECE82-KAISU(18)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU-YAK(19)  =
                             RECE82-TENSU-YAK(19)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ≤Ë¡¸ø«√«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(20)
                   END-IF
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(21)  =
                             RECE82-TENSU-YAK(21)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    §Ω§Œ¬æ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "80"
                 IF ((SRY-SRYCD(IDX3) =
                      CONS-ROUSAI42 OR
                      CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                      CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                      CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                      CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                      CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                      CONS-ROUSAI40 OR  CONS-ROUSAI41     ) AND
                     (RECE82-SRYYM    >=  "201307"        ))
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI42
                         COMPUTE RECE82-KINGAKU2(2)  =
                                 RECE82-KINGAKU2(2)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                   ELSE
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                   END-IF
                 ELSE
                  IF ((SRY-SRYCD(IDX3) =
                       CONS-ROUSAI13  OR  CONS-ROUSAI14  OR
                       CONS-ROUSAI15  OR  CONS-ROUSAI16  OR
                       CONS-ROUSAI17  OR  CONS-ROUSAI18  OR
                       CONS-ROUSAI19  OR  CONS-ROUSAI20 )  AND
                      (RECE82-SRYYM   <=   "201306"     ))
                  OR ((SRY-SRYCD(IDX3) =
                       CONS-ROUSAI22  OR  CONS-ROUSAI23  OR
                       CONS-ROUSAI24  OR  CONS-ROUSAI25  OR
                       CONS-ROUSAI26  OR  CONS-ROUSAI27  OR
                       CONS-ROUSAI28  OR  CONS-ROUSAI29 )  AND
                      (RECE82-SRYYM   >=   "201307"     ))
                  OR ((ACCT-SRYKBN          =  "80"     )  AND
                      (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                       IF   SRY-SRYCD(IDX3)(1:5) =  "09594"
                         COMPUTE RECE82-KINGAKU3(1)  =
                                 RECE82-KINGAKU3(1)  +
                                (WRK-JIBAIKZI-TNS(IDX3)
                                           *   ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE RECE82-KINGAKU3(1)  =
                                 RECE82-KINGAKU3(1)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                       END-IF
                       EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   CONS-ROUSAI13
                              WHEN   CONS-ROUSAI14
                              WHEN   CONS-ROUSAI22
                              WHEN   CONS-ROUSAI23
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI15
                              WHEN   CONS-ROUSAI16
                              WHEN   CONS-ROUSAI24
                              WHEN   CONS-ROUSAI25
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI17
                              WHEN   CONS-ROUSAI18
                              WHEN   CONS-ROUSAI26
                              WHEN   CONS-ROUSAI27
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI19
                              WHEN   CONS-ROUSAI20
                              WHEN   CONS-ROUSAI28
                              WHEN   CONS-ROUSAI29
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       IF    RECE82-HEYA-NINZU   =   ZERO
                       AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                       ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                       END-IF
                  ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                       CONTINUE
                   ELSE
                    IF ((ACCT-SRYKBN          =  "80"   )  AND
                        (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                    OR ((SRY-SRYCD(IDX3) >= "101910110" AND
                                         <= "101910400" )  AND
                        (RECE82-SRYYM   <=  "201306"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101800570" AND
                                         <= "101800860" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3)  = "101800550" OR
                             "101800560" OR "101800990" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                     IF  SRY-SRYCD(IDX3)(1:5) =  "09593"
                      EVALUATE  WRK-JIBAIKZI-SYUKEIKBN(IDX3)
                       WHEN       0
                       WHEN       5
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       WHEN       1
                         COMPUTE RECE82-KINGAKU2(1)  =
                                 RECE82-KINGAKU2(1)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       WHEN       4
                         COMPUTE RECE82-KINGAKU2(2)  =
                                 RECE82-KINGAKU2(2)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                      END-EVALUATE
                     ELSE
                      IF  SRY-SRYCD(IDX3)  = "101800550" OR
                              "101800560" OR "101800990"
                         COMPUTE WRK-FUTANWARI  = TNS-TEN / 100
                         COMPUTE WRK-KEISAN-KG2 =
                             (WRK-TEN-KG2 * WRK-FUTANWARI)
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (WRK-KEISAN-KG2 * ACCT-ZAIKAISU)
                      ELSE
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                         COMPUTE WRK-TEN-KG2 = TNS-TEN
                      END-IF
                     END-IF
                    ELSE                                                
                     IF  SRY-SRYCD(IDX3)      =    CONS-RYOYOTEATE
                       COMPUTE    RECE82-KINGAKU3(3)  =                 
                                  RECE82-KINGAKU3(3)  +                 
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                       WHEN      "0"
      **>                IF    WK-SYUTEN       NOT =   ZERO
      **>                   ADD  WK-ZAIKAISU  TO  RECE82-KAISU(14)
      **>                END-IF
                         COMPUTE   RECE82-TENSU(14)  =
                                   RECE82-TENSU(14)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       WHEN      "1"
                         COMPUTE   RECE82-TENSU(22)  =
                                   RECE82-TENSU(22)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                      IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(2)  =
                                   RECE82-TENSU(2)  +
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         EVALUATE  WRK-PARA-JIBAIPRTKBN
                          WHEN      "0"
                           COMPUTE RECE82-TENSU(14) =
                                   RECE82-TENSU(14) -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                          WHEN      "1"
                           COMPUTE RECE82-TENSU(22) =
                                   RECE82-TENSU(22) -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         END-EVALUATE
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                        WHEN      "0"
                          COMPUTE   RECE82-TENSU-YAK(15)  =
                                    RECE82-TENSU-YAK(15)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                        WHEN      "1"
                          COMPUTE   RECE82-TENSU-YAK(23)  =
                                    RECE82-TENSU-YAK(23)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                     END-IF
                    END-IF                                              
                   END-IF
                  END-IF
                 END-IF
           END-EVALUATE
           .
      *
       220222-RECE-LEFT-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜£≤° £∆£∞£∞£≤°À
      *****************************************************************
       220223-RECE-LEFT-SET3-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ΩÈø«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "11"
                 EVALUATE   RECE82-JIBAIKBN
                  WHEN       "1"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)    =
                                RECE82-TENSU(1)    +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     COMPUTE    TBL-SYUTEN-JI(1)   =
                                TBL-SYUTEN-JI(1)   +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2  OR
                                             CONS-NYUKSN3  OR
                                             CONS-NYUKSN4  OR
                                             CONS-NYUKSN5  OR
                                             CONS-NYUKSN6  OR
                                             CONS-NYUKSN7  OR
                                             CONS-NYUKSN8  OR
                                             CONS-NYUKSN9
                             MOVE  "1"     TO   RECE82-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1  OR
                                          CONS-ROUSAI21
     ****               COMPUTE    RECE82-SONOTAKIN-JYU  =
     ****                          RECE82-SONOTAKIN-JYU  +
     ****                          (TNS-TEN         *  ACCT-ZAIKAISU)
                        COMPUTE    WRK-KINGAKU-JI   =
                                   WRK-KINGAKU-JI   +
                                   (TNS-TEN         *  ACCT-ZAIKAISU)
                   END-IF
                   IF  (SRY-SRYCD(IDX3) = CONS-ROUSAI2)
                   AND (RECE82-SRYYM   <=   "201306"  )
     ****               COMPUTE    RECE82-SONOTAKIN-JYU  =
     ****                          RECE82-SONOTAKIN-JYU  +
     ****                          (TNS-TEN         *  ACCT-ZAIKAISU)
                        COMPUTE    WRK-KINGAKU-JI   =
                                   WRK-KINGAKU-JI   +
                                   (TNS-TEN         *  ACCT-ZAIKAISU)
                   END-IF  
                  WHEN       "2"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2  OR
                                             CONS-NYUKSN3  OR
                                             CONS-NYUKSN4  OR
                                             CONS-NYUKSN5  OR
                                             CONS-NYUKSN6  OR
                                             CONS-NYUKSN7  OR
                                             CONS-NYUKSN8  OR
                                             CONS-NYUKSN9
                             MOVE  "1"     TO   RECE82-SHOSHIN-NYU
                     END-IF
                   END-IF
                   COMPUTE    RECE82-TENSU(1)    =
                              RECE82-TENSU(1)    +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE    TBL-SYUTEN-JI(1)   =
                              TBL-SYUTEN-JI(1)   +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                 END-EVALUATE
      *    ªÿ∆≥§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "13"
                 EVALUATE   RECE82-JIBAIKBN
                  WHEN       "1"
                   IF ((SRY-SRYCD(IDX3) =
                        CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                        CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                        CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                        CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                        CONS-ROUSAI12                       ) AND
                       (RECE82-SRYYM   <=   "201306"        ))
                   OR ((SRY-SRYCD(IDX3) =   CONS-ROUSAI3    ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         COMPUTE    RECE82-SONOTAKIN-JYU  =
                                    RECE82-SONOTAKIN-JYU  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE82G-TENSU(2)  =
                                    WRK-RECE82G-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(2) =
                                    TBL-SYUTEN-JI(2) +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
                  WHEN       "2"
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE82G-TENSU(2)  =
                                    WRK-RECE82G-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(2) =
                                    TBL-SYUTEN-JI(2) +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     END-IF
                 END-EVALUATE
      *    ∫ﬂ¬§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   EVALUATE  WRK-PARA-JIBAIPRTKBN
                     WHEN      "0"
                       COMPUTE    WRK-RECE82G-TENSU(28) =
                                  WRK-RECE82G-TENSU(28) +
                                 (WK-ZAITEN     *   WK-ZAIKAISU)
                     WHEN      "1"
                       COMPUTE    WRK-RECE82G-TENSU(27) =
                                  WRK-RECE82G-TENSU(27) +
                                 (WK-ZAITEN     *   WK-ZAIKAISU)
                   END-EVALUATE
                 ELSE
                   EVALUATE  WRK-PARA-JIBAIPRTKBN
                     WHEN      "0"
                       COMPUTE    RECE82-TENSU(28)  =
                                  RECE82-TENSU(28)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-SYUTEN-JI(28) =
                                  TBL-SYUTEN-JI(28) +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU(28)  =
                                  RECE82-TENSU(28)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-YKZTEN-JI(28) =
                                  TBL-YKZTEN-JI(28) +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                     WHEN      "1"
                       COMPUTE    RECE82-TENSU(27)  =
                                  RECE82-TENSU(27)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-SYUTEN-JI(27) =
                                  TBL-SYUTEN-JI(27) +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU(28)  =
                                  RECE82-TENSU(28)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-YKZTEN-JI(28) =
                                  TBL-YKZTEN-JI(28) +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                   END-EVALUATE
                 END-IF
      *    ≈ÍÃÙ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                           COMPUTE   TBL-YKZTEN-JI(4) =
                                     TBL-YKZTEN-JI(4) -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                           COMPUTE   TBL-YKZTEN-JI(4) =
                                     TBL-YKZTEN-JI(4) +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU(4)  =
                                      RECE82-TENSU(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                            COMPUTE   TBL-YKZTEN-JI(4) =
                                      TBL-YKZTEN-JI(4) +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU(5)  =
                             RECE82-TENSU(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(5) =
                             TBL-YKZTEN-JI(5) +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU(6)  =
                             RECE82-TENSU(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(6) =
                             TBL-YKZTEN-JI(6) +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ƒ¥∫ﬁ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "24"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                     IF    RECE82-KHNTEN(7)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(7)
                     ELSE
                      IF   RECE82-KHNTEN(7) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(7)
                           MOVE   1       TO    FLG-KHNTEN7-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(7)
                   END-IF
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(7) =
                             TBL-SYUTEN-JI(7) +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    À„∆«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "26"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                     IF    RECE82-KHNTEN(8)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(8)
                     ELSE
                      IF   RECE82-KHNTEN(8) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(8)
                           MOVE   1       TO    FLG-KHNTEN8-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(8)
                   END-IF
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(8) =
                             TBL-SYUTEN-JI(8) +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ƒ¥¥§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "27"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                     IF    RECE82-KHNTEN(9)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(9)
                     ELSE
                      IF   RECE82-KHNTEN(9) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(9)
                           MOVE   1       TO    FLG-KHNTEN9-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(9)
                   END-IF
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(9) =
                             TBL-SYUTEN-JI(9) +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    »È≤º∂⁄∆˘∆‚√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(11)
                   COMPUTE   RECE82-TENSU(11)   =
                             RECE82-TENSU(11)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(11)  =
                             TBL-SYUTEN-JI(11)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(11)   =
                             RECE82-TENSU(11)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(11)  =
                             TBL-YKZTEN-JI(11)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU(11)  =
                             WRK-RECE82C-TENSU(11)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI-C(11)  =
                             TBL-YKZTEN-JI-C(11)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    ¿≈ÃÆ∆‚√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)   =
                             RECE82-TENSU(12)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(12)  =
                             TBL-SYUTEN-JI(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(12)   =
                             RECE82-TENSU(12)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(12)  =
                             TBL-YKZTEN-JI(12)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU(12)  =
                             WRK-RECE82C-TENSU(12)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI-C(12)  =
                             TBL-YKZTEN-JI-C(12)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    §Ω§Œ¬æ√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "33"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE   WRK-RECE82G-TENSU(13)   =
                             WRK-RECE82G-TENSU(13)   +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(13)
                   COMPUTE   RECE82-TENSU(13)   =
                             RECE82-TENSU(13)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(13)  =
                             TBL-SYUTEN-JI(13)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(13)   =
                             RECE82-TENSU(13)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(13)  =
                             TBL-YKZTEN-JI(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU(13)  =
                             WRK-RECE82C-TENSU(13)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI-C(13)  =
                             TBL-YKZTEN-JI-C(13)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
      *    ΩË√÷§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(15)
                   END-IF
                   COMPUTE   RECE82-TENSU(15)   =
                             RECE82-TENSU(15)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(15)  =
                             TBL-SYUTEN-JI(15)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(16)   =
                             RECE82-TENSU(16)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(16)  =
                             TBL-YKZTEN-JI(16)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ºÍΩ—§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(18)
                   END-IF
                   COMPUTE   RECE82-TENSU(18)   =
                             RECE82-TENSU(18)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(18)  =
                             TBL-SYUTEN-JI(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(19)   =
                             RECE82-TENSU(19)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(19)  =
                             TBL-YKZTEN-JI(19)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ∏°∫∫§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(21)
                   END-IF
                   COMPUTE   RECE82-TENSU(21)   =
                             RECE82-TENSU(21)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(21)  =
                             TBL-SYUTEN-JI(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(21)  =
                                RECE82-KAISU(21)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   CONS-SAIKETSU1  OR
                                              CONS-SAIKETSU2  OR
                                              CONS-SAIKETSU3
                         IF   RECE82-KAISU(21)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(21)  =
                                        RECE82-KAISU(21)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU(22)   =
                             RECE82-TENSU(22)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(22)  =
                             TBL-YKZTEN-JI(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ≤Ë¡¸ø«√«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(24)
                   END-IF
                   COMPUTE   RECE82-TENSU(24)   =
                             RECE82-TENSU(24)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(24)  =
                             TBL-SYUTEN-JI(24)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(25)   =
                             RECE82-TENSU(25)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(25)  =
                             TBL-YKZTEN-JI(25)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    §Ω§Œ¬æ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "80"
                 IF ((SRY-SRYCD(IDX3) =
                      CONS-ROUSAI42 OR
                      CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                      CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                      CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                      CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                      CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                      CONS-ROUSAI40 OR  CONS-ROUSAI41     ) AND
                     (RECE82-SRYYM    >=  "201307"        ))
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI42
                             COMPUTE  WRK-KINGAKU-JI   =
                                      WRK-KINGAKU-JI   +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                             COMPUTE  RECE82-SONOTAKIN-JYU  =
                                      RECE82-SONOTAKIN-JYU  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                 ELSE
                  IF ((SRY-SRYCD(IDX3) =
                       CONS-ROUSAI13  OR  CONS-ROUSAI14  OR
                       CONS-ROUSAI15  OR  CONS-ROUSAI16  OR
                       CONS-ROUSAI17  OR  CONS-ROUSAI18  OR
                       CONS-ROUSAI19  OR  CONS-ROUSAI20 )  AND
                      (RECE82-SRYYM   <=   "201306"     ))
                  OR ((SRY-SRYCD(IDX3) =
                       CONS-ROUSAI22  OR  CONS-ROUSAI23  OR
                       CONS-ROUSAI24  OR  CONS-ROUSAI25  OR
                       CONS-ROUSAI26  OR  CONS-ROUSAI27  OR
                       CONS-ROUSAI28  OR  CONS-ROUSAI29 )  AND
                      (RECE82-SRYYM   >=   "201307"     ))
                  OR ((ACCT-SRYKBN          =  "80"     )  AND
                      (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                       IF   SRY-SRYCD(IDX3)(1:5) =  "09594"
                         COMPUTE RECE82-SITURYO-JYU  =
                                 RECE82-SITURYO-JYU  +
                                (WRK-JIBAIKZI-TNS(IDX3)
                                           *   ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE RECE82-SITURYO-JYU  =
                                 RECE82-SITURYO-JYU  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                       END-IF
                       EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   CONS-ROUSAI13
                              WHEN   CONS-ROUSAI14
                              WHEN   CONS-ROUSAI22
                              WHEN   CONS-ROUSAI23
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI15
                              WHEN   CONS-ROUSAI16
                              WHEN   CONS-ROUSAI24
                              WHEN   CONS-ROUSAI25
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI17
                              WHEN   CONS-ROUSAI18
                              WHEN   CONS-ROUSAI26
                              WHEN   CONS-ROUSAI27
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI19
                              WHEN   CONS-ROUSAI20
                              WHEN   CONS-ROUSAI28
                              WHEN   CONS-ROUSAI29
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       IF    RECE82-HEYA-NINZU   =   ZERO
                       AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                       ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                       END-IF
                  ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                       CONTINUE
                   ELSE
                    IF ((ACCT-SRYKBN          =  "80"   )  AND
                        (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                    OR ((SRY-SRYCD(IDX3) >= "101910110" AND
                                         <= "101910400" )  AND
                        (RECE82-SRYYM   <=  "201306"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101800570" AND
                                         <= "101800860" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3)  = "101800550" OR
                             "101800560" OR "101800990" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                     IF  SRY-SRYCD(IDX3)(1:5) =  "09593"
                      EVALUATE   RECE82-JIBAIKBN
                       WHEN       "1"
                         EVALUATE  WRK-JIBAIKZI-SYUKEIKBN(IDX3)
                           WHEN       0
                           WHEN       5
                             COMPUTE  RECE82-SONOTAKIN-JYU =
                                      RECE82-SONOTAKIN-JYU +
                               (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                           WHEN       1
                           WHEN       4
                             COMPUTE  WRK-KINGAKU-JI =
                                      WRK-KINGAKU-JI +
                               (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                         END-EVALUATE
                       WHEN       "2"
                             COMPUTE  RECE82-SONOTAKIN-JYU =
                                      RECE82-SONOTAKIN-JYU +
                               (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                      END-EVALUATE
                     ELSE
                      IF  SRY-SRYCD(IDX3)  = "101800550" OR
                              "101800560" OR "101800990"
                             COMPUTE  WRK-FUTANWARI  = TNS-TEN / 100
                             COMPUTE  WRK-KEISAN-KG2 =
                               (WRK-TEN-KG2 * WRK-FUTANWARI)
                             COMPUTE  RECE82-SONOTAKIN-JYU  =
                                      RECE82-SONOTAKIN-JYU  +
                               (WRK-KEISAN-KG2 * ACCT-ZAIKAISU)
                      ELSE
                             COMPUTE  RECE82-SONOTAKIN-JYU  =
                                      RECE82-SONOTAKIN-JYU  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                             COMPUTE  WRK-TEN-KG2 = TNS-TEN
                      END-IF
                     END-IF
                    ELSE                                                
                     IF  RECE82-JIBAIKBN      =    "1"
                     AND SRY-SRYCD(IDX3)      =    CONS-RYOYOTEATE
                       COMPUTE    RECE82-SONOTAKIN-JYU  =               
                                  RECE82-SONOTAKIN-JYU  +               
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                       WHEN      "0"
      **>                IF    WK-SYUTEN       NOT =   ZERO
      **>                   ADD  WK-ZAIKAISU  TO  RECE82-KAISU(15)
      **>                END-IF
                         COMPUTE   RECE82-TENSU(15)   =
                                   RECE82-TENSU(15)   +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                         COMPUTE   TBL-SYUTEN-JI(15)  =
                                   TBL-SYUTEN-JI(15)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       WHEN      "1"
                         COMPUTE   RECE82-TENSU(27)   =
                                   RECE82-TENSU(27)   +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                         COMPUTE   TBL-SYUTEN-JI(27)  =
                                   TBL-SYUTEN-JI(27)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                      IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(2)   =
                                   RECE82-TENSU(2)   +
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         COMPUTE   TBL-SYUTEN-JI(2)  =
                                   TBL-SYUTEN-JI(2)  +
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         EVALUATE  WRK-PARA-JIBAIPRTKBN
                          WHEN      "0"
                            COMPUTE   RECE82-TENSU(15)  =
                                      RECE82-TENSU(15)  -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                            COMPUTE   TBL-SYUTEN-JI(15) =
                                      TBL-SYUTEN-JI(15) -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                          WHEN      "1"
                            COMPUTE   RECE82-TENSU(27)  =
                                      RECE82-TENSU(27)  -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                            COMPUTE   TBL-SYUTEN-JI(27) =
                                      TBL-SYUTEN-JI(27) -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         END-EVALUATE
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                        WHEN      "0"
                         IF  SRY-SRYCD(IDX3) = CONS-RYOYOTEATE
                          COMPUTE   RECE82-TENSU(28)  =
                                    RECE82-TENSU(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                          COMPUTE   TBL-YKZTEN-JI(28)  =
                                    TBL-YKZTEN-JI(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                         ELSE
                          COMPUTE   RECE82-TENSU(16)  =
                                    RECE82-TENSU(16)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                          COMPUTE   TBL-YKZTEN-JI(16)  =
                                    TBL-YKZTEN-JI(16)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                         END-IF
                        WHEN      "1"
                          COMPUTE   RECE82-TENSU(28)  =
                                    RECE82-TENSU(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                          COMPUTE   TBL-YKZTEN-JI(28)  =
                                    TBL-YKZTEN-JI(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                     END-IF
                    END-IF                                              
                   END-IF
                  END-IF
                 END-IF
           END-EVALUATE
           .
      *
       220223-RECE-LEFT-SET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜£≤° £∆£∞£∞£≤°À
      *****************************************************************
       220224-RECE-LEFT-SET4-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ΩÈø«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "11"
                   IF  ( SRY-SRYCD(IDX3) = CONS-ROUSAI1  OR
                                           CONS-ROUSAI21 )
                   OR  ((SRY-SRYCD(IDX3) = CONS-ROUSAI2 ) AND
                        (RECE82-SRYYM   <=   "201306"   ))
                      CONTINUE
                   ELSE
                      IF   TNS-DATAKBN  =   2
                        EVALUATE    TNS-TIMEKSNKBN
                            WHEN    1
                            WHEN    5
                            WHEN    6
                                MOVE  "1"     TO   RECE82-SHOSHINTIME
                            WHEN    3
                                MOVE  "2"     TO   RECE82-SHOSHINTIME
                            WHEN    4
                                MOVE  "3"     TO   RECE82-SHOSHINTIME
                        END-EVALUATE
                      END-IF
                      IF    RECE82-KOMUKBN  =   "2"
                        IF  TNS-CHUKSNTUBAN  NOT = "0"
                            CONTINUE
                        ELSE
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(1)
                        END-IF
                      ELSE
                        IF  TNS-DATAKBN  =   2
                          IF   SRY-SRYCD(IDX3) = "111012370"
                            CONTINUE
                          ELSE
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(1)
                          END-IF
                        END-IF
                      END-IF
                      COMPUTE   RECE82-TENSU(1)    =
                                RECE82-TENSU(1)    +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
      *    ªÿ∆≥§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "13"
                   IF ((SRY-SRYCD(IDX3) =
                        CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                        CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                        CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                        CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                        CONS-ROUSAI12                       ) AND
                       (RECE82-SRYYM   <=   "201306"        ))
                   OR ((SRY-SRYCD(IDX3) =   CONS-ROUSAI3    ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         CONTINUE
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE82G-TENSU(9)  =
                                    WRK-RECE82G-TENSU(9)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(9)   =
                                    RECE82-TENSU(9)   +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
      *    ∫ﬂ¬§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE82G-TENSU(14) =
                              WRK-RECE82G-TENSU(14) +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±˝ø«°¶∆√ Ã±˝ø«° ª˛¥÷≤√ªª°À
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(10)
                       COMPUTE   RECE82-TENSU(10)   =
                                 RECE82-TENSU(10)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ÃÎ¥÷
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(11)
                       COMPUTE   RECE82-TENSU(11)   =
                                 RECE82-TENSU(11)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            øºÃÎ°¶∂€µﬁ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(12)
                       COMPUTE   RECE82-TENSU(12)   =
                                 RECE82-TENSU(12)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ∫ﬂ¬¥µº‘À¨Ã‰ø«Œ≈
                   IF (( TNS-OSINKBN           =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")))
                   OR (  FLG-OSINKBN           =     9           )
      ***          OR (  SRY-SRYCD(IDX3) =    "114001110"        )
      ***          OR (  SRY-SRYCD(IDX3) =    "114012910"        )
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(13)
                       COMPUTE   RECE82-TENSU(13)   =
                                 RECE82-TENSU(13)   +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                   END-IF
      *            §Ω§Œ¬æ
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE82-TENSU(14)   =
                                 RECE82-TENSU(14)   +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            ÃÙ∫ﬁ
                   COMPUTE   RECE82-TENSU(15)   =
                             RECE82-TENSU(15)   +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ≈ÍÃÙ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                           COMPUTE   RECE82-TENSU(16)  =
                                     RECE82-TENSU(16)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(16)
                           COMPUTE   RECE82-TENSU(16)  =
                                     RECE82-TENSU(16)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(16)
                            COMPUTE   RECE82-TENSU(16)  =
                                      RECE82-TENSU(16)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(18)
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(20)
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ƒ¥∫ﬁ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "24"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(22)
                   END-IF
                   COMPUTE   RECE82-TENSU(22)  =
                             RECE82-TENSU(22)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    À„∆«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(23)
                   END-IF
                   COMPUTE   RECE82-TENSU(23)  =
                             RECE82-TENSU(23)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ƒ¥¥§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "27"
                   COMPUTE   RECE82-TENSU(24)  =
                             RECE82-TENSU(24)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    »È≤º∂⁄∆˘∆‚√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(25)
                   COMPUTE   RECE82-TENSU(25)  =
                             RECE82-TENSU(25)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU(25)  =
                             WRK-RECE82C-TENSU(25)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    ¿≈ÃÆ∆‚√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(26)
                   COMPUTE   RECE82-TENSU(26)  =
                             RECE82-TENSU(26)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU(26)  =
                             WRK-RECE82C-TENSU(26)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    §Ω§Œ¬æ√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "33"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE   WRK-RECE82G-TENSU(27)   =
                             WRK-RECE82G-TENSU(27)   +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(27)
                   COMPUTE   RECE82-TENSU(27)  =
                             RECE82-TENSU(27)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE82C-TENSU(27)  =
                             WRK-RECE82C-TENSU(27)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
      *    ΩË√÷§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(28)
                   END-IF
                   COMPUTE   RECE82-TENSU(28)   =
                             RECE82-TENSU(28)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(29)   =
                             RECE82-TENSU(29)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ºÍΩ—§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(30)
                   END-IF
                   COMPUTE   RECE82-TENSU(30)   =
                             RECE82-TENSU(30)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(31)   =
                             RECE82-TENSU(31)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ∏°∫∫§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(32)
                   END-IF
                   COMPUTE   RECE82-TENSU(32)   =
                             RECE82-TENSU(32)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                         COMPUTE   RECE82-KAISU(32)  =
                                   RECE82-KAISU(32)  -
                                   WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD  =   CONS-SAIKETSU1  OR
                                        CONS-SAIKETSU2  OR
                                        CONS-SAIKETSU3
                         IF   RECE82-KAISU(32)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(32)  =
                                        RECE82-KAISU(32)  -
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU(33)   =
                             RECE82-TENSU(33)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ≤Ë¡¸ø«√«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(34)
                   END-IF
                   COMPUTE   RECE82-TENSU(34)   =
                             RECE82-TENSU(34)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(35)   =
                             RECE82-TENSU(35)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    §Ω§Œ¬æ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "80"
                 IF ((SRY-SRYCD(IDX3) =
                      CONS-ROUSAI42 OR
                      CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                      CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                      CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                      CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                      CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                      CONS-ROUSAI40 OR  CONS-ROUSAI41     ) AND
                     (RECE82-SRYYM    >=  "201307"        ))
                     CONTINUE
                 ELSE
                   IF ((SRY-SRYCD(IDX3) =
                        CONS-ROUSAI13  OR  CONS-ROUSAI14  OR
                        CONS-ROUSAI15  OR  CONS-ROUSAI16  OR
                        CONS-ROUSAI17  OR  CONS-ROUSAI18  OR
                        CONS-ROUSAI19  OR  CONS-ROUSAI20 )  AND
                       (RECE82-SRYYM   <=   "201306"     ))
                   OR ((SRY-SRYCD(IDX3) =
                        CONS-ROUSAI22  OR  CONS-ROUSAI23  OR
                        CONS-ROUSAI24  OR  CONS-ROUSAI25  OR
                        CONS-ROUSAI26  OR  CONS-ROUSAI27  OR
                        CONS-ROUSAI28  OR  CONS-ROUSAI29 )  AND
                       (RECE82-SRYYM   >=   "201307"     ))
                   OR ((ACCT-SRYKBN          =  "80"     )  AND
                       (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                     IF   (FLG-TAISYO           =   1     )
                     AND  (SRY-SRYCD(IDX3)(1:5) =  "09594")
                         COMPUTE   WRK-ZAITENNASHI  =
                                   WRK-ZAITENNASHI  +
                                  (WRK-JIBAIKZI-TNS(IDX3)
                                               *   ACCT-ZAIKAISU)
                     END-IF
                   ELSE
                     IF ((ACCT-SRYKBN          =  "80"   )  AND
                         (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                     OR ((SRY-SRYCD(IDX3) >= "101910110" AND
                                          <= "101910400" )  AND
                         (RECE82-SRYYM   <=  "201306"    ))
                     OR ((SRY-SRYCD(IDX3) >= "101800570" AND
                                          <= "101800860" )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                     OR ((SRY-SRYCD(IDX3)  = "101800550" OR
                              "101800560" OR "101800990" )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                       IF   (FLG-TAISYO           =   1     )
                       AND  (SRY-SRYCD(IDX3)(1:5) =  "09593")
                          COMPUTE  WRK-ZAITENNASHI  =
                                   WRK-ZAITENNASHI  +
                                  (WRK-JIBAIKZI-TNS(IDX3)
                                               *   ACCT-ZAIKAISU)
                       END-IF
                     ELSE                
                       COMPUTE     RECE82-TENSU(37) =
                                   RECE82-TENSU(37) +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(9)  =
                                   RECE82-TENSU(9)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         COMPUTE   RECE82-TENSU(37) =
                                   RECE82-TENSU(37) -
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                       END-IF
                       COMPUTE       RECE82-TENSU(38)  =
                                     RECE82-TENSU(38)  +
                                    (WK-YKZTEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
                 END-IF
           END-EVALUATE
           .
      *
       220224-RECE-LEFT-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∫ﬁ§Œø«Œ≈∆‚Õ∆•¡•ß•√•Ø
      *****************************************************************
       2203-SINRYO-CHECK-SEC                 SECTION.
      *
      ***  PERFORM   VARYING   IDU   FROM   1   BY   1
           PERFORM   VARYING   IDU   FROM   2   BY   1
                      UNTIL  ( IDU   >   TBL-ZAICHECK-MAX )
                       OR    ( FLG-RINYAK  NOT =  ZERO    )
      *
              IF  ( IDU  =  1                                  )
              OR  ((IDU  =  2) AND (ACCT-SRYKBN = "60" OR "64"))
              OR  ((IDU  =  3) AND (ACCT-SRYKBN = "80"        ))
              OR  ((IDU  =  4) AND (ACCT-SRYKBN = "80"        ))
      *
                 INITIALIZE                  SRYACT-REC
                 MOVE RECE82-HOSPNUM     TO  SRY-HOSPNUM
                 MOVE RECE82-NYUGAIKBN   TO  SRY-NYUGAIKBN
                 MOVE RECE82-PTID        TO  SRY-PTID
                 MOVE RECE82-SRYYM       TO  SRY-SRYYM
                 MOVE ACCT-ZAINUM        TO  SRY-ZAINUM
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(1)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(2)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(3)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(4)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(5)
                 MOVE SRYACT-REC         TO  MCPDATA-REC
                 MOVE "tbl_sryact"       TO  MCP-TABLE
                 MOVE "key8"             TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC  =  ZERO
                     MOVE    "tbl_sryact"   TO  MCP-TABLE
                     MOVE    "key8"         TO  MCP-PATHNAME
                     PERFORM  900-SRYACT-READ-SEC
                     IF     FLG-SRYACT   =  ZERO
                         EVALUATE   IDU
                           WHEN      1
                             MOVE   1   TO  FLG-RINYAK
                           WHEN      2
                           WHEN      3
                           WHEN      4
                             MOVE   2   TO  FLG-RINYAK
                         END-EVALUATE
                     END-IF
                 END-IF
                 MOVE "tbl_sryact"   TO  MCP-TABLE
                 MOVE "key8"         TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
      *
              END-IF
      *
           END-PERFORM
      *
           .
       2203-SINRYO-CHECK-EXT.
           EXIT.
      *
      **************************************************************
      *    ø«Œ≈≤Ò∑◊…’≤√æ Û∏°∫˜
      **************************************************************
       2204-SRYACCTPLUS-KENSAKU-SEC           SECTION.
      *
           INITIALIZE                      SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM       TO   ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN     TO   ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID          TO   ACCTP-PTID
           MOVE    ACCT-SRYKA         TO   ACCTP-SRYKA
           MOVE    ACCT-SRYYM         TO   ACCTP-SRYYM
           MOVE    ACCT-ZAINUM        TO   ACCTP-ZAINUM
           MOVE    1                  TO   ACCTP-RENNUM
           MOVE    SRYACCTPLUS-REC    TO   MCPDATA-REC
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
               MOVE    "key"              TO   MCP-PATHNAME
               PERFORM  900-SRYACCTPLUS-READ-SEC
               IF        FLG-SRYACCTPLUS   =   ZERO
                 IF   ACCTP-RSIKINGAKU  NOT = ZERO
                    MOVE    1   TO   FLG-TAISYO
                 END-IF
               END-IF
           END-IF
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       2204-SRYACCTPLUS-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿≈©ºÍµª≤√ªª§ﬁ§»§·¬–æ›§´•¡•ß•√•Ø
      *****************************************************************
       2205-TENTEKIMATOME-CHECK-SEC                 SECTION.
      *
           PERFORM  VARYING  ID4    FROM   1   BY   1
                     UNTIL  (ID4                      >   20 )
                       OR   (WRK-TENTEKI2-ZAINUM(ID4) =  ZERO)
                       OR   (FLG-TENTEKIMATOME        =    1 )
      *
             IF    ACCT-ZAINUM  =  WRK-TENTEKI2-ZAINUM(ID4)
                  MOVE     1    TO   FLG-TENTEKIMATOME
             END-IF
      *
           END-PERFORM
      *
           .
       2205-TENTEKIMATOME-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£¥°À
      *****************************************************************
       230-RECE84-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE82-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE82-PTID           TO      NACCT-PTID
           MOVE    RECE82-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
grpsys     PERFORM  900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  
                   IF     NACCT-SRYCDTOTAL    =    099999916
                       CONTINUE
                   ELSE
                     PERFORM    VARYING   IDY   FROM   1   BY   1
                                      UNTIL    IDY    >    31
                       IF   NACCT-DAY(IDY)   NOT =  ZERO 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY1(IDY) 
                       END-IF
                     END-PERFORM
                   END-IF
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN    =    "2"  
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY2(IDY) 
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 IF   ( NACCT-ZAISKBKBN    =    "4"       )
                 AND  ( NACCT-SRYCDTOTAL   =    197000570 )
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY4(IDY) 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY4X(IDY)
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN    =    "5"  
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY5(IDY) 
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
                 MOVE    "key7"            TO      MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"         TO      MCP-TABLE
           MOVE    "key7"                  TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *
           MOVE    ZERO    TO    WRK-ZAINUM-CNT2
           MOVE    ZERO    TO    FLG-DOJITU
           INITIALIZE            WRK-SKJ-DOJITU-TABLE
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE82-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE82-PTID           TO      NACCT-PTID
           MOVE    RECE82-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1") 
      *
                 IF     NACCT-ZAISKBKBN    =     "4"  OR  "6"  OR "7"
      *
                   MOVE     ZERO    TO    FLG-JYOGAI
                   IF    NACCT-ZAISKBKBN    =  "6"
                     PERFORM  VARYING  ID4    FROM   1   BY   1
                               UNTIL  (ID4                  >   50  )
                                 OR   (WRK-CHUKSNSRYCD(ID4) =  SPACE)
                                 OR   (FLG-JYOGAI           =   1   )
                       IF  NACCT-SRYCDTOTAL(6:9) = WRK-CHUKSNSRYCD(ID4)
                          MOVE     1    TO   FLG-JYOGAI
                       END-IF
                     END-PERFORM
                   END-IF
      *
                   IF ((NACCT-ZAISKBKBN    =  "6"                  ) AND
                       (NACCT-SRYCDTOTAL   = 190117070 OR 190109670) AND
                       (RECE82-SRYYM      >=  "200604"             ))
                   OR ((NACCT-ZAISKBKBN    =  "6"                  ) AND
                       (NACCT-SRYCDTOTAL   = 190126670 OR 190126770
                                          OR 190126570 OR 190126470) AND
                       (RECE82-SRYYM      >=  "200804"             ))
                   OR ((NACCT-ZAISKBKBN    =  "6"                  ) AND
                       (NACCT-SRYCDTOTAL   = 190129870             ) AND
                       (RECE82-SRYYM      >=  "200804"             ) AND
                       (RECE82-SRYYM      <=  "201003"             ))
                   OR ( FLG-JYOGAI         = 1                      )
      *
                      CONTINUE
      *
                   ELSE
      *
                      IF   NACCT-ZAISKBKBN  = "6" OR "7"
                         ADD    1         TO    WRK-ZAINUM-CNT2
                      END-IF
      *
                      INITIALIZE          WRK-DAY-TABLE2
                                          WRK-SKJ-TABLE
                                          WRK-SKJ-TABLE2
                      MOVE     ZERO       TO    CNT-DAY21
                                                CNT-DAY24
                      MOVE     ZERO       TO    FLG-DAY21
                                                FLG-DAY24
      *
                      MOVE     ZERO       TO    WRK-TEN
                                                WRK-MEISAI-CNT
                                                WRK-SRYCDTOTAL
                                                WRK-SURYOUTOTAL
      *
                      PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                         IF  RECE82-HKNCOMBI     =  WRK-DAY5(IDY)
                            IF  ((WRK-DAY1(IDY)   NOT  =  ZERO  ) OR
                                 (RECE82-NYUINDAY-KBN(IDY) = "1"))
                            AND ( NACCT-ZAITEN    NOT  =  ZERO   )
                            AND ( NACCT-ZAISKBKBN NOT  =  "4"    )
                            AND ( NACCT-DAY(IDY)  NOT  =  ZERO   )
                               IF (WRK-DAY2(IDY)  =  ZERO)
                               OR (WRK-DAY2(IDY) >=   4  )
                                    MOVE   NACCT-DAY(IDY)  
                                           TO   WRK-DAY21(IDY) 
                                    ADD    1   TO   CNT-DAY21
                                    MOVE   1   TO   FLG-DAY21
                               END-IF
                            END-IF
      *                     *ø©ªˆ
                            IF  ((WRK-DAY1(IDY)   NOT  =  ZERO  ) OR
                                 (RECE82-NYUINDAY-KBN(IDY) = "1"))
                            AND ( NACCT-ZAITEN         =  ZERO   )
                            AND ( NACCT-ZAISKBKBN      =  "4"    )
                            AND ( NACCT-DAY(IDY)  NOT  =  ZERO   )
                            AND ( WRK-SHOKUJIRYOYO(IDY)
                                                  NOT  =  ZERO   )
                               IF (WRK-DAY2(IDY)  =  ZERO)
                               OR (WRK-DAY2(IDY) >=   4  )
                                IF   RECE82-SRYYM  >= "200604"
                                 IF  NACCT-SRYCDTOTAL = 099999918
                                                    OR  099999919
                                                    OR  099999920
                                  MOVE   NACCT-DAY(IDY)  
                                            TO   WRK-DAY24(IDY) 
                                  PERFORM      2301-NYUINACCT-SYU-SEC
                                 END-IF
                                ELSE
                                 IF  NACCT-SRYCDTOTAL = 099999913
                                  MOVE   NACCT-DAY(IDY)  
                                            TO   WRK-DAY24(IDY) 
                                  PERFORM      2301-NYUINACCT-SYU-SEC
                                  ADD    1     TO   CNT-DAY24
                                  MOVE   1     TO   FLG-DAY24
                                 END-IF
                                END-IF
                               END-IF
                            END-IF
                            IF     RECE82-SRYYM        >= "200610"
                              IF  ((WRK-DAY1(IDY)   NOT  =  ZERO  ) OR
                                   (RECE82-NYUINDAY-KBN(IDY) = "1"))
                              AND ( NACCT-ZAITEN         =  ZERO   )
                              AND ( NACCT-ZAISKBKBN      =  "4"    )
      ***                     AND ( NACCT-DAY(IDY)  NOT  =  ZERO   )
                              AND ( WRK-SHOKUJIRYOYO(IDY)
                                                    NOT  =  ZERO   )
                              AND ( NACCT-SRYCDTOTAL = 099999918
                                                   OR  099999919
                                                   OR  099999920   )
                              AND ((WRK-DAY2(IDY)  =  ZERO        ) OR
                                   (WRK-DAY2(IDY) >=   4          ))
                                PERFORM   2307-NYUINACCT-SYU-KAN-SEC
                              END-IF
                            END-IF
                         ELSE
      *                     *ø©ªˆ
                            IF  (RECE82-HKNCOMBI   =
                                   RECE82-DOUJITSU-HKNCOMBI(IDY))
                            AND (NACCT-ZAITEN         =  ZERO   )
                            AND (NACCT-ZAISKBKBN      =  "4"    )
                            AND (NACCT-DAY(IDY)  NOT  =  ZERO   )
                            AND (WRK-SHOKUJIRYOYO(IDY)
                                                 NOT  =  ZERO   )
                            AND ((WRK-DAY2(IDY)  =  ZERO       ) OR
                                 (WRK-DAY2(IDY) >=   4         ))
                            AND (RECE82-SRYYM >= "200604"       )
                             IF  NACCT-SRYCDTOTAL = 099999918
                                                 OR 099999919
                                                 OR 099999920
                              MOVE  1 TO  FLG-DOJITU
                              MOVE  1 TO  WRK-SKJ-DOJITU-KBN (IDY)
                              EVALUATE   NACCT-SRYCDTOTAL
                                WHEN  099999918
                                  MOVE  NACCT-DAY(IDY) TO
                                          WRK-SKJ-DOJITU-ASA (IDY)
                                WHEN  099999919
                                  MOVE  NACCT-DAY(IDY) TO
                                          WRK-SKJ-DOJITU-HIRU(IDY)
                                WHEN  099999920
                                  MOVE  NACCT-DAY(IDY) TO
                                          WRK-SKJ-DOJITU-YORU(IDY)
                              END-EVALUATE
                             END-IF
                            END-IF
                         END-IF
      *
                      END-PERFORM
      *
                      IF   FLG-DAY21    =   1
                         PERFORM   2302-RECE84-TENSOU2-SEC
                         PERFORM   2303-RECE85-HENSYU2-SEC
                         PERFORM   900-RECE84-WRITE-SEC
                      END-IF
      *
                      IF   FLG-DAY24    =   1
                       IF   RECE82-SRYYM  >= "200604"
                         CONTINUE
                       ELSE
                         PERFORM    2304-NYUINACCT-HEN-SEC
                         PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                            UNTIL  SYUKEI-IDX3           >  31
                             OR    WRK-SKJ2-ZAITEN(SYUKEI-IDX3) = ZERO
      *
                             PERFORM   2305-RECE84-TENSOU2-SKJ-SEC
                             PERFORM   2306-RECE85-HENSYU2-SKJ-SEC
                             PERFORM   900-RECE84-WRITE-SEC
      *
                         END-PERFORM
                       END-IF
                      END-IF
      *
                   END-IF
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
                 MOVE    "key7"            TO      MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"         TO      MCP-TABLE
           MOVE    "key7"                  TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    ∆±∆¸∫∆∆˛±°
      *
           IF   RECE82-SRYYM   >=  "200604"
              IF   FLG-DOJITU   =   1
                    PERFORM   2310-NYUINACCT-SYU-DOJITU-SEC
              END-IF
           END-IF
      *
      *    ∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€ºË∆¿
           IF    RECE82-SRYYM   >=  "200610"
             INITIALIZE                   ORCSNYULIFE-AREA
             MOVE    RECE82-HOSPNUM   TO  LNK-NYULIFE-HOSPNUM
             MOVE    RECE82-PTID      TO  LNK-NYULIFE-PTID
             MOVE    RECE82-SRYYM     TO  LNK-NYULIFE-SANTEIYM
             CALL    "ORCSNYULIFE"        USING
                                          ORCSNYULIFE-AREA
                                          SPA-AREA
             IF      LNK-NYULIFE-RC   =   ZERO
               PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                         UNTIL   SYUKEI-IDX3   >   31
                 MOVE  LNK-NYULIFE-KBN(SYUKEI-IDX3)
                                  TO  WRK-NYULIFE-KBN(SYUKEI-IDX3)
                 IF  ((RECE82-HKNKBN      =   "4") AND
                      (RECE82-JIBAIKBN    =   "2"))
                 OR  ((RECE82-HKNKBN      =   "5") AND
                      (RECE82-KOMUKBN     =   "2"))
                    CONTINUE
                 ELSE
                    MOVE  SPACE   TO  WRK-NYULIFE-KBN(SYUKEI-IDX3)
                 END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *    £≤£∞£∞£∂«Ø£¥∑Ó∞ πﬂ§Œø©ªˆŒ≈Õ‹
           IF   RECE82-SRYYM   >=  "200604"
      *
              PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                        UNTIL   SYUKEI-IDX3    >  31
      *
                 IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                                                    NOT =  ZERO
                      PERFORM  2305-RECE84-TENSOU2-SKJ200604-SEC
                      PERFORM  2306-RECE85-HENSYU2-SKJ200604-SEC
                      PERFORM  900-RECE84-WRITE-SEC
                 END-IF
      *
                 IF   WRK-SKJ200604-DAY-KAN(SYUKEI-IDX3)
                                                    NOT =  ZERO
                 AND  WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                      PERFORM  2308-RECE84-TENSOU2-SKJ200610-KAN-SEC
                      PERFORM  2309-RECE85-HENSYU2-SKJ200610-KAN-SEC
                      PERFORM  900-RECE84-WRITE-SEC
                 END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       230-RECE84-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ø©ªˆŒ≈Õ‹»Ò§ŒΩ∏∑◊
      **************************************************************
       2301-NYUINACCT-SYU-SEC           SECTION.
      *
           IF   RECE82-SRYYM  >= "200604"
      *
            IF  RECE82-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO
            AND RECE82-HKNCOMBI  NOT =  RECE82-DOUJITSU-HKNCOMBI(IDY)
             MOVE    1    TO  FLG-DOJITU
             MOVE    2    TO  WRK-SKJ-DOJITU-KBN (IDY)
             EVALUATE   NACCT-SRYCDTOTAL
               WHEN  099999918
                 MOVE  NACCT-DAY(IDY) TO  WRK-SKJ-DOJITU-ASA (IDY)
               WHEN  099999919
                 MOVE  NACCT-DAY(IDY) TO  WRK-SKJ-DOJITU-HIRU(IDY)
               WHEN  099999920
                 MOVE  NACCT-DAY(IDY) TO  WRK-SKJ-DOJITU-YORU(IDY)
             END-EVALUATE
            ELSE
             EVALUATE   WRK-SHOKUJIRYOYO(IDY)
              WHEN  1
                 EVALUATE   WRK-DAY24(IDY)
                   WHEN  1
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE  ZERO    TO  WRK-DAY4(IDY)
                     END-IF
                   WHEN  2
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 2)
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE  ZERO    TO  WRK-DAY4(IDY)
                     END-IF
                 END-EVALUATE
              WHEN  2
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
             END-EVALUATE
            END-IF
      *
           ELSE
      *
             MOVE       ZERO                TO   IDY2
      *
             EVALUATE   WRK-DAY24(IDY)
              WHEN  1
                EVALUATE   WRK-SHOKUJIRYOYO(IDY)
                  WHEN  1
      *              ∆˛±°ª˛ø©ªˆŒ≈Õ‹° £±°À
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              œ´∫“æËøÙ
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ∆√ Ã¥…Õ˝≤√ªª
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 œ´∫“æËøÙ
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ø©∆≤≤√ªª
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 œ´∫“æËøÙ
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
                  WHEN  2
      *              ∆˛±°ª˛ø©ªˆŒ≈Õ‹° £≤°À
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000710"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000710"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              œ´∫“æËøÙ
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                END-EVALUATE
              WHEN  2
      *              ∆˛±°ª˛ø©ªˆŒ≈Õ‹° £±°À
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              œ´∫“æËøÙ
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ∆√ Ã¥…Õ˝≤√ªª
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 œ´∫“æËøÙ
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ∆√ Ãø©≤√ªª
                     ADD    1             TO   IDY2
                     MOVE  "197000470"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000470"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              œ´∫“æËøÙ
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ø©∆≤≤√ªª
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 œ´∫“æËøÙ
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
              WHEN  3
      *              ∆˛±°ª˛ø©ªˆŒ≈Õ‹° £±°À
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              œ´∫“æËøÙ
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ∆√ Ã¥…Õ˝≤√ªª
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 œ´∫“æËøÙ
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ø©∆≤≤√ªª
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 œ´∫“æËøÙ
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *               £øÙ∏•Œ©≤√ªª
                     ADD    1             TO   IDY2
                     MOVE  "197000670"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000670"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              œ´∫“æËøÙ
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
              WHEN  4
      *              ∆˛±°ª˛ø©ªˆŒ≈Õ‹° £±°À
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              œ´∫“æËøÙ
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ∆√ Ã¥…Õ˝≤√ªª
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 œ´∫“æËøÙ
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ∆√ Ãø©≤√ªª
                     ADD    1             TO   IDY2
                     MOVE  "197000470"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000470"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              œ´∫“æËøÙ
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ø©∆≤≤√ªª
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 œ´∫“æËøÙ
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *               £øÙ∏•Œ©≤√ªª
                     ADD    1             TO   IDY2
                     MOVE  "197000670"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000670"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              œ´∫“æËøÙ
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
             END-EVALUATE
      *
           END-IF
      *
           .
       2301-NYUINACCT-SYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ø©ªˆŒ≈Õ‹»Ò§ŒΩ∏∑◊° ∆±∆¸∫∆∆˛±° ¨°À
      **************************************************************
       2310-NYUINACCT-SYU-DOJITU-SEC           SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
             EVALUATE  WRK-SKJ-DOJITU-KBN(IDY)
               WHEN         1
                 MOVE  WRK-DAY4X(IDY)
                                   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                 EVALUATE  RECE82-DOUJITSU-SKJKBN(IDY)
                   WHEN     "0"
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-ASA (IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-HIRU(IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-YORU(IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                   WHEN     "1"
                     CONTINUE
                   WHEN     "2"
                     IF   WRK-SKJ-DOJITU-ASA (IDY)  NOT = ZERO
                       MOVE ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-ASA (IDY)
                   WHEN     "3"
                     IF   WRK-SKJ-DOJITU-ASA (IDY)  NOT = ZERO
                     OR   WRK-SKJ-DOJITU-HIRU(IDY)  NOT = ZERO
                       MOVE ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-ASA (IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-HIRU(IDY)
                 END-EVALUATE
               WHEN         2
                 MOVE  WRK-DAY4X(IDY)
                                   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                 EVALUATE  RECE82-DOUJITSU-SKJKBN(IDY)
                   WHEN     "0"
                     CONTINUE
                   WHEN     "1"
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-ASA (IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-HIRU(IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-YORU(IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                   WHEN     "2"
                     IF   WRK-SKJ-DOJITU-ASA (IDY) = ZERO
                       MOVE ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-HIRU(IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-YORU(IDY)
                   WHEN     "3"
                     IF   WRK-SKJ-DOJITU-ASA (IDY) = ZERO
                     AND  WRK-SKJ-DOJITU-HIRU(IDY) = ZERO
                       MOVE ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-YORU(IDY)
                 END-EVALUATE
             END-EVALUATE
           END-PERFORM
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
             IF   WRK-SKJ-DOJITU-KBN(IDY)  NOT = ZERO
               EVALUATE   WRK-SHOKUJIRYOYO(IDY)
                 WHEN  1
                   IF   WRK-SKJ-DOJITU-ASA (IDY) =  1 OR 2
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     IF   WRK-SKJ-DOJITU-ASA (IDY) =  2
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 2)
                     END-IF
                     IF   WRK-SKJ-DOJITU-SHOKUDO(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE ZERO  TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                   END-IF
                   IF   WRK-SKJ-DOJITU-HIRU(IDY) =  1 OR 2
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     IF   WRK-SKJ-DOJITU-HIRU(IDY) =  2
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 2)
                     END-IF
                     IF   WRK-SKJ-DOJITU-SHOKUDO(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE ZERO  TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                   END-IF
                   IF   WRK-SKJ-DOJITU-YORU(IDY) =  1 OR 2
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     IF   WRK-SKJ-DOJITU-YORU(IDY) =  2
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 2)
                     END-IF
                     IF   WRK-SKJ-DOJITU-SHOKUDO(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE ZERO  TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                   END-IF
                 WHEN  2
                   IF   WRK-SKJ-DOJITU-ASA (IDY) =  1 OR 2
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                   END-IF
                   IF   WRK-SKJ-DOJITU-HIRU(IDY) =  1 OR 2
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                   END-IF
                   IF   WRK-SKJ-DOJITU-YORU(IDY) =  1 OR 2
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                   END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
      *
           .
       2310-NYUINACCT-SYU-DOJITU-EXT.
           EXIT.
      *
      **************************************************************
      *    ø©ªˆŒ≈Õ‹»Ò§Œ∑◊ªª
      **************************************************************
       23011-NYUINACCT-SYU-KEISAN-SEC           SECTION.
      *
           COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
           IF      RECE82-HKNKBN    =   "4"
           AND     RECE82-SKJKBN    =   "2"
               COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  1.2
           END-IF
           ADD      WRK-KEISAN  TO   WRK-SKJ-ZAITEN(IDY)
      *
           .
       23011-NYUINACCT-SYU-KEISAN-EXT.
           EXIT.
      *
      **************************************************************
      *    ø©ªˆŒ≈Õ‹»Ò§Œœ´∫“æËøÙ•ª•√•»
      **************************************************************
       23012-NYUINACCT-SYU-JYOSU-SEC           SECTION.
      *
           ADD    1             TO   IDY2
           MOVE  "101970010"    TO   WRK-SKJ-SRYCD-OCC (IDY IDY2)
           IF      RECE82-HKNKBN    =   "4"
           AND     RECE82-SKJKBN    =   "2"
      *       º´«Â¿’° £±°•£≤«‹°À
              ADD    1             TO   IDY2
      *       MOVE  "101970010"    TO   WRK-SKJ-SRYCD-OCC(IDY IDY2)
              MOVE  "099609901"    TO   WRK-SKJ-SRYCD-OCC(IDY IDY2)
           END-IF
      *
           .
       23012-NYUINACCT-SYU-JYOSU-EXT.
           EXIT.
      *
      **************************************************************
      *    ø©ªˆŒ≈Õ‹»Ò§ŒΩ∏∑◊° ¥ƒ∂≠°À
      **************************************************************
       2307-NYUINACCT-SYU-KAN-SEC           SECTION.
      *
           EVALUATE   WRK-SHOKUJIRYOYO(IDY)
             WHEN   1
                MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                MOVE    1       TO  WRK-SKJ200604-DAY-KAN(IDY)
             WHEN   2
                MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                MOVE    1       TO  WRK-SKJ200604-DAY-KAN(IDY)
           END-EVALUATE
      *
           .
       2307-NYUINACCT-SYU-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       2302-RECE84-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
      *
           EVALUATE  NACCT-ZAISKBKBN
            WHEN    "6"
            WHEN    "7"
             COMPUTE WRK-ZAINUM  = 99993000  +  WRK-ZAINUM-CNT2
             MOVE    WRK-ZAINUM         TO      RECE84-ZAINUM
            WHEN    OTHER
             MOVE    NACCT-ZAINUM       TO      RECE84-ZAINUM
           END-EVALUATE
      *
           MOVE    NACCT-SRYKBN         TO      RECE84-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE84-ZAISKBKBN
           IF      NACCT-ZAISKBKBN      =       "7"
      ****    IF   RECE82-HKNKBN        =       "4"
      ****      MOVE     "40"           TO      RECE84-SRYKBN
      ****    ELSE
                MOVE     "80"           TO      RECE84-SRYKBN
      ****    END-IF
              MOVE      SPACE           TO      RECE84-ZAISKBKBN
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    NACCT-ZAITEN         TO      RECE84-ZAITEN
           MOVE    NACCT-SRYCDTOTAL     TO      RECE84-SRYCDTOTAL
           IF      NACCT-ZAISKBKBN      =       "7"
                MOVE    199000510       TO      RECE84-SRYCDTOTAL
           END-IF
           MOVE    NACCT-SURYOUTOTAL    TO      RECE84-SURYOUTOTAL
           MOVE    NACCT-MEISAISU       TO      RECE84-MEISAISU
           MOVE    CNT-DAY21            TO      RECE84-ZAIKAISU
           MOVE    NACCT-SYUTEN1        TO      RECE84-SYUTEN1
           MOVE    NACCT-SYUTEN2        TO      RECE84-SYUTEN2
           MOVE    NACCT-YKZTEN         TO      RECE84-YKZTEN
           MOVE    NACCT-KIZAITEN       TO      RECE84-KIZAITEN
           IF    RECE82-HKNKBN    =  "4"
           AND   RECE82-JIBAIKBN  =  "2"
           AND   NACCT-ZAISKBKBN  =  "7"
             MOVE  NACCT-SYUTEN1        TO      RECE84-KIZAITEN
             MOVE  ZERO                 TO      RECE84-SYUTEN1
           END-IF
           MOVE    NACCT-DRCD           TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-DAY21(IDX2)  TO
                                            RECE84-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)   AND
                          (WRK-DAY21(IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       2302-RECE84-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£µ°À
      *****************************************************************
       2303-RECE85-HENSYU2-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
grpsys     PERFORM  900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =    "1")
                   PERFORM   23031-RECE85-TENSOU2-SEC
                   IF      NACCT-ZAISKBKBN      =       "7"
                      PERFORM   23032-RECE82-HENSYU2-SEC
                   END-IF
                   PERFORM   900-RECE85-WRITE-SEC
      *
                   MOVE    "tbl_nyuinact" TO   MCP-TABLE
                   MOVE    "key9"         TO   MCP-PATHNAME
                   PERFORM  900-NYUINACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinact"         TO   MCP-TABLE
           MOVE    "key9"                 TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2303-RECE85-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       23031-RECE85-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
      *
           EVALUATE  NACCT-ZAISKBKBN
            WHEN    "6"
            WHEN    "7"
             COMPUTE WRK-ZAINUM  = 99993000  +  WRK-ZAINUM-CNT2
             MOVE    WRK-ZAINUM         TO      RECE85-ZAINUM
            WHEN    OTHER
             MOVE    NSRY-ZAINUM        TO      RECE85-ZAINUM
           END-EVALUATE
      *
           MOVE    NSRY-RENNUM          TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE85-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE85-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE85-SRYSYUKBN
           IF      NACCT-ZAISKBKBN      =       "7"
      ****    IF    RECE82-HKNKBN       =       "4"
      ****       MOVE     "40"          TO      RECE85-SRYKBN
      ****    ELSE
                 MOVE     "80"          TO      RECE85-SRYKBN
      ****    END-IF
              MOVE       "800"          TO      RECE85-SRYSYUKBN
           END-IF
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (NSRY-SRYCD(IDX2)  =   SPACE)
             IF      NSRY-SRYCD(IDX2)(1:1)    =   "0"  OR  "1" OR  
                                                  "8"
               ADD     1                   TO  IDX4    
               MOVE    NSRY-SRYCD(IDX2)    TO  RECE85-SRYCD(IDX4)
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     "199000510"     TO  RECE85-SRYCD(IDX4)
               END-IF
               MOVE    NSRY-SRYSURYO(IDX2) TO  RECE85-SRYSURYO(IDX4)
               MOVE    NSRY-SRYKAISU(IDX2) TO  RECE85-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
               MOVE    NSRY-AUTOKBN(IDX2)  TO  RECE85-AUTOKBN(IDX4)
               MOVE    NSRY-INPUTNUM(IDX2) TO  RECE85-INPUTNUM(IDX4)
               MOVE    NSRY-SRYCD(IDX2)    TO  WRK-SRYCD
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     199000510       TO  WRK-SRYCD
               END-IF
               MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM    900-TENSU-TBL-SEC
               MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
               MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
               MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
               MOVE    TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
               MOVE    TNS-TEN             TO  RECE85-TEN(IDX4)
               IF    RECE82-HKNKBN    =  "4"
               AND   RECE82-JIBAIKBN  =  "1"
               AND   NACCT-ZAISKBKBN  =  "7"
                   MOVE  1                 TO  RECE85-TENSIKIBETU(IDX4)  
                   MOVE  120               TO  RECE85-TEN(IDX4)
               END-IF
               MOVE    TNS-NYUTENTTLKBN   TO    RECE85-TENTTLKBN(IDX4)
               MOVE    TNS-YUKOKETA       TO    RECE85-YUKOKETA(IDX4)
               MOVE    TNS-DATAKBN        TO    RECE85-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE85-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE85-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE85-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE85-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE85-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE85-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE85-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE85-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE85-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-SRYSYUKBN  TO RECE85-SRYSYUKBN-X  (IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE85-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE85-KNSJISGRPKBN (IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO RECE85-KENTAICOMMENT(IDX4)
               MOVE    TNS-KOKUJISKBKBN1
                                      TO RECE85-KOKUJISKBKBN1(IDX4)
               MOVE    TNS-KOKUJISKBKBN2
                                      TO RECE85-KOKUJISKBKBN2(IDX4)
               MOVE    TNS-TUSOKUGAIKBN
                                      TO RECE85-TUSOKUGAIKBN (IDX4)
               MOVE    TNS-TIMEKSNKBN TO RECE85-TIMEKSNKBN   (IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE85-CHUKSNCD     (IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE85-CHUKSNTUBAN  (IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO RECE85-TOKUKIZAISBT1(IDX4)
               MOVE    TNS-SANSOKBN   TO RECE85-SANSOKBN     (IDX4)
               IF    ((NSRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (NSRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( NSRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    NSRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    NSRY-PTID             TO  PTCOM-PTID
                   MOVE    NSRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    NSRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    NSRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 900-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT     TO  RECE85-NAME(IDX4)
               ELSE
                   MOVE    TNS-NAME              TO  RECE85-NAME(IDX4)
               END-IF
               IF    RECE82-HKNKBN    =  "4"
               AND   RECE82-JIBAIKBN  =  "1"
               AND   NACCT-ZAISKBKBN  =  "7"
      *
                 MOVE     1                 TO    WRK-MEISAI-CNT
                 MOVE     TNS-TEN           TO    WRK-TEN
                 MOVE     1                 TO    WRK-SURYOUTOTAL
                 MOVE     199000510         TO    WRK-SRYCDTOTAL
      *
                 MOVE     "2"               TO    RECE85-SIKFLG(IDX4)
               ELSE
                 MOVE     "1"               TO    RECE85-SIKFLG(IDX4)
               END-IF
      *
               MOVE    NSRY-SRYCD(IDX2)  TO  WRK-SRYCD
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     199000510     TO  WRK-SRYCD
               END-IF
               MOVE     ZERO    TO    WRK-FUKA-KINGAKU
               PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                         UNTIL   FUKA-IDX   >    9
                 MOVE   WRK-SRYCD(FUKA-IDX:1)
                                         TO  WRK-FUKA-VALUE
                 IF  WRK-FUKA-VALUE  =  ZERO
                   CONTINUE
                 ELSE
                   COMPUTE  RECE84-RUIKEITOTAL  =
                            RECE84-RUIKEITOTAL  +
                          ( WRK-FUKA-VALUE    * WRK-SRYCD)
                   COMPUTE  WRK-FUKA-KINGAKU    =
                            WRK-FUKA-KINGAKU    +
                          ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                              * NSRY-SRYSURYO(IDX2))
                 END-IF
               END-PERFORM
               COMPUTE  RECE84-RUIKEITOTAL  =
                        RECE84-RUIKEITOTAL  +
                        WRK-FUKA-KINGAKU
      *
             END-IF  
           END-PERFORM
      *
           .
       23031-RECE85-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° Œ≈Õ‹√¥≈ˆºÍ≈ˆ°À
      *****************************************************************
       23032-RECE82-HENSYU2-SEC                SECTION.
      *
      *    §Ω§Œ¬æ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
           EVALUATE   RECE82-HKNKBN
             WHEN      "1"
             WHEN      "2"
                COMPUTE    RECE82-TENSU(21)  =
                           RECE82-TENSU(21)  +
                          (NACCT-ZAITEN   *   CNT-DAY21)
             WHEN      "4"
                EVALUATE  WRK-PARA-RECEKBN
                  WHEN      "4"
      **************° ø∑ÕÕº∞°À
                     COMPUTE    RECE82-KINGAKU3(3)  =
                                RECE82-KINGAKU3(3)  +
                               (120            *   CNT-DAY21)
                  WHEN      "5"
      **************° ΩæÕËÕÕº∞°À
                   EVALUATE  RECE82-JIBAIKBN
                    WHEN   "1"
                     COMPUTE    RECE82-SONOTAKIN-JYU  =
                                RECE82-SONOTAKIN-JYU  +
                               (120            *   CNT-DAY21)
                    WHEN   "2"
                     COMPUTE   RECE82-TENSU(28)  =
                               RECE82-TENSU(28)  +
                               (NACCT-ZAITEN   *   CNT-DAY21)
                     COMPUTE   TBL-YKZTEN-JI(28)  =
                               TBL-YKZTEN-JI(28)  +
                               (NACCT-ZAITEN   *   CNT-DAY21)
                   END-EVALUATE
                END-EVALUATE
             WHEN      "5"
                COMPUTE    RECE82-TENSU(37)  =
                           RECE82-TENSU(37)  +
                          (NACCT-ZAITEN   *   CNT-DAY21)
           END-EVALUATE
      *
           .
       23032-RECE82-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ø©ªˆŒ≈Õ‹»Ò§Œ ‘Ω∏
      **************************************************************
       2304-NYUINACCT-HEN-SEC           SECTION.
      *
           PERFORM    VARYING  SYUKEI-IDX     FROM    1   BY  1
                  UNTIL  SYUKEI-IDX            >  31
      *
             IF    WRK-SKJ-ZAITEN(SYUKEI-IDX)  NOT =  ZERO
      *
               MOVE     ZERO     TO    FLG-ARI
               PERFORM  VARYING  SYUKEI-IDX2    FROM    1   BY  1
                      UNTIL  SYUKEI-IDX2           > 31
                       OR    WRK-SKJ2-ZAITEN(SYUKEI-IDX2) = ZERO
                       OR    FLG-ARI               = 1
                  IF  ( WRK-SKJ2-ZAITEN   (SYUKEI-IDX2)
                                     =  WRK-SKJ-ZAITEN   (SYUKEI-IDX) )
                  AND ( WRK-SKJ2-SRYCD-T  (SYUKEI-IDX2)
                                     =  WRK-SKJ-SRYCD-T  (SYUKEI-IDX) )
                     ADD   WRK-SKJ-ZAIKAISU(SYUKEI-IDX)  TO  
                                       WRK-SKJ2-ZAIKAISU (SYUKEI-IDX2)
                     MOVE  1  TO  WRK-SKJ2-KAISU(SYUKEI-IDX2 SYUKEI-IDX)
                     MOVE  1  TO   FLG-ARI
                  END-IF
               END-PERFORM  
               IF  FLG-ARI  =  ZERO  
                     MOVE  WRK-SKJ-ZAITEN   (SYUKEI-IDX)  TO  
                                       WRK-SKJ2-ZAITEN   (SYUKEI-IDX2)
                     ADD   WRK-SKJ-ZAIKAISU (SYUKEI-IDX)  TO  
                                       WRK-SKJ2-ZAIKAISU (SYUKEI-IDX2)
                     MOVE  WRK-SKJ-SRYCD-T  (SYUKEI-IDX)  TO  
                                       WRK-SKJ2-SRYCD-T  (SYUKEI-IDX2)
                     PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                        UNTIL ( SYUKEI-IDX3     >   15 )
                         OR   ( WRK-SKJ-SRYCD(SYUKEI-IDX SYUKEI-IDX3) 
                                                               =  ZERO)
      *
                        ADD   1  TO  WRK-SKJ2-MEISAISU(SYUKEI-IDX2)
                        ADD   WRK-SKJ-SRYCD(SYUKEI-IDX SYUKEI-IDX3)
                                 TO  WRK-SKJ2-SRYCDTOTAL(SYUKEI-IDX2)
      *
                     END-PERFORM
                     MOVE  1  TO  WRK-SKJ2-KAISU(SYUKEI-IDX2 SYUKEI-IDX)
               END-IF
      *
             END-IF
      *
           END-PERFORM
      *
           .
      *
       2304-NYUINACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       2305-RECE84-TENSOU2-SKJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    NACCT-SRYKBN         TO      RECE84-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    WRK-SKJ2-ZAITEN(SYUKEI-IDX3) 
                                        TO      RECE84-ZAITEN
           MOVE    WRK-SKJ2-SRYCDTOTAL(SYUKEI-IDX3)
                                        TO      RECE84-SRYCDTOTAL
           MOVE    WRK-SKJ2-MEISAISU(SYUKEI-IDX3)
                                        TO      RECE84-SURYOUTOTAL
           MOVE    WRK-SKJ2-MEISAISU(SYUKEI-IDX3)
                                        TO      RECE84-MEISAISU
           MOVE    WRK-SKJ2-ZAIKAISU(SYUKEI-IDX3)
                                        TO      RECE84-ZAIKAISU
           MOVE    WRK-SKJ2-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-SKJ2-KAISU(SYUKEI-IDX3 IDX2)             
                                        TO   RECE84-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)
                   AND    (WRK-SKJ2-KAISU(SYUKEI-IDX3 IDX2)             
                                                     NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       2305-RECE84-TENSOU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       2305-RECE84-TENSOU2-SKJ200604-SEC                SECTION.
      *
           ADD      1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
           IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
              ADD   1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
           END-IF
           IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
              ADD   1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
           END-IF
           IF  ((RECE82-HKNKBN      =   "4") AND
                (RECE82-JIBAIKBN    =   "2"))
           OR  ((RECE82-HKNKBN      =   "5") AND
                (RECE82-KOMUKBN     =   "2"))
             CONTINUE
           ELSE
             IF      RECE82-HKNKBN    =   "4"
             AND     RECE82-SKJKBN    =   "2"
                COMPUTE  WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) * 3
             ELSE
                COMPUTE  WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) * 2
             END-IF
           END-IF
      ***
           ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
           ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 2)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
           ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 3)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
      ***
           MOVE       ZERO     TO     IDX12
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
            EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
      *
            WHEN      1
      *
             ADD  197001310 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
             ADD  1         TO IDX12
             MOVE 197001310 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
             MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ADD   099609901
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  099609901
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
               ADD  197001570 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197001570 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 2)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
               ADD  197001670 TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO  IDX12
               MOVE 197001670 TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 3)
                            TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
            WHEN      2
      *
             ADD  197001710 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
             ADD  1         TO IDX12
             MOVE 197001710 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
             MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ADD   099609901
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  099609901
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              END-IF
             END-IF
      *
            END-EVALUATE
           ELSE
            EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
      *
            WHEN      1
      *
             ADD  197000110 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
             ADD  1         TO IDX12
             MOVE 197000110 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
             MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ADD   099609901
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  099609901
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
               ADD  197000470 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197000470 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 2)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
               ADD  197000570 TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO  IDX12
               MOVE 197000570 TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 3)
                            TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
            WHEN      2
      *
             ADD  197000710 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
             ADD  1         TO IDX12
             MOVE 197000710 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
             MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ADD   099609901
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  099609901
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              END-IF
             END-IF
      *
            END-EVALUATE
           END-IF
      ***
           MOVE    SPACE                TO  RECE84-REC
           INITIALIZE                       RECE84-REC
           MOVE    RECE82-HOSPNUM       TO  RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE84-PTID
           MOVE    RECE82-RECEKA        TO  RECE84-RECEKA
           MOVE    RECE82-HKNID         TO  RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE84-TEKSTYMD
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
      ***       COMPUTE WRK-ZAINUM  =  99999731  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996100  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE84-ZAINUM
           ELSE
      ***       COMPUTE WRK-ZAINUM  =  99999700  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996000  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE84-ZAINUM
           END-IF
      ***  COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
      ***  MOVE    WRK-ZAINUM           TO  RECE84-ZAINUM
           MOVE    "97"                 TO  RECE84-SRYKBN
           MOVE    "4"                  TO  RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO  RECE84-PTNUM
      *
           MOVE    SPACE                TO  RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO  RECE84-HKNCOMBI
           MOVE    SPACE                TO  RECE84-JIHOKBN
      *
           MOVE    WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                                        TO  RECE84-SRYCDTOTAL
           MOVE    WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                                        TO  RECE84-SURYOUTOTAL
           MOVE    WRK-SKJ200604-MEISAISU   (SYUKEI-IDX3)
                                        TO  RECE84-MEISAISU
           MOVE    1                    TO  RECE84-ZAIKAISU
      *
           MOVE    ZERO                 TO  RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO  RECE84-SYUTEN2
           MOVE    ZERO                 TO  RECE84-YKZTEN
           MOVE    ZERO                 TO  RECE84-KIZAITEN
           MOVE    SPACE                TO  RECE84-DRCD
           MOVE    SPACE                TO  RECE84-ZAIREQFLG
           MOVE    ZERO                 TO  RECE84-TEIGENRATE
           MOVE    1                    TO  RECE84-KAISU(1 SYUKEI-IDX3)
           MOVE    SYUKEI-IDX3          TO  WK-TAIHI-DAY
      *
           .
       2305-RECE84-TENSOU2-SKJ200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° ¥ƒ∂≠°À° £∆£∞£∞£¥°À
      *****************************************************************
       2308-RECE84-TENSOU2-SKJ200610-KAN-SEC                SECTION.
      *
           ADD     1    TO   WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3)
           IF  ((RECE82-HKNKBN      =   "4") AND
                (RECE82-JIBAIKBN    =   "2"))
           OR  ((RECE82-HKNKBN      =   "5") AND
                (RECE82-KOMUKBN     =   "2"))
             CONTINUE
           ELSE
             IF      RECE82-HKNKBN    =   "4"
             AND     RECE82-SKJKBN    =   "2"
                COMPUTE  WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) * 3
             ELSE
                COMPUTE  WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) * 2
             END-IF
           END-IF
      ***
           ADD     1    TO   WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
      ***
           MOVE       ZERO     TO     IDX12
           EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
            WHEN      1
             ADD  197001410
                        TO WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
             ADD  1     TO IDX12
             MOVE 197001410
                        TO WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
             MOVE 1     TO WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
                ADD   099609901
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  099609901
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              END-IF
             END-IF
            WHEN      2
             ADD  197001810
                        TO WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
             ADD  1     TO IDX12
             MOVE 197001810
                        TO WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
             MOVE 1     TO WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
                ADD   099609901
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  099609901
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              END-IF
             END-IF
           END-EVALUATE
      ***
           MOVE    SPACE                TO  RECE84-REC
           INITIALIZE                       RECE84-REC
           MOVE    RECE82-HOSPNUM       TO  RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE84-PTID
           MOVE    RECE82-RECEKA        TO  RECE84-RECEKA
           MOVE    RECE82-HKNID         TO  RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE84-TEKSTYMD
      ***  COMPUTE WRK-ZAINUM  =  99999762  +  SYUKEI-IDX3
           COMPUTE WRK-ZAINUM  =  99996300  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO  RECE84-ZAINUM
           MOVE    "97"                 TO  RECE84-SRYKBN
           MOVE    "4"                  TO  RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO  RECE84-PTNUM
      *
           MOVE    SPACE                TO  RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO  RECE84-HKNCOMBI
           MOVE    SPACE                TO  RECE84-JIHOKBN
      *
           MOVE    WRK-SKJ200604-SRYCDTOTAL-KAN (SYUKEI-IDX3)
                                        TO  RECE84-SRYCDTOTAL
           MOVE    WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
                                        TO  RECE84-SURYOUTOTAL
           MOVE    WRK-SKJ200604-MEISAISU-KAN   (SYUKEI-IDX3)
                                        TO  RECE84-MEISAISU
           MOVE    1                    TO  RECE84-ZAIKAISU
      *
           MOVE    ZERO                 TO  RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO  RECE84-SYUTEN2
           MOVE    ZERO                 TO  RECE84-YKZTEN
           MOVE    ZERO                 TO  RECE84-KIZAITEN
           MOVE    SPACE                TO  RECE84-DRCD
           MOVE    SPACE                TO  RECE84-ZAIREQFLG
           MOVE    ZERO                 TO  RECE84-TEIGENRATE
           MOVE    1                    TO  RECE84-KAISU(1 SYUKEI-IDX3)
           MOVE    SYUKEI-IDX3          TO  WK-TAIHI-DAY
      *
           .
       2308-RECE84-TENSOU2-SKJ200610-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£µ°À
      *****************************************************************
       2306-RECE85-HENSYU2-SKJ-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
           ELSE
               MOVE     1               TO      FLG-NYUINACT
           END-IF
           IF  FLG-NYUINACT   =   ZERO
               MOVE      1      TO     IDX7
               MOVE      1      TO     IDX8
               MOVE      5      TO     IDX9
               PERFORM   23061-RECE85-TENSOU2-SKJ-SEC
               PERFORM   900-RECE85-WRITE-SEC
               IF   WRK-SKJ2-SRYCD(SYUKEI-IDX3 6)  NOT =   ZERO
                  MOVE      2      TO     IDX7
                  MOVE      6      TO     IDX8
                  MOVE     10      TO     IDX9
                  PERFORM   23061-RECE85-TENSOU2-SKJ-SEC
                  PERFORM   900-RECE85-WRITE-SEC
               END-IF
               IF   WRK-SKJ2-SRYCD(SYUKEI-IDX3 11)  NOT =   ZERO
                  MOVE      3      TO     IDX7
                  MOVE     11      TO     IDX8
                  MOVE     15      TO     IDX9
                  PERFORM   23061-RECE85-TENSOU2-SKJ-SEC
                  PERFORM   900-RECE85-WRITE-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2306-RECE85-HENSYU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       23061-RECE85-TENSOU2-SKJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID      TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE85-ZAINUM
           MOVE    IDX7                 TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE85-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE85-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE85-SRYSYUKBN
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    IDX8    BY   1
                  UNTIL  (IDX2      >    IDX9)    
                    OR   (WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)  =   ZERO)
      *
                ADD     1                    TO    IDX4  
                MOVE    WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)
                                             TO  RECE85-SRYCD(IDX4)
                MOVE    1                    TO  RECE85-SRYSURYO(IDX4)
                MOVE    ZERO                 TO  RECE85-SRYKAISU(IDX4)
                MOVE    SPACE                TO  RECE85-MEISKYFLG(IDX4)
                MOVE    SPACE                TO  RECE85-AUTOKBN(IDX4)
                MOVE    ZERO                 TO  RECE85-INPUTNUM(IDX4)
                MOVE    WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)
                                             TO  WRK-SRYCD
                MOVE    RECE85-SRYYM         TO  WK-TAIHI-SRYYM
                MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
                PERFORM    900-TENSU-TBL-SEC
                MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
                MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
                MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
                MOVE    TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
                MOVE    TNS-TEN             TO  RECE85-TEN(IDX4)
                MOVE    TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
                MOVE    TNS-YUKOKETA        TO  RECE85-YUKOKETA(IDX4)
                MOVE    TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
                MOVE    TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
                MOVE    TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
                MOVE    TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
                MOVE    TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
                MOVE    TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
                MOVE    TNS-CDKBN-KBNNUM  
                                      TO  RECE85-CDKBN-KBNNUM(IDX4)
                MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
                MOVE    TNS-CDKBN-KOUBAN
                                      TO  RECE85-CDKBN-KOUBAN(IDX4)
                MOVE    TNS-KOUHYOJYUNNUM  
                                      TO  RECE85-KOUHYOJYUNNUM(IDX4)
                MOVE    TNS-SRYSYUKBN TO  RECE85-SRYSYUKBN-X  (IDX4)
                MOVE    TNS-KZMCOMPSIKIBETU
                                      TO  RECE85-KZMCOMPSIKIBETU(IDX4)
                MOVE    TNS-KNSJISGRPKBN
                                      TO  RECE85-KNSJISGRPKBN (IDX4)
                MOVE    TNS-KENTAICOMMENT
                                      TO  RECE85-KENTAICOMMENT(IDX4)
                MOVE    TNS-KOKUJISKBKBN1
                                      TO  RECE85-KOKUJISKBKBN1(IDX4)
                MOVE    TNS-KOKUJISKBKBN2
                                      TO  RECE85-KOKUJISKBKBN2(IDX4)
                MOVE    TNS-TUSOKUGAIKBN
                                      TO  RECE85-TUSOKUGAIKBN (IDX4)
                MOVE    TNS-TIMEKSNKBN
                                      TO  RECE85-TIMEKSNKBN   (IDX4)
                MOVE    TNS-CHUKSNCD  TO  RECE85-CHUKSNCD     (IDX4)
                MOVE    TNS-CHUKSNTUBAN
                                      TO  RECE85-CHUKSNTUBAN  (IDX4)
                MOVE    TNS-TOKUKIZAISBT1
                                      TO  RECE85-TOKUKIZAISBT1(IDX4)
                MOVE    TNS-SANSOKBN  TO  RECE85-SANSOKBN     (IDX4)
                MOVE    TNS-NAME            TO  RECE85-NAME(IDX4)
                MOVE     "1"                TO  RECE85-SIKFLG(IDX4)
      *
           END-PERFORM
      *
           .
       23061-RECE85-TENSOU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£µ°À
      *****************************************************************
       2306-RECE85-HENSYU2-SKJ200604-SEC               SECTION.
      *
           MOVE         1      TO     IDX7
           MOVE         1      TO     IDX8
           MOVE         5      TO     IDX9
           PERFORM   23061-RECE85-TENSOU2-SKJ200604-SEC
           PERFORM   900-RECE85-WRITE-SEC
      *
           IF   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)  >  5
              MOVE      2      TO     IDX7
              MOVE      6      TO     IDX8
              MOVE      9      TO     IDX9
              PERFORM   23061-RECE85-TENSOU2-SKJ200604-SEC
              PERFORM   900-RECE85-WRITE-SEC
           END-IF
      *
           .
       2306-RECE85-HENSYU2-SKJ200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       23061-RECE85-TENSOU2-SKJ200604-SEC                SECTION.
      *
           MOVE    SPACE                TO  RECE85-REC
           INITIALIZE                       RECE85-REC
           MOVE    RECE82-HOSPNUM       TO  RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE85-PTID
           MOVE    RECE82-RECEKA        TO  RECE85-RECEKA
           MOVE    RECE82-HKNID         TO  RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE85-TEKSTYMD
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
      ***       COMPUTE WRK-ZAINUM  =  99999731  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996100  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE85-ZAINUM
           ELSE
      ***       COMPUTE WRK-ZAINUM  =  99999700  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996000  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE85-ZAINUM
           END-IF
      ***  COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
      ***  MOVE    WRK-ZAINUM           TO  RECE85-ZAINUM
           MOVE    IDX7                 TO  RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO  RECE85-PTNUM
      *
           MOVE    SPACE                TO  RECE85-SRYKA
           MOVE    "97"                 TO  RECE85-SRYKBN
           MOVE    "970"                TO  RECE85-SRYSYUKBN
           MOVE    ZERO                 TO  RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO  IDX4
           PERFORM  VARYING  IDX2   FROM   IDX8   BY   1
                     UNTIL  (IDX2     >    IDX9)
                       OR   (WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX2)
                                                            =   ZERO)
      *
             ADD     1                   TO  IDX4
             MOVE    WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYCD(IDX4)
             MOVE    WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYSURYO(IDX4)
             MOVE    ZERO                TO  RECE85-SRYKAISU(IDX4)
             MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
             MOVE    SPACE               TO  RECE85-AUTOKBN(IDX4)
             MOVE    ZERO                TO  RECE85-INPUTNUM(IDX4)
             MOVE    RECE85-SRYCD(IDX4)  TO  WRK-SRYCD
             MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
             MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
             PERFORM    900-TENSU-TBL-SEC
             MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
             MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
             MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
             MOVE    TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
             MOVE    TNS-TEN             TO  RECE85-TEN(IDX4)
             MOVE    TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
             MOVE    TNS-YUKOKETA        TO  RECE85-YUKOKETA(IDX4)
             MOVE    TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
             MOVE    TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
             MOVE    TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
             MOVE    TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
             MOVE    TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
             MOVE    TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
             MOVE    TNS-CDKBN-KBNNUM    TO  RECE85-CDKBN-KBNNUM(IDX4)
             MOVE    TNS-CDKBN-KBNNUM-EDA
                                     TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
             MOVE    TNS-CDKBN-KOUBAN    TO  RECE85-CDKBN-KOUBAN(IDX4)
             MOVE    TNS-KOUHYOJYUNNUM   TO  RECE85-KOUHYOJYUNNUM(IDX4)
             MOVE    TNS-SRYSYUKBN       TO  RECE85-SRYSYUKBN-X  (IDX4)
             MOVE    TNS-KZMCOMPSIKIBETU TO  RECE85-KZMCOMPSIKIBETU
                                                                 (IDX4)
             MOVE    TNS-KNSJISGRPKBN    TO  RECE85-KNSJISGRPKBN (IDX4)
             MOVE    TNS-KENTAICOMMENT   TO  RECE85-KENTAICOMMENT(IDX4)
             MOVE    TNS-KOKUJISKBKBN1   TO  RECE85-KOKUJISKBKBN1(IDX4)
             MOVE    TNS-KOKUJISKBKBN2   TO  RECE85-KOKUJISKBKBN2(IDX4)
             MOVE    TNS-TUSOKUGAIKBN    TO  RECE85-TUSOKUGAIKBN (IDX4)
             MOVE    TNS-TIMEKSNKBN      TO  RECE85-TIMEKSNKBN   (IDX4)
             MOVE    TNS-CHUKSNCD        TO  RECE85-CHUKSNCD     (IDX4)
             MOVE    TNS-CHUKSNTUBAN     TO  RECE85-CHUKSNTUBAN  (IDX4)
             MOVE    TNS-TOKUKIZAISBT1   TO  RECE85-TOKUKIZAISBT1(IDX4)
             MOVE    TNS-SANSOKBN        TO  RECE85-SANSOKBN     (IDX4)
             MOVE    TNS-NAME            TO  RECE85-NAME(IDX4)
             MOVE     "1"                TO  RECE85-SIKFLG(IDX4)
      *
             IF    WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX2)  =
                            101970010   OR   099609901
               CONTINUE
             ELSE
              IF  ((RECE82-HKNKBN      =   "4") AND
                   (RECE82-JIBAIKBN    =   "2"))
              OR  ((RECE82-HKNKBN      =   "5") AND
                   (RECE82-KOMUKBN     =   "2"))
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (TNS-TEN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (TNS-TEN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
              ELSE
               IF    RECE82-HKNKBN    =   "4"
               AND   RECE82-SKJKBN    =   "2"
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  1.2
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
               ELSE
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
               END-IF
              END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       23061-RECE85-TENSOU2-SKJ200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° ¥ƒ∂≠°À° £∆£∞£∞£µ°À
      *****************************************************************
       2309-RECE85-HENSYU2-SKJ200610-KAN-SEC               SECTION.
      *
           MOVE         1      TO     IDX7
           MOVE         1      TO     IDX8
           MOVE         3      TO     IDX9
           PERFORM   23091-RECE85-TENSOU2-SKJ200610-KAN-SEC
           PERFORM   900-RECE85-WRITE-SEC
      *
           .
       2309-RECE85-HENSYU2-SKJ200610-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° ¥ƒ∂≠°À° £∆£∞£∞£µ°À
      *****************************************************************
       23091-RECE85-TENSOU2-SKJ200610-KAN-SEC                SECTION.
      *
           MOVE    SPACE                TO  RECE85-REC
           INITIALIZE                       RECE85-REC
           MOVE    RECE82-HOSPNUM       TO  RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE85-PTID
           MOVE    RECE82-RECEKA        TO  RECE85-RECEKA
           MOVE    RECE82-HKNID         TO  RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE85-TEKSTYMD
      ***  COMPUTE WRK-ZAINUM   =  99999762  +  SYUKEI-IDX3
           COMPUTE WRK-ZAINUM   =  99996300  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO  RECE85-ZAINUM
           MOVE    IDX7                 TO  RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO  RECE85-PTNUM
      *
           MOVE    SPACE                TO  RECE85-SRYKA
           MOVE    "97"                 TO  RECE85-SRYKBN
           MOVE    "970"                TO  RECE85-SRYSYUKBN
           MOVE    ZERO                 TO  RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO  IDX4
           PERFORM  VARYING  IDX2   FROM   IDX8   BY   1
                     UNTIL  (IDX2     >    IDX9)
                       OR   (WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX2)
                                                            =   ZERO)
      *
             ADD     1                   TO  IDX4
             MOVE    WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYCD(IDX4)
             MOVE    WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYSURYO(IDX4)
             MOVE    ZERO                TO  RECE85-SRYKAISU(IDX4)
             MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
             MOVE    SPACE               TO  RECE85-AUTOKBN(IDX4)
             MOVE    ZERO                TO  RECE85-INPUTNUM(IDX4)
             MOVE    RECE85-SRYCD(IDX4)  TO  WRK-SRYCD
             MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
             MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
             PERFORM    900-TENSU-TBL-SEC
             MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
             MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
             MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
             MOVE    TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
             MOVE    TNS-TEN             TO  RECE85-TEN(IDX4)
             MOVE    TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
             MOVE    TNS-YUKOKETA        TO  RECE85-YUKOKETA(IDX4)
             MOVE    TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
             MOVE    TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
             MOVE    TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
             MOVE    TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
             MOVE    TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
             MOVE    TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
             MOVE    TNS-CDKBN-KBNNUM    TO  RECE85-CDKBN-KBNNUM(IDX4)
             MOVE    TNS-CDKBN-KBNNUM-EDA
                                     TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
             MOVE    TNS-CDKBN-KOUBAN    TO  RECE85-CDKBN-KOUBAN(IDX4)
             MOVE    TNS-KOUHYOJYUNNUM   TO  RECE85-KOUHYOJYUNNUM(IDX4)
             MOVE    TNS-SRYSYUKBN       TO  RECE85-SRYSYUKBN-X  (IDX4)
             MOVE    TNS-KZMCOMPSIKIBETU TO  RECE85-KZMCOMPSIKIBETU
                                                                 (IDX4)
             MOVE    TNS-KNSJISGRPKBN    TO  RECE85-KNSJISGRPKBN (IDX4)
             MOVE    TNS-KENTAICOMMENT   TO  RECE85-KENTAICOMMENT(IDX4)
             MOVE    TNS-KOKUJISKBKBN1   TO  RECE85-KOKUJISKBKBN1(IDX4)
             MOVE    TNS-KOKUJISKBKBN2   TO  RECE85-KOKUJISKBKBN2(IDX4)
             MOVE    TNS-TUSOKUGAIKBN    TO  RECE85-TUSOKUGAIKBN (IDX4)
             MOVE    TNS-TIMEKSNKBN      TO  RECE85-TIMEKSNKBN   (IDX4)
             MOVE    TNS-CHUKSNCD        TO  RECE85-CHUKSNCD     (IDX4)
             MOVE    TNS-CHUKSNTUBAN     TO  RECE85-CHUKSNTUBAN  (IDX4)
             MOVE    TNS-TOKUKIZAISBT1   TO  RECE85-TOKUKIZAISBT1(IDX4)
             MOVE    TNS-SANSOKBN        TO  RECE85-SANSOKBN     (IDX4)
             MOVE    TNS-NAME            TO  RECE85-NAME(IDX4)
             MOVE     "1"                TO  RECE85-SIKFLG(IDX4)
      *
             IF    WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX2)  =
                            101970010   OR   099609901
               CONTINUE
             ELSE
              IF  ((RECE82-HKNKBN      =   "4") AND
                   (RECE82-JIBAIKBN    =   "2"))
              OR  ((RECE82-HKNKBN      =   "5") AND
                   (RECE82-KOMUKBN     =   "2"))
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (TNS-TEN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (TNS-TEN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
              ELSE
               IF    RECE82-HKNKBN    =   "4"
               AND   RECE82-SKJKBN    =   "2"
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  1.2
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
               ELSE
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
               END-IF
              END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       23091-RECE85-TENSOU2-SKJ200610-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£¥°À
      *****************************************************************
       240-RECE84-HENSYU3-SEC                SECTION.
      *
      *    ≈¿≈©§Œ∑◊ªª
      *
           INITIALIZE                  ORCSTENTEKAREA
           MOVE    "2"                 TO  ORCSTEN-KBN
           MOVE    RECE82-PTID         TO  ORCSTEN-PTID
           MOVE    RECE82-SRYYM        TO  ORCSTEN-SRYYM
           MOVE    RECE82-HKNCOMBI     TO  ORCSTEN-HKNCOMBI
      ***  MOVE    RECE82-HOSPNUM      TO  SPA-HOSPNUM
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *
      *    IF      ORCSTEN-ERRCD       =   ZERO
      *
      *           ÃÙ∫ﬁ
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                              UNTIL   IDY     >       31
      *
                    MOVE   SPACE  TO   WRK-FORMALNAME
                    MOVE   ZERO   TO   FLG-SETCOM
                    MOVE   ZERO   TO   FLG-HENSYU-OK
                    IF  ORCSTEN-ZAITEN(IDY)  NOT =  ZERO
      *
                       MOVE   1      TO   FLG-SIKIBETU
      *
                       INITIALIZE         WRK-ORCSTEN-TABLE
                       MOVE   ZERO   TO   IDX6
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       50
                                OR    ORCSTEN-SRYCD(IDY IDX5)  = SPACE
                          ADD   1   TO   IDX6
                          MOVE  ORCSTEN-SRYCD(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SRYCD(IDX6)
                          MOVE  ORCSTEN-SURYO(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SURYO(IDX6)
                          IF  ORCSTEN-HAIK-SRYCD(IDY IDX5) NOT = SPACE
                             ADD   1   TO   IDX6
                             MOVE  ORCSTEN-HAIK-SRYCD(IDY IDX5)
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  ORCSTEN-HAIK-SURYO(IDY IDX5)
                                       TO   WRK-ORCSTEN-SURYO(IDX6)
                          END-IF 
                          PERFORM  VARYING  IDX13   FROM    1   BY  1
                            UNTIL  IDX13    >      5
                              OR   ORCSTEN-COM-SRYCD(IDY IDX5 IDX13)
                                                              = SPACE
                             ADD   1   TO   IDX6
                             MOVE  "810000001"
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  1   TO   WRK-ORCSTEN-SURYO(IDX6)
                             MOVE  ORCSTEN-COM-MEI(IDY IDX5 IDX13)
                                       TO   WRK-ORCSTEN-COM-MEI(IDX6)
                          END-PERFORM
                       END-PERFORM
      *
                       PERFORM   2401-RECE84-TENSOU3-SEC
      *
                       MOVE      350  TO   IDX5
                       PERFORM   2402-RECE85-TENSOU3-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
      *
                    END-IF
      *
                  END-PERFORM
      *
      *           ¥Ô∫‡
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                              UNTIL   IDY     >       50
                                OR    ORCSTEN-KIZ-ZAITEN(IDY)  =  ZERO
      *
                       MOVE   SPACE  TO   WRK-FORMALNAME
                       MOVE   ZERO   TO   FLG-SETCOM
                       MOVE   ZERO   TO   FLG-HENSYU-OK
                       MOVE   2      TO   FLG-SIKIBETU
      *
                       INITIALIZE         WRK-ORCSTEN-TABLE
                       MOVE   ZERO   TO   IDX6
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       10
                                OR    ORCSTEN-KIZ-SRYCD(IDY IDX5)
                                                               = SPACE
                          ADD   1   TO   IDX6
                          MOVE  ORCSTEN-KIZ-SRYCD(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SRYCD(IDX6)
                          MOVE  ORCSTEN-KIZ-SURYO(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SURYO(IDX6)
                          PERFORM  VARYING  IDX13   FROM    1   BY  1
                            UNTIL  IDX13    >      5
                              OR   ORCSTEN-KIZ-COM-SRYCD(IDY IDX5 IDX13)
                                                              = SPACE
                             ADD   1   TO   IDX6
                             MOVE  "810000001"
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  1   TO   WRK-ORCSTEN-SURYO(IDX6)
                             MOVE  ORCSTEN-KIZ-COM-MEI(IDY IDX5 IDX13)
                                       TO   WRK-ORCSTEN-COM-MEI(IDX6)
                          END-PERFORM
                       END-PERFORM
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       31
                          MOVE  ORCSTEN-KIZ-DAY  (IDY IDX5)  TO  
                                      WRK-ORCSTEN-DAY  (IDX5)
                          ADD   ORCSTEN-KIZ-DAY  (IDY IDX5)  TO
                                      WRK-ORCSTEN-ZAIKAISU
                       END-PERFORM
      *
                       PERFORM   2401-RECE84-TENSOU3-SEC
      *
                       MOVE      60   TO   IDX5
                       PERFORM   2402-RECE85-TENSOU3-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
      *
                  END-PERFORM
      *
      *    END-IF
      *
      ***
      *
      *    ≈¿≈©ºÍµª≤√ªª
      *
           IF   WRK-TENTEKI2-HENKYAKUARI   NOT =  ZERO
      *
               IF   WRK-TENTEKI2-T-ERRCD       =  ZERO
                 PERFORM  VARYING  IDY    FROM    1   BY  1
                           UNTIL   IDY     >     31
                    MOVE    ZERO     TO   FLG-SETCOM
                    MOVE    ZERO     TO   FLG-HENSYU-OK
                    IF  WRK-TENTEKI2-T-ZAITEN(IDY)  NOT =  ZERO
                       MOVE      3   TO   FLG-SIKIBETU
                       PERFORM   2401X-RECE84-TENSOU3-SEC
                       MOVE      WRK-TENTEKI2-T-SRYCD-X(IDY)
                                     TO   WRK-TENTEKI2-SRYCD-X
                       PERFORM   2404-RECE85-TENTEKI2-HENSYU-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
                    END-IF
                 END-PERFORM
               END-IF
      *
               IF   WRK-TENTEKI2-C-ERRCD       =  ZERO
                 PERFORM  VARYING  IDY    FROM    1   BY  1
                           UNTIL   IDY     >     31
                    MOVE    ZERO     TO   FLG-SETCOM
                    MOVE    ZERO     TO   FLG-HENSYU-OK
                    IF  WRK-TENTEKI2-C-ZAITEN(IDY)  NOT =  ZERO
                       MOVE      4   TO   FLG-SIKIBETU
                       PERFORM   2401X-RECE84-TENSOU3-SEC
                       MOVE      WRK-TENTEKI2-C-SRYCD-X(IDY)
                                     TO   WRK-TENTEKI2-SRYCD-X
                       PERFORM   2404-RECE85-TENTEKI2-HENSYU-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
                    END-IF
                 END-PERFORM
               END-IF
      *
           END-IF
      *
           .
       240-RECE84-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       2401-RECE84-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           IF    FLG-SIKIBETU  =  1
             COMPUTE WRK-ZAINUM    =  99997000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99997400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    "33"                 TO      RECE84-SRYKBN
           MOVE    SPACE                TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    "00"                 TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE84-ZAITEN
           ELSE
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)  
                                        TO      RECE84-ZAITEN
           END-IF
           MOVE    ZERO                 TO      RECE84-SRYCDTOTAL
           MOVE    ZERO                 TO      RECE84-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE84-MEISAISU
           IF    FLG-SIKIBETU  =  1
             MOVE  1                    TO      RECE84-ZAIKAISU
           ELSE
             MOVE  WRK-ORCSTEN-ZAIKAISU TO      RECE84-ZAIKAISU
           END-IF
           MOVE    ZERO                 TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE84-YKZTEN
             MOVE  ZERO                 TO      RECE84-KIZAITEN
           ELSE
             MOVE  ZERO                 TO      RECE84-YKZTEN
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)
                                        TO      RECE84-KIZAITEN
           END-IF
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           IF    FLG-SIKIBETU  =  1
             MOVE    1                  TO      RECE84-KAISU (1 IDY)
             MOVE    IDY                TO      WK-TAIHI-DAY
           ELSE
             MOVE        ZERO           TO      WK-TAIHI-DAY
             MOVE        ZERO           TO      WK-TAIHI-FLG
             PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-ORCSTEN-DAY(IDX2)  TO
                                            RECE84-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (WRK-ORCSTEN-DAY(IDX2)  NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
             END-PERFORM
           END-IF
           .
       2401-RECE84-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       2401X-RECE84-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           IF    FLG-SIKIBETU  =  3
             COMPUTE WRK-ZAINUM    =  99995000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99995400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    "33"                 TO      RECE84-SRYKBN
           MOVE    SPACE                TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    "00"                 TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           IF    FLG-SIKIBETU  =  3
             MOVE  WRK-TENTEKI2-T-ZAITEN(IDY)
                                        TO      RECE84-ZAITEN
                                                RECE84-SYUTEN1
           ELSE
             MOVE  WRK-TENTEKI2-C-ZAITEN(IDY)
                                        TO      RECE84-ZAITEN
                                                RECE84-SYUTEN1
           END-IF
           MOVE    1                    TO      RECE84-ZAIKAISU
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           MOVE    1                    TO      RECE84-KAISU (1 IDY)
           MOVE    IDY                  TO      WK-TAIHI-DAY
           .
       2401X-RECE84-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       2402-RECE85-TENSOU3-SEC                SECTION.
      *
           MOVE    1                       TO   IDX2
           MOVE    ZERO                    TO   WRK-RENNUM
           MOVE    ZERO                    TO   WRK-TANICD
           MOVE    ZERO                    TO   WRK-TANIMOJISU
           MOVE    SPACE                   TO   WRK-TANINAME
           MOVE    ZERO                    TO   WRK-KZI058-TEN
           MOVE    ZERO                    TO   WRK-KZI058-TANICD
           MOVE    ZERO                    TO   WRK-KZI058-TANIMOJISU
           MOVE    SPACE                   TO   WRK-KZI058-TANINAME
      *
           PERFORM
                  UNTIL     (IDX2      >   IDX5)    OR
                            (WRK-ORCSTEN-SRYCD(IDX2)  =   SPACE)
      *
              MOVE    SPACE                TO   RECE85-REC
              INITIALIZE                        RECE85-REC
              MOVE    RECE82-HOSPNUM       TO   RECE85-HOSPNUM
              MOVE    RECE82-SRYYM         TO   RECE85-SRYYM
              MOVE    RECE82-NYUGAIKBN     TO   RECE85-NYUGAIKBN
              MOVE    RECE82-PTID          TO   RECE85-PTID
              MOVE    RECE82-RECEKA        TO   RECE85-RECEKA
              MOVE    RECE82-HKNID         TO   RECE85-HKNID
              MOVE    RECE82-TEKSTYMD      TO   RECE85-TEKSTYMD
              IF    FLG-SIKIBETU  =  1
                COMPUTE WRK-ZAINUM    =  99997000  +  IDY
              ELSE
                COMPUTE WRK-ZAINUM    =  99997400  +  IDY
              END-IF
              MOVE    WRK-ZAINUM           TO   RECE85-ZAINUM
              ADD     1                    TO   WRK-RENNUM
              MOVE    WRK-RENNUM           TO   RECE85-RENNUM
              MOVE    RECE82-PTNUM         TO   RECE85-PTNUM
      *
              MOVE    "00"                 TO   RECE85-SRYKA
              MOVE    "33"                 TO   RECE85-SRYKBN
              MOVE    "330"                TO   RECE85-SRYSYUKBN
              MOVE    ZERO                 TO   RECE85-JIHIMONEY
      *
              MOVE    ZERO                 TO      IDX4
              PERFORM
                     UNTIL  (IDX4      >=   5  )    OR
                            (IDX2      >   IDX5)    OR
                            (WRK-ORCSTEN-SRYCD(IDX2)  =   SPACE)
      *
                IF   WRK-ORCSTEN-SRYCD(IDX2)(1:1) =   "0" OR  "6" OR  
                                                      "7" OR  "8"
      *
                  ADD   1                   TO  IDX4    
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  RECE85-SRYCD(IDX4)
                  MOVE  WRK-ORCSTEN-SURYO(IDX2) 
                                            TO  RECE85-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE85-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE85-MEISKYFLG(IDX4)
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)  =  CONS-ZANHAIKI)
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"    )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008"  )
                    MOVE  "2"               TO  RECE85-MEISKYFLG(IDX4)
                  END-IF
                  MOVE  SPACE               TO  RECE85-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE85-INPUTNUM(IDX4)
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  WRK-SRYCD
                  MOVE  RECE85-SRYYM        TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM    900-TENSU-TBL-SEC
                  IF  WRK-ORCSTEN-SRYCD(IDX2)  =  CONS-ZANHAIKI
                    IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO
                      MOVE   WRK-TANICD     TO  RECE85-TANICD(IDX4)
                      MOVE   WRK-TANIMOJISU TO  RECE85-TANIMOJISU(IDX4)
                      MOVE   WRK-TANINAME   TO  RECE85-TANINAME(IDX4)
                    ELSE
                      MOVE   "1"            TO  RECE85-RECEDENFLG(IDX4)
                      MOVE   "1"            TO  RECE85-RECEPTFLG(IDX4)
                    END-IF
                  ELSE
                      IF      TNS-TANIMOJISU    =   ZERO
                      AND     TNS-TANINAME  NOT =   SPACE
                         PERFORM   VARYING   IDW   FROM   24   BY  -1
                                    UNTIL    IDW    <      1
                          IF   TNS-TANINAME(IDW:1)  NOT =   SPACE
                              COMPUTE  TNS-TANIMOJISU   =   IDW   /   2
                              MOVE    1     TO    IDW
                          END-IF
                         END-PERFORM
                      END-IF
                      IF   TNS-TANICD  NOT = SPACE AND "000"
                       MOVE TNS-TANICD      TO  RECE85-TANICD(IDX4)
                                                WRK-TANICD
                      END-IF
                      MOVE  TNS-TANIMOJISU  TO  RECE85-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU
                      MOVE  TNS-TANINAME    TO  RECE85-TANINAME(IDX4)
                                                WRK-TANINAME
                  END-IF
                  MOVE  TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
                  MOVE  TNS-TEN             TO  RECE85-TEN(IDX4)
                  MOVE  TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                     MOVE   ZERO            TO  RECE85-YUKOKETA(IDX4)
                  ELSE
                     MOVE   TNS-YUKOKETA    TO  RECE85-YUKOKETA(IDX4)
                  END-IF
                  MOVE  TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
                  MOVE  TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
                  MOVE  TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
                  MOVE  TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
                  MOVE  TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
                  MOVE  TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM    
                                      TO  RECE85-CDKBN-KBNNUM(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
                  MOVE  TNS-CDKBN-KOUBAN
                                      TO  RECE85-CDKBN-KOUBAN(IDX4)
                  MOVE  TNS-KOUHYOJYUNNUM  
                                      TO  RECE85-KOUHYOJYUNNUM(IDX4)
                  MOVE  TNS-SRYSYUKBN TO  RECE85-SRYSYUKBN-X  (IDX4)
                  MOVE  TNS-KZMCOMPSIKIBETU
                                      TO  RECE85-KZMCOMPSIKIBETU(IDX4)
                  MOVE  TNS-KNSJISGRPKBN
                                      TO  RECE85-KNSJISGRPKBN (IDX4)
                  MOVE  TNS-KENTAICOMMENT
                                      TO  RECE85-KENTAICOMMENT(IDX4)
                  MOVE  TNS-KOKUJISKBKBN1
                                      TO  RECE85-KOKUJISKBKBN1(IDX4)
                  MOVE  TNS-KOKUJISKBKBN2
                                      TO  RECE85-KOKUJISKBKBN2(IDX4)
                  MOVE  TNS-TUSOKUGAIKBN
                                      TO  RECE85-TUSOKUGAIKBN (IDX4)
                  MOVE  TNS-TIMEKSNKBN
                                      TO  RECE85-TIMEKSNKBN   (IDX4)
                  MOVE  TNS-CHUKSNCD  TO  RECE85-CHUKSNCD     (IDX4)
                  MOVE  TNS-CHUKSNTUBAN
                                      TO  RECE85-CHUKSNTUBAN  (IDX4)
                  MOVE  TNS-TOKUKIZAISBT1
                                      TO  RECE85-TOKUKIZAISBT1(IDX4)
                  MOVE  TNS-SANSOKBN  TO  RECE85-SANSOKBN     (IDX4)
                  IF      TNS-SRYCD (1:2)   =   "84"
                      PERFORM VARYING IDYY    FROM    1   BY  1
                              UNTIL   IDYY    >       10
                          MOVE    TNS-SSTKIJUNCD (IDYY)
                                      TO  RECE85-SSTKIJUNCD(IDX4 IDYY)
                      END-PERFORM
                  END-IF
      *
                  MOVE     "1"              TO  RECE85-SIKFLG(IDX4)
      *
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                     MOVE   WRK-ORCSTEN-COM-MEI(IDX2)
                                      TO RECE85-NAME(IDX4)
                                         RECE85-INPUTCOMENT(IDX4)
                  ELSE
                     MOVE   TNS-NAME  TO RECE85-NAME(IDX4)
                                         RECE85-INPUTCOMENT(IDX4)
                  END-IF
      *
                  IF  WRK-ORCSTEN-SRYCD(IDX2)  =  CONS-ZANHAIKI
                     IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO
                       PERFORM  220215-RECE85-TENSOU-SURYO-HEN-SEC
      *
                       MOVE    SPACE      TO  RECE85-NAME(IDX4)
                       MOVE    SPACE      TO  RECE85-INPUTCOMENT(IDX4)
                       STRING  TNS-NAME          DELIMITED  BY  " "
      ***                      "  "              DELIMITED  BY  SIZE
                               "°°"              DELIMITED  BY  SIZE
                               WRK-SRYSURYOW     DELIMITED  BY  " "
                               RECE85-TANINAME(IDX4)
                                                 DELIMITED  BY  " "
                               INTO              RECE85-NAME(IDX4)
                       END-STRING
                       MOVE  RECE85-NAME(IDX4)
                                          TO  RECE85-INPUTCOMENT(IDX4)
      *
      ***              MOVE  "810000001"  TO  RECE85-SRYCD(IDX4)
                       MOVE  ZERO         TO  RECE85-SRYSURYO(IDX4)
                       MOVE  ZERO         TO  RECE85-TANICD(IDX4)
                       MOVE  ZERO         TO  RECE85-TANIMOJISU(IDX4)
                       MOVE  SPACE        TO  RECE85-TANINAME(IDX4)
                       MOVE  ZERO         TO  RECE85-YUKOKETA(IDX4)
                     END-IF
                  END-IF
      *
                  IF   WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "058"
                    MOVE  TNS-FORMALYUKOKETA TO  RECE85-YUKOKETA(IDX4)
                    MOVE  TNS-FORMALNAME     TO  RECE85-NAME(IDX4)
                                                 WRK-FORMALNAME
                    MOVE  "1"                TO  RECE85-RECEDENFLG(IDX4)
      *
                    MOVE     TNS-TEN         TO  WRK-KZI058-TEN
                    IF   TNS-TANICD  NOT = SPACE AND "000"
                       MOVE  TNS-TANICD      TO  WRK-KZI058-TANICD
                       MOVE  TNS-TANIMOJISU  TO  WRK-KZI058-TANIMOJISU
                       MOVE  TNS-TANINAME    TO  WRK-KZI058-TANINAME
                    END-IF
                    MOVE     ZERO            TO  RECE85-TEN(IDX4)
                    MOVE     ZERO            TO  RECE85-TANICD(IDX4)
                    MOVE     ZERO            TO  RECE85-TANIMOJISU(IDX4)
                    MOVE     SPACE           TO  RECE85-TANINAME(IDX4)
                  END-IF
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "7")
                  AND (WRK-FORMALNAME     NOT =  SPACE      )
                    MOVE  WRK-FORMALNAME  TO  RECE85-RECEDENNAME(IDX4)
                    MOVE  "1"             TO  RECE85-RECEHENFLG(IDX4)
                    MOVE  SPACE           TO  WRK-FORMALNAME
                    IF    WRK-KZI058-TEN  NOT = ZERO
                       MOVE   WRK-KZI058-TEN
                                          TO  RECE85-TEN(IDX4)
                    END-IF
                    IF   (RECE85-TANICD(IDX4)    = ZERO)
                    AND  (WRK-KZI058-TANICD  NOT = ZERO)
                       MOVE   WRK-KZI058-TANICD
                                          TO  RECE85-TANICD(IDX4)
                       MOVE   WRK-KZI058-TANIMOJISU
                                          TO  RECE85-TANIMOJISU(IDX4)
                       MOVE   WRK-KZI058-TANINAME
                                          TO  RECE85-TANINAME(IDX4)
                    END-IF
                  END-IF
      *
                  IF   RECE85-YUKOKETA(IDX4)  =  ZERO
                     PERFORM   VARYING   IDW   FROM   200   BY  -1
                                UNTIL    IDW   <   1
                       IF   RECE85-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE85-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                     END-PERFORM
                  END-IF
      *
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                        MOVE     "1"             TO  RECE84-COMCDFLG
                  END-IF
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2)  TO  WRK-SRYCD
                  COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                              +  WRK-SRYCD
                  COMPUTE  RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                              +  WRK-ORCSTEN-SURYO(IDX2)
                  COMPUTE  RECE84-MEISAISU    =  RECE84-MEISAISU
                                              +  1
      *
                  MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                  PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                            UNTIL   FUKA-IDX   >    9
                    MOVE   WRK-ORCSTEN-SRYCD(IDX2)(FUKA-IDX:1)
                                            TO  WRK-FUKA-VALUE
                    IF  WRK-FUKA-VALUE  =  ZERO
                      CONTINUE
                    ELSE
                      COMPUTE  RECE84-RUIKEITOTAL  =
                               RECE84-RUIKEITOTAL  +
                             ( WRK-FUKA-VALUE    * WRK-SRYCD)
                      COMPUTE  WRK-FUKA-KINGAKU    =
                               WRK-FUKA-KINGAKU    +
                             ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                   * WRK-ORCSTEN-SURYO(IDX2))
                    END-IF
                  END-PERFORM
                  COMPUTE  RECE84-RUIKEITOTAL  =
                           RECE84-RUIKEITOTAL  +
                           WRK-FUKA-KINGAKU
      *
      *           ∆√ƒÍ¥Ô∫‡§Œ√±≤¡ ‘Ω∏
                  IF      WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "7"
                    IF  WRK-ORCSTEN-SRYCD(IDX2)       =
                                       CONS-FILM-YOUJI-KASAN  OR
                                       CONS-SANSO             OR
                                       CONS-SANSO2              
                      CONTINUE
                    ELSE
                      IF      TNS-TEN         =   ZERO
                        CONTINUE
                      ELSE
                        PERFORM 220213-RECE85-TENSOU-TANKA-HEN-SEC
                        COMPUTE IDW =   RECE85-YUKOKETA(IDX4)  *  2
                                    +   3
                        MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                   TO   RECE85-NAME(IDX4)(IDW:)
                        COMPUTE RECE85-YUKOKETA(IDX4)  =
                                RECE85-YUKOKETA(IDX4)  +
                                  1                    +
                               (WRK-CHAR-N-CNT  /  2)
                      END-IF
                    END-IF
                  END-IF
      **
                  IF   RECE85-MEISKYFLG(IDX4)  =  "2"
                     IF   (FLG-SETCOM              =  1   )
                     AND  (RECE85-RECEDENFLG(IDX4) = SPACE)
                        MOVE  "1"            TO  RECE84-SETCOMFLG
                     ELSE
                        MOVE  SPACE          TO  RECE85-MEISKYFLG(IDX4)
                     END-IF
                  ELSE
                     IF   (RECE85-SRYCD(IDX4)(1:1) = "1" OR "6" OR "7")
                     OR   (RECE85-SRYCD(IDX4)(1:3) = "059"            )
                        MOVE   1             TO  FLG-SETCOM
                     END-IF
                  END-IF
      **
                  IF      FLG-HENSYU-OK      =     ZERO
                    PERFORM  220217-TENTEKI-CDKBN-KBNNUM-HENSYU-SEC
                    MOVE     1     TO    FLG-HENSYU-OK
                  END-IF
      *
                END-IF  
      *
                ADD        1                TO  IDX2
      *
              END-PERFORM
      *
              PERFORM   900-RECE85-WRITE-SEC
      *
           END-PERFORM
      *
           .
       2402-RECE85-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       2404-RECE85-TENTEKI2-HENSYU-SEC                SECTION.
      *
           INITIALIZE             WRK-TENTEKI2-SRYCD2-X
      *
           MOVE      ZERO    TO   IDX4
           PERFORM  VARYING  IDX2   FROM    1    BY   1
                     UNTIL  (IDX2                     >      5  )
                       OR   (WRK-TENTEKI2-SRYCD(IDX2)  =   SPACE)
                       OR   (IDX4                     >=   100  )
               ADD    1      TO   IDX4
               MOVE   WRK-TENTEKI2-SRYCD(IDX2)
                             TO   WRK-TENTEKI2-SRYCD2(IDX4)
               PERFORM  VARYING  IDX5   FROM    1    BY   1
                         UNTIL  (IDX5                       >     5  )
                           OR   (WRK-TENTEKI2-COM-SRYCD(IDX2 IDX5)
                                                             =  SPACE)
                           OR   (IDX4                       >=  100  )
                     ADD    1   TO   IDX4
                     MOVE  "810000001"
                                TO   WRK-TENTEKI2-SRYCD2(IDX4)
                     MOVE  WRK-TENTEKI2-COM-FLG(IDX2 IDX5)
                                TO   WRK-TENTEKI2-COM-FLG2(IDX4)
                     MOVE  WRK-TENTEKI2-COM-MOJI(IDX2 IDX5)
                                TO   WRK-TENTEKI2-COM-MOJI2(IDX4)
               END-PERFORM
           END-PERFORM
      *
           MOVE    1                       TO   IDX2
           MOVE    ZERO                    TO   WRK-RENNUM
      *
           PERFORM   UNTIL  (WRK-TENTEKI2-SRYCD2(IDX2)  =   SPACE)
                      OR    (IDX2                       >   100  )
      *
              PERFORM   2402X-RECE85-TENSOU3-SEC
      *
           END-PERFORM
      *
           .
       2404-RECE85-TENTEKI2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       2402X-RECE85-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO   RECE85-REC
           INITIALIZE                        RECE85-REC
           MOVE    RECE82-HOSPNUM       TO   RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO   RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO   RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO   RECE85-PTID
           MOVE    RECE82-RECEKA        TO   RECE85-RECEKA
           MOVE    RECE82-HKNID         TO   RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO   RECE85-TEKSTYMD
           IF    FLG-SIKIBETU  =  3
             COMPUTE WRK-ZAINUM    =  99995000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99995400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO   RECE85-ZAINUM
           ADD     1                    TO   WRK-RENNUM
           MOVE    WRK-RENNUM           TO   RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO   RECE85-PTNUM
      *
           MOVE    "00"                 TO   RECE85-SRYKA
           MOVE    "33"                 TO   RECE85-SRYKBN
           IF    FLG-SIKIBETU  =  3
              MOVE    "330"             TO   RECE85-SRYSYUKBN
           ELSE
              MOVE    "340"             TO   RECE85-SRYSYUKBN
           END-IF
           MOVE    ZERO                 TO   RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO   IDX4
           PERFORM   UNTIL  (WRK-TENTEKI2-SRYCD2(IDX2)  =   SPACE)
                      OR    (IDX4                      >=     5  )
                      OR    (IDX2                      >    100  )
      *
                  ADD   1                   TO  IDX4
      *
                  MOVE  WRK-TENTEKI2-SRYCD2(IDX2)
                                            TO  RECE85-SRYCD(IDX4)
                  MOVE  1                   TO  RECE85-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE85-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE85-MEISKYFLG(IDX4)
                  IF   WRK-TENTEKI2-COM-FLG2(IDX2) = "1"
                    MOVE  "2"               TO  RECE85-MEISKYFLG(IDX4)
                  END-IF
                  MOVE  SPACE               TO  RECE85-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE85-INPUTNUM(IDX4)
                  MOVE  WRK-TENTEKI2-SRYCD2(IDX2)
                                            TO  WRK-SRYCD
                  MOVE  RECE85-SRYYM        TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM    900-TENSU-TBL-SEC
                  IF      TNS-TANIMOJISU    =   ZERO
                  AND     TNS-TANINAME  NOT =   SPACE
                     PERFORM   VARYING   IDW   FROM   24   BY  -1
                                UNTIL    IDW    <      1
                      IF   TNS-TANINAME(IDW:1)  NOT =   SPACE
                          COMPUTE  TNS-TANIMOJISU   =   IDW   /   2
                          MOVE    1     TO    IDW
                      END-IF
                     END-PERFORM
                  END-IF
                  MOVE  TNS-TANICD          TO  RECE85-TANICD(IDX4)
                  MOVE  TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
                  MOVE  TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
                  MOVE  TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
                  MOVE  TNS-TEN             TO  RECE85-TEN(IDX4)
                  MOVE  TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
                  IF  (WRK-TENTEKI2-SRYCD2(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-TENTEKI2-SRYCD2(IDX2)(1:3)  =   "008")
                     MOVE   ZERO            TO  RECE85-YUKOKETA(IDX4)
                  ELSE
                     MOVE   TNS-YUKOKETA    TO  RECE85-YUKOKETA(IDX4)
                  END-IF
                  MOVE  TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
                  MOVE  TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
                  MOVE  TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
                  MOVE  TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
                  MOVE  TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
                  MOVE  TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM
                                      TO  RECE85-CDKBN-KBNNUM(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
                  MOVE  TNS-CDKBN-KOUBAN
                                      TO  RECE85-CDKBN-KOUBAN(IDX4)
                  MOVE  TNS-KOUHYOJYUNNUM
                                      TO  RECE85-KOUHYOJYUNNUM(IDX4)
                  MOVE  TNS-SRYSYUKBN TO  RECE85-SRYSYUKBN-X  (IDX4)
                  MOVE  TNS-KZMCOMPSIKIBETU
                                      TO  RECE85-KZMCOMPSIKIBETU(IDX4)
                  MOVE  TNS-KNSJISGRPKBN
                                      TO  RECE85-KNSJISGRPKBN (IDX4)
                  MOVE  TNS-KENTAICOMMENT
                                      TO  RECE85-KENTAICOMMENT(IDX4)
                  MOVE  TNS-KOKUJISKBKBN1
                                      TO  RECE85-KOKUJISKBKBN1(IDX4)
                  MOVE  TNS-KOKUJISKBKBN2
                                      TO  RECE85-KOKUJISKBKBN2(IDX4)
                  MOVE  TNS-TUSOKUGAIKBN
                                      TO  RECE85-TUSOKUGAIKBN (IDX4)
                  MOVE  TNS-TIMEKSNKBN
                                      TO  RECE85-TIMEKSNKBN   (IDX4)
                  MOVE  TNS-CHUKSNCD  TO  RECE85-CHUKSNCD     (IDX4)
                  MOVE  TNS-CHUKSNTUBAN
                                      TO  RECE85-CHUKSNTUBAN  (IDX4)
                  MOVE  TNS-TOKUKIZAISBT1
                                      TO  RECE85-TOKUKIZAISBT1(IDX4)
                  MOVE  TNS-SANSOKBN  TO  RECE85-SANSOKBN     (IDX4)
                  IF      TNS-SRYCD (1:2)   =   "84"
                      PERFORM VARYING IDYY    FROM    1   BY  1
                              UNTIL   IDYY    >       10
                          MOVE    TNS-SSTKIJUNCD (IDYY)
                                      TO  RECE85-SSTKIJUNCD(IDX4 IDYY)
                      END-PERFORM
                  END-IF
                  MOVE     "1"        TO  RECE85-SIKFLG(IDX4)
      *
                  IF  (WRK-TENTEKI2-SRYCD2(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-TENTEKI2-SRYCD2(IDX2)(1:3)  =   "008")
                     MOVE   WRK-TENTEKI2-COM-MOJI2(IDX2)
                                      TO  RECE85-NAME(IDX4)
                                          RECE85-INPUTCOMENT(IDX4)
                  ELSE
                     MOVE   TNS-NAME  TO  RECE85-NAME(IDX4)
                                          RECE85-INPUTCOMENT(IDX4)
                  END-IF
      *
                  IF   RECE85-YUKOKETA(IDX4)  =  ZERO
                     PERFORM   VARYING   IDW   FROM   200   BY  -1
                                UNTIL    IDW   <   1
                       IF   RECE85-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE85-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                     END-PERFORM
                  END-IF
      *
                  IF  (WRK-TENTEKI2-SRYCD2(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-TENTEKI2-SRYCD2(IDX2)(1:3)  =   "008")
                        MOVE     "1"             TO  RECE84-COMCDFLG
                  END-IF
      *
                  MOVE  WRK-TENTEKI2-SRYCD2(IDX2)  TO  WRK-SRYCD
                  COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                              +  WRK-SRYCD
                  COMPUTE  RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                              +  1
                  COMPUTE  RECE84-MEISAISU    =  RECE84-MEISAISU
                                              +  1
      *
                  MOVE     ZERO       TO  WRK-FUKA-KINGAKU
                  PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                            UNTIL   FUKA-IDX   >    9
                    MOVE   WRK-TENTEKI2-SRYCD2(IDX2)(FUKA-IDX:1)
                                            TO  WRK-FUKA-VALUE
                    IF  WRK-FUKA-VALUE  =  ZERO
                      CONTINUE
                    ELSE
                      COMPUTE  RECE84-RUIKEITOTAL  =
                               RECE84-RUIKEITOTAL  +
                             ( WRK-FUKA-VALUE    * WRK-SRYCD)
                      COMPUTE  WRK-FUKA-KINGAKU    =
                               WRK-FUKA-KINGAKU    +
                             ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                 * 1)
                    END-IF
                  END-PERFORM
                  COMPUTE  RECE84-RUIKEITOTAL  =
                           RECE84-RUIKEITOTAL  +
                           WRK-FUKA-KINGAKU
      *
                  IF   RECE85-MEISKYFLG(IDX4)  =  "2"
                     IF   (FLG-SETCOM              =  1   )
                     AND  (RECE85-RECEDENFLG(IDX4) = SPACE)
                        MOVE  "1"            TO  RECE84-SETCOMFLG
                     ELSE
                        MOVE  SPACE          TO  RECE85-MEISKYFLG(IDX4)
                     END-IF
                  ELSE
                     IF   (RECE85-SRYCD(IDX4)(1:1) = "1" OR "6" OR "7")
                     OR   (RECE85-SRYCD(IDX4)(1:3) = "059"            )
                        MOVE   1             TO  FLG-SETCOM
                     END-IF
                  END-IF
      *
                  IF      FLG-HENSYU-OK      =     ZERO
                    PERFORM  220217-TENTEKI-CDKBN-KBNNUM-HENSYU-SEC
                    MOVE     1     TO    FLG-HENSYU-OK
                  END-IF
      *
                  ADD        1               TO  IDX2
      *
           END-PERFORM
      *
           IF     IDX4    NOT =  ZERO
              PERFORM   900-RECE85-WRITE-SEC
           END-IF
      *
           .
       2402X-RECE85-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜£≤° £∆£∞£∞£≤°À
      *****************************************************************
       2403-RECE82-HENSYU3-SEC               SECTION.
      *
      *    §Ω§Œ¬æ√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
           EVALUATE   RECE82-HKNKBN
            WHEN       "1"
            WHEN       "2"
             ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(12)
      *
             COMPUTE   RECE82-TENSU(12)  =
                       RECE82-TENSU(12)  +
                       (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
             COMPUTE   WRK-RECE82C-TENSU(12)  =
                       WRK-RECE82C-TENSU(12)  +
                       (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
            WHEN       "4"
                EVALUATE  WRK-PARA-RECEKBN
                  WHEN      "4"
      **************° ø∑ÕÕº∞°À
                    ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(12)
      *
                    IF    FLG-SIKIBETU  =  3 OR 4
                       COMPUTE   RECE82-TENSU(12)  =
                                 RECE82-TENSU(12)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                    ELSE
                       COMPUTE   RECE82-TENSU-YAK(13)  =
                                 RECE82-TENSU-YAK(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   WRK-RECE82C-TENSU-YAK(13)  =
                                 WRK-RECE82C-TENSU-YAK(13)  +
                                 (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
                    END-IF
                  WHEN      "5"
      **************° ΩæÕËÕÕº∞°À
                    ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(13)
      *
                    IF    FLG-SIKIBETU  =  3 OR 4
                       COMPUTE   RECE82-TENSU(13)  =
                                 RECE82-TENSU(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(13)  =
                                 TBL-SYUTEN-JI(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                    ELSE
                       COMPUTE   RECE82-TENSU(13)  =
                                 RECE82-TENSU(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(13) =
                                 TBL-YKZTEN-JI(13) +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   WRK-RECE82C-TENSU(13)  =
                                 WRK-RECE82C-TENSU(13)  +
                                 (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI-C(13)  =
                                 TBL-YKZTEN-JI-C(13)  +
                                 (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
                    END-IF
                END-EVALUATE
            WHEN       "5"
             ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(27)
      *
             COMPUTE   RECE82-TENSU(27)  =
                       RECE82-TENSU(27)  +
                       (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
             COMPUTE   WRK-RECE82C-TENSU(27)  =
                       WRK-RECE82C-TENSU(27)  +
                       (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
           END-EVALUATE
           .                                           
      *
       2403-RECE82-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£¥°À
      *****************************************************************
       250-RECE84-HENSYU4-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KHN
                                           WRK-SROUNYUIN-SRYCDTOTAL
                                           WRK-SROUNYUIN-MEISAISU
                                           WRK-SROUNYUIN-SRYCD
      *
           INITIALIZE                      ORCSROUNYUIN-AREA
           MOVE    RECE82-HOSPNUM      TO  LNK-SROUNYUIN-HOSPNUM
           MOVE    RECE82-PTID         TO  LNK-SROUNYUIN-PTID
           MOVE    RECE82-SRYYM        TO  LNK-SROUNYUIN-SANTEIYM
           MOVE    RECE82-HKNCOMBI     TO  LNK-SROUNYUIN-HKNCOMBI
           MOVE    SPACE               TO  LNK-SROUNYUIN-RYAKUKBN
      *
           CALL    "ORCSROUNYUIN"      USING   MCPAREA
                                               ORCSROUNYUIN-AREA
                                               SPA-AREA
      *
           IF   LNK-SROUNYUIN-RC   =   ZERO
      *
              PERFORM    VARYING   IDY   FROM   1   BY   1
                          UNTIL    IDY    >    31
      *
                IF   LNK-SROUNYUIN-NYUIN-FLG(IDY)    =  "1"
      ***       AND  RECE82-NYUINDAY-KBN(IDY)        =  SPACE
      *
                   MOVE     ZERO        TO    WRK-RENNUM
      *
                   IF   LNK-SROUNYUIN-GAIHAKU(IDY)  =  ZERO
      *
                     MOVE     1     TO    FLG-KHN
      *
                     PERFORM   2501-RECE84-TENSOU2-KHN-SEC
      *
                     MOVE     1     TO    SYUKEI-IDX
                     MOVE     5     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE     6     TO    SYUKEI-IDX
                     MOVE    10     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE    11     TO    SYUKEI-IDX
                     MOVE    15     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE    16     TO    SYUKEI-IDX
                     MOVE    20     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     PERFORM   900-RECE84-WRITE-SEC
      *
                   ELSE
      *
                     MOVE     2     TO    FLG-KHN
      *
                     MOVE      LNK-SROUNYUIN-SRYCD(IDY 1)
                                        TO   WRK-SROUNYUIN-SRYCD
                     COMPUTE   WRK-SROUNYUIN-SRYCDTOTAL      =
                               LNK-SROUNYUIN-SRYCDTOTAL(IDY) -
                               WRK-SROUNYUIN-SRYCD
                     COMPUTE   WRK-SROUNYUIN-MEISAISU        =
                               LNK-SROUNYUIN-MEISAISU(IDY)   -
                               1
      *
                     PERFORM   2501-RECE84-TENSOU2-KHN-SEC
      *
                     MOVE     2     TO    SYUKEI-IDX
                     MOVE     6     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE     7     TO    SYUKEI-IDX
                     MOVE    11     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE    12     TO    SYUKEI-IDX
                     MOVE    16     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE    17     TO    SYUKEI-IDX
                     MOVE    20     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     PERFORM   900-RECE84-WRITE-SEC
      *
                   END-IF
      *
                END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       250-RECE84-HENSYU4-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       2501-RECE84-TENSOU2-KHN-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
      *
           COMPUTE WRK-ZAINUM  =  99999800  +  IDY
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
      *
           MOVE    LNK-SROUNYUIN-SRYKBN(IDY)
                                        TO      RECE84-SRYKBN
           IF      FLG-KHN  =  1
              MOVE    "1"               TO      RECE84-ZAISKBKBN
           ELSE
              MOVE    "2"               TO      RECE84-ZAISKBKBN
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    "00"                 TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    LNK-SROUNYUIN-ZAITEN(IDY)
                                        TO      RECE84-ZAITEN
           IF      FLG-KHN  =  1
              MOVE    LNK-SROUNYUIN-SRYCDTOTAL(IDY)
                                        TO      RECE84-SRYCDTOTAL
              MOVE    LNK-SROUNYUIN-MEISAISU(IDY)
                                        TO      RECE84-SURYOUTOTAL
              MOVE    LNK-SROUNYUIN-MEISAISU(IDY)
                                        TO      RECE84-MEISAISU
           ELSE
              MOVE    WRK-SROUNYUIN-SRYCDTOTAL
                                        TO      RECE84-SRYCDTOTAL
              MOVE    WRK-SROUNYUIN-MEISAISU
                                        TO      RECE84-SURYOUTOTAL
              MOVE    WRK-SROUNYUIN-MEISAISU
                                        TO      RECE84-MEISAISU
           END-IF
           MOVE    1                    TO      RECE84-ZAIKAISU
           MOVE    LNK-SROUNYUIN-ZAITEN(IDY)
                                        TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           MOVE    1                    TO      RECE84-KAISU (1 IDY)
           MOVE    IDY                  TO      WK-TAIHI-DAY
           .
       2501-RECE84-TENSOU2-KHN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       2502-RECE85-TENSOU2-KHN-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
      *
           COMPUTE WRK-ZAINUM   =   99999800   +   IDY
           MOVE    WRK-ZAINUM           TO      RECE85-ZAINUM
      *
           ADD     1                    TO      WRK-RENNUM
           MOVE    WRK-RENNUM           TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    "00"                 TO      RECE85-SRYKA
           MOVE    LNK-SROUNYUIN-SRYKBN(IDY)
                                        TO      RECE85-SRYKBN
           IF      LNK-SROUNYUIN-SRYKBN(IDY) =  "90"
              MOVE    "900"             TO      RECE85-SRYSYUKBN
           ELSE
              MOVE    "920"             TO      RECE85-SRYSYUKBN
           END-IF 
           MOVE    ZERO                 TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM   SYUKEI-IDX   BY   1
               UNTIL (IDX2      >    SYUKEI-IDX2)
                OR   (LNK-SROUNYUIN-SRYCD(IDY IDX2)  =   SPACE)
      *
               ADD     1                   TO  IDX4    
               MOVE    LNK-SROUNYUIN-SRYCD(IDY IDX2)
                                           TO  RECE85-SRYCD(IDX4)
               MOVE    1                   TO  RECE85-SRYSURYO(IDX4)
               MOVE    ZERO                TO  RECE85-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
               MOVE    SPACE               TO  RECE85-AUTOKBN(IDX4)
               MOVE    ZERO                TO  RECE85-INPUTNUM(IDX4)
               MOVE    LNK-SROUNYUIN-SRYCD(IDY IDX2)
                                           TO  WRK-SRYCD
               MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM    900-TENSU-TBL-SEC
               MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
               MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
               MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
               MOVE    TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
               MOVE    TNS-TEN             TO  RECE85-TEN(IDX4)
               MOVE    TNS-NYUTENTTLKBN   TO    RECE85-TENTTLKBN(IDX4)
               MOVE    TNS-YUKOKETA       TO    RECE85-YUKOKETA(IDX4)
               MOVE    TNS-DATAKBN        TO    RECE85-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE85-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE85-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE85-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE85-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE85-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE85-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE85-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE85-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE85-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-SRYSYUKBN  TO RECE85-SRYSYUKBN-X  (IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE85-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE85-KNSJISGRPKBN (IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO RECE85-KENTAICOMMENT(IDX4)
               MOVE    TNS-KOKUJISKBKBN1
                                      TO RECE85-KOKUJISKBKBN1(IDX4)
               MOVE    TNS-KOKUJISKBKBN2
                                      TO RECE85-KOKUJISKBKBN2(IDX4)
               MOVE    TNS-TUSOKUGAIKBN
                                      TO RECE85-TUSOKUGAIKBN (IDX4)
               MOVE    TNS-TIMEKSNKBN TO RECE85-TIMEKSNKBN   (IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE85-CHUKSNCD     (IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE85-CHUKSNTUBAN  (IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO RECE85-TOKUKIZAISBT1(IDX4)
               MOVE    TNS-SANSOKBN   TO RECE85-SANSOKBN     (IDX4)
               MOVE    TNS-NAME           TO    RECE85-NAME(IDX4)
               MOVE     "1"               TO    RECE85-SIKFLG(IDX4)
      *
           END-PERFORM
      *
           .
       2502-RECE85-TENSOU2-KHN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§ÿ§Œ…È√¥∂‚•ª•√•»° £∆£∞£∞£≤°À
      *****************************************************************
       260-FTN-SET-SEC                SECTION.
      *
           MOVE    ZERO                 TO      FLG-KHNRYO-SKJ
      *
           MOVE    ZERO                 TO      FLG-NYUINRYO
      *
           MOVE    ZERO                 TO      FLG-SYUNOU-ARI
      *
           INITIALIZE                           SYUNOU-REC
           MOVE    RECE82-HOSPNUM       TO      SYU-HOSPNUM
           MOVE    RECE82-NYUGAIKBN     TO      SYU-NYUGAIKBN
           MOVE    RECE82-PTID          TO      SYU-PTID
           MOVE    RECE82-SRYYM         TO      SYU-SRYYMD(1:6)
           MOVE    "__"                 TO      SYU-SRYYMD(7:2)
           MOVE    RECE82-HKNCOMBI      TO      SYU-HKNCOMBINUM
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key19"              TO      MCP-PATHNAME
grpsys     PERFORM  900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_syunou"     TO      MCP-TABLE
               MOVE    "key19"          TO      MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
               PERFORM  UNTIL
                  (FLG-SYUNOU             =    1)
      *
                   IF  SYU-DENPJTIKBN    NOT =  "3"
                       PERFORM   2601-RECE82-FTN-HENSYU-SEC
                       MOVE   1   TO   FLG-SYUNOU-ARI
                   END-IF
      *
                   MOVE    "tbl_syunou" TO      MCP-TABLE
                   MOVE    "key19"      TO      MCP-PATHNAME
                   PERFORM  900-SYUNOU-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key19"              TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    ø©ªˆŒ¨æŒ ‘Ω∏
      *
           IF   RECE82-SRYYM  >= "200604"
           AND  RECE82-SRYYM  <  "200610"
                PERFORM    2602-SKJ-RAYKU-HENSYU-SEC
           END-IF
      *
           IF   FLG-SYUNOU-ARI  =  ZERO
              DISPLAY "<SYUNOU NG STOP>"
              MOVE    "≤æº˝«º•«°º•ø§¨§¢§Í§ﬁ§ª§Û"  TO  ERR-ERRMSG
              PERFORM 600-ERRMSG-EDIT-SEC
              MOVE    ERR-HEN-ERRMSG    TO  WRK-RECEERR
              PERFORM 500-ERR-HENSYU-SEC
              MOVE    1                 TO  FLG-END
           END-IF
      *
           .
       260-FTN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜°›…È√¥∂‚° £∆£∞£∞£≤°À
      *****************************************************************
       2601-RECE82-FTN-HENSYU-SEC               SECTION.
      *
           IF   RECE82-NYUINRYOFLG       =   "1"
           AND  FLG-NYUINRYO             =   ZERO
               PERFORM         26011-NYUINRYOFLG-SET-SEC
           END-IF
      *
           IF    RECE82-SRYYM  >= "200610"
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >    10
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                   CONTINUE
               ELSE
                 IF   (RECE82-HKNKBN         = "4")
                 AND  (WRK-PARA-RECEKBN      = "5")
                 AND  (WRK-PARA-JIBAIPRTKBN  = "1")
                   EVALUATE    ID1
                     WHEN       1
                     WHEN       2
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(1)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(1)
                      EVALUATE  ID1
                        WHEN     1
                          MOVE  "£±"   TO  RECE82-SHOKUJISYUBETU(1)
                        WHEN     2
                          MOVE  "£≤"   TO  RECE82-SHOKUJISYUBETU(1)
                      END-EVALUATE
                     WHEN       3
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(2)
                      MOVE  "∆√ Ã" TO  RECE82-SHOKUJISYUBETU(2)
                     WHEN       4
                     WHEN       8
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(3)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(3)
                      MOVE  "ø©∆≤" TO  RECE82-SHOKUJISYUBETU(3)
                     WHEN       9
                     WHEN      10
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(4)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(4)
                      MOVE  "¥ƒ∂≠" TO  RECE82-SHOKUJISYUBETU(4)
                     WHEN       5
                     WHEN       6
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-SEI(1)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO-SEI(1)
                      EVALUATE  ID1
                        WHEN     5
                          MOVE  "£±"   TO  RECE82-SHOKUJISYUBETU-KHNSEI
                        WHEN     6
                          MOVE  "£≤"   TO  RECE82-SHOKUJISYUBETU-KHNSEI
                      END-EVALUATE
                     WHEN       7
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-SEI(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO-SEI(2)
                   END-EVALUATE
                 ELSE
                   EVALUATE    ID1
                     WHEN       1
                     WHEN       5
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(1)
                      IF   FLG-KHNRYO-SKJ   =   ZERO
                        IF   RECE82-SHOKUJIKHNRYO(1)     =  ZERO
                          MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE82-SHOKUJIKHNRYO(1)
                        ELSE
                          IF RECE82-SHOKUJIKHNRYO(1) NOT =
                                                SYU-SHOKUJI(ID1)
                            MOVE   ZERO  TO  RECE82-SHOKUJIKHNRYO(1)
                            MOVE   1     TO  FLG-KHNRYO-SKJ
                          END-IF
                        END-IF
                      END-IF
                      MOVE  "£±"   TO  RECE82-SHOKUJISYUBETU(1)
                     WHEN       2
                     WHEN       6
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(1)
                      IF   FLG-KHNRYO-SKJ   =   ZERO
                        IF   RECE82-SHOKUJIKHNRYO(1)     =  ZERO
                          MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE82-SHOKUJIKHNRYO(1)
                        ELSE
                          IF RECE82-SHOKUJIKHNRYO(1) NOT =
                                                SYU-SHOKUJI(ID1)
                            MOVE   ZERO  TO  RECE82-SHOKUJIKHNRYO(1)
                            MOVE   1     TO  FLG-KHNRYO-SKJ
                          END-IF
                        END-IF
                      END-IF
                      MOVE  "£≤"   TO  RECE82-SHOKUJISYUBETU(1)
                     WHEN       3
                     WHEN       7
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(2)
                      MOVE  "∆√ Ã" TO  RECE82-SHOKUJISYUBETU(2)
                     WHEN       4
                     WHEN       8
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(3)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(3)
                      MOVE  "ø©∆≤" TO  RECE82-SHOKUJISYUBETU(3)
                     WHEN       9
                     WHEN      10
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(4)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(4)
                      MOVE  "¥ƒ∂≠" TO  RECE82-SHOKUJISYUBETU(4)
                   END-EVALUATE
                 END-IF
               END-IF
      *
             END-PERFORM
      *
           ELSE
      *
            IF   RECE82-SRYYM  >= "200604"
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >     4
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                   CONTINUE
               ELSE
                   ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(ID1)
                   MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(ID1)
               END-IF
      *
             END-PERFORM
      *
            ELSE
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >     5
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                 CONTINUE
               ELSE
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID2    FROM    1   BY  1
                           UNTIL   ID2                       > 2
                             OR    RECE82-SHOKUJIKHNRYO(ID2) = ZERO
                             OR    FLG-ARI                   = 1
                     IF  SYU-SHOKUJI(ID1) = RECE82-SHOKUJIKHNRYO(ID2)
                           ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                          RECE82-SHOKUJIKHNNISSU(ID2)
                           MOVE  1   TO   FLG-ARI
                     END-IF
                 END-PERFORM
                 IF  FLG-ARI  =  ZERO
                 AND ID2     <=   2
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                          RECE82-SHOKUJIKHNNISSU(ID2)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                          RECE82-SHOKUJIKHNRYO(ID2)
                 END-IF
      *
                 IF  FLG-ARI  =  ZERO
                 AND ID2      >   2
                   MOVE     ZERO     TO    FLG-ARI2
                   PERFORM  VARYING  ID4    FROM    1   BY  1
                             UNTIL ID4                           > 3
                               OR  RECE82-SHOKUJIKHNRYO-ETC(ID4) = ZERO
                               OR  FLG-ARI2                      = 1
                       IF  SYU-SHOKUJI(ID1)   =
                                       RECE82-SHOKUJIKHNRYO-ETC(ID4)
                             ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-ETC(ID4)
                             MOVE  1   TO   FLG-ARI2
                       END-IF
                   END-PERFORM
                   IF  FLG-ARI2  =  ZERO
                   AND ID4      <=   3
                       ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-ETC(ID4)
                       MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO-ETC(ID4)
                   END-IF
                 END-IF
               END-IF
      *
             END-PERFORM
      *
             IF      SYU-SHOKUJI(6)  NOT =  ZERO
                   ADD   SYU-SHOKUJI-NISSU(6)  TO
                                       RECE82-SHOKUJIKHNNISSU(3)
                   MOVE  SYU-SHOKUJI(6)        TO
                                       RECE82-SHOKUJIKHNRYO(3)
             END-IF
             IF      SYU-SHOKUJI(7)  NOT =  ZERO
                   ADD   SYU-SHOKUJI-NISSU(7)  TO
                                       RECE82-SHOKUJIKHNNISSU(4)
                   MOVE  SYU-SHOKUJI(7)        TO
                                       RECE82-SHOKUJIKHNRYO(4)
             END-IF
      *
            END-IF
      *
           END-IF
      *
           IF      SYU-SYUSKJ-RYOYOHI  NOT =  ZERO
                   ADD   SYU-SYUSKJ-NISSU     TO  
                                       RECE82-SHOKUJITOTALDAY
                   ADD   SYU-SYUSKJ-RYOYOHI   TO
                                       RECE82-SHOKUJITOTALKIN
           END-IF
      *
           .
       2601-RECE82-FTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¬æ ›∏±§À§∆∆˛±°Œ¡§ÚªªƒÍ§∑§∆§§§ÎæÏπÁ§Œ ›∏±∑ËƒÍ
      *****************************************************************
       26011-NYUINRYOFLG-SET-SEC               SECTION.
      *
           MOVE     SPACE    TO    RECE82-NYUINRYOFLG
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                            UNTIL   IDY     >       31
      *
             IF      SYU-DAY(IDY)    =    "2"
      *
                IF         WRK-DAY5(IDY)      NOT =  ZERO
                AND        WRK-DAY1(IDY)      NOT =  ZERO
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO1-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE82-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "6"     TO   RECE82-NYUINRYOFLG
                       WHEN     "2"
                          PERFORM  260111-NYUINRYOFLG-HENSYU-SEC
                     END-EVALUATE
                ELSE
                   IF      WRK-DAY5(IDY)              NOT =  ZERO
                   AND     RECE82-NYUINDAY-KBN(IDY)   NOT =  "1"
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE82-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "1"     TO   RECE82-NYUINRYOFLG
                       WHEN     "2"
                          MOVE    "6"     TO   RECE82-NYUINRYOFLG
                       WHEN     "3"
                          PERFORM   260111-NYUINRYOFLG-HENSYU-SEC
                     END-EVALUATE
                   END-IF
                   IF      WRK-DAY5(IDY)              NOT =  ZERO
                   AND     RECE82-NYUINDAY-KBN(IDY)       =  "1"
                   AND     RECE82-SRYYM                  >=  "200904"
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO1-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE82-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "6"     TO   RECE82-NYUINRYOFLG
                       WHEN     "2"
                          PERFORM  260111-NYUINRYOFLG-HENSYU-SEC
                     END-EVALUATE
                   END-IF
                END-IF
      *
                MOVE      1       TO     FLG-NYUINRYO
                MOVE     31       TO     IDY
      *
             END-IF
      *
           END-PERFORM
      *
           .
       26011-NYUINRYOFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     ›∏± ‘Ω∏
      *****************************************************************
       260111-NYUINRYOFLG-HENSYU-SEC               SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    RECE82-HOSPNUM  TO  COMB-HOSPNUM
           MOVE    RECE82-PTID     TO  COMB-PTID
           MOVE    WRK-DAY5(IDY)   TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 900-HKNCOMBI-INV-SEC
           IF      FLG-HKNCOMBI    =   ZERO
              IF   COMB-HKNNUM(1:2)  =  "98"
              OR   COMB-HKNNUM       =  "971"
              OR   COMB-HKNNUM       =  "973"
              OR   COMB-HKNNUM       =  "975"
                IF   COMB-HKNNUM(1:2)  =  "98"
                   MOVE    "1"   TO   RECE82-NYUINRYOFLG
                END-IF
                IF   COMB-HKNNUM       =  "971"
                   IF   RECE82-HKNNUM  =  "971"
                     CONTINUE
                   ELSE
                     MOVE   "2"  TO   RECE82-NYUINRYOFLG
                   END-IF
                END-IF
                IF   COMB-HKNNUM       =  "973"
                   IF   RECE82-HKNNUM  =  "973"
                     CONTINUE
                   ELSE
                     MOVE   "3"  TO   RECE82-NYUINRYOFLG
                   END-IF
                END-IF
                IF   COMB-HKNNUM       =  "975"
                   MOVE    "7"   TO   RECE82-NYUINRYOFLG
                END-IF
              ELSE
                EVALUATE  COMB-HKNNUM
                  WHEN     "060"
                  WHEN     "067"
                  WHEN     "068"
                  WHEN     "069"
                    MOVE   "4"   TO   RECE82-NYUINRYOFLG
                  WHEN     "039"
                  WHEN     "040"
                    MOVE   "8"   TO   RECE82-NYUINRYOFLG
                  WHEN      OTHER
                    MOVE   "5"   TO   RECE82-NYUINRYOFLG
                    IF   COMB-HKNNUM(1:2)  =  "90" OR "91"
                     MOVE SPACE  TO   RECE82-NYUINRYOFLG
                    END-IF
                END-EVALUATE
              END-IF
           END-IF
      *
           .
       260111-NYUINRYOFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø©ªˆŒ¨æŒ ‘Ω∏
      *****************************************************************
       2602-SKJ-RAYKU-HENSYU-SEC               SECTION.
      *
           PERFORM  VARYING  ID1    FROM    1   BY  1
                     UNTIL   ID1      >     4
      *
               IF  RECE82-SHOKUJIKHNNISSU(ID1)   =   ZERO
                   CONTINUE
               ELSE
                   EVALUATE   ID1
                     WHEN      1
                       MOVE   "£±"    TO   RECE82-SHOKUJISYUBETU(1)
                     WHEN      2
                       MOVE   "£≤"    TO   RECE82-SHOKUJISYUBETU(1)
                     WHEN      3
                       MOVE   "∆√ Ã"  TO   RECE82-SHOKUJISYUBETU(2)
                     WHEN      4
                       MOVE   "ø©∆≤"  TO   RECE82-SHOKUJISYUBETU(3)
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           IF    RECE82-SHOKUJIKHNNISSU(1)      =  ZERO
           AND   RECE82-SHOKUJIKHNNISSU(2)  NOT =  ZERO
               MOVE    RECE82-SHOKUJIKHNNISSU(2)
                              TO  RECE82-SHOKUJIKHNNISSU(1)
               MOVE    RECE82-SHOKUJIKHNRYO(2)
                              TO  RECE82-SHOKUJIKHNRYO(1)
      *
               MOVE    ZERO   TO  RECE82-SHOKUJIKHNNISSU(2)
                                  RECE82-SHOKUJIKHNRYO(2)
           END-IF
      *
           IF      RECE82-SHOKUJIKHNNISSU(2)      =  ZERO
             IF    RECE82-SHOKUJIKHNNISSU(3)  NOT =  ZERO
               MOVE    RECE82-SHOKUJIKHNNISSU(3)
                              TO  RECE82-SHOKUJIKHNNISSU(2)
               MOVE    RECE82-SHOKUJIKHNRYO(3)
                              TO  RECE82-SHOKUJIKHNRYO(2)
               MOVE    ZERO   TO  RECE82-SHOKUJIKHNNISSU(3)
                                  RECE82-SHOKUJIKHNRYO(3)
             END-IF
             IF    RECE82-SHOKUJIKHNNISSU(4)  NOT =  ZERO
               MOVE    RECE82-SHOKUJIKHNNISSU(4)
                              TO  RECE82-SHOKUJIKHNNISSU(3)
               MOVE    RECE82-SHOKUJIKHNRYO(4)
                              TO  RECE82-SHOKUJIKHNRYO(3)
               MOVE    ZERO   TO  RECE82-SHOKUJIKHNNISSU(4)
                                  RECE82-SHOKUJIKHNRYO(4)
             END-IF
           END-IF
      *
           .
       2602-SKJ-RAYKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ¥À‹≈¿øÙ…Ù ¨§Œ•Ø•Í•¢ΩËÕ˝° º´«Â¿’°À
      *****************************************************************
       270-KHNTEN-JI-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN7-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(7)
           END-IF
           IF        FLG-KHNTEN8-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(8)
           END-IF
           IF        FLG-KHNTEN9-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(9)
           END-IF
           .
       270-KHNTEN-JI-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *     Ò≥Á≈¿øÙ§Œº£∏≥∏∫≈¿ ¨ ‘Ω∏
      *****************************************************************
       281-GENTEN-HENSYU-SEC      SECTION.
      *
           EVALUATE   RECE82-HKNKBN
             WHEN       "1"
             WHEN       "2"
               IF   RECE82-TENSU(2)   >=  WRK-RECE82G-TENSU(2)
                 COMPUTE  RECE82-TENSU(2)   =
                          RECE82-TENSU(2)   -  WRK-RECE82G-TENSU(2)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(2)
               END-IF
               IF   RECE82-TENSU(3)   >=  WRK-RECE82G-TENSU(3)
                 COMPUTE  RECE82-TENSU(3)   =
                          RECE82-TENSU(3)   -  WRK-RECE82G-TENSU(3)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(3)
               END-IF
               IF   WRK-RECE82C-TENSU(12)  >=  WRK-RECE82G-TENSU(12)
                 COMPUTE  RECE82-TENSU(12)  =
                          RECE82-TENSU(12)  -  WRK-RECE82G-TENSU(12)
               ELSE
                 COMPUTE  RECE82-TENSU(12)  =
                          RECE82-TENSU(12)  -  WRK-RECE82C-TENSU(12)
                 COMPUTE  WRK-RECE82G-TENSU(12) =
                          WRK-RECE82G-TENSU(12) -
                          WRK-RECE82C-TENSU(12)
                 IF  WRK-RECE82C-TENSU(11)  >=   WRK-RECE82G-TENSU(12)
                   COMPUTE  RECE82-TENSU(11)  =
                            RECE82-TENSU(11)  -  WRK-RECE82G-TENSU(12)
                 ELSE
                   COMPUTE  RECE82-TENSU(11)  =
                            RECE82-TENSU(11)  -  WRK-RECE82C-TENSU(11)
                   COMPUTE  WRK-RECE82G-TENSU(12) =
                            WRK-RECE82G-TENSU(12) -
                            WRK-RECE82C-TENSU(11)
                   IF  WRK-RECE82C-TENSU(10)  >=   WRK-RECE82G-TENSU(12)
                     COMPUTE  RECE82-TENSU(10)  =
                              RECE82-TENSU(10)  -  WRK-RECE82G-TENSU(12)
                   ELSE
                     COMPUTE  RECE82-TENSU(10)  =
                              RECE82-TENSU(10)  -  WRK-RECE82C-TENSU(10)
                   END-IF
                 END-IF
               END-IF
             WHEN       "4"
               EVALUATE  WRK-PARA-RECEKBN
                WHEN      "4"
                  IF   RECE82-TENSU(2)   >=  WRK-RECE82G-TENSU(2)
                    COMPUTE  RECE82-TENSU(2)   =
                             RECE82-TENSU(2)   -  WRK-RECE82G-TENSU(2)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(2)
                  END-IF
                  IF   RECE82-TENSU(22)  >=  WRK-RECE82G-TENSU(22)
                    COMPUTE  RECE82-TENSU(22)  =
                             RECE82-TENSU(22)  -  WRK-RECE82G-TENSU(22)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(22)
                  END-IF
                  IF   WRK-RECE82C-TENSU-YAK(13)  >=
                                              WRK-RECE82G-TENSU-YAK(13)
                    COMPUTE  RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  -
                             WRK-RECE82G-TENSU-YAK(13)
                  ELSE
                    COMPUTE  RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  -
                             WRK-RECE82C-TENSU-YAK(13)
                  END-IF
                WHEN      "5"
                  IF   RECE82-TENSU(2)   >=  WRK-RECE82G-TENSU(2)
                    COMPUTE  RECE82-TENSU(2)   =
                             RECE82-TENSU(2)   -  WRK-RECE82G-TENSU(2)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(2)
                  END-IF
                  IF   RECE82-TENSU(27)  >=  WRK-RECE82G-TENSU(27)
                    COMPUTE  RECE82-TENSU(27)  =
                             RECE82-TENSU(27)  -  WRK-RECE82G-TENSU(27)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(27)
                  END-IF
                  IF   RECE82-TENSU(28)  >=  WRK-RECE82G-TENSU(28)
                    COMPUTE  RECE82-TENSU(28)  =
                             RECE82-TENSU(28)  -  WRK-RECE82G-TENSU(28)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(28)
                  END-IF
                  MOVE   WRK-RECE82G-TENSU(13)  TO   WRK-GENTEN-YKZTEN
                  IF   WRK-RECE82C-TENSU(13)  >=  WRK-RECE82G-TENSU(13)
                    COMPUTE  RECE82-TENSU(13)  =
                             RECE82-TENSU(13)  -  WRK-RECE82G-TENSU(13)
                  ELSE
                    COMPUTE  RECE82-TENSU(13)  =
                             RECE82-TENSU(13)  -  WRK-RECE82C-TENSU(13)
                    COMPUTE  WRK-RECE82G-TENSU(13) =
                             WRK-RECE82G-TENSU(13) -
                             WRK-RECE82C-TENSU(13)
                    IF  WRK-RECE82C-TENSU(12)  >= WRK-RECE82G-TENSU(13)
                      COMPUTE  RECE82-TENSU(12)  =
                               RECE82-TENSU(12)  -
                               WRK-RECE82G-TENSU(13)
                    ELSE
                      COMPUTE  RECE82-TENSU(12)  =
                               RECE82-TENSU(12)  -
                               WRK-RECE82C-TENSU(12)
                      COMPUTE  WRK-RECE82G-TENSU(13) =
                               WRK-RECE82G-TENSU(13) -
                               WRK-RECE82C-TENSU(12)
                      IF  WRK-RECE82C-TENSU(11)  >=
                                                  WRK-RECE82G-TENSU(13)
                        COMPUTE  RECE82-TENSU(11)  =
                                 RECE82-TENSU(11)  -
                                 WRK-RECE82G-TENSU(13)
                      ELSE
                        COMPUTE  RECE82-TENSU(11)  =
                                 RECE82-TENSU(11)  -
                                 WRK-RECE82C-TENSU(11)
                      END-IF
                    END-IF
                  END-IF
               END-EVALUATE
             WHEN       "5"
               IF   RECE82-TENSU(9)   >=  WRK-RECE82G-TENSU(9)
                 COMPUTE  RECE82-TENSU(9)   =
                          RECE82-TENSU(9)   -  WRK-RECE82G-TENSU(9)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(9)
               END-IF
               IF   RECE82-TENSU(14)  >=  WRK-RECE82G-TENSU(14)
                 COMPUTE  RECE82-TENSU(14)  =
                          RECE82-TENSU(14)  -  WRK-RECE82G-TENSU(14)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(14)
               END-IF
               IF   WRK-RECE82C-TENSU(27)  >=  WRK-RECE82G-TENSU(27)
                 COMPUTE  RECE82-TENSU(27)  =
                          RECE82-TENSU(27)  -  WRK-RECE82G-TENSU(27)
               ELSE
                 COMPUTE  RECE82-TENSU(27)  =
                          RECE82-TENSU(27)  -  WRK-RECE82C-TENSU(27)
                 COMPUTE  WRK-RECE82G-TENSU(27) =
                          WRK-RECE82G-TENSU(27) -
                          WRK-RECE82C-TENSU(27)
                 IF  WRK-RECE82C-TENSU(26)  >=   WRK-RECE82G-TENSU(27)
                   COMPUTE  RECE82-TENSU(26)  =
                            RECE82-TENSU(26)  -  WRK-RECE82G-TENSU(27)
                 ELSE
                   COMPUTE  RECE82-TENSU(26)  =
                            RECE82-TENSU(26)  -  WRK-RECE82C-TENSU(26)
                   COMPUTE  WRK-RECE82G-TENSU(27) =
                            WRK-RECE82G-TENSU(27) -
                            WRK-RECE82C-TENSU(26)
                   IF  WRK-RECE82C-TENSU(25)  >=   WRK-RECE82G-TENSU(27)
                     COMPUTE  RECE82-TENSU(25)  =
                              RECE82-TENSU(25)  -  WRK-RECE82G-TENSU(27)
                   ELSE
                     COMPUTE  RECE82-TENSU(25)  =
                              RECE82-TENSU(25)  -  WRK-RECE82C-TENSU(25)
                   END-IF
                 END-IF
               END-IF
           END-EVALUATE
      *
           .
       281-GENTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •«°º•øππø∑ΩËÕ˝° £∆£∞£∞£≤°À
      *****************************************************************
       280-RECE82-REWRITE-SEC                SECTION.
      *
           IF   RECE82-HKNKBN     NOT  =   "4"
             COMPUTE  RECE82-KINGAKU2-SYO  =  RECE82-KINGAKU2-SYO
                                           +  WRK-ZAITENNASHI
           END-IF
      *
           IF   RECE82-HKNKBN     =   "4"                               
      *
             EVALUATE  WRK-PARA-RECEKBN
              WHEN      "4"
      ********° ø∑ÕÕº∞°À
      *
               COMPUTE   RECE82-TENSU(3)      =  RECE82-TENSU(1)        
                                              +  RECE82-TENSU(2)        
      *
               COMPUTE   RECE82-TENSU(10)     =  RECE82-TENSU(7)       
                                              +  RECE82-TENSU(8)       
                                              +  RECE82-TENSU(9)       
      *
               COMPUTE   RECE82-TENSU-YAK(11) =  RECE82-TENSU-YAK(4)    
                                              +  RECE82-TENSU-YAK(5)   
                                              +  RECE82-TENSU-YAK(6)   
      *
               MOVE      ZERO              TO    RECE82-KINGAKU2-SYO
                                                 RECE82-TOTALKIN2
                                                 RECE82-KINGAKU3-SYO
      *
               COMPUTE   RECE82-KINGAKU2-SYO  =  RECE82-KINGAKU2(1)
                                              +  RECE82-KINGAKU2(2)
                                              +  RECE82-KINGAKU2(3)
                                              +  RECE82-KINGAKU2(4)
                                              +  RECE82-KINGAKU2(5)
      *
               COMPUTE   WRK-KSNRATE  =  RECE82-KSNRATE  /  100
               COMPUTE   RECE82-TOTALKIN2     =
                        (RECE82-KINGAKU2-SYO  *  WRK-KSNRATE)  +  0.5
      *
               COMPUTE   RECE82-KINGAKU3-SYO  =  RECE82-KINGAKU3(1)
                                              +  RECE82-KINGAKU3(2)
                                              +  RECE82-KINGAKU3(3)
                                              +  RECE82-KINGAKU3(4)
                                              +  RECE82-KINGAKU3(5)
      *
               COMPUTE   RECE82-TOTALKIN-ETC      =
                         RECE82-KINGAKU3-SYO      +
                         RECE82-SHOKUJITOTALKIN   +
                         RECE82-SINDAN-KIN        +
                         RECE82-MEISAI-KIN
      * 
              WHEN      "5"
      ********° ΩæÕËÕÕº∞°À
      *
               MOVE      ZERO              TO    RECE82-TOTALTEN
                                                 RECE82-TOTALTEN-YAK
                                                 RECE82-KINGAKU2-SYO
                                                 RECE82-KINGAKU3-SYO
                                                 RECE82-TOTALKIN
                                                 RECE82-TOTALKIN-YAK
                                                 RECE82-TOTALKIN2
                                                 RECE82-TOTALKIN-ETC
      *
               COMPUTE   RECE82-TENSU(3)      =  RECE82-TENSU(1)
                                              +  RECE82-TENSU(2)
      *
               COMPUTE   RECE82-TENSU(10)     =  RECE82-TENSU(4)
                                              +  RECE82-TENSU(5)
                                              +  RECE82-TENSU(6)
                                              +  RECE82-TENSU(7)
                                              +  RECE82-TENSU(8)
                                              +  RECE82-TENSU(9)
      *
               COMPUTE   RECE82-TENSU(14)     =  RECE82-TENSU(11)
                                              +  RECE82-TENSU(12)
                                              +  RECE82-TENSU(13)
      *
               COMPUTE   RECE82-TENSU(17)     =  RECE82-TENSU(15)
                                              +  RECE82-TENSU(16)
      *
               COMPUTE   RECE82-TENSU(20)     =  RECE82-TENSU(18)
                                              +  RECE82-TENSU(19)
      *
               COMPUTE   RECE82-TENSU(23)     =  RECE82-TENSU(21)
                                              +  RECE82-TENSU(22)
      *
               COMPUTE   RECE82-TENSU(26)     =  RECE82-TENSU(24)
                                              +  RECE82-TENSU(25)
      *
               COMPUTE   RECE82-TENSU(29)     =  RECE82-TENSU(27)
                                              +  RECE82-TENSU(28)
      *
      ***      COMPUTE   RECE82-TOTALTEN      =  RECE82-NYUTOTALTEN
      ***                                     +  RECE82-TENSU(3)
      ***                                     +  RECE82-TENSU(10)
      ***                                     +  RECE82-TENSU(14)
      ***                                     +  RECE82-TENSU(17)
      ***                                     +  RECE82-TENSU(20)
      ***                                     +  RECE82-TENSU(23)
      ***                                     +  RECE82-TENSU(26)
      ***                                     +  RECE82-TENSU(29)
      *
               MOVE     ZERO              TO   WRK-SYUTEN-JI
               MOVE     ZERO              TO   WRK-YKZTEN-JI
               PERFORM    VARYING    ID3    FROM    1   BY   1
                           UNTIL     ID3     >     31
                  COMPUTE  WRK-SYUTEN-JI  =  WRK-SYUTEN-JI
                                          +  RECE82-SYUTEN-JI(ID3)
                  COMPUTE  WRK-YKZTEN-JI  =  WRK-YKZTEN-JI
                                          +  RECE82-YKZTEN-JI(ID3)
               END-PERFORM
               COMPUTE   RECE82-TOTALTEN  =  RECE82-NYUTOTALTEN
                                          +  WRK-SYUTEN-JI
                                          +  WRK-YKZTEN-JI
      *
               IF   TBL-SYUTEN-JI(2)   >=  WRK-RECE82G-TENSU(2)
                 COMPUTE  TBL-SYUTEN-JI(2)  =
                          TBL-SYUTEN-JI(2)  -  WRK-RECE82G-TENSU(2)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(2)
               END-IF
               IF   TBL-SYUTEN-JI(27)  >=  WRK-RECE82G-TENSU(27)
                 COMPUTE  TBL-SYUTEN-JI(27) =
                          TBL-SYUTEN-JI(27) -  WRK-RECE82G-TENSU(27)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(27)
               END-IF
               IF   TBL-SYUTEN-JI(28)  >=  WRK-RECE82G-TENSU(28)
                 COMPUTE  TBL-SYUTEN-JI(28) =
                          TBL-SYUTEN-JI(28) -  WRK-RECE82G-TENSU(28)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(28)
               END-IF
               IF   TBL-YKZTEN-JI-C(13)  >=  WRK-GENTEN-YKZTEN
                 COMPUTE  TBL-YKZTEN-JI(13) =
                          TBL-YKZTEN-JI(13) -  WRK-GENTEN-YKZTEN
               ELSE
                 COMPUTE  TBL-YKZTEN-JI(13) =
                          TBL-YKZTEN-JI(13) -  TBL-YKZTEN-JI-C(13)
                 COMPUTE  WRK-GENTEN-YKZTEN =
                          WRK-GENTEN-YKZTEN -  TBL-YKZTEN-JI-C(13)
                 IF  TBL-YKZTEN-JI-C(12)   >=  WRK-GENTEN-YKZTEN
                   COMPUTE  TBL-YKZTEN-JI(12) =
                            TBL-YKZTEN-JI(12) -  WRK-GENTEN-YKZTEN
                 ELSE
                   COMPUTE  TBL-YKZTEN-JI(12) =
                            TBL-YKZTEN-JI(12) -  TBL-YKZTEN-JI-C(12)
                   COMPUTE  WRK-GENTEN-YKZTEN =
                            WRK-GENTEN-YKZTEN -  TBL-YKZTEN-JI-C(12)
                   IF  TBL-YKZTEN-JI-C(11)  >=   WRK-GENTEN-YKZTEN
                     COMPUTE  TBL-YKZTEN-JI(11) =
                              TBL-YKZTEN-JI(11) -  WRK-GENTEN-YKZTEN
                   ELSE
                     COMPUTE  TBL-YKZTEN-JI(11) =
                              TBL-YKZTEN-JI(11) -  TBL-YKZTEN-JI-C(11)
                   END-IF
                 END-IF
               END-IF
      *
               EVALUATE   RECE82-JIBAIKBN
      *
                WHEN       "1"
      *
                  COMPUTE   WRK-KSNRATE  =  RECE82-KSNRATE  /  100
      *
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     ((WRK-SYUTEN-JI +  RECE82-NYUTOTALTEN)
                                     *  RECE82-TANKA  *  WRK-KSNRATE)
                                                      +  0.5
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     (WRK-YKZTEN-JI  *  RECE82-TANKA) +  0.5
      *
                  COMPUTE  RECE82-SONOTAKIN-JYU  =
                           RECE82-SONOTAKIN-JYU  +
                     (WRK-KINGAKU-JI *  WRK-KSNRATE)  +  0.5
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     29
                    IF   ID3  =   3  OR  10  OR  14  OR  17  OR
                                 20  OR  23  OR  26  OR  29
                       CONTINUE
                    ELSE
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                       (TBL-SYUTEN-JI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                       (TBL-YKZTEN-JI(ID3) * RECE82-TANKA)
                                                          + 0.5
                    END-IF
                  END-PERFORM
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     10
      *
                    COMPUTE  RECE82-NYUKHNKIN-JYU(ID3) =
                             RECE82-NYUKHNKIN-JYU(ID3) +
                    (RECE82-NYUKHNKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
                    COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                             RECE82-NYUTOTALKIN-JYU    +
                    (RECE82-NYUKHNKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
      *
                    COMPUTE  RECE82-NYUTOKKIN-JYU(ID3) =
                             RECE82-NYUTOKKIN-JYU(ID3) +
                    (RECE82-NYUTOKKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
                    COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                             RECE82-NYUTOTALKIN-JYU    +
                    (RECE82-NYUTOKKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
      *
                  END-PERFORM
      *
                  MOVE   RECE82-NYUKHNKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUKHNKIN2-JYU  =
                           RECE82-NYUKHNKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-TANKA * WRK-KSNRATE)
                                                       + 0.5
                  MOVE   RECE82-NYUTOKKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUTOKKIN2-JYU  =
                           RECE82-NYUTOKKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-TANKA * WRK-KSNRATE)
                                                       + 0.5
                  IF     RECE82-NYUKHNKEI2  <   ZERO
                      COMPUTE  RECE82-NYUKHNKIN2-JYU   =
                               RECE82-NYUKHNKIN2-JYU   *  -1
                  END-IF
                  IF     RECE82-NYUTOKKEI2  <   ZERO
                      COMPUTE  RECE82-NYUTOKKIN2-JYU   =
                               RECE82-NYUTOKKIN2-JYU   *  -1
                  END-IF
      *
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUKHNKIN2-JYU
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUTOKKIN2-JYU
      *
                WHEN       "2"
      *
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     ((WRK-SYUTEN-JI +  RECE82-NYUTOTALTEN)
                                     *  RECE82-SYUTENTANKA )
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     ( WRK-YKZTEN-JI *  RECE82-YKZTENTANKA )
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     29
                    IF   ID3  =   3  OR  10  OR  14  OR  17  OR
                                 20  OR  23  OR  26  OR  29
                       CONTINUE
                    ELSE
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                         (TBL-SYUTEN-JI(ID3) * RECE82-SYUTENTANKA)
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                         (TBL-YKZTEN-JI(ID3) * RECE82-YKZTENTANKA)
                    END-IF
                  END-PERFORM
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     10
      *
                     COMPUTE  RECE82-NYUKHNKIN-JYU(ID3) =
                              RECE82-NYUKHNKIN-JYU(ID3) +
                       (RECE82-NYUKHNKEI(ID3) * RECE82-SYUTENTANKA)
                     COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                              RECE82-NYUTOTALKIN-JYU    +
                       (RECE82-NYUKHNKEI(ID3) * RECE82-SYUTENTANKA)
      *
                     COMPUTE  RECE82-NYUTOKKIN-JYU(ID3) =
                              RECE82-NYUTOKKIN-JYU(ID3) +
                       (RECE82-NYUTOKKEI(ID3) * RECE82-SYUTENTANKA)
                     COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                              RECE82-NYUTOTALKIN-JYU    +
                       (RECE82-NYUTOKKEI(ID3) * RECE82-SYUTENTANKA)
      *
                  END-PERFORM
      *
                  MOVE   RECE82-NYUKHNKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUKHNKIN2-JYU  =
                           RECE82-NYUKHNKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-SYUTENTANKA)
                  MOVE   RECE82-NYUTOKKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUTOKKIN2-JYU  =
                           RECE82-NYUTOKKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-SYUTENTANKA)
                  IF     RECE82-NYUKHNKEI2  <   ZERO
                      COMPUTE  RECE82-NYUKHNKIN2-JYU   =
                               RECE82-NYUKHNKIN2-JYU   *  -1
                  END-IF
                  IF     RECE82-NYUTOKKEI2  <   ZERO
                      COMPUTE  RECE82-NYUTOKKIN2-JYU   =
                               RECE82-NYUTOKKIN2-JYU   *  -1
                  END-IF
      *
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUKHNKIN2-JYU
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUTOKKIN2-JYU
      *
               END-EVALUATE
      *
               COMPUTE   RECE82-KINGAKU-JYU(3)  =
                         RECE82-KINGAKU-JYU(1)  +
                         RECE82-KINGAKU-JYU(2)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(10)  =
                         RECE82-KINGAKU-JYU(4)   +
                         RECE82-KINGAKU-JYU(5)   +
                         RECE82-KINGAKU-JYU(6)   +
                         RECE82-KINGAKU-JYU(7)   +
                         RECE82-KINGAKU-JYU(8)   +
                         RECE82-KINGAKU-JYU(9)    
      *
               COMPUTE   RECE82-KINGAKU-JYU(14)  =
                         RECE82-KINGAKU-JYU(11)  +
                         RECE82-KINGAKU-JYU(12)  +
                         RECE82-KINGAKU-JYU(13)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(17)  =
                         RECE82-KINGAKU-JYU(15)  +
                         RECE82-KINGAKU-JYU(16)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(20)  =
                         RECE82-KINGAKU-JYU(18)  +
                         RECE82-KINGAKU-JYU(19)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(23)  =
                         RECE82-KINGAKU-JYU(21)  +
                         RECE82-KINGAKU-JYU(22)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(26)  =
                         RECE82-KINGAKU-JYU(24)  +
                         RECE82-KINGAKU-JYU(25)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(29)  =
                         RECE82-KINGAKU-JYU(27)  +
                         RECE82-KINGAKU-JYU(28)   
      *
             END-EVALUATE
      *
           END-IF                                                       
      *
           REWRITE   RECE82-REC
      *
           IF      STS-RECE82          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    4                   TO  WRK-ERR-FLG
               MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT3
      *
           .
       280-RECE82-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    •«°º•øΩ–ŒœΩËÕ˝° £∆£∞£∞£¥°À
      *****************************************************************
       900-RECE84-WRITE-SEC                SECTION.
      *
           MOVE     RECE84-REC      TO  RECE8A-REC
           MOVE     RECE84-REC      TO  RECE84C-REC
      *
           COMPUTE  RECE84-ZAITEN      =  RECE84-ZAITEN     -  
                                          WRK-TEN
           COMPUTE  RECE84-MEISAISU    =  RECE84-MEISAISU   -
                                          WRK-MEISAI-CNT
           COMPUTE  RECE84-SYUTEN1     =  RECE84-SYUTEN1    -
                                          WRK-TEN
           COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL -
                                          WRK-SRYCDTOTAL
           COMPUTE  RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL -
                                          WRK-SURYOUTOTAL     
      *
           MOVE     WRK-TEN         TO  RECE8A-ZAITEN
           MOVE     WRK-MEISAI-CNT  TO  RECE8A-MEISAISU
           MOVE     WRK-TEN         TO  RECE8A-SYUTEN1  
           MOVE     WRK-SRYCDTOTAL  TO  RECE8A-SRYCDTOTAL
           MOVE     WRK-SURYOUTOTAL TO  RECE8A-SURYOUTOTAL
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *      ø«Œ≈≤Ò∑◊•ﬁ•π•ø…’≤√æ Û¿ﬂƒÍ
             PERFORM   920-SRYACCTPLUS-HENSYU-SEC
           END-IF
      *
           WRITE   RECE84-REC
      *
           IF      STS-RECE84          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE84PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE84          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                TO  CNT-OUT1
      *
           WRITE   RECE8A-REC
      *
           IF      STS-RECE8A          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE8APARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE8A          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                TO  CNT-OUT4
      *
           IF  (RECE82-SRYYM        <   201307      )
           OR  (RECE82-PTNUM        =   SPACE       )
           OR  (RECE82-TSTPTNUMKBN  =   "1"         )
           OR  (RECE82-RECESYUBETU  =   ZERO OR 7061)
           OR  (RECE82-RSIIRYOKBN   =   "0"         )
             CONTINUE
           ELSE
      *
             COMPUTE  RECE84C-ZAI-TENSU  =  RECE84C-ZAITEN
                                         -  WRK-TEN
             MOVE  WRK-ZAI-KINGAKU  TO  RECE84C-ZAI-KINGAKU
      *
             COMPUTE  RECE84C-ZAITEN     =  RECE84C-ZAITEN
                                         -  WRK-TEN
             COMPUTE  RECE84C-SYUTEN1    =  RECE84C-SYUTEN1
                                         -  WRK-TEN
      *
             WRITE RECE84C-REC
      *
             IF      STS-RECE84C    =   "00"
                 CONTINUE
             ELSE
                 MOVE    SPACE               TO  WRK-ERR-AREA
                 INITIALIZE                      WRK-ERR-AREA
      *
                 MOVE    3                   TO  WRK-ERR-FLG
                 MOVE    RECE84CPARA         TO  WRK-ERR-FILE-NM
                 MOVE    STS-RECE84C         TO  WRK-ERR-FILE-STS
                 PERFORM 500-FILE-ERR-ABORT-SEC
             END-IF
      *
             ADD   1                TO  CNT-OUT6
      *
           END-IF
      *
           .
       900-RECE84-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    •«°º•øΩ–ŒœΩËÕ˝° £∆£∞£∞£µ°À
      *****************************************************************
       900-RECE85-WRITE-SEC                SECTION.
      *
           MOVE    RECE85-REC          TO   RECE8B-REC      
           MOVE    RECE85-REC          TO   RECE85C-REC
      *
           WRITE   RECE85-REC
       *
           IF      STS-RECE85          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE85PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE85          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO   CNT-OUT2
      *
           WRITE   RECE8B-REC
      *
           IF      STS-RECE8B          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE8BPARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE8B          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO   CNT-OUT5
      *
           IF  (RECE82-SRYYM        <   201307      )
           OR  (RECE82-PTNUM        =   SPACE       )
           OR  (RECE82-TSTPTNUMKBN  =   "1"         )
           OR  (RECE82-RECESYUBETU  =   ZERO OR 7061)
           OR  (RECE82-RSIIRYOKBN   =   "0"         )
             CONTINUE
           ELSE
             WRITE RECE85C-REC
      *
             IF      STS-RECE85C    =   "00"
                 CONTINUE
             ELSE
                 MOVE    SPACE               TO  WRK-ERR-AREA
                 INITIALIZE                      WRK-ERR-AREA
      *
                 MOVE    3                   TO  WRK-ERR-FLG
                 MOVE    RECE85CPARA         TO  WRK-ERR-FILE-NM
                 MOVE    STS-RECE85C         TO  WRK-ERR-FILE-STS
                 PERFORM 500-FILE-ERR-ABORT-SEC
             END-IF
      *
             ADD   1                   TO   CNT-OUT7
           END-IF
      *
           .
       900-RECE85-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    •«°º•øΩ–ŒœΩËÕ˝° £∆£∞£∞£µ°À
      *****************************************************************
       900-RECE85-REWRITE-SEC              SECTION.                        
      *
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPNUM      TO  RECE85-HOSPNUM
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID     TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                    KEY IS    >=   RECE85-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
           END-START
           PERFORM   UNTIL
               (RECE84-HOSPNUM      NOT =  RECE85-HOSPNUM)     OR
               (RECE84-SRYYM        NOT =  RECE85-SRYYM)       OR
               (RECE84-NYUGAIKBN    NOT =  RECE85-NYUGAIKBN)   OR
               (RECE84-PTID         NOT =  RECE85-PTID)        OR
               (RECE84-RECEKA       NOT =  RECE85-RECEKA)      OR
               (RECE84-HKNID        NOT =  RECE85-HKNID)    OR
               (RECE84-TEKSTYMD     NOT =  RECE85-TEKSTYMD)    OR
               (RECE84-ZAINUM       NOT =  RECE85-ZAINUM)      OR
               (FLG-RECE85-ZAN            =      1        )    OR
               (FLG-REWRITE               =      1        )
                   IF    RECE85-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE85-SRYCD(IDXB)  NOT = SPACE
                          MOVE WRK-SRYSURYO3 TO RECE85-SRYSURYO(IDXB)
                          MOVE 1             TO IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE85-REC
      *
                      IF      STS-RECE85          =   "00"
                          CONTINUE
                      ELSE
                          MOVE    SPACE               TO  WRK-ERR-AREA
                          INITIALIZE                      WRK-ERR-AREA
      *
                          MOVE    4                   TO  WRK-ERR-FLG
                          MOVE    RECE85PARA          TO
                                                      WRK-ERR-FILE-NM
                          MOVE    STS-RECE85          TO
                                                      WRK-ERR-FILE-STS
                          PERFORM 500-FILE-ERR-ABORT-SEC
                      END-IF
      *
                   END-IF
                   READ    RECE85-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
           END-PERFORM
      *
      *****
      *
           IF  (RECE82-SRYYM        <   201307      )
           OR  (RECE82-PTNUM        =   SPACE       )
           OR  (RECE82-TSTPTNUMKBN  =   "1"         )
           OR  (RECE82-RECESYUBETU  =   ZERO OR 7061)
           OR  (RECE82-RSIIRYOKBN   =   "0"         )
             CONTINUE
           ELSE
      *
             MOVE    ZERO                TO  FLG-REWRITE
      *
             INITIALIZE                      RECE85C-REC
             MOVE    RECE84-HOSPNUM      TO  RECE85C-HOSPNUM
             MOVE    RECE84-SRYYM        TO  RECE85C-SRYYM
             MOVE    RECE84-NYUGAIKBN    TO  RECE85C-NYUGAIKBN
             MOVE    RECE84-PTID         TO  RECE85C-PTID
             MOVE    RECE84-RECEKA       TO  RECE85C-RECEKA
             MOVE    RECE84-HKNID        TO  RECE85C-HKNID
             MOVE    RECE84-TEKSTYMD     TO  RECE85C-TEKSTYMD
             MOVE    RECE84-ZAINUM       TO  RECE85C-ZAINUM
             START   RECE85C-FILE
                    KEY IS    >=   RECE85C-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85C-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
             END-START
             PERFORM   UNTIL
               (RECE84-HOSPNUM      NOT =  RECE85C-HOSPNUM  ) OR
               (RECE84-SRYYM        NOT =  RECE85C-SRYYM    ) OR
               (RECE84-NYUGAIKBN    NOT =  RECE85C-NYUGAIKBN) OR
               (RECE84-PTID         NOT =  RECE85C-PTID     ) OR
               (RECE84-RECEKA       NOT =  RECE85C-RECEKA   ) OR
               (RECE84-HKNID        NOT =  RECE85C-HKNID    ) OR
               (RECE84-TEKSTYMD     NOT =  RECE85C-TEKSTYMD ) OR
               (RECE84-ZAINUM       NOT =  RECE85C-ZAINUM   ) OR
               (FLG-RECE85-ZAN          =      1            ) OR
               (FLG-REWRITE             =      1            )
                   IF    RECE85C-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE85C-SRYCD(IDXB)  NOT = SPACE
                          MOVE WRK-SRYSURYO3 TO RECE85C-SRYSURYO(IDXB)
                          MOVE 1             TO IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE85C-REC
      *
                      IF      STS-RECE85C    =   "00"
                        CONTINUE
                      ELSE
                        MOVE    SPACE        TO  WRK-ERR-AREA
                        INITIALIZE               WRK-ERR-AREA
      *
                        MOVE    4            TO  WRK-ERR-FLG
                        MOVE    RECE85CPARA  TO  WRK-ERR-FILE-NM
                        MOVE    STS-RECE85C  TO  WRK-ERR-FILE-STS
                        PERFORM 500-FILE-ERR-ABORT-SEC
                      END-IF
                   END-IF
                   READ    RECE85C-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
             END-PERFORM
      *
           END-IF
      *
           .
       900-RECE85-REWRITE-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    •«°º•øΩ–ŒœΩËÕ˝° £∆£∞£∞£µ°À
      *****************************************************************
       900-RECE85-REWRITE2-SEC              SECTION.                        
      *REWRITE
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPNUM      TO  RECE85-HOSPNUM
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID     TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                    KEY IS    >=   RECE85-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
           END-START
           PERFORM   UNTIL
               (RECE84-HOSPNUM      NOT =  RECE85-HOSPNUM)     OR
               (RECE84-SRYYM        NOT =  RECE85-SRYYM)       OR
               (RECE84-NYUGAIKBN    NOT =  RECE85-NYUGAIKBN)   OR
               (RECE84-PTID         NOT =  RECE85-PTID)        OR
               (RECE84-RECEKA       NOT =  RECE85-RECEKA)      OR
               (RECE84-HKNID        NOT =  RECE85-HKNID)    OR
               (RECE84-TEKSTYMD     NOT =  RECE85-TEKSTYMD)    OR
               (RECE84-ZAINUM       NOT =  RECE85-ZAINUM)      OR
               (FLG-RECE85-ZAN            =      1        )    OR
               (FLG-REWRITE               =      1        )
                   IF    RECE85-RENNUM   =   WRK-RENNUM2
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE85-SRYCD(IDXB)  NOT = SPACE
                          MOVE   "1"  TO   RECE85-RECEPTFLG(IDXB)
                          MOVE    1   TO   IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE85-REC
      *
                      IF      STS-RECE85          =   "00"
                          CONTINUE
                      ELSE
                          MOVE    SPACE               TO  WRK-ERR-AREA
                          INITIALIZE                      WRK-ERR-AREA
      *
                          MOVE    4                   TO  WRK-ERR-FLG
                          MOVE    RECE85PARA          TO
                                                      WRK-ERR-FILE-NM
                          MOVE    STS-RECE85          TO
                                                      WRK-ERR-FILE-STS
                          PERFORM 500-FILE-ERR-ABORT-SEC
                      END-IF
      *
                   END-IF
                   READ    RECE85-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
           END-PERFORM
      *
      *****
      *
           IF  (RECE82-SRYYM        <   201307      )
           OR  (RECE82-PTNUM        =   SPACE       )
           OR  (RECE82-TSTPTNUMKBN  =   "1"         )
           OR  (RECE82-RECESYUBETU  =   ZERO OR 7061)
           OR  (RECE82-RSIIRYOKBN   =   "0"         )
             CONTINUE
           ELSE
      *
             MOVE    ZERO                TO  FLG-REWRITE
      *
             INITIALIZE                      RECE85C-REC
             MOVE    RECE84-HOSPNUM      TO  RECE85C-HOSPNUM
             MOVE    RECE84-SRYYM        TO  RECE85C-SRYYM
             MOVE    RECE84-NYUGAIKBN    TO  RECE85C-NYUGAIKBN
             MOVE    RECE84-PTID         TO  RECE85C-PTID
             MOVE    RECE84-RECEKA       TO  RECE85C-RECEKA
             MOVE    RECE84-HKNID        TO  RECE85C-HKNID
             MOVE    RECE84-TEKSTYMD     TO  RECE85C-TEKSTYMD
             MOVE    RECE84-ZAINUM       TO  RECE85C-ZAINUM
             START   RECE85C-FILE
                    KEY IS    >=   RECE85C-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85C-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
             END-START
             PERFORM   UNTIL
               (RECE84-HOSPNUM      NOT =  RECE85C-HOSPNUM  ) OR
               (RECE84-SRYYM        NOT =  RECE85C-SRYYM    ) OR
               (RECE84-NYUGAIKBN    NOT =  RECE85C-NYUGAIKBN) OR
               (RECE84-PTID         NOT =  RECE85C-PTID     ) OR
               (RECE84-RECEKA       NOT =  RECE85C-RECEKA   ) OR
               (RECE84-HKNID        NOT =  RECE85C-HKNID    ) OR
               (RECE84-TEKSTYMD     NOT =  RECE85C-TEKSTYMD ) OR
               (RECE84-ZAINUM       NOT =  RECE85C-ZAINUM   ) OR
               (FLG-RECE85-ZAN          =      1            ) OR
               (FLG-REWRITE             =      1            )
                   IF    RECE85C-RENNUM   =   WRK-RENNUM2
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE85C-SRYCD(IDXB)  NOT = SPACE
                          MOVE   "1"  TO   RECE85C-RECEPTFLG(IDXB)
                          MOVE    1   TO   IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE85C-REC
      *
                      IF      STS-RECE85C      =   "00"
                          CONTINUE
                      ELSE
                          MOVE    SPACE        TO  WRK-ERR-AREA
                          INITIALIZE               WRK-ERR-AREA
      *
                          MOVE    4            TO  WRK-ERR-FLG
                          MOVE    RECE85CPARA  TO  WRK-ERR-FILE-NM
                          MOVE    STS-RECE85C  TO  WRK-ERR-FILE-STS
                          PERFORM 500-FILE-ERR-ABORT-SEC
                      END-IF
                   END-IF
                   READ    RECE85C-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
             END-PERFORM
      *
           END-IF
      *
           .
       900-RECE85-REWRITE2-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    Ω™ŒªΩËÕ˝
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE82-FILE
                   RECE84-FILE
                   RECE85-FILE
                   RECE8A-FILE
                   RECE8B-FILE
                   RECE84C-FILE
                   RECE85C-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        •π•∆•√•◊¥…Õ˝Ω™ŒªΩËÕ˝
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE82-IN  :" CNT-IN   
           DISPLAY "RECE82-MOD :" CNT-OUT3  
           DISPLAY "RECE84-OUT :" CNT-OUT1 
           DISPLAY "RECE85-OUT :" CNT-OUT2  
           DISPLAY "RECE8A-OUT :" CNT-OUT4 
           DISPLAY "RECE8B-OUT :" CNT-OUT5  
           DISPLAY "RECE84C-OUT :" CNT-OUT6 
           DISPLAY "RECE85C-OUT :" CNT-OUT7  
           DISPLAY "*** ORCR0840 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    •®•È°ºΩ–ŒœΩËÕ˝
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            •∏•Á•÷¥…Õ˝Ω™ŒªΩËÕ˝
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •®•È°ºª˛Ω™ŒªΩËÕ˝
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    •’•°•§•Î•®•È°ºΩËÕ˝
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "•’•°•§•Î£œ£–£≈£Œ•®•È°º°°FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "•’•°•§•Î∆…§ﬂπ˛§ﬂ•®•È°º°°FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "•’•°•§•ÎΩÒ§≠π˛§ﬂ•®•È°º°°FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "•’•°•§•Îππø∑•®•È°º°°FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM-FILE-ID DELIMITED   BY  SIZE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0840 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM         DELIMITED   BY  SIZE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ΩËÕ˝√ÊªﬂΩËÕ˝
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "ΩËÕ˝§¨√Êªﬂ§µ§Ï§ﬁ§∑§ø"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                •∏•Á•÷¥…Õ˝Ω™ŒªΩËÕ˝
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •®•È°º•·•√•ª°º•∏ ‘Ω∏ΩËÕ˝
      *****************************************************************
       600-ERRMSG-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
      *
           MOVE    RECE82-PTNUM        TO  ERR-PTNUM
      *
           MOVE    ZERO                TO  WRK-SHO
                                           WRK-AMARI
           MOVE    1                   TO  WRK-LEN
           MOVE    SPACE               TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR
           WITH  POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN =   WRK-LEN -   1
      *
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
              STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                      "°Œ¥µº‘»÷πÊ°ß"      DELIMITED   BY  SIZE
                      ERR-PTNUM           DELIMITED   BY  SPACE
                      "°œ"                DELIMITED   BY  SIZE
              INTO    ERR-HEN-ERRMSG
              END-STRING
           ELSE
              STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                      "°Œ¥µº‘»÷πÊ:"       DELIMITED   BY  SIZE
                      ERR-PTNUM           DELIMITED   BY  SPACE
                      "°œ"                DELIMITED   BY  SIZE
              INTO    ERR-HEN-ERRMSG
              END-STRING
           END-IF
      *
           .
       600-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    √Ê¥÷•’•°•§•Î£“£≈£¡£ƒΩËÕ˝
      *****************************************************************
       900-RECE82-READ-SEC            SECTION.
      *
           READ    RECE82-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   IF      STS-RECE82          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE82 "  " PTNUM=" RECE82-PTNUM
                                      " (PTID=" RECE82-PTID ")"
      *
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       900-RECE82-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ø«Œ≈«Ø∑Ó§Œ¬–æ›§»§ §Î•∑•π¥…Õ˝§ÚºË∆¿° ∏ƒ ÃΩËÕ˝§Œ§ﬂ
      ****************************************************************
       910-SYSKANRI-HENSYU-SEC         SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
               CONTINUE
           ELSE
      *        •∑•π•∆•‡¥…Õ˝•ﬁ•π•ø° £±£∞£∞£±°À§´§Èæ√»Ò¿«√ºøÙ∂Ë ¨
      *        §ŒºË∆¿
               MOVE    SPACE      TO  WRK-TAX1001-KBN
      *
               MOVE  ZERO         TO  FLG-SET
               MOVE  RECE82-SRYYM TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"         TO  WRK-SYS-SRYYMD(7:2)
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                  MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                  IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                  AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                     MOVE     1       TO    FLG-SET
                     MOVE    SYS-1001-TAX-HASU-KBN
                                      TO    WRK-TAX1001-KBN
                  END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                  STRING
                  "∞ÂŒ≈µ°¥ÿæ Û§¨ºË∆¿§«§≠§ﬁ§ª§Û§«§∑§ø°£ø«Œ≈«Ø∑Ó°·"
                                               DELIMITED  BY  SIZE 
                       RECE82-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
                  GO  TO  910-SYSKANRI-HENSYU-EXT
               END-IF
      *
      *        ª‹¿ﬂ¥Ω‡æ Û
               MOVE    ZERO         TO  FLG-SET
               PERFORM  VARYING  SYSKAN-IDX2  FROM  1   BY  1
                         UNTIL ( SYSKAN-IDX2               >  100  )
                          OR   ( TBL-SYS10062(SYSKAN-IDX2) = SPACE )
                          OR   ( FLG-SET                   =   1   )
                   MOVE    TBL-SYS10062(SYSKAN-IDX2)  TO  SYS-1006-REC
                   IF   ( SYS-1006-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                   AND  ( WRK-SYS-SRYYMD     <=  SYS-1006-EDYUKYMD )
                       MOVE     1                  TO  FLG-SET
                   END-IF
               END-PERFORM
               IF      FLG-SET    =   ZERO
                   MOVE  ZERO                    TO  WRK-SISETU613
                   MOVE  ZERO                    TO  WRK-SISETU695
               ELSE
                   MOVE  SYS-1006-SSTKIJUN(113)  TO  WRK-SISETU613
                   MOVE  SYS-1006-SSTKIJUN(195)  TO  WRK-SISETU695
               END-IF
      *
               MOVE    ZERO         TO  FLG-SET
               PERFORM  VARYING  SYSKAN-IDX2  FROM  1   BY  1
                         UNTIL ( SYSKAN-IDX2               >  100  )
                          OR   ( TBL-SYS10067(SYSKAN-IDX2) = SPACE )
                          OR   ( FLG-SET                   =   1   )
                   MOVE    TBL-SYS10067(SYSKAN-IDX2)  TO  SYS-1006-REC
                   IF   ( SYS-1006-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                   AND  ( WRK-SYS-SRYYMD     <=  SYS-1006-EDYUKYMD )
                       MOVE     1                  TO  FLG-SET
                   END-IF
               END-PERFORM
               IF      FLG-SET    =   ZERO
                   MOVE  ZERO                    TO  WRK-SISETU3055
                   MOVE  ZERO                    TO  WRK-SISETU3056
                   MOVE  ZERO                    TO  WRK-SISETU3057
                   MOVE  ZERO                    TO  WRK-SISETU3058
                   MOVE  ZERO                    TO  WRK-SISETU3168
                   MOVE  ZERO                    TO  WRK-SISETU3169
               ELSE
                   MOVE  SYS-1006-SSTKIJUN(55)   TO  WRK-SISETU3055
                   MOVE  SYS-1006-SSTKIJUN(56)   TO  WRK-SISETU3056
                   MOVE  SYS-1006-SSTKIJUN(57)   TO  WRK-SISETU3057
                   MOVE  SYS-1006-SSTKIJUN(58)   TO  WRK-SISETU3058
                   MOVE  SYS-1006-SSTKIJUN(168)  TO  WRK-SISETU3168
                   MOVE  SYS-1006-SSTKIJUN(169)  TO  WRK-SISETU3169
               END-IF
      *
               MOVE      RECE82-SRYYM   TO  WRK-SRYYM-5000
               PERFORM   120-SYS-5000-CHK-SEC
      *
      *        ø©ªˆ°¶Ã§º¬ª‹∏∫ªªæ Û° •∑•π•∆•‡¥…Õ˝•∆°º•÷•Î§Ë§Í°À
               INITIALIZE             WRK-SHOKUJIRYOYO-OCC
                                      WRK-TOKUKANRIKASAN-OCC
      *
               MOVE  ZERO         TO  FLG-SET
               MOVE  RECE82-SRYYM TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"         TO  WRK-SYS-SRYYMD(7:2)
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS5000(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                  MOVE  TBL-SYS5000(SYSKAN-IDX)  TO  SYS-5000-REC
                  IF   ( SYS-5000-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                  AND  ( WRK-SYS-SRYYMD     <=  SYS-5000-EDYUKYMD )
                     MOVE     1       TO    FLG-SET
                     PERFORM  110-SYSKAN-HENSYU-SEC
                     IF        WRK-SYS5000-COUNT    =     ZERO
                        CONTINUE
                     ELSE
                        PERFORM  130-SYS-5000-GET-SEC
                     END-IF
                  END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                  STRING
              "∞ÂŒ≈µ°¥ÿæ Û°›∆˛±°¥À‹§¨ºË∆¿§«§≠§ﬁ§ª§Û§«§∑§ø°£ø«Œ≈«Ø∑Ó°·"
                                               DELIMITED  BY  SIZE 
                       RECE82-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
                  GO  TO  910-SYSKANRI-HENSYU-EXT
               END-IF
      *
           END-IF
      *
           .
      *
       910-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø«Œ≈≤Ò∑◊•ﬁ•π•ø…’≤√æ Û¿ﬂƒÍΩËÕ˝
      *****************************************************************
       920-SRYACCTPLUS-HENSYU-SEC     SECTION.
      *
           IF  RECE84-SRYKBN  =  "13" OR "14"
             CONTINUE
           ELSE
             GO  TO  920-SRYACCTPLUS-HENSYU-EXT
           END-IF
           IF  RECE84-ZAIREQFLG  =  "1"
             CONTINUE
           ELSE
             GO  TO  920-SRYACCTPLUS-HENSYU-EXT
           END-IF
      *
           INITIALIZE                      SRYACCTPLUS-REC
           MOVE    RECE84-HOSPNUM     TO   ACCTP-HOSPNUM
           MOVE    RECE84-NYUGAIKBN   TO   ACCTP-NYUGAIKBN
           MOVE    RECE84-PTID        TO   ACCTP-PTID
           MOVE    RECE84-SRYKA       TO   ACCTP-SRYKA
           MOVE    RECE84-SRYYM       TO   ACCTP-SRYYM
           MOVE    RECE84-ZAINUM      TO   ACCTP-ZAINUM
           MOVE    1                  TO   ACCTP-RENNUM
           MOVE    SRYACCTPLUS-REC    TO   MCPDATA-REC
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
             MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             PERFORM  900-SRYACCTPLUS-READ-SEC
             IF        FLG-SRYACCTPLUS   =   ZERO
               MOVE   ACCTP-SYUGI-TBL-G
                                      TO   RECE84-ACCTP-SYUGI-TBL-G
               MOVE   ACCTP-SANSOTEN  TO   RECE84-ACCTP-SANSOTEN
               MOVE   ACCTP-CHISOTEN  TO   RECE84-ACCTP-CHISOTEN
      *
               MOVE   ACCTP-SYUGI-TBL-G
                                      TO   RECE8A-ACCTP-SYUGI-TBL-G
               MOVE   ACCTP-SANSOTEN  TO   RECE8A-ACCTP-SANSOTEN
               MOVE   ACCTP-CHISOTEN  TO   RECE8A-ACCTP-CHISOTEN
      *
               MOVE   ACCTP-SYUGI-TBL-G
                                      TO   RECE84C-ACCTP-SYUGI-TBL-G
               MOVE   ACCTP-SANSOTEN  TO   RECE84C-ACCTP-SANSOTEN
               MOVE   ACCTP-CHISOTEN  TO   RECE84C-ACCTP-CHISOTEN
             END-IF
           END-IF
      *
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       920-SRYACCTPLUS-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø«Œ≈≤Ò∑◊∆…π˛
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                      SRYACCT-REC
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø«Œ≈π‘∞Ÿ∆…π˛
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø«Œ≈π‘∞Ÿ∆…π˛
      *****************************************************************
       900-SRYACCTPLUS-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
               MOVE    ZERO                TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                      SRYACCTPLUS-REC
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       900-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∆˛±°≤Ò∑◊•ﬁ•π•ø°º∆…π˛
      *****************************************************************
       900-NYUINACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
               INITIALIZE                      NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    ∆˛±°ø«Œ≈π‘∞Ÿ•ﬁ•π•ø°º∆…π˛
      **************************************************************
       900-NYUINACT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACT
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACT
               INITIALIZE                      NYUINACT-REC
           END-IF
      *
           .
       900-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ•ﬁ•π•ø∆…π˛
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO    FLG-TENSU
               MOVE    MCPDATA-REC   TO    TNS-TENSU-REC
           ELSE
               MOVE    1             TO    FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ•∆°º•÷•Î§Œ∏°∫˜
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
grpsys     MOVE    SPA-HOSPNUM          TO      TNS-HOSPNUM
           MOVE    WRK-SRYCD            TO      TNS-SRYCD
           MOVE    WK-TAIHI-SRYYMD      TO      TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_tensu"      TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE   MCPDATA-REC   TO      TNS-TENSU-REC
                   MOVE   ZERO          TO      FLG-TENSU
               ELSE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE    1            TO      FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                       TNS-TENSU-REC
               MOVE    1                TO      FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    º˝«º∆…π˛
      *****************************************************************
       900-SYUNOU-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYUNOU
               MOVE    MCPDATA-REC         TO  SYUNOU-REC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
               INITIALIZE                      SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *     ›∏±¡»πÁ§ª•ﬁ•π•ø∆…π˛
      *****************************************************************
       900-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1           TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    •∑•π•∆•‡¥…Õ˝•ﬁ•π•ø∆…π˛
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    •∑•π•∆•‡¥…Õ˝•ﬁ•π•ø∆…π˛
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥µº‘•≥•·•Û•»•ﬁ•π•ø∆…§ﬂπ˛§ﬂ
      *****************************************************************
       900-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTCOM-REC
                   MOVE    ZERO            TO  FLG-PTCOM
               ELSE
                   MOVE    1               TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                      PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    •∏•Á•÷¥…Õ˝£ƒ£¬¿©∏ÊΩËÕ˝
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬£”£≈£Ã£≈£√£‘ΩËÕ˝
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "£ƒ£¬∆…§ﬂπ˛§ﬂ•®•È°º°°TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0840 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬£∆£≈£‘£√£»ΩËÕ˝
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ø•Ì°º•∫ΩËÕ˝
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    •∆°º•÷•Î•¢•Ø•ª•πΩËÕ˝
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬•™°º•◊•ÛΩËÕ˝
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬•Ø•Ì°º•∫ΩËÕ˝
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬°°£√£œ£Õ£Õ£…£‘ΩËÕ˝
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £ƒ£¬°°£”£‘£¡£“£‘ΩËÕ˝
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
      *
