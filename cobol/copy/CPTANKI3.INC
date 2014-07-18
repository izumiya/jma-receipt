      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
      *         短期滞在手術等基本料３                           *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  TANKI3-AREA.
           03  TANKI3-VAL.
      *        イ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "97".
               05  FILLER          PIC X(09)   VALUE "190177610".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        イ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "98".
               05  FILLER          PIC X(09)   VALUE "190177710".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ロ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "99".
               05  FILLER          PIC X(09)   VALUE "190177810".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ロ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9A".
               05  FILLER          PIC X(09)   VALUE "190177910".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ハ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9B".
               05  FILLER          PIC X(09)   VALUE "190178010".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ハ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9C".
               05  FILLER          PIC X(09)   VALUE "190178110".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ニ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9D".
               05  FILLER          PIC X(09)   VALUE "190178210".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ニ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9E".
               05  FILLER          PIC X(09)   VALUE "190178310".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ホ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9F".
               05  FILLER          PIC X(09)   VALUE "190178410".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ホ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9G".
               05  FILLER          PIC X(09)   VALUE "190178510".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ヘ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9H".
               05  FILLER          PIC X(09)   VALUE "190178610".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ヘ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9I".
               05  FILLER          PIC X(09)   VALUE "190178710".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ト
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9J".
               05  FILLER          PIC X(09)   VALUE "190178810".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ト（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9K".
               05  FILLER          PIC X(09)   VALUE "190178910".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        チ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9L".
               05  FILLER          PIC X(09)   VALUE "190179010".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        チ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9M".
               05  FILLER          PIC X(09)   VALUE "190179110".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        リ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9N".
               05  FILLER          PIC X(09)   VALUE "190179210".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        リ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9O".
               05  FILLER          PIC X(09)   VALUE "190179310".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        リ（水晶体嚢拡張リングを用いた場合　縫着なし）
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "AW".
               05  FILLER          PIC X(09)   VALUE "190182750".
               05  FILLER          PIC X(08)   VALUE "20140701".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        リ（水晶体嚢拡張リングを用いた場合　縫着なし）（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AX".
               05  FILLER          PIC X(09)   VALUE "190182850".
               05  FILLER          PIC X(08)   VALUE "20140701".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ヌ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9P".
               05  FILLER          PIC X(09)   VALUE "190179410".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ヌ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9Q".
               05  FILLER          PIC X(09)   VALUE "190179510".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ル
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9R".
               05  FILLER          PIC X(09)   VALUE "190179610".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ル（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9S".
               05  FILLER          PIC X(09)   VALUE "190179710".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ヲ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9T".
               05  FILLER          PIC X(09)   VALUE "190179810".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ヲ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9U".
               05  FILLER          PIC X(09)   VALUE "190179910".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ワ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9V".
               05  FILLER          PIC X(09)   VALUE "190180010".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ワ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9W".
               05  FILLER          PIC X(09)   VALUE "190180110".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        カ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9X".
               05  FILLER          PIC X(09)   VALUE "190180210".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        カ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9Y".
               05  FILLER          PIC X(09)   VALUE "190180310".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ヨ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9Z".
               05  FILLER          PIC X(09)   VALUE "190180410".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ヨ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "A0".
               05  FILLER          PIC X(09)   VALUE "190180510".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        タ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "A1".
               05  FILLER          PIC X(09)   VALUE "190180610".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        タ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "A2".
               05  FILLER          PIC X(09)   VALUE "190180710".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        レ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "A3".
               05  FILLER          PIC X(09)   VALUE "190180810".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        レ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "A4".
               05  FILLER          PIC X(09)   VALUE "190180910".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ソ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "A5".
               05  FILLER          PIC X(09)   VALUE "190181010".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ソ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "A6".
               05  FILLER          PIC X(09)   VALUE "190181110".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ツ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "A7".
               05  FILLER          PIC X(09)   VALUE "190181210".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ツ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "A8".
               05  FILLER          PIC X(09)   VALUE "190181310".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ネ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "A9".
               05  FILLER          PIC X(09)   VALUE "190181410".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ネ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AA".
               05  FILLER          PIC X(09)   VALUE "190181510".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ナ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "AB".
               05  FILLER          PIC X(09)   VALUE "190181610".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ナ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AC".
               05  FILLER          PIC X(09)   VALUE "190181710".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ラ
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "AD".
               05  FILLER          PIC X(09)   VALUE "190181810".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ラ（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AE".
               05  FILLER          PIC X(09)   VALUE "190181910".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ム
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "AF".
               05  FILLER          PIC X(09)   VALUE "190182010".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        ム（生活療養）
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AG".
               05  FILLER          PIC X(09)   VALUE "190182110".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
           03  TANKI3-R  REDEFINES TANKI3-VAL.
               05  TANKI3-OCC      OCCURS      48.
                   07  TANKI3-LIFEKBN          PIC X(01).
                   07  TANKI3-TOKCD            PIC X(02).
                   07  TANKI3-TANKICD          PIC X(09).
                   07  TANKI3-YUKOSTYMD        PIC X(08).
                   07  TANKI3-YUKOEDYMD        PIC X(08).
       01  TANKI3-MAX                          PIC 9(03)   VALUE 48.
