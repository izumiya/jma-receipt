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
      *         点数マスタ更新用ファイル    COPY (CPTENSUUWK.INC)*
      *                       RECORD LEN  658 BYTE               *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/03/18  後発医薬品区分、
      *                                     基準不適合逓減区分
      *                                     基準不適合逓減施設 追加
      * 01.00.02     NACL-多々納  04/12/20  検索点数キー追加
      * 01.00.03     NACL-伊藤    06/03/15  ドナー分集計区分 追加
      * 01.00.04     NACL-伊藤    08/03/07  画像手術支援加算
      *                                     医療観察法対象区分
      *                                     麻酔識別区分 追加
      * 04.06.00     NACL-多々納  10/10/29  施設基準　４桁対応
      ************************************************************
       01  TNSWK-TENSU-REC.
      *(1)
           03  TNSWK-SYORIKBN            PIC X(01).
           03  TNSWK-KEY.
               05  TNSWK-SRYCD           PIC  X(09).
               05  TNSWK-YUKOSTYMD       PIC  X(08).
               05  TNSWK-YUKOEDYMD       PIC  X(08).
           03  TNSWK-YUKOEDYMD2          PIC  X(08).
           03  TNSWK-SRYKBN              PIC  X(02).
           03  TNSWK-SRYSYUKBN           PIC  X(03).
           03  TNSWK-YUKOKETA            PIC  9(02).
           03  TNSWK-NAME                PIC  X(200).
           03  TNSWK-KANAYUKOKETA        PIC  9(02).
           03  TNSWK-KANANAME            PIC  X(200).
           03  TNSWK-FORMALYUKOKETA      PIC  9(03).
           03  TNSWK-FORMALNAME          PIC  X(400).
           03  TNSWK-TENSIKIBETU         PIC  9(01).
           03  TNSWK-TEN                 PIC  9(09)V99.
           03  TNSWK-TANICD              PIC  X(03).
           03  TNSWK-TANIMOJISU          PIC  9(01).
           03  TNSWK-TANINAME            PIC  X(24).
           03  TNSWK-DATAKBN             PIC  9(01).
           03  TNSWK-HKNTEKKBN           PIC  9(01).
           03  TNSWK-NYUGAITEKKBN        PIC  9(01).
      *(20)
           03  TNSWK-ROUTEKKBN           PIC  9(01).
           03  TNSWK-HOSPSRYKBN          PIC  9(01).
           03  TNSWK-OSINKBN             PIC  9(01).
           03  TNSWK-HOUKNSKBN           PIC  9(02).
           03  TNSWK-BYOKANRENKBN        PIC  9(02).
           03  TNSWK-SDOKNRYO            PIC  9(02).
           03  TNSWK-JITUDAY             PIC  9(01).
           03  TNSWK-DAYCNT              PIC  9(01).
           03  TNSWK-YKZKNRKBN           PIC  9(01).
           03  TNSWK-KZMCOMPSIKIBETU     PIC  9(01).
           03  TNSWK-KZMKGN              PIC  9(08).
           03  TNSWK-KZMJGN              PIC  9(08).
           03  TNSWK-KZM                 PIC  9(08).
           03  TNSWK-KZMTEN              PIC  9(07)V99.
           03  TNSWK-KZMERR              PIC  9(01).
           03  TNSWK-JGNCNT              PIC  9(03).
           03  TNSWK-JGNCNT1D            PIC  9(03).
           03  TNSWK-JGNCNT1W            PIC  9(03).
           03  TNSWK-JGNCNTETCM          PIC  9(02).
           03  TNSWK-JGNCNTETC           PIC  9(03).
      *(40)
           03  TNSWK-JGNCNTERR           PIC  9(01).
           03  TNSWK-CHUKSNCD            PIC  X(04).
           03  TNSWK-CHUKSNTUBAN         PIC  X(01).
           03  TNSWK-TSUSOKUAGE          PIC  9(01).
           03  TNSWK-KGNAGE              PIC  X(02).
           03  TNSWK-JGNAGE              PIC  X(02).
           03  TNSWK-TIMEKSNKBN          PIC  9(01).
      *    基準不適合逓減区分
           03  TNSWK-KIJUNTEIGENKBN      PIC  9(01).
      *    基準不適合逓減対象施設基準コード
           03  TNSWK-KIJUNTEIGENCD       PIC  9(04).
           03  TNSWK-LASERKBN            PIC  9(01).
           03  TNSWK-CHPMESUKSN          PIC  9(01).
           03  TNSWK-MICMESUKSN          PIC  9(01).
           03  TNSWK-DONORKBN            PIC  9(01).
           03  TNSWK-KNSJISKBN           PIC  9(01).
           03  TNSWK-KNSJISGRPKBN        PIC  9(02).
           03  TNSWK-TEIGENKBN           PIC  9(01).
           03  TNSWK-SEKIZUISOKUTEI      PIC  9(01).
           03  TNSWK-KEIBUJYUTU          PIC  9(01).
           03  TNSWK-AUTOHOUGO           PIC  9(01).
           03  TNSWK-GAIKANRIKBN         PIC  9(01).
           03  TNSWK-TUSOKUGAIKBN        PIC  9(01).
           03  TNSWK-HOUKATUTEIGENKBN    PIC  9(03).
           03  TNSWK-CHPNAISIKSN         PIC  9(01).
      *(60)
           03  TNSWK-AUTOFUNGO           PIC  9(01).
           03  TNSWK-CHIIKIKSN           PIC  9(01).
           03  TNSWK-BYOSYOKBN           PIC  9(01).
           03  TNSWK-CHPGYOKOSOTIKSN     PIC  9(01).
           03  TNSWK-SHORTSTAYOPE        PIC  9(01).
           03  TNSWK-BUIKBN              PIC  9(01).
           03  TNSWK-SANTEIRRKKBN        PIC  9(01).
           03  TNSWK-SSTKIJUNCDTBL       OCCURS 10.
               05  TNSWK-SSTKIJUNCD      PIC  9(04).
           03  TNSWK-AGEKSNTBL           OCCURS 4.
               05  TNSWK-AGEKSNKGN       PIC  X(02).
               05  TNSWK-AGEKSNJGN       PIC  X(02).
               05  TNSWK-AGEKSNSRYCD     PIC  X(09).
           03  TNSWK-KENTAICOMMENT       PIC  9(01).
      *(90)
           03  TNSWK-NYUKHNKBN           PIC  9(02).
           03  TNSWK-NYUKHNKSNKBN        PIC  9(03).
           03  TNSWK-KANGOKSN            PIC  9(02).
           03  TNSWK-GAZOOPESUP          PIC  9(01).
           03  TNSWK-IRYOKANSATUKBN      PIC  9(01).
           03  TNSWK-MASUISKBKBN         PIC  9(01).
           03  TNSWK-OLDTENSKB           PIC  9(01).
           03  TNSWK-OLDTEN              PIC  9(09)V99.
           03  TNSWK-MADOKUKBN           PIC  9(01).
           03  TNSWK-SINKEIHAKAIKBN      PIC  9(01).
           03  TNSWK-SEIBUTUGAKUKBN      PIC  9(01).
           03  TNSWK-ZOEIZAIKBN          PIC  9(01).
           03  TNSWK-CSYYOURYO           PIC  9(05).
      *(100)
           03  TNSWK-YKZKBN              PIC  9(01).
           03  TNSWK-ZAIGATAKBN          PIC  9(01).
      *    後発医薬品区分
           03  TNSWK-KOUHATUKBN          PIC  9(01).
           03  TNSWK-LONGTOYOKBN         PIC  9(01).
           03  TNSWK-MEIUSESKB           PIC  9(01).
           03  TNSWK-TOKUKIZAIAGEKSNKBN  PIC  9(01).
           03  TNSWK-SANSOKBN            PIC  9(01).
           03  TNSWK-TOKUKIZAISBT1       PIC  9(01).
           03  TNSWK-TOKUKIZAISBT2       PIC  9(01).
           03  TNSWK-JGNTEN              PIC  9(07).
           03  TNSWK-GAITENTTLKBN        PIC  9(03).
           03  TNSWK-NYUTENTTLKBN        PIC  9(03).
           03  TNSWK-CDKBN-KBN           PIC  X(01).
           03  TNSWK-CDKBN-SYO           PIC  9(01).
           03  TNSWK-CDKBN-BU            PIC  9(02).
           03  TNSWK-CDKBN-KBNNUM        PIC  9(03).
           03  TNSWK-CDKBN-KBNNUM-EDA    PIC  9(02).
           03  TNSWK-CDKBN-KOUBAN        PIC  9(02).
           03  TNSWK-KOKUCHI-KBN         PIC  X(01).
           03  TNSWK-KOKUCHI-SYO         PIC  9(01).
           03  TNSWK-KOKUCHI-BU          PIC  9(02).
      *(120)
           03  TNSWK-KOKUCHI-KBNNUM      PIC  9(03).
           03  TNSWK-KOKUCHI-KBNNUM-EDA  PIC  9(02).
           03  TNSWK-KOKUCHI-KOUBAN      PIC  9(02).
           03  TNSWK-KOKUJISKBKBN1       PIC  9(01).
           03  TNSWK-KOKUJISKBKBN2       PIC  9(01).
           03  TNSWK-NAMECHGKBN          PIC  9(01).
           03  TNSWK-KANANAMECHGKBN      PIC  9(01).
           03  TNSWK-IDOKANREN           PIC  X(09).
           03  TNSWK-KOUHYOJYUNNUM       PIC  9(09).
           03  TNSWK-YAKKAKJNCD          PIC  X(12).
           03  TNSWK-SYUSAISKB           PIC  9(01).
           03  TNSWK-SYOMEIKANREN        PIC  X(09).
           03  TNSWK-CHGYMD              PIC  X(08).
           03  TNSWK-HAISIYMD            PIC  X(08).
           03  TNSWK-KEIKASOCHIYMD       PIC  X(08).
           03  TNSWK-TERMID              PIC  X(16).
           03  TNSWK-OPID                PIC  X(16).
           03  TNSWK-CREYMD              PIC  X(08).
           03  TNSWK-UPYMD               PIC  X(08).
           03  TNSWK-UPHMS               PIC  X(06).
      *　　主キー更新用項目
           03  TNSWK-UP-KEY.
               05  TNSWK-UP-HOSPNUM       PIC  9(02).
               05  TNSWK-UP-SRYCD         PIC  X(09).
               05  TNSWK-UP-YUKOSTYMD     PIC  X(08).
               05  TNSWK-UP-YUKOEDYMD     PIC  X(08).
      *
           03  TNSWK-STSRYKBN          PIC X(02).
           03  TNSWK-EDSRYKBN          PIC X(02).
           03  TNSXX-TK-CDKBN-KBNNUM   PIC  9(03).
           03  TNSXX-TK-CDKBN-KOUBAN   PIC  9(02).
           03  TNSXX-TK-TEN            PIC 9(09).
           03  TNSXX-TK-EDTEN          PIC 9(09).
