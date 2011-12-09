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
        02  S03N.
              03  S03N-B03-LABEL    PIC X(10).
              03  S03N-B05-LABEL    PIC X(10).
              03  S03N-B12-STATE PIC S9(9)   BINARY.
              03  S03N-B12S-STATE PIC S9(9)  BINARY.
              03  S03N-LBLGEMNEN-G.
                04  S03N-LBLGENMEN-STYLE
                                         PIC X(20).
                04  S03N-LBLGENMEN       PIC X(30).
              03  S03N-PTNUM     PIC X(20).
              03  S03N-SRYYMD    PIC X(9).
              03  S03N-KANANAME  PIC X(50).
              03  S03N-NAME      PIC X(50).
              03  S03N-SEX       PIC X(4).
              03  S03N-BIRTHDAY  PIC X(9).
              03  S03N-HKNCOMBI  PIC X(50).
              03  S03N-RATE      PIC X(6).
              03  S03N-SRYKA     PIC X(10).
              03  S03N-NYUGAIKBN PIC X(6).
              03  S03N-NAIYO-COMBO.
                04  S03N-NAIYO PIC X(12).
                04  S03N-NAIYO-STATE   PIC S9(9)   BINARY.
                04  S03N-NAIYO-COUNT   PIC S9(9)   BINARY.
                04  S03N-NAIYOLST      PIC X(12) OCCURS 5 TIMES.
                04  S03N-NAIYOLST-STATE   PIC S9(9)   BINARY.
              03  S03N-DENPNUM   PIC X(10).
              03  S03N-DENPJTI   PIC X(10).
              03  S03N-HAKYMD    PIC X(9).
              03  S03N-SSUHKNTEN PIC S9(10).
              03  S03N-SDOHKNTEN PIC S9(10).
              03  S03N-ZTKHKNTEN PIC S9(10).
              03  S03N-TYKHKNTEN PIC S9(10).
              03  S03N-CSYHKNTEN PIC S9(10).
              03  S03N-SYCHKNTEN PIC S9(10).
              03  S03N-SJTHKNTEN PIC S9(10).
              03  S03N-MSIHKNTEN PIC S9(10).
              03  S03N-KNSHKNTEN PIC S9(10).
              03  S03N-GZUHKNTEN PIC S9(10).
              03  S03N-ETCHKNTEN PIC S9(10).
              03  S03N-SSNHKNTEN PIC S9(10).
              03  S03N-HOUHKNTEN PIC S9(10).
              03  S03N-BYRHKNTEN PIC S9(10).
              03  S03N-NYUHKNTEN PIC S9(10).
              03  S03N-RYOHKNTEN PIC S9(10).
              03  S03N-TOTALHKNTEN
                                PIC S9(10).
              03  S03N-SSUTGMONEY
                                PIC S9(10).
              03  S03N-SDOTGMONEY
                                PIC S9(10).
              03  S03N-ZTKTGMONEY
                                PIC S9(10).
              03  S03N-TYKTGMONEY
                                PIC S9(10).
              03  S03N-CSYTGMONEY
                                PIC S9(10).
              03  S03N-SYCTGMONEY
                                PIC S9(10).
              03  S03N-SJTGMONEY PIC S9(10).
              03  S03N-MSITGMONEY PIC S9(10).
              03  S03N-KNSTGMONEY
                                PIC S9(10).
              03  S03N-GZUTGMONEY
                                PIC S9(10).
              03  S03N-ETCTGMONEY
                                PIC S9(10).
              03  S03N-SSNTGMONEY PIC S9(10).
              03  S03N-HOUTGMONEY PIC S9(10).
              03  S03N-BYRTGMONEY PIC S9(10).
              03  S03N-NYUTGMONEY
                                PIC S9(10).
              03  S03N-RYOTGMONEY PIC S9(10).
              03  S03N-HKNTEKMONEY
                                PIC S9(10).
              03  S03N-HKNTEKGAI PIC S9(10).
              03  S03N-JIHIMSG-G OCCURS 10.
                04  S03N-JIHIMSG PIC X(12).
              03  S03N-JIHI1     PIC S9(10).
              03  S03N-JIHI2     PIC S9(10).
              03  S03N-JIHI3     PIC S9(10).
              03  S03N-JIHI4     PIC S9(10).
              03  S03N-JIHI5     PIC S9(10).
              03  S03N-JIHI6     PIC S9(10).
              03  S03N-JIHI7     PIC S9(10).
              03  S03N-JIHI8     PIC S9(10).
              03  S03N-JIHI9     PIC S9(10).
              03  S03N-JIHI10    PIC S9(10).
              03  S03N-JIHITOTAL PIC S9(10).
              03  S03N-JIHI1TAX  PIC S9(10).
              03  S03N-JIHI2TAX  PIC S9(10).
              03  S03N-JIHI3TAX  PIC S9(10).
              03  S03N-JIHI4TAX  PIC S9(10).
              03  S03N-JIHI5TAX  PIC S9(10).
              03  S03N-JIHI6TAX  PIC S9(10).
              03  S03N-JIHI7TAX  PIC S9(10).
              03  S03N-JIHI8TAX  PIC S9(10).
              03  S03N-JIHI9TAX  PIC S9(10).
              03  S03N-JIHI10TAX PIC S9(10).
              03  S03N-JIHITAXTOTAL
                                 PIC S9(10).
              03  S03N-JIHITAX   PIC S9(10).
              03  S03N-SKJRYOYO  PIC S9(10).
              03  S03N-LIFERYOYO PIC S9(10).
              03  S03N-SKJFTN    PIC S9(10).
              03  S03N-LIFEFTN   PIC S9(10).
              03  S03N-RJNFTN    PIC S9(10).
              03  S03N-KOHFTN    PIC S9(10).
              03  S03N-FTNTOTAL  PIC S9(10).
              03  S03N-RSISHOSHIN
                                PIC S9(10).
              03  S03N-RSISAISHIN
                                PIC S9(10).
              03  S03N-RSISDO    PIC S9(10).
              03  S03N-RSIETC    PIC S9(10).
              03  S03N-RMSAGAKU  PIC S9(10).
              03  S03N-CHOSEI1   PIC S9(10).
              03  S03N-CHOSEI2   PIC S9(10).
              03  S03N-SKYMONEY  PIC S9(10).
              03  S03N-NYUKINGK1 PIC S9(10).
              03  S03N-MISYUZAN  PIC S9(10).
              03  S03N-ZENMISYU-GAI  PIC S9(10).
              03  S03N-ZENMISYU-NYU  PIC S9(10).
              03  S03N-GOKMISYU  PIC S9(10).
              03  S03N-CHOSEI1IN  PIC X(10).
              03  S03N-CHOSEI1IN-STATE
                                 PIC S9(9)   BINARY.
              03  S03N-CHOSEI2IN  PIC X(10).
              03  S03N-CHOSEI2IN-STATE
                                 PIC S9(9)   BINARY.
              03  S03N-SKYMONEYIN-STYLE  PIC X(20).
              03  S03N-SKYMONEYIN  PIC X(10).
              03  S03N-SKYMONEYIN-STATE
                                 PIC S9(9)   BINARY.
              03  S03N-NYUKINGK2 PIC X(10).
              03  S03N-NYUKINGK2-STATE
                                 PIC S9(9)   BINARY.
              03  S03N-NYUKINYMD.
                04  S03N-VALUE   PIC X(10).
              03  S03N-NYUKINYMD-STATE
                                 PIC S9(9)   BINARY.
              03  S03N-NYUKINHOUHOU-COMBO.
                04  S03N-NYUKINHOUHOU PIC X(33).
                04  S03N-NYUKINHOUHOU-STATE
                                 PIC S9(9)   BINARY.
                04  S03N-NYUKINHOUHOU-COUNT   PIC S9(9)   BINARY.
                04  S03N-NYUKINHOUHOULST      PIC X(33) OCCURS 50 TIMES.
                04  S03N-NYUKINHOUHOULST-STATE
                                 PIC S9(9)   BINARY.
              03  S03N-SKYPRT-COMBO.
                04  S03N-SKYPRT PIC X(24).
                04  S03N-SKYPRT-STATE   PIC S9(9)   BINARY.
                04  S03N-SKYPRT-COUNT   PIC S9(9)   BINARY.
                04  S03N-SKYPRTLST      PIC X(24) OCCURS 5 TIMES.
                04  S03N-SKYPRTLST-STATE   PIC S9(9)   BINARY.
              03  S03N-SRYPRT-COMBO.
                04  S03N-SRYPRT PIC X(24).
                04  S03N-SRYPRT-STATE   PIC S9(9)   BINARY.
                04  S03N-SRYPRT-COUNT   PIC S9(9)   BINARY.
                04  S03N-SRYPRTLST      PIC X(24) OCCURS 5 TIMES.
                04  S03N-SRYPRTLST-STATE   PIC S9(9)   BINARY.
              03  S03N-RIREKIIN PIC S9(2).
              03  S03N-SCROLLEDWINDOW1.
                04  S03N-CLIST1.
                  05  S03N-CLIST1-COUNT       PIC S9(9)   BINARY.
                  05  S03N-CLIST1-ROW         PIC S9(9)   BINARY.
                  05  S03N-CLIST1-ROWATTR     PIC S9(9)   BINARY.
                  05  S03N-ITEM        OCCURS  99 TIMES.
                      06  S03N-TRENNUM PIC X(4).
                      06  S03N-TSKYMONY    PIC X(12).
                      06  S03N-TNYUKINMONEY    PIC X(12).
                      06  S03N-TSYORIYMD   PIC X(11).
                      06  S03N-TJTI    PIC X(12).
                      06  S03N-TNYKN-NAME    PIC X(10).
                  05  S03N-SELECT  PIC X       OCCURS  99 TIMES.
