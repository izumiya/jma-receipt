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
      ******************************************************************
      *    ＡＰＩレスポンス室料差額更新
      *    COPY (CPHSCONFRCV2RES.INC)
      ******************************************************************
        01  XML-HSCONFRCRES.
            03  HSCONFRCRES-CONTEXT          PIC S9(9)   BINARY.
            03  HSCONFRCRES-OBJECT           PIC X(8).
            03  HSCONFRCRES-MODE             PIC S9(9)   BINARY.
            03  HSCONFRCRES-RECORDNAME       PIC X(128).
      *
            03  HSCONFRCRES-AREA.
                05  HSCONFRCRES-INFORMATION-DATE     PIC X(10).
                05  HSCONFRCRES-INFORMATION-TIME     PIC X(8).
                05  HSCONFRCRES-RESULT               PIC X(5).
                05  HSCONFRCRES-RESULT-MSG           PIC X(100).
                05  HSCONFRCRES-REQNUM               PIC X(02).
                05  HSCONFRCRES-BASEYMD              PIC X(10).
                05  HSCONFRCRES-OCC                  OCCURS   99.
                    07  HSCONFRCRES-TKBNCD           PIC X(02).
                    07  HSCONFRCRES-TSAGAKU          PIC X(6).
                    07  HSCONFRCRES-TSTYUKYMD        PIC X(10).
                    07  HSCONFRCRES-TEDYUKYMD        PIC X(10).
                    07  HSCONFRCRES-TERRIDX          PIC X(2).
                    07  HSCONFRCRES-TERRCD           PIC X(4).
                    07  HSCONFRCRES-TERRMSG          PIC X(100).
