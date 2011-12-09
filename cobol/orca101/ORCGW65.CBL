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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW65.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 公費付加情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/12/01    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
      *
       01  SPA-W65.
           03  SPA-GMN-CUR                         PIC 9(03).
           03  SPA-GMN-PAGENO                      PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-KJNYMD                  PIC X(10).
               05  SPA-GMN-INPUT-AREA.
                 06  SPA-GMN-SELNUM1               PIC X(50).
                 06  SPA-GMN-KOHNUM-G.
                   07  SPA-GMN-KOHNUM              PIC X(03).
                   07  SPA-GMN-KOHNUMF             PIC X(01).
                   07  SPA-GMN-KOHNUMMEI           PIC X(60).
                 06  SPA-GMN-YUKOSTYMD             PIC X(10).
                 06  SPA-GMN-YUKOEDYMD             PIC X(10).
                 06  SPA-GMN-SELNUM2               PIC S9(2).
                 06  SPA-GMN-NBOOK-AREA.
                   07  SPA-GMN-JNISSUKBN-G.
                       09  SPA-GMN-JNISSUKBN               PIC X(01).
                       09  SPA-GMN-JNISSUKBNF              PIC X(01).
                       09  SPA-GMN-JNISSUKBNMEI            PIC X(78).
                   07  SPA-GMN-JNISSUTAIKBN-G.
                       09  SPA-GMN-JNISSUTAIKBN            PIC X(01).
                       09  SPA-GMN-JNISSUTAIKBNF           PIC X(01).
                       09  SPA-GMN-JNISSUTAIKBNMEI         PIC X(38).
                   07  SPA-GMN-TOTALTENKBN-G.
                       09  SPA-GMN-TOTALTENKBN             PIC X(01).
                       09  SPA-GMN-TOTALTENKBNF            PIC X(01).
                       09  SPA-GMN-TOTALTENKBNMEI          PIC X(78).
                   07  SPA-GMN-TOTALTENTAIKBN-G.
                       09  SPA-GMN-TOTALTENTAIKBN          PIC X(01).
                       09  SPA-GMN-TOTALTENTAIKBNF         PIC X(01).
                       09  SPA-GMN-TOTALTENTAIKBNMEI       PIC X(38).
                   07  SPA-GMN-SYUKEIKBN-G.
                       09  SPA-GMN-SYUKEIKBN               PIC X(01).
                       09  SPA-GMN-SYUKEIKBNF              PIC X(01).
                       09  SPA-GMN-SYUKEIKBNMEI            PIC X(78).
                   07  SPA-GMN-SYUKEITAIKBN-G.
                       09  SPA-GMN-SYUKEITAIKBN            PIC X(01).
                       09  SPA-GMN-SYUKEITAIKBNF           PIC X(01).
                       09  SPA-GMN-SYUKEITAIKBNMEI         PIC X(38).
                   07  SPA-GMN-FTNKEIKBNG-G.
                       09  SPA-GMN-FTNKEIKBNG              PIC X(01).
                       09  SPA-GMN-FTNKEIKBNGF             PIC X(01).
                       09  SPA-GMN-FTNKEIKBNGMEI           PIC X(78).
                   07  SPA-GMN-FTNKEITAIKBNG-G.
                       09  SPA-GMN-FTNKEITAIKBNG           PIC X(01).
                       09  SPA-GMN-FTNKEITAIFKBNG          PIC X(01).
                       09  SPA-GMN-FTNKEITAIMEIKBNG        PIC X(38).
                   07  SPA-GMN-FTNKEIKBNN-G.
                       09  SPA-GMN-FTNKEIKBNN              PIC X(01).
                       09  SPA-GMN-FTNKEIKBNNF             PIC X(01).
                       09  SPA-GMN-FTNKEIKBNNMEI           PIC X(78).
                   07  SPA-GMN-FTNKEITAIKBNN-G.
                       09  SPA-GMN-FTNKEITAIKBNN           PIC X(01).
                       09  SPA-GMN-FTNKEITAIKBNNF          PIC X(01).
                       09  SPA-GMN-FTNKEITAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-FTNMONEYKBNG-G.
                       09  SPA-GMN-FTNMONEYKBNG            PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNGF           PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNGMEI         PIC X(78).
                   07  SPA-GMN-FTNMONEYTAIKBNG-G.
                       09  SPA-GMN-FTNMONEYTAIKBNG         PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNGF        PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNGMEI      PIC X(38).
                   07  SPA-GMN-FTNMONEYKBNN-G.
                       09  SPA-GMN-FTNMONEYKBNN            PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNNF           PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNNMEI         PIC X(78).
                   07  SPA-GMN-FTNMONEYTAIKBNN-G.
                       09  SPA-GMN-FTNMONEYTAIKBNN         PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNNF        PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNNMEI      PIC X(38).
                   07  SPA-GMN-FTNZEROKBNG-G.
                       09  SPA-GMN-FTNZEROKBNG             PIC X(01).
                       09  SPA-GMN-FTNZEROKBNGF            PIC X(01).
                       09  SPA-GMN-FTNZEROKBNGMEI          PIC X(78).
                   07  SPA-GMN-FTNZEROTAIKBNG-G.
                       09  SPA-GMN-FTNZEROTAIKBNG          PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNGF         PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNGMEI       PIC X(38).
                   07  SPA-GMN-FTNZEROKBNN-G.
                       09  SPA-GMN-FTNZEROKBNN             PIC X(01).
                       09  SPA-GMN-FTNZEROKBNNF            PIC X(01).
                       09  SPA-GMN-FTNZEROKBNNMEI          PIC X(78).
                   07  SPA-GMN-FTNZEROTAIKBNN-G.
                       09  SPA-GMN-FTNZEROTAIKBNN          PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNNF         PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNNMEI       PIC X(38).
                   07  SPA-GMN-SKJKBN-G.
                       09  SPA-GMN-SKJKBN                  PIC X(01).
                       09  SPA-GMN-SKJKBNF                 PIC X(01).
                       09  SPA-GMN-SKJKBNMEI               PIC X(78).
                   07  SPA-GMN-SKJTAIKBN-G.
                       09  SPA-GMN-SKJTAIKBN               PIC X(01).
                       09  SPA-GMN-SKJTAIKBNF              PIC X(01).
                       09  SPA-GMN-SKJTAIKBNMEI            PIC X(38).
                   07  SPA-GMN-OCRKBN-G.
                       09  SPA-GMN-OCRKBN                  PIC X(01).
                       09  SPA-GMN-OCRKBNF                 PIC X(01).
                       09  SPA-GMN-OCRKBNMEI               PIC X(78).
                   07  SPA-GMN-OCRTAIKBN-G.
                       09  SPA-GMN-OCRTAIKBN               PIC X(01).
                       09  SPA-GMN-OCRTAIKBNF              PIC X(01).
                       09  SPA-GMN-OCRTAIKBNMEI            PIC X(38).
                   07  SPA-GMN-SAIKEIKBN-G.
                       09  SPA-GMN-SAIKEIKBN               PIC X(01).
                       09  SPA-GMN-SAIKEIKBNF              PIC X(01).
                       09  SPA-GMN-SAIKEIKBNMEI            PIC X(78).
                   07  SPA-GMN-SAIKEITAIKBN-G.
                       09  SPA-GMN-SAIKEITAIKBN            PIC X(01).
                       09  SPA-GMN-SAIKEITAIKBNF           PIC X(01).
                       09  SPA-GMN-SAIKEITAIKBNMEI         PIC X(38).
                   07  SPA-GMN-HKNFTNKBNK-G.
                       09  SPA-GMN-HKNFTNKBNK              PIC X(01).
                       09  SPA-GMN-HKNFTNKBNKF             PIC X(01).
                       09  SPA-GMN-HKNFTNKBNKMEI           PIC X(78).
                   07  SPA-GMN-HKNFTNTAIKBNK-G.
                       09  SPA-GMN-HKNFTNTAIKBNK           PIC X(01).
                       09  SPA-GMN-HKNFTNTAIKBNKF          PIC X(01).
                       09  SPA-GMN-HKNFTNTAIKBNKMEI        PIC X(38).
                   07  SPA-GMN-HKNFTNKBNY-G.
                       09  SPA-GMN-HKNFTNKBNY              PIC X(01).
                       09  SPA-GMN-HKNFTNKBNYF             PIC X(01).
                       09  SPA-GMN-HKNFTNKBNYMEI           PIC X(78).
                   07  SPA-GMN-HKNFTNTAIKBNY-G.
                       09  SPA-GMN-HKNFTNTAIKBNY           PIC X(01).
                       09  SPA-GMN-HKNFTNTAIKBNYF          PIC X(01).
                       09  SPA-GMN-HKNFTNTAIKBNYMEI        PIC X(38).
                   07  SPA-GMN-RECEKSIKBNG-G.
                       09  SPA-GMN-RECEKSIKBNG             PIC X(01).
                       09  SPA-GMN-RECEKSIKBNGF            PIC X(01).
                       09  SPA-GMN-RECEKSIKBNGMEI          PIC X(78).
                   07  SPA-GMN-RECEKSITAIKBNG-G.
                       09  SPA-GMN-RECEKSITAIKBNG          PIC X(01).
                       09  SPA-GMN-RECEKSITAIKBNGF         PIC X(01).
                       09  SPA-GMN-RECEKSITAIKBNGMEI       PIC X(38).
                   07  SPA-GMN-RECEKSIKBNN-G.
                       09  SPA-GMN-RECEKSIKBNN             PIC X(01).
                       09  SPA-GMN-RECEKSIKBNNF            PIC X(01).
                       09  SPA-GMN-RECEKSIKBNNMEI          PIC X(78).
                   07  SPA-GMN-RECEKSITAIKBNN-G.
                       09  SPA-GMN-RECEKSITAIKBNN          PIC X(01).
                       09  SPA-GMN-RECEKSITAIKBNNF         PIC X(01).
                       09  SPA-GMN-RECEKSITAIKBNNMEI       PIC X(38).
                   07  SPA-GMN-CHKJGNKBNG-G.
                       09  SPA-GMN-CHKJGNKBNG              PIC X(01).
                       09  SPA-GMN-CHKJGNKBNGF             PIC X(01).
                       09  SPA-GMN-CHKJGNKBNGMEI           PIC X(78).
                   07  SPA-GMN-CHKJGNTAIKBNG-G.
                       09  SPA-GMN-CHKJGNTAIKBNG           PIC X(01).
                       09  SPA-GMN-CHKJGNTAIKBNGF          PIC X(01).
                       09  SPA-GMN-CHKJGNTAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-CHKJGNKBNN-G.
                       09  SPA-GMN-CHKJGNKBNN              PIC X(01).
                       09  SPA-GMN-CHKJGNKBNNF             PIC X(01).
                       09  SPA-GMN-CHKJGNKBNNMEI           PIC X(78).
                   07  SPA-GMN-CHKJGNTAIKBNN-G.
                       09  SPA-GMN-CHKJGNTAIKBNN           PIC X(01).
                       09  SPA-GMN-CHKJGNTAIKBNNF          PIC X(01).
                       09  SPA-GMN-CHKJGNTAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-KOHSKYKBNG-G.
                       09  SPA-GMN-KOHSKYKBNG              PIC X(01).
                       09  SPA-GMN-KOHSKYKBNGF             PIC X(01).
                       09  SPA-GMN-KOHSKYKBNGMEI           PIC X(78).
                   07  SPA-GMN-KOHSKYTAIKBNG-G.
                       09  SPA-GMN-KOHSKYTAIKBNG           PIC X(01).
                       09  SPA-GMN-KOHSKYTAIKBNGF          PIC X(01).
                       09  SPA-GMN-KOHSKYTAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-KOHSKYKBNN-G.
                       09  SPA-GMN-KOHSKYKBNN              PIC X(01).
                       09  SPA-GMN-KOHSKYKBNNF             PIC X(01).
                       09  SPA-GMN-KOHSKYKBNNMEI           PIC X(78).
                   07  SPA-GMN-KOHSKYTAIKBNN-G.
                       09  SPA-GMN-KOHSKYTAIKBNN           PIC X(01).
                       09  SPA-GMN-KOHSKYTAIKBNNF          PIC X(01).
                       09  SPA-GMN-KOHSKYTAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-JYUHENKBN-G.
                       09  SPA-GMN-JYUHENKBN               PIC X(01).
                       09  SPA-GMN-JYUHENKBNF              PIC X(01).
                       09  SPA-GMN-JYUHENKBNMEI            PIC X(78).
                   07  SPA-GMN-JYUHENTAIKBN-G.
                       09  SPA-GMN-JYUHENTAIKBN            PIC X(01).
                       09  SPA-GMN-JYUHENTAIKBNF           PIC X(01).
                       09  SPA-GMN-JYUHENTAIKBNMEI         PIC X(38).
                   07  SPA-GMN-JGNFTNKBN-G.
                       09  SPA-GMN-JGNFTNKBN               PIC X(01).
                       09  SPA-GMN-JGNFTNKBNF              PIC X(01).
                       09  SPA-GMN-JGNFTNKBNMEI            PIC X(78).
                   07  SPA-GMN-JGNFTNTAIKBN-G.
                       09  SPA-GMN-JGNFTNTAIKBN            PIC X(01).
                       09  SPA-GMN-JGNFTNTAIKBNF           PIC X(01).
                       09  SPA-GMN-JGNFTNTAIKBNMEI         PIC X(38).
                   07  SPA-GMN-PTJGNKBN-G.
                       09  SPA-GMN-PTJGNKBN                PIC X(01).
                       09  SPA-GMN-PTJGNKBNF               PIC X(01).
                       09  SPA-GMN-PTJGNKBNMEI             PIC X(78).
                   07  SPA-GMN-PTJGNTAIKBN-G.
                       09  SPA-GMN-PTJGNTAIKBN             PIC X(01).
                       09  SPA-GMN-PTJGNTAIKBNF            PIC X(01).
                       09  SPA-GMN-PTJGNTAIKBNMEI          PIC X(38).
                   07  SPA-GMN-IRYOKEIKBNG-G.
                       09  SPA-GMN-IRYOKEIKBNG             PIC X(01).
                       09  SPA-GMN-IRYOKEIKBNGF            PIC X(01).
                       09  SPA-GMN-IRYOKEIKBNGMEI          PIC X(78).
                   07  SPA-GMN-IRYOKEITAIKBNG-G.
                       09  SPA-GMN-IRYOKEITAIKBNG          PIC X(01).
                       09  SPA-GMN-IRYOKEITAIKBNGF         PIC X(01).
                       09  SPA-GMN-IRYOKEITAIKBNGMEI       PIC X(38).
                   07  SPA-GMN-IRYOKEIKBNN-G.
                       09  SPA-GMN-IRYOKEIKBNN             PIC X(01).
                       09  SPA-GMN-IRYOKEIKBNNF            PIC X(01).
                       09  SPA-GMN-IRYOKEIKBNNMEI          PIC X(78).
                   07  SPA-GMN-IRYOKEITAIKBNN-G.
                       09  SPA-GMN-IRYOKEITAIKBNN          PIC X(01).
                       09  SPA-GMN-IRYOKEITAIKBNNF         PIC X(01).
                       09  SPA-GMN-IRYOKEITAIKBNNMEI       PIC X(38).
                   07  SPA-GMN-SKJKEIKBN-G.
                       09  SPA-GMN-SKJKEIKBN               PIC X(01).
                       09  SPA-GMN-SKJKEIKBNF              PIC X(01).
                       09  SPA-GMN-SKJKEIKBNMEI            PIC X(78).
                   07  SPA-GMN-SKJKEITAIKBN-G.
                       09  SPA-GMN-SKJKEITAIKBN            PIC X(01).
                       09  SPA-GMN-SKJKEITAIKBNF           PIC X(01).
                       09  SPA-GMN-SKJKEITAIKBNMEI         PIC X(38).
                   07  SPA-GMN-HONTSYTNK-G.
                     08  SPA-GMN-SKJRYOKBNHON-G.
                       09  SPA-GMN-SKJRYOKBNHON            PIC X(01).
                       09  SPA-GMN-SKJRYOKBNHONF           PIC X(01).
                       09  SPA-GMN-SKJRYOKBNHONMEI         PIC X(78).
                     08  SPA-GMN-SKJRYOTAIKBNHON-G.
                       09  SPA-GMN-SKJRYOTAIKBNHON         PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNHONF        PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNHONMEI      PIC X(38).
                     08  SPA-GMN-SEIRYOKBNHON-G.
                       09  SPA-GMN-SEIRYOKBNHON            PIC X(01).
                       09  SPA-GMN-SEIRYOKBNHONF           PIC X(01).
                       09  SPA-GMN-SEIRYOKBNHONMEI         PIC X(78).
                     08  SPA-GMN-SEIRYOTAIKBNHON-G.
                       09  SPA-GMN-SEIRYOTAIKBNHON         PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNHONF        PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNHONMEI      PIC X(38).
                     08  SPA-GMN-SEIKANKBNHON-G.
                       09  SPA-GMN-SEIKANKBNHON            PIC X(01).
                       09  SPA-GMN-SEIKANKBNHONF           PIC X(01).
                       09  SPA-GMN-SEIKANKBNHONMEI         PIC X(78).
                     08  SPA-GMN-SEIKANTAIKBNHON-G.
                       09  SPA-GMN-SEIKANTAIKBNHON         PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNHONF        PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNHONMEI      PIC X(38).
                     08  SPA-GMN-SKJRYOKBNTSY-G.
                       09  SPA-GMN-SKJRYOKBNTSY            PIC X(01).
                       09  SPA-GMN-SKJRYOKBNTSYF           PIC X(01).
                       09  SPA-GMN-SKJRYOKBNTSYMEI         PIC X(78).
                     08  SPA-GMN-SKJRYOTAIKBNTSY-G.
                       09  SPA-GMN-SKJRYOTAIKBNTSY         PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNTSYF        PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNTSYMEI      PIC X(38).
                     08  SPA-GMN-SEIRYOKBNTSY-G.
                       09  SPA-GMN-SEIRYOKBNTSY            PIC X(01).
                       09  SPA-GMN-SEIRYOKBNTSYF           PIC X(01).
                       09  SPA-GMN-SEIRYOKBNTSYMEI         PIC X(78).
                     08  SPA-GMN-SEIRYOTAIKBNTSY-G.
                       09  SPA-GMN-SEIRYOTAIKBNTSY         PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNTSYF        PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNTSYMEI      PIC X(38).
                     08  SPA-GMN-SEIKANKBNTSY-G.
                       09  SPA-GMN-SEIKANKBNTSY            PIC X(01).
                       09  SPA-GMN-SEIKANKBNTSYF           PIC X(01).
                       09  SPA-GMN-SEIKANKBNTSYMEI         PIC X(78).
                     08  SPA-GMN-SEIKANTAIKBNTSY-G.
                       09  SPA-GMN-SEIKANTAIKBNTSY         PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNTSYF        PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNTSYMEI      PIC X(38).
                     08  SPA-GMN-SKJRYOKBNTNK-G.
                       09  SPA-GMN-SKJRYOKBNTNK            PIC X(01).
                       09  SPA-GMN-SKJRYOKBNTNKF           PIC X(01).
                       09  SPA-GMN-SKJRYOKBNTNKMEI         PIC X(78).
                     08  SPA-GMN-SKJRYOTAIKBNTNK-G.
                       09  SPA-GMN-SKJRYOTAIKBNTNK         PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNTNKF        PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNTNKMEI      PIC X(38).
                     08  SPA-GMN-SEIRYOKBNTNK-G.
                       09  SPA-GMN-SEIRYOKBNTNK            PIC X(01).
                       09  SPA-GMN-SEIRYOKBNTNKF           PIC X(01).
                       09  SPA-GMN-SEIRYOKBNTNKMEI         PIC X(78).
                     08  SPA-GMN-SEIRYOTAIKBNTNK-G.
                       09  SPA-GMN-SEIRYOTAIKBNTNK         PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNTNKF        PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNTNKMEI      PIC X(38).
                     08  SPA-GMN-SEIKANKBNTNK-G.
                       09  SPA-GMN-SEIKANKBNTNK            PIC X(01).
                       09  SPA-GMN-SEIKANKBNTNKF           PIC X(01).
                       09  SPA-GMN-SEIKANKBNTNKMEI         PIC X(78).
                     08  SPA-GMN-SEIKANTAIKBNTNK-G.
                       09  SPA-GMN-SEIKANTAIKBNTNK         PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNTNKF        PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNTNKMEI      PIC X(38).
                   07  SPA-GMN-HONTSYTNK-R    REDEFINES
                                              SPA-GMN-HONTSYTNK-G.
                     08  SPA-GMN-HONTSYTNK-OCC       OCCURS  3.
                       09  SPA-GMN-SKJRYOKBN-G.
                         11  SPA-GMN-SKJRYOKBN            PIC X(01).
                         11  SPA-GMN-SKJRYOKBNF           PIC X(01).
                         11  SPA-GMN-SKJRYOKBNMEI         PIC X(78).
                       09  SPA-GMN-SKJRYOTAIKBN-G.
                         11  SPA-GMN-SKJRYOTAIKBN         PIC X(01).
                         11  SPA-GMN-SKJRYOTAIKBNF        PIC X(01).
                         11  SPA-GMN-SKJRYOTAIKBNMEI      PIC X(38).
                       09  SPA-GMN-SEIRYOKBN-G.
                         11  SPA-GMN-SEIRYOKBN            PIC X(01).
                         11  SPA-GMN-SEIRYOKBNF           PIC X(01).
                         11  SPA-GMN-SEIRYOKBNMEI         PIC X(78).
                       09  SPA-GMN-SEIRYOTAIKBN-G.
                         11  SPA-GMN-SEIRYOTAIKBN         PIC X(01).
                         11  SPA-GMN-SEIRYOTAIKBNF        PIC X(01).
                         11  SPA-GMN-SEIRYOTAIKBNMEI      PIC X(38).
                       09  SPA-GMN-SEIKANKBN-G.
                         11  SPA-GMN-SEIKANKBN            PIC X(01).
                         11  SPA-GMN-SEIKANKBNF           PIC X(01).
                         11  SPA-GMN-SEIKANKBNMEI         PIC X(78).
                       09  SPA-GMN-SEIKANTAIKBN-G.
                         11  SPA-GMN-SEIKANTAIKBN         PIC X(01).
                         11  SPA-GMN-SEIKANTAIKBNF        PIC X(01).
                         11  SPA-GMN-SEIKANTAIKBNMEI      PIC X(38).
               05  SPA-GMN-CMB-AREA.
                   07  SPA-GMN-KOHNUMLST-G.
                       09  SPA-GMN-KOHNUM-MAX              PIC 9(03).
                       09  SPA-GMN-KOHNUMLST               OCCURS  200.
                           11  SPA-GMN-KOHNUML             PIC X(03).
                           11  SPA-GMN-KOHNUMFL            PIC X(01).
                           11  SPA-GMN-KOHNUMMEIL          PIC X(60).
                   07  SPA-GMN-JNISSUKBNLST-G.
                       09  SPA-GMN-JNISSUKBN-MAX           PIC 9(03).
                       09  SPA-GMN-JNISSUKBNLST            OCCURS  10.
                           11  SPA-GMN-JNISSUKBNL          PIC X(01).
                           11  SPA-GMN-JNISSUKBNFL         PIC X(01).
                           11  SPA-GMN-JNISSUKBNMEIL       PIC X(78).
                   07  SPA-GMN-TOTALTENKBNLST-G.
                       09  SPA-GMN-TOTALTENKBN-MAX         PIC 9(03).
                       09  SPA-GMN-TOTALTENKBNLST          OCCURS 10.
                           11  SPA-GMN-TOTALTENKBNL        PIC X(01).
                           11  SPA-GMN-TOTALTENKBNFL       PIC X(01).
                           11  SPA-GMN-TOTALTENKBNMEIL     PIC X(78).
                   07  SPA-GMN-SYUKEIKBNLST-G.
                       09  SPA-GMN-SYUKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-SYUKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-SYUKEIKBNL          PIC X(01).
                           11  SPA-GMN-SYUKEIKBNFL         PIC X(01).
                           11  SPA-GMN-SYUKEIKBNMEIL       PIC X(78).
                   07  SPA-GMN-FTNKEIKBNLST-G.
                       09  SPA-GMN-FTNKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-FTNKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-FTNKEIKBNL          PIC X(01).
                           11  SPA-GMN-FTNKEIKBNFL         PIC X(01).
                           11  SPA-GMN-FTNKEIKBNMEIL       PIC X(78).
                   07  SPA-GMN-FTNMONEYKBNLST-G.
                       09  SPA-GMN-FTNMONEYKBN-MAX         PIC 9(03).
                       09  SPA-GMN-FTNMONEYKBNLST          OCCURS 10.
                           11  SPA-GMN-FTNMONEYKBNL        PIC X(01).
                           11  SPA-GMN-FTNMONEYKBNFL       PIC X(01).
                           11  SPA-GMN-FTNMONEYKBNMEIL     PIC X(78).
                   07  SPA-GMN-FTNZEROKBNLST-G.
                       09  SPA-GMN-FTNZEROKBN-MAX          PIC 9(03).
                       09  SPA-GMN-FTNZEROKBNLST           OCCURS 10.
                           11  SPA-GMN-FTNZEROKBNL         PIC X(01).
                           11  SPA-GMN-FTNZEROKBNFL        PIC X(01).
                           11  SPA-GMN-FTNZEROKBNMEIL      PIC X(78).
                   07  SPA-GMN-SKJKBNLST-G.
                       09  SPA-GMN-SKJKBN-MAX              PIC 9(03).
                       09  SPA-GMN-SKJKBNLST               OCCURS 10.
                           11  SPA-GMN-SKJKBNL             PIC X(01).
                           11  SPA-GMN-SKJKBNFL            PIC X(01).
                           11  SPA-GMN-SKJKBNMEIL          PIC X(78).
                   07  SPA-GMN-OCRKBNLST-G.
                       09  SPA-GMN-OCRKBN-MAX              PIC 9(03).
                       09  SPA-GMN-OCRKBNLST               OCCURS 10.
                           11  SPA-GMN-OCRKBNL             PIC X(01).
                           11  SPA-GMN-OCRKBNFL            PIC X(01).
                           11  SPA-GMN-OCRKBNMEIL          PIC X(78).
                   07  SPA-GMN-SAIKEIKBNLST-G.
                       09  SPA-GMN-SAIKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-SAIKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-SAIKEIKBNL          PIC X(01).
                           11  SPA-GMN-SAIKEIKBNFL         PIC X(01).
                           11  SPA-GMN-SAIKEIKBNMEIL       PIC X(78).
                   07  SPA-GMN-SAIKEITAIKBNLST-G.
                       09  SPA-GMN-SAIKEITAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-SAIKEITAIKBNLST         OCCURS 10.
                           11  SPA-GMN-SAIKEITAIKBNL       PIC X(01).
                           11  SPA-GMN-SAIKEITAIKBNFL      PIC X(01).
                           11  SPA-GMN-SAIKEITAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-HKNFTNKBNLST-G.
                       09  SPA-GMN-HKNFTNKBN-MAX           PIC 9(03).
                       09  SPA-GMN-HKNFTNKBNLST            OCCURS 10.
                           11  SPA-GMN-HKNFTNKBNL          PIC X(01).
                           11  SPA-GMN-HKNFTNKBNFL         PIC X(01).
                           11  SPA-GMN-HKNFTNKBNMEIL       PIC X(78).
                   07  SPA-GMN-RECEKSIKBNLST-G.
                       09  SPA-GMN-RECEKSIKBN-MAX          PIC 9(03).
                       09  SPA-GMN-RECEKSIKBNLST           OCCURS 10.
                           11  SPA-GMN-RECEKSIKBNL         PIC X(01).
                           11  SPA-GMN-RECEKSIKBNFL        PIC X(01).
                           11  SPA-GMN-RECEKSIKBNMEIL      PIC X(78).
                   07  SPA-GMN-RECEKSITAIKBNLST-G.
                       09  SPA-GMN-RECEKSITAIKBN-MAX       PIC 9(03).
                       09  SPA-GMN-RECEKSITAIKBNLST        OCCURS 10.
                           11  SPA-GMN-RECEKSITAIKBNL      PIC X(01).
                           11  SPA-GMN-RECEKSITAIKBNFL     PIC X(01).
                           11  SPA-GMN-RECEKSITAIKBNMEIL   PIC X(38).
                   07  SPA-GMN-CHKJGNKBNGLST-G.
                       09  SPA-GMN-CHKJGNKBNG-MAX          PIC 9(03).
                       09  SPA-GMN-CHKJGNKBNGLST           OCCURS 10.
                           11  SPA-GMN-CHKJGNKBNGL         PIC X(01).
                           11  SPA-GMN-CHKJGNKBNGFL        PIC X(01).
                           11  SPA-GMN-CHKJGNKBNGMEIL      PIC X(78).
                   07  SPA-GMN-CHKJGNKBNNLST-G.
                       09  SPA-GMN-CHKJGNKBNN-MAX          PIC 9(03).
                       09  SPA-GMN-CHKJGNKBNNLST           OCCURS 10.
                           11  SPA-GMN-CHKJGNKBNNL         PIC X(01).
                           11  SPA-GMN-CHKJGNKBNNFL        PIC X(01).
                           11  SPA-GMN-CHKJGNKBNNMEIL      PIC X(78).
                   07  SPA-GMN-CHKJGNTAIKBNLST-G.
                       09  SPA-GMN-CHKJGNTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-CHKJGNTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-CHKJGNTAIKBNL       PIC X(01).
                           11  SPA-GMN-CHKJGNTAIKBNFL      PIC X(01).
                           11  SPA-GMN-CHKJGNTAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-KOHSKYKBNLST-G.
                       09  SPA-GMN-KOHSKYKBN-MAX           PIC 9(03).
                       09  SPA-GMN-KOHSKYKBNLST            OCCURS 10.
                           11  SPA-GMN-KOHSKYKBNL          PIC X(01).
                           11  SPA-GMN-KOHSKYKBNFL         PIC X(01).
                           11  SPA-GMN-KOHSKYKBNMEIL       PIC X(78).
                   07  SPA-GMN-JYUHENKBNLST-G.
                       09  SPA-GMN-JYUHENKBN-MAX           PIC 9(03).
                       09  SPA-GMN-JYUHENKBNLST            OCCURS 10.
                           11  SPA-GMN-JYUHENKBNL          PIC X(01).
                           11  SPA-GMN-JYUHENKBNFL         PIC X(01).
                           11  SPA-GMN-JYUHENKBNMEIL       PIC X(78).
                   07  SPA-GMN-JYUHENTAIKBNLST-G.
                       09  SPA-GMN-JYUHENTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-JYUHENTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-JYUHENTAIKBNL       PIC X(01).
                           11  SPA-GMN-JYUHENTAIKBNFL      PIC X(01).
                           11  SPA-GMN-JYUHENTAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-JGNFTNKBNLST-G.
                       09  SPA-GMN-JGNFTNKBN-MAX           PIC 9(03).
                       09  SPA-GMN-JGNFTNKBNLST            OCCURS 10.
                           11  SPA-GMN-JGNFTNKBNL          PIC X(01).
                           11  SPA-GMN-JGNFTNKBNFL         PIC X(01).
                           11  SPA-GMN-JGNFTNKBNMEIL       PIC X(78).
                   07  SPA-GMN-JGNFTNTAIKBNLST-G.
                       09  SPA-GMN-JGNFTNTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-JGNFTNTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-JGNFTNTAIKBNL       PIC X(01).
                           11  SPA-GMN-JGNFTNTAIKBNFL      PIC X(01).
                           11  SPA-GMN-JGNFTNTAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-PTJGNKBNLST-G.
                       09  SPA-GMN-PTJGNKBN-MAX            PIC 9(03).
                       09  SPA-GMN-PTJGNKBNLST             OCCURS 10.
                           11  SPA-GMN-PTJGNKBNL           PIC X(01).
                           11  SPA-GMN-PTJGNKBNFL          PIC X(01).
                           11  SPA-GMN-PTJGNKBNMEIL        PIC X(78).
                   07  SPA-GMN-PTJGNTAIKBNLST-G.
                       09  SPA-GMN-PTJGNTAIKBN-MAX         PIC 9(03).
                       09  SPA-GMN-PTJGNTAIKBNLST          OCCURS 10.
                           11  SPA-GMN-PTJGNTAIKBNL        PIC X(01).
                           11  SPA-GMN-PTJGNTAIKBNFL       PIC X(01).
                           11  SPA-GMN-PTJGNTAIKBNMEIL     PIC X(38).
                   07  SPA-GMN-IRYOKEIKBNLST-G.
                       09  SPA-GMN-IRYOKEIKBN-MAX          PIC 9(03).
                       09  SPA-GMN-IRYOKEIKBNLST           OCCURS 10.
                           11  SPA-GMN-IRYOKEIKBNL         PIC X(01).
                           11  SPA-GMN-IRYOKEIKBNFL        PIC X(01).
                           11  SPA-GMN-IRYOKEIKBNMEIL      PIC X(78).
                   07  SPA-GMN-IRYOKEITAIKBNLST-G.
                       09  SPA-GMN-IRYOKEITAIKBN-MAX       PIC 9(03).
                       09  SPA-GMN-IRYOKEITAIKBNLST        OCCURS 10.
                           11  SPA-GMN-IRYOKEITAIKBNL      PIC X(01).
                           11  SPA-GMN-IRYOKEITAIKBNFL     PIC X(01).
                           11  SPA-GMN-IRYOKEITAIKBNMEIL   PIC X(38).
                   07  SPA-GMN-SKJKEIKBNLST-G.
                       09  SPA-GMN-SKJKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-SKJKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-SKJKEIKBNL          PIC X(01).
                           11  SPA-GMN-SKJKEIKBNFL         PIC X(01).
                           11  SPA-GMN-SKJKEIKBNMEIL       PIC X(78).
                   07  SPA-GMN-SKJKEITAIKBNLST-G.
                       09  SPA-GMN-SKJKEITAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-SKJKEITAIKBNLST         OCCURS 10.
                           11  SPA-GMN-SKJKEITAIKBNL       PIC X(01).
                           11  SPA-GMN-SKJKEITAIKBNFL      PIC X(01).
                           11  SPA-GMN-SKJKEITAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-SKJFUKAKBNLST-G.
                       09  SPA-GMN-SKJFUKAKBN-MAX          PIC 9(03).
                       09  SPA-GMN-SKJFUKAKBNLST           OCCURS 10.
                           11  SPA-GMN-SKJFUKAKBNL         PIC X(01).
                           11  SPA-GMN-SKJFUKAKBNFL        PIC X(01).
                           11  SPA-GMN-SKJFUKAKBNMEIL      PIC X(78).
                   07  SPA-GMN-SKJFUKATAIKBNHONLST-G.
                       09  SPA-GMN-SKJFUKATAIKBNHON-MAX     PIC 9(03).
                       09  SPA-GMN-SKJFUKATAIKBNHONLST      OCCURS 10.
                           11  SPA-GMN-SKJFUKATAIKBNHONL    PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNHONFL   PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNHONMEIL PIC X(38).
                   07  SPA-GMN-SKJFUKATAIKBNTSYLST-G.
                       09  SPA-GMN-SKJFUKATAIKBNTSY-MAX     PIC 9(03).
                       09  SPA-GMN-SKJFUKATAIKBNTSYLST      OCCURS 10.
                           11  SPA-GMN-SKJFUKATAIKBNTSYL    PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNTSYFL   PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNTSYMEIL PIC X(38).
                   07  SPA-GMN-SKJFUKATAIKBNTNKLST-G.
                       09  SPA-GMN-SKJFUKATAIKBNTNK-MAX     PIC 9(03).
                       09  SPA-GMN-SKJFUKATAIKBNTNKLST      OCCURS 10.
                           11  SPA-GMN-SKJFUKATAIKBNTNKL    PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNTNKFL   PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNTNKMEIL PIC X(38).
                   07  SPA-GMN-COMMONTAIKBNLST-G.
                       09  SPA-GMN-COMMONTAIKBN-MAX         PIC 9(03).
                       09  SPA-GMN-COMMONTAIKBNLST          OCCURS 10.
                           11  SPA-GMN-COMMONTAIKBNL        PIC X(01).
                           11  SPA-GMN-COMMONTAIKBNFL       PIC X(01).
                           11  SPA-GMN-COMMONTAIKBNMEIL     PIC X(38).
               05  SPA-GMN-KOHLST-AREA.
                   07  SPA-GMN-KOHLST-MAX                  PIC 9(5).
                   07  SPA-GMN-KOHLST-SEL                  PIC 9(5).
                   07  SPA-GMN-KOHLST                      OCCURS  100.
                       09  SPA-GMN-TKKOHNUM                PIC X(3).
                       09  SPA-GMN-TKKOHNAME               PIC X(100).
                       09  SPA-GMN-TKYUKOSTYMD             PIC X(10).
                       09  SPA-GMN-TKYUKOEDYMD             PIC X(10).
               05  SPA-GMN-RIREKILST-AREA.
                   07  SPA-GMN-RIREKILST-MAX               PIC 9(5).
                   07  SPA-GMN-RIREKILST-SEL               PIC 9(5).
                   07  SPA-GMN-RIREKILST                   OCCURS   20.
                       09  SPA-GMN-TRYUKOSTYMD             PIC X(10).
                       09  SPA-GMN-TRYUKOEDYMD             PIC X(10).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-KJNYMD                      PIC X(08).
                   07  SPA-NAI-INPUT-AREA.
                       09  SPA-NAI-INPUT-MODE              PIC 9(1).
                       09  SPA-NAI-SELNUM1-FLG             PIC 9(1)
                                                           OCCURS  100.
                       09  SPA-NAI-KOHNUM                  PIC X(03).
                       09  SPA-NAI-KOHNUM-ORG              PIC X(03).
                       09  SPA-NAI-SKJKBNHON               PIC X(01).
                       09  SPA-NAI-SKJKBNTSY               PIC X(01).
                       09  SPA-NAI-SKJKBNTNK               PIC X(01).
                       09  SPA-NAI-YUKOSTYMD               PIC X(08).
                       09  SPA-NAI-YUKOSTYMD-ORG           PIC X(08).
                       09  SPA-NAI-YUKOEDYMD               PIC X(08).
                       09  SPA-NAI-YUKOEDYMD-ORG           PIC X(08).
                       09  SPA-NAI-NBOOK-AREA.
                           11  SPA-NAI-NBOOK-DUMMY         PIC X(01).
               05  SPA-NAI-KOHLST-AREA.
                   07  SPA-NAI-KOHLST                      OCCURS   100.
                       09  SPA-NAI-TKYUKOSTYMD             PIC X(08).
                       09  SPA-NAI-TKYUKOEDYMD             PIC X(08).
                       09  SPA-NAI-TKSKJKBNHON             PIC X(01).
                       09  SPA-NAI-TKSKJKBNTSY             PIC X(01).
                       09  SPA-NAI-TKSKJKBNTNK             PIC X(01).
               05  SPA-NAI-RIREKILST-AREA.
                   07  SPA-NAI-RIREKILST                   OCCURS   20.
                       09  SPA-NAI-TRYUKOSTYMD             PIC X(08).
                       09  SPA-NAI-TRYUKOEDYMD             PIC X(08).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                                     PIC 9(01).
           03  FLG-SYSKANRI                                PIC 9(01).
           03  FLG-HKNNUM                                  PIC 9(01).
           03  FLG-ENTRYCHK                                PIC 9(01).
           03  FLG-HIZUKECHK                               PIC 9(01).
           03  FLG-UPD-KBN                                 PIC 9(01).
           03  FLG-KIKAN-DABURI                            PIC 9(01).
           03  FLG-ERR                                     PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                        PIC 9(05).
           03  IDX1                                        PIC 9(05).
           03  IDX-MAX                                     PIC 9(05).
           03  IDX-CMB                                     PIC 9(05).
           03  IDX-MUL                                     PIC 9(05).
           03  IDX-LST                                     PIC 9(05).
           03  IDXE                                        PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SEL                                     PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET                              PIC X(64). 
           03  WRK-WIDMSG                                  PIC X(70). 
           03  WRK-MAX                                     PIC 9(05).
           03  WRK-YMD                                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                                 PIC 9(04).
               05  WRK-SMM                                 PIC 9(02).
               05  WRK-SDD                                 PIC 9(02).
           03  WRK-HENYMDG                                 PIC X(09).
           03  WRK-YUKOSTYMD                               PIC X(08).
           03  WRK-YUKOEDYMD                               PIC X(08).
           03  WRK-KANACHK-INPUT                           PIC X(400).
           03  WRK-ZZ9                                     PIC ZZ9.
           03  WRK-ZZZ9                                    PIC ZZZ9.
           03  WRK-ZZZ-G.
               05  WRK-ZZZ                                 PIC ZZZ.
           03  WRK-STR-TMP                                 PIC X(100).
           03  WRK-CMB-CD                                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                             PIC X(200).
               05  WRK-CMB-ITM2                            PIC X(200).
           03  WRK-ERRSTYMD                                PIC X(09).
           03  WRK-ERREDYMD                                PIC X(09).
           03  WRK-SELNUM-G.
               05  WRK-SELNUM-MUL                          PIC X(50).
               05  WRK-INPUT-MODE                          PIC 9(01).
               05  WRK-SELNUM-X                            PIC X(50).
               05  WRK-SELNUM                              PIC 9(03).
               05  WRK-BASESELNUM                          PIC 9(03).
               05  WRK-SELNUM-FLG                          PIC 9(01)
                                                           OCCURS  200.
           03  WRK-MULTI-AREA.
               05  WRK-MUL-NUM                             PIC X(10)
                                                           OCCURS 20.
               05  WRK-MUL-STNUM-X                         PIC X(10).
               05  WRK-MUL-EDNUM-X                         PIC X(10).
               05  WRK-MUL-STNUM                           PIC 9(05).
               05  WRK-MUL-EDNUM                           PIC 9(05).
           03  WRK-PAGENO                                  PIC 9(03).
           03  WRK-NAI-YUKOSTYMD                           PIC X(08).
           03  WRK-NAI-YUKOEDYMD                           PIC X(08).
           03  WRK-SKJKBN                                  PIC X(01)
                                                           OCCURS 3.
           03  WRK-TABNAME                                 PIC X(20).
      *
       01  KEY-AREA.
           03  KEY-HKNNUM                                  PIC X(03).
      *
       01  SEL-AREA.
           05  SEL-HKNNUM                                  PIC X(03).
           05  SEL-KJNYMD                                  PIC X(08).
           05  SEL-STYMD                                   PIC X(08).
           05  SEL-EDYMD                                   PIC X(08).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-KOHLST-MAX           PIC 9(03) VALUE 100.
           03  CONST-RIREKILST-MAX        PIC 9(03) VALUE 20.
      *
           03  CONST-HOKKAIDO             PIC 9(02) VALUE 01.
           03  CONST-AOMORI               PIC 9(02) VALUE 02.
           03  CONST-IWATE                PIC 9(02) VALUE 03.
           03  CONST-MIYAGI               PIC 9(02) VALUE 04.
           03  CONST-AKITA                PIC 9(02) VALUE 05.
           03  CONST-YAMAGATA             PIC 9(02) VALUE 06.
           03  CONST-FUKUSHIMA            PIC 9(02) VALUE 07.
           03  CONST-IBARAKI              PIC 9(02) VALUE 08.
           03  CONST-TOCHIGI              PIC 9(02) VALUE 09.
           03  CONST-GUNMA                PIC 9(02) VALUE 10.
           03  CONST-SAITAMA              PIC 9(02) VALUE 11.
           03  CONST-CHIBA                PIC 9(02) VALUE 12.
           03  CONST-TOKYO                PIC 9(02) VALUE 13.
           03  CONST-KANAGAWA             PIC 9(02) VALUE 14.
           03  CONST-NIIGATA              PIC 9(02) VALUE 15.
           03  CONST-TOYAMA               PIC 9(02) VALUE 16.
           03  CONST-ISHIKAWA             PIC 9(02) VALUE 17.
           03  CONST-FUKUI                PIC 9(02) VALUE 18.
           03  CONST-YAMANASHI            PIC 9(02) VALUE 19.
           03  CONST-NAGANO               PIC 9(02) VALUE 20.
           03  CONST-GIFU                 PIC 9(02) VALUE 21.
           03  CONST-SHIZUOKA             PIC 9(02) VALUE 22.
           03  CONST-AICHI                PIC 9(02) VALUE 23.
           03  CONST-MIE                  PIC 9(02) VALUE 24.
           03  CONST-SHIGA                PIC 9(02) VALUE 25.
           03  CONST-KYOTO                PIC 9(02) VALUE 26.
           03  CONST-OSAKA                PIC 9(02) VALUE 27.
           03  CONST-HYOGO                PIC 9(02) VALUE 28.
           03  CONST-NARA                 PIC 9(02) VALUE 29.
           03  CONST-WAKAYAMA             PIC 9(02) VALUE 30.
           03  CONST-TOTTORI              PIC 9(02) VALUE 31.
           03  CONST-SHIMANE              PIC 9(02) VALUE 32.
           03  CONST-OKAYAMA              PIC 9(02) VALUE 33.
           03  CONST-HIROSHIMA            PIC 9(02) VALUE 34.
           03  CONST-YAMAGUCHI            PIC 9(02) VALUE 35.
           03  CONST-TOKUSHIMA            PIC 9(02) VALUE 36.
           03  CONST-KAGAWA               PIC 9(02) VALUE 37.
           03  CONST-EHIME                PIC 9(02) VALUE 38.
           03  CONST-KOCHI                PIC 9(02) VALUE 39.
           03  CONST-FUKUOKA              PIC 9(02) VALUE 40.
           03  CONST-SAGA                 PIC 9(02) VALUE 41.
           03  CONST-NAGASAKI             PIC 9(02) VALUE 42.
           03  CONST-KUMAMOTO             PIC 9(02) VALUE 43.
           03  CONST-OITA                 PIC 9(02) VALUE 44.
           03  CONST-MIYAZAKI             PIC 9(02) VALUE 45.
           03  CONST-KAGOSHIMA            PIC 9(02) VALUE 46.
           03  CONST-OKINAWA              PIC 9(02) VALUE 47.
      *
      *    保険番号
       01  THKNNUM-REC.
           COPY    "CPHKNNUM.INC"         REPLACING     //HKN-//
                                          BY            //THKN-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    公費付加情報
           COPY  "CPSK2010.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "W01.INC".
           COPY    "W02.INC".
           COPY    "W03.INC".
           COPY    "W04.INC".
           COPY    "W05.INC".
           COPY    "W06.INC".
           COPY    "W07.INC". 
           COPY    "W08.INC". 
           COPY    "W09.INC".
           COPY    "W10.INC".
           COPY    "W11.INC".
           COPY    "W111.INC".
           COPY    "W12.INC".
           COPY    "W121.INC".
           COPY    "W13.INC".
           COPY    "W14.INC".
           COPY    "W15.INC".
           COPY    "W16.INC".
           COPY    "W17.INC".
           COPY    "W18.INC".
           COPY    "W19.INC".
           COPY    "W20.INC".
           COPY    "W201.INC".
           COPY    "W21.INC".
           COPY    "W22.INC".
           COPY    "W23.INC".
           COPY    "W24.INC".
           COPY    "W25.INC".
           COPY    "W28.INC".
           COPY    "W29.INC".
           COPY    "W30.INC".
           COPY    "W31.INC".
           COPY    "W32.INC".
           COPY    "W321.INC".
           COPY    "W33.INC".
           COPY    "W34.INC".
           COPY    "W35.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W62.INC".
           COPY    "W63.INC".
           COPY    "W64.INC".
           COPY    "W65.INC".
           COPY    "W66.INC".
           COPY    "W67.INC".
           COPY    "W68.INC".
           COPY    "W96.INC".
           COPY    "W98.INC".
           COPY    "W981.INC".
           COPY    "W97.INC".
           COPY    "W99.INC".
           COPY    "WERR.INC".
           COPY    "WID1.INC".
           COPY    "WID2.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SEL-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W65
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W65         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG      =   "WERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W65"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
               IF    ( SPA-W01-KBNCD  >=   "100" )
                AND  ( SPA-W01-KBNCD  <=   "899" )
                   PERFORM 312-SPASET-SEC
               END-IF
           END-EVALUATE
      *
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-W65
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-KJNYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-KJNYMD
      *
           PERFORM 311-SPASET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       311-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-RIREKILST-AREA
                                   SPA-NAI-RIREKILST-AREA
      *
           PERFORM 311-COMBO-EDIT-SEC
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           PERFORM 3110-KOHLST-EDIT-SEC
      *
           MOVE    0               TO  SPA-GMN-PAGENO
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       311-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       311-COMBO-EDIT-SEC         SECTION.
      *
      *    実日数省略記載コンボ編集処理
           PERFORM 3110-JNISSUKBNLST-SEC
      *
      *    請求点省略記載コンボ編集処理
           PERFORM 3110-TOTALTENKBNLST-SEC
      *
      *    全国公費併用分請求点集計コンボ編集処理
           PERFORM 3110-SYUKEIKBNLST-SEC
      *
      *    一部負担金金額集計コンボ編集処理
           PERFORM 3110-FTNKEIKBNLST-SEC
      *
      *    一部負担金省略記載コンボ編集処理
           PERFORM 3110-FTNMONEYKBNLST-SEC
      *
      *    一部負担金０円記載（記録）コンボ編集処理
           PERFORM 3110-FTNZEROKBNLST-SEC
      *
      *    入院食事療養省略記載コンボ編集処理
           PERFORM 3110-SKJKBNLST-SEC
      *
      *    ＯＣＲ実日数請求点省略記載コンボ編集処理
           PERFORM 3110-OCRKBNLST-SEC
      *
      *    請求書公費再掲欄記載コンボ編集処理
           PERFORM 3110-SAIKEIKBNLST-SEC
      *
      *    保険欄−一部負担金記載コンボ編集処理
           PERFORM 3110-HKNFTNKBNLST-SEC
      *
      *    設定と異なる記載設定コンボ編集処理
           PERFORM 3110-RECEKSIKBNLST-SEC
      *
      *    設定と異なる記載設定対象区分コンボ編集処理
           PERFORM 3110-RECEKSITAIKBNLST-SEC
      *
      *    チェック対象上限額設定（外来）コンボ編集処理
           PERFORM 3110-CHKJGNKBNGLST-SEC
      *
      *    チェック対象上限額設定（入院）コンボ編集処理
           PERFORM 3110-CHKJGNKBNNLST-SEC
      *
      *    チェック対象上限額設定対象区分コンボ編集処理
           PERFORM 3110-CHKJGNTAIKBNLST-SEC
      *
      *    公費請求テーブル作成コンボ編集処理
           PERFORM 3110-KOHSKYKBNLST-SEC
      *
      *    月途中受給者証変更時負担計算コンボ編集処理
           PERFORM 3110-JYUHENKBNLST-SEC
      *
      *    日・月上限負担金計算コンボ編集処理
           PERFORM 3110-JGNFTNKBNLST-SEC
      *
      *    患者登録−所得者情報−月上限額入力コンボ編集処理
           PERFORM 3110-PTJGNKBNLST-SEC
      *
      *    患者登録−所得者情報−月上限額入力対象区分コンボ編集処理
           PERFORM 3110-PTJGNTAIKBNLST-SEC
      *
      *    医療費負担金計算（外来）コンボ編集処理
           PERFORM 3110-IRYOKEIKBNLST-SEC
      *
      *    医療費負担金計算対象区分コンボ編集処理
           PERFORM 3110-IRYOKEITAIKBNLST-SEC
      *
      *    食事費負担金計算（外来）コンボ編集処理
           PERFORM 3110-SKJKEIKBNLST-SEC
      *
      *    食事費負担金計算対象区分コンボ編集処理
           PERFORM 3110-SKJKEITAIKBNLST-SEC
      *
      *    食事療養付加設定コンボ編集処理
           PERFORM 3110-SKJFUKAKBNLST-SEC
      *
      *    食事療養付加設定対象区分本人コンボ編集処理
           PERFORM 3110-SKJFUKATAIKBNHONLST-SEC
      *
      *    食事療養付加設定対象区分低所得コンボ編集処理
           PERFORM 3110-SKJFUKATAIKBNTSYLST-SEC
      *
      *    食事療養付加設定対象区分低年金コンボ編集処理
           PERFORM 3110-SKJFUKATAIKBNTNKLST-SEC
      *
      *    共通対象区分コンボ編集処理
           PERFORM 3110-COMMONTAIKBNLST-SEC
      *
      *    請求書公費再掲欄記載対象区分コンボ編集処理
           PERFORM 3110-SAIKEITAIKBNLST-SEC
      *
      *    月途中受給者証変更時負担計算対象区分コンボ編集処理
           PERFORM 3110-JYUHENTAIKBNLST-SEC
      *
           .
       311-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    実日数省略記載コンボ編集処理
      *****************************************************************
       3110-JNISSUKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-JNISSUKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JNISSUKBNL    (1)
           MOVE    "上位日数と同日数の場合、省略する"
                                   TO  SPA-GMN-JNISSUKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-JNISSUKBNL    (2)
           MOVE    "上位日数と同日数の場合、省略しない"
                                   TO  SPA-GMN-JNISSUKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-JNISSUKBNL    (3)
           MOVE    "上位日数と同日数の場合、無条件に省略する"
                                   TO  SPA-GMN-JNISSUKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-JNISSUKBN-MAX
      *
           .
       3110-JNISSUKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    請求点省略記載コンボ編集処理
      *****************************************************************
       3110-TOTALTENKBNLST-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-TOTALTENKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-TOTALTENKBNL    (1)
           MOVE    "上位点数と同点数の場合、省略する"
                                   TO  SPA-GMN-TOTALTENKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-TOTALTENKBNL    (2)
           MOVE    "上位点数と同点数の場合、省略しない"
                                   TO  SPA-GMN-TOTALTENKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-TOTALTENKBNL    (3)
           MOVE    "上位点数と同点数の場合、無条件に省略する"
                                   TO  SPA-GMN-TOTALTENKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-TOTALTENKBN-MAX
      *
           .
       3110-TOTALTENKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用分請求点集計コンボ編集処理
      *****************************************************************
       3110-SYUKEIKBNLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-SYUKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SYUKEIKBNL    (1)
           MOVE    "集計する"      TO  SPA-GMN-SYUKEIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SYUKEIKBNL    (2)
           MOVE    "集計しない"    TO  SPA-GMN-SYUKEIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-SYUKEIKBNL    (3)
           MOVE    "集計しない（請求点が０点となる場合は除く）"
                                   TO  SPA-GMN-SYUKEIKBNMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-SYUKEIKBNL    (4)
           MOVE    "集計しない【負担区分対応】"
                                   TO  SPA-GMN-SYUKEIKBNMEIL (4)
      *
           MOVE    "4"             TO  SPA-GMN-SYUKEIKBNL    (5)
           MOVE
           "集計しない（請求点が０点となる場合は除く）【負担区分対応】"
                                   TO  SPA-GMN-SYUKEIKBNMEIL (5)
      *
           MOVE    5               TO  SPA-GMN-SYUKEIKBN-MAX
      *
           .
       3110-SYUKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計コンボ編集処理
      *****************************************************************
       3110-FTNKEIKBNLST-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-FTNKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-FTNKEIKBNL    (1)
           MOVE    "集計する"      TO  SPA-GMN-FTNKEIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-FTNKEIKBNL    (2)
           MOVE    "集計しない"    TO  SPA-GMN-FTNKEIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-FTNKEIKBNL    (3)
           MOVE    "集計する（レセ記載・レセ電記録は省略する）"
                                   TO  SPA-GMN-FTNKEIKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-FTNKEIKBN-MAX
      *
           .
       3110-FTNKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載コンボ編集処理
      *****************************************************************
       3110-FTNMONEYKBNLST-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-FTNMONEYKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-FTNMONEYKBNL    (1)
           MOVE    "上位金額と同金額の場合、省略する"
                                   TO  SPA-GMN-FTNMONEYKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-FTNMONEYKBNL    (2)
           MOVE    "上位金額と同金額の場合、省略しない"
                                   TO  SPA-GMN-FTNMONEYKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-FTNMONEYKBNL    (3)
           MOVE    "上位金額と同金額の場合、無条件に省略する"
                                   TO  SPA-GMN-FTNMONEYKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-FTNMONEYKBN-MAX
      *
           .
       3110-FTNMONEYKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）コンボ編集処理
      *****************************************************************
       3110-FTNZEROKBNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-FTNZEROKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-FTNZEROKBNL    (1)
           MOVE    "０円の場合、省略する"
                                   TO  SPA-GMN-FTNZEROKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-FTNZEROKBNL    (2)
           MOVE    "０円の場合、省略しない"
                                   TO  SPA-GMN-FTNZEROKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-FTNZEROKBNL    (3)
           MOVE    "０円の場合、無条件に省略する"
                                   TO  SPA-GMN-FTNZEROKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-FTNZEROKBN-MAX
      *
           .
       3110-FTNZEROKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    入院食事療養省略記載コンボ編集処理
      *****************************************************************
       3110-SKJKBNLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJKBNL    (1)
           MOVE    "上位食事療養と同内容の場合、省略する"
                                   TO  SPA-GMN-SKJKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SKJKBNL    (2)
           MOVE    "上位食事療養と同内容の場合、省略しない"
                                   TO  SPA-GMN-SKJKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-SKJKBN-MAX
      *
           .
       3110-SKJKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    ＯＣＲ実日数請求点省略記載コンボ編集処理
      *****************************************************************
       3110-OCRKBNLST-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-OCRKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-OCRKBNL    (1)
           MOVE    "上位日数点数と同日数点数の場合、０とする"
                                   TO  SPA-GMN-OCRKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-OCRKBNL    (2)
           MOVE    "上位日数点数と同日数点数の場合、０としない"
                                   TO  SPA-GMN-OCRKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-OCRKBNL    (3)
           MOVE    "上位日数点数と同日数点数の場合、無条件に０とする"
                                   TO  SPA-GMN-OCRKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-OCRKBN-MAX
      *
           .
       3110-OCRKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載コンボ編集処理
      *****************************************************************
       3110-SAIKEIKBNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SAIKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SAIKEIKBNL    (1)
           MOVE    "記載する"      TO  SPA-GMN-SAIKEIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SAIKEIKBNL    (2)
           MOVE    "記載しない"    TO  SPA-GMN-SAIKEIKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-SAIKEIKBN-MAX
      *
           .
       3110-SAIKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載コンボ編集処理
      *****************************************************************
       3110-HKNFTNKBNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNFTNKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-HKNFTNKBNL    (1)
           MOVE    "所得区分に準ずる金額記載とする" 
                                   TO  SPA-GMN-HKNFTNKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-HKNFTNKBNL    (2)
           MOVE    "一律”一般区分”での金額記載とする"
                                   TO  SPA-GMN-HKNFTNKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-HKNFTNKBN-MAX
      *
           .
       3110-HKNFTNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定コンボ編集処理
      *****************************************************************
       3110-RECEKSIKBNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-RECEKSIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-RECEKSIKBNL    (1)
           MOVE    "設定なし" 
                                   TO  SPA-GMN-RECEKSIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-RECEKSIKBNL    (2)
           MOVE    "負担に関係なく記載なし"
                                   TO  SPA-GMN-RECEKSIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-RECEKSIKBNL    (3)
           MOVE    "負担上限未満記載あり"
                                   TO  SPA-GMN-RECEKSIKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-RECEKSIKBN-MAX
      *
           .
       3110-RECEKSIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定対象区分コンボ編集処理
      *****************************************************************
       3110-RECEKSITAIKBNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-RECEKSITAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-RECEKSITAIKBNL    (1)
           MOVE    "レセプト請求分全て" 
                                   TO  SPA-GMN-RECEKSITAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-RECEKSITAIKBN-MAX
      *
           .
       3110-RECEKSITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（外来）コンボ編集処理
      *****************************************************************
       3110-CHKJGNKBNGLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-CHKJGNKBNGLST-G
      *
           MOVE    "0"             TO  SPA-GMN-CHKJGNKBNGL    (1)
           MOVE    "月−上限額（上限額が０円の場合はチェックしない）"
                                   TO  SPA-GMN-CHKJGNKBNGMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-CHKJGNKBNGL    (2)
           MOVE "日−上限額（上限額が０円の場合は月−上限額でチェック）"
                                   TO  SPA-GMN-CHKJGNKBNGMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-CHKJGNKBNGL    (3)
           MOVE "回−上限額（上限額が０円の場合は月−上限額でチェック）"
                                   TO  SPA-GMN-CHKJGNKBNGMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-CHKJGNKBNG-MAX
      *
           .
       3110-CHKJGNKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（入院）コンボ編集処理
      *****************************************************************
       3110-CHKJGNKBNNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-CHKJGNKBNNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-CHKJGNKBNNL    (1)
           MOVE    "月−上限額（上限額が０円の場合はチェックしない）"
                                   TO  SPA-GMN-CHKJGNKBNNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-CHKJGNKBNNL    (2)
           MOVE "日−上限額（上限額が０円の場合は月−上限額でチェック）"
                                   TO  SPA-GMN-CHKJGNKBNNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-CHKJGNKBNN-MAX
      *
           .
       3110-CHKJGNKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定対象区分コンボ編集処理
      *****************************************************************
       3110-CHKJGNTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-CHKJGNTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-CHKJGNTAIKBNL    (1)
           MOVE    "負担上限未満記載なし設定公費"
                                   TO  SPA-GMN-CHKJGNTAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-CHKJGNTAIKBN-MAX
      *
           .
       3110-CHKJGNTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成コンボ編集処理
      *****************************************************************
       3110-KOHSKYKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KOHSKYKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-KOHSKYKBNL    (1)
           MOVE    "作成しない"
                                   TO  SPA-GMN-KOHSKYKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-KOHSKYKBNL    (2)
           MOVE    "作成する"
                                   TO  SPA-GMN-KOHSKYKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-KOHSKYKBN-MAX
      *
           .
       3110-KOHSKYKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    日・月上限負担金計算コンボ編集処理
      *****************************************************************
       3110-JGNFTNKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-JGNFTNKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JGNFTNKBNL    (1)
           MOVE    "入外それぞれで上限額までの計算をする"
                                   TO  SPA-GMN-JGNFTNKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-JGNFTNKBNL    (2)
           MOVE    "入外あわせて上限額までの計算をする"
                                   TO  SPA-GMN-JGNFTNKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-JGNFTNKBN-MAX
      *
           .
       3110-JGNFTNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算コンボ編集処理
      *****************************************************************
       3110-JYUHENKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-JYUHENKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JYUHENKBNL    (1)
           MOVE    "変更前の負担を考慮しない"
                                   TO  SPA-GMN-JYUHENKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-JYUHENKBNL    (2)
           MOVE    "変更前の負担を考慮する"
                                   TO  SPA-GMN-JYUHENKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-JYUHENKBNL    (3)
           MOVE    "変更前の負担を考慮する（同一市町村のみ）"
                                   TO  SPA-GMN-JYUHENKBNMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-JYUHENKBNL    (4)
           MOVE    "変更前の負担を無条件に考慮しない"
                                   TO  SPA-GMN-JYUHENKBNMEIL (4)
      *
           MOVE    4               TO  SPA-GMN-JYUHENKBN-MAX
      *
           .
       3110-JYUHENKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力コンボ編集処理
      *****************************************************************
       3110-PTJGNKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-PTJGNKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-PTJGNKBNL    (1)
           MOVE    "設定なし"
                                   TO  SPA-GMN-PTJGNKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-PTJGNKBNL    (2)
           MOVE    "外来上限額・入院上限額を入力する"
                                   TO  SPA-GMN-PTJGNKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-PTJGNKBNL    (3)
           MOVE    "入外上限額・他一部負担累計を入力する"
                                   TO  SPA-GMN-PTJGNKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-PTJGNKBN-MAX
      *
           .
       3110-PTJGNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力対象区分コンボ編集処理
      *****************************************************************
       3110-PTJGNTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-PTJGNTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-PTJGNTAIKBNL    (1)
           MOVE    "月上限額のある公費（患者単位）"
                                   TO  SPA-GMN-PTJGNTAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-PTJGNTAIKBN-MAX
      *
           .
       3110-PTJGNTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算コンボ編集処理
      *****************************************************************
       3110-IRYOKEIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-IRYOKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-IRYOKEIKBNL    (1)
           MOVE    "設定なし"
                                   TO  SPA-GMN-IRYOKEIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-IRYOKEIKBNL    (2)
           MOVE    "＜未満＞患者負担なし＜以上＞患者負担あり（全額）"
                                   TO  SPA-GMN-IRYOKEIKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-IRYOKEIKBN-MAX
      *
           .
       3110-IRYOKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算対象区分コンボ編集処理
      *****************************************************************
       3110-IRYOKEITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-IRYOKEITAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-IRYOKEITAIKBNL    (1)
           MOVE    "該当公費対象者全員"
                                   TO  SPA-GMN-IRYOKEITAIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-IRYOKEITAIKBNL    (2)
           MOVE    "低所得・低年金の方のみ"
                                   TO  SPA-GMN-IRYOKEITAIKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-IRYOKEITAIKBN-MAX
      *
           .
       3110-IRYOKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    食事費負担金計算コンボ編集処理
      *****************************************************************
       3110-SKJKEIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJKEIKBNL    (1)
           MOVE    "設定なし"
                                   TO  SPA-GMN-SKJKEIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SKJKEIKBNL    (2)
           MOVE    "＜未満＞患者負担なし＜以上＞患者負担あり（全額）"
                                   TO  SPA-GMN-SKJKEIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-SKJKEIKBNL    (3)
           MOVE
           "＜未満＞患者負担あり（半額）＜以上＞患者負担あり（全額）"
                                   TO  SPA-GMN-SKJKEIKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-SKJKEIKBN-MAX
      *
           .
       3110-SKJKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    食事費負担金計算対象区分コンボ編集処理
      *****************************************************************
       3110-SKJKEITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJKEITAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJKEITAIKBNL    (1)
           MOVE    "該当公費対象者全員"
                                   TO  SPA-GMN-SKJKEITAIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SKJKEITAIKBNL    (2)
           MOVE    "低所得・低年金の方のみ"
                                   TO  SPA-GMN-SKJKEITAIKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-SKJKEITAIKBN-MAX
      *
           .
       3110-SKJKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    食事療養付加設定コンボ編集処理
      *****************************************************************
       3110-SKJFUKAKBNLST-SEC      SECTION.
      *
           INITIALIZE              SPA-GMN-SKJFUKAKBNLST-G
      *
           MOVE    "0"         TO  SPA-GMN-SKJFUKAKBNL    (1)
           MOVE    "設定なし"
                               TO  SPA-GMN-SKJFUKAKBNMEIL (1)
      *
           MOVE    "1"         TO  SPA-GMN-SKJFUKAKBNL    (2)
           MOVE    "患者負担なし"
                               TO  SPA-GMN-SKJFUKAKBNMEIL (2)
      *
           MOVE    "2"         TO  SPA-GMN-SKJFUKAKBNL    (3)
           MOVE    "患者負担あり（半額）"
                               TO  SPA-GMN-SKJFUKAKBNMEIL (3)
      *
           MOVE    "3"         TO  SPA-GMN-SKJFUKAKBNL    (4)
           MOVE    "患者負担あり（全額）"
                               TO  SPA-GMN-SKJFUKAKBNMEIL (4)
      *
           MOVE    4           TO  SPA-GMN-SKJFUKAKBN-MAX
      *
           .
       3110-SKJFUKAKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    食事療養付加設定対象区分本人コンボ編集処理
      *****************************************************************
       3110-SKJFUKATAIKBNHONLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJFUKATAIKBNHONLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJFUKATAIKBNHONL    (1)
           MOVE    "本人タブ設定"
                                   TO  SPA-GMN-SKJFUKATAIKBNHONMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-SKJFUKATAIKBNHON-MAX
      *
           .
       3110-SKJFUKATAIKBNHONLST-EXT.
           EXIT.
      *****************************************************************
      *    食事療養付加設定対象区分低所得コンボ編集処理
      *****************************************************************
       3110-SKJFUKATAIKBNTSYLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJFUKATAIKBNTSYLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJFUKATAIKBNTSYL    (1)
           MOVE    "低所得タブ設定"
                                   TO  SPA-GMN-SKJFUKATAIKBNTSYMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-SKJFUKATAIKBNTSY-MAX
      *
           .
       3110-SKJFUKATAIKBNTSYLST-EXT.
           EXIT.
      *****************************************************************
      *    食事療養付加設定対象区分低年金コンボ編集処理
      *****************************************************************
       3110-SKJFUKATAIKBNTNKLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJFUKATAIKBNTNKLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJFUKATAIKBNTNKL    (1)
           MOVE    "低年金タブ設定"
                                   TO  SPA-GMN-SKJFUKATAIKBNTNKMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-SKJFUKATAIKBNTNK-MAX
      *
           .
       3110-SKJFUKATAIKBNTNKLST-EXT.
           EXIT.
      *****************************************************************
      *    共通対象区分コンボ編集処理
      *****************************************************************
       3110-COMMONTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-COMMONTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-COMMONTAIKBNL    (1)
           MOVE    "レセプト請求分全て" 
                                   TO  SPA-GMN-COMMONTAIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-COMMONTAIKBNL    (2)
           MOVE    "国保・広域（請求分内）"
                                   TO  SPA-GMN-COMMONTAIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-COMMONTAIKBNL    (3)
           MOVE    "社保（請求分内）"
                                   TO  SPA-GMN-COMMONTAIKBNMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-COMMONTAIKBNL    (4)
           MOVE    "国保（請求分内）"
                                   TO  SPA-GMN-COMMONTAIKBNMEIL (4)
      *
           MOVE    "4"             TO  SPA-GMN-COMMONTAIKBNL    (5)
           MOVE    "広域（請求分内）"
                                   TO  SPA-GMN-COMMONTAIKBNMEIL (5)
      *
           MOVE    5               TO  SPA-GMN-COMMONTAIKBN-MAX
      *
           .
       3110-COMMONTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載対象区分コンボ編集処理
      *****************************************************************
       3110-SAIKEITAIKBNLST-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-SAIKEITAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SAIKEITAIKBNL    (1)
           MOVE    "国保・広域（請求分内）"
                                   TO  SPA-GMN-SAIKEITAIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SAIKEITAIKBNL    (2)
           MOVE    "国保（請求分内）"
                                   TO  SPA-GMN-SAIKEITAIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-SAIKEITAIKBNL    (3)
           MOVE    "広域（請求分内）"
                                   TO  SPA-GMN-SAIKEITAIKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-SAIKEITAIKBN-MAX
      *
           .
       3110-SAIKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算対象区分コンボ編集処理
      *****************************************************************
       3110-JYUHENTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-JYUHENTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JYUHENTAIKBNL    (1)
           MOVE    "同一保険番号"  TO  SPA-GMN-JYUHENTAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-JYUHENTAIKBN-MAX
      *
           .
       3110-JYUHENTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類コンボ編集処理
      *****************************************************************
       3110-KOHNUMLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-KOHNUMLST-G
           MOVE    0               TO  IDX0
           MOVE    LOW-VALUE       TO  KEY-HKNNUM
      *
           MOVE    SPA-NAI-YUKOSTYMD   TO  SEL-STYMD
           MOVE    SPA-NAI-YUKOEDYMD   TO  SEL-EDYMD
           PERFORM 900-HKNNUM-KEY9-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNNUM  NOT =   ZERO )
                    OR   ( IDX0            >   200 )
      *
               IF    ( HKN-HKNNUM         >=   "100" )
                AND  ( HKN-HKNNUM         <=   "899" )
      *
                   IF    ( HKN-HKNNUM  NOT =   KEY-HKNNUM )
      *
                       COMPUTE IDX0 =  IDX0 +  1
      *
                       MOVE    HKN-HKNNUM  TO  SPA-GMN-KOHNUML (IDX0)
                       MOVE    HKN-SEIDONAME
                                           TO  SPA-GMN-KOHNUMMEIL (IDX0)
      *
                       MOVE    HKN-HKNNUM  TO  KEY-HKNNUM
                   END-IF
      *
               END-IF
      *
               PERFORM 900-HKNNUM-KEY9-FET-SEC
      *
           END-PERFORM
      *
           MOVE    IDX0            TO  SPA-GMN-KOHNUM-MAX
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3110-KOHNUMLST-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報リスト編集処理
      *****************************************************************
       3110-KOHLST-EDIT-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KOHLST-AREA
                                       SPA-NAI-KOHLST-AREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2010"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-2010-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-KOHLST-MAX  >   CONST-KOHLST-MAX )
      *
               COMPUTE SPA-GMN-KOHLST-MAX
                   =   SPA-GMN-KOHLST-MAX  +   1
      *
               MOVE    SPA-GMN-KOHLST-MAX
                                           TO  IDX-MAX
               MOVE    SYS-2010-KBNCD      TO  SPA-GMN-TKKOHNUM(IDX-MAX)
               MOVE    SYS-2010-KBNCD      TO  SEL-HKNNUM
               MOVE    SYS-2010-STYUKYMD   TO  SEL-STYMD
               MOVE    SYS-2010-EDYUKYMD   TO  SEL-EDYMD
               PERFORM 900-HKNNUM-KEY8-SEL-SEC
               MOVE    HKN-SEIDONAME
                                       TO  SPA-GMN-TKKOHNAME(IDX-MAX)
               MOVE    HKN-HON-NYUSKJRYYKBN
                                       TO  SPA-NAI-TKSKJKBNHON(IDX-MAX)
               MOVE    HKN-TSY-NYUSKJRYYKBN
                                       TO  SPA-NAI-TKSKJKBNTSY(IDX-MAX)
               MOVE    HKN-TNK-NYUSKJRYYKBN
                                       TO  SPA-NAI-TKSKJKBNTNK(IDX-MAX)
               MOVE    SYS-2010-STYUKYMD
                                       TO  SPA-NAI-TKYUKOSTYMD(IDX-MAX)
               MOVE    SYS-2010-STYUKYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TKYUKOSTYMD(IDX-MAX)
      *
               MOVE    SYS-2010-EDYUKYMD
                                       TO  SPA-NAI-TKYUKOEDYMD(IDX-MAX)
               MOVE    SYS-2010-EDYUKYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TKYUKOEDYMD(IDX-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-2010-REC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3110-KOHLST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       312-SPASET-SEC              SECTION.
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-KOHLST-MAX )
                    OR   ( IDX-LST >   ZERO )
      *
               IF    ( SPA-GMN-TKKOHNUM (IDX0)
                                   =   SPA-W01-KBNCD )
                   MOVE    IDX0    TO  IDX-LST
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX-LST >   ZERO )
               MOVE    IDX-LST     TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9    TO  W65-SELNUM1
               PERFORM 4100-SELNUM1-CHK-SEC
               MOVE    101     TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-W01-KBNCD
                                   TO  WRK-CMB-CD
               PERFORM 4200-KOHNUMLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    SPA-W01-KBNCD
                                   TO  SPA-GMN-KOHNUM-G
                   PERFORM 4102-KIHON-CHK-SEC
                   MOVE    101     TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-PAGENO
           MOVE    SPACE           TO  SPA-ERRCD
      *
           .
       312-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "2001"
                       PERFORM 421-SAKUJO-SEC
               WHEN "3001"
                       PERFORM 42021-HENKOU-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    追加
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 420-TUIKA-SEC
      *    変更
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 420-HENKOU-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 420-SAKUJO-SEC
      *    基準日
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    6           TO      SPA-GMN-CUR
      *
      *
      *    タブ切替
           WHEN    "CLICKED"       ALSO    "NPAGE00"
              MOVE    0            TO      WRK-PAGENO
              PERFORM 420-SWITCH-PAGE-SEC
           WHEN    "CLICKED"       ALSO    "NPAGE01"
              MOVE    1            TO      WRK-PAGENO
              PERFORM 420-SWITCH-PAGE-SEC
           WHEN    "CLICKED"       ALSO    "NPAGE02"
              MOVE    2            TO      WRK-PAGENO
              PERFORM 420-SWITCH-PAGE-SEC
           WHEN    "CLICKED"       ALSO    "NPAGE03"
              MOVE    3            TO      WRK-PAGENO
              PERFORM 420-SWITCH-PAGE-SEC
           WHEN    "CLICKED"       ALSO    "B09"
              COMPUTE WRK-PAGENO
                  =   SPA-GMN-PAGENO   +   1
              PERFORM 420-SWITCH-PAGE-SEC
      *
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    基準日
           WHEN    ANY             ALSO    "KJNYMD"
               PERFORM 4100-KJNYMD-CHK-SEC
      *    選択番号１
           WHEN    "ACTIVATE"      ALSO    "SELNUM1"
               PERFORM 4100-SELNUM1-CHK-SEC
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM2"
               PERFORM 4100-SELNUM2-CHK-SEC
      *    公費リスト
           WHEN    ANY             ALSO    "KOHLST"
               PERFORM 4100-KOHLST-CHK-SEC
      *    履歴リスト
           WHEN    ANY             ALSO    "RIREKILST"
               PERFORM 4100-RIREKILST-CHK-SEC
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       411-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4103-KIHON-CHK-SEC
      *
           .
      *
       411-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    W65-KOHNUM          TO  SPA-GMN-KOHNUM-G
           MOVE    W65-JNISSUKBN       TO  SPA-GMN-JNISSUKBN-G
           MOVE    W65-JNISSUTAIKBN    TO  SPA-GMN-JNISSUTAIKBN-G
           MOVE    W65-TOTALTENKBN     TO  SPA-GMN-TOTALTENKBN-G
           MOVE    W65-TOTALTENTAIKBN  TO  SPA-GMN-TOTALTENTAIKBN-G
           MOVE    W65-SYUKEIKBN       TO  SPA-GMN-SYUKEIKBN-G
           MOVE    W65-SYUKEITAIKBN    TO  SPA-GMN-SYUKEITAIKBN-G
           MOVE    W65-FTNKEIKBNG      TO  SPA-GMN-FTNKEIKBNG-G
           MOVE    W65-FTNKEITAIKBNG   TO  SPA-GMN-FTNKEITAIKBNG-G
           MOVE    W65-FTNKEIKBNN      TO  SPA-GMN-FTNKEIKBNN-G
           MOVE    W65-FTNKEITAIKBNN   TO  SPA-GMN-FTNKEITAIKBNN-G
           MOVE    W65-FTNMONEYKBNG    TO  SPA-GMN-FTNMONEYKBNG-G
           MOVE    W65-FTNMONEYTAIKBNG TO  SPA-GMN-FTNMONEYTAIKBNG-G
           MOVE    W65-FTNMONEYKBNN    TO  SPA-GMN-FTNMONEYKBNN-G
           MOVE    W65-FTNMONEYTAIKBNN TO  SPA-GMN-FTNMONEYTAIKBNN-G
           MOVE    W65-FTNZEROKBNG     TO  SPA-GMN-FTNZEROKBNG-G
           MOVE    W65-FTNZEROTAIKBNG  TO  SPA-GMN-FTNZEROTAIKBNG-G
           MOVE    W65-FTNZEROKBNN     TO  SPA-GMN-FTNZEROKBNN-G
           MOVE    W65-FTNZEROTAIKBNN  TO  SPA-GMN-FTNZEROTAIKBNN-G
           MOVE    W65-SKJKBN          TO  SPA-GMN-SKJKBN-G
           MOVE    W65-SKJTAIKBN       TO  SPA-GMN-SKJTAIKBN-G
           MOVE    W65-OCRKBN          TO  SPA-GMN-OCRKBN-G
           MOVE    W65-OCRTAIKBN       TO  SPA-GMN-OCRTAIKBN-G
           MOVE    W65-SAIKEIKBN       TO  SPA-GMN-SAIKEIKBN-G
           MOVE    W65-SAIKEITAIKBN    TO  SPA-GMN-SAIKEITAIKBN-G
           MOVE    W65-HKNFTNKBNK      TO  SPA-GMN-HKNFTNKBNK-G
           MOVE    W65-HKNFTNTAIKBNK   TO  SPA-GMN-HKNFTNTAIKBNK-G
           MOVE    W65-HKNFTNKBNY      TO  SPA-GMN-HKNFTNKBNY-G
           MOVE    W65-HKNFTNTAIKBNY   TO  SPA-GMN-HKNFTNTAIKBNY-G
           MOVE    W65-RECEKSIKBNG     TO  SPA-GMN-RECEKSIKBNG-G
           MOVE    W65-RECEKSITAIKBNG  TO  SPA-GMN-RECEKSITAIKBNG-G
           MOVE    W65-RECEKSIKBNN     TO  SPA-GMN-RECEKSIKBNN-G
           MOVE    W65-RECEKSITAIKBNN  TO  SPA-GMN-RECEKSITAIKBNN-G
           MOVE    W65-CHKJGNKBNG      TO  SPA-GMN-CHKJGNKBNG-G
           MOVE    W65-CHKJGNTAIKBNG   TO  SPA-GMN-CHKJGNTAIKBNG-G
           MOVE    W65-CHKJGNKBNN      TO  SPA-GMN-CHKJGNKBNN-G
           MOVE    W65-CHKJGNTAIKBNN   TO  SPA-GMN-CHKJGNTAIKBNN-G
           MOVE    W65-KOHSKYKBNG      TO  SPA-GMN-KOHSKYKBNG-G
           MOVE    W65-KOHSKYTAIKBNG   TO  SPA-GMN-KOHSKYTAIKBNG-G
           MOVE    W65-KOHSKYKBNN      TO  SPA-GMN-KOHSKYKBNN-G
           MOVE    W65-KOHSKYTAIKBNN   TO  SPA-GMN-KOHSKYTAIKBNN-G
           MOVE    W65-JYUHENKBN       TO  SPA-GMN-JYUHENKBN-G
           MOVE    W65-JYUHENTAIKBN    TO  SPA-GMN-JYUHENTAIKBN-G
           MOVE    W65-JGNFTNKBN       TO  SPA-GMN-JGNFTNKBN-G
           MOVE    W65-PTJGNKBN        TO  SPA-GMN-PTJGNKBN-G
           MOVE    W65-PTJGNTAIKBN     TO  SPA-GMN-PTJGNTAIKBN-G
           MOVE    W65-IRYOKEIKBNG     TO  SPA-GMN-IRYOKEIKBNG-G
           MOVE    W65-IRYOKEITAIKBNG  TO  SPA-GMN-IRYOKEITAIKBNG-G
           MOVE    W65-IRYOKEIKBNN     TO  SPA-GMN-IRYOKEIKBNN-G
           MOVE    W65-IRYOKEITAIKBNN  TO  SPA-GMN-IRYOKEITAIKBNN-G
           MOVE    W65-SKJKEIKBN       TO  SPA-GMN-SKJKEIKBN-G
           MOVE    W65-SKJKEITAIKBN    TO  SPA-GMN-SKJKEITAIKBN-G
           MOVE    W65-SKJRYOKBNHON    TO  SPA-GMN-SKJRYOKBNHON-G
           MOVE    W65-SKJRYOTAIKBNHON TO  SPA-GMN-SKJRYOTAIKBNHON-G
           MOVE    W65-SEIRYOKBNHON    TO  SPA-GMN-SEIRYOKBNHON-G
           MOVE    W65-SEIRYOTAIKBNHON TO  SPA-GMN-SEIRYOTAIKBNHON-G
           MOVE    W65-SEIKANKBNHON    TO  SPA-GMN-SEIKANKBNHON-G
           MOVE    W65-SEIKANTAIKBNHON TO  SPA-GMN-SEIKANTAIKBNHON-G
           MOVE    W65-SKJRYOKBNTSY    TO  SPA-GMN-SKJRYOKBNTSY-G
           MOVE    W65-SKJRYOTAIKBNTSY TO  SPA-GMN-SKJRYOTAIKBNTSY-G
           MOVE    W65-SEIRYOKBNTSY    TO  SPA-GMN-SEIRYOKBNTSY-G
           MOVE    W65-SEIRYOTAIKBNTSY TO  SPA-GMN-SEIRYOTAIKBNTSY-G
           MOVE    W65-SEIKANKBNTSY    TO  SPA-GMN-SEIKANKBNTSY-G
           MOVE    W65-SEIKANTAIKBNTSY TO  SPA-GMN-SEIKANTAIKBNTSY-G
           MOVE    W65-SKJRYOKBNTNK    TO  SPA-GMN-SKJRYOKBNTNK-G
           MOVE    W65-SKJRYOTAIKBNTNK TO  SPA-GMN-SKJRYOTAIKBNTNK-G
           MOVE    W65-SEIRYOKBNTNK    TO  SPA-GMN-SEIRYOKBNTNK-G
           MOVE    W65-SEIRYOTAIKBNTNK TO  SPA-GMN-SEIRYOTAIKBNTNK-G
           MOVE    W65-SEIKANKBNTNK    TO  SPA-GMN-SEIKANKBNTNK-G
           MOVE    W65-SEIKANTAIKBNTNK TO  SPA-GMN-SEIKANTAIKBNTNK-G
           MOVE    W65-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    W65-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           PERFORM 41021-KIHON-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE00-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE01-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE02-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE03-KIHON-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       41021-KIHON-CHK-SEC             SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SELNUM1-CHK-SEC
           END-IF
      *
           MOVE    SPA-NAI-YUKOSTYMD   TO  WRK-NAI-YUKOSTYMD
           MOVE    SPA-NAI-YUKOEDYMD   TO  WRK-NAI-YUKOEDYMD
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-YUKOSTYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-YUKOEDYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-NAI-YUKOSTYMD   NOT =   WRK-NAI-YUKOSTYMD )
            OR   ( SPA-NAI-YUKOEDYMD   NOT =   WRK-NAI-YUKOEDYMD )
               PERFORM 3110-KOHNUMLST-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHNUM-CHK-SEC
           END-IF
      *
           .
       41021-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4103-KIHON-CHK-SEC              SECTION.
      *
           PERFORM 41021-KIHON-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE00-KIHON-CHK-SEC
               PERFORM 4102-PAGE00-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE01-KIHON-CHK-SEC
               PERFORM 4102-PAGE01-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE02-KIHON-CHK-SEC
               PERFORM 4102-PAGE02-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE03-KIHON-CHK-SEC
               PERFORM 4102-PAGE03-KANREN-CHK-SEC
           END-IF
      *
           .
      *
       4103-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（０頁）
      *****************************************************************
       4102-PAGE00-KIHON-CHK-SEC       SECTION.
      *
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JNISSUKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JNISSUTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TOTALTENKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TOTALTENTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SYUKEIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SYUKEITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNKEIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNKEITAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNKEIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNKEITAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNMONEYKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNMONEYTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNMONEYKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNMONEYTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNZEROKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNZEROTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNZEROKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNZEROTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-OCRKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-OCRTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SAIKEIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SAIKEITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    0           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE00-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（１頁）
      *****************************************************************
       4102-PAGE01-KIHON-CHK-SEC       SECTION.
      *
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HKNFTNKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HKNFTNTAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HKNFTNKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HKNFTNTAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RECEKSIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RECEKSITAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RECEKSIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RECEKSITAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-CHKJGNKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-CHKJGNTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-CHKJGNKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-CHKJGNTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHSKYKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHSKYTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHSKYKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHSKYTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    1           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE01-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（２頁）
      *****************************************************************
       4102-PAGE02-KIHON-CHK-SEC       SECTION.
      *
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JYUHENKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JYUHENTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JGNFTNKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-PTJGNKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-PTJGNTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-IRYOKEIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-IRYOKEITAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-IRYOKEIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-IRYOKEITAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJKEIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJKEITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    2           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE02-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（３頁）
      *****************************************************************
       4102-PAGE03-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOTAIKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOTAIKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANTAIKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOTAIKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOTAIKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANTAIKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOTAIKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOTAIKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANTAIKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    3           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE03-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（０頁）
      *****************************************************************
       4102-PAGE00-KANREN-CHK-SEC       SECTION.
      *
      *
           .
       4102-PAGE00-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（１頁）
      *****************************************************************
       4102-PAGE01-KANREN-CHK-SEC       SECTION.
      *
      *
           .
       4102-PAGE01-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（２頁）
      *****************************************************************
       4102-PAGE02-KANREN-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-IRYOKEIKBNN     =   "0" )
               IF    ( SPA-GMN-SKJKEIKBN   =   "0" )
                   CONTINUE
               ELSE
                       MOVE    "0007"      TO  SPA-ERRCD
               END-IF
           ELSE
               IF    ( SPA-GMN-SKJKEIKBN   NOT =   "0" )
                   IF    ( SPA-GMN-IRYOKEITAIKBNN
                                   =   SPA-GMN-SKJKEITAIKBN )
                       CONTINUE
                   ELSE
                       MOVE    "0008"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4102-PAGE02-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（３頁）
      *****************************************************************
       4102-PAGE03-KANREN-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-NAI-INPUT-MODE
           WHEN    1
               MOVE    SPA-NAI-SKJKBNHON       TO  WRK-SKJKBN(1)
               MOVE    SPA-NAI-SKJKBNTSY       TO  WRK-SKJKBN(2)
               MOVE    SPA-NAI-SKJKBNTNK       TO  WRK-SKJKBN(3)
                PERFORM 41021-PAGE03-KANREN-CHK-SEC
           WHEN    2
               PERFORM VARYING IDX-LST FROM    1   BY  1
                       UNTIL ( IDX-LST >   SPA-GMN-KOHLST-MAX )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( SPA-NAI-SELNUM1-FLG (IDX-LST)   =   1 )
                       MOVE    SPA-NAI-TKSKJKBNHON (IDX-LST)
                                               TO  WRK-SKJKBN(1)
                       MOVE    SPA-NAI-TKSKJKBNTSY (IDX-LST)
                                               TO  WRK-SKJKBN(2)
                       MOVE    SPA-NAI-TKSKJKBNTNK (IDX-LST)
                                               TO  WRK-SKJKBN(3)
                       PERFORM 41021-PAGE03-KANREN-CHK-SEC
                   END-IF
      *
               END-PERFORM
           END-EVALUATE
      *
           .
       4102-PAGE03-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（３頁）
      *****************************************************************
       41021-PAGE03-KANREN-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO      FLG-ERR
           MOVE    SPACE           TO      WRK-TABNAME
      *
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >       3 )
                    OR   ( FLG-ERR NOT =   ZERO )
      *
               IF    ( WRK-SKJKBN (IDXE)   =   "0" OR "2" OR "4" OR "5")
                   IF    ( SPA-GMN-SKJRYOKBN (IDXE)  NOT =   "0" )
                    OR   ( SPA-GMN-SEIRYOKBN (IDXE)  NOT =   "0" )
                    OR   ( SPA-GMN-SEIKANKBN (IDXE)  NOT =   "0" )
                       MOVE    IDXE    TO  FLG-ERR
                   END-IF
               END-IF
      *
               IF    ( WRK-SKJKBN (IDXE) = "1" OR "3" )
                   IF    ( SPA-GMN-SKJRYOKBN(IDXE) =   "1" )
                    AND  ( SPA-GMN-SEIRYOKBN(IDXE) =   "1" )
                    AND  ( SPA-GMN-SEIKANKBN(IDXE) =   "1" )
                       COMPUTE FLG-ERR
                           =   IDXE    +   10
                   END-IF
                   IF    ( SPA-GMN-SKJRYOKBN(IDXE) =   "3" )
                    AND  ( SPA-GMN-SEIRYOKBN(IDXE) =   "3" )
                    AND  ( SPA-GMN-SEIKANKBN(IDXE) =   "3" )
                       COMPUTE FLG-ERR
                           =   IDXE    +   10
                   END-IF
               END-IF
           END-PERFORM
      *
      *
           EVALUATE    FLG-ERR
           WHEN    1   THRU  3
               MOVE    "0009"          TO  SPA-ERRCD
           WHEN    11  THRU  13
               MOVE    "0010"          TO  SPA-ERRCD
           END-EVALUATE
      *
           EVALUATE    FLG-ERR
           WHEN    1
           WHEN    11
               MOVE    "本人タブ"      TO  WRK-TABNAME
               MOVE    401             TO  SPA-GMN-CUR
           WHEN    2
           WHEN    12
               MOVE    "低所得タブ"    TO  WRK-TABNAME
               MOVE    407             TO  SPA-GMN-CUR
           WHEN    3
           WHEN    13
               MOVE    "低年金タブ"    TO  WRK-TABNAME
               MOVE    413             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       41021-PAGE03-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W65"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM 311-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    追加処理
      *****************************************************************
       420-TUIKA-SEC               SECTION.
      *
           PERFORM 411-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
      *    既存の履歴データをチェックし、期間の重複したデータが
      *    存在しないこと
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    1               TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               IF   ( FLG-KIKAN-DABURI NOT =   ZERO )
                   MOVE    "1104"          TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4201-SYS2010-INSERT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
               IF    ( SPA-GMN-KOHLST-MAX  >=  CONST-KOHLST-MAX )
                   MOVE    "1001"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           .
       420-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    変更処理
      *****************************************************************
       420-HENKOU-SEC                  SECTION.
      *
           PERFORM 411-INPUT-CHK-SEC
      *
      *    選択番号が入力されていること
           IF    ( SPA-GMN-SELNUM1 =   SPACE )
               MOVE    "1105"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   PERFORM 4202-HENKOU-SEC
               ELSE
                   PERFORM 4201-HENKOU-SEC
      *
                   IF    ( SPA-ERRCD       =   SPACE )
                        PERFORM 311-SPASET-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       420-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(通常の変更処理)
      *****************************************************************
       4201-HENKOU-SEC                 SECTION.
      *
      *    公費番号が変更されていないこと
           IF    ( SPA-GMN-KOHNUM  =   SPA-NAI-KOHNUM-ORG )
               CONTINUE
           ELSE
               MOVE    "1106"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        有効期間の開始日と終了日が同時に変更されていないこと
               IF    ( SPA-NAI-YUKOSTYMD   NOT = SPA-NAI-YUKOSTYMD-ORG )
                AND  ( SPA-NAI-YUKOEDYMD   NOT = SPA-NAI-YUKOEDYMD-ORG )
                   MOVE    "1107"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        既存の履歴データをチェックし、期間の重複したデータが
      *        存在しないこと
               MOVE    2                   TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               IF   ( FLG-KIKAN-DABURI NOT =   ZERO )
                   MOVE    "1104"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4201-SYS2010-UPDATE-SEC
           END-IF
      *
           .
       4201-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(一括変更)
      *****************************************************************
       4202-HENKOU-SEC                 SECTION.
      *
           MOVE    "3001"          TO  SPA-WIDCD
      *
           .
       4202-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(一括変更)
      *****************************************************************
       42021-HENKOU-SEC                SECTION.
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-KOHLST-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF  ( SPA-NAI-SELNUM1-FLG (IDX-LST) =   1 )
      *
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "2010"          TO  SYS-KANRICD
                   MOVE    SPA-GMN-TKKOHNUM (IDX-LST)
                                           TO  SYS-KBNCD
                   MOVE    SPA-NAI-TKYUKOSTYMD (IDX-LST)
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    SYSKANRI-REC    TO  SYS-2010-REC
      *
      *                公費記載情報編集処理
                       PERFORM 4201-KOHINF-EDIT-SEC
      *
                       MOVE    SYS-2010-REC    TO  SYSKANRI-REC
                       PERFORM 800-MCNDATE-SEC
                       MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
                       MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                       MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"  TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBUPDATE-SEC
                       IF   (  MCP-RC          NOT =   ZERO )
                           MOVE    "4005"      TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
           END-IF
      *
      *
           .
       42021-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    公費基本情報編集処理
      *****************************************************************
       4201-SYS2010-EDIT-SEC            SECTION.
      *
           MOVE    SPACE                  TO  SYS-2010-REC
           INITIALIZE                         SYS-2010-REC
           MOVE    SPA-HOSPNUM            TO  SYS-2010-HOSPNUM
           MOVE    "2010"                 TO  SYS-2010-KANRICD
           MOVE    SPA-GMN-KOHNUM         TO  SYS-2010-KBNCD
           MOVE    SPA-NAI-YUKOSTYMD      TO  SYS-2010-STYUKYMD
           MOVE    SPA-NAI-YUKOEDYMD      TO  SYS-2010-EDYUKYMD
      *
           PERFORM 4201-KOHINF-EDIT-SEC
      *
           .
       4201-SYS2010-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公費記載情報編集処理
      *****************************************************************
       4201-KOHINF-EDIT-SEC            SECTION.
      *
           MOVE    SPA-GMN-JNISSUKBN       TO SYS-2010-JNISSUKBN
           MOVE    SPA-GMN-JNISSUTAIKBN    TO SYS-2010-JNISSU-TAIKBN
           MOVE    SPA-GMN-TOTALTENKBN     TO SYS-2010-TOTALTENKBN
           MOVE    SPA-GMN-TOTALTENTAIKBN  TO SYS-2010-TOTALTEN-TAIKBN
           MOVE    SPA-GMN-SYUKEIKBN       TO SYS-2010-SYUKEIKBN
           MOVE    SPA-GMN-SYUKEITAIKBN    TO SYS-2010-SYUKEI-TAIKBN
           MOVE    SPA-GMN-FTNKEIKBNG      TO SYS-2010-FTNKEIKBN-G
           MOVE    SPA-GMN-FTNKEITAIKBNG   TO SYS-2010-FTNKEI-TAIKBN-G
           MOVE    SPA-GMN-FTNKEIKBNN      TO SYS-2010-FTNKEIKBN-N
           MOVE    SPA-GMN-FTNKEITAIKBNN   TO SYS-2010-FTNKEI-TAIKBN-N
           MOVE    SPA-GMN-FTNMONEYKBNG    TO SYS-2010-FTNMONEYKBN-G
           MOVE    SPA-GMN-FTNMONEYTAIKBNG TO SYS-2010-FTNMONEY-TAIKBN-G
           MOVE    SPA-GMN-FTNMONEYKBNN    TO SYS-2010-FTNMONEYKBN-N
           MOVE    SPA-GMN-FTNMONEYTAIKBNN TO SYS-2010-FTNMONEY-TAIKBN-N
           MOVE    SPA-GMN-FTNZEROKBNG     TO SYS-2010-FTNZEROKBN-G
           MOVE    SPA-GMN-FTNZEROTAIKBNG  TO SYS-2010-FTNZERO-TAIKBN-G
           MOVE    SPA-GMN-FTNZEROKBNN     TO SYS-2010-FTNZEROKBN-N
           MOVE    SPA-GMN-FTNZEROTAIKBNN  TO SYS-2010-FTNZERO-TAIKBN-N
           MOVE    SPA-GMN-SKJKBN          TO SYS-2010-SKJKBN
           MOVE    SPA-GMN-SKJTAIKBN       TO SYS-2010-SKJ-TAIKBN
           MOVE    SPA-GMN-OCRKBN          TO SYS-2010-OCRKBN
           MOVE    SPA-GMN-OCRTAIKBN       TO SYS-2010-OCR-TAIKBN
           MOVE    SPA-GMN-JYUHENKBN       TO SYS-2010-JYUHENKBN
           MOVE    SPA-GMN-JYUHENTAIKBN    TO SYS-2010-JYUHEN-TAIKBN
           MOVE    SPA-GMN-JGNFTNKBN       TO SYS-2010-JGNFTNKBN
           MOVE    "0"                     TO SYS-2010-JGNFTN-TAIKBN
           MOVE    SPA-GMN-SAIKEIKBN       TO SYS-2010-SAIKEIKBN
           MOVE    SPA-GMN-SAIKEITAIKBN    TO SYS-2010-SAIKEI-TAIKBN
           MOVE    SPA-GMN-HKNFTNKBNK      TO SYS-2010-HKNFTNKBN-K
           MOVE    SPA-GMN-HKNFTNTAIKBNK   TO SYS-2010-HKNFTN-TAIKBN-K
           MOVE    SPA-GMN-HKNFTNKBNY      TO SYS-2010-HKNFTNKBN-Y
           MOVE    SPA-GMN-HKNFTNTAIKBNY   TO SYS-2010-HKNFTN-TAIKBN-Y
           MOVE    SPA-GMN-RECEKSIKBNG     TO SYS-2010-RECEKSIKBN-G
           MOVE    SPA-GMN-RECEKSITAIKBNG  TO SYS-2010-RECEKSI-TAIKBN-G
           MOVE    SPA-GMN-RECEKSIKBNN     TO SYS-2010-RECEKSIKBN-N
           MOVE    SPA-GMN-RECEKSITAIKBNN  TO SYS-2010-RECEKSI-TAIKBN-N
           MOVE    SPA-GMN-CHKJGNKBNG      TO SYS-2010-CHKJGNKBN-G
           MOVE    SPA-GMN-CHKJGNTAIKBNG   TO SYS-2010-CHKJGN-TAIKBN-G
           MOVE    SPA-GMN-CHKJGNKBNN      TO SYS-2010-CHKJGNKBN-N
           MOVE    SPA-GMN-CHKJGNTAIKBNN   TO SYS-2010-CHKJGN-TAIKBN-N
           MOVE    SPA-GMN-KOHSKYKBNG      TO SYS-2010-KOHSKYKBN-G
           MOVE    SPA-GMN-KOHSKYTAIKBNG   TO SYS-2010-KOHSKY-TAIKBN-G
           MOVE    SPA-GMN-KOHSKYKBNN      TO SYS-2010-KOHSKYKBN-N
           MOVE    SPA-GMN-KOHSKYTAIKBNN   TO SYS-2010-KOHSKY-TAIKBN-N
           MOVE    SPA-GMN-PTJGNKBN        TO SYS-2010-PTJGNKBN
           MOVE    SPA-GMN-PTJGNTAIKBN     TO SYS-2010-PTJGN-TAIKBN
           MOVE    SPA-GMN-IRYOKEIKBNG     TO SYS-2010-IRYOKEIKBN-G
           MOVE    SPA-GMN-IRYOKEITAIKBNG  TO SYS-2010-IRYOKEI-TAIKBN-G
           MOVE    SPA-GMN-IRYOKEIKBNN     TO SYS-2010-IRYOKEIKBN-N
           MOVE    SPA-GMN-IRYOKEITAIKBNN  TO SYS-2010-IRYOKEI-TAIKBN-N
           MOVE    SPA-GMN-SKJKEIKBN       TO SYS-2010-SKJKEIKBN
           MOVE    SPA-GMN-SKJKEITAIKBN    TO SYS-2010-SKJKEI-TAIKBN
           MOVE    SPA-GMN-SKJRYOKBNHON    TO SYS-2010-SKJRYOKBN-HON
           MOVE    SPA-GMN-SKJRYOTAIKBNHON TO SYS-2010-SKJRYO-TAIKBN-HON
           MOVE    SPA-GMN-SEIRYOKBNHON    TO SYS-2010-SEIRYOKBN-HON
           MOVE    SPA-GMN-SEIRYOTAIKBNHON TO SYS-2010-SEIRYO-TAIKBN-HON
           MOVE    SPA-GMN-SEIKANKBNHON    TO SYS-2010-SEIKANKBN-HON
           MOVE    SPA-GMN-SEIKANTAIKBNHON TO SYS-2010-SEIKAN-TAIKBN-HON
           MOVE    SPA-GMN-SKJRYOKBNTSY    TO SYS-2010-SKJRYOKBN-TSY
           MOVE    SPA-GMN-SKJRYOTAIKBNTSY TO SYS-2010-SKJRYO-TAIKBN-TSY
           MOVE    SPA-GMN-SEIRYOKBNTSY    TO SYS-2010-SEIRYOKBN-TSY
           MOVE    SPA-GMN-SEIRYOTAIKBNTSY TO SYS-2010-SEIRYO-TAIKBN-TSY
           MOVE    SPA-GMN-SEIKANKBNTSY    TO SYS-2010-SEIKANKBN-TSY
           MOVE    SPA-GMN-SEIKANTAIKBNTSY TO SYS-2010-SEIKAN-TAIKBN-TSY
           MOVE    SPA-GMN-SKJRYOKBNTNK    TO SYS-2010-SKJRYOKBN-TNK
           MOVE    SPA-GMN-SKJRYOTAIKBNTNK TO SYS-2010-SKJRYO-TAIKBN-TNK
           MOVE    SPA-GMN-SEIRYOKBNTNK    TO SYS-2010-SEIRYOKBN-TNK
           MOVE    SPA-GMN-SEIRYOTAIKBNTNK TO SYS-2010-SEIRYO-TAIKBN-TNK
           MOVE    SPA-GMN-SEIKANKBNTNK    TO SYS-2010-SEIKANKBN-TNK
           MOVE    SPA-GMN-SEIKANTAIKBNTNK TO SYS-2010-SEIKAN-TAIKBN-TNK
      *
           .
       4201-KOHINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       420-SAKUJO-SEC                  SECTION.
      *
           PERFORM 4101-SELNUM1-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               CONTINUE
           ELSE
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SELNUM1 =   SPACE )
                   MOVE    "1105"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "2001"      TO  SPA-WIDCD
           END-IF
      *
           .
       420-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    タブ切替処理
      *****************************************************************
       420-SWITCH-PAGE-SEC             SECTION.
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               EVALUATE    SPA-GMN-PAGENO
               WHEN    0
                   PERFORM 4102-PAGE00-KANREN-CHK-SEC
               WHEN    1
                   PERFORM 4102-PAGE01-KANREN-CHK-SEC
               WHEN    2
                   PERFORM 4102-PAGE02-KANREN-CHK-SEC
               WHEN    3
                   PERFORM 4102-PAGE03-KANREN-CHK-SEC
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    WRK-PAGENO      TO  SPA-GMN-PAGENO
               IF    ( SPA-GMN-PAGENO  >   3 )
                   MOVE    0           TO  SPA-GMN-PAGENO
               END-IF
           END-IF
      *
           .
       420-SWITCH-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       421-SAKUJO-SEC                  SECTION.
      *
           PERFORM  4201-SYS2010-DELETE-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
           END-IF
      *
           .
       421-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    履歴チェック処理
      *****************************************************************
       4100-RIREKI-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-KIKAN-DABURI
           MOVE    SPACE           TO  WRK-ERRSTYMD
                                       WRK-ERREDYMD
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2010"          TO  SYS-KANRICD
           MOVE    SPA-GMN-KOHNUM  TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-2010-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( FLG-KIKAN-DABURI    =   1 )
      *
      *        有効期間が重複するデータがないかチェック
      *        （変更処理の場合、元データは対象外とする）
               IF    ( FLG-UPD-KBN           =  2 )
                AND  ( SPA-NAI-KOHNUM-ORG    =   SYS-2010-KBNCD  )
                AND  ( SPA-NAI-YUKOSTYMD-ORG =   SYS-2010-STYUKYMD )
                AND  ( SPA-NAI-YUKOEDYMD-ORG =   SYS-2010-EDYUKYMD )
                   CONTINUE
               ELSE
                   IF    ( SPA-NAI-YUKOEDYMD <   SYS-2010-STYUKYMD )
                     OR  ( SPA-NAI-YUKOSTYMD >   SYS-2010-EDYUKYMD )
                       CONTINUE
                   ELSE
                       MOVE    SYS-2010-STYUKYMD TO    WRK-YMD
                       PERFORM 800-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG       TO    WRK-ERRSTYMD
                       MOVE    SYS-2010-EDYUKYMD TO    WRK-YMD
                       PERFORM 800-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG       TO    WRK-ERREDYMD
                       MOVE    1             TO  FLG-KIKAN-DABURI
                   END-IF
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-2010-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       4100-RIREKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報更新処理
      *****************************************************************
       4201-SYS2010-UPDATE-SEC         SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2010"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KOHNUM-ORG
                                   TO  SYS-KBNCD
           MOVE    SPA-NAI-YUKOSTYMD-ORG
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI     =   ZERO )
      *
               PERFORM 4201-SYS2010-EDIT-SEC
      *
               MOVE    SYS-2010-REC        TO  SYSKANRI-REC
               MOVE    SYS-2010-HOSPNUM    TO  SYSUP-HOSPNUM
               MOVE    SYS-2010-KANRICD    TO  SYSUP-KANRICD
               MOVE    SPA-NAI-KOHNUM-ORG  TO  SYSUP-KBNCD
               MOVE    SPA-NAI-YUKOSTYMD-ORG
                                           TO  SYSUP-STYUKYMD
               MOVE    SPA-NAI-YUKOEDYMD-ORG
                                           TO  SYSUP-EDYUKYMD
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
      *
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF   (  MCP-RC          NOT =   ZERO )
                   MOVE    "4001"      TO  SPA-ERRCD
               END-IF
      *
           ELSE
                   MOVE    "4002"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4201-SYS2010-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報追加処理
      *****************************************************************
       4201-SYS2010-INSERT-SEC         SECTION.
      *
      *    公費付加情報編集処理
           PERFORM 4201-SYS2010-EDIT-SEC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-2010-CREYMD
                                       SYS-2010-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-2010-UPHMS
      *
           MOVE    SYS-2010-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    "4003"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4201-SYS2010-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報削除処理
      *****************************************************************
       4201-SYS2010-DELETE-SEC         SECTION.
      *
           INITIALIZE                      SYS-2010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-2010-HOSPNUM
           MOVE    "2010"              TO  SYS-2010-KANRICD
           MOVE    SPA-NAI-KOHNUM-ORG  TO  SYS-2010-KBNCD
           MOVE    SPA-NAI-YUKOSTYMD-ORG
                                       TO  SYS-2010-STYUKYMD
           MOVE    SPA-NAI-YUKOEDYMD-ORG
                                       TO  SYS-2010-EDYUKYMD
           MOVE    SYS-2010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    "4004"      TO  SPA-ERRCD
           END-IF
      *
           .
       4201-SYS2010-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    基準日
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           MOVE    ZERO        TO  SPA-GMN-CUR
      *
           MOVE    W65-KJNYMD  TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF  ( FLG-HIZUKECHK     =   ZERO )
      *
               INITIALIZE              SPA-W65
      *
               MOVE    WRK-HENYMDG TO  SPA-GMN-KJNYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-KJNYMD
      *
               PERFORM 311-SPASET-SEC
      *
           ELSE
               MOVE    "0014"      TO  SPA-ERRCD
               MOVE    6           TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１
      *****************************************************************
       4100-SELNUM1-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           INITIALIZE                  WRK-SELNUM-G
           MOVE    ZERO            TO  SPA-NAI-INPUT-MODE
      *
           MOVE    W65-SELNUM1     TO  SPA-GMN-SELNUM1
      *
           IF    ( SPA-GMN-SELNUM1 =  SPACE )
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
               PERFORM 4201-RIREKI-AREA-CLEAR-SEC
               MOVE    ZERO            TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-GMN-SELNUM1     TO  WRK-KANACHK-INPUT
               PERFORM 800-ENRTRY-CHK-SEC
               MOVE    WRK-KANACHK-INPUT   TO  SPA-GMN-SELNUM1
      *
               PERFORM 4101-SELNUM1-CHK-SEC
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 41001-SELNUM1-CHK-SEC
               ELSE
                   PERFORM 4201-INPUT-AREA-CLEAR-SEC
                   PERFORM 4201-RIREKI-AREA-CLEAR-SEC
                   MOVE    W65-SELNUM1     TO  SPA-GMN-SELNUM1
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１
      *****************************************************************
       41001-SELNUM1-CHK-SEC           SECTION.
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           PERFORM 4201-RIREKI-AREA-CLEAR-SEC
      *
           IF    ( WRK-INPUT-MODE      =   2 )
               MOVE    W65-SELNUM1     TO  SPA-GMN-SELNUM1
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-KOHLST-MAX )
                   MOVE    WRK-SELNUM-FLG (IDX0)
                                       TO  SPA-NAI-SELNUM1-FLG (IDX0)
               END-PERFORM
           END-IF
      *
           MOVE    WRK-BASESELNUM      TO  SPA-GMN-KOHLST-SEL
      *
           MOVE    WRK-INPUT-MODE      TO  SPA-NAI-INPUT-MODE
           MOVE    SPA-GMN-TKKOHNUM (WRK-BASESELNUM)
                                       TO  SEL-HKNNUM
           MOVE    SPA-NAI-KJNYMD      TO  SEL-KJNYMD
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
           IF    ( SPA-NAI-INPUT-MODE  =   1 )
               PERFORM 4100-RIREKI-AREA-EDIT-SEC
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
       41001-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１チェック処理
      *****************************************************************
       4101-SELNUM1-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-SELNUM1     =  SPACE )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-SELNUM1 (1:1)    =   "." )
                   MOVE    SPA-GMN-SELNUM1 (2:)
                                           TO  WRK-SELNUM-MUL
               ELSE
                   MOVE    SPA-GMN-SELNUM1 TO  WRK-SELNUM-MUL
               END-IF
      *
               PERFORM 41011-MULTI-SEL-CHK-SEC
           END-IF
      *
           .
      *
       4101-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複数行選択チェック処理
      *****************************************************************
       41011-MULTI-SEL-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-MULTI-AREA
      *
           MOVE    ZERO            TO  WRK-INPUT-MODE
                                       WRK-BASESELNUM
      *
           UNSTRING    WRK-SELNUM-MUL  DELIMITED   BY  ","
                                                   OR  "."
           INTO        WRK-MUL-NUM (1)
                       WRK-MUL-NUM (2)
                       WRK-MUL-NUM (3)
                       WRK-MUL-NUM (4)
                       WRK-MUL-NUM (5)
                       WRK-MUL-NUM (6)
                       WRK-MUL-NUM (7)
                       WRK-MUL-NUM (8)
                       WRK-MUL-NUM (9)
                       WRK-MUL-NUM (10)
                       WRK-MUL-NUM (11)
                       WRK-MUL-NUM (12)
                       WRK-MUL-NUM (13)
                       WRK-MUL-NUM (14)
                       WRK-MUL-NUM (15)
                       WRK-MUL-NUM (16)
                       WRK-MUL-NUM (17)
                       WRK-MUL-NUM (18)
                       WRK-MUL-NUM (19)
                       WRK-MUL-NUM (20)
           ON  OVERFLOW
               MOVE    1           TO  SPA-GMN-CUR
               MOVE    "0015"      TO  SPA-ERRCD  
               GO  TO  41011-MULTI-SEL-CHK-EXT
           END-UNSTRING
      *
           MOVE    ZERO            TO  CNT-SEL
                                       WRK-SELNUM
           PERFORM VARYING IDX-MUL    FROM    1   BY  1
                   UNTIL ( IDX-MUL    >       20 )
      *
               IF    ( WRK-MUL-NUM (IDX-MUL)  =   SPACE )
                   CONTINUE
               ELSE
      *
                   INITIALIZE  WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                               WRK-MUL-STNUM
                               WRK-MUL-EDNUM
      *
                   UNSTRING    WRK-MUL-NUM (IDX-MUL) DELIMITED   BY  "-"
                   INTO        WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                   END-UNSTRING
      *
                   IF    ( WRK-MUL-STNUM-X     =   SPACE )
                    AND  ( WRK-MUL-EDNUM-X     =   SPACE )
                       MOVE    1           TO  SPA-GMN-CUR
                       MOVE    "0001"      TO  SPA-ERRCD
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   ELSE
                       EVALUATE    TRUE
                       WHEN  ( WRK-MUL-EDNUM-X     =   SPACE )
                           MOVE    WRK-MUL-STNUM-X
                                           TO  WRK-MUL-EDNUM-X
                       WHEN  ( WRK-MUL-STNUM-X     =   SPACE )
                           MOVE    WRK-MUL-EDNUM-X
                                           TO  WRK-MUL-STNUM-X
                       END-EVALUATE
                   END-IF
      *
                   MOVE    WRK-MUL-STNUM-X     TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM  TO  WRK-MUL-STNUM
      *
                   MOVE    WRK-MUL-EDNUM-X TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM  TO  WRK-MUL-EDNUM
      *
      *
                   PERFORM VARYING IDX1    FROM    WRK-MUL-STNUM   BY  1
                           UNTIL ( IDX1    >       WRK-MUL-EDNUM )
                       IF    ( IDX1        >=  1 )
                        AND  ( IDX1        <=  SPA-GMN-KOHLST-MAX )
                           COMPUTE CNT-SEL  =   CNT-SEL  +   1
                           IF    ( CNT-SEL  =   1 )
                               MOVE    IDX1    TO  WRK-BASESELNUM
                           END-IF
                           MOVE    1       TO  WRK-SELNUM-FLG (IDX1)
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  CNT-SEL
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       SPA-GMN-KOHLST-MAX )
      *
               IF    ( WRK-SELNUM-FLG (IDX1)   =   1 )
                   COMPUTE CNT-SEL =   CNT-SEL +   1
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-SEL         >   1 )
               MOVE    2           TO  WRK-INPUT-MODE
           ELSE
               MOVE    1           TO  WRK-INPUT-MODE
           END-IF
      *
           .
       41011-MULTI-SEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号範囲チェック処理
      *****************************************************************
       4101-SELNUM-LIMIT-CHK-SEC       SECTION.
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    WRK-SELNUM-X    TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
            AND  ( SNUM-MINKBN     =   SPACE )
            AND  ( SNUM-SYOKBN     =   SPACE ) 
               MOVE    SNUM-OUTNUM TO  WRK-SELNUM
               IF    ( WRK-SELNUM  >=  1 )
                AND  ( WRK-SELNUM  <=  SPA-GMN-KOHLST-MAX )
                   CONTINUE
               ELSE
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
      *
           ELSE
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-SELNUM-LIMIT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力項目領域編集処理
      *****************************************************************
       4100-INPUT-AREA-EDIT-SEC        SECTION.
      *
      *    選択番号１
           IF    ( SPA-NAI-INPUT-MODE      =   1 )
               MOVE    SPA-GMN-KOHLST-SEL  TO  WRK-ZZZ
               IF    ( WRK-ZZZ-G (1:1)     =   SPACE )
                   UNSTRING    WRK-ZZZ-G   DELIMITED   BY  ALL SPACE
                   INTO        WRK-STR-TMP
                               SPA-GMN-SELNUM1
                   END-UNSTRING
               ELSE
                   MOVE    WRK-ZZZ-G   TO  SPA-GMN-SELNUM1
               END-IF
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2010"          TO  SYS-KANRICD
           MOVE    SEL-HKNNUM      TO  SYS-KBNCD
           MOVE    SEL-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-2010-REC
      *
           MOVE    SYS-2010-STYUKYMD
                                   TO  SPA-NAI-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD-ORG
                                       WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOSTYMD
      *
           MOVE    SYS-2010-EDYUKYMD
                                   TO  SPA-NAI-YUKOEDYMD
                                       SPA-NAI-YUKOEDYMD-ORG
                                       WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOEDYMD
      *
      *    公費の種類コンボ編集処理
           PERFORM 3110-KOHNUMLST-SEC
      *
           IF    ( SPA-NAI-INPUT-MODE  =   1 )
               MOVE    SYS-2010-KBNCD  TO  SPA-GMN-KOHNUM
                                           SPA-NAI-KOHNUM
                                           SPA-NAI-KOHNUM-ORG
           ELSE
               MOVE    SPACE           TO  SPA-GMN-KOHNUM
                                           SPA-NAI-KOHNUM
                                           SPA-NAI-KOHNUM-ORG
           END-IF
      *
           MOVE    SPA-GMN-KOHNUM  TO  WRK-CMB-CD
           PERFORM 4200-KOHNUMLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    WRK-CMB-CD  TO  SPA-GMN-KOHNUM-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-KOHNUM-G
           END-IF
      *
           MOVE    SYS-2010-JNISSUKBN  TO  WRK-CMB-CD
           PERFORM 4200-JNISSUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JNISSUKBN-G
      *
           MOVE    SYS-2010-JNISSU-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JNISSUTAIKBN-G
      *
           MOVE    SYS-2010-TOTALTENKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TOTALTENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TOTALTENKBN-G
      *
           MOVE    SYS-2010-TOTALTEN-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TOTALTENTAIKBN-G
      *
           MOVE    SYS-2010-SYUKEIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYUKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYUKEIKBN-G
           IF    ( WRK-CMB-ITM2       =   SPACE )
               CONTINUE
           ELSE
               MOVE    WRK-CMB-ITM2   TO  SYS-2010-SYUKEI-TAIKBN
           END-IF
      *
           MOVE    SYS-2010-SYUKEI-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYUKEITAIKBN-G
      *
           MOVE    SYS-2010-FTNKEIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNKEIKBNG-G
      *
           MOVE    SYS-2010-FTNKEI-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNKEITAIKBNG-G
      *
           MOVE    SYS-2010-FTNKEIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNKEIKBNN-G
      *
           MOVE    SYS-2010-FTNKEI-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNKEITAIKBNN-G
      
      *
           MOVE    SYS-2010-FTNMONEYKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNMONEYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNMONEYKBNG-G
      *
           MOVE    SYS-2010-FTNMONEY-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNMONEYTAIKBNG-G
      *
           MOVE    SYS-2010-FTNMONEYKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNMONEYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNMONEYKBNN-G
      *
           MOVE    SYS-2010-FTNMONEY-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNMONEYTAIKBNN-G
      *
           MOVE    SYS-2010-FTNZEROKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNZEROKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNZEROKBNG-G
      *
           MOVE    SYS-2010-FTNZERO-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNZEROTAIKBNG-G
      *
           MOVE    SYS-2010-FTNZEROKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNZEROKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNZEROKBNN-G
      *
           MOVE    SYS-2010-FTNZERO-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNZEROTAIKBNN-G
      *
           MOVE    SYS-2010-SKJKBN     TO  WRK-CMB-CD
           PERFORM 4200-SKJKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJKBN-G
      *
           MOVE    SYS-2010-SKJ-TAIKBN TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJTAIKBN-G
      *
           MOVE    SYS-2010-OCRKBN     TO  WRK-CMB-CD
           PERFORM 4200-OCRKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-OCRKBN-G
      *
           MOVE    SYS-2010-OCR-TAIKBN TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-OCRTAIKBN-G
      *
           MOVE    SYS-2010-SAIKEIKBN TO  WRK-CMB-CD
           PERFORM 4200-SAIKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SAIKEIKBN-G
      *
           MOVE    SYS-2010-SAIKEI-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SAIKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SAIKEITAIKBN-G
      *

           MOVE    SYS-2010-HKNFTNKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-HKNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNKBNK-G
      *
           MOVE    SYS-2010-HKNFTN-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNTAIKBNK-G
      *
           MOVE    SYS-2010-HKNFTNKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-HKNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNKBNY-G
      *
           MOVE    SYS-2010-HKNFTN-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNTAIKBNY-G
      *
           MOVE    SYS-2010-RECEKSIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-RECEKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSIKBNG-G
      *
           MOVE    SYS-2010-RECEKSI-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-RECEKSITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSITAIKBNG-G
      *
           MOVE    SYS-2010-RECEKSIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-RECEKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSIKBNN-G
      *
           MOVE    SYS-2010-RECEKSI-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-RECEKSITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSITAIKBNN-G
      *
           MOVE    SYS-2010-CHKJGNKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNKBNG-G
      *
           MOVE    SYS-2010-CHKJGN-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNTAIKBNG-G
      *
           MOVE    SYS-2010-CHKJGNKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNKBNN-G
      *
           MOVE    SYS-2010-CHKJGN-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNTAIKBNN-G
      *
           MOVE    SYS-2010-KOHSKYKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-KOHSKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYKBNG-G
      *
           MOVE    SYS-2010-KOHSKY-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYTAIKBNG-G
      *
           MOVE    SYS-2010-KOHSKYKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-KOHSKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYKBNN-G
      *
           MOVE    SYS-2010-KOHSKY-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYTAIKBNN-G
      *
           MOVE    SYS-2010-JYUHENKBN  TO  WRK-CMB-CD
           PERFORM 4200-JYUHENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JYUHENKBN-G
      *
           MOVE    SYS-2010-JYUHEN-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-JYUHENTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JYUHENTAIKBN-G
      *
           MOVE    SYS-2010-JGNFTNKBN  TO  WRK-CMB-CD
           PERFORM 4200-JGNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JGNFTNKBN-G
      *
           MOVE    SYS-2010-PTJGNKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-PTJGNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PTJGNKBN-G
      *
           MOVE    SYS-2010-PTJGN-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-PTJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PTJGNTAIKBN-G
      *
           MOVE    SYS-2010-IRYOKEIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEIKBNG-G
      *
           MOVE    SYS-2010-IRYOKEI-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEITAIKBNG-G
      *
           MOVE    SYS-2010-IRYOKEIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEIKBNN-G
      *
           MOVE    SYS-2010-IRYOKEI-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEITAIKBNN-G
      *
           MOVE    SYS-2010-SKJKEIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJKEIKBN-G
      *
           MOVE    SYS-2010-SKJKEI-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJKEITAIKBN-G
      *
           MOVE    SYS-2010-SKJRYOKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNHON-G
      *
           MOVE    SYS-2010-SKJRYO-TAIKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNHON-G
      *
           MOVE    SYS-2010-SEIRYOKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNHON-G
      *
           MOVE    SYS-2010-SEIRYO-TAIKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNHON-G
      *
           MOVE    SYS-2010-SEIKANKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNHON-G
      *
           MOVE    SYS-2010-SEIKAN-TAIKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNHON-G
      *
           MOVE    SYS-2010-SKJRYOKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNTSY-G
      *
           MOVE    SYS-2010-SKJRYO-TAIKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNTSY-G
      *
           MOVE    SYS-2010-SEIRYOKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNTSY-G
      *
           MOVE    SYS-2010-SEIRYO-TAIKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNTSY-G
      *
           MOVE    SYS-2010-SEIKANKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNTSY-G
      *
           MOVE    SYS-2010-SEIKAN-TAIKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNTSY-G
      *
           MOVE    SYS-2010-SKJRYOKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNTNK-G
      *
           MOVE    SYS-2010-SKJRYO-TAIKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNTNK-G
      *
           MOVE    SYS-2010-SEIRYOKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNTNK-G
      *
           MOVE    SYS-2010-SEIRYO-TAIKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNTNK-G
      *
           MOVE    SYS-2010-SEIKANKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNTNK-G
      *
           MOVE    SYS-2010-SEIKAN-TAIKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNTNK-G
      *
           MOVE    SYS-2010-STYUKYMD   TO  SEL-STYMD
           MOVE    SYS-2010-EDYUKYMD   TO  SEL-EDYMD
           MOVE    SYS-2010-KBNCD      TO  SEL-HKNNUM
           PERFORM 900-HKNNUM-KEY8-SEL-SEC
           MOVE    HKN-HON-NYUSKJRYYKBN
                                       TO  SPA-NAI-SKJKBNHON
           MOVE    HKN-TSY-NYUSKJRYYKBN
                                       TO  SPA-NAI-SKJKBNTSY
           MOVE    HKN-TNK-NYUSKJRYYKBN
                                       TO  SPA-NAI-SKJKBNTNK
      *
           IF    ( SYS-2010-FTNKEIKBN-G    =   SPACE )
               MOVE    HKNNUM-REC          TO  THKNNUM-REC
               PERFORM 42012-COMBO-SET-DEFAULT-SEC
           END-IF
      *
           .
       4100-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    履歴領域編集処理
      *****************************************************************
       4100-RIREKI-AREA-EDIT-SEC       SECTION.
      *
           INITIALIZE      SPA-GMN-RIREKILST-AREA
                           SPA-NAI-RIREKILST-AREA
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "2010"              TO  SYS-KANRICD
           MOVE    SPA-GMN-KOHNUM      TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-2010-REC
      *
           PERFORM VARYING IDX-MAX FROM    1   BY  1
               UNTIL     ( FLG-SYSKANRI    NOT =   ZERO )
                 OR      ( IDX-MAX      >  CONST-RIREKILST-MAX )
      *
               MOVE    IDX-MAX         TO  SPA-GMN-RIREKILST-MAX
      *
      *        有効開始日
               MOVE    SYS-2010-STYUKYMD
                                       TO  SPA-NAI-TRYUKOSTYMD (IDX-MAX)
                                           WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TRYUKOSTYMD (IDX-MAX)
      *
      *        有効終了日
               MOVE    SYS-2010-EDYUKYMD
                                       TO  SPA-NAI-TRYUKOEDYMD (IDX-MAX)
                                           WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TRYUKOEDYMD (IDX-MAX)
      *
               IF    ( SPA-NAI-YUKOSTYMD-ORG
                                   =   SPA-NAI-TRYUKOSTYMD (IDX-MAX))
                AND  ( SPA-NAI-YUKOEDYMD-ORG
                                   =   SPA-NAI-TRYUKOEDYMD (IDX-MAX))
                   MOVE    IDX-MAX    TO  SPA-GMN-SELNUM2
                   MOVE    IDX-MAX    TO  SPA-GMN-RIREKILST-SEL
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
               MOVE    SYSKANRI-REC   TO  SYS-2010-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
      *
       4100-RIREKI-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公費番号
      *****************************************************************
       4101-KOHNUM-CHK-SEC            SECTION.
      *
           INITIALIZE                      HKNNUM-REC
      *
           IF    ( SPA-GMN-KOHNUM =   SPACE   )
               MOVE    SPACE               TO  SPA-GMN-KOHNUM-G
           ELSE
               MOVE    SPA-GMN-KOHNUM      TO  WRK-CMB-CD
               PERFORM 4200-KOHNUMLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   IF    ( SPA-GMN-KOHNUM      >=   "100" )
                    AND  ( SPA-GMN-KOHNUM      <=   "899" )
                       MOVE    2           TO  SPA-GMN-CUR
                       MOVE    "0006"      TO  SPA-ERRCD
                   ELSE
                       MOVE    2           TO  SPA-GMN-CUR
                       MOVE    "0005"      TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-KOHNUM-G
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-KOHNUM      NOT =   SPA-NAI-KOHNUM )
      *
               IF    ( SPA-ERRCD           =   SPACE )
      *
                   PERFORM 4201-YUKOKIKAN-RESET-SEC
      *
                   INITIALIZE              SPA-GMN-NBOOK-AREA
                                           SPA-NAI-NBOOK-AREA
                   PERFORM 42011-COMBO-SET-DEFAULT-SEC
      *
                   IF    ( SPA-PREFNUM     =       CONST-NAGASAKI )
                    AND  ( SPA-GMN-KOHNUM  =   "180" )
                       MOVE    "1"         TO  SPA-GMN-JGNFTNKBN
                   END-IF
      *
                   MOVE    SPA-NAI-YUKOSTYMD   TO  SEL-STYMD
                   MOVE    SPA-NAI-YUKOEDYMD   TO  SEL-EDYMD
                   MOVE    SPA-GMN-KOHNUM      TO  SEL-HKNNUM
                   PERFORM 900-HKNNUM-KEY8-SEL-SEC
                   MOVE    HKNNUM-REC      TO  THKNNUM-REC
                   PERFORM 42012-COMBO-SET-DEFAULT-SEC
               END-IF
      *
               MOVE    SPA-GMN-KOHNUM  TO  SPA-NAI-KOHNUM
      *
           END-IF
      *
           .
      *
       4101-KOHNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数省略記載チェック処理
      *****************************************************************
       4101-JNISSUKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-JNISSUKBN       TO  WRK-CMB-CD
           PERFORM 4200-JNISSUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-JNISSUKBN-G
      *
           IF    ( SPA-GMN-JNISSUKBN       =   "0" )
               MOVE    "0"                 TO  SPA-GMN-JNISSUTAIKBN
           END-IF
      *
           .
      *
       4101-JNISSUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数省略記載対象区分チェック処理
      *****************************************************************
       4101-JNISSUTAIKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-JNISSUTAIKBN    TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-JNISSUTAIKBN-G
      *
           .
      *
       4101-JNISSUTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求点省略記載チェック処理
      *****************************************************************
       4101-TOTALTENKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-TOTALTENKBN     TO  WRK-CMB-CD
           PERFORM 4200-TOTALTENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-TOTALTENKBN-G
      *
           IF    ( SPA-GMN-TOTALTENKBN     =   "0" )
               MOVE    "0"                 TO  SPA-GMN-TOTALTENTAIKBN
           END-IF
      *
           .
      *
       4101-TOTALTENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求点省略記載対象区分チェック処理
      *****************************************************************
       4101-TOTALTENTAIKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-TOTALTENTAIKBN  TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-TOTALTENTAIKBN-G
      *
           .
      *
       4101-TOTALTENTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用分請求点集計チェック処理
      *****************************************************************
       4101-SYUKEIKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SYUKEIKBN     TO  WRK-CMB-CD
           PERFORM 4200-SYUKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM        TO  SPA-GMN-SYUKEIKBN-G
           IF    ( WRK-CMB-ITM2       =   SPACE )
               CONTINUE
           ELSE
               MOVE    WRK-CMB-ITM2   TO  SPA-GMN-SYUKEITAIKBN
           END-IF
           IF    ( SPA-GMN-SYUKEIKBN  =   "0" )
               MOVE    "0"            TO  SPA-GMN-SYUKEITAIKBN
           END-IF
      *
           .
      *
       4101-SYUKEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用分請求点集計対象区分チェック処理
      *****************************************************************
       4101-SYUKEITAIKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SYUKEITAIKBN     TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-SYUKEITAIKBN-G
      *
           .
      *
       4101-SYUKEITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計（外来）チェック処理
      *****************************************************************
       4101-FTNKEIKBNG-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-FTNKEIKBNG    TO  WRK-CMB-CD
           PERFORM 4200-FTNKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNKEIKBNG-G
      *
           IF    ( SPA-GMN-FTNKEIKBNG    =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNKEITAIKBNG
           END-IF
      *
           .
      *
       4101-FTNKEIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計（外来）対象区分チェック処理
      *****************************************************************
       4101-FTNKEITAIKBNG-CHK-SEC    SECTION.
      *
           MOVE    SPA-GMN-FTNKEITAIKBNG TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNKEITAIKBNG-G
      *
           .
      *
       4101-FTNKEITAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計（入院）チェック処理
      *****************************************************************
       4101-FTNKEIKBNN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-FTNKEIKBNN    TO  WRK-CMB-CD
           PERFORM 4200-FTNKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNKEIKBNN-G
      *
           IF    ( SPA-GMN-FTNKEIKBNN    =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNKEITAIKBNN
           END-IF
      *
           .
      *
       4101-FTNKEIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計（入院）対象区分チェック処理
      *****************************************************************
       4101-FTNKEITAIKBNN-CHK-SEC    SECTION.
      *
           MOVE    SPA-GMN-FTNKEITAIKBNN TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNKEITAIKBNN-G
      *
           .
      *
       4101-FTNKEITAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載（外来）チェック処理
      *****************************************************************
       4101-FTNMONEYKBNG-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-FTNMONEYKBNG    TO  WRK-CMB-CD
           PERFORM 4200-FTNMONEYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNMONEYKBNG-G
      *
           IF    ( SPA-GMN-FTNMONEYKBNG    =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNMONEYTAIKBNG
           END-IF
      *
           .
      *
       4101-FTNMONEYKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載（外来）対象区分チェック処理
      *****************************************************************
       4101-FTNMONEYTAIKBNG-CHK-SEC    SECTION.
      *
           MOVE    SPA-GMN-FTNMONEYTAIKBNG TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNMONEYTAIKBNG-G
      *
           .
      *
       4101-FTNMONEYTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載（入院）チェック処理
      *****************************************************************
       4101-FTNMONEYKBNN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-FTNMONEYKBNN    TO  WRK-CMB-CD
           PERFORM 4200-FTNMONEYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNMONEYKBNN-G
      *
           IF    ( SPA-GMN-FTNMONEYKBNN    =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNMONEYTAIKBNN
           END-IF
      *
           .
      *
       4101-FTNMONEYKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載（入院）対象区分チェック処理
      *****************************************************************
       4101-FTNMONEYTAIKBNN-CHK-SEC    SECTION.
      *
           MOVE    SPA-GMN-FTNMONEYTAIKBNN TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNMONEYTAIKBNN-G
      *
           .
      *
       4101-FTNMONEYTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）（外来）チェック処理
      *****************************************************************
       4101-FTNZEROKBNG-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-FTNZEROKBNG     TO  WRK-CMB-CD
           PERFORM 4200-FTNZEROKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNZEROKBNG-G
      *
           IF    ( SPA-GMN-FTNZEROKBNG     =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNZEROTAIKBNG
           END-IF
      *
           .
      *
       4101-FTNZEROKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）（外来）対象区分チェック処理
      *****************************************************************
       4101-FTNZEROTAIKBNG-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-FTNZEROTAIKBNG  TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNZEROTAIKBNG-G
      *
           .
      *
       4101-FTNZEROTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）（入院）チェック処理
      *****************************************************************
       4101-FTNZEROKBNN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-FTNZEROKBNN     TO  WRK-CMB-CD
           PERFORM 4200-FTNZEROKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNZEROKBNN-G
      *
           IF    ( SPA-GMN-FTNZEROKBNN     =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNZEROTAIKBNN
           END-IF
      *
           .
      *
       4101-FTNZEROKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）（入院）対象区分チェック処理
      *****************************************************************
       4101-FTNZEROTAIKBNN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-FTNZEROTAIKBNN  TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNZEROTAIKBNN-G
      *
           .
      *
       4101-FTNZEROTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院食事療養省略記載チェック処理
      *****************************************************************
       4101-SKJKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SKJKBN          TO  WRK-CMB-CD
           PERFORM 4200-SKJKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-SKJKBN-G
      *
           IF    ( SPA-GMN-SKJKBN          =   "0" )
               MOVE    "0"                 TO  SPA-GMN-SKJTAIKBN
           END-IF
      *
           .
      *
       4101-SKJKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院食事療養省略記載対象区分チェック処理
      *****************************************************************
       4101-SKJTAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SKJTAIKBN       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-SKJTAIKBN-G
      *
           .
      *
       4101-SKJTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＯＣＲ実日数請求点省略記載チェック処理
      *****************************************************************
       4101-OCRKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-OCRKBN          TO  WRK-CMB-CD
           PERFORM 4200-OCRKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-OCRKBN-G
      *
           IF    ( SPA-GMN-OCRKBN          =   "0" )
               MOVE    "0"                 TO  SPA-GMN-OCRTAIKBN
           END-IF
      *
           .
      *
       4101-OCRKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＯＣＲ実日数請求点省略記載対象区分チェック処理
      *****************************************************************
       4101-OCRTAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-OCRTAIKBN       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-OCRTAIKBN-G
      *
           .
      *
       4101-OCRTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載チェック処理
      *****************************************************************
       4101-SAIKEIKBN-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-SAIKEIKBN      TO  WRK-CMB-CD
           PERFORM 4200-SAIKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-SAIKEIKBN-G
      *
           IF    ( SPA-GMN-SAIKEIKBN       =   "0" )
               MOVE    "0"                 TO  SPA-GMN-SAIKEITAIKBN
           END-IF
      *
           .
      *
       4101-SAIKEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載対象区分チェック処理
      *****************************************************************
       4101-SAIKEITAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SAIKEITAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-SAIKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-SAIKEITAIKBN-G
      *
           .
      *
       4101-SAIKEITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載（高齢者）チェック処理
      *****************************************************************
       4101-HKNFTNKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-HKNFTNKBNK  TO  WRK-CMB-CD
           PERFORM 4200-HKNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNKBNK-G
      *
           IF    ( SPA-GMN-HKNFTNKBNK  =   "0" )
               MOVE    "0"             TO  SPA-GMN-HKNFTNTAIKBNK
           END-IF
      *
           .
      *
       4101-HKNFTNKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載（高齢者）対象区分チェック処理
      *****************************************************************
       4101-HKNFTNTAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-HKNFTNTAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNTAIKBNK-G
      *
           .
      *
       4101-HKNFTNTAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載（70歳未満）チェック処理
      *****************************************************************
       4101-HKNFTNKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-HKNFTNKBNY  TO  WRK-CMB-CD
           PERFORM 4200-HKNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNKBNY-G
      *
           IF    ( SPA-GMN-HKNFTNKBNY  =   "0" )
               MOVE    "0"             TO  SPA-GMN-HKNFTNTAIKBNY
           END-IF
      *
           .
      *
       4101-HKNFTNKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載（70歳未満）対象区分チェック処理
      *****************************************************************
       4101-HKNFTNTAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-HKNFTNTAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNTAIKBNY-G
      *
           .
      *
       4101-HKNFTNTAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定（外来）チェック処理
      *****************************************************************
       4101-RECEKSIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-RECEKSIKBNG  TO  WRK-CMB-CD
           PERFORM 4200-RECEKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSIKBNG-G
      *
           IF    ( SPA-GMN-RECEKSIKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-RECEKSITAIKBNG
           END-IF
      *
           .
      *
       4101-RECEKSIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定（外来）対象区分チェック処理
      *****************************************************************
       4101-RECEKSITAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-RECEKSITAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-RECEKSITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSITAIKBNG-G
      *
           .
      *
       4101-RECEKSITAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定（入院）チェック処理
      *****************************************************************
       4101-RECEKSIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-RECEKSIKBNN  TO  WRK-CMB-CD
           PERFORM 4200-RECEKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSIKBNN-G
      *
           IF    ( SPA-GMN-RECEKSIKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-RECEKSITAIKBNN
           END-IF
      *
           .
      *
       4101-RECEKSIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定（入院）対象区分チェック処理
      *****************************************************************
       4101-RECEKSITAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-RECEKSITAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-RECEKSITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSITAIKBNN-G
      *
           .
      *
       4101-RECEKSITAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（外来）チェック処理
      *****************************************************************
       4101-CHKJGNKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CHKJGNKBNG  TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNKBNG-G
      *
           IF    ( SPA-GMN-CHKJGNKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-CHKJGNTAIKBNG
           END-IF
      *
           .
      *
       4101-CHKJGNKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（外来）対象区分チェック処理
      *****************************************************************
       4101-CHKJGNTAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CHKJGNTAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNTAIKBNG-G
      *
           .
      *
       4101-CHKJGNTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（入院）チェック処理
      *****************************************************************
       4101-CHKJGNKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CHKJGNKBNN  TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNKBNN-G
      *
           IF    ( SPA-GMN-CHKJGNKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-CHKJGNTAIKBNN
           END-IF
      *
           .
      *
       4101-CHKJGNKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（入院）対象区分チェック処理
      *****************************************************************
       4101-CHKJGNTAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CHKJGNTAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNTAIKBNN-G
      *
           .
      *
       4101-CHKJGNTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成（外来）チェック処理
      *****************************************************************
       4101-KOHSKYKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOHSKYKBNG  TO  WRK-CMB-CD
           PERFORM 4200-KOHSKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYKBNG-G
      *
           IF    ( SPA-GMN-KOHSKYKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KOHSKYTAIKBNG
           END-IF
      *
           .
      *
       4101-KOHSKYKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成（外来）対象区分チェック処理
      *****************************************************************
       4101-KOHSKYTAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOHSKYTAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYTAIKBNG-G
      *
           .
      *
       4101-KOHSKYTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成（入院）チェック処理
      *****************************************************************
       4101-KOHSKYKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOHSKYKBNN  TO  WRK-CMB-CD
           PERFORM 4200-KOHSKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYKBNN-G
      *
           IF    ( SPA-GMN-KOHSKYKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KOHSKYTAIKBNN
           END-IF
      *
           .
      *
       4101-KOHSKYKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成（入院）対象区分チェック処理
      *****************************************************************
       4101-KOHSKYTAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOHSKYTAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYTAIKBNN-G
      *
           .
      *
       4101-KOHSKYTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日・月上限額負担計算チェック処理
      *****************************************************************
       4101-JGNFTNKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-JGNFTNKBN       TO  WRK-CMB-CD
           PERFORM 4200-JGNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-JGNFTNKBN-G
      *
           .
      *
       4101-JGNFTNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算チェック処理
      *****************************************************************
       4101-JYUHENKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-JYUHENKBN       TO  WRK-CMB-CD
           PERFORM 4200-JYUHENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-JYUHENKBN-G
      *
           IF    ( SPA-GMN-JYUHENKBN          =   "0" )
               MOVE    "0"                 TO  SPA-GMN-JYUHENTAIKBN
           END-IF
      *
           .
      *
       4101-JYUHENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算対象区分チェック処理
      *****************************************************************
       4101-JYUHENTAIKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-JYUHENTAIKBN    TO  WRK-CMB-CD
           PERFORM 4200-JYUHENTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-JYUHENTAIKBN-G
      *
           .
      *
       4101-JYUHENTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力チェック処理
      *****************************************************************
       4101-PTJGNKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-PTJGNKBN  TO  WRK-CMB-CD
           PERFORM 4200-PTJGNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PTJGNKBN-G
      *
           IF    ( SPA-GMN-PTJGNKBN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-PTJGNTAIKBN
           END-IF
      *
           .
      *
       4101-PTJGNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力対象区分チェック処理
      *****************************************************************
       4101-PTJGNTAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-PTJGNTAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-PTJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PTJGNTAIKBN-G
      *
           .
      *
       4101-PTJGNTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算（外来）チェック処理
      *****************************************************************
       4101-IRYOKEIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-IRYOKEIKBNG  TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEIKBNG-G
      *
           IF    ( SPA-GMN-IRYOKEIKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-IRYOKEITAIKBNG
           END-IF
      *
           .
      *
       4101-IRYOKEIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算（外来）対象区分チェック処理
      *****************************************************************
       4101-IRYOKEITAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-IRYOKEITAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEITAIKBNG-G
      *
           .
      *
       4101-IRYOKEITAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算（入院）チェック処理
      *****************************************************************
       4101-IRYOKEIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-IRYOKEIKBNN  TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEIKBNN-G
      *
           IF    ( SPA-GMN-IRYOKEIKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-IRYOKEITAIKBNN
           END-IF
      *
           .
      *
       4101-IRYOKEIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算（入院）対象区分チェック処理
      *****************************************************************
       4101-IRYOKEITAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-IRYOKEITAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEITAIKBNN-G
      *
           .
      *
       4101-IRYOKEITAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事負担金計算チェック処理
      *****************************************************************
       4101-SKJKEIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJKEIKBN  TO  WRK-CMB-CD
           PERFORM 4200-SKJKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJKEIKBN-G
      *
           IF    ( SPA-GMN-SKJKEIKBN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SKJKEITAIKBN
           END-IF
      *
           .
      *
       4101-SKJKEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事負担金計算対象区分チェック処理
      *****************************************************************
       4101-SKJKEITAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJKEITAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJKEITAIKBN-G
      *
           .
      *
       4101-SKJKEITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（本人）チェック処理
      *****************************************************************
       4101-SKJRYOKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOKBNHON  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNHON-G
      *
           IF    ( SPA-GMN-SKJRYOKBNHON  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SKJRYOTAIKBNHON
           END-IF
      *
           .
      *
       4101-SKJRYOKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（本人）対象区分チェック処理
      *****************************************************************
       4101-SKJRYOTAIKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOTAIKBNHON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNHON-G
      *
           .
      *
       4101-SKJRYOTAIKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（本人）チェック処理
      *****************************************************************
       4101-SEIRYOKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOKBNHON  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNHON-G
      *
           IF    ( SPA-GMN-SEIRYOKBNHON  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIRYOTAIKBNHON
           END-IF
      *
           .
      *
       4101-SEIRYOKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（本人）対象区分チェック処理
      *****************************************************************
       4101-SEIRYOTAIKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOTAIKBNHON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNHON-G
      *
           .
      *
       4101-SEIRYOTAIKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（本人）チェック処理
      *****************************************************************
       4101-SEIKANKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANKBNHON  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNHON-G
      *
           IF    ( SPA-GMN-SEIKANKBNHON  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIKANKBNHON
           END-IF
      *
           .
      *
       4101-SEIKANKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（本人）対象区分チェック処理
      *****************************************************************
       4101-SEIKANTAIKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANTAIKBNHON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNHON-G
      *
           .
      *
       4101-SEIKANTAIKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（低所得）チェック処理
      *****************************************************************
       4101-SKJRYOKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOKBNTSY  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNTSY-G
      *
           IF    ( SPA-GMN-SKJRYOKBNTSY  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SKJRYOTAIKBNTSY
           END-IF
      *
           .
      *
       4101-SKJRYOKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（低所得）対象区分チェック処理
      *****************************************************************
       4101-SKJRYOTAIKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOTAIKBNTSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNTSY-G
      *
           .
      *
       4101-SKJRYOTAIKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（低所得）チェック処理
      *****************************************************************
       4101-SEIRYOKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOKBNTSY  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNTSY-G
      *
           IF    ( SPA-GMN-SEIRYOKBNTSY  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIRYOTAIKBNTSY
           END-IF
      *
           .
      *
       4101-SEIRYOKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（低所得）対象区分チェック処理
      *****************************************************************
       4101-SEIRYOTAIKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOTAIKBNTSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNTSY-G
      *
           .
      *
       4101-SEIRYOTAIKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（低所得）チェック処理
      *****************************************************************
       4101-SEIKANKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANKBNTSY  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNTSY-G
      *
           IF    ( SPA-GMN-SEIKANKBNTSY  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIKANKBNTSY
           END-IF
      *
           .
      *
       4101-SEIKANKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（低所得）対象区分チェック処理
      *****************************************************************
       4101-SEIKANTAIKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANTAIKBNTSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNTSY-G
      *
           .
      *
       4101-SEIKANTAIKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（本人）チェック処理
      *****************************************************************
       4101-SKJRYOKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOKBNTNK  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNTNK-G
      *
           IF    ( SPA-GMN-SKJRYOKBNTNK  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SKJRYOTAIKBNTNK
           END-IF
      *
           .
      *
       4101-SKJRYOKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（低年金）対象区分チェック処理
      *****************************************************************
       4101-SKJRYOTAIKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOTAIKBNTNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNTNK-G
      *
           .
      *
       4101-SKJRYOTAIKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（低年金）チェック処理
      *****************************************************************
       4101-SEIRYOKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOKBNTNK  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNTNK-G
      *
           IF    ( SPA-GMN-SEIRYOKBNTNK  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIRYOTAIKBNTNK
           END-IF
      *
           .
      *
       4101-SEIRYOKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（低年金）対象区分チェック処理
      *****************************************************************
       4101-SEIRYOTAIKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOTAIKBNTNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNTNK-G
      *
           .
      *
       4101-SEIRYOTAIKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（低年金）チェック処理
      *****************************************************************
       4101-SEIKANKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANKBNTNK  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNTNK-G
      *
           IF    ( SPA-GMN-SEIKANKBNTNK  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIKANKBNTNK
           END-IF
      *
           .
      *
       4101-SEIKANKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（低年金）対象区分チェック処理
      *****************************************************************
       4101-SEIKANTAIKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANTAIKBNTNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNTNK-G
      *
           .
      *
       4101-SEIKANTAIKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間開始日
      *****************************************************************
       4101-YUKOSTYMD-CHK-SEC          SECTION.
      *
           MOVE    SPA-NAI-YUKOSTYMD   TO  WRK-YUKOSTYMD
           MOVE    SPACE               TO  SPA-NAI-YUKOSTYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-YUKOSTYMD   =   SPACE OR "00000000" )
               MOVE    "00000000"  TO  SPA-GMN-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD
           ELSE
               MOVE    SPA-GMN-YUKOSTYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF  ( FLG-HIZUKECHK     =   ZERO )
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOSTYMD
               ELSE
                   MOVE    WRK-YUKOSTYMD
                                       TO  WRK-YMD
                   PERFORM 800-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOSTYMD
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-NAI-YUKOSTYMD (7:2) =   "01" )
                       CONTINUE
                   ELSE
                       MOVE    "0016"  TO  SPA-ERRCD
                       MOVE    4       TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
           END-IF
      *
           .
      *
       4101-YUKOSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間終了日
      *****************************************************************
       4101-YUKOEDYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-YUKOEDYMD   TO  WRK-YUKOEDYMD
           MOVE    SPACE               TO  SPA-NAI-YUKOEDYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-YUKOEDYMD   =   SPACE OR "99999999" )
               MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                           SPA-NAI-YUKOEDYMD
           ELSE
               MOVE    SPA-GMN-YUKOEDYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF  ( FLG-HIZUKECHK     =   ZERO )
                   MOVE    WRK-HENYMDG TO  SPA-GMN-YUKOEDYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOEDYMD
               ELSE
                   MOVE    WRK-YUKOEDYMD
                                       TO  WRK-YMD
                   PERFORM 800-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-YUKOEDYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOEDYMD
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
               IF    ( SPA-ERRCD       =   SPACE )
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY7-AREA
                   MOVE    "71"        TO  LNK-DAY7-IRAI
                   MOVE    SPA-NAI-YUKOEDYMD (1:6)
                                       TO  LNK-DAY7-YM
                   CALL    "ORCSDAY"       USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
                   IF    ( LNK-DAY7-KOYOMI =   SPA-NAI-YUKOEDYMD )
                       CONTINUE
                   ELSE
                       MOVE    "0017"  TO  SPA-ERRCD
                       MOVE    5       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       4101-YUKOEDYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号２
      *****************************************************************
       4100-SELNUM2-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           MOVE    W65-SELNUM2     TO  SPA-GMN-SELNUM2
      *
           IF  (   SPA-GMN-SELNUM2 =   ZERO )
               MOVE    2           TO  SPA-GMN-CUR
           ELSE
               IF    ( SPA-GMN-RIREKILST-MAX
                                      >=   SPA-GMN-SELNUM2 )
                   PERFORM 41001-SELNUM2-CHK-SEC
               ELSE
                   MOVE    3       TO  SPA-GMN-CUR
                   MOVE    "1008"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号２
      *****************************************************************
       41001-SELNUM2-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-SELNUM2 TO  SPA-GMN-RIREKILST-SEL
      *
           MOVE    SPA-NAI-KOHNUM-ORG
                                   TO  SEL-HKNNUM
           MOVE    SPA-NAI-TRYUKOSTYMD (SPA-GMN-SELNUM2)
                                   TO  SEL-KJNYMD
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           MOVE    SPA-GMN-RIREKILST-SEL
                                   TO  SPA-GMN-SELNUM2
      *
           MOVE    1               TO  SPA-NAI-INPUT-MODE
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
      *
       41001-SELNUM2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費リスト
      *****************************************************************
       4100-KOHLST-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-KOHLST-SEL
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-KOHLST-MAX )
                    OR   ( SPA-GMN-KOHLST-SEL  NOT =   ZERO )
      *
               IF    ( W65-KOHLST-SELECT (IDX-LST) =   "T" )
                   MOVE    IDX-LST     TO  SPA-GMN-KOHLST-SEL
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KOHLST-SEL  TO  WRK-ZZZ
           MOVE    WRK-ZZZ-G           TO  W65-SELNUM1
           PERFORM 4100-SELNUM1-CHK-SEC
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       4100-KOHLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴リスト
      *****************************************************************
       4100-RIREKILST-CHK-SEC      SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-RIREKILST-SEL
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST    >   20 )
                    OR   ( SPA-GMN-RIREKILST-SEL   NOT =   ZERO )
      *
               IF    ( W65-RIREKILST-SELECT (IDX-LST)  =  "T" )
                   MOVE    IDX-LST TO  SPA-GMN-RIREKILST-SEL
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-RIREKILST-SEL
                                       TO  W65-SELNUM2
           PERFORM 4100-SELNUM2-CHK-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       4100-RIREKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-INPUT-AREA
                       SPA-NAI-INPUT-AREA
      *
           PERFORM 4201-YUKOKIKAN-RESET-SEC
      *
           PERFORM 42011-COMBO-SET-DEFAULT-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    有効期間初期化処理
      *****************************************************************
       4201-YUKOKIKAN-RESET-SEC        SECTION.
      *
      *    有効期間の開始日と終了日を設定
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD
      *
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                       SPA-NAI-YUKOEDYMD
      *
      *    公費の種類コンボ編集処理
           PERFORM 3110-KOHNUMLST-SEC
      *
           .
      *
       4201-YUKOKIKAN-RESET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期値設定処理
      *****************************************************************
       42011-COMBO-SET-DEFAULT-SEC     SECTION.
      *
      *    実日数省略記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JNISSUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JNISSUKBN-G
      *
      *    請求点省略記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-TOTALTENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-TOTALTENKBN-G
      *
      *    全国公費併用分請求点集計
           MOVE    SPACE           TO  WRK-CMB-CD
           PERFORM 4200-SYUKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SYUKEIKBN-G
           MOVE    WRK-CMB-ITM2    TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SYUKEITAIKBN-G
      *
      *    一部負担金金額集計
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-FTNKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-FTNKEIKBNG-G
                                       SPA-GMN-FTNKEIKBNN-G
      *
      *    一部負担金省略記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-FTNMONEYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-FTNMONEYKBNG-G
                                       SPA-GMN-FTNMONEYKBNN-G
      *
      *    一部負担金０円記載（記録）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-FTNZEROKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-FTNZEROKBNG-G
                                       SPA-GMN-FTNZEROKBNN-G
      *
      *    入院食事療養省略記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJKBN-G
      *
      *    ＯＣＲ実日数請求点省略記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-OCRKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-OCRKBN-G
      *
      *    月途中受給者証変更時負担計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JYUHENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JYUHENKBN-G
      *
      *    日・月上限額負担計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JGNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JGNFTNKBN-G
      *
      *    請求書公費再掲欄記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SAIKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SAIKEIKBN-G
      *
      *    保険欄−一部負担金記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-HKNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-HKNFTNKBNK-G
                                       SPA-GMN-HKNFTNKBNY-G
      *
      *    設定と異なる記載設定
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-RECEKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-RECEKSIKBNG-G
                                       SPA-GMN-RECEKSIKBNN-G
      *
      *    設定と異なる記載設定対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-RECEKSITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-RECEKSITAIKBNG-G
                                       SPA-GMN-RECEKSITAIKBNN-G
      *
      *    チェック対象上限額設定
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-CHKJGNKBNG-G
      *
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-CHKJGNKBNN-G
      *
      *    チェック対象上限額設定対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-CHKJGNTAIKBNG-G
                                       SPA-GMN-CHKJGNTAIKBNN-G
      *
      *    公費請求テーブル作成
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KOHSKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KOHSKYKBNG-G
                                       SPA-GMN-KOHSKYKBNN-G
      *
      *    患者登録−所得者情報−月上限額入力
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-PTJGNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-PTJGNKBN-G
      *
      *    患者登録−所得者情報−月上限額入力対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-PTJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-PTJGNTAIKBN-G
      *
      *    医療費負担金計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-IRYOKEIKBNG-G
                                       SPA-GMN-IRYOKEIKBNN-G
      *
      *    医療費負担金計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-IRYOKEITAIKBNG-G
                                       SPA-GMN-IRYOKEITAIKBNN-G
      *
      *    食事負担金計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJKEIKBN-G
      *
      *    食事負担金計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJKEITAIKBN-G
      *
      *    食事療養費付加設定
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJRYOKBNHON-G
                                       SPA-GMN-SEIRYOKBNHON-G
                                       SPA-GMN-SEIKANKBNHON-G
                                       SPA-GMN-SKJRYOKBNTSY-G
                                       SPA-GMN-SEIRYOKBNTSY-G
                                       SPA-GMN-SEIKANKBNTSY-G
                                       SPA-GMN-SKJRYOKBNTNK-G
                                       SPA-GMN-SEIRYOKBNTNK-G
                                       SPA-GMN-SEIKANKBNTNK-G
      *
      *    食事療養費付加設定対象区分本人
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJRYOTAIKBNHON-G
                                       SPA-GMN-SEIRYOTAIKBNHON-G
                                       SPA-GMN-SEIKANTAIKBNHON-G
      *
      *    食事療養費付加設定対象区分低所得
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJRYOTAIKBNTSY-G
                                       SPA-GMN-SEIRYOTAIKBNTSY-G
                                       SPA-GMN-SEIKANTAIKBNTSY-G
      *
      *    食事療養費付加設定対象区分低年金
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJRYOTAIKBNTNK-G
                                       SPA-GMN-SEIRYOTAIKBNTNK-G
                                       SPA-GMN-SEIKANTAIKBNTNK-G
      *
      *    共通対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JNISSUTAIKBN-G
                                       SPA-GMN-TOTALTENTAIKBN-G
                                       SPA-GMN-FTNKEITAIKBNG-G
                                       SPA-GMN-FTNKEITAIKBNN-G
                                       SPA-GMN-FTNMONEYTAIKBNG-G
                                       SPA-GMN-FTNMONEYTAIKBNN-G
                                       SPA-GMN-FTNZEROTAIKBNG-G
                                       SPA-GMN-FTNZEROTAIKBNN-G
                                       SPA-GMN-SKJTAIKBN-G
                                       SPA-GMN-OCRTAIKBN-G
                                       SPA-GMN-HKNFTNTAIKBNK-G
                                       SPA-GMN-HKNFTNTAIKBNY-G
                                       SPA-GMN-KOHSKYTAIKBNG-G
                                       SPA-GMN-KOHSKYTAIKBNN-G
      *
      *    請求書公費再掲欄記載対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SAIKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SAIKEITAIKBN-G
      *
      *    月途中受給者証変更時負担計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JYUHENTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JYUHENTAIKBN-G
      *
           .
       42011-COMBO-SET-DEFAULT-ETXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期値設定処理（Ver4.7.0追加分）
      *****************************************************************
       42012-COMBO-SET-DEFAULT-SEC     SECTION.
      *
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NARA )
                 AND ( THKN-HBTNUM      =   "71" OR "81" OR "91" )
               WHEN  ( SPA-PREFNUM     =   CONST-SHIMANE )
                 AND ( SPA-GMN-KOHNUM  =   "790" )
               WHEN  ( SPA-PREFNUM     =   CONST-YAMAGUCHI )
                 AND ( SPA-GMN-KOHNUM  =   "781" )
      *        WHEN  ( SPA-PREFNUM     =   CONST-TOKUSHIMA )
      *          AND ( SPA-GMN-KOHNUM  =   "245" )
               WHEN  ( SPA-PREFNUM     =   CONST-KOCHI )
                 AND ( THKN-HBTNUM      =   "72" )
                   MOVE    "1"         TO  SPA-GMN-FTNKEIKBNG
                   MOVE    "1"         TO  SPA-GMN-FTNKEITAIKBNG
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAZAKI )
                 AND ( SPA-GMN-KOHNUM  =   "190" )
                   MOVE    "2"         TO  SPA-GMN-FTNKEIKBNG
                   MOVE    "1"         TO  SPA-GMN-FTNKEITAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NARA )
                 AND ( THKN-HBTNUM      =   "71" OR "81" OR "91" )
               WHEN  ( SPA-PREFNUM     =   CONST-SHIMANE )
                 AND ( SPA-GMN-KOHNUM  =   "790" )
               WHEN  ( SPA-PREFNUM     =   CONST-YAMAGUCHI )
                 AND ( SPA-GMN-KOHNUM  =   "781" )
      *        WHEN  ( SPA-PREFNUM     =   CONST-TOKUSHIMA )
      *          AND ( SPA-GMN-KOHNUM  =   "245" )
               WHEN  ( SPA-PREFNUM     =   CONST-KOCHI )
                 AND ( THKN-HBTNUM      =   "72" )
                   MOVE    "1"         TO  SPA-GMN-FTNKEIKBNN
                   MOVE    "1"         TO  SPA-GMN-FTNKEITAIKBNN
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAZAKI )
                 AND ( SPA-GMN-KOHNUM  =   "190" )
                   MOVE    "2"         TO  SPA-GMN-FTNKEIKBNN
                   MOVE    "1"         TO  SPA-GMN-FTNKEITAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  =   "186" )
                   MOVE    "1"         TO  SPA-GMN-HKNFTNKBNK
                   MOVE    "0"         TO  SPA-GMN-HKNFTNTAIKBNK
               WHEN  ( SPA-PREFNUM     =   CONST-AKITA )
               WHEN  ( SPA-PREFNUM     =   CONST-IBARAKI )
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
               WHEN  ( SPA-PREFNUM     =   CONST-HIROSHIMA )
               WHEN  ( SPA-PREFNUM     =   CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  NOT =   "186" )
                   MOVE    "1"         TO  SPA-GMN-HKNFTNKBNK
                   MOVE    "2"         TO  SPA-GMN-HKNFTNTAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-OKAYAMA )
               WHEN  ( SPA-PREFNUM     =   CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  =   "186" )
                   MOVE    "1"         TO  SPA-GMN-HKNFTNKBNY
                   MOVE    "0"         TO  SPA-GMN-HKNFTNTAIKBNY
               WHEN  ( SPA-PREFNUM     =   CONST-AKITA )
               WHEN  ( SPA-PREFNUM     =   CONST-IBARAKI )
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
               WHEN  ( SPA-PREFNUM     =   CONST-HIROSHIMA )
               WHEN  ( SPA-PREFNUM     =   CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  NOT =   "186" )
                   MOVE    "1"         TO  SPA-GMN-HKNFTNKBNY
                   MOVE    "2"         TO  SPA-GMN-HKNFTNTAIKBNY
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "481" OR "482"
                                        OR "483" OR "383" )
                   MOVE    "1"         TO  SPA-GMN-RECEKSIKBNN
                   MOVE    "0"         TO  SPA-GMN-RECEKSITAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
                AND  ( SPA-GMN-KOHNUM  =   "588" )
                   MOVE    "1"         TO  SPA-GMN-CHKJGNKBNG
                   MOVE    "0"         TO  SPA-GMN-CHKJGNTAIKBNG
               WHEN  ( SPA-PREFNUM     =   CONST-CHIBA )
                   MOVE    "2"         TO  SPA-GMN-CHKJGNKBNG
                   MOVE    "0"         TO  SPA-GMN-CHKJGNTAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "783" )
               WHEN  ( SPA-PREFNUM     =   CONST-CHIBA )
               WHEN  ( SPA-PREFNUM     =   CONST-SHIGA )
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUOKA )
               WHEN  ( SPA-PREFNUM     =   CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  =   "182" OR "282" )
                   MOVE    "1"         TO  SPA-GMN-CHKJGNKBNN
                   MOVE    "0"         TO  SPA-GMN-CHKJGNTAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( THKN-HBTNUM      =   "80" OR "82" OR "84"
                                        OR "86" OR "00" )
               WHEN  ( SPA-PREFNUM     =   CONST-NARA )
                AND  ( THKN-HBTNUM      =   "71" OR "81" OR "91" )
                   MOVE    "1"         TO  SPA-GMN-KOHSKYKBNG
                   MOVE    "1"         TO  SPA-GMN-KOHSKYTAIKBNG
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUOKA )
                AND  ( THKN-HBTNUM NOT =   "41" )
                   MOVE    "1"         TO  SPA-GMN-KOHSKYKBNG
                   MOVE    "3"         TO  SPA-GMN-KOHSKYTAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NARA )
                AND  ( THKN-HBTNUM      =   "71" OR "81" OR "91" )
                   MOVE    "1"         TO  SPA-GMN-KOHSKYKBNN
                   MOVE    "1"         TO  SPA-GMN-KOHSKYTAIKBNN
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUOKA )
                AND  ( THKN-HBTNUM NOT =   "41" )
                   MOVE    "1"         TO  SPA-GMN-KOHSKYKBNN
                   MOVE    "3"         TO  SPA-GMN-KOHSKYTAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
      *        WHEN  ( SPA-PREFNUM     =   CONST-HOKKAIDO )
      *         AND  ( SPA-GMN-KOHNUM  =   "183" )
      *        WHEN  ( SPA-PREFNUM     =   CONST-IBARAKI )
      *         AND  ( SPA-GMN-KOHNUM  =   "152" )
      *        WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
      *         AND  ( SPA-GMN-KOHNUM  =   "151" )
      *        WHEN  ( SPA-PREFNUM     =   CONST-AICHI )
      *         AND  ( SPA-GMN-KOHNUM  =   "191" )
      *        WHEN  ( SPA-PREFNUM     =   CONST-HYOGO )
      *         AND  ( SPA-GMN-KOHNUM  =   "186" )
      *            MOVE    "1"         TO  SPA-GMN-PTJGNKBN
      *            MOVE    "0"         TO  SPA-GMN-PTJGNTAIKBN
      *        WHEN  ( SPA-PREFNUM     =   CONST-FUKUSHIMA )
      *         AND  ( SPA-GMN-KOHNUM  =   "281" )
      *            MOVE    "2"         TO  SPA-GMN-PTJGNKBN
      *            MOVE    "0"         TO  SPA-GMN-PTJGNTAIKBN
      *        END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "245" OR "246" OR "445"
                                        OR "545" )
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "343" OR "342" OR "181"
                                        OR "182" OR "183" OR "481"
                                        OR "482" OR "383" OR "543"
                                        OR "582" OR "743" OR "442"
                                        OR "781" OR "542" OR "681"
                                        OR "682" OR "381" OR "372"
                                        OR "373" OR "281" OR "462"
                                        OR "562" OR "662" OR "762"
                                        )
               WHEN  ( SPA-PREFNUM     =   CONST-KUMAMOTO )
                AND  ( SPA-GMN-KOHNUM  =   "341" OR "442" )
                   MOVE    "1"         TO  SPA-GMN-IRYOKEIKBNG
                   MOVE    "0"         TO  SPA-GMN-IRYOKEITAIKBNG
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "344" OR "483" OR "783"
                                        OR "683" )
                   MOVE    "1"         TO  SPA-GMN-IRYOKEIKBNG
                   MOVE    "1"         TO  SPA-GMN-IRYOKEITAIKBNG
               END-EVALUATE
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "245" OR "246" OR "445"
                                        OR "545" )
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "181" OR "182" OR "183"
                                        OR "543" OR "582" OR "743"
                                        OR "442" OR "781" OR "542"
                                        OR "681" OR "682" OR "381"
                                        OR "372" OR "373" OR "281"
                                        OR "462" OR "762" )
               WHEN  ( SPA-PREFNUM     =   CONST-KUMAMOTO )
                AND  ( SPA-GMN-KOHNUM  =   "341" OR "442" )
                   MOVE    "1"         TO  SPA-GMN-IRYOKEIKBNN
                   MOVE    "0"         TO  SPA-GMN-IRYOKEITAIKBNN
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "783" OR "683" )
                   MOVE    "1"         TO  SPA-GMN-IRYOKEIKBNN
                   MOVE    "1"         TO  SPA-GMN-IRYOKEITAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "445" OR "545" )
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =            "582" OR "743"
                                        OR "442" OR "781" OR "542"
                                        OR "381" OR "372" OR "373" )
                   MOVE    "1"         TO  SPA-GMN-SKJKEIKBN
                   MOVE    "0"         TO  SPA-GMN-SKJKEITAIKBN
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "682" OR "281" )
                   MOVE    "2"         TO  SPA-GMN-SKJKEIKBN
                   MOVE    "0"         TO  SPA-GMN-SKJKEITAIKBN
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "683" )
                   MOVE    "2"         TO  SPA-GMN-SKJKEIKBN
                   MOVE    "1"         TO  SPA-GMN-SKJKEITAIKBN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAGI )
                AND  ( SPA-GMN-KOHNUM  =   "183" OR "483" OR "583"
                                        OR "783" )
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "352" OR "353" OR "354"
                                        OR "683" )
                   MOVE    "2"         TO  SPA-GMN-SKJRYOKBNHON
                   MOVE    "0"         TO  SPA-GMN-SKJRYOTAIKBNHON
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "683" )
                   MOVE    "2"         TO  SPA-GMN-SEIRYOKBNHON
                   MOVE    "0"         TO  SPA-GMN-SEIRYOTAIKBNHON
               WHEN  ( SPA-PREFNUM     =   CONST-GUNMA )
                AND  ( SPA-GMN-KOHNUM  =   "146" OR "246" OR "143"
                                        OR "243" OR "177" OR "187" )
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "261" OR "361" OR "561"
                                        OR "264" OR "364" OR "564"
                                        OR "298" )
                   MOVE    "3"         TO  SPA-GMN-SEIRYOKBNHON
                   MOVE    "0"         TO  SPA-GMN-SEIRYOTAIKBNHON
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "683" )
                   MOVE    "2"         TO  SPA-GMN-SEIKANKBNHON
                   MOVE    "0"         TO  SPA-GMN-SEIKANTAIKBNHON
               WHEN  ( SPA-PREFNUM     =   CONST-GUNMA )
                AND  ( SPA-GMN-KOHNUM  =   "146" OR "246" OR "143"
                                        OR "243" OR "177" OR "187" )
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "261" OR "361" OR "561"
                                        OR "264" OR "364" OR "564"
                                        OR "298" )
                   MOVE    "3"         TO  SPA-GMN-SEIKANKBNHON
                   MOVE    "0"         TO  SPA-GMN-SEIKANTAIKBNHON
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "198" )
                   MOVE    "3"         TO  SPA-GMN-SEIRYOKBNTSY
                   MOVE    "0"         TO  SPA-GMN-SEIRYOTAIKBNTSY
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "198" )
                   MOVE    "3"         TO  SPA-GMN-SEIKANKBNTSY
                   MOVE    "0"         TO  SPA-GMN-SEIKANTAIKBNTSY
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "198" )
                   MOVE    "3"         TO  SPA-GMN-SEIRYOKBNTNK
                   MOVE    "0"         TO  SPA-GMN-SEIRYOTAIKBNTNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "198" )
                   MOVE    "3"         TO  SPA-GMN-SEIKANKBNTNK
                   MOVE    "0"         TO  SPA-GMN-SEIKANTAIKBNTNK
               END-EVALUATE
      *
           .
       42012-COMBO-SET-DEFAULT-ETXT.
           EXIT.
      *****************************************************************
      *   履歴情報領域初期化処理
      *****************************************************************
       4201-RIREKI-AREA-CLEAR-SEC      SECTION.
      *
           INITIALIZE  SPA-GMN-RIREKILST-AREA
                       SPA-NAI-RIREKILST-AREA
      *
           MOVE    ZERO            TO  SPA-GMN-KOHLST-SEL
      *
           .
      *
       4201-RIREKI-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-KOHNUM          =   SPACE )
               MOVE    "1101"          TO  SPA-ERRCD
               MOVE   2                TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOSTYMD   =   SPACE )
                   MOVE    "1102"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOEDYMD   =   SPACE )
                   MOVE    "1103"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOEDYMD   <   SPA-NAI-YUKOSTYMD )
                   MOVE    "1004"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
              END-IF
          END-IF
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-WIDCD       NOT =   SPACE )
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W65    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  W65
           INITIALIZE                  W65
      *
           MOVE    SPA-GMN-PAGENO      TO  W65-NB1-PAGENO
      *
      *    基準日
           MOVE    SPA-GMN-KJNYMD      TO  W65-KJNYMD
      *
      *    選択番号１
           MOVE    SPA-GMN-SELNUM1     TO  W65-SELNUM1
      *
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
              CONTINUE
           ELSE
              PERFORM 5001-GMNHEN-SEC
           END-IF
      *
           MOVE    SPA-GMN-JNISSUKBN-G         TO  W65-JNISSUKBN
           MOVE    SPA-GMN-JNISSUTAIKBN-G      TO  W65-JNISSUTAIKBN
           MOVE    SPA-GMN-TOTALTENKBN-G       TO  W65-TOTALTENKBN
           MOVE    SPA-GMN-TOTALTENTAIKBN-G    TO  W65-TOTALTENTAIKBN
           MOVE    SPA-GMN-SYUKEIKBN-G         TO  W65-SYUKEIKBN
           MOVE    SPA-GMN-SYUKEITAIKBN-G      TO  W65-SYUKEITAIKBN
           MOVE    SPA-GMN-FTNKEIKBNG-G        TO  W65-FTNKEIKBNG
           MOVE    SPA-GMN-FTNKEITAIKBNG-G     TO  W65-FTNKEITAIKBNG
           MOVE    SPA-GMN-FTNKEIKBNN-G        TO  W65-FTNKEIKBNN
           MOVE    SPA-GMN-FTNKEITAIKBNN-G     TO  W65-FTNKEITAIKBNN
           MOVE    SPA-GMN-FTNMONEYKBNG-G      TO  W65-FTNMONEYKBNG
           MOVE    SPA-GMN-FTNMONEYTAIKBNG-G   TO  W65-FTNMONEYTAIKBNG
           MOVE    SPA-GMN-FTNMONEYKBNN-G      TO  W65-FTNMONEYKBNN
           MOVE    SPA-GMN-FTNMONEYTAIKBNN-G   TO  W65-FTNMONEYTAIKBNN
           MOVE    SPA-GMN-FTNZEROKBNG-G       TO  W65-FTNZEROKBNG
           MOVE    SPA-GMN-FTNZEROTAIKBNG-G    TO  W65-FTNZEROTAIKBNG
           MOVE    SPA-GMN-FTNZEROKBNN-G       TO  W65-FTNZEROKBNN
           MOVE    SPA-GMN-FTNZEROTAIKBNN-G    TO  W65-FTNZEROTAIKBNN
           MOVE    SPA-GMN-SKJKBN-G            TO  W65-SKJKBN
           MOVE    SPA-GMN-SKJTAIKBN-G         TO  W65-SKJTAIKBN
           MOVE    SPA-GMN-OCRKBN-G            TO  W65-OCRKBN
           MOVE    SPA-GMN-OCRTAIKBN-G         TO  W65-OCRTAIKBN
           MOVE    SPA-GMN-JYUHENKBN-G         TO  W65-JYUHENKBN
           MOVE    SPA-GMN-JYUHENTAIKBN-G      TO  W65-JYUHENTAIKBN
           MOVE    SPA-GMN-JGNFTNKBN-G         TO  W65-JGNFTNKBN
           MOVE    "0 日・月上限額設定のある公費"
                                              TO   W65-JGNFTNTAIKBN
           MOVE    SPA-GMN-SAIKEIKBN-G        TO  W65-SAIKEIKBN
           MOVE    SPA-GMN-SAIKEITAIKBN-G     TO  W65-SAIKEITAIKBN
           MOVE    SPA-GMN-HKNFTNKBNK-G       TO  W65-HKNFTNKBNK
           MOVE    SPA-GMN-HKNFTNTAIKBNK-G    TO  W65-HKNFTNTAIKBNK
           MOVE    SPA-GMN-HKNFTNKBNY-G       TO  W65-HKNFTNKBNY
           MOVE    SPA-GMN-HKNFTNTAIKBNY-G    TO  W65-HKNFTNTAIKBNY
           MOVE    SPA-GMN-RECEKSIKBNG-G      TO  W65-RECEKSIKBNG
           MOVE    SPA-GMN-RECEKSITAIKBNG-G   TO  W65-RECEKSITAIKBNG
           MOVE    SPA-GMN-RECEKSIKBNN-G      TO  W65-RECEKSIKBNN
           MOVE    SPA-GMN-RECEKSITAIKBNN-G   TO  W65-RECEKSITAIKBNN
           MOVE    SPA-GMN-CHKJGNKBNG-G       TO  W65-CHKJGNKBNG
           MOVE    SPA-GMN-CHKJGNTAIKBNG-G    TO  W65-CHKJGNTAIKBNG
           MOVE    SPA-GMN-CHKJGNKBNN-G       TO  W65-CHKJGNKBNN
           MOVE    SPA-GMN-CHKJGNTAIKBNN-G    TO  W65-CHKJGNTAIKBNN
           MOVE    SPA-GMN-KOHSKYKBNG-G       TO  W65-KOHSKYKBNG
           MOVE    SPA-GMN-KOHSKYTAIKBNG-G    TO  W65-KOHSKYTAIKBNG
           MOVE    SPA-GMN-KOHSKYKBNN-G       TO  W65-KOHSKYKBNN
           MOVE    SPA-GMN-KOHSKYTAIKBNN-G    TO  W65-KOHSKYTAIKBNN
           MOVE    SPA-GMN-PTJGNKBN-G         TO  W65-PTJGNKBN
           MOVE    SPA-GMN-PTJGNTAIKBN-G      TO  W65-PTJGNTAIKBN
           MOVE    SPA-GMN-IRYOKEIKBNG-G      TO  W65-IRYOKEIKBNG
           MOVE    SPA-GMN-IRYOKEITAIKBNG-G   TO  W65-IRYOKEITAIKBNG
           MOVE    SPA-GMN-IRYOKEIKBNN-G      TO  W65-IRYOKEIKBNN
           MOVE    SPA-GMN-IRYOKEITAIKBNN-G   TO  W65-IRYOKEITAIKBNN
           MOVE    SPA-GMN-SKJKEIKBN-G        TO  W65-SKJKEIKBN
           MOVE    SPA-GMN-SKJKEITAIKBN-G     TO  W65-SKJKEITAIKBN
           MOVE    SPA-GMN-SKJRYOKBNHON-G     TO  W65-SKJRYOKBNHON
           MOVE    SPA-GMN-SKJRYOTAIKBNHON-G  TO  W65-SKJRYOTAIKBNHON
           MOVE    SPA-GMN-SEIRYOKBNHON-G     TO  W65-SEIRYOKBNHON
           MOVE    SPA-GMN-SEIRYOTAIKBNHON-G  TO  W65-SEIRYOTAIKBNHON
           MOVE    SPA-GMN-SEIKANKBNHON-G     TO  W65-SEIKANKBNHON
           MOVE    SPA-GMN-SEIKANTAIKBNHON-G  TO  W65-SEIKANTAIKBNHON
           MOVE    SPA-GMN-SKJRYOKBNTSY-G     TO  W65-SKJRYOKBNTSY
           MOVE    SPA-GMN-SKJRYOTAIKBNTSY-G  TO  W65-SKJRYOTAIKBNTSY
           MOVE    SPA-GMN-SEIRYOKBNTSY-G     TO  W65-SEIRYOKBNTSY
           MOVE    SPA-GMN-SEIRYOTAIKBNTSY-G  TO  W65-SEIRYOTAIKBNTSY
           MOVE    SPA-GMN-SEIKANKBNTSY-G     TO  W65-SEIKANKBNTSY
           MOVE    SPA-GMN-SEIKANTAIKBNTSY-G  TO  W65-SEIKANTAIKBNTSY
           MOVE    SPA-GMN-SKJRYOKBNTNK-G     TO  W65-SKJRYOKBNTNK
           MOVE    SPA-GMN-SKJRYOTAIKBNTNK-G  TO  W65-SKJRYOTAIKBNTNK
           MOVE    SPA-GMN-SEIRYOKBNTNK-G     TO  W65-SEIRYOKBNTNK
           MOVE    SPA-GMN-SEIRYOTAIKBNTNK-G  TO  W65-SEIRYOTAIKBNTNK
           MOVE    SPA-GMN-SEIKANKBNTNK-G     TO  W65-SEIKANKBNTNK
           MOVE    SPA-GMN-SEIKANTAIKBNTNK-G  TO  W65-SEIKANTAIKBNTNK
      *
           MOVE    SPA-GMN-JNISSUKBN-MAX       TO  W65-JNISSUKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JNISSUKBN-MAX )
                   MOVE    SPA-GMN-JNISSUKBNLST (IDX-CMB)
                                   TO  W65-JNISSUKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-TOTALTENKBN-MAX     TO  W65-TOTALTENKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-TOTALTENKBN-MAX )
                   MOVE    SPA-GMN-TOTALTENKBNLST (IDX-CMB)
                                   TO  W65-TOTALTENKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SYUKEIKBN-MAX     TO  W65-SYUKEIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYUKEIKBN-MAX )
                   MOVE    SPA-GMN-SYUKEIKBNLST (IDX-CMB)
                                   TO  W65-SYUKEIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-FTNKEIKBN-MAX TO    W65-FTNKEIKBNG-COUNT
                                               W65-FTNKEIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNKEIKBN-MAX )
                   MOVE    SPA-GMN-FTNKEIKBNLST (IDX-CMB)
                                   TO  W65-FTNKEIKBNG-ITEM (IDX-CMB)
                                       W65-FTNKEIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-FTNMONEYKBN-MAX TO  W65-FTNMONEYKBNG-COUNT
                                               W65-FTNMONEYKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNMONEYKBN-MAX )
                   MOVE    SPA-GMN-FTNMONEYKBNLST (IDX-CMB)
                                   TO  W65-FTNMONEYKBNG-ITEM (IDX-CMB)
                                       W65-FTNMONEYKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-FTNZEROKBN-MAX  TO  W65-FTNZEROKBNG-COUNT
                                               W65-FTNZEROKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNZEROKBN-MAX )
                   MOVE    SPA-GMN-FTNZEROKBNLST (IDX-CMB)
                                   TO  W65-FTNZEROKBNG-ITEM (IDX-CMB)
                                       W65-FTNZEROKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKJKBN-MAX          TO  W65-SKJKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKBN-MAX )
                   MOVE    SPA-GMN-SKJKBNLST (IDX-CMB)
                                   TO  W65-SKJKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-OCRKBN-MAX          TO  W65-OCRKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-OCRKBN-MAX )
                   MOVE    SPA-GMN-OCRKBNLST (IDX-CMB)
                                   TO  W65-OCRKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-JYUHENKBN-MAX       TO  W65-JYUHENKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JYUHENKBN-MAX )
                   MOVE    SPA-GMN-JYUHENKBNLST (IDX-CMB)
                                   TO  W65-JYUHENKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-JGNFTNKBN-MAX       TO  W65-JGNFTNKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JGNFTNKBN-MAX )
                   MOVE    SPA-GMN-JGNFTNKBNLST (IDX-CMB)
                                   TO  W65-JGNFTNKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    1                       TO  W65-JGNFTNTAIKBN-COUNT
           MOVE    "0 日・月上限額設定のある公費"
                                           TO  W65-JGNFTNTAIKBN-ITEM(1)
      *
           MOVE    SPA-GMN-SAIKEIKBN-MAX      TO  W65-SAIKEIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SAIKEIKBN-MAX )
                   MOVE    SPA-GMN-SAIKEIKBNLST (IDX-CMB)
                                   TO  W65-SAIKEIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-HKNFTNKBN-MAX   TO  W65-HKNFTNKBNK-COUNT
                                               W65-HKNFTNKBNY-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-HKNFTNKBN-MAX )
                   MOVE    SPA-GMN-HKNFTNKBNLST (IDX-CMB)
                                   TO  W65-HKNFTNKBNK-ITEM (IDX-CMB)
                                       W65-HKNFTNKBNY-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-RECEKSIKBN-MAX  TO  W65-RECEKSIKBNG-COUNT
                                               W65-RECEKSIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-RECEKSIKBN-MAX )
                   MOVE    SPA-GMN-RECEKSIKBNLST (IDX-CMB)
                                   TO  W65-RECEKSIKBNG-ITEM (IDX-CMB)
                                       W65-RECEKSIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-RECEKSITAIKBN-MAX
                                    TO  W65-RECEKSITAIKBNG-COUNT
                                        W65-RECEKSITAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-RECEKSITAIKBN-MAX )
                   MOVE    SPA-GMN-RECEKSITAIKBNLST (IDX-CMB)
                                   TO  W65-RECEKSITAIKBNG-ITEM (IDX-CMB)
                                       W65-RECEKSITAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-CHKJGNKBNG-MAX       TO  W65-CHKJGNKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNKBNG-MAX )
                   MOVE    SPA-GMN-CHKJGNKBNGLST (IDX-CMB)
                                   TO  W65-CHKJGNKBNG-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-CHKJGNKBNN-MAX       TO  W65-CHKJGNKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNKBNN-MAX )
                   MOVE    SPA-GMN-CHKJGNKBNNLST (IDX-CMB)
                                   TO  W65-CHKJGNKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-CHKJGNTAIKBN-MAX
                                    TO  W65-CHKJGNTAIKBNG-COUNT
                                        W65-CHKJGNTAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNTAIKBN-MAX )
                   MOVE    SPA-GMN-CHKJGNTAIKBNLST (IDX-CMB)
                                   TO  W65-CHKJGNTAIKBNG-ITEM (IDX-CMB)
                                       W65-CHKJGNTAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-KOHSKYKBN-MAX
                                    TO  W65-KOHSKYKBNG-COUNT
                                        W65-KOHSKYKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOHSKYKBN-MAX )
                   MOVE    SPA-GMN-KOHSKYKBNLST (IDX-CMB)
                                   TO  W65-KOHSKYKBNG-ITEM (IDX-CMB)
                                       W65-KOHSKYKBNN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-PTJGNKBN-MAX    TO  W65-PTJGNKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-PTJGNKBN-MAX )
                   MOVE    SPA-GMN-PTJGNKBNLST (IDX-CMB)
                                   TO  W65-PTJGNKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-PTJGNTAIKBN-MAX    TO  W65-PTJGNTAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-PTJGNTAIKBN-MAX )
                   MOVE    SPA-GMN-PTJGNTAIKBNLST (IDX-CMB)
                                   TO  W65-PTJGNTAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-IRYOKEIKBN-MAX    TO  W65-IRYOKEIKBNG-COUNT
                                                 W65-IRYOKEIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-IRYOKEIKBN-MAX )
                   MOVE    SPA-GMN-IRYOKEIKBNLST (IDX-CMB)
                                   TO  W65-IRYOKEIKBNG-ITEM (IDX-CMB)
                                       W65-IRYOKEIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-IRYOKEITAIKBN-MAX
                                           TO  W65-IRYOKEITAIKBNG-COUNT
                                               W65-IRYOKEITAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-IRYOKEITAIKBN-MAX )
                   MOVE    SPA-GMN-IRYOKEITAIKBNLST (IDX-CMB)
                                   TO  W65-IRYOKEITAIKBNG-ITEM (IDX-CMB)
                                       W65-IRYOKEITAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJKEIKBN-MAX   TO  W65-SKJKEIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKEIKBN-MAX )
                   MOVE    SPA-GMN-SKJKEIKBNLST (IDX-CMB)
                                   TO  W65-SKJKEIKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJKEITAIKBN-MAX
                                           TO  W65-SKJKEITAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKEITAIKBN-MAX )
                   MOVE    SPA-GMN-SKJKEITAIKBNLST (IDX-CMB)
                                   TO  W65-SKJKEITAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJFUKAKBN-MAX
                                   TO  W65-SKJRYOKBNHON-COUNT
                                       W65-SEIRYOKBNHON-COUNT
                                       W65-SEIKANKBNHON-COUNT
                                       W65-SKJRYOKBNTSY-COUNT
                                       W65-SEIRYOKBNTSY-COUNT
                                       W65-SEIKANKBNTSY-COUNT
                                       W65-SKJRYOKBNTNK-COUNT
                                       W65-SEIRYOKBNTNK-COUNT
                                       W65-SEIKANKBNTNK-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKAKBN-MAX )
                   MOVE    SPA-GMN-SKJFUKAKBNLST (IDX-CMB)
                                   TO  W65-SKJRYOKBNHON-ITEM (IDX-CMB)
                                       W65-SEIRYOKBNHON-ITEM (IDX-CMB)
                                       W65-SEIKANKBNHON-ITEM (IDX-CMB)
                                       W65-SKJRYOKBNTSY-ITEM (IDX-CMB)
                                       W65-SEIRYOKBNTSY-ITEM (IDX-CMB)
                                       W65-SEIKANKBNTSY-ITEM (IDX-CMB)
                                       W65-SKJRYOKBNTNK-ITEM (IDX-CMB)
                                       W65-SEIRYOKBNTNK-ITEM (IDX-CMB)
                                       W65-SEIKANKBNTNK-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJFUKATAIKBNHON-MAX
                               TO  W65-SKJRYOTAIKBNHON-COUNT
                                   W65-SEIRYOTAIKBNHON-COUNT
                                   W65-SEIKANTAIKBNHON-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNHON-MAX )
                   MOVE    SPA-GMN-SKJFUKATAIKBNHONLST (IDX-CMB)
                               TO  W65-SKJRYOTAIKBNHON-ITEM (IDX-CMB)
                                   W65-SEIRYOTAIKBNHON-ITEM (IDX-CMB)
                                   W65-SEIKANTAIKBNHON-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJFUKATAIKBNTSY-MAX
                               TO  W65-SKJRYOTAIKBNTSY-COUNT
                                   W65-SEIRYOTAIKBNTSY-COUNT
                                   W65-SEIKANTAIKBNTSY-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNTSY-MAX )
                   MOVE    SPA-GMN-SKJFUKATAIKBNTSYLST (IDX-CMB)
                               TO  W65-SKJRYOTAIKBNTSY-ITEM (IDX-CMB)
                                   W65-SEIRYOTAIKBNTSY-ITEM (IDX-CMB)
                                   W65-SEIKANTAIKBNTSY-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJFUKATAIKBNTNK-MAX
                               TO  W65-SKJRYOTAIKBNTNK-COUNT
                                   W65-SEIRYOTAIKBNTNK-COUNT
                                   W65-SEIKANTAIKBNTNK-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNTNK-MAX )
                   MOVE    SPA-GMN-SKJFUKATAIKBNTNKLST (IDX-CMB)
                               TO  W65-SKJRYOTAIKBNTNK-ITEM (IDX-CMB)
                                   W65-SEIRYOTAIKBNTNK-ITEM (IDX-CMB)
                                   W65-SEIKANTAIKBNTNK-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-COMMONTAIKBN-MAX
                                   TO  W65-JNISSUTAIKBN-COUNT
                                       W65-TOTALTENTAIKBN-COUNT
                                       W65-SYUKEITAIKBN-COUNT
                                       W65-FTNKEITAIKBNG-COUNT
                                       W65-FTNKEITAIKBNN-COUNT
                                       W65-FTNMONEYTAIKBNG-COUNT
                                       W65-FTNMONEYTAIKBNN-COUNT
                                       W65-FTNZEROTAIKBNG-COUNT
                                       W65-FTNZEROTAIKBNN-COUNT
                                       W65-SKJTAIKBN-COUNT
                                       W65-OCRTAIKBN-COUNT
                                       W65-HKNFTNTAIKBNK-COUNT
                                       W65-HKNFTNTAIKBNY-COUNT
                                       W65-KOHSKYTAIKBNG-COUNT
                                       W65-KOHSKYTAIKBNN-COUNT
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-COMMONTAIKBN-MAX )
                   MOVE    SPA-GMN-COMMONTAIKBNLST (IDX-CMB)
                                   TO  W65-JNISSUTAIKBN-ITEM (IDX-CMB)
                                       W65-TOTALTENTAIKBN-ITEM(IDX-CMB)
                                       W65-SYUKEITAIKBN-ITEM(IDX-CMB)
                                       W65-FTNKEITAIKBNG-ITEM(IDX-CMB)
                                       W65-FTNKEITAIKBNN-ITEM(IDX-CMB)
                                       W65-FTNMONEYTAIKBNG-ITEM(IDX-CMB)
                                       W65-FTNMONEYTAIKBNN-ITEM(IDX-CMB)
                                       W65-FTNZEROTAIKBNG-ITEM(IDX-CMB)
                                       W65-FTNZEROTAIKBNN-ITEM(IDX-CMB)
                                       W65-SKJTAIKBN-ITEM (IDX-CMB)
                                       W65-OCRTAIKBN-ITEM (IDX-CMB)
                                       W65-HKNFTNTAIKBNK-ITEM(IDX-CMB)
                                       W65-HKNFTNTAIKBNY-ITEM(IDX-CMB)
                                       W65-KOHSKYTAIKBNG-ITEM(IDX-CMB)
                                       W65-KOHSKYTAIKBNN-ITEM(IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SAIKEITAIKBN-MAX
                                       TO  W65-SAIKEITAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SAIKEITAIKBN-MAX )
                   MOVE    SPA-GMN-SAIKEITAIKBNLST (IDX-CMB)
                                   TO  W65-SAIKEITAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-JYUHENTAIKBN-MAX
                                       TO  W65-JYUHENTAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JYUHENTAIKBN-MAX )
                   MOVE    SPA-GMN-JYUHENTAIKBNLST (IDX-CMB)
                                   TO  W65-JYUHENTAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    公費リスト
      *
           MOVE    SPA-GMN-KOHLST-MAX  TO  W65-KOHLST-COUNT
           PERFORM VARYING IDX-LST    FROM    1   BY  1
               UNTIL     ( IDX-LST    >   SPA-GMN-KOHLST-MAX )
      *
               MOVE    IDX-LST         TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  W65-TKNO   (IDX-LST)
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   IF    ( SPA-NAI-SELNUM1-FLG (IDX-LST)
                                       =   1 )
                       MOVE    "*"     TO  W65-TKNO   (IDX-LST)(1:1)
                   END-IF
               END-IF
      *
               MOVE    SPA-GMN-TKKOHNUM  (IDX-LST)
                                       TO  W65-TKKOHNUM (IDX-LST)
               MOVE    SPA-GMN-TKKOHNAME (IDX-LST)
                                       TO  W65-TKKOHNAME(IDX-LST)
               MOVE    SPA-GMN-TKYUKOSTYMD(IDX-LST)
                                       TO  W65-TKYUKSTYMD(IDX-LST)
               MOVE    SPA-GMN-TKYUKOEDYMD(IDX-LST)
                                       TO  W65-TKYUKEDYMD(IDX-LST)
      *
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   CONTINUE
               ELSE
                   IF    ( SPA-GMN-KOHLST-SEL  =   IDX-LST )
                       MOVE    "T"     TO  W65-KOHLST-SELECT (IDX-LST)
                   ELSE
                       MOVE    "F"     TO  W65-KOHLST-SELECT (IDX-LST)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    履歴リスト
           MOVE    SPA-GMN-RIREKILST-MAX
                                       TO  W65-RIREKILST-COUNT
      *
           PERFORM VARYING IDX-LST    FROM    1   BY  1
               UNTIL     ( IDX-LST    >   SPA-GMN-RIREKILST-MAX )
      *
               MOVE    IDX-LST         TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  W65-TRNO (IDX-LST)
      *
               STRING  SPA-GMN-TRYUKOSTYMD (IDX-LST)
                       " 〜 "
                       SPA-GMN-TRYUKOEDYMD (IDX-LST)
                       DELIMITED   BY  SIZE
               INTO    W65-TRKIKAN (IDX-LST)
               END-STRING
      *
               IF    ( IDX-LST    =    SPA-GMN-RIREKILST-SEL )
                   MOVE    "T"         TO  W65-RIREKILST-SELECT(IDX-LST)
               ELSE
                   MOVE    "F"         TO  W65-RIREKILST-SELECT(IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           MOVE    "black"                 TO  W65-LBLMODE-STYLE
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
               MOVE    "blue"              TO  W65-LBLMODE-STYLE
               MOVE    "<一括変更＞"       TO  W65-LBLMODE
           END-IF
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       5001-GMNHEN-SEC                 SECTION.
      *
           MOVE    SPA-GMN-KOHNUM-G    TO  W65-KOHNUM
           MOVE    SPA-GMN-KOHNUM-MAX  TO  W65-KOHNUM-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOHNUM-MAX )
                   MOVE    SPA-GMN-KOHNUMLST (IDX-CMB)
                                   TO  W65-KOHNUM-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-YUKOSTYMD   TO  W65-YUKOSTYMD
           MOVE    SPA-GMN-YUKOEDYMD   TO  W65-YUKOEDYMD
           MOVE    SPA-GMN-SELNUM2     TO  W65-SELNUM2
      *
           .
       5001-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL   TO  W65-B06-STATE
                                       W65-B07-STATE
                                       W65-B08-STATE
                                       W65-KOHNUM-STATE
                                       W65-KOHNUM-ITEM-STATE
                                       W65-YUKOSTYMD-STATE
                                       W65-YUKOEDYMD-STATE
                                       W65-SELNUM2-STATE
                                       W65-JNISSUTAIKBN-STATE
                                       W65-JNISSUTAIKBN-ITEM-STATE
                                       W65-TOTALTENTAIKBN-STATE
                                       W65-TOTALTENTAIKBN-ITEM-STATE
                                       W65-SYUKEITAIKBN-STATE
                                       W65-SYUKEITAIKBN-ITEM-STATE
                                       W65-FTNKEITAIKBNG-STATE
                                       W65-FTNKEITAIKBNG-ITEM-STATE
                                       W65-FTNKEITAIKBNN-STATE
                                       W65-FTNKEITAIKBNN-ITEM-STATE
                                       W65-FTNMONEYTAIKBNG-STATE
                                       W65-FTNMONEYTAIKBNG-ITEM-STATE
                                       W65-FTNMONEYTAIKBNN-STATE
                                       W65-FTNMONEYTAIKBNN-ITEM-STATE
                                       W65-FTNZEROTAIKBNG-STATE
                                       W65-FTNZEROTAIKBNG-ITEM-STATE
                                       W65-FTNZEROTAIKBNN-STATE
                                       W65-FTNZEROTAIKBNN-ITEM-STATE
                                       W65-SKJTAIKBN-STATE
                                       W65-SKJTAIKBN-ITEM-STATE
                                       W65-OCRTAIKBN-STATE
                                       W65-OCRTAIKBN-ITEM-STATE
                                       W65-SAIKEITAIKBN-STATE
                                       W65-SAIKEITAIKBN-ITEM-STATE
                                       W65-JYUHENTAIKBN-STATE
                                       W65-JYUHENTAIKBN-ITEM-STATE
                                       W65-HKNFTNTAIKBNK-STATE
                                       W65-HKNFTNTAIKBNK-ITEM-STATE
                                       W65-HKNFTNTAIKBNY-STATE
                                       W65-HKNFTNTAIKBNY-ITEM-STATE
                                       W65-RECEKSITAIKBNG-STATE
                                       W65-RECEKSITAIKBNG-ITEM-STATE
                                       W65-RECEKSITAIKBNN-STATE
                                       W65-RECEKSITAIKBNN-ITEM-STATE
                                       W65-CHKJGNTAIKBNG-STATE
                                       W65-CHKJGNTAIKBNG-ITEM-STATE
                                       W65-CHKJGNTAIKBNN-STATE
                                       W65-CHKJGNTAIKBNN-ITEM-STATE
                                       W65-KOHSKYTAIKBNG-STATE
                                       W65-KOHSKYTAIKBNG-ITEM-STATE
                                       W65-KOHSKYTAIKBNN-STATE
                                       W65-KOHSKYTAIKBNN-ITEM-STATE
                                       W65-PTJGNTAIKBN-STATE
                                       W65-PTJGNTAIKBN-ITEM-STATE
                                       W65-IRYOKEITAIKBNG-STATE
                                       W65-IRYOKEITAIKBNG-ITEM-STATE
                                       W65-IRYOKEITAIKBNN-STATE
                                       W65-IRYOKEITAIKBNN-ITEM-STATE
                                       W65-SKJKEITAIKBN-STATE
                                       W65-SKJKEITAIKBN-ITEM-STATE
                                       W65-SKJRYOTAIKBNHON-STATE
                                       W65-SKJRYOTAIKBNHON-ITEM-STATE
                                       W65-SEIRYOTAIKBNHON-STATE
                                       W65-SEIRYOTAIKBNHON-ITEM-STATE
                                       W65-SEIKANTAIKBNHON-STATE
                                       W65-SEIKANTAIKBNHON-ITEM-STATE
                                       W65-SKJRYOTAIKBNTSY-STATE
                                       W65-SKJRYOTAIKBNTSY-ITEM-STATE
                                       W65-SEIRYOTAIKBNTSY-STATE
                                       W65-SEIRYOTAIKBNTSY-ITEM-STATE
                                       W65-SEIKANTAIKBNTSY-STATE
                                       W65-SEIKANTAIKBNTSY-ITEM-STATE
                                       W65-SKJRYOTAIKBNTNK-STATE
                                       W65-SKJRYOTAIKBNTNK-ITEM-STATE
                                       W65-SEIRYOTAIKBNTNK-STATE
                                       W65-SEIRYOTAIKBNTNK-ITEM-STATE
                                       W65-SEIKANTAIKBNTNK-STATE
                                       W65-SEIKANTAIKBNTNK-ITEM-STATE
      *
           IF    ( SPA-GMN-JGNFTNKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-JGNFTNTAIKBN-STATE
                                       W65-JGNFTNTAIKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-JNISSUKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-JNISSUTAIKBN-STATE
                                       W65-JNISSUTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-TOTALTENKBN  =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-TOTALTENTAIKBN-STATE
                                       W65-TOTALTENTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SYUKEIKBN  =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SYUKEITAIKBN-STATE
                                       W65-SYUKEITAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNKEIKBNG    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNKEITAIKBNG-STATE
                                       W65-FTNKEITAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNKEIKBNN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNKEITAIKBNN-STATE
                                       W65-FTNKEITAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNMONEYKBNG    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNMONEYTAIKBNG-STATE
                                       W65-FTNMONEYTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNMONEYKBNN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNMONEYTAIKBNN-STATE
                                       W65-FTNMONEYTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNZEROKBNG    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNZEROTAIKBNG-STATE
                                       W65-FTNZEROTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNZEROKBNN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNZEROTAIKBNN-STATE
                                       W65-FTNZEROTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SKJKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SKJTAIKBN-STATE
                                       W65-SKJTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-OCRKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-OCRTAIKBN-STATE
                                       W65-OCRTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SAIKEIKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                     TO  W65-SAIKEITAIKBN-STATE
                                         W65-SAIKEITAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-JYUHENKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-JYUHENTAIKBN-STATE
                                       W65-JYUHENTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-HKNFTNKBNK = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-HKNFTNTAIKBNK-STATE
                                       W65-HKNFTNTAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-HKNFTNKBNY = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-HKNFTNTAIKBNY-STATE
                                       W65-HKNFTNTAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-RECEKSIKBNG = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-RECEKSITAIKBNG-STATE
                                       W65-RECEKSITAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-RECEKSIKBNN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-RECEKSITAIKBNN-STATE
                                       W65-RECEKSITAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-CHKJGNKBNG = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-CHKJGNTAIKBNG-STATE
                                       W65-CHKJGNTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-CHKJGNKBNN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-CHKJGNTAIKBNN-STATE
                                       W65-CHKJGNTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KOHSKYKBNG = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KOHSKYTAIKBNG-STATE
                                       W65-KOHSKYTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KOHSKYKBNN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KOHSKYTAIKBNN-STATE
                                       W65-KOHSKYTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-PTJGNKBN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-PTJGNTAIKBN-STATE
                                       W65-PTJGNTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-IRYOKEIKBNG = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-IRYOKEITAIKBNG-STATE
                                       W65-IRYOKEITAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-IRYOKEIKBNN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-IRYOKEITAIKBNN-STATE
                                       W65-IRYOKEITAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SKJKEIKBN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SKJKEITAIKBN-STATE
                                       W65-SKJKEITAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SKJRYOKBNHON = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SKJRYOTAIKBNHON-STATE
                                       W65-SKJRYOTAIKBNHON-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIRYOKBNHON = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIRYOTAIKBNHON-STATE
                                       W65-SEIRYOTAIKBNHON-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIKANKBNHON = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIKANTAIKBNHON-STATE
                                       W65-SEIKANTAIKBNHON-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SKJRYOKBNTSY = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SKJRYOTAIKBNTSY-STATE
                                       W65-SKJRYOTAIKBNTSY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIRYOKBNTSY = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIRYOTAIKBNTSY-STATE
                                       W65-SEIRYOTAIKBNTSY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIKANKBNTSY = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIKANTAIKBNTSY-STATE
                                       W65-SEIKANTAIKBNTSY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SKJRYOKBNTNK = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SKJRYOTAIKBNTNK-STATE
                                       W65-SKJRYOTAIKBNTNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIRYOKBNTNK = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIRYOTAIKBNTNK-STATE
                                       W65-SEIRYOTAIKBNTNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIKANKBNTNK = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIKANTAIKBNTNK-STATE
                                       W65-SEIKANTAIKBNTNK-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-KOHLST-MAX  >=  CONST-KOHLST-MAX )
               MOVE    WIDGET-INSENSITIVE  TO  W65-B06-STATE
           END-IF
      *
           IF    ( SPA-NAI-INPUT-MODE   =  2 )
               MOVE    WIDGET-INSENSITIVE
                                        TO W65-B06-STATE
                                           W65-B08-STATE
                                           W65-KOHNUM-STATE
                                           W65-KOHNUM-ITEM-STATE
                                           W65-YUKOSTYMD-STATE
                                           W65-YUKOEDYMD-STATE
                                           W65-SELNUM2-STATE
           END-IF
      *
           IF    ( SPA-GMN-KOHLST-SEL   >  0 )
            AND  ( SPA-GMN-KOHLST-SEL   <= SPA-GMN-KOHLST-MAX )
               MOVE    WIDGET-INSENSITIVE
                                        TO W65-KOHNUM-STATE
                                           W65-KOHNUM-ITEM-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           WHEN    002
               MOVE    "KOHNUM"            TO  MCP-WIDGET
           WHEN    101
               MOVE    "JNISSUKBN"         TO  MCP-WIDGET
           WHEN    102
               MOVE    "JNISSUTAIKBN"      TO  MCP-WIDGET
           WHEN    103
               MOVE    "TOTALTENKBN"       TO  MCP-WIDGET
           WHEN    104
               MOVE    "TOTALTENTAIKBN"    TO  MCP-WIDGET
           WHEN    105
               MOVE    "SYUKEIKBN"         TO  MCP-WIDGET
           WHEN    106
               MOVE    "SYUKEITAIKBN"      TO  MCP-WIDGET
           WHEN    107
               MOVE    "FTNKEIKBNG"        TO  MCP-WIDGET
           WHEN    108
               MOVE    "FTNKEITAIKBNG"     TO  MCP-WIDGET
           WHEN    109
               MOVE    "FTNKEIKBNN"        TO  MCP-WIDGET
           WHEN    110
               MOVE    "FTNKEITAIKBNN"     TO  MCP-WIDGET
           WHEN    111
               MOVE    "FTNMONEYKBNG"      TO  MCP-WIDGET
           WHEN    112
               MOVE    "FTNMONEYTAIKBNG"   TO  MCP-WIDGET
           WHEN    113
               MOVE    "FTNMONEYKBNN"      TO  MCP-WIDGET
           WHEN    114
               MOVE    "FTNMONEYTAIKBNN"   TO  MCP-WIDGET
           WHEN    115
               MOVE    "FTNZEROKBNG"       TO  MCP-WIDGET
           WHEN    116
               MOVE    "FTNZEROTAIKBNG"    TO  MCP-WIDGET
           WHEN    117
               MOVE    "FTNZEROKBNN"       TO  MCP-WIDGET
           WHEN    118
               MOVE    "FTNZEROTAIKBNN"    TO  MCP-WIDGET
           WHEN    119
               MOVE    "SKJKBN"            TO  MCP-WIDGET
           WHEN    120
               MOVE    "SKJTAIKBN"         TO  MCP-WIDGET
           WHEN    121
               MOVE    "OCRKBN"            TO  MCP-WIDGET
           WHEN    122
               MOVE    "OCRTAIKBN"         TO  MCP-WIDGET
           WHEN    123
               MOVE    "SAIKEIKBN"         TO  MCP-WIDGET
           WHEN    124
               MOVE    "SAIKEITAIKBN"      TO  MCP-WIDGET
           WHEN    201
               MOVE    "HKNFTNKBNK"        TO  MCP-WIDGET
           WHEN    202
               MOVE    "HKNFTNTAIKBNK"     TO  MCP-WIDGET
           WHEN    203
               MOVE    "HKNFTNKBNY"        TO  MCP-WIDGET
           WHEN    204
               MOVE    "HKNFTNTAIKBNY"     TO  MCP-WIDGET
           WHEN    205
               MOVE    "RECEKSIKBNG"       TO  MCP-WIDGET
           WHEN    206
               MOVE    "RECEKSITAIKBNG"    TO  MCP-WIDGET
           WHEN    207
               MOVE    "RECEKSIKBNN"       TO  MCP-WIDGET
           WHEN    208
               MOVE    "RECEKSITAIKBNN"    TO  MCP-WIDGET
           WHEN    209
               MOVE    "CHKJGNKBNG"        TO  MCP-WIDGET
           WHEN    210
               MOVE    "CHKJGNTAIKBNG"     TO  MCP-WIDGET
           WHEN    211
               MOVE    "CHKJGNKBNN"        TO  MCP-WIDGET
           WHEN    212
               MOVE    "CHKJGNTAIKBNN"     TO  MCP-WIDGET
           WHEN    213
               MOVE    "KOHSKYKBNG"        TO  MCP-WIDGET
           WHEN    214
               MOVE    "KOHSKYTAIKBNG"     TO  MCP-WIDGET
           WHEN    215
               MOVE    "KOHSKYKBNN"        TO  MCP-WIDGET
           WHEN    216
               MOVE    "KOHSKYTAIKBNN"     TO  MCP-WIDGET
           WHEN    301
               MOVE    "JYUHENKBN"         TO  MCP-WIDGET
           WHEN    302
               MOVE    "JYUHENTAIKBN"      TO  MCP-WIDGET
           WHEN    303
               MOVE    "JGNFTNKBN"         TO  MCP-WIDGET
           WHEN    304
               MOVE    "JGNFTNTAIKBN"      TO  MCP-WIDGET
           WHEN    305
               MOVE    "PTJGNKBN"          TO  MCP-WIDGET
           WHEN    306
               MOVE    "PTJGNTAIKBN"       TO  MCP-WIDGET
           WHEN    307
               MOVE    "IRYOKEIKBNG"       TO  MCP-WIDGET
           WHEN    308
               MOVE    "IRYOKEITAIKBNG"    TO  MCP-WIDGET
           WHEN    309
               MOVE    "IRYOKEIKBNN"       TO  MCP-WIDGET
           WHEN    310
               MOVE    "IRYOKEITAIKBNN"    TO  MCP-WIDGET
           WHEN    311
               MOVE    "SKJKEIKBN"         TO  MCP-WIDGET
           WHEN    312
               MOVE    "SKJKEITAIKBN"      TO  MCP-WIDGET
           WHEN    401
               MOVE    "SKJRYOKBNHON"      TO  MCP-WIDGET
           WHEN    402
               MOVE    "SKJRYOTAIKBNHON"   TO  MCP-WIDGET
           WHEN    403
               MOVE    "SEIRYOKBNHON"      TO  MCP-WIDGET
           WHEN    404
               MOVE    "SEIRYOTAIKBNHON"   TO  MCP-WIDGET
           WHEN    405
               MOVE    "SEIKANKBNHON"      TO  MCP-WIDGET
           WHEN    406
               MOVE    "SEIKANTAIKBNHON"   TO  MCP-WIDGET
           WHEN    407
               MOVE    "SKJRYOKBNTSY"      TO  MCP-WIDGET
           WHEN    408
               MOVE    "SKJRYOTAIKBNTSY"   TO  MCP-WIDGET
           WHEN    409
               MOVE    "SEIRYOKBNTSY"      TO  MCP-WIDGET
           WHEN    410
               MOVE    "SEIRYOTAIKBNTSY"   TO  MCP-WIDGET
           WHEN    411
               MOVE    "SEIKANKBNTSY"      TO  MCP-WIDGET
           WHEN    412
               MOVE    "SEIKANTAIKBNTSY"   TO  MCP-WIDGET
           WHEN    413
               MOVE    "SKJRYOKBNTNK"      TO  MCP-WIDGET
           WHEN    414
               MOVE    "SKJRYOTAIKBNTNK"   TO  MCP-WIDGET
           WHEN    415
               MOVE    "SEIRYOKBNTNK"      TO  MCP-WIDGET
           WHEN    416
               MOVE    "SEIRYOTAIKBNTNK"   TO  MCP-WIDGET
           WHEN    417
               MOVE    "SEIKANKBNTNK"      TO  MCP-WIDGET
           WHEN    418
               MOVE    "SEIKANTAIKBNTNK"   TO  MCP-WIDGET
           WHEN    003
               MOVE    "SELNUM2"           TO  MCP-WIDGET
           WHEN    004
               MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
           WHEN    005
               MOVE    "YUKOEDYMD"         TO  MCP-WIDGET
           WHEN    006
               MOVE    "KJNYMD"            TO  MCP-WIDGET
           WHEN    007
               MOVE    "B06"               TO  MCP-WIDGET
           WHEN    008
               MOVE    "B07"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM1"
               MOVE    002                 TO  SPA-GMN-CUR
           WHEN    "KOHNUM"
               EVALUATE    SPA-GMN-PAGENO
               WHEN    1
                   MOVE    201             TO  SPA-GMN-CUR
               WHEN    2
                   MOVE    301             TO  SPA-GMN-CUR
               WHEN    3
                   MOVE    401             TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    101             TO  SPA-GMN-CUR
               END-EVALUATE
           WHEN    "JNISSUKBN"
               MOVE    102                 TO  SPA-GMN-CUR
           WHEN    "JNISSUTAIKBN"
               MOVE    103                 TO  SPA-GMN-CUR
           WHEN    "TOTALTENKBN"
               MOVE    104                 TO  SPA-GMN-CUR
           WHEN    "TOTALTENTAIKBN"
               MOVE    105                 TO  SPA-GMN-CUR
           WHEN    "SYUKEIKBN"
               MOVE    106                 TO  SPA-GMN-CUR
           WHEN    "SYUKEITAIKBN"
               MOVE    107                 TO  SPA-GMN-CUR
           WHEN    "FTNKEIKBNG"
               MOVE    108                 TO  SPA-GMN-CUR
           WHEN    "FTNKEITAIKBNG"
               MOVE    109                 TO  SPA-GMN-CUR
           WHEN    "FTNKEIKBNN"
               MOVE    110                 TO  SPA-GMN-CUR
           WHEN    "FTNKEITAIKBNN"
               MOVE    111                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYKBNG"
               MOVE    112                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYTAIKBNG"
               MOVE    113                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYKBNN"
               MOVE    114                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYTAIKBNN"
               MOVE    115                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROKBNG"
               MOVE    116                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROTAIKBNG"
               MOVE    117                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROKBNN"
               MOVE    118                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROTAIKBNN"
               MOVE    119                 TO  SPA-GMN-CUR
           WHEN    "SKJKBN"
               MOVE    120                 TO  SPA-GMN-CUR
           WHEN    "SKJTAIKBN"
               MOVE    121                 TO  SPA-GMN-CUR
           WHEN    "OCRKBN"
               MOVE    122                 TO  SPA-GMN-CUR
           WHEN    "OCRTAIKBN"
               MOVE    123                 TO  SPA-GMN-CUR
           WHEN    "SAIKEIKBN"
               MOVE    124                 TO  SPA-GMN-CUR
           WHEN    "SAIKEITAIKBN"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "HKNFTNKBNK"
               MOVE    202                 TO  SPA-GMN-CUR
           WHEN    "HKNFTNTAIKBNK"
               MOVE    203                 TO  SPA-GMN-CUR
           WHEN    "HKNFTNKBNY"
               MOVE    204                 TO  SPA-GMN-CUR
           WHEN    "HKNFTNTAIKBNY"
               MOVE    205                 TO  SPA-GMN-CUR
           WHEN    "RECEKSIKBNG"
               MOVE    206                 TO  SPA-GMN-CUR
           WHEN    "RECEKSITAIKBNG"
               MOVE    207                 TO  SPA-GMN-CUR
           WHEN    "RECEKSIKBNN"
               MOVE    208                 TO  SPA-GMN-CUR
           WHEN    "RECEKSITAIKBNN"
               MOVE    209                 TO  SPA-GMN-CUR
           WHEN    "CHKJGNKBNG"
               MOVE    210                 TO  SPA-GMN-CUR
           WHEN    "CHKJGNTAIKBNG"
               MOVE    211                 TO  SPA-GMN-CUR
           WHEN    "CHKJGNKBNN"
               MOVE    212                 TO  SPA-GMN-CUR
           WHEN    "CHKJGNTAIKBNN"
               MOVE    213                 TO  SPA-GMN-CUR
           WHEN    "KOHSKYKBNG"
               MOVE    214                 TO  SPA-GMN-CUR
           WHEN    "KOHSKYTAIKBNG"
               MOVE    215                 TO  SPA-GMN-CUR
           WHEN    "KOHSKYKBNN"
               MOVE    216                 TO  SPA-GMN-CUR
           WHEN    "KOHSKYTAIKBNN"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "JYUHENKBN"
               MOVE    302                 TO  SPA-GMN-CUR
           WHEN    "JYUHENTAIKBN"
               MOVE    303                 TO  SPA-GMN-CUR
           WHEN    "JGNFTNKBN"
               MOVE    304                 TO  SPA-GMN-CUR
           WHEN    "JGNFTNTAIKBN"
               MOVE    305                 TO  SPA-GMN-CUR
           WHEN    "PTJGNKBN"
               MOVE    306                 TO  SPA-GMN-CUR
           WHEN    "PTJGNTAIKBN"
               MOVE    307                 TO  SPA-GMN-CUR
           WHEN    "IRYOKEIKBNG"
               MOVE    308                 TO  SPA-GMN-CUR
           WHEN    "IRYOKEITAIKBNG"
               MOVE    309                 TO  SPA-GMN-CUR
           WHEN    "IRYOKEIKBNN"
               MOVE    310                 TO  SPA-GMN-CUR
           WHEN    "IRYOKEITAIKBNN"
               MOVE    311                 TO  SPA-GMN-CUR
           WHEN    "SKJKEIKBN"
               MOVE    312                 TO  SPA-GMN-CUR
           WHEN    "SKJKEITAIKBN"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOKBNHON"
               MOVE    402                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOTAIKBNHON"
               MOVE    403                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOKBNHON"
               MOVE    404                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOTAIKBNHON"
               MOVE    405                 TO  SPA-GMN-CUR
           WHEN    "SEIKANKBNHON"
               MOVE    406                 TO  SPA-GMN-CUR
           WHEN    "SEIKANTAIKBNHON"
               MOVE    407                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOKBNTSY"
               MOVE    408                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOTAIKBNTSY"
               MOVE    409                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOKBNTSY"
               MOVE    410                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOTAIKBNTSY"
               MOVE    411                 TO  SPA-GMN-CUR
           WHEN    "SEIKANKBNTSY"
               MOVE    412                 TO  SPA-GMN-CUR
           WHEN    "SEIKANTAIKBNTSY"
               MOVE    413                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOKBNTNK"
               MOVE    414                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOTAIKBNTNK"
               MOVE    415                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOKBNTNK"
               MOVE    416                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOTAIKBNTNK"
               MOVE    417                 TO  SPA-GMN-CUR
           WHEN    "SEIKANKBNTNK"
               MOVE    418                 TO  SPA-GMN-CUR
           WHEN    "SEIKANTAIKBNTNK"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "SELNUM2"
               MOVE    002                 TO  SPA-GMN-CUR
           WHEN    "YUKOSTYMD"
               MOVE    005                 TO  SPA-GMN-CUR
           WHEN    "YUKOEDYMD"
               MOVE    007                 TO  SPA-GMN-CUR
           WHEN    "KJNYMD"
               MOVE    001                 TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           IF    ( SPA-GMN-CUR         =   7 )
               IF    ( W65-B06-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    8           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   1 )
               IF    ( SPA-GMN-KOHLST-MAX  =   ZERO )
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   2)
               IF    ( W65-KOHNUM-STATE    =   WIDGET-INSENSITIVE )
                   EVALUATE    SPA-GMN-PAGENO
                   WHEN    1
                       MOVE    201         TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    301         TO  SPA-GMN-CUR
                   WHEN    3
                       MOVE    401         TO  SPA-GMN-CUR
                   WHEN    OTHER
                       MOVE    101         TO  SPA-GMN-CUR
                   END-EVALUATE
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   102 )
               IF    ( W65-JNISSUTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    103                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   104 )
               IF    ( W65-TOTALTENTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    105                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   106 )
               IF    ( W65-SYUKEITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    107                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   108 )
               IF    ( W65-FTNKEITAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    109                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   110 )
               IF    ( W65-FTNKEITAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    111                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   112 )
               IF    ( W65-FTNMONEYTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    113                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   114 )
               IF    ( W65-FTNMONEYTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    115                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   116 )
               IF    ( W65-FTNZEROTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    117                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   118 )
               IF    ( W65-FTNZEROTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    119                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   120 )
               IF    ( W65-SKJTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    121                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   122 )
               IF    ( W65-OCRTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    123                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   124 )
               IF    ( W65-SAIKEITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   202 )
               IF    ( W65-HKNFTNTAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    203                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   204 )
               IF    ( W65-HKNFTNTAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    205                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   206 )
               IF    ( W65-RECEKSITAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    207                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   208 )
               IF    ( W65-RECEKSITAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    209                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   210 )
               IF    ( W65-CHKJGNTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    211                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   212 )
               IF    ( W65-CHKJGNTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    213                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   214 )
               IF    ( W65-KOHSKYTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    215                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   216 )
               IF    ( W65-KOHSKYTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   302 )
               IF    ( W65-JYUHENTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    303                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   304 )
               IF    ( W65-JGNFTNTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    305                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   306 )
               IF    ( W65-PTJGNTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    307                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   308 )
               IF    ( W65-IRYOKEITAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    309                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   310 )
               IF    ( W65-IRYOKEITAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    311                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   312 )
               IF    ( W65-SKJKEITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   402 )
               IF    ( W65-SKJRYOTAIKBNHON-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    403                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   404 )
               IF    ( W65-SEIRYOTAIKBNHON-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    405                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   406 )
               IF    ( W65-SEIKANTAIKBNHON-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    407                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   408 )
               IF    ( W65-SKJRYOTAIKBNTSY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    409                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   410 )
               IF    ( W65-SEIRYOTAIKBNTSY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    411                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   412 )
               IF    ( W65-SEIKANTAIKBNTSY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    413                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   414 )
               IF    ( W65-SKJRYOTAIKBNTNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    415                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   416 )
               IF    ( W65-SEIRYOTAIKBNTNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    417                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   418 )
               IF    ( W65-SEIKANTAIKBNTNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
               EVALUATE    SPA-GMN-CUR
               WHEN    2
                   EVALUATE    SPA-GMN-PAGENO
                   WHEN    1
                       MOVE    201         TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    301         TO  SPA-GMN-CUR
                   WHEN    3
                       MOVE    401         TO  SPA-GMN-CUR
                   WHEN    OTHER
                       MOVE    101         TO  SPA-GMN-CUR
                   END-EVALUATE
               WHEN    4
               WHEN    5
                   COMPUTE SPA-GMN-CUR =   8
               END-EVALUATE
           END-IF
      *
           .
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "公費番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "有効期間開始日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "有効期間終了日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "地方公費の番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "有効期間に該当公費番号のマスタがありません"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "医療費負担金計算（入院）が０の時、"
                                       DELIMITED   BY  SIZE
                       "食事負担金計算（入院）は０を設定してください"
                                       DELIMITED   BY  SIZE
               INTO                        SPA-ERRMSG
               END-STRING
           WHEN    "0008"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "医療費負担金計算（入院）と"
                                       DELIMITED   BY  SIZE
                       "食事負担金計算（入院）の対象区分の設定が"
                                       DELIMITED   BY  SIZE
                       "異なります"
                                       DELIMITED   BY  SIZE
               INTO                    SPA-ERRMSG
               END-STRING
           WHEN    "0009"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "保険番号マスタの食事療養費の設定が"
                                       DELIMITED   BY  SIZE
                       "１又は３の時に設定可能です【"
                                       DELIMITED   BY  SIZE
                       WRK-TABNAME     DELIMITED   BY  SPACE
                                       "】"
                                       DELIMITED   BY  SIZE
               INTO                        SPA-ERRMSG
               END-STRING
           WHEN    "0010"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "この設定は保険番号マスタの"
                                       DELIMITED   BY  SIZE
                       "食事療養費で設定して下さい【"
                                       DELIMITED   BY  SIZE
                       WRK-TABNAME     DELIMITED   BY  SPACE
                                       "】"
                                       DELIMITED   BY  SIZE
               INTO                        SPA-ERRMSG
               END-STRING
           WHEN    "0014"
               MOVE    "基準日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "選択された番号が多すぎます"
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "有効期間開始日は１日で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "有効期間終了日は月末日で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "範囲内の選択番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "有効期間開始日＞有効期間終了日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1101"
               MOVE     "公費番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1102"
               MOVE     "有効期間開始日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1103"
               MOVE     "有効期間終了日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1104"
               STRING  "有効期間の重複するデータが存在します（"
                                           DELIMITED   BY  SIZE
                       WRK-ERRSTYMD        DELIMITED   BY  SIZE
                       " 〜 "              DELIMITED   BY  SIZE
                       WRK-ERREDYMD        DELIMITED   BY  SIZE
                       "）"                DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1001"
               MOVE    "追加可能最大件数に達しました"
                                       TO  SPA-ERRMSG
           WHEN    "1105"
               MOVE    "公費を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1106"
               MOVE    "公費番号の変更はできません"
                                       TO  SPA-ERRMSG
           WHEN    "1107"
               MOVE    "有効期間の開始日と終了日は同時に変更できません"
                                       TO  SPA-ERRMSG
           WHEN    "4000"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W65"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-WIDCD
           WHEN    "2001"
               MOVE   "選択中の公費付加情報を削除します。よろしいですか"
                                        TO  WRK-WIDMSG
           WHEN    "3001"
               MOVE   "選択中の公費付加情報を変更します"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W65"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数省略記載コンボボックスチェック処理
      *****************************************************************
       4200-JNISSUKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JNISSUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JNISSUKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JNISSUKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-JNISSUKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JNISSUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求点省略記載コンボボックスチェック処理
      *****************************************************************
       4200-TOTALTENKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-TOTALTENKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TOTALTENKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-TOTALTENKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TOTALTENKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-TOTALTENKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用分請求点集計コンボボックスチェック処理
      *****************************************************************
       4200-SYUKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYUKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SYUKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SYUKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               EVALUATE    SPA-PREFNUM
               WHEN    CONST-TOKYO
               WHEN    CONST-KYOTO
               WHEN    CONST-IBARAKI
               WHEN    CONST-SHIMANE
                   MOVE    SPA-GMN-SYUKEIKBNLST (3)
                                   TO  WRK-CMB-ITM
                   MOVE    "1"     TO  WRK-CMB-ITM2
               WHEN    CONST-EHIME
               WHEN    CONST-WAKAYAMA
                   MOVE    SPA-GMN-SYUKEIKBNLST (2)
                                   TO  WRK-CMB-ITM
                   MOVE    "1"     TO  WRK-CMB-ITM2
               WHEN    CONST-GUNMA
                   MOVE    SPA-GMN-SYUKEIKBNLST (3)
                                   TO  WRK-CMB-ITM
                   MOVE    "4"     TO  WRK-CMB-ITM2
               WHEN    CONST-TOKUSHIMA
                   MOVE    SPA-GMN-SYUKEIKBNLST (3)
                                   TO  WRK-CMB-ITM
                   MOVE    "0"     TO  WRK-CMB-ITM2
               WHEN    OTHER
                   MOVE    SPA-GMN-SYUKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
                   MOVE    "0"     TO  WRK-CMB-ITM2
               END-EVALUATE
           END-IF
      *
           .
       4200-SYUKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計コンボボックスチェック処理
      *****************************************************************
       4200-FTNKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-FTNKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-FTNKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-FTNKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載コンボボックスチェック処理
      *****************************************************************
       4200-FTNMONEYKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNMONEYKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-FTNMONEYKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-FTNMONEYKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-FTNMONEYKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNMONEYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）コンボボックスチェック処理
      *****************************************************************
       4200-FTNZEROKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNZEROKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-FTNZEROKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-FTNZEROKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-FTNZEROKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNZEROKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院食事療養省略記載コンボボックスチェック処理
      *****************************************************************
       4200-SKJKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＯＣＲ実日数請求点省略記載コンボボックスチェック処理
      *****************************************************************
       4200-OCRKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-OCRKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-OCRKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-OCRKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-OCRKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-OCRKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載コンボボックスチェック処理
      *****************************************************************
       4200-SAIKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SAIKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SAIKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SAIKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SAIKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SAIKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載コンボボックスチェック処理
      *****************************************************************
       4200-HKNFTNKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-HKNFTNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-HKNFTNKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-HKNFTNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-HKNFTNKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HKNFTNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定コンボボックスチェック処理
      *****************************************************************
       4200-RECEKSIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-RECEKSIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-RECEKSIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-RECEKSIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-RECEKSIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-RECEKSIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定対象区分コンボボックスチェック処理
      *****************************************************************
       4200-RECEKSITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-RECEKSITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-RECEKSITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-RECEKSITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-RECEKSITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-RECEKSITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（外来）コンボボックスチェック処理
      *****************************************************************
       4200-CHKJGNKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CHKJGNKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CHKJGNKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CHKJGNKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CHKJGNKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（入院）コンボボックスチェック処理
      *****************************************************************
       4200-CHKJGNKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CHKJGNKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CHKJGNKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CHKJGNKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CHKJGNKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定対象区分コンボボックスチェック処理
      *****************************************************************
       4200-CHKJGNTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CHKJGNTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CHKJGNTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CHKJGNTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CHKJGNTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成コンボボックスチェック処理
      *****************************************************************
       4200-KOHSKYKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOHSKYKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KOHSKYKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KOHSKYKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KOHSKYKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KOHSKYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算コンボボックスチェック処理
      *****************************************************************
       4200-JYUHENKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JYUHENKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JYUHENKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JYUHENKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-JYUHENKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JYUHENKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日・月上限負担計算コンボボックスチェック処理
      *****************************************************************
       4200-JGNFTNKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JGNFTNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JGNFTNKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JGNFTNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-JGNFTNKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JGNFTNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力コンボボックスチェック処理
      *****************************************************************
       4200-PTJGNKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-PTJGNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-PTJGNKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-PTJGNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PTJGNKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-PTJGNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力対象区分コンボボックスチェック処理
      *****************************************************************
       4200-PTJGNTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-PTJGNTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-PTJGNTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-PTJGNTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PTJGNTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-PTJGNTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算コンボボックスチェック処理
      *****************************************************************
       4200-IRYOKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-IRYOKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-IRYOKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-IRYOKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-IRYOKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-IRYOKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算対象区分コンボボックスチェック処理
      *****************************************************************
       4200-IRYOKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-IRYOKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-IRYOKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-IRYOKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-IRYOKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-IRYOKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事負担金計算コンボボックスチェック処理
      *****************************************************************
       4200-SKJKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事負担金計算対象区分コンボボックスチェック処理
      *****************************************************************
       4200-SKJKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費付加設定コンボボックスチェック処理
      *****************************************************************
       4200-SKJFUKAKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKAKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJFUKAKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJFUKAKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJFUKAKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJFUKAKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費付加設定対象区分本人コンボボックスチェック処理
      *****************************************************************
       4200-SKJFUKATAIKBNHONLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNHON-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJFUKATAIKBNHONL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJFUKATAIKBNHONLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJFUKATAIKBNHONLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJFUKATAIKBNHONLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費付加設定対象区分低所得コンボボックスチェック処理
      *****************************************************************
       4200-SKJFUKATAIKBNTSYLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNTSY-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJFUKATAIKBNTSYL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJFUKATAIKBNTSYLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJFUKATAIKBNTSYLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJFUKATAIKBNTSYLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費付加設定対象区分低年金コンボボックスチェック処理
      *****************************************************************
       4200-SKJFUKATAIKBNTNKLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNTNK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJFUKATAIKBNTNKL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJFUKATAIKBNTNKLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJFUKATAIKBNTNKLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJFUKATAIKBNTNKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    共通対象区分コンボボックスチェック処理
      *****************************************************************
       4200-COMMONTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-COMMONTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-COMMONTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-COMMONTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-COMMONTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-COMMONTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載対象区分コンボボックスチェック処理
      *****************************************************************
       4200-SAIKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SAIKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SAIKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SAIKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SAIKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SAIKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算対象区分コンボボックスチェック処理
      *****************************************************************
       4200-JYUHENTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JYUHENTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JYUHENTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JYUHENTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-JYUHENTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JYUHENTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類コンボボックスチェック処理
      *****************************************************************
       4200-KOHNUMLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOHNUM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KOHNUML (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KOHNUMLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KOHNUMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    KANACHK-SYUBETU TO  FLG-ENTRYCHK
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-HIZUKECHK
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    1           TO  FLG-HIZUKECHK
               END-IF
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-KEY8-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
      *    保険番号検索
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                      HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SEL-HKNNUM          TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
           IF    ( SEL-EDYMD           >   SEL-STYMD )
               MOVE    SEL-EDYMD       TO  HKN-TEKSTYMD
               MOVE    SEL-STYMD       TO  HKN-TEKEDYMD
           ELSE
               MOVE    SEL-STYMD       TO  HKN-TEKSTYMD
               MOVE    SEL-EDYMD       TO  HKN-TEKEDYMD
           END-IF
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-HKNNUM
           ELSE
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNNUM-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-KEY9-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
      *    保険番号検索
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                      HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           IF    ( SEL-EDYMD           >   SEL-STYMD )
               MOVE    SEL-EDYMD       TO  HKN-TEKSTYMD
               MOVE    SEL-STYMD       TO  HKN-TEKEDYMD
           ELSE
               MOVE    SEL-STYMD       TO  HKN-TEKSTYMD
               MOVE    SEL-EDYMD       TO  HKN-TEKEDYMD
           END-IF
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-HKNNUM
           ELSE
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           END-IF
      *
           .
       900-HKNNUM-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタFETCH処理
      *****************************************************************
       900-HKNNUM-KEY9-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
      *    保険番号検索
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-HKNNUM
           ELSE
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           END-IF
      *
           .
       900-HKNNUM-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   LOW-VALUE         TO  MCP-TABLE
                                        MCP-PATHNAME
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
