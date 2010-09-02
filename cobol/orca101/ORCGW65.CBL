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
           03  SPA-GMN-AREA.
               05  SPA-GMN-KJNYMD                  PIC X(10).
               05  SPA-GMN-INPUT-AREA.
                   07  SPA-GMN-SELNUM1             PIC X(50).
                   07  SPA-GMN-KOHNUM              PIC X(3).
                   07  SPA-GMN-KOHNAME             PIC X(50).
                   07  SPA-GMN-JNISSUKBN-G.
                       09  SPA-GMN-JNISSUKBN       PIC X(01).
                       09  SPA-GMN-JNISSUKBNF      PIC X(01).
                       09  SPA-GMN-JNISSUKBNMEI    PIC X(78).
                   07  SPA-GMN-JNISSUTAIKBN-G.
                       09  SPA-GMN-JNISSUTAIKBN    PIC X(01).
                       09  SPA-GMN-JNISSUTAIKBNF   PIC X(01).
                       09  SPA-GMN-JNISSUTAIKBNMEI PIC X(28).
                   07  SPA-GMN-TOTALTENKBN-G.
                       09  SPA-GMN-TOTALTENKBN     PIC X(01).
                       09  SPA-GMN-TOTALTENKBNF    PIC X(01).
                       09  SPA-GMN-TOTALTENKBNMEI  PIC X(78).
                   07  SPA-GMN-TOTALTENTAIKBN-G.
                       09  SPA-GMN-TOTALTENTAIKBN  PIC X(01).
                       09  SPA-GMN-TOTALTENTAIKBNF PIC X(01).
                       09  SPA-GMN-TOTALTENTAIKBNMEI
                                                   PIC X(28).
                   07  SPA-GMN-SYUKEIKBN-G.
                       09  SPA-GMN-SYUKEIKBN          PIC X(01).
                       09  SPA-GMN-SYUKEIKBNF         PIC X(01).
                       09  SPA-GMN-SYUKEIKBNMEI       PIC X(78).
                   07  SPA-GMN-SYUKEITAIKBN-G.
                       09  SPA-GMN-SYUKEITAIKBN          PIC X(01).
                       09  SPA-GMN-SYUKEITAIKBNF         PIC X(01).
                       09  SPA-GMN-SYUKEITAIKBNMEI       PIC X(28).
                   07  SPA-GMN-FTNMONEYKBNG-G.
                       09  SPA-GMN-FTNMONEYKBNG    PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNGF   PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNGMEI PIC X(78).
                   07  SPA-GMN-FTNMONEYTAIKBNG-G.
                       09  SPA-GMN-FTNMONEYTAIKBNG PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNGF
                                                   PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNGMEI
                                                   PIC X(28).
                   07  SPA-GMN-FTNMONEYKBNN-G.
                       09  SPA-GMN-FTNMONEYKBNN    PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNNF   PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNNMEI PIC X(78).
                   07  SPA-GMN-FTNMONEYTAIKBNN-G.
                       09  SPA-GMN-FTNMONEYTAIKBNN PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNNF
                                                   PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNNMEI
                                                   PIC X(28).
                   07  SPA-GMN-FTNZEROKBNG-G.
                       09  SPA-GMN-FTNZEROKBNG     PIC X(01).
                       09  SPA-GMN-FTNZEROKBNGF    PIC X(01).
                       09  SPA-GMN-FTNZEROKBNGMEI  PIC X(78).
                   07  SPA-GMN-FTNZEROTAIKBNG-G.
                       09  SPA-GMN-FTNZEROTAIKBNG  PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNGF PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNGMEI
                                                   PIC X(28).
                   07  SPA-GMN-FTNZEROKBNN-G.
                       09  SPA-GMN-FTNZEROKBNN     PIC X(01).
                       09  SPA-GMN-FTNZEROKBNNF    PIC X(01).
                       09  SPA-GMN-FTNZEROKBNNMEI  PIC X(78).
                   07  SPA-GMN-FTNZEROTAIKBNN-G.
                       09  SPA-GMN-FTNZEROTAIKBNN  PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNNF PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNNMEI
                                                   PIC X(28).
                   07  SPA-GMN-SKJKBN-G.
                       09  SPA-GMN-SKJKBN          PIC X(01).
                       09  SPA-GMN-SKJKBNF         PIC X(01).
                       09  SPA-GMN-SKJKBNMEI       PIC X(78).
                   07  SPA-GMN-SKJTAIKBN-G.
                       09  SPA-GMN-SKJTAIKBN       PIC X(01).
                       09  SPA-GMN-SKJTAIKBNF      PIC X(01).
                       09  SPA-GMN-SKJTAIKBNMEI    PIC X(28).
                   07  SPA-GMN-OCRKBN-G.
                       09  SPA-GMN-OCRKBN          PIC X(01).
                       09  SPA-GMN-OCRKBNF         PIC X(01).
                       09  SPA-GMN-OCRKBNMEI       PIC X(78).
                   07  SPA-GMN-OCRTAIKBN-G.
                       09  SPA-GMN-OCRTAIKBN       PIC X(01).
                       09  SPA-GMN-OCRTAIKBNF      PIC X(01).
                       09  SPA-GMN-OCRTAIKBNMEI    PIC X(28).
                   07  SPA-GMN-SAIKEIKBN-G.
                       09  SPA-GMN-SAIKEIKBN      PIC X(01).
                       09  SPA-GMN-SAIKEIKBNF     PIC X(01).
                       09  SPA-GMN-SAIKEIKBNMEI   PIC X(78).
                   07  SPA-GMN-JYUHENKBN-G.
                       09  SPA-GMN-JYUHENKBN       PIC X(01).
                       09  SPA-GMN-JYUHENKBNF      PIC X(01).
                       09  SPA-GMN-JYUHENKBNMEI    PIC X(78).
                   07  SPA-GMN-JYUHENTAIKBN-G.
                       09  SPA-GMN-JYUHENTAIKBN    PIC X(01).
                       09  SPA-GMN-JYUHENTAIKBNF   PIC X(01).
                       09  SPA-GMN-JYUHENTAIKBNMEI PIC X(28).
                   07  SPA-GMN-JGNFTNKBN-G.
                       09  SPA-GMN-JGNFTNKBN       PIC X(01).
                       09  SPA-GMN-JGNFTNKBNF      PIC X(01).
                       09  SPA-GMN-JGNFTNKBNMEI    PIC X(78).
                   07  SPA-GMN-SAIKEITAIKBN-G.
                       09  SPA-GMN-SAIKEITAIKBN   PIC X(01).
                       09  SPA-GMN-SAIKEITAIKBNF  PIC X(01).
                       09  SPA-GMN-SAIKEITAIKBNMEI
                                                   PIC X(28).
                   07  SPA-GMN-YUKOSTYMD           PIC X(10).
                   07  SPA-GMN-YUKOEDYMD           PIC X(10).
                   07  SPA-GMN-SELNUM2             PIC S9(2).
               05  SPA-GMN-CMB-AREA.
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
                       09  SPA-GMN-SYUKEIKBN-MAX         PIC 9(03).
                       09  SPA-GMN-SYUKEIKBNLST          OCCURS 10.
                           11  SPA-GMN-SYUKEIKBNL        PIC X(01).
                           11  SPA-GMN-SYUKEIKBNFL       PIC X(01).
                           11  SPA-GMN-SYUKEIKBNMEIL     PIC X(78).
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
                   07  SPA-GMN-JYUHENKBNLST-G.
                       09  SPA-GMN-JYUHENKBN-MAX           PIC 9(03).
                       09  SPA-GMN-JYUHENKBNLST            OCCURS 10.
                           11  SPA-GMN-JYUHENKBNL          PIC X(01).
                           11  SPA-GMN-JYUHENKBNFL         PIC X(01).
                           11  SPA-GMN-JYUHENKBNMEIL       PIC X(78).
                   07  SPA-GMN-JGNFTNKBNLST-G.
                       09  SPA-GMN-JGNFTNKBN-MAX           PIC 9(03).
                       09  SPA-GMN-JGNFTNKBNLST            OCCURS 10.
                           11  SPA-GMN-JGNFTNKBNL          PIC X(01).
                           11  SPA-GMN-JGNFTNKBNFL         PIC X(01).
                           11  SPA-GMN-JGNFTNKBNMEIL       PIC X(78).
                   07  SPA-GMN-SAIKEIKBNLST-G.
                       09  SPA-GMN-SAIKEIKBN-MAX          PIC 9(03).
                       09  SPA-GMN-SAIKEIKBNLST           OCCURS 10.
                           11  SPA-GMN-SAIKEIKBNL         PIC X(01).
                           11  SPA-GMN-SAIKEIKBNFL        PIC X(01).
                           11  SPA-GMN-SAIKEIKBNMEIL      PIC X(78).
                   07  SPA-GMN-COMMONTAIKBNLST-G.
                       09  SPA-GMN-COMMONTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-COMMONTAIKBNLST         OCCURS  10.
                           11  SPA-GMN-COMMONTAIKBNL       PIC X(01).
                           11  SPA-GMN-COMMONTAIKBNFL      PIC X(01).
                           11  SPA-GMN-COMMONTAIKBNMEIL    PIC X(28).
                   07  SPA-GMN-SAIKEITAIKBNLST-G.
                       09  SPA-GMN-SAIKEITAIKBN-MAX       PIC 9(03).
                       09  SPA-GMN-SAIKEITAIKBNLST        OCCURS 10.
                           11  SPA-GMN-SAIKEITAIKBNL      PIC X(01).
                           11  SPA-GMN-SAIKEITAIKBNFL     PIC X(01).
                           11  SPA-GMN-SAIKEITAIKBNMEIL   PIC X(28).
                   07  SPA-GMN-JYUHENTAIKBNLST-G.
                       09  SPA-GMN-JYUHENTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-JYUHENTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-JYUHENTAIKBNL       PIC X(01).
                           11  SPA-GMN-JYUHENTAIKBNFL      PIC X(01).
                           11  SPA-GMN-JYUHENTAIKBNMEIL    PIC X(28).
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
                       09  SPA-NAI-YUKOSTYMD               PIC X(08).
                       09  SPA-NAI-YUKOSTYMD-ORG           PIC X(08).
                       09  SPA-NAI-YUKOEDYMD               PIC X(08).
                       09  SPA-NAI-YUKOEDYMD-ORG           PIC X(08).
               05  SPA-NAI-KOHLST-AREA.
                   07  SPA-NAI-KOHLST                      OCCURS   100.
                       09  SPA-NAI-TKYUKOSTYMD             PIC X(08).
                       09  SPA-NAI-TKYUKOEDYMD             PIC X(08).
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
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                        PIC 9(05).
           03  IDX1                                        PIC 9(05).
           03  IDX-MAX                                     PIC 9(05).
           03  IDX-CMB                                     PIC 9(05).
           03  IDX-MUL                                     PIC 9(05).
           03  IDX-LST                                     PIC 9(05).
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
               05  WRK-SELNUM-FLG                          PIC 9(01)
                                                           OCCURS  200.
           03  WRK-MULTI-AREA.
               05  WRK-MUL-NUM                             PIC X(10)
                                                           OCCURS 20.
               05  WRK-MUL-STNUM-X                         PIC X(10).
               05  WRK-MUL-EDNUM-X                         PIC X(10).
               05  WRK-MUL-STNUM                           PIC 9(05).
               05  WRK-MUL-EDNUM                           PIC 9(05).
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
           03  CONST-FUKUSHIMA            PIC 9(02) VALUE 07.
           03  CONST-IBARAKI              PIC 9(02) VALUE 08.
           03  CONST-GUNMA                PIC 9(02) VALUE 10.
           03  CONST-TOKYO                PIC 9(02) VALUE 13.
           03  CONST-KYOTO                PIC 9(02) VALUE 26.
           03  CONST-WAKAYAMA             PIC 9(02) VALUE 30.
           03  CONST-SHIMANE              PIC 9(02) VALUE 32.
           03  CONST-TOKUSHIMA            PIC 9(02) VALUE 36.
           03  CONST-EHIME                PIC 9(02) VALUE 38.
           03  CONST-NAGASAKI             PIC 9(02) VALUE 42.
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
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
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
      *    月途中受給者証変更時負担計算コンボ編集処理
           PERFORM 3110-JYUHENKBNLST-SEC
      *
      *    日・月上限負担金計算コンボ編集処理
           PERFORM 3110-JGNFTNKBNLST-SEC
      *
      *    請求書公費再掲欄記載コンボ編集処理
           PERFORM 3110-SAIKEIKBNLST-SEC
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
           MOVE    2               TO  SPA-GMN-JNISSUKBN-MAX
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
           MOVE    2               TO  SPA-GMN-TOTALTENKBN-MAX
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
           MOVE    2               TO  SPA-GMN-FTNMONEYKBN-MAX
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
           MOVE    2               TO  SPA-GMN-OCRKBN-MAX
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
           MOVE    3               TO  SPA-GMN-JYUHENKBN-MAX
      *
           .
       3110-JYUHENKBNLST-EXT.
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
               MOVE    SYS-2010-EDYUKYMD   TO  SEL-STYMD
               MOVE    SYS-2010-STYUKYMD   TO  SEL-EDYMD
               PERFORM 900-HKNNUM-KEY8-SEL-SEC
               MOVE    HKN-SEIDONAME
                                       TO  SPA-GMN-TKKOHNAME(IDX-MAX)
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
      *
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    101         TO      SPA-GMN-CUR
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
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    W65-KOHNUM          TO  SPA-GMN-KOHNUM
           MOVE    W65-JNISSUKBN       TO  SPA-GMN-JNISSUKBN-G
           MOVE    W65-JNISSUTAIKBN    TO  SPA-GMN-JNISSUTAIKBN-G
           MOVE    W65-TOTALTENKBN     TO  SPA-GMN-TOTALTENKBN-G
           MOVE    W65-TOTALTENTAIKBN  TO  SPA-GMN-TOTALTENTAIKBN-G
           MOVE    W65-SYUKEIKBN          TO  SPA-GMN-SYUKEIKBN-G
           MOVE    W65-SYUKEITAIKBN          TO  SPA-GMN-SYUKEITAIKBN-G
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
           MOVE    W65-JYUHENKBN       TO  SPA-GMN-JYUHENKBN-G
           MOVE    W65-JGNFTNKBN       TO  SPA-GMN-JGNFTNKBN-G
           MOVE    W65-SAIKEITAIKBN    TO  SPA-GMN-SAIKEITAIKBN-G
           MOVE    W65-JYUHENTAIKBN    TO  SPA-GMN-JYUHENTAIKBN-G
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
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SELNUM1-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-YUKOSTYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-YUKOEDYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHNUM-CHK-SEC
           END-IF
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
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SAIKEIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SAIKEITAIKBN-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
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
           PERFORM 410-INPUT-CHK-SEC
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
                   MOVE    51          TO  SPA-GMN-CUR
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
           PERFORM 410-INPUT-CHK-SEC
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
                   MOVE    51          TO  SPA-GMN-CUR
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
                   MOVE    51              TO  SPA-GMN-CUR
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
           MOVE    SPA-GMN-SAIKEIKBN      TO SYS-2010-SAIKEIKBN
           MOVE    SPA-GMN-SAIKEITAIKBN   TO SYS-2010-SAIKEI-TAIKBN
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
               MOVE    101         TO  SPA-GMN-CUR
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
           MOVE    WRK-SELNUM          TO  SPA-GMN-KOHLST-SEL
      *
           MOVE    WRK-INPUT-MODE      TO  SPA-NAI-INPUT-MODE
           MOVE    SPA-GMN-TKKOHNUM (WRK-SELNUM)
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
                               MOVE    IDX1    TO  WRK-SELNUM
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
           MOVE    SYS-2010-KBNCD  TO  SPA-GMN-KOHNUM
                                       SPA-NAI-KOHNUM
                                       SPA-NAI-KOHNUM-ORG
      *
           MOVE    SYS-2010-EDYUKYMD   TO  SEL-STYMD
           MOVE    SYS-2010-STYUKYMD   TO  SEL-EDYMD
           PERFORM 900-HKNNUM-KEY8-SEL-SEC
           MOVE    HKN-SEIDONAME   TO  SPA-GMN-KOHNAME
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
           IF    ( SPA-GMN-KOHNUM =   SPACE   )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-KOHNUM      TO  WRK-KANACHK-INPUT
               PERFORM 800-ENRTRY-CHK-SEC
               MOVE    WRK-KANACHK-INPUT   TO  SPA-GMN-KOHNUM
               IF    ( FLG-ENTRYCHK        NOT =   1 )
                   MOVE    2               TO  SPA-GMN-CUR
                   MOVE    "0002"          TO  SPA-ERRCD
               END-IF
      *
               IF    ( SPA-GMN-KOHNUM      >=   "100" )
                AND  ( SPA-GMN-KOHNUM      <=   "899" )
                   CONTINUE
               ELSE
                   MOVE    2               TO  SPA-GMN-CUR
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   MOVE    SPA-GMN-KOHNUM      TO  SEL-HKNNUM
                   IF    ( SPA-NAI-YUKOEDYMD   >=  SPA-NAI-YUKOSTYMD )
                       MOVE    SPA-NAI-YUKOSTYMD   TO  SEL-EDYMD
                       MOVE    SPA-NAI-YUKOEDYMD   TO  SEL-STYMD
                   ELSE
                       MOVE    SPA-NAI-YUKOSTYMD   TO  SEL-STYMD
                       MOVE    SPA-NAI-YUKOEDYMD   TO  SEL-EDYMD
                   END-IF
                   PERFORM 900-HKNNUM-KEY8-SEL-SEC
                   IF    ( FLG-HKNNUM          =   ZERO )
                       MOVE    HKN-SEIDONAME   TO  SPA-GMN-KOHNAME
                   ELSE
                       MOVE    2               TO  SPA-GMN-CUR
                       MOVE    "0006"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-KOHNUM      NOT =   SPA-NAI-KOHNUM )
               IF    ( SPA-PREFNUM     =       CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  =   "180" )
                   MOVE    "1"         TO  SPA-GMN-JGNFTNKBN
               END-IF
               IF    ( SPA-PREFNUM     =       CONST-FUKUSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "281" )
                   MOVE    "1"         TO  SPA-GMN-JGNFTNKBN
               END-IF
               MOVE    SPA-GMN-KOHNUM  TO  SPA-NAI-KOHNUM
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
                   MOVE    51          TO  SPA-GMN-CUR
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-NAI-YUKOSTYMD (7:2) =   "01" )
                       CONTINUE
                   ELSE
                       MOVE    "0016"  TO  SPA-ERRCD
                       MOVE    51      TO  SPA-GMN-CUR
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
                   MOVE    52          TO  SPA-GMN-CUR
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
                       MOVE    52      TO  SPA-GMN-CUR
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
                   MOVE    100     TO  SPA-GMN-CUR
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
      *    有効期間の開始日と終了日を設定
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD
      *
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                       SPA-NAI-YUKOEDYMD
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
      *    共通対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JNISSUTAIKBN-G
                                       SPA-GMN-TOTALTENTAIKBN-G
                                       SPA-GMN-FTNMONEYTAIKBNG-G
                                       SPA-GMN-FTNMONEYTAIKBNN-G
                                       SPA-GMN-FTNZEROTAIKBNG-G
                                       SPA-GMN-FTNZEROTAIKBNN-G
                                       SPA-GMN-SKJTAIKBN-G
                                       SPA-GMN-OCRTAIKBN-G
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
                   MOVE    51          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOEDYMD   =   SPACE )
                   MOVE    "1103"      TO  SPA-ERRCD
                   MOVE    52          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOEDYMD   <   SPA-NAI-YUKOSTYMD )
                   MOVE    "1004"      TO  SPA-ERRCD
                   MOVE    51          TO  SPA-GMN-CUR
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
           MOVE    SPA-GMN-COMMONTAIKBN-MAX
                                   TO  W65-JNISSUTAIKBN-COUNT
                                       W65-TOTALTENTAIKBN-COUNT
                                       W65-SYUKEITAIKBN-COUNT
                                       W65-FTNMONEYTAIKBNG-COUNT
                                       W65-FTNMONEYTAIKBNN-COUNT
                                       W65-FTNZEROTAIKBNG-COUNT
                                       W65-FTNZEROTAIKBNN-COUNT
                                       W65-SKJTAIKBN-COUNT
                                       W65-OCRTAIKBN-COUNT
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-COMMONTAIKBN-MAX )
                   MOVE    SPA-GMN-COMMONTAIKBNLST (IDX-CMB)
                                   TO  W65-JNISSUTAIKBN-ITEM (IDX-CMB)
                                       W65-TOTALTENTAIKBN-ITEM(IDX-CMB)
                                       W65-SYUKEITAIKBN-ITEM(IDX-CMB)
                                       W65-FTNMONEYTAIKBNG-ITEM(IDX-CMB)
                                       W65-FTNMONEYTAIKBNN-ITEM(IDX-CMB)
                                       W65-FTNZEROTAIKBNG-ITEM(IDX-CMB)
                                       W65-FTNZEROTAIKBNN-ITEM(IDX-CMB)
                                       W65-SKJTAIKBN-ITEM (IDX-CMB)
                                       W65-OCRTAIKBN-ITEM (IDX-CMB)
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
           MOVE    SPA-GMN-KOHNUM      TO  W65-KOHNUM
           MOVE    SPA-GMN-KOHNAME     TO  W65-KOHNAME
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
                                       W65-KOHNAME-STATE
                                       W65-YUKOSTYMD-STATE
                                       W65-YUKOEDYMD-STATE
                                       W65-SELNUM2-STATE
                                       W65-JNISSUTAIKBN-STATE
                                       W65-JNISSUTAIKBN-ITEM-STATE
                                       W65-TOTALTENTAIKBN-STATE
                                       W65-TOTALTENTAIKBN-ITEM-STATE
                                       W65-SYUKEITAIKBN-STATE
                                       W65-SYUKEITAIKBN-ITEM-STATE
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
      *
           MOVE    WIDGET-INSENSITIVE
                                   TO  W65-JGNFTNTAIKBN-STATE
                                       W65-JGNFTNTAIKBN-ITEM-STATE
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
                                           W65-KOHNAME-STATE
                                           W65-YUKOSTYMD-STATE
                                           W65-YUKOEDYMD-STATE
                                           W65-SELNUM2-STATE
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
           WHEN    003
               MOVE    "JNISSUKBN"         TO  MCP-WIDGET
           WHEN    004
               MOVE    "JNISSUTAIKBN"      TO  MCP-WIDGET
           WHEN    005
               MOVE    "TOTALTENKBN"       TO  MCP-WIDGET
           WHEN    006
               MOVE    "TOTALTENTAIKBN"    TO  MCP-WIDGET
           WHEN    007
               MOVE    "FTNMONEYKBNG"      TO  MCP-WIDGET
           WHEN    008
               MOVE    "FTNMONEYTAIKBNG"   TO  MCP-WIDGET
           WHEN    009
               MOVE    "FTNMONEYKBNN"      TO  MCP-WIDGET
           WHEN    010
               MOVE    "FTNMONEYTAIKBNN"   TO  MCP-WIDGET
           WHEN    011
               MOVE    "FTNZEROKBNG"       TO  MCP-WIDGET
           WHEN    012
               MOVE    "FTNZEROTAIKBNG"    TO  MCP-WIDGET
           WHEN    013
               MOVE    "FTNZEROKBNN"       TO  MCP-WIDGET
           WHEN    014
               MOVE    "FTNZEROTAIKBNN"    TO  MCP-WIDGET
           WHEN    015
               MOVE    "SKJKBN"            TO  MCP-WIDGET
           WHEN    016
               MOVE    "SKJTAIKBN"         TO  MCP-WIDGET
           WHEN    017
               MOVE    "OCRKBN"            TO  MCP-WIDGET
           WHEN    018
               MOVE    "OCRTAIKBN"         TO  MCP-WIDGET
           WHEN    019
               MOVE    "SAIKEIKBN"        TO  MCP-WIDGET
           WHEN    020
               MOVE    "SAIKEITAIKBN"     TO  MCP-WIDGET
           WHEN    021
               MOVE    "JYUHENKBN"         TO  MCP-WIDGET
           WHEN    022
               MOVE    "JYUHENTAIKBN"      TO  MCP-WIDGET
           WHEN    023
               MOVE    "SYUKEIKBN"         TO  MCP-WIDGET
           WHEN    024
               MOVE    "SYUKEITAIKBN"      TO  MCP-WIDGET
           WHEN    025
               MOVE    "JGNFTNKBN"         TO  MCP-WIDGET
           WHEN    051
               MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
           WHEN    052
               MOVE    "YUKOEDYMD"         TO  MCP-WIDGET
           WHEN    100
               MOVE    "SELNUM2"           TO  MCP-WIDGET
           WHEN    101
               MOVE    "KJNYMD"            TO  MCP-WIDGET
           WHEN    206
               MOVE    "B06"               TO  MCP-WIDGET
           WHEN    207
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
               MOVE    003                 TO  SPA-GMN-CUR
           WHEN    "JNISSUKBN"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "JNISSUTAIKBN"
               MOVE    005                 TO  SPA-GMN-CUR
           WHEN    "TOTALTENKBN"
               MOVE    006                 TO  SPA-GMN-CUR
           WHEN    "TOTALTENTAIKBN"
               MOVE    023                 TO  SPA-GMN-CUR
           WHEN    "SYUKEIKBN"
               MOVE    024                 TO  SPA-GMN-CUR
           WHEN    "SYUKEITAIKBN"
               MOVE    007                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYKBNG"
               MOVE    008                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYTAIKBNG"
               MOVE    009                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYKBNN"
               MOVE    010                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYTAIKBNN"
               MOVE    011                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROKBNG"
               MOVE    012                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROTAIKBNG"
               MOVE    013                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROKBNN"
               MOVE    014                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROTAIKBNN"
               MOVE    015                 TO  SPA-GMN-CUR
           WHEN    "SKJKBN"
               MOVE    016                 TO  SPA-GMN-CUR
           WHEN    "SKJTAIKBN"
               MOVE    017                 TO  SPA-GMN-CUR
           WHEN    "OCRKBN"
               MOVE    018                 TO  SPA-GMN-CUR
           WHEN    "OCRTAIKBN"
               MOVE    021                 TO  SPA-GMN-CUR
           WHEN    "JGNFTNKBN"
               MOVE    019                 TO  SPA-GMN-CUR
           WHEN    "JYUHENKBN"
               MOVE    022                 TO  SPA-GMN-CUR
           WHEN    "JYUHENTAIKBN"
               MOVE    025                 TO  SPA-GMN-CUR
           WHEN    "SAIKEIKBN"
               MOVE    020                 TO  SPA-GMN-CUR
           WHEN    "SAIKEITAIKBN"
               MOVE    051                 TO  SPA-GMN-CUR
           WHEN    "YUKOSTYMD"
               MOVE    052                 TO  SPA-GMN-CUR
           WHEN    "YUKOEDYMD"
               MOVE    206                 TO  SPA-GMN-CUR
           WHEN    "SELNUM2"
               MOVE    2                   TO  SPA-GMN-CUR
           WHEN    "KJNYMD"
               MOVE    1                   TO  SPA-GMN-CUR
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
           IF    ( SPA-GMN-CUR         =   206 )
               IF    ( W65-B06-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    207         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   1 )
               IF    ( SPA-GMN-KOHLST-MAX  =   ZERO )
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   4 )
               IF    ( W65-JNISSUTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   6 )
               IF    ( W65-TOTALTENTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    23          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   24 )
               IF    ( W65-SYUKEITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    7           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   8 )
               IF    ( W65-FTNMONEYTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    9           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   10 )
               IF    ( W65-FTNMONEYTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    11          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   12 )
               IF    ( W65-FTNZEROTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    13          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   14 )
               IF    ( W65-FTNZEROTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    15          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   16 )
               IF    ( W65-SKJTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    17          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   18 )
               IF    ( W65-OCRTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    21          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   22 )
               IF    ( W65-JYUHENTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    25          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   20 )
               IF    ( W65-SAIKEITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    51          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
               EVALUATE    SPA-GMN-CUR
               WHEN    2
                   COMPUTE SPA-GMN-CUR =   3
               WHEN    51
               WHEN    52
                   COMPUTE SPA-GMN-CUR =   207
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
           WHEN    "0014"
               MOVE    "基準日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "選択された番号が多すぎます"
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "有効期間開始日は１日で入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "有効期間終了日は月末日で入力して下さい"
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
           MOVE    SEL-STYMD           TO  HKN-TEKSTYMD
           MOVE    SEL-EDYMD           TO  HKN-TEKEDYMD
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
      *
