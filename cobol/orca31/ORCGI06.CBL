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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGI06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入退院登録
      *  コンポーネント名  : 入院オーダー変更
      *  管理者            : 
      *  05/03/09    NACL-太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07.05.15  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    チェックマスタ設定用共通パラメタ
           COPY    "I0COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-I06.
           03  SPA-I06-AREA.
               05  SPA-GMN-ALLIDOLST-PAGE          PIC 9(03).
               05  SPA-GMN-CUR                     PIC 9(03).
               05  SPA-GMN-HEAD-AREA.
                   07  SPA-GMN-PTNUM               PIC X(20).
                   07  SPA-GMN-NAME                PIC X(100).
                   07  SPA-GMN-SEX                 PIC X(4).
                   07  SPA-GMN-BIRTHDAY            PIC X(9).
                   07  SPA-GMN-AGE                 PIC X(9).
                   07  SPA-GMN-HSRYKA              PIC X(10).
                   07  SPA-GMN-HHKNCOMBI           PIC X(46).
                   07  SPA-GMN-IDOJIYU             PIC X(30).
                   07  SPA-GMN-LBL-MAEIDOMEI       PIC X(12).
                   07  SPA-GMN-MAEIDOYMD           PIC X(10).
               05  SPA-GMN-INPUT-AREA.
                   07  SPA-GMN-BRMNUM              PIC X(6).
                   07  SPA-GMN-BRMNUM-ORG          PIC X(6).
                   07  SPA-GMN-BTUNAME-G.
                       09  SPA-GMN-BTUNAME         PIC X(2).
                       09  SPA-GMN-BTUNAMEFL       PIC X(1).
                       09  SPA-GMN-BTUNAMEMEI      PIC X(20).
                   07  SPA-GMN-BTUNAME-ORG         PIC X(2).
                   07  SPA-GMN-SAGAKU-G.
                       09  SPA-GMN-SAGAKU          PIC X(2).
                       09  SPA-GMN-SAGAKUFL        PIC X(1).
                       09  SPA-GMN-SAGAKUMEI.
                           11  SPA-GMN-SAGAKUMEI-NUM PIC X(05).
                           11  SPA-GMN-SAGAKUMEI-EN  PIC X(02).
                           11  SPA-GMN-SAGAKUMEI-FLI PIC X(13).
                   07  SPA-GMN-NYUINYMD            PIC X(10).
                   07  SPA-GMN-NYUINKA-G.
                       09  SPA-GMN-NYUINKA         PIC X(02).
                       09  SPA-GMN-NYUINKAFL       PIC X(01).
                       09  SPA-GMN-NYUINKAMEI      PIC X(24).
                   07  SPA-GMN-SHOKAI-G.
                       09  SPA-GMN-SHOKAI          PIC X(01).
                       09  SPA-GMN-SHOKAIFL        PIC X(01).
                       09  SPA-GMN-SHOKAIMEI       PIC X(06).
                   07  SPA-GMN-SHONUM              PIC S9(2).
                   07  SPA-GMN-LBL-SHONYUINYMD     PIC X(16).
                   07  SPA-GMN-TAIINYMD            PIC X(10).
                   07  SPA-GMN-TAIINCD-G.
                       09  SPA-GMN-TAIINCD         PIC X(02).
                       09  SPA-GMN-TAIINCDFL       PIC X(01).
                       09  SPA-GMN-TAIINCDMEI      PIC X(20).
                   07  SPA-GMN-DR-G                OCCURS  3.
                       09  SPA-GMN-DR              PIC X(04).
                       09  SPA-GMN-DRFL            PIC X(01).
                       09  SPA-GMN-DRMEI           PIC X(22).
                   07  SPA-GMN-HKNCOMBI-G.
                       09  SPA-GMN-HKNCOMBI        PIC X(04).
                       09  SPA-GMN-HKNCOMBIFL      PIC X(01).
                       09  SPA-GMN-HKNCOMBIMEI     PIC X(46).
                   07  SPA-GMN-NYUKBN-G.
                       09  SPA-GMN-NYUKBN          PIC X(02).
                       09  SPA-GMN-NYUKBNFL        PIC X(01).
                       09  SPA-GMN-NYUKBNMEI       PIC X(08).
                   07  SPA-GMN-TOKNYUIN-G.
                       09  SPA-GMN-TOKNYUIN        PIC X(02).
                       09  SPA-GMN-TOKNYUINFL      PIC X(01).
                       09  SPA-GMN-TOKNYUINMEI     PIC X(60).
                   07  SPA-GMN-SNTNYUIN            PIC X(60).
                   07  SPA-GMN-SKJTIME-G.
                       09  SPA-GMN-SKJTIME         PIC X(02).
                       09  SPA-GMN-SKJTIMEFL       PIC X(01).
                       09  SPA-GMN-SKJTIMEMEI      PIC X(08).
                   07  SPA-GMN-NISSU               PIC X(50).
               05  SPA-GMN-ALLIDOLIST-INPUT-AREA.
                   07  SPA-GMN-SELNUM              PIC S9(3).
                   07  SPA-GMN-UKEYMD              PIC X(10).
               05  SPA-GMN-ALLIDOLST-AREA.
                   07  SPA-GMN-ALLIDOLST-NEXT      PIC 9(01).
                   07  SPA-GMN-ALLIDOLST-MAX       PIC  9(5).
                   07  SPA-GMN-ALLIDOLST-SEL       PIC  9(5).
                   07  SPA-GMN-ALLIDOLST-OCC       OCCURS  50.
                       09  SPA-GMN-TALLNUM         PIC  9(5).
                       09  SPA-GMN-TALLIDOYMD      PIC X(10).
                       09  SPA-GMN-TALLIDOJIYU     PIC X(20).
                       09  SPA-GMN-TALLPTNUM       PIC X(20).
                       09  SPA-GMN-TALLNAME        PIC X(100).
                       09  SPA-GMN-TALLSTS         PIC 9(1).
               05  SPA-GMN-KJNIDOLST-AREA.
                   07  SPA-GMN-KJNIDOLST-MAX       PIC  9(5).
                   07  SPA-GMN-KJNIDOLST-SEL       PIC  9(5).
                   07  SPA-GMN-KJNIDOLST-OCC       OCCURS  50.
                       09  SPA-GMN-TKJNUKEYMD      PIC X(10).
                       09  SPA-GMN-TKJNIDOYMD      PIC X(10).
                       09  SPA-GMN-TKJNIDOJIYU     PIC X(20).
                       09  SPA-GMN-TKJNSTS         PIC 9(1).
               05  SPA-GMN-ERRMESLST-AREA.
                   07  SPA-GMN-ERRMESLST-MAX       PIC  9(5).
                   07  SPA-GMN-ERRMESLST-SEL       PIC  9(5).
                   07  SPA-GMN-ERRMESLST-OCC       OCCURS  10.
                       09  SPA-GMN-TERRCD          PIC X(04).
                       09  SPA-GMN-TERRMES         PIC X(100).
               05  SPA-GMN-COMBO-AREA.
                   07  SPA-GMN-BTUNAMELST-G.
                       09  SPA-GMN-BTUNAME-MAX     PIC 9(05).
                       09  SPA-GMN-BTUNAMELST      OCCURS  50.
                           11  SPA-GMN-BTUNAMEL    PIC X(02).
                           11  SPA-GMN-BTUNAMEFLL  PIC X(01).
                           11  SPA-GMN-BTUNAMEMEIL PIC X(20).
                   07  SPA-GMN-SAGAKULST-G.
                       09  SPA-GMN-SAGAKU-MAX      PIC 9(05).
                       09  SPA-GMN-SAGAKULST       OCCURS  20.
                           11  SPA-GMN-SAGAKUL     PIC X(02).
                           11  SPA-GMN-SAGAKUFLL   PIC X(01).
                           11  SPA-GMN-SAGAKUMEIL  PIC X(20).
                   07  SPA-GMN-NYUINKALST-G.
                       09  SPA-GMN-NYUINKA-MAX     PIC 9(05).
                       09  SPA-GMN-NYUINKALST      OCCURS  99.
                           11  SPA-GMN-NYUINKAL    PIC X(02).
                           11  SPA-GMN-NYUINKAFLL  PIC X(01).
                           11  SPA-GMN-NYUINKAMEIL PIC X(24).
                   07  SPA-GMN-SHOKAILST-G.
                       09  SPA-GMN-SHOKAI-MAX      PIC 9(05).
                       09  SPA-GMN-SHOKAILST       OCCURS  5.
                           11  SPA-GMN-SHOKAIL     PIC X(01).
                           11  SPA-GMN-SHOKAIFLL   PIC X(01).
                           11  SPA-GMN-SHOKAIMEIl  PIC X(06).
                   07  SPA-GMN-TAIINCDLST-G.
                       09  SPA-GMN-TAIINCD-MAX     PIC 9(05).
                       09  SPA-GMN-TAIINCDLST      OCCURS  50.
                           11  SPA-GMN-TAIINCDL    PIC X(02).
                           11  SPA-GMN-TAIINCDFLL  PIC X(01).
                           11  SPA-GMN-TAIINCDMEIL PIC X(20).
                   07  SPA-GMN-DRLST-G.
                       09  SPA-GMN-DR-MAX          PIC 9(05).
                       09  SPA-GMN-DRLST           OCCURS  99.
                           11  SPA-GMN-DRL         PIC X(04).
                           11  SPA-GMN-DRFLL       PIC X(01).
                           11  SPA-GMN-DRMEIL      PIC X(22).
                   07  SPA-GMN-HKNCOMBILST-G.
                       09  SPA-GMN-HKNCOMBI-MAX        PIC 9(05).
                       09  SPA-GMN-HKNCOMBILST         OCCURS  30.
                           11  SPA-GMN-HKNCOMBIL       PIC X(04).
                           11  SPA-GMN-HKNCOMBIFLL     PIC X(01).
                           11  SPA-GMN-HKNCOMBIMEIL    PIC X(46).
                   07  SPA-GMN-NYUKBNLST-G.
                       09  SPA-GMN-NYUKBN-MAX          PIC 9(05).
                       09  SPA-GMN-NYUKBNLST           OCCURS  2.
                           11  SPA-GMN-NYUKBNL         PIC X(2).
                           11  SPA-GMN-NYUKBNFLL       PIC X(1).
                           11  SPA-GMN-NYUKBNMEIL      PIC X(8).
                   07  SPA-GMN-TOKNYUINLST-G.
                       09  SPA-GMN-TOKNYUIN-SRYCDTTL   PIC 9(14).
                       09  SPA-GMN-TOKNYUIN-MAX        PIC 9(05).
                       09  SPA-GMN-TOKNYUINLST-OCC     OCCURS  30.
                           11  SPA-GMN-TOKNYUINLST.
                               13  SPA-GMN-TOKNYUINL       PIC X(02).
                               13  SPA-GMN-TOKNYUINFLL     PIC X(01).
                               13  SPA-GMN-TOKNYUINMEIL    PIC X(60).
                           11  SPA-GMN-TOKNYUINL2.
                               13  SPA-GMN-TOKNYUINL2-KBN    PIC X(01).
                               13  SPA-GMN-TOKNYUINL2-NYUIN  PIC X(02).
                           11  SPA-GMN-TOKNYUINL3.
                               13  SPA-GMN-TOKNYUINL3-NYUINRYOKBN
                                                           PIC X(02).
                   07  SPA-GMN-SKJTIMELST-G.
                       09  SPA-GMN-SKJTIME-MAX         PIC 9(05).
                       09  SPA-GMN-SKJTIMELST          OCCURS  5.
                           11  SPA-GMN-SKJTIMEL        PIC X(2).
                           11  SPA-GMN-SKJTIMEFLL      PIC X(1).
                           11  SPA-GMN-SKJTIMEMEIL     PIC X(8).
               05  SPA-NAI-KJNYMD                  PIC X(08).
               05  SPA-NAI-HEAD-AREA.
                   07  SPA-NAI-KARTE-KEY           PIC X(36).
                   07  SPA-NAI-ORDERNUM            PIC 9(02).
                   07  SPA-NAI-STATUS-ORDER        PIC 9(01).
                   07  SPA-NAI-HIDOYMD             PIC X(08).
                   07  SPA-NAI-ORDERID             PIC X(04).
                   07  SPA-NAI-PTID                PIC X(20).
                   07  SPA-NAI-BIRTHDAY            PIC X(08).
                   07  SPA-NAI-HSRYKA              PIC X(02).
                   07  SPA-NAI-HHKNCOMBI           PIC X(04).
                   07  SPA-NAI-MAEIDOYMD           PIC X(08).
                   07  SPA-NAI-DIRECT-KHNERR       PIC 9(01).
                   07  SPA-NAI-ORDER-DAT           PIC X(1000).
                   07  SPA-NAI-INF-CHK             PIC 9(05).
               05  SPA-NAI-INPUT-AREA.
                   07  SPA-NAI-STATUS-G.
                       09  SPA-NAI-STS-NYUKBN      PIC 9(01).
                       09  SPA-NAI-STS-BRMNUM      PIC 9(01).
                       09  SPA-NAI-STS-BTUNUM      PIC 9(01).
                       09  SPA-NAI-STS-SAGAKU      PIC 9(01).
                       09  SPA-NAI-STS-SHOKAI      PIC 9(01).
                       09  SPA-NAI-STS-TAIINCD     PIC 9(01).
                       09  SPA-NAI-STS-DR          PIC 9(01)
                                                   OCCURS 3.
                       09  SPA-NAI-STS-TOKNYUIN    PIC 9(01).
                       09  SPA-NAI-STS-HKNCOMBI    PIC 9(01).
                       09  SPA-NAI-STS-NYUINKA     PIC 9(01).
                       09  SPA-NAI-STS-SKJTIME     PIC 9(01).
                       09  SPA-NAI-STS-NISSU       PIC 9(01).
                   07  SPA-NAI-BTU-G.
                       09  SPA-NAI-BTU-KHNSRYCD    PIC X(09).
                       09  SPA-NAI-BTU-SBT         PIC X(02).
                       09  SPA-NAI-BTU-BTUTOK      PIC X(02).
                       09  SPA-NAI-BTU-BRMTOK      PIC X(02).
                       09  SPA-NAI-BTU-BTUKHNKBNCD PIC X(03).
                       09  SPA-NAI-BTU-BRMKHNKBNCD PIC X(03).
                   07  SPA-NAI-NYUINYMD            PIC X(08).
                   07  SPA-NAI-SHONYUINYMD         PIC X(08).
                   07  SPA-NAI-TAIINYMD            PIC X(08).
                   07  SPA-NAI-TOKUTEI-G.
                       09  SPA-NAI-TOKUTEI-KBN     PIC X(01).
                       09  SPA-NAI-TOKUTEI-NYUIN   PIC X(02).
                   07  SPA-NAI-KHNNYUIN            PIC X(60).
                   07  SPA-NAI-SELNYUKHN           PIC 9(01).
                   07  SPA-NAI-HKNNUM              PIC X(03).
               05  SPA-NAI-ALLIDOLIST-INPUT-AREA.
                   07  SPA-NAI-UKEYMD              PIC X(08).
               05  SPA-NAI-ALLIDOLST-AREA.
                   07  SPA-NAI-ALLIDOLST-OCC       OCCURS  50.
                       09  SPA-NAI-TALLHOSPNUM     PIC 9(02).
                       09  SPA-NAI-TALLKARTE-KEY   PIC X(36).
                       09  SPA-NAI-TALLORDERNUM    PIC 9(02).
                       09  SPA-NAI-TALLIDOYMD      PIC X(08).
                       09  SPA-NAI-TALLORDERID     PIC X(04).
                       09  SPA-NAI-TALLPTID        PIC 9(10).
               05  SPA-NAI-KJNIDOLST-AREA.
                   07  SPA-NAI-KJNIDOLST-OCC       OCCURS  50.
                       09  SPA-NAI-TKJNKARTE-KEY   PIC X(36).
                       09  SPA-NAI-TKJNORDERNUM    PIC 9(02).
                       09  SPA-NAI-TKJNUKEYMD      PIC X(10).
                       09  SPA-NAI-TKJNIDOYMD      PIC X(10).
                       09  SPA-NAI-TKJNORDERID     PIC X(04).
               05  SPA-NAI-BTUDEF-AREA.
                   07  SPA-NAI-BTUDEF-MAX          PIC 9(05).
                   07  SPA-NAI-BTUDEF-OCC          OCCURS  50.
                       09  SPA-NAI-BTUDEF-BTUNUM   PIC X(02).
                       09  SPA-NAI-BTUDEF-SAGAKU   PIC X(02).
                       09  SPA-NAI-BTUDEF-NYUINKA  PIC X(02).
                       09  SPA-NAI-BTUDEF-KHNSRYCD PIC X(09).
                       09  SPA-NAI-BTUDEF-BTUTOK   PIC X(02).
                       09  SPA-NAI-BTUDEF-BRMTOK   PIC X(02).
                       09  SPA-NAI-BTUDEF-SBT      PIC X(02).
                       09  SPA-NAI-BTUDEF-BTUKHNKBNCD
                                                   PIC X(03).
                       09  SPA-NAI-BTUDEF-BRMKHNKBNCD
                                                   PIC X(03).
               05  SPA-NAI-COMBO-AREA.
                   07  SPA-NAI-HKNCOMBILST-G.
                       09  SPA-NAI-HKNCOMBILST     OCCURS  30.
                           11  SPA-NAI-HKNNUML     PIC X(03).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-PTHKNINF                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-HIZUKE-ERR              PIC 9(01).
           03  FLG-ALLIDOLST-ROW           PIC 9(01).
           03  FLG-KJNIDOLST-ROW           PIC 9(01).
           03  FLG-ERRMESLST-ROW           PIC 9(01).
           03  FLG-CMB-EDIT                PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-ORDER                   PIC 9(01).
           03  FLG-ODRITEM                 PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-ORCSNUM                 PIC 9(01).
           03  FLG-KHNERR                  PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-TARGET                  PIC 9(01).
           03  FLG-RYOTARGET               PIC 9(01).
           03  FLG-ROUJIN                  PIC 9(01).
           03  FLG-LIFE-RJN                PIC 9(01).
           03  FLG-RYOYOKHN                PIC 9(01).
           03  FLG-PASS                    PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-MAX                     PIC 9(05).
           03  CNT-STR                     PIC 9(05).
           03  CNT-ORDER                   PIC 9(05).
           03  CNT-DRCD                    PIC 9(05).
           03  CNT-SKIP                    PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX10                       PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-LST                     PIC 9(05).
           03  IDX-MAX                     PIC 9(05).
           03  IDX-HEAD                    PIC 9(05).
           03  IDX-ITM                     PIC 9(05).
           03  IDX-LIFE                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-IDX             PIC 9(05).
               05  WRK-CMB-ITM             PIC X(200).
               05  WRK-CMB-ITM2            PIC X(200).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD                    PIC X(08).
           03  WRK-HENYMDG                 PIC X(10).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-NUM-EDIT.
               05  WRK-99                  PIC 9(02).
               05  WRK-99999               PIC 9(05).
               05  WRK-999999              PIC 9(06).
               05  WRK-ZZ9                 PIC ZZ9.
               05  WRK-ZZZ9                PIC ZZZ9.
               05  WRK-ZZZZ9               PIC ZZZZ9.
               05  WRK-ZZZZZ9              PIC ZZZZZ9.
           03  WRK-SAGAKU.
               05  WRK-SAGAKU-NUM          PIC ZZZZZ9.
               05  WRK-SAGAKU-EN           PIC X(02).
           03  WRK-TNS-SRYCD               PIC X(09).
           03  WRK-TNS-KJNYMD              PIC X(08).
           03  WRK-TOKNYUIN.
               05  WRK-TOKNYUIN-KBN        PIC X(01).
               05  WRK-TOKKBNCD            PIC X(02).
           03  WRK-CHKTOKNYUIN.
               05  WRK-CHKTOKNYUIN-KBN     PIC X(01).
               05  WRK-CHKTOKNYUINCD       PIC X(02).
           03  WRK-TAIHI-HKN               PIC X(04).
           03  WRK-I0IDMSG                 PIC X(80). 
           03  WRK-INPUT                   PIC X(100).
           03  WRK-PTINF-PTID              PIC 9(10).
           03  WRK-AGE-G.
               05  WRK-AGE-Z               PIC ---9.
               05  WRK-AGE-MEI             PIC X(04).
           03  WRK-SHONUM                  PIC 9(02).
           03  WRK-LBL-SHONYUINYMD         PIC X(20).
           03  WRK-SAGAKUCD                PIC X(02).
           03  WRK-SAGAKUNM                PIC 9(08).
           03  WRK-SNUM-INX                PIC X(26).
           03  WRK-SNUM-OUTNUM             PIC 9(12).
           03  WRK-STR                     PIC X(500).
           03  WRK-BTUDEF-G.
               05  WRK-BTUDEF-SAGAKU       PIC X(02).
               05  WRK-BTUDEF-NYUINKA      PIC X(02).
           03  WRK-TAIINCD                 PIC X(02).
           03  WRK-INF-DIALOG              PIC 9(02).
           03  WRK-I0ID1-FLG               PIC X(02).
           03  WRK-I0IDCD                  PIC X(04).
           03  WRK-PTNUM-PTID              PIC 9(10).
           03  WRK-PTNUM-PTNUM             PIC X(20).
           03  WRK-ALLORDERID              PIC X(04).
           03  WRK-SKIP-MAX                PIC 9(05).
           03  WRK-TALLNUM                 PIC 9(05).
           03  WRK-NUMSRYCD                PIC 9(09).
           03  WRK-TOKNYUIN-SRYCDTTL       PIC 9(14).
           03  WRK-ERRMSG                  PIC X(80).
           03  WRK-TOK-G.
               05  WRK-TOK-SRYCD           PIC X(09).
               05  WRK-TOK-NAME            PIC X(60).
               05  WRK-TOK-NYUINRYOKBN     PIC X(02).
      *
       01  WRK-ROW-AREA.
           03  WRK-ALLIDOLST-ROW           PIC S9(9).
           03  WRK-KJNIDOLST-ROW           PIC S9(9).
           03  WRK-ERRMESLST-ROW           PIC S9(9).
      *
       01  ITEM-AREA.
           03  ITEM-LASTNYINYMD            PIC X(08).
           03  ITEM-TOKNYUIN-G.
               05  ITEM-TOKNYUIN-KBN       PIC X(01).
               05  ITEM-TOKNYUIN-NYUIN     PIC X(02).
      *
       01  CONST-AREA.
           03  CONST-H240401               PIC X(08)   VALUE "20120401".
           03  CONST-H220401               PIC X(08)   VALUE "20100401".
           03  CONST-H200401               PIC X(08)   VALUE "20080401".
           03  CONST-H181001               PIC X(08)   VALUE "20061001".
           03  CONST-H180701               PIC X(08)   VALUE "20060701".
           03  CONST-ERRCD9001             PIC X(04)   VALUE "9001".
           03  CONST-BTUNAME-MAX           PIC 9(05)   VALUE 50.
           03  CONST-SAGAKU-MAX            PIC 9(05)   VALUE 20.
           03  CONST-NYUINKA-MAX           PIC 9(05)   VALUE 99.
           03  CONST-TAIINCD-MAX           PIC 9(05)   VALUE 50.
           03  CONST-DR-MAX                PIC 9(05)   VALUE 99.
           03  CONST-HKNCOMBI-MAX          PIC 9(05)   VALUE 30.
           03  CONST-TOKNYUIN-MAX          PIC 9(05)   VALUE 30.
           03  CONST-ALLIDOLST-MAX         PIC 9(05)   VALUE 50.
           03  CONST-KJNIDOLST-MAX         PIC 9(05)   VALUE 50.
           03  CONST-ERRCD-MAX             PIC 9(05)   VALUE 10.
           03  CONST-ODRITEM-MAX           PIC 9(05)   VALUE 30.
           03  CONST-TOK-LIFECD-G.
               05  CONST-TOK-LIFECD-VAL.
                   07                      PIC X(03)   VALUE   "213".
                   07                      PIC X(03)   VALUE   "216".
      *
                   07                      PIC X(03)   VALUE   "218".
                   07                      PIC X(03)   VALUE   "219".
      *
                   07                      PIC X(03)   VALUE   "220".
                   07                      PIC X(03)   VALUE   "221".
      *
                   07                      PIC X(03)   VALUE   "227".
                   07                      PIC X(03)   VALUE   "228".
      *
                   07                      PIC X(03)   VALUE   "229".
                   07                      PIC X(03)   VALUE   "230".
      *
                   07                      PIC X(03)   VALUE   "231".
                   07                      PIC X(03)   VALUE   "232".
      *
                   07                      PIC X(03)   VALUE   "302".
                   07                      PIC X(03)   VALUE   "349".
      *
                   07                      PIC X(03)   VALUE   "321".
                   07                      PIC X(03)   VALUE   "350".
      *
                   07                      PIC X(03)   VALUE   "325".
                   07                      PIC X(03)   VALUE   "337".
      *
                   07                      PIC X(03)   VALUE   "326".
                   07                      PIC X(03)   VALUE   "338".
      *
                   07                      PIC X(03)   VALUE   "327".
                   07                      PIC X(03)   VALUE   "339".
      *
                   07                      PIC X(03)   VALUE   "328".
                   07                      PIC X(03)   VALUE   "340".
      *
                   07                      PIC X(03)   VALUE   "329".
                   07                      PIC X(03)   VALUE   "341".
      *
                   07                      PIC X(03)   VALUE   "330".
                   07                      PIC X(03)   VALUE   "342".
      *
                   07                      PIC X(03)   VALUE   "331".
                   07                      PIC X(03)   VALUE   "343".
      *
                   07                      PIC X(03)   VALUE   "332".
                   07                      PIC X(03)   VALUE   "344".
      *
                   07                      PIC X(03)   VALUE   "333".
                   07                      PIC X(03)   VALUE   "345".
      *
                   07                      PIC X(03)   VALUE   "334".
                   07                      PIC X(03)   VALUE   "346".
      *
                   07                      PIC X(03)   VALUE   "335".
                   07                      PIC X(03)   VALUE   "347".
      *
                   07                      PIC X(03)   VALUE   "336".
                   07                      PIC X(03)   VALUE   "348".
      *
                   07                      PIC X(03)   VALUE   "323".
                   07                      PIC X(03)   VALUE   "351".
      *
                   07                      PIC X(03)   VALUE   "352".
                   07                      PIC X(03)   VALUE   "353".
      *
                   07                      PIC X(03)   VALUE   "363".
                   07                      PIC X(03)   VALUE   "364".
      *
                   07                      PIC X(03)   VALUE   "365".
                   07                      PIC X(03)   VALUE   "366".
      *
                   07                      PIC X(03)   VALUE   "367".
                   07                      PIC X(03)   VALUE   "368".
      *
                   07                      PIC X(03)   VALUE   "369".
                   07                      PIC X(03)   VALUE   "370".
      *
                   07                      PIC X(03)   VALUE   "371".
                   07                      PIC X(03)   VALUE   "372".
      *
                   07                      PIC X(03)   VALUE   "373".
                   07                      PIC X(03)   VALUE   "374".
      *
                   07                      PIC X(03)   VALUE   "375".
                   07                      PIC X(03)   VALUE   "376".
      *
                   07                      PIC X(03)   VALUE   "377".
                   07                      PIC X(03)   VALUE   "378".
      *
                   07                      PIC X(03)   VALUE   "379".
                   07                      PIC X(03)   VALUE   "380".
      *
                   07                      PIC X(03)   VALUE   "381".
                   07                      PIC X(03)   VALUE   "382".
      *
                   07                      PIC X(03)   VALUE   "383".
                   07                      PIC X(03)   VALUE   "384".
      *
                   07                      PIC X(03)   VALUE   "385".
                   07                      PIC X(03)   VALUE   "386".
      *
                   07                      PIC X(03)   VALUE   "387".
                   07                      PIC X(03)   VALUE   "388".
               05  CONST-TOK-LIFECD-R  REDEFINES   CONST-TOK-LIFECD-VAL.
                   07  CONST-TOK-LIFECD-OCC    OCCURS  35.
                       09  CONST-TOK-LIFECD-IPN
                                           PIC X(03).
                       09  CONST-TOK-LIFECD-LIFE
                                           PIC X(03).
               05  CONST-TOK-LIFECD-MAX    PIC 9(05)   VALUE   35.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WRK-SPA-//.
      *
      *
      *    項目IDコピー句
           COPY    "CPNCLAIMID.INC".
      *
      *    ORDERIDコピー句
           COPY    "CPNORDERID.INC".
      *
      *    エラーコード
           COPY    "CPNCLAIMERRCD.INC".
      *
      *
       01  ERR-ORDER-ERRCD             PIC X(04).
       01  ERR-ODRITEM-REC.
           02  ERR-ODRITEM-MAX         PIC 9(03).
           02  ERR-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //ERR-ODRITEM-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    （診療科入院科情報）
           COPY    "CPSK1005.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *    （医療機関情報）
           COPY    "CPSK5000.INC".
      *    （病棟情報）
           COPY    "CPSK5001.INC".
      *    （病室情報）
           COPY    "CPSK5002.INC".
      *    （室料差額）
           COPY    "CPSK5113.INC".
      *    （特定入院料−病棟）
           COPY    "CPSK5110.INC".
      *    （特定入院料−病室）
           COPY    "CPSK5111.INC".
      *
           COPY    "CPSK5013.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    診療行為算定日一括入力
           COPY    "CPORCSNDAYCHG.INC".
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
      *
      *    病室名称編集
           COPY    "CPORCSBRMNUM.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    年齢計算
           COPY    "CPORCSAGECHK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "I01.INC".
           COPY    "I02.INC".
           COPY    "I03.INC".
           COPY    "I04.INC".
           COPY    "I05.INC".
           COPY    "I06.INC".
           COPY    "I0ERR.INC".
           COPY    "I0ID1.INC".
           COPY    "I0ID2.INC".
           COPY    "I041.INC".
      *
       PROCEDURE                                 DIVISION    USING
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
           MOVE    SPA-FREE        TO  SPA-I06
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I06         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "I0ERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END     =   1 )
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
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "I06"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
      *
               MOVE    SPA-MOTOPG      TO  SPA-MOTOPG2
      *
               MOVE    SPACE           TO  SPA-I0ID1-FLG
               MOVE    SPACE           TO  SPA-I0IDCD
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "I0ID1"
               PERFORM 330-I0ID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-I06
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-UKEYMD
           MOVE    SPA-NAI-UKEYMD  TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-UKEYMD
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-KJNYMD
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           MOVE    1               TO  SPA-GMN-ALLIDOLST-PAGE
      *
      *    異動リスト編集処理
           PERFORM 3101-ALLIDOLST-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
      *    室料差額設定処理
           PERFORM 3101-SAGAKULST-SET-SEC
      *
      *    入院科設定処理
           PERFORM 3101-NYUINKALST-SET-SEC
      *
      *    初回設定処理
           PERFORM 3101-SHOKAILST-SET-SEC
      *
      *    担当医設定処理
           PERFORM 3101-DRLST-SET-SEC
      *
      *    入院区分設定処理
           PERFORM 3101-NYUKBNLST-SET-SEC
      *
      *    食事時間設定処理
           PERFORM 3101-SKJTIMELST-SET-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    病棟（病室）基本情報編集処理
      *****************************************************************
       3101-BTUDEF-SEC                 SECTION.
      *
           INITIALIZE                  SPA-NAI-BTUDEF-AREA
      *
           INITIALIZE              ORCSBRMNUMAREA
           MOVE    "2"             TO  SBRMNUM-IN-PROCKBN
           MOVE    SPA-GMN-BRMNUM  TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-MAX
      *
           PERFORM UNTIL ( FLG-SYSKANRI        =   1 )
                   OR    ( IDX-MAX            >= CONST-BTUNAME-MAX  )
      *
               MOVE    MCPDATA-REC TO  SYS-5002-REC
      *
               IF    ( SYS-5002-KBNCD (3:)     =   SBRMNUM-OT-BRMNUM )
                   COMPUTE IDX-MAX =   IDX-MAX +   1
                   MOVE    SYS-5002-KBNCD (1 : 2)
                                           TO  SPA-NAI-BTUDEF-BTUNUM
                                                           (IDX-MAX)
                   MOVE    SYS-5002-BRM-SAGAKU
                                           TO  SPA-NAI-BTUDEF-SAGAKU
                                                           (IDX-MAX)
                   MOVE    SYS-5002-BRM-KA TO  SPA-NAI-BTUDEF-NYUINKA
                                                           (IDX-MAX)
                   MOVE    SYS-5002-BRM-TOKTEI-NYUIN
                                           TO  SPA-NAI-BTUDEF-BRMTOK
                                                           (IDX-MAX)
                   MOVE    SYS-5002-KHNKBNCD
                                       TO  SPA-NAI-BTUDEF-BRMKHNKBNCD
                                                             (IDX-CMB)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    IDX-MAX         TO  SPA-NAI-BTUDEF-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       3101-BTUDEF-EXT.
           EXIT.
      *****************************************************************
      *    病棟設定処理
      *****************************************************************
       3101-BTUNAMELST-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-BTUNAMELST-G
      *
           MOVE    ZERO            TO  CNT-MAX
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT  =  ZERO )
                    OR   ( CNT-MAX     >=  CONST-BTUNAME-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   SPA-NAI-BTUDEF-MAX )
      *
                   IF    ( SPA-NAI-BTUDEF-BTUNUM (IDX-CMB)
                                       =   SYS-5001-KBNCD )
                       COMPUTE CNT-MAX =   CNT-MAX +   1
                       MOVE    SYS-5001-BTU-NUM
                                   TO  SPA-GMN-BTUNAMEL (CNT-MAX)
                       MOVE    SYS-5001-BTU-NAME
                                   TO  SPA-GMN-BTUNAMEMEIL (CNT-MAX)
      *
                       IF    ( SPA-NAI-BTUDEF-NYUINKA (IDX-CMB)
                                       =   SPACE )
                           MOVE    SYS-5001-BTU-KA
                                   TO  SPA-NAI-BTUDEF-NYUINKA (IDX-CMB)
                       END-IF
      *
                       MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  SPA-NAI-BTUDEF-KHNSRYCD (IDX-CMB)
                       MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                   TO  SPA-NAI-BTUDEF-BTUTOK (IDX-CMB)
                       MOVE    SYS-5001-BTU-SBT
                                   TO  SPA-NAI-BTUDEF-SBT (IDX-CMB)
                       MOVE    SYS-5001-KHNKBNCD
                                   TO  SPA-NAI-BTUDEF-BTUKHNKBNCD
                                                             (IDX-CMB)
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    CNT-MAX                 TO  SPA-GMN-BTUNAME-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-BTUNAMELST-SET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額設定処理
      *****************************************************************
       3101-SAGAKULST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SAGAKULST-G
      *
           MOVE    ZERO            TO  CNT-MAX
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5113"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT  =  ZERO )
                    OR   ( CNT-MAX     >=  CONST-SAGAKU-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
               COMPUTE CNT-MAX =   CNT-MAX +   1
      *
               MOVE    SYS-5113-KBNCD
                               TO  SPA-GMN-SAGAKUL (CNT-MAX)
               MOVE    SYS-5113-BRM-SAGAKU-NM
                               TO  WRK-SAGAKU-NUM
               MOVE    "円"    TO  WRK-SAGAKU-EN
               MOVE    WRK-SAGAKU
                               TO  SPA-GMN-SAGAKUMEIL (CNT-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    CNT-MAX                 TO  SPA-GMN-SAGAKU-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-SAGAKULST-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院科設定処理
      *****************************************************************
       3101-NYUINKALST-SET-SEC         SECTION.
      *
      *
           INITIALIZE                  SPA-GMN-NYUINKALST-G
      *
           MOVE    ZERO            TO  CNT-MAX
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT  =  ZERO )
                    OR   ( CNT-MAX     >=  CONST-NYUINKA-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
               COMPUTE CNT-MAX =   CNT-MAX +   1
      *
               MOVE    SYS-1005-KBNCD
                               TO  SPA-GMN-NYUINKAL    (CNT-MAX)
               MOVE    SYS-1005-SRYKANAME
                               TO  SPA-GMN-NYUINKAMEIL (CNT-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    CNT-MAX                 TO  SPA-GMN-NYUINKA-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-NYUINKALST-SET-EXT.
           EXIT.
      *****************************************************************
      *    初回設定処理
      *****************************************************************
       3101-SHOKAILST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SHOKAILST-G
      *
           MOVE    2                   TO  SPA-GMN-SHOKAI-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-SHOKAIL    (1)
           MOVE    "初回"              TO  SPA-GMN-SHOKAIMEIL (1)
      *
           MOVE    "2"                 TO  SPA-GMN-SHOKAIL    (2)
           MOVE    "継続"              TO  SPA-GMN-SHOKAIMEIL (2)
      *
           . 
       3101-SHOKAILST-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院事由設定処理
      *****************************************************************
       3101-TAIINCDLST-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-TAIINCDLST-G
      *
           MOVE    ZERO            TO  CNT-MAX
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5013"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT  =  ZERO )
                    OR   ( CNT-MAX     >=  CONST-TAIINCD-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5013-REC
      *
               COMPUTE CNT-MAX =   CNT-MAX +   1
      *
               MOVE    SYS-5013-KBNCD
                               TO  SPA-GMN-TAIINCDL    (CNT-MAX)
               MOVE    SYS-5013-NAME
                               TO  SPA-GMN-TAIINCDMEIL (CNT-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    CNT-MAX                 TO  SPA-GMN-TAIINCD-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-TAIINCDLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    担当医設定処理
      *****************************************************************
       3101-DRLST-SET-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-DRLST-G
      *
           MOVE    ZERO            TO  CNT-MAX
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1010"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT  =  ZERO )
                    OR   ( CNT-MAX     >=  CONST-DR-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1010-REC
      *
               IF    ( SYS-1010-KBNCD (1 : 1)  =   "1" )
      *
                   COMPUTE CNT-MAX =   CNT-MAX +   1
      *
                   MOVE    SYS-1010-KBNCD (2 : 4)
                               TO  SPA-GMN-DRL    (CNT-MAX)
                   MOVE    SYS-1010-NAME
                               TO  SPA-GMN-DRMEIL (CNT-MAX)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    CNT-MAX                 TO  SPA-GMN-DR-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-DRLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ設定処理
      *****************************************************************
       3101-HKNCOMBILST-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNCOMBILST-G
                                       SPA-NAI-HKNCOMBILST-G
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           INITIALIZE                  WRK-SPA-KYOTUKEY
                                       WRK-SPA-KYOTU-SET
                                       ORCSHKNSETAREA
      *
           MOVE    SPA-NAI-KJNYMD      TO  WRK-SPA-SRYYMD
           MOVE    SPA-GMN-NYUINKA
                                       TO  WRK-SPA-SRYKA
           MOVE    "1"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-NAI-PTID        TO  WRK-SPA-PTID
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-SPA-BIRTHDAY
           MOVE    SPA-GMN-HKNCOMBI    TO  WRK-SPA-HKNCOMBINUM
           MOVE    "1"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   WRK-SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               GO  TO  3101-HKNCOMBILST-SET-EXT
           END-IF
      *
           IF    ( ORCSHKN-HKN-MAX     >   CONST-HKNCOMBI-MAX )
               MOVE    30              TO  SPA-GMN-HKNCOMBI-MAX
           ELSE
               MOVE    ORCSHKN-HKN-MAX TO  SPA-GMN-HKNCOMBI-MAX
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-HKNCOMBI-MAX )
      *
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX0)
                                       TO  SPA-GMN-HKNCOMBIL   (IDX0)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX0)
                                       TO  SPA-GMN-HKNCOMBIMEIL(IDX0)
               MOVE    ORCSHKN-NAI-HKNNUM (IDX0)
                                       TO  SPA-NAI-HKNNUML   (IDX0)
      *
           END-PERFORM
      *
      *    IF    ( ORCSHKN-IDX    >=   1 )
      *     AND  ( ORCSHKN-IDX    <=   SPA-GMN-HKNCOMBI-MAX )
      *        MOVE    SPA-GMN-HKNCOMBILST (ORCSHKN-IDX)
      *                                TO  SPA-GMN-HKNCOMBI-G
      *    END-IF
      *
           . 
       3101-HKNCOMBILST-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院区分設定処理
      *****************************************************************
       3101-NYUKBNLST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-NYUKBNLST-G
      *
           MOVE    2                   TO  SPA-GMN-NYUKBN-MAX
      *
           MOVE    "01"                TO  SPA-GMN-NYUKBNL    (1)
           MOVE    "一般入院"          TO  SPA-GMN-NYUKBNMEIL (1)
      *
           MOVE    "02"                TO  SPA-GMN-NYUKBNL    (2)
           MOVE    "特定入院"          TO  SPA-GMN-NYUKBNMEIL (2)
      *
           . 
       3101-NYUKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事時間設定処理
      *****************************************************************
       3101-SKJTIMELST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJTIMELST-G
      *
           MOVE    3                   TO  SPA-GMN-SKJTIME-MAX
      *
           MOVE    "01"                TO  SPA-GMN-SKJTIMEL    (1)
           MOVE    "朝"                TO  SPA-GMN-SKJTIMEMEIL (1)
      *
           MOVE    "02"                TO  SPA-GMN-SKJTIMEL    (2)
           MOVE    "昼"                TO  SPA-GMN-SKJTIMEMEIL (2)
      *
           MOVE    "03"                TO  SPA-GMN-SKJTIMEL    (3)
           MOVE    "夕"                TO  SPA-GMN-SKJTIMEMEIL (3)
      *
           . 
       3101-SKJTIMELST-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料設定処理
      *****************************************************************
       3101-TOKNYUINLST-SET-SEC        SECTION.
      *
           INITIALIZE  SPA-GMN-TOKNYUINLST-G
                       SPA-NAI-KHNNYUIN
                       SPA-NAI-SELNYUKHN
      *
           MOVE    ZERO            TO  CNT-MAX
           MOVE    ZERO            TO  FLG-CMB-EDIT
      *
           PERFORM 800-ROUJINAGE-CHK-SEC
      *
           IF    ( SPA-NAI-BTU-KHNSRYCD    NOT =   SPACE )
               MOVE    SPA-NAI-BTU-KHNSRYCD    TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-KJNYMD          TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME    TO   SPA-NAI-KHNNYUIN
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-GMN-BTUNAME TO  SYS-KBNCD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    MCPDATA-REC TO  SYS-5001-REC
      *
           IF    ( SPA-NAI-KJNYMD         >=   CONST-H180701 )
            AND  ( SPA-NAI-BTU-SBT     =   "04" OR "09" )
      *
               IF    ( SPA-NAI-BTU-SBT =   "04" )
                   MOVE    1           TO  FLG-RYOYOKHN
               ELSE
                   MOVE    2           TO  FLG-RYOYOKHN
               END-IF
               PERFORM 410111-RYOYOKHN-EDIT-SEC
           END-IF
      *
           IF    ( SPA-NAI-BTU-BTUTOK  NOT =   SPACE )
      *
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-BTU-BTUTOK  =   "01" OR "02" OR "14"
                                            OR "17" OR "26" )
      *
                   INITIALIZE ORCSAGECHKAREA
                   MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
                   MOVE    SPA-NAI-PTID        TO  AGECHK-PTID
                   MOVE    SPA-NAI-BIRTHDAY    TO  AGECHK-BIRTHDAY
                   MOVE    SPA-NAI-KJNYMD      TO  AGECHK-SRYYMD
      *
                   CALL    "ORCSAGECHK"    USING ORCSAGECHKAREA
                                                 SPA-AREA
                   IF    ( AGECHK-NENREI2-YY   <   15 )
                       MOVE    "2"     TO  WRK-TOKNYUIN-KBN
                       MOVE    SPA-NAI-BTU-BTUTOK
                                       TO  WRK-TOKKBNCD
                       PERFORM 31011-TOKNYUINLST-SET-SEC
                   END-IF
               WHEN    OTHER
                   MOVE    "2"     TO  WRK-TOKNYUIN-KBN
                   MOVE    SPA-NAI-BTU-BTUTOK
                                   TO  WRK-TOKKBNCD
                   PERFORM 31011-TOKNYUINLST-SET-SEC
               END-EVALUATE
      *
               IF   (  SPA-NAI-BTU-BTUTOK  =   "11" )
                AND (  SPA-NAI-ORDERID NOT =   NORDERID-NYUINID )
                   MOVE    "24"        TO  WRK-TOKKBNCD
                   MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                   PERFORM 31011-TOKNYUINLST-SET-SEC
               END-IF
      *
               IF   (  SPA-NAI-BTU-BTUTOK  =   "12" )
                AND (  SPA-NAI-ORDERID NOT =   NORDERID-NYUINID )
                       MOVE    "25"    TO  WRK-TOKKBNCD
                       MOVE    "2"     TO  WRK-TOKNYUIN-KBN
                   PERFORM 31011-TOKNYUINLST-SET-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-BTU-BRMTOK  NOT =   SPACE )
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-BTU-BRMTOK  =   "02" )
      *
                   MOVE    "02"        TO  WRK-TOKKBNCD
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   PERFORM 31011-TOKNYUINLST-SET-SEC
      *
                   MOVE    "23"        TO  WRK-TOKKBNCD
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   PERFORM 31011-TOKNYUINLST-SET-SEC
      *
               WHEN    OTHER
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   MOVE    SPA-NAI-BTU-BRMTOK
                                   TO  WRK-TOKKBNCD
                   PERFORM 31011-TOKNYUINLST-SET-SEC
               END-EVALUATE
           END-IF
      *
      *
           IF    ( SPA-NAI-KJNYMD     >=  CONST-H240401 )
               EVALUATE  TRUE
               WHEN  ( SPA-NAI-BTU-SBT  =   "08" )
                AND  ( SYS-5001-CRYOYOKBN  =   "1" )
                   MOVE    4         TO  FLG-RYOYOKHN
                   PERFORM 410111-RYOYOKHN-EDIT-SEC
               WHEN  ( SPA-NAI-BTU-SBT  =   "09" )
                   MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                   EVALUATE    SYS-5001-CRYOYOKBN
                   WHEN    "1"
                       MOVE    "35"  TO  WRK-TOKKBNCD
                       PERFORM 31011-TOKNYUINLST-SET-SEC
                   WHEN    "2"
                       MOVE    "36"  TO  WRK-TOKKBNCD
                       PERFORM 31011-TOKNYUINLST-SET-SEC
                   WHEN    "3"
                       MOVE    "37"  TO  WRK-TOKKBNCD
                       PERFORM 31011-TOKNYUINLST-SET-SEC
                   END-EVALUATE
               WHEN  ( SYS-5001-BTU-TOKTEI-NYUIN
                                     =  "33" OR "34" )
                   MOVE    3         TO  FLG-RYOYOKHN
                   PERFORM 410111-RYOYOKHN-EDIT-SEC
               END-EVALUATE
           END-IF
      *
      *    老人の場合、老人一般病棟特定入院基本料をコンボに設定
      *    平成２２年４月から年齢要件廃止
           PERFORM 31011-ROUJIN-CHK-SEC
           IF  ( SPA-NAI-KJNYMD   >=  CONST-H220401 )
               IF    ( SPA-NAI-BTU-SBT  =   "01" OR "05" OR "06")
                   IF    ( SYS-5001-BTU-TOKTEI-NYUIN
                                           =   "33" OR "34" )
                       CONTINUE
                   ELSE
                       MOVE    "22"        TO  WRK-TOKKBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 31011-TOKNYUINLST-SET-SEC
      *
                       MOVE    "24"        TO  WRK-TOKKBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 31011-TOKNYUINLST-SET-SEC
                   END-IF
               END-IF
           ELSE
               IF  ( FLG-RJN   =   1 )
                   MOVE    "22"        TO  WRK-TOKKBNCD
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   PERFORM 31011-TOKNYUINLST-SET-SEC
      *
                   MOVE    "24"        TO  WRK-TOKKBNCD
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   PERFORM 31011-TOKNYUINLST-SET-SEC
               END-IF
           END-IF
      *
      *    短期滞在手術基本料をコンボに設定
           MOVE    "21"        TO  WRK-TOKKBNCD
           MOVE    "3"         TO  WRK-TOKNYUIN-KBN
           PERFORM 31011-TOKNYUINLST-SET-SEC
      *
           MOVE    "52"        TO  WRK-TOKKBNCD
           MOVE    "3"         TO  WRK-TOKNYUIN-KBN
           PERFORM 31011-TOKNYUINLST-SET-SEC
      *
           MOVE    CNT-MAX         TO  SPA-GMN-TOKNYUIN-MAX
      *
           . 
       3101-TOKNYUINLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    療養病棟入院基本料編集処理
      *****************************************************************
       410111-RYOYOKHN-EDIT-SEC         SECTION.
      *
           MOVE    1               TO  SPA-NAI-SELNYUKHN
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5111"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY23-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5111-REC
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    ZERO        TO  FLG-TARGET
      *
               IF    ( FLG-RYOYOKHN                =   1 OR 3)
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "05" )
                   IF    ( FLG-LIFE-RJN            =   ZERO OR 2)
                       IF    ( SYS-5111-SEIKATU-RYOKBN
                                                   =   SPACE )
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF    ( FLG-LIFE-RJN            =   1    OR 2)
                       IF    ( SYS-5111-SEIKATU-RYOKBN
                                               NOT =   SPACE )
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-RYOYOKHN                =   2 OR 4 )
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "06" )
                   IF    ( FLG-LIFE-RJN            =   ZERO OR 2)
                       IF    ( SYS-5111-SEIKATU-RYOKBN
                                                   =   SPACE )
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF    ( FLG-LIFE-RJN            =   1    OR 2)
                       IF    ( SYS-5111-SEIKATU-RYOKBN
                                               NOT =   SPACE )
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( SPA-NAI-KJNYMD >=  CONST-H220401 )
                   IF    ( SYS-5111-SRYCD
                                       =   "099999953"
                                        OR "099999954" )
                           MOVE  ZERO  TO  FLG-TARGET
                   END-IF
                   IF   (( FLG-RYOYOKHN    =   1 )
                    AND  ( SYS-5001-KHNSRYCD
                                       =   SPACE
                                  OR       "099999953"
                                  OR       "190121310"
                                  OR       "190121410"
                                  OR       "190121510"
                                  OR       "190121610"
                                  OR       "190121710"
                                  OR       "190131610"
                                  OR       "190131810"
                                  OR       "190132010"
                                  OR       "190132210"
                                  OR       "190121810" ))
                    OR   ( FLG-RYOYOKHN    =   3 )
                       IF    ( SYS-5111-SRYCD
                                       =   "190121310"
                                  OR       "190121410"
                                  OR       "190121510"
                                  OR       "190121610"
                                  OR       "190121710"
                                  OR       "190131610"
                                  OR       "190131810"
                                  OR       "190132010"
                                  OR       "190132210"
                                  OR       "190123710"
                                  OR       "190123810"
                                  OR       "190123910"
                                  OR       "190124010"
                                  OR       "190124110"
                                  OR       "190131710"
                                  OR       "190131910"
                                  OR       "190132110"
                                  OR       "190132310"
                                  OR       "190121810"
                                  OR       "190124210" )
                           CONTINUE
                       ELSE
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF   (( FLG-RYOYOKHN    =   1 )
                    AND  ( SYS-5001-KHNSRYCD
                                       =   "099999954"
                                  OR       "190132410"
                                  OR       "190132610"
                                  OR       "190132810"
                                  OR       "190133010"
                                  OR       "190133210"
                                  OR       "190133410"
                                  OR       "190133610"
                                  OR       "190133810"
                                  OR       "190134010"
                                  OR       "190121810" ))
                       IF    ( SYS-5111-SRYCD
                                       =   "190132410"
                                  OR       "190132610"
                                  OR       "190132810"
                                  OR       "190133010"
                                  OR       "190133210"
                                  OR       "190133410"
                                  OR       "190133610"
                                  OR       "190133810"
                                  OR       "190134010"
                                  OR       "190132510"
                                  OR       "190132710"
                                  OR       "190132910"
                                  OR       "190133110"
                                  OR       "190133310"
                                  OR       "190133510"
                                  OR       "190133710"
                                  OR       "190133910"
                                  OR       "190134110"
                                  OR       "190121810"
                                  OR       "190124210" )
                           CONTINUE
                       ELSE
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF   (  FLG-RYOYOKHN    =   3 )
                       IF    ( SYS-5111-SRYCD
                                  =        "190121810"
                                  OR       "190124210" )
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF   (  FLG-RYOYOKHN    =   4 )
                       IF    ( SYS-5111-SRYCD
                                  =        "190122510"
                                  OR       "190124810" )
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
      *
                   MOVE    "3"             TO  WRK-TOKNYUIN-KBN
                   MOVE    SYS-5111-KBNCD  TO  WRK-TOKKBNCD
                   PERFORM 310111-TOKNYUINLST-SET-SEC
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY23-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       410111-RYOYOKHN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料設定処理
      *****************************************************************
       31011-TOKNYUINLST-SET-SEC       SECTION.
      *
           MOVE    WRK-TOKNYUIN    TO  WRK-CHKTOKNYUIN
      *
           IF    ( SPA-NAI-BTU-SBT  =  "04" OR "09" )
            AND  ( FLG-LIFE-RJN     =  1  OR  2 )
               PERFORM VARYING IDX-LIFE    FROM    1   BY  1
                       UNTIL ( IDX-LIFE    >   CONST-TOK-LIFECD-MAX )
      *
                   IF    ( WRK-CHKTOKNYUIN =   CONST-TOK-LIFECD-IPN
                                                             (IDX-LIFE))
                       MOVE    CONST-TOK-LIFECD-LIFE (IDX-LIFE)
                                           TO  WRK-CHKTOKNYUIN
                       MOVE    CONST-TOK-LIFECD-MAX
                                           TO  IDX-LIFE
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           IF    ( SPA-NAI-BTU-SBT  =  "04" OR "09" )
            AND  ( FLG-LIFE-RJN     =  2 )
               IF    ( WRK-CHKTOKNYUIN NOT =    WRK-TOKNYUIN )
                   PERFORM 310111-TOKNYUINLST-SET-SEC
               END-IF
           END-IF
      *
           MOVE    WRK-CHKTOKNYUIN TO  WRK-TOKNYUIN
           PERFORM 310111-TOKNYUINLST-SET-SEC
      *
           . 
       31011-TOKNYUINLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料設定処理
      *****************************************************************
       310111-TOKNYUINLST-SET-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PASS
           INITIALIZE                      WRK-TOK-G
      *
           IF    ( WRK-TOKNYUIN-KBN    =   "2" )
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5110"          TO  SYS-KANRICD
               MOVE    WRK-TOKKBNCD    TO  SYS-KBNCD
               MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5110-REC
               MOVE    SYS-5110-SRYCD  TO  WRK-TOK-SRYCD
               MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                                       TO  WRK-TOK-NAME
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5111"          TO  SYS-KANRICD
               MOVE    WRK-TOKKBNCD    TO  SYS-KBNCD
               MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
               MOVE    SYS-5111-SRYCD  TO  WRK-TOK-SRYCD
               MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                       TO  WRK-TOK-NAME
               MOVE    SYS-5111-BRM-NYUINRYOKBN
                                       TO  WRK-TOK-NYUINRYOKBN
           END-IF
      *
           MOVE    WRK-TOK-SRYCD   TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-KJNYMD  TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       =   ZERO )
               IF    ( TNS-HOSPSRYKBN  =   0 )
                OR   ( TNS-HOSPSRYKBN  =   SPA-HOSPSBT )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-PASS
               END-IF
           ELSE
                   MOVE    1       TO  FLG-PASS
           END-IF
      *
           IF    ( CNT-MAX         >=  CONST-TOKNYUIN-MAX )
            OR   ( FLG-PASS         =   1 )
               CONTINUE
           ELSE
               COMPUTE CNT-MAX =   CNT-MAX +   1
      *
               MOVE    CNT-MAX     TO  WRK-99
      *
               MOVE    WRK-99      TO  SPA-GMN-TOKNYUINL   (CNT-MAX)
               MOVE    WRK-TOKNYUIN-KBN
                                   TO  SPA-GMN-TOKNYUINL2-KBN
                                                           (CNT-MAX)
               MOVE    WRK-TOKKBNCD
                                   TO  SPA-GMN-TOKNYUINL2-NYUIN
                                                           (CNT-MAX)
      *
               MOVE    WRK-TOK-NAME
                                   TO  SPA-GMN-TOKNYUINMEIL(CNT-MAX)
               MOVE    WRK-TOK-NYUINRYOKBN
                                   TO  SPA-GMN-TOKNYUINL3-NYUINRYOKBN
                                                           (CNT-MAX)
      *
               IF  ( WRK-TOKNYUIN-KBN  =   "3" )
                AND( WRK-TOKKBNCD      =   "23" OR "49" OR "51")
                   MOVE    TNS-NAME
                                   TO  SPA-GMN-TOKNYUINMEIL   (CNT-MAX)
               END-IF
      *
               IF    ( WRK-TOK-SRYCD   IS  NUMERIC )
                   MOVE    WRK-TOK-SRYCD   TO  WRK-NUMSRYCD
                   COMPUTE SPA-GMN-TOKNYUIN-SRYCDTTL
                       =   SPA-GMN-TOKNYUIN-SRYCDTTL
                       +   WRK-NUMSRYCD
               END-IF
           END-IF
      *
           . 
       310111-TOKNYUINLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    老人チェック処理
      *****************************************************************
       31011-ROUJIN-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-RJN
      *
      *    入院患者の老人保険判定
           INITIALIZE                       HKNCOMBI-REC
      *
           MOVE    SPA-HOSPNUM          TO   COMB-HOSPNUM
           MOVE    SPA-NAI-PTID        TO   COMB-PTID
           MOVE    SPA-GMN-HKNCOMBI    TO   COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-KEY-SEL-SEC 
      *
      *    入院患者の年齢計算
           INITIALIZE ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM          TO  AGECHK-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  AGECHK-PTID
           MOVE    SPA-NAI-KJNYMD      TO  AGECHK-SRYYMD
           MOVE    SPA-NAI-BIRTHDAY    TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING   ORCSAGECHKAREA
                                               SPA-AREA
           IF    ( AGECHK-RC       =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
           IF    ( SPA-NAI-KJNYMD     >=   CONST-H200401 )
      *
      *        患者保険情報から老人保健の登録チェック
               INITIALIZE                      PTHKNINF-REC
               MOVE    SPA-HOSPNUM          TO  PTHKN-HOSPNUM
               MOVE    SPA-NAI-PTID        TO  PTHKN-PTID
               MOVE    SPA-NAI-KJNYMD      TO  PTHKN-TEKSTYMD
                                               PTHKN-TEKEDYMD
               PERFORM 900-PTHKNINF-KEY11-SEL-SEC
               IF    ( FLG-PTHKNINF        =   ZERO )
                   MOVE    1               TO  FLG-RJN
               END-IF
      *
      *        患者公費情報から老人保健の登録チェック
               INITIALIZE                      PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-NAI-PTID        TO  PTKOH-PTID
               MOVE    "968"               TO  PTKOH-KOHNUM
               MOVE    SPA-NAI-KJNYMD      TO  PTKOH-TEKSTYMD
                                               PTKOH-TEKEDYMD
               PERFORM 900-PTKOHINF-KEY6-SEL-SEC
               IF    ( FLG-PTKOHINF        =   ZERO )
                   MOVE    1               TO  FLG-RJN
               END-IF
           ELSE
      *
      *        老人対象患者判定
               IF    ((FLG-HKNCOMBI        =   ZERO)
                  AND (COMB-KOH1HKNNUM     =   "027"))
                 OR  ( AGECHK-NENREI      >=    75)
                 OR  ((AGECHK-NENREI      >=    70)
                  AND (SPA-NAI-BIRTHDAY   <=   "19320930"))
                 OR  (AGECHK-ROUJIN        =    1)
                   MOVE    1               TO  FLG-RJN
               END-IF
           END-IF
      *
           .
       31011-ROUJIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    異動リスト編集処理
      *****************************************************************
       3101-ALLIDOLST-SEC              SECTION.
      *
           MOVE    1              TO   FLG-ALLIDOLST-ROW
      *
           INITIALIZE                  SPA-GMN-SELNUM
                                       SPA-GMN-ALLIDOLST-AREA
                                       SPA-NAI-ALLIDOLST-AREA
      *
           PERFORM 900-ORDER-KEY3-SEL-SEC
      *
           PERFORM 31011-ALLIDOLST-SKIP-SEC
      *
           MOVE    ZERO            TO  IDX-MAX
      *
           PERFORM UNTIL ( FLG-ORDER   NOT =   ZERO )
                    OR   ( IDX-MAX     >=  CONST-ALLIDOLST-MAX )
      *
               COMPUTE IDX-MAX =   IDX-MAX +   1
      *
               MOVE    ORDER-HOSPNUM
                                   TO  SPA-NAI-TALLHOSPNUM    (IDX-MAX)
               MOVE    ORDER-KARTE-KEY
                                   TO  SPA-NAI-TALLKARTE-KEY (IDX-MAX)
               MOVE    ORDER-ORDERNUM
                                   TO  SPA-NAI-TALLORDERNUM  (IDX-MAX)
      *
               COMPUTE WRK-TALLNUM
                   =   CONST-ALLIDOLST-MAX
                   * ( SPA-GMN-ALLIDOLST-PAGE    -   1 )
                   +   IDX-MAX
      *
               MOVE    WRK-TALLNUM TO  SPA-GMN-TALLNUM    (IDX-MAX)
      *
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-TALLIDOYMD (IDX-MAX)
               MOVE    WRK-SYMD    TO  SPA-NAI-TALLIDOYMD (IDX-MAX)
      *
               MOVE    ORDER-ORDERID
                                   TO  SPA-NAI-TALLORDERID(IDX-MAX)
               MOVE    ORDER-ORDERID   TO  WRK-CMB-CD
               PERFORM 4200-ORDERID-CHK-SEC
               MOVE    WRK-CMB-ITM2    TO  SPA-GMN-TALLIDOJIYU(IDX-MAX)
      *
               MOVE    ORDER-PTNUM TO  SPA-GMN-TALLPTNUM  (IDX-MAX)
               MOVE    ORDER-PTNUM TO  WRK-PTNUM-PTNUM
               PERFORM 900-PTNUM-KEY2-SEL-SEC
               IF    ( FLG-PTNUM   =   ZERO )
                   MOVE    PTNUM-PTID
                                       TO  SPA-NAI-TALLPTID   (IDX-MAX)
               END-IF
      *
               MOVE    SPA-NAI-TALLPTID   (IDX-MAX)
                                   TO  WRK-PTINF-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               MOVE    PTINF-NAME  TO  SPA-GMN-TALLNAME   (IDX-MAX)
      *
               MOVE    ORDER-STATUS    TO  SPA-GMN-TALLSTS (IDX-MAX)
      *
               PERFORM 900-ORDER-KEY3-FET-SEC
      *
           END-PERFORM
      *
           MOVE    IDX-MAX         TO  SPA-GMN-ALLIDOLST-MAX
      *
           MOVE    "tbl_order"             TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( SPA-GMN-ALLIDOLST-MAX   =   ZERO )
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( FLG-ORDER       =   ZERO )
               MOVE    1                   TO  SPA-GMN-ALLIDOLST-NEXT
           END-IF
      *
           .
       3101-ALLIDOLST-EXT.
           EXIT.
      *****************************************************************
      *    異動リスト読み飛ばし処理
      *****************************************************************
       31011-ALLIDOLST-SKIP-SEC        SECTION.
      *
           COMPUTE WRK-SKIP-MAX
               =   CONST-ALLIDOLST-MAX
               * ( SPA-GMN-ALLIDOLST-PAGE  -   1 )
      *
           MOVE    ZERO            TO  CNT-SKIP
      *
           PERFORM UNTIL ( FLG-ORDER   NOT =   ZERO )
                    OR   ( CNT-SKIP       >=  WRK-SKIP-MAX )
      *
               COMPUTE CNT-SKIP    =   CNT-SKIP    +   1
      *
               PERFORM 900-ORDER-KEY3-FET-SEC
      *
           END-PERFORM
      *
           .
       31011-ALLIDOLST-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    異動リスト編集処理
      *****************************************************************
       3101-KJNIDOLST-SEC              SECTION.
      *
           MOVE    1              TO   FLG-KJNIDOLST-ROW
      *
           INITIALIZE                  SPA-GMN-KJNIDOLST-AREA
                                       SPA-NAI-KJNIDOLST-AREA
      *
           PERFORM 900-ORDER-KEY4-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-MAX
      *
           PERFORM UNTIL ( FLG-ORDER   NOT =   ZERO )
                    OR   ( IDX-MAX     >=  CONST-KJNIDOLST-MAX )
      *
               COMPUTE IDX-MAX =   IDX-MAX +   1
      *
               MOVE    ORDER-KARTE-KEY
                                   TO  SPA-NAI-TKJNKARTE-KEY (IDX-MAX)
               MOVE    ORDER-ORDERNUM
                                   TO  SPA-NAI-TKJNORDERNUM  (IDX-MAX)
               MOVE    ORDER-UKEYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-TKJNUKEYMD (IDX-MAX)
               MOVE    WRK-SYMD    TO  SPA-NAI-TKJNUKEYMD (IDX-MAX)
      *
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-TKJNIDOYMD (IDX-MAX)
               MOVE    WRK-SYMD    TO  SPA-NAI-TKJNIDOYMD (IDX-MAX)
      *
               MOVE    ORDER-ORDERID
                                   TO  SPA-NAI-TKJNORDERID(IDX-MAX)
               MOVE    ORDER-ORDERID   TO  WRK-CMB-CD
               PERFORM 4200-ORDERID-CHK-SEC
               MOVE    WRK-CMB-ITM2    TO  SPA-GMN-TKJNIDOJIYU(IDX-MAX)
      *
               MOVE    ORDER-STATUS    TO  SPA-GMN-TKJNSTS (IDX-MAX)
      *
               PERFORM 900-ORDER-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    IDX-MAX             TO  SPA-GMN-KJNIDOLST-MAX
      *
           MOVE    "tbl_order"             TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-KJNIDOLST-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       3101-ERRMESLST-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-ERRMESLST-ROW
      *
           INITIALIZE                  SPA-GMN-ERRMESLST-AREA
           MOVE    ZERO                TO  IDX-MAX
      *
           IF    ( ERR-ORDER-ERRCD  NOT =   SPACE )
               MOVE    ERR-ORDER-ERRCD TO  SPA-GMN-TERRCD (1)
               MOVE    ERR-ORDER-ERRCD TO  WRK-CMB-CD
               PERFORM 4200-ERRCD-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TERRMES (1)
               MOVE    1               TO  IDX-MAX
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   ERR-ODRITEM-MAX )
      *
               MOVE    ERR-ODRITEM-OCC (IDX0)  TO  ODRITEM-REC
      *
               IF    ( ODRITEM-ERRCD   NOT =   SPACE )
                   IF    ( IDX-MAX    >=   CONST-ERRCD-MAX )
                       CONTINUE
                   ELSE
                       COMPUTE IDX-MAX =   IDX-MAX +   1
      *
                       MOVE    ODRITEM-ERRCD
                                           TO  SPA-GMN-TERRCD (IDX-MAX)
                       IF    ( ODRITEM-ERRCD
                                           =   CONST-ERRCD9001 )
                           MOVE    ODRITEM-ERRCD   TO  WRK-CMB-CD
                           PERFORM 4200-ERRCD-CHK-SEC
                           MOVE    WRK-CMB-ITM     TO  WRK-STR
                           MOVE    ODRITEM-ITEMID  TO  WRK-CMB-CD
                           PERFORM 4200-NCLAIMID-CHK-SEC
                           STRING  WRK-STR         DELIMITED BY SPACE
                                   "［"            DELIMITED BY SIZE
                                   WRK-CMB-ITM     DELIMITED BY SPACE
                                   "］"            DELIMITED BY SIZE
                           INTO    SPA-GMN-TERRMES(IDX-MAX)
                           END-STRING
                       ELSE
                           MOVE    ODRITEM-ERRCD   TO  WRK-CMB-CD
                           PERFORM 4200-ERRCD-CHK-SEC
                           MOVE    WRK-CMB-ITM
                                           TO  SPA-GMN-TERRMES(IDX-MAX)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDX-MAX             TO  SPA-GMN-ERRMESLST-MAX
      *
           .
       3101-ERRMESLST-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（Ｉ０ＩＤ１）ＯＫ処理
      *****************************************************************
       330-I0ID1-SET-SEC               SECTION.
      *
           MOVE    SPA-I0ID1-FLG   TO  WRK-I0ID1-FLG
           MOVE    SPA-I0IDCD      TO  WRK-I0IDCD
      *
           MOVE    SPACE           TO  SPA-I0ID1-FLG
           MOVE    SPACE           TO  SPA-I0IDCD
      *
           IF    ( WRK-I0ID1-FLG       =   "OK" )
      *
               MOVE    SPACE           TO  SPA-I0ID1-FLG
               MOVE    SPACE           TO  SPA-I0IDCD
      *
               EVALUATE    WRK-I0IDCD
               WHEN    "2004"
                   MOVE    1       TO  WRK-INF-DIALOG
                   PERFORM 2121-NYUIN-TOUROKU-SEC
               WHEN    "2005"
                   MOVE    2       TO  WRK-INF-DIALOG
                   PERFORM 2121-NYUIN-TOUROKU-SEC
               WHEN    "2001"
               WHEN    "2001"
                   PERFORM 2301-DEL-SEC
               WHEN    "2002"
                   MOVE    1       TO  WRK-INF-DIALOG
                   PERFORM 2121-TAIIN-TOUROKU-SEC
               WHEN    "2003"
                   MOVE    2       TO  WRK-INF-DIALOG
                   PERFORM 2121-TAIIN-TOUROKU-SEC
               END-EVALUATE
           END-IF
      *
           .
       330-I0ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO      WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"     TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 220-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 230-DEL-SEC
      *    前日
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 260-PREVDAY-SEC
      *    翌日
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 270-NEXTDAY-SEC
      *    前頁
           WHEN    "CLICKED"       ALSO    "B06S"
               PERFORM 260-PREVPAGE-SEC
      *    次頁
           WHEN    "CLICKED"       ALSO    "B07S"
               PERFORM 270-NEXTPAGE-SEC
      *    歴表示更新
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 310-SPASET-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 212-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "UKEYMD"
               PERFORM 4100-UKEYMD-CHK-SEC
           WHEN        ANY         ALSO    "ALLIDOLST"
               PERFORM 4101-ALLIDOLST-SEL-SEC
           WHEN    OTHER
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    I06-BRMNUM          TO  SPA-GMN-BRMNUM
           MOVE    I06-BTUNAME         TO  SPA-GMN-BTUNAME-G
           MOVE    I06-SAGAKU          TO  SPA-GMN-SAGAKU-G
           MOVE    I06-NYUINYMD        TO  SPA-GMN-NYUINYMD
           MOVE    I06-NYUINKA         TO  SPA-GMN-NYUINKA-G
           MOVE    I06-SHOKAI          TO  SPA-GMN-SHOKAI-G
           MOVE    I06-SHONUM          TO  SPA-GMN-SHONUM
           MOVE    I06-TAIINYMD        TO  SPA-GMN-TAIINYMD
           MOVE    I06-TAIINCD         TO  SPA-GMN-TAIINCD-G
           MOVE    I06-DR(1)           TO  SPA-GMN-DR-G (1)
           MOVE    I06-DR(2)           TO  SPA-GMN-DR-G (2)
           MOVE    I06-DR(3)           TO  SPA-GMN-DR-G (3)
           MOVE    I06-HKNCOMBI        TO  SPA-GMN-HKNCOMBI-G
           MOVE    I06-NYUKBN          TO  SPA-GMN-NYUKBN-G
           MOVE    I06-TOKNYUIN        TO  SPA-GMN-TOKNYUIN-G
           MOVE    I06-SKJTIME         TO  SPA-GMN-SKJTIME-G
           MOVE    I06-NISSU           TO  SPA-GMN-NISSU
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    異動リスト選択編集処理
      *****************************************************************
       4101-ALLIDOLST-SEL-SEC          SECTION.
      *
           PERFORM 220-CLEAR-SEC
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-ALLIDOLST-MAX )
      *
               IF    ( I06-ALLIDOLST-SELECT (IDX-LST)  =   "T" )
                   MOVE    IDX-LST TO  SPA-GMN-ALLIDOLST-SEL
                   MOVE    SPA-GMN-TALLNUM  (IDX-LST)
                                   TO  SPA-GMN-SELNUM
      *
                   PERFORM 41011-INPUT-AREA-SEC
      *
                   MOVE    SPA-GMN-ALLIDOLST-MAX
                               TO  IDX-LST
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4101-ALLIDOLST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力エリア処理
      *****************************************************************
       41011-INPUT-AREA-SEC            SECTION.
      *
           IF    ( SPA-GMN-TALLSTS (SPA-GMN-ALLIDOLST-SEL) =   4 )
               PERFORM 41011-DIRECT-UPD-KHNCHK-SEC
               IF    ( FLG-KHNERR  NOT =   ZERO )
                   GO  TO  41011-INPUT-AREA-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-TALLSTS (SPA-GMN-ALLIDOLST-SEL)
           WHEN    1
               PERFORM 41011-KHNERR-HEN-SEC
           WHEN    3
               CONTINUE
           WHEN    OTHER
               EVALUATE    SPA-NAI-TALLORDERID (SPA-GMN-ALLIDOLST-SEL)
               WHEN    NORDERID-NYUINID
               WHEN    NORDERID-TAIINID
               WHEN    NORDERID-GAIHKID
               WHEN    NORDERID-GBACKID
               WHEN    NORDERID-FNORLID
               WHEN    NORDERID-FSPCLID
               WHEN    NORDERID-FSELEID
               WHEN    NORDERID-FSTOPID
               WHEN    NORDERID-FSPSLID
               WHEN    NORDERID-IDODRID
               WHEN    NORDERID-IDOKAID
               WHEN    NORDERID-IDOBTUID
               WHEN    NORDERID-IDOBRMID
               WHEN    NORDERID-DELNYUINID
               WHEN    NORDERID-ALLDELNYUINID
                   PERFORM 41011-INPUT-AREA-HEN-SEC
               END-EVALUATE
           END-EVALUATE
      *
           .
       41011-INPUT-AREA-EXT.
           EXIT.
      *****************************************************************
      *    直接更新時基本項目チェック処理
      *****************************************************************
       41011-DIRECT-UPD-KHNCHK-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-KHNERR
      *
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-TALLHOSPNUM    (SPA-GMN-ALLIDOLST-SEL)
                                   TO  SODR-HOSPNUM
           MOVE    SPA-NAI-TALLKARTE-KEY (SPA-GMN-ALLIDOLST-SEL)
                                   TO  SODR-KARTE-KEY
           MOVE    SPA-NAI-TALLORDERNUM (SPA-GMN-ALLIDOLST-SEL)
                                   TO  SODR-ORDERNUM
           CALL    "ORCSODRS02"        USING   SODR-AREA
                                               LNK-ODRITEM-REC
                                               SPA-AREA
           IF    ( SODR-ERRFLG NOT =   ZERO )
      *
               MOVE    1           TO  FLG-KHNERR
      *
               PERFORM 41011-HEAD-AREA-HEN-SEC
      *
               MOVE    1           TO  SPA-NAI-DIRECT-KHNERR
      *
               INITIALIZE                  SPA-GMN-INPUT-AREA
                                           SPA-NAI-INPUT-AREA
               PERFORM 3101-KJNIDOLST-SEC
      *
               MOVE    SODR-ERRCD        TO  ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC   TO  ERR-ODRITEM-REC
               PERFORM 3101-ERRMESLST-SEC
      *
           END-IF
      *
           .
       41011-DIRECT-UPD-KHNCHK-EXT.
           EXIT.
      *****************************************************************
      *    基本項目エラー時編集処理
      *****************************************************************
       41011-KHNERR-HEN-SEC            SECTION.
      *
           PERFORM 41011-HEAD-AREA-HEN-SEC
      *
           INITIALIZE                  SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
                                       ERR-ODRITEM-REC
      *
           PERFORM 900-ORDER-KEY-SEL-SEC
           MOVE    ORDER-ERRCD1    TO  ERR-ORDER-ERRCD
      *
           PERFORM 900-ODRITEM-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ODRITEM   NOT =   ZERO )
      *
               IF    ( ERR-ODRITEM-MAX   >=  CONST-ODRITEM-MAX )
                   CONTINUE
               ELSE
                   COMPUTE ERR-ODRITEM-MAX =   ERR-ODRITEM-MAX   +   1
                   MOVE    ODRITEM-REC TO  ERR-ODRITEM-OCC
                                                      (ERR-ODRITEM-MAX)
               END-IF
      *
               PERFORM 900-ODRITEM-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 3101-KJNIDOLST-SEC
      *
           PERFORM 3101-ERRMESLST-SEC
      *
           .
       41011-KHNERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       41011-INPUT-AREA-HEN-SEC        SECTION.
      *
           MOVE    SPA-NAI-TALLORDERID (SPA-GMN-ALLIDOLST-SEL)
                                   TO  WRK-ALLORDERID
      *
           PERFORM 41011-HEAD-AREA-HEN-SEC
      *
           INITIALIZE                  SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
                                       ERR-ORDER-ERRCD
                                       ERR-ODRITEM-REC
                                       ITEM-AREA
      *
           MOVE    ALL "9"         TO  SPA-NAI-STATUS-G
           MOVE    ZERO            TO  CNT-ORDER
           MOVE    ZERO            TO  CNT-DRCD
      *
           PERFORM 900-ORDER-KEY-SEL-SEC
           MOVE    ORDER-ERRCD1    TO  ERR-ORDER-ERRCD
      *
           PERFORM 900-ODRITEM-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ODRITEM     NOT =   ZERO )
      *
               IF    ( ODRITEM-STATUS      NOT =   3 )
      *
                   EVALUATE    WRK-ALLORDERID
                   WHEN    NORDERID-NYUINID
                       PERFORM 410111-NYUIN-HEN-SEC
                   WHEN    NORDERID-TAIINID
                       PERFORM 410111-TAIIN-HEN-SEC
                   WHEN    NORDERID-GAIHKID
                   WHEN    NORDERID-GBACKID
                       PERFORM 410111-GAIHAKU-HEN-SEC
                   WHEN    NORDERID-FNORLID
                   WHEN    NORDERID-FSPCLID
                   WHEN    NORDERID-FSELEID
                   WHEN    NORDERID-FSTOPID
                   WHEN    NORDERID-FSPSLID
                       PERFORM 410111-SYOKUJI-HEN-SEC
                   WHEN    NORDERID-IDODRID
                       PERFORM 410111-DOCTOR-HEN-SEC
                   WHEN   NORDERID-IDOBTUID
                   WHEN   NORDERID-IDOBRMID
                   WHEN   NORDERID-IDOKAID
                       PERFORM 410111-IDOKABTUBRM-HEN-SEC
                   WHEN    NORDERID-DELNYUINID
                   WHEN    NORDERID-ALLDELNYUINID
                       PERFORM 410111-DELNYUIN-HEN-SEC
                   END-EVALUATE
      *
               END-IF
      *
               IF    ( ERR-ODRITEM-MAX      >=   CONST-ODRITEM-MAX )
                   CONTINUE
               ELSE
                   COMPUTE ERR-ODRITEM-MAX   =   ERR-ODRITEM-MAX   +   1
                   MOVE    ODRITEM-REC TO  ERR-ODRITEM-OCC
                                                (ERR-ODRITEM-MAX)
               END-IF
      *
               PERFORM 900-ODRITEM-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 41011-DIRECT-UPD-CHK-SEC
      *
           PERFORM 410112-INPUT-AREA-HEN-SEC
      *
           PERFORM 3101-KJNIDOLST-SEC
      *
           IF    ( SPA-NAI-STATUS-ORDER    =   ZERO )
               CONTINUE
           ELSE
               PERFORM 3101-ERRMESLST-SEC
           END-IF
      *
           .
       41011-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院時編集処理
      *****************************************************************
       410111-NYUIN-HEN-SEC            SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           IF    ( CNT-ORDER   =  1 )
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-NYUINYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUINYMD
               MOVE    ORDER-REC       TO  SPA-NAI-ORDER-DAT
           END-IF
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-NYUKBNJMA
               PERFORM 4101111-NYUKBN-HEN-SEC
      *
           WHEN    NCLAIMID-SHOKAIJMA
               PERFORM 4101111-SHOKAI-HEN-SEC
      *
           WHEN    NCLAIMID-BTUNUMJMA
               PERFORM 4101111-BTUNUM-HEN-SEC
      *
           WHEN    NCLAIMID-BRMNUMJMA
               PERFORM 4101111-BRMNUM-HEN-SEC
      *
           WHEN    NCLAIMID-SAGAKUJMA
               PERFORM 4101111-SAGAKU-HEN-SEC
      *
           WHEN    NCLAIMID-DRJMA
               PERFORM 4101111-DR-HEN-SEC
      *
           WHEN    NCLAIMID-TOKNYUINJMA
               PERFORM 4101111-TOKNYUIN-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-NYUIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    退院時編集処理
      *****************************************************************
       410111-TAIIN-HEN-SEC            SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           IF    ( CNT-ORDER   =  1 )
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-TAIINYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TAIINYMD
               MOVE    ORDER-REC       TO  SPA-NAI-ORDER-DAT
           END-IF
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-TAIINCDJMA
               PERFORM 4101111-TAIINCD-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-TAIIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    外泊時編集処理
      *****************************************************************
       410111-GAIHAKU-HEN-SEC          SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           IF    ( CNT-ORDER   =  1 )
               MOVE    ORDER-REC       TO  SPA-NAI-ORDER-DAT
           END-IF
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-GAIHAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事時編集処理
      *****************************************************************
       410111-SYOKUJI-HEN-SEC          SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           IF    ( CNT-ORDER   =  1 )
               MOVE    ORDER-REC       TO  SPA-NAI-ORDER-DAT
           END-IF
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-SKJTIMEJMA
               PERFORM 4101111-SKJTIME-HEN-SEC
      *
           WHEN    NCLAIMID-NISSUJMA
               PERFORM 4101111-NISSU-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    担当医時編集処理
      *****************************************************************
       410111-DOCTOR-HEN-SEC           SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           IF    ( CNT-ORDER   =  1 )
               MOVE    ORDER-REC       TO  SPA-NAI-ORDER-DAT
           END-IF
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-DRJMA
               PERFORM 4101111-DR-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-DOCTOR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    転科転棟転室編集処理
      *****************************************************************
       410111-IDOKABTUBRM-HEN-SEC      SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           IF    ( CNT-ORDER   =  1 )
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-NYUINYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUINYMD
               MOVE    ORDER-REC       TO  SPA-NAI-ORDER-DAT
           END-IF
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-NYUKBNJMA
               PERFORM 4101111-NYUKBN-HEN-SEC
      *
           WHEN    NCLAIMID-BTUNUMJMA
               PERFORM 4101111-BTUNUM-HEN-SEC
      *
           WHEN    NCLAIMID-BRMNUMJMA
               PERFORM 4101111-BRMNUM-HEN-SEC
      *
           WHEN    NCLAIMID-SAGAKUJMA
               PERFORM 4101111-SAGAKU-HEN-SEC
      *
           WHEN    NCLAIMID-DRJMA
               PERFORM 4101111-DR-HEN-SEC
      *
           WHEN    NCLAIMID-TOKNYUINJMA
               PERFORM 4101111-TOKNYUIN-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-IDOKABTUBRM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院取消時編集処理
      *****************************************************************
       410111-DELNYUIN-HEN-SEC         SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           IF    ( CNT-ORDER   =  1 )
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-NYUINYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUINYMD
               MOVE    ORDER-REC       TO  SPA-NAI-ORDER-DAT
           END-IF
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-DELNYUIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    共通入力項目編集処理
      *****************************************************************
       410111-INPUT-COMMON-HEN-SEC     SECTION.
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-NYUINKAJMA
               PERFORM 4101111-NYUINKA-HEN-SEC
      *
           WHEN    NCLAIMID-HKNCOMBIJMA
               PERFORM 4101111-HKNCOMBI-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-INPUT-COMMON-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       410112-INPUT-AREA-HEN-SEC       SECTION.
      *
           PERFORM 4101112-NYUINKAMEI-HEN-SEC
           PERFORM 4101112-HKNCOMBIMEI-HEN-SEC
      *
           EVALUATE    WRK-ALLORDERID
      *
           WHEN    NORDERID-NYUINID
      *
               PERFORM 4101112-SHOKAI-HEN-SEC
               PERFORM 4101112-BTUNUM-HEN-SEC
               PERFORM 4101112-SAGAKUMEI-HEN-SEC
               PERFORM 4101112-SHOKAIMEI-HEN-SEC
               PERFORM 4101112-DRMEI-HEN-SEC
               PERFORM 4101112-NYUKBNMEI-HEN-SEC
               PERFORM 4101112-TOKNYUINMEI-HEN-SEC
      *
           WHEN    NORDERID-TAIINID
      *
               PERFORM 4101112-TAIINCDMEI-HEN-SEC
      *
           WHEN    NORDERID-GAIHKID
           WHEN    NORDERID-GAIHKID
           WHEN    NORDERID-GBACKID
               CONTINUE
           WHEN    NORDERID-FNORLID
           WHEN    NORDERID-FSPCLID
           WHEN    NORDERID-FSELEID
           WHEN    NORDERID-FSTOPID
           WHEN    NORDERID-FSPSLID
      *
               PERFORM 4101112-SKJTIME-HEN-SEC
           WHEN    NORDERID-IDODRID
      *
               PERFORM 4101112-DRMEI-HEN-SEC
           WHEN   NORDERID-IDOKAID
           WHEN   NORDERID-IDOBTUID
           WHEN   NORDERID-IDOBRMID
      *
               PERFORM 4101112-BTUNUM-HEN-SEC
               PERFORM 4101112-SAGAKUMEI-HEN-SEC
               PERFORM 4101112-DRMEI-HEN-SEC
               PERFORM 4101112-NYUKBNMEI-HEN-SEC
               PERFORM 4101112-TOKNYUINMEI-HEN-SEC
           WHEN    NORDERID-DELNYUINID
           WHEN    NORDERID-ALLDELNYUINID
               CONTINUE
           END-EVALUATE
      *
           .
       410112-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回入院日編集処理
      *****************************************************************
       41011-SHONYUINYMD-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-LBL-SHONYUINYMD
           MOVE    ZERO                TO  WRK-SHONUM
      *
           INSPECT ITEM-LASTNYINYMD REPLACING  ALL "-" BY  "."
           MOVE    ITEM-LASTNYINYMD    TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF    ( FLG-HIZUKE-ERR  =   ZERO )
               MOVE    WRK-SYMD        TO  ITEM-LASTNYINYMD
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
               MOVE    SPA-NAI-PTID    TO  PTNYUINRRK-PTID
               MOVE    ITEM-LASTNYINYMD
                                      TO  PTNYUINRRK-NYUINYMD
               PERFORM 900-PTNYUINRRK-KEY39-SEL-SEC
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   MOVE    PTNYUINRRK-SHONUM
                                       TO  WRK-SHONUM
                   PERFORM 900-PTNYUINRRK-KEY30-SEL-SEC
                   IF    ( FLG-PTNYUINRRK  =   ZERO )
                       MOVE    PTNYUINRRK-NYUINYMD     TO   WRK-YMD
                       PERFORM 800-SEIWA-HEN-SEC
                       STRING  "入院日 "   DELIMITED   BY  SIZE
                               WRK-HENYMDG DELIMITED   BY  SIZE
                       INTO    WRK-LBL-SHONYUINYMD
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
           .
       41011-SHONYUINYMD-EXT.
           EXIT.
      *****************************************************************
      *    入院科編集処理
      *****************************************************************
       4101111-NYUINKA-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-NYUINKA
           MOVE    ODRITEM-ITEM        TO  SPA-GMN-NYUINKA-G
      *
           .
       4101111-NYUINKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ処理編集処理
      *****************************************************************
       4101111-HKNCOMBI-HEN-SEC        SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-HKNCOMBI
           MOVE    ODRITEM-ITEM        TO  SPA-GMN-HKNCOMBI-G
      *
           .
       4101111-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院区分編集処理
      *****************************************************************
       4101111-NYUKBN-HEN-SEC          SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-NYUKBN
           MOVE    ODRITEM-ITEM        TO  SPA-GMN-NYUKBN-G
      *
           .
       4101111-NYUKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回区分編集処理
      *****************************************************************
       4101111-SHOKAI-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-SHOKAI
           IF    ( ODRITEM-ITEM        =   "01" )
               MOVE    "2"             TO  SPA-GMN-SHOKAI
               MOVE    ODRITEM-MEMO   TO  ITEM-LASTNYINYMD
           END-IF
      *
           .
       4101111-SHOKAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号編集処理
      *****************************************************************
       4101111-BTUNUM-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-BTUNUM
           MOVE    ODRITEM-ITEM        TO  SPA-GMN-BTUNAME-G
      *
           .
       4101111-BTUNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病室番号編集処理
      *****************************************************************
       4101111-BRMNUM-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS   TO  SPA-NAI-STS-BRMNUM
           MOVE    ODRITEM-ITEM          TO  SPA-GMN-BRMNUM
      *
           .
       4101111-BRMNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    室料差額編集処理
      *****************************************************************
       4101111-SAGAKU-HEN-SEC          SECTION.
      *
           MOVE    ODRITEM-STATUS   TO  SPA-NAI-STS-SAGAKU
           MOVE    ODRITEM-ITEM     TO  SPA-GMN-SAGAKU-G
      *
           .
       4101111-SAGAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    担当医編集処理
      *****************************************************************
       4101111-DR-HEN-SEC              SECTION.
      *
           IF    ( CNT-DRCD    <   3 )
               COMPUTE CNT-DRCD    =   CNT-DRCD    +   1
               MOVE    ODRITEM-STATUS
                                       TO  SPA-NAI-STS-DR(CNT-DRCD)
               MOVE    ODRITEM-ITEM (2:) TO  SPA-GMN-DR-G  (CNT-DRCD)
           END-IF
      *
           .
       4101111-DR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    退院事由編集処理
      *****************************************************************
       4101111-TAIINCD-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS   TO  SPA-NAI-STS-TAIINCD
           MOVE    ODRITEM-ITEM     TO  SPA-GMN-TAIINCD-G
      *
           .
       4101111-TAIINCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料編集処理
      *****************************************************************
       4101111-TOKNYUIN-HEN-SEC        SECTION.
      *
           MOVE    ODRITEM-STATUS   TO  SPA-NAI-STS-TOKNYUIN
           MOVE    ODRITEM-ITEM          TO  ITEM-TOKNYUIN-G
      *
           .
       4101111-TOKNYUIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事時間編集処理
      *****************************************************************
       4101111-SKJTIME-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS        TO  SPA-NAI-STS-SKJTIME
           MOVE    ODRITEM-ITEM          TO  SPA-GMN-SKJTIME-G
      *
           .
       4101111-SKJTIME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理
      *****************************************************************
       4101111-NISSU-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-NISSU
           MOVE    ODRITEM-MEMO        TO  SPA-GMN-NISSU
      *
      *
           .
       4101111-NISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回編集処理
      *****************************************************************
       4101112-SHOKAI-HEN-SEC          SECTION.
      *
           IF    ( SPA-GMN-SHOKAI  =   "2" )
               PERFORM 41011-SHONYUINYMD-SEC
               MOVE    WRK-SHONUM      TO  SPA-GMN-SHONUM
               MOVE    WRK-LBL-SHONYUINYMD
                                       TO  SPA-GMN-LBL-SHONYUINYMD
           ELSE
               MOVE    "1"             TO  SPA-GMN-SHOKAI
           END-IF
      *
           .
       4101112-SHOKAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病棟編集処理
      *****************************************************************
       4101112-BTUNUM-HEN-SEC          SECTION.
      *
           PERFORM 3101-BTUDEF-SEC
           PERFORM 3101-BTUNAMELST-SET-SEC
           MOVE    SPA-GMN-BTUNAME     TO  WRK-CMB-CD
           PERFORM 4200-BTUNAMELST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-BTUNAME-G
               MOVE    SPA-NAI-BTUDEF-KHNSRYCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-KHNSRYCD
               MOVE    SPA-NAI-BTUDEF-SBT (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-SBT
               MOVE    SPA-NAI-BTUDEF-BTUTOK (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BTUTOK
               MOVE    SPA-NAI-BTUDEF-BRMTOK (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BRMTOK
               MOVE    SPA-NAI-BTUDEF-BTUKHNKBNCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BTUKHNKBNCD
               MOVE    SPA-NAI-BTUDEF-BRMKHNKBNCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BRMKHNKBNCD
           END-IF
      *
           .
       4101112-BTUNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    室料差額名編集処理
      *****************************************************************
       4101112-SAGAKUMEI-HEN-SEC       SECTION.
      *
           PERFORM 3101-SAGAKULST-SET-SEC
           MOVE    SPA-GMN-SAGAKU      TO  WRK-CMB-CD
           PERFORM 4200-SAGAKULST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SAGAKU-G
           END-IF
      *
           .
       4101112-SAGAKUMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院科名編集処理
      *****************************************************************
       4101112-NYUINKAMEI-HEN-SEC      SECTION.
      *
           PERFORM 3101-NYUINKALST-SET-SEC
           MOVE    SPA-GMN-NYUINKA     TO  WRK-CMB-CD
           PERFORM 4200-NYUINKALST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUINKA-G
           END-IF
      *
           .
       4101112-NYUINKAMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回名編集処理
      *****************************************************************
       4101112-SHOKAIMEI-HEN-SEC       SECTION.
      *
           PERFORM 3101-SHOKAILST-SET-SEC
           MOVE    SPA-GMN-SHOKAI      TO  WRK-CMB-CD
           PERFORM 4200-SHOKAILST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SHOKAI-G
           END-IF
      *
           .
       4101112-SHOKAIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    担当医名編集処理
      *****************************************************************
       4101112-DRMEI-HEN-SEC           SECTION.
      *
           PERFORM 3101-DRLST-SET-SEC
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
               MOVE    SPA-GMN-DR (IDX1)   TO  WRK-CMB-CD
               PERFORM 4200-DRLST-CHK-SEC
               IF    ( WRK-CMB-ITM NOT =   SPACE )
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-DR-G (IDX1)
               END-IF
           END-PERFORM
      *
           .
       4101112-DRMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名編集処理
      *****************************************************************
       4101112-HKNCOMBIMEI-HEN-SEC     SECTION.
      *
           PERFORM 3101-HKNCOMBILST-SET-SEC
           MOVE    SPA-GMN-HKNCOMBI    TO  WRK-CMB-CD
           PERFORM 4200-HKNCOMBILST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-HKNCOMBI-G
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-HKNNUM
           END-IF
      *
           .
       4101112-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院区分名編集処理
      *****************************************************************
       4101112-NYUKBNMEI-HEN-SEC       SECTION.
      *
           PERFORM 3101-NYUKBNLST-SET-SEC
           MOVE    SPA-GMN-NYUKBN      TO  WRK-CMB-CD
           PERFORM 4200-NYUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUKBN-G
           END-IF
      *
           .
       4101112-NYUKBNMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料名編集処理
      *****************************************************************
       4101112-TOKNYUINMEI-HEN-SEC     SECTION.
      *
           PERFORM 3101-TOKNYUINLST-SET-SEC
           MOVE    ITEM-TOKNYUIN-G TO WRK-CMB-CD
           PERFORM 4201-TOKNYUINLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-TOKNYUIN-G
               MOVE    SPA-GMN-TOKNYUINL2 (WRK-CMB-IDX)
                                   TO  SPA-NAI-TOKUTEI-G
           END-IF
      *
           IF    ( SPA-GMN-TOKNYUINMEI NOT =   SPACE )
               MOVE    SPA-GMN-TOKNYUINMEI TO  SPA-GMN-SNTNYUIN
           ELSE
               MOVE    SPA-NAI-KHNNYUIN    TO  SPA-GMN-SNTNYUIN
           END-IF
      *
           .
       4101112-TOKNYUINMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    退院事由名編集処理
      *****************************************************************
       4101112-TAIINCDMEI-HEN-SEC      SECTION.
      *
           PERFORM 3101-TAIINCDLST-SET-SEC
           MOVE    SPA-GMN-TAIINCD     TO  WRK-CMB-CD
           PERFORM 4200-TAIINCDLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TAIINCD-G
           END-IF
      *
           .
       4101112-TAIINCDMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事時間名編集処理
      *****************************************************************
       4101112-SKJTIME-HEN-SEC         SECTION.
      *
           PERFORM 3101-SKJTIMELST-SET-SEC
           MOVE    SPA-GMN-SKJTIME     TO  WRK-CMB-CD
           PERFORM 4200-SKJTIMELST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJTIME-G
           END-IF
      *
           .
       4101112-SKJTIME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    直接更新時入院科保険組合せ設定処理
      *****************************************************************
       41011-DIRECT-UPD-CHK-SEC        SECTION.
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           IF    ( SPA-NAI-STATUS-ORDER    =   4 )
               IF    ( SPA-NAI-STS-NYUINKA =   9 )
                   MOVE    ORDER-SRYKA     TO  SPA-GMN-NYUINKA
               END-IF
      *
               IF    ( SPA-NAI-STS-HKNCOMBI    =   9 )
                   MOVE    ORDER-HKNCOMBI  TO  SPA-GMN-HKNCOMBI
               END-IF
           END-IF
      *
           .
       41011-DIRECT-UPD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者基本情報編集処理
      *****************************************************************
       41011-HEAD-AREA-HEN-SEC         SECTION.
      *
           MOVE    SPA-GMN-ALLIDOLST-SEL
                                       TO  IDX-HEAD
      *
           INITIALIZE                  SPA-GMN-HEAD-AREA
                                       SPA-NAI-HEAD-AREA
      *
           MOVE    SPA-NAI-TALLKARTE-KEY (IDX-HEAD)
                                       TO  SPA-NAI-KARTE-KEY
           MOVE    SPA-NAI-TALLORDERNUM (IDX-HEAD)
                                       TO  SPA-NAI-ORDERNUM
           MOVE    SPA-GMN-TALLSTS (IDX-HEAD)
                                       TO  SPA-NAI-STATUS-ORDER
      *
           MOVE    SPA-NAI-TALLPTID (IDX-HEAD)  TO  WRK-PTINF-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPA-NAI-TALLIDOYMD (IDX-HEAD)
                                       TO  SPA-NAI-HIDOYMD
                                           SPA-NAI-KJNYMD
      *
           MOVE    SPA-NAI-TALLORDERID(IDX-HEAD)
                                       TO  SPA-NAI-ORDERID
      *
           MOVE    SPA-NAI-TALLPTID (IDX-HEAD)
                                       TO  SPA-NAI-PTID
      *
           MOVE    SPA-GMN-TALLPTNUM (IDX-HEAD)
                                       TO  SPA-GMN-PTNUM
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
      *
           EVALUATE    PTINF-SEX
           WHEN    "1"
               MOVE    "男"            TO  SPA-GMN-SEX
           WHEN    "2"
               MOVE    "女"            TO  SPA-GMN-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY
           MOVE    WRK-SYMD            TO  SPA-NAI-BIRTHDAY
      *
           IF    ( SPA-NAI-BIRTHDAY    NOT =   ZERO )
               INITIALIZE ORCSAGECHKAREA
               MOVE    SPA-HOSPNUM          TO  AGECHK-HOSPNUM
               MOVE    SPA-NAI-PTID        TO  AGECHK-PTID
               MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
               MOVE    SPA-NAI-BIRTHDAY    TO  AGECHK-BIRTHDAY
               CALL    "ORCSAGECHK"        USING   ORCSAGECHKAREA
                                                   SPA-AREA
               IF    ( AGECHK-RC       =   ZERO )
                   IF    ( AGECHK-NENREI2-YY   =   ZERO )
                       MOVE    AGECHK-NENREI2-MM
                                           TO  WRK-AGE-Z
                       MOVE    "ヶ月"      TO  WRK-AGE-MEI
                   ELSE
                       MOVE    AGECHK-NENREI2-YY
                                           TO  WRK-AGE-Z
                       MOVE    "才"        TO  WRK-AGE-MEI
                   END-IF
               END-IF
      *
               MOVE    WRK-AGE-G   TO  SPA-GMN-AGE
      *
           END-IF
      *
           IF    ( SPA-NAI-HIDOYMD  NOT =   ZERO )
               PERFORM 900-PTNYUINRRK-KEY42-SEL-SEC
           END-IF
      *
           IF    ( FLG-PTNYUINRRK      =   ZERO )
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                   CONTINUE
               ELSE
                   MOVE     PTNYUINRRK-NYUINKA TO  SPA-NAI-HSRYKA
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    "1005"              TO  SYS-KANRICD
                   MOVE     PTNYUINRRK-NYUINKA TO  SYS-KBNCD
                   MOVE     SPA-NAI-HIDOYMD    TO  SYS-STYUKYMD
                                                   SYS-EDYUKYMD
                   PERFORM  900-SYSKANRI-KEY10-SEL-SEC
                   MOVE     SYSKANRI-REC       TO  SYS-1005-REC
                   MOVE     SYS-1005-SRYKANAME1    TO  SPA-GMN-HSRYKA
      *
                   MOVE    PTNYUINRRK-HKNCOMBINUM
                                               TO  SPA-NAI-HHKNCOMBI
      *
                   MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
                   INITIALIZE                      WRK-SPA-KYOTUKEY
                                                   WRK-SPA-KYOTU-SET
                                                   ORCSHKNSETAREA
      *
                   MOVE    SPA-NAI-HIDOYMD     TO  WRK-SPA-SRYYMD
                   MOVE    SPA-NAI-HSRYKA      TO  WRK-SPA-SRYKA
                   MOVE    "1"                 TO  WRK-SPA-NYUGAIKBN
                   MOVE    SPA-NAI-PTID        TO  WRK-SPA-PTID
                   MOVE    SPA-NAI-BIRTHDAY    TO  WRK-SPA-BIRTHDAY
                   MOVE    PTNYUINRRK-HKNCOMBINUM
                                               TO  WRK-SPA-HKNCOMBINUM
                   MOVE    "1"                 TO  ORCSHKN-KBN
                   CALL    "ORCSHKNSET"    USING   WRK-SPA-AREA
                                                   ORCSHKNSETAREA
                   IF    ( ORCSHKN-ERRCD       =   ZERO )
                       IF    ( ORCSHKN-IDX    >=   1 )
                        AND  ( ORCSHKN-IDX    <=   ORCSHKN-HKN-MAX )
                           MOVE    ORCSHKN-GMN-HKNCOMBIMEI (ORCSHKN-IDX)
                                               TO  SPA-GMN-HHKNCOMBI
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
           IF    ( FLG-PTNYUINRRK      =   ZERO )
               EVALUATE    TRUE
               WHEN  ( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
                   MOVE    "前回退院日"    TO  SPA-GMN-LBL-MAEIDOMEI
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-MAEIDOYMD
                   MOVE    WRK-SYMD            TO  SPA-NAI-MAEIDOYMD
                   IF    ( PTNYUINRRK-JTIKBN       =   "6" )
                    AND  ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                        MOVE   "*"         TO  SPA-GMN-MAEIDOYMD (10:)
                   END-IF
               WHEN  ( PTNYUINRRK-RRKEDANUM    >   1 )
                   MOVE    "前回異動日"        TO  SPA-GMN-LBL-MAEIDOMEI
                   MOVE    PTNYUINRRK-TENNYUYMD    TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-MAEIDOYMD
                   MOVE    WRK-SYMD            TO  SPA-NAI-MAEIDOYMD
               WHEN  OTHER
                   MOVE    "    入院日"        TO  SPA-GMN-LBL-MAEIDOMEI
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-MAEIDOYMD
                   MOVE    WRK-SYMD            TO  SPA-NAI-MAEIDOYMD
               END-EVALUATE
      *
           END-IF
      *
           MOVE    SPA-NAI-ORDERID     TO  WRK-CMB-CD
           PERFORM 4200-ORDERID-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IDOJIYU
      *
           IF    ( SPA-NAI-PTID        NOT =   ZERO )
               MOVE    SPA-GMN-PTNUM   TO  SPA-LAST-PTNUM
               MOVE    SPA-NAI-PTID    TO  SPA-LAST-PTID
           END-IF
      *
           .
       41011-HEAD-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           PERFORM 220-CLEAR-SEC
      *
           MOVE    I06-SELNUM      TO  SPA-GMN-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    IDX-LST     TO  SPA-GMN-ALLIDOLST-SEL
               PERFORM 41011-INPUT-AREA-HEN-SEC
               MOVE    ZERO        TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受付日チェック処理
      *****************************************************************
       4100-UKEYMD-CHK-SEC             SECTION.
      *
           MOVE    I06-UKEYMD      TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF    ( FLG-HIZUKE-ERR  =   ZERO )
      *
               MOVE    WRK-HENYMDG TO  SPA-GMN-UKEYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-UKEYMD
      *
               PERFORM 220-CLEAR-SEC
      *
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
      *
           ELSE
               MOVE    "0002"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
      *
           .
       4100-UKEYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-BRMNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-BTUNAME-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-SAGAKU-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-NYUINKA-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-SHOKAI-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-SHONUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-TAIINCD-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
      *
               PERFORM 4101-DR-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
           PERFORM 4101-HKNCOMBI-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-NYUKBN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-TOKNYUIN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-SKJTIME-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-NISSU-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-TOKNYUINMEI NOT =   SPACE )
               MOVE    SPA-GMN-TOKNYUINMEI TO  SPA-GMN-SNTNYUIN
           ELSE
               MOVE    SPA-NAI-KHNNYUIN    TO  SPA-GMN-SNTNYUIN
           END-IF
      *
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-SELNUM  =   ZERO )
               MOVE    "1001"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-CUR
           ELSE
               MOVE    ZERO        TO  IDX-LST
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL ( IDX3    >   SPA-GMN-ALLIDOLST-MAX )
                        OR   ( IDX-LST NOT =   ZERO )
                   IF    ( SPA-GMN-SELNUM  =   SPA-GMN-TALLNUM (IDX3))
                       MOVE    IDX3    TO  IDX-LST
                   END-IF
               END-PERFORM
      *
               IF    ( IDX-LST     =   ZERO )
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
      *
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室チェック
      *****************************************************************
       4101-BRMNUM-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-BTUDEF-G
      *
           IF    ( SPA-GMN-BRMNUM          =   SPACE )
               GO  TO  4101-BRMNUM-CHK-EXT
           END-IF
      *
           INITIALIZE              ORCSBRMNUMAREA
           MOVE    "1"             TO  SBRMNUM-IN-PROCKBN
           MOVE    SPA-GMN-BRMNUM  TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
           IF    ( SBRMNUM-RC      =   ZERO )
               MOVE    SBRMNUM-OT-BRMNUM
                                   TO  SPA-GMN-BRMNUM
           END-IF
      *
           PERFORM 3101-BTUDEF-SEC
           IF    ( SPA-NAI-BTUDEF-MAX  =   ZERO )
               MOVE    "0003"      TO  SPA-ERRCD
               MOVE   3            TO  SPA-GMN-CUR
               GO  TO  4101-BRMNUM-CHK-EXT
           END-IF
      *
           IF    ( WRK-MCP-WIDGET          =   "BRMNUM" )
            OR   ( WRK-MCP-WIDGET          =   "BTUNAME" )
            OR   ( WRK-MCP-WIDGET          =   "B12" )
               CONTINUE
           ELSE
               GO  TO  4101-BRMNUM-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-BRMNUM-ORG  =   SPACE )
            OR   ( SPA-GMN-BTUNAME-ORG =   SPACE )
               MOVE    SPA-GMN-BRMNUM  TO  SPA-GMN-BRMNUM-ORG
               MOVE    SPA-GMN-BTUNAME TO  SPA-GMN-BTUNAME-ORG
           END-IF
      *
           PERFORM 3101-BTUNAMELST-SET-SEC
           MOVE    SPA-GMN-BTUNAME     TO  WRK-CMB-CD
           PERFORM 4200-BTUNAMELST-CHK-SEC
           IF    ( SPA-GMN-BTUNAME-MAX   =   1 )
            OR   ( WRK-CMB-IDX           =   ZERO )
                MOVE   1               TO  WRK-CMB-IDX
                IF   ( SPA-GMN-BTUNAME-MAX   = 1 )
                    MOVE   5           TO  SPA-GMN-CUR
                END-IF
           END-IF
      *
           MOVE    SPA-GMN-BTUNAMELST      (WRK-CMB-IDX)
                                       TO  SPA-GMN-BTUNAME-G
           MOVE    SPA-NAI-BTUDEF-KHNSRYCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-KHNSRYCD
           MOVE    SPA-NAI-BTUDEF-SBT      (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-SBT
           MOVE    SPA-NAI-BTUDEF-BTUTOK   (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BTUTOK
           MOVE    SPA-NAI-BTUDEF-BRMTOK   (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BRMTOK
           MOVE    SPA-NAI-BTUDEF-BTUKHNKBNCD   (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BTUKHNKBNCD
           MOVE    SPA-NAI-BTUDEF-BRMKHNKBNCD   (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BRMKHNKBNCD
           MOVE    SPA-NAI-BTUDEF-SAGAKU   (WRK-CMB-IDX)
                                       TO  WRK-BTUDEF-SAGAKU
           MOVE    SPA-NAI-BTUDEF-NYUINKA  (WRK-CMB-IDX)
                                       TO  WRK-BTUDEF-NYUINKA
      *
      *    室料差額、診療科の初期値を設定
           IF    ( WRK-MCP-WIDGET      NOT =  "B12" ) 
               IF    ( SPA-GMN-BRMNUM      =  SPA-GMN-BRMNUM-ORG )
                AND  ( SPA-GMN-BTUNAME     =  SPA-GMN-BTUNAME-ORG )
                    MOVE    SPA-GMN-SAGAKU      TO  WRK-CMB-CD
                    PERFORM 4200-SAGAKULST-CHK-SEC
                    MOVE    WRK-CMB-ITM         TO  SPA-GMN-SAGAKU-G
      *
                    MOVE    SPA-GMN-NYUINKA     TO  WRK-CMB-CD
                    PERFORM 4200-NYUINKALST-CHK-SEC
                    MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUINKA-G
      *
                ELSE
                    MOVE    WRK-BTUDEF-SAGAKU   TO  WRK-CMB-CD
                    PERFORM 4200-SAGAKULST-CHK-SEC
                    MOVE    WRK-CMB-ITM         TO  SPA-GMN-SAGAKU-G
      *
                    MOVE    WRK-BTUDEF-NYUINKA   TO  WRK-CMB-CD
                    PERFORM 4200-NYUINKALST-CHK-SEC
                    MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUINKA-G
                END-IF
           END-IF
      *
           PERFORM 3101-TOKNYUINLST-SET-SEC
           IF    ( WRK-MCP-WIDGET      NOT =  "B12" ) 
               IF    ( SPA-GMN-NYUKBN      =   "01" )
                AND  ( SPA-NAI-BTU-SBT NOT =   "04" )
                AND  ( SPA-NAI-BTU-SBT NOT =   "09" )
                   INITIALIZE  SPA-GMN-TOKNYUIN-G
                               SPA-NAI-TOKUTEI-G
                               SPA-GMN-SNTNYUIN
               ELSE
                   IF    ( SPA-GMN-BRMNUM-ORG  NOT =   SPA-GMN-BRMNUM )
                    OR   ( SPA-GMN-BTUNAME-ORG NOT =   SPA-GMN-BTUNAME )
                           PERFORM 4101-TOKNYUIN-SYOKI-SEC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-BRMNUM      TO  SPA-GMN-BRMNUM-ORG
           MOVE    SPA-GMN-BTUNAME     TO  SPA-GMN-BTUNAME-ORG
      *
           .
       4101-BRMNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟チェック
      *****************************************************************
       4101-BTUNAME-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-BTUNAME-G   =   SPACE )
               GO  TO  4101-BTUNAME-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-BTUNAME     TO  WRK-CMB-CD
           PERFORM 4200-BTUNAMELST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-BTUNAME-G
           END-IF
      *
           .
       4101-BTUNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック
      *****************************************************************
       4101-SAGAKU-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-SAGAKU-G    =   SPACE )
               GO  TO  4101-SAGAKU-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SAGAKU      TO  WRK-CMB-CD
           PERFORM 4200-SAGAKULST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SAGAKU-G
           END-IF
      *
           .
       4101-SAGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック
      *****************************************************************
       4101-NYUINKA-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-NYUINKA-G   =   SPACE )
               GO  TO  4101-NYUINKA-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-NYUINKA      TO  WRK-CMB-CD
           PERFORM 4200-NYUINKALST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUINKA-G
           END-IF
      *
           .
       4101-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回区分チェック処理
      *****************************************************************
       4101-SHOKAI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-SHOKAI-G    =   SPACE )
               GO  TO  4101-SHOKAI-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SHOKAI      TO  WRK-CMB-CD
           PERFORM 4200-SHOKAILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0008"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SHOKAI-G
               IF    ( SPA-GMN-SHOKAI  =   "1" )
                   MOVE    ZERO        TO  SPA-GMN-SHONUM
                   MOVE    SPACE       TO  SPA-GMN-LBL-SHONYUINYMD
               END-IF
           END-IF
      *
           .
       4101-SHOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初歴チェック処理
      *****************************************************************
       4101-SHONUM-CHK-SEC             SECTION.
      *
           IF    ( WRK-MCP-WIDGET          =   "SHONUM" )
            OR   ( WRK-MCP-WIDGET          =   "B12" )
               CONTINUE
           ELSE
               GO  TO  4101-SHONUM-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-GMN-LBL-SHONYUINYMD
      *
           IF    ( SPA-GMN-SHONUM         =   ZERO )
               GO  TO  4101-SHONUM-CHK-EXT
           END-IF
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-GMN-SHONUM  TO  PTNYUINRRK-SHONUM
           PERFORM 900-PTNYUINRRK-KEY30-SEL-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO )
               MOVE    PTNYUINRRK-NYUINYMD     TO   WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               STRING  "入院日 "   DELIMITED   BY  SIZE
                       WRK-HENYMDG DELIMITED   BY  SIZE
               INTO    SPA-GMN-LBL-SHONYUINYMD
               END-STRING
           ELSE
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    9               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-SHONUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由チェック処理
      *****************************************************************
       4101-TAIINCD-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-TAIINCD-G    =   SPACE )
               GO  TO  4101-TAIINCD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-TAIINCD     TO  WRK-CMB-CD
           PERFORM 4200-TAIINCDLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TAIINCD-G
           END-IF
      *
           .
       4101-TAIINCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医チェック処理
      *****************************************************************
       4101-DR-CHK-SEC                 SECTION.
      *
           IF    ( SPA-GMN-DR-G (IDX2) =   SPACE )
               GO  TO  4101-DR-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-DR (IDX2)   TO  WRK-CMB-CD
           PERFORM 4200-DRLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0012"          TO  SPA-ERRCD
               EVALUATE    IDX2
               WHEN    1
                   MOVE    12          TO  SPA-GMN-CUR
               WHEN    2
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    3
                   MOVE    14          TO  SPA-GMN-CUR
               END-EVALUATE
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-DR-G (IDX2)
           END-IF
      *
           .
       4101-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       4101-HKNCOMBI-CHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-HKNNUM
      *
           IF    ( SPA-GMN-HKNCOMBI-G    =   SPACE )
               GO  TO  4101-HKNCOMBI-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-HKNCOMBI     TO  WRK-CMB-CD
           PERFORM 4200-HKNCOMBILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0015"          TO  SPA-ERRCD
               MOVE    15              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-HKNCOMBI-G
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-HKNNUM
           END-IF
      *
           IF    ( WRK-MCP-WIDGET  =   "HKNCOMBI" )
               MOVE    SPA-GMN-TOKNYUIN-SRYCDTTL
                                   TO  WRK-TOKNYUIN-SRYCDTTL
               PERFORM 3101-TOKNYUINLST-SET-SEC
               IF    ( SPA-GMN-TOKNYUIN-SRYCDTTL
                               NOT =   WRK-TOKNYUIN-SRYCDTTL  )
                   PERFORM 4101-TOKNYUIN-SYOKI-SEC
               END-IF
      *
           END-IF
      *
           .
       4101-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院区分チェック処理
      *****************************************************************
       4101-NYUKBN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-NYUKBN-G    =   SPACE )
               GO  TO  4101-NYUKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-NYUKBN      TO  WRK-CMB-CD
           PERFORM 4200-NYUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    16              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUKBN-G
           END-IF
      *
           .
       4101-NYUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料チェック処理
      *****************************************************************
       4101-TOKNYUIN-CHK-SEC           SECTION.
      *
           INITIALIZE  SPA-NAI-TOKUTEI-G
      *
           IF    ( SPA-GMN-TOKNYUIN-G  =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    SPA-GMN-TOKNYUIN    TO  WRK-CMB-CD
               PERFORM 4200-TOKNYUINLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0017"          TO  SPA-ERRCD
                   MOVE    17              TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-TOKNYUIN-G
                   MOVE    SPA-GMN-TOKNYUINL2 (WRK-CMB-IDX)
                                           TO  SPA-NAI-TOKUTEI-G
               END-IF
           END-IF
      *
           IF    ( WRK-MCP-WIDGET      =   "TOKNYUIN" )
               PERFORM 4101-NYUKBN-SYOKI-SEC
           END-IF
      *
           .
       4101-TOKNYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料初期値設定処理
      *****************************************************************
       4101-TOKNYUIN-SYOKI-SEC         SECTION.
      *
           INITIALIZE  SPA-GMN-TOKNYUIN-G
                       SPA-NAI-TOKUTEI-G
                       SPA-GMN-SNTNYUIN
      *
           PERFORM VARYING IDX-CMB
                   FROM    SPA-GMN-TOKNYUIN-MAX    BY  -1
                   UNTIL ( IDX-CMB     <   1 )
                    OR   ( SPA-GMN-TOKNYUIN-G NOT    =  SPACE )
      *
               IF    ( SPA-GMN-TOKNYUINL2-KBN (IDX-CMB) (1 : 1)
                                   =   "1" OR  "2" OR  "3" )
      *
                   MOVE    ZERO        TO  FLG-TARGET
      *
                   IF    ( SPA-GMN-TOKNYUINL2-KBN (IDX-CMB)
                                   =  "3" )
                       IF    ( SPA-GMN-TOKNYUINL3-NYUINRYOKBN
                                                  (IDX-CMB)
                                   =   "01" )
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   ELSE
                           MOVE    1   TO  FLG-TARGET
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET      =   1 )
                   MOVE    SPA-GMN-TOKNYUINLST (IDX-CMB)
                                       TO  SPA-GMN-TOKNYUIN-G
                   MOVE    SPA-GMN-TOKNYUINL2 (IDX-CMB)
                                       TO  SPA-NAI-TOKUTEI-G
               END-IF
      *
           END-PERFORM
      *
      *
           IF    ( SPA-GMN-TOKNYUINMEI     =   SPACE )
               IF    ( SPA-NAI-BTU-BRMKHNKBNCD NOT =   SPACE )
                   MOVE    SPA-NAI-BTU-BRMKHNKBNCD
                                           TO  WRK-CMB-CD
                   IF    ( SPA-NAI-KJNYMD  >=  CONST-H181001 )
                       PERFORM 41011-TOKNYUIN-SYOKI-SEC
                   END-IF
                   PERFORM 4201-TOKNYUINLST-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-TOKNYUIN-G
                   MOVE    SPA-GMN-TOKNYUINL2 (WRK-CMB-IDX)
                                       TO  SPA-NAI-TOKUTEI-G
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-TOKNYUINMEI    =   SPACE )
               IF    ( SPA-NAI-BTU-BTUKHNKBNCD NOT =   SPACE )
                   MOVE    SPA-NAI-BTU-BTUKHNKBNCD
                                           TO  WRK-CMB-CD
                   IF    ( SPA-NAI-KJNYMD  >=  CONST-H181001 )
                       PERFORM 41011-TOKNYUIN-SYOKI-SEC
                   END-IF
                   PERFORM 4201-TOKNYUINLST-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-TOKNYUIN-G
                   MOVE    SPA-GMN-TOKNYUINL2 (WRK-CMB-IDX)
                                       TO  SPA-NAI-TOKUTEI-G
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-TOKNYUINMEI NOT =   SPACE )
               MOVE    SPA-GMN-TOKNYUINMEI TO  SPA-GMN-SNTNYUIN
           ELSE
               MOVE    SPA-NAI-KHNNYUIN    TO  SPA-GMN-SNTNYUIN
           END-IF
      *
           PERFORM 4101-NYUKBN-SYOKI-SEC
      *
           .
       4101-TOKNYUIN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料初期値設定処理
      *****************************************************************
       41011-TOKNYUIN-SYOKI-SEC        SECTION.
      *
           PERFORM 800-ROUJINAGE-CHK-SEC
           IF    ( FLG-LIFE-RJN        =   1 )
               PERFORM VARYING IDX10   FROM    1   BY  1
                       UNTIL ( IDX10 > CONST-TOK-LIFECD-MAX)
                   IF    ( WRK-CMB-CD  =   CONST-TOK-LIFECD-IPN (IDX10))
                       MOVE    CONST-TOK-LIFECD-LIFE (IDX10)
                                   TO  WRK-CMB-CD
                       MOVE    CONST-TOK-LIFECD-MAX
                                   TO  IDX10
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       41011-TOKNYUIN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入院区分初期値設定処理
      *****************************************************************
       4101-NYUKBN-SYOKI-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-CD
           EVALUATE    SPA-NAI-TOKUTEI-KBN
           WHEN    SPACE
               MOVE    "01"            TO  WRK-CMB-CD
           WHEN    "1"
               MOVE    "02"            TO  WRK-CMB-CD
           WHEN    "2"
               MOVE    "02"            TO  WRK-CMB-CD
           WHEN    "3"
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5111"          TO  SYS-KANRICD
               MOVE    SPA-NAI-TOKUTEI-NYUIN
                                       TO  SYS-KBNCD
               MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
               IF    ( SYS-5111-BRM-NYUINRYOKBN
                                       =   "03" OR "05" OR "06" )
                   MOVE    "01"        TO  WRK-CMB-CD
               ELSE
                   MOVE    "02"        TO  WRK-CMB-CD
               END-IF
           END-EVALUATE
           PERFORM 4200-NYUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               CONTINUE
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUKBN-G
           END-IF
      *
           .
       4101-NYUKBN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    食事時間チェック処理
      *****************************************************************
       4101-SKJTIME-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-SKJTIME-G    =   SPACE )
               GO  TO  4101-SKJTIME-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SKJTIME     TO  WRK-CMB-CD
           PERFORM 4200-SKJTIMELST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0018"          TO  SPA-ERRCD
               MOVE    18              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJTIME-G
           END-IF
      *
           .
       4101-SKJTIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数チェック
      *****************************************************************
       4101-NISSU-CHK-SEC               SECTION.
      *
           IF    ( SPA-GMN-NISSU        =   SPACE )
               GO  TO  4101-NISSU-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-NISSU       TO  WRK-INPUT
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-ERR             =   ZERO )
               CONTINUE
           ELSE
               MOVE    "0020"          TO  SPA-ERRCD
               MOVE    19              TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-NISSU (1:1) =   "*" )
                   CONTINUE
               ELSE
                   MOVE    "0021"      TO  SPA-ERRCD
                   MOVE    19          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               INITIALIZE                  ORCSNDAYCHGAREA
               MOVE    SPA-GMN-NISSU   TO  ORCSNDAYCHG-INPUTCD
               MOVE    SPA-NAI-KJNYMD  TO  ORCSNDAYCHG-SRYYMD
               MOVE    "1"             TO  ORCSNDAYCHG-KBN
               MOVE    50              TO  ORCSNDAYCHG-KETA
               CALL    "ORCSNDAYCHG"       USING
                                           SPA-AREA
                                           ORCSNDAYCHGAREA
               IF    ( ORCSNDAYCHG-ERRCD   =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    "0019"      TO  SPA-ERRCD
                   MOVE    ORCSNDAYCHG-ERRMSG
                                       TO  WRK-ERRMSG
                   MOVE    19          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-NISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟名コンボチェック処理
      *****************************************************************
       4200-BTUNAMELST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-BTUNAME-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-BTUNAMEL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-BTUNAMELST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-BTUNAMELST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額コンボチェック処理
      *****************************************************************
       4200-SAGAKULST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SAGAKU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SAGAKUL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SAGAKULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SAGAKULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科コンボチェック処理
      *****************************************************************
       4200-NYUINKALST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-NYUINKA-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NYUINKAL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NYUINKALST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NYUINKALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回コンボチェック処理
      *****************************************************************
       4200-SHOKAILST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SHOKAI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SHOKAIL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SHOKAILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SHOKAILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由コンボチェック処理
      *****************************************************************
       4200-TAIINCDLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-TAIINCD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TAIINCDL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TAIINCDLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TAIINCDLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医コンボチェック処理
      *****************************************************************
       4200-DRLST-CHK-SEC              SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-DR-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-DRL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-DRLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1010"          TO  SYS-KANRICD
               MOVE    WRK-CMB-CD      TO  SYS-KBNCD
               MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYSKANRI-REC    TO  SYS-1010-REC
                   MOVE    SYS-1010-KBNCD (2:4)
                                           TO  WRK-CMB-ITM (1:4)
                   MOVE    SYS-1010-NAME   TO  WRK-CMB-ITM (6:)
               END-IF
      *
           END-IF
      *
           .
       4200-DRLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せコンボチェック処理
      *****************************************************************
       4200-HKNCOMBILST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-HKNCOMBI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-HKNCOMBIL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-HKNCOMBILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-HKNNUML (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-HKNCOMBILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院区分コンボチェック処理
      *****************************************************************
       4200-NYUKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-NYUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NYUKBNL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NYUKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NYUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事時間コンボチェック処理
      *****************************************************************
       4200-SKJTIMELST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SKJTIME-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJTIMEL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SKJTIMELST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SKJTIMELST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボチェック処理
      *****************************************************************
       4200-TOKNYUINLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-TOKNYUIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TOKNYUINL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TOKNYUINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TOKNYUINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボチェック処理
      *****************************************************************
       4201-TOKNYUINLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-TOKNYUIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TOKNYUINL2 (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TOKNYUINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4201-TOKNYUINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダーＩＤチェック処理
      *****************************************************************
       4200-ORDERID-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   NORDERID-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( NORDERID-ORCAID (IDX-CMB) =   WRK-CMB-CD )
                   MOVE    NORDERID-ORCAMES1 (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    NORDERID-ORCAMES2 (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-ORDERID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダーエラーコードチェック処理
      *****************************************************************
       4200-ERRCD-CHK-SEC              SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   NCLAIMERRCD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( NCLAIMERRCDL (IDX-CMB) =   WRK-CMB-CD )
                   MOVE    NCLAIMERRCDLMEI  (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-ERRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダー項目名称チェック処理
      *****************************************************************
       4200-NCLAIMID-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   NCLAIMID-ITEM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( NCLAIMID-ITEMJMA (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    NCLAIMID-ITEMMES  (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NCLAIMID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I01"               TO  SPA-SAKIPG
           MOVE    "I06"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                      SPA-GMN-HEAD-AREA
                                           SPA-GMN-INPUT-AREA
                                           SPA-GMN-SELNUM
                                           SPA-GMN-ALLIDOLST-SEL
                                           SPA-GMN-KJNIDOLST-AREA
                                           SPA-GMN-ERRMESLST-AREA
                                           SPA-NAI-HEAD-AREA
                                           SPA-NAI-INPUT-AREA
                                           SPA-NAI-KJNIDOLST-AREA
                                           SPA-GMN-COMBO-AREA
                                           SPA-NAI-COMBO-AREA
      *
           MOVE    SPA-NAI-UKEYMD      TO  SPA-NAI-KJNYMD
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           IF    ( SPA-GMN-ALLIDOLST-MAX   =   ZERO )
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       230-DEL-SEC                     SECTION.
      *
           IF    ( SPA-GMN-ALLIDOLST-SEL   =   ZERO )
               MOVE    "1001"          TO  SPA-ERRCD
           ELSE
               MOVE    "2001"          TO  SPA-I0IDCD
           END-IF
      *
           .
       230-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理（削除状態に更新）
      *****************************************************************
       2301-DEL-SEC                    SECTION.
      *
           MOVE    SPA-GMN-ALLIDOLST-SEL   TO  IDX-LST
      *
           INITIALIZE                  ORDER-REC
           MOVE    SPA-HOSPNUM             TO  ORDER-HOSPNUM
           MOVE    SPA-NAI-TALLKARTE-KEY (IDX-LST)
                                           TO  ORDER-KARTE-KEY
           MOVE    SPA-NAI-TALLORDERNUM  (IDX-LST)
                                           TO  ORDER-ORDERNUM
           MOVE    ORDER-REC               TO  MCPDATA-REC
           MOVE    "tbl_order"             TO  MCP-TABLE
           MOVE    "upd1"                  TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      =   ZERO )
      *
               PERFORM 220-CLEAR-SEC
      *
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           ELSE
               MOVE    "4001"              TO  SPA-ERRCD
           END-IF
      *
           .
       2301-DEL-EXT.
           EXIT.
      *****************************************************************
      *    前日処理
      *****************************************************************
       260-PREVDAY-SEC                 SECTION.
      *
           MOVE    SPA-NAI-UKEYMD      TO  WRK-YMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
           MOVE    -1                  TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD       TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-UKEYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-UKEYMD
      *
           PERFORM 220-CLEAR-SEC
      *
           MOVE    1                   TO  SPA-GMN-ALLIDOLST-PAGE
      *
      *    異動リスト編集処理
           PERFORM 3101-ALLIDOLST-SEC
      *
           .
       260-PREVDAY-EXT.
           EXIT.
      *****************************************************************
      *    翌日処理
      *****************************************************************
       270-NEXTDAY-SEC                 SECTION.
      *
           MOVE    SPA-NAI-UKEYMD      TO  WRK-YMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
           MOVE    1                   TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD       TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-UKEYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-UKEYMD
      *
           PERFORM 220-CLEAR-SEC
      *
           MOVE    1                   TO  SPA-GMN-ALLIDOLST-PAGE
      *
      *    異動リスト編集処理
           PERFORM 3101-ALLIDOLST-SEC
      *
           .
       270-NEXTDAY-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-PREVPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-ALLIDOLST-PAGE  >   1 )
      *
               PERFORM 220-CLEAR-SEC
      *
               COMPUTE SPA-GMN-ALLIDOLST-PAGE
                   =   SPA-GMN-ALLIDOLST-PAGE  -   1
      *
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-IF
      *
           .
       260-PREVPAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       270-NEXTPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-ALLIDOLST-NEXT  =   1 )
      *
               PERFORM 220-CLEAR-SEC
      *
               COMPUTE SPA-GMN-ALLIDOLST-PAGE
                   =   SPA-GMN-ALLIDOLST-PAGE  +   1
      *
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-IF
      *
           .
       270-NEXTPAGE-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       212-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  212-TOUROKU-EXT
           END-IF
      *
           EVALUATE    SPA-NAI-ORDERID
           WHEN    NORDERID-NYUINID
               PERFORM 2121-NYUIN-KANREN-CHK-SEC
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( SPA-I0IDCD  =   SPACE )
                   PERFORM 2121-NYUIN-TOUROKU-SEC
               END-IF
           WHEN    NORDERID-TAIINID
               PERFORM 2121-TAIIN-TOUROKU-SEC
           WHEN    NORDERID-GAIHKID
           WHEN    NORDERID-GBACKID
               PERFORM 2121-GAIHAKU-TOUROKU-SEC
           WHEN    NORDERID-FNORLID
           WHEN    NORDERID-FSPCLID
           WHEN    NORDERID-FSELEID
           WHEN    NORDERID-FSTOPID
           WHEN    NORDERID-FSPSLID
               PERFORM 2121-SYOKUJI-TOUROKU-SEC
           WHEN    NORDERID-IDODRID
               PERFORM 2121-DOCTOR-TOUROKU-SEC
           WHEN   NORDERID-IDOBTUID
           WHEN   NORDERID-IDOBRMID
           WHEN   NORDERID-IDOKAID
               PERFORM 2121-IDOKABTUBRM-TOUROKU-SEC
           WHEN    NORDERID-DELNYUINID
           WHEN    NORDERID-ALLDELNYUINID
               PERFORM 2121-DELNYUIN-TOUROKU-SEC
           END-EVALUATE
      *
           .
       212-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    入院関連チェック
      *****************************************************************
       2121-NYUIN-KANREN-CHK-SEC       SECTION.
      *
      *
           .
       2121-NYUIN-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院登録処理
      *****************************************************************
       2121-NYUIN-TOUROKU-SEC          SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  ODRITEM-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SHOKAI-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BTUNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BRMNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SAGAKU-SET-SEC
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-DR-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TOKNYUIN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR01"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4002"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMESLST-SEC
               EVALUATE    SODR-DIALOG
               WHEN    1
                   MOVE    "2004"       TO  SPA-I0IDCD
               WHEN    2
                   MOVE    "2005"       TO  SPA-I0IDCD
               END-EVALUATE
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-NYUIN-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    退院登録処理
      *****************************************************************
       2121-TAIIN-TOUROKU-SEC          SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TAIINCD-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR02"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4003"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMESLST-SEC
               EVALUATE    SODR-DIALOG
               WHEN    1
                   MOVE    "2002"       TO  SPA-I0IDCD
               WHEN    2
                   MOVE    "2003"       TO  SPA-I0IDCD
               END-EVALUATE
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-TAIIN-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    外泊登録処理
      *****************************************************************
       2121-GAIHAKU-TOUROKU-SEC        SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR03"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4004"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMESLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-GAIHAKU-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    食事登録処理
      *****************************************************************
       2121-SYOKUJI-TOUROKU-SEC        SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SKJTIME-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NISSU-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR04"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4005"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMESLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-SYOKUJI-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    担当医登録処理
      *****************************************************************
       2121-DOCTOR-TOUROKU-SEC         SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-DR-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR05"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4006"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMESLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-DOCTOR-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    転科転棟転室処理
      *****************************************************************
       2121-IDOKABTUBRM-TOUROKU-SEC          SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BTUNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BRMNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SAGAKU-SET-SEC
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-DR-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TOKNYUIN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR06"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4007"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMESLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-IDOKABTUBRM-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    入院取消登録処理
      *****************************************************************
       2121-DELNYUIN-TOUROKU-SEC       SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  ODRITEM-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR07"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4008"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMESLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-DELNYUIN-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    入院区分設定処理
      *****************************************************************
       2300-NYUKBN-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-NYUKBNJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           IF    ( SPA-NAI-STS-NYUKBN  =   4   OR  9 )
                MOVE     4         TO  LNK-ODRITEM-STATUS (IDX-ITM)
           ELSE
                MOVE     0         TO  LNK-ODRITEM-STATUS (IDX-ITM)
           END-IF
      *
           MOVE    SPA-GMN-NYUKBN  TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-NYUKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    初回設定処理
      *****************************************************************
       2300-SHOKAI-SET-SEC             SECTION.
      *
           IF    ( SPA-NAI-STS-SHOKAI      = 9 )
            AND  ( SPA-GMN-SHOKAI          = "01" )
               GO  TO  2300-SHOKAI-SET-EXT
           END-IF
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-SHOKAIJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-STS-SHOKAI      =   4 )
           WHEN  ( SPA-NAI-STS-SHOKAI      =   9 )
             AND ( SPA-GMN-SHOKAI          = "02" )
                MOVE   4           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           WHEN  OTHER
                MOVE   0           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           END-EVALUATE
      *
           IF    ( SPA-GMN-SHOKAI  = "2" )
               MOVE    "01"        TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
               MOVE    SPA-NAI-PTID    TO  PTNYUINRRK-PTID
               MOVE    ALL "9"         TO  PTNYUINRRK-NYUINYMD
               MOVE    SPA-GMN-SHONUM  TO  PTNYUINRRK-SHONUM
               PERFORM 900-PTNYUINRRK-KEY35-SEL-SEC
               MOVE    PTNYUINRRK-NYUINYMD
                                       TO  LNK-ODRITEM-MEMO(IDX-ITM)
           END-IF
      *
           .
       2300-SHOKAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号設定処理
      *****************************************************************
       2300-BTUNUM-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-BTUNUMJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           IF    ( SPA-NAI-STS-BTUNUM  =   4   OR  9 )
               MOVE    4           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           ELSE
               MOVE    0           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           END-IF
      *
           MOVE    SPA-GMN-BTUNAME TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-BTUNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    病室番号設定処理
      *****************************************************************
       2300-BRMNUM-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-BRMNUMJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           IF    ( SPA-NAI-STS-BRMNUM  =   4   OR  9 )
               MOVE    4           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           ELSE
               MOVE    0           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           END-IF
      *
           MOVE    SPA-GMN-BRMNUM  TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-BRMNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額設定処理
      *****************************************************************
       2300-SAGAKU-SET-SEC             SECTION.
      *
           IF    ( SPA-NAI-STS-SAGAKU      =   9 )
            AND  ( SPA-GMN-SAGAKU          = SPACE )
               GO  TO  2300-SAGAKU-SET-EXT
           END-IF
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-SAGAKUJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-STS-SAGAKU      =   4 )
           WHEN  ( SPA-NAI-STS-SAGAKU      =   9 )
             AND ( SPA-GMN-SAGAKU      NOT = SPACE )
               MOVE    4           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           WHEN  OTHER
               MOVE    0           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           END-EVALUATE
      *
      *
           MOVE    SPA-GMN-SAGAKU  TO  LNK-ODRITEM-ITEM (IDX-ITM)
           MOVE    SPA-GMN-SAGAKUMEI-NUM
                                   TO  LNK-ODRITEM-MEMO (IDX-ITM)
      *
           .
       2300-SAGAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    担当医設定処理
      *****************************************************************
       2300-DR-SET-SEC                 SECTION.
      *
           IF    ( SPA-NAI-STS-DR (IDX2)   =   9 )
            AND  ( SPA-GMN-DR     (IDX2)   =   SPACE )
               GO  TO  2300-DR-SET-EXT
           END-IF
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-DRJMA      TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-STS-DR (IDX2)       =   4 )
           WHEN  ( SPA-NAI-STS-DR (IDX2)       =   9 )
             AND ( SPA-GMN-DR     (IDX2)   NOT = SPACE )
               MOVE    4           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           WHEN  OTHER
               MOVE    0           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           END-EVALUATE
      *
           IF    ( SPA-GMN-DR     (IDX2)     NOT =   SPACE )
               MOVE    "1"                 TO  LNK-ODRITEM-ITEM(IDX-ITM)
               MOVE    SPA-GMN-DR (IDX2)   TO  LNK-ODRITEM-ITEM(IDX-ITM)
                                                              (2:)
           END-IF
      *
           .
       2300-DR-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料設定処理
      *****************************************************************
       2300-TOKNYUIN-SET-SEC           SECTION.
      *
           IF    ( SPA-NAI-STS-TOKNYUIN    =   9 )
            AND  ( SPA-GMN-TOKNYUIN        = SPACE )
               GO  TO  2300-TOKNYUIN-SET-EXT
           END-IF
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-TOKNYUINJMA TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-STS-TOKNYUIN    =   4 )
           WHEN  ( SPA-NAI-STS-TOKNYUIN    =   9 )
             AND ( SPA-GMN-TOKNYUIN    NOT = SPACE )
               MOVE    4           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           WHEN  OTHER
               MOVE    0           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           END-EVALUATE
      *
           IF    ( SPA-GMN-TOKNYUIN    NOT =   SPACE )
               MOVE    SPA-NAI-TOKUTEI-G
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-TOKNYUIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事時間設定処理
      *****************************************************************
       2300-SKJTIME-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-SKJTIMEJMA TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           IF    ( SPA-NAI-STS-SKJTIME =   4   OR  9 )
                MOVE    4          TO  LNK-ODRITEM-STATUS (IDX-ITM)
           ELSE
                MOVE    0          TO  LNK-ODRITEM-STATUS (IDX-ITM)
           END-IF
      *
           MOVE    SPA-GMN-SKJTIME TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-SKJTIME-SET-EXT.
           EXIT.
      *****************************************************************
      *    日数設定処理
      *****************************************************************
       2300-NISSU-SET-SEC              SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-NISSUJMA TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           IF    ( SPA-NAI-STS-NISSU =   4   OR  9 )
                MOVE    4            TO  LNK-ODRITEM-STATUS (IDX-ITM)
           ELSE
                MOVE    0            TO  LNK-ODRITEM-STATUS (IDX-ITM)
           END-IF
      *
           MOVE    SPA-GMN-NISSU     TO  LNK-ODRITEM-MEMO   (IDX-ITM)
      *
           .
       2300-NISSU-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院事由設定処理
      *****************************************************************
       2300-TAIINCD-SET-SEC            SECTION.
      *
           IF    ( SPA-NAI-STS-TAIINCD      = 9 )
            AND  ( SPA-GMN-TAIINCD          = SPACE )
               GO  TO  2300-TAIINCD-SET-EXT
           END-IF
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-TAIINCDJMA TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-STS-TAIINCD     = 4 )
           WHEN  ( SPA-NAI-STS-TAIINCD     = 9 )
             AND ( SPA-GMN-TAIINCD     NOT = SPACE )
               MOVE    4           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           WHEN  OTHER
               MOVE    0           TO  LNK-ODRITEM-STATUS (IDX-ITM)
           END-EVALUATE
      *
           MOVE    SPA-GMN-TAIINCD TO  LNK-ODRITEM-ITEM (IDX-ITM)
           MOVE    SPA-GMN-TAIINCDMEI
                                   TO  LNK-ODRITEM-MEMO (IDX-ITM)
      *
           .
       2300-TAIINCD-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院科設定処理
      *****************************************************************
       2300-NYUINKA-SET-SEC            SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-NYUINKAJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           IF    ( SPA-NAI-STS-NYUINKA  =   4   OR  9 )
               MOVE    4           TO   LNK-ODRITEM-STATUS (IDX-ITM)
           ELSE
               MOVE    0           TO   LNK-ODRITEM-STATUS (IDX-ITM)
           END-IF
      *
           MOVE    SPA-GMN-NYUINKA TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-NYUINKA-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ設定処理
      *****************************************************************
       2300-HKNCOMBI-SET-SEC           SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-HKNCOMBIJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           IF    ( SPA-NAI-STS-HKNCOMBI  = 4   OR  9 )
               MOVE    4           TO   LNK-ODRITEM-STATUS (IDX-ITM)
           ELSE
               MOVE    0           TO   LNK-ODRITEM-STATUS (IDX-ITM)
           END-IF
      *
           MOVE    SPA-GMN-HKNCOMBI
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-HKNCOMBI-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-SODR-COMMON-SET-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM          TO  SODR-HOSPNUM
           MOVE    ORDER-KARTE-KEY     TO  SODR-KARTE-KEY
           MOVE    ORDER-ORDERNUM      TO  SODR-ORDERNUM
           MOVE    ORDER-NYUGAIKBN     TO  SODR-NYUGAIKBN
           MOVE    SPA-GMN-PTNUM       TO  SODR-PTNUM
           MOVE    SPA-NAI-PTID        TO  SODR-PTID
           MOVE    ORDER-SRYKA         TO  SODR-SRYKA
           MOVE    ORDER-SRYYMD        TO  SODR-SRYYMD
           MOVE    ORDER-HKNCOMBI      TO  SODR-HKNCOMBI
           MOVE    ORDER-ORDERID       TO  SODR-ORDERID
           MOVE    ORDER-UKEYMD        TO  SODR-UKEYMD
           MOVE    ORDER-UKEHMS        TO  SODR-UKEHMS
           MOVE    "1"                 TO  SODR-PROCKBN
           MOVE    ORDER-KEYGENERATE   TO  SODR-KEYGENERATE
           MOVE    WRK-INF-DIALOG      TO  SODR-INF-DIALOG
      *
           MOVE    SPA-OPID            TO  SODR-OPID
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SODR-UPYMD
           MOVE    SMCNDATE-HMS        TO  SODR-UPHMS
      *
           .
       2300-SODR-COMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-ODRITEM-COMMON-SET-SEC       SECTION.
      *
           COMPUTE    IDX-ITM  =   IDX-ITM +   1
      *
           MOVE    ORDER-HOSPNUM   TO  LNK-ODRITEM-HOSPNUM  (IDX-ITM)
           MOVE    ORDER-KARTE-KEY TO  LNK-ODRITEM-KARTE-KEY(IDX-ITM)
           MOVE    ORDER-ORDERNUM  TO  LNK-ODRITEM-ORDERNUM(IDX-ITM)
           MOVE    IDX-ITM         TO  LNK-ODRITEM-ITEMNUM (IDX-ITM)
      *
           .
       2300-ODRITEM-COMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-I0IDCD      NOT =   SPACE )
               PERFORM 520-I0IDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I06    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE   I06-ALLIDOLST-ROW    TO  WRK-ALLIDOLST-ROW
           MOVE   I06-KJNIDOLST-ROW    TO  WRK-KJNIDOLST-ROW
           MOVE   I06-ERRMESLST-ROW    TO  WRK-ERRMESLST-ROW
      *
           INITIALIZE                  I06
      *
           IF    ( FLG-ALLIDOLST-ROW   =   1 )
               MOVE    ZERO                TO  I06-ALLIDOLST-ROW
                                               I06-ALLIDOLST-ROWATTR
           ELSE
               MOVE    WRK-ALLIDOLST-ROW   TO  I06-ALLIDOLST-ROW
               MOVE    ZERO                TO  I06-ALLIDOLST-ROWATTR
           END-IF
      *
           IF    ( FLG-KJNIDOLST-ROW   =   1 )
               MOVE    ZERO                TO  I06-KJNIDOLST-ROW
                                               I06-KJNIDOLST-ROWATTR
           ELSE
               MOVE    WRK-KJNIDOLST-ROW   TO  I06-KJNIDOLST-ROW
               MOVE    ZERO                TO  I06-KJNIDOLST-ROWATTR
           END-IF
      *
           IF    ( FLG-ERRMESLST-ROW   =   1 )
               MOVE    ZERO                TO  I06-ERRMESLST-ROW
                                               I06-ERRMESLST-ROWATTR
           ELSE
               MOVE    WRK-ERRMESLST-ROW   TO  I06-ERRMESLST-ROW
               MOVE    ZERO                TO  I06-ERRMESLST-ROWATTR
           END-IF
      *
      *
           MOVE    SPA-GMN-PTNUM       TO  I06-PTNUM
           MOVE    SPA-GMN-NAME        TO  I06-NAME
           MOVE    SPA-GMN-SEX         TO  I06-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  I06-BIRTHDAY
           MOVE    SPA-GMN-AGE         TO  I06-AGE
           MOVE    SPA-GMN-HSRYKA      TO  I06-HSRYKA
           MOVE    SPA-GMN-HHKNCOMBI   TO  I06-HHKNCOMBI
           MOVE    SPA-GMN-IDOJIYU     TO  I06-IDOJIYU
           MOVE    SPA-GMN-LBL-MAEIDOMEI
                                       TO  I06-LBL-MAEIDOMEI
           MOVE    SPA-GMN-MAEIDOYMD   TO  I06-MAEIDOYMD
      *
           MOVE    SPA-GMN-SELNUM      TO  I06-SELNUM
           MOVE    SPA-GMN-UKEYMD      TO  I06-UKEYMD
      *
           MOVE    SPA-GMN-BRMNUM      TO  I06-BRMNUM
      *
           MOVE    SPA-GMN-BTUNAME-G   TO  I06-BTUNAME
           MOVE    SPA-GMN-BTUNAME-MAX TO  I06-BTUNAME-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-BTUNAME-MAX )
               MOVE    SPA-GMN-BTUNAMELST (IDX-CMB)
                                       TO  I06-BTUNAME-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SAGAKU-G    TO  I06-SAGAKU
           MOVE    SPA-GMN-SAGAKU-MAX  TO  I06-SAGAKU-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SAGAKU-MAX )
               MOVE    SPA-GMN-SAGAKULST (IDX-CMB)
                                       TO  I06-SAGAKU-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-NYUINYMD    TO  I06-NYUINYMD
      *
           MOVE    SPA-GMN-NYUINKA-G   TO  I06-NYUINKA
           MOVE    SPA-GMN-NYUINKA-MAX TO  I06-NYUINKA-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-NYUINKA-MAX )
               MOVE    SPA-GMN-NYUINKALST (IDX-CMB)
                                       TO  I06-NYUINKA-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SHOKAI-G    TO  I06-SHOKAI
           MOVE    SPA-GMN-SHOKAI-MAX  TO  I06-SHOKAI-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SHOKAI-MAX )
               MOVE    SPA-GMN-SHOKAILST (IDX-CMB)
                                       TO  I06-SHOKAI-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SHONUM      TO  I06-SHONUM
      *
           MOVE    SPA-GMN-LBL-SHONYUINYMD
                                       TO  I06-LBL-SHONYUINYMD
      *
           MOVE    SPA-GMN-TAIINYMD    TO  I06-TAIINYMD
      *
           MOVE    SPA-GMN-TAIINCD-G   TO  I06-TAIINCD
           MOVE    SPA-GMN-TAIINCD-MAX TO  I06-TAIINCD-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TAIINCD-MAX )
               MOVE    SPA-GMN-TAIINCDLST (IDX-CMB)
                                       TO  I06-TAIINCD-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-DR-G (1)    TO  I06-DR (1)
           MOVE    SPA-GMN-DR-G (2)    TO  I06-DR (2)
           MOVE    SPA-GMN-DR-G (3)    TO  I06-DR (3)
           MOVE    SPA-GMN-DR-MAX  TO  I06-DR-COUNT (1)
                                       I06-DR-COUNT (2)
                                       I06-DR-COUNT (3)
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-DR-MAX )
               MOVE    SPA-GMN-DRLST (IDX-CMB)
                                   TO  I06-DR-ITEM (1 IDX-CMB)
                                       I06-DR-ITEM (2 IDX-CMB)
                                       I06-DR-ITEM (3 IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-HKNCOMBI-G  TO  I06-HKNCOMBI
           MOVE    SPA-GMN-HKNCOMBI-MAX
                                       TO  I06-HKNCOMBI-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-HKNCOMBI-MAX )
               MOVE    SPA-GMN-HKNCOMBILST (IDX-CMB)
                                       TO  I06-HKNCOMBI-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-NYUKBN-G    TO  I06-NYUKBN
           MOVE    SPA-GMN-NYUKBN-MAX  TO  I06-NYUKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-NYUKBN-MAX )
               MOVE    SPA-GMN-NYUKBNLST (IDX-CMB)
                                       TO  I06-NYUKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-TOKNYUIN-G  TO  I06-TOKNYUIN
           MOVE    SPA-GMN-TOKNYUIN-MAX
                                       TO  I06-TOKNYUIN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TOKNYUIN-MAX )
               MOVE    SPA-GMN-TOKNYUINLST (IDX-CMB)
                                       TO  I06-TOKNYUIN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SNTNYUIN    TO  I06-SNTNYUIN
      *
           MOVE    SPA-GMN-SKJTIME-G    TO  I06-SKJTIME
           MOVE    SPA-GMN-SKJTIME-MAX  TO  I06-SKJTIME-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SKJTIME-MAX )
               MOVE    SPA-GMN-SKJTIMELST (IDX-CMB)
                                       TO  I06-SKJTIME-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-NISSU       TO  I06-NISSU
      *
           MOVE    SPA-GMN-ALLIDOLST-MAX   TO  I06-ALLIDOLST-COUNT
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-ALLIDOLST-MAX )
      *
               MOVE    SPA-GMN-TALLNUM    (IDX-LST)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  I06-TALLNUM    (IDX-LST)
               MOVE    SPA-GMN-TALLIDOYMD (IDX-LST)
                                           TO  I06-TALLIDOYMD (IDX-LST)
               MOVE    SPA-GMN-TALLIDOJIYU(IDX-LST)
                                           TO  I06-TALLIDOJIYU(IDX-LST)
               MOVE    SPA-GMN-TALLPTNUM  (IDX-LST) 
                                           TO  I06-TALLPTNUM  (IDX-LST)
               MOVE    SPA-GMN-TALLNAME   (IDX-LST) 
                                           TO  I06-TALLNAME   (IDX-LST)
               EVALUATE    SPA-GMN-TALLSTS (IDX-LST)
               WHEN    1
                   MOVE    "　重"          TO  I06-TALLERR    (IDX-LST)
               WHEN    2
                   MOVE    "　有"          TO  I06-TALLERR    (IDX-LST)
               WHEN    3
                   MOVE    "　削"          TO  I06-TALLERR    (IDX-LST)
               WHEN    4
                   MOVE    "　未"          TO  I06-TALLERR    (IDX-LST)
               WHEN    5
                   MOVE    "保留"          TO  I06-TALLERR    (IDX-LST)
               END-EVALUATE
      *
               IF    ( SPA-GMN-ALLIDOLST-SEL   =   IDX-LST )
                   MOVE    "T"             TO  I06-ALLIDOLST-SELECT
                                                             (IDX-LST)
               ELSE
                   MOVE    "F"             TO  I06-ALLIDOLST-SELECT
                                                             (IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KJNIDOLST-MAX   TO  I06-KJNIDOLST-COUNT
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-KJNIDOLST-MAX )
      *
               MOVE    IDX-LST             TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  I06-TKJNNUM     (IDX-LST)
               MOVE    SPA-GMN-TKJNUKEYMD  (IDX-LST)
                                           TO  I06-TKJNUKEYMD  (IDX-LST)
               MOVE    SPA-GMN-TKJNIDOYMD  (IDX-LST)
                                           TO  I06-TKJNIDOYMD  (IDX-LST)
               MOVE    SPA-GMN-TKJNIDOJIYU (IDX-LST)
                                           TO  I06-TKJNIDOJIYU (IDX-LST)
               EVALUATE    SPA-GMN-TKJNSTS (IDX-LST)
               WHEN    1
                   MOVE    "重"            TO  I06-TKJNERR    (IDX-LST)
               WHEN    2
                   MOVE    "有"            TO  I06-TKJNERR    (IDX-LST)
               WHEN    3
                   MOVE    "削"            TO  I06-TKJNERR    (IDX-LST)
               WHEN    4
                   MOVE    "未"            TO  I06-TKJNERR    (IDX-LST)
               WHEN    5
                   MOVE    "保留"          TO  I06-TKJNERR    (IDX-LST)
               END-EVALUATE
      *
               IF    ( SPA-GMN-KJNIDOLST-SEL   =   IDX-LST )
                   MOVE    "T"             TO  I06-KJNIDOLST-SELECT
                                                               (IDX-LST)
               ELSE
                   MOVE    "F"             TO  I06-KJNIDOLST-SELECT
                                                               (IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-ERRMESLST-MAX   TO  I06-ERRMESLST-COUNT
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-ERRMESLST-MAX )
      *
               MOVE    SPA-GMN-TERRCD (IDX-LST)
                                           TO  I06-TERRCD (IDX-LST)
               MOVE    SPA-GMN-TERRMES(IDX-LST)
                                           TO  I06-TERRMES(IDX-LST)
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-SELNYUKHN       =   1 )
               MOVE    "入院料選択"        TO  I06-LBL-TOKNYUIN
           ELSE
               MOVE    "特定入院料"        TO  I06-LBL-TOKNYUIN
           END-IF
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-INSENSITIVE  TO      I06-BRMNUM-STATE
                                               I06-BTUNAME-STATE
                                               I06-BTUNAME-ITEM-STATE
                                               I06-SAGAKU-STATE
                                               I06-SAGAKU-ITEM-STATE
                                               I06-NYUINYMD-STATE
                                               I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-SHOKAI-STATE
                                               I06-SHOKAI-ITEM-STATE
                                               I06-SHONUM-STATE
                                               I06-TAIINYMD-STATE
                                               I06-TAIINCD-STATE
                                               I06-TAIINCD-ITEM-STATE
                                               I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-NYUKBN-STATE
                                               I06-NYUKBN-ITEM-STATE
                                               I06-TOKNYUIN-STATE
                                               I06-TOKNYUIN-ITEM-STATE
                                               I06-SNTNYUIN-STATE
                                               I06-SKJTIME-STATE
                                               I06-SKJTIME-ITEM-STATE
                                               I06-NISSU-STATE
                                               I06-B06S-STATE
                                               I06-B07S-STATE
                                               I06-B12-STATE
      *
           IF    ( SPA-GMN-ALLIDOLST-SEL       =   ZERO )
               MOVE    WIDGET-NORMAL   TO      I06-BRMNUM-STATE
                                               I06-BTUNAME-STATE
                                               I06-BTUNAME-ITEM-STATE
                                               I06-SAGAKU-STATE
                                               I06-SAGAKU-ITEM-STATE
                                               I06-NYUINYMD-STATE
                                               I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-SHOKAI-STATE
                                               I06-SHOKAI-ITEM-STATE
                                               I06-SHONUM-STATE
                                               I06-TAIINYMD-STATE
                                               I06-TAIINCD-STATE
                                               I06-TAIINCD-ITEM-STATE
                                               I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-NYUKBN-STATE
                                               I06-NYUKBN-ITEM-STATE
                                               I06-TOKNYUIN-STATE
                                               I06-TOKNYUIN-ITEM-STATE
                                               I06-SNTNYUIN-STATE
                                               I06-SKJTIME-STATE
                                               I06-SKJTIME-ITEM-STATE
                                               I06-NISSU-STATE
               MOVE    WIDGET-INSENSITIVE  TO  I06-B12-STATE
           ELSE
               IF    ( SPA-NAI-STATUS-ORDER    =   "2" )
                OR   ( SPA-NAI-STATUS-ORDER    =   "5" )
                OR  (( SPA-NAI-STATUS-ORDER    =   "4" )
                 AND ( SPA-NAI-DIRECT-KHNERR   =   ZERO ))
                   EVALUATE    SPA-NAI-ORDERID
                   WHEN    NORDERID-NYUINID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-BRMNUM-STATE
                                               I06-BTUNAME-STATE
                                               I06-BTUNAME-ITEM-STATE
                                               I06-SAGAKU-STATE
                                               I06-SAGAKU-ITEM-STATE
                                               I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-SHOKAI-STATE
                                               I06-SHOKAI-ITEM-STATE
                                               I06-SHONUM-STATE
                                               I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-NYUKBN-STATE
                                               I06-NYUKBN-ITEM-STATE
                                               I06-TOKNYUIN-STATE
                                               I06-TOKNYUIN-ITEM-STATE
                                               I06-B12-STATE
                                               I06-SNTNYUIN-STATE
                   WHEN    NORDERID-TAIINID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-TAIINCD-STATE
                                               I06-TAIINCD-ITEM-STATE
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-B12-STATE
                   WHEN    NORDERID-GAIHKID
                   WHEN    NORDERID-GBACKID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-B12-STATE
                   WHEN    NORDERID-FNORLID
                   WHEN    NORDERID-FSPCLID
                   WHEN    NORDERID-FSELEID
                   WHEN    NORDERID-FSTOPID
                   WHEN    NORDERID-FSPSLID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-SKJTIME-STATE
                                               I06-SKJTIME-ITEM-STATE
                                               I06-NISSU-STATE
                                               I06-B12-STATE
                   WHEN    NORDERID-IDODRID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-B12-STATE
                   WHEN   NORDERID-IDOBTUID
                   WHEN   NORDERID-IDOBRMID
                   WHEN   NORDERID-IDOKAID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-BRMNUM-STATE
                                               I06-BTUNAME-STATE
                                               I06-BTUNAME-ITEM-STATE
                                               I06-SAGAKU-STATE
                                               I06-SAGAKU-ITEM-STATE
                                               I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-NYUKBN-STATE
                                               I06-NYUKBN-ITEM-STATE
                                               I06-TOKNYUIN-STATE
                                               I06-TOKNYUIN-ITEM-STATE
                                               I06-B12-STATE
                                               I06-SNTNYUIN-STATE
                   WHEN    NORDERID-DELNYUINID
                   WHEN    NORDERID-ALLDELNYUINID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-B12-STATE
                   END-EVALUATE
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-SHOKAI      = "1" )
               MOVE    WIDGET-INSENSITIVE  TO  I06-SHONUM-STATE
           END-IF
      *
           IF    ( SPA-GMN-ALLIDOLST-PAGE  >   1 )
               MOVE    WIDGET-NORMAL       TO  I06-B06S-STATE
           END-IF
      *
           IF    ( SPA-GMN-ALLIDOLST-NEXT  =   1 )
               MOVE    WIDGET-NORMAL       TO  I06-B07S-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5011-INPUT-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    01
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    02
               MOVE    "UKEYMD"        TO  MCP-WIDGET
           WHEN    03
               MOVE    "BRMNUM"        TO  MCP-WIDGET
           WHEN    04
               MOVE    "BTUNAME"       TO  MCP-WIDGET
           WHEN    05
               MOVE    "SAGAKU"        TO  MCP-WIDGET
           WHEN    06
               MOVE    "NYUINYMD"      TO  MCP-WIDGET
           WHEN    07
               MOVE    "NYUINKA"       TO  MCP-WIDGET
           WHEN    08
               MOVE    "SHOKAI"        TO  MCP-WIDGET
           WHEN    09
               MOVE    "SHONUM"        TO  MCP-WIDGET
           WHEN    10
               MOVE    "TAIINYMD"      TO  MCP-WIDGET
           WHEN    11
               MOVE    "TAIINCD"       TO  MCP-WIDGET
           WHEN    12
               MOVE    "DR1"           TO  MCP-WIDGET
           WHEN    13
               MOVE    "DR2"           TO  MCP-WIDGET
           WHEN    14
               MOVE    "DR3"           TO  MCP-WIDGET
           WHEN    15
               MOVE    "HKNCOMBI"      TO  MCP-WIDGET
           WHEN    16
               MOVE    "NYUKBN"        TO  MCP-WIDGET
           WHEN    17
               MOVE    "TOKNYUIN"      TO  MCP-WIDGET
           WHEN    18
               MOVE    "SKJTIME"       TO  MCP-WIDGET
           WHEN    19
               MOVE    "NISSU"         TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    03              TO  SPA-GMN-CUR
           WHEN    "UKEYMD"
               MOVE    03              TO  SPA-GMN-CUR
           WHEN    "BRMNUM"
               MOVE    04              TO  SPA-GMN-CUR
           WHEN    "BTUNAME"
               MOVE    05              TO  SPA-GMN-CUR
           WHEN    "SAGAKU"
               MOVE    06              TO  SPA-GMN-CUR
           WHEN    "NYUINYMD"
               MOVE    07              TO  SPA-GMN-CUR
           WHEN    "NYUINKA"
               MOVE    08              TO  SPA-GMN-CUR
           WHEN    "SHOKAI"
               MOVE    09              TO  SPA-GMN-CUR
           WHEN    "SHONUM"
               MOVE    10              TO  SPA-GMN-CUR
           WHEN    "TAIINYMD"
               MOVE    11              TO  SPA-GMN-CUR
           WHEN    "TAIINCD"
               MOVE    12              TO  SPA-GMN-CUR
           WHEN    "DR1"
               MOVE    13              TO  SPA-GMN-CUR
           WHEN    "DR2"
               MOVE    14              TO  SPA-GMN-CUR
           WHEN    "DR3"
               MOVE    15              TO  SPA-GMN-CUR
           WHEN    "HKNCOMBI"
               MOVE    16              TO  SPA-GMN-CUR
           WHEN    "NYUKBN"
               MOVE    17              TO  SPA-GMN-CUR
           WHEN    "TOKNYUIN"
               MOVE    18              TO  SPA-GMN-CUR
           WHEN    "SKJTIME"
               MOVE    19              TO  SPA-GMN-CUR
           WHEN    "NISSU"
               MOVE    212             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5011-INPUT-CUR-SKIP-SEC         SECTION.
      *
           IF    ( SPA-GMN-CUR         =   3 )
               IF    ( I06-BRMNUM-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   4 )
               IF    ( I06-BTUNAME-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   5 )
               IF    ( I06-SAGAKU-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    6           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   6 )
               IF    ( I06-NYUINYMD-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    7           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   7 )
               IF    ( I06-NYUINKA-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    8           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   8 )
               IF    ( I06-SHOKAI-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    9           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   9 )
               IF    ( I06-SHONUM-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    10           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   10 )
               IF    ( I06-TAIINYMD-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    11           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   11 )
               IF    ( I06-TAIINCD-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    12           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   12 )
               IF    ( I06-DR-STATE (1)    =   WIDGET-INSENSITIVE )
                   MOVE    13           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   13 )
               IF    ( I06-DR-STATE (2)    =   WIDGET-INSENSITIVE )
                   MOVE    14          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   14 )
               IF    ( I06-DR-STATE (3)    =   WIDGET-INSENSITIVE )
                   MOVE    15          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   15 )
               IF    ( I06-HKNCOMBI-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    16          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   16 )
               IF    ( I06-NYUKBN-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    17          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   17 )
               IF    ( I06-TOKNYUIN-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    18          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   18 )
               IF    ( I06-SKJTIME-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    19          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   19 )
               IF    ( I06-NISSU-STATE     =   WIDGET-INSENSITIVE )
                   MOVE    212         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   212 )
               IF    ( I06-B12-STATE   =   WIDGET-INSENSITIVE )
                   IF    ( SPA-GMN-ALLIDOLST-MAX   =   ZERO )
                       MOVE    2       TO  SPA-GMN-CUR
                   ELSE
                       MOVE    1       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       5011-INPUT-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "異動日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "病室番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "病棟名入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "室料差額入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "入院科入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "初回入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "初歴入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "退院事由入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "担当医入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "保険組合せ入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "入院区分入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "特定入院料入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "食事時間入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0019"
               IF    ( WRK-ERRMSG      =   SPACE )
                   MOVE    "一括入力欄入力エラー" 
                                       TO  SPA-ERRMSG
               ELSE
                   MOVE    WRK-ERRMSG  TO  SPA-ERRMSG
               END-IF
           WHEN    "0020"
               MOVE    "一括入力欄は半角文字で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0021"
               MOVE    "一括入力欄の一文字目は＊を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "異動歴を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ERR
           MOVE    SPA-ERRCD           TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I0ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I06"               TO  SPA-MOTOPG
           MOVE    "I0ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I0ERR"             TO  MCP-WINDOW
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
       520-I0IDSET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-I0IDMSG
      *
           EVALUATE    SPA-I0IDCD
           WHEN    "2001"
               MOVE    "異動歴を削除します"
                                       TO  WRK-I0IDMSG
           WHEN    "2002"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "定期請求が月末のみに変更されています。"
                       "月途中で作成した請求データは取消します"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "2003"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "退院後の診療日で入力された診療行為は"
                       "削除されます。確認してください"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "2004"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院日が他院退院日と重複します。"
                       "他院歴の通算日数を減算しますか？"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "2005"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院日が他院退院日と重複します。"
                       "よろしいですか？"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ID1
           MOVE    SPA-I0IDCD          TO  I0ID1-ID1CODE
           MOVE    WRK-I0IDMSG         TO  I0ID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "I06"               TO  SPA-MOTOPG
           MOVE    "I0ID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I0ID1"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-I0IDSET-EXT.
           EXIT.
      *****************************************************************
      *    老人年齢チェック
      *****************************************************************
       800-ROUJINAGE-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-ROUJIN
                                       FLG-LIFE-RJN
      *
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM          TO  AGECHK-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  AGECHK-PTID
           MOVE    SPA-NAI-KJNYMD      TO  AGECHK-SRYYMD
           MOVE    SPA-NAI-BIRTHDAY    TO  AGECHK-BIRTHDAY
           MOVE    SPA-GMN-HKNCOMBI    TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING   ORCSAGECHKAREA
                                               SPA-AREA
           IF    ( AGECHK-RC       NOT =   ZERO )
               INITIALIZE              ORCSAGECHKAREA
           END-IF
           IF    ( AGECHK-ROUJIN2      =   1 )
               MOVE    1               TO  FLG-ROUJIN
           END-IF
      *
           IF    ( SPA-NAI-HKNNUM  NOT =  "971" AND "973" )
             OR  ( AGECHK-HKNKBN       =  4 OR 5)
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-KJNYMD         >=  CONST-H200401 )
                   IF    ( AGECHK-NENREI2-YY  >=  65 )
                       MOVE    1           TO  FLG-LIFE-RJN
                   END-IF
               WHEN  ( SPA-NAI-KJNYMD         >=  CONST-H181001 )
                   MOVE    FLG-ROUJIN      TO  FLG-LIFE-RJN
                   IF    ( AGECHK-NENREI2-YY   >=  70 )
                       MOVE    1           TO  FLG-LIFE-RJN
                   END-IF
               END-EVALUATE
      *
               IF    ( FLG-LIFE-RJN    =   1 )
                   IF    ( AGECHK-HKNKBN   =   4 OR 5 )
                       MOVE    2       TO  FLG-LIFE-RJN
                   END-IF
               END-IF
           END-IF
      *
           .
       800-ROUJINAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
      *
           MOVE    ZERO                TO  WRK-SYMD
           MOVE    SPACE               TO  WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
      *    増減パターン 1:日／2:月／3:年
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
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
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
           IF  ( WRK-INPUT             =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-INPUT       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-INPUT
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   1 )
                       MOVE    1       TO  FLG-ERR
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-ERR
               END-IF
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理(KEY)
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO   HKNCOMBI-REC
           ELSE
               INITIALIZE                   HKNCOMBI-REC
               MOVE    1               TO   FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索処理(KEY6)
      *****************************************************************
       900-PTKOHINF-KEY6-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKOHINF
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTKOHINF-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報検索処理(KEY11)
      *****************************************************************
       900-PTHKNINF-KEY11-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTHKNINF
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTHKNINF-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    WRK-TNS-KJNYMD  TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダー検索(KEY3)
      *****************************************************************
       900-ORDER-KEY3-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-ORDER
      *
           INITIALIZE                  ORDER-REC
           MOVE    SPA-HOSPNUM      TO  ORDER-HOSPNUM
           MOVE    SPA-NAI-UKEYMD  TO  ORDER-UKEYMD
           MOVE    ORDER-REC       TO  MCPDATA-REC
           MOVE    "tbl_order"     TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ORDER-REC
           ELSE
               INITIALIZE              ORDER-REC
               MOVE    1           TO  FLG-ORDER
           END-IF
      *
      *
           .
       900-ORDER-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダーFETCH(KEY3)
      *****************************************************************
       900-ORDER-KEY3-FET-SEC          SECTION.
      *
           MOVE    "tbl_order"     TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ORDER-REC
           ELSE
               INITIALIZE              ORDER-REC
               MOVE    1           TO  FLG-ORDER
           END-IF
      *
           .
       900-ORDER-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダー検索(KEY4)
      *****************************************************************
       900-ORDER-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-ORDER
      *
           INITIALIZE                  ORDER-REC
           MOVE    SPA-HOSPNUM      TO  ORDER-HOSPNUM
           MOVE    SPA-GMN-PTNUM   TO  ORDER-PTNUM
           MOVE    ORDER-REC       TO  MCPDATA-REC
           MOVE    "tbl_order"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ORDER-REC
           ELSE
               INITIALIZE              ORDER-REC
               MOVE    1           TO  FLG-ORDER
           END-IF
      *
      *
           .
       900-ORDER-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダーFETCH(KEY4)
      *****************************************************************
       900-ORDER-KEY4-FET-SEC          SECTION.
      *
           MOVE    "tbl_order"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ORDER-REC
           ELSE
               INITIALIZE              ORDER-REC
               MOVE    1           TO  FLG-ORDER
           END-IF
      *
           .
       900-ORDER-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダー検索(KEY)
      *****************************************************************
       900-ORDER-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           INITIALIZE                      ORDER-REC
           MOVE    SPA-HOSPNUM          TO  ORDER-HOSPNUM
           MOVE    SPA-NAI-KARTE-KEY   TO  ORDER-KARTE-KEY
           MOVE    SPA-NAI-ORDERNUM    TO  ORDER-ORDERNUM
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  ORDER-REC
           ELSE
               INITIALIZE                  ORDER-REC
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダ項目検索(KEY4)
      *****************************************************************
       900-ODRITEM-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-ODRITEM
      *
           INITIALIZE                      ODRITEM-REC
           MOVE    SPA-HOSPNUM          TO  ODRITEM-HOSPNUM
           MOVE    SPA-NAI-KARTE-KEY   TO  ODRITEM-KARTE-KEY
           MOVE    SPA-NAI-ORDERNUM    TO  ODRITEM-ORDERNUM
           MOVE    ODRITEM-REC     TO  MCPDATA-REC
           MOVE    "tbl_odritem"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ODRITEM-REC
           ELSE
               INITIALIZE              ODRITEM-REC
               MOVE    1           TO  FLG-ODRITEM
           END-IF
      *
           .
       900-ODRITEM-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダ項目FETCH(KEY4)
      *****************************************************************
       900-ODRITEM-KEY4-FET-SEC          SECTION.
      *
           MOVE    "tbl_odritem"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ODRITEM-REC
           ELSE
               INITIALIZE              ODRITEM-REC
               MOVE    1           TO  FLG-ODRITEM
           END-IF
      *
           .
       900-ODRITEM-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    WRK-PTINF-PTID  TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY3)
      *****************************************************************
       900-PTNYUINRRK-KEY3-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-PTID    TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY42)
      *****************************************************************
       900-PTNYUINRRK-KEY42-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-HIDOYMD TO  PTNYUINRRK-TENNYUYMD
                                       PTNYUINRRK-TENSTUYMD
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY42-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY30)
      *****************************************************************
       900-PTNYUINRRK-KEY30-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY30-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY35)
      *****************************************************************
       900-PTNYUINRRK-KEY35-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY35-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY39)
      *****************************************************************
       900-PTNYUINRRK-KEY39-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY39-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報検索処理(KEY)
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM          TO  PTNUM-HOSPNUM
           MOVE    WRK-PTNUM-PTID      TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報検索処理(KEY2)
      *****************************************************************
       900-PTNUM-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           IF    ( WRK-PTNUM-PTNUM     =   SPACE )
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           ELSE
      *
               INITIALIZE                  PTNUM-REC
               MOVE    SPA-HOSPNUM          TO  PTNUM-HOSPNUM
               STRING  WRK-PTNUM-PTNUM     DELIMITED   BY  SPACE
                       "%"                 DELIMITED   BY  SIZE
               INTO    PTNUM-PTNUM
               END-STRING
      *
               MOVE    PTNUM-REC           TO  MCPDATA-REC
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
               ELSE
                   INITIALIZE                  PTNUM-REC
                   MOVE    1               TO  FLG-PTNUM
               END-IF
      *
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-PTNUM-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY23-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY7)
      *****************************************************************
       900-SYSKANRI-KEY23-FET-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
