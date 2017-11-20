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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORAPI052BR2V3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 点数マスタ返却
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/06/30    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    一時ファイル対応
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT             PIC 9(10).
           03  CNT-OUT1            PIC 9(10).
           03  CNT-XMLCOUNT        PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC X(02).
           03  WRK-PARA-TERMID     PIC X(64).
           03  WRK-PARA-BASE-YMD   PIC X(08).
           03  WRK-PARA-ERRFILE    PIC X(64).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
      *
           03  WRK-CNT             PIC 9(10).
           03  WRK-CNT-X           REDEFINES   WRK-CNT
                                   PIC X(10).
      *
           03  WRK-PARAM.
               05  WRK-PARAM-ORCA-UID  PIC X(36).
               05  WRK-PARAM-KARTE-UID PIC X(36).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC X(04).
               05  WRK-SMM         PIC X(02).
               05  WRK-SDD         PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY      PIC X(04).
               05  WRK-HEN-H1      PIC X(01).
               05  WRK-HEN-MM      PIC X(02).
               05  WRK-HEN-H2      PIC X(01).
               05  WRK-HEN-DD      PIC X(02).
      *
           03  WRK-THMS.
               05  WRK-THH         PIC X(02).
               05  WRK-TMM         PIC X(02).
               05  WRK-TSS         PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH     PIC X(02).
               05  WRK-HEN-T1      PIC X(01).
               05  WRK-HEN-TMM     PIC X(02).
               05  WRK-HEN-T2      PIC X(01).
               05  WRK-HEN-TSS     PIC X(02).
      *
      *    点数データ(1294 byte)
       01  TENSULST-REC.
           03  TENSULST-OCC          OCCURS   1000.
               05  TENSULST-SRYCD              PIC X(09).
               05  TENSULST-YUKOSTYMD          PIC X(10).
               05  TENSULST-YUKOEDYMD          PIC X(10).
               05  TENSULST-SRYKBN             PIC X(02).
               05  TENSULST-SRYSYUKBN          PIC X(03).
               05  TENSULST-YUKOKETA           PIC 9(02).
               05  TENSULST-NAME               PIC X(200).
               05  TENSULST-KANAYUKOKETA       PIC 9(02).
               05  TENSULST-KANANAME           PIC X(200).
               05  TENSULST-FORMALYUKOKETA     PIC 9(03).
               05  TENSULST-FORMALNAME         PIC X(400).
               05  TENSULST-TENSIKIBETU        PIC 9(01).
               05  TENSULST-TEN                PIC 999999999.99.
               05  TENSULST-TANICD             PIC X(03).
               05  TENSULST-TANIMOJISU         PIC 9(01).
               05  TENSULST-TANINAME           PIC X(24).
               05  TENSULST-DATAKBN            PIC 9(01).
               05  TENSULST-HKNTEKKBN          PIC 9(01).
               05  TENSULST-NYUGAITEKKBN       PIC 9(01).
               05  TENSULST-ROUTEKKBN          PIC 9(01).
      *21-40
               05  TENSULST-HOSPSRYKBN         PIC 9(01).
               05  TENSULST-OSINKBN            PIC 9(01).
               05  TENSULST-HOUKNSKBN          PIC 9(02).
               05  TENSULST-BYOKANRENKBN       PIC 9(02).
               05  TENSULST-SDOKNRYO           PIC 9(02).
               05  TENSULST-JITUDAY            PIC 9(01).
               05  TENSULST-DAYCNT             PIC 9(01).
               05  TENSULST-YKZKNRKBN          PIC 9(01).
               05  TENSULST-KZMCOMPSIKIBETU    PIC 9(01).
               05  TENSULST-KZMKGN             PIC 9(08).
               05  TENSULST-KZMJGN             PIC 9(08).
               05  TENSULST-KZM                PIC 9(08).
               05  TENSULST-KZMTEN             PIC 9999999.99.
               05  TENSULST-KZMERR             PIC 9(01).
               05  TENSULST-JGNCNT             PIC 9(03).
               05  TENSULST-JGNCNT1D           PIC 9(03).
               05  TENSULST-JGNCNT1W           PIC 9(03).
               05  TENSULST-JGNCNTETCM         PIC 9(02).
               05  TENSULST-JGNCNTETC          PIC 9(03).
               05  TENSULST-JGNCNTERR          PIC 9(01).
      *41-60
               05  TENSULST-CHUKSNCD           PIC X(04).
               05  TENSULST-CHUKSNTUBAN        PIC X(01).
               05  TENSULST-TSUSOKUAGE         PIC 9(01).
               05  TENSULST-KGNAGE             PIC X(02).
               05  TENSULST-JGNAGE             PIC X(02).
               05  TENSULST-TIMEKSNKBN         PIC 9(01).
               05  TENSULST-KIJUNTEIGENKBN     PIC 9(01).
               05  TENSULST-KIJUNTEIGENCD      PIC 9(04).
               05  TENSULST-LASERKBN           PIC 9(01).
               05  TENSULST-CHPMESUKSN         PIC 9(01).
               05  TENSULST-MICMESUKSN         PIC 9(01).
               05  TENSULST-DONORKBN           PIC 9(01).
               05  TENSULST-KNSJISKBN          PIC 9(01).
               05  TENSULST-KNSJISGRPKBN       PIC 9(02).
               05  TENSULST-TEIGENKBN          PIC 9(01).
               05  TENSULST-SEKIZUISOKUTEI     PIC 9(01).
               05  TENSULST-KEIBUJYUTU         PIC 9(01).
               05  TENSULST-AUTOHOUGO          PIC 9(01).
               05  TENSULST-GAIKANRIKBN        PIC 9(01).
               05  TENSULST-TUSOKUGAIKBN       PIC 9(01).
      *61-91
               05  TENSULST-HOUKATUTEIGENKBN   PIC 9(03).
               05  TENSULST-CHPNAISIKSN        PIC 9(01).
               05  TENSULST-AUTOFUNGO          PIC 9(01).
               05  TENSULST-CHIIKIKSN          PIC 9(01).
               05  TENSULST-BYOSYOKBN          PIC 9(01).
               05  TENSULST-CHPGYOKOSOTIKSN    PIC 9(01).
               05  TENSULST-SHORTSTAYOPE       PIC 9(01).
               05  TENSULST-BUIKBN             PIC 9(01).
               05  TENSULST-SANTEIRRKKBN       PIC 9(01).
               05  TENSULST-SSTKIJUNCDTBL      OCCURS 10.
                   07  TENSULST-SSTKIJUNCD         PIC 9(04).
               05  TENSULST-AGEKSNTBL          OCCURS 4.
                   07  TENSULST-AGEKSNKGN          PIC X(02).
                   07  TENSULST-AGEKSNJGN          PIC X(02).
                   07  TENSULST-AGEKSNSRYCD        PIC X(09).
      *92-110
               05  TENSULST-KENTAICOMMENT      PIC 9(01).
               05  TENSULST-NYUKHNKBN          PIC 9(02).
               05  TENSULST-NYUKHNKSNKBN       PIC 9(03).
               05  TENSULST-KANGOKSN           PIC 9(02).
               05  TENSULST-OLDTENSKB          PIC 9(01).
               05  TENSULST-OLDTEN             PIC 999999999.99.
               05  TENSULST-MADOKUKBN          PIC 9(01).
               05  TENSULST-SINKEIHAKAIKBN     PIC 9(01).
               05  TENSULST-SEIBUTUGAKUKBN     PIC 9(01).
               05  TENSULST-ZOEIZAIKBN         PIC 9(01).
               05  TENSULST-CSYYOURYO          PIC 9(05).
               05  TENSULST-YKZKBN             PIC 9(01).
               05  TENSULST-ZAIGATAKBN         PIC 9(01).
               05  TENSULST-KOUHATUKBN         PIC 9(01).
               05  TENSULST-LONGTOYOKBN        PIC 9(01).
               05  TENSULST-MEIUSESKB          PIC 9(01).
               05  TENSULST-TOKUKIZAIAGEKSNKBN PIC 9(01).
               05  TENSULST-SANSOKBN           PIC 9(01).
               05  TENSULST-TOKUKIZAISBT1      PIC 9(01).
      *111-130
               05  TENSULST-TOKUKIZAISBT2      PIC 9(01).
               05  TENSULST-JGNTEN             PIC 9(07).
               05  TENSULST-GAITENTTLKBN       PIC 9(03).
               05  TENSULST-NYUTENTTLKBN       PIC 9(03).
               05  TENSULST-CDKBN-KBN          PIC X(01).
               05  TENSULST-CDKBN-SYO          PIC 9(01).
               05  TENSULST-CDKBN-BU           PIC 9(02).
               05  TENSULST-CDKBN-KBNNUM       PIC 9(03).
               05  TENSULST-CDKBN-KBNNUM-EDA   PIC 9(02).
               05  TENSULST-CDKBN-KOUBAN       PIC 9(02).
               05  TENSULST-KOKUCHI-KBN        PIC X(01).
               05  TENSULST-KOKUCHI-SYO        PIC 9(01).
               05  TENSULST-KOKUCHI-BU         PIC 9(02).
               05  TENSULST-KOKUCHI-KBNNUM     PIC 9(03).
               05  TENSULST-KOKUCHI-KBNNUM-EDA PIC 9(02).
               05  TENSULST-KOKUCHI-KOUBAN     PIC 9(02).
               05  TENSULST-KOKUJISKBKBN1      PIC 9(01).
               05  TENSULST-KOKUJISKBKBN2      PIC 9(01).
               05  TENSULST-NAMECHGKBN         PIC 9(01).
               05  TENSULST-KANANAMECHGKBN     PIC 9(01).
      *131-90
               05  TENSULST-IDOKANREN          PIC X(09).
               05  TENSULST-KOUHYOJYUNNUM      PIC 9(09).
               05  TENSULST-YAKKAKJNCD         PIC X(12).
               05  TENSULST-SYUSAISKB          PIC 9(01).
               05  TENSULST-SYOMEIKANREN       PIC X(09).
               05  TENSULST-CHGYMD             PIC X(10).
               05  TENSULST-HAISIYMD           PIC X(10).
               05  TENSULST-KEIKASOCHIYMD      PIC X(10).
      ******** 05  TENSULST-TERMID             PIC X(16).
      ******** 05  TENSULST-OPID               PIC X(16).
               05  TENSULST-CREYMD             PIC X(10).
               05  TENSULST-UPYMD              PIC X(10).
               05  TENSULST-UPHMS              PIC X(08).
      *********05  TENSULST-HOSPNUM            PIC 9(02).
      *********05  TENSULST-MASTER-CLASS       PIC 9(01).
               05  TENSULST-GAZOOPESUP         PIC 9(01).
               05  TENSULST-IRYOKANSATUKBN     PIC 9(01).
               05  TENSULST-MASUISKBKBN        PIC 9(01).
               05  TENSULST-FUKUBIKUNAIKBN     PIC 9(01).
               05  TENSULST-FUKUBIKUKOTUKBN    PIC 9(01).
               05  TENSULST-TIMEMASUIKBN       PIC 9(01).
               05  TENSULST-DPCKBN             PIC 9(01).
               05  TENSULST-HISINSYUMONITORKSN PIC 9(01).
               05  TENSULST-TOUKETUHOZONKSN    PIC 9(01).
               05  TENSULST-KUBUNBANGOU        PIC X(30).
               05  TENSULST-ROSAIKBN           PIC 9(01).
               05  TENSULST-SISIKSN            PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "MONBLOB".
           COPY    "XMLDATA.INC".
           COPY    "ORCA-BLOB".
      *
           COPY    "CPORCSAPIFILE.INC".
      *
      *    PUSH
           COPY    "CPPUSHBATCH01.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
                                       CNT-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-BASE-YMD
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORAPI052BR2V3"     TO  JOB-PGID
           MOVE    "点数マスタ返却"    TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *      
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-PARA-BASE-YMD   TO  TNS-UPYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key35"             TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-N-SEC
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
      *    点数マスタ編集処理
           PERFORM     UNTIL   FLG-TENSU  =   1
               MOVE    ZERO                TO  CNT-OUT1
      *
               MOVE    SPACE               TO  TENSULST-REC
      *
               PERFORM         UNTIL   FLG-TENSU   =   1
                               OR      CNT-OUT1    >=  1000
                   PERFORM 2001-TENSULST-HENSYU-SEC
               END-PERFORM
      *
               ADD     1                   TO  CNT-XMLCOUNT
               PERFORM 910-XMLEXPORT-SEC
           END-PERFORM
      *
           MOVE    "tbl_tensu"        TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ編集処理
      *****************************************************************
       2001-TENSULST-HENSYU-SEC          SECTION.
      *
           ADD     1                   TO  CNT-OUT1
                                           CNT-OUT
           MOVE    CNT-OUT1            TO  IDX
      *
           INITIALIZE                      TENSULST-OCC (IDX)
      *
           MOVE    TNS-SRYCD           TO  TENSULST-SRYCD          (IDX)
           MOVE    TNS-YUKOSTYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-YUKOSTYMD      (IDX)
           MOVE    TNS-YUKOEDYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-YUKOEDYMD      (IDX)
           MOVE    TNS-SRYKBN          TO  TENSULST-SRYKBN         (IDX)
           MOVE    TNS-SRYSYUKBN       TO  TENSULST-SRYSYUKBN      (IDX)
           MOVE    TNS-YUKOKETA        TO  TENSULST-YUKOKETA       (IDX)
           MOVE    TNS-NAME            TO  TENSULST-NAME           (IDX)
           MOVE    TNS-KANAYUKOKETA    TO  TENSULST-KANAYUKOKETA   (IDX)
           MOVE    TNS-KANANAME        TO  TENSULST-KANANAME       (IDX)
           MOVE    TNS-FORMALYUKOKETA  TO  TENSULST-FORMALYUKOKETA (IDX)
           MOVE    TNS-FORMALNAME      TO  TENSULST-FORMALNAME     (IDX)
           MOVE    TNS-TENSIKIBETU     TO  TENSULST-TENSIKIBETU    (IDX)
           MOVE    TNS-TEN             TO  TENSULST-TEN            (IDX)
           MOVE    TNS-TANICD          TO  TENSULST-TANICD         (IDX)
           MOVE    TNS-TANIMOJISU      TO  TENSULST-TANIMOJISU     (IDX)
           MOVE    TNS-TANINAME        TO  TENSULST-TANINAME       (IDX)
           MOVE    TNS-DATAKBN         TO  TENSULST-DATAKBN        (IDX)
           MOVE    TNS-HKNTEKKBN       TO  TENSULST-HKNTEKKBN      (IDX)
           MOVE    TNS-NYUGAITEKKBN    TO  TENSULST-NYUGAITEKKBN   (IDX)
           MOVE    TNS-ROUTEKKBN       TO  TENSULST-ROUTEKKBN      (IDX)
           MOVE    TNS-HOSPSRYKBN      TO  TENSULST-HOSPSRYKBN     (IDX)
           MOVE    TNS-OSINKBN         TO  TENSULST-OSINKBN        (IDX)
           MOVE    TNS-HOUKNSKBN       TO  TENSULST-HOUKNSKBN      (IDX)
           MOVE    TNS-BYOKANRENKBN    TO  TENSULST-BYOKANRENKBN   (IDX)
           MOVE    TNS-SDOKNRYO        TO  TENSULST-SDOKNRYO       (IDX)
           MOVE    TNS-JITUDAY         TO  TENSULST-JITUDAY        (IDX)
           MOVE    TNS-DAYCNT          TO  TENSULST-DAYCNT         (IDX)
           MOVE    TNS-YKZKNRKBN       TO  TENSULST-YKZKNRKBN      (IDX)
           MOVE    TNS-KZMCOMPSIKIBETU TO  TENSULST-KZMCOMPSIKIBETU(IDX)
           MOVE    TNS-KZMKGN          TO  TENSULST-KZMKGN         (IDX)
           MOVE    TNS-KZMJGN          TO  TENSULST-KZMJGN         (IDX)
           MOVE    TNS-KZM             TO  TENSULST-KZM            (IDX)
           MOVE    TNS-KZMTEN          TO  TENSULST-KZMTEN         (IDX)
           MOVE    TNS-KZMERR          TO  TENSULST-KZMERR         (IDX)
           MOVE    TNS-JGNCNT          TO  TENSULST-JGNCNT         (IDX)
           MOVE    TNS-JGNCNT1D        TO  TENSULST-JGNCNT1D       (IDX)
           MOVE    TNS-JGNCNT1W        TO  TENSULST-JGNCNT1W       (IDX)
           MOVE    TNS-JGNCNTETCM      TO  TENSULST-JGNCNTETCM     (IDX)
           MOVE    TNS-JGNCNTETC       TO  TENSULST-JGNCNTETC      (IDX)
           MOVE    TNS-JGNCNTERR       TO  TENSULST-JGNCNTERR      (IDX)
           MOVE    TNS-CHUKSNCD        TO  TENSULST-CHUKSNCD       (IDX)
           MOVE    TNS-CHUKSNTUBAN     TO  TENSULST-CHUKSNTUBAN    (IDX)
           MOVE    TNS-TSUSOKUAGE      TO  TENSULST-TSUSOKUAGE     (IDX)
           MOVE    TNS-KGNAGE          TO  TENSULST-KGNAGE         (IDX)
           MOVE    TNS-JGNAGE          TO  TENSULST-JGNAGE         (IDX)
           MOVE    TNS-TIMEKSNKBN      TO  TENSULST-TIMEKSNKBN     (IDX)
           MOVE    TNS-KIJUNTEIGENKBN  TO  TENSULST-KIJUNTEIGENKBN (IDX)
           MOVE    TNS-KIJUNTEIGENCD   TO  TENSULST-KIJUNTEIGENCD  (IDX)
           MOVE    TNS-LASERKBN        TO  TENSULST-LASERKBN       (IDX)
           MOVE    TNS-CHPMESUKSN      TO  TENSULST-CHPMESUKSN     (IDX)
           MOVE    TNS-MICMESUKSN      TO  TENSULST-MICMESUKSN     (IDX)
           MOVE    TNS-DONORKBN        TO  TENSULST-DONORKBN       (IDX)
           MOVE    TNS-KNSJISKBN       TO  TENSULST-KNSJISKBN      (IDX)
           MOVE    TNS-KNSJISGRPKBN    TO  TENSULST-KNSJISGRPKBN   (IDX)
           MOVE    TNS-TEIGENKBN       TO  TENSULST-TEIGENKBN      (IDX)
           MOVE    TNS-SEKIZUISOKUTEI  TO  TENSULST-SEKIZUISOKUTEI (IDX)
           MOVE    TNS-KEIBUJYUTU      TO  TENSULST-KEIBUJYUTU     (IDX)
           MOVE    TNS-AUTOHOUGO       TO  TENSULST-AUTOHOUGO      (IDX)
           MOVE    TNS-GAIKANRIKBN     TO  TENSULST-GAIKANRIKBN    (IDX)
           MOVE    TNS-TUSOKUGAIKBN    TO  TENSULST-TUSOKUGAIKBN   (IDX)
           MOVE    TNS-HOUKATUTEIGENKBN
                                       TO  TENSULST-HOUKATUTEIGENKBN
                                                                   (IDX)
           MOVE    TNS-CHPNAISIKSN     TO  TENSULST-CHPNAISIKSN    (IDX)
           MOVE    TNS-AUTOFUNGO       TO  TENSULST-AUTOFUNGO      (IDX)
           MOVE    TNS-CHIIKIKSN       TO  TENSULST-CHIIKIKSN      (IDX)
           MOVE    TNS-BYOSYOKBN       TO  TENSULST-BYOSYOKBN      (IDX)
           MOVE    TNS-CHPGYOKOSOTIKSN TO  TENSULST-CHPGYOKOSOTIKSN(IDX)
           MOVE    TNS-SHORTSTAYOPE    TO  TENSULST-SHORTSTAYOPE   (IDX)
           MOVE    TNS-BUIKBN          TO  TENSULST-BUIKBN         (IDX)
           MOVE    TNS-SANTEIRRKKBN    TO  TENSULST-SANTEIRRKKBN   (IDX)
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       10
               MOVE    TNS-SSTKIJUNCD (IDY)
                                       TO  TENSULST-SSTKIJUNCD (IDX IDY)
           END-PERFORM
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       4
               MOVE    TNS-AGEKSNKGN (IDY)
                                       TO  TENSULST-AGEKSNKGN  (IDX IDY)
               MOVE    TNS-AGEKSNJGN (IDY)
                                       TO  TENSULST-AGEKSNJGN  (IDX IDY)
               MOVE    TNS-AGEKSNSRYCD (IDY)
                                       TO  TENSULST-AGEKSNSRYCD(IDX IDY)
           END-PERFORM
           MOVE    TNS-KENTAICOMMENT   TO  TENSULST-KENTAICOMMENT  (IDX)
           MOVE    TNS-NYUKHNKBN       TO  TENSULST-NYUKHNKBN      (IDX)
           MOVE    TNS-NYUKHNKSNKBN    TO  TENSULST-NYUKHNKSNKBN   (IDX)
           MOVE    TNS-KANGOKSN        TO  TENSULST-KANGOKSN       (IDX)
           MOVE    TNS-OLDTENSKB       TO  TENSULST-OLDTENSKB      (IDX)
           MOVE    TNS-OLDTEN          TO  TENSULST-OLDTEN         (IDX)
           MOVE    TNS-MADOKUKBN       TO  TENSULST-MADOKUKBN      (IDX)
           MOVE    TNS-SINKEIHAKAIKBN  TO  TENSULST-SINKEIHAKAIKBN (IDX)
           MOVE    TNS-SEIBUTUGAKUKBN  TO  TENSULST-SEIBUTUGAKUKBN (IDX)
           MOVE    TNS-ZOEIZAIKBN      TO  TENSULST-ZOEIZAIKBN     (IDX)
           MOVE    TNS-CSYYOURYO       TO  TENSULST-CSYYOURYO      (IDX)
           MOVE    TNS-YKZKBN          TO  TENSULST-YKZKBN         (IDX)
           MOVE    TNS-ZAIGATAKBN      TO  TENSULST-ZAIGATAKBN     (IDX)
           MOVE    TNS-KOUHATUKBN      TO  TENSULST-KOUHATUKBN     (IDX)
           MOVE    TNS-LONGTOYOKBN     TO  TENSULST-LONGTOYOKBN    (IDX)
           MOVE    TNS-MEIUSESKB       TO  TENSULST-MEIUSESKB      (IDX)
           MOVE    TNS-TOKUKIZAIAGEKSNKBN
                                       TO  TENSULST-TOKUKIZAIAGEKSNKBN
                                                                   (IDX)
           MOVE    TNS-SANSOKBN        TO  TENSULST-SANSOKBN       (IDX)
           MOVE    TNS-TOKUKIZAISBT1   TO  TENSULST-TOKUKIZAISBT1  (IDX)
           MOVE    TNS-TOKUKIZAISBT2   TO  TENSULST-TOKUKIZAISBT2  (IDX)
           MOVE    TNS-JGNTEN          TO  TENSULST-JGNTEN         (IDX)
           MOVE    TNS-GAITENTTLKBN    TO  TENSULST-GAITENTTLKBN   (IDX)
           MOVE    TNS-NYUTENTTLKBN    TO  TENSULST-NYUTENTTLKBN   (IDX)
           MOVE    TNS-CDKBN-KBN       TO  TENSULST-CDKBN-KBN      (IDX)
           MOVE    TNS-CDKBN-SYO       TO  TENSULST-CDKBN-SYO      (IDX)
           MOVE    TNS-CDKBN-BU        TO  TENSULST-CDKBN-BU       (IDX)
           MOVE    TNS-CDKBN-KBNNUM    TO  TENSULST-CDKBN-KBNNUM   (IDX)
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  TENSULST-CDKBN-KBNNUM-EDA
                                                                   (IDX)
           MOVE    TNS-CDKBN-KOUBAN    TO  TENSULST-CDKBN-KOUBAN   (IDX)
           MOVE    TNS-KOKUCHI-KBN     TO  TENSULST-KOKUCHI-KBN    (IDX)
           MOVE    TNS-KOKUCHI-SYO     TO  TENSULST-KOKUCHI-SYO    (IDX)
           MOVE    TNS-KOKUCHI-BU      TO  TENSULST-KOKUCHI-BU     (IDX)
           MOVE    TNS-KOKUCHI-KBNNUM  TO  TENSULST-KOKUCHI-KBNNUM (IDX)
           MOVE    TNS-KOKUCHI-KBNNUM-EDA
                                       TO  TENSULST-KOKUCHI-KBNNUM-EDA
                                                                   (IDX)
           MOVE    TNS-KOKUCHI-KOUBAN  TO  TENSULST-KOKUCHI-KOUBAN (IDX)
           MOVE    TNS-KOKUJISKBKBN1   TO  TENSULST-KOKUJISKBKBN1  (IDX)
           MOVE    TNS-KOKUJISKBKBN2   TO  TENSULST-KOKUJISKBKBN2  (IDX)
           MOVE    TNS-NAMECHGKBN      TO  TENSULST-NAMECHGKBN     (IDX)
           MOVE    TNS-KANANAMECHGKBN  TO  TENSULST-KANANAMECHGKBN (IDX)
           MOVE    TNS-IDOKANREN       TO  TENSULST-IDOKANREN      (IDX)
           MOVE    TNS-KOUHYOJYUNNUM   TO  TENSULST-KOUHYOJYUNNUM  (IDX)
           MOVE    TNS-YAKKAKJNCD      TO  TENSULST-YAKKAKJNCD     (IDX)
           MOVE    TNS-SYUSAISKB       TO  TENSULST-SYUSAISKB      (IDX)
           MOVE    TNS-SYOMEIKANREN    TO  TENSULST-SYOMEIKANREN   (IDX)
           MOVE    TNS-CHGYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-CHGYMD         (IDX)
           MOVE    TNS-HAISIYMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-HAISIYMD       (IDX)
           MOVE    TNS-KEIKASOCHIYMD   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-KEIKASOCHIYMD  (IDX)
      ***  MOVE    TNS-TERMID          TO  TENSULST-TERMID         (IDX)
      ***  MOVE    TNS-OPID            TO  TENSULST-OPID           (IDX)
           MOVE    TNS-CREYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-CREYMD         (IDX)
           MOVE    TNS-UPYMD           TO  WRK-SYMD
           MOVE    TNS-UPHMS           TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-UPYMD          (IDX)
           MOVE    WRK-HEN-TIME        TO  TENSULST-UPHMS          (IDX)
      *****MOVE    TNS-MASTER-CLASS    TO  TENSULST-MASTER-CLASS   (IDX)
           MOVE    TNS-GAZOOPESUP      TO  TENSULST-GAZOOPESUP     (IDX)
           MOVE    TNS-IRYOKANSATUKBN  TO  TENSULST-IRYOKANSATUKBN (IDX)
           MOVE    TNS-MASUISKBKBN     TO  TENSULST-MASUISKBKBN    (IDX)
           MOVE    TNS-FUKUBIKUNAIKBN  TO  TENSULST-FUKUBIKUNAIKBN (IDX)
           MOVE    TNS-FUKUBIKUKOTUKBN TO  TENSULST-FUKUBIKUKOTUKBN(IDX)
           MOVE    TNS-TIMEMASUIKBN    TO  TENSULST-TIMEMASUIKBN   (IDX)
           MOVE    TNS-DPCKBN          TO  TENSULST-DPCKBN         (IDX)
           MOVE    TNS-HISINSYUMONITORKSN
                                       TO  TENSULST-HISINSYUMONITORKSN
                                                                   (IDX)
           MOVE    TNS-TOUKETUHOZONKSN TO  TENSULST-TOUKETUHOZONKSN(IDX)
           MOVE    TNS-KUBUNBANGOU     TO  TENSULST-KUBUNBANGOU    (IDX)
           MOVE    TNS-ROSAIKBN        TO  TENSULST-ROSAIKBN       (IDX)
           MOVE    TNS-SISIKSN         TO  TENSULST-SISIKSN        (IDX)
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 900-TENSU-READ-N-SEC
           .
       2001-TENSULST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    EXPORT 処理
      *****************************************************************
       910-XMLEXPORT-SEC           SECTION.
      *
           INITIALIZE                  XMLDATA-REC
           MOVE   TENSULST-REC     TO  XMLDATA-DATA
      *
           MOVE    1               TO  XMLDATA-MODE
           MOVE    "xml_medication_masterlst"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "XMLOPEN"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XMLDATA-REC
      *
           MOVE    "medication_masterlst"
                                   TO  XMLDATA-RECORDNAME
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XMLDATA-REC
      *
           INITIALIZE                   ORCA-BLOB
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    XMLDATA-OBJECT  TO  ORCA-BLOB-OBJECT
           INITIALIZE                  SGETTEMP-AREA
           STRING  "medication_masterlst"
                                   DELIMITED   BY  SIZE
                   CNT-XMLCOUNT    DELIMITED   BY  SIZE
                   ".xml"          DELIMITED   BY  SIZE
                                   INTO        SGETTEMP-BASENAMES  (1)
           END-STRING
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ORCA-BLOB-FILE
           MOVE    "BLOBEXPORT"    TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       ORCA-BLOB
           .
       910-XMLEXPORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   SPACE   OR  "00000000"
               MOVE    "1900"              TO  WRK-HEN-YY
               MOVE    "01"                TO  WRK-HEN-MM
               MOVE    "01"                TO  WRK-HEN-DD
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           IF      WRK-THMS (1:3)      =   SPACE   OR  "000"
               MOVE    "00"                TO  WRK-HEN-THH
                                               WRK-HEN-TMM
                                               WRK-HEN-TSS
           ELSE
               MOVE    WRK-THH             TO  WRK-HEN-THH
               MOVE    WRK-TMM             TO  WRK-HEN-TMM
               MOVE    WRK-TSS             TO  WRK-HEN-TSS
           END-IF
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-OUT         =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
      *
      *        ＰＵＳＨ通知処理
               PERFORM 3001-PUSH-SEC
           ELSE
               INITIALIZE                 ORCSAPIFILEAREA
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-IN-DIRNAME
               MOVE    "medication_masterlst"
                                       TO  SAPIFILE-IN-FILENAME
               MOVE    "xml"           TO  SAPIFILE-IN-FILE-TYPE
               move    "2"             TO  SAPIFILE-IN-KBN
               MOVE    1               TO  SAPIFILE-IN-RENNUM
               MOVE    CNT-XMLCOUNT    TO  SAPIFILE-IN-COUNT
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-OT-DIRNAME
               MOVE    "medication_masterlst.xml"
                                       TO  SAPIFILE-OT-FILENAME
               CALL    "ORCSAPIFILE"   USING   ORCSAPIFILEAREA
      *
               IF      SAPIFILE-RC     =   0
      *            ＰＵＳＨ通知処理
                   PERFORM 3001-PUSH-SEC
               ELSE
                   STRING   "ｘｍｌファイル作成エラー RC="
                                           DELIMITED   BY  SIZE
                            SAPIFILE-RC    DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORAPI052BR2V3 OUT:" CNT-OUT
           DISPLAY "*** ORAPI052BR2V3 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＳＨ通知処理
      *****************************************************************
       3001-PUSH-SEC        SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN
                                   =   ZERO  )
           AND   ( JOB-PARAM   NOT =   SPACE )
               INITIALIZE                  PUSHBA01-REC
               MOVE    JOB-PARAM       TO  WRK-PARAM
               MOVE    WRK-PARAM-KARTE-UID
                                       TO  PUSHBA01API-KARTE-UID
               MOVE    WRK-PARAM-ORCA-UID
                                       TO  PUSHBA01API-ORCA-UID
               MOVE    "000"           TO  PUSHBA01API-RESULT
               MOVE    "medicalpoints_master_make"
                                       TO  PUSHBA01-EVENT
               MOVE    WRK-PARA-BASE-YMD
                                       TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  PUSHBA01-PERFORM-DATE
               MOVE    CNT-OUT         TO  WRK-CNT
               MOVE    WRK-CNT-X       TO  PUSHBA01-OUTPUT-COUNT
      *??
               display "push=" PUSHBA01-REC "#"
               MOVE    PUSHBA01-REC    TO  MCPDATA-REC
               MOVE    "PUSHEVENT"     TO  MCP-FUNC
               MOVE    "push_batch01"  TO  MCP-TABLE
               MOVE    "push_batch01"  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
           .
       3001-PUSH-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
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
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-N-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-TENSU
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
              MOVE    1               TO  FLG-TENSU
           END-IF
           .
       900-TENSU-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
