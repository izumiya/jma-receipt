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
      *
      *    点数データ
           SELECT  TENSULST-FILE   ASSIGN  TENSULSTPARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TENSULST.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    点数データ
       FD  TENSULST-FILE.
       01  TENSULST-REC.
           03  TENSULST-SRYCD              PIC X(09).
           03  TENSULST-YUKOSTYMD          PIC X(10).
           03  TENSULST-YUKOEDYMD          PIC X(10).
           03  TENSULST-SRYKBN             PIC X(02).
           03  TENSULST-SRYSYUKBN          PIC X(03).
           03  TENSULST-YUKOKETA           PIC 9(02).
           03  TENSULST-NAME               PIC X(200).
           03  TENSULST-KANAYUKOKETA       PIC 9(02).
           03  TENSULST-KANANAME           PIC X(200).
           03  TENSULST-FORMALYUKOKETA     PIC 9(03).
           03  TENSULST-FORMALNAME         PIC X(400).
           03  TENSULST-TENSIKIBETU        PIC 9(01).
           03  TENSULST-TEN                PIC 9(09)V99.
           03  TENSULST-TANICD             PIC X(03).
           03  TENSULST-TANIMOJISU         PIC 9(01).
           03  TENSULST-TANINAME           PIC X(24).
           03  TENSULST-DATAKBN            PIC 9(01).
           03  TENSULST-HKNTEKKBN          PIC 9(01).
           03  TENSULST-NYUGAITEKKBN       PIC 9(01).
           03  TENSULST-ROUTEKKBN          PIC 9(01).
           03  TENSULST-HOSPSRYKBN         PIC 9(01).
           03  TENSULST-OSINKBN            PIC 9(01).
           03  TENSULST-HOUKNSKBN          PIC 9(02).
           03  TENSULST-BYOKANRENKBN       PIC 9(02).
           03  TENSULST-SDOKNRYO           PIC 9(02).
           03  TENSULST-JITUDAY            PIC 9(01).
           03  TENSULST-DAYCNT             PIC 9(01).
           03  TENSULST-YKZKNRKBN          PIC 9(01).
           03  TENSULST-KZMCOMPSIKIBETU    PIC 9(01).
           03  TENSULST-KZMKGN             PIC 9(08).
           03  TENSULST-KZMJGN             PIC 9(08).
           03  TENSULST-KZM                PIC 9(08).
           03  TENSULST-KZMTEN             PIC 9(07)V99.
           03  TENSULST-KZMERR             PIC 9(01).
           03  TENSULST-JGNCNT             PIC 9(03).
           03  TENSULST-JGNCNT1D           PIC 9(03).
           03  TENSULST-JGNCNT1W           PIC 9(03).
           03  TENSULST-JGNCNTETCM         PIC 9(02).
           03  TENSULST-JGNCNTETC          PIC 9(03).
           03  TENSULST-JGNCNTERR          PIC 9(01).
           03  TENSULST-CHUKSNCD           PIC X(04).
           03  TENSULST-CHUKSNTUBAN        PIC X(01).
           03  TENSULST-TSUSOKUAGE         PIC 9(01).
           03  TENSULST-KGNAGE             PIC X(02).
           03  TENSULST-JGNAGE             PIC X(02).
           03  TENSULST-TIMEKSNKBN         PIC 9(01).
           03  TENSULST-KIJUNTEIGENKBN     PIC 9(01).
           03  TENSULST-KIJUNTEIGENCD      PIC 9(04).
           03  TENSULST-LASERKBN           PIC 9(01).
           03  TENSULST-CHPMESUKSN         PIC 9(01).
           03  TENSULST-MICMESUKSN         PIC 9(01).
           03  TENSULST-DONORKBN           PIC 9(01).
           03  TENSULST-KNSJISKBN          PIC 9(01).
           03  TENSULST-KNSJISGRPKBN       PIC 9(02).
           03  TENSULST-TEIGENKBN          PIC 9(01).
           03  TENSULST-SEKIZUISOKUTEI     PIC 9(01).
           03  TENSULST-KEIBUJYUTU         PIC 9(01).
           03  TENSULST-AUTOHOUGO          PIC 9(01).
           03  TENSULST-GAIKANRIKBN        PIC 9(01).
           03  TENSULST-TUSOKUGAIKBN       PIC 9(01).
           03  TENSULST-HOUKATUTEIGENKBN   PIC 9(03).
           03  TENSULST-CHPNAISIKSN        PIC 9(01).
           03  TENSULST-AUTOFUNGO          PIC 9(01).
           03  TENSULST-CHIIKIKSN          PIC 9(01).
           03  TENSULST-BYOSYOKBN          PIC 9(01).
           03  TENSULST-CHPGYOKOSOTIKSN    PIC 9(01).
           03  TENSULST-SHORTSTAYOPE       PIC 9(01).
           03  TENSULST-BUIKBN             PIC 9(01).
           03  TENSULST-SANTEIRRKKBN       PIC 9(01).
           03  TENSULST-SSTKIJUNCDTBL      OCCURS 10.
               05  TENSULST-SSTKIJUNCD         PIC 9(04).
           03  TENSULST-AGEKSNTBL          OCCURS 4.
               05  TENSULST-AGEKSNKGN          PIC X(02).
               05  TENSULST-AGEKSNJGN          PIC X(02).
               05  TENSULST-AGEKSNSRYCD        PIC X(09).
           03  TENSULST-KENTAICOMMENT      PIC 9(01).
           03  TENSULST-NYUKHNKBN          PIC 9(02).
           03  TENSULST-NYUKHNKSNKBN       PIC 9(03).
           03  TENSULST-KANGOKSN           PIC 9(02).
           03  TENSULST-GAZOOPESUP         PIC 9(01).
           03  TENSULST-IRYOKANSATUKBN     PIC 9(01).
           03  TENSULST-MASUISKBKBN        PIC 9(01).
           03  TENSULST-FUKUBIKUNAIKBN     PIC 9(01).
           03  TENSULST-FUKUBIKUKOTUKBN    PIC 9(01).
           03  TENSULST-TIMEMASUIKBN       PIC 9(01).
           03  TENSULST-DPCKBN             PIC 9(01).
           03  TENSULST-HISINSYUMONITORKSN PIC 9(01).
           03  TENSULST-TOUKETUHOZONKSN    PIC 9(01).
           03  TENSULST-KUBUNBANGOU        PIC X(30).
           03  TENSULST-ROSAIKBN           PIC 9(01).
           03  TENSULST-SISIKSN            PIC 9(01).
           03  TENSULST-OLDTENSKB          PIC 9(01).
           03  TENSULST-OLDTEN             PIC 9(09)V99.
           03  TENSULST-MADOKUKBN          PIC 9(01).
           03  TENSULST-SINKEIHAKAIKBN     PIC 9(01).
           03  TENSULST-SEIBUTUGAKUKBN     PIC 9(01).
           03  TENSULST-ZOEIZAIKBN         PIC 9(01).
           03  TENSULST-CSYYOURYO          PIC 9(05).
           03  TENSULST-YKZKBN             PIC 9(01).
           03  TENSULST-ZAIGATAKBN         PIC 9(01).
           03  TENSULST-KOUHATUKBN         PIC 9(01).
           03  TENSULST-LONGTOYOKBN        PIC 9(01).
           03  TENSULST-MEIUSESKB          PIC 9(01).
           03  TENSULST-TOKUKIZAIAGEKSNKBN PIC 9(01).
           03  TENSULST-SANSOKBN           PIC 9(01).
           03  TENSULST-TOKUKIZAISBT1      PIC 9(01).
           03  TENSULST-TOKUKIZAISBT2      PIC 9(01).
           03  TENSULST-JGNTEN             PIC 9(07).
           03  TENSULST-GAITENTTLKBN       PIC 9(03).
           03  TENSULST-NYUTENTTLKBN       PIC 9(03).
           03  TENSULST-CDKBN-KBN          PIC X(01).
           03  TENSULST-CDKBN-SYO          PIC 9(01).
           03  TENSULST-CDKBN-BU           PIC 9(02).
           03  TENSULST-CDKBN-KBNNUM       PIC 9(03).
           03  TENSULST-CDKBN-KBNNUM-EDA   PIC 9(02).
           03  TENSULST-CDKBN-KOUBAN       PIC 9(02).
           03  TENSULST-KOKUCHI-KBN        PIC X(01).
           03  TENSULST-KOKUCHI-SYO        PIC 9(01).
           03  TENSULST-KOKUCHI-BU         PIC 9(02).
           03  TENSULST-KOKUCHI-KBNNUM     PIC 9(03).
           03  TENSULST-KOKUCHI-KBNNUM-EDA PIC 9(02).
           03  TENSULST-KOKUCHI-KOUBAN     PIC 9(02).
           03  TENSULST-KOKUJISKBKBN1      PIC 9(01).
           03  TENSULST-KOKUJISKBKBN2      PIC 9(01).
           03  TENSULST-NAMECHGKBN         PIC 9(01).
           03  TENSULST-KANANAMECHGKBN     PIC 9(01).
           03  TENSULST-IDOKANREN          PIC X(09).
           03  TENSULST-KOUHYOJYUNNUM      PIC 9(09).
           03  TENSULST-YAKKAKJNCD         PIC X(12).
           03  TENSULST-SYUSAISKB          PIC 9(01).
           03  TENSULST-SYOMEIKANREN       PIC X(09).
           03  TENSULST-CHGYMD             PIC X(10).
           03  TENSULST-HAISIYMD           PIC X(10).
           03  TENSULST-KEIKASOCHIYMD      PIC X(10).
      *****03  TENSULST-TERMID             PIC X(16).
      *****03  TENSULST-OPID               PIC X(16).
           03  TENSULST-CREYMD             PIC X(10).
           03  TENSULST-UPYMD              PIC X(10).
           03  TENSULST-UPHMS              PIC X(08).
           03  TENSULST-MASTER-CLASS       PIC 9(01).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TENSULSTPARA//.
      *
      *    一時ファイル対応
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TENSULST        PIC X(02).
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
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
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
           MOVE    "TENSU001"          TO  TENSULSTPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  TENSULSTPARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  TENSULSTPARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    TENSULSTPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  TENSULSTPARA-FULLNAME
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORAPI052BR2V3"     TO  JOB-PGID
           MOVE    "点数マスタ返却"    TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    ファイルオープン
           OPEN    OUTPUT              TENSULST-FILE
           IF      STS-TENSULST    =   "00"
                CONTINUE
            ELSE
                MOVE   SPACE           TO  WRK-RECEERR
                STRING "点数データＯＰＥＮエラー　STS="
                                           DELIMITED  BY  SIZE
                       STS-TENSULST        DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
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
           PERFORM     UNTIL   FLG-TENSU  =   1
      *        点数マスタ編集処理
               PERFORM 2001-TENSULST-HENSYU-SEC
      *
               MOVE    "tbl_tensu"        TO  MCP-TABLE
               MOVE    "key35"            TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-N-SEC
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
           MOVE    SPACE               TO  TENSULST-REC
           INITIALIZE                      TENSULST-REC
      *
           MOVE    TNS-SRYCD           TO  TENSULST-SRYCD
           MOVE    TNS-YUKOSTYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-YUKOEDYMD
           MOVE    TNS-SRYKBN          TO  TENSULST-SRYKBN
           MOVE    TNS-SRYSYUKBN       TO  TENSULST-SRYSYUKBN
           MOVE    TNS-YUKOKETA        TO  TENSULST-YUKOKETA
           MOVE    TNS-NAME            TO  TENSULST-NAME
           MOVE    TNS-KANAYUKOKETA    TO  TENSULST-KANAYUKOKETA
           MOVE    TNS-KANANAME        TO  TENSULST-KANANAME
           MOVE    TNS-FORMALYUKOKETA  TO  TENSULST-FORMALYUKOKETA
           MOVE    TNS-FORMALNAME      TO  TENSULST-FORMALNAME
           MOVE    TNS-TENSIKIBETU     TO  TENSULST-TENSIKIBETU
           MOVE    TNS-TEN             TO  TENSULST-TEN
           MOVE    TNS-TANICD          TO  TENSULST-TANICD
           MOVE    TNS-TANIMOJISU      TO  TENSULST-TANIMOJISU
           MOVE    TNS-TANINAME        TO  TENSULST-TANINAME
           MOVE    TNS-DATAKBN         TO  TENSULST-DATAKBN
           MOVE    TNS-HKNTEKKBN       TO  TENSULST-HKNTEKKBN
           MOVE    TNS-NYUGAITEKKBN    TO  TENSULST-NYUGAITEKKBN
           MOVE    TNS-ROUTEKKBN       TO  TENSULST-ROUTEKKBN
           MOVE    TNS-HOSPSRYKBN      TO  TENSULST-HOSPSRYKBN
           MOVE    TNS-OSINKBN         TO  TENSULST-OSINKBN
           MOVE    TNS-HOUKNSKBN       TO  TENSULST-HOUKNSKBN
           MOVE    TNS-BYOKANRENKBN    TO  TENSULST-BYOKANRENKBN
           MOVE    TNS-SDOKNRYO        TO  TENSULST-SDOKNRYO
           MOVE    TNS-JITUDAY         TO  TENSULST-JITUDAY
           MOVE    TNS-DAYCNT          TO  TENSULST-DAYCNT
           MOVE    TNS-YKZKNRKBN       TO  TENSULST-YKZKNRKBN
           MOVE    TNS-KZMCOMPSIKIBETU TO  TENSULST-KZMCOMPSIKIBETU
           MOVE    TNS-KZMKGN          TO  TENSULST-KZMKGN
           MOVE    TNS-KZMJGN          TO  TENSULST-KZMJGN
           MOVE    TNS-KZM             TO  TENSULST-KZM
           MOVE    TNS-KZMTEN          TO  TENSULST-KZMTEN
           MOVE    TNS-KZMERR          TO  TENSULST-KZMERR
           MOVE    TNS-JGNCNT          TO  TENSULST-JGNCNT
           MOVE    TNS-JGNCNT1D        TO  TENSULST-JGNCNT1D
           MOVE    TNS-JGNCNT1W        TO  TENSULST-JGNCNT1W
           MOVE    TNS-JGNCNTETCM      TO  TENSULST-JGNCNTETCM
           MOVE    TNS-JGNCNTETC       TO  TENSULST-JGNCNTETC
           MOVE    TNS-JGNCNTERR       TO  TENSULST-JGNCNTERR
           MOVE    TNS-CHUKSNCD        TO  TENSULST-CHUKSNCD
           MOVE    TNS-CHUKSNTUBAN     TO  TENSULST-CHUKSNTUBAN
           MOVE    TNS-TSUSOKUAGE      TO  TENSULST-TSUSOKUAGE
           MOVE    TNS-KGNAGE          TO  TENSULST-KGNAGE
           MOVE    TNS-JGNAGE          TO  TENSULST-JGNAGE
           MOVE    TNS-TIMEKSNKBN      TO  TENSULST-TIMEKSNKBN
           MOVE    TNS-KIJUNTEIGENKBN  TO  TENSULST-KIJUNTEIGENKBN
           MOVE    TNS-KIJUNTEIGENCD   TO  TENSULST-KIJUNTEIGENCD
           MOVE    TNS-LASERKBN        TO  TENSULST-LASERKBN
           MOVE    TNS-CHPMESUKSN      TO  TENSULST-CHPMESUKSN
           MOVE    TNS-MICMESUKSN      TO  TENSULST-MICMESUKSN
           MOVE    TNS-DONORKBN        TO  TENSULST-DONORKBN
           MOVE    TNS-KNSJISKBN       TO  TENSULST-KNSJISKBN
           MOVE    TNS-KNSJISGRPKBN    TO  TENSULST-KNSJISGRPKBN
           MOVE    TNS-TEIGENKBN       TO  TENSULST-TEIGENKBN
           MOVE    TNS-SEKIZUISOKUTEI  TO  TENSULST-SEKIZUISOKUTEI
           MOVE    TNS-KEIBUJYUTU      TO  TENSULST-KEIBUJYUTU
           MOVE    TNS-AUTOHOUGO       TO  TENSULST-AUTOHOUGO
           MOVE    TNS-GAIKANRIKBN     TO  TENSULST-GAIKANRIKBN
           MOVE    TNS-TUSOKUGAIKBN    TO  TENSULST-TUSOKUGAIKBN
           MOVE    TNS-HOUKATUTEIGENKBN
                                       TO  TENSULST-HOUKATUTEIGENKBN
           MOVE    TNS-CHPNAISIKSN     TO  TENSULST-CHPNAISIKSN
           MOVE    TNS-AUTOFUNGO       TO  TENSULST-AUTOFUNGO
           MOVE    TNS-CHIIKIKSN       TO  TENSULST-CHIIKIKSN
           MOVE    TNS-BYOSYOKBN       TO  TENSULST-BYOSYOKBN
           MOVE    TNS-CHPGYOKOSOTIKSN TO  TENSULST-CHPGYOKOSOTIKSN
           MOVE    TNS-SHORTSTAYOPE    TO  TENSULST-SHORTSTAYOPE
           MOVE    TNS-BUIKBN          TO  TENSULST-BUIKBN
           MOVE    TNS-SANTEIRRKKBN    TO  TENSULST-SANTEIRRKKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               MOVE    TNS-SSTKIJUNCD (IDX)
                                           TO  TENSULST-SSTKIJUNCD (IDX)
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               MOVE    TNS-AGEKSNKGN (IDX) TO  TENSULST-AGEKSNKGN  (IDX)
               MOVE    TNS-AGEKSNJGN (IDX) TO  TENSULST-AGEKSNJGN  (IDX)
               MOVE    TNS-AGEKSNSRYCD (IDX)
                                           TO  TENSULST-AGEKSNSRYCD(IDX)
           END-PERFORM
           MOVE    TNS-KENTAICOMMENT   TO  TENSULST-KENTAICOMMENT
           MOVE    TNS-NYUKHNKBN       TO  TENSULST-NYUKHNKBN
           MOVE    TNS-NYUKHNKSNKBN    TO  TENSULST-NYUKHNKSNKBN
           MOVE    TNS-KANGOKSN        TO  TENSULST-KANGOKSN
           MOVE    TNS-GAZOOPESUP      TO  TENSULST-GAZOOPESUP
           MOVE    TNS-IRYOKANSATUKBN  TO  TENSULST-IRYOKANSATUKBN
           MOVE    TNS-MASUISKBKBN     TO  TENSULST-MASUISKBKBN
           MOVE    TNS-FUKUBIKUNAIKBN  TO  TENSULST-FUKUBIKUNAIKBN
           MOVE    TNS-FUKUBIKUKOTUKBN TO  TENSULST-FUKUBIKUKOTUKBN
           MOVE    TNS-TIMEMASUIKBN    TO  TENSULST-TIMEMASUIKBN
           MOVE    TNS-DPCKBN          TO  TENSULST-DPCKBN
           MOVE    TNS-HISINSYUMONITORKSN
                                       TO  TENSULST-HISINSYUMONITORKSN
           MOVE    TNS-TOUKETUHOZONKSN TO  TENSULST-TOUKETUHOZONKSN
           MOVE    TNS-KUBUNBANGOU     TO  TENSULST-KUBUNBANGOU
           MOVE    TNS-ROSAIKBN        TO  TENSULST-ROSAIKBN
           MOVE    TNS-SISIKSN         TO  TENSULST-SISIKSN 
           MOVE    TNS-OLDTENSKB       TO  TENSULST-OLDTENSKB
           MOVE    TNS-OLDTEN          TO  TENSULST-OLDTEN 
           MOVE    TNS-MADOKUKBN       TO  TENSULST-MADOKUKBN
           MOVE    TNS-SINKEIHAKAIKBN  TO  TENSULST-SINKEIHAKAIKBN
           MOVE    TNS-SEIBUTUGAKUKBN  TO  TENSULST-SEIBUTUGAKUKBN
           MOVE    TNS-ZOEIZAIKBN      TO  TENSULST-ZOEIZAIKBN 
           MOVE    TNS-CSYYOURYO       TO  TENSULST-CSYYOURYO
           MOVE    TNS-YKZKBN          TO  TENSULST-YKZKBN
           MOVE    TNS-ZAIGATAKBN      TO  TENSULST-ZAIGATAKBN
           MOVE    TNS-KOUHATUKBN      TO  TENSULST-KOUHATUKBN
           MOVE    TNS-LONGTOYOKBN     TO  TENSULST-LONGTOYOKBN
           MOVE    TNS-MEIUSESKB       TO  TENSULST-MEIUSESKB
           MOVE    TNS-TOKUKIZAIAGEKSNKBN
                                       TO  TENSULST-TOKUKIZAIAGEKSNKBN
           MOVE    TNS-SANSOKBN        TO  TENSULST-SANSOKBN
           MOVE    TNS-TOKUKIZAISBT1   TO  TENSULST-TOKUKIZAISBT1
           MOVE    TNS-TOKUKIZAISBT2   TO  TENSULST-TOKUKIZAISBT2
           MOVE    TNS-JGNTEN          TO  TENSULST-JGNTEN
           MOVE    TNS-GAITENTTLKBN    TO  TENSULST-GAITENTTLKBN
           MOVE    TNS-NYUTENTTLKBN    TO  TENSULST-NYUTENTTLKBN
           MOVE    TNS-CDKBN-KBN       TO  TENSULST-CDKBN-KBN
           MOVE    TNS-CDKBN-SYO       TO  TENSULST-CDKBN-SYO
           MOVE    TNS-CDKBN-BU        TO  TENSULST-CDKBN-BU
           MOVE    TNS-CDKBN-KBNNUM    TO  TENSULST-CDKBN-KBNNUM
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  TENSULST-CDKBN-KBNNUM-EDA
           MOVE    TNS-CDKBN-KOUBAN    TO  TENSULST-CDKBN-KOUBAN
           MOVE    TNS-KOKUCHI-KBN     TO  TENSULST-KOKUCHI-KBN
           MOVE    TNS-KOKUCHI-SYO     TO  TENSULST-KOKUCHI-SYO
           MOVE    TNS-KOKUCHI-BU      TO  TENSULST-KOKUCHI-BU
           MOVE    TNS-KOKUCHI-KBNNUM  TO  TENSULST-KOKUCHI-KBNNUM
           MOVE    TNS-KOKUCHI-KBNNUM-EDA
                                       TO  TENSULST-KOKUCHI-KBNNUM-EDA
           MOVE    TNS-KOKUCHI-KOUBAN  TO  TENSULST-KOKUCHI-KOUBAN
           MOVE    TNS-KOKUJISKBKBN1   TO  TENSULST-KOKUJISKBKBN1
           MOVE    TNS-KOKUJISKBKBN2   TO  TENSULST-KOKUJISKBKBN2
           MOVE    TNS-NAMECHGKBN      TO  TENSULST-NAMECHGKBN
           MOVE    TNS-KANANAMECHGKBN  TO  TENSULST-KANANAMECHGKBN
           MOVE    TNS-IDOKANREN       TO  TENSULST-IDOKANREN
           MOVE    TNS-KOUHYOJYUNNUM   TO  TENSULST-KOUHYOJYUNNUM
           MOVE    TNS-YAKKAKJNCD      TO  TENSULST-YAKKAKJNCD
           MOVE    TNS-SYUSAISKB       TO  TENSULST-SYUSAISKB
           MOVE    TNS-SYOMEIKANREN    TO  TENSULST-SYOMEIKANREN
           MOVE    TNS-CHGYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-CHGYMD
           MOVE    TNS-HAISIYMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-HAISIYMD
           MOVE    TNS-KEIKASOCHIYMD   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-KEIKASOCHIYMD
      ***  MOVE    TNS-TERMID          TO  TENSULST-TERMID
      ***  MOVE    TNS-OPID            TO  TENSULST-OPID
           MOVE    TNS-CREYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-CREYMD
           MOVE    TNS-UPYMD           TO  WRK-SYMD
           MOVE    TNS-UPHMS           TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-UPYMD
           MOVE    WRK-HEN-TIME        TO  TENSULST-UPHMS
      *****MOVE    TNS-MASTER-CLASS    TO  TENSULST-MASTER-CLASS
           WRITE       TENSULST-REC
      *
           IF      STS-TENSULST       =   "00"
                ADD    1                   TO  CNT-OUT
            ELSE
                MOVE   SPACE               TO  WRK-RECEERR
                STRING "点数データ書き込みエラー　STS="
                                               DELIMITED  BY  SIZE
                       STS-TENSULST            DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       2001-TENSULST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
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
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
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
           CLOSE   TENSULST-FILE
      *
      **   IF      CNT-OUT         =   ZERO
      **       MOVE    "処理対象のデータがありませんでした"
      **                               TO  WRK-RECEERR
      **       PERFORM 500-ERR-HENSYU-SEC
      **   END-IF
      *
      *    ＰＵＳＨ通知処理
           PERFORM 3001-PUSH-SEC
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
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORAPI052BR2V3 "    DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
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
           MOVE    "tbl_tensu"        TO  MCP-TABLE
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
