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
       PROGRAM-ID.             ORCHXLST.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェクマスタ
      *  コンポーネント名  : チェクマスタ一覧表印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/04/26    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    04/03/01  一括印刷対応
      *  01.00.02    NACL-太田    05/02/04  フォーム変更
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    明細情報（チェック２診療行為）
           SELECT  B01L-FILE       ASSIGN      ASS01
                                   ORGANIZATION    LINE  SEQUENTIAL
                                   FILE    STATUS  IS    STS-ASS01.
      *    明細情報（チェック２病名）
           SELECT  B01R-FILE       ASSIGN      ASS02
                                   ORGANIZATION    LINE  SEQUENTIAL
                                   FILE    STATUS  IS    STS-ASS02.
      *    明細情報（チェック２対象）
           SELECT  B01T-FILE       ASSIGN      ASS03
                                   ORGANIZATION    LINE  SEQUENTIAL
                                   FILE    STATUS  IS    STS-ASS03.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  B01L-FILE.
       01  B01L-REC.
           03  B01L-KEY.
               05  B01L-MSTSEQ         PIC 9(10).
           03  B01L-NYUGAIKBN          PIC X(01).
           03  B01L-TEKIYO-G.
               05 B01L-NYUGAIKBNMEI    PIC X(02).
               05 B01L-SRYCD           PIC X(12).
               05 B01L-FIL1            PIC X(01).
               05 B01L-NAME            PIC X(46).
               05 B01L-FIL2            PIC X(02).
      *
       FD  B01R-FILE.
       01  B01R-REC.
           03  B01R-KEY.
               05  B01R-MSTSEQ         PIC 9(10).
           03  B01R-UTAGAIKBN          PIC X(01).
           03  B01R-COMPKBN            PIC X(01).
           03  B01R-TEKIYO-G.
               05  B01R-COMPKBNMEI     PIC X(02).
               05  B01R-UTAGAIKBNMEI   PIC X(01).
               05  B01R-GRPID          PIC X(01).
               05  B01R-FIL1           PIC X(01).
               05  B01R-TEKIYO         PIC X(90).
               05  B01R-TEKIYO-R1      REDEFINES  B01R-TEKIYO.
                   07  B01R-BYOMEICD   PIC X(07).
                   07  B01R-FIL2       PIC X(03).
                   07  B01R-BYOMEI     PIC X(80).
               05  B01R-TEKIYO-R2      REDEFINES  B01R-TEKIYO.
                   07  B01R-FIL3       PIC X(10).
                   07  B01R-MEISAI     PIC X(80).
      *
       FD  B01T-FILE.
       01  B01T-REC.
           03  B01T-MSTSEQ             PIC 9(10).
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "HCM44.INC".
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT5.INC"  REPLACING
                                       //TEIKI01PARA// BY //ASS01//.
      *
           COPY    "CPCOMMONDAT5.INC"  REPLACING
                                       //TEIKI01PARA// BY //ASS02//.
      *
           COPY    "CPCOMMONDAT5.INC"  REPLACING
                                       //TEIKI01PARA// BY //ASS03//.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT                 PIC 9(05).
           03  CNT-TEKCD               PIC 9(05).
           03  CNT-TEKLINE             PIC 9(05).
           03  CNT-SETCD               PIC 9(05).
           03  CNT-MSTSEQ              PIC 9(10).
      *
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-ASS01               PIC X(02).
           03  STS-ASS02               PIC X(02).
           03  STS-ASS03               PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-CHKMST              PIC 9(01).
           03  FLG-CHKSND              PIC 9(01).
           03  FLG-CHKTRD              PIC 9(01).
           03  FLG-CHK005              PIC 9(01).
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-MEISAI              PIC 9(01).
           03  FLG-LINE-FEED           PIC 9(01).
           03  FLG-TOTUGO-G.
               05  FLG-KANZEN          PIC 9(01).
               05  FLG-ZENPO           PIC 9(01).
               05  FLG-KOHO            PIC 9(01).
           03  FLG-TOTUGO-R    REDEFINES   FLG-TOTUGO-G.
               05  FLG-TOTUGO          PIC 9(01)
                                       OCCURS  3.
           03  FLG-TEKCD               PIC 9(01).
           03  FLG-UTAGAIKBN           PIC 9(01).
           03  FLG-NOCHECK             PIC 9(01).
           03  FLG-CHKERR-G.
               05  FLG-KEIKOKU         PIC 9(01).
               05  FLG-NASI            PIC 9(01).
           03  FLG-CHKERR-R    REDEFINES   FLG-CHKERR-G.
               05  FLG-CHKERR          PIC 9(01)
                                       OCCURS  2.
           03  FLG-MSTDAT              PIC 9(01).
           03  FLG-INPUTCD             PIC 9(01).
           03  FLG-INPUTSET            PIC 9(01).
           03  FLG-SRYCDCHG            PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-TARGET              PIC 9(01).
           03  FLG-B01L                PIC 9(01).
           03  FLG-B01R                PIC 9(01).
           03  FLG-B01T                PIC 9(01).
           03  FLG-SET                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX-LINE                PIC 9(05).
           03  IDX-LEN                 PIC 9(05).
           03  IDX-CMB                 PIC 9(05).
           03  IDXY                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD                PIC X(08).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-EDTYMD3             PIC X(22).
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-TNS-SRYCD           PIC X(09).
           03  WRK-PAGE                PIC 9(03).
           03  WRK-NAME-G.
               05  WRK-NAME1           PIC X(04).
               05  WRK-NAME            PIC X(100).
           03  WRK-KEY-CD-G.
               05  WRK-KEY-CD-OCC      OCCURS  21.
                   07  WRK-KEY-CD      PIC X(07).
                   07  WRK-KEY-CDSEP   PIC X(01).
           03  WRK-MEISAISTR           PIC X(500).
           03  WRK-SRYNAME             PIC X(50).
           03  WRK-PNT                 PIC 9(05).
           03  WRK-TGPNT               PIC 9(05).
           03  WRK-STR                 PIC X(500).
           03  WRK-STRLEN              PIC 9(05).
           03  WRK-TEKIYOLEN           PIC 9(05).
           03  WRK-BYOMEICD            PIC X(07).
           03  WRK-BYOMEI-G.
               05  WRK-BYOMEI-SP       PIC X(03).
               05  WRK-BYOMEI          PIC X(80).
           03  WRK-MSIBYO-G.
               05  WRK-MSIBYO-MEI      PIC X(80).
           03  WRK-CMB-CD              PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM         PIC X(200).
           03  WRK-TEKCD-G.
               05  WRK-TEKCD-MAX       PIC 9(05).
               05  WRK-TEKCD-OCC       OCCURS  200.
                   07  WRK-DMKBN       PIC X(01).
                   07  WRK-TEKCD       PIC X(170).
           03  WRK-MIDASI1             PIC X(56).
           03  WRK-MIDASI2             PIC X(100).
           03  WRK-YKSK-G.
               05  WRK-YKSK-MAX        PIC 9(05).
               05  WRK-YKSK-CD         PIC X(09)
                                       OCCURS  100.
           03  WRK-STSRYCD             PIC X(09).
           03  WRK-EDSRYCD             PIC X(09).
      *
       01  KEY-AREA.
           03  KEY-TEKCD               PIC X(200).
           03  KEY-ID                  PIC 9(09).
      *
      *    印刷内容
       01  HEAD-01.
           03  H01-MIDASI              PIC X(60).
           03  FILLER                  PIC X(08)   VALUE   "基準日：".
           03  H01-KJNYMD              PIC X(09).
           03  FILLER                  PIC X(46)   VALUE   SPACE.
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-SAKYMD              PIC X(09).
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(04)   VALUE   "  P-".
           03  H01-PCNT                PIC ZZ9.
       01  HEAD-02.
           03  FILLER                  PIC X(04)   VALUE   SPACE.
           03  H02-MIDASI1             PIC X(56).
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  H02-MIDASI2             PIC X(100).
       01  BODY-01.
           03  B01-LEFT                PIC X(63).
           03  B01-RIGHT               PIC X(95).
       01  BODY-02.
           03  B02-NYUGAIKBN           PIC X(02).
           03  B02-NYUGAIBNDKBN        PIC X(02).
           03  B02-SRYCD1              PIC X(09).
           03  B02-FL01                PIC X(01).
           03  B02-NAME1               PIC X(50).
           03  B02-FL02                PIC X(03).
           03  B02-DMKBN               PIC X(02).
           03  B02-FL03                PIC X(01).
           03  B02-TEKIYO              PIC X(88).
           03  B02-TEKIYO-R1           REDEFINES   B02-TEKIYO.
               05  B02-SRYCD2          PIC X(09).
               05  B02-FL04            PIC X(01).
               05  B02-NAME2           PIC X(78).
       01  BODY-03.
           03  B03-NYUGAIKBN           PIC X(02).
           03  B03-NYUGAIBNDKBN        PIC X(02).
           03  B03-COMPKBN             PIC X(02).
           03  B03-FL01                PIC X(02).
           03  B03-LEFT                PIC X(60).
           03  B03-LEFT-R1             REDEFINES   B03-LEFT.
               05  B03-BYOMEICD        PIC X(07).
               05  B03-FL02            PIC X(03).
               05  B03-BYOMEI          PIC X(50).
           03  B03-LEFT-R2             REDEFINES   B03-LEFT.
               05  B03-FL03            PIC X(10).
               05  B03-MEISAI          PIC X(50).
           03  B03-FL04                PIC X(03).
           03  B03-DMKBN               PIC X(02).
           03  B03-FL05                PIC X(01).
           03  B03-RIGHT               PIC X(84).
           03  B03-RIGHT-R1            REDEFINES   B03-RIGHT.
               05  B03-SRYCD           PIC X(09).
               05  B03-FL06            PIC X(01).
               05  B03-NAME            PIC X(74).
       01  BODY-04.
           03  B04-SRYCD1              PIC X(09).
           03  B04-FL01                PIC X(01).
           03  B04-NAME1               PIC X(50).
           03  B04-FL02                PIC X(03).
           03  B04-NYUGAIBNDKBN        PIC X(02).
           03  B04-CHKERR              PIC X(02).
           03  B04-DATAKBN             PIC X(02).
           03  B04-FL03                PIC X(01).
           03  B04-SRYCD2              PIC X(09).
           03  B04-FL04                PIC X(01).
           03  B04-NAME2               PIC X(78).
      *
       01  CONST-AREA.
           03  CONST-HCM44-MAX         PIC 9(05)   VALUE   55.
           03  CONST-MEISAI-ST         PIC 9(05)   VALUE   4.
           03  CONST-TEKIYO-MAX        PIC 9(05)   VALUE   90.
           03  CONST-TEKIYO-R3-MAX     PIC 9(05)   VALUE   80.
           03  CONST-LEFT-MAX          PIC 9(05)   VALUE   60.
           03  CONST-LEFT-R2-MAX       PIC 9(05)   VALUE   50.
           03  CONST-MKCD-MAX          PIC 9(05)   VALUE   21.
           03  CONST-SETCD-MAX         PIC 9(05)   VALUE   100.
           03  CONST-TOTUGO-MES-MAX    PIC 9(05)   VALUE   3.
           03  CONST-TOTUGO-MES-VAL.
               05  CONST-TOTUGO-KANZEN PIC X(12)   VALUE "完：完全一致".
               05  CONST-TOTUGO-ZENPO  PIC X(12)   VALUE "前：前方一致".
               05  CONST-TOTUGO-KOHO   PIC X(12)   VALUE "後：後方一致".
           03  CONST-TOTUGO-MES-R      REDEFINES   CONST-TOTUGO-MES-VAL.
               05  CONST-TOTUGO-MES    PIC X(12)   OCCURS  3.
           03  CONST-CHKERR-MES-MAX    PIC 9(05)   VALUE   2.
           03  CONST-CHKERR-MES-VAL.
               05  CONST-CHKERR-KEIKOKU PIC X(20) 
                                           VALUE "警：警告".
               05  CONST-CHKERR-NASI   PIC X(20)
                                           VALUE "無：チェックしない".
           03  CONST-CHKERR-MES-R      REDEFINES   CONST-CHKERR-MES-VAL.
               05  CONST-CHKERR-MES    PIC X(20)   OCCURS  2.
           03  CONST-TEKCD-MAX         PIC 9(05)   VALUE   200.
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *    チェックマスタ−５退避領域
       01  WKCHK005-REC.
           COPY    "CPCHK005.INC"      REPLACING   //CHK005-//
                                       BY          //WKCHK005-//.
      *
       01  WKINPUTSET-REC.
           COPY    "CPINPUTSET.INC"    REPLACING   //ISET-//
                                       BY          //WKISET-//.
      *
           COPY    "CPTTNS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY  "CPSYSKANRI.INC".
      *
           COPY  "CPSK1101.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    チェックマスタ−２
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
      *    チェックマスタ−３
       01  CHKTRD-REC.
           COPY    "CPCHKTRD.INC".
      *
      *    チェックマスタ−５
       01  CHK005-REC.
           COPY    "CPCHK005.INC".
      *
      *
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
       01 UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  ST                          PIC 9(2).
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    一覧印刷変換領域
           COPY    "CPORCHXLST.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHXLSTAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  HXLST-RCD
           INITIALIZE                  HCM44
           INITIALIZE                  KEY-AREA
           INITIALIZE                  TTNS-AREA
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
           MOVE    SPA-HOSPNUM         TO  ASS01-HOSPNUM
           MOVE    "XLSTB01L"          TO  ASS01-FILE-ID
           MOVE    C-UID               TO  ASS01-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  ASS02-HOSPNUM
           MOVE    "XLSTB01R"          TO  ASS02-FILE-ID
           MOVE    C-UID               TO  ASS02-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  ASS03-HOSPNUM
           MOVE    "XLSTB01T"          TO  ASS03-FILE-ID
           MOVE    C-UID               TO  ASS03-TERMID
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
           IF    ( HXLST-RCD       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
           MOVE    ZERO            TO  CNT-TEKCD
           MOVE    ZERO            TO  WRK-PAGE
           MOVE    CONST-MEISAI-ST TO  IDX-LINE
      *
           IF    ( HXLST-SRYCD         =   SPACE )
               MOVE    "000000000"     TO  WRK-STSRYCD
               MOVE    "999999999"     TO  WRK-EDSRYCD
           ELSE
               MOVE    HXLST-SRYCD     TO  WRK-STSRYCD
               IF    ( HXLST-SRYCD-END =   SPACE  )
                OR   ( HXLST-SRYCD     >   HXLST-SRYCD-END )
                   MOVE    HXLST-SRYCD TO  WRK-EDSRYCD
               ELSE
                   MOVE    HXLST-SRYCD-END
                                       TO  WRK-EDSRYCD
               END-IF
           END-IF
      *
      *    明細編集処理
           EVALUATE    HXLST-CHKKBN
           WHEN    "1"
           WHEN    "2"
           WHEN    "6"
      *
               PERFORM 200-MEISAI-CHKSND-SYORI-SEC
           WHEN    "7"
               PERFORM UNTIL ( FLG-CHKTRD  =   1 )
                   PERFORM 200-MEISAI-CHKTRD-SYORI-SEC
               END-PERFORM
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           WHEN    "8"
           WHEN    "9"
               PERFORM UNTIL ( FLG-CHK005  =   1 )
                   PERFORM 200-MEISAI-CHK005-SYORI-SEC
               END-PERFORM
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           WHEN    OTHER
               PERFORM UNTIL ( FLG-CHKMST  =   1 )
                   PERFORM 200-MEISAI-CHKMST-SYORI-SEC
               END-PERFORM
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-EVALUATE
      *
      *    印刷処理
           IF    ( IDX-LINE            >   CONST-MEISAI-ST )
               MOVE    ALL "-" TO  HCM44-MOJI (IDX-LINE)
               PERFORM 2100-HEAD-SEC
               IF    ( HXLST-PROCKBN   =   "1" )
                   PERFORM 400-PRINT-SYORI-SEC
               ELSE
                   PERFORM 410-PRINT-SYORI-SEC
               END-IF
           END-IF
      *
           MOVE    CNT-OUT         TO  HXLST-OUT-CNT
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                   SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1101"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1101-REC
           IF    ( SYS-1101-YBUTAGAIKBN    =   SPACE )
               MOVE    "1"         TO  SYS-1101-YBUTAGAIKBN
           END-IF
      *
           IF    ( HXLST-KJNYMD    =   SPACE )
               MOVE    SPA-SYSYMD  TO  HXLST-KJNYMD
           END-IF
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1     TO  H01-SAKYMD
      *
           MOVE    HXLST-KJNYMD    TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1     TO  H01-KJNYMD
      *
           EVALUATE    HXLST-CHKKBN
           WHEN    "1"
           WHEN    "2"
           WHEN    "6"
               PERFORM 1001-CHKSND-SYOKI-SEC
           WHEN    "4"
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               PERFORM 1001-CHKMST-SYOKI-SEC
           WHEN    "7"
               PERFORM 1001-CHKTRD-SYOKI-SEC
           WHEN    "8"
           WHEN    "9"
               PERFORM 1001-CHK005-SYOKI-SEC
           WHEN    OTHER
               MOVE    -1          TO  HXLST-RCD
           END-EVALUATE
      *
           .
       100-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    初期編集処理（チェックマスタ）
      *****************************************************************
       1001-CHKMST-SYOKI-SEC           SECTION.
      *
           INITIALIZE                  CHK-REC
      *
           EVALUATE    HXLST-CHKKBN
           WHEN    "3"
               MOVE    "3"             TO  CHK-CDKBN
           WHEN    "4" 
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               MOVE    "2"             TO  CHK-CDKBN
           END-EVALUATE
      *
           MOVE    HXLST-CHKKBN        TO  CHK-CHKKBN
           MOVE    HXLST-SRYCD         TO  CHK-SRYCD
      *
           IF    ( HXLST-SRYCD         =   SPACE )
               MOVE    "000000000"     TO  CHK-SRYCD
               MOVE    "999999999"     TO  CHK-UPSRYCD
           ELSE
               IF    ( HXLST-SRYCD-END =   SPACE  )
                OR   ( HXLST-SRYCD     >   HXLST-SRYCD-END )
                   MOVE    HXLST-SRYCD TO  CHK-UPSRYCD
               ELSE
                   MOVE    HXLST-SRYCD-END
                                       TO  CHK-UPSRYCD
               END-IF
           END-IF
      *
           MOVE    HXLST-KJNYMD        TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
      *
           INITIALIZE                  WRK-MCP-TABLE
                                       WRK-MCP-PATHNAME
      *
           EVALUATE HXLST-SRTKBN
           WHEN   "1"
               MOVE    "tbl_chk"           TO  WRK-MCP-TABLE
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
           WHEN   "2"
               MOVE    "tbl_chk"           TO  WRK-MCP-TABLE
               MOVE    "key8"              TO  WRK-MCP-PATHNAME
           WHEN   "3"
               MOVE    "tbl_chk"           TO  WRK-MCP-TABLE
               MOVE    "key9"              TO  WRK-MCP-PATHNAME
           WHEN   OTHER
               MOVE    "tbl_chk"           TO  WRK-MCP-TABLE
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
           END-EVALUATE
      *
      *
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           PERFORM 900-CHKMST-SEL-SEC
      *
           EVALUATE    HXLST-CHKKBN
           WHEN    "3"
               MOVE
               "＜　薬剤と禁忌一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "薬剤コード"
                                       TO  WRK-MIDASI1
                   MOVE    "禁忌コード"
                                       TO  WRK-MIDASI2
           WHEN    "4" 
               MOVE
               "＜　薬剤と併用禁忌一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "薬剤コード"
                                       TO  WRK-MIDASI1
                   MOVE    "適用薬剤コード"
                                       TO  WRK-MIDASI2
           WHEN    "5"
               MOVE
               "＜　同一月に併用算定できない診療行為一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "診療行為コード"
                                       TO  WRK-MIDASI1
                   MOVE    "適用診療行為コード"
                                       TO  WRK-MIDASI2
           WHEN    "A"
               MOVE
               "＜　同一日に併用算定できない診療行為一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "診療行為コード"
                                       TO  WRK-MIDASI1
                   MOVE    "適用診療行為コード"
                                       TO  WRK-MIDASI2
           WHEN    "B"
               MOVE
               "＜　同一会計に併用算定できない診療行為一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "診療行為コード"
                                       TO  WRK-MIDASI1
                   MOVE    "適用診療行為コード"
                                       TO  WRK-MIDASI2
           END-EVALUATE
      *
           .
       1001-CHKMST-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    初期編集処理（チェックマスタ２）
      *****************************************************************
       1001-CHKSND-SYOKI-SEC           SECTION.
      *
           EVALUATE    HXLST-CHKKBN
           WHEN    "1"
               MOVE
               "＜　薬剤と病名一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "薬剤コード"
                                       TO  WRK-MIDASI1
                   MOVE    "病名コード"
                                       TO  WRK-MIDASI2
           WHEN    "2"
               MOVE
               "＜　診療行為と病名一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "診療行為コード"
                                       TO  WRK-MIDASI1
                   MOVE    "病名コード"
                                       TO  WRK-MIDASI2
           WHEN    "6"
               MOVE
               "＜　投与禁忌薬剤と病名　＞"
                                       TO  H01-MIDASI
                   MOVE    "薬剤コード"
                                       TO  WRK-MIDASI1
                   MOVE    "病名コード"
                                       TO  WRK-MIDASI2
           END-EVALUATE
      *
           .
       1001-CHKSND-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    初期編集処理（チェックマスタ３）
      *****************************************************************
       1001-CHKTRD-SYOKI-SEC           SECTION.
      *
           INITIALIZE                  CHKTRD-REC
      *
           MOVE    "2"                 TO  CHKTRD-CDKBN
      *
           MOVE    HXLST-CHKKBN        TO  CHKTRD-CHKKBN
           MOVE    HXLST-SRYCD         TO  CHKTRD-SRYCD
      *
           IF    ( HXLST-SRYCD         =   SPACE )
               MOVE    "000000000"     TO  CHKTRD-SRYCD
               MOVE    "999999999"     TO  CHKTRD-UPSRYCD
           ELSE
               IF    ( HXLST-SRYCD-END =   SPACE  )
                OR   ( HXLST-SRYCD     >   HXLST-SRYCD-END )
                   MOVE    HXLST-SRYCD TO  CHKTRD-UPSRYCD
               ELSE
                   MOVE    HXLST-SRYCD-END
                                       TO  CHKTRD-UPSRYCD
               END-IF
           END-IF
      *
           MOVE    HXLST-KJNYMD        TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
      *
           INITIALIZE                  WRK-MCP-TABLE
                                       WRK-MCP-PATHNAME
      *
           EVALUATE HXLST-SRTKBN
           WHEN   "1"
               MOVE    "tbl_chktrd"    TO  WRK-MCP-TABLE
               MOVE    "key10"         TO  WRK-MCP-PATHNAME
           WHEN   "2"
               MOVE    "tbl_chktrd"    TO  WRK-MCP-TABLE
               MOVE    "key8"          TO  WRK-MCP-PATHNAME
           WHEN   "3"
               MOVE    "tbl_chktrd"    TO  WRK-MCP-TABLE
               MOVE    "key9"          TO  WRK-MCP-PATHNAME
           WHEN   OTHER
               MOVE    "tbl_chktrd"    TO  WRK-MCP-TABLE
               MOVE    "key10"         TO  WRK-MCP-PATHNAME
           END-EVALUATE
      *
      *
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           PERFORM 900-CHKTRD-SEL-SEC
      *
           MOVE
               "＜　診療行為どうしの算定漏れ一覧表　＞"
                                       TO  H01-MIDASI
           MOVE    "診療行為コード（薬剤コード）"
                                       TO  WRK-MIDASI1
           MOVE    "診療行為コード（薬剤コード）"
                                       TO  WRK-MIDASI2
      *
           .
       1001-CHKTRD-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    初期編集処理（チェックマスタ５）
      *****************************************************************
       1001-CHK005-SYOKI-SEC           SECTION.
      *
           INITIALIZE                  CHK005-REC
      *
           MOVE    "2"                 TO  CHK005-CDKBN
      *
           MOVE    HXLST-CHKKBN        TO  CHK005-CHKKBN
      *
           MOVE    HXLST-KJNYMD        TO  CHK005-YUKOSTYMD
                                           CHK005-YUKOEDYMD
      *
           MOVE    "tbl_chk005"        TO  WRK-MCP-TABLE
           MOVE    "key6"              TO  WRK-MCP-PATHNAME
      *
      *
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    CHK005-REC          TO  MCPDATA-REC
           PERFORM 900-CHK005-SEL-SEC
      *
           EVALUATE    HXLST-CHKKBN
           WHEN    "8"
               MOVE
               "＜　病名と薬剤一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "病名コード"
                                       TO  WRK-MIDASI1
                   MOVE    "薬剤コード"
                                       TO  WRK-MIDASI2
           WHEN    "9"
               MOVE
               "＜　病名と診療行為一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "病名コード"
                                       TO  WRK-MIDASI1
                   MOVE    "診療行為コード"
                                       TO  WRK-MIDASI2
           END-EVALUATE
      *
           .
       1001-CHK005-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタより）
      *****************************************************************
       200-MEISAI-CHKTRD-SYORI-SEC     SECTION.
      *
           IF    ( KEY-TEKCD   NOT =   CHKTRD-SRYCD )
      *
               IF    ( IDX-LINE    >   CONST-MEISAI-ST )
                   MOVE    ALL "-" TO  HCM44-MOJI (IDX-LINE)
                   PERFORM 2200-PAGE-FEED-CHK-SEC
               END-IF
      *
               MOVE    CHKTRD-SRYCD   TO  KEY-TEKCD
      *
               MOVE    ZERO           TO  CNT-TEKCD
      *
               MOVE    CHKTRD-SRYCD   TO  WRK-TNS-SRYCD
               PERFORM 2001-SRYCD-HEN-SEC
               MOVE    WRK-NAME-G  TO  WRK-SRYNAME
      *
           END-IF
      *
           COMPUTE CNT-TEKCD   =   CNT-TEKCD   +   1
      *
           INITIALIZE          BODY-02
      *
           IF    ( CNT-TEKCD   =   1 )
      *
               EVALUATE    CHKTRD-NYUGAIKBN
               WHEN    "1"
                   MOVE    "入"       TO  B02-NYUGAIKBN
               WHEN    "2"
                   MOVE    "外"       TO  B02-NYUGAIKBN
               WHEN    OTHER
                   MOVE    "　"       TO  B02-NYUGAIKBN
               END-EVALUATE
      *
               IF    ( CHKTRD-NYUGAIBNDKBN =  "2" )
                   MOVE    "別"       TO  B02-NYUGAIBNDKBN
               ELSE
                   MOVE    "　"       TO  B02-NYUGAIBNDKBN
               END-IF
      *
               MOVE    CHKTRD-SRYCD   TO  B02-SRYCD1
               MOVE    WRK-SRYNAME    TO  B02-NAME1
           ELSE
               MOVE    "　"       TO  B02-NYUGAIKBN
               MOVE    "　"       TO  B02-NYUGAIBNDKBN
           END-IF
      *
           IF    ( CHKTRD-DAYMONTHKBN =   "0" )
               MOVE    "日"           TO  B02-DMKBN
           ELSE
               MOVE    "月"           TO  B02-DMKBN
           END-IF
      *
           MOVE    CHKTRD-CD          TO  B02-SRYCD2
      *
           MOVE    CHKTRD-CD          TO  WRK-TNS-SRYCD
           PERFORM 2001-SRYCD-HEN-SEC
           MOVE    WRK-NAME-G         TO  B02-NAME2
      *
           INSPECT B02-NAME1   REPLACING   ALL "  "    BY  "　"
      *
           MOVE    BODY-02            TO  HCM44-MOJI (IDX-LINE)
      *
           PERFORM 2200-PAGE-FEED-CHK-SEC
      *
           PERFORM 900-CHKTRD-FET-SEC
      *
           .
       200-MEISAI-CHKTRD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタ２より）
      *****************************************************************
       200-MEISAI-CHKSND-SYORI-SEC     SECTION.
      *
           OPEN    OUTPUT  B01L-FILE   B01R-FILE   B01T-FILE
      *
           MOVE    ZERO                TO  FLG-TARGET
           INITIALIZE                      WRK-YKSK-G
      *
           INITIALIZE                  CHKSND-REC
      *
           MOVE    "1"                 TO  CHKSND-CDKBN
      *
           MOVE    HXLST-CHKKBN        TO  CHKSND-CHKKBN
      *
           MOVE    WRK-STSRYCD         TO  CHKSND-SRYCD
           MOVE    WRK-EDSRYCD         TO  CHKSND-UPSRYCD
      *
           MOVE    HXLST-KJNYMD        TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
      *
           INITIALIZE                  WRK-MCP-TABLE
                                       WRK-MCP-PATHNAME
      *
           EVALUATE HXLST-SRTKBN
           WHEN   "1"
               MOVE    "tbl_chksnd"    TO  WRK-MCP-TABLE
               MOVE    "key10"         TO  WRK-MCP-PATHNAME
           WHEN   "2"
               MOVE    "tbl_chksnd"    TO  WRK-MCP-TABLE
               MOVE    "key8"          TO  WRK-MCP-PATHNAME
           WHEN   "3"
               MOVE    "tbl_chksnd"    TO  WRK-MCP-TABLE
               MOVE    "key9"          TO  WRK-MCP-PATHNAME
           WHEN   OTHER
               MOVE    "tbl_chksnd"    TO  WRK-MCP-TABLE
               MOVE    "key10"         TO  WRK-MCP-PATHNAME
           END-EVALUATE
      *
      *
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    ZERO                TO  FLG-SET
           PERFORM 900-CHKSND-SEL-SEC
           PERFORM 2101-MEISAI-CHKSND-SYORI-SEC
      *
           IF    ( HXLST-SRYCD(1:1)    =   "S" OR "P" )
               CONTINUE
           ELSE
               MOVE    "P%"            TO  WRK-TNS-SRYCD
               MOVE    1               TO  FLG-SET
               MOVE    "tbl_chksnd"    TO  WRK-MCP-TABLE
               MOVE    "key23"         TO  WRK-MCP-PATHNAME
               PERFORM 900-CHKSND-KEY23-SEL-SEC
               PERFORM 2101-MEISAI-CHKSND-SYORI-SEC
      *
               MOVE    "S%"                TO  WRK-TNS-SRYCD
               MOVE    1               TO  FLG-SET
               MOVE    "tbl_chksnd"    TO  WRK-MCP-TABLE
               MOVE    "key23"         TO  WRK-MCP-PATHNAME
               PERFORM 900-CHKSND-KEY23-SEL-SEC
               PERFORM 2101-MEISAI-CHKSND-SYORI-SEC
           END-IF
      *
      *
           CLOSE   B01L-FILE   B01R-FILE   B01T-FILE
      *
           OPEN    INPUT   B01L-FILE   B01R-FILE   B01T-FILE
      *
           PERFORM 900-B01L-READ-SEC
           PERFORM 900-B01R-READ-SEC
           PERFORM 900-B01T-READ-SEC
      *
           PERFORM UNTIL ( FLG-B01T    NOT =   ZERO )
      *
               EVALUATE    TRUE
               WHEN  ( B01T-MSTSEQ >   B01L-MSTSEQ )
                   PERFORM 900-B01L-READ-SEC
               WHEN  ( B01T-MSTSEQ >   B01R-MSTSEQ )
                   PERFORM 900-B01R-READ-SEC
               WHEN  ( B01T-MSTSEQ  =   B01L-MSTSEQ )
                AND  ( B01T-MSTSEQ  =   B01R-MSTSEQ )
      *
                   PERFORM 210-BODY-01-LEFT-SEC
                   PERFORM 210-BODY-01-RIGHT-SEC
                   INSPECT BODY-01    REPLACING     ALL "  "    BY  "　"
                   MOVE    BODY-01    TO  HCM44-MOJI (IDX-LINE)
                   PERFORM 2200-PAGE-FEED-CHK-SEC
      *
                   PERFORM 900-B01R-READ-SEC
                   PERFORM 900-B01L-READ-SEC
               WHEN  ( B01T-MSTSEQ  <   B01L-MSTSEQ )
                AND  ( B01T-MSTSEQ  =   B01R-MSTSEQ )
      *
                   MOVE    SPACE      TO  B01-LEFT
                   PERFORM 210-BODY-01-RIGHT-SEC
                   INSPECT BODY-01    REPLACING     ALL "  "    BY  "　"
                   MOVE    BODY-01    TO  HCM44-MOJI (IDX-LINE)
                   PERFORM 2200-PAGE-FEED-CHK-SEC
                   PERFORM 900-B01R-READ-SEC
               WHEN  ( B01T-MSTSEQ  =   B01L-MSTSEQ )
                AND  ( B01T-MSTSEQ  <   B01R-MSTSEQ )
      *
                   MOVE    SPACE      TO  B01-RIGHT
                   PERFORM 210-BODY-01-LEFT-SEC
                   INSPECT BODY-01    REPLACING     ALL "  "    BY  "　"
                   MOVE    BODY-01    TO  HCM44-MOJI (IDX-LINE)
                   PERFORM 2200-PAGE-FEED-CHK-SEC
      *
                   PERFORM 900-B01L-READ-SEC
               WHEN    OTHER
                   PERFORM 900-B01T-READ-SEC
                   IF    ( FLG-B01T    =   ZERO )
                       IF    ( IDX-LINE    >   CONST-MEISAI-ST )
                           MOVE    ALL "-" TO  HCM44-MOJI (IDX-LINE)
                           PERFORM 2200-PAGE-FEED-CHK-SEC
                       END-IF
                   END-IF
               END-EVALUATE
      *
           END-PERFORM
      *
           CLOSE   B01L-FILE   B01R-FILE   B01T-FILE
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    ASS01           TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    ASS02           TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    ASS03           TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       200-MEISAI-CHKSND-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理（チェックマスタ２より）
      *****************************************************************
       2101-MEISAI-CHKSND-SYORI-SEC    SECTION.
      *
           PERFORM UNTIL ( FLG-CHKSND  =   1 )
      *
               COMPUTE CNT-MSTSEQ  =   CNT-MSTSEQ  +   1
      *
               MOVE    CHKSND-SRYCD    TO  KEY-TEKCD
      *
               MOVE    ZERO            TO  CNT-TEKCD
               MOVE    ZERO            TO  FLG-TARGET
               INITIALIZE                  WRK-YKSK-G
      *
               PERFORM 210-MEISAI-B01L-SEC
      *
               PERFORM UNTIL ( FLG-CHKSND      =   1 )
                        OR   ( KEY-TEKCD   NOT =   CHKSND-SRYCD )
      *
                   PERFORM 210-MEISAI-B01R-SEC
      *
                   PERFORM 900-CHKSND-FET-SEC
      *
               END-PERFORM
      *
               PERFORM VARYING IDXY    FROM    1   BY  1
                       UNTIL ( IDXY    >   WRK-YKSK-MAX )
                   PERFORM 210-YAKUSOKU-CHK-SEC
               END-PERFORM
      *
               IF    ( FLG-TARGET  =   1 )
                   INITIALIZE              B01T-REC
                   MOVE    CNT-MSTSEQ  TO  B01T-MSTSEQ
                   WRITE   B01T-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2101-MEISAI-CHKSND-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタ２より）
      *****************************************************************
       210-MEISAI-B01L-SEC             SECTION.
      *
           INITIALIZE                  B01L-REC
      *
           MOVE    CNT-MSTSEQ      TO  B01L-MSTSEQ
      *
           MOVE    CHKSND-SRYCD    TO  WRK-TNS-SRYCD
           IF    ( CHKSND-SRYCD(1:1)   =   "S" OR "P" )
               PERFORM 2001-INPUTCD-HEN-SEC
      *
               MOVE    SPACE           TO  WRK-SRYNAME
               IF    ( WRK-NAME1       =   SPACE )
                   STRING  "【"            DELIMITED   BY  SIZE
                           WRK-NAME        DELIMITED   BY  SPACE
                           "】"            DELIMITED   BY  SIZE
                   INTO    WRK-SRYNAME
                   END-STRING
               ELSE
                   STRING  "【"            DELIMITED   BY  SIZE
                           WRK-NAME1       DELIMITED   BY  SPACE
                           WRK-NAME        DELIMITED   BY  SPACE
                           "】"            DELIMITED   BY  SIZE
                   INTO    WRK-SRYNAME
               END-STRING
               END-IF
           ELSE
               PERFORM 2001-SRYCD-HEN-SEC
               MOVE    WRK-NAME-G      TO  WRK-SRYNAME
           END-IF
      *
           MOVE    CHKSND-NYUGAIKBN    TO  B01L-NYUGAIKBN
           EVALUATE    CHKSND-NYUGAIKBN
           WHEN    "1"
               MOVE    "入"        TO  B01L-NYUGAIKBNMEI
           WHEN    "2"
               MOVE    "外"        TO  B01L-NYUGAIKBNMEI
           WHEN    OTHER
               MOVE    "　"        TO  B01L-NYUGAIKBNMEI
           END-EVALUATE
      *
           MOVE    CHKSND-SRYCD    TO  B01L-SRYCD
           MOVE    WRK-SRYNAME     TO  B01L-NAME
      *
           WRITE   B01L-REC
      *
           IF    ( CHKSND-SRYCD(1:1)       =   "S" OR "P" )
      *
               MOVE    ZERO        TO  CNT-SETCD
      *
               PERFORM 900-INPUTSET-KEY7-SEL-SEC
      *
               PERFORM UNTIL ( FLG-INPUTSET    NOT =   ZERO )
                        OR   ( CNT-SETCD  >=   CONST-SETCD-MAX )
      *
                   IF    ( ISET-INPUTCD(1:1)   =   "." )
                       CONTINUE
                   ELSE
      *
                       COMPUTE CNT-SETCD   =   CNT-SETCD   +   1
      *
                       IF    ( ISET-INPUTCD   >=   WRK-STSRYCD )
                        AND  ( ISET-INPUTCD   <=   WRK-EDSRYCD )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
      *
                       INITIALIZE              B01L-REC
      *
                       MOVE    CNT-MSTSEQ  TO  B01L-MSTSEQ
      *
                       STRING  ">"             DELIMITED   BY  SIZE
                               ISET-INPUTCD    DELIMITED   BY  SIZE
                       INTO    B01L-SRYCD
                       END-STRING
      *
                       MOVE    ISET-INPUTCD    TO  WRK-TNS-SRYCD
                       IF    ( WRK-TNS-SRYCD (1:1) =   "S" )
      *
                           PERFORM 2001-INPUTCD-HEN-SEC
      *
                           INITIALIZE                  WKINPUTSET-REC
                           MOVE    SPA-HOSPNUM     TO  WKISET-HOSPNUM
                           MOVE    ISET-INPUTCD    TO  WKISET-SETCD
                           MOVE    HXLST-KJNYMD    TO  WKISET-YUKOSTYMD
                                                       WKISET-YUKOEDYMD
                           MOVE    1               TO  WKISET-SETSEQ
                           MOVE    WKINPUTSET-REC  TO  MCPDATA-REC
                           MOVE    "tbl_inputset"  TO  MCP-TABLE
                           MOVE    "key"           TO  MCP-PATHNAME
                           PERFORM 910-DBSELECT-SEC
                           IF    ( MCP-RC      NOT =   ZERO )
                               MOVE    "削"        TO  WRK-NAME1
                           END-IF
                           MOVE    "tbl_inputset"  TO  MCP-TABLE
                           MOVE    "key"           TO  MCP-PATHNAME
                           PERFORM 910-DBCLOSECURSOR-SEC
      *
                           MOVE    WRK-NAME-G      TO  WRK-SRYNAME
      *
                       ELSE
                           PERFORM 2001-SRYCD-HEN-SEC
                           MOVE    WRK-NAME-G      TO  WRK-SRYNAME
                       END-IF
                       MOVE    WRK-SRYNAME     TO  B01L-NAME
      *
                       WRITE   B01L-REC
      *
                       IF    ( ISET-INPUTCD(1:1)   =   "S" )
                           IF    ( WRK-YKSK-MAX   >=   CONST-SETCD-MAX )
                               CONTINUE
                           ELSE
                               COMPUTE WRK-YKSK-MAX
                                   =   WRK-YKSK-MAX    +   1
                               MOVE    ISET-INPUTCD
                                       TO  WRK-YKSK-CD (WRK-YKSK-MAX)
                           END-IF
                       ELSE
                           PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                           IF    ( FLG-SRYCDCHG    =   ZERO )
      *
                               IF    ( CHG-RJNSRYCD   >=   WRK-STSRYCD )
                                AND  ( CHG-RJNSRYCD   <=   WRK-EDSRYCD )
                                   MOVE    1       TO  FLG-TARGET
                               END-IF
      *
                               INITIALIZE          B01L-REC
      *
                               MOVE    CNT-MSTSEQ  TO  B01L-MSTSEQ
      *
                               STRING      ">("        DELIMITED BY SIZE
                                       CHG-RJNSRYCD    DELIMITED BY SIZE
                                       ")"             DELIMITED BY SIZE
                               INTO    B01L-SRYCD
                               END-STRING
                               MOVE    CHG-RJNSRYCD    TO WRK-TNS-SRYCD
                               PERFORM 2001-SRYCD-HEN-SEC
                               IF    ( WRK-NAME1       =   SPACE )
                                 STRING  WRK-NAME1 DELIMITED   BY  SIZE
                                       "（"        DELIMITED   BY  SIZE
                                       WRK-NAME    DELIMITED   BY  SPACE
                                       "）"        DELIMITED   BY  SIZE
                                 INTO  B01L-NAME
                                 END-STRING
                               ELSE
                                 STRING  "（"      DELIMITED   BY  SIZE
                                       WRK-NAME1   DELIMITED   BY  SPACE
                                       WRK-NAME    DELIMITED   BY  SPACE
                                       "）"        DELIMITED   BY  SIZE
                                 INTO    B01L-NAME
                                 END-STRING
                               END-IF
      *
                               WRITE   B01L-REC
                           END-IF
                       END-IF
      *
                   END-IF
      *
                   PERFORM 900-INPUTSET-KEY7-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_inputset"  TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( FLG-SET             =   ZERO )
                AND  ( HXLST-SRYCD(1:1)    =   "S" OR "P" )
                   MOVE    1               TO  FLG-TARGET
               END-IF
      *
           ELSE
               MOVE    1               TO  FLG-TARGET
           END-IF
      *
           .
       210-MEISAI-B01L-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタ２より）
      *****************************************************************
       210-MEISAI-B01R-SEC             SECTION.
      *
           INITIALIZE                 B01R-REC
      *
           MOVE    CNT-MSTSEQ         TO  B01R-MSTSEQ
           MOVE    CHKSND-COMPKBN     TO  B01R-COMPKBN
      *
           EVALUATE    CHKSND-COMPKBN
           WHEN    "2"
               MOVE    "完"           TO  B01R-COMPKBNMEI
           WHEN    "3"
               MOVE    "前"           TO  B01R-COMPKBNMEI
           WHEN    "4"
               MOVE    "後"           TO  B01R-COMPKBNMEI
           WHEN    OTHER
               MOVE    "　"           TO  B01R-COMPKBNMEI
           END-EVALUATE
      *
           EVALUATE    CHKSND-UTAGAIKBN
           WHEN    "0"
               IF    ( SYS-1101-YBUTAGAIKBN    =   "2" )
                   MOVE    "1"        TO  B01R-UTAGAIKBN
                   MOVE    "#"        TO  B01R-UTAGAIKBNMEI
               END-IF
           WHEN    "1"
               CONTINUE
           WHEN    "2"
                   MOVE    "1"        TO  B01R-UTAGAIKBN
                   MOVE    "#"        TO  B01R-UTAGAIKBNMEI
           END-EVALUATE
      *
           IF    ( CHKSND-CHKKBN   =   "1" OR "2" )
               MOVE    CHKSND-GRPID   TO  B01R-GRPID
           END-IF
      *
           MOVE    CHKSND-CD       TO  WRK-KEY-CD-G
           MOVE    1               TO  WRK-PNT
      *
           IF    ( WRK-KEY-CD (2)  =   SPACE )
               MOVE    WRK-KEY-CD (1)  TO  B01R-BYOMEICD
               MOVE    CHKSND-BYOMEI   TO  B01R-BYOMEI
           ELSE
               PERFORM 2001-MEISAI-CHKSND-SYORI-SEC
           END-IF
      *
           WRITE   B01R-REC
      *
           .
       210-MEISAI-B01R-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタ２より）
      *****************************************************************
       2001-MEISAI-CHKSND-SYORI-SEC    SECTION.
      *
           MOVE    ZERO        TO  FLG-MEISAI
           MOVE    ZERO        TO  FLG-TEKCD
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MKCD-MAX )
                    OR   ( WRK-KEY-CD (IDX1)   =   SPACE )
      *
               IF    ( WRK-KEY-CDSEP  (IDX1)   =   "-" )
                   MOVE    "," TO  WRK-KEY-CDSEP  (IDX1)
               END-IF
      *
               MOVE    WRK-KEY-CD-OCC (IDX1)   TO  WRK-MEISAISTR
               PERFORM 20011-MEISAI-CHKSND-SYORI-SEC
      *
           END-PERFORM
      *
           MOVE    CHKSND-BYOMEI   TO  WRK-MEISAISTR
           PERFORM 20011-LINE-FEED-CHK-SEC
           IF    ( FLG-LINE-FEED   =   ZERO )
               MOVE    SPACE           TO  WRK-BYOMEI-G
               MOVE    CHKSND-BYOMEI   TO  WRK-BYOMEI
               MOVE    WRK-BYOMEI-G    TO  WRK-MEISAISTR
               PERFORM 20011-MEISAI-CHKSND-SYORI-SEC
           ELSE
               MOVE    CHKSND-BYOMEI   TO  WRK-MEISAISTR
               PERFORM 20011-MEISAI-CHKSND-SYORI-SEC
           END-IF
      *
           MOVE    "【"            TO  WRK-MEISAISTR
           PERFORM 20011-MEISAI-CHKSND-SYORI-SEC
      *
           MOVE    1               TO  FLG-TEKCD
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MKCD-MAX )
                    OR   ( WRK-KEY-CD (IDX1)   =   SPACE )
      *
               INITIALIZE          WRK-MSIBYO-G
      *
               MOVE    WRK-KEY-CD (IDX1)   TO  WRK-CMB-CD
               PERFORM 2100-YYKBYOCD-CHK-SEC
               IF    ( WRK-CMB-ITM NOT =   SPACE )
                   MOVE    WRK-CMB-ITM TO  WRK-MSIBYO-MEI
               ELSE
                   MOVE    WRK-KEY-CD (IDX1)   TO  WRK-BYOMEICD
                   PERFORM 900-BYOMEI-KEY-SEL-SEC
                   MOVE    BYO-BYOMEI  TO  WRK-MSIBYO-MEI
               END-IF
               IF    ( IDX1    >=  CONST-MKCD-MAX )
                OR   ( WRK-KEY-CD (IDX1 + 1 )  =   SPACE )
                   MOVE    WRK-MSIBYO-G    TO  WRK-MEISAISTR
                   PERFORM 20011-MEISAI-CHKSND-SYORI-SEC
               ELSE
                   INITIALIZE  WRK-MEISAISTR
                   STRING  WRK-MSIBYO-MEI  DELIMITED   BY  SPACE
                           ","             DELIMITED   BY  SIZE
                   INTO    WRK-MEISAISTR
                   END-STRING
                   PERFORM 20011-MEISAI-CHKSND-SYORI-SEC
               END-IF
      *
      *
           END-PERFORM
      *
           MOVE    "】"            TO  WRK-MEISAISTR
           PERFORM 20011-MEISAI-CHKSND-SYORI-SEC
      *
           .
       2001-MEISAI-CHKSND-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    入力セット内約束コード対象チェック処理
      *****************************************************************
       210-YAKUSOKU-CHK-SEC            SECTION.
      *
           MOVE    WRK-YKSK-CD (WRK-YKSK-MAX)
                                   TO  WRK-TNS-SRYCD
           PERFORM 900-INPUTSET-KEY7-SEL-SEC
      *
           PERFORM UNTIL ( FLG-INPUTSET    NOT =   ZERO )
      *
               IF    ( ISET-INPUTCD   >=   WRK-STSRYCD )
                AND  ( ISET-INPUTCD   <=   WRK-EDSRYCD )
                   MOVE    1       TO  FLG-TARGET
               END-IF
      *
               PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
               IF    ( FLG-SRYCDCHG    =   ZERO )
                   IF    ( CHG-RJNSRYCD   >=   WRK-STSRYCD )
                    AND  ( CHG-RJNSRYCD   <=   WRK-EDSRYCD )
                       MOVE    1   TO  FLG-TARGET
                   END-IF
               END-IF
      *
               PERFORM 900-INPUTSET-KEY7-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_inputset"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-YAKUSOKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックリスト編集処理
      *****************************************************************
       210-BODY-01-LEFT-SEC            SECTION.
      *
           MOVE    B01L-TEKIYO-G   TO  B01-LEFT
      *
           IF    ( B01L-NYUGAIKBN  =   "0" )
               MOVE    1           TO  FLG-NOCHECK
           END-IF
      *
           .
       210-BODY-01-LEFT-EXT.
           EXIT.
      *****************************************************************
      *    チェックリスト編集処理
      *****************************************************************
       210-BODY-01-RIGHT-SEC           SECTION.
      *
           EVALUATE    B01R-COMPKBN
           WHEN    "2"
               MOVE    1      TO  FLG-KANZEN
           WHEN    "3"
               MOVE    1      TO  FLG-ZENPO
           WHEN    "4"
               MOVE    1      TO  FLG-KOHO
           END-EVALUATE
      *
           MOVE    B01R-TEKIYO-G   TO  B01-RIGHT
           IF    ( B01R-UTAGAIKBN  =   "1" )
               MOVE    1      TO  FLG-UTAGAIKBN
           END-IF
      *
           .
       210-BODY-01-RIGHT-EXT.
           EXIT.
      *****************************************************************
      *    改行チェック処理
      *****************************************************************
       20011-LINE-FEED-CHK-SEC         SECTION.
      *
           MOVE    WRK-MEISAISTR   TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
      *
           COMPUTE WRK-TEKIYOLEN   =   WRK-STRLEN  +   WRK-PNT -   1
      *
           MOVE    ZERO            TO  FLG-LINE-FEED
      *
           IF    (      FLG-MEISAI         =   ZERO )
               IF    (  WRK-TEKIYOLEN  >   CONST-TEKIYO-MAX )
                   MOVE    1       TO  FLG-LINE-FEED
               END-IF
           ELSE
               IF    (  WRK-TEKIYOLEN  >   CONST-TEKIYO-R3-MAX )
                   MOVE    1       TO  FLG-LINE-FEED
               END-IF
           END-IF
      *
           .
       20011-LINE-FEED-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタ２より）
      *****************************************************************
       20011-MEISAI-CHKSND-SYORI-SEC   SECTION.
      *
           PERFORM 20011-LINE-FEED-CHK-SEC
      *
           IF  ( FLG-LINE-FEED         =   1 )
      *
               WRITE   B01R-REC
      *
               INITIALIZE              B01R-REC
               MOVE    CNT-MSTSEQ  TO  B01R-MSTSEQ
      *
               MOVE    1       TO  WRK-PNT
               MOVE    1       TO  FLG-MEISAI
           END-IF
      *
           IF    ( FLG-MEISAI      =   ZERO )
               STRING  WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
               INTO    B01R-TEKIYO
               WITH    POINTER     WRK-PNT
               END-STRING
           ELSE
               IF    ( FLG-TEKCD   =   ZERO )
                   STRING  WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
                   INTO    B01R-MEISAI
                   WITH    POINTER     WRK-PNT
                   END-STRING
               ELSE
                   STRING  WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
                           " "                     DELIMITED   BY  SIZE
                   INTO    B01R-MEISAI
                   WITH    POINTER     WRK-PNT
                   END-STRING
               END-IF
           END-IF
      *
           .
       20011-MEISAI-CHKSND-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタより）
      *****************************************************************
       200-MEISAI-CHKMST-SYORI-SEC     SECTION.
      *
           IF    ( KEY-TEKCD   NOT =   CHK-SRYCD )
               IF    ( IDX-LINE    >   CONST-MEISAI-ST )
                   MOVE    ALL "-" TO  HCM44-MOJI (IDX-LINE)
                   PERFORM 2200-PAGE-FEED-CHK-SEC
               END-IF
      *
               MOVE    CHK-SRYCD   TO  KEY-TEKCD
      *
               MOVE    ZERO        TO  CNT-TEKCD
      *
               MOVE    CHK-SRYCD   TO  WRK-TNS-SRYCD
               PERFORM 2001-SRYCD-HEN-SEC
               MOVE    WRK-NAME-G  TO  WRK-SRYNAME
      *
           END-IF
      *
           COMPUTE CNT-TEKCD   =   CNT-TEKCD   +   1
      *
           INITIALIZE          BODY-04
      *
           IF    ( CNT-TEKCD   =   1 )
               MOVE    CHK-SRYCD   TO  B04-SRYCD1
               MOVE    WRK-SRYNAME TO  B04-NAME1
           END-IF
      *
           IF    ( CHK-DATAKBN     =   "1" )
               MOVE    "◎"        TO  B04-DATAKBN
               MOVE    1           TO  FLG-MSTDAT
           ELSE
               MOVE    "　"        TO  B04-DATAKBN
           END-IF
      *
           IF    ( CHK-NYUGAIBNDKBN =  "1" )
               MOVE    "別"        TO  B04-NYUGAIBNDKBN
           ELSE
               MOVE    "　"        TO  B04-NYUGAIBNDKBN
           END-IF
      *
           EVALUATE    CHK-CHKERR
           WHEN    "8"
               MOVE    "無"        TO  B04-CHKERR
               MOVE    1           TO  FLG-NASI
           WHEN    "9"
               MOVE    "警"        TO  B04-CHKERR
               MOVE    1           TO  FLG-KEIKOKU
           WHEN    OTHER
               MOVE    "　"        TO  B04-CHKERR
           END-EVALUATE
      *
           MOVE    CHK-CD          TO  B04-SRYCD2
      *
           MOVE    CHK-CD          TO  WRK-TNS-SRYCD
           PERFORM 2001-SRYCD-HEN-SEC
           MOVE    WRK-NAME-G      TO  B04-NAME2
      *
           INSPECT B04-NAME1       REPLACING   ALL "  "  BY  "　"
      *
           MOVE    BODY-04         TO  HCM44-MOJI (IDX-LINE)
           PERFORM 2200-PAGE-FEED-CHK-SEC
      *
           PERFORM 900-CHKMST-FET-SEC
      *
           .
       200-MEISAI-CHKMST-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタ５より）
      *****************************************************************
       200-MEISAI-CHK005-SYORI-SEC     SECTION.
      *
           MOVE    CHK005-ID       TO  KEY-ID
           MOVE    CHK005-REC      TO  WKCHK005-REC
           MOVE    ZERO            TO  CNT-TEKCD
           INITIALIZE                  WRK-TEKCD-G
           PERFORM UNTIL ( FLG-CHK005  NOT =   ZERO )
                    OR   ( CHK005-ID   NOT =   KEY-ID )
      *
               IF    ( CONST-TEKCD-MAX >   WRK-TEKCD-MAX )
                   COMPUTE WRK-TEKCD-MAX
                       =   WRK-TEKCD-MAX   +   1
                   MOVE    CHK005-DAYMONTHKBN
                                       TO  WRK-DMKBN (WRK-TEKCD-MAX)
                   MOVE    CHK005-SRYCD
                                       TO  WRK-TEKCD (WRK-TEKCD-MAX)
               END-IF
      *
               PERFORM 900-CHK005-FET-SEC
      *
           END-PERFORM
      *
           IF    ( IDX-LINE    >   CONST-MEISAI-ST )
               MOVE    ALL "-" TO  HCM44-MOJI (IDX-LINE)
               PERFORM 2200-PAGE-FEED-CHK-SEC
           END-IF
      *
      *
           INITIALIZE              BODY-03
           EVALUATE    WKCHK005-NYUGAIKBN
           WHEN    "1"
               MOVE    "入"       TO  B03-NYUGAIKBN
           WHEN    "2"
               MOVE    "外"       TO  B03-NYUGAIKBN
           WHEN    OTHER
               MOVE    "　"       TO  B03-NYUGAIKBN
           END-EVALUATE
      *
           IF    ( WKCHK005-NYUGAIBNDKBN =  "2" )
               MOVE    "別"       TO  B03-NYUGAIBNDKBN
           ELSE
               MOVE    "　"       TO  B03-NYUGAIBNDKBN
           END-IF
      *
           EVALUATE    WKCHK005-COMPKBN
           WHEN    "2"
               MOVE    "完"           TO  B03-COMPKBN
               MOVE    1              TO  FLG-KANZEN
           WHEN    "3"
               MOVE    "前"           TO  B03-COMPKBN
               MOVE    1              TO  FLG-ZENPO
           WHEN    "4"
               MOVE    "後"           TO  B03-COMPKBN
               MOVE    1              TO  FLG-KOHO
           WHEN    OTHER
               MOVE    "　"           TO  B03-COMPKBN
           END-EVALUATE
      *
           MOVE    WKCHK005-CD       TO  WRK-KEY-CD-G
           MOVE    1                 TO  WRK-PNT
      *
           IF    ( WRK-KEY-CD (2)  =   SPACE )
               MOVE    WRK-KEY-CD (1)  TO  B03-BYOMEICD
               MOVE    WKCHK005-BYOMEI TO  B03-BYOMEI
           ELSE
               PERFORM 2001-MEISAI-CHK005-SYORI-SEC
           END-IF
      *
      *
           PERFORM 20011-MEISAI-CHK005-SRYCD-SEC
           INSPECT B03-LEFT    REPLACING   ALL "  "  BY  "　"
           MOVE    BODY-03         TO  HCM44-MOJI (IDX-LINE)
           PERFORM 2200-PAGE-FEED-CHK-SEC
           INITIALIZE              BODY-03
      *
           PERFORM UNTIL ( CNT-TEKCD   >=  WRK-TEKCD-MAX )
      *
               PERFORM 20011-MEISAI-CHK005-SRYCD-SEC
               INSPECT B03-LEFT    REPLACING   ALL "  "  BY  "　"
               MOVE    BODY-03     TO  HCM44-MOJI (IDX-LINE)
               PERFORM 2200-PAGE-FEED-CHK-SEC
               INITIALIZE          BODY-03
      *
           END-PERFORM
      *
           .
       200-MEISAI-CHK005-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタ５より）
      *****************************************************************
       2001-MEISAI-CHK005-SYORI-SEC    SECTION.
      *
           MOVE    ZERO        TO  FLG-MEISAI
           MOVE    ZERO        TO  FLG-TEKCD
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MKCD-MAX )
                    OR   ( WRK-KEY-CD (IDX1)   =   SPACE )
      *
               IF    ( WRK-KEY-CDSEP  (IDX1)   =   "-" )
                   MOVE    "," TO  WRK-KEY-CDSEP  (IDX1)
               END-IF
      *
               MOVE    WRK-KEY-CD-OCC (IDX1)   TO  WRK-MEISAISTR
               PERFORM 20011-MEISAI-CHK005-SYORI-SEC
      *
           END-PERFORM
      *
           MOVE    WKCHK005-BYOMEI TO  WRK-MEISAISTR
           PERFORM 20011-CHK005-LINE-FEED-CHK-SEC
           IF    ( FLG-LINE-FEED   =   ZERO )
               MOVE    SPACE           TO  WRK-BYOMEI-G
               MOVE    WKCHK005-BYOMEI TO  WRK-BYOMEI
               MOVE    WRK-BYOMEI-G    TO  WRK-MEISAISTR
               PERFORM 20011-MEISAI-CHK005-SYORI-SEC
           ELSE
               MOVE    WKCHK005-BYOMEI TO  WRK-MEISAISTR
               PERFORM 20011-MEISAI-CHK005-SYORI-SEC
           END-IF
      *
           MOVE    "【"            TO  WRK-MEISAISTR
           PERFORM 20011-MEISAI-CHK005-SYORI-SEC
      *
           MOVE    1               TO  FLG-TEKCD
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MKCD-MAX )
                    OR   ( WRK-KEY-CD (IDX1)   =   SPACE )
      *
               INITIALIZE          WRK-MSIBYO-G
      *
               MOVE    WRK-KEY-CD (IDX1)   TO  WRK-CMB-CD
               PERFORM 2100-YYKBYOCD-CHK-SEC
               IF    ( WRK-CMB-ITM NOT =   SPACE )
                   MOVE    WRK-CMB-ITM TO  WRK-MSIBYO-MEI
               ELSE
                   MOVE    WRK-KEY-CD (IDX1)   TO  WRK-BYOMEICD
                   PERFORM 900-BYOMEI-KEY-SEL-SEC
                   MOVE    BYO-BYOMEI  TO  WRK-MSIBYO-MEI
               END-IF
               IF    ( IDX1    >=  CONST-MKCD-MAX )
                OR   ( WRK-KEY-CD (IDX1 + 1 )  =   SPACE )
                   MOVE    WRK-MSIBYO-G    TO  WRK-MEISAISTR
                   PERFORM 20011-MEISAI-CHK005-SYORI-SEC
               ELSE
                   INITIALIZE  WRK-MEISAISTR
                   STRING  WRK-MSIBYO-MEI  DELIMITED   BY  SPACE
                           ","             DELIMITED   BY  SIZE
                   INTO    WRK-MEISAISTR
                   END-STRING
                   PERFORM 20011-MEISAI-CHK005-SYORI-SEC
               END-IF
      *
      *
           END-PERFORM
      *
           MOVE    "】"            TO  WRK-MEISAISTR
           PERFORM 20011-MEISAI-CHK005-SYORI-SEC
      *
           .
       2001-MEISAI-CHK005-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタ５より）
      *****************************************************************
       20011-MEISAI-CHK005-SYORI-SEC   SECTION.
      *
           PERFORM 20011-CHK005-LINE-FEED-CHK-SEC
      *
           IF  ( FLG-LINE-FEED         =   1 )
      *
               PERFORM 20011-MEISAI-CHK005-SRYCD-SEC
      *
               INSPECT B03-LEFT    REPLACING   ALL "  "  BY  "　"
               MOVE    BODY-03 TO  HCM44-MOJI (IDX-LINE)
               PERFORM 2200-PAGE-FEED-CHK-SEC
               INITIALIZE          BODY-03
               MOVE    1       TO  WRK-PNT
               MOVE    1       TO  FLG-MEISAI
           END-IF
      *
           IF    ( FLG-MEISAI   =   ZERO )
               STRING  WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
               INTO    B03-LEFT
               WITH    POINTER     WRK-PNT
               END-STRING
           ELSE
               IF    ( FLG-TEKCD   =   ZERO )
                   STRING  WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
                   INTO    B03-MEISAI
                   WITH    POINTER     WRK-PNT
                   END-STRING
               ELSE
                   STRING  WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
                           " "                     DELIMITED   BY  SIZE
                   INTO    B03-MEISAI
                   WITH    POINTER     WRK-PNT
                   END-STRING
               END-IF
           END-IF
      *
           .
       20011-MEISAI-CHK005-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタ５より）
      *****************************************************************
       20011-CHK005-LINE-FEED-CHK-SEC  SECTION.
      *
           MOVE    WRK-MEISAISTR   TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
      *
           COMPUTE WRK-TEKIYOLEN   =   WRK-STRLEN  +   WRK-PNT -   1
      *
           MOVE    ZERO            TO  FLG-LINE-FEED
      *
           IF    (      FLG-MEISAI         =   ZERO )
               IF    (  WRK-TEKIYOLEN  >   CONST-LEFT-MAX )
                   MOVE    1       TO  FLG-LINE-FEED
               END-IF
           ELSE
               IF    (  WRK-TEKIYOLEN  >   CONST-LEFT-R2-MAX )
                   MOVE    1       TO  FLG-LINE-FEED
               END-IF
           END-IF
      *
           .
       20011-CHK005-LINE-FEED-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理（チェックマスタ５より）
      *****************************************************************
       20011-MEISAI-CHK005-SRYCD-SEC   SECTION.
      *
           IF    ( WRK-TEKCD-MAX   >   CNT-TEKCD )
               COMPUTE CNT-TEKCD   =   CNT-TEKCD   +   1
               IF    ( WRK-DMKBN (CNT-TEKCD)   =   "0" )
                   MOVE    "日"    TO  B03-DMKBN
               ELSE
                   MOVE    "月"    TO  B03-DMKBN
               END-IF
               MOVE    WRK-TEKCD (CNT-TEKCD)
                                   TO  B03-SRYCD
               MOVE    WRK-TEKCD (CNT-TEKCD)
                                   TO  WRK-TNS-SRYCD
               PERFORM 2001-SRYCD-HEN-SEC
               MOVE    WRK-NAME-G  TO  B03-NAME
           END-IF
      *
           .
       20011-MEISAI-CHK005-SRYCD-EXT.
           EXIT.
      *****************************************************************
      *   診療コード処理
      *****************************************************************
       2001-SRYCD-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-NAME-G
      *
           PERFORM 900-TENSU-KEY4-SEL-SEC
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL ( FLG-TENSU   =   1 )
                    OR   ( FLG-OK      =   1 )
      *
               IF    ( TNS-SRYCD(1:1)  =   "1" )
                AND  ( TNS-ROUTEKKBN   =   2   )
      *
                   MOVE    SPACE       TO  WRK-NAME
                   STRING  "［老］"        DELIMITED   BY  SIZE
                           TNS-NAME        DELIMITED   BY  SIZE
                   INTO    WRK-NAME
                   END-STRING
      *
               ELSE
                   MOVE    TNS-NAME    TO  WRK-NAME
               END-IF
      *
               IF     (TNS-YUKOSTYMD  <=  HXLST-KJNYMD )  AND
                      (TNS-YUKOEDYMD  >=  HXLST-KJNYMD )
                   MOVE    1           TO  FLG-OK
               END-IF
      *
               PERFORM 900-TENSU-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-OK              =   ZERO )
               MOVE    "削"            TO  WRK-NAME1
           END-IF
      *
           .
       2001-SRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *   入力コード処理
      *****************************************************************
       2001-INPUTCD-HEN-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-NAME-G
           PERFORM 900-INPUTCD-KEY2-SEL-SEC
           IF    ( FLG-INPUTCD     =   ZERO )
               MOVE    ICD-DSPNAME     TO  WRK-NAME
           ELSE
               MOVE    "削"            TO  WRK-NAME1
           END-IF
      *
           .
       2001-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    予約病名コードチェック処理
      *****************************************************************
       2100-YYKBYOCD-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   TBL-BYOMEICD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( TBL-YYK-BYOMEICD (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE   TBL-YYK-BYOMEI(IDX-CMB)
                                           TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       2100-YYKBYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       2100-HEAD-SEC                   SECTION.
      *
           COMPUTE WRK-PAGE    =   WRK-PAGE    +   1
      *
           MOVE    WRK-PAGE            TO  H01-PCNT
           MOVE    HEAD-01             TO  HCM44-MOJI(1)
      *
           MOVE    1                   TO  WRK-TGPNT
           MOVE    SPACE               TO  H02-MIDASI1
           STRING  WRK-MIDASI1  DELIMITED   BY  SPACE
                   "　"         DELIMITED   BY  SIZE
           INTO    H02-MIDASI1
           WITH    POINTER WRK-TGPNT
           END-STRING
      *
           IF        ( HXLST-CHKKBN    =   "1" )
               IF    ( FLG-NOCHECK     =   1 )
                   STRING "×：チェックしない薬剤"
                                       DELIMITED   BY  SIZE
                   INTO   H02-MIDASI1
                   WITH   POINTER WRK-TGPNT
                   END-STRING
               END-IF
           END-IF
      *
           IF        ( HXLST-CHKKBN    =   "2" )
               IF    ( FLG-NOCHECK     =   1 )
                   STRING "×：チェックしない診療行為"
                                       DELIMITED   BY  SIZE
                   INTO   H02-MIDASI1
                   WITH   POINTER WRK-TGPNT
                   END-STRING
               END-IF
           END-IF
      *
           IF        ( HXLST-CHKKBN    =   "8" OR "9" )
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   CONST-TOTUGO-MES-MAX )
      *
                   IF    ( FLG-TOTUGO (IDX2)   =   1 )
                       STRING CONST-TOTUGO-MES (IDX2)
                                           DELIMITED   BY  SPACE
                              " "          DELIMITED   BY  SIZE
                       INTO   H02-MIDASI1
                       WITH   POINTER  WRK-TGPNT
                       END-STRING
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           MOVE    1                   TO  WRK-TGPNT
           MOVE    SPACE               TO  H02-MIDASI2
           STRING  WRK-MIDASI2         DELIMITED   BY  SPACE
                   "　　"              DELIMITED   BY  SIZE
           INTO    H02-MIDASI2
           WITH    POINTER WRK-TGPNT
           END-STRING
      *
           IF        ( HXLST-CHKKBN    =   "1" OR "2" OR "6" )
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   CONST-TOTUGO-MES-MAX )
      *
                   IF    ( FLG-TOTUGO (IDX2)   =   1 )
                       STRING CONST-TOTUGO-MES (IDX2)
                                           DELIMITED   BY  SPACE
                              " "          DELIMITED   BY  SIZE
                       INTO   H02-MIDASI2
                       WITH   POINTER  WRK-TGPNT
                       END-STRING
                   END-IF
      *
               END-PERFORM
      *
           END-IF
      *
           IF        ( HXLST-CHKKBN    =   "1" OR "2" OR "6" )
               IF    ( FLG-UTAGAIKBN       =   1 )
                   IF    ( HXLST-CHKKBN    =   "1" OR "2" )
                       STRING "#:疑い病名を認めない（適応としない）"
                              DELIMITED   BY  SPACE
                          " " DELIMITED   BY  SIZE
                       INTO   H02-MIDASI2
                       WITH   POINTER  WRK-TGPNT
                       END-STRING
                   ELSE
                       STRING "#:疑い病名を認めない"
                              DELIMITED   BY  SPACE
                          " " DELIMITED   BY  SIZE
                       INTO   H02-MIDASI2
                       WITH   POINTER  WRK-TGPNT
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
           IF        ( HXLST-CHKKBN    =   "5" OR "A" OR "B" )
      *
               IF    ( FLG-MSTDAT      =   1 )
                   STRING "◎：マスタ提供分"
                                       DELIMITED   BY  SPACE
                          " "          DELIMITED   BY  SIZE
                   INTO   H02-MIDASI2
                   WITH   POINTER  WRK-TGPNT
                   END-STRING
               END-IF
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   CONST-CHKERR-MES-MAX )
      *
                   IF    ( FLG-CHKERR (IDX2)   =   1 )
                       STRING CONST-CHKERR-MES (IDX2)
                                           DELIMITED   BY  SPACE
                              " "          DELIMITED   BY  SIZE
                       INTO   H02-MIDASI2
                       WITH   POINTER  WRK-TGPNT
                       END-STRING
                   END-IF
      *
               END-PERFORM
      *
           END-IF
      *
           MOVE    HEAD-02             TO  HCM44-MOJI(2)
      *
           MOVE     ALL "-"            TO  HCM44-MOJI(3)
      *
      *
           .
       2100-HEAD-EXT.
           EXIT.
      *****************************************************************
      *   改ページチェック処理
      *****************************************************************
       2200-PAGE-FEED-CHK-SEC          SECTION.
      *
           IF    ( IDX-LINE        >=  CONST-HCM44-MAX )
      *
               PERFORM 2100-HEAD-SEC
               IF    ( HXLST-PROCKBN   =   "1" )
                   PERFORM 400-PRINT-SYORI-SEC
               ELSE
                   PERFORM 410-PRINT-SYORI-SEC
               END-IF
      *
               INITIALIZE              HCM44
               MOVE    ZERO        TO  FLG-TOTUGO-G
               MOVE    ZERO        TO  FLG-UTAGAIKBN
               MOVE    ZERO        TO  FLG-KEIKOKU
               MOVE    ZERO        TO  FLG-NASI
               MOVE    ZERO        TO  FLG-MSTDAT
               MOVE    ZERO        TO  FLG-NOCHECK
      *
               MOVE    CONST-MEISAI-ST TO  IDX-LINE
      *
           ELSE
               COMPUTE IDX-LINE    =   IDX-LINE    +   1
           END-IF
      *
           .
       2200-PAGE-FEED-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理（オンライン印刷）
      *****************************************************************
       400-PRINT-SYORI-SEC             SECTION.
      *
      *    帳票印刷開始処理
           PERFORM 400-SYOKI-PRINT-SEC
      *
      *    帳票印刷処理
           PERFORM 400-PRINT-SEC
      *
           .
       400-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC44"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SMCNDATE-HMS        TO  HC01PARA-SYOHMS
      *
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
           OPEN    OUTPUT              PRT-FILE
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                SECTION.
      *
      *     印刷用データ出力
            WRITE  HCM44
      *
            CLOSE  PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
            MOVE   "HCM44.red"         TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
            MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   SYS-1031-PRTNM(6)   TO  MKPRT-PRTNM
            CALL   "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           COMPUTE CNT-OUT         =   CNT-OUT     +   1
      *
           .
       400-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理（一括印刷）
      *****************************************************************
       410-PRINT-SYORI-SEC             SECTION.
      *
           MOVE    HXLST-PRTAREA       TO  ORCSPRTAREA
      *
           MOVE    "HCM44.red"        TO  SPRT-PRTID
           MOVE    HCM44              TO  SPRT-PRTDATA
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
      *        CONTINUE
               COMPUTE CNT-OUT         =   CNT-OUT     +   1
           ELSE
               MOVE    9               TO  HXLST-RCD
           END-IF
      *
           .
       410-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
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
      *    文字列長チェック処理
      *****************************************************************
       800-STRLEN-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-STRLEN
           PERFORM VARYING IDX-LEN     FROM    500 BY  -1
                   UNTIL ( IDX-LEN     <   1 )
                    OR   ( WRK-STRLEN  >   ZERO )
               IF    ( WRK-STR (IDX-LEN  : 1)  NOT =   SPACE )
                   MOVE    IDX-LEN TO  WRK-STRLEN
               END-IF
           END-PERFORM
      *
           .
       800-STRLEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKMST-FET-SEC
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           .
       900-CHKMST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタFETCH処理
      *****************************************************************
       900-CHKMST-FET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHK-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   MOVE    HXLST-KJNYMD    TO  TTNS-CHK-YMD
                   MOVE    CHK-SRYCD       TO  TTNS-CHK-SRYCD
      *            点数マスタ検索
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
                   MOVE    HXLST-KJNYMD    TO  TTNS-CHK-YMD
                   MOVE    CHK-CD          TO  TTNS-CHK-SRYCD
      *            点数マスタ検索
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
               ELSE
                   INITIALIZE                  CHK-REC
                   MOVE    1               TO  FLG-CHKMST
               END-IF
      *
           END-PERFORM
      *
           .
       900-CHKMST-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKSND-FET-SEC
           ELSE
               INITIALIZE                  CHKSND-REC
               MOVE    1               TO  FLG-CHKSND
           END-IF
      *
           .
       900-CHKSND-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理
      *****************************************************************
       900-CHKSND-FET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
      *
                   MOVE    MCPDATA-REC     TO  CHKSND-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( CHKSND-SRYCD(1:1)   =   "P" OR "S" )
                       PERFORM 900-INPUTSET-KEY-SEL-SEC
                       IF    ( FLG-INPUTSET    NOT =   ZERO )
                           MOVE    ZERO        TO  FLG-LOOPEND
                       END-IF
                   ELSE
                       MOVE    HXLST-KJNYMD    TO  TTNS-CHK-YMD
                       MOVE    CHKSND-SRYCD    TO  TTNS-CHK-SRYCD
      *                点数マスタ検索
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       IF    ( FLG-TENSU       NOT =   ZERO )
                           MOVE    ZERO        TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
               ELSE
                   INITIALIZE                  CHKSND-REC
                   MOVE    1               TO  FLG-CHKSND
               END-IF
      *
           END-PERFORM
      *
           .
       900-CHKSND-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY23-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    HXLST-CHKKBN        TO  CHKSND-CHKKBN
           MOVE    WRK-TNS-SRYCD       TO  CHKSND-SRYCD
           MOVE    HXLST-KJNYMD        TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    "1"                 TO  CHKSND-CDKBN
           MOVE    CHKSND-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               INITIALIZE                  CHKSND-REC
               MOVE    1               TO  FLG-CHKSND
           END-IF
      *
           .
       900-CHKSND-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理
      *****************************************************************
       900-CHKSND-KEY23-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               INITIALIZE                  CHKSND-REC
               MOVE    1               TO  FLG-CHKSND
           END-IF
      *
           .
       900-CHKSND-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKTRD-FET-SEC
           ELSE
               INITIALIZE                  CHKTRD-REC
               MOVE    1               TO  FLG-CHKTRD
           END-IF
      *
           .
       900-CHKTRD-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３FETCH処理
      *****************************************************************
       900-CHKTRD-FET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHKTRD-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   MOVE    HXLST-KJNYMD    TO  TTNS-CHK-YMD
                   MOVE    CHKTRD-SRYCD    TO  TTNS-CHK-SRYCD
      *            点数マスタ検索
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
                   MOVE    HXLST-KJNYMD    TO  TTNS-CHK-YMD
                   MOVE    CHKTRD-CD       TO  TTNS-CHK-SRYCD
      *            点数マスタ検索
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   INITIALIZE                  CHKTRD-REC
                   MOVE    1               TO  FLG-CHKTRD
               END-IF
      *
           END-PERFORM
      *
           .
       900-CHKTRD-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK005
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHK005-FET-SEC
           ELSE
               INITIALIZE                  CHK005-REC
               MOVE    1               TO  FLG-CHK005
           END-IF
      *
           .
       900-CHK005-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５FETCH処理
      *****************************************************************
       900-CHK005-FET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK005
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHK005  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHK005-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   MOVE    HXLST-KJNYMD    TO  TTNS-CHK-YMD
                   MOVE    CHK005-SRYCD    TO  TTNS-CHK-SRYCD
      *            点数マスタ検索
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
               ELSE
                   INITIALIZE                  CHK005-REC
                   MOVE    1               TO  FLG-CHK005
               END-IF
      *
           END-PERFORM
      *
           .
       900-CHK005-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY4)
      *****************************************************************
       900-TENSU-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-KEY4-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタFETCH処理(KEY4)
      *****************************************************************
       900-TENSU-KEY4-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    病名検索処理(KEY)
      *****************************************************************
       900-BYOMEI-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-BYOMEI
      *
           INITIALIZE                  BYOMEI-REC
           MOVE    WRK-BYOMEICD    TO  BYO-BYOMEICD
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    BYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
                MOVE    MCPDATA-REC    TO  BYOMEI-REC
           ELSE
                INITIALIZE                 BYOMEI-REC
                MOVE    1              TO  FLG-BYOMEI
           END-IF
      *
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BYOMEI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
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
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力コード検索処理
      *****************************************************************
       900-INPUTCD-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTCD
      *
           INITIALIZE                  INPUTCD-REC
           MOVE    SPACE           TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM     TO  ICD-HOSPNUM
           MOVE    "6"             TO  ICD-CDSYU
           MOVE    WRK-TNS-SRYCD   TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  INPUTCD-REC
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1               TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-INPUTCD-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力セット検索処理
      *****************************************************************
       900-INPUTSET-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTSET
      *
           INITIALIZE                  INPUTSET-REC
           MOVE    SPA-HOSPNUM     TO  ISET-HOSPNUM
           MOVE    CHKSND-SRYCD    TO  ISET-SETCD
           MOVE    HXLST-KJNYMD    TO  ISET-YUKOSTYMD
                                       ISET-YUKOEDYMD
           MOVE    1               TO  ISET-SETSEQ
           MOVE    INPUTSET-REC    TO  MCPDATA-REC
           MOVE    "tbl_inputset"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTSET-REC
           ELSE
               INITIALIZE              INPUTSET-REC
               MOVE    1           TO  FLG-INPUTSET
           END-IF
      *
           MOVE    "tbl_inputset"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-INPUTSET-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力セット検索処理
      *****************************************************************
       900-INPUTSET-KEY7-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTSET
      *
           INITIALIZE                  INPUTSET-REC
           MOVE    SPA-HOSPNUM     TO  ISET-HOSPNUM
           MOVE    WRK-TNS-SRYCD   TO  ISET-SETCD
           MOVE    HXLST-KJNYMD    TO  ISET-YUKOSTYMD
                                       ISET-YUKOEDYMD
           MOVE    INPUTSET-REC    TO  MCPDATA-REC
           MOVE    "tbl_inputset"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTSET-REC
           ELSE
               INITIALIZE              INPUTSET-REC
               MOVE    1           TO  FLG-INPUTSET
           END-IF
      *
           .
       900-INPUTSET-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力セットFETCH処理
      *****************************************************************
       900-INPUTSET-KEY7-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTSET
      *
           MOVE    "tbl_inputset"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTSET-REC
           ELSE
               INITIALIZE              INPUTSET-REC
               MOVE    1           TO  FLG-INPUTSET
           END-IF
      *
           .
       900-INPUTSET-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    入力コード検索処理
      *****************************************************************
       900-SRYCDCHG-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYCDCHG
      *
           INITIALIZE                  SRYCDCHG-REC
           MOVE    SPACE           TO  SRYCDCHG-REC
           MOVE    SPA-HOSPNUM     TO  CHG-HOSPNUM
           MOVE    ISET-INPUTCD    TO  CHG-IPNSRYCD
      *
           MOVE    SRYCDCHG-REC    TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYCDCHG-REC
           ELSE
               INITIALIZE              SRYCDCHG-REC
               MOVE    1           TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "tbl_srycdchg"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYCDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY1)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
      *
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
      *
      *            点数マスタ退避ワークに編集する
                   PERFORM 9001-TTNS-HEN-SEC
               ELSE
                   MOVE    1               TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    明細情報（チェック２診療行為）読込処理
      *****************************************************************
       900-B01L-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-B01L
      *
           READ    B01L-FILE
           AT  END
               MOVE    1           TO  FLG-B01L
               MOVE    HIGH-VALUE  TO  B01L-MSTSEQ
           END-READ
      *
           .
      *
       900-B01L-READ-EXT.
           EXIT.
      *****************************************************************
      *    明細情報（チェック２病名）読込処理
      *****************************************************************
       900-B01R-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-B01R
      *
           READ    B01R-FILE
           AT  END
               MOVE    1           TO  FLG-B01R
               MOVE    HIGH-VALUE  TO  B01R-MSTSEQ
           END-READ
      *
           .
      *
       900-B01R-READ-EXT.
           EXIT.
      *****************************************************************
      *    明細情報（チェック２対象）読込処理
      *****************************************************************
       900-B01T-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-B01T
      *
           READ    B01T-FILE
           AT  END
               MOVE    1           TO  FLG-B01T
               MOVE    HIGH-VALUE  TO  B01T-MSTSEQ
           END-READ
     *
           .
      *
       900-B01T-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
