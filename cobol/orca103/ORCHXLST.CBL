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
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "HCM44.INC".
      *
       WORKING-STORAGE             SECTION.
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
      *
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
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
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX-LINE                PIC 9(05).
           03  IDX-LEN                 PIC 9(05).
           03  IDX-CMB                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
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
               05  WRK-BYOMEI-SP       PIC X(01).
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
      *
       01  KEY-AREA.
           03  KEY-TEKCD               PIC X(200).
           03  KEY-ID                  PIC 9(09).
      *
      *    印刷内容
       01  HEAD-01.
           03  FILLER                  PIC X(25)   VALUE   SPACE.
           03  H01-MIDASI              PIC X(80).
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-YY                  PIC 9(04).
           03  FILLER                  PIC X(01)   VALUE   ".".
           03  H01-MM                  PIC Z9.
           03  FILLER                  PIC X(01)   VALUE   ".".
           03  H01-DD                  PIC Z9.
           03  FILLER                  PIC X(04)   VALUE   "  P-".
           03  H01-PCNT                PIC ZZ9.
       01  HEAD-02.
           03  FILLER                  PIC X(10)   VALUE   SPACE.
           03  H02-MIDASI1             PIC X(50).
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  H02-MIDASI2             PIC X(100).
           03  H02-MIDASI2-R   REDEFINES   H02-MIDASI2.
               05  H02-MIDASI2R1       PIC X(30).
               05  H02-MIDASI2R2       PIC X(70).
       01  BODY-01.
           03  B01-NYUGAIKBN           PIC X(02).
           03  B01-SRYCD1              PIC X(09).
           03  FILLER                  PIC X(01).
           03  B01-NAME1               PIC X(50).
           03  FILLER                  PIC X(02).
           03  B01-COMPKBN             PIC X(02).
           03  FILLER                  PIC X(02).
           03  B01-TEKIYO              PIC X(90).
           03  B01-TEKIYO-R1           REDEFINES   B01-TEKIYO.
               05  B01-SRYCD2          PIC X(09).
               05  FILLER              PIC X(01).
               05  B01-NAME2           PIC X(80).
           03  B01-TEKIYO-R2           REDEFINES   B01-TEKIYO.
               05  B01-BYOMEICD        PIC X(07).
               05  FILLER              PIC X(01).
               05  B01-BYOMEI          PIC X(82).
           03  B01-TEKIYO-R3           REDEFINES   B01-TEKIYO.
               05  FILLER              PIC X(08).
               05  B01-MEISAI          PIC X(82).
       01  BODY-02.
           03  B02-NYUGAIKBN           PIC X(02).
           03  B02-NYUGAIBNDKBN        PIC X(02).
           03  B02-SRYCD1              PIC X(09).
           03  FILLER                  PIC X(01).
           03  B02-NAME1               PIC X(50).
           03  FILLER                  PIC X(02).
           03  B02-DMKBN               PIC X(02).
           03  FILLER                  PIC X(02).
           03  B02-TEKIYO              PIC X(88).
           03  B02-TEKIYO-R1           REDEFINES   B02-TEKIYO.
               05  B02-SRYCD2          PIC X(09).
               05  FILLER              PIC X(01).
               05  B02-NAME2           PIC X(78).
       01  BODY-03.
           03  B03-NYUGAIKBN           PIC X(02).
           03  B03-NYUGAIBNDKBN        PIC X(02).
           03  B03-COMPKBN             PIC X(02).
           03  FILLER                  PIC X(02).
           03  B03-LEFT                PIC X(60).
           03  B03-LEFT-R1             REDEFINES   B03-LEFT.
               05  B03-BYOMEICD        PIC X(07).
               05  FILLER              PIC X(01).
               05  B03-BYOMEI          PIC X(52).
           03  B03-LEFT-R2             REDEFINES   B03-LEFT.
               05  FILLER              PIC X(08).
               05  B03-MEISAI          PIC X(52).
           03  FILLER                  PIC X(02).
           03  B03-DMKBN               PIC X(02).
           03  FILLER                  PIC X(02).
           03  B03-RIGHT               PIC X(84).
           03  B03-RIGHT-R1            REDEFINES   B03-RIGHT.
               05  B03-SRYCD           PIC X(09).
               05  FILLER              PIC X(01).
               05  B03-NAME            PIC X(74).
      *
       01  CONST-AREA.
           03  CONST-HCM44-MAX         PIC 9(05)   VALUE   55.
           03  CONST-MEISAI-ST         PIC 9(05)   VALUE   4.
           03  CONST-TEKIYO-MAX        PIC 9(05)   VALUE   90.
           03  CONST-TEKIYO-R3-MAX     PIC 9(05)   VALUE   82.
           03  CONST-LEFT-MAX          PIC 9(05)   VALUE   60.
           03  CONST-LEFT-R2-MAX       PIC 9(05)   VALUE   52.
           03  CONST-MKCD-MAX          PIC 9(05)   VALUE   21.
           03  CONST-CHK-MAX           PIC 9(05)   VALUE   10.
           03  CONST-TOTUGO-MES-MAX    PIC 9(05)   VALUE   3.
           03  CONST-TOTUGO-MES-VAL.
               05  CONST-TOTUGO-KANZEN PIC X(12)   VALUE "完：完全一致".
               05  CONST-TOTUGO-ZENPO  PIC X(12)   VALUE "前：前方一致".
               05  CONST-TOTUGO-KOHO   PIC X(12)   VALUE "後：後方一致".
           03  CONST-TOTUGO-MES-R      REDEFINES   CONST-TOTUGO-MES-VAL.
               05  CONST-TOTUGO-MES    PIC X(12)   OCCURS  3.
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
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
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
      *
           PERFORM 800-MCNDATE-SEC
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
      *    明細編集処理
           EVALUATE    HXLST-CHKKBN
           WHEN    "1"
           WHEN    "2"
           WHEN    "6"
               PERFORM UNTIL ( FLG-CHKSND  =   1 )
                   PERFORM 200-MEISAI-CHKSND-SYORI-SEC
               END-PERFORM
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
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
               PERFORM 2200-PAGE-FEED-CHK-SEC
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
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *
           MOVE    SPA-SYSYMD(1:4)     TO  H01-YY
           MOVE    SPA-SYSYMD(5:2)     TO  H01-MM
           MOVE    SPA-SYSYMD(7:2)     TO  H01-DD
      *
           EVALUATE    HXLST-CHKKBN
           WHEN    "1"
           WHEN    "2"
           WHEN    "6"
               PERFORM 1001-CHKSND-SYOKI-SEC
           WHEN    "4"
           WHEN    "5"
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
               MOVE    "2"             TO  CHK-CDKBN
           END-EVALUATE
      *
           MOVE    HXLST-CHKKBN        TO  CHK-CHKKBN
           MOVE    HXLST-SRYCD         TO  CHK-SRYCD
      *
           IF    ( HXLST-SRYCD         =   SPACE )
               MOVE    "000000000"     TO  CHK-SRYCD
               MOVE    "999999999"     TO  CHKUP-SRYCD
           ELSE
               IF    ( HXLST-SRYCD-END =   SPACE  )
                OR   ( HXLST-SRYCD     >   HXLST-SRYCD-END )
                   MOVE    HXLST-SRYCD TO  CHKUP-SRYCD
               ELSE
                   MOVE    HXLST-SRYCD-END
                                       TO  CHKUP-SRYCD
               END-IF
           END-IF
      *
           MOVE    SPA-SYSYMD          TO  CHK-YUKOSTYMD
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
                                       TO  H02-MIDASI1
                   MOVE    "禁忌コード"
                                       TO  H02-MIDASI2
           WHEN    "4" 
               MOVE
               "＜　薬剤と併用禁忌一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "薬剤コード"
                                       TO  H02-MIDASI1
                   MOVE    "適用薬剤コード"
                                       TO  H02-MIDASI2
           WHEN    "5"
               MOVE
               "＜　同一月に併用算定できない診療行為一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "診療行為コード"
                                       TO  H02-MIDASI1
                   MOVE    "適用診療行為コード"
                                       TO  H02-MIDASI2
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
           INITIALIZE                  CHKSND-REC
      *
           MOVE    "1"                 TO  CHKSND-CDKBN
      *
           MOVE    HXLST-CHKKBN        TO  CHKSND-CHKKBN
           MOVE    HXLST-SRYCD         TO  CHKSND-SRYCD
      *
           IF    ( HXLST-SRYCD         =   SPACE )
               MOVE    "000000000"     TO  CHKSND-SRYCD
               MOVE    "999999999"     TO  CHKSND-UPSRYCD
           ELSE
               IF    ( HXLST-SRYCD-END =   SPACE  )
                OR   ( HXLST-SRYCD     >   HXLST-SRYCD-END )
                   MOVE    HXLST-SRYCD TO  CHKSND-UPSRYCD
               ELSE
                   MOVE    HXLST-SRYCD-END
                                       TO  CHKSND-UPSRYCD
               END-IF
           END-IF
      *
           MOVE    SPA-SYSYMD          TO  CHKSND-YUKOSTYMD
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
           PERFORM 900-CHKSND-SEL-SEC
      *
           EVALUATE    HXLST-CHKKBN
           WHEN    "1"
               MOVE
               "＜　薬剤と病名一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "薬剤コード"
                                       TO  H02-MIDASI1
                   MOVE    "病名コード"
                                       TO  H02-MIDASI2R1
           WHEN    "2"
               MOVE
               "＜　診療行為と病名一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "診療行為コード"
                                       TO  H02-MIDASI1
                   MOVE    "病名コード"
                                       TO  H02-MIDASI2R1
           WHEN    "6"
               MOVE
               "＜　投与禁忌薬剤と病名　＞"
                                       TO  H01-MIDASI
                   MOVE    "薬剤コード"
                                       TO  H02-MIDASI1
                   MOVE    "病名コード"
                                       TO  H02-MIDASI2R1
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
           MOVE    SPA-SYSYMD          TO  CHKTRD-YUKOSTYMD
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
                                       TO  H02-MIDASI1
           MOVE    "診療行為コード（薬剤コード）"
                                       TO  H02-MIDASI2
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
           MOVE    SPA-SYSYMD          TO  CHK005-YUKOSTYMD
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
                                       TO  H02-MIDASI1
                   MOVE    "薬剤コード"
                                       TO  H02-MIDASI2R1
           WHEN    "9"
               MOVE
               "＜　病名と診療行為一覧表　＞"
                                       TO  H01-MIDASI
                   MOVE    "病名コード"
                                       TO  H02-MIDASI1
                   MOVE    "診療行為コード"
                                       TO  H02-MIDASI2R1
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
           IF    ( KEY-TEKCD   NOT =   CHKSND-SRYCD )
               IF    ( IDX-LINE    >   CONST-MEISAI-ST )
                   MOVE    ALL "-" TO  HCM44-MOJI (IDX-LINE)
                   PERFORM 2200-PAGE-FEED-CHK-SEC
               END-IF
      *
               MOVE    CHKSND-SRYCD    TO  KEY-TEKCD
      *
               MOVE    ZERO            TO  CNT-TEKCD
      *
               MOVE    CHKSND-SRYCD    TO  WRK-TNS-SRYCD
               PERFORM 2001-SRYCD-HEN-SEC
               MOVE    WRK-NAME-G  TO  WRK-SRYNAME
      *
           END-IF
      *
           COMPUTE CNT-TEKCD   =   CNT-TEKCD   +   1
      *
           INITIALIZE              BODY-01
      *
           IF    ( CNT-TEKCD   =   1 )
      *
               EVALUATE    CHKSND-NYUGAIKBN
               WHEN    "1"
                   MOVE    "入"       TO  B01-NYUGAIKBN
               WHEN    "2"
                   MOVE    "外"       TO  B01-NYUGAIKBN
               WHEN    OTHER
                   MOVE    "　"       TO  B01-NYUGAIKBN
               END-EVALUATE
      *
               MOVE    CHKSND-SRYCD   TO  B01-SRYCD1
               MOVE    WRK-SRYNAME    TO  B01-NAME1
           END-IF
      *
           EVALUATE    CHKSND-COMPKBN
           WHEN    "2"
               MOVE    "完"           TO  B01-COMPKBN
               MOVE    1              TO  FLG-KANZEN
           WHEN    "3"
               MOVE    "前"           TO  B01-COMPKBN
               MOVE    1              TO  FLG-ZENPO
           WHEN    "4"
               MOVE    "後"           TO  B01-COMPKBN
               MOVE    1              TO  FLG-KOHO
           WHEN    OTHER
               MOVE    "　"           TO  B01-COMPKBN
           END-EVALUATE
      *
           MOVE    CHKSND-CD       TO  WRK-KEY-CD-G
           MOVE    1               TO  WRK-PNT
      *
           IF    ( WRK-KEY-CD (2)  =   SPACE )
               MOVE    WRK-KEY-CD (1)  TO  B01-BYOMEICD
               MOVE    CHKSND-BYOMEI   TO  B01-BYOMEI
           ELSE
               PERFORM 2001-MEISAI-CHKSND-SYORI-SEC
           END-IF
      *
           INSPECT B01-NAME1   REPLACING   ALL "  "  BY  "　"
      *
           MOVE    BODY-01         TO  HCM44-MOJI (IDX-LINE)
           PERFORM 2200-PAGE-FEED-CHK-SEC
      *
           PERFORM 900-CHKSND-FET-SEC
      *
           .
       200-MEISAI-CHKSND-SYORI-EXT.
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
      *    INSPECT B01-NAME1   REPLACING   ALL "  "  BY  "　"
      *
      *    MOVE    BODY-01         TO  HCM44-MOJI (IDX-LINE)
      *    PERFORM 2200-PAGE-FEED-CHK-SEC
      *
      *    INITIALIZE                  BODY-01
      *    MOVE    1               TO  WRK-PNT
      *    MOVE    1               TO  FLG-MEISAI
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
               INSPECT B01-NAME1   REPLACING   ALL "  "  BY  "　"
      *
               MOVE    BODY-01 TO  HCM44-MOJI (IDX-LINE)
               PERFORM 2200-PAGE-FEED-CHK-SEC
               INITIALIZE          BODY-01
               MOVE    1       TO  WRK-PNT
               MOVE    1       TO  FLG-MEISAI
           END-IF
      *
           IF    ( FLG-MEISAI      =   ZERO )
               STRING  WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
               INTO    B01-TEKIYO
               WITH    POINTER     WRK-PNT
               END-STRING
           ELSE
               IF    ( FLG-TEKCD   =   ZERO )
                   STRING  WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
                   INTO    B01-MEISAI
                   WITH    POINTER     WRK-PNT
                   END-STRING
               ELSE
                   STRING  WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
                           " "                     DELIMITED   BY  SIZE
                   INTO    B01-MEISAI
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
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-CHK-MAX )
      *
               IF    ( CHK-CD (IDX1)   NOT =   SPACE )
      *
                   COMPUTE CNT-TEKCD   =   CNT-TEKCD   +   1
      *
                   INITIALIZE          BODY-01
      *
                   IF    ( CNT-TEKCD   =   1 )
                       MOVE    CHK-SRYCD   TO  B01-SRYCD1
                       MOVE    WRK-SRYNAME TO  B01-NAME1
                   END-IF
      *
                   MOVE    CHK-CD (IDX1)   TO  B01-SRYCD2
      *
                   MOVE    CHK-CD (IDX1)   TO  WRK-TNS-SRYCD
                   PERFORM 2001-SRYCD-HEN-SEC
                   MOVE    WRK-NAME-G      TO  B01-NAME2
      *
                   INSPECT B01-NAME1   REPLACING   ALL "  "  BY  "　"
      *
                   MOVE    BODY-01         TO  HCM44-MOJI (IDX-LINE)
      *
                   PERFORM 2200-PAGE-FEED-CHK-SEC
      *
               END-IF
      *
           END-PERFORM
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
               IF     (TNS-YUKOSTYMD  <=  SPA-SYSYMD )  AND
                      (TNS-YUKOEDYMD  >=  SPA-SYSYMD )
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
           IF        ( HXLST-CHKKBN    =   "1" OR "2" OR "6" 
                                        OR "8" OR "9" )
               MOVE    SPACE           TO  H02-MIDASI2R2
               MOVE    1               TO  WRK-TGPNT
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   CONST-TOTUGO-MES-MAX )
      *
                   IF    ( FLG-TOTUGO (IDX2)   =   1 )
                       STRING CONST-TOTUGO-MES (IDX2)
                                           DELIMITED   BY  SPACE
                              "　"         DELIMITED   BY  SIZE
                       INTO   H02-MIDASI2R2
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
           MOVE   SPA-HOSPNUM          TO  HC01PARA-HOSPNUM
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
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
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
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
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
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
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
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               INITIALIZE                  CHKSND-REC
               MOVE    1               TO  FLG-CHKSND
           END-IF
      *
           .
       900-CHKSND-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
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
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               INITIALIZE                  CHKTRD-REC
               MOVE    1               TO  FLG-CHKTRD
           END-IF
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
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK005-REC
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
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK005-REC
           ELSE
               INITIALIZE                  CHK005-REC
               MOVE    1               TO  FLG-CHK005
           END-IF
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
