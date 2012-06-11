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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGD04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : チェック内容確認画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07.02.22    太田          新規作成
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
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    データチェック共通パラメタ
           COPY    "D01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "D04SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-CHKSND                  PIC 9(01).
           03  FLG-CHK004                  PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTBYOMEI                PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-LOOPEND                 PIC 9(01).
           03  FLG-HIZUKE-ERR              PIC 9(01).
           03  FLG-ERRLST-ROW              PIC 9(01).
           03  FLG-BYOLST-ROW              PIC 9(01).
           03  FLG-CHKLST-ROW              PIC 9(01).
           03  FLG-CHKREIGAI               PIC 9(01).
           03  FLG-INPUTCD                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-2001                    PIC 9(05).
           03  CNT-RENNUM                  PIC 9(05).
           03  CNT-CHK004                  PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDXL                        PIC 9(05).
           03  IDX-LST                     PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-HKN                     PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM             PIC X(200).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-YMD                     PIC X(10).
           03  WRK-SYMD                    PIC X(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTYMD3                 PIC X(22).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-DIDMSG                  PIC X(200).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-PTNUM                   PIC X(20).
           03  WRK-DELKBNCD                PIC X(08).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD-KBN           PIC X(01).
               05  WRK-KBNCD-SEQ           PIC 9(07).
           03  WRK-ZZZZ9                   PIC ZZZZ9.
           03  WRK-ERRLST-SEL              PIC 9(05).
           03  WRK-BYOLST-SEL              PIC 9(05).
           03  WRK-MOTOPG                  PIC X(05).
           03  WRK-SELKBNCD                PIC X(08).
           03  WRK-NAME                    PIC X(100).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-KJNYMD                  PIC X(08).
           03  WRK-TENKIYMD                PIC X(08).
           03  WRK-OFFSET                  PIC 9(08).
      *
       01  WRK-ROW-AREA.
           03  WRK-ERRLST-ROW              PIC S9(9).
           03  WRK-BYOLST-ROW              PIC S9(9).
           03  WRK-CHKLST-ROW              PIC S9(9).
      *
       01  CONST-AREA.
           03  CONST-ERRLST-MAX            PIC 9(05)   VALUE   200.
           03  CONST-BYOLST-MAX            PIC 9(05)   VALUE   300.
           03  CONST-CHKLST-MAX            PIC 9(05)   VALUE   200.
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      *
           COPY "CPLNKD04.INC".
      *
           COPY "COMMON-SPA"               REPLACING //SPA-//
                                           BY        //WKSPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
           COPY    "CPCHK004.INC".
           COPY    "CPCK1200.INC".
           COPY    "CPCK1201.INC".
           COPY    "CPCK2000.INC".
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK1010.INC".
           COPY    "CPSK1101.INC".
      *
           COPY    "CPCHKREIGAI.INC".
      *
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "CPTENSU.INC".
      *
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPDSUB01.INC".
      *
           COPY    "CPSENDX01.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    保険組み合せ取得
           COPY    "CPORCSHKNSET.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "D01.INC".
           COPY    "D02.INC".
           COPY    "D03.INC".
           COPY    "D04.INC".
           COPY    "D05.INC".
           COPY    "D99.INC".
           COPY    "DERR.INC".
           COPY    "DID1.INC".
           COPY    "DID2.INC".
      *
      *
       PROCEDURE                       DIVISION    USING
                                                   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
                                           IDX-AREA
                                           WRK-AREA
                                           CNT-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-WORK            TO  SPA-D01KYOUTU
           MOVE    SPA-FREE            TO  SPA-D04FREE
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           PERFORM 800-MCNDATE-SEC
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
               PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-D01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-D04FREE         TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "DERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
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
           MOVE    "D04    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
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
      *
               EVALUATE    SPA-MOTOPG
               WHEN    "X01"
               WHEN    "C02"
                   PERFORM 800-LOAD-D04SPA-SEC
                   MOVE    DSUB01-SCRSPA-AREA
                                           TO  SPA-D04
                   MOVE    DSUB01-COMMON   TO  SPA-D01KYOUTU
                   IF    ( SPA-MOTOPG      =   "X01" )
                       MOVE    SPACE       TO  SPA-GMN-BYOMEI
                       MOVE    ZERO        TO  SPA-GMN-BYOLST-SELNUM
                                               SPA-GMN-BYOLST-SEL
                       INITIALIZE              SPA-GMN-CHKLST-AREA
                                               SPA-NAI-CHKLST-AREA
                       PERFORM 3101-CHKLST-SEC
                   ELSE
                       IF    ( SPA-NAI-PTID    NOT =   ZERO )
                           INITIALIZE          SPA-GMN-BYOLST-AREA
                                               SPA-NAI-BYOLST-AREA
                           MOVE    SPACE       TO  SPA-GMN-BYOMEI
                           PERFORM 3101-BYOLST-SEC
                       END-IF
                   END-IF
      *
                   IF    ( SPA-MOTOPG  =   "X01" )
                       MOVE    4       TO  SPA-GMN-CUR
                   END-IF
                   GO  TO  300-SCREEN-EXT
               END-EVALUATE
      *
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    例外一覧画面
           WHEN    "D05"
               PERFORM 3102-ERRLST-SEC
               GO  TO  300-SCREEN-EXT
      *    確認画面１
           WHEN    "DID1"
               PERFORM 330-DID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-D04
      *
      *    コンボボックス編集処理
           PERFORM 3101-COMBO-EDIT-SEC
      *
           INITIALIZE                  SYS-1010-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    SPA-SYSYMD      TO  SYS-1010-STYUKYMD
                                       SYS-1010-EDYUKYMD
           MOVE    SPA-OPID        TO  SYS-1010-USERID
           MOVE    SYS-1010-REC    TO  SYSKANRI-REC
           STRING  SYS-1010-USERID DELIMITED    BY SIZE
                   "%"             DELIMITED    BY SIZE
           INTO    SYS-KANRITBL
           END-STRING
           PERFORM 900-SYSKANRI-KEY3-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1010-REC
           MOVE    SYS-1010-GSRAUTH(22:1)
                                   TO  SPA-NAI-GSRAUTH-22
           MOVE    SYS-1010-GSRAUTH(63:1)
                                   TO  SPA-NAI-GSRAUTH-103
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1101"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1101-REC
           IF    ( SYS-1101-REIGAICHKKBN   =   1 )
               MOVE    "1"         TO  SPA-NAI-REIGAICHKKBN
           END-IF
      *
           MOVE    1               TO  SPA-GMN-ERRLST-PAGE
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2000"          TO  CHK004-KANRICD
           MOVE    "4"             TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY-SEL-SEC
           IF    ( FLG-CHK004      =   ZERO )
               MOVE    CHK004-REC      TO  CK4-2000-REC
               MOVE    CK4-2000-TBL    TO  ORCBSD01PARA
               MOVE    ORCBSD01-SRYYM  TO  SPA-NAI-SRYYM
               MOVE    ORCBSD01-NYUGAIKBN
                                       TO  SPA-NAI-NYUGAIKBN
               MOVE    SPA-NAI-SRYYM       TO  WRK-YMD
               MOVE    "01"                TO  WRK-YMD(7:2)
               PERFORM 800-SEIWA-HEN-SEC
               STRING  WRK-EDTYMD3 (1:16)  DELIMITED   BY  SIZE
                       "診療分"            DELIMITED   BY  SIZE
               INTO    SPA-GMN-LBLSRYYM
               END-STRING
      *
               IF    ( SPA-NAI-NYUGAIKBN   =   "1" )
                   MOVE    "入院"          TO  SPA-GMN-LBLNYUGAI
               ELSE
                   MOVE    "外来"          TO  SPA-GMN-LBLNYUGAI
               END-IF
      *
               MOVE    "00"            TO  WRK-CMB-CD
               PERFORM 4200-CHKKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-CHKKBN-G
      *
               PERFORM 3101-ERRLST-SEC
           END-IF
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象コンボ編集処理
      *****************************************************************
       3101-COMBO-EDIT-SEC             SECTION.
      *
           PERFORM 3101-CHKKBNLST-SEC
      *
           PERFORM 3101-BEHAVELST-SEC
      *
           .
       3101-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分コンボ編集処理
      *****************************************************************
       3101-CHKKBNLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-CHKKBNLST-G
           MOVE    "00"                TO  SPA-GMN-CHKKBNL    (1)
           MOVE    "全て"              TO  SPA-GMN-CHKKBNMEIL (1)
           MOVE    "08"                TO  SPA-GMN-CHKKBNL    (2)
           MOVE    "薬剤と病名"        TO  SPA-GMN-CHKKBNMEIL (2)
           MOVE    "09"                TO  SPA-GMN-CHKKBNL    (3)
           MOVE    "診療行為と病名"    TO  SPA-GMN-CHKKBNMEIL (3)
      *
           MOVE    3                   TO  SPA-GMN-CHKKBN-MAX
      *
           .
       3101-CHKKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象外コンボ編集処理
      *****************************************************************
       3101-BEHAVELST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-BEHAVELST-G
           MOVE    "1"                 TO  SPA-GMN-BEHAVEL    (1)
           MOVE    "チェックしない（当月）"
                                       TO  SPA-GMN-BEHAVEMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-BEHAVEL    (2)
           MOVE    "チェックしない"
                                       TO  SPA-GMN-BEHAVEMEIL (2)
      *
           MOVE    2                   TO  SPA-GMN-BEHAVE-MAX
      *
           .
       3101-BEHAVELST-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象外コンボ編集処理
      *****************************************************************
       3102-BEHAVELST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-BEHAVELST-G
           MOVE    "0"                 TO  SPA-GMN-BEHAVEL    (1)
           MOVE    "チェックしないを解除"
                                       TO  SPA-GMN-BEHAVEMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-BEHAVEL    (2)
           MOVE    "チェックしない（当月）"
                                       TO  SPA-GMN-BEHAVEMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-BEHAVEL    (3)
           MOVE    "チェックしない"
                                       TO  SPA-GMN-BEHAVEMEIL (3)
      *
           MOVE    3                   TO  SPA-GMN-BEHAVE-MAX
      *
           .
       3102-BEHAVELST-EXT.
           EXIT.
      *****************************************************************
      *    エラー内容コラムリスト編集処理
      *****************************************************************
       3101-ERRLST-SEC                 SECTION.
      *
           MOVE    1               TO  FLG-ERRLST-ROW
      *
           INITIALIZE                  SPA-GMN-PTINF-AREA
                                       SPA-NAI-PTINF-AREA
                                       SPA-GMN-PTINFERR-AREA
                                       SPA-NAI-PTINFERR-AREA
      *
           INITIALIZE                  SPA-GMN-ERRLST-AREA
                                       SPA-NAI-ERRLST-AREA
           MOVE    ZERO            TO  IDX-LST
      *
           INITIALIZE                  CHK004-REC
           MOVE    "1200"          TO  CHK004-KANRICD
           MOVE    "______"        TO  CHK004-KBNCD (1:6)
           IF    ( SPA-GMN-CHKKBN  =   "00" )
               MOVE    "__"            TO  CHK004-KBNCD (7:2)
           ELSE
               MOVE    SPA-GMN-CHKKBN  TO  CHK004-KBNCD (7:2)
           END-IF
           MOVE    SPA-SYSYMD          TO  CHK004-STYUKYMD
                                           CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY7-SEL-SEC
           MOVE    CHK004-REC      TO  CK4-1200-REC
      *
           MOVE    ZERO                TO  CNT-CHK004
           COMPUTE WRK-OFFSET
               = ( SPA-GMN-ERRLST-PAGE - 1 )   *   CONST-ERRLST-MAX
      *
           PERFORM UNTIL ( FLG-CHK004  NOT  =  ZERO )
                    OR   ( CNT-CHK004      >=  WRK-OFFSET )
      *
               COMPUTE CNT-CHK004
                   =   CNT-CHK004      +   1
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
               MOVE    CHK004-REC  TO  CK4-1200-REC
      *
           END-PERFORM
      *
           PERFORM UNTIL ( FLG-CHK004      NOT  =  ZERO )
                    OR   ( SPA-GMN-ERRLST-MAX  >=  CONST-ERRLST-MAX )
      *
               COMPUTE IDX-LST  =  IDX-LST     +   1
      *
               COMPUTE SPA-GMN-TNUM (IDX-LST)
                   = ( SPA-GMN-ERRLST-PAGE - 1 )   *   CONST-ERRLST-MAX
                   +   IDX-LST
      *
               MOVE    IDX-LST  TO SPA-GMN-ERRLST-MAX
      *
               MOVE    CK4-1200-PTID   TO  WRK-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               PERFORM 900-PTNUM-KEY-SEL-SEC
      *
               MOVE    CK4-1200-PTID   TO  SPA-NAI-TPTID  (IDX-LST)
               MOVE    PTNUM-PTNUM     TO  SPA-GMN-TPTNUM (IDX-LST)
               MOVE    PTINF-NAME      TO  SPA-GMN-TNAME  (IDX-LST)
      *
               EVALUATE    CK4-1200-HKNKBN
               WHEN    "1"
                   MOVE    "社保"          TO  SPA-GMN-THKNSBT(IDX-LST)
               WHEN    "2"
                   MOVE    "国保"          TO  SPA-GMN-THKNSBT(IDX-LST)
               WHEN    "3"
                   MOVE    "労災"          TO  SPA-GMN-THKNSBT(IDX-LST)
               WHEN    "4"
                   MOVE    "自賠責"        TO  SPA-GMN-THKNSBT(IDX-LST)
               WHEN    "5"
                   MOVE    "自費"          TO  SPA-GMN-THKNSBT(IDX-LST)
               WHEN    "6"
                   MOVE    "公害"          TO  SPA-GMN-THKNSBT(IDX-LST)
               WHEN    "7"
                   MOVE    "後期高齢者"    TO  SPA-GMN-THKNSBT(IDX-LST)
               END-EVALUATE
      *
               MOVE    CK4-1200-ERRMSG TO  SPA-GMN-TERRMSG (IDX-LST)
               MOVE    CK4-1200-KBNCD  TO  SPA-NAI-TKBNCD  (IDX-LST)
               MOVE    CK4-1200-ERRCD  TO  SPA-NAI-TERRCD  (IDX-LST)
               EVALUATE    SPA-NAI-TERRCD(IDX-LST)
               WHEN    "080001"
               WHEN    "080002"
               WHEN    "080003"
               WHEN    "080004"
                   MOVE    "0801"      TO  SPA-NAI-TERRKBN (IDX-LST)
               WHEN    "090001"
               WHEN    "090002"
               WHEN    "090003"
               WHEN    "090004"
                   MOVE    "0901"      TO  SPA-NAI-TERRKBN(IDX-LST)
               END-EVALUATE
      *
      *
               INITIALIZE                      CHK004-REC
               MOVE    "1201"              TO  CHK004-KANRICD
               MOVE    SPA-NAI-TKBNCD(IDX-LST)
                                           TO  CHK004-KBNCD
               MOVE    SPA-SYSYMD          TO  CHK004-STYUKYMD
                                               CHK004-EDYUKYMD
               PERFORM 900-CHK004-KEY-SEL-SEC
               MOVE    CHK004-REC          TO  CK4-1201-REC
      *
               PERFORM 900-CHKREIGAI-KEY5-SEL-SEC
               IF    ( FLG-CHKREIGAI   =   ZERO )
                   IF    ( CHKREIGAI-STYUKYMD  =   "00000000" )
                       MOVE    "2"         TO  SPA-GMN-TREIGAI(IDX-LST)
                   ELSE
                       MOVE    "1"         TO  SPA-GMN-TREIGAI(IDX-LST)
                   END-IF
               END-IF
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
               MOVE    CHK004-REC  TO  CK4-1200-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-CHK004      =   ZERO )
               MOVE    1           TO  SPA-GMN-ERRLST-NEXT
           END-IF
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       3101-ERRLST-EXT.
           EXIT.
      *****************************************************************
      *    エラー内容コラムリスト編集処理
      *****************************************************************
       3102-ERRLST-SEC                 SECTION.
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   CONST-ERRLST-MAX )
      *
               MOVE    SPACE               TO  SPA-GMN-TREIGAI(IDX-LST)
      *
               INITIALIZE                      CHK004-REC
               MOVE    "1201"              TO  CHK004-KANRICD
               MOVE    SPA-NAI-TKBNCD(IDX-LST)
                                           TO  CHK004-KBNCD
               MOVE    SPA-SYSYMD          TO  CHK004-STYUKYMD
                                               CHK004-EDYUKYMD
               PERFORM 900-CHK004-KEY-SEL-SEC
               MOVE    CHK004-REC          TO  CK4-1201-REC
               PERFORM 900-CHKREIGAI-KEY5-SEL-SEC
               IF    ( FLG-CHKREIGAI   =   ZERO )
                   IF    ( CHKREIGAI-STYUKYMD  =   "00000000" )
                       MOVE    "1"         TO  SPA-GMN-TREIGAI(IDX-LST)
                   ELSE
                       MOVE    "2"         TO  SPA-GMN-TREIGAI(IDX-LST)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       3102-ERRLST-EXT.
           EXIT.
      *****************************************************************
      *    患者病名コラムリスト編集処理
      *****************************************************************
       3101-BYOLST-SEC                 SECTION.
      *
           IF  ( SPA-NAI-BYOLST-PTID =   SPA-NAI-PTID )
               CONTINUE
           ELSE
      *
               MOVE    SPA-AREA            TO  WKSPA-AREA
               INITIALIZE                      ORCSHKNSETAREA
               MOVE    SPA-NAI-PTID        TO  WKSPA-PTID
               MOVE    SPA-NAI-BIRTHDAY    TO  WKSPA-BIRTHDAY
               MOVE    "2"                 TO  ORCSHKN-KBN
      *
               CALL    "ORCSHKNSET"        USING
                                           WKSPA-AREA
                                           ORCSHKNSETAREA
               IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
                   INITIALIZE              ORCSHKNSETAREA
               END-IF
      *
               INITIALIZE                  SPA-GMN-BYOLST-AREA
                                           SPA-NAI-BYOLST-AREA
                                           SPA-GMN-BYOMEI
      *
               MOVE    SPA-NAI-PTID    TO  SPA-NAI-BYOLST-PTID
      *
               MOVE    ZERO            TO  IDX-LST
      *
               MOVE    1               TO  FLG-BYOLST-ROW
               PERFORM 900-PTBYOMEI-KEY3-SEL-SEC
      *
               PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                        OR   ( SPA-GMN-BYOLST-MAX >=   CONST-BYOLST-MAX)
      *
                   COMPUTE IDX-LST =   IDX-LST +   1
                   MOVE    IDX-LST         TO  SPA-GMN-BYOLST-MAX
      *
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    PTBYO-SRYKA     TO  SYS-KBNCD
                   MOVE    PTBYO-SRYYMD    TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
                   MOVE    PTBYO-BYOMEI    TO  SPA-GMN-TBYOMEI (IDX-LST)
                   MOVE    SYS-1005-SRYKANAME3
                                           TO  SPA-GMN-TSRYKA  (IDX-LST)
                   MOVE    PTBYO-SRYYMD    TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD1     TO  SPA-GMN-TSRYYMD (IDX-LST)
      *
                   EVALUATE    PTBYO-TENKIKBN
                   WHEN "1"
                       MOVE    "治ゆ"  TO  SPA-GMN-TTENKIKBN (IDX-LST)
                   WHEN "2"
                       MOVE    "死亡"  TO  SPA-GMN-TTENKIKBN (IDX-LST)
                   WHEN "3"
                       MOVE    "中止"  TO  SPA-GMN-TTENKIKBN (IDX-LST)
                   WHEN "4"
                       MOVE    "移行"  TO  SPA-GMN-TTENKIKBN (IDX-LST)
                   END-EVALUATE
      *
                   MOVE    PTBYO-TENKIYMD  TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD1     TO  SPA-GMN-TTENKIYMD
                                                             (IDX-LST)
      *
                   IF    ( PTBYO-SYUBYOFLG =   "1" )
                       MOVE    "主"        TO  SPA-GMN-TSYUBYO
                                                             (IDX-LST)
                   END-IF
      *
                   EVALUATE    PTBYO-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"        TO  SPA-GMN-TNYUGAIKBN
                                                             (IDX-LST)
                   WHEN    "2"
                       MOVE    "外"        TO  SPA-GMN-TNYUGAIKBN
                                                             (IDX-LST)
                   END-EVALUATE
      *
                   IF    ( PTBYO-HKNCOMBI  =   ZERO )
                       CONTINUE
                   ELSE
                       PERFORM VARYING IDX-HKN FROM    1   BY  1
                               UNTIL ( IDX-HKN >   ORCSHKN-HKN-MAX )
                                OR   ( SPA-GMN-THKNCOMBI (IDX-LST)
                                           NOT =   SPACE )
      *
                           IF    ( PTBYO-HKNCOMBI
                                   =   ORCSHKN-GMN-HKNCOMBINUM(IDX-HKN))
                               MOVE    ORCSHKN-GMN-HKNCOMBIMEI(IDX-HKN)
                                        TO  SPA-GMN-THKNCOMBI (IDX-LST)
                               INITIALIZE   ORCSKANACHKAREA
                               MOVE    "1"  TO  KANACHK-SYORI
                               MOVE    SPA-GMN-THKNCOMBI (IDX-LST)
                                            TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               MOVE    KANACHK-MAE-INPUT
                                            TO  SPA-GMN-THKNCOMBI
                                                         (IDX-LST)
                           END-IF
      *
                       END-PERFORM
                   END-IF
      *
                   PERFORM 900-PTBYOMEI-KEY3-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       3101-BYOLST-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理
      *****************************************************************
       3101-CHKLST-SEC                 SECTION.
      *
           IF    ( SPA-NAI-CHKLST-CHKKBN   =   SPA-NAI-CHKKBN )
            AND  ( SPA-NAI-CHKLST-SRYCD    =   SPA-GMN-SRYCD )
               CONTINUE
           ELSE
      *
               INITIALIZE                      SPA-GMN-CHKLST-AREA
                                               SPA-NAI-CHKLST-AREA
      *
               MOVE    SPA-NAI-CHKKBN      TO  SPA-NAI-CHKLST-CHKKBN
               MOVE    SPA-GMN-SRYCD       TO  SPA-NAI-CHKLST-SRYCD
      *
               MOVE    ZERO                TO  IDX-LST
      *
               MOVE    1                   TO  FLG-CHKLST-ROW
      *
               MOVE    SPA-NAI-SRYYM       TO  WRK-KJNYMD
               MOVE    "01"                TO  WRK-KJNYMD(7:2)
               PERFORM 900-CHKSND-KEY2-SEL-SEC
      *
               PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                        OR   ( SPA-GMN-CHKLST-MAX >=   CONST-CHKLST-MAX)
      *
                   COMPUTE IDX-LST =   IDX-LST +   1
                   MOVE    IDX-LST         TO  SPA-GMN-CHKLST-MAX
      *
                   IF    ( CHKSND-CHKKBN   =   "1" OR "2" )
                       MOVE    CHKSND-GRPID    TO  SPA-GMN-TGRPID
                                                               (IDX-LST)
                   END-IF
      *
                   MOVE    CHKSND-BYOMEI       TO  SPA-GMN-TCHKBYOMEI
                                                               (IDX-LST)
      *
                   EVALUATE    CHKSND-COMPKBN
                   WHEN    "1"
                       MOVE    "部分"          TO  SPA-GMN-TCOMPKBN
                                                               (IDX-LST)
                   WHEN    "2"
                       MOVE    "完全"          TO  SPA-GMN-TCOMPKBN
                                                               (IDX-LST)
                   WHEN    "3"
                       MOVE    "前方"          TO  SPA-GMN-TCOMPKBN
                                                               (IDX-LST)
                   WHEN    "4"
                       MOVE    "後方"          TO  SPA-GMN-TCOMPKBN
                                                               (IDX-LST)
                   END-EVALUATE
      *
                   PERFORM 900-CHKSND-KEY2-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
       3101-CHKLST-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＤＩＤ１）ＯＫ処理
      *****************************************************************
       330-DID1-SET-SEC            SECTION.
      *
      *    IF    ( SPA-DID1-FLG    =   "OK" )
      *        EVALUATE    SPA-DIDCD
      *        WHEN    "6666"
      *            MOVE    "JOIN"      TO  MCP-PUTTYPE
      *            PERFORM 210-BACK-SYORI-SEC
      *        WHEN    OTHER
      *        END-EVALUATE
      *    END-IF
      *
           MOVE    SPACE               TO  SPA-DID1-FLG
           MOVE    SPACE               TO  SPA-DIDCD
      *
           .
       330-DID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
      *        戻る
               MOVE    SPACE           TO  SPA-DIDCD
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 220-CLEAR-SEC
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 240-D05-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 260-PREVPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 270-NEXTPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 280-C02-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM 290-X01-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 412-REIGAI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "CHKKBN"
               PERFORM 4100-CHKKBN-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "BYOSELNUM"
               PERFORM 4100-BYOSELNUM-CHK-SEC
           WHEN    ANY             ALSO    "ERRLST"
               PERFORM 4100-ERRLST-CHK-SEC
           WHEN    ANY             ALSO    "BYOLST"
               PERFORM 4100-BYOLST-CHK-SEC
      *    WHEN    ANY             ALSO    "CHKLST"
      *        PERFORM 4100-CHKLST-CHK-SEC
           WHEN    OTHER
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
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
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
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
           MOVE    D04-BEHAVE      TO  SPA-GMN-BEHAVE-G
           MOVE    D04-BYOMEI      TO  SPA-GMN-BYOMEI
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
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BEHAVE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BYOSELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BYOMEI-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分チェック処理
      *****************************************************************
       4100-CHKKBN-CHK-SEC             SECTION.
      *
           MOVE    D04-CHKKBN      TO  SPA-GMN-CHKKBN-G
      *
           PERFORM 4101-CHKKBN-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    1           TO  SPA-GMN-ERRLST-PAGE
               PERFORM 3101-ERRLST-SEC
           END-IF
      *
           .
      *
       4100-CHKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-ERRLST-SEL
      *
           MOVE    D04-SELNUM          TO  SPA-GMN-ERRLST-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-ERRLST-SEL  >   ZERO )
                   MOVE    WRK-ERRLST-SEL  TO  SPA-GMN-ERRLST-SEL
                   PERFORM 4110-PTINF-EDIT-SEC
                   MOVE    3           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-ERRLST-SEL
      *
           IF    ( SPA-GMN-ERRLST-SELNUM  >   ZERO )
               PERFORM VARYING IDX-LST     FROM    1   BY  1
                       UNTIL ( IDX-LST     >   SPA-GMN-ERRLST-MAX )
                        OR   ( WRK-ERRLST-SEL  >   ZERO )
      *
                   IF    ( SPA-GMN-ERRLST-SELNUM
                                       =   SPA-GMN-TNUM (IDX-LST))
                       MOVE    IDX-LST
                                       TO  WRK-ERRLST-SEL
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-ERRLST-SEL  =   ZERO )
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名選択番号チェック処理
      *****************************************************************
       4100-BYOSELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-BYOLST-SEL
      *
           MOVE    D04-BYOSELNUM       TO  SPA-GMN-BYOLST-SELNUM
      *
           PERFORM 4101-BYOSELNUM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-BYOLST-SEL  >   ZERO )
                   MOVE    WRK-BYOLST-SEL  TO  SPA-GMN-BYOLST-SEL
                   MOVE    SPA-GMN-TBYOMEI (SPA-GMN-BYOLST-SEL)
                                       TO  SPA-GMN-BYOMEI
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4100-BYOSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名選択番号チェック処理
      *****************************************************************
       4101-BYOSELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-BYOLST-SEL
      *
           IF    ( SPA-GMN-BYOLST-SELNUM  >   ZERO )
      *
               IF    ( SPA-GMN-BYOLST-SELNUM
                                       <= SPA-GMN-BYOLST-MAX )
                   MOVE    SPA-GMN-BYOLST-SELNUM
                                       TO  WRK-BYOLST-SEL
               ELSE
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-BYOSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック内容リストチェック処理
      *****************************************************************
       4100-ERRLST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-ERRLST-SEL
      *
           PERFORM VARYING IDXL FROM    1  BY  1
                   UNTIL ( IDXL >   SPA-GMN-ERRLST-MAX )
                    OR   ( SPA-GMN-ERRLST-SEL  >   ZERO )
      *
               IF    ( D04-ERRLST-SELECT (IDXL)    =   "T")
                   MOVE    IDXL        TO  SPA-GMN-ERRLST-SEL
                   MOVE    SPA-GMN-TNUM (IDXL)
                                       TO  D04-SELNUM
                   MOVE    3           TO  SPA-GMN-CUR
                   PERFORM 4100-SELNUM-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-ERRLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名リストチェック処理
      *****************************************************************
       4100-BYOLST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-BYOLST-SEL
      *
           PERFORM VARYING IDXL FROM    1  BY  1
                   UNTIL ( IDXL >   SPA-GMN-BYOLST-MAX )
                    OR   ( SPA-GMN-BYOLST-SEL  >   ZERO )
      *
               IF    ( D04-BYOLST-SELECT (IDXL)    =   "T")
                   MOVE    IDXL        TO  SPA-GMN-BYOLST-SEL
                                           D04-BYOSELNUM
                   MOVE    5           TO  SPA-GMN-CUR
                   PERFORM 4100-BYOSELNUM-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-BYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタリストチェック処理
      *****************************************************************
       4100-CHKLST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CHKLST-SEL
      *
           PERFORM VARYING IDXL FROM    1  BY  1
                   UNTIL ( IDXL >   SPA-GMN-CHKLST-MAX )
                    OR   ( SPA-GMN-CHKLST-SEL  >   ZERO )
      *
               IF    ( D04-CHKLST-SELECT (IDXL)    =   "T")
                   MOVE    IDXL     TO  SPA-GMN-CHKLST-SEL
                   MOVE    SPA-GMN-TCHKBYOMEI (IDXL)
                                       TO  SPA-GMN-BYOMEI
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-CHKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分チェック処理
      *****************************************************************
       4101-CHKKBN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-CHKKBN-G    =   SPACE )
                   MOVE    "0001"      TO  SPA-ERRCD
           ELSE
               MOVE    SPA-GMN-CHKKBN  TO  WRK-CMB-CD
               PERFORM 4200-CHKKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE   1            TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-CHKKBN-G
               END-IF
           END-IF
      *
      *
           .
      *
       4101-CHKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象外区分チェック処理
      *****************************************************************
       4101-BEHAVE-CHK-SEC             SECTION.
      *
           IF    ( SPA-NAI-REIGAICHKKBN    =   "1" )
               MOVE    SPA-GMN-BEHAVE  TO  WRK-CMB-CD
               PERFORM 4200-BEHAVELST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   IF    ( SPA-NAI-REIGAI  =   "1" OR "2" )
                       MOVE    "0"         TO  WRK-CMB-CD
                       PERFORM 4200-BEHAVELST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-BEHAVE-G
                   ELSE
                       MOVE    SPACE   TO  SPA-GMN-BEHAVE-G
                   END-IF
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-BEHAVE-G
               END-IF
           END-IF
      *
           .
      *
       4101-BEHAVE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       4101-BYOMEI-CHK-SEC             SECTION.
      *
           INITIALIZE   ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-BYOMEI      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING
                                   ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-BYOMEI
      *
           .
      *
       4101-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       4110-PTINF-EDIT-SEC             SECTION.
      *
           PERFORM 41101-PTERR-EDIT-SEC
      *
           PERFORM 3101-BYOLST-SEC
      *
           PERFORM 3101-CHKLST-SEC
      *
           .
      *
       4110-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       41101-PTERR-EDIT-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-PTINF-AREA
                                       SPA-NAI-PTINF-AREA
                                       SPA-GMN-PTINFERR-AREA
                                       SPA-NAI-PTINFERR-AREA
      *
           MOVE    SPA-GMN-ERRLST-SEL      TO  IDXL
      *
           MOVE    SPA-NAI-TPTID (IDXL)    TO  WRK-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           INITIALIZE                      CHK004-REC
           MOVE    "1201"              TO  CHK004-KANRICD
           MOVE    SPA-NAI-TKBNCD(IDXL)
                                       TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD          TO  CHK004-STYUKYMD
                                           CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY-SEL-SEC
           MOVE    CHK004-REC          TO  CK4-1201-REC
      *
           MOVE    SPA-GMN-ERRLST-SEL  TO  IDXL
      *
           MOVE    SPA-NAI-TPTID  (IDXL)   TO  SPA-NAI-PTID
           MOVE    SPA-GMN-TPTNUM (IDXL)   TO  SPA-GMN-PTNUM
           MOVE    SPA-GMN-TNAME  (IDXL)   TO  SPA-GMN-NAME
           MOVE    SPA-GMN-TERRMSG(IDXL)   TO  SPA-GMN-ERRMSG
           EVALUATE    PTINF-SEX
           WHEN    "1"
               MOVE    "男"    TO  SPA-GMN-SEX
           WHEN    "2"
               MOVE    "女"    TO  SPA-GMN-SEX
           END-EVALUATE
           MOVE    PTINF-BIRTHDAY          TO  SPA-NAI-BIRTHDAY
           MOVE    PTINF-BIRTHDAY          TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1 TO  SPA-GMN-BIRTHDAY
      *
           MOVE    SPA-NAI-TKBNCD(IDXL)(7:2)
                                       TO  SPA-NAI-CHKKBN
           MOVE    CK4-1201-SRYCD      TO  SPA-GMN-SRYCD
           MOVE    CK4-1201-SRYKA      TO  SPA-NAI-SRYKA
      *
           IF    ( CK4-1201-SRYCD(1:1) =   "S" OR "P" )
               MOVE    CK4-1201-SRYCD      TO  WRK-SRYCD
               PERFORM 900-INPUTCD-KEY2-SEL-SEC
               MOVE    ICD-DSPNAME         TO  SPA-GMN-SRYNAME
           ELSE
               MOVE    CK4-1201-SRYCD      TO  WRK-SRYCD
               MOVE    CK4-1201-LASTYMD    TO  WRK-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME            TO  SPA-GMN-SRYNAME
           END-IF
      *
           MOVE    SPA-NAI-TERRKBN(IDXL)
                                       TO  SPA-NAI-ERRKBN
           MOVE    SPA-GMN-TREIGAI(IDXL)
                                       TO  SPA-NAI-REIGAI
      *
           IF    ( SPA-NAI-REIGAI      =   "1" OR "2" )
               PERFORM 3102-BEHAVELST-SEC
           ELSE
               PERFORM 3101-BEHAVELST-SEC
           END-IF
      *
           MOVE    SPA-NAI-REIGAI      TO  WRK-CMB-CD
           PERFORM 4200-BEHAVELST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-BEHAVE-G
      *
           .
      *
       41101-PTERR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "D01"               TO  SPA-SAKIPG
           MOVE    "D04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    例外登録一覧画面遷移処理
      *****************************************************************
       240-D05-SENI-SEC                SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2401-D05-SENI-SEC
           END-IF
      *
           .
       240-D05-SENI-EXT.
           EXIT.
      *****************************************************************
      *    例外登録一覧画面遷移処理
      *****************************************************************
       2401-D05-SENI-SEC               SECTION.
      *
      *    ＳＰＡの内容を退避
           PERFORM 800-SAVE-D04SPA-SEC
      *
           INITIALIZE                      SPA-D04-INF-AREA
           MOVE    SPA-NAI-SRYYM       TO  SPA-D04-SRYYM
           MOVE    SPA-NAI-NYUGAIKBN   TO  SPA-D04-NYUGAIKBN
      *
           MOVE    SPA-D01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-D04FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGD05"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-WORK            TO  SPA-D01KYOUTU
           MOVE    SPA-FREE            TO  SPA-D04FREE
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "D04"               TO  SPA-MOTOPG
           MOVE    "D05"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "D05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2401-D05-SENI-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-PREVPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-ERRLST-PAGE >   1 )
      *
               INITIALIZE              SPA-GMN-ERRLST-AREA
                                       SPA-NAI-ERRLST-AREA
      *
               COMPUTE SPA-GMN-ERRLST-PAGE
                   =   SPA-GMN-ERRLST-PAGE     -   1
      *
               PERFORM 3101-ERRLST-SEC
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
           IF    ( SPA-GMN-ERRLST-NEXT  =   1 )
      *
               INITIALIZE              SPA-GMN-ERRLST-AREA
                                       SPA-NAI-ERRLST-AREA
      *
               COMPUTE SPA-GMN-ERRLST-PAGE
                   =   SPA-GMN-ERRLST-PAGE     +   1
      *
               PERFORM 3101-ERRLST-SEC
           END-IF
      *
           .
       270-NEXTPAGE-EXT.
           EXIT.
      *****************************************************************
      *    病名登録画面遷移処理
      *****************************************************************
       280-C02-SENI-SEC                SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2801-C02-SENI-SEC
           END-IF
      *
           .
       280-C02-SENI-EXT.
           EXIT.
      *****************************************************************
      *    病名登録画面遷移処理
      *****************************************************************
       2801-C02-SENI-SEC                SECTION.
      *
      *    ＳＰＡの内容を退避
           PERFORM 800-SAVE-D04SPA-SEC
      *
           MOVE    SPA-AREA            TO  SPA-AREA
           INITIALIZE                      SPA-KYOTUKEY
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
      *
           IF    ( SPA-NAI-PTID    =   ZERO )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-PTNUM   TO  SPA-PTNUM
               MOVE    SPA-NAI-PTID    TO  SPA-PTID
               MOVE    SPA-NAI-SRYKA   TO  SPA-SRYKA
               MOVE    SPA-GMN-NAME    TO  SPA-NAME
           END-IF
      *
           MOVE    "D04"               TO  SPA-MOTOPG
           MOVE    "C02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      LNK-D04-AREA
           MOVE    LNK-D04-AREA        TO  SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "C02"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2801-C02-SENI-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ登録画面遷移処理
      *****************************************************************
       290-X01-SENI-SEC                SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SRYCD       =   SPACE )
                   MOVE    "0007"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2901-X01-SENI-SEC
           END-IF
      *
           .
       290-X01-SENI-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ登録画面遷移処理
      *****************************************************************
       2901-X01-SENI-SEC               SECTION.
      *
      *    ＳＰＡの内容を退避
           PERFORM 800-SAVE-D04SPA-SEC
      *
           MOVE    "D04"               TO  SPA-MOTOPG
           MOVE    "X01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-AREA            TO  WKSPA-AREA
      *
           INITIALIZE                      SENDX01-AREA
           MOVE     "D04"              TO  SENDX01-MOTOPG
           MOVE     SPA-GMN-SRYCD      TO  SENDX01-SRYCD
           MOVE     SPA-NAI-SRYYM      TO  SENDX01-SRYYMD
           MOVE     "01"               TO  SENDX01-SRYYMD(7:2)
           EVALUATE    SPA-NAI-CHKKBN
           WHEN    "08"
               MOVE    "1"             TO  SENDX01-CHKKBN
           WHEN    "09"
               MOVE    "2"             TO  SENDX01-CHKKBN
           END-EVALUATE
           MOVE     SPA-GMN-BYOMEI     TO  SENDX01-BYOMEI
      *
           MOVE     SENDX01-AREA       TO  WKSPA-WORK
           MOVE     WKSPA-KYOUTU       TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "X01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2901-X01-SENI-EXT.
           EXIT.
      *****************************************************************
      *    例外登録処理
      *****************************************************************
       412-REIGAI-SEC                 SECTION.
      *
           IF    ( SPA-GMN-SRYCD  NOT =   SPACE )
               PERFORM  4121-REIGAI-SEC
           END-IF
      *
           .
      *
       412-REIGAI-EXT.
           EXIT.
      *****************************************************************
      *    例外処理
      *****************************************************************
       4121-REIGAI-SEC                 SECTION.
      *
           INITIALIZE                      CHKREIGAI-REC
           MOVE    SPA-HOSPNUM         TO  CHKREIGAI-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  CHKREIGAI-PTID
           MOVE    SPA-NAI-CHKKBN      TO  CHKREIGAI-CHKNUM
           MOVE    SPA-NAI-ERRKBN      TO  CHKREIGAI-ERRKBN
           MOVE    SPA-NAI-NYUGAIKBN   TO  CHKREIGAI-NYUGAIKBN
           MOVE    SPA-GMN-SRYCD       TO  CHKREIGAI-SRYCD
           IF    ( SPA-GMN-BEHAVE      =   "1" )
               MOVE    SPA-NAI-SRYYM       TO  CHKREIGAI-STYUKYMD
               MOVE    "01"                TO  CHKREIGAI-STYUKYMD(7:2)
               MOVE    CHKREIGAI-STYUKYMD  TO  CHKREIGAI-EDYUKYMD
           ELSE
               MOVE    "99999999"          TO  CHKREIGAI-STYUKYMD
               MOVE    "00000000"          TO  CHKREIGAI-EDYUKYMD
           END-IF
           MOVE    CHKREIGAI-REC       TO  MCPDATA-REC
           MOVE    "tbl_chkreigai"     TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC              =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
            AND  ( SPA-GMN-BEHAVE      =   "1" OR "2" )
               PERFORM 900-CHKREIGAI-KEY4-SEL-SEC
               COMPUTE CNT-RENNUM  =   CHKREIGAI-RENNUM    +   1
      *
               INITIALIZE                       CHKREIGAI-REC
               MOVE    SPA-HOSPNUM         TO   CHKREIGAI-HOSPNUM
               MOVE    SPA-NAI-PTID        TO   CHKREIGAI-PTID
               MOVE    CNT-RENNUM          TO   CHKREIGAI-RENNUM
               MOVE    SPA-NAI-CHKKBN      TO   CHKREIGAI-CHKNUM
               MOVE    SPA-NAI-ERRKBN      TO   CHKREIGAI-ERRKBN
               MOVE    SPA-NAI-NYUGAIKBN   TO   CHKREIGAI-NYUGAIKBN
               MOVE    SPA-GMN-SRYCD       TO   CHKREIGAI-SRYCD
               IF    ( SPA-GMN-BEHAVE      =    "1" )
                   MOVE    SPA-NAI-SRYYM   TO   CHKREIGAI-STYUKYMD
                   MOVE    "01"            TO   CHKREIGAI-STYUKYMD(7:2)
                   MOVE    CHKREIGAI-STYUKYMD   TO WRK-SYMD
                   PERFORM 800-GETUMATU-SEC
                   MOVE    WRK-SYMD        TO   CHKREIGAI-EDYUKYMD
               ELSE
                   MOVE    "00000000"      TO   CHKREIGAI-STYUKYMD
                   MOVE    "99999999"      TO   CHKREIGAI-EDYUKYMD
               END-IF
               MOVE    SMCNDATE-YMD        TO   CHKREIGAI-CREYMD
                                                CHKREIGAI-UPYMD
               MOVE    SMCNDATE-HMS        TO   CHKREIGAI-UPHMS


               MOVE    CHKREIGAI-REC       TO  MCPDATA-REC
               MOVE    "tbl_chkreigai"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC              =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "4002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 3101-ERRLST-SEC
      *
               INITIALIZE                  SPA-GMN-BYOLST-AREA
                                           SPA-NAI-BYOLST-AREA
                                           SPA-GMN-CHKLST-AREA
                                           SPA-NAI-CHKLST-AREA
               PERFORM 3101-BEHAVELST-SEC
           END-IF
      *
           .
       4121-REIGAI-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分チェック処理
      *****************************************************************
       4200-CHKKBNLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-CHKKBN-MAX )
                OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
                IF   ( WRK-CMB-CD  =   SPA-GMN-CHKKBNL (IDX-CMB))
                   MOVE    SPA-GMN-CHKKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                END-IF
      *
           END-PERFORM
      *
           .
       4200-CHKKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象外区分コンボチェック処理
      *****************************************************************
       4200-BEHAVELST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-BEHAVE-MAX )
                OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
                IF   ( WRK-CMB-CD  =   SPA-GMN-BEHAVEL (IDX-CMB))
                   MOVE    SPA-GMN-BEHAVELST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                END-IF
      *
           END-PERFORM
      *
           .
       4200-BEHAVELST-CHK-EXT.
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
           IF    ( SPA-DIDCD       NOT =   SPACE )
               PERFORM 520-DID1SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "D04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE   D04-ERRLST-ROW       TO  WRK-ERRLST-ROW
           MOVE   D04-BYOLST-ROW       TO  WRK-BYOLST-ROW
           MOVE   D04-CHKLST-ROW       TO  WRK-CHKLST-ROW
      *
           MOVE    SPACE               TO  D04
           INITIALIZE                      D04
      *
           IF    ( FLG-ERRLST-ROW      =   1 )
               MOVE    ZERO            TO  D04-ERRLST-ROW
                                           D04-ERRLST-ROWATTR
           ELSE
               MOVE    WRK-ERRLST-ROW  TO  D04-ERRLST-ROW
               MOVE    ZERO            TO  D04-ERRLST-ROWATTR
           END-IF
      *
           IF    ( FLG-BYOLST-ROW      =   1 )
               MOVE    ZERO            TO  D04-BYOLST-ROW
                                           D04-BYOLST-ROWATTR
           ELSE
               MOVE    WRK-BYOLST-ROW  TO  D04-BYOLST-ROW
               MOVE    ZERO            TO  D04-BYOLST-ROWATTR
           END-IF
      *
           IF    ( FLG-CHKLST-ROW      =   1 )
               MOVE    ZERO            TO  D04-CHKLST-ROW
                                           D04-CHKLST-ROWATTR
           ELSE
               MOVE    WRK-CHKLST-ROW  TO  D04-CHKLST-ROW
               MOVE    ZERO            TO  D04-CHKLST-ROWATTR
           END-IF
      *
           MOVE    "blue"              TO  D04-LBLSRYYM-STYLE
                                           D04-LBLNYUGAI-STYLE
           MOVE    SPA-GMN-LBLSRYYM    TO  D04-LBLSRYYM
           MOVE    SPA-GMN-LBLNYUGAI   TO  D04-LBLNYUGAI
           MOVE    SPA-GMN-CHKKBN-G    TO  D04-CHKKBN
           MOVE    SPA-GMN-ERRLST-SELNUM
                                       TO  D04-SELNUM
      *
           MOVE    SPA-GMN-ERRLST-MAX  TO  D04-ERRLST-COUNT
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-ERRLST-MAX )
               MOVE    SPA-GMN-TNUM (IDX-LST)
                                       TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9       TO  D04-TERRLSTNO (IDX-LST)
      *
               IF    ( SPA-GMN-TREIGAI(IDX-LST)    =   "1" OR "2" )
                   MOVE    "*"         TO  D04-TERRLSTNO (IDX-LST)(1:1)
               END-IF
      *
               MOVE    SPA-GMN-TPTNUM (IDX-LST)
                                       TO  D04-TPTNUM    (IDX-LST)
               MOVE    SPA-GMN-TNAME  (IDX-LST)
                                       TO  D04-TNAME     (IDX-LST)
               MOVE    SPA-GMN-THKNSBT(IDX-LST)
                                       TO  D04-THKNSBT   (IDX-LST)
               MOVE    SPA-GMN-TERRMSG (IDX-LST)
                                       TO  D04-TERRMSG   (IDX-LST)
      *
               IF    ( IDX-LST     =   SPA-GMN-ERRLST-SEL )
                   MOVE    "T"         TO  D04-ERRLST-SELECT (IDX-LST)
               ELSE
                   MOVE    "F"         TO  D04-ERRLST-SELECT (IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-CHKKBN-G    TO  D04-CHKKBN
           MOVE    SPA-GMN-CHKKBN-MAX  TO  D04-CHKKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   D04-CHKKBN-COUNT )
               MOVE    SPA-GMN-CHKKBNLST (IDX-CMB)
                                       TO  D04-CHKKBN-ITEM  (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-PTNUM       TO  D04-PTNUM
           MOVE    SPA-GMN-NAME        TO  D04-NAME
           MOVE    SPA-GMN-SEX         TO  D04-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  D04-BIRTHDAY
           MOVE    SPA-GMN-SRYCD       TO  D04-SRYCD
           MOVE    SPA-GMN-SRYNAME     TO  D04-SRYNAME
      *    MOVE    SPA-GMN-ERRMSG      TO  D04-ERRMSG
           MOVE    SPA-GMN-BYOLST-SELNUM
                                       TO  D04-BYOSELNUM
           MOVE    SPA-GMN-BYOMEI      TO  D04-BYOMEI
      *
           IF    ( SPA-NAI-REIGAICHKKBN    =   "1" )
               MOVE    SPA-GMN-BEHAVE-G    TO  D04-BEHAVE
               MOVE    SPA-GMN-BEHAVE-MAX  TO  D04-BEHAVE-COUNT
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   D04-BEHAVE-COUNT )
                   MOVE    SPA-GMN-BEHAVELST (IDX-CMB)
                                           TO  D04-BEHAVE-ITEM(IDX-CMB)
               END-PERFORM
           END-IF
      *
           IF   (  SPA-GMN-NAME        =   SPACE )
               MOVE    "患者病名"      TO  D04-BYOLST-TITLE
           ELSE
               STRING  SPA-GMN-NAME        DELIMITED   BY  SPACE
               INTO    D04-BYOLST-TITLE
               END-STRING
           END-IF
      *
           MOVE    SPA-GMN-BYOLST-MAX  TO  D04-BYOLST-COUNT
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-BYOLST-MAX )
               MOVE    IDX-LST         TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9       TO  D04-TBYOLSTNO (IDX-LST)
               MOVE    SPA-GMN-TBYOMEI (IDX-LST)
                                       TO  D04-TBYOMEI   (IDX-LST)
               MOVE    SPA-GMN-TSRYKA  (IDX-LST)
                                       TO  D04-TSRYKA    (IDX-LST)
               MOVE    SPA-GMN-TSRYYMD(IDX-LST)
                                       TO  D04-TSRYYMD   (IDX-LST)
               MOVE    SPA-GMN-TTENKIYMD (IDX-LST)
                                       TO  D04-TTENKIYMD (IDX-LST)
               MOVE    SPA-GMN-TTENKIKBN (IDX-LST)
                                       TO  SPA-GMN-TTENKIKBN (IDX-LST)
               MOVE    SPA-GMN-TSYUBYO (IDX-LST)
                                       TO  D04-TSYUBYO   (IDX-LST)
               MOVE    SPA-GMN-TNYUGAIKBN (IDX-LST)
                                       TO  D04-TNYUGAIKBN(IDX-LST)
               MOVE    SPA-GMN-THKNCOMBI (IDX-LST)
                                       TO  D04-THKNCOMBI(IDX-LST)
      *
               IF    ( IDX-LST     =   SPA-GMN-BYOLST-SEL )
                   MOVE    "T"         TO  D04-BYOLST-SELECT (IDX-LST)
               ELSE
                   MOVE    "F"         TO  D04-BYOLST-SELECT (IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-CHKLST-MAX  TO  D04-CHKLST-COUNT
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-CHKLST-MAX )
               MOVE    SPA-GMN-TGRPID (IDX-LST)
                                       TO  D04-TCHKBYOMEI
                                                  (IDX-LST)
               MOVE    SPA-GMN-TCHKBYOMEI (IDX-LST)
                                       TO  D04-TCHKBYOMEI
                                                  (IDX-LST) (3:)
               MOVE    SPA-GMN-TCOMPKBN   (IDX-LST)
                                       TO  D04-TCOMPKBN     (IDX-LST)
      *
               IF    ( IDX-LST     =   SPA-GMN-CHKLST-SEL )
                   MOVE    "T"         TO  D04-CHKLST-SELECT (IDX-LST)
               ELSE
                   MOVE    "F"         TO  D04-CHKLST-SELECT (IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
           .
      *
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  D04-B06-STATE
                                           D04-B07-STATE
                                           D04-B08-STATE
                                           D04-B09-STATE
                                           D04-B12-STATE
                                           D04-BEHAVE-STATE
                                           D04-BEHAVE-ITEM-STATE
      *
           IF    ( SPA-GMN-ERRLST-PAGE    <=   1 )
               MOVE    WIDGET-INSENSITIVE  TO  D04-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-ERRLST-NEXT     =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  D04-B07-STATE
           END-IF
      *
           IF    ( SPA-NAI-GSRAUTH-22      =   "1" )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  D04-B08-STATE
           END-IF
      *
           IF    ( SPA-NAI-GSRAUTH-103     =   "1" )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  D04-B09-STATE
           END-IF
      *
           IF    ( SPA-NAI-REIGAICHKKBN    =   "1" )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  D04-B12-STATE
                                               D04-BEHAVE-STATE
                                               D04-BEHAVE-ITEM-STATE
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
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5011-INPUT-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "CHKKBN"        TO  MCP-WIDGET
           WHEN    002
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    003
               MOVE    "BEHAVE"        TO  MCP-WIDGET
           WHEN    004
               MOVE    "BYOSELNUM"     TO  MCP-WIDGET
           WHEN    005
               MOVE    "BYOMEI"        TO  MCP-WIDGET
           WHEN    209
               MOVE    "B09"           TO  MCP-WIDGET
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
           WHEN    "CHKKBN"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "SELNUM"
               MOVE    003             TO  SPA-GMN-CUR
           WHEN    "BEHAVE"
               IF    ( SPA-GMN-BEHAVE-G    =   SPACE )
                   MOVE    004         TO  SPA-GMN-CUR
               ELSE
                   MOVE    212         TO  SPA-GMN-CUR
               END-IF
           WHEN    "BYOMEI"
               MOVE    209             TO  SPA-GMN-CUR
           WHEN    "BREIGAI"
               MOVE    002             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5011-INPUT-CUR-SKIP-SEC         SECTION.
      *
           IF    ( SPA-GMN-CUR     =   3 )
               IF    ( D04-BEHAVE-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    4       TO  SPA-GMN-CUR
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
               MOVE    "確認項目を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "確認項目入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "病名選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "エラー内容を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  DERR
           MOVE    SPA-ERRCD           TO  DERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  DERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "D04"               TO  SPA-MOTOPG
           MOVE    "DERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "DERR"              TO  MCP-WINDOW
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
       520-DID1SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-DIDMSG
      *
      *    EVALUATE    SPA-DIDCD
      *    WHEN    "6666"
      *        MOVE    "処理中です"    TO  WRK-DIDMSG
      *    END-EVALUATE
      *
           MOVE    SPACE               TO  DID1
           MOVE    SPA-DIDCD           TO  DID1-ID1CODE
           MOVE    WRK-DIDMSG          TO  DID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "D04"               TO  SPA-MOTOPG
           MOVE    "DID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "DID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-DID1SET-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-YMD             =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-YMD             TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
               INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    SPA保存処理
      *****************************************************************
       800-SAVE-D04SPA-SEC         SECTION.
      *
           INITIALIZE                  DSUB01-LNK
           MOVE    SPA-TERMID          TO  DSUB01-TERMID
           MOVE    "O"                 TO  DSUB01-KBN
           MOVE    SPA-D04             TO  DSUB01-SCRSPA-AREA
           MOVE    SPA-D01KYOUTU       TO  DSUB01-COMMON
      *
           CALL    "ORCGDSUB01"        USING   DSUB01-LNK
      *
           .
      *
       800-SAVE-D04SPA-EXT.
           EXIT.
      *****************************************************************
      *    SPA復元処理
      *****************************************************************
       800-LOAD-D04SPA-SEC         SECTION.
      *
           INITIALIZE                  DSUB01-LNK
           MOVE    SPA-TERMID      TO  DSUB01-TERMID
           MOVE    "I"             TO  DSUB01-KBN
           CALL    "ORCGDSUB01"    USING   DSUB01-LNK
      *
           .
      *
       800-LOAD-D04SPA-EXT.
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
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    患者情報検索(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    WRK-PTID        TO  PTINF-PTID
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
      *    患者番号情報検索処理(KEY)
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM          TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
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
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-CHK004-KEY7-FET-SEC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４FETCH処理
      *****************************************************************
       900-CHK004-KEY7-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
                                       FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHK004      NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "tbl_chk004"    TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHK004-REC
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( CHK004-KBNCD (7:2)  =   "08" OR "09" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   INITIALIZE              CHK004-REC
                   MOVE    1           TO  FLG-CHK004
               END-IF
      *
           END-PERFORM
      *
           .
       900-CHK004-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY5-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY6-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター検索（KEY）
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-KJNYMD      TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者病名検索処理
      *****************************************************************
       900-PTBYOMEI-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYOMEI
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-PTBYOMEI-KEY3-FET-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYOMEI-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者病名FETCH処理
      *****************************************************************
       900-PTBYOMEI-KEY3-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYOMEI
                                           FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   MOVE    "99999999"      TO  WRK-TENKIYMD
                   IF    ( PTBYO-TENKIYMD  NOT =   SPACE )
                       MOVE    PTBYO-TENKIYMD
                                           TO  WRK-TENKIYMD
                   END-IF
      *
                   IF    ( PTBYO-NYUGAIKBN =   SPACE
                                           OR  SPA-NAI-NYUGAIKBN )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( PTBYO-SRYYMD (1:6) <=  SPA-NAI-SRYYM )
                    AND  ( WRK-TENKIYMD (1:6) >=  SPA-NAI-SRYYM )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( PTBYO-REZEMM        >   ZERO )
                       MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                       IF    ( PTBYO-REZEMM    >   1 )
                           MOVE    "2"         TO  WRK-ZOGENPTN
                           COMPUTE WRK-ZOGEN   =   PTBYO-REZEMM    -   1
                           PERFORM 800-CALENDAR-SEC
                       END-IF
                       IF    ( SPA-NAI-SRYYM   >   WRK-SYMD(1:6))
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( PTBYO-REZEFLG  =   "1" )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   INITIALIZE                  PTBYOMEI-REC
                   MOVE    1               TO  FLG-PTBYOMEI
               END-IF
           END-PERFORM
      *
           .
       900-PTBYOMEI-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
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
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY3-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           EVALUATE    SPA-NAI-CHKKBN
           WHEN    "08"
               MOVE    "1"             TO  CHKSND-CHKKBN
           WHEN    "09"
               MOVE    "2"             TO  CHKSND-CHKKBN
           END-EVALUATE
           MOVE    SPA-GMN-SRYCD       TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理
      *****************************************************************
       900-CHKSND-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェック例外マスタ検索処理
      *****************************************************************
       900-CHKREIGAI-KEY4-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKREIGAI
      *
           INITIALIZE                  CHKREIGAI-REC
           MOVE    SPA-HOSPNUM     TO  CHKREIGAI-HOSPNUM
           MOVE    SPA-NAI-PTID    TO  CHKREIGAI-PTID
           MOVE    CHKREIGAI-REC   TO  MCPDATA-REC
           MOVE    "tbl_chkreigai" TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKREIGAI-REC
           ELSE
               INITIALIZE              CHKREIGAI-REC
               MOVE    1           TO  FLG-CHKREIGAI
           END-IF
      *
           MOVE    "tbl_chkreigai" TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKREIGAI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェック例外マスタ検索処理
      *****************************************************************
       900-CHKREIGAI-KEY5-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKREIGAI
      *
           INITIALIZE                      CHKREIGAI-REC
           MOVE    SPA-HOSPNUM         TO  CHKREIGAI-HOSPNUM
           MOVE    SPA-NAI-TPTID  (IDX-LST)
                                       TO  CHKREIGAI-PTID
           MOVE    SPA-NAI-TKBNCD (IDX-LST)(7:2)
                                       TO  CHKREIGAI-CHKNUM
           MOVE    SPA-NAI-TERRKBN(IDX-LST)
                                       TO  CHKREIGAI-ERRKBN
           MOVE    SPA-NAI-NYUGAIKBN   TO  CHKREIGAI-NYUGAIKBN
           MOVE    CK4-1201-SRYCD      TO  CHKREIGAI-SRYCD
           MOVE    SPA-NAI-SRYYM       TO  CHKREIGAI-STYUKYMD
           MOVE    "01"                TO  CHKREIGAI-STYUKYMD(7:2)
           MOVE    CHKREIGAI-STYUKYMD  TO  CHKREIGAI-EDYUKYMD
           MOVE    CHKREIGAI-REC       TO  MCPDATA-REC
           MOVE    "tbl_chkreigai"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKREIGAI-REC
           ELSE
               INITIALIZE              CHKREIGAI-REC
               MOVE    1           TO  FLG-CHKREIGAI
           END-IF
      *
           MOVE    "tbl_chkreigai" TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKREIGAI-KEY5-SEL-EXT.
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
           MOVE    WRK-SRYCD       TO  ICD-INPUTCD
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
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    ＤＢ検索処理(FETCHは行わない）
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
