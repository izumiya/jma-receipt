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
       PROGRAM-ID.                 ORCGI07.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 出産一時金画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04.04.00    NACL-太田     新規作成
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
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    入退院登録共通領域
           COPY    "I0COMMON-SPA".
      *
           COPY    "I07SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-BUNBEN                  PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-KYUSEIRRK               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-INFO                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-SYMD                    PIC X(10).
           03  WRK-HENYMDG                 PIC X(10).
           03  WRK-CMB-CD                  PIC X(05).
           03  WRK-CMB-G.
               05  WRK-CMB-ITM             PIC X(100).
           03  WRK-KANACHK-STR             PIC X(100).
           03  WRK-AGE-G.
               05  WRK-AGE-ZZ9             PIC ZZ9.
               05  WRK-AGE-LBL             PIC X(02).
           03  WRK-TEATE                   PIC 9(08).
           03  WRK-FTNTOTAL                PIC 9(08).
           03  WRK-I0IDMSG                 PIC X(80).
           03  WRK-NINPUFTN-MONEY          PIC 9(10).
           03  WRK-CREYMD                  PIC X(08).
           03  WRK-KYUSEINAME              PIC X(100).
           03  WRK-SKYYMD.
               05  WRK-SKYYM               PIC X(06).
               05  WRK-SKYDD               PIC X(02).
           03  WRK-BUNBENKBN               PIC X(01).
      *
      *
       01  CONST-AREA.
           03  CONST-H211001               PIC X(08)   VALUE "20091001".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
      *    旧姓履歴
       01  KYUSEIRRK-REC.
           COPY  "CPKYUSEIRRK.INC".
      *
      *    分娩管理
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    分娩負担金計算サブ
       01  LNKBUNBEN-REC.
           COPY  "CPBUNBEN.INC"        REPLACING  //BUNBEN-//
                                              BY  //LNKBUNBEN-//.
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCHCN63.INC".
           COPY    "CPONPRTDATA.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "I01.INC".
           COPY    "I02.INC".
           COPY    "I03.INC".
           COPY    "I04.INC".
           COPY    "I05.INC".
           COPY    "I06.INC".
           COPY    "I07.INC".
           COPY    "I08.INC".
           COPY    "I0ERR.INC".
           COPY    "I0ID1.INC".
           COPY    "I0ID2.INC".
           COPY    "I041.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I07FREE.
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I07FREE     TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "I0ERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
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
           MOVE    "I07"               TO  MCP-WINDOW
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
           EVALUATE    SPA-MOTOPG
           WHEN    "I0ID1"
               PERFORM 3001-I0ID1-SET-SEC
               MOVE   SPACE        TO  SPA-I0IDCD
           WHEN    "I08"
               PERFORM 3001-I08-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報変更画面ＯＫ処理
      *****************************************************************
       3001-I08-SET-SEC            SECTION.
      *
           IF    ( SPA-I08-BTN =   "B12" )
      *
               PERFORM 900-BUNBEN-KEY3-SEL-SEC
      *
               MOVE    BUNBEN-KANYUKBN     TO  SPA-GMN-SYAKOK
               MOVE    BUNBEN-HONKZKKBN    TO  SPA-GMN-HONKZK
               MOVE    BUNBEN-HKNJANUM     TO  SPA-GMN-HKNJANUM
               MOVE    BUNBEN-KIGO         TO  SPA-GMN-KIGO
               MOVE    BUNBEN-NUM          TO  SPA-GMN-NUM
               MOVE    BUNBEN-HKNJANUM-S   TO  SPA-NAI-HKNJANUM-S
      *
           END-IF
           .
       3001-I08-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（Ｉ５ＩＤ１）ＯＫ処理
      *****************************************************************
       3001-I0ID1-SET-SEC         SECTION.
      *
           IF    ( SPA-I0ID1-FLG   =   "OK" )
               EVALUATE    SPA-I0IDCD
               WHEN    "3001"
                   PERFORM 2201-TOUROKU-SEC
               END-EVALUATE
           END-IF
           .
       3001-I0ID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-I07
           INITIALIZE                  SPA-I07
      *
      *    コンボボックス編集処理
           PERFORM 310-COMBO-EDIT-SEC
      *
           PERFORM 800-ORCSBUNBENFTN-SEC
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
      *
           PERFORM 311-SPASET-SEC
      *
           MOVE    BUNBEN-RIYOUKBN     TO  WRK-CMB-CD
           PERFORM 4200-RIYOUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RIYOUKBN-G
           IF    ( SPA-GMN-RIYOUKBN-G  =   SPACE )
               MOVE    SPA-GMN-RIYOUKBNLST (2)
                                       TO  SPA-GMN-RIYOUKBN-G
           END-IF
      *
           IF    ( BUNBEN-SKYYM        =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-SKYYM
               MOVE    ZERO            TO  SPA-NAI-SKYYM
           ELSE
               MOVE    BUNBEN-SKYYM    TO  SPA-NAI-SKYYM
               MOVE    BUNBEN-SKYYM    TO  WRK-SYMD
               PERFORM 801-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG (1:6)
                                       TO  SPA-GMN-SKYYM
           END-IF
      *
           MOVE    BUNBEN-TEISYUTUBIKBN       TO  WRK-CMB-CD
           PERFORM 4200-TEISYUTUBIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TEISYUTUBIKBN-G
           IF    ( SPA-GMN-TEISYUTUBIKBN-G    =   SPACE )
               IF    ( SPA-GMN-RIYOUKBN       =   "1" )
                   MOVE    SPA-GMN-TEISYUTUBIKBNLST (1)
                                       TO  SPA-GMN-TEISYUTUBIKBN-G
               END-IF
           END-IF
      *
           MOVE    BUNBEN-SKYKBN       TO  WRK-CMB-CD
           PERFORM 4200-SKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKYKBN-G
           IF    ( SPA-GMN-SKYKBN-G    =   SPACE )
               MOVE    SPA-GMN-SKYKBNLST (2)
                                       TO  SPA-GMN-SKYKBN-G
           END-IF
      *
           MOVE    BUNBEN-BUNBENKBN    TO  WRK-CMB-CD
           PERFORM 4200-BUNBENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-BUNBENKBN-G
           IF    ( SPA-GMN-BUNBENKBN-G  =   SPACE )
               MOVE    SPA-GMN-BUNBENKBNLST (1)
                                       TO  SPA-GMN-BUNBENKBN-G
           END-IF
      *
           MOVE    BUNBEN-TEISYUTUSAKI TO SPA-GMN-TEISYUTUSAKI
      *
           MOVE    SPA-GMN-TEISYUTUSAKI TO WRK-CMB-CD
           PERFORM 4200-TEISYUTUSAKILST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-TEISYUTUSAKI-G
           IF    ( WRK-CMB-ITM             =   SPACE )
               PERFORM 3121-TEISYUTUSAKI-EDIT-SEC
               MOVE    SPA-GMN-TEISYUTUSAKI    TO  WRK-CMB-CD
               PERFORM 4200-TEISYUTUSAKILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-TEISYUTUSAKI-G
           END-IF
      *
           MOVE    BUNBEN-BIRTHDAY     TO  SPA-NAI-BIRTHDAY-BABY
           MOVE    BUNBEN-BIRTHDAY     TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY-BABY
      *
           MOVE    BUNBEN-ZAIWEEK      TO  SPA-GMN-ZAIWEEK
      *
           MOVE    BUNBEN-SIZANKBN     TO  WRK-CMB-CD
           PERFORM 4200-SIZANKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SIZANKBN-G
           IF    ( SPA-GMN-SIZANKBN-G  =   SPACE )
               MOVE    SPA-GMN-SIZANKBNLST (2)
                                       TO  SPA-GMN-SIZANKBN-G
           END-IF
      *
           MOVE    BUNBEN-BIRTHCNT     TO  SPA-GMN-BIRTHCNT
      *
           MOVE    BUNBEN-NYUIN-DAY    TO  SPA-GMN-NYUIN-DAY
      *
           MOVE    BUNBEN-SANKAKBN TO  WRK-CMB-CD
           PERFORM 4200-SANKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SANKAKBN-G
           IF    ( SPA-GMN-SANKAKBN-G      =   SPACE )
               IF    ( SPA-NAI-SANKANUM    =   ZERO )
                   MOVE    SPA-GMN-SANKAKBNLST (2)
                                   TO  SPA-GMN-SANKAKBN-G
               ELSE
                   MOVE    SPA-GMN-SANKAKBNLST (1)
                                   TO  SPA-GMN-SANKAKBN-G
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-MEIPRTKBNLST (2)
                                       TO  SPA-GMN-MEIPRTKBN-G
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
           PERFORM 900-PTNYUINRRK-KEY4-SEL-SEC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPACE               TO  WRK-KYUSEINAME
           PERFORM 900-KYUSEIRRK-KEY3-SEL-SEC
           PERFORM UNTIL ( FLG-KYUSEIRRK   NOT =   ZERO )
               MOVE    KYUSEI-NAME     TO  WRK-KYUSEINAME
               PERFORM 900-KYUSEIRRK-KEY3-FET-SEC
           END-PERFORM
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TAIINYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
           IF    ( SYS-1001-SANKANUM   IS  NOT NUMERIC )
            OR   ( SYS-1001-SANKANUM   =   ZERO    )
               MOVE    ZERO            TO  SPA-NAI-SANKANUM
           ELSE
               MOVE    SYS-1001-SANKANUM
                                       TO  SPA-NAI-SANKANUM
           END-IF
      *
           MOVE    BUNBEN-PTNUM        TO  SPA-GMN-PTNUM
           MOVE    LNKBUNBEN-NAME      TO  SPA-GMN-KANANAME
           IF    ( WRK-KYUSEINAME      =   SPACE )
               MOVE    PTINF-NAME      TO  SPA-GMN-NAME
           ELSE
               MOVE    WRK-KYUSEINAME  TO  SPA-GMN-NAME
           END-IF
           MOVE    PTNYUINRRK-NYUINYMD TO  SPA-NAI-NYUINYMD
           MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-NYUINYMD
           MOVE    PTNYUINRRK-TAIINYMD TO  SPA-NAI-TAIINYMD
           MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TAIINYMD
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY-MOM
      *
           INITIALIZE ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-I07-PTID        TO  AGECHK-PTID
           MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"            USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-GMN-AGE
           MOVE    BUNBEN-KANYUKBN     TO  SPA-GMN-SYAKOK
           MOVE    BUNBEN-HONKZKKBN    TO  SPA-GMN-HONKZK
           MOVE    BUNBEN-HKNJANUM     TO  SPA-GMN-HKNJANUM
           MOVE    BUNBEN-KIGO         TO  SPA-GMN-KIGO
           MOVE    BUNBEN-NUM          TO  SPA-GMN-NUM
      *
           MOVE    BUNBEN-NYU-MONEY    TO  SPA-GMN-NYU-MONEY
           MOVE    BUNBEN-RMSGK-MONEY  TO  SPA-GMN-RMSGK-MONEY
           MOVE    BUNBEN-KAIJO-MONEY  TO  SPA-GMN-KAIJO-MONEY
           MOVE    BUNBEN-BUNBEN-MONEY TO  SPA-GMN-BUNBEN-MONEY
           MOVE    BUNBEN-SHIN-MONEY   TO  SPA-GMN-SHIN-MONEY
           MOVE    BUNBEN-KNS-MONEY    TO  SPA-GMN-KNS-MONEY
           MOVE    BUNBEN-SYC-MONEY    TO  SPA-GMN-SYC-MONEY
           MOVE    BUNBEN-SNK-MONEY    TO  SPA-GMN-SNK-MONEY
           MOVE    BUNBEN-ETC-MONEY    TO  SPA-GMN-ETC-MONEY
           MOVE    BUNBEN-ICBFTN-MONEY TO  SPA-GMN-ICBFTN-MONEY
           MOVE    BUNBEN-NINPUFTN-MONEY
                                       TO  SPA-GMN-NINPUFTN-MONEY
           MOVE    BUNBEN-DAIRI-MONEY  TO  SPA-GMN-DAIRI-MONEY
           MOVE    BUNBEN-BIKOU        TO  SPA-GMN-BIKOU
           MOVE    BUNBEN-HKNJANUM-S   TO  SPA-NAI-HKNJANUM-S
      *
           IF    ( LNKBUNBEN-KANYUKBN  =   "2" )
               MOVE    "2"             TO  SPA-NAI-RECESAKI
           ELSE
               MOVE    "1"             TO  SPA-NAI-RECESAKI
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       311-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    提出先編集処理
      *****************************************************************
       3121-TEISYUTUSAKI-EDIT-SEC              SECTION.
      *
           IF    ( SPA-GMN-BUNBENKBN       =   "1" )
                   MOVE    "2"     TO  SPA-GMN-TEISYUTUSAKI
           ELSE
               IF    ( SPA-NAI-RECESAKI    =   "2" )
                   MOVE    "2"     TO  SPA-GMN-TEISYUTUSAKI
               ELSE
                   MOVE    "1"     TO  SPA-GMN-TEISYUTUSAKI
               END-IF
           END-IF
      *
           .
       3121-TEISYUTUSAKI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       310-COMBO-EDIT-SEC              SECTION.
      *
      *    直接支払制度利用区分
           PERFORM 3101-RIYOUKBNLST-SEC
      *
      *    提出日区分
           PERFORM 3101-TEISYUTUBIKBNLST-SEC
      *
      *    請求区分
           PERFORM 3101-SKYKBNLST-SEC
      *
      *    分娩区分
           PERFORM 3101-BUNBENKBNLST-SEC
      *
      *    提出先区分
           PERFORM 3101-TEISYUTUSAKILST-SEC
      *
      *
      *    死産有無区分
           PERFORM 3101-SIZANKBNLST-SEC
      *
      *    産科医療保障制度区分
           PERFORM 3101-SANKAKBNLST-SEC
      *
      *    内訳明細書印刷区分
           PERFORM 3101-MEIPRTKBNLST-SEC
      *
           .
       310-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分コンボ初期処理
      *****************************************************************
       3101-RIYOUKBNLST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-RIYOUKBNLST-G
           MOVE    "0"             TO  SPA-GMN-RIYOUKBNL   (1)
           MOVE    "利用しない"    TO  SPA-GMN-RIYOUKBNMEIL(1)
           MOVE    "1"             TO  SPA-GMN-RIYOUKBNL   (2)
           MOVE    "利用する"      TO  SPA-GMN-RIYOUKBNMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-RIYOUKBN-MAX
      *
           .
       3101-RIYOUKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    提出日区分コンボ初期処理
      *****************************************************************
       3101-TEISYUTUBIKBNLST-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-TEISYUTUBIKBNLST-G
           MOVE    "1"             TO  SPA-GMN-TEISYUTUBIKBNL   (1)
           MOVE    "１０日請求"    TO  SPA-GMN-TEISYUTUBIKBNMEIL(1)
           MOVE    "2"             TO  SPA-GMN-TEISYUTUBIKBNL   (2)
           MOVE    "２５日請求"    TO  SPA-GMN-TEISYUTUBIKBNMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-TEISYUTUBIKBN-MAX
      *
           .
       3101-TEISYUTUBIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    請求区分コンボ初期処理
      *****************************************************************
       3101-SKYKBNLST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-SKYKBNLST-G
           MOVE    "0"             TO  SPA-GMN-SKYKBNL   (1)
           MOVE    "請求しない"    TO  SPA-GMN-SKYKBNMEIL(1)
           MOVE    "1"             TO  SPA-GMN-SKYKBNL   (2)
           MOVE    "未請求"        TO  SPA-GMN-SKYKBNMEIL(2)
           MOVE    "2"             TO  SPA-GMN-SKYKBNL   (3)
           MOVE    "請求済"        TO  SPA-GMN-SKYKBNMEIL(3)
      *
           MOVE    3               TO  SPA-GMN-SKYKBN-MAX
      *
           .
       3101-SKYKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分区分コンボ初期処理
      *****************************************************************
       3101-BUNBENKBNLST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-BUNBENKBNLST-G
           MOVE    "1"             TO  SPA-GMN-BUNBENKBNL   (1)
           MOVE    "正常分娩"      TO  SPA-GMN-BUNBENKBNMEIL(1)
           MOVE    "2"             TO  SPA-GMN-BUNBENKBNL   (2)
           MOVE    "異常分娩"      TO  SPA-GMN-BUNBENKBNMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-BUNBENKBN-MAX
      *
           .
       3101-BUNBENKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    提出先コンボ初期処理
      *****************************************************************
       3101-TEISYUTUSAKILST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-TEISYUTUSAKILST-G
           MOVE    "1"             TO  SPA-GMN-TEISYUTUSAKIL   (1)
           MOVE    "支払基金"      TO  SPA-GMN-TEISYUTUSAKIMEIL(1)
           MOVE    "2"             TO  SPA-GMN-TEISYUTUSAKIL   (2)
           MOVE    "国保連合会"    TO  SPA-GMN-TEISYUTUSAKIMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-TEISYUTUSAKI-MAX
      *
           .
       3101-TEISYUTUSAKILST-EXT.
           EXIT.
      *****************************************************************
      *    死産有無区分コンボ初期処理
      *****************************************************************
       3101-SIZANKBNLST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-SIZANKBNLST-G
           MOVE    "1"             TO  SPA-GMN-SIZANKBNL   (1)
           MOVE    "死産"          TO  SPA-GMN-SIZANKBNMEIL(1)
           MOVE    "2"             TO  SPA-GMN-SIZANKBNL   (2)
           MOVE    "死産でない"    TO  SPA-GMN-SIZANKBNMEIL(2)
           MOVE    "3"             TO  SPA-GMN-SIZANKBNL   (3)
           MOVE    "混在"          TO  SPA-GMN-SIZANKBNMEIL(3)
      *
           MOVE    3               TO  SPA-GMN-SIZANKBN-MAX
      *
           .
       3101-SIZANKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    産科医療保障制度区分コンボ初期処理
      *****************************************************************
       3101-SANKAKBNLST-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-SANKAKBNLST-G
           MOVE    "1"                 TO  SPA-GMN-SANKAKBNL   (1)
           MOVE    "対象分娩"          TO  SPA-GMN-SANKAKBNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-SANKAKBNL   (2)
           MOVE    "対象分娩でない"    TO  SPA-GMN-SANKAKBNMEIL(2)
           MOVE    "3"                 TO  SPA-GMN-SANKAKBNL   (3)
           MOVE    "混在"              TO  SPA-GMN-SANKAKBNMEIL(3)
      *
           MOVE    3                   TO  SPA-GMN-SANKAKBN-MAX
      *
           .
       3101-SANKAKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    内訳明細書印刷区分コンボ初期処理
      *****************************************************************
       3101-MEIPRTKBNLST-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-MEIPRTKBNLST-G
           MOVE    "0"                 TO  SPA-GMN-MEIPRTKBNL   (1)
           MOVE    "発行しない"        TO  SPA-GMN-MEIPRTKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-MEIPRTKBNL   (2)
           MOVE    "発行する"          TO  SPA-GMN-MEIPRTKBNMEIL(2)
      *
           MOVE    2                   TO  SPA-GMN-MEIPRTKBN-MAX
      *
           .
       3101-MEIPRTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    保険情報変更画面遷移処理
      *****************************************************************
       320-I08-SENI-SEC              SECTION.
      *
           INITIALIZE                  SPA-I08-AREA
      *
           MOVE    SPA-I07-PTID    TO  SPA-I08-PTID
           MOVE    SPA-I07-RRKNUM  TO  SPA-I08-RRKNUM
      *
           MOVE    SPACE           TO  SPA-MOTOPG
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I07FREE     TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI08"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I07FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    "HKNJANUM"      TO  MCP-WIDGET
      *
           MOVE    "I07"           TO  SPA-MOTOPG
           MOVE    "I08"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "I08"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  FLG-END
      *
           .
       320-I08-SENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    WHEN    "CLICKED"       ALSO    "B02"
      *        PERFORM 310-SPASET-SEC
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM 320-I08-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B09"
               MOVE    "9999"      TO  SPA-ERRCD
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 220-TOUROKU-SEC
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
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
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
           MOVE    SPA-GMN-BUNBENKBN-G TO  WRK-BUNBENKBN
      *
           MOVE    I07-RIYOUKBN        TO  SPA-GMN-RIYOUKBN-G
           MOVE    I07-SKYYM           TO  SPA-GMN-SKYYM
           MOVE    I07-TEISYUTUBIKBN   TO  SPA-GMN-TEISYUTUBIKBN-G
           MOVE    I07-SKYKBN          TO  SPA-GMN-SKYKBN-G
           MOVE    I07-BUNBENKBN       TO  SPA-GMN-BUNBENKBN-G
           MOVE    I07-TEISYUTUSAKI    TO  SPA-GMN-TEISYUTUSAKI-G
           MOVE    I07-ZAIWEEK         TO  SPA-GMN-ZAIWEEK
           MOVE    I07-BIRTHDAY-BABY   TO  SPA-GMN-BIRTHDAY-BABY
           MOVE    I07-SIZANKBN        TO  SPA-GMN-SIZANKBN-G
           MOVE    I07-BIRTHCNT        TO  SPA-GMN-BIRTHCNT
           MOVE    I07-NYUIN-DAY       TO  SPA-GMN-NYUIN-DAY
           MOVE    I07-SANKAKBN        TO  SPA-GMN-SANKAKBN-G
           MOVE    I07-NYU-MONEY       TO  SPA-GMN-NYU-MONEY
           MOVE    I07-RMSGK-MONEY     TO  SPA-GMN-RMSGK-MONEY
           MOVE    I07-KAIJO-MONEY     TO  SPA-GMN-KAIJO-MONEY
           MOVE    I07-BUNBEN-MONEY    TO  SPA-GMN-BUNBEN-MONEY
           MOVE    I07-SHIN-MONEY      TO  SPA-GMN-SHIN-MONEY
           MOVE    I07-KNS-MONEY       TO  SPA-GMN-KNS-MONEY
           MOVE    I07-SYC-MONEY       TO  SPA-GMN-SYC-MONEY
           MOVE    I07-SNK-MONEY       TO  SPA-GMN-SNK-MONEY
           MOVE    I07-ETC-MONEY       TO  SPA-GMN-ETC-MONEY
           MOVE    I07-ICBFTN-MONEY    TO  SPA-GMN-ICBFTN-MONEY
           MOVE    I07-BIKOU           TO  SPA-GMN-BIKOU
           MOVE    I07-MEIPRTKBN       TO  SPA-GMN-MEIPRTKBN-G
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
               PERFORM 4101-RIYOUKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKYYM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TEISYUTUBIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKYKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BUNBENKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TEISYUTUSAKI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-ZAIWEEK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BIRTHDAY-BABY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SIZANKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BIRTHCNT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-NYUIN-DAY-CHK-SEC
           END-IF
      *
           IF    ( SPA-NAI-SANKANUM    NOT =   ZERO )
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-SANKAKBN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-NYU-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RMSGK-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KAIJO-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BUNBEN-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SHIN-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KNS-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SYC-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SNK-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-ETC-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-ICBFTN-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BIKOU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-MEIPRTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-NINPUFTN-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-DAIRI-MONEY-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分チェック処理
      *****************************************************************
       4101-RIYOUKBN-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-RIYOUKBN    TO  WRK-CMB-CD
           PERFORM 4200-RIYOUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RIYOUKBN-G
           IF    ( SPA-GMN-RIYOUKBN-G  =   SPACE )
               MOVE    SPA-GMN-RIYOUKBNLST (2)
                                       TO  SPA-GMN-RIYOUKBN-G
           END-IF
      *
           IF    ( SPA-GMN-RIYOUKBN    =   "0" )
            OR   ( SPA-GMN-HKNJANUM    =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-SKYYM
               MOVE    ZERO            TO  SPA-NAI-SKYYM
               MOVE    SPACE           TO  SPA-GMN-TEISYUTUBIKBN
               MOVE    "1"             TO  SPA-GMN-SKYKBN
           END-IF
      *
           .
       4101-RIYOUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求年月チェック処理
      *****************************************************************
       4101-SKYYM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-SKYYM
      *
           IF    ( SPA-GMN-SKYYM       =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-SKYYM   TO  WRK-SYMD
               PERFORM 801-HIZUKE-CHK-SEC
               IF    ( WRK-HENYMDG     =   SPACE )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-SYMD    (1:6)
                                       TO  SPA-NAI-SKYYM
                   MOVE    WRK-HENYMDG (1:6)
                                       TO  SPA-GMN-SKYYM
               END-IF
           END-IF
      *
           .
       4101-SKYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    提出日区分チェック処理
      *****************************************************************
       4101-TEISYUTUBIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-TEISYUTUBIKBN      TO  WRK-CMB-CD
           PERFORM 4200-TEISYUTUBIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TEISYUTUBIKBN-G
           IF    ( SPA-GMN-TEISYUTUBIKBN-G    =   SPACE )
               IF    ( SPA-GMN-RIYOUKBN       =   "1" )
                   MOVE    SPA-GMN-TEISYUTUBIKBNLST (1)
                                       TO  SPA-GMN-TEISYUTUBIKBN-G
               END-IF
           END-IF
      *
           .
       4101-TEISYUTUBIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求区分チェック処理
      *****************************************************************
       4101-SKYKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SKYKBN      TO  WRK-CMB-CD
           PERFORM 4200-SKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKYKBN-G
           IF    ( SPA-GMN-SKYKBN-G  =   SPACE )
               MOVE    SPA-GMN-SKYKBNLST (2)
                                       TO  SPA-GMN-SKYKBN-G
           END-IF
      *
           .
       4101-SKYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分チェック処理
      *****************************************************************
       4101-BUNBENKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-BUNBENKBN      TO  WRK-CMB-CD
           PERFORM 4200-BUNBENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-BUNBENKBN-G
           IF    ( SPA-GMN-BUNBENKBN-G  =   SPACE )
               MOVE    SPA-GMN-BUNBENKBNLST (1)
                                       TO  SPA-GMN-BUNBENKBN-G
           END-IF
      *
           IF    ( SPA-GMN-BUNBENKBN   NOT =   WRK-BUNBENKBN )
               PERFORM 3121-TEISYUTUSAKI-EDIT-SEC
           END-IF
      *
           IF    ( SPA-GMN-BUNBENKBN   =   "2" )
               IF    ( SPA-GMN-TEISYUTUBIKBN   =   "2" )
                   MOVE    "0011"      TO  SPA-ERRCD
                   MOVE    23          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-BUNBENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    提出先チェック処理
      *****************************************************************
       4101-TEISYUTUSAKI-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-TEISYUTUSAKI    TO  WRK-CMB-CD
           PERFORM 4200-TEISYUTUSAKILST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-TEISYUTUSAKI-G
           IF    ( WRK-CMB-ITM             =   SPACE )
               PERFORM 3121-TEISYUTUSAKI-EDIT-SEC
               MOVE    SPA-GMN-TEISYUTUSAKI    TO  WRK-CMB-CD
               PERFORM 4200-TEISYUTUSAKILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-TEISYUTUSAKI-G
           END-IF
      *
           .
       4101-TEISYUTUSAKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    在胎週数チェック処理
      *****************************************************************
       4101-ZAIWEEK-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-ZAIWEEK     =   ZERO )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-ZAIWEEK >=  12 )
                   CONTINUE
               ELSE
                   MOVE    "0006"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-ZAIWEEK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    出産年月日チェック処理
      *****************************************************************
       4101-BIRTHDAY-BABY-CHK-SEC      SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-BIRTHDAY-BABY
      *
           IF    ( SPA-GMN-BIRTHDAY-BABY   =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-BIRTHDAY-BABY
                                       TO  WRK-SYMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF    ( WRK-HENYMDG     =   SPACE )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-NAI-BIRTHDAY-BABY
                   MOVE    WRK-HENYMDG TO  SPA-GMN-BIRTHDAY-BABY
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-NAI-BIRTHDAY-BABY   <   CONST-H211001 )
                       MOVE    "0008"  TO  SPA-ERRCD
                       MOVE    6       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-BIRTHDAY-BABY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死産有無区分チェック処理
      *****************************************************************
       4101-SIZANKBN-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-SIZANKBN      TO  WRK-CMB-CD
           PERFORM 4200-SIZANKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SIZANKBN-G
           IF    ( SPA-GMN-SIZANKBN-G  =   SPACE )
               MOVE    SPA-GMN-SIZANKBNLST (2)
                                       TO  SPA-GMN-SIZANKBN-G
           END-IF
      *
           .
       4101-SIZANKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    出産数チェック処理
      *****************************************************************
       4101-BIRTHCNT-CHK-SEC           SECTION.
      *
      *
           .
       4101-BIRTHCNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院日数チェック処理
      *****************************************************************
       4101-NYUIN-DAY-CHK-SEC          SECTION.
      *
      *
           .
       4101-NYUIN-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    産科医療保障制度区分チェック処理
      *****************************************************************
       4101-SANKAKBN-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-SANKAKBN      TO  WRK-CMB-CD
           PERFORM 4200-SANKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SANKAKBN-G
           IF    ( SPA-GMN-SANKAKBN-G  =   SPACE )
               IF    ( SPA-NAI-SANKANUM    =   ZERO )
                   MOVE    SPA-GMN-SANKAKBNLST (2)
                                       TO  SPA-GMN-SANKAKBN-G
               ELSE
                   MOVE    SPA-GMN-SANKAKBNLST (1)
                                       TO  SPA-GMN-SANKAKBN-G
               END-IF
           END-IF
      *
           .
       4101-SANKAKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料チェック処理
      *****************************************************************
       4101-NYU-MONEY-CHK-SEC          SECTION.
      *
      *
           .
       4101-NYU-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       4101-RMSGK-MONEY-CHK-SEC        SECTION.
      *
      *
           .
       4101-RMSGK-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩介助料チェック処理
      *****************************************************************
       4101-KAIJO-MONEY-CHK-SEC        SECTION.
      *
      *
           .
       4101-KAIJO-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩料チェック処理
      *****************************************************************
       4101-BUNBEN-MONEY-CHK-SEC       SECTION.
      *
      *
           .
       4101-BUNBEN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    新生児管理保育料チェック処理
      *****************************************************************
       4101-SHIN-MONEY-CHK-SEC         SECTION.
      *
      *
           .
       4101-SHIN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検査・薬剤料チェック処理
      *****************************************************************
       4101-KNS-MONEY-CHK-SEC          SECTION.
      *
      *
           .
       4101-KNS-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処置・手当料チェック処理
      *****************************************************************
       4101-SYC-MONEY-CHK-SEC          SECTION.
      *
      *
           .
       4101-SYC-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    産科医療保障制度チェック処理
      *****************************************************************
       4101-SNK-MONEY-CHK-SEC          SECTION.
      *
      *
           .
       4101-SNK-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他チェック処理
      *****************************************************************
       4101-ETC-MONEY-CHK-SEC          SECTION.
      *
      *
           .
       4101-ETC-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金等チェック処理
      *****************************************************************
       4101-ICBFTN-MONEY-CHK-SEC       SECTION.
      *
      *
           .
       4101-ICBFTN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    備考チェック処理
      *****************************************************************
       4101-BIKOU-CHK-SEC              SECTION.
      *
           MOVE    SPA-GMN-BIKOU   TO  WRK-KANACHK-STR
           PERFORM 800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-STR TO  SPA-GMN-BIKOU
           IF    ( FLG-ENTRYCHK    NOT =   ZERO )
               MOVE    "0003"      TO  SPA-ERRCD
               MOVE    21          TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-BIKOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    内訳明細書印刷区分チェック処理
      *****************************************************************
       4101-MEIPRTKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-MEIPRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-MEIPRTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-MEIPRTKBN-G
           IF    ( SPA-GMN-MEIPRTKBN-G  =   SPACE )
               MOVE    SPA-GMN-MEIPRTKBNLST (2)
                                       TO  SPA-GMN-MEIPRTKBN-G
           END-IF
      *
           .
       4101-MEIPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    妊婦合計負担額チェック処理
      *****************************************************************
       4101-NINPUFTN-MONEY-CHK-SEC     SECTION.
      *
           COMPUTE WRK-NINPUFTN-MONEY
               =   SPA-GMN-NYU-MONEY
               +   SPA-GMN-RMSGK-MONEY
               +   SPA-GMN-KAIJO-MONEY
               +   SPA-GMN-BUNBEN-MONEY
               +   SPA-GMN-SHIN-MONEY
               +   SPA-GMN-KNS-MONEY
               +   SPA-GMN-SYC-MONEY
               +   SPA-GMN-SNK-MONEY
               +   SPA-GMN-ETC-MONEY
               +   SPA-GMN-ICBFTN-MONEY
      *
           IF    ( WRK-NINPUFTN-MONEY >=   10000000 )
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-GMN-CUR
               MOVE    9999999         TO  SPA-GMN-NINPUFTN-MONEY
           ELSE
               MOVE    WRK-NINPUFTN-MONEY
                                       TO  SPA-GMN-NINPUFTN-MONEY
           END-IF
      *
           .
       4101-NINPUFTN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    代理受取額チェック処理
      *****************************************************************
       4101-DAIRI-MONEY-CHK-SEC        SECTION.
      *
           IF    ( SPA-GMN-RIYOUKBN        =   "1" )
               IF    ( SPA-NAI-SANKANUM    NOT =   ZERO )
                AND  ( SPA-GMN-ZAIWEEK        >=   22 )
                   COMPUTE SPA-GMN-DAIRI-MONEY
                       =   420000      *   SPA-GMN-BIRTHCNT
               ELSE
                   COMPUTE SPA-GMN-DAIRI-MONEY
                       =   390000      *   SPA-GMN-BIRTHCNT
               END-IF
      *
               IF    ( SPA-GMN-DAIRI-MONEY >   SPA-GMN-NINPUFTN-MONEY )
                   MOVE    SPA-GMN-NINPUFTN-MONEY
                                   TO  SPA-GMN-DAIRI-MONEY
               END-IF
           ELSE
                   MOVE    ZERO    TO  SPA-GMN-DAIRI-MONEY
           END-IF
      *
           .
       4101-DAIRI-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分コンボチェック処理
      *****************************************************************
       4200-RIYOUKBNLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-RIYOUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-RIYOUKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-RIYOUKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-RIYOUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    提出日区分コンボチェック処理
      *****************************************************************
       4200-TEISYUTUBIKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TEISYUTUBIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TEISYUTUBIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TEISYUTUBIKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-TEISYUTUBIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求区分コンボチェック処理
      *****************************************************************
       4200-SKYKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SKYKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKYKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SKYKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SKYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分コンボチェック処理
      *****************************************************************
       4200-BUNBENKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-BUNBENKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-BUNBENKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-BUNBENKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-BUNBENKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    提出先コンボチェック処理
      *****************************************************************
       4200-TEISYUTUSAKILST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TEISYUTUSAKI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TEISYUTUSAKIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TEISYUTUSAKILST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-TEISYUTUSAKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死産有無区分コンボチェック処理
      *****************************************************************
       4200-SIZANKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SIZANKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SIZANKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SIZANKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SIZANKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    産科医療保障制度区分コンボチェック処理
      *****************************************************************
       4200-SANKAKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SANKAKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SANKAKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SANKAKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SANKAKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    内訳明細書印刷区分コンボチェック処理
      *****************************************************************
       4200-MEIPRTKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-MEIPRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-MEIPRTKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-MEIPRTKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-MEIPRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       411-KANREN-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD                   =   SPACE )
            AND  ( SPA-GMN-SKYYM           NOT =   SPACE )
            AND  ( SPA-GMN-TEISYUTUBIKBN   NOT =   SPACE )
               IF    ( SPA-GMN-BUNBENKBN       =   "1" )
                   MOVE    SPA-NAI-SKYYM
                                       TO  WRK-SKYYM
                   IF    ( SPA-GMN-TEISYUTUBIKBN   =   "1" )
                       MOVE    "10"    TO  WRK-SKYDD
                   ELSE
                       MOVE    "25"    TO  WRK-SKYDD
                   END-IF
                   IF    ( WRK-SKYYMD
                                       <   SPA-NAI-TAIINYMD  )
                       MOVE    "0005"  TO  SPA-ERRCD
                       MOVE    2       TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF    ( SPA-NAI-SKYYM
                                       <=  SPA-NAI-TAIINYMD (1:6) )
                       MOVE    "0007"  TO  SPA-ERRCD
                       MOVE    2       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-ZAIWEEK     =   ZERO )
                   MOVE    "1001"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-BIRTHDAY-BABY   =   SPACE )
                   MOVE    "1002"          TO  SPA-ERRCD
                   MOVE    6               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-BIRTHCNT    =   ZERO )
                   MOVE    "1003"          TO  SPA-ERRCD
                   MOVE    8               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-NYUIN-DAY   =   ZERO )
                   MOVE    "1004"          TO  SPA-ERRCD
                   MOVE    9               TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-BUNBENKBN   =   "1" )
                AND   ( SPA-GMN-KAIJO-MONEY >   ZERO )
                   MOVE    "1007"          TO  SPA-ERRCD
                   MOVE    13              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-BUNBENKBN   =   "2" )
                AND  ( SPA-GMN-BUNBEN-MONEY >   ZERO )
                   MOVE    "1008"          TO  SPA-ERRCD
                   MOVE    14              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-RIYOUKBN        =   "1" )
                   IF    ( SPA-GMN-TEISYUTUSAKI    =   SPACE )
                       MOVE    "1012"      TO  SPA-ERRCD
                       MOVE    14          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-BUNBENKBN   =   "1" )
                   IF    ( SPA-GMN-TEISYUTUSAKI    =   "1" )
                       MOVE    "1013"      TO  SPA-ERRCD
                       MOVE    14          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-RIYOUKBN    =   "1" )
                   IF    ( SPA-GMN-BUNBENKBN   =   "2" )
                    AND  ( SPA-NAI-RECESAKI    =   "1" )
                    AND  ( SPA-GMN-SYAKOK      =   "1" )
                       IF    ( SPA-GMN-TEISYUTUSAKI    =   "2" )
                           MOVE    "1014"  TO  SPA-ERRCD
                           MOVE    7       TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-RIYOUKBN    =   "1" )
                   IF    ( SPA-GMN-BUNBENKBN   =   "2" )
                    AND  ( SPA-NAI-RECESAKI    =   "2" )
                    AND  ( SPA-GMN-SYAKOK      =   "2" )
                       IF    ( SPA-GMN-TEISYUTUSAKI    =   "1" )
                           MOVE    "1015"  TO  SPA-ERRCD
                           MOVE    7       TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-BIRTHCNT    =   1 )
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-GMN-SIZANKBN    =   "3" )
                       MOVE    "1005"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-CUR
                   END-IF
               END-IF
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-GMN-SANKAKBN    =   "3" )
                       MOVE    "1006"      TO  SPA-ERRCD
                       MOVE    10          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-SANKAKBN        =   "1" OR "3" )
                   CONTINUE
               ELSE
                   IF    ( SPA-GMN-SNK-MONEY   >   ZERO )
                       MOVE    "1011"      TO  SPA-ERRCD
                       MOVE    18          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-SANKAKBN    =   "3" )
                AND  ( SPA-NAI-SANKANUM    =   ZERO )
                   MOVE    "0009"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SANKAKBN         =   "1" )
                   IF    ( SPA-GMN-ZAIWEEK     >=   22 )
                    AND  ( SPA-NAI-SANKANUM NOT =   ZERO )
                       CONTINUE
                   ELSE
                       IF    ( SPA-NAI-SANKANUM =   ZERO )
                           MOVE    "0009"  TO  SPA-ERRCD
                           MOVE    10      TO  SPA-GMN-CUR
                       END-IF
                       IF    ( SPA-ERRCD        =   SPACE )
                           IF    ( SPA-GMN-ZAIWEEK >=  22 )
                               CONTINUE
                           ELSE
                               MOVE    "0010"  TO  SPA-ERRCD
                               MOVE    10      TO  SPA-GMN-CUR
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       411-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I01"               TO  SPA-SAKIPG
           MOVE    "I07"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       220-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    (  SPA-ERRCD          =   SPACE )
               PERFORM 411-KANREN-CHK-SEC
           END-IF
      *
           IF    (  SPA-ERRCD          =   SPACE )
               MOVE    "3001"          TO  SPA-I0IDCD
           END-IF
      *
           .
      *
       220-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       2201-TOUROKU-SEC                SECTION.
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           MOVE    BUNBEN-CREYMD           TO  WRK-CREYMD
      *
           INITIALIZE                          BUNBEN-REC
           MOVE    SPA-HOSPNUM             TO  BUNBEN-HOSPNUM
           MOVE    SPA-I07-PTID            TO  BUNBEN-PTID
           MOVE    SPA-NAI-SKYYM           TO  BUNBEN-SKYYM
           MOVE    SPA-I07-RRKNUM          TO   BUNBEN-RRKNUM
           MOVE    SPA-GMN-PTNUM           TO   BUNBEN-PTNUM
           MOVE    SPA-GMN-TEISYUTUBIKBN   TO   BUNBEN-TEISYUTUBIKBN
           MOVE    SPA-GMN-SKYKBN          TO   BUNBEN-SKYKBN
           MOVE    SPA-GMN-RIYOUKBN        TO   BUNBEN-RIYOUKBN
           MOVE    SPA-GMN-BUNBENKBN       TO   BUNBEN-BUNBENKBN
      *
           IF    ( SPA-GMN-RIYOUKBN    =   "1" )
               MOVE    SPA-GMN-TEISYUTUSAKI TO BUNBEN-TEISYUTUSAKI
           END-IF
      *
           MOVE    SPA-GMN-SYAKOK          TO  BUNBEN-KANYUKBN
           MOVE    SPA-GMN-HONKZK          TO  BUNBEN-HONKZKKBN
           MOVE    SPA-GMN-HKNJANUM        TO  BUNBEN-HKNJANUM
           MOVE    SPA-GMN-KIGO            TO  BUNBEN-KIGO
           MOVE    SPA-GMN-NUM             TO  BUNBEN-NUM
           MOVE    SPA-GMN-KANANAME        TO  BUNBEN-NAME
           MOVE    SPA-GMN-ZAIWEEK         TO  BUNBEN-ZAIWEEK
           MOVE    SPA-NAI-BIRTHDAY-BABY   TO  BUNBEN-BIRTHDAY
           MOVE    SPA-GMN-SIZANKBN        TO  BUNBEN-SIZANKBN
           MOVE    SPA-GMN-BIRTHCNT        TO  BUNBEN-BIRTHCNT
           MOVE    SPA-GMN-NYUIN-DAY       TO  BUNBEN-NYUIN-DAY
           MOVE    SPA-GMN-SANKAKBN        TO  BUNBEN-SANKAKBN
           MOVE    SPA-GMN-NYU-MONEY       TO  BUNBEN-NYU-MONEY
           MOVE    SPA-GMN-RMSGK-MONEY     TO  BUNBEN-RMSGK-MONEY
           MOVE    SPA-GMN-KAIJO-MONEY     TO  BUNBEN-KAIJO-MONEY
           MOVE    SPA-GMN-BUNBEN-MONEY    TO  BUNBEN-BUNBEN-MONEY
           MOVE    SPA-GMN-SHIN-MONEY      TO  BUNBEN-SHIN-MONEY
           MOVE    SPA-GMN-KNS-MONEY       TO  BUNBEN-KNS-MONEY
           MOVE    SPA-GMN-SYC-MONEY       TO  BUNBEN-SYC-MONEY
           MOVE    SPA-GMN-SNK-MONEY       TO  BUNBEN-SNK-MONEY
           MOVE    SPA-GMN-ETC-MONEY       TO  BUNBEN-ETC-MONEY
           MOVE    SPA-GMN-ICBFTN-MONEY    TO  BUNBEN-ICBFTN-MONEY
           MOVE    SPA-GMN-NINPUFTN-MONEY  TO  BUNBEN-NINPUFTN-MONEY
           MOVE    SPA-GMN-DAIRI-MONEY     TO  BUNBEN-DAIRI-MONEY
           MOVE    SPA-GMN-BIKOU           TO  BUNBEN-BIKOU
           MOVE    SPA-NAI-HKNJANUM-S      TO  BUNBEN-HKNJANUM-S
           MOVE    "1"                     TO  BUNBEN-UPDKBN
           PERFORM 800-MCNDATE-SEC
           MOVE    WRK-CREYMD              TO  BUNBEN-CREYMD
           MOVE    SMCNDATE-YMD            TO  BUNBEN-UPYMD
           MOVE    SMCNDATE-HMS            TO  BUNBEN-UPHMS
           MOVE    BUNBEN-REC              TO  MCPDATA-REC
           MOVE    "tbl_bunben"            TO  MCP-TABLE
           MOVE    "del3"                  TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    BUNBEN-REC      TO  MCPDATA-REC
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "4002"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               IF    ( SPA-GMN-MEIPRTKBN   =   "1" )
      *
                   CALL    "ORCSONPRTSET"  USING   SPA-AREA
      *
                   INITIALIZE                      ONPRTDATA
                                                   ORCHCN63AREA
                   MOVE    SPA-I07-PTID        TO  ORCHCN63-PTID
                   MOVE    SPA-I07-RRKNUM      TO  ORCHCN63-RRKNUM
                   MOVE    ORCHCN63AREA        TO  ONPRTDATA-DATA
                   MOVE    "ORCHCN63"          TO  ONPRTDATA-PGID
                   MOVE    "01"                TO  SPA-PRT-FLG
                   CALL    "ORCHCOMMON"        USING
                                               SPA-AREA
                                               ONPRTDATA
                   MOVE    SPACE               TO  SPA-PRT-FLG
               END-IF
      *
               IF    ( SPA-I07-MOTOPG  =   "I04" )
                   MOVE    "JOIN"          TO  MCP-PUTTYPE
                   PERFORM 210-BACK
               ELSE
                   PERFORM 310-SPASET-SEC
               END-IF
           END-IF
      *
           .
      *
       2201-TOUROKU-EXT.
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
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I07    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC                      SECTION.
      *
           MOVE   SPACE                TO  I07
           INITIALIZE                      I07
      *
           MOVE    SPA-GMN-PTNUM           TO  I07-PTNUM
           MOVE    SPA-GMN-NAME            TO  I07-NAME
           MOVE    SPA-GMN-KANANAME        TO  I07-KANANAME
           MOVE    SPA-GMN-BIRTHDAY-MOM    TO  I07-BIRTHDAY-MOM
      *
           INITIALIZE                          WRK-AGE-G
           MOVE    SPA-GMN-AGE             TO  WRK-AGE-ZZ9
           MOVE    "才"                    TO  WRK-AGE-LBL
           MOVE    WRK-AGE-G               TO  I07-AGE
      *
           MOVE    SPA-GMN-NYUINYMD        TO  I07-NYUINYMD
           MOVE    SPA-GMN-TAIINYMD        TO  I07-TAIINYMD
      *
           EVALUATE    SPA-GMN-SYAKOK
           WHEN    "1"
               MOVE    "社保"              TO  I07-SYAKOK
           WHEN    "2"
               MOVE    "国保"              TO  I07-SYAKOK
           END-EVALUATE
      *
           IF    ( SPA-GMN-SYAKOK      NOT =   SPACE )
               IF    ( SPA-GMN-HONKZK      =   "1" )
                   MOVE    "本人"          TO  I07-HONKZK
               ELSE
                   MOVE    "家族"          TO  I07-HONKZK
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-HKNJANUM        TO  I07-HKNJANUM
           MOVE    SPA-GMN-KIGO            TO  I07-KIGO
           MOVE    SPA-GMN-NUM             TO  I07-NUM
      *
           MOVE    SPA-GMN-RIYOUKBN-G      TO  I07-RIYOUKBN
           MOVE    SPA-GMN-RIYOUKBN-MAX    TO  I07-RIYOUKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-RIYOUKBN-MAX )
               MOVE    SPA-GMN-RIYOUKBNLST (IDX-CMB)
                                           TO  I07-RIYOUKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKYYM           TO  I07-SKYYM
      *
           MOVE    SPA-GMN-TEISYUTUBIKBN-G TO  I07-TEISYUTUBIKBN
           MOVE    SPA-GMN-TEISYUTUBIKBN-MAX
                                           TO  I07-TEISYUTUBIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-TEISYUTUBIKBN-MAX )
               MOVE    SPA-GMN-TEISYUTUBIKBNLST (IDX-CMB)
                                           TO  I07-TEISYUTUBIKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKYKBN-G        TO  I07-SKYKBN
           MOVE    SPA-GMN-SKYKBN-MAX      TO  I07-SKYKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKYKBN-MAX )
               MOVE    SPA-GMN-SKYKBNLST (IDX-CMB)
                                           TO  I07-SKYKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-BUNBENKBN-G     TO  I07-BUNBENKBN
           MOVE    SPA-GMN-BUNBENKBN-MAX   TO  I07-BUNBENKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-BUNBENKBN-MAX )
               MOVE    SPA-GMN-BUNBENKBNLST (IDX-CMB)
                                           TO  I07-BUNBENKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           IF    ( SPA-GMN-RIYOUKBN    =   "0" )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-TEISYUTUSAKI-G
                                           TO  I07-TEISYUTUSAKI
               MOVE    SPA-GMN-TEISYUTUSAKI-MAX
                                           TO  I07-TEISYUTUSAKI-COUNT
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   SPA-GMN-TEISYUTUSAKI-MAX )
                   MOVE    SPA-GMN-TEISYUTUSAKILST (IDX-CMB)
                                               TO  I07-TEISYUTUSAKI-ITEM
                                                               (IDX-CMB)
               END-PERFORM
           END-IF
           MOVE    SPA-GMN-ZAIWEEK         TO  I07-ZAIWEEK
           MOVE    SPA-GMN-BIRTHDAY-BABY   TO  I07-BIRTHDAY-BABY
      *
           MOVE    SPA-GMN-SIZANKBN-G      TO  I07-SIZANKBN
           MOVE    SPA-GMN-SIZANKBN-MAX    TO  I07-SIZANKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SIZANKBN-MAX )
               MOVE    SPA-GMN-SIZANKBNLST (IDX-CMB)
                                           TO  I07-SIZANKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-BIRTHCNT        TO  I07-BIRTHCNT
           MOVE    SPA-GMN-NYUIN-DAY       TO  I07-NYUIN-DAY
      *
           MOVE    SPA-GMN-SANKAKBN-G      TO  I07-SANKAKBN
           MOVE    SPA-GMN-SANKAKBN-MAX    TO  I07-SANKAKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SANKAKBN-MAX )
               MOVE    SPA-GMN-SANKAKBNLST (IDX-CMB)
                                           TO  I07-SANKAKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-NYU-MONEY       TO  I07-NYU-MONEY
           MOVE    SPA-GMN-RMSGK-MONEY     TO  I07-RMSGK-MONEY
           MOVE    SPA-GMN-KAIJO-MONEY     TO  I07-KAIJO-MONEY
           MOVE    SPA-GMN-BUNBEN-MONEY    TO  I07-BUNBEN-MONEY
           MOVE    SPA-GMN-SHIN-MONEY      TO  I07-SHIN-MONEY
           MOVE    SPA-GMN-KNS-MONEY       TO  I07-KNS-MONEY
           MOVE    SPA-GMN-SYC-MONEY       TO  I07-SYC-MONEY
           MOVE    SPA-GMN-SNK-MONEY       TO  I07-SNK-MONEY
           MOVE    SPA-GMN-ETC-MONEY       TO  I07-ETC-MONEY
           MOVE    SPA-GMN-ICBFTN-MONEY    TO  I07-ICBFTN-MONEY
           MOVE    SPA-GMN-NINPUFTN-MONEY  TO  I07-NINPUFTN-MONEY
           MOVE    SPA-GMN-DAIRI-MONEY     TO  I07-DAIRI-MONEY
           MOVE    SPA-GMN-BIKOU           TO  I07-BIKOU
      *
           MOVE    SPA-GMN-MEIPRTKBN-G     TO  I07-MEIPRTKBN
           MOVE    SPA-GMN-MEIPRTKBN-MAX   TO  I07-MEIPRTKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-MEIPRTKBN-MAX )
               MOVE    SPA-GMN-MEIPRTKBNLST (IDX-CMB)
                                           TO  I07-MEIPRTKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    "black"                 TO  I07-LBLINFO-STYLE (1)
           MOVE    "black"                 TO  I07-LBLINFO-STYLE (2)
           MOVE    "black"                 TO  I07-LBLINFO-STYLE (3)
           MOVE    "black"                 TO  I07-LBLINFO-STYLE (4)
           MOVE    "black"                 TO  I07-LBLHEADER-STYLE
      *
           MOVE    ZERO                    TO  IDX-INFO
      *
           IF    ( IDX-INFO           >=   4 )
               CONTINUE
           ELSE
               IF    ( SPA-NAI-CHGNAME     =   "1" )
                   COMPUTE IDX-INFO    =   IDX-INFO    +   1
                   MOVE    "red"       TO  I07-LBLINFO-STYLE (IDX-INFO)
                   STRING  "氏名が変更されています。"
                                               DELIMITED BY SIZE
                           "登録ボタンを押してください。"
                                               DELIMITED BY SIZE
                   INTO     I07-LBLINFO (IDX-INFO)
                   END-STRING
               END-IF
           END-IF
      *
           IF    ( IDX-INFO           >=   4 )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-BUNBENKBN   =   "2"   )
                AND  ( SPA-GMN-BIKOU       =   SPACE )
                AND  ( SPA-GMN-RIYOUKBN    =   "1" )
                   COMPUTE IDX-INFO        =   IDX-INFO    +   1
                   MOVE    "bule"      TO  I07-LBLINFO-STYLE (IDX-INFO)
                   STRING  "補足情報等が必要な場合は"
                                               DELIMITED BY SIZE
                           "備考欄に入力してください。"
                                               DELIMITED BY SIZE
                   INTO     I07-LBLINFO (IDX-INFO)
                   END-STRING
               END-IF
           END-IF
      *
           IF    ( IDX-INFO           >=   4 )
               CONTINUE
           ELSE
               COMPUTE IDX-INFO    =   IDX-INFO    +   1
               MOVE    "blue"      TO  I07-LBLINFO-STYLE (IDX-INFO)
               IF    ( SPA-GMN-SANKAKBN        =   "1" OR "3" )
                   IF    ( SPA-GMN-SNK-MONEY   =   ZERO )
                       STRING  "産科医療保障制度の金額が未入力です。"
                                               DELIMITED BY SIZE
                       INTO     I07-LBLINFO (IDX-INFO)
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-HKNJANUM        =   SPACE )
            AND  ( SPA-GMN-RIYOUKBN        =   "1" )
               MOVE    "red"               TO  I07-LBLHEADER-STYLE
               MOVE    "保険者番号未入力"  TO  I07-LBLHEADER
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
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  I07-SKYYM-STATE
                                           I07-TEISYUTUBIKBN-STATE
                                           I07-TEISYUTUBIKBN-ITEM-STATE
                                           I07-SKYKBN-STATE
                                           I07-SKYKBN-ITEM-STATE
                                           I07-TEISYUTUSAKI-STATE
                                           I07-TEISYUTUSAKI-ITEM-STATE
                                           I07-SANKAKBN-STATE
                                           I07-SANKAKBN-ITEM-STATE
      *
           IF    ( SPA-GMN-RIYOUKBN    =   "0" )
            OR   ( SPA-GMN-HKNJANUM    =   SPACE )
               MOVE    WIDGET-INSENSITIVE
                                       TO  I07-SKYYM-STATE
                                           I07-TEISYUTUBIKBN-STATE
                                           I07-TEISYUTUBIKBN-ITEM-STATE
                                           I07-SKYKBN-STATE
                                           I07-SKYKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-RIYOUKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                       TO  I07-TEISYUTUSAKI-STATE
                                           I07-TEISYUTUSAKI-ITEM-STATE
           END-IF
      *
           IF    ( SPA-NAI-SANKANUM    =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  I07-SANKAKBN-STATE
                                           I07-SANKAKBN-ITEM-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
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
               MOVE    "RIYOUKBN"      TO  MCP-WIDGET
           WHEN    002
               MOVE    "SKYYM"         TO  MCP-WIDGET
           WHEN    003
               MOVE    "SKYKBN"        TO  MCP-WIDGET
           WHEN    004
               MOVE    "BUNBENKBN"     TO  MCP-WIDGET
           WHEN    005
               MOVE    "ZAIWEEK"       TO  MCP-WIDGET
           WHEN    006
               MOVE    "BIRTHDAY_BABY" TO  MCP-WIDGET
           WHEN    007
               MOVE    "SIZANKBN"      TO  MCP-WIDGET
           WHEN    008
               MOVE    "BIRTHCNT"      TO  MCP-WIDGET
           WHEN    009
               MOVE    "NYUIN_DAY"     TO  MCP-WIDGET
           WHEN    010
               MOVE    "SANKAKBN"      TO  MCP-WIDGET
           WHEN    011
               MOVE    "NYU_MONEY"     TO  MCP-WIDGET
           WHEN    012
               MOVE    "RMSGK_MONEY"   TO  MCP-WIDGET
           WHEN    013
               MOVE    "KAIJO_MONEY"   TO  MCP-WIDGET
           WHEN    014
               MOVE    "BUNBEN_MONEY"  TO  MCP-WIDGET
           WHEN    015
               MOVE    "SHIN_MONEY"    TO  MCP-WIDGET
           WHEN    016
               MOVE    "KNS_MONEY"     TO  MCP-WIDGET
           WHEN    017
               MOVE    "SYC_MONEY"     TO  MCP-WIDGET
           WHEN    018
               MOVE    "SNK_MONEY"     TO  MCP-WIDGET
           WHEN    019
               MOVE    "ETC_MONEY"     TO  MCP-WIDGET
           WHEN    020
               MOVE    "ICBFTN_MONEY"  TO  MCP-WIDGET
           WHEN    021
               MOVE    "BIKOU"         TO  MCP-WIDGET
           WHEN    022
               MOVE    "MEIPRTKBN"     TO  MCP-WIDGET
           WHEN    023
               MOVE    "TEISYUTUBIKBN" TO  MCP-WIDGET
           WHEN    024
               MOVE    "TEISYUTUSAKI"  TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "RIYOUKBN"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "SKYYM"
               MOVE    023             TO  SPA-GMN-CUR
           WHEN    "TEISYUTUBIKBN"
               MOVE    003             TO  SPA-GMN-CUR
           WHEN    "SKYKBN"
               MOVE    004             TO  SPA-GMN-CUR
           WHEN    "BUNBENKBN"
               MOVE    024             TO  SPA-GMN-CUR
           WHEN    "TEISYUTUSAKI"
               MOVE    005             TO  SPA-GMN-CUR
           WHEN    "ZAIWEEK"
               MOVE    006             TO  SPA-GMN-CUR
           WHEN    "BIRTHDAY_BABY"
               MOVE    007             TO  SPA-GMN-CUR
           WHEN    "SIZANKBN"
               MOVE    008             TO  SPA-GMN-CUR
           WHEN    "BIRTHCNT"
               MOVE    009             TO  SPA-GMN-CUR
           WHEN    "NYUIN_DAY"
               MOVE    010             TO  SPA-GMN-CUR
           WHEN    "SANKAKBN"
               MOVE    011             TO  SPA-GMN-CUR
           WHEN    "NYU_MONEY"
               MOVE    012             TO  SPA-GMN-CUR
           WHEN    "RMSGK_MONEY"
               MOVE    013             TO  SPA-GMN-CUR
           WHEN    "KAIJO_MONEY"
               MOVE    014             TO  SPA-GMN-CUR
           WHEN    "BUNBEN_MONEY"
               MOVE    015             TO  SPA-GMN-CUR
           WHEN    "SHIN_MONEY"
               MOVE    016             TO  SPA-GMN-CUR
           WHEN    "KNS_MONEY"
               MOVE    017             TO  SPA-GMN-CUR
           WHEN    "SYC_MONEY"
               MOVE    018             TO  SPA-GMN-CUR
           WHEN    "SNK_MONEY"
               MOVE    019             TO  SPA-GMN-CUR
           WHEN    "ETC_MONEY"
               MOVE    020             TO  SPA-GMN-CUR
           WHEN    "ICBFTN_MONEY"
               MOVE    021             TO  SPA-GMN-CUR
           WHEN    "BIKOU"
               MOVE    022             TO  SPA-GMN-CUR
           WHEN    "MEIPRTKBN"
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
       5012-SKIP-CUR-SEC               SECTION.
      *
           IF    ( SPA-GMN-CUR     =   2 )
               IF    ( I07-SKYYM-STATE =   WIDGET-INSENSITIVE )
                   MOVE    23      TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   23 )
               IF    ( I07-TEISYUTUBIKBN-STATE
                                   =   WIDGET-INSENSITIVE )
                   MOVE    3       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   3 )
               IF    ( I07-SKYKBN-STATE =   WIDGET-INSENSITIVE )
                   MOVE    4       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   24 )
               IF    ( I07-TEISYUTUSAKI-STATE
                                   =   WIDGET-INSENSITIVE )
                   MOVE    5       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   10 )
               IF    ( I07-SANKAKBN-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    11      TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       5012-SKIP-CUR-CUR-EXT.
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
               MOVE    "請求年月入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "出産年月日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "備考欄は全て全角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "妊婦負担額の上限をオーバーしています"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "請求日＜退院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "在胎週数が１２週未満です"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "請求年月≦退院月となっています"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "出産年月日が直接支払制度開始前の日付です"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE   "分娩機関管理番号が未設定です"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE   "在胎週数が２２週未満です"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE   "異常分娩は２５日請求できません"
                                       TO  SPA-ERRMSG
           WHEN    "1012"
               MOVE    "提出先を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1013"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "正常分娩の場合、"  DELIMITED   BY  SIZE
                       "提出先に国保連合会を選択してください"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1014"
               MOVE    "提出先に支払基金を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1015"
               MOVE    "提出先に国保連合会を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "在胎週数を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "出産年月日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "出産数を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "入院日数を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "出産数が１人で死産有無区分に混在が"
                                           DELIMITED BY SIZE
                       "設定されています"  DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1006"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "出産数が１人で産科医療保障制度区分に混在が"
                                           DELIMITED BY SIZE
                       "設定されています"  DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1007"
               MOVE    "正常分娩で分娩介助料が入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "異常分娩で分娩料が入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "1011"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "産科医療保障制度の金額が"
                                           DELIMITED BY SIZE
                       "入力されています"  DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "4001"  THRU    "4002"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ERR
           MOVE    SPA-ERRCD           TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I0ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I07"               TO  SPA-MOTOPG
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
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-I0IDSET-SEC                 SECTION.
      *
           EVALUATE    SPA-I0IDCD
           WHEN    "3001"
               MOVE    "登録します。よろしいですか"
                                       TO  WRK-I0IDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ID1
           MOVE    SPA-I0IDCD          TO  I0ID1-ID1CODE
           MOVE    WRK-I0IDMSG         TO  I0ID1-ID1MSG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "I07"               TO  SPA-MOTOPG
           MOVE    "I0ID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I0ID1"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-I0IDSET-EXT.
           EXIT.
      *****************************************************************
      *    分娩負担金サブ
      *****************************************************************
       800-ORCSBUNBENFTN-SEC              SECTION.
      *
           INITIALIZE                     LNKBUNBEN-REC
           MOVE    SPA-HOSPNUM         TO LNKBUNBEN-HOSPNUM
           MOVE    SPA-I07-PTID        TO LNKBUNBEN-PTID
           MOVE    SPA-I07-RRKNUM      TO LNKBUNBEN-RRKNUM
           CALL    "ORCSBUNBENFTN"     USING
                                       LNKBUNBEN-REC
                                       SPA-AREA
      *
           .
       800-ORCSBUNBENFTN-EXT.
           EXIT.


      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  (   WRK-KANACHK-STR     =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING
                                       ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-STR
               IF      ( KANACHK-RC    NOT =   ZERO )
                   MOVE    1       TO  FLG-ENTRYCHK
               END-IF
               IF      ( KANACHK-SYUBETU   NOT =   2 )
                   MOVE    1       TO  FLG-ENTRYCHK
               END-IF
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE        SPACE       TO  WRK-HENYMDG
      *
           INITIALIZE                  ORCSGDAYAREA
           MOVE    WRK-SYMD        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF  ( SGDAY-RC          =   ZERO )
               MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY  TO  WRK-SYMD
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       801-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE        SPACE       TO  WRK-HENYMDG
      *
           INITIALIZE                  ORCSGDAYAREA
           MOVE    "1"             TO  SGDAY-INTYPE
           MOVE    WRK-SYMD        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF  ( SGDAY-RC          =   ZERO )
               MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY  TO  WRK-SYMD
           END-IF
      *
           .
       801-HIZUKE-CHK-EXT.
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
      *    患者情報検索処理(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-I07-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY4-SEL-SEC     SECTION.
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-I07-PTID        TO  PTNYUINRRK-PTID
           MOVE    SPA-I07-RRKNUM      TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SPA-I07-PTID    TO  BUNBEN-PTID
           MOVE    SPA-I07-RRKNUM  TO  BUNBEN-RRKNUM
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    旧姓履歴検索処理
      *****************************************************************
       900-KYUSEIRRK-KEY3-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUSEIRRK
      *
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  KYUSEI-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  KYUSEI-PTID
           MOVE    PTNYUINRRK-TAIINYMD TO  KYUSEI-CHGYMD
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  KYUSEIRRK-REC
           ELSE
               INITIALIZE                  KYUSEIRRK-REC
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           .
       900-KYUSEIRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    旧姓履歴FETCH処理
      *****************************************************************
       900-KYUSEIRRK-KEY3-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUSEIRRK
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  KYUSEIRRK-REC
           ELSE
               INITIALIZE                  KYUSEIRRK-REC
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           .
       900-KYUSEIRRK-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
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
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
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
      *    DBUPDATE処理
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
      *    DBINSERT処理
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
      *    DBDELETE処理
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

