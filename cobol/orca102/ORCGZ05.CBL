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
       PROGRAM-ID.                               ORCGZ05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ特定器材設定画面（Ｚ０５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01/07/03  日付入力チェックサブ追加
      *  01.00.02    MCC-多々納   01/07/18  正式名称等追加
      *  01.01.01    MCC-藤原     01/09/26  JOINの修正      
      *  01.01.02    MCC-多々納   02/07/18  服用方法画面追加
      *  01.01.03    NACL-多々納  02/10/09  半角空白→全角空白追加
      *  02.00.00    NACL-多々納  03/06/13  一括入力へ変更
      *  02.00.01    NACL-多々納  05/12/09  登録コードチェック追加
      *                                     MONFUNC対応 他
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.05.00    NACL-多々納  10/04/XX  帳票名称追加対応
      *  04.06.00    NACL-多々納  12/08/16  期間変更警告追加
      *  04.07.00    NACL-多々納  13/02/01  特定器材２を上限価格へ
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
       01  SPA-Z05.
      *
      *    第五画面ＳＰＡエリア
           03  SPA-GMN-AREA.
               05  SPA-GMN-SRYCDH              PIC X(09).
               05  SPA-GMN-NAMEH               PIC X(100).
               05  SPA-GMN-YUKOSTYMDH          PIC X(09).
               05  SPA-GMN-YUKOEDYMDH          PIC X(09).
               05  SPA-GMN-YUKOSTYMD           PIC X(10).
               05  SPA-GMN-YUKOEDYMD           PIC X(10).
               05  SPA-GMN-KANANAME            PIC X(100).
               05  SPA-GMN-NAME                PIC X(100).
               05  SPA-GMN-FORMALNAME          PIC X(200).
               05  SPA-GMN-SHONAME             PIC X(100).
               05  SPA-GMN-TEN                 PIC 9(8)V9(2).
      *
               05  SPA-GMN-TENSIKIBETU-G.
                   07  SPA-GMN-TENSIKIBETU         PIC 9(1).
                   07  SPA-GMN-F1                  PIC X(1).
                   07  SPA-GMN-TENSIKIBETUMEI      PIC X(20).
               05  SPA-GMN-TENSIKIBETUCOB-G.
                   07  SPA-GMN-TENSIKIBETUCOB      PIC X(26)
                                                   OCCURS   7.
               05  SPA-TENSIKI-MAX             PIC 9(04).
      *
               05  SPA-GMN-JGNTEN              PIC 9(7).
               05  SPA-GMN-OLDTEN              PIC 9(8)V9(2).
      *
               05  SPA-GMN-OLDTENSKB-G.
                   07  SPA-GMN-OLDTENSKB           PIC 9(1).
                   07  SPA-GMN-F1                  PIC X(1).
                   07  SPA-GMN-OLDTENSKBMEI        PIC X(20).
               05  SPA-GMN-OLDTENSKBCOB-G.
                   07  SPA-GMN-OLDTENSKBCOB        PIC X(26)
                                                   OCCURS   7.
               05  SPA-OLDTENSIKI-MAX              PIC 9(04).
      *
               05  SPA-GMN-TANICD-G.
                   07  SPA-GMN-TANICD              PIC X(3).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-TANINAME            PIC X(26).
               05  SPA-GMN-TANICDCOB-G.
                   07  SPA-GMN-TANICDCOB           PIC X(30)
                                                   OCCURS  200.
               05  SPA-TANICD-MAX             PIC 9(04).
      *
               05  SPA-GMN-TOKUKIZAIAGEKSNKBN-G.
                   07  SPA-GMN-TOKUKIZAIAGEKSNKBN  PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-TOKUIZAIMEI         PIC X(26).
               05  SPA-GMN-TOKUKIZAIAGEKSNCOB-G.
                   07  SPA-GMN-TOKUKIZAIAGEKSNCOB  PIC X(30)
                                                   OCCURS   2.
               05  SPA-TOKUKIZAI-MAX             PIC 9(04).
      *
               05  SPA-GMN-KGNAGE              PIC X(2).
               05  SPA-GMN-JGNAGE              PIC X(2).
      *
               05  SPA-GMN-MEIUSESKB-G.
                   07  SPA-GMN-MEIUSESKB           PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-MEIUSESKBMEI        PIC X(26).
               05  SPA-GMN-MEIUSESKBCOB-G.
                   07  SPA-GMN-MEIUSESKBCOB        PIC X(30)
                                                   OCCURS   2.
               05  SPA-MEIUSESKB-MAX             PIC 9(04).
      *
               05  SPA-GMN-SANSOKBN-G.
                   07  SPA-GMN-SANSOKBN            PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-SANSOKBNMEI         PIC X(26).
               05  SPA-GMN-SANSOKBNCOB-G.
                   07  SPA-GMN-SANSOKBNCOB         PIC X(30)
                                                   OCCURS   7.
               05  SPA-SANSOKBN-MAX             PIC 9(04).
      *
               05  SPA-GMN-TOKUKIZAISBT1-G.
                   07  SPA-GMN-TOKUKIZAISBT1       PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-TOKUKIZAISBT1MEI    PIC X(26).
               05  SPA-GMN-TOKUKIZAISBT1COB-G.
                   07  SPA-GMN-TOKUKIZAISBT1COB    PIC X(30)
                                                   OCCURS   4.
               05  SPA-TOKUKIZAISBT1-MAX             PIC 9(04).
      *
               05  SPA-GMN-TOKUKIZAISBT2-G.
                   07  SPA-GMN-TOKUKIZAISBT2       PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-TOKUKIZAISBT2MEI    PIC X(26).
               05  SPA-GMN-TOKUKIZAISBT2COB-G.
                   07  SPA-GMN-TOKUKIZAISBT2COB    PIC X(30)
                                                   OCCURS   3.
               05  SPA-TOKUKIZAISBT2-MAX             PIC 9(04).
      *
               05  SPA-GMN-DATAKBN-G.
                   07  SPA-GMN-DATAKBN             PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-DATAKBNMEI          PIC X(14).
               05  SPA-GMN-DATAKBNCOB-G.
                   07  SPA-GMN-DATAKBNCOB          PIC X(16)
                                                   OCCURS   3.
               05  SPA-DATAKBN-MAX             PIC 9(04).
      *
               05  SPA-GMN-KOUHYOJYUNNUM       PIC 9(9).
               05  SPA-GMN-INPCHKKBN           PIC 9(1).
               05  SPA-GMN-KEIKASOCHIYMD       PIC X(10).
               05  SPA-GMN-CDKBN-BU            PIC 9(2).
               05  SPA-GMN-CDKBN-KBNNUM        PIC 9(3).
      *********05  SPA-GMN-CUR                 PIC X(30).
               05  SPA-GMN-CUR                 PIC 9(02).
      *
      *    第五画面内容
           03  SPA-NAI-AREA.
               05  SPA-NAI-YUKOSTYMD           PIC X(08).
               05  SPA-NAI-YUKOEDYMD           PIC X(08).
      *
               05  SPA-NAI-KEIFLG1             PIC 9(01).
               05  SPA-NAI-KEIFLG2             PIC 9(01).
               05  SPA-NAI-KEIKASOCHIYMD       PIC X(08).
      *
               05  SPA-NAI-KEIFLG3             PIC 9(01).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-OK                            PIC 9(01).
           03  FLG-ERR                           PIC 9(01).
           03  FLG-TOUROKU                       PIC 9(01).
      *
           03  FLG-TENSU                         PIC 9(01).
           03  FLG-TENSUPLUS                     PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
           03  CNT-PAGE                          PIC 9(02).
           03  CNT-ERRCHK                        PIC 9(03).
           03  CNT-STRING                        PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(02).
           03  IDX-1                             PIC 9(02).
           03  IDX-2                             PIC 9(02).
           03  IDXX                              PIC 9(02).
           03  IDXY                              PIC 9(02).
           03  IDXZ                              PIC 9(02).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-GMN-CUR                       PIC X(30).
           03  WRK-ZIDMSG                        PIC X(70).
           03  WRK-STRINGTBL.
               05  WRK-STRING                    PIC X(01)  OCCURS 200.
           03  WRK-LNKYMD                        PIC X(10).
           03  WRK-WYMD.
               05  WRK-WGO                       PIC X(01).
               05  WRK-WYY                       PIC 9(02).
               05  WRK-WMM                       PIC 9(02).
               05  WRK-WDD                       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-YMD             PIC X(08).
           03  WRK-OTYMD           PIC X(08).
      *
           03  WRK-MAE-INPUT       PIC X(400).
           03  WRK-ATO-INPUT       PIC X(400).
           03  WRK-LEN             PIC 9(04).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-YUKOEDYMD       PIC X(08).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    単位の項目順テーブル
           COPY    "CMTANICD.INC".
      *
      *    ワーク点数マスタ
       01  WRK-TNS-TENSU-REC           PIC X(1290).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    点数附加情報更新内容
           COPY    "CPORCGZS02.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           COPY    "Z01.INC".
           COPY    "Z02.INC".
           COPY    "Z03.INC".
           COPY    "Z04.INC".
           COPY    "Z05.INC".
           COPY    "Z06.INC".
           COPY    "Z07.INC".
           COPY    "Z08.INC".
           COPY    "Z90.INC".
           COPY    "Z91.INC".
           COPY    "Z97.INC".
           COPY    "Z98.INC".
           COPY    "Z99.INC".
           COPY    "ZERR.INC".
           COPY    "ZID1.INC".
           COPY    "ZID2.INC".
           COPY    "ZID3.INC".
           COPY    "Z100.INC".
           COPY    "ZFIL.INC".
           COPY    "CPAPIMOD.INC".
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
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA
           INITIALIZE                            WRK-AREA
           INITIALIZE                            IDX-AREA
           INITIALIZE                            CNT-AREA
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z05
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           EVALUATE    MCP-STATUS       ALSO    MCP-EVENT
               WHEN    "LINK"           ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END                       =   ZERO
               PERFORM 500-SET-SCREEN-SEC
           END-IF.
      *    
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-Z05                       TO  SPA-Z01SONOTA 
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
               MOVE    SPACE                TO  LINKAREA
      *
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "Z05"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *    更新処理
               WHEN    "ZID1"
                   PERFORM 330-ZID1-SET-SEC
      *    入力ＣＤより
               WHEN    "Z99"
                   MOVE    1                   TO  SPA-GMN-CUR
      *
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *    点数マスタ該当データ読み出し
           INITIALIZE                            SPA-Z05
           INITIALIZE                            SPA-GMN-AREA
      *
           MOVE  SPA-Z01-SRYCD                   TO  SPA-GMN-SRYCDH
      *
           MOVE  SPA-Z01-YUKOSTYMD               TO  SPA-NAI-YUKOSTYMD
           IF  SPA-Z01-SYORIKBN                  =  2
               MOVE  SPA-Z01-YUKOSTYMDH          TO  SPA-GMN-YUKOSTYMDH
           END-IF
           MOVE  SPA-Z01-YUKOSTYMDH              TO  SPA-GMN-YUKOSTYMD
      *
           MOVE  SPA-Z01-YUKOEDYMD               TO  SPA-NAI-YUKOEDYMD
           IF  SPA-Z01-SYORIKBN                  =  2
                MOVE  SPA-Z01-YUKOEDYMDH         TO  SPA-GMN-YUKOEDYMDH
           END-IF
           MOVE  SPA-Z01-YUKOEDYMDH              TO  SPA-GMN-YUKOEDYMD
      *
      *    点数マスタ読込み
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           IF      SPA-Z01-SELNUM      =   ZERO
               MOVE    ALL "9"             TO  TNS-YUKOSTYMD
               MOVE    ALL "9"             TO  TNS-YUKOEDYMD
           ELSE
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNS-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNS-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        点数マスタ編集
               PERFORM 3001-TENSU-SET-SEC
           ELSE
      *        単位コードをゼロとする
               MOVE    ZERO            TO  SPA-GMN-TANICD
      *        金額種別
               IF      SPA-Z01-SRYCD(1:3)  =   "059"
                   MOVE    1           TO  SPA-GMN-TENSIKIBETU
               END-IF
           END-IF
      *    各リストボックス編集
           PERFORM 30012-COB-HEN-SEC
      *
      *****PERFORM  813-TENSUCLS-SEC
      *    点数マスタ付加情報検索
           IF      FLG-TENSU           =   ZERO
               MOVE    SPACE               TO  TENSUPLUS-REC
               MOVE    TNS-SRYCD           TO  TNSP-SRYCD
               MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
               MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 990-TENSUPLUS-READ-SEC
                   IF      FLG-TENSUPLUS       =   ZERO
                       MOVE    TNSP-INPCHKKBN  TO  SPA-GMN-INPCHKKBN
      *                帳票名称
                       MOVE    TNSP-SHONAME    TO  SPA-GMN-SHONAME
                   END-IF
               ELSE
                   INITIALIZE                 TENSUPLUS-REC
               END-IF
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBLOSEC-SEC
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ情報をＳＰＡにセット
      *****************************************************************
       3001-TENSU-SET-SEC                     SECTION.
      *
           MOVE    TNS-NAME            TO  SPA-GMN-NAMEH
      *
           MOVE    TNS-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    TNS-NAME            TO  SPA-GMN-NAME
           MOVE    TNS-FORMALNAME      TO  SPA-GMN-FORMALNAME
           MOVE    TNS-TEN             TO  SPA-GMN-TEN
           MOVE    TNS-TENSIKIBETU     TO  SPA-GMN-TENSIKIBETU
           MOVE    TNS-TENSIKIBETU     TO  SPA-GMN-TENSIKIBETU
           MOVE    TNS-JGNTEN          TO  SPA-GMN-JGNTEN
           MOVE    TNS-OLDTEN          TO  SPA-GMN-OLDTEN
           MOVE    TNS-OLDTENSKB       TO  SPA-GMN-OLDTENSKB
           MOVE    TNS-TANICD          TO  SPA-GMN-TANICD
           MOVE    TNS-TANINAME        TO  SPA-GMN-TANINAME
           MOVE    TNS-TOKUKIZAIAGEKSNKBN
                                       TO  SPA-GMN-TOKUKIZAIAGEKSNKBN
           MOVE    TNS-KGNAGE          TO  SPA-GMN-KGNAGE
           MOVE    TNS-JGNAGE          TO  SPA-GMN-JGNAGE
           MOVE    TNS-MEIUSESKB       TO  SPA-GMN-MEIUSESKB
           MOVE    TNS-SANSOKBN        TO  SPA-GMN-SANSOKBN
           MOVE    TNS-TOKUKIZAISBT1   TO  SPA-GMN-TOKUKIZAISBT1
           MOVE    TNS-TOKUKIZAISBT2   TO  SPA-GMN-TOKUKIZAISBT2
           MOVE    TNS-KOUHYOJYUNNUM   TO  SPA-GMN-KOUHYOJYUNNUM
           MOVE    TNS-DATAKBN         TO  SPA-GMN-DATAKBN  
           MOVE    TNS-KEIKASOCHIYMD   TO  SPA-NAI-KEIKASOCHIYMD
                                           WRK-LNKYMD
           IF      TNS-KEIKASOCHIYMD   =   SPACE
      *        経過措置日　空白はゼロとする
               MOVE    "00000000"          TO  SPA-GMN-KEIKASOCHIYMD
               MOVE    ZERO                TO  SPA-NAI-KEIKASOCHIYMD
           ELSE
               PERFORM 832-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-KEIKASOCHIYMD
           END-IF
           MOVE    TNS-CDKBN-BU        TO  SPA-GMN-CDKBN-BU
           MOVE    TNS-CDKBN-KBNNUM    TO  SPA-GMN-CDKBN-KBNNUM
      *
           .
       3001-TENSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   各リストボックス編集処理
      *****************************************************************
       30012-COB-HEN-SEC            SECTION.
      *
      *    データ区分
           MOVE    "0 なし"            TO  SPA-GMN-DATAKBNCOB (1)
           MOVE    "3 フィルム"        TO  SPA-GMN-DATAKBNCOB (2)
           MOVE    2                   TO  SPA-DATAKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DATAKBN-MAX
               IF      SPA-GMN-DATAKBN     =   SPA-GMN-DATAKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-DATAKBNCOB (IDX)
                                           TO  SPA-GMN-DATAKBN-G
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
      *    データ規格
      **** MOVE    1               TO  IDX-1
      **** MOVE    "000 "          TO  SPA-GMN-TANICDCOB(IDX-1)
           MOVE    ZERO            TO  SPA-TANICD-MAX
           MOVE    ZERO            TO  IDX-1
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       MTANI-MAX
               ADD     1               TO  IDX-1
               MOVE    MTANI-TANICD  (IDX)
                                       TO  SPA-GMN-TANICDCOB(IDX-1)(1:3)
               MOVE    MTANI-TANINAME(IDX)
                                       TO  SPA-GMN-TANICDCOB(IDX-1)(5:)
               MOVE    IDX-1           TO  SPA-TANICD-MAX
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TANICD-MAX
               IF      SPA-GMN-TANICD     =   SPA-GMN-TANICDCOB
                                                         (IDX)(1:3)
                   MOVE    SPA-GMN-TANICDCOB (IDX)
                                           TO  SPA-GMN-TANICD-G
                   MOVE    SPA-TANICD-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      *    点数識別
           MOVE    "0     "            TO  SPA-GMN-TENSIKIBETUCOB (1)
           MOVE    "1 金額"            TO  SPA-GMN-TENSIKIBETUCOB (2)
           MOVE    "2 都道府県購入価格"
                                       TO  SPA-GMN-TENSIKIBETUCOB (3)
           MOVE    "5 ％加算"          TO  SPA-GMN-TENSIKIBETUCOB (4)
           MOVE    "9 乗算割合"        TO  SPA-GMN-TENSIKIBETUCOB (5)
           MOVE    5                   TO  SPA-TENSIKI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TENSIKI-MAX
               IF      SPA-GMN-TENSIKIBETU =   SPA-GMN-TENSIKIBETUCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TENSIKIBETUCOB (IDX)
                                           TO  SPA-GMN-TENSIKIBETU-G
                   MOVE    SPA-TENSIKI-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
      *    旧点数識別
           MOVE    "0     "            TO  SPA-GMN-OLDTENSKBCOB (1)
           MOVE    "1 金額"            TO  SPA-GMN-OLDTENSKBCOB (2)
           MOVE    "2 都道府県購入価格"
                                       TO  SPA-GMN-OLDTENSKBCOB (3)
           MOVE    "5 ％加算"          TO  SPA-GMN-OLDTENSKBCOB (4)
           MOVE    "9 乗算割合"        TO  SPA-GMN-OLDTENSKBCOB (5)
           MOVE    5                   TO  SPA-OLDTENSIKI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OLDTENSIKI-MAX
               IF      SPA-GMN-OLDTENSKB =   SPA-GMN-OLDTENSKBCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-OLDTENSKBCOB (IDX)
                                           TO  SPA-GMN-OLDTENSKB-G
                   MOVE    SPA-OLDTENSIKI-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      *    加算年齢
           MOVE    "0 年齢加算なし"    TO  SPA-GMN-TOKUKIZAIAGEKSNCOB(1)
           MOVE    "1 年齢加算あり"    TO  SPA-GMN-TOKUKIZAIAGEKSNCOB(2)
           MOVE    2                   TO  SPA-TOKUKIZAI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAI-MAX
               IF      SPA-GMN-TOKUKIZAIAGEKSNKBN
                                       =   SPA-GMN-TOKUKIZAIAGEKSNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOKUKIZAIAGEKSNCOB (IDX)
                                       TO  SPA-GMN-TOKUKIZAIAGEKSNKBN-G
                   MOVE    SPA-TOKUKIZAI-MAX   TO  IDX
               END-IF
           END-PERFORM
      *    名称識別
           MOVE    "0 通常"            TO  SPA-GMN-MEIUSESKBCOB(1)
           MOVE    "1 医療機関登録"    TO  SPA-GMN-MEIUSESKBCOB(2)
           MOVE    2                   TO  SPA-MEIUSESKB-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIUSESKB-MAX
               IF      SPA-GMN-MEIUSESKB
                                       =   SPA-GMN-MEIUSESKBCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-MEIUSESKBCOB (IDX)
                                               TO  SPA-GMN-MEIUSESKB-G
                   MOVE    SPA-MEIUSESKB-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    酸素
           MOVE    "0 酸素、窒素以外"
                                       TO  SPA-GMN-SANSOKBNCOB (1)
           MOVE    "1 酸素補正率、高気圧酸素加算"
                                       TO  SPA-GMN-SANSOKBNCOB (2)
           MOVE    "2 定置式液化酸素貯槽"
                                       TO  SPA-GMN-SANSOKBNCOB (3)
           MOVE    "3 可搬式液化酸素容器"
                                       TO  SPA-GMN-SANSOKBNCOB (4)
           MOVE    "4 大型ボンベ"
                                       TO  SPA-GMN-SANSOKBNCOB (5)
           MOVE    "5 小型ボンベ"
                                       TO  SPA-GMN-SANSOKBNCOB (6)
           MOVE    "9 窒素"
                                       TO  SPA-GMN-SANSOKBNCOB (7)
           MOVE    7                   TO  SPA-SANSOKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANSOKBN-MAX
               IF      SPA-GMN-SANSOKBN    =   SPA-GMN-SANSOKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-SANSOKBNCOB (IDX)
                                           TO  SPA-GMN-SANSOKBN-G
                   MOVE    SPA-SANSOKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
      *
      *    特定器材１
           MOVE    "0 通常"
                                       TO  SPA-GMN-TOKUKIZAISBT1COB(1)
           MOVE    "2 酸素"
                                       TO  SPA-GMN-TOKUKIZAISBT1COB(2)
           MOVE    "3 高線量率イリジウム"
                                       TO  SPA-GMN-TOKUKIZAISBT1COB(3)
           MOVE    "4 コバルト"
                                       TO  SPA-GMN-TOKUKIZAISBT1COB(4)
           MOVE    4                   TO  SPA-TOKUKIZAISBT1-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAISBT1-MAX
               IF      SPA-GMN-TOKUKIZAISBT1
                                           =   SPA-GMN-TOKUKIZAISBT1COB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOKUKIZAISBT1COB (IDX)
                                           TO  SPA-GMN-TOKUKIZAISBT1-G
                   MOVE    SPA-TOKUKIZAISBT1-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    特定器材２（上限価格（酸素））
           MOVE    "0 下記以外"        TO  SPA-GMN-TOKUKIZAISBT2COB (1)
           MOVE    "1 上限価格設定"    TO  SPA-GMN-TOKUKIZAISBT2COB (2)
           IF      SPA-GMN-TOKUKIZAISBT2   =   "1"
               MOVE    SPA-GMN-TOKUKIZAISBT2COB (2)
                                       TO  SPA-GMN-TOKUKIZAISBT2-G
           ELSE
               MOVE    SPA-GMN-TOKUKIZAISBT2COB (1)
                                       TO  SPA-GMN-TOKUKIZAISBT2-G
           END-IF
           MOVE    2                   TO  SPA-TOKUKIZAISBT2-MAX
           .
      *
       30012-COB-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-ZID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
      *            削除
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4031-DEL-SEC
                   ELSE
                       MOVE    ZERO            TO  SPA-NAI-KEIFLG1
                   END-IF
               WHEN    "0002"
      *            登録
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4121-TOUROKU-SEC
                   ELSE
                       MOVE    ZERO            TO  SPA-NAI-KEIFLG2
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =  SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD      
           .
       330-ZID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE  "CHANGE"      TO  MCP-PUTTYPE
                   PERFORM  401-BACK-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        入力コード設定
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM  405-INPUTCD-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
      *        登録時、内容変更の無い場合、エラーとしない
               IF     (FLG-TOUROKU         =   1  )   AND
                      (SPA-Z01-SYORIKBN    =   2  )
      *            更新処理の時のみ
                   PERFORM 4103-HENKOU-CHK-SEC
               END-IF
           END-IF
      *
      *    登録時、必須入力チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-TOUROKU         =   1     )
               PERFORM 4109-TOUROKU-CHK-SEC
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
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    Z05-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    Z05-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
           MOVE    Z05-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    Z05-NAME            TO  SPA-GMN-NAME
           MOVE    Z05-FORMALNAME      TO  SPA-GMN-FORMALNAME
           MOVE    Z05-SHONAME         TO  SPA-GMN-SHONAME
      *
           MOVE    Z05-TEN             TO  SPA-GMN-TEN
           MOVE    Z05-TENSIKIBETU     TO  SPA-GMN-TENSIKIBETU-G
           MOVE    Z05-JGNTEN          TO  SPA-GMN-JGNTEN
           MOVE    Z05-OLDTEN          TO  SPA-GMN-OLDTEN
           MOVE    Z05-OLDTENSKB       TO  SPA-GMN-OLDTENSKB-G
           MOVE    Z05-DATAKBN         TO  SPA-GMN-DATAKBN-G
           MOVE    Z05-TOKUKIZAIAGEKSNKBN
                                       TO  SPA-GMN-TOKUKIZAIAGEKSNKBN-G
           MOVE    Z05-TANICD          TO  SPA-GMN-TANICD-G
           MOVE    Z05-KGNAGE          TO  SPA-GMN-KGNAGE
           MOVE    Z05-JGNAGE          TO  SPA-GMN-JGNAGE
           MOVE    Z05-MEIUSESKB       TO  SPA-GMN-MEIUSESKB-G
           MOVE    Z05-SANSOKBN        TO  SPA-GMN-SANSOKBN-G
           MOVE    Z05-TOKUKIZAISBT1   TO  SPA-GMN-TOKUKIZAISBT1-G
           MOVE    Z05-TOKUKIZAISBT2   TO  SPA-GMN-TOKUKIZAISBT2-G
           MOVE    Z05-KOUHYOJYUNNUM   TO  SPA-GMN-KOUHYOJYUNNUM
           MOVE    Z05-INPCHKKBN       TO  SPA-GMN-INPCHKKBN
           MOVE    Z05-KEIKASOCHIYMD   TO  SPA-GMN-KEIKASOCHIYMD
           MOVE    Z05-CDKBN-BU        TO  SPA-GMN-CDKBN-BU
           MOVE    Z05-CDKBN-KBNNUM    TO  SPA-GMN-CDKBN-KBNNUM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    有効開始年月日チェック
           PERFORM 501-YUKOSTYMDCHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    有効開始年月日チェック
           PERFORM 502-YUKOEDYMDCHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    カナ名称チェック
           PERFORM 503-KANANAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    漢字名称チェック
           PERFORM 504-NAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    通称名称チェック
           PERFORM 505-FORMALNAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    帳票名称チェック
           PERFORM 505-SHONAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    金額種別チェック
           PERFORM 506-TENSIKIBETU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    旧金額種別チェック
           PERFORM 506-OLDTENSKB-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    単位コード
           PERFORM 507-TANICD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    データ区分
           PERFORM 507-DATAKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    年齢加算
           PERFORM 507-TOKUKIZAI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    下限年齢
           PERFORM 5002-KGNAGE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    上限年齢
           PERFORM 5003-JGNAGE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    名称識別
           PERFORM 5003-MEIUSESKB-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    酸素等
           PERFORM 5003-SANSOKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    特定器材種別１
           PERFORM 5003-TOKUKIZAISBT1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    特定器材種別２
           PERFORM 5003-TOKUKIZAISBT2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    経過措置年月日チェック
           PERFORM 5003-KEIKASOCHIYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    別表番号チェック
           PERFORM 5003-CDKBN-BU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    区分番号チェック
           PERFORM 5003-CDKBN-KBNNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効開始年月日チェック
      *****************************************************************
       501-YUKOSTYMDCHK-SEC            SECTION.
      *
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "0000000"           TO  SPA-GMN-YUKOSTYMD
           END-IF
      *
           MOVE    SPA-GMN-YUKOSTYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOSTYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOSTYMD
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       501-YUKOSTYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効終了年月日チェック
      *****************************************************************
       502-YUKOEDYMDCHK-SEC                      SECTION.
      *
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "9999999"           TO  SPA-GMN-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-GMN-YUKOEDYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOEDYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOEDYMD
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       502-YUKOEDYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ名称チェック処理
      *****************************************************************
       503-KANANAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-KANANAME    TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANANAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANANAME
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       503-KANANAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    漢字名称チェック処理
      *****************************************************************
       504-NAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-NAME        TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NAME
           ELSE
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       504-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    通称名称チェック処理
      *****************************************************************
       505-FORMALNAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-FORMALNAME  TO  WRK-MAE-INPUT
           MOVE    200                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-FORMALNAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-FORMALNAME
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       505-FORMALNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票名称チェック処理
      *****************************************************************
       505-SHONAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SHONAME     TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 410-ZENCHK-HEN-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-SHONAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-SHONAME
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    25                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       505-SHONAME-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *  　金額種別チェック
      *****************************************************************
       506-TENSIKIBETU-CHK-SEC                      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TENSIKI-MAX
               IF      SPA-GMN-TENSIKIBETU =   SPA-GMN-TENSIKIBETUCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TENSIKIBETUCOB (IDX)
                                               TO  SPA-GMN-TENSIKIBETU-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-TENSIKI-MAX     TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       506-TENSIKIBETU-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *  　金額種別チェック
      *****************************************************************
       506-OLDTENSKB-CHK-SEC                      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OLDTENSIKI-MAX
               IF      SPA-GMN-OLDTENSKB =   SPA-GMN-OLDTENSKBCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-OLDTENSKBCOB (IDX)
                                               TO  SPA-GMN-OLDTENSKB-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-OLDTENSIKI-MAX  TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       506-OLDTENSKB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　単位コードチェック
      *****************************************************************
       507-TANICD-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-TANICD      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-TANICD
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-TANICD      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
               GO      TO              507-TANICD-CHK-EXT
           ELSE
               MOVE    SNUM-OUTEDT(13:3)       TO  SPA-GMN-TANICD
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-TANICD-MAX  )   OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-TANICD     =   SPA-GMN-TANICDCOB
                                                         (IDX)(1:3)
                   MOVE    SPA-GMN-TANICDCOB (IDX)
                                               TO  SPA-GMN-TANICD-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-TANICD-MAX      TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    11                  TO  SPA-GMN-CUR
               MOVE    "0008"              TO  SPA-ERRCD
           END-IF
           .
      *
       507-TANICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　データ区分チェック
      *****************************************************************
       507-DATAKBN-CHK-SEC                      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DATAKBN-MAX
               IF      SPA-GMN-DATAKBN =   SPA-GMN-DATAKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-DATAKBNCOB (IDX)
                                               TO  SPA-GMN-DATAKBN-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-DATAKBN-MAX     TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       507-DATAKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　加算年齢チェック
      *****************************************************************
       507-TOKUKIZAI-CHK-SEC                      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAI-MAX
               IF      SPA-GMN-TOKUKIZAIAGEKSNKBN
                                       =   SPA-GMN-TOKUKIZAIAGEKSNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOKUKIZAIAGEKSNCOB (IDX)
                                       TO  SPA-GMN-TOKUKIZAIAGEKSNKBN-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-TOKUKIZAI-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       507-TOKUKIZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　下限年齢チェック
      *****************************************************************
       5002-KGNAGE-CHK-SEC                      SECTION.
      *
           IF      SPA-GMN-KGNAGE      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-KGNAGE
           END-IF
      *
           IF      SPA-GMN-KGNAGE      =   "AA"
                                       OR  "B3"
                                       OR  "BF"
               GO      TO      5002-KGNAGE-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-KGNAGE      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    14                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-KGNAGE
           END-IF
           .
      *
       5002-KGNAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　上限年齢チェック
      *****************************************************************
       5003-JGNAGE-CHK-SEC                      SECTION.
      *
           IF      SPA-GMN-JGNAGE      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-JGNAGE
           END-IF
      *
           IF      SPA-GMN-JGNAGE      =   "AA"
                                       OR  "B3"
                                       OR  "BF"
               CONTINUE
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JGNAGE      TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    15                  TO  SPA-GMN-CUR
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-JGNAGE
               END-IF
           END-IF
           .
      *
       5003-JGNAGE-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    名称識別チェック処理
      *****************************************************************
       5003-MEIUSESKB-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIUSESKB-MAX
               IF      SPA-GMN-MEIUSESKB
                                       =   SPA-GMN-MEIUSESKBCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-MEIUSESKBCOB (IDX)
                                               TO  SPA-GMN-MEIUSESKB-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-MEIUSESKB-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    16                  TO  SPA-GMN-CUR
           END-IF
           .
      *
      *
       5003-MEIUSESKB-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    酸素等チェック処理
      *****************************************************************
       5003-SANSOKBN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANSOKBN-MAX
               IF      SPA-GMN-SANSOKBN    =   SPA-GMN-SANSOKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-SANSOKBNCOB (IDX)
                                           TO  SPA-GMN-SANSOKBN-G
                   MOVE    SPA-SANSOKBN-MAX    TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    17                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5003-SANSOKBN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    特定器材種別１チェック処理
      *****************************************************************
       5003-TOKUKIZAISBT1-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAISBT1-MAX
               IF      SPA-GMN-TOKUKIZAISBT1
                                           =   SPA-GMN-TOKUKIZAISBT1COB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOKUKIZAISBT1COB (IDX)
                                           TO  SPA-GMN-TOKUKIZAISBT1-G
                   MOVE    SPA-TOKUKIZAISBT1-MAX   TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    18                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5003-TOKUKIZAISBT1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定器材種別２チェック処理
      *****************************************************************
       5003-TOKUKIZAISBT2-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAISBT2-MAX
               IF      SPA-GMN-TOKUKIZAISBT2
                                           =   SPA-GMN-TOKUKIZAISBT2COB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOKUKIZAISBT2COB (IDX)
                                           TO  SPA-GMN-TOKUKIZAISBT2-G
                   MOVE    SPA-TOKUKIZAISBT2-MAX   TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0016"              TO  SPA-ERRCD
               MOVE    19                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5003-TOKUKIZAISBT2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　経過措置年月日チェック
      *****************************************************************
       5003-KEIKASOCHIYMD-CHK-SEC  SECTION.
      *
           IF      SPA-GMN-KEIKASOCHIYMD   =   SPACE
               MOVE    "0000000"           TO  SPA-GMN-KEIKASOCHIYMD
           END-IF
      *
           MOVE    SPA-GMN-KEIKASOCHIYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-KEIKASOCHIYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-KEIKASOCHIYMD
           ELSE
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    21                  TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       5003-KEIKASOCHIYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  別表番号チェック
      *****************************************************************
       5003-CDKBN-BU-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-CDKBN-BU      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CDKBN-BU
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-CDKBN-BU    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    22                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-CDKBN-BU
           END-IF
           .
      *
       5003-CDKBN-BU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  区分番号チェック
      *****************************************************************
       5003-CDKBN-KBNNUM-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-CDKBN-KBNNUM    =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CDKBN-KBNNUM
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-CDKBN-KBNNUM    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0019"              TO  SPA-ERRCD
               MOVE    23                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(13:3)   TO  SPA-GMN-CDKBN-KBNNUM
           END-IF
           .
      *
       5003-CDKBN-KBNNUM-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、必須入力チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC      SECTION.
      *
      *    登録時、必須入力チェック
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
           IF      SPA-NAI-YUKOSTYMD   >=  SPA-NAI-YUKOEDYMD
               MOVE    "1010"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
      *    有効期限範囲チェック
           IF      SPA-Z01-SYORIKBN    =   2
      *        前チェック
               IF      SPA-Z01-SELNUM      >   1
                   MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM - 1)
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
      **********   IF      SPA-NAI-YUKOSTYMD   NOT =   WRK-OTYMD
                   IF      SPA-NAI-YUKOSTYMD       <   WRK-OTYMD
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
      *        後チェック
               IF      SPA-Z01-SELNUM      <   SPA-Z01-MAX
                   MOVE    SPA-NAI-YUKOEDYMD
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
      **********   IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM + 1)
      ********                                 NOT =   WRK-OTYMD
                   IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM + 1)
                                                   <   WRK-OTYMD
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *    改正チェック
           IF      SPA-Z01-SYORIKBN    =   3
               IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           >=  SPA-NAI-YUKOSTYMD
                   MOVE    "1012"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
      *
               IF  SPA-NAI-YUKOEDYMD    NOT    =  ALL "9"
                   MOVE    "1013"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
      *
      *H24.8
      *        器材は期間変更警告
               IF      SPA-Z01-SRYCD(1:1)  =   "7"
                   IF      SPA-NAI-KEIFLG3     =   ZERO
                       MOVE    "K005"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI-KEIFLG3
                       MOVE    1               TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *H24.8
      *    器材の期間変更チェック
           IF      SPA-Z01-SELNUM      >=  1
               IF      (SPA-Z01-SRYCD(1:1)  =   "7"      )
               AND    ((SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       NOT =   SPA-NAI-YUKOSTYMD)
               OR      (SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       NOT =   SPA-NAI-YUKOEDYMD))
                   IF      SPA-NAI-KEIFLG3     =   ZERO
                       MOVE    "K004"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI-KEIFLG3
                       MOVE    1               TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-GMN-NAME        =   SPACE
               MOVE    "1014"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           .
      *
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       401-BACK-SEC                              SECTION.
      *
           MOVE  "Z01"                           TO  SPA-SAKIPG.
      *
           MOVE  "Z05"                           TO  SPA-MOTOPG.
      *
      ***  MOVE  "CHANGE"                        TO  MCP-PUTTYPE.
           MOVE  SPA-SAKIPG                      TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END
           .
       401-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           IF      SPA-NAI-KEIFLG1     =   ZERO
               IF      SPA-Z01-SRYCD(1:1)  =   "7"
                   MOVE    "K003"          TO  SPA-ERRCD
               END-IF
               MOVE    1               TO  SPA-NAI-KEIFLG1
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0001"              TO  SPA-ZIDCD
           END-IF
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF     MCP-RC           NOT =   ZERO
               DISPLAY "Z05 TENSU DEL ERR!!"     MCP-RC
                       ",KEY:" TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
      *    点数付加情報削除
           MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNSP-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Z03 TENSUPLUS DEL ERR!!"   MCP-RC
                       ",KEY:"   TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
      *    入力コード削除
           IF     (SPA-Z01-SELNUM      =   1    )  AND
                  (SPA-Z01-MAX         =   1    )
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    SPA-Z01-SRYCD       TO  ICD-SRYCD
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "del22"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           IF      SPA-ERRCD          =   SPACE
               MOVE  "JOIN"               TO  MCP-PUTTYPE
               PERFORM  401-BACK-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード　設定処理
      *****************************************************************
       405-INPUTCD-SEC                              SECTION.
      *
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z05             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ99"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA       TO  SPA-Z05
      *
      *    初期カーソル
           MOVE    "INPUTCD01"         TO  MCP-WIDGET
      *
           MOVE    "Z99"               TO  SPA-SAKIPG.
           MOVE    "Z05"               TO  SPA-MOTOPG.
      *
           MOVE    SPA-MOTOPG          TO  SPA-Z99GMN-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
       405-INPUTCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SEC                           SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-TOUROKU-EXT
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z01-SYORIKBN    =   1
      *        新規処理
                   IF      SPA-NAI-KEIFLG2     =   ZERO
                       IF      SPA-Z01-SRYCD(1:1)  =   "7"
                           MOVE    "K001"          TO  SPA-ERRCD
                       END-IF
                       MOVE    1               TO  SPA-NAI-KEIFLG2
                   END-IF
               END-IF
           END-IF
      *    登録確認メッセージ
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0002"              TO  SPA-ZIDCD
           END-IF
           .    
       412-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、内容変更チェック処理
      ****************************************************************
       4103-HENKOU-CHK-SEC      SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-TENSU-REC(1:1290)   TO  WRK-TNS-TENSU-REC
      *        更新項目編集
               PERFORM 4121-KOUMOKUISO-SEC
               IF      TNS-TENSU-REC(1:1290)   =   WRK-TNS-TENSU-REC
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4103-HENKOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4121-TOUROKU-SEC                          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           EVALUATE    SPA-Z01-SYORIKBN
      *        改正時処理
               WHEN    3
                   PERFORM 41211-KAISEI-SYORI-SEC
      *　          新規処理
                   PERFORM 41213-INSERT-SYORI-SEC
      *　      更新処理
               WHEN    2
                   PERFORM 41212-UPDATE-SYORI-SEC
      *　      新規処理
               WHEN    1
                   PERFORM 41213-INSERT-SYORI-SEC
           END-EVALUATE
      *
      *    点数マスタ付加情報更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4129-TENSUPLUS-UPD-SEC
           END-IF
      *
      *    入力コード更新
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGZS01"          USING
                                           MCPAREA
                                           SPAAREA
           END-IF
      *    前画面に戻る
           IF      SPA-ERRCD           =   SPACE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
               PERFORM 401-BACK-SEC
           END-IF
           .
       4121-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    改正時処理
      *****************************************************************
       41211-KAISEI-SYORI-SEC                       SECTION.
      *
      *    有効終了日を決定する
      *    今回の有効開始日の前日とする
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-YUKOSTYMD   TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-YUKOEDYMD
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41211-KAISEI-SYORI-EXT
           END-IF
      *
      *    有効年月日を更新する
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新
               MOVE    WRK-YUKOEDYMD       TO  TNS-YUKOEDYMD
      *
               MOVE    SPA-Z01-SRYCD       TO  TNSUP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOEDYMD
      *
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z05 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNS-KEY
                   MOVE    1                     TO  SPA-GMN-CUR
                   MOVE    "9002"                TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41211-KAISEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41212-UPDATE-SYORI-SEC                       SECTION.
      *
      *
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新項目編集
               MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
               MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
               PERFORM 4121-KOUMOKUISO-SEC
      *
               MOVE    SPA-Z01-SRYCD       TO  TNSUP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOEDYMD
      *
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z05 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNS-KEY
                   MOVE    1                     TO  SPA-GMN-CUR
                   MOVE    "9002"                TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41212-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41213-INSERT-SYORI-SEC          SECTION.
      *
      *    データＩＮＳＥＲＴ
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
           MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
           PERFORM 4121-KOUMOKUISO-SEC
      *
           MOVE    SMCNDATE-YMD        TO  TNS-CREYMD
           MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
           MOVE    SPA-OPID            TO  TNS-OPID
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z05 TENSU INS ERR:" MCP-RC
                       ",KEY:" TNS-KEY
           END-IF
      *
           .
       41213-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録編集処理
      *****************************************************************
       4121-KOUMOKUISO-SEC                       SECTION.
      *
           MOVE  SPA-GMN-KANANAME          TO  TNS-KANANAME
                                               WRK-STRINGTBL
           MOVE  100                       TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                TO  TNS-KANAYUKOKETA
      *
           MOVE  SPA-GMN-NAME              TO  TNS-NAME
                                               WRK-STRINGTBL
           MOVE  100                       TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                TO  TNS-YUKOKETA
      *
           MOVE  SPA-GMN-FORMALNAME        TO  TNS-FORMALNAME
                                               WRK-STRINGTBL
           MOVE  200                       TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                TO  TNS-FORMALYUKOKETA
      *
           MOVE  SPA-GMN-TEN               TO  TNS-TEN
           MOVE  SPA-GMN-TENSIKIBETU       TO  TNS-TENSIKIBETU
           MOVE  SPA-GMN-JGNTEN            TO  TNS-JGNTEN
           MOVE  SPA-GMN-OLDTEN            TO  TNS-OLDTEN
           MOVE  SPA-GMN-OLDTENSKB         TO  TNS-OLDTENSKB
           MOVE  SPA-GMN-TANICD            TO  TNS-TANICD
           MOVE  SPA-GMN-TANINAME          TO  TNS-TANINAME
           MOVE  SPA-GMN-TANINAME          TO  WRK-STRINGTBL
           MOVE  12                        TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                TO  TNS-TANIMOJISU
           MOVE  SPA-GMN-TOKUKIZAIAGEKSNKBN
                                           TO  TNS-TOKUKIZAIAGEKSNKBN
           MOVE  SPA-GMN-KGNAGE            TO  TNS-KGNAGE
           MOVE  SPA-GMN-JGNAGE            TO  TNS-JGNAGE
           MOVE  SPA-GMN-MEIUSESKB         TO  TNS-MEIUSESKB
           MOVE  SPA-GMN-SANSOKBN          TO  TNS-SANSOKBN
           MOVE  SPA-GMN-TOKUKIZAISBT1     TO  TNS-TOKUKIZAISBT1
           MOVE  SPA-GMN-TOKUKIZAISBT2     TO  TNS-TOKUKIZAISBT2
           MOVE  SPA-GMN-KOUHYOJYUNNUM     TO  TNS-KOUHYOJYUNNUM
           MOVE  SPA-GMN-DATAKBN           TO  TNS-DATAKBN  
           MOVE  SPA-NAI-KEIKASOCHIYMD     TO  TNS-KEIKASOCHIYMD
           MOVE  SPA-GMN-CDKBN-BU          TO  TNS-CDKBN-BU
           MOVE  SPA-GMN-CDKBN-KBNNUM      TO  TNS-CDKBN-KBNNUM
           .
       4121-KOUMOKUISO-EXT.
           EXIT.
      *
      *****************************************************************
      *     点数マスタ付加情報更新処理
      *****************************************************************
       4129-TENSUPLUS-UPD-SEC                       SECTION.
      *
      *
           INITIALIZE                  ORCGZS02AREA
           MOVE    SPA-NAI-YUKOSTYMD   TO  ZS02-NAI-YUKOSTYMD
           MOVE    SPA-NAI-YUKOEDYMD   TO  ZS02-NAI-YUKOEDYMD
           MOVE    WRK-YUKOEDYMD       TO  ZS02-YUKOEDYMD
      *
           INITIALIZE                  TENSUPLUS-REC
           MOVE    SPA-GMN-INPCHKKBN   TO  TNSP-INPCHKKBN
      *    処方名称
           MOVE    SPA-GMN-SHONAME     TO  TNSP-SHONAME
                                           WRK-STRINGTBL
           MOVE    100                 TO  CNT-STRING
           PERFORM 891-STRCOUNT-SEC
           MOVE    CNT-STRING          TO  TNSP-SHOYUKOKETA
      *    入力コード更新
           CALL    "ORCGZS02"          USING
                                       MCPAREA
                                       SPAAREA
                                       ORCGZS02AREA
                                       TENSUPLUS-REC
      *
           .
       4129-TENSUPLUS-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角チェック処理
      *****************************************************************
       400-ZENCHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *        全角半角混在エラー
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT 
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT 
           END-IF
           .
      *
       400-ZENCHK-EXT.
           EXIT.
      *****************************************************************
      *    半角変換（チェック）処理
      *****************************************************************
       410-ZENCHK-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT 
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT 
           END-IF
           .
      *
       410-ZENCHK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN-SEC             SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-ZIDCD       NOT =   SPACE
               PERFORM 520-ZIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面セット処理
      *****************************************************************
       500-GMNHEN-SEC                            SECTION.
      *
      *     画面ワークエリアにセット
           MOVE    SPACE               TO  Z05
      *
           MOVE  SPA-GMN-SRYCDH                TO  Z05-SRYCDH
           MOVE  SPA-GMN-NAMEH                 TO  Z05-NAMEH
           MOVE  SPA-GMN-YUKOSTYMDH            TO  Z05-YUKOSTYMDH
           MOVE  SPA-GMN-YUKOEDYMDH            TO  Z05-YUKOEDYMDH
           MOVE  SPA-GMN-YUKOSTYMD             TO  Z05-YUKOSTYMD
           MOVE  SPA-GMN-YUKOEDYMD             TO  Z05-YUKOEDYMD
           MOVE  SPA-GMN-KANANAME              TO  Z05-KANANAME
           MOVE  SPA-GMN-NAME                  TO  Z05-NAME
           MOVE  SPA-GMN-FORMALNAME            TO  Z05-FORMALNAME
           MOVE  SPA-GMN-SHONAME               TO  Z05-SHONAME
           MOVE  SPA-GMN-TEN                   TO  Z05-TEN
           MOVE  SPA-GMN-TENSIKIBETU-G         TO  Z05-TENSIKIBETU
           MOVE  SPA-GMN-JGNTEN                TO  Z05-JGNTEN
           MOVE  SPA-GMN-OLDTEN                TO  Z05-OLDTEN
           MOVE  SPA-GMN-OLDTENSKB-G           TO  Z05-OLDTENSKB
           MOVE  SPA-GMN-TANICD-G              TO  Z05-TANICD
     ******MOVE  SPA-GMN-TANINAME              TO  Z05-TANINAME
           MOVE  SPA-GMN-TOKUKIZAIAGEKSNKBN-G  TO
                                               Z05-TOKUKIZAIAGEKSNKBN
           MOVE  SPA-GMN-KGNAGE                TO  Z05-KGNAGE
           MOVE  SPA-GMN-JGNAGE                TO  Z05-JGNAGE
           MOVE  SPA-GMN-MEIUSESKB-G           TO  Z05-MEIUSESKB
           MOVE  SPA-GMN-SANSOKBN-G            TO  Z05-SANSOKBN
           MOVE  SPA-GMN-TOKUKIZAISBT1-G       TO  Z05-TOKUKIZAISBT1
           MOVE  SPA-GMN-TOKUKIZAISBT2-G       TO  Z05-TOKUKIZAISBT2
           MOVE  SPA-GMN-KOUHYOJYUNNUM         TO  Z05-KOUHYOJYUNNUM
           MOVE  SPA-GMN-DATAKBN-G             TO  Z05-DATAKBN
      *
           MOVE  SPA-GMN-INPCHKKBN             TO  Z05-INPCHKKBN
      *
           MOVE  SPA-GMN-KEIKASOCHIYMD         TO  Z05-KEIKASOCHIYMD
           MOVE  SPA-GMN-CDKBN-BU              TO  Z05-CDKBN-BU
           MOVE  SPA-GMN-CDKBN-KBNNUM          TO  Z05-CDKBN-KBNNUM
      *
      *    データ区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DATAKBN-MAX
               MOVE    SPA-GMN-DATAKBNCOB (IDX)
                                           TO  Z05-DATAKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-DATAKBN-MAX     TO  Z05-DATAKBN-COUNT
      *    データ規格
           MOVE    ZERO                TO  IDX-1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TANICD-MAX
               IF      SPA-GMN-TANICDCOB (IDX) NOT =   SPACE
                   ADD     1               TO  IDX-1
                   MOVE    SPA-GMN-TANICDCOB (IDX)
                                           TO  Z05-TANICD-ITEM (IDX-1)
               END-IF
           END-PERFORM
           MOVE    SPA-TANICD-MAX      TO  Z05-TANICD-COUNT
      *    点数識別
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TENSIKI-MAX
               MOVE    SPA-GMN-TENSIKIBETUCOB (IDX)
                                       TO  Z05-TENSIKIBETU-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-TENSIKI-MAX     TO  Z05-TENSIKIBETU-COUNT
      *     旧点数識別
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OLDTENSIKI-MAX
               MOVE    SPA-GMN-OLDTENSKBCOB (IDX)
                                           TO  Z05-OLDTENSKB-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-OLDTENSIKI-MAX      TO  Z05-OLDTENSKB-COUNT
      *
      *    加算年齢
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAI-MAX
               MOVE    SPA-GMN-TOKUKIZAIAGEKSNCOB (IDX)
                                           TO  Z05-TOKUKIZAIAGEKSN-ITEM
                                                                (IDX)
           END-PERFORM
           MOVE    SPA-TOKUKIZAI-MAX   TO  Z05-TOKUKIZAIAGEKSN-COUNT
      *    名称識別
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIUSESKB-MAX
               MOVE    SPA-GMN-MEIUSESKBCOB (IDX)
                                           TO  Z05-MEIUSESKB-ITEM
                                                                (IDX)
           END-PERFORM
           MOVE    SPA-MEIUSESKB-MAX   TO  Z05-MEIUSESKB-COUNT
      *    加算年齢
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANSOKBN-MAX
               MOVE    SPA-GMN-SANSOKBNCOB (IDX)
                                           TO  Z05-SANSOKBN-ITEM
                                                                (IDX)
           END-PERFORM
           MOVE    SPA-SANSOKBN-MAX    TO  Z05-SANSOKBN-COUNT
      *    特定器材１
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAISBT1-MAX
               MOVE    SPA-GMN-TOKUKIZAISBT1COB (IDX)
                                           TO  Z05-TOKUKIZAISBT1-ITEM
                                                                (IDX)
           END-PERFORM
           MOVE    SPA-TOKUKIZAISBT1-MAX  TO  Z05-TOKUKIZAISBT1-COUNT
      *    特定器材１
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAISBT2-MAX
               MOVE    SPA-GMN-TOKUKIZAISBT2COB (IDX)
                                           TO  Z05-TOKUKIZAISBT2-ITEM
                                                                (IDX)
           END-PERFORM
           MOVE    SPA-TOKUKIZAISBT2-MAX   TO  Z05-TOKUKIZAISBT2-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
               WHEN    2
                   MOVE    "YUKOEDYMD"         TO  MCP-WIDGET
               WHEN    3
                   MOVE    "KANANAME"          TO  MCP-WIDGET
               WHEN    4
                   MOVE    "NAME"              TO  MCP-WIDGET
               WHEN    5
                   MOVE    "FORMALNAME"        TO  MCP-WIDGET
               WHEN    25
                   MOVE    "SHONAME"           TO  MCP-WIDGET
               WHEN    6
                   MOVE    "TEN"               TO  MCP-WIDGET
               WHEN    7
                   MOVE    "TENSIKIBETU"       TO  MCP-WIDGET
               WHEN    8
                   MOVE    "JGNTEN"               TO  MCP-WIDGET
               WHEN    9
                   MOVE    "OLDTEN"                TO  MCP-WIDGET
               WHEN    10
                   MOVE    "OLDTENSKB"             TO  MCP-WIDGET
               WHEN    11
                   MOVE    "TANICD"                TO  MCP-WIDGET
               WHEN    12
                   MOVE    "DATAKBN"               TO  MCP-WIDGET
               WHEN    13
                   MOVE    "TOKUKIZAIAGEKSNKBN"    TO  MCP-WIDGET
               WHEN    14
                   MOVE    "KGNAGE"                TO  MCP-WIDGET
               WHEN    15
                   MOVE    "JGNAGE"                TO  MCP-WIDGET
               WHEN    16
                   MOVE    "MEIUSESKB"             TO  MCP-WIDGET
               WHEN    17
                   MOVE    "SANSOKBN"              TO  MCP-WIDGET
               WHEN    18
                   MOVE    "TOKUKIZAISBT1"         TO  MCP-WIDGET
               WHEN    19
                   MOVE    "TOKUKIZAISBT2"         TO  MCP-WIDGET
               WHEN    20
                   MOVE    "KOUHYOJYUNNUM"         TO  MCP-WIDGET
               WHEN    21
                   MOVE    "KEIKASOCHIYMD"         TO  MCP-WIDGET
               WHEN    22
                   MOVE    "CDKBN_BU"              TO  MCP-WIDGET
               WHEN    23
                   MOVE    "CDKBN_KBNNUM"          TO  MCP-WIDGET
               WHEN    24
                   MOVE    "INPCHKKBN"             TO  MCP-WIDGET
               WHEN    40
                   MOVE    "B12"                   TO  MCP-WIDGET
             END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルセット処理
      *****************************************************************
       50011-CUR-SET-SEC                            SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "YUKOSTYMD"
                   MOVE    2                   TO  SPA-GMN-CUR
               WHEN    "YUKOEDYMD"
                   MOVE    3                   TO  SPA-GMN-CUR
               WHEN    "KANANAME"
                   MOVE    4                   TO  SPA-GMN-CUR
               WHEN    "NAME"
                   MOVE    5                   TO  SPA-GMN-CUR
               WHEN    "FORMALNAME"
      *****        MOVE    6                   TO  SPA-GMN-CUR
                   MOVE    25                  TO  SPA-GMN-CUR
               WHEN    "SHONAME"
                   MOVE    6                   TO  SPA-GMN-CUR
               WHEN    "TEN"
                   MOVE    7                   TO  SPA-GMN-CUR
               WHEN    "TENSIKIBETU"
                   MOVE    8                   TO  SPA-GMN-CUR
               WHEN    "JGNTEN"
                   MOVE    9                   TO  SPA-GMN-CUR
               WHEN    "OLDTEN"
                   MOVE    10                  TO  SPA-GMN-CUR
               WHEN    "OLDTENSKB"
                   MOVE    11                  TO  SPA-GMN-CUR
               WHEN    "TANICD"
                   MOVE    12                  TO  SPA-GMN-CUR
               WHEN    "DATAKBN"
                   MOVE    13                  TO  SPA-GMN-CUR
               WHEN    "TOKUKIZAIAGEKSNKBN"
                   MOVE    14                  TO  SPA-GMN-CUR
               WHEN    "KGNAGE"
                   MOVE    15                  TO  SPA-GMN-CUR
               WHEN    "JGNAGE"
                   MOVE    16                  TO  SPA-GMN-CUR
               WHEN    "MEIUSESKB"
                   MOVE    17                  TO  SPA-GMN-CUR
               WHEN    "SANSOKBN"
                   MOVE    18                  TO  SPA-GMN-CUR
               WHEN    "TOKUKIZAISBT1"
                   MOVE    19                  TO  SPA-GMN-CUR
               WHEN    "TOKUKIZAISBT2"
                   MOVE    20                  TO  SPA-GMN-CUR
               WHEN    "KOUHYOJYUNNUM"
                   MOVE    21                  TO  SPA-GMN-CUR
               WHEN    "KEIKASOCHIYMD"
                   MOVE    22                  TO  SPA-GMN-CUR
               WHEN    "CDKBN_BU"
                   MOVE    23                  TO  SPA-GMN-CUR
               WHEN    "CDKBN_KBNNUM"
                   MOVE    24                  TO  SPA-GMN-CUR
               WHEN    "INPCHKKBN"
                   MOVE    40                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC                            SECTION.
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "有効開始年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "有効終了年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "カナ名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "漢字名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "正式名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "出力名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "金額種別がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "旧金額種別がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "単位コードがありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "データ区分がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "年齢加算がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "下限年齢がエラーです"
                                                 TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "上限年齢がエラーです"
                                                 TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "名称識別がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "酸素等がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "特定器材種別１がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0016"
      ***********  MOVE    "特定器材種別２がありません"
                   MOVE    "上限価格（酸素）がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "経過措置年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "別表番号が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "区分番号が違います"
                                                 TO  SPA-ERRMSG
      *        以下共通
               WHEN    "1010"
                   MOVE
                   "有効開始年月日≧有効終了年月日で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "有効年月日の範囲が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "有効年月日が重複しています"
                                                 TO  SPA-ERRMSG
               WHEN    "1013"
                   MOVE    "改正の終了年月日はＡＬＬ９のみ入力できます"
                                                 TO  SPA-ERRMSG
               WHEN    "1014"
                   MOVE    "漢字名称を入力してください"
                                                 TO  SPA-ERRMSG
              WHEN    "1001"
                   MOVE    "入力コード更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "削除処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "更新処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "追加処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9004"
                   MOVE    "点数マスタ付加情報が更新できません"
                                                 TO  SPA-ERRMSG
               WHEN    "K001"
                   STRING    "警告！厚生労働省から提供される"
                                                   DELIMITED BY  SIZE
                             "コードの範囲です。"
                                                   DELIMITED BY  SIZE
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K002"
                   STRING    "警告！ユーザ登録できる範囲のコード"
                                                   DELIMITED BY  SIZE
                             "ではありません。"
                                                   DELIMITED BY  SIZE
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K003"
                   STRING   "警告！厚生労働省から提供されたデータです。"
                                                   DELIMITED BY  SIZE
                            "削除しないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *H24.8
               WHEN    "K004"
                   STRING    "警告！"
                             "厚生労働省から提供される範囲のコードです"
                             "。有効年月日の変更はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K005"
                   STRING    "警告！"
                             "厚生労働省から提供される範囲のコードです"
                             "。改正処理はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *
          END-EVALUATE.
      *
           MOVE    SPACE                         TO  ZERR
           MOVE    SPA-ERRCD                     TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG                TO  ZERR-ERRMSG
      *
           MOVE    "Z05"                         TO  SPA-MOTOPG
           MOVE    "ZERR"                        TO  SPA-SAKIPG
      *
           MOVE    "NEW"                         TO  MCP-PUTTYPE
           MOVE    "ZERR"                        TO  MCP-WINDOW
           MOVE    "B01"                         TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END    
           .
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-ZIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
                   MOVE    "削除します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0002"
                   MOVE    "更新します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z05"                TO  SPA-MOTOPG
           MOVE    "ZID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-ZIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　処理
      *****************************************************************
      *
      *
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       990-TENSUPLUS-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .
      *
       990-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBLOSEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *****************************************************************
      *    日付変換　処理
      *****************************************************************
      *****************************************************************
      *     対象日＋１日計算処理
      *****************************************************************
       800-DAYSET-SEC                  SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           EVALUATE    WRK-YMD
               WHEN    ZERO
                   MOVE    ZERO            TO  WRK-OTYMD
               WHEN    "99999999"
                   MOVE    ALL "9"         TO  WRK-OTYMD
               WHEN    OTHER
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    1                   TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    LNK-DAY6-KEISAN      TO  WRK-OTYMD
                   ELSE
                       MOVE    SPACE                TO  WRK-OTYMD
                   END-IF
           END-EVALUATE
      *
           .
       800-DAYSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:7)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
               WHEN  "9999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
               WHEN  OTHER
      *
      *************IF  WRK-LNKYMD(1:7)           NOT NUMERIC
      *                MOVE    WRK-LNKYMD(1:1)   TO  WRK-WGO
      *                MOVE    WRK-LNKYMD(2:2)   TO  WRK-WYY
      *                MOVE    WRK-LNKYMD(5:2)   TO  WRK-WMM
      *                MOVE    WRK-LNKYMD(8:2)   TO  WRK-WDD
      *                INSPECT  WRK-WYMD  REPLACING   ALL " " BY  "0"
      *            ELSE
      *                MOVE    WRK-LNKYMD(1:7)   TO  WRK-WYMD
      *            END-IF
      *            INITIALIZE                    LNK-DAY1-AREA
      *            MOVE  "12"                    TO  LNK-DAY1-IRAI
      *            MOVE  ZERO                    TO  LNK-DAY1-YMD
      *            MOVE  WRK-WYMD                TO  LNK-DAY1-YMD(2:7)
      *            CALL  "ORCSDAY"               USING   STS-AREA-DAY
      *                                          LNK-DAY1-AREA
      *            IF      STS-DAY-RC1           =   ZERO
      *                MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
      *************END-IF
      *----(01.00.01) LINE ADD START --------------------------------
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-LNKYMD          TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-LNKYMD
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                       MOVE    ZERO                TO  FLG-ERR
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
      *----(01.00.01) LINE ADD END   --------------------------------
           END-EVALUATE
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *   西暦→和暦（機能２１）
      *****************************************************************
       832-YMDCONV21-SEC                          SECTION.
      *
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:8)
               WHEN  ZERO 
               WHEN  "99999999"
                   MOVE    WRK-LNKYMD            TO  WRK-HENYMDG
               WHEN  OTHER
                  INITIALIZE                     STS-AREA-DAY
                  INITIALIZE                     LNK-DAY2-AREA
                  MOVE  "21"                     TO  LNK-DAY2-IRAI
                  MOVE  WRK-LNKYMD(1:8)          TO  LNK-DAY2-YMD
                  CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                                 LNK-DAY2-AREA
                  IF      SGDAY-RC            =   ZERO
                      MOVE    LNK-DAY2-EDTYMD1       TO  WRK-HENYMDG
                  ELSE
                      MOVE    SPACE                  TO  WRK-HENYMDG
                  END-IF
           END-EVALUATE
           .
      *
       832-YMDCONV21-EXT.
           EXIT.
      *
      *****************************************************************
      *   文字数カウント処理（全角のみ）
      *****************************************************************
       891-STRCOUNT-SEC                         SECTION.
      *
           PERFORM  UNTIL  WRK-STRING(CNT-STRING)  NOT  =  SPACE
                    OR  CNT-STRING  =  1
               COMPUTE  CNT-STRING  =  CNT-STRING  -  1
           END-PERFORM
      *
           IF  CNT-STRING                        =  1
               MOVE  ZERO                        TO  CNT-STRING
           ELSE
               COMPUTE  CNT-STRING               =  CNT-STRING  /  2
           END-IF
           .
       891-STRCOUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
