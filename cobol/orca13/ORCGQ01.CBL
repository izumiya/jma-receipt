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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGQ01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 基本情報（Ｑ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/03/19    MCC−太田　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-太田     01/06/04 +性別コンボに男女を追加
      *                                    +診察年月日(F2)クリア対応
      *                                    +最終受診日を範囲指定式に変更
      *                                    +患者番号を範囲指定式に変更
      *                                    +病名追加
      *                                    +診療行為追加
      * 01.00.02     MCC-太田     02/06/26 +ジョブ管理ＤＢ追加
      *                                    +画面ＱＩＤ１を
      *                                    |Ｑ１００に名称変更
      *                                    +新たにＱＩＤ１を追加
      *                                    +病名の初期化不具合を修正
      *                                    +診療行為の初期化不具合を修正
      *                                    +検索結果に年齢を表示させる
      *                                    +最終受診日の未来日入力可に
      *                                    +病名を入力可能に
      *                                    +保険者名を追加
      *                                    +保険の種類を追加
      *                                    +公費の種類をコンボに変更
      *                                    +テスト患者を検索から
      *                                    |外せるようにする
      *                                    +診療行為で、
      *                                    |老人の場合は［老］を付加
      * 01.00.03     NACL-太田    02/10/14 +コンパイルエラーチェック
      *                                    |強化対応
      * 01.00.03     NACL-太田    02/10/17 +woody対応
      * 01.00.03     NACL-太田    02/11/27 +病名コードを空白にしたとき、
      *                                    |対応する病名も空白にする
      * 01.00.04     NACL-太田    03/05/26 +老人一割、２割を追加
      * 01.00.05     NACL-太田    03/05/28 +一括入力対応
      * 01.00.06     NACL-太田    03/06/12 +保険の種類・公費の種類
      *                                    |・病名・診療行為について、
      *                                    |複数入力があった場合は
      *                                    |条件AND,ORの選択を可とする
      * 01.00.07     NACL-太田    03/09/10 +状態を追加
      * 01.00.08     NACL-太田    03/09/24 +診療科を追加
      * 03.05.00     NACL-太田    07.05.23  グループ診療対応
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "Q01SCR-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-CHK                             PIC 9(01).
           03  FLG-PTINF                           PIC 9(01).
           03  FLG-ADRS                            PIC 9(01).
           03  FLG-BYOMEI                          PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-INPUTCD                         PIC 9(01).
           03  FLG-USERBYOMEI                      PIC 9(01).
           03  FLG-HKNNUM                          PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-SHOUKAIKANRI                    PIC 9(01).
           03  FLG-SHOUKAIDATA                     PIC 9(01).
           03  FLG-KANJI                           PIC 9(01).
           03  FLG-CHGSRYYMD                       PIC 9(01).
           03  FLG-PAGENO-G.
               05  FLG-PAGENO-00                   PIC 9(01).
               05  FLG-PAGENO-01                   PIC 9(01).
               05  FLG-PAGENO-02                   PIC 9(01).
               05  FLG-PAGENO-03                   PIC 9(01).
      *
           03  FLG-OK                              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(04).
           03  IDX1                                PIC 9(04).
           03  IDX2                                PIC 9(04).
           03  IDX3                                PIC 9(04).
           03  IDX4                                PIC 9(04).
           03  IDX5                                PIC 9(04).
           03  IDX6                                PIC 9(04).
           03  IDX7                                PIC 9(04).
           03  IDX8                                PIC 9(04).
           03  IDX9                                PIC 9(04).
           03  IDXA                                PIC 9(04).
           03  IDXB                                PIC 9(04).
           03  IDXC                                PIC 9(04).
           03  IDXD                                PIC 9(04).
           03  IDXE                                PIC 9(04).
           03  IDXF                                PIC 9(04).
           03  IDXZ                                PIC 9(04).
           03  IDX-LEN                             PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-KOHNAME                         PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ADRS                            PIC 9(04).
           03  CNT-PNT                             PIC 9(04).
           03  CNT-STR                             PIC 9(04).
           03  CNT-KOHNAME                         PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-SELNO                           PIC 9(03).
           03  WRK-YMD                             PIC X(10). 
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
           03  WRK-KEISANYMD                       PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
           03  WRK-BYOMEICD                        PIC X(20).
           03  WRK-SRYCD                           PIC X(20).
           03  WRK-MOJI                            PIC 9(03).
           03  WRK-CDSYUBETU                       PIC X(01).
           03  WRK-HIZUKE-CALC-AREA.
               05  WRK-HIZUKE-YY                   PIC 9(04).
               05  WRK-HIZUKE-MM                   PIC 9(02).
               05  WRK-HIZUKE-DD                   PIC 9(02).
           03  WRK-EDITADRS-NAME                   PIC X(220).
           03  WRK-RDBYOMEI-A-STATE                PIC S9(9)
                                                   BINARY.
           03  WRK-BYOBUNCD                        PIC X(3).
           03  WRK-PTNUM                           PIC X(20).
           03  WRK-PTNUM-WK                        PIC X(20).
           03  WRK-PTNUM-KETA                      PIC 9(05).
           03  WRK-SYS-KETA                        PIC 9(05).
           03  WRK-SYS-KETA2                       PIC 9(05).
           03  WRK-ST                              PIC 9(05).
           03  WRK-IDX                             PIC 9(05).
           03  WRK-ERR-CUR                         PIC 9(05).
           03  WRK-PAGENO                          PIC 9(01).
           03  WRK-JYUSHO-IDX                      PIC 9(03).
           03  WRK-STIDX                           PIC 9(04).
           03  WRK-INPUTCD                         PIC X(20).
           03  WRK-CD-MCNT                         PIC 9(02).
           03  WRK-KANACHK-MAE-INPUT               PIC X(500).
           03  WRK-KANACHK-OUT-INPUT               PIC X(500).
           03  WRK-SELWORD                         PIC X(500).
           03  WRK-SIZE                            PIC S9(05).
           03  WRK-SIZE-STR                        PIC X(500).
           03  WRK-SIZE-TMP                        PIC X(500).
           03  WRK-NAME                            PIC X(200).
           03  WRK-NAME-G.
               05  WRK-NAME1                       PIC X(100).
               05  WRK-NAME2                       PIC X(100).
           03  WRK-NAME1-G.
               05  WRK-NAME11           PIC X(20).
               05  WRK-NAME12           PIC X(50).
           03  WRK-NAME2-G.
               05  WRK-NAME21           PIC X(30).
               05  WRK-NAME22           PIC X(40).
           03  WRK-STR                  PIC X(20000).
           03  WRK-STR-LEN              PIC 9(05).
           03  WRK-SELPATTERN-G.
               05  WRK-SELPATTERN       PIC 9(02).
               05  WRK-SELPATTERNFL     PIC X(01).
               05  WRK-SELPATTERNMEI    PIC X(30).
           03  WRK-RENNUM               PIC 9(05).
           03  WRK-UID                  PIC X(36).
           03  WRK-CMB-CD                          PIC X(50).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
               05  WRK-CMB-ITM2                    PIC X(200).
           03  WRK-QIDMSG                          PIC X(80).
           03  WRK-UPDKBN               PIC X(03).
           03  WRK-RECECOM-G.
               05  WRK-RECECOM          PIC X(50)
                                        OCCURS     5.
           03  WRK-STSRYYMD             PIC X(08).
           03  WRK-KOHNAME              PIC X(80).
           03  WRK-FTNJANUM             PIC X(08).
           03  WRK-ORCSSTR-NUM1         PIC 9(03).
           03  WRK-TOKKICD.
               05  WRK-TOKKICD9         PIC 9(02).
      *
      *
       01  WRK-WIDGET-AREA.
           03  WRK-WIDGET                          PIC X(20).
      *
           03  WRK-WIDGET-R1   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R1-DAMMY1            PIC X(01).
               05  WRK-WIDGET1                     PIC 9(01).
               05  WRK-WIDGET-R1-DAMMY2            PIC X(18).
      *
           03  WRK-WIDGET-R2   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R2-DAMMY1            PIC X(02).
               05  WRK-WIDGET2                     PIC 9(01).
               05  WRK-WIDGET-R2-DAMMY2            PIC X(17).
      *
           03  WRK-WIDGET-R3   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R3-DAMMY1            PIC X(03).
               05  WRK-WIDGET3                     PIC 9(01).
               05  WRK-WIDGET-R3-DAMMY2            PIC X(16).
      *
           03  WRK-WIDGET-R4   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R4-DAMMY1            PIC X(04).
               05  WRK-WIDGET4                     PIC 9(01).
               05  WRK-WIDGET-R4-DAMMY2            PIC X(15).
      *
           03  WRK-WIDGET-R5   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R5-DAMMY1            PIC X(05).
               05  WRK-WIDGET5                     PIC 9(01).
               05  WRK-WIDGET-R5-DAMMY2            PIC X(14).
      *
           03  WRK-WIDGET-R6   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R6-DAMMY1            PIC X(06).
               05  WRK-WIDGET6                     PIC 9(01).
               05  WRK-WIDGET-R6-DAMMY2            PIC X(13).
      *
           03  WRK-WIDGET-R7   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R7-DAMMY1            PIC X(07).
               05  WRK-WIDGET7                     PIC 9(01).
               05  WRK-WIDGET-R7-DAMMY2            PIC X(12).
      *
           03  WRK-WIDGET-R8   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R8-DAMMY1            PIC X(08).
               05  WRK-WIDGET8                     PIC 9(01).
               05  WRK-WIDGET-R8-DAMMY2            PIC X(11).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-H200401                       PIC X(08)
                                                   VALUE   "20080401".
           03  CONST-NUM-G.
               05  CONST-NUM-VAL               PIC X(10)
                                               VALUE   "0123456789".
               05  CONST-NUM-R         REDEFINES   CONST-NUM-VAL.
                   07  CONST-NUM               PIC X(01)   OCCURS    10.
               05  CONST-ZENNUM-VAL            PIC X(20)
                                       VALUE   "０１２３４５６７８９".
               05  CONST-ZENNUM-R      REDEFINES   CONST-ZENNUM-VAL.
                   07  CONST-ZENNUM            PIC X(02)   OCCURS    10.
           03  CONST-SEX-AREA.
               05  FILLER                          PIC X(10)
                   VALUE   "  すべて　".
               05  FILLER                          PIC X(10)
                   VALUE   "1 男性のみ".
               05  FILLER                          PIC X(10)
                   VALUE   "2 女性のみ".
           03  CONST-SEX-R         REDEFINES       CONST-SEX-AREA.
               05  CONST-SEX                       PIC X(10)
                                                   OCCURS  3.
      *
           03  CONST-KMK-ITEM-AREA.
               05  FILLER                          PIC X(30)
                   VALUE   "1 患者番号".
               05  FILLER                          PIC X(30)
                   VALUE   "2 カナ氏名".
               05  FILLER                          PIC X(30)
                   VALUE   "3 性別".
               05  FILLER                          PIC X(30)
                   VALUE   "4 生年月日".
               05  FILLER                          PIC X(30)
                   VALUE   "5 公費受給者番号".
               05  FILLER                          PIC X(30)
                   VALUE   SPACE.
               05  FILLER                          PIC X(30)
                   VALUE   SPACE.
               05  FILLER                          PIC X(30)
                   VALUE   SPACE.
               05  FILLER                          PIC X(30)
                   VALUE   SPACE.
               05  FILLER                          PIC X(30)
                   VALUE   SPACE.
      *
           03  CONST-KMK-ITEM-R    REDEFINES       CONST-KMK-ITEM-AREA.
               05  CONST-KMK-ITEM                  PIC X(30)
                                                   OCCURS  10.
           03  CONST-BYOMEI-MAX                    PIC 9(05)   VALUE 80.
           03  CONST-SELPATTERN-MAX                PIC 9(05)   VALUE 50.
           03  CONST-UTF8                          PIC 9(01)   VALUE 2.
      *
      *    画面項目ID
           COPY    "CPSHOKAIITEMID.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者状態コメント
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1018.INC".
           COPY    "CPSK1026.INC".
      *
      *    診療科
           COPY    "CPSK1005.INC".
      *
      *    住所マスター
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *    自院病名マスター
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *    自院マスター
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    患者照会検索条件管理情報
       01  SHOUKAIKANRI-REC.
           COPY    "CPSHOUKAIKANRI.INC".
      *
      *    患者照会検索条件データ情報
       01  SHOUKAIDATA-REC.
           COPY    "CPSHOUKAIDATA.INC".
      *
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    文字列操作サブ
           COPY    "CPORCSSTRING.INC".
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
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ(拡張文字対応版)
           COPY    "CPORCSKANACHK2.INC".
      *
      *    部分一致文字列編集
           COPY    "CPORCSADDSIGN.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
       01 UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  ST                          PIC 9(2).
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
           COPY    "Q01.INC".
           COPY    "Q02.INC".
           COPY    "Q03.INC".
           COPY    "Q04.INC".
           COPY    "Q96.INC".
           COPY    "Q97.INC".
           COPY    "Q98.INC".
           COPY    "Q99.INC".
           COPY    "Q100.INC".
           COPY    "QERR.INC".
           COPY    "QID1.INC".
           COPY    "QFIL.INC".
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
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  ( FLG-END           =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF  ( SPA-MOTOPG            =   "QERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "Q01"               TO  MCP-WINDOW
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
      *
      *    他のＬＤより
           IF  ( LINKAREA      NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    予約登録
      * xx WHEN    "Y01"
      *    受付一覧
      * xx WHEN    "U01"
      * xx     MOVE    LNK-COMMON      TO  SPA-KYOTUKEY
      * xx     MOVE    LNK-SCRSPA      TO  SPA-Q01FREE
      * xx     GO      TO      300-SCREEN-EXT
      *    検索結果画面
           WHEN    "Q02"
               PERFORM 310-Q02SPASET-SEC
               GO      TO      300-SCREEN-EXT
           WHEN    "Q96"
               PERFORM 310-Q96SPASET-SEC
               GO      TO      300-SCREEN-EXT
           WHEN    "Q98"
               PERFORM 310-Q98SPASET-SEC
               GO      TO      300-SCREEN-EXT
           WHEN    "Q99"
               PERFORM 310-Q99SPASET-SEC
               GO      TO      300-SCREEN-EXT
           WHEN    "Q04"
               PERFORM 310-Q04SPASET-SEC
               GO      TO      300-SCREEN-EXT
      *    確認メッセージ画面
           WHEN    "QID1"
               PERFORM 310-QID1SPASET-SEC
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｑ０２戻りスパ編集処理
      *****************************************************************
       310-Q02SPASET-SEC               SECTION.
      *
      *
           MOVE    1               TO  SPA-GMN-Q01-CUR
      *
           .
       310-Q02SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｑ９６戻りスパ編集処理
      *****************************************************************
       310-Q96SPASET-SEC               SECTION.
      *
           IF    ( SPA-Q01-Q96-SRYCD   NOT =   SPACE )
               MOVE    SPA-Q01-Q96-MOTOIDX
                                       TO  IDXZ
               MOVE    SPA-Q01-Q96-SRYCD
                                       TO  SPA-GMN-Q01-SRYCD (IDXZ)
               MOVE    IDXZ            TO  IDXA
               PERFORM 4121-SRYCD-CHK-SEC
               IF    ( SPA-Q01-Q96-MOTOIDX =   5 )
                   COMPUTE SPA-GMN-Q01-CUR =   1
               ELSE
                   COMPUTE SPA-GMN-Q01-CUR
                                       =   314 +   SPA-Q01-Q96-MOTOIDX
               END-IF
           ELSE
               COMPUTE SPA-GMN-Q01-CUR =   313 +   SPA-Q01-Q96-MOTOIDX
           END-IF 
      *
           .
       310-Q96SPASET-EXT.
           EXIT.
      *****************************************************************
      *    Ｑ９８戻りスパ編集処理
      *****************************************************************
       310-Q98SPASET-SEC               SECTION.
      *
           IF  ( SPA-Q01-Q98-JYUSHO    NOT =  SPACE )
               MOVE    SPA-Q01-Q98-JYUSHO
                                   TO  SPA-GMN-Q01-JYUSHO
                                           (SPA-Q01-Q98-JYUSHO-IDX)
               MOVE    SPA-Q01-Q98-POST
                                   TO  SPA-GMN-Q01-POST
                                           (SPA-Q01-Q98-JYUSHO-IDX)
               COMPUTE SPA-GMN-Q01-CUR
                                   =   112
                                   + ( SPA-Q01-Q98-JYUSHO-IDX  - 1 ) * 2
                                   +   1
           END-IF
      *
           .
       310-Q98SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｑ９９戻りスパ編集処理
      *****************************************************************
       310-Q99SPASET-SEC               SECTION.
      *
           IF  ( SPA-Q01-Q99-JYUSHO    NOT =  SPACE )
               MOVE    SPA-Q01-Q99-JYUSHO
                                   TO  SPA-GMN-Q01-JYUSHO
                                               (SPA-Q01-Q99-JYUSHO-IDX)
               MOVE    SPA-Q01-Q99-POST
                                   TO  SPA-GMN-Q01-POST
                                               (SPA-Q01-Q99-JYUSHO-IDX)
               COMPUTE SPA-GMN-Q01-CUR
                                   =   112
                                   + ( SPA-Q01-Q99-JYUSHO-IDX  - 1 ) * 2
                                   +   1
           END-IF
      *
           .
       310-Q99SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｑ０４戻りスパ編集処理
      *****************************************************************
       310-Q04SPASET-SEC               SECTION.
      *
           IF  ( SPA-Q01-Q04-NAME  NOT =  SPACE )
               MOVE    SPA-Q01-Q04-NAME   TO   WRK-SELPATTERNMEI
               IF    ( SPA-Q01-Q04-UPDKBN =    "UPD" )
                   PERFORM 4601-SELPATTERN-UPD-SEC
               ELSE
                   PERFORM 700-SELPATTERN-INS-SEC
               END-IF
           
           END-IF
      *
           .
       310-Q04SPASET-EXT.
           EXIT.
      *****************************************************************
      *    ＱＩＤ１戻りスパ編集処理
      *****************************************************************
       310-QID1SPASET-SEC              SECTION.
      *
           IF    ( SPA-QID1-FLG    =   "OK" )
      *
               EVALUATE    SPA-QIDCD
               WHEN    "3002"
                   PERFORM 4501-SELPATTERN-DEL-SEC
               END-EVALUATE
           END-IF
      *
           INITIALIZE  SPA-Q00-QID-AREA
           .
      *
       310-QID1SPASET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-Q01
           INITIALIZE                      SPA-Q01
      *
           MOVE    SPACE               TO  SPA-Q01KYOUTU
           INITIALIZE                      SPA-Q01KYOUTU
      *
      *    タブページ番号
           MOVE    ZERO                TO  SPA-GMN-Q01-PAGENO
      *
      *    基準日開始日和暦
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-Q01-KJYNYMD
      *    基準日開始日西暦
           MOVE    SPA-SYSYMD          TO  SPA-NAI-Q01-KJYNYMD
      *
      *    テスト患者チェックボックス
           MOVE   "F"                  TO  SPA-GMN-Q01-CHKTSTPT
      *
      *    性別コンボ初期処理
           PERFORM 3101-SEX-CMB-SYOKI-SEC
           MOVE    SPA-GMN-Q01-SEXLST (1)
                                       TO  SPA-GMN-Q01-SEX-G
      *
      *    検索条件コンボ初期処理
           PERFORM 3101-SELPATTERN-CMB-SYOKI-SEC
      *
      *    入外区分コンボ初期処理
           PERFORM 3101-NYUGAIKBN-CMB-SYOKI-SEC
      *
      *    診療科コンボ初期処理
           PERFORM 3101-SRYKA-CMB-SYOKI-SEC
      *
      *    死亡コンボ初期処理
           PERFORM 3101-DEATH-CMB-SYOKI-SEC
      *
      *    状態１コンボ初期処理
           PERFORM 3101-CONDITION1-CMB-SYOKI-SEC
      *
      *    状態２コンボ初期処理
           PERFORM 3101-CONDITION2-CMB-SYOKI-SEC
      *
      *    状態３コンボ初期処理
           PERFORM 3101-CONDITION3-CMB-SYOKI-SEC
      *
      *    減免コンボ初期処理
           PERFORM 3101-GENMEN-CMB-SYOKI-SEC
      *
      *    コメント選択コンボ初期処理
           PERFORM 3101-SELCOMMENT-CMB-SYOKI-SEC
           MOVE    SPA-GMN-Q01-SELCOMMENTLST (1)
                                   TO  SPA-GMN-Q01-SELCOMMENT-G
      *
      *    低所得コンボ初期処理
           PERFORM 3101-TSY-CMB-SYOKI-SEC
      *
      *    低所得期間開始識別コンボ初期処理
           PERFORM 3101-STTSYSKBT-CMB-SYOKI-SEC
           MOVE    SPA-GMN-Q01-STTSYSKBTLST (1)
                                       TO  SPA-GMN-Q01-STTSYSKBT-G
      *
      *    電話先コンボ初期処理
           PERFORM 3101-TEL-TO-CMB-SYOKI-SEC
           MOVE    SPA-GMN-Q01-TEL-TOLST (1)
                                       TO  SPA-GMN-Q01-TEL-TO-G
      *
      *    特記事項コンボ初期処理
           PERFORM 3101-TOKKIINFO-CMB-SYOKI-SEC
      *
      *    災害該当コンボ初期処理
           PERFORM 3101-SAIGAIKBN-CMB-SYOKI-SEC
      *
      *    本人家族コンボ初期処理
           PERFORM 3101-HONKZKKBN-CMB-SYOKI-SEC
      *
      *    保険の種類・公費の種類コンボ初期処理
           PERFORM 3101-HKNKOH-CMB-SYOKI-SEC
      *
      *    疑い病名区分コンボ初期処理
           PERFORM 3101-UTAGAIKBN-CMB-SYOKI-SEC
      *
      *    急性病名区分コンボ初期処理
           PERFORM 3101-KYUSEIKBN-CMB-SYOKI-SEC
      *
      *    転帰区分コンボ初期処理
           PERFORM 3101-TENKIKBN-CMB-SYOKI-SEC
      *
      *    主病名区分コンボ初期処理
           PERFORM 3101-SYUBYOKBN-CMB-SYOKI-SEC
      *
      *    病名検索コンボ初期処理
           PERFORM 3101-BYOMEI-CMB-SYOKI-SEC
      *
      *    診療区分コンボ初期処理
           PERFORM 3101-SRYKBN-CMB-SYOKI-SEC
      *
      *    ソートキー項目初期処理
           PERFORM 3101-SORT-KMK-SYOKI-SEC
           MOVE    SPA-GMN-Q01-SORTLST (1)
                                       TO  SPA-GMN-Q01-KMK-G (1)
           MOVE    SPA-GMN-Q01-SORTLST (2)
                                       TO  SPA-GMN-Q01-KMK-G (2)
           MOVE    SPA-GMN-Q01-SORTLST (3)
                                       TO  SPA-GMN-Q01-KMK-G (3)
      *
      *    並び順ラジオボタン初期処理
           PERFORM 3101-RDIO-SYOKI-SEC
      *
      *    検索条件AND/OR切り替えラジオボタン初期化
           MOVE    "T"                 TO  SPA-GMN-Q01-RDHKNKOH-AND
           MOVE    "F"                 TO  SPA-GMN-Q01-RDHKNKOH-OR
           MOVE    "F"                 TO  SPA-GMN-Q01-RDKOHNUM-AND
           MOVE    "T"                 TO  SPA-GMN-Q01-RDKOHNUM-OR
           MOVE    "F"                 TO  SPA-GMN-Q01-RDBYOMEI-AND
           MOVE    "T"                 TO  SPA-GMN-Q01-RDBYOMEI-OR
           MOVE    "F"                 TO  SPA-GMN-Q01-RDSRYACT-AND
           MOVE    "T"                 TO  SPA-GMN-Q01-RDSRYACT-OR
      *
      *    カーソルキー制御項目
           MOVE   3                    TO  SPA-GMN-Q01-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索条件コンボ初期処理
      *****************************************************************
       3101-SELPATTERN-CMB-SYOKI-SEC   SECTION.
      *
           INITIALIZE                  SPA-GMN-Q01-SELPATTERNLST-G
                                       SPA-NAI-Q01-SELPATTERNLST-G
                                       WRK-SELPATTERN-G
      *
           PERFORM 900-SHOUKAIKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SHOUKAIKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-Q01-SELPATTERNLST-MAX
                                       >=  CONST-SELPATTERN-MAX )
      *
               COMPUTE WRK-SELPATTERN  =   WRK-SELPATTERN  +   1
      *
               MOVE    WRK-SELPATTERN  TO  SPA-GMN-Q01-SELPATTERNLST-MAX
      *
               MOVE    SHOKANRI-NAME   TO  WRK-SELPATTERNMEI
               MOVE    WRK-SELPATTERN-G
                                   TO SPA-GMN-Q01-SELPATTERNLST
                                                        (WRK-SELPATTERN)
      *
               MOVE    SHOKANRI-UID
                                   TO  SPA-NAI-Q01-SELPATTERN-UIDL
                                                        (WRK-SELPATTERN)
      *
               PERFORM 900-SHOUKAIKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       3101-SELPATTERN-CMB-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    性別コンボ初期処理
      *****************************************************************
       3101-SEX-CMB-SYOKI-SEC          SECTION.
      *
           MOVE    3                   TO  SPA-GMN-Q01-SEXLST-MAX
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL ( IDX5    >   SPA-GMN-Q01-SEXLST-MAX )
      *
               MOVE    CONST-SEX (IDX5)
                                       TO  SPA-GMN-Q01-SEXLST (IDX5)
      *
           END-PERFORM
           .
      *
       3101-SEX-CMB-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分コンボ初期処理
      *****************************************************************
       3101-NYUGAIKBN-CMB-SYOKI-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-Q01-NYUGAIKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-Q01-NYUGAIKBNLST-MAX
      *
           MOVE    "1"             TO  SPA-GMN-Q01-NYUGAIKBNL    (1)
           MOVE    " 入院"         TO  SPA-GMN-Q01-NYUGAIKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-Q01-NYUGAIKBNL    (2)
           MOVE    " 外来"         TO  SPA-GMN-Q01-NYUGAIKBNMEIL (2)
      *
           .
      *
       3101-NYUGAIKBN-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    診療科コンボ
      *****************************************************************
       3101-SRYKA-CMB-SYOKI-SEC        SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-SRYKALST-G
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-Q01-SRYKALST-MAX    >   50 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
               COMPUTE SPA-GMN-Q01-SRYKALST-MAX
                   =   SPA-GMN-Q01-SRYKALST-MAX    +   1
      *
               MOVE    SPA-GMN-Q01-SRYKALST-MAX
                                       TO  IDX5
      *
               MOVE    SYS-1005-KBNCD (1:2)
                                   TO  SPA-GMN-Q01-SRYKAL (IDX5)
               MOVE    SYS-1005-SRYKANAME
                                   TO  SPA-GMN-Q01-SRYKAMEIL (IDX5)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SRYKA-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    死亡コンボ
      *****************************************************************
       3101-DEATH-CMB-SYOKI-SEC        SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-DEATHLST-G
      *
           MOVE    2                   TO  SPA-GMN-Q01-DEATHLST-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-Q01-DEATHL    (1)
           MOVE    "未設定"            TO  SPA-GMN-Q01-DEATHMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-Q01-DEATHL    (2)
           MOVE    "死亡"              TO  SPA-GMN-Q01-DEATHMEIL (2)
      *
           .
       3101-DEATH-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    状態１コンボ初期処理
      *****************************************************************
       3101-CONDITION1-CMB-SYOKI-SEC   SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-CONDITION1LST-G
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1018-REC
           MOVE    "1018"              TO  SYS-1018-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1018-STYUKYMD
                                           SYS-1018-EDYUKYMD
           MOVE    SYS-1018-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-Q01-CONDITION1LST-MAX   >=   99 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1018-REC
      *
               COMPUTE SPA-GMN-Q01-CONDITION1LST-MAX
                   =   SPA-GMN-Q01-CONDITION1LST-MAX   +   1
      *
               MOVE    SPA-GMN-Q01-CONDITION1LST-MAX
                                       TO  IDX5
      *
               MOVE    SYS-1018-KBNCD (1:2)
                                   TO  SPA-GMN-Q01-CONDITION1L (IDX5)
               MOVE    SYS-1018-CONDITION-BODY
                                   TO  SPA-GMN-Q01-CONDITION1MEIL (IDX5)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-CONDITION1-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    状態２コンボ初期処理
      *****************************************************************
       3101-CONDITION2-CMB-SYOKI-SEC   SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-CONDITION2LST-G
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1018-REC
           MOVE    "1019"              TO  SYS-1018-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1018-STYUKYMD
                                           SYS-1018-EDYUKYMD
           MOVE    SYS-1018-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-Q01-CONDITION2LST-MAX   >=   99 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1018-REC
      *
               COMPUTE SPA-GMN-Q01-CONDITION2LST-MAX
                   =   SPA-GMN-Q01-CONDITION2LST-MAX   +   1
      *
               MOVE    SPA-GMN-Q01-CONDITION2LST-MAX
                                       TO  IDX5
      *
               MOVE    SYS-1018-KBNCD (1:2)
                                   TO  SPA-GMN-Q01-CONDITION2L (IDX5)
               MOVE    SYS-1018-CONDITION-BODY
                                   TO  SPA-GMN-Q01-CONDITION2MEIL (IDX5)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-CONDITION2-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    状態３コンボ初期処理
      *****************************************************************
       3101-CONDITION3-CMB-SYOKI-SEC   SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-CONDITION3LST-G
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1018-REC
           MOVE    "1020"              TO  SYS-1018-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1018-STYUKYMD
                                           SYS-1018-EDYUKYMD
           MOVE    SYS-1018-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-Q01-CONDITION3LST-MAX   >=   99 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1018-REC
      *
               COMPUTE SPA-GMN-Q01-CONDITION3LST-MAX
                   =   SPA-GMN-Q01-CONDITION3LST-MAX   +   1
      *
               MOVE    SPA-GMN-Q01-CONDITION3LST-MAX
                                       TO  IDX5
      *
               MOVE    SYS-1018-KBNCD (1:2)
                                   TO  SPA-GMN-Q01-CONDITION3L (IDX5)
               MOVE    SYS-1018-CONDITION-BODY
                                   TO  SPA-GMN-Q01-CONDITION3MEIL (IDX5)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-CONDITION3-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    減免事由コンボ初期処理
      *****************************************************************
       3101-GENMEN-CMB-SYOKI-SEC   SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-GENMENLST-G
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1026-REC
           MOVE    "1026"              TO  SYS-1026-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1026-STYUKYMD
                                           SYS-1026-EDYUKYMD
           MOVE    SYS-1026-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-Q01-GENMENLST-MAX   >=   99 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1026-REC
      *
               COMPUTE SPA-GMN-Q01-GENMENLST-MAX
                   =   SPA-GMN-Q01-GENMENLST-MAX   +   1
      *
               MOVE    SPA-GMN-Q01-GENMENLST-MAX
                                       TO  IDX5
      *
               MOVE    SYS-1026-KBNCD (1:2)
                                   TO  SPA-GMN-Q01-GENMENL (IDX5)
               MOVE    SYS-1026-DISCOUNT-BODY
                                   TO  SPA-GMN-Q01-GENMENMEIL (IDX5)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-GENMEN-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    コメント選択コンボ初期処理
      *****************************************************************
       3101-SELCOMMENT-CMB-SYOKI-SEC   SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-SELCOMMENTLST-G
      *
           MOVE    "1"                 TO  SPA-GMN-Q01-SELCOMMENTL   (1)
           MOVE    "患者コメント"      TO  SPA-GMN-Q01-SELCOMMENTMEIL(1)
      *
           MOVE    "2"                 TO  SPA-GMN-Q01-SELCOMMENTL   (2)
           MOVE    "患者禁忌・アレルギー・感染症"
                                       TO  SPA-GMN-Q01-SELCOMMENTMEIL(2)
      *
           COMPUTE SPA-GMN-Q01-SELCOMMENTLST-MAX
               =   2
      *
           .
       3101-SELCOMMENT-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    低所得コンボ初期処理
      *****************************************************************
       3101-TSY-CMB-SYOKI-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-TSYLST-G
      *
           MOVE    2                   TO  SPA-GMN-Q01-TSYLST-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-Q01-TSYL    (1)
           MOVE    "低所得者１"        TO  SPA-GMN-Q01-TSYMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-Q01-TSYL    (2)
           MOVE    "低所得者２"        TO  SPA-GMN-Q01-TSYMEIL (2)
      *    MOVE    "3"                 TO  SPA-GMN-Q01-TSYL    (3)
      *    MOVE    "低所得者１・２"    TO  SPA-GMN-Q01-TSYMEIL (3)
      *
           .
       3101-TSY-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    低所得期間開始識別コンボ初期処理
      *****************************************************************
       3101-STTSYSKBT-CMB-SYOKI-SEC        SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-STTSYSKBTLST-G
      *
           MOVE    4                   TO  SPA-GMN-Q01-STTSYSKBTLST-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-Q01-STTSYSKBTL    (1)
           MOVE    "有効月"            TO  SPA-GMN-Q01-STTSYSKBTMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-Q01-STTSYSKBTL    (2)
           MOVE    "開始月"            TO  SPA-GMN-Q01-STTSYSKBTMEIL (2)
           MOVE    "3"                 TO  SPA-GMN-Q01-STTSYSKBTL    (3)
           MOVE    "終了月"            TO  SPA-GMN-Q01-STTSYSKBTMEIL (3)
           MOVE    "4"                 TO  SPA-GMN-Q01-STTSYSKBTL    (4)
           MOVE    "有効日"            TO  SPA-GMN-Q01-STTSYSKBTMEIL (4)
      *
           .
       3101-STTSYSKBT-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    電話先コンボ初期処理
      *****************************************************************
       3101-TEL-TO-CMB-SYOKI-SEC          SECTION.
      *
           MOVE    3                   TO  SPA-GMN-Q01-TEL-TOLST-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-Q01-TEL-TOL    (1)
           MOVE    "自宅"              TO  SPA-GMN-Q01-TEL-TOMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-Q01-TEL-TOL    (2)
           MOVE    "連絡先"            TO  SPA-GMN-Q01-TEL-TOMEIL (2)
           MOVE    "3"                 TO  SPA-GMN-Q01-TEL-TOL    (3)
           MOVE    "自宅・連絡先"      TO  SPA-GMN-Q01-TEL-TOMEIL (3)
      *
           .
       3101-TEL-TO-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    特記事項コンボ初期処理
      *****************************************************************
       3101-TOKKIINFO-CMB-SYOKI-SEC        SECTION.
      *
           MOVE    23              TO  SPA-GMN-Q01-TOKKIINFOLST-MAX
      *
           MOVE    "01"            TO  SPA-GMN-Q01-TOKKIINFOL    (01)
           MOVE    "公"            TO  SPA-GMN-Q01-TOKKIINFOMEIL (01)
      *
           MOVE    "02"            TO  SPA-GMN-Q01-TOKKIINFOL    (02)
           MOVE    "長"            TO  SPA-GMN-Q01-TOKKIINFOMEIL (02)
      *
           MOVE    "03"            TO  SPA-GMN-Q01-TOKKIINFOL    (03)
           MOVE    "長処"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (03)
      *
           MOVE    "04"            TO  SPA-GMN-Q01-TOKKIINFOL    (04)
           MOVE    "後保"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (04)
      *
           MOVE    "07"            TO  SPA-GMN-Q01-TOKKIINFOL    (05)
           MOVE    "老併"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (05)
      *
           MOVE    "08"            TO  SPA-GMN-Q01-TOKKIINFOL    (06)
           MOVE    "老健"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (06)
      *
           MOVE    "09"            TO  SPA-GMN-Q01-TOKKIINFOL    (07)
           MOVE    "施"            TO  SPA-GMN-Q01-TOKKIINFOMEIL (07)
      *
           MOVE    "10"            TO  SPA-GMN-Q01-TOKKIINFOL    (08)
           MOVE    "第三"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (08)
      *
           MOVE    "11"            TO  SPA-GMN-Q01-TOKKIINFOL    (09)
           MOVE    "薬治"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (09)
      *
           MOVE    "12"            TO  SPA-GMN-Q01-TOKKIINFOL    (10)
           MOVE    "器治"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (10)
      *
           MOVE    "13"            TO  SPA-GMN-Q01-TOKKIINFOL    (11)
           MOVE    "先進"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (11)
      *
           MOVE    "14"            TO  SPA-GMN-Q01-TOKKIINFOL    (12)
           MOVE    "制超"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (12)
      *
           MOVE    "15"            TO  SPA-GMN-Q01-TOKKIINFOL    (13)
           MOVE    "経過"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (13)
      *
           MOVE    "16"            TO  SPA-GMN-Q01-TOKKIINFOL    (14)
           MOVE    "長２"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (14)
      *
           MOVE    "17"            TO  SPA-GMN-Q01-TOKKIINFOL    (15)
           MOVE    "上位"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (15)
      *
           MOVE    "18"            TO  SPA-GMN-Q01-TOKKIINFOL    (16)
           MOVE    "一般"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (16)
      *
           MOVE    "19"            TO  SPA-GMN-Q01-TOKKIINFOL    (17)
           MOVE    "低所"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (17)
      *
           MOVE    "20"            TO  SPA-GMN-Q01-TOKKIINFOL    (18)
           MOVE    "二割"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (18)
      *
           MOVE    "21"            TO  SPA-GMN-Q01-TOKKIINFOL    (19)
           MOVE    "高半"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (19)
      *
           MOVE    "22"            TO  SPA-GMN-Q01-TOKKIINFOL    (20)
           MOVE    "多上"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (20)
      *
           MOVE    "23"            TO  SPA-GMN-Q01-TOKKIINFOL    (21)
           MOVE    "多一"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (21)
      *
           MOVE    "24"            TO  SPA-GMN-Q01-TOKKIINFOL    (22)
           MOVE    "多低"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (22)
      *
           MOVE    "25"            TO  SPA-GMN-Q01-TOKKIINFOL    (23)
           MOVE    "出産"          TO  SPA-GMN-Q01-TOKKIINFOMEIL (23)
      *
           .
       3101-TOKKIINFO-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    災害該当コンボ初期処理
      *****************************************************************
       3101-SAIGAIKBN-CMB-SYOKI-SEC        SECTION.
      *
           MOVE    2               TO  SPA-GMN-Q01-SAIGAIKBNLST-MAX
      *
           MOVE    "1"             TO  SPA-GMN-Q01-SAIGAIKBNL    (01)
           MOVE    "未確定分"      TO  SPA-GMN-Q01-SAIGAIKBNMEIL (01)
      *
           MOVE    "2"             TO  SPA-GMN-Q01-SAIGAIKBNL    (02)
           MOVE    "確定分"        TO  SPA-GMN-Q01-SAIGAIKBNMEIL (02)
      *
           .
       3101-SAIGAIKBN-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    本人家族コンボ初期処理
      *****************************************************************
       3101-HONKZKKBN-CMB-SYOKI-SEC       SECTION.
      *
           MOVE    2                   TO  SPA-GMN-Q01-HONKZKKBNLST-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-Q01-HONKZKKBNL    (1)
           MOVE    "本人"              TO  SPA-GMN-Q01-HONKZKKBNMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-Q01-HONKZKKBNL    (2)
           MOVE    "家族"              TO  SPA-GMN-Q01-HONKZKKBNMEIL (2)
      *
           .
       3101-HONKZKKBN-CMB-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険の種類・公費の種類コンボ初期処理
      *****************************************************************
       3101-HKNKOH-CMB-SYOKI-SEC       SECTION.
      *
           PERFORM 3101-HKNKOH-CMB-EDIT-SEC
      *
           .
       3101-HKNKOH-CMB-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険の種類・公費の種類コンボ初期処理
      *****************************************************************
       3101-HKNKOH-CMB-EDIT-SEC        SECTION.
      *
           MOVE    SPACE       TO  SPA-GMN-Q01-HKNNUMLST-G
           INITIALIZE              SPA-GMN-Q01-HKNNUMLST-G
      *
      *    保険番号検索
           PERFORM 900-HKNNUM-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNNUM      =   1 )
      *
               IF    ( HKN-HKNKOHSBTKBN    =   "1" OR  "4"
                                           OR  "8" OR  "9" )
                   MOVE    SPA-GMN-Q01-HKNNUMLST-MAX   TO  IDX5
      *            保険
                   IF    ( IDX5            >   ZERO )
                       IF    ( HKN-HKNNUM  
                                   NOT =   SPA-GMN-Q01-HKNNUML (IDX5))
      *                    保険情報編集
                           PERFORM 31011-HKNHEN-SEC
                       END-IF
                   ELSE
      *                保険情報編集
                       PERFORM 31011-HKNHEN-SEC
                   END-IF
      *
               ELSE
      *            公費
                   MOVE    SPA-GMN-Q01-KOHNUMLST-MAX   TO  IDX5
                   IF    ( IDX5            >   ZERO )
                       IF    ( HKN-HKNNUM 
                                       =   SPA-GMN-Q01-KOHNUML (IDX5))
                           IF    ( HKN-HKNNUM   =   "027"  ) 
                             AND ( HKN-PAYKBN   =  "01" OR "02" OR "03")
                               IF ( HKN-PAYKBN
                                    NOT =   SPA-GMN-Q01-PAYKBNL (IDX5))
      *                            公費情報編集
                                   PERFORM 31011-KOHHEN-SEC
                               END-IF
                           END-IF
                       ELSE
      *                    公費情報編集
                           PERFORM 31011-KOHHEN-SEC
                       END-IF
                   ELSE
      *                公費情報編集
                       PERFORM 31011-KOHHEN-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-HKNNUM-KEY3-FET-SEC
      *
           END-PERFORM
      *
      *    保険番号クローズ
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    SPA-GMN-Q01-HKNNUMLST-MAX   TO  IDX5
      *
           MOVE    "SYA"           TO  SPA-GMN-Q01-HKNNUML   (IDX5 + 1)
           MOVE    "社保"          TO  SPA-GMN-Q01-HKNNUMMEIL(IDX5 + 1)
           MOVE    "KOK"           TO  SPA-GMN-Q01-HKNNUML   (IDX5 + 2)
           MOVE    "国保"          TO  SPA-GMN-Q01-HKNNUMMEIL(IDX5 + 2)
           IF    ( SPA-SYSYMD          >=  CONST-H200401 )
               MOVE    "KOU"       TO  SPA-GMN-Q01-HKNNUML   (IDX5 + 3)
               MOVE    "広域"      TO SPA-GMN-Q01-HKNNUMMEIL(IDX5 + 3)
               COMPUTE SPA-GMN-Q01-HKNNUMLST-MAX
                   =   IDX5 + 3
           ELSE
               COMPUTE SPA-GMN-Q01-HKNNUMLST-MAX
                   =   IDX5 + 2
           END-IF
      *
           .
       3101-HKNKOH-CMB-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       31011-HKNHEN-SEC          SECTION.
      *
           COMPUTE SPA-GMN-Q01-HKNNUMLST-MAX
               =   SPA-GMN-Q01-HKNNUMLST-MAX   +   1
      *
           MOVE    SPA-GMN-Q01-HKNNUMLST-MAX   TO  IDX5
      *
           MOVE    HKN-HKNNUM          TO  SPA-GMN-Q01-HKNNUML    (IDX5)
           MOVE    HKN-TANSEIDONAME    TO  SPA-GMN-Q01-HKNNUMMEIL (IDX5)
      *
           .
       31011-HKNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       31011-KOHHEN-SEC          SECTION.
      *
           COMPUTE SPA-GMN-Q01-KOHNUMLST-MAX
               =   SPA-GMN-Q01-KOHNUMLST-MAX   +   1
      *
           MOVE    SPA-GMN-Q01-KOHNUMLST-MAX   TO  IDX5
      *
           MOVE    HKN-HKNNUM          TO  SPA-GMN-Q01-KOHNUML (IDX5)
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN      NOT =   "01" AND "02" AND "03" )
               MOVE    "老人保健"      TO  SPA-GMN-Q01-KOHNUMMEIL (IDX5)
           ELSE
               MOVE    HKN-TANSEIDONAME
                                       TO  SPA-GMN-Q01-KOHNUMMEIL (IDX5)
           END-IF
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN          =   "01" OR  "02" OR "03")
               MOVE    HKN-PAYKBN (2:1)
                                       TO  SPA-GMN-Q01-PAYKBNL (IDX5)
           ELSE
               MOVE    SPACE           TO  SPA-GMN-Q01-PAYKBNL (IDX5)
           END-IF
      *
           .
       31011-KOHHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑い病名コンボ初期処理
      *****************************************************************
       3101-UTAGAIKBN-CMB-SYOKI-SEC    SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-UTAGAIKBNLST-G
      *
           MOVE    2                   TO  SPA-GMN-Q01-UTAGAIKBNLST-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-Q01-UTAGAIKBNL (1)
           MOVE    "含まない"          TO  SPA-GMN-Q01-UTAGAIKBNMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-Q01-UTAGAIKBNL (2)
           MOVE    "疑い病名のみ"      TO  SPA-GMN-Q01-UTAGAIKBNMEIL (2)
      *
           .
       3101-UTAGAIKBN-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    急性病名コンボ初期処理
      *****************************************************************
       3101-KYUSEIKBN-CMB-SYOKI-SEC    SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-KYUSEIKBNLST-G
      *
           MOVE    2                   TO  SPA-GMN-Q01-KYUSEIKBNLST-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-Q01-KYUSEIKBNL (1)
           MOVE    "含まない"          TO  SPA-GMN-Q01-KYUSEIKBNMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-Q01-KYUSEIKBNL (2)
           MOVE    "急性病名のみ"      TO  SPA-GMN-Q01-KYUSEIKBNMEIL (2)
      *
           .
       3101-KYUSEIKBN-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    転帰コンボ初期処理
      *****************************************************************
       3101-TENKIKBN-CMB-SYOKI-SEC     SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-TENKIKBNLST-G
      *
           MOVE    6                   TO  SPA-GMN-Q01-TENKIKBNLST-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-Q01-TENKIKBNL (1)
           MOVE    "未入力分のみ"      TO  SPA-GMN-Q01-TENKIKBNMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-Q01-TENKIKBNL (2)
           MOVE    "治ゆ"              TO  SPA-GMN-Q01-TENKIKBNMEIL (2)
      *
           MOVE    "2"                 TO  SPA-GMN-Q01-TENKIKBNL (3)
           MOVE    "死亡"              TO  SPA-GMN-Q01-TENKIKBNMEIL (3)
      *
           MOVE    "3"                 TO  SPA-GMN-Q01-TENKIKBNL (4)
           MOVE    "中止"              TO  SPA-GMN-Q01-TENKIKBNMEIL (4)
      *
           MOVE    "8"                 TO  SPA-GMN-Q01-TENKIKBNL (5)
           MOVE    "移行"              TO  SPA-GMN-Q01-TENKIKBNMEIL (5)
      *
           MOVE    "9"                 TO  SPA-GMN-Q01-TENKIKBNL (6)
           MOVE    "転帰すべて"        TO  SPA-GMN-Q01-TENKIKBNMEIL (6)
      *
           .
       3101-TENKIKBN-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    主病名区分コンボ初期処理
      *****************************************************************
       3101-SYUBYOKBN-CMB-SYOKI-SEC    SECTION.
      *
           INITIALIZE                      SPA-GMN-Q01-SYUBYOKBNLST-G
      *
           MOVE    2                   TO  SPA-GMN-Q01-SYUBYOKBNLST-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-Q01-SYUBYOKBNL (1)
           MOVE    "主病名でない"      TO  SPA-GMN-Q01-SYUBYOKBNMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-Q01-SYUBYOKBNL (2)
           MOVE    "主病名である"      TO  SPA-GMN-Q01-SYUBYOKBNMEIL (2)
      *
           .
       3101-SYUBYOKBN-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    病名検索コンボ初期処理
      *****************************************************************
       3101-BYOMEI-CMB-SYOKI-SEC       SECTION.
      *
           MOVE    "T"             TO  SPA-GMN-Q01-RDBYOMEI-U
           MOVE    "F"             TO  SPA-GMN-Q01-RDBYOMEI-A
           MOVE    WIDGET-NORMAL
                                   TO  SPA-GMN-Q01-UBYOMEI-STATE
                                       SPA-GMN-Q01-RDBYOMEI-U-STATE
                                       SPA-GMN-Q01-RDBYOMEI-A-STATE
      *
           MOVE    ZERO            TO  SPA-GMN-Q01-UBYOMEILST-MAX
      *
      *    自院病名マスタ−検索処理
           PERFORM 900-USERBYOMEI-KEY4-SEL-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   98)
                OR       ( FLG-USERBYOMEI  NOT =   ZERO )
      *
               MOVE    IDX1    TO  SPA-GMN-Q01-UBYOMEILST-MAX
               MOVE    USBYO-BYOMEIINPUTCD
                               TO  SPA-GMN-Q01-UBYOMEIL (IDX1)
               MOVE    USBYO-BYOMEI
                               TO  SPA-GMN-Q01-UBYOMEIMEIL (IDX1)
      *
               PERFORM 900-USERBYOMEI-KEY4-FET-SEC
      *
           END-PERFORM
      *
      *    自院病名マスタ−クローズ処理
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    分類コード設定データが存在したかチェック
           IF  ( SPA-GMN-Q01-UBYOMEILST-MAX  =   ZERO )
      *
      *        xxx 存在しなかった場合 xxx
      *        自院病名マスタ−読込処理２（データ存在チェック）
               INITIALIZE              USERBYOMEI-REC
               MOVE    SPA-HOSPNUM  TO  USBYO-HOSPNUM
               PERFORM 900-USERBYOMEI-KEY3-SEL-SEC
      *
      *        分類コードが設定されていないデータが存在したかチェック
               IF  ( FLG-USERBYOMEI    =   ZERO )
                   MOVE    "NON"   TO  SPA-GMN-Q01-UBYOMEIL (1)
                   MOVE    "分類なし"
                                   TO  SPA-GMN-Q01-UBYOMEIMEIL  (1)
                   MOVE    "ALL"   TO  SPA-GMN-Q01-UBYOMEIL (2)
                   MOVE    "すべて"
                                   TO  SPA-GMN-Q01-UBYOMEIMEIL  (2)
                   MOVE    2       TO  SPA-GMN-Q01-UBYOMEILST-MAX
               ELSE
                   MOVE    "F"     TO  SPA-GMN-Q01-RDBYOMEI-U
                   MOVE    "T"     TO  SPA-GMN-Q01-RDBYOMEI-A
                   MOVE    WIDGET-INSENSITIVE
                                   TO  SPA-GMN-Q01-UBYOMEI-STATE
                                       SPA-GMN-Q01-RDBYOMEI-U-STATE
                                       SPA-GMN-Q01-RDBYOMEI-A-STATE
               END-IF
           ELSE
      *
      *        xxx 存在した場合 xxx
               MOVE    SPA-GMN-Q01-UBYOMEILST-MAX
                                   TO  IDX1
      *
               COMPUTE IDX1        =   IDX1    +   1
               MOVE    "NON"       TO  SPA-GMN-Q01-UBYOMEIL  (IDX1)
               MOVE    "分類なし"  TO  SPA-GMN-Q01-UBYOMEIMEIL
                                                             (IDX1)
      *
               COMPUTE IDX1        =   IDX1    +   1
               MOVE    "ALL"       TO  SPA-GMN-Q01-UBYOMEIL  (IDX1)
               MOVE    "すべて"    TO  SPA-GMN-Q01-UBYOMEIMEIL
                                                             (IDX1)
      *
               MOVE    IDX1        TO  SPA-GMN-Q01-UBYOMEILST-MAX
           END-IF
      *
           .
       3101-BYOMEI-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    診療区分コンボ初期処理
      *****************************************************************
       3101-SRYKBN-CMB-SYOKI-SEC           SECTION.
      *
           MOVE    "11"            TO  SPA-GMN-Q01-SRYKBNL    (1)
           MOVE    "初診"          TO  SPA-GMN-Q01-SRYKBNMEIL (1)
      *
           MOVE    "12"            TO  SPA-GMN-Q01-SRYKBNL    (2)
           MOVE    "再診"          TO  SPA-GMN-Q01-SRYKBNMEIL (2)
      *
           MOVE    "13"            TO  SPA-GMN-Q01-SRYKBNL    (3)
           MOVE    "医学管理"      TO  SPA-GMN-Q01-SRYKBNMEIL (3)
      *
           MOVE    "14"            TO  SPA-GMN-Q01-SRYKBNL    (4)
           MOVE    "在宅"          TO  SPA-GMN-Q01-SRYKBNMEIL (4)
      *
           MOVE    "20"            TO  SPA-GMN-Q01-SRYKBNL    (5)
           MOVE    "投薬"          TO  SPA-GMN-Q01-SRYKBNMEIL (5)
      *
           MOVE    "30"            TO  SPA-GMN-Q01-SRYKBNL    (6)
           MOVE    "注射"          TO  SPA-GMN-Q01-SRYKBNMEIL (6)
      *
           MOVE    "40"            TO  SPA-GMN-Q01-SRYKBNL    (7)
           MOVE    "処置"          TO  SPA-GMN-Q01-SRYKBNMEIL (7)
      *
           MOVE    "50"            TO  SPA-GMN-Q01-SRYKBNL    (8)
           MOVE    "手術"          TO  SPA-GMN-Q01-SRYKBNMEIL (8)
      *
           MOVE    "54"            TO  SPA-GMN-Q01-SRYKBNL    (9)
           MOVE    "麻酔"          TO  SPA-GMN-Q01-SRYKBNMEIL (9)
      *
           MOVE    "60"            TO  SPA-GMN-Q01-SRYKBNL    (10)
           MOVE    "検査"          TO  SPA-GMN-Q01-SRYKBNMEIL (10)
      *
           MOVE    "64"            TO  SPA-GMN-Q01-SRYKBNL    (11)
           MOVE    "病理診断"      TO  SPA-GMN-Q01-SRYKBNMEIL (11)
      *
           MOVE    "70"            TO  SPA-GMN-Q01-SRYKBNL    (12)
           MOVE    "画像診断"      TO  SPA-GMN-Q01-SRYKBNMEIL (12)
      *
           MOVE    "80"            TO  SPA-GMN-Q01-SRYKBNL    (13)
           MOVE    "リハビリ・その他"
                                   TO  SPA-GMN-Q01-SRYKBNMEIL (13)
      *
           MOVE    "83"            TO  SPA-GMN-Q01-SRYKBNL    (14)
           MOVE    "精神科専門"    TO  SPA-GMN-Q01-SRYKBNMEIL (14)
      *
           MOVE    "84"            TO  SPA-GMN-Q01-SRYKBNL    (15)
           MOVE    "放射線治療"    TO  SPA-GMN-Q01-SRYKBNMEIL (15)
      *
           MOVE    "90"            TO  SPA-GMN-Q01-SRYKBNL    (16)
           MOVE    "入院"          TO  SPA-GMN-Q01-SRYKBNMEIL (16)
      *
           MOVE    "95"            TO  SPA-GMN-Q01-SRYKBNL    (17)
           MOVE    "保険外（消費税なし）"
                                   TO  SPA-GMN-Q01-SRYKBNMEIL (17)
      *
           MOVE    "96"            TO  SPA-GMN-Q01-SRYKBNL    (18)
           MOVE    "保険外（消費税あり）"
                                   TO  SPA-GMN-Q01-SRYKBNMEIL (18)
      *
           MOVE    "97"            TO  SPA-GMN-Q01-SRYKBNL    (19)
           MOVE    "食事"          TO  SPA-GMN-Q01-SRYKBNMEIL (19)
      *
           MOVE    "98"            TO  SPA-GMN-Q01-SRYKBNL    (20)
           MOVE    "処方箋コメント"
                                   TO  SPA-GMN-Q01-SRYKBNMEIL (20)
      *
           MOVE    "99"            TO  SPA-GMN-Q01-SRYKBNL    (21)
           MOVE    "コメント"      TO  SPA-GMN-Q01-SRYKBNMEIL (21)
      *
           MOVE    21              TO  SPA-GMN-Q01-SRYKBNLST-MAX
      *
           .
       3101-SRYKBN-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    診療種別区分コンボ初期処理
      *****************************************************************
       3101-SRYSYUKBN-CMB-SYOKI-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-Q01-SRYSYUKBNLST-G
           EVALUATE    SPA-GMN-Q01-SRYKBN
           WHEN    "20"
               MOVE    "21"        TO  SPA-GMN-Q01-SRYSYUKBNL    (1)
               MOVE    "内服"      TO  SPA-GMN-Q01-SRYSYUKBNMEIL (1)
               MOVE    "22"        TO  SPA-GMN-Q01-SRYSYUKBNL    (2)
               MOVE    "頓服"      TO  SPA-GMN-Q01-SRYSYUKBNMEIL (2)
               MOVE    "23"        TO  SPA-GMN-Q01-SRYSYUKBNL    (3)
               MOVE    "外用"      TO  SPA-GMN-Q01-SRYSYUKBNMEIL (3)
               MOVE    "24"        TO  SPA-GMN-Q01-SRYSYUKBNL    (4)
               MOVE    "入院調剤料"
                                   TO  SPA-GMN-Q01-SRYSYUKBNMEIL (4)
               MOVE    "25"        TO  SPA-GMN-Q01-SRYSYUKBNL    (5)
               MOVE    "処方料"    TO  SPA-GMN-Q01-SRYSYUKBNMEIL (5)
               MOVE    "26"        TO  SPA-GMN-Q01-SRYSYUKBNL    (6)
               MOVE    "麻毒加算"  TO  SPA-GMN-Q01-SRYSYUKBNMEIL (6)
               MOVE    "27"        TO  SPA-GMN-Q01-SRYSYUKBNL    (7)
               MOVE    "調剤技術基本料"
                                   TO  SPA-GMN-Q01-SRYSYUKBNMEIL (7)
               MOVE    7           TO  SPA-GMN-Q01-SRYSYUKBNLST-MAX
           WHEN    "30"
               MOVE    "31"        TO  SPA-GMN-Q01-SRYSYUKBNL    (1)
               MOVE    "皮下筋肉注射"
                                   TO  SPA-GMN-Q01-SRYSYUKBNMEIL (1)
               MOVE    "32"        TO  SPA-GMN-Q01-SRYSYUKBNL    (2)
               MOVE    "静脈注射"  TO  SPA-GMN-Q01-SRYSYUKBNMEIL (2)
               MOVE    "33"        TO  SPA-GMN-Q01-SRYSYUKBNL    (3)
               MOVE    "点滴注射・その他"
                                   TO  SPA-GMN-Q01-SRYSYUKBNMEIL (3)
               MOVE    3           TO  SPA-GMN-Q01-SRYSYUKBNLST-MAX
           WHEN    "90"
               MOVE    "90"        TO  SPA-GMN-Q01-SRYSYUKBNL    (1)
               MOVE    "入院基本"  TO  SPA-GMN-Q01-SRYSYUKBNMEIL (1)
               MOVE    "92"        TO  SPA-GMN-Q01-SRYSYUKBNL    (2)
               MOVE    "特定入院・その他"
                                   TO  SPA-GMN-Q01-SRYSYUKBNMEIL (2)
               MOVE    2           TO  SPA-GMN-Q01-SRYSYUKBNLST-MAX
           END-EVALUATE
      *
           .
       3101-SRYSYUKBN-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    ソートキー項目初期処理
      *****************************************************************
       3101-SORT-KMK-SYOKI-SEC             SECTION.
      *
           MOVE    5                   TO  SPA-GMN-Q01-SORTLST-MAX
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-Q01-SORTLST-MAX )
               MOVE    CONST-KMK-ITEM (IDX1)
                                       TO  SPA-GMN-Q01-SORTLST (IDX1)
           END-PERFORM
      *
           .
       3101-SORT-KMK-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    並び順ラジオボタン初期処理
      *****************************************************************
       3101-RDIO-SYOKI-SEC             SECTION.
      *
      *    ソートキー並び順ラジオボタン
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   3 )
      *
               MOVE    "T"             TO  SPA-GMN-Q01-RDIO1 (IDX1)
               MOVE    "F"             TO  SPA-GMN-Q01-RDIO2 (IDX1)
      *
           END-PERFORM
      *
           .
       3101-RDIO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                         SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"           ALSO    "B01"
               PERFORM 210-BACK
      *    条件クリア
           WHEN    "CLICKED"           ALSO    "B02"
               PERFORM 480-CLEAR-SEC
           WHEN    "CLICKED"           ALSO    "B04"
               PERFORM 4100-SRYYMD-SET-SEC
           WHEN    "CLICKED"           ALSO    "B04S"
               PERFORM 4100-SRYYM-SET-SEC
      *    条件クリア（全て）
           WHEN    "CLICKED"           ALSO    "B02S"
               PERFORM 480-ALL-CLEAR-SEC
      *    予約登録へ
      * xx     WHEN    "CLICKED"       ALSO    "B10"
      * xx         PERFORM 480-YOYAKU-SEC
      *    受付一覧へ
      * xx     WHEN    "CLICKED"       ALSO    "B11"
      * xx         PERFORM 490-UKEICHI-SEC
      *    タブ切替
           WHEN    "CLICKED"           ALSO    "B08"
               PERFORM 480-TABSWITCH-SEC
      *    検索
           WHEN    "CLICKED"           ALSO    "B12"
               PERFORM 490-KENSAKU-SEC
      *    テスト患者チェックボタン
           WHEN    "CLICKED"           ALSO    "CHKTSTPT"
               PERFORM 410-CHKTSTPT-SET-SEC
      *    住所ボタン
           WHEN    "CLICKED"           ALSO    "JYUSHOBT1"
           WHEN    "CLICKED"           ALSO    "JYUSHOBT2"
           WHEN    "CLICKED"           ALSO    "JYUSHOBT3"
           WHEN    "CLICKED"           ALSO    "JYUSHOBT4"
           WHEN    "CLICKED"           ALSO    "JYUSHOBT5"
               MOVE    MCP-WIDGET   TO  WRK-WIDGET
               PERFORM 410-JYUSHO-SET-SEC
      *    病名コード編集
           WHEN    "CLICKED"           ALSO    "BYOBT1"
           WHEN    "CLICKED"           ALSO    "BYOBT2"
           WHEN    "CLICKED"           ALSO    "BYOBT3"
           WHEN    "CLICKED"           ALSO    "BYOBT4"
           WHEN    "CLICKED"           ALSO    "BYOBT5"
               MOVE    MCP-WIDGET   TO  WRK-WIDGET
               PERFORM 420-BYOMEICD-SET-SEC
      *    診療行為コード編集
           WHEN    "CLICKED"           ALSO    "SRYBT1"
           WHEN    "CLICKED"           ALSO    "SRYBT2"
           WHEN    "CLICKED"           ALSO    "SRYBT3"
           WHEN    "CLICKED"           ALSO    "SRYBT4"
           WHEN    "CLICKED"           ALSO    "SRYBT5"
               MOVE    MCP-WIDGET   TO  WRK-WIDGET
               PERFORM 430-SRYCD-SET-SEC
      *    病名検索トグルボタン
           WHEN    "CLICKED"           ALSO    "RDBYOMEI_U"
      *
      *        画面ＳＰＡ編集処理
               PERFORM 4101-GMN-SPA-SET-SEC
      *
               MOVE    WIDGET-NORMAL
                                    TO  SPA-GMN-Q01-UBYOMEI-STATE
               MOVE    "T"          TO  SPA-GMN-Q01-RDBYOMEI-U
               MOVE    "F"          TO  SPA-GMN-Q01-RDBYOMEI-A
               PERFORM 4118-BYOMEISEL-CHK-SEC
           WHEN    "CLICKED"       ALSO    "RDBYOMEI_A"
      *
      *        画面ＳＰＡ編集処理
               PERFORM 4101-GMN-SPA-SET-SEC
      *
               MOVE    WIDGET-INSENSITIVE
                                    TO  SPA-GMN-Q01-UBYOMEI-STATE
               MOVE    SPACE        TO  SPA-GMN-Q01-UBYOMEI-G
               MOVE    "F"          TO  SPA-GMN-Q01-RDBYOMEI-U
               MOVE    "T"          TO  SPA-GMN-Q01-RDBYOMEI-A
               IF  ( SPA-GMN-Q01-CUR   =   301 )
                       MOVE    302  TO  SPA-GMN-Q01-CUR
               END-IF
               PERFORM 4118-BYOMEISEL-CHK-SEC
      *    並び順トグルボタン
           WHEN    "CLICKED"       ALSO    "RDIO11"
           WHEN    "CLICKED"       ALSO    "RDIO12"
           WHEN    "CLICKED"       ALSO    "RDIO21"
           WHEN    "CLICKED"       ALSO    "RDIO22"
           WHEN    "CLICKED"       ALSO    "RDIO31"
           WHEN    "CLICKED"       ALSO    "RDIO32"
               PERFORM 450-RDIO-SET-SEC
      *    タブ
           WHEN    "CLICKED"       ALSO    "N01"
           WHEN    "CLICKED"       ALSO    "N02"
           WHEN    "CLICKED"       ALSO    "N03"
           WHEN    "CLICKED"       ALSO    "N04"
               MOVE    MCP-WIDGET   TO  WRK-WIDGET
      *
               PERFORM 440-TAB-CHANGE-SEC
           WHEN    "CLICKED"       ALSO    "RDKOHNUM_AND"
           WHEN    "CLICKED"       ALSO    "RDKOHNUM_OR"
      *        画面をＳＰＡにセット
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    Q01-RDKOHNUM-AND    TO  SPA-GMN-Q01-RDKOHNUM-AND
               MOVE    Q01-RDKOHNUM-OR     TO  SPA-GMN-Q01-RDKOHNUM-OR
           WHEN    "CLICKED"       ALSO    "RDHKNKOH_AND"
           WHEN    "CLICKED"       ALSO    "RDHKNKOH_OR"
      *        画面をＳＰＡにセット
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    Q01-RDHKNKOH-AND    TO  SPA-GMN-Q01-RDHKNKOH-AND
               MOVE    Q01-RDHKNKOH-OR     TO  SPA-GMN-Q01-RDHKNKOH-OR
           WHEN    "CLICKED"       ALSO    "RDBYOMEI_AND"
           WHEN    "CLICKED"       ALSO    "RDBYOMEI_OR"
      *        画面をＳＰＡにセット
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    Q01-RDBYOMEI-AND    TO  SPA-GMN-Q01-RDBYOMEI-AND
               MOVE    Q01-RDBYOMEI-OR     TO  SPA-GMN-Q01-RDBYOMEI-OR
           WHEN    "CLICKED"       ALSO    "RDSRYACT_AND"
           WHEN    "CLICKED"       ALSO    "RDSRYACT_OR"
      *        画面をＳＰＡにセット
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    Q01-RDSRYACT-AND    TO  SPA-GMN-Q01-RDSRYACT-AND
               MOVE    Q01-RDSRYACT-OR     TO  SPA-GMN-Q01-RDSRYACT-OR
           WHEN    "CLICKED"       ALSO    "BTBYOBUBUN"
               PERFORM 4101-BTBYOBUBUN-CHK-SEC
           WHEN    "CLICKED"       ALSO    "BTBYOKOHO"
               PERFORM 4101-BTBYOKOHO-CHK-SEC
           WHEN    "CLICKED"       ALSO    "BTSRYBUBUN"
               PERFORM 4101-BTSRYBUBUN-CHK-SEC
           WHEN    "CLICKED"       ALSO    "BTSRYKOHO"
               PERFORM 4101-BTSRYKOHO-CHK-SEC
           WHEN    "CLICKED"       ALSO    "B05S"
      *        検索条件保存内容削除
               PERFORM 450-SELPATTERN-DEL-SEC
           WHEN    "CLICKED"       ALSO    "B06S"
               MOVE    "UPD"       TO  WRK-UPDKBN
               PERFORM 470-Q04-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B07S"
               MOVE    "INS"       TO  WRK-UPDKBN
               PERFORM 470-Q04-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B06"
               MOVE    ZERO        TO  FLG-CHGSRYYMD
               PERFORM 4100-CHGSRYYMD-SEC
           WHEN    "CLICKED"       ALSO    "B07"
               MOVE    1           TO  FLG-CHGSRYYMD
               PERFORM 4100-CHGSRYYMD-SEC
           END-EVALUATE
      *
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
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *
      *    患者照会検索条件チェック処理
           WHEN    "ACTIVATE"      ALSO    "SELPATTERN"
               PERFORM 4100-SELPATTERN-CHK-SEC
      *
      *    病名分類区分チェック処理
           WHEN    "ACTIVATE"      ALSO    "BYOBUN"
               PERFORM 4118-BYOBUN-CHK-SEC
      *
      *    病名リスト選択チェック
           WHEN    "ACTIVATE"      ALSO    "BYOMEISEL"
               PERFORM 4118-BYOMEISEL-CHK-SEC
      *
      *    病名診療行為リスト選択チェック
           WHEN    "ACTIVATE"      ALSO   "SRYACTSEL"
               PERFORM 4120-SRYACTSEL-CHK-SEC
      *
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
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
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-Q01-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    開始診療年月チェック
           PERFORM 4100-STSRYYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    終了診療年月チェック
           PERFORM 4101-EDSRYYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-Q01-PAGENO
      *    基本情報タブ
           WHEN    ZERO
               PERFORM 4101-KHN-INPUT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  410-INPUT-CHK-EXT
               END-IF
      *    保険　公費タブ
           WHEN    1
               PERFORM 4101-HKN-INPUT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  410-INPUT-CHK-EXT
               END-IF
      *    病名　診療行為タブ
           WHEN    2
               PERFORM 4101-BYOSRY-INPUT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  410-INPUT-CHK-EXT
               END-IF
      *    並び順タブ
           WHEN    3
               PERFORM 4101-SRT-INPUT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  410-INPUT-CHK-EXT
               END-IF
           END-EVALUATE
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
      *    開始診療年月日
           MOVE    Q01-STSRYYMD            TO  SPA-GMN-Q01-STSRYYMD
      *    終了診療年月日
           MOVE    Q01-EDSRYYMD            TO  SPA-GMN-Q01-EDSRYYMD
      *    開始生年月日
           MOVE    Q01-STBIRTHDAY          TO  SPA-GMN-Q01-STBIRTHDAY
      *    終了生年月日
           MOVE    Q01-EDBIRTHDAY          TO  SPA-GMN-Q01-EDBIRTHDAY
      *    開始年齢
           MOVE    Q01-STAGE               TO  SPA-GMN-Q01-STAGE
      *    終了年齢
           MOVE    Q01-EDAGE               TO  SPA-GMN-Q01-EDAGE
      *    基準日
           MOVE    Q01-KJYNYMD             TO  SPA-GMN-Q01-KJYNYMD
      *    誕生月
           MOVE    Q01-BIRTHMON            TO  SPA-GMN-Q01-BIRTHMON
      *    性別
           MOVE    Q01-SEX                 TO  SPA-GMN-Q01-SEX-G
      *    開始最終受診日
           MOVE    Q01-STLASTYMD           TO  SPA-GMN-Q01-STLASTYMD
      *    終了最終受診日
           MOVE    Q01-EDLASTYMD           TO  SPA-GMN-Q01-EDLASTYMD
      *    入外区分
           MOVE    Q01-NYUGAIKBN           TO  SPA-GMN-Q01-NYUGAIKBN-G
      *    診療科
           MOVE    Q01-SRYKA               TO  SPA-GMN-Q01-SRYKA-G
      *    死亡
           MOVE    Q01-DEATH               TO  SPA-GMN-Q01-DEATH-G
      *    開始患者番号
           MOVE    Q01-STPTNUM             TO  SPA-GMN-Q01-STPTNUM
      *    終了患者番号
           MOVE    Q01-EDPTNUM             TO  SPA-GMN-Q01-EDPTNUM
      *    電話先
           MOVE    Q01-TEL-TO              TO  SPA-GMN-Q01-TEL-TO-G
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL   IDXB    >       5
      *        郵便番号
               MOVE    Q01-POST    (IDXB)
                                       TO  SPA-GMN-Q01-POST   (IDXB)
      *        住所
               MOVE    Q01-JYUSHO  (IDXB)
                                       TO  SPA-GMN-Q01-JYUSHO  (IDXB)
      *        氏名
               MOVE    Q01-NAME    (IDXB)
                                       TO  SPA-GMN-Q01-NAME    (IDXB)
           END-PERFORM
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL   IDXB    >       3
      *
      *        状態１
               MOVE    Q01-CONDITION1 (IDXB)
                                       TO  SPA-GMN-Q01-CONDITION1-G
                                                               (IDXB)
      *        状態２
               MOVE    Q01-CONDITION2 (IDXB)
                                       TO  SPA-GMN-Q01-CONDITION2-G
                                                               (IDXB)
      *        状態３
               MOVE    Q01-CONDITION3 (IDXB)
                                       TO  SPA-GMN-Q01-CONDITION3-G
                                                               (IDXB)
      *        減免事由
               MOVE    Q01-GENMEN (IDXB)
                                       TO  SPA-GMN-Q01-GENMEN-G
                                                               (IDXB)
      *        コメント
               MOVE    Q01-COMMENT (IDXB)
                                       TO  SPA-GMN-Q01-COMMENT  (IDXB)
      *        電話番号
               MOVE    Q01-TEL (IDXB)  TO  SPA-GMN-Q01-TEL (IDXB)
           END-PERFORM
      *
      *    本人家族
           MOVE    Q01-HONKZKKBN       TO  SPA-GMN-Q01-HONKZKKBN-G
      *
      *    コメント選択
           MOVE    Q01-SELCOMMENT      TO  SPA-GMN-Q01-SELCOMMENT-G
      *
      *    低所得
           MOVE    Q01-TSY             TO  SPA-GMN-Q01-TSY-G
           MOVE    Q01-STTSYYM         TO  SPA-GMN-Q01-STTSYYM
           MOVE    Q01-STTSYSKBT       TO  SPA-GMN-Q01-STTSYSKBT-G
           MOVE    Q01-EDTSYYM         TO  SPA-GMN-Q01-EDTSYYM
      *
      *    特記事項
           MOVE    Q01-TOKKICD         TO  SPA-GMN-Q01-TOKKICD
           MOVE    Q01-TOKKINAME       TO  SPA-GMN-Q01-TOKKINAME
           MOVE    Q01-TOKKIINFO       TO  SPA-GMN-Q01-TOKKIINFO
      *
      *    災害該当
           MOVE    Q01-SAIGAIKBN       TO  SPA-GMN-Q01-SAIGAIKBN-G
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL   IDXB    >       5
      *
      *        保険者番号
               MOVE    Q01-HKNJANUM(IDXB)
                                       TO  SPA-GMN-Q01-HKNJANUM (IDXB)
      *        保険の種類
               MOVE    Q01-HKNNUM  (IDXB)
                                       TO  SPA-GMN-Q01-HKNNUM-G (IDXB)
      *        記号
               MOVE    Q01-KIGO    (IDXB)
                                       TO  SPA-GMN-Q01-KIGO     (IDXB)
      *
      *        番号
               MOVE    Q01-NUM     (IDXB)
                                       TO  SPA-GMN-Q01-NUM      (IDXB)
      *
      *        公費の種類
               MOVE    Q01-KOHNUM  (IDXB)
                                       TO  SPA-GMN-Q01-KOHNUM-G (IDXB)
      *
      *        負担者番号
               MOVE    Q01-FTNJANUM(IDXB)
                                       TO  SPA-GMN-Q01-FTNJANUM (IDXB)
      *
      *
           END-PERFORM
      *
      *    疑い病名区分
           MOVE    Q01-UTAGAIKBN       TO  SPA-GMN-Q01-UTAGAIKBN-G
      *
      *    急性病名区分
           MOVE    Q01-KYUSEIKBN       TO  SPA-GMN-Q01-KYUSEIKBN-G
      *
      *    転帰区分
           MOVE    Q01-TENKIKBN        TO  SPA-GMN-Q01-TENKIKBN-G
      *
      *    転帰日
           MOVE    Q01-STTENKIYMD      TO  SPA-GMN-Q01-STTENKIYMD
           MOVE    Q01-EDTENKIYMD      TO  SPA-GMN-Q01-EDTENKIYMD
      *
      *    主病名区分
           MOVE    Q01-SYUBYOKBN       TO  SPA-GMN-Q01-SYUBYOKBN-G
      *
      *    診療区分
           MOVE    Q01-SRYKBN          TO  SPA-GMN-Q01-SRYKBN-G
      *
      *    診療種別区分
           MOVE    Q01-SRYSYUKBN       TO  SPA-GMN-Q01-SRYSYUKBN-G
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL   IDXB    >       5
      *        病名コード
               MOVE    Q01-BYOMEICD(IDXB)
                                       TO  SPA-GMN-Q01-BYOMEICD(IDXB)
      *        病名
               MOVE    Q01-BYOMEI  (IDXB)
                                       TO  SPA-GMN-Q01-BYOMEI  (IDXB)
      *        診療行為コード
               MOVE    Q01-SRYCD(IDXB)
                                       TO  SPA-GMN-Q01-SRYCD(IDXB)
           END-PERFORM
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL   IDXB    >       3
      *        レセプトコメント
               MOVE    Q01-RECECOM(IDXB)
                                       TO  SPA-GMN-Q01-RECECOM(IDXB)
           END-PERFORM
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL   IDXB    >       3
      *
      *       並び順
              MOVE    Q01-CMB-KMK (IDXB)
                                       TO  SPA-GMN-Q01-KMK-G (IDXB)
      *
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    基本情報タブ入力チェック処理
      *****************************************************************
       4101-KHN-INPUT-CHK-SEC          SECTION.
      *
      *    開始生年月日チェック
           PERFORM 4102-STBIRTHDAY-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    終了生年月日チェック
           PERFORM 4103-EDBIRTHDAY-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    開始年齢チェック
           PERFORM 4104-STAGE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    終了年齢チェック
           PERFORM 4105-EDAGE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    終了年齢チェック
           PERFORM 4105-EDAGE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    基準日チェック
           PERFORM 4106-KJYNYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    誕生月チェック
           PERFORM 4106-BIRTHMON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    性別チェック
           PERFORM 4107-SEX-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    開始最終受診日チェック
           PERFORM 4108-STLASTYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    終了最終受診日チェック
           PERFORM 4108-EDLASTYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    入外区分チェック
           PERFORM 4117-NYUGAIKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    診療科チェック
           PERFORM 4117-SRYKA-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    死亡チェック
           PERFORM 4117-DEATH-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    開始患者番号チェック
           PERFORM 4109-STPTNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    終了患者番号チェック
           PERFORM 4109-EDPTNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    郵便番号チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
               PERFORM 4110-POST-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                 OR  ( FLG-END     NOT =   ZERO  )
                   GO  TO  4101-KHN-INPUT-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    住所チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
               PERFORM 4110-JYUSHO-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-KHN-INPUT-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    漢字氏名チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
               PERFORM 4112-NAME-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-KHN-INPUT-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    状態１チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
      *
               PERFORM 4114-CONDITION1-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-KHN-INPUT-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    状態２チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
      *
               PERFORM 4115-CONDITION2-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-KHN-INPUT-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    状態３チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
      *
               PERFORM 4116-CONDITION3-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-KHN-INPUT-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    減免事由チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
      *
               PERFORM 4116-GENMEN-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-KHN-INPUT-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    コメント選択チェック
           PERFORM 4116-SELCOMMENT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    コメントチェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
      *
               PERFORM 4113-COMMENT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-KHN-INPUT-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    電話先チェック
           PERFORM 4113-TEL-TO-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    電話番号チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
      *
               PERFORM 4113-TEL-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-KHN-INPUT-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    低所得コンボチェック
           PERFORM 4117-TSY-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    低所得開始日識別チェック
           PERFORM 4117-STTSYSKBT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    低所得開始日チェック
           PERFORM 4117-STTSYYM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    低所得終了日チェック
           PERFORM 4117-EDTSYYM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    特記事項コードチェック
           PERFORM 4117-TOKKICD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    特記事項名称チェック
           PERFORM 4117-TOKKINAME-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    特記事項情報チェック
           PERFORM 4117-TOKKIINFO-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
      *    災害該当情報チェック
           PERFORM 4117-SAIGAIKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-KHN-INPUT-CHK-EXT
           END-IF
      *
           .
       4101-KHN-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険　公費タブ入力チェック処理
      *****************************************************************
       4101-HKN-INPUT-CHK-SEC          SECTION.
      *
      *    本人家族チェック
           PERFORM 4114-HONKZKKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-HKN-INPUT-CHK-EXT
           END-IF
      *
      *    保険者番号チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
               PERFORM 4114-HKNJANUM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-HKN-INPUT-CHK-EXT
               END-IF
           END-PERFORM
      *
      *    保険の種類チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
               PERFORM 4114-HKNNUM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-HKN-INPUT-CHK-EXT
               END-IF
           END-PERFORM
      *
      *    記号チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
               PERFORM 4114-KIGO-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-HKN-INPUT-CHK-EXT
               END-IF
           END-PERFORM
      *
      *    番号チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
               PERFORM 4115-NUM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-HKN-INPUT-CHK-EXT
               END-IF
           END-PERFORM
      *
      *    公費の種類チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
               PERFORM 4116-KOHNUM-CHK-SEC  
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-HKN-INPUT-CHK-EXT
               END-IF
           END-PERFORM
      *
      *    負担者番号チェック処理
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
               PERFORM 4117-FTNJANUM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-HKN-INPUT-CHK-EXT
               END-IF
           END-PERFORM
      *
           .
       4101-HKN-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名　診療行為タブ入力チェック処理
      *****************************************************************
       4101-BYOSRY-INPUT-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        疑い病名チェック
               PERFORM 4119-UTAGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        急性病名チェック
               PERFORM 4119-KYUSEIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        転帰区分チェック
               PERFORM 4119-TENKIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        開始転帰日チェック
               PERFORM 4119-STTENKIYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        終了転帰日チェック
               PERFORM 4119-EDTENKIYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        主病名区分チェック
               PERFORM 4119-SYUBYOKBN-CHK-SEC
           END-IF
      *
      *    病名コードチェック処理
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               PERFORM 4119-BYOMEICD-CHK-SEC
      *
           END-PERFORM
      *
      *    病名チェック処理
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               PERFORM 4119-BYOMEI-CHK-SEC
      *
           END-PERFORM
      *
      *    診療区分チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4121-SRYKBN-CHK-SEC
           END-IF
      *
      *    診療種別区分チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4121-SRYSYUKBN-CHK-SEC
           END-IF
      *
      *    診療行為コードチェック処理
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
                    OR   ( FLG-END     NOT =   ZERO  )
      *
               PERFORM 4121-SRYCD-CHK-SEC
      *
           END-PERFORM
      *
      *    レセプトコメントチェック処理
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
                    OR   ( FLG-END     NOT =   ZERO  )
      *
               PERFORM 4121-RECECOM-CHK-SEC
      *
           END-PERFORM
      *
           .
       4101-BYOSRY-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順タブ入力チェック処理
      *****************************************************************
       4101-SRT-INPUT-CHK-SEC          SECTION.
      *
      *    並び順項目名チェック処理
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
      *
               PERFORM 4122-KMK-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-SRT-INPUT-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
           .
       4101-SRT-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始診療年月チェック
      *****************************************************************
       4100-STSRYYMD-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-STSRYYMD
      *
      *    日付妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-STSRYYMD  NOT =   SPACE )
               MOVE    SPA-GMN-Q01-STSRYYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF  ( SPA-ERRCD         =   SPACE )
                   MOVE    WRK-SYMD    TO  SPA-NAI-Q01-STSRYYMD
                   MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-STSRYYMD
               ELSE
                   MOVE    "0101"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-Q01-CUR
                   GO  TO  4100-STSRYYMD-CHK-EXT
               END-IF
           END-IF
      *
           .
       4100-STSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了診療年月チェック
      *****************************************************************
       4101-EDSRYYMD-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-EDSRYYMD
      *
      *    日付妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-EDSRYYMD  NOT =   SPACE )
               MOVE    SPA-GMN-Q01-EDSRYYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF  ( SPA-ERRCD     =   SPACE )
                   MOVE    WRK-SYMD    TO  SPA-NAI-Q01-EDSRYYMD
                   MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-EDSRYYMD
               ELSE
                   MOVE    "0101"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-Q01-CUR
                   GO  TO  4101-EDSRYYMD-CHK-EXT
               END-IF
           END-IF
      *
           .
       4101-EDSRYYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始生年月日チェック
      *****************************************************************
       4102-STBIRTHDAY-CHK-SEC     SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-STBIRTHDAY
      *
      *    日付妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-STBIRTHDAY    NOT =   SPACE )
               MOVE    SPA-GMN-Q01-STBIRTHDAY
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF  ( SPA-ERRCD         =   SPACE )
                   MOVE    WRK-SYMD        TO  SPA-NAI-Q01-STBIRTHDAY
                   MOVE    WRK-HENYMDG     TO  SPA-GMN-Q01-STBIRTHDAY
               ELSE
                   MOVE    "0102"          TO  SPA-ERRCD
                   MOVE    101             TO  SPA-GMN-Q01-CUR
                   GO  TO  4102-STBIRTHDAY-CHK-EXT
               END-IF
           END-IF
      *
           .
       4102-STBIRTHDAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了生年月日チェック
      *****************************************************************
       4103-EDBIRTHDAY-CHK-SEC     SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-EDBIRTHDAY
      *
           MOVE    Q01-EDBIRTHDAY      TO  SPA-GMN-Q01-EDBIRTHDAY
      *
      *    日付妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-EDBIRTHDAY    NOT =   SPACE )
               MOVE    SPA-GMN-Q01-EDBIRTHDAY
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF  ( SPA-ERRCD     =   SPACE )
                   MOVE    WRK-SYMD    TO  SPA-NAI-Q01-EDBIRTHDAY
                   MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-EDBIRTHDAY
               ELSE
                   MOVE    "0102"          TO  SPA-ERRCD
                   MOVE    102             TO  SPA-GMN-Q01-CUR
                   GO  TO  4103-EDBIRTHDAY-CHK-EXT
               END-IF
           END-IF
      *
      *
           .
       4103-EDBIRTHDAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始年齢チェック
      *****************************************************************
       4104-STAGE-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-Q01-STAGE
      *
      *    数値妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-STAGE NOT =   SPACE )
               INITIALIZE                     ORCSNUMAREA
               MOVE    SPA-GMN-Q01-STAGE   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
      *
                   MOVE    SPA-GMN-Q01-STAGE
                                       TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-KANACHK-CHK-SEC
                   MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  SPA-GMN-Q01-STAGE
      *
                   MOVE    "0103"          TO  SPA-ERRCD
                   MOVE    103             TO  SPA-GMN-Q01-CUR
                   GO  TO  4104-STAGE-CHK-EXT
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-NAI-Q01-STAGE
               END-IF
           END-IF
      *
           .
       4104-STAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了年齢チェック
      *****************************************************************
       4105-EDAGE-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-Q01-EDAGE
      *
      *    数値妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-EDAGE     NOT =   SPACE )
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-Q01-EDAGE   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
      *
                   MOVE    SPA-GMN-Q01-EDAGE
                                       TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-KANACHK-CHK-SEC
                   MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  SPA-GMN-Q01-EDAGE
      *
                   MOVE    "0103"          TO  SPA-ERRCD
                   MOVE    104             TO  SPA-GMN-Q01-CUR
                   GO  TO  4105-EDAGE-CHK-EXT
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-NAI-Q01-EDAGE
               END-IF
           END-IF
      *
           .
       4105-EDAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基準日チェック
      *****************************************************************
       4106-KJYNYMD-CHK-SEC        SECTION.
      *
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-KJYNYMD
      *
      *    基準日未入力は、システム日付をセット
           IF  ( SPA-GMN-Q01-KJYNYMD   =   SPACE )
               MOVE    SPA-SYSYMDWH    TO  SPA-GMN-Q01-KJYNYMD
               MOVE    SPA-SYSYMD      TO  SPA-NAI-Q01-KJYNYMD
      *
           ELSE
      *        日付妥当性チェック＆編集処理
               MOVE    SPA-GMN-Q01-KJYNYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF  ( SPA-ERRCD         =   SPACE )
                   MOVE    WRK-SYMD    TO  SPA-NAI-Q01-KJYNYMD
                   MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-KJYNYMD
               ELSE
                   MOVE    "0104"          TO  SPA-ERRCD
                   MOVE    105             TO  SPA-GMN-Q01-CUR
                   GO  TO  4106-KJYNYMD-CHK-EXT
               END-IF
           END-IF
      *
           .
       4106-KJYNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    誕生月チェック
      *****************************************************************
       4106-BIRTHMON-CHK-SEC        SECTION.
      *
           MOVE    ZERO             TO  SPA-NAI-Q01-BIRTHMON
      *
      *    数値妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-BIRTHMON  NOT =   SPACE )
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-Q01-BIRTHMON   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
      *
                   MOVE    SPA-GMN-Q01-BIRTHMON
                                       TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-KANACHK-CHK-SEC
                   MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  SPA-GMN-Q01-BIRTHMON
      *
                   MOVE    "0133"          TO  SPA-ERRCD
                   MOVE    158             TO  SPA-GMN-Q01-CUR
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-NAI-Q01-BIRTHMON
               END-IF
           END-IF
      *
           .
       4106-BIRTHMON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    性別チェック
      *****************************************************************
       4107-SEX-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-SEXLST-MAX )
               IF    ( SPA-GMN-Q01-SEX =   SPA-GMN-Q01-SEXL (IDX2))
                   MOVE    SPA-GMN-Q01-SEXLST (IDX2)
                                       TO  SPA-GMN-Q01-SEX-G
                   MOVE    1           TO  FLG-CHK
                   MOVE    SPA-GMN-Q01-SEXLST-MAX
                                       TO  IDX2
               END-IF
           END-PERFORM
      *
           IF    ( FLG-CHK         =   ZERO )
               MOVE    "0109"          TO  SPA-ERRCD
               MOVE    106             TO  SPA-GMN-Q01-CUR
               GO TO   4107-SEX-CHK-EXT
           END-IF
      *
           .
       4107-SEX-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始最終受診日チェック
      *****************************************************************
       4108-STLASTYMD-CHK-SEC      SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-STLASTYMD
      *
      *    日付妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-STLASTYMD NOT =   SPACE )
               MOVE    SPA-GMN-Q01-STLASTYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF  ( SPA-ERRCD         =   SPACE )
                   MOVE    WRK-SYMD    TO  SPA-NAI-Q01-STLASTYMD
                   MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-STLASTYMD
               ELSE    
                   MOVE    "0105"          TO  SPA-ERRCD
                   MOVE    107             TO  SPA-GMN-Q01-CUR
                   GO  TO  4108-STLASTYMD-CHK-EXT
               END-IF
           END-IF
      *
           .
       4108-STLASTYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了最終受診日チェック
      *****************************************************************
       4108-EDLASTYMD-CHK-SEC      SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-EDLASTYMD
      *
      *    日付妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-EDLASTYMD NOT =   SPACE )
               MOVE    SPA-GMN-Q01-EDLASTYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF  ( SPA-ERRCD         =   SPACE )
                   MOVE    WRK-SYMD    TO  SPA-NAI-Q01-EDLASTYMD
                   MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-EDLASTYMD
               ELSE    
                   MOVE    "0105"          TO  SPA-ERRCD
                   MOVE    108             TO  SPA-GMN-Q01-CUR
                   GO  TO  4108-EDLASTYMD-CHK-EXT
               END-IF
           END-IF
      *
           .
       4108-EDLASTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       4117-NYUGAIKBN-CHK-SEC      SECTION.
      *
           IF    ( SPA-GMN-Q01-NYUGAIKBN-G     =   SPACE )
               GO TO   4117-NYUGAIKBN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-NYUGAIKBNLST-MAX )
                      OR ( SPA-GMN-Q01-NYUGAIKBN
                                   =   SPA-GMN-Q01-NYUGAIKBNL (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-NYUGAIKBNLST-MAX )
               MOVE    SPA-GMN-Q01-NYUGAIKBNLST (IDX2)
                                   TO  SPA-GMN-Q01-NYUGAIKBN-G
           ELSE
               MOVE    "0118"      TO  SPA-ERRCD
               MOVE    144         TO  SPA-GMN-Q01-CUR
               GO TO   4117-NYUGAIKBN-CHK-EXT
           END-IF
      *
           .
       4117-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科チェック
      *****************************************************************
       4117-SRYKA-CHK-SEC      SECTION.
      *
           IF    ( SPA-GMN-Q01-SRYKA-G     =   SPACE )
               GO TO   4117-SRYKA-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-SRYKALST-MAX )
                      OR ( SPA-GMN-Q01-SRYKA
                                   =   SPA-GMN-Q01-SRYKAL (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-SRYKALST-MAX )
               MOVE    SPA-GMN-Q01-SRYKALST (IDX2)
                                       TO  SPA-GMN-Q01-SRYKA-G
           ELSE
               MOVE    "0117"          TO  SPA-ERRCD
               MOVE    109             TO  SPA-GMN-Q01-CUR
               GO TO   4117-SRYKA-CHK-EXT
           END-IF
      *
           .
       4117-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死亡チェック
      *****************************************************************
       4117-DEATH-CHK-SEC      SECTION.
      *
           IF    ( SPA-GMN-Q01-DEATH-G     =   SPACE )
               CONTINUE
           ELSE
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   SPA-GMN-Q01-DEATHLST-MAX )
                          OR ( SPA-GMN-Q01-DEATH
                                       =   SPA-GMN-Q01-DEATHL (IDX2))
      *
                   CONTINUE
      *
               END-PERFORM
      *
               IF    ( IDX2            <=  SPA-GMN-Q01-DEATHLST-MAX )
                   MOVE    SPA-GMN-Q01-DEATHLST (IDX2)
                                           TO  SPA-GMN-Q01-DEATH-G
               ELSE
                   MOVE    "0128"          TO  SPA-ERRCD
                   MOVE    156             TO  SPA-GMN-Q01-CUR
               END-IF
           END-IF
      *
           .
       4117-DEATH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始患者番号チェック処理
      *****************************************************************
       4109-STPTNUM-CHK-SEC            SECTION.
      *
           IF  ( SPA-GMN-Q01-STPTNUM   =   SPACE )
               GO  TO  4109-STPTNUM-CHK-EXT
           END-IF
      *
      *    拡張構成か、
      *    自由構成で数字型でない場合は単純に桁数をチェックし、
      *    上記以外は桁数（溢れ）チェック＋ZERO詰め編集を行う
           MOVE    SPACE               TO  WRK-PTNUM
           MOVE    1                   TO  WRK-PTNUM-KETA
      *
           STRING  SPA-GMN-Q01-STPTNUM DELIMITED   BY  SPACE
           INTO    WRK-PTNUM
           WITH    POINTER WRK-PTNUM-KETA
           END-STRING
      *
           COMPUTE WRK-PTNUM-KETA  =   WRK-PTNUM-KETA  -   1
      *
           EVALUATE    SPA-1009-PTNUMKSIKBN    ALSO  SPA-1009-JIYKSIKBN
           WHEN    "3"     ALSO    ANY
           WHEN    "1"     ALSO    NOT "2"
      *
      *        桁数チェック１
               PERFORM 41091-KETA-CNT-CHK1-SEC
      *
           WHEN    OTHER
      *
      *        桁数チェック２
               PERFORM 41091-KETA-CNT-CHK2-SEC
      *
           END-EVALUATE
      *
           IF  ( SPA-ERRCD         NOT =   SPACE )
               MOVE    110             TO  SPA-GMN-Q01-CUR
               GO  TO                  4109-STPTNUM-CHK-EXT
           END-IF
      *
           MOVE    WRK-PTNUM           TO   SPA-GMN-Q01-STPTNUM
      *
           .
       4109-STPTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了患者番号チェック処理
      *****************************************************************
       4109-EDPTNUM-CHK-SEC            SECTION.
      *
           IF  ( SPA-GMN-Q01-EDPTNUM   =   SPACE )
               GO  TO  4109-EDPTNUM-CHK-EXT
           END-IF
      *
      *    拡張構成か、
      *    自由構成で数字型でない場合は単純に桁数をチェックし、
      *    上記以外は桁数（溢れ）チェック＋ZERO詰め編集を行う
           MOVE    SPACE               TO  WRK-PTNUM
           MOVE    1                   TO  WRK-PTNUM-KETA
      *
           STRING  SPA-GMN-Q01-EDPTNUM DELIMITED   BY  SPACE
           INTO    WRK-PTNUM
           WITH    POINTER WRK-PTNUM-KETA
           END-STRING
      *
           COMPUTE WRK-PTNUM-KETA  =   WRK-PTNUM-KETA  -   1
      *
           EVALUATE    SPA-1009-PTNUMKSIKBN    ALSO  SPA-1009-JIYKSIKBN
           WHEN    "3"     ALSO    ANY
           WHEN    "1"     ALSO    NOT "2"
      *
      *        桁数チェック１
               PERFORM 41091-KETA-CNT-CHK1-SEC
      *
           WHEN    OTHER
      *
      *        桁数チェック２
               PERFORM 41091-KETA-CNT-CHK2-SEC
      *
           END-EVALUATE
      *
           IF  ( SPA-ERRCD         NOT =   SPACE )
               MOVE    111             TO  SPA-GMN-Q01-CUR
               GO  TO                  4109-EDPTNUM-CHK-EXT
           END-IF
      *
           MOVE    WRK-PTNUM           TO   SPA-GMN-Q01-EDPTNUM
      *
           .
       4109-EDPTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    桁数チェック１
      *****************************************************************
       41091-KETA-CNT-CHK1-SEC             SECTION.
      *
           MOVE    ZERO            TO      WRK-SYS-KETA
                                           WRK-SYS-KETA2
      *
           EVALUATE    SPA-1009-PTNUMKSIKBN
           WHEN    "1"
               MOVE    SPA-1009-JIYKSIKETA
                                   TO      WRK-SYS-KETA
           WHEN    "2"
               MOVE    SPA-1009-HJNKSIRENNUMKETA
                                   TO      WRK-SYS-KETA
               IF  ( SPA-1009-HJNKSINENKBN     =   "1" )
      *            和暦
                   MOVE    3   TO      WRK-SYS-KETA2
               ELSE
      *            西暦
                   MOVE    4   TO      WRK-SYS-KETA2
               END-IF
      *
               EVALUATE    SPA-1009-HJNKSIKBN
               WHEN    "1"
                   COMPUTE WRK-SYS-KETA    =   WRK-SYS-KETA
                                           +   WRK-SYS-KETA2
                                           +   1
               WHEN    "2"
                   COMPUTE WRK-SYS-KETA    =   WRK-SYS-KETA
                                           +   WRK-SYS-KETA2
               WHEN    "3"
                   COMPUTE WRK-SYS-KETA    =   WRK-SYS-KETA
                                           +   1
               WHEN    "4"
                   CONTINUE
               END-EVALUATE
           WHEN    "3"
               COMPUTE WRK-SYS-KETA        =  SPA-1009-KKCKSIMAEKETA
                                           +  SPA-1009-KKCKSIRENNUMKETA
                                           +  SPA-1009-KKCKSIATOKETA
           END-EVALUATE
      *
           IF  ( WRK-PTNUM-KETA    NOT =   WRK-SYS-KETA )
               MOVE    "0002"      TO  SPA-ERRCD
           END-IF
      *
           .
       41091-KETA-CNT-CHK1-EXT.
           EXIT.
      *
      *****************************************************************
      *    桁数チェック２
      *****************************************************************
       41091-KETA-CNT-CHK2-SEC             SECTION.
      *
           MOVE    ZERO            TO      WRK-SYS-KETA
      *
      *    自由構成の桁数編集
           IF  ( SPA-1009-PTNUMKSIKBN  =   "1" )
               MOVE    SPA-1009-JIYKSIKETA
                                   TO      WRK-SYS-KETA
           ELSE
      *
      *        標準構成の桁数
      *        システム管理マスタで設定された桁数をカウント
               MOVE    SPA-1009-HJNKSIRENNUMKETA
                                   TO      WRK-SYS-KETA
               IF  ( SPA-1009-HJNKSIKBN
                                       =   "1"  OR  "2" )
                   EVALUATE    SPA-1009-HJNKSINENKBN
                   WHEN    "1"
                       COMPUTE WRK-SYS-KETA    =   WRK-SYS-KETA  +   3
                   WHEN    "2"
                       COMPUTE WRK-SYS-KETA    =   WRK-SYS-KETA  +   4
                   END-EVALUATE
               END-IF
      *
               IF  ( SPA-1009-HJNKSIKBN    =   "1"  OR  "3" )
                   COMPUTE WRK-SYS-KETA    =   WRK-SYS-KETA  +   1
               END-IF
           END-IF
      *
      *    パラメータ（IN）の患者番号桁数が設定された桁数を超えた場合
           IF  ( WRK-PTNUM-KETA        >   WRK-SYS-KETA )
               MOVE    "0002"          TO  SPA-ERRCD
               GO  TO                  41091-KETA-CNT-CHK2-EXT
           END-IF
      *
      *    開始位置算出
           COMPUTE     WRK-ST          =   WRK-SYS-KETA
                                       -   WRK-PTNUM-KETA   +   1
      *
           IF  ( WRK-ST                =   ZERO )  OR
               ( WRK-ST                >   20   )
               MOVE    "0002"          TO  SPA-ERRCD
               GO  TO                  41091-KETA-CNT-CHK2-EXT
           END-IF
      *
      *    検索するため、患者番号をゼロ詰編集
           MOVE    ZERO                TO  WRK-PTNUM-WK
           MOVE    WRK-PTNUM  (1:WRK-PTNUM-KETA)
                                       TO  WRK-PTNUM-WK (WRK-ST:)
      *    ニューメリックチェック
           IF    ( WRK-PTNUM-WK (1:WRK-PTNUM-KETA)
                                       IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    "0106"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    WRK-PTNUM-WK        TO  WRK-PTNUM
      *
           .
       41091-KETA-CNT-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    郵便番号チェック処理
      *****************************************************************
       4110-POST-CHK-SEC           SECTION.
      *
           MOVE    IDXA                TO  IDX1
      *
           MOVE    WRK-MCP-WIDGET      TO  WRK-WIDGET
      *
           IF  ( SPA-GMN-Q01-POST(IDX1)   =   SPACE )
               GO  TO  4110-POST-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-Q01-POST(IDX1)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-POST(IDX1)
      *
           IF    ( WRK-WIDGET     (1:4) =  "POST" )
            AND  ( WRK-WIDGET4          =  IDX1 )
               CONTINUE
           ELSE
               GO  TO  4110-POST-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-Q01-POST (IDX1) (1:1)   =   "/" )
      *
      *        周辺住所検索画面に遷移する
               MOVE    IDX1            TO  WRK-JYUSHO-IDX
               PERFORM 410-Q98-SENI-SEC
               GO  TO  4110-POST-CHK-EXT
           END-IF
      *
      *    住所エリアを初期化
           MOVE    ZERO                TO  CNT-ADRS
           MOVE    SPACE               TO  WRK-EDITADRS-NAME
           INITIALIZE                  ADRS-REC
      *
           MOVE    SPA-GMN-Q01-POST (IDX1)
                                       TO  ADRS-POST
      *    住所検索処理
           PERFORM 900-ADRS-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ADRS    =   1 )
                    OR   ( CNT-ADRS    >   1 )
      *
      *        住所読込処理
               COMPUTE CNT-ADRS    =   CNT-ADRS    +   1
               IF  ( CNT-ADRS      =   1 )
                   MOVE    ADRS-EDITADRS-NAME
                                   TO  WRK-EDITADRS-NAME
               END-IF
      *
               PERFORM 900-ADRS-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    郵便番号による住所検索チェック
           EVALUATE    CNT-ADRS
      *
      *    該当件数０件
           WHEN    0
               MOVE    SPACE       TO  SPA-GMN-Q01-POST (IDX1)
      *
      *    該当件数１件
           WHEN    1
               MOVE    WRK-EDITADRS-NAME
                                   TO  SPA-GMN-Q01-JYUSHO  (IDX1)
      *
      *    複数件該当
           WHEN    OTHER
      *
               PERFORM 5011-INPUT-CUR-SEC
      *
      *        住所検索−町域選択画面に遷移する
               MOVE  IDX1          TO  SPA-Q01-Q99-JYUSHO-IDX
               MOVE  SPA-GMN-Q01-POST (IDX1)
                                   TO  SPA-Q01-Q99-POST
      *
      *
               MOVE    SPACE           TO  SPA-MOTOPG
               MOVE    SPA-AREA        TO  SPA-COMMON
               MOVE    SPA-Q01FREE     TO  SPA-FREE
      *
               MOVE    "LINK"          TO  MCP-STATUS
               MOVE    SPACE           TO  MCP-EVENT
      *
               CALL    "ORCGQ99"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
               MOVE    SPA-COMMON      TO  SPA-AREA
               MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
               MOVE  "SELNUM"      TO  MCP-WIDGET
      *
               MOVE  "Q01"         TO  SPA-MOTOPG
               MOVE  "Q99"         TO  SPA-SAKIPG
      *
               MOVE  "NEW"         TO  MCP-PUTTYPE
               MOVE  "Q99"         TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE  1             TO  FLG-END
      *
               GO  TO  4110-POST-CHK-EXT
      *
           END-EVALUATE
      *
           .
       4110-POST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所チェック処理
      *****************************************************************
       4110-JYUSHO-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-Q01-JYUSHO  (IDXA)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-JYUSHO  (IDXA)
      *
           .
       4110-JYUSHO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    漢字氏名チェック処理
      *****************************************************************
       4112-NAME-CHK-SEC           SECTION.
      *
           MOVE    IDXA            TO  IDX1
      *
           IF    ( SPA-ENCODING    =   CONST-UTF8 )
      *
      *        全角変換処理
               MOVE    SPA-GMN-Q01-NAME(IDX1)
                                   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 801-KANACHK2-CHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 801-ZENKAKU-CHG-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-NAME(IDX1)
           ELSE
      *
      *        全角変換処理
               MOVE    SPA-GMN-Q01-NAME(IDX1)
                                   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-CHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-ZENKAKU-CHG-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-NAME(IDX1)
           END-IF
      *
           .
       4112-NAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    状態１チェック処理
      *****************************************************************
       4114-CONDITION1-CHK-SEC         SECTION.
      *
           COMPUTE WRK-ERR-CUR         =   131     +   IDXA
      *
           IF    ( SPA-GMN-Q01-CONDITION1-G (IDXA) =   SPACE )
               GO  TO  4114-CONDITION1-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-CONDITION1LST-MAX )
                      OR ( SPA-GMN-Q01-CONDITION1 (IDXA)
                                   =   SPA-GMN-Q01-CONDITION1L (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-CONDITION1LST-MAX )
               MOVE    SPA-GMN-Q01-CONDITION1LST (IDX2)
                                       TO  SPA-GMN-Q01-CONDITION1-G
                                                               (IDXA)
           ELSE
               MOVE    "0114"          TO  SPA-ERRCD
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO TO   4114-CONDITION1-CHK-EXT
           END-IF
      *
           .
       4114-CONDITION1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    状態２チェック処理
      *****************************************************************
       4115-CONDITION2-CHK-SEC         SECTION.
      *
           COMPUTE WRK-ERR-CUR         =   134     +   IDXA
      *
           IF    ( SPA-GMN-Q01-CONDITION2-G (IDXA) =   SPACE )
               GO  TO  4115-CONDITION2-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-CONDITION2LST-MAX )
                      OR ( SPA-GMN-Q01-CONDITION2 (IDXA)
                                   =   SPA-GMN-Q01-CONDITION2L (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-CONDITION2LST-MAX )
               MOVE    SPA-GMN-Q01-CONDITION2LST (IDX2)
                                       TO  SPA-GMN-Q01-CONDITION2-G
                                                               (IDXA)
           ELSE
               MOVE    "0115"          TO  SPA-ERRCD
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO TO   4115-CONDITION2-CHK-EXT
           END-IF
      *
           .
       4115-CONDITION2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    状態３チェック処理
      *****************************************************************
       4116-CONDITION3-CHK-SEC         SECTION.
      *
           COMPUTE WRK-ERR-CUR         =   137     +   IDXA
      *
           IF    ( SPA-GMN-Q01-CONDITION3-G (IDXA) =   SPACE )
               GO  TO  4116-CONDITION3-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-CONDITION3LST-MAX )
                      OR ( SPA-GMN-Q01-CONDITION3 (IDXA)
                                   =   SPA-GMN-Q01-CONDITION3L (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-CONDITION3LST-MAX )
               MOVE    SPA-GMN-Q01-CONDITION3LST (IDX2)
                                       TO  SPA-GMN-Q01-CONDITION3-G
                                                               (IDXA)
           ELSE
               MOVE    "0116"          TO  SPA-ERRCD
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO TO   4116-CONDITION3-CHK-EXT
           END-IF
      *
           .
       4116-CONDITION3-CHK-EXT.
           EXIT.
      *****************************************************************
      *    減免事由チェック処理
      *****************************************************************
       4116-GENMEN-CHK-SEC         SECTION.
      *
           COMPUTE WRK-ERR-CUR         =   148     +   IDXA
      *
           IF    ( SPA-GMN-Q01-GENMEN-G (IDXA) =   SPACE )
               GO  TO  4116-GENMEN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-GENMENLST-MAX )
                      OR ( SPA-GMN-Q01-GENMEN (IDXA)
                                   =   SPA-GMN-Q01-GENMENL (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-GENMENLST-MAX )
               MOVE    SPA-GMN-Q01-GENMENLST (IDX2)
                                       TO  SPA-GMN-Q01-GENMEN-G
                                                               (IDXA)
           ELSE
               MOVE    "0123"          TO  SPA-ERRCD
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO TO   4116-GENMEN-CHK-EXT
           END-IF
      *
           .
       4116-GENMEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コメント選択チェック処理
      *****************************************************************
       4116-SELCOMMENT-CHK-SEC         SECTION.
      *
           PERFORM VARYING IDX2 FROM    1   BY  1
                   UNTIL ( IDX2 >   SPA-GMN-Q01-SELCOMMENTLST-MAX )
                      OR ( SPA-GMN-Q01-SELCOMMENT
                                =   SPA-GMN-Q01-SELCOMMENTL (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2        <=  SPA-GMN-Q01-SELCOMMENTLST-MAX )
               MOVE    SPA-GMN-Q01-SELCOMMENTLST (IDX2)
                               TO  SPA-GMN-Q01-SELCOMMENT-G
           ELSE
               MOVE    SPA-GMN-Q01-SELCOMMENTLST (1)
                               TO  SPA-GMN-Q01-SELCOMMENT-G
           END-IF
      *
           .
       4116-SELCOMMENT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コメントチェック処理
      *****************************************************************
       4113-COMMENT-CHK-SEC        SECTION.
      *
           COMPUTE WRK-ERR-CUR         =   140     +   IDXA
      *
           MOVE    SPA-GMN-Q01-COMMENT  (IDXA)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-COMMENT  (IDXA)
      *
           .
       4113-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    電話先チェック処理
      *****************************************************************
       4113-TEL-TO-CHK-SEC             SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-TEL-TOLST-MAX )
                      OR ( SPA-GMN-Q01-TEL-TO
                                   =   SPA-GMN-Q01-TEL-TOL (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-TEL-TOLST-MAX )
               MOVE    SPA-GMN-Q01-TEL-TOLST (IDX2)
                                       TO  SPA-GMN-Q01-TEL-TO-G
           ELSE
               MOVE    SPA-GMN-Q01-TEL-TOLST (1)
                                       TO  SPA-GMN-Q01-TEL-TO-G
           END-IF
      *
           .
       4113-TEL-TO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電話番号チェック処理
      *****************************************************************
       4113-TEL-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-Q01-TEL (IDXA)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-TEL (IDXA)
      *
           .
       4113-TEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低所得チェック処理
      *****************************************************************
       4117-TSY-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-Q01-TSY-G   =   SPACE )
               GO  TO  4117-TSY-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-TSYLST-MAX )
                      OR ( SPA-GMN-Q01-TSY
                                   =   SPA-GMN-Q01-TSYL (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-TSYLST-MAX )
               MOVE    SPA-GMN-Q01-TSYLST (IDX2)
                                       TO  SPA-GMN-Q01-TSY-G
           ELSE
               MOVE    "0124"          TO  SPA-ERRCD
               MOVE    152             TO  SPA-GMN-Q01-CUR
               GO TO   4117-TSY-CHK-EXT
           END-IF
      *
           .
       4117-TSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低所得開始日識別チェック処理
      *****************************************************************
       4117-STTSYSKBT-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-STTSYSKBTLST-MAX )
                      OR ( SPA-GMN-Q01-STTSYSKBT
                                   =   SPA-GMN-Q01-STTSYSKBTL (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-STTSYSKBTLST-MAX )
               MOVE    SPA-GMN-Q01-STTSYSKBTLST (IDX2)
                                   TO  SPA-GMN-Q01-STTSYSKBT-G
           ELSE
               MOVE    SPA-GMN-Q01-STTSYSKBTLST (1)
                                   TO  SPA-GMN-Q01-STTSYSKBT-G
           END-IF
      *
           IF    ( SPA-GMN-Q01-STTSYSKBT   =   "2" OR  "3" )
               MOVE    SPACE       TO  SPA-GMN-Q01-EDTSYYM
                                       SPA-NAI-Q01-EDTSYYM
           END-IF
      *
           .
       4117-STTSYSKBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低所得開始月チェック
      *****************************************************************
       4117-STTSYYM-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-STTSYYM
      *
      *    日付妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-STTSYYM NOT =   SPACE )
      *
               IF    ( SPA-GMN-Q01-STTSYSKBT   =   "1" OR "2" OR "3" )
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    SPA-GMN-Q01-STTSYYM     TO  SGDAY-INDAY
                   MOVE    "1"                     TO  SGDAY-INTYPE
                   CALL    "ORCSGDAY"         USING
                                              ORCSGDAYAREA
                   IF    ( SGDAY-RC           =   ZERO )
                       MOVE    SGDAY-OTDAY (1:6)
                                              TO  SPA-GMN-Q01-STTSYYM
                       MOVE    SGDAY-SDAY  (1:6) 
                                              TO  SPA-NAI-Q01-STTSYYM
                   ELSE
                       MOVE    SPA-GMN-Q01-STTSYYM
                                               TO  WRK-YMD
                       PERFORM 5002-HIZUKE-CHK-SEC
                       IF  ( SPA-ERRCD         =   SPACE )
                           MOVE    WRK-SYMD    TO  SPA-NAI-Q01-STTSYYM
                           MOVE    WRK-HENYMDG (1:6)
                                               TO  SPA-GMN-Q01-STTSYYM
                       ELSE
                           MOVE    "0125"              TO  SPA-ERRCD
                           MOVE    154             TO  SPA-GMN-Q01-CUR
                           GO  TO  4117-STTSYYM-CHK-EXT
                       END-IF
                   END-IF
               ELSE
                   MOVE    SPA-GMN-Q01-STTSYYM
                                               TO  WRK-YMD
                   PERFORM 5002-HIZUKE-CHK-SEC
                   IF  ( SPA-ERRCD         =   SPACE )
                       MOVE    WRK-SYMD    TO  SPA-NAI-Q01-STTSYYM
                       MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-STTSYYM
                   ELSE
                       MOVE    SPACE       TO  SPA-ERRCD
                       INITIALIZE                      ORCSGDAYAREA
                       MOVE    SPA-GMN-Q01-STTSYYM TO  SGDAY-INDAY
                       MOVE    "1"                 TO  SGDAY-INTYPE
                       CALL    "ORCSGDAY"         USING
                                                  ORCSGDAYAREA
                       IF    ( SGDAY-RC           =   ZERO )
                           MOVE    SGDAY-OTDAY TO  SPA-GMN-Q01-STTSYYM
                           MOVE    SGDAY-SDAY  TO  SPA-NAI-Q01-STTSYYM
                       ELSE
                           MOVE    "0125"              TO  SPA-ERRCD
                           MOVE    154             TO  SPA-GMN-Q01-CUR
                           GO  TO  4117-STTSYYM-CHK-EXT
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       4117-STTSYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低所得終了月チェック
      *****************************************************************
       4117-EDTSYYM-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-EDTSYYM
      *
      *    日付妥当性チェック＆編集処理
           IF  ( SPA-GMN-Q01-EDTSYYM NOT =   SPACE )
      *
               IF    ( SPA-GMN-Q01-STTSYSKBT   =   "1" OR "2" OR "3" )
                   INITIALIZE                  ORCSGDAYAREA
                   MOVE    SPA-GMN-Q01-EDTSYYM TO  SGDAY-INDAY
                   MOVE    "1"                 TO  SGDAY-INTYPE
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF    ( SGDAY-RC            =   ZERO )
                       MOVE    SGDAY-OTDAY (1:6)
                                               TO  SPA-GMN-Q01-EDTSYYM
                       MOVE    SGDAY-SDAY  (1:6)
                                               TO  SPA-NAI-Q01-EDTSYYM
                   ELSE
                       MOVE    SPA-GMN-Q01-EDTSYYM
                                               TO  WRK-YMD
                       PERFORM 5002-HIZUKE-CHK-SEC
                       IF  ( SPA-ERRCD         =   SPACE )
                           MOVE    WRK-SYMD    TO  SPA-NAI-Q01-EDTSYYM
                           MOVE    WRK-HENYMDG (1:6)
                                               TO  SPA-GMN-Q01-EDTSYYM
                       ELSE
                           MOVE    "0127"              TO  SPA-ERRCD
                           MOVE    155             TO  SPA-GMN-Q01-CUR
                           GO  TO  4117-EDTSYYM-CHK-EXT
                       END-IF
                   END-IF
               ELSE
                   MOVE    SPA-GMN-Q01-EDTSYYM
                                               TO  WRK-YMD
                   PERFORM 5002-HIZUKE-CHK-SEC
                   IF  ( SPA-ERRCD         =   SPACE )
                       MOVE    WRK-SYMD    TO  SPA-NAI-Q01-EDTSYYM
                       MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-EDTSYYM
                   ELSE
                       MOVE    SPACE       TO  SPA-ERRCD
                       INITIALIZE              ORCSGDAYAREA
                       MOVE    SPA-GMN-Q01-EDTSYYM
                                               TO  SGDAY-INDAY
                       MOVE    "1"             TO  SGDAY-INTYPE
                       CALL    "ORCSGDAY"      USING
                                               ORCSGDAYAREA
                       IF    ( SGDAY-RC        =   ZERO )
                           MOVE    SGDAY-SDAY  TO  WRK-YMD
                           PERFORM 800-GETUMATU-SEC
                           MOVE    WRK-SYMD    TO  WRK-YMD
                           PERFORM 5002-HIZUKE-CHK-SEC
                           MOVE    WRK-SYMD    TO  SPA-NAI-Q01-EDTSYYM
                           MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-EDTSYYM
                       ELSE
                           MOVE    "0127"      TO  SPA-ERRCD
                           MOVE    155         TO  SPA-GMN-Q01-CUR
                           GO  TO  4117-EDTSYYM-CHK-EXT
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       4117-EDTSYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特記事項コードチェック処理
      *****************************************************************
       4117-TOKKICD-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-Q01-TOKKICD =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE              ORCSNUMAREA
               MOVE    SPA-GMN-Q01-TOKKICD
                                   TO  SNUM-INX
               CALL    "ORCSNUM"   USING
                                   ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
      *
                   MOVE    SPA-GMN-Q01-TOKKICD
                                   TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-KANACHK-CHK-SEC
                   MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-TOKKICD
      *
                   MOVE    "0137"      TO  SPA-ERRCD
                   MOVE    160         TO  SPA-GMN-Q01-CUR
                ELSE
                   MOVE    SNUM-OUTNUM TO  WRK-TOKKICD9
                   MOVE    WRK-TOKKICD TO  SPA-GMN-Q01-TOKKICD
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-NAI-Q01-TOKKICD-BF
                                       =   SPACE )
                    OR   ( SPA-GMN-Q01-TOKKICD
                                   NOT =   SPA-NAI-Q01-TOKKICD-BF )
      *
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL ( IDX2
                                   >   SPA-GMN-Q01-TOKKIINFOLST-MAX )
                             OR ( SPA-GMN-Q01-TOKKICD
                                   =   SPA-GMN-Q01-TOKKIINFOL (IDX2))
      *
                           CONTINUE
      *
                       END-PERFORM
      *
                       IF    ( IDX2   <=   SPA-GMN-Q01-TOKKIINFOLST-MAX)
                           MOVE    SPA-GMN-Q01-TOKKIINFOMEIL (IDX2)
                                   TO  SPA-GMN-Q01-TOKKINAME
      *
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-Q01-TOKKICD TO  SPA-NAI-Q01-TOKKICD-BF
      *
           .
       4117-TOKKICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特記事項名称チェック処理
      *****************************************************************
       4117-TOKKINAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-Q01-TOKKINAME
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ZENKAKU-CHG-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-TOKKINAME
      *
           MOVE    SPACE           TO  WRK-TOKKICD
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   10 )
               IF    ( SPA-GMN-Q01-TOKKINAME (1:2)
                                   =   CONST-ZENNUM (IDX0))
                   MOVE    CONST-NUM (IDX0)
                                   TO  WRK-TOKKICD (1:1)
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   10 )
               IF    ( SPA-GMN-Q01-TOKKINAME (3:2)
                                   =   CONST-ZENNUM (IDX0))
                   MOVE    CONST-NUM (IDX0)
                                   TO  WRK-TOKKICD (2:1)
               END-IF
           END-PERFORM
      *
           EVALUATE    TRUE
           WHEN   ( WRK-TOKKICD    IS  NUMERIC )
               MOVE    WRK-TOKKICD TO  SPA-GMN-Q01-TOKKICD
               MOVE    SPA-GMN-Q01-TOKKICD
                                   TO  SPA-NAI-Q01-TOKKICD-BF
               MOVE    SPA-GMN-Q01-TOKKINAME (5:)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  SPA-GMN-Q01-TOKKINAME
           WHEN   ( WRK-TOKKICD (1:1)
                                   IS  NUMERIC )
               MOVE    "0"         TO  SPA-GMN-Q01-TOKKICD(1:1)
               MOVE    WRK-TOKKICD TO  SPA-GMN-Q01-TOKKICD(2:1)
               MOVE    SPA-GMN-Q01-TOKKICD
                                   TO  SPA-NAI-Q01-TOKKICD-BF
               MOVE    SPA-GMN-Q01-TOKKINAME (3:)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  SPA-GMN-Q01-TOKKINAME
           END-EVALUATE
      *
           .
       4117-TOKKINAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特記事項情報チェック処理
      *****************************************************************
       4117-TOKKIINFO-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-Q01-TOKKIINFO-G   =   SPACE )
               GO  TO  4117-TOKKIINFO-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-TOKKIINFOLST-MAX )
                      OR ( SPA-GMN-Q01-TOKKIINFO
                                   =   SPA-GMN-Q01-TOKKIINFOL (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-TOKKIINFOLST-MAX )
               MOVE    SPA-GMN-Q01-TOKKIINFOLST (IDX2)
                                       TO  SPA-GMN-Q01-TOKKIINFO-G
      *
               MOVE    SPA-GMN-Q01-TOKKIINFO
                                       TO  SPA-GMN-Q01-TOKKICD
                                           SPA-NAI-Q01-TOKKICD-BF
               MOVE    SPA-GMN-Q01-TOKKIINFOMEI
                                       TO  SPA-GMN-Q01-TOKKINAME
               MOVE    SPACE           TO  SPA-GMN-Q01-TOKKIINFO-G
      *
           ELSE
               MOVE    "0136"          TO  SPA-ERRCD
               MOVE    159             TO  SPA-GMN-Q01-CUR
               GO TO   4117-TOKKIINFO-CHK-EXT
           END-IF
      *
           .
       4117-TOKKIINFO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    災害該当情報チェック処理
      *****************************************************************
       4117-SAIGAIKBN-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-Q01-SAIGAIKBN-G   =   SPACE )
               GO  TO  4117-SAIGAIKBN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-SAIGAIKBNLST-MAX )
                      OR ( SPA-GMN-Q01-SAIGAIKBN
                                   =   SPA-GMN-Q01-SAIGAIKBNL (IDX2))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2            <=  SPA-GMN-Q01-SAIGAIKBNLST-MAX )
               MOVE    SPA-GMN-Q01-SAIGAIKBNLST (IDX2)
                                       TO  SPA-GMN-Q01-SAIGAIKBN-G
      *
           ELSE
               MOVE    "0143"          TO  SPA-ERRCD
               MOVE    163             TO  SPA-GMN-Q01-CUR
               GO TO   4117-SAIGAIKBN-CHK-EXT
           END-IF
      *
           .
       4117-SAIGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    本人家族チェック処理
      *****************************************************************
       4114-HONKZKKBN-CHK-SEC             SECTION.
      *
           COMPUTE WRK-ERR-CUR         =   201
      *
           IF    ( SPA-GMN-Q01-HONKZKKBN-G =   SPACE )
               GO  TO  4114-HONKZKKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-HONKZKKBNLST-MAX )
               IF    ( SPA-GMN-Q01-HONKZKKBN
                                   =   SPA-GMN-Q01-HONKZKKBNL (IDX2))
                   MOVE    SPA-GMN-Q01-HONKZKKBNLST (IDX2)
                                       TO  SPA-GMN-Q01-HONKZKKBN-G
                   MOVE    1           TO  FLG-CHK
                   MOVE    SPA-GMN-Q01-HONKZKKBNLST-MAX
                                       TO  IDX2
               END-IF
           END-PERFORM
      *
           IF    ( FLG-CHK         =   ZERO )
               MOVE    "0113"          TO  SPA-ERRCD
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO TO   4114-HONKZKKBN-CHK-EXT
           END-IF
      *
      *
           .
       4114-HONKZKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号チェック処理
      *****************************************************************
       4114-HKNJANUM-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-Q01-HKNJANUM  (IDXA)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-HKNJANUM  (IDXA)
      *
           .
       4114-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険の種類チェック処理
      *****************************************************************
       4114-HKNNUM-CHK-SEC             SECTION.
      *
           MOVE    IDXA                TO  IDX1
      *
           COMPUTE WRK-ERR-CUR         =   206 +   IDX1
      *
           IF    ( SPA-GMN-Q01-HKNNUM-G (IDX1) =   SPACE )
               GO  TO  4114-HKNNUM-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-HKNNUMLST-MAX )
               IF    ( SPA-GMN-Q01-HKNNUM (IDX1)
                                       =   SPA-GMN-Q01-HKNNUML (IDX2))
                   MOVE    SPA-GMN-Q01-HKNNUMLST (IDX2)
                                       TO  SPA-GMN-Q01-HKNNUM-G (IDX1)
                   MOVE    1           TO  FLG-CHK
                   MOVE    SPA-GMN-Q01-HKNNUMLST-MAX
                                       TO  IDX2
               END-IF
           END-PERFORM
      *
           IF    ( FLG-CHK         =   ZERO )
               MOVE    "0110"          TO  SPA-ERRCD
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO TO   4114-HKNNUM-CHK-EXT
           END-IF
      *
      *
           .
       4114-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    記号チェック処理
      *****************************************************************
       4114-KIGO-CHK-SEC        SECTION.
      *
           MOVE    IDXA            TO  IDX1
      *
      *    全角変換処理
           MOVE    SPA-GMN-Q01-KIGO(IDX1)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ZENKAKU-CHG-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-KIGO(IDX1)
      *
           .
       4114-KIGO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号チェック処理
      *****************************************************************
       4115-NUM-CHK-SEC        SECTION.
      *
           MOVE    IDXA            TO  IDX1
      *
      *    全角変換処理
           MOVE    SPA-GMN-Q01-NUM (IDX1)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ZENKAKU-CHG-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-NUM (IDX1)
      *
           .
       4115-NUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費保険番号チェック処理
      *****************************************************************
       4116-KOHNUM-CHK-SEC        SECTION.
      *
           MOVE    IDXA                TO  IDX1
      *
           COMPUTE WRK-ERR-CUR         =   221 +   IDX1
      *
           IF    ( SPA-GMN-Q01-KOHNUM-G (IDX1) =   SPACE )
               GO  TO  4116-KOHNUM-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-KOHNUMLST-MAX )
               IF    ( SPA-GMN-Q01-KOHNUM (IDX1)
                                       =   SPA-GMN-Q01-KOHNUML (IDX2))
                 AND ( SPA-GMN-Q01-PAYKBN (IDX1)
                                       =   SPA-GMN-Q01-PAYKBNL (IDX2))
                   MOVE    SPA-GMN-Q01-KOHNUMLST (IDX2)
                                       TO  SPA-GMN-Q01-KOHNUM-G (IDX1)
                   MOVE    1           TO  FLG-CHK
                   MOVE    SPA-GMN-Q01-KOHNUMLST-MAX
                                       TO  IDX2
               END-IF
           END-PERFORM
      *
           IF    ( FLG-CHK         =   ZERO )
               MOVE    "0111"          TO  SPA-ERRCD
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO TO   4116-KOHNUM-CHK-EXT
           END-IF
      *
           .
       4116-KOHNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担者番号チェック処理
      *****************************************************************
       4117-FTNJANUM-CHK-SEC        SECTION.
      *
      *    全角変換処理
           MOVE    SPA-GMN-Q01-FTNJANUM (IDXA)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-FTNJANUM (IDXA)
      *
           .
       4117-FTNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者照会検索条件チェック処理
      *****************************************************************
       4100-SELPATTERN-CHK-SEC         SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-Q01-CUR
      *
           MOVE    Q01-SELPATTERN      TO  SPA-GMN-Q01-SELPATTERN-G
      *
           INITIALIZE                  SPA-NAI-Q01-SELPATTERN-G
      *
           MOVE    SPA-GMN-Q01-SELPATTERN  TO  WRK-CMB-CD
           PERFORM 4200-SELPATTERNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPACE       TO  SPA-GMN-Q01-SELPATTERN-G
           ELSE
      *
               PERFORM 480-ALL-CLEAR-SEC
      *
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-Q01-SELPATTERN-G
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-Q01-SELPATTERN-UID
      *
               PERFORM 700-GMNHEN-SEC
      *
               EVALUATE    TRUE
               WHEN  ( FLG-PAGENO-00   =   1 )
                   MOVE    ZERO    TO  SPA-GMN-Q01-PAGENO
                   MOVE    101     TO  SPA-GMN-Q01-CUR
               WHEN  ( FLG-PAGENO-01   =   1 )
                   MOVE    1       TO  SPA-GMN-Q01-PAGENO
                   MOVE    201     TO  SPA-GMN-Q01-CUR
               WHEN  ( FLG-PAGENO-02   =   1 )
                   MOVE    2       TO  SPA-GMN-Q01-PAGENO
                   MOVE    301     TO  SPA-GMN-Q01-CUR
               WHEN    OTHER
                   MOVE    ZERO    TO  SPA-GMN-Q01-PAGENO
                   MOVE    101     TO  SPA-GMN-Q01-CUR
               END-EVALUATE
      *
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 4901-ALL-INPUT-CHK-SEC
               MOVE    SPACE       TO  SPA-ERRCD
           END-IF
      *
           .
       4100-SELPATTERN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名分類区分チェック処理
      *****************************************************************
       4118-BYOBUN-CHK-SEC             SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-Q01-CUR
      *
           MOVE    Q01-BYOBUN          TO  SPA-GMN-Q01-UBYOMEI-G
      *
           IF  ( SPA-GMN-Q01-UBYOMEI   =   SPACE )
               GO  TO  4118-BYOBUN-CHK-EXT
           END-IF
      *
           MOVE    "%"                 TO  SPA-GMN-Q01-BYOMEISEL
      *
           MOVE    ZERO                TO  SPA-GMN-Q01-BYOMEI-MAX
                                           SPA-GMN-Q01-BYOMEI-SEL
      *
           MOVE    1                   TO  SPA-GMN-Q01-BYOTBL-FLG
      *    病名編集処理(自院病名)
           PERFORM 4118-USERBYOMEI-LST-EDIT-SEC
      *
           MOVE    SPACE               TO  SPA-GMN-Q01-BYOMEISEL
      *
           IF      ( IDX1             >=   200 )
            AND    ( FLG-USERBYOMEI    =   ZERO )
               MOVE    "0003"          TO  SPA-ERRCD
           END-IF
           .
       4118-BYOBUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       4118-BYOMEISEL-CHK-SEC          SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-Q01-CUR
      *
           MOVE    Q01-BYOMEISEL       TO  SPA-GMN-Q01-BYOMEISEL
      *
           MOVE    SPA-GMN-Q01-BYOMEISEL   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT   TO  SPA-GMN-Q01-BYOMEISEL
      *
           IF  ( SPA-GMN-Q01-BYOMEISEL =   SPACE )
               GO  TO  4118-BYOMEISEL-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-Q01-BYOMEI-MAX
                                           SPA-GMN-Q01-BYOMEI-SEL
      *
           INITIALIZE                  SPA-GMN-Q01-BYOTBL
      *
           IF  ( SPA-GMN-Q01-RDBYOMEI-U    =   "T" )
               MOVE    1               TO  SPA-GMN-Q01-BYOTBL-FLG
      *
      *        病名編集処理(自院病名)
               PERFORM 4118-USERBYOMEI-LST-EDIT-SEC
      *
               IF      ( IDX1              >=  200 )
                AND    ( FLG-USERBYOMEI    =   ZERO )
                   MOVE    "0003"          TO  SPA-ERRCD
                   GO  TO  4118-BYOMEISEL-CHK-EXT
               END-IF
           ELSE
               MOVE    ZERO            TO  SPA-GMN-Q01-BYOTBL-FLG
      *
      *        病名編集処理(病名)
               PERFORM 4118-BYOMEI-LST-EDIT-SEC
      *
               IF      ( IDX1              >   200 )
                AND    ( FLG-BYOMEI        =   ZERO )
                   MOVE    "0003"          TO  SPA-ERRCD
                   GO  TO  4118-BYOMEISEL-CHK-EXT
               END-IF
           END-IF
      *
           .
       4118-BYOMEISEL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理(自院病名)
      *****************************************************************
       4118-USERBYOMEI-LST-EDIT-SEC        SECTION.
      *
           INITIALIZE                      USERBYOMEI-REC
           MOVE    SPA-HOSPNUM          TO  USBYO-HOSPNUM
      *
           MOVE    SPA-GMN-Q01-BYOMEISEL
                                       TO  WRK-SELWORD
           PERFORM 800-SELWORD-SEC
           MOVE    WRK-SELWORD         TO  USBYO-BYOMEI
      *
      *    病名で自院病名マスターを検索
           PERFORM 900-USERBYOMEI-KEY8-SEL-SEC
      *
           MOVE    ZERO            TO  IDX1
      *
           PERFORM UNTIL      (IDX1               >=   200 )
                    OR        (FLG-USERBYOMEI  NOT =   ZERO )
      *
      *        自院病名編集処理
               PERFORM 41181-USERBYOMEI-HENSYU-SEC
      *
      *        自院病名マスター読込
               PERFORM 900-USERBYOMEI-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       4118-USERBYOMEI-LST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    自院病名編集処理
      *****************************************************************
       41181-USERBYOMEI-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO            TO  WRK-IDX
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
               UNTIL     ( IDX2    >   SPA-GMN-Q01-UBYOMEILST-MAX )
      *
               IF  ( SPA-GMN-Q01-UBYOMEI
                                   =   SPA-GMN-Q01-UBYOMEIL (IDX2))
                   MOVE    SPA-GMN-Q01-UBYOMEILST (IDX2)
                                   TO  SPA-GMN-Q01-UBYOMEI-G
                   MOVE    IDX2        TO  WRK-IDX
                   MOVE    100         TO  IDX2
               END-IF
                   
           END-PERFORM
      *
           IF  ( WRK-IDX           =   ZERO )
               MOVE    "ALL"           TO  WRK-BYOBUNCD
           ELSE
               MOVE    SPA-GMN-Q01-UBYOMEIL (WRK-IDX)
                                       TO  WRK-BYOBUNCD
           END-IF
      *
           EVALUATE    TRUE
      *
           WHEN    ( USBYO-BUNRUICD    =   WRK-BYOBUNCD)
      *
           WHEN    ( WRK-BYOBUNCD      =   "NON" )
            AND    ( USBYO-BUNRUICD    =   SPACE )
      *
           WHEN    ( WRK-BYOBUNCD      =   "ALL" )
      *
               COMPUTE IDX1    =   IDX1    +   1
               MOVE    USBYO-BYOMEIINPUTCD
                                      TO  SPA-GMN-Q01-TBYOMEICD(IDX1)
               MOVE    USBYO-BYOMEI   TO  SPA-GMN-Q01-TBYOMEI  (IDX1)
               MOVE    IDX1           TO  SPA-GMN-Q01-BYOMEI-MAX
               MOVE    1              TO  SPA-GMN-Q01-BYOMEI-SEL
           END-EVALUATE
      *
           .
       41181-USERBYOMEI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    病名編集処理(病名)
      *****************************************************************
       4118-BYOMEI-LST-EDIT-SEC        SECTION.
      *
           INITIALIZE                      BYOMEI-REC
      *
           MOVE    SPA-GMN-Q01-BYOMEISEL
                                       TO  WRK-SELWORD
           PERFORM 800-SELWORD-SEC
           MOVE    WRK-SELWORD         TO  BYO-BYOMEI
                                           BYO-BYOMEIKANA
      *
      *    病名漢字（カナ）で病名マスターを検索
           PERFORM 900-BYOMEI-KEY2-SEL-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL      (IDX1            >   200 )   OR
                              (FLG-BYOMEI  NOT =   ZERO )
      *
      *        病名編集処理
               PERFORM 41181-BYOMEI-HENSYU-SEC
      *
      *        病名マスター読込
               PERFORM 900-BYOMEI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       4118-BYOMEI-LST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       41181-BYOMEI-HENSYU-SEC            SECTION.
      *
           MOVE    BYO-BYOMEICD        TO  SPA-GMN-Q01-TBYOMEICD(IDX1)
           MOVE    BYO-BYOMEI          TO  SPA-GMN-Q01-TBYOMEI  (IDX1)
           MOVE    IDX1                TO  SPA-GMN-Q01-BYOMEI-MAX
           MOVE    1                   TO  SPA-GMN-Q01-BYOMEI-SEL
      *
           .
       41181-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑い病名区分チェック処理
      *****************************************************************
       4119-UTAGAIKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-Q01-UTAGAIKBN-G =   SPACE )
               GO  TO  4119-UTAGAIKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-Q01-UTAGAIKBNLST-MAX )
      *
               IF    ( SPA-GMN-Q01-UTAGAIKBN
                                   =   SPA-GMN-Q01-UTAGAIKBNL (IDX0))
                   MOVE    SPA-GMN-Q01-UTAGAIKBNLST (IDX0)
                                       TO  SPA-GMN-Q01-UTAGAIKBN-G
                   MOVE    1           TO  FLG-CHK
               END-IF
      *
           END-PERFORM
      *
           IF  ( FLG-CHK           =   ZERO )
               MOVE    "0119"          TO  SPA-ERRCD
               MOVE    319             TO  SPA-GMN-Q01-CUR
           END-IF
      *
           .
       4119-UTAGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    急性病名区分チェック処理
      *****************************************************************
       4119-KYUSEIKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-Q01-KYUSEIKBN-G =   SPACE )
               GO  TO  4119-KYUSEIKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-Q01-KYUSEIKBNLST-MAX )
      *
               IF    ( SPA-GMN-Q01-KYUSEIKBN
                                   =   SPA-GMN-Q01-KYUSEIKBNL (IDX0))
                   MOVE    SPA-GMN-Q01-KYUSEIKBNLST (IDX0)
                                       TO  SPA-GMN-Q01-KYUSEIKBN-G
                   MOVE    1           TO  FLG-CHK
               END-IF
      *
           END-PERFORM
      *
           IF  ( FLG-CHK           =   ZERO )
               MOVE    "0134"          TO  SPA-ERRCD
               MOVE    325             TO  SPA-GMN-Q01-CUR
           END-IF
      *
           .
       4119-KYUSEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    転帰病名区分チェック処理
      *****************************************************************
       4119-TENKIKBN-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-Q01-TENKIKBN-G =   SPACE )
      *
               MOVE    SPACE       TO  SPA-GMN-Q01-STTENKIYMD
                                       SPA-NAI-Q01-STTENKIYMD
                                       SPA-GMN-Q01-EDTENKIYMD
                                       SPA-NAI-Q01-EDTENKIYMD
      *
               GO  TO  4119-TENKIKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-Q01-TENKIKBNLST-MAX )
      *
               IF    ( SPA-GMN-Q01-TENKIKBN
                                   =   SPA-GMN-Q01-TENKIKBNL (IDX0))
                   MOVE    SPA-GMN-Q01-TENKIKBNLST (IDX0)
                                       TO  SPA-GMN-Q01-TENKIKBN-G
                   MOVE    1           TO  FLG-CHK
               END-IF
      *
           END-PERFORM
      *
           IF  ( FLG-CHK           =   ZERO )
               MOVE    "0120"          TO  SPA-ERRCD
               MOVE    320             TO  SPA-GMN-Q01-CUR
           END-IF
      *
           IF  ( SPA-GMN-Q01-TENKIKBN  =   "1"
                                        OR "2"
                                        OR "3"
                                        OR "8"
                                        OR "9"
                                               )
               CONTINUE
           ELSE
      *
               MOVE    SPACE       TO  SPA-GMN-Q01-STTENKIYMD
                                       SPA-NAI-Q01-STTENKIYMD
                                       SPA-GMN-Q01-EDTENKIYMD
                                       SPA-NAI-Q01-EDTENKIYMD
      *
           END-IF
      *
           .
       4119-TENKIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始転帰日チェック
      *****************************************************************
       4119-STTENKIYMD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-STTENKIYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-Q01-STTENKIYMD  NOT =   SPACE )
               MOVE    SPA-GMN-Q01-STTENKIYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF  ( SPA-ERRCD         =   SPACE )
                   MOVE    WRK-SYMD    TO  SPA-NAI-Q01-STTENKIYMD
                   MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-STTENKIYMD
               ELSE
                   MOVE    "0121"          TO  SPA-ERRCD
                   MOVE    321             TO  SPA-GMN-Q01-CUR
                   GO  TO  4119-STTENKIYMD-CHK-EXT
               END-IF
           END-IF
      *
           .
       4119-STTENKIYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了転帰日チェック
      *****************************************************************
       4119-EDTENKIYMD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-EDTENKIYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-Q01-EDTENKIYMD  NOT =   SPACE )
               MOVE    SPA-GMN-Q01-EDTENKIYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF  ( SPA-ERRCD         =   SPACE )
                   MOVE    WRK-SYMD    TO  SPA-NAI-Q01-EDTENKIYMD
                   MOVE    WRK-HENYMDG TO  SPA-GMN-Q01-EDTENKIYMD
               ELSE
                   MOVE    "0122"          TO  SPA-ERRCD
                   MOVE    322             TO  SPA-GMN-Q01-CUR
                   GO  TO  4119-EDTENKIYMD-CHK-EXT
               END-IF
           END-IF
      *
           .
       4119-EDTENKIYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主病名区分チェック処理
      *****************************************************************
       4119-SYUBYOKBN-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-Q01-SYUBYOKBN-G =   SPACE )
               CONTINUE
           ELSE
               MOVE    ZERO            TO  FLG-CHK
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-Q01-SYUBYOKBNLST-MAX)
      *
                   IF    ( SPA-GMN-Q01-SYUBYOKBN
                                   =   SPA-GMN-Q01-SYUBYOKBNL (IDX0))
                       MOVE    SPA-GMN-Q01-SYUBYOKBNLST (IDX0)
                                       TO  SPA-GMN-Q01-SYUBYOKBN-G
                       MOVE    1       TO  FLG-CHK
                   END-IF
      *
               END-PERFORM
      *
               IF  ( FLG-CHK           =   ZERO )
                   MOVE    "0135"      TO  SPA-ERRCD
                   MOVE    326         TO  SPA-GMN-Q01-CUR
               END-IF
           END-IF
      *
           .
       4119-SYUBYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       4119-BYOMEICD-CHK-SEC        SECTION.
      *
           MOVE    IDXA                TO  IDX1
      *
           MOVE    SPA-GMN-Q01-BYOMEICD (IDX1)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-BYOMEICD (IDX1)
      *
           MOVE    WRK-MCP-WIDGET      TO  WRK-WIDGET
      *
           COMPUTE WRK-ERR-CUR         =   303
                                       + ( IDX1 - 1 ) * 2
      *
           IF    ( WRK-WIDGET (1 : 8)  =   "BYOMEICD" )
            AND  ( WRK-WIDGET8         =   IDX1 )
               CONTINUE
           ELSE
               GO  TO  4119-BYOMEICD-CHK-EXT
           END-IF
      *
           IF  ( SPA-GMN-Q01-BYOMEICD (IDX1)   =   SPACE )
               MOVE    SPACE           TO  SPA-GMN-Q01-BYOMEI  (IDX1)
               MOVE    SPACE           TO  SPA-NAI-Q01-BYOMEI-OLD 
                                                               (IDX1)
               GO  TO  4119-BYOMEICD-CHK-EXT
           END-IF
      *
      *
      *    自院病名マスタ−検索条件編集処理
           PERFORM 4121-USERBYOMEI-SEL-EDIT-SEC
           IF  ( SPA-ERRCD         NOT =   SPACE )
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO  TO  4119-BYOMEICD-CHK-EXT
           END-IF
      *
           IF  ( WRK-MOJI              =   ZERO )
               GO  TO  4119-BYOMEICD-CHK-EXT
           END-IF
      *
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-HOSPNUM          TO  USBYO-HOSPNUM
           MOVE    WRK-CDSYUBETU       TO  USBYO-CDSYU
           MOVE    SPA-GMN-Q01-BYOMEICD(IDX1)
                                       TO  USBYO-BYOMEIINPUTCD
      *
      *    自院病名マスタ−読み込み処理
           PERFORM 910-USERBYOMEI-KEY-SEL-SEC
      *
      *    自院病名マスタ−に存在せず、
      *    病名コードの桁数が７桁の場合、病名マスターを検索する
      *    病名マスター読み込み（病名コード存在チェック）
           IF  ( FLG-USERBYOMEI    =   ZERO )
               MOVE    USBYO-BYOMEI
                                   TO  SPA-GMN-Q01-BYOMEI (IDX1)
                                       SPA-NAI-Q01-BYOMEI-OLD
                                                          (IDX1)
           ELSE
               IF  ( WRK-MOJI      =   7 )
                   INITIALIZE              BYOMEI-REC
                   MOVE    SPA-GMN-Q01-BYOMEICD (IDX1)
                                   TO  BYO-BYOMEICD
                   PERFORM 900-BYOMEI-KEY-SEL-SEC
                   IF  ( FLG-BYOMEI    NOT =   ZERO )
                       MOVE    "0004"      TO  SPA-ERRCD
                       MOVE    WRK-ERR-CUR TO  SPA-GMN-Q01-CUR
                       GO  TO  4119-BYOMEICD-CHK-EXT
                   END-IF
      *
                   MOVE    BYO-BYOMEI
                                   TO  SPA-GMN-Q01-BYOMEI (IDX1)
                                       SPA-NAI-Q01-BYOMEI-OLD (IDX1)
               ELSE
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    WRK-ERR-CUR TO  SPA-GMN-Q01-CUR
                   GO  TO  4119-BYOMEICD-CHK-EXT
               END-IF
           END-IF
      *
           .
       4119-BYOMEICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスタ−検索条件編集処理
      *****************************************************************
       4121-USERBYOMEI-SEL-EDIT-SEC    SECTION.
      *
           MOVE    ZERO                TO  WRK-MOJI
      *
           MOVE    SPA-GMN-Q01-BYOMEICD (IDX1)
                                       TO  WRK-BYOMEICD
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL  (IDX2    >   20  )   OR
                          (WRK-BYOMEICD (IDX2:1)
                                   =   SPACE  )
      *
               COMPUTE WRK-MOJI    =   WRK-MOJI    +   1
      *
           END-PERFORM
      *
           IF  ( WRK-MOJI          =   ZERO )
               GO  TO  4121-USERBYOMEI-SEL-EDIT-EXT
           END-IF
      *
      *    全角半角混在チェック
           MOVE    WRK-BYOMEICD    TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-BYOMEICD
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    "0107"          TO  SPA-ERRCD
               GO  TO  4121-USERBYOMEI-SEL-EDIT-EXT
           END-IF
      *
           EVALUATE    KANACHK-SYUBETU
           WHEN    1
      *    半角入力
               MOVE    "A"             TO  WRK-CDSYUBETU
      *    全角入力
           WHEN    2
               MOVE    "J"             TO  WRK-CDSYUBETU
           WHEN    OTHER
               MOVE    "0107"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
      *
       4121-USERBYOMEI-SEL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       4119-BYOMEI-CHK-SEC             SECTION.
      *
           MOVE    IDXA                TO  IDX1
      *
           COMPUTE WRK-ERR-CUR         =   304
                                       + ( IDX1 - 1 ) * 2
      *
           MOVE    WRK-MCP-WIDGET      TO  WRK-WIDGET
      *
           IF    ( SPA-GMN-Q01-BYOMEI (IDX1)
                                       =   SPACE )
            OR   ( SPA-GMN-Q01-BYOMEI (IDX1)
                                       =   SPA-NAI-Q01-BYOMEI-OLD(IDX1))
      *
               IF    ( WRK-WIDGET (1 : 6)
                                               =   "BYOMEI" )
                AND  ( WRK-WIDGET (7 : 2)  NOT =   "CD" )
                AND  ( WRK-WIDGET6             =   IDX1 )
                   IF    ( SPA-GMN-Q01-BYOMEI (IDX1)   =   SPACE )
                       MOVE    SPACE   TO  SPA-GMN-Q01-BYOMEICD  (IDX1)
                                           SPA-GMN-Q01-BYOMEI    (IDX1)
                                           SPA-NAI-Q01-BYOMEI-OLD(IDX1)
                   END-IF
               END-IF
      *
               GO  TO  4119-BYOMEI-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-Q01-BYOMEICD (IDX1)
      *
      *    半角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
      *
           MOVE    ZERO                TO  CNT-PNT
           MOVE    SPACE               TO  WRK-KANACHK-MAE-INPUT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   CONST-BYOMEI-MAX )
                    OR   ( SPA-GMN-Q01-BYOMEI (IDX1) (IDX0 :  )
                                       =   SPACE )
      *
               IF    ( SPA-GMN-Q01-BYOMEI (IDX1) (IDX0 : 1 )
                                       =   "*" )
                   CONTINUE
               ELSE
                   COMPUTE CNT-PNT =   CNT-PNT +   1
                   MOVE    SPA-GMN-Q01-BYOMEI (IDX1) (IDX0 : 1 )
                                       TO  WRK-KANACHK-MAE-INPUT
                                                       (CNT-PNT : 1 )
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
            AND  ( KANACHK-SYUBETU     =   "2" )
               CONTINUE
           ELSE
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO  TO  4119-BYOMEI-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-Q01-BYOMEI (IDX1)
                                       TO  SPA-NAI-Q01-BYOMEI-OLD (IDX1)
      *
           .
       4119-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為名チェック処理
      *****************************************************************
       4120-SRYACTSEL-CHK-SEC        SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    313                 TO  SPA-GMN-Q01-CUR
      *
           MOVE    Q01-SRYACTSEL       TO  SPA-GMN-Q01-SRYACTSEL
      *
           MOVE    SPA-GMN-Q01-SRYACTSEL   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT   TO  SPA-GMN-Q01-SRYACTSEL
      *
           IF  ( SPA-GMN-Q01-SRYACTSEL =   SPACE )
               COMPUTE SPA-GMN-Q01-CUR =   SPA-GMN-Q01-CUR
                                       +   1
               GO  TO  4120-SRYACTSEL-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-Q01-SRYACT-MAX
                                           SPA-GMN-Q01-SRYACT-SEL
      *
           INITIALIZE                  SPA-GMN-Q01-SRYACTTBL
      *
           MOVE    ZERO                TO  IDX1
      *
      *    点数マスタ検索（診療行為名存在チェック）
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-GMN-Q01-SRYACTSEL
                                       TO  WRK-SELWORD
           PERFORM 800-SELWORD-SEC
           MOVE    WRK-SELWORD         TO  TNS-NAME
                                           TNS-KANANAME
      *
           PERFORM 900-TENSU-KEY72-SEL-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL      (IDX1            >   200  )   OR
                              (FLG-TENSU   NOT =   ZERO )
      *
      *        診療行為編集処理
               PERFORM 41201-SRYACT-HENSYU-SEC
      *
      *        点数マスタ読込
               PERFORM 900-TENSU-KEY72-FET-SEC
      *
           END-PERFORM
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key72"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF      ( IDX1              >   200 )
            AND    ( FLG-TENSU         =   ZERO )
               MOVE    "0003"              TO  SPA-ERRCD
               GO  TO  4120-SRYACTSEL-CHK-EXT
           END-IF
      *
           COMPUTE SPA-GMN-Q01-CUR     =   SPA-GMN-Q01-CUR
                                       +   1
      *
           .
       4120-SRYACTSEL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       41201-SRYACT-HENSYU-SEC            SECTION.
      * 
           MOVE    TNS-SRYCD           TO  SPA-GMN-Q01-TSRYCD(IDX1)
      * 
           IF     (TNS-ROUTEKKBN       =   2   )
               STRING  "［老］"
                    TNS-NAME               DELIMITED   BY  SIZE
               INTO                        SPA-GMN-Q01-TSRYACT(IDX1)
               END-STRING
           ELSE
               MOVE    TNS-NAME        TO  SPA-GMN-Q01-TSRYACT(IDX1)
           END-IF
      *
           IF    ( TNS-SRYCD(1:1)      =   "1") 
            AND  ( TNS-SRYKBN          =   "60")
            AND  ( TNS-FORMALNAME  NOT =   SPACE )
            AND  ( TNS-FORMALNAME  NOT =   TNS-NAME )
                MOVE    SPACE              TO  SPA-GMN-Q01-TSRYACT(IDX1)
                STRING  TNS-NAME           DELIMITED   BY  SPACE
                        "　【"             DELIMITED   BY  SIZE
                        TNS-FORMALNAME     DELIMITED   BY  SPACE
                        "】"               DELIMITED   BY  SIZE
                               INTO        SPA-GMN-Q01-TSRYACT(IDX1)
                END-STRING
                PERFORM 310211-KENSA-NAME-SEC
           END-IF
      
      *
           MOVE    IDX1                TO  SPA-GMN-Q01-SRYACT-MAX
           MOVE    1                   TO  SPA-GMN-Q01-SRYACT-SEL
      *
           .
      * 
       41201-SRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査名称再編集処理
      *****************************************************************
       310211-KENSA-NAME-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-NAME1-G
           MOVE    TNS-NAME            TO  WRK-NAME1-G
           IF      WRK-NAME12          =   SPACE
               STRING  "【"                DELIMITED   BY  SIZE
                        TNS-FORMALNAME     DELIMITED   BY  SPACE
                        "】"               DELIMITED   BY  SIZE
                               INTO        WRK-NAME12
               END-STRING
               INSPECT WRK-NAME1-G         REPLACING  ALL "  "  BY  "　"
               MOVE    WRK-NAME1-G         TO  SPA-GMN-Q01-TSRYACT(IDX1)
           ELSE
               MOVE    SPACE               TO  WRK-NAME2-G
               MOVE    TNS-NAME            TO  WRK-NAME2-G
               IF      WRK-NAME22          =   SPACE
                   STRING  "【"                DELIMITED   BY  SIZE
                            TNS-FORMALNAME     DELIMITED   BY  SPACE
                            "】"               DELIMITED   BY  SIZE
                                   INTO        WRK-NAME22
                   END-STRING
                   INSPECT WRK-NAME2-G     REPLACING  ALL "  "  BY  "　"
                   MOVE    WRK-NAME2-G     TO  SPA-GMN-Q01-TSRYACT(IDX1)
               END-IF
           END-IF
      *
           .
       310211-KENSA-NAME-EXT.
           EXIT.
      *****************************************************************
      *    診療区分チェック処理
      *****************************************************************
       4121-SRYKBN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-Q01-SRYKBN-G    =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-Q01-SRYKBN  TO  WRK-CMB-CD
               PERFORM 4200-SRYKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0129"          TO  SPA-ERRCD
                   MOVE    323             TO  SPA-GMN-Q01-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-Q01-SRYKBN-G
                   IF    ( SPA-GMN-Q01-SRYKBN  =   "20" OR "30" OR "90")
                    AND  ( SPA-GMN-Q01-SRYKBN (1:1)
                                       =   SPA-GMN-Q01-SRYSYUKBN (1:1))
                       CONTINUE
                   ELSE
                       INITIALIZE      SPA-GMN-Q01-SRYSYUKBN-G
                   END-IF
                   PERFORM 3101-SRYSYUKBN-CMB-SYOKI-SEC
               END-IF
           END-IF
      *
           .
       4121-SRYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療種別チェック処理
      *****************************************************************
       4121-SRYSYUKBN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-Q01-SRYSYUKBN-G    =    SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-Q01-SRYSYUKBN  TO  WRK-CMB-CD
               PERFORM 4200-SRYSYUKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0130"          TO  SPA-ERRCD
                   MOVE    324             TO  SPA-GMN-Q01-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-Q01-SRYSYUKBN-G
               END-IF
           END-IF
      *
           .
       4121-SRYSYUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードチェック処理
      *****************************************************************
       4121-SRYCD-CHK-SEC        SECTION.
      *
           MOVE    IDXA                TO  IDX1
      *
           MOVE    SPA-GMN-Q01-SRYCD (IDX1)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-SRYCD (IDX1)
      *
           COMPUTE WRK-ERR-CUR         =   313
                                       +   IDX1
      *
           MOVE    SPACE               TO  SPA-NAI-Q01-SRYCD (IDX1)
           MOVE    SPACE               TO  SPA-GMN-Q01-SRYACT (IDX1)
      *
      *    診療行為検索画面へ遷移
           IF  ( SPA-GMN-Q01-SRYCD (IDX1) (1:1)   =   "/" )
               MOVE    IDX1            TO  IDXZ
               MOVE    ZERO            TO  FLG-KANJI
               INITIALIZE              WRK-INPUTCD
                                       WRK-CD-MCNT
               PERFORM 409-KENSAKU-SEC
               GO  TO  4121-SRYCD-CHK-EXT
           END-IF
      *
      *    セットコードでの入力はエラーとする。
           IF  ( SPA-GMN-Q01-SRYCD (IDX1) (1:1)   =   "P"  OR  "S" )
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               MOVE    "0007"          TO  SPA-ERRCD
               GO  TO  4121-SRYCD-CHK-EXT
           END-IF
      *
      *    入力コードマスタ−検索条件編集処理
           PERFORM 4121-INPUTCD-SELECT-EDIT-SEC
           IF  ( SPA-ERRCD         NOT =   SPACE )
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO  TO  4121-SRYCD-CHK-EXT
           END-IF
      *
           IF  ( WRK-MOJI              =   ZERO )
               GO  TO  4121-SRYCD-CHK-EXT
           END-IF
      *
           INITIALIZE              INPUTCD-REC
           MOVE    SPA-HOSPNUM      TO  ICD-HOSPNUM
           MOVE    WRK-CDSYUBETU   TO  ICD-CDSYU
           MOVE    WRK-SRYCD       TO  ICD-INPUTCD
      *
      *    入力コードマスタ−読み込み処理
           PERFORM 900-INPUTCD-KEY-SEL-SEC
      *
      *    入力コードマスターに存在せず、
      *    入力コードの桁数が９桁の場合、点数マスターを検索する
           IF  ( FLG-INPUTCD       =   ZERO )
               INITIALIZE      TNS-TENSU-REC
               MOVE   ICD-SRYCD    TO  TNS-SRYCD
      *
               MOVE   SPA-SYSYMD   TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
      *        点数マスタ−読み込み処理
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF  ( FLG-TENSU     =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    WRK-ERR-CUR TO  SPA-GMN-Q01-CUR
                   MOVE    "0005"      TO  SPA-ERRCD
                   GO  TO  4121-SRYCD-CHK-EXT
               END-IF
           ELSE
               IF    ( WRK-MOJI              =   9 )
                AND  ( FLG-KANJI             =   ZERO )
      *
                   INITIALIZE      TNS-TENSU-REC
                   MOVE   WRK-SRYCD    TO  TNS-SRYCD
      *
                   MOVE   SPA-SYSYMD   TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
      *            点数マスタ−読み込み処理
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF  ( FLG-TENSU     =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    WRK-ERR-CUR TO  SPA-GMN-Q01-CUR
                       MOVE    "0005"      TO  SPA-ERRCD
                       GO  TO  4121-SRYCD-CHK-EXT
                   END-IF
               ELSE
                   MOVE    IDX1            TO  IDXZ
                   MOVE    WRK-SRYCD       TO  WRK-INPUTCD
                   PERFORM 409-KENSAKU-SEC
                   GO  TO  4121-SRYCD-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    TNS-SRYCD           TO  SPA-NAI-Q01-SRYCD  (IDX1)
      *
           IF     (TNS-ROUTEKKBN       =   2   )
               MOVE    SPACE           TO  SPA-GMN-Q01-SRYACT (IDX1)
               STRING  "［老］"
                    TNS-NAME               DELIMITED   BY  SIZE
               INTO                        SPA-GMN-Q01-SRYACT (IDX1)
               END-STRING
           ELSE
               MOVE    TNS-NAME        TO  SPA-GMN-Q01-SRYACT (IDX1)
           END-IF
      *
           .
       4121-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスタ−検索条件編集処理
      *****************************************************************
       4121-INPUTCD-SELECT-EDIT-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  WRK-MOJI
      *
           MOVE    SPA-GMN-Q01-SRYCD (IDX1)
                                       TO  WRK-SRYCD
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL  (IDX2    >   20  )   OR
                          (WRK-SRYCD (IDX2:1)
                                   =   SPACE  )
      *
               COMPUTE WRK-MOJI    =   WRK-MOJI    +   1
      *
           END-PERFORM
      *
           IF  ( WRK-MOJI          =   ZERO )
               GO  TO  4121-INPUTCD-SELECT-EDIT-EXT
           END-IF
      *
      *    全角半角混在チェック
           MOVE    WRK-SRYCD       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-SRYCD
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    "0108"          TO  SPA-ERRCD
               GO  TO  4121-INPUTCD-SELECT-EDIT-EXT
           END-IF
      *
           EVALUATE    KANACHK-SYUBETU
           WHEN    1
      *        半角入力
               MOVE    ZERO            TO  FLG-KANJI
      *
               IF  ( WRK-SRYCD (1:WRK-MOJI) IS  NUMERIC )
                   EVALUATE    WRK-MOJI
                   WHEN    4
                       MOVE    "4"     TO  WRK-CDSYUBETU
                   WHEN    5
                       MOVE    "5"     TO  WRK-CDSYUBETU
                   WHEN    6
                   WHEN    3
                        MOVE   "6"     TO  WRK-CDSYUBETU
                   WHEN    OTHER
                       MOVE    "6"     TO  WRK-CDSYUBETU
                   END-EVALUATE
               ELSE
                   MOVE    "A"         TO  WRK-CDSYUBETU
               END-IF
      *    全角入力
           WHEN    2
               MOVE    1               TO  FLG-KANJI
               MOVE    "J"             TO  WRK-CDSYUBETU
           WHEN    OTHER
               MOVE    "0108"          TO  SPA-ERRCD
           END-EVALUATE
      *
      *
           .
      *
       4121-INPUTCD-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レセプトコメントチェック処理
      *****************************************************************
       4121-RECECOM-CHK-SEC            SECTION.
      *
           COMPUTE WRK-ERR-CUR         =   326     +   IDXA
      *
           MOVE    SPA-GMN-Q01-RECECOM (IDXA)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q01-RECECOM (IDXA)
      *
           MOVE    SPACE       TO  WRK-RECECOM-G
           UNSTRING    SPA-GMN-Q01-COMMENT (IDXA)
                                   DELIMITED BY  ALL "　"
           INTO        WRK-RECECOM (1)
                       WRK-RECECOM (2)
                       WRK-RECECOM (3)
                       WRK-RECECOM (4)
                       WRK-RECECOM (5)
           END-UNSTRING
      *
      *    レセコメントのキーワードは３語まで指定可能とする。
           IF    (     WRK-RECECOM (1) NOT =   SPACE )
               IF    ( WRK-RECECOM (4) NOT =   SPACE )
                   MOVE    "0132"      TO  SPA-ERRCD
                   MOVE    WRK-ERR-CUR TO  SPA-GMN-Q01-CUR
               END-IF
           ELSE
               IF    ( WRK-RECECOM (5) NOT =   SPACE )
                   MOVE    "0132"      TO  SPA-ERRCD
                   MOVE    WRK-ERR-CUR TO  SPA-GMN-Q01-CUR
               END-IF
           END-IF
      *
           .
      *
       4121-RECECOM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順項目名チェック処理
      *****************************************************************
       4122-KMK-CHK-SEC                SECTION.
      *
           MOVE    IDXA                TO  IDX1
      *
           COMPUTE WRK-ERR-CUR     =   400
                                   +   IDX1
      *
      *    必ず指定されていること。
           IF  ( SPA-GMN-Q01-KMK-G (IDX1)  NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               MOVE    "0008"  TO  SPA-ERRCD
               GO  TO  4122-KMK-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-Q01-SORTLST-MAX )
               IF    ( SPA-GMN-Q01-KMK (IDX1)
                                       =   SPA-GMN-Q01-SORTL (IDX2))
                   MOVE    SPA-GMN-Q01-SORTLST (IDX2)
                                       TO  SPA-GMN-Q01-KMK-G (IDX1)
                   MOVE    1           TO  FLG-CHK
                   MOVE    SPA-GMN-Q01-SORTLST-MAX
                                       TO  IDX2
               END-IF
           END-PERFORM
      *
           IF    ( FLG-CHK         =   ZERO )
               MOVE    "0112"          TO  SPA-ERRCD
               MOVE    WRK-ERR-CUR     TO  SPA-GMN-Q01-CUR
               GO TO   4122-KMK-CHK-EXT
           END-IF
      *
           .
       4122-KMK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月日設定処理
      *****************************************************************
       4100-SRYYMD-SET-SEC             SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPA-SYSYMD          TO  SPA-NAI-Q01-STSRYYMD
                                           SPA-NAI-Q01-EDSRYYMD
      *
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-Q01-STSRYYMD
                                           SPA-GMN-Q01-EDSRYYMD
      *
           MOVE    1                   TO  SPA-GMN-Q01-MMORDD
      *
           MOVE    1                   TO  SPA-GMN-Q01-CUR
      *
           .
       4100-SRYYMD-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療年月設定処理
      *****************************************************************
       4100-SRYYM-SET-SEC              SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPA-SYSYMD          TO  WRK-STSRYYMD
           MOVE    "01"                TO  WRK-STSRYYMD(7:2)
           PERFORM 41001-SRYYM-SET-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-Q01-MMORDD
      *
           MOVE    1                   TO  SPA-GMN-Q01-CUR
      *
           .
       4100-SRYYM-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療年月設定処理
      *****************************************************************
       41001-SRYYM-SET-SEC             SECTION.
      *
           MOVE    WRK-STSRYYMD        TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-SYMD            TO  SPA-NAI-Q01-STSRYYMD
           MOVE    WRK-HENYMDG         TO  SPA-GMN-Q01-STSRYYMD
      *
           MOVE    WRK-STSRYYMD        TO  WRK-YMD
           PERFORM 800-GETUMATU-SEC
           MOVE    WRK-SYMD            TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-SYMD            TO  SPA-NAI-Q01-EDSRYYMD
           MOVE    WRK-HENYMDG         TO  SPA-GMN-Q01-EDSRYYMD
      *
           .
       41001-SRYYM-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療日変更処理
      *****************************************************************
       4100-CHGSRYYMD-SEC              SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4100-STSRYYMD-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-Q01-STSRYYMD    =   SPACE )
                   MOVE    SPA-SYSYMD      TO  WRK-YMD
                   MOVE    "01"            TO  WRK-YMD(7:2)
               ELSE
                   MOVE    SPA-NAI-Q01-STSRYYMD
                                           TO  WRK-YMD
               END-IF
               IF    ( FLG-CHGSRYYMD       =   ZERO )
                   MOVE    -1              TO  WRK-ZOGEN
               ELSE
                   MOVE    1               TO  WRK-ZOGEN
               END-IF
               IF    ( SPA-GMN-Q01-MMORDD  =   1 )
                   MOVE    "1"             TO  WRK-ZOGENPTN
               ELSE
                   MOVE    "2"             TO  WRK-ZOGENPTN
               END-IF
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD       TO  WRK-STSRYYMD
               PERFORM 41001-SRYYM-SET-SEC
               IF    ( SPA-GMN-Q01-MMORDD  =   1 )
                   MOVE    SPA-NAI-Q01-STSRYYMD
                                           TO  SPA-NAI-Q01-EDSRYYMD
                   MOVE    SPA-GMN-Q01-STSRYYMD
                                           TO  SPA-GMN-Q01-EDSRYYMD
               END-IF
      *
               MOVE    1                   TO  SPA-GMN-Q01-CUR
      *
           END-IF
      *
           .
       4100-CHGSRYYMD-EXT.
           EXIT.
      *****************************************************************
      *    次月(次日)処理
      *****************************************************************
       4100-GOTONEXT-SEC              SECTION.
      *
      *
           .
       4100-GOTONEXT-EXT.
           EXIT.
      *****************************************************************
      *    検索条件コンボボックスチェック処理
      *****************************************************************
       4200-SELPATTERNLST-CHK-SEC        SECTION.
      *
           INITIALIZE                    WRK-CMB-ITM-G.
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-Q01-SELPATTERNLST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( WRK-CMB-CD  =   SPA-GMN-Q01-SELPATTERNL(IDX-CMB))
                   MOVE    SPA-GMN-Q01-SELPATTERNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-Q01-SELPATTERN-UIDL(IDX-CMB)
                                       TO  WRK-CMB-ITM2
               END-IF
           END-PERFORM
      *
           .
       4200-SELPATTERNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索条件コンボボックスチェック処理
      *****************************************************************
       4201-SELPATTERNLST-CHK-SEC        SECTION.
      *
           INITIALIZE                    WRK-CMB-ITM-G.
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-Q01-SELPATTERNLST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( WRK-CMB-CD  =   SPA-NAI-Q01-SELPATTERN-UIDL
                                                              (IDX-CMB))
                   MOVE    SPA-GMN-Q01-SELPATTERNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-Q01-SELPATTERN-UIDL(IDX-CMB)
                                       TO  WRK-CMB-ITM2
               END-IF
           END-PERFORM
      *
           .
       4201-SELPATTERNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療区分コンボボックスチェック処理
      *****************************************************************
       4200-SRYKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                    WRK-CMB-ITM-G.
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-Q01-SRYKBNLST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( WRK-CMB-CD  =   SPA-GMN-Q01-SRYKBNL (IDX-CMB))
                   MOVE    SPA-GMN-Q01-SRYKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
       4200-SRYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療種別区分コンボボックスチェック処理
      *****************************************************************
       4200-SRYSYUKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                    WRK-CMB-ITM-G.
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-Q01-SRYSYUKBNLST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( WRK-CMB-CD  =   SPA-GMN-Q01-SRYSYUKBNL (IDX-CMB))
                   MOVE    SPA-GMN-Q01-SRYSYUKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
       4200-SRYSYUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    テスト患者チェックボタン編集処理
      *****************************************************************
       410-CHKTSTPT-SET-SEC              SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    Q01-CHKTSTPT    TO  SPA-GMN-Q01-CHKTSTPT
      *
           .
      *
       410-CHKTSTPT-SET-EXT.
           EXIT.
      *****************************************************************
      *    住所編集処理
      *****************************************************************
       410-JYUSHO-SET-SEC              SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    WRK-WIDGET8     TO  IDX1
      *
      *    周辺住所検索画面に遷移する
           MOVE    IDX1            TO  WRK-JYUSHO-IDX
           PERFORM 410-Q98-SENI-SEC
      *
           .
       410-JYUSHO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    周辺住所検索画面遷移処理
      *****************************************************************
       410-Q98-SENI-SEC                SECTION.
      *
           PERFORM 5011-INPUT-CUR-SEC
      *
      *    周辺住所検索画面に遷移する
           MOVE    WRK-JYUSHO-IDX  TO  SPA-Q01-Q98-JYUSHO-IDX
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGQ98"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
           MOVE  "SELNUM"          TO  MCP-WIDGET
      *
           MOVE  "Q01"             TO  SPA-MOTOPG
           MOVE  "Q98"             TO  SPA-SAKIPG
      *
           MOVE  "NEW"             TO  MCP-PUTTYPE
           MOVE  "Q98"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1                 TO  FLG-END
      *
           .
       410-Q98-SENI-EXT.
           EXIT.
      *****************************************************************
      *    病名コード編集処理
      *****************************************************************
       420-BYOMEICD-SET-SEC            SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    WRK-WIDGET5     TO  IDX1
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >       200 )
               IF  ( Q01-BYOLST-SELECT (IDX2)  =   "T" )
      *
                   MOVE    SPA-GMN-Q01-TBYOMEICD (IDX2)
                                   TO  SPA-GMN-Q01-BYOMEICD (IDX1)
                   MOVE    SPA-GMN-Q01-TBYOMEI (IDX2)
                                   TO  SPA-GMN-Q01-BYOMEI (IDX1)
                                       SPA-NAI-Q01-BYOMEI-OLD (IDX1)
                   MOVE    200     TO  IDX2
               END-IF
           END-PERFORM
      *     
           .
       420-BYOMEICD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コード編集処理
      *****************************************************************
       430-SRYCD-SET-SEC            SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    WRK-WIDGET5     TO  IDX1
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >       200 )
               IF  ( Q01-SRYLST-SELECT (IDX2)  =   "T" )
                   MOVE    SPA-GMN-Q01-TSRYCD (IDX2)
                                           TO  SPA-GMN-Q01-SRYCD (IDX1)
                                               SPA-NAI-Q01-SRYCD (IDX1)
                   INITIALIZE          TNS-TENSU-REC
                   MOVE   SPA-NAI-Q01-SRYCD (IDX1)
                                   TO  TNS-SRYCD
      *
                   MOVE   SPA-SYSYMD
                                   TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
      *            点数マスタ−読み込み処理
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF     (TNS-ROUTEKKBN       =   2   )
                       MOVE    SPACE       TO  SPA-GMN-Q01-SRYACT (IDX1)
                       STRING  "［老］"    DELIMITED   BY  SIZE
                               TNS-NAME    DELIMITED   BY  SPACE
                               INTO        SPA-GMN-Q01-SRYACT (IDX1)
                       END-STRING
                   ELSE
                       MOVE    TNS-NAME    TO  SPA-GMN-Q01-SRYACT(IDX1)
                   END-IF
                   MOVE    200             TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       430-SRYCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    タブ編集処理
      *****************************************************************
       440-TAB-CHANGE-SEC              SECTION.
      *
           MOVE    WRK-WIDGET2         TO  WRK-PAGENO
      *
           COMPUTE WRK-PAGENO  =   WRK-PAGENO  -   1
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           EVALUATE    SPA-GMN-Q01-PAGENO
           WHEN    0
               PERFORM 4101-KHN-INPUT-CHK-SEC
           WHEN    1
               PERFORM 4101-HKN-INPUT-CHK-SEC
           WHEN    2
               PERFORM 4101-BYOSRY-INPUT-CHK-SEC
           WHEN    3
               PERFORM 4101-SRT-INPUT-CHK-SEC
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  440-TAB-CHANGE-EXT
           END-IF
      *
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  440-TAB-CHANGE-EXT
           END-IF
      *
           MOVE    WRK-PAGENO          TO  SPA-GMN-Q01-PAGENO
      *
           EVALUATE    WRK-PAGENO
           WHEN    0
                   MOVE    101         TO  SPA-GMN-Q01-CUR
           WHEN    1
                   MOVE    201         TO  SPA-GMN-Q01-CUR
           WHEN    2
                   MOVE    301         TO  SPA-GMN-Q01-CUR
           WHEN    3
                   MOVE    401         TO  SPA-GMN-Q01-CUR
           END-EVALUATE
      *
           .
       440-TAB-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    並び順ラジオボタン設定処理
      *****************************************************************
       450-RDIO-SET-SEC                SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "RDIO11"
               MOVE    "T"         TO  SPA-GMN-Q01-RDIO1 (1)
               MOVE    "F"         TO  SPA-GMN-Q01-RDIO2 (1)
           WHEN    "RDIO12"
               MOVE    "F"         TO  SPA-GMN-Q01-RDIO1 (1)
               MOVE    "T"         TO  SPA-GMN-Q01-RDIO2 (1)
           WHEN    "RDIO21"
               MOVE    "T"         TO  SPA-GMN-Q01-RDIO1 (2)
               MOVE    "F"         TO  SPA-GMN-Q01-RDIO2 (2)
           WHEN    "RDIO22"
               MOVE    "F"         TO  SPA-GMN-Q01-RDIO1 (2)
               MOVE    "T"         TO  SPA-GMN-Q01-RDIO2 (2)
           WHEN    "RDIO31"
               MOVE    "T"         TO  SPA-GMN-Q01-RDIO1 (3)
               MOVE    "F"         TO  SPA-GMN-Q01-RDIO2 (3)
           WHEN    "RDIO32"
               MOVE    "F"         TO  SPA-GMN-Q01-RDIO1 (3)
               MOVE    "T"         TO  SPA-GMN-Q01-RDIO2 (3)
           END-EVALUATE
      *
           .
       450-RDIO-SET-EXT.
           EXIT.
      *****************************************************************
      *    検索条件保存内容削除処理
      *****************************************************************
       450-SELPATTERN-DEL-SEC          SECTION.
      *
           MOVE    "3002"          TO  SPA-QIDCD
      *
           .
       450-SELPATTERN-DEL-EXT.
           EXIT.
      *****************************************************************
      *    検索条件保存内容削除処理
      *****************************************************************
       4501-SELPATTERN-DEL-SEC         SECTION.
      *
           PERFORM 45011-SHOUKAIDATA-DEL-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 45011-SHOUKAIKANRI-DEL-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM     3101-SELPATTERN-CMB-SYOKI-SEC
               INITIALIZE              SPA-GMN-Q01-SELPATTERN-G
                                       SPA-NAI-Q01-SELPATTERN-G
           END-IF
      *
           .
       4501-SELPATTERN-DEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件データ情報削除処理
      *****************************************************************
       45011-SHOUKAIDATA-DEL-SEC       SECTION.
      *
           INITIALIZE                  SHOUKAIDATA-REC
           MOVE    SPA-HOSPNUM         TO  SHODATA-HOSPNUM
           MOVE    SPA-NAI-Q01-SELPATTERN-UID
                                       TO  SHODATA-UID
           MOVE    SHOUKAIDATA-REC     TO  MCPDATA-REC
           MOVE    "tbl_shoukaidata"   TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4003"          TO  SPA-ERRCD
           END-IF
      *
           .
       45011-SHOUKAIDATA-DEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件データ情報削除処理
      *****************************************************************
       45011-SHOUKAIKANRI-DEL-SEC      SECTION.
      *
           INITIALIZE                  SHOUKAIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    SPA-NAI-Q01-SELPATTERN-UID
                                       TO  SHOKANRI-UID
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4004"          TO  SPA-ERRCD
           END-IF
      *
           .
       45011-SHOUKAIKANRI-DEL-EXT.
           EXIT.
      *****************************************************************
      *    検索条件保存内容削除処理
      *****************************************************************
       4601-SELPATTERN-UPD-SEC         SECTION.
      *
           INITIALIZE                      SHOUKAIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    SPA-NAI-Q01-SELPATTERN-UID
                                       TO  SHOKANRI-UID
           MOVE    WRK-SELPATTERNMEI   TO  SHOKANRI-NAME
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4005"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 45011-SHOUKAIDATA-DEL-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    SPA-NAI-Q01-SELPATTERN-UID
                                   TO  WRK-UID
               PERFORM 7001-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               PERFORM 3101-SELPATTERN-CMB-SYOKI-SEC
      *
               MOVE    SPA-NAI-Q01-SELPATTERN-UID
                                   TO  WRK-CMB-CD
               PERFORM 4201-SELPATTERNLST-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-Q01-SELPATTERN-G
               MOVE    WRK-CMB-ITM2 TO SPA-NAI-Q01-SELPATTERN-UID
           END-IF
      *
           .
       4601-SELPATTERN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    検索条件保存画面遷移
      *****************************************************************
       470-Q04-SENI-SEC                SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4901-ALL-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               INITIALIZE                  SPA-Q01-Q04-SENI-AREA
               MOVE    WRK-UPDKBN      TO  SPA-Q01-Q04-UPDKBN
      *
               IF    ( WRK-UPDKBN      =   "UPD" )
                   MOVE    SPA-GMN-Q01-SELPATTERNMEI
                                       TO  SPA-Q01-Q04-NAME
                   MOVE    SPA-NAI-Q01-SELPATTERN-UID
                                       TO  SPA-Q01-Q04-UID
               END-IF
      *
               MOVE    SPA-AREA        TO  SPA-COMMON
               MOVE    SPA-Q01FREE     TO  SPA-FREE
      *
               MOVE    "LINK"          TO  MCP-STATUS
               MOVE    SPACE           TO  MCP-EVENT
      *
               CALL    "ORCGQ04"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
               MOVE    SPA-COMMON      TO  SPA-AREA
               MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
               IF    ( WRK-UPDKBN      =   "UPD" )
                   MOVE    "B12"       TO  MCP-WIDGET
               ELSE
                   MOVE    "NAME"      TO  MCP-WIDGET
               END-IF
      *
               MOVE    "Q01"           TO  SPA-MOTOPG
               MOVE    "Q04"           TO  SPA-SAKIPG
      *
               MOVE    "NEW"           TO  MCP-PUTTYPE
               MOVE    "Q04    "       TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       470-Q04-SENI-EXT.
           EXIT.
      *****************************************************************
      *    病名部分検索処理
      *****************************************************************
       4101-BTBYOBUBUN-CHK-SEC         SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    Q01-BYOMEISEL       TO  SPA-GMN-Q01-BYOMEISEL
      *
           MOVE    SPA-GMN-Q01-BYOMEISEL
                                       TO  WRK-SELWORD
           PERFORM 800-SELWORD-BUBUN-SEC
           MOVE    WRK-SELWORD         TO  Q01-BYOMEISEL
      *
           PERFORM 4118-BYOMEISEL-CHK-SEC
      *
           .
       4101-BTBYOBUBUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名後方検索処理
      *****************************************************************
       4101-BTBYOKOHO-CHK-SEC          SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    Q01-BYOMEISEL       TO  SPA-GMN-Q01-BYOMEISEL
      *
           MOVE    SPA-GMN-Q01-BYOMEISEL
                                       TO  WRK-SELWORD
           PERFORM 800-SELWORD-KOHO-SEC
           MOVE    WRK-SELWORD         TO  Q01-BYOMEISEL
      *
           PERFORM 4118-BYOMEISEL-CHK-SEC
      *
           .
       4101-BTBYOKOHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為部分検索処理
      *****************************************************************
       4101-BTSRYBUBUN-CHK-SEC         SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    Q01-SRYACTSEL       TO  SPA-GMN-Q01-SRYACTSEL
      *
           MOVE    SPA-GMN-Q01-SRYACTSEL
                                       TO  WRK-SELWORD
           PERFORM 800-SELWORD-BUBUN-SEC
           MOVE    WRK-SELWORD         TO  Q01-SRYACTSEL
      *
           PERFORM 4120-SRYACTSEL-CHK-SEC
      *
           .
       4101-BTSRYBUBUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為後方検索処理
      *****************************************************************
       4101-BTSRYKOHO-CHK-SEC          SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    Q01-SRYACTSEL       TO  SPA-GMN-Q01-SRYACTSEL
      *
           MOVE    SPA-GMN-Q01-SRYACTSEL
                                       TO  WRK-SELWORD
           PERFORM 800-SELWORD-KOHO-SEC
           MOVE    WRK-SELWORD         TO  Q01-SRYACTSEL
      *
           PERFORM 4120-SRYACTSEL-CHK-SEC
      *
           .
       4101-BTSRYKOHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "Q01"               TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG2
           MOVE    SPACE               TO  SPA-MOTOPG3
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      *    検索処理
      *****************************************************************
       409-KENSAKU-SEC             SECTION.
      *
           PERFORM 5011-INPUT-CUR-SEC
      *
      *    共通ＳＰＡ編集
           INITIALIZE                      SPA-Q01-Q96-SENI-AREA
           MOVE    IDXZ                TO  SPA-Q01-Q96-MOTOIDX
      *
           MOVE    "2"                 TO  SPA-Q01-Q96-TYPE
      *
           INITIALIZE                  SPA-Q96-GMN-AREA
           IF      WRK-INPUTCD         NOT =   SPACE
      *        内服で検索
               MOVE    "3"                 TO  SPA-Q96-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-Q01-Q96-INPUTCD
               MOVE    "1"                 TO  SPA-Q01-Q96-TYPE
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-Q01-Q96-CDSYUBETU
               ELSE
                   IF      WRK-INPUTCD(1:1)    NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    3
                           WHEN    6
                               MOVE    "6"     TO  SPA-Q01-Q96-CDSYUBETU
                           WHEN    4
                               MOVE    "4"     TO  SPA-Q01-Q96-CDSYUBETU
                           WHEN    5
                               MOVE    "5"     TO  SPA-Q01-Q96-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-Q01-Q96-CDSYUBETU
                   END-IF
               END-IF
      *        画面編集
               PERFORM 41011-Q96-SET-SEC
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-Q96-GMN-MAX     =   ZERO )  AND
                      (SPA-Q01-Q96-CDSYUBETU   =   "J"  )
                   INITIALIZE                  SPA-Q96-GMN-AREA
                   MOVE    "8"                 TO  SPA-Q96-GMN-SYORI
      *            画面編集
                   PERFORM 41011-Q96-SET-SEC
               END-IF
           ELSE
      *        画面編集
               PERFORM 41011-Q96-SET-SEC
           END-IF
      *
           MOVE    "Q01"               TO  SPA-MOTOPG
           MOVE    "Q96"               TO  SPA-SAKIPG
           MOVE    "Q01"               TO  SPA-Q96-MOTOPG
      *
           IF      SPA-Q96-GMN-MAX     =   ZERO
               MOVE    "INPUT"         TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"       TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "Q96    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-Q96-SET-SEC                           SECTION.
      *
      *
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Q01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGQ96"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
      *
           MOVE    SPA-COMMON           TO  SPA-AREA
           MOVE    SPA-FREE             TO  SPA-Q01FREE
      *
           .
       41011-Q96-SET-EXT.
           EXIT.
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       480-CLEAR-SEC               SECTION.
      *
           MOVE        SPACE       TO  SPA-GMN-Q01-STSRYYMD
                                       SPA-GMN-Q01-EDSRYYMD
           MOVE        SPACE       TO  SPA-NAI-Q01-STSRYYMD
                                       SPA-NAI-Q01-EDSRYYMD
           MOVE        ZERO        TO  SPA-GMN-Q01-MMORDD
      *
           EVALUATE    SPA-GMN-Q01-PAGENO
      *
      *    基本情報
           WHEN    0
               MOVE    101         TO  SPA-GMN-Q01-CUR
      *
               INITIALIZE              SPA-GMN-Q01-KIHON-AREA
                                       SPA-NAI-Q01-KIHON-AREA
      *
      *        基準日開始日和暦
               MOVE    SPA-SYSYMDWH
                                   TO  SPA-GMN-Q01-KJYNYMD
      *        基準日開始日西暦
               MOVE    SPA-SYSYMD  TO  SPA-NAI-Q01-KJYNYMD
      *
      *        性別コンボ
               MOVE    SPA-GMN-Q01-SEXLST (1)
                                   TO  SPA-GMN-Q01-SEX-G
      *        電話番号コンボ
               MOVE    SPA-GMN-Q01-TEL-TOLST (1)
                                   TO  SPA-GMN-Q01-TEL-TO-G
               MOVE    "F"         TO  SPA-GMN-Q01-CHKTSTPT
      *
      *        コメント選択コンボ
               MOVE    SPA-GMN-Q01-SELCOMMENTLST (1)
                                   TO  SPA-GMN-Q01-SELCOMMENT-G
      *
      *        低所得識別コンボ
               MOVE    SPA-GMN-Q01-STTSYSKBTLST (1)
                                   TO  SPA-GMN-Q01-STTSYSKBT-G
      *
      *    保険　公費
           WHEN    1
               MOVE    201         TO  SPA-GMN-Q01-CUR
      *
      *
               INITIALIZE              SPA-GMN-Q01-HOKEN-AREA
                                       SPA-NAI-Q01-HOKEN-AREA
      *
      *        検索条件AND/OR切り替えラジオボタン初期化
               MOVE    "T"         TO  SPA-GMN-Q01-RDHKNKOH-AND
               MOVE    "F"         TO  SPA-GMN-Q01-RDHKNKOH-OR
               MOVE    "F"         TO  SPA-GMN-Q01-RDKOHNUM-AND
               MOVE    "T"         TO  SPA-GMN-Q01-RDKOHNUM-OR
      *
      *    病名　診療行為
           WHEN    2
               IF ( SPA-GMN-Q01-UBYOMEI-STATE
                                       =   WIDGET-NORMAL )
                   MOVE    301         TO  SPA-GMN-Q01-CUR
               ELSE
                   MOVE    302         TO  SPA-GMN-Q01-CUR
               END-IF
      *
               MOVE    SPA-GMN-Q01-RDBYOMEI-A-STATE
                                       TO  WRK-RDBYOMEI-A-STATE
      *
               INITIALIZE                  SPA-GMN-Q01-BYOMEI-AREA
                                           SPA-NAI-Q01-BYOMEI-AREA
      *
      *
               IF  ( WRK-RDBYOMEI-A-STATE
                                   =   WIDGET-NORMAL )
                   MOVE    "T"         TO  SPA-GMN-Q01-RDBYOMEI-U
                   MOVE    "F"         TO  SPA-GMN-Q01-RDBYOMEI-A
                   MOVE    WIDGET-NORMAL
                                       TO  SPA-GMN-Q01-UBYOMEI-STATE
                                           SPA-GMN-Q01-RDBYOMEI-U-STATE
                                           SPA-GMN-Q01-RDBYOMEI-A-STATE
      *
               ELSE
                   MOVE    "F"         TO  SPA-GMN-Q01-RDBYOMEI-U
                   MOVE    "T"         TO  SPA-GMN-Q01-RDBYOMEI-A
                   MOVE    WIDGET-INSENSITIVE
                                       TO  SPA-GMN-Q01-UBYOMEI-STATE
                                           SPA-GMN-Q01-RDBYOMEI-U-STATE
                                           SPA-GMN-Q01-RDBYOMEI-A-STATE
               END-IF
      *
      *        検索条件AND/OR切り替えラジオボタン初期化
               MOVE    "F"             TO  SPA-GMN-Q01-RDBYOMEI-AND
               MOVE    "T"             TO  SPA-GMN-Q01-RDBYOMEI-OR
               MOVE    "F"             TO  SPA-GMN-Q01-RDSRYACT-AND
               MOVE    "T"             TO  SPA-GMN-Q01-RDSRYACT-OR
      *
      *    並び順
           WHEN    3
               MOVE    401             TO  SPA-GMN-Q01-CUR
      *
               INITIALIZE                  SPA-GMN-Q01-NARABI-AREA
                                           SPA-NAI-Q01-NARABI-AREA
               MOVE    SPA-GMN-Q01-SORTLST (1)
                                       TO  SPA-GMN-Q01-KMK-G (1)
               MOVE    SPA-GMN-Q01-SORTLST (2)
                                       TO  SPA-GMN-Q01-KMK-G (2)
               MOVE    SPA-GMN-Q01-SORTLST (3)
                                       TO  SPA-GMN-Q01-KMK-G (3)
               PERFORM 3101-RDIO-SYOKI-SEC
      *
           END-EVALUATE
      *
           .
       480-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       480-ALL-CLEAR-SEC               SECTION.
      *
           MOVE    SPA-GMN-Q01-PAGENO  TO  WRK-PAGENO
      *
           INITIALIZE                  SPA-GMN-Q01-SELPATTERN-G
                                       SPA-NAI-Q01-SELPATTERN-G
      *
           PERFORM VARYING SPA-GMN-Q01-PAGENO  FROM    0   BY  1
                   UNTIL ( SPA-GMN-Q01-PAGENO  >   3 )
      *
                   PERFORM 480-CLEAR-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-PAGENO          TO  SPA-GMN-Q01-PAGENO
      *
           EVALUATE    WRK-PAGENO
           WHEN    0
                   MOVE    101         TO  SPA-GMN-Q01-CUR
           WHEN    1
                   MOVE    201         TO  SPA-GMN-Q01-CUR
           WHEN    2
                   MOVE    301         TO  SPA-GMN-Q01-CUR
           WHEN    3
                   MOVE    401         TO  SPA-GMN-Q01-CUR
           END-EVALUATE
      *
           .
       480-ALL-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    タブ切替処理
      *****************************************************************
       480-TABSWITCH-SEC           SECTION.
      *
           EVALUATE    SPA-GMN-Q01-PAGENO
           WHEN    0
               MOVE    "N02"       TO  WRK-WIDGET
           WHEN    1
               MOVE    "N03"       TO  WRK-WIDGET
           WHEN    2
               MOVE    "N04"       TO  WRK-WIDGET
           WHEN    3
               MOVE    "N01"       TO  WRK-WIDGET
           END-EVALUATE
      *
           PERFORM 440-TAB-CHANGE-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TABSWITCH-EXT
           END-IF
      *
           .
       480-TABSWITCH-EXT.
           EXIT.
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC              SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Q01FREE         TO  LNK-SCRSPA
      *
           INITIALIZE                  SPA-KYOTUKEY
      *    画面移行用のパラメタ変更
           MOVE    "Q01"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "Q01"               TO  SPA-MOTOPG3
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "Y01    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Q01FREE         TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "Q01"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "U01"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       490-KENSAKU-SEC             SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4901-ALL-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-KENSAKU-EXT
           END-IF
      *
      *    遷移情報ＳＰＡ編集処理
           PERFORM 4901-SPASET-SEC
      *
      *    検索該当項目情報設定処理
           PERFORM 4901-GAITOINF-SET-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-KENSAKU-EXT
           END-IF
      * 
      
           MOVE    "Q01"               TO  SPA-MOTOPG
           MOVE    "Q02"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Q02    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全入力項目チェック
      *****************************************************************
       4901-ALL-INPUT-CHK-SEC          SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        開始診療年月基本チェック
               PERFORM 4100-STSRYYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        終了診療年月基本チェック
               PERFORM 4101-EDSRYYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        診療年月日関連チェック
               PERFORM 4900-SRYYMDCHK-SEC
           END-IF
      *
      *    -- 基本 --
           IF    ( SPA-ERRCD   =   SPACE )
      *        基本チェック
               PERFORM 4101-KHN-INPUT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    ZERO        TO  SPA-GMN-Q01-PAGENO
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        関連チェック
               PERFORM 4901-TAB1-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    ZERO    TO  SPA-GMN-Q01-PAGENO
               END-IF
           END-IF
      *
      *    -- 保険　公費 --
           IF    ( SPA-ERRCD   =   SPACE )
      *        基本チェック
               PERFORM 4101-HKN-INPUT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    1       TO  SPA-GMN-Q01-PAGENO
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        関連チェック
               PERFORM 4901-TAB2-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    1       TO  SPA-GMN-Q01-PAGENO
               END-IF
           END-IF
      *
      *    -- 病名　診療行為 --
           IF    ( SPA-ERRCD   =   SPACE )
      *        基本チェック
               PERFORM 4101-BYOSRY-INPUT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    2       TO  SPA-GMN-Q01-PAGENO
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        関連チェック
               PERFORM 4901-TAB3-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    2       TO  SPA-GMN-Q01-PAGENO
               END-IF
           END-IF
      *
      *    -- 並び順 --
           IF    ( SPA-ERRCD   =   SPACE )
      *        基本チェック
               PERFORM 4101-SRT-INPUT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    3       TO  SPA-GMN-Q01-PAGENO
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        関連チェック
               PERFORM 4901-TAB4-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    3       TO  SPA-GMN-Q01-PAGENO
               END-IF
           END-IF
      *
           .
       4901-ALL-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *       診療年月日チェック処理
      *****************************************************************
       4900-SRYYMDCHK-SEC              SECTION.
      *
      *
      *    診療開始日チェック
      *    1.開始診療年月日≠空白かつ
      *      終了診療年月日≠空白のとき、
      *      開始診療年月日≦終了診療年月日であること
           IF      ( SPA-NAI-Q01-STSRYYMD  NOT =   SPACE )
            AND    ( SPA-NAI-Q01-EDSRYYMD  NOT =   SPACE )
               IF  ( SPA-NAI-Q01-STSRYYMD     <=   SPA-NAI-Q01-EDSRYYMD)
                   CONTINUE
               ELSE
                   MOVE    "1001"      TO   SPA-ERRCD
                   MOVE    1           TO   SPA-GMN-Q01-CUR
                   GO  TO  4900-SRYYMDCHK-EXT
               END-IF
           END-IF
      *
           .
       4900-SRYYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
           EVALUATE    SPA-GMN-Q01-PAGENO
               WHEN    0
                   PERFORM 4901-TAB1-CHK-SEC
               WHEN    1
                   PERFORM 4901-TAB2-CHK-SEC
               WHEN    2
                   PERFORM 4901-TAB3-CHK-SEC
               WHEN    3
                   PERFORM 4901-TAB4-CHK-SEC
           END-EVALUATE    
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       基本情報タブの関連チェック処理
      *****************************************************************
       4901-TAB1-CHK-SEC           SECTION.
      *
      *    ＜患者番号＞
      *    1.開始患者番号≠空白かつ
      *      終了患者番号≠空白かつのとき、
      *      開始患者番号≦終了患者番号であること
           IF      ( SPA-GMN-Q01-STPTNUM   NOT =   SPACE )
            AND    ( SPA-GMN-Q01-EDPTNUM   NOT =   SPACE )
               IF  ( SPA-GMN-Q01-STPTNUM      <=  SPA-GMN-Q01-EDPTNUM )
                   CONTINUE
               ELSE
                   MOVE    "1006"      TO   SPA-ERRCD
                   MOVE    110         TO   SPA-GMN-Q01-CUR
                   GO  TO  4901-TAB1-CHK-EXT
               END-IF
           END-IF
      *
      *    ＜生年月日＞
      *    1.開始生年月日≠空白かつ
      *      終了生年月日≠空白のとき、
      *      開始生年月日≦終了生年月日であること
           IF      ( SPA-GMN-Q01-STBIRTHDAY
                                       NOT =   SPACE  )
            AND    ( SPA-GMN-Q01-EDBIRTHDAY
                                       NOT =   SPACE  )
               IF  ( SPA-NAI-Q01-STBIRTHDAY
                                          <=  SPA-NAI-Q01-EDBIRTHDAY )
                   CONTINUE
               ELSE
                   MOVE    "1002"      TO   SPA-ERRCD
                   MOVE    101         TO   SPA-GMN-Q01-CUR
                   GO  TO  4901-TAB1-CHK-EXT
               END-IF
           END-IF
      *
      *    ＜年齢＞
      *    1.開始年齢≠空白かつ
      *      終了年齢≠空白のとき、
      *      開始年齢≦終了年齢であること
           IF      ( SPA-GMN-Q01-STAGE NOT =   SPACE )
            AND    ( SPA-GMN-Q01-EDAGE NOT =   SPACE )
               IF  ( SPA-NAI-Q01-STAGE    <=   SPA-NAI-Q01-EDAGE )
                   CONTINUE
               ELSE
                   MOVE    "1003"      TO   SPA-ERRCD
                   MOVE    103         TO   SPA-GMN-Q01-CUR
                   GO  TO  4901-TAB1-CHK-EXT
               END-IF
           END-IF
      *
      *
      *    ＜生年月日・年齢＞
      *    1.開始生年月日≠空白または
      *      終了生年月日≠空白のとき、
      *      開始年齢＝空白かつ
      *      終了年齢＝空白であること。
           IF  ( SPA-GMN-Q01-STBIRTHDAY    NOT =   SPACE )
            OR ( SPA-GMN-Q01-EDBIRTHDAY    NOT =   SPACE )
               IF      ( SPA-GMN-Q01-STAGE =   SPACE )
                AND    ( SPA-GMN-Q01-EDAGE =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    "1004"      TO  SPA-ERRCD
                   MOVE    101         TO  SPA-GMN-Q01-CUR
                   GO  TO  4901-TAB1-CHK-EXT
               END-IF
           END-IF
      *
      *    ＜年齢・基準日＞
      *    1.開始年齢≠空白または
      *      終了年齢≠空白のとき、
      *      基準日≠空白であること
           IF  ( SPA-GMN-Q01-STAGE         NOT =   SPACE )
            OR ( SPA-GMN-Q01-EDAGE         NOT =   SPACE )
               IF  ( SPA-GMN-Q01-KJYNYMD   NOT =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    "1005"      TO  SPA-ERRCD
                   MOVE    105         TO  SPA-GMN-Q01-CUR
                   GO  TO  4901-TAB1-CHK-EXT
               END-IF
           END-IF
      *
      *    ＜最終受診日＞
      *    1.開始最終受診日≠空白かつ
      *      終了最終受診日≠空白のとき、
      *      開始最終受診日≦最終受診日終了日であること
           IF      ( SPA-GMN-Q01-STLASTYMD NOT =   SPACE )
            AND    ( SPA-GMN-Q01-EDLASTYMD NOT =   SPACE )
               IF  ( SPA-NAI-Q01-STLASTYMD    <=  SPA-NAI-Q01-EDLASTYMD)
                   CONTINUE
               ELSE
                   MOVE    "1008"      TO   SPA-ERRCD
                   MOVE    107         TO   SPA-GMN-Q01-CUR
                   GO  TO  4901-TAB1-CHK-EXT
               END-IF
           END-IF
      *
      *    ＜低所得有効期間＞
      *    1.開始月≠空白かつ
      *      終了月≠空白のとき、
      *      開始月≦終了月であること
           IF      ( SPA-GMN-Q01-TSY         NOT =   SPACE )
               IF      ( SPA-GMN-Q01-STTSYYM NOT =   SPACE )
                AND    ( SPA-GMN-Q01-EDTSYYM NOT =   SPACE )
                   IF  ( SPA-NAI-Q01-STTSYYM    <= SPA-NAI-Q01-EDTSYYM )
                       CONTINUE
                   ELSE
                       MOVE    "1010"      TO   SPA-ERRCD
                       MOVE    154         TO   SPA-GMN-Q01-CUR
                       GO  TO  4901-TAB1-CHK-EXT
                   END-IF
               END-IF
      *
               IF      ( SPA-GMN-Q01-STTSYSKBT   =   "2" OR "3" )
                   IF  ( SPA-NAI-Q01-STTSYYM NOT =   SPACE )
                       CONTINUE
                   ELSE
                       IF    ( SPA-GMN-Q01-STTSYSKBT
                                                =   "2" )
                           MOVE    "1011"  TO   SPA-ERRCD
                       ELSE
                           MOVE    "1012"  TO   SPA-ERRCD
                       END-IF
                       MOVE    154         TO   SPA-GMN-Q01-CUR
                       GO  TO  4901-TAB1-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           .
       4901-TAB1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       遷移情報ＳＰＡ編集処理
      *****************************************************************
       4901-SPASET-SEC             SECTION.
      *
           INITIALIZE              SPA-Q01-Q02-SENI-AREA
      *
      *    診療年月日
           MOVE    SPA-NAI-Q01-STSRYYMD    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-STSRYYMD
      *
           MOVE    SPA-NAI-Q01-EDSRYYMD    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-EDSRYYMD
      *
      *    基本情報
           MOVE    SPA-NAI-Q01-STBIRTHDAY  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-STBIRTHDAY
      *
           MOVE    SPA-NAI-Q01-EDBIRTHDAY  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-EDBIRTHDAY
      *
      *    年齢が入力のとき、生年月日を計算  
      *    ex.2003.04.01現在で１〜２歳の子供が対象の場合、
      *       2000.04.02〜2002.04.01の間の生年月日の子供が対象と
      *       なるように換算する
           IF  (   SPA-GMN-Q01-STAGE   NOT =   SPACE )
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-Q01-KJYNYMD TO  LNK-DAY6-KIJUN
               MOVE    "3"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN  =   SPA-NAI-Q01-STAGE
                                       *   -1
               CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-EDBIRTHDAY
      *
           END-IF
      *
           IF  (   SPA-GMN-Q01-EDAGE   NOT =   SPACE )
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-Q01-KJYNYMD TO  LNK-DAY6-KIJUN
               MOVE    "3"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN  = ( SPA-NAI-Q01-EDAGE   +   1 )
                                       *   -1
               CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  WRK-YMD
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE     1                  TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-STBIRTHDAY
           END-IF
      *
           IF    ( SPA-GMN-Q01-STAGE       =   SPACE )
            AND  ( SPA-GMN-Q01-EDAGE       =   SPACE )
               MOVE    ZERO                TO  SPA-Q01-AGE-CALC-KBN
           ELSE
               MOVE    1                   TO  SPA-Q01-AGE-CALC-KBN
           END-IF
      *
           MOVE    SPA-NAI-Q01-KJYNYMD     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-KJYNYMD
      *
           MOVE    SPA-NAI-Q01-BIRTHMON    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-BIRTHMON
      *
           MOVE    SPA-GMN-Q01-SEX         TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-SEX
      *
           MOVE    SPA-NAI-Q01-STLASTYMD   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-STLASTYMD
      *
           MOVE    SPA-NAI-Q01-EDLASTYMD   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-EDLASTYMD
      *
           MOVE    SPA-GMN-Q01-NYUGAIKBN   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-NYUGAIKBN
      *
           MOVE    SPA-GMN-Q01-SRYKA       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-SRYKA
      *
           MOVE    SPA-GMN-Q01-DEATH       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-DEATH
      *
           MOVE    SPA-GMN-Q01-STPTNUM     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-STPTNUM
      *
           MOVE    SPA-GMN-Q01-EDPTNUM     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-EDPTNUM
      *
           MOVE    SPA-GMN-Q01-CHKTSTPT    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-CHKTSTPT
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   5 )
      *
               MOVE    SPA-GMN-Q01-JYUSHO  (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-JYUSHO  (IDXC)
      *
               INSPECT SPA-Q01-JYUSHO (IDXC)
                       REPLACING   ALL "＊" BY  "%%"
      *
               MOVE    SPA-GMN-Q01-NAME    (IDXC)
                                           TO  WRK-NAME
               PERFORM 49011-EDIT-SRHNAME-SEC
               MOVE    WRK-NAME            TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-NAME    (IDXC)
      *
           END-PERFORM
      *
           EVALUATE    SPA-GMN-Q01-TEL-TO
           WHEN    "1"
               MOVE    "1"                 TO  SPA-Q01-TEL-TO-HOME1
           WHEN    "2"
               MOVE    "1"                 TO  SPA-Q01-TEL-TO-HOME2
           WHEN    "3"
               MOVE    "1"                 TO  SPA-Q01-TEL-TO-HOME1
                                               SPA-Q01-TEL-TO-HOME2
           WHEN    OTHER
               MOVE    "1"                 TO  SPA-Q01-TEL-TO-HOME1
           END-EVALUATE
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   3 )
      *
               MOVE    SPA-GMN-Q01-CONDITION1 (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-CONDITION1 (IDXC)
      *
               MOVE    SPA-GMN-Q01-CONDITION2 (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-CONDITION2 (IDXC)
      *
               MOVE    SPA-GMN-Q01-CONDITION3 (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-CONDITION3 (IDXC)
      *
               MOVE    SPA-GMN-Q01-GENMEN (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-GENMEN (IDXC)
      *
               MOVE    SPA-GMN-Q01-SELCOMMENT
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-SELCOMMENT
      *
               MOVE    SPA-GMN-Q01-COMMENT (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-COMMENT (IDXC)
      *
               INSPECT SPA-Q01-COMMENT (IDXC)
                       REPLACING   ALL "＊" BY  "%%"
      *
               MOVE    SPA-GMN-Q01-TEL (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-TEL (IDXC)
      *
               INSPECT SPA-Q01-TEL (IDXC)
                       REPLACING   ALL "*" BY  "%"
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-Q01-TSY         TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-TSY
      *
           MOVE    SPA-NAI-Q01-STTSYYM     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-STTSYYM
      *
           MOVE    SPA-GMN-Q01-STTSYSKBT   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-STTSYSKBT
      *
           MOVE    SPA-NAI-Q01-EDTSYYM     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-EDTSYYM
      *
           MOVE    SPA-GMN-Q01-TOKKICD     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-TOKKICD
      *
           MOVE    SPA-GMN-Q01-TOKKINAME   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-TOKKINAME
      *
           MOVE    SPA-GMN-Q01-SAIGAIKBN   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-SAIGAIKBN
      *
      *    保険　公費
           IF    ( SPA-GMN-Q01-RDKOHNUM-AND    =   "T" )
               MOVE    "1"                 TO  SPA-Q01-KOHNUM-JKN
           ELSE
               MOVE    SPACE               TO  SPA-Q01-KOHNUM-JKN
           END-IF
      *
           IF    ( SPA-GMN-Q01-RDHKNKOH-AND    =   "T" )
               MOVE    "1"                 TO  SPA-Q01-HKNKOH-JKN
           ELSE
               MOVE    SPACE               TO  SPA-Q01-HKNKOH-JKN
           END-IF
      *
           MOVE    SPA-GMN-Q01-HONKZKKBN   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-HONKZKKBN
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   5 )
      *
               MOVE    SPA-GMN-Q01-HKNJANUM(IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-HKNJANUM(IDXC)
      *
               MOVE    SPA-GMN-Q01-HKNNUM  (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-HKNNUM  (IDXC)
      *
               MOVE    SPA-GMN-Q01-KIGO    (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-KIGO    (IDXC)
      *
               MOVE    SPA-GMN-Q01-NUM     (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-NUM     (IDXC)
      *
               MOVE    SPA-GMN-Q01-KOHNUM  (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-KOHNUM  (IDXC)
      *
               EVALUATE    SPA-GMN-Q01-PAYKBN  (IDXC)
               WHEN    "1"
                   MOVE    "01"            TO  SPA-Q01-PAYKBN  (IDXC)
               WHEN    "2"
                   MOVE    "02"            TO  SPA-Q01-PAYKBN  (IDXC)
               WHEN    "3"
                   MOVE    "03"            TO  SPA-Q01-PAYKBN  (IDXC)
               WHEN    OTHER
                   MOVE    SPACE           TO  SPA-Q01-PAYKBN  (IDXC)
               END-EVALUATE
      *
               MOVE    SPA-GMN-Q01-FTNJANUM(IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-FTNJANUM(IDXC)
      *
           END-PERFORM
      *
      *    病名　診療行為
           MOVE    SPA-GMN-Q01-UTAGAIKBN   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-UTAGAIKBN
      *
           MOVE    SPA-GMN-Q01-KYUSEIKBN   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-KYUSEIKBN
      *
           MOVE    SPA-GMN-Q01-TENKIKBN    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-TENKIKBN
      *
           MOVE    SPA-NAI-Q01-STTENKIYMD  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-STTENKIYMD
      *
           MOVE    SPA-NAI-Q01-EDTENKIYMD  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-EDTENKIYMD
      *
           MOVE    SPA-GMN-Q01-SYUBYOKBN   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-SYUBYOKBN
      *
           IF    ( SPA-GMN-Q01-RDBYOMEI-AND    =   "T" )
               MOVE    "1"                 TO  SPA-Q01-BYOMEI-JKN
           ELSE
               MOVE    SPACE               TO  SPA-Q01-BYOMEI-JKN
           END-IF
      *
           MOVE    SPA-GMN-Q01-SRYKBN      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR   TO  SPA-Q01-SRYKBN
      *
           MOVE    SPA-GMN-Q01-SRYSYUKBN   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR       TO  SPA-Q01-SRYSYUKBN
      *
           IF    ( SPA-GMN-Q01-RDSRYACT-AND    =   "T" )
               MOVE    "1"                 TO  SPA-Q01-SRYACT-JKN
           ELSE
               MOVE    SPACE               TO  SPA-Q01-SRYACT-JKN
           END-IF
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   5 )
               MOVE    SPA-GMN-Q01-BYOMEI(IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-BYOMEI(IDXC)
      *
               INSPECT SPA-Q01-BYOMEI(IDXC)
                       REPLACING   ALL "*" BY  "%"
      *
               INSPECT SPA-Q01-BYOMEI(IDXC)
                       REPLACING   ALL "＊" BY  "%%"
      *
               MOVE    SPA-NAI-Q01-SRYCD (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-SRYCD (IDXC)
           END-PERFORM
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   3 )
      *
               MOVE    SPA-GMN-Q01-RECECOM (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-RECECOM (IDXC)
      *
               INSPECT SPA-Q01-RECECOM (IDXC)
                       REPLACING   ALL "＊" BY  "%%"
      *
           END-PERFORM
      *
      *    並び順
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   3 )
      *
               MOVE    SPA-GMN-Q01-KMK   (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-KMK (IDXC)
      *
               IF  ( SPA-GMN-Q01-RDIO1 (IDXC)  =   "T" )
                   MOVE    1               TO  SPA-Q01-NARABE (IDXC)
               ELSE
                   MOVE    2               TO  SPA-Q01-NARABE (IDXC)
               END-IF
      *
           END-PERFORM
      *
      *    重複チェック
           PERFORM 4901-SPASET-DABURI-CHK
      *
           .
       4901-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索条件編集処理
      *****************************************************************
       49011-EDIT-SRHNAME-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-NAME-G
      *
           MOVE    "sub"               TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "＊　"              TO  ORCSSTR-STRING2
           MOVE    "＊"                TO  ORCSSTR-STRING3
           CALL    "orcsstring" USING  ORCSSTRINGAREA
           MOVE    ORCSSTR-STRING1     TO  WRK-NAME
           MOVE    "search"            TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "＊"                TO  ORCSSTR-STRING2
           CALL    "orcsstring" USING  ORCSSTRINGAREA
           MOVE    ORCSSTR-NUM1        TO  WRK-ORCSSTR-NUM1
           IF    ( ORCSSTR-NUM1        >   ZERO )
               IF    ( ORCSSTR-NUM1    =   1 )
                   MOVE    "substr"    TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME    TO  ORCSSTR-STRING1
                   COMPUTE ORCSSTR-NUM1    =   2
                   COMPUTE ORCSSTR-NUM2    =   0
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2
                                       TO  WRK-NAME2
               ELSE
                   MOVE    "substr"    TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME    TO  ORCSSTR-STRING1
                   COMPUTE ORCSSTR-NUM2    =   ORCSSTR-NUM1    -   1
                   COMPUTE ORCSSTR-NUM1    =   1
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2
                                       TO  WRK-NAME1
                   MOVE    "substr"    TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME    TO  ORCSSTR-STRING1
                   COMPUTE ORCSSTR-NUM1    =   WRK-ORCSSTR-NUM1
                                           +   1
                   COMPUTE ORCSSTR-NUM2    =   0
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2
                                       TO  WRK-NAME2
               END-IF
           ELSE
               MOVE    WRK-NAME        TO  WRK-NAME1
           END-IF
      *
           MOVE    SPACE           TO  WRK-NAME
           IF    ( WRK-NAME1       NOT =   SPACE )
            AND  ( WRK-NAME2           =   SPACE )
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
               INTO    WRK-NAME
               END-STRING
           END-IF
           IF    ( WRK-NAME1       NOT =   SPACE )
            AND  ( WRK-NAME2       NOT =   SPACE )
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                       "　"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
               INTO    WRK-NAME
               END-STRING
           END-IF
           IF    ( WRK-NAME1           =   SPACE )
            AND  ( WRK-NAME2       NOT =   SPACE )
               STRING  "%"             DELIMITED  BY  SIZE
                       "　"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
               INTO    WRK-NAME
               END-STRING
           END-IF
      *
           .
       49011-EDIT-SRHNAME-EXT.
           EXIT.
      *****************************************************************
      *       重複チェック
      *****************************************************************
       4901-SPASET-DABURI-CHK          SECTION.
      *
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   4 )
      *
               COMPUTE WRK-STIDX   =   IDX6    +   1
               PERFORM VARYING IDX7    FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX7    >   5 )
      *
      *            住所
                   IF    ( SPA-Q01-JYUSHO (IDX6)   NOT =   SPACE )
                       IF    ( SPA-Q01-JYUSHO (IDX6)
                                       =   SPA-Q01-JYUSHO (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-JYUSHO (IDX7)
                       END-IF
                   END-IF
      *
      *            漢字氏名
                   IF    ( SPA-Q01-NAME (IDX6)     NOT =   SPACE )
                       IF    ( SPA-Q01-NAME (IDX6)
                                       =   SPA-Q01-NAME (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-NAME (IDX7)
                       END-IF
                   END-IF
      *
      *            保険者番号
                   IF    ( SPA-Q01-HKNJANUM (IDX6) NOT =   SPACE )
                       IF    ( SPA-Q01-HKNJANUM (IDX6)
                                       =   SPA-Q01-HKNJANUM (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-HKNJANUM (IDX7)
                       END-IF
                   END-IF
      *
      *            保険の種類
                   IF    ( SPA-Q01-HKNNUM (IDX6)   NOT =   SPACE )
                       IF    ( SPA-Q01-HKNNUM (IDX6)
                                       =   SPA-Q01-HKNNUM (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-HKNNUM (IDX7)
                       END-IF
                   END-IF
      *
      *            記号・番号
                   IF    ( SPA-Q01-KIGO (IDX6)     NOT =   SPACE )
                     OR  ( SPA-Q01-NUM  (IDX6)     NOT =   SPACE )
                       IF    ( SPA-Q01-KIGO (IDX6)
                                       =   SPA-Q01-KIGO (IDX7))
                         AND ( SPA-Q01-NUM  (IDX6)
                                       =   SPA-Q01-NUM  (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-KIGO (IDX7)
                           MOVE    SPACE   TO  SPA-Q01-NUM  (IDX7)
                       END-IF
                   END-IF
      *
      *            公費の種類・支払い区分
                   IF    ( SPA-Q01-KOHNUM (IDX6)   NOT =   SPACE )
                     OR  ( SPA-Q01-PAYKBN (IDX6)   NOT =   SPACE )
                       IF    ( SPA-Q01-KOHNUM (IDX6)
                                       =   SPA-Q01-KOHNUM (IDX7))
                         AND ( SPA-Q01-PAYKBN (IDX6)
                                       =   SPA-Q01-PAYKBN (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-KOHNUM (IDX7)
                           MOVE    SPACE   TO  SPA-Q01-PAYKBN (IDX7)
                       END-IF
                   END-IF
      *
      *            負担者番号
                   IF    ( SPA-Q01-FTNJANUM (IDX6) NOT =   SPACE )
                       IF    ( SPA-Q01-FTNJANUM (IDX6)
                                       =   SPA-Q01-FTNJANUM (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-FTNJANUM (IDX7)
                       END-IF
                   END-IF
      *
      *            病名
                   IF    ( SPA-Q01-BYOMEI (IDX6)   NOT =   SPACE )
                       IF    ( SPA-Q01-BYOMEI (IDX6)
                                       =   SPA-Q01-BYOMEI (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-BYOMEI (IDX7)
                       END-IF
                   END-IF
      *
      *            診療行為コード
                   IF    ( SPA-Q01-SRYCD (IDX6)   NOT =   SPACE )
                       IF    ( SPA-Q01-SRYCD (IDX6)
                                       =   SPA-Q01-SRYCD (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-SRYCD (IDX7)
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   2 )
      *
               COMPUTE WRK-STIDX   =   IDX6    +   1
               PERFORM VARYING IDX7    FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX7    >   3 )
      *
      *            状態１
                   IF    ( SPA-Q01-CONDITION1 (IDX6)   NOT =   SPACE )
                       IF    ( SPA-Q01-CONDITION1 (IDX6)
                                       =   SPA-Q01-CONDITION1 (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-CONDITION1 (IDX7)
                       END-IF
                   END-IF
      *
      *            状態２
                   IF    ( SPA-Q01-CONDITION2 (IDX6)   NOT =   SPACE )
                       IF    ( SPA-Q01-CONDITION2 (IDX6)
                                       =   SPA-Q01-CONDITION2 (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-CONDITION2 (IDX7)
                       END-IF
                   END-IF
      *
      *            状態３
                   IF    ( SPA-Q01-CONDITION3 (IDX6)   NOT =   SPACE )
                       IF    ( SPA-Q01-CONDITION3 (IDX6)
                                       =   SPA-Q01-CONDITION3 (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-CONDITION3 (IDX7)
                       END-IF
                   END-IF
      *
      *            減免事由
                   IF    ( SPA-Q01-GENMEN (IDX6)   NOT =   SPACE )
                       IF    ( SPA-Q01-GENMEN (IDX6)
                                       =   SPA-Q01-GENMEN (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-GENMEN (IDX7)
                       END-IF
                   END-IF
      *
      *            コメント
                   IF    ( SPA-Q01-COMMENT (IDX6)  NOT =   SPACE )
                       IF    ( SPA-Q01-COMMENT (IDX6)
                                       =   SPA-Q01-COMMENT (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-COMMENT (IDX7)
                       END-IF
                   END-IF
      *
      *            レセプトコメント
                   IF    ( SPA-Q01-RECECOM (IDX6)  NOT =   SPACE )
                       IF    ( SPA-Q01-RECECOM (IDX6)
                                       =   SPA-Q01-RECECOM (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-RECECOM (IDX7)
                       END-IF
                   END-IF
      *
      *            電話番号
                   IF    ( SPA-Q01-TEL (IDX6)  NOT =   SPACE )
                       IF    ( SPA-Q01-TEL (IDX6)
                                       =   SPA-Q01-TEL (IDX7))
                           MOVE    SPACE   TO  SPA-Q01-TEL (IDX7)
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           .
       4901-SPASET-DABURI-EXT.
           EXIT.
      *****************************************************************
      *       検索該当項目情報設定処理
      *****************************************************************
       4901-GAITOINF-SET-SEC           SECTION.
      *
           MOVE    SPACE                   TO  SPA-Q01-GAITO-TBL
      *
      *    該当（１）
           IF  ( SPA-Q01-STPTNUM           NOT =   SPACE )
            OR ( SPA-Q01-EDPTNUM           NOT =   SPACE )
               MOVE    "1"                 TO  SPA-Q01-GTBL(1)
      *
           END-IF
      *
      *    該当（２）
           IF  ( SPA-Q01-NAME-G            NOT =   SPACE )
      *
               MOVE    "1"                 TO  SPA-Q01-GTBL(2)
      *
           END-IF
      *
      *    該当（３）
           IF  ( SPA-Q01-STLASTYMD         NOT =   SPACE )
            OR ( SPA-Q01-EDLASTYMD         NOT =   SPACE )
               MOVE    "1"                 TO  SPA-Q01-GTBL(3)
           END-IF
      *
      *    該当（４）
           IF  ( SPA-Q01-BYOMEI-G          NOT =   SPACE )
            OR ( SPA-Q01-UTAGAIKBN         NOT =   SPACE )
            OR ( SPA-Q01-KYUSEIKBN         NOT =   SPACE )
            OR ( SPA-Q01-TENKIKBN          NOT =   SPACE )
            OR ( SPA-Q01-SYUBYOKBN         NOT =   SPACE )
      *
               MOVE    "1"                 TO  SPA-Q01-GTBL(4)
      *
           END-IF
      *
      *
      *    該当（５）
           IF  ( SPA-Q01-HONKZKKBN         NOT =   SPACE )
            OR ( SPA-Q01-HKNJANUM-G        NOT =   SPACE )
            OR ( SPA-Q01-HKNNUM-G          NOT =   SPACE )
            OR ( SPA-Q01-KIGO-G            NOT =   SPACE )
            OR ( SPA-Q01-NUM-G             NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GTBL(5)
      *
           END-IF
      *
      *    該当（６）
           IF  ( SPA-Q01-KOHNUM-G          NOT =   SPACE )
            OR ( SPA-Q01-FTNJANUM-G        NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GTBL(6)
      *
           END-IF
      *
      *    該当（７）
           IF  ( SPA-Q01-JYUSHO-G          NOT =   SPACE )
            OR ( SPA-Q01-CONDITION1-G      NOT =   SPACE )
            OR ( SPA-Q01-CONDITION2-G      NOT =   SPACE )
            OR ( SPA-Q01-CONDITION3-G      NOT =   SPACE )
            OR ( SPA-Q01-GENMEN-G          NOT =   SPACE )
            OR ( SPA-Q01-COMMENT-G         NOT =   SPACE )
            OR ( SPA-Q01-STBIRTHDAY        NOT =   SPACE )
            OR ( SPA-Q01-EDBIRTHDAY        NOT =   SPACE )
            OR ( SPA-Q01-BIRTHMON          NOT =   "00" )
            OR ( SPA-Q01-SEX               NOT =   SPACE )
            OR ( SPA-Q01-TEL-G             NOT =   SPACE )
            OR ( SPA-Q01-DEATH             NOT =   SPACE )
      *
               MOVE    "1"                 TO  SPA-Q01-GTBL(7)
      *
           END-IF
      *
      *    該当（８）
           IF  ( SPA-Q01-STSRYYMD          NOT =   SPACE  )
            OR ( SPA-Q01-EDSRYYMD          NOT =   SPACE  )
            OR ( SPA-Q01-NYUGAIKBN         NOT =   SPACE )
            OR ( SPA-Q01-SRYKA             NOT =   SPACE )
      *
               MOVE    "1"                 TO  SPA-Q01-GTBL(8)
      *
           END-IF
      *
      *    該当（９）
           IF  ( SPA-Q01-SRYKBN            NOT =   SPACE )
            OR ( SPA-Q01-SRYCD-G           NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GTBL(9)
      *
           END-IF
      *
      *    該当（１０）
           IF  ( SPA-Q01-TSY           NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GTBL(10)
      *
           END-IF
      *
      *    該当（１１）
           IF   (  SPA-Q01-RECECOM-G       NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GTBL(11)
      *
           END-IF
      *
      *    該当（１２）
           IF  ( SPA-Q01-TOKKI-G          NOT =   SPACE )
            OR ( SPA-Q01-SAIGAIKBN        NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GTBL(12)
      *
           END-IF
      *
      *    検索条件が未入力のとき
           IF  ( SPA-Q01-GAITO-TBL         =   SPACE )
               MOVE    "1007"              TO  SPA-ERRCD
               GO  TO  4901-GAITOINF-SET-EXT
           END-IF
      *
           .
       4901-GAITOINF-SET-EXT.
           EXIT.
      *****************************************************************
      *       保険・公費情報タブの関連チェック処理
      *****************************************************************
       4901-TAB2-CHK-SEC               SECTION.
      *
           .
       4901-TAB2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       病名・診療行為情報タブの関連チェック処理
      *****************************************************************
       4901-TAB3-CHK-SEC               SECTION.
      *
      *    ＜転帰日＞
      *    1.開始転帰日≠空白かつ
      *      終了転帰日≠空白のとき、
      *      開始転帰日≦終了転帰日であること
           IF      ( SPA-GMN-Q01-STTENKIYMD
                                       NOT =   SPACE  )
            AND    ( SPA-GMN-Q01-EDTENKIYMD
                                       NOT =   SPACE  )
               IF  ( SPA-NAI-Q01-STTENKIYMD
                                          <=  SPA-NAI-Q01-EDTENKIYMD )
                   CONTINUE
               ELSE
                   MOVE    "1009"      TO   SPA-ERRCD
                   MOVE    321         TO   SPA-GMN-Q01-CUR
                   GO  TO  4901-TAB3-CHK-EXT
               END-IF
           END-IF
      *
           .
       4901-TAB3-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       並び順情報タブの関連チェック処理
      *****************************************************************
       4901-TAB4-CHK-SEC               SECTION.
      *
           .
       4901-TAB4-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC             SECTION.
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
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
           IF    ( SPA-QIDCD       NOT =   SPACE )
               PERFORM 520-QIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Q01"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE                   TO  Q01
           INITIALIZE                          Q01
      *
      *    タブページ番号
           MOVE    SPA-GMN-Q01-PAGENO      TO  Q01-PAGENO
      *
      *    入外区分
           MOVE    SPA-GMN-Q01-SELPATTERN-G TO  Q01-SELPATTERN
           MOVE    SPA-GMN-Q01-SELPATTERNLST-MAX
                                           TO  Q01-CMB-SELPATTERN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-Q01-SELPATTERNLST-MAX )
      *
               MOVE    SPA-GMN-Q01-SELPATTERNLST (IDX1)
                           TO  Q01-CMB-SELPATTERN-ITEM (IDX1)
      *
           END-PERFORM
      *
      *    開始診療年月日
           MOVE    SPA-GMN-Q01-STSRYYMD    TO  Q01-STSRYYMD
      *    終了診療年月日
           MOVE    SPA-GMN-Q01-EDSRYYMD    TO  Q01-EDSRYYMD
      *    前月(前日)/次月(次日)
           IF    ( SPA-GMN-Q01-MMORDD      =   ZERO )
               MOVE    "前月"              TO  Q01-B06-LABEL
               MOVE    "次月"              TO  Q01-B07-LABEL
           ELSE
               MOVE    "前日"              TO  Q01-B06-LABEL
               MOVE    "次日"              TO  Q01-B07-LABEL
           END-IF
      *    開始生年月日
           MOVE    SPA-GMN-Q01-STBIRTHDAY  TO  Q01-STBIRTHDAY
      *    終了生年月日
           MOVE    SPA-GMN-Q01-EDBIRTHDAY  TO  Q01-EDBIRTHDAY
      *    開始年齢
           MOVE    SPA-GMN-Q01-STAGE       TO  Q01-STAGE
      *    終了年齢
           MOVE    SPA-GMN-Q01-EDAGE       TO  Q01-EDAGE
      *    基準日
           MOVE    SPA-GMN-Q01-KJYNYMD     TO  Q01-KJYNYMD
      *    誕生月
           MOVE    SPA-GMN-Q01-BIRTHMON    TO  Q01-BIRTHMON
      *    性別
           MOVE    SPA-GMN-Q01-SEX-G       TO  Q01-SEX
      *
           MOVE    SPA-GMN-Q01-SEXLST-MAX  TO  Q01-CMB-SEX-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       SPA-GMN-Q01-SEXLST-MAX
               MOVE    SPA-GMN-Q01-SEXLST (IDX1)
                                           TO  Q01-CMB-SEX-ITEM (IDX1)
           END-PERFORM
      *
      *    入外区分
           MOVE    SPA-GMN-Q01-NYUGAIKBN-G TO  Q01-NYUGAIKBN
           MOVE    SPA-GMN-Q01-NYUGAIKBNLST-MAX
                                           TO  Q01-CMB-NYUGAIKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-Q01-NYUGAIKBNLST-MAX )
      *
               MOVE    SPA-GMN-Q01-NYUGAIKBNLST (IDX1)
                           TO  Q01-CMB-NYUGAIKBN-ITEM (IDX1)
      *
      *
           END-PERFORM
      *
      *    診療科
           MOVE    SPA-GMN-Q01-SRYKA-G     TO  Q01-SRYKA
           MOVE    SPA-GMN-Q01-SRYKALST-MAX
                                           TO  Q01-CMB-SRYKA-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-Q01-SRYKALST-MAX )
      *
               MOVE    SPA-GMN-Q01-SRYKALST (IDX1)
                           TO  Q01-CMB-SRYKA-ITEM (IDX1)
      *
           END-PERFORM
      *
      *    死亡
           MOVE    SPA-GMN-Q01-DEATH-G     TO  Q01-DEATH
           MOVE    SPA-GMN-Q01-DEATHLST-MAX
                                           TO  Q01-CMB-DEATH-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-Q01-DEATHLST-MAX )
      *
               MOVE    SPA-GMN-Q01-DEATHLST (IDX1)
                           TO  Q01-CMB-DEATH-ITEM (IDX1)
      *
           END-PERFORM
      *
      *    開始最終受診日
           MOVE    SPA-GMN-Q01-STLASTYMD   TO  Q01-STLASTYMD
      *    終了最終受診日
           MOVE    SPA-GMN-Q01-EDLASTYMD   TO  Q01-EDLASTYMD
      *    開始患者番号
           MOVE    SPA-GMN-Q01-STPTNUM     TO  Q01-STPTNUM
      *    終了患者番号
           MOVE    SPA-GMN-Q01-EDPTNUM     TO  Q01-EDPTNUM
      *    テスト患者チェックボタン
           MOVE    SPA-GMN-Q01-CHKTSTPT    TO  Q01-CHKTSTPT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       5
      *
      *        郵便番号
               MOVE    SPA-GMN-Q01-POST   (IDX1)
                                           TO  Q01-POST    (IDX1)
      *        住所
               MOVE    SPA-GMN-Q01-JYUSHO  (IDX1)
                                           TO  Q01-JYUSHO  (IDX1)
      *        漢字氏名
               MOVE    SPA-GMN-Q01-NAME    (IDX1)
                                           TO  Q01-NAME    (IDX1)
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       3
      *        状態１
               MOVE    SPA-GMN-Q01-CONDITION1-G (IDX1)
                                           TO  Q01-CONDITION1 (IDX1)
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2
                                   >   SPA-GMN-Q01-CONDITION1LST-MAX )
      *
                   MOVE    SPA-GMN-Q01-CONDITION1LST (IDX2)
                               TO  Q01-CMB-CONDITION1-ITEM (IDX1 IDX2)
      *
               END-PERFORM
      *
               MOVE    SPA-GMN-Q01-CONDITION1LST-MAX
                                           TO  Q01-CMB-CONDITION1-COUNT
                                                              (IDX1)
      *
      *        状態２
               MOVE    SPA-GMN-Q01-CONDITION2-G (IDX1)
                                           TO  Q01-CONDITION2 (IDX1)
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2
                                   >   SPA-GMN-Q01-CONDITION2LST-MAX )
      *
                   MOVE    SPA-GMN-Q01-CONDITION2LST (IDX2)
                               TO  Q01-CMB-CONDITION2-ITEM (IDX1 IDX2)
      *
               END-PERFORM
      *
               MOVE    SPA-GMN-Q01-CONDITION2LST-MAX
                                           TO  Q01-CMB-CONDITION2-COUNT
                                                              (IDX1)
      *        状態３
               MOVE    SPA-GMN-Q01-CONDITION3-G (IDX1)
                                           TO  Q01-CONDITION3 (IDX1)
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2
                                   >   SPA-GMN-Q01-CONDITION3LST-MAX )
      *
                   MOVE    SPA-GMN-Q01-CONDITION3LST (IDX2)
                               TO  Q01-CMB-CONDITION3-ITEM (IDX1 IDX2)
      *
               END-PERFORM
      *
               MOVE    SPA-GMN-Q01-CONDITION3LST-MAX
                                           TO  Q01-CMB-CONDITION3-COUNT
                                                              (IDX1)
      *        減免事由
               MOVE    SPA-GMN-Q01-GENMEN-G (IDX1)
                                           TO  Q01-GENMEN (IDX1)
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2
                                   >   SPA-GMN-Q01-GENMENLST-MAX )
      *
                   MOVE    SPA-GMN-Q01-GENMENLST (IDX2)
                               TO  Q01-CMB-GENMEN-ITEM (IDX1 IDX2)
      *
               END-PERFORM
      *
               MOVE    SPA-GMN-Q01-GENMENLST-MAX
                                           TO  Q01-CMB-GENMEN-COUNT
                                                              (IDX1)
      *        コメント選択
               MOVE    SPA-GMN-Q01-SELCOMMENT-G
                                           TO  Q01-SELCOMMENT
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2
                                   >   SPA-GMN-Q01-SELCOMMENTLST-MAX )
      *
                   MOVE    SPA-GMN-Q01-SELCOMMENTLST (IDX2)
                               TO  Q01-CMB-SELCOMMENT-ITEM (IDX2)
      *
               END-PERFORM
      *
               MOVE    SPA-GMN-Q01-SELCOMMENTLST-MAX
                                           TO  Q01-CMB-SELCOMMENT-COUNT
      *
      *        コメント
               MOVE    SPA-GMN-Q01-COMMENT (IDX1)
                                           TO  Q01-COMMENT (IDX1)
      *
      *        コメント
               MOVE    SPA-GMN-Q01-TEL (IDX1)
                                           TO  Q01-TEL (IDX1)
           END-PERFORM
      *
      *    電話先
           MOVE    SPA-GMN-Q01-TEL-TO-G TO  Q01-TEL-TO
           MOVE    SPA-GMN-Q01-TEL-TOLST-MAX
                                           TO  Q01-CMB-TEL-TO-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-Q01-TEL-TOLST-MAX )
      *
               MOVE    SPA-GMN-Q01-TEL-TOLST (IDX1)
                                   TO  Q01-CMB-TEL-TO-ITEM (IDX1)
      *
           END-PERFORM
      *
      *    低所得
           MOVE    SPA-GMN-Q01-TSY-G TO  Q01-TSY
           MOVE    SPA-GMN-Q01-TSYLST-MAX
                                           TO  Q01-CMB-TSY-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-Q01-TSYLST-MAX )
      *
               MOVE    SPA-GMN-Q01-TSYLST (IDX1)
                                   TO  Q01-CMB-TSY-ITEM (IDX1)
      *
           END-PERFORM
      *    低所得開始日
           MOVE    SPA-GMN-Q01-STTSYYM     TO  Q01-STTSYYM
      *
      *    低所得開始月識別
           MOVE    SPA-GMN-Q01-STTSYSKBT-G TO  Q01-STTSYSKBT
           MOVE    SPA-GMN-Q01-STTSYSKBTLST-MAX
                                           TO  Q01-CMB-STTSYSKBT-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-Q01-STTSYSKBTLST-MAX )
      *
               MOVE    SPA-GMN-Q01-STTSYSKBTLST (IDX1)
                                   TO  Q01-CMB-STTSYSKBT-ITEM (IDX1)
      *
           END-PERFORM
      *
           EVALUATE    SPA-GMN-Q01-STTSYSKBT
           WHEN    "2"
               MOVE    WIDGET-INSENSITIVE  TO  Q01-EDTSYYM-STATE
           WHEN    "3"
               MOVE    WIDGET-INSENSITIVE  TO  Q01-EDTSYYM-STATE
           WHEN    OTHER
               MOVE    WIDGET-NORMAL       TO  Q01-EDTSYYM-STATE
               MOVE    "〜"                TO  Q01-LBLTSY
           END-EVALUATE
      *
      *    低所得終了日
           MOVE    SPA-GMN-Q01-EDTSYYM     TO  Q01-EDTSYYM
      *
      *    特記事項
           MOVE    SPA-GMN-Q01-TOKKICD         TO  Q01-TOKKICD
           MOVE    SPA-GMN-Q01-TOKKINAME       TO  Q01-TOKKINAME
           MOVE    SPA-GMN-Q01-TOKKIINFO-G     TO  Q01-TOKKIINFO
           MOVE    SPA-GMN-Q01-TOKKIINFOLST-MAX
                                           TO  Q01-CMB-TOKKIINFO-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-Q01-TOKKIINFOLST-MAX )
      *
               MOVE    SPA-GMN-Q01-TOKKIINFOLST (IDX1)
                                   TO  Q01-CMB-TOKKIINFO-ITEM (IDX1)
      *
           END-PERFORM
      *
      *    災害該当
           MOVE    SPA-GMN-Q01-SAIGAIKBN-G     TO  Q01-SAIGAIKBN
           MOVE    SPA-GMN-Q01-SAIGAIKBNLST-MAX
                                           TO  Q01-CMB-SAIGAIKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-Q01-SAIGAIKBNLST-MAX )
      *
               MOVE    SPA-GMN-Q01-SAIGAIKBNLST (IDX1)
                                   TO  Q01-CMB-SAIGAIKBN-ITEM (IDX1)
      *
           END-PERFORM
      *
      *    本人家族
           MOVE    SPA-GMN-Q01-HONKZKKBN-G TO  Q01-HONKZKKBN
           MOVE    SPA-GMN-Q01-HONKZKKBNLST-MAX
                                           TO  Q01-CMB-HONKZKKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-Q01-HONKZKKBNLST-MAX )
      *
               MOVE    SPA-GMN-Q01-HONKZKKBNLST (IDX1)
                                   TO  Q01-CMB-HONKZKKBN-ITEM (IDX1)
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  CNT-KOHNAME
           MOVE    1               TO  IDX-KOHNAME
           MOVE    SPACE           TO  WRK-KOHNAME
                                       WRK-FTNJANUM
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
      *
      *        保険者番号
               MOVE    SPA-GMN-Q01-HKNJANUM(IDX1)
                                           TO  Q01-HKNJANUM(IDX1)
      *        保険の種類
               MOVE    SPA-GMN-Q01-HKNNUM-G  (IDX1)
                                           TO  Q01-HKNNUM  (IDX1)
      *        保険記号
               MOVE    SPA-GMN-Q01-KIGO    (IDX1)
                                           TO  Q01-KIGO    (IDX1)
      *
      *        保険番号
               MOVE    SPA-GMN-Q01-NUM     (IDX1)
                                           TO  Q01-NUM     (IDX1)
      *
      *        公費保険番号
               MOVE    SPA-GMN-Q01-KOHNUM-G  (IDX1)
                                           TO  Q01-KOHNUM  (IDX1)
      *        公費負担者番号
               MOVE    SPA-GMN-Q01-FTNJANUM(IDX1)
                                           TO  Q01-FTNJANUM(IDX1)
               MOVE    SPA-GMN-Q01-HKNNUMLST-MAX
                                           TO  Q01-CMB-HKNNUM-COUNT
                                                           (IDX1)
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >   SPA-GMN-Q01-HKNNUMLST-MAX )
      *
                   MOVE    SPA-GMN-Q01-HKNNUMLST (IDX2)
                               TO  Q01-CMB-HKNNUM-ITEM (IDX1 IDX2)
      *
               END-PERFORM
      *
               MOVE    SPA-GMN-Q01-KOHNUMLST-MAX
                                           TO  Q01-CMB-KOHNUM-COUNT
                                                           (IDX1)
      *
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >   SPA-GMN-Q01-KOHNUMLST-MAX )
      *
                   MOVE    SPA-GMN-Q01-KOHNUMLST (IDX2)
                               TO  Q01-CMB-KOHNUM-ITEM (IDX1 IDX2)
      *
               END-PERFORM
      *
               IF    ( SPA-GMN-Q01-KOHNUMMEI (IDX1)    NOT =   SPACE )
                   COMPUTE CNT-KOHNAME =   CNT-KOHNAME +   1
                   IF    ( SPA-GMN-Q01-RDKOHNUM-AND    =   "T" )
                       IF    ( WRK-KOHNAME             =   SPACE )
                           MOVE    SPA-GMN-Q01-KOHNUMMEI (IDX1)
                                           TO  WRK-KOHNAME
                       END-IF
                   ELSE
                       IF    ( CNT-KOHNAME >   1 )
                           STRING "と"     DELIMITED   BY  SIZE
                           INTO   WRK-KOHNAME
                           WITH   POINTER  IDX-KOHNAME
                           END-STRING
                       END-IF
      *
                           STRING SPA-GMN-Q01-KOHNUMMEI (IDX1)
                                           DELIMITED   BY  SPACE
                           INTO   WRK-KOHNAME
                           WITH   POINTER  IDX-KOHNAME
                           END-STRING
      *
                   END-IF
               END-IF
      *
               IF    ( SPA-GMN-Q01-FTNJANUM(IDX1)  NOT =   SPACE )
                   MOVE    SPA-GMN-Q01-FTNJANUM(IDX1)
                                           TO  WRK-FTNJANUM
               END-IF
      *
           END-PERFORM
      *
      *    病名検索ラジオボタン
           MOVE    SPA-GMN-Q01-RDBYOMEI-U  TO  Q01-RDBYOMEI-U
           MOVE    SPA-GMN-Q01-RDBYOMEI-U-STATE
                                           TO  Q01-RDBYOMEI-U-STATE
           MOVE    SPA-GMN-Q01-RDBYOMEI-A  TO  Q01-RDBYOMEI-A
           MOVE    SPA-GMN-Q01-RDBYOMEI-A-STATE
                                           TO  Q01-RDBYOMEI-A-STATE
      *    病名検索コンボボックス
           MOVE    SPA-GMN-Q01-UBYOMEI-G   TO  Q01-BYOBUN
           MOVE    SPA-GMN-Q01-UBYOMEI-STATE
                                           TO  Q01-BYOBUN-STATE
                                               Q01-CMB-BYOBUN-STATE
           MOVE    SPA-GMN-Q01-UBYOMEILST-MAX
                                           TO  Q01-CMB-BYOBUN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-Q01-UBYOMEILST-MAX )
               MOVE    SPA-GMN-Q01-UBYOMEILST (IDX1)
                                           TO  Q01-CMB-BYOBUN-ITEM
                                                               (IDX1)
           END-PERFORM
      *
      *    疑い病名
           MOVE    SPA-GMN-Q01-UTAGAIKBN-G TO  Q01-UTAGAIKBN
      *
           MOVE    SPA-GMN-Q01-UTAGAIKBNLST-MAX
                                       TO  Q01-CMB-UTAGAIKBN-COUNT
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >       SPA-GMN-Q01-UTAGAIKBNLST-MAX)
      *
               MOVE    SPA-GMN-Q01-UTAGAIKBNLST (IDXB)
                                   TO   Q01-CMB-UTAGAIKBN-ITEM (IDXB)
      *
           END-PERFORM
      *
      *    急性病名
           MOVE    SPA-GMN-Q01-KYUSEIKBN-G TO  Q01-KYUSEIKBN
      *
           MOVE    SPA-GMN-Q01-KYUSEIKBNLST-MAX
                                       TO  Q01-CMB-KYUSEIKBN-COUNT
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >       SPA-GMN-Q01-KYUSEIKBNLST-MAX)
      *
               MOVE    SPA-GMN-Q01-KYUSEIKBNLST (IDXB)
                                   TO   Q01-CMB-KYUSEIKBN-ITEM (IDXB)
      *
           END-PERFORM
      *
      *    転帰区分
           MOVE    SPA-GMN-Q01-TENKIKBN-G TO  Q01-TENKIKBN
      *
           MOVE    SPA-GMN-Q01-TENKIKBNLST-MAX
                                       TO  Q01-CMB-TENKIKBN-COUNT
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   SPA-GMN-Q01-TENKIKBNLST-MAX )
      *
               MOVE    SPA-GMN-Q01-TENKIKBNLST (IDXB)
                                       TO  Q01-CMB-TENKIKBN-ITEM (IDXB)
      *
           END-PERFORM
      *
      *    転帰日
           MOVE    SPA-GMN-Q01-STTENKIYMD  TO  Q01-STTENKIYMD
      *
           MOVE    SPA-GMN-Q01-EDTENKIYMD  TO  Q01-EDTENKIYMD
      *
      *    主病名区分
           MOVE    SPA-GMN-Q01-SYUBYOKBN-G TO  Q01-SYUBYOKBN
      *
           MOVE    SPA-GMN-Q01-SYUBYOKBNLST-MAX
                                       TO  Q01-CMB-SYUBYOKBN-COUNT
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   SPA-GMN-Q01-SYUBYOKBNLST-MAX )
      *
               MOVE    SPA-GMN-Q01-SYUBYOKBNLST (IDXB)
                                       TO  Q01-CMB-SYUBYOKBN-ITEM (IDXB)
      *
           END-PERFORM
      *
      *    病名検索
           MOVE    SPA-GMN-Q01-BYOMEISEL   TO  Q01-BYOMEISEL
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       5
      *        病名コード
               MOVE    SPA-GMN-Q01-BYOMEICD(IDX1)
                                           TO  Q01-BYOMEICD(IDX1)
      *        病名
               MOVE    SPA-GMN-Q01-BYOMEI  (IDX1)
                                           TO  Q01-BYOMEI  (IDX1)
      *        診療行為コード
               MOVE    SPA-GMN-Q01-SRYCD   (IDX1)
                                           TO  Q01-SRYCD(IDX1)
      *        診療行為
               MOVE    SPA-GMN-Q01-SRYACT  (IDX1)
                                           TO  Q01-SRYACT(IDX1)
           END-PERFORM
      *
      *    病名リスト内件数
           MOVE    SPA-GMN-Q01-BYOMEI-MAX  TO  Q01-BYOLST-COUNT
      *
      *    病名リスト
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       SPA-GMN-Q01-BYOMEI-MAX
      *        病名コード
               MOVE    SPA-GMN-Q01-TBYOMEICD(IDX1)
                                           TO  Q01-BYOLST-BYOMEICD(IDX1)
      *        病名
               MOVE    SPA-GMN-Q01-TBYOMEI  (IDX1)
                                           TO  Q01-BYOLST-BYOMEI  (IDX1)
           END-PERFORM
      *
      *    診療区分
           MOVE    SPA-GMN-Q01-SRYKBN-G TO  Q01-SRYKBN
      *
           MOVE    SPA-GMN-Q01-SRYKBNLST-MAX
                                       TO  Q01-CMB-SRYKBN-COUNT
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   SPA-GMN-Q01-SRYKBNLST-MAX )
      *
               MOVE    SPA-GMN-Q01-SRYKBNLST (IDXB)
                                       TO  Q01-CMB-SRYKBN-ITEM (IDXB)
      *
           END-PERFORM
      *
      *    診療種別
      *    IF    ( SPA-GMN-Q01-SRYKBN  =   "20" OR "30" OR "90" )
               MOVE    "診療種別"          TO  Q01-LBLSRYSYUKBN
      *    END-IF
           MOVE    SPA-GMN-Q01-SRYSYUKBN-G TO  Q01-SRYSYUKBN
      *
           MOVE    SPA-GMN-Q01-SRYSYUKBNLST-MAX
                                       TO  Q01-CMB-SRYSYUKBN-COUNT
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   SPA-GMN-Q01-SRYSYUKBNLST-MAX )
      *
               MOVE    SPA-GMN-Q01-SRYSYUKBNLST (IDXB)
                                       TO  Q01-CMB-SRYSYUKBN-ITEM (IDXB)
      *
           END-PERFORM
      *
      *    行為名検索
           MOVE    SPA-GMN-Q01-SRYACTSEL   TO  Q01-SRYACTSEL
      *
      *    行為名リスト内件数
           MOVE    SPA-GMN-Q01-SRYACT-MAX  TO  Q01-SRYLST-COUNT
      *
      *    行為名リスト
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       SPA-GMN-Q01-SRYACT-MAX
      *        行為名コード
               MOVE    SPA-GMN-Q01-TSRYCD(IDX1)
                                           TO  Q01-SRYLST-SRYCD(IDX1)
      *        行為名
               MOVE    SPA-GMN-Q01-TSRYACT  (IDX1)
                                           TO  Q01-SRYLST-SRYACT  (IDX1)
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       3
      *
      *        コメント
               MOVE    SPA-GMN-Q01-RECECOM (IDX1)
                                           TO  Q01-RECECOM (IDX1)
      *
           END-PERFORM
      *
      *    並び順
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       3
      *
               MOVE    SPA-GMN-Q01-KMK-G (IDX1)
                                           TO  Q01-CMB-KMK (IDX1)
      *
               MOVE    SPA-GMN-Q01-SORTLST-MAX
                                           TO  Q01-CMB-KMK-COUNT (IDX1)
      *
      *        並び順コンボ
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-Q01-SORTLST-MAX
                   MOVE    SPA-GMN-Q01-SORTLST(IDX2)
                                           TO  Q01-CMB-KMK-ITEM
                                                        (IDX1, IDX2)
               END-PERFORM
      *
               MOVE    SPA-GMN-Q01-RDIO1 (IDX1)
                                           TO  Q01-RDIO1 (IDX1)
               MOVE    SPA-GMN-Q01-RDIO2 (IDX1)
                                           TO  Q01-RDIO2 (IDX1)
      *
               MOVE    "black"             TO  Q01-LBLORDER-STYLE (IDX1)
      *        IF    ( SPA-GMN-Q01-KMK (IDX1)  =   "5" )
      *            IF    ( WRK-KOHNAME     =   SPACE )
      *             AND  ( WRK-FTNJANUM    =   SPACE )
      *                MOVE    SPACE   TO  Q01-LBLORDER (IDX1)
      *                MOVE    "red"   TO  Q01-LBLORDER-STYLE (IDX1)
      *                STRING  "検索条件に公費情報を"
      *                                        DELIMITED BY SIZE
      *                        "入力してください"
      *                                        DELIMITED BY SIZE
      *                INTO    Q01-LBLORDER (IDX1)
      *                END-STRING
      *            ELSE
      *                IF    ( SPA-GMN-Q01-RDHKNKOH-OR =   "T" )
      *                    MOVE    "red"   TO  Q01-LBLORDER-STYLE (IDX1)
      *                STRING  "主保険と公費のＯＲ条件指定がされている"
      *                                        DELIMITED BY SIZE
      *                        "場合は無効となります。"
      *                                        DELIMITED BY SIZE
      *                INTO    Q01-LBLORDER (IDX1)
      *                END-STRING
      *                    MOVE    "主保険と公費のＯＲ条件指定の時は"
      *                                    TO  Q01-LBLORDER (IDX1)
      *                ELSE
      *                    IF    ( SPA-GMN-Q01-RDKOHNUM-OR =   "T" )
      *                     OR   ( WRK-KOHNAME             =   SPACE )
      *                        CONTINUE
      *                    ELSE
      *                        MOVE    "blue"
      *                                    TO  Q01-LBLORDER-STYLE (IDX1)
      *                        MOVE    WRK-KOHNAME
      *                                    TO  Q01-LBLORDER (IDX1)
      *                    END-IF
      *                END-IF
      *            END-IF
      *        END-IF
      *
           END-PERFORM
      *
      *    検索条件AND/OR切り替えラジオボタン
           MOVE    SPA-GMN-Q01-RDHKNKOH-AND    TO  Q01-RDHKNKOH-AND
           MOVE    SPA-GMN-Q01-RDHKNKOH-OR     TO  Q01-RDHKNKOH-OR
           MOVE    SPA-GMN-Q01-RDKOHNUM-AND    TO  Q01-RDKOHNUM-AND
           MOVE    SPA-GMN-Q01-RDKOHNUM-OR     TO  Q01-RDKOHNUM-OR
      *
           MOVE    SPA-GMN-Q01-RDBYOMEI-AND    TO  Q01-RDBYOMEI-AND
           MOVE    SPA-GMN-Q01-RDBYOMEI-OR     TO  Q01-RDBYOMEI-OR
      *
           MOVE    SPA-GMN-Q01-RDSRYACT-AND    TO  Q01-RDSRYACT-AND
           MOVE    SPA-GMN-Q01-RDSRYACT-OR     TO  Q01-RDSRYACT-OR
      *
           MOVE    WIDGET-NORMAL           TO  Q01-B05S-STATE
                                               Q01-B06S-STATE
                                               Q01-B07S-STATE
                                               Q01-STTENKIYMD-STATE
                                               Q01-EDTENKIYMD-STATE
                                               Q01-SRYSYUKBN-STATE
                                               Q01-CMB-SRYSYUKBN-STATE
      *
           IF  ( SPA-NAI-Q01-SELPATTERN-UID    =   SPACE )
               MOVE    WIDGET-INSENSITIVE      TO  Q01-B05S-STATE
                                                   Q01-B06S-STATE
           END-IF
      *
           IF  ( SPA-GMN-Q01-SELPATTERNLST-MAX >=  CONST-SELPATTERN-MAX)
               MOVE    WIDGET-INSENSITIVE      TO  Q01-B07S-STATE
           END-IF
      *
           IF  ( SPA-GMN-Q01-TENKIKBN  =   "1"
                                        OR "2"
                                        OR "3"
                                        OR "8"
                                        OR "9"
                                               )
               CONTINUE
           ELSE
      *
               MOVE    WIDGET-INSENSITIVE      TO  Q01-STTENKIYMD-STATE
                                                   Q01-EDTENKIYMD-STATE
      *
           END-IF
      *
           IF    ( SPA-GMN-Q01-SRYKBN  =   "20" OR "30" OR "90" )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  Q01-SRYSYUKBN-STATE
                                               Q01-CMB-SRYSYUKBN-STATE
           END-IF
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
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-Q01-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
      *
           END-IF
      *
           PERFORM 5012-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-Q01-CUR
      *
      *    診療年月日
           WHEN   001
               MOVE  "STSRYYMD"    TO  MCP-WIDGET
           WHEN   002
               MOVE  "EDSRYYMD"    TO  MCP-WIDGET
      *
      *    検索条件
           WHEN   003
               MOVE  "SELPATTERN"  TO  MCP-WIDGET
      *
      *    基本情報タブ
           WHEN   101
               MOVE  "STBIRTHDAY"  TO  MCP-WIDGET
           WHEN   102
               MOVE  "EDBIRTHDAY"  TO  MCP-WIDGET
           WHEN   103
               MOVE  "STAGE"       TO  MCP-WIDGET
           WHEN   104
               MOVE  "EDAGE"       TO  MCP-WIDGET
           WHEN   105
               MOVE  "KJYNYMD"     TO  MCP-WIDGET
           WHEN   106
               MOVE  "SEX"         TO  MCP-WIDGET
           WHEN   107
               MOVE  "STLASTYMD"   TO  MCP-WIDGET
           WHEN   108
               MOVE  "EDLASTYMD"   TO  MCP-WIDGET
           WHEN   144
               MOVE  "NYUGAIKBN"   TO  MCP-WIDGET
           WHEN   109
               MOVE  "SRYKA"       TO  MCP-WIDGET
           WHEN   110
               MOVE  "STPTNUM"     TO  MCP-WIDGET
           WHEN   111
               MOVE  "EDPTNUM"     TO  MCP-WIDGET
           WHEN   112
               MOVE  "POST1"       TO  MCP-WIDGET
           WHEN   113
               MOVE  "JYUSHO1"     TO  MCP-WIDGET
           WHEN   114
               MOVE  "POST2"       TO  MCP-WIDGET
           WHEN   115
               MOVE  "JYUSHO2"     TO  MCP-WIDGET
           WHEN   116
               MOVE  "POST3"       TO  MCP-WIDGET
           WHEN   117
               MOVE  "JYUSHO3"     TO  MCP-WIDGET
           WHEN   118
               MOVE  "POST4"       TO  MCP-WIDGET
           WHEN   119
               MOVE  "JYUSHO4"     TO  MCP-WIDGET
           WHEN   120
               MOVE  "POST5"       TO  MCP-WIDGET
           WHEN   121
               MOVE  "JYUSHO5"     TO  MCP-WIDGET
           WHEN   122
               MOVE  "KANANAME1"   TO  MCP-WIDGET
           WHEN   123
               MOVE  "KANANAME2"   TO  MCP-WIDGET
           WHEN   124
               MOVE  "KANANAME3"   TO  MCP-WIDGET
           WHEN   125
               MOVE  "KANANAME4"   TO  MCP-WIDGET
           WHEN   126
               MOVE  "KANANAME5"   TO  MCP-WIDGET
           WHEN   127
               MOVE  "NAME1"       TO  MCP-WIDGET
           WHEN   128
               MOVE  "NAME2"       TO  MCP-WIDGET
           WHEN   129
               MOVE  "NAME3"       TO  MCP-WIDGET
           WHEN   130
               MOVE  "NAME4"       TO  MCP-WIDGET
           WHEN   131
               MOVE  "NAME5"       TO  MCP-WIDGET
           WHEN   132
               MOVE  "CONDITION11" TO  MCP-WIDGET
           WHEN   133
               MOVE  "CONDITION12" TO  MCP-WIDGET
           WHEN   134
               MOVE  "CONDITION13" TO  MCP-WIDGET
           WHEN   135
               MOVE  "CONDITION21" TO  MCP-WIDGET
           WHEN   136
               MOVE  "CONDITION22" TO  MCP-WIDGET
           WHEN   137
               MOVE  "CONDITION23" TO  MCP-WIDGET
           WHEN   138
               MOVE  "CONDITION31" TO  MCP-WIDGET
           WHEN   139
               MOVE  "CONDITION32" TO  MCP-WIDGET
           WHEN   140
               MOVE  "CONDITION33" TO  MCP-WIDGET
           WHEN   141
               MOVE  "COMMENT1"    TO  MCP-WIDGET
           WHEN   142
               MOVE  "COMMENT2"    TO  MCP-WIDGET
           WHEN   143
               MOVE  "COMMENT3"    TO  MCP-WIDGET
           WHEN   145
               MOVE  "TEL_TO"      TO  MCP-WIDGET
           WHEN   146
               MOVE  "TEL1"        TO  MCP-WIDGET
           WHEN   147
               MOVE  "TEL2"        TO  MCP-WIDGET
           WHEN   148
               MOVE  "TEL3"        TO  MCP-WIDGET
           WHEN   149
               MOVE  "GENMEN1"     TO  MCP-WIDGET
           WHEN   150
               MOVE  "GENMEN2"     TO  MCP-WIDGET
           WHEN   151
               MOVE  "GENMEN3"     TO  MCP-WIDGET
           WHEN   152
               MOVE  "TSY"         TO  MCP-WIDGET
           WHEN   153
               MOVE  "STTSYSKBT"   TO  MCP-WIDGET
           WHEN   154
               MOVE  "STTSYYM"     TO  MCP-WIDGET
           WHEN   155
               MOVE  "EDTSYYM"     TO  MCP-WIDGET
           WHEN   156
               MOVE  "DEATH"       TO  MCP-WIDGET
           WHEN   157
               MOVE  "SELCOMMENT"  TO  MCP-WIDGET
           WHEN   158
               MOVE  "BIRTHMON"    TO  MCP-WIDGET
           WHEN   159
               MOVE  "TOKKIINFO"   TO  MCP-WIDGET
           WHEN   160
               MOVE  "TOKKICD"     TO  MCP-WIDGET
           WHEN   161
               MOVE  "TOKKINAME"   TO  MCP-WIDGET
           WHEN   163
               MOVE  "SAIGAIKBN"   TO  MCP-WIDGET
      *
      *    保険・公費タブ
           WHEN   201
               MOVE  "HONKZKKBN"   TO  MCP-WIDGET
           WHEN   202
               MOVE  "HKNJANUM1"   TO  MCP-WIDGET
           WHEN   203
               MOVE  "HKNJANUM2"   TO  MCP-WIDGET
           WHEN   204
               MOVE  "HKNJANUM3"   TO  MCP-WIDGET
           WHEN   205
               MOVE  "HKNJANUM4"   TO  MCP-WIDGET
           WHEN   206
               MOVE  "HKNJANUM5"   TO  MCP-WIDGET
           WHEN   207
               MOVE  "HKNNUM1"     TO  MCP-WIDGET
           WHEN   208
               MOVE  "HKNNUM2"     TO  MCP-WIDGET
           WHEN   209
               MOVE  "HKNNUM3"     TO  MCP-WIDGET
           WHEN   210
               MOVE  "HKNNUM4"     TO  MCP-WIDGET
           WHEN   211
               MOVE  "HKNNUM5"     TO  MCP-WIDGET
           WHEN   212
               MOVE  "KIGO1"       TO  MCP-WIDGET
           WHEN   213
               MOVE  "NUM1"        TO  MCP-WIDGET
           WHEN   214
               MOVE  "KIGO2"       TO  MCP-WIDGET
           WHEN   215
               MOVE  "NUM2"        TO  MCP-WIDGET
           WHEN   216
               MOVE  "KIGO3"       TO  MCP-WIDGET
           WHEN   217
               MOVE  "NUM3"        TO  MCP-WIDGET
           WHEN   218
               MOVE  "KIGO4"       TO  MCP-WIDGET
           WHEN   219
               MOVE  "NUM4"        TO  MCP-WIDGET
           WHEN   220
               MOVE  "KIGO5"       TO  MCP-WIDGET
           WHEN   221
               MOVE  "NUM5"        TO  MCP-WIDGET
           WHEN   222
               MOVE  "KOHNUM1"     TO  MCP-WIDGET
           WHEN   223
               MOVE  "KOHNUM2"     TO  MCP-WIDGET
           WHEN   224
               MOVE  "KOHNUM3"     TO  MCP-WIDGET
           WHEN   225
               MOVE  "KOHNUM4"     TO  MCP-WIDGET
           WHEN   226
               MOVE  "KOHNUM5"     TO  MCP-WIDGET
           WHEN   227
               MOVE  "FTNJANUM1"   TO  MCP-WIDGET
           WHEN   228
               MOVE  "FTNJANUM2"   TO  MCP-WIDGET
           WHEN   229
               MOVE  "FTNJANUM3"   TO  MCP-WIDGET
           WHEN   230
               MOVE  "FTNJANUM4"   TO  MCP-WIDGET
           WHEN   231
               MOVE  "FTNJANUM5"   TO  MCP-WIDGET
      *
      *    病名・診療行為タブ
           WHEN   301
               MOVE  "BYOBUN"      TO  MCP-WIDGET
           WHEN   302
               MOVE  "BYOMEISEL"   TO  MCP-WIDGET
           WHEN   303
               MOVE  "BYOMEICD1"   TO  MCP-WIDGET
           WHEN   304
               MOVE  "BYOMEI1"     TO  MCP-WIDGET
           WHEN   305
               MOVE  "BYOMEICD2"   TO  MCP-WIDGET
           WHEN   306
               MOVE  "BYOMEI2"     TO  MCP-WIDGET
           WHEN   307
               MOVE  "BYOMEICD3"   TO  MCP-WIDGET
           WHEN   308
               MOVE  "BYOMEI3"     TO  MCP-WIDGET
           WHEN   309
               MOVE  "BYOMEICD4"   TO  MCP-WIDGET
           WHEN   310
               MOVE  "BYOMEI4"     TO  MCP-WIDGET
           WHEN   311
               MOVE  "BYOMEICD5"   TO  MCP-WIDGET
           WHEN   312
               MOVE  "BYOMEI5"     TO  MCP-WIDGET
           WHEN   313
               MOVE  "SRYACTSEL"   TO  MCP-WIDGET
           WHEN   314
               MOVE  "SRYCD1"      TO  MCP-WIDGET
           WHEN   315
               MOVE  "SRYCD2"      TO  MCP-WIDGET
           WHEN   316
               MOVE  "SRYCD3"      TO  MCP-WIDGET
           WHEN   317
               MOVE  "SRYCD4"      TO  MCP-WIDGET
           WHEN   318
               MOVE  "SRYCD5"      TO  MCP-WIDGET
           WHEN   319
               MOVE  "UTAGAIKBN"   TO  MCP-WIDGET
           WHEN   320
               MOVE  "TENKIKBN"    TO  MCP-WIDGET
           WHEN   321
               MOVE  "STTENKIYMD"  TO  MCP-WIDGET
           WHEN   322
               MOVE  "EDTENKIYMD"  TO  MCP-WIDGET
           WHEN   323
               MOVE  "SRYKBN"      TO  MCP-WIDGET
           WHEN   324
               MOVE  "SRYSYUKBN"   TO  MCP-WIDGET
           WHEN   325
               MOVE  "KYUSEIKBN"   TO  MCP-WIDGET
           WHEN   326
               MOVE  "SYUBYOKBN"   TO  MCP-WIDGET
           WHEN   327
               MOVE  "RECECOM1"    TO  MCP-WIDGET
           WHEN   328
               MOVE  "RECECOM2"    TO  MCP-WIDGET
           WHEN   329
               MOVE  "RECECOM3"    TO  MCP-WIDGET
      *
      *    並び順タブ
           WHEN   401
               MOVE  "KMK1"        TO  MCP-WIDGET
           WHEN   402
               MOVE  "KMK2"        TO  MCP-WIDGET
           WHEN   403
               MOVE  "KMK3"        TO  MCP-WIDGET
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
      *    検索条件
           WHEN   "SELPATTERN"
               MOVE  001           TO  SPA-GMN-Q01-CUR
      *
      *    診療年月日
           WHEN   "STSRYYMD"
               MOVE  002           TO  SPA-GMN-Q01-CUR
           WHEN   "EDSRYYMD"
               EVALUATE    SPA-GMN-Q01-PAGENO
               WHEN    ZERO
                   MOVE    101     TO  SPA-GMN-Q01-CUR
               WHEN    1
                   MOVE    201     TO  SPA-GMN-Q01-CUR
               WHEN    2
                   MOVE    301     TO  SPA-GMN-Q01-CUR
               WHEN    3
                   MOVE    401     TO  SPA-GMN-Q01-CUR
               END-EVALUATE
      *
      *    基本情報タブ
           WHEN   "STBIRTHDAY"
               MOVE  102           TO  SPA-GMN-Q01-CUR
           WHEN   "EDBIRTHDAY"
               MOVE  103           TO  SPA-GMN-Q01-CUR
           WHEN   "STAGE"
               MOVE  104           TO  SPA-GMN-Q01-CUR
           WHEN   "EDAGE"
               MOVE  105           TO  SPA-GMN-Q01-CUR
           WHEN   "KJYNYMD"
               MOVE  158           TO  SPA-GMN-Q01-CUR
           WHEN   "BIRTHMON"
               MOVE  106           TO  SPA-GMN-Q01-CUR
           WHEN   "SEX"
               MOVE  107           TO  SPA-GMN-Q01-CUR
           WHEN   "STLASTYMD"
               MOVE  108           TO  SPA-GMN-Q01-CUR
           WHEN   "EDLASTYMD"
               MOVE  144           TO  SPA-GMN-Q01-CUR
           WHEN   "NYUGAIKBN"
               MOVE  109           TO  SPA-GMN-Q01-CUR
           WHEN   "SRYKA"
               MOVE  156           TO  SPA-GMN-Q01-CUR
           WHEN   "DEATH"
               MOVE  110           TO  SPA-GMN-Q01-CUR
           WHEN   "STPTNUM"
               MOVE  111           TO  SPA-GMN-Q01-CUR
           WHEN   "EDPTNUM"
               MOVE  127           TO  SPA-GMN-Q01-CUR
           WHEN   "POST1"
               MOVE  113           TO  SPA-GMN-Q01-CUR
           WHEN   "JYUSHO1"
               MOVE  114           TO  SPA-GMN-Q01-CUR
           WHEN   "POST2"
               MOVE  115           TO  SPA-GMN-Q01-CUR
           WHEN   "JYUSHO2"
               MOVE  116           TO  SPA-GMN-Q01-CUR
           WHEN   "POST3"
               MOVE  117           TO  SPA-GMN-Q01-CUR
           WHEN   "JYUSHO3"
               MOVE  118           TO  SPA-GMN-Q01-CUR
           WHEN   "POST4"
               MOVE  119           TO  SPA-GMN-Q01-CUR
           WHEN   "JYUSHO4"
               MOVE  120           TO  SPA-GMN-Q01-CUR
           WHEN   "POST5"
               MOVE  121           TO  SPA-GMN-Q01-CUR
           WHEN   "JYUSHO5"
               MOVE  145           TO  SPA-GMN-Q01-CUR
           WHEN   "NAME1"
               MOVE  128           TO  SPA-GMN-Q01-CUR
           WHEN   "NAME2"
               MOVE  129           TO  SPA-GMN-Q01-CUR
           WHEN   "NAME3"
               MOVE  130           TO  SPA-GMN-Q01-CUR
           WHEN   "NAME4"
               MOVE  131           TO  SPA-GMN-Q01-CUR
           WHEN   "NAME5"
               MOVE  112           TO  SPA-GMN-Q01-CUR
           WHEN   "CONDITION11"
               MOVE  133           TO  SPA-GMN-Q01-CUR
           WHEN   "CONDITION12"
               MOVE  134           TO  SPA-GMN-Q01-CUR
           WHEN   "CONDITION13"
               MOVE  135           TO  SPA-GMN-Q01-CUR
           WHEN   "CONDITION21"
               MOVE  136           TO  SPA-GMN-Q01-CUR
           WHEN   "CONDITION22"
               MOVE  137           TO  SPA-GMN-Q01-CUR
           WHEN   "CONDITION23"
               MOVE  138           TO  SPA-GMN-Q01-CUR
           WHEN   "CONDITION31"
               MOVE  139           TO  SPA-GMN-Q01-CUR
           WHEN   "CONDITION32"
               MOVE  140           TO  SPA-GMN-Q01-CUR
           WHEN   "CONDITION33"
               MOVE  157           TO  SPA-GMN-Q01-CUR
           WHEN   "COMMENT1"
               MOVE  142           TO  SPA-GMN-Q01-CUR
           WHEN   "COMMENT2"
               MOVE  143           TO  SPA-GMN-Q01-CUR
           WHEN   "COMMENT3"
               MOVE  149           TO  SPA-GMN-Q01-CUR
           WHEN   "TEL_TO"
               MOVE  146           TO  SPA-GMN-Q01-CUR
           WHEN   "TEL1"
               MOVE  147           TO  SPA-GMN-Q01-CUR
           WHEN   "TEL2"
               MOVE  148           TO  SPA-GMN-Q01-CUR
           WHEN   "TEL3"
               MOVE  132           TO  SPA-GMN-Q01-CUR
           WHEN   "GENMEN1"
               MOVE  150           TO  SPA-GMN-Q01-CUR
           WHEN   "GENMEN2"
               MOVE  151           TO  SPA-GMN-Q01-CUR
           WHEN   "GENMEN3"
               MOVE  152           TO  SPA-GMN-Q01-CUR
           WHEN   "SELCOMMENT"
               MOVE  141           TO  SPA-GMN-Q01-CUR
           WHEN   "TSY"
               MOVE  153           TO  SPA-GMN-Q01-CUR
           WHEN   "STTSYSKBT"
               MOVE  154           TO  SPA-GMN-Q01-CUR
           WHEN   "STTSYYM"
               MOVE  155           TO  SPA-GMN-Q01-CUR
           WHEN   "EDTSYYM"
               MOVE  160           TO  SPA-GMN-Q01-CUR
           WHEN   "TOKKICD"
               MOVE  161           TO  SPA-GMN-Q01-CUR
           WHEN   "TOKKINAME"
               MOVE  159           TO  SPA-GMN-Q01-CUR
           WHEN   "TOKKIINFO"
               MOVE  163           TO  SPA-GMN-Q01-CUR
           WHEN   "SAIGAIKBN"
               MOVE  001           TO  SPA-GMN-Q01-CUR
      *
      *    保険・公費タブ
           WHEN   "HONKZKKBN"
               MOVE  202           TO  SPA-GMN-Q01-CUR
           WHEN   "HKNJANUM1"
               MOVE  203           TO  SPA-GMN-Q01-CUR
           WHEN   "HKNJANUM2"
               MOVE  204           TO  SPA-GMN-Q01-CUR
           WHEN   "HKNJANUM3"
               MOVE  205           TO  SPA-GMN-Q01-CUR
           WHEN   "HKNJANUM4"
               MOVE  206           TO  SPA-GMN-Q01-CUR
           WHEN   "HKNJANUM5"
               MOVE  207           TO  SPA-GMN-Q01-CUR
           WHEN   "HKNNUM1"
               MOVE  208           TO  SPA-GMN-Q01-CUR
           WHEN   "HKNNUM2"
               MOVE  209           TO  SPA-GMN-Q01-CUR
           WHEN   "HKNNUM3"
               MOVE  210           TO  SPA-GMN-Q01-CUR
           WHEN   "HKNNUM4"
               MOVE  211           TO  SPA-GMN-Q01-CUR
           WHEN   "HKNNUM5"
               MOVE  212           TO  SPA-GMN-Q01-CUR
           WHEN   "KIGO1"
               MOVE  213           TO  SPA-GMN-Q01-CUR
           WHEN   "NUM1"
               MOVE  214           TO  SPA-GMN-Q01-CUR
           WHEN   "KIGO2"
               MOVE  215           TO  SPA-GMN-Q01-CUR
           WHEN   "NUM2"
               MOVE  216           TO  SPA-GMN-Q01-CUR
           WHEN   "KIGO3"
               MOVE  217           TO  SPA-GMN-Q01-CUR
           WHEN   "NUM3"
               MOVE  218           TO  SPA-GMN-Q01-CUR
           WHEN   "KIGO4"
               MOVE  219           TO  SPA-GMN-Q01-CUR
           WHEN   "NUM4"
               MOVE  220           TO  SPA-GMN-Q01-CUR
           WHEN   "KIGO5"
               MOVE  221           TO  SPA-GMN-Q01-CUR
           WHEN   "NUM5"
               MOVE  222           TO  SPA-GMN-Q01-CUR
           WHEN   "KOHNUM1"
               MOVE  223           TO  SPA-GMN-Q01-CUR
           WHEN   "KOHNUM2"
               MOVE  224           TO  SPA-GMN-Q01-CUR
           WHEN   "KOHNUM3"
               MOVE  225           TO  SPA-GMN-Q01-CUR
           WHEN   "KOHNUM4"
               MOVE  226           TO  SPA-GMN-Q01-CUR
           WHEN   "KOHNUM5"
               MOVE  227           TO  SPA-GMN-Q01-CUR
           WHEN   "FTNJANUM1"
               MOVE  228           TO  SPA-GMN-Q01-CUR
           WHEN   "FTNJANUM2"
               MOVE  229           TO  SPA-GMN-Q01-CUR
           WHEN   "FTNJANUM3"
               MOVE  230           TO  SPA-GMN-Q01-CUR
           WHEN   "FTNJANUM4"
               MOVE  231           TO  SPA-GMN-Q01-CUR
           WHEN   "FTNJANUM5"
               MOVE  001           TO  SPA-GMN-Q01-CUR
      *
      *    病名・診療行為タブ
           WHEN   "BYOBUN"
               MOVE  302           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEISEL"
               MOVE  319           TO  SPA-GMN-Q01-CUR
           WHEN   "UTAGAIKBN"
               MOVE  325           TO  SPA-GMN-Q01-CUR
           WHEN   "KYUSEIKBN"
               MOVE  320           TO  SPA-GMN-Q01-CUR
           WHEN   "TENKIKBN"
               MOVE  321           TO  SPA-GMN-Q01-CUR
           WHEN   "STTENKIYMD"
               MOVE  322           TO  SPA-GMN-Q01-CUR
           WHEN   "EDTENKIYMD"
               MOVE  326           TO  SPA-GMN-Q01-CUR
           WHEN   "SYUBYOKBN"
               MOVE  303           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEICD1"
               MOVE  304           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEI1"
               MOVE  305           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEICD2"
               MOVE  306           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEI2"
               MOVE  307           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEICD3"
               MOVE  308           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEI3"
               MOVE  309           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEICD4"
               MOVE  310           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEI4"
               MOVE  311           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEICD5"
               MOVE  312           TO  SPA-GMN-Q01-CUR
           WHEN   "BYOMEI5"
               MOVE  313           TO  SPA-GMN-Q01-CUR
           WHEN   "SRYACTSEL"
               MOVE  323           TO  SPA-GMN-Q01-CUR
           WHEN   "SRYKBN"
               MOVE  324           TO  SPA-GMN-Q01-CUR
           WHEN   "SRYSYUKBN"
               MOVE  314           TO  SPA-GMN-Q01-CUR
           WHEN   "SRYCD1"
               MOVE  315           TO  SPA-GMN-Q01-CUR
           WHEN   "SRYCD2"
               MOVE  316           TO  SPA-GMN-Q01-CUR
           WHEN   "SRYCD3"
               MOVE  317           TO  SPA-GMN-Q01-CUR
           WHEN   "SRYCD4"
               MOVE  318           TO  SPA-GMN-Q01-CUR
           WHEN   "SRYCD5"
               MOVE  327           TO  SPA-GMN-Q01-CUR
           WHEN   "RECECOM1"
               MOVE  328           TO  SPA-GMN-Q01-CUR
           WHEN   "RECECOM2"
               MOVE  329           TO  SPA-GMN-Q01-CUR
           WHEN   "RECECOM3"
               MOVE  001           TO  SPA-GMN-Q01-CUR
      *
      *    並び順タブ
           WHEN   "KMK1"
               MOVE  402           TO  SPA-GMN-Q01-CUR
           WHEN   "KMK2"
               MOVE  403           TO  SPA-GMN-Q01-CUR
           WHEN   "KMK3"
               MOVE  001           TO  SPA-GMN-Q01-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-CUR-SKIP-SEC          SECTION.
      *
           IF    ( SPA-GMN-Q01-CUR =   301 )
               IF    ( SPA-GMN-Q01-UBYOMEI-STATE
                                   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-Q01-CUR =   302
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-Q01-CUR =   321 )
               IF    ( Q01-STTENKIYMD-STATE
                                   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-Q01-CUR =   322
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-Q01-CUR =   322 )
               IF    ( Q01-EDTENKIYMD-STATE
                                   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-Q01-CUR =   326
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-Q01-CUR =   155 )
               IF    ( Q01-EDTSYYM-STATE
                                   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-Q01-CUR =   001
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-Q01-CUR =   324 )
               IF    ( Q01-SRYSYUKBN-STATE
                                   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-Q01-CUR =   314
               END-IF
           END-IF
      *
           .
      *
       5012-CUR-SKIP-EXT.
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
               MOVE    "入力エラー"    TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "患者番号の桁数が違います" 
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "対象件数が２００件をオーバーしました" 
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "このコードの病名は登録されていません" 
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "このコードの行為名は登録されていません"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "未来日は入力できません"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "セットコードは入力できません"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "項目を設定してください"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "全角文字でのみ入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "診療年月日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE    "生年月日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE    "年齢入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE    "基準日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0105"
               MOVE    "最終受診療日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0106"
               MOVE    "患者番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0107"
               MOVE    "病名コード入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0108"
               MOVE    "診療行為コード入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0109"
               MOVE    "性別入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0110"
               MOVE    "保険の種類入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0111"
               MOVE    "公費の種類入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0112"
               MOVE    "項目名入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0113"
               MOVE    "本人家族入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0114"
               MOVE    "状態１入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0115"
               MOVE    "状態２入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0116"
               MOVE    "状態３入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0117"
               MOVE    "診療科入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0118"
               MOVE    "入外区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0119"
               MOVE    "疑い病名区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0120"
               MOVE    "転帰区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0121"
               MOVE    "開始転帰日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0122"
               MOVE    "終了転帰日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0123"
               MOVE    "減免事由入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0124"
               MOVE    "低所得１・２入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0125"
               MOVE    "低所得期間開始入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0126"
               MOVE    "低所得開始月識別入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0127"
               MOVE    "低所得期間終了入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0128"
               MOVE    "死亡区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0129"
               MOVE    "診療区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0130"
               MOVE    "診療種別入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0131"
               MOVE    "コメント選択入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0132"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "レセプトコメントのキーワード"  DELIMITED BY SIZE
                       "は３語までにしてください。"    DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0133"
               MOVE    "誕生月入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0134"
               MOVE    "急性病名区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0135"
               MOVE    "主病名区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0136"
               MOVE    "特記事項情報入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0137"
               MOVE    "特記事項コード入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0138"
               MOVE    "特記事項名称入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0143"
               MOVE    "災害該当入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "開始診療年月日＞終了診療年月日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "開始生年月日＞終了生年月日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "開始年齢＞終了年齢となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "生年月日と年齢は同時に入力できません"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "年齢を入力する場合、基準日は必須項目です"
                                       TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    "開始患者番号＞終了患者番号となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "検索条件が入力されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "開始最終受診日＞終了最終受診日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1009"
               MOVE    "開始転帰日＞終了転帰日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    "低所得の有効期間の指定に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "1011"
               MOVE    "低所得の開始月を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1012"
               MOVE    "低所得の終了月を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"      THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  QERR
           MOVE    SPA-ERRCD            TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  QERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Q01"                TO  SPA-MOTOPG
           MOVE    "QERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "QERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-QIDSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-QIDMSG
      *
           EVALUATE    SPA-QIDCD
           WHEN    "3002"
               MOVE    "選択中の検索条件を削除します"
                                       TO  WRK-QIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  QID1
           MOVE    SPA-QIDCD           TO  QID1-ID1CODE
           MOVE    WRK-QIDMSG          TO  QID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "Q01"               TO  SPA-MOTOPG
           MOVE    "QID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "QID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-QIDSET-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件情報より画面編集処理
      *****************************************************************
       700-GMNHEN-SEC                  SECTION.
      *
           MOVE    ZERO            TO  FLG-PAGENO-G
      *
           MOVE    SPACE           TO  Q01
           INITIALIZE                  Q01
      *
           PERFORM 900-SHOUKAIDATA-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SHOUKAIDATA NOT =   ZERO )
      *
               PERFORM 7001-GMNHEN-SEC
      *
               PERFORM 900-SHOUKAIDATA-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_shoukaidata"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       700-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件情報より画面編集処理
      *****************************************************************
       7001-GMNHEN-SEC                 SECTION.
      *
           EVALUATE    SHODATA-ITEMID
      *
           WHEN    SHKITEMID-STSRYYMD
               MOVE    SHODATA-ITEM    TO  Q01-STSRYYMD
           WHEN    SHKITEMID-EDSRYYMD
               MOVE    SHODATA-ITEM    TO  Q01-EDSRYYMD
           WHEN    SHKITEMID-STBIRTHDAY
               MOVE    SHODATA-ITEM    TO  Q01-STBIRTHDAY
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-EDBIRTHDAY
               MOVE    SHODATA-ITEM    TO  Q01-EDBIRTHDAY
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-STAGE
               MOVE    SHODATA-ITEM    TO  Q01-STAGE
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-EDAGE
               MOVE    SHODATA-ITEM    TO  Q01-EDAGE
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-BIRTHMON
               MOVE    SHODATA-ITEM    TO  Q01-BIRTHMON
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-SEX
               MOVE    SHODATA-ITEM    TO  Q01-SEX
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-STLASTYMD
               MOVE    SHODATA-ITEM    TO  Q01-STLASTYMD
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-EDLASTYMD
               MOVE    SHODATA-ITEM    TO  Q01-EDLASTYMD
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-NYUGAIKBN
               MOVE    SHODATA-ITEM    TO  Q01-NYUGAIKBN
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-SRYKA
               MOVE    SHODATA-ITEM    TO  Q01-SRYKA
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-DEATH
               MOVE    SHODATA-ITEM    TO  Q01-DEATH
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-STPTNUM
               MOVE    SHODATA-ITEM    TO  Q01-STPTNUM
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-EDPTNUM
               MOVE    SHODATA-ITEM    TO  Q01-EDPTNUM
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-CHKTSTPT
               IF    ( SHODATA-ITEM        =   "1" )
                   MOVE    "T"         TO  Q01-CHKTSTPT
                                           SPA-GMN-Q01-CHKTSTPT
               ELSE
                   MOVE    "F"         TO  Q01-CHKTSTPT
                                           SPA-GMN-Q01-CHKTSTPT
               END-IF
           WHEN    SHKITEMID-POST01
               MOVE    SHODATA-ITEM    TO  Q01-POST (1)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-POST02
               MOVE    SHODATA-ITEM    TO  Q01-POST (2)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-POST03
               MOVE    SHODATA-ITEM    TO  Q01-POST (3)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-POST04
               MOVE    SHODATA-ITEM    TO  Q01-POST (4)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-POST05
               MOVE    SHODATA-ITEM    TO  Q01-POST (5)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-JYUSHO01
               MOVE    SHODATA-ITEM    TO  Q01-JYUSHO (1)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-JYUSHO02
               MOVE    SHODATA-ITEM    TO  Q01-JYUSHO (2)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-JYUSHO03
               MOVE    1               TO  FLG-PAGENO-00
               MOVE    SHODATA-ITEM    TO  Q01-JYUSHO (3)
           WHEN    SHKITEMID-JYUSHO04
               MOVE    SHODATA-ITEM    TO  Q01-JYUSHO (4)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-JYUSHO05
               MOVE    SHODATA-ITEM    TO  Q01-JYUSHO (5)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-KANANAME01
               MOVE    SHODATA-ITEM    TO  Q01-NAME (1)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-KANANAME02
               MOVE    SHODATA-ITEM    TO  Q01-NAME (2)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-KANANAME03
               MOVE    SHODATA-ITEM    TO  Q01-NAME (3)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-KANANAME04
               MOVE    SHODATA-ITEM    TO  Q01-NAME (4)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-KANANAME05
               MOVE    SHODATA-ITEM    TO  Q01-NAME (5)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-NAME01
               IF    ( Q01-NAME (1)    =   SPACE )
                   MOVE    SHODATA-ITEM    TO  Q01-NAME (1)
                   MOVE    1               TO  FLG-PAGENO-00
               END-IF
           WHEN    SHKITEMID-NAME02
               IF    ( Q01-NAME (2)    =   SPACE )
                   MOVE    SHODATA-ITEM    TO  Q01-NAME (2)
                   MOVE    1               TO  FLG-PAGENO-00
               END-IF
           WHEN    SHKITEMID-NAME03
               IF    ( Q01-NAME (3)    =   SPACE )
                   MOVE    SHODATA-ITEM    TO  Q01-NAME (3)
                   MOVE    1               TO  FLG-PAGENO-00
               END-IF
           WHEN    SHKITEMID-NAME04
               IF    ( Q01-NAME (4)    =   SPACE )
                   MOVE    SHODATA-ITEM    TO  Q01-NAME (4)
                   MOVE    1               TO  FLG-PAGENO-00
               END-IF
           WHEN    SHKITEMID-NAME05
               IF    ( Q01-NAME (5)    =   SPACE )
                   MOVE    SHODATA-ITEM    TO  Q01-NAME (5)
                   MOVE    1               TO  FLG-PAGENO-00
               END-IF
           WHEN    SHKITEMID-CONDITION101
               MOVE    SHODATA-ITEM    TO  Q01-CONDITION1 (1)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-CONDITION102
               MOVE    SHODATA-ITEM    TO  Q01-CONDITION1 (2)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-CONDITION103
               MOVE    SHODATA-ITEM    TO  Q01-CONDITION1 (3)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-CONDITION201
               MOVE    SHODATA-ITEM    TO  Q01-CONDITION2 (1)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-CONDITION202
               MOVE    SHODATA-ITEM    TO  Q01-CONDITION2 (2)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-CONDITION203
               MOVE    SHODATA-ITEM    TO  Q01-CONDITION2 (3)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-CONDITION301
               MOVE    SHODATA-ITEM    TO  Q01-CONDITION3 (1)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-CONDITION302
               MOVE    SHODATA-ITEM    TO  Q01-CONDITION3 (2)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-CONDITION303
               MOVE    SHODATA-ITEM    TO  Q01-CONDITION3 (3)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-GENMEN01
               MOVE    SHODATA-ITEM    TO  Q01-GENMEN (1)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-GENMEN02
               MOVE    SHODATA-ITEM    TO  Q01-GENMEN (2)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-GENMEN03
               MOVE    SHODATA-ITEM    TO  Q01-GENMEN (3)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-SELCOMMENT
               MOVE    SHODATA-ITEM    TO  Q01-SELCOMMENT
           WHEN    SHKITEMID-COMMENT01
               MOVE    SHODATA-ITEM    TO  Q01-COMMENT (1)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-COMMENT02
               MOVE    SHODATA-ITEM    TO  Q01-COMMENT (2)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-COMMENT03
               MOVE    SHODATA-ITEM    TO  Q01-COMMENT (3)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-TEL-TO
               MOVE    SHODATA-ITEM    TO  Q01-TEL-TO
           WHEN    SHKITEMID-TEL01
               MOVE    SHODATA-ITEM    TO  Q01-TEL (1)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-TEL02
               MOVE    SHODATA-ITEM    TO  Q01-TEL (2)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-TEL03
               MOVE    SHODATA-ITEM    TO  Q01-TEL (3)
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-TSY
               MOVE    SHODATA-ITEM    TO  Q01-TSY
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-STTSYSKBT
               MOVE    SHODATA-ITEM    TO  Q01-STTSYSKBT
           WHEN    SHKITEMID-STTSYYM
               MOVE    SHODATA-ITEM    TO  Q01-STTSYYM
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-EDTSYYM
               MOVE    SHODATA-ITEM    TO  Q01-EDTSYYM
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-TOKKICD
               MOVE    SHODATA-ITEM    TO  Q01-TOKKICD
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-TOKKINAME
               MOVE    SHODATA-ITEM    TO  Q01-TOKKINAME
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-SAIGAIKBN
               MOVE    SHODATA-ITEM    TO  Q01-SAIGAIKBN
               MOVE    1               TO  FLG-PAGENO-00
           WHEN    SHKITEMID-HONKZKKBN
               MOVE    SHODATA-ITEM    TO  Q01-HONKZKKBN
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-HKNJANUM01
               MOVE    SHODATA-ITEM    TO  Q01-HKNJANUM (1)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-HKNJANUM02
               MOVE    SHODATA-ITEM    TO  Q01-HKNJANUM (2)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-HKNJANUM03
               MOVE    SHODATA-ITEM    TO  Q01-HKNJANUM (3)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-HKNJANUM04
               MOVE    SHODATA-ITEM    TO  Q01-HKNJANUM (4)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-HKNJANUM05
               MOVE    SHODATA-ITEM    TO  Q01-HKNJANUM (5)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-HKNNUM01
               MOVE    SHODATA-ITEM    TO  Q01-HKNNUM (1)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-HKNNUM02
               MOVE    SHODATA-ITEM    TO  Q01-HKNNUM (2)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-HKNNUM03
               MOVE    SHODATA-ITEM    TO  Q01-HKNNUM (3)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-HKNNUM04
               MOVE    SHODATA-ITEM    TO  Q01-HKNNUM (4)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-HKNNUM05
               MOVE    SHODATA-ITEM    TO  Q01-HKNNUM (5)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-KIGO01
               MOVE    SHODATA-ITEM    TO  Q01-KIGO (1)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-KIGO02
               MOVE    SHODATA-ITEM    TO  Q01-KIGO (2)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-KIGO03
               MOVE    SHODATA-ITEM    TO  Q01-KIGO (3)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-KIGO04
               MOVE    SHODATA-ITEM    TO  Q01-KIGO (4)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-KIGO05
               MOVE    SHODATA-ITEM    TO  Q01-KIGO (5)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-NUM01
               MOVE    SHODATA-ITEM    TO  Q01-NUM (1)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-NUM02
               MOVE    SHODATA-ITEM    TO  Q01-NUM (2)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-NUM03
               MOVE    SHODATA-ITEM    TO  Q01-NUM (3)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-NUM04
               MOVE    SHODATA-ITEM    TO  Q01-NUM (4)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-NUM05
               MOVE    SHODATA-ITEM    TO  Q01-NUM (5)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-RDHKNKOH
               IF    ( SHODATA-ITEM    =   "1" )
                   MOVE    "T"         TO  Q01-RDHKNKOH-AND
                                           SPA-GMN-Q01-RDHKNKOH-AND
                   MOVE    "F"         TO  Q01-RDHKNKOH-OR
                                           SPA-GMN-Q01-RDHKNKOH-OR
               ELSE
                   MOVE    "F"         TO  Q01-RDHKNKOH-AND
                                           SPA-GMN-Q01-RDHKNKOH-AND
                   MOVE    "T"         TO  Q01-RDHKNKOH-OR
                                           SPA-GMN-Q01-RDHKNKOH-OR
               END-IF
           WHEN    SHKITEMID-RDKOHNUM
               IF    ( SHODATA-ITEM    =   "1" )
                   MOVE    "T"         TO  Q01-RDKOHNUM-AND
                                           SPA-GMN-Q01-RDKOHNUM-AND
                   MOVE    "F"         TO  Q01-RDKOHNUM-OR
                                           SPA-GMN-Q01-RDKOHNUM-OR
               ELSE
                   MOVE    "F"         TO  Q01-RDKOHNUM-AND
                                           SPA-GMN-Q01-RDKOHNUM-AND
                   MOVE    "T"         TO  Q01-RDKOHNUM-OR
                                           SPA-GMN-Q01-RDKOHNUM-OR
               END-IF
           WHEN    SHKITEMID-KOHNUM01
               MOVE    SHODATA-ITEM    TO  Q01-KOHNUM (1)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-KOHNUM02
               MOVE    SHODATA-ITEM    TO  Q01-KOHNUM (2)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-KOHNUM03
               MOVE    SHODATA-ITEM    TO  Q01-KOHNUM (3)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-KOHNUM04
               MOVE    SHODATA-ITEM    TO  Q01-KOHNUM (4)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-KOHNUM05
               MOVE    SHODATA-ITEM    TO  Q01-KOHNUM (5)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-FTNJANUM01
               MOVE    SHODATA-ITEM    TO  Q01-FTNJANUM (1)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-FTNJANUM02
               MOVE    SHODATA-ITEM    TO  Q01-FTNJANUM (2)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-FTNJANUM03
               MOVE    SHODATA-ITEM    TO  Q01-FTNJANUM (3)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-FTNJANUM04
               MOVE    SHODATA-ITEM    TO  Q01-FTNJANUM (4)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-FTNJANUM05
               MOVE    SHODATA-ITEM    TO  Q01-FTNJANUM (5)
               MOVE    1               TO  FLG-PAGENO-01
           WHEN    SHKITEMID-UTAGAIKBN
               MOVE    SHODATA-ITEM    TO  Q01-UTAGAIKBN
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-KYUSEIKBN
               MOVE    SHODATA-ITEM    TO  Q01-KYUSEIKBN
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-TENKIKBN
               MOVE    SHODATA-ITEM    TO  Q01-TENKIKBN
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-STTENKIYMD
               MOVE    SHODATA-ITEM    TO  Q01-STTENKIYMD
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-EDTENKIYMD
               MOVE    SHODATA-ITEM    TO  Q01-EDTENKIYMD
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-SYUBYOKBN
               MOVE    SHODATA-ITEM    TO  Q01-SYUBYOKBN
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-RDBYOMEI
               IF    ( SHODATA-ITEM    =   "1" )
                   MOVE    "T"         TO  Q01-RDBYOMEI-AND
                                           SPA-GMN-Q01-RDBYOMEI-AND
                   MOVE    "F"         TO  Q01-RDBYOMEI-OR
                                           SPA-GMN-Q01-RDBYOMEI-OR
               ELSE
                   MOVE    "F"         TO  Q01-RDBYOMEI-AND
                                           SPA-GMN-Q01-RDBYOMEI-AND
                   MOVE    "T"         TO  Q01-RDBYOMEI-OR
                                           SPA-GMN-Q01-RDBYOMEI-OR
               END-IF
           WHEN    SHKITEMID-BYOMEICD01
               MOVE    SHODATA-ITEM    TO  Q01-BYOMEICD (1)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-BYOMEICD02
               MOVE    SHODATA-ITEM    TO  Q01-BYOMEICD (2)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-BYOMEICD03
               MOVE    SHODATA-ITEM    TO  Q01-BYOMEICD (3)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-BYOMEICD04
               MOVE    SHODATA-ITEM    TO  Q01-BYOMEICD (4)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-BYOMEICD05
               MOVE    SHODATA-ITEM    TO  Q01-BYOMEICD (5)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-BYOMEI01
               MOVE    SHODATA-ITEM    TO  Q01-BYOMEI   (1)
                                           SPA-NAI-Q01-BYOMEI-OLD 
                                                        (1)

               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-BYOMEI02
               MOVE    SHODATA-ITEM    TO  Q01-BYOMEI   (2)
                                           SPA-NAI-Q01-BYOMEI-OLD 
                                                        (2)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-BYOMEI03
               MOVE    SHODATA-ITEM    TO  Q01-BYOMEI   (3)
                                           SPA-NAI-Q01-BYOMEI-OLD 
                                                        (3)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-BYOMEI04
               MOVE    SHODATA-ITEM    TO  Q01-BYOMEI   (4)
                                           SPA-NAI-Q01-BYOMEI-OLD 
                                                        (4)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-BYOMEI05
               MOVE    SHODATA-ITEM    TO  Q01-BYOMEI   (5)
                                           SPA-NAI-Q01-BYOMEI-OLD 
                                                        (5)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-SRYKBN
               MOVE    SHODATA-ITEM    TO  Q01-SRYKBN
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-SRYSYUKBN
               MOVE    SHODATA-ITEM    TO  Q01-SRYSYUKBN
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-RDSRYACT
               IF    ( SHODATA-ITEM    =   "1" )
                   MOVE    "T"         TO  Q01-RDSRYACT-AND
                                           SPA-GMN-Q01-RDSRYACT-AND
                   MOVE    "F"         TO  Q01-RDSRYACT-OR
                                           SPA-GMN-Q01-RDSRYACT-OR
               ELSE
                   MOVE    "F"         TO  Q01-RDSRYACT-AND
                                           SPA-GMN-Q01-RDSRYACT-AND
                   MOVE    "T"         TO  Q01-RDSRYACT-OR
                                           SPA-GMN-Q01-RDSRYACT-OR
               END-IF
           WHEN    SHKITEMID-SRYCD01
               MOVE    SHODATA-ITEM    TO  Q01-SRYCD (1)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-SRYCD02
               MOVE    SHODATA-ITEM    TO  Q01-SRYCD (2)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-SRYCD03
               MOVE    SHODATA-ITEM    TO  Q01-SRYCD (3)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-SRYCD04
               MOVE    SHODATA-ITEM    TO  Q01-SRYCD (4)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-SRYCD05
               MOVE    SHODATA-ITEM    TO  Q01-SRYCD (5)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-RECECOM01
               MOVE    SHODATA-ITEM    TO  Q01-RECECOM (1)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-RECECOM02
               MOVE    SHODATA-ITEM    TO  Q01-RECECOM (2)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-RECECOM03
               MOVE    SHODATA-ITEM    TO  Q01-RECECOM (3)
               MOVE    1               TO  FLG-PAGENO-02
           WHEN    SHKITEMID-KMK01
               MOVE    SHODATA-ITEM    TO  Q01-CMB-KMK (1)
           WHEN    SHKITEMID-KMK02
               MOVE    SHODATA-ITEM    TO  Q01-CMB-KMK (2)
           WHEN    SHKITEMID-KMK03
               MOVE    SHODATA-ITEM    TO  Q01-CMB-KMK (3)
           WHEN    SHKITEMID-RDIO01
               IF    ( SHODATA-ITEM    =   "1" )
                   MOVE    "T"         TO  Q01-RDIO1 (1)
                                           SPA-GMN-Q01-RDIO1 (1)
                   MOVE    "F"         TO  Q01-RDIO2 (1)
                                           SPA-GMN-Q01-RDIO2 (1)
               ELSE
                   MOVE    "F"         TO  Q01-RDIO1 (1)
                                           SPA-GMN-Q01-RDIO1 (1)
                   MOVE    "T"         TO  Q01-RDIO2 (1)
                                           SPA-GMN-Q01-RDIO2 (1)
               END-IF
           WHEN    SHKITEMID-RDIO02
               IF    ( SHODATA-ITEM    =   "1" )
                   MOVE    "T"         TO  Q01-RDIO1 (2)
                                           SPA-GMN-Q01-RDIO1 (2)
                   MOVE    "F"         TO  Q01-RDIO2 (2)
                                           SPA-GMN-Q01-RDIO2 (2)
               ELSE
                   MOVE    "F"         TO  Q01-RDIO1 (2)
                                           SPA-GMN-Q01-RDIO1 (2)
                   MOVE    "T"         TO  Q01-RDIO2 (2)
                                           SPA-GMN-Q01-RDIO2 (2)
               END-IF
           WHEN    SHKITEMID-RDIO03
               IF    ( SHODATA-ITEM    =   "1" )
                   MOVE    "T"         TO  Q01-RDIO1 (3)
                                           SPA-GMN-Q01-RDIO1 (3)
                   MOVE    "F"         TO  Q01-RDIO2 (3)
                                           SPA-GMN-Q01-RDIO2 (3)
               ELSE
                   MOVE    "F"         TO  Q01-RDIO1 (3)
                                           SPA-GMN-Q01-RDIO1 (3)
                   MOVE    "T"         TO  Q01-RDIO2 (3)
                                           SPA-GMN-Q01-RDIO2 (3)
               END-IF
      *
           END-EVALUATE
      *
           .
       7001-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件情報より画面編集処理
      *****************************************************************
       700-SELPATTERN-INS-SEC          SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
           MOVE    C-UID           TO  WRK-UID
      *
           PERFORM 7001-SHOUKAIDATA-INS-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 7001-SHOUKAIKANRI-INS-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               PERFORM 3101-SELPATTERN-CMB-SYOKI-SEC
      *
               MOVE    WRK-UID     TO  WRK-CMB-CD
               PERFORM 4201-SELPATTERNLST-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-Q01-SELPATTERN-G
               MOVE    WRK-CMB-ITM2 TO SPA-NAI-Q01-SELPATTERN-UID
           END-IF
      *
           .
       700-SELPATTERN-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件登録処理
      *****************************************************************
       7001-SHOUKAIDATA-INS-SEC        SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                      SHOUKAIDATA-REC
           MOVE    SPA-HOSPNUM         TO  SHODATA-HOSPNUM
           MOVE    WRK-UID             TO  SHODATA-UID
      *
           IF    ( SPA-NAI-Q01-STSRYYMD    NOT =   SPACE )
               MOVE    SHKITEMID-STSRYYMD
                                   TO  SHODATA-ITEMID
               MOVE    SPA-NAI-Q01-STSRYYMD
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-NAI-Q01-EDSRYYMD    NOT =   SPACE )
               MOVE    SHKITEMID-EDSRYYMD
                                   TO  SHODATA-ITEMID
               MOVE    SPA-NAI-Q01-EDSRYYMD
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-NAI-Q01-STBIRTHDAY  NOT =   SPACE )
               MOVE    SHKITEMID-STBIRTHDAY
                                   TO  SHODATA-ITEMID
               MOVE    SPA-NAI-Q01-STBIRTHDAY
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-NAI-Q01-EDBIRTHDAY  NOT =   SPACE )
               MOVE    SHKITEMID-EDBIRTHDAY
                                   TO  SHODATA-ITEMID
               MOVE    SPA-NAI-Q01-EDBIRTHDAY
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-STAGE   NOT =   SPACE )
               MOVE    SHKITEMID-STAGE
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-STAGE
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-EDAGE   NOT =   SPACE )
               MOVE    SHKITEMID-EDAGE
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-EDAGE
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-BIRTHMON   NOT =   SPACE )
               MOVE    SHKITEMID-BIRTHMON
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BIRTHMON
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SEX     NOT =   SPACE )
               MOVE    SHKITEMID-SEX
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SEX
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-NAI-Q01-STLASTYMD   NOT =   SPACE )
               MOVE    SHKITEMID-STLASTYMD
                                   TO  SHODATA-ITEMID
               MOVE    SPA-NAI-Q01-STLASTYMD
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-NAI-Q01-EDLASTYMD   NOT =   SPACE )
               MOVE    SHKITEMID-EDLASTYMD
                                   TO  SHODATA-ITEMID
               MOVE    SPA-NAI-Q01-EDLASTYMD
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NYUGAIKBN   NOT =   SPACE )
               MOVE    SHKITEMID-NYUGAIKBN
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NYUGAIKBN
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SRYKA       NOT =   SPACE )
               MOVE    SHKITEMID-SRYKA
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SRYKA
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-DEATH       NOT =   SPACE )
               MOVE    SHKITEMID-DEATH
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-DEATH
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-STPTNUM     NOT =   SPACE )
               MOVE    SHKITEMID-STPTNUM
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-STPTNUM
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-EDPTNUM     NOT =   SPACE )
               MOVE    SHKITEMID-EDPTNUM
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-EDPTNUM
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-CHKTSTPT    =   "T" )
               MOVE    SHKITEMID-CHKTSTPT
                                   TO  SHODATA-ITEMID
               MOVE    "1"         TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           ELSE
               MOVE    SHKITEMID-CHKTSTPT
                                   TO  SHODATA-ITEMID
               MOVE    "0"         TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-POST (1)    NOT =   SPACE )
               MOVE    SHKITEMID-POST01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-POST (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-POST (2)    NOT =   SPACE )
               MOVE    SHKITEMID-POST02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-POST (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-POST (3)    NOT =   SPACE )
               MOVE    SHKITEMID-POST03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-POST (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-POST (4)    NOT =   SPACE )
               MOVE    SHKITEMID-POST04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-POST (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-POST (5)    NOT =   SPACE )
               MOVE    SHKITEMID-POST05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-POST (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-JYUSHO (1)  NOT =   SPACE )
               MOVE    SHKITEMID-JYUSHO01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-JYUSHO (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-JYUSHO (2)  NOT =   SPACE )
               MOVE    SHKITEMID-JYUSHO02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-JYUSHO (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-JYUSHO (3)  NOT =   SPACE )
               MOVE    SHKITEMID-JYUSHO03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-JYUSHO (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-JYUSHO (4)  NOT =   SPACE )
               MOVE    SHKITEMID-JYUSHO04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-JYUSHO (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-JYUSHO (5)  NOT =   SPACE )
               MOVE    SHKITEMID-JYUSHO05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-JYUSHO (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NAME (1)  NOT =   SPACE )
               MOVE    SHKITEMID-NAME01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NAME (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NAME (2)  NOT =   SPACE )
               MOVE    SHKITEMID-NAME02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NAME (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NAME (3)  NOT =   SPACE )
               MOVE    SHKITEMID-NAME03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NAME (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NAME (4)  NOT =   SPACE )
               MOVE    SHKITEMID-NAME04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NAME (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NAME (5)  NOT =   SPACE )
               MOVE    SHKITEMID-NAME05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NAME (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-CONDITION1 (1)  NOT =   SPACE )
               MOVE    SHKITEMID-CONDITION101
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-CONDITION1 (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-CONDITION1 (2)  NOT =   SPACE )
               MOVE    SHKITEMID-CONDITION102
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-CONDITION1 (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-CONDITION1 (3)  NOT =   SPACE )
               MOVE    SHKITEMID-CONDITION103
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-CONDITION1 (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-CONDITION2 (1)  NOT =   SPACE )
               MOVE    SHKITEMID-CONDITION201
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-CONDITION2 (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-CONDITION2 (2)  NOT =   SPACE )
               MOVE    SHKITEMID-CONDITION202
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-CONDITION2 (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-CONDITION2 (3)  NOT =   SPACE )
               MOVE    SHKITEMID-CONDITION203
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-CONDITION2 (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-CONDITION3 (1)  NOT =   SPACE )
               MOVE    SHKITEMID-CONDITION301
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-CONDITION3 (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-CONDITION3 (2)  NOT =   SPACE )
               MOVE    SHKITEMID-CONDITION302
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-CONDITION3 (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-CONDITION3 (3)  NOT =   SPACE )
               MOVE    SHKITEMID-CONDITION303
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-CONDITION3 (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-GENMEN (1)  NOT =   SPACE )
               MOVE    SHKITEMID-GENMEN01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-GENMEN (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-GENMEN (2)  NOT =   SPACE )
               MOVE    SHKITEMID-GENMEN02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-GENMEN (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-GENMEN (3)  NOT =   SPACE )
               MOVE    SHKITEMID-GENMEN03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-GENMEN (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SELCOMMENT  NOT =   SPACE )
               MOVE    SHKITEMID-SELCOMMENT
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SELCOMMENT
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-COMMENT (1)  NOT =   SPACE )
               MOVE    SHKITEMID-COMMENT01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-COMMENT (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-COMMENT (2)  NOT =   SPACE )
               MOVE    SHKITEMID-COMMENT02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-COMMENT (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-COMMENT (3)  NOT =   SPACE )
               MOVE    SHKITEMID-COMMENT03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-COMMENT (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-TEL-TO      NOT =   SPACE )
               MOVE    SHKITEMID-TEL-TO
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-TEL-TO
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-TEL (1)  NOT =   SPACE )
               MOVE    SHKITEMID-TEL01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-TEL (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-TEL (2)  NOT =   SPACE )
               MOVE    SHKITEMID-TEL02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-TEL (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-TEL (3)  NOT =   SPACE )
               MOVE    SHKITEMID-TEL03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-TEL (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-TSY NOT =   SPACE )
               MOVE    SHKITEMID-TSY
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-TSY
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-STTSYSKBT   NOT =   SPACE )
               MOVE    SHKITEMID-STTSYSKBT
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-STTSYSKBT
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-NAI-Q01-STTSYYM     NOT =   SPACE )
               MOVE    SHKITEMID-STTSYYM
                                   TO  SHODATA-ITEMID
               MOVE    SPA-NAI-Q01-STTSYYM
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-NAI-Q01-EDTSYYM     NOT =   SPACE )
               MOVE    SHKITEMID-EDTSYYM
                                   TO  SHODATA-ITEMID
               MOVE    SPA-NAI-Q01-EDTSYYM
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-TOKKICD     NOT =   SPACE )
               MOVE    SHKITEMID-TOKKICD
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-TOKKICD
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-TOKKINAME   NOT =   SPACE )
               MOVE    SHKITEMID-TOKKINAME
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-TOKKINAME
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SAIGAIKBN   NOT =   SPACE )
               MOVE    SHKITEMID-SAIGAIKBN
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SAIGAIKBN
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-HONKZKKBN   NOT =   SPACE )
               MOVE    SHKITEMID-HONKZKKBN
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HONKZKKBN
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
      *
           IF    ( SPA-GMN-Q01-HKNJANUM (1)  NOT =   SPACE )
               MOVE    SHKITEMID-HKNJANUM01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HKNJANUM (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-HKNJANUM (2)  NOT =   SPACE )
               MOVE    SHKITEMID-HKNJANUM02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HKNJANUM (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-HKNJANUM (3)  NOT =   SPACE )
               MOVE    SHKITEMID-HKNJANUM03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HKNJANUM (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-HKNJANUM (4)  NOT =   SPACE )
               MOVE    SHKITEMID-HKNJANUM04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HKNJANUM (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-HKNJANUM (5)  NOT =   SPACE )
               MOVE    SHKITEMID-HKNJANUM05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HKNJANUM (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
      *
           IF    ( SPA-GMN-Q01-HKNNUM (1)  NOT =   SPACE )
               MOVE    SHKITEMID-HKNNUM01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HKNNUM (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-HKNNUM (2)  NOT =   SPACE )
               MOVE    SHKITEMID-HKNNUM02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HKNNUM (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-HKNNUM (3)  NOT =   SPACE )
               MOVE    SHKITEMID-HKNNUM03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HKNNUM (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-HKNNUM (4)  NOT =   SPACE )
               MOVE    SHKITEMID-HKNNUM04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HKNNUM (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-HKNNUM (5)  NOT =   SPACE )
               MOVE    SHKITEMID-HKNNUM05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-HKNNUM (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KIGO (1)  NOT =   SPACE )
               MOVE    SHKITEMID-KIGO01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KIGO (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KIGO (2)  NOT =   SPACE )
               MOVE    SHKITEMID-KIGO02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KIGO (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KIGO (3)  NOT =   SPACE )
               MOVE    SHKITEMID-KIGO03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KIGO (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KIGO (4)  NOT =   SPACE )
               MOVE    SHKITEMID-KIGO04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KIGO (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KIGO (5)  NOT =   SPACE )
               MOVE    SHKITEMID-KIGO05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KIGO (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NUM (1)  NOT =   SPACE )
               MOVE    SHKITEMID-NUM01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NUM (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NUM (2)  NOT =   SPACE )
               MOVE    SHKITEMID-NUM02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NUM (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NUM (3)  NOT =   SPACE )
               MOVE    SHKITEMID-NUM03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NUM (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NUM (4)  NOT =   SPACE )
               MOVE    SHKITEMID-NUM04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NUM (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-NUM (5)  NOT =   SPACE )
               MOVE    SHKITEMID-NUM05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-NUM (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF

      *
           IF    ( SPA-GMN-Q01-RDHKNKOH-AND  =   "T" )
               MOVE    SHKITEMID-RDHKNKOH
                                   TO  SHODATA-ITEMID
               MOVE    "1"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           ELSE
               MOVE    SHKITEMID-RDHKNKOH
                                   TO  SHODATA-ITEMID
               MOVE    "0"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-RDKOHNUM-AND   =   "T" )
               MOVE    SHKITEMID-RDKOHNUM
                                   TO  SHODATA-ITEMID
               MOVE    "1"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           ELSE
               MOVE    SHKITEMID-RDKOHNUM
                                   TO  SHODATA-ITEMID
               MOVE    "0"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KOHNUM (1)  NOT =   SPACE )
               MOVE    SHKITEMID-KOHNUM01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KOHNUM (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KOHNUM (2)  NOT =   SPACE )
               MOVE    SHKITEMID-KOHNUM02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KOHNUM (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KOHNUM (3)  NOT =   SPACE )
               MOVE    SHKITEMID-KOHNUM03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KOHNUM (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KOHNUM (4)  NOT =   SPACE )
               MOVE    SHKITEMID-KOHNUM04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KOHNUM (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KOHNUM (5)  NOT =   SPACE )
               MOVE    SHKITEMID-KOHNUM05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KOHNUM (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-FTNJANUM (1)  NOT =   SPACE )
               MOVE    SHKITEMID-FTNJANUM01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-FTNJANUM (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-FTNJANUM (2)  NOT =   SPACE )
               MOVE    SHKITEMID-FTNJANUM02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-FTNJANUM (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-FTNJANUM (3)  NOT =   SPACE )
               MOVE    SHKITEMID-FTNJANUM03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-FTNJANUM (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-FTNJANUM (4)  NOT =   SPACE )
               MOVE    SHKITEMID-FTNJANUM04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-FTNJANUM (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-FTNJANUM (5)  NOT =   SPACE )
               MOVE    SHKITEMID-FTNJANUM05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-FTNJANUM (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-UTAGAIKBN   NOT =   SPACE )
               MOVE    SHKITEMID-UTAGAIKBN
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-UTAGAIKBN
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KYUSEIKBN   NOT =   SPACE )
               MOVE    SHKITEMID-KYUSEIKBN
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KYUSEIKBN
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-TENKIKBN    NOT =   SPACE )
               MOVE    SHKITEMID-TENKIKBN
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-TENKIKBN
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-NAI-Q01-STTENKIYMD  NOT =   SPACE )
               MOVE    SHKITEMID-STTENKIYMD
                                   TO  SHODATA-ITEMID
               MOVE    SPA-NAI-Q01-STTENKIYMD
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-NAI-Q01-EDTENKIYMD  NOT =   SPACE )
               MOVE    SHKITEMID-EDTENKIYMD
                                   TO  SHODATA-ITEMID
               MOVE    SPA-NAI-Q01-EDTENKIYMD
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SYUBYOKBN    NOT =   SPACE )
               MOVE    SHKITEMID-SYUBYOKBN
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SYUBYOKBN
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-RDBYOMEI-AND    =   "T" )
               MOVE    SHKITEMID-RDBYOMEI
                                   TO  SHODATA-ITEMID
               MOVE    "1"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           ELSE
               MOVE    SHKITEMID-RDBYOMEI
                                   TO  SHODATA-ITEMID
               MOVE    "0"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-BYOMEICD (1)  NOT =   SPACE )
               MOVE    SHKITEMID-BYOMEICD01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BYOMEICD (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-BYOMEICD (2)  NOT =   SPACE )
               MOVE    SHKITEMID-BYOMEICD02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BYOMEICD (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-BYOMEICD (3)  NOT =   SPACE )
               MOVE    SHKITEMID-BYOMEICD03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BYOMEICD (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-BYOMEICD (4)  NOT =   SPACE )
               MOVE    SHKITEMID-BYOMEICD04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BYOMEICD (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-BYOMEICD (5)  NOT =   SPACE )
               MOVE    SHKITEMID-BYOMEICD05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BYOMEICD (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF

      *
           IF    ( SPA-GMN-Q01-BYOMEI (1)  NOT =   SPACE )
               MOVE    SHKITEMID-BYOMEI01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BYOMEI (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-BYOMEI (2)  NOT =   SPACE )
               MOVE    SHKITEMID-BYOMEI02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BYOMEI (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-BYOMEI (3)  NOT =   SPACE )
               MOVE    SHKITEMID-BYOMEI03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BYOMEI (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-BYOMEI (4)  NOT =   SPACE )
               MOVE    SHKITEMID-BYOMEI04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BYOMEI (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-BYOMEI (5)  NOT =   SPACE )
               MOVE    SHKITEMID-BYOMEI05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-BYOMEI (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SRYKBN  NOT =   SPACE )
               MOVE    SHKITEMID-SRYKBN
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SRYKBN
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SRYSYUKBN  NOT =   SPACE )
               MOVE    SHKITEMID-SRYSYUKBN
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SRYSYUKBN
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-RDSRYACT-AND    =   "T" )
               MOVE    SHKITEMID-RDSRYACT
                                   TO  SHODATA-ITEMID
               MOVE    "1"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           ELSE
               MOVE    SHKITEMID-RDSRYACT
                                   TO  SHODATA-ITEMID
               MOVE    "0"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SRYCD (1)  NOT =   SPACE )
               MOVE    SHKITEMID-SRYCD01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SRYCD (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SRYCD (2)  NOT =   SPACE )
               MOVE    SHKITEMID-SRYCD02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SRYCD (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SRYCD (3)  NOT =   SPACE )
               MOVE    SHKITEMID-SRYCD03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SRYCD (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SRYCD (4)  NOT =   SPACE )
               MOVE    SHKITEMID-SRYCD04
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SRYCD (4)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-SRYCD (5)  NOT =   SPACE )
               MOVE    SHKITEMID-SRYCD05
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-SRYCD (5)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-RECECOM (1)  NOT =   SPACE )
               MOVE    SHKITEMID-RECECOM01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-RECECOM (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-RECECOM (2)  NOT =   SPACE )
               MOVE    SHKITEMID-RECECOM02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-RECECOM (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-RECECOM (3)  NOT =   SPACE )
               MOVE    SHKITEMID-RECECOM03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-RECECOM (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KMK (1)  NOT =   SPACE )
               MOVE    SHKITEMID-KMK01
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KMK (1)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KMK (2)  NOT =   SPACE )
               MOVE    SHKITEMID-KMK02
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KMK (2)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-KMK (3)  NOT =   SPACE )
               MOVE    SHKITEMID-KMK03
                                   TO  SHODATA-ITEMID
               MOVE    SPA-GMN-Q01-KMK (3)
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-RDIO1 (1)    =   "T" )
               MOVE    SHKITEMID-RDIO01
                                   TO  SHODATA-ITEMID
               MOVE    "1"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           ELSE
               MOVE    SHKITEMID-RDIO01
                                   TO  SHODATA-ITEMID
               MOVE    "0"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-RDIO1 (2)    =   "T" )
               MOVE    SHKITEMID-RDIO02
                                   TO  SHODATA-ITEMID
               MOVE    "1"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           ELSE
               MOVE    SHKITEMID-RDIO02
                                   TO  SHODATA-ITEMID
               MOVE    "0"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           IF    ( SPA-GMN-Q01-RDIO1 (3)    =   "T" )
               MOVE    SHKITEMID-RDIO03
                                   TO  SHODATA-ITEMID
               MOVE    "1"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           ELSE
               MOVE    SHKITEMID-RDIO03
                                   TO  SHODATA-ITEMID
               MOVE    "0"
                                   TO  SHODATA-ITEM
               PERFORM 70011-SHOUKAIDATA-INS-SEC
           END-IF
      *
           .
       7001-SHOUKAIDATA-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件登録処理
      *****************************************************************
       7001-SHOUKAIKANRI-INS-SEC       SECTION.
      *
           PERFORM 900-SHOUKAIKANRI-KEY4-SEL-SEC
           COMPUTE WRK-RENNUM  =   SHOKANRI-RENNUM     +   1
      *
           INITIALIZE                  SHOUKAIKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    WRK-UID             TO  SHOKANRI-UID
           MOVE    WRK-RENNUM          TO  SHOKANRI-RENNUM
           MOVE    WRK-SELPATTERNMEI   TO  SHOKANRI-NAME
           MOVE    "0"                 TO  SHOKANRI-NYUGAIKBN
           MOVE    SMCNDATE-YMD        TO  SHOKANRI-CREYMD
                                           SHOKANRI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SHOKANRI-UPHMS
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT  =  ZERO )
               MOVE    "4002"          TO  SPA-ERRCD
           END-IF
      *
           .
       7001-SHOUKAIKANRI-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件登録処理
      *****************************************************************
       70011-SHOUKAIDATA-INS-SEC        SECTION.
      *
           IF    ( SPA-ERRCD       =    SPACE )
               MOVE    SMCNDATE-YMD        TO  SHODATA-CREYMD
                                               SHODATA-UPYMD
               MOVE    SMCNDATE-HMS        TO  SHODATA-UPHMS
               MOVE    SHOUKAIDATA-REC     TO  MCPDATA-REC
               MOVE    "tbl_shoukaidata"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4001"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       70011-SHOUKAIDATA-INS-EXT.
           EXIT.
      *****************************************************************
      *    全角半角チェック処理
      *****************************************************************
       800-KANACHK-CHK-SEC              SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
           ELSE
               IF    ( KANACHK-RC2 =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               ELSE
                   MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               END-IF
           END-IF
      *
           .
       800-KANACHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角半角チェック処理
      *****************************************************************
       801-KANACHK2-CHK-SEC              SECTION.
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "1"             TO  KANACHK2-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"   USING   ORCSKANACHK2AREA
           IF      ( KANACHK2-RC   =   ZERO )
               MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
           ELSE
               IF    ( KANACHK2-RC2 =   7 )
                   MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               ELSE
                   MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               END-IF
           END-IF
      *
           .
       801-KANACHK2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索文字列編集処理
      *****************************************************************
       800-SELWORD-SEC                 SECTION.
      *
           MOVE    1                   TO  WRK-SIZE
           MOVE    SPACE               TO  WRK-SIZE-STR
           STRING  WRK-SELWORD         DELIMITED   BY  SIZE
           INTO    WRK-SIZE-STR
           WITH    POINTER WRK-SIZE
           END-STRING
      *
           COMPUTE WRK-SIZE    =   WRK-SIZE    -   1
      *
           INSPECT WRK-SIZE-STR    TALLYING
                   CNT-STR         FOR ALL "＊"
      *
           INSPECT WRK-SIZE-STR
                   REPLACING   ALL "＊" BY "%%"
      *
           IF    ( CNT-STR     =   ZERO )
               PERFORM VARYING IDX8    FROM    1   BY  1
                       UNTIL ( IDX8    >   WRK-SIZE )
                   IF    ( WRK-SIZE-STR (IDX8 : )  =   SPACE )
                       MOVE    "%"     TO  WRK-SIZE-STR (IDX8 : 1)
                       MOVE    WRK-SIZE TO IDX8
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    WRK-SIZE-STR    TO  WRK-SELWORD
      *
           .
       800-SELWORD-EXT.
           EXIT.
      *****************************************************************
      *    検索文字列編集処理
      *****************************************************************
       800-SELWORD-BUBUN-SEC           SECTION.
      *
           MOVE    1                   TO  WRK-SIZE
           MOVE    SPACE               TO  WRK-SIZE-STR
           STRING  WRK-SELWORD         DELIMITED   BY  SIZE
           INTO    WRK-SIZE-STR
           WITH    POINTER WRK-SIZE
           END-STRING
      *
           COMPUTE WRK-SIZE    =   WRK-SIZE    -   1
      *
           INSPECT WRK-SIZE-STR
                   REPLACING   ALL "＊" BY "  "
      *
           IF    ( WRK-SIZE-STR    NOT =   SPACE )
      *
               MOVE    ZERO                TO  IDX9
               MOVE    SPACE               TO  WRK-SIZE-TMP
               PERFORM VARYING IDX8    FROM    1   BY  1
                       UNTIL ( IDX8    >   WRK-SIZE )
                   IF    ( WRK-SIZE-STR (IDX8 : 1)  =   SPACE )
                       CONTINUE
                   ELSE
                       COMPUTE IDX9    =   IDX9    +   1
                       MOVE    WRK-SIZE-STR (IDX8 : 1)
                                       TO  WRK-SIZE-TMP (IDX9 : 1)
                   END-IF
               END-PERFORM
      *
               MOVE    SPACE               TO  WRK-SELWORD
               STRING  "＊"                DELIMITED   BY  SIZE
                       WRK-SIZE-TMP        DELIMITED   BY  SPACE
                       "＊"                DELIMITED   BY  SIZE
               INTO    WRK-SELWORD
               END-STRING
           END-IF
      *
           .
       800-SELWORD-BUBUN-EXT.
           EXIT.
      *****************************************************************
      *    検索文字列編集処理
      *****************************************************************
       800-SELWORD-KOHO-SEC            SECTION.
      *
           MOVE    1                   TO  WRK-SIZE
           MOVE    SPACE               TO  WRK-SIZE-STR
           STRING  WRK-SELWORD         DELIMITED   BY  SIZE
           INTO    WRK-SIZE-STR
           WITH    POINTER WRK-SIZE
           END-STRING
      *
           COMPUTE WRK-SIZE    =   WRK-SIZE    -   1
      *
           INSPECT WRK-SIZE-STR
                   REPLACING   ALL "＊" BY "  "
      *
           IF    ( WRK-SIZE-STR    NOT =   SPACE )
      *
               MOVE    ZERO                TO  IDX9
               MOVE    SPACE               TO  WRK-SIZE-TMP
               PERFORM VARYING IDX8    FROM    1   BY  1
                       UNTIL ( IDX8    >   WRK-SIZE )
                   IF    ( WRK-SIZE-STR (IDX8 : 1)  =   SPACE )
                       CONTINUE
                   ELSE
                       COMPUTE IDX9    =   IDX9    +   1
                       MOVE    WRK-SIZE-STR (IDX8 : 1)
                                       TO  WRK-SIZE-TMP (IDX9 : 1)
                   END-IF
               END-PERFORM
      *
               MOVE    SPACE               TO  WRK-SELWORD
               STRING  "＊"                DELIMITED   BY  SIZE
                       WRK-SIZE-TMP        DELIMITED   BY  SPACE
               INTO    WRK-SELWORD
               END-STRING
           END-IF
      *
           .
       800-SELWORD-KOHO-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-CHG-SEC             SECTION.
      *
           INITIALIZE                  WRK-KANACHK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
           ELSE
               IF    ( KANACHK-RC2 =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               ELSE
                   MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-CHG-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       801-ZENKAKU-CHG-SEC             SECTION.
      *
           INITIALIZE                  WRK-KANACHK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "2"             TO  KANACHK2-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"  USING   ORCSKANACHK2AREA
           IF      ( KANACHK2-RC   =   ZERO )
               MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
           ELSE
               IF    ( KANACHK2-RC2 =   7 )
                   MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               ELSE
                   MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               END-IF
           END-IF
      *
           .
       801-ZENKAKU-CHG-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-YMD (1 : 6)     TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    文字列長取得処理
      *****************************************************************
       800-STR-LEN-GET-SEC                 SECTION.
      *
           PERFORM VARYING IDX-LEN FROM    WRK-STR-LEN BY  -1
               UNTIL   ( IDX-LEN   <   1)
                OR     ( WRK-STR (IDX-LEN : 1) NOT =   SPACE)
      *
               CONTINUE
           END-PERFORM
      *
           MOVE    IDX-LEN     TO  WRK-STR-LEN
      *
           .
       800-STR-LEN-GET-EXT.
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
      *    住所マスター検索（郵便番号）
      *****************************************************************
       900-ADRS-KEY4-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ADRS
      *
           INITIALIZE                  ORCSADDSIGNAREA
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
           MOVE    ADRS-POST           TO  SADDSIGN-STR
           CALL    "ORCSADDSIGN"       USING   ORCSADDSIGNAREA
           MOVE    SADDSIGN-STR        TO  ADRS-POST
      *
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
      *
           .
       900-ADRS-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    住所マスター読込（郵便番号）
      *****************************************************************
       900-ADRS-KEY4-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ADRS
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
           .
       900-ADRS-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名マスター検索処理
      *****************************************************************
       900-USERBYOMEI-KEY8-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
      *
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
           .
      *
       900-USERBYOMEI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名マスター読込処理
      *****************************************************************
       900-USERBYOMEI-KEY8-FET-SEC     SECTION.
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
           .
      *
       900-USERBYOMEI-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    病名マスター検索（漢字・カナ名）
      *****************************************************************
       900-BYOMEI-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-BYOMEI
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
           END-IF
      *
           .
       900-BYOMEI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病名マスター読込（漢字・カナ名）
      *****************************************************************
       900-BYOMEI-KEY2-FET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-BYOMEI
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
           END-IF
      *
           .
       900-BYOMEI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    病名マスター読込（主キー）
      *****************************************************************
       900-BYOMEI-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-BYOMEI
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-BYOMEI-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター検索（漢字・カナ名）
      *****************************************************************
       900-TENSU-KEY72-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-SYSYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key72"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-KEY72-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込（漢字・カナ名）
      *****************************************************************
       900-TENSU-KEY72-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key72"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
      *
       900-TENSU-KEY72-FET-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込処理
      *****************************************************************
       900-INPUTCD-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-INPUTCD
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTCD-REC
           ELSE
               MOVE    1           TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-INPUTCD-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *!
      *    検査正式名称
           IF    ( SPA-FORMALFLG           =   "1" )
               IF    ( TNS-SRYCD(1:1)      =   "1")
                AND  ( TNS-SRYKBN          =   "60")
                AND  ( TNS-FORMALNAME  NOT =   SPACE )
                AND  ( TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタFETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスター検索処理
      *****************************************************************
       900-HKNNUM-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
      *
       900-HKNNUM-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスター読込処理
      *****************************************************************
       900-HKNNUM-KEY3-FET-SEC       SECTION.
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
      *
       900-HKNNUM-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名マスター検索処理
      *****************************************************************
       900-USERBYOMEI-KEY4-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-HOSPNUM          TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
           .
      *
       900-USERBYOMEI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名マスター読込処理
      *****************************************************************
       900-USERBYOMEI-KEY4-FET-SEC     SECTION.
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
           .
      *
       900-USERBYOMEI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名マスター検索処理
      *****************************************************************
       900-USERBYOMEI-KEY3-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
      *
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
                   CONTINUE
           ELSE
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-USERBYOMEI-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名マスター読込処理
      *****************************************************************
       910-USERBYOMEI-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       910-USERBYOMEI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件管理情報検索処理
      *****************************************************************
       900-SHOUKAIKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIKANRI
      *
           INITIALIZE                      SHOUKAIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    "0"                 TO  SHOKANRI-NYUGAIKBN
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIKANRI
               INITIALIZE                  SHOUKAIKANRI-REC
           END-IF
           .
      *
       900-SHOUKAIKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件管理情報FETCH処理
      *****************************************************************
       900-SHOUKAIKANRI-KEY2-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIKANRI
      *
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIKANRI
               INITIALIZE                  SHOUKAIKANRI-REC
           END-IF
           .
      *
       900-SHOUKAIKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件管理情報検索処理
      *****************************************************************
       900-SHOUKAIKANRI-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIKANRI
      *
           INITIALIZE                      SHOUKAIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    "0"                 TO  SHOKANRI-NYUGAIKBN
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIKANRI
               INITIALIZE                  SHOUKAIKANRI-REC
           END-IF
      *
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SHOUKAIKANRI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件データ情報検索処理
      *****************************************************************
       900-SHOUKAIDATA-KEY2-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIDATA
      *
           INITIALIZE                      SHOUKAIDATA-REC
           MOVE    SPA-HOSPNUM         TO  SHODATA-HOSPNUM
           MOVE    SPA-NAI-Q01-SELPATTERN-UID
                                       TO  SHODATA-UID
           MOVE    SHOUKAIDATA-REC     TO  MCPDATA-REC
           MOVE    "tbl_shoukaidata"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIDATA-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIDATA
               INITIALIZE                  SHOUKAIDATA-REC
           END-IF
      *
           .
       900-SHOUKAIDATA-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件データ情報FETCH処理
      *****************************************************************
       900-SHOUKAIDATA-KEY2-FET-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIDATA
      *
           MOVE    "tbl_shoukaidata"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIDATA-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIDATA
               INITIALIZE                  SHOUKAIDATA-REC
           END-IF
      *
           .
       900-SHOUKAIDATA-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    検索文字エスケープ処理
      *****************************************************************
       900-PTSRH-DBESCAPE-SEC          SECTION.
      *
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBESCAPE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           MOVE    MCPDATA-REC     TO  PTSRH-REC
      *
           .
      *
       900-PTSRH-DBESCAPE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
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
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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

