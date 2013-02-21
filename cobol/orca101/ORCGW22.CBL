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
       PROGRAM-ID.                               ORCGW22.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 入院関連情報設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.09    ひさなが 　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-太田    03.11.18  更新日付編集対応
      * 01.00.02     NACL-小豆沢  06.12.22  室料差額に特化した画面に変更
      * 03.05.00     NACL-太田    07.05.18  グループ診療対応
      * 04.01.00     NACL-太田    07/11/20  室料差額６桁対応
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
           COPY    "W22SCR-SPA".
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-ERR                             PIC 9(01).
           03  FLG-KIKAN-DABURI                    PIC 9(01).
           03  FLG-UPD-KBN                         PIC 9(01).
           03  FLG-ARI                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG                          PIC X(70).
      *     03  WRK-DBPATH                          PIC X(20).
           03  WRK-TABLE                           PIC X(20).
           03  WRK-PATHNAME                        PIC X(20).
           03  WRK-YMD                             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
           03  WRK-SAGAKU-AREA.
               05  WRK-SAGAKU1                     PIC X(06).
               05  WRK-SAGAKU1-R       REDEFINES   WRK-SAGAKU1.
                   07  WRK-SAGAKU1-9               PIC 9(06).
           03  WRK-NUM-06                          PIC 9(06).
           03  WRK-CREYMD                          PIC X(08).
           03  WRK-BRMNUMKBN                       PIC 9(02).
      *
      *    メッセージ領域
       01  T-MESSAGE-AREA.
           03  T-MESSAGE-001       PIC X(16)   VALUE
               "入力エラーです。".
           03  T-MESSAGE-002       PIC X(32)   VALUE
               "半角文字でのみ入力してください。".
           03  T-MESSAGE-003       PIC X(32)   VALUE
               "変更対象のデータは存在しません。".
           03  T-MESSAGE-004       PIC X(32)   VALUE
               "削除対象のデータは存在しません。".
           03  T-MESSAGE-005       PIC X(46)   VALUE
               "有効期間開始日＞有効期間終了日となっています。".
           03  T-MESSAGE-006       PIC X(48)   VALUE
               "有効期間終了日が過去日の追加・変更は行えません。".
           03  T-MESSAGE-007       PIC X(30)   VALUE
               "管理コードを入力してください。".
           03  T-MESSAGE-008       PIC X(30)   VALUE
               "区分コードを入力してください。".
           03  T-MESSAGE-009       PIC X(24)   VALUE
               "名称を入力してください。".
           03  T-MESSAGE-010       PIC X(34)   VALUE
               "有効期間開始日を入力してください。".
           03  T-MESSAGE-011       PIC X(34)   VALUE
               "有効期間終了日を入力してください。".
           03  T-MESSAGE-012       PIC X(38)   VALUE
               "有効期間の重複するデータが存在します。".
           03  T-MESSAGE-013       PIC X(48)   VALUE
               "有効期間の開始日と終了日は同時に変更できません。".
           03  T-MESSAGE-014       PIC X(30)   VALUE
               "追加対象のデータは存在します。".
           03  T-MESSAGE-051       PIC X(22)   VALUE
               "更新できませんでした。".
           03  T-MESSAGE-052       PIC X(22)   VALUE
               "追加できませんでした。".
           03  T-MESSAGE-053       PIC X(22)   VALUE
               "削除できませんでした。".
           03  T-MESSAGE-054       PIC X(40)   VALUE
               "該当データを削除します。よろしいですか？".
           03  T-MESSAGE-055       PIC X(60)   VALUE
               "区分コードは０１から９９の範囲で入力してください".
           03  T-MESSAGE-056       PIC X(34)   VALUE
               "室料差額の登録は最大３０件迄です。".
           03  T-MESSAGE-057       PIC X(50)   VALUE
               "室料差額は３０万円以内で登録してください。".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    病棟種別
           COPY  "CPSK5101.INC".
      *    平均在日数
           COPY  "CPSK5102.INC".
      *    看護配置
           COPY  "CPSK5103.INC".
      *    看護比率
           COPY  "CPSK5104.INC".
      *    看護補助加算
           COPY  "CPSK5105.INC".
      *    夜間勤務看護加算
           COPY  "CPSK5106.INC".
      *    看護師人数区分
           COPY  "CPSK5107.INC".
      *    病室種別
           COPY  "CPSK5108.INC".
      *    性別特定
           COPY  "CPSK5109.INC".
      *    特定入院料（病棟）
           COPY  "CPSK5110.INC".
      *    特定入院料（病室）
           COPY  "CPSK5111.INC".
      *    室料差額
           COPY  "CPSK5113.INC".
      *    保育士人数区分
           COPY  "CPSK5114.INC".
      *    看護補助配置
           COPY  "CPSK5115.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
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
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
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
           COPY    "W122.INC".
           COPY    "W13.INC".
           COPY    "W14.INC".
           COPY    "W15.INC".
           COPY    "W16.INC".
           COPY    "W17.INC".
           COPY    "W18.INC".
           COPY    "W19.INC".
           COPY    "W20.INC".
           COPY    "W201.INC".
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
           COPY    "W321.INC".
           COPY    "W33.INC".
           COPY    "W34.INC".
           COPY    "W35.INC".
           COPY    "W36.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W62.INC".
           COPY    "W63.INC".
           COPY    "W64.INC".
           COPY    "W65.INC".
           COPY    "W66.INC".
           COPY    "W67.INC".
           COPY    "W68.INC".
           COPY    "W96.INC".
           COPY    "W98.INC".
           COPY    "W981.INC".
           COPY    "W97.INC".
           COPY    "W99.INC".
           COPY    "WERR.INC".
           COPY    "WID1.INC".
           COPY    "WID2.INC".
           COPY    "W00.INC".
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
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU.
           MOVE    SPA-FREE        TO  SPA-W22.
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
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
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK.
           MOVE    SPA-W22         TO  SPA-FREE.
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF  SPA-MOTOPG   =   "WERR"  OR  "WID1"
               IF  (SPA-MOTOPG     =   "WID1")  AND
                   (SPA-WID1-FLG   =   "OK")    AND
                   (SPA-W22-PFK    =   "08")
                   PERFORM 4201-DELETE-SEC
                   IF  SPA-W22-ERRMSG   =   SPACE
                       PERFORM 420-TORIKOMI-SEC
                       PERFORM 420-CLEAR-SEC
                       PERFORM 500-GMNHEN-SEC
                   END-IF
               END-IF
               MOVE    SPACE               TO  SPA-MOTOPG
               MOVE    SPACE               TO  SPA-WID1-FLG
               MOVE    SPACE               TO  SPA-W22-PFK
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF.
      *
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "W22"               TO  MCP-WINDOW.
           PERFORM 900-PUT-WINDOW.
           MOVE    1                   TO  FLG-END.
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
           IF  LINKAREA   NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF.
      *
           PERFORM 310-SPASET-SEC.
           PERFORM 420-TORIKOMI-SEC.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                        SPA-W22.
      *
           MOVE    SPA-MOTOPG            TO  SPA-W22-MOTOPG.
           MOVE    1                     TO  SPA-W22-CUR.
           MOVE    1                     TO  SPA-W22-PAGE.
           INITIALIZE                        SPA-W22-GMN.
      *
      *
      *    室料差額のみコンボに編集
           MOVE    "5113"                TO  SPA-W22-KANRICD-1 (01).
           MOVE    "室料差額"            TO  SPA-W22-KANRICD-3 (01).
      *
      *     MOVE    SPACE                 TO  SPA-W22-KANRICD.
           MOVE    "5113 室料差額"       TO  SPA-W22-KANRICD.
           MOVE    SPACE                 TO  SPA-W22-KBNCD.
           MOVE    SPACE                 TO  SPA-W22-MEISYOU.
           MOVE    SPACE                 TO  SPA-W22-STYUKYMD-L.
           MOVE    SPACE                 TO  SPA-W22-EDYUKYMD-L.
           INITIALIZE                        SPA-W22-MEISYOU-TBL.
      *
           MOVE    2                     TO  SPA-W22-CUR.
      *
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "01"                 TO  SPA-W22-PFK
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM  210-BACK
               WHEN    "CLICKED"       ALSO    "B02"
                   MOVE   "02"                 TO  SPA-W22-PFK
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 420-CLEAR-SEC
               WHEN    "CLICKED"       ALSO    "B06"
                   MOVE   "06"                 TO  SPA-W22-PFK
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 420-TUIKA-SEC
               WHEN    "CLICKED"       ALSO    "B07"
                   MOVE   "07"                 TO  SPA-W22-PFK
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 420-HENKOU-SEC
               WHEN    "CLICKED"       ALSO    "B08"
                   MOVE   "08"                 TO  SPA-W22-PFK
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 420-SAKUJO-SEC
               WHEN    OTHER
                   MOVE    2                   TO  SPA-W22-CUR
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "KANRICD"
                   PERFORM T-000-KANRICD-SEC
               WHEN    "ACTIVATE"      ALSO    "KBNCD"
                   PERFORM T-000-KBNCD-SEC
               WHEN    "ACTIVATE"      ALSO    "MEISYOU"
                   PERFORM T-000-MEISYOU-SEC
               WHEN    "ACTIVATE"      ALSO    "STYUKYMD"
                   PERFORM T-000-STYUKYMD-SEC
               WHEN    "ACTIVATE"      ALSO    "EDYUKYMD"
                   PERFORM T-000-EDYUKYMD-SEC
               WHEN    "SELECT"        ALSO    "MEISYOULIST"
                   PERFORM T-000-MEISYOU-ITEM-SEC
               WHEN    OTHER
                   MOVE    2                   TO  SPA-W22-CUR
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG.
           MOVE    SPA-MOTOPG2         TO  SPA-MOTOPG3.
           MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2.
           MOVE    "W22"               TO  SPA-MOTOPG.
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           MOVE    SPACE               TO  SPA-W22-KBNCD.
           MOVE    SPACE               TO  SPA-W22-MEISYOU.
           MOVE    SPACE               TO  SPA-W22-STYUKYMD-L.
           MOVE    SPACE               TO  SPA-W22-EDYUKYMD-L.
      *
           MOVE    SPACE               TO  SPA-W22-ERRMSG.
           MOVE    ZERO                TO  FLG-ARI.
           MOVE    ZERO                TO  SPA-W22-BTNO
      *
           MOVE    2                   TO  SPA-W22-CUR.
      *
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       420-TUIKA-SEC               SECTION.
      *
           PERFORM 4900-KANRENCHK-SEC
           IF  SPA-W22-ERRMSG   =   SPACE
               IF   SPA-W22-MEISYOU-COUNT   >=  30
                    MOVE    T-MESSAGE-056   TO  SPA-W22-ERRMSG
                    MOVE    2               TO  SPA-W22-CUR
               END-IF
           END-IF
           IF  SPA-W22-ERRMSG   =   SPACE
               IF  FLG-ARI   =   1
                   MOVE    T-MESSAGE-014   TO  SPA-W22-ERRMSG
                   MOVE    2               TO  SPA-W22-CUR
               END-IF
           END-IF
           IF  SPA-W22-ERRMSG   =   SPACE
               MOVE    1                   TO  FLG-UPD-KBN
               PERFORM 4201-KAKUNIN-SEC
               IF  FLG-KIKAN-DABURI   NOT =   ZERO
                   MOVE    T-MESSAGE-012   TO  SPA-W22-ERRMSG
                   MOVE    4               TO  SPA-W22-CUR
               ELSE
                   PERFORM 4201-INSERT-SEC
                   IF  SPA-W22-ERRMSG   =   SPACE
                       PERFORM 420-TORIKOMI-SEC
                       PERFORM 420-CLEAR-SEC
                   END-IF
               END-IF
           END-IF.
      *
       420-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更処理
      *****************************************************************
       420-HENKOU-SEC                  SECTION.
      *
           PERFORM 4900-KANRENCHK-SEC
           IF  SPA-W22-ERRMSG   =   SPACE
               IF  FLG-ARI   =   ZERO
                   MOVE    T-MESSAGE-003   TO  SPA-W22-ERRMSG
                   MOVE    2               TO  SPA-W22-CUR
               END-IF
           END-IF
           IF  SPA-W22-ERRMSG   =   SPACE
      *    有効期間の開始日と終了日が同時に変更されていないこと
                   IF  (SPA-W22-STYUKYMD-S  NOT =
                                               SPA-W22-STYUKYMD-OLD) AND
                       (SPA-W22-EDYUKYMD-S  NOT =
                                               SPA-W22-EDYUKYMD-OLD)
                       MOVE    T-MESSAGE-013   TO  SPA-W22-ERRMSG
                       MOVE    4               TO  SPA-W22-CUR
                   ELSE
                       MOVE    2                   TO  FLG-UPD-KBN
                       PERFORM 4201-KAKUNIN-SEC
                       IF  FLG-KIKAN-DABURI   NOT =   ZERO
                           MOVE    T-MESSAGE-012   TO  SPA-ERRCD
                           MOVE    4               TO  SPA-W22-CUR
                       ELSE
                           PERFORM 4201-UPDATE-SEC
                           IF  SPA-W22-ERRMSG   =   SPACE
                               PERFORM 420-TORIKOMI-SEC
                               PERFORM 420-CLEAR-SEC
                           END-IF
                       END-IF
                   END-IF
           END-IF.
      *
       420-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       420-SAKUJO-SEC                  SECTION.
      *
           IF  SPA-W22-ERRMSG    =   SPACE
               IF  SPA-W22-KANRICD   =   SPACE
                   MOVE    T-MESSAGE-007   TO  SPA-W22-ERRMSG
                   MOVE    2               TO  SPA-W22-CUR
               ELSE
                   IF  SPA-W22-KBNCD   =   SPACE
                       MOVE    T-MESSAGE-008   TO  SPA-W22-ERRMSG
                       MOVE    2               TO  SPA-W22-CUR
                   ELSE
                       MOVE    T-MESSAGE-054   TO  WRK-WIDMSG
                       MOVE    SPA-MOTOPG      TO  SPA-W22-MOTOPG
                       MOVE    SPACE           TO  WID1
                       MOVE    SPA-WIDCD       TO  WID1-ID1CODE
                       MOVE    WRK-WIDMSG      TO  WID1-ID1MSG
                       MOVE    "B12"           TO  MCP-WIDGET
                       MOVE    "W22"           TO  SPA-MOTOPG
                       MOVE    "WID1"          TO  SPA-SAKIPG
                       MOVE    "NEW"           TO  MCP-PUTTYPE
                       MOVE    "WID1"          TO  MCP-WINDOW
      *
                       PERFORM 900-PUT-WINDOW
      *
                       MOVE    1               TO  FLG-END
                   END-IF
               END-IF
           END-IF.
      *
       420-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    取り込み処理
      *****************************************************************
       420-TORIKOMI-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDX.
           INITIALIZE                      SYSKANRI-REC.
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SPA-W22-KANRICD-C   TO  SYS-KANRICD.
      *     MOVE    SPA-SYSYMD          TO  ORC-DBYMD.
      *    システム管理で有効なデータのみ表示はＫＥＹ２を使用
      *    MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH.
      *    MOVE    "SYSKANRI-KEY2"     TO  WRK-DBPATH.
           MOVE    "tbl_syskanri"      TO  MCP-TABLE.
           MOVE    "key5"              TO  MCP-PATHNAME.
           MOVE    "tbl_syskanri"      TO  WRK-TABLE.
           MOVE    "key5"              TO  WRK-PATHNAME.
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC.
      *
           PERFORM 910-DB-SELECT-SEC.
           IF  MCP-RC   =   ZERO
               PERFORM SYS-KANRI-READ-SEC
               PERFORM VARYING IDX    FROM    1   BY  1
                       UNTIL   IDX    >   30
                   MOVE    SYS-KBNCD           TO  SPA-W22-ITEM1 (IDX)
                   EVALUATE    SPA-W22-KANRICD-C
                       WHEN    "5101"
                           MOVE    MCPDATA-REC   TO  SYS-5101-REC
                           MOVE    SYS-5101-BTU-SBT-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5102"
                           MOVE    MCPDATA-REC   TO  SYS-5102-REC
                           MOVE    SYS-5102-BTU-HEIKINZAIKBN-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5103"
                           MOVE    MCPDATA-REC   TO  SYS-5103-REC
                           MOVE    SYS-5103-BTU-KNGHAICHIKBN-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5104"
                           MOVE    MCPDATA-REC   TO  SYS-5104-REC
                           MOVE    SYS-5104-BTU-KNGHIRITUKBN-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5105"
                           MOVE    MCPDATA-REC   TO  SYS-5105-REC
                           MOVE    SYS-5105-BTU-KNGHJOKSN-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5106"
                           MOVE    MCPDATA-REC   TO  SYS-5106-REC
                           MOVE    SYS-5106-BTU-YAKANKNGKSN-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5107"
                           MOVE    MCPDATA-REC   TO  SYS-5107-REC
                           MOVE    SYS-5107-BTU-KNGSISUKBN-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5108"
                           MOVE    MCPDATA-REC   TO  SYS-5108-REC
                           MOVE    SYS-5108-BRM-SBT-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5109"
                           MOVE    MCPDATA-REC   TO  SYS-5109-REC
                           MOVE    SYS-5109-BRM-SEX-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5110"
                           MOVE    MCPDATA-REC   TO  SYS-5110-REC
                           MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5111"
                           MOVE    MCPDATA-REC   TO  SYS-5111-REC
                           MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5113"
                           MOVE    MCPDATA-REC   TO  SYS-5113-REC
                           MOVE    SYS-5113-BRM-SAGAKU-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5114"
                           MOVE    MCPDATA-REC   TO  SYS-5114-REC
                           MOVE    SYS-5114-BTU-HIKSISUKBN-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                       WHEN    "5115"
                           MOVE    MCPDATA-REC   TO  SYS-5115-REC
                           MOVE    SYS-5115-BTU-KNGHJOHAI-NM
                                                 TO  SPA-W22-ITEM2 (IDX)
                   END-EVALUATE
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    SYS-STYUKYMD        TO  WRK-YMD
                   MOVE    SYS-STYUKYMD        TO  SPA-W22-STYUKYMD-S
                   MOVE    SYS-STYUKYMD        TO  SPA-W22-STYUKYMD-OLD
                   PERFORM HIZUKE-CHK-SEC
                   IF  SPA-ERRCD   =   SPACE
                       MOVE    WRK-HENYMDG       TO  SPA-W22-ITEM3 (IDX)
                   END-IF
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    SYS-EDYUKYMD        TO  WRK-YMD
                   MOVE    SYS-EDYUKYMD        TO  SPA-W22-EDYUKYMD-S
                   MOVE    SYS-EDYUKYMD        TO  SPA-W22-EDYUKYMD-OLD
                   PERFORM HIZUKE-CHK-SEC
                   IF  SPA-ERRCD   =   SPACE
                       MOVE    WRK-HENYMDG       TO  SPA-W22-ITEM4 (IDX)
                   END-IF
                   MOVE    IDX                 TO  SPA-W22-MEISYOU-COUNT
                   PERFORM SYS-KANRI-READ-SEC
               END-PERFORM
           END-IF.
           PERFORM SYS-KANRI-CLOSE-SEC
           .
      *
       420-TORIKOMI-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込処理
      *****************************************************************
       SYS-KANRI-READ-SEC             SECTION.
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE.
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME.
           PERFORM 910-DB-READ-SEC.
           IF  MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    99              TO  IDX
           END-IF.
      *
       SYS-KANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理クローズ処理
      *****************************************************************
       SYS-KANRI-CLOSE-SEC            SECTION.
      *
           MOVE    WRK-TABLE     TO  MCP-TABLE.
           MOVE    WRK-PATHNAME  TO  MCP-PATHNAME.
           PERFORM 910-DB-CLOSE-SEC.
      *
       SYS-KANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報確認処理
      *****************************************************************
       4201-KAKUNIN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX.
           MOVE    ZERO                TO  FLG-KIKAN-DABURI.
           INITIALIZE                      SYSKANRI-REC.
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SPA-W22-KANRICD-C   TO  SYS-KANRICD.
           MOVE    SPA-W22-KBNCD       TO  SYS-KBNCD.
           MOVE    "tbl_syskanri"      TO  MCP-TABLE.
           MOVE    "key8"              TO  MCP-PATHNAME.
           MOVE    "tbl_syskanri"      TO  WRK-TABLE.
           MOVE    "key8"              TO  WRK-PATHNAME.
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC.
      *
           PERFORM 910-DB-SELECT-SEC.
           IF  MCP-RC   =   ZERO
               PERFORM SYS-KANRI-READ-SEC
               PERFORM UNTIL   IDX   >   30
                   IF  (FLG-UPD-KBN            =   2)  AND
                       (SPA-W22-KBNCD          =   SYS-KBNCD)  AND
                       (SPA-W22-STYUKYMD-OLD   =   SYS-STYUKYMD)  AND
                       (SPA-W22-EDYUKYMD-OLD   =   SYS-EDYUKYMD)
                       CONTINUE
                   ELSE
                       IF  (SPA-W22-STYUKYMD-OLD   <   SYS-STYUKYMD)  OR
                           (SPA-W22-EDYUKYMD-OLD   >   SYS-EDYUKYMD)
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-KIKAN-DABURI
                       END-IF
                   END-IF
                   COMPUTE  IDX   =   IDX   +   1
                   PERFORM SYS-KANRI-READ-SEC
               END-PERFORM
           END-IF.
           PERFORM SYS-KANRI-CLOSE-SEC
           .
      *
       4201-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院関連情報追加処理
      *****************************************************************
       4201-INSERT-SEC                 SECTION.
      *
           PERFORM 4201-EDIT-SEC.
      *
           EVALUATE    SPA-W22-KANRICD-C
               WHEN    "5101"
                   MOVE    SYS-5101-REC    TO  SYSKANRI-REC
               WHEN    "5102"
                   MOVE    SYS-5102-REC    TO  SYSKANRI-REC
               WHEN    "5103"
                   MOVE    SYS-5103-REC    TO  SYSKANRI-REC
               WHEN    "5104"
                   MOVE    SYS-5104-REC    TO  SYSKANRI-REC
               WHEN    "5105"
                   MOVE    SYS-5105-REC    TO  SYSKANRI-REC
               WHEN    "5106"
                   MOVE    SYS-5106-REC    TO  SYSKANRI-REC
               WHEN    "5107"
                   MOVE    SYS-5107-REC    TO  SYSKANRI-REC
               WHEN    "5108"
                   MOVE    SYS-5108-REC    TO  SYSKANRI-REC
               WHEN    "5109"
                   MOVE    SYS-5109-REC    TO  SYSKANRI-REC
               WHEN    "5110"
                   MOVE    SYS-5110-REC    TO  SYSKANRI-REC
               WHEN    "5111"
                   MOVE    SYS-5111-REC    TO  SYSKANRI-REC
               WHEN    "5113"
                   MOVE    SYS-5113-REC    TO  SYSKANRI-REC
               WHEN    "5114"
                   MOVE    SYS-5114-REC    TO  SYSKANRI-REC
               WHEN    "5115"
                   MOVE    SYS-5115-REC    TO  SYSKANRI-REC
           END-EVALUATE.
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                               SYS-UPYMD
           MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
           MOVE    SYSKANRI-REC          TO  MCPDATA-REC.
           MOVE    "tbl_syskanri"        TO  MCP-TABLE.
           MOVE    "key"                 TO  MCP-PATHNAME.
           PERFORM 910-DB-INSERT-SEC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    T-MESSAGE-052      TO  SPA-W22-ERRMSG
           ELSE
               MOVE    SPACE              TO  SPA-W22-ERRMSG
           END-IF.
      *
       4201-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院関連情報更新処理
      *****************************************************************
       4201-UPDATE-SEC                SECTION.
      *
      *    更新対象存在チェック
           MOVE    ZERO                    TO  FLG-SYSKANRI
           MOVE    SPACE                   TO  WRK-CREYMD
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
           MOVE    SPA-W22-KANRICD-C       TO  SYS-KANRICD
           MOVE    SPA-W22-KBNCD           TO  SYS-KBNCD
           MOVE    SPA-W22-STYUKYMD-OLD    TO  SYS-STYUKYMD
           MOVE    SPA-W22-EDYUKYMD-OLD    TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           MOVE    SYSKANRI-REC            TO  MCPDATA-REC
           PERFORM 910-DB-SELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DB-READ-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    SYS-CREYMD      TO  WRK-CREYMD
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
                   MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM 910-DB-CLOSE-SEC
      *
           IF    ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    T-MESSAGE-051       TO  SPA-W22-ERRMSG
               GO  TO  4201-UPDATE-EXT
           END-IF
      *
           PERFORM 4201-EDIT-SEC.
      *
           EVALUATE    SPA-W22-KANRICD-C
               WHEN    "5101"
                   MOVE    SYS-5101-REC    TO  SYSKANRI-REC
               WHEN    "5102"
                   MOVE    SYS-5102-REC    TO  SYSKANRI-REC
               WHEN    "5103"
                   MOVE    SYS-5103-REC    TO  SYSKANRI-REC
               WHEN    "5104"
                   MOVE    SYS-5104-REC    TO  SYSKANRI-REC
               WHEN    "5105"
                   MOVE    SYS-5105-REC    TO  SYSKANRI-REC
               WHEN    "5106"
                   MOVE    SYS-5106-REC    TO  SYSKANRI-REC
               WHEN    "5107"
                   MOVE    SYS-5107-REC    TO  SYSKANRI-REC
               WHEN    "5108"
                   MOVE    SYS-5108-REC    TO  SYSKANRI-REC
               WHEN    "5109"
                   MOVE    SYS-5109-REC    TO  SYSKANRI-REC
               WHEN    "5110"
                   MOVE    SYS-5110-REC    TO  SYSKANRI-REC
               WHEN    "5111"
                   MOVE    SYS-5111-REC    TO  SYSKANRI-REC
               WHEN    "5113"
                   MOVE    SYS-5113-REC    TO  SYSKANRI-REC
               WHEN    "5114"
                   MOVE    SYS-5114-REC    TO  SYSKANRI-REC
               WHEN    "5115"
                   MOVE    SYS-5115-REC    TO  SYSKANRI-REC
           END-EVALUATE.
      *
           MOVE    SPA-HOSPNUM           TO  SYSUP-HOSPNUM
           MOVE    SPA-W22-KANRICD-C     TO  SYSUP-KANRICD.
           MOVE    SPA-W22-KBNCD         TO  SYSUP-KBNCD.
           MOVE    SPA-W22-STYUKYMD-OLD  TO  SYSUP-STYUKYMD.
           MOVE    SPA-W22-EDYUKYMD-OLD  TO  SYSUP-EDYUKYMD.
      *
      *
           MOVE    WRK-CREYMD            TO  SYS-CREYMD
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD          TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS          TO  SYS-UPHMS
      *
           MOVE    SYSKANRI-REC          TO  MCPDATA-REC.
      *
           MOVE    "tbl_syskanri"        TO  MCP-TABLE.
           MOVE    "upd1"                TO  MCP-PATHNAME.
           PERFORM 910-DB-UPDATE-SEC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    T-MESSAGE-051      TO  SPA-W22-ERRMSG
           ELSE
               MOVE    SPACE              TO  SPA-W22-ERRMSG
           END-IF.
      *
       4201-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院関連情報削除処理
      *****************************************************************
       4201-DELETE-SEC                 SECTION.
      *
           INITIALIZE                        SYSKANRI-REC.
           MOVE    SPA-HOSPNUM           TO  SYS-HOSPNUM
           MOVE    SPA-W22-KANRICD-C     TO  SYS-KANRICD.
           MOVE    SPA-W22-KBNCD         TO  SYS-KBNCD.
           MOVE    SPA-W22-STYUKYMD-OLD  TO  SYS-STYUKYMD.
           MOVE    SPA-W22-EDYUKYMD-OLD  TO  SYS-EDYUKYMD.
           MOVE    SYSKANRI-REC          TO  MCPDATA-REC.
           MOVE    "tbl_syskanri"        TO  MCP-TABLE.
           MOVE    "key"                 TO  MCP-PATHNAME.
           PERFORM 910-DB-DELETE-SEC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    T-MESSAGE-053      TO  SPA-W22-ERRMSG
           ELSE
               MOVE    SPACE              TO  SPA-W22-ERRMSG
           END-IF.
      *
       4201-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院関連情報編集処理
      *****************************************************************
       4201-EDIT-SEC                   SECTION.
      *
           EVALUATE    SPA-W22-KANRICD-C
               WHEN    "5101"
                   MOVE    SPACE       TO          SYS-5101-REC
                   INITIALIZE                      SYS-5101-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5101-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5101-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5101-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5101-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5101-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5101-BTU-SBT-NM
               WHEN    "5102"
                   MOVE    SPACE       TO          SYS-5102-REC
                   INITIALIZE                      SYS-5102-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5102-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5102-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5102-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5102-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5102-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5102-BTU-HEIKINZAIKBN-NM
               WHEN    "5103"
                   MOVE    SPACE       TO          SYS-5103-REC
                   INITIALIZE                      SYS-5103-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5103-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5103-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5103-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5103-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5103-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5103-BTU-KNGHAICHIKBN-NM
               WHEN    "5104"
                   MOVE    SPACE       TO          SYS-5104-REC
                   INITIALIZE                      SYS-5104-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5104-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5104-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5104-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5104-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5104-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5104-BTU-KNGHIRITUKBN-NM
               WHEN    "5105"
                   MOVE    SPACE       TO          SYS-5105-REC
                   INITIALIZE                      SYS-5105-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5105-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5105-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5105-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5105-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5105-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5105-BTU-KNGHJOKSN-NM
               WHEN    "5106"
                   MOVE    SPACE       TO          SYS-5106-REC
                   INITIALIZE                      SYS-5106-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5106-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5106-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5106-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5106-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5106-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5106-BTU-YAKANKNGKSN-NM
               WHEN    "5107"
                   MOVE    SPACE       TO          SYS-5107-REC
                   INITIALIZE                      SYS-5107-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5107-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5107-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5107-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5107-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5107-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5107-BTU-KNGSISUKBN-NM
               WHEN    "5108"
                   MOVE    SPACE       TO          SYS-5108-REC
                   INITIALIZE                      SYS-5108-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5108-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5108-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5108-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5108-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5108-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5108-BRM-SBT-NM
               WHEN    "5109"
                   MOVE    SPACE       TO          SYS-5109-REC
                   INITIALIZE                      SYS-5109-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5109-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5109-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5109-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5109-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5109-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5109-BRM-SEX-NM
               WHEN    "5110"
                   MOVE    SPACE       TO          SYS-5110-REC
                   INITIALIZE                      SYS-5110-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5110-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5110-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5110-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5110-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5110-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5110-BTU-TOKUTEINYUIN-NM
               WHEN    "5111"
                   MOVE    SPACE       TO          SYS-5111-REC
                   INITIALIZE                      SYS-5111-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5111-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5111-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5111-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5111-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5111-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5111-BRM-TOKUTEINYUIN-NM
               WHEN    "5113"
                   MOVE    SPACE       TO          SYS-5113-REC
                   INITIALIZE                      SYS-5113-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5113-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5113-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5113-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5113-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5113-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU(1:6)
                                       TO  WRK-SAGAKU1
                   MOVE    WRK-SAGAKU1-9
                                       TO  SYS-5113-BRM-SAGAKU-NM
               WHEN    "5114"
                   MOVE    SPACE       TO          SYS-5114-REC
                   INITIALIZE                      SYS-5114-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5114-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5114-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5114-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5114-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5114-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5114-BTU-HIKSISUKBN-NM
               WHEN    "5115"
                   MOVE    SPACE       TO          SYS-5115-REC
                   INITIALIZE                      SYS-5115-REC
                   MOVE    SPA-HOSPNUM TO  SYS-5115-HOSPNUM
                   MOVE    SPA-W22-KANRICD-C
                                       TO  SYS-5115-KANRICD
                   MOVE    SPA-W22-KBNCD
                                       TO  SYS-5115-KBNCD
                   MOVE    SPA-W22-STYUKYMD-S
                                       TO  SYS-5115-STYUKYMD
                   MOVE    SPA-W22-EDYUKYMD-S
                                       TO  SYS-5115-EDYUKYMD
                   MOVE    SPA-W22-MEISYOU
                                       TO  SYS-5115-BTU-KNGHJOHAI-NM
           END-EVALUATE.
      *
       4201-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-W22-ERRMSG.
      *
           PERFORM T-001-KANRICD-SEC
           IF    ( SPA-W22-ERRMSG  NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           PERFORM T-001-KBNCD-SEC
           IF    ( SPA-W22-ERRMSG  NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           PERFORM T-001-MEISYOU-SEC
           IF    ( SPA-W22-ERRMSG  NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           PERFORM T-001-STYUKYMD-SEC
           IF    ( SPA-W22-ERRMSG  NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           PERFORM T-001-EDYUKYMD-SEC
           IF    ( SPA-W22-ERRMSG  NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           IF  SPA-W22-KANRICD   =   SPACE
               MOVE    T-MESSAGE-007   TO  SPA-W22-ERRMSG
               MOVE    1               TO  SPA-W22-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF.
      *
           IF  SPA-W22-KBNCD   =   SPACE
               MOVE    T-MESSAGE-008   TO  SPA-W22-ERRMSG
               MOVE    2               TO  SPA-W22-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF.
      *
           IF  SPA-W22-MEISYOU   =   SPACE
               MOVE    T-MESSAGE-009   TO  SPA-W22-ERRMSG
               MOVE    3               TO  SPA-W22-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF.
      *
           IF  SPA-W22-STYUKYMD-S   =   SPACE
               MOVE    T-MESSAGE-010   TO  SPA-W22-ERRMSG
               MOVE    4               TO  SPA-W22-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF.
      *
           IF  SPA-W22-EDYUKYMD-S   =   SPACE
               MOVE    T-MESSAGE-011   TO  SPA-W22-ERRMSG
               MOVE    5               TO  SPA-W22-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF.
      *
           IF  SPA-W22-STYUKYMD-S   >   SPA-W22-EDYUKYMD-S
               MOVE    T-MESSAGE-005   TO  SPA-W22-ERRMSG
               MOVE    4               TO  SPA-W22-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF.
      *
           IF  SPA-W22-EDYUKYMD-S   <   SPA-SYSYMD
               MOVE    T-MESSAGE-006   TO  SPA-W22-ERRMSG
               MOVE    4               TO  SPA-W22-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF.
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC.
      *
           IF  SPA-W22-ERRMSG   NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               MOVE    SPACE               TO  SPA-W22-ERRMSG
           ELSE
               MOVE    "CURRENT"           TO  MCP-PUTTYPE
               MOVE    "W22    "           TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
           END-IF.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE                   TO  W22.
      *
           MOVE    SPA-W22-KANRICD         TO  W22-KANRICD.
           PERFORM VARYING IDX FROM   1   BY  1
                   UNTIL   IDX >      20
               IF  SPA-W22-KANRICD-L (IDX)   NOT =   SPACE
      *             MOVE    IDX                TO  W22-KANRICD-COUNT
      *             MOVE    SPA-W22-KANRICD-L (IDX)
      *                                        TO  W22-KANRICD-ITEM (IDX)
                    CONTINUE
               END-IF
           END-PERFORM.
      *
           MOVE    SPA-W22-KBNCD           TO  W22-KBNCD.
           MOVE    SPA-W22-MEISYOU         TO  W22-MEISYOU.
           MOVE    SPA-W22-STYUKYMD        TO  W22-STYUKYMD.
           MOVE    SPA-W22-EDYUKYMD        TO  W22-EDYUKYMD.
      *
           PERFORM VARYING IDX FROM   1   BY  1
                   UNTIL   IDX >      30
               MOVE    SPA-W22-ITEM1 (IDX)
                                           TO  W22-VALUE1 (IDX)
               MOVE    SPA-W22-ITEM2 (IDX)
                                           TO  W22-VALUE2 (IDX)
               MOVE    SPA-W22-ITEM3 (IDX)
                                           TO  W22-VALUE3 (IDX)
               MOVE    SPA-W22-ITEM4 (IDX)
                                           TO  W22-VALUE4 (IDX)
               IF    ( SPA-W22-BTNO        =   IDX )
                   MOVE    "T"             TO  W22-MEISYOU-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  W22-MEISYOU-SELECT (IDX)
               END-IF
           END-PERFORM.
      *
           MOVE    SPA-W22-MEISYOU-COUNT   TO  W22-MEISYOU-COUNT.
      *
           PERFORM 5001-MAPCUR-SEC.
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-W22-CUR
               WHEN    01
                   MOVE  "KANRICD"         TO   MCP-WIDGET
               WHEN    02
                   MOVE  "KBNCD"           TO   MCP-WIDGET
               WHEN    03
                   MOVE  "MEISYOU"         TO   MCP-WIDGET
               WHEN    04
                   MOVE  "STYUKYMD"        TO   MCP-WIDGET
               WHEN    05
                   MOVE  "EDYUKYMD"        TO   MCP-WIDGET
               WHEN    06
                   MOVE  "B06"             TO   MCP-WIDGET
           END-EVALUATE.
      *
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG.
      *
           MOVE    SPACE               TO  WERR.
           MOVE    "1001"              TO  WERR-ERRCODE.
           MOVE    SPA-W22-ERRMSG      TO  WERR-ERRMSG.
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "W22"               TO  SPA-MOTOPG.
           MOVE    "WERR"              TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "WERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DB-DELETE-SEC               SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       910-DB-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DB-UPDATE-SEC               SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       910-DB-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DB-INSERT-SEC               SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       910-DB-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DB-SELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DB-READ-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       910-DB-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理コード 確認処理
      *****************************************************************
       T-000-KANRICD-SEC                 SECTION.
      *
           MOVE    2                   TO  SPA-W22-CUR.
      *
           INITIALIZE                      SPA-W22-KBNCD
                                           SPA-W22-MEISYOU
                                           SPA-W22-STYUKYMD-L
                                           SPA-W22-EDYUKYMD-L
                                           SPA-W22-MEISYOU-TBL
      *
      *
           MOVE    W22-KANRICD         TO  SPA-W22-KANRICD.
      *
           PERFORM T-001-KANRICD-SEC
      *
           IF  SPA-W22-ERRMSG   =   SPACE
               PERFORM 420-TORIKOMI-SEC
           END-IF.
      *
       T-000-KANRICD-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理コード 確認処理
      *****************************************************************
       T-001-KANRICD-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-ERR.
           IF  SPA-W22-KANRICD   NOT =   SPACE
               PERFORM VARYING IDX FROM   1   BY  1
                       UNTIL   IDX >      20
                   IF  SPA-W22-KANRICD-C   =   SPA-W22-KANRICD-1 (IDX)
                       MOVE    SPA-W22-KANRICD-L (IDX)
                                                   TO  SPA-W22-KANRICD
                       MOVE    99                  TO  IDX
                       MOVE    ZERO                TO  FLG-ERR
                   END-IF
               END-PERFORM
           END-IF.
      *
           IF  FLG-ERR   =   1   
               MOVE    T-MESSAGE-007         TO  SPA-W22-ERRMSG
               MOVE    1                     TO  SPA-W22-CUR
           END-IF.
      *
           .
       T-001-KANRICD-EXT.
           EXIT.
      *****************************************************************
      *    区分コード 確認処理
      *****************************************************************
       T-000-KBNCD-SEC                   SECTION.
      *
           MOVE    3                   TO  SPA-W22-CUR.
           MOVE    W22-KBNCD           TO  SPA-W22-KBNCD.
      *
           PERFORM T-001-KBNCD-SEC
           .
      *
       T-000-KBNCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    区分コード 確認処理
      *****************************************************************
       T-001-KBNCD-SEC                 SECTION.
      *
           IF  SPA-W22-KBNCD   NOT =   SPACE
               MOVE    SPACE               TO  ORCSKANACHKAREA
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-W22-KBNCD       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  SPA-W22-KBNCD
               IF  (KANACHK-RC        =   ZERO)  AND
                   (KANACHK-SYUBETU   =   1)
                   CONTINUE
               ELSE
                   MOVE    T-MESSAGE-002   TO  SPA-W22-ERRMSG
                   MOVE    2               TO  SPA-W22-CUR
               END-IF
      *
      *
               IF    ( SPA-W22-ERRMSG      =   SPACE )
                   IF    ( SPA-W22-KBNCD (1:2)
                                          IS  NUMERIC )
                    AND  ( SPA-W22-KBNCD (3:)  =   SPACE )
                       MOVE    SPA-W22-KBNCD  (1:2)
                                           TO  WRK-BRMNUMKBN
                       IF    ( WRK-BRMNUMKBN   >=  01 )
                        AND  ( WRK-BRMNUMKBN   <=  99 )
                           CONTINUE
                       ELSE
                           MOVE    T-MESSAGE-055   TO  SPA-W22-ERRMSG
                           MOVE    2               TO  SPA-W22-CUR
                       END-IF
                   ELSE
                           MOVE    T-MESSAGE-055   TO  SPA-W22-ERRMSG
                           MOVE    2               TO  SPA-W22-CUR
                   END-IF
               END-IF
      *
           END-IF.
           IF  SPA-W22-ERRMSG   =   SPACE
               MOVE    ZERO                  TO  IDX
               MOVE    ZERO                  TO  FLG-ARI
               INITIALIZE                        SYSKANRI-REC
               MOVE    SPA-HOSPNUM           TO  SYS-HOSPNUM
               MOVE    SPA-W22-KANRICD-C     TO  SYS-KANRICD
               MOVE    SPA-W22-KBNCD         TO  SYS-KBNCD
               MOVE    "tbl_syskanri"        TO  MCP-TABLE
               MOVE    "key8"                TO  MCP-PATHNAME
               MOVE    "tbl_syskanri"        TO  WRK-TABLE
               MOVE    "key8"                TO  WRK-PATHNAME
               MOVE    SYSKANRI-REC          TO  MCPDATA-REC
      *
               PERFORM 910-DB-SELECT-SEC
               IF  MCP-RC   =   ZERO
                   PERFORM SYS-KANRI-READ-SEC
                   IF  IDX   =   ZERO
                       MOVE    1            TO  FLG-ARI
                       MOVE    SYS-STYUKYMD TO  SPA-W22-STYUKYMD-OLD
                       MOVE    SYS-EDYUKYMD TO  SPA-W22-EDYUKYMD-OLD
                   END-IF
               END-IF
               PERFORM SYS-KANRI-CLOSE-SEC
      *
           END-IF.
      *
           .
       T-001-KBNCD-EXT.
           EXIT.
      *****************************************************************
      *    名称 確認処理
      *****************************************************************
       T-000-MEISYOU-SEC                 SECTION.
      *
           MOVE    4                   TO  SPA-W22-CUR.
           MOVE    W22-MEISYOU         TO  SPA-W22-MEISYOU.
      *
           PERFORM T-001-MEISYOU-SEC
      *
	  .
      *
       T-000-MEISYOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称 確認処理
      *****************************************************************
       T-001-MEISYOU-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-ERR.
      *
           IF  SPA-W22-MEISYOU   NOT =   SPACE
               MOVE    SPACE               TO  ORCSKANACHKAREA
               INITIALIZE                      ORCSKANACHKAREA
               IF  SPA-W22-KANRICD-C   =   "5113"
                   MOVE    "1"                 TO  KANACHK-SYORI
               ELSE
                   MOVE    "2"                 TO  KANACHK-SYORI
               END-IF
               MOVE    SPA-W22-MEISYOU     TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"    USING  ORCSKANACHKAREA
               IF  SPA-W22-KANRICD-C   =   "5113"
                   IF  (KANACHK-RC        =   ZERO)  AND
                       (KANACHK-SYUBETU   =   1)
                       MOVE    ZERO                TO  FLG-ERR
                   END-IF
               ELSE
                   IF  (KANACHK-RC2       =   ZERO)  AND
                       (KANACHK-SYUBETU   =   2)
                       MOVE    ZERO                TO  FLG-ERR
                   END-IF
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-ERR
           END-IF.
      *
           IF  FLG-ERR   =   1
               IF  SPA-W22-KANRICD-C   =   "5113"
                   MOVE    T-MESSAGE-002         TO  SPA-W22-ERRMSG
               ELSE
                   MOVE    T-MESSAGE-001         TO  SPA-W22-ERRMSG
               END-IF
               MOVE    3                     TO  SPA-W22-CUR
               GO  TO  T-001-MEISYOU-EXT
           END-IF.
      *
          IF  SPA-W22-KANRICD-C         =   "5113"

               INITIALIZE                     ORCSNUMAREA
               MOVE   SPA-W22-MEISYOU  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    3               TO  SPA-W22-CUR
                   MOVE    T-MESSAGE-001   TO  SPA-W22-ERRMSG
                   GO  TO  T-001-MEISYOU-EXT
               ELSE
                   MOVE    SNUM-OUTNUM TO  WRK-NUM-06
                   MOVE    WRK-NUM-06  TO  SPA-W22-MEISYOU
               END-IF
      *
               IF    ( SNUM-OUTNUM     >   300000 )
                   MOVE    T-MESSAGE-057   TO  SPA-W22-ERRMSG
                   MOVE    3               TO  SPA-W22-CUR
                   GO  TO  T-001-MEISYOU-EXT
               END-IF
      *
          END-IF
      *
           .
       T-001-MEISYOU-EXT.
           EXIT.
      *****************************************************************
      *    有効期間開始日 確認処理
      *****************************************************************
       T-000-STYUKYMD-SEC               SECTION.
      *
           MOVE    5                   TO  SPA-W22-CUR.
           MOVE    SPACE               TO  SPA-W22-STYUKYMD-S.
           MOVE    W22-STYUKYMD        TO  SPA-W22-STYUKYMD.
      *
           PERFORM T-001-STYUKYMD-SEC
           .
      *
       T-000-STYUKYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効期間開始日 確認処理
      *****************************************************************
       T-001-STYUKYMD-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-ERR.
           IF  SPA-W22-STYUKYMD   =   SPACE
               MOVE    "00000000"          TO  SPA-W22-STYUKYMD
               MOVE    "00000000"          TO  SPA-W22-STYUKYMD-S
               MOVE    ZERO                TO  FLG-ERR
           ELSE
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    SPA-W22-STYUKYMD    TO  WRK-YMD
               PERFORM HIZUKE-CHK-SEC
               IF  SPA-ERRCD   =   SPACE
                   MOVE    WRK-HENYMDG         TO  SPA-W22-STYUKYMD
                   MOVE    WRK-SYMD            TO  SPA-W22-STYUKYMD-S
                   MOVE    ZERO                TO  FLG-ERR
               END-IF
           END-IF.
      *
           IF  FLG-ERR   =   1   
               MOVE    T-MESSAGE-001         TO  SPA-W22-ERRMSG
               MOVE    4                     TO  SPA-W22-CUR
           END-IF.
      *
           .
       T-001-STYUKYMD-EXT.
           EXIT.
      *****************************************************************
      *    有効期間終了日 確認処理
      *****************************************************************
       T-000-EDYUKYMD-SEC               SECTION.
      *
           MOVE    6                   TO  SPA-W22-CUR.
           MOVE    SPACE               TO  SPA-W22-EDYUKYMD-S.
           MOVE    W22-EDYUKYMD        TO  SPA-W22-EDYUKYMD.
      *
           PERFORM T-001-EDYUKYMD-SEC
           .
      *
       T-000-EDYUKYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効期間終了日 確認処理
      *****************************************************************
       T-001-EDYUKYMD-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-ERR.
           IF  SPA-W22-EDYUKYMD   =   SPACE
               MOVE    "99999999"          TO  SPA-W22-EDYUKYMD
               MOVE    "99999999"          TO  SPA-W22-EDYUKYMD-S
               MOVE    ZERO                TO  FLG-ERR
           ELSE
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    SPA-W22-EDYUKYMD    TO  WRK-YMD
               PERFORM HIZUKE-CHK-SEC
               IF  SPA-ERRCD   =   SPACE
                   MOVE    WRK-HENYMDG         TO  SPA-W22-EDYUKYMD
                   MOVE    WRK-SYMD            TO  SPA-W22-EDYUKYMD-S
                   MOVE    ZERO                TO  FLG-ERR
               END-IF
           END-IF.
      *
           IF  FLG-ERR   =   1   
               MOVE    T-MESSAGE-001         TO  SPA-W22-ERRMSG
               MOVE    5                     TO  SPA-W22-CUR
           END-IF.
      *
           .
       T-001-EDYUKYMD-EXT.
           EXIT.
      *****************************************************************
      *    入院関連情報 確認処理
      *****************************************************************
       T-000-MEISYOU-ITEM-SEC           SECTION.
      *
           MOVE    SPACE            TO  SPA-W22-KBNCD.
           MOVE    SPACE            TO  SPA-W22-MEISYOU.
           MOVE    SPACE            TO  SPA-W22-STYUKYMD.
           MOVE    SPACE            TO  SPA-W22-EDYUKYMD.
           MOVE    ZERO             TO  FLG-ARI.
           MOVE    ZERO             TO  SPA-W22-BTNO
      *
           PERFORM VARYING IDX  FROM  1  BY 1
                   UNTIL   IDX  >    SPA-W22-MEISYOU-COUNT
               IF  W22-MEISYOU-SELECT (IDX)  =  "T"
                   MOVE    SPA-W22-ITEM1 (IDX)  TO  SPA-W22-KBNCD
                   MOVE    SPA-W22-ITEM2 (IDX)  TO  SPA-W22-MEISYOU
                   MOVE    SPA-W22-ITEM3 (IDX)  TO  SPA-W22-STYUKYMD
                   MOVE    SPACE                TO  SPA-ERRCD
                   MOVE    SPA-W22-ITEM3 (IDX)  TO  WRK-YMD
                   PERFORM HIZUKE-CHK-SEC
                   IF  SPA-ERRCD   =   SPACE
                       MOVE    WRK-HENYMDG      TO  SPA-W22-STYUKYMD
                       MOVE    WRK-SYMD         TO  SPA-W22-STYUKYMD-S
                       MOVE    WRK-SYMD         TO  SPA-W22-STYUKYMD-OLD
                   END-IF
                   MOVE    SPA-W22-ITEM4 (IDX)  TO  SPA-W22-EDYUKYMD
                   MOVE    SPACE                TO  SPA-ERRCD
                   MOVE    SPA-W22-ITEM4 (IDX)  TO  WRK-YMD
                   PERFORM HIZUKE-CHK-SEC
                   IF  SPA-ERRCD   =   SPACE
                       MOVE    WRK-HENYMDG      TO  SPA-W22-EDYUKYMD
                       MOVE    WRK-SYMD         TO  SPA-W22-EDYUKYMD-S
                       MOVE    WRK-SYMD         TO  SPA-W22-EDYUKYMD-OLD
                   END-IF
                   MOVE    IDX                  TO  SPA-W22-BTNO
                   MOVE    3                    TO  SPA-W22-CUR
                   MOVE    1                    TO  FLG-ARI
               END-IF
           END-PERFORM.
      *
       T-000-MEISYOU-ITEM-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       HIZUKE-CHK-SEC                  SECTION.
      *
           IF  WRK-YMD   =   "00000000"  OR  "99999999"
               MOVE    WRK-YMD             TO  WRK-HENYMDG
               MOVE    WRK-YMD             TO  WRK-SYMD
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-YMD             TO  SGDAY-INDAY
               CALL    "ORCSGDAY"       USING  ORCSGDAYAREA
               IF  SGDAY-RC   =   ZERO
                   MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY      TO  WRK-SYMD
               ELSE
                   MOVE    "0001"          TO  SPA-ERRCD
               END-IF
           END-IF.
      *
       HIZUKE-CHK-EXT.
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
