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
       PROGRAM-ID.             ORCGP02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者情報（Ｐ０２）（メイン）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     01/04/18  生年月日未来日チェック追加
      *  01.00.02    MCC-太田     01/04/20  受付戻り時の患者IDセット
      *  01.00.03    MCC-太田     01/04/25  終了時の更新確認等
      *  01.00.04    MCC-太田     01/05/24  終了時の更新確認等修正
      *  01.00.05    MCC-多々納   01/06/13  登録後、診療行為遷移を追加
      *  01.00.06    MCC-多々納   01/07/02  日付画面用チェックサブ追加
      *                                     年齢表示追加
      *                                     画面遷移後のカーソル移動修正
      *  01.00.07    MCC-多々納   01/12/04  画面遷移変更に伴う修正
      *  01.00.08    MCC-多々納   02/04/05  労災保険の追加
      *  01.00.09    MCC-太田     02/05/23  患者照会からの遷移を考慮
      *  01.00.10    MCC-太田     02/05/31  カナ氏名候補画面を追加
      *  01.00.11    MCC-多々納   02/06/18  月代り受給者番号の追加
      *  02.00.00    MCC-多々納   02/06/21  画面レイアウト変更
      *  02.00.01    NACL-多々納  02/09/10  平成１４年１０月改正対応
      *  02.00.02    NACL-多々納  02/10/23  入院追加
      *  02.00.03    NACL-多々納  02/11/07  入院基本画面遷移追加
      *  02.00.04    NACL-多々納  02/11/20  ＣＬＡＩＭ処理追加
      *  02.00.05    NACL-多々納  02/11/21  共済下船３月追加
      *  02.00.06    NACL-多々納  03/01/28  一括削除に患者定期請求追加
      *  02.00.07    NACL-多々納  03/03/05  排他制御処理追加
      *  02.00.08    NACL-多々納  03/03/24  公費負担額追加
      *  02.00.07    NACL-多々納  03/04/22  保険組合変更時メッセージ追加
      *  02.00.08    NACL-多々納  03/07/08  保険者異動日チェック追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者登録全画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-PTNUMO          PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-KYUSEIRRK       PIC 9(01).
           03  FLG-PTGETNUM        PIC 9(01).
           03  FLG-TSYRRK          PIC 9(01).
           03  FLG-TNKRRK          PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-MONTHLYNUM      PIC 9(01).
      *
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTTAINFUKA      PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
      *
           03  FLG-SYSKANRI       PIC 9(01).
      *
           03  FLG-HAKKOU          PIC 9(01).
      *
           03  FLG-CLAIM           PIC 9(01).
           03  FLG-CLAIM-OK        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-YMD             PIC X(08).
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-SAKIPG          PIC X(08).
           03  WRK-WIDGET          PIC X(20).
           03  WRK-P02-GAMEN       PIC X(06).
      *
           03  WRK-EVENT           PIC X(20).
           03  WRK-STATUS          PIC X(04).
           03  WRK-ACCEPT          PIC X(03).
           03  WRK-ID-AREA.
               05  WRK-HKNID           PIC 9(10).
               05  WRK-KOHID           PIC 9(10).
               05  WRK-AUTOCOMBINUM    PIC 9(04).
               05  WRK-MANUCOMBINUM    PIC 9(04).
           03  WRK-PIDMSG          PIC X(100).
      *
           03  WRK-MOTOPG              PIC X(08).
           03  WRK-ERRCD               PIC X(04).
      *
           03  WRK-MEISAI1         PIC 9(06).
           03  WRK-MEISAI2         PIC 9(06).
           03  WRK-MEISAI3         PIC 9(06).
      *    文字数
           03  WRK-KETA                PIC 9(02).
           03  FLG-KETA                PIC 9(01).
      *
      *保存元ＰＧ
           03  WRK-HZN-PTNUM              PIC X(20).
           03  WRK-HZN-MOTOPG             PIC X(08).
      *    受付時間
           03  WRK-UKETIME                PIC X(06).
           03  WRK-PTID                   PIC X(10).
      *
           03  WRK-IDOYMD-H            PIC X(22).
      *
      *
           03  WRK-GMN-SELNUM        PIC 9(02). 
      *    ＣＬＡＩＭファイル名
       01  CLAIM-FILE.
           03  FILLER              PIC X(30)   VALUE
               "scripts/claim/HL01.sh".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *
      *    画面カーソル移動用
       01  WRK-WIDGET-AREA.
           03  WRK-P020-WIDGET         PIC X(64).
           03  WRK-P02W1-WIDGET        PIC X(64).
           03  WRK-P027-WIDGET         PIC X(64).
           03  WRK-P021-WIDGET         PIC X(64).
           03  WRK-P022-WIDGET         PIC X(64).
           03  WRK-P024-WIDGET         PIC X(64).
           03  WRK-P025-WIDGET         PIC X(64).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *　　（ＣＬＡＩＭ接続）
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    旧姓履歴
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    低所得
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    年金
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    紹介履歴
       01  SYKRRK-REC.
           COPY    "CPSYKRRK.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    予約マスター保存
       01  WRK-YYK-REC.
           COPY    "CPYYK.INC"       REPLACING
                                     //YYK-// BY //WRK-YYK-//.
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    月代り受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者他院歴付加情報
       01  PTTAINFUKA-REC.
           COPY    "CPPTTAINFUKA.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *    地方公費請求書
           COPY    "CPKOHSKY.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    公費負担額
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *   患者番号発生サブ
           COPY    "CPORCSP01.INC".
      *   画面編集サブ
           COPY    "CPORCSP02.INC".
      *   チェックデジット
           COPY    "CPORCCHKDGT.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA"          REPLACING
                                     //MCP// BY //WRK-MCP//.
      *
      *===> For Claim Start
           COPY    "CPSHELLTBL.INC".
           COPY    "ORCA-DBPATH".
      *===> For Claim End
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "P02.INC".
           COPY    "P02A.INC".
      *****COPY    "P02B.INC".
      *****COPY    "P02C.INC".
           COPY    "P02D.INC".
           COPY    "P02E.INC".
           COPY    "P02F.INC".
           COPY    "P014.INC".
           COPY    "P98.INC".
           COPY    "P981.INC".
           COPY    "P99.INC".
           COPY    "P0201.INC".
           COPY    "P982.INC".
           COPY    "P100.INC".
           COPY    "P02X.INC".
           COPY    "PERR.INC".
           COPY    "PID1.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  SPA-FLG-END
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面編集後、表示へ
           IF     (FLG-END             =   ZERO)   AND
                  (SPA-FLG-END         =   ZERO)
               PERFORM 500-SET-SCREEN
      *
           END-IF
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               MOVE    SPA-WIDGET      TO  MCP-WIDGET
               MOVE    SPACE           TO  SPA-WIDGET
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF  ( FLG-END           =   1 )  OR
                   ( SPA-FLG-END       =   1 )
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF  ( FLG-END           =   1 )  OR
                   ( SPA-FLG-END       =   1 )
                   GO      TO      100-INIT-EXT
               END-IF
           END-IF
      *
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "P02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA            NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"  OR
                                               "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-P02SCRAREA
                   MOVE    SPA-WORK        TO  SPA-P02KYOUTU
               END-IF
      *        予約の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01"
                   GO      TO              300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
      *            表示画面設定
                   PERFORM 3001-P97SPASET-SEC
                   GO      TO              300-SCREEN-EXT
               ELSE
                   MOVE    SPA-MOTOPG      TO  SPA-MOTOPG2
               END-IF
           END-IF
      *
           MOVE    SPA-WORK                TO  SPA-P02KYOUTU
           MOVE    ZERO                    TO  SPA-FLG-END
           MOVE    SPACE                   TO  SPA-ERRCD
      *
           EVALUATE    SPA-MOTOPG
      *        業務選択画面より
               WHEN    "M01"
                   INITIALIZE                  SPA-P02SCRAREA
                   INITIALIZE                  SPA-P02KYOUTU
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   MOVE    ZERO                TO  SPA-GMN-CUR
      *            各画面を初期編集する
                   PERFORM 310-SPA-SET-SEC
      *        受付、診療行為、患者照会より
               WHEN    "K02"
               WHEN    "K02N"
               WHEN    "U02"
               WHEN    "Q02"
               WHEN    "I01"
                   INITIALIZE                  SPA-P02SCRAREA
                   INITIALIZE                  SPA-P02KYOUTU
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   MOVE    ZERO                TO  SPA-GMN-CUR
                   MOVE    SPA-PTNUM           TO  SPA-HZN-PTNUM
                   MOVE    SPA-MOTOPG          TO  SPA-HZN-MOTOPG
                   IF      SPA-PTNUM       NOT =   SPACE
                       MOVE    SPA-PTNUM           TO  P02-PTNUM
                       PERFORM 4100-PTNUM-SYORI-SEC
                   ELSE
      *                各画面を初期編集する
                       MOVE    1                   TO  SPA-P02-SYORI
                       PERFORM 310-SPA-SET-SEC
                   END-IF
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
                   GO      TO                  300-SCREEN-EXT
      *        拡張患者番号設定
               WHEN    "P014"
                   MOVE    "P02"           TO  SPA-P02-GAMEN
                   IF      SPA-GMN014-PTNUM    =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN014-PTNUM    TO  SPA-GMN-PTNUM
                       MOVE    1                   TO  SPA-P02-SYORI
                       PERFORM 310-SPA-SET-SEC
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    SPACE               TO  SPA-P02-WIDGET
                   END-IF
                   GO      TO                  300-SCREEN-EXT
      *        患者複写
               WHEN    "P02E"
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   IF      SPA-GMN2E-PTNUM     =   SPACE
                       MOVE    1                   TO  SPA-GMN-CUR
                   ELSE
                       MOVE    "1"                 TO  SPA-UPDFLG
      *                複写あり（各画面の初期編集）
                       MOVE    "LINK"              TO  MCP-STATUS
                       MOVE    "P02E"              TO  SPA-MOTOPG
                       PERFORM 49012-GMNSPA-SET-SEC
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
                   GO      TO                  300-SCREEN-EXT
      *        二重登録画面より
               WHEN    "P02A"
                   IF      SPA-NAI2A-PTNUM NOT =   SPACE
                       MOVE    SPA-NAI2A-PTNUM     TO  P02-PTNUM
                       PERFORM 4100-PTNUM-SYORI-SEC
                       MOVE    SPACE              TO  SPA-NAI2A-PTNUM
                   ELSE
      *                カーソルを移動する
                       MOVE    "LINK"              TO  MCP-STATUS
                       PERFORM 3002-GMNSET-SEC
                   END-IF
      *        ＣＬＡＩＭ画面より
               WHEN    "P02X"
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   IF      SPA-P02X-FLG        =   SPACE
                       MOVE    1                   TO  SPA-GMN-CUR
                   ELSE
                       PERFORM 3009-CLAIM-SYORI-SEC
                   END-IF
                   GO      TO                  300-SCREEN-EXT
      *        各画面より
               WHEN    OTHER
                   MOVE    "LINK"              TO  MCP-STATUS
                   PERFORM 3002-GMNSET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｐ９７より処理
      *****************************************************************
       3001-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *
      *    受付、患者検索より
           IF    ( WRK-SPA-PTNUM   NOT =   SPA-GMN-PTNUM )  AND
                 ( WRK-SPA-PTNUM   NOT =   SPACE         )
      *        患者番号選択あり
               MOVE    WRK-SPA-PTNUM       TO  P02-PTNUM
               MOVE    WRK-SPA-P02-UKEID   TO  SPA-P02-UKEID
               PERFORM 4100-PTNUM-SYORI-SEC
           ELSE
      *        受付氏名のみ選択あり
              IF      (WRK-SPA-PTNUM       =   SPACE   )  AND
                      (WRK-SPA-NAME    NOT =   SPACE   )  AND
                      (WRK-SPA-P02-UKEID   NOT =   ZERO    )
                   MOVE    WRK-SPA-NAME        TO  SPA-NAME
                   MOVE    WRK-SPA-P02-UKEID   TO  SPA-P02-UKEID
      *            各画面を初期編集する
                   MOVE    1                   TO  SPA-P02-SYORI
                   PERFORM 310-SPA-SET-SEC
               ELSE
      *            選択なし
                   IF     (WRK-SPA-PTNUM       =   SPACE  )  AND
                          (SPA-GMN-PTID        =   ZERO   )
                       PERFORM 30011-PTNUM-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    ZERO                TO  SPA-GMN-CUR
           END-IF
           .
       3001-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    未選択時、患者番号チェック処理
      *****************************************************************
       30011-PTNUM-CHK-SEC              SECTION.
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    00
      *            患者あり
                   CONTINUE
               WHEN    20
      *            患者検索へ
                   MOVE    ZERO                TO  SPA-P02-SYORI
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
               WHEN    10
      *            新規患者検索へ
                   IF      SPTNUM-SYUBETU       =   1
                       CONTINUE
                   ELSE
                       MOVE    ZERO                TO  SPA-P02-SYORI
                   END-IF
               WHEN    OTHER
      *            エラー
                   MOVE    ZERO                TO  SPA-P02-SYORI
           END-EVALUATE
           .
       30011-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
      *        戻る確認
               WHEN    "0101"
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    SPACE           TO  SPA-UPDFLG
                       PERFORM 210-BACK-SYORI-SEC
                   ELSE
                       MOVE    "1"             TO  SPA-UPDFLG
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        削除確認
               WHEN    "0103"
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4991-DELETE-SYORI-SEC
                   END-IF
      *        保険変更確認
               WHEN    "9100"
               WHEN    "9101"
               WHEN    "9102"
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM  49021-KOUSIN-AFTER-SEC
                   ELSE
                       PERFORM  49021-KOUSIN-AFTER-SEC
                   END-IF
      *        保険重複確認
               WHEN    "0105"
               WHEN    "0104"
               WHEN    "0106"
               WHEN    "0107"
      *        保険変換確認
               WHEN    "0201"
               WHEN    "0202"
               WHEN    "0203"
               WHEN    "0204"
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    "P02 "          TO  SPA-P02-GAMEN
                       PERFORM 3002-GMNSET-SEC
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移各画面チェック処理
      *****************************************************************
       3002-GMNSET-SEC                 SECTION.
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
           EVALUATE    SPA-P02-GAMEN
      *       受付
               WHEN     "P020"
                   CALL    "ORCGP020"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        基本
               WHEN     "P02 "
                   CALL    "ORCGP02W1"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        保険組合せ
               WHEN     "P027"
                   CALL    "ORCGP027"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        連絡先
               WHEN     "P021"
                   CALL    "ORCGP021"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        所得者情報
               WHEN     "P024"
                   CALL    "ORCGP024"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        各種履歴
               WHEN     "P025"
                   CALL    "ORCGP025"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        紹介情報入力
               WHEN     "P023"
                   CALL    "ORCGP023"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
           END-EVALUATE
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
           .
       3002-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPA-SET-SEC              SECTION.
      *
      *    パラメタファイル、患者情報のセット
           MOVE    "INIT"              TO  ORCSP02-SYORI
           CALL    "ORCSP02"           USING   ORCSP02AREA
                                               SPA-AREA
                                               SPA-P02KYOUTU
                                               SPA-P02SCRAREA
                                               P02
      *
      *    患者情報以外のセット
           IF      SPA-P02-SYORI       =   2
      *        画面ＳＰＡ編集のみ（各画面の初期編集）
               MOVE    "LINK"              TO  MCP-STATUS
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 49012-GMNSPA-SET-SEC
           END-IF
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＭＥ画面よりの処理
      *****************************************************************
       3009-CLAIM-SYORI-SEC              SECTION.
      *
           IF      SPA-P02X-FLG        =  "OK"
               MOVE    1               TO  FLG-CLAIM
           ELSE
               MOVE    ZERO            TO  FLG-CLAIM
           END-IF
      *
      *    更新処理
           MOVE    1                   TO  FLG-CLAIM-OK
           PERFORM 4902-KOUSIN-SYORI-SEC
      *
          .
       3009-CLAIM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       49012-GMNSPA-SET-SEC          SECTION.
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
      *    基本
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP02W1"      USING   WRK-MCPAREA
                                            SPAAREA
                                            LINKAREA
                                            SCRAREA
      *
      *    受付
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP020"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
      *    保険組合せ
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP027"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *    連絡先
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP021"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *    所得者情報
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP024"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *    各種履歴
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP025"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *    紹介情報入力
      *    MOVE    MCPAREA             TO  WRK-MCPAREA
      *    CALL    "ORCGP023"      USING   WRK-MCPAREA
      *                                            SPAAREA
      *                                            LINKAREA
      ****                                         SCRAREA
      *
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
      *
          .
       49012-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "NO"                TO  SPA-ERRMSG
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 410-CLREA-SEC
      *    前回患者
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 420-ZENPTNUM-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 499-DELETE-SEC
      *    再発行
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 450-SAINUM-SEC
      *    タグ切り替え（→）
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 430-BETUGAMEN-SEC
      *    タグ切り替え（←）
               WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 430-BETUGAMEN2-SEC
      *    氏名検索（Ｐ９７）
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 460-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    複写処理
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 499-COPY-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    タグ切り替え
               WHEN    "CLICKED"       ALSO    "N01"
               WHEN    "CLICKED"       ALSO    "N02"
               WHEN    "CLICKED"       ALSO    "N03"
               WHEN    "CLICKED"       ALSO    "N04"
      *********WHEN    "CLICKED"       ALSO    "N06"
               WHEN    "CLICKED"       ALSO    "N07"
               WHEN    "CLICKED"       ALSO    "N08"
                   PERFORM 460-TAGUCHG-SEC
      *    各画面へ
               WHEN    OTHER
                   MOVE    ZERO                TO  SPA-TAGU-FLG
                   PERFORM 3002-GMNSET-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次画面　処理
      *****************************************************************
       430-BETUGAMEN-SEC               SECTION.
      *
           MOVE    SPA-P02-GAMEN       TO  WRK-P02-GAMEN
      *
      *    患者指定がない時、チェックなし
           IF      SPA-GMN-PTNUM       =   SPACE
               CONTINUE
           ELSE
      *
      *        表示画面の登録処理
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               MOVE    1                   TO  SPA-TAGU-FLG
               PERFORM 3002-GMNSET-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                   GO  TO                  430-BETUGAMEN-EXT
               END-IF
      *
           END-IF
      *
           EVALUATE    WRK-P02-GAMEN
               WHEN    "P020"
                   MOVE    "P02 "          TO  SPA-P02-GAMEN
                   MOVE    "N02"           TO  SPA-P02-WIDGET
               WHEN    "P02"
                   MOVE    "P027"          TO  SPA-P02-GAMEN
                   MOVE    "N03"           TO  SPA-P02-WIDGET
               WHEN    "P027"
                   MOVE    "P021"          TO  SPA-P02-GAMEN
                   MOVE    "N04"           TO  SPA-P02-WIDGET
               WHEN    "P021"
                   MOVE    "P024"          TO  SPA-P02-GAMEN
                   MOVE    "N07"           TO  SPA-P02-WIDGET
      ***      WHEN    "P022"
      *++          MOVE    "P023"          TO  SPA-P02-GAMEN
      *++          MOVE    "N06"           TO  SPA-P02-WIDGET
               WHEN    "P024"
                   MOVE    "P025"          TO  SPA-P02-GAMEN
                   MOVE    "N08"           TO  SPA-P02-WIDGET
               WHEN    "P025"
                   MOVE    "P020"          TO  SPA-P02-GAMEN
                   MOVE    "N01"           TO  SPA-P02-WIDGET
           END-EVALUATE
      *
           MOVE    SPA-P02-WIDGET     TO  MCP-WIDGET
      *    画面ＳＰＡ編集のみ（次の画面の初期編集）
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    ZERO                TO  SPA-TAGU-FLG
           MOVE    SPACE               TO  SPA-MOTOPG
           PERFORM 3002-GMNSET-SEC
           MOVE    SPACE              TO  SPA-P02-WIDGET
           MOVE    ZERO               TO  SPA-FLG-END
      *
           .
       430-BETUGAMEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前画面　処理
      *****************************************************************
       430-BETUGAMEN2-SEC               SECTION.
      *
           MOVE    SPA-P02-GAMEN       TO  WRK-P02-GAMEN
      *    患者指定がない時、チェックなし
           IF      SPA-GMN-PTNUM       =   SPACE
               CONTINUE
           ELSE
      *
      *        表示画面の登録処理
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               MOVE    1                   TO  SPA-TAGU-FLG
               PERFORM 3002-GMNSET-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                   GO  TO                  430-BETUGAMEN2-EXT
               END-IF
      *
           END-IF
      *
           EVALUATE    WRK-P02-GAMEN
               WHEN    "P020"
                   MOVE    "P025"          TO  SPA-P02-GAMEN
                   MOVE    "N08"           TO  SPA-P02-WIDGET
               WHEN    "P02"
                   MOVE    "P020"          TO  SPA-P02-GAMEN
                   MOVE    "N01"           TO  SPA-P02-WIDGET
               WHEN    "P027"
                   MOVE    "P02 "          TO  SPA-P02-GAMEN
                   MOVE    "N02"           TO  SPA-P02-WIDGET
               WHEN    "P021"
                   MOVE    "P027"          TO  SPA-P02-GAMEN
                   MOVE    "N03"           TO  SPA-P02-WIDGET
               WHEN    "P024"
                   MOVE    "P021"          TO  SPA-P02-GAMEN
                   MOVE    "N04"           TO  SPA-P02-WIDGET
               WHEN    "P025"
                   MOVE    "P024"          TO  SPA-P02-GAMEN
                   MOVE    "N07"           TO  SPA-P02-WIDGET
           END-EVALUATE
      *
           MOVE    SPA-P02-WIDGET     TO  MCP-WIDGET
      *    画面ＳＰＡ編集のみ（各画面の初期編集）
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    ZERO                TO  SPA-TAGU-FLG
           MOVE    SPACE               TO  SPA-MOTOPG
           PERFORM 3002-GMNSET-SEC
           MOVE    SPACE              TO  SPA-P02-WIDGET
           MOVE    ZERO               TO  SPA-FLG-END
      *
           .
       430-BETUGAMEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ切り替え処理
      *****************************************************************
       460-TAGUCHG-SEC           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
      *    患者指定がない時、チェックなし
           IF      SPA-GMN-PTNUM       =   SPACE
               CONTINUE
           ELSE
      *        表示画面の登録処理
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               MOVE    1                   TO  SPA-TAGU-FLG
               PERFORM 3002-GMNSET-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                  GO  TO                  460-TAGUCHG-EXT
               END-IF
           END-IF
      *
           EVALUATE    WRK-WIDGET
      *        受付
               WHEN    "N01"
                   MOVE    "P020"          TO  SPA-P02-GAMEN
      *        基本
               WHEN    "N02"
                   MOVE    "P02 "         TO  SPA-P02-GAMEN
      *        保険組合せ
               WHEN    "N03"
                   MOVE    "P027"         TO  SPA-P02-GAMEN
      *        連絡先
               WHEN    "N04"
                   MOVE    "P021"         TO  SPA-P02-GAMEN
      *        禁忌
      *********WHEN    "N05"
      *            MOVE    "P022"         TO  SPA-P02-GAMEN
      *        紹介
      **       WHEN    "N06"
      *++             MOVE    "P023"         TO  SPA-P02-GAMEN
      *        履歴
               WHEN    "N07" 
                   MOVE    "P024"          TO  SPA-P02-GAMEN
      *        旧姓
               WHEN    "N08"
                   MOVE    "P025"          TO  SPA-P02-GAMEN
           END-EVALUATE
      *
      *    画面ＳＰＡ編集のみ（各画面の初期編集）
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    ZERO                TO  SPA-TAGU-FLG
           MOVE    SPACE               TO  SPA-MOTOPG
           PERFORM 3002-GMNSET-SEC
      *
           MOVE    SPACE              TO  SPA-P02-WIDGET
           MOVE    ZERO               TO  SPA-FLG-END
      *
           .
       460-TAGUCHG-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 4100-PTNUM-SYORI-SEC
      *        各画面へ
               WHEN    OTHER
                   IF     (SPA-GMN-PTNUM   NOT =   SPACE  )  AND
                          (SPA-P02-SYORI   NOT =   ZERO   )
                       MOVE    "1"             TO  SPA-UPDFLG
                       MOVE    ZERO            TO  SPA-TAGU-FLG
                       PERFORM 3002-GMNSET-SEC
                   ELSE
                       MOVE    "1001"              TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       4100-PTNUM-SYORI-SEC            SECTION.
      *
      *    排他制御解除処理
           IF      SPA-GMN-PTID    NOT =   ZERO
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    ZERO                TO  SPA-P02-SYORI
           MOVE    P02-PTNUM           TO  SPA-GMN-PTNUM
           IF      SPA-GMN-PTNUM(1:1)      =   "*"
               MOVE    ZERO                TO  SPA-GMN-PTID
      *        新規自動採番
               IF      SPA-1009-PTNUMKSIKBN    =   "1"
      *            自由構成
                   MOVE    "1002"              TO  SPA-ERRCD
               ELSE
                   MOVE    ZERO                TO  FLG-HAKKOU
                   PERFORM 41001-AUTONUM-SEC
               END-IF
               GO      TO     4100-PTNUM-SYORI-EXT
           END-IF
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "1009"              TO  SPA-ERRCD
               GO      TO     4100-PTNUM-SYORI-EXT
           END-IF
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    00
      *            患者あり
                   MOVE    SPTNUM-PTID         TO  SPTID-PTID
                   MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
                   IF     (SPA-P02-UKEID       NOT =   ZERO   )  AND
                          (SPA-PTNUM               =   SPACE  )  AND
                          (SPA-PTID                =   ZERO   )  AND
                          (SPA-NAME            NOT =   SPACE  )
                       MOVE    "1010"              TO  SPA-ERRCD
                       GO      TO      4100-PTNUM-SYORI-EXT
                   END-IF
               WHEN    20
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
      *            患者検索へ
                   MOVE    SPACE               TO  LINKAREA
                   MOVE    SPA-KYOUTU          TO  LNK-COMMON
                   MOVE    SPA-P02SCRAREA      TO  LNK-SCRSPA
      *            画面移行用のパラメタ変更
                   MOVE    "P02"               TO  SPA-MOTOPG
                   MOVE    "P97"               TO  SPA-SAKIPG
      *            氏名セット
                   MOVE    SPA-GMN-PTNUM       TO  SPA-NAME
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "P97"               TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
                   GO      TO     4100-PTNUM-SYORI-EXT
               WHEN    10
      *            新規患者検索へ
                   IF      SPTNUM-SYUBETU       =   1
                       PERFORM 41002-PTNUMCHK-SEC
                   ELSE
                       MOVE    "1003"              TO  SPA-ERRCD
                   END-IF
                   GO      TO     4100-PTNUM-SYORI-EXT
               WHEN    OTHER
      *            エラー
                   MOVE    "1003"              TO  SPA-ERRCD
                   GO      TO     4100-PTNUM-SYORI-EXT
           END-EVALUATE
      *
      *    患者マスターあり
           INITIALIZE                  SPA-P02SCRAREA
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID
      *    患者マスター存在チェック
           MOVE    SPA-HOSPID         TO  PTINF-HOSPID
           MOVE    SPA-GMN-PTID       TO  PTINF-PTID
           PERFORM  900-PTINF-READ-SEC
           IF      FLG-PTINF      NOT =   ZERO
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    "1003"              TO  SPA-ERRCD
               GO      TO      4100-PTNUM-SYORI-EXT
           END-IF
           MOVE    "P02 "              TO  SPA-P02-GAMEN
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4100-PTNUM-SYORI-EXT
           END-IF
      *
           MOVE    2                   TO  SPA-P02-SYORI
           MOVE    1                   TO  SPA-GMN-CUR
           PERFORM 310-SPA-SET-SEC
           MOVE    SPACE               TO  SPA-P02-WIDGET
      *
      *    前回患者
           MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
      *
           .
       4100-PTNUM-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者番号入力チェック処理
      *****************************************************************
       41002-PTNUMCHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-PTID
      *
           IF      SPTNUM-PTNUM    NOT =   SPACE
               MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
           END-IF
      *
      *    桁数チェック
           MOVE    ZERO                TO  WRK-KETA
           MOVE    ZERO                TO  FLG-KETA
           PERFORM VARYING     IDX     FROM    20  BY  -1
                   UNTIL       IDX         <   1
               IF      SPA-GMN-PTNUM(IDX:1)    NOT =   SPACE
                   MOVE    IDX                 TO  WRK-KETA
                   MOVE    1                   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      WRK-KETA            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
           END-IF
      *
      *    間に空白がある時エラー
           MOVE    ZERO                TO  FLG-KETA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   WRK-KETA
               IF      SPA-GMN-PTNUM(IDX:1)    =   SPACE
                   IF      FLG-KETA            =   1
                       MOVE    "1005"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-KETA
               END-IF
           END-PERFORM
      *
           IF      WRK-KETA            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41002-PTNUMCHK-EXT
           END-IF
      *
      *    患者構成チェック
           INITIALIZE                  ORCSP01AREA
           MOVE    "2"                 TO  ORCSP01-SYORI
           MOVE    SPA-GMN-PTNUM       TO  ORCSP01-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCSP01-PTID
      *
           CALL    "ORCSP01"           USING
                                       ORCSP01AREA
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-HZN-PTNUM       TO  WRK-HZN-PTNUM
               MOVE    SPA-HZN-MOTOPG      TO  WRK-HZN-MOTOPG
               INITIALIZE                  SPA-P02SCRAREA
               INITIALIZE                  SPA-P02KYOUTU
               MOVE    WRK-HZN-PTNUM       TO  SPA-HZN-PTNUM
               MOVE    WRK-HZN-MOTOPG      TO  SPA-HZN-MOTOPG
               MOVE    ORCSP01-PTNUM       TO  SPA-GMN-PTNUM
               MOVE    ORCSP01-PTID        TO  SPA-GMN-PTID
               MOVE    1                   TO  SPA-P02-SYORI
               MOVE    "P02 "              TO  SPA-P02-GAMEN
               PERFORM 310-SPA-SET-SEC
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-P02-WIDGET
           END-IF
      *
           .
       41002-PTNUMCHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号自動連番処理
      *****************************************************************
       41001-AUTONUM-SEC           SECTION.
      *
      *
           EVALUATE    SPA-1009-PTNUMKSIKBN
      *    1:自由構成
               WHEN    "1"
                   CONTINUE
      *    2:標準構成
               WHEN    "2"
                   PERFORM 410011-AUTO-SYORI-SEC
      *    3:拡張構成
               WHEN    "3"
                   PERFORM 410012-P014-SET-SEC
           END-EVALUATE
      *
           .
       41001-AUTONUM-EXT.
           EXIT.
      *****************************************************************
      *    患者番号自動採番処理
      *****************************************************************
       410011-AUTO-SYORI-SEC           SECTION.
      *
           INITIALIZE                  ORCSP01AREA
           IF      FLG-HAKKOU          =   1
               MOVE    "3"                 TO  ORCSP01-SYORI
           ELSE
               MOVE    "1"                 TO  ORCSP01-SYORI
           END-IF
           MOVE    SPA-GMN-PTNUM       TO  ORCSP01-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCSP01-PTID
      *
           CALL    "ORCSP01"           USING
                                       ORCSP01AREA
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE  OR  "8000"
               MOVE    SPA-ERRCD           TO  WRK-ERRCD
               MOVE    SPACE               TO  SPA-ERRCD
      *
               MOVE    ORCSP01-PTNUM       TO  SPA-GMN-PTNUM
               MOVE    ORCSP01-PTID        TO  SPA-GMN-PTID
               MOVE    1                   TO  SPA-P02-SYORI
               MOVE    "P02 "              TO  SPA-P02-GAMEN
               PERFORM 310-SPA-SET-SEC
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-P02-WIDGET
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-ERRCD           TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       410011-AUTO-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    拡張番号編集画面へ（Ｐ０１４）
      *****************************************************************
       410012-P014-SET-SEC           SECTION.
      *
           INITIALIZE                  ORCSP01AREA
           IF      FLG-HAKKOU          =   1
               MOVE    "3"                 TO  ORCSP01-SYORI
           ELSE
               MOVE    "1"                 TO  ORCSP01-SYORI
           END-IF
           MOVE    SPA-GMN-PTNUM       TO  ORCSP01-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCSP01-PTID
      *
           CALL    "ORCSP01"           USING
                                       ORCSP01AREA
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE  OR  "8000"
               CONTINUE
           ELSE
               GO      TO      410012-P014-SET-EXT
           END-IF
           MOVE    SPA-ERRCD           TO  WRK-ERRCD
           MOVE    SPACE               TO  SPA-ERRCD
      *
           MOVE    ORCSP01-PTID        TO  SPA-GMN-PTID
           INITIALIZE                  SPA-P014-AREA
           MOVE    ORCSP01-PTNUM1      TO  SPA-GMN014-PTNUM1
           MOVE    ORCSP01-PTNUM2      TO  SPA-GMN014-PTNUM2
           MOVE    ORCSP01-PTNUM3      TO  SPA-GMN014-PTNUM3
      *
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP014"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P014"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "P014"              TO  MCP-WINDOW.
      *    カーソル移動
           EVALUATE    SPA-1009-KKCKSIKBN
      *        拡張構成 <フリー + 連番号 + フリー>
               WHEN    "1"
                   MOVE    "PTNUM1"            TO  MCP-WIDGET
      *        拡張構成 <フリー + 連番号>
               WHEN    "2"
                   MOVE    "PTNUM1"            TO  MCP-WIDGET
      *        拡張構成 <連番号 + フリー>
               WHEN    "3"
                   MOVE    "PTNUM2"            TO  MCP-WIDGET
           END-EVALUATE
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410012-P014-SET-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       410-CLREA-SEC           SECTION.
      *
           MOVE    SPA-HZN-PTNUM       TO  WRK-HZN-PTNUM
           MOVE    SPA-HZN-MOTOPG      TO  WRK-HZN-MOTOPG
           INITIALIZE                  SPA-P02SCRAREA
           INITIALIZE                  SPA-P02KYOUTU
           MOVE    SPACE               TO  SPA-NAME
           MOVE    WRK-HZN-PTNUM       TO  SPA-HZN-PTNUM
           MOVE    WRK-HZN-MOTOPG      TO  SPA-HZN-MOTOPG
           IF     (SPA-HZN-MOTOPG      =   "U02" )  AND
                  (SPA-HZN-PTNUM       =   SPACE )
               CONTINUE
           ELSE
               MOVE    ZERO                TO  SPA-P02-UKEID
           END-IF
      *
           MOVE    "P02"               TO  SPA-P02-GAMEN
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           PERFORM 310-SPA-SET-SEC
      *
           .
       410-CLREA-EXT.
           EXIT.
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       420-ZENPTNUM-SEC           SECTION.
      *
           IF      SPA-LAST-PTNUM      NOT =   SPACE
               MOVE    SPA-LAST-PTNUM      TO  P02-PTNUM
               PERFORM 4100-PTNUM-SYORI-SEC
           END-IF
      *
           .
       420-ZENPTNUM-EXT.
           EXIT.
      *****************************************************************
      *    再発行処理
      *****************************************************************
       450-SAINUM-SEC           SECTION.
      *    患者ＩＤがあり、新規の時
           IF     (SPA-GMN-PTNUM   NOT =   ZERO )  AND
                  (SPA-P02-SYORI       =   1    )
      *        自由構成の時、登録
               MOVE    1                   TO  FLG-HAKKOU
               PERFORM 41001-AUTONUM-SEC
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
           END-IF
      *
           .
       450-SAINUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *----(01.00.03) LINE ADD START ----------------------------------
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0101"          TO  SPA-PIDCD
               GO      TO              210-BACK-EXT
           END-IF
      *----(01.00.03) LINE ADD END   ----------------------------------
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    パラメタファイル削除
           PERFORM 992-PARA-DEL-SEC
      *
           IF      SPA-HZN-MOTOPG(1:3) =   "U02"  OR  "K02" OR "Q02"
                                                  OR  "I01"
               MOVE    "P02"               TO  SPA-MOTOPG
               MOVE    SPACE               TO  SPA-MOTOPG2
               MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
      *
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE
               MOVE    "P02"               TO  SPA-MOTOPG
               MOVE    SPACE               TO  SPA-MOTOPG2
               MOVE    "M01"               TO  SPA-SAKIPG
      *
               MOVE    SPACE               TO  LINKAREA
               INITIALIZE                      SPA-KYOTUKEY
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           END-IF
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC               SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    パラメタファイル削除
           PERFORM 992-PARA-DEL-SEC
      *
           IF      SPA-HZN-MOTOPG(1:3) =   "U02"  OR  "K02" OR "Q02"
                                                  OR  "I01"
      *********MOVE    SPA-MACHIPG         TO  SPA-SAKIPG
               MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
               MOVE    "P02"               TO  SPA-MOTOPG
               MOVE    SPACE               TO  SPA-MOTOPG2
               MOVE    SPACE               TO  SPA-ERRMSG
      *
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE
               MOVE    "P02"               TO  SPA-MOTOPG
               MOVE    SPACE               TO  SPA-MOTOPG2
      *
               MOVE    "M01"               TO  SPA-SAKIPG
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       460-YOYAKU-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-P02SCRAREA      TO  LNK-SCRSPA
      *
           INITIALIZE                  SPA-KYOTUKEY
      *    画面移行用のパラメタ変更
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "P02"               TO  SPA-MOTOPG3
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "Y01    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       460-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-P02SCRAREA      TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "U01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-P02SCRAREA      TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-SIMEIKEN-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       499-DELETE-SEC             SECTION.
      *
           IF     (SPA-P02-SYORI       =   2      )   AND
                  (SPA-GMN-PTID    NOT =   ZERO   )
               MOVE    "0103"              TO  SPA-PIDCD
           END-IF
      *
           .
       499-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4991-DELETE-SYORI-SEC             SECTION.
      *
      *    ファイル削除
           PERFORM 49911-DB-DELETE-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4991-DELETE-SYORI-EXT
           END-IF
      *
      *****IF      SPA-MACHIPG         =   "U02"  OR  "K02" OR "Q02"
           IF      SPA-HZN-MOTOPG(1:3) =   "U02"  OR  "K02" OR "Q02"
                                                  OR  "I01"
               IF      SPA-HZN-PTNUM       =   SPACE
                   CONTINUE
               ELSE
      *            更新後、元の画面へ
                   INITIALIZE                  SPA-KYOTUKEY
                   PERFORM 210-BACK-SYORI-SEC
                   GO      TO      4991-DELETE-SYORI-EXT
               END-IF
           END-IF
      *
      *    各画面を初期編集する
           MOVE    SPA-HZN-PTNUM       TO  WRK-HZN-PTNUM
           MOVE    SPA-HZN-MOTOPG      TO  WRK-HZN-MOTOPG
           INITIALIZE                  SPA-P02SCRAREA
           INITIALIZE                  SPA-P02KYOUTU
           MOVE    WRK-HZN-PTNUM       TO  SPA-HZN-PTNUM
           MOVE    WRK-HZN-MOTOPG      TO  SPA-HZN-MOTOPG
           MOVE    "P02"               TO  SPA-P02-GAMEN
           MOVE    ZERO                TO  SPA-GMN-CUR
           PERFORM 310-SPA-SET-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           .
      *
       4991-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       49911-DB-DELETE-SEC             SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    患者情報削除
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1000"              TO  SPA-ERRCD
               GO      TO    49911-DB-DELETE-EXT
           END-IF
      *    保険組合せ
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-GMN-PTID            TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-DEL1"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    受診履歴
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID            TO  JYURRK-PTID
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "JYURRK-DEL1"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    診療科履歴
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPID          TO  KARRK-HOSPID
           MOVE    SPA-GMN-PTID            TO  KARRK-PTID
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SRYKARRK-DEL1"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    患者病名
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-GMN-PTID            TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    患者コメント
           INITIALIZE                      PTCOM-REC
           MOVE    SPA-HOSPID          TO  PTCOM-HOSPID
           MOVE    SPA-GMN-PTID            TO  PTCOM-PTID
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTCOM-DEL2"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    患者番号変換
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPID          TO  PTNUM-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTNUM-DEL1"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    患者保険情報
           INITIALIZE                      PTHKNINF-REC
           MOVE    SPA-HOSPID          TO  PTHKN-HOSPID
           MOVE    SPA-GMN-PTID            TO  PTHKN-PTID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTHKNINF-DEL1"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    患者公費情報
           INITIALIZE                      PTKOHINF-REC
           MOVE    SPA-HOSPID          TO  PTKOH-HOSPID
           MOVE    SPA-GMN-PTID            TO  PTKOH-PTID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTKOHINF-DEL1"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    旧姓履歴
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPID          TO  KYUSEI-HOSPID
           MOVE    SPA-GMN-PTID        TO  KYUSEI-PTID
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "KYUSEIRRK-DEL1"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    診療会計
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-GMN-PTID            TO  ACCT-PTID
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SRYACCT-DEL1"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    診療行為
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPID          TO  SRY-HOSPID
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SRYACT-DEL1"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    収納
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SYUNOU-DEL1"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    収納
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPID          TO  SMEI-HOSPID
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SYUMEI-DEL1"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    低所得者
           INITIALIZE                      TSYRRK-REC
           MOVE    SPA-HOSPID          TO  TSYRRK-HOSPID
           MOVE    SPA-GMN-PTID            TO  TSYRRK-PTID
           MOVE    TSYRRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "TSYRRK-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    紹介履歴
           INITIALIZE                      SYKRRK-REC
           MOVE    SPA-HOSPID          TO  SYK-HOSPID
           MOVE    SPA-GMN-PTID            TO  SYK-PTID
           MOVE    SYKRRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SYKRRK-DEL1"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    受付
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPID          TO  UKE-HOSPID
           MOVE    SPA-GMN-PTID        TO  UKE-PTID
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "UKETUKE-DEL1"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    年金履歴
           INITIALIZE                      TNKRRK-REC
           MOVE    SPA-HOSPID          TO  TNKRRK-HOSPID
           MOVE    SPA-GMN-PTID            TO  TNKRRK-PTID
           MOVE    TNKRRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "TNKRRK-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    算定履歴
           INITIALIZE                      SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-GMN-PTID            TO  SANTEI-PTID
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SANTEI-DEL1"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    ワーク診療行為
           INITIALIZE                      WKSRYACT-REC
           MOVE    SPA-HOSPID          TO  WKSRY-HOSPID
           MOVE    SPA-GMN-PTID            TO  WKSRY-PTID
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "WKSRYACT-DEL1"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    請求管理
           INITIALIZE                      RECE10-REC
           MOVE    SPA-HOSPID          TO  RECE10-HOSPID
           MOVE    SPA-GMN-PTID        TO  RECE10-PTID
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SEIKYU-DEL1"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
      *    レセプトコメント
           INITIALIZE                      RECECOM-REC
           MOVE    SPA-HOSPID          TO  RECECOM-HOSPID
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "RECECOM-DEL1"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
      *    労災保険情報
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTRSIINF-DEL1"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    月代り受給者番号
           INITIALIZE                      MONTHLYNUM-REC
           MOVE    SPA-HOSPID          TO  MONTHLYNUM-HOSPID
           MOVE    SPA-GMN-PTID        TO  MONTHLYNUM-PTID
           MOVE    MONTHLYNUM-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "MONTHLYNUM-DEL1"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    予約
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-PTID        TO  YYK-PTID
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "YYK-KEY6"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "YYK-KEY6"          TO  ORC-DBPATH
               PERFORM 900-YOYAKU-READ-SEC
            ELSE
                MOVE    1              TO  FLG-YYK
            END-IF
            PERFORM UNTIL    FLG-YYK   =   1
      *
      *        予約件数の減算
                MOVE    YYK-REC            TO  WRK-YYK-REC
                MOVE    ZERO               TO  YYK-YYKID
                MOVE    YYK-REC            TO  MCPDATA-REC
                MOVE    "DBSELECT"         TO  MCP-FUNC
                MOVE    "YYK-KEY"          TO  ORC-DBPATH
                CALL    "ORCMCPSUB"         USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "YYK-KEY"           TO  ORC-DBPATH
                   PERFORM 900-YOYAKU-READ-SEC
                   COMPUTE YYK-YYKCNT          =   YYK-YYKCNT
                                               -   1
      *
                   MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *            更新
                   MOVE    YYK-REC             TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "YYK-KEY"           TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               END-IF
      *        削除
               MOVE    WRK-YYK-REC         TO  YYK-REC
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "YYK-KEY"           TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               MOVE    "YYK-KEY6"          TO  ORC-DBPATH
               PERFORM 900-YOYAKU-READ-SEC
            END-PERFORM
      *
      *    入院診療行為マスター
           INITIALIZE                      NYUINACT-REC
           MOVE    SPA-HOSPID          TO  NSRY-HOSPID
           MOVE    SPA-GMN-PTID        TO  NSRY-PTID
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "NYUINACT-DEL12"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    入院診療会計マスター
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "NYUINACCT-DEL1"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    患者入院履歴
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTNYUINRRK-DEL2"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    患者他院歴付加情報
           INITIALIZE                      PTTAINFUKA-REC
           MOVE    SPA-HOSPID          TO  PTTAINFUKA-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTTAINFUKA-PTID
           MOVE    PTTAINFUKA-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTTAINFUKA-DEL2"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    患者定期請求履歴
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPID          TO  PTTEIKIRRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTTEIKIRRK-PTID
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTTEIKIRRK-DEL1"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    地方公費請求書
           INITIALIZE                      KOHSKY-REC
           MOVE    SPA-HOSPID          TO  KOHSKY-HOSPID
           MOVE    SPA-GMN-PTID        TO  KOHSKY-PTID
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "KOHSKY-DEL3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    公費負担額
           INITIALIZE                      PTKOHFTN-REC
           MOVE    SPA-HOSPID          TO  PTKOHFTN-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTKOHFTN-PTID
           MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PTKOHFTN-DEL1"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       49911-DB-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
      **   画面の内容を保存＆更新前チェック
           PERFORM 4901-KOUSINMAE-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO  TO                  490-TOUROKU-EXT
           END-IF
      *
      *    ＣＬＡＩＭ接続チェック処理
           MOVE    ZERO                TO  FLG-CLAIM
           PERFORM 4905-CLAIM-CHK-SEC
           IF      FLG-CLAIM       =   2
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
      *    更新処理
           MOVE    ZERO                TO  FLG-CLAIM-OK
           PERFORM 4902-KOUSIN-SYORI-SEC
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4902-KOUSIN-SYORI-SEC           SECTION.
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
           MOVE    SPA-AREA        TO  SPA-COMMON
      *    更新サブへ
           CALL    "ORCGP029"      USING   MCPAREA
                                           SPAAREA
                                           LINKAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4902-KOUSIN-SYORI-EXT
           END-IF
      *
           IF      SPA-KEI-ERRCD   NOT =   SPACE
               MOVE    SPA-KEI-ERRCD       TO  SPA-PIDCD
               MOVE    1                   TO  SPA-KEI-FLG
               GO      TO      4902-KOUSIN-SYORI-EXT
           END-IF
      *
      *    登録後、処理
           PERFORM  49021-KOUSIN-AFTER-SEC
           .
       4902-KOUSIN-SYORI-EXT.
           EXIT.
      *----(01.00.05) LINE UPD START ----------------------------------
      *
      *****************************************************************
      *    登録後、処理
      *****************************************************************
       49021-KOUSIN-AFTER-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-KEI-ERRCD
      *
      *    更新済
           MOVE    SPACE               TO  SPA-UPDFLG
      *    前回患者へ
           MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
      *
      *    ＣＬＡＩＭ送信
           IF      FLG-CLAIM           =   1
               PERFORM 4906-CLAIM-SOUSIN-SEC
           END-IF
      *
      **** IF      SPA-MACHIPG         =   "U02"  OR  "K02" OR "Q02"
           IF      SPA-HZN-MOTOPG(1:3) =   "U02"  OR  "K02" OR "Q02"
                                                  OR  "I01"
               IF     (SPA-HZN-PTNUM       =   SPACE  )  AND
                      (SPA-P02-UKEID       =   ZERO   )
                   CONTINUE
               ELSE
      *
                   MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
                   MOVE    SPA-GMN-PTID        TO  SPA-PTID
                   MOVE    SPACE               TO  SPA-ERRMSG
      *            更新後、元の画面へ
                   IF     (FLG-CLAIM-OK        =   ZERO )  AND
                          (SPA-KEI-FLG         =   ZERO )
                       PERFORM 210-BACK
                   ELSE
                       PERFORM 210-BACK-SYORI-SEC
                   END-IF
                   MOVE    ZERO                TO  SPA-KEI-FLG
                   GO      TO      49021-KOUSIN-AFTER-EXT
               END-IF
           END-IF
           MOVE    ZERO                TO  SPA-KEI-FLG
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    各画面を初期編集する
           MOVE    SPA-HZN-PTNUM       TO  WRK-HZN-PTNUM
           MOVE    SPA-HZN-MOTOPG      TO  WRK-HZN-MOTOPG
           INITIALIZE                  SPA-P02SCRAREA
           INITIALIZE                  SPA-P02KYOUTU
           MOVE    WRK-HZN-PTNUM       TO  SPA-HZN-PTNUM
           MOVE    WRK-HZN-MOTOPG      TO  SPA-HZN-MOTOPG
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    "P02"               TO  SPA-P02-GAMEN
           MOVE    ZERO                TO  SPA-GMN-CUR
           PERFORM 310-SPA-SET-SEC
      *
           .
       49021-KOUSIN-AFTER-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4901-KOUSINMAE-SEC             SECTION.
      *
           IF    ( SPA-GMN-PTNUM       =   SPACE   )  OR
                 ((SPA-P02-SYORI   NOT =   1     ) AND
                  (SPA-GMN-PTID        =   ZERO  ))
               MOVE    ZERO                TO  SPA-GMN-CUR
               MOVE    "0009"              TO  SPA-ERRCD
               GO  TO                  4901-KOUSINMAE-EXT
           END-IF
      *
      **   画面の内容を保存＆更新前チェック
      *    受付(タグ変更時・切替押下時、未更新のため、ここで必ず更新)
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
           CALL    "ORCGP020"      USING   MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO                  4901-KOUSINMAE-EXT
           END-IF
      *
      ***  所得(タグ変更時・切替押下時、未更新のため、ここで必ず更新)
      *    MOVE    "CLICKED" TO  MCP-EVENT
      *    MOVE    "B12"     TO  MCP-WIDGET
      *    MOVE    "PUTG"    TO  MCP-STATUS
      *    CALL    "ORCGP024"       USING   MCPAREA
      *                                            SPAAREA
      *                                            LINKAREA
      *                                            SCRAREA
      *    MOVE    SPA-COMMON          TO  SPA-AREA
      *    MOVE    SPA-FREE            TO  SPA-P02SCRAREA
      *    MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *    IF  SPA-ERRCD           NOT =   SPACE
      *        MOVE    "P024"              TO  SPA-P02-GAMEN
      *        GO  TO                  4901-KOUSINMAE-EXT
      **** END-IF
      *
      **** EVALUATE    SPA-P02-GAMEN
      *    基本
      *****WHEN    "P02 "
      *    基本情報チェック
               MOVE    "CLICKED" TO  MCP-EVENT
               MOVE    "B12"     TO  MCP-WIDGET
               MOVE    "PUTG"    TO  MCP-STATUS
               CALL    "ORCGP02W1"      USING   MCPAREA
                                                SPAAREA
                                                LINKAREA
                                                SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
               IF     (SPA-ERRCD           NOT =   SPACE )  OR
                      (SPA-PIDCD           NOT =   SPACE )
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
                   GO      TO                  4901-KOUSINMAE-EXT
               END-IF
      *
      *    所得(タグ変更時・切替押下時、未更新のため、ここで必ず更新)
           MOVE    "CLICKED" TO  MCP-EVENT
           MOVE    "B12"     TO  MCP-WIDGET
           MOVE    "PUTG"    TO  MCP-STATUS
           CALL    "ORCGP024"       USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           IF  SPA-ERRCD           NOT =   SPACE
               MOVE    "P024"              TO  SPA-P02-GAMEN
               GO  TO                  4901-KOUSINMAE-EXT
           END-IF
      *
      *    -------------------------
           EVALUATE    SPA-P02-GAMEN
      *
      *    住所
           WHEN    "P021"
               MOVE    "CLICKED" TO  MCP-EVENT
               MOVE    "B12"     TO  MCP-WIDGET
               MOVE    "PUTG"    TO  MCP-STATUS
               CALL    "ORCGP021"       USING   MCPAREA
                                                SPAAREA
                                                LINKAREA
                                                SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
               IF  SPA-ERRCD           NOT =   SPACE
                   MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
                   GO  TO                  4901-KOUSINMAE-EXT
               END-IF
      *    -------------------------
      *
      *    紹介
      *     WHEN    "P023"
      *         MOVE    "CLICKED" TO  MCP-EVENT
      *         MOVE    "B12"     TO  MCP-WIDGET
      *         MOVE    "PUTG"    TO  MCP-STATUS
      *         CALL    "ORCGP023"       USING   MCPAREA
      *                                          SPAAREA
      *                                          LINKAREA
      *                                          SCRAREA
      *         MOVE    SPA-COMMON          TO  SPA-AREA
      *         MOVE    SPA-FREE            TO  SPA-P02SCRAREA
      *         MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *         IF  SPA-ERRCD           NOT =   SPACE
      *             GO  TO                  4901-KOUSINMAE-EXT
      *         END-IF
      *    -------------------------
      *
      *     WHEN    "P024"
      *
      *    -------------------------
      *
      *    入力履歴
           WHEN    "P025"
               MOVE    "CLICKED" TO  MCP-EVENT
               MOVE    "B12"     TO  MCP-WIDGET
               MOVE    "PUTG"    TO  MCP-STATUS
               CALL    "ORCGP025"       USING   MCPAREA
                                                SPAAREA
                                                LINKAREA
                                                SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
               IF  SPA-ERRCD           NOT =   SPACE
                   MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
                   GO  TO                  4901-KOUSINMAE-EXT
               END-IF
      *
           END-EVALUATE
      *
      *
      *    患者番号存在チェック（新規の時、存在しないこと）
           IF      SPA-P02-SYORI       =   1
               PERFORM 491010-PTNUM-DBCHECK-SEC
               IF  SPA-ERRCD           NOT =   SPACE
                   GO  TO                  4901-KOUSINMAE-EXT
               END-IF
           END-IF
      *    患者番号変換マスタ　変更チェック
      *****排他制御がかかるはずなので、削除した
      *    PERFORM 4910-PTNUM-DBCHECK-SEC
      *    IF  SPA-ERRCD           NOT =   SPACE
      *        GO  TO                  4901-KOUSINMAE-EXT
      *****END-IF
      *
      *
           .
       4901-KOUSINMAE-EXT.
           EXIT.
      *****************************************************************
      *    患者番号変換マスタ　変更チェック
      *****************************************************************
       491010-PTNUM-DBCHECK-SEC          SECTION.
      *
      *    患者番号変換（管理）マスタ読み込み
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPID          TO  PTNUM-HOSPID
           MOVE    SPA-GMN-PTNUM       TO  PTNUM-PTNUM
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTNUM-KEY2"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"      USING   MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "PTNUM-KEY2"    TO  ORC-DBPATH
               CALL    "ORCMCPSUB"      USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
                   MOVE    ZERO            TO  FLG-PTNUM
               ELSE
                   MOVE    1               TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTNUM-KEY2"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"      USING   MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
           IF      FLG-PTNUM           =   ZERO
               MOVE    "1011"              TO  SPA-ERRCD
               GO  TO                  491010-PTNUM-DBCHECK-EXT
           END-IF
      *
      *    患者マスター存在チェック
           IF      SPA-GMN-PTID        =   ZERO 
               GO  TO                  491010-PTNUM-DBCHECK-EXT
           END-IF
           MOVE    SPA-HOSPID         TO  PTINF-HOSPID
           MOVE    SPA-GMN-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF          =   ZERO 
               MOVE    "1011"              TO  SPA-ERRCD
               GO  TO                  491010-PTNUM-DBCHECK-EXT
           END-IF
      *
           .
       491010-PTNUM-DBCHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写処理
      *****************************************************************
       499-COPY-SEC             SECTION.
      *
           IF    ( SPA-GMN-PTNUM       =   SPACE   )  OR
                 ((SPA-P02-SYORI   NOT =   1     ) AND
                  (SPA-GMN-PTID        =   ZERO  ))
               MOVE    "1021"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-CUR
               GO      TO      499-COPY-EXT
           END-IF
      *    新規の時のみ、複写できることとする
           IF      SPA-P02-SYORI   NOT =   1
               MOVE    "1022"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-CUR
               GO      TO      499-COPY-EXT
           END-IF
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P02E"              TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  P02E
           INITIALIZE                      P02E
           INITIALIZE                      SPA-GMN2E-AREA
           MOVE    1                   TO  SPA-GMN2E-CUR
      *    カーソル移動
           MOVE    "PTNUM"             TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02E"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       499-COPY-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4905-CLAIM-CHK-SEC               SECTION.
      *
           INITIALIZE                  SPA-P02X-AREA
      *
           MOVE    ZERO                TO  FLG-CLAIM
      *    ＣＬＡＩＭ接続判定
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
      *    システム管理検索
           MOVE    SYS-9000-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-9000-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      FLG-SYSKANRI        =   1
               CONTINUE
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-9000-REC
      *TEST!!!
      *        MOVE    SPACE               TO  SYS-9000-PT-FLG
      *TEST!!!
               MOVE    SYS-9000-CODE       TO  SPA-GMN-SCODE
               MOVE    1                   TO  SPA-P02X-GMN-SEL
               IF     (SYS-9000-SETUZOKU   =   ZERO )  OR
                      (SYS-9000-PT-FLG NOT =   "1"  )
                   CONTINUE
               ELSE
                   MOVE    SYS-9000-CODE       TO  SPA-P02X-CODE
                   IF      SYS-9000-KANBUTTON (02) =   "T"
                       MOVE    2               TO  FLG-CLAIM
                   ELSE
                       MOVE    1               TO  FLG-CLAIM
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-CLAIM           =   2
               CONTINUE
           ELSE
               GO      TO      4905-CLAIM-CHK-EXT
           END-IF
      *
      *    ＣＬＡＩＭ接続情報
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02X"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
     *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P02X"              TO  SPA-SAKIPG
      *    カーソル移動
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02X"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4905-CLAIM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4906-CLAIM-SOUSIN-SEC               SECTION.
      *
           MOVE    SPACE               TO  SYS-9001-REC
           INITIALIZE                  SYS-9001-REC
           MOVE    "9001"              TO  SYS-9001-KANRICD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
      *    システム管理検索
           MOVE    SYS-9001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-9001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   10   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-9001-REC
               MOVE    SYS-9001-KBNCD (1:2)  TO  WRK-GMN-SELNUM
               IF      WRK-GMN-SELNUM        =   SPA-P02X-GMN-SEL
                 MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG6
                 MOVE    SYS-9001-PORT1      TO  SHELLTBL-ARG7
                 MOVE    99                    TO  IDX
               END-IF
      *
               MOVE    "SYSKANRI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    SPACE               TO  WRK-UKETIME
      *    受付時間取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-HMS        TO  WRK-UKETIME
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    CLAIM-FILE          TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *
      **** MOVE    "1"                 TO  SHELLTBL-ARG1
           MOVE    SPA-GMN-SCODE       TO  SHELLTBL-ARG1
           MOVE    SPA-GMN-PTID        TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG2
           MOVE    "XX"                TO  SHELLTBL-ARG3
           MOVE    WRK-UKETIME         TO  SHELLTBL-ARG4
           MOVE    SPA-HOSPID          TO  SHELLTBL-ARG5
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
           CALL    "MCPSUB"            USING
                   MCPAREA
                   MCPDATA-REC
      *
           .
       4906-CLAIM-SOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       410112-SE-WAFORMAT-YMDEDIT-SEC  SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"    USING   STS-AREA-DAY  LNK-DAY2-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  WRK-HENYMD
           END-IF
           .
       410112-SE-WAFORMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P02"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    "GMN"               TO  ORCSP02-SYORI
           CALL    "ORCSP02"           USING   ORCSP02AREA
                                               SPA-AREA
                                               SPA-P02KYOUTU
                                               SPA-P02SCRAREA
                                               P02
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
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    "PTNUM"             TO   MCP-WIDGET
               GO      TO      5001-MAPCUR-EXT
           END-IF
      *
           IF      SPA-P02-WIDGET      =   SPACE
               EVALUATE    P02-PAGENO
                   WHEN    ZERO
                       MOVE  "P020_SELNUM"     TO   MCP-WIDGET
                   WHEN    1
                       MOVE  "P02_KANANAME"    TO   MCP-WIDGET
                   WHEN    2
                       MOVE  "P027_SELNUM"     TO   MCP-WIDGET
                   WHEN    3
                       MOVE  "P021_FAX"        TO   MCP-WIDGET
                   WHEN    4
                       MOVE  "P023_SELNUM"     TO   MCP-WIDGET
                   WHEN    5
                       MOVE  "P024_TSRSELNUM"  TO   MCP-WIDGET
                   WHEN    6
                       MOVE  "P025_SELNUM1"    TO   MCP-WIDGET
               END-EVALUATE
           ELSE
               MOVE    SPA-P02-WIDGET          TO  MCP-WIDGET
           END-IF
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "カナ氏名は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "日付変換エラー"    TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "日付重複エラー"    TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "未登録予約者は存在していません"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "対象住所が見つかりません"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "削除対象の明細が変更されています"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "必須入力エラー"    TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "これ以上登録できません"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "未入力項目があります。登録できません" 
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "作成可能な組合せ数を超えています"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "組合せ番号の上限を超えています"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "ファイル読込エラー"    TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "他で使用しているので更新できません"
                                               TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "保険番号マスタ確認エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "組合せ作成中、テーブルオーバーしました"
                                               TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "生年月日が未入力です"
                                               TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "変更年月日が重複しています"  TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "正しく選択されていません"
                                       TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "減免事由を正しく選択してください"
                                               TO  SPA-ERRMSG
      *----(01.00.01) LINE ADD START --------------------------------
               WHEN    "0020"
                   MOVE    "生年月日に未来日が入力されました"
                                               TO  SPA-ERRMSG
      *----(01.00.01) LINE ADD END   --------------------------------
               WHEN    "0021"
                   MOVE    "性別は１：男２：女で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "労災・自賠責保険ではありません。"
                                               TO  SPA-ERRMSG
                   MOVE    "労災・自賠責を表示後、選択して下さい。"
                                               TO  SPA-ERRMSG(33:)
               WHEN    "0023"
                   MOVE    "労災・自賠責保険です。選択できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "労災・自賠責保険が保険番号マスタ"
                                               TO  SPA-ERRMSG
                   MOVE    "に登録されていません。"
                                               TO  SPA-ERRMSG(33:)
               WHEN    "0025"
                   MOVE    "カナ氏名は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "漢字氏名は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "通称名は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "生年月日の暦日エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "カルテ発行区分が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "すべて全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "警告！郵便番号は存在しません。"
                                               TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "住所は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "世帯主は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "続柄は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE    "割引率入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE    "状態入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "保険組合せがありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0038"
                   MOVE    "診療科がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0039"
                   MOVE
                    "カルテを発行する時は、診療科を選択して下さい。"
                                               TO  SPA-ERRMSG
      *    保険エラー
               WHEN    "0040"
                   MOVE    "保険者番号検証チェックエラー"
                                               TO  SPA-ERRMSG
               WHEN    "0041"
                   MOVE    "保険者番号桁数エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0042"
                   MOVE    "保険の種別は変更できません。"
                                               TO  SPA-ERRMSG
                   MOVE    "新しく保険を追加して下さい"
                                               TO  SPA-ERRMSG(29:)
               WHEN    "0043"
               MOVE    "労災・自賠責保険はこの画面では変更できません。"
                                               TO  SPA-ERRMSG
                   MOVE    "労災・自賠ボタンを選択して下さい"
                                               TO  SPA-ERRMSG(47:)
               WHEN    "0044"
                   MOVE    "保険の種別がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0045"
                   MOVE    "本人家族区分がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0046"
                   MOVE    "補助区分がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0047"
                   MOVE    "継続区分がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0048"
                   MOVE    "保険の記号は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0049"
                   MOVE    "保険の番号は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0050"
                   MOVE    "資格取得日が入力エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0051"
                   MOVE    "保険有効開始日が入力エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0052"
                   MOVE    "保険有効終了日が入力エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0052"
                   MOVE    "継続の時保険有効終了日を入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0053"
                   MOVE    "継続時の適用終了日以降の終了日です"
                                               TO  SPA-ERRMSG
               WHEN    "0055"
                   MOVE    "被保険者名は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0054"
                   MOVE    "保険有効開始日は必須入力です。"
                                               TO  SPA-ERRMSG
               WHEN    "0056"
                   MOVE    "保険の確認年月日が入力エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0057"
                   MOVE    "保険の確認年月日に未来日が入力されました"
                                               TO  SPA-ERRMSG
               WHEN    "0058"
                   MOVE
                   "保険有効開始日＜有効終了日で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0059"
                   MOVE    "保険の種別を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0060"
                   MOVE     "国保・自費の場合、補助区分は必須入力です"
                                               TO  SPA-ERRMSG
               WHEN    "0061"
                   MOVE     "補助区分は入力できません"
                                               TO  SPA-ERRMSG
               WHEN    "0062"
                   MOVE    "保険有効開始日は必須入力です。"
                                               TO  SPA-ERRMSG
               WHEN    "0063"
                   MOVE    "適用期間がマスタの範囲を超えています"
                                               TO  SPA-ERRMSG
               WHEN    "0064"
                   MOVE    "保険期間が重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "0065"
                   MOVE    "保険番号マスタが存在していません。"
                                               TO  SPA-ERRMSG
               WHEN    "0066"
                   MOVE    "本人のみ入力可能です。"
                                               TO  SPA-ERRMSG
               WHEN    "1065"
                   MOVE    "補助区分は本人の時のみ入力可能です。"
                                               TO  SPA-ERRMSG
               WHEN    "1066"
                   MOVE    "補助区分は老人割合のみ入力可能です。"
                                               TO  SPA-ERRMSG
               WHEN    "0067"
                   MOVE    "保険者番号と法別番号が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "0068"
                   MOVE    "老人補助区分は変更できません。"
                                               TO  SPA-ERRMSG
                   MOVE    "新しい保険を入力して下さい。"
                                               TO  SPA-ERRMSG(33:)
               WHEN    "0069"
                   MOVE    "５年（２年）後日付取得エラー"
                                               TO  SPA-ERRMSG
               WHEN    "1047"
                   MOVE    "下船３月の時継続区分は選択できません。"
                                               TO  SPA-ERRMSG
               WHEN    "1048"
                   MOVE    "特別は日雇特例のみ選択できます。"
                                               TO  SPA-ERRMSG
               WHEN    "1049"
                   MOVE    "特別はＨ１５年４月からの選択です。"
                                               TO  SPA-ERRMSG
               WHEN    "1050"
                   MOVE    "継続はＨ１５年３月３１日までの有効です。"
                                               TO  SPA-ERRMSG
      *    公費エラー
               WHEN    "0070"
                   MOVE    "負担者番号・受給者番号が入力されています"
                                               TO  SPA-ERRMSG
               WHEN    "0071"
                   MOVE    "負担者番号が入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0072"
                   MOVE    "公費の種類がありません"
                                               TO  SPA-ERRMSG
               WHEN    "0073"
                   MOVE    "公費の種類は必須入力です。入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0074"
                   MOVE    "公費の種別は変更できません"
                                               TO  SPA-ERRMSG
               WHEN    "0075"
                   MOVE    "受給者番号は全角・半角の混在はできません。"
                                               TO  SPA-ERRMSG
               WHEN    "0076"
                   MOVE    "受給者番号が入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0077"
                   MOVE    "公費の適用開始日が入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0078"
                   MOVE    "公費の適用終了日が入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0079"
                   MOVE    "公費の確認年月日が入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0080"
                   MOVE    "公費の確認年月日に未来日が入力されました"
                                               TO  SPA-ERRMSG
               WHEN    "0081"
                   MOVE
                   "保険有効開始日＜有効終了日で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0082"
                   MOVE    "負担者番号・受給者番号は入力できません"
                                               TO  SPA-ERRMSG
               WHEN    "0083"
                   MOVE    "負担者番号検証番号エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0084"
                   MOVE    "受給者番号検証番号エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0085"
                 MOVE    "公費に適用年齢の指定があります。"
                                               TO  SPA-ERRMSG
                 MOVE    "適用終了日を入力してください。"
                                               TO  SPA-ERRMSG(33:)
               WHEN    "0086"
                   MOVE    "公費の年齢適用エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "K005"
                   MOVE    "警告！公費の年齢開始日適用エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "K006"
                   MOVE    "警告！公費の年齢終了日適用エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0087"
                   MOVE    "適用開始日時点での年齢が６５歳未満です"
                                               TO  SPA-ERRMSG
               WHEN    "0088"
                   MOVE    "公費期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "0089"
                   MOVE    "国保・生活保護の期間が重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "K001"
                   MOVE
                   "警告！適用開始日時点での年齢が７０歳未満です"
                                               TO  SPA-ERRMSG
               WHEN    "K002"
                   MOVE
                   "警告！適用開始日時点での年齢が７５歳未満です"
                                               TO  SPA-ERRMSG
               WHEN    "K003"
      *            保険者マスタ期限切れ
                   MOVE    SPA-HKNJA-IDOYMD    TO  WRK-SYMD
                   PERFORM 5101-IDOYMD-ERR-SEC
               WHEN    "K004"
      *            保険者マスタ期限切れ（公費）
                   PERFORM 5102-IDOYMD-KOHERR-SEC
               WHEN    "0090"
                   MOVE    "公費の空いている行に追加して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0091"
                   MOVE    "適用期間がマスタの範囲を超えています"
                                               TO  SPA-ERRMSG
               WHEN    "0092"
                   MOVE    "負担者番号との法別番号が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "0093"
                   MOVE    "適用開始日が保険適用の終了日以降です。"
                                               TO  SPA-ERRMSG
               WHEN    "0094"
                   MOVE    "適用開始日が保険適用の開始日以前です。"
                                               TO  SPA-ERRMSG
               WHEN    "0095"
                   MOVE    "老人公費の場合、全角での入力はできません。"
                                               TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE    "日付変換エラーです。"
                                               TO  SPA-ERRMSG
      *    更新前エラー
               WHEN    "0100"
                   MOVE    "カナ氏名は必ず入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE    "漢字氏名は必ず入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0102"
                   MOVE    "生年月日は必ず入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "性別は必ず入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0104"
                   MOVE    "保険の老人割合が入力されていません。"
                                               TO  SPA-ERRMSG
                   MOVE    "保険を再作成して下さい。"
                                               TO  SPA-ERRMSG(37:)
               WHEN    "0105"
                   MOVE    "老人割合が入力されていません。"
                                               TO  SPA-ERRMSG
                   MOVE    "公費を作成して下さい。"
                                               TO  SPA-ERRMSG(31:)
      *    キーエラー
               WHEN    "0110"
                   MOVE    "この画面では選択できません。"
                                               TO  SPA-ERRMSG
      *    保険組合せ
               WHEN    "2701"
                   MOVE    "保険組合せがありません。"
                                               TO  SPA-ERRMSG
      *    連絡先
               WHEN    "2101"
                   MOVE    "連絡先名称は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2102"
                   MOVE    "続柄は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2103"
                   MOVE    "連絡先郵便番号はありません"
                                               TO  SPA-ERRMSG
               WHEN    "2104"
                   MOVE    "連絡先住所は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2105"
                   MOVE    "勤務先名称は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2106"
                   MOVE    "勤務先郵便番号はありません"
                                               TO  SPA-ERRMSG
               WHEN    "2107"
                   MOVE    "勤務先住所は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2108"
                   MOVE    "帰省先名称は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2109"
                   MOVE    "帰省先郵便番号はありません"
                                               TO  SPA-ERRMSG
               WHEN    "2110"
                   MOVE    "帰省先住所は全角で入力して下さい"
                                               TO  SPA-ERRMSG
      *    受付エラー
               WHEN    "2001"
                   MOVE    "受付選択番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2002"
                   MOVE    "保険組合せ番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2003"
                   MOVE    "カルテ選択は１です。再入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2005"
                   MOVE    "処方箋選択は１です。再入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2004"
                   MOVE    "未登録受付者は存在していません"
                                               TO  SPA-ERRMSG
               WHEN    "2008"
                   MOVE    "１０件以上登録できません"
                                               TO  SPA-ERRMSG
               WHEN    "2009"
                   MOVE    "登録できません"    TO  SPA-ERRMSG
               WHEN    "2010"
                   MOVE    "診療科を入力して下さい。" 
                                               TO  SPA-ERRMSG
               WHEN    "2011"
                   MOVE
           "カルテ、処方せんを入力した時は、保険組合せを入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2012"
                   MOVE    "ドウターコードがありません"
                                               TO  SPA-ERRMSG
               WHEN    "2013"
                   MOVE    "診療内容コードがありません"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "患者を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "自由構成です。患者番号を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "患者マスターが存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "新患ではありません。再発行はできません。"
                                               TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号の桁数が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "1006"
                   MOVE    "患者番号の構成が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "1007"
                   MOVE    "患者番号の検証番号が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "1008"
                   MOVE    "患者番号の連番は管理連番以下にして下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1009"
                   MOVE    "新規の患者番号を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1010"
                   MOVE
                   "既に存在する患者番号です。"
                                               TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "患者番号が登録済みです。再度入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1020"
                   MOVE    "患者ＩＤが取得できません。"
                                               TO  SPA-ERRMSG
               WHEN    "1021"
                   MOVE    "患者番号を入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "1022"
                   MOVE    "複写は新規患者の時のみできます。"
                                               TO  SPA-ERRMSG
               WHEN    "2501"
                   MOVE    "選択番号が入力されていません"
                                               TO  SPA-ERRMSG
      *        所得者情報
               WHEN    "2401"
                   MOVE    "低所得者２の履歴番号がありません"
                                               TO  SPA-ERRMSG
               WHEN    "2402"
                   MOVE    "低所得者２の認定開始日がエラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2403"
                   MOVE    "低所得者２の認定終了日がエラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2404"
                   MOVE
                   "低所得者２の認定開始日＜終了日で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2405"
                   MOVE
                   "低所得者２の認定期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "2406"
      ************ MOVE    "老齢福祉年金履歴番号がありません"
                   MOVE    "低所得者１の履歴番号がありません"
                                               TO  SPA-ERRMSG
               WHEN    "2407"
      ************ MOVE    "老齢福祉年金開始日がエラーです"
                   MOVE    "低所得者１の認定終了日がエラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2408"
      *************MOVE    "老齢福祉年金終了日がエラーです"
                   MOVE    "低所得者１の認定終了日がエラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2409"
                   MOVE
      *************"老齢福祉年金開始日＜終了日で入力して下さい"
                   "低所得者１の認定開始日＜終了日で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2410"
                   MOVE
      ************ "老齢福祉年金期間が重複しています。"
                   "低所得者１の認定期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "2411"
      ************ MOVE    "低所得者１の認定日を入力して下さい"
                   MOVE    "低所得者２の認定日を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2412"
      ************ MOVE    "老齢福祉年金履歴日を入力して下さい"
      ************ MOVE    "低所得者２の認定日を入力して下さい"
                   MOVE    "低所得者１の認定日を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2413"
                   MOVE    "長期入院該当年月日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2414"
                   MOVE    "長期入院該当年月日は低所得２の"
                                               TO  SPA-ERRMSG
                   MOVE    "範囲内で入力して下さい"
                                               TO  SPA-ERRMSG(31:)
      *
               WHEN    "2430"
                   MOVE    "公費負担選択番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2431"
                   MOVE    "公費負担選択番号２入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2432"
                   MOVE    "公費負担適用開始日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2433"
                   MOVE    "公費負担適用終了日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2434"
                   MOVE    "公費負担適用期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "2435"
                   MOVE    "公費負担の外来上減額入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2436"
                   MOVE    "公費負担の入院上減額入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2437"
                   MOVE    "公費負担適用期間を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2438"
                   MOVE    "公費負担適用開始日＜終了日で入力して下さい"
                                               TO  SPA-ERRMSG
      *        入力履歴
               WHEN    "2502"
                   MOVE    "選択番号入力エラー"    TO  SPA-ERRMSG
               WHEN    "2503"
                   MOVE    "変更年月日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2504"
                   MOVE    "旧姓履歴を選択して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2506"
                   MOVE    "削除対象の明細が変更されています"
                                               TO  SPA-ERRMSG
               WHEN    "2507"
                   MOVE    "氏名を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2508"
                   MOVE    "受診科履歴選択番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2509"
                   MOVE    "登録できません"    TO  SPA-ERRMSG
               WHEN    "2510"
                   MOVE    "旧姓履歴が２０件以上です。"
                                               TO  SPA-ERRMSG
                   MOVE    "登録できません。"
                                               TO  SPA-ERRMSG(27:)
               WHEN    "2530"
                   MOVE    "全角で入力して下さい"    TO  SPA-ERRMSG
               WHEN    "2517"
                   MOVE    "変更年月日が重複しています"  TO  SPA-ERRMSG
      *        患者番号取得エラー
               WHEN    "8000"
                   MOVE    "自動採番の患者番号が最大値です。"
                                               TO  SPA-ERRMSG
                   MOVE    "初期番号から採番しました。"
                                               TO  SPA-ERRMSG(33:)
               WHEN    "8001"
                   MOVE    "患者ＩＤが最大値です。"
                                               TO  SPA-ERRMSG
                   MOVE    "初期番号から採番しました。"
                                               TO  SPA-ERRMSG(23:)
               WHEN    "8002"
                   MOVE    "自動採番の患者番号が最大値になりました。"
                                               TO  SPA-ERRMSG
                   MOVE    "連番号桁数を変更して下さい。"
                                               TO  SPA-ERRMSG(41:)
               WHEN    "8003"
                   MOVE    "患者ＩＤが登録済です。登録できません。"
                                               TO  SPA-ERRMSG
      *
               WHEN    "9000"
                   MOVE    "ＤＢ更新エラー"
                                               TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "受付ＩＤが９９９９９を超えます。"
                                                TO  SPA-ERRMSG
                   MOVE    "これ以上受付はできません。"
                                                TO  SPA-ERRMSG(33:)
               WHEN    "9002"
                   MOVE    "受付更新エラー"
                                                TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "保険組合更新エラー"
                                                TO  SPA-ERRMSG
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
           END-EVALUATE
           MOVE    MCP-WIDGET          TO  SPA-WIDGET
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *----(01.00.03) LINE ADD START ----------------------------------
      *****************************************************************
      *    保険者マスタ期限切れメッセージ表示理
      *****************************************************************
       5101-IDOYMD-ERR-SEC              SECTION.
      *
      *    日本語変換
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    SPACE               TO  WRK-IDOYMD-H
               STRING      LNK-DAY2-EDTYMD2-G  DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-Y  DELIMITED  BY  SIZE
                           "年"                DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-M  DELIMITED  BY  SIZE
                           "月"                DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-D  DELIMITED  BY  SIZE
                           "日"                DELIMITED  BY  SIZE
                                       INTO    WRK-IDOYMD-H
               END-STRING
           ELSE
               MOVE    SPACE               TO  WRK-IDOYMD-H
           END-IF
           MOVE    SPACE               TO  SPA-ERRMSG
           STRING  "この保険者は、"        DELIMITED  BY  SIZE
                   WRK-IDOYMD-H            DELIMITED  BY  SIZE
                   "に廃止となりました。"  DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
           END-STRING
           .
       5101-IDOYMD-ERR-EXT.
           EXIT.
      *****************************************************************
      *    保険者マスタ期限切れ（公費）メッセージ表示理
      *****************************************************************
       5102-IDOYMD-KOHERR-SEC              SECTION.
      *
           EVALUATE   SPA-GMN-CUR
               WHEN   20
                   MOVE    SPA-PTKOH-IDOYMD (1)    TO  WRK-SYMD
               WHEN   26
                   MOVE    SPA-PTKOH-IDOYMD (2)    TO  WRK-SYMD
               WHEN   32
                   MOVE    SPA-PTKOH-IDOYMD (3)    TO  WRK-SYMD
           END-EVALUATE
      *
           PERFORM 5101-IDOYMD-ERR-SEC
           .
       5102-IDOYMD-KOHERR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   MOVE
                       "内容が修正されています。"
                                       TO  WRK-PIDMSG(1:)
                   MOVE
                        "ＯＫで修正分は反映されずに終了します"
                                       TO  WRK-PIDMSG(25:)
               WHEN    "0103"
                   MOVE
                       "該当患者のデータをすべて削除します。"
                                       TO  WRK-PIDMSG(1:)
                   MOVE
                        "よろしいですか？"
                                       TO  WRK-PIDMSG(37:)
               WHEN    "0104"
                   STRING    SPA-PIDMSG    DELIMITED  BY  SPACE
                             "を老人負担割合へ追加変更します。"
                                           DELIMITED  BY  SIZE
                             "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-PIDMSG
                   END-STRING
               WHEN    "0105"
      **********   MOVE
      *            "保険の期間が重複します。前の保険を終了させます。"
      *                                TO  WRK-PIDMSG
      *            MOVE
      *                 "よろしいですか？"
      *********                        TO  WRK-PIDMSG(47:)
                   STRING     "保険の期間が重複します。前の"
                                           DELIMITED  BY  SIZE
                              SPA-PIDMSG   DELIMITED  BY  SPACE
                              "を終了させます。よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-PIDMSG
                   END-STRING
               WHEN    "0106"
                   MOVE
                   "公費の期間が重複します。前の公費を終了させます。"
                                       TO  WRK-PIDMSG
                   MOVE
                        "よろしいですか？"
                                       TO  WRK-PIDMSG(47:)
               WHEN    "0107"
                   MOVE    SPA-PIDMSG      TO  WRK-PIDMSG
      *        Ｈ１５．４改正から
               WHEN    "0201"
                   MOVE
                   "保険が終了しています。終了日を変更します。"
                                       TO  WRK-PIDMSG
                   MOVE
                        "よろしいですか？"
                                       TO  WRK-PIDMSG(43:)
               WHEN    "0202"
                   MOVE
                   "保険を切り替えます。よろしいですか？"
                                       TO  WRK-PIDMSG
               WHEN    "0203"
                   MOVE
                   "継続は平成１５年３月末で終了しています。"
                                       TO  WRK-PIDMSG
                   MOVE
                        "終了日を変更します。よろしいですか？"
                                       TO  WRK-PIDMSG(41:)
               WHEN    "0204"
                   MOVE
                   "任継を切り替えます。切り替え後の終了日を"
                                       TO  WRK-PIDMSG
                   MOVE
                        "確認して下さい。"
                                       TO  WRK-PIDMSG(41:)
      *
               WHEN    "9100"
                   MOVE    "保険組合の変更で保険期間外の診療が"
                                               TO  WRK-PIDMSG
                   MOVE    "存在します。診療を保険変更して下さい。"
                                               TO  WRK-PIDMSG(35:)
               WHEN    "9101"
                   MOVE    "保険組合の変更で保険期間外の病名が"
                                               TO  WRK-PIDMSG
                   MOVE    "存在します。病名を保険変更して下さい。"
                                               TO  WRK-PIDMSG(35:)
               WHEN    "9102"
                   MOVE    "保険組合の変更で保険期間外の診療と病名が"
                                               TO  WRK-PIDMSG
                   MOVE    "存在します。診療と病名を保険変更。"
                                               TO  WRK-PIDMSG(41:)
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           EVALUATE    SPA-PIDCD
               WHEN    "0103"
                   MOVE    "B01"               TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *----(01.00.03) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       900-YOYAKU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-YYK
               MOVE    MCPDATA-REC         TO  YYK-REC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
      *
           .
       900-YOYAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費履歴読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       920-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付読み込み
      *****************************************************************
       930-UKETUKE-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
               MOVE    ZERO            TO  FLG-UKETUKE
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       930-UKETUKE-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       940-HKNCOMB-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       940-HKNCOMB-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       950-KYUSEI-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  KYUSEIRRK-REC
               MOVE    ZERO            TO  FLG-KYUSEIRRK
           ELSE
               INITIALIZE                  KYUSEIRRK-REC
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           .
       950-KYUSEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号変換（管理）マスタ読み込み
      *****************************************************************
       970-PTNUM-READ-SEC         SECTION.
      *
           MOVE    SPACE               TO  PTNUM-REC
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPID          TO  PTNUM-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTNUM-REC
                   MOVE    ZERO                TO  FLG-PTNUM
               ELSE
                   MOVE    1                   TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       970-PTNUM-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得マスタ読み込み
      *****************************************************************
       990-TSYRRK-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TSYRRK-REC
               MOVE    ZERO            TO  FLG-TSYRRK
           ELSE
               INITIALIZE                  TSYRRK-REC
               MOVE    1               TO  FLG-TSYRRK
           END-IF
      *
           .
       990-TSYRRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金マスタ読み込み
      *****************************************************************
       991-TNKRRK-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNKRRK-REC
               MOVE    ZERO            TO  FLG-TNKRRK
           ELSE
               INITIALIZE                  TNKRRK-REC
               MOVE    1               TO  FLG-TNKRRK
           END-IF
      *
           .
       991-TNKRRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       992-PARA-DEL-SEC          SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PARA-DEL2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       992-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCMCPSUB"   USING
                                MCPAREA
                                ORCMCPAREA
                                MCPDATA-REC.
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
