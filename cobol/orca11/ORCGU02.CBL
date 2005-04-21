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
       PROGRAM-ID.             ORCGU02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 来院受付
      *  コンポーネント名  : 来院受付（Ｕ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/04/10  項目入力チェック追加
      *  01.00.02    MCC-藤原     01/04/10  保険組合せ削除区分＝１読み飛ばし	 
      *  01.00.03    MCC-藤原     01/04/10  カルテ・処方箋の印刷追加
      *  01.00.04    MCC-藤原     01/04/20  患者登録との遷移の修正
      *  01.00.05    MCC-藤原     01/04/20  クリアボタン追加
      *  01.00.06    MCC-藤原     01/04/20  受付・予約済みのときは受診履歴を読まない
      *  01.02.01    MCC-藤原     01/04/20  受付・予約からシス管・患者マスタを読まない
      *  01.02.02    MCC-藤原     01/04/20  患者登録に遷移するとき選択番号をセット
      *  01.02.03    MCC-藤原     01/05/24  患者番号ゼロ詰め処理
      *  01.02.04    MCC-藤原     01/09/13  選択番号の編集
      *                                     患者氏名を漢字、かな同時に検索する    
      *  01.03.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.03.02    MCC-多々納   01/09/25  一覧表選択の追加
      *  01.03.03    MCC-多々納   01/11/15  患者番号等のプロテクト他
      *  01.03.03    MCC-多々納   01/11/28  画面遷移変更
      *  01.03.04    MCC-多々納   02/01/10  受診履歴キー変更
      *  01.03.05    MCC-多々納   02/01/31  選択番号未入力の時患者番号へ
      *  01.03.06    MCC-多々納   02/03/14  帳票カスタマイズ
      *  01.03.07    MCC-多々納   02/04/18  一覧表の表示修正
      *  02.00.00    NACL-多々納  02/08/12  保険確認の追加、
      *                                     インターフェースの変更
      *  02.00.01    NACL-多々納  02/10/09  未確認に記号番号追加
      *  02.00.02    NACL-多々納  02/11/20  ＣＬＡＩＭ処理追加
      *  02.00.03    NACL-多々納  02/11/26  診療科、ドクター件数変更
      *  02.00.04    NACL-多々納  02/12/16  新規患者取消修正
      *  02.00.05    NACL-多々納  03/02/26  診療科・ドクター件数修正
      *  02.00.06    NACL-多々納  03/03/05  排他制御処理追加
      *  02.00.07    NACL-多々納  03/03/26  カルテ続紙印刷追加
      *  02.00.08    NACL-多々納  03/06/25  受付一覧更新追加
      *  02.00.09    NACL-多々納  03/06/30  カスタマイズ帳票名取得サブ追加
      *  02.00.10    NACL-多々納  03/09/26  業務権限判定追加
      *                                     処方せんにドクターなし追加
      *  02.00.11    NACL-多々納  03/10/15  ２００明細へ
      *  02.00.12    NACL-多々納  03/11/07  受付更新修正
      *  02.00.13    NACL-多々納  05/02/04  受付マスタに保険組合せ追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "U02COMMON-SPA".
      *
      *---- (01.02.02) LINE ADD START---------------------------------- 
      *    患者登録スパ領域
      *****COPY    "P02COMMON-SPA".
      *---- (01.02.02) LINE ADD END  ----------------------------------
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA                                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-YYK                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-UKETUKE             PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
      *
           03  FLG-HENFLG              PIC 9(01).
           03  FLG-TOUROKU             PIC 9(01).
           03  FLG-IN-PTNUM            PIC 9(01).
           03  FLG-IN-NAME             PIC 9(01).
           03  FLG-CLAIM               PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-HKNOK               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDX3                    PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-C                   PIC 9(04).
      *
      *    ＣＬＡＩＭファイル名
       01  CLAIM-FILE.
           03  FILLER              PIC X(30)   VALUE
               "scripts/claim/HL01.sh".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-NUM               PIC ZZZ9.
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-WYMD              PIC X(07).
           03  WRK-UKEID             PIC 9(05). 
           03  WRK-SRYKA             PIC X(02). 
      *
           03  WRK-SELNUM            PIC 9(04). 
           03  WRK-GMN-SELNUM        PIC 9(02). 
      *
           03  WRK-MOTOPG            PIC X(08). 
      *    入力項目名
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-KNJ.
               07  WRK-PTNUM                           PIC X(20).
               07  WRK-PTID                            PIC X(10).
               07  WRK-NAME                            PIC X(50).
      *---- (01.00.03) LINE MOD START ---------------------------------
      **       07  WRK-SEX                             PIC X(02).
               07  WRK-SEX                             PIC X(01).
               07  WRK-BIRTHDAYS                       PIC X(08). 
      *---- (01.00.03) LINE MOD END -----------------------------------
               07  WRK-BIRTHDAY                        PIC X(09).
           03  WRK-MOJISU                              PIC 9(04).
      *  受診履歴表示用
           03  WRK-JYURRK-AREA.
               07  WRK-JRK-SRYKA                       PIC X(02).
               07  WRK-JRK-DRCD                        PIC X(05).
               07  WRK-JRK-SRYYMD                      PIC X(08).
               07  WRK-JRK-HKNCOMBI                    PIC 9(04).
      *
      *    カルテＰＧ名
           03  WRK-KARUTE-PG           PIC X(20).
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
      *
           03  WRK-HKNKBN-MEI      PIC X(02).
      *
      *    表示用保険
           03  WRK-IDXH                PIC 9(04).
           03  WRK-CNT-HKN             PIC 9(04).
           03  WRK-CNT-HKN2            PIC 9(04).
      *
           03  WRK-HKNCOMBI            PIC X(100).
      *
      *D01  WRK-COMBO-SCODE-MAX                       PIC 9(03).
026700*D01  WRK-COMBO-TBL.
026800*D    03  WRK-COMBO-TBLO                        OCCURS   3.
      *D        05  WRK-COMBO-SCODEL                  PIC X(01).  
      *D        05  WRK-COMBO-FIL                     PIC X(01).  
      *D        05  WRK-COMBO-SCODEMEIL               PIC X(18).  
      *
        01  WRK-SORT-TBL.
            03  WRK-SORT-TBLO                         OCCURS   200.
                05  WRK-SORT-TUKEID                   PIC 9(05).  
                05  WRK-SORT-TYYKTIME                 PIC X(04).
                05  WRK-SORT-TUKETIME                 PIC X(06).
                05  WRK-SORT-TPTID                    PIC 9(10).
                05  WRK-SORT-TNAME                    PIC X(50).
                05  WRK-SORT-TSRYKA                   PIC X(02).
                05  WRK-SORT-TDRCD                    PIC X(04).
                05  WRK-SORT-TSRYNAIYOCD              PIC X(02).
                05  WRK-SORT-NARABE                   PIC X(04).
                05  WRK-SORT-TKYYKTIME                PIC X(04).
                05  WRK-SORT-TYYKID                   PIC X(02).
                05  WRK-SORT-THKNCOMBI                PIC 9(04).
      *
           03  WORK-SORT                               PIC X(100).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG                  PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO                  PIC X(01).
               05  WRK-HYY                  PIC Z9.
               05  FILLER                   PIC X(01)   VALUE  ".".
               05  WRK-HMM                  PIC Z9.
               05  FILLER                   PIC X(01)   VALUE  ".".
               05  WRK-HDD                  PIC Z9.
      *
           03  WRK-HENYMDN.
               05  WRK-HENNYMD              PIC X(22).
               05  FILLER                   PIC X(02)   VALUE  "（".
               05  WRK-HENNYOBI             PIC X(02).
               05  FILLER                   PIC X(02)   VALUE  "）".
      *
           03  WRK-HENTIME.
               05  WRK-THH                  PIC 99.
               05  FILLER                   PIC X(01)   VALUE  ":".
               05  WRK-TMM                  PIC 99.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *    受付
       01  WRK-UKETUKE-REC.
           COPY    "CPUKETUKE.INC"   REPLACING
                                     //UKE-// BY //WRK-UKE-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *　　（職員情報）
           COPY    "CPSK1010.INC".
      *　　（診療内容）
           COPY    "CPSK1012.INC".
      *　　（帳票プログラム）
           COPY    "CPSK1032.INC".
      *　　（ＣＬＡＩＭ接続）
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    カルテパラメタ
           COPY    "CPORCHC01.INC".
      *
      *    処方箋印刷パラメタ
           COPY    "CPORCHC02.INC".
      *
      *    処方箋印刷パラメタ（Ａ５）
           COPY    "CPORCHC19.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *===> For Claim Start
           COPY    "CPSHELLTBL.INC".
           COPY    "ORCA-DBPATH".
      *===> For Claim End
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "U02.INC".
           COPY    "U02X.INC".
           COPY    "U2ERR.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-U02
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
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
           IF      FLG-END                       =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-U02                       TO  SPA-FREE
           .
       000-PROC-EXIT.
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                            IDX-AREA
           INITIALIZE                            FLG-AREA
           INITIALIZE                            WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "U2ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "U02"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC                  SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-GMN-PAGE
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *        元の画面へ戻す
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"  OR
                                               "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-U02
               END-IF
      *        予約とエラー表示の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01"
      *            予約，受付編集
                   PERFORM 3101-UKETU-HEN-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-U01SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               ELSE
                   MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
               END-IF
           END-IF
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "U02X"
      *        ＣＬＡＩＭ画面より
                   IF      SPA-U02X-FLG        =   SPACE
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    62                  TO  SPA-GMN-CUR
                   ELSE
                       PERFORM 3009-CLAIM-SYORI-SEC
                   END-IF
                   GO      TO                  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "P02"
                   IF     (SPA-ERRMSG(1:2)    =   "NO"  )  AND
                          (SPA-PTNUM(1:1)     =   "*"   )
                       MOVE    SPACE              TO  SPA-PTNUM
                       MOVE    ZERO               TO  SPA-PTID
                   END-IF
                   IF      SPA-PTNUM       NOT =   SPACE
                       MOVE    SPA-PTNUM           TO  U02-PTNUM
                       MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
                       MOVE    1                   TO  FLG-IN-PTNUM
                       PERFORM 4101-PTNUM-CHK-SEC
                       MOVE    4                   TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
           IF      SPA-MOTOPG(1:3)     =   "K02"
               MOVE    SPA-MOTOPG          TO  SPA-HZN-MOTOPG
           END-IF
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-U01SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM           =   SPACE
               MOVE    4               TO  SPA-GMN-CUR
               IF      WRK-MOTOPG(1:3)     =   "P97"
                   MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
               END-IF
           ELSE
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               MOVE    ZERO                TO  SPA-NAI-SELNUM
               MOVE    WRK-SPA-PTNUM       TO  U02-PTNUM
               MOVE    U02-PTNUM           TO  SPA-GMN-PTNUM
               MOVE    1                   TO  FLG-IN-PTNUM
               PERFORM 4101-PTNUM-CHK-SEC
               MOVE    4               TO  SPA-GMN-CUR
           END-IF
           .
       3101-U01SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-U02
      *
           MOVE    2                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-GMN-PAGE
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-SYSYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENNYMD
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE  "31"                  TO  LNK-DAY3-IRAI
           MOVE  SPA-SYSYMD            TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY3-AREA
           MOVE  LNK-DAY3-YOBIMEI      TO  WRK-HENNYOBI
      *
           MOVE    WRK-HENYMDN         TO  SPA-GMN-SRYYMDH
      *
      *    診療科
           MOVE    SPACE               TO  SYS-KEY
           MOVE    "1005"              TO  SYS-1005-KANRICD
      *    MOVE    ZERO                TO  SYS-1005-STYUKYMD
      *    MOVE    ALL "9"             TO  SYS-1005-EDYUKYMD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
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
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   40   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-GMN-SRYKA
               MOVE    SPACE                 TO  SPA-GMN-SRYKA1 
               MOVE    SYS-1005-SRYKANAME1   TO  SPA-GMN-SRYKANAME
               MOVE    SPA-GMN-SRYKA-G       TO  SPA-GMN-SRYKALST (IDX)
               MOVE    IDX                   TO  SPA-SRYKA-MAX
      *
               MOVE    "SYSKANRI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-SRYKALST(01)
                                       TO  SPA-GMN-SRYKA-G
      *
      *    職員情報をＳＰＡにセット
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
           MOVE    SPA-GMN-DRCDLST(01) TO  SPA-GMN-DRCD-G
      *
      *    診療内容をＳＰＡにセット
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
      *    MOVE    ZERO                TO  SYS-1012-STYUKYMD
      *    MOVE    ALL "9"             TO  SYS-1012-EDYUKYMD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
      *    システム管理検索
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
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
               INITIALIZE                  SYS-1012-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL      (IDX             >   30    )   OR
                              (FLG-SYSKANRI    =   1   )
      *
               MOVE    MCPDATA-REC       TO  SYS-1012-REC
               MOVE    SYS-1012-KBNCD    TO  SPA-GMN-SRYNAIYOCD
               MOVE    SPACE             TO  SPA-GMN-SRYNAIYO1
               MOVE    SYS-1012-SRYNAIYODSP
                                         TO  SPA-GMN-SRYNAIYODSP
               MOVE    SPA-GMN-SRYNAIYOCD-G
                                         TO  SPA-GMN-SRYNAILST (IDX)
               MOVE    IDX               TO  SPA-SRYNAIYO-MAX
      *
               MOVE    "SYSKANRI-KEY2"         TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-SRYNAILST(01) TO  SPA-GMN-SRYNAIYOCD-G
      *
      *    予約，受付編集
           PERFORM 3101-UKETU-HEN-SEC
      *
      *    カルテ
           MOVE    "0:発行なし"        TO  SPA-GMN-KARUTELST (1)
           MOVE    "1:発行あり"        TO  SPA-GMN-KARUTELST (2)
           MOVE    "2:続紙頭書"        TO  SPA-GMN-KARUTELST (3)
           MOVE    SPA-GMN-KARUTELST (1)   TO  SPA-GMN-KARUTE-G
      *
      *    処方
           MOVE    "0:発行なし"        TO  SPA-GMN-SYOHOLST (1)
           MOVE    "1:発行あり"        TO  SPA-GMN-SYOHOLST (2)
           MOVE    SPA-GMN-SYOHOLST (1)    TO  SPA-GMN-SYOHO-G
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    予約，受付編集処理
      *****************************************************************
       3101-UKETU-HEN-SEC                        SECTION.
      *
           MOVE    ZERO                          TO  IDX
           MOVE    ZERO                          TO  SPA-UKE-MAX
      *
           INITIALIZE                            WRK-SORT-TBL
                                                 SPA-GMN-TBL
      *
      *    予約マスター読み込み
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-SYSYMD          TO  YYK-YYKYMD
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "YYK-KEY5"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "YYK-KEY5"      TO  ORC-DBPATH
               PERFORM 910-YOYAKU-READ-SEC
           ELSE
               INITIALIZE                  YYK-REC
               MOVE    1               TO  FLG-YYK
           END-IF
      *
           PERFORM UNTIL     (FLG-YYK            =  1     )  OR
                             (IDX               >=  200   )
               IF     (YYK-YYKID       NOT       =   ZERO       ) AND
                      (YYK-RAIINZUMI             =   SPACE      )
                   ADD     1             TO  IDX
                   MOVE    YYK-YYKTIME   TO  WRK-SORT-NARABE   (IDX)
                   MOVE    YYK-YYKTIME   TO  WRK-SORT-TYYKTIME (IDX)
                   MOVE    YYK-PTID      TO  WRK-SORT-TPTID    (IDX)
                   MOVE    YYK-NAME      TO  WRK-SORT-TNAME    (IDX)
                   MOVE    YYK-SRYKA     TO  WRK-SORT-TSRYKA   (IDX)
                   MOVE    YYK-DRCD(2:4) TO  WRK-SORT-TDRCD    (IDX)
                   MOVE    YYK-SRYNAIYO  TO  WRK-SORT-TSRYNAIYOCD(IDX)
                   MOVE    YYK-KEYYYKTIME
                                         TO  WRK-SORT-TKYYKTIME (IDX)
                   MOVE    YYK-YYKID     TO  WRK-SORT-TYYKID   (IDX)
               END-IF
      *
               MOVE    "YYK-KEY5"           TO  ORC-DBPATH
               PERFORM 910-YOYAKU-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "YYK-KEY5"          TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
      *    受付テーブル読み込み
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPID          TO  UKE-HOSPID
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "UKETUKE-KEY3"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "UKETUKE-KEY3"      TO  ORC-DBPATH
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           PERFORM     UNTIL  (FLG-UKETUKE        =  1     )  OR
                              (IDX               >=  200   )
               IF     (UKE-UKEID         >   ZERO    )   AND
                      (UKE-KAIKEITIME    =   ZERO    ) 
                       ADD     1             TO  IDX
                       IF  UKE-YYKTIME       >   ZERO
                           MOVE    UKE-YYKTIME
                                             TO  WRK-SORT-TYYKTIME(IDX)
                           MOVE    UKE-KEYYYKTIME
                                             TO
                                             WRK-SORT-TKYYKTIME(IDX)
                           MOVE    UKE-YYKID TO  WRK-SORT-TYYKID  (IDX)
                       END-IF
                       MOVE    UKE-UKEID     TO  WRK-SORT-TUKEID  (IDX)
                       MOVE    UKE-UKETIME   TO  WRK-SORT-TUKETIME(IDX)
                       MOVE    UKE-UKETIME   TO  WRK-SORT-NARABE  (IDX)
                       MOVE    UKE-PTID      TO  WRK-SORT-TPTID   (IDX)
                       MOVE    UKE-NAME      TO  WRK-SORT-TNAME   (IDX)
                       MOVE    UKE-SRYKA     TO  WRK-SORT-TSRYKA  (IDX)
                       MOVE    UKE-DRCD(2:4) TO  WRK-SORT-TDRCD   (IDX)
                       MOVE    UKE-SRYNAIYO  TO 
                                             WRK-SORT-TSRYNAIYOCD(IDX)
                       MOVE    UKE-HKNCOMBI  TO 
                                             WRK-SORT-THKNCOMBI   (IDX)
      *                患者氏名を患者マスタより
                       IF     (UKE-PTID      NOT =   ZERO  )  AND
                              (UKE-NAME          =   SPACE )
      *
      *    患者マスター存在チェック
                           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
                           MOVE    UKE-PTID            TO  PTINF-PTID
                           PERFORM 900-PTINF-READ-SEC
                           IF      FLG-PTINF       =   ZERO
                               MOVE    PTINF-NAME
                                               TO  WRK-SORT-TNAME (IDX)
                               MOVE    PTINF-NAME
                                               TO  UKE-NAME
      *
                               MOVE    UKETUKE-REC     TO  MCPDATA-REC
                               MOVE    "DBUPDATE"      TO  MCP-FUNC
                               MOVE    "UKETUKE-KEY"   TO  ORC-DBPATH
                               CALL    "ORCMCPSUB"          USING
                                                            MCPAREA
                                                            ORCMCPAREA
                                                            MCPDATA-REC
                           END-IF
                       END-IF
               END-IF
      *
               MOVE    "UKETUKE-KEY3"      TO  ORC-DBPATH
               PERFORM 910-UKETUKE-READ-SEC
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "UKETUKE-KEY3"      TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
      *
           PERFORM  31012-SORT-SEC
      *
           MOVE    ZERO            TO  IDX  SPA-UKE-MAX
           INITIALIZE              SPA-GMN-TBL
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       200 
                   OR      WRK-SORT-NARABE(IDX)   =   SPACE
               MOVE    IDX                 TO  SPA-UKE-MAX
      *
               MOVE    IDX            TO  SPA-GMN-TNUM       (IDX)
               MOVE    WRK-SORT-TUKEID  (IDX)
                                      TO  SPA-GMN-TUKEID     (IDX)
               MOVE    WRK-SORT-TYYKTIME(IDX)
                                      TO  SPA-GMN-TYYKTIME   (IDX)
               MOVE    WRK-SORT-TUKETIME(IDX)
                                      TO  SPA-GMN-TUKETIME   (IDX)
               MOVE    WRK-SORT-TPTID(IDX)
                                      TO  SPA-GMN-TPTID      (IDX)
               MOVE    WRK-SORT-TNAME(IDX)
                                      TO  SPA-GMN-TNAME      (IDX)
               MOVE    WRK-SORT-TSRYKA(IDX)
                                      TO  SPA-GMN-TSRYKA     (IDX)
               MOVE    WRK-SORT-TDRCD(IDX)
                                      TO  SPA-GMN-TDRCD      (IDX)
               MOVE    WRK-SORT-TSRYNAIYOCD(IDX)
                                      TO  SPA-GMN-TSRYNAIYOCD(IDX)
               MOVE    WRK-SORT-THKNCOMBI   (IDX)
                                      TO  SPA-GMN-THKNCOMBI  (IDX)
               MOVE    WRK-SORT-TKYYKTIME(IDX)
                                      TO  SPA-GMN-TKYYKTIME  (IDX)
               MOVE    WRK-SORT-TYYKID(IDX)
                                      TO  SPA-GMN-TYYKID     (IDX)
      *        患者番号セット
               IF      SPA-GMN-TPTID(IDX)      =   ZERO   OR  SPACE
                   MOVE    SPACE               TO  SPA-GMN-TPTNUM(IDX)
               ELSE
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPID          TO  SPTID-HOSPID
                   MOVE    SPA-GMN-TPTID(IDX)  TO  SPTID-PTID
                   CALL    "ORCSPTID"          USING   ORCSPTIDAREA
      *----(01.02.02) LINE ADD START ----------------------------------
                                                       SPA-1009-TBL
      *----(01.02.02) LINE ADD END   ----------------------------------
                   IF      SPTID-RC            =   ZERO
                       MOVE    SPTID-PTNUM     TO  SPA-GMN-TPTNUM(IDX)
      *                最終受診日セット
                       PERFORM 31019-JYURRK-HEN-SEC
                   END-IF
               END-IF 
      *
      *        診療科情報をＳＰＡにセット
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >       SPA-SRYKA-MAX
                   IF      SPA-GMN-TSRYKA (IDX)
                                       =  SPA-GMN-SRYKALST(IDY)(1:2)
                       MOVE    SPA-GMN-SRYKALST (IDY) (4:)
                                           TO  SPA-GMN-TSRYKANAME(IDX)
                       MOVE    SPA-SRYKA-MAX       TO  IDY
                   END-IF
               END-PERFORM
      *        職員情報をＳＰＡにセット
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
               MOVE    "2"                 TO  ORCSDR-KBN
               MOVE    SPA-GMN-TSRYKA (IDX)
                                           TO  ORCSDR-SRYKA
               MOVE    SPA-GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
               MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
               MOVE    SPA-GMN-TDRCD (IDX)
                                           TO  ORCSDR-IN-DRCD
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-TDRCD-G(IDX)
      *        診療内容をＳＰＡにセット
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-SRYNAIYO-MAX
                       OR          SPA-GMN-SRYNAILST(IDY)  =   SPACE
                   IF      SPA-GMN-TSRYNAIYOCD(IDX)
                                           =   SPA-GMN-SRYNAILST(IDY)
                                                                (1:2)
                       MOVE    SPA-GMN-SRYNAILST(IDY) (4:)
                                       TO  SPA-GMN-TSRYNAIYODSP(IDX)
                       MOVE    SPA-SRYNAIYO-MAX    TO  IDY
                   END-IF
               END-PERFORM
      *        保険組合せ設定
               IF      SPA-GMN-THKNCOMBI  (IDX)    =   SPACE  OR  ZERO
                   MOVE    SPACE           TO  SPA-GMN-THKNCOMBI-G (IDX)
               ELSE
                   PERFORM 31013-HKNCOMBI-HEN-SEC
               END-IF
           END-PERFORM
      *
           .
      *
       3101-UKETU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    予約受付状況並べ替え
      *****************************************************************
       31012-SORT-SEC              SECTION.
      *  
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >=      200
                   OR      WRK-SORT-NARABE(IDX)   =   SPACE
               COMPUTE IDY     =   IDX     +   1
               PERFORM VARYING IDZ     FROM    IDY     BY  1
                       UNTIL   IDZ     >       200
                       OR      WRK-SORT-NARABE(IDZ)   =   SPACE
                   IF      WRK-SORT-NARABE (IDX)   >   
                                           WRK-SORT-NARABE (IDZ)
                       MOVE    WRK-SORT-TBLO  (IDX)   TO  WORK-SORT
                       MOVE    WRK-SORT-TBLO  (IDZ)   TO  
                                               WRK-SORT-TBLO (IDX)
                       MOVE    WORK-SORT               TO  
                                               WRK-SORT-TBLO (IDZ)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       31012-SORT-EXT.
           EXIT.
      *****************************************************************
      *    最終受診日セット処理
      *****************************************************************
       31019-JYURRK-HEN-SEC            SECTION.
      *    最終受診日セット
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-TPTID (IDX) TO  JYURRK-PTID
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY8"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY8"      TO  ORC-DBPATH
               PERFORM 900-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           IF      FLG-JYURRK          =   ZERO
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    JYURRK-SRYYMD       TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  SPA-GMN-TRRKYMD (IDX)
           END-IF
      *
      *    カーソルクロース
           MOVE    "JYURRK-KEY8"       TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           .
       31019-JYURRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ設定処理
      *****************************************************************
       31013-HKNCOMBI-HEN-SEC              SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-GMN-TPTID (IDX) TO  COMB-PTID
           MOVE    SPA-GMN-THKNCOMBI(IDX)
                                       TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               PERFORM 900-COMB-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           IF      COMB-DELKBN     NOT =   SPACE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           IF      FLG-HKNCOMBI        =   ZERO
               PERFORM 310131-HKNCOMBIMEI-HEN-SEC
           END-IF
      *
           .
       31013-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       310131-HKNCOMBIMEI-HEN-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
               END-STRING
           END-IF
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "PTRSIINF-KEY"     TO  ORC-DBPATH
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
               PERFORM 990-DBCLOSE-SEC
      *
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBI
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    WRK-HKNCOMBI        TO  SPA-GMN-THKNCOMBIMEI (IDX)
      *
           .
       310131-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＭＥ画面よりの処理
      *****************************************************************
       3009-CLAIM-SYORI-SEC              SECTION.
      *
           IF      SPA-U02X-FLG        =  "OK"
               MOVE    1               TO  FLG-CLAIM
           ELSE
               MOVE    ZERO            TO  FLG-CLAIM
           END-IF
      *
      *    更新処理
           PERFORM 49001-KOUSIN-SEC
      *
          .
       3009-CLAIM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *---- (01.00.05) LINE ADD START --------------------------------
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 493-CLEAR-SEC
      *---- (01.00.05) LINE ADD END ----------------------------------
      *    受付一覧更新
               WHEN    "CLICKED"       ALSO    "pandatimer1"
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 4931-UKELIST-SEC
      *
      *    受付取消へ
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 491-SAKUJO-SEC
      *    前回患者
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-MAEKPTNUM-SEC
      *    患者登録
               WHEN    "CLICKED"       ALSO    "B05"
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 492-ORCGP01-SEC
      *    診療行為
               WHEN    "CLICKED"       ALSO    "B06"
      *********WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 493-ORCGK02-SEC
      *    病名登録
               WHEN    "CLICKED"       ALSO    "B07"
      *********WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 494-ORCGC02-SEC
      *    収納登録
               WHEN    "CLICKED"       ALSO    "B08"
               WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 495-ORCGS02-SEC
      *    会計照会
               WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 496-ORCGJ02-SEC
      *    氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 480-KENSAKU-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 460-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    受付完了
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    印刷のみ
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 495-PRINT-SEC
      *    前（↑）
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 465-UP-SYORI-SEC
      *    次（↓）
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 475-DOWN-SYORI-SEC
      *    保険チェック
               WHEN    "CLICKED"       ALSO    "BHK1"
               WHEN    "CLICKED"       ALSO    "BHK2"
               WHEN    "CLICKED"       ALSO    "BHK3"
               WHEN    "CLICKED"       ALSO    "BHK4"
               WHEN    "CLICKED"       ALSO    "BHK5"
                   PERFORM 400-HKNCHK-SYORI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *            保険組合せ選択
               WHEN    ANY             ALSO    "HKNCOMBI_LIST"
                   PERFORM 4109-HKNCOMBI-LIST-SEC
      *            受付一覧選択
               WHEN    ANY             ALSO    "UKETUKE_LIST"
                   PERFORM 4110-UKETUKE-LIST-SEC
      *           入力チェック処理
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-TOUROKU
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
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
      *
      *    登録時、必須入力チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               IF      FLG-TOUROKU         =   1
                   PERFORM 4109-TOUROKU-CHK-SEC
               END-IF
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
      *    患者番号
           IF      U02-PTNUM           =   SPA-MAE-PTNUM
               MOVE    ZERO                TO  FLG-IN-PTNUM
           ELSE
               MOVE    1                   TO  FLG-IN-PTNUM
           END-IF
           MOVE    U02-PTNUM           TO  SPA-GMN-PTNUM
      *    新患予約者氏名
           IF      U02-NAME            =   SPA-MAE-NAME
               MOVE    ZERO                TO  FLG-IN-NAME
           ELSE
               MOVE    1                   TO  FLG-IN-NAME
           END-IF
           MOVE    U02-NAME            TO  SPA-GMN-NAME
      *
           MOVE    U02-SRYKA           TO  SPA-GMN-SRYKA-G
           MOVE    U02-DRNAME          TO  SPA-GMN-DRCD-G
           MOVE    U02-SRYNAIYO        TO  SPA-GMN-SRYNAIYOCD-G
           MOVE    U02-KARUTE          TO  SPA-GMN-KARUTE-G
           MOVE    U02-SYOHO           TO  SPA-GMN-SYOHO-G
           MOVE    U02-HKNCOMBI        TO  SPA-GMN-COMBINUM
      *
           MOVE    U02-SELNUM          TO  SPA-GMN-SELNUM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    選択番号入力時、選択処理
           IF      WRK-MCP-WIDGET      =   "SELNUM"
               PERFORM 4104-SELNUM-CHK-SEC
               IF      SPA-GMN-SELNUM      NOT =   ZERO
                   GO      TO      4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    患者番号
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4101-PTNUM-CHK-SEC
           END-IF
      *
      *    氏名
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4102-NAME-CHK-SEC
           END-IF
      *    診療科
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4103-SRYKA-CHK-SEC
           END-IF
      *    ドクター
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4105-DRCD-CHK-SEC
           END-IF
      *    内容
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4106-SRYNAIYO-CHK-SEC
           END-IF
      *            カルテ
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4107-KARUTE-CHK-SEC
           END-IF
      *            処方箋
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4108-SYOHO-CHK-SEC
           END-IF
      *     組合せ
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4109-HKNCOMBI-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号 チェック処理
      *****************************************************************
       4101-PTNUM-CHK-SEC          SECTION.
      *
      *    ”＊”で、新患者登録へ
           IF      SPA-GMN-PTNUM(1:1)  =   "*"
               MOVE    ZERO                TO  SPA-GMN-PTID
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               PERFORM 492-ORCGP01-SEC
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *    入力なしの時、以下処理なし
           IF      FLG-IN-PTNUM        =   ZERO
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *
      *    クリア
           PERFORM 1009-HEAD-CLEAR-SEC
      *
           MOVE    U02-PTNUM           TO  SPA-GMN-PTNUM
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    SPA-GMN-PTNUM       TO  SPA-MAE-PTNUM
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *
      *    患者ＩＤ読み出し
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPID          TO  SPTID-HOSPID
           MOVE    SPA-GMN-PTNUM       TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-1009-TBL
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID
      *
      *    患者マスター存在チェック
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4101-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 500-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY
      *---- (01.00.03) LINE ADD START ---------------------------------
           MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-BIRTHDAY 
           MOVE    PTINF-SEX           TO  SPA-NAI-SEX
      *---- (01.00.03) LINE ADD END ---------------------------------- 
      *    保険組合せ表示処理
           PERFORM 410114-KUMIHYOJI-SEC
      *    保険確認表示処理
           PERFORM 410115-KAKUHYOJI-SEC
      *    前回受診履歴処理
           IF      SPA-GMN-SELNUM           =   ZERO
               PERFORM 410116-JYURRKSET-SEC
           END-IF
      *
      *    前回編集
           MOVE    SPA-GMN-PTNUM       TO  SPA-MAE-PTNUM
           MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
      *
      *    前回患者ヘ編集
           MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
           .
       4101-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    新患予約者氏名
      *****************************************************************
       4102-NAME-CHK-SEC            SECTION.
      *
           IF      SPA-GMN-NAME        =   SPACE
               MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
               GO      TO      4102-NAME-CHK-EXT
           END-IF
      *
           IF      FLG-IN-NAME         =   ZERO
               GO      TO      4102-NAME-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-KNJ
           MOVE    SPACE               TO  WRK-PTID
           PERFORM 41021-KNJKEN-HEN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      4102-NAME-CHK-EXT
           END-IF
      *
           IF      IDZ                  >   1
      *        氏名検索へ
               PERFORM 480-KENSAKU-SEC
           ELSE
               IF      WRK-PTID           =   SPACE
                   CONTINUE
               ELSE
      *            患者マスタに予約者あり
                   MOVE    WRK-PTID            TO  SPA-GMN-PTID
                   MOVE    WRK-PTNUM           TO  SPA-GMN-PTNUM
                   MOVE    WRK-NAME            TO  SPA-GMN-NAME
                   MOVE    WRK-BIRTHDAY        TO  SPA-GMN-BIRTHDAY
      *---- (01.00.03) LINE ADD START ---------------------------------
                   MOVE    WRK-BIRTHDAYS       TO  SPA-NAI-BIRTHDAY 
                   MOVE    WRK-SEX             TO  SPA-NAI-SEX
      *---- (01.00.03) LINE ADD END ---------------------------------- 
                   PERFORM   410114-KUMIHYOJI-SEC
                   PERFORM   410115-KAKUHYOJI-SEC
      *---- (01.00.06) LINE MOD START ---------------------------------
                   IF      SPA-GMN-SELNUM           =   ZERO
                       PERFORM 410116-JYURRKSET-SEC
                   END-IF
      *---- (01.00.06) LINE MOD END ---------------------------------- 
      *            前回患者ヘ編集
                   MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
                   MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
               END-IF
           END-IF
      *
      *    前回編集
           MOVE    SPA-GMN-PTNUM       TO  SPA-MAE-PTNUM
           MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
      *
           .
       4102-NAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者検索処理
      *****************************************************************
       41021-KNJKEN-HEN-SEC        SECTION.
      *
      *    全角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-NAME        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO      41021-KNJKEN-HEN-EXT
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-NAME
           END-IF
      *
      *    漢字検索
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPACE               TO  PTINF-NAME
           MOVE    SPA-GMN-NAME        TO  PTINF-NAME
           MOVE    SPA-GMN-NAME        TO  PTINF-KANANAME
      *
      *    患者マスターを漢字氏名で検索
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY5"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTINF-KEY5"        TO  ORC-DBPATH
               PERFORM 910-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           MOVE    ZERO                TO  IDZ
           PERFORM UNTIL      (IDZ             >   1         ) OR
                              (FLG-PTINF       =   1         )
               PERFORM 41021-PTINF-HENSYU-SEC
      *
               MOVE    "PTINF-KEY5"        TO  ORC-DBPATH
               PERFORM 910-PTINF-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "PTINF-KEY5"        TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41021-KNJKEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ編集処理
      *****************************************************************
       41021-PTINF-HENSYU-SEC          SECTION.
      *
      *    患者番号変換より、患者NOより患者IDを取得する
      *
           ADD     1                   TO  IDZ
           MOVE    PTINF-PTID          TO  WRK-PTID
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPID          TO  SPTID-HOSPID
           MOVE    PTINF-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING  ORCSPTIDAREA
      *----(01.02.02) LINE ADD START ----------------------------------
                                              SPA-1009-TBL
      *----(01.02.02) LINE ADD END   ----------------------------------
           IF      SPTID-RC        NOT =   ZERO
               MOVE    SPACE           TO  WRK-PTNUM
           ELSE
               MOVE    SPTID-PTNUM     TO  WRK-PTNUM
           END-IF
      *
           MOVE    PTINF-NAME          TO  WRK-NAME
           MOVE    PTINF-SEX           TO  WRK-SEX 
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-BIRTHDAYS 
      *---- (01.00.03) LINE MOD END --------------------------------- 
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 500-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-BIRTHDAY
           .
       41021-PTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力
      *****************************************************************
       4103-SRYKA-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-SRYKA       =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-HENFLG
               PERFORM VARYING IDY         FROM   1   BY  1
                       UNTIL  (IDY         >   SPA-SRYKA-MAX )
                       OR     (FLG-HENFLG  =   1  )
                   IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKALST
                                                       (IDY)(1:2) 
                       MOVE    SPA-GMN-SRYKALST (IDY)
                                               TO  SPA-GMN-SRYKA-G
                       MOVE    1               TO  FLG-HENFLG
                   END-IF
               END-PERFORM
               IF      FLG-HENFLG          =   ZERO
                   MOVE    "0019"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-SRYKA   NOT =   SPACE  )  AND
                  (SPA-GMN-SRYKA   NOT =   SPA-MAE-SRYKA)
      *        診療科が変わった時、ドクターを再編集する
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
               MOVE    "1"                 TO  ORCSDR-KBN
               MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *        受診履歴よりドクター、保険組合せを決定する
               PERFORM 41031-JYURRK-SRYKA-SEC
           END-IF
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
           .
       4103-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科より保険組合せ・ドクター決定処理
      *****************************************************************
       41031-JYURRK-SRYKA-SEC               SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-SYSYMD          TO  JYURRK-SRYYMD
           MOVE    SPA-GMN-SRYKA       TO  JYURRK-SRYKA
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY27"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY27"     TO  ORC-DBPATH
               PERFORM 900-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *    カーソルクロース
           MOVE    "JYURRK-KEY27"      TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-DRCD         TO  WRK-JRK-DRCD
      *        保険組合番号
               MOVE    JYURRK-HKNCOMBINUM  TO  WRK-JRK-HKNCOMBI
           ELSE
               MOVE    SPACE               TO  WRK-JRK-DRCD
               MOVE    ZERO                TO  WRK-JRK-HKNCOMBI
           END-IF
      *    ドクター、保険組合せ編集
           PERFORM 4101161-DRCD-HKNCOMBI-SEC
      *
           .
       41031-JYURRK-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力
      *****************************************************************
       4105-DRCD-CHK-SEC               SECTION.
      *
           IF      SPA-GMN-DRCD        =   SPACE
               GO      TO      4105-DRCD-EXT
           END-IF
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "2"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
           MOVE    SPA-GMN-DRCD-G      TO  ORCSDR-IN-DRCD-G
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
           IF      ORCSDR-ERRCD    NOT =   ZERO
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4105-DRCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容入力
      *****************************************************************
       4106-SRYNAIYO-CHK-SEC           SECTION.
      *
      *
           IF      SPA-GMN-SRYNAIYOCD        =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO               TO  FLG-HENFLG
               PERFORM VARYING IDY         FROM   1   BY  1
                       UNTIL  (IDY         >   SPA-SRYNAIYO-MAX )
                       OR     (FLG-HENFLG  =   1  )
                   IF      SPA-GMN-SRYNAIYOCD
                                      =   SPA-GMN-SRYNAILST(IDY)(1:2)
                       MOVE    SPA-GMN-SRYNAILST (IDY)
                                           TO  SPA-GMN-SRYNAIYOCD-G
                       MOVE    1           TO  FLG-HENFLG
                   END-IF
               END-PERFORM
               IF      FLG-HENFLG          =   ZERO
                   MOVE    "0021"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-CUR
                   GO  TO  4106-SRYNAIYO-EXT
               END-IF
           END-IF
      *            
           .
       4106-SRYNAIYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ入力
      *****************************************************************
       4107-KARUTE-CHK-SEC               SECTION.
      *
           EVALUATE    SPA-GMN-KARUTE
               WHEN    "0"
               WHEN    "2"
                   CONTINUE
               WHEN    "1"
                   IF      SPA-COMBI-MAX       =   ZERO
                       CONTINUE
                   ELSE
      *                保険組合せが１件の時、１とする
                       IF      SPA-COMBI-MAX       =   1
                           MOVE    SPA-GMN-HKNCOMBINUM (01)
                                               TO  SPA-GMN-COMBINUM
                       END-IF
                   END-IF
               WHEN    OTHER
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-GMN-CUR
           END-EVALUATE
           .
       4107-KARUTE-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方箋入力
      *****************************************************************
       4108-SYOHO-CHK-SEC               SECTION.
      *
           EVALUATE    SPA-GMN-SYOHO
               WHEN    "0"
                   CONTINUE
               WHEN    "1"
                   IF      SPA-COMBI-MAX       =   ZERO
                       CONTINUE
                   ELSE
      *                保険組合せが１件の時、１とする
                       IF      SPA-COMBI-MAX       =   1
                           MOVE    SPA-GMN-HKNCOMBINUM (01)
                                               TO  SPA-GMN-COMBINUM
                           MOVE    62                  TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               WHEN    OTHER
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-GMN-CUR
           END-EVALUATE
      *---- (01.00.01) LINE MOD END -----------------------------------
           .
       4108-SYOHO-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ選択 チェック処理
      *****************************************************************
       4109-HKNCOMBI-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-COMBINUM    =   ZERO
               GO      TO      4109-HKNCOMBI-CHK-EXT
           END-IF
      *
      *    選択番号チェック
           MOVE    ZERO                TO  FLG-HENFLG
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-COMBI-MAX 
               IF      SPA-GMN-COMBINUM    =   SPA-GMN-HKNCOMBINUM
                                                                (IDY)
                   MOVE    1                   TO  FLG-HENFLG
                   MOVE    SPA-COMBI-MAX       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-HENFLG          =   ZERO
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    9                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4109-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ選択処理
      *****************************************************************
       4109-HKNCOMBI-LIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-COMBI-MAX
              IF      U02-HKNCOMBI-SELECT (IDX)   =  "T"
                  IF      SPA-GMN-HKNCOMBINUM (IDX)
                                               =   SPA-GMN-COMBINUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-GMN-HKNCOMBINUM (IDX)
                                              TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM      TO  SPA-GMN-COMBINUM
           END-IF
      *
           IF      SPA-GMN-COMBINUM    NOT =   ZERO
      *        組合せ
               PERFORM 4109-HKNCOMBI-CHK-SEC
           END-IF
      *
           .
       4109-HKNCOMBI-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約選択 チェック処理
      *****************************************************************
       4104-SELNUM-CHK-SEC        SECTION.
      *
      *    未入力の時、患者番号へ
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4104-SELNUM-EXT
           END-IF
      *
      *    選択番号チェック
           IF      SPA-GMN-SELNUM      >=  1   AND  <=  SPA-UKE-MAX
               CONTINUE
           ELSE
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4104-SELNUM-EXT
           END-IF
      *
           MOVE    SPA-GMN-SELNUM          TO  SPA-NAI-SELNUM
           INITIALIZE                      SPA-GMN-AREA
           INITIALIZE                      SPA-NAIB-AREA
      *    受付一覧より編集
           MOVE    SPA-GMN-SELNUM          TO  IDX
           MOVE    SPA-GMN-TPTNUM   (IDX)  TO  SPA-GMN-PTNUM
           MOVE    SPA-GMN-TPTID    (IDX)  TO  SPA-GMN-PTID
      *
           MOVE    SPA-GMN-TNAME    (IDX)  TO  SPA-GMN-NAME
           MOVE    SPA-GMN-TSRYKA-G (IDX)  TO  SPA-GMN-SRYKA-G
           MOVE    SPA-GMN-TDRCD-G  (IDX)  TO  SPA-GMN-DRCD-G
           MOVE    SPA-GMN-TSRYNAIYOCD-G (IDX)
                                       TO  SPA-GMN-SRYNAIYOCD-G
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-GMN-TPTID (IDX) TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF               =   ZERO
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               PERFORM 500-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY
               MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-BIRTHDAY
               MOVE    PTINF-SEX           TO  SPA-NAI-SEX
           END-IF
           IF      SPA-GMN-THKNCOMBI(IDX)  =   SPACE  OR  ZERO
               MOVE    ZERO                TO  SPA-GMN-COMBINUM
           ELSE
               MOVE    SPA-GMN-THKNCOMBI(IDX)  TO  SPA-GMN-COMBINUM
           END-IF
      *    登録済みの患者の時、保険情報をセットする
           IF      SPA-GMN-PTID        NOT =  ZERO
      *
      *        排他制御処理
               PERFORM 990-LOCK-IN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4104-SELNUM-EXT
               END-IF
      *
               PERFORM 410114-KUMIHYOJI-SEC
               PERFORM 410115-KAKUHYOJI-SEC
               IF      SPA-GMN-SELNUM          =   ZERO
                   PERFORM 410116-JYURRKSET-SEC
               END-IF
      *---- (01.00.06) LINE MOD END ----------------------------------
           END-IF
      *
           IF      SPA-GMN-TUKETIME(SPA-GMN-SELNUM)    =  SPACE
      *         受付完了へ
               MOVE    62                  TO  SPA-GMN-CUR
           ELSE
               IF     (SPA-GMN-PTID        =   ZERO  )
      *            権限がある場合
                AND   (SPA-GSRAUTH-12      =   "1"  )
      *            患者登録へ
                   MOVE    58                  TO  SPA-GMN-CUR
               ELSE
      *            診療科へ
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    前回編集
           MOVE    SPA-GMN-PTNUM       TO  SPA-MAE-PTNUM
           MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
           MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
      *    前回患者ヘ編集
           IF      SPA-GMN-PTID    NOT =  ZERO
               MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
               MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
           END-IF
           .
       4104-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧選択処理
      *****************************************************************
       4110-UKETUKE-LIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-UKE-MAX
              IF      U02-UKETUKE-SELECT (IDX)   =  "T"
                  IF      IDX                 =   SPA-GMN-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    IDX             TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM      TO  SPA-GMN-SELNUM
           END-IF
      *
           IF      SPA-GMN-SELNUM      NOT =   ZERO
      *******  MOVE    SPA-GMN-SELNUM      TO  U02-SELNUM
      ******   MOVE    "SELNUM"            TO  WRK-MCP-WIDGET
               PERFORM 4104-SELNUM-CHK-SEC
           END-IF
      *
           .
       4110-UKETUKE-LIST-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       410114-KUMIHYOJI-SEC           SECTION.
      *
      *   保険組合せ読み出し
           INITIALIZE                  SPA-GMN-COMBI
           MOVE   ZERO                 TO  SPA-COMBI-MAX
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-COMB-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL       (IDX            =   15  )  OR
                               (FLG-HKNCOMBI   =   1    )
               MOVE    ZERO                TO  FLG-HKNOK
               IF     (SPA-SYSYMD          >=  COMB-TEKSTYMD) AND
                      (SPA-SYSYMD          <=  COMB-TEKEDYMD) AND
                      (COMB-DELKBN         =   SPACE        )
                   MOVE    1               TO  FLG-HKNOK
      *            労災保険情報の判定
                   IF      COMB-HKNNUM         =   "971"  OR  "973"
                       PERFORM 410114-RSIINF-CHK-SEC
                       IF      FLG-PTRSIINF    =   1
                           MOVE    ZERO            TO  FLG-HKNOK
                       END-IF
                   END-IF
               END-IF
               IF      FLG-HKNOK           =   1
      *---- (01.00.02) LINE MOD END   ---------------------------------
                   ADD     1                   TO  IDX
                   MOVE    IDX                 TO  SPA-COMBI-MAX
      *
                   MOVE    COMB-HKNCOMBINUM    TO  SPA-GMN-HKNCOMBINUM
                                                                   (IDX)
                   MOVE    COMB-SYU-TANSEIDONAME
                                    TO  SPA-GMN-SYU-TANSEIDONAME(IDX)
                   MOVE    COMB-KOH1-TANSEIDONAME  
                                    TO  SPA-GMN-KOH1-TANSEIDONAME(IDX)
                   MOVE    COMB-KOH2-TANSEIDONAME  
                                    TO  SPA-GMN-KOH2-TANSEIDONAME(IDX)
                   MOVE    COMB-KOH3-TANSEIDONAME  
                                    TO  SPA-GMN-KOH3-TANSEIDONAME(IDX)
                   MOVE    COMB-KOH4-TANSEIDONAME  
                                    TO  SPA-GMN-KOH4-TANSEIDONAME(IDX)
      *
                   MOVE    COMB-HKNNUM     TO  SPA-NAI-HKNNUM  (IDX)
      *            労災の時、部位を公費１の項目へ
                   IF      COMB-HKNNUM         =   "971"  OR  "973"
                       PERFORM 4101152-RSI-HENSYU-SEC
                   END-IF
      *
               END-IF
      *
               MOVE    "HKNCOMBI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-COMB-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
           .
       410114-KUMIHYOJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険情報の判定処理
      *****************************************************************
       410114-RSIINF-CHK-SEC           SECTION.
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTRSIINF-KEY"     TO  ORC-DBPATH
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
           .
       410114-RSIINF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    未確認保険公費表示処理
      *****************************************************************
       410115-KAKUHYOJI-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-MIKAKUNIN
           INITIALIZE                  SPA-NAI-MIKAKUNIN
           MOVE    ZERO                TO  SPA-MIKAKU-MAX
           MOVE    ZERO                TO  IDX
      *
      *   保険読み出し
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPID          TO  PTHKN-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTHKN-PTID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTHKNINF-KEY2"     TO  ORC-DBPATH
               PERFORM 900-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    ZERO                TO  IDX 
           PERFORM UNTIL       (IDX            =   10   )  OR
                               (FLG-PTHKNINF   =   1    )
               IF     (SPA-SYSYMD          >=  PTHKN-TEKSTYMD ) AND
                      (SPA-SYSYMD          <=  PTHKN-TEKEDYMD ) AND
                      (SPA-SYSYMD(1:6)     >   PTHKN-KAKUNINYMD
                                                         (1:6)) AND
                      (PTHKN-SAKJOKBN      =   SPACE    )
      *
      *            保険番号読込み  
                   INITIALIZE                  HKNNUM-REC
                   MOVE    SPA-HOSPID          TO  HKN-HOSPID
                   MOVE    PTHKN-HKNNUM        TO  HKN-HKNNUM
                   MOVE    PTHKN-TEKSTYMD      TO  HKN-TEKSTYMD
                   MOVE    "00"                TO  HKN-PAYKBN 
                   MOVE    HKNNUM-REC          TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-INV-SEC
      *            労災の時、対象外
                   IF    ( FLG-HKNNUM          =   ZERO   )  AND
                         ( PTHKN-HKNNUM    NOT =   "971" AND
                                                   "973"  )
                       ADD     1                   TO  IDX
                       MOVE    IDX                 TO  SPA-MIKAKU-MAX
                       MOVE    HKN-TANSEIDONAME    TO
                                                   SPA-GMN-TMIKAKUNAME
                                                                  (IDX)
                       MOVE    PTHKN-KAKUNINYMD    TO  WRK-SYMD
                       PERFORM 500-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO
                                                   SPA-GMN-TMIKAKUYMD
                                                                (IDX)
      *                記号・番号
                       IF     (PTHKN-KIGO          =   SPACE )  AND
                              (PTHKN-NUM           =   SPACE )
                           CONTINUE
                       ELSE
                           STRING  PTHKN-KIGO      DELIMITED  BY  SPACE
                                   "・"            DELIMITED  BY  SIZE
                                   PTHKN-NUM       DELIMITED  BY  SPACE
                                   INTO            SPA-GMN-TMIKIGONUM
                                                                (IDX)
                           END-STRING
                       END-IF
      *
                       MOVE    PTHKN-KAKUNINYMD    TO
                                                   SPA-NAI-KAKUNINYMD
                                                                 (IDX)
                       MOVE    PTHKN-HKNID         TO  SPA-NAI-HKNID
                                                                 (IDX)
                       MOVE    1                   TO  SPA-NAI-HKNKBN
                                                                 (IDX)
                       MOVE    PTHKN-KAKUNINYMD    TO
                                                   SPA-MAE-KAKUNINYMD
                                                                 (IDX)
                       MOVE    SPA-GMN-TMIKAKUYMD (IDX)
                                                   TO
                                                   SPA-MAE-KAKUNINYMDW
                                                                 (IDX)
                   END-IF
               END-IF    
      *
               MOVE    "PTHKNINF-KEY2"       TO  ORC-DBPATH
               PERFORM 900-PTHKNINF-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "PTHKNINF-KEY2"     TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
      *    公費読み出し
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPID          TO  PTKOH-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTKOHINF-KEY2"     TO  ORC-DBPATH
               PERFORM 900-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           PERFORM UNTIL       (IDX            =   10   )  OR
                               (FLG-PTKOHINF   =   1    )
               IF     (SPA-SYSYMD      >=  PTKOH-TEKSTYMD )      AND
                      (SPA-SYSYMD      <=  PTKOH-TEKEDYMD )      AND
                      (SPA-SYSYMD(1:6) >   PTKOH-KAKUNINYMD(1:6)) AND 
                      (PTKOH-SAKJOKBN  =   SPACE           )
      *
      *            保険番号読込み  
                   INITIALIZE                  HKNNUM-REC
                   MOVE    SPA-HOSPID          TO  HKN-HOSPID
                   MOVE    PTKOH-KOHNUM        TO  HKN-HKNNUM
                   MOVE    HKNNUM-REC          TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "HKNNUM-KEY2"       TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
                   IF      MCP-RC              =   ZERO
                       MOVE    "HKNNUM-KEY2"       TO  ORC-DBPATH
                       PERFORM 900-HKNNUM-READ-SEC
                   ELSE
                        MOVE    1                  TO  FLG-HKNNUM
                   END-IF
      *
                   PERFORM UNTIL       FLG-HKNNUM      =   1
                       IF      HKN-HKNKOHSBTKBN  =   "3"
                           IF     (SPA-HOSPSBT     =   HKN-PAYKBN (1:1))
                           AND    (SPA-ROUPAYKBN   =   HKN-PAYKBN (2:1))
                               PERFORM 4101153-HOKI-HENSYU-SEC
                           END-IF
                       ELSE
                           PERFORM 4101153-HOKI-HENSYU-SEC
                       END-IF
                       IF      FLG-HKNNUM           =   ZERO
                           MOVE    "HKNNUM-KEY2"       TO  ORC-DBPATH
                           PERFORM 900-HKNNUM-READ-SEC
                       END-IF 
                   END-PERFORM
      *    カーソルクロース
                   MOVE    "HKNNUM-KEY2"       TO  ORC-DBPATH
                   PERFORM 990-DBCLOSE-SEC
               END-IF
      *
               MOVE    "PTKOHINF-KEY2"       TO  ORC-DBPATH
               PERFORM 900-PTKOHINF-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "PTKOHINF-KEY2"     TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           .
       410115-KAKUHYOJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の時、部位を追加編集処理
      *****************************************************************
       4101152-RSI-HENSYU-SEC           SECTION.
      *
      *    労災の時、部位を追加
      *    患者保険情報検索
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTRSIINF-KEY"     TO  ORC-DBPATH
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-HKNKBN-MEI
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
                   MOVE    "短"        TO  WRK-HKNKBN-MEI
               WHEN    "2"
                   MOVE    "傷"        TO  WRK-HKNKBN-MEI
               WHEN    "3"
                   MOVE    "ア"        TO  WRK-HKNKBN-MEI
           END-EVALUATE
           IF      PTRSI-COMMENT       =   SPACE
               MOVE    WRK-HKNKBN-MEI  TO  SPA-GMN-KOH1-TANSEIDONAME
                                                                   (IDX)
           ELSE
               MOVE    SPACE           TO  SPA-GMN-KOH1-TANSEIDONAME
                                                                   (IDX)
               STRING  WRK-HKNKBN-MEI          DELIMITED  BY  SIZE
                       "（"                    DELIMITED  BY  SIZE
                       PTRSI-COMMENT           DELIMITED  BY  SPACE
                       "）"                    DELIMITED  BY  SIZE
                       INTO              SPA-GMN-KOH1-TANSEIDONAME (IDX)
               END-STRING
           END-IF
           .
       4101152-RSI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費未確認編集処理
      *****************************************************************
       4101153-HOKI-HENSYU-SEC           SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    IDX                 TO  SPA-MIKAKU-MAX
           MOVE    HKN-TANSEIDONAME    TO  SPA-GMN-TMIKAKUNAME(IDX)
      *
           MOVE    PTKOH-KAKUNINYMD    TO  WRK-SYMD
           PERFORM 500-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TMIKAKUYMD(IDX)
      *    受給者番号
           MOVE    PTKOH-JKYSNUM       TO  SPA-GMN-TMIKIGONUM (IDX)
      *
           MOVE    PTKOH-KAKUNINYMD    TO  SPA-NAI-KAKUNINYMD (IDX)
           MOVE    PTKOH-KOHID         TO  SPA-NAI-HKNID      (IDX)
           MOVE    2                   TO  SPA-NAI-HKNKBN     (IDX)
           MOVE    PTKOH-KAKUNINYMD    TO  SPA-MAE-KAKUNINYMD (IDX)
           MOVE    SPA-GMN-TMIKAKUYMD (IDX)
                                       TO  SPA-MAE-KAKUNINYMDW (IDX)
      *
           MOVE    1                   TO  FLG-HKNNUM
      *
           .
       4101153-HOKI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴表示処理
      *****************************************************************
       410116-JYURRKSET-SEC           SECTION.
      *
      *    受診履歴ＯＰＥＮ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-SYSYMD          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****MOVE    "JYURRK-KEY2"       TO  ORC-DBPATH
           MOVE    "JYURRK-KEY10"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY10"     TO  ORC-DBPATH
               PERFORM 900-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  WRK-JRK-HKNCOMBI
           INITIALIZE                  WRK-JYURRK-AREA
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-SRYKA        TO  WRK-JRK-SRYKA
               MOVE    JYURRK-DRCD         TO  WRK-JRK-DRCD
      *        保険組合番号
               MOVE    JYURRK-HKNCOMBINUM  TO  WRK-JRK-HKNCOMBI
           END-IF
      *    カーソルクロース
           MOVE    "JYURRK-KEY10"      TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-JRK-SRYKA  NOT  =  SPACE
               MOVE    ZERO                TO  FLG-HENFLG
               PERFORM VARYING IDY         FROM   1   BY  1
                       UNTIL  (IDY         >   SPA-SRYKA-MAX )
                       OR     (FLG-HENFLG  =   1  )
                   IF      WRK-JRK-SRYKA   =   SPA-GMN-SRYKALST
                                                       (IDY)(1:2) 
                       MOVE    SPA-GMN-SRYKALST (IDY)
                                               TO  SPA-GMN-SRYKA-G
                       MOVE    1               TO  FLG-HENFLG
                   END-IF
               END-PERFORM
      *
               IF      SPA-GMN-SRYKA       NOT =   SPA-MAE-SRYKA
      *            診療科が変わった時、ドクターを再編集する
                   INITIALIZE                  ORCSDRSETAREA
                   MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
                   MOVE    "1"                 TO  ORCSDR-KBN
                   MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
                   CALL    "ORCSDRSET"         USING
                                               SPA-AREA
                                               ORCSDRSETAREA
                   MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
                   MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
               END-IF
               MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
           END-IF
      *    ドクター、保険組合せ編集
           PERFORM 4101161-DRCD-HKNCOMBI-SEC
           .
       410116-JYURRKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター、保険組合せ編集処理
      *****************************************************************
       4101161-DRCD-HKNCOMBI-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX           >   SPA-DRCD-MAX 
               IF      WRK-JRK-DRCD(2:4)   =   SPA-GMN-DRCDLST(IDX)
                                                                 (1:4)
                   MOVE    SPA-GMN-DRCDLST(IDX)
                                           TO  SPA-GMN-DRCD-G
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-DRCD-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               IF      WRK-JRK-DRCD        =   SPACE
                   MOVE    SPA-GMN-DRCDLST(1)  TO  SPA-GMN-DRCD-G
               ELSE
                   MOVE    WRK-JRK-DRCD(2:4)   TO  SPA-GMN-DRCD
      *           ドクター
                   PERFORM 4105-DRCD-CHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    SPA-GMN-DRCDLST(1)
                                                   TO  SPA-GMN-DRCD-G
                   END-IF
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-COMBINUM
      *    削除分ならないことと
           IF      WRK-JRK-HKNCOMBI    NOT =   ZERO
               PERFORM VARYING    IDX-C    FROM    1   BY  1
                       UNTIL      IDX-C    >   SPA-COMBI-MAX
                   IF      WRK-JRK-HKNCOMBI    =   SPA-GMN-HKNCOMBINUM
                                                          (IDX-C)
                       MOVE    WRK-JRK-HKNCOMBI    TO  SPA-GMN-COMBINUM
                       MOVE    SPA-COMBI-MAX       TO  IDX-C
                   END-IF
               END-PERFORM
           END-IF
           IF     (SPA-COMBI-MAX       >   ZERO )  AND
                  (SPA-GMN-COMBINUM    =   ZERO )
               PERFORM 4101161-HKNCOMBI-SET-SEC
           END-IF
           .
       4101161-DRCD-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ初期表示処理
      *****************************************************************
       4101161-HKNCOMBI-SET-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-HKN
           MOVE    ZERO                TO  WRK-CNT-HKN2
           MOVE    ZERO                TO  WRK-IDXH
      *
           PERFORM VARYING     IDX-C   FROM    1   BY  1
                   UNTIL       IDX-C   >   SPA-COMBI-MAX
      *      自費は対象外
             IF      SPA-NAI-HKNNUM (IDX-C)(1:2)   =   "98"
               CONTINUE
             ELSE
               MOVE    ZERO                TO  WRK-CNT-HKN2
               IF      SPA-NAI-HKNNUM (IDX-C)  NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-GMN-KOH1-TANSEIDONAME(IDX-C)  NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-GMN-KOH2-TANSEIDONAME(IDX-C)  NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-GMN-KOH3-TANSEIDONAME(IDX-C)  NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-GMN-KOH4-TANSEIDONAME(IDX-C)  NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      WRK-CNT-HKN         <=  WRK-CNT-HKN2
                   MOVE    IDX-C               TO  WRK-IDXH
                   MOVE    WRK-CNT-HKN2        TO  WRK-CNT-HKN
               END-IF
      *        労災・自賠責があれば対象とする
               IF      SPA-NAI-HKNNUM (IDX-C)  =   "971"
                                               OR  "973"
                   MOVE    IDX-C               TO  WRK-IDXH
                   MOVE    SPA-COMBI-MAX       TO  IDX-C
               END-IF
             END-IF
           END-PERFORM
      *
           IF      WRK-IDXH            =   ZERO
               MOVE    1               TO  IDX-C
           ELSE
               MOVE    WRK-IDXH        TO  IDX-C
           END-IF
           MOVE    SPA-GMN-HKNCOMBINUM (IDX-C)
                                       TO  SPA-GMN-COMBINUM
      *
           .
       4101161-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           IF      SPA-MACHIPG         =   "U02"
      *        待ち受け画面の時、Ｍ０１へ
               MOVE    "M01"               TO  SPA-SAKIPG
           ELSE
      *        待ち受け以外の時、Ｋ０２へ
               IF      SPA-HZN-MOTOPG      =   SPACE
                   MOVE    "K02"               TO  SPA-SAKIPG
               ELSE
                   MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
               END-IF
           END-IF
      *
           MOVE    "U02"               TO  SPA-MOTOPG
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
      *---- (01.00.05) LINE ADD START ---------------------------------
      *
      *****************************************************************
      *    項目クリア
      *****************************************************************
       493-CLEAR-SEC                   SECTION.
      *
      *    クリア
           PERFORM 1009-HEAD-CLEAR-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
           .
       493-CLEAR-EXT.
           EXIT.
      *---- (01.00.05) LINE ADD END -----------------------------------
      *
      *
      *****************************************************************
      *    受付内容クリア処理
      *****************************************************************
       1009-HEAD-CLEAR-SEC                   SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  WRK-SRYKA
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           INITIALIZE                      SPA-GMN-AREA
           INITIALIZE                      SPA-NAIB-AREA
           MOVE    SPA-GMN-SRYKALST (1)
                                       TO  SPA-GMN-SRYKA-G
           IF      WRK-SRYKA       NOT =   SPA-GMN-SRYKA
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
               MOVE    "1"                 TO  ORCSDR-KBN
               MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           END-IF
           MOVE    SPA-GMN-DRCDLST  (1)
                                       TO  SPA-GMN-DRCD-G
           MOVE    SPA-GMN-SRYNAILST(1)
                                       TO  SPA-GMN-SRYNAIYOCD-G
           MOVE    SPA-GMN-SYOHOLST (1)
                                       TO  SPA-GMN-SYOHO-G
           MOVE    SPA-GMN-KARUTELST(1)
                                       TO  SPA-GMN-KARUTE-G
           .
       1009-HEAD-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    受付一覧更新処理
      *****************************************************************
       4931-UKELIST-SEC                   SECTION.
      *
      *    選択中でないこと
           IF     (SPA-GMN-SELNUM  NOT =   ZERO )  OR
                  (SPA-GMN-NAME    NOT =   SPACE)
               IF      MCP-WIDGET          =   "B11S"
                   MOVE    "0027"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
               GO      TO      4931-UKELIST-EXT
           END-IF
      *    受付一覧編集
           PERFORM 3101-UKETU-HEN-SEC
      *
           .
       4931-UKELIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       230-MAEKPTNUM-SEC                   SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      230-MAEKPTNUM-EXT
           END-IF
      *
           MOVE    SPA-LAST-PTNUM      TO  U02-PTNUM
           MOVE    SPA-LAST-PTNUM      TO  SPA-GMN-PTNUM
           MOVE    1                   TO  FLG-IN-PTNUM
           PERFORM 4101-PTNUM-CHK-SEC
      *
           .
       230-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    前（↑）編集処理
      *****************************************************************
       465-UP-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      465-UP-SYORI-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    "2001"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE
                                       -   1
           END-IF
           .
       465-UP-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次（↑）編集処理
      *****************************************************************
       475-DOWN-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      475-DOWN-SYORI-EXT
           END-IF
      *
           COMPUTE IDY                 =   SPA-GMN-PAGE
                                       *   5
                                       +   1
           IF      IDY                 >   SPA-MIKAKU-MAX
               MOVE    "2002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE
                                       +   1
           END-IF
           .
       475-DOWN-SYORI-EXT.
           EXIT.
      *---- (01.00.05) LINE ADD END -----------------------------------
      *
      *****************************************************************
      *    予約画面へ
      *****************************************************************
       460-YOYAKU-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-U02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "U02"               TO  SPA-MOTOPG3
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       460-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       480-KENSAKU-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-U02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           IF      MCP-WIDGET          =   "B09"
               CONTINUE
           ELSE 
               MOVE    SPA-GMN-NAME        TO  SPA-NAME
           END-IF
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "P97    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       480-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-U02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "U01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
        490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷のみ処理
      *****************************************************************
       495-PRINT-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    登録チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      495-PRINT-EXT
           END-IF
      *
      *    帳票プログラム取得
           PERFORM 4901-PRINT-PG-SEC
      *
      *  カルテ印刷
           IF      SPA-GMN-KARUTE          =   "1"  OR  "2"
               IF     (SPA-GMN-PTID            =   ZERO  )  OR
                      (SPA-GMN-SRYKA           =   SPACE )
      ************OR *(SPA-GMN-COMBINUM        =   ZERO  )
                   MOVE    "0024"          TO  SPA-ERRCD
                   GO      TO   495-PRINT-EXT
               END-IF
      *
               MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
               MOVE    SPA-GMN-SRYKANAME   TO  SPA-SRYKAMEI
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-COMBINUM    TO  SPA-HKNCOMBINUM   
      *********CALL    "ORCHC01"           USING  SPA-AREA 
               MOVE    SPA-GMN-KARUTE      TO  ORCHC01-SYORIKBN
               CALL    WRK-KARUTE-PG       USING  SPA-AREA
                                                  ORCHC01AREA
           END-IF
      *
      *    処方箋印刷
           IF      SPA-GMN-SYOHO          =   "1"
               IF     (SPA-GMN-PTID            =   ZERO  )  OR
                      (SPA-GMN-SRYKA           =   SPACE )
      *************OR (SPA-GMN-COMBINUM        =   ZERO  )
                   MOVE    "0025"          TO  SPA-ERRCD
                   GO      TO   495-PRINT-EXT
               END-IF
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-NAME        TO  SPA-NAME
               EVALUATE    SPA-NAI-SEX
                   WHEN    "1"
                       MOVE    "男"                TO  SPA-SEX
                   WHEN    "2"
                       MOVE    "女"                TO  SPA-SEX
               END-EVALUATE
               MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
               MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
               MOVE    SPA-GMN-SRYKANAME   TO  SPA-SRYKAMEI
               MOVE    "2"                 TO  SPA-NYUGAIKBN
               IF      SPA-GMN-DRCD        =   SPACE
                   MOVE    SPACE               TO  SPA-DRCD
               ELSE
                   MOVE    "1"                 TO  SPA-DRCD (1:1) 
                   MOVE    SPA-GMN-DRCD        TO  SPA-DRCD (2:)
               END-IF
               MOVE    SPA-GMN-COMBINUM    TO  SPA-HKNCOMBINUM
               INITIALIZE                      ORCHC19AREA
               MOVE    ZERO                TO  ORCHC19-RENNUM
               MOVE    "1"                 TO  ORCHC19-KBN
      ****     CALL    "ORCHC19"           USING   SPA-AREA
      ****                                         ORCHC19AREA
               CALL    WRK-SYOHOU-PG       USING   SPA-AREA
                                                   ORCHC19AREA
           END-IF
      *
           PERFORM 493-CLEAR-SEC
           .
      *
        495-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       4901-PRINT-PG-SEC              SECTION.
      *
      *    カルテ
           MOVE    SPACE               TO  WRK-KARUTE-PG
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000001"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-KARUTE-PG
      *
      *    処方せん
           MOVE    SPACE               TO  WRK-SYOHOU-PG
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000002"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
      *
      *    カルテ
      *****MOVE    SPACE               TO  WRK-KARUTE-PG
      *    INITIALIZE                      SYS-1032-REC
      *    MOVE    "1032"              TO  SYS-1032-KANRICD
      *    MOVE    "00000001"          TO  SYS-1032-KBNCD
      *    MOVE    SYS-1032-REC        TO  MCPDATA-REC
      *    PERFORM 910-SYSKANRI-READ-SEC
      *    IF      FLG-SYSKANRI        =   ZERO
      *        MOVE    MCPDATA-REC         TO  SYS-1032-REC
      *        独自開発分
      *        IF      SYS-1032-ORCACSTCHK     =   "T"
      *            MOVE    SYS-1032-ORCACSTNM  TO  WRK-KARUTE-PG
      *        ELSE
      *        ＯＲＣＡ標準分
      *            MOVE    SYS-1032-ORCAORGNM  TO  WRK-KARUTE-PG
      *        END-IF
      *****END-IF
           IF      WRK-KARUTE-PG       =   SPACE
               MOVE    "ORCHC01"           TO  WRK-KARUTE-PG
           END-IF
      *
      ***  処方せん
      *    MOVE    SPACE               TO  WRK-SYOHOU-PG
      *    INITIALIZE                      SYS-1032-REC
      *    MOVE    "1032"              TO  SYS-1032-KANRICD
      *    MOVE    "00000002"          TO  SYS-1032-KBNCD
      *    MOVE    SYS-1032-REC        TO  MCPDATA-REC
      *    PERFORM 910-SYSKANRI-READ-SEC
      *    IF      FLG-SYSKANRI        =   ZERO
      *        MOVE    MCPDATA-REC         TO  SYS-1032-REC
      *        独自開発分
      *        IF      SYS-1032-ORCACSTCHK     =   "T"
      *            MOVE    SYS-1032-ORCACSTNM  TO  WRK-SYOHOU-PG
      *        ELSE
      *        ＯＲＣＡ標準分
      *            MOVE    SYS-1032-ORCAORGNM  TO  WRK-SYOHOU-PG
      *        END-IF
      *****END-IF
           IF      WRK-SYOHOU-PG       =   SPACE
               MOVE    "ORCHC19"           TO  WRK-SYOHOU-PG
           END-IF
      *
           .
      *
       4901-PRINT-PG-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    登録チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
      *    ＣＬＡＩＭ接続チェック処理
           PERFORM 4905-CLAIM-CHK-SEC
           IF      FLG-CLAIM       =   2 
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
           PERFORM 49001-KOUSIN-SEC
           .
       490-TOUROKU-EXT.
           EXIT.   
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       49001-KOUSIN-SEC             SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    受付マスタ追加          
           IF      SPA-GMN-SELNUM            =   ZERO
               PERFORM 4901-UKEADD-SEC
           ELSE
               MOVE    SPA-GMN-SELNUM            TO  IDX
               IF      SPA-GMN-TUKEID(IDX)   NOT =   ZERO
      *    受付マスタ更新
                  PERFORM 4902-UKEUPD-SEC
               ELSE
      *    受付マスタ追加
                  PERFORM 4901-UKEADD-SEC
      *    予約マスタ更新
                  IF      SPA-ERRCD          =   SPACE  
                      PERFORM 4903-YYKUPD-SEC
                  END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        保険・公費情報更新
               PERFORM 4904-HKN-KOUSIN-SEC
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  49001-KOUSIN-EXT
           END-IF
      *
           MOVE    SPA-KYOUTU          TO  WRK-SPA-KYOUTU
      *
           IF     (SPA-GMN-KARUTE      =   "1"  OR "2")  OR
                  (SPA-GMN-SYOHO       =   "1" )
      *        帳票プログラム取得
               PERFORM 4901-PRINT-PG-SEC
           END-IF
      *
      *  カルテ印刷
           IF      SPA-GMN-KARUTE          =   "1"   OR  "2"
      ************ SPA-GMN-COMBINUM    NOT =   ZERO
               MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
               MOVE    SPA-GMN-SRYKANAME   TO  SPA-SRYKAMEI
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-COMBINUM    TO  SPA-HKNCOMBINUM   
      *********CALL    "ORCHC01"           USING  SPA-AREA 
               MOVE    SPA-GMN-KARUTE      TO  ORCHC01-SYORIKBN
               CALL    WRK-KARUTE-PG       USING  SPA-AREA
                                                  ORCHC01AREA
           END-IF
      *
      *    処方箋印刷
           IF      SPA-GMN-SYOHO          =   "1"
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-NAME        TO  SPA-NAME
               EVALUATE    SPA-NAI-SEX
                   WHEN    "1"
                       MOVE    "男"                TO  SPA-SEX
                   WHEN    "2"
                       MOVE    "女"                TO  SPA-SEX
               END-EVALUATE
               MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
               MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
               MOVE    SPA-GMN-SRYKANAME   TO  SPA-SRYKAMEI
               MOVE    "2"                 TO  SPA-NYUGAIKBN
               IF      SPA-GMN-DRCD        =   SPACE
                   MOVE    SPACE               TO  SPA-DRCD
               ELSE
                   MOVE    "1"                 TO  SPA-DRCD (1:1) 
                   MOVE    SPA-GMN-DRCD        TO  SPA-DRCD (2:)
               END-IF
               MOVE    SPA-GMN-COMBINUM    TO  SPA-HKNCOMBINUM
               INITIALIZE                      ORCHC19AREA
               MOVE    ZERO                TO  ORCHC19-RENNUM
               MOVE    "1"                 TO  ORCHC19-KBN
      ******** CALL    "ORCHC19"           USING   SPA-AREA
      ********                                     ORCHC19AREA
               CALL    WRK-SYOHOU-PG       USING   SPA-AREA
                                                   ORCHC19AREA
           END-IF
      *
           IF      FLG-CLAIM       =   1 
               PERFORM 4906-CLAIM-SOUSIN-SEC
           END-IF
      *
           MOVE    WRK-SPA-KYOUTU          TO  SPA-KYOUTU
      *
      *??? INITIALIZE                            SPA-U02
      *??? PERFORM 310-SPASET-SEC
           PERFORM 493-CLEAR-SEC  
           PERFORM 3101-UKETU-HEN-SEC
           .
       49001-KOUSIN-EXT.
           EXIT.   
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       4109-TOUROKU-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-NAME            =  SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO  TO  4109-TOUROKU-CHK-EXT
           END-IF
      *
           IF     (SPA-GMN-SRYKA           =  SPACE )  OR
                  (SPA-GMN-SRYNAIYOCD-G    =  SPACE )
               MOVE    "0026"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
      *
          .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受付追加処理
      *****************************************************************
       4901-UKEADD-SEC              SECTION.
      *
      *    受付ＩＤ＝００検索
           MOVE    SPACE               TO  UKETUKE-REC
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPID          TO  UKE-HOSPID
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    ZERO                TO  UKE-UKEID
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE         =   ZERO
               ADD     1                   TO  UKE-PTID
               IF      UKE-PTID            >   99999
                   MOVE    "1007"              TO  SPA-ERRCD
                   GO  TO  4901-UKEADD-EXT
               END-IF
               MOVE    UKE-PTID            TO  WRK-UKEID
      *
               MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
               MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
               MOVE    UKETUKE-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "UKETUKE-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    SPA-HOSPID          TO  UKE-HOSPID
               MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
               MOVE    ZERO                TO  UKE-UKEID
               MOVE    1                   TO  UKE-PTID
               MOVE    1                   TO  WRK-UKEID
      *        作成年月日
               MOVE    SMCNDATE-YMD        TO  UKE-CREYMD
               MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
               MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
               MOVE    UKETUKE-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "UKETUKE-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           END-IF
      * 
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "U02 UKETUKE KEY INS ERR:"  MCP-RC
                       ",KEY:" UKE-KEY
               GO  TO  4901-UKEADD-EXT
           END-IF
      *
      *    明細レコード追加
      *
           INITIALIZE                       UKETUKE-REC
           MOVE    SPA-HOSPID               TO  UKE-HOSPID
           MOVE    SPA-SYSYMD               TO  UKE-UKEYMD
           MOVE    WRK-UKEID                TO  UKE-UKEID
           ACCEPT  UKE-UKETIME              FROM   TIME
           IF      SPA-GMN-PTNUM            =   SPACE
               MOVE    ZERO                     TO  UKE-PTID
           ELSE
               MOVE    SPA-GMN-PTID             TO  UKE-PTID
           END-IF
           MOVE    SPA-GMN-NAME             TO  UKE-NAME
           MOVE    SPA-GMN-SRYKA            TO  UKE-SRYKA
           IF      SPA-GMN-DRCD        NOT =   SPACE
               MOVE    "1"                      TO  UKE-DRCD
               MOVE    SPA-GMN-DRCD             TO  UKE-DRCD(2:4)
           END-IF
           MOVE    SPA-GMN-SRYNAIYOCD       TO  UKE-SRYNAIYO
      *    保険組合せ
           MOVE    SPA-GMN-COMBINUM         TO  UKE-HKNCOMBI
      *
      **** MOVE    UKE-UKEYMD               TO  UKE-CREYMD
      ****                                      UKE-UPYMD
           IF      SPA-GMN-SELNUM           >   ZERO
               MOVE    SPA-GMN-SELNUM           TO  IDX 
               IF      SPA-GMN-TYYKTIME(IDX)    NOT =  SPACE
                   MOVE    SPA-GMN-TYYKTIME (IDX)   TO  UKE-YYKTIME
                   MOVE    SPA-GMN-TKYYKTIME(IDX)   TO  UKE-KEYYYKTIME
                   MOVE    SPA-GMN-TYYKID   (IDX)   TO  UKE-YYKID
               END-IF
           END-IF
      *    作成年月日
           MOVE    SMCNDATE-YMD        TO  UKE-CREYMD
           MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
           MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
           MOVE    UKETUKE-REC              TO  WRK-UKETUKE-REC
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE         =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               DISPLAY "U02 UKETUKE INS ERR KEY:"  UKE-KEY
               GO      TO      4901-UKEADD-EXT
           END-IF
      *
           MOVE    WRK-UKETUKE-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"               TO  MCP-FUNC
           MOVE    "UKETUKE-KEY"            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"               USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
      *
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "U02 UKETUKE INS ERR:"  MCP-RC
                       ",KEY:" UKE-KEY
           END-IF
           .
       4901-UKEADD-EXT.
           EXIT.   
      *
      *****************************************************************
      *    受付更新処理
      *****************************************************************
       4902-UKEUPD-SEC              SECTION.
      *
           INITIALIZE                        UKETUKE-REC
           MOVE    SPA-HOSPID                TO  UKE-HOSPID
           MOVE    SPA-SYSYMD                TO  UKE-UKEYMD
           MOVE    SPA-GMN-TUKEID  (IDX)     TO  UKE-UKEID
      *
           PERFORM  900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE         NOT =   ZERO
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "U02 UKETUKE UPD ERR KEY:" UKE-KEY
               GO      TO      4902-UKEUPD-EXT
           END-IF
      *
      *****ACCEPT  UKE-UKETIME               FROM   TIME
           MOVE    SPA-GMN-PTID              TO  UKE-PTID
           MOVE    SPA-GMN-NAME              TO  UKE-NAME
           MOVE    SPA-GMN-SRYKA             TO  UKE-SRYKA
           IF      SPA-GMN-DRCD        NOT =   SPACE
               MOVE    "1"                       TO  UKE-DRCD
               MOVE    SPA-GMN-DRCD              TO  UKE-DRCD(2:4)
           END-IF
           MOVE    SPA-GMN-SRYNAIYOCD        TO  UKE-SRYNAIYO
      *    保険組合せ
           MOVE    SPA-GMN-COMBINUM         TO  UKE-HKNCOMBI
           IF      SPA-GMN-TYYKTIME(IDX)     NOT =  SPACE
               MOVE    SPA-GMN-TYYKTIME (IDX)    TO  UKE-YYKTIME
               MOVE    SPA-GMN-TKYYKTIME(IDX)    TO  UKE-KEYYYKTIME
               MOVE    SPA-GMN-TYYKID   (IDX)    TO  UKE-YYKID
           END-IF
      *****MOVE    UKE-UKEYMD                TO  UKE-CREYMD
      *****                                      UKE-UPYMD
      *
      **** PERFORM  900-UKETUKE-READ-SEC
      *    IF      FLG-UKETUKE         NOT =   ZERO
      *        MOVE    "1003"              TO  SPA-ERRCD
      *        DISPLAY "U02 UKETUKE UPD ERR KEY:" UKE-KEY
      *        GO      TO      4902-UKEUPD-EXT
      *****END-IF
      *
           MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
           MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
      *
           MOVE    UKETUKE-REC               TO  MCPDATA-REC
           MOVE    "DBUPDATE"                TO  MCP-FUNC
           MOVE    "UKETUKE-KEY"             TO  ORC-DBPATH
           CALL    "ORCMCPSUB"                USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
      *
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "U02 UKETUKE UPD ERR:"  MCP-RC
                       ",KEY:" UKE-KEY
           END-IF
           .
       4902-UKEUPD-EXT.
           EXIT.   
      *
      *****************************************************************
      *    予約更新処理
      *****************************************************************
       4903-YYKUPD-SEC              SECTION.
      * 
           INITIALIZE                    YYK-KEY
           MOVE   SPA-HOSPID             TO  YYK-HOSPID
           MOVE   SPA-GMN-TSRYNAIYOCD(IDX)
                                         TO  YYK-SRYNAIYO
           MOVE   "1"                    TO  YYK-DRCD
           MOVE   SPA-GMN-TDRCD    (IDX) TO  YYK-DRCD(2:4)
           MOVE   SPA-SYSYMD             TO  YYK-YYKYMD
           MOVE   SPA-GMN-TKYYKTIME(IDX) TO  YYK-KEYYYKTIME
           MOVE   SPA-GMN-TYYKID   (IDX) TO  YYK-YYKID
           PERFORM 900-YOYAKU-READ-SEC
           IF      FLG-YYK             =   ZERO
               MOVE  "1"                   TO  YYK-RAIINZUMI
      * 
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      * 
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "YYK-KEY"           TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1004"              TO  SPA-ERRCD
                   DISPLAY "U02 YYK    UPD ERR:"  MCP-RC
                       ",KEY:" YYK-KEY
               END-IF
           END-IF
           .
       4903-YYKUPD-EXT.
           EXIT.
      *
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
               IF      WRK-GMN-SELNUM        =   SPA-U02X-GMN-SEL
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
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    CLAIM-FILE          TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *
           MOVE    SPA-GMN-SCODE       TO  SHELLTBL-ARG1
           MOVE    SPA-GMN-PTID        TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG2
           MOVE    SPA-GMN-SRYKA       TO  SHELLTBL-ARG3
           MOVE    UKE-UKETIME         TO  SHELLTBL-ARG4
           MOVE    SPA-HOSPID          TO  SHELLTBL-ARG5
      *    ドクター
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    "@@@@@"             TO  SHELLTBL-ARG8
           ELSE
               MOVE    SPACE               TO  SHELLTBL-ARG8
               MOVE    "1"                 TO  SHELLTBL-ARG8(1:1)
               MOVE    SPA-GMN-DRCD        TO  SHELLTBL-ARG8(2:4)
           END-IF
      *    保険組合せ
           IF      SPA-GMN-COMBINUM    =   ZERO
               MOVE    "@@@@"              TO  SHELLTBL-ARG9
           ELSE
               MOVE    SPA-GMN-COMBINUM    TO  SHELLTBL-ARG9
           END-IF
      *
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
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4905-CLAIM-CHK-SEC               SECTION.
      *
           INITIALIZE                  SPA-U02X-AREA
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
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   1
               CONTINUE
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-9000-REC
               MOVE    SYS-9000-CODE       TO  SPA-GMN-SCODE
               MOVE    1                   TO  SPA-U02X-GMN-SEL
               IF      SYS-9000-SETUZOKU   =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SYS-9000-CODE       TO  SPA-U02X-CODE
                   IF      SYS-9000-UKEBUTTON (02) =   "T"
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
      *
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-U02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGU02X"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-U02
     *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "U02X"              TO  SPA-SAKIPG
      *    カーソル移動
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "U02X"              TO  MCP-WINDOW
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
      *    保険・公費情報更新処理
      *****************************************************************
       4904-HKN-KOUSIN-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MIKAKU-MAX
               EVALUATE    SPA-NAI-HKNKBN (IDX)
      *            保険
                   WHEN    1
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    SPA-HOSPID          TO  PTHKN-HOSPID
                       MOVE    SPA-GMN-PTID        TO  PTHKN-PTID
                       MOVE    SPA-NAI-HKNID (IDX) TO  PTHKN-HKNID
      *
                       MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                       MOVE    "DBSELECT"          TO  MCP-FUNC
                       MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"         USING
                                                   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
                       IF      MCP-RC              =   ZERO
                           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
                           PERFORM 900-PTHKNINF-READ-SEC
                       ELSE
                           MOVE    1               TO  FLG-PTHKNINF
                       END-IF
                       IF      FLG-PTHKNINF        =   ZERO
                           MOVE    SPA-NAI-KAKUNINYMD (IDX)
                                                   TO  PTHKN-KAKUNINYMD
      *
                           MOVE    SMCNDATE-YMD        TO  PTHKN-UPYMD
                           MOVE    SMCNDATE-HMS        TO  PTHKN-UPHMS
      *
                           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                           MOVE    "DBUPDATE"          TO  MCP-FUNC
                           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
                           CALL    "ORCMCPSUB"         USING
                                                       MCPAREA
                                                       ORCMCPAREA
                                                       MCPDATA-REC
                       END-IF
      *
      *            公費
                   WHEN    2
                       INITIALIZE                  PTKOHINF-REC
                       MOVE    SPA-HOSPID          TO  PTKOH-HOSPID
                       MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
                       MOVE    SPA-NAI-HKNID (IDX) TO  PTKOH-KOHID
      *
                       MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                       MOVE    "DBSELECT"          TO  MCP-FUNC
                       MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"         USING
                                                   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
                       IF      MCP-RC              =   ZERO
                           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
                           PERFORM 900-PTKOHINF-READ-SEC
                       ELSE
                           MOVE    1               TO  FLG-PTKOHINF
                       END-IF
                       IF      FLG-PTKOHINF        =   ZERO
                           MOVE    SPA-NAI-KAKUNINYMD (IDX)
                                                   TO  PTKOH-KAKUNINYMD
      *
                           MOVE    SMCNDATE-YMD        TO  PTKOH-UPYMD
                           MOVE    SMCNDATE-HMS        TO  PTKOH-UPHMS
      *
                           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                           MOVE    "DBUPDATE"          TO  MCP-FUNC
                           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
                           CALL    "ORCMCPSUB"         USING
                                                       MCPAREA
                                                       ORCMCPAREA
                                                       MCPDATA-REC
                       END-IF
      *
               END-EVALUATE
           END-PERFORM
      *
           .
       4904-HKN-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    受付取消処理
      *****************************************************************
       491-SAKUJO-SEC              SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO   
               MOVE    "0016"              TO  SPA-ERRCD
               GO  TO  491-SAKUJO-EXT
           END-IF
      *
           IF  SPA-GMN-TUKETIME(SPA-GMN-SELNUM) =  SPACE
               MOVE    "0017"              TO  SPA-ERRCD
               GO  TO  491-SAKUJO-EXT
           END-IF
      *
      *    受付マスタ削除
           MOVE    SPA-GMN-SELNUM          TO  IDX
      *
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPID              TO  UKE-HOSPID
           MOVE    SPA-SYSYMD              TO  UKE-UKEYMD
           MOVE    SPA-GMN-TUKEID(IDX)     TO  UKE-UKEID
      *
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE         =   ZERO
               MOVE    UKETUKE-REC             TO  MCPDATA-REC
               MOVE    "DBDELETE"              TO  MCP-FUNC
               MOVE    "UKETUKE-KEY"           TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1005"              TO  SPA-ERRCD
                   GO  TO  491-SAKUJO-EXT
               END-IF
      *
           END-IF
      *
      *    予約マスタ更新
           IF      SPA-GMN-TYYKTIME(IDX)  NOT       =  SPACE
               INITIALIZE                      YYK-KEY
               MOVE    SPA-HOSPID              TO  YYK-HOSPID
               MOVE    SPA-GMN-TSRYNAIYOCD(IDX)
                                               TO  YYK-SRYNAIYO
               MOVE    "1"                     TO  YYK-DRCD
               MOVE    SPA-GMN-TDRCD(IDX)      TO  YYK-DRCD(2:4)
               MOVE    SPA-SYSYMD              TO  YYK-YYKYMD
               MOVE    SPA-GMN-TKYYKTIME(IDX)
                                               TO  YYK-KEYYYKTIME
               MOVE    SPA-GMN-TYYKID(IDX)     TO  YYK-YYKID
               PERFORM 900-YOYAKU-READ-SEC
               IF      FLG-YYK             =   ZERO
                   MOVE    SPACE               TO  YYK-RAIINZUMI
      *
                   MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
                   MOVE    YYK-REC             TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "YYK-KEY"           TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    "1004"              TO  SPA-ERRCD
                       GO  TO  491-SAKUJO-EXT
                   END-IF
               END-IF
           END-IF
      *
      *
           PERFORM 493-CLEAR-SEC  
           PERFORM 3101-UKETU-HEN-SEC
           .
       491-SAKUJO-EXT.
           EXIT.
      *
      ***************************************************************
      *    保険確認年月日入力済み処理
      ***************************************************************
       400-HKNCHK-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX
           EVALUATE    MCP-WIDGET
               WHEN    "BHK1"
                   MOVE    1              TO  IDX
               WHEN    "BHK2"
                   MOVE    2              TO  IDX
               WHEN    "BHK3"
                   MOVE    3              TO  IDX
               WHEN    "BHK4"
                   MOVE    4              TO  IDX
               WHEN    "BHK5"
                   MOVE    5              TO  IDX
           END-EVALUATE
      *
           COMPUTE IDY                 =   (SPA-GMN-PAGE   -   1  )
                                       *    5
                                       +   IDX
      *
           IF     (IDY                 =   ZERO )  OR
                  (SPA-GMN-TMIKAKUNAME (IDY)   =   SPACE)
               GO      TO      400-HKNCHK-SYORI-EXT
           END-IF
      *    労災保険の時、セットしない
           IF      SPA-NAI-HKNKBN  (IDY)   =   3
               CONTINUE
           ELSE
               IF     (SPA-SYSYMD          =   SPA-NAI-KAKUNINYMD(IDY))
                  AND (SPA-NAI-KAKUNINYMD(IDY) NOT =  SPACE)
                   MOVE    SPA-MAE-KAKUNINYMDW(IDY)
                                               TO  SPA-GMN-TMIKAKUYMD
                                                               (IDY)
                   MOVE    SPA-MAE-KAKUNINYMD (IDY)
                                               TO  SPA-NAI-KAKUNINYMD
                                                               (IDY)
               ELSE
                   MOVE    SPA-SYSYMD          TO  SPA-NAI-KAKUNINYMD
                                                               (IDY)
                   MOVE    SPA-SYSYMDWH        TO  SPA-GMN-TMIKAKUYMD
                                                               (IDY)
               END-IF
           END-IF
      *
           .
       400-HKNCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録へ
      *****************************************************************
       492-ORCGP01-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    患者登録の権限がない時、エラー
           IF      SPA-GSRAUTH-12      =   "1"
               CONTINUE
           ELSE
               MOVE    "0028"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO  TO  492-ORCGP01-EXT
           END-IF
      *
      *
           IF      SPA-GMN-SELNUM      >   ZERO
      *
      *    選択番号チェック
               IF    SPA-GMN-SELNUM        >   SPA-UKE-MAX
                   MOVE    "0015"              TO  SPA-ERRCD
                   GO                          TO  492-ORCGP01-EXT
               END-IF
      *
      *  受付チェック
               IF      SPA-GMN-TUKETIME(SPA-GMN-SELNUM)   =   SPACE  
                   MOVE    "0004"              TO  SPA-ERRCD
                   GO                          TO  492-ORCGP01-EXT
               END-IF
      *
      *  患者番号チェック
              IF      SPA-GMN-PTNUM        NOT =  SPACE
                  MOVE    SPA-HOSPID           TO  PTINF-HOSPID
                  MOVE    SPA-GMN-PTID         TO  PTINF-PTID
                  PERFORM 900-PTINF-READ-SEC
                  IF      FLG-PTINF                =   1
                      MOVE    "0002"               TO  SPA-ERRCD
                      GO  TO  492-ORCGP01-EXT
      *---- (01.00.04) LINE MOD START ---------------------------------
                  END-IF
               END-IF
      **** ELSE
      *        IF      SPA-GMN-PTID         =   ZERO               
      *            MOVE    "0005"              TO  SPA-ERRCD
      *            GO                          TO  492-ORCGP01-EXT
      *********END-IF
           END-IF    
      *---- (01.00.04) LINE MOD END -----------------------------------
      *
      *    共通ＳＰＡセット
           PERFORM 4921-SPA-SET-SEC
           IF      SPA-GMN-PTNUM(1:1)  =   "*"
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           END-IF
           IF      SPA-GMN-SELNUM      >   ZERO
      *        受付ＩＤ
               MOVE    SPA-GMN-TUKEID  (SPA-GMN-SELNUM)
                                           TO  SPA-P02-UKEID
      *        患者氏名
               MOVE    SPA-GMN-NAME        TO  SPA-NAME
           END-IF
           MOVE    SPA-GMN-SELNUM      TO   SPA-P02-U02
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "P02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       492-ORCGP01-EXT.
           EXIT.
      *
      *****************************************************************
      *    共通画面遷移処理
      *****************************************************************
       4921-SPA-SET-SEC              SECTION.
      *
           INITIALIZE                  SPA-KYOTUKEY
      *    診療日
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
      *
      *    患者引継ぎなし
           IF      SPA-GMN-PTNUM       =   SPACE
               GO      TO      4921-SPA-SET-EXT
           END-IF
      *
      *    患者番号、患者ＩＤ
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *    診療科
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKANAME   TO  SPA-SRYKAMEI
      *    患者氏名
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
      *    ドクター
           MOVE    "1"                 TO  SPA-DRCD (1:1) 
           MOVE    SPA-GMN-DRCD        TO  SPA-DRCD (2:)
      *    保険組合せ
           MOVE    SPA-GMN-COMBINUM    TO  SPA-HKNCOMBINUM
      *
           .
      *
       4921-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為へ
      *****************************************************************
       493-ORCGK02-SEC              SECTION.
      *
           IF     (SPA-GMN-SELNUM      >   ZERO  )  AND
                  (SPA-GMN-PTID        =   ZERO  )
               MOVE    "0022"              TO  SPA-ERRCD
               GO       TO      493-ORCGK02-EXT
           END-IF
      *
      *    対象保険があること
           IF      SPA-GMN-SELNUM      >   ZERO
               PERFORM 410114-KUMIHYOJI-SEC
               IF      SPA-COMBI-MAX       =   ZERO
                   MOVE    "0023"              TO  SPA-ERRCD
                   GO       TO      493-ORCGK02-EXT
               END-IF
           END-IF
      *
           PERFORM 4921-SPA-SET-SEC
           IF      SPA-GMN-SELNUM      >   ZERO
      *        受付ＩＤ
               MOVE    SPA-GMN-TUKEID  (SPA-GMN-SELNUM)
                                           TO  SPA-P02-UKEID
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "K02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       493-ORCGK02-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名登録へ
      *****************************************************************
       494-ORCGC02-SEC              SECTION.
      *
           IF     (SPA-GMN-SELNUM      >   ZERO  )  AND
                  (SPA-GMN-PTID        =   ZERO  )
               MOVE    "0022"              TO  SPA-ERRCD
               GO       TO      494-ORCGC02-EXT
           END-IF
           PERFORM 4921-SPA-SET-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "C02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "C02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       494-ORCGC02-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納登録へ
      *****************************************************************
       495-ORCGS02-SEC              SECTION.
      *
           IF     (SPA-GMN-SELNUM      >   ZERO  )  AND
                  (SPA-GMN-PTID        =   ZERO  )
               MOVE    "0022"              TO  SPA-ERRCD
               GO       TO      495-ORCGS02-EXT
           END-IF
           PERFORM 4921-SPA-SET-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "S02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "S02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       495-ORCGS02-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計照会へ
      *****************************************************************
       496-ORCGJ02-SEC              SECTION.
      *
           IF     (SPA-GMN-SELNUM      >   ZERO  )  AND
                  (SPA-GMN-PTID        =   ZERO  )
               MOVE    "0022"              TO  SPA-ERRCD
               GO       TO      496-ORCGJ02-EXT
           END-IF
      *
           PERFORM 4921-SPA-SET-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "J02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "J02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       496-ORCGJ02-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "U02    "           TO  MCP-WINDOW.
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
           MOVE    SPACE                        TO  U02
      *
           MOVE    SPA-GMN-SRYYMDH              TO  U02-SRYYMD
      *
           MOVE    SPA-GMN-SELNUM               TO  U02-SELNUM
           MOVE    SPA-GMN-PTNUM                TO  U02-PTNUM
           MOVE    SPA-GMN-NAME                 TO  U02-NAME
           MOVE    SPA-GMN-BIRTHDAY             TO  U02-BIRTHDAY
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST(IDX)  TO  U02-SRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  U02-SRYKA-COUNT
           MOVE    SPA-GMN-SRYKA-G     TO  U02-SRYKA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCDLST(IDX)    TO  U02-DRNAME-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  U02-DRNAME-COUNT
           MOVE    SPA-GMN-DRCD-G      TO  U02-DRNAME
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-SRYNAIYO-MAX
               MOVE    SPA-GMN-SRYNAILST(IDX)
                                           TO  U02-SRYNAIYO-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SRYNAIYO-MAX        TO  U02-SRYNAIYO-COUNT
           MOVE    SPA-GMN-SRYNAIYOCD-G           TO  U02-SRYNAIYO
      *
      *    カルテ
           MOVE    SPA-GMN-KARUTELST (1)   TO  U02-KARUTE-ITEM (1)
           MOVE    SPA-GMN-KARUTELST (2)   TO  U02-KARUTE-ITEM (2)
           MOVE    SPA-GMN-KARUTELST (3)   TO  U02-KARUTE-ITEM (3)
           MOVE    3                       TO  U02-KARUTE-COUNT
           IF      SPA-GMN-KARUTE-G        =   SPACE
               MOVE    SPA-GMN-KARUTELST (1)   TO  SPA-GMN-KARUTE-G
           END-IF
           MOVE    SPA-GMN-KARUTE-G        TO  U02-KARUTE
      *
      *    処方箋
           MOVE    SPA-GMN-SYOHOLST (1)    TO  U02-SYOHO-ITEM (1)
           MOVE    SPA-GMN-SYOHOLST (2)    TO  U02-SYOHO-ITEM (2)
           MOVE    2                       TO  U02-SYOHO-COUNT
           IF      SPA-GMN-SYOHO-G         =   SPACE
               MOVE    SPA-GMN-SYOHOLST (1)    TO  SPA-GMN-SYOHO-G
           END-IF
           MOVE    SPA-GMN-SYOHO-G         TO  U02-SYOHO
      *
           MOVE    SPA-GMN-COMBINUM    TO  U02-HKNCOMBI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-COMBI-MAX
               MOVE    SPA-GMN-HKNCOMBINUM      (IDX)
                                                TO  U02-HCNUM (IDX)
               MOVE    SPA-GMN-SYU-TANSEIDONAME (IDX)
                                                TO  U02-HCHKN (IDX)
               MOVE    SPA-GMN-KOH1-TANSEIDONAME(IDX)
                                                TO  U02-HCKOH1(IDX)
               MOVE    SPA-GMN-KOH2-TANSEIDONAME(IDX)
                                                TO  U02-HCKOH2(IDX)
               MOVE    SPA-GMN-KOH3-TANSEIDONAME(IDX)
                                                TO  U02-HCKOH3(IDX)
               MOVE    SPA-GMN-KOH4-TANSEIDONAME(IDX)
                                                TO  U02-HCKOH4(IDX)
      *
               IF      SPA-GMN-HKNCOMBINUM (IDX)
                                        =   SPA-GMN-COMBINUM
                   MOVE    "T"          TO  U02-HKNCOMBI-SELECT
                                                            (IDX)
               ELSE
                   MOVE    "F"          TO  U02-HKNCOMBI-SELECT
                                                            (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-COMBI-MAX        TO  U02-HKNCOMBI-COUNT
      *
           MOVE    ZERO                TO  U02-MIKAKUNINN-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       5
               COMPUTE IDY             =   (SPA-GMN-PAGE  -   1  )
                                       *   5
                                       +   IDX
               IF      IDY             >   SPA-MIKAKU-MAX
                   MOVE    5               TO  IDX
               ELSE
                   MOVE    IDX             TO  U02-MIKAKUNINN-COUNT
                   MOVE    SPA-GMN-TMIKAKUNAME (IDY)
                                           TO  U02-MLSEIDONAME (IDX)
                   MOVE    SPA-GMN-TMIKIGONUM  (IDY)
                                           TO  U02-MLKIGONUM   (IDX)
                   MOVE    SPA-GMN-TMIKAKUYMD (IDY)
                                           TO  U02-MLLASTYMD   (IDX)
      *            保険確認
                   IF      SPA-NAI-KAKUNINYMD (IDY)(1:6)
                                           =   SPA-SYSYMD (1:6)
                       EVALUATE    IDX
                           WHEN    1
                               MOVE    "済"        TO  U02-BHK1-LABEL
                               MOVE    "T"         TO  U02-BHK1-VALUE
                           WHEN    2
                               MOVE    "済"        TO  U02-BHK2-LABEL
                               MOVE    "T"         TO  U02-BHK2-VALUE
                           WHEN    3
                               MOVE    "済"        TO  U02-BHK3-LABEL
                               MOVE    "T"         TO  U02-BHK3-VALUE
                           WHEN    4
                               MOVE    "済"        TO  U02-BHK4-LABEL
                               MOVE    "T"         TO  U02-BHK4-VALUE
                           WHEN    5
                               MOVE    "済"        TO  U02-BHK5-LABEL
                               MOVE    "T"         TO  U02-BHK5-VALUE
                       END-EVALUATE
                   ELSE
                       EVALUATE    IDX
                           WHEN    1
                               MOVE    "未"        TO  U02-BHK1-LABEL
                               MOVE    "F"         TO  U02-BHK1-VALUE
                           WHEN    2
                               MOVE    "未"        TO  U02-BHK2-LABEL
                               MOVE    "F"         TO  U02-BHK2-VALUE
                           WHEN    3
                               MOVE    "未"        TO  U02-BHK3-LABEL
                               MOVE    "F"         TO  U02-BHK3-VALUE
                           WHEN    4
                               MOVE    "未"        TO  U02-BHK4-LABEL
                               MOVE    "F"         TO  U02-BHK4-VALUE
                           WHEN    5
                               MOVE    "未"        TO  U02-BHK5-LABEL
                               MOVE    "F"         TO  U02-BHK5-VALUE
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-UKE-MAX
               MOVE    IDX                   TO  U02-UKETUKE-COUNT
      *
               MOVE    SPA-GMN-TNUM  (IDX)   TO  WRK-NUM
               MOVE    WRK-NUM               TO  U02-ULNUM  (IDX)
               IF  SPA-GMN-TYYKTIME(IDX)  NOT   =  SPACE
                   MOVE    SPA-GMN-TYYKTIME(IDX)(1:2) 
                                                TO  WRK-THH
                   MOVE    SPA-GMN-TYYKTIME(IDX)(3:2) 
                                                TO  WRK-TMM
                   MOVE    WRK-HENTIME          TO  U02-ULYYKTIME(IDX)
               END-IF
               IF  SPA-GMN-TUKETIME(IDX)  NOT   =  SPACE
                   MOVE    SPA-GMN-TUKETIME(IDX)(1:2) 
                                          TO  WRK-THH
                   MOVE    SPA-GMN-TUKETIME(IDX)(3:2) 
                                          TO  WRK-TMM
                   MOVE    WRK-HENTIME    TO  U02-ULUKETUKETIME(IDX)
               END-IF
               MOVE    SPA-GMN-TPTNUM     (IDX) TO  U02-ULPTNUM   (IDX)
               MOVE    SPA-GMN-TNAME      (IDX) TO  U02-ULNAME    (IDX)
               MOVE    SPA-GMN-TSRYKA-G   (IDX) TO  U02-ULSRYKA   (IDX)
               MOVE    SPA-GMN-TDRCD-G    (IDX) TO  U02-ULDRNAME  (IDX)
               MOVE    SPA-GMN-TSRYNAIYOCD-G(IDX)
                                                TO  U02-ULSRYNAIYO(IDX)
               MOVE    SPA-GMN-TSRYNAIYOCD-G(IDX)
                                                TO  U02-ULSRYNAIYO(IDX)
               MOVE    SPA-GMN-THKNCOMBI-G (IDX)
                                                TO  U02-ULHKNCOMBI(IDX)
               MOVE    SPA-GMN-TRRKYMD  (IDX)   TO  U02-ULRRKYMD  (IDX)
      *
               IF      SPA-GMN-TNUM  (IDX)  =   SPA-NAI-SELNUM
                   MOVE    "T"              TO  U02-UKETUKE-SELECT
                                                            (IDX)
               ELSE
                   MOVE    "F"              TO  U02-UKETUKE-SELECT
                                                            (IDX)
               END-IF
            END-PERFORM
            MOVE    SPA-UKE-MAX        TO  U02-UKETUKE-COUNT
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
            .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  U02-PTNUM-STATE
           MOVE    WIDGET-NORMAL       TO  U02-NAME-STATE
      *
      *    選択番号が入力時は、患者番号入力不可
           IF     (SPA-NAI-SELNUM  NOT =   ZERO )  AND
                  (SPA-GMN-PTNUM   NOT =   SPACE)
               MOVE    WIDGET-INSENSITIVE      TO  U02-PTNUM-STATE
           END-IF
      *    患者氏名入力不可
           IF     (SPA-GMN-PTNUM   NOT =   SPACE)   AND
                  (SPA-GMN-NAME    NOT =   SPACE)
               MOVE    WIDGET-INSENSITIVE      TO  U02-NAME-STATE
           END-IF
      *
      *業務処理権限設定
      *    患者登録
           IF      SPA-GSRAUTH-12      =   "1"
               MOVE    WIDGET-NORMAL       TO  U02-B05-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  U02-B05-STATE
           END-IF
      *    診療行為
           IF      SPA-GSRAUTH-21      =   "1"
               MOVE    WIDGET-NORMAL       TO  U02-B06-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  U02-B06-STATE
           END-IF
      *    病名登録
           IF      SPA-GSRAUTH-22      =   "1"
               MOVE    WIDGET-NORMAL       TO  U02-B07-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  U02-B07-STATE
           END-IF
      *    収納登録
           IF      SPA-GSRAUTH-23      =   "1"
               MOVE    WIDGET-NORMAL       TO  U02-B08-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  U02-B08-STATE
           END-IF
      *    会計照会
           IF      SPA-GSRAUTH-24      =   "1"
               MOVE    WIDGET-NORMAL       TO  U02-B09S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  U02-B09S-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次へ
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM"    TO  MCP-WIDGET
               WHEN    02
                   MOVE    "PTNUM"      TO  MCP-WIDGET
               WHEN    03
                   MOVE    "NAME"       TO  MCP-WIDGET
               WHEN    04
                   MOVE    "SRYKA"      TO  MCP-WIDGET
               WHEN    05
                   MOVE    "DRNAME"     TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SRYNAIYO"   TO  MCP-WIDGET
               WHEN    07
                   MOVE    "KARUTE"     TO  MCP-WIDGET
               WHEN    08
                   MOVE    "SYOHO"      TO  MCP-WIDGET
               WHEN    09
                   MOVE    "HKNCOMBI"   TO  MCP-WIDGET
               WHEN    58
                   MOVE    "B05"        TO  MCP-WIDGET
               WHEN    62
                   MOVE    "B12"        TO  MCP-WIDGET
           END-EVALUATE
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "PTNUM"
                   IF      SPA-GMN-NAME        =   SPACE
                       MOVE    3               TO  SPA-GMN-CUR
                   ELSE
                       MOVE    4               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "NAME"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "SRYKA"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "DRNAME"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "SRYNAIYO"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "KARUTE"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "SYOHO"
      ********     IF     (SPA-COMBI-MAX       =   1   )  OR
      *                   (SPA-GMN-SYOHO       =   "0"  AND
      *                    SPA-GMN-KARUTE      =   "0"  )
      *                MOVE    62                  TO  SPA-GMN-CUR
      *            ELSE
      *                MOVE    9                   TO  SPA-GMN-CUR
      *******      END-IF
      *            マスタに追加したため
                   IF      SPA-COMBI-MAX       =   ZERO
                       MOVE    62                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    9                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "HKNCOMBI"
                   MOVE    62              TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "患者氏名を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "該当の患者番号が存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "患者氏名は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "受付されていません"
                                                TO  SPA-ERRMSG
      *----- (01.00.04) LINE ADD START --------------------------------
               WHEN    "0005"
                   MOVE    "未登録の患者です"   TO  SPA-ERRMSG
      *----- (01.00.04) LINE ADD END ----------------------------------
               WHEN    "0009"
                   MOVE    "登録できません"     TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "カルテ入力エラー"   TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "対象となる保険がありません"
                                                TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "処方箋入力エラー"   TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE
                   "カルテ、処方箋が選んであるので入力して下さい"
                                                TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "保険組合せ入力エラー"
                                                TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "選択番号入力エラー" TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "取消するデータを選んで下さい"
                                                TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "受付されていないデータです"
                                                TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "診療科がありません。"
                                                TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "ドクター入力エラー" TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "診療内容入力エラー" TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "新規患者です。患者登録後、遷移して下さい"
                                                TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "対象の保険組合せがありません。"
                                                TO  SPA-ERRMSG
                   MOVE    "診療行為は入力できません。"
                                                TO  SPA-ERRMSG(33:)
               WHEN    "0024"
                   MOVE    "カルテ印刷に必要な情報が入力されてません。"
                                                TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE
                   "処方せん印刷に必要な情報が入力されてません。"
                                                TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE
                   "診療科と診療内容は必ず選択して下さい。"
                                                TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE
                   "選択中です。受付一覧の更新はできません。"
                                                TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "患者登録の業務権限はありません。"
                                                TO  SPA-ERRMSG
                   MOVE    "患者登録はできません。"
                                                TO  SPA-ERRMSG(33:)
               WHEN    "2001"
                   MOVE    "前の未確認保険はありません。"
                                                TO  SPA-ERRMSG
               WHEN    "2002"
                   MOVE    "次の未確認保険はありません。"
                                                TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE
                   "受付マスタ（管理）の更新処理でエラーになりました"
                                                TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "受付マスタの追加処理でエラーになりました"
                                                TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "受付マスタの更新処理でエラーになりました"
                                                TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "予約マスタの更新処理でエラーになりました"
                                                TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "受付マスタの削除処理でエラーになりました"
                                                TO  SPA-ERRMSG
               WHEN    "1006"
                   MOVE    "既に受付ＩＤが登録されています。"
                                                TO  SPA-ERRMSG
                   MOVE    "再度、受付登録をして下さい。"
                                                TO  SPA-ERRMSG(33:)
               WHEN    "1007"
                   MOVE    "受付ＩＤが９９９９９を超えます。"
                                                TO  SPA-ERRMSG
                   MOVE    "これ以上受付はできません。"
                                                TO  SPA-ERRMSG(33:)
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
      *
           END-EVALUATE
      *
           MOVE    SPACE                TO  U2ERR
           MOVE    SPA-ERRCD            TO  U2ERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  U2ERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "U02"                TO  SPA-MOTOPG
           MOVE    "U2ERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "U2ERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
123900     .
124000 510-ERRSET-EXT.
124100     EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       500-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *    西暦和暦変更
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    ALL "9"             TO  WRK-WYMD
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-GENGO      TO  WRK-WYMD(1:1)
               MOVE    LNK-DAY1-YMD(3:6)   TO  WRK-WYMD(2:6)
           END-IF
      *
           .
       500-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       900-YOYAKU-READ-SEC         SECTION.
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "YYK-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "YYK-KEY"           TO  ORC-DBPATH
               PERFORM 910-YOYAKU-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
      *    カーソルクロース
           MOVE    "YYK-KEY"           TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-YOYAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       910-YOYAKU-READ-SEC         SECTION.
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
       910-YOYAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       900-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "UKETUKE-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "UKETUKE-KEY"       TO  ORC-DBPATH
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
                MOVE    1              TO  FLG-UKETUKE
           END-IF
      *    カーソルクロース
           MOVE    "UKETUKE-KEY"       TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-UKETUKE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスター読込処理
      *****************************************************************
       910-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-UKETUKE
               MOVE    MCPDATA-REC         TO  UKETUKE-REC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
      *
           .
       910-UKETUKE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
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
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTINF
               MOVE    MCPDATA-REC         TO  PTINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
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
      *    システム管理マスタ読込（ＫＥＹ４）
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY4"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY4"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "SYSKANRI-KEY4"      TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ（主キー）読込処理
      *****************************************************************
       900-COMB-INV-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               PERFORM 900-COMB-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-COMB-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-COMB-READ-SEC         SECTION.
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
       900-COMB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスター読込
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
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
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費マスター読込
      *****************************************************************
       900-PTKOHINF-READ-SEC         SECTION.
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
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号（主キー）読込処理
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           PERFORM 990-DBCLOSE-SEC
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"                   TO  MCP-FUNC.
           CALL   "ORCMCPSUB"                    USING
                                                 MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC.
      *
       900-PUT-WINDOW-EXT.
           EXIT.
