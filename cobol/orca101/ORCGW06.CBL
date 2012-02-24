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
       PROGRAM-ID.             ORCGW06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 職員情報（Ｗ０６）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-藤原     01/09/11  削除処理の修正 
      * 01.01.01     MCC-藤原     01/09/21  JOINの修正      
      * 01.01.02     MCC-森脇     02/05/14  麻薬ライセンスの追加
      *                                     画面の変更      
      * 01.01.03     MCC-多々納   02/06/13  オルカマスターの
      *                                     削除を非活性にする
      * 01.01.04     MCC-太田     02/06/20  人名辞書マスタ追加
      * 01.01.05     NACL-森脇    02/07/30  薬剤情報マスタ追加
      * 01.01.06     NACL-藤原    02/09/01  看護婦の名称変更
      * 01.01.07     NACL-森脇    02/11/08  入院対応
      * 01.01.08     NACL-多々納  02/11/13  一括入力 
      * 02.00.01     NACL-森脇    04/02/06  プログラム管理の追加
      * 02.00.02     NACL-森脇    04/08/04  住所マスタの追加
      * 02.00.03     NACL-森脇    05/04/25  ヘルプマスタの追加
      * 03.00.01     NACL-多々納  05/08/29  画面変更
      *                                     診療行為初期表示追加
      * 03.00.02     NACL-多々納  05/12/01  請求書発行フラグ等追加
      *                                     MONFUNC 対応
      * 03.00.03     NACL-多々納  06/04/11  診療費明細書発行追加
      * 03.04.00     NACL-多々納  06/11/13  検査正式名称表示区分の追加
      * 03.05.00     NACL-多々納  07/05/02  グループ診療対応
      * 03.05.01     NACL-藤原    07/09/14  クライアント保存可否フラグ追加
      * 04.00.00     NACL-多々納  07/10/05  SYSUSER登録対応
      * 04.04.00     NACL-太田    08/01/27  他院設定追加
      * 04.04.01     NACL-森脇    09/03/13  外部まとめ追加
      * 04.05.00     NACL-多々納  10/03/XX  平成２２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W06.
           03  SPA-W06-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-PAGENO      PIC 9(02).
               05  SPA-GMN-SYORI       PIC 9(01).
      *
               05  SPA-GMN-AREA.
      *            職員区分
                   07  SPA-GMN-SYOKUKBN-G.
                       09  SPA-GMN-SYOKUKBN       PIC X(01).
                       09  SPA-GMN-SYOKUKBNF      PIC X(01).
                       09  SPA-GMN-SYOKUKBNMEI    PIC X(18).
      *            職員コード
                   07  SPA-GMN-SYOKUCOD-G.
                       09  SPA-GMN-SYOKUCOD       PIC X(04).
                       09  SPA-GMN-SYOKUCODF      PIC X(01).
                       09  SPA-GMN-SYOKUCOD-MEI   PIC X(30).
                   07  SPA-GMN-SYOKUCOD-LIST-G.
                       09  SPA-GMN-SYOKUCOD-LIST  OCCURS   200.
                           11  SPA-GMN-TSYOKUCOD      PIC X(04).
                           11  SPA-GMN-TSYOKUCODF     PIC X(01).
                           11  SPA-GMN-TSYOKUCOD-MEI  PIC X(30).
                       09  SPA-SYOKUCOD-MAX       PIC 9(04).
      *
               05  SPA-GMN-AREA2.
                   07  SPA-GMN-STYUKYMD       PIC X(10).
                   07  SPA-GMN-EDYUKYMD       PIC X(10).
      *
                   07  SPA-GMN-KIKANTBL.
                       09  SPA-GMN-TBLO          OCCURS  10.
                           11  SPA-GMN-TBANGO     PIC 9(02).
                           11  SPA-GMN-TSTYUKYMD  PIC X(09).
                           11  SPA-GMN-TEDYUKYMD  PIC X(09).
      *
                           11  SPA-NAI-TSTYUKYMD  PIC X(08).
                           11  SPA-NAI-TEDYUKYMD  PIC X(08).
                       09  SPA-GMN-TBL-MAX        PIC 9(04).
                   07  SPA-GMN-SELNUM             PIC 9(02).
      *
                   07  SPA-GMN-AREA3.
                       09  SPA-GMN-USERID         PIC X(16).
                       09  SPA-GMN-KANANAME       PIC X(80).
                       09  SPA-GMN-NAME           PIC X(80).
      *
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-MAYAKULICENSE  PIC X(40).
                       09  SPA-GMN-ISEKINUM       PIC X(08).
                       09  SPA-GMN-HKNDRNUMS      PIC X(08).
                       09  SPA-GMN-HKNDRNUMK      PIC X(12).
      *                業務権限
                       09  SPA-GMN-CHK-G.
                           11  SPA-GMN-CHK        PIC X     OCCURS  36.
      *            専門科
                   07  SPA-GMN-SEMONKACD-G    OCCURS  5.
                       09  SPA-GMN-SEMONKACD       PIC X(02).
                       09  SPA-GMN-SEMONKACD-FL    PIC X(01).
                       09  SPA-GMN-SEMONKACD-MEI   PIC X(14).
      *            管理者権限
                   07  SPA-GMN-KANRIAUTHFLG-G. 
                       09  SPA-GMN-KANRIAUTH-MEI    PIC X(12).
                   07  SPA-NAI-KANRIAUTHFLG         PIC X(01).
      *            診療行為初期表示
                   07  SPA-GMN-ORCA21FLG-G. 
                       09  SPA-GMN-ORCA21FLG       PIC X(01).
                       09  SPA-GMN-ORCA21FLGF1     PIC X(01).
                       09  SPA-GMN-ORCA21FLG-MEI   PIC X(10).
      *
      *        診療科テーブル
               05  SPA-GMN-SEMONKACD-AREA.
                   07  SPA-GMN-SEMONKACD-TBL  OCCURS 99.
                       09  SPA-GMN-SRYKA      PIC X(02).
                       09  SPA-GMN-FL2        PIC X(01).
                       09  SPA-GMN-SRYKAMEI   PIC X(14).
                   07  SPA-SRYKA-MAX          PIC 9(04).
                   07  SPA-SRYKA-YMD          PIC X(08).
      *
               05  SPA-GMN-INIT-G.
      *            職員区分名称
                   07  SPA-GMN-SYOKUKBN-LIST      PIC X(20)
                                                  OCCURS   10.
                   07  SPA-SYOKUKBN-MAX       PIC 9(04).
      *            管理者権限
                   07  SPA-GMN-KANRIAUTH-LSTG.
                       09  SPA-GMN-KANRIAUTH-LST  PIC X(12)
                                                  OCCURS  2. 
                   07  SPA-KANRIAUTHFLG-MAX       PIC 9(04).  
      *            診療行為初期表示
                   07  SPA-GMN-ORCA21FLG-LSTG.
                       09  SPA-GMN-ORCA21FLG-LST   PIC X(12)
                                                   OCCURS  2. 
                   07  SPA-ORCA21FLG-MAX       PIC 9(04).  
      *            請求書初期表示
                   07  SPA-GMN-SKYPRTFLG-LSTG.
                       09  SPA-GMN-SKYPRTFLG-LST   PIC X(32)
                                                   OCCURS  9.
                   07  SPA-SKYPRTFLG-MAX       PIC 9(04).  
      *            院外処方せん
                   07  SPA-GMN-SYOHOPRTFLG-LSTG.
                       09  SPA-GMN-SYOHOPRTFLG-LST   PIC X(30)
                                                   OCCURS  5. 
                   07  SPA-SYOHOPRTFLG-MAX      PIC 9(04).  
      *            薬剤情報発行
                   07  SPA-GMN-YAKUJYOFLG-LSTG.
                       09  SPA-GMN-YAKUJYOFLG-LST   PIC X(30)
                                                   OCCURS  5. 
                   07  SPA-YAKUJYOFLG-MAX      PIC 9(04).  
      *            診療費明細書
                   07  SPA-GMN-MEIPRTFLG-LSTG.
                       09  SPA-GMN-MEIPRTFLG-LST   PIC X(32)
                                                   OCCURS  9. 
                   07  SPA-MEIPRTFLG-MAX      PIC 9(04).  
      *            検査正式名称表示
                   07  SPA-GMN-FORMALFLG-LSTG.
                       09  SPA-GMN-FORMALFLG-LST   PIC X(40)
                                                   OCCURS  5. 
                   07  SPA-FORMALFLG-MAX      PIC 9(04).  
      *            クライアント保存可否
                   07  SPA-GMN-CLIENT-SAVE-LSTG.
                       09  SPA-GMN-CLIENT-SAVE-LST PIC X(40)
                                                   OCCURS  2. 
                   07  SPA-CLIENT-SAVE-MAX    PIC 9(04).  
      *            ユーザーログイン権限
                   07  SPA-GMN-USER-KANRI-LSTG.
                       09  SPA-GMN-USER-KANRI-LST  PIC X(40)
                                                   OCCURS  5. 
                   07  SPA-USER-KANRI-MAX      PIC 9(04).  
      *            基本情報の発行区分
                   07  SPA-1001-SKYPRTFLG      PIC  X(01).
                   07  SPA-1001-SYOHOPRTFLG    PIC  X(01).
                   07  SPA-1001-YAKUJYOPRTFLG  PIC  X(01).
                   07  SPA-1001-MEIPRTFLG      PIC  X(01).
                   07  SPA-1038-FORMALFLG      PIC  X(01).
      *
               05  SPA-PAGE2-G.
      *            請求書
                   07  SPA-GMN-SKYPRTFLG-G. 
                       09  SPA-GMN-SKYPRTFLG       PIC X(01).
                       09  SPA-GMN-SKYPRTFLG-F1    PIC X(01).
                       09  SPA-GMN-SKYPRTFLG-MEI   PIC X(30).
      *            院外処方せん
                   07  SPA-GMN-SYOHOPRTFLG-G. 
                       09  SPA-GMN-SYOHOPRTFLG       PIC X(01).
                       09  SPA-GMN-SYOHOPRTFLG-F1    PIC X(01).
                       09  SPA-GMN-SYOHOPRTFLG-MEI   PIC X(28).
      *            薬剤情報発行
                   07  SPA-GMN-YAKUJYOFLG-G. 
                       09  SPA-GMN-YAKUJYOFLG       PIC X(01).
                       09  SPA-GMN-YAKUJYOFLG-F1    PIC X(01).
                       09  SPA-GMN-YAKUJYOFLG-MEI   PIC X(28).
      *            診療費明細書
                   07  SPA-GMN-MEIPRTFLG-G. 
                       09  SPA-GMN-MEIPRTFLG        PIC X(01).
                       09  SPA-GMN-MEIPRTFLG-F1     PIC X(01).
                       09  SPA-GMN-MEIPRTFLG-MEI    PIC X(30).
      *            検査正式名称表示
                   07  SPA-GMN-FORMALFLG-G. 
                       09  SPA-GMN-FORMALFLG        PIC X(01).
                       09  SPA-GMN-FORMALFLG-F1     PIC X(01).
                       09  SPA-GMN-FORMALFLG-MEI    PIC X(24).
      *            クライアント保存可否
                   07  SPA-GMN-CLIENT-SAVE-G. 
                       09  SPA-GMN-CLIENT-SAVE      PIC X(01).
                       09  SPA-GMN-CLIENT-SAVE-F1   PIC X(01).
                       09  SPA-GMN-CLIENT-SAVE-MEI  PIC X(24).
      *            ユーザーログイン権限
                   07  SPA-GMN-USER-KANRI-G. 
                       09  SPA-GMN-USER-KANRI      PIC X(01).
                       09  SPA-GMN-USER-KANRI-F1   PIC X(01).
                       09  SPA-GMN-USER-KANRI-MEI  PIC X(24).
      *
               05  SPA-PAGE3-G.
      *            他院医療機関情報
                   07  SPA-GMN-GRPHOSP-G.
                       09  SPA-GMN-GRPHOSP-MAX      PIC 9(02).
                       09  SPA-GMN-GRPHOSP-OCC      OCCURS 9.
                           11  SPA-GMN-GRPUSERID    PIC X(16).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD       PIC X(08).
                   07  SPA-NAI-EDYUKYMD       PIC X(08).
      *
                   07  SPA-MAE-STYUKYMD       PIC X(08).
                   07  SPA-MAE-EDYUKYMD       PIC X(08).
      *
                   07  SPA-NAI-KEIFLG         PIC 9(01).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-SYSUSER         PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-USERID-UPD      PIC 9(01).
           03  FLG-USERMATCH       PIC 9(01).
      *
      *    カウント領域
      *
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-GRP             PIC 9(04).
      *
           03  IDX2                PIC 9(02).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-PATH            PIC X(64).
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    職員区分
           03  WRK-MAE-SYOKUKBN-G.
               05  WRK-MAE-SYOKUKBN       PIC X(01).
               05  WRK-MAE-SYOKUKBNF      PIC X(01).
               05  WRK-MAE-SYOKUKBNMEI    PIC X(18).
      *
           03  WRK-SELNUM          PIC 9(03).
      *
           03  WRK-CD              PIC X(02).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-MAE-USERID      PIC X(16).
           03  WRK-GRPUSERID-WIDGET.
               05  WRK-GRPUSERIDFL         PIC X(09).
               05  WRK-GRPUSERIDNUM        PIC 9(02).
           03  WRK-ERRCUR          PIC 9(03).
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
       01  CONST-AREA.
           03  CONST-GRPHOSP-MAX   PIC 9(02)   VALUE   9.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    診療科情報
           COPY  "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    基本情報
           COPY  "CPSK1001.INC".
      *    基本情報
           COPY  "CPSK1038.INC".
      *
      *    システムユーザー
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    カナチェック
           COPY  "CPORCSKANACHK.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK2.INC".
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      **** COPY    "ORCA-DBPATH".
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
           COPY    "W33.INC".
           COPY    "W34.INC".
           COPY    "W35.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W62.INC".
           COPY    "W63.INC".
           COPY    "W64.INC".
           COPY    "W65.INC".
           COPY    "W66.INC".
           COPY    "W67.INC".
           COPY    "W96.INC".
           COPY    "W98.INC".
           COPY    "W981.INC".
           COPY    "W97.INC".
           COPY    "W99.INC".
           COPY    "WERR.INC".
           COPY    "WID1.INC".
           COPY    "WID2.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W06
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W06         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
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
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 510-GSRAUTH-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      * 
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W06"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
               WHEN    "1002"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   ELSE
                       MOVE    2           TO  SPA-GMN-CUR
                   END-IF
               WHEN    "1003"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   ELSE
                       MOVE    2           TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W06
      *    職員区分
           MOVE    "1 医師"            TO  SPA-GMN-SYOKUKBN-LIST (1)
           MOVE    "2 看護師（婦）"    TO  SPA-GMN-SYOKUKBN-LIST (2)
           MOVE    "3 技師"            TO  SPA-GMN-SYOKUKBN-LIST (3)
           MOVE    "4 事務職"          TO  SPA-GMN-SYOKUKBN-LIST (4)
           MOVE    "5 管理職"          TO  SPA-GMN-SYOKUKBN-LIST (5)
           MOVE    "0 マスター"        TO  SPA-GMN-SYOKUKBN-LIST (6)
           MOVE    6                   TO  SPA-SYOKUKBN-MAX
      *    医療機関番号が２以上は 0 マスター なし
           IF      SPA-HOSPNUM         >=  02
               MOVE    5                   TO  SPA-SYOKUKBN-MAX
           END-IF
      *    管理者権限
           MOVE    "管理者である"      TO  SPA-GMN-KANRIAUTH-LST(1)
           MOVE    "管理者でない"      TO  SPA-GMN-KANRIAUTH-LST(2) 
           MOVE    2                   TO  SPA-KANRIAUTHFLG-MAX
      *    診療行為初期表示
           MOVE    "1 入院画面"        TO  SPA-GMN-ORCA21FLG-LST (1)
           MOVE    "2 外来画面"        TO  SPA-GMN-ORCA21FLG-LST (2)
           MOVE    2                   TO  SPA-ORCA21FLG-MAX
      *    診療科をシステム日付で検索
           MOVE    SPA-SYSYMD          TO  SPA-SRYKA-YMD
           PERFORM 3201-SRYKA-HEN-SEC
      *
      *    請求書発行
           MOVE    "0 発行しない"      TO  SPA-GMN-SKYPRTFLG-LST (1)
           MOVE    "1 発行する"        TO  SPA-GMN-SKYPRTFLG-LST (2)
           MOVE    "2 発行する（請求あり）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (3)
           MOVE    "3 発行する（訂正時なし）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (4)
           MOVE    4                   TO  SPA-SKYPRTFLG-MAX
      *    院外処方せん発行フラグ
           MOVE    "0 発行しない"      TO  SPA-GMN-SYOHOPRTFLG-LST (1)
           MOVE    "1 発行する"        TO  SPA-GMN-SYOHOPRTFLG-LST (2)
           MOVE    "2 院内処方発行"    TO  SPA-GMN-SYOHOPRTFLG-LST (3)
           MOVE    "3 発行する（訂正時なし）"
                                       TO  SPA-GMN-SYOHOPRTFLG-LST (4)
           MOVE    "4 院内処方発行（訂正時なし）" 
                                       TO  SPA-GMN-SYOHOPRTFLG-LST (5)
           MOVE    5                   TO  SPA-SYOHOPRTFLG-MAX
      *    薬剤情報発行フラグ
           MOVE    "0 発行しない"      TO  SPA-GMN-YAKUJYOFLG-LST (1)
           MOVE    "1 発行する"        TO  SPA-GMN-YAKUJYOFLG-LST (2)
           MOVE    "2 発行する（訂正時なし）" 
                                       TO  SPA-GMN-YAKUJYOFLG-LST (3)
           MOVE    3                   TO  SPA-YAKUJYOFLG-MAX
      *    診療費明細書フラグ
           MOVE    "0 発行しない"      TO  SPA-GMN-MEIPRTFLG-LST (1)
           MOVE    "1 発行する"        TO  SPA-GMN-MEIPRTFLG-LST (2)
           MOVE    "2 発行する（請求あり）" 
                                       TO  SPA-GMN-MEIPRTFLG-LST (3)
           MOVE    "3 発行する（訂正時なし）" 
                                       TO  SPA-GMN-MEIPRTFLG-LST (4)
           MOVE    4                   TO  SPA-MEIPRTFLG-MAX
      *    検査正式名称表示
           MOVE    "0 使用しない"      TO  SPA-GMN-FORMALFLG-LST(1)
           MOVE    "1 使用する"        TO  SPA-GMN-FORMALFLG-LST(2)
           MOVE    2                   TO  SPA-FORMALFLG-MAX
      *    クライアント保存可否
           MOVE    "0 保存不可"        TO  SPA-GMN-CLIENT-SAVE-LST(1)
           MOVE    "1 保存可"          TO  SPA-GMN-CLIENT-SAVE-LST(2)
           MOVE    2                   TO  SPA-CLIENT-SAVE-MAX
      *    ユーザーログイン権限
           MOVE    "0 表示しない"      TO  SPA-GMN-USER-KANRI-LST(1)
           MOVE    "1 閲覧のみ可(自院のみ)"
                                       TO  SPA-GMN-USER-KANRI-LST(2)
           MOVE    "2 閲覧のみ可(全て)"
                                       TO  SPA-GMN-USER-KANRI-LST(3)
           MOVE    "3 更新も可(自院のみ)"
                                       TO  SPA-GMN-USER-KANRI-LST(4)
           MOVE    "4 更新も可(全て)"  TO  SPA-GMN-USER-KANRI-LST(5)
           MOVE    5                   TO  SPA-USER-KANRI-MAX
      *
      *    基本情報の発行フラグ
           PERFORM 3202-KIHON-HEN-SEC
      *
      *    職員情報初期表示
      *    １件目の職員情報読み込み
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL    FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) NOT =   "0"
                   MOVE    1               TO  FLG-SYSKANRI
               ELSE
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key6"              TO  MCP-PATHNAME
                   PERFORM 990-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOKUKBN-MAX
               IF      SYS-1010-KBNCD(1:1) =   SPA-GMN-SYOKUKBN-LIST
                                                     (IDX)(1:1)
                   MOVE    SPA-GMN-SYOKUKBN-LIST (IDX)
                                           TO  SPA-GMN-SYOKUKBN-G
                   MOVE    SPA-SYOKUKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      SPA-GMN-SYOKUKBN-G  =   SPACE
               MOVE    SPA-GMN-SYOKUKBN-LIST (1)
                                           TO  SPA-GMN-SYOKUKBN-G
           END-IF
      *
           PERFORM 320-SYOKUCOD-HENSYU-SEC
           IF      SPA-SYOKUCOD-MAX    >   ZERO
               MOVE    SPA-GMN-SYOKUCOD-LIST(1)
                                           TO  SPA-GMN-SYOKUCOD-G
           ELSE
               MOVE    SPACE               TO  SPA-GMN-SYOKUCOD-G
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    1                   TO  SPA-GMN-PAGENO
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報初期表示編集処理
      *****************************************************************
       3101-INIT-GMN-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SYORI
           INITIALIZE                      SPA-GMN-AREA2
           INITIALIZE                      SPA-NAIB-AREA
           INITIALIZE                      SPA-PAGE2-G
           INITIALIZE                      SPA-PAGE3-G
      *
           PERFORM 320-SYOKUCOD-HENSYU-SEC
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SYOKUCOD-MAX)
               IF      SPA-GMN-SYOKUCOD    =   SPA-GMN-TSYOKUCOD
                                                         (IDX)
                   MOVE    SPA-GMN-SYOKUCOD-LIST (IDX)
                                               TO  SPA-GMN-SYOKUCOD-G
                   MOVE    SPA-SYOKUCOD-MAX    TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               IF      SPA-SYOKUCOD-MAX    >   ZERO
                   MOVE    SPA-GMN-SYOKUCOD-LIST(1)
                                               TO  SPA-GMN-SYOKUCOD-G
               ELSE
                   MOVE    SPACE               TO  SPA-GMN-SYOKUCOD-G
               END-IF
           END-IF
           MOVE    2                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-GMN-PAGENO
           . 
       3101-INIT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員区分毎の職員編集処理 
      *****************************************************************
       320-SYOKUCOD-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SYOKUCOD-LIST-G
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    "%"                 TO  SYS-1010-KBNCD (2:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM  VARYING    IDX     FROM    1   BY  1
               UNTIL          (IDX     >     100   )  OR
                              (FLG-SYSKANRI    =   1 )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SYS-1010-KBNCD(2:4) TO  SPA-GMN-TSYOKUCOD (IDX)
               MOVE    SYS-1010-NAME       TO  SPA-GMN-TSYOKUCOD-MEI
                                                               (IDX)
               MOVE    IDX                 TO  SPA-SYOKUCOD-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (SPA-SYOKUCOD-MAX    =   100 )  AND
                  (FLG-SYSKANRI        =   ZERO)
      *        １００件以上あり
               PERFORM 3202-SYOKUCOD-HENSYU2-SEC
           END-IF
      *
           . 
       320-SYOKUCOD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員区分毎の職員編集処理２
      *****************************************************************
       3202-SYOKUCOD-HENSYU2-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SYOKUCOD-LIST-G
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    "%"                 TO  SYS-1010-KBNCD (2:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
      *
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key21"             TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL       (IDX             >= 100 )  OR
                               (FLG-SYSKANRI    =   1  )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF     (SYS-1010-STYUKYMD   <=  SPA-SYSYMD  ) AND
                      (SYS-1010-EDYUKYMD   >=  SPA-SYSYMD  )
                   ADD     1                   TO  IDX
                   MOVE    SYS-1010-KBNCD(2:4) TO  SPA-GMN-TSYOKUCOD
                                                              (IDX)
                   MOVE    SYS-1010-NAME       TO  SPA-GMN-TSYOKUCOD-MEI
                                                               (IDX)
                   MOVE    IDX                 TO  SPA-SYOKUCOD-MAX
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key21"             TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           . 
       3202-SYOKUCOD-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ専門科コード初期編集処理 
      *****************************************************************
       3201-SRYKA-HEN-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SEMONKACD-AREA
      *    診療科コード
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM  VARYING    IDX     FROM    1   BY  1
               UNTIL          (IDX     >   99  )  OR
                              (FLG-SYSKANRI  =   1 )
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD  TO  SPA-GMN-SRYKA (IDX)
               MOVE    SYS-1005-SRYKANAME 
                                       TO  SPA-GMN-SRYKAMEI (IDX)
               MOVE  IDX               TO  SPA-SRYKA-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           . 
       3201-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報の発行フラグ編集処理 
      *****************************************************************
       3202-KIHON-HEN-SEC              SECTION.
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD 
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               INITIALIZE                      SYS-1001-REC
           END-IF
      *
           MOVE    SYS-1001-SKYPRTFLG      TO  SPA-1001-SKYPRTFLG
           MOVE    SYS-1001-SYOHOPRTFLG    TO  SPA-1001-SYOHOPRTFLG
           MOVE    SYS-1001-YAKUJYOPRTFLG  TO  SPA-1001-YAKUJYOPRTFLG
           MOVE    SYS-1001-MEIPRTFLG      TO  SPA-1001-MEIPRTFLG
      *
      *    検査正式名称表示
           INITIALIZE                      SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1038-STYUKYMD 
           MOVE    SPA-SYSYMD          TO  SYS-1038-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1038-REC
           ELSE
               INITIALIZE                      SYS-1038-REC
           END-IF
      *
           MOVE    SYS-1038-FORMALFLG      TO  SPA-1038-FORMALFLG 
           . 
       3202-KIHON-HEN-EXT.
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
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"      ALSO    "B02"
                   PERFORM  402-CLERA-SEC
      *    削除
               WHEN    "CLICKED"      ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    登録
               WHEN    "CLICKED"      ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    タグ切替
               WHEN    "CLICKED"      ALSO    "B08"
                   PERFORM 480-BETUGAMEN-SEC
      *    業務権限
               WHEN    "CLICKED"      ALSO    "CHK01"
               WHEN    "CLICKED"      ALSO    "CHK02"
               WHEN    "CLICKED"      ALSO    "CHK03"
               WHEN    "CLICKED"      ALSO    "CHK04"
               WHEN    "CLICKED"      ALSO    "CHK05"
               WHEN    "CLICKED"      ALSO    "CHK06"
               WHEN    "CLICKED"      ALSO    "CHK07"
               WHEN    "CLICKED"      ALSO    "CHK08"
               WHEN    "CLICKED"      ALSO    "CHK09"
               WHEN    "CLICKED"      ALSO    "CHK10"
               WHEN    "CLICKED"      ALSO    "CHK11"
               WHEN    "CLICKED"      ALSO    "CHK12"
               WHEN    "CLICKED"      ALSO    "CHK13"
               WHEN    "CLICKED"      ALSO    "CHK14"
               WHEN    "CLICKED"      ALSO    "CHK15"
               WHEN    "CLICKED"      ALSO    "CHK16"
               WHEN    "CLICKED"      ALSO    "CHK17"
               WHEN    "CLICKED"      ALSO    "CHK18"
               WHEN    "CLICKED"      ALSO    "CHK19"
               WHEN    "CLICKED"      ALSO    "CHK20"
               WHEN    "CLICKED"      ALSO    "CHK21"
               WHEN    "CLICKED"      ALSO    "CHK22"
               WHEN    "CLICKED"      ALSO    "CHK23"
               WHEN    "CLICKED"      ALSO    "CHK24"
               WHEN    "CLICKED"      ALSO    "CHK25"
      *----(01.01.05)----START------------
               WHEN    "CLICKED"      ALSO    "CHK26"
      *----(01.01.05)----END--------------
      *----(01.01.07)----START------------
               WHEN    "CLICKED"      ALSO    "CHK27"
               WHEN    "CLICKED"      ALSO    "CHK28"
               WHEN    "CLICKED"      ALSO    "CHK29"
               WHEN    "CLICKED"      ALSO    "CHK30"
               WHEN    "CLICKED"      ALSO    "CHK31"
               WHEN    "CLICKED"      ALSO    "CHK32"
      *----(01.01.07)----END--------------
      *----(02.00.01)--ADD-START---
               WHEN    "CLICKED"      ALSO    "CHK33"
      *----(02.00.01)--ADD-END-----
      *----(02.00.02)--ADD-START---
               WHEN    "CLICKED"      ALSO    "CHK34"
      *----(02.00.02)--ADD-END-----
      *----(02.00.03)--ADD-START---
               WHEN    "CLICKED"      ALSO    "CHK35"
      *----(02.00.03)--ADD-END-----
               WHEN    "CLICKED"      ALSO    "CHK36"
                   PERFORM 4119-GSRAUTH-CHK-SEC
      *    タグ切替え
           WHEN    "CLICKED"       ALSO    "TGN1"
           WHEN    "CLICKED"       ALSO    "TGN2"
           WHEN    "CLICKED"       ALSO    "TGN3"
               PERFORM 460-TAGUCHG-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        職員区分入力
               WHEN    "ACTIVATE"      ALSO    "SYOKUKBN"
                   PERFORM 2001-SYOKUKBN-SYORI-SEC
      *        職員番号入力
               WHEN    "ACTIVATE"      ALSO    "SYOKUCOD"
                   PERFORM 2002-SYOKUCOD-SYORI-SEC
      *        番号選択
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 2003-SELNUM-CHK-SEC
      *        明細選択
               WHEN    ANY             ALSO    "KIKAN_LIST"
                   PERFORM 2004-KIKANLIST-SEC
      *
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員区分入力処理
      *****************************************************************
       2001-SYOKUKBN-SYORI-SEC         SECTION.
      *
           MOVE    SPA-GMN-SYOKUKBN-G  TO  WRK-MAE-SYOKUKBN-G
      *
           MOVE    W06-SYOKUKBN        TO  SPA-GMN-SYOKUKBN-G
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOKUKBN-MAX
               IF      SPA-GMN-SYOKUKBN    =   SPA-GMN-SYOKUKBN-LIST
                                                     (IDX)(1:1)
                   MOVE    SPA-GMN-SYOKUKBN-LIST (IDX)
                                           TO  SPA-GMN-SYOKUKBN-G
                   MOVE    SPA-SYOKUKBN-MAX    TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               IF      SPA-GMN-SYOKUKBN    NOT =   WRK-MAE-SYOKUKBN
                   INITIALIZE                      SPA-GMN-AREA2
                   INITIALIZE                      SPA-GMN-KOUMOKU
                   INITIALIZE                      SPA-PAGE2-G
                   INITIALIZE                      SPA-PAGE3-G
      *            職員編集
                   PERFORM 320-SYOKUCOD-HENSYU-SEC
                   IF      SPA-SYOKUCOD-MAX    >   ZERO
                        MOVE    SPA-GMN-SYOKUCOD-LIST(1)
                                           TO  SPA-GMN-SYOKUCOD-G
                   ELSE
                       MOVE     SPACE      TO  SPA-GMN-SYOKUCOD-G
                   END-IF
               END-IF
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGENO
      *
           .
       2001-SYOKUKBN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員コード入力処理
      *****************************************************************
       2002-SYOKUCOD-SYORI-SEC         SECTION.
      *
           MOVE    W06-SYOKUCOD        TO  SPA-GMN-SYOKUCOD-G
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-SYOKUCOD(1:4)
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-GMN-SYOKUCOD
           END-IF
      *    9999 はエラー
           IF      SPA-GMN-SYOKUCOD    =   "9999"
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SYOKUCOD-MAX) OR
                              (SPA-ERRCD   NOT =   SPACE   )
               IF      SPA-GMN-SYOKUCOD    =   SPA-GMN-TSYOKUCOD
                                                         (IDX)
                   MOVE    SPA-GMN-SYOKUCOD-LIST (IDX)
                                               TO  SPA-GMN-SYOKUCOD-G
                   MOVE    SPA-SYOKUCOD-MAX    TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF     SPA-ERRCD            =   SPACE
      *        職員編集
               PERFORM 20021-SYOKUCOD-CHK-SEC
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGENO
      *
           .
       2002-SYOKUCOD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    職員コードチェック処理
      *****************************************************************
       20021-SYOKUCOD-CHK-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-AREA2
           INITIALIZE                      SPA-GMN-KOUMOKU
           INITIALIZE                      SPA-PAGE2-G
           INITIALIZE                      SPA-PAGE3-G
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1010-KBNCD (2:4)
           MOVE    "%"                 TO  SYS-1010-KBNCD (6:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM  VARYING    IDX     FROM    1   BY  1
               UNTIL          (IDX     >     10   )  OR
                              (FLG-SYSKANRI    =   1 )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               ADD     1                   TO  SPA-GMN-TBL-MAX
               MOVE    IDX                 TO  SPA-GMN-TBANGO    (IDX)
               MOVE    SYS-1010-STYUKYMD   TO  SPA-NAI-TSTYUKYMD (IDX)
               MOVE    SYS-1010-STYUKYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD (IDX)
               MOVE    SYS-1010-EDYUKYMD   TO  SPA-NAI-TEDYUKYMD (IDX)
               MOVE    SYS-1010-EDYUKYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD (IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-GMN-TBL-MAX     =   ZERO
      *        新規
               MOVE    1                   TO  SPA-GMN-SYORI
               MOVE    3                   TO  SPA-GMN-CUR
               PERFORM 200212-INIT-GMN-SEC
           ELSE
               IF      FLG-OK              =   ZERO
                   IF      SPA-SYOKUCOD-MAX     <  100
                       ADD     1               TO  SPA-SYOKUCOD-MAX
                   END-IF
                   MOVE    SYS-1010-KBNCD (2:4)
                                           TO  SPA-GMN-TSYOKUCOD
                                                   (SPA-SYOKUCOD-MAX)
                   MOVE    SYS-1010-NAME   TO  SPA-GMN-TSYOKUCOD-MEI
                                                   (SPA-SYOKUCOD-MAX)
                   MOVE    SPA-GMN-SYOKUCOD-LIST (SPA-SYOKUCOD-MAX)
                                           TO  SPA-GMN-SYOKUCOD-G
               END-IF
      *
               MOVE    2                   TO  SPA-GMN-SYORI
               IF      SPA-GMN-TBL-MAX     =   1
      *            １件を選択とする
                   MOVE    1                   TO  SPA-GMN-SELNUM
                   MOVE    SPA-GMN-SELNUM      TO  W06-SELNUM
                   PERFORM 2003-SELNUM-CHK-SEC
               ELSE
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       20021-SYOKUCOD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    新規初期表示処理
      *****************************************************************
       200212-INIT-GMN-SEC       SECTION.
      *
      *    管理者権限（管理者でない）
           MOVE    SPA-GMN-KANRIAUTH-LST(2)
                                       TO  SPA-GMN-KANRIAUTH-MEI
           MOVE    "F"                 TO  SPA-NAI-KANRIAUTHFLG
      *    診療行為初期表示
           IF      SPA-BEDFLG          >  ZERO
               MOVE    SPA-GMN-ORCA21FLG-LST (1)
                                          TO  SPA-GMN-ORCA21FLG-G
           ELSE
               MOVE    SPA-GMN-ORCA21FLG-LST (2)
                                          TO  SPA-GMN-ORCA21FLG-G
           END-IF
      *    業務権限
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           PERFORM 320-SPASET-GSRAUTH-SEC
      *
           .
       200212-INIT-GMN-EXT.
           EXIT.
      *****************************************************************
      *    番号選択チェック処理
      *****************************************************************
       2003-SELNUM-CHK-SEC       SECTION.
      *
           MOVE    W06-SELNUM          TO  SPA-GMN-SELNUM
      *
           IF     (SPA-GMN-SELNUM      >=  1   )  AND
                  (SPA-GMN-SELNUM      <=  SPA-GMN-TBL-MAX)
               MOVE    SPA-GMN-SELNUM      TO  IDX
               MOVE    SPA-GMN-TSTYUKYMD(IDX)  TO  SPA-GMN-STYUKYMD
               MOVE    SPA-GMN-TEDYUKYMD(IDX)  TO  SPA-GMN-EDYUKYMD
      *
               MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-NAI-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-NAI-EDYUKYMD
               MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-MAE-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-MAE-EDYUKYMD
      *        職員情報編集
               PERFORM 2009-SYOKUIN-HENSYU-SEC
               MOVE    3                   TO  SPA-GMN-CUR
           ELSE
               IF      SPA-GMN-SELNUM      =   ZERO
      *            選択番号が無い時、新規追加とする
                   MOVE    SPACE           TO  SPA-MAE-STYUKYMD
                   MOVE    SPACE           TO  SPA-MAE-EDYUKYMD
                   MOVE    1               TO  SPA-GMN-SYORI
               ELSE
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       2003-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    職員情報編集処理
      *****************************************************************
       2004-KIKANLIST-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TBL-MAX
               IF      W06-KIKAN-SELECT(IDX)   =   "T"
                   IF      IDX                 =   SPA-GMN-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-TBL-MAX     TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM
      *
               MOVE    SPA-GMN-SELNUM      TO  W06-SELNUM
               PERFORM 2003-SELNUM-CHK-SEC
           END-IF
           .
        2004-KIKANLIST-EXT.
           EXIT.
      *****************************************************************
      *    職員情報編集処理
      *****************************************************************
       2009-SYOKUIN-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1010-KBNCD (2:4)
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-1010-STYUKYMD 
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-1010-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1010-REC
               MOVE    SYS-1010-USERID         TO  SPA-GMN-USERID
               MOVE    SYS-1010-KANANAME       TO  SPA-GMN-KANANAME
               MOVE    SYS-1010-NAME           TO  SPA-GMN-NAME
               MOVE    SYS-1010-ISEKINUM       TO  SPA-GMN-ISEKINUM
               MOVE    SYS-1010-HKNDRNUMS      TO  SPA-GMN-HKNDRNUMS
               MOVE    SYS-1010-HKNDRNUMK      TO  SPA-GMN-HKNDRNUMK
               MOVE    SYS-1010-MAYAKULICENSE  TO  SPA-GMN-MAYAKULICENSE
      *        業務権限
               PERFORM 320-SPASET-GSRAUTH-SEC
      *
               MOVE    SYS-1010-KANRIAUTHFLG   TO  SPA-NAI-KANRIAUTHFLG
      *
               IF      SPA-NAI-KANRIAUTHFLG    =  "T"
      *            管理者である
                   MOVE    SPA-GMN-KANRIAUTH-LST(1)
                                               TO  SPA-GMN-KANRIAUTH-MEI
               ELSE
      *            管理者でない
                   MOVE    SPA-GMN-KANRIAUTH-LST(2)
                                               TO  SPA-GMN-KANRIAUTH-MEI
               END-IF
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                   MOVE    SPACE           TO  SPA-GMN-SEMONKACD-G (IDX)
                   MOVE    SYS-1010-SEMONKACD (IDX)
                                           TO  SPA-GMN-SEMONKACD (IDX)
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   SPA-SRYKA-MAX
                       IF      SPA-GMN-SEMONKACD (IDX)
                                               =   SPA-GMN-SRYKA (IDY)
                           MOVE    SPA-GMN-SEMONKACD-TBL (IDY)
                                               TO  SPA-GMN-SEMONKACD-G
                                                                 (IDX)
                           MOVE    SPA-SRYKA-MAX   TO  IDY
                       END-IF 
                   END-PERFORM
               END-PERFORM
      *        診療行為初期表示
               MOVE    SYS-1010-ORCA21FLG  TO  SPA-GMN-ORCA21FLG
               EVALUATE   SPA-GMN-ORCA21FLG
                   WHEN    "1"
                       MOVE    SPA-GMN-ORCA21FLG-LST (1)
                                               TO  SPA-GMN-ORCA21FLG-G
                   WHEN    "2"
                       MOVE    SPA-GMN-ORCA21FLG-LST (2)
                                               TO  SPA-GMN-ORCA21FLG-G
                   WHEN    OTHER
      *                診療行為初期表示
                       IF      SPA-BEDFLG          >  ZERO
                           MOVE    SPA-GMN-ORCA21FLG-LST (1)
                                               TO  SPA-GMN-ORCA21FLG-G
                       ELSE
                           MOVE    SPA-GMN-ORCA21FLG-LST (2)
                                               TO  SPA-GMN-ORCA21FLG-G
                       END-IF
               END-EVALUATE
      *
      *        発行フラグ
               MOVE    SYS-1010-SKYPRTFLG      TO  SPA-GMN-SKYPRTFLG
               MOVE    SYS-1010-MEIPRTFLG      TO  SPA-GMN-MEIPRTFLG
               MOVE    SYS-1010-SYOHOPRTFLG    TO  SPA-GMN-SYOHOPRTFLG
               MOVE    SYS-1010-YAKUJYOPRTFLG  TO  SPA-GMN-YAKUJYOFLG
               MOVE    SYS-1010-FORMALFLG      TO  SPA-GMN-FORMALFLG
               MOVE    SYS-1010-CLIENT-SAVE    TO  SPA-GMN-CLIENT-SAVE
               MOVE    SYS-1010-USER-KANRI     TO  SPA-GMN-USER-KANRI
               PERFORM 20091-PRTFLG-HEN-SEC
      *
               INITIALIZE                     SPA-PAGE3-G
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >   CONST-GRPHOSP-MAX )
                        OR   ( SPA-GRPHOSPNUM (IDX)    =   ZERO )
      *
                   MOVE    IDX             TO  SPA-GMN-GRPHOSP-MAX
      *
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL ( IDY    >   CONST-GRPHOSP-MAX )
                            OR   ( SPA-GMN-GRPUSERID (IDX)
                                          NOT =   SPACE )
                       IF    ( SYS-1010-GRPHOSPNUM (IDY) IS  NUMERIC )
                           IF    ( SYS-1010-GRPHOSPNUM (IDY)
                                       =   SPA-GRPHOSPNUM (IDX) )
                               MOVE    SYS-1010-GRPHOSPUSERID (IDY)
                                           TO  SPA-GMN-GRPUSERID (IDX)
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
           END-IF
      *
           . 
       2009-SYOKUIN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    発行フラグ編集処理 
      *****************************************************************
       20091-PRTFLG-HEN-SEC              SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-SKYPRTFLG-MAX
               IF      SPA-GMN-SKYPRTFLG   =   SPA-GMN-SKYPRTFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-SKYPRTFLG-LST (IDX)
                                           TO  SPA-GMN-SKYPRTFLG-G
                   MOVE    SPA-SKYPRTFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-MEIPRTFLG-MAX
               IF      SPA-GMN-MEIPRTFLG   =   SPA-GMN-MEIPRTFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-MEIPRTFLG-LST (IDX)
                                           TO  SPA-GMN-MEIPRTFLG-G
                   MOVE    SPA-MEIPRTFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-SYOHOPRTFLG-MAX
               IF      SPA-GMN-SYOHOPRTFLG =   SPA-GMN-SYOHOPRTFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-SYOHOPRTFLG-LST (IDX)
                                           TO  SPA-GMN-SYOHOPRTFLG-G
                   MOVE    SPA-SYOHOPRTFLG-MAX TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-YAKUJYOFLG-MAX
               IF      SPA-GMN-YAKUJYOFLG   =   SPA-GMN-YAKUJYOFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-YAKUJYOFLG-LST (IDX)
                                           TO  SPA-GMN-YAKUJYOFLG-G
                   MOVE    SPA-YAKUJYOFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-FORMALFLG-MAX
               IF      SPA-GMN-FORMALFLG   =   SPA-GMN-FORMALFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-FORMALFLG-LST (IDX)
                                           TO  SPA-GMN-FORMALFLG-G
                   MOVE    SPA-FORMALFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-CLIENT-SAVE-MAX
               IF      SPA-GMN-CLIENT-SAVE =   SPA-GMN-CLIENT-SAVE-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-CLIENT-SAVE-LST (IDX)
                                           TO  SPA-GMN-CLIENT-SAVE-G
                   MOVE    SPA-CLIENT-SAVE-MAX TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-USER-KANRI-MAX
               IF      SPA-GMN-USER-KANRI  =   SPA-GMN-USER-KANRI-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-USER-KANRI-LST (IDX)
                                           TO  SPA-GMN-USER-KANRI-G
                   MOVE    SPA-USER-KANRI-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           . 
       20091-PRTFLG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ業務処理権限初期編集処理 
      *****************************************************************
       320-SPASET-GSRAUTH-SEC              SECTION.
      *医事業務
           IF  SYS-1010-GSRAUTH(1:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(1)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(1)
           END-IF
      *受付
           IF  SYS-1010-GSRAUTH(16:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(2)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(2)
           END-IF
      *登録
           IF  SYS-1010-GSRAUTH(17:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(3)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(3)
           END-IF
      *照会
           IF  SYS-1010-GSRAUTH(18:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(4)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(4)
           END-IF
      *予約
           IF  SYS-1010-GSRAUTH(19:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(5)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(5)
           END-IF
      *診療行為
           IF  SYS-1010-GSRAUTH(21:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(6)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(6)
           END-IF
      *病名
           IF  SYS-1010-GSRAUTH(22:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(7)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(7)
           END-IF
      *収納
           IF  SYS-1010-GSRAUTH(23:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(8)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(8)
           END-IF
      *会計照会
           IF  SYS-1010-GSRAUTH(24:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(9)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(9)
           END-IF
      *データチェック
           IF  SYS-1010-GSRAUTH(26:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(10)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(10)
           END-IF
      *明細書
           IF  SYS-1010-GSRAUTH(27:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(11)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(11)
           END-IF
      *請求管理
           IF  SYS-1010-GSRAUTH(28:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(12)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(12)
           END-IF
      *総括票
           IF  SYS-1010-GSRAUTH(29:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(13)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(13)
           END-IF
      *日次統計
           IF  SYS-1010-GSRAUTH(31:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(14)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(14)
           END-IF
      *月次統計
           IF  SYS-1010-GSRAUTH(32:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(15)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(15)
           END-IF
      *データ出力
           IF  SYS-1010-GSRAUTH(41:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(16)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(16)
           END-IF
      *外部媒体
           IF  SYS-1010-GSRAUTH(47:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(17)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(17)
           END-IF
      *マスタ登録
           IF  SYS-1010-GSRAUTH(51:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(18)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(18)
           END-IF
      *マスタ更新
           IF  SYS-1010-GSRAUTH(52:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(19)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(19)
           END-IF
      *システム管理
           IF  SYS-1010-GSRAUTH(61:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(20)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(20)
           END-IF
      *点数マスタ
           IF  SYS-1010-GSRAUTH(62:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(21)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(21)
           END-IF
      *チェックマスタ
           IF  SYS-1010-GSRAUTH(63:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(22)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(22)
           END-IF
      *保険番号マスタ
           IF  SYS-1010-GSRAUTH(64:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(23)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(23)
           END-IF
      *保険者マスタ
           IF  SYS-1010-GSRAUTH(65:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(24)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(24)
           END-IF
      *人名辞書マスタ
           IF  SYS-1010-GSRAUTH(66:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(25)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(25)
           END-IF
      *----(01.01.05)----START------------
      *薬剤情報マスタ
           IF  SYS-1010-GSRAUTH(67:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(26)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(26)
           END-IF
      *----(01.01.05)----END--------------
      *----(01.01.07)----START------------
      *入退院登録
           IF  SYS-1010-GSRAUTH(36:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(27)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(27)
           END-IF
      *入院会計照会
           IF  SYS-1010-GSRAUTH(37:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(28)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(28)
           END-IF
      *入院定期請求
           IF  SYS-1010-GSRAUTH(38:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(29)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(29)
           END-IF
      *退院時仮計算
           IF  SYS-1010-GSRAUTH(39:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(30)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(30)
           END-IF
      *病床管理
           IF  SYS-1010-GSRAUTH(56:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(31)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(31)
           END-IF
      *入院患者照会
           IF  SYS-1010-GSRAUTH(57:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(32)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(32)
           END-IF
      *----(01.01.07)----END--------------
      *----(02.00.01)--ADD-START---
      *プログラム更新
           IF      SYS-1010-GSRAUTH(3:1)    =  "1"
               MOVE  "T"  TO  SPA-GMN-CHK(33)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(33)
           END-IF
      *----(02.00.01)--ADD-END-----
      *----(02.00.02)--ADD-START---
      *住所マスタ
           IF      SYS-1010-GSRAUTH(68:1)    =  "1"
               MOVE  "T"  TO  SPA-GMN-CHK(34)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(34)
           END-IF
      *----(02.00.02)--ADD-END-----
      *----(02.00.03)--ADD-START---
      *ヘルプマスタ
           IF      SYS-1010-GSRAUTH(69:1)    =  "1"
               MOVE  "T"  TO  SPA-GMN-CHK(35)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(35)
           END-IF
      *----(02.00.031)--ADD-END-----
      *    外来まとめ
           IF      SYS-1010-GSRAUTH(5:1)    =  "1"
               MOVE  "T"  TO  SPA-GMN-CHK(36)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(36)
           END-IF
           . 
       320-SPASET-GSRAUTH-EXT.
           EXIT.
           .
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    W06-STYUKYMD        TO  SPA-GMN-STYUKYMD
           MOVE    W06-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
      *
           MOVE    W06-USERID          TO  SPA-GMN-USERID
           MOVE    W06-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    W06-NAME            TO  SPA-GMN-NAME
           MOVE    W06-ISEKINUM        TO  SPA-GMN-ISEKINUM
           MOVE    W06-HKNDRNUMS       TO  SPA-GMN-HKNDRNUMS
           MOVE    W06-HKNDRNUMK       TO  SPA-GMN-HKNDRNUMK
           MOVE    W06-MAYAKULICENSE   TO  SPA-GMN-MAYAKULICENSE
      *    専門科コード
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               MOVE    W06-SEMONKACD(IDX)  TO  SPA-GMN-SEMONKACD-G(IDX)
           END-PERFORM
      *    管理者権限
           MOVE    W06-KANRIAUTHFLG    TO  SPA-GMN-KANRIAUTH-MEI
           IF      SPA-GMN-KANRIAUTH-MEI   =   SPACE
               MOVE    SPA-GMN-KANRIAUTH-LST(1)
                                           TO  SPA-GMN-KANRIAUTH-MEI
           END-IF
      *
           MOVE    W06-ORCA21FLG       TO  SPA-GMN-ORCA21FLG-G
      *
      *    個別設定画面
           MOVE    W06-SKYPRTFLG       TO  SPA-GMN-SKYPRTFLG-G
           MOVE    W06-MEIPRTFLG       TO  SPA-GMN-MEIPRTFLG-G
           MOVE    W06-SYOHOPRTFLG     TO  SPA-GMN-SYOHOPRTFLG-G
           MOVE    W06-YAKUJYOFLG      TO  SPA-GMN-YAKUJYOFLG-G
           MOVE    W06-FORMALFLG       TO  SPA-GMN-FORMALFLG-G
           MOVE    W06-CLIENT-SAVE     TO  SPA-GMN-CLIENT-SAVE-G
           MOVE    W06-USER-KANRI      TO  SPA-GMN-USER-KANRI-G
      *
      *    他院情報設定画面
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   CONST-GRPHOSP-MAX
               MOVE    W06-GRPUSERID(IDX)  TO  SPA-GMN-GRPUSERID(IDX)
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    職員番号チェック
           IF     (SPA-GMN-SYOKUCOD    =   SPACE ) OR
                  (SPA-GMN-SYOKUCOD    NOT NUMERIC )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41021-STYUKYMD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41021-EDYUKYMD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-USERID-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4112-KANANAME-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4113-NAME-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4114-ISEKINUM-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4115-HKNDRNUMS-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4116-HKNDRNUMK-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4117-MAYAKULICENSE-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE)
              AND (SPA-GMN-PAGENO      =   1    )
               PERFORM 4118-SEMONKACD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4120-KANRIAUTHFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4120-ORCA21FLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-SKYPRTFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-SYOHOPRTFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-YAKUJYOFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-MEIPRTFLG-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-FORMALFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-CLIENT-SAVE-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-USER-KANRI-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDX-GRP FROM    1   BY  1
                   UNTIL ( IDX-GRP >   SPA-GMN-GRPHOSP-MAX )
                    OR   ( SPA-ERRCD           NOT =   SPACE )
               PERFORM 4122-GRPUSERID-CHK-SEC
           END-PERFORM
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効開始日チェック処理
      *****************************************************************
       41021-STYUKYMD-CHK-SEC            SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-STYUKYMD
      *
           IF      SPA-GMN-STYUKYMD   =   SPACE
               IF      WRK-MCP-WIDGET     =   "STYUKYMD"
                   MOVE    "00000000"         TO  SPA-GMN-STYUKYMD
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-STYUKYMD 
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
           .
        41021-STYUKYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効終了日チェック処理
      *****************************************************************
       41021-EDYUKYMD-CHK-SEC            SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-EDYUKYMD
      *
           IF      SPA-GMN-EDYUKYMD   =   SPACE
               IF      WRK-MCP-WIDGET     =   "EDYUKYMD"
                   MOVE    "99999999"         TO  SPA-GMN-EDYUKYMD
               END-IF
           END-IF
      *
           IF      SPA-GMN-EDYUKYMD    NOT =   SPACE
               MOVE    SPA-GMN-EDYUKYMD    TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-SYMD            TO  SPA-NAI-EDYUKYMD 
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
               ELSE
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF     (SPA-GMN-STYUKYMD    NOT =   SPACE ) AND
                  (SPA-GMN-EDYUKYMD    NOT =   SPACE )
               IF      SPA-NAI-STYUKYMD    >   SPA-NAI-EDYUKYMD
                   MOVE    "0005"             TO  SPA-ERRCD
                   MOVE    4                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
        41021-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    オペレータＩＤチェック
      *****************************************************************
       4100-USERID-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-USERID      =   SPACE
               IF      WRK-MCP-WIDGET     =   "USERID"
                   MOVE    "0008"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-CUR
               END-IF
           ELSE 
      *    半角チェック
               INITIALIZE              ORCSKANACHKAREA
               MOVE  "1"               TO  KANACHK-SYORI
               MOVE  SPA-GMN-USERID    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING 
                                       ORCSKANACHKAREA
      *
               IF      (KANACHK-SYUBETU NOT  =  1)
                   OR  (KANACHK-RC      NOT  =  0)
                   MOVE    "0009"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        登録済みチェック
               INITIALIZE                  SYS-1010-REC
               MOVE    "1010"              TO  SYS-1010-KANRICD
               STRING  SPA-GMN-USERID      DELIMITED  BY  SPACE
                       "%"                 DELIMITED  BY  SIZE
                                           INTO    SYS-1010-TBL
               END-STRING
               MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD 
               MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD 
               MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
               MOVE    SYS-1010-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 990-SYSKANRI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
               PERFORM UNTIL  (FLG-SYSKANRI    =   1    )  OR
                              (SPA-ERRCD   NOT =   SPACE)
                   MOVE    MCPDATA-REC             TO  SYS-1010-REC
                   IF      SYS-1010-USERID         =   SPA-GMN-USERID
                       IF     (SYS-1010-KBNCD(1:1) =   SPA-GMN-SYOKUKBN)
                         AND  (SYS-1010-KBNCD(2:4) =   SPA-GMN-SYOKUCOD)
                           CONTINUE
                       ELSE
                           MOVE    "0007"              TO  SPA-ERRCD
                           MOVE    6                   TO  SPA-GMN-CUR
                       END-IF
                   END-IF
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 990-SYSKANRI-READ-SEC
               END-PERFORM
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
           .
       4100-USERID-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    フリガナチェック処理
      *****************************************************************
       4112-KANANAME-CHK-SEC           SECTION.
      *
      *    全角チェック
           IF      SPA-GMN-KANANAME    NOT  =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-KANANAME    TO  KANACHK-MAE-INPUT
               MOVE    80                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-KANANAME
      *
               IF      (KANACHK-RC         NOT =  0)  OR
                       (KANACHK-SYUBETU    NOT =  2)
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-KANANAME
               END-IF
           END-IF
      *  
           .
       4112-KANANAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名チェック処理
      *****************************************************************
       4113-NAME-CHK-SEC        SECTION.
      *
      *    全角チェック
           IF      SPA-GMN-NAME        =   SPACE
               CONTINUE
           ELSE
      *
           IF      SPA-ENCODING        =   2
      *        拡張漢字対応
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"                 TO  KANACHK2-SYORI
               MOVE    SPA-GMN-NAME        TO  KANACHK2-MAE-INPUT
               MOVE    80                  TO  KANACHK2-MAX-CNT
               CALL    "ORCSKANACHK2"      USING 
                                           ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT   TO  SPA-GMN-NAME
      *
               IF      (KANACHK2-RC         NOT =  0)  OR
                       (KANACHK2-SYUBETU    NOT =  2)
                   MOVE    "0011"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK2-OUT-INPUT  TO  SPA-GMN-NAME
               END-IF
           ELSE
      *        拡張漢字不可
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-NAME        TO  KANACHK-MAE-INPUT
               MOVE    80                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-NAME
      *
               IF      (KANACHK-RC         NOT =  0)  OR
                       (KANACHK-SYUBETU    NOT =  2)
                   MOVE    "0011"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-NAME
               END-IF
           END-IF
      *
           END-IF
           . 
       4113-NAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医籍登録番号チェック処理
      *****************************************************************
       4114-ISEKINUM-CHK-SEC        SECTION.
           IF      SPA-GMN-ISEKINUM    NOT  =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-ISEKINUM    TO  KANACHK-MAE-INPUT
               MOVE    8                   TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-ISEKINUM
           END-IF
      *
           .
       4114-ISEKINUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険医登録番号（社保）チェック処理
      *****************************************************************
       4115-HKNDRNUMS-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-HKNDRNUMS  NOT  =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-HKNDRNUMS   TO  KANACHK-MAE-INPUT
               MOVE    8                   TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-HKNDRNUMS
           END-IF
           .
       4115-HKNDRNUMS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険医登録番号（国保）チェック処理
      *****************************************************************
       4116-HKNDRNUMK-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-HKNDRNUMK  NOT  =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-HKNDRNUMK   TO  KANACHK-MAE-INPUT
               MOVE    12                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-HKNDRNUMK
           END-IF
      *
           .
       4116-HKNDRNUMk-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻薬ライセンス チェック処理
      *****************************************************************
       4117-MAYAKULICENSE-CHK-SEC        SECTION.
      *
      *    混在チェック
           IF      SPA-GMN-MAYAKULICENSE   =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-MAYAKULICENSE
                                           TO  KANACHK-MAE-INPUT
               MOVE    40                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-MAYAKULICENSE
      *
               IF      KANACHK-RC      NOT =   ZERO
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    12                  TO  SPA-GMN-CUR
               ELSE
      *            全角の時、空白変換分を編集
                   IF      KANACHK-SYUBETU     =   2
                       MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-MAYAKULICENSE
                   END-IF
               END-IF
           END-IF
      *
           .
       4117-MAYAKULICENSE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    専門科コードチェック処理
      *****************************************************************
       4118-SEMONKACD-CHK-SEC        SECTION.
      *
      *    専門科コード
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5   )   OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      SPA-GMN-SEMONKACD(IDX)  NOT =   SPACE
      *            桁数変換
                   MOVE    SPA-GMN-SEMONKACD (IDX) TO  WRK-CD
                   PERFORM 41181-NUMHEN-SEC
                   MOVE    WRK-CD              TO  SPA-GMN-SEMONKACD
                                                               (IDX)
      *            科コードチェック
                   MOVE    ZERO                TO  FLG-CHK
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   SPA-SRYKA-MAX
                       IF      SPA-GMN-SEMONKACD (IDX)
                                               =   SPA-GMN-SRYKA (IDY)
                           MOVE    SPA-GMN-SEMONKACD-TBL (IDY)
                                               TO  SPA-GMN-SEMONKACD-G
                                                                (IDX)
                           MOVE    1               TO  FLG-CHK
                           MOVE    SPA-SRYKA-MAX   TO  IDY
                       END-IF
                   END-PERFORM
      *            登録前・タグ遷移のチェックはしない
                   IF      FLG-TOUROKU         =   1
                       CONTINUE
                   ELSE
                       IF      FLG-CHK             =   ZERO
                           MOVE    "0013"          TO  SPA-ERRCD
                           COMPUTE SPA-GMN-CUR     =   IDX  +   12
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4118-SEMONKACD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    科コード数字変換処理
      *****************************************************************
       41181-NUMHEN-SEC             SECTION.
      *    数字変換
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-CD              TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SEISUU         >   2     )   OR
                  (SNUM-SYOSUU         >   ZERO  )
               MOVE    "0013"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  WRK-CD
           END-IF
      *
           .
       41181-NUMHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務処理権限チェック処理
      *****************************************************************
       4119-GSRAUTH-CHK-SEC             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           IF      MCP-WIDGET(4:2)     NUMERIC
               MOVE    MCP-WIDGET(4:2)     TO  IDX2
               MOVE    IDX2                TO  IDX
           ELSE
               GO      TO      4119-GSRAUTH-CHK-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-CHK (IDX)
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-CHK (IDX)
               WHEN    "F"
                   MOVE    "T"             TO  SPA-GMN-CHK (IDX)
           END-EVALUATE
      *
      *    システム管理はオルカマスターの時、可能
           IF       SPA-GMN-CHK(20)    =   "F"
               IF      (SPA-GMN-SYOKUKBN   =   "0"  )  AND
                       (SPA-GMN-SYOKUCOD   =   "0001")
                    MOVE    "0100"         TO  SPA-ERRCD
               END-IF
           END-IF
           EVALUATE    IDX
               WHEN    18
      *        マスタ登録
               IF      SPA-GMN-CHK(IDX)    =   "F"
                   MOVE    "F"            TO  SPA-GMN-CHK(20)
                   MOVE    "F"            TO  SPA-GMN-CHK(21)
                   MOVE    "F"            TO  SPA-GMN-CHK(22)
                   MOVE    "F"            TO  SPA-GMN-CHK(23)
                   MOVE    "F"            TO  SPA-GMN-CHK(24)
                   MOVE    "F"            TO  SPA-GMN-CHK(25)
                   MOVE    "F"            TO  SPA-GMN-CHK(26)
                   MOVE    "F"            TO  SPA-GMN-CHK(34)
                   MOVE    "F"            TO  SPA-GMN-CHK(35)
               END-IF
               WHEN    20  THRU  26
               WHEN    34
               WHEN    35
      *        M02画面
               IF      SPA-GMN-CHK(IDX)    =   "T"
                   MOVE    "T"            TO  SPA-GMN-CHK(18)
               END-IF
           END-EVALUATE
      *
      *
      *    他の入力項目のチェック
           PERFORM 410-INPUT-CHK-SEC
           .
       4119-GSRAUTH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理者権限フラグチェック処理
      *****************************************************************
       4120-KANRIAUTHFLG-CHK-SEC        SECTION.
      *
           EVALUATE    SPA-GMN-KANRIAUTH-MEI
      *        管理者である
               WHEN    SPA-GMN-KANRIAUTH-LST(1)
                   MOVE    "T"             TO  SPA-NAI-KANRIAUTHFLG
      *        管理者でない
               WHEN    SPA-GMN-KANRIAUTH-LST(2)
                   MOVE    "F"             TO  SPA-NAI-KANRIAUTHFLG
      *        その他（管理者でない）
               WHEN    OTHER
                   MOVE    SPA-GMN-KANRIAUTH-LST(2)
                                           TO  SPA-GMN-KANRIAUTH-MEI
                   MOVE    "F"             TO  SPA-NAI-KANRIAUTHFLG
           END-EVALUATE
      *
           .
       4120-KANRIAUTHFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為初期画チェック処理
      *****************************************************************
       4120-ORCA21FLG-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-ORCA21FLG   =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-ORCA21FLG-MAX
                   IF      SPA-GMN-ORCA21FLG   =   SPA-GMN-ORCA21FLG-LST
                                                       (IDX)(1:1)
                       MOVE    SPA-GMN-ORCA21FLG-LST (IDX)
                                               TO  SPA-GMN-ORCA21FLG-G
                       MOVE    1               TO  FLG-CHK
                       MOVE    SPA-ORCA21FLG-MAX   TO  IDX
                    END-IF
               END-PERFORM
               IF     (FLG-CHK             =   ZERO )
                   MOVE    "0014"          TO  SPA-ERRCD
                   MOVE    18              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4120-ORCA21FLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行フラグチェック処理
      *****************************************************************
       4121-SKYPRTFLG-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SKYPRTFLG-MAX
               IF      SPA-GMN-SKYPRTFLG   =   SPA-GMN-SKYPRTFLG-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-SKYPRTFLG-LST (IDX)
                                               TO  SPA-GMN-SKYPRTFLG-G
                    MOVE    1                  TO  FLG-CHK
                   MOVE    SPA-SKYPRTFLG-MAX   TO  IDX
                END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-SKYPRTFLG   =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-SKYPRTFLG-G
               ELSE
                   MOVE    "0016"          TO  SPA-ERRCD
                   MOVE    21              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-SKYPRTFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外処方せん発行フラグチェック処理
      *****************************************************************
       4121-SYOHOPRTFLG-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOHOPRTFLG-MAX
               IF      SPA-GMN-SYOHOPRTFLG =   SPA-GMN-SYOHOPRTFLG-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-SYOHOPRTFLG-LST (IDX)
                                               TO  SPA-GMN-SYOHOPRTFLG-G
                    MOVE    1                  TO  FLG-CHK
                   MOVE    SPA-SYOHOPRTFLG-MAX TO  IDX
                END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-SYOHOPRTFLG =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-SYOHOPRTFLG-G
               ELSE
                   MOVE    "0017"          TO  SPA-ERRCD
                   MOVE    22              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-SKYPRTFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報発行フラグチェック処理
      *****************************************************************
       4121-YAKUJYOFLG-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YAKUJYOFLG-MAX
               IF      SPA-GMN-YAKUJYOFLG   =   SPA-GMN-YAKUJYOFLG-LST
                                                       (IDX)(1:1)
                    MOVE    SPA-GMN-YAKUJYOFLG-LST (IDX)
                                               TO  SPA-GMN-YAKUJYOFLG-G
                    MOVE    1                  TO  FLG-CHK
                    MOVE    SPA-YAKUJYOFLG-MAX   TO  IDX
                END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-YAKUJYOFLG  =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-YAKUJYOFLG-G
               ELSE
                   MOVE    "0018"          TO  SPA-ERRCD
                   MOVE    23              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-YAKUJYOFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行フラグチェック処理
      *****************************************************************
       4121-MEIPRTFLG-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIPRTFLG-MAX
               IF      SPA-GMN-MEIPRTFLG   =   SPA-GMN-MEIPRTFLG-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-MEIPRTFLG-LST (IDX)
                                               TO  SPA-GMN-MEIPRTFLG-G
                    MOVE    1                  TO  FLG-CHK
                   MOVE    SPA-MEIPRTFLG-MAX   TO  IDX
                END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-MEIPRTFLG   =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-MEIPRTFLG-G
               ELSE
                   MOVE    "0019"          TO  SPA-ERRCD
                   MOVE    24              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-MEIPRTFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検査正式名称表示フラグチェック処理
      *****************************************************************
       4121-FORMALFLG-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FORMALFLG-MAX
               IF      SPA-GMN-FORMALFLG   =   SPA-GMN-FORMALFLG-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-FORMALFLG-LST (IDX)
                                               TO  SPA-GMN-FORMALFLG-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-FORMALFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-FORMALFLG   =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-FORMALFLG-G
               ELSE
                   MOVE    "0022"          TO  SPA-ERRCD
                   MOVE    25              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-FORMALFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    クライアント保存可否チェック処理
      *****************************************************************
       4121-CLIENT-SAVE-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-CLIENT-SAVE-MAX
               IF      SPA-GMN-CLIENT-SAVE =   SPA-GMN-CLIENT-SAVE-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-CLIENT-SAVE-LST (IDX)
                                               TO  SPA-GMN-CLIENT-SAVE-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-CLIENT-SAVE-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-CLIENT-SAVE  =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-CLIENT-SAVE-G
               ELSE
                   MOVE    "0023"          TO  SPA-ERRCD
                   MOVE    26              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-CLIENT-SAVE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザーログイン権限チェック処理
      *****************************************************************
       4121-USER-KANRI-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-USER-KANRI-MAX
               IF      SPA-GMN-USER-KANRI  =   SPA-GMN-USER-KANRI-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-USER-KANRI-LST (IDX)
                                               TO  SPA-GMN-USER-KANRI-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-USER-KANRI-MAX  TO  IDX
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-USER-KANRI  =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-USER-KANRI-G
               ELSE
                   MOVE    "0025"          TO  SPA-ERRCD
                   MOVE    27              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-USER-KANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    他院オペレータＩＤチェック処理
      *****************************************************************
       4122-GRPUSERID-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-GRPUSERID (IDX-GRP)     =   SPACE )
               CONTINUE
           ELSE
      *
               COMPUTE WRK-ERRCUR  =   IDX-GRP    +   200
      *
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    SPA-GMN-GRPUSERID (IDX-GRP)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING 
                                       ORCSKANACHKAREA
      *
               IF      (KANACHK-SYUBETU    NOT =  1)
                   OR  (KANACHK-RC         NOT =  0)
                   MOVE    "0024"          TO  SPA-ERRCD
                   MOVE    WRK-ERRCUR      TO  SPA-GMN-CUR
               END-IF
      *
      *            他院職員情報存在チェック
               IF    ( SPA-ERRCD       =   SPACE )
      *
                   MOVE    ZERO                TO  FLG-USERMATCH
                   MOVE    SPACE               TO  SYS-1010-REC
                   INITIALIZE                      SYS-1010-REC
                   MOVE    SPA-GRPHOSPNUM (IDX-GRP)
                                               TO  SYS-1010-HOSPNUM
                   MOVE    "1010"              TO  SYS-1010-KANRICD
                   MOVE    SPA-MAE-STYUKYMD    TO  SYS-1010-STYUKYMD
                   MOVE    SPA-MAE-STYUKYMD    TO  SYS-1010-EDYUKYMD
      *
                   MOVE    SYS-1010-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM   910-DBSELECT-SEC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM   920-DBFETCH-SEC
                   PERFORM   UNTIL     (FLG-USERMATCH  =  1)  OR
                                       (MCP-RC      NOT = 0)
                       MOVE    MCPDATA-REC     TO  SYS-1010-REC
                       IF    SPA-GMN-GRPUSERID (IDX-GRP)
                                            =   SYS-1010-USERID
                           MOVE   1     TO   FLG-USERMATCH
                       END-IF
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM    920-DBFETCH-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
                   IF         FLG-USERMATCH    =   ZERO
                       MOVE    "0028"          TO  SPA-ERRCD
                       MOVE    WRK-ERRCUR      TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-GMN-GRPUSERID (IDX-GRP)
                                       =   SPA-GMN-USERID )
                       MOVE    "0026"      TO  SPA-ERRCD
                       MOVE    WRK-ERRCUR  TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
               PERFORM VARYING IDX FROM    IDX-GRP BY  1
                    UNTIL( IDX         >   SPA-GMN-GRPHOSP-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( IDX     =   IDX-GRP )
                       CONTINUE
                   ELSE
                       IF    ( SPA-ERRCD   =   SPACE )
                           IF    ( SPA-GMN-GRPUSERID (IDX-GRP)
                                           =   SPA-GMN-GRPUSERID (IDX))
                               MOVE    "0027"      TO  SPA-ERRCD
                               COMPUTE SPA-GMN-CUR =   IDX +   200
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
           END-IF
      *
           .
       4122-GRPUSERID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    タグ切替え処理
      *****************************************************************
       460-TAGUCHG-SEC           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    1                   TO  FLG-TOUROKU
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "TGN1"
      *                基本画面
                       IF      SPA-GMN-SYOKUKBN    =   "1"
                           MOVE    9               TO  SPA-GMN-CUR
                       ELSE
                           MOVE    31              TO  SPA-GMN-CUR
                       END-IF
                       MOVE    1                   TO  SPA-GMN-PAGENO
                   WHEN    "TGN2"
      *                個別設定
                       MOVE    21                  TO  SPA-GMN-CUR
                       MOVE    2                   TO  SPA-GMN-PAGENO
                   WHEN    "TGN3"
      *                他院設定
                       MOVE    201                 TO  SPA-GMN-CUR
                       MOVE    3                   TO  SPA-GMN-PAGENO
               END-EVALUATE
           END-IF
      *
           .
       460-TAGUCHG-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W06"               TO  SPA-MOTOPG
      *
      *    INITIALIZE                  SPA-W01KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      *    クリア処理
      *****************************************************************
       402-CLERA-SEC                               SECTION.
      *
      *    職員情報初期表示
           PERFORM 3101-INIT-GMN-SEC
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       402-CLERA-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           IF      SPA-GMN-SELNUM      >   ZERO
               MOVE    "1003"              TO  SPA-WIDCD
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
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"                  TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN        TO  SYS-1010-KBNCD(1:1)
           MOVE    SPA-GMN-SYOKUCOD        TO  SYS-1010-KBNCD(2:4)
           MOVE    SPA-MAE-STYUKYMD        TO  SYS-1010-STYUKYMD
           MOVE    SPA-MAE-EDYUKYMD        TO  SYS-1010-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "W06 DB DEL ERR SYSKANRI-KEY =>" SYS-1010-KEY 
           END-IF
      *    ユーザＩＤ登録判定
           MOVE    ZERO                TO  FLG-USERID-UPD
           IF      SPA-GMN-TBL-MAX     >   1
               PERFORM 40311-USERID-CHEK-SEC
           END-IF
      *    ユーザＩＤを削除
           IF      FLG-USERID-UPD      =   ZERO
               INITIALIZE                      SYSUSER-REC
               MOVE    SPA-GMN-USERID      TO  SYSUSER-USERID
               MOVE    SYSUSER-REC         TO  MCPDATA-REC
               MOVE    "tbl_sysuser"       TO  MCP-TABLE 
               MOVE    "key"               TO  MCP-PATHNAME 
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "W06 DB DEL ERR USERID-KEY =>" SYSUSER-USERID
           END-IF
      *
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        職員情報初期表示
               PERFORM 3101-INIT-GMN-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザＩＤチェック処理
      *****************************************************************
       40311-USERID-CHEK-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1010-KBNCD (2:4)
           MOVE    "%"                 TO  SYS-1010-KBNCD (6:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL        (FLG-SYSKANRI      =   1 )
                           OR   (FLG-USERID-UPD    =   1 )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SYS-1010-USERID     =   SPA-GMN-USERID
                   MOVE    1                   TO  FLG-USERID-UPD
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       40311-USERID-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ切替処理
      *****************************************************************
       480-BETUGAMEN-SEC              SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    1                   TO  FLG-TOUROKU
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    TRUE
               WHEN  ( SPA-GMN-PAGENO      =   1 )
                   MOVE    2                   TO  SPA-GMN-PAGENO
                   MOVE    21                  TO  SPA-GMN-CUR
               WHEN  ( SPA-GMN-PAGENO      =   2 )
                  AND( SPA-GRPHOSPKBN      =   1 )
                   MOVE    3                   TO  SPA-GMN-PAGENO
                   MOVE    201                 TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    1                   TO  SPA-GMN-PAGENO
      *            基本画面
                   IF      SPA-GMN-SYOKUKBN    =   "1"
                       MOVE    9               TO  SPA-GMN-CUR
                   ELSE
                       MOVE    31              TO  SPA-GMN-CUR
                   END-IF
               END-EVALUATE
           END-IF
           .
       480-BETUGAMEN-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力項目のチェック
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
              MOVE    "1002"           TO  SPA-WIDCD
      *       有効開始日の変更は追加
              IF     (SPA-GMN-SELNUM       =   ZERO )  OR
                     (SPA-GMN-SYORI        =   1    )
                  MOVE    "1001"           TO  SPA-WIDCD
               END-IF
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *    職員コード
           IF     (SPA-GMN-SYOKUCOD    =   SPACE )  OR
                  (SPA-GMN-SYOKUCOD    NOT NUMERIC)
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-SYOKUCOD-MAX) OR
                                  (FLG-OK      =   1    )
                   IF      SPA-GMN-SYOKUCOD    =   SPA-GMN-TSYOKUCOD
                                                         (IDX)
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-PERFORM
      *        入力済みの時、選択があること
               IF      FLG-OK          =   1
                   IF      SPA-GMN-TBL-MAX =   ZERO
                       MOVE    "0021"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *    医師でゼロの場合、警告
           IF     (SPA-GMN-SYOKUKBN    =   "1"  )  AND
                  (SPA-GMN-SYOKUCOD    =   ZERO )
               IF      SPA-NAI-KEIFLG      =   ZERO
                   MOVE    "K001"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-KEIFLG
               END-IF
           END-IF
      *    オペレータＩＤ
           IF     (SPA-GMN-USERID      =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
      *
      *    氏名必須チェック
           IF     (SPA-GMN-NAME        =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-GMN-STYUKYMD   =   SPACE
               MOVE    "00000000"         TO  SPA-GMN-STYUKYMD
               MOVE    "00000000"         TO  SPA-NAI-STYUKYMD
           END-IF
           IF      SPA-GMN-EDYUKYMD   =   SPACE
               MOVE    "99999999"         TO  SPA-GMN-EDYUKYMD
               MOVE    "99999999"         TO  SPA-NAI-EDYUKYMD
           END-IF
           IF      SPA-ERRCD          =   SPACE
               IF      SPA-NAI-STYUKYMD   >   SPA-NAI-EDYUKYMD
                   MOVE    "0005"             TO  SPA-ERRCD
                   MOVE    3                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    期間重複チェック
      *    IF     (SPA-GMN-SELNUM      =   ZERO  )  OR
      *           (SPA-NAI-STYUKYMD    NOT =   SPA-MAE-STYUKYMD)
      *******  MOVE    1               TO  SPA-GMN-SYORI
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN-TBL-MAX )
                              OR  (SPA-ERRCD   NOT =   SPACE )
                   IF      SPA-GMN-SELNUM  NOT =   IDX
                       IF     (SPA-NAI-TEDYUKYMD (IDX)
                                           <   SPA-NAI-STYUKYMD )  OR
                              (SPA-NAI-TSTYUKYMD (IDX)
                                           >   SPA-NAI-EDYUKYMD )
                           CONTINUE
                       ELSE
                           MOVE    "0020"           TO  SPA-ERRCD
                           MOVE    3                TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-PERFORM
      **** END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        システムユーザーチェック
               PERFORM 49001-SYSUSER-CHK-SEC
           END-IF
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       システムユーザーチェック処理
      *****************************************************************
       49001-SYSUSER-CHK-SEC           SECTION.
      *
           INITIALIZE                      SYSUSER-REC
           MOVE    SPA-GMN-USERID      TO  SYSUSER-USERID
           MOVE    SYSUSER-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysuser"   TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 900-SYSUSER-READ-SEC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           MOVE    ZERO                TO  FLG-USERID-UPD
           IF      FLG-SYSUSER         =   ZERO
      *        登録済みの時、同じ医療機関であること
               IF      SPA-HOSPNUM     NOT =   SYSUSER-HOSPNUM
                   MOVE    "0025"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    ZERO                TO  FLG-USERID-UPD
               END-IF
           ELSE
               MOVE    1                   TO  FLG-USERID-UPD
           END-IF
           .
       49001-SYSUSER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    更新日取得
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD(1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1010-KBNCD(2:4)
           MOVE    SPA-MAE-STYUKYMD    TO  SYS-1010-STYUKYMD
           MOVE    SPA-MAE-STYUKYMD    TO  SYS-1010-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1010-REC
           ELSE
               MOVE    SPACE                   TO  SYS-1010-REC
               INITIALIZE                      SYS-1010-REC
               MOVE   "1010"                   TO  SYS-1010-KANRICD
               MOVE    SPA-GMN-SYOKUKBN        TO  SYS-1010-KBNCD(1:1)
               MOVE    SPA-GMN-SYOKUCOD        TO  SYS-1010-KBNCD(2:4)
           END-IF
      *
      *    ユザー
      *    システムユーザーチェック
           PERFORM 49001-SYSUSER-CHK-SEC
           MOVE    SPACE               TO  WRK-MAE-USERID
           IF      SPA-GMN-USERID  NOT =   SYS-1010-USERID
               MOVE    SYS-1010-USERID     TO  WRK-MAE-USERID
               MOVE    1                   TO  FLG-USERID-UPD
           END-IF
      *
           MOVE    SPA-NAI-STYUKYMD        TO  SYS-1010-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD        TO  SYS-1010-EDYUKYMD
           MOVE    SPA-GMN-USERID          TO  SYS-1010-USERID 
           MOVE    SPA-GMN-KANANAME        TO  SYS-1010-KANANAME 
           MOVE    SPA-GMN-NAME            TO  SYS-1010-NAME 
           MOVE    SPA-GMN-ISEKINUM        TO  SYS-1010-ISEKINUM 
           MOVE    SPA-GMN-HKNDRNUMS       TO  SYS-1010-HKNDRNUMS
           MOVE    SPA-GMN-HKNDRNUMK       TO  SYS-1010-HKNDRNUMK
           MOVE    SPA-GMN-MAYAKULICENSE   TO  SYS-1010-MAYAKULICENSE
      *
           PERFORM 4900-KOUSHIN-GSRAUTH-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
               MOVE    SPA-GMN-SEMONKACD(IDX)
                                           TO  SYS-1010-SEMONKACD (IDX)
           END-PERFORM
           MOVE    SPA-NAI-KANRIAUTHFLG    TO  SYS-1010-KANRIAUTHFLG
      *
           MOVE    SPA-GMN-ORCA21FLG       TO  SYS-1010-ORCA21FLG
      *
           MOVE    SPA-GMN-SKYPRTFLG       TO  SYS-1010-SKYPRTFLG
           MOVE    SPA-GMN-SYOHOPRTFLG     TO  SYS-1010-SYOHOPRTFLG
           MOVE    SPA-GMN-YAKUJYOFLG      TO  SYS-1010-YAKUJYOPRTFLG
           MOVE    SPA-GMN-MEIPRTFLG       TO  SYS-1010-MEIPRTFLG
           MOVE    SPA-GMN-FORMALFLG       TO  SYS-1010-FORMALFLG
           MOVE    SPA-GMN-CLIENT-SAVE     TO  SYS-1010-CLIENT-SAVE
           MOVE    SPA-GMN-USER-KANRI      TO  SYS-1010-USER-KANRI
      *
           INITIALIZE                          SYS-1010-GRPHOSPINF-G
           MOVE    ZERO                    TO  IDX-GRP
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >   SPA-GMN-GRPHOSP-MAX )
      *
               IF    ( SPA-GMN-GRPUSERID (IDX) NOT =   SPACE )
                   COMPUTE IDX-GRP =   IDX-GRP +   1
                   MOVE    SPA-GRPHOSPNUM (IDX)
                                   TO  SYS-1010-GRPHOSPNUM    (IDX-GRP)
                   MOVE    SPA-GMN-GRPUSERID (IDX)
                                   TO  SYS-1010-GRPHOSPUSERID (IDX-GRP)
               END-IF
      *
           END-PERFORM
      *
           IF     (FLG-SYSKANRI        =   ZERO ) AND
                  (SPA-GMN-SYORI       =   2    )
      *        更新
               MOVE    SMCNDATE-YMD        TO  SYS-1010-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1010-UPHMS
               MOVE    SYS-1010-REC        TO  SYSKANRI-REC
      *
               MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
               MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
               MOVE    SPA-MAE-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-MAE-EDYUKYMD    TO  SYSUP-EDYUKYMD
      *
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W06 DB UPD ERR KEY:" SYS-1010-KEY 
               END-IF
           ELSE
      *        追加
               MOVE    SMCNDATE-YMD        TO  SYS-1010-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYS-1010-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1010-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
               MOVE    SYS-1010-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W06 DB INS ERR KEY:" SYS-1010-KEY 
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-USERID-UPD      =   1     )
      *        システムユーザー更新処理
               PERFORM 4905-SYSUSER-UPD-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        職員情報初期表示
               PERFORM 3101-INIT-GMN-SEC
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       チェック登録処理
      *****************************************************************
       4900-KOUSHIN-GSRAUTH-SEC           SECTION.
      *医事業務
           IF  SPA-GMN-CHK(1)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(1:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(1:1)
           END-IF
      *受付
           IF  SPA-GMN-CHK(2)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(16:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(16:1)
           END-IF
      *登録
           IF  SPA-GMN-CHK(3)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(17:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(17:1)
           END-IF
      *照会
           IF  SPA-GMN-CHK(4)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(18:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(18:1)
           END-IF
      *予約
           IF  SPA-GMN-CHK(5)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(19:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(19:1)
           END-IF
      *診療行為
           IF  SPA-GMN-CHK(6)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(21:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(21:1)
           END-IF
      *病名
           IF  SPA-GMN-CHK(7)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(22:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(22:1)
           END-IF
      *収納
           IF  SPA-GMN-CHK(8)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(23:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(23:1) 
           END-IF
      *会計照会
           IF  SPA-GMN-CHK(9)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(24:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(24:1)
           END-IF
      *データチェック
           IF  SPA-GMN-CHK(10)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(26:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(26:1)
           END-IF
      *明細書
           IF  SPA-GMN-CHK(11)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(27:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(27:1)
           END-IF
      *請求管理
           IF  SPA-GMN-CHK(12)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(28:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(28:1)
           END-IF
      *総括票
           IF  SPA-GMN-CHK(13)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(29:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(29:1)
           END-IF
      *日次統計
           IF  SPA-GMN-CHK(14)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(31:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(31:1)
           END-IF
      *月次統計
           IF  SPA-GMN-CHK(15)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(32:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(32:1)
           END-IF
      *データ出力
           IF  SPA-GMN-CHK(16)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(41:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(41:1)
           END-IF
      *外部媒体
           IF  SPA-GMN-CHK(17)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(47:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(47:1)
           END-IF
      *マスタ登録
           IF  SPA-GMN-CHK(18)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(51:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(51:1)
           END-IF
      *マスタ更新
           IF  SPA-GMN-CHK(19)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(52:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(52:1) 
           END-IF
      *システム管理
           IF  SPA-GMN-CHK(20)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(61:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(61:1)
           END-IF
      *点数マスタ
           IF  SPA-GMN-CHK(21)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(62:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(62:1)
           END-IF
      *チェックマスタ
           IF  SPA-GMN-CHK(22)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(63:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(63:1)
           END-IF
      *保険番号マスタ
           IF  SPA-GMN-CHK(23)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(64:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(64:1)
           END-IF
      *保険者マスタ
           IF  SPA-GMN-CHK(24)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(65:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(65:1)
           END-IF
      *人名辞書マスタ
           IF  SPA-GMN-CHK(25)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(66:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(66:1)
           END-IF
      *----(01.01.05)----START------------
      *薬剤情報マスタ
           IF  SPA-GMN-CHK(26)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(67:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(67:1)
           END-IF
      *----(01.01.05)----START------------
      *----(01.01.07)----START------------
      *入退院登録
           IF  SPA-GMN-CHK(27)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(36:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(36:1)
           END-IF
      *入院会計照会
           IF  SPA-GMN-CHK(28)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(37:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(37:1)
           END-IF
      *入院定期請求
           IF  SPA-GMN-CHK(29)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(38:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(38:1)
           END-IF
      *退院時仮計算
           IF  SPA-GMN-CHK(30)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(39:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(39:1)
           END-IF
      *病床管理
           IF  SPA-GMN-CHK(31)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(56:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(56:1)
           END-IF
      *入院患者照会
           IF  SPA-GMN-CHK(32)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(57:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(57:1)
           END-IF
      *----(01.01.07)----START------------
      *----(02.00.01)--ADD-START---
      *プログラム管理
           IF  SPA-GMN-CHK(33)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(3:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(3:1)
           END-IF
      *----(02.00.01)--ADD-END-----
      *----(02.00.02)--ADD-START---
      *住所マスタ
           IF  SPA-GMN-CHK(34)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(68:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(68:1)
           END-IF
      *----(02.00.02)--ADD-END-----
      *----(02.00.03)--ADD-START---
      *ヘルプマスタ
           IF  SPA-GMN-CHK(35)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(69:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(69:1)
           END-IF
      *----(02.00.03)--ADD-END-----
      *外来まとめ
           IF  SPA-GMN-CHK(36)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(5:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(5:1)
           END-IF
          .
       4900-KOUSHIN-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    システムユーザー更新処理
      *****************************************************************
       4905-SYSUSER-UPD-SEC              SECTION.
      *
      *    前のユーザＩＤを削除
           IF      WRK-MAE-USERID  NOT =   SPACE
               INITIALIZE                      SYSUSER-REC
               MOVE    WRK-MAE-USERID      TO  SYSUSER-USERID
               MOVE    SYSUSER-REC         TO  MCPDATA-REC
               MOVE    "tbl_sysuser"       TO  MCP-TABLE 
               MOVE    "key"               TO  MCP-PATHNAME 
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1002"              TO  SPA-ERRCD
                   DISPLAY "W06 DB DEL ERR USERID-KEY =>" SYSUSER-USERID
               END-IF
           END-IF
      *
      *    新規追加
           INITIALIZE                      SYSUSER-REC
           MOVE    SYS-1010-USERID     TO  SYSUSER-USERID
           MOVE    SYSUSER-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysuser"   TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 900-SYSUSER-READ-SEC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSUSER         =   1
               INITIALIZE                      SYSUSER-REC
               MOVE    SYS-1010-USERID     TO  SYSUSER-USERID
               MOVE    SPA-HOSPNUM         TO  SYSUSER-HOSPNUM
               MOVE    "F"                 TO  SYSUSER-SUPERVISOR
               MOVE    SMCNDATE-YMD        TO  SYSUSER-UPYMD
      *
               MOVE    SYSUSER-REC         TO  MCPDATA-REC
               MOVE    "tbl_sysuser"       TO  MCP-TABLE 
               MOVE    "key"               TO  MCP-PATHNAME 
               MOVE    "DBINSERT"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                    MOVE    "1002"              TO  SPA-ERRCD
                   DISPLAY "W06 DB DEL ERR USERID-KEY =>" SYSUSER-USERID
               END-IF
           END-IF
          .
       4905-SYSUSER-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W06    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  W06
      *
           MOVE    SPA-GMN-SYOKUKBN-G      TO  W06-SYOKUKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOKUKBN-MAX
               MOVE    SPA-GMN-SYOKUKBN-LIST (IDX)
                                           TO  W06-SYOKUKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SYOKUKBN-MAX        TO  W06-SYOKUKBN-COUNT
      *
           MOVE    SPA-GMN-SYOKUCOD-G      TO  W06-SYOKUCOD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOKUCOD-MAX
               MOVE    SPA-GMN-SYOKUCOD-LIST (IDX)
                                           TO  W06-SYOKUCOD-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SYOKUCOD-MAX        TO  W06-SYOKUCOD-COUNT
      *
           MOVE    SPA-GMN-STYUKYMD        TO  W06-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD        TO  W06-EDYUKYMD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TBL-MAX
               MOVE    SPA-GMN-TBANGO (IDX)
                                           TO  W06-TNO (IDX)
               MOVE    SPA-GMN-TSTYUKYMD (IDX)
                                           TO  W06-TSTYUKYMD (IDX)
               MOVE    SPA-GMN-TEDYUKYMD (IDX)
                                           TO  W06-TEDYUKYMD (IDX)
               IF      SPA-GMN-SELNUM      =   IDX
                   MOVE    "T"             TO  W06-KIKAN-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  W06-KIKAN-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-TBL-MAX         TO  W06-KIKAN-COUNT
           MOVE    SPA-GMN-SELNUM          TO  W06-SELNUM
      *
           MOVE    SPA-GMN-USERID          TO  W06-USERID
           MOVE    SPA-GMN-KANANAME        TO  W06-KANANAME
           MOVE    SPA-GMN-NAME            TO  W06-NAME  
           MOVE    SPA-GMN-ISEKINUM        TO  W06-ISEKINUM
           MOVE    SPA-GMN-HKNDRNUMS       TO  W06-HKNDRNUMS
           MOVE    SPA-GMN-HKNDRNUMK       TO  W06-HKNDRNUMK
           MOVE    SPA-GMN-MAYAKULICENSE   TO  W06-MAYAKULICENSE
      *
           MOVE  SPA-GMN-CHK(1)            TO  W06-CHK01
           MOVE  SPA-GMN-CHK(2)            TO  W06-CHK02
           MOVE  SPA-GMN-CHK(3)            TO  W06-CHK03
           MOVE  SPA-GMN-CHK(4)            TO  W06-CHK04
           MOVE  SPA-GMN-CHK(5)            TO  W06-CHK05
           MOVE  SPA-GMN-CHK(6)            TO  W06-CHK06
           MOVE  SPA-GMN-CHK(7)            TO  W06-CHK07
           MOVE  SPA-GMN-CHK(8)            TO  W06-CHK08
           MOVE  SPA-GMN-CHK(9)            TO  W06-CHK09
           MOVE  SPA-GMN-CHK(10)           TO  W06-CHK10
           MOVE  SPA-GMN-CHK(11)           TO  W06-CHK11
           MOVE  SPA-GMN-CHK(12)           TO  W06-CHK12
           MOVE  SPA-GMN-CHK(13)           TO  W06-CHK13
           MOVE  SPA-GMN-CHK(14)           TO  W06-CHK14
           MOVE  SPA-GMN-CHK(15)           TO  W06-CHK15
           MOVE  SPA-GMN-CHK(16)           TO  W06-CHK16
           MOVE  SPA-GMN-CHK(17)           TO  W06-CHK17
           MOVE  SPA-GMN-CHK(18)           TO  W06-CHK18
           MOVE  SPA-GMN-CHK(19)           TO  W06-CHK19
           MOVE  SPA-GMN-CHK(20)           TO  W06-CHK20
           MOVE  SPA-GMN-CHK(21)           TO  W06-CHK21
           MOVE  SPA-GMN-CHK(22)           TO  W06-CHK22
           MOVE  SPA-GMN-CHK(23)           TO  W06-CHK23
           MOVE  SPA-GMN-CHK(24)           TO  W06-CHK24
           MOVE  SPA-GMN-CHK(25)           TO  W06-CHK25
      *----(01.01.05)----START------------
           MOVE  SPA-GMN-CHK(26)           TO  W06-CHK26
      *----(01.01.05)----START------------
      *----(01.01.07)----START------------
           MOVE  SPA-GMN-CHK(27)           TO  W06-CHK27
           MOVE  SPA-GMN-CHK(28)           TO  W06-CHK28
           MOVE  SPA-GMN-CHK(29)           TO  W06-CHK29
           MOVE  SPA-GMN-CHK(30)           TO  W06-CHK30
           MOVE  SPA-GMN-CHK(31)           TO  W06-CHK31
           MOVE  SPA-GMN-CHK(32)           TO  W06-CHK32
      *----(01.01.07)----START------------
      *----(02.00.01)--ADD-START---
           MOVE  SPA-GMN-CHK(33)           TO  W06-CHK33
      *----(02.00.01)--ADD-END-----
      *----(02.00.02)--ADD-START---
           MOVE  SPA-GMN-CHK(34)           TO  W06-CHK34
      *----(02.00.02)--ADD-END-----
      *----(02.00.03)--ADD-START---
           MOVE  SPA-GMN-CHK(35)           TO  W06-CHK35
      *----(02.00.03)--ADD-END-----
           MOVE  SPA-GMN-CHK(36)           TO  W06-CHK36
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-SRYKA-MAX
                   MOVE    SPA-GMN-SEMONKACD-TBL (IDY) 
                                           TO  W06-SEMONKACD-ITEM
                                                        (IDX IDY)
               END-PERFORM
               MOVE    SPA-SRYKA-MAX       TO  W06-SEMONKACD-COUNT(IDX)
               MOVE    SPA-GMN-SEMONKACD-G(IDX)
                                           TO  W06-SEMONKACD(IDX)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KANRIAUTHFLG-MAX
               MOVE    SPA-GMN-KANRIAUTH-LST(IDX)
                                       TO  W06-KANRIAUTHFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-KANRIAUTHFLG-MAX  TO  W06-KANRIAUTHFLG-COUNT
           MOVE  SPA-GMN-KANRIAUTH-MEI TO  W06-KANRIAUTHFLG
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ORCA21FLG-MAX
               MOVE    SPA-GMN-ORCA21FLG-LST(IDX)
                                       TO  W06-ORCA21FLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-ORCA21FLG-MAX     TO  W06-ORCA21FLG-COUNT
           MOVE  SPA-GMN-ORCA21FLG-G   TO  W06-ORCA21FLG
      *
      *    請求書発行フラグ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SKYPRTFLG-MAX
               MOVE    SPA-GMN-SKYPRTFLG-LST(IDX)
                                       TO  W06-SKYPRTFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-SKYPRTFLG-MAX     TO  W06-SKYPRTFLG-COUNT
           MOVE  SPA-GMN-SKYPRTFLG-G   TO  W06-SKYPRTFLG
      *    院外処方せん発行フラグ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOHOPRTFLG-MAX
               MOVE    SPA-GMN-SYOHOPRTFLG-LST(IDX)
                                       TO  W06-SYOHOPRTFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-SYOHOPRTFLG-MAX     TO  W06-SYOHOPRTFLG-COUNT
           MOVE  SPA-GMN-SYOHOPRTFLG-G   TO  W06-SYOHOPRTFLG
      *    薬剤情報発行フラグ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YAKUJYOFLG-MAX
               MOVE    SPA-GMN-YAKUJYOFLG-LST(IDX)
                                       TO  W06-YAKUJYOFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-YAKUJYOFLG-MAX     TO  W06-YAKUJYOFLG-COUNT
           MOVE  SPA-GMN-YAKUJYOFLG-G   TO  W06-YAKUJYOFLG
      *    診療費明細書
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIPRTFLG-MAX
               MOVE    SPA-GMN-MEIPRTFLG-LST(IDX)
                                       TO  W06-MEIPRTFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-MEIPRTFLG-MAX     TO  W06-MEIPRTFLG-COUNT
           MOVE  SPA-GMN-MEIPRTFLG-G   TO  W06-MEIPRTFLG
      *    検査正式名称表示
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FORMALFLG-MAX
               MOVE    SPA-GMN-FORMALFLG-LST(IDX)
                                       TO  W06-FORMALFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-FORMALFLG-MAX     TO  W06-FORMALFLG-COUNT
           MOVE  SPA-GMN-FORMALFLG-G   TO  W06-FORMALFLG
      *    クライアント保存可否表示
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-CLIENT-SAVE-MAX
               MOVE    SPA-GMN-CLIENT-SAVE-LST(IDX)
                                       TO  W06-CLIENT-SAVE-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-CLIENT-SAVE-MAX   TO  W06-CLIENT-SAVE-COUNT
           MOVE  SPA-GMN-CLIENT-SAVE-G TO  W06-CLIENT-SAVE
      *    ユーザーログイン権限表示
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-USER-KANRI-MAX
               MOVE    SPA-GMN-USER-KANRI-LST(IDX)
                                       TO  W06-USER-KANRI-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-USER-KANRI-MAX    TO  W06-USER-KANRI-COUNT
           MOVE  SPA-GMN-USER-KANRI-G  TO  W06-USER-KANRI
      *
      *    他院情報設定画面
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL (     IDX     >   SPA-GMN-GRPHOSP-MAX )
               MOVE    SPA-GRPHOSPNUM (IDX)
                                       TO  W06-GRPHOSPNUM(IDX)
               MOVE    SPA-GRPHOSPNAME(IDX)
                                       TO  W06-GRPHOSPNAME(IDX)
               MOVE    SPA-GMN-GRPUSERID(IDX)
                                       TO  W06-GRPUSERID(IDX)
           END-PERFORM
      *
           MOVE    WIDGET-INSENSITIVE  TO  W06-TGN3-STATE
           IF    ( SPA-GRPHOSPKBN      =   1 )
               MOVE    WIDGET-NORMAL   TO  W06-TGN3-STATE
           END-IF
      *
           EVALUATE    SPA-GMN-PAGENO
               WHEN    1
                   MOVE    0           TO  W06-PAGENO
               WHEN    2
                   MOVE    1           TO  W06-PAGENO
               WHEN    3
                   MOVE    2           TO  W06-PAGENO
           END-EVALUATE
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-INSENSITIVE      TO  W06-CHK19-STATE
           MOVE    WIDGET-INSENSITIVE      TO  W06-CHK33-STATE
      *
           IF     (SPA-GMN-SYORI       =   1   ) AND
                  (SPA-GMN-SELNUM      =   ZERO)
               MOVE    WIDGET-INSENSITIVE  TO  W06-B03-STATE 
           ELSE
               MOVE    WIDGET-NORMAL       TO  W06-B03-STATE
           END-IF
      *    オルカマスターのみ削除不可能にする
           IF      (SPA-GMN-SYOKUKBN   =   "0"  )  AND
                   (SPA-GMN-SYOKUCOD   =   "0001")
               MOVE    WIDGET-INSENSITIVE      TO  W06-B03-STATE
           END-IF
      *
           IF      SPA-NAI-KANRIAUTHFLG    =  "T"
      *        管理者である時
               MOVE    WIDGET-NORMAL      TO  W06-CHK19-STATE
               MOVE    WIDGET-NORMAL      TO  W06-CHK33-STATE
           END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "職員区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "職員番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "有効開始日入力エラ−"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "有効終了日入力エラ−"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "有効開始日＜有効終了日で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "選択番号がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
               WHEN    "0008"
                   MOVE    "オペレータＩＤを入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "オペレータＩＤは半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0025"
                   STRING  "オペレータＩＤがシステム内に存在します。"
                           "オペレータＩＤを変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   MOVE    "フリガナは全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "氏名を全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "麻薬施用者免許証番号は、"
                                               TO  SPA-ERRMSG
                   MOVE    "半角か全角のどちらかで入力して下さい"
                                               TO  SPA-ERRMSG(25:)
               WHEN    "0013"
                   MOVE    "専門科コード入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "診療行為初期画面入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "９９９９は入力できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "請求書発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "院外処方せん発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "薬剤情報発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "診療費明細書発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "職員の選択がしてありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "検査正式名称表示フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "クライアント保存可否入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "他院オペレータＩＤは半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "ユーザーログイン権限入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "自院と他院のオペレータＩＤが重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "他院のオペレータＩＤが重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "職員情報が未登録です"
                                               TO  SPA-ERRMSG
               WHEN    "0100"
                   MOVE
                   "オルカマスターはシステム管理を有効にして下さい"
                                               TO  SPA-ERRMSG
               WHEN    "K001"
                   STRING    "警告！医師の職員番号がゼロです。"
                             "診療行為の複数科入力に使用できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W06"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
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
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "職員情報を追加登録します"
                                                     TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "職員情報を更新します"    TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "職員情報を削除します"    TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W06"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE  "SYOKUKBN"       TO   MCP-WIDGET
                WHEN    2
                    MOVE  "SYOKUCOD"       TO   MCP-WIDGET
                WHEN    3
                    MOVE  "STYUKYMD"       TO   MCP-WIDGET
                WHEN    4
                    MOVE  "EDYUKYMD"       TO   MCP-WIDGET
                WHEN    5
                    MOVE  "SELNUM"         TO   MCP-WIDGET
                WHEN    6
                    MOVE  "USERID"         TO   MCP-WIDGET
                WHEN    7
                    MOVE  "KANANAME"       TO   MCP-WIDGET
                WHEN    8
                    MOVE  "NAME"           TO   MCP-WIDGET
                WHEN    9
                    MOVE  "ISEKINUM"       TO   MCP-WIDGET
                WHEN    10
                    MOVE  "HKNDRNUMS"      TO   MCP-WIDGET
                WHEN    11
                    MOVE  "HKNDRNUMK"      TO   MCP-WIDGET
                WHEN    12
                    MOVE  "MAYAKULICENSE"  TO   MCP-WIDGET
                WHEN    13
                    MOVE  "SEMONKACD1"  TO   MCP-WIDGET
                WHEN    14
                    MOVE  "SEMONKACD2"  TO   MCP-WIDGET
                WHEN    15
                    MOVE  "SEMONKACD3"  TO   MCP-WIDGET
                WHEN    16
                    MOVE  "SEMONKACD4"  TO   MCP-WIDGET
                WHEN    17
                    MOVE  "SEMONKACD5"  TO   MCP-WIDGET
      *         業務権限
                WHEN    31   THRU  70
                    MOVE     SPA-GMN-CUR       TO  IDX2
                    MOVE     "CHK"             TO  MCP-WIDGET
                    COMPUTE  IDX2              =   IDX2   -   30
                    MOVE     IDX2              TO  MCP-WIDGET(4:2)
      *
                WHEN    18
                    MOVE  "KANRIAUTHFLG"   TO   MCP-WIDGET
                WHEN    19
                    MOVE  "ORCA21FLG"      TO   MCP-WIDGET
      *    個別設定画面
                WHEN    21
                    MOVE  "SKYPRTFLG"      TO   MCP-WIDGET
                WHEN    22
                    MOVE  "SYOHOPRTFLG"    TO   MCP-WIDGET
                WHEN    23
                    MOVE  "YAKUJYOFLG"     TO   MCP-WIDGET
                WHEN    24
                    MOVE  "MEIPRTFLG"      TO   MCP-WIDGET
                WHEN    25
                    MOVE  "FORMALFLG"      TO   MCP-WIDGET
                WHEN    26
                    MOVE  "CLIENT_SAVE"    TO   MCP-WIDGET
                WHEN    27
                    MOVE  "USER_KANRI"     TO   MCP-WIDGET
      *
                WHEN    99
                    MOVE  "B12"            TO   MCP-WIDGET
      *    他院設定画面
                WHEN    201
                    MOVE  "GRPUSERID01"    TO   MCP-WIDGET
                WHEN    202
                    MOVE  "GRPUSERID02"    TO   MCP-WIDGET
                WHEN    203
                    MOVE  "GRPUSERID03"    TO   MCP-WIDGET
                WHEN    204
                    MOVE  "GRPUSERID04"    TO   MCP-WIDGET
                WHEN    205
                    MOVE  "GRPUSERID05"    TO   MCP-WIDGET
                WHEN    206
                    MOVE  "GRPUSERID06"    TO   MCP-WIDGET
                WHEN    207
                    MOVE  "GRPUSERID07"    TO   MCP-WIDGET
                WHEN    208
                    MOVE  "GRPUSERID08"    TO   MCP-WIDGET
                WHEN    209
                    MOVE  "GRPUSERID09"    TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SYOKUKBN"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "SYOKUCOD"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "STYUKYMD"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "EDYUKYMD"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "SELNUM"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "USERID"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "KANANAME"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "NAME"
                   IF      SPA-GMN-SYOKUKBN    =   "1"
                       MOVE    9               TO  SPA-GMN-CUR
                   ELSE
                       MOVE    31              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "ISEKINUM"
                   MOVE    10              TO  SPA-GMN-CUR
               WHEN    "HKNDRNUMS" 
                   MOVE    11              TO  SPA-GMN-CUR
               WHEN    "HKNDRNUMK"
                   MOVE    12              TO  SPA-GMN-CUR
               WHEN    "MAYAKULICENSE"
                   MOVE    13              TO  SPA-GMN-CUR
               WHEN     "SEMONKACD1"
                   MOVE    14              TO  SPA-GMN-CUR
               WHEN     "SEMONKACD2"
                   MOVE    15              TO  SPA-GMN-CUR
               WHEN     "SEMONKACD3"
                   MOVE    16              TO  SPA-GMN-CUR
               WHEN     "SEMONKACD4"
                   MOVE    17              TO  SPA-GMN-CUR
               WHEN     "SEMONKACD5"
                   MOVE    31              TO  SPA-GMN-CUR
               WHEN     "KANRIAUTHFLG"
                   MOVE    19              TO  SPA-GMN-CUR
               WHEN     "ORCA21FLG"
                   MOVE    99              TO  SPA-GMN-CUR
      *    個別設定画面
               WHEN     "SKYPRTFLG"
                   MOVE    22              TO  SPA-GMN-CUR
               WHEN     "SYOHOPRTFLG"
                   MOVE    23              TO  SPA-GMN-CUR
               WHEN     "YAKUJYOFLG"
                   MOVE    24              TO  SPA-GMN-CUR
               WHEN     "MEIPRTFLG"
                   MOVE    25              TO  SPA-GMN-CUR
               WHEN     "FORMALFLG"
                   MOVE    26              TO  SPA-GMN-CUR
               WHEN     "CLIENT_SAVE"
                   MOVE    27              TO  SPA-GMN-CUR
               WHEN     "USER_KANRI"
                   MOVE    99              TO  SPA-GMN-CUR
      *    他院設定画面
               WHEN     "GRPUSERID01"  THRU    "GRPUSERID08"
                   MOVE    WRK-MCP-WIDGET  TO  WRK-GRPUSERID-WIDGET
                   IF    ( SPA-GRPHOSPNUM (WRK-GRPUSERIDNUM + 1)
                                           =   ZERO )
                       MOVE    99          TO  SPA-GMN-CUR
                   ELSE
                       COMPUTE SPA-GMN-CUR
                           =   WRK-GRPUSERIDNUM
                           +   201
                   END-IF
               WHEN     "GRPUSERID09"
                   MOVE    99              TO  SPA-GMN-CUR
      *
               WHEN     OTHER
                   IF      WRK-MCP-WIDGET(1:3) =   "CHK"
                       MOVE    WRK-MCP-WIDGET(4:2) TO  IDX2
                       COMPUTE SPA-GMN-CUR     =   IDX2
                                               +   31
      *----(02.00.01)--UPD-START---
      *                医事業務の後、プログラム更新へ
                       IF      IDX2            =   1
                           COMPUTE SPA-GMN-CUR     =   33
                                                   +   30
                       END-IF
      *                プログラム更新の後、外来まとめへ
                       IF      IDX2            =   33
                           MOVE    66          TO  SPA-GMN-CUR
                       END-IF
      *                外来まとめの後、受付へ
                       IF      IDX2            =   36
                           MOVE    32          TO  SPA-GMN-CUR
                       END-IF
      *                会計照会の後、入院へ
                       IF      IDX2            =   9
                           COMPUTE SPA-GMN-CUR     =   27
                                                   +   30
                       END-IF
      *                入院照会の後、日次統計へ
                       IF      IDX2            =   32
                           COMPUTE SPA-GMN-CUR     =   10
                                                   +   30
                       END-IF
      *----(02.00.02)--UPD-START---
      *                薬剤情報の後、住所マスタへ
                       IF      IDX2            =   26
                           COMPUTE SPA-GMN-CUR     =   34
                                                   +   30
                       END-IF
      *                ヘルプマスタの後、管理者へ
                       IF      IDX2            =   35
                           MOVE    18          TO  SPA-GMN-CUR
                       END-IF
      *----(02.00.01)--UPD-END-----
      *                管理者で無い時
                       IF      SPA-NAI-KANRIAUTHFLG    =  "T"
                           CONTINUE
                       ELSE
                           IF      SPA-GMN-CUR     =   63
                               MOVE    32          TO  SPA-GMN-CUR
                           ELSE
                               IF      SPA-GMN-CUR     =   49
                                   MOVE    50          TO  SPA-GMN-CUR
                               END-IF
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           EVALUATE    WRK-YMD(1:8)
               WHEN    ZERO
                   MOVE    ZERO                TO  WRK-SYMD
                   MOVE    WRK-YMD(1:8)        TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"               TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)          TO  WRK-HENYMDG  
               WHEN    OTHER
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-YMD             TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    ZERO
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    "99999999"
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    OTHER
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-EVALUATE
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-KEY-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
            .
       900-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-SYSUSER-READ-SEC            SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
               MOVE    ZERO            TO  FLG-SYSUSER
           ELSE
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           .
       900-SYSUSER-READ-EXT.
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
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
