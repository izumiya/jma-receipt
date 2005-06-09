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
       PROGRAM-ID.                               ORCGW11.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 出力先プリンタ名割り当て
      *  管理者            :
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   01/11/01  予約一覧を（７）へ追加
      * 01.00.02     MCC-藤原     02/01/11  タグにして、帳票編集区分情報
      *                                     の追加
      * 01.00.03     MCC-太田     02/03/11  一部の帳票ＰＧについて
      *                                     標準提供・独自作成のいずれか
      *                                     を選択できるよう修正
      * 01.00.03     MCC-多々納   02/03/15  後発医薬品名称を追加
      * 01.00.04     MCC-多々納   02/05/02  全角チェックを追加
      * 01.00.05     NACL-森脇    02/08/19  薬剤情報を追加
      * 01.00.06     NACL-太田    02/11/05  入院用オンライン帳票を追加
      * 02.00.01     NACL-多々納  02/11/13  一括入力
      * 02.00.02     NACL-太田    02/11/05  入院オンライン帳票を個別
      *                                     設定できるよう修正
      * 02.00.03     NACL-森脇    03/07/04  画面変更
      * 02.00.04     NACL-多々納  03/08/12  画面変更に伴う修正
      * 02.00.05     NACL-小豆沢  03/09/10  データチェックリストを（１１）
      *                                     に追加
      * 02.00.06     NACL-太田    04/01/29  標準提供請求書をコンボで選択
      *                                     可能とする
      * 02.00.07     NACL-多々納  04/08/11  処方せんの標準プログラム名を変更する
      * 02.00.08     NACL-森脇    04/11/05  支払証明書プログラム選択
      * 02.00.09     NACL-多々納  04/12/15  カルテ３号紙（続紙）、入院追加他
      * 02.00.10     NACL-多々納  05/02/16  入院指示せん追加
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
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W11.
           03  SPA-W11-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(04).
               05  SPA-GMN-CUR         PIC 9(04).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD    PIC X(09).
                   07  SPA-GMN-EDYUKYMD    PIC X(09).
      *
                   07  SPA-GMN-PAGENO      PIC 9(01).
      *            出力先プリンタ割り当て情報
                   07  SPA-GMN-1031-AREA.
      *----(02.00.03)--ADD-START---
                       09  SPA-GMN-USERKBN-G.
                           11  SPA-GMN-USERKBN     PIC X(01).
                           11  SPA-GMN-U1          PIC X(01).
                           11  SPA-GMN-USERKBNMEI  PIC X(18).
      *
                       09  SPA-GMN-USERKBNLST      OCCURS  5.
                           11  SPA-GMN-USERKBNL    PIC X(01).
                           11  SPA-GMN-U1L         PIC X(01).
                           11  SPA-GMN-USERKBNMEIL PIC X(18).
                       09  SPA-GMN-USERKBN-MAX     PIC 9(01).
      *                有効判定
                       09  SPA-GMN-YUKOKBN-G.
                           11  SPA-GMN-YUKOKBN     PIC X(01).
                           11  SPA-GMN-Y1          PIC X(01).
                           11  SPA-GMN-YUKOKBNMEI  PIC X(18).
      *
                       09  SPA-GMN-YUKOKBNLST      OCCURS  2.
                           11  SPA-GMN-YUKOKBNL    PIC X(01).
                           11  SPA-GMN-Y1L         PIC X(01).
                           11  SPA-GMN-YUKOKBNMEIL PIC X(18).
                       09  SPA-GMN-YUKOKBN-MAX     PIC 9(01).
      *
                       09  SPA-GMN-USERLSTSCROLLE.
                           11  SPA-GMN-USERLST         OCCURS  100.
                               13  SPA-GMN-USERNUML.
                                   15  SPA-GMN-UNUM    PIC X(03).
                                   15  SPA-GMN-UMARK   PIC X(01).
                               13  SPA-GMN-USERMEIL    PIC X(16).
                           11  SPA-GMN-USERLST-MAX     PIC 9(03).
      *
                       09  SPA-GMN-SELNUM          PIC 9(03).
      *----(02.00.03)--ADD-END-----
                       09  SPA-GMN-TBL.
      *----(01.00.03) LINE UPD START --------------------------
      *----(01.00.05) LINE UPD START --------------------------
      *                    外来・プリンタ名
                           11  SPA-GMN-TBL01       OCCURS  20.
                               13  SPA-GMN-TNAME   PIC X(30).
                               13  SPA-GMN-TPRTNM  PIC X(16).
      *----(02.00.03)--UPD-START---
      *----(02.00.08)--UPD-START---
      *                    外来・プログラム名
      *                     11  SPA-GMN-TBL02       OCCURS  5.
                           11  SPA-GMN-TBL02       OCCURS  7.
      *----(02.00.08)--UPD-END-----
                               13  SPA-GMN-TORGPGNM
                                                   PIC X(20).
                               13  SPA-GMN-TORGPGCHK
                                                   PIC X(01).
                               13  SPA-GMN-TCSTPGNM
                                                   PIC X(20).
                               13  SPA-GMN-TCSTPGCHK
                                                   PIC X(01).
                           11  SPA-GPGNM-MAX       PIC 9(04).
      *
                       09  SPA-GMN-ORGPGNM03LST-G.
                           11  SPA-GMN-ORGPGNM03MEIL  PIC X(20)
                                                   OCCURS  2.
                           11  SPA-GMN-ORGPGNM03-MAX
                                                   PIC 9(04).
      *----(02.00.03)--UPD-END-----
      *----(01.00.05) LINE UPD END   --------------------------
      *----(01.00.03) LINE UPD END   --------------------------
      *                    入院帳票
      *                    入院・プリンタ名
                           11  SPA-GMN-TBL01-N         OCCURS  20.
                               13  SPA-GMN-TNAME-N     PIC X(30).
                               13  SPA-GMN-TPRTNM-N    PIC X(16).
      *                    入院・プログラム名
                           11  SPA-GMN-TBL02-N         OCCURS  7.
                               13  SPA-GMN-TORGPGNM-N
                                                       PIC X(20).
                               13  SPA-GMN-TORGPGCHK-N
                                                       PIC X(01).
                               13  SPA-GMN-TCSTPGNM-N
                                                       PIC X(20).
                               13  SPA-GMN-TCSTPGCHK-N
                                                       PIC X(01).
                           11  SPA-NPGNM-MAX           PIC 9(04).
                           11  SPA-NPGNM-MAX2          PIC 9(04).
      *
                       09  SPA-GMN-ORGPGNM03NLST-G.
                           11  SPA-GMN-ORGPGNM03NMEIL PIC X(20)
                                                   OCCURS  2.
                           11  SPA-GMN-ORGPGNM03N-MAX
                                                   PIC 9(04).
      *
      *            帳票編集区分情報
                   07  SPA-GMN-1030-AREA.
                       09  SPA-GMN-SYOHOGEPRTFLG-G.
                           11  SPA-GMN-SYOHOGEPRTFLG
                                                  PIC X(01).
                           11  SPA-GMN-F1         PIC X(01).
                           11  SPA-GMN-SYOHOGEPRTFLGMEI
                                                  PIC X(10).
      *
                       09  SPA-GMN-SYOHOGEPRTFLGLST    OCCURS  2.
                           11  SPA-GMN-SYOHOGEPRTFLGL
                                              PIC X(01).
                           11  SPA-GMN-F1L        PIC X(01).
                           11  SPA-GMN-SYOHOGEPRTFLGMEIL
                                                  PIC X(10).
                       09  SPA-GMN-SYOHOGEPRTFLG-MAX
                                                  PIC 9(02).
      *
                       09  SPA-GMN-SYOHODSPNAME   PIC X(10).
      *                カルテ３号紙様式
                       09  SPA-GMN-KARUTE3KBN-G.
                           11  SPA-GMN-KARUTE3KBN      PIC X(01).
                           11  SPA-GMN-KARUTE3KBNF1    PIC X(01).
                           11  SPA-GMN-KARUTE3KBN-MEI     PIC X(18).
                       09  SPA-GMN-KARUTE3KBNLST-G  OCCURS  2.
                           11  SPA-GMN-KARUTE3KBNLST      PIC X(20).
                       09  SPA-GMN-KARUTE3KBN-MAX
                                                  PIC 9(02).
      *
      *----(02.00.03)--ADD-START---
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBLO           OCCURS  100.
                           11  SPA-NAI-USERCD     PIC 9(5).
      *
      *        出力先プリンタ割り当て 標準情報
               05  SPA-HYOJUN-AREA.
                   07  SPA-NAI-TBL01           OCCURS  20.
                       09  SPA-NAI-TNAME       PIC X(30).
                       09  SPA-NAI-TPRTNM      PIC X(16).
      *
      *----(02.00.08)--UPD-START---
      *             07  SPA-NAI-TBL02           OCCURS  5.
                   07  SPA-NAI-TBL02           OCCURS  7.
      *----(02.00.08)--UPD-END-----
                       09  SPA-NAI-TORGPGNM    PIC X(20).
                       09  SPA-NAI-TORGPGCHK   PIC X(01).
                       09  SPA-NAI-TCSTPGNM    PIC X(20).
                       09  SPA-NAI-TCSTPGCHK   PIC X(01).
      *
                   07  SPA-NAI-TBL01-N         OCCURS  20.
                       09  SPA-NAI-TNAME-N     PIC X(30).
                       09  SPA-NAI-TPRTNM-N    PIC X(16).
      *
                   07  SPA-NAI-TBL02-N         OCCURS  7.
                       09  SPA-NAI-TORGPGNM-N  PIC X(20).
                       09  SPA-NAI-TORGPGCHK-N PIC X(01).
                       09  SPA-NAI-TCSTPGNM-N  PIC X(20).
                       09  SPA-NAI-TCSTPGCHK-N PIC X(01).
      *----(02.00.03)--ADD-END-----
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
           03  FLG-SAI                             PIC 9(01).
      *
           03  FLG-CHK                             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDZ                               PIC 9(04).
           03  IDZ1                              PIC 9(04).
      *----(01.00.03) LINE ADD START --------------------------
           03  IDZ2                              PIC 9(04).
      *----(01.00.03) LINE ADD END   --------------------------
           03  IDX1                              PIC 9(04).
      *----(02.00.03)--ADD-START---
           03  IDX3                              PIC 9(03).
      *----(02.00.03)--ADD-END-----
           03  IDX2                              PIC 9(02).
           03  IDX4                              PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70).
      *----(01.00.03) LINE UPD START --------------------------
      *    03  WRK-KBNCD           PIC 9(01).
      *----(02.00.03)--UPD-START---
      *     03  WRK-KBNCD                PIC 9(08).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD1           PIC 9(05).
               05  WRK-KBNCD2           PIC 9(03).
      *
           03  WRK-DBPATH              PIC X(20).
           03  WRK-SELNO               PIC 9(03).
      *----(02.00.03)--UPD-END-----
      *----(01.00.03) LINE UPD END   --------------------------
           03  WRK-MAX-X.
               05  WRK-MAX         PIC 9(04).
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
      *----(01.00.03) LINE ADD START --------------------------
      *----(01.00.05) LINE UPD START --------------------------
      *----(02.00.03)--UPD-START---
      *----(02.00.08)--UPD-START---
      *     03  WRK-ORCAORGCHK-G  OCCURS   4.
      *     03  WRK-ORCAORGCHK-G  OCCURS   5.
           03  WRK-ORCAORGCHK-G  OCCURS   7.
               05  WRK-ORCAORGCHK  PIC X(01).
      *     03  WRK-ORCACSTCHK-G  OCCURS   4.
      *     03  WRK-ORCACSTCHK-G  OCCURS   5.
           03  WRK-ORCACSTCHK-G  OCCURS   7.
               05  WRK-ORCACSTCHK  PIC X(01).
           03  FLG-HENFLG          PIC 9(01).
      *----(02.00.08)--UPD-END-----
      *----(02.00.03)--UPD-END-----
      *----(01.00.05) LINE UPD END   --------------------------
      *----(01.00.03) LINE ADD END   --------------------------
      *
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-KANACHK-MAE-INPUT
                                   PIC X(400).
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
       01  WRK-WIDGET-AREA.
           03  WRK-WIDGET          PIC X(20).
           03  WRK-WIDGET-R10      REDEFINES  WRK-WIDGET.
               05  FILLER          PIC X(10).
               05  WRK-WIDGET10    PIC 9(02).
               05  FILLER          PIC X(08).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *----(02.00.03)--ADD-START---
      *    職員情報
           COPY  "CPSK1010.INC".
      *----(02.00.03)--ADD-END-----
      *----(01.00.03) LINE UPD START --------------------------
      *    出力先プリンタ名割り当て情報
      *    COPY  "CPSK1031.INC".
      *
      *    COPY  "CPSK1030.INC".
      *    帳票編集区分情報
           COPY  "CPSK1030.INC".
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *    帳票プログラム選択情報
           COPY  "CPSK1032.INC".
      *    出力先プリンタ名割り当て情報（入院）
           COPY  "CPSK1034.INC".
      *    帳票プログラム選択情報 （入院）
           COPY  "CPSK1035.INC".
      *----(01.00.03) LINE UPD END   --------------------------
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           COPY    "W12.INC".
           COPY    "W13.INC".
           COPY    "W14.INC".
           COPY    "W15.INC".
           COPY    "W16.INC".
           COPY    "W17.INC".
           COPY    "W18.INC".
           COPY    "W19.INC".
           COPY    "W20.INC".
           COPY    "W21.INC".
           COPY    "W22.INC".
           COPY    "W23.INC".
           COPY    "W24.INC".
           COPY    "W25.INC".
           COPY    "W26.INC".
           COPY    "W27.INC".
           COPY    "W28.INC".
           COPY    "W29.INC".
           COPY    "W30.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W98.INC".
           COPY    "W99.INC".
           COPY    "WERR.INC".
           COPY    "WID1.INC".
           COPY    "WID2.INC".
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W11
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W11         TO  SPA-FREE
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
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W11    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *
           INITIALIZE              SPA-W11-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      *
           EVALUATE    SPA-W01-KANRICD
      *
      *    帳票編集区分情報初期処理
           WHEN    "1030"
               PERFORM 3101-SYS1030-SYOKI-SEC
      *
      *    出力先プリンタ割り当て情報
           WHEN    "1031"
      *----(02.00.03)--UPD-START---
      *        端末区分初期処理
               PERFORM 3101-USERKBN-SYOKI-SEC
      *        出力先プリンタ割り当て情報
               PERFORM 3101-SYS1031-SYOKI-SEC
      *        標準情報セット
               PERFORM 3102-HYOUJYUN-SET-SEC
               MOVE    1                   TO  SPA-GMN-PAGENO
               MOVE    101                 TO  SPA-GMN-CUR
      *----(02.00.03)--UPD-END-----
           END-EVALUATE
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集区分情報初期処理
      *****************************************************************
       3101-SYS1030-SYOKI-SEC          SECTION.
      *
           MOVE    "0"                 TO  SPA-GMN-SYOHOGEPRTFLGL    (1)
           MOVE    "表示しない"        TO  SPA-GMN-SYOHOGEPRTFLGMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-SYOHOGEPRTFLGL    (2)
           MOVE    "表示する"          TO  SPA-GMN-SYOHOGEPRTFLGMEIL (2)
           MOVE    2                   TO  SPA-GMN-SYOHOGEPRTFLG-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-KARUTE3KBNLST(1)
           MOVE    "剤毎１行"          TO  SPA-GMN-KARUTE3KBNLST(1)(3:)
           MOVE    "1"                 TO  SPA-GMN-KARUTE3KBNLST(2)
           MOVE    "日毎１行"          TO  SPA-GMN-KARUTE3KBNLST(2)(3:)
           MOVE    2                   TO  SPA-GMN-KARUTE3KBN-MAX
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1030"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF  (   FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1030-REC
               MOVE    SYS-1030-SYOHOGEPRTFLG
                                       TO  SPA-GMN-SYOHOGEPRTFLG
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL ( IDY >   SPA-GMN-SYOHOGEPRTFLG-MAX )
                   IF    ( SPA-GMN-SYOHOGEPRTFLG
                                   =   SPA-GMN-SYOHOGEPRTFLGL (IDY) )
                       MOVE    SPA-GMN-SYOHOGEPRTFLGLST (IDY)
                                       TO  SPA-GMN-SYOHOGEPRTFLG-G
                       MOVE    SPA-GMN-SYOHOGEPRTFLG-MAX
                                       TO  IDY
                   END-IF
               END-PERFORM
      *        後発医薬品表示名
               MOVE    SYS-1030-SYOHODSPNAME
                                       TO  SPA-GMN-SYOHODSPNAME
      *        カルテ３号紙
               MOVE    SYS-1030-KARUTE3KBN
                                       TO  SPA-GMN-KARUTE3KBN
           ELSE
               MOVE    SPA-GMN-SYOHOGEPRTFLGL (1)
                                       TO  SPA-GMN-SYOHOGEPRTFLG
           END-IF
      *
           IF      SPA-GMN-KARUTE3KBN  =   "1"
               MOVE    SPA-GMN-KARUTE3KBNLST(2)
                                       TO  SPA-GMN-KARUTE3KBN-G
           ELSE
               MOVE    SPA-GMN-KARUTE3KBNLST(1)
                                       TO  SPA-GMN-KARUTE3KBN-G
           END-IF
      *
           MOVE    301                 TO  SPA-GMN-CUR
      *
           .
       3101-SYS1030-SYOKI-EXT.
           EXIT.
      *----(02.00.03)--ADD-START---
      *****************************************************************
      *    端末区分初期処理
      *****************************************************************
       3101-USERKBN-SYOKI-SEC          SECTION.
      *
      *    端末区分セット
           MOVE    "*"                 TO  SPA-GMN-USERKBNL(1)
           MOVE    "標準"              TO  SPA-GMN-USERKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-USERKBNL(2)
           MOVE    "医師"              TO  SPA-GMN-USERKBNMEIL(2)
           MOVE    "2"                 TO  SPA-GMN-USERKBNL(3)
           MOVE    "看護師"            TO  SPA-GMN-USERKBNMEIL(3)
           MOVE    "3"                 TO  SPA-GMN-USERKBNL(4)
           MOVE    "技師"              TO  SPA-GMN-USERKBNMEIL(4)
           MOVE    "4"                 TO  SPA-GMN-USERKBNL(5)
           MOVE    "事務職"            TO  SPA-GMN-USERKBNMEIL(5)
           MOVE    5                   TO  SPA-GMN-USERKBN-MAX
      *
      *    有効区分セット
           MOVE    "1"                 TO  SPA-GMN-YUKOKBNL(1)
           MOVE    "現在有効"          TO  SPA-GMN-YUKOKBNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-YUKOKBNL(2)
           MOVE    "現在無効"          TO  SPA-GMN-YUKOKBNMEIL(2)
           MOVE    2                   TO  SPA-GMN-YUKOKBN-MAX
      *
           MOVE    SPA-GMN-USERKBNL(1) TO  SPA-GMN-USERKBN
           MOVE    SPA-GMN-USERKBNMEIL(1)
                                       TO  SPA-GMN-USERKBNMEI
           MOVE    SPA-GMN-YUKOKBNL(1) TO  SPA-GMN-YUKOKBN
           MOVE    SPA-GMN-YUKOKBNMEIL(1)
                                       TO  SPA-GMN-YUKOKBNMEI
      *
      *    請求書兼領収書コンボ（外来）
           MOVE    "ORCHC03"           TO  SPA-GMN-ORGPGNM03MEIL(1)
           MOVE    "ORCHC03V02"        TO  SPA-GMN-ORGPGNM03MEIL(2)
           MOVE    2                   TO  SPA-GMN-ORGPGNM03-MAX
      *
      *    請求書兼領収書コンボ（入院）
           MOVE    "ORCHCN03"          TO  SPA-GMN-ORGPGNM03NMEIL(1)
           MOVE    "ORCHCN03V02"       TO  SPA-GMN-ORGPGNM03NMEIL(2)
           MOVE    2                   TO  SPA-GMN-ORGPGNM03N-MAX
      *
           INITIALIZE                      SPA-GMN-USERLSTSCROLLE
           INITIALIZE                      SPA-NAI-TBL
           MOVE    ZERO                TO  SPA-GMN-USERLST-MAX
           MOVE    ZERO                TO  SPA-GMN-SELNUM
      *
           MOVE    ZERO                TO  WRK-KBNCD-G
      *
      *    帳票名初期処理
           PERFORM 3101-PRTNAME-SYOKI-SEC
           PERFORM 3101-NYU-PRTNAME-SYOKI-SEC
      *
           .
       3101-USERKBN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    出力先プリンタ割り当て情報
      *****************************************************************
       3101-SYS1031-SYOKI-SEC          SECTION.
      *
      *    帳票名初期処理
      *****PERFORM 3101-PRTNAME-SYOKI-SEC
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1031"              TO  SYS-KANRICD
      *----(02.00.03)--UPD-START---
      *     MOVE    "*"                 TO  SYS-KBNCD
           IF      WRK-KBNCD1          =   ZERO
               MOVE    "*"                 TO  SYS-KBNCD
           ELSE
               MOVE    WRK-KBNCD1          TO  SYS-KBNCD(1:5)
           END-IF
      *----(02.00.03)--UPD-END-----
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1031-REC
               PERFORM VARYING IDX         FROM    1   BY  1
                       UNTIL   IDX         >       20
                   MOVE    SYS-1031-PRTNM (IDX)
                                               TO  SPA-GMN-TPRTNM (IDX)
               END-PERFORM
      *
               IF   ( SYS-1031-PRTNM (12)  =   SPACE )
                   MOVE    SYS-1031-PRTNM (9)
                                           TO  SPA-GMN-TPRTNM (12)
               END-IF
      *
           END-IF
      *
      *    外来プログラム名編集
           PERFORM 3103-PRGNAME-SYOKI-SEC
      *
           IF    ( SPA-BEDSU           >   ZERO )
               CONTINUE
           ELSE
               GO  TO  3101-SYS1031-SYOKI-EXT
           END-IF
      *
      *    -- 入院帳票 --
      *
      *    帳票名初期処理
      *****PERFORM 3101-NYU-PRTNAME-SYOKI-SEC
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1034"              TO  SYS-KANRICD
      *----(02.00.03)--UPD-START---
      *     MOVE    "*"                 TO  SYS-KBNCD
           IF      WRK-KBNCD1          =   ZERO
               MOVE    "*"                 TO  SYS-KBNCD
           ELSE
               MOVE    WRK-KBNCD1          TO  SYS-KBNCD
           END-IF
      *----(02.00.03)--UPD-END-----
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1034-REC
               PERFORM VARYING IDX         FROM    1   BY  1
                       UNTIL   IDX         >       20
                   MOVE    SYS-1034-PRTNM (IDX)
                                           TO  SPA-GMN-TPRTNM-N (IDX)
               END-PERFORM
           END-IF
      *
      *    入院プログラム名編集
           PERFORM 3103-NYU-PRGNAME-SYOKI-SEC
      *
           .
       3101-SYS1031-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票名初期処理
      *****************************************************************
       3101-PRTNAME-SYOKI-SEC          SECTION.
      *
           MOVE    "診療録（カルテ１号紙）"
                                       TO  SPA-GMN-TNAME (1)
           MOVE    "処方せん（院外）"  TO  SPA-GMN-TNAME (2)
           MOVE    "請求書兼領収書"    TO  SPA-GMN-TNAME (3)
           MOVE    "診療報酬明細書（レセプト）"
                                       TO  SPA-GMN-TNAME (4)
           MOVE    "総括表／保険請求書"
                                       TO  SPA-GMN-TNAME (5)
           MOVE    "各種統計表"        TO  SPA-GMN-TNAME (6)
           MOVE    "予約一覧／予約患者一覧"
                                       TO  SPA-GMN-TNAME (7)
           MOVE    "薬剤情報提供書"    TO  SPA-GMN-TNAME (8)
      *----(02.00.08)--UPD-START---
           MOVE    "支払証明書（日別）"
                                       TO  SPA-GMN-TNAME (9)
      *----(02.00.08)--UPD-END-----
      *----(02.00.03)--ADD-START---
           MOVE    "診療録（カルテ３号紙）"
                                       TO  SPA-GMN-TNAME (10)
      *----(02.00.03)--ADD-END-----
           MOVE    "データチェック"
                                       TO  SPA-GMN-TNAME (11)
      *----(02.00.08)--ADD-START---
           MOVE    "支払証明書（月別）"
                                       TO  SPA-GMN-TNAME (12)
      *----(02.00.08)--ADD-END-----
           MOVE    "診療録（カルテ３号紙）追記"
                                       TO  SPA-GMN-TNAME (13)
      *
      *    カルテ１号紙
           MOVE    "ORCHC01"       TO  SPA-GMN-TORGPGNM (1)
           MOVE    "T"             TO  SPA-GMN-TORGPGCHK(1)
           MOVE    "F"             TO  SPA-GMN-TCSTPGCHK(1)
      *    処方せん
           MOVE    "ORCHCM19"      TO  SPA-GMN-TORGPGNM (2)
           MOVE    "T"             TO  SPA-GMN-TORGPGCHK(2)
           MOVE    "F"             TO  SPA-GMN-TCSTPGCHK(2)
      *    請求書兼領収書
           MOVE    "ORCHC03"       TO  SPA-GMN-TORGPGNM (3)
           MOVE    "T"             TO  SPA-GMN-TORGPGCHK(3)
           MOVE    "F"             TO  SPA-GMN-TCSTPGCHK(3)
      *    薬剤情報提供書
           MOVE    "ORCHC30"       TO  SPA-GMN-TORGPGNM (4)
           MOVE    "T"             TO  SPA-GMN-TORGPGCHK(4)
           MOVE    "F"             TO  SPA-GMN-TCSTPGCHK(4)
      *----(02.00.08)--UPD-START---
      *    カルテ３号紙
           MOVE    "ORCHC33"       TO  SPA-GMN-TORGPGNM (5)
           MOVE    "T"             TO  SPA-GMN-TORGPGCHK(5)
           MOVE    "F"             TO  SPA-GMN-TCSTPGCHK(5)
      *    支払証明書（日別）
           MOVE    "ORCHCM31"      TO  SPA-GMN-TORGPGNM (6)
           MOVE    "T"             TO  SPA-GMN-TORGPGCHK(6)
           MOVE    "F"             TO  SPA-GMN-TCSTPGCHK(6)
      *    支払証明書（月別）
           MOVE    "ORCHCM32"      TO  SPA-GMN-TORGPGNM (7)
           MOVE    "T"             TO  SPA-GMN-TORGPGCHK(7)
           MOVE    "F"             TO  SPA-GMN-TCSTPGCHK(7)
      *    カスタマイズ帳票最大
      *     MOVE    5                   TO  SPA-GPGNM-MAX
           MOVE    7                   TO  SPA-GPGNM-MAX
      *----(02.00.08)--UPD-END-----
      *
      *
           .
       3101-PRTNAME-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    帳票名初期処理
      *****************************************************************
       3101-NYU-PRTNAME-SYOKI-SEC      SECTION.
      *
      **   入院帳票
           MOVE    "診療録（カルテ１号紙）"
                                       TO  SPA-GMN-TNAME-N (1)
           MOVE    "退院証明書"        TO  SPA-GMN-TNAME-N (2)
           MOVE    "請求書兼領収書"    TO  SPA-GMN-TNAME-N (3)
           MOVE    "診療録（カルテ３号紙）"
                                       TO  SPA-GMN-TNAME-N (4)
           MOVE    "診療録（カルテ３号紙）追記"
                                       TO  SPA-GMN-TNAME-N (5)
      *
           MOVE    "入院処方箋"        TO  SPA-GMN-TNAME-N (6)
           MOVE    "注射処方箋"        TO  SPA-GMN-TNAME-N (7)
           MOVE    "指示箋"            TO  SPA-GMN-TNAME-N (8)
      *
      *    カルテ１号紙
           MOVE    "ORCHCN01"          TO  SPA-GMN-TORGPGNM-N (1)
           MOVE    "T"                 TO  SPA-GMN-TORGPGCHK-N(1)
           MOVE    "F"                 TO  SPA-GMN-TCSTPGCHK-N(1)
      *    退院証明書
           MOVE    "ORCHCN02"          TO  SPA-GMN-TORGPGNM-N (2)
           MOVE    "T"                 TO  SPA-GMN-TORGPGCHK-N(2)
           MOVE    "F"                 TO  SPA-GMN-TCSTPGCHK-N(2)
      *    請求書兼領収書
           MOVE    "ORCHCN03"          TO  SPA-GMN-TORGPGNM-N (3)
           MOVE    "T"                 TO  SPA-GMN-TORGPGCHK-N(3)
           MOVE    "F"                 TO  SPA-GMN-TCSTPGCHK-N(3)
      *    カルテ３号紙
           MOVE    "ORCHCN33"          TO  SPA-GMN-TORGPGNM-N (4)
           MOVE    "T"                 TO  SPA-GMN-TORGPGCHK-N(4)
           MOVE    "F"                 TO  SPA-GMN-TCSTPGCHK-N(4)
      *    入院処方箋
           MOVE    "ORCHC501"          TO  SPA-GMN-TORGPGNM-N (5)
           MOVE    "T"                 TO  SPA-GMN-TORGPGCHK-N(5)
           MOVE    "F"                 TO  SPA-GMN-TCSTPGCHK-N(5)
      *    注射処方箋
           MOVE    "ORCHC502"          TO  SPA-GMN-TORGPGNM-N (6)
           MOVE    "T"                 TO  SPA-GMN-TORGPGCHK-N(6)
           MOVE    "F"                 TO  SPA-GMN-TCSTPGCHK-N(6)
      *    指示箋
           MOVE    "ORCHC503"          TO  SPA-GMN-TORGPGNM-N (7)
           MOVE    "T"                 TO  SPA-GMN-TORGPGCHK-N(7)
           MOVE    "F"                 TO  SPA-GMN-TCSTPGCHK-N(7)
      *    入院帳票
           MOVE    7                   TO  SPA-NPGNM-MAX
      *
           MOVE    4                   TO  SPA-NPGNM-MAX2
      *
           .
       3101-NYU-PRTNAME-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム名初期処理
      *****************************************************************
       3103-PRGNAME-SYOKI-SEC          SECTION.
      *
      *    帳票プログラム選択情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1032"              TO  SYS-KANRICD
      *----(02.00.03)--UPD-START---
      *     MOVE    "*"                 TO  SYS-KBNCD
      *    コードが５桁までゼロのものを対象とする
           MOVE    WRK-KBNCD1          TO  SYS-KBNCD(1:5)
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
      *----(02.00.03)--UPD-END-----
           PERFORM 900-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
      *        システム管理マスタ読込
               PERFORM 900-SYSKANRI-READ-SEC
           END-IF
      *
           PERFORM VARYING IDZ2        FROM    1   BY  1
                   UNTIL ( IDZ2        >   SPA-GPGNM-MAX)
                      OR ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  SYS-1032-REC
               MOVE    IDZ2            TO  WRK-KBNCD2
               IF      SYS-1032-KBNCD      =   WRK-KBNCD-G
      *
                   EVALUATE    IDZ2
                   WHEN    2
                       IF  (SYS-1032-ORCAORGNM
                                       NOT =  SPA-GMN-TORGPGNM  (2))
                           MOVE    "ORCHCM19"  TO  SPA-GMN-TORGPGNM (2)
                       END-IF
                   WHEN    6
                       IF  (SYS-1032-ORCAORGNM
                                       NOT =  SPA-GMN-TORGPGNM  (6))
                           MOVE    "ORCHCM31"
                                           TO  SPA-GMN-TORGPGNM (6)
                       END-IF
                   WHEN    7
                       IF  (SYS-1032-ORCAORGNM
                                       NOT =  SPA-GMN-TORGPGNM  (7))
                           MOVE    "ORCHCM32"
                                           TO  SPA-GMN-TORGPGNM (7)
                       END-IF
                   WHEN    OTHER
                           MOVE    SYS-1032-ORCAORGNM
                                           TO  SPA-GMN-TORGPGNM  (IDZ2)
                   END-EVALUATE
      *
                   MOVE    SYS-1032-ORCAORGCHK
                                       TO  SPA-GMN-TORGPGCHK (IDZ2)
                   MOVE    SYS-1032-ORCACSTNM
                                       TO  SPA-GMN-TCSTPGNM  (IDZ2)
                   MOVE    SYS-1032-ORCACSTCHK
                                       TO  SPA-GMN-TCSTPGCHK (IDZ2)
      *
      *            システム管理マスタ読込
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
      *
           END-PERFORM
      *
      *----(02.00.03)--UPD-END-----
      *
           PERFORM 900-SYSKANRI-CLOSE-SEC
           .
      *
       3103-PRGNAME-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入院帳票プログラム名初期処理
      *****************************************************************
       3103-NYU-PRGNAME-SYOKI-SEC      SECTION.
      *
      *    帳票プログラム選択情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1035"              TO  SYS-KANRICD
      *----(02.00.03)--UPD-START---
      *     MOVE    "*"                 TO  SYS-KBNCD
      *    コードが５桁までゼロのものを対象とする
           MOVE    WRK-KBNCD1          TO  SYS-KBNCD(1:5)
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
      *        システム管理マスタ読込
               PERFORM 900-SYSKANRI-READ-SEC
           END-IF
      *
      *----(02.00.03)--UPD-START---
           PERFORM VARYING IDZ2        FROM    1   BY  1
                   UNTIL ( IDZ2        >   SPA-NPGNM-MAX   )
                      OR ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  SYS-1035-REC
               MOVE    IDZ2            TO  WRK-KBNCD2
      *
               IF      SYS-1035-KBNCD       =   WRK-KBNCD-G
      *
                   MOVE    SYS-1035-ORCAORGNM
                                       TO  SPA-GMN-TORGPGNM-N  (IDZ2)
                   MOVE    SYS-1035-ORCAORGCHK
                                       TO  SPA-GMN-TORGPGCHK-N (IDZ2)
                   MOVE    SYS-1035-ORCACSTNM
                                       TO  SPA-GMN-TCSTPGNM-N  (IDZ2)
                   MOVE    SYS-1035-ORCACSTCHK
                                       TO  SPA-GMN-TCSTPGCHK-N (IDZ2)
                   IF      IDZ2        =   5
                       MOVE    "ORCHC501"      TO  SPA-GMN-TORGPGNM-N(5)
                   END-IF
                   IF      IDZ2        =   6
                       MOVE    "ORCHC502"      TO  SPA-GMN-TORGPGNM-N(6)
                   END-IF
                   IF      IDZ2        =   7
                       MOVE    "ORCHC503"      TO  SPA-GMN-TORGPGNM-N(7)
                   END-IF
      *
      *            システム管理マスタ読込
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
      *
           END-PERFORM
      *----(02.00.03)--UPD-END-----
      *
           PERFORM 900-SYSKANRI-CLOSE-SEC
           .
      *
       3103-NYU-PRGNAME-SYOKI-EXT.
           EXIT.
      *
      *----(02.00.03)--ADD-START---
      *****************************************************************
      *   標準項目セット処理
      *****************************************************************
       3102-HYOUJYUN-SET-SEC          SECTION.
      *
            INITIALIZE                 SPA-HYOJUN-AREA
      *     外来
            PERFORM VARYING IDX        FROM    1   BY  1
                    UNTIL   IDX        >   20
                MOVE    SPA-GMN-TNAME(IDX)
                                           TO  SPA-NAI-TNAME(IDX)
                MOVE    SPA-GMN-TPRTNM(IDX)
                                           TO  SPA-NAI-TPRTNM(IDX)
            END-PERFORM
      *
            PERFORM VARYING IDX        FROM    1   BY  1
                    UNTIL   IDX        >   SPA-GPGNM-MAX
                MOVE    SPA-GMN-TORGPGNM(IDX)
                                           TO  SPA-NAI-TORGPGNM(IDX)
                MOVE    SPA-GMN-TORGPGCHK(IDX)
                                           TO  SPA-NAI-TORGPGCHK(IDX)
                MOVE    SPA-GMN-TCSTPGNM(IDX)
                                           TO  SPA-NAI-TCSTPGNM(IDX)
                MOVE    SPA-GMN-TCSTPGCHK(IDX)
                                           TO  SPA-NAI-TCSTPGCHK(IDX)
            END-PERFORM
      *     入院
            PERFORM VARYING IDX        FROM    1   BY  1
                    UNTIL   IDX        >   20
                MOVE    SPA-GMN-TNAME-N(IDX)
                                           TO  SPA-NAI-TNAME-N(IDX)
                MOVE    SPA-GMN-TPRTNM-N(IDX)
                                           TO  SPA-NAI-TPRTNM-N(IDX)
            END-PERFORM
      *
            PERFORM VARYING IDX        FROM    1   BY  1
                    UNTIL   IDX        >   SPA-NPGNM-MAX
                MOVE    SPA-GMN-TORGPGNM-N(IDX)
                                           TO  SPA-NAI-TORGPGNM-N(IDX)
                MOVE    SPA-GMN-TORGPGCHK-N(IDX)
                                           TO  SPA-NAI-TORGPGCHK-N(IDX)
                MOVE    SPA-GMN-TCSTPGNM-N(IDX)
                                           TO  SPA-NAI-TCSTPGNM-N(IDX)
                MOVE    SPA-GMN-TCSTPGCHK-N(IDX)
                                           TO  SPA-NAI-TCSTPGCHK-N(IDX)
            END-PERFORM
      *
            .
       3102-HYOUJYUN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   比較処理
      *****************************************************************
       3102-HYOUJYUN-SAI-SET-SEC          SECTION.
      *
            MOVE    ZERO                TO  FLG-SAI
      *
            PERFORM VARYING IDX FROM    1   BY  1
                    UNTIL   IDX >       20
                IF      SPA-NAI-TPRTNM(IDX)
                                    NOT =   SPA-GMN-TPRTNM(IDX)
                    MOVE    1           TO  FLG-SAI
                END-IF
            END-PERFORM
      *
            PERFORM VARYING IDX FROM    1   BY  1
                    UNTIL   IDX >       SPA-GPGNM-MAX
                IF     (SPA-NAI-TORGPGNM(IDX)
                                        =   SPA-GMN-TORGPGNM (IDX)) AND
                       (SPA-NAI-TORGPGCHK(IDX)
                                        =   SPA-GMN-TORGPGCHK(IDX)) AND
                       (SPA-NAI-TCSTPGNM(IDX)
                                        =   SPA-GMN-TCSTPGNM (IDX)) AND
                       (SPA-NAI-TCSTPGCHK(IDX)
                                        =   SPA-GMN-TCSTPGCHK(IDX))
                    CONTINUE
                ELSE
                    MOVE    1           TO  FLG-SAI
                END-IF
            END-PERFORM
      *
            PERFORM VARYING IDX FROM    1   BY  1
                    UNTIL   IDX >       20
                IF      SPA-NAI-TPRTNM-N(IDX)
                                    NOT =   SPA-GMN-TPRTNM-N(IDX)
                    MOVE    1           TO  FLG-SAI
                END-IF
            END-PERFORM
      *
            PERFORM VARYING IDX FROM    1   BY  1
                    UNTIL   IDX >       SPA-NPGNM-MAX
               IF      (SPA-NAI-TORGPGNM-N(IDX)
                                        =   SPA-GMN-TORGPGNM-N(IDX) )
                  AND  (SPA-NAI-TORGPGCHK-N(IDX)
                                        =   SPA-GMN-TORGPGCHK-N(IDX))
                  AND  (SPA-NAI-TCSTPGNM-N(IDX)
                                        =   SPA-GMN-TCSTPGNM-N(IDX) )
                  AND  (SPA-NAI-TCSTPGCHK-N(IDX)
                                        =   SPA-GMN-TCSTPGCHK-N(IDX))
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SAI
               END-IF
            END-PERFORM
      *
            .
       3102-HYOUJYUN-SAI-EXT.
           EXIT.
      *
      *****************************************************************
      *   標準項目画面セット処理
      *****************************************************************
       3102-HYOUJYUN-GMN-SET-SEC          SECTION.
      *
            PERFORM VARYING IDX FROM    1   BY  1
                    UNTIL   IDX >       20
                MOVE    SPA-NAI-TNAME(IDX)
                                    TO  SPA-GMN-TNAME(IDX)
                MOVE    SPA-NAI-TPRTNM(IDX)
                                    TO  SPA-GMN-TPRTNM(IDX)
            END-PERFORM
      *
            PERFORM VARYING IDX FROM    1   BY  1
                    UNTIL   IDX >       SPA-GPGNM-MAX
                MOVE    SPA-NAI-TORGPGNM(IDX)
                                    TO  SPA-GMN-TORGPGNM(IDX)
                MOVE    SPA-NAI-TORGPGCHK(IDX)
                                    TO  SPA-GMN-TORGPGCHK(IDX)
                MOVE    SPA-NAI-TCSTPGNM(IDX)
                                    TO  SPA-GMN-TCSTPGNM(IDX)
                MOVE    SPA-NAI-TCSTPGCHK(IDX)
                                    TO  SPA-GMN-TCSTPGCHK(IDX)
            END-PERFORM
      *
            PERFORM VARYING IDX FROM    1   BY  1
                    UNTIL   IDX >       20
                MOVE    SPA-NAI-TNAME-N(IDX)
                                    TO  SPA-GMN-TNAME-N(IDX)
                MOVE    SPA-NAI-TPRTNM-N(IDX)
                                    TO  SPA-GMN-TPRTNM-N(IDX)
            END-PERFORM
      *
            PERFORM VARYING IDX FROM    1   BY  1
                    UNTIL   IDX >       SPA-NPGNM-MAX
                MOVE    SPA-NAI-TORGPGNM-N(IDX)
                                    TO  SPA-GMN-TORGPGNM-N(IDX)
                MOVE    SPA-NAI-TORGPGCHK-N(IDX)
                                    TO  SPA-GMN-TORGPGCHK-N(IDX)
                MOVE    SPA-NAI-TCSTPGNM-N(IDX)
                                    TO  SPA-GMN-TCSTPGNM-N(IDX)
                MOVE    SPA-NAI-TCSTPGCHK-N(IDX)
                                    TO  SPA-GMN-TCSTPGCHK-N(IDX)
            END-PERFORM
      *
           .
       3102-HYOUJYUN-GMN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    端末名初期処理
      *****************************************************************
       3103-USERID-SYOKI-SEC          SECTION.
      *
      *    端末名初期値セット（職員情報）
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1010"              TO  SYS-KANRICD
           MOVE    SPA-GMN-USERKBN     TO  SYS-KBNCD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
           MOVE    "SYSKANRI-KEY4"     TO  WRK-DBPATH
           PERFORM 900-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO 
      *        システム管理マスタ読込
               PERFORM 900-SYSKANRI-READ-SEC
           END-IF
      *
           MOVE    ZERO                 TO  IDX3
      *
           PERFORM UNTIL ( IDX3            >=  100   )
                      OR ( FLG-SYSKANRI    NOT =   ZERO )
               EVALUATE    SPA-GMN-YUKOKBN
               WHEN    "1"
                   MOVE    MCPDATA-REC     TO  SYS-1010-REC
                   IF          SYS-1010-STYUKYMD   <=  SPA-W01-STYUKYMD
                       AND     SYS-1010-EDYUKYMD   >=  SPA-W01-EDYUKYMD
                       ADD     1               TO  IDX3
                       MOVE    IDX3            TO  SPA-GMN-UNUM(IDX3)
                       MOVE    SYS-1010-USERID
                                               TO  SPA-GMN-USERMEIL
                                                                 (IDX3)
                       MOVE    SYS-1010-KBNCD  TO  SPA-NAI-USERCD(IDX3)
                   END-IF
               WHEN    "2"
                   MOVE    MCPDATA-REC     TO  SYS-1010-REC
                   IF          SYS-1010-STYUKYMD   >   SPA-W01-STYUKYMD
                       OR      SYS-1010-EDYUKYMD   <   SPA-W01-EDYUKYMD
                       ADD     1               TO  IDX3
                       MOVE    IDX3            TO  SPA-GMN-UNUM(IDX3)
                       MOVE    SYS-1010-USERID
                                               TO  SPA-GMN-USERMEIL
                                                                 (IDX3)
                       MOVE    SYS-1010-KBNCD  TO  SPA-NAI-USERCD(IDX3)
                   END-IF
               END-EVALUATE
      *
      *        システム管理マスタ読込
               PERFORM 900-SYSKANRI-READ-SEC
      *
           END-PERFORM
      *
           PERFORM 900-SYSKANRI-CLOSE-SEC
      *
           MOVE    IDX3                TO  SPA-GMN-USERLST-MAX
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >   SPA-GMN-USERLST-MAX
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1031"              TO  SYS-KANRICD
               MOVE    SPA-NAI-USERCD(IDX3)
                                           TO  SYS-KBNCD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI            =   ZERO
                   MOVE    "*"         TO  SPA-GMN-UMARK(IDX3)
               ELSE
                   MOVE    SPACE               TO  SYSKANRI-REC
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    "1032"              TO  SYS-KANRICD
                   MOVE    SPA-NAI-USERCD(IDX3)
                                               TO  SYS-KBNCD
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI            =   ZERO
                       MOVE    "*"         TO  SPA-GMN-UMARK(IDX3)
                   END-IF
               END-IF
      *
               IF        ( SPA-BEDSU   >   ZERO )
                   AND   ( SPA-GMN-UMARK(IDX3)     NOT =   "*")
      *
                   MOVE    SPACE               TO  SYSKANRI-REC
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    "1034"              TO  SYS-KANRICD
                   MOVE    SPA-NAI-USERCD(IDX3)
                                               TO  SYS-KBNCD
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI            =   ZERO
                       MOVE    "*"         TO  SPA-GMN-UMARK(IDX3)
                   ELSE
                       MOVE    SPACE               TO  SYSKANRI-REC
                       INITIALIZE                      SYSKANRI-REC
                       MOVE    "1035"              TO  SYS-KANRICD
                       MOVE    SPA-NAI-USERCD(IDX3)
                                                   TO  SYS-KBNCD
                       MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                       MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
                       PERFORM 900-SYSKANRI-INV-SEC
                       IF      FLG-SYSKANRI            =   ZERO
                           MOVE    "*"         TO  SPA-GMN-UMARK(IDX3)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       3103-USERID-SYOKI-EXT.
           EXIT.
      *----(02.00.03)--ADD-END-----
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
      *----(02.00.03)--UPD-START---
      *         PERFORM 490-KOUSHIN-SEC
               EVALUATE    SPA-WIDCD
                   WHEN    "1001"
                       PERFORM 490-KOUSHIN-SEC
                   WHEN    "1002"
                       PERFORM 490-SAKUJYO-SEC
               END-EVALUATE
           ELSE
               EVALUATE    SPA-W01-KANRICD
                   WHEN    "1031"
                       MOVE    101                  TO  SPA-GMN-CUR
                   WHEN    "1030"
                       MOVE    301                 TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *----(02.00.03)--UPD-END-----
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    標準プログラム選択時
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK01"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK02"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK03"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK04"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK05"
      *----(02.00.08)--ADD-START---
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK06"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK07"
      *----(02.00.08)--ADD-START---
               MOVE    MCP-WIDGET  TO      WRK-WIDGET
               PERFORM  4101-ORCAORGCHK-CHK-SEC
      *    カスタマイズプログラム選択時
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK01"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK02"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK03"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK04"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK05"
      *----(02.00.08)--ADD-START---
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK06"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK07"
      *----(02.00.08)--ADD-START---
               MOVE    MCP-WIDGET  TO      WRK-WIDGET
               PERFORM  4101-ORCACSTCHK-CHK-SEC
      *    標準プログラム選択時 ＝ 入院 ＝
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK01N"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK02N"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK03N"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK04N"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK05N"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK06N"
           WHEN    "CLICKED"       ALSO    "ORCAORGCHK07N"
               MOVE    MCP-WIDGET  TO      WRK-WIDGET
               PERFORM  4101-NYU-ORCAORGCHK-CHK-SEC
      *    カスタマイズプログラム選択時 ＝ 入院 ＝
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK01N"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK02N"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK03N"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK04N"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK05N"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK06N"
           WHEN    "CLICKED"       ALSO    "ORCACSTCHK07N"
               MOVE    MCP-WIDGET  TO      WRK-WIDGET
               PERFORM  4101-NYU-ORCACSTCHK-CHK-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM  410-KAKUTEI-SEC
      *----(02.00.03)--ADD-START---
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM  410-SAKUJYO-SEC
      *----(02.00.03)--ADD-END-----
      *
      *    タグ切替え
           WHEN    "CLICKED"       ALSO    "N0101"
           WHEN    "CLICKED"       ALSO    "N0102"
               PERFORM 460-TAGUCHG-SEC
      *
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                        SECTION.
      *
           MOVE    MCP-WIDGET           TO  WRK-MCP-WIDGET
           MOVE    ZERO                 TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT        ALSO    MCP-WIDGET
      *        端末区分
               WHEN    "ACTIVATE"       ALSO    "USERKBN"
                   PERFORM  4103-USERKBN-CHK-SEC
      *        有効区分
               WHEN    "ACTIVATE"       ALSO    "YUKOKBN"
                   PERFORM  4104-YUKOKBN-CHK-SEC
      *        選択番号
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM  4101-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY              ALSO    "USERLST"
                   PERFORM  4105-USERLST-SEC
      *        一括入力チェック
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *----(02.00.03)--ADD-END-----
      *
      *    入力チェック処理
      *     PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
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
      *    出力先プリンタ割り当て
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               MOVE    W11-PRTNM (IDX)     TO  SPA-GMN-TPRTNM (IDX)
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GPGNM-MAX
               MOVE    W11-ORCACSTNM (IDX)
                                           TO  SPA-GMN-TCSTPGNM (IDX)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               MOVE    W11-PRTNM-N (IDX)   TO  SPA-GMN-TPRTNM-N (IDX)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NPGNM-MAX2
               MOVE    W11-ORCACSTNM-N (IDX)
                                           TO  SPA-GMN-TCSTPGNM-N (IDX)
           END-PERFORM
           MOVE    W11-ORCACSTNM05N    TO  SPA-GMN-TCSTPGNM-N (5)
           MOVE    W11-ORCACSTNM06N    TO  SPA-GMN-TCSTPGNM-N (6)
           MOVE    W11-ORCACSTNM07N    TO  SPA-GMN-TCSTPGNM-N (7)
      *
           MOVE    W11-ORCAORGNM03     TO  SPA-GMN-TORGPGNM (03)
           MOVE    W11-ORCAORGNM-N03   TO  SPA-GMN-TORGPGNM-N (03)
      *
      *    帳票編集区分
           MOVE    W11-SYOHOGEPRTFLG   TO  SPA-GMN-SYOHOGEPRTFLG-G
           MOVE    W11-SYOHODSPNAME    TO  SPA-GMN-SYOHODSPNAME
           MOVE    W11-KARUTE3KBN      TO  SPA-GMN-KARUTE3KBN-G
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
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1031"
      *            標準以外の時、選択があること
                   PERFORM 4100-HYOJUN-CHK-SEC
      *            出力先プリンタ割り当て
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4101-PRTNM-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4101-ORGPGNM-SET-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4101-CSTPGNM-SET-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4101-NYU-PRTNM-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4101-NYU-ORGPGNM-SET-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4101-NYU-CSTPGNM-SET-SEC
                   END-IF
               WHEN    "1030"
      *            帳票編集区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4100-SYOHOGEPRTFLG-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4200-SYOHODSPNAME-CHK-SEC
                   END-IF
      *            カルテ３号
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4300-KARUTE3KBN-CHK-SEC
                   END-IF
           END-EVALUATE
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択チェック処理
      *****************************************************************
       4100-HYOJUN-CHK-SEC        SECTION.
      *
      *    標準以外の時、選択があること
           IF      SPA-GMN-USERKBN     =   "*"
               CONTINUE
           ELSE
               IF      SPA-GMN-SELNUM      =   ZERO
                   MOVE    "0007"              TO  SPA-ERRCD
                   MOVE    103                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4100-HYOJUN-CHK-EXT.
           EXIT.
      *
      *----(02.00.03)--ADD-START---
      *****************************************************************
      *    端末区分　入力処理
      *****************************************************************
       4103-USERKBN-CHK-SEC        SECTION.
      *
           MOVE    W11-USERKBN     TO  SPA-GMN-USERKBN-G
      *
           IF      SPA-GMN-USERKBN     =   SPACE
               MOVE    101             TO  SPA-GMN-CUR
               MOVE    "0011"          TO  SPA-ERRCD
               GO  TO  4103-USERKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-HENFLG
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-USERKBN-MAX )  OR
                          (FLG-HENFLG  =   1    )
               IF      SPA-GMN-USERKBN     =   SPA-GMN-USERKBNL (IDY)
                   MOVE    SPA-GMN-USERKBNLST (IDY)
                                           TO  SPA-GMN-USERKBN-G
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
           IF      FLG-HENFLG          =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    101                 TO  SPA-GMN-CUR
               GO  TO  4103-USERKBN-CHK-EXT
           END-IF
      *
      *    外来を表示する
           MOVE    1                   TO  SPA-GMN-PAGENO
      *
           EVALUATE    SPA-GMN-USERKBN
           WHEN    "*"
               MOVE    SPA-GMN-YUKOKBNL(1) TO  SPA-GMN-YUKOKBN
               MOVE    SPA-GMN-YUKOKBNMEIL(1)
                                           TO  SPA-GMN-YUKOKBNMEI
               INITIALIZE                      SPA-GMN-USERLSTSCROLLE
               INITIALIZE                      SPA-NAI-TBL
               MOVE    ZERO                TO  SPA-GMN-USERLST-MAX
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               MOVE    1                   TO  SPA-GMN-CUR
               PERFORM 3102-HYOUJYUN-GMN-SET-SEC
           WHEN    OTHER
               INITIALIZE                      SPA-GMN-USERLSTSCROLLE
               INITIALIZE                      SPA-NAI-TBL
               MOVE    ZERO                TO  SPA-GMN-USERLST-MAX
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               MOVE    103                 TO  SPA-GMN-CUR
               PERFORM 3103-USERID-SYOKI-SEC
           END-EVALUATE
      *
           .
       4103-USERKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効区分　入力処理
      *****************************************************************
       4104-YUKOKBN-CHK-SEC        SECTION.
      *
      *
           MOVE    W11-YUKOKBN     TO  SPA-GMN-YUKOKBN-G
      *
           IF      SPA-GMN-YUKOKBN     =   SPACE
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    102                 TO  SPA-GMN-CUR
               GO  TO  4104-YUKOKBN-CHK-EXT
           ELSE
               MOVE    ZERO               TO  FLG-HENFLG
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL  (IDY         >   SPA-GMN-YUKOKBN-MAX )
                       OR     (FLG-HENFLG  =   1    )
                   IF      SPA-GMN-YUKOKBN =   SPA-GMN-YUKOKBNL (IDY)
                       MOVE    SPA-GMN-YUKOKBNLST (IDY)
                                               TO  SPA-GMN-YUKOKBN-G
                       MOVE    1               TO  FLG-HENFLG
                   END-IF
               END-PERFORM
               IF      FLG-HENFLG         =   ZERO
                   MOVE    "0011"                  TO  SPA-ERRCD
                   MOVE    102                 TO  SPA-GMN-CUR
                   GO  TO  4104-YUKOKBN-CHK-EXT
               END-IF
           END-IF
      *    外来を表示する
           MOVE    1                   TO  SPA-GMN-PAGENO
      *
           INITIALIZE                      SPA-GMN-USERLSTSCROLLE
           INITIALIZE                      SPA-NAI-TBL
           MOVE    ZERO                TO  SPA-GMN-USERLST-MAX
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    103                 TO  SPA-GMN-CUR
           PERFORM 3103-USERID-SYOKI-SEC
           .
       4104-YUKOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       4101-SELNUM-CHK-SEC       SECTION.
      *
           MOVE    W11-SELNUM          TO  SPA-GMN-SELNUM
      *
      *    選択番号チェック
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    103                 TO  SPA-GMN-CUR
               GO  TO  4101-SELNUM-CHK-EXT 
           ELSE
               IF     (SPA-GMN-SELNUM      >=  1    )
               AND    (SPA-GMN-SELNUM      <=  SPA-GMN-USERLST-MAX)
                   CONTINUE 
               ELSE
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    103                 TO  SPA-GMN-CUR
                   GO  TO  4101-SELNUM-CHK-EXT 
               END-IF
           END-IF   
      *    外来を表示する
           MOVE    1                   TO  SPA-GMN-PAGENO
      *
           PERFORM 3102-HYOUJYUN-GMN-SET-SEC
      *
           IF      SPA-GMN-UMARK(SPA-GMN-SELNUM)   =   "*"
               MOVE    ZERO                TO  WRK-KBNCD-G
               MOVE    SPA-NAI-USERCD(SPA-GMN-SELNUM)
                                           TO  WRK-KBNCD1
               PERFORM 3101-SYS1031-SYOKI-SEC
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR 
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    端末区分　入力処理
      *****************************************************************
       4105-USERLST-SEC        SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNO
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-USERLST-MAX
               IF      W11-USERLST-SELECT(IDX) =   "T"
                   IF      IDX                 =   SPA-GMN-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX             TO  WRK-SELNO
                       MOVE    SPA-GMN-USERLST-MAX TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-GMN-SELNUM 
           END-IF
      *
      *    外来を表示する
           MOVE    1                   TO  SPA-GMN-PAGENO
           PERFORM 3102-HYOUJYUN-GMN-SET-SEC
      *
           IF      SPA-GMN-UMARK(SPA-GMN-SELNUM)  =   "*"
               MOVE    SPA-NAI-USERCD(SPA-GMN-SELNUM)   TO  WRK-KBNCD1
               PERFORM 3101-SYS1031-SYOKI-SEC
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR 
      *
           .
       4105-USERLST-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W11"               TO  SPA-MOTOPG
      *
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
      *    処方せんＧＥ表示区分チェック
      *****************************************************************
       4100-SYOHOGEPRTFLG-CHK-SEC       SECTION.
      *
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-SYOHOGEPRTFLG-MAX
               IF      SPA-GMN-SYOHOGEPRTFLG
                                   =   SPA-GMN-SYOHOGEPRTFLGL (IDY)
                   MOVE    SPA-GMN-SYOHOGEPRTFLGLST (IDY)
                                           TO  SPA-GMN-SYOHOGEPRTFLG-G
                   MOVE    SPA-GMN-SYOHOGEPRTFLG-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-SYOHOGEPRTFLGMEI    =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    301                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-SYOHOGEPRTFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    後発医薬品表示名チェック
      *****************************************************************
       4200-SYOHODSPNAME-CHK-SEC       SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SYOHODSPNAME    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *        全角以外エラー
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    302                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       4200-SYOHODSPNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ３号紙内容チェック
      *****************************************************************
       4300-KARUTE3KBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-KARUTE3KBN-MAX
               IF      SPA-GMN-KARUTE3KBN
                                   =   SPA-GMN-KARUTE3KBNLST (IDY)(1:1)
                   MOVE    SPA-GMN-KARUTE3KBNLST (IDY)
                                           TO  SPA-GMN-KARUTE3KBN-G
                   MOVE    SPA-GMN-KARUTE3KBN-MAX
                                           TO  IDY
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    303                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       4300-KARUTE3KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　出力先プリンタ名チェック
      *****************************************************************
       4101-PRTNM-CHK-SEC                         SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   20  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-GMN-TNAME (IDX)     =   SPACE )
               AND   ( SPA-GMN-TPRTNM(IDX) NOT =   SPACE )
                  MOVE     SPACE       TO  SPA-GMN-TPRTNM(IDX)
               END-IF
      *
               MOVE    SPA-GMN-TPRTNM(IDX)
                                       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-ENRTRY-CHK-SEC
               IF      FLG-ENTRYCHK    NOT =   1
                   MOVE    IDX             TO  SPA-GMN-CUR
                   MOVE    "0006"          TO  SPA-ERRCD
               END-IF
      *
           END-PERFORM
      *
           .
       4101-PRTNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　標準プログラム名設定処理
      *****************************************************************
       4101-ORGPGNM-SET-SEC            SECTION.
      *
      *    請求書兼領収書（外来）
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   SPA-GMN-ORGPGNM03-MAX )
                    OR   ( SPA-GMN-ORGPGNM03MEIL (IDX4)
                                   =   SPA-GMN-TORGPGNM (3) )
               CONTINUE
           END-PERFORM
      *
           IF    ( IDX4    >   SPA-GMN-ORGPGNM03-MAX )
               MOVE    SPA-GMN-ORGPGNM03MEIL (1)
                                   TO  SPA-GMN-TORGPGNM (3)
           END-IF
      *
           .
      *
       4101-ORGPGNM-SET-EXT.
           EXIT.
      *****************************************************************
      *  　カスタマイズプログラム名設定処理
      *****************************************************************
       4101-CSTPGNM-SET-SEC            SECTION.
      *
           .
      *
       4101-CSTPGNM-SET-EXT.
           EXIT.
      *****************************************************************
      *  　出力先プリンタ名チェック
      *****************************************************************
       4101-NYU-PRTNM-CHK-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   20  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-GMN-TNAME-N (IDX)      =   SPACE )
               AND    (SPA-GMN-TPRTNM-N (IDX) NOT =   SPACE )
                   MOVE    SPACE       TO SPA-GMN-TPRTNM-N (IDX)
               END-IF
      *
               MOVE    SPA-GMN-TPRTNM-N(IDX)
                                       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-ENRTRY-CHK-SEC
               IF      FLG-ENTRYCHK    NOT =   1
                   COMPUTE SPA-GMN-CUR     =   IDX     +   50
                   MOVE    "0006"          TO  SPA-ERRCD
               END-IF
      *
           END-PERFORM
      *
           .
       4101-NYU-PRTNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　標準プログラム名設定処理
      *****************************************************************
       4101-NYU-ORGPGNM-SET-SEC        SECTION.
      *
      *    請求書兼領収書（入院）
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   SPA-GMN-ORGPGNM03N-MAX )
                    OR   ( SPA-GMN-ORGPGNM03NMEIL (IDX4)
                                   =   SPA-GMN-TORGPGNM-N (3) )
               CONTINUE
           END-PERFORM
      *
           IF    ( IDX4    >   SPA-GMN-ORGPGNM03N-MAX )
               MOVE    SPA-GMN-ORGPGNM03NMEIL (1)
                                   TO  SPA-GMN-TORGPGNM-N (3)
           END-IF
      *
           .
      *
       4101-NYU-ORGPGNM-SET-EXT.
           EXIT.
      *****************************************************************
      *  　カスタマイズプログラム名設定処理
      *****************************************************************
       4101-NYU-CSTPGNM-SET-SEC        SECTION.
      *
      *
           .
      *
       4101-NYU-CSTPGNM-SET-EXT.
           EXIT.
      *****************************************************************
      *  　標準プログラム選択時チェック
      *****************************************************************
       4101-ORCAORGCHK-CHK-SEC         SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    WRK-WIDGET10    TO  IDX1
      **** IF    ( SPA-GMN-TNAME (IDX1)    =   SPACE )
           IF    ( SPA-GMN-TCSTPGNM (IDX1)    =   SPACE )
               MOVE    "T"         TO  SPA-GMN-TORGPGCHK (IDX1)
           ELSE
               MOVE    W11-ORCAORGCHK(IDX1)
                                   TO  SPA-GMN-TORGPGCHK (IDX1)
           END-IF
      *
      *
           .
       4101-ORCAORGCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　カスタマイズプログラム選択時
      *****************************************************************
       4101-ORCACSTCHK-CHK-SEC         SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    WRK-WIDGET10    TO  IDX1
      *****IF    ( SPA-GMN-TNAME (IDX1)    =   SPACE )
           IF    ( SPA-GMN-TCSTPGNM (IDX1)    =   SPACE )
               MOVE    "F"         TO  SPA-GMN-TCSTPGCHK (IDX1)
           ELSE
               MOVE    W11-ORCACSTCHK(IDX1)
                                   TO  SPA-GMN-TCSTPGCHK (IDX1)
           END-IF
      *
           .
       4101-ORCACSTCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　標準プログラム選択時チェック
      *****************************************************************
       4101-NYU-ORCAORGCHK-CHK-SEC     SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    WRK-WIDGET10    TO  IDX1
           IF    ( SPA-GMN-TCSTPGNM-N (IDX1)    =   SPACE )
               MOVE    "T"         TO  SPA-GMN-TORGPGCHK-N (IDX1)
           ELSE
               EVALUATE    IDX1
                   WHEN    1   THRU   4
                       MOVE    W11-ORCAORGCHK-N (IDX1)
                                   TO  SPA-GMN-TORGPGCHK-N (IDX1)
                   WHEN    5
                       MOVE    W11-ORCAORGCHK05N
                                   TO  SPA-GMN-TORGPGCHK-N (5)
                   WHEN    6
                       MOVE    W11-ORCAORGCHK06N
                                   TO  SPA-GMN-TORGPGCHK-N (6)
                   WHEN    7
                       MOVE    W11-ORCAORGCHK07N
                                   TO  SPA-GMN-TORGPGCHK-N (7)
               END-EVALUATE
           END-IF
      *
           .
       4101-NYU-ORCAORGCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　カスタマイズプログラム選択時
      *****************************************************************
       4101-NYU-ORCACSTCHK-CHK-SEC     SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    WRK-WIDGET10    TO  IDX1
      **** IF    ( SPA-GMN-TNAME-N (IDX1)    =   SPACE )
           IF    ( SPA-GMN-TCSTPGNM-N (IDX1)    =   SPACE )
               MOVE    "F"         TO  SPA-GMN-TCSTPGCHK-N (IDX1)
           ELSE
               EVALUATE    IDX1
                   WHEN    1   THRU   4
                       MOVE    W11-ORCACSTCHK-N (IDX1)
                                   TO  SPA-GMN-TCSTPGCHK-N (IDX1)
                   WHEN    5
                       MOVE    W11-ORCACSTCHK05N
                                   TO  SPA-GMN-TCSTPGCHK-N (IDX1)
                   WHEN    6
                       MOVE    W11-ORCACSTCHK06N
                                   TO  SPA-GMN-TCSTPGCHK-N (IDX1)
                   WHEN    7
                       MOVE    W11-ORCACSTCHK07N
                                   TO  SPA-GMN-TCSTPGCHK-N (IDX1)
               END-EVALUATE
           END-IF
      *
           .
       4101-NYU-ORCACSTCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    タグ切替え処理
      *****************************************************************
       460-TAGUCHG-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      460-TAGUCHG-EXT
           END-IF
      *
           IF      WRK-MCP-WIDGET      =   "N0101"
      *        外来画面へ
               MOVE    01                  TO  SPA-GMN-CUR
               MOVE    1                   TO  SPA-GMN-PAGENO
           ELSE
      *        入院画面へ
               MOVE    51                  TO  SPA-GMN-CUR
               MOVE    2                   TO  SPA-GMN-PAGENO
           END-IF
      *
           .
       460-TAGUCHG-EXT.
           EXIT.
      *****************************************************************
      *    確定
      *****************************************************************
       410-KAKUTEI-SEC           SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD        NOT  =   SPACE
               GO  TO  410-KAKUTEI-EXT
           END-IF
      *
           PERFORM 4901-KANRENCHK-SEC
      *
           IF      SPA-ERRCD        NOT  =   SPACE
               GO  TO  410-KAKUTEI-EXT
           END-IF
      *
           IF      SPA-ERRCD        =   SPACE
               MOVE    "1001"           TO  SPA-WIDCD
           END-IF
           .
       410-KAKUTEI-EXT.
           EXIT.
      *
      *----(02.00.03)--ADD-START---
      *****************************************************************
      *    削除
      *****************************************************************
       410-SAKUJYO-SEC             SECTION.
      *
           IF     (SPA-GMN-USERKBN      =   SPACE )
               OR (SPA-GMN-YUKOKBN      =   SPACE )
               MOVE    "0003"              TO  SPA-ERRCD
               GO  TO  410-SAKUJYO-EXT
           END-IF
      *
           IF      SPA-GMN-USERKBN     =   "*"
               MOVE    "0009"              TO  SPA-ERRCD
           ELSE
               IF      SPA-GMN-SELNUM       =   ZERO
                   MOVE    "0007"              TO  SPA-ERRCD
               ELSE
                   IF      SPA-GMN-UMARK(SPA-GMN-SELNUM)   NOT =   "*"
                       MOVE    "0010"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD        =   SPACE
               MOVE    "1002"           TO  SPA-WIDCD
           END-IF
           .
       410-SAKUJYO-EXT.
           EXIT.
      *
      *----(02.00.03)--ADD-END-----
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           EVALUATE    SPA-W01-KANRICD
           WHEN    "1031"
      *----(02.00.03)--UPD-START---
               IF      SPA-GMN-USERKBN    =   "*"
      *            標準登録
                   PERFORM 4901-SYS1031-HYOJUN-SEC
               ELSE
      *            職員毎登録
                   PERFORM 4902-SYS1031-SYOUIN-SEC
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  490-KOUSHIN-EXT
               END-IF
      *
      *        初期画面へ
               PERFORM 310-SPASET-SEC
      *
           WHEN    "1030"
               PERFORM 490-SYS-1030-KOUSHIN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  490-KOUSHIN-EXT
               END-IF
      *        選択へ
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-EVALUATE
      *
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *----(02.00.03)--ADD-START---
      *****************************************************************
      *       削除処理
      *****************************************************************
       490-SAKUJYO-SEC           SECTION.
      *
           EVALUATE    SPA-W01-KANRICD
           WHEN    "1031"
               IF      SPA-GMN-USERKBN    =   "*"
                   MOVE    ZERO           TO  WRK-KBNCD1
               ELSE
                   MOVE    SPA-NAI-USERCD(SPA-GMN-SELNUM)
                                          TO  WRK-KBNCD1
               END-IF
      *
               PERFORM 490-SYS-1031-SAKUJYO-SEC
      *
               PERFORM VARYING IDZ2    FROM    1   BY  1
                       UNTIL   ( IDZ2  >   SPA-GPGNM-MAX ) OR
                               (SPA-ERRCD  NOT =   SPACE )
                   PERFORM 490-SYS-1032-SAKUJYO-SEC
               END-PERFORM
      *
               IF    ( SPA-BEDSU       >   ZERO )
      *
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 490-SYS-1034-SAKUJYO-SEC
                   END-IF
      *
                   PERFORM VARYING IDZ2    FROM    1   BY  1
                           UNTIL   ( IDZ2  >   3 )     OR
                                   (SPA-ERRCD  NOT =   SPACE )
                       PERFORM 490-SYS-1035-SAKUJYO-SEC
                   END-PERFORM
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
      *            初期画面へ
                   PERFORM 310-SPASET-SEC
               END-IF
           END-EVALUATE
      *
      *----(02.00.03)--UPD-START---
      *     MOVE   "JOIN"               TO  MCP-PUTTYPE
      *     PERFORM 210-BACK
      ***** PERFORM 310-SPASET-SEC
      *----(02.00.03)--UPD-END-----
           .
       490-SAKUJYO-EXT.
           EXIT.
      *----(02.00.03)--ADD-END-----
      *
      *****************************************************************
      *       出力先プリンタ割り当て情報・標準登録
      *****************************************************************
       4901-SYS1031-HYOJUN-SEC    SECTION.
      *
           MOVE    ZERO                TO  WRK-KBNCD1
      *    出力先プリンタ割り当て情報
           PERFORM 490-SYS-1031-KOUSHIN-SEC
      *    外来・帳票プログラム
           PERFORM VARYING IDZ2        FROM    1   BY  1
                   UNTIL  (IDZ2        >   SPA-GPGNM-MAX )  OR
                          (SPA-ERRCD   NOT =   SPACE )
               PERFORM 490-SYS-1032-KOUSHIN-SEC
           END-PERFORM
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO      TO      4901-SYS1031-HYOJUN-EXT
           END-IF
      *
      *    入院
           IF      SPA-BEDSU           >   ZERO
      *
               PERFORM 490-SYS-1034-KOUSHIN-SEC
      *        入院・帳票プログラム
               PERFORM VARYING IDZ2        FROM    1   BY  1
                       UNTIL  (IDZ2        >   SPA-NPGNM-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
                      PERFORM 490-SYS-1035-KOUSHIN-SEC
               END-PERFORM
           ELSE
      *        外来の時、入院情報を削除する
               PERFORM 490-SYS-1034-SAKUJYO-SEC
      *
               PERFORM VARYING IDZ2        FROM    1   BY  1
                       UNTIL   (IDZ2       >   SPA-NPGNM-MAX )  OR
                               (SPA-ERRCD  NOT =   SPACE )
                   PERFORM 490-SYS-1035-SAKUJYO-SEC
               END-PERFORM
           END-IF
      *
           .
       4901-SYS1031-HYOJUN-EXT.
           EXIT.
      *
      *****************************************************************
      *       出力先プリンタ割り当て情報・職員毎登録
      *****************************************************************
       4902-SYS1031-SYOUIN-SEC    SECTION.
      *
      *    標準と１ヶ所でも違う場合に登録する
           MOVE    SPA-NAI-USERCD(SPA-GMN-SELNUM)
                                       TO  WRK-KBNCD1
      *    外来出力先プリンタ割り当て
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               IF       SPA-NAI-TPRTNM (IDX)   =   SPA-GMN-TPRTNM(IDX)
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
      *        標準と同じ時、削除
               PERFORM 490-SYS-1031-SAKUJYO-SEC
           ELSE
      *        登録
               PERFORM 490-SYS-1031-KOUSHIN-SEC
           END-IF
      *    外来・帳票プログラム
           PERFORM VARYING IDZ2        FROM    1   BY  1
                   UNTIL  (IDZ2        >   SPA-GPGNM-MAX )  OR
                          (SPA-ERRCD   NOT =   SPACE     )
               IF     (SPA-NAI-TORGPGNM(IDZ2)
                                           =   SPA-GMN-TORGPGNM(IDZ2))
                 AND  (SPA-NAI-TORGPGCHK(IDZ2)
                                           =   SPA-GMN-TORGPGCHK(IDZ2))
                 AND  (SPA-NAI-TCSTPGNM(IDZ2)
                                           =   SPA-GMN-TCSTPGNM (IDZ2))
                 AND  (SPA-NAI-TCSTPGCHK(IDZ2)
                                           =   SPA-GMN-TCSTPGCHK(IDZ2))
      *            変更なし
                   PERFORM 490-SYS-1032-SAKUJYO-SEC
               ELSE
      *            変更あり
                   PERFORM 490-SYS-1032-KOUSHIN-SEC
               END-IF
           END-PERFORM
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO      TO      4902-SYS1031-SYOUIN-EXT
           END-IF
      *
      *    以下外来のみ
           IF    ( SPA-BEDSU       >   ZERO )
               CONTINUE
           ELSE
               GO      TO      4902-SYS1031-SYOUIN-EXT
           END-IF
      *    入院出力先プリンタ割り当て
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               IF       SPA-NAI-TPRTNM-N(IDX)  =   SPA-GMN-TPRTNM-N(IDX)
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
      *        標準と同じ時、削除
               PERFORM 490-SYS-1034-SAKUJYO-SEC
           ELSE
      *        登録
               PERFORM 490-SYS-1034-KOUSHIN-SEC
           END-IF
      *
      *    入院・帳票プログラム
           PERFORM VARYING IDZ2        FROM    1   BY  1
                   UNTIL  (IDZ2        >   SPA-NPGNM-MAX )  OR
                          (SPA-ERRCD   NOT =   SPACE     )
               IF     (SPA-NAI-TORGPGNM-N(IDZ2)
                                       =   SPA-GMN-TORGPGNM-N (IDZ2))
                 AND  (SPA-NAI-TORGPGCHK-N(IDZ2)
                                       =   SPA-GMN-TORGPGCHK-N(IDZ2))
                 AND  (SPA-NAI-TCSTPGNM-N(IDZ2)
                                       =   SPA-GMN-TCSTPGNM-N (IDZ2))
                 AND  (SPA-NAI-TCSTPGCHK-N(IDZ2)
                                       =   SPA-GMN-TCSTPGCHK-N(IDZ2))
      *            変更なし
                   PERFORM 490-SYS-1035-SAKUJYO-SEC
               ELSE
      *            変更あり
                   PERFORM 490-SYS-1035-KOUSHIN-SEC
               END-IF
           END-PERFORM
      *
           .
       4902-SYS1031-SYOUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       出力先プリンタ割り当て情報
      *****************************************************************
       490-SYS-1031-KOUSHIN-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                      SYS-1031-REC
           MOVE   "1031"               TO  SYS-1031-KANRICD
      *----(02.00.03)--UPD-START---
      *     MOVE    "*"                 TO  SYS-1031-KBNCD
           IF      WRK-KBNCD1          =   ZERO
               MOVE    "*"                 TO  SYS-1031-KBNCD
           ELSE
               MOVE    WRK-KBNCD1          TO  SYS-1031-KBNCD
           END-IF
      *----(02.00.03)--UPD-END-----
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1031-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1031-EDYUKYMD
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >       20 )
               MOVE    SPA-GMN-TPRTNM (IDX)
                                       TO  SYS-1031-PRTNM (IDX)
           END-PERFORM
      *
           MOVE    SYS-1031-REC        TO  SYSKANRI-REC
      *
           PERFORM 490-SYSKANRI-KOUSHIN-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       490-SYS-1031-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       帳票プログラム選択情報
      *****************************************************************
       490-SYS-1032-KOUSHIN-SEC        SECTION.
      *
      *----(02.00.03)--UPD-START---
           MOVE    IDZ2                TO  WRK-KBNCD2
      *
           MOVE    SPACE               TO  SYS-1032-REC
           INITIALIZE                      SYS-1032-REC
           MOVE    "1032"              TO  SYS-1032-KANRICD
      *     MOVE    WRK-KBNCD           TO  SYS-1032-KBNCD
           MOVE    WRK-KBNCD-G         TO  SYS-1032-KBNCD
      *----(02.00.03)--UPD-END-----
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1032-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1032-EDYUKYMD
      *
           MOVE    SPA-GMN-TORGPGNM (IDZ2)
                                       TO  SYS-1032-ORCAORGNM
           MOVE    SPA-GMN-TORGPGCHK(IDZ2)
                                       TO  SYS-1032-ORCAORGCHK
           MOVE    SPA-GMN-TCSTPGNM (IDZ2)
                                       TO  SYS-1032-ORCACSTNM
           MOVE    SPA-GMN-TCSTPGCHK(IDZ2)
                                       TO  SYS-1032-ORCACSTCHK
      *
           MOVE    SYS-1032-REC        TO  SYSKANRI-REC
      *
           PERFORM 490-SYSKANRI-KOUSHIN-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4002"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       490-SYS-1032-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       出力先プリンタ割り当て情報（入院）
      *****************************************************************
       490-SYS-1034-KOUSHIN-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-1034-REC
           INITIALIZE                      SYS-1034-REC
           MOVE   "1034"               TO  SYS-1034-KANRICD
      *----(02.00.03)--UPD-START---
      *     MOVE    "*"                 TO  SYS-1034-KBNCD
           IF      WRK-KBNCD1          =   ZERO
               MOVE    "*"                 TO  SYS-1034-KBNCD
           ELSE
               MOVE    WRK-KBNCD1          TO  SYS-1034-KBNCD
           END-IF
      *----(02.00.03)--UPD-END-----
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1034-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1034-EDYUKYMD
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >       20 )
               MOVE    SPA-GMN-TPRTNM-N (IDX)
                                       TO  SYS-1034-PRTNM (IDX)
           END-PERFORM
      *
           MOVE    SYS-1034-REC        TO  SYSKANRI-REC
      *
           PERFORM 490-SYSKANRI-KOUSHIN-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4003"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       490-SYS-1034-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       帳票プログラム選択情報（入院）
      *****************************************************************
       490-SYS-1035-KOUSHIN-SEC        SECTION.
      *
      *----(02.00.03)--UPD-START---
      *     MOVE    IDZ2                TO  WRK-KBNCD
           MOVE    IDZ2                TO  WRK-KBNCD2
      *
           MOVE    SPACE               TO  SYS-1035-REC
           INITIALIZE                      SYS-1035-REC
           MOVE    "1035"              TO  SYS-1035-KANRICD
      *     MOVE    WRK-KBNCD           TO  SYS-1035-KBNCD
           MOVE    WRK-KBNCD-G         TO  SYS-1035-KBNCD
      *----(02.00.03)--UPD-END-----
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1035-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1035-EDYUKYMD
      *
           MOVE    SPA-GMN-TORGPGNM-N (IDZ2)
                                       TO  SYS-1035-ORCAORGNM
           MOVE    SPA-GMN-TORGPGCHK-N(IDZ2)
                                       TO  SYS-1035-ORCAORGCHK
           MOVE    SPA-GMN-TCSTPGNM-N (IDZ2)
                                       TO  SYS-1035-ORCACSTNM
           MOVE    SPA-GMN-TCSTPGCHK-N(IDZ2)
                                       TO  SYS-1035-ORCACSTCHK
      *
           MOVE    SYS-1035-REC        TO  SYSKANRI-REC
      *
           PERFORM 490-SYSKANRI-KOUSHIN-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4004"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       490-SYS-1035-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       帳票編集区分
      *****************************************************************
       490-SYS-1030-KOUSHIN-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-1030-REC
           INITIALIZE                      SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
      *
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1030-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1030-EDYUKYMD
           MOVE    SPA-GMN-SYOHOGEPRTFLG
                                       TO SYS-1030-SYOHOGEPRTFLG
      *    後発医薬品表示名称
           MOVE    SPA-GMN-SYOHODSPNAME
                                       TO  SYS-1030-SYOHODSPNAME
      *    カルテ３号紙内容
           MOVE    SPA-GMN-KARUTE3KBN
                                       TO  SYS-1030-KARUTE3KBN
      *
           MOVE    SYS-1030-REC        TO  SYSKANRI-REC
      *
           PERFORM 490-SYSKANRI-KOUSHIN-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4005"          TO  SPA-ERRCD
           END-IF
           .
      *
       490-SYS-1030-KOUSHIN-EXT.
           EXIT.
      *
      *----(02.00.03)--ADD-START---
      *****************************************************************
      *       出力先プリンタ割り当て情報
      *****************************************************************
       490-SYS-1031-SAKUJYO-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                      SYS-1031-REC
           MOVE   "1031"               TO  SYS-1031-KANRICD
           IF      WRK-KBNCD1          =   ZERO
               MOVE    "*"                 TO  SYS-1031-KBNCD
           ELSE
               MOVE    WRK-KBNCD1          TO  SYS-1031-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1031-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1031-EDYUKYMD
      *
           MOVE    SYS-1031-REC        TO  SYSKANRI-REC
      *
           PERFORM 490-SYSKANRI-SAKUJYO-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "3001"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       490-SYS-1031-SAKUJYO-EXT.
           EXIT.
      *****************************************************************
      *       帳票プログラム選択情報
      *****************************************************************
       490-SYS-1032-SAKUJYO-SEC        SECTION.
      *
           MOVE    IDZ2                TO  WRK-KBNCD2
      *
           MOVE    SPACE               TO  SYS-1032-REC
           INITIALIZE                      SYS-1032-REC
           MOVE    "1032"              TO  SYS-1032-KANRICD
           MOVE    WRK-KBNCD-G         TO  SYS-1032-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1032-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1032-EDYUKYMD
      *
           MOVE    SYS-1032-REC        TO  SYSKANRI-REC
      *
           PERFORM 490-SYSKANRI-SAKUJYO-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "3002"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       490-SYS-1032-SAKUJYO-EXT.
           EXIT.
      *****************************************************************
      *       出力先プリンタ割り当て情報（入院）
      *****************************************************************
       490-SYS-1034-SAKUJYO-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-1034-REC
           INITIALIZE                      SYS-1034-REC
           MOVE   "1034"               TO  SYS-1034-KANRICD
           IF      WRK-KBNCD1          =   ZERO
               MOVE    "*"                 TO  SYS-1034-KBNCD
           ELSE
               MOVE    WRK-KBNCD1          TO  SYS-1034-KBNCD
           END-IF
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1034-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1034-EDYUKYMD
      *
           MOVE    SYS-1034-REC        TO  SYSKANRI-REC
      *
           PERFORM 490-SYSKANRI-SAKUJYO-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "3003"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       490-SYS-1034-SAKUJYO-EXT.
           EXIT.
      *****************************************************************
      *       帳票プログラム選択情報（入院）
      *****************************************************************
       490-SYS-1035-SAKUJYO-SEC        SECTION.
      *
           MOVE    IDZ2                TO  WRK-KBNCD2
      *
           MOVE    SPACE               TO  SYS-1035-REC
           INITIALIZE                      SYS-1035-REC
           MOVE    "1035"              TO  SYS-1035-KANRICD
           MOVE    WRK-KBNCD-G         TO  SYS-1035-KBNCD
      *
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1035-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1035-EDYUKYMD
      *
           MOVE    SYS-1035-REC        TO  SYSKANRI-REC
      *
           PERFORM 490-SYSKANRI-SAKUJYO-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "3004"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       490-SYS-1035-SAKUJYO-EXT.
           EXIT.
      *----(02.00.03)--ADD-END-----
      *****************************************************************
      *       システム管理マスタ更新処理
      *****************************************************************
       490-SYSKANRI-KOUSHIN-SEC        SECTION.
      *
           MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF  (   FLG-SYSKANRI        =   ZERO )
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
      *
               MOVE    "SYSKANRI-UPD1"         TO  ORC-DBPATH
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "W11 SYSKANRI UPD ERR:" MCP-RC
                           ",KEY:"    SYS-KEY
               END-IF
           ELSE
               MOVE    "SYSKANRI-KEY"          TO  ORC-DBPATH
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "W11 SYSKANRI INS ERR:" MCP-RC
                           ",KEY:"    SYS-KEY
               END-IF
           END-IF
           .
      *
       490-SYSKANRI-KOUSHIN-EXT.
           EXIT.
      *
      *----(02.00.03)--ADD-START---
      *****************************************************************
      *       システム管理マスタ削除処理
      *****************************************************************
       490-SYSKANRI-SAKUJYO-SEC        SECTION.
      *
           MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "W11 SYSKANRI DEL ERR:" MCP-RC
                           ",KEY:"    SYS-KEY
           END-IF
           .
      *
       490-SYSKANRI-SAKUJYO-EXT.
           EXIT.
      *----(02.00.03)--ADD-END-----
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4901-KANRENCHK-SEC           SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRCD
      *
      *----(01.00.03) LINE ADD START --------------------------
      *    出力先プリンタ割り当て情報チェック
           IF      SPA-W01-KANRICD     =   "1031"
      *        必須入力チェック
               PERFORM 4902-SYS1031-KANREN-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4901-KANRENCHK-EXT
               END-IF
      *        独自開発プログラム名チェック
               PERFORM 4901-CSTPGNM-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4901-KANRENCHK-EXT
               END-IF
           END-IF
      *----(02.00.03)--ADD-END-----
      *----(01.00.03) LINE ADD END   --------------------------
           .
       4901-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    必須入力チェック処理
      *****************************************************************
       4902-SYS1031-KANREN-CHK-SEC        SECTION.
      *
           IF     (SPA-GMN-USERKBN     =   SPACE )  OR
                  (SPA-GMN-YUKOKBN     =   SPACE )
               MOVE    "0003"              TO  SPA-ERRCD
               GO  TO  4902-SYS1031-KANREN-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-USERKBN NOT =   "*"
               IF      SPA-GMN-SELNUM      =    ZERO
                   MOVE    "0007"              TO  SPA-ERRCD
                   GO  TO  4902-SYS1031-KANREN-CHK-EXT
               END-IF
      *
               PERFORM 3102-HYOUJYUN-SAI-SET-SEC
               IF      FLG-SAI          =    ZERO
                   IF      SPA-GMN-UMARK(SPA-GMN-SELNUM)   NOT =   "*"
                       MOVE    "0008"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4902-SYS1031-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    独自開発プログラム名チェック処理
      *****************************************************************
       4901-CSTPGNM-CHK-SEC        SECTION.
      *
           PERFORM VARYING IDZ2    FROM    1   BY  1
                   UNTIL  (IDZ2    >   SPA-GPGNM-MAX )
      *
               IF  ( SPA-GMN-TCSTPGNM  (IDZ2)     =   SPACE )
                   IF  ( W11-ORCACSTCHK (IDZ2)    =   "F" )
                       CONTINUE
                   ELSE
                       MOVE    "0004"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDZ2    +   10
                       MOVE    SPA-GPGNM-MAX   TO  IDZ2
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDZ2    FROM    1   BY  1
                   UNTIL   ( IDZ2  >   SPA-NPGNM-MAX2 )
      *
               IF  ( SPA-GMN-TCSTPGNM-N  (IDZ2)    =   SPACE )
                   IF  ( W11-ORCACSTCHK-N (IDZ2)   =   "F" )
                       CONTINUE
                   ELSE
                       MOVE    "0004"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR =   IDZ2    +   60
                       MOVE    SPA-NPGNM-MAX2  TO  IDZ2
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-TCSTPGNM-N (5)  =   SPACE )
               IF  ( W11-ORCACSTCHK05N     =   "F" )
                   CONTINUE
               ELSE
                   MOVE    "0004"          TO  SPA-ERRCD
                   COMPUTE SPA-GMN-CUR =   5       +   60
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-TCSTPGNM-N (6)  =   SPACE )
               IF  ( W11-ORCACSTCHK06N     =   "F" )
                   CONTINUE
               ELSE
                   MOVE    "0004"          TO  SPA-ERRCD
                   COMPUTE SPA-GMN-CUR =   6       +   60
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-TCSTPGNM-N (7)  =   SPACE )
               IF  ( W11-ORCACSTCHK07N     =   "F" )
                   CONTINUE
               ELSE
                   MOVE    "0004"          TO  SPA-ERRCD
                   COMPUTE SPA-GMN-CUR =   7       +   60
               END-IF
           END-IF
      *
      *
           .
      *
       4901-CSTPGNM-CHK-EXT.
           EXIT.
      *
      *----(01.00.05) LINE UPD END   --------------------------
      *----(01.00.03) LINE ADD END   --------------------------
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE  WRK-YMD(1:8)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG
               WHEN    OTHER
                   IF      WRK-YMD  (1:7)  NOT NUMERIC
                       INSPECT     WRK-YMD      REPLACING   ALL " "
                                                BY          "0"
                       MOVE    WRK-YMD(1:1)      TO  WRK-WGO
                       MOVE    WRK-YMD(2:2)      TO  WRK-WYY
                       MOVE    WRK-YMD(5:2)      TO  WRK-WMM
                       MOVE    WRK-YMD(8:2)      TO  WRK-WDD
                   ELSE
                       MOVE    WRK-YMD(1:7)      TO  WRK-WYMD
                   END-IF
      *
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "12"                TO  LNK-DAY1-IRAI
                   MOVE    ZERO                TO  LNK-DAY1-YMD
                   MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
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
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
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
           MOVE    "W11    "           TO  MCP-WINDOW.
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
      *
           MOVE    SPACE               TO  W11
           INITIALIZE                      W11
      *
           MOVE    SPA-GMN-USERKBN-G   TO  W11-USERKBN
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-USERKBN-MAX
               MOVE    SPA-GMN-USERKBNLST (IDX2)
                                       TO  W11-USERKBNLST (IDX2)
           END-PERFORM
           MOVE    SPA-GMN-USERKBN-MAX TO  W11-USERKBN-COUNT
      *
           MOVE    SPA-GMN-YUKOKBN-G   TO  W11-YUKOKBN
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-YUKOKBN-MAX
               MOVE    SPA-GMN-YUKOKBNLST (IDX2)
                                       TO  W11-YUKOKBNLST (IDX2)
           END-PERFORM
           MOVE    SPA-GMN-YUKOKBN-MAX TO  W11-YUKOKBN-COUNT
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >       SPA-GMN-USERLST-MAX
               MOVE    SPA-GMN-USERNUML(IDX2)  TO  W11-USERNUM(IDX2)
               MOVE    SPA-GMN-USERMEIL(IDX2)  TO  W11-USERNAME(IDX2)
               IF      IDX2            =   SPA-GMN-SELNUM
                   MOVE    "T"             TO  W11-USERLST-SELECT(IDX2)
               ELSE
                   MOVE    "F"             TO  W11-USERLST-SELECT(IDX2)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-USERLST-MAX     TO  W11-USERLST-COUNT
      *
           MOVE    SPA-GMN-SELNUM          TO  W11-SELNUM
      *----(02.00.03)--ADD-END-----
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       20
               MOVE    SPA-GMN-TPRTNM  (IDX)   TO  W11-PRTNM  (IDX)
               MOVE    SPA-GMN-TNAME   (IDX)   TO  W11-DSPNAME(IDX)
           END-PERFORM
      *
      *----(02.00.03)--UPD-START---
      *
           MOVE    SPA-GMN-TORGPGNM  (1)
                                        TO  W11-ORCAORGNM01
           MOVE    SPA-GMN-TORGPGNM  (2)
                                        TO  W11-ORCAORGNM02
           MOVE    SPA-GMN-TORGPGNM  (3)
                                        TO  W11-ORCAORGNM03
           MOVE    SPA-GMN-TORGPGNM  (4)
                                        TO  W11-ORCAORGNM04
           MOVE    SPA-GMN-TORGPGNM  (5)
                                        TO  W11-ORCAORGNM05
      *----(02.00.08)--ADD-START---
           MOVE    SPA-GMN-TORGPGNM  (6)
                                        TO  W11-ORCAORGNM06
           MOVE    SPA-GMN-TORGPGNM  (7)
                                        TO  W11-ORCAORGNM07
      *----(02.00.08)--ADD-END-----
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-ORGPGNM03-MAX
               MOVE    SPA-GMN-ORGPGNM03MEIL (IDX2)
                                       TO  W11-ORCAORGNM03LST (IDX2)
           END-PERFORM
           MOVE    SPA-GMN-ORGPGNM03-MAX
                                       TO  W11-ORCAORGNM03-COUNT
      *
      *
           PERFORM VARYING     IDZ2     FROM    1   BY  1
                   UNTIL       IDZ2     >       SPA-GPGNM-MAX
               MOVE    SPA-GMN-TCSTPGNM  (IDZ2)
                                        TO  W11-ORCACSTNM  (IDZ2)
      *
               MOVE    SPA-GMN-TORGPGCHK (IDZ2)
                                        TO  W11-ORCAORGCHK (IDZ2)
      *
               MOVE    SPA-GMN-TCSTPGCHK (IDZ2)
                                        TO  W11-ORCACSTCHK (IDZ2)
           END-PERFORM
      *
      *    入院
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       20
               MOVE    SPA-GMN-TPRTNM-N  (IDX) TO  W11-PRTNM-N  (IDX)
               MOVE    SPA-GMN-TNAME-N   (IDX) TO  W11-DSPNAME-N(IDX)
           END-PERFORM
      *
           MOVE    SPA-GMN-TORGPGNM-N  (1)
                                        TO  W11-ORCAORGNM-N01
           MOVE    SPA-GMN-TORGPGNM-N  (2)
                                        TO  W11-ORCAORGNM-N02
           MOVE    SPA-GMN-TORGPGNM-N  (3)
                                        TO  W11-ORCAORGNM-N03
           MOVE    SPA-GMN-TORGPGNM-N  (4)
                                        TO  W11-ORCAORGNM-N04
           MOVE    SPA-GMN-TORGPGNM-N  (5)
                                        TO  W11-ORCAORGNM05N
           MOVE    SPA-GMN-TORGPGNM-N  (6)
                                        TO  W11-ORCAORGNM06N
           MOVE    SPA-GMN-TORGPGNM-N  (7)
                                        TO  W11-ORCAORGNM07N
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-ORGPGNM03N-MAX
               MOVE    SPA-GMN-ORGPGNM03NMEIL (IDX2)
                                       TO  W11-ORCAORGNM-N03LST (IDX2)
           END-PERFORM
           MOVE    SPA-GMN-ORGPGNM03N-MAX
                                       TO  W11-ORCAORGNM-N03-COUNT
      *
           PERFORM VARYING     IDZ2     FROM    1   BY  1
                   UNTIL       IDZ2     >       SPA-NPGNM-MAX2
               MOVE    SPA-GMN-TCSTPGNM-N  (IDZ2)
                                        TO  W11-ORCACSTNM-N  (IDZ2)
      *
               MOVE    SPA-GMN-TORGPGCHK-N (IDZ2)
                                        TO  W11-ORCAORGCHK-N (IDZ2)
      *
               MOVE    SPA-GMN-TCSTPGCHK-N (IDZ2)
                                        TO  W11-ORCACSTCHK-N (IDZ2)
           END-PERFORM
           MOVE    SPA-GMN-TCSTPGNM-N (5)  TO  W11-ORCACSTNM05N
           MOVE    SPA-GMN-TCSTPGNM-N (6)  TO  W11-ORCACSTNM06N
           MOVE    SPA-GMN-TCSTPGNM-N (7)  TO  W11-ORCACSTNM07N
      *
           MOVE    SPA-GMN-TORGPGCHK-N (5) TO  W11-ORCAORGCHK05N
           MOVE    SPA-GMN-TORGPGCHK-N (6) TO  W11-ORCAORGCHK06N
           MOVE    SPA-GMN-TORGPGCHK-N (7) TO  W11-ORCAORGCHK07N
      *
           MOVE    SPA-GMN-TCSTPGCHK-N (5) TO  W11-ORCACSTCHK05N
           MOVE    SPA-GMN-TCSTPGCHK-N (6) TO  W11-ORCACSTCHK06N
           MOVE    SPA-GMN-TCSTPGCHK-N (7) TO  W11-ORCACSTCHK07N
      *
           MOVE    SPA-GMN-SYOHOGEPRTFLG-G   TO  W11-SYOHOGEPRTFLG
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SYOHOGEPRTFLG-MAX
               MOVE    SPA-GMN-SYOHOGEPRTFLGLST (IDY)
                                       TO  W11-SYOHOGEPRTFLGLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYOHOGEPRTFLG-MAX
                                       TO  W11-SYOHOGEPRTFLG-COUNT
      *
           MOVE    SPA-GMN-SYOHODSPNAME
                                       TO  W11-SYOHODSPNAME
      *    カルテ３号紙
           MOVE    SPA-GMN-KARUTE3KBN-G    TO  W11-KARUTE3KBN
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KARUTE3KBN-MAX
               MOVE    SPA-GMN-KARUTE3KBNLST (IDY)
                                       TO  W11-KARUTE3KBN-ITEM (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KARUTE3KBN-MAX
                                       TO  W11-KARUTE3KBN-COUNT
      *
           MOVE    SPA-GMN-STYUKYMD     TO  W11-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD     TO  W11-EDYUKYMD
      *
      *----(02.00.03)--ADD-START---
           EVALUATE    SPA-W01-KANRICD
           WHEN    "1030"
               MOVE    WIDGET-INSENSITIVE  TO  W11-YUKOKBN-STATE
               MOVE    WIDGET-INSENSITIVE  TO  W11-SELNUM-STATE
               MOVE    WIDGET-INSENSITIVE  TO  W11-B03-STATE
           WHEN    "1031"
               EVALUATE    SPA-GMN-USERKBN
               WHEN    "*"
                   MOVE    WIDGET-INSENSITIVE  TO  W11-YUKOKBN-STATE
                   MOVE    WIDGET-INSENSITIVE  TO  W11-SELNUM-STATE
                   MOVE    WIDGET-INSENSITIVE  TO  W11-B03-STATE
               WHEN    OTHER
                   MOVE    WIDGET-NORMAL       TO  W11-YUKOKBN-STATE
                   MOVE    WIDGET-NORMAL       TO  W11-SELNUM-STATE
                   MOVE    WIDGET-NORMAL       TO  W11-B03-STATE
               END-EVALUATE
           END-EVALUATE
      *----(02.00.03)--ADD-END-----
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
           IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           MOVE    SPACE               TO  MCP-WIDGET
           EVALUATE    SPA-GMN-CUR
               WHEN    1       THRU   20
                   MOVE    SPA-GMN-CUR     TO  IDX2
                   MOVE    "PRTNM"         TO  MCP-WIDGET
                   MOVE    IDX2            TO  MCP-WIDGET(6:2)
                   MOVE    1               TO  SPA-GMN-PAGENO
               WHEN    21      THRU   30
                   MOVE    SPA-GMN-CUR     TO  IDX2
                   COMPUTE IDX2            =   IDX2    -   20
                   MOVE    "ORCACSTNM"     TO  MCP-WIDGET
                   MOVE    IDX2            TO  MCP-WIDGET(10:2)
                   MOVE    1               TO  SPA-GMN-PAGENO
      *        入院
               WHEN    51      THRU   70
                   MOVE    SPA-GMN-CUR     TO  IDX2
                   COMPUTE IDX2            =   IDX2    -   50
                   MOVE    "PRTNM"         TO  MCP-WIDGET
                   MOVE    IDX2            TO  MCP-WIDGET(6:2)
                   MOVE    "N"             TO  MCP-WIDGET(8:1)
                   MOVE    2               TO  SPA-GMN-PAGENO
               WHEN    71      THRU   80
                   MOVE    SPA-GMN-CUR     TO  IDX2
                   COMPUTE IDX2            =   IDX2    -   70
                   MOVE    "ORCACSTNM"     TO  MCP-WIDGET
                   MOVE    IDX2            TO  MCP-WIDGET(10:2)
                   MOVE    "N"             TO  MCP-WIDGET(12:1)
                   MOVE    2               TO  SPA-GMN-PAGENO
      *----(02.00.03)--ADD-START---
                WHEN   101
                    MOVE  "USERKBN"
                                       TO  MCP-WIDGET
                WHEN   102
                    MOVE  "YUKOKBN"
                                       TO  MCP-WIDGET
                WHEN   103
                    MOVE  "SELNUM"
                                       TO  MCP-WIDGET
      *----(02.00.03)--ADD-END-----
                WHEN   301
                    MOVE  "SYOHOGEPRTFLG"
                                       TO  MCP-WIDGET
                WHEN   302
                    MOVE  "SYOHODSPNAME"
                                       TO  MCP-WIDGET
                WHEN   303
                    MOVE  "KARUTE3KBN"
                                       TO  MCP-WIDGET
                 WHEN   999
                    MOVE  "B12"        TO  MCP-WIDGET
           END-EVALUATE
      *
            EVALUATE    SPA-W01-KANRICD
            WHEN    "1031"
                MOVE    0                TO  W11-PAGENO
                IF      SPA-GMN-PAGENO       =   1
      *             外来
                    MOVE    0                TO  W11-NOTE4-PAGENO
                ELSE
      *             入院
                    MOVE    1                TO  W11-NOTE4-PAGENO
                END-IF
            WHEN    "1030"
                    MOVE    1                TO  W11-PAGENO
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
               WHEN    "PRTNM01"
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "PRTNM02"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "PRTNM03"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "PRTNM04"
                   MOVE    5           TO  SPA-GMN-CUR
               WHEN    "PRTNM05"
                   MOVE    6           TO  SPA-GMN-CUR
               WHEN    "PRTNM06"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "PRTNM07"
                   MOVE    8           TO  SPA-GMN-CUR
               WHEN    "PRTNM08"
                   MOVE    9           TO  SPA-GMN-CUR
               WHEN    "PRTNM09"
                   MOVE    10          TO  SPA-GMN-CUR
               WHEN    "PRTNM10"
                   MOVE    11          TO  SPA-GMN-CUR
               WHEN    "PRTNM11"
                   MOVE    12          TO  SPA-GMN-CUR
               WHEN    "PRTNM12"
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    "PRTNM13"
                   MOVE    14          TO  SPA-GMN-CUR
               WHEN    "PRTNM14"
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "PRTNM15"
                   MOVE    16          TO  SPA-GMN-CUR
               WHEN    "PRTNM16"
                   MOVE    17          TO  SPA-GMN-CUR
               WHEN    "PRTNM17"
                   MOVE    18          TO  SPA-GMN-CUR
               WHEN    "PRTNM18"
                   MOVE    19          TO  SPA-GMN-CUR
               WHEN    "PRTNM19"
                   MOVE    20          TO  SPA-GMN-CUR
               WHEN    "PRTNM20"
                   MOVE    21          TO  SPA-GMN-CUR
               WHEN    "ORCAORGNM03"
                   MOVE    23           TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM01"
                   MOVE    22          TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM02"
                   MOVE    23          TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM03"
                   MOVE    24          TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM04"
                   MOVE    26          TO  SPA-GMN-CUR
      *----(02.00.03)--ADD-START---
               WHEN    "ORCACSTNM05"
                   MOVE    27          TO  SPA-GMN-CUR
      *----(02.00.08)--ADD-START---
               WHEN    "ORCACSTNM06"
                   MOVE    25          TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM07"
      *----(02.00.08)--ADD-END-----
      *            外来最終
                   IF    ( SPA-BEDSU       >   ZERO )
                       MOVE    51          TO  SPA-GMN-CUR
                       MOVE    2           TO  SPA-GMN-PAGENO
                    ELSE
                       MOVE    999         TO  SPA-GMN-CUR
                   END-IF
      *----(02.00.03)--ADD-END-----
      *
               WHEN    "PRTNM01N"
                   MOVE    52           TO  SPA-GMN-CUR
               WHEN    "PRTNM02N"
                   MOVE    53           TO  SPA-GMN-CUR
               WHEN    "PRTNM03N"
                   MOVE    54           TO  SPA-GMN-CUR
               WHEN    "PRTNM04N"
                   MOVE    55           TO  SPA-GMN-CUR
               WHEN    "PRTNM05N"
                   MOVE    56           TO  SPA-GMN-CUR
               WHEN    "PRTNM06N"
                   MOVE    57           TO  SPA-GMN-CUR
               WHEN    "PRTNM07N"
                   MOVE    58           TO  SPA-GMN-CUR
               WHEN    "PRTNM08N"
                   MOVE    59           TO  SPA-GMN-CUR
               WHEN    "PRTNM09N"
                   MOVE    60           TO  SPA-GMN-CUR
               WHEN    "PRTNM10N"
                   MOVE    61           TO  SPA-GMN-CUR
               WHEN    "PRTNM11N"
                   MOVE    62           TO  SPA-GMN-CUR
               WHEN    "PRTNM12N"
                   MOVE    63           TO  SPA-GMN-CUR
               WHEN    "PRTNM13N"
                   MOVE    64           TO  SPA-GMN-CUR
               WHEN    "PRTNM14N"
                   MOVE    65           TO  SPA-GMN-CUR
               WHEN    "PRTNM15N"
                   MOVE    66           TO  SPA-GMN-CUR
               WHEN    "PRTNM16N"
                   MOVE    67           TO  SPA-GMN-CUR
               WHEN    "PRTNM17N"
                   MOVE    68           TO  SPA-GMN-CUR
               WHEN    "PRTNM18N"
                   MOVE    69           TO  SPA-GMN-CUR
               WHEN    "PRTNM19N"
                   MOVE    70           TO  SPA-GMN-CUR
               WHEN    "PRTNM20N"
                   MOVE    71           TO  SPA-GMN-CUR
               WHEN    "ORCAORGNM03N"
                   MOVE    73           TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM01N"
                   MOVE    72           TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM02N"
                   MOVE    73           TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM03N"
                   MOVE    74           TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM04N"
      *            入院最終、確定へ
      *********    MOVE    999          TO  SPA-GMN-CUR
                   MOVE    75           TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM05N"
                   MOVE    76           TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM06N"
                   MOVE    77           TO  SPA-GMN-CUR
               WHEN    "ORCACSTNM07N"
      *            入院最終、確定へ
                   MOVE    999          TO  SPA-GMN-CUR
      *
      *----(02.00.03)--ADD-START---
               WHEN    "USERKBN"
                   MOVE    103          TO  SPA-GMN-CUR
               WHEN    "YUKOKBN"
                   MOVE    103          TO  SPA-GMN-CUR
               WHEN    "SELNUM"
                   MOVE    01           TO  SPA-GMN-CUR
               WHEN    "USERLST"
                   MOVE    01           TO  SPA-GMN-CUR
      *----(02.00.03)--ADD-END-----
      *
      *        帳票編集区分情報
               WHEN    "SYOHOGEPRTFLG"
                   MOVE    302         TO  SPA-GMN-CUR
               WHEN    "SYOHODSPNAME"
                   MOVE    303         TO  SPA-GMN-CUR
               WHEN    "KARUTE3KBN"
                   MOVE    999         TO  SPA-GMN-CUR
           END-EVALUATE
      *
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-INSENSITIVE      TO  W11-N01-STATE
                                               W11-N02-STATE
      *
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1031"
                   MOVE    WIDGET-NORMAL      TO  W11-N01-STATE
                   MOVE    WIDGET-NORMAL      TO  W11-N0101-STATE
                   MOVE    WIDGET-NORMAL      TO  W11-N0102-STATE
               WHEN    "1030"
                   MOVE    WIDGET-NORMAL      TO  W11-N02-STATE
           END-EVALUATE
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "処方せんＧＥ表示区分入力エラー"
                                                      TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "カルテ３号紙内容入力エラー"
                                                      TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "プリンタ名入力エラー"     TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "項目に入力がありません"   TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "プログラム名を入力してください"
                                                      TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "後発医薬品表示名称は全角で入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "プリンタ名は半角で入力してください"
                                                      TO  SPA-ERRMSG
      *----(02.00.03)--ADD-START---
               WHEN    "0007"
                   MOVE    "端末が指定されていません"
                                                      TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "標準設定と同じです。"
                                                      TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "標準の設定は削除できません"
                                                      TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "この端末の設定は登録されていません"
                                                      TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "区分が正しくありません"
                                                      TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "選択番号が正しくありません"
                                                      TO  SPA-ERRMSG
               WHEN    "3001"  THRU    "3999"
                   MOVE    "削除処理に失敗しました"
                                                      TO  SPA-ERRMSG
      *----(02.00.03)--ADD-END-----
               WHEN    "4001"  THRU    "4999"
                   MOVE    "更新処理に失敗しました"
                                                      TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W11"                TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "登録処理をします"    TO  WRK-WIDMSG
      *----(02.00.03)--ADD-START---
               WHEN    "1002"
                   MOVE    "削除処理をします"    TO  WRK-WIDMSG
      *----(02.00.03)--ADD-END-----
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W11"                TO  SPA-MOTOPG
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
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               MOVE    1           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    KANACHK-SYUBETU TO  FLG-ENTRYCHK
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *----(01.00.03) LINE ADD START --------------------------
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SELECT-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *     MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
      *----(02.00.03)--UPD-START---
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
      *----(02.00.03)--UPD-END-----
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *----(01.00.03) LINE ADD END   --------------------------
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
      *----(01.00.03) LINE ADD START --------------------------
      *     MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
      *----(02.00.03)--UPD-START---
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
      *----(02.00.03)--UPD-END-----
      *----(01.00.03) LINE ADD END   --------------------------
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *----(01.00.03) LINE ADD START --------------------------
      *****************************************************************
      *    システム管理マスタクローズ処理
      *****************************************************************
       900-SYSKANRI-CLOSE-SEC         SECTION.
      *
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *     MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
      *----(02.00.03)--UPD-START---
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
      *----(02.00.03)--UPD-END-----
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-SYSKANRI-CLOSE-EXT.
           EXIT.
      *
      *----(01.00.03) LINE ADD END   --------------------------
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

