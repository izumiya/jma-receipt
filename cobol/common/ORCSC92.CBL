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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC92.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為の点数編集処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    多々納       01/04/11  施設基準の修正等
      *  01.00.02    多々納       01/05/18  入力コードの表示連番追加
      *  01.00.03    多々納       01/07/09  老人一般コード変換
      *  01.00.04    多々納       01/07/25  チェックのみ（区分：３）
      *                                     併用算定チェック追加
      *  02.00.00    多々納       02/03/01  訂正時、併用算定回数を
      *                                     カウントしない
      *  02.00.01    NACL-多々納  02/08/06  時間外判定に労災再診を追加
      *  02.00.02    NACL-多々納  02/08/26  用法の時施設基準チェックをし
      *                                     ない
      *  02.00.03    NACL-多々納  02/10/02  注加算チェックを剤毎とする
      *  02.00.04    NACL-多々納  02/10/03  労災加算なしの追加
      *  02.00.05    NACL-多々納  02/10/18  入院回数追加
      *  02.00.06    NACL-多々納  02/10/21  慢性疼痛管理料の改正追加
      *  02.00.07    NACL-多々納  02/11/28  慢性疼痛管理料の改正修正
      *  02.00.08    NACL-多々納  02/12/06  慢性疼痛の算定チェック修正
      *  02.00.09    NACL-多々納  03/02/10  病床数（一般）チェック追加
      *  02.00.10    NACL-多々納  03/05/21  慢性疼痛の同時チェック追加
      *  02.00.11    NACL-多々納  03/07/02  入院の消炎鎮痛処理と
      *                                     在宅寝たきり処置チェック変更
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-PTNYUINRRK        PIC 9(01).
      *
           03  FLG-SST             PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-NYUIN           PIC 9(01).
      *
           03  FLG-MANSEI-ARI      PIC 9(01).
      *
           03  FLG-SANTEI-OK       PIC 9(01).
           03  FLG-SANTEI-OK2      PIC 9(01).
      *    手術施設基準不適合
           03  FLG-SSTKIJUNCD-ERR      PIC  9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
           03  IDX-C               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SST             PIC 9(04).
           03  IDX-SS2             PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDZ                 PIC 9(02).
           03  IDX-ICD1            PIC 9(01).
           03  IDX-ICD             PIC 9(02).
      *
           03  IDX-STR             PIC 9(02).
           03  IDX-END             PIC 9(02).
           03  IDX-Y3              PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *    単位名称
           03  WRK-TANINAME        PIC X(04).
      *
           03  WRK-MEISYO          PIC X(80).
      *****03  WRK-MEISYO-N        REDEFINES   WRK-MEISYO
      *****                        PIC N(22).
      *   きざみ値行為再計算用
           03  WRK-KEI-TEN         PIC 9(08)V9(03).
           03  WRK-KEI-TEN1        PIC 9(08)V9(03).
           03  WRK-KEI-TEN2        PIC 9(08).
           03  WRK-SURYO           PIC 9(05)V9(03).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(03).
      *    年齢判定用
           03  WRK-KGNAGE          PIC 9(03).
           03  WRK-JGNAGE          PIC 9(03).
      *    最大行
           03  WRK-MAX             PIC 9(04).
      *    算定回数
           03  WRK-SANTEI-CNT      PIC 9(04).
      *    診療科名称
           03  WRK-SRYKAMEI        PIC X(20).
      *
           03  WRK-SRYCD               PIC X(09).
      *
           03  WRK-SYOSINYMD1          PIC X(08).
           03  WRK-YMD.
               05  WRK-YYMM            PIC X(06).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-CHK-CD               PIC X(09).
      *
      *    コメントの入力値用
       01  WRK-INPUT-SET-AREA.
           03  WRK-INPUT-TBL.
               05  WRK-INPUT-SET       PIC 9(03)   OCCURS  8.
           03  WRK-INPUT-TBL-R         REDEFINES   WRK-INPUT-TBL.
               05  WRK-INPUT-TBL2          OCCURS  4.
                   07  WRK-INPUT-ICHI      PIC 9(03).
                   07  WRK-INPUT-KETA      PIC 9(03).
           03  WRK-KETA                PIC 9(02).
           03  WRK-KETA2               PIC 9(02).
           03  WRK-ICHI                PIC 9(02).
           03  WRK-ICHI2               PIC 9(02).
           03  IDX-KNJ                 PIC 9(02).
           03  IDX-KNJ1                PIC 9(01).
           03  IDX-KNJ2                PIC 9(04).
           03  WRK-ZZZ9-X.
               05  WRK-ZZZ9                PIC ZZZZ9.
           03  WRK-ATAI-X.
               05  WRK-ATAI-9          PIC 9(15).
           03  WRK-KANJI.
               05  WRK-KANJI-X             PIC X(02)
                                           OCCURS   5.
      *
           03  WRK-TIME                PIC 9(01).
      *
           03  WRK-HEN-INPUTCD         PIC X(22).
           03  WRK-IDX-FT              PIC 9(04).
           03  WRK-CD-MCNT             PIC 9(04).
      *
      *    老人・一般変換用
           03  WRK-SRYCD-CHG-AREA.
               05  WRK-SRYCDCHG        PIC X(09)   OCCURS  5.
           03  WRK-NAIYOU              PIC X(22).
           03  WRK-PATH                PIC X(64).
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    点数マスタ−（ワーク）
           COPY    "CPTENSU.INC"       REPLACING
                                       //TNS-// BY //HZN-TNS-//.
      *
      *    システム管理マスタ
           COPY    "CPSK1006.INC".
      *    システム管理マスタ
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1001.INC".
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *----(01.00.02) LINE ADD END   ----------------------------------
      *----(01.00.03) LINE ADD START ----------------------------------
      *    老人一般コード変換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *----(01.00.03) LINE ADD END   ----------------------------------
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    算定履歴
       01  HZN-SANTEI-REC.
           COPY    "CPSANTEI.INC"    REPLACING
                                     //SANTEI-// BY //HZN-SANTEI-//.
      *
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
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    画面表示編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC92.INC".
      *
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC92AREA
           SPA-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-INPUT-SET-AREA
      *
           MOVE    SPACE               TO  LNK-SC92-ERRAREA
           MOVE    LNK-SC92-GMN-IDX    TO  IDX-T
      *    約束の時、処理なし
           IF     (LNK-SC92-SRYCD(1:1) =   "S" )  AND
                  (LNK-SC92-SRYCD(2:5) NUMERIC )  AND
                  (LNK-SC92-SRYCD(7:)  =   SPACE)
               GO      TO      000-PROC-EXT
           END-IF
      *    入院回数処理なし
           IF      LNK-SC92-SRYCD(1:1) =   "*"   OR  SPACE
               GO      TO      000-PROC-EXT
           END-IF
      *    ２００行以上エラー
           IF      LNK-SC92-GMN-IDX    >   200
               MOVE    "0007"          TO  SPA-ERRCD
               GO      TO      000-PROC-EXT
           END-IF
      *
      *    数量・回数のみ変更の時、編集のみ
           IF     (LNK-SC92-KBN        =   "5"  )  AND
                  (SPA-COM-INPUTCD(IDX-T)  NOT =   SPACE )  AND
                  (SPA-COM-CHKFLG (IDX-T)  NOT =   SPACE )
               PERFORM 300-SURYOHEN-SEC
               GO      TO      000-PROC-EXT
           END-IF
      *
           MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               MOVE    "0001"              TO  SPA-ERRCD
               IF      LNK-SC92-KBN    NOT =   "3"
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               END-IF
               GO      TO      000-PROC-EXT
           END-IF
      *    併用算定チェックのみの時、終了
           IF      LNK-SC92-KBN        =   "4"
               PERFORM 2005-HEIYOU-CHK-SEC
               GO      TO      000-PROC-EXT
           END-IF
      *
      *    基本チェック
           PERFORM 200-INPUTCD-KIHON-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               IF      LNK-SC92-KBN    NOT =   "3"
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               END-IF
               GO      TO      000-PROC-EXT
           END-IF
      *
      *    追加挿入
           IF      LNK-SC92-KBN        =   "1"
               PERFORM 400-GYOINSN-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "2"             TO  LNK-SC92-KBN
               ELSE
                   GO      TO      000-PROC-EXT
               END-IF
           ELSE
               MOVE    LNK-SC92-SRYCD      TO  SPA-COM-INPUTCD(IDX-T)
           END-IF
      *
      *    基本チェック
           PERFORM 200-INPUTCD-KIHON2-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               IF      LNK-SC92-KBN    NOT =   "3"
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               END-IF
               GO      TO      000-PROC-EXT
           END-IF
      *    チェックのみの時、終了
           IF      LNK-SC92-KBN        =   "3"
      *        併用算定チェック（算定回数がＯＫの時）
               IF     (LNK-SC92-ERRCD3     =   SPACE ) AND
                      (LNK-SC92-SRYCD(1:1) =   "1"   )
                   PERFORM 2005-HEIYOU-CHK-SEC
               END-IF
               GO      TO      000-PROC-EXT
           END-IF
      *
      *    ＳＰＡ編集
           PERFORM 300-TENSU-HENSYU-SEC
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *    入力コードの主表示コードに編集
           IF      SPA-COM-CHKFLG (IDX-T)  =   SPACE
               PERFORM 400-INPUTCD-HEN-SEC
           END-IF
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *    併用算定チェック（算定回数がＯＫの時）
           IF      SPA-COM-CHKFLG (IDX-T)  =   SPACE
               IF     (LNK-SC92-ERRCD3     =   SPACE ) AND
                      (LNK-SC92-SRYCD(1:1) =   "1"   )
                   PERFORM 2005-HEIYOU-CHK-SEC
               END-IF
           END-IF
      *
      *    基本チェック済フラグ
           IF     (SPA-ERRCD               =   SPACE )  AND
                  (LNK-SC92-ERRAREA        =   SPACE )
      *        画面再編集（追加の時） 
               IF     (SPA-COM-GMNFLG (IDX-T)  =   SPACE  )  AND
                      (LNK-SC92-KBN            =   "1"    )
                   PERFORM 500-GMN-HEN-SEC
               END-IF
               MOVE    "1"             TO  SPA-COM-CHKFLG (IDX-T)
           ELSE
               MOVE    SPACE           TO  SPA-COM-CHKFLG (IDX-T)
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       200-INPUTCD-KIHON-SEC      SECTION.
      *
      *    老人適用区分チェック
           EVALUATE    TNS-ROUTEKKBN
               WHEN    0
                   CONTINUE
               WHEN    1
                   IF      LNK-SC92-ROUJIN  NOT =   ZERO
                       PERFORM 2002-SRYCDCHG-HEN-SEC
                   END-IF
               WHEN    2
                   IF      LNK-SC92-ROUJIN  NOT =   1
                       PERFORM 2002-SRYCDCHG-HEN-SEC
                   END-IF
           END-EVALUATE
      *
      *    病院診療所区分チェック
           EVALUATE    TNS-HOSPSRYKBN
               WHEN    0
                   CONTINUE
               WHEN    1
                   IF      SPA-HOSPSBT     NOT =   1
                       MOVE    "0016"          TO  SPA-ERRCD
                       GO      TO      200-INPUTCD-KIHON-EXT
                   END-IF
               WHEN    2
                   IF      SPA-HOSPSBT     NOT =   2
                       MOVE    "0116"          TO  SPA-ERRCD
                       GO      TO      200-INPUTCD-KIHON-EXT
                   END-IF
           END-EVALUATE
      *
      *    入外適用区分チェック
           EVALUATE    TNS-NYUGAITEKKBN
               WHEN    0
                   CONTINUE
               WHEN    1
                   IF      SPA-NYUGAIKBN   NOT =   "1"
                       MOVE    "0014"          TO  LNK-SC92-ERRCD1
                   END-IF
               WHEN    2
                   IF      SPA-NYUGAIKBN   NOT =   "2"
                       MOVE    "0114"          TO  LNK-SC92-ERRCD1
                   END-IF
           END-EVALUATE
      *
      **** 老人適用区分チェック
      *    EVALUATE    TNS-ROUTEKKBN
      *        WHEN    0
      *            CONTINUE
      *        WHEN    1
      *            IF      LNK-SC92-ROUJIN  NOT =   ZERO
      *                PERFORM 2002-SRYCDCHG-HEN-SEC
      *****************MOVE    "0015"          TO  LNK-SC92-ERRCD2
      *            END-IF
      *        WHEN    2
      *            IF      LNK-SC92-ROUJIN  NOT =   1
      *                PERFORM 2002-SRYCDCHG-HEN-SEC
      *****************MOVE    "0015"          TO  LNK-SC92-ERRCD2
      *            END-IF
      *****END-EVALUATE
      *
      *    算定回数チェック
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               PERFORM 2001-SANTEI-CHK-SEC
           END-IF
      *
      *    施設基準適合分チェック
      *----(01.00.01) LINE MOD START ----------------------------------
      *    IF     (LNK-SC92-SRYCD  (1:2)
      *                                NOT =   "84"   )  AND
      **** IF     (LNK-SC92-SRYCD  (1:1)
      ****                             NOT =   "8"    )  AND
      *----(01.00.01) LINE MOD END   ----------------------------------
      *    施設基準適合分チェック（コメント・用法は対象外）
           IF     (LNK-SC92-SRYCD  (1:1)
                                           =   "8"    )  OR
                  (LNK-SC92-SRYCD  (1:3)
                                           =   "001"  )  OR
                  (TNS-SSTKIJUNCD(01)      =   ZERO   )
               CONTINUE
           ELSE
               PERFORM 2002-SSTKIJUN-CHK-SEC
           END-IF
      *
      *    対象年齢チェック
           IF     (LNK-SC92-SRYCD(1:1) =   "1" )  OR
                  (LNK-SC92-SRYCD(1:1) =   "7" )
               PERFORM 2003-NENREI-CHK-SEC
           END-IF
      *
      *    病床数チェック
           IF      TNS-BYOSYOKBN   NOT =   ZERO
               PERFORM 2004-BYOSYOKBN-CHK-SEC
           END-IF
      *
      *    労災・自賠責チェック
           PERFORM 20088-RSIJIB-CHK-SEC
      *
           .
       200-INPUTCD-KIHON-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人一般コード変換処理
      *****************************************************************
       2002-SRYCDCHG-HEN-SEC             SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC 
      *    コード変換読込
           EVALUATE    TNS-ROUTEKKBN
      *        一般
               WHEN    1
                   MOVE    TNS-SRYCD           TO  CHG-IPNSRYCD
                   MOVE    "SRYCDCHG-KEY2"     TO  WRK-PATH
               WHEN    2
                   MOVE    TNS-SRYCD           TO  CHG-RJNSRYCD
                   MOVE    "SRYCDCHG-KEY3"     TO  WRK-PATH
           END-EVALUATE
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
      *
           INITIALIZE                  WRK-SRYCD-CHG-AREA
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL     FLG-SRYCDCHG      =   1
               ADD     1               TO  IDX
               IF      TNS-ROUTEKKBN   =   1
                   MOVE    CHG-RJNSRYCD    TO  WRK-SRYCDCHG (IDX)
               ELSE
                   MOVE    CHG-IPNSRYCD    TO  WRK-SRYCDCHG (IDX)
               END-IF
      *
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 990-SRYCDCHG-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    WRK-PATH            TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
           IF      IDX                 =   ZERO
               IF      TNS-ROUTEKKBN   =   1
                   MOVE    "0115"          TO  LNK-SC92-ERRCD2
               ELSE
                   MOVE    "0015"          TO  LNK-SC92-ERRCD2
               END-IF
               GO      TO      2002-SRYCDCHG-HEN-EXT
           END-IF
      *
      *    点数マスタ編集
           MOVE    WRK-SRYCDCHG (01)   TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               MOVE    "0015"              TO  LNK-SC92-ERRCD2
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      2002-SRYCDCHG-HEN-EXT
           END-IF
      *
           MOVE    WRK-SRYCDCHG (01)   TO  LNK-SC92-SRYCD
           MOVE    LNK-SC92-SRYCD      TO  SPA-COM-INPUTCD (IDX-T)
      *    画面入力値の入れ替え
           MOVE    ZERO                TO  WRK-CD-MCNT
      *    画面再表示
      *     文字列調査の開始位置　取得
           IF  (SPA-GMN-INPUTCD(IDX-T)(1:1)    IS  NUMERIC) AND
               (SPA-GMN-INPUTCD(IDX-T)(2:1)    =   SPACE  ) AND
               (SPA-GMN-INPUTCD(IDX-T)(3:1)    NOT =   SPACE)
      *        時間加算あり
               MOVE    3                   TO  WRK-IDX-FT
           ELSE
               MOVE    1                   TO  WRK-IDX-FT
           END-IF
           PERFORM VARYING     IDX     FROM    WRK-IDX-FT  BY  1
                   UNTIL      (IDX         >   22  )   OR
                              (SPA-GMN-INPUTCD(IDX-T)(IDX:1)
                                           =   SPACE   OR  "*" )
               MOVE    IDX                 TO  WRK-CD-MCNT
           END-PERFORM
           ADD     1                   TO  WRK-CD-MCNT
           MOVE    SPA-GMN-INPUTCD(IDX-T)(WRK-CD-MCNT:)
                                       TO  WRK-NAIYOU
           MOVE    LNK-SC92-SRYCD      TO  SPA-GMN-INPUTCD (IDX-T)
                                                   (1:9)
           MOVE    WRK-NAIYOU          TO  SPA-GMN-INPUTCD (IDX-T)
                                                   (10:)
      *
           .
       2002-SRYCDCHG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理（挿入後）
      *****************************************************************
       200-INPUTCD-KIHON2-SEC             SECTION.
      *
      *    最終行判定
           MOVE    ZERO                TO  WRK-MAX
           PERFORM VARYING     IDX     FROM    200   BY  -1
                   UNTIL       IDX     <   1
               IF     (SPA-GMN-INPUTCD (IDX)   NOT =   SPACE ) OR
                      (SPA-COM-INPUTCD (IDX)   NOT =   SPACE )
                   MOVE    IDX             TO  WRK-MAX
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
      *
      *    時間外加算チェック
           IF      SPA-COM-TIMEFLG(IDX-T)  NOT =   ZERO
               IF      TNS-TIMEKSNKBN          =   ZERO
                   MOVE    "0030"              TO  SPA-ERRCD
      *        診療の時、手技料ならＯＫとする（現在、時間加算区分＝０）
                   IF    ((SPA-COM-INPUTCD (IDX-T) (2:2)
                                                   =   "11"  OR
                                                       "12" )  AND
                          (TNS-DATAKBN             =   "1"  ) )  OR
      *               小児科外来診療料の時、時間外ありとする
                          (SPA-COM-INPUTCD (IDX-T)
                                                   =   "113003610"  OR
                                                       "113003510"  OR
                                                       "113003810"  OR
                                                       "113003710")  OR
      *         労災の時、診察料ならＯＫとする
                         ((SPA-HKNKBN          =   1    )  AND
                          (SPA-COM-INPUTCD(IDX-T)  =   "101110010" OR
                                                       "101120010" OR
                                                       "101120040" OR
                                                       "101120050" OR
                                                       "101120060" ))
                       MOVE     SPACE              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    注加算コード、注加算連番チェック
           IF     (TNS-CHUKSNCD        =   ZERO  OR  SPACE )  OR
                  (TNS-CHUKSNTUBAN     =   ZERO  OR  SPACE )
               CONTINUE
           ELSE
             IF      LNK-SC92-KBN        =   "1"
      *        追加挿入の時のチェックとする
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-MAX
                   IF      IDX         NOT =   IDX-T
      *                    注加算コード、通番が同じ時エラー
                       IF      (SPA-COM-CHUKSNCD   (IDX)
                                                   =  TNS-CHUKSNCD) AND
                               (SPA-COM-CHUKSNTUBAN(IDX)
                                                   =   TNS-CHUKSNTUBAN)
                          MOVE    "0042"          TO  SPA-ERRCD
                          IF      (LNK-SC92-CHUKSN    =   "1" )  AND
                                  (SPA-COM-INPUTCD (IDX)
                                                   =   LNK-SC92-SRYCD)
                               MOVE    SPACE           TO  SPA-ERRCD
                          END-IF
      *
                       END-IF
                   END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *    同一剤内に時間加算は１つのみ
           IF      TNS-TIMEKSNKBN      NOT =   ZERO
      *----(01.00.01) LINE ADD START ----------------------------------
              AND  TNS-DATAKBN             =   "2"
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
               MOVE    ZERO                TO  WRK-TIME
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   WRK-MAX   )  OR
                                  (SPA-ERRCD       NOT =   SPACE )
                   IF      IDX             NOT =   IDX-T
                       IF     (SPA-COM-DATAKBN   (IDX)   =   "2" ) AND
                              (SPA-COM-TIMEKSNKBN(IDX) NOT =   ZERO)
                           ADD     1               TO  WRK-TIME
                           IF      WRK-TIME        >   1
                               MOVE    "0043"          TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
      *
                   IF      SPA-COM-ZAIEND (IDX)    =   "1"
                       MOVE    ZERO                TO  WRK-TIME
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200-INPUTCD-KIHON2-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（単独）
      *****************************************************************
       2001-SANTEI-CHK-SEC             SECTION.
      *
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      2001-SANTEI-CHK-EXT
           END-IF
      *
      *
           MOVE    TNS-TENSU-REC       TO  HZN-TNS-TENSU-REC
      *    算定履歴チェック処理
           INITIALIZE                  ORCSC91AREA
           MOVE    LNK-SC92-SYORIFLG   TO  LNK-SC91-SYORIFLG
           MOVE    LNK-SC92-SRYCD      TO  LNK-SC91-SRYCD
           MOVE    "1"                 TO  LNK-SC91-KBN
           MOVE    IDX-T               TO  LNK-SC91-GMN-IDX
           MOVE    SPA-COM-KAISU (IDX-T)
                                       TO  LNK-SC91-KAISU
           MOVE    LNK-SC92-TEISEI-AREA
                                       TO  LNK-SC91-TEISEI-AREA
      *
           CALL    "ORCSC91"           USING    MCPAREA
                                                ORCSC91AREA
                                                SPA-SCR-REC
                                                SPA-AREA
                                                TNS-TENSU-REC
           MOVE    LNK-SC91-ERRCD      TO  LNK-SC92-ERRCD3
           MOVE    LNK-SC91-ERRMSG     TO  LNK-SC92-ERRMSG
      *
           MOVE    HZN-TNS-TENSU-REC   TO  TNS-TENSU-REC
      *
           .
       2001-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準適合分チェック
      *****************************************************************
       2002-SSTKIJUN-CHK-SEC      SECTION.
      *
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    "1"                 TO  SYS-1006-KBNCD
           MOVE    ZERO                TO  SYS-1006-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1006-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
           ELSE
               INITIALIZE                  SYS-1006-REC
           END-IF
      *
      *    カーソルクロース
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
           MOVE    ZERO                TO  FLG-SST
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )   OR
                              (FLG-SST     =   1   )
               IF      TNS-SSTKIJUNCD (IDX)    =   ZERO
                   CONTINUE
               ELSE
                   MOVE    TNS-SSTKIJUNCD (IDX)    TO  IDY
                   IF      SYS-1006-SSTKIJUN (IDY) =   1
                       MOVE    1               TO  FLG-SST
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-SST             =   ZERO
               MOVE    "0024"          TO  LNK-SC92-ERRCD4
           END-IF
      *
      *平成１６年４月から
      *    手術の基準不適合逓減判定
           MOVE    ZERO                TO  FLG-SSTKIJUNCD-ERR
           IF     (SPA-SRYYMD          >=  "20040401" )  AND
                  (TNS-SRYKBN          =   "50"       )  AND
                  (TNS-KIJUNTEIGENKBN  =   "2"  OR  "3" )
               IF      FLG-SST             =   ZERO
                   MOVE    SPACE           TO  LNK-SC92-ERRCD4
                   MOVE    1               TO  FLG-SSTKIJUNCD-ERR
               END-IF
           END-IF
      *
           .
       2002-SSTKIJUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢チェック処理
      *****************************************************************
       2003-NENREI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-KGNAGE
           MOVE    ZERO                TO  WRK-JGNAGE
           MOVE    TNS-KGNAGE          TO  WRK-KGNAGE(2:2)
           MOVE    TNS-JGNAGE          TO  WRK-JGNAGE(2:2)
      *
      *    下限年齢チェック
           IF      TNS-KGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-KGNAGE          =   "AA"
      *            新生児判定
                   IF     (LNK-SC92-NENREI-YY   =   ZERO )  AND
                          (LNK-SC92-NENREI-MM   =   ZERO )  AND
                          (LNK-SC92-NENREI-DD   <   28   )
                       MOVE    "0032"              TO  LNK-SC92-ERRCD5
                   END-IF
               ELSE
                   IF      LNK-SC92-NENREI-YY   >=  WRK-KGNAGE
                      CONTINUE
                   ELSE
                       MOVE    "0032"              TO  LNK-SC92-ERRCD5
                   END-IF
               END-IF
           END-IF
      *
      *    上限年齢チェック
           IF      TNS-JGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-JGNAGE          =   "AA"
      *            新生児判定
                   IF     (LNK-SC92-NENREI-YY    =   ZERO )  AND
                          (LNK-SC92-NENREI-MM    =   ZERO )  AND
                          (LNK-SC92-NENREI-DD    <   28   )
                       CONTINUE
                   ELSE
                       MOVE    "0032"              TO  LNK-SC92-ERRCD5
                   END-IF
               ELSE
                   IF      LNK-SC92-NENREI-YY    <   WRK-JGNAGE
                       CONTINUE
                   ELSE
                       MOVE    "0032"              TO  LNK-SC92-ERRCD5
                   END-IF
      *            １５歳未満で、１５歳到達月の判定
                   IF      (WRK-JGNAGE             =   15  )  AND
                           (LNK-SC92-NENREI-YY     =   15  )  AND
                           (LNK-SC92-15MIMAN-ARI   =   1   )
      *                初診・再診で手技料の時、エラーとしない
                       IF     (TNS-DATAKBN             =   1  )  AND
                              (TNS-SRYKBN              =   11  OR  12 )
                           MOVE    SPACE           TO  LNK-SC92-ERRCD5
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       2003-NENREI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病床数チェック
      *****************************************************************
       2004-BYOSYOKBN-CHK-SEC      SECTION.
      *
      *    病床数チェック（ＳＰＡの病床数）
           EVALUATE    TNS-BYOSYOKBN
               WHEN    1
                   IF      SPA-BEDSU           >=  1  AND  <=  99
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
               WHEN    2
                   IF      SPA-BEDSU           >=  100 AND  <=  199
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
               WHEN    3
                   IF      SPA-BEDSU           <   200
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
               WHEN    4
                   IF      SPA-BEDSU           >=  200
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
      *        一般病床数判定
               WHEN    5
                   IF      SPA-BEDSUIPN        >=  0  AND  <=  199
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
               WHEN    6
                   IF      SPA-BEDSUIPN        >=  200
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
           END-EVALUATE
      *
      *     医療機関情報
      ***  MOVE    SPACE               TO  SYS-1001-REC
      *    INITIALIZE                  SYS-1001-REC
      *    MOVE    "1001"              TO  SYS-1001-KANRICD
      ***  MOVE    "1"                 TO  SYS-1001-KBNCD
      *    MOVE    "*"                 TO  SYS-1001-KBNCD
      *    MOVE    ZERO                TO  SYS-1001-STYUKYMD
      *    MOVE    ALL "9"             TO  SYS-1001-EDYUKYMD
      *    システム管理検索
      *    MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *    MOVE    SPA-SRYYMD          TO  ORC-DBYMD
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *        PERFORM 900-KANRIMST-READ-SEC
      *    ELSE
      *        MOVE    1               TO  FLG-SYSKANRI
      *    END-IF
      *    IF      FLG-SYSKANRI        =   ZERO
      *        MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *    ELSE
      *        INITIALIZE                  SYS-1001-REC
      *    END-IF
      *
      *    カーソルクロース
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
      *    病床数チェック
      *    EVALUATE    TNS-BYOSYOKBN
      *        WHEN    1
      *            IF      SYS-1001-BEDSU      >=  1  AND  <=  99
      *                CONTINUE
      *            ELSE
      *****************MOVE    "0025"          TO  LNK-SC92-ERRCD6
      *                MOVE    "0075"          TO  LNK-SC92-ERRCD6
      *            END-IF
      *        WHEN    2
      *            IF      SYS-1001-BEDSU      >=  100 AND  <=  199
      *                CONTINUE
      *            ELSE
      *****************MOVE    "0025"          TO  LNK-SC92-ERRCD6
      *                MOVE    "0075"          TO  LNK-SC92-ERRCD6
      *            END-IF
      *        WHEN    3
      *            IF      SYS-1001-BEDSU      <   200
      *                CONTINUE
      *            ELSE
      *****************MOVE    "0025"          TO  LNK-SC92-ERRCD6
      *                MOVE    "0075"          TO  LNK-SC92-ERRCD6
      *            END-IF
      *        WHEN    4
      *            IF      SYS-1001-BEDSU      >=  200
      *                CONTINUE
      *            ELSE
      *****************MOVE    "0025"          TO  LNK-SC92-ERRCD6
      *                MOVE    "0075"          TO  LNK-SC92-ERRCD6
      *            END-IF
      *        一般病床数判定
      *        WHEN    5
      *            IF      SYS-1001-BEDSUIPN   >=  0  AND  <=  199
      *                CONTINUE
      *            ELSE
      *                MOVE    "0075"          TO  LNK-SC92-ERRCD6
      *            END-IF
      *        WHEN    6
      *            IF      SYS-1001-BEDSUIPN   >=  200
      *                CONTINUE
      *            ELSE
      *                MOVE    "0075"          TO  LNK-SC92-ERRCD6
      *            END-IF
      **** END-EVALUATE
      *
           .
       2004-BYOSYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    併用算定チェック処理
      *****************************************************************
       2005-HEIYOU-CHK-SEC           SECTION.
      *
      *   包括まとめ入力は以下対象外
          IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      2005-HEIYOU-CHK-EXT
           END-IF
      *
      *
           IF      SPA-COM-CHKFLG (IDX-T)  =   "1"
               GO      TO      2005-HEIYOU-CHK-EXT
           END-IF
      *
      *    チェックテーブル検索
           INITIALIZE                  CHK-REC
           MOVE    "5"                 TO  CHK-CHKKBN
           MOVE    LNK-SC92-SRYCD      TO  CHK-SRYCD
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    ZERO                TO  CHK-RENNUM
      *
           MOVE    ZERO                TO  FLG-CHKMST
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "CHK-KEY3"          TO  ORC-DBPATH
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF
           PERFORM
                   UNTIL     (FLG-CHKMST       =   1    )  OR
                             (LNK-SC92-ERRCD6 NOT =   SPACE )
               PERFORM VARYING     IDX-C       FROM    1   BY  1
                       UNTIL      (IDX-C           >   10  )    OR
                                  (CHK-CD (IDX-C)  =   SPACE )  OR
                                  (LNK-SC92-ERRCD6 NOT =   SPACE )
                   PERFORM 20051-CHK-SANTEI-SEC
      *            入力値とのチェック
                   PERFORM 20052-CHK-INPUT-SEC
                   IF      LNK-SC92-ERRCD6 NOT =   SPACE
                       MOVE    CHK-CD (IDX-C)  TO  WRK-CHK-CD
                   END-IF
               END-PERFORM
      *
               MOVE    "CHK-KEY3"          TO  ORC-DBPATH
               PERFORM 950-CHK-READ-SEC
           END-PERFORM
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "CHK-KEY3"          TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
      *    慢性疼痛疾患管理料のチェック
           IF     (LNK-SC92-ERRCD6 NOT =   SPACE      )  AND
                  (SPA-SRYYMD          >=  "20021017" )  AND
                  (LNK-SC92-SRYCD      =   "113006510" OR
                   WRK-CHK-CD          =   "113006510" )
               PERFORM 20053-MANSEI-CHK-SEC
           END-IF
      *
           .
       2005-HEIYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックコードの算定チェック
      *****************************************************************
       20051-CHK-SANTEI-SEC            SECTION.
      *
      *    点数マスタ読込
      *D   MOVE    CHK-CD (IDX-C)      TO  TNS-SRYCD
      *D   PERFORM 910-TENSU-READ-SEC
      *D   IF      FLG-TENSU           =   1
      *D       GO      TO      20051-CHK-SANTEI-EXT
      *D   END-IF
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
           INITIALIZE                  HZN-SANTEI-REC
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    CHK-CD (IDX-C)      TO  SANTEI-SRYCD
      *    保険毎の時、保険組合せが同じこと
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SANTEI-KEY7"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SANTEI-KEY7"       TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM UNTIL       FLG-SANTEI      =   1
      *
      *        対象の算定履歴判定
               PERFORM 200511-SANTEI-CHK-SEC
      *
               IF      FLG-SANTEI-OK       =   1
                   MOVE    SANTEI-REC              TO  HZN-SANTEI-REC
               END-IF
      *
               MOVE    "SANTEI-KEY7"       TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           MOVE    HZN-SANTEI-REC      TO  SANTEI-REC
      *
      *
      *
      *    算定履歴検索
      *    INITIALIZE                  SANTEI-REC
      *    MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
      *    MOVE    SPA-PTID            TO  SANTEI-PTID
      *    MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
      *    MOVE    CHK-CD (IDX-C)      TO  SANTEI-SRYCD
      *    EVALUATE    TNS-SANTEIRRKKBN
      *        WHEN    1
      *            MOVE    ZERO                TO  SANTEI-NYUGAIKBN
      *            MOVE    ZERO                TO  SANTEI-SRYKA
      *        WHEN    2
      *            MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
      *            MOVE    ZERO                TO  SANTEI-SRYKA
      *        WHEN    3
      *            MOVE    ZERO                TO  SANTEI-NYUGAIKBN
      *            MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
      *        WHEN    4
      *            MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
      *            MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
      *    END-EVALUATE
      *
      *    IF     (FLG-HKNSAN          =   1   )  AND
      *           (SPA-HKNKBN      NOT =   ZERO)
      *        MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
      *    ELSE
      *        MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *    END-IF
      *
      *    IF      WRK-RSI-KBN         =   SPACE
      *        主キー
      *        MOVE    "SANTEI-KEY"        TO  WRK-PATH
      *    ELSE
      *        労災毎の算定
      *        MOVE    "SANTEI-KEY3"       TO  WRK-PATH
      *    END-IF
      *
      *
      *    MOVE    SANTEI-REC          TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *****MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
      *    MOVE    WRK-PATH            TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *********MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
      *        MOVE    WRK-PATH            TO  ORC-DBPATH
      *        PERFORM 920-SANTEI-READ-SEC
      *    ELSE
      *        MOVE    1                   TO  FLG-SANTEI
      *    END-IF
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
      *    IF      FLG-SANTEI          =   1
      *        INITIALIZE              SANTEI-REC
      *****END-IF
      *
      *    算定歴の回数をチェックする
           MOVE    SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNT
      *
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC92-SYORIFLG   =   1
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               IF     (SANTEI-SRYYM        =   SPA-SRYYMD(1:6)) AND
                      (SANTEI-DAY (IDX-Y2) >   ZERO )  AND
                      (WRK-SANTEI-CNT      >   ZERO )
                   COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                           -   SANTEI-DAY (IDX-Y2)
               END-IF
           END-IF
      *
      *    在宅寝たきり患者処置指導管理料チェック
           IF     (WRK-SANTEI-CNT      >   ZERO    )  AND
                 ((LNK-SC92-SRYCD      =   "114005810" ) OR
                  (CHK-CD (IDX-C)      =   "114005810" ))
               PERFORM 20054-NETAKIRI-CHK-SEC
           END-IF
      *
      *    算定歴の回数をチェックする
           IF      WRK-SANTEI-CNT      >   ZERO
               MOVE    "0033"              TO  LNK-SC92-ERRCD6
      *
               MOVE    CHK-CD (IDX-C)      TO  TNS-SRYCD
               PERFORM 911-TENSU-READ22-SEC
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    SANTEI-SRYYM        TO  LNK-DAY2-YMD(1:6)
               MOVE    SANTEI-MSANTEID     TO  LNK-DAY2-YMD(7:2)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  LNK-DAY2-EDTYMD3    DELIMITED  BY  SIZE
                       "　"                DELIMITED  BY  SIZE
                       TNS-NAME            DELIMITED  BY  SPACE
                      INTO                 LNK-SC92-ERRMSG
               END-STRING
           END-IF
           .
       20051-CHK-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険毎のコードの判定処理
      *****************************************************************
       1000-SANTEI-CHK-SEC      SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
                   MOVE    SPACE           TO  WRK-RSI-KBN
               WHEN    CHK-CD (IDX-C)      =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
                   MOVE    TBL-RSI-KBN    (TBL-SAN)
                                           TO  WRK-RSI-KBN
           END-SEARCH
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の算定履歴判定処理
      *****************************************************************
       200511-SANTEI-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEI-OK
      *
           EVALUATE    TRUE
      *         算定履歴区分＝１
               WHEN   (SANTEI-NYUGAIKBN       =   ZERO )  AND
                      (SANTEI-SRYKA           =   ZERO )
                   MOVE    1                  TO  FLG-SANTEI-OK
      *         算定履歴区分＝２
               WHEN   (SANTEI-NYUGAIKBN       =   SPA-NYUGAIKBN) AND
                      (SANTEI-SRYKA           =   ZERO )
                   MOVE    1                  TO  FLG-SANTEI-OK
      *
      *         算定履歴区分＝３
               WHEN   (SANTEI-NYUGAIKBN       =   ZERO )  AND
                      (SANTEI-SRYKA           =   SPA-SRYKA )
                   MOVE    1                  TO  FLG-SANTEI-OK
      *
      *         算定履歴区分＝４
               WHEN   (SANTEI-NYUGAIKBN       =   SPA-NYUGAIKBN) AND
                      (SANTEI-SRYKA           =   SPA-SRYKA )
                   MOVE    1                  TO  FLG-SANTEI-OK
               END-EVALUATE
      *
      *
           .
       200511-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力値とのチェック処理
      *****************************************************************
       20052-CHK-INPUT-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   WRK-MAX   )  OR
                              (LNK-SC92-ERRCD6     NOT =   SPACE )
               IF      IDX                 =   IDX-T
                   CONTINUE
               ELSE
                   IF      SPA-COM-INPUTCD(IDX)
                                               =   CHK-CD (IDX-C)
                       MOVE    "0033"          TO  LNK-SC92-ERRCD6
      *
                       MOVE    CHK-CD (IDX-C)      TO  TNS-SRYCD
                       PERFORM 911-TENSU-READ22-SEC
      *
                       STRING  "今回　"            DELIMITED  BY  SIZE
                               TNS-NAME            DELIMITED  BY  SPACE
                              INTO                 LNK-SC92-ERRMSG
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
           .
       20052-CHK-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *     慢性疼痛疾患管理料のチェック処理
      *****************************************************************
       20053-MANSEI-CHK-SEC        SECTION.
      *
      *    理学療法４、消炎鎮痛等処置の時チェック
           IF     (LNK-SC92-SRYCD      =   "180707410"
                                       OR  "180022710"
                                       OR  "180022810"
                                       OR  "180707510"
                                       OR  "140029610"
                                       OR  "140040310"
                                       OR  "140002210"
                                       OR  "140002310")  OR
                  (WRK-CHK-CD          =   "180707410"
                                       OR  "180022710"
                                       OR  "180022810"
                                       OR  "180707510"
                                       OR  "140029610"
                                       OR  "140040310"
                                       OR  "140002210"
                                       OR  "140002310" )
               CONTINUE
           ELSE
               GO      TO      20053-MANSEI-CHK-EXT
           END-IF
      *
      *    直帰の初診算定日を決定する
           PERFORM 200531-SYOSIN-SANTEI-SEC
      *
      *    直帰の慢性疼痛疾患管理料算定日
           MOVE    "113006510"         TO  WRK-SRYCD
           PERFORM 2005332-SANTEI-CHK-SEC
      *    今回入力でないこと
           MOVE    ZERO                TO  FLG-MANSEI-ARI
           IF      LNK-SC92-SRYCD  NOT =   WRK-SRYCD
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-MAX
                   IF      SPA-COM-INPUTCD (IDX)   =   WRK-SRYCD
                       MOVE    1                   TO  FLG-MANSEI-ARI
                   END-IF
               END-PERFORM
           END-IF
      *
      *    初回慢性疼痛がない時ＯＫ
           IF     (WRK-YMD             =   SPACE  )  AND
                  (FLG-MANSEI-ARI      =   ZERO   )
               MOVE    SPACE           TO  LNK-SC92-ERRCD6
               MOVE    SPACE           TO  LNK-SC92-ERRMSG
           END-IF
      *    訂正の時、当日ならＯＫとする
           IF      LNK-SC92-SYORIFLG   =   1
               IF     (WRK-YMD             =   SPA-SRYYMD  )  AND
                      (FLG-MANSEI-ARI      =   ZERO   )
                   MOVE    SPACE           TO  LNK-SC92-ERRCD6
                   MOVE    SPACE           TO  LNK-SC92-ERRMSG
               END-IF
           END-IF
      *
      *    初診算定日が前回の算定日より後の時、算定ができること
           IF    ((WRK-YMD             =   SPACE  )  OR
                  (WRK-SYOSINYMD1      >   WRK-YMD))
              AND (FLG-MANSEI-ARI      =   ZERO   )
               MOVE    SPACE           TO  LNK-SC92-ERRCD6
               MOVE    SPACE           TO  LNK-SC92-ERRMSG
           END-IF
           .
       20053-MANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     在宅寝たきり患者処置指導管理料のチェック処理
      *****************************************************************
       20054-NETAKIRI-CHK-SEC        SECTION.
      *
      *    消炎鎮痛等処置との時チェック
           IF      (CHK-CD (IDX-C)     =   "140029610"
                                       OR  "140040310"
                                       OR  "140002210"
                                       OR  "140002310"
                                       OR  "114005810")  OR
                   (LNK-SC92-SRYCD     =   "140029610"
                                       OR  "140040310"
                                       OR  "140002210"
                                       OR  "140002310"
                                       OR  "114005810")
               CONTINUE
           ELSE
               GO      TO      20054-NETAKIRI-CHK-EXT
           END-IF
      *
      *    入院中の場合対象外
           IF     (SPA-NYUGAIKBN       =   "1"     )  AND
                  (CHK-CD (IDX-C)      =   "114005810" )
               MOVE    ZERO                TO  WRK-SANTEI-CNT
               GO      TO      20054-NETAKIRI-CHK-EXT
           END-IF
      *
      *    外来で当月入院が有り消炎鎮痛の場合のみ対象とする
           IF     (CHK-CD (IDX-C)      =   "140029610"
                                       OR  "140040310"
                                       OR  "140002210"
                                       OR  "140002310" )
               CONTINUE
           ELSE
               GO      TO      20054-NETAKIRI-CHK-EXT
           END-IF
      *
      *    入院中の場合、算定なしとする
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTNYUINRRK-KEY2"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTNYUINRRK-KEY2"   TO  ORC-DBPATH
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    ZERO            TO  FLG-NYUIN
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )  OR
                              (FLG-NYUIN           =   1 )
      *        対象外
               IF     (PTNYUINRRK-NYUINYMD(1:6)   >   SPA-SRYYMD
                                                             (1:6) ) OR
                      (PTNYUINRRK-TAIINYMD(1:6)   <   SPA-SRYYMD
                                                             (1:6) )
                   MOVE    1               TO  FLG-CHK
               ELSE
                   MOVE    ZERO            TO  FLG-CHK
               END-IF
               IF     (PTNYUINRRK-TAIINYMD(1:6)   <   SPA-SRYYMD (1:6) )
                   MOVE    1               TO  FLG-NYUIN
               END-IF
      *        入院中
               IF      FLG-CHK             =   ZERO
                   IF      PTNYUINRRK-TAIINYMD     =   ALL "9"
                       MOVE    31                  TO  IDX-END
                   ELSE
                       MOVE    PTNYUINRRK-TAIINYMD(7:2)
                                                   TO  IDX-END
                   END-IF
                   IF      PTNYUINRRK-NYUINYMD(1:6)   =  SPA-SRYYMD
                                                             (1:6)
                       MOVE    PTNYUINRRK-NYUINYMD(7:2)
                                                   TO  IDX-STR
                   ELSE
                       MOVE    1                   TO  IDX-STR
                   END-IF
      *            算定回数の削除
                   PERFORM VARYING     IDX-Y3  FROM    IDX-STR   BY   1
                           UNTIL      (IDX-Y3  >   IDX-END      )  OR
                                      (WRK-SANTEI-CNT  =   ZERO )
                       COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                           -   SANTEI-DAY (IDX-Y3)
                   END-PERFORM
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY2"   TO  ORC-DBPATH
               PERFORM 910-PTNYUINRRK-READ-SEC
      *
           END-PERFORM
      *
           .
       20054-NETAKIRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       200531-SYOSIN-SANTEI-SEC                SECTION.
      *
      *    初診料の算定履歴のチェック
           MOVE    SPACE               TO  WRK-SYOSINYMD1
      *
           EVALUATE    SPA-HOSPSBT
      *        病院
               WHEN    1
      *            老人
                   MOVE    "111700110"         TO  WRK-SRYCD
                   PERFORM 2005332-SANTEI-CHK-SEC
                   IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                          (WRK-YMD             >   WRK-SYOSINYMD1)
                       MOVE    WRK-YMD         TO  WRK-SYOSINYMD1
                   END-IF
      *            老人外
                   MOVE    "111000110"         TO  WRK-SRYCD
                   PERFORM 2005332-SANTEI-CHK-SEC
                   IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                          (WRK-YMD             >   WRK-SYOSINYMD1)
                       MOVE    WRK-YMD         TO  WRK-SYOSINYMD1
                   END-IF
      *        診療所
               WHEN    2
      *            老人
                   MOVE    "111700210"         TO  WRK-SRYCD
                   PERFORM 2005332-SANTEI-CHK-SEC
                   IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                          (WRK-YMD             >   WRK-SYOSINYMD1)
                       MOVE    WRK-YMD         TO  WRK-SYOSINYMD1
                   END-IF
      *            老人外
                   MOVE    "111003610"         TO  WRK-SRYCD
                   PERFORM 2005332-SANTEI-CHK-SEC
                   IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                          (WRK-YMD             >   WRK-SYOSINYMD1)
                       MOVE    WRK-YMD         TO  WRK-SYOSINYMD1
                   END-IF
           END-EVALUATE
      *    小児科診察料
      *    　　院外処方   初診
           MOVE    "113003510"         TO  WRK-SRYCD
           PERFORM 2005332-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD1)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD1
           END-IF
      *    　　院内処方   初診
           MOVE    "113003710"         TO  WRK-SRYCD
           PERFORM 2005332-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD1)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD1
           END-IF
      *
      *    仮の初診料
           MOVE    "099110001"         TO  WRK-SRYCD
           PERFORM 2005332-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD1)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD1
           END-IF
      *
      *    労災初診料
           MOVE    "101110010"         TO  WRK-SRYCD
           PERFORM 2005332-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD1)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD1
           END-IF
      *
           .
      *
       200531-SYOSIN-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       2005332-SANTEI-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-YMD
      *
      *D   MOVE    WRK-SRYCD           TO  TNS-SRYCD
      *D   PERFORM 910-TENSU-READ-SEC
      *
           INITIALIZE                  HZN-SANTEI-REC
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SANTEI-KEY8"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SANTEI-KEY8"       TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM UNTIL       FLG-SANTEI          =   1
      *        終了判定
               IF     (SANTEI-MSANTEICNT   >   ZERO )   AND
                      (SANTEI-SRYYM        <   HZN-SANTEI-SRYYM)
                   MOVE    1                   TO  FLG-SANTEI
               ELSE
      *            対象の算定履歴判定
                   PERFORM 200511-SANTEI-CHK-SEC
                   IF      FLG-SANTEI-OK       =   1
                       MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
                   END-IF
               END-IF
      *
               IF      FLG-SANTEI          =   ZERO
                   MOVE    "SANTEI-KEY8"       TO  ORC-DBPATH
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SANTEI-KEY8"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    HZN-SANTEI-REC          TO  SANTEI-REC
           IF      SANTEI-MSANTEICNT   >   ZERO
               PERFORM VARYING     IDX-Y2  FROM    1  BY  1
                       UNTIL       IDX-Y2      >   31
                   IF      SANTEI-DAY (IDX-Y2) >   ZERO
                       MOVE    IDX-Y2          TO  WRK-DD
                   END-IF
               END-PERFORM
               MOVE    SANTEI-SRYYM        TO  WRK-YYMM
      *        初回慢性疼痛の時、診療日より後の時なしとする
               IF     (WRK-SRYCD       =   "113006510" ) AND
                      (SANTEI-FSANTEIYMD(1:6)
                                           =   SANTEI-SRYYM ) AND
                          (SPA-SRYYMD(1:6) =   SANTEI-SRYYM ) AND
                          (SPA-SRYYMD(7:2) <   SANTEI-MSANTEID)
                   MOVE    SPACE               TO  WRK-YMD
               END-IF
           END-IF
      *
      *    算定履歴検索
      *    INITIALIZE                  SANTEI-REC
      *    MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
      *    MOVE    SPA-PTID            TO  SANTEI-PTID
      *    MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
      *    EVALUATE    TNS-SANTEIRRKKBN
      *        WHEN    1
      *            MOVE    ZERO                TO  SANTEI-NYUGAIKBN
      *            MOVE    ZERO                TO  SANTEI-SRYKA
      *        WHEN    2
      *            MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
      *            MOVE    ZERO                TO  SANTEI-SRYKA
      *        WHEN    3
      *            MOVE    ZERO                TO  SANTEI-NYUGAIKBN
      *            MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
      *        WHEN    4
      *            MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
      *            MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
      *    END-EVALUATE
      *
      *    MOVE    SANTEI-REC          TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
      *        PERFORM 920-SANTEI-READ-SEC
      *    ELSE
      *        MOVE    1                   TO  FLG-SANTEI
      *    END-IF
      *
      *    PERFORM UNTIL       FLG-SANTEI          =   1
      *
      *        IF      SANTEI-MSANTEICNT   >   ZERO
      *            IF      SANTEI-SRYYM    <=  SPA-SRYYMD(1:6)
      *                PERFORM VARYING     IDX-Y2  FROM    1  BY  1
      *                        UNTIL       IDX-Y2      >   31
      *                    IF      SANTEI-DAY (IDX-Y2) >   ZERO
      *                        MOVE    IDX-Y2          TO  WRK-DD
      *                    END-IF
      *                END-PERFORM
      *                MOVE    SANTEI-SRYYM        TO  WRK-YYMM
      *                MOVE    1                   TO  FLG-SANTEI
      *                初回慢性疼痛の時、診療日より後の時なしとする
      *                IF     (WRK-SRYCD       =   "113006510" ) AND
      *                       (SANTEI-FSANTEIYMD(1:6)
      *                                        =   SANTEI-SRYYM ) AND
      *                       (SPA-SRYYMD(1:6) =   SANTEI-SRYYM ) AND
      *                       (SPA-SRYYMD(7:2) <   SANTEI-MSANTEID)
      *                    MOVE    SPACE               TO  WRK-YMD
      *                END-IF
      *            END-IF
      *        END-IF
      *
      *        IF      FLG-SANTEI          =   ZERO
      *            MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
      *            PERFORM 920-SANTEI-READ-SEC
      *        END-IF
      *    END-PERFORM
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *
           .
      *
       2005332-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     労災・自賠責 チェック処理
      *****************************************************************
       20088-RSIJIB-CHK-SEC        SECTION.
      *
      *    包括まとめ入力は以下対象外
          IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      20088-RSIJIB-CHK-EXT
           END-IF
      *
      *     労災のコード
           IF     (LNK-SC92-SRYCD (1:1)    =   "1"  )  AND
                  (LNK-SC92-SRYCD (2:2)    =   "01" )
               IF      SPA-HKNKBN          NOT =   1
                   MOVE    "8000"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    労災（器材）
           IF     (LNK-SC92-SRYCD (1:1)    =   "7"  )  AND
                  (LNK-SC92-SRYCD (2:5)    =   "88888")
               IF      SPA-HKNKBN          NOT =   1
                   MOVE    "8000"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    労災（コメント）
           IF     (LNK-SC92-SRYCD (1:1)    =   "8"  )  AND
                  (LNK-SC92-SRYCD (4:1)    =   "8"  )
               IF      SPA-HKNKBN          NOT =   1
                   MOVE    "8000"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    労災加算振替え
           IF      LNK-SC92-SRYCD          =   "099509901"
               IF      SPA-HKNKBN          NOT =   1
                   MOVE    "8000"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20088-RSIJIB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ内容編集処理
      *****************************************************************
       300-TENSU-HENSYU-SEC           SECTION.
      *
      *    基礎点数
           MOVE    TNS-TEN             TO  SPA-COM-TEN      (IDX-T)
           MOVE    TNS-TEN             TO  SPA-COM-KISOTEN  (IDX-T)
      *    名称
           MOVE    TNS-NAME            TO  SPA-COM-NAME     (IDX-T)
      *
           IF      SPA-HKNKBN          =   1
      *        労災・自賠責 編集
               PERFORM 3000-RSIJIB-HEN-SEC
           END-IF
      *
      *    名称(フリーコメントの時、前回のコメントをのこす）
           IF    ((SPA-COM-INPUTCD (IDX-T)   =   "810000001" )  AND
                  (SPA-GMN-MEISYO-G(IDX-T)   NOT =   SPACE   )) OR
                 ((SPA-COM-INPUTCD (IDX-T)(1:2)  =   "83"    ) AND
                  (SPA-GMN-MEISYO  (IDX-T)   NOT =   SPACE   ))
               CONTINUE
           ELSE
               MOVE    TNS-NAME      TO  SPA-GMN-MEISYO   (IDX-T)
      *        薬剤服用コメントの時、再編集
               IF      SPA-COM-INPUTCD (IDX-T)(1:3)    =   "001"
                    PERFORM 3001-FUKIYO-MEISYO-SEC
                    MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
               END-IF
           END-IF
      *    入力値のある時、再編集
           IF      SPA-COM-INPUTCD (IDX-T)(1:2)    =   "84"
                PERFORM 3002-INPUT-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
           END-IF
      *    診療科名埋め込み再編集
           IF     (SPA-COM-INPUTCD (IDX-T)(1:2)    =   "83")  AND
                  (SPA-COM-ATAI1   (IDX-T)     NOT =   SPACE)
      *D          (LNK-SC92-LSTSRYKA           NOT =   SPACE)
                PERFORM 3003-INPUT-MEISYO2-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
           END-IF
      *
      *    数量単位（薬剤・器材の時）
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)   =   "6"  OR  "7" ) OR
                  (SPA-COM-INPUTCD (IDX-T)(1:3)   =   "059" )  
               PERFORM 3004-TANI-HENSYU-SEC
               MOVE    WRK-TANINAME        TO  SPA-GMN-TANINAME (IDX-T)
           ELSE
               MOVE    SPACE               TO  SPA-GMN-TANINAME (IDX-T)
           END-IF
      *
      *    診療種別区分
           IF      SPA-COM-INPUTCD (IDX-T)(1:1)    =   "1"
               MOVE    TNS-SRYSYUKBN   TO  SPA-COM-TNSSRYSYUKBN(IDX-T)
               MOVE    TNS-SRYSYUKBN   TO  SPA-COM-SRYSYUKBN   (IDX-T)
      *        画像診断の時、すべて"700"とする
               EVALUATE    TNS-SRYSYUKBN(1:1)
                   WHEN    "7"
                       MOVE    "700"       TO  SPA-COM-SRYSYUKBN(IDX-T)
               END-EVALUATE
           END-IF
      *    データ区分
           MOVE    TNS-DATAKBN         TO  SPA-COM-DATAKBN   (IDX-T)
      *    診療区分
           MOVE    TNS-SRYKBN          TO  SPA-COM-TNSSRYKBN (IDX-T)
      *    医薬品　項目
           MOVE    TNS-MADOKUKBN       TO  SPA-COM-MADOKUKBN  (IDX-T)
           MOVE    TNS-SINKEIHAKAIKBN  TO  SPA-COM-SINKEIKBN  (IDX-T)
           MOVE    TNS-SEIBUTUGAKUKBN  TO  SPA-COM-SEIBUTUKBN (IDX-T)
           MOVE    TNS-ZOEIZAIKBN      TO  SPA-COM-ZOEIZAIKBN (IDX-T)
           MOVE    TNS-CSYYOURYO       TO  SPA-COM-CSYYOURYO  (IDX-T)
      *    単位コード
           MOVE    TNS-TANICD          TO  SPA-COM-TANICD    (IDX-T)
      *    薬剤区分
           MOVE    TNS-YKZKBN          TO  SPA-COM-YKZKBN    (IDX-T)
      *    剤型区分
           MOVE    TNS-ZAIGATAKBN      TO  SPA-COM-ZAIGATAKBN (IDX-T)
      *    部位区分
           MOVE    TNS-BUIKBN          TO  SPA-COM-BUIKBN   (IDX-T)
      *    検査等実施判断区分
           MOVE    TNS-KNSJISKBN       TO  SPA-COM-KNSJISKBN (IDX-T)
      *    検査等実施判断グループ
           MOVE    TNS-KNSJISGRPKBN    TO  SPA-COM-KNSJISGRPKBN (IDX-T)
      *    注加算コード
           MOVE    TNS-CHUKSNCD        TO  SPA-COM-CHUKSNCD (IDX-T)
      *    注加算通番
           MOVE    TNS-CHUKSNTUBAN     TO  SPA-COM-CHUKSNTUBAN (IDX-T)
      *    通則加算対象外区分
           MOVE    TNS-TUSOKUGAIKBN    TO  SPA-COM-TUSOKUGAIKBN (IDX-T)
      *    通則年齢区分
           MOVE    TNS-TSUSOKUAGE      TO  SPA-COM-TSUSOKUAGE (IDX-T)
      *    時間加算区分
           MOVE    TNS-TIMEKSNKBN      TO  SPA-COM-TIMEKSNKBN (IDX-T)
      *    外来管理加算区分
           MOVE    TNS-GAIKANRIKBN     TO  SPA-COM-GAIKANRIKBN (IDX-T)
      *    検査用　区分番号
           MOVE    TNS-CDKBN-KBN       TO  SPA-COM-CDKBN-KBN (IDX-T)
           MOVE    TNS-CDKBN-SYO       TO  SPA-COM-CDKBN-SYO (IDX-T)
           MOVE    TNS-CDKBN-BU        TO  SPA-COM-CDKBN-BU  (IDX-T)
      *    労災用　区分番号
           MOVE    TNS-CDKBN-KBNNUM    TO  SPA-COM-CDKBN-KBNNUM (IDX-T)
      *    検査用　包括対象検査
           MOVE    TNS-HOUKNSKBN       TO  SPA-COM-HOUKNSKBN (IDX-T)
      *    算定履歴区分
           MOVE    TNS-SANTEIRRKKBN    TO  SPA-COM-SANTEIRRKKBN (IDX-T)
      *    指導管理料
           MOVE    TNS-SDOKNRYO        TO  SPA-COM-SDOKNRYO (IDX-T)
      *    点数識別
           MOVE    TNS-TENSIKIBETU     TO  SPA-COM-TENSIKIBETU (IDX-T)
      *    特定器材年齢加算区分
           MOVE    TNS-TOKUKIZAIAGEKSNKBN
                                       TO  SPA-COM-TOKUKIZAIAGEKSNKBN
                                                               (IDX-T)
      *    酸素等区分
           MOVE    TNS-SANSOKBN        TO  SPA-COM-SANSOKBN (IDX-T)
      *    特定器材種別１
           MOVE    TNS-TOKUKIZAISBT1   TO  SPA-COM-TOKUKIZAISBT1(IDX-T)
      *    上限点数（器材の上限）
           MOVE    TNS-JGNTEN          TO  SPA-COM-JGNTEN (IDX-T)
      *    保険適用区分
           MOVE    TNS-HKNTEKKBN       TO  SPA-COM-HKNTEKKBN  (IDX-T)
      *    点数欄集計先識別
           MOVE    TNS-GAITENTTLKBN    TO  SPA-COM-GAITENTTLKBN (IDX-T)
      *    点数欄集計先識別
           MOVE    TNS-NYUTENTTLKBN    TO  SPA-COM-NYUTENTTLKBN (IDX-T)
      *    履歴算定用診療行為コード
           MOVE    TNS-SRYCD           TO  SPA-COM-CHK-SRYCD (IDX-T)
      *    きざみ値計算識別判定
           MOVE    TNS-KZMCOMPSIKIBETU TO  SPA-COM-KZMCOMPSIKIBETU
                                                             (IDX-T)
      *    きざみ値計上限
           MOVE    TNS-KZMJGN          TO  SPA-COM-KZMJGN (IDX-T)
      *    検査等実施判定区分
           MOVE    TNS-HOUKATUTEIGENKBN    TO  SPA-COM-HOUKATUTEIGENKBN
                                                             (IDX-T)
      *    年齢加算
           MOVE    TNS-AGEKSNTBL(1)    TO  SPA-COM-AGEKSNTBL (IDX-T 1)
           MOVE    TNS-AGEKSNTBL(2)    TO  SPA-COM-AGEKSNTBL (IDX-T 2)
           MOVE    TNS-AGEKSNTBL(3)    TO  SPA-COM-AGEKSNTBL (IDX-T 3)
           MOVE    TNS-AGEKSNTBL(4)    TO  SPA-COM-AGEKSNTBL (IDX-T 4)
      *    往診区分
           MOVE    TNS-OSINKBN         TO  SPA-COM-OSINKBN   (IDX-T)
      *
      *    きざみ値計算識別判定
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)
                                       =   "1"     )  AND
                  (TNS-KZMCOMPSIKIBETU =   1       )
               PERFORM 3005-SYOCHI-SYUGI-SEC
           END-IF
      *
      *    レセプト用実日数（実日数＝２の日数・回数をセット）
           IF      TNS-JITUDAY         =   2
               MOVE    TNS-DAYCNT      TO  SPA-COM-DAYCNT (IDX-T)
           END-IF
      *
      *    逓減検査区分
           MOVE    TNS-TEIGENKBN       TO  SPA-COM-TEIGENKBN (IDX-T)
      *
      *    後発医薬品区分
           MOVE    TNS-KOUHATUKBN       TO  SPA-COM-KOUHATUKBN (IDX-T)
      *    基準不適合逓減区分
           MOVE    TNS-KIJUNTEIGENKBN   TO  SPA-COM-KIJUNTEIGENKBN
                                                               (IDX-T)
      *    基準不適合逓減対象施設基準
           MOVE    TNS-KIJUNTEIGENCD    TO  SPA-COM-KIJUNTEIGENCD
                                                               (IDX-T)
      *Ｈ１６年４月から
      *    手術施設基準不適合
           MOVE    FLG-SSTKIJUNCD-ERR  TO  SPA-COM-KIJUNCHK-FLG (IDX-T)
      *
      *    点数マスタ付加情報より編集
           PERFORM 3006-TENSUPLUS-HEN-SEC
      *
           .
       300-TENSU-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     労災・自賠責 編集処理
      *****************************************************************
       3000-RSIJIB-HEN-SEC        SECTION.
      *
           IF      SPA-COM-INPUTCD (IDX-T)(1:1)    =   "1"
      *        労災のコード
               IF      SPA-COM-INPUTCD (IDX-T)(2:2)    =   "01"
                   MOVE    1               TO  SPA-COM-RSIKBN (IDX-T)
               END-IF
      *        金額を点数へ置換える
               IF      TNS-TENSIKIBETU     =   1
                   COMPUTE SPA-COM-KISOTEN  (IDX-T)    =   TNS-TEN
                                                       /   10
               END-IF
           END-IF
      *    労災（器材）
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)    =   "7"  )  AND
                  (SPA-COM-INPUTCD (IDX-T)(2:5)    =   "88888")
               MOVE    1               TO  SPA-COM-RSIKBN (IDX-T)
           END-IF
      *    労災（コメント）
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)    =   "8"  )  AND
                  (SPA-COM-INPUTCD (IDX-T)(4:1)    =   "8"  )
               MOVE    1               TO  SPA-COM-RSIKBN (IDX-T)
           END-IF
      *
           .
       3000-RSIJIB-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     薬剤服用コメントの時、再編集処理
      *****************************************************************
       3001-FUKIYO-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3001-FUKIYO-MEISYO-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力値コメントの時、再編集処理
      *****************************************************************
       3002-INPUT-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
           MOVE    TNS-NAME            TO  WRK-MEISYO
           INITIALIZE                  WRK-INPUT-SET-AREA
      *
      *    施設基準コード（入力値編集）
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL   IDX-SST     >   8
               MOVE    TNS-SSTKIJUNCDTBL(IDX-SST)
                                      TO  WRK-INPUT-SET (IDX-SST)
           END-PERFORM
      *
      *    入力値を桁数にあわせて数字変換する
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL  (IDX-SST         >   4     )   OR
                          (SPA-ERRCD   NOT =   SPACE )
               MOVE    WRK-INPUT-KETA (IDX-SST)    TO  WRK-KETA
               COMPUTE WRK-KETA2           =   15  -   WRK-KETA   +  1
               IF     (WRK-INPUT-KETA (IDX-SST)      NOT =   ZERO ) AND
                      (SPA-COM-ATAI (IDX-T IDX-SST)  NOT =   SPACE)
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    SPA-COM-ATAI (IDX-T IDX-SST)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )   OR
                          (SNUM-KETSUU         >   WRK-KETA)
                       MOVE    "0029"          TO  SPA-ERRCD
                   ELSE
                       MOVE    SNUM-OUTEDT(WRK-KETA2:WRK-KETA)
                                               TO  SPA-COM-ATAI
                                                       (IDX-T IDX-SST)
                   END-IF
               ELSE
                   MOVE    SPACE           TO  SPA-COM-ATAI
                                                        (IDX-T IDX-SST)
               END-IF
           END-PERFORM
      *
      *    数字入力エラー時、エラー表示へ
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      3002-INPUT-MEISYO-EXT
           END-IF
      *
      *    漢字変換後、名称へ
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL  (IDX-SST         >   4     )   OR
                          (WRK-INPUT-KETA (IDX-SST)  =   ZERO)
               IF      SPA-COM-ATAI(IDX-T IDX-SST)   =   SPACE
                   CONTINUE
               ELSE
                   MOVE    WRK-INPUT-KETA (IDX-SST)    TO  WRK-KETA
                   MOVE    WRK-INPUT-ICHI (IDX-SST)    TO  WRK-ICHI
                   MOVE    ZERO            TO  WRK-ATAI-X
                   COMPUTE IDX-SS2         =   16   -   WRK-KETA
                   MOVE    SPA-COM-ATAI(IDX-T IDX-SST)(1:WRK-KETA)
                                           TO  WRK-ATAI-X
                                                   (IDX-SS2:WRK-KETA)
                   MOVE    WRK-ATAI-9      TO  WRK-ZZZ9
      *            MOVE    SPA-COM-ATAI(IDX-T IDX-SST)(1:WRK-KETA)
      *                                    TO  WRK-ZZZ9
      *
                   PERFORM 30021-KANJIHEN-SEC
                   COMPUTE WRK-KETA        =   WRK-KETA  *   2
                   COMPUTE IDX-K           =   (10       -   WRK-KETA)
                                           +   1
                   COMPUTE WRK-ICHI2       =   (WRK-ICHI     *  2  )
                                           -   1
                   MOVE    WRK-KANJI(IDX-K:)
                                           TO  WRK-MEISYO
                                                 (WRK-ICHI2:WRK-KETA)
               END-IF
           END-PERFORM
      *
           .
       3002-INPUT-MEISYO-EXT.
           EXIT.
      *
      *****************************************************************
      *     数字漢字変換処理
      *****************************************************************
       30021-KANJIHEN-SEC            SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZZZ9            TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-KANJI
               GO      TO      30021-KANJIHEN-EXT
           END-IF
      *    変換できなかったもの
           MOVE    SPACE               TO  WRK-KANJI
           PERFORM VARYING     IDX-KNJ2    FROM    1   BY  1
                   UNTIL       IDX-KNJ2    >   5
               IF      WRK-ZZZ9-X (IDX-KNJ2:1)  NUMERIC
                   MOVE    WRK-ZZZ9-X (IDX-KNJ2:1) TO  IDX-KNJ1
                   IF      IDX-KNJ1         =   ZERO
                       MOVE    10                  TO  IDX-KNJ
                   ELSE
                       MOVE    IDX-KNJ1            TO  IDX-KNJ
                   END-IF
                   MOVE    TBL-SUJI (IDX-KNJ)  TO  WRK-KANJI-X
                                                      (IDX-KNJ2)
               END-IF
           END-PERFORM
           .
       30021-KANJIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力値コメントの時、再編集処理
      *****************************************************************
       3003-INPUT-MEISYO2-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
           MOVE    TNS-NAME            TO  WRK-MEISYO
           INITIALIZE                  WRK-INPUT-SET-AREA
      *
      *    初診料算定科、再診料算定科のみ対象とする
           IF      SPA-COM-INPUTCD (IDX-T) =   "830000020"  OR
                                               "830000021"
               CONTINUE
           ELSE
               GO      TO      3003-INPUT-MEISYO2-EXT
            END-IF
      *
      *    施設基準コード（入力値編集）
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL   IDX-SST     >   8
               MOVE    TNS-SSTKIJUNCDTBL(IDX-SST)
                                      TO  WRK-INPUT-SET (IDX-SST)
           END-PERFORM
      *
      *    診療科名編集へ
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL  (IDX-SST         >   4     )   OR
                          (WRK-INPUT-KETA (IDX-SST)  =   ZERO)
               IF      SPA-COM-ATAI(IDX-T IDX-SST)   =   SPACE
                   CONTINUE
               ELSE
                   MOVE    WRK-INPUT-KETA (IDX-SST)    TO  WRK-KETA
                   MOVE    WRK-INPUT-ICHI (IDX-SST)    TO  WRK-ICHI
                   COMPUTE WRK-KETA        =   WRK-KETA  *   2
                   COMPUTE IDX-K           =   (10       -   WRK-KETA)
                                           +  1
                   COMPUTE WRK-ICHI2       =   (WRK-ICHI     *  2  )
                                           -  1
                   PERFORM 30031-SRYKA-MEI-HEN-SEC
      *************MOVE    LNK-SC92-LSTSRYKAMEI  TO  WRK-MEISYO
                   MOVE    WRK-SRYKAMEI          TO  WRK-MEISYO
                                                       (WRK-ICHI2:)
               END-IF
           END-PERFORM
      *
           .
       3003-INPUT-MEISYO2-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療科名称編集処理
      *****************************************************************
       30031-SRYKA-MEI-HEN-SEC            SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-COM-ATAI(IDX-T IDX-SST)(1:2)
                                       TO  SYS-1005-KBNCD (1:2)
           MOVE    ZERO                TO  SYS-1005-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    SPACE               TO  WRK-SRYKAMEI
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1   TO  WRK-SRYKAMEI
           END-IF
      *
           .
       30031-SRYKA-MEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     数量単位編集処理
      *****************************************************************
       3004-TANI-HENSYU-SEC            SECTION.
      *
           MOVE    TNS-TANINAME(1:2)     TO  WRK-TANINAME
      *
           EVALUATE    TNS-TANICD
      *            セット
               WHEN    009
                   MOVE    "Set"               TO  WRK-TANINAME
      *            トローチ
               WHEN    013
                   MOVE    TNS-TANINAME(1:2) TO  WRK-TANINAME
      *            アンプル
               WHEN    014
                   MOVE    "A"                 TO  WRK-TANINAME
      *            カプセル
               WHEN    015
                   MOVE    "Cap"               TO  WRK-TANINAME
      *            バイアル
               WHEN    039
                   MOVE    "B"                 TO  WRK-TANINAME
      *            キット
               WHEN    051
                   MOVE    "Kit"               TO  WRK-TANINAME
      *            フィート
               WHEN    030
                   MOVE    "FT"                TO  WRK-TANINAME
      *            ｍｇ
               WHEN    032
                   MOVE    "mg"                TO  WRK-TANINAME
               WHEN    034
                   MOVE    "Kg"                TO  WRK-TANINAME
               WHEN    035
                   MOVE    "cc"                TO  WRK-TANINAME
               WHEN    036
                   MOVE    "mL"                TO  WRK-TANINAME
               WHEN    038
                   MOVE    "mLV"               TO  WRK-TANINAME
               WHEN    040
                   MOVE    "cm"                TO  WRK-TANINAME
               WHEN    041
                   MOVE    "cm2"               TO  WRK-TANINAME
               WHEN    042
                   MOVE    "m"                 TO  WRK-TANINAME
               WHEN    043
                   MOVE    "uCi"               TO  WRK-TANINAME
               WHEN    044
                   MOVE    "mCi"               TO  WRK-TANINAME
               WHEN    045
                   MOVE    "ug"                TO  WRK-TANINAME
               WHEN    048
                   MOVE    "GBq"                TO  WRK-TANINAME
               WHEN    049
                   MOVE    "MBq"                TO  WRK-TANINAME
               WHEN    050
                   MOVE    "KBq"                TO  WRK-TANINAME
               WHEN    058
                   MOVE    "mLg"                TO  WRK-TANINAME
           END-EVALUATE
           .
       3004-TANI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     きざみ値計算・編集処理
      *****************************************************************
       3005-SYOCHI-SYUGI-SEC            SECTION.
      *
      *    きざみ値計算
           MOVE    SPA-COM-SURYO (IDX-T)   TO  WRK-SURYO
           MOVE    ZERO                    TO  WRK-KEI-TEN2
      *    きざみ値判定
           EVALUATE    TRUE
      *            データ≦（下限値−きざみ値）
               WHEN    WRK-SURYO       <=  (TNS-KZMKGN  -  TNS-KZM)
                   IF      TNS-KZMERR          =   2  OR  3
                       MOVE    "0021"          TO  SPA-ERRCD
                   END-IF
      *
      *            下限値＜データ≦上限値
               WHEN     (TNS-KZMKGN        <   WRK-SURYO )   AND
                        (TNS-KZMJGN        >=  WRK-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO -  TNS-KZMKGN)
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                   COMPUTE WRK-KEI-TEN     =   TNS-TEN
                                           + ( WRK-KEI-TEN2
                                           *   TNS-KZMTEN )
                   COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    TNS-KZMJGN        <   WRK-SURYO
                   IF      TNS-KZMERR        =   0  OR  2
      *            点数計算式１をせず、エラーとする
                       MOVE    "0022"        TO  SPA-ERRCD
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KEI-TEN1    =  (TNS-KZMJGN
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   TNS-TEN
                                               + ( WRK-KEI-TEN2
                                               *   TNS-KZMTEN )
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (WRK-KEI-TEN2        >   ZERO  )
               MOVE    WRK-KEI-TEN2    TO  SPA-COM-KISOTEN  (IDX-T)
           END-IF
      *
           .
       3005-SYOCHI-SYUGI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       3006-TENSUPLUS-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSUPLUS-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "TENSUPLUS-KEY"     TO  ORC-DBPATH
               PERFORM 980-TENSUPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "TENSUPLUS-KEY"     TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
      *    採血料区分
           MOVE    TNSP-SAIKETUKBN     TO  SPA-COM-SAIKETUKBN (IDX-T)
      *    入力チェック区分
           MOVE    TNSP-INPCHKKBN      TO  SPA-COM-INPCHKKBN  (IDX-T)
      *
           .
       3006-TENSUPLUS-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    加算行追加編集処理
      *****************************************************************
       400-GYOINSN-SEC                 SECTION.
      *
           IF      SPA-GMN-INPUTCD (200)   NOT =   SPACE
               MOVE    "0007"          TO  SPA-ERRCD
               GO      TO      400-GYOINSN-EXT
           END-IF
      *
      *    加算行を開ける（対象行の次に追加）
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX1    FROM    200 BY  -1
                   UNTIL       IDX1    <   IDX-T
               IF      SPA-GMN-INPUTCD (IDX1)  NOT =   SPACE
                   COMPUTE IDX2            =   IDX1    +   1
                   MOVE    SPA-COM-TBLREC (IDX1)
                                           TO  SPA-COM-TBLREC (IDX2)
                   MOVE    SPA-GMN-TBLREC (IDX1)
                                           TO  SPA-GMN-TBLREC (IDX2)
                   INITIALIZE              SPA-COM-TBLREC (IDX1)
                   INITIALIZE              SPA-GMN-TBLREC (IDX1)
               END-IF
           END-PERFORM
      *
           IF      SPA-COM-INPUTCD(IDX-T)  NOT =   SPACE
               MOVE    "0020"              TO  SPA-ERRCD
           ELSE
               MOVE    LNK-SC92-SRYCD      TO  SPA-COM-INPUTCD(IDX-T)
               MOVE    LNK-SC92-SRYCD      TO  SPA-GMN-INPUTCD(IDX-T)
           END-IF
           .
       400-GYOINSN-EXT.
           EXIT.
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *
      *****************************************************************
      *    入力コードの主表示コードに編集処理
      *****************************************************************
       400-INPUTCD-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-COM-ICD-INPUTCD(IDX-T)
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPID          TO  ICD-HOSPID
           MOVE    SPA-COM-INPUTCD(IDX-T)
                                       TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "INPUTCD-KEY4"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "INPUTCD-KEY4"      TO  ORC-DBPATH
               PERFORM                     920-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "INPUTCD-KEY4"      TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
           IF      FLG-INPUTCD         =   1
               GO      TO      400-INPUTCD-HEN-EXT
           END-IF
      *
           MOVE    ICD-INPUTCD         TO  SPA-COM-ICD-INPUTCD(IDX-T)
      *
           .
       400-INPUTCD-HEN-EXT.
           EXIT.
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    入力コードの主表示コードに編集処理
      *****************************************************************
       500-GMN-HEN-SEC                SECTION.
      *
      *    数量・回数等の初期化
           MOVE    1                   TO  SPA-COM-SURYO  (IDX-T)
           MOVE    SPACE               TO  SPA-COM-SURFLG (IDX-T)
           MOVE    1                   TO  SPA-COM-BUNGSU (IDX-T)
           MOVE    1                   TO  SPA-COM-KAISU  (IDX-T)
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    LNK-SC92-PG         TO  LNK-SC94-PG
           MOVE    IDX-T               TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           .
       500-GMN-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    数量・回数のみ変更の時の編集処理
      *****************************************************************
       300-SURYOHEN-SEC         SECTION.
      *
      *    回数が１以上の時、算定回数チェック
           IF     (SPA-COM-INPUTCD(IDX-T)(1:1)     =   "1" )  AND
                  (SPA-COM-SRYKBN (IDX-T)(1:1) NOT =  "9"  )  AND
                  (SPA-COM-KAISU  (IDX-T)          >  1    )
               MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
      *        算定回数チェック
               IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                   PERFORM 2001-SANTEI-CHK-SEC
                   IF      LNK-SC92-ERRCD3 NOT =   SPACE
                       GO      TO      300-SURYOHEN-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    入力値のある時、再編集
           IF      SPA-COM-INPUTCD (IDX-T)(1:2)    =   "84"
               MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   PERFORM 3002-INPUT-MEISYO-SEC
                   MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
               ELSE
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    診療科名埋め込み再編集
           IF     (SPA-COM-INPUTCD (IDX-T)(1:2)    =   "83")  AND
                  (SPA-COM-ATAI1   (IDX-T)     NOT =   SPACE)
               MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                    PERFORM 3003-INPUT-MEISYO2-SEC
                    MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
               ELSE
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    きざみ値計算識別判定
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)
                                       =   "1"     )  AND
                  (SPA-COM-KZMCOMPSIKIBETU (IDX-T) =  1 )
               MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
      *            ＳＰＡ編集
                   PERFORM 300-TENSU-HENSYU-SEC
               ELSE
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       300-SURYOHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
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
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込（算定履歴検索用）
      *****************************************************************
       911-TENSU-READ22-SEC         SECTION.
      *
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY22"       TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY22"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "TENSU-KEY22"       TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
           .
       911-TENSU-READ22-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       920-INPUTCD-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェックデータ読込
      *****************************************************************
       950-CHK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  CHK-REC
               MOVE    ZERO                TO  FLG-CHKMST
           ELSE
               INITIALIZE                      CHK-REC
               MOVE    1                   TO  FLG-CHKMST
           END-IF
      *
           .
       950-CHK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       980-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .
      *
       980-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人一般コード変換読込
      *****************************************************************
       990-SRYCDCHG-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHG-REC
               MOVE    ZERO                TO  FLG-SRYCDCHG
           ELSE
               INITIALIZE                      SRYCDCHG-REC
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           .
       990-SRYCDCHG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *
