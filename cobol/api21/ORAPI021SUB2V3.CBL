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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI021SUB2V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療内容編集サブ（一体化）
      *                      （WKSRYACT を API返却編集）
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/05/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  16/12/XX  院内・院外区分対応
      *  04.08.00    NACL-多々納  17/05/XX  包括検査数、単位コード対応
      *                                     フィルム分画数等対応
      *  05.00.00    NACL-多々納  18/03/XX  Ｈ３０年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-GENERICNAME         PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
      *
           03  FLG-HENCHK              PIC 9(01).
           03  FLG-SP                  PIC 9(01).
      *H28.12
           03  FLG-INGAIKBN            PIC 9(01).
           03  FLG-INGAIKBN-ARI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
      *
           03  IDX-A               PIC 9(04).
      *
           03  IDXM                PIC 9(04).
           03  IDXR                PIC 9(04).
      *
      *    カウント領域
      *01  CNT-AREA.
      *    03  CNT-SRYCNT              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
      *    数値変換用
           03  WRK-SURYO               PIC S9(08)V9(05).
           03  WRK-SURYO-S             PIC 9(08)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO-S.
               05  WRK-SURYO-S1        PIC 9(08).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO2-S1           PIC S9(08).
      *
           03  WRK-SURYO-XX            PIC X(15).
           03  WRK-SURYO-X             REDEFINES    WRK-SURYO-XX.
               05  WRK-SURYO-Z1        PIC -(08)9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-HX            PIC X(15).
           03  WRK-S-MAX               PIC 9(02).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(40).
           03  WRK-SRYCD               PIC X(09).
      *    包括検査数
           03  WRK-HOUKNS-CNT          PIC 9(02).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *
       01  LNK-WKSRYACT-MAX            PIC 9(04).
      *
      *返却剤数
       01  COB-ZAI-OUTMAX          PIC 9(04)   VALUE   120.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
      *    COPY    "CPORCSMCNDATE.INC".
      *
      *    COPY    "CPORCXKANACONV.INC".
      *
      *    COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    一般名テーブル
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *     パラメタテーブル
      *01  PARA-REC.
      *    COPY    "CPPARA.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *
      *    COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "MCPAREA".
      *    スパ領域
           COPY    "COMMON-SPA".
      *    ワーク診療行為データ編集領域
           COPY    "CPORAPI021SUB2V3.INC".
      *
      *    ワーク診療行為マスタ−
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
      *    附加情報を追加
           03  LNK-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //LNK-WKSRYP-//.
      *    API用剤削除連番
           03  LNK-WKSRYAPI-RENNUM         PIC 9(02).
      *    剤削除区分
           03  LNK-WKSRYAPI-DELKBN         PIC 9(01).
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA
           ORAPI021SUB2V3AREA
           LNK-WKSRYACT-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORAPISUB2-ERRCD
           INITIALIZE                  ORAPISUB2-ERRMSG
           INITIALIZE                  ORAPISUB2-HENSYU-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           MOVE    ORAPISUB2-WKSRYACT-MAX  TO  LNK-WKSRYACT-MAX
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDZ
           MOVE    ZERO                TO  FLG-HENCHK
      *H28.12
      *    投薬判定
           MOVE    ZERO                TO  FLG-INGAIKBN
           MOVE    ZERO                TO  FLG-INGAIKBN-ARI
           MOVE    ZERO                TO  WRK-HOUKNS-CNT
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   LNK-WKSRYACT-MAX)
                       OR  (FLG-HENCHK NOT =   ZERO  )
      *
               IF      (IDZ            =   ZERO )
                  OR  ((IDX            >   1    )  AND
                       (LNK-WKSRY-ZAINUM (IDX - 1)
                                   NOT =   LNK-WKSRY-ZAINUM(IDX)))
      *H29.5
      *            包括検査の検査数
                   IF     (WRK-HOUKNS-CNT      >   ZERO )
                      AND (IDY                 >   ZERO )
                       MOVE    WRK-HOUKNS-CNT
                                               TO  ORAPISUB2-HKTKNSA-CNT
                                                                 (IDY)
                   END-IF
                   MOVE    ZERO                TO  WRK-HOUKNS-CNT
                   ADD     1                   TO  IDY
                   IF      IDY                 >   COB-ZAI-OUTMAX
                       MOVE    1               TO  FLG-HENCHK
                   END-IF
                   MOVE    ZERO            TO  IDZ
               END-IF
               PERFORM 2002341-WKSRYHEAD-HEN-SEC
               PERFORM VARYING   IDW       FROM    1   BY  1
                       UNTIL    (IDW           >   5   )
                           OR   (FLG-HENCHK    =   1   )
                           OR   (LNK-WKSRY-SRYCD  (IDX IDW) =   SPACE)
                   ADD     1               TO  IDZ
                   IF      IDZ                 >   50
                       MOVE    1               TO  FLG-HENCHK
                   END-IF
                   IF      FLG-HENCHK          =   ZERO
                       PERFORM 2002342-WKSRYMEI-HEN-SEC
                   END-IF
      *H28.12
      *            投薬の判定
                   IF     (FLG-HENCHK          =   ZERO )
                     AND  (LNK-WKSRY-SRYKBN(IDX)
                                               =   "21"
                                               OR  "22"
                                               OR  "23" )
      *             薬評以外
                     AND  (LNK-WKSRY-ZAIKBN (IDX)
                                           NOT =   1   )
                       PERFORM 2002343-INGAI-CHK-SEC
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
      *H28.12
      *    院内・院外区分
           IF      FLG-INGAIKBN-ARI    =   1
               MOVE    FLG-INGAIKBN        TO  ORAPISUB2-INGAIKBN
           END-IF
      *
           IF     (FLG-HENCHK          =   1     )
      *        剤数オーバー
               MOVE    "AP03"              TO  ORAPISUB2-ERRCD
               MOVE    "剤数・明細数が最大件数以上となります"
                                           TO  ORAPISUB2-ERRMSG
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *     剤編集処理
      *****************************************************************
       2002341-WKSRYHEAD-HEN-SEC          SECTION.
      *
      *    回数
           IF      LNK-WKSRY-ZAIKAIKEI(IDX) >   ZERO
               MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  ORAPISUB2-MDC-CLASS-NUMBER
                                                             (IDY)
           END-IF
      *    剤点数
           IF      LNK-WKSRY-ZAITENKEI(IDX) NOT =   ZERO
      *        減点
               IF    ( LNK-WKSRYP-PLUSKBN (IDX) =   1 )
               OR    ( LNK-WKSRY-ZAIKBN   (IDX) =   2 )
                   COMPUTE WRK-SURYO       =   LNK-WKSRY-ZAITENKEI(IDX)
                                           *   -1
               ELSE
                   MOVE    LNK-WKSRY-ZAITENKEI(IDX) TO  WRK-SURYO
               END-IF
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  ORAPISUB2-MDC-POINT(IDY)
           END-IF
      *    剤金額
           IF      LNK-WKSRY-JIHIMONEYTOTAL(IDX)   >   ZERO
               MOVE    LNK-WKSRY-JIHIMONEYTOTAL(IDX)
                                           TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  ORAPISUB2-MDC-MONEY(IDY)
           END-IF
      *    剤区分（包括分）
           IF      LNK-WKSRY-CONTKBN (IDX) =   "H"
                MOVE    "1"            TO  ORAPISUB2-MDC-CODE (IDY)
           END-IF
      *    診療区分
           MOVE    LNK-WKSRY-SRYKBN(IDX)   TO  WRK-SRYKBN
           MOVE    LNK-WKSRY-SRYSYUKBN(IDX)    TO  WRK-SRYSYUKBN
           MOVE    WRK-SRYKBN          TO  ORAPISUB2-MDC-CLASS-KBN (IDY)
           MOVE    WRK-SRYSYUKBN       TO  ORAPISUB2-MDC-CLASS     (IDY)
           IF      WRK-SRYSYUKBN       =   SPACE
               MOVE    WRK-SRYKBN          TO  WRK-SRYSYUKBN(1:2)
               MOVE    "0"                 TO  WRK-SRYSYUKBN(3:1)
           END-IF
           PERFORM 510-SRYKBN-MEI-SEC
           MOVE    WRK-SRYSYUKBNMEI    TO  ORAPISUB2-MDC-CLASS-NAME(IDY)
      *
      *    削除対象連番
           IF     (ORAPISUB2-SYORIKBN  =   "1"    )
             AND  (LNK-WKSRYAPI-RENNUM (IDX)   NOT =   ZERO )
               MOVE    LNK-WKSRYAPI-RENNUM (IDX)
                                           TO  ORAPISUB2-DEL-NUMBER
                                                             (IDY)
           END-IF
      *    削除対象
           IF      LNK-WKSRYAPI-DELKBN (IDX)   =   1
               MOVE    "1"                 TO  ORAPISUB2-DEL-INF
                                                             (IDY)
           END-IF
           .
       2002341-WKSRYHEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤明細編集処理
      *****************************************************************
       2002342-WKSRYMEI-HEN-SEC          SECTION.
      *
      *    コード
           MOVE    LNK-WKSRY-SRYCD (IDX IDW)
                                   TO ORAPISUB2-PRSCRPT-CODE(IDY IDZ)
      *
           MOVE    LNK-WKSRY-SRYCD (IDX IDW)
                                       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    名称
           IF     (LNK-WKSRY-SRYCD (IDX IDW)(1:1) =   "8")
             OR   (LNK-WKSRY-SRYCD (IDX IDW)(1:3) =   "008")
             OR   (LNK-WKSRY-SRYCD (IDX IDW)(1:3) =   "001")
      *H30.4
             OR   (LNK-WKSRY-SRYCD (IDX IDW)(1:7) =   "0992081")
      *        編集名称
               IF      LNK-WKSRY-INPUTCOMENT (IDX IDW) NOT =   SPACE
                   MOVE    LNK-WKSRY-INPUTCOMENT (IDX IDW) 
                                           TO  ORAPISUB2-PRSCRPT-NAME
                                                             (IDY IDZ)
               END-IF
           END-IF
           IF      ORAPISUB2-PRSCRPT-NAME (IDY IDZ)   =   SPACE
      *        点数マスタの名称
               MOVE    TNS-NAME            TO  ORAPISUB2-PRSCRPT-NAME
                                                             (IDY IDZ)
           END-IF
      *
      *    一般名記載
           IF     (LNK-WKSRY-SRYCD (IDX IDW)(1:1) =   "6")
              AND (LNK-WKSRY-INPUTCOMENT (IDX IDW)
                                       NOT =   SPACE  )
               IF      LNK-WKSRY-INPUTCOMENT (IDX IDW) =   "API+1"
      *            コメント名称に薬価基準コード
                   PERFORM 9102-GENERICNAME-READ-SEC
                   IF      FLG-GENERICNAME     =   ZERO
                       MOVE    GENERIC-GENERICNAME
                                           TO  ORAPISUB2-PRSCRPT-GENNAME
                                                             (IDY IDZ)
                       MOVE    "1"         TO  ORAPISUB2-PRSCRPT-GENERIC
                                                             (IDY IDZ)
                   END-IF
               ELSE
                   MOVE    "3"         TO  ORAPISUB2-PRSCRPT-GENERIC
                                                             (IDY IDZ)
      *            点数附加情報検索
                   PERFORM 9101-TENSUPLUS-READ-SEC
                   MOVE    TNSP-SHONAME
                                           TO  ORAPISUB2-PRSCRPT-GENNAME
                                                             (IDY IDZ)
               END-IF
           END-IF
      *
      *    数量
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDW) TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  ORAPISUB2-PRSCRPT-NUMBER
                                                             (IDY IDZ)
      *H29.5
      *    単位コード
           IF     (LNK-WKSRY-SRYCD (IDX IDW)(1:1)  =   "6"
                                                   OR  "7" )
      *        薬剤・器材
               MOVE    TNS-TANICD      TO  ORAPISUB2-PRSCRPT-TANICD
                                                             (IDY IDZ)
               MOVE    TNS-TANINAME    TO  ORAPISUB2-PRSCRPT-TANICD-NAME
                                                             (IDY IDZ)
           END-IF
           IF    ((LNK-WKSRY-SRYCD (IDX IDW)(1:1)  =   "1" )  OR
                  (LNK-WKSRY-SRYCD (IDX IDW)(1:3)  =   "058"
                                                   OR  "059"
                                                   OR  "095"
                                                   OR  "096"))
               IF      TNS-TANICD          NOT =   ZERO
                   MOVE    TNS-TANICD      TO  ORAPISUB2-PRSCRPT-TANICD
                                                             (IDY IDZ)
                   MOVE    TNS-TANINAME    TO
                                           ORAPISUB2-PRSCRPT-TANICD-NAME
                                                             (IDY IDZ)
               END-IF
           END-IF
      *
      *    分画数を編集
           IF     (LNK-WKSRY-SRYKBN (IDX)  =   "70"   )
              AND (LNK-WKSRY-SRYCD (IDX IDW)(1:1)  =   "7" )
              AND (LNK-WKSRY-SRYKAISU(IDX IDW) NOT =   ZERO)
               MOVE    LNK-WKSRY-SRYKAISU(IDX IDW) TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  ORAPISUB2-PRSCRPT-FILMNUM
                                                             (IDY IDZ)
           END-IF
      *H29.5
      *    加算自動算定なし
           IF     (LNK-WKSRY-SRYCD (IDX IDW)(1:1)  =   "1")
             AND  (TNS-DATAKBN             =   "1"   )
      *        在宅加算等の自動算定なし設定
               IF      (TNS-CDKBN-KBN          =   "C"   )
                  AND  (TNS-CDKBN-KBNNUM       =   "002" )
                   MOVE    "Yes"           TO  ORAPISUB2-PRSCRPT-NOAUTO
                                                             (IDY IDZ)
               END-IF
      *        精神通院の２０歳未満加算の自動発生なし
               IF      (TNS-CDKBN-KBN          =   "I"   )
                  AND  (TNS-CDKBN-KBNNUM       =   "002" )
                  AND  (TNS-AGEKSNSRYCD(1) NOT =   ZERO
                                               AND SPACE )
                   MOVE    "Yes"           TO  ORAPISUB2-PRSCRPT-NOAUTO
                                                             (IDY IDZ)
               END-IF
      *H29.7
      *        心身療法の２０歳未満加算の自動発生なし
               IF      (TNS-CDKBN-KBN          =   "I"   )
                  AND  (TNS-CDKBN-KBNNUM       =   "004" )
                  AND  (TNS-AGEKSNSRYCD(1) NOT =   ZERO
                                               AND SPACE )
                  AND  (SPA-SRYYMD             >=  "20170701")
                   MOVE    "Yes"           TO  ORAPISUB2-PRSCRPT-NOAUTO
                                                             (IDY IDZ)
               END-IF
           END-IF
      *
      *    自費金額
           IF     (LNK-WKSRY-SRYKBN (IDX)  =   "96"
                                           OR  "95"   )
      *        自賠責金額
           OR     (LNK-WKSRY-SRYSYUKBN (IDX)    =   "809" )
               IF      LNK-WKSRY-JIHIMONEY(IDX IDW) >   ZERO
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDW) TO  WRK-SURYO
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  ORAPISUB2-PRSCRPT-MONEY
                                                             (IDY IDZ)
               END-IF
           END-IF
      *    器材の商品名入力がある時
           IF     (LNK-WKSRY-SRYCD (IDX IDW)(1:1)  =   "7" )
             AND  (LNK-WKSRY-AUTOKBN (IDX IDW)     =   "T" )
               MOVE    "2"        TO  ORAPISUB2-PRSCRPT-CONTKBN(IDY IDZ)
           END-IF
      *    自動算定区分
           IF      LNK-WKSRY-AUTOKBN (IDX IDW) =   "2"
                                               OR  "3"
                                               OR  "4"
                                               OR  "5"
                                               OR  "6"
                                               OR  "7"
               MOVE    LNK-WKSRY-AUTOKBN (IDX IDW)
                                           TO  ORAPISUB2-PRSCRPT-AUTOKBN
                                                             (IDY IDZ)
           END-IF
      *    継続コメント
           IF     (LNK-WKSRY-SRYCD (IDX IDW)(1:1) =   "8")
             OR   (LNK-WKSRY-SRYCD (IDX IDW)(1:3) =   "008")
               IF     (LNK-WKSRY-INPUTKBN (IDX IDW)    =   "1" )
                   MOVE    "1"        TO  ORAPISUB2-PRSCRPT-CONTKBN
                                                             (IDY IDZ)
               END-IF
           END-IF
      *    内服１種類区分
           IF     (LNK-WKSRY-SRYCD (IDX IDW)(1:1) =   "6")
               IF     (LNK-WKSRY-INPUTKBN (IDX IDW)    =   "2" )
                   MOVE    "1"        TO  ORAPISUB2-PRSCRPT-NAIFKBN
                                                             (IDY IDZ)
               END-IF
           END-IF
      *    埋め込み入力値
           IF     (LNK-WKSRY-SRYCD (IDX IDW)(1:2)  =   "84" )
              OR  (LNK-WKSRY-SRYCD (IDX IDW)(1:4)  =   "0084")
      *       用法コメント
              OR  (LNK-WKSRY-SRYCD (IDX IDW)(1:3)  =   "001")
      *H30.4
             OR   (LNK-WKSRY-SRYCD (IDX IDW)(1:7) =   "0992081")
               PERFORM VARYING     IDX-A   FROM    1   BY  1
                       UNTIL      (IDX-A       >   5   )
                   IF      LNK-WKSRY-INPUTCHI (IDX IDW IDX-A)
                                           NOT =   SPACE
                       MOVE    LNK-WKSRY-INPUTCHI (IDX IDW IDX-A)
                                       TO  ORAPISUB2-PRSCRPT-ATAI
                                                     (IDY IDZ IDX-A)
                   END-IF
               END-PERFORM
           END-IF
      *H29.5
      *    包括検査の判定
           IF     (LNK-WKSRY-SRYCD (IDX IDW)(1:1) =   "1" )  AND
                  (LNK-WKSRY-SRYKBN (IDX)      =   "60" ) AND
                  (TNS-DATAKBN                 =   1    ) AND
                  (TNS-HOUKNSKBN           NOT =   ZERO )
               ADD     1                   TO  WRK-HOUKNS-CNT
           END-IF
      *!
      *
           .
       2002342-WKSRYMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     院内・院外投薬の判定
      *****************************************************************
       2002343-INGAI-CHK-SEC        SECTION.
      *
      *    訂正の時、診療種別が未入力で院外の薬剤があった時
           IF     (LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE
                                               OR  "210"
                                               OR  "220"
                                               OR  "230"
                                               OR  "290"
                                               OR  "293"
                                               OR  "296" )
                MOVE    1                   TO  FLG-INGAIKBN-ARI
                IF      LNK-WKSRY-ZAITENKEI (IDX)   =   ZERO
      *            剤点数＝ゼロ　院外
      *            剤内に薬剤がある時のみ
                   IF     (LNK-WKSRY-SRYCD (IDX IDW)(1:1)
                                               =   "6" )
                       IF      FLG-INGAIKBN    =   ZERO
                           MOVE    1               TO  FLG-INGAIKBN
                       END-IF
                   END-IF
      *            ユーザ器材で、金額＞ゼロの時、院外とする
                   IF      FLG-INGAIKBN    =   ZERO
                       IF     ( LNK-WKSRY-SRYCD (IDX IDW)(1:3)
                                                   =   "059" )
                           IF      TNS-TEN         >   ZERO
                               MOVE    1           TO  FLG-INGAIKBN
                           END-IF
                       END-IF
                   END-IF
               ELSE
      *            剤点数≠ゼロ　院内
                   MOVE    2               TO  FLG-INGAIKBN
               END-IF
           END-IF
      *
           .
       2002343-INGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
               AT   END
                   MOVE    SPACE           TO  WRK-SRYKBN
                   MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
               WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                   MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                   MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値変換編集処理
      *****************************************************************
       810-NUMHENKAN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           IF      WRK-SURYO           <   ZERO
               COMPUTE WRK-SURYO-S         =   WRK-SURYO      *  -1
               COMPUTE WRK-SURYO2-S1       =   WRK-SURYO-S1   *  -1
               MOVE    WRK-SURYO2-S1       TO  WRK-SURYO-Z1
           ELSE
               MOVE    WRK-SURYO           TO  WRK-SURYO-S
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
           END-IF
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1)    NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)    TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SURYO-Z2
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *    左詰
           MOVE    SPACE              TO  WRK-SURYO-HX
           MOVE    1                  TO  IDXR
           MOVE    ZERO               TO  WRK-S-MAX
           PERFORM VARYING     IDXM    FROM    1    BY  1
                   UNTIL       IDXM    >   15
               IF      WRK-SURYO-X (IDXM:1)    NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:1)    TO  WRK-SURYO-HX
                                                       (IDXR:1)
                   COMPUTE IDXR            =   IDXR    +   1
      *            桁数
                   ADD     1               TO  WRK-S-MAX
               END-IF
           END-PERFORM
           .
       810-NUMHENKAN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9102-GENERICNAME-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  GENERICNAME-REC
           INITIALIZE                      GENERICNAME-REC
           MOVE    TNS-YAKKAKJNCD      TO  GENERIC-YAKKAKJNCD
      *
           MOVE    GENERICNAME-REC     TO  MCPDATA-REC
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_genericname"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICNAME-REC
                   MOVE    ZERO                TO  FLG-GENERICNAME
               ELSE
                   INITIALIZE                  GENERICNAME-REC
                   MOVE    1                   TO  FLG-GENERICNAME
               END-IF
           ELSE
               INITIALIZE                      GENERICNAME-REC
               MOVE    1                   TO  FLG-GENERICNAME
           END-IF
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9102-GENERICNAME-READ-EXT.
           EXIT.
      *
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
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
