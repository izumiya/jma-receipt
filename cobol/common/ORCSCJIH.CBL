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
       PROGRAM-ID.                 ORCSCJIH.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（自費）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/04/12    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/11/20  自賠責器材追加
      * 01.00.02     NACL-多々納  03/04/30  剤別点数追加
      *****************************************************************
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
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-MOJI            PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *
           03  WRK-KINGAK-9        PIC 9(08)V99.
           03  WRK-KINGAK          PIC 9(08).
      *
           03  WRK-ZEIKIN-9        PIC 9(08)V9(5).
           03  WRK-ZEIKIN          PIC 9(08).
           03  WRK-TANKA           PIC 9(08)V99.
           03  WRK-KISOTEN         PIC 9(08)V99.
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           EVALUATE    LNK-ORCSC-KBN
      *        剤別　チェック、編集、自動算定
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        全体のチェック
               WHEN    "2"
                   PERFORM 200-ALL-SYORI-SEC
      *        自動算定
               WHEN    "3"
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    OTHER
                   MOVE    "1001"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    剤別のチェック、編集、自動算定処理
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    チェック、自動算定
           PERFORM 1001-NAI-CHK-SEC
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    点数計算
           PERFORM 1002-TENSU-KEI-SEC
      *
           .
       100-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       1001-NAI-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "S"
                                                               OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
                   WHEN    "S"
      *                セットは入力不可とする
                       MOVE    "0055"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
           MOVE    WRK-ERRCD          TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       10011-CHK-SEC                  SECTION.
      *
           EVALUATE    WRK-MOJI
               WHEN    "0"
      *            金額入力のする自費は回数は１とする
      *!           IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "095" OR
      *!                                                   "096" ) AND
      *!                  (WRK-COM-KISOTEN(IDX-Z)      =   ZERO  )
      *!               IF      LNK-ORCSC-KAISU         >   1
      *!                   MOVE    "0008"          TO  WRK-ERRCD
      *!                   MOVE    "1"             TO  WRK-COM-CUR
      *!                                                  (IDX-Z)
      *!               END-IF
      *!           END-IF
      *            Ｈ１９／４／１から税込み追加
                   IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "095" OR
                                                           "096" )
                       IF     (WRK-COM-TENSIKIBETU(IDX-Z)  =  4  )  AND
                              (WRK-COM-SRYKBN  (IDX-Z)     =  "95")
                           MOVE    "0086"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-Z)
                       END-IF
                   END-IF
      *
               WHEN    OTHER
      *            計算できる点数識別であること
                   EVALUATE    WRK-COM-TENSIKIBETU(IDX-Z)
                       WHEN    1
                       WHEN    2
                       WHEN    3
                       WHEN    4
                           CONTINUE
                       WHEN    OTHER
                           MOVE    "0055"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-Z)
                   END-EVALUATE
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-TENSU (IDX-Z)
      *
               MOVE    ZERO                TO  WRK-COM-SYUTEN1 (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-SYUTEN2 (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-KIZAITEN (IDX-Z)
               MOVE    ZERO                TO  WRK-KINGAK
      *
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR  "S"
                                                                OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
      *                自費計算へ
                       PERFORM 10021-JIHI-KEI-SEC
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
      *
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "095" OR
                                                       "096" ) AND
                      (WRK-COM-KISOTEN(IDX-Z)      =   ZERO  )
                   COMPUTE WRK-KINGAK          =   WRK-COM-JIHIMONEY
                                                               (IDX-Z)
                                               *   LNK-ORCSC-KAISU
                   MOVE    WRK-KINGAK          TO  WRK-COM-KEI (IDX-Z)
                   MOVE    WRK-KINGAK          TO  WRK-GMN-KEI (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-KEIFLG(IDX-Z)
               ELSE
      *
                   MOVE    WRK-KINGAK          TO  WRK-COM-JIHIMONEY
                                                               (IDX-Z)
                   COMPUTE WRK-KINGAK          =   WRK-KINGAK
                                               *   LNK-ORCSC-KAISU
                   MOVE    WRK-KINGAK          TO  WRK-COM-KEI (IDX-Z)
                   MOVE    WRK-KINGAK          TO  WRK-GMN-KEI (IDX-Z)
      *
                   MOVE    "2"                 TO  WRK-COM-KEIFLG(IDX-Z)
               END-IF
               MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU(IDX-Z)
               MOVE    LNK-ORCSC-KAISU     TO  WRK-GMN-KAISU(IDX-Z)
           END-PERFORM
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費計算処理
      *****************************************************************
       10021-JIHI-KEI-SEC                  SECTION.
      *
           IF      WRK-COM-KISOTEN(IDX-Z)      =   ZERO
               CONTINUE
           ELSE
      *        消費税計算
               IF      (SPA-SRYYMD     >=  "20070401" )  AND
                       (WRK-COM-SRYKBN(IDX-Z)  =   "96")  AND
                       (WRK-COM-TENSIKIBETU(IDX-Z)  =  0 )
                   MOVE    WRK-COM-KISOTEN (IDX-Z)  TO  WRK-KISOTEN
                   PERFORM 10021-ZEIKEISEN-SEC
               ELSE
                   MOVE    WRK-COM-KISOTEN (IDX-Z)  TO  WRK-TANKA
               END-IF
               COMPUTE WRK-KINGAK      =   WRK-TANKA
                                       *   WRK-COM-SURYO(IDX-Z)
           END-IF
      *
           .
       10021-JIHI-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    消費税計算処理
      *****************************************************************
       10021-ZEIKEISEN-SEC                  SECTION.
      *
           COMPUTE WRK-ZEIKIN-9        =   WRK-KISOTEN
                                       *   0.05
           IF      SPA-TAX-HASU-KBN    =   "1"
      *        小数点以下四捨五入
               COMPUTE WRK-ZEIKIN      =   WRK-ZEIKIN-9
                                       +   0.5
           ELSE
      *        小数点以下切り捨て
               MOVE    WRK-ZEIKIN-9        TO  WRK-ZEIKIN
           END-IF
           COMPUTE WRK-TANKA           =   WRK-KISOTEN
                                       +   WRK-ZEIKIN
           .
       10021-ZEIKEISEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
           IF     (WRK-COM-INPUTCD(IDX-Z)(1:5) =   "09591" OR
                                                   "09592" OR
                                                   "09593" OR
                                                   "09594" ) AND
                  (WRK-COM-KISOTEN(IDX-Z)  NOT =   WRK-COM-TEN(IDX-Z))
               MOVE    WRK-COM-TEN(IDX-Z)  TO  WRK-COM-KISOTEN(IDX-Z)
           END-IF
      *
           EVALUATE    WRK-COM-TENSIKIBETU(IDX-Z)
      *        金額
               WHEN    1
      *            消費税計算
                   IF      (SPA-SRYYMD     >=  "20070401" )  AND
                           (WRK-COM-SRYKBN(IDX-Z)  =   "96")
                       MOVE    WRK-COM-KISOTEN (IDX-Z)  TO  WRK-KISOTEN
                       PERFORM 10021-ZEIKEISEN-SEC
                   ELSE
                       MOVE    WRK-COM-KISOTEN (IDX-Z)  TO  WRK-TANKA
                   END-IF
                   IF      WRK-COM-KZMCOMPSIKIBETU (IDX-Z)  =   "1"
                       MOVE    WRK-TANKA           TO  WRK-KINGAK
                   ELSE
                       COMPUTE WRK-KINGAK-9    =   WRK-TANKA
                                               *   WRK-COM-SURYO(IDX-Z)
                                               +   0.5
                       MOVE    WRK-KINGAK-9    TO  WRK-KINGAK
                   END-IF
      *        点数
               WHEN    3
               WHEN    4
                   COMPUTE WRK-KISOTEN     =   WRK-COM-KISOTEN (IDX-Z)
                                           *   10
                   IF      (SPA-SRYYMD     >=  "20070401" )  AND
                           (WRK-COM-SRYKBN(IDX-Z)  =   "96")
                       PERFORM 10021-ZEIKEISEN-SEC
                   ELSE
                       MOVE    WRK-KISOTEN     TO  WRK-TANKA
                   END-IF
                   MOVE    WRK-TANKA       TO  WRK-KINGAK
           END-EVALUATE
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC              SECTION.
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
