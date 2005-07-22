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
       PROGRAM-ID.             ORCHC30.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 薬情一覧印刷
      *  コンポーネント名  : 薬情一覧印刷（ＨＣ３０）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/11    MCC-森脇      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.01.01    NACL-森脇    02.09.04  編集数量のクリア
      *                                     剤区分の編集
      *  00.01.02    NACL-森脇    02.09.12  ファイルディレクトリ位置指定
      *  00.01.03    NACL-多々納  02.09.17  印刷サプの追加
      *  00.01.04    NACL-森脇    02.09.27  内容の変更
      *  00.01.05    NACL-森脇    02.10.01  マスタ読み込みの修正
      *  00.01.06    NACL-森脇    02.10.02  区分の作業領域のクリア
      *  00.01.07    NACL-森脇    02.11.07  記号の折り返し桁数変更
      *  01.01.01    NACL-森脇    02.12.12  MONPEに対応
      *  01.01.02    NACL-伊藤    03.03.03  画像パス名サイズ変更
      *  01.01.03    NACL-伊藤    03.03.28  印刷用データサイズ変更
      *  01.01.04    NACL-森脇    03/07/14  出力先プリンタサブ追加
      *  02.01.01    NACL-森脇    03/11/19  患者氏名２追加
      *  02.01.02    NACL-森脇    04/01/26  患者氏名敬称編集
      *  02.01.03    NACL-多々納  04.03.04  用法複数編集他
      *  02.01.04    NACL-森脇    05/07/22  記号半角修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE            ASSIGN  HC01PARA
                                       FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                            SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA             PIC X(20000).
      *
       WORKING-STORAGE                 SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    シェル用領域
      **** COPY    "CPCOMMONSHELL.INC".
      *    COPY    "CPSHELLTBL.INC".
      *****COPY    "ORCA-DBPATH".
      *
      *----(01.01.01)--UPD-START---
      *    帳票領域
           COPY    "HCM301.INC".
           COPY    "HCM302.INC".
           COPY    "HCM303.INC".
           COPY    "HCM304.INC".
           COPY    "HCM305.INC".
           COPY    "HCM306.INC".
      *----(01.01.01)--UPD-END-----
      *
      *----(00.01.02)--ADD-START----
       01  FILE-NAME           PIC X(512).
      *
      *    空白の画像
       01  KUHAKU-NAME.
           03  FILLER          PIC X(05)   VALUE   "data/".
           03  FILLER          PIC X(15)   VALUE   "transparent.jpg".
      *----(00.01.02)--ADD-END------
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                PIC 9(08).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-YAKUJYO             PIC 9(01).
           03  FLG-SP                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(03).
           03  CNT-LINE                PIC 9(01).
           03  CNT-PARA                PIC 9(04).
           03  CNT-KAISU               PIC 9(01).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(03).
           03  IDZ                     PIC 9(03).
           03  STR-KET                 PIC 9(03).
           03  END-KET                 PIC 9(03).
           03  NUM-KET                 PIC 9(03).
      *
           03  IDX-STR                 PIC 9(04).
           03  IDX-STR1                PIC 9(04).
           03  IDX-KET                 PIC 9(04).
           03  IDX-MAX                 PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDX2-MAX                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-PAGE                    PIC 9(03).
      *
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-HENSYURUI           PIC X(12).
           03  WRK-HENSURYO            PIC X(12).
           03  WRK-HENYKZNAME          PIC X(85).
           03  WRK-HENKONOKOKA         PIC X(200).
           03  WRK-HENKEIJYO           PIC X(180).
           03  WRK-HENNAIYO            PIC X(128).
           03  WRK-HENCYUJKO           PIC X(425).
           03  WRK-SRYCD-NAME          PIC X(100).
      * 
           03  WRK-PRTNM               PIC X(20).
           03  WRK-SRYCD               PIC X(09).
      *----(00.01.04)--UPD-START----
      *     03  WRK-TANICD              PIC X(03).
           03  WRK-SSTKIJUNCD          PIC X(04).
      *----(00.01.04)--UPD-END------
      *
           03  WRK-SURYO               PIC 9(08)V9(03).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1        PIC 9(08).
               05  WRK-SURYO-S2        PIC 9(03).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1        PIC ZZZZZZZZ.
               05  WRK-SURYO-Z1-9      REDEFINES   WRK-SURYO-Z1
                                       PIC ZZZZZZZ9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZ.
      *
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-Z9                  PIC Z9.
      *
      *    全角を行毎に編集
       01  WRK-HENSYU-AREA.
           03  WRK-HENSYU-OCC          OCCURS   10.
               05  WRK-HENSYU          PIC X(40).
           03  WRK-NAIYOU              PIC X(600).
           03  GYO-MAX                 PIC 9(04).
           03  KOU-MAX                 PIC 9(04).
      *
       01  WRK-YKJ-AREA.
           03  WRK-YKJ-SYORIBI       PIC  X(22).
           03  WRK-YKJ-PAGE          PIC  X(3).
           03  WRK-YKJ-SYURUI        PIC  X(12).
           03  WRK-YKJ-PTNUM         PIC  X(20).
           03  WRK-YKJ-KANANAME      PIC  X(44).
      *----(02.01.02)--UPD-START---
      *----(02.01.01)--UPD-START---
           03  WRK-YKJ-NAME          PIC  X(88).
      *----(02.01.01)--UPD-END-----
      *----(02.01.02)--UPD-END-----
           03  WRK-YKJ-JYUSYO        PIC  X(56).
           03  WRK-YKJ-HSPNAME       PIC  X(56).
           03  WRK-YKJ-TEL           PIC  X(15).
           03  WRK-YKJ-DRNAME        PIC  X(26).
           03  WRK-YKJ-COMMENT       PIC  X(108)  OCCURS  7.
           03  WRK-YKJ-TBL                        OCCURS  6.
             05  WRK-YKJ-NUM         PIC  X(2).
             05  WRK-YKJ-YKZNAME     PIC  X(26)   OCCURS  3.
             05  WRK-YKJ-KONOKOKA    PIC  X(30)   OCCURS  6.
             05  WRK-YKJ-GAZO        PIC  X(1024).
             05  WRK-YKJ-KEIJYO      PIC  X(16)   OCCURS  10.
             05  WRK-YKJ-KBN         PIC  X(2)    OCCURS  4.
             05  WRK-YKJ-NAIYO       PIC  X(16)   OCCURS  8.
             05  WRK-YKJ-CYUJKO      PIC  X(40)   OCCURS  10.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報−基本情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    薬情マスタ
           COPY    "CPYAKUJYO.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *----(00.01.02)--ADD-START----
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *----(00.01.02)--ADD-END------
      *----(01.01.04)--ADD-START---
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *----(01.01.04)--ADD-END-----
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPORCHC30.INC".
      *
      ******************************************************************
       PROCEDURE                   DIVISION
                                       USING
                                       SPA-AREA
                                       ORCHC30AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    出力先プリンタ名割り当て情報編集
           PERFORM 900-1031-READ-SEC
      *
      *    医療機関情報−基本情報セット
           PERFORM 900-1001-READ-SEC
      *
      *    医療機関情報−所在地、連絡先セット
           PERFORM 900-1002-READ-SEC
      *
      *    職員情報
           PERFORM 900-1010-READ-SEC
      *
      *----(00.01.02)--ADD-START----
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    KUHAKU-NAME         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *----(00.01.02)--ADD-END------
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           INITIALIZE                  WRK-YKJ-AREA
      *
           MOVE    ZERO               TO  CNT-PARA
      *
           PERFORM
                   UNTIL       CNT-PARA    =   ORCHC30-MAX
      *    帳票明細処理
               PERFORM 210-PRT-LINE-SEC
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票明細処理
      *****************************************************************
       210-PRT-LINE-SEC                SECTION.
      *
           INITIALIZE                  WRK-YKJ-AREA
           MOVE    ZERO                TO  CNT-LINE
      *
           PERFORM UNTIL   (CNT-PARA   =   ORCHC30-MAX)
                   OR      (CNT-LINE   =   6)
      *
               COMPUTE CNT-PARA    =   CNT-PARA    +   1
               COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
      *        番号セット
               MOVE    CNT-PARA            TO  WRK-Z9
               MOVE    WRK-Z9              TO  WRK-YKJ-NUM(CNT-LINE)
      *
      *        薬情マスタ検索
               PERFORM 900-YAKUJYO-READ-SEC
      *
               IF      FLG-YAKUJYO         =   ZERO
      *
      *            効能効果セット
                   MOVE    YAKUJYO-EFFECT      TO  WRK-HENKONOKOKA
                   PERFORM 221-HENEFFECT-SET-SEC
      *
      *            注意事項
                   MOVE    YAKUJYO-CAUTION     TO  WRK-HENCYUJKO
                   PERFORM 222-HENCAUTION-SET-SEC
      *            画像編集
                   IF      YAKUJYO-PHOTO       NOT =   SPACE
                       MOVE  YAKUJYO-PHOTO
                                           TO  WRK-YKJ-GAZO(CNT-LINE)
                   ELSE
      *----(00.01.02)--UPD-START----
      *                空白の画像
                       MOVE    FILE-NAME   TO  WRK-YKJ-GAZO(CNT-LINE)
      *----(00.01.02)--UPD-END------
      *                形状編集
                       PERFORM 230-KEIJYO-SET-SEC
                   END-IF
               ELSE
      *----(00.01.02)--UPD-START----
      *            空白の画像
                   MOVE    FILE-NAME   TO  WRK-YKJ-GAZO(CNT-LINE)
      *----(00.01.02)--UPD-END------
               END-IF
      *
      *----(00.01.04)--UPD-START----
      *        内容編集、区分編集
               PERFORM 240-NAIYO-SET-SEC
      *
      *        薬剤名セット
               IF  YAKUJYO-NAME    =   SPACE
      ************ MOVE    TNS-NAME        TO  WRK-HENYKZNAME
                   MOVE    WRK-SRYCD-NAME  TO  WRK-HENYKZNAME
               ELSE
                   MOVE    YAKUJYO-NAME    TO  WRK-HENYKZNAME
               END-IF
               PERFORM 223-HENYKZNAME-SET-SEC
      *----(00.01.04)--UPD-END------
      *
           END-PERFORM
      *
      *    帳票頁処理
           PERFORM 260-PRT-PAGE-SEC
      *
           .
       210-PRT-LINE-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票効能効果データ編集処理
      *****************************************************************
       221-HENEFFECT-SET-SEC           SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    WRK-HENKONOKOKA     TO  WRK-NAIYOU
      *    行
           MOVE    6                   TO  GYO-MAX
      *    文字数
           MOVE    30                  TO  KOU-MAX
           PERFORM 290-HENSYU-SYORI-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX)     TO  WRK-YKJ-KONOKOKA
                                                         (CNT-LINE IDX)
           END-PERFORM
      *
           .
       221-HENEFFECT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票全角編集処理
      *****************************************************************
       290-HENSYU-SYORI-SEC           SECTION.
      *
           MOVE    1                   TO  IDX
           MOVE    1                   TO  IDX-STR
           MOVE    1                   TO  IDX-KET
      *
           PERFORM UNTIL       (IDX-STR       >   600 )  OR
                               (IDX           >   GYO-MAX )  OR
                               (WRK-NAIYOU(IDX-STR:)  =   SPACE)
      *        改行は対象外
               IF      (WRK-NAIYOU(IDX-STR:1)  =   X"0A")
                   IF      IDX-STR             >   1
                        COMPUTE IDX-STR1       =   IDX-STR   -  1
                   END-IF
      *            行の最後が改行の時
                   IF     (IDX-KET             =   1   )  AND
                          (IDX-STR             >   1   )  AND
                          (WRK-NAIYOU(IDX-STR1:1)
                                               >=  X"A1" )
                       ADD     1                   TO  IDX-STR
                   ELSE
                       MOVE    KOU-MAX             TO  IDX-KET
                       ADD     1                   TO  IDX-STR
                   END-IF
               ELSE
                   IF      WRK-NAIYOU(IDX-STR:1)
                                               >=  X"A1"
                       MOVE    WRK-NAIYOU(IDX-STR:2)
                                               TO  WRK-HENSYU (IDX)
                                                         (IDX-KET:2)
                       ADD     2                   TO  IDX-STR
                       ADD     2                   TO  IDX-KET
                   ELSE
                       ADD     1                   TO  IDX-STR
                   END-IF
               END-IF
      *
      *        改行、最大文字で改行
               IF       (IDX-KET       >=  KOU-MAX  )
                   ADD     1                   TO  IDX
                   MOVE    1                   TO  IDX-KET
               END-IF
      *
           END-PERFORM
      *
           .
       290-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票注意事項データ編集処理
      *****************************************************************
       222-HENCAUTION-SET-SEC           SECTION.
      *
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    WRK-HENCYUJKO       TO  WRK-NAIYOU
      *    行
           MOVE    10                  TO  GYO-MAX
      *    文字数
           MOVE    40                  TO  KOU-MAX
           PERFORM 290-HENSYU-SYORI-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX)     TO  WRK-YKJ-CYUJKO
                                                         (CNT-LINE IDX)
           END-PERFORM
      *
      *
           .
       222-HENCAUTION-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票薬剤名データ編集処理
      *****************************************************************
       223-HENYKZNAME-SET-SEC           SECTION.
      *
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    WRK-HENYKZNAME      TO  WRK-NAIYOU
      *    行
           MOVE    3                   TO  GYO-MAX
      *    文字数
           MOVE    26                  TO  KOU-MAX
           PERFORM 290-HENSYU-SYORI-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX)     TO  WRK-YKJ-YKZNAME
                                                         (CNT-LINE IDX)
           END-PERFORM
      *
           .
       223-HENYKZNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票形状データセット処理
      *****************************************************************
       230-KEIJYO-SET-SEC              SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    1                   TO  IDX
      *    行
           MOVE    10                  TO  GYO-MAX
      *    文字数
           MOVE    16                  TO  KOU-MAX
      *
      *    色編集
           MOVE    YAKUJYO-COLOR       TO  WRK-HENKEIJYO
           PERFORM 231-HENKEIJYO-SET-SEC
      *
      *    形編集
           MOVE    YAKUJYO-SHAPE       TO  WRK-HENKEIJYO
           PERFORM 231-HENKEIJYO-SET-SEC
      *
      *    記号編集
           IF          YAKUJYO-MARK    =   SPACE
               GO  TO  230-KEIJYO-SET-EXT
           END-IF
      *
      *    記号チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    YAKUJYO-MARK        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
      *
           IF          KANACHK-SYUBETU     =   1
      *        文字数
               MOVE    14                  TO  KOU-MAX
           END-IF
      *
           MOVE    YAKUJYO-MARK        TO  WRK-HENKEIJYO
           PERFORM 231-HENKEIJYO-SET-SEC
      *
           .
       230-KEIJYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票形状データ編集処理
      *****************************************************************
       231-HENKEIJYO-SET-SEC           SECTION.
      *
           MOVE    1                   TO  IDX-STR
           MOVE    1                   TO  IDX-KET
      *
           PERFORM UNTIL       (IDX-STR       >   180 )  OR
                               (IDX           >   GYO-MAX )  OR
                               (WRK-HENKEIJYO(IDX-STR:)  =   SPACE)
      *        改行は対象外
               IF      (WRK-HENKEIJYO(IDX-STR:1)  =   X"0A")
                   MOVE    KOU-MAX             TO  IDX-KET
                   ADD     1                   TO  IDX-STR
               ELSE
                   IF      WRK-HENKEIJYO(IDX-STR:1)
                                               >=  X"A1"
                       MOVE    WRK-HENKEIJYO(IDX-STR:2)
                                               TO  WRK-YKJ-KEIJYO
                                                       (CNT-LINE IDX)
                                                       (IDX-KET:2)
                       ADD     2                   TO  IDX-STR
                       ADD     2                   TO  IDX-KET
      *----(02.01.04)--UPD-START---
                   ELSE
                       MOVE    WRK-HENKEIJYO(IDX-STR:1)
                                               TO  WRK-YKJ-KEIJYO
                                                       (CNT-LINE IDX)
                                                       (IDX-KET:1)
                       ADD     1                   TO  IDX-STR
                       ADD     1                   TO  IDX-KET
                   END-IF
      *----(02.01.04)--UPD-END-----
               END-IF
      *
      *        改行、最大文字で改行
               IF       (IDX-KET       >=  KOU-MAX  )
                   OR  ((IDX-STR       <=  180  )
                   AND (WRK-HENKEIJYO(IDX-STR:)  =   SPACE))
                   ADD     1                   TO  IDX
                   MOVE    1                   TO  IDX-KET
                   IF          WRK-HENKEIJYO(IDX-STR:1)    =   X"0A"
                       COMPUTE IDX-STR     =   IDX-STR     +   1
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       231-HENKEIJYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票形状データ編集処理
      *****************************************************************
       231-HENKEIJYO-SET1-SEC           SECTION.
      *
           MOVE    1                   TO  STR-KET
           MOVE    1                   TO  END-KET
           MOVE    ZERO                TO  NUM-KET
      *
           PERFORM UNTIL   (END-KET    >   600 )
                   OR      (IDX    >   GYO-MAX)
                   OR      (WRK-HENKEIJYO(END-KET:)    =   SPACE)
      *
               PERFORM UNTIL   (END-KET    >   180 )
                       OR      (NUM-KET    =   KOU-MAX)
                       OR      (WRK-HENKEIJYO(END-KET:)    =   SPACE)
                       OR      (WRK-HENKEIJYO(END-KET:1)   =   X"0A")
                   IF      WRK-NAIYOU(END-KET:1)    >=  X"A1"
                       COMPUTE END-KET     =   END-KET     +   1
                       COMPUTE NUM-KET     =   NUM-KET     +   1
                   END-IF
               END-PERFORM
      *
               MOVE    WRK-NAIYOU(STR-KET:NUM-KET)
                                       TO  WRK-YKJ-KEIJYO(CNT-LINE IDX)
      *
               IF          (END-KET    <=   600 )
                   AND     (WRK-NAIYOU(END-KET:1)   =   X"0A")
                   COMPUTE END-KET     =   END-KET    +   1
               END-IF
      *
               MOVE    ZERO                TO  NUM-KET
               MOVE    END-KET             TO  STR-KET
      *
               COMPUTE IDX     =   IDX     +   1
      *
           END-PERFORM
      *
           .
       231-HENKEIJYO-SET1-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票内容データセット処理
      *****************************************************************
       240-NAIYO-SET-SEC               SECTION.
      *
      *----(00.01.04)--UPD-START----
      *    点数マスタ用法検索
           MOVE    ORCHC30-YSRYCD(CNT-PARA)    TO  WRK-SRYCD
           PERFORM 900-TENSU-READ-SEC
      *
      *    用法編集
           MOVE    1                   TO  IDX
           MOVE    SPACE               TO  WRK-HENNAIYO
           MOVE    TNS-NAME            TO  WRK-HENNAIYO
           PERFORM 241-HENNAIYO-SET-SEC
      *
      *----(00.01.01)--UPD-START----
      *    日数編集
           IF      ORCHC30-ZAIKBN(CNT-PARA)    NOT =   3
               PERFORM 242-HENNAIYO-NISSU-SET-SEC
               PERFORM 241-HENNAIYO-SET-SEC
           END-IF
      *----(00.01.01)--UPD-END------
      *
      *    回数カウント
      *----(00.01.06)--UPD-START----
           MOVE    SPACE               TO  WRK-SSTKIJUNCD
      *----(00.01.06)--UPD-END------
           MOVE    ZERO                TO  CNT-KAISU
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL       IDZ     >   4
               IF  TNS-SSTKIJUNCD(IDZ) =   1
                   COMPUTE CNT-KAISU   =   CNT-KAISU   +   1
                   MOVE    "1"         TO  WRK-SSTKIJUNCD(IDZ:1)
               END-IF
           END-PERFORM
      *
      *----(00.01.05)--UPD-START----
      *    点数マスタ薬剤検索
           MOVE    ORCHC30-SRYCD(CNT-PARA)     TO  WRK-SRYCD
           PERFORM 900-TENSU-READ-SEC
           MOVE    TNS-NAME            TO  WRK-SRYCD-NAME
      *
           IF      CNT-KAISU           =   ZERO
      *        その他の用法
               IF      ORCHC30-YOHOU-G(CNT-PARA)   NOT =   SPACE
                   PERFORM 249-YOUHOU-HEN-SEC
               END-IF
               GO  TO  240-NAIYO-SET-EXT
           END-IF
      *
      *    回数編集
           PERFORM 242-HENNAIYO-KAISU-SET-SEC
           PERFORM 241-HENNAIYO-SET-SEC
      *
      *    点数マスタ薬剤検索
      *     MOVE    ORCHC30-SRYCD(CNT-PARA)     TO  WRK-SRYCD
      *     PERFORM 900-TENSU-READ-SEC
      *----(00.01.05)--UPD-END------
      *
           MOVE    IDX                 TO  IDX-MAX
      *    数量編集
           IF          TNS-TANICD  =   "015"
               OR      TNS-TANICD  =   "016"
               OR      TNS-TANICD  =   "018"
               OR      TNS-TANICD  =   "059"
               PERFORM 242-HENNAIYO-SURYO-SET-SEC
               PERFORM 241-HENNAIYO-SET-SEC
           END-IF
      *----(00.01.04)--UPD-END------
      *
      *    その他の用法
           IF      ORCHC30-YOHOU-G(CNT-PARA)   NOT =   SPACE
               MOVE    IDX-MAX         TO  IDX
               PERFORM 249-YOUHOU-HEN-SEC
           END-IF
      *
      *
      *    区分編集
           PERFORM 243-HENKBN-SET-SEC
      *
           .
       240-NAIYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票内容データ編集処理
      *****************************************************************
       241-HENNAIYO-SET-SEC            SECTION.
      *
           MOVE    1                   TO  STR-KET
           MOVE    1                   TO  END-KET
           MOVE    ZERO                TO  NUM-KET
      *
           PERFORM UNTIL   (END-KET    >   128  )
                   OR      (WRK-HENNAIYO(END-KET:)  =   SPACE)
                   OR      (IDX     >   8)
      *
               PERFORM UNTIL   (END-KET    >   128  )
                       OR      (NUM-KET    =   16)
                       OR      (WRK-HENNAIYO(END-KET:)  =   SPACE)
                   COMPUTE END-KET     =   END-KET     +   1
                   COMPUTE NUM-KET     =   NUM-KET     +   1
               END-PERFORM
      *
               MOVE    WRK-HENNAIYO(STR-KET:NUM-KET)
                                       TO  WRK-YKJ-NAIYO(CNT-LINE IDX)
               MOVE    ZERO                TO  NUM-KET
               MOVE    END-KET             TO  STR-KET
               COMPUTE IDX     =   IDX     +   1
           END-PERFORM
      *
           .
       241-HENNAIYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票内容データ日数編集処理
      *****************************************************************
       242-HENNAIYO-NISSU-SET-SEC      SECTION.
      *
           MOVE    ORCHC30-KAISU(CNT-PARA)     TO  WRK-ZZ9
      *
           MOVE    SPACE               TO  WRK-HENSURYO
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >   3
               IF  WRK-ZZ9(IDZ:1)  NOT =   SPACE
                   COMPUTE IDY     =   IDY     +   1
                   MOVE  WRK-ZZ9(IDZ:1)        TO  WRK-HENSURYO(IDY:1)
               END-IF
           END-PERFORM
      *
      *    全角変換
           MOVE    SPACE               TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-HENSURYO        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
      *
      *    数量文字セット
           MOVE    SPACE               TO  WRK-HENNAIYO
           MOVE    ZERO                TO  IDZ
           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-HENNAIYO(1:KANACHK-MAX)
           COMPUTE IDZ     =   KANACHK-MAX     +   1
      *----(00.01.01)--UPD-START----
           EVALUATE    ORCHC30-ZAIKBN(CNT-PARA)
               WHEN    1
                   MOVE    "日分"              TO  WRK-HENNAIYO(IDZ:4)
               WHEN    2
                   MOVE    "回分"              TO  WRK-HENNAIYO(IDZ:4)
           END-EVALUATE
      *----(00.01.01)--UPD-END------
      *
           .
       242-HENNAIYO-NISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票内容データ回数編集処理
      *****************************************************************
       242-HENNAIYO-KAISU-SET-SEC      SECTION.
      *
      *    全角変換
           MOVE    SPACE               TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    CNT-KAISU           TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
      *
      *    数量文字セット
           MOVE    SPACE               TO  WRK-HENNAIYO
           MOVE    ZERO                TO  IDZ
           MOVE    "１日"              TO  WRK-HENNAIYO(1:4)
           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-HENNAIYO(5:KANACHK-MAX)
           COMPUTE IDZ     =   KANACHK-MAX     +   5
           MOVE    "回上記量"          TO  WRK-HENNAIYO(IDZ:)
      *
           .
       242-HENNAIYO-KAISU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票内容データ数量編集処理
      *****************************************************************
       242-HENNAIYO-SURYO-SET-SEC      SECTION.
      *
           COMPUTE WRK-SURYO   =   ORCHC30-SURYO(CNT-PARA)
                               *   ORCHC30-KAISU(CNT-PARA)
      *
      *    数量Ｚ編集
           PERFORM 250-ZHEN-SURYO-SET-SEC
      *
           IF  WRK-SURYO-X(11:)    =   SPACE
      *        全角変換
               MOVE    SPACE               TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-HENSURYO        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
      *
      *        数量文字セット
               MOVE    SPACE               TO  WRK-HENNAIYO
               MOVE    ZERO                TO  IDZ
               MOVE    "全"                TO  WRK-HENNAIYO(1:2)
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-HENNAIYO(3:KANACHK-MAX)
               COMPUTE IDZ     =   KANACHK-MAX     +   3
               MOVE    TNS-TANINAME        TO  WRK-HENNAIYO(IDZ:)
           ELSE
               MOVE    "　　　　　　　　"  TO  WRK-HENNAIYO
           END-IF
      *
      *----(00.01.04)--UPD-START----
           MOVE    8                   TO  IDX
      *----(00.01.04)--UPD-END------
      *
           .
       242-HENNAIYO-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票区分データ編集処理
      *****************************************************************
       243-HENKBN-SET-SEC              SECTION.
      *
      *----(00.01.01)--ADD-START----
           MOVE    SPACE               TO  WRK-HENSURYO
      *----(00.01.01)--ADD-END------
      *    区分セット
           EVALUATE    TNS-TANICD
               WHEN    "032"
               WHEN    "033"
                   MOVE    "1"             TO  WRK-HENSURYO
               WHEN    OTHER
                   COMPUTE WRK-SURYO   =   ORCHC30-SURYO(CNT-PARA)
                                       /   CNT-KAISU
      *            数量Ｚ編集
                   PERFORM 250-ZHEN-SURYO-SET-SEC
      *
      *            区分加工
                   IF  WRK-SURYO-Z3    NOT =   SPACE
                       IF  WRK-HENSURYO        =   "0.5"
                           MOVE    "半"        TO  WRK-HENSURYO
                       ELSE
                           MOVE    SPACE       TO  WRK-HENSURYO
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *----(00.01.04)--UPD-START----
      *    区分セット
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ     >   4
               IF          WRK-SSTKIJUNCD(IDZ:1)   =   "1"
                   MOVE    WRK-HENSURYO    TO  WRK-YKJ-KBN(CNT-LINE IDZ)
               END-IF
           END-PERFORM
      *----(00.01.04)--UPD-END------
           .
      *
       243-HENKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   その他の用法処理
      *****************************************************************
       249-YOUHOU-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL  (IDZ     >   5   )  OR
                          (IDX     >=  8   )  OR
                          (ORCHC30-YOHOU (CNT-PARA IDZ)  =   SPACE )
      *        点数マスタ検索
               MOVE    ORCHC30-YOHOU (CNT-PARA IDZ)
                                           TO  WRK-SRYCD
               PERFORM 900-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-NAME            TO  WRK-HENNAIYO
                   PERFORM 241-HENNAIYO-SET-SEC
               END-IF
           END-PERFORM
           .
      *
       249-YOUHOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       250-ZHEN-SURYO-SET-SEC      SECTION.
      *
      *    数量Ｚ編集
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
      *    数量小数点以下編集
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDY     FROM    3   BY  -1
                   UNTIL       IDY     <   1
               IF  WRK-SURYO-S2(IDY:1) NOT =   ZERO
                       MOVE    1           TO  FLG-SP
               END-IF
               IF  FLG-SP  =   1
                   MOVE    WRK-SURYO-S2(IDY:1) TO  WRK-SURYO-Z3(IDY:1)
               END-IF
           END-PERFORM
      *
           IF  WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."             TO  WRK-SURYO-Z2
               IF  WRK-SURYO-X(1:8)    =   SPACE
                   MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-HENSURYO
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >   12
               IF  WRK-SURYO-X(IDZ:1)  NOT =   SPACE
                   COMPUTE IDY     =   IDY     +   1
                   MOVE  WRK-SURYO-X(IDZ:1)    TO  WRK-HENSURYO(IDY:1)
               END-IF
           END-PERFORM
      *
           .
       250-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       260-PRT-PAGE-SEC                SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
      *    ページセット
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  WRK-YKJ-PAGE
      *
      *    日付セット
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-YKJ-SYORIBI
      *
      *    種類セット
           MOVE  "（"                  TO  WRK-HENSYURUI(1:2)
           IF  ORCHC30-MAX(3:1)    =   ZERO
               MOVE    ORCHC30-MAX(4:1)    TO  WRK-HENSYURUI(3:1)
               MOVE    "種類）"            TO  WRK-HENSYURUI(4:6)
           ELSE
               MOVE    ORCHC30-MAX(3:2)    TO  WRK-HENSYURUI(3:2)
               MOVE    "種類）"            TO  WRK-HENSYURUI(5:6)
           END-IF
           MOVE    WRK-HENSYURUI       TO  WRK-YKJ-SYURUI
      *
      *    患者番号
           MOVE    SPA-PTNUM           TO  WRK-YKJ-PTNUM
      *
      *    患者カナ氏名
           MOVE    SPA-KANANAME        TO  WRK-YKJ-KANANAME
      *
      *    患者氏名
      *----(02.01.02)--UPD-START---
      *     MOVE    SPA-NAME            TO  WRK-YKJ-NAME
           PERFORM 265-NAMEHEN-SET-SEC
      *----(02.01.02)--UPD-END-----
      *
      *    所在地
           MOVE    SYS-1002-ADRS       TO  WRK-YKJ-JYUSYO
      *
      *    医療機関名称
           MOVE    SYS-1001-HOSPNAME   TO  WRK-YKJ-HSPNAME
      *
      *    電話番号
           MOVE    SYS-1002-TEL        TO  WRK-YKJ-TEL
      *
      *    職員氏名
           MOVE    SYS-1010-NAME       TO  WRK-YKJ-DRNAME
      *
           PERFORM  270-PRT-WRITE-SEC
      *
           .
       260-PRT-PAGE-EXT.
           EXIT.
      *
      *----(02.01.02)--ADD-START---
      *****************************************************************
      *   患者氏名編集処理
      *****************************************************************
       265-NAMEHEN-SET-SEC           SECTION.
      *
           MOVE    1                   TO  STR-KET
           MOVE    1                   TO  END-KET
           MOVE    ZERO                TO  NUM-KET
      *
           PERFORM VARYING END-KET     FROM    1   BY  1
                   UNTIL  (END-KET     =  50       )
                      OR  (SPA-NAME(END-KET:)      =   SPACE)
               COMPUTE NUM-KET    =    NUM-KET     +   1
           END-PERFORM
      *
           IF      END-KET    =    50
               MOVE    END-KET     TO  NUM-KET
           END-IF
      *
      *
           EVALUATE    NUM-KET
           WHEN    0
               GO  TO  265-NAMEHEN-SET-EXT
           WHEN    1   THRU    30
               MOVE    SPA-NAME(1:NUM-KET) TO  WRK-YKJ-NAME(1:NUM-KET)
               MOVE    "　様"         TO  WRK-YKJ-NAME(END-KET:)
           WHEN    31  THRU    40
               COMPUTE END-KET =   END-KET +   44
               MOVE    SPA-NAME(1:NUM-KET) TO  WRK-YKJ-NAME(45:NUM-KET)
               MOVE    "　様"         TO  WRK-YKJ-NAME(END-KET:)
           WHEN    41  THRU    50
      *********COMPUTE END-KET =   END-KET +   1
      *        COMPUTE NUM-KET =   NUM-KET -   40
      *        MOVE    SPA-NAME(1:40)      TO  WRK-YKJ-NAME(1:40)
      *        MOVE    SPA-NAME(41:NUM-KET)
      *                                    TO  WRK-YKJ-NAME(45:NUM-KET)
      *        PERFORM UNTIL   END-KET     >=  85
      *            MOVE    "　"            TO  WRK-YKJ-NAME(END-KET:2)
      *            COMPUTE END-KET =   END-KET +   2
      ******** END-PERFORM
               MOVE    SPA-NAME            TO  WRK-YKJ-NAME
               INSPECT WRK-YKJ-NAME        REPLACING  ALL "  "  BY  "　"
               MOVE    "　様"              TO  WRK-YKJ-NAME(85:)
           END-EVALUATE
      *
      *
           .
       265-NAMEHEN-SET-EXT.
           EXIT.
      *
      *----(02.01.02)--ADD-END-----
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       270-PRT-WRITE-SEC               SECTION.
      *
      *    印刷用ファイル作成
           MOVE    "HC30"             TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
           OPEN    OUTPUT              PRT-FILE
      *
           MOVE    SPACE               TO  PRT-REC
      *
           EVALUATE    CNT-LINE
               WHEN    1
                   PERFORM 2701-HC301-HENSYU-SEC
               WHEN    2
                   PERFORM 2702-HC302-HENSYU-SEC
               WHEN    3
                   PERFORM 2703-HC303-HENSYU-SEC
               WHEN    4
                   PERFORM 2704-HC304-HENSYU-SEC
               WHEN    5
                   PERFORM 2705-HC305-HENSYU-SEC
               WHEN    6
                   PERFORM 2706-HC306-HENSYU-SEC
           END-EVALUATE
      *
      *　　印刷用データ出力
           WRITE   PRT-REC
      *    帳票印刷処理
           PERFORM 400-PRINT-SEC
      *
      *
           .
       270-PRT-WRITE-EXT.
           EXIT.
      *
      *----(01.01.01)--UPD-STTART---
      *****************************************************************
      *    帳票編集処理（ＨＣ３０１）
      *****************************************************************
       2701-HC301-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  HCM301
      *
           MOVE    WRK-YKJ-SYORIBI          TO  HCM301-SYORIBI
           MOVE    WRK-YKJ-PAGE             TO  HCM301-PAGE
           MOVE    WRK-YKJ-SYURUI           TO  HCM301-SYURUI
           MOVE    WRK-YKJ-PTNUM            TO  HCM301-ID
           MOVE    WRK-YKJ-KANANAME         TO  HCM301-KANANAME
      *----(02.01.01)--UPD-STTART---
      *----(02.01.02)--UPD-STTART---
           IF      WRK-YKJ-NAME(35:)    =   SPACE
               MOVE    WRK-YKJ-NAME(1:34)       TO  HCM301-NAME
           ELSE
               MOVE    WRK-YKJ-NAME             TO  HCM301-NAME2
           END-IF
      *----(02.01.02)--UPD-END------
      *----(02.01.01)--UPD-END------
           MOVE    WRK-YKJ-JYUSYO           TO  HCM301-JYUSYO
           MOVE    WRK-YKJ-HSPNAME          TO  HCM301-HSPNAME
           MOVE    WRK-YKJ-TEL              TO  HCM301-TEL
           MOVE    WRK-YKJ-DRNAME           TO  HCM301-DRNAME
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   7
              MOVE    WRK-YKJ-COMMENT(IDX)  TO  HCM301-COMMENT(IDX)
           END-PERFORM
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   CNT-LINE
      *
               MOVE    WRK-YKJ-NUM(IDX)     TO  HCM301-NUM
               MOVE    WRK-YKJ-GAZO(IDX)    TO  HCM301-GAZO
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     3
                   MOVE    WRK-YKJ-YKZNAME(IDX IDY)
                                            TO  HCM301-YKZNAME(IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     6
                   MOVE    WRK-YKJ-KONOKOKA(IDX IDY)
                                            TO  HCM301-KONOKOKA(IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-KEIJYO(IDX IDY)
                                            TO  HCM301-KEIJYO(IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     4
                   MOVE    WRK-YKJ-KBN(IDX IDY)
                                            TO  HCM301-KBN(IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     8
                   MOVE    WRK-YKJ-NAIYO(IDX IDY)
                                            TO  HCM301-NAIYO(IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-CYUJKO(IDX IDY)
                                            TO  HCM301-CYUJKO(IDY)
               END-PERFORM
           END-PERFORM
      *
           MOVE    HCM301               TO  PRT-PRTDATA
      *
           .
       2701-HC301-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理（ＨＣ３０２）
      *****************************************************************
       2702-HC302-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  HCM302
      *
           MOVE    WRK-YKJ-SYORIBI          TO  HCM302-SYORIBI
           MOVE    WRK-YKJ-PAGE             TO  HCM302-PAGE
           MOVE    WRK-YKJ-SYURUI           TO  HCM302-SYURUI
           MOVE    WRK-YKJ-PTNUM            TO  HCM302-ID
           MOVE    WRK-YKJ-KANANAME         TO  HCM302-KANANAME
      *----(02.01.01)--UPD-STTART---
      *----(02.01.02)--UPD-STTART---
           IF      WRK-YKJ-NAME(35:)    =   SPACE
               MOVE    WRK-YKJ-NAME(1:34)       TO  HCM302-NAME
           ELSE
               MOVE    WRK-YKJ-NAME             TO  HCM302-NAME2
           END-IF
      *----(02.01.02)--UPD-END------
      *----(02.01.01)--UPD-END------
           MOVE    WRK-YKJ-JYUSYO           TO  HCM302-JYUSYO
           MOVE    WRK-YKJ-HSPNAME          TO  HCM302-HSPNAME
           MOVE    WRK-YKJ-TEL              TO  HCM302-TEL
           MOVE    WRK-YKJ-DRNAME           TO  HCM302-DRNAME
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   7
              MOVE    WRK-YKJ-COMMENT(IDX)  TO  HCM302-COMMENT(IDX)
           END-PERFORM
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   CNT-LINE
      *
               MOVE    WRK-YKJ-NUM(IDX)     TO  HCM302-NUM(IDX)
               MOVE    WRK-YKJ-GAZO(IDX)    TO  HCM302-GAZO(IDX)
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     3
                   MOVE    WRK-YKJ-YKZNAME(IDX IDY)
                                            TO  HCM302-YKZNAME(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     6
                   MOVE    WRK-YKJ-KONOKOKA(IDX IDY)
                                            TO  HCM302-KONOKOKA(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-KEIJYO(IDX IDY)
                                            TO  HCM302-KEIJYO(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     4
                   MOVE    WRK-YKJ-KBN(IDX IDY)
                                            TO  HCM302-KBN(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     8
                   MOVE    WRK-YKJ-NAIYO(IDX IDY)
                                            TO  HCM302-NAIYO(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-CYUJKO(IDX IDY)
                                            TO  HCM302-CYUJKO(IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           MOVE    HCM302               TO  PRT-PRTDATA
      *
           .
       2702-HC302-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理（ＨＣ３０３）
      *****************************************************************
       2703-HC303-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  HCM303
      *
           MOVE    WRK-YKJ-SYORIBI          TO  HCM303-SYORIBI
           MOVE    WRK-YKJ-PAGE             TO  HCM303-PAGE
           MOVE    WRK-YKJ-SYURUI           TO  HCM303-SYURUI
           MOVE    WRK-YKJ-PTNUM            TO  HCM303-ID
           MOVE    WRK-YKJ-KANANAME         TO  HCM303-KANANAME
      *----(02.01.01)--UPD-STTART---
      *----(02.01.02)--UPD-STTART---
           IF      WRK-YKJ-NAME(35:)    =   SPACE
               MOVE    WRK-YKJ-NAME(1:34)       TO  HCM303-NAME
           ELSE
               MOVE    WRK-YKJ-NAME             TO  HCM303-NAME2
           END-IF
      *----(02.01.02)--UPD-END------
      *----(02.01.01)--UPD-END------
           MOVE    WRK-YKJ-JYUSYO           TO  HCM303-JYUSYO
           MOVE    WRK-YKJ-HSPNAME          TO  HCM303-HSPNAME
           MOVE    WRK-YKJ-TEL              TO  HCM303-TEL
           MOVE    WRK-YKJ-DRNAME           TO  HCM303-DRNAME
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   7
              MOVE    WRK-YKJ-COMMENT(IDX)      TO  HCM303-COMMENT(IDX)
           END-PERFORM
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   CNT-LINE
      *
               MOVE    WRK-YKJ-NUM(IDX)     TO  HCM303-NUM(IDX)
               MOVE    WRK-YKJ-GAZO(IDX)    TO  HCM303-GAZO(IDX)
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     3
                   MOVE    WRK-YKJ-YKZNAME(IDX IDY)
                                            TO  HCM303-YKZNAME(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     6
                   MOVE    WRK-YKJ-KONOKOKA(IDX IDY)
                                            TO  HCM303-KONOKOKA(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-KEIJYO(IDX IDY)
                                            TO  HCM303-KEIJYO(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     4
                   MOVE    WRK-YKJ-KBN(IDX IDY)
                                            TO  HCM303-KBN(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     8
                   MOVE    WRK-YKJ-NAIYO(IDX IDY)
                                            TO  HCM303-NAIYO(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-CYUJKO(IDX IDY)
                                            TO  HCM303-CYUJKO(IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           MOVE    HCM303               TO  PRT-PRTDATA
      *
           .
       2703-HC303-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理（ＨＣ３０４）
      *****************************************************************
       2704-HC304-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  HCM304
      *
           MOVE    WRK-YKJ-SYORIBI          TO  HCM304-SYORIBI
           MOVE    WRK-YKJ-PAGE             TO  HCM304-PAGE
           MOVE    WRK-YKJ-SYURUI           TO  HCM304-SYURUI
           MOVE    WRK-YKJ-PTNUM            TO  HCM304-ID
           MOVE    WRK-YKJ-KANANAME         TO  HCM304-KANANAME
      *----(02.01.01)--UPD-STTART---
      *----(02.01.02)--UPD-STTART---
           IF      WRK-YKJ-NAME(35:)    =   SPACE
               MOVE    WRK-YKJ-NAME(1:34)       TO  HCM304-NAME
           ELSE
               MOVE    WRK-YKJ-NAME             TO  HCM304-NAME2
           END-IF
      *----(02.01.02)--UPD-END------
      *----(02.01.01)--UPD-END------
           MOVE    WRK-YKJ-JYUSYO           TO  HCM304-JYUSYO
           MOVE    WRK-YKJ-HSPNAME          TO  HCM304-HSPNAME
           MOVE    WRK-YKJ-TEL              TO  HCM304-TEL
           MOVE    WRK-YKJ-DRNAME           TO  HCM304-DRNAME
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   7
              MOVE    WRK-YKJ-COMMENT(IDX)      TO  HCM304-COMMENT(IDX)
           END-PERFORM
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   CNT-LINE
      *
               MOVE    WRK-YKJ-NUM(IDX)     TO  HCM304-NUM(IDX)
               MOVE    WRK-YKJ-GAZO(IDX)    TO  HCM304-GAZO(IDX)
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     3
                   MOVE    WRK-YKJ-YKZNAME(IDX IDY)
                                            TO  HCM304-YKZNAME(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     6
                   MOVE    WRK-YKJ-KONOKOKA(IDX IDY)
                                            TO  HCM304-KONOKOKA(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-KEIJYO(IDX IDY)
                                            TO  HCM304-KEIJYO(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     4
                   MOVE    WRK-YKJ-KBN(IDX IDY)
                                            TO  HCM304-KBN(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     8
                   MOVE    WRK-YKJ-NAIYO(IDX IDY)
                                            TO  HCM304-NAIYO(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-CYUJKO(IDX IDY)
                                            TO  HCM304-CYUJKO(IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           MOVE    HCM304               TO  PRT-PRTDATA
      *
           .
       2704-HC304-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理（ＨＣ３０５）
      *****************************************************************
       2705-HC305-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  HCM305
      *
           MOVE    WRK-YKJ-SYORIBI          TO  HCM305-SYORIBI
           MOVE    WRK-YKJ-PAGE             TO  HCM305-PAGE
           MOVE    WRK-YKJ-SYURUI           TO  HCM305-SYURUI
           MOVE    WRK-YKJ-PTNUM            TO  HCM305-ID
           MOVE    WRK-YKJ-KANANAME         TO  HCM305-KANANAME
      *----(02.01.01)--UPD-STTART---
      *----(02.01.02)--UPD-STTART---
           IF      WRK-YKJ-NAME(35:)    =   SPACE
               MOVE    WRK-YKJ-NAME(1:34)       TO  HCM305-NAME
           ELSE
               MOVE    WRK-YKJ-NAME             TO  HCM305-NAME2
           END-IF
      *----(02.01.02)--UPD-END------
      *----(02.01.01)--UPD-END------
           MOVE    WRK-YKJ-JYUSYO           TO  HCM305-JYUSYO
           MOVE    WRK-YKJ-HSPNAME          TO  HCM305-HSPNAME
           MOVE    WRK-YKJ-TEL              TO  HCM305-TEL
           MOVE    WRK-YKJ-DRNAME           TO  HCM305-DRNAME
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   7
              MOVE    WRK-YKJ-COMMENT(IDX)      TO  HCM305-COMMENT(IDX)
           END-PERFORM
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   CNT-LINE
      *
               MOVE    WRK-YKJ-NUM(IDX)     TO  HCM305-NUM(IDX)
               MOVE    WRK-YKJ-GAZO(IDX)    TO  HCM305-GAZO(IDX)
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     3
                   MOVE    WRK-YKJ-YKZNAME(IDX IDY)
                                            TO  HCM305-YKZNAME(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     6
                   MOVE    WRK-YKJ-KONOKOKA(IDX IDY)
                                            TO  HCM305-KONOKOKA(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-KEIJYO(IDX IDY)
                                            TO  HCM305-KEIJYO(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     4
                   MOVE    WRK-YKJ-KBN(IDX IDY)
                                            TO  HCM305-KBN(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     8
                   MOVE    WRK-YKJ-NAIYO(IDX IDY)
                                            TO  HCM305-NAIYO(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-CYUJKO(IDX IDY)
                                            TO  HCM305-CYUJKO(IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           MOVE    HCM305               TO  PRT-PRTDATA
      *
           .
       2705-HC305-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理（ＨＣ３０６）
      *****************************************************************
       2706-HC306-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  HCM306
      *
           MOVE    WRK-YKJ-SYORIBI          TO  HCM306-SYORIBI
           MOVE    WRK-YKJ-PAGE             TO  HCM306-PAGE
           MOVE    WRK-YKJ-SYURUI           TO  HCM306-SYURUI
           MOVE    WRK-YKJ-PTNUM            TO  HCM306-ID
           MOVE    WRK-YKJ-KANANAME         TO  HCM306-KANANAME
      *----(02.01.01)--UPD-STTART---
      *----(02.01.02)--UPD-STTART---
           IF      WRK-YKJ-NAME(35:)    =   SPACE
               MOVE    WRK-YKJ-NAME(1:34)       TO  HCM306-NAME
           ELSE
               MOVE    WRK-YKJ-NAME             TO  HCM306-NAME2
           END-IF
      *----(02.01.02)--UPD-END------
      *----(02.01.01)--UPD-END------
           MOVE    WRK-YKJ-JYUSYO           TO  HCM306-JYUSYO
           MOVE    WRK-YKJ-HSPNAME          TO  HCM306-HSPNAME
           MOVE    WRK-YKJ-TEL              TO  HCM306-TEL
           MOVE    WRK-YKJ-DRNAME           TO  HCM306-DRNAME
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   7
              MOVE    WRK-YKJ-COMMENT(IDX)      TO  HCM306-COMMENT(IDX)
           END-PERFORM
      *
           PERFORM VARYING    IDX       FROM    1   BY 1
                   UNTIL      IDX       >   CNT-LINE
      *
               MOVE    WRK-YKJ-NUM(IDX)     TO  HCM306-NUM(IDX)
               MOVE    WRK-YKJ-GAZO(IDX)    TO  HCM306-GAZO(IDX)
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     3
                   MOVE    WRK-YKJ-YKZNAME(IDX IDY)
                                            TO  HCM306-YKZNAME(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     6
                   MOVE    WRK-YKJ-KONOKOKA(IDX IDY)
                                            TO  HCM306-KONOKOKA(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-KEIJYO(IDX IDY)
                                            TO  HCM306-KEIJYO(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     4
                   MOVE    WRK-YKJ-KBN(IDX IDY)
                                            TO  HCM306-KBN(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     8
                   MOVE    WRK-YKJ-NAIYO(IDX IDY)
                                            TO  HCM306-NAIYO(IDX IDY)
               END-PERFORM
      *
               PERFORM VARYING    IDY       FROM  1  BY 1
                       UNTIL      IDY       >     10
                   MOVE    WRK-YKJ-CYUJKO(IDX IDY)
                                            TO  HCM306-CYUJKO(IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           MOVE    HCM306               TO  PRT-PRTDATA
      *
           .
       2706-HC306-HENSYU-EXT.
           EXIT.
      *
      *----(01.01.01)--UPD-END-----
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                SECTION.
      *
      *****MOVE   "HC01.sh"          TO  SHELL-ID
      *
      *    EVALUATE  CNT-LINE
      *        WHEN  1
      *            MOVE   "HC301.dia"          TO  SHELL-DIA
      *            MOVE   "HC301.def"          TO  SHELL-DEF
      *        WHEN  2
      *            MOVE   "HC302.dia"          TO  SHELL-DIA
      *            MOVE   "HC302.def"          TO  SHELL-DEF
      *        WHEN  3
      *            MOVE   "HC303.dia"          TO  SHELL-DIA
      *            MOVE   "HC303.def"          TO  SHELL-DEF
      *        WHEN  4
      *            MOVE   "HC304.dia"          TO  SHELL-DIA
      *            MOVE   "HC304.def"          TO  SHELL-DEF
      *        WHEN  5
      *            MOVE   "HC305.dia"          TO  SHELL-DIA
      *            MOVE   "HC305.def"          TO  SHELL-DEF
      *        WHEN  6
      *            MOVE   "HC306.dia"          TO  SHELL-DIA
      *            MOVE   "HC306.def"          TO  SHELL-DEF
      *    END-EVALUATE
      *    MOVE   HC01PARA            TO  SHELL-DAT
      **** MOVE   SYS-1031-PRTNM(8)   TO  SHELL-PRTNM 
           CLOSE                       PRT-FILE
      *
      **** 印刷用スクリプト処理
      *    MOVE    SHELL-NAME          TO  SHELLTBL-NAME
      *    MOVE    SHELL-ARG1          TO  SHELLTBL-ARG1
      *    MOVE    SHELL-ARG2          TO  SHELLTBL-ARG2
      *    MOVE    SHELL-ARG3          TO  SHELLTBL-ARG3
      *    MOVE    SHELL-ARG4          TO  SHELLTBL-ARG4
      *    MOVE    SHELL-ARG5          TO  SHELLTBL-ARG5
      *    MOVE    SHELLTBL            TO  MCPDATA-REC
      *    MOVE    "SHELL"             TO  MCP-FUNC
      *    MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
      *    CALL    "MCPSUB"            USING
      *            MCPAREA
      *****        MCPDATA-REC
      *
      *----(01.01.01)--UPD-START---
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"           TO  MKPRT-ID
           EVALUATE  CNT-LINE
               WHEN  1
                   MOVE   "HCM301.red"         TO  MKPRT-DIA
      *             MOVE   "HC301.dia"          TO  MKPRT-DIA
      *             MOVE   "HC301.def"          TO  MKPRT-DEF
               WHEN  2
                   MOVE   "HCM302.red"         TO  MKPRT-DIA
      *             MOVE   "HC302.dia"          TO  MKPRT-DIA
      *             MOVE   "HC302.def"          TO  MKPRT-DEF
               WHEN  3
                   MOVE   "HCM303.red"         TO  MKPRT-DIA
      *             MOVE   "HC303.dia"          TO  MKPRT-DIA
      *             MOVE   "HC303.def"          TO  MKPRT-DEF
               WHEN  4
                   MOVE   "HCM304.red"         TO  MKPRT-DIA
      *             MOVE   "HC304.dia"          TO  MKPRT-DIA
      *             MOVE   "HC304.def"          TO  MKPRT-DEF
               WHEN  5
                   MOVE   "HCM305.red"         TO  MKPRT-DIA
      *             MOVE   "HC305.dia"          TO  MKPRT-DIA
      *             MOVE   "HC305.def"          TO  MKPRT-DEF
               WHEN  6
                   MOVE   "HCM306.red"         TO  MKPRT-DIA
      *             MOVE   "HC306.dia"          TO  MKPRT-DIA
      *             MOVE   "HC306.def"          TO  MKPRT-DEF
           END-EVALUATE
           MOVE    SPACE               TO  MKPRT-DEF
           MOVE    HC01PARA            TO  MKPRT-DAT
           MOVE    SYS-1031-PRTNM(8)   TO  MKPRT-PRTNM
           CALL    "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
      *
           .
       400-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC             SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬情マスタ読み込み
      *****************************************************************
       900-YAKUJYO-READ-SEC            SECTION.
      *
           INITIALIZE                  YAKUJYO-REC
           MOVE    SYS-1001-HOSPID     TO  YAKUJYO-HOSPID
           MOVE    ORCHC30-SRYCD(CNT-PARA)
                                       TO  YAKUJYO-SRYCD
           MOVE    1                   TO  YAKUJYO-TEKNUM
           MOVE    YAKUJYO-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "YAKUJYO-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF  MCP-RC  =  ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "YAKUJYO-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF  MCP-RC  =  ZERO
                   MOVE    ZERO            TO  FLG-YAKUJYO
                   MOVE    MCPDATA-REC     TO  YAKUJYO-REC
               ELSE
                   MOVE    1               TO  FLG-YAKUJYO
               END-IF
           ELSE
               MOVE    1                   TO  FLG-YAKUJYO
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "YAKUJYO-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           .
       900-YAKUJYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC                 SECTION.
      *
           INITIALIZE                     TNS-TENSU-REC
           MOVE    WRK-SRYCD              TO  TNS-SRYCD
           MOVE    TNS-TENSU-REC          TO  MCPDATA-REC
           MOVE    SPA-SRYYMD             TO  ORC-DBYMD
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "TENSU-KEY"            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"            USING
                                          MCPAREA
                                          ORCMCPAREA
                                          MCPDATA-REC
           IF  MCP-RC  =  ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "TENSU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"        USING
                                          MCPAREA
                                          ORCMCPAREA
                                          MCPDATA-REC
               IF  MCP-RC  =  ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "TENSU-KEY"             TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−基本情報読み込み
      *****************************************************************
       900-1001-READ-SEC               SECTION.
      *
      *    医療機関情報−基本情報読み込み
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE    
               INITIALIZE                  SYS-1001-REC
           END-IF
           .
      *
       900-1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−所在地、連絡先読み込み
      *****************************************************************
       900-1002-READ-SEC               SECTION.
      *
      *    医療機関情報−所在地、連絡先読み込み
           MOVE    SPACE               TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           ELSE    
               INITIALIZE                  SYS-1002-REC
           END-IF
           .
      *
       900-1002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報読み込み
      *****************************************************************
       900-1010-READ-SEC               SECTION.
      *
      *    職員情報読み込み
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-DRCD            TO  SYS-1010-KBNCD
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
           END-IF
            .
      *
       900-1010-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名割り当て情報読み込み
      *****************************************************************
       900-1031-READ-SEC             SECTION.
      *
      *----(01.01.04)--UPD-START---
      *    出力先プリンタ名割り当て情報編集
      *     MOVE    SPACE               TO  SYS-1031-REC
      *     INITIALIZE                  SYS-1031-REC
      *     MOVE    "1031"              TO  SYS-1031-KANRICD
      *     MOVE    "*"                 TO  SYS-1031-KBNCD
      *     MOVE    SYS-1031-REC        TO  MCPDATA-REC
      *     PERFORM 800-SYSKANRI-READ-SEC
      *     IF      FLG-SYSKANRI        =   ZERO
      *         MOVE    MCPDATA-REC         TO  SYS-1031-REC
      *     ELSE
      *         INITIALIZE                  SYS-1031-REC
      *     END-IF
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF 
           .
      *
       900-1031-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
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
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
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
       800-SYSKANRI-READ-EXT.
           EXIT.
