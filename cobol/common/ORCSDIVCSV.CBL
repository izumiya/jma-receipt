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
       PROGRAM-ID.             ORCSDIVCSV.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : CSVレコード分割
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/12/12    NACL-伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-EDIT-AREA.
               05  FLG-COMMA           PIC 9(01).
               05  FLG-DELI1           PIC 9(01).
               05  FLG-DELI2           PIC 9(01).
               05  FLG-NODATA          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(04).
               05  WRK-PO-ED           PIC 9(04).
           03  WRK-ELEMENT-NO          PIC 9(04).
           03  WRK-MOVE-AREA.
               05  WRK-MO-ST           PIC 9(04).
               05  WRK-MO-BYTE         PIC 9(04).
               05  WRK-WK1             PIC 9(04).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-RECSIZE-MAX       PIC 9(04)  VALUE 3000.
           03  CONST-TBL-MAX           PIC 9(03)  VALUE 100.
           03  CONST-ELESIZE-MAX       PIC 9(03)  VALUE 300.
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY  "CPORCSDIVCSV.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSDIVCSV-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  WRK-AREA
      *
           PERFORM 100-PARAM-CHECK-SEC
      *
           PERFORM 200-DIVIDE-CSV-SEC
                   UNTIL   FLG-END =   1
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    引数チェック
      *****************************************************************
       100-PARAM-CHECK-SEC         SECTION.
      *
           IF     (ORCSDIVCSV-REC      =   SPACE)  OR
                  (ORCSDIVCSV-ELE-C    =   ZERO )  OR
                  (ORCSDIVCSV-ELE-C    >   CONST-TBL-MAX )
               MOVE    1                   TO  ORCSDIVCSV-RC
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-PARAM-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVレコードから要素毎のデータを抽出
      *****************************************************************
       200-DIVIDE-CSV-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-POINT-AREA
      *
           PERFORM VARYING IDX
                           FROM    1
                           BY      1
                   UNTIL   FLG-END     =   1
               COMPUTE WRK-PO-ST           =   WRK-PO-ED   +   1
               MOVE    ZERO                TO  FLG-EDIT-AREA
               MOVE    IDX                 TO  WRK-ELEMENT-NO
      *
               IF      ORCSDIVCSV-REC (WRK-PO-ST:)
                                           =   SPACE
                   MOVE    WRK-PO-ST           TO  WRK-PO-ED
               ELSE
      *            項目終了文字後のカンマ位置を取得(WRK-PO-ED)
                   PERFORM 2001-CHAR-END-SEC
               END-IF
      *
      *        項目共通処理
               PERFORM 2002-ELEMENT-KYOTU-SEC
      *
               IF     (IDX                 =   ORCSDIVCSV-ELE-C)   OR
                      (WRK-PO-ED          >=   CONST-RECSIZE-MAX)
                   MOVE    1                   TO  FLG-END
               END-IF
           END-PERFORM
           .
       200-DIVIDE-CSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象項目の文字終了位置を取得
      *****************************************************************
       2001-CHAR-END-SEC           SECTION.
      *
           PERFORM VARYING IDY
                           FROM    WRK-PO-ST
                           BY      1
                   UNTIL  (IDY        >=   CONST-RECSIZE-MAX)
                       OR (FLG-COMMA   =   1)
      *        カンマの位置
               IF     (ORCSDIVCSV-CHAR (IDY)
                                           =   ",")    OR
                     ((IDX                 =   ORCSDIVCSV-ELE-C)   AND
                      (ORCSDIVCSV-CHAR (IDY)
                                           =   SPACE))
                   MOVE    IDY                 TO  WRK-PO-ED
                   MOVE    1                   TO  FLG-COMMA
               END-IF
      *        デリミタ(")存在するかどうか
               IF      ORCSDIVCSV-CHAR (IDY)
                                           =   """"
                   IF      FLG-DELI1           =   ZERO
                       MOVE    1                   TO  FLG-DELI1
                   ELSE
                       MOVE    1                   TO  FLG-DELI2
                   END-IF
               END-IF
           END-PERFORM
           .
       2001-CHAR-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目共通処理
      *****************************************************************
       2002-ELEMENT-KYOTU-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-MOVE-AREA
      *
      *    データが省略されている場合、FLG-NODATAに1をセット
           IF      FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               IF      FLG-DELI2           =   1
                   COMPUTE WRK-WK1     =   WRK-PO-ST   +   2
               ELSE
                   COMPUTE WRK-WK1     =   WRK-PO-ST   +   1
               END-IF
               IF      WRK-WK1             =   WRK-PO-ED
                   MOVE    1                   TO  FLG-NODATA
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               IF      WRK-PO-ST           =   WRK-PO-ED
                   MOVE    1                   TO  FLG-NODATA
               END-IF
           END-IF
      *
           IF      FLG-NODATA          =   ZERO
      *    転送開始位置・転送桁数を算出
               IF      FLG-DELI1           =   1
      *            デリミタ(")を含んでいる場合
                   COMPUTE WRK-MO-ST           =   WRK-PO-ST   +   1
                   IF      FLG-DELI2           =   1
                       COMPUTE WRK-MO-BYTE         =   WRK-PO-ED
                                                   -   WRK-PO-ST
                                                   +   2
                   ELSE
                       COMPUTE WRK-MO-BYTE         =   WRK-PO-ED
                                                   -   1
                                                   -   WRK-PO-ST
                   END-IF
               ELSE
      *            デリミタ(")を含んでいない場合
                   MOVE    WRK-PO-ST           TO  WRK-MO-ST
                   COMPUTE WRK-MO-BYTE         =   WRK-PO-ED
                                               -   WRK-PO-ST
               END-IF
           END-IF
      *
           PERFORM 20021-DATA-COPY-SEC
           .
       2002-ELEMENT-KYOTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    要素データをテーブルへ格納
      *****************************************************************
       20021-DATA-COPY-SEC         SECTION.
      *
           IF      WRK-MO-BYTE         >   CONST-ELESIZE-MAX
               MOVE    2                   TO  ORCSDIVCSV-RC
               MOVE    1                   TO  FLG-END
               GO  TO  20021-DATA-COPY-EXT
           END-IF
      *
           IF     (WRK-MO-BYTE         =   ZERO)   OR
                  (FLG-NODATA          =   1   )
               MOVE    SPACE               TO  ORCSDIVCSV-D (IDX)
           ELSE
               MOVE    ORCSDIVCSV-REC (WRK-MO-ST:WRK-MO-BYTE)
                                           TO  ORCSDIVCSV-D (IDX)
           END-IF
           .
       20021-DATA-COPY-EXT.
           EXIT.
      *
