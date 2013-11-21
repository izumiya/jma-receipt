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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCS02MSG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 負担金計算サブエラーメッセージ編集
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/01/28    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       DATA                            DIVISION.
       WORKING-STORAGE             SECTION.
      *
       01  WRK-AREA.
           03  WRK-MSG                 PIC X(100).
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCS02MSG.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           S02MSG-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  S02MSG-MSG-AREA
      *
           EVALUATE    S02MSG-S02-RC
           WHEN    13
               STRING  "退院計算に必要な会計情報が作成されていません"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    14
               STRING  "入院料と異なる保険の入院料加算があります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    15
               STRING  "外泊日に入院料加算が算定されています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    16
               STRING  "入院料の算定が無い日に入院料加算が"
                                           DELIMITED   BY  SIZE
                       "算定されています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    17
               STRING  "入院料の有効期間が切れています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    18
               STRING  "保険組合せの有効期間が切れています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    19
               STRING  "包括点数の治験減点分の点数を"
                                           DELIMITED   BY  SIZE
                       "確認してください"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    20
               STRING  "入院点数に誤りがあります。"
                                           DELIMITED   BY  SIZE
                       "異動処理をしてください"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    21
               STRING  "救急搬送診療料の算定日に"
                                           DELIMITED   BY  SIZE
                       "入院料（加算）が算定されています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    22
               STRING  "他医受診（外泊）の入力に誤りがあります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    23
               STRING  "同日再入院日に実日数算定コード"
                                           DELIMITED   BY  SIZE
                       "を入力してください"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    24
               STRING  "同日再入院日の再入院分に入院料加算"
                                           DELIMITED   BY  SIZE
                       "が算定されています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    25
               STRING  "薬剤料減点数を確認してください"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    26
               STRING  "特定時間退院減算（外泊）の入力に誤りがあります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           WHEN    27
               STRING  "特定曜日入退院減算（外泊）の入力に"
                                           DELIMITED   BY  SIZE
                       "誤りがあります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
           END-EVALUATE
      *
           MOVE    WRK-MSG             TO  S02MSG-MSG (1)
                                           S02MSG-MSG (2)
                                           S02MSG-MSG (3)
      *
           .
           EXIT PROGRAM
           .
