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
       PROGRAM-ID.             ORCHCN68.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 医療区分・ＡＤＬ評価票
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/09/17    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  CONST-AREA.
           03  CONST-H3004         PIC X(06)   VALUE   "201604".
           03  CONST-H2804         PIC X(06)   VALUE   "201604".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCHCN68.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "CPONPRTDATA.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   ONPRTDATA
                                   .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    ONPRTDATA-DATA  TO  ORCHCN68AREA
      *
           EVALUATE    TRUE
           WHEN  ( ORCHCN68-SRYYM  >=  CONST-H3004 )
               CALL    "ORCHCN68H30"   USING
                                       SPA-AREA
                                       ONPRTDATA
           WHEN  ( ORCHCN68-SRYYM  >=  CONST-H2804 )
               CALL    "ORCHCN68H28"   USING
                                       SPA-AREA
                                       ONPRTDATA
           WHEN    OTHER
               CALL    "ORCHCN68H24"   USING
                                       SPA-AREA
                                       ONPRTDATA
      *
           END-EVALUATE
      *
           EXIT    PROGRAM
           .
