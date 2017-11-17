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
       PROGRAM-ID.                     ORCBDELTEMPDIR.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : バッチ処理
      *  コンポーネント名  : 一時ディレクトリ削除
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/11/17    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
       DATA                                DIVISION.
       FILE                                SECTION.
      *
       WORKING-STORAGE                     SECTION.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    一時ディレクトリ作成サブ
           COPY "CPORCSTEMPDIR.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  STEMPDIR-AREA
           MOVE    "DEL"           TO  STEMPDIR-MODE
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           EXIT PROGRAM
           .

