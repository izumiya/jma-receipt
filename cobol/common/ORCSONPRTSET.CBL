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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSONPRTSET.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ONLINE帳票再印刷準備処理
      *  コンポーネント名  : GROUP & UUID 作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/10    NACL−竹田　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
      *
      *    フラグ領域
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-JOBKANRI                            PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC 9(03). 
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
      *
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).  
               05  WRK-SYSDD           PIC 9(02).  
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *    ＵＩＤ取得用エリア
       01 UIDIO-AREA.
           03  C-RET   PIC X(2).
           03  C-UID   PIC X(36).
       01  ST          PIC 9(2).
      *
      **************************************************************************
      *
       LINKAGE                 SECTION.
           COPY    "COMMON-SPA".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *???
           DISPLAY "ORCSONPRTSET ST ="
      *???     
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
      *    印刷DBのGROUP key 用
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    SPACE               TO  SPA-PRT-GROUP            
           MOVE    WRK-SYSYMD          TO
                                   SPA-PRT-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                   SPA-PRT-GROUP (9:)
      *
      * UUID GET
           INITIALIZE     UIDIO-AREA.
           CALL    "orcuidset" USING ST UIDIO-AREA
           DISPLAY "UUID CALL" UIDIO-AREA.
           MOVE    C-UID           TO  SPA-PRT-UUID
           IF    ( SPA-GSRAUTH-31  =   "1" )
               MOVE    1           TO  SPA-CLIENT-PRT-FLG
           END-IF
      *
           DISPLAY "ORCSONPRTSET ED ="
      *???     
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
