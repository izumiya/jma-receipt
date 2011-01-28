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
       PROGRAM-ID.             ORCSINFCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 感染症サーベイランス処理チェック処理
      *  返却エラーコード  : 00:正常
      *                      10:データ作成なし(システム管理)
      *                      20:６時より前
      *                      30:データ作成処理中
      *                      40:データ作成処理済み
      *                      50:参加意思なし
      *                      90:データ作成処理中(開始日が当日でない)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09.12.09    伊藤          新規作成
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
      *    定点設定データ
           SELECT  CONFIG-FILE     ASSIGN  FILE-NAME
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-CONFIG.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    定点設定データ
       FD  CONFIG-FILE.
       01  CONFIG-REC              PIC X(600).
      *
       WORKING-STORAGE             SECTION.
      *
       01  FILE-NAME.
           03  FILLER              PIC X(40)   VALUE
           "/etc/jma-receipt/das-upload.d/das-upload".
           03  FILE-HOSPNUM        PIC 9(02)   VALUE   ZERO.
           03  FILLER              PIC X(05)   VALUE   ".conf".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-CONFIG          PIC X(02).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  FLG-CONFIG          PIC 9(01).
           03  WRK-AGREEMENT       PIC X(01).
           03  WRK-SENDRESULT      PIC X(03).
      *    ジョブ管理ＤＢのキー値（感染症データ）
           03  WRK-CONS-JOB-SHELLID-INFECT
                                   PIC X(08)   VALUE   "INFECTIO".
           03  WRK-CONS-JOB-JOBID-INFECT
                                   PIC 9(07)   VALUE   1.
       01  WRK-DATA-AGREE.
           03  FILLER              PIC X(16)   VALUE
               '"Agreement" => "'.
       01  WRK-DATA-RESULT.
           03  FILLER              PIC X(17)   VALUE
               '"SendResult" => "'.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSINFCHK.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSINFCHKAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    ZERO                TO  INFCHK-RC
      *
      *    ジョブ管理チェック（データ収集）
      *##  IF      SPA-DATACREATEFLG   NOT =   1
           IF      SPA-DATATEISYUTUKBN NOT =   2
               MOVE    10                  TO  INFCHK-RC
           END-IF
      *
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      INFCHK-RC           =   ZERO
      *        ６時以降？
               IF      SMCNDATE-HMS      <=    "060000"
                   MOVE    20                  TO  INFCHK-RC
               END-IF
           END-IF
      *
           IF      INFCHK-RC           =   ZERO
               PERFORM 100-JOBEXEC-CHK-SEC
           END-IF
      *
           IF      INFCHK-RC           =   ZERO
               PERFORM 200-AGREEMENT-CHK-SEC
           END-IF
           .
      *
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    感染症データ作成処理チェック処理
      *****************************************************************
       100-JOBEXEC-CHK-SEC         SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID-INFECT
                                       TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID-INFECT
                                       TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ENDYMD          =   SPACE
                   MOVE    30              TO  INFCHK-RC
                   IF      JOB-STARTYMD    NOT =   SMCNDATE-YMD
                       MOVE    90          TO  INFCHK-RC
                   END-IF
               ELSE
                   IF      JOB-ENDYMD      =   SMCNDATE-YMD
                       MOVE    40          TO  INFCHK-RC
                   END-IF
               END-IF
           END-IF
           .
       100-JOBEXEC-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    収集データファイル有無チェック処理
      *****************************************************************
       200-AGREEMENT-CHK-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM
           OPEN    INPUT               CONFIG-FILE
           IF      STS-CONFIG          =   ZERO
               PERFORM 2001-AGREEMENT-CHK-SEC
               IF    ( WRK-AGREEMENT        =   "1"   )  AND
                     ( WRK-SENDRESULT       =   "000" )
                   CONTINUE
               ELSE
                   MOVE    50              TO  INFCHK-RC
               END-IF
           ELSE
               MOVE    50              TO  INFCHK-RC
           END-IF
           CLOSE                       CONFIG-FILE
           .
       200-AGREEMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    収集データファイル有無チェック処理
      *****************************************************************
       2001-AGREEMENT-CHK-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-AGREEMENT
           MOVE    SPACE           TO  WRK-SENDRESULT
           MOVE    ZERO            TO  FLG-CONFIG
           MOVE    SPACE           TO  CONFIG-REC
           PERFORM UNTIL   FLG-CONFIG  =   1
               READ    CONFIG-FILE     NEXT
                   AT  END
                       MOVE    1           TO  FLG-CONFIG
                   NOT AT  END
                       IF      CONFIG-REC(1:16)  =   WRK-DATA-AGREE
                           UNSTRING  CONFIG-REC(17:) DELIMITED  BY '",'
                                               INTO  WRK-AGREEMENT
                           END-UNSTRING
                       END-IF
                       IF      CONFIG-REC(1:17)  =   WRK-DATA-RESULT
                           UNSTRING  CONFIG-REC(18:) DELIMITED  BY '",'
                                               INTO  WRK-SENDRESULT
                           END-UNSTRING
                       END-IF
               END-READ
           END-PERFORM
           .
       2001-AGREEMENT-CHK-EXT.
           EXIT.
      *
