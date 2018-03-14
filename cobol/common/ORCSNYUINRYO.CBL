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
       PROGRAM-ID.             ORCSNYUINRYO.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 入院料取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/04/07    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-KOKI            PIC 9(01).
           03  FLG-LIFE-RJN        PIC 9(01).
           03  FLG-RYOYOKHN        PIC 9(01).
           03  FLG-EDIT            PIC 9(01).
           03  FLG-TARGET          PIC 9(01).
           03  FLG-SKIP            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX10               PIC 9(05).
           03  IDX17               PIC 9(05).
           03  IDX-LIFE            PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TOKNYUIN-G.
               05  WRK-TOKNYUIN-KBN        PIC X(01).
               05  WRK-TOKNYUIN-CD         PIC X(02).
           03  WRK-TOKTEMP-G.
               05  WRK-TOKTEMP-KBN         PIC X(01).
               05  WRK-TOKTEMP-CD          PIC X(02).
           03  WRK-NYUINRYO-AREA.
               05  WRK-NYUINRYO-NAME       PIC X(60).
               05  WRK-NYUINRYO-G.
                   07  WRK-NYUINRYO-KBN    PIC X(01).
                   07  WRK-NYUINRYO-CD     PIC X(02).
               05  WRK-NYUINRYO-SRYCD-X.
                   07  WRK-NYUINRYO-SRYCD  PIC 9(09).
               05  WRK-NYUINRYO-TYPE       PIC X(02).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-N02-G.
               05  WRK-N02                 PIC 99.
           03  WRK-HOSPSBT                 PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-NYUIN                 PIC X(02)   VALUE "01".
           03  CONST-H280401               PIC X(08)   VALUE "20160401".
           03  CONST-H261001               PIC X(08)   VALUE "20141001".
           03  CONST-H260401               PIC X(08)   VALUE "20140401".
           03  CONST-H241001               PIC X(08)   VALUE "20121001".
           03  CONST-H240401               PIC X(08)   VALUE "20120401".
           03  CONST-H220401               PIC X(08)   VALUE "20100401".
           03  CONST-H200401               PIC X(08)   VALUE "20080401".
           03  CONST-H181001               PIC X(08)   VALUE "20061001".
           03  CONST-H180701               PIC X(08)   VALUE "20060701".
           03  CONST-KANSATU               PIC X(02)    VALUE "10".
           03  CONST-TOKNYUIN-MAX          PIC 9(05)   VALUE  100.
      *
           COPY    "CPRYOYOCD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    病棟情報
           COPY    "CPSK5001.INC".
      *    病室情報
           COPY    "CPSK5002.INC".
      *    短期３
           COPY    "CPSK5003.INC".
      *    特定入院料−病棟
           COPY    "CPSK5110.INC".
      *    特定入院料−病室
           COPY    "CPSK5111.INC".
      *    医療観察入院医学管理料
           COPY    "CPSK5112.INC".
      *
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保健情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    病室名編集
           COPY    "CPORCSBRMNUM.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    特定入院料コード取得サブ
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *    施設基準取得
           COPY "CPORCSS014.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSNYUINRYO.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SNYUINRYO-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    SNYUINRYO-BTUNUM    TO  SYS-KBNCD
           MOVE    SNYUINRYO-SRYYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
      *
           INITIALIZE                      ORCSBRMNUMAREA
           MOVE    "2"                 TO  SBRMNUM-IN-PROCKBN
           MOVE    SNYUINRYO-BRMNUM    TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"        USING   ORCSBRMNUMAREA
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5002"              TO  SYS-KANRICD
           MOVE    SNYUINRYO-BTUNUM    TO  SYS-KBNCD(1:2)
           MOVE    SBRMNUM-OT-BRMNUM   TO  SYS-KBNCD(3:6)
           MOVE    SNYUINRYO-SRYYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5002-REC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *
           INITIALIZE                  PTRSIINF-REC
           IF    ( COMB-HKNNUM         =    "971" OR "973" )
               PERFORM 900-PTRSIINF-KEY-SEL-SEC
           END-IF
      *
           INITIALIZE ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SNYUINRYO-SRYYMD    TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           MOVE    SNYUINRYO-HKNCOMBI  TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF    ( AGECHK-RC           =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
           MOVE    ZERO                    TO  FLG-KOKI
      *
           IF    ( SNYUINRYO-SRYYMD   >=   CONST-H200401 )
      *
      *        患者保険情報から後期高齢者の登録チェック
               INITIALIZE                      PTHKNINF-REC
               MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
               MOVE    SPA-PTID            TO  PTHKN-PTID
               MOVE    SNYUINRYO-SRYYMD    TO  PTHKN-TEKSTYMD
                                               PTHKN-TEKEDYMD
               PERFORM 900-PTHKNINF-KEY11-SEL-SEC
               IF    ( FLG-PTHKNINF        =   ZERO )
                   MOVE    1               TO  FLG-KOKI
               END-IF
      *
      *        患者公費情報から老人保健の登録チェック
               INITIALIZE                      PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-PTID            TO  PTKOH-PTID
               MOVE    "968"               TO  PTKOH-KOHNUM
               MOVE    SNYUINRYO-SRYYMD    TO  PTKOH-TEKSTYMD
                                               PTKOH-TEKEDYMD
               PERFORM 900-PTKOHINF-KEY6-SEL-SEC
               IF    ( FLG-PTKOHINF        =   ZERO )
                   MOVE    1               TO  FLG-KOKI
               END-IF
           ELSE
      *
      *        高齢者対象患者判定
               IF    (COMB-KOH1HKNNUM      =   "027" )
                 OR  ( AGECHK-NENREI      >=    75)
                 OR  ((AGECHK-NENREI      >=    70)
                  AND (PTINF-BIRTHDAY     <=   "19320930"))
                 OR  ( AGECHK-ROUJIN       =    1 )
                   MOVE    1               TO  FLG-KOKI
               END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                    TO  FLG-LIFE-RJN
      *
           PERFORM 2001-LIFE-CHK-SEC
      *
           IF    ( SYS-5001-BTU-SBT    =   CONST-KANSATU )
               PERFORM 2001-KANSATU-EDIT-SEC
           ELSE
               PERFORM 2001-NYUINRYO-EDIT-SEC
           END-IF
      *
           MOVE    SYS-5001-BTU-KHNSRYCD   TO  SNYUINRYO-BTU-KHNSRYCD
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    生活療養対象患者チェック処理
      *****************************************************************
       2001-LIFE-CHK-SEC           SECTION.
      *
           IF    ( PTRSI-HKNKBN            =   SPACE )
            OR   ( AGECHK-HKNKBN           =   4 OR 5 OR 7 )
      *
      *
               EVALUATE    TRUE
               WHEN  ( SNYUINRYO-SRYYMD   >=  CONST-H200401 )
                   IF    ( FLG-KOKI        =   1  ) 
                    OR   ( AGECHK-NENREI  >=  65 )
                       MOVE    1           TO FLG-LIFE-RJN
                   END-IF
               WHEN  ( SNYUINRYO-SRYYMD  >=  CONST-H181001 )
      *
      *            高齢者対象患者判定
                   IF    ( FLG-KOKI        =   1  ) 
                    OR   ( AGECHK-NENREI  >=   70 )
                       MOVE    1           TO  FLG-LIFE-RJN
                   END-IF
               END-EVALUATE
      *
               IF    ( FLG-LIFE-RJN        =   1 )
                   IF    ( AGECHK-HKNKBN   =   4 OR 5 )
                       MOVE    2           TO  FLG-LIFE-RJN
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       2001-LIFE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       2001-NYUINRYO-EDIT-SEC          SECTION.
      *
           IF    ( SNYUINRYO-SRYYMD   >=   CONST-H180701 )
            AND  ( SYS-5001-BTU-SBT    =   "04" OR "09" )
               IF    ( SYS-5001-BTU-SBT    =   "04" )
                   MOVE    1           TO  FLG-RYOYOKHN
               ELSE
                   MOVE    2           TO  FLG-RYOYOKHN
               END-IF
               PERFORM 20011-RYOYOKHN-EDIT-SEC
           END-IF
      *
           IF    ( SYS-5001-BTU-SBT    =   "04" )
            AND  ( SNYUINRYO-SRYYMD   >=   CONST-H280401 )
      *
               INITIALIZE                  SS014-AREA
               MOVE    SNYUINRYO-SRYYMD TO SS014-SRYYMD
               CALL    "ORCSS014"      USING
                                       SS014-AREA
                                       SPA-AREA
               IF    ( SS014-SSTKJN-CD (8015)  =   "1" )
                   PERFORM 20011-RYOYOIPN-EDIT-SEC
               END-IF
           END-IF
      *
           IF    ( SYS-5001-BTU-TOKTEI-NYUIN   NOT =   SPACE )
               EVALUATE    TRUE
               WHEN  ( SYS-5001-BTU-TOKTEI-NYUIN
                           =   "01" OR "02" OR "14"
                            OR "17" OR "26" )
                   EVALUATE    TRUE
                   WHEN  ( SNYUINRYO-SRYYMD   >=   CONST-H280401 )
                    AND  ( AGECHK-NENREI2-YY   <   20 )
                   WHEN  ( SNYUINRYO-SRYYMD    <   CONST-H280401 )
                    AND  ( AGECHK-NENREI2-YY   <   15 )
                       MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                       MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                           TO  WRK-TOKNYUIN-CD
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                   END-EVALUATE
               WHEN    OTHER
                   MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                   MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                       TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
               END-EVALUATE
      *
               IF    ( SNYUINRYO-SRYYMD   >=   CONST-H280401 )
                   IF    ( SYS-5001-BTU-TOKTEI-NYUIN   =   "03" )
                       MOVE    "49"        TO  WRK-TOKNYUIN-CD
                       MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                       MOVE    "50"        TO  WRK-TOKNYUIN-CD
                       MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                   END-IF
                   IF    ( SYS-5001-BTU-TOKTEI-NYUIN   =   "04" )
                       MOVE    "51"        TO  WRK-TOKNYUIN-CD
                       MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                       MOVE    "52"        TO  WRK-TOKNYUIN-CD
                       MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                   END-IF
               END-IF
      *
               IF    ( SYS-5001-BTU-TOKTEI-NYUIN   =   "11" )
                AND  ( SNYUINRYO-SYORIKBN      NOT =   CONST-NYUIN )
                   MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                   MOVE    "24"        TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
               END-IF
      *
               IF    ( SYS-5001-BTU-TOKTEI-NYUIN   =   "12" )
                AND  ( SNYUINRYO-SYORIKBN      NOT =   CONST-NYUIN )
                   MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                   MOVE    "25"        TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
               END-IF
      *
           END-IF
      *
           IF    ( SYS-5002-BRM-TOKTEI-NYUIN
                                   NOT =   SPACE )
               EVALUATE    TRUE
               WHEN    ( SYS-5002-BRM-TOKTEI-NYUIN   =   "02" )
      *
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   MOVE    "02"        TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
      *
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   MOVE    "23"        TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
               WHEN    ( SYS-5002-BRM-TOKTEI-NYUIN = "18" OR "54" )
                   AND ( SNYUINRYO-SRYYMD >=   CONST-H240401 )
      *
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   MOVE    "18"        TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
      *
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   MOVE    "54"        TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
               WHEN    ( SYS-5002-BRM-TOKTEI-NYUIN = "92" OR "93" )
                   AND ( SNYUINRYO-SRYYMD >=   CONST-H240401 )
      *
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   MOVE    "92"        TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
      *
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   MOVE    "93"        TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
               WHEN    OTHER
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   MOVE    SYS-5002-BRM-TOKTEI-NYUIN
                                       TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
               END-EVALUATE
      *
               IF    ( SNYUINRYO-SRYYMD   >=   CONST-H280401 )
                   IF    ( SYS-5002-BRM-TOKTEI-NYUIN
                                           =   "01" )
                       MOVE    "BT"        TO  WRK-TOKNYUIN-CD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                       MOVE    "BU"        TO  WRK-TOKNYUIN-CD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( SNYUINRYO-SRYYMD   >=   CONST-H280401 )
               IF    ( SYS-5001-BTU-SBT    =   "05" )
                   IF    ( SYS-5001-BTU-KHNSRYCD   =   "190126310" )
                       MOVE    "BK"        TO  WRK-TOKNYUIN-CD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                       MOVE    "BL"        TO  WRK-TOKNYUIN-CD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                   END-IF
                   IF    ( SYS-5001-BTU-KHNSRYCD   =   "190093710" )
                       MOVE    "BM"        TO  WRK-TOKNYUIN-CD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                       MOVE    "BN"        TO  WRK-TOKNYUIN-CD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                   END-IF
                   IF    ( SYS-5001-BTU-KHNSRYCD   =   "190093810" )
                       MOVE    "BO"        TO  WRK-TOKNYUIN-CD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                       MOVE    "BP"        TO  WRK-TOKNYUIN-CD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                   END-IF
                   IF    ( SYS-5001-BTU-KHNSRYCD   =   "190093910" )
                       MOVE    "BQ"        TO  WRK-TOKNYUIN-CD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                       MOVE    "BR"        TO  WRK-TOKNYUIN-CD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 2002-NYUINRYO-EDIT-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SNYUINRYO-SRYYMD >=   CONST-H240401 )
               EVALUATE  TRUE
               WHEN  ( SYS-5001-BTU-SBT    =   "08" )
                AND  ( SYS-5001-CRYOYOKBN  =   "1" )
                   MOVE    4         TO  FLG-RYOYOKHN
                   PERFORM 20011-RYOYOKHN-EDIT-SEC
               WHEN  ( SYS-5001-BTU-SBT    =   "09" )
                   MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                   EVALUATE    TRUE
                   WHEN  ( SNYUINRYO-SRYYMD   >=  CONST-H260401 )
                       EVALUATE    SYS-5001-CRYOYOKBN
                       WHEN    "1"
                           MOVE    "38"    TO  WRK-TOKNYUIN-CD
                           PERFORM 2002-NYUINRYO-EDIT-SEC
                       WHEN    "2"
                           MOVE    "39"    TO  WRK-TOKNYUIN-CD
                           PERFORM 2002-NYUINRYO-EDIT-SEC
                       WHEN    "3"
                           MOVE    "40"    TO  WRK-TOKNYUIN-CD
                           PERFORM 2002-NYUINRYO-EDIT-SEC
                       WHEN    "4"
                           MOVE    "35"    TO  WRK-TOKNYUIN-CD
                           PERFORM 2002-NYUINRYO-EDIT-SEC
                       WHEN    "5"
                           MOVE    "36"    TO  WRK-TOKNYUIN-CD
                           PERFORM 2002-NYUINRYO-EDIT-SEC
                       WHEN    "6"
                           MOVE    "37"    TO  WRK-TOKNYUIN-CD
                           PERFORM 2002-NYUINRYO-EDIT-SEC
                       END-EVALUATE
                   WHEN    OTHER
                       EVALUATE    SYS-5001-CRYOYOKBN
                       WHEN    "1"
                           MOVE    "35"    TO  WRK-TOKNYUIN-CD
                           PERFORM 2002-NYUINRYO-EDIT-SEC
                       WHEN    "2"
                           MOVE    "36"    TO  WRK-TOKNYUIN-CD
                           PERFORM 2002-NYUINRYO-EDIT-SEC
                       WHEN    "3"
                           MOVE    "37"    TO  WRK-TOKNYUIN-CD
                           PERFORM 2002-NYUINRYO-EDIT-SEC
                       END-EVALUATE
                   END-EVALUATE
               WHEN  ( SYS-5001-BTU-TOKTEI-NYUIN
                                     =  "33" OR "34" )
                   MOVE    3         TO  FLG-RYOYOKHN
                   PERFORM 20011-RYOYOKHN-EDIT-SEC
               WHEN  ( SYS-5001-BTU-SBT    =   "01" )
                AND  ( SYS-5001-CRYOYOKBN  =   "1" )
                   IF    ( SNYUINRYO-SRYYMD   >=  CONST-H241001 )
                       MOVE    5         TO  FLG-RYOYOKHN
                       PERFORM 20011-RYOYOKHN-EDIT-SEC
                   END-IF
               WHEN  ( SYS-5001-BTU-SBT    =   "06" )
                AND  ( SYS-5001-CRYOYOKBN  =   "1" )
                   IF    ( SNYUINRYO-SRYYMD   >=  CONST-H261001 )
                       MOVE    5         TO  FLG-RYOYOKHN
                       PERFORM 20011-RYOYOKHN-EDIT-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *
      *    後期高齢者の場合、後期高齢者特定入院基本料をコンボに設定
      *    平成２２年４月から年齢要件廃止
      *
           MOVE    ZERO        TO  FLG-EDIT
           IF  ( SNYUINRYO-SRYYMD >=  CONST-H220401 )
      *
               MOVE    1       TO  FLG-EDIT
      *
               IF    ( SNYUINRYO-SRYYMD       >= CONST-H240401 )
                   IF    ( SYS-5001-BTU-SBT    =   "01" OR "05" OR "06")
                       IF    ( SYS-5001-BTU-TOKTEI-NYUIN
                                           =   "33" OR "34" )
                           MOVE    ZERO    TO  FLG-EDIT
                       END-IF
                   END-IF
               END-IF
      *
               IF  ( SNYUINRYO-SRYYMD   >=  CONST-H241001 )
                   IF    ( SYS-5001-BTU-SBT    =   "01" )
                     AND ( SYS-5001-CRYOYOKBN  =   "1" )
                           MOVE    ZERO    TO  FLG-EDIT
                   END-IF
               END-IF
           ELSE
               IF  ( FLG-KOKI     =   1 )
                   MOVE    1      TO  FLG-EDIT
               END-IF
           END-IF
      *
           IF    ( FLG-EDIT            =   1 )
               IF    ( SNYUINRYO-SRYYMD   >=  CONST-H261001 )
                   IF    ( SYS-5001-BTU-SBT = "05" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-EDIT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-EDIT            =   1 )
               MOVE    "3"         TO  WRK-TOKNYUIN-KBN
               MOVE    "22"        TO  WRK-TOKNYUIN-CD
               PERFORM 2002-NYUINRYO-EDIT-SEC
      *
               IF    ( SNYUINRYO-SRYYMD   >=  CONST-H260401 )
                AND  ( SYS-5001-BTU-SBT    =   "05" OR "06")
                   CONTINUE
               ELSE
                   MOVE    "3"     TO  WRK-TOKNYUIN-KBN
                   MOVE    "24"    TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
               END-IF
           END-IF
      *
      *    短期滞在手術基本料をコンボに設定
           MOVE    "3"         TO  WRK-TOKNYUIN-KBN
           MOVE    "21"        TO  WRK-TOKNYUIN-CD
           PERFORM 2002-NYUINRYO-EDIT-SEC
      *
           IF    ( SNYUINRYO-SRYYMD  >=  CONST-H260401 )
               PERFORM 2002-TANKI3-EDIT-SEC
           ELSE
               MOVE    "3"         TO  WRK-TOKNYUIN-KBN
               MOVE    "52"        TO  WRK-TOKNYUIN-CD
               PERFORM 2002-NYUINRYO-EDIT-SEC
           END-IF
      *
           IF    ( SNYUINRYO-EXTRA-G   NOT =   SPACE )
               MOVE    SNYUINRYO-EXTRA-KBN TO  WRK-TOKNYUIN-KBN
               MOVE    SNYUINRYO-EXTRA-CD  TO  WRK-TOKNYUIN-CD
               PERFORM 2002-NYUINRYO-EDIT-SEC
           END-IF
      *
           .
       2001-NYUINRYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    療養病棟入院基本料編集処理
      *****************************************************************
       20011-RYOYOKHN-EDIT-SEC     SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5111"              TO  SYS-KANRICD
           MOVE    SNYUINRYO-SRYYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY23-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5111-REC
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    ZERO        TO  FLG-TARGET
      *
               IF    ( FLG-RYOYOKHN                =   1 )
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "05" )
                       IF    ( FLG-LIFE-RJN    =   ZERO OR 2)
                           IF    ( SYS-5111-SEIKATU-RYOKBN
                                               =   SPACE )
                               MOVE    1   TO  FLG-TARGET
                           END-IF
                       END-IF
                       IF    ( FLG-LIFE-RJN    =   1    OR 2)
                           IF    ( SYS-5111-SEIKATU-RYOKBN
                                           NOT =   SPACE )
                               MOVE    1   TO  FLG-TARGET
                           END-IF
                       END-IF
               END-IF
      *
               IF    ( FLG-RYOYOKHN                =   3 )
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "05" )
                  IF    ( SNYUINRYO-SRYYMD        >=   CONST-H260401 )
                          IF    ( SYS-5111-SEIKATU-RYOKBN
                                              =   SPACE )
                              MOVE    1   TO  FLG-TARGET
                          END-IF
                  ELSE
                       IF    ( FLG-LIFE-RJN    =   ZERO OR 2)
                           IF    ( SYS-5111-SEIKATU-RYOKBN
                                               =   SPACE )
                               MOVE    1   TO  FLG-TARGET
                           END-IF
                       END-IF
                       IF    ( FLG-LIFE-RJN    =   1    OR 2)
                           IF    ( SYS-5111-SEIKATU-RYOKBN
                                           NOT =   SPACE )
                               MOVE    1   TO  FLG-TARGET
                           END-IF
                       END-IF
                  END-IF
               END-IF
      *
               IF    ( FLG-RYOYOKHN                =   5 )
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "05" )
                   IF    ( SYS-5111-SEIKATU-RYOKBN
                                               =   SPACE )
                       MOVE    1   TO  FLG-TARGET
                   END-IF
               END-IF
      *
               IF    ( FLG-RYOYOKHN                =   2 OR 4)
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "06" )
                   IF    ( FLG-LIFE-RJN        =   ZERO OR 2)
                       IF    ( SYS-5111-SEIKATU-RYOKBN
                                                   =   SPACE )
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF    ( FLG-LIFE-RJN        =   1    OR 2)
                       IF    ( SYS-5111-SEIKATU-RYOKBN
                                               NOT =   SPACE )
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( SNYUINRYO-SRYYMD  >=  CONST-H220401 )
                   IF    ( SYS-5111-SRYCD
                                       =   "099999953"
                                        OR "099999954" )
                           MOVE  ZERO  TO  FLG-TARGET
                   END-IF
                   IF   (( FLG-RYOYOKHN    =   1 )
                    AND  ( SYS-5001-KHNSRYCD
                                       =   SPACE
                                  OR       "099999953"
                                  OR       "190121310"
                                  OR       "190121410"
                                  OR       "190121510"
                                  OR       "190121610"
                                  OR       "190121710"
                                  OR       "190131610"
                                  OR       "190131810"
                                  OR       "190132010"
                                  OR       "190132210"
                                  OR       "190121810" ))
                    OR   ( FLG-RYOYOKHN    =   3 OR 5 )
                       IF    ( SYS-5111-SRYCD
                                       =   "190121310"
                                  OR       "190121410"
                                  OR       "190121510"
                                  OR       "190121610"
                                  OR       "190121710"
                                  OR       "190131610"
                                  OR       "190131810"
                                  OR       "190132010"
                                  OR       "190132210"
                                  OR       "190123710"
                                  OR       "190123810"
                                  OR       "190123910"
                                  OR       "190124010"
                                  OR       "190124110"
                                  OR       "190131710"
                                  OR       "190131910"
                                  OR       "190132110"
                                  OR       "190132310"
                                  OR       "190121810"
                                  OR       "190124210" )
                           CONTINUE
                       ELSE
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF   (( FLG-RYOYOKHN    =   1 )
                    AND  ( SYS-5001-KHNSRYCD
                                       =   "099999954"
                                  OR       "190132410"
                                  OR       "190132610"
                                  OR       "190132810"
                                  OR       "190133010"
                                  OR       "190133210"
                                  OR       "190133410"
                                  OR       "190133610"
                                  OR       "190133810"
                                  OR       "190134010"
                                  OR       "190121810" ))
                       IF    ( SYS-5111-SRYCD
                                       =   "190132410"
                                  OR       "190132610"
                                  OR       "190132810"
                                  OR       "190133010"
                                  OR       "190133210"
                                  OR       "190133410"
                                  OR       "190133610"
                                  OR       "190133810"
                                  OR       "190134010"
                                  OR       "190132510"
                                  OR       "190132710"
                                  OR       "190132910"
                                  OR       "190133110"
                                  OR       "190133310"
                                  OR       "190133510"
                                  OR       "190133710"
                                  OR       "190133910"
                                  OR       "190134110"
                                  OR       "190121810"
                                  OR       "190124210" )
                           CONTINUE
                       ELSE
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF   (  FLG-RYOYOKHN    =   3 OR 5 )
                       IF    ( SYS-5111-SRYCD
                                  =        "190121810"
                                  OR       "190124210" )
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF   (  FLG-RYOYOKHN    =   4 )
                       IF    ( SYS-5111-SRYCD
                                  =        "190122510"
                                  OR       "190124810" )
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
                   MOVE    "3"             TO  WRK-TOKNYUIN-KBN
                   MOVE    SYS-5111-KBNCD  TO  WRK-TOKNYUIN-CD
                   PERFORM 20021-NYUINRYO-EDIT-SEC
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY23-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-RYOYOKHN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    療養・一般病棟入院基本料編集処理
      *****************************************************************
       20011-RYOYOIPN-EDIT-SEC        SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5111"          TO  SYS-KANRICD
           MOVE    SNYUINRYO-SRYYMD TO SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY23-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5111-REC
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    1           TO  FLG-TARGET
      *
               IF    ( SYS-5111-BRM-NYUINRYOKBN    =   "12" )
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
                   MOVE    "3"             TO  WRK-TOKNYUIN-KBN
                   MOVE    SYS-5111-KBNCD  TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY23-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       20011-RYOYOIPN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       2002-NYUINRYO-EDIT-SEC              SECTION.
      *
           MOVE    WRK-TOKNYUIN-KBN    TO  WRK-TOKTEMP-KBN
           MOVE    WRK-TOKNYUIN-CD     TO  WRK-TOKTEMP-CD
      *
           IF    ( SYS-5001-BTU-SBT  =   "04" OR "09" )
      *
                PERFORM VARYING IDX-LIFE    FROM    1   BY  1
                        UNTIL ( IDX-LIFE    >   CONST-TOK-LIFECD-MAX )
      *
                   IF     ( FLG-LIFE-RJN        =   1    OR 2 )
                       IF    ( CONST-TOK-LIFECD-IPN (IDX-LIFE)
                                                =   WRK-TOKTEMP-G )
                           MOVE    CONST-TOK-LIFECD-LIFE (IDX-LIFE)
                                               TO   WRK-TOKTEMP-G
                           MOVE    CONST-TOK-LIFECD-MAX
                                               TO  IDX-LIFE
                       END-IF
                   ELSE
                       IF    ( CONST-TOK-LIFECD-LIFE (IDX-LIFE)
                                                =   WRK-TOKTEMP-G )
                           MOVE    CONST-TOK-LIFECD-IPN (IDX-LIFE)
                                               TO   WRK-TOKTEMP-G
                           MOVE    CONST-TOK-LIFECD-MAX
                                               TO  IDX-LIFE
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           IF    ( SYS-5001-BTU-SBT    =   "04" OR "09" )
            AND  ( FLG-LIFE-RJN        =   2 )
      *
               IF    ( WRK-TOKNYUIN-CD NOT =  WRK-TOKTEMP-CD )
                   PERFORM 20021-NYUINRYO-EDIT-SEC
               END-IF
           END-IF
      *
           MOVE    WRK-TOKTEMP-CD      TO  WRK-TOKNYUIN-CD
           PERFORM 20021-NYUINRYO-EDIT-SEC
      *
           .
       2002-NYUINRYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       20021-NYUINRYO-EDIT-SEC         SECTION.
      *
           INITIALIZE                      WRK-NYUINRYO-AREA
      *
           MOVE    WRK-TOKNYUIN-KBN    TO  WRK-NYUINRYO-KBN
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           IF    ( WRK-TOKNYUIN-KBN    =   "2" )
               MOVE    "5110"          TO  SYS-KANRICD
           ELSE
               MOVE    "5111"          TO  SYS-KANRICD
           END-IF
           MOVE    WRK-TOKNYUIN-CD     TO  SYS-KBNCD
           MOVE    SNYUINRYO-SRYYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               IF    ( WRK-TOKNYUIN-KBN    =   "3" )
                   MOVE    SYSKANRI-REC    TO  SYS-5111-REC
                   IF    ( SYS-5111-KBNCD  =   "23" OR "49" OR "51" )
                       MOVE    SYS-5111-SRYCD
                                           TO  WRK-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       MOVE    TNS-NAME    TO  WRK-NYUINRYO-NAME
                   ELSE
                       MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                           TO  WRK-NYUINRYO-NAME
                   END-IF
                   MOVE    SYS-5111-KBNCD  TO  WRK-NYUINRYO-CD
                   MOVE    SYS-5111-SRYCD  TO  WRK-NYUINRYO-SRYCD-X
                   MOVE    SYS-5111-BRM-NYUINRYOKBN
                                           TO  WRK-NYUINRYO-TYPE
               ELSE
                   MOVE    SYSKANRI-REC    TO  SYS-5110-REC
                   MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                                           TO  WRK-NYUINRYO-NAME
                   MOVE    SYS-5110-KBNCD
                                           TO  WRK-NYUINRYO-CD
                   MOVE    SYS-5110-SRYCD  TO  WRK-NYUINRYO-SRYCD-X
                   MOVE    SPACE           TO  WRK-NYUINRYO-TYPE
               END-IF
               PERFORM 200211-NYUINRYO-EDIT-SEC
           END-IF
      *
           .
       20021-NYUINRYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       200211-NYUINRYO-EDIT-SEC        SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    ZERO                TO  FLG-SKIP
      *
           IF    ( WRK-NYUINRYO-NAME   =   SPACE )
               MOVE    1               TO  FLG-SKIP
           END-IF
      *
           IF    ( WRK-NYUINRYO-KBN    =   "4" )
               CONTINUE
           ELSE
               MOVE    WRK-NYUINRYO-SRYCD-X
                                       TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   EVALUATE    TNS-HOSPSRYKBN
                   WHEN    3
                       MOVE    0           TO  WRK-HOSPSBT
                   WHEN    4
                       MOVE    1           TO  WRK-HOSPSBT
                   END-EVALUATE
                   IF    ( WRK-HOSPSBT     =   0 )
                    OR   ( WRK-HOSPSBT     =   SPA-HOSPSBT )
                       CONTINUE
                   ELSE
                       MOVE    1       TO  FLG-SKIP
                   END-IF
               ELSE
                   MOVE    1           TO  FLG-SKIP
               END-IF
           END-IF
      *
           PERFORM VARYING IDX17   FROM    1   BY  1
                   UNTIL ( IDX17   >   SNYUINRYO-MAX )
                    OR   ( FLG-SKIP    =   1 )
      *
               IF    ( SNYUINRYO-NYUINRYO-G (IDX17)
                                       =   WRK-NYUINRYO-G )
                   MOVE    1           TO  FLG-SKIP
               END-IF
      *
           END-PERFORM
      *
           IF    ( SNYUINRYO-MAX      >=   CONST-TOKNYUIN-MAX )
            OR   ( FLG-SKIP                    =   1 )
               CONTINUE
           ELSE
               COMPUTE SNYUINRYO-MAX
                   =   SNYUINRYO-MAX   +   1
               MOVE    SNYUINRYO-MAX   TO  IDX10
               MOVE    IDX10           TO  WRK-N02
               MOVE    WRK-N02-G       TO  SNYUINRYO-NO   (IDX10)
               MOVE    WRK-NYUINRYO-NAME
                                       TO  SNYUINRYO-NAME (IDX10)
               MOVE    WRK-NYUINRYO-G  TO  SNYUINRYO-NYUINRYO-G (IDX10)
               MOVE    WRK-NYUINRYO-SRYCD-X
                                       TO  SNYUINRYO-SRYCD(IDX10)
               MOVE    WRK-NYUINRYO-TYPE
                                       TO  SNYUINRYO-TYPE (IDX10)
      *
               INITIALIZE                    ORCSTOKNYUIN-AREA
               MOVE    WRK-NYUINRYO-KBN
                                       TO  LNK-TOKNYUIN-KBN
               MOVE    WRK-NYUINRYO-CD TO  LNK-TOKNYUIN-NYUIN
               MOVE    SNYUINRYO-BTUNUM
                                       TO  LNK-TOKNYUIN-BTUNUM
               MOVE    "1"             TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
               MOVE    SNYUINRYO-SRYYMD
                                       TO  LNK-TOKNYUIN-SANTEIYM
               CALL    "ORCSTOKNYUIN"  USING
                                       ORCSTOKNYUIN-AREA
                                       SPA-AREA
               IF    ( LNK-TOKNYUIN-RC          =   ZERO )
                AND  ( LNK-TOKNYUIN-GAI-SRYCD   NOT =  SPACE )
                   MOVE    LNK-TOKNYUIN-GAI-SRYCD
                                       TO  SNYUINRYO-NASRYCD(IDX10)
               END-IF
      *
               IF    ( WRK-NYUINRYO-SRYCD-X    IS  NUMERIC )
                   COMPUTE SNYUINRYO-SRYCDTTL
                       =   SNYUINRYO-SRYCDTTL
                       +   WRK-NYUINRYO-SRYCD
               END-IF
      *
               IF    ( TNS-CDKBN-KBN       =   "A" )
                 AND ( TNS-CDKBN-KBNNUM   >=   100 )
                 AND ( TNS-CDKBN-KBNNUM   <=   199 )
                   MOVE    1               TO  SNYUINRYO-SELECT
               END-IF
      *
               IF    ( WRK-NYUINRYO-KBN    =   "4" )
                   MOVE    1               TO  SNYUINRYO-SELECT
               END-IF
      *
           END-IF
      *
           .
       200211-NYUINRYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    短期滞在手術等基本料３編集処理
      *****************************************************************
       2002-TANKI3-EDIT-SEC        SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5003"          TO  SYS-KANRICD
           MOVE    SNYUINRYO-SRYYMD TO SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5003-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               IF    ( SYS-5003-KBNCD  NOT =   "*" )
                   MOVE    "3"     TO  WRK-TOKNYUIN-KBN
                   MOVE    SYS-5003-TOKCD
                                   TO  WRK-TOKNYUIN-CD
                   PERFORM 2002-NYUINRYO-EDIT-SEC
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5003-REC
      *
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2002-TANKI3-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    医療観察法入院料編集処理
      *****************************************************************
       2001-KANSATU-EDIT-SEC       SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5112"          TO  SYS-KANRICD
           MOVE    SNYUINRYO-SRYYMD TO SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY15-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5112-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               INITIALIZE                      WRK-NYUINRYO-AREA
               MOVE    SYS-5112-KANSATUMEI TO  WRK-NYUINRYO-NAME
               MOVE    "4"                 TO  WRK-NYUINRYO-KBN
               MOVE    SYS-5112-KBNCD      TO  WRK-NYUINRYO-CD
               MOVE    ZERO                TO  WRK-NYUINRYO-SRYCD-X
               MOVE    SYS-5112-KBNCD(1:2) TO  WRK-NYUINRYO-SRYCD-X(8:2)
               MOVE    SPACE               TO  WRK-NYUINRYO-TYPE
               PERFORM 200211-NYUINRYO-EDIT-SEC
      *
               PERFORM 900-SYSKANRI-KEY15-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5112-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-KANSATU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
      *    患者マスタクローズ
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SNYUINRYO-HKNCOMBI  TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者労災情報検索処理
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-PTID            TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
     *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報検索処理(KEY11)
      *****************************************************************
       900-PTHKNINF-KEY11-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTHKNINF
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTHKNINF-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索処理
      *****************************************************************
       900-PTKOHINF-KEY6-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKOHINF
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTKOHINF-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY15-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY15-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY15-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY15-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY23-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY23-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    SNYUINRYO-SRYYMD    TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
