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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBGS137.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 新保険別請求チェック表 社保
      *  管理者            :
      *  作成日付    作業者        記述
      *  11/01/13    NACL-太田     新規作成
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
      *    請求ファイル
           SELECT  BGSSKY-FILE     ASSIGN  ASS-BGSSKY
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSSKY-KEY
                                   FILE    STATUS  IS  STS-BGSSKY.
      *
      *    公費ファイル
           SELECT  BGSKOH-FILE     ASSIGN  ASS-BGSKOH
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSKOH-KEY
                                   FILE    STATUS  IS  STS-BGSKOH.
      *
      *    その他請求ファイル
           SELECT  BGSETC-FILE     ASSIGN  ASS-BGSETC
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSETC-KEY
                                   FILE    STATUS  IS  STS-BGSETC.
      *
      *    エラーファイル
           SELECT  BGS100ERR-FILE  ASSIGN  ASS-BGS100ERR
                                   FILE    STATUS  IS  STS-BGS100ERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求ファイル
       FD  BGSSKY-FILE.
       01  BGSSKY-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSSKY-//.
      *
      *    公費ファイル
       FD  BGSKOH-FILE.
       01  BGSKOH-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSKOH-//.
      *    その他請求ファイル
       FD  BGSETC-FILE.
       01  BGSETC-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSETC-//.
      *
      *    エラーファイル
       FD  BGS100ERR-FILE.
       01  BGS100ERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY  "HCGS137N1.INC".
           COPY  "HCGS137N2.INC".
           COPY  "HCGS137G.INC".
      *
       01  ASS-AREA.
           03  ASS-BGSSKY          PIC X(255).
           03  ASS-BGSKOH          PIC X(255).
           03  ASS-BGSETC          PIC X(255).
           03  ASS-BGS100ERR       PIC X(255).
      *
       01  STS-AREA.
           03  STS-BGSSKY          PIC X(02).
           03  STS-BGSKOH          PIC X(02).
           03  STS-BGSETC          PIC X(02).
           03  STS-BGS100ERR       PIC X(02).
      *
       01  CNT-AREA.
           03  CNT-NYUIN           PIC 9(05).
           03  CNT-GAIRAI          PIC 9(05).
           03  CNT-PAGE            PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-KENSU           PIC 9(05).
      *
       01  FLG-AREA.
           03  FLG-BGSSKY          PIC 9(01).
           03  FLG-BGSKOH          PIC 9(01).
           03  FLG-BGSETC          PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX-HKN             PIC 9(05).
           03  IDX-TTL             PIC 9(05).
           03  IDX-KOH             PIC 9(05).
           03  IDX-BASE            PIC 9(05).
           03  IDXP                PIC 9(05).
           03  IDX-RECEKA          PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-EDTYMD1         PIC X(10).
           03  WRK-EDTYMD3         PIC X(22).
           03  WRK-ERRMSG          PIC X(200).
           03  WRK-Z3              PIC Z(3).
           03  WRK-Z4              PIC Z(4).
           03  WRK-Z5              PIC Z(5).
           03  WRK-Z9              PIC Z(9).
           03  WRK-M9              PIC -(9).
           03  WRK-HKNNUM          PIC X(3).
           03  WRK-TTLID           PIC 9(5).
           03  WRK-PRT-SRYYM       PIC X(16).
           03  WRK-PRT-SAKYMD      PIC X(22).
           03  WRK-KANACHK-STR     PIC X(500).
           03  WRK-SRYKANAME1      PIC X(24).
      *
       01  KEY-AREA.
           03  KEY-RECEKA          PIC X(02).
      *
       01  THKN-AREA.
           03  THKN-NYUGAI-OCC     OCCURS  2.
               05  THKN-BODY-OCC   OCCURS  100.
                   07  THKN-KENSU          PIC 9(06).
                   07  THKN-NISSU          PIC 9(08).
                   07  THKN-TENSU          PIC 9(08).
                   07  THKN-FTNMONEY       PIC 9(09).
                   07  THKN-HKNMONEY       PIC 9(09).
                   07  THKN-KOHMONEY       PIC 9(09).
                   07  THKN-TTLMONEY       PIC 9(09).
                   07  THKN-SKJKAISU       PIC 9(06).
                   07  THKN-SKJTTLMONEY    PIC 9(09).
                   07  THKN-SKJFTNMONEY    PIC 9(09).
                   07  THKN-SKJHKNMONEY    PIC 9(09).
                   07  THKN-SKJKOHMONEY    PIC 9(09).
                   07  THKN-TTLTTLMONEY    PIC 9(09).
      *
       01  TKOH-AREA.
           03  TKOH-NYUGAI-OCC     OCCURS  2.
               05  TKOH-CNT                PIC 9(05).
               05  TKOH-MAX                PIC 9(05).
               05  TKOH-KOHMSG             PIC 9(01).
               05  TKOH-BODY-OCC   OCCURS  30.
                   07  TKOH-KOHNUM         PIC X(03).
                   07  TKOH-KOHKBN         PIC X(01).
                   07  TKOH-KENSU          PIC 9(06).
                   07  TKOH-NISSU          PIC 9(08).
                   07  TKOH-TENSU          PIC 9(08).
      *
       01  TETC-AREA.
           03  TETC-NYUGAI-OCC     OCCURS  2.
               05  TETC-JIHIMONEY          PIC S9(08).
               05  TETC-GENMEN             PIC S9(08).
               05  TETC-CHOSEI1            PIC S9(08).
               05  TETC-CHOSEI2            PIC S9(08).
               05  TETC-HASU               PIC S9(08).
      *
       01  CONST-AREA.
           03  CONST-SYAHO         PIC 9(01) VALUE 1.
           03  CONST-KOU9          PIC 9(03) VALUE 1.
           03  CONST-KOU7          PIC 9(03) VALUE 10.
           03  CONST-HON           PIC 9(03) VALUE 19.
           03  CONST-KZK           PIC 9(03) VALUE 29.
           03  CONST-MISGK         PIC 9(03) VALUE 38.
           03  CONST-HKNTTL        PIC 9(03) VALUE 47.
           03  CONST-KOHTAN        PIC 9(03) VALUE 48.
           03  CONST-KOH2HEI       PIC 9(03) VALUE 49.
           03  CONST-KOH3HEI       PIC 9(03) VALUE 50.
           03  CONST-KOH4HEI       PIC 9(03) VALUE 51.
           03  CONST-KOHTTL        PIC 9(03) VALUE 52.
           03  CONST-HKNKOHTTL     PIC 9(03) VALUE 53.
           03  CONST-TKOH-MAX      PIC 9(03) VALUE 30.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  BGS100P-AREA.
           COPY    "CPORCSPRTLNK.INC".
           COPY    "CPBGS100PARA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE               DIVISION
                                       USING
                                       BGS100P-AREA
                                       SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      STS-AREA
                                           CNT-AREA
                                           FLG-AREA
                                           IDX-AREA
                                           WRK-AREA
                                           THKN-AREA
                                           TKOH-AREA
                                           TETC-AREA
      *
           MOVE    BGS100P-SKYFNAME    TO  ASS-BGSSKY
           MOVE    BGS100P-KOHFNAME    TO  ASS-BGSKOH
           MOVE    BGS100P-ETCFNAME    TO  ASS-BGSETC
           MOVE    BGS100P-ERRFNAME    TO  ASS-BGS100ERR
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "社保分作成中"
                                       TO  JOB-YOBI
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-HIZUKE-EDIT-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-PRT-SAKYMD
      *
           MOVE    BGS100P-SRYYMD      TO  WRK-SYMD
           PERFORM 800-HIZUKE-EDIT-SEC
           MOVE    WRK-EDTYMD3(1:16)   TO  WRK-PRT-SRYYM
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
           IF    ( BGS100P-SYUKA-SYA   =   1 )
               PERFORM VARYING IDX-RECEKA  FROM    1   BY  1
                       UNTIL ( IDX-RECEKA  >   BGS100P-RECEKA-MAX )
      *
                   MOVE    BGS100P-RECEKA (IDX-RECEKA)
                                   TO  KEY-RECEKA
      *
                   PERFORM 2001-MAIN-SEC
               END-PERFORM
               MOVE    SPACE       TO  KEY-RECEKA
               PERFORM 2001-MAIN-SEC
           ELSE
               MOVE    SPACE       TO  KEY-RECEKA
               PERFORM 2001-MAIN-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       2001-MAIN-SEC               SECTION.
      *
           MOVE    ZERO            TO      CNT-NYUIN
                                           CNT-GAIRAI
      *
           INITIALIZE                      THKN-AREA
                                           TKOH-AREA
                                           TETC-AREA
      *
           MOVE    SPACE           TO      WRK-SRYKANAME1
           IF    ( BGS100P-SYUKA-SYA   =   1 )
               EVALUATE    KEY-RECEKA
               WHEN    SPACE
                   MOVE    "全科合計"  TO  WRK-SRYKANAME1
               WHEN    "00"
                   MOVE    "主科設定なし"
                                       TO  WRK-SRYKANAME1
               WHEN    OTHER
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    KEY-RECEKA      TO  SYS-KBNCD
                   MOVE    BGS100P-SRYYMD  TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME1
                                       TO  WRK-SRYKANAME1
               END-EVALUATE
           END-IF
      *
           PERFORM 2001-SKY-EDIT-SEC
      *
           PERFORM 2001-KOH-EDIT-SEC
      *
           PERFORM 2001-ETC-EDIT-SEC
      *
           IF    ( CNT-NYUIN   >   ZERO )
               MOVE    1       TO  IDX1
               PERFORM 2001-HCGS137N1-EDIT-SEC
               PERFORM 2001-HCGS137N2-EDIT-SEC
           END-IF
      *
           IF    ( CNT-GAIRAI  >   ZERO )
               MOVE    2       TO  IDX1
               PERFORM 2001-HCGS137G-EDIT-SEC
           END-IF
      *
           .
       2001-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    請求情報編集処理
      *****************************************************************
       2001-SKY-EDIT-SEC           SECTION.
      *
            OPEN    INPUT  BGSSKY-FILE
      *
            PERFORM 900-BGSSKY-READ-SEC
      *
            PERFORM UNTIL ( FLG-BGSSKY NOT =   ZERO )
      *
                PERFORM 20011-SKY-EDIT-SEC
      *
                PERFORM 900-BGSSKY-READ-SEC
      *
            END-PERFORM
      *
            CLOSE   BGSSKY-FILE
      *
           .
       2001-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求情報編集処理
      *****************************************************************
       20011-SKY-EDIT-SEC           SECTION.
      *
           IF    ( BGSSKY-NYUGAIKBN =   "1" )
               MOVE    1           TO   IDX1
           ELSE
               MOVE    2           TO   IDX1
           END-IF
      *
           EVALUATE    BGSSKY-RECESBT
           WHEN    "1117"
           WHEN    "1118"
           WHEN    "1127"
           WHEN    "1128"
           WHEN    "1137"
           WHEN    "1138"
           WHEN    "1147"
           WHEN    "1148"
           WHEN    "1157"
           WHEN    "1158"
               MOVE    CONST-KOU9  TO  IDX-BASE
               PERFORM 200111-SKY-SUM-SEC
           WHEN    "1119"
           WHEN    "1110"
           WHEN    "1129"
           WHEN    "1120"
           WHEN    "1139"
           WHEN    "1130"
           WHEN    "1149"
           WHEN    "1140"
           WHEN    "1159"
           WHEN    "1150"
               MOVE    CONST-KOU7  TO  IDX-BASE
               PERFORM 200111-SKY-SUM-SEC
           WHEN    "1111"
           WHEN    "1112"
           WHEN    "1121"
           WHEN    "1122"
           WHEN    "1131"
           WHEN    "1132"
           WHEN    "1141"
           WHEN    "1142"
           WHEN    "1151"
           WHEN    "1152"
               MOVE    CONST-HON   TO  IDX-BASE
               PERFORM 200111-SKY-SUM-SEC
           WHEN    "1115"
           WHEN    "1116"
           WHEN    "1125"
           WHEN    "1126"
           WHEN    "1135"
           WHEN    "1136"
           WHEN    "1145"
           WHEN    "1146"
           WHEN    "1155"
           WHEN    "1156"
               MOVE    CONST-KZK   TO  IDX-BASE
               PERFORM 200111-SKY-SUM-SEC
           WHEN    "1113"
           WHEN    "1114"
           WHEN    "1123"
           WHEN    "1124"
           WHEN    "1133"
           WHEN    "1134"
           WHEN    "1143"
           WHEN    "1144"
           WHEN    "1153"
           WHEN    "1154"
               MOVE    CONST-MISGK TO  IDX-BASE
               PERFORM 200111-SKY-SUM-SEC
           WHEN    "1211"
           WHEN    "1212"
               MOVE    CONST-KOHTAN    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
               MOVE    CONST-KOHTTL    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1221"
           WHEN    "1222"
               MOVE    CONST-KOH2HEI   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
               MOVE    CONST-KOHTTL    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1231"
           WHEN    "1232"
           WHEN    "1241"
           WHEN    "1242"
               MOVE    CONST-KOH3HEI   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
               MOVE    CONST-KOHTTL    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    OTHER
               DISPLAY "RECESBT[" BGSSKY-RECESBT "]" BGSSKY-REC
           END-EVALUATE
      *
           MOVE    CONST-HKNKOHTTL     TO  IDX2
           PERFORM 2001111-SKY-SUM-SEC
      *
           .
       20011-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求情報集計処理
      *****************************************************************
       200111-SKY-SUM-SEC          SECTION.
      *
           IF    ( BGSSKY-RECESBT(1:3) =   "111" )
      *
               PERFORM 200111-HKNIDX-CHK-SEC
      *
               IF    ( IDX-HKN     >   ZERO )
                   COMPUTE IDX2
                       =   IDX-BASE    +   IDX-HKN
                   PERFORM 2001111-SKY-SUM-SEC
               END-IF
      *
               IF    ( IDX-TTL     >   ZERO )
                   COMPUTE IDX2
                       =   IDX-BASE    +   IDX-TTL
                   PERFORM 2001111-SKY-SUM-SEC
               END-IF
               MOVE    CONST-HKNTTL TO IDX2
               PERFORM 2001111-SKY-SUM-SEC
           ELSE
      *
               MOVE    IDX-BASE    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
               MOVE    CONST-HKNTTL TO IDX2
               PERFORM 2001111-SKY-SUM-SEC
           END-IF
      *
           .
       200111-SKY-SUM-EXT.
           EXIT.
      *****************************************************************
      *    請求情報集計処理
      *****************************************************************
       2001111-SKY-SUM-SEC         SECTION.
      *
           COMPUTE THKN-KENSU (IDX1 IDX2)
               =   THKN-KENSU (IDX1 IDX2)    +   BGSSKY-KENSU
           COMPUTE THKN-NISSU (IDX1 IDX2)
               =   THKN-NISSU (IDX1 IDX2)    +   BGSSKY-NISSU
           COMPUTE THKN-TENSU (IDX1 IDX2)
               =   THKN-TENSU (IDX1 IDX2)        + BGSSKY-TENSU
           COMPUTE THKN-FTNMONEY (IDX1 IDX2)
               =   THKN-FTNMONEY (IDX1 IDX2)     + BGSSKY-FTNMONEY
           COMPUTE THKN-HKNMONEY (IDX1 IDX2)
               =   THKN-HKNMONEY (IDX1 IDX2)     + BGSSKY-HKNMONEY
           COMPUTE THKN-KOHMONEY (IDX1 IDX2)
               =   THKN-KOHMONEY (IDX1 IDX2)     + BGSSKY-KOHMONEY
           COMPUTE THKN-TTLMONEY (IDX1 IDX2)
               =   THKN-TTLMONEY (IDX1 IDX2)     + BGSSKY-TTLMONEY
      *
           COMPUTE THKN-SKJKAISU (IDX1 IDX2)
               =   THKN-SKJKAISU (IDX1 IDX2)     + BGSSKY-SKJKAISU
           COMPUTE THKN-SKJTTLMONEY (IDX1 IDX2)
               =   THKN-SKJTTLMONEY (IDX1 IDX2)  + BGSSKY-SKJTTLMONEY
           COMPUTE THKN-SKJFTNMONEY (IDX1 IDX2)
               =   THKN-SKJFTNMONEY (IDX1 IDX2)  + BGSSKY-SKJFTNMONEY
           COMPUTE THKN-SKJHKNMONEY (IDX1 IDX2)
               =   THKN-SKJHKNMONEY (IDX1 IDX2)  + BGSSKY-SKJHKNMONEY
           COMPUTE THKN-SKJKOHMONEY (IDX1 IDX2)
               =   THKN-SKJKOHMONEY (IDX1 IDX2)  + BGSSKY-SKJKOHMONEY
           COMPUTE THKN-TTLTTLMONEY (IDX1 IDX2)
               =   THKN-TTLTTLMONEY (IDX1 IDX2)  + BGSSKY-TTLTTLMONEY
      *
           IF    ( IDX1    =   1 )
               COMPUTE CNT-NYUIN   =   CNT-NYUIN   +   1
           ELSE
               COMPUTE CNT-GAIRAI  =   CNT-GAIRAI  +   1
           END-IF
      *
           .
       2001111-SKY-SUM-EXT.
           EXIT.
      *****************************************************************
      *    保険別集計先チェック処理
      *****************************************************************
       200111-HKNIDX-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-HKN
           MOVE    ZERO            TO  IDX-TTL
      *
           EVALUATE     BGSSKY-RECESBT
           WHEN    "1111"
           WHEN    "1112"
               MOVE    9           TO  IDX-TTL
               EVALUATE    BGSSKY-HKNNUM
               WHEN    "001"
               WHEN    "009"
                   MOVE    1       TO  IDX-HKN
               WHEN    "002"
                   MOVE    2       TO  IDX-HKN
               WHEN    "003"
                   MOVE    3       TO  IDX-HKN
               WHEN    "004"
                   MOVE    4       TO  IDX-HKN
               WHEN    "031"   THRU    "034"
                   MOVE    5       TO  IDX-HKN
               WHEN    "006"
                   MOVE    6       TO  IDX-HKN
               WHEN    "007"
                   MOVE    7       TO  IDX-HKN
               WHEN    "063"
               WHEN    "072"   THRU    "075"
                   MOVE    8       TO  IDX-HKN
               END-EVALUATE
           WHEN    OTHER
               MOVE    8           TO  IDX-TTL
               EVALUATE    BGSSKY-HKNNUM
               WHEN    "001"
               WHEN    "009"
                   MOVE    1       TO  IDX-HKN
               WHEN    "002"
                   MOVE    2       TO  IDX-HKN
               WHEN    "003"
                   MOVE    3       TO  IDX-HKN
               WHEN    "004"
                   MOVE    4       TO  IDX-HKN
               WHEN    "031"   THRU    "034"
                   MOVE    5       TO  IDX-HKN
               WHEN    "006"
                   MOVE    6       TO  IDX-HKN
               WHEN    "063"
               WHEN    "072"   THRU    "075"
                   MOVE    7       TO  IDX-HKN
               END-EVALUATE
           END-EVALUATE
      *
           IF    ( IDX-HKN     =   ZERO )
               DISPLAY "HKNNUM[" BGSSKY-HKNNUM "]" BGSSKY-REC
           END-IF
      *
           .
       200111-HKNIDX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       2001-KOH-EDIT-SEC           SECTION.
      *
            OPEN    INPUT  BGSKOH-FILE
      *
            PERFORM 900-BGSKOH-READ-SEC
      *
            PERFORM UNTIL ( FLG-BGSKOH NOT =   ZERO )
      *
                PERFORM 20011-KOH-EDIT-SEC
      *
                PERFORM 900-BGSKOH-READ-SEC
      *
            END-PERFORM
      *
            CLOSE   BGSKOH-FILE
      *
           .
       2001-KOH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       20011-KOH-EDIT-SEC          SECTION.
      *
           IF    ( BGSKOH-NYUGAIKBN    =  "1" )
               MOVE    1           TO  IDX1
           ELSE
               MOVE    2           TO  IDX1
           END-IF
      *
           COMPUTE TKOH-CNT (IDX1)
              =    TKOH-CNT (IDX1)    +   1
      *
           IF    ( TKOH-CNT (IDX1)    >    CONST-TKOH-MAX )
               MOVE    CONST-TKOH-MAX  TO  IDX2
                                           TKOH-MAX (IDX1)
           ELSE
               MOVE    TKOH-CNT (IDX1) TO  IDX2
                                           TKOH-MAX (IDX1)
               MOVE    BGSKOH-KOHNUM   TO  TKOH-KOHNUM (IDX1 IDX2)
               MOVE    BGSKOH-KOHKBN   TO  TKOH-KOHKBN (IDX1 IDX2)
               IF    ( BGSKOH-KOHKBN   =   "2" )
                   MOVE    1           TO  TKOH-KOHMSG (IDX1)
               END-IF
           END-IF
      *
           COMPUTE TKOH-KENSU (IDX1 IDX2)
               =   TKOH-KENSU (IDX1 IDX2)  +   BGSKOH-KENSU
      *
           COMPUTE TKOH-NISSU (IDX1 IDX2)
               =   TKOH-NISSU (IDX1 IDX2)  +   BGSKOH-NISSU
      *
           COMPUTE TKOH-TENSU (IDX1 IDX2)
               =   TKOH-TENSU (IDX1 IDX2)  +   BGSKOH-TENSU
      *
           .
       20011-KOH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求情報編集処理
      *****************************************************************
       2001-ETC-EDIT-SEC           SECTION.
      *
            OPEN    INPUT  BGSETC-FILE
      *
            PERFORM 900-BGSETC-READ-SEC
      *
            PERFORM UNTIL ( FLG-BGSETC NOT =   ZERO )
      *
               IF    ( BGSETC-NYUGAIKBN    =  "1" )
                   MOVE    1           TO  IDX1
               ELSE
                   MOVE    2           TO  IDX1
               END-IF
      *
               MOVE    BGSETC-JIHIMONEY    TO  TETC-JIHIMONEY (IDX1)
               MOVE    BGSETC-GENMEN       TO  TETC-GENMEN    (IDX1)
               MOVE    BGSETC-CHOSEI1      TO  TETC-CHOSEI1   (IDX1)
               MOVE    BGSETC-CHOSEI2      TO  TETC-CHOSEI2   (IDX1)
               MOVE    BGSETC-HASU         TO  TETC-HASU      (IDX1)
      *
               PERFORM 900-BGSETC-READ-SEC
      *
            END-PERFORM
      *
            CLOSE   BGSETC-FILE
      *
           .
       2001-ETC-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院チェック表編集処理（１枚目）
      *****************************************************************
       2001-HCGS137N1-EDIT-SEC         SECTION.
      *
           INITIALIZE                      HCGS137N1
      *
           MOVE    WRK-PRT-SRYYM       TO  HCGS137N1-SRYYM
           MOVE    "入院"              TO  HCGS137N1-NYUGAI
           MOVE    WRK-PRT-SAKYMD      TO  HCGS137N1-SYSYMD
           MOVE    WRK-SRYKANAME1      TO  HCGS137N1-SRYKANAME
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
           MOVE    CNT-PAGE            TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCGS137N1-PAGE
      *
           MOVE    ZERO                TO  CNT-LINE
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CONST-HKNTTL )
      *
               COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
               MOVE    THKN-KENSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137N1-KENSU (CNT-LINE)
      *
               MOVE    THKN-NISSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137N1-NISSU (CNT-LINE)
      *
               MOVE    THKN-TENSU (IDX1 IDX2)     TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N1-TENSU (CNT-LINE)
      *
               MOVE    THKN-FTNMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N1-FTNMONEY (CNT-LINE)
      *
               MOVE    THKN-HKNMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N1-HKNMONEY (CNT-LINE)
      *
               MOVE    THKN-KOHMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N1-KOHMONEY (CNT-LINE)
      *
               MOVE    THKN-TTLMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N1-TTLMONEY (CNT-LINE)
      *
               MOVE    THKN-SKJKAISU (IDX1 IDX2)  TO  WRK-Z5
               MOVE    WRK-Z5  TO  HCGS137N1-SKJKAISU (CNT-LINE)
      *
               MOVE    THKN-SKJTTLMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N1-SKJTTLMONEY (CNT-LINE)
      *
               MOVE    THKN-SKJFTNMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N1-SKJFTNMONEY (CNT-LINE)
      *
               MOVE    THKN-SKJHKNMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N1-SKJHKNMONEY (CNT-LINE)
      *
               MOVE    THKN-SKJKOHMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N1-SKJKOHMONEY (CNT-LINE)
      *
               MOVE    THKN-TTLTTLMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N1-TTLTTLMONEY (CNT-LINE)
      *
           END-PERFORM
      *
           PERFORM 230-HCGS137N1-PRINT-SEC
      *
           .
       2001-HCGS137N1-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-HCGS137N1-PRINT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
      *
           MOVE    "HCGS137N1.red" TO  SPRT-PRTID
           MOVE    HCGS137N1       TO  SPRT-PRTDATA
      *
           PERFORM 230-PRINT-SEC
      *
           .
       230-HCGS137N1-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    入院チェック表編集処理（２枚目）
      *****************************************************************
       2001-HCGS137N2-EDIT-SEC         SECTION.
      *
           INITIALIZE                      HCGS137N2
      *
           MOVE    WRK-PRT-SRYYM       TO  HCGS137N2-SRYYM
           MOVE    "入院"              TO  HCGS137N2-NYUGAI
           MOVE    WRK-PRT-SAKYMD      TO  HCGS137N2-SYSYMD
           MOVE    WRK-SRYKANAME1      TO  HCGS137N2-SRYKANAME
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
           MOVE    CNT-PAGE            TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCGS137N2-PAGE
      *
           MOVE    ZERO                TO  CNT-LINE
           PERFORM VARYING IDX2    FROM    CONST-KOHTAN  BY  1
                   UNTIL ( IDX2    >       CONST-KOHTTL )
      *
               COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
               MOVE    THKN-KENSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137N2-KENSU (CNT-LINE)
      *
               MOVE    THKN-NISSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137N2-NISSU (CNT-LINE)
      *
               MOVE    THKN-TENSU (IDX1 IDX2)     TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-TENSU (CNT-LINE)
      *
               MOVE    THKN-FTNMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-FTNMONEY (CNT-LINE)
      *
               MOVE    THKN-HKNMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-HKNMONEY (CNT-LINE)
      *
               MOVE    THKN-KOHMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-KOHMONEY (CNT-LINE)
      *
               MOVE    THKN-TTLMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-TTLMONEY (CNT-LINE)
      *
               MOVE    THKN-SKJKAISU (IDX1 IDX2)  TO  WRK-Z5
               MOVE    WRK-Z5  TO  HCGS137N2-SKJKAISU (CNT-LINE)
      *
               MOVE    THKN-SKJTTLMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-SKJTTLMONEY (CNT-LINE)
      *
               MOVE    THKN-SKJFTNMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-SKJFTNMONEY (CNT-LINE)
      *
               MOVE    THKN-SKJHKNMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-SKJHKNMONEY (CNT-LINE)
      *
               MOVE    THKN-SKJKOHMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-SKJKOHMONEY (CNT-LINE)
      *
               MOVE    THKN-TTLTTLMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-TTLTTLMONEY (CNT-LINE)
      *
           END-PERFORM
      *
               MOVE    CONST-HKNKOHTTL         TO  IDX2
      *
               MOVE    THKN-KENSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137N2-GKENSU
      *
               MOVE    THKN-NISSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137N2-GNISSU
      *
               MOVE    THKN-TENSU (IDX1 IDX2)     TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-GTENSU
      *
               MOVE    THKN-FTNMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-GFTNMONEY
      *
               MOVE    THKN-HKNMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-GHKNMONEY
      *
               MOVE    THKN-KOHMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-GKOHMONEY
      *
               MOVE    THKN-TTLMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-GTTLMONEY
      *
               MOVE    THKN-SKJKAISU (IDX1 IDX2)  TO  WRK-Z5
               MOVE    WRK-Z5  TO  HCGS137N2-GSKJKAISU
      *
               MOVE    THKN-SKJTTLMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-GSKJTTLMONEY
      *
               MOVE    THKN-SKJFTNMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-GSKJFTNMONEY
      *
               MOVE    THKN-SKJHKNMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-GSKJHKNMONEY
      *
               MOVE    THKN-SKJKOHMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-GSKJKOHMONEY
      *
               MOVE    THKN-TTLTTLMONEY (IDX1 IDX2)   TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-GTTLTTLMONEY
      *
           MOVE    ZERO                TO  CNT-LINE
           MOVE    ZERO                TO  CNT-KENSU
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >       TKOH-MAX (IDX1) )
      *
               COMPUTE CNT-LINE  = CNT-LINE  +   1
      *
               COMPUTE CNT-KENSU = CNT-KENSU +   TKOH-KENSU (IDX1 IDX2)
      *
               MOVE    TKOH-KOHNUM (IDX1 IDX2)    TO  WRK-HKNNUM
               PERFORM 900-HKNNUM-KEY-SEL-SEC
               MOVE    SPACE           TO  WRK-KANACHK-STR
               MOVE    1               TO  IDXP
               IF    ( TKOH-KOHKBN (IDX1 IDX2)    =   "1" )
                   STRING  "　"            DELIMITED BY SIZE
                   INTO    WRK-KANACHK-STR
                   WITH    POINTER IDXP
                   END-STRING
               ELSE
                   STRING    "＊"          DELIMITED BY SIZE
                   INTO    WRK-KANACHK-STR
                   WITH    POINTER IDXP
                   END-STRING
               END-IF
               STRING  HKN-HBTNUM          DELIMITED BY SIZE
                       "（"                DELIMITED BY SIZE
                       HKN-TANSEIDONAME    DELIMITED BY SPACE
                       "）"                DELIMITED BY SIZE
               INTO    WRK-KANACHK-STR
               WITH    POINTER IDXP
               END-STRING
               IF    ( IDX2        =       CONST-TKOH-MAX )
                AND  ( TKOH-CNT(IDX1) >       CONST-TKOH-MAX )
                   STRING    "　他"        DELIMITED BY SIZE
                   INTO    WRK-KANACHK-STR
                   WITH    POINTER IDXP
                   END-STRING
               END-IF
               PERFORM 800-ZENKAKU-SEC
               MOVE    WRK-KANACHK-STR TO  HCGS137N2-KOHNAME (CNT-LINE)
      *
               MOVE    TKOH-KENSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137N2-KOHKENSU (CNT-LINE)
      *
               MOVE    TKOH-NISSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137N2-KOHNISSU (CNT-LINE)
      *
               MOVE    TKOH-TENSU (IDX1 IDX2)     TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137N2-KOHTENSU (CNT-LINE)
      *
           END-PERFORM
           MOVE    CNT-KENSU       TO  WRK-Z4
           MOVE    WRK-Z4          TO  HCGS137N2-GKOHKENSU
           IF    ( TKOH-KOHMSG (IDX1) =   1 )
               MOVE    "＊：レセプトで請求を行わない公費"
                                   TO  HCGS137N2-KOHMSG
           END-IF
      *
           IF    ( KEY-RECEKA      =   SPACE )
               MOVE    TETC-JIHIMONEY (IDX1)
                                   TO  WRK-M9
               MOVE    WRK-M9      TO  HCGS137N2-JIHIMONEY
               MOVE    TETC-GENMEN    (IDX1)
                                   TO  WRK-M9
               MOVE    WRK-M9      TO  HCGS137N2-GENMEN
               MOVE    TETC-CHOSEI1   (IDX1)
                                   TO  WRK-M9
               MOVE    WRK-M9      TO  HCGS137N2-CHOSEI1
               MOVE    TETC-CHOSEI2   (IDX1)
                                   TO  WRK-M9
               MOVE    WRK-M9      TO  HCGS137N2-CHOSEI2
               MOVE    TETC-HASU      (IDX1)
                                   TO  WRK-M9
               MOVE    WRK-M9      TO  HCGS137N2-HASU
           ELSE
               MOVE    SPACE   TO  HCGS137N2-ETCMSG
               STRING  "その他計は全科合計に編集されます。"
                                       DELIMITED   BY  SIZE
               INTO    HCGS137N2-ETCMSG
               END-STRING
           END-IF
      *
           PERFORM 230-HCGS137N2-PRINT-SEC
      *
           .
       2001-HCGS137N2-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-HCGS137N2-PRINT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
      *
           MOVE    "HCGS137N2.red" TO  SPRT-PRTID
           MOVE    HCGS137N2       TO  SPRT-PRTDATA
      *
           PERFORM 230-PRINT-SEC
      *
           .
       230-HCGS137N2-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    外来チェック表編集処理
      *****************************************************************
       2001-HCGS137G-EDIT-SEC         SECTION.
      *
           INITIALIZE                      HCGS137G
      *
           MOVE    WRK-PRT-SRYYM       TO  HCGS137G-SRYYM
           MOVE    "外来"              TO  HCGS137G-NYUGAI
           MOVE    WRK-PRT-SAKYMD      TO  HCGS137G-SYSYMD
           MOVE    WRK-SRYKANAME1      TO  HCGS137G-SRYKANAME
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
           MOVE    CNT-PAGE            TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCGS137G-PAGE
      *
           MOVE    ZERO                TO  CNT-LINE
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >       CONST-KOHTTL )
      *
               COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
               MOVE    THKN-KENSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137G-KENSU (CNT-LINE)
      *
               MOVE    THKN-NISSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137G-NISSU (CNT-LINE)
      *
               MOVE    THKN-TENSU (IDX1 IDX2)     TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-TENSU (CNT-LINE)
      *
               MOVE    THKN-FTNMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-FTNMONEY (CNT-LINE)
      *
               MOVE    THKN-HKNMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-HKNMONEY (CNT-LINE)
      *
               MOVE    THKN-KOHMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-KOHMONEY (CNT-LINE)
      *
               MOVE    THKN-TTLMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-TTLMONEY (CNT-LINE)
      *
      *
           END-PERFORM
      *
               MOVE    CONST-HKNKOHTTL         TO  IDX2
      *
               MOVE    THKN-KENSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137G-GKENSU
      *
               MOVE    THKN-NISSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137G-GNISSU
      *
               MOVE    THKN-TENSU (IDX1 IDX2)     TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-GTENSU
      *
               MOVE    THKN-FTNMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-GFTNMONEY
      *
               MOVE    THKN-HKNMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-GHKNMONEY
      *
               MOVE    THKN-KOHMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-GKOHMONEY
      *
               MOVE    THKN-TTLMONEY (IDX1 IDX2)  TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-GTTLMONEY
      *
           MOVE    ZERO                TO  CNT-LINE
           MOVE    ZERO                TO  CNT-KENSU
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >       TKOH-MAX (IDX1) )
      *
               COMPUTE CNT-LINE  = CNT-LINE  +   1
      *
               COMPUTE CNT-KENSU = CNT-KENSU +   TKOH-KENSU (IDX1 IDX2)
      *
               MOVE    TKOH-KOHNUM (IDX1 IDX2)    TO  WRK-HKNNUM
               PERFORM 900-HKNNUM-KEY-SEL-SEC
               MOVE    SPACE           TO  WRK-KANACHK-STR
               MOVE    1               TO  IDXP
               IF    ( TKOH-KOHKBN (IDX1 IDX2)    =   "1" )
                   STRING    "　"          DELIMITED BY SIZE
                   INTO    WRK-KANACHK-STR
                   WITH    POINTER IDXP
                   END-STRING
               ELSE
                   STRING    "＊"          DELIMITED BY SIZE
                   INTO    WRK-KANACHK-STR
                   WITH    POINTER IDXP
                   END-STRING
               END-IF
               STRING  HKN-HBTNUM          DELIMITED BY SIZE
                       "（"                DELIMITED BY SIZE
                       HKN-TANSEIDONAME    DELIMITED BY SPACE
                       "）"                DELIMITED BY SIZE
               INTO    WRK-KANACHK-STR
               WITH    POINTER IDXP
               END-STRING
               IF    ( IDX2        =       CONST-TKOH-MAX )
                AND  ( TKOH-CNT(IDX1) >       CONST-TKOH-MAX )
                   STRING    "　他"        DELIMITED BY SIZE
                   INTO    WRK-KANACHK-STR
                   WITH    POINTER IDXP
                   END-STRING
               END-IF
               PERFORM 800-ZENKAKU-SEC
               MOVE    WRK-KANACHK-STR TO  HCGS137G-KOHNAME (CNT-LINE)
      *
               MOVE    TKOH-KENSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137G-KOHKENSU (CNT-LINE)
      *
               MOVE    TKOH-NISSU (IDX1 IDX2)     TO  WRK-Z4
               MOVE    WRK-Z4  TO  HCGS137G-KOHNISSU (CNT-LINE)
      *
               MOVE    TKOH-TENSU (IDX1 IDX2)     TO  WRK-Z9
               MOVE    WRK-Z9  TO  HCGS137G-KOHTENSU (CNT-LINE)
      *
           END-PERFORM
           MOVE    CNT-KENSU       TO  WRK-Z4
           MOVE    WRK-Z4          TO  HCGS137G-GKOHKENSU
           IF    ( TKOH-KOHMSG (IDX1) =   1 )
               MOVE    "＊：レセプトで請求を行わない公費"
                                   TO  HCGS137G-KOHMSG
           END-IF
      *
           IF    ( KEY-RECEKA      =   SPACE )
               MOVE    TETC-JIHIMONEY (IDX1)
                                   TO  WRK-M9
               MOVE    WRK-M9      TO  HCGS137G-JIHIMONEY
               MOVE    TETC-GENMEN    (IDX1)
                                   TO  WRK-M9
               MOVE    WRK-M9      TO  HCGS137G-GENMEN
               MOVE    TETC-CHOSEI1   (IDX1)
                                   TO  WRK-M9
               MOVE    WRK-M9      TO  HCGS137G-CHOSEI1
               MOVE    TETC-CHOSEI2   (IDX1)
                                   TO  WRK-M9
               MOVE    WRK-M9      TO  HCGS137G-CHOSEI2
               MOVE    TETC-HASU      (IDX1)
                                   TO  WRK-M9
               MOVE    WRK-M9      TO  HCGS137G-HASU
           ELSE
               MOVE    SPACE   TO  HCGS137G-ETCMSG
               STRING  "その他計は全科合計に編集されます。"
                                       DELIMITED   BY  SIZE
               INTO    HCGS137G-ETCMSG
               END-STRING
           END-IF
      *
           PERFORM 230-HCGS137G-PRINT-SEC
      *
           .
       2001-HCGS137G-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-HCGS137G-PRINT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
      *
           MOVE    "HCGS137G.red" TO  SPRT-PRTID
           MOVE    HCGS137G       TO  SPRT-PRTDATA
      *
           PERFORM 230-PRINT-SEC
      *
           .
       230-HCGS137G-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-PRINT-SEC                   SECTION.
      *
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    BGS100P-SRYYM   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE    BGS100P-SHELLMSG TO SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN     =   ZERO )
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       230-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           COMPUTE BGS100P-PAGE
               =   BGS100P-PAGE    +   CNT-PAGE
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   BGS100ERR-FILE
           IF      STS-BGS100ERR     =   ZERO
               CLOSE   BGS100ERR-FILE
           ELSE
               OPEN    OUTPUT              BGS100ERR-FILE
      *
               MOVE    WRK-ERRMSG      TO  BGS100ERR-REC
               WRITE   BGS100ERR-REC
               CLOSE   BGS100ERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-ERRMSG      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  BGS100P-RC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-HIZUKE-EDIT-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
                                       WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"   USING
                                   STS-AREA-DAY
                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
               INSPECT WRK-EDTYMD3 REPLACING  ALL "  "  BY  "　"
           END-IF
      *
      *
           .
       800-HIZUKE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE   "2"              TO  KANACHK-SYORI
           MOVE  500               TO  KANACHK-MAX-CNT
           MOVE  WRK-KANACHK-STR   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    BGS100P-JOBID       TO  JOB-JOBID
           MOVE    BGS100P-SHELLID     TO  JOB-SHELLID
           MOVE    "ORCBGS100"         TO  JOB-PGID
           MOVE    BGS100P-SHELLMSG    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求ファイル読込処理
      *****************************************************************
       900-BGSSKY-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSSKY
                                       FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-BGSSKY  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    BGSSKY-FILE     NEXT
               AT  END
                   MOVE    1       TO  FLG-BGSSKY
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   IF    ( BGSSKY-HOKEN    NOT =   CONST-SYAHO )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( BGS100P-NYUGAIKBN   =   "1" OR "2" )
                       IF    ( BGS100P-NYUGAIKBN
                                           NOT =   BGSSKY-NYUGAIKBN )
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( KEY-RECEKA      NOT =   SPACE )
                       IF    ( KEY-RECEKA  NOT =   BGSSKY-RECEKA )
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
               END-READ
           END-PERFORM
      *
           .
      *
       900-BGSSKY-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費ファイル読込処理
      *****************************************************************
       900-BGSKOH-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSKOH
                                       FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-BGSKOH  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    BGSKOH-FILE     NEXT
               AT  END
                   MOVE    1       TO  FLG-BGSKOH
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   IF    ( BGSKOH-HOKEN    NOT =   CONST-SYAHO )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( BGS100P-NYUGAIKBN   =   "1" OR "2" )
                       IF    ( BGS100P-NYUGAIKBN
                                           NOT =   BGSKOH-NYUGAIKBN )
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( KEY-RECEKA      NOT =   SPACE )
                       IF    ( KEY-RECEKA  NOT =   BGSKOH-RECEKA )
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
               END-READ
           END-PERFORM
      *
           .
      *
       900-BGSKOH-READ-EXT.
           EXIT.
      *****************************************************************
      *    その他請求ファイル読込処理
      *****************************************************************
       900-BGSETC-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSETC
                                       FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-BGSETC  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    BGSETC-FILE     NEXT
               AT  END
                   MOVE    1       TO  FLG-BGSETC
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   IF    ( BGSETC-HOKEN    NOT =   CONST-SYAHO )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( BGS100P-NYUGAIKBN   =   "1" OR "2" )
                       IF    ( BGS100P-NYUGAIKBN
                                           NOT =   BGSETC-NYUGAIKBN )
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( KEY-RECEKA      NOT =   SPACE )
                       IF    ( KEY-RECEKA  NOT =   BGSETC-RECEKA )
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
               END-READ
           END-PERFORM
      *
           .
      *
       900-BGSETC-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
           INITIALIZE                      HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    BGS100P-SRYYMD      TO  HKN-TEKSTYMD
           MOVE    BGS100P-SRYYMD      TO  HKN-TEKEDYMD
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
               INITIALIZE                  HKNNUM-REC
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    ＤＢ検索処理（FHETCHを行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "ORCBGS137 RC [" BGS100P-RC "]"
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
