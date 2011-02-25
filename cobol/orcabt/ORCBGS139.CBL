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
       PROGRAM-ID.                 ORCBGS139.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 新保険別請求チェック表 総括
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
      *
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
           COPY  "HCGS139N.INC".
           COPY  "HCGS139G.INC".
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
           03  FLG-SYSKANRI        PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX-NG              PIC 9(05).
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
           03  THKN-NYUGAI-OCC     OCCURS  3.
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
                   07  THKN-JIHIMONEY      PIC S9(08).
                   07  THKN-GENMEN         PIC S9(08).
                   07  THKN-CHOSEI1        PIC S9(08).
                   07  THKN-CHOSEI2        PIC S9(08).
                   07  THKN-HASU           PIC S9(09).
      *
       01  TKOH-AREA.
           03  TKOH-NYUGAI-OCC             OCCURS  3.
               05  TKOH-KENSU              PIC 9(06).
      *
       01  KEY-AERA.
           03  KEY-NYUGAIKBN               PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-SYAHO         PIC 9(01) VALUE 1.
           03  CONST-KOKUHO        PIC 9(01) VALUE 2.
           03  CONST-KOUKI         PIC 9(01) VALUE 3.
           03  CONST-TOKURYO       PIC 9(01) VALUE 4.
           03  CONST-JIHIHKN       PIC 9(01) VALUE 5.
           03  CONST-CHIKEN        PIC 9(01) VALUE 6.
           03  CONST-ROUSAI        PIC 9(01) VALUE 7.
           03  CONST-JIBAI         PIC 9(01) VALUE 8.
           03  CONST-KOUGAI        PIC 9(01) VALUE 9.
           03  CONST-SYAKOU9HEI    PIC 9(03) VALUE 1.
           03  CONST-SYAKOU9TAN    PIC 9(03) VALUE 2.
           03  CONST-SYAKOU7HEI    PIC 9(03) VALUE 3.
           03  CONST-SYAKOU7TAN    PIC 9(03) VALUE 4.
           03  CONST-SYAHONHEI     PIC 9(03) VALUE 5.
           03  CONST-SYAHONTAN     PIC 9(03) VALUE 6.
           03  CONST-SYAKZKHEI     PIC 9(03) VALUE 7.
           03  CONST-SYAKZKTAN     PIC 9(03) VALUE 8.
           03  CONST-SYAMISGKHEI   PIC 9(03) VALUE 9.
           03  CONST-SYAMISGKTAN   PIC 9(03) VALUE 10.
           03  CONST-SYAKOH        PIC 9(03) VALUE 11.
           03  CONST-SYATTL        PIC 9(03) VALUE 12.
           03  CONST-KOKIPNKOU9    PIC 9(03) VALUE 13.
           03  CONST-KOKIPNKOU7    PIC 9(03) VALUE 14.
           03  CONST-KOKIPNHON     PIC 9(03) VALUE 15.
           03  CONST-KOKIPNMISGK   PIC 9(03) VALUE 16.
           03  CONST-KOKTAIHON     PIC 9(03) VALUE 17.
           03  CONST-KOKTAIKZK     PIC 9(03) VALUE 18.
           03  CONST-KOKTAIMISGK   PIC 9(03) VALUE 19.
           03  CONST-KOKTTL        PIC 9(03) VALUE 20.
           03  CONST-KOUKOU9       PIC 9(03) VALUE 21.
           03  CONST-KOUKOU7       PIC 9(03) VALUE 22.
           03  CONST-KOUTTL        PIC 9(03) VALUE 23.
           03  CONST-HKNTTL        PIC 9(03) VALUE 24.
           03  CONST-TOKTTL        PIC 9(03) VALUE 25.
           03  CONST-JIHITTL       PIC 9(03) VALUE 26.
           03  CONST-CHIKENTTL     PIC 9(03) VALUE 27.
           03  CONST-ROUSAITTL     PIC 9(03) VALUE 28.
           03  CONST-JIBAITTL      PIC 9(03) VALUE 29.
           03  CONST-KOUGAITTL     PIC 9(03) VALUE 30.
           03  CONST-ALLTTL        PIC 9(03) VALUE 31.
           03  CONST-THKN-MAX      PIC 9(03) VALUE 31.
      *
      *    集計領域
       01  TTLHKN-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //TTLHKN-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
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
      *
           MOVE    BGS100P-SKYFNAME    TO  ASS-BGSSKY
           MOVE    BGS100P-KOHFNAME    TO  ASS-BGSKOH
           MOVE    BGS100P-ETCFNAME    TO  ASS-BGSETC
           MOVE    BGS100P-ERRFNAME    TO  ASS-BGS100ERR
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "総括表作成中"
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
           IF    ( BGS100P-SYUKA   =   1 )
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
      *****************************************************************
      *    主　処理
      *****************************************************************
       2001-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO      CNT-NYUIN
                                           CNT-GAIRAI
      *
           INITIALIZE                      THKN-AREA
                                           TKOH-AREA
      *
           MOVE    SPACE           TO      WRK-SRYKANAME1
           IF    ( BGS100P-SYUKA   =   1 )
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
           PERFORM 2001-ETC-SKY-EDIT-SEC
      *
           PERFORM 2001-KOH-EDIT-SEC
      *
           IF    ( CNT-NYUIN   >   ZERO )
               MOVE    1       TO  IDX1
               PERFORM 2001-HCGS139N-EDIT-SEC
           END-IF
      *
           IF    ( CNT-GAIRAI  >   ZERO )
               MOVE    2       TO  IDX1
               PERFORM 2001-HCGS139G-EDIT-SEC
           END-IF
      *
           IF    ( CNT-NYUIN   >   ZERO )
            AND  ( CNT-GAIRAI  >   ZERO )
               MOVE    3       TO  IDX1
               PERFORM 2001-HCGS139N-EDIT-SEC
           END-IF
      *
           .
       2001-MAIN-EXT.
           EXIT.
      *
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
               PERFORM 200111-SKY-EDIT-SEC
           ELSE
               MOVE    2           TO   IDX1
               PERFORM 200111-SKY-EDIT-SEC
           END-IF
      *
           MOVE    3               TO   IDX1
           PERFORM 200111-SKY-EDIT-SEC
      *
           .
       20011-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求情報編集処理
      *****************************************************************
       200111-SKY-EDIT-SEC           SECTION.
      *
           EVALUATE    BGSSKY-HOKEN
           WHEN    CONST-SYAHO
               PERFORM 200111-SYAHO-SKY-EDIT-SEC
           WHEN    CONST-KOKUHO
               PERFORM 200111-KOKUHO-SKY-EDIT-SEC
           WHEN    CONST-KOUKI
               PERFORM 200111-KOUKI-SKY-EDIT-SEC
           WHEN    OTHER
               DISPLAY "HOKEN[" BGSSKY-HOKEN "]" BGSSKY-REC
           END-EVALUATE
      *
           MOVE    CONST-HKNTTL    TO   IDX2
           PERFORM 2001111-SKY-SUM-SEC
      *
           MOVE    CONST-ALLTTL    TO   IDX2
           PERFORM 2001111-SKY-SUM-SEC
      *
           .
       200111-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    社保請求情報編集処理
      *****************************************************************
       200111-SYAHO-SKY-EDIT-SEC       SECTION.
      *
           EVALUATE    BGSSKY-RECESBT
           WHEN    "1117"
           WHEN    "1118"
               MOVE    CONST-SYAKOU9TAN    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1127"
           WHEN    "1128"
           WHEN    "1137"
           WHEN    "1138"
           WHEN    "1147"
           WHEN    "1148"
           WHEN    "1157"
           WHEN    "1158"
               MOVE    CONST-SYAKOU9HEI    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1119"
           WHEN    "1110"
               MOVE    CONST-SYAKOU7TAN    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1129"
           WHEN    "1120"
           WHEN    "1139"
           WHEN    "1130"
           WHEN    "1149"
           WHEN    "1140"
           WHEN    "1159"
           WHEN    "1150"
               MOVE    CONST-SYAKOU7HEI    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1111"
           WHEN    "1112"
               MOVE    CONST-SYAHONTAN     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1121"
           WHEN    "1122"
           WHEN    "1131"
           WHEN    "1132"
           WHEN    "1141"
           WHEN    "1142"
           WHEN    "1151"
           WHEN    "1152"
               MOVE    CONST-SYAHONHEI     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1115"
           WHEN    "1116"
               MOVE    CONST-SYAKZKTAN     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1125"
           WHEN    "1126"
           WHEN    "1135"
           WHEN    "1136"
           WHEN    "1145"
           WHEN    "1146"
           WHEN    "1155"
           WHEN    "1156"
               MOVE    CONST-SYAKZKHEI     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1113"
           WHEN    "1114"
               MOVE    CONST-SYAMISGKTAN   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1123"
           WHEN    "1124"
           WHEN    "1133"
           WHEN    "1134"
           WHEN    "1143"
           WHEN    "1144"
           WHEN    "1153"
           WHEN    "1154"
               MOVE    CONST-SYAMISGKHEI   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1211"
           WHEN    "1212"
           WHEN    "1221"
           WHEN    "1222"
           WHEN    "1231"
           WHEN    "1232"
           WHEN    "1241"
           WHEN    "1242"
               MOVE    CONST-SYAKOH        TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    OTHER
               DISPLAY "RECESBT[" BGSSKY-RECESBT "]" BGSSKY-REC
           END-EVALUATE
      *
               MOVE    CONST-SYATTL        TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           .
       200111-SYAHO-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    国保請求情報編集処理
      *****************************************************************
       200111-KOKUHO-SKY-EDIT-SEC      SECTION.
      *
           EVALUATE    BGSSKY-RECESBT
           WHEN    "1117"
           WHEN    "1118"
           WHEN    "1127"
           WHEN    "1128"
           WHEN    "1138"
           WHEN    "1138"
           WHEN    "1147"
           WHEN    "1148"
           WHEN    "1157"
           WHEN    "1158"
               MOVE    CONST-KOKIPNKOU9    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
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
               MOVE    CONST-KOKIPNKOU7    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
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
               MOVE    CONST-KOKIPNHON     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
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
               MOVE    CONST-KOKIPNHON     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
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
               MOVE    CONST-KOKIPNMISGK   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1411"
           WHEN    "1412"
           WHEN    "1421"
           WHEN    "1422"
           WHEN    "1431"
           WHEN    "1432"
           WHEN    "1441"
           WHEN    "1442"
           WHEN    "1451"
           WHEN    "1452"
               MOVE    CONST-KOKTAIHON     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1415"
           WHEN    "1416"
           WHEN    "1425"
           WHEN    "1426"
           WHEN    "1435"
           WHEN    "1436"
           WHEN    "1445"
           WHEN    "1446"
           WHEN    "1455"
           WHEN    "1456"
               MOVE    CONST-KOKTAIKZK     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1413"
           WHEN    "1414"
           WHEN    "1423"
           WHEN    "1424"
           WHEN    "1433"
           WHEN    "1434"
           WHEN    "1443"
           WHEN    "1444"
           WHEN    "1453"
           WHEN    "1454"
               MOVE    CONST-KOKTAIMISGK   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    OTHER
               DISPLAY "RECESBT[" BGSSKY-RECESBT "]" BGSSKY-REC
           END-EVALUATE
      *
           MOVE    CONST-KOKTTL            TO  IDX2
           PERFORM 2001111-SKY-SUM-SEC
      *
           .
       200111-KOKUHO-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢者請求情報編集処理
      *****************************************************************
       200111-KOUKI-SKY-EDIT-SEC       SECTION.
      *
           EVALUATE    BGSSKY-RECESBT
           WHEN    "1317"
           WHEN    "1318"
           WHEN    "1327"
           WHEN    "1328"
           WHEN    "1338"
           WHEN    "1338"
           WHEN    "1347"
           WHEN    "1348"
           WHEN    "1357"
           WHEN    "1358"
               MOVE    CONST-KOUKOU9   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    "1319"
           WHEN    "1310"
           WHEN    "1329"
           WHEN    "1320"
           WHEN    "1339"
           WHEN    "1330"
           WHEN    "1349"
           WHEN    "1340"
           WHEN    "1359"
           WHEN    "1350"
               MOVE    CONST-KOUKOU7   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
           WHEN    OTHER
               DISPLAY "RECESBT[" BGSSKY-RECESBT "]" BGSSKY-REC
           END-EVALUATE
      *
           MOVE    CONST-KOUTTL        TO  IDX2
           PERFORM 2001111-SKY-SUM-SEC
      *
           .
       200111-KOUKI-SKY-EDIT-EXT.
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
           EVALUATE    IDX1
           WHEN    1
               COMPUTE CNT-NYUIN   =   CNT-NYUIN   +   1
           WHEN    2
               COMPUTE CNT-GAIRAI  =   CNT-GAIRAI  +   1
           END-EVALUATE
      *
           .
       2001111-SKY-SUM-EXT.
           EXIT.
      *****************************************************************
      *    その他請求情報編集処理
      *****************************************************************
       2001-ETC-SKY-EDIT-SEC           SECTION.
      *
           OPEN    INPUT               BGSETC-FILE
      *
           PERFORM 900-BGSETC-READ-SEC
      *
           PERFORM UNTIL ( FLG-BGSETC  NOT =   ZERO )
      *
               IF    ( BGSETC-NYUGAIKBN    =   "1" )
                   MOVE    1           TO  IDX-NG
               ELSE
                   MOVE    2           TO  IDX-NG
               END-IF
      *
               EVALUATE    BGSETC-HOKEN
               WHEN    CONST-SYAHO
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-SYATTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3                 TO  IDX1
                   MOVE    CONST-SYATTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-KOKUHO
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-KOKTTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3                 TO  IDX1
                   MOVE    CONST-KOKTTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-KOUKI
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-KOUTTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3                 TO  IDX1
                   MOVE    CONST-KOUTTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-TOKURYO
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-TOKTTL    TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-TOKTTL    TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-JIHIHKN
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-JIHITTL   TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-JIHITTL   TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-CHIKEN
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-CHIKENTTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-CHIKENTTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-ROUSAI
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-ROUSAITTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-ROUSAITTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-JIBAI
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-JIBAITTL  TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-JIBAITTL  TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-KOUGAI
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-KOUGAITTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-KOUGAITTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               END-EVALUATE
      *
               MOVE    IDX-NG            TO  IDX1
               MOVE    CONST-ALLTTL    TO  IDX2
               PERFORM 20011-ETC-SKY-SUM-SEC
               MOVE    3               TO  IDX1
               MOVE    CONST-ALLTTL    TO  IDX2
               PERFORM 20011-ETC-SKY-SUM-SEC
      *
               PERFORM 900-BGSETC-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   BGSETC-FILE
      *
           .
       2001-ETC-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    その他請求情報集計処理
      *****************************************************************
       20011-ETC-SKY-SUM-SEC           SECTION.
      *
           IF    ( BGSETC-HOKEN    =   CONST-SYAHO
                                    OR CONST-KOKUHO
                                    OR CONST-KOUKI  )
               CONTINUE
           ELSE
               COMPUTE THKN-KENSU (IDX1 IDX2)
                   =   THKN-KENSU (IDX1 IDX2)
                   +   BGSETC-KENSU
      *
               COMPUTE THKN-NISSU (IDX1 IDX2)
                   =   THKN-NISSU (IDX1 IDX2)
                   +   BGSETC-NISSU
      *
               COMPUTE THKN-TENSU (IDX1 IDX2)
                   =   THKN-TENSU (IDX1 IDX2)
                   +   BGSETC-TENSU
      *
               COMPUTE THKN-FTNMONEY (IDX1 IDX2)
                   =   THKN-FTNMONEY (IDX1 IDX2)
                   +   BGSETC-FTNMONEY
      *
               COMPUTE THKN-HKNMONEY (IDX1 IDX2)
                   =   THKN-HKNMONEY (IDX1 IDX2)
                   +   BGSETC-HKNMONEY
      *
               COMPUTE THKN-KOHMONEY (IDX1 IDX2)
                   =   THKN-KOHMONEY (IDX1 IDX2)
                   +   BGSETC-KOHMONEY
      *
               COMPUTE THKN-TTLMONEY (IDX1 IDX2)
                   =   THKN-TTLMONEY (IDX1 IDX2)
                   +   BGSETC-TTLMONEY
      *
               COMPUTE THKN-SKJKAISU (IDX1 IDX2)
                   =   THKN-SKJKAISU (IDX1 IDX2)
                   +   BGSETC-SKJKAISU
      *
               COMPUTE THKN-SKJTTLMONEY (IDX1 IDX2)
                   =   THKN-SKJTTLMONEY (IDX1 IDX2)
                   +   BGSETC-SKJTTLMONEY
      *
               COMPUTE THKN-SKJFTNMONEY (IDX1 IDX2)
                   =   THKN-SKJFTNMONEY (IDX1 IDX2)
                   +   BGSETC-SKJFTNMONEY
      *
               COMPUTE THKN-SKJHKNMONEY (IDX1 IDX2)
                   =   THKN-SKJHKNMONEY (IDX1 IDX2)
                   +   BGSETC-SKJHKNMONEY
      *
               COMPUTE THKN-SKJKOHMONEY (IDX1 IDX2)
                   =   THKN-SKJKOHMONEY (IDX1 IDX2)
                   +   BGSETC-SKJKOHMONEY
      *
               COMPUTE THKN-TTLTTLMONEY (IDX1 IDX2)
                   =   THKN-TTLTTLMONEY (IDX1 IDX2)
                   +   BGSETC-TTLTTLMONEY
      *
               IF    ( BGSETC-KENSU        NOT =   ZERO )
                   EVALUATE    BGSETC-NYUGAIKBN
                   WHEN    "1"
                       COMPUTE CNT-NYUIN   =   CNT-NYUIN   +   1
                   WHEN    "2"
                       COMPUTE CNT-GAIRAI  =   CNT-GAIRAI  +   1
                   END-EVALUATE
               END-IF
           END-IF
      *
               COMPUTE THKN-JIHIMONEY (IDX1 IDX2)
                   =   THKN-JIHIMONEY (IDX1 IDX2)
                   +   BGSETC-JIHIMONEY
      *
               COMPUTE THKN-GENMEN (IDX1 IDX2)
                   =   THKN-GENMEN (IDX1 IDX2)
                   +   BGSETC-GENMEN
      *
               COMPUTE THKN-CHOSEI1 (IDX1 IDX2)
                   =   THKN-CHOSEI1 (IDX1 IDX2)
                   +   BGSETC-CHOSEI1
      *
               COMPUTE THKN-CHOSEI2 (IDX1 IDX2)
                   =   THKN-CHOSEI2 (IDX1 IDX2)
                   +   BGSETC-CHOSEI2
      *
               COMPUTE THKN-HASU (IDX1 IDX2)
                   =   THKN-HASU (IDX1 IDX2)
                   +   BGSETC-HASU
      *
           .
       20011-ETC-SKY-SUM-EXT.
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
           COMPUTE TKOH-KENSU (IDX1)
               =   TKOH-KENSU (IDX1)   +   BGSKOH-KENSU
      *
           COMPUTE TKOH-KENSU (3)
               =   TKOH-KENSU (3)      +   BGSKOH-KENSU
      *
           .
       20011-KOH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院総括表編集処理
      *****************************************************************
       2001-HCGS139N-EDIT-SEC          SECTION.
      *
           INITIALIZE                      HCGS139N
      *
           MOVE    WRK-PRT-SRYYM       TO  HCGS139N-SRYYM
           IF    ( IDX1                =   1 )
               MOVE    "入院"          TO  HCGS139N-NYUGAI
           ELSE
               MOVE    "入院・外来　総計"
                                       TO  HCGS139N-NYUGAI
           END-IF
           MOVE    WRK-PRT-SAKYMD      TO  HCGS139N-SYSYMD
           MOVE    WRK-SRYKANAME1      TO  HCGS139N-SRYKANAME
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
           MOVE    CNT-PAGE            TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCGS139N-PAGE
      *
           MOVE    ZERO                TO  CNT-LINE
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CONST-THKN-MAX )
      *
               COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
               IF    ( KEY-RECEKA  NOT =   SPACE )
                AND  ( IDX2     =      CONST-TOKTTL
                                 OR    CONST-JIHITTL
                                 OR    CONST-CHIKENTTL
                                 OR    CONST-ROUSAITTL
                                 OR    CONST-JIBAITTL
                                 OR    CONST-KOUGAITTL
                                 OR    CONST-ALLTTL )
                   CONTINUE
               ELSE
      *
                   MOVE    THKN-KENSU (IDX1 IDX2)     TO  WRK-Z4
                   MOVE    WRK-Z4  TO  HCGS139N-KENSU (CNT-LINE)
      *
                   MOVE    THKN-NISSU (IDX1 IDX2)     TO  WRK-Z4
                   MOVE    WRK-Z4  TO  HCGS139N-NISSU (CNT-LINE)
      *
                   MOVE    THKN-TENSU (IDX1 IDX2)     TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-TENSU (CNT-LINE)
      *
                   MOVE    THKN-FTNMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-FTNMONEY (CNT-LINE)
      *
                   MOVE    THKN-HKNMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-HKNMONEY (CNT-LINE)
      *
                   MOVE    THKN-KOHMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-KOHMONEY (CNT-LINE)
      *
                   MOVE    THKN-TTLMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-TTLMONEY (CNT-LINE)
      *
                   MOVE    THKN-SKJKAISU (IDX1 IDX2)  TO  WRK-Z5
                   MOVE    WRK-Z5  TO  HCGS139N-SKJKAISU (CNT-LINE)
      *
                   MOVE    THKN-SKJTTLMONEY (IDX1 IDX2)   TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-SKJTTLMONEY (CNT-LINE)
      *
                   MOVE    THKN-SKJFTNMONEY (IDX1 IDX2)   TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-SKJFTNMONEY (CNT-LINE)
      *
                   MOVE    THKN-SKJHKNMONEY (IDX1 IDX2)   TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-SKJHKNMONEY (CNT-LINE)
      *
                   MOVE    THKN-SKJKOHMONEY (IDX1 IDX2)   TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-SKJKOHMONEY (CNT-LINE)
      *
                   MOVE    THKN-TTLTTLMONEY (IDX1 IDX2) TO WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-TTLTTLMONEY (CNT-LINE)
               END-IF
      *
               IF    ( IDX2     =      CONST-SYATTL
                                 OR    CONST-KOKTTL
                                 OR    CONST-KOUTTL
                                 OR    CONST-TOKTTL
                                 OR    CONST-JIHITTL
                                 OR    CONST-CHIKENTTL
                                 OR    CONST-ROUSAITTL
                                 OR    CONST-JIBAITTL
                                 OR    CONST-KOUGAITTL
                                 OR    CONST-ALLTTL )
      *
                   IF    ( KEY-RECEKA      =   SPACE )
                       MOVE    THKN-JIHIMONEY  (IDX1 IDX2)  TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139N-JIHIMONEY (CNT-LINE)
                       MOVE    THKN-GENMEN    (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139N-GENMEN (CNT-LINE)
                       MOVE    THKN-CHOSEI1   (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139N-CHOSEI1 (CNT-LINE)
                       MOVE    THKN-CHOSEI2   (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139N-CHOSEI2 (CNT-LINE)
                       MOVE    THKN-HASU      (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139N-HASU    (CNT-LINE)
      *
                       DISPLAY IDX1 ":"IDX2 " "
                           "JIHIMONEY[" THKN-JIHIMONEY(IDX1 IDX2) "]"
                           "GENMEN["    THKN-GENMEN   (IDX1 IDX2) "]"
                           "CHOSEI1["   THKN-CHOSEI1  (IDX1 IDX2) "]"
                           "CHOSEI2["   THKN-CHOSEI2  (IDX1 IDX2) "]"
                           "HASU["      THKN-HASU     (IDX1 IDX2) "]"
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( KEY-RECEKA          =   SPACE )
               MOVE    SPACE           TO  HCGS139N-ETCHKNMSG
               STRING  "（注）件数は患者数"
                                           DELIMITED BY SIZE
                       "（複数保険使用の場合はその数）"
                                           DELIMITED BY SIZE
                       "の集計結果となります。実際のレセプト件数"
                                           DELIMITED BY SIZE
                       "・日数と一致しない場合があります。"
                                           DELIMITED BY SIZE
               INTO    HCGS139N-ETCHKNMSG
               END-STRING
           ELSE
               MOVE    SPACE           TO  HCGS139N-ETCHKNMSG
               STRING    "（注）全科合計に編集されます。"
                                           DELIMITED   BY  SIZE
               INTO    HCGS139N-ETCHKNMSG
               END-STRING
      *
               MOVE    SPACE   TO  HCGS139N-ETCMSG
               STRING  "その他計は全科合計に編集されます。"
                                       DELIMITED   BY  SIZE
               INTO    HCGS139N-ETCMSG
               END-STRING
           END-IF
      *
           MOVE    TKOH-KENSU (IDX1)   TO  WRK-Z4
           MOVE    WRK-Z4              TO  HCGS139N-KOHKENSU
      *
           PERFORM 230-HCGS139N-PRINT-SEC
      *
           .
       2001-HCGS139N-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-HCGS139N-PRINT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
      *
           MOVE    "HCGS139N.red" TO  SPRT-PRTID
           MOVE    HCGS139N       TO  SPRT-PRTDATA
      *
           PERFORM 230-PRINT-SEC
      *
           .
       230-HCGS139N-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    外来総括表編集処理
      *****************************************************************
       2001-HCGS139G-EDIT-SEC          SECTION.
      *
           INITIALIZE                      HCGS139G
      *
           MOVE    WRK-PRT-SRYYM       TO  HCGS139G-SRYYM
           MOVE    "外来"              TO  HCGS139G-NYUGAI
           MOVE    WRK-PRT-SAKYMD      TO  HCGS139G-SYSYMD
           MOVE    WRK-SRYKANAME1      TO  HCGS139G-SRYKANAME
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
           MOVE    CNT-PAGE            TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCGS139G-PAGE
      *
           MOVE    ZERO                TO  CNT-LINE
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CONST-THKN-MAX )
      *
               COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
               IF    ( KEY-RECEKA  NOT =   SPACE )
                AND  ( IDX2     =      CONST-TOKTTL
                                 OR    CONST-JIHITTL
                                 OR    CONST-CHIKENTTL
                                 OR    CONST-ROUSAITTL
                                 OR    CONST-JIBAITTL
                                 OR    CONST-KOUGAITTL
                                 OR    CONST-ALLTTL )
                   CONTINUE
               ELSE
                   MOVE    THKN-KENSU (IDX1 IDX2)     TO  WRK-Z4
                   MOVE    WRK-Z4  TO  HCGS139G-KENSU (CNT-LINE)
      *
                   MOVE    THKN-NISSU (IDX1 IDX2)     TO  WRK-Z4
                   MOVE    WRK-Z4  TO  HCGS139G-NISSU (CNT-LINE)
      *
                   MOVE    THKN-TENSU (IDX1 IDX2)     TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139G-TENSU (CNT-LINE)
      *
                   MOVE    THKN-FTNMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139G-FTNMONEY (CNT-LINE)
      *
                   MOVE    THKN-HKNMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139G-HKNMONEY (CNT-LINE)
      *
                   MOVE    THKN-KOHMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139G-KOHMONEY (CNT-LINE)
      *
                   MOVE    THKN-TTLMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139G-TTLMONEY (CNT-LINE)
               END-IF
      *
               IF    ( IDX2     =      CONST-SYATTL
                                 OR    CONST-KOKTTL
                                 OR    CONST-KOUTTL
                                 OR    CONST-TOKTTL
                                 OR    CONST-JIHITTL
                                 OR    CONST-CHIKENTTL
                                 OR    CONST-ROUSAITTL
                                 OR    CONST-JIBAITTL
                                 OR    CONST-KOUGAITTL
                                 OR    CONST-ALLTTL )
      *
                   IF    ( KEY-RECEKA      =   SPACE )
                       MOVE    THKN-JIHIMONEY  (IDX1 IDX2)  TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139G-JIHIMONEY (CNT-LINE)
                       MOVE    THKN-GENMEN    (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139G-GENMEN (CNT-LINE)
                       MOVE    THKN-CHOSEI1   (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139G-CHOSEI1 (CNT-LINE)
                       MOVE    THKN-CHOSEI2   (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139G-CHOSEI2 (CNT-LINE)
                       MOVE    THKN-HASU      (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139G-HASU    (CNT-LINE)
      *
                       DISPLAY IDX1 ":"IDX2 " "
                           "JIHIMONEY[" THKN-JIHIMONEY(IDX1 IDX2) "]"
                           "GENMEN["    THKN-GENMEN   (IDX1 IDX2) "]"
                           "CHOSEI1["   THKN-CHOSEI1  (IDX1 IDX2) "]"
                           "CHOSEI2["   THKN-CHOSEI2  (IDX1 IDX2) "]"
                           "HASU["      THKN-HASU     (IDX1 IDX2) "]"
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( KEY-RECEKA          =   SPACE )
               MOVE    SPACE           TO  HCGS139G-ETCHKNMSG
               STRING  "（注）件数は患者数"
                                           DELIMITED BY SIZE
                       "（複数保険使用の場合はその数）"
                                           DELIMITED BY SIZE
                       "の集計結果となります。実際のレセプト件数"
                                           DELIMITED BY SIZE
                       "・日数と一致しない場合があります。"
                                           DELIMITED BY SIZE
               INTO    HCGS139G-ETCHKNMSG
               END-STRING
           ELSE
               MOVE    SPACE           TO  HCGS139G-ETCHKNMSG
               STRING    "（注）全科合計に編集されます。"
                                           DELIMITED   BY  SIZE
               INTO    HCGS139G-ETCHKNMSG
               END-STRING
      *
               MOVE    SPACE   TO  HCGS139G-ETCMSG
               STRING  "その他計は全科合計に編集されます。"
                                       DELIMITED   BY  SIZE
               INTO    HCGS139G-ETCMSG
               END-STRING
           END-IF
      *
           MOVE    TKOH-KENSU (IDX1)   TO  WRK-Z4
           MOVE    WRK-Z4              TO  HCGS139G-KOHKENSU
      *
           PERFORM 230-HCGS139G-PRINT-SEC
      *
           .
       2001-HCGS139G-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-HCGS139G-PRINT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
      *
           MOVE    "HCGS139G.red" TO  SPRT-PRTID
           MOVE    HCGS139G       TO  SPRT-PRTDATA
      *
           PERFORM 230-PRINT-SEC
      *
           .
       230-HCGS139G-PRINT-EXT.
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
           DISPLAY "ORCBGS139 RC [" BGS100P-RC "]"
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
