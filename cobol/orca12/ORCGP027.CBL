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
       PROGRAM-ID.             ORCGP027.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 保険組合せ確認（Ｐ０２７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     01/05/01  特別地方公費組合せ修正
      *  01.00.02    MCC-多々納   01/06/13  登録後、診療行為遷移を追加
      *  01.00.03    MCC-小豆沢   02/03/13  患者負担区分に固定額を追加
      *  02.00.01    NACL-多々納  02/07/31  パラメタファイルをＤＢへ
      *  02.00.02    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/XX  保険組合せ非表示対応
      *  04.05.00    NACL-多々納  09/06/26  入院食事区分対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  INDEXED
      *                            ACCESS  MODE    IS  DYNAMIC
      *                            RECORD  KEY     IS  PAR-KEY
      *                            FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PAR-KEY.
      *        05  PAR-FILEMEI         PIC X(08).
      *        05  PAR-EDANUM          PIC 9(04).
      *****03  PAR-DATA-REC            PIC X(3000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
       01  FILE-NAME1.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  FILLER              PIC X(07)   VALUE   "P01PARA".
           03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RENNUM          PIC ZZZ9.
           03  WRK-RENNUMZ         PIC ZZZZ.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-STR             PIC X(06).
           03  WRK-FTNKBN          PIC X(01).
           03  WRK-FTNKBN2         PIC X(01).
           03  WRK-SAKIPG          PIC X(08).
           03  WRK-ST              PIC 9(04).
           03  WRK-ED              PIC 9(04).
           03  WRK-SBTA            PIC 9(01).
      *
           03  WRK-PARA-EDANUM     PIC 9(03).
      *
           03  WRK-SELNUM          PIC 9(04).
      *
           03  WRK-PAGE            PIC 9(04).
      *
      *    ソート用キー
           03  WRK-SORT-KEY.
                05  WRK-SRT-TEKISTYMD      PIC X(08).
                05  WRK-SRT-HKNCOMBINUM    PIC X(04).
                05  WRK-SRT-IDX            PIC 9(04).
      *
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-KOHID           PIC 9(10).
           03  WRK-JKYSNUM         PIC X(20).
      *
      *    非表示区分テーブル（P02SCR-SPA のSPA-HYO-HKNCOMBI-AREA）
           03  WRK-HYO-HKNCOMBI-AREA.
               05  WRK-HYO-HKNCOMB-OCC     OCCURS    200.
                   07  WRK-HYO-HKNCOMBINUM     PIC 9(04).
                   07  WRK-HYO-HKNNUM          PIC X(03).
                   07  WRK-HYO-KOH1HKNNUM      PIC X(03).
                   07  WRK-HYO-KOH2HKNNUM      PIC X(03).
                   07  WRK-HYO-KOH3HKNNUM      PIC X(03).
                   07  WRK-HYO-KOH4HKNNUM      PIC X(03).
                   07  WRK-HYO-HYOJIKBN        PIC X(01).
                   07  WRK-HYO-ADDKBN          PIC 9(01).
               05  WRK-HYO-MAX                 PIC 9(03).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
           03  WRK-GYOCNT          PIC 9(02)   VALUE   15.
           03  WRK-AMARI           PIC 9(02).
      *
           03  WRK-ZHENSYU.
               05  WRK-ZHEN-GAI-NAIYO.
                   07  WRK-ZHEN-GAI-KFTNRATE   PIC  ZZZ.
                   07  WRK-ZHEN-GAI-KKOTGAK    PIC  ZZZZZZ.
                   07  WRK-ZHEN-GAI-KJGNGAK    PIC  ZZZZZZ.
                   07  WRK-ZHEN-GAI-DJGNGAK    PIC  ZZZZZZ.
                   07  WRK-ZHEN-GAI-DJGNCNT    PIC  ZZ.
                   07  WRK-ZHEN-GAI-MNAIJGNGAK PIC  ZZZZZZ.
                   07  WRK-ZHEN-GAI-MGAIJGNGAK PIC  ZZZZZZ.
                   07  WRK-ZHEN-GAI-MJGNCNT    PIC  ZZ.
      *
      *    保険組合せＳＯＲＴテーブル
       01  SRT-HKNCOMB-AREA.
           02  SRT-HKNCOMB-REC         OCCURS   50.
           COPY  "CPHKNCOMBI.INC"
                         REPLACING  //COMB-// BY //SRT-COMB-//.
               03  SRT-COMB-EDANUM      PIC 9(03).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    保険組合せ
       01  HKNCOMB-REC.
           COPY  "CPHKNCOMBI.INC".
      *
      *    保険組合せテーブル
       01  LNK-HKNCOMB-AREA.
           02  LNK-HKNCOMB-REC         OCCURS   50.
           COPY  "CPHKNCOMBI.INC"
                         REPLACING  //COMB-// BY //LNK-COMB-//.
               03  LNK-COMB-EDANUM      PIC 9(03).
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "P02.INC".
           COPY    "P02A.INC".
      *****COPY    "P02B.INC".
      *****COPY    "P02C.INC".
           COPY    "P02D.INC".
           COPY    "P02E.INC".
           COPY    "P02F.INC".
           COPY    "P014.INC".
           COPY    "P015.INC".
           COPY    "P98.INC".
           COPY    "P981.INC".
      *****COPY    "P99.INC".
           COPY    "P0201.INC".
           COPY    "P982.INC".
           COPY    "P100.INC".
           COPY    "P02X.INC".
           COPY    "P02S.INC".
           COPY    "P02Q.INC".
           COPY    "P02Q1.INC".
           COPY    "P02Y.INC".
           COPY    "P02Y1.INC".
           COPY    "P02G.INC".
           COPY    "P02G2.INC".
           COPY    "P02K.INC".
           COPY    "PERR.INC".
           COPY    "PID1.INC".
           COPY    "CPAPIMOD.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
           IF      FLG-END             =   ZERO
               PERFORM 5001-MAPCUR-SEC
      *********PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-P027-AREA
      *
      *    初期画面編集
           PERFORM 300-SCREEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    初期画面編集
           PERFORM 310-SPASET-SEC
      *
           MOVE    1                   TO  SPA-GMN27-HKN-PCNT
           MOVE    1                   TO  SPA-GMN27-KOH-PCNT
      *
      *    パラメタファイル検索処理
           PERFORM 320-PARAFILE-KENSAKU-SEC
      *
           MOVE    1               TO  SPA-GMN27-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI27-GMN
           INITIALIZE                      SPA-GMN27-HKN-AREA
      *
      *    非表示区分 テーブル位置クリア
           MOVE    SPA-HYO-HKNCOMBI-AREA
                                       TO  WRK-HYO-HKNCOMBI-AREA
           MOVE    ZERO                TO  SPA-HYO-MAX
           PERFORM VARYING     IDX5    FROM    1   BY  1
                   UNTIL       IDX5    >   200
               IF     (SPA-HYO-HKNCOMBINUM (IDX5)  NOT =   ZERO )
                  AND (SPA-HYO-ADDKBN      (IDX5)  =   ZERO  )
                  ADD     1                TO  SPA-HYO-MAX
               ELSE
                   INITIALIZE              SPA-HYO-HKNCOMB-OCC(IDX5)
               END-IF
           END-PERFORM
      *
      *    保険表示
           INITIALIZE                  SPA-GMN27-HKN2-AREA
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTHKN-MAX
               IF      SPA-PTHKN-SAKJOKBN (IDX)   =   SPACE
                   IF      IDY             >=  5
                       ADD     1               TO  WRK-PAGE
                       MOVE    ZERO            TO  IDY
                   END-IF
                   ADD     1               TO  IDY
                   MOVE    IDX             TO  SPA-GMN27-HKNRENNUM
                                                      (WRK-PAGE IDY)
               END-IF
           END-PERFORM
      *
      *    公費表示
           INITIALIZE                  SPA-GMN27-KOH-AREA
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-PTKOH-MAX )
                         OR   (WRK-PAGE    >   8    )
               IF      SPA-PTKOH-SAKJOKBN (IDX)   =   SPACE
                   IF      IDY             >=  5
                       ADD     1               TO  WRK-PAGE
                       MOVE    ZERO            TO  IDY
                   END-IF
                   IF      WRK-PAGE        <=   8
                       ADD     1               TO  IDY
                       MOVE    IDX             TO  SPA-GMN27-KOHRENNUM
                                                      (WRK-PAGE IDY)
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    "2"             TO  SPA-GMN27-NYUGAIKBN
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル検索処理
      *****************************************************************
       320-PARAFILE-KENSAKU-SEC        SECTION.
      *
      *    組合せ表示　初期設定処理
           PERFORM 3210-COMB-INIT-SET-SEC
      *
      *    パラメタファイルより編集
           INITIALIZE                  LNK-HKNCOMB-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO            TO  IDY
           PERFORM
               UNTIL   FLG-PARA            =   1
      *
               MOVE    PARA-DATA-REC       TO  HKNCOMB-REC
               IF     (PARA-DATA-REC   NOT =   SPACE )  AND
                      (COMB-DELKBN         =   SPACE )
                   ADD     1               TO  IDX
                   IF     (IDX             >=  WRK-ST )   AND
                          (IDX             <=  WRK-ED )
                       ADD     1               TO  IDY
      *DDDD            MOVE    PARA-EDANUM     TO  SPA-NAI27-EDANUM
      *DDD                                                 (IDY)
                       MOVE    PARA-DATA-REC   TO  LNK-HKNCOMB-REC(IDY)
                       MOVE    PARA-EDANUM     TO  LNK-COMB-EDANUM
                                                                 (IDY)
                   END-IF
               END-IF
      *        今回追加分を非表示テーブルへ追加
               IF      COMB-CHGKBN         =   "A"
                   ADD     1                   TO  SPA-HYO-MAX
                   MOVE    COMB-HKNCOMBINUM    TO  SPA-HYO-HKNCOMBINUM
                                                          (SPA-HYO-MAX)
                   MOVE    COMB-HKNNUM         TO  SPA-HYO-HKNNUM
                                                          (SPA-HYO-MAX)
                   MOVE    COMB-KOH1HKNNUM     TO  SPA-HYO-KOH1HKNNUM
                                                          (SPA-HYO-MAX)
                   MOVE    COMB-KOH2HKNNUM     TO  SPA-HYO-KOH2HKNNUM
                                                          (SPA-HYO-MAX)
                   MOVE    COMB-KOH3HKNNUM     TO  SPA-HYO-KOH3HKNNUM
                                                          (SPA-HYO-MAX)
                   MOVE    COMB-KOH4HKNNUM     TO  SPA-HYO-KOH4HKNNUM
                                                          (SPA-HYO-MAX)
                   MOVE    1                   TO  SPA-HYO-ADDKBN
                                                          (SPA-HYO-MAX)
                   PERFORM 3201-ADD-HYOJIKBN-SEC
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDY                 >   ZERO
               MOVE    IDY             TO  SPA-GMN27-MAX
      *        初期画面編集
               PERFORM 320-HONCOMBI-SET-SEC
           END-IF
      *
           .
       320-PARAFILE-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    新規保険組合せの表示区分設定処理
      *****************************************************************
       3201-ADD-HYOJIKBN-SEC     SECTION.
      *
           PERFORM VARYING     IDX5    FROM    1   BY  1
                   UNTIL       IDX5    >   WRK-HYO-MAX
               IF     (COMB-HKNCOMBINUM    =   WRK-HYO-HKNCOMBINUM
                                                          (IDX5))  AND
                      (COMB-HKNNUM         =   WRK-HYO-HKNNUM
                                                          (IDX5))  AND
                      (COMB-KOH1HKNNUM     =   WRK-HYO-KOH1HKNNUM
                                                          (IDX5))  AND
                      (COMB-KOH2HKNNUM     =   WRK-HYO-KOH2HKNNUM
                                                          (IDX5))  AND
                      (COMB-KOH3HKNNUM     =   WRK-HYO-KOH3HKNNUM
                                                          (IDX5))  AND
                      (COMB-KOH4HKNNUM     =   WRK-HYO-KOH4HKNNUM
                                                          (IDX5))  AND
                      (WRK-HYO-ADDKBN (IDX5)   =   1    )
                   MOVE    WRK-HYO-HYOJIKBN (IDX5)
                                               TO  SPA-HYO-HYOJIKBN 
                                                          (SPA-HYO-MAX)
                   MOVE    WRK-HYO-MAX         TO  IDX5
               END-IF
           END-PERFORM
           .
       3201-ADD-HYOJIKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険公費表示　初期設定処理
      *****************************************************************
       3210-COMB-INIT-SET-SEC    SECTION.
      *
      *    表示ページ設定
           IF  SPA-P027-PG         =   ZERO
               MOVE    1           TO  SPA-P027-PG
           END-IF
           MOVE    ZERO            TO  IDX
           MOVE    ZERO            TO  IDY
      *    画面表示するデータが何番目から何番目までかを求める
           COMPUTE   WRK-ST        =   ( SPA-P027-PG - 1 )  *  50  +  1
           COMPUTE   WRK-ED        =   WRK-ST   +   49
      *
           .
       3210-COMB-INIT-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険公費表示　初期設定処理
      *****************************************************************
       320-HONCOMBI-SET-SEC    SECTION.
      *
      *    適用開始日順にＳＯＲＴする
           PERFORM 3201-HKNCOMBI-SORT-SEC
      *
           MOVE    ZERO                TO  SPA-NAI27-GMN
           INITIALIZE                      SPA-GMN27-HKN-AREA
      *
           MOVE    ZERO                TO  IDY
           PERFORM   VARYING   IDX     FROM   1     BY 1
                     UNTIL     IDX     >   SPA-GMN27-MAX
              ADD     1                    TO  IDY
      *       選択チェック用
              MOVE    LNK-COMB-HKNCOMBINUM  (IDX)(1:4)
                                           TO  SPA-NAI27-HKNNUM (IDY)
      *
               MOVE    LNK-COMB-HKNCOMBINUM  (IDX)
                                           TO  SPA-GMN27-NUM  (IDY)
               MOVE    LNK-COMB-SYU-TANSEIDONAME (IDX)
                                           TO  SPA-GMN27-HKNSEIDO(IDY)
               MOVE    LNK-COMB-KOH1-TANSEIDONAME(IDX)
                                           TO  SPA-GMN27-KOHSEIDO1(IDY)
               MOVE    LNK-COMB-KOH2-TANSEIDONAME(IDX)
                                           TO  SPA-GMN27-KOHSEIDO2(IDY)
               MOVE    LNK-COMB-KOH3-TANSEIDONAME(IDX)
                                           TO  SPA-GMN27-KOHSEIDO3(IDY)
               MOVE    LNK-COMB-KOH4-TANSEIDONAME(IDX)
                                           TO  SPA-GMN27-KOHSEIDO4(IDY)
      *
               MOVE    LNK-COMB-TEKSTYMD(IDX) TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN27-TEKSTYMD(IDY)
      *
               MOVE    LNK-COMB-TEKEDYMD(IDX) TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN27-TEKEDYMD(IDY)
      *        労災の時、部位を公費１の項目へ
               IF      LNK-COMB-HKNNUM (IDX)   =   "971"
                                               OR  "973"
                   PERFORM 3201-RSI-HENSYU-SEC
               END-IF
      *
      *        特定負有の時、受給者番号設定
               IF     (LNK-COMB-KOH1HKNNUM (IDX)   =   "051"
                                                   OR  "052"
                                                   OR  "091"
                                                   OR  "015"
                                                   OR  "016"
                                                   OR  "038" )
      *            北海道の地方公費
                   OR ((LNK-COMB-KOH1HKNNUM (IDX)  =   "183" ) AND
                       (SPA-PREFNUM                 =   01   ))
                   MOVE    LNK-COMB-KOH1HKNNUM (IDX)   TO  WRK-KOHNUM
                   MOVE    LNK-COMB-KOH1ID (IDX)       TO  WRK-KOHID
                   PERFORM 3202-JKYSNUM-HENSYU-SEC
               END-IF
               IF     (LNK-COMB-KOH2HKNNUM (IDX)   =   "051"
                                                   OR  "052"
                                                   OR  "091"
                                                   OR  "015"
                                                   OR  "016"
                                                   OR  "038" )
      *            北海道の地方公費
                   OR ((LNK-COMB-KOH2HKNNUM (IDX)  =   "183" ) AND
                       (SPA-PREFNUM                 =   01   ))
                   MOVE    LNK-COMB-KOH2HKNNUM (IDX)   TO  WRK-KOHNUM
                   MOVE    LNK-COMB-KOH2ID (IDX)       TO  WRK-KOHID
                   PERFORM 3202-JKYSNUM-HENSYU-SEC
               END-IF
               IF     (LNK-COMB-KOH3HKNNUM (IDX)   =   "051"
                                                   OR  "052"
                                                   OR  "091"
                                                   OR  "015"
                                                   OR  "016"
                                                   OR  "038" )
      *            北海道の地方公費
                   OR ((LNK-COMB-KOH3HKNNUM (IDX)  =   "183" ) AND
                       (SPA-PREFNUM                 =   01   ))
                   MOVE    LNK-COMB-KOH3HKNNUM (IDX)   TO  WRK-KOHNUM
                   MOVE    LNK-COMB-KOH3ID (IDX)       TO  WRK-KOHID
                   PERFORM 3202-JKYSNUM-HENSYU-SEC
               END-IF
               IF     (LNK-COMB-KOH4HKNNUM (IDX)   =   "051"
                                                   OR  "052"
                                                   OR  "091"
                                                   OR  "015"
                                                   OR  "016"
                                                   OR  "038" )
      *            北海道の地方公費
                   OR ((LNK-COMB-KOH4HKNNUM (IDX)  =   "183" ) AND
                       (SPA-PREFNUM                 =   01   ))
                   MOVE    LNK-COMB-KOH4HKNNUM (IDX)   TO  WRK-KOHNUM
                   MOVE    LNK-COMB-KOH4ID (IDX)       TO  WRK-KOHID
                   PERFORM 3202-JKYSNUM-HENSYU-SEC
               END-IF
      *        パラメタ位置
               MOVE    LNK-COMB-EDANUM (IDX)   TO  SPA-NAI27-EDANUM(IDY)
      *        表示区分
               PERFORM 3201-HYOJIKBN-HEN-SEC
           END-PERFORM
      *
           .
       320-HONCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示区分編集処理
      *****************************************************************
       3201-HYOJIKBN-HEN-SEC            SECTION.
     *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX5    FROM    1   BY  1
                   UNTIL      (IDX5        >   SPA-HYO-MAX )
                           OR (FLG-OK      =   1    )
               IF     (LNK-COMB-HKNCOMBINUM  (IDX)
                                       =   SPA-HYO-HKNCOMBINUM(IDX5))
               AND    (LNK-COMB-HKNNUM  (IDX)
                                       =   SPA-HYO-HKNNUM  (IDX5))
               AND    (LNK-COMB-KOH1HKNNUM  (IDX)
                                       =   SPA-HYO-KOH1HKNNUM (IDX5))
               AND    (LNK-COMB-KOH2HKNNUM  (IDX)
                                       =   SPA-HYO-KOH2HKNNUM (IDX5))
               AND    (LNK-COMB-KOH3HKNNUM  (IDX)
                                       =   SPA-HYO-KOH3HKNNUM (IDX5))
               AND    (LNK-COMB-KOH4HKNNUM  (IDX)
                                       =   SPA-HYO-KOH4HKNNUM (IDX5))
                   MOVE    SPA-HYO-HYOJIKBN (IDX5)
                                           TO  SPA-GMN27-THYOJIKBN (IDY)
                   MOVE    IDX5            TO  SPA-GMN27-HYOIDX   (IDY)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           .
       3201-HYOJIKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定疾患の受給者番号編集処理
      *****************************************************************
       3202-JKYSNUM-HENSYU-SEC            SECTION.
     *
           MOVE    SPACE               TO  WRK-JKYSNUM
           PERFORM VARYING     IDX4    FROM    1   BY  1
                   UNTIL       IDX4    >   SPA-PTKOH-MAX
               IF     (SPA-PTKOH-KOHID (IDX4)  NOT =   ZERO )  AND
                      (SPA-PTKOH-KOHID (IDX4)  =   WRK-KOHID)
                   MOVE    SPA-PTKOH-JKYSNUM (IDX4)
                                               TO  WRK-JKYSNUM
                   MOVE    SPA-PTKOH-MAX       TO  IDX4
               END-IF
           END-PERFORM
      *
           IF      WRK-JKYSNUM     NOT =   SPACE
               MOVE    WRK-JKYSNUM         TO  SPA-GMN27-TJKYSNUM(IDX)
           END-IF
      *
           .
       3202-JKYSNUM-HENSYUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用開始日順にＳＯＲＴ編集処理
      *****************************************************************
       3201-HKNCOMBI-SORT-SEC            SECTION.
      *
           INITIALIZE                  SRT-HKNCOMB-AREA
           MOVE    ZERO                TO  IDX3
      *    当日適用分 適用開始日順にＳＯＲＴする
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN27-MAX
               MOVE    ALL "9"         TO  WRK-SRT-TEKISTYMD
               MOVE    ALL "9"         TO  WRK-SRT-HKNCOMBINUM
               MOVE    ZERO            TO  WRK-SRT-IDX
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2     >   SPA-GMN27-MAX
                   IF      LNK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                       CONTINUE
                   ELSE
                       IF    (SPA-SYSYMD      >=  LNK-COMB-TEKSTYMD
                                                           (IDX2))  AND
                             (SPA-SYSYMD      <=  LNK-COMB-TEKEDYMD
                                                           (IDX2))
                           PERFORM 301331-SORTSYORI-SEC
                       END-IF
                   END-IF
               END-PERFORM
               IF      WRK-SRT-IDX     NOT =   ZERO
                   ADD     1           TO  IDX3
                   MOVE    LNK-HKNCOMB-REC (WRK-SRT-IDX)
                                       TO  SRT-HKNCOMB-REC(IDX3)
                   INITIALIZE              LNK-HKNCOMB-REC (WRK-SRT-IDX)
               END-IF
           END-PERFORM
      *    当日適用分外
           PERFORM VARYING     IDX     FROM    1     BY  1
                   UNTIL       IDX     >   50
               MOVE    ALL "9"         TO  WRK-SRT-TEKISTYMD
               MOVE    ALL "9"         TO  WRK-SRT-HKNCOMBINUM
               MOVE    ZERO            TO  WRK-SRT-IDX
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2     >   SPA-GMN27-MAX
                   IF      LNK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                       CONTINUE
                   ELSE
                       PERFORM 301331-SORTSYORI-SEC
                   END-IF
               END-PERFORM
               IF      WRK-SRT-IDX     NOT =   ZERO
                   ADD     1           TO  IDX3
                   MOVE    LNK-HKNCOMB-REC (WRK-SRT-IDX)
                                       TO  SRT-HKNCOMB-REC(IDX3)
                   INITIALIZE              LNK-HKNCOMB-REC (WRK-SRT-IDX)
               END-IF
           END-PERFORM
           MOVE    ZERO                TO   SPA-GMN27-MAX
           INITIALIZE                   LNK-HKNCOMB-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
               IF      SRT-COMB-HKNCOMBINUM (IDX) NOT =   ZERO
                   ADD     1           TO   SPA-GMN27-MAX
                   MOVE    SRT-HKNCOMB-REC (IDX)
                                       TO  LNK-HKNCOMB-REC
                                                  (SPA-GMN27-MAX)
               END-IF
           END-PERFORM
      *
           .
       3201-HKNCOMBI-SORT-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       301331-SORTSYORI-SEC             SECTION.
      *
           IF      WRK-SRT-TEKISTYMD   >   LNK-COMB-TEKSTYMD  (IDX2)
               MOVE    IDX2                    TO  WRK-SRT-IDX
               MOVE    LNK-COMB-TEKSTYMD (IDX2) TO  WRK-SRT-TEKISTYMD
               MOVE    LNK-COMB-HKNCOMBINUM(IDX2)
                                               TO  WRK-SRT-HKNCOMBINUM
           ELSE
               IF      WRK-SRT-TEKISTYMD   =   LNK-COMB-TEKSTYMD(IDX2)
                   IF      WRK-SRT-HKNCOMBINUM
                                               >   LNK-COMB-HKNCOMBINUM
                                                              (IDX2)
                       MOVE    IDX2                TO  WRK-SRT-IDX
                       MOVE    LNK-COMB-TEKSTYMD (IDX2)
                                                   TO  WRK-SRT-TEKISTYMD
                       MOVE    LNK-COMB-HKNCOMBINUM(IDX2)
                                                   TO
                                                   WRK-SRT-HKNCOMBINUM
                   END-IF
               END-IF
           END-IF
           .
       301331-SORTSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の時、部位を公費１の項目へ編集処理
      *****************************************************************
       3201-RSI-HENSYU-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   SPA-PTHKN-MAX
               IF      SPA-PTHKN-HKNID (IDX2)  =   LNK-COMB-HKNID
                                                             (IDX)
                   MOVE    SPACE         TO  SPA-GMN27-KOHSEIDO1(IDY)
                   IF      SPA-PTHKN-NUM (IDX2)    =   SPACE
                       MOVE    SPA-PTHKN-KIGO (IDX2)(1:2)
                                         TO  SPA-GMN27-KOHSEIDO1(IDY)
                   ELSE
                       STRING  SPA-PTHKN-KIGO (IDX2)(1:2)
                                             DELIMITED  BY  SIZE
                           "（"              DELIMITED  BY  SIZE
                           SPA-PTHKN-NUM (IDX2)
                                             DELIMITED  BY  SPACE
                           "）"              DELIMITED  BY  SIZE
                                       INTO  SPA-GMN27-KOHSEIDO1(IDY)
                       END-STRING
                   END-IF
                   MOVE    SPA-PTHKN-MAX   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       3201-RSI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC            SECTION.
      *
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN27-CUR
               WHEN    01
                   MOVE    "P027_SELNUM"   TO  MCP-WIDGET
               WHEN    02
                   MOVE    "BCOB2"         TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    [前↑]押下
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 440-ZEN-GMNHYOUJI-SEC
      *    [次↓]押下
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 450-TUGI-GMNHYOUJI-SEC
      *    保険チェック
               WHEN    "CLICKED"       ALSO    "BHO1"
               WHEN    "CLICKED"       ALSO    "BHO2"
               WHEN    "CLICKED"       ALSO    "BHO3"
               WHEN    "CLICKED"       ALSO    "BHO4"
               WHEN    "CLICKED"       ALSO    "BHO5"
                   PERFORM 450-HKNKAKUNINYMD-ZUMI-SEC
      *    公費チェック
               WHEN    "CLICKED"       ALSO    "BKH1"
               WHEN    "CLICKED"       ALSO    "BKH2"
               WHEN    "CLICKED"       ALSO    "BKH3"
               WHEN    "CLICKED"       ALSO    "BKH4"
               WHEN    "CLICKED"       ALSO    "BKH5"
                   PERFORM 460-KOHKAKUNINYMD-ZUMI-SEC
      *    登録処理
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    入院・外来切り替え
               WHEN    "CLICKED"       ALSO    "P027_NYUGAIKBN"
                   PERFORM 400-NYUGAI-SYORI-SEC
      *    表示切り替え
               WHEN    "CLICKED"       ALSO    "P027_HYOJIKBN"
                   PERFORM 400-HYOJIKBN-SYORI-SEC
      *     以外はエラー
               WHEN    "CLICKED"       ALSO     ANY
                   MOVE    "0110"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN27-CUR
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前表示（組合せ情報）処理
      *****************************************************************
       440-ZEN-GMNHYOUJI-SEC           SECTION.
      *
      *    組合せ情報
           IF  SPA-P027-PG             >   1
               COMPUTE   SPA-P027-PG   =   SPA-P027-PG   -   1
           END-IF
      *
      *    パラメタより編集
           PERFORM 320-PARAFILE-KENSAKU-SEC
      *
      *    保険
           IF      SPA-GMN27-HKN-PCNT  >   1
               COMPUTE SPA-GMN27-HKN-PCNT  =   SPA-GMN27-HKN-PCNT
                                           -   1
           END-IF
      *
      *    公費
           IF      SPA-GMN27-KOH-PCNT  >   1
               COMPUTE SPA-GMN27-KOH-PCNT  =   SPA-GMN27-KOH-PCNT
                                           -   1
           END-IF
      *
           .
       440-ZEN-GMNHYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次表示（組合せ情報）処理
      *****************************************************************
       450-TUGI-GMNHYOUJI-SEC          SECTION.
      *
      *    組合せ情報
      *    IF  SPA-P027-PG             <   SPA-P027-PGMAX
               ADD     1               TO  SPA-P027-PG
      *    END-IF
      *
      *    パラメタより編集
           PERFORM 320-PARAFILE-KENSAKU-SEC
      *
      *    保険
           IF      SPA-GMN27-HKN-PCNT  <   8
               COMPUTE IDX             =   SPA-GMN27-HKN-PCNT  +  1
               IF      SPA-GMN27-HKNRENNUM (IDX 1)  NOT =   ZERO
                   MOVE    IDX             TO  SPA-GMN27-HKN-PCNT
               END-IF
           END-IF
      *
      *    公費
           IF      SPA-GMN27-KOH-PCNT  <   8
               COMPUTE IDX             =   SPA-GMN27-KOH-PCNT  +  1
               IF      SPA-GMN27-KOHRENNUM (IDX 1)  NOT =   ZERO
                   MOVE    IDX             TO  SPA-GMN27-KOH-PCNT
               END-IF
           END-IF
      *
           .
       450-TUGI-GMNHYOUJI-EXT.
           EXIT.
      *
      ***************************************************************
      *    保険確認年月日入力済み処理
      ***************************************************************
       450-HKNKAKUNINYMD-ZUMI-SEC         SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "BHO1"
                   MOVE    1              TO  IDX
               WHEN    "BHO2"
                   MOVE    2              TO  IDX
               WHEN    "BHO3"
                   MOVE    3              TO  IDX
               WHEN    "BHO4"
                   MOVE    4              TO  IDX
               WHEN    "BHO5"
                   MOVE    5              TO  IDX
           END-EVALUATE
      *
           MOVE    SPA-GMN27-HKNRENNUM(SPA-GMN27-HKN-PCNT IDX)
                                       TO  IDY
           IF      IDY                 >   ZERO
      *        労災保険の時、セットしない
               IF      SPA-PTHKN-HKNNUM (IDY) =   "971"
                                              OR  "973"
                   CONTINUE
               ELSE
                   IF     (SPA-SYSYMD      =  SPA-PTHKN-KAKUNINYMD(IDY))
                      AND (SPA-PTHKN-MAEKAKUNINYMD(IDY) NOT =  SPACE)
                       MOVE    SPA-PTHKN-MAEKAKUNINYMD(IDY)
                                               TO  SPA-PTHKN-KAKUNINYMD
                                                               (IDY)
                       MOVE    SPA-PTHKN-MAEKAKUNINYMDW(IDY)
                                               TO  SPA-PTHKN-KAKUNINYMDW
                                                               (IDY)
                   ELSE
                       MOVE    SPA-SYSYMD      TO  SPA-PTHKN-KAKUNINYMD
                                                               (IDY)
                       MOVE    SPA-SYSYMDWH    TO  SPA-PTHKN-KAKUNINYMDW
                                                               (IDY)
                   END-IF
               END-IF
           END-IF
      *
           .
       450-HKNKAKUNINYMD-ZUMI-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費確認年月日入力済み処理
      *****************************************************************
       460-KOHKAKUNINYMD-ZUMI-SEC             SECTION.
      *
      *
           EVALUATE    MCP-WIDGET
               WHEN    "BKH1"
                   MOVE    1              TO  IDX
               WHEN    "BKH2"
                   MOVE    2              TO  IDX
               WHEN    "BKH3"
                   MOVE    3              TO  IDX
               WHEN    "BKH4"
                   MOVE    4              TO  IDX
               WHEN    "BKH5"
                   MOVE    5              TO  IDX
           END-EVALUATE
      *
           MOVE    SPA-GMN27-KOHRENNUM(SPA-GMN27-KOH-PCNT IDX)
                                           TO  IDY
           IF      IDY                 >   ZERO
               IF     (SPA-SYSYMD      =  SPA-PTKOH-KAKUNINYMD(IDY))
                  AND (SPA-PTKOH-MAEKAKUNINYMD(IDY) NOT =  SPACE)
                   MOVE    SPA-PTKOH-MAEKAKUNINYMD(IDY)
                                               TO  SPA-PTKOH-KAKUNINYMD
                                                               (IDY)
                   MOVE    SPA-PTKOH-MAEKAKUNINYMDW(IDY)
                                               TO  SPA-PTKOH-KAKUNINYMDW
                                                               (IDY)
               ELSE
                   MOVE    SPA-SYSYMD      TO  SPA-PTKOH-KAKUNINYMD(IDY)
                   MOVE    SPA-SYSYMDWH    TO  SPA-PTKOH-KAKUNINYMDW
                                                               (IDY)
               END-IF
           END-IF
           .
       460-KOHKAKUNINYMD-ZUMI-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタへ保存処理
      *****************************************************************
       500-SPA-PARA-HEN-SEC            SECTION.
      *
          .
       500-SPA-PARA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院・外来切り替え処理
      *****************************************************************
       400-NYUGAI-SYORI-SEC            SECTION.
      *
           IF      SPA-GMN27-NYUGAIKBN =   "1"
               MOVE    "2"             TO  SPA-GMN27-NYUGAIKBN
           ELSE
               MOVE    "1"             TO  SPA-GMN27-NYUGAIKBN
           END-IF
      *    表示内容変更
           IF      SPA-NAI27-SELNUM    NOT =   ZERO
               PERFORM 2109-HONCOMBI-HEN-SEC
           END-IF
      *
           .
       400-NYUGAI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    表示切り替え処理
      *****************************************************************
       400-HYOJIKBN-SYORI-SEC            SECTION.
      *
           IF      SPA-NAI27-SELNUM    NOT =   ZERO
               IF      SPA-GMN27-HYOJIKBN  =   SPACE
      *            受診履歴・病名等の算定チェック
                   IF      SPA-P02-SYORI       =   1
                       CONTINUE
                   ELSE
                       PERFORM 4001-HKNCOMBI-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "1"             TO  SPA-GMN27-HYOJIKBN
                   END-IF
               ELSE
                   MOVE    SPACE           TO  SPA-GMN27-HYOJIKBN
               END-IF
               IF      SPA-ERRCD           =   SPACE
      *        表示内容変更
                   MOVE    SPA-NAI27-IDX       TO  IDX
                   MOVE    SPA-GMN27-HYOJIKBN  TO  SPA-GMN27-THYOJIKBN
                                                                 (IDX)
                   MOVE    SPA-GMN27-HYOIDX (IDX)
                                               TO  IDX5
                   MOVE    SPA-GMN27-THYOJIKBN (IDX)
                                               TO  SPA-HYO-HYOJIKBN
                                                                 (IDX5)
               END-IF
           END-IF
      *
           .
       400-HYOJIKBN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴・病名等の算定チェック処理
      *****************************************************************
       4001-HKNCOMBI-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NAI27-SELNUM    TO  JYURRK-HKNCOMBINUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key41"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
               MOVE    "9103"              TO  SPA-ERRCD
           END-IF
      *
      *    病名チェック
           MOVE    SPACE               TO  PTBYOMEI-REC
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTBYO-PTID
           MOVE    SPA-NAI27-SELNUM    TO  PTBYO-HKNCOMBI
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 980-PTBYOMEI-READ-SEC
           ELSE
               MOVE    SPACE               TO  PTBYOMEI-REC
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTBYOMEI        =   ZERO
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "9104"          TO  SPA-ERRCD
               ELSE
                   MOVE    "9105"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4001-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                       SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "P027_SELNUM"
                   PERFORM 210-SELNUM-CHK-SEC
      *        選択番号
               WHEN    ANY             ALSO    "P027_HKNKUMI_LIST"
                   PERFORM 220-HKNKUMI-LIST-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号選択処理
      *****************************************************************
       210-SELNUM-CHK-SEC              SECTION.
      *
      *    パラメタファイル検索処理
      *DDD PERFORM 320-PARAFILE-KENSAKU-SEC
      *
           MOVE    P02-P027-SELNUM     TO  SPA-GMN27-SELNUM
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN27-SELNUM    TO  SNUM-INX
           CALL    "ORCSNUM"               USING
                                           ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "2701"          TO  SPA-ERRCD
               GO      TO              210-SELNUM-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI27-SELNUM
               MOVE    SPA-NAI27-SELNUM    TO  SPA-GMN27-SELNUM
           END-IF
      *
           IF  SPA-NAI27-SELNUM        =   SPACE  OR  ZERO
               MOVE    SPACE           TO  SPA-GMN27-SELNUM
               INITIALIZE                  SPA-GMN27-MEISAI-AREA
               MOVE    1               TO  SPA-GMN27-CUR
               GO  TO                  210-SELNUM-CHK-EXT
           END-IF
      *    選択番号　入力チェック
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  SPA-NAI27-IDX
           MOVE    ZERO                TO  SPA-NAI27-PARADENUM
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL   ( IDX         >   SPA-GMN27-MAX )
                     OR      ( IDY     NOT =   ZERO )
      ******** IF  LNK-COMB-HKNCOMBINUM (IDX)  =   SPA-NAI27-SELNUM
               IF      SPA-GMN27-NUM (IDX)     =   SPA-NAI27-SELNUM
                   MOVE    SPA-NAI27-EDANUM(IDX)
                                               TO  SPA-NAI27-PARADENUM
      *            非表示区分
                   MOVE    SPA-GMN27-THYOJIKBN (IDX)
                                               TO  SPA-GMN27-HYOJIKBN
                   MOVE    IDX                 TO  SPA-NAI27-IDX
               END-IF
           END-PERFORM
           IF       SPA-NAI27-IDX      =   ZERO
               MOVE    "2701"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN27-CUR
               GO  TO                  210-SELNUM-CHK-EXT
           END-IF
      *    保険組合せ編集
           PERFORM 2109-HONCOMBI-HEN-SEC
           .
       210-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せパラメタデータ処理
      *****************************************************************
       21091-PARAFILE-HEN-SEC            SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
           MOVE    SPA-NAI27-PARADENUM TO  PARA-EDANUM
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PARA            =   ZERO
               MOVE    PARA-DATA-REC       TO  HKNCOMB-REC
           ELSE
               INITIALIZE                      HKNCOMB-REC
           END-IF
      *    パラメタファイルより編集
           INITIALIZE                      LNK-HKNCOMB-AREA
           MOVE    HKNCOMB-REC         TO  LNK-HKNCOMB-REC (1)
           MOVE    SPA-NAI27-PARADENUM TO  LNK-COMB-EDANUM (1)
           .
       21091-PARAFILE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       2109-HONCOMBI-HEN-SEC            SECTION.
      *
      *    パラメタデータ読込み
           PERFORM 21091-PARAFILE-HEN-SEC
      *
      **** MOVE    SPA-NAI27-IDX       TO  IDX
           MOVE    1                   TO  IDX
      *
           INITIALIZE                  SPA-GMN27-MEISAI-AREA
           MOVE    LNK-COMB-HKNNUM     (IDX) TO  SPA-GMN27-HKNNUM
           MOVE    LNK-COMB-KOH1HKNNUM (IDX) TO  SPA-GMN27-KOH1HKNNUM
           MOVE    LNK-COMB-KOH2HKNNUM (IDX) TO  SPA-GMN27-KOH2HKNNUM
           MOVE    LNK-COMB-KOH3HKNNUM (IDX) TO  SPA-GMN27-KOH3HKNNUM
           MOVE    LNK-COMB-KOH4HKNNUM (IDX) TO  SPA-GMN27-KOH4HKNNUM
           MOVE    LNK-COMB-SYU-TANSEIDONAME (IDX)
                                       TO  SPA-GMN27-HKNSEIDON
           MOVE    LNK-COMB-KOH1-TANSEIDONAME(IDX)
                                       TO  SPA-GMN27-KOHSEIDO1N
           MOVE    LNK-COMB-KOH2-TANSEIDONAME(IDX)
                                       TO  SPA-GMN27-KOHSEIDO2N
           MOVE    LNK-COMB-KOH3-TANSEIDONAME(IDX)
                                       TO  SPA-GMN27-KOHSEIDO3N
           MOVE    LNK-COMB-KOH4-TANSEIDONAME(IDX)
                                       TO  SPA-GMN27-KOHSEIDO4N
      *----(01.00.01) LINE ADD START -------------------------------
      *    特殊負担
           MOVE    ZERO                TO  WRK-SBTA
           IF  ( LNK-COMB-KOH1-HKNKOHSBTKBN(IDX) =   "A" )  OR
               ( LNK-COMB-KOH2-HKNKOHSBTKBN(IDX) =   "A" )  OR
               ( LNK-COMB-KOH3-HKNKOHSBTKBN(IDX) =   "A" )  OR
               ( LNK-COMB-KOH4-HKNKOHSBTKBN(IDX) =   "A" )
               MOVE    1                     TO  WRK-SBTA
           END-IF
      *
           IF      SPA-GMN27-NYUGAIKBN     =   "1"
      *        入院
               PERFORM 21091-NYU-HEN-SEC
           ELSE
      *        外来
               PERFORM 21092-GAI-HEN-SEC
           END-IF
           .
       2109-HONCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理（入院）
      *****************************************************************
       21091-NYU-HEN-SEC            SECTION.
      *
           PERFORM VARYING    IDY    FROM    1   BY  1
                   UNTIL      IDY        >   3
             EVALUATE    IDY
             WHEN    1
                 MOVE    LNK-COMB-NYU-FTNKBN    (IDX)    TO  WRK-FTNKBN
             WHEN    2
                 MOVE    LNK-COMB-TSY-NYUFTNKBN (IDX)    TO  WRK-FTNKBN
             WHEN    3
                 MOVE    LNK-COMB-TNK-NYUFTNKBN (IDX)    TO  WRK-FTNKBN
             END-EVALUATE
             IF  ( WRK-SBTA                  =   1   )  AND
                 ( WRK-FTNKBN            NOT =   "2" )
                 MOVE    "特殊負担"      TO  SPA-GMN27-MEI-FTNKBN (IDY)
             ELSE
               EVALUATE    IDY
                   WHEN    1
                   MOVE    LNK-COMB-NYU-FTNKBN     (IDX)
                                               TO  WRK-FTNKBN
                   MOVE    LNK-COMB-NYU-SKJRYOKBN  (IDX)
                                               TO  WRK-FTNKBN2
                   MOVE    LNK-COMB-NYU-KFTNRATE   (IDX)
                                               TO  WRK-ZHEN-GAI-KFTNRATE
                   MOVE    LNK-COMB-NYU-KKOTGAK (IDX)
                                               TO  WRK-ZHEN-GAI-KKOTGAK
                   MOVE    LNK-COMB-NYU-KJGNGAK (IDX)
                                               TO  WRK-ZHEN-GAI-KJGNGAK
                   MOVE    LNK-COMB-NYU-DJGNGAK  (IDX)
                                               TO  WRK-ZHEN-GAI-DJGNGAK
                   MOVE    LNK-COMB-NYU-DJGNCNT  (IDX)
                                               TO  WRK-ZHEN-GAI-DJGNCNT
                   MOVE    LNK-COMB-NYU-MJGNGAK (IDX)
                                               TO
                                               WRK-ZHEN-GAI-MNAIJGNGAK
                   MOVE    LNK-COMB-NYU-MJGNCNT (IDX)
                                               TO  WRK-ZHEN-GAI-MJGNCNT
               WHEN    2
      *            低所得
                   MOVE    LNK-COMB-TSY-NYUFTNKBN    (IDX)
                                               TO  WRK-FTNKBN
                   MOVE    LNK-COMB-TSY-NYUSKJRYOKBN (IDX)
                                               TO  WRK-FTNKBN2
                   MOVE    LNK-COMB-TSY-NYUKFTNRATE  (IDX)
                                               TO  WRK-ZHEN-GAI-KFTNRATE
                   MOVE    LNK-COMB-TSY-NYUKKOTGAK(IDX)
                                               TO  WRK-ZHEN-GAI-KKOTGAK
                   MOVE    LNK-COMB-TSY-NYUKJGNGAK(IDX)
                                               TO  WRK-ZHEN-GAI-KJGNGAK
                   MOVE    LNK-COMB-TSY-NYUDJGNGAK (IDX) 
                                               TO  WRK-ZHEN-GAI-DJGNGAK
                   MOVE    LNK-COMB-TSY-NYUDJGNCNT (IDX)
                                               TO  WRK-ZHEN-GAI-DJGNCNT
                   MOVE    LNK-COMB-TSY-NYUMJGNGAK (IDX)
                                           TO  WRK-ZHEN-GAI-MNAIJGNGAK
                   MOVE    LNK-COMB-TSY-NYUMJGNCNT(IDX)
                                               TO  WRK-ZHEN-GAI-MJGNCNT
               WHEN    3
      *            低年金
                   MOVE    LNK-COMB-TNK-NYUFTNKBN    (IDX)
                                    TO  WRK-FTNKBN
                   MOVE    LNK-COMB-TNK-NYUSKJRYOKBN (IDX)
                                    TO  WRK-FTNKBN2
                   MOVE    LNK-COMB-TNK-NYUKFTNRATE  (IDX)
                                    TO  WRK-ZHEN-GAI-KFTNRATE
                   MOVE    LNK-COMB-TNK-NYUKKOTGAK(IDX)
                                    TO  WRK-ZHEN-GAI-KKOTGAK
                   MOVE    LNK-COMB-TNK-NYUKJGNGAK(IDX)
                                    TO  WRK-ZHEN-GAI-KJGNGAK
                   MOVE    LNK-COMB-TNK-NYUDJGNGAK (IDX)
                                    TO  WRK-ZHEN-GAI-DJGNGAK
                   MOVE    LNK-COMB-TNK-NYUDJGNCNT (IDX)
                                    TO  WRK-ZHEN-GAI-DJGNCNT
                   MOVE    LNK-COMB-TNK-NYUMJGNGAK (IDX)
                                    TO  WRK-ZHEN-GAI-MNAIJGNGAK
                   MOVE    LNK-COMB-TNK-NYUMJGNCNT(IDX)
                                    TO  WRK-ZHEN-GAI-MJGNCNT
               END-EVALUATE
      *
               MOVE    WRK-ZHEN-GAI-KFTNRATE
                                    TO  SPA-GMN27-MEI-KFTNRATE (IDY)
               MOVE    WRK-ZHEN-GAI-KKOTGAK
                                    TO  SPA-GMN27-MEI-KKOTGAK(IDY)
               MOVE    WRK-ZHEN-GAI-KJGNGAK
                                    TO  SPA-GMN27-MEI-KJGNGAK(IDY)
               MOVE    WRK-ZHEN-GAI-DJGNGAK
                                    TO  SPA-GMN27-MEI-DJGNGAK (IDY)
               MOVE    WRK-ZHEN-GAI-DJGNCNT
                                    TO  SPA-GMN27-MEI-DJGNCNT (IDY)
               MOVE    WRK-ZHEN-GAI-MNAIJGNGAK
                                    TO  SPA-GMN27-MEI-MNAIJGNGAK(IDY)
               MOVE    WRK-ZHEN-GAI-MJGNCNT
                                    TO  SPA-GMN27-MEI-MJGNCNT(IDY)
      *         MOVE    WRK-ZHEN-GAI-MGAIJGNGAK (IDX)
      *                              TO  SPA-GMN27-MEI-MGAIJGNGAK(IDY)
      *
      *        負担区分 0:使用しない / 1:患者負担有り / 2:患者負担無し
      *                 3:患者負担有り（固定額）
               EVALUATE    WRK-FTNKBN
               WHEN    "0"
                   MOVE    "不使用"   TO  SPA-GMN27-MEI-FTNKBN (IDY)
               WHEN    "1"
                   MOVE    "負担有り" TO  SPA-GMN27-MEI-FTNKBN (IDY)
               WHEN    "2"
                   MOVE    "負担無し" TO  SPA-GMN27-MEI-FTNKBN (IDY)
               WHEN    "3"
                   MOVE    "固定額"   TO  SPA-GMN27-MEI-FTNKBN (IDY)
               END-EVALUATE
      *        食事療養費   0:使用しない 　1:患者負担有り
      *                     2:患者負担有り（上限含む） / 3:患者負担無し
      *                     4:患者負担有り（消費税あり）
      *                     5:患者負担有り（消費税なし）
               EVALUATE    WRK-FTNKBN2
               WHEN    "0"
                   MOVE    "不使用"   TO  SPA-GMN27-MEI-FTNKBN2(IDY)
               WHEN    "1"
                   MOVE    "有り"     TO  SPA-GMN27-MEI-FTNKBN2(IDY)
               WHEN    "2"
                   MOVE  "有り(上限含む)"
                                      TO  SPA-GMN27-MEI-FTNKBN2(IDY)
               WHEN    "3"
                   MOVE  "無し"       TO  SPA-GMN27-MEI-FTNKBN2(IDY)
               WHEN    "4"
                   MOVE  "有り(消費税有)"
                                      TO  SPA-GMN27-MEI-FTNKBN2(IDY)
               WHEN    "5"
                   MOVE  "有り(消費税無)"
                                      TO  SPA-GMN27-MEI-FTNKBN2(IDY)
               END-EVALUATE
             END-IF
           END-PERFORM
           MOVE    LNK-COMB-TENTNK(IDX) TO  SPA-GMN27-TENTNK
           IF    LNK-COMB-TAXKBN  (IDX) =   "1"
               MOVE    "課税"           TO  SPA-GMN27-TAXKBN
           ELSE
               MOVE    "非課税"         TO  SPA-GMN27-TAXKBN
           END-IF
           .
       21091-NYU-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険組合せ編集処理（外来）
      *****************************************************************
       21092-GAI-HEN-SEC            SECTION.
      *
           PERFORM VARYING    IDY    FROM    1   BY  1
                   UNTIL      IDY        >   3
             EVALUATE    IDY
             WHEN    1
                 MOVE    LNK-COMB-GAI-FTNKBN    (IDX)    TO  WRK-FTNKBN
             WHEN    2
                 MOVE    LNK-COMB-TSY-GAIFTNKBN (IDX)    TO  WRK-FTNKBN
             WHEN    3
                 MOVE    LNK-COMB-TNK-GAIFTNKBN (IDX)    TO  WRK-FTNKBN
             END-EVALUATE
             IF  ( WRK-SBTA                  =   1   )  AND
                 ( WRK-FTNKBN            NOT =   "2" )
                 MOVE    "特殊負担"      TO  SPA-GMN27-MEI-FTNKBN (IDY)
             ELSE
      *----(01.00.01) LINE ADD END   -------------------------------
               EVALUATE    IDY
               WHEN    1
                   MOVE    LNK-COMB-GAI-FTNKBN     (IDX)
                                                 TO  WRK-FTNKBN
                   MOVE    LNK-COMB-GAI-YKZFTNKBN  (IDX)
                                                 TO  WRK-FTNKBN2
                   MOVE    LNK-COMB-GAI-KFTNRATE   (IDX)
                                    TO  WRK-ZHEN-GAI-KFTNRATE
                   MOVE    LNK-COMB-GAI-KKOTGAK (IDX)
                                    TO  WRK-ZHEN-GAI-KKOTGAK
                   MOVE    LNK-COMB-GAI-KJGNGAK (IDX)
                                    TO  WRK-ZHEN-GAI-KJGNGAK
                   MOVE    LNK-COMB-GAI-DJGNGAK  (IDX)
                                    TO  WRK-ZHEN-GAI-DJGNGAK
                   MOVE    LNK-COMB-GAI-DJGNCNT  (IDX)
                                    TO  WRK-ZHEN-GAI-DJGNCNT
                   MOVE    LNK-COMB-GAI-MNAIJGNGAK (IDX)
                                    TO  WRK-ZHEN-GAI-MNAIJGNGAK
                   MOVE    LNK-COMB-GAI-MJGNCNT (IDX)
                                    TO  WRK-ZHEN-GAI-MJGNCNT
      *             MOVE    LNK-COMB-GAI-MGAIJGNGAK (IDX)
      *                              TO  SPA-GMN27-MEI-MGAIJGNGAK(IDY)
               WHEN    2
                   MOVE    LNK-COMB-TSY-GAIFTNKBN    (IDX)
                                    TO  WRK-FTNKBN
                   MOVE    LNK-COMB-TSY-GAIYKZFTNKBN (IDX)
                                    TO  WRK-FTNKBN2
                   MOVE    LNK-COMB-TSY-GAIKFTNRATE  (IDX)
                                    TO  WRK-ZHEN-GAI-KFTNRATE
                   MOVE    LNK-COMB-TSY-GAIKKOTGAK(IDX)
                                    TO  WRK-ZHEN-GAI-KKOTGAK
                   MOVE    LNK-COMB-TSY-GAIKJGNGAK(IDX)
                                    TO  WRK-ZHEN-GAI-KJGNGAK
                   MOVE    LNK-COMB-TSY-GAIDJGNGAK (IDX) 
                                    TO  WRK-ZHEN-GAI-DJGNGAK
                   MOVE    LNK-COMB-TSY-GAIDJGNCNT (IDX)
                                    TO  WRK-ZHEN-GAI-DJGNCNT
                   MOVE    LNK-COMB-TSY-GAIMNAIJGNGAK(IDX)
                                    TO  WRK-ZHEN-GAI-MNAIJGNGAK
                   MOVE    LNK-COMB-TSY-GAIMJGNCNT(IDX)
                                    TO  WRK-ZHEN-GAI-MJGNCNT
      *             MOVE    LNK-COMB-TSY-GAIMGAIJGNGAK (IDX)
      *                              TO  SPA-GMN27-MEI-MGAIJGNGAK(IDY)
               WHEN    3
                   MOVE    LNK-COMB-TNK-GAIFTNKBN    (IDX)
                                    TO  WRK-FTNKBN
                   MOVE    LNK-COMB-TNK-GAIYKZFTNKBN (IDX)
                                    TO  WRK-FTNKBN2
                   MOVE    LNK-COMB-TNK-GAIKFTNRATE  (IDX)
                                    TO  WRK-ZHEN-GAI-KFTNRATE
                   MOVE    LNK-COMB-TNK-GAIKKOTGAK(IDX)
                                    TO  WRK-ZHEN-GAI-KKOTGAK
                   MOVE    LNK-COMB-TNK-GAIKJGNGAK(IDX)
                                    TO  WRK-ZHEN-GAI-KJGNGAK
                   MOVE    LNK-COMB-TNK-GAIDJGNGAK (IDX)
                                    TO  WRK-ZHEN-GAI-DJGNGAK
                   MOVE    LNK-COMB-TNK-GAIDJGNCNT (IDX)
                                    TO  WRK-ZHEN-GAI-DJGNCNT
                   MOVE    LNK-COMB-TNK-GAIMNAIJGNGAK(IDX)
                                    TO  WRK-ZHEN-GAI-MNAIJGNGAK
                   MOVE    LNK-COMB-TNK-GAIMJGNCNT(IDX)
                                    TO  WRK-ZHEN-GAI-MJGNCNT
      *             MOVE    LNK-COMB-TNK-GAIMGAIJGNGAK (IDX)
      *                              TO  SPA-GMN27-MEI-MGAIJGNGAK(IDY)
               END-EVALUATE
      *
               MOVE    WRK-ZHEN-GAI-KFTNRATE
                                    TO  SPA-GMN27-MEI-KFTNRATE  (IDY)
               MOVE    WRK-ZHEN-GAI-KKOTGAK
                                    TO  SPA-GMN27-MEI-KKOTGAK(IDY)
               MOVE    WRK-ZHEN-GAI-KJGNGAK
                                    TO  SPA-GMN27-MEI-KJGNGAK(IDY)
               MOVE    WRK-ZHEN-GAI-DJGNGAK
                                    TO  SPA-GMN27-MEI-DJGNGAK (IDY)
               MOVE    WRK-ZHEN-GAI-DJGNCNT
                                    TO  SPA-GMN27-MEI-DJGNCNT (IDY)
               MOVE    WRK-ZHEN-GAI-MNAIJGNGAK
                                    TO  SPA-GMN27-MEI-MNAIJGNGAK(IDY)
               MOVE    WRK-ZHEN-GAI-MJGNCNT
                                    TO  SPA-GMN27-MEI-MJGNCNT(IDY)
      *         MOVE    WRK-ZHEN-GAI-MGAIJGNGAK (IDX)
      *                              TO  SPA-GMN27-MEI-MGAIJGNGAK(IDY)
      *
      *        負担区分 0:使用しない / 1:患者負担有り / 2:患者負担無し
      *                 3:患者負担有り（固定額）
               EVALUATE    WRK-FTNKBN
               WHEN    "0"
                   MOVE    "不使用"   TO  SPA-GMN27-MEI-FTNKBN (IDY)
               WHEN    "1"
                   MOVE    "負担有り" TO  SPA-GMN27-MEI-FTNKBN (IDY)
               WHEN    "2"
                   MOVE    "負担無し" TO  SPA-GMN27-MEI-FTNKBN (IDY)
               WHEN    "3"
                   MOVE    "固定額"   TO  SPA-GMN27-MEI-FTNKBN (IDY)
               END-EVALUATE
      *        薬剤負担区分 0:使用しない 　/ 1:患者負担有り
      *                     2:患者負担有り（上限含む） / 3:患者負担無し
               EVALUATE    WRK-FTNKBN2
               WHEN    "0"
                   MOVE    "不使用"   TO  SPA-GMN27-MEI-FTNKBN2(IDY)
               WHEN    "1"
                   MOVE    "有り"     TO  SPA-GMN27-MEI-FTNKBN2(IDY)
               WHEN    "2"
                   MOVE  "有り(上限含む)"
                                      TO  SPA-GMN27-MEI-FTNKBN2(IDY)
               WHEN    "3"
                   MOVE  "無し"       TO  SPA-GMN27-MEI-FTNKBN2(IDY)
               END-EVALUATE
      *----(01.00.01) LINE ADD START -------------------------------
             END-IF
      *----(01.00.01) LINE ADD END   -------------------------------
           END-PERFORM
           MOVE    LNK-COMB-TENTNK(IDX) TO  SPA-GMN27-TENTNK
           IF    LNK-COMB-TAXKBN  (IDX) =   "1"
               MOVE    "課税"           TO  SPA-GMN27-TAXKBN
           ELSE
               MOVE    "非課税"         TO  SPA-GMN27-TAXKBN
           END-IF
           .
       21092-GAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ選択処理
      *****************************************************************
       220-HKNKUMI-LIST-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               IF      P02-P027-HKNKUMI-SELECT (IDX)  =   "T"
                   IF      SPA-NAI27-HKNNUM (IDX)
                                                =   SPA-NAI27-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    SPA-NAI27-HKNNUM (IDX)
                                                TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI27-SELNUM
           END-IF
           IF      SPA-NAI27-SELNUM   NOT =   ZERO
               MOVE     SPA-NAI27-SELNUM      TO   P02-P027-SELNUM
               PERFORM 210-SELNUM-CHK-SEC
           END-IF
      *
          .
       220-HKNKUMI-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
          .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       410112-SE-WAFORMAT-YMDEDIT-SEC             SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE           TO  WRK-HENYMD
               WHEN    ZERO
                   MOVE    "0000000"       TO  WRK-HENYMD
               WHEN    "99999999"
                   MOVE    "9999999"       TO  WRK-HENYMD
               WHEN    OTHER
      *
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1         =   ZERO
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   ELSE
                       MOVE    SPACE               TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
           .
       410112-SE-WAFORMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       980-PTBYOMEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       980-PTBYOMEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
