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
       PROGRAM-ID.             ORCBM022.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 社保・総括表用集計処理
      *                      ２００２．１０改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/09/19    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/11/07  船員保険の集計の追加 
      *  01.00.02    NACL-藤原    02/11/26  共済組合の下船３ヶ月の
      *                                     集計の追加 
      *  01.00.03    NACL-藤原    02/12/09  共済組合の前期高齢者の集計の
      *                                     修正
      *  01.00.04    NACL-藤原    02/12/09  入院対応
      *
      *  ２００３．４改正対応
      *  01.01.01    NACL-藤原    03/04/10  ２００３．３以前のみ対象とする
      *  01.01.02    NACL-藤原    03/07/10  特定疾患有り・無しの集計を
      *                                     同じ欄に編集する
      *  01.01.03    NACL-藤原    03/10/30  備考欄に老人再掲を追加
      *  01.01.04    NACL-藤原    04/12/10  備考欄の一部負担金等の記載
      *返戻分対応  
      *  01.02.01    NACL-藤原    05/02/25  返戻分対応
      *  01.02.03    NACL-藤原    05/03/22  備考欄の一部負担金の編集方法
      *                                     の変更
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *                                     ２００３年４月以降のみ作成
      *
      *  ２００６年４月改正対応
      *  02.09.01    NACL-藤原    06/04/13  ２００６年４月改正対応
      *  02.09.02    NACL-藤原    06/04/28  総括表（医科・入院外）のとき
      *                                     ２００６年３月以前を分けない
      *
      *  03.02.01    NACL-藤原    06/07/24  マル長の別総括対応
      *                                     MONFUNC対応
      *
      *  03.03.01    NACL-藤原    06/10/13  ２００６年１０月改正対応
      *  03.03.02    NACL-藤原    07/02/10  同一地方公費のまとめ集計
      *                                     岡山対応
      *  03.03.03    NACL-藤原    07/05/16  同一地方公費のまとめ集計
      *                                     岡山対応
      *                                    （公費番号の追加に伴う
      *
      *  03.04.01    NACL-藤原    07/08/20  同一地方公費のまとめ集計
      *                                     北海道対応
      *  03.04.02    NACL-藤原    07/09/11  新潟県中越沖地震対応
      *                                     ２００７年９月〜１２月対象 
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求ＤＢ更新用ファイル
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01). 
           03  FLG-ERR             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDXZ                PIC 9(04).
      *     
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *     
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(04).
           03  IDY4                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-NYUGAIKBN  PIC X(01).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-MARUCHO    PIC X(01).
           03  WRK-PARA-GENMENKBN  PIC 9(01).
      *          
      *    一時領域
       01  WRK-AREA.
           03  WRK-NYUGAIKBN       PIC X(01).
      *    1:200603 2:200604
           03  WRK-PRTFLG          PIC 9(01).
      *
           03  WRK-RECEERR         PIC X(200). 
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-INDEX           PIC 9(02).
           03  WRK-FTNMONEY        PIC 9(10).
           03  WRK-SYOKBN          PIC X(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
      *
      *    集計領域
       01  SUM-AREA.
         02  SUM-TABLE-OCC         OCCURS  100.
           03  SUM-TBL-SRYKA       PIC X(02).
      *    1:その他のレセ、2:在総診を算定したレセ 
           03  SUM-TBL-OCC         OCCURS  2.
      *        医療保険　1,2:７０以上 3:本人 4:家族 5:三歳 
               05  SUM-TABLE           OCCURS  5. 
                   07  SUM-TBL             OCCURS  12.
                       09  SUM-KENSU          PIC  9(06).
                       09  SUM-NISSU          PIC  9(06).
                       09  SUM-TOTALTEN       PIC  9(10).
                       09  SUM-FTNMONEY       PIC  9(10).
                       09  SUM-YKZFTN         PIC  9(10).
                       09  SUM-SHOKUJIKENSU   PIC  9(06).
                       09  SUM-SHOKUJINISSU   PIC  9(06).
                       09  SUM-SHOKUJIRYOYOHI PIC  9(10).
                       09  SUM-SHOKUJIFTN     PIC  9(10).
      *        老人保健
               05  SUM-ROU-TABLE           OCCURS  2. 
                   07  SUM-ROU-TBL             OCCURS  3.
                       09  SUM-ROU-KENSU          PIC  9(06).
                       09  SUM-ROU-NISSU          PIC  9(06).
                       09  SUM-ROU-TOTALTEN       PIC  9(10).
                       09  SUM-ROU-FTNMONEY       PIC  9(10).
                       09  SUM-ROU-SHOKUJIKENSU   PIC  9(06).
                       09  SUM-ROU-SHOKUJINISSU   PIC  9(06).
                       09  SUM-ROU-SHOKUJIRYOYOHI PIC  9(10).
                       09  SUM-ROU-SHOKUJIFTN     PIC  9(10).
               05  SUM-ROU-GOKEI.
                   07  SUM-ROU-GOK-NISSU          PIC  9(06).
                   07  SUM-ROU-GOK-TOTALTEN       PIC  9(10).
                   07  SUM-ROU-GOK-FTNMONEY       PIC  9(10).
                   07  SUM-ROU-GOK-SHOKUJINISSU   PIC  9(06).
                   07  SUM-ROU-GOK-SHOKUJIRYOYOHI PIC  9(10).
                   07  SUM-ROU-GOK-SHOKUJIFTN     PIC  9(10).
      *        公費負担 
               05  SUM-KOH-TABLE           OCCURS  3. 
                   07  SUM-KOH-TBL         OCCURS  100.
                       09  SUM-KOH-KOHNUM         PIC  X(03).
                       09  SUM-KOH-KENSU          PIC  9(06).
                       09  SUM-KOH-TOTALTEN       PIC  9(10).
                       09  SUM-KOH-FTNMONEY       PIC  9(10).
                       09  SUM-KOH-YKZFTN         PIC  9(10).
                       09  SUM-KOH-SHOKUJIKENSU   PIC  9(06).
                       09  SUM-KOH-SHOKUJIRYOYOHI PIC  9(10).
                       09  SUM-KOH-SHOKUJIFTN     PIC  9(10).
      *        老人保健（再掲）      
               05  SUM-ROU-KOH-TABLE           OCCURS  10. 
                   07  SUM-ROU-KOH-TBL.
                       09  SUM-ROU-KOH-KENSU          PIC  9(06).
                       09  SUM-ROU-KOH-NISSU          PIC  9(06).
                       09  SUM-ROU-KOH-ITBFTN         PIC  9(08).
                       09  SUM-ROU-KOH-FTNMONEY       PIC  9(09).
                       09  SUM-ROU-KOH-SHOKUJIKENSU   PIC  9(06).
                       09  SUM-ROU-KOH-SHOKUJINISSU   PIC  9(06).
                       09  SUM-ROU-KOH-SHOKUJIRYOYOHI PIC  9(10).
                       09  SUM-ROU-KOH-SHOKUJIFTN     PIC  9(10).
      *        合計         
               05  SUM-GOK-TBL         OCCURS  4.
                   07  SUM-GOK-KENSU      PIC 9(06).    
                   07  SUM-GOK-SHOKENSU   PIC 9(06).
      *
      *    固定項目
       01  WRK-CONS-AREA.
      *
      *    ２００２年１０月改正
           03  WRK-CONS-SRYYM-200210   PIC X(06)   VALUE   "200210".
      *    ２００３年４月改正
           03  WRK-CONS-SRYYM-200304   PIC X(06)   VALUE   "200304".
      *    ２００６年４月改正
           03  WRK-CONS-SRYYM-200604   PIC X(06)   VALUE   "200604".
      *    ２００６年１０月改正
           03  WRK-CONS-SRYYM-200610   PIC X(06)   VALUE   "200610".
      *     
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK2009.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
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
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
           INITIALIZE              STS-AREA
           INITIALIZE              WRK-AREA
           INITIALIZE              CNT-AREA
           INITIALIZE              SUM-AREA
           INITIALIZE              WRK-PARA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID    
                                               LNK-PRTKANRI-PRTNM    
                                               WRK-PARA-HOSPID
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-MARUCHO 
           END-UNSTRING
      *    新潟県中越沖地震対応
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  WRK-PARA-GENMENKBN
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBM022"      TO  JOB-PGID
           IF      WRK-PARA-GENMENKBN  =   ZERO 
               MOVE    "社保総括表（改正版）"
                                   TO  JOB-SHELLMSG
           ELSE
               MOVE    "未確定分・社保総括表（改正版）"
                                   TO  JOB-SHELLMSG
           END-IF
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *
      *    システム管理マスタ情報取得
           PERFORM 110-SYSKANRI-GET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ情報取得
      *****************************************************************
       110-SYSKANRI-GET-SEC            SECTION.
      *
      *    医療機関情報
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF 
      *         
      *    レセプト編集区分情報
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-2009-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD (7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2009-REC
           ELSE    
               INITIALIZE                  SYS-2009-REC
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-2009-SYUKA-KBN
           END-IF
           .
       110-SYSKANRI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT              UPD-FILE
      *
      *    返戻分
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    ZERO
               WHEN    "2"
                   MOVE    "2"         TO  WRK-SYOKBN
                   PERFORM 2001-SYOKBN-SYUKEI-SEC
           END-EVALUATE
      *    当月・月遅れ分
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    ZERO
               WHEN    "1"
                   MOVE    "1"         TO  WRK-SYOKBN
                   PERFORM 2001-SYOKBN-SYUKEI-SEC
           END-EVALUATE
      *
           CLOSE   UPD-FILE
      *
          IF      FLG-UPD        =   1
          AND     FLG-ERR        =   ZERO 
              IF      CNT-UPDFILE    >   ZERO
                  PERFORM 310-SEIKYU-UPD-SEC
              END-IF    
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYOKBN-SYUKEI-SEC         SECTION.
      *
      *    入院外分
      *
           MOVE    "2"                 TO  WRK-NYUGAIKBN
      *
      *    1:-200603 2:200604-200609 3:200610-   
           PERFORM VARYING WRK-PRTFLG  FROM    1   BY  1
                   UNTIL   WRK-PRTFLG  >       3
      *
               IF    ( WRK-PRTFLG          =   3                     )
               AND   ( LNK-PRTKANRI-SRYYM  <   WRK-CONS-SRYYM-200610 )
                   CONTINUE
               ELSE
                   IF    ( WRK-PRTFLG          >=  2                 )
                   AND   ( LNK-PRTKANRI-SRYYM  <
                                               WRK-CONS-SRYYM-200604 )
                       CONTINUE
                   ELSE
                       IF    ( WRK-PRTFLG          =   1   )
                       AND   ( LNK-PRTKANRI-SRYYM  >=
                                               WRK-CONS-SRYYM-200604 )
                           CONTINUE
                       ELSE
                           PERFORM 2001-GAIRAI-SYUKEI-SEC
      *
                           IF      CNT-RECE10          >   ZERO 
                               IF      WRK-PRTFLG          <   3
                                   IF      WRK-PARA-NYUGAIKBN  =   "1"
                                       CALL    "ORCBM028"  USING 
                                                           SUM-AREA
                                                           WRK-PARA
                                                           WRK-NYUGAIKBN
                                                           WRK-SYOKBN
                                                           WRK-PRTFLG
                                   ELSE
                                       CALL    "ORCBM027"  USING 
                                                           SUM-AREA
                                                           WRK-PARA
                                                           WRK-SYOKBN
                                                           WRK-PRTFLG
                                   END-IF
                               ELSE
                                   IF      WRK-PARA-NYUGAIKBN  =   "1"
                                       CALL    "ORCBM033"  USING 
                                                           SUM-AREA
                                                           WRK-PARA
                                                           WRK-NYUGAIKBN
                                                           WRK-SYOKBN
                                                           WRK-PRTFLG
                                   ELSE
                                       CALL    "ORCBM032"  USING 
                                                           SUM-AREA
                                                           WRK-PARA
                                                           WRK-SYOKBN
                                                           WRK-PRTFLG
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF    
           END-PERFORM    
      *
      *    入院分
      *
           MOVE    "1"                 TO  WRK-NYUGAIKBN
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1"  )
           AND   ( FLG-ERR             =   ZERO )
               PERFORM VARYING WRK-PRTFLG  FROM    1   BY  1
                       UNTIL   WRK-PRTFLG  >       3
                   IF    ( WRK-PRTFLG          =   3                 )
                   AND   ( LNK-PRTKANRI-SRYYM  <
                                               WRK-CONS-SRYYM-200610 )
                       CONTINUE
                   ELSE
                       IF    ( WRK-PRTFLG          >=  2             )
                       AND   ( LNK-PRTKANRI-SRYYM  <
                                               WRK-CONS-SRYYM-200604 )
                           CONTINUE
                       ELSE
                           PERFORM 2001-NYUIN-SYUKEI-SEC
      *
                           IF      CNT-RECE10          >   ZERO 
                               IF      WRK-PRTFLG          <   3
                                   CALL    "ORCBM028"      USING 
                                                           SUM-AREA
                                                           WRK-PARA
                                                           WRK-NYUGAIKBN
                                                           WRK-SYOKBN
                                                           WRK-PRTFLG
                               ELSE
                                   CALL    "ORCBM033"      USING 
                                                           SUM-AREA
                                                           WRK-PARA
                                                           WRK-NYUGAIKBN
                                                           WRK-SYOKBN
                                                           WRK-PRTFLG
                               END-IF
                           END-IF
                       END-IF 
                   END-IF
               END-PERFORM
           END-IF
           .
       2001-SYOKBN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院外分集計処理
      *****************************************************************
       2001-GAIRAI-SYUKEI-SEC         SECTION.
      *
           INITIALIZE                      SUM-AREA
           MOVE    ZERO                TO  CNT-RECE10
      *
      *    その他のレセ
           EVALUATE    WRK-SYOKBN  ALSO    WRK-PARA-MARUCHO
               WHEN    "1"         ALSO    "0"
                   MOVE    "key28"                  TO  WRK-MCP-PATHNAME
               WHEN    "2"         ALSO    "0"
                   MOVE    "key27"                  TO  WRK-MCP-PATHNAME
               WHEN    "1"         ALSO    "1"
                   MOVE    "key46"                  TO  WRK-MCP-PATHNAME
               WHEN    "2"         ALSO    "1"
                   MOVE    "key45"                  TO  WRK-MCP-PATHNAME
               WHEN    "1"         ALSO    "2"
                   MOVE    "key52"                  TO  WRK-MCP-PATHNAME
               WHEN    "2"         ALSO    "2"
                   MOVE    "key51"                  TO  WRK-MCP-PATHNAME
           END-EVALUATE  
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           MOVE    1                   TO  WRK-INDEX
           PERFORM 2001-SYUKEI-SEC     UNTIL   FLG-RECE10   =   1 
                                       OR      FLG-ERR      =   1
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-ERR             =   ZERO
      *        在総診または在医総を算定したレセ
               EVALUATE    WRK-SYOKBN  ALSO    WRK-PARA-MARUCHO
                   WHEN    "1"         ALSO    "0"
                       MOVE    "key30"             TO  WRK-MCP-PATHNAME
                   WHEN    "2"         ALSO    "0"
                       MOVE    "key29"             TO  WRK-MCP-PATHNAME
                   WHEN    "1"         ALSO    "1"
                       MOVE    "key48"             TO  WRK-MCP-PATHNAME
                   WHEN    "2"         ALSO    "1"
                       MOVE    "key47"             TO  WRK-MCP-PATHNAME
                   WHEN    "1"         ALSO    "2"
                       MOVE    "key54"             TO  WRK-MCP-PATHNAME
                   WHEN    "2"         ALSO    "2"
                       MOVE    "key53"             TO  WRK-MCP-PATHNAME
               END-EVALUATE  
               MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
               PERFORM 900-SEIKYU-SELECT-SEC
      *
               MOVE    2                   TO  WRK-INDEX
               PERFORM 2001-SYUKEI-SEC     UNTIL   FLG-RECE10   =   1 
                                           OR      FLG-ERR      =   1
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       2001-GAIRAI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院分集計処理
      *****************************************************************
       2001-NYUIN-SYUKEI-SEC         SECTION.
      *
           INITIALIZE                      SUM-AREA
           MOVE    ZERO                TO  CNT-RECE10
      *
           EVALUATE    WRK-SYOKBN  ALSO    WRK-PARA-MARUCHO
               WHEN    "1"         ALSO    "0"
                   MOVE    "key32"             TO  WRK-MCP-PATHNAME
               WHEN    "2"         ALSO    "0"
                   MOVE    "key31"             TO  WRK-MCP-PATHNAME
               WHEN    "1"         ALSO    "1"
                   MOVE    "key50"             TO  WRK-MCP-PATHNAME
               WHEN    "2"         ALSO    "1"
                   MOVE    "key49"             TO  WRK-MCP-PATHNAME
               WHEN    "1"         ALSO    "2"
                   MOVE    "key56"             TO  WRK-MCP-PATHNAME
               WHEN    "2"         ALSO    "2"
                   MOVE    "key55"             TO  WRK-MCP-PATHNAME
           END-EVALUATE  
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           MOVE    1                   TO  WRK-INDEX
           PERFORM 2001-SYUKEI-SEC     UNTIL   FLG-RECE10   =   1 
                                       OR      FLG-ERR      =   1
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2001-NYUIN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      *
      *
           MOVE    ZERO                TO  IDX-AREA
                                           FLG-UPD
      *
           MOVE    WRK-INDEX           TO  IDY3
      *
      *    主科別
           IF      RECE10-RECEKA       =   "00"
               MOVE    RECE10-RECEKA   TO  SUM-TBL-SRYKA (1)
               MOVE    1                   TO  IDXX
           ELSE
               PERFORM VARYING IDXY    FROM    2   BY  1
                       UNTIL   IDXY    >       99
                   IF      SUM-TBL-SRYKA (IDXY)    =   SPACE
                       MOVE    RECE10-RECEKA   TO  SUM-TBL-SRYKA (IDXY)
                   END-IF 
                   IF      SUM-TBL-SRYKA (IDXY)    =   RECE10-RECEKA
                       MOVE    IDXY            TO  IDXX
                       MOVE    99              TO  IDXY
                   END-IF
               END-PERFORM
           END-IF        
      *
      *    老人保健欄
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               EVALUATE    RECE10-KYURATE
                   WHEN    9
                       MOVE    1                   TO  IDY1
                   WHEN    8
                   WHEN    7
                       MOVE    2                   TO  IDY1
               END-EVALUATE        
               IF      RECE10-KOHNUM (1)   NOT =   SPACE
                   MOVE    1                   TO  IDY2 
               ELSE
                   MOVE    2                   TO  IDY2
               END-IF
               IF      IDXX            >   ZERO
                   MOVE    IDY1            TO  IDXZ
                   MOVE    IDXX            TO  IDY4
                   PERFORM 210-ROU-SYUKEI-SEC
                   MOVE    IDXZ            TO  IDY1
                   MOVE    100             TO  IDY4
                   PERFORM 210-ROU-SYUKEI-SEC
               END-IF
           END-IF
      *     
      *    医療保険欄
           IF    ( RECE10-RJNHKNNUM        =   SPACE )
           AND   ( RECE10-HKNNUM       NOT =   SPACE )
      *      
      *        船員保険の補助区分が前期高齢者以外の時
         
               IF    ( RECE10-HKNNUM           =   "002"               )
               AND   ( RECE10-HOJOKBN          =   "1" OR  "2" OR  "3"
                                                       OR  SPACE       )
                   IF      RECE10-AGE              <   3
                       MOVE    5                       TO  IDY1
                   ELSE                    
                       IF      RECE10-HONKZKKBN        =   "1"
                          MOVE    3                       TO  IDY1
                       ELSE     
                          MOVE    4                       TO  IDY1
                       END-IF
                   END-IF
               ELSE    
      *      
      *            共済組合の補助区分が前期高齢者以外の時     
                   IF    (  RECE10-HKNNUM          >=  "031"   AND
                                                   <=  "034"         )
                   AND   (  RECE10-HOJOKBN         =   "2" OR  SPACE ) 
                       IF      RECE10-AGE              <   3
                           MOVE    5                       TO  IDY1
                       ELSE                    
                           IF      RECE10-HONKZKKBN        =   "1"
                               MOVE    3                       TO  IDY1
                           ELSE     
                               MOVE    4                       TO  IDY1
                           END-IF
                       END-IF
                   ELSE    
                       IF      RECE10-AGE          >=  70
      *                    船員保険の補助区分が前期高齢者の時
                           IF      RECE10-HKNNUM           =   "002"
                               EVALUATE    RECE10-HOJOKBN
                                   WHEN    "9"
                                   WHEN    "A"
                                   WHEN    "B"
                                   WHEN    "C"
                                       MOVE    1               TO  IDY1
                                   WHEN    "8"    
                                   WHEN    "D"
                                   WHEN    "E"
                                   WHEN    "F"
                                   WHEN    "7"    
                                   WHEN    "G"
                                   WHEN    "H"
                                   WHEN    "I"
                                       MOVE    2               TO  IDY1
                               END-EVALUATE
                           ELSE            
      *                        共済組合の補助区分が前期高齢者の時
                               IF      RECE10-HKNNUM   >=  "031"   AND
                                                       <=  "034"
                                   EVALUATE    RECE10-HOJOKBN
                                       WHEN    "9"
                                       WHEN    "B"
                                           MOVE    1           TO  IDY1
                                       WHEN    "8"
                                       WHEN    "E"
                                       WHEN    "7"
                                       WHEN    "H"
                                           MOVE    2           TO  IDY1
                                   END-EVALUATE
                               ELSE            
                                   EVALUATE    RECE10-KYURATE
                                       WHEN    9
                                           MOVE    1           TO  IDY1
                                       WHEN    8
                                       WHEN    7
                                           MOVE    2           TO  IDY1
                                   END-EVALUATE
                               END-IF
                           END-IF
                       ELSE
                           IF      RECE10-AGE              <   3
                               MOVE    5                       TO  IDY1
                           ELSE                    
                               IF      RECE10-HONKZKKBN    =   "1"
                                   MOVE    3                   TO  IDY1
                               ELSE     
                                   MOVE    4                   TO  IDY1
                               END-IF
                           END-IF
                       END-IF
                   END-IF        
               END-IF
      *                      
               IF      RECE10-KOHNUM (1)   NOT =   SPACE
                   MOVE    1                   TO  IDY2 
               ELSE
                   EVALUATE    IDY1
                       WHEN    1
                           EVALUATE    RECE10-HKNNUM
                               WHEN    "001"
                                   MOVE    2               TO  IDY2
                               WHEN    "002"
                                   EVALUATE    RECE10-HOJOKBN
                                       WHEN    "A"
                                       WHEN    "B"
                                       WHEN    "C"
                                           MOVE    3       TO  IDY2
                                       WHEN    "9"
                                           MOVE    4       TO  IDY2
                                   END-EVALUATE        
                               WHEN    "003"
                                   MOVE    5               TO  IDY2
                               WHEN    "004"
                                   MOVE    6               TO  IDY2
                               WHEN    "031"  THRU  "034"
                                   EVALUATE    RECE10-HOJOKBN
                                       WHEN    "B"
                                           MOVE    7       TO  IDY2
                                       WHEN    OTHER
                                           MOVE    8       TO  IDY2
                                   END-EVALUATE        
                               WHEN    "006"
                                   MOVE    9               TO  IDY2
                               WHEN    "063"
                               WHEN    "072"  THRU  "075"
                                   MOVE    10              TO  IDY2
                           END-EVALUATE
                       WHEN    2
                           EVALUATE    RECE10-HKNNUM
                               WHEN    "001"
                                   MOVE    2               TO  IDY2
                               WHEN    "002"    
                                   EVALUATE    RECE10-HOJOKBN
                                       WHEN    "D"
                                       WHEN    "E"
                                       WHEN    "F"
                                       WHEN    "G"
                                       WHEN    "H"
                                       WHEN    "I"
                                           MOVE    3       TO  IDY2
                                       WHEN    "8"
                                       WHEN    "7"
                                           MOVE    4       TO  IDY2
                                   END-EVALUATE        
                               WHEN    "031"  THRU  "034"
                                   EVALUATE    RECE10-HOJOKBN
                                       WHEN    "E"
                                       WHEN    "H"
                                           MOVE    5       TO  IDY2
                                       WHEN    OTHER
                                           MOVE    6       TO  IDY2
                                   END-EVALUATE        
                               WHEN    "006"
                                   MOVE    7               TO  IDY2
                               WHEN    "063"
                               WHEN    "072"  THRU  "075"
                                   MOVE    8               TO  IDY2
                           END-EVALUATE
                       WHEN    3
                           EVALUATE    RECE10-HKNNUM
                               WHEN    "001"
                                   MOVE    2               TO  IDY2
                               WHEN    "002"    
                                   IF      RECE10-HOJOKBN  =   "1" OR
                                                               "2" OR
                                                               "3"
                                       MOVE    3               TO  IDY2
                                   ELSE
                                       IF      RECE10-HOJOKBN  =   SPACE
                                           MOVE    4           TO  IDY2
                                       END-IF    
                                   END-IF        
                               WHEN    "003"
                                   MOVE    5               TO  IDY2
                               WHEN    "004"
                                   MOVE    6               TO  IDY2
                               WHEN    "031"  THRU  "034"
                                   EVALUATE    RECE10-HOJOKBN
                                       WHEN    "2"
                                           MOVE    7       TO  IDY2
                                       WHEN    OTHER
                                           MOVE    8       TO  IDY2
                                   END-EVALUATE        
                               WHEN    "006"
                                   MOVE    9               TO  IDY2
                               WHEN    "007"
                                   MOVE    10              TO  IDY2
                               WHEN    "063"
                               WHEN    "072"  THRU  "075"
                                   MOVE    11              TO  IDY2
                           END-EVALUATE
                       WHEN    4
                       WHEN    5
                           EVALUATE    RECE10-HKNNUM
                               WHEN    "001"
                                   MOVE    2               TO  IDY2
                               WHEN    "002"
                                   MOVE    3               TO  IDY2
                               WHEN    "003"
                                   MOVE    4               TO  IDY2
                               WHEN    "004"
                                   MOVE    5               TO  IDY2
                               WHEN    "031"  THRU  "034"
                                   MOVE    6               TO  IDY2
                               WHEN    "006"
                                   MOVE    7               TO  IDY2
                               WHEN    "063"
                               WHEN    "072"  THRU  "075"
                                   MOVE    8               TO  IDY2
                           END-EVALUATE
                   END-EVALUATE        
               END-IF
               IF      IDXX            >   ZERO
                   MOVE    IDXX            TO  IDY4
                   PERFORM 210-HKN-SYUKEI-SEC
                   MOVE    100             TO  IDY4
                   PERFORM 210-HKN-SYUKEI-SEC
               END-IF
           END-IF    
      *
      *    公費負担分     
           IF      RECE10-KOHNUM (1)   NOT =   SPACE
               IF    ( RECE10-RJNHKNNUM    NOT =   SPACE )
               OR    ( RECE10-HKNNUM       NOT =   SPACE )
      *            公費と医保・老人の併用欄
                   MOVE    1               TO  IDX1
                   IF      IDXX            >   ZERO
                       MOVE    IDXX            TO  IDY4
                       PERFORM 220-KOH-SYUKEI-SEC
                       MOVE    100              TO  IDY4
                       PERFORM 220-KOH-SYUKEI-SEC
                   END-IF
               ELSE
                   IF      RECE10-KOHNUM (2)   NOT =   SPACE  
      *            公費と公費の併用欄
                       MOVE    2               TO  IDX1
                       IF      IDXX            >   ZERO
                           MOVE    IDXX            TO  IDY4
                           PERFORM 220-KOH-SYUKEI-SEC
                           MOVE    100             TO  IDY4
                           PERFORM 220-KOH-SYUKEI-SEC
                       END-IF
                   ELSE
      *            公費単独欄
                       MOVE    3               TO  IDX1
                       IF      IDXX            >   ZERO
                           MOVE    IDXX            TO  IDY4
                           PERFORM 220-KOH-SYUKEI-SEC
                           MOVE    100             TO  IDY4
                           PERFORM 220-KOH-SYUKEI-SEC
                       END-IF
                   END-IF
               END-IF     
           END-IF                
      *
      *    集計の対象にならなかったとき                
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               DISPLAY "*** ORCBM022 ERR KEY=" RECE10-KEY
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "集計対象外　患者番号＝"     DELIMITED  BY  SIZE
                       RECE10-PTNUM                DELIMITED  BY  SPACE
                       " 診療年月＝"               DELIMITED  BY  SIZE
                       RECE10-SRYYM                DELIMITED  BY  SIZE
                       " レセ種別＝"               DELIMITED  BY  SIZE
                       RECE10-RECESYUBETU          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING                                
               PERFORM 500-ERR-HENSYU-SEC
           END-IF 
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人保健分集計処理
      *****************************************************************
       210-ROU-SYUKEI-SEC          SECTION.
      *
           IF      IDY1            =   ZERO
           OR      IDY2            =   ZERO
               GO  TO  210-ROU-SYUKEI-EXT
           END-IF
      *     
           MOVE    1                         TO  IDX1
      *
      *    件数
           ADD     1           TO  SUM-ROU-KENSU (IDY4 IDY3 IDY1 IDY2)
           IF      RECE10-SHOKUJINISSU(IDX1)   >   ZERO
               ADD     1       TO  SUM-ROU-SHOKUJIKENSU
                                                 (IDY4 IDY3 IDY1 IDY2)
           END-IF
      *    （２）合計、総件数
           ADD     1                   TO  SUM-GOK-KENSU (IDY4 IDY3 2)
                                           SUM-GOK-KENSU (IDY4 IDY3 4)
           IF      RECE10-SHOKUJINISSU(IDX1)   >   ZERO
               ADD     1               TO
                                       SUM-GOK-SHOKENSU (IDY4 IDY3 2)
                                       SUM-GOK-SHOKENSU (IDY4 IDY3 4) 
           END-IF
      *
      *    診療実日数〜一部負担金 
           ADD     RECE10-JNISSU         (IDX1)
                         TO  SUM-ROU-NISSU         (IDY4 IDY3 IDY1 IDY2)
                             SUM-ROU-GOK-NISSU     (IDY4 IDY3)
           ADD     RECE10-TOTALTEN       (IDX1)
                         TO  SUM-ROU-TOTALTEN      (IDY4 IDY3 IDY1 IDY2)
                             SUM-ROU-GOK-TOTALTEN  (IDY4 IDY3)
           ADD     RECE10-FTNMONEY       (IDX1)
                         TO  SUM-ROU-FTNMONEY      (IDY4 IDY3 IDY1 IDY2)
                             SUM-ROU-GOK-FTNMONEY  (IDY4 IDY3)
      *                           
           ADD     RECE10-SHOKUJINISSU   (IDX1)
                         TO  SUM-ROU-SHOKUJINISSU  (IDY4 IDY3 IDY1 IDY2)
                             SUM-ROU-GOK-SHOKUJINISSU(IDY4 IDY3)
           ADD     RECE10-SHOKUJIRYOYOHI (IDX1)
                         TO  SUM-ROU-SHOKUJIRYOYOHI(IDY4 IDY3 IDY1 IDY2)
                             SUM-ROU-GOK-SHOKUJIRYOYOHI(IDY4 IDY3)
           ADD     RECE10-SHOKUJIFTN     (IDX1)
                         TO  SUM-ROU-SHOKUJIFTN    (IDY4 IDY3 IDY1 IDY2)
                             SUM-ROU-GOK-SHOKUJIFTN(IDY4 IDY3)
      *
      *    小計
           ADD     1         TO  SUM-ROU-KENSU    (IDY4 IDY3 IDY1 3)
           ADD     RECE10-FTNMONEY       (IDX1)
                             TO  SUM-ROU-FTNMONEY (IDY4 IDY3 IDY1 3)
           IF      RECE10-SHOKUJINISSU(IDX1)   >   ZERO
               ADD     1     TO  SUM-ROU-SHOKUJIKENSU (IDY4 IDY3 IDY1 3)
           END-IF
      *
      *    公費再掲                      
           IF      RECE10-KOHNUM (1)   NOT =   SPACE
               PERFORM VARYING IDW FROM    1   BY  1
                       UNTIL   IDW >       4
                       OR      RECE10-KOHNUM (IDW) =   SPACE
                   EVALUATE    RECE10-KOHNUM (IDW)
                       WHEN    "019"
                       WHEN    "018"
                           MOVE    1               TO  IDY1
                       WHEN    "010"
                           MOVE    2               TO  IDY1
                       WHEN    "011"
                           MOVE    3               TO  IDY1
                       WHEN    "012"
                           MOVE    4               TO  IDY1
                       WHEN    "015"
                           MOVE    5               TO  IDY1
                       WHEN    "020"
                           MOVE    6               TO  IDY1
                       WHEN    "021"
                           MOVE    7               TO  IDY1
                       WHEN    "022"
                           MOVE    8               TO  IDY1
                       WHEN    "028"
                           MOVE    9               TO  IDY1
                       WHEN    "051"
                       WHEN    "091"
                           MOVE    10              TO  IDY1
                       WHEN    OTHER
                           MOVE    ZERO            TO  IDY1
                   END-EVALUATE
      *
                   IF      IDY1                >   ZERO 
                       COMPUTE IDX1    =   IDW     +       1
                       ADD     1
                             TO  SUM-ROU-KOH-KENSU (IDY4 IDY3 IDY1)
                       ADD     RECE10-JNISSU         (IDX1)
                             TO  SUM-ROU-KOH-NISSU (IDY4 IDY3 IDY1)
                       EVALUATE    WRK-NYUGAIKBN
                                           ALSO    RECE10-ZAITAKU
                           WHEN    "1"     ALSO    ANY
                           WHEN    "2"     ALSO    "1"
                               ADD     RECE10-FTNMONEY-FTN      (IDX1)
                                           TO  SUM-ROU-KOH-ITBFTN
                                                       (IDY4 IDY3 IDY1)
                               ADD     RECE10-FTNMONEY          (IDX1)
                                           TO  SUM-ROU-KOH-FTNMONEY
                                                       (IDY4 IDY3 IDY1)
                           WHEN    "2"     ALSO    SPACE
                               IF      IDY1        =   1
                                   ADD     RECE10-FTNMONEY-FTN  (IDX1)
                                           TO  SUM-ROU-KOH-ITBFTN
                                                       (IDY4 IDY3 IDY1)
                               END-IF
                                   ADD     RECE10-FTNMONEY      (IDX1)
                                           TO  SUM-ROU-KOH-FTNMONEY
                                                       (IDY4 IDY3 IDY1)
                       END-EVALUATE
      *                           
                       IF      RECE10-SHOKUJINISSU(IDX1)   >   ZERO
                           ADD     1
                             TO  SUM-ROU-KOH-SHOKUJIKENSU
                                                      (IDY4 IDY3 IDY1)
                       END-IF
                       ADD     RECE10-SHOKUJINISSU   (IDX1)
                             TO  SUM-ROU-KOH-SHOKUJINISSU
                                                      (IDY4 IDY3 IDY1)
                       ADD     RECE10-SHOKUJIRYOYOHI (IDX1)
                             TO  SUM-ROU-KOH-SHOKUJIRYOYOHI
                                                      (IDY4 IDY3 IDY1)
                       ADD     RECE10-SHOKUJIFTN     (IDX1)
                             TO  SUM-ROU-KOH-SHOKUJIFTN
                                                      (IDY4 IDY3 IDY1)
                   END-IF           
               END-PERFORM
           END-IF
      *
      *    公費再掲（老人被曝）レセ記載外分                  
           IF      RECE10-ETC-KOHNUM (1)   NOT =   SPACE
               PERFORM VARYING IDW FROM    1   BY  1
                       UNTIL   IDW >       4
                       OR      RECE10-ETC-KOHNUM (IDW) =   SPACE
                   EVALUATE    RECE10-ETC-KOHNUM (IDW)
                       WHEN    "019"
                           MOVE    1               TO  IDY1
                       WHEN    OTHER
                           MOVE    ZERO            TO  IDY1
                   END-EVALUATE
      *
                   IF      IDY1                >   ZERO 
                       ADD     1
                             TO  SUM-ROU-KOH-KENSU     (IDY4 IDY3 IDY1)
                       ADD     RECE10-ETC-JNISSU  (IDW)
                             TO  SUM-ROU-KOH-NISSU     (IDY4 IDY3 IDY1)
                       EVALUATE    WRK-NYUGAIKBN
                                           ALSO    RECE10-ZAITAKU
                           WHEN    "1"     ALSO    ANY
                           WHEN    "2"     ALSO    "1"
                               ADD     RECE10-ETC-FTNMONEY-FTN   (IDW)
                                           TO  SUM-ROU-KOH-ITBFTN
                                                       (IDY4 IDY3 IDY1)
                               ADD     RECE10-ETC-FTNMONEY       (IDW)
                                           TO  SUM-ROU-KOH-FTNMONEY
                                                       (IDY4 IDY3 IDY1)
                           WHEN    "2"     ALSO    SPACE
                               IF      IDY1        =   1
                                   ADD     RECE10-ETC-FTNMONEY-FTN(IDW)
                                           TO  SUM-ROU-KOH-ITBFTN
                                                       (IDY4 IDY3 IDY1)
                               END-IF
                                   ADD     RECE10-ETC-FTNMONEY    (IDW)
                                           TO  SUM-ROU-KOH-FTNMONEY
                                                       (IDY4 IDY3 IDY1)
                       END-EVALUATE
      *                           
                       IF      RECE10-ETC-SHOKUJINISSU (IDW)
                                                 >   ZERO
                           ADD     1
                             TO  SUM-ROU-KOH-SHOKUJIKENSU
                                                     (IDY4 IDY3 IDY1)
                       END-IF
                       ADD     RECE10-ETC-SHOKUJINISSU   (IDW)
                             TO  SUM-ROU-KOH-SHOKUJINISSU
                                                     (IDY4 IDY3 IDY1)
                       ADD     RECE10-ETC-SHOKUJIRYOYOHI (IDW)
                             TO  SUM-ROU-KOH-SHOKUJIRYOYOHI
                                                     (IDY4 IDY3 IDY1)
                       ADD     RECE10-ETC-SHOKUJIFTN     (IDW)
                             TO  SUM-ROU-KOH-SHOKUJIFTN
                                                     (IDY4 IDY3 IDY1)
                   END-IF           
               END-PERFORM
           END-IF                  
      *
           MOVE    1               TO  FLG-UPD 
           .
       210-ROU-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
           IF    ( IDY1            =   ZERO )
           OR    ( IDY2            =   ZERO )
               GO  TO  210-HKN-SYUKEI-EXT
           END-IF
      *     
           MOVE    1                   TO  IDX1
      *    
      *    件数
           ADD     1               TO  SUM-KENSU (IDY4 IDY3 IDY1 IDY2)
           IF      RECE10-SHOKUJINISSU(IDX1)   >   ZERO
               ADD     1           TO  SUM-SHOKUJIKENSU
                                      (IDY4 IDY3 IDY1 IDY2)
           END-IF
      *    （１）合計、総件数
           ADD     1                   TO  SUM-GOK-KENSU (IDY4 IDY3 1)
                                           SUM-GOK-KENSU (IDY4 IDY3 4)
           IF      RECE10-SHOKUJINISSU(IDX1)   >   ZERO
               ADD     1               TO  SUM-GOK-SHOKENSU(IDY4 IDY3 1)
                                           SUM-GOK-SHOKENSU(IDY4 IDY3 4) 
           END-IF
      *
      *    診療実日数〜一部負担金 
           ADD     RECE10-JNISSU         (IDX1)
                                 TO  SUM-NISSU     (IDY4 IDY3 IDY1 IDY2)
           ADD     RECE10-TOTALTEN       (IDX1)
                                 TO  SUM-TOTALTEN  (IDY4 IDY3 IDY1 IDY2)
           IF    (  IDY1              <   3        )
           OR    ( (IDY1              =   3 ) AND
                   (IDY2              =   3 )      )                          
               ADD     RECE10-FTNMONEY       (IDX1)
                                 TO  SUM-FTNMONEY  (IDY4 IDY3 IDY1 IDY2)
           END-IF                          
           ADD     RECE10-SHOKUJINISSU   (IDX1)
                                 TO  SUM-SHOKUJINISSU
                                                   (IDY4 IDY3 IDY1 IDY2)
           ADD     RECE10-SHOKUJIRYOYOHI (IDX1)
                                 TO  SUM-SHOKUJIRYOYOHI
                                                   (IDY4 IDY3 IDY1 IDY2)
           ADD     RECE10-SHOKUJIFTN     (IDX1)
                                 TO  SUM-SHOKUJIFTN(IDY4 IDY3 IDY1 IDY2)
      *
      *    小計
           IF      IDY2            >   1
      *        件数
               ADD     1           TO  SUM-KENSU (IDY4 IDY3 IDY1 12)
               IF      RECE10-SHOKUJINISSU(IDX1)   >   ZERO
                   ADD     1       TO  SUM-SHOKUJIKENSU
                                                 (IDY4 IDY3 IDY1 12)
               END-IF
      *        診療実日数〜一部負担金 
               ADD     RECE10-JNISSU         (IDX1)
                                   TO  SUM-NISSU     (IDY4 IDY3 IDY1 12)    
               ADD     RECE10-TOTALTEN       (IDX1)
                                   TO  SUM-TOTALTEN  (IDY4 IDY3 IDY1 12)
               IF      IDY1              <   3                          
                   ADD     RECE10-FTNMONEY       (IDX1)
                                   TO  SUM-FTNMONEY  (IDY4 IDY3 IDY1 12)
               END-IF                      
               ADD     RECE10-SHOKUJINISSU   (IDX1)
                                   TO  SUM-SHOKUJINISSU
                                                     (IDY4 IDY3 IDY1 12)
               ADD     RECE10-SHOKUJIRYOYOHI (IDX1)
                                   TO  SUM-SHOKUJIRYOYOHI
                                                     (IDY4 IDY3 IDY1 12)
               ADD     RECE10-SHOKUJIFTN     (IDX1)
                                   TO  SUM-SHOKUJIFTN(IDY4 IDY3 IDY1 12)
           END-IF                              
      *
           MOVE    1               TO  FLG-UPD 
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       4
                   OR  RECE10-KOHNUM (IDZ) =   SPACE
      *
      *        特定疾患無しのとき 
               IF      RECE10-KOHNUM (IDZ) =   "091"
                   MOVE    "051"               TO  RECE10-KOHNUM (IDZ)
               END-IF
      *
      *        岡山県の地方公費のまとめ（２００６年１０月以降）
               IF    ( SYS-1001-PREFNUM    =   "33"                  ) 
               AND   ( LNK-PRTKANRI-SRYYM  >=  WRK-CONS-SRYYM-200610 )
                   EVALUATE    RECE10-KOHNUM (IDZ) (2:2)
                       WHEN    "41"
                           MOVE    "141"       TO  RECE10-KOHNUM (IDZ)
                       WHEN    "85"
                           MOVE    "185"       TO  RECE10-KOHNUM (IDZ)
                       WHEN    "80"
                           MOVE    "180"       TO  RECE10-KOHNUM (IDZ)
                       WHEN    "86"
                           MOVE    "186"       TO  RECE10-KOHNUM (IDZ)
                   END-EVALUATE
               END-IF
      *
      *        北海道の地方公費のまとめ（２００７年８月以降）
               IF    ( SYS-1001-PREFNUM    =   "01"     ) 
               AND   ( LNK-PRTKANRI-SRYYM  >=  "200708" )
                   EVALUATE    RECE10-KOHNUM (IDZ) (2:2)
                       WHEN    "90"
                           MOVE    "190"       TO  RECE10-KOHNUM (IDZ)
                       WHEN    "91"
                           MOVE    "191"       TO  RECE10-KOHNUM (IDZ)
                       WHEN    "92"
                           MOVE    "192"       TO  RECE10-KOHNUM (IDZ)
                   END-EVALUATE
               END-IF
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2 >          100
                   IF      SUM-KOH-KOHNUM (IDY4 IDY3 IDX1 IDX2)
                                               =   SPACE
                       MOVE    RECE10-KOHNUM (IDZ)     TO 
                                       SUM-KOH-KOHNUM
                                                   (IDY4 IDY3 IDX1 IDX2)
                   END-IF                                
                   IF      RECE10-KOHNUM (IDZ) 
                                   =   SUM-KOH-KOHNUM
                                                   (IDY4 IDY3 IDX1 IDX2)
                       PERFORM 2201-KOH-SYUKEI-01-SEC
                       MOVE    100             TO  IDX2
                   END-IF
               END-PERFORM
           END-PERFORM                 
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計処理
      *****************************************************************
       2201-KOH-SYUKEI-01-SEC        SECTION.
      *
           COMPUTE IDY     =   IDZ     +   1
      *    
      *    件数
           ADD     1           TO  SUM-KOH-KENSU (IDY4 IDY3 IDX1 IDX2)
           IF      RECE10-SHOKUJINISSU(IDY)    >   ZERO
               ADD     1       TO  SUM-KOH-SHOKUJIKENSU
                                                 (IDY4 IDY3 IDX1 IDX2)
           END-IF
      *   （３）合計、総件数
           ADD     1                   TO  SUM-GOK-KENSU (IDY4 IDY3 3)
                                           SUM-GOK-KENSU (IDY4 IDY3 4)
           IF      RECE10-SHOKUJINISSU(IDY)   >   ZERO
               ADD     1               TO  SUM-GOK-SHOKENSU(IDY4 IDY3 3)
                                           SUM-GOK-SHOKENSU(IDY4 IDY3 4) 
           END-IF
      *
      *    診療実日数〜一部負担金 
           ADD     RECE10-TOTALTEN       (IDY)
                             TO  SUM-KOH-TOTALTEN  (IDY4 IDY3 IDX1 IDX2)
           ADD     RECE10-FTNMONEY       (IDY)
                             TO  SUM-KOH-FTNMONEY  (IDY4 IDY3 IDX1 IDX2)
      *                           
           ADD     RECE10-SHOKUJIRYOYOHI (IDY)
                             TO  SUM-KOH-SHOKUJIRYOYOHI
                                                   (IDY4 IDY3 IDX1 IDX2)
           ADD     RECE10-SHOKUJIFTN     (IDY)
                             TO  SUM-KOH-SHOKUJIFTN(IDY4 IDY3 IDX1 IDX2)
      *
           MOVE    1               TO  FLG-UPD 
      *
           .
       2201-KOH-SYUKEI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求ＤＢ更新用ファイル出力処理
      *****************************************************************
       230-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           WRITE   RECE10X-REC
      *
           ADD     1                   TO  CNT-UPDFILE
           .
       230-UPD-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
           END-IF
      *
           MOVE    1                   TO  FLG-END
                                           FLG-ERR     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCBM022 IN  "  CNT-RECE10
           DISPLAY "***          UPD "  CNT-UPDFILE 
           DISPLAY "*** ORCBM022 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT                         
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *   請求管理ＤＢ更新処理
      *****************************************************************
       310-SEIKYU-UPD-SEC              SECTION.
      *
           OPEN    INPUT    UPD-FILE
      *
           PERFORM 900-UPD-FILE-READ-SEC
           PERFORM             UNTIL   FLG-UPDFILE    =   1
               MOVE    SPACE           TO  RECE10-REC   
               INITIALIZE                  RECE10-REC
               MOVE    RECE10X-KEY         TO  RECE10-KEY
               MOVE    RECE10-REC          TO  MCPDATA-REC
               PERFORM 910-SEIKYU-INV-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    "2"             TO  RECE10-SKYKBN
                   MOVE    LNK-PRTKANRI-SRYYM  
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD  
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "MONFUNC"       USING
                                               MCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       MOVE    SPACE               TO  WRK-RECEERR
                       STRING "請求管理ＤＢ　更新エラー　"
                                               DELIMITED  BY  SIZE
                               RECE10-KEY      DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                       END-STRING                                
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                   STRING "請求管理ＤＢ　読込み更新エラー　"
                                               DELIMITED  BY  SIZE
                           RECE10X-KEY         DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING                                
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-UPDFILE
               END-IF
           END-PERFORM  
      *
           CLOSE   UPD-FILE
           .
       310-SEIKYU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE10
      *     
      *    請求管理ＤＢ検索
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                      RECE10-REC
           MOVE    WRK-PARA-HOSPID     TO  RECE10-HOSPID
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
           EVALUATE    WRK-PRTFLG
               WHEN    "1"
                   MOVE    WRK-CONS-SRYYM-200304
                                           TO  RECE10-SRYYM
                   MOVE    WRK-CONS-SRYYM-200604
                                           TO  RECE10-XXSRYYM
               WHEN    "2"
                   IF      WRK-NYUGAIKBN       =   "2"
                       MOVE    WRK-CONS-SRYYM-200304
                                               TO  RECE10-SRYYM
                       MOVE    WRK-CONS-SRYYM-200610
                                               TO  RECE10-XXSRYYM
                   ELSE
                       MOVE    WRK-CONS-SRYYM-200604
                                               TO  RECE10-SRYYM
                       MOVE    WRK-CONS-SRYYM-200610
                                               TO  RECE10-XXSRYYM
                   END-IF
               WHEN    "3"
                   MOVE    WRK-CONS-SRYYM-200610
                                               TO  RECE10-SRYYM
                   MOVE    999999              TO  RECE10-XXSRYYM
           END-EVALUATE
           MOVE    WRK-NYUGAIKBN        TO  RECE10-NYUGAIKBN
      *
      *###
           DISPLAY "SYOKBN=" WRK-SYOKBN
                   " SRYYM=" RECE10-SRYYM "-" RECE10-XXSRYYM 
                   " NYUGAIKBN= " RECE10-NYUGAIKBN
                   " PATHNAME=" WRK-MCP-PATHNAME
      *###
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE10
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE10
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
               IF      RECE10-NYUGAIKBN
                                   NOT =   WRK-NYUGAIKBN
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
               EVALUATE    WRK-PARA-GENMENKBN
                   WHEN    ZERO
                       IF      RECE10-ETC-YKZFTN (1)   >   ZERO
                           GO  TO  900-SEIKYU-READ-SEC
                       END-IF
                   WHEN    1
                       IF      RECE10-ETC-YKZFTN (1)   =   ZERO
                           GO  TO  900-SEIKYU-READ-SEC
                       END-IF
               END-EVALUATE 
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    RECE10-HOSPID       TO  PTINF-HOSPID
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF    ( FLG-PTINF           =   ZERO )
               AND   ( PTINF-TSTPTNUMKBN   =   "1"  )
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *         
               ADD     1                   TO  CNT-RECE10
      *         
               DISPLAY "PTNUM=" RECE10-PTNUM " " RECE10-ETC-YKZFTN (1)
      *         
           ELSE
               MOVE    1                   TO  FLG-RECE10
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *  
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理ＤＢ更新用ファイル読込
      *****************************************************************
       900-UPD-FILE-READ-SEC             SECTION.
      *
           READ    UPD-FILE
               AT  END
                   MOVE    1           TO  FLG-UPDFILE
               NOT AT  END
                   MOVE    ZERO        TO  FLG-UPDFILE
           END-READ
           .
       900-UPD-FILE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "MONFUNC"       USING
                                           MCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               CALL    "MONFUNC"           USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
