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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC91.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 算定歴チェック処理（診療行為入力のみＫ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01.06.05  警告メッセージ追加
      *  01.00.02    MCC-多々納   02.04.30  消炎鎮痛処置を同時算定へ追加
      *  01.00.03    MCC-多々納   02.05.17  労災用保険種別毎の追加
      *  01.00.04    NACL-多々納  02.08.06  同時算定チェックに継続管理加
      *                                     算追加
      *  01.00.05    NACL-多々納  02.12.17  入院時算定チェック変更
      *  01.00.06    NACL-多々納  03.03.17  介達牽引を同時算定へ追加
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-NYU-CHK         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-ORC-DBPATH          PIC X(64).
      *
      *    算定回数チェック用
           03  WRK-STRYM.
               05  WRK-STRYY           PIC 9(04).
               05  WRK-STRMM           PIC 9(02).
           03  WRK-SANTEI-CNT          PIC 9(04).
           03  WRK-SANTEI-CNTW         PIC 9(04).
           03  WRK-CNT                 PIC 9(04).
           03  WRK-DAYCNT              PIC 9(04).
           03  WRK-DOCNT               PIC 9(04).
           03  WRK-CNT-OK              PIC 9(04).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-CODE                PIC X(03).
      *    現在件数
           03  WRK-SC91-CNT            PIC 9(04).
           03  WRK-SC91-DAYCNT         PIC 9(04).
      *
           03  WRK-TEI-CNT             PIC 9(04).
           03  WRK-ATO-CNT             PIC 9(04).
      *
           03  WRK-JGNCNTERR           PIC 9(01).
      *
           03  WRK-RIGAKU-RYO          PIC X(01). 
           03  WRK-RIGAKU-KBN          PIC X(01). 
      *    労災のリハビリ件数
           03  WRK-RSI-CNT             PIC 9(04).
      *
           03  WRK-CHK-SRYCD           PIC X(09).
           03  WRK-CHK-SRYYMD.
               05  WRK-CHK-SRYYM           PIC X(06).
               05  WRK-CHK-SRYDD           PIC X(02).
           03  WRK-CHK-NAME            PIC X(100).
      *
           03  WRK-DAYMAXCNT           PIC 9(04).
      *
      *    同時に算定する診療行為コード一覧
       01  TBL-DOUJI-AREA.
      *     寝たきり老人在宅総合診療科（処方箋交付あり）
      *    03  FILLER                  PIC X(03)   VALUE   "001".
      *    03  FILLER                  PIC X(09)   VALUE   "114701210".
      *     寝たきり老人在宅総合診療科（処方箋交付なし）
      *    03  FILLER                  PIC X(03)   VALUE   "001".
      *    03  FILLER                  PIC X(09)   VALUE   "114701710".
      *
      *     薬剤情報提供料
           03  FILLER                  PIC X(03)   VALUE   "002".
           03  FILLER                  PIC X(09)   VALUE   "120002370".
      *     薬剤情報提供料（老人手帳記載なし）
           03  FILLER                  PIC X(03)   VALUE   "002".
           03  FILLER                  PIC X(09)   VALUE   "113701410".
      *     薬剤情報提供料（老人手帳記載あり）
           03  FILLER                  PIC X(03)   VALUE   "002".
           03  FILLER                  PIC X(09)   VALUE   "113701310".
      *
      *     特定疾患処方管理加算処方箋料
           03  FILLER                  PIC X(03)   VALUE   "003".
           03  FILLER                  PIC X(09)   VALUE   "120002570".
      *     特定疾患処方管理加算処方料
           03  FILLER                  PIC X(03)   VALUE   "003".
           03  FILLER                  PIC X(09)   VALUE   "120002270".
      *
      *    小児科外来診療科（院外処方）初診
           03  FILLER                  PIC X(03)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113003510".
      *    小児科外来診療科（院内処方）初診
           03  FILLER                  PIC X(03)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113003710".
      *    小児科外来診療科（院外処方）再診
           03  FILLER                  PIC X(03)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113003610".
      *    小児科外来診療科（院内処方）再診
           03  FILLER                  PIC X(03)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113003810".
      *    消炎鎮痛処置テーブル
           03  FILLER                  PIC X(03)   VALUE   "005".
           03  FILLER                  PIC X(09)   VALUE   "140029610".
           03  FILLER                  PIC X(03)   VALUE   "005".
           03  FILLER                  PIC X(09)   VALUE   "140040310".
           03  FILLER                  PIC X(03)   VALUE   "005".
           03  FILLER                  PIC X(09)   VALUE   "140002210".
           03  FILLER                  PIC X(03)   VALUE   "005".
           03  FILLER                  PIC X(09)   VALUE   "140002310".
      *    介達牽引
           03  FILLER                  PIC X(03)   VALUE   "005".
           03  FILLER                  PIC X(09)   VALUE   "140048010".
      *
      *    継続管理加算（一般）
           03  FILLER                  PIC X(03)   VALUE   "006".
           03  FILLER                  PIC X(09)   VALUE   "112007170".
      *    継続管理加算（老人）
           03  FILLER                  PIC X(03)   VALUE   "006".
           03  FILLER                  PIC X(09)   VALUE   "112702270".
      *
      *    長期投薬加算（処方箋料）
           03  FILLER                  PIC X(03)   VALUE   "007".
           03  FILLER                  PIC X(09)   VALUE   "120003270".
      *    長期投薬加算（処方料）
           03  FILLER                  PIC X(03)   VALUE   "007".
           03  FILLER                  PIC X(09)   VALUE   "120003170".
      *
       01  TBL-DOUJI-AREA-R            REDEFINES  TBL-DOUJI-AREA.
      *****03  TBL-DOUJI-OCC           OCCURS   9   INDEXED   TBL-IDX.
      *****03  TBL-DOUJI-OCC           OCCURS   13  INDEXED   TBL-IDX.
           03  TBL-DOUJI-OCC           OCCURS   18  INDEXED   TBL-IDX.
               05  TBL-CODE                PIC X(03).
               05  TBL-SRYCD               PIC X(09).
      *01  TBL-MAX                     PIC 9(04)   VALUE   9.
       01  TBL-MAX                     PIC 9(04)   VALUE   18.
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC91.INC".
      *
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC91AREA
           SPA-SCR-REC
           SPA-AREA 
           TNS-TENSU-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPACE               TO  LNK-SC91-ERRCD
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *    入院回数処理なし
           IF      LNK-SC91-SRYCD(1:1) =   "*"  OR  SPACE
               GO      TO      000-PROC-EXT
           END-IF
      *
           IF      LNK-SC91-KBN        =   "2"  OR  "3"  OR  "4"
               MOVE    LNK-SC91-SRYCD      TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   1
                   MOVE    "0001"              TO  LNK-SC91-ERRCD
                   GO      TO      000-PROC-EXT
               END-IF
           END-IF
      *
           IF      TNS-SANTEIRRKKBN    =   ZERO
               GO      TO      000-PROC-EXT
           END-IF
      *
      *    訂正の時、前回算定時の算定回数
           IF      LNK-SC91-SYORIFLG   =   1
               MOVE    LNK-SC91-SRYCD      TO  WRK-SRYCD
               PERFORM 1001-TEISEI-CNT-SEC
           ELSE
               MOVE    ZERO            TO  WRK-TEI-CNT
           END-IF
      *
           EVALUATE    LNK-SC91-KBN
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
               WHEN    "4"
      *            算定歴　回数チェック
                   PERFORM 100-SANTEI-CHK-SEC
                   IF      LNK-SC91-ERRCD  =   SPACE
      *                同時算定歴チェック
                       PERFORM 200-DOUJI-CHK-SEC
                   END-IF
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    前回算定時の算定回数処理
      *****************************************************************
       1001-TEISEI-CNT-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-TEI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   100 )  OR
                              (LNK-SC91-TEI-SRYCD(IDX)
                                           =   SPACE )
               IF      WRK-SRYCD           =   LNK-SC91-TEI-SRYCD(IDX)
                   ADD     LNK-SC91-TEI-KAISU(IDX)
                                           TO  WRK-TEI-CNT
               END-IF
           END-PERFORM
           .
       1001-TEISEI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険毎のコードの判定処理
      *****************************************************************
       1000-SANTEI-CHK-SEC      SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
                   MOVE    SPACE           TO  WRK-RIGAKU-RYO
                   MOVE    SPACE           TO  WRK-RIGAKU-KBN
                   MOVE    SPACE           TO  WRK-RSI-KBN
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
                   MOVE    TBL-RIGAKU-RYO (TBL-SAN)
                                           TO  WRK-RIGAKU-RYO
                   MOVE    TBL-RIGAKU-KBN (TBL-SAN)
                                           TO  WRK-RIGAKU-KBN
                   MOVE    TBL-RSI-KBN    (TBL-SAN)
                                           TO  WRK-RSI-KBN
           END-SEARCH
      *
           IF      WRK-RIGAKU-RYO      =   SPACE
               MOVE    ZERO                TO  FLG-HKN-SANTEI
           ELSE
               MOVE    1                   TO  FLG-HKN-SANTEI
           END-IF
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理
      *****************************************************************
       100-SANTEI-CHK-SEC      SECTION.
      *
           MOVE    LNK-SC91-SRYCD      TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-CNT
           MOVE    ZERO                TO  WRK-DOCNT
           MOVE    ZERO                TO  WRK-DAYCNT
           MOVE    ZERO                TO  WRK-ATO-CNT
           MOVE    SPACE               TO  WRK-CHK-SRYYMD
      *
           IF      LNK-SC91-KBN        =   "4"
               MOVE    ZERO            TO  WRK-DOCNT
           ELSE
      *        今回入力分カウント
               PERFORM 1003-INPUT-CHK-SEC
           END-IF
      *
      *    上限他月数
           IF      TNS-JGNCNTETCM     >   ZERO
               PERFORM 1002-SANTEI-ETC-SEC
           ELSE
               PERFORM 1001-SANTEI-YMD-SEC
           END-IF
           MOVE    TNS-JGNCNT1D        TO  LNK-SC91-DAYMAXCNT
           MOVE    TNS-JGNCNT          TO  LNK-SC91-MAXCNT
      *
           MOVE    TNS-JGNCNTERR       TO  WRK-JGNCNTERR
      *
           COMPUTE LNK-SC91-CNT        =   LNK-SC91-CNT
                                       +   WRK-DOCNT
           COMPUTE LNK-SC91-DAYCNT     =   LNK-SC91-DAYCNT
                                       +   WRK-DOCNT
      *
      *    入力回数が１回以上の時、回数に加算
           IF     (LNK-SC91-KBN        =   "1"  OR  "3" )  AND
                  (LNK-SC91-KAISU      >   1    )
               COMPUTE WRK-SC91-CNT        =   LNK-SC91-CNT
                                           +  (LNK-SC91-KAISU
                                           -   1   )
               COMPUTE WRK-SC91-DAYCNT     =   LNK-SC91-DAYCNT
                                           +  (LNK-SC91-KAISU
                                           -   1   )
           ELSE
               MOVE    LNK-SC91-CNT        TO  WRK-SC91-CNT
               MOVE    LNK-SC91-DAYCNT     TO  WRK-SC91-DAYCNT
           END-IF
      *
      *    当月上限
           IF     (LNK-SC91-MAXCNT     >   ZERO   )  AND
                  (LNK-SC91-MAXCNT     <=  WRK-SC91-CNT)
               MOVE    "0091"          TO  LNK-SC91-ERRCD
      *        上限回数エラー処理＝９の時、回数エラーは警告とする
               IF       WRK-CNT        =   ZERO
                   IF       TNS-JGNCNTERR      =   9
                       MOVE    "K004"              TO  LNK-SC91-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    労災で消炎鎮痛処置の時、当日上限回数を変更する
           IF      SPA-HKNKBN      NOT =   ZERO
               EVALUATE    WRK-RIGAKU-KBN
      *            マッサージ
                   WHEN    "M"
      *            器具
                   WHEN    "K"
                       MOVE    3               TO  LNK-SC91-DAYMAXCNT
      *            介達牽引
                   WHEN    "T"
                       MOVE    3               TO  LNK-SC91-DAYMAXCNT
      *            湿布
                   WHEN    "P"
                       MOVE    ZERO            TO  LNK-SC91-DAYMAXCNT
               END-EVALUATE
           END-IF
      *    当日上限
           IF     (LNK-SC91-DAYMAXCNT  >   ZERO   )   AND
                  (LNK-SC91-DAYMAXCNT  <=  WRK-SC91-DAYCNT )
               MOVE    "0091"          TO  LNK-SC91-ERRCD
      *        上限回数エラー処理＝９の時、回数エラーは警告とする
               IF       WRK-DAYCNT     =   ZERO
                   IF       TNS-JGNCNTERR      =   9
                       MOVE    "K004"              TO  LNK-SC91-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    労災毎の算定時
           IF     (LNK-SC91-ERRCD  NOT =   SPACE  )  AND
                  (WRK-RSI-KBN     NOT =   SPACE  )
               MOVE    "0094"          TO  LNK-SC91-ERRCD
           END-IF
      *    月上限のエラーの時、算定日を編集
           IF     (LNK-SC91-ERRCD  NOT =   SPACE  )  AND
                  (LNK-SC91-MAXCNT     >   ZERO   )  AND
                  (WRK-CHK-SRYYMD  NOT =   SPACE  )
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-CHK-SRYYMD      TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  LNK-DAY2-EDTYMD3    DELIMITED  BY  SIZE
                       "に算定済です"      DELIMITED  BY  SIZE
                       INTO                LNK-SC91-ERRMSG
               END-STRING
           END-IF
      *
      *    訂正後の回数
           MOVE    WRK-ATO-CNT         TO  LNK-SC91-ATO-CNT
      *
           MOVE    TNS-JGNCNTERR       TO  WRK-JGNCNTERR
           .
       100-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（診療年月）
      *****************************************************************
       1001-SANTEI-YMD-SEC      SECTION.
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           IF      WRK-RSI-KBN         =   SPACE
      *        主キー
               MOVE    "SANTEI-KEY"        TO  WRK-ORC-DBPATH
           ELSE
      *        労災毎の算定
               MOVE    "SANTEI-KEY3"       TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      **** MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
           MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
      ******** MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
      *****MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
           MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      FLG-SANTEI          =   1
               INITIALIZE              SANTEI-REC
           END-IF
      *
      *    算定歴の回数をチェックする
           IF      TNS-JGNCNT          >   ZERO
               MOVE    TNS-JGNCNT          TO  LNK-SC91-MAXCNT
               MOVE    SANTEI-MSANTEICNT   TO  LNK-SC91-CNT
           ELSE
               MOVE    SANTEI-MSANTEICNT   TO  LNK-SC91-CNT
           END-IF
      *    労災でリハビリの時３月以内の件数はカウントしない
           IF     (SPA-HKNKBN      NOT =   ZERO  )  AND
                  (FLG-HKN-SANTEI      =   1     )
               MOVE    LNK-SC91-CNT        TO  WRK-RSI-CNT
               PERFORM 10012-RSI-MCNT-SEC
               MOVE    WRK-RSI-CNT         TO  LNK-SC91-CNT
           END-IF
      *    エラー内容用
           IF      LNK-SC91-CNT        >   ZERO
               MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
               MOVE    SANTEI-MSANTEID     TO  WRK-CHK-SRYDD
           END-IF
      *
      *    上限１日
      *****IF     (TNS-JGNCNT1D        >   ZERO  )  AND
           IF     (SPA-SRYYMD(7:2)     NUMERIC   )  AND
                  (SPA-SRYYMD(7:2)     >   ZERO  )
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               MOVE    TNS-JGNCNT1D        TO  LNK-SC91-DAYMAXCNT
               MOVE    SANTEI-DAY (IDX-Y2) TO  LNK-SC91-DAYCNT
           END-IF
      *
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC91-SYORIFLG   =   1
               IF      LNK-SC91-DAYCNT     >=  WRK-TEI-CNT
                   COMPUTE LNK-SC91-DAYCNT     =   LNK-SC91-DAYCNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO                TO  LNK-SC91-DAYCNT
               END-IF
               IF      LNK-SC91-CNT        >=  WRK-TEI-CNT
                   COMPUTE LNK-SC91-CNT        =   LNK-SC91-CNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO                TO  LNK-SC91-CNT
               END-IF
      *        上限１日の時、訂正日以降の回数を求める
      *****    IF      TNS-JGNCNT1D        >   ZERO
                   MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                   ADD     1                   TO  IDX-Y2
                   PERFORM VARYING     IDY     FROM    IDX-Y2  BY  1
                           UNTIL       IDY     >   31
                       ADD     SANTEI-DAY (IDY)    TO  WRK-ATO-CNT
                   END-PERFORM
      *****    END-IF
           END-IF
      *
           .
       1001-SANTEI-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災でリハビリの時３月以降の件数カウント処理（月数回数）
      *****************************************************************
       10012-RSI-MCNT-SEC      SECTION.
      *
      *    システム月＜傷病日＋３月
           IF       SPA-SRYYMD(1:6)    <   SPA-RSI-SHOBYOYMD3(1:6)
               MOVE    ZERO            TO  WRK-RSI-CNT
           ELSE
      *        システム月＝傷病日＋３月
               IF       SPA-SRYYMD(1:6)    =   SPA-RSI-SHOBYOYMD3(1:6)
                   MOVE    SPA-RSI-SHOBYOYMD3(7:2)     TO  IDX-Y2
                   MOVE    ZERO                TO  WRK-RSI-CNT
                   PERFORM VARYING     IDY     FROM    IDX-Y2  BY  1
                           UNTIL       IDY     >   31
                       ADD     SANTEI-DAY (IDY)    TO  WRK-RSI-CNT
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       10012-RSI-MCNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（月数回数）
      *****************************************************************
       1002-SANTEI-ETC-SEC      SECTION.
      *
      *    算定開始年月
           MOVE    SPA-SRYYMD(1:6)     TO  WRK-STRYM
           IF      WRK-STRMM           <   TNS-JGNCNTETCM
               COMPUTE WRK-STRYY       =   WRK-STRYY    -   1
               COMPUTE WRK-STRMM       =   WRK-STRMM
                                       +   12
                                       -  (TNS-JGNCNTETCM   -  1 )
           ELSE
               COMPUTE WRK-STRMM       =   WRK-STRMM
                                       -  (TNS-JGNCNTETCM   -  1 )
           END-IF
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL  (IDX         =   TNS-JGNCNTETCM )  OR
                          (WRK-STRYM   >   SPA-SRYYMD(1:6))
      *
      *        保険毎のコードの判定
               PERFORM 1000-SANTEI-CHK-SEC
      *        算定履歴検索
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    WRK-STRYM           TO  SANTEI-SRYYM
    **         MOVE    LNK-SC91-SRYCD      TO  SANTEI-SRYCD
               MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
               EVALUATE    TNS-SANTEIRRKKBN
                   WHEN    1
                       MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                       MOVE    ZERO                TO  SANTEI-SRYKA
                   WHEN    2
                       MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                       MOVE    ZERO                TO  SANTEI-SRYKA
                   WHEN    3
                       MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                       MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
                   WHEN    4
                       MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                       MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               END-EVALUATE
               IF     (FLG-HKNSAN          =   1   )  AND
                      (SPA-HKNKBN      NOT =   ZERO)
                   MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
               ELSE
                   MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               END-IF
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
                   PERFORM 920-SANTEI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
                   INITIALIZE                  SANTEI-REC
               END-IF
      *
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               IF      FLG-SANTEI          =   ZERO
      *************ADD     SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNT
                   MOVE    SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNTW
               ELSE
                   MOVE    ZERO                TO  WRK-SANTEI-CNTW
               END-IF
      *        労災でリハビリの時３月以内の件数はカウントしない
               IF     (SPA-HKNKBN      NOT =   ZERO  )  AND
                      (FLG-HKN-SANTEI      =   1     )
                   MOVE    WRK-SANTEI-CNTW     TO  WRK-RSI-CNT
                   PERFORM 10012-RSI-MCNT-SEC
                   MOVE    WRK-RSI-CNT         TO  WRK-SANTEI-CNTW
               END-IF
               ADD     WRK-SANTEI-CNTW         TO  WRK-SANTEI-CNT
      *
      *        訂正日以降の回数を求める
               IF      LNK-SC91-SYORIFLG   =   1
                   MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                   ADD     1                   TO  IDX-Y2
                   PERFORM VARYING     IDY     FROM    IDX-Y2  BY  1
                           UNTIL       IDY     >   31
                       ADD     SANTEI-DAY (IDY)    TO  WRK-ATO-CNT
                   END-PERFORM
               END-IF
      *
               ADD     1                   TO  WRK-STRMM
               IF      WRK-STRMM           >   12
                   COMPUTE WRK-STRYY       =   WRK-STRYY   +   1
                   MOVE    1               TO  WRK-STRMM
               END-IF
           END-PERFORM
      *
      *    算定歴の回数をチェックする
           MOVE    TNS-JGNCNTETC       TO  LNK-SC91-MAXCNT
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC91-SYORIFLG   =   1
               IF      WRK-SANTEI-CNT      >=   WRK-TEI-CNT
                   COMPUTE WRK-SANTEI-CNT      =   WRK-SANTEI-CNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO                TO  WRK-SANTEI-CNT
               END-IF
           END-IF
           MOVE    WRK-SANTEI-CNT      TO  LNK-SC91-CNT
      *
           .
       1002-SANTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    今回診療行為チェック処理
      *****************************************************************
       1003-INPUT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-DOCNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   200 )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *        入院で１日上限の時、入力日のみ
               MOVE    ZERO            TO  FLG-NYU-CHK
               IF     (SPA-NYUGAIKBN       =   "1"   )  AND
                      (TNS-JGNCNT1D        >   ZERO  )  AND
                      (SPA-COM-NYU-KAI(IDX)    >   ZERO )
                   MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                   IF      SPA-COM-NYUINDAY(IDX IDX-Y2)    =   ZERO
                       MOVE    1               TO  FLG-NYU-CHK
                   END-IF
               END-IF
      *
            IF     FLG-NYU-CHK          =   ZERO
               IF      LNK-SC91-KBN        =   "1"  OR  "3"
                   IF      IDX             =   LNK-SC91-GMN-IDX
                       CONTINUE
                   ELSE
                       IF     (SPA-COM-INPUTCD(IDX)
                                           =   LNK-SC91-SRYCD  )  OR
                              (SPA-COM-CHK-SRYCD (IDX)
                                           =   LNK-SC91-SRYCD  )
                           ADD     SPA-COM-KAISU(IDX)
                                               TO  WRK-DOCNT
                       END-IF
                   END-IF
               ELSE
                   IF     (SPA-COM-INPUTCD(IDX)
                                           =   LNK-SC91-SRYCD  )  OR
                          (SPA-COM-CHK-SRYCD (IDX)
                                           =   LNK-SC91-SRYCD  )
                       ADD     SPA-COM-KAISU(IDX)
                                               TO  WRK-DOCNT
                   END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       1003-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同時算定歴チェック処理
      *****************************************************************
       200-DOUJI-CHK-SEC      SECTION.
      *
      *    同時算定コード存在のチェック
           SET     TBL-IDX            TO  1
           SEARCH      TBL-DOUJI-OCC    VARYING   TBL-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-CODE
                       GO      TO      200-DOUJI-CHK-EXT
                   WHEN    LNK-SC91-SRYCD      =   TBL-SRYCD (TBL-IDX)
                       MOVE    TBL-CODE   (TBL-IDX)    TO  WRK-CODE
           END-SEARCH
      *
           MOVE    SPACE               TO  WRK-CHK-SRYCD
           MOVE    SPACE               TO  WRK-CHK-NAME
           MOVE    SPACE               TO  WRK-CHK-SRYYMD
           MOVE    ZERO                TO  WRK-CNT
           MOVE    ZERO                TO  WRK-DOCNT
           MOVE    ZERO                TO  WRK-DAYCNT
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   TBL-MAX
               SET     TBL-IDX         TO  IDX2
               IF      (LNK-SC91-SRYCD NOT =   TBL-SRYCD (TBL-IDX)) AND
                       (WRK-CODE           =   TBL-CODE  (TBL-IDX))
                   MOVE    TBL-SRYCD (TBL-IDX)     TO  WRK-SRYCD
                   MOVE    WRK-SRYCD               TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF     (FLG-TENSU           =   1    )  OR
                          (TNS-SANTEIRRKKBN    =   ZERO )
                       CONTINUE
                   ELSE
      *
                       IF      LNK-SC91-SYORIFLG   =   1
                           PERFORM 1001-TEISEI-CNT-SEC
                       ELSE
                           MOVE    ZERO            TO  WRK-TEI-CNT
                       END-IF
      *
                      IF      LNK-SC91-KBN        =   "4"
                           CONTINUE
                      ELSE
      *                    今回入力分カウント
                           PERFORM 2003-DOJINPUT-CHK-SEC
                      END-IF
      *
      *               上限他月数
                      IF      TNS-JGNCNTETCM     >   ZERO
                          PERFORM 2002-DOJSANTEI-ETC-SEC
                      ELSE
                          PERFORM 2001-DOJSANTEI-YMD-SEC
                      END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    当月のＭＡＸ
           IF     (LNK-SC91-MAXCNT     >   ZERO  )  AND
                  (LNK-SC91-MAXCNT     <= ( WRK-SC91-CNT  +  WRK-CNT
                                                          +  WRK-DOCNT))
               MOVE    "0033"          TO  LNK-SC91-ERRCD
      *        上限回数エラー処理＝９の時、回数エラーは警告とする
               IF       WRK-JGNCNTERR      =   9
                   MOVE    "K004"              TO  LNK-SC91-ERRCD
               END-IF
           END-IF
      *
      *
      *    労災で消炎鎮痛処置の時、当日上限回数を変更する
           MOVE    LNK-SC91-DAYMAXCNT  TO  WRK-DAYMAXCNT
           IF      SPA-HKNKBN      NOT =   ZERO
               EVALUATE    WRK-RIGAKU-KBN
      *            マッサージ
                   WHEN    "M"
      *            器具
                   WHEN    "K"
                       MOVE    3               TO  WRK-DAYMAXCNT
      *            介達牽引
                   WHEN    "T"
                       MOVE    3               TO  WRK-DAYMAXCNT
      *           湿布
                   WHEN    "P"
                       MOVE    ZERO            TO  WRK-DAYMAXCNT
               END-EVALUATE
           END-IF
      *    当日のＭＡＸ
           IF     (LNK-SC91-ERRCD      =   SPACE  )  AND
                  (WRK-DAYMAXCNT       >   ZERO   )
               IF      WRK-DAYMAXCNT
                                   <= ( WRK-SC91-DAYCNT  +  WRK-DAYCNT
                                                         +  WRK-DOCNT )
                   MOVE    "0033"          TO  LNK-SC91-ERRCD
      *            消炎鎮痛処置の時、エラーメッセージを換える
                   IF      WRK-CODE        =   "005"
                       MOVE    "1022"          TO  LNK-SC91-ERRCD
                   END-IF
      *            上限回数エラー処理＝９の時、回数エラーは警告とする
                   IF       WRK-JGNCNTERR      =   9
                      MOVE    "K004"              TO  LNK-SC91-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    併用算定エラーの内容を編集する（最後に算定したもの）
           IF     LNK-SC91-ERRCD   NOT =   SPACE
               IF      WRK-CHK-NAME        =   SPACE
                   MOVE    WRK-CHK-SRYCD       TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   MOVE    TNS-NAME            TO  WRK-CHK-NAME
                   STRING  "今回　"            DELIMITED  BY  SIZE
                           WRK-CHK-NAME        DELIMITED  BY  SPACE
                           INTO                LNK-SC91-ERRMSG
                   END-STRING
               ELSE
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-CHK-SRYYMD      TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                   STRING  LNK-DAY2-EDTYMD3    DELIMITED  BY  SIZE
                           "　"                DELIMITED  BY  SIZE
                           WRK-CHK-NAME        DELIMITED  BY  SPACE
                           INTO                LNK-SC91-ERRMSG
                   END-STRING
               END-IF
           END-IF
      *
      *    点数マスタの内容を入力分へ戻す
           MOVE    LNK-SC91-SRYCD      TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           .
       200-DOUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（診療年月）
      *****************************************************************
       2001-DOJSANTEI-YMD-SEC      SECTION.
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
      *
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           IF      WRK-RSI-KBN         =   SPACE
      *        主キー
               MOVE    "SANTEI-KEY"        TO  WRK-ORC-DBPATH
           ELSE
      *        労災毎の算定
               MOVE    "SANTEI-KEY3"       TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
           MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
      ******** MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
      *****MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
           MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      FLG-SANTEI          =   1
               INITIALIZE              SANTEI-REC
           END-IF
      *
      *    算定歴の回数をチェックする
           IF      TNS-JGNCNT          >   ZERO
               ADD     SANTEI-MSANTEICNT   TO  WRK-CNT
           END-IF
      *    労災でリハビリの時３月以内の件数はカウントしない
           IF     (SPA-HKNKBN      NOT =   ZERO  )  AND
                  (FLG-HKN-SANTEI      =   1     )
               MOVE    WRK-CNT             TO  WRK-RSI-CNT
               PERFORM 10012-RSI-MCNT-SEC
               MOVE    WRK-RSI-CNT         TO  WRK-CNT
           END-IF
      *    上限１日
           IF     (TNS-JGNCNT1D        >   ZERO  )  AND
                  (SPA-SRYYMD(7:2)     NUMERIC   )  AND
                  (SPA-SRYYMD(7:2)     >   ZERO  )
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               ADD     SANTEI-DAY (IDX-Y2) TO  WRK-DAYCNT
           END-IF
      *
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC91-SYORIFLG   =   1
               IF      WRK-DAYCNT          >=  WRK-TEI-CNT
                   COMPUTE WRK-DAYCNT          =   WRK-DAYCNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO            TO  WRK-DAYCNT
               END-IF
               IF      WRK-CNT              >=  WRK-TEI-CNT
                   COMPUTE WRK-CNT          =   WRK-CNT
                                            -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO            TO  WRK-CNT
               END-IF
           END-IF
      *
           IF     (WRK-CNT             >   ZERO  )  OR
                  (WRK-DAYCNT          >   ZERO  )
               IF     (SANTEI-MSANTEID     >   ZERO )  AND
                      (SANTEI-SRYYM        >   ZERO )
                   MOVE    WRK-SRYCD           TO  WRK-CHK-SRYCD
                   MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
                   MOVE    SANTEI-MSANTEID     TO  WRK-CHK-SRYDD
                   MOVE    TNS-NAME            TO  WRK-CHK-NAME
               END-IF
           END-IF
           .
       2001-DOJSANTEI-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（月数回数）
      *****************************************************************
       2002-DOJSANTEI-ETC-SEC      SECTION.
      *
      *    算定開始年月
           MOVE    SPA-SRYYMD(1:6)     TO  WRK-STRYM
           IF      WRK-STRMM           <   TNS-JGNCNTETCM
               COMPUTE WRK-STRYY       =   WRK-STRYY    -   1
               COMPUTE WRK-STRMM       =   WRK-STRMM
                                       +   12
                                       -  (TNS-JGNCNTETCM   -  1 )
           ELSE
               COMPUTE WRK-STRMM       =   WRK-STRMM
                                       -  (TNS-JGNCNTETCM   -  1 )
           END-IF
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL  (IDX         =   TNS-JGNCNTETCM )  OR
                          (WRK-STRYM   >   SPA-SRYYMD(1:6))
      *
      *        保険毎のコードの判定
               PERFORM 1000-SANTEI-CHK-SEC
      *        算定履歴検索
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    WRK-STRYM           TO  SANTEI-SRYYM
    **         MOVE    LNK-SC91-SRYCD      TO  SANTEI-SRYCD
               MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
               EVALUATE    TNS-SANTEIRRKKBN
                   WHEN    1
                       MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                       MOVE    ZERO                TO  SANTEI-SRYKA
                   WHEN    2
                       MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                       MOVE    ZERO                TO  SANTEI-SRYKA
                   WHEN    3
                       MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                       MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
                   WHEN    4
                       MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                       MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               END-EVALUATE
      *
               IF     (FLG-HKNSAN          =   1   )  AND
                      (SPA-HKNKBN      NOT =   ZERO)
                   MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
               ELSE
                   MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               END-IF
      *
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
                   PERFORM 920-SANTEI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
               END-IF
      *
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               IF      FLG-SANTEI          =   ZERO
      ***********  ADD     SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNT
                   MOVE    SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNTW
               ELSE
                   MOVE    ZERO                TO  WRK-SANTEI-CNTW
               END-IF
      *        労災でリハビリの時３月以内の件数はカウントしない
               IF     (SPA-HKNKBN      NOT =   ZERO  )  AND
                      (FLG-HKN-SANTEI      =   1     )
                   MOVE    WRK-SANTEI-CNTW     TO  WRK-RSI-CNT
                   PERFORM 10012-RSI-MCNT-SEC
                   MOVE    WRK-RSI-CNT         TO  WRK-SANTEI-CNTW
               END-IF
               ADD     WRK-SANTEI-CNTW     TO  WRK-SANTEI-CNT
      *
               IF      WRK-SANTEI-CNTW     >   ZERO
                   MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
                   MOVE    SANTEI-MSANTEID     TO  WRK-CHK-SRYDD
                   MOVE    WRK-SRYCD           TO  WRK-CHK-SRYCD
                   MOVE    TNS-NAME            TO  WRK-CHK-NAME
               END-IF
      *
               ADD     1                   TO  WRK-STRMM
               IF      WRK-STRMM           >   12
                   COMPUTE WRK-STRYY       =   WRK-STRYY   +   1
                   MOVE    1               TO  WRK-STRMM
               END-IF
      *
           END-PERFORM
      *
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC91-SYORIFLG   =   1
               IF      WRK-SANTEI-CNT      >=  WRK-TEI-CNT
                   COMPUTE WRK-SANTEI-CNT      =   WRK-SANTEI-CNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO            TO  WRK-SANTEI-CNT
               END-IF
           END-IF
      *
      *    算定歴の回数をチェックする
           ADD     WRK-SANTEI-CNT      TO  WRK-CNT
      *
           .
       2002-DOJSANTEI-ETC-EXT.
           EXIT.
      *****************************************************************
      *    今回診療行為チェック処理
      *****************************************************************
       2003-DOJINPUT-CHK-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   200 )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *        入院で１日上限の時、入力日のみ
               MOVE    ZERO            TO  FLG-NYU-CHK
               IF     (SPA-NYUGAIKBN       =   "1"   )  AND
                      (TNS-JGNCNT1D        >   ZERO  )  AND
                      (SPA-COM-NYU-KAI(IDX)    >   ZERO )
                   MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                   IF      SPA-COM-NYUINDAY(IDX IDX-Y2)    =   ZERO
                       MOVE    1               TO  FLG-NYU-CHK
                   END-IF
               END-IF
      *
      *
            IF     FLG-NYU-CHK          =   ZERO
               IF      LNK-SC91-KBN        =   "1"  OR  "3"
                   IF      IDX             =   LNK-SC91-GMN-IDX
                       CONTINUE
                   ELSE
                       IF     (SPA-COM-INPUTCD(IDX)
                                           =   WRK-SRYCD  )  OR
                              (SPA-COM-CHK-SRYCD (IDX)
                                           =   WRK-SRYCD  )
                           ADD     SPA-COM-KAISU(IDX)
                                               TO  WRK-DOCNT
                           MOVE    SPA-COM-CHK-SRYCD (IDX)
                                               TO  WRK-CHK-SRYCD
                       END-IF
                   END-IF
               ELSE
                   IF     (SPA-COM-INPUTCD(IDX)
                                           =   WRK-SRYCD  )  OR
                          (SPA-COM-CHK-SRYCD (IDX)
                                           =   WRK-SRYCD  )
                       ADD     SPA-COM-KAISU(IDX)
                                               TO  WRK-DOCNT
                       MOVE    SPA-COM-CHK-SRYCD (IDX)
                                               TO  WRK-CHK-SRYCD
                   END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       2003-DOJINPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
      *    算定履歴に関する項目のみ
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY22"       TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY22"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "TENSU-KEY22"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
