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
       PROGRAM-ID.                 ORCGI43.
      ********************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : チェック（Ｉ４３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.19    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "I4COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "I4SCR-SPA".
      *
           COPY    "I4SCR-SPA3".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-EXAMPLE         PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
      *
           03  FLG-HKNCOMBICHK     PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-HKNCOMBI        PIC 9(04).
           03  WRK-HKNCOMBIMEI     PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
      *
           03  WRK-NSRYKA          PIC X(02).
           03  WRK-NSRYKAMEI       PIC X(20).
      *
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-SELNUM2         PIC 9(03).
      *
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-ZZ              PIC ZZ.
      *
           03  WRK-DBPATH          PIC X(20).
      *
           03  WRK-JIDMSG          PIC X(80).
      *
      *    保険組合せワーク
           03  WRK-I43-HKNCOMB-COMB.
               05  WRK-I43-HKN-LIST    OCCURS  10.
                   07  WRK-I43-THKNCOMBINUM    PIC X(04).
                   07  WRK-I43-THKNCOMBY1      PIC X(01).
                   07  WRK-I43-THKNCOMBIMEI    PIC X(100).
                   07  WRK-I43-FLG             PIC X(01).
                   07  WRK-I43-RSI-HKNKBN      PIC X(01).
               05  WRK-I43-HKNMAX              PIC 9(04).
      *
      *    入院科ワーク
           03  WRK-I43-NSRYKA-AREA.
               05  WRK-I43-NSRYKA-LIST    OCCURS  100.
                   07  WRK-I43-NSRYKA          PIC X(02).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    雛形
       01  EXAMPLE-REC.
           COPY    "CPEXAMPLE.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
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
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           COPY    "I41.INC".
           COPY    "I42.INC".
           COPY    "I43.INC".
           COPY    "I44.INC".
           COPY    "I45.INC".
           COPY    "I46.INC".
           COPY    "I47.INC".
           COPY    "I48.INC".
           COPY    "I49.INC".
           COPY    "I4ERR.INC".
           COPY    "I4ID1.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I43.
           MOVE    SPA-WORK        TO  SPA-I4KYOUTU.
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-I4KYOUTU    TO  SPA-WORK.
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-I43         TO  SPA-FREE.
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA.
           INITIALIZE                  STS-AREA.
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I4ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF.
      *
           MOVE    SPACE               TO  LNK-KYOUTU.
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "I43"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG(1:5)
      *            文例画面より
               WHEN    "I45  "
                   MOVE    SPACE           TO  SPA-IIDCD
      *            確認画面より
               WHEN    "I4ID1"
                   PERFORM 3003-JID1-SET-SEC
                   MOVE    SPACE           TO  SPA-IIDCD
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE.
      *
           MOVE    LNK-COMMON      TO  SPA-KYOUTU.
           MOVE    LNK-SCRSPA      TO  SPA-I4.
           MOVE    SPA-WORK        TO  SPA-I4KYOUTU.
      *
           PERFORM 310-SPA-SET-SEC.
      *
           MOVE    1               TO  SPA-I43-CUR.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-IIDCD
               WHEN    "0101"
               WHEN    "0105"
      *            コメント登録の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       PERFORM 2121-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-I43-CUR
                   END-IF
               WHEN    "0104"
      *            コメント削除の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       PERFORM 2401-COMMENT-DELSYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-I43-CUR
                   END-IF
               WHEN    "0102"
               WHEN    "0103"
               WHEN    "0106"
      *            文例の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       PERFORM 2801-BUNREI-SYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-I43-CUR
                   END-IF
           END-EVALUATE.
      *
       3003-JID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-I43-AREA.
           IF      SPA-MOTOPG(1:3)    =   "I41"
               INITIALIZE                  SPA-I43-I45-AREA
           END-IF.
      *
           MOVE    SPA-NAI-SRYYM-41    TO  SPA-I43-SRYYMS.
           MOVE    SPA-GMN-SRYYM-41    TO  SPA-I43-SRYYM.
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    2               TO  SPA-I43-SYORI
               GO      TO      310-SPA-SET-EXT
           END-IF.
      *
      *    保険組合せ
           PERFORM 3101-HKNCOMBI-HEN-SEC.
      *
      *    初期編集
           PERFORM 314-SYOKI-SEC.
      *
      *    コメント編集編集
           PERFORM 3103-RECECOM-HEN-SEC.
      *
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       3101-HKNCOMBI-HEN-SEC        SECTION.
      *
           INITIALIZE                  SPA-I43-HKNCOMB-COMB.
           MOVE    ZERO                TO  SPA-I43-HKNMAX.
           MOVE    ZERO                TO  FLG-HKNCOMBICHK.
      *
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC.
           INITIALIZE                  HKNCOMBI-REC.
           MOVE    SPA-HOSPID          TO  COMB-HOSPID.
           MOVE    SPA-GMN-PTID        TO  COMB-PTID.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF.
           PERFORM UNTIL      FLG-HKNCOMBI        =   1
      *
      *        対象の保険組合せのみ編集
               MOVE    ZERO                TO  FLG-HKNCOMBICHK
               IF      COMB-DELKBN         =   "1"
                   CONTINUE
               ELSE
                   ADD     1                   TO  SPA-I43-HKNMAX
                   MOVE    COMB-HKNCOMBINUM    TO  SPA-I43-THKNCOMBINUM
                                                  (SPA-I43-HKNMAX)
                   PERFORM 31011-HKNCOMBI-HEN-SEC
                   MOVE    WRK-HKNCOMBIMEI     TO  SPA-I43-THKNCOMBIMEI
                                                  (SPA-I43-HKNMAX)
               END-IF
      *
               MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH
               PERFORM 910-HKNCOMBI-READ-SEC
           END-PERFORM.
      *
       3101-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       31011-HKNCOMBI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI.
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING.
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF.
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
               MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "PTRSIINF-KEY"     TO  ORC-DBPATH
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT           DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               END-IF
           END-IF.
      *
       31011-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集処理
      *****************************************************************
       3103-RECECOM-HEN-SEC         SECTION.
      *
           INITIALIZE                  SPA-I43-RECECOMLIST-G.
      *
           MOVE    SPACE               TO  RECECOM-REC.
           INITIALIZE                  RECECOM-REC.
           MOVE    SPA-HOSPID          TO  RECECOM-HOSPID.
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID.
           MOVE    "1"                 TO  RECECOM-NYUGAIKBN.
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "RECECOM-KEY2"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "RECECOM-KEY2"     TO  ORC-DBPATH
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF.
           MOVE    ZERO                TO  IDX.
           PERFORM UNTIL       FLG-RECECOM        =   1
               IF     (SPA-I43-SRYKA      =   "00")          OR
                      (SPA-I43-SRYKA      =   RECECOM-SRYKA)
                   ADD     1                 TO  IDX
                   MOVE    IDX               TO  SPA-I43-RECE-MAX
                   MOVE    IDX               TO  SPA-I43-TRENNUM (IDX)
                   MOVE    RECECOM-SRYKA     TO  SPA-I43-NSRYKA  (IDX)
                   MOVE    RECECOM-SRYYM     TO  SPA-I43-NSRYYM  (IDX)
                   MOVE    RECECOM-HKNCOMBI  TO  SPA-I43-NHKNCOMBI(IDX)
                   MOVE    RECECOM-COMMENT   TO  SPA-I43-TCOMMENT(IDX)
      *            入院年月
                   MOVE    RECECOM-SRYYM     TO  WRK-SYMD(1:6)
                   MOVE    "01"              TO  WRK-SYMD(7:2)
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG (1:6) TO  SPA-I43-TSRYYM (IDX)
      *            入院科
                   MOVE    RECECOM-SRYKA     TO  WRK-NSRYKA
                   PERFORM 810-NSRYKA-SET-SEC
                   MOVE    WRK-NSRYKAMEI     TO  SPA-I43-TSRYKA (IDX)
      *            保険組合せ番号
                   MOVE    RECECOM-HKNCOMBI  TO  WRK-HKNCOMBI
                   PERFORM 820-HKNCOMBI-SET-SEC
                   MOVE    WRK-HKNCOMBIMEI   TO  SPA-I43-THKNCOMBI(IDX)
      *            入院日
                   IF      RECECOM-SRYDD       =   ZERO
                       MOVE    SPACE           TO  SPA-I43-TSRYDD (IDX)
                   ELSE
                       MOVE    RECECOM-SRYDD   TO  SPA-I43-TSRYDD (IDX)
                   END-IF
               END-IF
      *
               MOVE    "RECECOM-KEY2"      TO  ORC-DBPATH
               PERFORM 920-RECECOM-READ-SEC
           END-PERFORM.
      *
           MOVE    ZERO                TO  SPA-I43-SELNUM.
      *
           IF      SPA-MOTOPG(1:3)    NOT =   "I45"
               MOVE    SPACE               TO  SPA-I43-COMMENT
               MOVE    ZERO                TO  SPA-I43-BUNNUM
           END-IF.
      *
       3103-RECECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       314-SYOKI-SEC             SECTION.
      *
      *    保険組合せの再編集
           INITIALIZE                  WRK-I43-HKNCOMB-COMB.
           INITIALIZE                  WRK-I43-NSRYKA-AREA.
           MOVE    ZERO                TO  IDY.
      *
      *    入院科セット
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKA-MAX-41
               ADD     1               TO  IDY
               MOVE    SPA-GMN-SRYKA-L-41 (IDX)
                                       TO  WRK-I43-NSRYKA (IDY)
               MOVE    IDY             TO  IDZ
           END-PERFORM.
      *
           MOVE    ZERO                TO  WRK-I43-HKNMAX.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >    SPA-I43-HKNMAX
               MOVE    SPA-I43-THKNCOMBINUM (IDX)
                                      TO  WRK-I43-THKNCOMBINUM (IDX)
               MOVE    SPA-I43-THKNCOMBIMEI (IDX)
                                      TO  WRK-I43-THKNCOMBIMEI (IDX)
           END-PERFORM.
           MOVE    SPA-I43-HKNMAX     TO  WRK-I43-HKNMAX.
      *
      *    入院会計を検索する  医療機関、患者ＩＤ、年月
           INITIALIZE                  NYUINACCT-REC.
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID.
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID.
           MOVE    SPA-I43-SRYYMS      TO  NACCT-SRYYM.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY12"   TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY12"   TO  ORC-DBPATH
               PERFORM 970-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF.
      *
           MOVE    ZERO                TO  WRK-HKNCOMBI.
           MOVE    ZERO                TO  IDX3.
           MOVE    SPACE               TO  WRK-NSRYKA.
           PERFORM UNTIL    FLG-NYUINACCT     =   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-I43-HKNMAX
                   IF      NACCT-ZAISKBKBN   =   "5"
                       PERFORM VARYING     IDX1    FROM    1   BY  1
                               UNTIL       IDX1    >   31
                           IF     (NACCT-DAY (IDX1) >= 1)   AND
                                  (NACCT-DAY (IDX1) <= 10)
                               MOVE    NACCT-DAY (IDX1)   TO  IDX2
                               MOVE    "1"                TO
                                                     WRK-I43-FLG (IDX2)
                               IF     IDX3   =   ZERO
                                   MOVE    NACCT-DAY (IDX1)   TO  IDX3
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
               MOVE    "NYUINACCT-KEY12"   TO  ORC-DBPATH
               PERFORM 970-NYUINACCT-READ-SEC
           END-PERFORM.
      *
      *    入院のあった保険組合せのみ表示する
           INITIALIZE                  SPA-I43-HKNCOMB-COMB.
           MOVE    ZERO                TO  SPA-I43-HKNMAX.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >    10
               IF      WRK-I43-FLG  (IDX)  =   "1"
                   ADD     1                TO  SPA-I43-HKNMAX
                   MOVE    WRK-I43-THKNCOMBINUM (IDX)
                                      TO  SPA-I43-THKNCOMBINUM
                                                (SPA-I43-HKNMAX)
                   MOVE    WRK-I43-THKNCOMBIMEI (IDX)
                                      TO  SPA-I43-THKNCOMBIMEI
                                                (SPA-I43-HKNMAX)
                  IF     IDX   =   IDX3
                      MOVE    SPA-I43-HKNMAX   TO  IDX4
                  END-IF
               END-IF
           END-PERFORM.
      *
      *    初期編集
           IF      SPA-MOTOPG(1:3)     =   "I41"
               MOVE    SPA-GMN-SRYKA-IG-41     TO  SPA-I43-SRYKA-G
               IF     IDX4   =   ZERO
                   MOVE    01                TO  IDX4
               END-IF
               MOVE    SPA-I43-HKN-LIST(IDX4)  TO  SPA-I43-HKNCOMBI-G
           END-IF.
      *
       314-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院科名編集処理
      *****************************************************************
       810-NSRYKA-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-SRYKA-MAX-41
               IF      WRK-NSRYKA          =   SPA-GMN-SRYKA-L-41 (IDY)
                   MOVE    SPA-GMN-SRYKAMEI-L-41 (IDY)
                                                   TO  WRK-NSRYKAMEI
                   MOVE    SPA-GMN-SRYKA-MAX-41    TO  IDY
               END-IF
           END-PERFORM.
      *
       810-NSRYKA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名編集処理
      *****************************************************************
       820-HKNCOMBI-SET-SEC             SECTION.
      *
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC.
           INITIALIZE                  HKNCOMBI-REC.
           MOVE    SPA-HOSPID          TO  COMB-HOSPID.
           MOVE    SPA-GMN-PTID        TO  COMB-PTID.
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF.
           IF      FLG-HKNCOMBI        =   ZERO
               PERFORM 31011-HKNCOMBI-HEN-SEC
           ELSE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           END-IF.
      *
       820-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF.
      *
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE              TO  I43.
      *
      *    画面編集
           MOVE    SPA-I43-SRYYM      TO  I43-SRYYM.
           MOVE    SPA-I43-COMMENT    TO  I43-COMMENT.
      *
      *    入院科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-MAX-41
               MOVE    SPA-GMN-SRYKA-LIST-L-41 (IDX)
                                           TO  I43-SRYKA-ITEM (IDX)
           END-PERFORM.
           MOVE    SPA-GMN-SRYKA-MAX-41 TO  I43-SRYKA-COUNT.
           MOVE    SPA-I43-SRYKA-G      TO  I43-SRYKA.
      *    保険組合せ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-I43-HKNMAX
               MOVE    SPA-I43-HKN-LIST(IDX)
                                           TO  I43-HKNCOMBI-ITEM (IDX)
           END-PERFORM.
           MOVE    SPA-I43-HKNMAX     TO  I43-HKNCOMBI-COUNT.
           MOVE    SPA-I43-HKNCOMBI-G TO  I43-HKNCOMBI.
      *
           EVALUATE    SPA-I43-SYORI
               WHEN    0
                   MOVE    SPACE               TO  I43-MIDASI
                   MOVE    SPACE               TO  I43-MIDASI-STYLE
               WHEN    1
                   MOVE    "＜変　更＞"        TO  I43-MIDASI
                   MOVE    "red"               TO  I43-MIDASI-STYLE
               WHEN    2
                   MOVE    "＜文　例＞"        TO  I43-MIDASI
                   MOVE    "red"               TO  I43-MIDASI-STYLE
           END-EVALUATE.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-I43-RECE-MAX
               MOVE    SPA-I43-TRENNUM (IDX)  TO  WRK-ZZZ
               MOVE    WRK-ZZZ                TO  I43-TRENNUM (IDX)
               MOVE    SPA-I43-TSRYYM  (IDX)  TO  I43-TSRYYM  (IDX)
               MOVE    SPA-I43-TSRYKA  (IDX)  TO  I43-TSRYKA  (IDX)
               MOVE    SPA-I43-THKNCOMBI(IDX) TO  I43-THKNCOMBI(IDX)
               IF      SPA-I43-TSRYDD  (IDX)  =   ZERO
                   MOVE    SPACE               TO  I43-TSRYDD  (IDX)
               ELSE
                   MOVE    SPA-I43-TSRYDD  (IDX)
                                               TO  I43-TSRYDD  (IDX)
               END-IF
               MOVE    SPA-I43-TCOMMENT(IDX)  TO  I43-TCOMMENT (IDX)
      *
               IF      SPA-I43-SELNUM     =   SPA-I43-TRENNUM (IDX)
                   MOVE    "T"             TO  I43-RECE-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  I43-RECE-SELECT (IDX)
               END-IF
           END-PERFORM.
           MOVE    SPA-I43-RECE-MAX   TO  I43-RECE-COUNT.
      *
           MOVE    SPA-I43-SELNUM     TO  I43-SELNUM.
           MOVE    SPA-I43-BUNNUM     TO  I43-BUNNUM.
      *
           PERFORM 5001-MAPCUR-SEC.
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-I43-CUR
               WHEN    1
                   MOVE    "COMMENT"           TO  MCP-WIDGET
               WHEN    2
                   MOVE    "SRYKA"             TO  MCP-WIDGET
               WHEN    3
                   MOVE    "HKNCOMBI"          TO  MCP-WIDGET
               WHEN    4
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    5
                   MOVE    "BUNNUM"            TO  MCP-WIDGET
               WHEN    6
                   MOVE    "B12"               TO  MCP-WIDGET
               WHEN    7
                   MOVE    "B08"               TO  MCP-WIDGET
               WHEN    8
                   MOVE    "B05"               TO  MCP-WIDGET
           END-EVALUATE.
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEA-SEC
      *        コメント削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 240-COMMENT-DEL-SEC
      *        変更
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 210-HENKOU-SEC
      *        コピー
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 250-COPY-SEC
      *        文例選択画面へ
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 250-BUNREI-I45-SEC
      *        文例登録
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 280-BUNREI-ADD-SEC
      *        内容確定
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 209-COMMENT-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 212-TOUROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        入院科
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 400-NSRYKA-SEC
      *        保険組合せ
               WHEN    "ACTIVATE"      ALSO    "HKNCOMBI"
                   PERFORM 410-HKNCOMBI-SEC
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 420-SELNUM-SEC
      *        文例番号
               WHEN    "ACTIVATE"      ALSO    "BUNNUM"
                   PERFORM 430-BUNNUM-SEC
      *        明細選択
               WHEN    ANY             ALSO    "RECELIST"
                   PERFORM 440-RECELIST-SEC
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント確定処理
      *****************************************************************
       209-COMMENT-SEC                    SECTION.
      *
           MOVE    1                   TO  SPA-I43-CUR.
      *
           MOVE    I43-COMMENT         TO  SPA-I43-COMMENT.
           PERFORM 800-COMMENT-CHK-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      209-COMMENT-EXT
           END-IF.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    7                   TO  SPA-I43-CUR
           ELSE
               MOVE    6                   TO  SPA-I43-CUR
           END-IF.
      *
       209-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院科処理
      *****************************************************************
       400-NSRYKA-SEC                   SECTION.
      *
           MOVE    2                   TO  SPA-I43-CUR.
      *
           MOVE    I43-SRYKA           TO  SPA-I43-SRYKA-G.
      *
           MOVE    ZERO                TO  FLG-OK.
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKA-MAX-41
               IF      SPA-I43-SRYKA      =   SPA-GMN-SRYKA-L-41 (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST-L-41 (IDX)
                                           TO  SPA-I43-SRYKA-G
               END-IF
           END-PERFORM.
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      WRK-I43-NSRYKA (IDX)   =   SPACE
               IF    WRK-I43-NSRYKA (IDX)     =  SPA-I43-SRYKA
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM.
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    1                   TO  SPA-I43-CUR
               PERFORM 3103-RECECOM-HEN-SEC
           END-IF.
      *
       400-NSRYKA-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ処理
      *****************************************************************
       410-HKNCOMBI-SEC                 SECTION.
      *
           MOVE    3                   TO  SPA-I43-CUR.
      *
           MOVE    I43-HKNCOMBI        TO  SPA-I43-HKNCOMBI-G.
      *
           MOVE    ZERO                TO  FLG-OK.
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-I43-HKNMAX
               IF      SPA-I43-HKNCOMBINUM    =
                                           SPA-I43-THKNCOMBINUM (IDX)
                   MOVE    SPA-I43-HKN-LIST  (IDX)
                                           TO  SPA-I43-HKNCOMBI-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM.
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      410-HKNCOMBI-EXT
           END-IF.
      *
           MOVE    1                   TO  SPA-I43-CUR.
      *
       410-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       420-SELNUM-SEC                 SECTION.
      *
           MOVE    4                   TO  SPA-I43-CUR.
      *
           MOVE    I43-SELNUM          TO  SPA-I43-SELNUM.
      *
           IF      SPA-I43-SELNUM     =   ZERO
               GO      TO      420-SELNUM-EXT
           END-IF.
           IF      SPA-I43-SELNUM     >   SPA-I43-RECE-MAX
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      420-SELNUM-EXT
           END-IF.
      *
           EVALUATE    SPA-I43-SYORI
      *        変更処理
               WHEN    1
                   PERFORM 2101-HENKOU-SYORI-SEC
      *        コピー処理
               WHEN    3
                   PERFORM 2501-COPY-SYORI-SEC
      *
               WHEN    OTHER
                   MOVE    8                   TO  SPA-I43-CUR
           END-EVALUATE.
      *
       420-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       440-RECELIST-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM.
           MOVE    ZERO                TO  WRK-SELNUM2.
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-I43-RECE-MAX
              IF      I43-RECE-SELECT (IDX)   =  "T"
                  IF      SPA-I43-TRENNUM  (IDX)  =   SPA-I43-SELNUM
                      MOVE    SPA-I43-TRENNUM  (IDX)  TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-I43-TRENNUM  (IDX)  TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM.
      *
           IF      WRK-SELNUM2     NOT =   ZERO
               MOVE    WRK-SELNUM2         TO  SPA-I43-SELNUM
           ELSE
               MOVE    WRK-SELNUM          TO  SPA-I43-SELNUM
           END-IF.
      *
           MOVE    SPA-I43-SELNUM     TO  I43-SELNUM.
           PERFORM 420-SELNUM-SEC.
      *
       440-RECELIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例番号処理
      *****************************************************************
       430-BUNNUM-SEC                 SECTION.
      *
           MOVE    5                   TO  SPA-I43-CUR.
      *
           MOVE    I43-BUNNUM          TO  SPA-I43-BUNNUM.
      *
           MOVE    7                   TO  SPA-I43-CUR.
      *
       430-BUNNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEA-SEC                   SECTION.
      *
           PERFORM                     310-SPA-SET-SEC.
      *
           MOVE    1                  TO  SPA-I43-CUR.
      *
       220-CLEA-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       212-TOUROKU-SEC                   SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      212-TOUROKU-EXT
           END-IF.
      *
           MOVE    I43-COMMENT        TO  SPA-I43-COMMENT.
      *
           IF      SPA-I43-HKNCOMBINUM    =   SPACE  AND
                   SPA-I43-HKNMAX         =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               GO      TO      212-TOUROKU-EXT
           END-IF.
           IF      SPA-I43-HKNCOMBINUM    =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      212-TOUROKU-EXT
           END-IF.
      *
           IF      SPA-I43-COMMENT    =   SPACE
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      212-TOUROKU-EXT
           END-IF.
           PERFORM 800-COMMENT-CHK-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      212-TOUROKU-EXT
           END-IF.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   IDZ
              IF    WRK-I43-NSRYKA (IDY)       =   SPA-I43-SRYKA
                  PERFORM 2120-RECECOM-SEL-SEC
              END-IF
           END-PERFORM.
      *
           IF      FLG-RECECOM         =   ZERO
               MOVE    "0105"              TO  SPA-IIDCD
           ELSE
               MOVE    "0101"              TO  SPA-IIDCD
           END-IF.
      *
       212-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントチェック処理
      *****************************************************************
       2120-RECECOM-SEL-SEC          SECTION.
      *
           MOVE    SPACE                 TO  RECECOM-REC.
           INITIALIZE                    RECECOM-REC.
           MOVE    SPA-HOSPID            TO  RECECOM-HOSPID.
           MOVE    SPA-GMN-PTID          TO  RECECOM-PTID.
           MOVE    "1"                   TO  RECECOM-NYUGAIKBN.
           MOVE    WRK-I43-NSRYKA (IDY)  TO  RECECOM-SRYKA.
           MOVE    SPA-I43-SRYYMS        TO  RECECOM-SRYYM.
           MOVE    SPA-I43-HKNCOMBINUM   TO  RECECOM-HKNCOMBI.
           MOVE    "00"                  TO  RECECOM-SRYDD.
      *
           MOVE    RECECOM-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"            TO  MCP-FUNC.
           MOVE    "RECECOM-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "RECECOM-KEY"      TO  ORC-DBPATH
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF.
      *
       2120-RECECOM-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       2121-TOUROKU-SYORI-SEC          SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   IDZ
              IF    WRK-I43-NSRYKA (IDY)       =   SPA-I43-SRYKA
                  PERFORM 2120-RECECOM-SEL-SEC
                  IF      FLG-RECECOM         =   ZERO
      *               更新
                      MOVE    SPA-I43-COMMENT      TO  RECECOM-COMMENT
      *
                      MOVE    RECECOM-REC          TO  MCPDATA-REC
                      MOVE    "DBUPDATE"           TO  MCP-FUNC
                      MOVE    "RECECOM-KEY"        TO  ORC-DBPATH
                      CALL    "ORCMCPSUB"          USING
                                                   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
                      IF      MCP-RC          NOT =   ZERO
                          DISPLAY "I43 RECECOM UPD ERR:" MCP-RC "."
                      END-IF
                  ELSE
      *               追加
                      MOVE    SPACE                TO  RECECOM-REC
                      INITIALIZE                   RECECOM-REC
                      MOVE    SPA-HOSPID           TO  RECECOM-HOSPID
                      MOVE    SPA-GMN-PTID         TO  RECECOM-PTID
                      MOVE    "1"                  TO  RECECOM-NYUGAIKBN
                      MOVE    WRK-I43-NSRYKA (IDY) TO  RECECOM-SRYKA
                      MOVE    SPA-I43-SRYYMS       TO  RECECOM-SRYYM
                      MOVE    SPA-I43-HKNCOMBINUM  TO  RECECOM-HKNCOMBI
                      MOVE    "00"                 TO  RECECOM-SRYDD
                      MOVE    SPA-I43-COMMENT      TO  RECECOM-COMMENT
      *
                      MOVE    RECECOM-REC          TO  MCPDATA-REC
                      MOVE    "DBINSERT"           TO  MCP-FUNC
                      MOVE    "RECECOM-KEY"        TO  ORC-DBPATH
                      CALL    "ORCMCPSUB"          USING
                                                   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
                      IF      MCP-RC          NOT =   ZERO
                          DISPLAY "I43 RECECOM INS ERR:" MCP-RC "."
                      END-IF
                  END-IF
              END-IF
           END-PERFORM.
      *    再表示
           PERFORM                     310-SPA-SET-SEC.
      *
           MOVE    1                   TO  SPA-I43-CUR.
      *
       2121-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例選択画面へ　処理
      *****************************************************************
       250-BUNREI-I45-SEC                   SECTION.
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET.
      *
           MOVE    "I43"               TO  SPA-MOTOPG.
           MOVE    "I45"               TO  SPA-SAKIPG.
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "I45"               TO  MCP-WINDOW.
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
           MOVE    SPA-AREA            TO  SPA-COMMON.
           MOVE    SPA-I43             TO  SPA-FREE.
      *
           MOVE    SPACE               TO  LINKAREA.
           MOVE    SPA-KYOUTU          TO  LNK-COMMON.
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
           MOVE    SPA-I4              TO  LNK-SCRSPA.
      *
           PERFORM 900-PUT-WINDOW,
      *
           MOVE    1                   TO  FLG-END.
      *
       250-BUNREI-I45-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントチェック処理
      *****************************************************************
       800-COMMENT-CHK-SEC                   SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA.
           MOVE    "1"                 TO  KANACHK-SYORI.
           MOVE    SPA-I43-COMMENT     TO  KANACHK-MAE-INPUT.
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA.
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *        全角以外エラー
               MOVE    "0013"          TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-I43-COMMENT
           END-IF.
      *
       800-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例登録　処理
      *****************************************************************
       280-BUNREI-ADD-SEC                   SECTION.
      *
           MOVE    I43-COMMENT         TO  SPA-I43-COMMENT.
      *
           IF      SPA-I43-COMMENT    =   SPACE
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      280-BUNREI-ADD-EXT
           END-IF.
           PERFORM 800-COMMENT-CHK-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      280-BUNREI-ADD-EXT
           END-IF.
      *
           IF      SPA-I43-BUNNUM     =   ZERO
               MOVE    "0102"              TO  SPA-IIDCD
           ELSE
               PERFORM 28001-EXAMPLE-CHK-SEC
               IF      FLG-EXAMPLE         =   ZERO
                   MOVE    "0103"              TO  SPA-IIDCD
               ELSE
                   MOVE    "0106"              TO  SPA-IIDCD
               END-IF
           END-IF.
      *
       280-BUNREI-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    雛形有無チェック処理
      *****************************************************************
       28001-EXAMPLE-CHK-SEC                   SECTION.
      *
      *    雛形有無
           MOVE    SPACE               TO  EXAMPLE-REC.
           INITIALIZE                  EXAMPLE-REC.
           MOVE    SPA-HOSPID          TO  EXAMPLE-HOSPID.
           MOVE    "1"                 TO  EXAMPLE-KUBUN.
           MOVE    SPA-I43-BUNNUM      TO  EXAMPLE-RENNUM.
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "EXAMPLE-KEY"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "EXAMPLE-KEY"       TO  ORC-DBPATH
               PERFORM 930-EXAMPLE-READ-SEC
           ELSE
               MOVE    1               TO  FLG-EXAMPLE
               INITIALIZE                  EXAMPLE-REC
           END-IF.
      *
       28001-EXAMPLE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例登録　処理
      *****************************************************************
       2801-BUNREI-SYORI-SEC                   SECTION.
      *
           IF      SPA-I43-BUNNUM     =   ZERO
      *        最大連番決定
               MOVE    SPACE               TO  EXAMPLE-REC
               INITIALIZE                  EXAMPLE-REC
               MOVE    SPA-HOSPID          TO  EXAMPLE-HOSPID
               MOVE    "1"                 TO  EXAMPLE-KUBUN
      *
               MOVE    EXAMPLE-REC         TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "EXAMPLE-KEY2"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "EXAMPLE-KEY2"      TO  ORC-DBPATH
                   PERFORM 930-EXAMPLE-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-EXAMPLE
                   INITIALIZE                  EXAMPLE-REC
               END-IF
               MOVE    EXAMPLE-RENNUM      TO  SPA-I43-BUNNUM
               IF      SPA-I43-BUNNUM     =   999
                   MOVE    "0010"              TO  SPA-ERRCD
                   GO      TO      2801-BUNREI-SYORI-EXT
               END-IF
               ADD     1                   TO  SPA-I43-BUNNUM
           END-IF.
      *    雛形有無
           PERFORM 28001-EXAMPLE-CHK-SEC.
           IF      FLG-EXAMPLE         =   ZERO
      *        更新
               MOVE    SPA-I43-COMMENT    TO  EXAMPLE-COMMENT
      *
               MOVE    EXAMPLE-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "EXAMPLE-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "I43 EXAMPLE UPD ERR:" MCP-RC "."
               END-IF
           ELSE
      *        追加
               MOVE    SPACE               TO  EXAMPLE-REC
               INITIALIZE                  EXAMPLE-REC
               MOVE    SPA-HOSPID          TO  EXAMPLE-HOSPID
               MOVE    "1"                 TO  EXAMPLE-KUBUN
               MOVE    SPA-I43-BUNNUM      TO  EXAMPLE-RENNUM
               MOVE    SPA-I43-COMMENT     TO  EXAMPLE-COMMENT
      *
               MOVE    EXAMPLE-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "EXAMPLE-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "I43 EXAMPLE INS ERR:" MCP-RC "."
               END-IF
           END-IF.
      *
           MOVE    SPACE               TO  SPA-I43-COMMENT.
           MOVE    ZERO                TO  SPA-I43-SELNUM.
           MOVE    ZERO                TO  SPA-I43-BUNNUM.
           MOVE    1                   TO  SPA-I43-CUR.
      *
       2801-BUNREI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除　処理
      *****************************************************************
       240-COMMENT-DEL-SEC                   SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      240-COMMENT-DEL-EXT
           END-IF.
      *
           IF      SPA-I43-SELNUM     =   ZERO    OR
                                       >   SPA-I43-RECE-MAX
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO      240-COMMENT-DEL-EXT
           END-IF.
      *
           MOVE    "0104"              TO  SPA-IIDCD.
      *
       240-COMMENT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除処理
      *****************************************************************
       2401-COMMENT-DELSYORI-SEC                   SECTION.
      *
           MOVE    SPA-I43-SELNUM     TO  IDX.
      *
           MOVE    SPACE               TO  RECECOM-REC.
           INITIALIZE                  RECECOM-REC.
           MOVE    SPA-HOSPID          TO  RECECOM-HOSPID.
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID.
           MOVE    "1"                 TO  RECECOM-NYUGAIKBN.
           MOVE    SPA-I43-NSRYKA (IDX)
                                       TO  RECECOM-SRYKA.
           MOVE    SPA-I43-NSRYYM (IDX)
                                       TO  RECECOM-SRYYM.
           MOVE    SPA-I43-NHKNCOMBI (IDX)
                                       TO  RECECOM-HKNCOMBI.
           MOVE    "00"                TO  RECECOM-SRYDD.
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC.
           MOVE    "DBDELETE"          TO  MCP-FUNC.
           MOVE    "RECECOM-KEY"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "I43 RECECOM DEL ERR:" MCP-RC "."
           END-IF.
      *    再表示
           PERFORM                     310-SPA-SET-SEC.
      *
       2401-COMMENT-DELSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更　処理
      *****************************************************************
       210-HENKOU-SEC                   SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      210-HENKOU-EXT
           END-IF.
      *
           IF      SPA-I43-SELNUM     >   SPA-I43-RECE-MAX
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO      210-HENKOU-EXT
           END-IF.
      *
           MOVE    1                   TO  SPA-I43-SYORI.
           IF      SPA-I43-SELNUM     =   ZERO
               MOVE    4               TO  SPA-I43-CUR
           ELSE
               PERFORM 2101-HENKOU-SYORI-SEC
           END-IF.
      *
       210-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更　処理
      *****************************************************************
       2101-HENKOU-SYORI-SEC                   SECTION.
      *
           MOVE    SPA-I43-TSRYYM  (SPA-I43-SELNUM)
                                       TO  SPA-I43-SRYYM.
           MOVE    SPA-I43-NSRYYM  (SPA-I43-SELNUM)
                                       TO  SPA-I43-SRYYMS.
           MOVE    SPA-I43-NSRYKA  (SPA-I43-SELNUM)
                                       TO  I43-SRYKA.
           MOVE    SPA-I43-NHKNCOMBI  (SPA-I43-SELNUM)
                                       TO  I43-HKNCOMBI.
           MOVE    SPA-I43-TCOMMENT(SPA-I43-SELNUM)
                                       TO  SPA-I43-COMMENT.
      *
           MOVE    1                   TO  SPA-I43-CUR.
      *
       2101-HENKOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コピー　処理
      *****************************************************************
       250-COPY-SEC                   SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      250-COPY-EXT
           END-IF.
      *
           IF      SPA-I43-SELNUM     >   SPA-I43-RECE-MAX
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO      250-COPY-EXT
           END-IF.
      *
           MOVE    3                   TO  SPA-I43-SYORI.
           IF      SPA-I43-SELNUM     =   ZERO
               MOVE    4               TO  SPA-I43-CUR
           ELSE
               PERFORM 2501-COPY-SYORI-SEC
           END-IF.
      *
       250-COPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントコピー処理
      *****************************************************************
       2501-COPY-SYORI-SEC                   SECTION.
      *
           MOVE    SPA-I43-TCOMMENT(SPA-I43-SELNUM)
                                       TO  SPA-I43-COMMENT.
      *
           MOVE    1                   TO  SPA-I43-CUR.
      *
       2501-COPY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I41"               TO  SPA-SAKIPG.
           MOVE    "I43"               TO  SPA-MOTOPG.
      *
           MOVE    SPACE               TO  LINKAREA.
           MOVE    SPA-KYOUTU          TO  LNK-COMMON.
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
           MOVE    SPA-I4              TO  LNK-SCRSPA.
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
            PERFORM 500-GMNHEN-SEC.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF.
      *
           IF      SPA-IIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF.
      *
           MOVE    SPACE               TO  LNK-KYOUTU.
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I43"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入院科選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "保険組合せ選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "選択番号が有りません"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "文例登録番号エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "入院年月に診療行為がありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "コメントは登録できません。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0007"
                   MOVE    "コメント内容が未入力です。入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE
                   "文例が９９９件あります。これ以上追加できません"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE
                   "患者が選択されてません。文例の処理しかできません"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE
                   "対象の入院科と保険組合せは受診が有りません。"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "コメントは全角で入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "アフターケアの時、入院日を選択して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "アフターケア入院日選択番号がありません"
                                       TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  I4ERR.
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE.
           MOVE    SPA-ERRMSG          TO  I4ERR-ERRMSG.
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "I43"               TO  SPA-MOTOPG.
           MOVE    "I4ERR"             TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-IIDCD
               WHEN    "0101"
                   MOVE    "入院年月のレセプトコメントを入力します。"
                                       TO  WRK-JIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-JIDMSG(41:)
               WHEN    "0102"
                   MOVE    "文例へ追加登録します。"
                                       TO  WRK-JIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-JIDMSG(23:)
               WHEN    "0103"
                   MOVE    "文例登録番号の文例を更新します。"
                                       TO  WRK-JIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-JIDMSG(39:)
               WHEN    "0106"
                   MOVE    "文例を文例登録番号で追加します。"
                                       TO  WRK-JIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-JIDMSG(39:)
               WHEN    "0104"
                   MOVE    "選択されているコメントを削除します。"
                                       TO  WRK-JIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-JIDMSG(37:)
               WHEN    "0105"
                   MOVE    "選択されているコメントを変更します。"
                                       TO  WRK-JIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-JIDMSG(37:)
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-JIDMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  SPA-I4ID-FLG.
      *
           MOVE    SPACE               TO  I4ID1.
           MOVE    SPA-IIDCD           TO  I4ID1-ID1CODE.
           MOVE    WRK-JIDMSG          TO  I4ID1-ID1MSG.
           MOVE    "B12"               TO  MCP-WIDGET.
      *
           MOVE    "I43"               TO  SPA-MOTOPG.
           MOVE    "I4ID1"             TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ID1"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF.
      *
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメント読込
      *****************************************************************
       920-RECECOM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  RECECOM-REC
               MOVE    ZERO                TO  FLG-RECECOM
           ELSE
               INITIALIZE                      RECECOM-REC
               MOVE    1                   TO  FLG-RECECOM
           END-IF.
      *
       920-RECECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    雛形読込
      *****************************************************************
       930-EXAMPLE-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  EXAMPLE-REC
               MOVE    ZERO                TO  FLG-EXAMPLE
           ELSE
               INITIALIZE                      EXAMPLE-REC
               MOVE    1                   TO  FLG-EXAMPLE
           END-IF.
      *
       930-EXAMPLE-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       970-NYUINACCT-READ-SEC       SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF.
      *
       970-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF.
      *
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCMCPSUB"   USING
                                MCPAREA
                                ORCMCPAREA
                                MCPDATA-REC.
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
