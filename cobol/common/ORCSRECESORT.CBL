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
       PROGRAM-ID.             ORCSRECESORT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : レセ用器材再編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/03/13    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(04).
           03  IDY4                PIC 9(04).
           03  IDXZ                PIC 9(04).
      *
           03  IDX-HZN             PIC 9(04).
           03  IDX-HOSI            PIC 9(04).
      *
      *    一時領域
      *01  WRK-AREA.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    レセ器材再編集パラメタ
           COPY    "CPORCSRECESORT.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSRECESORTAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      ***  INITIALIZE                  WRK-AREA
      *
           INITIALIZE                  ORCSRECESORT-OUT-AREA
      *    診療会計附加情報検索
           PERFORM 100-INIT-SYORI-SEC
           IF      ORCSRECESORT-ERRCD  =   ZERO
      *        器材再編集
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療会計附加情報検索処理
      *****************************************************************
       100-INIT-SYORI-SEC              SECTION.
      *
      *    診療会計附加情報検索
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ORCSRECESORT-HOSPID TO  ACCTP-HOSPID
           MOVE    ORCSRECESORT-NYUGAIKBN
                                       TO  ACCTP-NYUGAIKBN
           MOVE    ORCSRECESORT-PTID   TO  ACCTP-PTID
           MOVE    ORCSRECESORT-SRYYM  TO  ACCTP-SRYYM
           MOVE    ORCSRECESORT-ZAINUM TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYACCTPLUS     =   1
               MOVE    1                   TO  ORCSRECESORT-ERRCD
           END-IF
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材再編集処理
      *****************************************************************
       200-MAIN-SYORI-SEC             SECTION.
      *
      *    酸素補正率決定
           MOVE    ZERO                TO  IDX-HOSI
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   50  )
                         OR   (ORCSRECESORT-IN-SRYCD (IDX2)  =   SPACE)
      *        酸素補正率
               IF      ORCSRECESORT-IN-SRYCD (IDX2)
                                               =   "770020070"
                   MOVE    IDX2                TO  IDX-HOSI
                   MOVE    50                  TO  IDX2
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  IDY1
           MOVE    ZERO                TO  IDY2
           MOVE    ZERO                TO  IDY3
           MOVE    ZERO                TO  IDY4
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   50  )
                         OR   (ORCSRECESORT-IN-SRYCD (IDX2)  =   SPACE)
      *        器材
               IF      ORCSRECESORT-IN-SRYCD (IDX2)(1:1)   =   "7"
      *            フィルム判定
                   IF     (ORCSRECESORT-IN-DATAKBN(IDX2)   =   "3" ) OR
                          (ORCSRECESORT-IN-SRYCD (IDX2)
                                                       =   "799990070")
                       MOVE    1               TO  IDX
                       ADD     1               TO  IDY1
                       MOVE    IDY1            TO  IDXZ
                       PERFORM 2009-TBL-HEN-SEC
                   ELSE
      *            フィルム以外
      *                酸素補正率は対象外とする
                       IF      ORCSRECESORT-IN-SRYCD (IDX2)
                                               =   "770020070"
                           CONTINUE
                       ELSE
                           PERFORM 2001-KIZAI-HEN-SEC
                       END-IF
                   END-IF
               END-IF
      *        コメント
               IF     (ORCSRECESORT-IN-SRYCD (IDX2)(1:1)   =   "8")
                   IF    IDX                   >   ZERO
                       PERFORM 2002-COMMENT-HEN-SEC
                   END-IF
               END-IF
      *        器材・コメント以外
               IF     (ORCSRECESORT-IN-SRYCD (IDX2)(1:1)   =   "8"
                                                           OR  "7")
                   MOVE    ZERO                TO  IDX
               END-IF
           END-PERFORM
      *
      *    テーブル数
           MOVE    IDY1                TO  ORCSRECESORT-TBLMAX (1)
           MOVE    IDY2                TO  ORCSRECESORT-TBLMAX (2)
           MOVE    IDY3                TO  ORCSRECESORT-TBLMAX (3)
           MOVE    IDY4                TO  ORCSRECESORT-TBLMAX (4)
      *    点数編集
      *    酸素
           MOVE    ACCTP-SANSOTEN      TO  ORCSRECESORT-ZAITEN (2)
      *    窒素
           MOVE    ACCTP-CHISOTEN      TO  ORCSRECESORT-ZAITEN (3)
      *
      *    手技毎の点数を集計（本来は別々にレセ電記載する）
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
      *        フィルム
               ADD     ACCTP-FIRTEN (IDX)  TO  ORCSRECESORT-ZAITEN (1)
      *        その他器材点数（フィルム・酸素・窒素以外）
               ADD     ACCTP-KIZAITEN(IDX) TO  ORCSRECESORT-ZAITEN (4)
           END-PERFORM
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    フィルム以外編集処理
      *****************************************************************
       2009-TBL-HEN-SEC            SECTION.
      *
           MOVE    ORCSRECESORT-IN-SRYCD (IDX2)
                                       TO  ORCSRECESORT-SRYCD
                                                        (IDX IDXZ)
      *    データ区分
           MOVE    ORCSRECESORT-IN-DATAKBN (IDX2)
                                       TO  ORCSRECESORT-DATAKBN
                                                        (IDX IDXZ)
      *    数量
           MOVE    ORCSRECESORT-IN-SRYSURYO (IDX2)
                                       TO  ORCSRECESORT-SRYSURYO
                                                        (IDX IDXZ)
      *    回数
           MOVE    ORCSRECESORT-IN-SRYKAISU (IDX2)
                                       TO  ORCSRECESORT-SRYKAISU
                                                        (IDX IDXZ)
      *    単位コード
           MOVE    ORCSRECESORT-IN-TANICD (IDX2)
                                       TO  ORCSRECESORT-TANICD
                                                        (IDX IDXZ)
      *    点数識別区分
           MOVE    ORCSRECESORT-IN-TENSIKIBETU (IDX2)
                                       TO  ORCSRECESORT-TENSIKIBETU
                                                        (IDX IDXZ)
      *    点数
           MOVE    ORCSRECESORT-IN-TEN (IDX2)
                                       TO  ORCSRECESORT-TEN
                                                        (IDX IDXZ)
      *    漢字名称
           MOVE    ORCSRECESORT-IN-NAME (IDX2)
                                       TO  ORCSRECESORT-NAME
                                                        (IDX IDXZ)
           .
       2009-TBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    フィルム以外編集処理
      *****************************************************************
       2001-KIZAI-HEN-SEC            SECTION.
      *
           EVALUATE    ORCSRECESORT-IN-TOKUKIZAISBT1 (IDX2)
               WHEN    ZERO
      *            器材
                   MOVE    4                   TO  IDX
                   ADD     1                   TO  IDY4
                   MOVE    IDY4                TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
               WHEN    2
                   IF      ORCSRECESORT-IN-SANSOKBN (IDX2) =   "9"
      *            窒素
                       MOVE    3               TO  IDX
                       ADD     1               TO  IDY3
                       MOVE    IDY3            TO  IDXZ
                       PERFORM 2009-TBL-HEN-SEC
                   ELSE
      *            酸素
                       MOVE    2               TO  IDX
                       ADD     1               TO  IDY2
                       MOVE    IDY2            TO  IDXZ
                       PERFORM 2009-TBL-HEN-SEC
      *                高気圧加算以外
                       IF     (ORCSRECESORT-IN-SANSOKBN (IDX2) =   "1")
                         AND  (ORCSRECESORT-IN-SRYCD (IDX2)
                                               NOT =   "770000000"  )
                           CONTINUE
                       ELSE
      *                酸素補正率を酸素の直下に再編集
                           IF      IDX-HOSI    NOT =   ZERO
                               ADD     1               TO  IDY2
                               MOVE    IDX2            TO  IDX-HZN
                               MOVE    IDX-HOSI        TO  IDX2
                               MOVE    IDY2            TO  IDXZ
                               PERFORM 2009-TBL-HEN-SEC
                               MOVE    IDX-HZN         TO  IDX2
                           END-IF
                       END-IF
                   END-IF
               WHEN    3
      *            高線量率イリジウム（その他器材）
                   MOVE    4                   TO  IDX
                   ADD     1                   TO  IDY4
                   MOVE    IDY4            TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
           END-EVALUATE
      *
           .
       2001-KIZAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集処理
      *****************************************************************
       2002-COMMENT-HEN-SEC             SECTION.
      *
           EVALUATE    IDX
               WHEN    1
      *            フィルム
                   ADD     1                   TO  IDY1
                   MOVE    IDY1                TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
               WHEN    2
      *            酸素
                   ADD     1                   TO  IDY2
                   MOVE    IDY2                TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
               WHEN    3
      *            窒素
                   ADD     1                   TO  IDY3
                   MOVE    IDY3                TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
               WHEN    4
      *            その他器材
                   ADD     1                   TO  IDY4
                   MOVE    IDY4                TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
           END-EVALUATE
      *
           .
       2002-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ付加テーブル検索処理
      *****************************************************************
       910-SRYACCTPLUS-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
               MOVE    ZERO                TO  FLG-SRYACCTPLUS
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       910-SRYACCTPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
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
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
