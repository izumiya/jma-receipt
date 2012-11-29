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
       PROGRAM-ID.             ORCSCHN30.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 入院薬剤情報印刷用パラメタ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/09/11    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.03.00    NACL-太田   08/10/31   オン帳票バッチ起動対応
      *  04.05.00    NACL-多々納 09/07/23   在宅・コメント対象対応
      *  04.05.00    NACL-多々納 09/09/XX   表示コメントコード対応
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-KAISU           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ZAITAKU         PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-YSET            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDXC                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDXY2               PIC 9(04).
           03  IDT                 PIC 9(04).
      *
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-RENNUM          PIC 9(01).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-TBL-AREA.
               05  WRK-TBL-OCC             OCCURS   99.
                   07  TBL-SRYCD           PIC X(09).
                   07  TBL-SURYO           PIC 9(05)V9(05).
                   07  TBL-KANSURYO        PIC 9(05)V9(05).
                   07  TBL-YOURYOU         PIC 9(05)V9(05)
                                           OCCURS   5.
                   07  TBL-SRYKAISU        PIC 9(03).
               05  WRK-YSRYCD              PIC X(09).
               05  WRK-SRYKBN              PIC X(02).
               05  WRK-SRYSYUKBN           PIC X(03).
      *
               05  WRK-YOHOU-G.
                   07  WRK-YOHOU           PIC X(09)   OCCURS  5.
      *        コメント
               05  TBL-COMMENT-G           OCCURS   20.
                   07  TBL-COMMENT             PIC X(09).
                   07  TBL-INPUTCOMENT         PIC X(80).
      *
           03  WRK-ZAITENKEI            PIC 9(07).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-KAISU           PIC 9(05).
      *    プログラム名
           03  WRK-YAKJYO-PG       PIC X(20).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    入院薬情報出力パラメタ
           COPY    "CPORCHCN30.INC".
      *
      *    オンライン帳票共通パラメタ
           COPY    "CPONPRTDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    パラメタ
           COPY    "CPORCSCHN30.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCHN30AREA
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
           INITIALIZE                  ORCHCN30AREA
           INITIALIZE                  ONPRTDATA
      *
           EVALUATE    ORCSHCN30-KBN
               WHEN    "N"
      *            入院印刷画面発行
                   PERFORM 100-ORCSHCN30-SYORI-SEC
           END-EVALUATE
      *
      *TEST
      *    DISPLAY "ORCSHCN30-KBN:" ORCSHCN30-KBN
      *    DISPLAY "ORCHCN30-MAX:" ORCHCN30-MAX
      *    PERFORM VARYING    IDX   FROM    1  BY  1
      *            UNTIL      IDX   >  ORCHCN30-MAX
      *      DISPLAY "ORCHCN30-SRYCD:" ORCHCN30-SRYCD (IDX)
      *              ",ORCHCN30-ZAIKBN:" ORCHCN30-ZAIKBN (IDX)
      *              ",ORCHCN30-SURYO:" ORCHCN30-SURYO (IDX)
      *              ",ORCHCN30-YSRYCD:" ORCHCN30-YSRYCD (IDX)
      *              ",ORCHCN30-YOURYOU 1:" ORCHCN30-YOURYOU (IDX 1)
      *              ",2:" ORCHCN30-YOURYOU (IDX 2)
      *              ",3:" ORCHCN30-YOURYOU (IDX 3)
      *              ",4:" ORCHCN30-YOURYOU (IDX 4)
      *              ",ORCHCN30-SRYKAISU:" ORCHCN30-SRYKAISU (IDX)
      *      DISPLAY "ORCHCN30-COMMENT-CD:" ORCHCN30-COMMENT-CD (IDX 1)
      *       "," ORCHCN30-COMMENT-MEI (IDX 1)
      *      DISPLAY "ORCHCN30-COMMENT-CD:" ORCHCN30-COMMENT-CD (IDX 2)
      *       "," ORCHCN30-COMMENT-MEI (IDX 2)
      *      DISPLAY "ORCHCN30-COMMENT-CD:" ORCHCN30-COMMENT-CD (IDX 3)
      *       "," ORCHCN30-COMMENT-MEI (IDX 3)
      *      DISPLAY "ORCHCN30-COMMENT-CD:" ORCHCN30-COMMENT-CD (IDX 4)
      *       "," ORCHCN30-COMMENT-MEI (IDX 4)
      *    END-PERFORM 
      *TEST
      *
      *    印刷処理へ
           IF      ORCHCN30-MAX         >   ZERO
               PERFORM 300-HCN30-PRINT-SEC
           END-IF
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       300-HCN30-PRINT-SEC            SECTION.
      *
      *    薬情報出力
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000009"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-YAKJYO-PG
           END-IF
      *
      *    交付日
           MOVE    ORCSHCN30-SRYYMD    TO  SPA-SRYYMD
      *    ドクター
           MOVE    ORCSHCN30-DRCD      TO  ORCHCN30-DRCD
      *    病棟
           MOVE    ORCSHCN30-BTUNAME   TO  ORCHCN30-BTUNAME
      *    病室
           MOVE    ORCSHCN30-ROOMNO    TO  ORCHCN30-ROOMNO
      *
           IF      WRK-YAKJYO-PG   NOT =   SPACE
               MOVE    WRK-YAKJYO-PG   TO  ONPRTDATA-PGID
               MOVE    ORCHCN30AREA    TO  ONPRTDATA-DATA
               CALL    "ORCHCOMMON"        USING
                                           SPA-AREA
                                           ONPRTDATA
           END-IF
      *
           .
       300-HCN30-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容変更発行処理
      *****************************************************************
       100-ORCSHCN30-SYORI-SEC         SECTION.
      *
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   ORCSHCN30-ZAI-MAX)
      *
               MOVE    SPACE               TO  WRK-TBL-AREA
               INITIALIZE                      WRK-TBL-AREA
               MOVE    ZERO                TO  IDX-S
               MOVE    ZERO                TO  IDXY
               MOVE    ZERO                TO  IDXC
               MOVE    SPACE               TO  WRK-YSRYCD
               MOVE    ORCSHCN30-ZAIKAIKEI(IDT)
                                           TO  WRK-KAISU
               MOVE    ORCSHCN30-SRYKBN (IDT)
                                           TO  WRK-SRYKBN
               MOVE    ORCSHCN30-SRYSYUKBN (IDT)
                                           TO  WRK-SRYSYUKBN
               MOVE    SPACE               TO  WRK-YOHOU-G
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA    >   50   )  OR
                                  (ORCSHCN30-SRYCD(IDT IDXA)  =   SPACE)
                   PERFORM 3001-TBLSET-SEC
               END-PERFORM
      *
               IF      IDX-S               >   ZERO
      *            パラメタ編集
                   PERFORM 1001-HCN30TBL-SET-SEC
               END-IF
           END-PERFORM
      *
           .
       100-ORCSHCN30-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療会計よりテーブル編集処理
      *****************************************************************
       3001-TBLSET-SEC         SECTION.
      *    薬剤編集
           IF      ORCSHCN30-SRYCD(IDT IDXA)(1:1)   =   "6"
               ADD     1               TO  IDX-S
               IF      IDX-S           >   99
                   DISPLAY "ORCSHC30 TBL OVER !!"
                   MOVE    99              TO  IDX-S
               END-IF
               MOVE    ORCSHCN30-SRYCD(IDT IDXA)
                                           TO  TBL-SRYCD (IDX-S)
               MOVE    ORCSHCN30-SRYSURYO(IDT IDXA)
                                           TO  TBL-SURYO (IDX-S)
               MOVE    ORCSHCN30-KANSURYO(IDT IDXA)
                                           TO  TBL-KANSURYO (IDX-S)
      *        外用の日数
               IF      (ORCSHCN30-SRYKBN (IDT)          =   "23" )  AND
                       (ORCSHCN30-SRYKAISU(IDT IDXA)    >   1 )
                   MOVE    ORCSHCN30-SRYKAISU(IDT IDXA)
                                                   TO  TBL-SRYKAISU
                                                                 (IDX-S)
               END-IF
           END-IF
      *    用法編集
           IF      ORCSHCN30-SRYCD(IDT IDXA)(1:3)   =   "001"
               MOVE    ZERO                TO  FLG-YSET
      *        点数マスタを検索して、用法時点を判定する
               MOVE    ORCSHCN30-SRYCD(IDT IDXA)    TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   IF     (TNS-SSTKIJUNCD(1)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(2)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(3)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(4)   =   ZERO )
                       CONTINUE
                   ELSE
                       IF      WRK-YSRYCD      =   SPACE
                           MOVE    ORCSHCN30-SRYCD(IDT IDXA) 
                                                       TO  WRK-YSRYCD
                           MOVE    1                   TO  FLG-YSET
                       END-IF
                   END-IF
                   IF      FLG-YSET            =   ZERO
                       ADD     1                   TO  IDXY
                       IF      IDXY                <=  5
                           MOVE    ORCSHCN30-SRYCD(IDT IDXA) 
                                                   TO  WRK-YOHOU
                                                                (IDXY)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *    用量割合コード
           IF     (ORCSHCN30-SRYCD(IDT IDXA)(1:7)
                                           =   "0992000" ) AND
                  (IDX-S                   >   ZERO      )
               EVALUATE    ORCSHCN30-SRYCD(IDT IDXA)
      *            朝
                   WHEN    "099200011"
                       MOVE    ORCSHCN30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 1)
      *            昼
                   WHEN    "099200012"
                       MOVE    ORCSHCN30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 2)
      *            夜
                   WHEN    "099200013"
                       MOVE    ORCSHCN30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 3)
      *            寝
                   WHEN    "099200014"
                       MOVE    ORCSHCN30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 4)
      *            起床
                   WHEN    "099200015"
                       MOVE    ORCSHCN30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 5)
               END-EVALUATE
           END-IF
      *
      *    コメント
           IF     (ORCSHCN30-SRYCD(IDT IDXA)(1:1)  =   "8"   )   OR
                  (ORCSHCN30-SRYCD(IDT IDXA)(1:4)
                                               =   "0082"
                                               OR  "0083"
                                               OR  "0084"
                                               OR  "0085" )
               IF     (IDXC                    <   20     )
                   ADD     1                       TO  IDXC
                   MOVE    ORCSHCN30-SRYCD (IDT IDXA)
                                               TO  TBL-COMMENT (IDXC)
                   MOVE    ORCSHCN30-INPUTCOMENT (IDT IDXA)
                                               TO  TBL-INPUTCOMENT
                                                             (IDXC)
               END-IF
           END-IF
      *
           .
       3001-TBLSET-EXT.
           EXIT.
      *****************************************************************
      *    薬情報パラメタ編集処理
      *****************************************************************
       1001-HCN30TBL-SET-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-S   )   OR
                              (ORCHCN30-MAX     >=  99 )
               ADD     1                   TO  ORCHCN30-MAX
               MOVE    ORCHCN30-MAX        TO  IDY
               MOVE    TBL-SRYCD (IDX)     TO  ORCHCN30-SRYCD (IDY)
               MOVE    TBL-SURYO (IDX)     TO  ORCHCN30-SURYO (IDY)
               MOVE    TBL-KANSURYO (IDX)  TO  ORCHCN30-KANSURYO (IDY)
               PERFORM VARYING     IDXY    FROM    1   BY  1
                       UNTIL       IDXY        >   5
                   MOVE    TBL-YOURYOU (IDX IDXY)
                                           TO  ORCHCN30-YOURYOU
                                                            (IDY IDXY)
               END-PERFORM
               MOVE    TBL-SRYKAISU (IDX)  TO  ORCHCN30-SRYKAISU (IDY)
      *
               MOVE    WRK-YSRYCD          TO  ORCHCN30-YSRYCD(IDY)
               MOVE    WRK-KAISU           TO  ORCHCN30-KAISU (IDY)
      *
               MOVE    ZERO                TO  IDXY2
               PERFORM VARYING     IDXY    FROM    1   BY  1
                       UNTIL      (IDXY        >   5   )   OR
                                  (WRK-YOHOU(IDXY) =   SPACE)
                   IF      ORCHCN30-YSRYCD(IDY)     =   SPACE
                       MOVE    WRK-YOHOU(IDXY)     TO  ORCHCN30-YSRYCD
                                                                (IDY)
                   ELSE
                       ADD     1                   TO  IDXY2
                       MOVE    WRK-YOHOU(IDXY)     TO  ORCHCN30-YOHOU
                                                            (IDY IDXY2)
                   END-IF
               END-PERFORM
      *        コメント
               PERFORM VARYING     IDXC    FROM    1   BY  1
                       UNTIL      (IDXC        >   10   )   OR
                                  (TBL-COMMENT (IDXC)  =   SPACE)
                   MOVE    TBL-COMMENT (IDXC)  TO  ORCHCN30-COMMENT-CD
                                                            (IDY IDXC)
                   MOVE    TBL-INPUTCOMENT (IDXC)
                                               TO  ORCHCN30-COMMENT-MEI
                                                            (IDY IDXC)
               END-PERFORM
      *
               EVALUATE    WRK-SRYKBN
      *            内服
                   WHEN    "21"
                       MOVE    "1"         TO  ORCHCN30-ZAIKBN(IDY)
      *            頓服
                   WHEN    "22"
                       MOVE    "2"         TO  ORCHCN30-ZAIKBN(IDY)
      *            外用
                   WHEN    "23"
                       MOVE    "3"         TO  ORCHCN30-ZAIKBN(IDY)
      *            在宅
                   WHEN    "14"
                       MOVE    "4"         TO  ORCHCN30-ZAIKBN(IDY)
               END-EVALUATE
           END-PERFORM
      *
           .
       1001-HCN30TBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ORCSHCN30-SRYYMD     TO  TNS-YUKOSTYMD
           MOVE    ORCSHCN30-SRYYMD     TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
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
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
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
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
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
