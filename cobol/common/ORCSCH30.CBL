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
       PROGRAM-ID.             ORCSCH30.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 薬剤情報印刷用パラメタ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/11    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/09/04  種別区分の追加
      * 01.00.02     NACL-多々納  02/09/17  処方のみの時院内処方とする
      * 01.00.03     NACL-多々納  02/10/04  逓減を対象外とする
      * 01.00.04     NACL-多々納  02/10/25  入院対応
      * 01.00.05     NACL-多々納  04/03/04  用法複数対応
      * 01.00.06     NACL-多々納  04/10/14  複数科保険対応
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
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
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
           03  IDXY                PIC 9(04).
           03  IDXY2               PIC 9(04).
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
                   07  TBL-SURYO           PIC 9(05)V9(03).
               05  WRK-YSRYCD              PIC X(09).
               05  WRK-SRYKBN              PIC X(02).
               05  WRK-SRYSYUKBN           PIC X(03).
      *
               05  WRK-YOHOU-G.
                   07  WRK-YOHOU           PIC X(09)   OCCURS  5.
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
      *    保険組合せ
           03  WRK-HKNCOMBINUM         PIC X(04).
      *    プログラム名
           03  WRK-YAKJYO-PG       PIC X(12).
      *
           03  WRK-DENPNUM         PIC 9(07).
           03  WRK-ORC-DBPATH      PIC X(64).
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
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    薬情報出力パラメタ
           COPY    "CPORCHC30.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           COPY    "CPORCSCH30.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCH30AREA
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
           INITIALIZE                  ORCHC30AREA
      *
           EVALUATE    ORCSCH30-KBN
               WHEN    "1"
               WHEN    "2"
      *            更新時発行
                   PERFORM 100-SRYACT-SYORI-SEC
               WHEN    "3"
               WHEN    "4"
      *            中途終了時発行
                   PERFORM 200-WKSRYACT-SYORI-SEC
           END-EVALUATE
      *
      *TEST
      *    DISPLAY "ORCSCH30-KBN:" ORCSCH30-KBN
      *    DISPLAY "ORCHC30-MAX:" ORCHC30-MAX
      *    PERFORM VARYING    IDX   FROM    1  BY  1
      *            UNTIL      IDX   >  ORCHC30-MAX
      *      DISPLAY "ORCHC30-SRYCD:" ORCHC30-SRYCD (IDX)
      *              ",ORCHC30-YSRYCD:" ORCHC30-YSRYCD (IDX)
      *    END-PERFORM 
      *TEST
      *
      *    印刷処理へ
           IF      ORCHC30-MAX         >   ZERO
               PERFORM 300-HC30-PRINT-SEC
           END-IF
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       300-HC30-PRINT-SEC             SECTION.
      *
      *    薬情報出力
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000004"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               GO      TO      300-HC30-PRINT-EXT
           END-IF
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-YAKJYO-PG
      *
      *    帳票プログラム取得
      ***  MOVE    SPACE               TO  WRK-YAKJYO-PG
      *    MOVE    SPACE               TO  SYS-1032-REC
      *    INITIALIZE                      SYS-1032-REC
      *    MOVE    "1032"              TO  SYS-1032-KANRICD
      *    MOVE    "00000004"          TO  SYS-1032-KBNCD
      *
      *    MOVE    SYS-1032-REC        TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *    MOVE    SPA-SRYYMD          TO  ORC-DBYMD
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *        PERFORM 960-KANRIMST-READ-SEC
      *    ELSE
      *        INITIALIZE                  SYS-1032-REC
      *        MOVE    1               TO  FLG-SYSKANRI
      *    END-IF
      *    IF      FLG-SYSKANRI        =   ZERO
      *        MOVE    MCPDATA-REC         TO  SYS-1032-REC
      *        独自開発分
      *        IF      SYS-1032-ORCACSTCHK     =   "T"
      *            MOVE    SYS-1032-ORCACSTNM  TO  WRK-YAKJYO-PG
      *        ELSE
      *        ＯＲＣＡ標準分
      *            MOVE    SYS-1032-ORCAORGNM  TO  WRK-YAKJYO-PG
      *        END-IF
      *    END-IF
      *    IF      WRK-YAKJYO-PG       =   SPACE
      *        MOVE    "ORCHC30"           TO  WRK-YAKJYO-PG
      *****END-IF
      *
           CALL    WRK-YAKJYO-PG           USING
                                           SPA-AREA
                                           ORCHC30AREA
      *
           .
       300-HC30-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新時発行処理
      *****************************************************************
       100-SRYACT-SYORI-SEC             SECTION.
      *
           IF      ORCSCH30-DENPNUM    =   ZERO
               MOVE    ZERO                TO  WRK-RENNUM
               PERFORM 1001-RENNUM-SET-SEC
           ELSE
               MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
               MOVE    ORCSCH30-DENPNUM    TO  WRK-DENPNUM
      *        編集
               PERFORM 1002-SYOHOU-SYORI-SEC
           END-IF
      *
           .
       100-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の連番確定処理
      *****************************************************************
       1001-RENNUM-SET-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-PRINT
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY7"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY7"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
               MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
               MOVE    ZERO                TO  FLG-SYORI
               IF      SPA-HKNCOMBINUM     =   SPACE
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
                   MOVE    1                   TO  FLG-SYORI
               ELSE
                   IF      SPA-HKNCOMBINUM     =   JYURRK-HKNCOMBINUM
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               END-IF
      *        編集
               IF      FLG-SYORI           =   1
                   MOVE    JYURRK-DENPNUM      TO  WRK-DENPNUM
                   PERFORM 1002-SYOHOU-SYORI-SEC
               END-IF
               IF      FLG-PRINT           =   1
                   MOVE    1               TO  FLG-JYURRK
               ELSE
                   MOVE    ZERO            TO  FLG-JYURRK
                   MOVE    "JYURRK-KEY7"   TO  ORC-DBPATH
                   PERFORM 970-JYURRK-NEXT-SEC
               END-IF
           END-PERFORM
      *
      *
           .
       1001-RENNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       1002-SYOHOU-SYORI-SEC          SECTION.
      *
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-DENPNUM         TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY6"       TO  WRK-ORC-DBPATH
           MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
           IF      JYURRK-GRP-DENPNUM  =   ZERO
      *         単独分
                PERFORM 10021-ONLY-SYORI-SEC
           ELSE
      *         複数分
                PERFORM 10022-FUKU-SYORI-SEC
           END-IF
      *
           .
       1002-SYOHOU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    単独分編集処理
      *****************************************************************
       10021-ONLY-SYORI-SEC          SECTION.
      *
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *
               PERFORM 100211-JYURRK-CHK-SEC
      *
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-ORC-DBPATH       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       10021-ONLY-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    複数分編集処理
      *****************************************************************
       10022-FUKU-SYORI-SEC          SECTION.
      *
      *    受診履歴検索
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY26"      TO  WRK-ORC-DBPATH
           MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
      *
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *
               PERFORM 100211-JYURRK-CHK-SEC
      *
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-ORC-DBPATH       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       10022-FUKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       100211-JYURRK-CHK-SEC          SECTION.
      *
           PERFORM VARYING     IDX-JYU     FROM    1   BY  1
                   UNTIL      (IDX-JYU         >   15  )   OR
                              (JYURRK-ZAINUM (IDX-JYU)
                                               =   ZERO )
               MOVE    JYURRK-ZAINUM (IDX-JYU) TO  WRK-ZAINUM
               COMPUTE IDX-REN         =   JYURRK-RENNUM
                                       +   1
               IF      IDX-REN         >   4
                   MOVE    4               TO  IDX-REN
               END-IF
               PERFORM 2002-SRYACT-SYORI-SEC
           END-PERFORM
      *
           .
       100211-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *        診療行為より編集処理
      *****************************************************************
       2002-SRYACT-SYORI-SEC      SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPID          TO  SRY-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
           MOVE    WRK-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACT-KEY2"         TO  ORC-DBPATH
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    SPACE               TO  WRK-TBL-AREA
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  IDXY
           MOVE    SPACE               TO  WRK-YSRYCD
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPACE               TO  WRK-YOHOU-G
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *        投薬を対象とする
               IF      SRY-SRYKBN          =   "21"   OR  "22" OR "23"
                   MOVE    SRY-SRYKBN          TO  WRK-SRYKBN
                   MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
      *            診療会計より、点数チェック
                   PERFORM 20021-SRYACCT-SYORI-SEC
                   MOVE    ZERO                TO  FLG-OK
      *            発行あり（院内）点数＞ゼロ
                   IF     (ACCT-ZAITEN     NOT =   ZERO  )  AND
                          (ORCSCH30-KBN        =   "1"   )
                       MOVE    1               TO  FLG-OK
                   END-IF
      *            院外分発行あり（院外）点数＝ゼロ
                   IF     (ACCT-ZAITEN         =   ZERO  )  AND
                          (ORCSCH30-KBN        =   "2"   )
                       MOVE    1               TO  FLG-OK
                   END-IF
      *            処方のみのときは院内分とする
                   IF      WRK-SRYSYUKBN       =   "213"  OR
                                                   "223"  OR
                                                   "233"
                       IF      ORCSCH30-KBN        =   "1"
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF      ORCSCH30-KBN        =   "2"
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   END-IF
      *
                   IF      FLG-OK              =   1
                       PERFORM VARYING     IDXA    FROM    1   BY  1
                               UNTIL      (IDXA    >   5   )  OR
                                          (SRY-SRYCD (IDXA)   =   SPACE)
                           IF      SRY-SRYCD (IDXA)     =   "630010002"
                               CONTINUE
                           ELSE
                               PERFORM 20022-TBLSET-SEC
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           IF      IDX-S               >   ZERO
      *        パラメタ編集
               PERFORM 2003-HC30TBL-SET-SEC
           END-IF
      *
           .
       2002-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計よりテーブル編集処理
      *****************************************************************
       20022-TBLSET-SEC         SECTION.
      *    薬剤編集
           IF      SRY-SRYCD (IDXA)(1:1)   =   "6"
               ADD     1               TO  IDX-S
               IF      IDX-S           >   99
                   DISPLAY "ORCSHC30 TBL OVER !!"
                   MOVE    99              TO  IDX-S
               END-IF
               MOVE    SRY-SRYCD (IDXA)    TO  TBL-SRYCD (IDX-S)
               MOVE    SRY-SRYSURYO (IDXA) TO  TBL-SURYO (IDX-S)
           END-IF
      *    用法編集
           IF      SRY-SRYCD (IDXA)(1:3)   =   "001"
               MOVE    ZERO                TO  FLG-YSET
      *        点数マスタを検索して、用法時点を判定する
               MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   IF     (TNS-SSTKIJUNCD(1)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(2)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(3)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(4)   =   ZERO )
                       CONTINUE
                   ELSE
                       IF      WRK-YSRYCD      =   SPACE
                           MOVE    SRY-SRYCD (IDXA)    TO  WRK-YSRYCD
                           MOVE    1                   TO  FLG-YSET
                       END-IF
                   END-IF
                   IF      FLG-YSET            =   ZERO
                       ADD     1                   TO  IDXY
                       IF      IDXY                <=  5
                           MOVE    SRY-SRYCD (IDXA)    TO  WRK-YOHOU
                                                                (IDXY)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       20022-TBLSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬情報パラメタ編集処理
      *****************************************************************
       2003-HC30TBL-SET-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-S   )   OR
                              (ORCHC30-MAX     >=  99 )
               ADD     1                   TO  ORCHC30-MAX
               MOVE    ORCHC30-MAX         TO  IDY
               MOVE    TBL-SRYCD (IDX)     TO  ORCHC30-SRYCD (IDY)
               MOVE    TBL-SURYO (IDX)     TO  ORCHC30-SURYO (IDY)
      *
               MOVE    WRK-YSRYCD          TO  ORCHC30-YSRYCD(IDY)
               MOVE    WRK-KAISU           TO  ORCHC30-KAISU (IDY)
      *
               MOVE    ZERO                TO  IDXY2
               PERFORM VARYING     IDXY    FROM    1   BY  1
                       UNTIL      (IDXY        >   5   )   OR
                                  (WRK-YOHOU(IDXY) =   SPACE)
                   IF      ORCHC30-YSRYCD(IDY)     =   SPACE
                       MOVE    WRK-YOHOU(IDXY)     TO  ORCHC30-YSRYCD
                                                                (IDY)
                   ELSE
                       ADD     1                   TO  IDXY2
                       MOVE    WRK-YOHOU(IDXY)     TO  ORCHC30-YOHOU
                                                            (IDY IDXY2)
                   END-IF
               END-PERFORM
      *
               EVALUATE    WRK-SRYKBN
      *            内服
                   WHEN    "21"
                       MOVE    "1"         TO  ORCHC30-ZAIKBN(IDY)
      *            頓服
                   WHEN    "22"
                       MOVE    "2"         TO  ORCHC30-ZAIKBN(IDY)
      *            外用
                   WHEN    "23"
                       MOVE    "3"         TO  ORCHC30-ZAIKBN(IDY)
               END-EVALUATE
           END-PERFORM
      *
           .
       2003-HC30TBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計より回数編集処理
      *****************************************************************
       20021-SRYACCT-SYORI-SEC         SECTION.
      *
      *    診療会計マスター読み込み
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-ACT
           MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                       TO  WRK-KAISU
      *
           .
       20021-SRYACCT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    中途終了時の発行処理
      *****************************************************************
       200-WKSRYACT-SYORI-SEC         SECTION.
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPID          TO  WKSRY-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "WKSRYACT-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "WKSRYACT-KEY2"   TO  ORC-DBPATH
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
      *
           MOVE    SPACE               TO  WRK-TBL-AREA
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  IDXY
           MOVE    SPACE               TO  WRK-YSRYCD
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    ZERO                TO  WRK-ZAITENKEI
           MOVE    SPACE               TO  WRK-YOHOU-G
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
               IF      WKSRY-RENNUM        =   1
      *            剤終了時、編集へ
                   IF      IDX-S           >   ZERO
                       MOVE    ZERO            TO  FLG-OK
      *                投薬で、点数がゼロのもを対象（院内）
                       IF     (ORCSCH30-KBN        =   "3" )  AND
                              (WRK-ZAITENKEI       >   ZERO)
                           MOVE    1               TO  FLG-OK
                       END-IF
      *                投薬で、点数がゼロのもを対象（院外）
                       IF     (ORCSCH30-KBN        =   "4" )  AND
                              (WRK-ZAITENKEI       =   ZERO)
                           MOVE    1               TO  FLG-OK
                       END-IF
      *                処方のみのときは院内分とする
                       IF      WRK-SRYSYUKBN       =   "213"  OR
                                                       "223"  OR
                                                       "233"
                           IF      ORCSCH30-KBN        =   "3"
                               MOVE    1               TO  FLG-OK
                           END-IF
                           IF      ORCSCH30-KBN        =   "4"
                               MOVE    ZERO            TO  FLG-OK
                           END-IF
                       END-IF
      *
                       IF      FLG-OK              =   1
      *                    パラメタ編集
                           PERFORM 2003-HC30TBL-SET-SEC
                       END-IF
                   END-IF
      *            回数
                   MOVE    WKSRY-ZAIKAIKEI     TO  WRK-KAISU
                   MOVE    ZERO                TO  FLG-OK
                   MOVE    SPACE               TO  WRK-TBL-AREA
                   MOVE    ZERO                TO  IDX-S
                   MOVE    ZERO                TO  IDXY
                   MOVE    SPACE               TO  WRK-YSRYCD
                   MOVE    SPACE               TO  WRK-YOHOU-G
                   MOVE    WKSRY-SRYKBN        TO  WRK-SRYKBN
                   MOVE    WKSRY-SRYSYUKBN     TO  WRK-SRYSYUKBN
                   MOVE    ZERO                TO  WRK-ZAITENKEI
               END-IF
      *
      *        投薬を対象とする
               IF      WKSRY-SRYKBN        =   "21"  OR
                                               "22"  OR
                                               "23"
                   MOVE    WKSRY-ZAITENKEI         TO  WRK-ZAITENKEI
                   PERFORM 20031-WKSRYACT-HEN-SEC
               END-IF
      *
               MOVE    "WKSRYACT-KEY2"     TO  ORC-DBPATH
               PERFORM 940-WKSRYACT-READ-SEC
           END-PERFORM
      *
      *    剤終了時、編集へ
           IF      IDX-S           >   ZERO
               MOVE    ZERO            TO  FLG-OK
      *        投薬で、点数がゼロのもを対象（院内）
               IF      (ORCSCH30-KBN        =   "3" )  AND
                       (WRK-ZAITENKEI       >   ZERO)
                   MOVE    1               TO  FLG-OK
               END-IF
      *        投薬で、点数がゼロのもを対象（院外）
               IF     (ORCSCH30-KBN        =   "4" )  AND
                      (WRK-ZAITENKEI       =   ZERO)
                   MOVE    1               TO  FLG-OK
               END-IF
      *        処方のみのときは院内分とする
               IF      WRK-SRYSYUKBN       =   "213"  OR
                                               "223"  OR
                                               "233"
                   IF      ORCSCH30-KBN        =   "3"
                       MOVE    1               TO  FLG-OK
                   END-IF
                   IF      ORCSCH30-KBN        =   "4"
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               END-IF
               IF      FLG-OK              =   1
      *            パラメタ編集
                   PERFORM 2003-HC30TBL-SET-SEC
               END-IF
           END-IF
      *
           .
       2003-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為内容を診療行為へ
      *****************************************************************
       20031-WKSRYACT-HEN-SEC         SECTION.
      *
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   5   )  OR
                              (WKSRY-SRYCD (IDXA) =   SPACE)
            IF      WKSRY-SRYCD (IDXA)     =   "630010002"
               CONTINUE
            ELSE
      *
      *        薬剤編集
               IF      WKSRY-SRYCD (IDXA)(1:1)   =   "6"
                   ADD     1               TO  IDX-S
                   IF      IDX-S           >   99
                       DISPLAY "ORCSHC30 TBL OVER !!"
                       MOVE    99                  TO  IDX-S
                   END-IF
                   MOVE    WKSRY-SRYCD (IDXA)      TO  TBL-SRYCD (IDX-S)
                   MOVE    WKSRY-SRYSURYO (IDXA)   TO  TBL-SURYO (IDX-S)
               END-IF
      *        用法編集
               IF      WKSRY-SRYCD (IDXA)(1:3) =   "001"
                   MOVE    ZERO                TO  FLG-YSET
      *            点数マスタを検索して、用法時点を判定する
                   MOVE    WKSRY-SRYCD (IDXA)  TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU           =   ZERO
                       IF     (TNS-SSTKIJUNCD(1)   =   ZERO )  AND
                              (TNS-SSTKIJUNCD(2)   =   ZERO )  AND
                              (TNS-SSTKIJUNCD(3)   =   ZERO )  AND
                              (TNS-SSTKIJUNCD(4)   =   ZERO )
                           CONTINUE
                       ELSE
                           IF      WRK-YSRYCD      =   SPACE
                               MOVE    WKSRY-SRYCD (IDXA)
                                                       TO  WRK-YSRYCD
                               MOVE    1               TO  FLG-YSET
                            END-IF
                       END-IF
                       IF      FLG-YSET            =   ZERO
                           ADD     1                   TO  IDXY
                           IF      IDXY                <=  5
                               MOVE    WKSRY-SRYCD (IDXA)  TO  WRK-YOHOU
                                                                (IDXY)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
             END-IF
      *
           END-PERFORM
      *
           .
       20031-WKSRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
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
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-NEXT-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
