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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSSYUNOUCHK.
      ****************************************************************
      *    OS           LINUX
      *
      *    PROGRAM      ORCSSYUNOUCHK  /  一括再計算該当者チェックサブ
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/11/01    NACL−門脇    新規作成
      ****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ****************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE SECTION.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-ARI                     PIC 9(01).
           03  FLG-ARI2                    PIC 9(01).
           03  FLG-GENBUTU                 PIC 9(01).
           03  FLG-GENBUTU2                PIC 9(01).
           03  FLG-GENBUTU3                PIC 9(01).
           03  FLG-GENBUTU4                PIC 9(01).
           03  FLG-SYORI-KBN               PIC 9(01).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-PTID-TABLE.
             04  WRK-PTID-OCC              OCCURS   10000.
               05  WRK-PTID                PIC 9(10).
               05  WRK-GAITOKBN            PIC 9(02).
               05  WRK-SKYMONEY-FLG        PIC 9(01).
               05  WRK-TOTALTENKBN-A       PIC 9(01).
               05  WRK-TOTALTENKBN-B       PIC 9(01).
               05  WRK-CHKNGDATA           PIC 9(01).
      *
           03  WRK-PTID-TABLE2.
             04  WRK-PTID-OCC2             OCCURS   10000.
               05  WRK-PTID2               PIC 9(10).
               05  WRK-GAITOKBN2           PIC 9(02).
               05  WRK-SKYMONEY2-FLG       PIC 9(01).
               05  WRK-TOTALTENKBN2-A      PIC 9(01).
               05  WRK-TOTALTENKBN2-B      PIC 9(01).
               05  WRK-CHKNGDATA2          PIC 9(01).
      *
           03  WRK-PTID-TABLE3.
             04  WRK-PTID-OCC3             OCCURS   10000.
               05  WRK-PTID3               PIC 9(10).
               05  WRK-GAITOKBN3           PIC 9(02).
               05  WRK-SKYMONEY3-FLG       PIC 9(01).
               05  WRK-TOTALTENKBN3-A      PIC 9(01).
               05  WRK-TOTALTENKBN3-B      PIC 9(01).
               05  WRK-CHKNGDATA3          PIC 9(01).
      *
           03  WRK-DENPNUM-END-TABLE.
             04  WRK-DENPNUM-END-OCC       OCCURS   100.
               05  WRK-DENPNUM-END         PIC 9(07).
               05  WRK-SYUNOU-DATA         PIC X(2500).
      *
           03  WRK-IKT-TABLE               PIC X(64).
           03  WRK-IKT-PATHNAME            PIC X(64).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納データ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    システム管理
           COPY    "CPSK1039.INC".
      *
      *    ワーク収納データ
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING  //SYU-// BY //WRK-SYU-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    負担金再計算用サブ
           COPY    "CPORCSSAIKEISAN.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      /
      ***
       LINKAGE             SECTION.
           COPY  "CPORCSSYUNOUCHK.INC".
           COPY  "COMMON-SPA".
      /
       PROCEDURE           DIVISION  USING
                                     LNK-ORCSSYUNOUCHK-REC
                                     SPA-AREA
                                     .
      ******************************************************************
      *        依頼制御処理
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
      *
           INITIALIZE         LNK-ORCSSYUNOUCHK-OUT-AREA
           MOVE   ZERO   TO   LNK-ORCSSYUNOUCHK-RC
      *
           INITIALIZE         IDX-AREA
                              FLG-AREA
                              WRK-AREA
      *
           INITIALIZE         WRK-PTID-TABLE
           INITIALIZE         WRK-PTID-TABLE2
           INITIALIZE         WRK-PTID-TABLE3
      *
      *    該当診療年月の収納がある方を抽出
           INITIALIZE                              SYUNOU-REC
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM    TO  SYU-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM      TO  SYU-SRYYMD(1:6)
           MOVE   "__"                         TO  SYU-SRYYMD(7:2)
           MOVE   SYUNOU-REC                   TO  MCPDATA-REC
           MOVE   "DBSELECT"                   TO  MCP-FUNC
           MOVE   "tbl_syunou"                 TO  MCP-TABLE
           MOVE   "key96"                      TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      "tbl_syunou"            TO  MCP-TABLE
             MOVE      "key96"                 TO  MCP-PATHNAME
             PERFORM    900-SYUNOU-READ-SEC
             PERFORM    UNTIL  FLG-SYUNOU  =  1
               IF  SYU-DENPJTIKBN  NOT = "3"
                 MOVE     ZERO     TO  FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL  (IDX1            >  10000)
                            OR    (WRK-PTID(IDX1)  =  ZERO )
                            OR    (FLG-ARI         =   1   )
                   IF  SYU-PTID  =  WRK-PTID(IDX1)
                     MOVE    1     TO  FLG-ARI
                   END-IF
                 END-PERFORM
                 IF    FLG-ARI   =  ZERO
                 AND   IDX1     <=  10000
                   MOVE  SYU-PTID  TO  WRK-PTID(IDX1)
                 END-IF
               END-IF
               MOVE    "tbl_syunou"            TO  MCP-TABLE
               MOVE    "key96"                 TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           ELSE
             MOVE        1         TO  LNK-ORCSSYUNOUCHK-RC
           END-IF
           MOVE        "tbl_syunou"            TO  MCP-TABLE
           MOVE        "key96"                 TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
      *    システム管理１０３９を検索
           INITIALIZE                            SYS-1039-REC
           MOVE   "1039"                     TO  SYS-1039-KANRICD
           MOVE   "*"                        TO  SYS-1039-KBNCD
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM  TO  SYS-1039-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM    TO  SYS-1039-STYUKYMD(1:6)
           MOVE   "01"                       TO  SYS-1039-STYUKYMD(7:2)
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM    TO  SYS-1039-EDYUKYMD(1:6)
           MOVE   "01"                       TO  SYS-1039-EDYUKYMD(7:2)
           MOVE   SYS-1039-REC               TO  MCPDATA-REC
           MOVE   "DBSELECT"                 TO  MCP-FUNC
           MOVE   "tbl_syskanri"             TO  MCP-TABLE
           MOVE   "key10"                    TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"             USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
               MOVE    "tbl_syskanri"        TO  MCP-TABLE
               MOVE    "key10"               TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                        SYS-1039-REC
           END-IF
           MOVE        "tbl_syskanri"        TO  MCP-TABLE
           MOVE        "key10"               TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
      *    一括再計算該当者チェック
           MOVE     1             TO   FLG-SYORI-KBN
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                     UNTIL  (IDX1            >  10000)
                      OR    (WRK-PTID(IDX1)  =  ZERO )
             PERFORM  100-IKATUSAIKEI-SEC
           END-PERFORM
      *
      *    返却値セット
           MOVE       ZERO      TO   IDX2
           MOVE       ZERO      TO   IDX3
           MOVE       ZERO      TO   IDX4
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                     UNTIL  (IDX1            >  10000)
                      OR    (WRK-PTID(IDX1)  =  ZERO )
             IF     WRK-CHKNGDATA(IDX1)  =  ZERO
               PERFORM  500-HENKYAKU-SET-SEC
             ELSE
               DISPLAY "CHKNGDATA:PTID=" WRK-PTID(IDX1)
             END-IF
           END-PERFORM
      *
           IF  LNK-ORCSSYUNOUCHK-OPTION1  =  "1"
      *      患者請求額の変更がない方：自動で一括再計算
             INITIALIZE                  WRK-PTID-TABLE
             MOVE  WRK-PTID-TABLE2  TO   WRK-PTID-TABLE
             MOVE     ZERO          TO   FLG-SYORI-KBN
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                       UNTIL  (IDX1            >  10000)
                        OR    (WRK-PTID(IDX1)  =  ZERO )
               PERFORM  100-IKATUSAIKEI-SEC
             END-PERFORM
           ELSE
      *      患者請求額の変更がない方：該当者チェック抽出分の収納更新
             INITIALIZE                  WRK-PTID-TABLE
             MOVE  WRK-PTID-TABLE2  TO   WRK-PTID-TABLE
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                       UNTIL  (IDX1            >  10000)
                        OR    (WRK-PTID(IDX1)  =  ZERO )
               PERFORM  400-SYUNOU-KOUSIN-SEC
             END-PERFORM
           END-IF
      *
      *    患者請求額の変更がある方：該当者チェック抽出分の収納更新
           INITIALIZE                    WRK-PTID-TABLE
           MOVE    WRK-PTID-TABLE3  TO   WRK-PTID-TABLE
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                       UNTIL  (IDX1            >  10000)
                        OR    (WRK-PTID(IDX1)  =  ZERO )
             PERFORM    400-SYUNOU-KOUSIN-SEC
           END-PERFORM
      *
           .
      *
       MAIN-EXT.
           EXIT PROGRAM
           STOP RUN.
      *
      **************************************************************
      *    一括再計算
      **************************************************************
       100-IKATUSAIKEI-SEC           SECTION.
      *
           MOVE   SPACE   TO  SPA-ERRCD
      *
           MOVE   ZERO    TO  IDX2
           INITIALIZE         WRK-DENPNUM-END-TABLE
      *
           MOVE   ZERO    TO  FLG-GENBUTU
           MOVE   ZERO    TO  FLG-GENBUTU2
           MOVE   ZERO    TO  FLG-GENBUTU3
           MOVE   ZERO    TO  FLG-GENBUTU4
      *
           INITIALIZE                              SYUNOU-REC
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM    TO  SYU-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE   WRK-PTID(IDX1)               TO  SYU-PTID
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM      TO  SYU-SRYYMD(1:6)
           MOVE   "__"                         TO  SYU-SRYYMD(7:2)
           MOVE   SYUNOU-REC                   TO  MCPDATA-REC
           MOVE   "DBSELECT"                   TO  MCP-FUNC
           MOVE   "tbl_syunou"                 TO  MCP-TABLE
           MOVE   "key35"                      TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      "tbl_syunou"            TO  MCP-TABLE
             MOVE      "key35"                 TO  MCP-PATHNAME
             PERFORM    900-SYUNOU-READ-SEC
             PERFORM    UNTIL  FLG-SYUNOU  =  1
               IF  SYU-DENPJTIKBN  NOT = "3"
                 IF   SYU-SAIKEISANKBN     = "1"
                    MOVE   1   TO  FLG-GENBUTU2
                 END-IF
                 IF  (SYU-SYUHKNNUM    NOT = SPACE)
                 AND (SYU-KOH1HKNNUM   NOT = SPACE)
                 AND (SYU-KOH1HKNNUM   NOT = "970")
                    MOVE   1   TO  FLG-GENBUTU3
                 END-IF
      *
                 IF   SYU-KOH1HKNNUM       = "970"
                    MOVE   1   TO  FLG-GENBUTU4
                 END-IF
               END-IF
               MOVE    "tbl_syunou"            TO  MCP-TABLE
               MOVE    "key35"                 TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE        "tbl_syunou"            TO  MCP-TABLE
           MOVE        "key35"                 TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
           IF     FLG-GENBUTU4  =   1
                    MOVE    2     TO  FLG-GENBUTU
           ELSE
             IF  (FLG-GENBUTU2  =   1)
             AND (FLG-GENBUTU3  =   1)
                    MOVE    1     TO  FLG-GENBUTU
             END-IF
           END-IF
      *
      *    第三者行為診療分が１件でもある場合
           IF     FLG-GENBUTU   =   2
      *      通常診療分の計算
             IF     SYS-1039-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key88"        TO  WRK-IKT-PATHNAME
             ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key89"        TO  WRK-IKT-PATHNAME
             END-IF
             PERFORM  200-IKATUSAIKEI2-SEC
      *      第三者行為診療分の計算
             IF     SYS-1039-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key90"        TO  WRK-IKT-PATHNAME
             ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key91"        TO  WRK-IKT-PATHNAME
             END-IF
             PERFORM  200-IKATUSAIKEI2-SEC
           ELSE
      *      第三者行為診療分がない場合
             IF   FLG-GENBUTU = ZERO
               IF   SYS-1039-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key35"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key69"        TO  WRK-IKT-PATHNAME
               END-IF
             ELSE
               IF   SYS-1039-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key82"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key83"        TO  WRK-IKT-PATHNAME
               END-IF
             END-IF
             PERFORM  200-IKATUSAIKEI2-SEC
           END-IF
      *
           .
       100-IKATUSAIKEI-EXT.
           EXIT.
      *
      **************************************************************
      *    一括再計算
      **************************************************************
       200-IKATUSAIKEI2-SEC           SECTION.
      *
           INITIALIZE                              SYUNOU-REC
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM    TO  SYU-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE   WRK-PTID(IDX1)               TO  SYU-PTID
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM      TO  SYU-SRYYMD(1:6)
           MOVE   "__"                         TO  SYU-SRYYMD(7:2)
           MOVE   SYUNOU-REC                   TO  MCPDATA-REC
           MOVE   "DBSELECT"                   TO  MCP-FUNC
           MOVE   WRK-IKT-TABLE                TO  MCP-TABLE
           MOVE   WRK-IKT-PATHNAME             TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      WRK-IKT-TABLE           TO  MCP-TABLE
             MOVE      WRK-IKT-PATHNAME        TO  MCP-PATHNAME
             PERFORM    900-SYUNOU-READ-SEC
             PERFORM    UNTIL  FLG-SYUNOU  =  1
               MOVE     ZERO     TO  FLG-ARI
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                         UNTIL  (IDX3                   >  100 )
                          OR    (WRK-DENPNUM-END(IDX3)  =  ZERO)
                          OR    (FLG-ARI                =   1  )
                 IF  SYU-DENPNUM  =  WRK-DENPNUM-END(IDX3)
                   MOVE    1     TO  FLG-ARI
                   IF   FLG-SYORI-KBN  =  1
                     MOVE   WRK-SYUNOU-DATA(IDX3)
                                 TO  WRK-SYUNOU-REC
                     PERFORM    300-SYUNOU-DATA-CHECK-SEC
                   END-IF
                 END-IF
               END-PERFORM
      *
               IF  (SYU-DENPJTIKBN  NOT = "3" )
               AND (FLG-ARI             = ZERO)
                 INITIALIZE                 LNK-ORCSSAIKEISAN-REC
                 MOVE   SYU-HOSPNUM    TO   LNK-ORCSSKS-HOSPNUM
                 MOVE   SYU-NYUGAIKBN  TO   LNK-ORCSSKS-NYUGAIKBN
                 MOVE   SYU-PTID       TO   LNK-ORCSSKS-PTID
                 MOVE   SYU-DENPNUM    TO   LNK-ORCSSKS-DENPNUM
                 MOVE   SYU-SRYKA      TO   LNK-ORCSSKS-SRYKA
                 MOVE   SYS-1039-IKTSORTKBN
                                       TO   LNK-ORCSSKS-IKTSORTKBN
                 MOVE   SYS-1039-IKTKEISANKBN
                                       TO   LNK-ORCSSKS-IKTKEISANKBN
                 IF     FLG-GENBUTU  =  1 OR 2
                   MOVE   "1"          TO   LNK-ORCSSKS-IKTKEISANKBN
                   MOVE   "1"          TO   LNK-ORCSSKS-IKTGENKEIFLG
                 END-IF
                 MOVE     "1"          TO   LNK-ORCSSKS-IKTJISSIFLG
      *
                 IF     FLG-SYORI-KBN  =  1
                   MOVE   "1"          TO   LNK-ORCSSKS-IKTCHECKFLG
                 END-IF
      *
                 CALL    "ORCSSAIKEISAN"     USING
                                             SPA-AREA
                                             LNK-ORCSSAIKEISAN-REC
      *
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL  (IDX3                      >  15  )
                            OR    (LNK-ORCSSKS-DENPNUM-END(IDX3)
                                                             =  ZERO)
                   ADD    1        TO   IDX2
                   IF    IDX2     <=  100
                     MOVE   LNK-ORCSSKS-DENPNUM-END(IDX3)
                                   TO   WRK-DENPNUM-END(IDX2)
                     IF   FLG-SYORI-KBN  =  1
                       MOVE   LNK-ORCSSKS-SYUNOU-DATA(IDX3)
                                   TO   WRK-SYUNOU-DATA(IDX2)
                     END-IF
                   END-IF
                 END-PERFORM
      *
                 IF   FLG-SYORI-KBN  =  1
                   MOVE     ZERO     TO  FLG-ARI2
                   PERFORM  VARYING  IDX3  FROM  1  BY  1
                             UNTIL  (IDX3                      >  15  )
                              OR    (LNK-ORCSSKS-DENPNUM-END(IDX3)
                                                               =  ZERO)
                              OR    (FLG-ARI2                  =   1  )
                     IF  SYU-DENPNUM  =  LNK-ORCSSKS-DENPNUM-END(IDX3)
                       MOVE    1     TO  FLG-ARI2
                       MOVE   LNK-ORCSSKS-SYUNOU-DATA(IDX3)
                                     TO  WRK-SYUNOU-REC
                       PERFORM    300-SYUNOU-DATA-CHECK-SEC
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI2  =  ZERO
                     MOVE      1     TO  WRK-CHKNGDATA(IDX1)
                   END-IF
                 END-IF
               END-IF
      *
               MOVE    WRK-IKT-TABLE           TO  MCP-TABLE
               MOVE    WRK-IKT-PATHNAME        TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE        WRK-IKT-TABLE           TO  MCP-TABLE
           MOVE        WRK-IKT-PATHNAME        TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
           .
       200-IKATUSAIKEI2-EXT.
           EXIT.
      *
      **************************************************************
      *    収納データチェック
      **************************************************************
       300-SYUNOU-DATA-CHECK-SEC           SECTION.
      *
           IF  (SYU-SECMONEY         =  1 OR 2 OR 3 OR 4)
           OR  (SYU-ACCT-UPDKBN  NOT =  SPACE           )
      *      １：第三者行為現物給付
             IF    SYU-SECMONEY  =  4
               MOVE     1     TO    WRK-GAITOKBN(IDX1)
             END-IF
      *      ２：７０歳未満現物給付
             IF    SYU-SECMONEY  =  1 OR 3
               IF  WRK-GAITOKBN(IDX1)  NOT =  1
                 MOVE   2     TO    WRK-GAITOKBN(IDX1)
               END-IF
             END-IF
      *      ３：高齢者現物給付
             IF    SYU-SECMONEY  =  2
               IF  WRK-GAITOKBN(IDX1)  NOT =  1 AND 2
                 MOVE   3     TO    WRK-GAITOKBN(IDX1)
               END-IF
             END-IF
      *      ４：収納更新未処理
             IF    SYU-ACCT-UPDKBN  NOT =  SPACE
               IF  WRK-GAITOKBN(IDX1)  NOT =  1 AND 2 AND 3
                 MOVE   4     TO    WRK-GAITOKBN(IDX1)
               END-IF
             END-IF
           ELSE
      *      ５：該当者チェック抽出分
             IF    WRK-GAITOKBN(IDX1)  NOT =  1 AND 2 AND 3 AND 4
                 MOVE   5     TO    WRK-GAITOKBN(IDX1)
             END-IF
           END-IF
      *
           IF   SYU-SKYMONEY  NOT =  WRK-SYU-SKYMONEY
      *      患者請求額の変更がある方
             MOVE       1     TO    WRK-SKYMONEY-FLG(IDX1)
             MOVE       1     TO    WRK-TOTALTENKBN-A(IDX1)
           ELSE
      *      患者請求額の変更がない方（データ項目値が変更となる収納あり）
             IF (SYU-TOTAL-HKNTEN      NOT = WRK-SYU-TOTAL-HKNTEN      )
             OR (SYU-RSISHOSHIN-MONEY  NOT = WRK-SYU-RSISHOSHIN-MONEY  )
             OR (SYU-RSISAISHIN-MONEY  NOT = WRK-SYU-RSISAISHIN-MONEY  )
             OR (SYU-RSISDO-MONEY      NOT = WRK-SYU-RSISDO-MONEY      )
             OR (SYU-RSIETC-MONEY      NOT = WRK-SYU-RSIETC-MONEY      )
      *
             OR (SYU-HKNTEKMONEY       NOT = WRK-SYU-HKNTEKMONEY       )
             OR (SYU-SYUCOMPFTN        NOT = WRK-SYU-SYUCOMPFTN        )
             OR (SYU-SYUCOMPFTN-ENTANI NOT = WRK-SYU-SYUCOMPFTN-ENTANI )
             OR (SYU-KOH-FTN           NOT = WRK-SYU-KOH-FTN           )
             OR (SYU-KOH-FTN-ENTANI    NOT = WRK-SYU-KOH-FTN-ENTANI    )
             OR (SYU-KOH1HKNFTNMONEY   NOT = WRK-SYU-KOH1HKNFTNMONEY   )
             OR (SYU-KOH1COMPFTN       NOT = WRK-SYU-KOH1COMPFTN       )
             OR (SYU-KOH1COMPFTN-ENTANI
                                       NOT = WRK-SYU-KOH1COMPFTN-ENTANI)
             OR (SYU-KOH2HKNFTNMONEY   NOT = WRK-SYU-KOH2HKNFTNMONEY   )
             OR (SYU-KOH2COMPFTN       NOT = WRK-SYU-KOH2COMPFTN       )
             OR (SYU-KOH2COMPFTN-ENTANI
                                       NOT = WRK-SYU-KOH2COMPFTN-ENTANI)
             OR (SYU-KOH3HKNFTNMONEY   NOT = WRK-SYU-KOH3HKNFTNMONEY   )
             OR (SYU-KOH3COMPFTN       NOT = WRK-SYU-KOH3COMPFTN       )
             OR (SYU-KOH3COMPFTN-ENTANI
                                       NOT = WRK-SYU-KOH3COMPFTN-ENTANI)
             OR (SYU-KOH4HKNFTNMONEY   NOT = WRK-SYU-KOH4HKNFTNMONEY   )
             OR (SYU-KOH4COMPFTN       NOT = WRK-SYU-KOH4COMPFTN       )
             OR (SYU-KOH4COMPFTN-ENTANI
                                       NOT = WRK-SYU-KOH4COMPFTN-ENTANI)
      *
             OR (SYU-RSIJIBAI-SKYMONEY NOT = WRK-SYU-RSIJIBAI-SKYMONEY )
             OR (SYU-SKJ-FTNGAKU(1)    NOT = WRK-SYU-SKJ-FTNGAKU(1)    )
             OR (SYU-SKJ-FTNGAKU(2)    NOT = WRK-SYU-SKJ-FTNGAKU(2)    )
             OR (SYU-SKJ-FTNGAKU(3)    NOT = WRK-SYU-SKJ-FTNGAKU(3)    )
             OR (SYU-SKJ-FTNDAY(1)     NOT = WRK-SYU-SKJ-FTNDAY(1)     )
             OR (SYU-SKJ-FTNDAY(2)     NOT = WRK-SYU-SKJ-FTNDAY(2)     )
             OR (SYU-SKJ-FTNDAY(3)     NOT = WRK-SYU-SKJ-FTNDAY(3)     )
             OR (SYU-YKZKENNUM         NOT = WRK-SYU-YKZKENNUM         )
             OR (SYU-KOHTAIYKZKBN      NOT = WRK-SYU-KOHTAIYKZKBN      )
             OR (SYU-NYNSHOKAISU       NOT = WRK-SYU-NYNSHOKAISU       )
             OR (SYU-SAIKEISANKBN      NOT = WRK-SYU-SAIKEISANKBN      )
      *
             OR (SYU-SECMONEY          NOT = ZERO AND 9                )
             OR (SYU-ACCT-UPDKBN       NOT = SPACE                     )
               MOVE     1     TO    WRK-TOTALTENKBN-B(IDX1)
             END-IF
           END-IF
      *
           .
       300-SYUNOU-DATA-CHECK-EXT.
           EXIT.
      *
      **************************************************************
      *    収納データ更新
      **************************************************************
       400-SYUNOU-KOUSIN-SEC           SECTION.
      *
           INITIALIZE                              SYUNOU-REC
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM    TO  SYU-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE   WRK-PTID(IDX1)               TO  SYU-PTID
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM      TO  SYU-SRYYMD(1:6)
           MOVE   "__"                         TO  SYU-SRYYMD(7:2)
           MOVE   SYUNOU-REC                   TO  MCPDATA-REC
           MOVE   "DBSELECT"                   TO  MCP-FUNC
           MOVE   "tbl_syunou"                 TO  MCP-TABLE
           MOVE   "key35"                      TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      "tbl_syunou"            TO  MCP-TABLE
             MOVE      "key35"                 TO  MCP-PATHNAME
             PERFORM    900-SYUNOU-READ-SEC
             PERFORM    UNTIL  FLG-SYUNOU  =  1
               IF  SYU-DENPJTIKBN  NOT = "3"
                 MOVE     7                    TO  SYU-SECMONEY
      *
                 MOVE   SYUNOU-REC             TO  MCPDATA-REC
                 MOVE  "DBUPDATE"              TO  MCP-FUNC
                 MOVE  "tbl_syunou"            TO  MCP-TABLE
                 MOVE  "key"                   TO  MCP-PATHNAME
                 CALL  "ORCDBMAIN"          USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               END-IF
               MOVE    "tbl_syunou"            TO  MCP-TABLE
               MOVE    "key35"                 TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE        "tbl_syunou"            TO  MCP-TABLE
           MOVE        "key35"                 TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
           .
       400-SYUNOU-KOUSIN-EXT.
           EXIT.
      *
      **************************************************************
      *    返却値セット１
      **************************************************************
       500-HENKYAKU-SET-SEC           SECTION.
      *
           IF     WRK-GAITOKBN(IDX1)   NOT =  ZERO
             IF  (WRK-TOTALTENKBN-A(IDX1)  =  ZERO)
             AND (WRK-TOTALTENKBN-B(IDX1)  =  ZERO)
               CONTINUE
             ELSE
               ADD        1     TO   IDX2
               MOVE  WRK-PTID(IDX1)
                                TO   LNK-ORCSSYUNOUCHK-PTID(IDX2)
               IF    WRK-SKYMONEY-FLG(IDX1)  =  ZERO
                 IF  LNK-ORCSSYUNOUCHK-OPTION1  =  "1"
                   MOVE   9     TO   LNK-ORCSSYUNOUCHK-JTIFLG(IDX2)
                 ELSE
                   MOVE   8     TO   LNK-ORCSSYUNOUCHK-JTIFLG(IDX2)
                 END-IF
               ELSE
                 MOVE    ZERO   TO   LNK-ORCSSYUNOUCHK-JTIFLG(IDX2)
               END-IF
               MOVE  WRK-GAITOKBN(IDX1)
                                TO   LNK-ORCSSYUNOUCHK-GAITOKBN(IDX2)
      *
               IF    WRK-SKYMONEY-FLG(IDX1)  =  ZERO
                 IF  LNK-ORCSSYUNOUCHK-OPTION1  =  "1"
                     ADD    1     TO   IDX3
                     MOVE   WRK-PTID(IDX1)
                                  TO   WRK-PTID2(IDX3)
                 ELSE
                   IF  WRK-GAITOKBN(IDX1)  =  5
                     ADD    1     TO   IDX3
                     MOVE   WRK-PTID(IDX1)
                                  TO   WRK-PTID2(IDX3)
                   END-IF
                 END-IF
               ELSE
                 IF    WRK-GAITOKBN(IDX1)  =  5
                     ADD    1     TO   IDX4
                     MOVE   WRK-PTID(IDX1)
                                  TO   WRK-PTID3(IDX4)
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           .
       500-HENKYAKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1039-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1039-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納データ読込
      *****************************************************************
       900-SYUNOU-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
      *
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *
