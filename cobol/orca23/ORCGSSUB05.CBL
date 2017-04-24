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
       PROGRAM-ID.                 ORCGSSUB05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納
      *  コンポーネント名  : 再計算サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/04/08    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-ARI             PIC 9(01).
           03  FLG-GENBUTU         PIC 9(01).
           03  FLG-GENBUTU2        PIC 9(01).
           03  FLG-GENBUTU3        PIC 9(01).
           03  FLG-GENBUTU4        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX3                PIC 9(05).
           03  IDX6                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-DENPNUM-END-TABLE.
               05  WRK-DENPNUM-END PIC 9(07)   OCCURS  100.
           03  WRK-IKT-TABLE       PIC X(64).
           03  WRK-IKT-PATHNAME    PIC X(64).
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-TABLE           PIC X(64).
           03  WRK-PATHNAME        PIC X(64).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1039.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    主科情報アクセス用パラメタ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    負担金再計算用パラメタ
           COPY    "CPORCSSAIKEISAN.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "CPSSUB05.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SSUB05-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    SSUB05-SYORIKBN
           WHEN    "1"
               PERFORM 440-SAIKEI-SEC
           WHEN    "2"
               PERFORM 440-IKATUSAIKEI-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      **************************************************************
      *    再計算処理
      **************************************************************
       440-SAIKEI-SEC           SECTION.
      *
      ******************************
      *    負担金再計算処理
      ******************************
           INITIALIZE                      LNK-ORCSSAIKEISAN-REC
      *
           MOVE     SPA-HOSPNUM        TO  LNK-ORCSSKS-HOSPNUM
           MOVE     SSUB05-NYUGAIKBN   TO  LNK-ORCSSKS-NYUGAIKBN
           MOVE     SSUB05-PTID        TO  LNK-ORCSSKS-PTID
           MOVE     SSUB05-DENPNUM     TO  LNK-ORCSSKS-DENPNUM
           MOVE     SSUB05-SRYKA       TO  LNK-ORCSSKS-SRYKA
           MOVE     SSUB05-SYSYMD      TO  LNK-ORCSSKS-SYSYMD
           MOVE     SSUB05-SYSHMS      TO  LNK-ORCSSKS-SYSHMS
      *
           CALL    "ORCSSAIKEISAN"     USING
                                       SPA-AREA
                                       LNK-ORCSSAIKEISAN-REC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               INITIALIZE                  ORCSSYUACCAREA
               MOVE    "03"            TO  LNK-SYUACC-I-KBN
               MOVE    SSUB05-SKYYM    TO  LNK-SYUACC-I-SRYYM
               MOVE    SSUB05-NYUGAIKBN
                                       TO  SPA-NYUGAIKBN
               MOVE    SSUB05-PTID     TO  SPA-PTID
               MOVE    PTINF-BIRTHDAY  TO  SPA-BIRTHDAY
               CALL    "ORCSSYUKAMAIN"     USING
                                           SPA-AREA
                                           ORCSSYUACCAREA
           ELSE
               MOVE    "4008"          TO  SPA-ERRCD
           END-IF
           .
       440-SAIKEI-EXT.
           EXIT.
      **************************************************************
      *    外来分一括再計算処理
      **************************************************************
       440-IKATUSAIKEI-SEC           SECTION.
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SSUB05-SKYYM           TO  SYS-1039-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1039-STYUKYMD(7:2)
           MOVE    SYS-1039-STYUKYMD   TO  SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
      *
           MOVE    ZERO                TO  IDX6
           INITIALIZE     WRK-DENPNUM-END-TABLE
      *
      ***  IF    ( SPA-GMN-NYUGAIKBN   =   "0" OR  "2" )
      ***      MOVE    "2"             TO  WRK-NYUGAIKBN
      ***  ELSE
      ***      GO      TO      440-IKATUSAIKEI-EXT
      ***  END-IF
           MOVE    "2"                 TO  WRK-NYUGAIKBN
      *
      *    現物給付計算：一括再計算対象の収納データがあるかチェック
      *
           MOVE    ZERO                TO  FLG-GENBUTU
           MOVE    ZERO                TO  FLG-GENBUTU2
           MOVE    ZERO                TO  FLG-GENBUTU3
           MOVE    ZERO                TO  FLG-GENBUTU4
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SSUB05-PTID        TO  SYU-PTID
           MOVE    SSUB05-SKYYM           TO  SYU-SRYYMD (1:6)
           MOVE    "__"                TO  SYU-SRYYMD (7:)
           MOVE    "tbl_syunou"        TO  WRK-TABLE
           MOVE    "key35"             TO  WRK-PATHNAME
           PERFORM 900-SYUNOU-SEL-IKT-SEC
           PERFORM   UNTIL   FLG-SYUNOU  =  1
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               IF    SYU-DENPJTIKBN  NOT =  "3"
                 IF   SYU-SAIKEISANKBN    =  "1"
                    MOVE    1     TO  FLG-GENBUTU2
                 END-IF
                 IF  (SYU-SYUHKNNUM   NOT = SPACE)
                 AND (SYU-KOH1HKNNUM  NOT = SPACE)
                 AND (SYU-KOH1HKNNUM  NOT = "970")
                    MOVE    1     TO  FLG-GENBUTU3
                 END-IF
      *
                 IF   SYU-KOH1HKNNUM  = "970"
                    MOVE    1     TO  FLG-GENBUTU4
                 END-IF
               END-IF
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key35"         TO  WRK-PATHNAME
               PERFORM 900-SYUNOU-FET-SEC
           END-PERFORM
           MOVE    "tbl_syunou"   TO  MCP-TABLE
           MOVE    "key35"        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
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
             PERFORM  4402-IKATUSAIKEI-HENSYU-SEC
      *      第三者行為診療分の計算
             IF     SYS-1039-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key90"        TO  WRK-IKT-PATHNAME
             ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key91"        TO  WRK-IKT-PATHNAME
             END-IF
             PERFORM  4402-IKATUSAIKEI-HENSYU-SEC
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
             PERFORM  4402-IKATUSAIKEI-HENSYU-SEC
           END-IF
      *
           IF    ( SPA-ERRCD      NOT =   "0070" )
      *
               IF    SPA-ERRCD    NOT =  SPACE
                      MOVE    "0064"     TO  SPA-ERRCD
               ELSE
                  IF    WRK-DENPNUM-END(1)  =  ZERO
                      MOVE    "0060"     TO  SPA-ERRCD
                  END-IF
               END-IF
      *
               INITIALIZE                 ORCSSYUACCAREA
               MOVE    "03"           TO  LNK-SYUACC-I-KBN
               MOVE    SSUB05-SKYYM      TO  LNK-SYUACC-I-SRYYM
               MOVE    WRK-NYUGAIKBN  TO  SPA-NYUGAIKBN
               MOVE    SSUB05-PTID   TO  SPA-PTID
               MOVE    PTINF-BIRTHDAY
                                      TO  SPA-BIRTHDAY
               CALL    "ORCSSYUKAMAIN"    USING
                                          SPA-AREA
                                          ORCSSYUACCAREA
           END-IF
      *
           .
       440-IKATUSAIKEI-EXT.
           EXIT.
      *
      **************************************************************
      *    外来分一括再計算編集処理
      **************************************************************
       4402-IKATUSAIKEI-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SSUB05-PTID        TO  SYU-PTID
           MOVE    SSUB05-SKYYM           TO  SYU-SRYYMD (1:6)
           MOVE    "__"                TO  SYU-SRYYMD (7:)
           MOVE    WRK-IKT-TABLE       TO  WRK-TABLE
           MOVE    WRK-IKT-PATHNAME    TO  WRK-PATHNAME
           PERFORM 900-SYUNOU-SEL-IKT-SEC
      *
           PERFORM   UNTIL ( FLG-SYUNOU  =   1 )
      *
               MOVE    MCPDATA-REC     TO   SYUNOU-REC
      *
               MOVE    ZERO            TO   FLG-ARI
               PERFORM    VARYING   IDX3   FROM   1   BY   1
                       UNTIL  (IDX3                   >  100 )
                         OR   (WRK-DENPNUM-END(IDX3)  =  ZERO)
                         OR   (FLG-ARI                =   1  )
                   IF  WRK-DENPNUM-END(IDX3)  =   SYU-DENPNUM
                         MOVE   1    TO   FLG-ARI
                   END-IF
               END-PERFORM
      *
               IF   SYU-DENPJTIKBN  NOT =   "3"
               AND  FLG-ARI             =   ZERO
                  INITIALIZE                   LNK-ORCSSAIKEISAN-REC
      *
                  MOVE     SYU-HOSPNUM    TO   LNK-ORCSSKS-HOSPNUM
                  MOVE     SYU-NYUGAIKBN  TO   LNK-ORCSSKS-NYUGAIKBN
                  MOVE     SYU-PTID       TO   LNK-ORCSSKS-PTID
                  MOVE     SYU-DENPNUM    TO   LNK-ORCSSKS-DENPNUM
                  MOVE     SYU-SRYKA      TO   LNK-ORCSSKS-SRYKA
                  MOVE     SYS-1039-IKTSORTKBN
                                          TO   LNK-ORCSSKS-IKTSORTKBN
                  MOVE     SYS-1039-IKTKEISANKBN
                                          TO   LNK-ORCSSKS-IKTKEISANKBN
                  IF   FLG-GENBUTU  =  1 OR 2
                    MOVE     "1"          TO   LNK-ORCSSKS-IKTKEISANKBN
                    MOVE     "1"          TO   LNK-ORCSSKS-IKTGENKEIFLG
                  END-IF
                  MOVE       "1"          TO   LNK-ORCSSKS-IKTJISSIFLG
                  MOVE     SSUB05-SYSYMD  TO  LNK-ORCSSKS-SYSYMD
                  MOVE     SSUB05-SYSHMS  TO  LNK-ORCSSKS-SYSHMS
      *
                  CALL    "ORCSSAIKEISAN"     USING
                                              SPA-AREA
                                              LNK-ORCSSAIKEISAN-REC
      *
                  PERFORM    VARYING   IDX3   FROM   1   BY   1
                       UNTIL  (IDX3                          >   15 )
                         OR   (LNK-ORCSSKS-DENPNUM-END(IDX3) =  ZERO)
                      ADD    1     TO   IDX6
                      IF    IDX6     <=  100
                        MOVE   LNK-ORCSSKS-DENPNUM-END(IDX3)
                                   TO   WRK-DENPNUM-END(IDX6)
                      END-IF
                  END-PERFORM
      *
               END-IF
      *
               MOVE    WRK-IKT-TABLE       TO  WRK-TABLE
               MOVE    WRK-IKT-PATHNAME    TO  WRK-PATHNAME
               PERFORM 900-SYUNOU-FET-SEC
      *
               IF    SPA-ERRCD   NOT =  SPACE
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-IKT-TABLE       TO  MCP-TABLE
           MOVE    WRK-IKT-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           .
       4402-IKATUSAIKEI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC                SECTION.
      *
           CALL    "ORCSDAILYKEY"      USING
                                       SDAILYKEYAREA
                                       SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理（一括再計算）
      **************************************************************
       900-SYUNOU-SEL-IKT-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SEL-IKT-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
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
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      **************************************************************
      *    患者情報検索処理(KEY)
      **************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SSUB05-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
