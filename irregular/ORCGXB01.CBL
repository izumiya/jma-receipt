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
       PROGRAM-ID.                 ORCGXB01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ジョブ管理検索
      *  コンポーネント名  : ジョブ管理検索（ＸＢ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/05/10    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−太田   04/01/23  DBSTART,DBCOMMIT処理削除
      *  02.07.00    NACL-竹田    05/10/24  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/05/22  グループ診療対応
      *****************************************************************
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    シェルＩＤ対応ＪＯＢ名称テーブル
           COPY    "CMJOBNAME.INC".
      *
      *     XB01画面ＳＰＡ
           COPY    "XB01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-JOBKANRI        PIC 9(01).
      *
      *    処理区分領域
       01  SKBN-AREA.
           03  SKBN-IDCODE         PIC X(04).
           03  SKBN-RUN            PIC X(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(04).
           03  IDY4                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK1-ZZ             PIC ZZZ9.
           03  WRK-JOBNAME         PIC X(38).
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
           03  WRK-HENTIME         PIC X(08).
       01  WRK-HIKAKU-AREA.
           03  WRK-ENDYMD          PIC X(09).
           03  WRK-ENDTIME         PIC X(08).
           03  WRK-STEPSTARTTIME   PIC X(08).
           03  WRK-STEPENDTIME     PIC X(08).
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
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
           COPY    "XB01.INC".
           COPY    "XBER.INC".
           COPY    "XBID.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-ORCXB01.
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-ORCXB01     TO  SPA-FREE.
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
           INITIALIZE                  FLG-AREA.
           INITIALIZE                  WRK-AREA.
      *
      *    エラー画面より
           IF  SPA-MOTOPG   =   "XBER"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5011-MAPCUR-SEC
               MOVE    ZERO                TO  SPA-GMN-SELNO
               MOVE    ZERO                TO  SPA-KBN-JOBID
               MOVE    SPACE               TO  SPA-KBN-SHELLID
               PERFORM VARYING     IDY4  FROM   1   BY  1
                       UNTIL       IDY4  >   SPA-GMN-MAX
                  IF  XB01-SELECT (IDY4)   =   "T"
                      MOVE    "F"                 TO  XB01-SELECT (IDY4)
                  END-IF
               END-PERFORM
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF.
      *
           IF  SPA-ERRCD   =   "0004"
               PERFORM 502-ERRSET-SEC
           ELSE
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "XB01"              TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF.
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           IF  SKBN-IDCODE   NOT =   "0201"
               INITIALIZE              SPA-ORCXB01
           END-IF.
      *
      *    他のＬＤより
           IF  LINKAREA   NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF.
      *
           PERFORM 310-SPASET-SEC.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPA-MOTOPG          TO  SPA-NAI-MOTOPG.
      *
           IF  (SKBN-IDCODE    =   "0201")  AND
               (SPA-KBN-FLG    =   "OK")
               PERFORM 2013-JOBKANRI-DELETE-SEC
           END-IF.
      *
           PERFORM 2011-JOBKANRI-HEN-SEC.
           MOVE    SPACE               TO  SKBN-IDCODE.
           MOVE    ZERO                TO  SPA-KBN-SHELLID.
      *
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        終了
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        削除
               WHEN    "CLICKED"       ALSO    "B02"
                   IF  (SPA-KBN-JOBID     NOT =   ZERO)  AND
                       (SPA-KBN-SHELLID   NOT =   SPACE)
                       PERFORM 201-DELETE-SEC
                   END-IF
                   MOVE    ZERO        TO  SPA-GMN-SELNO
      *        状況
               WHEN    "CLICKED"       ALSO    "B11"
                   MOVE    ZERO        TO  SPA-KBN-SHELLID
                   PERFORM 2011-JOBKANRI-HEN-SEC
                   MOVE    ZERO        TO  SPA-GMN-SELNO
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        番号選択入力
               WHEN    "ACTIVATE"      ALSO    "SELNO"
                   PERFORM 202-SELNO-SEC
      *        行選択
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 203-SELECT-SEC
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除　処理
      *****************************************************************
       201-DELETE-SEC                  SECTION.
      *
           MOVE    SPACE                  TO  SKBN-IDCODE.
           MOVE    SPACE                  TO  SKBN-RUN.
           MOVE    SPACE                  TO  WRK-JOBNAME.
           MOVE    SPA-GMN-SELNO          TO  IDX.
      *
           MOVE    "0201"                       TO  SKBN-IDCODE.
           MOVE    SPA-GMN-JOBNAME (IDX)(1:38)  TO  WRK-JOBNAME.
           MOVE    SPACE                        TO  XBID.
           MOVE    "0201"                       TO  XBID-IDCODE.
           STRING
               WRK-JOBNAME                      DELIMITED  BY  SPACE
               "を削除します。よろしいですか？" DELIMITED  BY  SIZE
                                                INTO XBID-IDMSG.
      *
           PERFORM VARYING     IDX    FROM    1   BY  1
                   UNTIL       IDX    >   SPA-GMN-MAX
               IF  SPA-KBN-SHELLID   =  SPA-GMN-SHELLID (IDX)
                   MOVE    SPA-GMN-JOBNAME (IDX)(1:26)
                                               TO  WRK-JOBNAME
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    SPA-GMN-JOBID (IDX) TO  JOB-JOBID
                   MOVE    SPA-KBN-SHELLID     TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                   MOVE    JOBKANRI-REC        TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "tbl_jobkanri"      TO  MCP-TABLE
                   MOVE    "key"
                                               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF  MCP-RC   =   ZERO
                       MOVE    "DBFETCH"       TO  MCP-FUNC
                       MOVE    "tbl_jobkanri"  TO  MCP-TABLE
                       MOVE    "key"
                                               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       MOVE    MCPDATA-REC     TO  JOBKANRI-REC
                       IF  (MCP-RC         =   ZERO)   AND
                           ((JOB-ENDYMD    =   SPACE)  OR
                            (JOB-ENDTIME   =   SPACE))
                           IF  JOB-ERRCD   NOT =   SPACE
                               MOVE    "END"           TO  SKBN-RUN
                           END-IF
                           IF  (SKBN-RUN    =   SPACE) AND
                               (JOB-ERRCD   =   SPACE)
                               MOVE    "RUN"           TO  SKBN-RUN
                           END-IF
                           IF  (JOB-ENDYMD  NOT =  SPACE)  AND
                               (JOB-ENDYMD(7:2) =  SPACE)
                               MOVE    "20"            TO  WRK-SYMD(1:2)
                               MOVE    JOB-ENDYMD(1:6) TO  WRK-SYMD(3:6)
                           ELSE
                               MOVE    JOB-ENDYMD      TO  WRK-SYMD
                           END-IF
                           PERFORM 700-SEIWA-HEN-SEC
                           IF  (SKBN-RUN        =  SPACE)  AND
                               (WRK-HENYMDG NOT =  SPA-GMN-ENDYMD (IDX))
                               MOVE    "RUN"           TO  SKBN-RUN
                           END-IF
                           IF  JOB-ENDTIME   =   SPACE
                               MOVE    JOB-ENDTIME  TO  WRK-HENTIME
                           ELSE
                               MOVE    ":"          TO  WRK-HENTIME(3:1)
                               MOVE    ":"          TO  WRK-HENTIME(6:1)
                               MOVE    JOB-ENDTIME(1:2)
                                                    TO  WRK-HENTIME(1:2)
                               MOVE    JOB-ENDTIME(3:2)
                                                    TO  WRK-HENTIME(4:2)
                               MOVE    JOB-ENDTIME(5:2)
                                                    TO  WRK-HENTIME(7:2)
                           END-IF
                           IF  (SKBN-RUN        = SPACE)  AND
                               (WRK-HENTIME NOT = SPA-GMN-ENDTIME (IDX))
                               MOVE    "RUN"        TO  SKBN-RUN
                           END-IF
                           IF  JOB-STEPENDTIME   =   SPACE
                               MOVE    JOB-STEPENDTIME
                                                    TO  WRK-HENTIME
                           ELSE
                               MOVE    ":"          TO  WRK-HENTIME(3:1)
                               MOVE    ":"          TO  WRK-HENTIME(6:1)
                               MOVE    JOB-STEPENDTIME(1:2)
                                                    TO  WRK-HENTIME(1:2)
                               MOVE    JOB-STEPENDTIME(3:2)
                                                    TO  WRK-HENTIME(4:2)
                               MOVE    JOB-STEPENDTIME(5:2)
                                                    TO  WRK-HENTIME(7:2)
                           END-IF
                           IF  (SKBN-RUN        = SPACE)  AND
                               (WRK-HENTIME NOT =
                                              SPA-GMN-STEPENDTIME (IDX))
                               MOVE    "RUN"        TO  SKBN-RUN
                           END-IF
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_jobkanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-PERFORM.
      *
           IF  SKBN-RUN   =   "RUN"
               MOVE    "0201"                       TO  SKBN-IDCODE
               MOVE    SPACE                        TO  XBID
               MOVE    "0202"                       TO  XBID-IDCODE
               STRING
                   "実行中の処理"                   DELIMITED  BY  SIZE
                   WRK-JOBNAME                      DELIMITED  BY  SPACE
                   "を削除します。よろしいですか？" DELIMITED  BY  SIZE
                                                    INTO XBID-IDMSG
           END-IF.
      *
           MOVE    "B01"                TO  MCP-WIDGET.
           MOVE    "XB01"               TO  SPA-MOTOPG.
           MOVE    "XBID"               TO  SPA-SAKIPG.
           MOVE    "NEW"                TO  MCP-PUTTYPE.
           MOVE    "XBID"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                    TO  FLG-END.
      *
       201-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理検索処理
      *****************************************************************
       2011-JOBKANRI-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX.
           MOVE    ZERO                TO  SPA-GMN-PAGE.
      *
           INITIALIZE                  SPA-GMN-TBLG.
           MOVE    ZERO                TO  SPA-KBN-JOBID.
           MOVE    SPACE               TO  SPA-KBN-SHELLID.
           MOVE    1                   TO  IDX.
      *
           INITIALIZE                  JOBKANRI-REC.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "all02"
                                       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF  MCP-RC   =   ZERO
              MOVE    "tbl_jobkanri"   TO  MCP-TABLE
               MOVE    "all02"
                                       TO  MCP-PATHNAME
               PERFORM 810-JOBKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-JOBKANRI
           END-IF.
      *
           MOVE    IDX                 TO  SPA-GMN-NO      (IDX).
           MOVE    IDX                 TO  SPA-GMN-JOBID   (IDX).
           MOVE    "指定された条件の処理が存在しません"
                                       TO  SPA-GMN-JOBNAME (IDX).
           MOVE    IDX                 TO  SPA-GMN-MAX.
      *
           PERFORM UNTIL      (IDX            >   100) OR
                              (FLG-JOBKANRI   =     1)
               IF  (SPA-JOB-SHELLID   =   JOB-SHELLID) OR
                   (SPA-JOB-SHELLID   =   "SHEL-ALL") OR
                  ((SPA-JOB-SHELLID   =   "SOKATU") AND
                   (JOB-SHELLID       =   "COLLECT"))
                   PERFORM 2012-JOBKANRI-HENSYU-SEC
               END-IF
               MOVE    "tbl_jobkanri"   TO  MCP-TABLE
               MOVE    "all02"
                                        TO  MCP-PATHNAME
               PERFORM 810-JOBKANRI-READ-SEC
           END-PERFORM.
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "all02"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF  (IDX              >   100)   AND
               (FLG-JOBKANRI     =   ZERO)
               MOVE    "0003"          TO  SPA-ERRCD
           END-IF.
      *
       2011-JOBKANRI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理編集処理
      *****************************************************************
       2012-JOBKANRI-HENSYU-SEC       SECTION.
      *
           MOVE    IDX               TO  SPA-GMN-NO      (IDX).
           MOVE    JOB-JOBID         TO  SPA-GMN-JOBID   (IDX).
           MOVE    JOB-SHELLID       TO  SPA-GMN-SHELLID (IDX).
           MOVE    JOB-TERMID        TO  SPA-GMN-TERMID  (IDX).
           MOVE    SPACE             TO  SPA-GMN-JOBNAME (IDX).
      *
           PERFORM VARYING     TBL-IDX    FROM    1   BY  1
                   UNTIL       TBL-IDX    >   MNAME-MAX
               IF  JOB-SHELLID   =  MNAME-SHELLID (TBL-IDX)
                   MOVE    MNAME-JOBNAME (TBL-IDX)
                                             TO SPA-GMN-JOBNAME (IDX)
               END-IF
           END-PERFORM.
           IF  SPA-GMN-JOBNAME (IDX)   =  SPACE
               IF  JOB-SHELLID             =  "INFECTIO"
                 MOVE    "感染症サーベイ"      TO SPA-GMN-JOBNAME (IDX)
               ELSE
                 MOVE    "【未設定です】"      TO SPA-GMN-JOBNAME (IDX)
               END-IF
           END-IF.
      *
           MOVE    38                TO  IDY1.
           MOVE    ZERO              TO  IDY2.
           PERFORM UNTIL       IDY1    <   1
               IF  SPA-GMN-JOBNAME (IDX)(IDY1:1)   NOT =  SPACE
                   COMPUTE IDY2      =     IDY1        +   2
                   MOVE    ZERO              TO IDY1
               ELSE
                   COMPUTE IDY1      =     IDY1        -   1
               END-IF
           END-PERFORM.
           IF  (IDY2    NOT =   ZERO)   AND
               (IDY2        <   38)
               COMPUTE IDY3      =     39          -   IDY2
               MOVE    JOB-SHELLMSG(1:IDY3)
                                 TO  SPA-GMN-JOBNAME (IDX)(IDY2:IDY3)
           END-IF.
      *
           IF  (JOB-STARTYMD    NOT =   SPACE)  AND
               (JOB-STARTYMD(7:2)   =   SPACE)
               MOVE    "20"              TO  WRK-SYMD(1:2)
               MOVE    JOB-STARTYMD(1:6) TO  WRK-SYMD(3:6)
           ELSE
               MOVE    JOB-STARTYMD      TO  WRK-SYMD
           END-IF.
           PERFORM 700-SEIWA-HEN-SEC.
           MOVE    WRK-HENYMDG       TO  SPA-GMN-STARTYMD (IDX).
           IF  (JOB-ENDYMD    NOT =   SPACE)  AND
               (JOB-ENDYMD(7:2)   =   SPACE)
               MOVE    "20"              TO  WRK-SYMD(1:2)
               MOVE    JOB-ENDYMD(1:6)   TO  WRK-SYMD(3:6)
           ELSE
               MOVE    JOB-ENDYMD        TO  WRK-SYMD
           END-IF.
           PERFORM 700-SEIWA-HEN-SEC.
           MOVE    WRK-HENYMDG       TO  SPA-GMN-ENDYMD (IDX).
           IF  JOB-STARTTIME   =   SPACE
               MOVE    JOB-STARTTIME  TO  SPA-GMN-STARTTIME (IDX)
           ELSE
               MOVE    ":"            TO  SPA-GMN-STARTTIME (IDX)(3:1)
               MOVE    ":"            TO  SPA-GMN-STARTTIME (IDX)(6:1)
               MOVE    JOB-STARTTIME(1:2)
                                      TO  SPA-GMN-STARTTIME (IDX)(1:2)
               MOVE    JOB-STARTTIME(3:2)
                                      TO  SPA-GMN-STARTTIME (IDX)(4:2)
               MOVE    JOB-STARTTIME(5:2)
                                      TO  SPA-GMN-STARTTIME (IDX)(7:2)
           END-IF.
           IF  JOB-ENDTIME   =   SPACE
               MOVE    JOB-ENDTIME       TO  SPA-GMN-ENDTIME (IDX)
           ELSE
               MOVE    ":"               TO  SPA-GMN-ENDTIME (IDX)(3:1)
               MOVE    ":"               TO  SPA-GMN-ENDTIME (IDX)(6:1)
               MOVE    JOB-ENDTIME(1:2)  TO  SPA-GMN-ENDTIME (IDX)(1:2)
               MOVE    JOB-ENDTIME(3:2)  TO  SPA-GMN-ENDTIME (IDX)(4:2)
               MOVE    JOB-ENDTIME(5:2)  TO  SPA-GMN-ENDTIME (IDX)(7:2)
           END-IF.
           IF  JOB-STEPSTARTTIME   =   SPACE
               MOVE    JOB-STEPSTARTTIME TO  SPA-GMN-STEPSTARTTIME (IDX)
           ELSE
               MOVE    ":"          TO  SPA-GMN-STEPSTARTTIME (IDX)(3:1)
               MOVE    ":"          TO  SPA-GMN-STEPSTARTTIME (IDX)(6:1)
               MOVE    JOB-STEPSTARTTIME(1:2)
                                    TO  SPA-GMN-STEPSTARTTIME (IDX)(1:2)
               MOVE    JOB-STEPSTARTTIME(3:2)
                                    TO  SPA-GMN-STEPSTARTTIME (IDX)(4:2)
               MOVE    JOB-STEPSTARTTIME(5:2)
                                    TO  SPA-GMN-STEPSTARTTIME (IDX)(7:2)
           END-IF.
           IF  JOB-STEPENDTIME   =   SPACE
               MOVE    JOB-STEPENDTIME TO  SPA-GMN-STEPENDTIME (IDX)
           ELSE
               MOVE    ":"          TO  SPA-GMN-STEPENDTIME (IDX)(3:1)
               MOVE    ":"          TO  SPA-GMN-STEPENDTIME (IDX)(6:1)
               MOVE    JOB-STEPENDTIME(1:2)
                                    TO  SPA-GMN-STEPENDTIME (IDX)(1:2)
               MOVE    JOB-STEPENDTIME(3:2)
                                    TO  SPA-GMN-STEPENDTIME (IDX)(4:2)
               MOVE    JOB-STEPENDTIME(5:2)
                                    TO  SPA-GMN-STEPENDTIME (IDX)(7:2)
           END-IF.
      *
           MOVE    JOB-ERRCD         TO  SPA-GMN-ERRCD (IDX).
           MOVE    IDX               TO  SPA-GMN-MAX.
      *
           ADD     1                 TO  IDX.
      *
       2012-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理削除処理
      *****************************************************************
       2013-JOBKANRI-DELETE-SEC       SECTION.
      *
           PERFORM VARYING     IDX    FROM    1   BY  1
                   UNTIL       IDX    >   SPA-GMN-MAX
               IF  SPA-KBN-SHELLID   =  SPA-GMN-SHELLID (IDX)
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    SPA-GMN-JOBID (IDX) TO  JOB-JOBID
                   MOVE    SPA-KBN-SHELLID     TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                   MOVE    JOBKANRI-REC        TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "tbl_jobkanri"      TO  MCP-TABLE
                   MOVE    "key2"
                                               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF  MCP-RC   =   ZERO
                       MOVE    "DBFETCH"       TO  MCP-FUNC
                       MOVE    "tbl_jobkanri"  TO  MCP-TABLE
                       MOVE    "key2"
                                               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       MOVE    MCPDATA-REC     TO  JOBKANRI-REC
                       IF  MCP-RC   =   ZERO
                           IF  (JOB-ENDYMD    NOT =   SPACE)  AND
                               (JOB-ENDYMD(7:2)   =   SPACE)
                               MOVE    "20"            TO  WRK-SYMD(1:2)
                               MOVE    JOB-ENDYMD(1:6) TO  WRK-SYMD(3:6)
                           ELSE
                               MOVE    JOB-ENDYMD      TO  WRK-SYMD
                           END-IF
                           PERFORM 700-SEIWA-HEN-SEC
                           MOVE    WRK-HENYMDG          TO  WRK-ENDYMD
                           IF  JOB-ENDTIME   =   SPACE
                               MOVE    JOB-ENDTIME  TO  WRK-ENDTIME
                           ELSE
                               MOVE    ":"          TO  WRK-ENDTIME(3:1)
                               MOVE    ":"          TO  WRK-ENDTIME(6:1)
                               MOVE    JOB-ENDTIME(1:2)
                                                    TO  WRK-ENDTIME(1:2)
                               MOVE    JOB-ENDTIME(3:2)
                                                    TO  WRK-ENDTIME(4:2)
                               MOVE    JOB-ENDTIME(5:2)
                                                    TO  WRK-ENDTIME(7:2)
                           END-IF
                           IF  JOB-STEPSTARTTIME   =   SPACE
                               MOVE    JOB-STEPSTARTTIME
                                              TO  WRK-STEPSTARTTIME
                           ELSE
                               MOVE    ":"    TO  WRK-STEPSTARTTIME(3:1)
                               MOVE    ":"    TO  WRK-STEPSTARTTIME(6:1)
                               MOVE    JOB-STEPSTARTTIME(1:2)
                                              TO  WRK-STEPSTARTTIME(1:2)
                               MOVE    JOB-STEPSTARTTIME(3:2)
                                              TO  WRK-STEPSTARTTIME(4:2)
                               MOVE    JOB-STEPSTARTTIME(5:2)
                                              TO  WRK-STEPSTARTTIME(7:2)
                           END-IF
                           IF  JOB-STEPENDTIME   =   SPACE
                                MOVE    JOB-STEPENDTIME
                                                TO  WRK-STEPENDTIME
                           ELSE
                               MOVE    ":"      TO  WRK-STEPENDTIME(3:1)
                               MOVE    ":"      TO  WRK-STEPENDTIME(6:1)
                               MOVE    JOB-STEPENDTIME(1:2)
                                                TO  WRK-STEPENDTIME(1:2)
                               MOVE    JOB-STEPENDTIME(3:2)
                                                TO  WRK-STEPENDTIME(4:2)
                               MOVE    JOB-STEPENDTIME(5:2)
                                                TO  WRK-STEPENDTIME(7:2)
                           END-IF
                           IF  (WRK-ENDYMD          NOT =
                                         SPA-GMN-ENDYMD (IDX)) OR
                               (WRK-ENDTIME         NOT =
                                         SPA-GMN-ENDTIME (IDX)) OR
                               (WRK-STEPSTARTTIME   NOT =
                                         SPA-GMN-STEPSTARTTIME (IDX)) OR
                               (WRK-STEPENDTIME     NOT =
                                         SPA-GMN-STEPENDTIME (IDX))
                               MOVE    "0004"         TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
                   MOVE    "tbl_jobkanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-PERFORM.
      *    ジョブ管理削除 JOBID で DEL 指定   SHELLID で ALL 指定
           IF  SPA-ERRCD   NOT =   "0004"
               INITIALIZE                      JOBKANRI-REC
               MOVE    SPA-KBN-SHELLID     TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    JOBKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "del12"
                                           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
           END-IF.
      *
       2013-JOBKANRI-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除項目入力　処理
      *****************************************************************
       202-SELNO-SEC               SECTION.
      *
           IF  XB01-SELNO   =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
               MOVE    XB01-SELNO          TO  SPA-GMN-SELNO
      *
      *    選択番号チェック
               IF  (SPA-GMN-SELNO   =   ZERO)  OR
                   (SPA-GMN-SELNO   >   SPA-GMN-MAX)
                   MOVE    "0002"              TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-SHELLID (SPA-GMN-SELNO)
                                               TO  SPA-KBN-SHELLID
                   MOVE    SPA-GMN-JOBID (SPA-GMN-SELNO)
                                               TO  SPA-KBN-JOBID
               END-IF
           END-IF.
      *
       202-SELNO-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面選択入力　処理
      *****************************************************************
       203-SELECT-SEC               SECTION.
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-GMN-MAX
              IF  (XB01-SELECT (IDX)           =  "T")  AND
                  (SPA-GMN-SHELLID (IDX)   NOT =  SPA-KBN-SHELLID)
                  MOVE    IDX                       TO  SPA-GMN-SELNO
                  MOVE    SPA-GMN-SHELLID (IDX)     TO  SPA-KBN-SHELLID
                  MOVE    SPA-GMN-JOBID (IDX)       TO  SPA-KBN-JOBID
                  COMPUTE IDX   =     SPA-GMN-MAX   +   1
              END-IF
           END-PERFORM.
      *
       203-SELNO-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-NAI-MOTOPG      TO  SPA-SAKIPG.
           MOVE    "XB01"              TO  SPA-MOTOPG.
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
           MOVE    SPA-WORK            TO  SPA-FREE.
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       700-SEIWA-HEN-SEC             SECTION.
      *
           IF  WRK-SYMD   =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HENYMDG
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
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC.
      *
           IF  SPA-ERRCD   NOT =   SPACE
               PERFORM 502-ERRSET-SEC
           ELSE
               MOVE    "CURRENT"       TO  MCP-PUTTYPE
               MOVE    "XB01"          TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
           END-IF.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  XB01.
      *
           MOVE    ZERO                TO  XB01-COUNT.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX                >   100)
                           OR (SPA-GMN-NO (IDX)   =   ZERO)
      *
               MOVE    SPA-GMN-NO    (IDX)     TO  WRK1-ZZ
               MOVE    WRK1-ZZ                 TO  XB01-NO       (IDX)
               MOVE    SPA-GMN-JOBNAME   (IDX) TO  XB01-JOBNAME  (IDX)
               MOVE    SPA-GMN-JOBID     (IDX) TO  WRK1-ZZ
               MOVE    WRK1-ZZ                 TO  XB01-JOBID    (IDX)
               MOVE    SPA-GMN-SHELLID   (IDX) TO  XB01-SHELLID  (IDX)
               MOVE    SPA-GMN-STARTYMD  (IDX) TO  XB01-STARTYMD (IDX)
               MOVE    SPA-GMN-STARTTIME (IDX) TO  XB01-STARTTIME(IDX)
               MOVE    SPA-GMN-STEPSTARTTIME (IDX)
                                     TO  XB01-STEPSTARTTIME (IDX)
               MOVE    SPA-GMN-ENDYMD    (IDX) TO  XB01-ENDYMD   (IDX)
               MOVE    SPA-GMN-ENDTIME   (IDX) TO  XB01-ENDTIME  (IDX)
               MOVE    SPA-GMN-STEPENDTIME   (IDX)
                                     TO  XB01-STEPENDTIME   (IDX)
               MOVE    SPA-GMN-ERRCD (IDX)     TO  XB01-ERRCD    (IDX)
      *
               IF  (SPA-GMN-SHELLID (IDX)      =  SPA-KBN-SHELLID)  AND
                   (SPA-GMN-SHELLID (IDX)  NOT =  SPACE)
                   MOVE    "T"                  TO  XB01-SELECT (IDX)
               ELSE
                   MOVE    "F"                  TO  XB01-SELECT (IDX)
               END-IF
      *
               MOVE    IDX                     TO  XB01-COUNT
           END-PERFORM.
      *
           PERFORM 5011-MAPCUR-SEC.
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5011-MAPCUR-SEC             SECTION.
      *
           MOVE    "SELNO"         TO  MCP-WIDGET.
      *
       5011-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       502-ERRSET-SEC              SECTION.
      *
            EVALUATE    SPA-ERRCD
                WHEN    "0001"
                    MOVE    "入力エラー"
                                                TO  SPA-ERRMSG
                WHEN    "0002"
                    MOVE    "選択番号が有りません"
                                                TO  SPA-ERRMSG
                WHEN    "0003"
                    MOVE    "対象が１００件以上あります"
                                                TO  SPA-ERRMSG
                WHEN    "0004"
                    MOVE    "処理が実行中の為、削除できません"
                                                TO  SPA-ERRMSG
                WHEN    OTHER
                    MOVE    SPACE               TO  SPA-ERRMSG
            END-EVALUATE.
      *
           MOVE    SPACE                TO  XBER.
           MOVE    SPA-ERRCD            TO  XBER-ERCODE.
           MOVE    SPA-ERRMSG           TO  XBER-ERMSG.
           MOVE    "B01"                TO  MCP-WIDGET.
           MOVE    "XB01"               TO  SPA-MOTOPG.
           MOVE    "XBER"               TO  SPA-SAKIPG.
           MOVE    "NEW"                TO  MCP-PUTTYPE.
           MOVE    "XBER"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                    TO  FLG-END.              
      *
       502-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       810-JOBKANRI-READ-SEC      SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA

      *
           IF  MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO  JOBKANRI-REC
               MOVE    ZERO            TO  FLG-JOBKANRI
           ELSE
               INITIALIZE                  JOBKANRI-REC
               MOVE    1               TO  FLG-JOBKANRI
           END-IF.
      *
       810-JOBKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"                   TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
