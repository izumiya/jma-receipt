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
       PROGRAM-ID.             ORCR0830.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 診療実日数編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/17    NaCl−門脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04.01.21  労災の特定器材(09593)対応
      *  01.00.02    NACL-門脇    04.01.21  労災・自賠責の入院室料加算
      *                                     (09594)対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC.
           COPY    "CPRCF082.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE82//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE82                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-NYURYAKUSHO                         PIC 9(01).
           03  FLG-KYUKYU                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-MOD                                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX1                                    PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDY2                                    PIC 9(03).
           03  ID1                                     PIC 9(03).
           03  SRYACT-IDX                              PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
      *
      *    実日数カウント用領域
       01  TBL-JNISSU.
           03  TBL-JNISSU-DAY                          PIC X(01)
                                                       OCCURS   31.
      *    合計点数（診療行為）集計用領域
       01  TOTAL-ACCT.
           03  TOTALTEN-ACCT                           PIC S9(08).
           03  TOTALKIN-ACCT                           PIC  9(08).
           03  TOTALKIN2-ACCT                          PIC  9(08).
      *
      *    手技点・薬剤等点数用領域
       01  TBL-TENSUG-JI.
           03  TBL-TENSU-JI                            OCCURS   31.
              05  TBL-SYUTEN-JI                        PIC  9(09).
              05  TBL-YKZTEN-JI                        PIC  9(09).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY41                           PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
      *
           03  WRK-SYS-SRYYMD                          PIC  X(08).
      *
           03  WRK-TAIHI-FLG                           PIC  9(01).
           03  WRK-TAIHI-SRYYMD.
               05  WRK-TAIHI-SRYYM                     PIC  9(06).
               05  WRK-TAIHI-SRYD                      PIC  9(02).
      *
           03  WRK-TEN                                 PIC  9(09).
           03  WRK-KIN                                 PIC  9(09).
           03  WRK-KIN2                                PIC  9(09).
      *
           03  WRK-SINDAN-TUU                          PIC  9(02).
           03  WRK-SINDAN-KIN                          PIC  9(09).
           03  WRK-MEISAI-TUU                          PIC  9(02).
           03  WRK-MEISAI-KIN                          PIC  9(09).
      *
           03  WRK-KSNRATE                             PIC  9V99.
           03  WRK-TAXKBN                              PIC  X(01).
           03  WRK-TANKA                               PIC  9(02)V99.
      *    消費税端数区分
           03  WRK-TAX1001-KBN                         PIC  X(01).
      *
           03  WRK-ZAIKAISU-JI                         PIC  9(03).
           03  WRK-KEISAN-JI                           PIC  9(09).
           03  WRK-TEN-JI                              PIC  9(09).
      *
           03  WRK-KOYOMI-YMD                          PIC  9(08).
           03  WRK-KOYOMI2-YMD.
               05  WRK-KOYOMI2-YYMM                    PIC  9(06).
               05  WRK-KOYOMI2-DD                      PIC  9(02).
      *
           03  WRK-SHOKUJIRYOYO                        PIC  9(01).
           03  WRK-TOKUKANRIKASAN                      PIC  9(01).
           03  WRK-SRYCD                               PIC  X(09).
           03  WRK-NYUTOKRYO                           PIC  9(09).
      *
       01  WRK-TENTEK.
           03  WRK-TENTEK-SYUTEN                       PIC  9(07).
           03  WRK-TENTEK-ZAIKAISU                     PIC  9(04).
           03  WRK-TENTEK-YKZTEN                       PIC  9(07).
           03  WRK-TENTEK-YKZTEN2                      PIC  9(07)
                                                       OCCURS   31.
           03  WRK-TENTEK-KIZAITEN                     PIC  9(07).
           03  WRK-TENTEK-KIZAITEN2                    PIC  9(07)
                                                       OCCURS   31.
           03  WRK-TENTEK-FLG                          PIC  9(01).
           03  WRK-TENTEK-FLG2                         PIC  9(01).
      *
       01  WRK-CONS-AREA.
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(64)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ".
      *
           03  WRK-RECEERR                             PIC X(200).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  10.
               05  TBL-SYS1001                         PIC X(610).
      *    労災自賠医療機関退避用領域
       01  TBL-SYS4001-T.
           03  TBL-SYS4001-OCC     OCCURS  10.
               05  TBL-SYS4001                         PIC X(610).
      *    医療機関（入院基本）退避用領域
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC     OCCURS  10.
               05  TBL-SYS5000                         PIC X(610).
      *
      *---------------------------------------------------------------
      *    固定値
       01  CONST-AREA.
           03  CONS-ROUSAI1    PIC X(09)   VALUE   "101110010".
           03  CONS-ROUSAI2    PIC X(09)   VALUE   "101110020".
           03  CONS-ROUSAI3    PIC X(09)   VALUE   "101130010".
           03  CONS-ROUSAI4    PIC X(09)   VALUE   "101130020".
           03  CONS-ROUSAI5    PIC X(09)   VALUE   "101130030".
           03  CONS-ROUSAI6    PIC X(09)   VALUE   "101130040".
           03  CONS-ROUSAI7    PIC X(09)   VALUE   "101130050".
           03  CONS-ROUSAI8    PIC X(09)   VALUE   "101130060".
           03  CONS-ROUSAI9    PIC X(09)   VALUE   "101130070".
           03  CONS-ROUSAI10   PIC X(09)   VALUE   "101130080".
           03  CONS-ROUSAI11   PIC X(09)   VALUE   "101130090".
           03  CONS-ROUSAI12   PIC X(09)   VALUE   "101130100".
      *
           03  CONS-ROUSAI13   PIC X(09)   VALUE   "101910010".
           03  CONS-ROUSAI14   PIC X(09)   VALUE   "101910020".
           03  CONS-ROUSAI15   PIC X(09)   VALUE   "101910030".
           03  CONS-ROUSAI16   PIC X(09)   VALUE   "101910040".
           03  CONS-ROUSAI17   PIC X(09)   VALUE   "101910070".
           03  CONS-ROUSAI18   PIC X(09)   VALUE   "101910080".
           03  CONS-ROUSAI19   PIC X(09)   VALUE   "101910090".
           03  CONS-ROUSAI20   PIC X(09)   VALUE   "101910100".
      *
      *    腰部固定帯加算
           03  CONS-YOUBUKASAN PIC X(09)   VALUE   "140037490".
      *    療養担当手当（入院）
           03  CONS-RYOYOTEATE PIC X(09)   VALUE   "199000510".
      *---------------------------------------------------------------
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本
           COPY    "CPSK1001.INC".
      *    労災自賠医療機関情報
           COPY    "CPSK4001.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    入院略称
       01  NYURYAKUSHO-REC.
           COPY    "CPNYUINRYAKUSHO.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    入院点滴一括更新サブ
           COPY    "CPORCSTENTEK.INC".
      *
      *    労災入院会計作成サブ
           COPY    "CPORCSROUNYUIN.INC".
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
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCR0830"      TO  JOB-PGID
           MOVE    "診療実日数編集（労災・自賠責）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *
      *      医療機関減算の取得（システム管理テーブルより）
             MOVE     ZERO               TO  WRK-SHOKUJIRYOYO
                                             WRK-TOKUKANRIKASAN
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
             MOVE    "01"                TO  ORC-DBYMD (7:2)
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
             CALL    "ORCMCPSUB"             USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
             IF      MCP-RC              =   ZERO
                 MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
                 PERFORM  SYSKANRI-READ-SEC
      *
                 PERFORM  110-SYSKAN-HENSYU-SEC
      *
             END-IF
      *
             MOVE    "DBCLOSE"               TO  MCP-FUNC
             MOVE    "SYSKANRI-KEY"          TO  ORC-DBPATH
             CALL    "ORCMCPSUB"             USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
      *
           ELSE
      *
             MOVE     ZERO               TO  SYSKAN-IDX
             MOVE     SPACE              TO  TBL-SYS5000-T
             INITIALIZE                      TBL-SYS5000-T
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
             CALL    "ORCMCPSUB"             USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
             IF      MCP-RC              =   ZERO
                 MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                 PERFORM  SYSKANRI-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
      *
                   ADD     1            TO  SYSKAN-IDX
                   MOVE   SYS-5000-REC  TO  TBL-SYS5000(SYSKAN-IDX)
      *
                   MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                   PERFORM  SYSKANRI-READ-SEC
      *
                 END-PERFORM
      *
             END-IF
      *
             MOVE    "DBCLOSE"               TO  MCP-FUNC
             MOVE    "SYSKANRI-KEY8"         TO  ORC-DBPATH
             CALL    "ORCMCPSUB"             USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
      *
           END-IF
      *
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関情報−基本
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
              MOVE    "01"                TO  ORC-DBYMD (7:2)
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
                  PERFORM  SYSKANRI-READ-SEC
                  IF      FLG-SYSKANRI        =   ZERO
                     MOVE     SYS-1001-TAX-HASU-KBN
                                             TO  WRK-TAX1001-KBN
                  ELSE
                     MOVE    "医療機関情報が取得できませんでした。"
                                             TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                     MOVE    1               TO  FLG-END
                  END-IF
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                             TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1                  TO  FLG-END
              END-IF
      *
              MOVE    "DBCLOSE"           TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                  PERFORM  SYSKANRI-READ-SEC
                  PERFORM  UNTIL   FLG-SYSKANRI  =    1
      *
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                    PERFORM  SYSKANRI-READ-SEC
      *
                  END-PERFORM
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
              END-IF
      *
              MOVE    "DBCLOSE"           TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"          USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       システム管理マスタ（４００１）から点数単価の取得
              INITIALIZE                         SYS-4001-REC
              MOVE    "4001"               TO    SYS-4001-KANRICD
              MOVE    "*"                  TO    SYS-4001-KBNCD
              MOVE    WRK-PARA-SRYYM       TO    ORC-DBYMD (1:6)
              MOVE    "01"                 TO    ORC-DBYMD (7:2)
              MOVE    SYS-4001-REC         TO    MCPDATA-REC
              MOVE    "DBSELECT"           TO    MCP-FUNC
              MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH
              CALL    "ORCMCPSUB"                USING
                                                 MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "SYSKANRI-KEY"   TO    ORC-DBPATH
                  PERFORM  SYSKANRI-READ-SEC
                  IF      FLG-SYSKANRI        =   ZERO
                     EVALUATE  SYS-4001-TENKBN
                         WHEN  "1"
                             MOVE     11.5    TO    WRK-TANKA
                         WHEN  "2"
                             MOVE     12.0    TO    WRK-TANKA
                     END-EVALUATE
                     MOVE   SYS-4001-TAXKBN   TO    WRK-TAXKBN
                  ELSE
                     MOVE
                       "労災自賠医療機関情報が取得できませんでした"     
                                              TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                     MOVE    1                TO  FLG-END
                  END-IF
              ELSE
                     MOVE
                       "労災自賠医療機関情報が取得できませんでした"     
                                              TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                     MOVE    1                TO  FLG-END
              END-IF
      *
              MOVE    "DBCLOSE"            TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
              CALL    "ORCMCPSUB"              USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS4001-T
              INITIALIZE                      TBL-SYS4001-T
      *
              INITIALIZE                      SYS-4001-REC
              MOVE    "4001"              TO  SYS-4001-KANRICD
              MOVE    "*"                 TO  SYS-4001-KBNCD
              MOVE    SYS-4001-REC        TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                  PERFORM SYSKANRI-READ-SEC
                  PERFORM  UNTIL   FLG-SYSKANRI  =    1
      *
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-4001-REC  TO  TBL-SYS4001(SYSKAN-IDX)
      *
                    MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                    PERFORM SYSKANRI-READ-SEC
      *
                  END-PERFORM
              ELSE
                  MOVE    "労災自賠医療機関情報が取得できませんでした"     
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1                   TO  FLG-END
              END-IF
      *
              MOVE    "DBCLOSE"           TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"          USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-IF
      *
      *
           OPEN    I-O                 RECE82-FILE
      *
           PERFORM    RECE82-READ-SEC
      *
          .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    シスカンの編集
      *****************************************************************
       110-SYSKAN-HENSYU-SEC                SECTION.
      *
           IF  SYS-5000-SHOKUJIRYOYOCD   =  "197000110"
                  MOVE     1         TO   WRK-SHOKUJIRYOYO
                  IF  SYS-5000-SHOKUKBN(1) =  "1"
                       MOVE  ZERO    TO   WRK-TOKUKANRIKASAN
                  ELSE
                       MOVE  1       TO   WRK-TOKUKANRIKASAN
                  END-IF
           ELSE
                  MOVE     2         TO   WRK-SHOKUJIRYOYO
           END-IF
      *
           .
       110-SYSKAN-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM    210-HENSYU-SEC
      *
           PERFORM    RECE82-READ-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           MOVE       SPACE             TO     TBL-JNISSU
           INITIALIZE                          TBL-JNISSU
      *
           MOVE       SPACE             TO     TBL-TENSUG-JI
           INITIALIZE                          TBL-TENSUG-JI
      *
           MOVE       SPACE             TO     TOTAL-ACCT
           INITIALIZE                          TOTAL-ACCT
      *
           MOVE       SPACE             TO     WRK-DAY-TABLE
           INITIALIZE                          WRK-DAY-TABLE
      *
           MOVE       ZERO              TO     WRK-TAIHI-FLG
      *
           MOVE       ZERO              TO     WRK-SINDAN-TUU
                                               WRK-SINDAN-KIN
                                               WRK-MEISAI-TUU
                                               WRK-MEISAI-KIN
      *
           PERFORM     220-RECE82-HENSYU-SEC
           PERFORM     230-RECE82-HENSYU2-SEC
           PERFORM     250-RECE82-HENSYU4-SEC
           PERFORM     260-RECE82-HENSYU9-SEC
      *
           PERFORM     270-JNISSU-SET-SEC
      *
           PERFORM     RECE82-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       220-RECE82-HENSYU-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE82-HOSPID         TO      NACCT-HOSPID
           MOVE    RECE82-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE82-PTID           TO      NACCT-PTID
           MOVE    RECE82-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "DBSELECT"            TO      MCP-FUNC
           MOVE    "NYUINACCT-KEY7"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "NYUINACCT-KEY7"  TO      ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  OR  "2"  OR
                                                "4"  OR  "5"
                       PERFORM    2201-NYUIN-DAY-TAIHI-SEC
                 END-IF
      *
                 MOVE          "NYUINACCT-KEY7"  TO   ORC-DBPATH
                 PERFORM        NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY7"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                          UNTIL   IDY     >       31
      *
                 IF        WRK-DAY5(IDY)   =   RECE82-HKNCOMBI
                      MOVE    ZERO            TO    FLG-SHOSAISIN
                      IF   WRK-DAY1(IDY)   NOT =    ZERO    
                               MOVE    1      TO    FLG-SHOSAISIN
                      END-IF
                      PERFORM   2201-RECE82-TENSOU11-SEC
                 END-IF
      *
           END-PERFORM
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE82-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE82-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE82-PTID          TO      ACCT-PTID
           MOVE    RECE82-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE82-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY22"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY22"  TO      ORC-DBPATH
               PERFORM  SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)
                        MOVE    ZERO     TO    FLG-SHOSAISIN
                                               FLG-KYUKYU
                        IF   ACCT-SRYKBN  =  "11"  OR
                                             "13"  OR  "14"
                            PERFORM   2201-SHOSAISIN-HANTEI-SEC
                        END-IF
                        IF   ACCT-SRYKBN  =  "11"   
                            PERFORM   2201-KYUKYU-HANTEI-SEC
                        END-IF
                        PERFORM   2201-RECE82-TENSOU12-SEC
                   END-IF
                   IF   (ACCT-ZAITEN    =        ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)
                        MOVE    ZERO     TO    FLG-SHOSAISIN
                                               FLG-KYUKYU
                        IF   ACCT-SRYKBN  =  "11"  OR  "99" 
                            PERFORM   2201-SHOSAISIN-HANTEI-SEC
                        END-IF
                        PERFORM   2201-RECE82-TENSOU12-SEC
                   END-IF
                   MOVE     "SRYACCT-KEY22"  TO   ORC-DBPATH
                   PERFORM   SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACCT-KEY22"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       220-RECE82-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    会計フラグ退避処理                                          *
      ******************************************************************
       2201-NYUIN-DAY-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO 
      *
               IF     NACCT-ZAISKBKBN    =    "1"  
                  IF     NACCT-SRYCDTOTAL    =    099999916
                    CONTINUE
                  ELSE
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY1(IDY) 
                  END-IF
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "2"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY2(IDY) 
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "4"  
                 IF    NACCT-SRYCDTOTAL  =    197000570 
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY41(IDY) 
                 ELSE 
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY4(IDY) 
                 END-IF
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "5"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY5(IDY) 
               END-IF 
      *
             END-IF
      *
           END-PERFORM.
      *
       2201-NYUIN-DAY-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2201-RECE82-TENSOU11-SEC                 SECTION.
      *
           IF   FLG-SHOSAISIN        =       1
                       MOVE    "1"   TO   TBL-JNISSU-DAY(IDY)
           END-IF
           .
       2201-RECE82-TENSOU11-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    診療実日数のカウントを行える剤であるかの判定
      *****************************************************************
       2201-SHOSAISIN-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY9"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY9"    TO      ORC-DBPATH
               PERFORM  SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)  OR
                                      (FLG-SHOSAISIN     =    1)
                       MOVE    SPACE         TO   TNS-TENSU-REC
                       INITIALIZE                 TNS-TENSU-REC
                       MOVE    SRY-SRYCD(SRYACT-IDX) TO   TNS-SRYCD
                       MOVE    ACCT-SRYYM         TO  ORC-DBYMD(1:6)
                       MOVE    "01"               TO  ORC-DBYMD(7:2)
                       PERFORM  TENSU-READ-SEC
                       IF       FLG-TENSU      =    ZERO
                         IF ( (TNS-JITUDAY    =     2)    AND
                              (TNS-DAYCNT     =     1   OR   2) ) OR
                            (  SRY-SRYCD(SRYACT-IDX) =  "099409901" OR
                                                        "099999902" OR
                                                        "099110001" OR
                                                        "099120001")
                             MOVE    1      TO    FLG-SHOSAISIN
                         END-IF
                       END-IF
                       MOVE    "DBCLOSE"          TO  MCP-FUNC
                       MOVE    "TENSU-KEY"        TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"        USING
                                                  MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
                   END-PERFORM
                   MOVE     "SRYACT-KEY9"    TO   ORC-DBPATH
                   PERFORM   SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACT-KEY9"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
      *
       2201-SHOSAISIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    救急管理加算があるかの判定
      *****************************************************************
       2201-KYUKYU-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY9"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY9"    TO      ORC-DBPATH
               PERFORM  SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)  OR
                                      (FLG-KYUKYU        =    1)
                       IF  SRY-SRYCD(SRYACT-IDX) =  CONS-ROUSAI2
                             MOVE    1      TO    FLG-KYUKYU
                       END-IF
                   END-PERFORM
      *
                   MOVE     "SRYACT-KEY9"    TO   ORC-DBPATH
                   PERFORM   SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACT-KEY9"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           .
      *
       2201-KYUKYU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2201-RECE82-TENSOU12-SEC                 SECTION.
      *
           IF   FLG-SHOSAISIN        =       1
              PERFORM    VARYING   IDY   FROM   1    BY   1
                           UNTIL    IDY   >      31
                  IF  ACCT-DAY(1 IDY)   NOT =    ZERO
                       MOVE   "1"  TO  TBL-JNISSU-DAY(IDY)
                  END-IF
              END-PERFORM
           END-IF
           IF   FLG-KYUKYU           =       1
              PERFORM    VARYING   IDY   FROM   1    BY   1
                     UNTIL  ( IDY                  >    31 )  OR
                            ( WRK-TAIHI-FLG        =     1 )
                  IF  ACCT-DAY(1 IDY)   NOT =    ZERO
                      MOVE   RECE82-SRYYM  TO  WRK-TAIHI-SRYYM
                      MOVE   IDY           TO  WRK-TAIHI-SRYD
                      MOVE   1             TO  WRK-TAIHI-FLG
                  END-IF
              END-PERFORM
           END-IF
           .
       2201-RECE82-TENSOU12-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       230-RECE82-HENSYU2-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-KOYOMI-YMD
                                           WRK-KOYOMI2-YMD
      *
      *    初回入院年月日から２週間目を算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    RECE82-NYUINYMD     TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    14                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      *
           MOVE    LNK-DAY6-KOYOMI     TO  WRK-KOYOMI-YMD
      *
      *
           INITIALIZE                      ORCSROUNYUIN-AREA
           MOVE    RECE82-HOSPID       TO  LNK-SROUNYUIN-HOSPID
           MOVE    RECE82-PTID         TO  LNK-SROUNYUIN-PTID
           MOVE    RECE82-SRYYM        TO  LNK-SROUNYUIN-SANTEIYM
           MOVE    RECE82-HKNCOMBI     TO  LNK-SROUNYUIN-HKNCOMBI
           MOVE    "1"                 TO  LNK-SROUNYUIN-RYAKUKBN
      *    労災乗数区分セット
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
      *
              MOVE    RECE82-SRYYM     TO  WRK-KOYOMI2-YYMM
              MOVE    IDY              TO  WRK-KOYOMI2-DD
              IF      WRK-KOYOMI2-YMD  <=  WRK-KOYOMI-YMD
                   MOVE    "1"    TO   LNK-SROUNYUIN-JOUSU-KBN(IDY)
              ELSE
                   MOVE    "2"    TO   LNK-SROUNYUIN-JOUSU-KBN(IDY)
              END-IF
           END-PERFORM
      *
           CALL    "ORCSROUNYUIN"      USING   MCPAREA
                                               ORCSROUNYUIN-AREA
      *
           IF   LNK-SROUNYUIN-RC   =   ZERO
      *
              PERFORM    VARYING   IDY   FROM   1   BY   1
                          UNTIL    IDY    >    31
      *
                IF   LNK-SROUNYUIN-NYUIN-FLG(IDY)    =  "1"
      *
                     PERFORM   2301-KHNTEN-HENSYU-SEC
      *
                END-IF
      *
              END-PERFORM
      *
              PERFORM   2301-RYAKUSYO-HENSYU-SEC
      *
           END-IF
      *
           .
       230-RECE82-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料と外泊等の編集
      **************************************************************
       2301-KHNTEN-HENSYU-SEC           SECTION.
      *
           EVALUATE   LNK-SROUNYUIN-SRYKBN(IDY)
             WHEN   "90"
                  MOVE     ZERO     TO    FLG-ARI
                  PERFORM  VARYING  ID1    FROM    1   BY  1
                                UNTIL   ID1                   > 10
                                  OR    RECE82-NYUKHNNISSU(ID1)
                                                              = ZERO
                                  OR    FLG-ARI               = 1
                      IF  RECE82-NYUKHNRYO(ID1)  =  
                                          LNK-SROUNYUIN-ZAITEN(IDY)
                            ADD   1   TO   RECE82-NYUKHNNISSU(ID1)
                            ADD   LNK-SROUNYUIN-ZAITEN(IDY)
                                      TO   RECE82-NYUKHNKEI(ID1)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                  END-PERFORM  
                  IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE82-NYUKHNNISSU(ID1)
                      MOVE   LNK-SROUNYUIN-ZAITEN(IDY)
                                             TO  RECE82-NYUKHNRYO(ID1)
                      ADD    LNK-SROUNYUIN-ZAITEN(IDY)
                                       TO  RECE82-NYUKHNKEI(ID1)
                  END-IF
      *
                  COMPUTE  RECE82-NYUKHNKEI2  =
                           RECE82-NYUKHNKEI2  +  
                           LNK-SROUNYUIN-ZAITEN-NACCT(IDY)
      *
                  COMPUTE  RECE82-NYUKHNKEI2  =
                           RECE82-NYUKHNKEI2  +  
                           LNK-SROUNYUIN-ZAITEN-ACCT(IDY)
      *
                  COMPUTE  RECE82-NYUTOKKEI2  =
                           RECE82-NYUTOKKEI2  +  
                           LNK-SROUNYUIN-ZAITEN-ACCTETC(IDY)
      *
             WHEN   "92"
                  COMPUTE  WRK-NYUTOKRYO  =  
                           LNK-SROUNYUIN-ZAITEN(IDY)  +
                           LNK-SROUNYUIN-ZAITEN-NACCT(IDY) +
                           LNK-SROUNYUIN-ZAITEN-ACCT(IDY)
      *
                  IF     LNK-SROUNYUIN-GAIHAKU(IDY) =  ZERO
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 10    )  
                          OR  ( RECE82-NYUTOKNISSU(ID1)      = ZERO  )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE82-NYUTOKRYO-RYAKU(ID1)  =  
                                      LNK-SROUNYUIN-RYAKUSYO(IDY)
                            ADD   1   TO   RECE82-NYUTOKNISSU(ID1)
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE82-NYUTOKKEI(ID1)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE82-NYUTOKNISSU(ID1)
                      MOVE   LNK-SROUNYUIN-RYAKUSYO(IDY)
                                             TO  RECE82-NYUTOKRYO-RYAKU
                                                                   (ID1)
      ***             略称が空白の場合の為の処置
                      MOVE   99999999        TO  RECE82-NYUTOKRYO(ID1)
      *
                      ADD    WRK-NYUTOKRYO   TO  RECE82-NYUTOKKEI(ID1)
                    END-IF
                  ELSE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 10    )  
                          OR  ( RECE82-NYUTOKNISSU(ID1)      = ZERO  )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE82-NYUTOKRYO(ID1)  =  
                                            WRK-NYUTOKRYO
                            ADD   1   TO   RECE82-NYUTOKNISSU(ID1)
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE82-NYUTOKKEI(ID1)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE82-NYUTOKNISSU(ID1)
      ***             点数がゼロの場合の為の処置
                      MOVE   "９９９９９"    TO  RECE82-NYUTOKRYO-RYAKU
                                                                   (ID1)
      *
                      MOVE   WRK-NYUTOKRYO   TO  RECE82-NYUTOKRYO(ID1)
                      ADD    WRK-NYUTOKRYO   TO  RECE82-NYUTOKKEI(ID1)
                    END-IF
                  END-IF
      *
                  COMPUTE  RECE82-NYUTOKKEI2  =
                           RECE82-NYUTOKKEI2  +
                           LNK-SROUNYUIN-ZAITEN-ACCTETC(IDY)
      *
           END-EVALUATE
      *
           .
       2301-KHNTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    略称の編集
      *****************************************************************
       2301-RYAKUSYO-HENSYU-SEC                SECTION.
      *
           PERFORM  VARYING  IDY2    FROM    1   BY  1
                      UNTIL  (IDY2    >   20)
                       OR    (LNK-SROUNYUIN-RYAKUSYO2(IDY2) = SPACE)
      *
              MOVE   LNK-SROUNYUIN-RYAKUSYO2(IDY2)
                                 TO    RECE82-NYUSYUBETU(IDY2)
      *
           END-PERFORM
      *
           .
       2301-RYAKUSYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       250-RECE82-HENSYU4-SEC                SECTION.
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                          UNTIL   IDY     >       31
      *
                 IF  WRK-DAY1(IDY) NOT =  ZERO 
                 AND WRK-DAY5(IDY)  =  RECE82-HKNCOMBI
                      IF   (WRK-DAY4(IDY)   NOT =    ZERO)  
                      AND  (WRK-DAY2(IDY)  =    ZERO  OR  4)
                           PERFORM   2501-NYUINKHN-GET4-SEC
                      END-IF
                 END-IF
      *
           END-PERFORM
      *
           .
       250-RECE82-HENSYU4-EXT.
           EXIT.
      *
      **************************************************************
      *    食事略称の取得
      **************************************************************
       2501-NYUINKHN-GET4-SEC           SECTION.
      *
           EVALUATE   WRK-DAY4(IDY)
             WHEN  1
                EVALUATE   WRK-SHOKUJIRYOYO
                  WHEN  1
                     MOVE    "197000110"  TO  WRK-SRYCD
                     PERFORM  25011-NYUINKHN-SET4-SEC
                     IF    WRK-TOKUKANRIKASAN  =  1
                        MOVE    "197000270"  TO  WRK-SRYCD
                        PERFORM  25011-NYUINKHN-SET4-SEC
                     END-IF
                     IF    WRK-DAY41(IDY)  =  1
                        MOVE    "197000570"  TO  WRK-SRYCD
                        PERFORM  25011-NYUINKHN-SET4-SEC
                     END-IF
                  WHEN  2
                     MOVE    "197000710"  TO  WRK-SRYCD
                     PERFORM  25011-NYUINKHN-SET4-SEC
                END-EVALUATE
             WHEN  2
             WHEN  3
             WHEN  4
                     MOVE    "197000110"  TO  WRK-SRYCD
                     PERFORM  25011-NYUINKHN-SET4-SEC
                     IF    WRK-TOKUKANRIKASAN  =  1
                        MOVE    "197000270"  TO  WRK-SRYCD
                        PERFORM  25011-NYUINKHN-SET4-SEC
                     END-IF
                     IF    WRK-DAY41(IDY)  =  1
                        MOVE    "197000570"  TO  WRK-SRYCD
                        PERFORM  25011-NYUINKHN-SET4-SEC
                     END-IF
           END-EVALUATE
      *
           .
       2501-NYUINKHN-GET4-EXT.
           EXIT.
      *
      **************************************************************
      *    食事略称のセット
      **************************************************************
       25011-NYUINKHN-SET4-SEC           SECTION.
      *
           MOVE    SPACE          TO   NYURYAKUSHO-REC
           INITIALIZE                  NYURYAKUSHO-REC
           MOVE    WRK-SRYCD      TO   NYURYAKUSHO-SRYCD
           MOVE    RECE82-SRYYM   TO   ORC-DBYMD(1:6)
           MOVE    "01"           TO   ORC-DBYMD(7:2)
           PERFORM  NYURYAKUSHO-READ-SEC
           IF       FLG-NYURYAKUSHO    =    ZERO
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 3
                         OR    RECE82-SHOKUJISYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE82-SHOKUJISYUBETU(ID1)  
                               =  NYURYAKUSHO-RYAKUSHO
                        MOVE  1   TO   FLG-ARI
                    END-IF
                END-PERFORM  
                IF  FLG-ARI  =  ZERO  
                    MOVE   NYURYAKUSHO-RYAKUSHO
                             TO   RECE82-SHOKUJISYUBETU(ID1)
                END-IF
           END-IF                  
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "NYUINRYAKUSHO-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       25011-NYUINKHN-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       260-RECE82-HENSYU9-SEC                SECTION.
      *
      *     点滴薬剤（器材）まとめ処理
           PERFORM    2601-TENTEK-MATOME-SEC
      *
           MOVE       ZERO              TO      WRK-TENTEK-FLG
                                                WRK-TENTEK-FLG2
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE82-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE82-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE82-PTID          TO      ACCT-PTID
           MOVE    RECE82-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE82-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY22"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY22"  TO      ORC-DBPATH
               PERFORM  SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)
                       MOVE    ZERO     TO    WRK-TEN
                                              WRK-KIN
                                              WRK-KIN2
                       PERFORM   2601-ENDATE-HANTEI-SEC
                       IF    RECE82-HKNKBN   =   "4"
                         IF   ACCT-SRYKBN  =  "80"
                             PERFORM   2601-SINDAN-HANTEI-SEC
                         END-IF
      *
                         IF  WRK-TENTEK-FLG2  =   ZERO
                           PERFORM VARYING IDY FROM 1  BY 1
                                       UNTIL    IDY     >    31
                             COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                      TBL-YKZTEN-JI(IDY)  +
                                      WRK-TENTEK-YKZTEN2(IDY) +
                                      WRK-TENTEK-KIZAITEN2(IDY)
                           END-PERFORM
                           MOVE  1        TO   WRK-TENTEK-FLG2
                         END-IF
      *
                         PERFORM VARYING IDY FROM 1  BY 1
                                   UNTIL    IDY     >    31
                           IF  ACCT-DAY(1 IDY)   NOT =  ZERO
                                MOVE  ACCT-DAY(1 IDY)  
                                       TO  WRK-ZAIKAISU-JI
                                PERFORM  2601-SYUGITEN-HANTEI-SEC
                           END-IF
                         END-PERFORM
                       END-IF
                       PERFORM   2601-RECE82-TENSOU9-SEC
                   END-IF
                   MOVE     "SRYACCT-KEY22"  TO   ORC-DBPATH
                   PERFORM   SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACCT-KEY22"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       260-RECE82-HENSYU9-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴注射の薬剤（器材）点数算定
      *****************************************************************
       2601-TENTEK-MATOME-SEC                    SECTION.
      *
           INITIALIZE                  WRK-TENTEK
      *
           INITIALIZE                  ORCSTENTEKAREA
           MOVE    "2"                 TO  ORCSTEN-KBN
           MOVE    RECE82-PTID         TO  ORCSTEN-PTID
           MOVE    RECE82-SRYYM        TO  ORCSTEN-SRYYM
           MOVE    RECE82-HKNCOMBI     TO  ORCSTEN-HKNCOMBI
           MOVE    RECE82-HOSPID       TO  SPA-HOSPID
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *    薬剤点数と器材点数
           PERFORM  VARYING  IDX1    FROM    1   BY  1
                      UNTIL  IDX1     >       31
      *
              IF  ORCSTEN-ZAITEN(IDX1)  NOT =  ZERO
                 ADD  ORCSTEN-ZAITEN(IDX1)  TO  WRK-TENTEK-YKZTEN
                 ADD  ORCSTEN-ZAITEN(IDX1)  TO  WRK-TENTEK-YKZTEN2
                                                             (IDX1)
              END-IF
      *
           END-PERFORM
      *
           PERFORM  VARYING  IDX1    FROM    1   BY  1
                      UNTIL  IDX1     >       50
                       OR    ORCSTEN-KIZ-ZAITEN(IDX1)  =  ZERO
      *
                MOVE    ZERO   TO   WRK-TENTEK-ZAIKAISU
                PERFORM  VARYING  IDX2    FROM    1   BY  1
                      UNTIL   IDX2     >       31
                  ADD    ORCSTEN-KIZ-DAY(IDX1 IDX2)  
                                     TO  WRK-TENTEK-ZAIKAISU
                END-PERFORM
      *
                COMPUTE  WRK-TENTEK-KIZAITEN  =
                         WRK-TENTEK-KIZAITEN  +
                         ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                           WRK-TENTEK-ZAIKAISU ) 
      **
                MOVE    ZERO   TO   WRK-TENTEK-ZAIKAISU
                PERFORM  VARYING  IDX2    FROM    1   BY  1
                      UNTIL   IDX2     >       31
                  ADD    ORCSTEN-KIZ-DAY(IDX1 IDX2)  
                                     TO  WRK-TENTEK-ZAIKAISU
                  COMPUTE  WRK-TENTEK-KIZAITEN2(IDX2)  =
                           WRK-TENTEK-KIZAITEN2(IDX2)  +
                         ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                           WRK-TENTEK-ZAIKAISU ) 
                END-PERFORM
      *
           END-PERFORM
      *
           .
       2601-TENTEK-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    円建て項目等であるかの判定
      *****************************************************************
       2601-ENDATE-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY9"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY9"    TO      ORC-DBPATH
               PERFORM  SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                 PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                           UNTIL    (SRYACT-IDX    >   5)  OR
                                    (SRY-SRYCD(SRYACT-IDX)
                                                       =   SPACE)
      *
                   MOVE    SPACE         TO   TNS-TENSU-REC
                   INITIALIZE                 TNS-TENSU-REC
                   MOVE    SRY-SRYCD(SRYACT-IDX) TO   TNS-SRYCD
                   MOVE    ACCT-SRYYM         TO  ORC-DBYMD(1:6)
                   MOVE    "01"               TO  ORC-DBYMD(7:2)
                   PERFORM  TENSU-READ-SEC
                   IF       FLG-TENSU      =    ZERO
      *
                     IF    RECE82-HKNKBN   =   "4"
                         IF  SRY-SRYCD(SRYACT-IDX) =  CONS-ROUSAI1  OR
                                                      CONS-ROUSAI2
                            ADD    TNS-TEN      TO    WRK-KIN
                         END-IF
                         IF  SRY-SRYCD(SRYACT-IDX) =  CONS-ROUSAI3  OR
                                                      CONS-ROUSAI4  OR
                                                      CONS-ROUSAI5  OR
                                                      CONS-ROUSAI6  OR
                                                      CONS-ROUSAI7  OR
                                                      CONS-ROUSAI8  OR
                                                      CONS-ROUSAI9  OR
                                                      CONS-ROUSAI10 OR
                                                      CONS-ROUSAI11 OR
                                                      CONS-ROUSAI12 OR
                                                      CONS-ROUSAI13 OR
                                                      CONS-ROUSAI14 OR
                                                      CONS-ROUSAI15 OR
                                                      CONS-ROUSAI16 OR
                                                      CONS-ROUSAI17 OR
                                                      CONS-ROUSAI18 OR
                                                      CONS-ROUSAI19 OR
                                                      CONS-ROUSAI20
                            ADD    TNS-TEN      TO    WRK-KIN2
                         END-IF
                         IF  ( ACCT-SRYKBN                =  "80"    ) 
                         AND ( SRY-SRYCD(SRYACT-IDX)(1:5) =  "09593" 
                                                          OR "09594" ) 
                            ADD    TNS-TEN      TO    WRK-KIN2
                         END-IF
      *                 自賠責・療養担当手当用特例処理
      *                （自賠責の場合、１０点を１２０円に読み替える）
      *                （療養担当手当：１月１回の算定）
                         IF   SRY-SRYCD(SRYACT-IDX) =  CONS-RYOYOTEATE
                            ADD    120          TO    WRK-KIN2
                         END-IF
                     ELSE 
                         IF  SRY-SRYCD(SRYACT-IDX) =  CONS-ROUSAI1  OR
                                                      CONS-ROUSAI2  OR
                                                      CONS-ROUSAI3  OR
                                                      CONS-ROUSAI4  OR
                                                      CONS-ROUSAI5  OR
                                                      CONS-ROUSAI6  OR
                                                      CONS-ROUSAI7  OR
                                                      CONS-ROUSAI8  OR
                                                      CONS-ROUSAI9  OR
                                                      CONS-ROUSAI10 OR
                                                      CONS-ROUSAI11 OR
                                                      CONS-ROUSAI12 OR
                                                      CONS-ROUSAI13 OR
                                                      CONS-ROUSAI14 OR
                                                      CONS-ROUSAI15 OR
                                                      CONS-ROUSAI16 OR
                                                      CONS-ROUSAI17 OR
                                                      CONS-ROUSAI18 OR
                                                      CONS-ROUSAI19 OR
                                                      CONS-ROUSAI20
                            ADD    TNS-TEN      TO    WRK-KIN
                            COMPUTE  WRK-TEN  =   WRK-TEN   +
                                     ( TNS-TEN / 10)
                         END-IF
                         IF  ( ACCT-SRYKBN                =  "80"    ) 
                         AND ( SRY-SRYCD(SRYACT-IDX)(1:5) =  "09593" 
                                                          OR "09594" ) 
                            ADD    TNS-TEN      TO    WRK-KIN
                            COMPUTE  WRK-TEN  =   WRK-TEN   +
                                     ( TNS-TEN / 10)
                         END-IF
                     END-IF
      *
                   END-IF
      *
                   MOVE    "DBCLOSE"          TO  MCP-FUNC
                   MOVE    "TENSU-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"        USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
      *
                 END-PERFORM
      *
                 MOVE     "SRYACT-KEY9"    TO   ORC-DBPATH
                 PERFORM   SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACT-KEY9"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           .
      *
       2601-ENDATE-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    自賠責の診断書料・明細書料であるかの判定
      *****************************************************************
       2601-SINDAN-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY9"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY9"    TO      ORC-DBPATH
               PERFORM  SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)
      *
                     MOVE    SPACE         TO   TNS-TENSU-REC
                     INITIALIZE                 TNS-TENSU-REC
                     MOVE    SRY-SRYCD(SRYACT-IDX) TO   TNS-SRYCD
                     MOVE    ACCT-SRYYM         TO  ORC-DBYMD(1:6)
                     MOVE    "01"               TO  ORC-DBYMD(7:2)
                     PERFORM  TENSU-READ-SEC
                     IF       FLG-TENSU      =    ZERO
      *
                        IF   SRY-SRYCD(SRYACT-IDX)(1:5) =  "09591"
      *
                          COMPUTE  WRK-SINDAN-TUU = WRK-SINDAN-TUU +
                                                    ACCT-ZAIKAISU
                          COMPUTE  WRK-SINDAN-KIN = WRK-SINDAN-KIN +
                                       ( TNS-TEN  *  ACCT-ZAIKAISU )         
      *
                        END-IF
      *
                        IF   SRY-SRYCD(SRYACT-IDX)(1:5) =  "09592"
      *
      *
                          COMPUTE  WRK-MEISAI-TUU = WRK-MEISAI-TUU +
                                                    ACCT-ZAIKAISU
                          COMPUTE  WRK-MEISAI-KIN = WRK-MEISAI-KIN +
                                       ( TNS-TEN  *  ACCT-ZAIKAISU )         
      *
                        END-IF
      *
                     END-IF
      *
                     MOVE    "DBCLOSE"          TO  MCP-FUNC
                     MOVE    "TENSU-KEY"        TO  ORC-DBPATH
                     CALL    "ORCMCPSUB"        USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
      *
                   END-PERFORM
      *
                   MOVE     "SRYACT-KEY9"    TO   ORC-DBPATH
                   PERFORM   SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACT-KEY9"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           .
      *
       2601-SINDAN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    日単位の手技点・薬剤点等をセット
      *****************************************************************
       2601-SYUGITEN-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY9"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY9"    TO      ORC-DBPATH
               PERFORM  SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                 IF    SRY-RENNUM     =     1
      *
                   IF    ACCT-SRYKBN       =  "90"  OR  "92"  OR
                                              "95"  OR  "96"  OR  "97"
                         CONTINUE
                   ELSE
                     IF        ACCT-SRYCDTOTAL    =    630010002
                         COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                  TBL-YKZTEN-JI(IDY)  -
                                 ( WRK-ZAIKAISU-JI * ACCT-ZAITEN   )
                     ELSE 
                      IF    ACCT-SRYKBN  =  "33"
                         EVALUATE  SRY-SRYSYUKBN
                            WHEN  "330"
                            WHEN  "331"
                              COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                       TBL-SYUTEN-JI(IDY)  +
                                      (WRK-ZAIKAISU-JI * ACCT-SYUTEN1)
                            WHEN  "340"
                              COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                       TBL-SYUTEN-JI(IDY)  +
                                      (WRK-ZAIKAISU-JI * ACCT-SYUTEN1)
                              COMPUTE  WRK-KEISAN-JI = 
                                       ACCT-KIZAITEN + ACCT-YKZTEN
                              COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                       TBL-YKZTEN-JI(IDY)  +
                                      (WRK-ZAIKAISU-JI * WRK-KEISAN-JI)
                         END-EVALUATE
                      ELSE
                       IF   ACCT-SRYKBN  =  "70"
                         COMPUTE  WRK-KEISAN-JI   =  ACCT-SYUTEN1
                                                  -  ACCT-SYUTEN2
                         COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                  TBL-SYUTEN-JI(IDY)  +
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-JI )
                         COMPUTE  WRK-KEISAN-JI   =  ACCT-SYUTEN2   +
                                                     ACCT-KIZAITEN  +
                                                     ACCT-YKZTEN
                         COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                  TBL-YKZTEN-JI(IDY)  +
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-JI )
                       ELSE
                         COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                  TBL-SYUTEN-JI(IDY)  +
                                 ( WRK-ZAIKAISU-JI * ACCT-SYUTEN1  )
                         COMPUTE  WRK-KEISAN-JI   =  ACCT-KIZAITEN  +
                                                     ACCT-YKZTEN
                         COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                  TBL-YKZTEN-JI(IDY)  +
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-JI )
                       END-IF
                      END-IF
                     END-IF
                   END-IF
      *
                 END-IF
      *
                 PERFORM   VARYING   SRYACT-IDX   FROM   1   BY   1
                         UNTIL    (SRYACT-IDX            >       5)  OR
                                  (SRY-SRYCD(SRYACT-IDX) =   SPACE)  
      *
                     MOVE    SPACE         TO   TNS-TENSU-REC
                     INITIALIZE                 TNS-TENSU-REC
                     MOVE    SRY-SRYCD(SRYACT-IDX) TO   TNS-SRYCD
                     MOVE    ACCT-SRYYM         TO  ORC-DBYMD(1:6)
                     MOVE    "01"               TO  ORC-DBYMD(7:2)
                     PERFORM  TENSU-READ-SEC
                     IF       FLG-TENSU      =    ZERO
      *
                       IF   SRY-SRYCD(SRYACT-IDX) =  CONS-ROUSAI1  OR
                                                     CONS-ROUSAI2  OR
                                                     CONS-ROUSAI3  OR
                                                     CONS-ROUSAI4  OR
                                                     CONS-ROUSAI5  OR
                                                     CONS-ROUSAI6  OR
                                                     CONS-ROUSAI7  OR
                                                     CONS-ROUSAI8  OR
                                                     CONS-ROUSAI9  OR
                                                     CONS-ROUSAI10 OR
                                                     CONS-ROUSAI11 OR
                                                     CONS-ROUSAI12 OR
                                                     CONS-ROUSAI13 OR
                                                     CONS-ROUSAI14 OR
                                                     CONS-ROUSAI15 OR
                                                     CONS-ROUSAI16 OR
                                                     CONS-ROUSAI17 OR
                                                     CONS-ROUSAI18 OR
                                                     CONS-ROUSAI19 OR
                                                     CONS-ROUSAI20
      *
                            COMPUTE  WRK-TEN-JI   =  TNS-TEN / 10
                            COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                     TBL-SYUTEN-JI(IDY)  -
                              ( WRK-ZAIKAISU-JI * WRK-TEN-JI )
      *
                       END-IF
      *
                       IF  ( ACCT-SRYKBN         =  "80"    ) AND
                           ( SRY-SRYCD(SRYACT-IDX)(1:5) =  "09591" 
                                                        OR "09592" 
                                                        OR "09593" 
                                                        OR "09594")
                               COMPUTE  WRK-TEN-JI   =  TNS-TEN / 10
                               COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                        TBL-SYUTEN-JI(IDY)  -
                                 ( WRK-ZAIKAISU-JI * WRK-TEN-JI )
                       END-IF
      *
                       IF   SRY-SRYCD(SRYACT-IDX) =  CONS-YOUBUKASAN
                            COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                     TBL-SYUTEN-JI(IDY)  -
                                    ( WRK-ZAIKAISU-JI * TNS-TEN )
                            COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                     TBL-YKZTEN-JI(IDY)  +
                                    ( WRK-ZAIKAISU-JI * TNS-TEN )
                       END-IF
      *
                       IF   SRY-SRYCD(SRYACT-IDX) =  CONS-RYOYOTEATE
                            COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                     TBL-SYUTEN-JI(IDY)  -
                                    ( WRK-ZAIKAISU-JI * TNS-TEN )
                       END-IF
      *
                     END-IF
      *
                     MOVE    "DBCLOSE"          TO  MCP-FUNC
                     MOVE    "TENSU-KEY"        TO  ORC-DBPATH
                     CALL    "ORCMCPSUB"        USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
      *
                 END-PERFORM
      *
                 MOVE     "SRYACT-KEY9"    TO   ORC-DBPATH
                 PERFORM   SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACT-KEY9"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           .
      *
       2601-SYUGITEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2601-RECE82-TENSOU9-SEC                 SECTION.
      *
           IF        ACCT-SRYKBN   =    "90" OR "92" OR "95"  
                                             OR "96" OR "97"
               CONTINUE
           ELSE
               IF        ACCT-SRYCDTOTAL    =    630010002
                 COMPUTE   TOTALTEN-ACCT =
                           TOTALTEN-ACCT -
                           (ACCT-ZAITEN  *
                            ACCT-ZAIKAISU)
               ELSE
                 IF        WRK-TENTEK-FLG  =   ZERO
                     COMPUTE   TOTALTEN-ACCT        =
                               TOTALTEN-ACCT        +
                               WRK-TENTEK-YKZTEN    +
                               WRK-TENTEK-KIZAITEN
                     MOVE  1        TO   WRK-TENTEK-FLG
                 END-IF
                 IF        ACCT-SRYKBN     =    "33"
                     PERFORM   26011-TENTEK-SYUTEN-SEC
                     COMPUTE   TOTALTEN-ACCT =  
                               TOTALTEN-ACCT +
                               WRK-TENTEK-SYUTEN
                 ELSE
                     COMPUTE   TOTALTEN-ACCT =  
                               TOTALTEN-ACCT +
                               (ACCT-ZAITEN  *  
                                ACCT-ZAIKAISU)
                     IF  ( WRK-KIN  NOT =  ZERO )
                     OR  ( WRK-KIN2 NOT =  ZERO )
                     OR  ( WRK-TEN  NOT =  ZERO )                       
                       COMPUTE  TOTALTEN-ACCT  =  TOTALTEN-ACCT  -
                                                   (WRK-TEN    *
                                                    ACCT-ZAIKAISU)
      *
                       IF    RECE82-HKNKBN   =   "4"
                         COMPUTE  TOTALKIN-ACCT  =  TOTALKIN-ACCT  +
                                                     (WRK-KIN    *
                                                      ACCT-ZAIKAISU)
                         COMPUTE  TOTALKIN2-ACCT =  TOTALKIN2-ACCT +
                                                     (WRK-KIN2   *
                                                      ACCT-ZAIKAISU)

                       ELSE
                         COMPUTE  TOTALKIN-ACCT  =  TOTALKIN-ACCT  +
                                                     (WRK-KIN    *
                                                      ACCT-ZAIKAISU)
                       END-IF
                     END-IF 
                 END-IF
               END-IF
           END-IF
           .
       2601-RECE82-TENSOU9-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴注射の手技点数算定
      *****************************************************************
       26011-TENTEK-SYUTEN-SEC                    SECTION.
      *
      *診療行為テーブルから診療種別区分の取得
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID        TO   SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
           MOVE    ACCT-PTID          TO   SRY-PTID
           MOVE    ACCT-SRYYM         TO   SRY-SRYYM
           MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
           MOVE    SRYACT-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "SRYACT-KEY9"      TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC             =    ZERO
                 MOVE    "SRYACT-KEY9"    TO   ORC-DBPATH
                 PERFORM  SRYACT-READ-SEC
           ELSE
                 MOVE    1                TO   FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                   TO   WRK-TENTEK-SYUTEN
      *
           IF   FLG-SRYACT   =    ZERO
                EVALUATE  SRY-SRYSYUKBN
                    WHEN  "330"
                    WHEN  "331"
                       COMPUTE WRK-TENTEK-SYUTEN = WRK-TENTEK-SYUTEN +
                               (ACCT-SYUTEN1 * ACCT-ZAIKAISU)
                    WHEN  "340"
                       COMPUTE WRK-TENTEK-SYUTEN = WRK-TENTEK-SYUTEN +
                               (ACCT-ZAITEN  * ACCT-ZAIKAISU)
                END-EVALUATE
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACT-KEY9"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       26011-TENTEK-SYUTEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数の項目転送（Ｆ００２）
      *****************************************************************
       270-JNISSU-SET-SEC                    SECTION.
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     31
                IF    TBL-JNISSU-DAY(IDY)  NOT =  SPACE
                          ADD    1      TO    RECE82-JNISSU
                END-IF
           END-PERFORM
      *
           IF       RECE82-JNISSU   =    ZERO  
                     MOVE     999       TO    RECE82-JNISSU
           END-IF
      *
           IF     WRK-TAIHI-FLG       =   1
                MOVE   WRK-TAIHI-FLG       TO  RECE82-KYUKYU-PRTKBN
                MOVE   WRK-TAIHI-SRYYMD    TO  RECE82-KYUKYUYMD
           END-IF
      *
           IF     WRK-SINDAN-TUU      =   ZERO                          
                CONTINUE                                                
           ELSE                                                         
                MOVE   WRK-SINDAN-TUU      TO  RECE82-SINDAN-TUU        
                IF    WRK-TAXKBN      =   "2"                           
                  IF  WRK-TAX1001-KBN =   "1"
                     COMPUTE  WRK-SINDAN-KIN  =  
                             (WRK-SINDAN-KIN  *  1.05)  +  0.5          
                  ELSE
                     COMPUTE  WRK-SINDAN-KIN  =  
                              WRK-SINDAN-KIN  *  1.05                   
                  END-IF
                END-IF                                                  
                MOVE   WRK-SINDAN-KIN      TO  RECE82-SINDAN-KIN        
           END-IF
      *
           IF     WRK-MEISAI-TUU      =   ZERO                          
                CONTINUE                                                
           ELSE                                                         
                MOVE   WRK-MEISAI-TUU      TO  RECE82-MEISAI-TUU        
                IF    WRK-TAXKBN      =   "2"                           
                  IF  WRK-TAX1001-KBN =   "1"
                     COMPUTE  WRK-MEISAI-KIN  =  
                             (WRK-MEISAI-KIN  *  1.05)  +  0.5          
                  ELSE
                     COMPUTE  WRK-MEISAI-KIN  =  
                              WRK-MEISAI-KIN  *  1.05                   
                  END-IF
                END-IF                                                  
                MOVE   WRK-MEISAI-KIN      TO  RECE82-MEISAI-KIN
           END-IF                                                       
      *
           IF    RECE82-HKNKBN   =   "4"
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      10
                    COMPUTE  RECE82-TOTALTEN  = 
                             RECE82-TOTALTEN  + 
                             RECE82-NYUKHNKEI(IDX)
                    COMPUTE  RECE82-TOTALTEN  = 
                             RECE82-TOTALTEN  + 
                             RECE82-NYUTOKKEI(IDX)
              END-PERFORM
      *
              COMPUTE  RECE82-TOTALTEN  = 
                       RECE82-TOTALTEN  + 
                       RECE82-NYUKHNKEI2
              COMPUTE  RECE82-TOTALTEN  = 
                       RECE82-TOTALTEN  + 
                       RECE82-NYUTOKKEI2
      *
              MOVE     RECE82-TOTALTEN   TO   RECE82-NYUTOTALTEN
      *
              PERFORM    VARYING    IDY    FROM    1   BY   1              
                          UNTIL      IDY    >     31                    
      *
                  MOVE  TBL-SYUTEN-JI(IDY)  TO  RECE82-SYUTEN-JI(IDY)     
                  ADD   TBL-SYUTEN-JI(IDY)  TO  RECE82-TOTALTEN
                  MOVE  TBL-YKZTEN-JI(IDY)  TO  RECE82-YKZTEN-JI(IDY)     
                  ADD   TBL-YKZTEN-JI(IDY)  TO  RECE82-TOTALTEN-YAK
      *
              END-PERFORM                                                  
      *
              COMPUTE   WRK-KSNRATE     =  RECE82-KSNRATE  /  100
      *
              COMPUTE   RECE82-TOTALKIN   =  
                  (RECE82-TOTALTEN  *  WRK-TANKA  *  WRK-KSNRATE) 
                                                  +  0.5
      *
              COMPUTE   RECE82-TOTALKIN-YAK =  
                  (RECE82-TOTALTEN-YAK  *  WRK-TANKA)  
                                                  +  0.5
      *
              MOVE     TOTALKIN-ACCT   TO   RECE82-KINGAKU2-SYO
      *
              MOVE     TOTALKIN2-ACCT  TO   RECE82-KINGAKU3-SYO
      *
              COMPUTE  RECE82-TOTALKIN2     =  
                  (RECE82-KINGAKU2-SYO  *  WRK-KSNRATE)  +  0.5  
      *
           ELSE
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      10
                    COMPUTE  RECE82-TOTALTEN  = 
                             RECE82-TOTALTEN  + 
                             RECE82-NYUKHNKEI(IDX)
                    COMPUTE  RECE82-TOTALTEN  = 
                             RECE82-TOTALTEN  + 
                             RECE82-NYUTOKKEI(IDX)
              END-PERFORM
      *
              COMPUTE  RECE82-TOTALTEN  = 
                       RECE82-TOTALTEN  + 
                       RECE82-NYUKHNKEI2
              COMPUTE  RECE82-TOTALTEN  = 
                       RECE82-TOTALTEN  + 
                       RECE82-NYUTOKKEI2
      *
              ADD      TOTALTEN-ACCT    TO   RECE82-TOTALTEN
      *
              COMPUTE  RECE82-TOTALKIN   =   RECE82-TOTALTEN   *
                                             WRK-TANKA    
      *
              MOVE     TOTALKIN-ACCT    TO   RECE82-KINGAKU2-SYO
      *
           END-IF                                                       
      *
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      10
                 IF   RECE82-NYUTOKRYO(IDX)  =  99999999
                    MOVE    ZERO    TO   RECE82-NYUTOKRYO(IDX)
                 END-IF
                 IF   RECE82-NYUTOKRYO-RYAKU(IDX)  
                                             =  "９９９９９"
                    MOVE    SPACE   TO   RECE82-NYUTOKRYO-RYAKU(IDX)
                 END-IF
           END-PERFORM
      *
           .
       270-JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００２）
      *****************************************************************
       RECE82-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE82-REC
      *
           ADD     1                   TO  CNT-MOD
      *
           .
       RECE82-WRITE-EXT.
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
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE82-FILE
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "RECE820 IN  :" CNT-IN
           DISPLAY "        MOD :" CNT-MOD
           DISPLAY "*** ORCR0830 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       SRYACCT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       SRYACT-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    点数マスター読込（主キー）
      *****************************************************************
       TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
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
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       TENSU-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    入院略称マスター読込（主キー）
      *****************************************************************
       NYURYAKUSHO-READ-SEC        SECTION.
      *
           MOVE    NYURYAKUSHO-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINRYAKUSHO-KEY" TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "NYUINRYAKUSHO-KEY" TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  NYURYAKUSHO-REC
                   MOVE    ZERO                TO  FLG-NYURYAKUSHO
               ELSE
                   MOVE    1                   TO  FLG-NYURYAKUSHO
                   INITIALIZE                  NYURYAKUSHO-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-NYURYAKUSHO
               INITIALIZE                  NYURYAKUSHO-REC
           END-IF
      *
           .
       NYURYAKUSHO-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    システム管理マスタ読み込み処理
      **************************************************************
       SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                                           SYS-1001-REC
                                           SYS-4001-REC
                                           SYS-5000-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
                                           SYS-1001-REC
                                           SYS-4001-REC
                                           SYS-5000-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       RECE82-READ-SEC            SECTION.
      *
           READ    RECE82-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
                   IF   WRK-PARA-SRYYM    NOT =  ZERO
                     CONTINUE
                   ELSE
      *              医療機関ＩＤ編集
                     MOVE  ZERO           TO  FLG-SET
                     MOVE  RECE82-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
                     MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
                     PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                           UNTIL  ( SYSKAN-IDX              >  10   )
                            OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                            OR    ( FLG-SET                 =   1   )
      *
                       MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                       IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                       AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                          MOVE     1       TO    FLG-SET
                          MOVE    SYS-1001-TAX-HASU-KBN
                                           TO    WRK-TAX1001-KBN
                       END-IF
      *
                     END-PERFORM
                     IF     FLG-SET  =  ZERO
               STRING  "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       RECE82-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
                         PERFORM 500-ERR-HENSYU-SEC
                         MOVE    1               TO  FLG-END
                     END-IF
      *              労災自賠医療機関情報
                     MOVE  ZERO           TO  FLG-SET
                     MOVE  RECE82-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
                     MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
                     PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                           UNTIL  ( SYSKAN-IDX              >  10   )
                            OR    ( TBL-SYS4001(SYSKAN-IDX) = SPACE )
                            OR    ( FLG-SET                 =   1   )
      *
                       MOVE  TBL-SYS4001(SYSKAN-IDX)  TO  SYS-4001-REC
                       IF   ( SYS-4001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                       AND  ( WRK-SYS-SRYYMD     <=  SYS-4001-EDYUKYMD )
                          MOVE     1       TO    FLG-SET
                          EVALUATE  SYS-4001-TENKBN
                            WHEN  "1"
                               MOVE     11.5       TO    WRK-TANKA
                            WHEN  "2"
                               MOVE     12.0       TO    WRK-TANKA
                          END-EVALUATE
                          MOVE   SYS-4001-TAXKBN   TO    WRK-TAXKBN
                       END-IF
      *
                     END-PERFORM
                     IF     FLG-SET  =  ZERO
             STRING
               "労災自賠医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE 
                        RECE82-SRYYM       DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
             END-STRING
                         PERFORM 500-ERR-HENSYU-SEC
                         MOVE    1               TO  FLG-END
                     END-IF
                   END-IF
      *
                   IF   WRK-PARA-SRYYM    NOT =  ZERO
                      CONTINUE
                   ELSE
      *               医療機関減算の取得（システム管理テーブルより）
                      MOVE     ZERO      TO  WRK-SHOKUJIRYOYO
                                             WRK-TOKUKANRIKASAN
      *
                      MOVE  ZERO         TO  FLG-SET
                      MOVE  RECE82-SRYYM TO  WRK-SYS-SRYYMD(1:6)
                      MOVE  "01"         TO  WRK-SYS-SRYYMD(7:2)
                      PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                           UNTIL  ( SYSKAN-IDX              >  10   )
                            OR    ( TBL-SYS5000(SYSKAN-IDX) = SPACE )
                            OR    ( FLG-SET                 =   1   )
      *
                       MOVE  TBL-SYS5000(SYSKAN-IDX)  TO  SYS-5000-REC
                       IF   ( SYS-5000-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                       AND  ( WRK-SYS-SRYYMD     <=  SYS-5000-EDYUKYMD )
                          MOVE     1       TO    FLG-SET
                          PERFORM  110-SYSKAN-HENSYU-SEC
                       END-IF
      *
                      END-PERFORM
                   END-IF
           END-READ
           .
       RECE82-READ-EXT.
           EXIT.
      *
