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
       PROGRAM-ID.             ORCR0600.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 仮収納データ作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  02/01/07    NACL−門脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.04.02  負担金計算サブ引数修正
      *  01.00.02    NACL-門脇    03.12.15  日別収納更新・削除
      *  01.00.03    NACL-門脇    04.01.14  診療行為の包括分は除く
      *                                     SRYACCT8→29
      *                                     SRYACCT9→30
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/07  中止処理の追加
      *  01.01.02    NACL-門脇    05/01/28  ダミー収納削除処理の修正
      *
      *  02.06.01    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.02    NACL-門脇    05/07/19  ダミー収納削除処理の修正
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *  02.07.02    NACL-藤原    05/07/12  診療会計テーブルの読み込みを
      *                                     グループ化に変更
      *                                     SRYACCT-KEY29 -> SRYACCT-KEY36
      *                                     SRYACCT-KEY30 -> SRYACCT-KEY37
      *  02.07.03    NACL-門脇    05/02/17  診療会計個別処理の編集条件修正
      *  03.03.01    NACL-門脇    06/11/08  個別処理時の項目クリア
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE   ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-SYS2002                             PIC 9(01).
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-PTNYUINRRK                          PIC 9(01).
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-DATA                                PIC 9(01).
           03  FLG-SYORI                               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC 9(04). 
           03  IDX1                                    PIC 9(04). 
           03  SYSKAN-IDX                              PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                          PIC 9(10).
               05  KEY-N-HKNCOMBI                      PIC 9(04).
               05  KEY-N-SRYKA                         PIC X(02).
           03  KEY-OLD.
               05  KEY-O-PTID                          PIC 9(10).
               05  KEY-O-HKNCOMBI                      PIC 9(04).
               05  KEY-O-SRYKA                         PIC X(02).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  10.
               05  TBL-SYS1001                         PIC X(610).
      *
      *    パラメタ領域
       01  WRK-PARA.
           03  WRK-PARA-SRYYM                          PIC X(06).
           03  WRK-PARA-TERMID                         PIC X(16).
           03  WRK-PARA-SYSYMD                         PIC X(08).
           03  WRK-PARA-SYOKBN                         PIC X(01).
           03  WRK-PARA-RECEKBN                        PIC X(01).
           03  WRK-PARA-JOBID                          PIC 9(07).
           03  WRK-PARA-SHELLID                        PIC X(08).
           03  WRK-PARA-PTID                           PIC X(10).
           03  WRK-PARA-SRYYM-O                        PIC X(06).
      *
      *    エラーメッセージ編集領域
       01  ERR-EDIT-AREA.
           03  ERR-PTNUM               PIC X(20).
           03  ERR-ERRMSG              PIC X(80).
           03  ERR-HEN-ERRMSG          PIC X(200).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-RECEERR                             PIC X(200).
      *
           03  WRK-STYMD                               PIC X(08).
           03  WRK-EDYMD                               PIC X(08).
      *
      *    診療回数テーブル（入院会計用　１：基本　２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL                         OCCURS  2.
                   07  WRK-DAY                         PIC 9(04)
                                                       OCCURS   31.
      *
           03  WRK-ROUSAIKBN                           PIC X(01).
           03  WRK-DENPNUM                             PIC 9(07).
           03  WRK-SEIKYUSTYMD                         PIC 9(08).
           03  WRK-SEIKYUEDYMD                         PIC 9(08).
           03  WRK-DATAKBN                             PIC X(01).
           03  WRK-SYOKBN                              PIC X(01).
      *
           03  WRK-STR                                 PIC X(200).
           03  WRK-LEN                                 PIC 9(05).
           03  WRK-SHO                                 PIC 9(05).
           03  WRK-AMARI                               PIC 9(05).
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
       01  WRK-CONS-AREA.
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *    レセプト作成指示コード
           COPY    "CPSK2001.INC".
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *    患者番号構成情報
           COPY    "CPSK1009.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    日別収納マスター
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    定期請求用収納作成サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(2000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL ( FLG-END =   1 )
                                      OR ( FLG-ERR =   1 )
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           MOVE    WRK-PARA-SRYYM      TO  SYS-2001-SRYYM
           MOVE    WRK-PARA-SYOKBN     TO  SYS-2001-SYOKBN 
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    "ORCR0600"      TO  JOB-PGID
               MOVE    "仮収納データ作成"
                                       TO  JOB-SHELLMSG
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        医療機関ＩＤ編集
               INITIALIZE                  SYS-1001-REC
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
               MOVE    "01"                TO  ORC-DBYMD (7:2)
               MOVE    SYS-1001-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1001-REC
               ELSE
                   MOVE    "医療機関情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               IF   WRK-PARA-SRYYM    NOT =  ZERO
                  CONTINUE
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
                      PERFORM 900-SYSKANRI-READ-SEC
                      PERFORM  UNTIL   FLG-SYSKANRI  =    1
                          MOVE    MCPDATA-REC   TO  SYS-1001-REC
                          ADD     1             TO  SYSKAN-IDX
                          MOVE    SYS-1001-REC  TO
                                                TBL-SYS1001(SYSKAN-IDX)
      *
                          MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                          PERFORM 900-SYSKANRI-READ-SEC
                      END-PERFORM
                  ELSE
                      MOVE    "医療機関情報が取得できませんでした。"
                                                TO  WRK-RECEERR
                      PERFORM 500-ERR-HENSYU-SEC
                  END-IF
      *
                  MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
                  MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
                  CALL    "ORCMCPSUB"          USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
               END-IF
      *
      *        患者番号構成管理情報編集
               INITIALIZE                  SYS-1009-REC
               MOVE    "1009"              TO  SYS-1009-KANRICD
               MOVE    "*"                 TO  SYS-1009-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
               MOVE    "01"                TO  ORC-DBYMD (7:2)
               MOVE    SYS-1009-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1009-REC
                   MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
                   MOVE    SYS-1009-HJNKSIKBN
                                           TO  SPA-1009-HJNKSIKBN
                   MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
               ELSE
                   MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        仮収納の削除処理
               PERFORM  1001-SYUNOU-DELETE-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    仮収納の削除処理
      *****************************************************************
       1001-SYUNOU-DELETE-SEC                SECTION.
      *
      *    一括ダミー収納削除処理
      *
      ***  IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *
      ***    伝票番号９００００００〜９０９９９９９削除（以前分）
      ***    INITIALIZE                      SYUNOU-REC
      ***    MOVE    SYUNOU-REC          TO  MCPDATA-REC
      ***    MOVE    "SYUNOU-DEL5"       TO  ORC-DBPATH
      ***    MOVE    "DBDELETE"          TO  MCP-FUNC
      ***    CALL    "ORCMCPSUB"             USING
      ***                                    MCPAREA
      ***                                    ORCMCPAREA
      ***                                    MCPDATA-REC
      ***    IF      MCP-RC          NOT =   ZERO
      ***        MOVE  "仮収納データ削除処理に失敗しました"
      ***                        TO  WRK-RECEERR
      ***        PERFORM 500-ERR-HENSYU-SEC
      ***    END-IF
      *
      *      一括ダミー日別収納削除処理
      *
      ***    IF   FLG-ERR         =   1
      ***       CONTINUE
      ***    ELSE
      ***       伝票番号９００００００〜９０９９９９９削除（以前分）
      ***       INITIALIZE                      SYUDAY-REC
      ***       MOVE    SYUDAY-REC          TO  MCPDATA-REC
      ***       MOVE    "SYUDAY-DEL4"       TO  ORC-DBPATH
      ***       MOVE    "DBDELETE"          TO  MCP-FUNC
      ***       CALL    "ORCMCPSUB"             USING
      ***                                       MCPAREA
      ***                                       ORCMCPAREA
      ***                                       MCPDATA-REC
      ***       IF      MCP-RC          NOT =   ZERO
      ***           MOVE  "仮日別収納データ削除処理に失敗しました"
      ***                           TO  WRK-RECEERR
      ***           PERFORM 500-ERR-HENSYU-SEC
      ***       END-IF
      ***    END-IF
      *
      ***  END-IF
      *
      *    ダミー収納削除処理
      *    ダミー日別収納削除処理
      *
           IF    ( WRK-PARA-SYOKBN   =   "1"  )
           AND   ( FLG-ERR           =   ZERO )
             INITIALIZE                           SYUNOU-REC
             MOVE    SYS-1001-HOSPID      TO      SYU-HOSPID
             MOVE    "1"                  TO      SYU-NYUGAIKBN
             MOVE    WRK-PARA-SRYYM       TO      SYU-SRYYMD(1:6)
             MOVE    "__"                 TO      SYU-SRYYMD(7:2)
             MOVE    SYUNOU-REC           TO      MCPDATA-REC
             MOVE    "DBSELECT"           TO      MCP-FUNC
             MOVE    "SYUNOU-KEY40"       TO      ORC-DBPATH
             CALL    "ORCMCPSUB"          USING
                                          MCPAREA
                                          ORCMCPAREA
                                          MCPDATA-REC
             IF      MCP-RC               =       ZERO
               MOVE      "SYUNOU-KEY40"   TO      ORC-DBPATH
               PERFORM  900-SYUNOU-READ-SEC
               PERFORM  UNTIL
                              (FLG-SYUNOU     =   1)  OR
                              (FLG-ERR        =   1)
      *
                 IF   SYU-SYUHKNNUM   =  WRK-CONS-ROUSAI-HKNNUM
                                     OR  WRK-CONS-JIBAI-HKNNUM
                   CONTINUE
                 ELSE
                   IF      SYU-DAYINFFLG   =   "1"
      *
                      INITIALIZE                      SYUDAY-REC
                      MOVE    SYS-1001-HOSPID     TO  SDAY-HOSPID
                      MOVE    "1"                 TO  SDAY-NYUGAIKBN
                      MOVE    SYU-PTID            TO  SDAY-PTID
                      MOVE    SYU-DENPNUM         TO  SDAY-DENPNUM
                      MOVE    SYUDAY-REC          TO  MCPDATA-REC
                      MOVE    "SYUDAY-DEL5"       TO  ORC-DBPATH
                      MOVE    "DBDELETE"          TO  MCP-FUNC
                      CALL    "ORCMCPSUB"         USING
                                                  MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
                      IF      MCP-RC          NOT =   ZERO
                          MOVE  "仮日別収納データ削除処理に失敗しました"
                                  TO  WRK-RECEERR
                          PERFORM 500-ERR-HENSYU-SEC
                      END-IF
      *
                   END-IF
      *
                   MOVE    SYUNOU-REC       TO      MCPDATA-REC
                   MOVE    "DBDELETE"       TO      MCP-FUNC
                   MOVE    "SYUNOU-KEY"     TO      ORC-DBPATH
                   CALL    "ORCMCPSUB"      USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
                 END-IF
      *
                 MOVE       "SYUNOU-KEY40"  TO   ORC-DBPATH
                 PERFORM     900-SYUNOU-READ-SEC
      *
               END-PERFORM
             ELSE
               MOVE  "仮収納データ削除処理に失敗しました"
                               TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
             END-IF
      *
             MOVE    "SYUNOU-KEY40"  TO  ORC-DBPATH
             MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
             CALL    "ORCMCPSUB"     USING   MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
           END-IF
      *
           .
       1001-SYUNOU-DELETE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF      FLG-ERR             =   ZERO
               EVALUATE    WRK-PARA-SYOKBN
                   WHEN    "1"
                       PERFORM 2001-IKKATU-SYORI-SEC
                   WHEN    "2"
                       PERFORM 2002-KOBETU-SYORI-SEC
               END-EVALUATE
           END-IF    
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    一括処理
      *****************************************************************
       2001-IKKATU-SYORI-SEC               SECTION.
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF      FLG-ERR             =   ZERO
      *        診療会計読み込み（患者ＩＤ、保険組合せ、診療科順）
               INITIALIZE                  SRYACCT-REC
               MOVE    SYS-1001-HOSPID     TO  ACCT-HOSPID
               MOVE    SYS-2001-SRYYM      TO  ACCT-SRYYM
               MOVE    "1"                 TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "SRYACCT-KEY36"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "SRYACCT-KEY36"     TO  ORC-DBPATH
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM UNTIL       FLG-SRYACCT     =   1
                         OR        FLG-ERR         =   1
                   PERFORM 20011-IKKATU-SRYACCT-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               MOVE    "SRYACCT-KEY36"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        入院会計読み込み（患者ＩＤ、診療科、剤識別区分順）
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *
               INITIALIZE                  NYUINACCT-REC
               MOVE    SYS-1001-HOSPID     TO  NACCT-HOSPID
               MOVE    SYS-2001-SRYYM      TO  NACCT-SRYYM
               MOVE    "1"                 TO  NACCT-NYUGAIKBN
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "NYUINACCT-KEY13"   TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "NYUINACCT-KEY13"   TO  ORC-DBPATH
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT   =   1
                         OR        FLG-ERR         =   1
                   PERFORM 20012-IKKATU-NYUINACCT-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
               MOVE    "NYUINACCT-KEY13"    TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           END-IF
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    診療会計一括処理（患者ＩＤ、保険組合せ、診療科順）
      *****************************************************************
       20011-IKKATU-SRYACCT-HENSYU-SEC              SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
           IF      FLG-ERR             =   1
               GO  TO  20011-IKKATU-SRYACCT-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE                   TO  WRK-DATAKBN
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SYS-1001-HOSPID         TO  PTINF-HOSPID
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "SRYACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF    ( FLG-PTINF           =   ZERO )
           AND   ( PTINF-TSTPTNUMKBN   =   "1"  )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-ERR         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "SRYACCT-KEY36"         TO  ORC-DBPATH
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-ERR         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SYS-1001-HOSPID     TO  COMB-HOSPID
                   MOVE    KEY-N-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
                   MOVE    SPACE               TO  WRK-ROUSAIKBN
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       IF      COMB-HKNNUM     =
                                              WRK-CONS-ROUSAI-HKNNUM OR
                                              WRK-CONS-JIBAI-HKNNUM 
                           MOVE    "1"             TO  WRK-ROUSAIKBN
                       END-IF
                   END-IF
      *
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-ERR         =   1
                                   OR      KEY-NEW     NOT =   KEY-OLD
      *                社保・国保の読み飛ばし
                       EVALUATE    WRK-PARA-RECEKBN
                           WHEN    "0"
                               PERFORM 20011-SRYACCT-FILE-HENSYU-SEC 
                           WHEN    "1"
                               IF      COMB-HKNNUM 
                                               NOT =   "060" AND "067"
                                   PERFORM 20011-SRYACCT-FILE-HENSYU-SEC 
                               END-IF
                           WHEN    "2"
                               IF      COMB-HKNNUM =   "060" OR  "067"
                                   PERFORM 20011-SRYACCT-FILE-HENSYU-SEC 
                               END-IF
                       END-EVALUATE
      *
                       IF      FLG-ERR            =   1
                          CONTINUE
                       ELSE
                         IF      WRK-DATAKBN        =   "1"
      *                    仮収納作成のとき患者ＩＤで読み飛ばす
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM UNTIL   FLG-SRYACCT =   1 
                                   OR      FLG-ERR     =   1
                                   OR      KEY-N-PTID  
                                                   NOT =   KEY-O-PTID
                               MOVE    "SRYACCT-KEY36" TO  ORC-DBPATH
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                         ELSE
      *                    同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1 
                                       OR      FLG-ERR     =   1
                                       OR      KEY-NEW NOT =   KEY-OLD
                               MOVE    "SRYACCT-KEY36" TO  ORC-DBPATH
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                         END-IF    
                       END-IF
                   END-PERFORM    
               END-PERFORM         
           END-IF    
           .
       20011-IKKATU-SRYACCT-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    仮収納作成処理（診療会計）
      *****************************************************************
       20011-SRYACCT-FILE-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-DATAKBN
      *
      *    労災保険・自賠責のとき処理しない
           IF      WRK-ROUSAIKBN       =   "1"
               GO  TO  20011-SRYACCT-FILE-HENSYU-EXT
           END-IF
      *
      *    仮収納作成 
           PERFORM 20011-HENSYU-SEC
      *
           .
       20011-SRYACCT-FILE-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計一括編集処理
      *****************************************************************
       20012-IKKATU-NYUINACCT-HENSYU-SEC              SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
           IF      FLG-ERR             =   1
               GO  TO  20012-IKKATU-NYUINACCT-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE                   TO  WRK-DATAKBN
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SYS-1001-HOSPID         TO  PTINF-HOSPID
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF    ( FLG-PTINF           =   ZERO )
           AND   ( PTINF-TSTPTNUMKBN   =   "1"  )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-NYUINACCT   =   1
                               OR      FLG-ERR         =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "NYUINACCT-KEY13"       TO  ORC-DBPATH
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
      *
      *      既に仮収納があれば作成しない
             PERFORM 900-SYUNOU-INV-SEC
             IF      FLG-DATA          =   1
                 MOVE    KEY-NEW             TO  KEY-OLD
                 PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                                 OR      FLG-ERR         =   1 
                                 OR      KEY-N-PTID  NOT =   KEY-O-PTID
                     MOVE    "NYUINACCT-KEY13"       TO  ORC-DBPATH
                     PERFORM 900-NYUINACCT-READ-SEC
                 END-PERFORM
             ELSE
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               INITIALIZE                      WRK-DAY-AREA
      *
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      FLG-ERR         =   1 
                               OR      KEY-NEW     NOT =   KEY-OLD
      *            剤識別区分別テーブル編集
                   EVALUATE    NACCT-ZAISKBKBN
                       WHEN    "1"
                           MOVE    1               TO  IDX
                       WHEN    "5"
                           MOVE    2               TO  IDX
                   END-EVALUATE
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1  >        31
                       IF      NACCT-DAY  (IDX1)       >   ZERO
                           MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
                       END-IF
                   END-PERFORM            
                   MOVE    "NYUINACCT-KEY13"       TO  ORC-DBPATH
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
      *
               IF      FLG-ERR         =   ZERO 
      *            対象の入院会計があるとき仮収納作成
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       31
                       IF      WRK-DAY (1 IDX)     >   ZERO    AND
                               WRK-DAY (2 IDX)     >   ZERO
                           PERFORM 200121-NYUINACCT-CHK-SEC
                           IF      WRK-SYOKBN      =   SPACE
                               PERFORM 200122-NYUINACCT-HENSYU-SEC
                               IF    ( WRK-DATAKBN     =   "1" ) 
                               OR    ( FLG-ERR         =    1  )
                                   MOVE    31              TO  IDX
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF            
             END-IF
           END-IF    
           .
       20012-IKKATU-NYUINACCT-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       200121-NYUINACCT-CHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOKBN
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYS-1001-HOSPID TO  COMB-HOSPID
           MOVE    KEY-O-PTID      TO  COMB-PTID
           MOVE    WRK-DAY (2 IDX) TO  COMB-HKNCOMBINUM 
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 910-HKNCOMBI-INV-SEC
      *
           IF      FLG-HKNCOMBI    =   ZERO
      *        医保以外読み飛ばし
               IF      COMB-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM OR
                                           WRK-CONS-JIBAI-HKNNUM 
                   MOVE    "1"         TO  WRK-SYOKBN
               ELSE    
      *            社保・国保の読み飛ばし
                   EVALUATE    WRK-PARA-RECEKBN
                       WHEN    "0"
                           CONTINUE 
                       WHEN    "1"
                           IF      COMB-HKNNUM 
                                       NOT =   "060" AND "067"
                               CONTINUE
                           ELSE
                               MOVE    "1"       TO  WRK-SYOKBN
                           END-IF
                       WHEN    "2"
                           IF      COMB-HKNNUM
                                           =   "060" OR  "067"
                               CONTINUE
                           ELSE
                               MOVE    "1"       TO  WRK-SYOKBN
                           END-IF
                   END-EVALUATE
               END-IF
           END-IF
           .
       200121-NYUINACCT-CHK-EXT.
           EXIT. 
      *
      *****************************************************************
      *    仮収納作成処理（入院会計）
      *****************************************************************
       200122-NYUINACCT-HENSYU-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-DATAKBN
      *
      *    労災保険・自賠責のとき処理しない
           IF      WRK-SYOKBN       =   "1"
               GO  TO  200122-NYUINACCT-HENSYU-EXT
           END-IF
      *
      *    仮収納作成 
           PERFORM 20011-HENSYU-SEC
      *
           .
        200122-NYUINACCT-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    仮収納作成処理 
      *****************************************************************
       20011-HENSYU-SEC                SECTION.
      *
      *    ダミー収納削除処理
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SYS-1001-HOSPID     TO  SYU-HOSPID
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    KEY-O-PTID          TO  SYU-PTID
           MOVE    WRK-STYMD (1:6)     TO  SYU-SRYYMD(1:6)
           MOVE    "__"                TO  SYU-SRYYMD(7:2)
           MOVE    "3"                 TO  SYU-CREATEKBN
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "SYUNOU-DEL4"       TO  ORC-DBPATH
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               MOVE  "仮収納データ削除処理に失敗しました"
                               TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ダミー日別収納削除処理
      *
           IF   FLG-ERR         =   1
              CONTINUE
           ELSE
              INITIALIZE                      SYUDAY-REC
              MOVE    SYS-1001-HOSPID     TO  SDAY-HOSPID
              MOVE    "1"                 TO  SDAY-NYUGAIKBN
              MOVE    KEY-O-PTID          TO  SDAY-PTID
              MOVE    WRK-STYMD (1:6)     TO  SDAY-SKYYM
              MOVE    "3"                 TO  SDAY-CREATEKBN
              MOVE    SYUDAY-REC          TO  MCPDATA-REC
              MOVE    "SYUDAY-DEL3"       TO  ORC-DBPATH
              MOVE    "DBDELETE"          TO  MCP-FUNC
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
              IF      MCP-RC          NOT =   ZERO
                  MOVE  "仮日別収納データ削除処理に失敗しました"
                                  TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
           END-IF
      *
           IF   FLG-ERR         =   ZERO
      *
      *       伝票番号セット
      *
              INITIALIZE                       SYUNOU-REC
              MOVE    SYS-1001-HOSPID      TO  SYU-HOSPID
              MOVE    "1"                  TO  SYU-NYUGAIKBN
              MOVE    KEY-O-PTID           TO  SYU-PTID
              MOVE    "3"                  TO  SYU-CREATEKBN
              MOVE    SYUNOU-REC           TO  MCPDATA-REC
              MOVE    "DBSELECT"           TO  MCP-FUNC
              MOVE    "SYUNOU-KEY39"       TO  ORC-DBPATH
              CALL    "ORCMCPSUB"          USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
              IF      MCP-RC               =       ZERO
                  MOVE    "SYUNOU-KEY39"   TO  ORC-DBPATH
      *
                  MOVE    "DBFETCH"        TO  MCP-FUNC
                  CALL    "ORCMCPSUB"          USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                  IF      MCP-RC              =   ZERO
                     MOVE    MCPDATA-REC     TO  SYUNOU-REC
                     MOVE    SYU-DENPNUM     TO  WRK-DENPNUM
                  ELSE
                     MOVE    9100000         TO  WRK-DENPNUM
                  END-IF
              ELSE
                  DISPLAY  "SYUNOU SELECT ERR"
                  MOVE       9100000         TO  WRK-DENPNUM
              END-IF
      *
              MOVE    "SYUNOU-KEY39"  TO  ORC-DBPATH
              MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
              CALL    "ORCMCPSUB"     USING   MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
      *
      ********
      *
              MOVE    ZERO                TO  WRK-SEIKYUSTYMD
                                              WRK-SEIKYUEDYMD
      *       診療月内の入退院情報
              INITIALIZE                      PTNYUINRRK-REC
              MOVE    SYS-1001-HOSPID     TO  PTNYUINRRK-HOSPID
              MOVE    KEY-O-PTID          TO  PTNYUINRRK-PTID
              MOVE    WRK-STYMD (1:6)     TO  PTNYUINRRK-NYUINYMD (1:6)
              MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
              MOVE    WRK-STYMD (1:6)     TO  PTNYUINRRK-TAIINYMD (1:6)
              MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
              MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
              MOVE    "PTNYUINRRK-KEY23"  TO  ORC-DBPATH
              MOVE    "DBSELECT"          TO  MCP-FUNC
              CALL    "ORCMCPSUB"         USING   MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "PTNYUINRRK-KEY23"  TO  ORC-DBPATH
                  PERFORM 900-PTNYUINRRK-READ-SEC
              ELSE
                  MOVE    1                   TO  FLG-PTNYUINRRK    
              END-IF
              PERFORM     UNTIL   FLG-PTNYUINRRK    =   1
                   IF    ( WRK-SEIKYUSTYMD =   ZERO                )   
                   OR    ( WRK-SEIKYUSTYMD >   PTNYUINRRK-NYUINYMD )
                       IF  WRK-SEIKYUSTYMD =   WRK-STYMD
                          CONTINUE
                       ELSE
                         IF  PTNYUINRRK-NYUINYMD  >  WRK-STYMD
                           MOVE  PTNYUINRRK-NYUINYMD 
                                                 TO  WRK-SEIKYUSTYMD
                         ELSE
                           MOVE  WRK-STYMD       TO  WRK-SEIKYUSTYMD
                         END-IF 
                       END-IF
                   END-IF             
                   IF      WRK-SEIKYUEDYMD =   ZERO
                       MOVE  PTNYUINRRK-TAIINYMD TO  WRK-SEIKYUEDYMD
                   END-IF             
                   MOVE    "PTNYUINRRK-KEY23"  TO  ORC-DBPATH
                   PERFORM 900-PTNYUINRRK-READ-SEC
              END-PERFORM        
      *                 
              MOVE    "PTNYUINRRK-KEY23"  TO  ORC-DBPATH
              MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
              CALL    "ORCMCPSUB"         USING   MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
      *
              IF      WRK-SEIKYUSTYMD     =   ZERO
                  MOVE    WRK-STYMD           TO  WRK-SEIKYUSTYMD
              END-IF 
              IF    ( WRK-SEIKYUEDYMD     =   ZERO    OR   99999999 )
              OR    ( WRK-SEIKYUEDYMD     >   WRK-EDYMD             )
                  MOVE    WRK-EDYMD           TO  WRK-SEIKYUEDYMD
              END-IF 
      *
      *       収納データ取得処理
              MOVE    SPACE           TO  LNK-ORCS02-REC
                                          LNK-SYUNOU-REC-T
                                          LNK-SYUDAY-REC-T
      *
              INITIALIZE                  LNK-ORCS02-REC
                                          LNK-SYUNOU-REC-T
                                          LNK-SYUDAY-REC-T
      *
              MOVE    SYS-1001-HOSPID TO  LNK-S02-HOSPID
              MOVE    "1"             TO  LNK-S02-NYUGAIKBN
              MOVE    KEY-O-PTID      TO  LNK-S02-PTID
              MOVE    WRK-SEIKYUSTYMD TO  LNK-S02-SKYSTYMD
              MOVE    WRK-SEIKYUEDYMD TO  LNK-S02-SKYEDYMD
              MOVE    "X"             TO  LNK-S02-TEIKI-KBN
              MOVE    WRK-PARA-SYSYMD TO  LNK-S02-SYSYMD
              MOVE    "2"             TO  LNK-S02-SYORI-KBN
      *
              CALL    "ORCSNYUFTN"    USING
                                      SPA-AREA
                                      LNK-ORCS02-REC
                                      LNK-SYUNOU-REC-T
                                      LNK-SYUDAY-REC-T
      *
      *       仮収納出力処理
              IF      LNK-S02-RC      =   ZERO
                  PERFORM VARYING IDX1   FROM    1   BY  1
                          UNTIL ( IDX1                    >     10  )
                            OR  ( LNK-SYU-SKYSTYMD (IDX1) =   SPACE )
                            OR  ( FLG-ERR                 =      1  )
      *****         IF  LNK-SYU-SYUHKNNUM(IDX1)  =
      *****                                 WRK-CONS-ROUSAI-HKNNUM OR
      *****                                 WRK-CONS-JIBAI-HKNNUM 
      *****            CONTINUE
      *****         ELSE
                       PERFORM 20011-KARITEIKI-OUTPUT-SEC
      *****         END-IF
                  END-PERFORM
              ELSE
                  DISPLAY "PTID=" KEY-O-PTID
                  DISPLAY "SKYSTYMD=" WRK-SEIKYUSTYMD
                  DISPLAY "SKYEDYMD=" WRK-SEIKYUEDYMD
      *
                  MOVE    "仮収納データの取得に失敗"
                                          TO  ERR-ERRMSG
                  PERFORM 600-ERRMSG-EDIT-SEC
      *
                  MOVE  ERR-HEN-ERRMSG    TO  WRK-RECEERR
      *
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
      *
           END-IF
      *
           .
       20011-HENSYU-EXT.
           EXIT. 
      *****************************************************************
      *    仮収納出力処理
      *****************************************************************
       20011-KARITEIKI-OUTPUT-SEC      SECTION.
      *
           MOVE    "1"                 TO  WRK-DATAKBN
      *
           MOVE    LNK-SYUNOU-REC (IDX1)
                                       TO  SYUNOU-REC
           MOVE    LNK-SYUDAY-REC (IDX1)
                                       TO  SYUDAY-REC
      *
      *    伝票番号取得処理
           ADD     1                   TO  WRK-DENPNUM
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
                                           SDAY-DENPNUM
      *
           MOVE    WRK-PARA-TERMID     TO  SYU-TERMID
                                           SDAY-TERMID
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SMCNDATE-YMD        TO  SYU-CREYMD
                                           SDAY-CREYMD
           MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                                           SDAY-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
                                           SDAY-UPHMS
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO 
               COMPUTE CNT-OUT1    =   CNT-OUT1    +   1
           ELSE 
               DISPLAY "PTID=" KEY-O-PTID
               DISPLAY "SKYSTYMD=" WRK-SEIKYUSTYMD
               DISPLAY "SKYEDYMD=" WRK-SEIKYUEDYMD
      *
      *        MOVE    "仮収納データの更新に失敗しました"
      *                                TO  WRK-RECEERR
      *
               MOVE    "仮収納データの更新に失敗"
                                       TO  ERR-ERRMSG
               PERFORM 600-ERRMSG-EDIT-SEC
      *
               MOVE  ERR-HEN-ERRMSG    TO  WRK-RECEERR
      *
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF  FLG-ERR          =   1
               CONTINUE
           ELSE
             IF  SDAY-DAYINFFLG   =   "1"
      *         仮日別収納
                MOVE    SYUDAY-REC          TO  MCPDATA-REC
                MOVE    "SYUDAY-KEY"        TO  ORC-DBPATH
                MOVE    "DBINSERT"          TO  MCP-FUNC
                CALL    "ORCMCPSUB"         USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
                IF      MCP-RC              =   ZERO 
                    COMPUTE CNT-OUT2    =   CNT-OUT2    +   1
                ELSE 
                    DISPLAY "PTID=" KEY-O-PTID
                    DISPLAY "SKYYM=" WRK-SEIKYUSTYMD(1:6)
      *
                    MOVE    "仮日別収納データの更新に失敗"
                                            TO  ERR-ERRMSG
                    PERFORM 600-ERRMSG-EDIT-SEC
      *
                    MOVE  ERR-HEN-ERRMSG    TO  WRK-RECEERR
      *
                    PERFORM 500-ERR-HENSYU-SEC
                END-IF
             END-IF
           END-IF
      *
           .
       20011-KARITEIKI-OUTPUT-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    個別処理
      *****************************************************************
       2002-KOBETU-SYORI-SEC               SECTION.
      *
           IF      WRK-PARA-SHELLID        =   "RECEPTX"
               PERFORM 20021-KOBETU-REAL-SEC
               MOVE    "1"   TO  PTINF-TSTPTNUMKBN
               GO  TO    2002-KOBETU-SYORI-EXT
           END-IF
      *
      *    レセプト指示（個別指示）コード読み込み
           INITIALIZE                      SYS-2002-REC
           MOVE    "2002"              TO  SYS-2002-KANRICD
           MOVE    ZERO                TO  ORC-DBYMD
           MOVE    SYS-2002-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY7"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY7"     TO  ORC-DBPATH
               PERFORM 900-SYS2002-READ-SEC
           ELSE
               INITIALIZE                  SYS-2002-REC
               MOVE    1                   TO  FLG-SYS2002
           END-IF
      *
           PERFORM 20021-KOBETU-HENSYU-SEC
                              UNTIL       FLG-SYS2002     =   1
                                OR        FLG-ERR         =   1
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY7"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
      *
       2002-KOBETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-REAL-SEC              SECTION.
      *
           MOVE    WRK-PARA-PTID    TO  SYS-2002-PTID
           MOVE    WRK-PARA-SRYYM-O TO  SYS-2002-SRYYM
           MOVE    "1"              TO  SYS-2002-NYUGAIKBN
           DISPLAY "SYS PTID=" SYS-2002-PTID
      *
      *    該当診療年月の医療機関情報取得
           MOVE  ZERO           TO  FLG-SET
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  10   )
                     OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
                MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                    MOVE     1       TO    FLG-SET
                END-IF
           END-PERFORM
           IF     FLG-SET              =   ZERO
               STRING   "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO       TO      20021-KOBETU-REAL-EXT
           END-IF
      *
      *    患者情報取得
           MOVE    SYS-1001-HOSPID     TO  PTINF-HOSPID
           MOVE    SYS-2002-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    "1"                 TO  PTINF-TSTPTNUMKBN
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
      *
      *    診療会計読み込み
      *
           MOVE    SPACE               TO  WRK-DATAKBN
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SYS-1001-HOSPID     TO  ACCT-HOSPID
           MOVE    SYS-2002-PTID       TO  ACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY37"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY37"     TO  ORC-DBPATH
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
                     OR        FLG-ERR         =   1
               PERFORM 200211-KOBETU-SRYACCT-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACCT-KEY37"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    処理対象のとき仮収納作成
           IF      FLG-SYORI           =   1
               PERFORM 20011-SRYACCT-FILE-HENSYU-SEC 
           END-IF
      *
      *    診療会計より仮収納を作成しなかったときのみ処理
           IF    ( WRK-DATAKBN         =   SPACE )
           AND   ( FLG-ERR             =   ZERO  )
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *        入院会計読み込み
               INITIALIZE                  NYUINACCT-REC
               MOVE    SYS-1001-HOSPID     TO  NACCT-HOSPID
               MOVE    SYS-2002-PTID       TO  NACCT-PTID
               MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "NYUINACCT-KEY14"   TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "NYUINACCT-KEY14"   TO  ORC-DBPATH
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT     =   1
                         OR        FLG-ERR           =   1
                   PERFORM 200212-KOBETU-NYUINACCT-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               MOVE    "NYUINACCT-KEY14"   TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           END-IF 
      *
           .
       20021-KOBETU-REAL-EXT.
           EXIT.
      *     
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-HENSYU-SEC              SECTION.
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID 
      *
      *    医療機関ＩＤ編集
           MOVE  ZERO           TO  FLG-SET
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
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
                END-IF
      *
           END-PERFORM
           IF     FLG-SET  =  ZERO
               STRING   "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE 
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO       TO      20021-KOBETU-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-DATAKBN
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *    診療会計読み込み
           INITIALIZE                  SRYACCT-REC
           MOVE    SYS-1001-HOSPID     TO  ACCT-HOSPID
           MOVE    SYS-2002-PTID       TO  ACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY37"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY37"     TO  ORC-DBPATH
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
                     OR        FLG-ERR         =   1
               PERFORM 200211-KOBETU-SRYACCT-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACCT-KEY37"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    処理対象のとき仮収納作成
           IF      FLG-SYORI           =   1
               PERFORM 20011-SRYACCT-FILE-HENSYU-SEC 
           END-IF
      *
      *    診療会計より仮収納を作成しなかったときのみ処理
           IF    ( WRK-DATAKBN         =   SPACE )
           AND   ( FLG-ERR             =   ZERO  )
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *        入院会計読み込み
               INITIALIZE                  NYUINACCT-REC
               MOVE    SYS-1001-HOSPID     TO  NACCT-HOSPID
               MOVE    SYS-2002-PTID       TO  NACCT-PTID
               MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "NYUINACCT-KEY14"   TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "NYUINACCT-KEY14"   TO  ORC-DBPATH
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT     =   1
                         OR        FLG-ERR           =   1
                   PERFORM 200212-KOBETU-NYUINACCT-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               MOVE    "NYUINACCT-KEY14"   TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           END-IF 
      *
           MOVE    "SYSKANRI-KEY7"     TO  ORC-DBPATH
           PERFORM 900-SYS2002-READ-SEC
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    診療会計個別処理（患者ＩＤ、保険組合せ、診療科順）
      *****************************************************************
       200211-KOBETU-SRYACCT-HENSYU-SEC    SECTION.
      *
      *    保険組合せ読込み
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYS-1001-HOSPID     TO  COMB-HOSPID
           MOVE    KEY-N-PTID          TO  COMB-PTID
           MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM  910-HKNCOMBI-INV-SEC
      *
           MOVE    SPACE               TO  WRK-ROUSAIKBN
      *
           IF      FLG-HKNCOMBI        =   ZERO
               IF      COMB-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM OR 
                                           WRK-CONS-JIBAI-HKNNUM 
                   MOVE    "1"             TO  WRK-ROUSAIKBN
               END-IF    
           END-IF    
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM         UNTIL   FLG-SRYACCT     =   1 
                           OR      FLG-ERR         =   1
                           OR      KEY-NEW     NOT =   KEY-OLD
               IF      WRK-ROUSAIKBN      =   SPACE
      *          社保・国保の読み飛ばし
                 EVALUATE    WRK-PARA-RECEKBN
                   WHEN    "0"
                       MOVE    1               TO  FLG-SYORI
                   WHEN    "1"
                       IF      COMB-HKNNUM NOT =   "060" AND "067" AND
                                                   "068" AND "069" 
                           MOVE    1               TO  FLG-SYORI
                       END-IF
                   WHEN    "2"
                       IF      COMB-HKNNUM     =   "060" OR  "067" OR
                                                   "068" OR  "069"
                           MOVE    1               TO  FLG-SYORI
                       END-IF
                 END-EVALUATE
               END-IF  
      *
               IF        FLG-ERR            =    1
                 CONTINUE
               ELSE
                 IF      FLG-SYORI          =   1
      *            仮収納作成のとき処理を抜ける 
                   MOVE    1                  TO  FLG-SRYACCT
                 ELSE
      *            同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM     UNTIL   FLG-SRYACCT     =   1
                               OR      FLG-ERR         =   1  
                               OR      KEY-NEW     NOT =   KEY-OLD
                       MOVE    "SRYACCT-KEY37"     TO  ORC-DBPATH
                       PERFORM 900-SRYACCT-READ-SEC
                   END-PERFORM
                 END-IF
               END-IF 
           END-PERFORM    
           .
       200211-KOBETU-SRYACCT-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    入院会計個別編集処理
      *****************************************************************
       200212-KOBETU-NYUINACCT-HENSYU-SEC              SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           INITIALIZE                      WRK-DAY-AREA
      *
           PERFORM         UNTIL   FLG-NYUINACCT   =   1
                           OR      FLG-ERR         =   1 
      *        剤識別区分別テーブル編集
               EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "1"
                       MOVE    1               TO  IDX
                   WHEN    "5"
                       MOVE    2               TO  IDX
               END-EVALUATE
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1  >        31
                   IF      NACCT-DAY  (IDX1)       >   ZERO
                       MOVE    NACCT-DAY (IDX1)    TO
                                               WRK-DAY (IDX IDX1)
                   END-IF
               END-PERFORM            
               MOVE    "NYUINACCT-KEY14"       TO  ORC-DBPATH
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           IF      FLG-ERR             =   ZERO
      *        対象の入院会計があるとき仮収納作成
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF      WRK-DAY (1 IDX)     >   ZERO    AND
                           WRK-DAY (2 IDX)     >   ZERO
                       PERFORM 200121-NYUINACCT-CHK-SEC
                       IF      WRK-SYOKBN      =   SPACE
                           PERFORM 200122-NYUINACCT-HENSYU-SEC
                           IF    ( WRK-DATAKBN     =   "1" ) 
                           OR    ( FLG-ERR         =    1  )
                                   MOVE    31              TO  IDX
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF    
           .
       200212-KOBETU-NYUINACCT-HENSYU-EXT.
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
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
           END-IF
      *                             
           MOVE    1                   TO  FLG-ERR
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
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
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
                       MOVE    WRK-PARA-SHELLID
                                               TO  JOB-SHELLID
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
                       CALL    "ORCSJOB"       USING
                                               ORCSJOBKANRIAREA  
                                               JOBKANRI-REC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
                                                   FLG-ERR
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       600-ERRMSG-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
                                           ERR-PTNUM
      *
      *    患者番号を取得
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SYS-1001-HOSPID     TO  SPTID-HOSPID
           MOVE    KEY-O-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-1009-TBL
           IF    ( SPTID-RC            =   ZERO )
               MOVE    SPTID-PTNUM     TO  ERR-PTNUM
           ELSE
               STRING "?(ID:"          DELIMITED   BY  SIZE
                      KEY-O-PTID       DELIMITED   BY  SIZE
                      ")"              DELIMITED   BY  SIZE
               INTO   ERR-PTNUM
               END-STRING
           END-IF
      *
           MOVE    ZERO                TO  WRK-SHO
                                           WRK-AMARI
           MOVE    1                   TO  WRK-LEN
           MOVE    SPACE               TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR
           WITH  POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN =   WRK-LEN -   1
      *
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
              STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                      "［患者番号："      DELIMITED   BY  SIZE
                      ERR-PTNUM           DELIMITED   BY  SPACE
                      "］"                DELIMITED   BY  SIZE
              INTO    ERR-HEN-ERRMSG
              END-STRING
           ELSE
              STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                      "［患者番号:"       DELIMITED   BY  SIZE
                      ERR-PTNUM           DELIMITED   BY  SPACE
                      "］"                DELIMITED   BY  SIZE
              INTO    ERR-HEN-ERRMSG
              END-STRING
           END-IF
      *
           .
       600-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *    IF     FLG-ERR     =   ZERO
      *    AND    CNT-OUT1    =   ZERO
      *        MOVE    "処理対象のデータがありませんでした"
      *                                TO  WRK-RECEERR
      *        PERFORM 500-ERR-HENSYU-SEC
      *    END-IF 
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
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
           END-IF
      *
           DISPLAY "KARITEIKI (ORCR0600) CNT " CNT-OUT1
           DISPLAY "HIBETU    (ORCR0600) CNT " CNT-OUT2
           DISPLAY "*** ORCR0600 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYS2002-READ-SEC         SECTION.
      *
           MOVE    "SYSKANRI-KEY7"     TO  ORC-DBPATH
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYS2002
               MOVE    MCPDATA-REC     TO  SYS-2002-REC
           ELSE
               MOVE    1               TO  FLG-SYS2002
               INITIALIZE                  SYS-2002-REC
           END-IF
           .
       900-SYS2002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                       TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスタ　ＲＥＡＤ
      *****************************************************************
       900-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               MOVE    ZERO        TO  FLG-PTNYUINRRK
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ　ＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ　ＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    NACCT-PTID          TO  KEY-N-PTID 
      *********MOVE    NACCT-HKNCOMBI      TO  KEY-N-HKNCOMBI
      *********MOVE    NACCT-SRYKA         TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1               TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                       TO  FLG-HKNCOMBI
               INITIALIZE                          HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                       TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       900-SYUNOU-INV-SEC         SECTION.
      *
           MOVE       ZERO              TO      FLG-DATA
      *
           INITIALIZE                           SYUNOU-REC
           MOVE    SYS-1001-HOSPID      TO      SYU-HOSPID
           MOVE    "1"                  TO      SYU-NYUGAIKBN
           MOVE    KEY-N-PTID           TO      SYU-PTID
           MOVE    WRK-PARA-SRYYM       TO      SYU-SRYYMD(1:6)
           MOVE    "__"                 TO      SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SYUNOU-KEY17"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
             MOVE      "SYUNOU-KEY17"   TO      ORC-DBPATH
             PERFORM  900-SYUNOU-READ-SEC
             PERFORM  UNTIL
                            (FLG-SYUNOU     =    1)  OR
                            (FLG-DATA       =    1)
      *
               IF   SYU-SYUHKNNUM   =  WRK-CONS-ROUSAI-HKNNUM
                                   OR  WRK-CONS-JIBAI-HKNNUM
                 CONTINUE
               ELSE
                 MOVE     1       TO     FLG-DATA
               END-IF
      *
               MOVE       "SYUNOU-KEY17"  TO   ORC-DBPATH
               PERFORM     900-SYUNOU-READ-SEC
      *
             END-PERFORM
           END-IF
      *
           MOVE    "SYUNOU-KEY17"  TO  ORC-DBPATH
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCMCPSUB"     USING   MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       900-SYUNOU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       900-SYUNOU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-READ-EXT.
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
