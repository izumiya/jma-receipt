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
000200 IDENTIFICATION          DIVISION.
000300 PROGRAM-ID.             ORCR0610.
000400*****************************************************************
000500*  システム名        : ＯＲＣＡ
000600*  サブシステム名    : レセプト
000700*  コンポーネント名  : 月次業務　明細書・請求書用ファイル作成
000800*  管理者            : 
000900*  作成日付    作業者        記述
      *  02/12/101   NACL−藤原　  新規作成
001100*****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *  01.00.02    NACL-藤原    03/03/11  種別不明の情報を編集
      *  01.00.03    NACL-藤原    03/03/15  入院会計からの診療科の編集の修正
      *  01.01.04    NACL-藤原    03/04/21  レセのとき地方公費のみの場合は
      *                                     出力しない
      *  01.01.05    NACL-藤原    03/07/28  診療回数テーブルを編集
      *  01.01.06    NACL-藤原    03/11/18  社保・国保別で作成のとき
      *                                     読み飛ばしをしない
      *  01.01.07    NACL-藤原    03/11/19  老人＋原爆のレセ記載をシス管
      *                                     より取得
      *  01.01.08    NACL-門脇    04/01/14  診療行為の包括分は除く
      *                                     SRYACCT8→29
      *                                     SRYACCT9→30
      *  01.01.09    NACL-藤原    04/04/15  ＤＢ更新エラー時の処理
      *  01.03.04    NACL-藤原    05/03/03  老人又は前期高齢者のとき、
      *                                     月途中に保険者は変わらずに
      *                                     保険が変更になった場合は１枚
      *                                     にする
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
           SELECT  RECE61-FILE     ASSIGN  RECE61PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE61-KEY
                                   FILE    STATUS  IS  STS-RECE61.
      *
           SELECT  RECE611-FILE    ASSIGN  RECE61PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE611-KEY
                                   FILE    STATUS  IS  STS-RECE611.
      *
      *    医療費請求書  
           SELECT  RECE61X-FILE    ASSIGN  RECE61XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE61X-KEY
                                   FILE    STATUS  IS  STS-RECE61X.
      *
           SELECT  RECE611X-FILE   ASSIGN  RECE61XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE611X-KEY
                                   FILE    STATUS  IS  STS-RECE611X.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE61-FILE.
       01  RECE61-REC. 
           COPY    "CPRCF061.INC".
      *
       FD  RECE611-FILE.
       01  RECE611-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //RECE611//.
      *
      *    医療費請求書  
       FD  RECE61X-FILE.
       01  RECE61X-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //RECE61X//.
      *
       FD  RECE611X-FILE.
       01  RECE611X-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //RECE611X//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE61//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE61X//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE61                              PIC X(02).
           03  STS-RECE611                             PIC X(02).
           03  STS-RECE61X                             PIC X(02).
           03  STS-RECE611X                            PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01). 
           03  FLG-HKNNUM                              PIC 9(01). 
           03  FLG-RECE61                              PIC 9(01).
           03  FLG-RECE61X                             PIC 9(01).
           03  FLG-ADDKBN                              PIC 9(01).
           03  FLG-PTHKNINF                            PIC 9(01).
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-SEIKYU                              PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-KOHSKY                              PIC 9(01).
           03  FLG-RECE07                              PIC 9(01).
      * 
           03  FLG-KOH                                 PIC 9(01).
           03  FLG-CONTKBN                             PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC 9(03). 
           03  IDX1                                    PIC 9(03). 
           03  IDX2                                    PIC 9(03). 
           03  IDX3                                    PIC 9(03). 
           03  IDY                                     PIC 9(03). 
           03  IDZ                                     PIC 9(03).
           03  TBL-MAX                                 PIC 9(03).
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
           03  WRK-PARA-SRYYM                      PIC  X(06).
           03  WRK-PARA-TERMID                     PIC  X(16).
           03  WRK-PARA-SYSYMD                     PIC  X(08).
           03  WRK-PARA-SYOKBN                     PIC  X(01).
           03  WRK-PARA-RECEKBN                    PIC  X(01).
           03  WRK-PARA-JOBID                      PIC  9(07).
           03  WRK-PARA-SHELLID                    PIC  X(08).
      *
      *    一時領域
      *
       01  WRK-AREA.
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-CONTKBN             PIC X(01).
      *
           03  WRK-RECEERR             PIC X(200). 
      *
           03  WRK-HKNNUM              PIC 9(03).     
      *
           03  WRK-PATH                PIC X(64).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).  
           03  WRK-SYOKBN              PIC X(01).
           03  WRK-DBKBN               PIC X(01).
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN          PIC X(01).
               05  WRK-TEISEIKBN       PIC X(01).
               05  WRK-ROUSAIKBN       PIC X(01).
               05  WRK-ERR-DAY         PIC 9(02).
               05  WRK-ERR-HKNCOMBINUM PIC 9(04).
               05  WRK-ERR-ERRKBN      PIC 9(01).
               05  WRK-ERR-TEKSTYMD    PIC 9(08).
               05  WRK-ERR-TEKEDYMD    PIC 9(08).
      * 
           03  WRK-NENREI                PIC 9(03).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *              
      *    診療回数テーブル（入院会計用　１：基本　２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  2.
                   07  WRK-DAY         PIC 9(03)
                                           OCCURS   31.
      *
      *    公費並替え用ワーク
           03  WRK-KOHI-AREA.
               05  WRK-KOHI-TBL        OCCURS  4.
                   07  WRK-KOHID-O     PIC  9(10).
                   07  WRK-KOHHKNNUM-O PIC  X(03).
                   07  WRK-KOHPAYKBN-O PIC  X(02).
           03  WRK-KOHI.
               05  WRK-KOHID           PIC  9(10).
               05  WRK-KOHHKNNUM       PIC  X(03).
               05  WRK-KOHPAYKBN       PIC  X(02).
      *
      *    請求区分別公費情報（１：レセプト　２：請求書）
           03  WRK-SEIKYUKBN-KOHI-AREA.
               05  WRK-SEIKYU-TBL   OCCURS  2.
                   07  WRK-SEIKYU-OCC    OCCURS  4.
                       09  WRK-SEIKYU-KOHID      PIC  9(10).
                       09  WRK-SEIKYU-KOHHKNNUM  PIC  X(03).
                       09  WRK-SEIKYU-KOHPAYKBN  PIC  X(02).
      *
       01  WRK-SRT-AREA.
           03  WRK-SRT-TABLE.
               05                      PIC X(03)   VALUE   "027".  
               05                      PIC X(03)   VALUE   "013".  
               05                      PIC X(03)   VALUE   "014".  
               05                      PIC X(03)   VALUE   "018".  
               05                      PIC X(03)   VALUE   "029".  
               05                      PIC X(03)   VALUE   "010".  
               05                      PIC X(03)   VALUE   "011".  
               05                      PIC X(03)   VALUE   "020".  
               05                      PIC X(03)   VALUE   "021".  
               05                      PIC X(03)   VALUE   "022".  
               05                      PIC X(03)   VALUE   "028".  
               05                      PIC X(03)   VALUE   "015".  
               05                      PIC X(03)   VALUE   "016".  
               05                      PIC X(03)   VALUE   "017".  
               05                      PIC X(03)   VALUE   "019".  
               05                      PIC X(03)   VALUE   "023".  
               05                      PIC X(03)   VALUE   "051".  
               05                      PIC X(03)   VALUE   "091".  
               05                      PIC X(03)   VALUE   "052".  
               05                      PIC X(03)   VALUE   "053".  
               05                      PIC X(03)   VALUE   "012".
               05                      PIC X(03)   VALUE   "972".
           03  WRK-SRT-TABLE-R         REDEFINES   WRK-SRT-TABLE.
               05  WRK-SRT-KOHHKNNUM   PIC X(03)   OCCURS  22.
      *     
      *    ワーク保険組合せ（入院会計用） 
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10. 
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           03  WRK-HKNCOMBI-SYOKBN     PIC X(01).   
           03  WRK-HKNCOMBI-ERR-DAY    PIC 9(02).   
      *
       01  WRK-CONS-AREA.
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *     
      * 
       01  WRK-RECE61-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WRK-RECE61//.
      *
       01  WRK-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //WRK-RECE07//.
      *
      *    社保（継続）のとき 
       01  WRK-CONTKBN-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WRK-CONTKBN//.
      *
           COPY    "MCPAREA".
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
           COPY  "CPSK1001.INC".
      *    医療機関情報
           COPY  "CPSK1005.INC".
      *    レセプト作成指示コード
           COPY    "CPSK2001.INC".
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *     
           COPY    "CPSK2005.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
      *    レセプト
           COPY    "CPRCF008.INC".
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
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *   保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
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
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCR0610"          TO  JOB-PGID
           MOVE    "対象患者抽出"      TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *
           MOVE    ZERO                TO  FLG-END
      *
           MOVE    WRK-PARA-SRYYM      TO  SYS-2001-SRYYM
           MOVE    WRK-PARA-SYOKBN     TO  SYS-2001-SYOKBN 
      *
           MOVE    "RECE610"           TO  RECE61PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE61PARA-TERMID
      *
           MOVE    "RECE61X"           TO  RECE61XPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE61XPARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関ＩＤ編集
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
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                    PERFORM 900-SYSKANRI-READ-SEC
      *
                  END-PERFORM
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                            TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
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
      *    レセプト編集区分情報
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE               TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-RJNGNBKKBN =   SPACE
               MOVE    ZERO                TO  SYS-2005-RJNGNBKKBN
           END-IF
      *
           OPEN    OUTPUT              RECE611-FILE
           CLOSE                       RECE611-FILE
      *
           OPEN    I-O                 RECE61-FILE  
      *
           OPEN    OUTPUT              RECE611X-FILE
           CLOSE                       RECE611X-FILE
      *
           OPEN    I-O                 RECE61X-FILE  
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "1"
                   PERFORM 2001-IKKATU-SYORI-SEC
               WHEN    "2"
                   PERFORM 2002-KOBETU-SYORI-SEC
           END-EVALUATE
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
      *    診療年月の請求管理削除処理（種別不明分）
           INITIALIZE                  RECE10-REC
           MOVE    SYS-1001-HOSPID     TO  RECE10-HOSPID
           MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
           MOVE    "1"                 TO  RECE10-NYUGAIKBN
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "SEIKYU-DEL5"       TO  ORC-DBPATH
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC      
           IF      MCP-RC           NOT =   ZERO
               DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
               MOVE
                   "請求管理削除処理（種別不明分）でエラーとなりました"
                                           TO    WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  2001-IKKATU-SYORI-EXT
           END-IF
      *
      *    診療年月の請求管理削除処理（社保分）
           IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
               INITIALIZE                  RECE10-REC
               MOVE    SYS-1001-HOSPID     TO  RECE10-HOSPID
               MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
               MOVE    "1"                 TO  RECE10-NYUGAIKBN
               MOVE    1                   TO  RECE10-TEISYUTUSAKI
               MOVE    RECE10-REC          TO  MCPDATA-REC
               MOVE    "SEIKYU-DEL6"       TO  ORC-DBPATH
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC      
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                   MOVE
                       "請求管理削除処理（社保分）でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  2001-IKKATU-SYORI-EXT
               END-IF
           END-IF      
      *
      *    診療年月の請求管理削除処理（国保分）
           IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
               INITIALIZE                  RECE10-REC
               MOVE    SYS-1001-HOSPID     TO  RECE10-HOSPID
               MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
               MOVE    "1"                 TO  RECE10-NYUGAIKBN
               MOVE    2                   TO  RECE10-TEISYUTUSAKI
               MOVE    RECE10-REC          TO  MCPDATA-REC
               MOVE    "SEIKYU-DEL6"       TO  ORC-DBPATH
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC      
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                   MOVE
                       "請求管理削除処理（国保分）でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  2001-IKKATU-SYORI-EXT
               END-IF
           END-IF      
      *
      *    診療年月の公費請求書削除処理（社保分）
           IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
               INITIALIZE                  KOHSKY-REC
               MOVE    SYS-1001-HOSPID     TO  KOHSKY-HOSPID
               MOVE    WRK-PARA-SRYYM      TO  KOHSKY-SRYYM
               MOVE    1                   TO  KOHSKY-TEISYUTUSAKI
               MOVE    "1"                 TO  KOHSKY-NYUGAIKBN
               MOVE    KOHSKY-REC          TO  MCPDATA-REC
               MOVE    "KOHSKY-DEL1"       TO  ORC-DBPATH
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC      
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                   MOVE
                     "公費請求書削除処理（社保分）でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  2001-IKKATU-SYORI-EXT
               END-IF
           END-IF      
      *
      *    診療年月公費請求書削除処理（国保分）
           IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
               INITIALIZE                  KOHSKY-REC
               MOVE    SYS-1001-HOSPID     TO  KOHSKY-HOSPID
               MOVE    WRK-PARA-SRYYM      TO  KOHSKY-SRYYM
               MOVE    2                   TO  KOHSKY-TEISYUTUSAKI
               MOVE    "1"                 TO  KOHSKY-NYUGAIKBN
               MOVE    KOHSKY-REC          TO  MCPDATA-REC
               MOVE    "KOHSKY-DEL1"        TO  ORC-DBPATH
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC      
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                   MOVE
                     "公費請求書削除処理（国保分）でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  2001-IKKATU-SYORI-EXT
               END-IF
           END-IF      
      *
      *    診療年月レセプト管理削除処理（一括分）
           INITIALIZE                  RECE07-REC
           MOVE    "HC06"              TO  RECE07-PRTID
           MOVE    WRK-PARA-SRYYM      TO  RECE07-SRYYM
           MOVE    "1"                 TO  RECE07-SYOKBN
           MOVE    "1"                 TO  RECE07-NYUGAIKBN
           MOVE    WRK-PARA-RECEKBN    TO  RECE07-RECEKBN
           MOVE    RECE07-REC          TO  MCPDATA-REC
           MOVE    "RECEKANRI-KEY7"    TO  WRK-PATH
           PERFORM 900-RECE07-INV-SEC
           IF      FLG-RECE07          =   ZERO
               MOVE    RECE07-REC          TO  WRK-RECE07-REC
               MOVE    RECE07-REC          TO  MCPDATA-REC 
               MOVE    "RECEKANRI-DEL1"    TO  ORC-DBPATH
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC    
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                   MOVE
                   "レセプト管理削除処理でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  2001-IKKATU-SYORI-EXT
               END-IF
      *
      *        診療年月レセプト削除処理（一括分）
               INITIALIZE                  RECE08-REC
               MOVE    WRK-RECE07-PRTID    TO  RECE08-PRTID
               MOVE    WRK-RECE07-SRYYM    TO  RECE08-RECEYM
               MOVE    WRK-RECE07-CREYMD   TO  RECE08-CREYMD
               MOVE    WRK-RECE07-CREHMS   TO  RECE08-CREHMS
               MOVE    WRK-RECE07-NYUGAIKBN
                                           TO  RECE08-NYUGAIKBN
               MOVE    RECE08-REC          TO  MCPDATA-REC
               MOVE    "RECEPRT-DEL1"      TO  ORC-DBPATH
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                   MOVE    "レセプト削除処理でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  2001-IKKATU-SYORI-EXT
               END-IF
           END-IF  
      *
      *    診療会計読み込み（患者ＩＤ、保険組合せ、診療科順）
      *
           MOVE    "1"                 TO  WRK-DBKBN 
      *      
           INITIALIZE                  SRYACCT-REC
           MOVE    SYS-1001-HOSPID     TO  ACCT-HOSPID
           MOVE    SYS-2001-SRYYM      TO  ACCT-SRYYM
           MOVE    "1"                 TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY29"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY29"     TO  ORC-DBPATH
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
               PERFORM 20011-IKKATU-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYACCT-KEY29"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    入院会計読み込み（患者ＩＤ、診療科、剤識別区分順）
      *
           MOVE    "2"                 TO  WRK-DBKBN 
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
      *     
           INITIALIZE                  NYUINACCT-REC
           MOVE    SYS-1001-HOSPID     TO  NACCT-HOSPID
           MOVE    SYS-2001-SRYYM      TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY13"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY13"       TO  ORC-DBPATH
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL       FLG-NYUINACCT   =   1
               PERFORM 20012-IKKATU-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY13"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    診療会計一括処理（患者ＩＤ、保険組合せ、診療科順）
      *****************************************************************
       20011-IKKATU-HENSYU-SEC              SECTION.
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SYS-1001-HOSPID         TO  PTINF-HOSPID
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "SRYACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "SRYACCT-KEY29"         TO  ORC-DBPATH
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *
      *            患者年齢計算
                   PERFORM 500-AGECHK-SEC
      *
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SYS-1001-HOSPID     TO  COMB-HOSPID
                   MOVE    KEY-N-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
      *            未訂正分があるか
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       IF      COMB-HKNNUM     =
                                               WRK-CONS-ROUSAI-HKNNUM OR
                                               WRK-CONS-JIBAI-HKNNUM 
                           MOVE    "1"             TO  WRK-ROUSAIKBN
                       ELSE     
                           IF      COMB-TEKSTYMD   <=  WRK-STYMD
                           AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                               MOVE    SPACE           TO  WRK-UPDKBN
                           ELSE
                               MOVE    "1"             TO  WRK-UPDKBN
                           END-IF
                       END-IF    
                   ELSE
                       MOVE    "1"             TO  WRK-TEISEIKBN    
                       MOVE    1               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO  WRK-ERR-HKNCOMBINUM
                   END-IF    
      *             
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-SRYACCT     =   1 
                                   OR      KEY-NEW     NOT =   KEY-OLD
      *****************社保・国保の読み飛ばし
      *****************EVALUATE    WRK-PARA-RECEKBN
      *                    WHEN    "0"
      *                        PERFORM 20011-FILE-SYORI-SEC 
      *                    WHEN    "1"
      *                        IF      COMB-HKNNUM 
      *                                        NOT =   "060" AND "067"
      *                            PERFORM 20011-FILE-SYORI-SEC 
      *                        END-IF
      *                    WHEN    "2"
      *                        IF      COMB-HKNNUM =   "060" OR  "067"
      *                            PERFORM 20011-FILE-SYORI-SEC 
      *                        END-IF
      *****************END-EVALUATE
                       PERFORM 20011-FILE-SYORI-SEC 
      *
                       IF      WRK-UPDKBN          =   SPACE                   
      *                    同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1 
                                       OR      KEY-NEW NOT =   KEY-OLD
                               MOVE    "SRYACCT-KEY29" TO  ORC-DBPATH
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                       ELSE
                           MOVE    "SRYACCT-KEY29"     TO  ORC-DBPATH
                           PERFORM 900-SRYACCT-READ-SEC
                       END-IF
                   END-PERFORM    
               END-PERFORM         
           END-IF    
           .
       20011-IKKATU-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    診療会計読込処理
      *****************************************************************
       20011-FILE-SYORI-SEC              SECTION.
      *
      *    労災保険・自賠責のとき処理しない
           IF      WRK-ROUSAIKBN       =   "1"
               GO  TO  20011-FILE-SYORI-EXT
           END-IF
      *                  
           IF      WRK-UPDKBN          =   "1"
      *    保険の訂正がされているか
               IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                   MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX             >=  WRK-DD
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           MOVE    IDX             TO  WRK-ERR-DAY
                           MOVE    2               TO  WRK-ERR-ERRKBN
                           MOVE    KEY-N-HKNCOMBI  TO
                                                   WRK-ERR-HKNCOMBINUM
                           MOVE    COMB-TEKSTYMD   TO  WRK-ERR-TEKSTYMD    
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKSTYMD(1:6)  >   WRK-STYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    3               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO
                                               WRK-ERR-HKNCOMBINUM
                       MOVE    COMB-TEKSTYMD   TO  WRK-ERR-TEKSTYMD    
                   END-IF
               END-IF      
               IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                   MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                   ADD     1                   TO  WRK-DD
                   PERFORM VARYING IDX FROM    WRK-DD  BY  1
                           UNTIL   IDX             >   31
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           MOVE    IDX             TO  WRK-ERR-DAY
                           MOVE    4               TO  WRK-ERR-ERRKBN
                           MOVE    KEY-N-HKNCOMBI  TO
                                                   WRK-ERR-HKNCOMBINUM
                           MOVE    COMB-TEKEDYMD   TO  WRK-ERR-TEKEDYMD    
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKEDYMD(1:6)  <   WRK-EDYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    5               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO
                                               WRK-ERR-HKNCOMBINUM
                       MOVE    COMB-TEKEDYMD   TO  WRK-ERR-TEKEDYMD    
                   END-IF    
               END-IF    
           END-IF
      *     
      *        レセプト診療科（旧総合病院のとき）
           IF      SYS-1001-HOSPSBT1   =   1
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    ACCT-SRYKA          TO  SYS-1005-KBNCD
               IF      WRK-PARA-SYOKBN     =   "1"
                   MOVE    SYS-2001-SRYYM      TO  ORC-DBYMD(1:6)
               ELSE
                   MOVE    SYS-2002-SRYYM      TO  ORC-DBYMD(1:6)
               END-IF
               MOVE    "01"                TO  ORC-DBYMD(7:2)
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
           ELSE
               INITIALIZE                   SYS-1005-REC
           END-IF
      *
      *    最終診療日
           MOVE    ACCT-SRYYM          TO  WRK-SRYYM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               IF      ACCT-DAY (1 IDX)    >   ZERO
                   MOVE    IDX                 TO  WRK-SRYDD
               END-IF
           END-PERFORM
      *
      *   主保険情報読み出し
           IF      COMB-HKNID          NOT =   ZERO
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYS-1001-HOSPID     TO  PTHKN-HOSPID
               MOVE    COMB-PTID           TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               MOVE    COMB-TEKSTYMD       TO  ORC-DBYMD
               PERFORM 910-PTHKNINF-INV-SEC
           ELSE
               INITIALIZE                      PTHKNINF-REC
           END-IF
      *
      *    社保（継続）編集処理
      *****MOVE    SPACE                   TO  WRK-CONTKBN-KEY
      *
      *     IF      COMB-KOHHKNNUM (1)  =   "012"   OR
      *             COMB-KOHHKNNUM (2)  =   "012"   OR
      *             COMB-KOHHKNNUM (3)  =   "012"   OR
      *             COMB-KOHHKNNUM (4)  =   "012" 
      *         IF      COMB-HKNID          NOT =   ZERO
      *             IF      PTHKN-CONTKBN           =   1       
      *                 PERFORM 20011-CONTKBN-CHECK-SEC
      *             END-IF
      *         ELSE
      *             PERFORM 20011-CONTKBN-CHECK-SEC
      *         END-IF
      *****END-IF
      *
      *    公費のレセプト・請求書への出力
      *
           INITIALIZE                  WRK-SEIKYUKBN-KOHI-AREA
                                       FLG-KOH
           MOVE    ZERO                TO  IDY
                                           IDZ    
      *
      *        公費単独のとき全てレセプトに出力
      *****IF      COMB-HKNNUM         =   SPACE
      *****     MOVE    COMB-KOHI-AREA      TO  WRK-SEIKYU-TBL (1)
      *****ELSE        
      *    地方公費のときレセプト請求区分を取得して判別
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4
                       OR      COMB-KOHHKNNUM (IDX)    
                                               =   SPACE
                   IF      COMB-KOHHKNNUM (IDX)(1:1)
                                               >=  "1" AND <=  "8"
                       INITIALIZE                      HKNNUM-REC
                       MOVE    SYS-1001-HOSPID         TO  HKN-HOSPID
                       MOVE    COMB-KOHHKNNUM (IDX)    TO  HKN-HKNNUM
                       MOVE    WRK-SRYYMD              TO  HKN-TEKSTYMD
                                                           HKN-TEKEDYMD
                       MOVE    "00"                    TO  HKN-PAYKBN   
                       MOVE    HKNNUM-REC              TO  MCPDATA-REC
                       PERFORM 900-HKNNUM-INV-SEC
      *
                       IF      COMB-HKNNUM         =   "060"   OR  "067" 
                           IF      HKN-RECESKYKBN      =   "1" OR  "3"
                               MOVE    2                   TO  IDX1
                           ELSE
                               MOVE    1                   TO  IDX1
                           END-IF
                       ELSE                
                           IF      HKN-RECESKYKBN      =   "2" OR  "3"
                               MOVE    2                   TO  IDX1
                           ELSE
                               MOVE    1                   TO  IDX1
                           END-IF
                       END-IF
                   ELSE
                       MOVE    1                      TO  IDX1
                   END-IF       
      *
                   IF     IDX1                =   1
                       ADD     1                      TO  IDY    
                       MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDY)
                       MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDY)
                   ELSE
                       ADD     1                      TO  IDZ    
                       MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDZ)
                       MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDZ)
                       MOVE    1                      TO  FLG-KOH 
                   END-IF  
               END-PERFORM
      ****END-IF           
      *
      *    レセプト明細書
           MOVE    "1"                 TO  WRK-SYOKBN
      *****IF      WRK-CONTKBN-KEY     NOT =   SPACE
      *        社保（継続）＋生保のとき
      *         MOVE    WRK-CONTKBN-KEY     TO  WRK-RECE61-KEY
      *         MOVE    WRK-RECE61-KEY      TO  RECE61-KEY 
      *         PERFORM 900-RECE61-READ-SEC
      *         IF      FLG-RECE61          =   ZERO
      *             MOVE    RECE61-REC          TO  WRK-RECE61-REC 
      *             PERFORM 20011-RECE61-REWRITE-SEC
      *         END-IF
      *****ELSE
      *        社保（継続）＋生保以外のとき
               PERFORM 20011-FILE-HENSYU-SEC
      *****END-IF   
      *
      *    医療費請求書
           IF      FLG-KOH             =   1  
               MOVE    "2"             TO  WRK-SYOKBN
               PERFORM 20011-FILE-HENSYU-SEC
           END-IF   
           .
       20011-FILE-SYORI-EXT.
           EXIT. 
      *            
      *            
      *****************************************************************
      *    入院会計一括編集処理
      *****************************************************************
       20012-IKKATU-HENSYU-SEC              SECTION.
      *     
      *    テスト患者のときは読み飛ばす
           MOVE    SYS-1001-HOSPID         TO  PTINF-HOSPID
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "NYUINACCT-KEY13"       TO  ORC-DBPATH
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               INITIALIZE                      WRK-DAY-AREA
                                               WRK-HKNCOMBI-TABLE
      *
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      KEY-NEW     NOT =   KEY-OLD
      *            剤識別区分別テーブル編集
                   EVALUATE    NACCT-ZAISKBKBN
                       WHEN    "1"
                           MOVE    1               TO  IDX
                       WHEN    "5"
                           MOVE    2               TO  IDX
                   END-EVALUATE
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       31
                       IF      NACCT-DAY  (IDX1)       >   ZERO
                           MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
                       END-IF
                   END-PERFORM            
                   MOVE    "NYUINACCT-KEY13"       TO  ORC-DBPATH
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
      *
      *        入院会計より保険組合せ読込     
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF      WRK-DAY (1 IDX)     >   ZERO    AND
                           WRK-DAY (2 IDX)     >   ZERO
                       PERFORM 200121-NYUINACCT-CHK-SEC
                   END-IF
               END-PERFORM        
      *
      *        入院会計の保険組合せを追加
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       10
                       OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "1"
                       CONTINUE
                   ELSE    
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
      *
                       MOVE    WRK-HKNCOMBI-REC (IDX2)
                                               TO  HKNCOMBI-REC    
                       IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           IF      WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                   =   ZERO
                               MOVE    1           TO  WRK-ERR-ERRKBN
                           ELSE
                               MOVE    WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                   TO  WRK-ERR-DAY
                               MOVE    6           TO  WRK-ERR-ERRKBN
                           END-IF
                           MOVE    WRK-COMB-HKNCOMBINUM (IDX2)  TO
                                                   WRK-ERR-HKNCOMBINUM
                       END-IF        
      *
                       PERFORM 200122-NYUINACCT-HENSYU-SEC
                   END-IF 
               END-PERFORM
           END-IF    
           .
       20012-IKKATU-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       200121-NYUINACCT-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
      *
      *        保険組合せ追加
               IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SYS-1001-HOSPID TO  COMB-HOSPID
                   MOVE    KEY-O-PTID      TO  COMB-PTID
                   MOVE    WRK-DAY (2 IDX) TO
                                           COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *            （対象外の組合せは１をセット）
                   IF      FLG-HKNCOMBI    =   ZERO
                       MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
      *                医保以外読み飛ばし
                       IF      COMB-HKNNUM     =
                                           WRK-CONS-ROUSAI-HKNNUM OR
                                           WRK-CONS-JIBAI-HKNNUM 
                           MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
      *****************ELSE    
      *********************社保・国保の読み飛ばし
      *********************EVALUATE    WRK-PARA-RECEKBN
      *                        WHEN    "0"
      *                            CONTINUE 
      *                        WHEN    "1"
      *                            IF      COMB-HKNNUM 
      *                                        NOT =   "060" AND "067"
      *                                CONTINUE
      *                            ELSE
      *                                MOVE    "1"       TO
      *                                    WRK-HKNCOMBI-SYOKBN (IDX1)   
      *                            END-IF
      *                        WHEN    "2"
      *                            IF      COMB-HKNNUM
      *                                            =   "060" OR  "067"
      *                                        CONTINUE
      *                            ELSE
      *                                MOVE    "1"       TO
      *                                    WRK-HKNCOMBI-SYOKBN (IDX1)      
      *                            END-IF
      *********************END-EVALUATE
                       END-IF
                   ELSE
                       MOVE    WRK-DAY (2 IDX) TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                       MOVE    "2"     TO  WRK-HKNCOMBI-SYOKBN  (IDX1)
                   END-IF
               END-IF
      *
      *        適用範囲かチェック（範囲外のとき２をセット）
               IF      WRK-DAY (2 IDX)     =
                                       WRK-COMB-HKNCOMBINUM (IDX1)
                   IF      WRK-HKNCOMBI-SYOKBN (IDX1)   NOT =   SPACE
                       CONTINUE                    
                   ELSE     
                       MOVE    WRK-STYMD       TO  WRK-YMD
                       MOVE    IDX             TO  WRK-DD
                       IF      WRK-COMB-TEKSTYMD (IDX1)
                                               <=  WRK-YMD
                       AND     WRK-COMB-TEKEDYMD (IDX1)
                                               >=  WRK-YMD
                           CONTINUE
                       ELSE   
                           MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           MOVE    IDX         TO
                                           WRK-HKNCOMBI-ERR-DAY (IDX1)
                       END-IF
                   END-IF
                   MOVE    10              TO  IDX1
               END-IF            
           END-PERFORM
           .
       200121-NYUINACCT-CHK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計ファイル処理
      *****************************************************************
       200122-NYUINACCT-HENSYU-SEC           SECTION.
      *
           INITIALIZE                   SYS-1005-REC
      *
      *    最終診療日
           MOVE    NACCT-SRYYM         TO  WRK-SRYYM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               IF      NACCT-DAY (IDX)     >   ZERO
                   MOVE    IDX                 TO  WRK-SRYDD
               END-IF
           END-PERFORM
      *
      *   主保険情報読み出し
           IF      COMB-HKNID          NOT =   ZERO
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYS-1001-HOSPID     TO  PTHKN-HOSPID
               MOVE    COMB-PTID           TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               MOVE    COMB-TEKSTYMD       TO  ORC-DBYMD
               PERFORM 910-PTHKNINF-INV-SEC
           ELSE
               INITIALIZE                      PTHKNINF-REC
           END-IF
      *
      *    社保（継続）編集処理
      *****MOVE    SPACE                   TO  WRK-CONTKBN-KEY
      *
      **     IF      COMB-KOHHKNNUM (1)  =   "012"   OR
      **             COMB-KOHHKNNUM (2)  =   "012"   OR
      **             COMB-KOHHKNNUM (3)  =   "012"   OR
      **             COMB-KOHHKNNUM (4)  =   "012" 
      **         IF      COMB-HKNID          NOT =   ZERO
      **             IF      PTHKN-CONTKBN           =   1       
      **                 PERFORM 20011-CONTKBN-CHECK-SEC
      **             END-IF
      **         ELSE
      **             PERFORM 20011-CONTKBN-CHECK-SEC
      **         END-IF
      **** END-IF
      *
      *    公費のレセプト・請求書への出力
      *
           INITIALIZE                  WRK-SEIKYUKBN-KOHI-AREA
                                       FLG-KOH
           MOVE    ZERO                TO  IDY
                                           IDZ    
      *
      *        公費単独のとき全てレセプトに出力
      *****IF      COMB-HKNNUM         =   SPACE
      *****    MOVE    COMB-KOHI-AREA      TO  WRK-SEIKYU-TBL (1)
      *****ELSE        
      *    地方公費のときレセプト請求区分を取得して判別
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4
                       OR      COMB-KOHHKNNUM (IDX)    
                                               =   SPACE
                   IF      COMB-KOHHKNNUM (IDX)(1:1)
                                               >=  "1" AND <=  "8"
                       INITIALIZE                      HKNNUM-REC
                       MOVE    SYS-1001-HOSPID         TO  HKN-HOSPID
                       MOVE    COMB-KOHHKNNUM (IDX)    TO  HKN-HKNNUM
                       MOVE    WRK-SRYYMD              TO  HKN-TEKSTYMD
                                                           HKN-TEKEDYMD
                       MOVE    "00"                    TO  HKN-PAYKBN   
                       MOVE    HKNNUM-REC              TO  MCPDATA-REC
                       PERFORM 900-HKNNUM-INV-SEC
      *
                       IF      COMB-HKNNUM         =   "060"   OR  "067" 
                           IF      HKN-RECESKYKBN      =   "1" OR  "3"
                               MOVE    2                   TO  IDX1
                           ELSE
                               MOVE    1                   TO  IDX1
                           END-IF
                       ELSE                
                           IF      HKN-RECESKYKBN      =   "2" OR  "3"
                               MOVE    2                   TO  IDX1
                           ELSE
                               MOVE    1                   TO  IDX1
                           END-IF
                       END-IF
                   ELSE
                       MOVE    1                      TO  IDX1
                   END-IF       
      *
                   IF     IDX1                =   1
                       ADD     1                      TO  IDY    
                       MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDY)
                       MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDY)
                   ELSE
                       ADD     1                      TO  IDZ    
                       MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDZ)
                       MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDZ)
                       MOVE    1                      TO  FLG-KOH 
                   END-IF  
               END-PERFORM
      ****END-IF           
      *
      *    レセプト明細書
           MOVE    "1"                 TO  WRK-SYOKBN
      *****IF      WRK-CONTKBN-KEY     NOT =   SPACE
      *        社保（継続）＋生保のとき
      *         MOVE    WRK-CONTKBN-KEY     TO  WRK-RECE61-KEY
      *         MOVE    WRK-RECE61-KEY      TO  RECE61-KEY 
      *         PERFORM 900-RECE61-READ-SEC
      *         IF      FLG-RECE61          =   ZERO
      *             MOVE    RECE61-REC          TO  WRK-RECE61-REC 
      *             PERFORM 20011-RECE61-REWRITE-SEC
      *         END-IF
      *****ELSE
      *        社保（継続）＋生保以外のとき
               PERFORM 20011-FILE-HENSYU-SEC
      *****END-IF   
      *
      *    医療費請求書
           IF      FLG-KOH             =   1  
               MOVE    "2"             TO  WRK-SYOKBN
               PERFORM 20011-FILE-HENSYU-SEC
           END-IF   
           .
       20011-FILE-SYORI-EXT.
           EXIT. 
           .
      *****************************************************************
      *    社保（継続）編集処理   （生保のときのみ）
      *****************************************************************
       20011-CONTKBN-CHECK-SEC         SECTION.
      *
      *    既に社保（継続）のデータがあるか検索
           INITIALIZE                      WRK-RECE61-REC
           MOVE    SYS-1001-HOSPID     TO  WRK-RECE61-HOSPID
           IF      WRK-PARA-SYOKBN     =   "1"
               MOVE    SYS-2001-SRYYM      TO  WRK-RECE61-SRYYM
           ELSE
               MOVE    SYS-2002-SRYYM      TO  WRK-RECE61-SRYYM
           END-IF
           MOVE    ACCT-NYUGAIKBN      TO  WRK-RECE61-NYUGAIKBN
           MOVE    COMB-PTID           TO  WRK-RECE61-PTID
           MOVE    WRK-RECE61-KEY      TO  RECE61-KEY
           START   RECE61-FILE         KEY IS >=  RECE61-KEY
               INVALID 
                   MOVE    1               TO  FLG-RECE61
               NOT INVALID 
                   PERFORM 910-RECE61-READ-N-SEC 
           END-START
      *     
           PERFORM                 UNTIL   FLG-RECE61  =   1
                                   OR      WRK-RECE61-HOSPID 
                                               NOT =   RECE61-HOSPID
                                   OR      WRK-RECE61-SRYYM 
                                               NOT =   RECE61-SRYYM
                                   OR      WRK-RECE61-NYUGAIKBN 
                                               NOT =   RECE61-NYUGAIKBN
                                   OR      WRK-RECE61-PTID 
                                               NOT =   RECE61-PTID
      *        生保があり適用年月日が同じデータがあるとき
               IF    ( RECE61-KOHHKNNUM-O (1)  =   "012"   OR  
                       RECE61-KOHHKNNUM-O (2)  =   "012"   OR
                       RECE61-KOHHKNNUM-O (3)  =   "012"   OR
                       RECE61-KOHHKNNUM-O (4)  =   "012"              )
               AND   ( RECE61-TEKSTYMDX (1)    <=  COMB-TEKSTYMD   AND
                       RECE61-TEKEDYMDX (1)    >=  COMB-TEKSTYMD      )
      *            公費単独のとき
      *****        IF      RECE61-HKNID        =   ZERO
                   IF      RECE61-HKNNUM       =   SPACE
                       DELETE  RECE61-FILE RECORD
                   ELSE     
      *            社保（継続）のとき対象データとする
                       IF      RECE61-CONTKBN      =   "1"
                           MOVE    RECE61-KEY      TO  WRK-CONTKBN-KEY     
                       END-IF      
                   END-IF
                   MOVE    1                   TO  FLG-RECE61
               ELSE
                   PERFORM 910-RECE61-READ-N-SEC 
               END-IF
           END-PERFORM
           .
       20011-CONTKBN-CHECK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    ファイル編集処理
      *****************************************************************
       20011-FILE-HENSYU-SEC              SECTION.
      *
      *    公費で不明分は出力しない
           IF      WRK-SYOKBN         =   "2"
           AND     WRK-TEISEIKBN      =   1
               GO  TO  20011-FILE-HENSYU-EXT
           END-IF
      *     
           INITIALIZE                      WRK-RECE61-REC
           MOVE    SYS-1001-HOSPID     TO  WRK-RECE61-HOSPID
           IF      WRK-PARA-SYOKBN     =   "1"
               MOVE    SYS-2001-SRYYM      TO  WRK-RECE61-SRYYM
           ELSE
               MOVE    SYS-2002-SRYYM      TO  WRK-RECE61-SRYYM
           END-IF
           MOVE    "1"                 TO  WRK-RECE61-NYUGAIKBN
           MOVE    COMB-PTID           TO  WRK-RECE61-PTID
           MOVE    COMB-HKNNUM         TO  WRK-RECE61-HKNNUM
           MOVE    PTHKN-HONKZKKBN     TO  WRK-RECE61-HONKZKKBN
           IF      COMB-HKNNUM         =   SPACE
               MOVE    ZERO                TO  WRK-RECE61-KOHID
           ELSE
               IF      COMB-KOH1HKNNUM     =   "027"
                   MOVE    COMB-KOH1ID         TO  WRK-RECE61-KOHID
                   MOVE    "0"                 TO  WRK-RECE61-HONKZKKBN
               ELSE
                   MOVE    ZERO                TO  WRK-RECE61-KOHID
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "002" 
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             END-EVALUATE
                       WHEN    "060" 
                           IF     (WRK-NENREI  >=   70)  AND
                                  (WRK-NENREI  <=   74)
                               EVALUATE   PTHKN-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE    "0" 
                                               TO  WRK-RECE61-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE    "0"
                                               TO  WRK-RECE61-HONKZKKBN
                              END-EVALUATE
                          END-IF     
                      WHEN    OTHER 
                          EVALUATE   PTHKN-HOJOKBN
                             WHEN   "9"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             WHEN   "8"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                         END-EVALUATE
                   END-EVALUATE
               END-IF 
           END-IF
      *
           MOVE    PTHKN-HKNJANUM      TO  WRK-RECE61-HKNJANUM       
           MOVE    ZERO                TO  WRK-RECE61-TEKSTYMD 
      *****MOVE    PTHKN-HONKZKKBN     TO  WRK-RECE61-HONKZKKBN
           EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO  WRK-RECE61-HOJOKBN
                   END-IF
           END-EVALUATE             
           MOVE    SPACE               TO WRK-RECE61-RECEKA
      *
      *    レセのとき地方公費のみの場合は出力しない
           IF      WRK-SYOKBN             =   "1"        
               IF      WRK-RECE61-HKNNUM      =   SPACE
               AND     WRK-RECE61-KOHID       =   ZERO
               AND     WRK-SEIKYU-KOHID (1 1) =   ZERO
                   GO  TO  20011-FILE-HENSYU-EXT
               END-IF
           END-IF 
      *   
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-RECE61-KEY      TO  RECE61-KEY 
                   PERFORM 900-RECE61-READ-SEC
                   IF      FLG-RECE61          =   ZERO
                       MOVE    RECE61-REC          TO  WRK-RECE61-REC 
                       PERFORM 20011-RECE61-REWRITE-SEC
                   ELSE
                       PERFORM 20011-RECE61-WRITE-SEC
                   END-IF
               WHEN    "2"
                   MOVE    WRK-RECE61-KEY      TO  RECE61X-KEY 
                   PERFORM 900-RECE61X-READ-SEC
                   IF      FLG-RECE61X         =   ZERO
                       MOVE    RECE61X-REC         TO  WRK-RECE61-REC 
                       PERFORM 20011-RECE61-REWRITE-SEC
                   ELSE
                       PERFORM 20011-RECE61-WRITE-SEC
                   END-IF
           END-EVALUATE
           .
       20011-FILE-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       20011-RECE61-WRITE-SEC              SECTION.
      *
           PERFORM 20012-KOHID-HENSYU-SEC
           MOVE    COMB-HKNCOMBINUM   TO  WRK-RECE61-HKNCOMBI-O (1)
           IF      WRK-DBKBN          =   "2"
               CONTINUE
           ELSE              
               MOVE    ACCT-SRYKA     TO  WRK-RECE61-SRYKA      (1)
           END-IF
           IF      WRK-DBKBN          =   "2"
      ***      MOVE    WRK-DAY-TBL (2)    TO  WRK-RECE61-DAY-AREA
               PERFORM   VARYING   IDX3    FROM    1   BY  1
                           UNTIL   IDX3    >      31
                 IF      WRK-DAY (1 IDX3)     >   ZERO    AND
                         WRK-DAY (2 IDX3)     >   ZERO
                    MOVE   WRK-DAY(2 IDX3)  TO  WRK-RECE61-DAY(IDX3)
                 END-IF
               END-PERFORM
           END-IF
           MOVE    PTHKN-TEKSTYMD     TO  WRK-RECE61-TEKSTYMDX  (1)
           MOVE    PTHKN-TEKEDYMD     TO  WRK-RECE61-TEKEDYMDX  (1)
           MOVE    PTHKN-CONTKBN      TO  WRK-RECE61-CONTKBN
           MOVE    PTHKN-HKNID        TO  WRK-RECE61-HKNID
           MOVE    WRK-TEISEIKBN      TO  WRK-RECE61-TEISEIKBN
           IF      WRK-TEISEIKBN      =   "1"
               MOVE    WRK-ERR-ERRKBN     TO  WRK-RECE61-ERR-ERRKBN
               MOVE    WRK-ERR-DAY        TO  WRK-RECE61-ERR-DAY
               MOVE    WRK-ERR-HKNCOMBINUM 
                                          TO  WRK-RECE61-ERR-HKNCOMBINUM
               MOVE    WRK-ERR-TEKSTYMD   TO  WRK-RECE61-ERR-TEKSTYMD
               MOVE    WRK-ERR-TEKEDYMD   TO  WRK-RECE61-ERR-TEKEDYMD
           END-IF 
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-RECE61-REC      TO  RECE61-REC
                   WRITE   RECE61-REC
                   ADD     1                   TO  CNT-OUT1
               WHEN    "2"
                   MOVE    WRK-RECE61-REC      TO  RECE61X-REC
                   WRITE   RECE61X-REC
                   ADD     1                   TO  CNT-OUT2
           END-EVALUATE
      *
           .
       20011-RECE61-WRITE-EXT.
           EXIT.
      *            
070100*****************************************************************
070200*    出力処理
070300*****************************************************************
070400 20011-RECE61-REWRITE-SEC            SECTION.
      *
           PERFORM 20012-KOHID-HENSYU-SEC
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      WRK-RECE61-HKNCOMBI-O  (IDX)   =   ZERO
                   MOVE    COMB-HKNCOMBINUM    TO
                                       WRK-RECE61-HKNCOMBI-O (IDX)
                   MOVE    10                   TO  IDX
               ELSE
                   IF      COMB-HKNCOMBINUM    =
                                       WRK-RECE61-HKNCOMBI-O (IDX)
                       MOVE    10                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-DBKBN               =   "2"
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       20
                   IF      WRK-RECE61-SRYKA    (IDX)       =   SPACE
                       MOVE    ACCT-SRYKA  TO  WRK-RECE61-SRYKA (IDX)
                       MOVE    20          TO  IDX
                   ELSE
                       IF      WRK-RECE61-SRYKA  (IDX) =   ACCT-SRYKA
                           MOVE    20          TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      WRK-RECE61-TEKSTYMDX (IDX)  =   SPACE
                   MOVE    PTHKN-TEKSTYMD  TO
                                           WRK-RECE61-TEKSTYMDX (IDX)
                   MOVE    PTHKN-TEKEDYMD  TO
                                           WRK-RECE61-TEKEDYMDX (IDX)
                   MOVE    PTHKN-HKNID     TO  WRK-RECE61-HKNID
                   MOVE    10              TO  IDX
               ELSE
                   IF      WRK-RECE61-TEKSTYMDX(IDX)
                                           >=  PTHKN-TEKSTYMD
                       MOVE    10                  TO  IDX
                   END-IF    
               END-IF    
           END-PERFORM
      *
           IF      WRK-DBKBN          =   "2"
      ***      MOVE    WRK-DAY-TBL (2)    TO  WRK-RECE61-DAY-AREA
               PERFORM   VARYING   IDX3    FROM    1   BY  1
                           UNTIL   IDX3    >      31
                 IF      WRK-DAY (1 IDX3)     >   ZERO    AND
                         WRK-DAY (2 IDX3)     >   ZERO
                    MOVE   WRK-DAY(2 IDX3)  TO  WRK-RECE61-DAY(IDX3)
                 END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-TEISEIKBN           =   "1"
               IF      WRK-RECE61-TEISEIKBN    =   SPACE
                   MOVE    WRK-ERR-ERRKBN  TO  WRK-RECE61-ERR-ERRKBN
                   MOVE    WRK-ERR-DAY     TO  WRK-RECE61-ERR-DAY
                   MOVE    WRK-ERR-HKNCOMBINUM TO 
                                           WRK-RECE61-ERR-HKNCOMBINUM
                   MOVE    WRK-ERR-TEKSTYMD
                                           TO  WRK-RECE61-ERR-TEKSTYMD
                   MOVE    WRK-ERR-TEKEDYMD
                                           TO  WRK-RECE61-ERR-TEKEDYMD
               END-IF            
               MOVE    "1"                 TO  WRK-RECE61-TEISEIKBN     
           END-IF          
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-RECE61-REC      TO  RECE61-REC
                   REWRITE RECE61-REC
               WHEN    "2"
                   MOVE    WRK-RECE61-REC      TO  RECE61X-REC
                   REWRITE RECE61X-REC
           END-EVALUATE
076800     .
076900 20011-RECE61-REWRITE-EXT.
077000     EXIT.
      * 
      *****************************************************************
      *    公費個別処理
      *****************************************************************
       20012-KOHID-HENSYU-SEC              SECTION.
      *
           MOVE    ZERO                    TO  TBL-MAX
                                               FLG-ADDKBN 
      *
           MOVE    WRK-RECE61-KOHI-AREA    TO  WRK-KOHI-AREA
      *
           MOVE    WRK-SYOKBN              TO  IDX1   
      *
      *    公費の追加 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               IF      WRK-SEIKYU-KOHID (IDX1 IDX)    =   ZERO
                   MOVE    4                   TO  IDX
               ELSE    
      *            老人＋原爆のとき  
                   IF    ( WRK-RECE61-KOHID    NOT =   ZERO  )
                   AND   ( WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)  
                                                   =   "019" )
                       IF      WRK-RECE61-HKNNUM   =   "060"   OR  "067" 
                           IF      SYS-2005-RJNGNBKKBN =   "1" OR  ZERO
                               MOVE    WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)
                                               TO  WRK-RECE61-ETC-HKNNUM
                           ELSE
                               PERFORM 200121-KOHID-TBL-HENSYU-SEC
                           END-IF
                       ELSE                
                           IF      SYS-2005-RJNGNBKKBN =   "2" OR  ZERO
                               MOVE    WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)
                                               TO  WRK-RECE61-ETC-HKNNUM
                           ELSE
                               PERFORM 200121-KOHID-TBL-HENSYU-SEC
                           END-IF
                       END-IF
                   ELSE
                       PERFORM 200121-KOHID-TBL-HENSYU-SEC
                   END-IF
               END-IF      
           END-PERFORM
      *
      *    公費を追加しなかったとき
           IF      FLG-ADDKBN              =   ZERO
               GO  TO  20012-KOHID-HENSYU-EXT
           END-IF
      *
      *    優先順位に並べ替え
           INITIALIZE                      WRK-RECE61-KOHI-AREA
           MOVE    ZERO                    TO  TBL-MAX
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       22
                   OR      TBL-MAX     =   4 
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       4
                   IF      WRK-SRT-KOHHKNNUM   (IDX)
                                       =   WRK-KOHHKNNUM-O (IDY)
                       ADD     1               TO  TBL-MAX
                       MOVE    WRK-KOHI-TBL    (IDY)   TO
                                           WRK-RECE61-KOHI-TBL (TBL-MAX)
                       INITIALIZE          WRK-KOHI-TBL    (IDY)
                       MOVE    4               TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    すべて並べ替えた時 
           IF      WRK-KOHHKNNUM-O (1)     =   SPACE
           AND     WRK-KOHHKNNUM-O (2)     =   SPACE
           AND     WRK-KOHHKNNUM-O (3)     =   SPACE
           AND     WRK-KOHHKNNUM-O (4)     =   SPACE
               GO  TO  20012-KOHID-HENSYU-EXT
           END-IF
      *
      *    地方公費の並び替え
           PERFORM VARYING IDX FROM    1    BY  1
                   UNTIL   IDX >       3
               COMPUTE IDX1    =   IDX     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       4
                   IF      WRK-KOHHKNNUM-O (IDX)
                                           >   WRK-KOHHKNNUM-O (IDY)
                   OR      WRK-KOHHKNNUM-O (IDX) =   SPACE 
                       MOVE    WRK-KOHI-TBL(IDX) TO  WRK-KOHI
                       MOVE    WRK-KOHI-TBL(IDY) TO  WRK-KOHI-TBL(IDX)
                       MOVE    WRK-KOHI          TO  WRK-KOHI-TBL(IDY)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    地方公費の追加
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   4
                   OR      TBL-MAX     =   4 
               IF      WRK-KOHID-O (IDX)   >   ZERO
                   ADD     1                   TO  TBL-MAX
                   MOVE    WRK-KOHI-TBL (IDX)  TO 
                                           WRK-RECE61-KOHI-TBL(TBL-MAX)
               END-IF    
           END-PERFORM
076800     .
076900 20012-KOHID-HENSYU-EXT.
077000     EXIT.
      * 
      *****************************************************************
      *    公費情報退避処理
      *****************************************************************
       200121-KOHID-TBL-HENSYU-SEC     SECTION.
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       4
               IF      WRK-KOHID-O (IDY)   =   ZERO
                   MOVE    WRK-SEIKYU-KOHID    (IDX1 IDX)
                                           TO  WRK-KOHID-O    (IDY)
                   MOVE    WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)
                                           TO  WRK-KOHHKNNUM-O(IDY)
                   MOVE    1               TO  FLG-ADDKBN
                   MOVE    4               TO  IDY
               ELSE 
                   IF      WRK-SEIKYU-KOHID (IDX1 IDX)
                                       =   WRK-KOHID-O  (IDY)
                       MOVE    4           TO  IDY
                   END-IF
               END-IF
           END-PERFORM
           .
       200121-KOHID-TBL-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    個別処理
      *****************************************************************
       2002-KOBETU-SYORI-SEC               SECTION.
      *
      *    レセプト管理削除処理（個別分）
           INITIALIZE                  RECE07-REC
           MOVE    "HC06"              TO  RECE07-PRTID
           MOVE    "1"                 TO  RECE07-NYUGAIKBN
           MOVE    RECE07-REC          TO  MCPDATA-REC
           MOVE    "RECEKANRI-DEL2"    TO  ORC-DBPATH
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC      
           IF      MCP-RC           NOT =   ZERO
               DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
               MOVE
                   "レセプト管理削除処理（個別分）でエラーとなりました"
                                           TO    WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  2002-KOBETU-SYORI-EXT
           END-IF
      *
      *    レセプト削除処理（個別分）
           INITIALIZE                  RECE08-REC
           MOVE    "HC06"              TO  RECE08-PRTID
           MOVE    "1"                 TO  RECE08-NYUGAIKBN
           MOVE    RECE08-REC          TO  MCPDATA-REC
           MOVE    "RECEPRT-DEL2"      TO  ORC-DBPATH
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC      
           IF      MCP-RC           NOT =   ZERO
               DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
               MOVE
                   "レセプト削除処理（個別分）でエラーとなりました"
                                           TO    WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  2002-KOBETU-SYORI-EXT
           END-IF
      *
      *    レセプト指示（個別指示）コード読み込み
           INITIALIZE                      SYS-2002-REC
           MOVE    "2002"              TO  SYS-2002-KANRICD
           MOVE    ZERO                TO  ORC-DBYMD
           MOVE    SYS-2002-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM 20021-KOBETU-HENSYU-SEC
                              UNTIL       FLG-SYSKANRI    =   1
                              OR          FLG-END         =   1
      *   
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
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
       20021-KOBETU-HENSYU-SEC              SECTION.
      *
           MOVE    MCPDATA-REC         TO  SYS-2002-REC
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
      *    患者年齢計算
           PERFORM 500-AGECHK-SEC
      *
      *    診療年月の請求管理削除処理
           INITIALIZE                  RECE10-REC
           MOVE    SYS-1001-HOSPID     TO  RECE10-HOSPID
           MOVE    SYS-2002-PTID       TO  RECE10-PTID
           MOVE    SYS-2002-SRYYM      TO  RECE10-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  RECE10-NYUGAIKBN
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "SEIKYU-DEL4"       TO  ORC-DBPATH
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC      
           IF      MCP-RC           NOT =   ZERO
               DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
               MOVE
                   "請求管理削除処理（個別分）でエラーとなりました"
                                           TO    WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  20021-KOBETU-HENSYU-EXT
           END-IF
      *
      *    診療年月の公費請求書削除処理
           INITIALIZE                  KOHSKY-REC
           MOVE    SYS-1001-HOSPID     TO  KOHSKY-HOSPID
           MOVE    SYS-2002-PTID       TO  KOHSKY-PTID
           MOVE    SYS-2002-SRYYM      TO  KOHSKY-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  KOHSKY-NYUGAIKBN
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "KOHSKY-DEL2"       TO  ORC-DBPATH
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC           NOT =   ZERO
               DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
               MOVE
                   "公費請求書削除処理（個別分）でエラーとなりました"
                                           TO    WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  20021-KOBETU-HENSYU-EXT
           END-IF
      *
      *        診療会計読み込み
           MOVE    "1"                 TO  WRK-DBKBN 
      *      
           INITIALIZE                  SRYACCT-REC
           MOVE    SYS-1001-HOSPID     TO  ACCT-HOSPID
           MOVE    SYS-2002-PTID       TO  ACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY30"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY30"     TO  ORC-DBPATH
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
               PERFORM 20011-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYACCT-KEY30"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *        入院会計読み込み
           MOVE    "2"                 TO  WRK-DBKBN 
      *      
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
      *     
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
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL       FLG-NYUINACCT     =   1
               PERFORM 20012-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY14"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           PERFORM 900-SYSKANRI-READ-SEC
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       20011-HENSYU-SEC                SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM             UNTIL   FLG-SRYACCT     =   1 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *        保険組合せ読込み
               INITIALIZE                  HKNCOMBI-REC
               MOVE    SYS-1001-HOSPID     TO  COMB-HOSPID
               MOVE    ACCT-PTID           TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM  910-HKNCOMBI-INV-SEC
      *
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               IF      FLG-HKNCOMBI        =   ZERO
                   IF      COMB-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM OR 
                                               WRK-CONS-JIBAI-HKNNUM 
                       MOVE    "1"             TO  WRK-ROUSAIKBN
                   ELSE                                         
                       IF      COMB-TEKSTYMD   <=  WRK-STYMD
                       AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                           MOVE    SPACE           TO  WRK-UPDKBN
                       ELSE
                           MOVE    "1"             TO  WRK-UPDKBN
                       END-IF
                   END-IF    
               ELSE
                   MOVE    "1"                 TO  WRK-TEISEIKBN        
                   MOVE    1                   TO  WRK-ERR-ERRKBN
                   MOVE    ACCT-HKNCOMBI       TO  WRK-ERR-HKNCOMBINUM        
               END-IF    
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      KEY-NEW     NOT =   KEY-OLD
      *************社保・国保の読み飛ばし
      *            EVALUATE    WRK-PARA-RECEKBN
      *                WHEN    "0"
      *                    PERFORM 20011-FILE-SYORI-SEC 
      *                WHEN    "1"
      *                    IF      COMB-HKNNUM NOT =   "060" AND "067"
      *                        PERFORM 20011-FILE-SYORI-SEC 
      *                    END-IF
      *                WHEN    "2"
      *                    IF      COMB-HKNNUM     =   "060" OR  "067"
      *                        PERFORM 20011-FILE-SYORI-SEC 
      *                    END-IF
      *************END-EVALUATE
                   PERFORM 20011-FILE-SYORI-SEC 
      *
                   IF      WRK-UPDKBN          =   SPACE                   
      *                同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM     UNTIL   FLG-SRYACCT     =   1 
                                   OR      KEY-NEW     NOT =   KEY-OLD
                           MOVE    "SRYACCT-KEY30"     TO  ORC-DBPATH
                           PERFORM 900-SRYACCT-READ-SEC
                        END-PERFORM
                   ELSE
                       MOVE    "SRYACCT-KEY30"     TO  ORC-DBPATH
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM         
      *
           .
       20011-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    入院会計個別編集処理
      *****************************************************************
       20012-HENSYU-SEC              SECTION.
      *     
           MOVE    KEY-NEW             TO  KEY-OLD
           INITIALIZE                      WRK-DAY-AREA
                                           WRK-HKNCOMBI-TABLE
      *
           PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                           OR      KEY-NEW     NOT =   KEY-OLD
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
      *    入院会計より保険組合せ読込     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               IF      WRK-DAY (1 IDX)     >   ZERO    AND
                       WRK-DAY (2 IDX)     >   ZERO
                   PERFORM 200121-NYUINACCT-CHK-SEC
               END-IF
           END-PERFORM        
      *
      *    入院会計の保険組合せを追加
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       10
                   OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
               IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "1"
                   CONTINUE
               ELSE    
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
                   MOVE    WRK-HKNCOMBI-REC (IDX2)
                                           TO  HKNCOMBI-REC    
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                       MOVE    "1"         TO  WRK-TEISEIKBN
                       IF      WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                   =   ZERO
                           MOVE    1           TO  WRK-ERR-ERRKBN
                       ELSE
                           MOVE    WRK-HKNCOMBI-ERR-DAY (IDX2)
                                               TO  WRK-ERR-DAY
                           MOVE    6           TO  WRK-ERR-ERRKBN
                       END-IF
                       MOVE    WRK-COMB-HKNCOMBINUM (IDX2)  TO
                                               WRK-ERR-HKNCOMBINUM
                   END-IF        
                   PERFORM 200122-NYUINACCT-HENSYU-SEC
               END-IF 
           END-PERFORM
           .
       20012-HENSYU-EXT.
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
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE61-FILE
           CLOSE   RECE61X-FILE
      *
           IF      CNT-OUT1            =   ZERO
           AND     CNT-OUT2            =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
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
           DISPLAY "RECE61  CNT " CNT-OUT1 
           DISPLAY "RECE61X CNT " CNT-OUT2  
           DISPLAY "*** ORCR0610 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *        患者年齢計算処理
      *****************************************************************
       500-AGECHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-NENREI
      *
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    SYS-1001-HOSPID     TO  AGECHK-HOSPID
           MOVE    PTINF-PTID          TO  AGECHK-PTID
           MOVE    WRK-STYMD (1:6)     TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"            USING
                                           ORCSAGECHKAREA
           IF      AGECHK-RC           =   ZERO
               MOVE  AGECHK-NENREI         TO  WRK-NENREI
           END-IF
      *
           .
       500-AGECHK-EXT.
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
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
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
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
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
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    NACCT-PTID          TO  KEY-N-PTID 
      *********MOVE    NACCT-HKNCOMBI      TO  KEY-N-HKNCOMBI
      *********MOVE    NACCT-SRYKA         TO  KEY-N-SRYKA
               MOVE    SPACE               TO  KEY-N-SRYKA
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
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
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
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY3"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTHKNINF-KEY3"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY3"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号（主キー）読込処理
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
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
           CALL    "ORCMCPSUB"          USING
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
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
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
      *    レセプト管理読込（キー）
      *****************************************************************
       900-RECE07-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    WRK-PATH            TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-RECE07-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-RECE61-READ-SEC             SECTION.
      *
           READ    RECE61-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE61
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE61
           END-READ
           .
       900-RECE61-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込（順読み）
      *****************************************************************
       910-RECE61-READ-N-SEC             SECTION.
      *
           READ    RECE61-FILE     NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE61
               NOT AT  END
                   MOVE    ZERO        TO  FLG-RECE61
           END-READ
           .
       910-RECE61-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療費請求書読込
      *****************************************************************
       900-RECE61X-READ-SEC            SECTION.
      *
           READ    RECE61X-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE61X
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE61X
           END-READ
           .
       900-RECE61X-READ-EXT.
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
       900-DBCLOSE-EXT.
           EXIT.
