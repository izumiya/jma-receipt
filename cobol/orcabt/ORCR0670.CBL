      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard RECE63ipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard RECE63ipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have RECE63ived a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCR0670.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : 病名編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/10    NACL−藤原　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *  01.00.02    NACL-藤原    03/07/02  主病名編集フラグによる編集の追加
      *  01.00.03    NACL-藤原    04/02/01  疑いフラグからのコードの編集
      *                                     病名コード２１に変更
      *  01.01.02    NACL-藤原    05/02/23  予約コード入力による自院病名
      *                                     に病名編集フラグをセットする
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
      *
           SELECT  RECE63-FILE     ASSIGN  RECE63PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE63-KEY
                                   FILE    STATUS  IS  STS-RECE63.
      *
           SELECT  RECE63W-FILE    ASSIGN  RECE63PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE63W-KEY
                                   FILE    STATUS  IS  STS-RECE63.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE62-FILE.
       01  RECE62-REC. 
           COPY    "CPRCF062.INC".
      *
       FD  RECE63-FILE.
       01  RECE63-REC. 
           COPY    "CPRCF063.INC".
      *
       FD  RECE63W-FILE.
       01  RECE63W-REC. 
           COPY    "CPRCF063.INC"  REPLACING  //RECE63//
                                   BY         //RECE63W//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE62//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE63//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03                      PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE62          PIC X(02).
           03  STS-RECE63          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(06).
           03  CNT-OUT             PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03). 
           03  IDY                 PIC 9(03). 
           03  IDZ                 PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM  PIC  X(06).
               05  WRK-PARA-TERMID PIC  X(16).
               05  WRK-PARA-SYSYMD PIC  X(08).
               05  WRK-PARA-JOBID  PIC  9(07).
               05  WRK-PARA-SHELLID
                                   PIC  X(08).
           03  WRK-PATH            PIC X(64). 
           03  WRK-YM              PIC 9(06).
           03  WRK-RENNUM          PIC 9(02).
      *
           03  WRK-RECEERR         PIC X(200). 
      *    傷病名編集
           03  WRK-BYOMEI          PIC X(150).
           03  WRK-BYOMEIW         PIC X(150).
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01). 
      *    病名文字数         
           03  WRK-MOJISU          PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-SRYYMD    PIC 9(08).
           03  KEY-OLD.
               05  KEY-O-SRYYMD    PIC 9(08).
      *
      *    疑い編集
       01  WRK-UTAGAI-AREA.
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4.       
      *
       01  WRK-CONS-AREA.
      *    疑い
           03  WRK-CONS-BYOMEICD-UTAGAI
                                   PIC X(07)   VALUE   "ZZZ8002".   
      *    急性
           03  WRK-CONS-BYOMEICD-KYUSEI
                                   PIC X(07)   VALUE   "ZZZ4012".   
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
      *     
           COPY    "CPSK2005.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
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
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
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
           MOVE    "ORCR0670"      TO  JOB-PGID
           MOVE    "傷病名編集"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECE620"           TO  RECE62PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE62PARA-TERMID
      *
           MOVE    "RECE630"           TO  RECE63PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE63PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    OUTPUT              RECE63W-FILE
           CLOSE                       RECE63W-FILE
      *
           OPEN    INPUT               RECE62-FILE    
           OPEN    I-O                 RECE63-FILE
      *     
      *    レセプト編集区分情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
                
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-BYOMEIHENKBN1   =   SPACE
               MOVE    ZERO             TO  SYS-2005-BYOMEIHENKBN1
           END-IF
           IF      SYS-2005-SYUBYOHENFLG    =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYUBYOHENFLG
           END-IF
      *
      *        レセプト明細０２読み込み
           PERFORM 900-RECE62-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
      *    患者病名読み込み（患者ＩＤ、診療年月日、診療科順）
           INITIALIZE                  PTBYOMEI-REC
           MOVE    RECE62-HOSPID       TO  PTBYO-HOSPID
           MOVE    RECE62-PTID         TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH 
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM 2001-PTBYO-HENSYU-SEC
                           UNTIL       FLG-PTBYOMEI     =   1
      *    カーソルクロース
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH 
           CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
           PERFORM 900-RECE62-READ-SEC      
           .
       200-MAIN-EXT.
           EXIT. 
      *            
      * 
      *****************************************************************
      *    患者病名診療年月日別処理
      *****************************************************************
       2001-PTBYO-HENSYU-SEC                SECTION.
      *
           MOVE    PTBYO-SRYYMD            TO  KEY-N-SRYYMD
           MOVE    ZERO                    TO  WRK-RENNUM
      *
           MOVE    KEY-NEW                 TO  KEY-OLD   
           PERFORM             UNTIL       FLG-PTBYOMEI    =   1
                               OR          KEY-NEW     NOT =   KEY-OLD
               PERFORM 2001-PTBYO-CHK-SEC
      * 
               MOVE    "PTBYOMEI-KEY3"         TO  ORC-DBPATH 
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM    
           .
       2001-PTBYO-HENSYU-EXT.
           EXIT. 
      *            
070100*****************************************************************
070200*    患者病名チェック処理
070300*****************************************************************
070400 2001-PTBYO-CHK-SEC         SECTION.
      *
      *    診療科
           MOVE    ZERO                TO  FLG-CHK 
           IF      SYS-2005-BYOMEIHENKBN1      =   ZERO     
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       20
                       OR      RECE62-SRYKA(IDX)   =   SPACE
                   IF      PTBYO-SRYKA          =   RECE62-SRYKA (IDX)
                       MOVE    1                    TO  FLG-CHK
                       MOVE    20                   TO  IDX
                   END-IF
               END-PERFORM
               IF      FLG-CHK              =   ZERO
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF        
      *
      *    診療開始日
           IF      PTBYO-SRYYMD(1:6)    =   RECE62-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    <   RECE62-SRYYM 
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF      PTBYO-TENKIYMD(1:6)  >=  RECE62-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  2001-PTBYO-CHK-EXT
                       END-IF
                   END-IF       
               ELSE
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    入外区分
           IF      PTBYO-NYUGAIKBN      =   SPACE
               CONTINUE
           ELSE
               IF      PTBYO-NYUGAIKBN  NOT =   RECE62-NYUGAIKBN
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    保険組合せ
           IF      PTBYO-HKNCOMBI     =   ZERO
               CONTINUE
           ELSE 
               MOVE    ZERO            TO  IDZ  
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       10
                       OR      RECE62-HKNCOMBI (IDX)   =   ZERO 
                   IF      PTBYO-HKNCOMBI   =
                                            RECE62-HKNCOMBI (IDX)
                       MOVE    IDX             TO  IDZ
                       MOVE    10              TO  IDX
                   END-IF
               END-PERFORM
               IF      IDZ            >   ZERO          
                   CONTINUE
               ELSE
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    レセプト表示月
           IF      PTBYO-REZEFLG          =   "1"
              GO  TO  2001-PTBYO-CHK-EXT
           END-IF
      *
           IF      PTBYO-REZEMM           >   ZERO
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    PTBYO-SRYYMD        TO  LNK-DAY6-KIJUN 
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN  =   PTBYO-REZEMM    -   1    
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-YM
               IF      WRK-YM              >=  RECE62-SRYYM
                   CONTINUE
               ELSE
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF                    
      *                               
           PERFORM 20011-RECE63-WRITE-SEC
           .
       2001-PTBYO-CHK-EXT.
           EXIT. 
      *            
070100*****************************************************************
070200*    レセプト明細０３編集処理
070300*****************************************************************
070400 20011-RECE63-WRITE-SEC              SECTION.
      *
      *    疑いフラグより病名を編集
           MOVE    SPACE               TO  WRK-HENFLG1
                                           WRK-HENFLG2
      *       
           IF      PTBYO-UTAGAIFLG NOT =   SPACE
               COMPUTE WRK-MOJISU   =   PTBYO-BYOMEIMOJI   *   2
               IF      PTBYO-UTAGAIFLG  =   "1" OR  "3"
                   IF      WRK-MOJISU       <   8
                       MOVE    1                TO  IDZ
                   ELSE    
                       COMPUTE IDZ     =   WRK-MOJISU    -   7
                   END-IF    
                   PERFORM VARYING IDX FROM    IDZ BY  2
                           UNTIL   IDX >       WRK-MOJISU
                       IF      PTBYO-BYOMEI (IDX:2)      =   "疑"
                           MOVE    "1"              TO  WRK-HENFLG1
                           MOVE    WRK-MOJISU       TO  IDX
                       END-IF
                   END-PERFORM
               END-IF                
               IF      PTBYO-UTAGAIFLG =   "2" OR  "3"
                   PERFORM VARYING IDX FROM    1   BY  2
                           UNTIL   IDX >       WRK-MOJISU
                       IF      PTBYO-BYOMEI (IDX:4)      =   "急性"
                           MOVE    "1"              TO  WRK-HENFLG2
                           MOVE    WRK-MOJISU       TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *     
      *    病名文字数を計算
           COMPUTE WRK-MOJISU          =   PTBYO-BYOMEIMOJI   *   2
           MOVE    PTBYO-BYOMEI      TO  WRK-BYOMEI
      *
      *    疑いフラグがセットされているとき
           IF    ( PTBYO-UTAGAIFLG   =   "1" OR  "3" )
           AND   ( WRK-HENFLG1         =   SPACE     )                         
               MOVE    SPACE               TO  WRK-BYOMEI
               STRING  PTBYO-BYOMEI        DELIMITED BY  SPACE
                       WRK-CONS-UTAGAI     DELIMITED BY  SIZE
                       INTO                WRK-BYOMEI
               END-STRING
               COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-UTAGAI-MOJISU
               IF      PTBYO-BYOMEICD (21)  NOT =   SPACE             
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       21
                       IF      PTBYO-BYOMEICD (IDX)    =   SPACE
                           MOVE    WRK-CONS-BYOMEICD-UTAGAI
                                               TO  PTBYO-BYOMEICD (IDX)
                           MOVE    21          TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF    
           IF    ( PTBYO-UTAGAIFLG   =   "2" OR  "3" )
           AND   ( WRK-HENFLG2         =   SPACE     )                          
               MOVE    SPACE               TO  WRK-BYOMEIW
               STRING  WRK-CONS-KYUSEI     DELIMITED BY  SIZE
                       WRK-BYOMEI          DELIMITED BY  SPACE
                       INTO                WRK-BYOMEIW
               END-STRING
               COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-KYUSEI-MOJISU
               MOVE    WRK-BYOMEIW         TO  WRK-BYOMEI  
               MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG                 
               IF      PTBYO-BYOMEICD (21) NOT =   SPACE             
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       21
                       IF      PTBYO-BYOMEICD (IDX)    =   SPACE
                           MOVE    WRK-CONS-BYOMEICD-KYUSEI
                                               TO  PTBYO-BYOMEICD (IDX)
                           MOVE    21          TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           ADD     1                   TO  WRK-RENNUM
           
           INITIALIZE                      RECE63-REC
           MOVE    RECE62-HOSPID       TO  RECE63-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE63-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE63-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE63-PTID
           MOVE    RECE62-RECEKA       TO  RECE63-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE63-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE63-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE63-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE63-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE63-TEKSTYMD
           MOVE    PTBYO-SRYYMD        TO  RECE63-SRYYMD
           MOVE    WRK-RENNUM          TO  RECE63-RENNUM
           MOVE    RECE62-PTNUM        TO  RECE63-PTNUM
           MOVE    PTBYO-SRYKA         TO  RECE63-SRYKA
           MOVE    PTBYO-KHNBYOMEICD   TO  RECE63-KHNBYOMEICD   
           MOVE    PTBYO-BYOMEICD-G    TO  RECE63-BYOMEICD-G
           MOVE    PTBYO-UTAGAIFLG     TO  RECE63-UTAGAIFLG
           IF      SYS-2005-SYUBYOHENFLG    =   ZERO
               MOVE    "9"                 TO  RECE63-SYUBYOFLG 
           ELSE
               IF      PTBYO-SYUBYOFLG     =   "1"
                   MOVE    PTBYO-SYUBYOFLG     TO  RECE63-SYUBYOFLG    
               ELSE
                   MOVE    "9"                 TO  RECE63-SYUBYOFLG 
               END-IF
           END-IF
           MOVE    PTBYO-MANSEIKBN     TO  RECE63-MANSEIKBN
           IF      RECE62-SRYYM        <   PTBYO-TENKIYMD(1:6)
               CONTINUE
           ELSE    
               MOVE    PTBYO-TENKIKBN      TO  RECE63-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  RECE63-TENKIYMD   
           END-IF    
           MOVE    WRK-BYOMEI          TO  RECE63-BYOMEI
           COMPUTE RECE63-BYOMEIMOJI    =   WRK-MOJISU    /   2
           MOVE    PTBYO-BYOMEIHENFLG  TO  RECE63-BYOMEIHENFLG   
      *
           IF      PTBYO-BYOMEIHENFLG  =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                   IF      PTBYO-BYOMEICD (IDX) (1:4)  =   "ZZZC"
                                                       OR  "ZZZ("
                       MOVE    "1"         TO  RECE63-BYOMEIHENFLG
                       MOVE    21          TO  IDX
                   END-IF
               END-PERFORM
           END-IF   
      *
           WRITE   RECE63-REC
           ADD     1                   TO  CNT-OUT   
           .
       20011-RECE63-WRITE-EXT.
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
057400*
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE62-FILE
           CLOSE   RECE63-FILE
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
          DISPLAY "RECE62  IN  " CNT-IN 
          DISPLAY "RECE63  CNT " CNT-OUT  
          DISPLAY "*** ORCR0670 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ　ＲＥＡＤ
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC 
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書０２読込
      *****************************************************************
       900-RECE62-READ-SEC             SECTION.
      *
           READ    RECE62-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-IN
                   MOVE    ZERO        TO  FLG-END
      *
                   DISPLAY "RECE62  PTNUM=" RECE62-PTNUM
      *
           END-READ
           .
       900-RECE62-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      * 
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
           MOVE    WRK-PARA-SRYYM  TO  ORC-DBYMD (1:6)
           MOVE    "01"            TO  ORC-DBYMD (7:2) 
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               MOVE    "DBFETCH"       TO  MCP-FUNC
               CALL    "ORCMCPSUB"     USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-SYSKANRI-READ-EXT.
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
