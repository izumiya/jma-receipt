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
       PROGRAM-ID.             ORHCML09.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 自院病名マスタ
      *  コンポーネント名  : 自院病名一覧（ＨＣＭＬ０９）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/04/24    MCC-森脇      新規作成
      *****************************************************************
      *  00.00.01    NACL-森脇     順番並び替え
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *    エラーファイル
           SELECT  ML09ERR-FILE    ASSIGN  ML09ERR
                                   FILE    STATUS  IS  STS-ML09ERR.
      *
      *----(00.00.01)--ADD-START---
      *    並び換え用
           SELECT  TOKEI-FILE      ASSIGN  TOKEIPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOKEI-KEY
                                   FILE    STATUS  IS  STS-TOKEI.
      *----(00.00.01)--ADD-END-----
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "HCML09.INC".
      *
      *    エラーファイル
       FD  ML09ERR-FILE.
       01  ML09ERR-REC             PIC X(200).
      *
      *----(00.00.01)--ADD-START---
      *    並び換え用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC.
           03  TOKEI-KEY.
               05  TOKEI-KEY-NAIYO     PIC X(25).
           03  TOKEI-DSPSEQ            PIC 9(02).
           03  TOKEI-BYOMEIINPUTCD     PIC X(20).
           03  TOKEI-BUNRUICD          PIC X(03).
           03  TOKEI-BYOMEI            PIC X(160).
           03  TOKEI-MANSEIKBN         PIC 9(02).
           03  TOKEI-UTAGAIFLG         PIC X(01).
      *----(00.00.01)--ADD-END-----
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
           COPY    "ORCA-DBPATH".
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //ML09ERR//.
           03                          PIC X(04)   VALUE   ".dat".
      *----(00.00.01)--ADD-START---
      *    並び換え用 名称領域 
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
           03  FILLER                  PIC X(04)   VALUE   ".dat".
      *----(00.00.01)--ADD-END-----
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-ML09ERR         PIC X(02).
      *----(00.00.01)--ADD-START---
           03  STS-TOKEI           PIC X(02).
      *----(00.00.01)--ADD-END-----
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-BUNRUI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *----(00.00.01)--ADD-START---
           03  FLG-TOKEI           PIC 9(01).
      *----(00.00.01)--ADD-END-----
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-BYOMEI          PIC 9(04).
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA. 
           03  WRK-PARA.
               05  WRK-PARA-TERMID PIC X(64).
               05  WRK-PARA-SYSYMD PIC X(8).
               05  WRK-PARA-HOSPID PIC X(24).
      *----(00.00.01)--ADD-START---
               05  WRK-PARA-SYUKBN PIC X(01).
      *----(00.00.01)--ADD-END-----
      *
           03  WRK-ML09ERR         PIC X(200).
      *  
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
      *     03  WRK-IN             PIC X.
      *
           03  WRK-PAGE            PIC ZZ,ZZ9.
      *
      *----(00.00.01)--ADD-START---
           03  WRK-KEY0-AREA.
               05  WRK-KEY0-DSPSEQ        PIC 9(02).
               05  WRK-KEY0-BYOMEIINPUTCD PIC X(20).
               05  WRK-KEY0-BUNRUICD      PIC X(03).
           03  WRK-KEY1-AREA.
               05  WRK-KEY1-BUNRUICD      PIC X(03).
               05  WRK-KEY1-DSPSEQ        PIC 9(02).
               05  WRK-KEY1-BYOMEIINPUTCD PIC X(20).
           03  WRK-KEY2-AREA.
               05  WRK-KEY2-BYOMEIINPUTCD PIC X(20).
               05  WRK-KEY2-BUNRUICD      PIC X(03).
               05  WRK-KEY2-DSPSEQ        PIC 9(02).
      *----(00.00.01)--ADD-END-----
      *
       01  WRK-JOBKANRI-AREA.
           03  WRK-JOBKANRI-MODE   PIC X(03).
018900     03  WRK-JOBKANRI-RETURN PIC S9(03).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *  
      *    自院病名マスタ
       01  USBYO-REC.
           COPY    "CPUSERBYOMEI.INC".
       01  USBUN-REC.
           COPY    "CPUSERBYOMEI.INC" 
                                   REPLACING  //USBYO//
                                   BY         //USBUN//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *----(00.00.01)--ADD-ATART---
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *----(00.00.01)--ADD-END-----
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ******************************************************************
       PROCEDURE                   DIVISION
                                   USING     COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME  
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-SYSYMD
                                               WRK-PARA-HOSPID
      *----(00.00.01)--ADD-START---
                                               WRK-PARA-SYUKBN
      *----(00.00.01)--ADD-END-----
      *
           MOVE    "ML09ERR"       TO  ML09ERR-FILE-ID
           MOVE    WRK-PARA-TERMID TO  ML09ERR-TERMID
      *----(00.00.01)--ADD-START---
           MOVE    "TOKEIZZ"       TO  TOKEIPARA-FILE-ID
           MOVE    WRK-PARA-TERMID TO  TOKEIPARA-TERMID
      *----(00.00.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 120-DBOPEN-SEC
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *      
      *    出力先プリンタ名割り当て情報編集
           MOVE    SPACE           TO  SYS-1031-REC
           INITIALIZE                  SYS-1031-REC
           MOVE    "1031"          TO  SYS-1031-KANRICD
           MOVE    "*"             TO  SYS-1031-KBNCD
           MOVE    SYS-1031-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1031-REC
           ELSE    
               INITIALIZE                  SYS-1031-REC
           END-IF
           .
      *
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    並び換え用ファイルをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *    並び換え用ファイルを開く
           OPEN    I-O                 TOKEI-FILE
      *
      *----(00.00.01)--UPD-START---
      *    自院病名セット
           PERFORM 900-DBSELECT-SEC
           PERFORM UNTIL   FLG-BYOMEI  =   1
               PERFORM 210-TOKEI-SET-SEC
               PERFORM 900-DBFETCH-SEC
           END-PERFORM
           PERFORM 900-DBCLOSE-SEC
      *
      *    並び換え用ファイルを閉じる
           CLOSE                       TOKEI-FILE
      *    並び換え用ファイルを開く
           OPEN    INPUT               TOKEI-FILE
      *
      *    印刷処理
           PERFORM 300-INSATSU-SEC
      *
      *    並び換え用ファイルを閉じる
           CLOSE                       TOKEI-FILE
      *    並び換え用ファイルをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TOKEIPARA       TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *----(00.00.01)--UPD-END-----
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *----(00.00.01)--ADD-START---
      *****************************************************************
      *    並び換えようセット処理
      *****************************************************************
       210-TOKEI-SET-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TOKEI
      *
           INITIALIZE                  TOKEI-REC
      *
      *    並び換え指定
           EVALUATE    WRK-PARA-SYUKBN
               WHEN    "0"
               WHEN    SPACE
                   INITIALIZE                  WRK-KEY0-AREA
                   MOVE    USBYO-BUNRUICD  TO  WRK-KEY0-BUNRUICD
                   MOVE    USBYO-DSPSEQ    TO  WRK-KEY0-DSPSEQ
                   MOVE    USBYO-BYOMEIINPUTCD
                                           TO  WRK-KEY0-BYOMEIINPUTCD
                   MOVE    WRK-KEY0-AREA   TO  TOKEI-KEY-NAIYO
               WHEN    "1"
                   INITIALIZE                  WRK-KEY1-AREA
                   MOVE    USBYO-BUNRUICD  TO  WRK-KEY1-BUNRUICD
                   MOVE    USBYO-DSPSEQ    TO  WRK-KEY1-DSPSEQ
                   MOVE    USBYO-BYOMEIINPUTCD
                                           TO  WRK-KEY1-BYOMEIINPUTCD
                   MOVE    WRK-KEY1-AREA   TO  TOKEI-KEY-NAIYO
               WHEN    "2"
                   INITIALIZE                  WRK-KEY2-AREA
                   MOVE    USBYO-BUNRUICD  TO  WRK-KEY2-BUNRUICD
                   MOVE    USBYO-DSPSEQ    TO  WRK-KEY2-DSPSEQ
                   MOVE    USBYO-BYOMEIINPUTCD
                                           TO  WRK-KEY2-BYOMEIINPUTCD
                   MOVE    WRK-KEY2-AREA   TO  TOKEI-KEY-NAIYO
           END-EVALUATE
      *
           PERFORM 900-TOKEI-READ-SEC
      *
           IF      FLG-TOKEI       NOT =   1
               GO  TO  210-TOKEI-SET-EXT
           END-IF
      *
           MOVE    USBYO-BUNRUICD  TO  TOKEI-BUNRUICD
           MOVE    USBYO-DSPSEQ    TO  TOKEI-DSPSEQ
           MOVE    USBYO-BYOMEIINPUTCD
                                   TO  TOKEI-BYOMEIINPUTCD
           MOVE    USBYO-BYOMEI    TO  TOKEI-BYOMEI
           MOVE    USBYO-MANSEIKBN TO  TOKEI-MANSEIKBN
           MOVE    USBYO-UTAGAIFLG TO  TOKEI-UTAGAIFLG
           WRITE   TOKEI-REC
           .
       210-TOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票セット処理
      *****************************************************************
       300-INSATSU-SEC             SECTION.
      *
           IF      CNT-BYOMEI      =   ZERO
               GO  TO  300-INSATSU-EXT
           END-IF
      *
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
      *
      *    統計ファイル読み込み
           INITIALIZE                  TOKEI-REC
           PERFORM 900-TOKEI-NEXT-SEC
      *
      *    帳票印刷開始処理
           PERFORM UNTIL   FLG-TOKEI    =   1
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           .
       300-INSATSU-EXT.
           EXIT.
      *
      *----(00.00.01)--ADD-END-----
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
           ADD     1               TO  CNT-PAGE
      *
           INITIALIZE                  HCML09
           MOVE    SPACE           TO  HCML09
      *
           MOVE    WRK-PARA-SYSYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  HCML09-SAKUSEIYMD
      *
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCML09-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
      *
           PERFORM UNTIL   FLG-TOKEI   =   1
                   OR      CNT-LINE    =   28
      *
               ADD     1                 TO  CNT-LINE
               MOVE    TOKEI-BYOMEIINPUTCD
                               TO  HCML09-BYOMEIINPUTCD(CNT-LINE)
               MOVE    TOKEI-BYOMEI      TO  HCML09-BYOMEI(CNT-LINE)
      *
               EVALUATE    TOKEI-MANSEIKBN
                   WHEN    "00"
                       MOVE    SPACE     TO  HCML09-MANSEIKBN(CNT-LINE)
                   WHEN    "03"
                       MOVE   "皮膚１"   TO  HCML09-MANSEIKBN(CNT-LINE)
                   WHEN    "04"
                       MOVE   "皮膚２"   TO  HCML09-MANSEIKBN(CNT-LINE)
                   WHEN    "05"
                       MOVE   "特定疾患" TO  HCML09-MANSEIKBN(CNT-LINE)
                   WHEN    "07"
                       MOVE   "皮膚３"   TO  HCML09-MANSEIKBN(CNT-LINE)
               END-EVALUATE
      *
               EVALUATE    TOKEI-UTAGAIFLG
                   WHEN    "0"
                       MOVE    SPACE     TO  HCML09-UTAGAIFLG(CNT-LINE)
                   WHEN    "1"
                       MOVE   "疑い"     TO  HCML09-UTAGAIFLG(CNT-LINE)
                   WHEN    "2"
                       MOVE   "急性"     TO  HCML09-UTAGAIFLG(CNT-LINE)
                   WHEN    "3"
                       MOVE   "急疑"     TO  HCML09-UTAGAIFLG(CNT-LINE)
               END-EVALUATE
      *
              IF  TOKEI-BUNRUICD  NOT  =  SPACE
                   PERFORM 900-DBREAD-SEC
               END-IF
      *
               PERFORM 900-TOKEI-NEXT-SEC
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
      *     IF      CNT-LINE        =   0
      *         GO  TO  390-PRINT-OUT-EXT
      *     END-IF
      *
      *    ファイルＯＰＥＮ
           MOVE    "HCML"              TO  HC01PARA-FORM-ID
           MOVE    WRK-PARA-TERMID     TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           MOVE    CNT-PAGE            TO  HC01PARA-CNT
           OPEN    OUTPUT              PRT-FILE
           WRITE   HCML09
           CLOSE                       PRT-FILE
      *
      *----(00.00.01)--UPD-START---
           MOVE   "HC01.sh"            TO  SHELL-ID
           MOVE   "HCML09.dia"         TO  SHELL-DIA
           MOVE   "HCML09.def"         TO  SHELL-DEF
           MOVE   HC01PARA             TO  SHELL-DAT
           MOVE   SYS-1031-PRTNM(6)    TO  SHELL-PRTNM 
      *   印刷用スクリプト処理
           MOVE  SHELL-NAME            TO  SHELLTBL-NAME
           MOVE  SHELL-ARG1            TO  SHELLTBL-ARG1
           MOVE  SHELL-ARG2            TO  SHELLTBL-ARG2
           MOVE  SHELL-ARG3            TO  SHELLTBL-ARG3
           MOVE  SHELL-ARG4            TO  SHELLTBL-ARG4
           MOVE  SHELL-ARG5            TO  SHELLTBL-ARG5
           MOVE  SHELLTBL              TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *----(00.00.01)--UPD-END-----
           .
      *
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   ML09ERR-FILE
      *     
           IF      STS-ML09ERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              ML09ERR-FILE
      *
               MOVE    WRK-ML09ERR     TO  ML09ERR-REC
               WRITE   ML09ERR-REC
           END-IF
      *
           CLOSE   ML09ERR-FILE
      *
           MOVE    1               TO  FLG-END
      * 
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *----(00.00.01)--UPD-START---
      *****************************************************************
      *    並び換え用ファイル読込
      *****************************************************************
       900-TOKEI-READ-SEC           SECTION.
      *
           READ    TOKEI-FILE
               INVALID
                   MOVE    1           TO  FLG-TOKEI
               NOT INVALID
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
           .
       900-TOKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    並び換え用ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC           SECTION.
      *
           READ    TOKEI-FILE       NEXT
               AT  END
                   MOVE    1           TO  FLG-TOKEI
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスタ　検索 
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           INITIALIZE                  USBYO-REC
           MOVE    WRK-PARA-HOSPID     TO  USBYO-HOSPID
           MOVE    USBYO-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "USERBYOMEI-KEY3"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     自院病名マスタを検索処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "USERBYOMEI-KEY3"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-BYOMEI
               MOVE    MCPDATA-REC     TO  USBYO-REC
               ADD     1               TO  CNT-BYOMEI
           ELSE
               MOVE    1               TO  FLG-BYOMEI
           END-IF  
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスタ　クローズ処理
      *****************************************************************
       900-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "USERBYOMEI-KEY3"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *----(00.00.01)--UPD-END-----
      *****************************************************************
      *    自院病名マスタ　検索 
      *****************************************************************
       900-DBREAD-SEC                 SECTION.
      *
           INITIALIZE                  USBUN-REC
           MOVE    WRK-PARA-HOSPID     TO  USBUN-HOSPID
           MOVE    "C"                 TO  USBUN-CDSYU
           MOVE    TOKEI-BUNRUICD      TO  USBUN-BYOMEIINPUTCD
      *
           MOVE    USBUN-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "USERBYOMEI-KEY"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "USERBYOMEI-KEY"    TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-BUNRUI
                   MOVE    MCPDATA-REC     TO  USBUN-REC
                   MOVE    USBUN-BYOMEI    
                           TO  HCML09-BUNRUIMEI(CNT-LINE)
               ELSE
                   MOVE    1               TO  FLG-BUNRUI
               END-IF  
           ELSE
               MOVE    1                   TO  FLG-BUNRUI
           END-IF        
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "USERBYOMEI-KEY"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       900-DBREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    WRK-PARA-SYSYMD TO  ORC-DBYMD
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       120-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
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
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
