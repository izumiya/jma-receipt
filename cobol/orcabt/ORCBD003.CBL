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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBD003.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 日計表（診療年月日）（ＨＣ１７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/10/10  サイト区分追加
      *  01.01.02    NACL-藤原    02/10/17  自費の集計方法修正
      *  02.00.01    NACL-森脇    04/01/08  マイナスを考慮
      *  02.00.02    NACL-門脇    05/01/12  自費欄集計修正（端数処理）
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *----(02.00.01)--UPD-START---
      *     COPY    "HC17.INC".
           COPY    "HCM17.INC"     REPLACING  //HCM17//
                                   BY         //HC17//.
      *----(02.00.01)--UPD-END-----
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-SRYYMD     PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH                PIC X(64).
      *
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-PARA-SRYYMDWH PIC X(22).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
      *  
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-DISCOUNT-HASU-KBN2  PIC X(01).
      *
      *****03  WRK-IN                PIC X(01).
      *
      *----(02.00.01)--UPD-START---
      *     03  WRK-MONEY             PIC 9(08).
      *     03  WRK-JIHI              PIC 9(08).
           03  WRK-MONEY             PIC S9(08).
           03  WRK-JIHI              PIC S9(08).
           03  WRK-JIHI2             PIC S9(08).
      *----(02.00.01)--UPD-END-----
      *
           03  WRK-RENNUM-X          PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
      *----(02.00.01)--UPD-START---
           03  WRK-Z8                PIC ZZZZZZZ9.
           03  WRK-Z9                PIC ZZZZZZZZ9.
           03  WRK-Z99               PIC --------9.
      *----(02.00.01)--UPD-END-----
      *
       01  SUM-AREA                             VALUE   ZERO.
           03  SUM-OCC               OCCURS  10.
               05  SUM-KENSU         PIC 9(05).
               05  SUM-HKNTEN        PIC 9(08).
      *----(02.00.01)--UPD-START---
      *         05  SUM-MONEY         PIC 9(08).
               05  SUM-MONEY         PIC S9(08).
      *----(02.00.01)--UPD-END-----
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      *
           COPY    "CPSHELLTBL.INC".
      *     
           COPY    "ORCA-DBPATH".         
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    収納マスタ
       01  BD002-REC.
           COPY    "CPBD002.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
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
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
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
           INITIALIZE                  SUM-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID    
                                               LNK-PRTKANRI-PRTNM    
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPID
                                               WRK-PARA-SRYYMD
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBD003"      TO  JOB-PGID
           MOVE    "日計表（診療年月日）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
      *
           MOVE    "RECEERR"       TO  RECEERR-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  RECEERR-TERMID
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDWH 
      *
      *    対象年月日編集
           MOVE    WRK-PARA-SRYYMD TO    WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PARA-SRYYMDWH 
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
               MOVE    SYS-1001-DISCOUNT-HASU-KBN2
                                           TO  WRK-DISCOUNT-HASU-KBN2
           ELSE    
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF                                     
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 900-DBSELECT-SEC
      *
           PERFORM              UNTIL   FLG-END     =   1
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *        帳票編集<合計>処理
               IF     FLG-END       =   1
                   PERFORM 330-TERM-HEN-SEC
               END-IF
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           PERFORM 900-DBCLOSE-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           ADD     1                   TO  CNT-PAGE
      *
           INITIALIZE                      HC17      
      *
           MOVE    "診療年月日"        TO  HC17-MIDASI
           MOVE    WRK-PARA-SRYYMDWH
                                       TO  HC17-SRYYMD
           MOVE    WRK-SYSYMDWH        TO  HC17-SYSYMD
      *
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC17-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *
           PERFORM             UNTIL   FLG-END     =   1
                               OR      CNT-LINE    =   30
      *    患者情報読み込み       
               INITIALIZE                  PTINF-REC
               MOVE    BD002-HOSPID          TO  PTINF-HOSPID
               MOVE    BD002-PTID            TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
      *    
               ADD     1                   TO  CNT-LINE
      *    連番
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  WRK-RENNUM-X
               MOVE    WRK-RENNUM-X        TO  HC17-RENNUM (CNT-LINE)     
      *    患者番号
               MOVE    BD002-PTNUM         TO  HC17-PTNUM (CNT-LINE)
      *    氏名
               MOVE    PTINF-NAME          TO  HC17-NAME  (CNT-LINE)
      *    保険点数
               MOVE    ZERO                TO  BD002-TOTAL-HKNTEN
               EVALUATE    BD002-SYUHKNNUM
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                   WHEN    WRK-CONS-ROUSAI-HKNNUM
                       CONTINUE
                   WHEN    OTHER    
                       PERFORM VARYING IDZ FROM    1   BY  1
                               UNTIL   IDZ >       11
                           ADD     BD002-HKNTEN (IDZ)
                                               TO  BD002-TOTAL-HKNTEN
                       END-PERFORM
      *----(02.00.01)--UPD-START---
                       MOVE    BD002-TOTAL-HKNTEN
                                           TO  WRK-Z8
                       MOVE    WRK-Z8      TO  HC17-HKNTEN (CNT-LINE)
      *----(02.00.01)--UPD-END-----
               END-EVALUATE        
      *    保険別金額（社保本人〜自賠責）
               MOVE    ZERO                TO  IDX 
               EVALUATE    BD002-SYUHKNNUM
                   WHEN    SPACE
                       MOVE    6                   TO  IDX
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                       MOVE    9                   TO  IDX
                   WHEN    WRK-CONS-ROUSAI-HKNNUM    
                       MOVE    8                   TO  IDX
      ***          WHEN    WRK-CONS-JIHI-HKNNUM    
      ***              MOVE    7                   TO  IDX
                   WHEN    "060"    
                   WHEN    "067"
                       IF      BD002-KOH1HKNNUM      =   "027" 
                           MOVE    5                   TO  IDX
                       ELSE
                           MOVE    4                   TO  IDX
                       END-IF
                   WHEN    OTHER
                       IF  BD002-SYUHKNNUM(1:2) = "98"
                           MOVE    7                   TO  IDX
                       ELSE
                         IF      BD002-KOH1HKNNUM      =   "027" 
                           MOVE    3                   TO  IDX
                         ELSE
                           PERFORM 3201-PTHKNINF-HENSYU-SEC
                           EVALUATE    PTHKN-HONKZKKBN
                               WHEN    "1"
                                   MOVE    1                   TO  IDX
                               WHEN    "2"     
                                   MOVE    2                   TO  IDX
                           END-EVALUATE
                         END-IF
                       END-IF
               END-EVALUATE
      *
               IF      IDX                 >   ZERO     
      *
                   DISPLAY  "BD002-PTNUM = " BD002-PTNUM
                   DISPLAY  "BD002-RSIJIBAI-SKYMONEY = "
                             BD002-RSIJIBAI-SKYMONEY
                   DISPLAY  "BD002-JIHI-TOTAL-TAX = "
                             BD002-JIHI-TOTAL-TAX
      *            自費以外のとき自費分だけ集計
                   IF      IDX             =   7
                       MOVE    ZERO            TO  WRK-JIHI
                   ELSE
                       COMPUTE WRK-JIHI  =   BD002-TOTAL-TGMONEY  
                                         +   BD002-TOTAL-TGMONEY-TAX  
                                         +   BD002-JIHI-TOTAL
                                         +   BD002-JIHI-TOTAL-TAX  
                                         +   BD002-JIHI-TAX
                                         +   BD002-RMSAGAKU
                                         + BD002-SKYMONEY-SKJ-JIHI-KEI
                   END-IF
                   IF    IDX  =  8  OR  9
                       COMPUTE WRK-MONEY   =   BD002-RSIJIBAI-SKYMONEY
                                           -   WRK-JIHI
                   ELSE
                       COMPUTE WRK-MONEY   =   BD002-SKYMONEY
                                           -   WRK-JIHI                  
                       IF   WRK-JIHI  NOT =  ZERO
                         IF  ((BD002-DISCOUNT-TEIRITU  NOT =  SPACE) AND
                              (BD002-DISCOUNT-RATE     NOT =  ZERO ))
                         OR  ( BD002-CHOSEI            NOT =  ZERO  )
                            CONTINUE
                         ELSE
                            COMPUTE WRK-MONEY  =  BD002-HKNTEKMONEY
                                               +  BD002-RJN-FTN
                                               +  BD002-KOH-FTN
                                               +  BD002-YKZ-FTN
                                               +  BD002-SKYMONEY-SKJ-KEI
                            COMPUTE WRK-JIHI   =  BD002-SKYMONEY
                                               -  WRK-MONEY
                         END-IF
                       END-IF
                   END-IF
                   DISPLAY "WRK-JIHI=" WRK-JIHI
                   DISPLAY "BD002-SKYMONEY=" BD002-SKYMONEY
                   DISPLAY "WRK-MONEY=" WRK-MONEY
      *              
      *----(02.00.01)--UPD-START---
                   IF      WRK-MONEY       <   0
                       MOVE    WRK-MONEY   TO  WRK-Z99
                       MOVE    WRK-Z99     TO  HC17-MONEY (IDX CNT-LINE)
                   ELSE
                       MOVE    WRK-MONEY   TO  WRK-Z9
                       MOVE    WRK-Z9      TO  HC17-MONEY (IDX CNT-LINE)
                   END-IF
      *----(02.00.01)--UPD-END-----
                   IF      IDX             =   7
                       CONTINUE
                   ELSE
                       IF      WRK-JIHI    >   ZERO      
      *----(02.00.01)--UPD-START---
                           MOVE    WRK-JIHI
                                           TO  WRK-Z9
                           MOVE    "*"     TO  WRK-Z9 (2:1)
                           MOVE    WRK-Z9  TO  HC17-MONEY (7   CNT-LINE)
      *----(02.00.01)--UPD-END-----
                       END-IF
                   END-IF        
               END-IF
      *
               ADD     BD002-TOTAL-HKNTEN  TO  SUM-HKNTEN (1)
               ADD     BD002-SKYMONEY      TO  SUM-MONEY  (1)
               ADD     1                   TO  SUM-KENSU  (1)
               IF      IDX                 >   ZERO
                   COMPUTE IDY     =       IDX     +   1          
                   IF      IDX                 <   8 
                       ADD     BD002-TOTAL-HKNTEN  TO  SUM-HKNTEN (IDY)
                   END-IF    
                   ADD     WRK-MONEY               TO  SUM-MONEY  (IDY)
                   ADD     1                       TO  SUM-KENSU  (IDY)
                   ADD     WRK-JIHI                TO  SUM-MONEY  (8)
               END-IF
      *
               PERFORM 900-DBFETCH-SEC
           END-PERFORM     
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *   保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
           MOVE    BD002-HOSPID        TO  COMB-HOSPID
           MOVE    BD002-PTID          TO  COMB-PTID
           MOVE    BD002-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *    患者保険情報読み込み       
               INITIALIZE                  PTHKNINF-REC
               MOVE    BD002-HOSPID        TO  PTHKN-HOSPID
               MOVE    BD002-PTID          TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               PERFORM 900-PTHKNINF-READ-SEC
           END-IF    
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *          *    帳票編集<合計>処理
      *****************************************************************
      *    
       330-TERM-HEN-SEC         SECTION.
      *
      *----(02.00.01)--UPD-START---
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      IDX                <   9    
                   MOVE    SUM-HKNTEN (IDX)   TO  WRK-Z8
                   MOVE    WRK-Z8             TO  HC17-GHKNTEN (IDX)
               END-IF    
               IF      SUM-MONEY (IDX)    <   0
                   MOVE    SUM-MONEY (IDX)    TO  WRK-Z99 
                   MOVE    WRK-Z99            TO  HC17-GMONEY  (IDX) 
               ELSE
                   MOVE    SUM-MONEY (IDX)    TO  WRK-Z9 
                   MOVE    WRK-Z9             TO  HC17-GMONEY  (IDX) 
               END-IF
               MOVE    SUM-KENSU (IDX)    TO  WRK-Z8
               MOVE    WRK-Z8             TO  HC17-GKENSU  (IDX)
           END-PERFORM
      *----(02.00.01)--UPD-END-----
      *
           .
       331-TOTAL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
      *****MOVE   "HC17"               TO  SPRT-PRTID
      *----(02.00.01)--UPD-START---
      *     MOVE   "HC17V01"            TO  SPRT-PRTID
           MOVE   "HCM17.red"          TO  SPRT-PRTID
      *----(02.00.01)--UPD-END-----
           MOVE    "日計表（診療年月日）"
                                       TO  SPRT-TITLE
           MOVE    HC17                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC                           
           END-IF                                                              
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
057400*
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT                        
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
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
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ　伝票発行日で検索
      *****************************************************************
       900-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  BD002-REC
           MOVE    WRK-PARA-HOSPID     TO  BD002-HOSPID  
           MOVE    WRK-PARA-SRYYMD     TO  BD002-SRYYMD
      *
           MOVE    BD002-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "BD002-KEY2"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF        
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納を検索処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "BD002-KEY2"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  BD002-REC
           ELSE
               MOVE    1                       TO  FLG-END
           END-IF  
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "BD002-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
      *    MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    WRK-PARA-SRYYMD     TO  ORC-DBYMD
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
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
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
               CALL    "ORCMCPSUB"          USING
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
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   MOVE    ZERO                TO  FLG-PTHKNINF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
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
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *
           .
       900-HKNCOMBI-READ-EXT.
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
