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
       PROGRAM-ID.             ORCBD008.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 日計表（ＨＣＭ４１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/01/09    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇     05/01/12  自費欄集計修正（端数処理）
      *  03.40.01    NACL-森脇     07/02/08  平成１９年４月自費消費税表示変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    収納ファイル
           SELECT  BD00801-FILE   ASSIGN   BD00801PARA
                                  ORGANIZATION    IS  INDEXED
                                  ACCESS  MODE    IS  DYNAMIC
                                  RECORD  KEY     IS  BD00801-ISAM-KEY
                                  FILE    STATUS  IS  STS-BD00801.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    収納ファイル
       FD  BD00801-FILE.
       01  BD00801-REC.
           02  BD00801-ISAM-KEY.
               03  BD00801-ISAM-KEY-VAL        PIC X(100).
               03  BD00801-ISAM-KEY1 REDEFINES BD00801-ISAM-KEY-VAL.
                   05  BD00801-ISAM-KEY1-PTNUM      PIC X(20).
                   05  BD00801-ISAM-KEY1-PTID       PIC 9(10).
                   05  BD00801-ISAM-KEY1-DENPNUM    PIC 9(07).
                   05  BD00801-ISAM-KEY1-NYUGAIKBN  PIC X(01).
                   05  BD00801-ISAM-KEY1-FIL        PIC X(62).
               03  BD00801-ISAM-KEY2 REDEFINES BD00801-ISAM-KEY-VAL.
                   05  BD00801-ISAM-KEY2-NYUGAIKBN  PIC X(01).
                   05  BD00801-ISAM-KEY2-DENPNUM    PIC 9(07).
                   05  BD00801-ISAM-KEY2-FIL        PIC X(92).
           02  BD00801-PTNUM                        PIC X(20).
           02  BD00801-SYU-DAT.
           COPY    "CPSYUNOU.INC"  REPLACING  //SYU-//
                                   BY         //BD00801-SYU-//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    収納ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //BD00801//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
           COPY    "HCM41.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-BD00801         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-OK              PIC 9(01).
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
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH.
               05  WRK-MCP-BLOCKS  PIC S9(9)   BINARY.
               05  WRK-MCP-RNAME   PIC S9(9)   BINARY.
               05  WRK-MCP-PNAME   PIC S9(9)   BINARY.
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-SYORIYMDWH      PIC X(22).
           03  WRK-SYSYMDWH        PIC X(22).
           03  WRK-RENNUM          PIC 9(03).
           03  WRK-PTNUM           PIC X(20).
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
           03  WRK-DISCOUNT-HASU-KBN2   PIC X(01).
      *
      *****03  WRK-IN              PIC X(01).
      *
           03  WRK-MONEY           PIC S9(08).
           03  WRK-JIHI            PIC S9(08).
           03  WRK-JIHI2           PIC S9(08).
      *
           03  WRK-RENNUM-X        PIC ZZ9.
           03  WRK-PAGE            PIC ZZ9.
      *
           03  WRK-Z8              PIC ZZZZZZZ9.
           03  WRK-Z9              PIC ZZZZZZZZ9.
           03  WRK-Z99             PIC --------9.
      *
       01  SUM-AREA                            VALUE   ZERO.
           03  SUM-OCC             OCCURS  10.
               05  SUM-KENSU       PIC 9(05).
               05  SUM-HKNTEN      PIC 9(08).
               05  SUM-MONEY       PIC S9(08).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980".
      *    労災保険番号
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    自賠責保険番号
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *
      *----(03.04.01)--ADD-START---
       01  CONST-AREA.
           03  CONST-H190401           PIC X(08)   VALUE   "20070401".
      *----(03.04.01)--ADD-END-----
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
           COPY    "CPSK1009.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    収納マスタ
       01  SYU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
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
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
           COPY    "ORCA-DBPATH".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPID     PIC X(24).
      *
       COPY    "CPORCBD008.INC".
      *
      ****************************************************************
       PROCEDURE               DIVISION
               USING
               WRK-PARA
               ORCBD008AREA.
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
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
      *
           EVALUATE    ORCBD008-SYORIKBN
           WHEN        "1"
               MOVE    "ORCBD006"              TO  JOB-PGID
               MOVE    "日計表（伝票発行日）"  TO  JOB-SHELLMSG
           WHEN        "2"
               MOVE    "ORCBD007"              TO  JOB-PGID
               MOVE    "日計表（診療年月日）"  TO  JOB-SHELLMSG
           WHEN        "3"
               MOVE    "ORCBD002"              TO  JOB-PGID
               MOVE    "日計表（伝票発行日）"  TO  JOB-SHELLMSG
           WHEN        "4"
               MOVE    "ORCBD003"              TO  JOB-PGID
               MOVE    "日計表（診療年月日）"  TO  JOB-SHELLMSG
           END-EVALUATE
      *
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *
           MOVE    "RECEERR"       TO  RECEERR-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  RECEERR-TERMID
      *
           MOVE    "BD00801"       TO  BD00801PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BD00801PARA-TERMID
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
      *    収納ファイル編集処理
           PERFORM 120-BD00801-HEN-SEC
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
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  WRK-SYSYMDWH
      *
      *    対象年月日編集
           MOVE    ORCBD008-SYORIYMD
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  WRK-SYORIYMDWH
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID TO  WRK-PARA-HOSPID
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
      *    収納ファイル編集処理
      *****************************************************************
       120-BD00801-HEN-SEC             SECTION.
      *
           OPEN    OUTPUT  BD00801-FILE
      *
           PERFORM 900-DBSELECT-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               INITIALIZE                  PTNUM-REC
               MOVE    SYU-HOSPID    TO  PTNUM-HOSPID
               MOVE    SYU-PTID      TO  PTNUM-PTID
               PERFORM 900-PTNUM-READ-SEC
      *
               MOVE    PTNUM-PTNUM   TO  BD00801-PTNUM
      *
               IF    ( ORCBD008-SORTKBN  =     "1" )
                   MOVE    SYU-NYUGAIKBN TO  BD00801-ISAM-KEY2-NYUGAIKBN
                   MOVE    SYU-DENPNUM   TO  BD00801-ISAM-KEY2-DENPNUM
               ELSE
                   MOVE    PTNUM-PTNUM   TO  BD00801-ISAM-KEY1-PTNUM
                   MOVE    SYU-PTID      TO  BD00801-ISAM-KEY1-PTID
                   MOVE    SYU-DENPNUM   TO  BD00801-ISAM-KEY1-DENPNUM
                   MOVE    SYU-NYUGAIKBN TO  BD00801-ISAM-KEY1-NYUGAIKBN
               END-IF
      *
               MOVE    SYU-REC       TO  BD00801-SYU-DAT
      *
               WRITE   BD00801-REC
      *
               PERFORM 900-DBFETCH-SEC
      *
           END-PERFORM
      *
           PERFORM 900-DBCLOSE-SEC
      *
           CLOSE   BD00801-FILE
      *
           .
       120-BD00801-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    INPUT  BD00801-FILE
      *
           PERFORM 900-BD00801-READ-SEC
      *
           PERFORM UNTIL   FLG-END     =   1
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
           CLOSE   BD00801-FILE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
           ADD     1               TO  CNT-PAGE
      *
           INITIALIZE                  HCM41
      *
           IF      ORCBD008-SYORIKBN   =   "1" OR "3"
               MOVE    "伝票発行日"    TO  HCM41-MIDASI
           ELSE
               MOVE    "診療年月日"    TO  HCM41-MIDASI
           END-IF
      *
           MOVE    WRK-SYORIYMDWH  TO  HCM41-SRYYMD
           MOVE    WRK-SYSYMDWH    TO  HCM41-SYSYMD
      *
           IF    ( ORCBD008-SYORIKBN   =   "1"  OR "2" )
               IF      ORCBD008-NYUGAIKBN  =   "1"
                   MOVE    "（入院）"  TO  HCM41-NYUGAIKBN
               ELSE
                   MOVE    "（外来）"  TO  HCM41-NYUGAIKBN
               END-IF
           END-IF
      *
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCM41-PAGE
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
           PERFORM UNTIL   FLG-END     =   1
                   OR      CNT-LINE    =   30
      *    患者情報読み込み
      *         INITIALIZE                  PTINF-REC
      *         MOVE    SYU-HOSPID    TO  PTINF-HOSPID
      *         MOVE    SYU-PTID      TO  PTINF-PTID
      *         PERFORM 900-PTINF-READ-SEC
      *         IF    ( FLG-PTINF               =   ZERO )
      *          AND  ( PTINF-TSTPTNUMKBN   NOT =   "1" )
                   PERFORM 3201-BODY-HEN-SEC
      *         END-IF
      *
               PERFORM 900-BD00801-READ-SEC
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-BODY-HEN-SEC            SECTION.
      *
               ADD     1               TO  CNT-LINE
      *    連番
               ADD     1               TO  WRK-RENNUM
               MOVE    WRK-RENNUM      TO  WRK-RENNUM-X
               MOVE    WRK-RENNUM-X    TO  HCM41-RENNUM(CNT-LINE)
      *    患者番号
               MOVE    BD00801-PTNUM
                                       TO  HCM41-PTNUM(CNT-LINE)
      *    氏名
               MOVE    PTINF-NAME      TO  HCM41-NAME(CNT-LINE)
      *    保険点数
               EVALUATE    SYU-SYUHKNNUM
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                   WHEN    WRK-CONS-ROUSAI-HKNNUM
                       MOVE    ZERO        TO  SYU-TOTAL-HKNTEN
                   WHEN    OTHER
                       MOVE    SYU-TOTAL-HKNTEN
                                           TO  WRK-Z8
                       MOVE    WRK-Z8      TO  HCM41-HKNTEN (CNT-LINE)
               END-EVALUATE
      *    保険別金額（社保本人〜自賠責）
               MOVE    ZERO                TO  IDX
               EVALUATE    SYU-SYUHKNNUM
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
                   WHEN    "068"
                   WHEN    "069"
                       IF      SYU-KOH1HKNNUM      =   "027"
                           MOVE    5                   TO  IDX
                       ELSE
                           MOVE    4                   TO  IDX
                       END-IF
                   WHEN    OTHER
                       IF  SYU-SYUHKNNUM(1:2) = "98"
                           MOVE    7                   TO  IDX
                       ELSE
                         IF      SYU-KOH1HKNNUM      =   "027"
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
      *            自費以外のとき自費分だけ集計
                   IF      IDX             =   7
                       MOVE    ZERO            TO  WRK-JIHI
                   ELSE
      *----(03.04.01)--UPD-START---
                       COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                           +   SYU-TOTAL-TGMONEY-TAX
                                           +   SYU-JIHI-TOTAL
                                           +   SYU-JIHI-TOTAL-TAX
                                           +   SYU-RMSAGAKU
                                       +   SYU-SKYMONEY-SKJ-JIHI-KEI
                       IF      SYU-SRYYMD  <   CONST-H190401
                           COMPUTE WRK-JIHI    =   WRK-JIHI
                                               +   SYU-JIHI-TAX
                       END-IF
      *----(03.04.01)--UPD-END-----
                   END-IF
                   IF    IDX  =  8  OR  9
                       COMPUTE WRK-MONEY   =   SYU-RSIJIBAI-SKYMONEY
                                           -   WRK-JIHI
                   ELSE
                       COMPUTE WRK-MONEY   =   SYU-SKYMONEY
                                           -   WRK-JIHI                  
                       IF   WRK-JIHI  NOT =  ZERO
                         IF  ((SYU-DISCOUNT-TEIRITU  NOT =  SPACE) AND
                              (SYU-DISCOUNT-RATE     NOT =  ZERO ))
                         OR  ( SYU-CHOSEI            NOT =  ZERO  )
                            CONTINUE
                         ELSE
                            COMPUTE WRK-MONEY  =  SYU-HKNTEKMONEY
                                               +  SYU-RJN-FTN
                                               +  SYU-KOH-FTN
                                               +  SYU-YKZ-FTN
                                               +  SYU-SKYMONEY-SKJ-KEI
                            COMPUTE WRK-JIHI   =  SYU-SKYMONEY
                                               -  WRK-MONEY
                         END-IF
                       END-IF
                   END-IF
      *
                   IF      WRK-MONEY       <   0
                       MOVE    WRK-MONEY   TO  WRK-Z99
                       MOVE    WRK-Z99     TO  HCM41-MONEY(IDX CNT-LINE)
                   ELSE
                       MOVE    WRK-MONEY   TO  WRK-Z9
                       MOVE    WRK-Z9      TO  HCM41-MONEY(IDX CNT-LINE)
                   END-IF
                   IF      IDX             =   7
                       CONTINUE
                   ELSE
                       IF      WRK-JIHI    >   ZERO
                           MOVE    WRK-JIHI
                                           TO  WRK-Z9
                           MOVE    "*"     TO  WRK-Z9 (2:1)
                           MOVE    WRK-Z9  TO  HCM41-MONEY (7 CNT-LINE)
                       END-IF
                   END-IF
               END-IF
      *
               ADD     SYU-TOTAL-HKNTEN  TO  SUM-HKNTEN (1)
               ADD     SYU-SKYMONEY      TO  SUM-MONEY  (1)
               ADD     1                   TO  SUM-KENSU  (1)
               IF      IDX                 >   ZERO
                   COMPUTE IDY     =   IDX     +   1
                   IF      IDX     <   8
                       ADD     SYU-TOTAL-HKNTEN
                                               TO  SUM-HKNTEN (IDY)
                   END-IF
                   ADD     WRK-MONEY       TO  SUM-MONEY(IDY)
                   ADD     1               TO  SUM-KENSU(IDY)
                   ADD     WRK-JIHI        TO  SUM-MONEY(8)
               END-IF
      *
           .
       3201-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC    SECTION.
      *
      *   保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYU-HOSPID    TO  COMB-HOSPID
           MOVE    SYU-PTID      TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM
                                   TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *    患者保険情報読み込み
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYU-HOSPID    TO  PTHKN-HOSPID
               MOVE    SYU-PTID      TO  PTHKN-PTID
               MOVE    COMB-HKNID      TO  PTHKN-HKNID
               PERFORM 900-PTHKNINF-READ-SEC
           END-IF
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *          *    帳票編集<合計>処理
      *****************************************************************
       330-TERM-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      IDX                 <   9
                   MOVE    SUM-HKNTEN(IDX) TO  WRK-Z8
                   MOVE    WRK-Z8          TO  HCM41-GHKNTEN(IDX)
               END-IF
               IF      SUM-MONEY (IDX)     <   0
                   MOVE    SUM-MONEY(IDX)  TO  WRK-Z99
                   MOVE    WRK-Z99         TO  HCM41-GMONEY(IDX)
               ELSE
                   MOVE    SUM-MONEY(IDX)  TO  WRK-Z9 
                   MOVE    WRK-Z9          TO  HCM41-GMONEY(IDX)
               END-IF
               MOVE    SUM-KENSU(IDX)  TO  WRK-Z8
               MOVE    WRK-Z8          TO  HCM41-GKENSU(IDX)
           END-PERFORM
      *
           .
       331-TOTAL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE    "HCM41.red"     TO  SPRT-PRTID
           IF      ORCBD008-SYORIKBN   =   "1" OR "3"
               MOVE    "日計表（伝票発行日）"
                                       TO  SPRT-TITLE
           ELSE
               MOVE    "日計表（診療年月日）"
                                       TO  SPRT-TITLE
           END-IF
           MOVE    HCM41            TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
           IF      SPRT-RETURN          =   ZERO
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
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR     =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
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
               CALL    "ORCSJOB"           USING
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
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BD00801PARA     TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
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
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ　伝票発行日で検索
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYU-REC
                                       WRK-PATH
      *
           MOVE    WRK-PARA-HOSPID TO  SYU-HOSPID
           EVALUATE    ORCBD008-SYORIKBN
           WHEN    "1"
               MOVE    ORCBD008-NYUGAIKBN      TO  SYU-NYUGAIKBN
               MOVE    ORCBD008-SYORIYMD       TO  SYU-DENPPRTYMD
               MOVE    PATH-TBL-SYUNOU-KEY43   TO  WRK-PATH
           WHEN    "2"
               MOVE    ORCBD008-NYUGAIKBN      TO  SYU-NYUGAIKBN
               MOVE    ORCBD008-SYORIYMD       TO  SYU-SRYYMD
               MOVE    PATH-TBL-SYUNOU-KEY44   TO  WRK-PATH
           WHEN    "3"
               MOVE    ORCBD008-SYORIYMD       TO  SYU-DENPPRTYMD
               MOVE    PATH-TBL-SYUNOU-KEY43   TO  WRK-PATH
               MOVE    "%"                     TO  SYU-NYUGAIKBN
           WHEN    "4"
               MOVE    ORCBD008-NYUGAIKBN      TO  SYU-NYUGAIKBN
               MOVE    ORCBD008-SYORIYMD       TO  SYU-SRYYMD
               MOVE    PATH-TBL-SYUNOU-KEY44   TO  WRK-PATH
           WHEN    OTHER
               MOVE    ORCBD008-SYORIYMD       TO  SYU-SRYYMD
               MOVE    PATH-TBL-SYUNOU-KEY44   TO  WRK-PATH
               MOVE    "%"                     TO  SYU-NYUGAIKBN
           END-EVALUATE
      *
           MOVE    SYU-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-PATH        TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納を検索処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    WRK-PATH        TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
      *
               MOVE    ZERO            TO  FLG-SYUNOU
               MOVE    MCPDATA-REC     TO  SYU-REC
      *
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    WRK-PATH        TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
      *    MOVE    WRK-SYSYMD      TO  ORC-DBYMD
           MOVE    ORCBD008-SYORIYMD  TO  ORC-DBYMD
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
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
      *    収納ファイル読込処理
      *****************************************************************
       900-BD00801-READ-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL   FLG-OK      =   1
               READ    BD00801-FILE        NEXT
               AT  END
                   MOVE    1               TO  FLG-END
                   MOVE    1               TO  FLG-OK
               NOT AT  END
                   MOVE    ZERO            TO  FLG-END
                   MOVE    BD00801-SYU-DAT TO  SYU-REC
      *            患者情報読み込み
                   INITIALIZE                  PTINF-REC
                   MOVE    SYU-HOSPID    TO  PTINF-HOSPID
                   MOVE    SYU-PTID      TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF    ( FLG-PTINF               =   ZERO )
                    AND  ( PTINF-TSTPTNUMKBN   NOT =   "1" )
                       MOVE    1               TO  FLG-OK
                   ELSE
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               END-READ
           END-PERFORM
      *
           .
       900-BD00801-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "PTINF-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "PTINF-KEY"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "PTINF-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者番号マスター読込処理
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "PTNUM-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "PTNUM-KEY"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
                   MOVE    ZERO            TO  FLG-PTNUM
               ELSE
                   MOVE    1               TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "PTNUM-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC       SECTION.
      *
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "PTHKNINF-KEY"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC       SECTION.
      *
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "HKNCOMBI-KEY"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
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
