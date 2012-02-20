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
       PROGRAM-ID.                 ORCBG017.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 診療行為別集計表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/06/21    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *  04.03.01    NACL-森脇    08/08/29  パラメタ増加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    診療行為集計ファイル
           SELECT  IN01-FILE       ASSIGN  BG0171
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  IN01-KEY
                                   FILE    STATUS  IS  STS-IN01.
      *
      *    診療会計ファイル
           SELECT  IN02-FILE       ASSIGN  BG0172
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  IN02-KEY
                                   FILE    STATUS  IS  STS-IN02.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    診療行為集計ファイル
       FD  IN01-FILE.
       01  IN01-REC.
           03  IN01-KEY.
               05  IN01-KEY-SRYKBN     PIC X(02).
               05  IN01-KEY-SRYKA      PIC X(02).
               05  IN01-KEY-SRYKBN2    PIC X(02).
               05  IN01-KEY-DATAKBN    PIC X(01).
               05  IN01-KEY-KANANAME   PIC X(100).
               05  IN01-KEY-SRYCD      PIC X(09).
               05  IN01-KEY-SURYO      PIC 9(05)V999.
           03  IN01-NAME               PIC X(100).
           03  IN01-TEN                PIC 9(09)V99.
           03  IN01-TENSIKIBETU        PIC 9(01).
           03  IN01-TANI               PIC X(10).
           03  IN01-KAISU              PIC 9(06).
      *
      *    診療会計ファイル
       FD  IN02-FILE.
       01  IN02-REC.
           COPY    "CPSRYACCT.INC"         REPLACING   //ACCT-//
                                           BY          //IN02-//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *    診療行為集計ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01PARA//
                                   BY          //BG0171//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    診療会計ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01PARA//
                                   BY          //BG0172//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
           COPY    "HCG017.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-IN01            PIC X(02).
           03  STS-IN02            PIC X(02).
           03  STS-BG01DAT         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-IN01            PIC 9(01).
           03  FLG-LOOP-END        PIC 9(01).
           03  FLG-BG01DAT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-TARGET          PIC 9(01).
           03  FLG-TOUYAKU         PIC 9(01).
           03  FLG-CHUSYA          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(03).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-SEQ             PIC 9(06).
           03  CNT-INPUT-ITEM      PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDX5                PIC 9(05).
           03  IDX6                PIC 9(05).
           03  IDX-NACCT           PIC 9(05).
           03  IDX-NSRY            PIC 9(05).
           03  IDX-SRYKBN          PIC 9(05).
           03  IDX-SML             PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  RECEERR             PIC X(100).
           03  WRK-PARA-SRYYM      PIC X(06).
           03  WRK-PARA-NYUGAIKBN  PIC X(01).
           03  WRK-PARA-SRYKA      PIC X(02).
           03  WRK-PARA-SRYKBN     PIC X(02)
                                   OCCURS  7.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-BG01DAT-FILE    PIC X(1024).
           03  WRK-RECEERR-G.
               05  WRK-RECEERR     PIC X(200).
               05  WRK-RECEERR-END PIC X(1).
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-SYMD            PIC X(08).
           03  WRK-EDTYMD1         PIC X(10).
           03  WRK-EDTYMD3         PIC X(22).
           03  WRK-SRYYM-N         PIC X(22).
           03  WRK-SAKYMD-N        PIC X(22).
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-ZZZZ9           PIC ZZZZ9.
           03  WRK-ZZZZZ9          PIC ZZZZZ9.
           03  WRK-ZZZZZZZZZZ9M    PIC ----------9.
           03  WRK-SRYKA           PIC X(02).
           03  WRK-ITEM            PIC X(20).
           03  WRK-STR             PIC X(20).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-KAISU           PIC 9(03).
           03  WRK-PTID            PIC 9(10).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-CHK-SRYKBN      PIC X(02).
           03  WRK-SMLNUM-G.
               05  WRK-SMLLEN-MAX  PIC 9(03).
               05  WRK-SMLNUMIN    PIC ZZZZZZZZ9.999.
               05  WRK-SMLNUMOT    PIC X(10).
               05  WRK-SMLNUMTMP   PIC X(10).
           03  WRK-SIKIBETU        PIC X(02).
           03  WRK-TEN             PIC S9(09).
           03  WRK-TENX            PIC X(11).
           03  WRK-IN-TEN          PIC 9(09)V99.
           03  WRK-OT-TEN          PIC X(18).
           03  WRK-KIZAMI          PIC 9(09)V999.
           03  WRK-SHO             PIC 9(09).
           03  WRK-AMARI           PIC 9(09).
      *
       01  TSRYKBN-AREA.
           03  TSRYKBN-MAX         PIC 9(03).
           03  TSRYKBN-OCC         OCCURS  14.
               05  TSRYKBN-SRYKBN  PIC X(02).
      *
       01  TPTINF-AREA.
        02 TPTINF-MAX                      PIC 9(05).
        02 TPTINF-CHK-HOSPNUM              PIC 9(02).
        02 TPTINF-CHK-PTID                 PIC 9(10).
        02 TPTINF-FLG                      PIC 9(01).
        02 TPTINF-IDX                      PIC 9(05).
        02 TPTINF-OCC                      OCCURS  1000.
           COPY    "CPPTINF.INC"           REPLACING   //PTINF-//
                                           BY          //TPTINF-//.
           03  TPTINF-PTNUM                PIC X(20).
      *
       01  KEY-AREA.
           03  KEY-SRYKA                   PIC X(02).
           03  KEY-SRYKBN                   PIC X(09).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-H2004                 PIC X(06)   VALUE "200804".
           03  CONST-GAZOU-TANICD          PIC X(03)   VALUE "006".
           03  CONST-LINE-MAX              PIC 9(05)   VALUE 80.
           03  CONST-TPTINF-MAX            PIC 9(05)   VALUE 1000.
           03  CONST-TSRYKBN-MAX           PIC 9(05)   VALUE 14.
           03  CONST-SRYCD-MAX             PIC 9(05)   VALUE 5.
           03  CONST-NSRYACT-MAX           PIC 9(05)   VALUE 20.
           03  CONST-PARA-SRYKBN-MAX       PIC 9(05)   VALUE 7.
           03  CONST-SRYKBN-G.
               05  CONST-SRYKBN-VAL.
                   07              PIC X(08)   VALUE   "11000000".
                   07              PIC X(14)   VALUE   "初診".
                   07              PIC X(08)   VALUE   "12000000".
                   07              PIC X(14)   VALUE   "再診".
                   07              PIC X(08)   VALUE   "13000000".
                   07              PIC X(14)   VALUE   "医学管理".
                   07              PIC X(08)   VALUE   "14000000".
                   07              PIC X(14)   VALUE   "在宅".
                   07              PIC X(08)   VALUE   "20000000".
                   07              PIC X(14)   VALUE   "投薬".
                   07              PIC X(08)   VALUE   "30000000".
                   07              PIC X(14)   VALUE   "注射".
                   07              PIC X(08)   VALUE   "40000000".
                   07              PIC X(14)   VALUE   "処置".
                   07              PIC X(08)   VALUE   "50000000".
                   07              PIC X(14)   VALUE   "手術".
                   07              PIC X(08)   VALUE   "54000000".
                   07              PIC X(14)   VALUE   "麻酔".
                   07              PIC X(08)   VALUE   "60200804".
                   07              PIC X(14)   VALUE   "検査、病理診断".
                   07              PIC X(08)   VALUE   "60000000".
                   07              PIC X(14)   VALUE   "検査".
                   07              PIC X(08)   VALUE   "70000000".
                   07              PIC X(14)   VALUE   "画像診断".
                   07              PIC X(08)   VALUE   "80000000".
                   07              PIC X(14)   VALUE   "その他".
                   07              PIC X(08)   VALUE   "90000000".
                   07              PIC X(14)   VALUE   "入院".
               05  CONST-SRYKBN-R    REDEFINES   CONST-SRYKBN-VAL.
                   07  CONST-SRYKBN-OCC        OCCURS  14.
                       09  CONST-SRYKBN        PIC X(02).
                       09  CONST-SRYKBNSTYM    PIC X(06).
                       09  CONST-SRYKBNNAME    PIC X(14).
           03  CONST-SRYKBN-MAX        PIC 9(03)   VALUE   14.
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPTTNS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK1005.INC".
      *
      *    入院診療会計マスター
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
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
      *    パス指定
           COPY    "CPMKPASS.INC".
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
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    指定診療行為件数調サブ２
           COPY    "CPORCSS009.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ****************************************************************
       PROCEDURE               DIVISION
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
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
            STOP   RUN
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  TSRYKBN-AREA
           INITIALIZE                  TPTINF-AREA
           INITIALIZE                  TTNS-AREA
           INITIALIZE                  SPA-AREA
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
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-SRYYM
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-SRYKA
                                               WRK-PARA-SRYKBN (1)
                                               WRK-PARA-SRYKBN (2)
                                               WRK-PARA-SRYKBN (3)
                                               WRK-PARA-SRYKBN (4)
                                               WRK-PARA-SRYKBN (5)
                                               WRK-PARA-SRYKBN (6)
                                               WRK-PARA-SRYKBN (7)
grpsys     END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBG017"      TO  JOB-PGID
           MOVE    "診療行為別集計表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    WRK-PARA-HOSPNUM TO BG0171-HOSPNUM
           MOVE    "BG01701"       TO  BG0171-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BG0171-TERMID
      *
           MOVE    WRK-PARA-HOSPNUM TO BG0172-HOSPNUM
           MOVE    "BG01702"       TO  BG0172-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BG0172-TERMID
      *
      *    パラメタ編集処理
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
           MOVE    WRK-PARA-SRYYM      TO  WRK-SRYYMD
           MOVE    "01"                TO  WRK-SRYYMD(7:)
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               CONTINUE
           ELSE
               MOVE    "2"             TO  WRK-PARA-NYUGAIKBN
           END-IF
      *
      *    対象年月日編集
           MOVE    WRK-SRYYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3 (1:16)  TO  WRK-SRYYM-N
      *
      *    作成日編集
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-SAKYMD-N
      *
           PERFORM 1101-TSRYKBN-HEN-SEC
      *
           IF    ( WRK-PARA-SRYKA  NOT =   SPACE )
               INITIALIZE              SYSKANRI-REC
               MOVE    "1005"          TO  SYS-KANRICD
               MOVE    WRK-PARA-SRYKA  TO  SYS-KBNCD
               MOVE    WRK-SRYYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "診療科を正しく設定してください"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           IF ( FLG-END                =   ZERO )
               IF     ( TSRYKBN-MAX    =   ZERO )
                AND   ( FLG-TOUYAKU    =   ZERO )
                AND   ( FLG-CHUSYA     =   ZERO )
                   IF    ( CNT-INPUT-ITEM  =   ZERO )
                       MOVE    "診療区分が未入力です"
                                       TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   ELSE
                       MOVE    "診療区分の入力内容を確認してください"
                                       TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分テーブル編集処理
      *****************************************************************
       1101-TSRYKBN-HEN-SEC             SECTION.
      *
           MOVE    ZERO            TO  CNT-INPUT-ITEM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0            >   CONST-PARA-SRYKBN-MAX )
                   OR    ( TSRYKBN-MAX     >=  CONST-TSRYKBN-MAX )
               IF    ( WRK-PARA-SRYKBN (IDX0)  =   "*" )
                   COMPUTE CNT-INPUT-ITEM  =   CNT-INPUT-ITEM  +   1
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   CONST-SRYKBN-MAX )
                       COMPUTE TSRYKBN-MAX =   TSRYKBN-MAX  +   1
                       MOVE    CONST-SRYKBN (IDX1)
                                       TO  TSRYKBN-SRYKBN (TSRYKBN-MAX)
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-PARA-SRYKBN-MAX )
                    OR   ( TSRYKBN-MAX
                                   >=  CONST-TSRYKBN-MAX )
      *
               IF    ( WRK-PARA-SRYKBN (IDX0)  NOT =   SPACE )
      *
                   COMPUTE CNT-INPUT-ITEM
                       =   CNT-INPUT-ITEM  +   1
      *
      *
                   PERFORM VARYING IDX1    FROM    1   BY  1
                               UNTIL ( IDX1
                                           >   CONST-SRYKBN-MAX )
                       IF    ( WRK-PARA-SRYKBN (IDX0)
                                           =   CONST-SRYKBN (IDX1) )
      *
                           COMPUTE TSRYKBN-MAX
                               =   TSRYKBN-MAX  +   1
      *
                           MOVE    CONST-SRYKBN (IDX1)
                                       TO  TSRYKBN-SRYKBN
                                                (TSRYKBN-MAX)
      
                           MOVE    CONST-SRYKBN-MAX
                                       TO  IDX1
      *
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       1101-TSRYKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-IN01-EDIT-SEC
      *
           OPEN    INPUT   IN01-FILE
      *
           MOVE    ZERO            TO  FLG-IN01
           PERFORM 900-IN01SEQ-READ-SEC
      *
           PERFORM UNTIL ( FLG-IN01 NOT =   ZERO )
                     OR  ( FLG-END  NOT =   ZERO )
      *
      *        ヘッダー編集処理
               PERFORM 2001-HEAD-HEN-SEC
      *
      *        明細行編集処理
               PERFORM 2001-BODY-HEN-SEC 
      * 
      *        印刷処理
               PERFORM 2001-PRINT-SEC
      *
           END-PERFORM
      *
           CLOSE    IN01-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為集計ファイル編集処理
      *****************************************************************
       2001-IN01-EDIT-SEC              SECTION.
      *
           OPEN    OUTPUT  IN01-FILE
           CLOSE           IN01-FILE
      *
           OPEN    OUTPUT  IN02-FILE
           CLOSE           IN02-FILE
      *
           OPEN    I-O     IN01-FILE
           OPEN    I-O     IN02-FILE
      *
           PERFORM 20011-SRYACT-GET-SEC
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               PERFORM 20012-NYUACT-GET-SEC
           END-IF
      *
           CLOSE           IN01-FILE
                           IN02-FILE
      *
           .
       2001-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為取得処理
      *****************************************************************
       20011-SRYACT-GET-SEC            SECTION.
      *
           PERFORM 900-SRYACT-KEY14-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
               IF   (  WRK-PARA-SRYKA  NOT =   SPACE  )
                AND (  WRK-PARA-SRYKA  NOT =   SRY-SRYKA  )
                   CONTINUE
               ELSE
      *
                   MOVE    SRY-SRYKBN  TO  WRK-SRYKBN
      *
                   EVALUATE    TRUE
                   WHEN    ( WRK-SRYKBN  =   "90" OR "92" OR "97" )
                       MOVE    "90"    TO  WRK-SRYKBN
		   WHEN    ( WRK-SRYKBN (1:1)     =  "2" )
                       MOVE    "20"    TO  WRK-SRYKBN
		   WHEN    ( WRK-SRYKBN (1:1)     =  "3" )
                       MOVE    "30"    TO  WRK-SRYKBN
		   WHEN    ( WRK-SRYKBN =  "64" )
                       MOVE    "60"    TO  WRK-SRYKBN
                   END-EVALUATE
      *
                   MOVE    WRK-SRYKBN  TO  WRK-CHK-SRYKBN
      *
                   PERFORM 20011-CHK-SRYKBN-SEC
                   IF    ( FLG-TARGET  =   1 )
      *
                       PERFORM VARYING IDX3    FROM    1   BY  1
                               UNTIL ( IDX3    >   CONST-SRYCD-MAX )
      *
                           IF    ( SRY-SRYCD (IDX3)
                                           NOT =   SPACE )
                               PERFORM 20011-IN01-HEN-SEC
                           END-IF
      *
                       END-PERFORM
                   END-IF
      *
               END-IF
      *
               PERFORM 900-SRYACT-KEY14-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key14"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-SRYACT-GET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為集計ファイル編集処理
      *****************************************************************
       20011-CHK-SRYKBN-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-TARGET
           PERFORM VARYING IDX-SRYKBN  FROM    1   BY  1
                   UNTIL ( IDX-SRYKBN  >   TSRYKBN-MAX )
                     OR  ( FLG-TARGET  =   1 )
      *
               IF    ( WRK-CHK-SRYKBN  =   TSRYKBN-SRYKBN (IDX-SRYKBN))
                   MOVE    1       TO  FLG-TARGET
               END-IF
      *
           END-PERFORM
      *
           .
       20011-CHK-SRYKBN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為集計ファイル編集処理
      *****************************************************************
       20011-IN01-HEN-SEC              SECTION.
      *
           PERFORM 900-SRYACCT-KEY-SEL-SEC
           IF    ( FLG-SRYACCT     NOT =   ZERO )
               DISPLAY "SRYACCT SEL ERR !! " SRY-PTID ":" SRY-ZAINUM
           END-IF
           IF    ( ACCT-ZAIKBN     =   1 ) OR
                 ( ACCT-ZAIKBN     =   2 )
               GO  TO  20011-IN01-HEN-EXT
           END-IF
      *
           MOVE    SRY-PTID            TO  WRK-PTID
           PERFORM 900-PTINF-PTNUM-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               DISPLAY "PTINF   SEL ERR !! " SRY-PTID
           END-IF
      *
           MOVE    SRY-SRYCD (IDX3)    TO  WRK-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               DISPLAY "TENSU   SEL ERR !! " WRK-SRYCD
           END-IF
      *
           PERFORM 200111-TARGET-CHK-SEC
           IF    ( FLG-TARGET          =   1 )
               PERFORM 200111-IN01-HEN-SEC
           END-IF
      *
           .
       20011-IN01-HEN-EXT.
           EXIT.
      *****************************************************************
      *    対象チェック処理
      *****************************************************************
       200111-TARGET-CHK-SEC           SECTION.
      *
           MOVE    1               TO  FLG-TARGET
      *
           IF    ( ACCT-ZAIKAISU   <   1 )
               MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           IF    ( PTINF-TSTPTNUMKBN   =   "1" )
               MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           IF    ( TNS-DATAKBN     =   1 OR 2 )
               CONTINUE
           ELSE
               MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           .
       200111-TARGET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為集計ファイル編集処理
      *****************************************************************
       200111-IN01-HEN-SEC              SECTION.
      *
           INITIALIZE                  IN01-REC
           MOVE    WRK-SRYKBN          TO  IN01-KEY-SRYKBN
           MOVE    SRY-SRYKA           TO  IN01-KEY-SRYKA
           MOVE    SRY-SRYKBN          TO  IN01-KEY-SRYKBN2
           MOVE    TNS-DATAKBN         TO  IN01-KEY-DATAKBN
           MOVE    TNS-KANANAME        TO  IN01-KEY-KANANAME
           MOVE    TNS-SRYCD           TO  IN01-KEY-SRYCD
           MOVE    SRY-SRYSURYO (IDX3) TO  IN01-KEY-SURYO
           PERFORM 900-IN01DYN-READ-SEC
           IF    ( FLG-IN01            =   ZERO )
      *
               PERFORM 200111-IN01-ACCT-HEN-SEC
      *
               REWRITE                   IN01-REC
      *
           ELSE
               MOVE    WRK-SRYKBN          TO  IN01-KEY-SRYKBN
               MOVE    SRY-SRYKA           TO  IN01-KEY-SRYKA
               MOVE    SRY-SRYKBN          TO  IN01-KEY-SRYKBN2
               MOVE    TNS-DATAKBN         TO  IN01-KEY-DATAKBN
               MOVE    TNS-KANANAME        TO  IN01-KEY-KANANAME
               MOVE    TNS-SRYCD           TO  IN01-KEY-SRYCD
               MOVE    SRY-SRYSURYO (IDX3) TO  IN01-KEY-SURYO
               MOVE    TNS-NAME            TO  IN01-NAME
               MOVE    TNS-TEN             TO  IN01-TEN
               MOVE    TNS-TENSIKIBETU     TO  IN01-TENSIKIBETU
               IF    ( TNS-TANINAME        NOT =   SPACE )
                   IF    ( TNS-SRYCD           =   "1" )
                     AND ( WRK-SRYKBN          =   "70" )
                     AND ( TNS-TANICD          =   CONST-GAZOU-TANICD )
                       MOVE    "回"        TO  IN01-TANI
                   ELSE
                       MOVE    TNS-TANINAME TO IN01-TANI
                   END-IF
               END-IF
      *
               PERFORM 200111-IN01-ACCT-HEN-SEC
      *
               WRITE                     IN01-REC
           END-IF
      *
           .
       200111-IN01-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為集計診療会計情報編集処理
      *****************************************************************
       200111-IN01-ACCT-HEN-SEC        SECTION.
      *
           COMPUTE IN01-KAISU   =  IN01-KAISU  +   ACCT-ZAIKAISU
      *
           .
       200111-IN01-ACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為取得処理
      *****************************************************************
       20012-NYUACT-GET-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUINACCT
      *
           PERFORM 900-NYUINACCT-KEY27-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF    ( NACCT-ZAIKAISU  >   ZERO )
                   INITIALIZE          SS009-AREA
                   MOVE    "1"         TO      SS009-EXGAIHAKU
                   CALL    "ORCSS009"  USING   SS009-AREA
                                                   NYUINACCT-REC
                                                   SPA-AREA
                   IF    ( SS009-RC    =   ZERO )
                       PERFORM  20012-NSRYACT-GET-SEC
                   ELSE
                       DISPLAY "ERR SS009-RC:" SS009-RC
                   END-IF
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY27-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20012-NYUACT-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為取得処理
      *****************************************************************
       20012-NSRYACT-GET-SEC           SECTION.
      *
           PERFORM VARYING IDX-NACCT   FROM    1   BY  1
                   UNTIL ( IDX-NACCT   >   SS009-HENKYAKU-MAX )
      *
               MOVE    SS009-SRYKBN (IDX-NACCT)
                                       TO  WRK-SRYKBN
               IF    ( WRK-SRYKBN      =   "90" OR "92" OR "97" )
                   MOVE    "90"        TO  WRK-SRYKBN
               END-IF
      *
               IF   (  WRK-PARA-SRYKA  NOT =   SPACE  )
                AND (  WRK-PARA-SRYKA  NOT =   SS009-SRYKA
                                                          (IDX-NACCT)  )
                   CONTINUE
               ELSE
                   MOVE    WRK-SRYKBN  TO  WRK-CHK-SRYKBN
                   PERFORM 20011-CHK-SRYKBN-SEC
                   IF    ( FLG-TARGET  =   1 )
      *
                       PERFORM VARYING IDX-NSRY    FROM    1   BY  1
                               UNTIL ( IDX-NSRY
                                               >   CONST-NSRYACT-MAX )
      *
                           IF    ( SS009-SRYCD (IDX-NACCT IDX-NSRY)
                                           NOT =   SPACE )
      *
                               PERFORM 20012-IN01-HEN-SEC
      *
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20012-NSRYACT-GET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為集計ファイル編集処理
      *****************************************************************
       20012-IN01-HEN-SEC              SECTION.
      *
           MOVE    NACCT-PTID          TO  WRK-PTID
           PERFORM 900-PTINF-PTNUM-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               DISPLAY "PTINF   SEL ERR !! " SRY-PTID
               GO  TO  20012-IN01-HEN-EXT
           END-IF
      *
      *
           MOVE    SS009-SRYCD (IDX-NACCT IDX-NSRY)
                                       TO  WRK-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               DISPLAY "TENSU   SEL ERR !! " WRK-SRYCD
           END-IF
      *
           PERFORM 200121-TARGET-CHK-SEC
           IF    ( FLG-TARGET          =   1 )
               PERFORM 200121-IN01-HEN-SEC
           END-IF
      *
           .
       20012-IN01-HEN-EXT.
           EXIT.
      *****************************************************************
      *    対象チェック処理
      *****************************************************************
       200121-TARGET-CHK-SEC           SECTION.
      *
           MOVE    1               TO  FLG-TARGET
      *
           IF    ( SS009-ZAIKAISU (IDX-NACCT)
                                       <   1 )
               MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           IF    ( PTINF-TSTPTNUMKBN   =   "1" )
               MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           IF    ( TNS-DATAKBN     =   1 OR 2 )
               CONTINUE
           ELSE
               MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           .
       200121-TARGET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為集計ファイル編集処理
      *****************************************************************
       200121-IN01-HEN-SEC             SECTION.
      *
           INITIALIZE                  IN01-REC
           MOVE    WRK-SRYKBN          TO  IN01-KEY-SRYKBN
           MOVE    SS009-SRYKBN(IDX-NACCT)
                                       TO  IN01-KEY-SRYKBN2
           MOVE    SS009-SRYKA (IDX-NACCT)
                                       TO  IN01-KEY-SRYKA
           MOVE    TNS-DATAKBN         TO  IN01-KEY-DATAKBN
           MOVE    TNS-KANANAME        TO  IN01-KEY-KANANAME
           MOVE    TNS-SRYCD           TO  IN01-KEY-SRYCD
           MOVE    SS009-SURYO (IDX-NACCT IDX-NSRY)
                                       TO  IN01-KEY-SURYO
           PERFORM 900-IN01DYN-READ-SEC
           IF    ( FLG-IN01            =   ZERO )
      *
               PERFORM 200121-IN01-ACCT-HEN-SEC
      *
               REWRITE                     IN01-REC
      *
           ELSE
               INITIALIZE                  IN01-REC
               MOVE    WRK-SRYKBN          TO  IN01-KEY-SRYKBN
               MOVE    SS009-SRYKBN(IDX-NACCT)
                                           TO  IN01-KEY-SRYKBN2
               MOVE    SS009-SRYKA (IDX-NACCT)
                                           TO  IN01-KEY-SRYKA
               MOVE    TNS-DATAKBN         TO  IN01-KEY-DATAKBN
               MOVE    TNS-KANANAME        TO  IN01-KEY-KANANAME
               MOVE    TNS-SRYCD           TO  IN01-KEY-SRYCD
               MOVE    SS009-SURYO (IDX-NACCT IDX-NSRY)
                                           TO  IN01-KEY-SURYO
      *
               MOVE    TNS-NAME            TO  IN01-NAME
               MOVE    TNS-TEN             TO  IN01-TEN
               MOVE    TNS-TENSIKIBETU     TO  IN01-TENSIKIBETU
               IF    ( TNS-TANINAME        NOT =   SPACE )
                   IF    ( TNS-SRYCD
                                               =   "1" )
                     AND ( SS009-SRYKBN(IDX-NACCT)
                                               =   "70" )
                     AND ( TNS-TANICD          =   CONST-GAZOU-TANICD )
                       MOVE    "回"         TO  IN01-TANI
                   ELSE
                       MOVE    TNS-TANINAME TO IN01-TANI
                   END-IF
               END-IF
      *
               PERFORM 200121-IN01-ACCT-HEN-SEC
      *
               WRITE                     IN01-REC
           END-IF
      *
           .
       200121-IN01-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為集計診療会計情報編集処理
      *****************************************************************
       200121-IN01-ACCT-HEN-SEC        SECTION.
      *
           COMPUTE IN01-KAISU
                =  IN01-KAISU
                +  SS009-ZAIKAISU (IDX-NACCT)
      *
           .
       200121-IN01-ACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       2001-HEAD-HEN-SEC               SECTION.
      *
           INITIALIZE                  HCG017
      *
           MOVE    ZERO                TO  CNT-LINE
      *
           MOVE    IN01-KEY-SRYKA      TO  KEY-SRYKA
           MOVE    IN01-KEY-SRYKBN     TO  KEY-SRYKBN
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    KEY-SRYKA           TO  SYS-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           MOVE    SYS-1005-SRYKANAME  TO  HCG017-SRYKA
      *
           MOVE    WRK-SRYYM-N         TO  HCG017-SRYYM
           MOVE    WRK-SAKYMD-N        TO  HCG017-SAKYMD
      *
           PERFORM VARYING IDX-SRYKBN  FROM    1   BY  1
                   UNTIL ( IDX-SRYKBN  >   CONST-SRYKBN-MAX )
                    OR   ( HCG017-SRYKBN   NOT =   SPACE )
      *
               IF    ( CONST-SRYKBN  (IDX-SRYKBN)
                                       =   IN01-KEY-SRYKBN )
		 AND ( WRK-PARA-SRYYM  >=  CONST-SRYKBNSTYM
		                               (IDX-SRYKBN))
                   MOVE    CONST-SRYKBNNAME  (IDX-SRYKBN)
                                       TO  HCG017-SRYKBN
               END-IF
      *
           END-PERFORM
      *
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               MOVE    "（入院）"      TO  HCG017-NYUGAIKBN
           ELSE
               MOVE    "（外来）"      TO  HCG017-NYUGAIKBN
           END-IF
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE        +   1
      *
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCG017-PAGE
      *
           .
      *
       2001-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       2001-BODY-HEN-SEC               SECTION.
      *
           PERFORM UNTIL ( FLG-IN01        =   1 )
                   OR    ( KEY-SRYKBN  NOT =   IN01-KEY-SRYKBN )
                   OR    ( KEY-SRYKA   NOT =   IN01-KEY-SRYKA )
                   OR    ( CNT-LINE       >=   CONST-LINE-MAX )
      *
               PERFORM 20011-BODY-NAIYO-HEN-SEC
      *
               PERFORM 900-IN01SEQ-READ-SEC
      *
           END-PERFORM
      *
           IF    ( FLG-IN01        NOT =   ZERO )
             OR  ( KEY-SRYKBN      NOT =   IN01-KEY-SRYKBN )
             OR  ( KEY-SRYKA       NOT =   IN01-KEY-SRYKA )
      *
               MOVE    ZERO        TO  CNT-SEQ
      *
           END-IF
      *
           .
       2001-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       20011-BODY-NAIYO-HEN-SEC        SECTION.
      *
           COMPUTE CNT-SEQ     =   CNT-SEQ     +   1
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           MOVE    CNT-SEQ             TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9           TO  HCG017-RENNUM(CNT-LINE)
      *
           MOVE    IN01-KEY-SRYCD      TO  HCG017-SRYCD (CNT-LINE)
           MOVE    IN01-NAME           TO  HCG017-NAME  (CNT-LINE)
      *
           MOVE    IN01-TEN            TO  WRK-IN-TEN
           PERFORM 200111-TEN-EDIT-SEC
           MOVE    WRK-OT-TEN          TO  HCG017-TEN (CNT-LINE)
      *
           IF    ( IN01-TANI       NOT =   SPACE )
               MOVE    IN01-KEY-SURYO      TO  WRK-ZZZZ9
                                               WRK-SMLNUMIN
               PERFORM 200111-SMLNUM-EDIT-SEC
               IF    ( WRK-SMLNUMOT        =   "." )
                   STRING  "　"                DELIMITED BY SIZE
                           WRK-ZZZZ9           DELIMITED BY SIZE
                           IN01-TANI           DELIMITED BY SPACE
                   INTO    HCG017-SURYO (CNT-LINE)
                   END-STRING
               ELSE
                   STRING  "　"                DELIMITED BY SIZE
                           WRK-ZZZZ9           DELIMITED BY SIZE
                           WRK-SMLNUMOT        DELIMITED BY SPACE
                           IN01-TANI           DELIMITED BY SPACE
                   INTO    HCG017-SURYO (CNT-LINE)
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    IN01-KAISU          TO  WRK-ZZZZZ9
           MOVE    WRK-ZZZZZ9          TO  HCG017-KENSU (CNT-LINE)
      *
           MOVE    IN01-KEY-SRYCD      TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-IN-TEN
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( TNS-KZMCOMPSIKIBETU     =   1 )
               MOVE    IN01-KEY-SURYO  TO  WRK-KIZAMI
               IF    ( IN01-KEY-SURYO  <   TNS-KZMKGN )
                   MOVE    TNS-KZMKGN  TO  WRK-KIZAMI
               END-IF
      *
               COMPUTE WRK-IN-TEN
                   =   WRK-KIZAMI
                   -   TNS-KZMKGN
      *
               DIVIDE  TNS-KZM INTO WRK-IN-TEN GIVING    WRK-SHO
                                               REMAINDER WRK-AMARI
      *
               IF    ( WRK-AMARI > 0 )
                   COMPUTE WRK-SHO =   WRK-SHO +   1
               END-IF
      *
               COMPUTE WRK-IN-TEN
                   = ( WRK-SHO * TNS-KZMTEN + TNS-TEN )
                   *   IN01-KAISU
      *
           ELSE
               COMPUTE WRK-IN-TEN
                   =   IN01-TEN
                   *   IN01-KEY-SURYO
                   *   IN01-KAISU
           END-IF
           PERFORM 200111-TEN-EDIT-SEC
           IF    ( WRK-SIKIBETU        =   "円" OR "点" )
               MOVE    WRK-OT-TEN      TO  HCG017-KEI (CNT-LINE)
           END-IF
      *
           .
       20011-BODY-NAIYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数計編集処理
      *****************************************************************
       200111-TEN-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-OT-TEN
      *
           MOVE    "　"                TO  WRK-SIKIBETU
           MOVE    WRK-IN-TEN          TO  WRK-TEN
           EVALUATE    IN01-TENSIKIBETU
           WHEN    "1"
               MOVE    "円"            TO  WRK-SIKIBETU
           WHEN    "3"
               MOVE    "点"            TO  WRK-SIKIBETU
           WHEN    "4"
               MOVE    "円"            TO  WRK-SIKIBETU
           WHEN    "5"
               MOVE    "％"            TO  WRK-SIKIBETU
           WHEN    "6"
               MOVE    "％"            TO  WRK-SIKIBETU
               COMPUTE WRK-TEN =   WRK-TEN *   -1
           WHEN    "7"
               MOVE    "点"            TO  WRK-SIKIBETU
               COMPUTE WRK-TEN =   WRK-TEN *   -1
           WHEN    "8"
               MOVE    "点"            TO  WRK-SIKIBETU
               COMPUTE WRK-TEN =   WRK-TEN *   -1
           END-EVALUATE
      *
           MOVE    WRK-TEN             TO  WRK-ZZZZZZZZZZ9M
           MOVE    WRK-ZZZZZZZZZZ9M    TO  WRK-TENX
           IF    ( WRK-TEN             <   ZERO )
               INSPECT WRK-TENX    REPLACING   FIRST   " -"    BY  "▲"
           ELSE
               INSPECT WRK-TENX    REPLACING   FIRST   "  "    BY  "　"
           END-IF
      *
           MOVE    WRK-IN-TEN          TO  WRK-SMLNUMIN
           PERFORM 200111-SMLNUM-EDIT-SEC
      *
           DISPLAY "WRK-SMLNUMOT =" WRK-SMLNUMOT
           DISPLAY "WRK-TENX =" WRK-TENX
           IF    ( WRK-SMLNUMOT        =   "." )
               STRING  "　"                DELIMITED BY SIZE
                       WRK-TENX            DELIMITED BY SIZE
                       WRK-SIKIBETU        DELIMITED BY SPACE
               INTO    WRK-OT-TEN
               END-STRING
           ELSE
               STRING  "　"                DELIMITED BY SIZE
                       WRK-TENX            DELIMITED BY SIZE
                       WRK-SMLNUMOT        DELIMITED BY SPACE
                       WRK-SIKIBETU        DELIMITED BY SPACE
               INTO    WRK-OT-TEN
               END-STRING
           END-IF
           DISPLAY "WRK-OT-TEN =" WRK-OT-TEN
      *
           .
       200111-TEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    少数部編集処理
      *****************************************************************
       200111-SMLNUM-EDIT-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-SMLNUMOT
      *
           UNSTRING    WRK-SMLNUMIN    DELIMITED   BY  "."
           INTO        WRK-SMLNUMTMP   ,
                       WRK-SMLNUMOT
           END-UNSTRING
      *
           MOVE    WRK-SMLNUMOT        TO  WRK-SMLNUMTMP
      *
           MOVE    1                   TO  WRK-SMLLEN-MAX
           MOVE    SPACE               TO  WRK-SMLNUMOT
           STRING  "."                 DELIMITED   BY  SIZE
                   WRK-SMLNUMTMP       DELIMITED   BY  SIZE
           INTO    WRK-SMLNUMOT
           WITH POINTER    WRK-SMLLEN-MAX
           END-STRING
      *
           COMPUTE WRK-SMLLEN-MAX
               =   WRK-SMLLEN-MAX  -   1
      *
           PERFORM VARYING IDX-SML     FROM    WRK-SMLLEN-MAX  BY  -1
                   UNTIL ( IDX-SML     <   1 )
                      OR ((WRK-SMLNUMOT (IDX-SML : 1)  NOT =   SPACE )
                      AND (WRK-SMLNUMOT (IDX-SML : 1)  NOT =   "0" ))
      *
               MOVE    SPACE           TO  WRK-SMLNUMOT (IDX-SML : 1)
      *
           END-PERFORM
      *
      *
           .
       200111-SMLNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2001-PRINT-SEC                  SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-SRYYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE    "HCG017.red"    TO  SPRT-PRTID
           MOVE    "診療行為別集計表"
                                   TO  SPRT-TITLE
           MOVE    HCG017          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN     =   ZERO )
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
      *
       2001-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
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
               MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           MOVE    LOW-VALUE       TO      WRK-RECEERR-END
           CALL    "cobabort"      USING   WRK-RECEERR-G
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
           MOVE    BG0171          TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BG0172          TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
      *
           MOVE    WRK-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    WRK-SRYYMD          TO  TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
      *
      *            点数マスタ退避ワークに編集する
                   PERFORM 9001-TTNS-HEN-SEC
                ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理(KEY)
      *****************************************************************
       900-SRYACCT-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACCT
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SRY-HOSPNUM      TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID        TO  ACCT-PTID
           MOVE    SRY-SRYKA       TO  ACCT-SRYKA
           MOVE    SRY-SRYYM       TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN      TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC     TO  IN02-REC
           READ    IN02-FILE
           INVALID KEY
               MOVE    SRYACCT-REC TO  MCPDATA-REC
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC      =   ZERO )
                   MOVE    MCPDATA-REC     TO  SRYACCT-REC
                                               IN02-REC
                   WRITE   IN02-REC
               ELSE
                   INITIALIZE          SRYACCT-REC
                   MOVE    1       TO  FLG-SRYACCT
               END-IF
      *
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           NOT INVALID KEY
               MOVE    IN02-REC    TO  SRYACCT-REC
           END-READ
      *
           .
       900-SRYACCT-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報、番号検索処理(KEY)
      *****************************************************************
       900-PTINF-PTNUM-KEY-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           MOVE    WRK-PARA-HOSPNUM TO  TPTINF-CHK-HOSPNUM
           MOVE    WRK-PTID        TO  TPTINF-CHK-PTID
           PERFORM 9001-TPTINF-CHK-SEC
      *
           IF    ( TPTINF-FLG      =   ZERO )
               INITIALIZE              PTINF-REC
                                       PTNUM-REC
      *
               MOVE    WRK-PARA-HOSPNUM
                                   TO  PTINF-HOSPNUM
               MOVE    WRK-PTID    TO  PTINF-PTID
               MOVE    PTINF-REC   TO  MCPDATA-REC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC      =   ZERO )
                   MOVE    MCPDATA-REC TO  PTINF-REC
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
      *
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( FLG-PTINF       =   ZERO )
                   MOVE    WRK-PARA-HOSPNUM
                                       TO  PTNUM-HOSPNUM
                   MOVE    WRK-PTID    TO  PTNUM-PTID
                   MOVE    PTNUM-REC   TO  MCPDATA-REC
                   MOVE    "tbl_ptnum"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC      =   ZERO )
                       MOVE    MCPDATA-REC TO  PTNUM-REC
                   ELSE
                       INITIALIZE              PTNUM-REC
                   END-IF
      *
                   MOVE    "tbl_ptnum"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
      *
                   PERFORM 9001-TPTINF-HEN-SEC
      *
               END-IF
      *
           END-IF
      *
           .
       900-PTINF-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報退避エリアチェック処理
      *****************************************************************
       9001-TPTINF-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  TPTINF-FLG
           INITIALIZE                  PTINF-REC
                                       PTNUM-REC
      *
           PERFORM VARYING TPTINF-IDX  FROM    1   BY  1
                   UNTIL ( TPTINF-IDX  >   TPTINF-MAX )
                    OR   ( TPTINF-FLG  =   1 )
      *
               IF    ( TPTINF-HOSPNUM (TPTINF-IDX)
                                       =   TPTINF-CHK-HOSPNUM )
                 AND ( TPTINF-PTID   (TPTINF-IDX)
                                       =   TPTINF-CHK-PTID   )
                   MOVE    1           TO  TPTINF-FLG
                   MOVE    TPTINF-OCC   (TPTINF-IDX)
                                       TO  PTINF-REC
                   MOVE    TPTINF-HOSPNUM (TPTINF-IDX)
                                       TO  PTNUM-HOSPNUM
                   MOVE    TPTINF-PTNUM (TPTINF-IDX)
                                       TO  PTNUM-PTNUM
               END-IF
      *
           END-PERFORM
      *
           .
       9001-TPTINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報退避エリア編集処理
      *****************************************************************
       9001-TPTINF-HEN-SEC             SECTION.
      *
           IF    ( CONST-TPTINF-MAX    >   TPTINF-MAX )
      *
               COMPUTE TPTINF-MAX      =   TPTINF-MAX  +   1
      *
               MOVE    PTINF-REC       TO  TPTINF-OCC   (TPTINF-MAX)
               MOVE    PTNUM-PTNUM     TO  TPTINF-PTNUM (TPTINF-MAX)
      *
           END-IF
      *
           .
       9001-TPTINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為集計ファイル検索処理
      *****************************************************************
       900-IN01DYN-READ-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-IN01
      *
           READ    IN01-FILE
           INVALID KEY
               MOVE    1           TO  FLG-IN01
           END-READ
      *
           .
       900-IN01DYN-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為集計ファイル読込処理
      *****************************************************************
       900-IN01SEQ-READ-SEC            SECTION.
      *
           READ    IN01-FILE   NEXT
           AT  END
               MOVE    1           TO      FLG-IN01
           END-READ
      *
           .
       900-IN01SEQ-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索処理(KEY14)
      *****************************************************************
       900-SRYACT-KEY14-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
           INITIALIZE                  SRYACT-REC
           MOVE    WRK-PARA-HOSPNUM TO  SRY-HOSPNUM
           MOVE    WRK-PARA-SRYYM  TO  SRY-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  SRY-NYUGAIKBN
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key14"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACT-REC
           ELSE
               INITIALIZE              SRYACT-REC
               MOVE    1           TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-KEY14-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為FETCH処理(KEY14)
      *****************************************************************
       900-SRYACT-KEY14-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key14"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACT-REC
           ELSE
               INITIALIZE              SRYACT-REC
               MOVE    1           TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-KEY14-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY27)
      *****************************************************************
       900-NYUINACCT-KEY27-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                  NYUINACCT-REC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY27-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理(KEY27)
      *****************************************************************
       900-NYUINACCT-KEY27-FET-SEC     SECTION.
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY27-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA

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
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
