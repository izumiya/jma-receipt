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
       PROGRAM-ID.             ORCBQ01B.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 患者一覧出力（診療行為）（ＯＲＣＢＱ０１Ｂ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/12/24    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付       内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  BQ01-FILE   ASSIGN  PRFNAME
                                   ORGANIZATION    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-BQ01.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PRT.
      *
      *    ＣＳＶファイル
           SELECT  CSV-FILE        ASSIGN  CSVNAME
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-CSV.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメータファイル
       FD  BQ01-FILE.
           COPY    "CPBQ01.INC".
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTID               PIC X(30).
           03  PRT-PRTNM               PIC X(20).
           03  PRT-OFFSET-X            PIC X(05).
           03  PRT-OFFSET-Y            PIC X(05).
           03  PRT-SITEKBN             PIC X(01).
           03  PRT-PRTDATA             PIC X(20000).
      *
      *    ＣＳＶファイル
       FD  CSV-FILE.
       01  CSV-REC                     PIC X(2000).
      *
       WORKING-STORAGE                 SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    シェル用領域
      *    COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *
      *    ワークパラメータファイル
           COPY    "CPBQ01.INC"
                                   REPLACING  //BQ01//
                                   BY         //WKBQ01//.
      *
      *
       01  ASSIGN-NAME-AREA.
      *        パラメータファイル名
           03  PRFNAME                 PIC X(100).
      *        ＣＳＶファイル名
           03  CSVNAME                 PIC X(100).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-BQ01                PIC X(02).
           03  STS-PRT                 PIC X(02).
           03  STS-CSV                 PIC X(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX1                    PIC 9(09).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDX-LEN                 PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTSRH               PIC 9(01).
           03  FLG-EDIT                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(09).
           03  CNT-LINE                PIC 9(05).
           03  CNT-RENNUM              PIC 9(05).
           03  CNT-KNS                 PIC 9(05).
           03  CNT-CSV                 PIC 9(09).
      *
      *    一時領域
       01  WRK-AREA. 
           03  WRK-STR                 PIC X(20000).
           03  WRK-STR-LEN             PIC 9(05).
           03  WRK-STR-PNT             PIC 9(05).
           03  WRK-MAE-INPUT           PIC X(400).
           03  WRK-OUT-INPUT           PIC X(400).
           03  WRK-OUT-MAX             PIC 9(05).
           03  WRK-PTINF-ADR-LEN       PIC 9(05).
           03  WRK-PTINF-BANTI-LEN     PIC 9(05).
           03  WRK-ADRS                PIC X(400).
           03  WRK-PARA.
               05  WRK-PARA-PAFNAME
                                       PIC X(100).
               05  WRK-PARA-TERMID     PIC X(64).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-SYSTIME    PIC X(08).
               05  WRK-PARA-HOSPNUM     PIC 9(02).
               05  WRK-PARA-STAFFCD    PIC X(05).
               05  WRK-PARA-FILE       PIC X(01).
               05  WRK-PARA-FILENM     PIC X(100).
               05  WRK-PARA-FILENM-FOLDER
                                       PIC X(100).
               05  WRK-PARA-CSVFILENM  PIC X(100).
           03  WRK-PROC-KNS            PIC 9(09).
           03  WRK-ZZZZ9               PIC ZZZZ9.
           03  WRK-ZZZZ9C              PIC ZZ,ZZ9.
           03  WRK-ZZZZZZZ9            PIC ZZZZZZZ9.
           03  WRK-ZZZZZZZ9C           PIC ZZ,ZZZ,ZZ9.
           03  WRK-SYMD                PIC X(08).
           03  WRK-HENYMDG1            PIC X(09).
           03  WRK-HENYMDG3            PIC X(22).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PRTNM               PIC X(20).
           03  WRK-CALC-YMD1.
               05  WRK-CALC-YY1                    PIC 9(04).
               05  WRK-CALC-MM1                    PIC 9(02).
               05  WRK-CALC-DD1                    PIC 9(02).
      *
           03  WRK-CALC-YMD2.
               05  WRK-CALC-YY2                    PIC 9(04).
               05  WRK-CALC-MM2                    PIC 9(02).
               05  WRK-CALC-DD2                    PIC 9(02).
           03  WRK-AGE-YY                          PIC S9(04).
           03  WRK-AGE-MM                          PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z                       PIC ---9.
               05  WRK-AGE-MEI                     PIC X(04).
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-ENCODING            PIC X(01).
           03  WRK-ENCODING9           REDEFINES WRK-ENCODING
                                       PIC 9(01).
           03  WRK-SRYSURYOX.
               05  WRK-SRYSURYOZ       PIC ZZZZ9.99999.
           03  WRK-ZAIKAISUZ           PIC ZZZZ9.
           03  WRK-SURYOTOTALX.
               05  WRK-SURYOTOTALZ     PIC ZZZZZZ9.99999.
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC 9(02).
           03  WRK-DAY-G.
               05  WRK-DAY                             PIC 9(02).
           03  WRK-NISSU-DAY-G.
                   07  WRK-NISSU-DAY                   PIC 9(01)
                                                       OCCURS  31.
           03  WRK-NISSU-AREA.
               05  WRK-NISSU-KUGIRI-FLG                PIC 9(01).
               05  WRK-NISSU-RENZOKU-CNT               PIC 9(03).
               05  WRK-NISSU-ST-G.
                   07  WRK-NISSU-ST                    PIC 9(02).
                   07  WRK-NISSU-ST-X                  PIC X(02).
                   07  WRK-NISSU-ST-R1 REDEFINES   WRK-NISSU-ST-X.
                       09  WRK-NISSU-ST-Z9             PIC Z9.
                   07  WRK-NISSU-ST-R2 REDEFINES   WRK-NISSU-ST-X.
                       09  WRK-NISSU-ST-9              PIC 9(01).
                       09  WRK-NISSU-ST-FL             PIC X(01).
               05  WRK-NISSU-ED-G.
                   07  WRK-NISSU-ED                    PIC 9(02).
                   07  WRK-NISSU-ED-X                  PIC X(02).
                   07  WRK-NISSU-ED-R1 REDEFINES   WRK-NISSU-ED-X.
                       09  WRK-NISSU-ED-Z9             PIC Z9.
                   07  WRK-NISSU-ED-R2 REDEFINES   WRK-NISSU-ED-X.
                       09  WRK-NISSU-ED-9              PIC 9(01).
                       09  WRK-NISSU-ED-FL             PIC X(01).
               05  WRK-NISSU-MES                       PIC X(200).
               05  WRK-NISSU-PTR                       PIC 9(03).
               05  WRK-NISSU-LASTYMD                   PIC X(08).
      *
       01  KEY-AREA.
           03  KEY-PTID                PIC 9(10).
           03  KEY-SRYCD               PIC X(09).
      *
       01  CSV-EDIT-AREA.
           03  CSV-NO                  PIC Z(9)9.
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-PTNUM               PIC X(20).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-NAME                PIC X(100).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-KANANAME            PIC X(100).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SEX                 PIC X(2).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-BIRTHDAY            PIC X(10).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-AGENUM              PIC X(04).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-AGETANI             PIC X(04).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYCD               PIC X(09).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYNAME             PIC X(80).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYKANANAME         PIC X(80).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYSURYO            PIC X(11).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-ZAIKAISU            PIC X(05).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SURYOTOTAL          PIC X(13).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYYM               PIC X(07).
           03  CSV-SRYYMD-OCC          OCCURS  31.
               05  FILLER              PIC X(01)   VALUE ",".
               05  CSV-SRYYMD          PIC X(10).
      *
       01  CONST-AREA.
           03  CONST-FORMID            PIC X(20)   VALUE  "HCM28B.red".
           03  CONST-LINE              PIC 9(05)   VALUE   30.
           03  CONST-JOBID             PIC 9(07)   VALUE   1.
           03  CONST-JOB-SHELLID                   PIC X(08)
               VALUE   "ORCBQ01".
      *
       01  CONST-CSV-HEADER.
           03  CONST-CSV-NO            PIC X(20)   VALUE "番号".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-PTNUM         PIC X(20)   VALUE "患者番号".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-NAME          PIC X(20)   VALUE "氏名".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-KANANAME      PIC X(20)   VALUE "カナ氏名".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SEX           PIC X(20)   VALUE "性別".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-BIRTHDAY      PIC X(20)   VALUE "生年月日".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-AGENUM        PIC X(20)   VALUE "年齢".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-AGETANI       PIC X(20)   VALUE "年齢ラベル".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYCD         PIC X(20)   VALUE "診療コード".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYNAME       PIC X(20)   VALUE "診療名称".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYKANANAME   PIC X(20)   VALUE "診療カナ名称".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYSURYO      PIC X(20)   VALUE "数量".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-ZAIKAISU      PIC X(20)   VALUE "回数".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SURYOTOTAL    PIC X(20)   VALUE "合計".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYM         PIC X(20)   VALUE "診療年月".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD01      PIC X(20)   VALUE "算定日１".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD02      PIC X(20)   VALUE "算定日２".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD03      PIC X(20)   VALUE "算定日３".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD04      PIC X(20)   VALUE "算定日４".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD05      PIC X(20)   VALUE "算定日５".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD06      PIC X(20)   VALUE "算定日６".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD07      PIC X(20)   VALUE "算定日７".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD08      PIC X(20)   VALUE "算定日８".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD09      PIC X(20)   VALUE "算定日９".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD10      PIC X(20)   VALUE "算定日１０".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD11      PIC X(20)   VALUE "算定日１１".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD12      PIC X(20)   VALUE "算定日１２".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD13      PIC X(20)   VALUE "算定日１３".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD14      PIC X(20)   VALUE "算定日１４".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD15      PIC X(20)   VALUE "算定日１５".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD16      PIC X(20)   VALUE "算定日１６".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD17      PIC X(20)   VALUE "算定日１７".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD18      PIC X(20)   VALUE "算定日１８".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD19      PIC X(20)   VALUE "算定日１９".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD20      PIC X(20)   VALUE "算定日２０".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD21      PIC X(20)   VALUE "算定日２１".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD22      PIC X(20)   VALUE "算定日２２".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD23      PIC X(20)   VALUE "算定日２３".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD24      PIC X(20)   VALUE "算定日２４".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD25      PIC X(20)   VALUE "算定日２５".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD26      PIC X(20)   VALUE "算定日２６".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD27      PIC X(20)   VALUE "算定日２７".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD28      PIC X(20)   VALUE "算定日２８".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD29      PIC X(20)   VALUE "算定日２９".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD30      PIC X(20)   VALUE "算定日３０".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD31      PIC X(20)   VALUE "算定日３１".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    monpe印刷データ泣き別れ修正サブ
           COPY  "CPORCSFIXREDDATA.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
           COPY  "CPSK1044.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者照会
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者一覧
           COPY "HCM28B.INC"
                                   REPLACING  //HCM28B//
                                   BY         //HC28//.
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCXKANACONV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      *
      ******************************************************************
       PROCEDURE                       DIVISION
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
           INITIALIZE                  STS-AREA
                                       FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       CNT-AREA
                                       KEY-AREA
      *
           MOVE    SPACE               TO  SPA-AREA
           INITIALIZE                      SPA-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA-PAFNAME
                                               WRK-PARA-TERMID
                                               WRK-PARA-SYSYMD
                                               WRK-PARA-SYSTIME
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-STAFFCD
                                               WRK-PARA-FILE
                                               WRK-PARA-FILENM-FOLDER
                                               WRK-PARA-FILENM
                                               WRK-PARA-CSVFILENM
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    WRK-PARA-TERMID     TO  SPA-TERMID
           MOVE    WRK-PARA-STAFFCD    TO  SPA-STAFFCD
      *
      *    ＤＢオープン
           PERFORM 120-DBOPEN-SEC
      *
           CALL    "ORCSENCODING"          USING
                                           MCPAREA
                                           SPA-AREA
           MOVE    SPA-ENCODING        TO  WRK-ENCODING9
      *
           PERFORM 900-JOB-STEP-START-SEC
      *
           MOVE    WRK-PARA-PAFNAME    TO  PRFNAME
      *
      *    パラメータファイルオープン
           OPEN    INPUT               BQ01-FILE
      *
      *    パラメータファイル読み込み
           READ    BQ01-FILE
      *
           MOVE    BQ01                TO  WKBQ01
      *
           CLOSE   BQ01-FILE
      *
           IF  ( WKBQ01-PROC-FLG       =   ZERO )
               MOVE    WRK-PARA-CSVFILENM      TO  CSVNAME
      *
               OPEN    OUTPUT              CSV-FILE
      *
           ELSE
      *        印刷用ファイルオープン
               MOVE    WRK-PARA-HOSPNUM TO HC01PARA-HOSPNUM
               MOVE    "HC99"          TO  HC01PARA-FORM-ID
               MOVE    WRK-PARA-TERMID
                                       TO  HC01PARA-TERMID
               MOVE    WRK-PARA-SYSTIME
                                       TO  HC01PARA-SYOHMS
               MOVE    1               TO  HC01PARA-CNT
      *
               OPEN    OUTPUT              PRT-FILE
      *
      *        プリンタ名取得処理
               PERFORM 110-PRTNUM-GET-SEC
               MOVE    SYS-1031-PRTNM(6)
                                       TO  WRK-PRTNM
      *
               PERFORM 800-OFFSET-GET-SEC
      *
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ名取得処理
      *****************************************************************
       110-PRTNUM-GET-SEC                  SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    WRK-PARA-SYSYMD     TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           .
      *
       110-PRTNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
                                       CNT-KNS
                                       CNT-RENNUM
      *
           MOVE    ZERO            TO  KEY-PTID
           MOVE    LOW-VALUE       TO  KEY-SRYCD
      *
      *    患者対象データ検索処理
           PERFORM 910-PTSRH-ODR-SEL-SEC
      *
      *    患者データ読み飛ばし処理
           PERFORM 910-PTSRH-SKIP-SEC
      *
      *    処理件数算出
           COMPUTE WRK-PROC-KNS    =   WKBQ01-EDKNS
                                   -   WKBQ01-STKNS
                                   +   1
      *
           COMPUTE CNT-PAGE        =   WKBQ01-STPAGE
                                   -   1
      *
           INITIALIZE                  HC28
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1        >   WRK-PROC-KNS )
                OR       ( FLG-PTSRH   =   1 )
      *
               PERFORM 900-PTINF-KEY-SEL-SEC
      *
               IF  ( WKBQ01-PROC-FLG   =  ZERO )
      *
      *            ＣＳＶ編集処理
                   PERFORM 800-CSV-EDIT-SEC
               ELSE
      *
      *            帳票編集処理
                   PERFORM 800-PRT-EDIT-SEC
               END-IF
      *
      *        患者マスタ読込処理
               PERFORM 910-PTSRH-ODR-FET-SEC
      *
           END-PERFORM
      *
           IF  ( WKBQ01-PROC-FLG   =   1 )
               IF  ( CNT-LINE      >   ZERO )
      *            帳票出力処理
                   PERFORM 800-PRT-WRITE-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF  ( WKBQ01-PROC-FLG    =  ZERO )
               CLOSE   CSV-FILE
      *
           ELSE
      *
               CLOSE   PRT-FILE
      *
               MOVE    SPACE           TO  SHELLTBL
      *        ファイルデイレクトリ位置指定
               INITIALIZE              ORCSMKPASSAREA
               MOVE    "scripts/daily/HCALL2.sh"
                                       TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *
               MOVE    HC01PARA        TO  SHELLTBL-ARG1
               MOVE    WRK-ENCODING    TO  SHELLTBL-ARG2
               MOVE    SHELLTBL        TO  MCPDATA-REC
               MOVE    "SHELL"         TO  MCP-FUNC
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "shell"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
      *
           IF  ( WKBQ01-PROC-FLG    =  ZERO )
      *        ファイル保存情報更新
               PERFORM 3001-FILE-INFO-INSERT-SEC
           END-IF
      *
           PERFORM 900-PTSRH-DEL1-SEC
      *
           PERFORM 900-JOB-STEP-END-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-RENNUM
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    CONST-JOB-SHELLID
                                   TO  SFILESV-TBL-KEY
      *
           IF    ( WRK-PARA-FILE       =   "1"             )
               ADD     1               TO  WRK-RENNUM
      *
               MOVE    "ORCBQ01.sh"    TO  SFILESV-SHELLID  (WRK-RENNUM)
               MOVE    1               TO  SFILESV-SHORI-RENNUM
                                                            (WRK-RENNUM)
               MOVE    WRK-RENNUM      TO  SFILESV-RENNUM   (WRK-RENNUM)
               MOVE    "/tmp/"         TO  SFILESV-FOR-FOLDER
                                                            (WRK-RENNUM)
               STRING   WRK-PARA-FILENM    DELIMITED  BY  SPACE
                        ".wk"              DELIMITED  BY  SIZE
                                           INTO
                                           SFILESV-FOR-DATA (WRK-RENNUM)
               END-STRING
               MOVE    WRK-PARA-FILENM-FOLDER
                                       TO  SFILESV-TO-FOLDER(WRK-RENNUM)
               MOVE    WRK-PARA-FILENM TO  SFILESV-TO-DATA  (WRK-RENNUM)
               MOVE    WRK-PROC-KNS    TO  SFILESV-CNT-ALL  (WRK-RENNUM)
               MOVE    "患者一覧（診療行為）"
                                       TO  SFILESV-TITLE    (WRK-RENNUM)
      *********MOVE    "1"             TO  SFILESV-DATA-TYPE(WRK-RENNUM) 
           END-IF
      *
           IF      WRK-RENNUM          >   ZERO
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オフセット取得処理
      *****************************************************************
       800-OFFSET-GET-SEC              SECTION.
      *
           INITIALIZE                  SYS-1044-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1044-HOSPNUM
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    CONST-FORMID       TO  SYS-1044-FORMID
           STRING  WRK-PRTNM           DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
           INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE  MCPDATA-REC       TO  SYS-1044-REC
           ELSE
               INITIALIZE                  SYS-1044-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF   (  SYS-1044-FORMID  =   CONST-FORMID  )
            AND (  SYS-1044-PRTNM   =   WRK-PRTNM    )
               CONTINUE
           ELSE
               INITIALIZE  SYS-1044-REC
           END-IF
      *
           .
       800-OFFSET-GET-EXT.
            EXIT.
      *****************************************************************
      *    ＣＳＶ編集処理
      *****************************************************************
       800-CSV-EDIT-SEC                SECTION.
      *
           INITIALIZE                  CSV-EDIT-AREA
                                       CSV-NO
                                       CSV-PTNUM
                                       CSV-NAME
                                       CSV-KANANAME
                                       CSV-SEX
                                       CSV-BIRTHDAY
                                       CSV-AGENUM
                                       CSV-AGETANI
                                       CSV-SRYCD
                                       CSV-SRYNAME
                                       CSV-SRYKANANAME
                                       CSV-SRYSURYO
                                       CSV-ZAIKAISU
                                       CSV-SURYOTOTAL
                                       CSV-SRYYM
      *
           COMPUTE CNT-CSV =   CNT-CSV +   1
           IF    ( CNT-CSV =   1 )
               MOVE    CONST-CSV-HEADER
                                   TO  CSV-REC
               WRITE   CSV-REC
           END-IF
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               MOVE    SPACE       TO  CSV-SRYYMD (IDXD)
           END-PERFORM
      *
           IF    ( PTSRH-PTID  NOT =   KEY-PTID  )
            OR   ( PTSRH-SRYCD NOT =   KEY-SRYCD )
               COMPUTE CNT-RENNUM  =   CNT-RENNUM  +   1
               MOVE    PTSRH-PTID  TO  KEY-PTID
               MOVE    PTSRH-SRYCD TO  KEY-SRYCD
           END-IF
      *
           MOVE    CNT-RENNUM      TO  CSV-NO
           MOVE    PTSRH-PTNUM
                                   TO  CSV-PTNUM
           MOVE    PTINF-NAME
                                   TO  CSV-NAME
           MOVE    PTINF-KANANAME
                                   TO  CSV-KANANAME
           EVALUATE    PTINF-SEX
           WHEN    1
               MOVE    "男"        TO  CSV-SEX
           WHEN    2
               MOVE    "女"        TO  CSV-SEX
           WHEN    OTHER
               MOVE    SPACE       TO  CSV-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY (1:4)
                                   TO  CSV-BIRTHDAY (1:4)
           MOVE    "/"             TO  CSV-BIRTHDAY (5:1)
           MOVE    PTINF-BIRTHDAY (5:2)
                                   TO  CSV-BIRTHDAY (6:2)
           MOVE    "/"             TO  CSV-BIRTHDAY (8:1)
           MOVE    PTINF-BIRTHDAY (7:2)
                                   TO  CSV-BIRTHDAY (9:2)
      *
           PERFORM 900-AGE-EDIT-SEC
           MOVE    WRK-AGE-Z       TO  CSV-AGENUM
           MOVE    WRK-AGE-MEI     TO  CSV-AGETANI
      *
           MOVE    PTSRH-SRYCD     TO  CSV-SRYCD
           MOVE    PTSRH-SRYNAME   TO  CSV-SRYNAME
           MOVE    PTSRH-SRYKANANAME   TO  CSV-SRYKANANAME
           IF    ( PTSRH-SURYO-EDIT    =   1 )
               PERFORM 8001-SRYSURYO-EDIT-SEC
               MOVE    WRK-SRYSURYOX
                                   TO  CSV-SRYSURYO
           END-IF
      *
           MOVE    PTSRH-ZAIKAISU  TO  WRK-ZAIKAISUZ
           MOVE    WRK-ZAIKAISUZ   TO  CSV-ZAIKAISU
      *
           PERFORM 8001-SURYOTOTAL-EDIT-SEC
           MOVE    WRK-SURYOTOTALX TO  CSV-SURYOTOTAL
      *
           MOVE    PTSRH-SRYYM(1:4)
                                   TO  CSV-SRYYM (1:4)
           MOVE    "/"             TO  CSV-SRYYM (5:1)
           MOVE    PTSRH-SRYYM(5:2) TO  CSV-SRYYM (6:2)
      *
           MOVE    ZERO            TO  IDXA
           MOVE    PTSRH-SRYYM     TO  WRK-SRYYM
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    SPACE       TO  CSV-SRYYMD (IDXD)
               IF    ( PTSRH-DAY (IDXD)    >   ZERO )
                   COMPUTE IDXA    =   IDXA    +   1
                   MOVE    IDXD        TO  WRK-SRYDD
                   MOVE    WRK-SRYYMD(1:4)
                                       TO  CSV-SRYYMD (IDXA)(1:4)
                   MOVE    "/"         TO  CSV-SRYYMD (IDXA)(5:1)
                   MOVE    WRK-SRYYMD(5:2)
                                       TO  CSV-SRYYMD (IDXA)(6:2)
                   MOVE    "/"         TO  CSV-SRYYMD (IDXA)(8:1)
                   MOVE    WRK-SRYYMD(7:2)
                                       TO  CSV-SRYYMD (IDXA)(9:2)
               END-IF
      *
           END-PERFORM
      *
           MOVE    CSV-EDIT-AREA   TO  CSV-REC
      *
           WRITE   CSV-REC
      *
           .
       800-CSV-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       8001-SRYSURYO-EDIT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-SRYSURYOX
      *
           IF    ( PTSRH-SRYSURYO  =   ZERO )
               MOVE    "***"
                                   TO  WRK-SRYSURYOX
           ELSE
               MOVE    PTSRH-SRYSURYO
                                   TO  WRK-SRYSURYOZ
               PERFORM VARYING IDXA    FROM    11  BY  -1
                       UNTIL ( IDXA    <   1 )
                        OR   ( WRK-SRYSURYOX(IDXA:1)
                                   NOT =   "0" )
                       MOVE    SPACE   TO WRK-SRYSURYOX(IDXA:1)
               END-PERFORM
               IF    ( IDXA    >   0 )
                   IF    ( WRK-SRYSURYOX (IDXA:1)
                                    =   "." )
                       MOVE    SPACE   TO WRK-SRYSURYOX(IDXA:1)
                   END-IF
               END-IF
           END-IF
      *
           .
       8001-SRYSURYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    合計編集処理
      *****************************************************************
       8001-SURYOTOTAL-EDIT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-SURYOTOTALX
      *
           MOVE    PTSRH-SURYOTOTAL    TO  WRK-SURYOTOTALZ
           PERFORM VARYING IDXA    FROM    13  BY  -1
                   UNTIL ( IDXA    <   1 )
                    OR   ( WRK-SURYOTOTALX(IDXA:1)
                               NOT =   "0" )
                   MOVE    SPACE   TO WRK-SURYOTOTALX(IDXA:1)
           END-PERFORM
           IF    ( IDXA    >   0 )
               IF    ( WRK-SURYOTOTALX (IDXA:1)
                                    =   "." )
                   MOVE    SPACE   TO WRK-SURYOTOTALX(IDXA:1)
               END-IF
           END-IF
      *
           .
       8001-SURYOTOTAL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       800-PRT-EDIT-SEC                SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           MOVE    ZERO        TO  FLG-EDIT
           IF    ( CNT-LINE    =   1 )
               MOVE    1       TO  FLG-EDIT
           END-IF
      *
           IF    ( PTSRH-PTID  NOT =   KEY-PTID  )
            OR   ( PTSRH-SRYCD NOT =   KEY-SRYCD )
               MOVE    1       TO  FLG-EDIT
               COMPUTE CNT-RENNUM  =   CNT-RENNUM  +   1
               MOVE    PTSRH-PTID  TO  KEY-PTID
               MOVE    PTSRH-SRYCD TO  KEY-SRYCD
           END-IF
      *
           IF    ( FLG-EDIT        =   1 )
               PERFORM 8001-PRT-EDIT-SEC
           END-IF
      *
           MOVE    PTSRH-SRYYM     TO  WRK-SYMD
           MOVE    "01"            TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG1    TO  HC28-SRYYM (CNT-LINE)
      *
      *    備考
           INITIALIZE                  WRK-NISSU-DAY-G
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               IF    ( PTSRH-DAY(IDXD)     >   ZERO )
                   MOVE    1       TO  WRK-NISSU-DAY (IDXD)
               END-IF
           END-PERFORM
           PERFORM 8001-NISSU-HEN-SEC
           IF    ( WRK-NISSU-MES (47:) NOT =   SPACE )
               MOVE    WRK-NISSU-MES
                                   TO  HC28-SRYYMD2(CNT-LINE)
           ELSE
               MOVE    WRK-NISSU-MES
                                   TO  HC28-SRYYMD (CNT-LINE)
           END-IF
      *
           IF  ( CNT-LINE         >=   CONST-LINE )
      *
      *        帳票出力処理
               PERFORM 800-PRT-WRITE-SEC
      *
               INITIALIZE              HC28
               MOVE    ZERO        TO  CNT-LINE
      *
      *
           END-IF
      *
           .
       800-PRT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       8001-PRT-EDIT-SEC               SECTION.
      *
           MOVE    CNT-RENNUM      TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9       TO  HC28-CD     (CNT-LINE)
           IF    ( PTSRH-PTNUM (11:)   NOT =   SPACE )
              MOVE    PTSRH-PTNUM(1:10)
                                   TO  HC28-PTNUMUP(CNT-LINE)
              MOVE    PTSRH-PTNUM(11:)
                                   TO  HC28-PTNUMDN(CNT-LINE)
           ELSE
              MOVE    PTSRH-PTNUM
                                   TO  HC28-PTNUM  (CNT-LINE)
           END-IF
      *
           IF    ( PTINF-NAME (21: ) NOT =   SPACE )
               MOVE    PTINF-NAME
                                   TO  HC28-NAME2  (CNT-LINE)
           ELSE
               MOVE    PTINF-NAME
                                   TO  HC28-NAME   (CNT-LINE)
           END-IF
      *
           EVALUATE    PTINF-SEX
           WHEN    1
               MOVE    "男"        TO  HC28-SEIBETU(CNT-LINE)
           WHEN    2
               MOVE    "女"        TO  HC28-SEIBETU(CNT-LINE)
           END-EVALUATE
      *
           PERFORM 900-AGE-EDIT-SEC
           MOVE    WRK-AGE-Z       TO  HC28-AGENUM (CNT-LINE)
           MOVE    WRK-AGE-MEI     TO  HC28-AGETANI(CNT-LINE)
      *
           MOVE    PTSRH-SRYCD     TO  HC28-SRYCD (CNT-LINE)
      *
           IF    ( PTSRH-SRYNAME (33:) NOT =   SPACE )
              MOVE    PTSRH-SRYNAME
                                   TO  HC28-SRYNAME2(CNT-LINE)
           ELSE
              MOVE    PTSRH-SRYNAME
                                   TO  HC28-SRYNAME (CNT-LINE)
           END-IF
      *
           IF    ( PTSRH-SURYO-EDIT    =   1 )
               PERFORM 8001-SRYSURYO-EDIT-SEC
               MOVE    WRK-SRYSURYOX
                                   TO  HC28-SRYSURYO (CNT-LINE)
           END-IF
      *
           MOVE    PTSRH-ZAIKAISU  TO  WRK-ZAIKAISUZ
           MOVE    WRK-ZAIKAISUZ   TO  HC28-ZAIKAISU (CNT-LINE)
      *
           PERFORM 8001-SURYOTOTAL-EDIT-SEC
           MOVE    WRK-SURYOTOTALX TO  HC28-SURYOTOTAL (CNT-LINE)
      *
           .
       8001-PRT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理
      *****************************************************************
       8001-NISSU-HEN-SEC            SECTION.
      *
           INITIALIZE  WRK-NISSU-AREA
      *
           MOVE    1                   TO  WRK-NISSU-PTR
           MOVE    1                   TO  WRK-NISSU-ST
           MOVE    SPACE               TO  WRK-NISSU-MES
           MOVE    ZERO                TO  WRK-NISSU-KUGIRI-FLG
           MOVE    SPACE               TO  WRK-NISSU-LASTYMD
      *
           PERFORM UNTIL ( WRK-NISSU-ST    >   31 )
      *
               IF    ( WRK-NISSU-DAY (WRK-NISSU-ST )
                                       NOT =   ZERO )
      *
                   MOVE    WRK-NISSU-ST
                                       TO  WRK-DAY
                   MOVE    BQ01-SRYYM  TO  WRK-NISSU-LASTYMD(1:6)
                   MOVE    WRK-DAY-G   TO  WRK-NISSU-LASTYMD(7:2)
      *
                   MOVE    ZERO        TO  WRK-NISSU-RENZOKU-CNT
                   COMPUTE WRK-NISSU-ED    =   WRK-NISSU-ST    +   1
                   PERFORM UNTIL ( WRK-NISSU-ED    >   31 )
                            OR   ( WRK-NISSU-DAY (WRK-NISSU-ED)
                                           =   ZERO )
                       COMPUTE WRK-NISSU-ED
                            =  WRK-NISSU-ED  +   1
      *
                       COMPUTE WRK-NISSU-RENZOKU-CNT
                            =  WRK-NISSU-RENZOKU-CNT   +   1
      *
                   END-PERFORM
      *
                   IF    ( WRK-NISSU-KUGIRI-FLG   =   ZERO )
                       STRING  "　"            DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
      *
                       MOVE    1       TO  WRK-NISSU-KUGIRI-FLG
      *
                   ELSE
                       STRING  "，"            DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                   END-IF
      *
                   IF    ( WRK-NISSU-RENZOKU-CNT  >   ZERO )
                       COMPUTE WRK-NISSU-ED    =   WRK-NISSU-ED    -   1
      *
                       IF    ( WRK-NISSU-ST    <   10 )
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-9
                       ELSE
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-Z9
                       END-IF
      *
                       IF    ( WRK-NISSU-ED    <   10 )
                           MOVE    WRK-NISSU-ED    TO  WRK-NISSU-ED-9
                       ELSE
                           MOVE    WRK-NISSU-ED    TO  WRK-NISSU-ED-Z9
                       END-IF
      *
                       STRING  WRK-NISSU-ST-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                               "〜"                DELIMITED   BY  SIZE
                               WRK-NISSU-ED-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                       COMPUTE WRK-NISSU-ST
                           =   WRK-NISSU-ED      +   1
                   ELSE
      *
                       IF    ( WRK-NISSU-ST    <   10 )
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-9
                       ELSE
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-Z9
                       END-IF
      *
                       STRING  WRK-NISSU-ST-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                       COMPUTE WRK-NISSU-ST
                           =   WRK-NISSU-ST      +   1
                   END-IF
      *
               ELSE
                   COMPUTE WRK-NISSU-ST
                       =   WRK-NISSU-ST      +   1
               END-IF
           END-PERFORM
      *
           MOVE    WRK-NISSU-MES       TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-NISSU-MES
      *
           .
       8001-NISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       800-PRT-WRITE-SEC           SECTION.
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE    +   1
      *
      *    ヘッダ部編集
           MOVE    WKBQ01-HYODAI   TO  HC28-HYODAI
      *
           MOVE    WKBQ01-JYOKEN1  TO  HC28-JYOKEN (1)
      *
           MOVE    WKBQ01-JYOKEN2  TO  HC28-JYOKEN (2)
      *
           MOVE    WRK-PARA-SYSYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG3    TO  HC28-SAKUSEIBI
      *
           MOVE    CNT-PAGE        TO  WRK-ZZZZ9C
           MOVE    WRK-ZZZZ9C      TO  HC28-PAGE
      *
      *
           MOVE    SPACE           TO  PRT-REC
           MOVE    "HCM28B.red"    TO  PRT-PRTID
           MOVE    WRK-PRTNM       TO  PRT-PRTNM
           MOVE    SYS-1044-OFFSET-X   TO  PRT-OFFSET-X
           MOVE    SYS-1044-OFFSET-Y   TO  PRT-OFFSET-Y
           MOVE    "1"             TO  PRT-SITEKBN
           INITIALIZE                  SFIXREDDATA-AREA
           MOVE    "1"             TO  SFIXREDDATA-SITEKBN
           MOVE    PRT-PRTID       TO  SFIXREDDATA-FORMID
           MOVE    HC28            TO  SFIXREDDATA-DATA
           CALL    "ORCSFIXREDDATA"    USING
                                       SFIXREDDATA-AREA
                                       SPA-AREA
           MOVE    SFIXREDDATA-DATA
                                   TO  PRT-PRTDATA
      *
           WRITE   PRT-REC
      *
           .
       800-PRT-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
           INSPECT WRK-HENYMDG3   REPLACING  ALL "  "  BY  "　"
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-OUT-INPUT
           MOVE    ZERO            TO  WRK-OUT-MAX
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE 300                TO  KANACONV-LEN
           MOVE 1000               TO  KANACONV-STLEN
           MOVE 1                  TO  KANACONV-CONV-FLAG
           MOVE 15                 TO  KANACONV-CHAR-TYPE
           MOVE WRK-MAE-INPUT      TO  KANACONV-INDATA
           CALL "kanaconv"             USING ORCXKANACONVAREA
           IF    ( KANACONV-RETURN =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  WRK-OUT-INPUT
               MOVE    KANACONV-MAXLEN TO  WRK-OUT-MAX
           END-IF
      *
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    年齢編集処理
      *****************************************************************
       900-AGE-EDIT-SEC                SECTION.
      *
           IF  (   PTINF-BIRTHDAY
                                       <   BQ01-KJYNYMD )
               MOVE    PTINF-BIRTHDAY
                                       TO  WRK-CALC-YMD1
               MOVE    BQ01-KJYNYMD    TO  WRK-CALC-YMD2
               PERFORM 900-AGE-CALC-SEC
           ELSE
               MOVE    BQ01-KJYNYMD    TO  WRK-CALC-YMD1
               MOVE    PTINF-BIRTHDAY
                                       TO  WRK-CALC-YMD2
               PERFORM 900-AGE-CALC-SEC
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       *   -1
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
                                       *   -1
           END-IF
      *
           IF    ( WRK-AGE-YY          =   ZERO )
               MOVE    WRK-AGE-MM      TO  WRK-AGE-Z
               MOVE    "ヶ月"          TO  WRK-AGE-MEI
           ELSE
               MOVE    WRK-AGE-YY      TO  WRK-AGE-Z
               MOVE    "歳"            TO  WRK-AGE-MEI
           END-IF
      *
           .
       900-AGE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       900-AGE-CALC-SEC                SECTION.
      *
           COMPUTE WRK-AGE-YY          =   WRK-CALC-YY2
                                       -   WRK-CALC-YY1
           IF    ( WRK-CALC-YMD1(5:4)  >   WRK-CALC-YMD2(5:4) )
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       -   1
               COMPUTE WRK-AGE-MM      =  (WRK-CALC-MM2    +   12 )
                                       -   WRK-CALC-MM1
           ELSE
               COMPUTE WRK-AGE-MM      =   WRK-CALC-MM2
                                       -   WRK-CALC-MM1
           END-IF
           IF    ( WRK-CALC-DD1        >   WRK-CALC-DD2 )
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
                                       -   1
           END-IF
      *
           .
       900-AGE-CALC-EXT.
           EXIT.
      *****************************************************************
      *    文字列長取得処理
      *****************************************************************
       900-STR-LEN-GET-SEC                 SECTION.
      *
           PERFORM VARYING IDX-LEN FROM    WRK-STR-LEN BY  -1
               UNTIL   ( IDX-LEN   <   1)
                OR     ( WRK-STR (IDX-LEN : 1) NOT =   SPACE)
      *
               CONTINUE
           END-PERFORM
      *
           MOVE    IDX-LEN     TO  WRK-STR-LEN
      *
           .
       900-STR-LEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       910-PTSRH-ODR-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID          TO  PTSRH-TERMID
           MOVE    "2"                 TO  PTSRH-DATAKBN
           MOVE    "%"                 TO  PTSRH-UPYMD
                                           PTSRH-UPHMS
           MOVE    WKBQ01-SQL          TO  PTSRH-SQL
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       910-PTSRH-ODR-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会FETCH処理
      *****************************************************************
       910-PTSRH-ODR-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
               COMPUTE CNT-KNS =   CNT-KNS +   1
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       910-PTSRH-ODR-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL1-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "2"             TO  PTSRH-DATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    患者マスタ読み飛ばし処理
      *****************************************************************
       910-PTSRH-SKIP-SEC         SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1        >       WKBQ01-STKNS )
                 OR      ( FLG-PTSRH   =       1 )
      *
      *        患者マスタ読込処理
               PERFORM 910-PTSRH-ODR-FET-SEC
      *
               IF    ( PTSRH-PTID  NOT =   KEY-PTID  )
                OR   ( PTSRH-SRYCD NOT =   KEY-SRYCD )
      *
                   COMPUTE CNT-RENNUM  =   CNT-RENNUM  +   1
                   MOVE    PTSRH-PTID  TO  KEY-PTID
                   MOVE    PTSRH-SRYCD TO  KEY-SRYCD
      *
               END-IF
      *
           END-PERFORM
      *
           .
       910-PTSRH-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    PTSRH-PTID      TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
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
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-KEY5-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUM
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM      TO  HKN-HKNNUM
           MOVE    BQ01-SRYYM (1:6)
                                   TO  HKN-TEKSTYMD(1:6)
           MOVE    "31"            TO  HKN-TEKSTYMD(7:2)
           MOVE    BQ01-SRYYM (1:6)
                                   TO  HKN-TEKEDYMD(1:6)
           MOVE    "01"            TO  HKN-TEKEDYMD(7:2)
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNNUM-REC
           ELSE
               MOVE    1           TO  FLG-HKNNUM
               INITIALIZE              HKNNUM-REC
           END-IF
      *
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNNUM-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ステップ管理開始処理
      *****************************************************************
       900-JOB-STEP-START-SEC      SECTION.
      *
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "患者一覧出力（診療行為）"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCBQ01"       TO  JOB-PGID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       900-JOB-STEP-START-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       900-JOB-STEP-END-SEC            SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           IF  ( WKBQ01-PROC-FLG   =   ZERO )
               MOVE    WRK-PROC-KNS
                                   TO  JOB-UPDCNT
           ELSE
               COMPUTE JOB-UPDCNT  =   CNT-PAGE
                                   -   WKBQ01-STPAGE
                                   +   1
           END-IF
      *
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       900-JOB-STEP-END-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　接続処理
      *****************************************************************
       120-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       120-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
