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
       PROGRAM-ID.             ORCBSETCHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次処理
      *  コンポーネント名  : セットコード一括置換え
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/03/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
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
      *
      *    確認一覧（縦）
           COPY    "HCMSL80.INC".
      *    エラー一覧（横）
           COPY    "HCMSL55.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
           03  STS-PARA                PIC X(02).
           03  STS-DATA                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-INPUTSET            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-INPUTCD             PIC 9(01).
      *
           03  FLG-SET-ERR             PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-END-SYORI           PIC 9(01).
           03  FLG-END-SYORI2          PIC 9(01).
           03  FLG-SYORI-SET           PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE1               PIC 9(03).
           03  CNT-PAGE1               PIC 9(05).
           03  CNT-LINE2               PIC 9(03).
           03  CNT-PAGE2               PIC 9(05).
      *
           03  CNT-IN                  PIC 9(05).
           03  CNT-OK                  PIC 9(05).
           03  CNT-ERR                 PIC 9(05).
           03  CNT-KEI                 PIC 9(05).
           03  CNT-OUT                 PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDX-ERR                 PIC 9(04).
           03  IDXN                    PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDX4                    PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH             PIC 9(02).
               05  SYS-TMM             PIC 9(02).
               05  SYS-TSS             PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  RECEERR                 PIC X(100).
           03  WRK-PARA-SYORIKBN       PIC X(01).
           03  WRK-PARA-SRYYMD         PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYSYMDG             PIC X(22).
      *
           03  WRK-SKBN                PIC X(02).
           03  WRK-SRYCD-D             PIC X(09).
           03  WRK-SRYCD-A             PIC X(09).
      *
           03  WRK-MAE-SRYYMD          PIC X(08).
           03  WRK-STR-SRYYMD          PIC X(08).
      *
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRMSG              PIC X(80).
           03  WRK-ERRMSG2             PIC X(80).
      *   撮影料
       01  TBL-SATUEI-AREA.
      * 単純撮影
           03  FILLER              PIC X(02)  VALUE   "01".
      *    単純撮影（アナログ撮影）
           03  FILLER              PIC X(09)  VALUE   "170001910".
      *    単純撮影（デジタル撮影）
           03  FILLER              PIC X(09)  VALUE   "170027910".
      *
      * 特殊撮影
           03  FILLER              PIC X(02)  VALUE   "02".
      *    （アナログ撮影）
           03  FILLER              PIC X(09)  VALUE   "170002010".
      *    （デジタル撮影）
           03  FILLER              PIC X(09)  VALUE   "170028010".
      *
      * 造影剤使用撮影
           03  FILLER              PIC X(02)  VALUE   "03".
      *    造影剤使用撮影（アナログ撮影）
           03  FILLER              PIC X(09)  VALUE   "170002110".
      *    造影剤使用撮影（デジタル撮影）
           03  FILLER              PIC X(09)  VALUE   "170028110".
      *
      * 乳房撮影
           03  FILLER              PIC X(02)  VALUE   "04".
      *    乳房撮影（アナログ撮影）
           03  FILLER              PIC X(09)  VALUE   "170027010".
      *    乳房撮影（デジタル撮影）
           03  FILLER              PIC X(09)  VALUE   "170028210".
      *
      *    単純撮影（手前）
           03  FILLER              PIC X(02)  VALUE   "05".
           03  FILLER              PIC X(09)  VALUE   "170021750".
      *    単純撮影（手前）（デジタル撮影）
           03  FILLER              PIC X(09)  VALUE   "170031350".
      *
      *    単純撮影（短期）
           03  FILLER              PIC X(02)  VALUE   "06".
           03  FILLER              PIC X(09)  VALUE   "170023950".
      *    単純撮影（短期）（デジタル撮影）
           03  FILLER              PIC X(09)  VALUE   "170031550".
      *
      *    単純間接
           03  FILLER              PIC X(02)  VALUE   "07".
           03  FILLER              PIC X(09)  VALUE   "170002410".
      *    単純間接（デジタル撮影）
           03  FILLER              PIC X(09)  VALUE   "170028310".
      *
      *    単純間接（手前）
           03  FILLER              PIC X(02)  VALUE   "08".
           03  FILLER              PIC X(09)  VALUE   "170021850".
      *    単純間接（手前）（デジタル撮影）
           03  FILLER              PIC X(09)  VALUE   "170031450".
      *
      *    単純間接（短期）
           03  FILLER              PIC X(02)  VALUE   "09".
           03  FILLER              PIC X(09)  VALUE   "170024050".
      *    単純間接（短期）（デジタル撮影）
           03  FILLER              PIC X(09)  VALUE   "170031650".
      *
      *    特殊撮影（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "10".
           03  FILLER              PIC X(09)  VALUE   "170005530".
           03  FILLER              PIC X(09)  VALUE   "170028930".
      *    特殊撮影（他方と同時併施）（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "11".
           03  FILLER              PIC X(09)  VALUE   "170005630".
           03  FILLER              PIC X(09)  VALUE   "170029030".
      *    パントモグラフィー（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "12".
           03  FILLER              PIC X(09)  VALUE   "170006530".
           03  FILLER              PIC X(09)  VALUE   "170029130".
      *    パントモグラフィー（他方と同時併施）（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "13".
           03  FILLER              PIC X(09)  VALUE   "170006630".
           03  FILLER              PIC X(09)  VALUE   "170029230".
      *    断層撮影（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "14".
           03  FILLER              PIC X(09)  VALUE   "170006930".
           03  FILLER              PIC X(09)  VALUE   "170029330".
      *    断層撮影（他方と同時併施）（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "15".
           03  FILLER              PIC X(09)  VALUE   "170007030".
           03  FILLER              PIC X(09)  VALUE   "170029430".
      *    同時多層撮影（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "16".
           03  FILLER              PIC X(09)  VALUE   "170007130".
           03  FILLER              PIC X(09)  VALUE   "170029530".
      *    同時多層撮影（他方と同時併施）（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "17".
           03  FILLER              PIC X(09)  VALUE   "170007200".
           03  FILLER              PIC X(09)  VALUE   "170029630".
      *    回転横断撮影（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "18".
           03  FILLER              PIC X(09)  VALUE   "170007330".
           03  FILLER              PIC X(09)  VALUE   "170029730".
      *    回転横断撮影（他方と同時併施）（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "19".
           03  FILLER              PIC X(09)  VALUE   "170007430".
           03  FILLER              PIC X(09)  VALUE   "170029830".
      *    スポット撮影（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "20".
           03  FILLER              PIC X(09)  VALUE   "170007530".
           03  FILLER              PIC X(09)  VALUE   "170029930".
      *    スポット撮影（他方と同時併施）（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "21".
           03  FILLER              PIC X(09)  VALUE   "170007630".
           03  FILLER              PIC X(09)  VALUE   "170030030".
      *    児頭骨盤不均衡撮影（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "22".
           03  FILLER              PIC X(09)  VALUE   "170008430".
           03  FILLER              PIC X(09)  VALUE   "170030430".
      *    児頭骨盤不均衡撮影（他方と同時併施）（診断・撮影）
           03  FILLER              PIC X(02)  VALUE   "23".
           03  FILLER              PIC X(09)  VALUE   "170008530".
           03  FILLER              PIC X(09)  VALUE   "170030530".
      *    特殊撮影（撮影・診断）新生児
           03  FILLER              PIC X(02)  VALUE   "24".
           03  FILLER              PIC X(09)  VALUE   "170019130".
           03  FILLER              PIC X(09)  VALUE   "170030730".
      *    特殊撮影（他方と同時併施）（撮影・診断）新生児
           03  FILLER              PIC X(02)  VALUE   "25".
           03  FILLER              PIC X(09)  VALUE   "170019330".
           03  FILLER              PIC X(09)  VALUE   "170030930".
      *    特殊撮影（撮影・診断）乳幼児
           03  FILLER              PIC X(02)  VALUE   "26".
           03  FILLER              PIC X(09)  VALUE   "170019230".
           03  FILLER              PIC X(09)  VALUE   "170030830".
      *    特殊撮影（他方と同時併施）（撮影・診断）乳幼児
           03  FILLER              PIC X(02)  VALUE   "27".
           03  FILLER              PIC X(09)  VALUE   "170019430".
           03  FILLER              PIC X(09)  VALUE   "170031030".
      *    側頭骨曲面断層撮影（撮影・診断）
           03  FILLER              PIC X(02)  VALUE   "28".
           03  FILLER              PIC X(09)  VALUE   "170008130".
           03  FILLER              PIC X(09)  VALUE   "170030130".
      *    上顎骨曲面断層撮影（撮影・診断）
           03  FILLER              PIC X(02)  VALUE   "29".
           03  FILLER              PIC X(09)  VALUE   "170008230".
           03  FILLER              PIC X(09)  VALUE   "170030230".
      *    副鼻腔曲面断層撮影（撮影・診断）
           03  FILLER              PIC X(02)  VALUE   "30".
           03  FILLER              PIC X(09)  VALUE   "170008330".
           03  FILLER              PIC X(09)  VALUE   "170030330".
      *
      *
       01  TBL-SATUEI-AREA-R       REDEFINES  TBL-SATUEI-AREA.
           03  TBL-SATUEI-OCC      OCCURS     30  INDEXED  TBL-SIDX.
               05  TBL-SATUKBN             PIC X(02).
      *        撮影料（アナログ）
               05  TBL-SATU-SRYCD-A        PIC X(09).
      *        撮影料（デジタル）
               05  TBL-SATU-SRYCD-D        PIC X(09).
      *
      *    リスト領域
       01  LST-PRT-AREA.
           03  LST-PRT-HEAD1.
               05  FILLER              PIC X(01)   VALUE   SPACE.
               05  FILLER              PIC X(32)   VALUE   
                   "＃＃アナログ撮影→デジタル撮影　".
               05  FILLER              PIC X(20)   VALUE   
                   "セット変更リスト＃＃".
               05  FILLER              PIC X(04)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE
                   "開始日：".
               05  PRT-SRYYMD          PIC X(22).
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  PRT-SYSYMD          PIC X(10)   VALUE   SPACE.
      *******  05  PRT-TIME            PIC X(07)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "P.".
               05  PRT-PAGE            PIC ZZZ9.
           03  LST-PRT-HEAD2.
               05  FILLER              PIC X(12)   VALUE
                   "セットＣＤ".
               05  FILLER              PIC X(20)   VALUE
                   "有効期間（対象）".
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(46)   VALUE
                   "セット名称".
           03  LST-PRT-BODY-G.
               05  LST-PRT-BODY        OCCURS   50.
                   07  PRT-SETCD           PIC X(10).
                   07  PRT-STYMD           PIC X(09).
                   07  PRT-P1              PIC X(02).
                   07  PRT-EDYMD           PIC X(09).
                   07  FILLER              PIC X(01).
                   07  PRT-NAME            PIC X(50).
           03  LST-PRT-TAIL1.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(12)   VALUE
                   "更新件数：".
               05  PRT-UPDCNT          PIC ZZ,ZZ9.
               05  FILLER              PIC X(04)   VALUE
                   "　件".
      *
           03  LST-PRT-TAIL2.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(16)   VALUE
                   "対象セット件数：".
               05  PRT-CNT             PIC ZZ,ZZ9.
               05  FILLER              PIC X(04)   VALUE
                   "　件".
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
           03  WRK-HENTIME.
               05  FILLER          PIC X(01)   VALUE   "(".
               05  WRK-THH         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-TMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ")".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK1009.INC".
      *    チェック機能制御情報 
           COPY    "CPSK1008.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    セット
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    セット
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
       01  WRK-INPUTSET-REC.
           COPY    "CPINPUTSET.INC"  REPLACING
                                     //ISET-// BY //WRK-ISET-//.
      *
       01  TBL-INPUTSET-AREA.
           02  TBL-INPUTSET-REC          OCCURS   400.
           COPY    "CPINPUTSET.INC"  REPLACING
                                     //ISET-// BY //TBL-ISET-//.
      *
       01  HZN-INPUTSET-AREA.
           02  HZN-INPUTSET-REC          OCCURS   400.
           COPY    "CPINPUTSET.INC"  REPLACING
                                     //ISET-// BY //HZN-ISET-//.
      *
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
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
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
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
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
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
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  MCPAREA
           MOVE    SPACE           TO  LST-PRT-BODY-G
           INITIALIZE                  LST-PRT-BODY-G
           MOVE    ZERO            TO  CNT-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
                                       WRK-PARA-SYORIKBN
      *
      *    ＳＰＡ設定
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    CALL    "ORCSENCODING"      USING
      *                                MCPAREA
      *                                SPA-AREA
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBSETCHG"        TO  JOB-PGID
           MOVE    "撮影料セット一括変更処理"
                                       TO  JOB-SHELLMSG
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    初期値セット
           PERFORM 110-SYOKI-SET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期値セット
      *****************************************************************
       110-SYOKI-SET-SEC           SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYSYMD
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           END-IF
      *
      *    処理日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SYSYMD
      *
      *    処理時間
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    SYS-THH             TO  WRK-THH
           MOVE    SYS-TMM             TO  WRK-TMM
      ***  MOVE    WRK-HENTIME         TO  PRT-TIME
      *
      *ＳＰＡ設定
      *    改正日
           IF      WRK-PARA-SRYYMD     =   SPACE
              MOVE    "20100401"           TO  WRK-PARA-SRYYMD
           END-IF
           IF      WRK-PARA-SRYYMD     <   "20100401"
              MOVE    "20100401"           TO  WRK-PARA-SRYYMD
           END-IF
           MOVE    WRK-PARA-SRYYMD     TO  SPA-SRYYMD
           MOVE    WRK-PARA-SRYYMD     TO  WRK-STR-SRYYMD
      *
      *    診療日
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-STR-SRYYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  PRT-SRYYMD
      *
      *    診療日の前日
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-STR-SRYYMD      TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-MAE-SRYYMD
           ELSE
               MOVE    "開始日の前日がエラーです。処理できません。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO      TO      110-SYOKI-SET-EXT
           END-IF
      *
      *    区分判定
           IF      WRK-PARA-SYORIKBN   =   "1"
                                       OR  "2"
               CONTINUE
           ELSE
               MOVE    "処理区分が設定されていません。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO      TO      110-SYOKI-SET-EXT
           END-IF
      *
           MOVE    ALL "*"             TO  SPA-TERMID
      *    オペレータＩＤ
           MOVE    LNK-PRTKANRI-OPID   TO  SPA-OPID
           INSPECT SPA-OPID            REPLACING  ALL " "  BY  "*"
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    入力セット検索
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
      *    入力セット検索
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
               MOVE    1                   TO  FLG-END
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYORI-SET
           INITIALIZE                      TBL-INPUTSET-AREA
           INITIALIZE                      HZN-INPUTSET-AREA
           MOVE    ZERO                TO  IDX
      *
           PERFORM UNTIL   FLG-END         =   1
      *
               IF      ISET-SETSEQ         =   1
                   MOVE    INPUTSET-REC        TO  WRK-INPUTSET-REC
                   IF      IDX                 >   ZERO
                       PERFORM 2001-SET-SYORI-SEC
                   END-IF
                   INITIALIZE                      TBL-INPUTSET-AREA
                   INITIALIZE                      HZN-INPUTSET-AREA
                   MOVE    WRK-INPUTSET-REC        TO  INPUTSET-REC
                   MOVE    ZERO                TO  IDX
                   MOVE    ZERO                TO  FLG-SYORI-SET
                   IF     (ISET-YUKOSTYMD      <=  WRK-MAE-SRYYMD )
                   AND    (ISET-YUKOEDYMD      =   "99999999"     )
      *                有効期間内
                       MOVE    1               TO  FLG-SYORI-SET
                   END-IF
               END-IF
               IF      FLG-SYORI-SET           =   1
                   ADD     1                   TO  IDX
                   MOVE    INPUTSET-REC        TO  TBL-INPUTSET-REC(IDX)
               END-IF
      *
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
               IF      FLG-INPUTSET        =   1
                   MOVE    1                   TO  FLG-END
               END-IF
           END-PERFORM
      *
           IF      IDX                 >   ZERO
               PERFORM 2001-SET-SYORI-SEC
           END-IF
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM 2903-END-HENSYU-SEC
           IF      CNT-LINE2           >   ZERO
      *    確認リスト印刷
               PERFORM 2902-HCML55-PRT02-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット変更処理
      *****************************************************************
       2001-SET-SYORI-SEC             SECTION.
      *
           MOVE    TBL-INPUTSET-AREA   TO  HZN-INPUTSET-AREA
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   400 )  OR
                              (TBL-ISET-SETCD (IDX)    =   SPACE )
               PERFORM 2000-SRYCD-CHK1-SEC
               IF      WRK-SKBN            NOT =   SPACE
      *            デジタルコードへ変更
                   MOVE    WRK-SRYCD-D     TO  HZN-ISET-INPUTCD (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF     WRK-PARA-SYORIKBN    =   "1"
      *        一覧表示
               IF      FLG-OK              =   1
                   PERFORM 20011-SET-LIST-SEC
               END-IF
           ELSE
      *        セット一括変更
               IF      FLG-OK              =   1
                   PERFORM 20012-SET-CHNG-SEC
               END-IF
           END-IF
      *
           .
       2001-SET-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象セット一覧処理
      *****************************************************************
       20011-SET-LIST-SEC             SECTION.
      *
           ADD     1               TO  CNT-OK
           PERFORM 700-PTNUM-PRINT-SEC
           .
       20011-SET-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット期間更新処理
      *****************************************************************
       20012-SET-CHNG-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SET-ERR
      *    セット検索
           MOVE    TBL-INPUTSET-REC(1) TO  INPUTSET-REC
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           PERFORM UNTIL    FLG-INPUTSET   =   1
      *        追加の開始日以降の開始日があれば、切り替えなし
               IF     (ISET-YUKOSTYMD      >=  WRK-STR-SRYYMD )
                   MOVE    1               TO  FLG-SET-ERR
               END-IF
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-OK              =   1    )  AND
                  (FLG-SET-ERR         =   ZERO )
               PERFORM 200111-INPUTSET-SET-SEC
           END-IF
      *
           .
       20012-SET-CHNG-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間更新処理
      *****************************************************************
       200111-INPUTSET-SET-SEC            SECTION.
      *
           MOVE    TBL-INPUTSET-REC(1) TO  INPUTSET-REC
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "INPUTSET   DEL ERR:"  MCP-RC
           END-IF
      *
           MOVE    ZERO                TO  FLG-END
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   400 )  OR
                              (TBL-ISET-SETCD (IDX)   =   SPACE )
                          OR  (FLG-END     =   1   )
               MOVE    TBL-INPUTSET-REC (IDX)  TO  INPUTSET-REC
               MOVE    WRK-MAE-SRYYMD      TO  ISET-YUKOEDYMD
      *
               MOVE    INPUTSET-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "INPUTSET INS ERR:" MCP-RC
                    ",KEY:" ISET-KEY
                   MOVE    SPACE               TO  WRK-RECEERR
                   STRING  "セット更新でエラーとなりました。"
                           "処理を終了します。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   400 )  OR
                              (HZN-ISET-SETCD (IDX)   =   SPACE )
                          OR  (FLG-END     =   1   )
               MOVE    HZN-INPUTSET-REC (IDX)  TO  INPUTSET-REC
               MOVE    WRK-STR-SRYYMD      TO  ISET-YUKOSTYMD
      *
               MOVE    INPUTSET-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                  MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "INPUTSET INS ERR:" MCP-RC
                    ",KEY:" ISET-KEY
                   MOVE    SPACE               TO  WRK-RECEERR
                   STRING  "セット更新でエラーとなりました。"
                           "処理を終了します。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           IF      WRK-RECEERR         =   SPACE
      *
               ADD     1               TO  CNT-OK
               PERFORM 700-PTNUM-PRINT-SEC
           END-IF
           .
       200111-INPUTSET-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更前アナログコード判定処理
      *****************************************************************
       2000-SRYCD-CHK1-SEC             SECTION.
      *
      *    撮影料の判断
           SET     TBL-SIDX            TO  1
           SEARCH  TBL-SATUEI-OCC      VARYING   TBL-SIDX
               AT   END
                   MOVE    SPACE               TO  WRK-SKBN
                   MOVE    SPACE               TO  WRK-SRYCD-D
               WHEN    TBL-ISET-INPUTCD (IDX)  =   TBL-SATU-SRYCD-A
                                                           (TBL-SIDX)
                   MOVE    TBL-SATUKBN (TBL-SIDX)
                                                TO  WRK-SKBN
                   MOVE    TBL-SATU-SRYCD-D(TBL-SIDX)
                                                TO  WRK-SRYCD-D
           END-SEARCH
           .
       2000-SRYCD-CHK1-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録確認メッセージ処理
      *****************************************************************
       700-PTNUM-PRINT-SEC          SECTION.
      *
           ADD     1                   TO  CNT-LINE2
           IF      CNT-LINE2           >   50
               PERFORM 2902-HCML55-PRT02-SEC
               MOVE    1               TO  CNT-LINE2
           END-IF
           MOVE    TBL-INPUTSET-REC (1)    TO  INPUTSET-REC
           MOVE    ISET-SETCD          TO  PRT-SETCD (CNT-LINE2)
      *    期間
           MOVE    ISET-YUKOSTYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-STYMD (CNT-LINE2)
           MOVE    ISET-YUKOEDYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-EDYMD (CNT-LINE2)
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    ISET-HOSPNUM        TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    ISET-SETCD          TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           MOVE    ICD-DSPNAME         TO  PRT-NAME  (CNT-LINE2)
      *
           .
       700-PTNUM-PRINT-EXIT.
           EXIT.
      *****************************************************************
      *    処理件数編集印刷処理
      *****************************************************************
       2903-END-HENSYU-SEC          SECTION.
      *
      *    確認件数
           ADD     3                   TO  CNT-LINE2
           IF      CNT-LINE2           >   80
               PERFORM 2902-HCML55-PRT02-SEC
               MOVE    1               TO  CNT-LINE2
           END-IF
      *
           IF      WRK-PARA-SYORIKBN   =   "1"
               MOVE    CNT-OK              TO  PRT-CNT
               MOVE    LST-PRT-TAIL2       TO  LST-PRT-BODY (CNT-LINE2)
           ELSE
               MOVE    CNT-OK              TO  PRT-UPDCNT
               MOVE    LST-PRT-TAIL1       TO  LST-PRT-BODY (CNT-LINE2)
           END-IF
      *
           .
       2903-END-HENSYU-EXIT.
           EXIT.
      *****************************************************************
      *    確認リスト印刷処理
      *****************************************************************
       2902-HCML55-PRT02-SEC          SECTION.
      *
           MOVE    SPACE               TO  HCMSL80
           ADD     1                   TO  CNT-PAGE2
           MOVE    CNT-PAGE2           TO  PRT-PAGE
      *
           MOVE    LST-PRT-HEAD1       TO  HCMSL80-MOJI (1)
           MOVE    LST-PRT-HEAD2       TO  HCMSL80-MOJI (3)
      *
           MOVE    4                  TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   50 )    OR
                              (IDX     >=  80 )
               ADD     1                   TO  IDX
               MOVE    LST-PRT-BODY (IDY)
                                       TO  HCMSL80-MOJI (IDX)
           END-PERFORM
           PERFORM 2902-PRINT02-SEC
      *
           MOVE    ZERO                    TO  CNT-LINE2
      *
           INITIALIZE                      LST-PRT-BODY-G
           .
       2902-HCML55-PRT02-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2902-PRINT02-SEC          SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"            TO  SPRT-MODE
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
      *****MOVE    LNK-PRTKANRI-SHORI-RENNUM
           MOVE    2
                                    TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                    TO  SPRT-PRIORITY
           MOVE    "HCMSL80.red"    TO  SPRT-PRTID
           MOVE    "改正セット一括変換確認リスト"
                                    TO  SPRT-TITLE
           MOVE    HCMSL80          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2902-PRINT02-EXIT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      ***  PERFORM 3001-PARA-DEL-SEC
      *
           DISPLAY "*** ORCBSETCHG OK :"  CNT-OK   " ***"
           DISPLAY "*** ORCBSETCHG END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-OK          TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
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
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
                                       OR  ZERO
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       920-INPUTCD-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       900-INPUTSET-READ-SEC         SECTION.
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           .
       900-INPUTSET-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　更新処理
      *****************************************************************
       990-DBCOMMIT-SEC                SECTION.
      *
      *    ＤＢ　クローズ処理
           PERFORM 900-DBDISCONNECT-SEC
      *    ＤＢ　オープン処理
           PERFORM 100-DBOPEN-SEC
           .
       990-DBCOMMIT-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
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
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
