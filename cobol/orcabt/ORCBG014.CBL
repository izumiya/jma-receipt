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
       PROGRAM-ID.             ORCBG014.
      *****************************************************************
      *  システム名        :   ＯＲＣＡ
      *  サブシステム名    :   月次帳票
      *  コンポーネント名  :   保険請求確認リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/30    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  00.00.01    NACL-森脇     05/10/05  出力順パラメタ追加
      *                                      ＣＳＶ出力
      *                                      統計ファイルをひとつにまとめる
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    統計用ファイル
           SELECT  TOKEI-FILE      ASSIGN  TOKEIPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOKEI-KEY
                                   FILE    STATUS  IS  STS-TOKEI.
      *    クリア用ファイル
           SELECT  TOKEIX-FILE     ASSIGN  TOKEIPARA
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-TOKEIX.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *    ＣＳＶファイル
           SELECT  RECECSV-FILE    ASSIGN  "/var/tmp/ORCBG014.csv"
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-RECECSV.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *KEYの説明-------------------------------------------------------------------------
      *        SKYKBN      請求区分    1:請求済、未請求、2:請求しない
      *        TEISYUTU    提出先区分  1:社保、2:国保
      *        HKNJANUM    保険者番号  国保の時だけ使用、社保＆請求しない場合はオールゼロ
      *        SYOKBN      処理区分    1:通常分、2:返戻分、3:月遅れ分
      *        NYUGAIKBN   入外区分    1:外来分、2:入院分
      *        ZAIKBN      在総診区分  1:その他のレセ、2:在総診または在医総を算定したレセ
      *        SRYYMFLG    診療月フラグ
      *        HKNKBN      保険区分    1:医保、2:老人、3:公費
      *        SYUBETUKBN  種別区分
      *                    社保  医保  1:老人９割、2:老人８割、3:本人、4:家族、5:３歳
      *                          老人  1:老人９割、2:老人８割
      *                          公費  1:公費と公費の併用、2:公費単独
      *                    国保  一般  1:老人９割、2:老人８割、3:被保険者、4:３歳、
      *                          退職  5:退職本人、6:老人９割、7:老人８割、8:被扶養者、9:三歳
      *                          老人  1:老人９割、2:老人８割
      *        KBN         詳細区分
      *                    社保  医保  001:公費併用合計、002:政、003:船、004:日、005:日特、
      *                                006:共、007:組、008:自、009:退
      *                          老人  001:公費併用合計、002:単独合計
      *                          公費  001〜999
      *                    国保        001
      *----------------------------------------------------------------------------------
      *    統計用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-KEY.
               05  TOKEI-SKYKBN        PIC X(01).
               05  TOKEI-TEISYUTU      PIC X(01).
               05  TOKEI-HKNJANUM      PIC X(08).
               05  TOKEI-SYOKBN        PIC X(01).
               05  TOKEI-NYUGAIKBN     PIC X(01).
               05  TOKEI-ZAIKBN        PIC X(01).
               05  TOKEI-SRYYMFLG      PIC 9(03).
               05  TOKEI-HKNKBN        PIC X(01).
               05  TOKEI-SYUBETUKBN    PIC X(01).
               05  TOKEI-KBN           PIC X(03).
               05  TOKEI-JYUNBAN       PIC X(131).
           03  TOKEI-SRYYM             PIC X(06).
           03  TOKEI-PTNUM             PIC X(20).
           03  TOKEI-RENNUM            PIC 9(01).
           03  TOKEI-NAME              PIC X(26).
           03  TOKEI-NISSU             PIC 9(06).
           03  TOKEI-TENSU             PIC 9(10).
      *    クリア用ファイル
       FD  TOKEIX-FILE.
       01  TOKEIX-REC                  PIC X(200).
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *    ＣＳＶファイル
       FD  RECECSV-FILE.
       01  RECECSV-REC                 PIC X(400).
      *
       WORKING-STORAGE             SECTION.
      *
      *    統計ファイル
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
           03  FILLER                  PIC X(04)   VALUE   ".dat".
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //RECEERR//.
           03  FILLER                  PIC X(04)   VALUE   ".dat".
      *
           COPY    "HCMSL55.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-TOKEIX              PIC X(02).
           03  STS-RECEERR             PIC X(02).
           03  STS-RECECSV             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-CSV                 PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-RECE10              PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-HKNJAINF            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
      *
      *PARAの説明---------------------------------------------------------------------
      *        DENPPRTYM   請求日付
      *        TEISYUTU    提出先区分  0:全部、1:社保、2:国保
      *        HKNJANUM    保険者番号  国保の時だけ使用
      *        SYOKBN      処理区分    0:全部、1:通常分、2:返戻分、3:月遅れ分、4:請求しない
      *        NYUGAIKBN   入外区分    O:全部、1:入院分、2:外来分
      *        ZAIKBN      在総診区分  0:全部、1:その他のレセ、2:在総診または在医総を算定したレセ
      *        SYUBETUKBN  種別区分
      *                    社保  医保  1:老人９割、2:老人８割、3:保本人、4:家族、5:三歳
      *                          老人  6:老人９割、7:老人８割
      *                          公費  8:公費と公費の併用、9:公費単独
      *                    国保  一般 10:老人９割、11:老人８割、12:被保険者、13:３歳
      *                          退職 14:本人、15:老人９割、16:老人８割、17:被扶養者、18:三歳
      *                          老人 19:老人９割、20:老人８割
      *        KBN         詳細区分
      *                    社保  医保  1:公費併用合計、2:政、3:船、4:日、5:日特、
      *                                6:共、7:組、8:自、9:退
      *                          老人  1:公費併用合計、2:単独合計
      *        JYUNBAN     出力順      0:患者カナ氏名順、1:患者番号順、2:点数順
      *-------------------------------------------------------------------------------
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPID         PIC X(24).
           03  WRK-PARA-DENPPRTYM      PIC X(06).
           03  WRK-PARA-TEISYUTU       PIC X(01).
           03  WRK-PARA-HKNJANUM       PIC X(08).
           03  WRK-PARA-SYOKBN         PIC X(01).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-PARA-ZAIKBN         PIC X(01).
           03  WRK-PARA-SYUBETU-G.
               05  WRK-PARA-SYUBETUKBN PIC X(02).
               05  WRK-PARA-KBN        PIC X(01).
           03  WRK-PARA-JYUNBAN        PIC X(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE10              PIC 9(06).
           03  CNT-PAGE                PIC 9(04).
           03  CNT-LINE                PIC 9(03).
      *
      *    集計領域  （１）小計、（２）合計、（３）レセプト合計
       01  SUM-AREA.
           03  SUM-GOK-TBL             OCCURS  3.
               05  SUM-GOK-KENSU       PIC 9(06).
               05  SUM-GOK-NISSU       PIC 9(06).
               05  SUM-GOK-TENSU       PIC 9(10).
      *
      *    一時領域
       01  WRK-AREA.
      *    編集用キー
           03  WRK-KEY.
               05  WRK-SKYKBN          PIC X(01).
               05  WRK-TEISYUTU        PIC X(01).
               05  WRK-HKNJANUM        PIC X(08).
               05  WRK-SYOKBN          PIC X(01).
               05  WRK-NYUGAIKBN       PIC X(01).
               05  WRK-ZAIKBN          PIC X(01).
               05  WRK-SRYYMFLG        PIC 9(03).
               05  WRK-HKNKBN          PIC X(01).
               05  WRK-SYUBETUKBN      PIC X(01).
               05  WRK-KBN             PIC X(03).
               05  WRK-SRYYM           PIC X(06).
               05  WRK-RENNUM          PIC 9(01).
      *    患者カナ氏名順
           03  WRK-KEY1.
               05  WRK-KEY1-KANANAME   PIC X(100).
               05  WRK-KEY1-PTNUM      PIC X(20).
               05  WRK-KEY1-RENNUM     PIC 9(01).
               05  WRK-KEY1-TENSU      PIC 9(10).
      *    患者番号順
           03  WRK-KEY2.
               05  WRK-KEY2-PTNUM      PIC X(20).
               05  WRK-KEY2-RENNUM     PIC 9(01).
               05  WRK-KEY2-KANANAME   PIC X(100).
               05  WRK-KEY2-TENSU      PIC 9(10).
      *    点数順
           03  WRK-KEY3.
               05  WRK-KEY3-TENSU      PIC 9(10).
               05  WRK-KEY3-KANANAME   PIC X(100).
               05  WRK-KEY3-PTNUM      PIC X(20).
               05  WRK-KEY3-RENNUM     PIC 9(01).
      *
      *    エラー情報
           03  WRK-RECEERR             PIC X(200).
      *    検索パス
           03  WRK-PATH                PIC X(64).
      *
      *    日付編集用
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-DENPYMD             PIC X(16).
      *
      *    診療年月編集用
           03  WRK-HENSRYYM-G.
               05  WRK-HENSRYYM.
                   07  WRK-HENSRY-YY   PIC 9(04).
                   07  WRK-HENSRY-MM   PIC 9(02).
               05  WRK-HENDENPYM.
                   07  WRK-HENDENP-YY  PIC 9(04).
                   07  WRK-HENDENP-MM  PIC 9(02).
               05  WRK-HENSYY          PIC S9(04).
               05  WRK-HENSMM          PIC S9(02).
      *
      *    数字編集
           03  WRK-Z4                  PIC ZZZZ.
           03  WRK-Z5                  PIC ZZZZ9.
           03  WRK-Z8                  PIC ZZZZZZZ9.
           03  WRK-HENPTNUM            PIC X(20).
           03  WRK-HENPTNUM2           PIC X(08).
      *
      *    印刷編集領域
       01  WRK-PRT-AREA.
      *    編集行
           03  WRK-PRT-LINE            PIC X(158).
      *    編集用レセプト種別行
           03  WRK-PRT-LINE-AREA.
               05  WRK-PRT-GOKEI       PIC X(22).
               05  WRK-PRT-KENSU       PIC 9(06).
               05  WRK-PRT-NISSU       PIC 9(06).
               05  WRK-PRT-TENSU       PIC 9(10).
      *    帳票タイトル
           03  WRK-PRT1-AREA.
               05  WRK-PRT1-HOSPNAME   PIC X(50).
               05  WRK-PRT1-FILLER1    PIC X(12).
               05  WRK-PRT1-TITLE      PIC X(42).
               05  WRK-PRT1-FILLER2    PIC X(04).
               05  WRK-PRT1-DENPPRTYM  PIC X(16).
               05  WRK-PRT1-FILLER3    PIC X(04).
               05  WRK-PRT1-BUNRUI     PIC X(16).
               05  WRK-PRT1-FILLER4    PIC X(04).
               05  WRK-PRT1-PAGE       PIC X(04).
               05  WRK-PRT1-FILLER5    PIC X(02).
      *    保険者タイトル
           03  WRK-PRT2-AREA.
               05  WRK-PRT2-HKNJANUM   PIC X(08).
               05  WRK-PRT2-FILLER     PIC X(02).
               05  WRK-PRT2-HKNJANAME  PIC X(40).
      *    レセプト種別明細
           03  WRK-PRT3-AREA.
               05  WRK-PRT3-FILLER1    PIC X(02).
               05  WRK-PRT3-GOKEI      PIC X(22).
               05  WRK-PRT3-KENSU      PIC X(05).
               05  WRK-PRT3-FILLER2    PIC X(02).
               05  WRK-PRT3-NISSU      PIC X(05).
               05  WRK-PRT3-FILLER3    PIC X(02).
               05  WRK-PRT3-TENSU      PIC X(08).
               05  WRK-PRT3-FILLER4    PIC X(02).
      *    患者明細
           03  WRK-PRT4-AREA.
               05  WRK-PRT4-G              OCCURS  3.
                   07  WRK-PRT4-FILLER1    PIC X(02).
                   07  WRK-PRT4-PTNUM      PIC X(08).
                   07  WRK-PRT4-FILLER2    PIC X(02).
                   07  WRK-PRT4-NAME       PIC X(18).
                   07  WRK-PRT4-NISSU      PIC X(05).
                   07  WRK-PRT4-FILLER3    PIC X(02).
                   07  WRK-PRT4-TENSU      PIC X(08).
                   07  WRK-PRT4-FILLER4    PIC X(02).
      *    CSV編集用
           COPY    "CPCSV.INC".
      *
           COPY    "CPSHELLTBL.INC".
           COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
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
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
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
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
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
           INITIALIZE                  WRK-PARA
      *
      *    ＤＢオープン処理
           PERFORM 100-DBOPEN-SEC
      *
      *    パラメタセット
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
                                               WRK-PARA-DENPPRTYM
                                               WRK-PARA-TEISYUTU
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-ZAIKBN
                                               WRK-PARA-SYUBETU-G
                                               WRK-PARA-JYUNBAN
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBG014"          TO  JOB-PGID
           MOVE    "保険請求確認リスト"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
      *
      *    ファイル設定
           MOVE    "TOKEIZZ"           TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  TOKEIPARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
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
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYSYMD
      *
      *    対象年月セット
           MOVE    WRK-PARA-DENPPRTYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-DENPYMD
      *
      *    提出先判定
           EVALUATE    WRK-PARA-SYUBETUKBN
      *        社保をセット
               WHEN    "01"    THRU    "09"
                   MOVE    "1"         TO  WRK-PARA-TEISYUTU
      *        国保をセット
               WHEN    "10"    THRU    "20"
                   MOVE    "2"         TO  WRK-PARA-TEISYUTU
           END-EVALUATE
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    統計用ファイルをクリアする
           OPEN    OUTPUT              TOKEIX-FILE
           CLOSE                       TOKEIX-FILE
      *    統計用ファイルを開く
           OPEN    I-O                 TOKEI-FILE
      *
      *    請求社保分集計処理
           IF        ( WRK-PARA-SYOKBN     NOT = "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT = "2" )
               AND   ( WRK-PARA-HKNJANUM       =   SPACE )
               MOVE    "SEIKYU-KEY3"       TO  WRK-PATH
               PERFORM 210-SYUKEI-READ-SEC
           END-IF
      *
      *    請求国保分集計処理
           IF        ( WRK-PARA-SYOKBN     NOT = "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT = "1" )
               MOVE    "SEIKYU-KEY4"       TO  WRK-PATH
               PERFORM 210-SYUKEI-READ-SEC
           END-IF
      *
      *    請求しない分集計処理
           IF        ( WRK-PARA-SYOKBN     NOT = "1" )
               AND   ( WRK-PARA-SYOKBN     NOT = "2" )
               AND   ( WRK-PARA-SYOKBN     NOT = "3" )
               MOVE    "SEIKYU-KEY5"       TO  WRK-PATH
               PERFORM 210-SYUKEI-READ-SEC
           END-IF
      *
      *    統計用ファイルを閉じる
           CLOSE                       TOKEI-FILE
      *    統計用ファイルを開く
           OPEN    INPUT               TOKEI-FILE
      *    ＣＳＶファイルを開く
           OPEN    OUTPUT              RECECSV-FILE
      *
      *    印刷処理
           PERFORM 300-PRINT-HENSYU-SEC
      *
      *    統計用ファイルを閉じる
           CLOSE   RECECSV-FILE
           CLOSE                       TOKEI-FILE
      *    統計用ファイルをクリアする
           OPEN    OUTPUT              TOKEIX-FILE
           CLOSE                       TOKEIX-FILE
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       210-SYUKEI-READ-SEC         SECTION.
      *
      *    請求読み込み
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-RECE10  =   1 )
                   OR    ( FLG-ERR     =   1 )
      *
      *        社保分集計処理
               IF        ( WRK-TEISYUTU    =   "1" )
      *            医保
                   PERFORM 220-IHO-SYAHO-SYUKEI-SEC
      *            老人
                   PERFORM 220-ROU-SYAHO-SYUKEI-SEC
      *            公費
                   PERFORM 220-KOH-SYAHO-SYUKEI-SEC
               END-IF
      *
      *        国保分集計処理
               IF        ( WRK-TEISYUTU    =   "2" )
      *            一般・退職
                   PERFORM 220-IHO-KOKHO-SYUKEI-SEC
      *            老人
                   PERFORM 220-ROU-KOKHO-SYUKEI-SEC
               END-IF
      *
      *        請求読み込み
               PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
      *
      *    請求読み込み
           PERFORM 900-SEIKYU-CLOSE-SEC
      *
           .
       210-SYUKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保分医療保険分集計処理
      *****************************************************************
       220-IHO-SYAHO-SYUKEI-SEC    SECTION.
      *
      *    医保でない場合は処理抜ける
           IF        ( RECE10-RJNHKNNUM    NOT =   SPACE )
               OR    ( RECE10-HKNNUM           =   SPACE )
               GO  TO  220-IHO-SYAHO-SYUKEI-EXT
           END-IF
      *
           MOVE    "1"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
      *    船員保険の補助区分が前期高齢者以外の時
           IF        ( RECE10-HKNNUM       =   "002" )
               AND   (( RECE10-HOJOKBN     =   "1"   )
               OR     ( RECE10-HOJOKBN     =   "2"   )
               OR     ( RECE10-HOJOKBN     =   "3"   )
               OR     ( RECE10-HOJOKBN     =   SPACE ))
      *        三歳以下の時
               IF      RECE10-AGE      <   3
                   MOVE    "5"             TO  WRK-SYUBETUKBN
               ELSE
      *            本人の時
                   IF      RECE10-HONKZKKBN        =   "1"
                       MOVE    "3"             TO  WRK-SYUBETUKBN
                   ELSE
      *            家族の時
                       MOVE    "4"             TO  WRK-SYUBETUKBN
                   END-IF
               END-IF
           ELSE
      *        共済組合の補助区分が前期高齢者以外の時
               IF         ( RECE10-HKNNUM  >=  "031" )
                   AND    ( RECE10-HKNNUM  <=  "034" )
                   AND   (( RECE10-HOJOKBN =   "2" )
                   OR     ( RECE10-HOJOKBN =   SPACE ))
      *            三歳以下の時
                   IF      RECE10-AGE      <   3
                       MOVE    "5"             TO  WRK-SYUBETUKBN
                   ELSE
      *                本人の時
                       IF      RECE10-HONKZKKBN        =   "1"
                           MOVE    "3"             TO  WRK-SYUBETUKBN
                       ELSE
      *                家族の時
                           MOVE    "4"             TO  WRK-SYUBETUKBN
                       END-IF
                   END-IF
               ELSE
                   IF      RECE10-AGE      >=  70
      *                船員保険の補助区分が前期高齢者の時
                       IF      RECE10-HKNNUM           =   "002"
                           EVALUATE    RECE10-HOJOKBN
                               WHEN    "9"
                               WHEN    "A"
                               WHEN    "B"
                               WHEN    "C"
                                   MOVE    "1"     TO  WRK-SYUBETUKBN
                               WHEN    "8"
                               WHEN    "D"
                               WHEN    "E"
                               WHEN    "F"
                                   MOVE    "2"     TO  WRK-SYUBETUKBN
                           END-EVALUATE
                       ELSE
      *                共済組合の補助区分が前期高齢者の時
                           IF        ( RECE10-HKNNUM   >=  "031" )
                               AND   ( RECE10-HKNNUM   <=  "034" )
                               EVALUATE    RECE10-HOJOKBN
                                   WHEN    "9"
                                   WHEN    "B"
                                       MOVE    "1" TO  WRK-SYUBETUKBN
                                   WHEN    "8"
                                   WHEN    "E"
                                       MOVE    "2" TO  WRK-SYUBETUKBN
                               END-EVALUATE
                           ELSE
                               EVALUATE    RECE10-KYURATE
                                   WHEN    9
                                       MOVE    "1" TO  WRK-SYUBETUKBN
                                   WHEN    8
                                       MOVE    "2" TO  WRK-SYUBETUKBN
                               END-EVALUATE
                           END-IF
                       END-IF
                   ELSE
      *                三歳以下の時
                       IF      RECE10-AGE      <   3
                           MOVE    "5"             TO  WRK-SYUBETUKBN
                       ELSE
      *                    本人の時
                           IF      RECE10-HONKZKKBN    =   "1"
                               MOVE    "3"         TO  WRK-SYUBETUKBN
                           ELSE
      *                    家族の時
                               MOVE    "4"         TO  WRK-SYUBETUKBN
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    詳細区分分類
           IF      RECE10-KOHNUM(1)    NOT =   SPACE
      *        公費と併用
               MOVE    "001"           TO  WRK-KBN
           ELSE
      *        単独
               EVALUATE    RECE10-HKNNUM
                   WHEN    "001"
                       MOVE    "002"           TO  WRK-KBN
                   WHEN    "002"
                       MOVE    "003"           TO  WRK-KBN
                   WHEN    "003"
                       MOVE    "004"           TO  WRK-KBN
                   WHEN    "004"
                       MOVE    "005"           TO  WRK-KBN
                   WHEN    "031"  THRU  "034"
                       MOVE    "006"           TO  WRK-KBN
                   WHEN    "006"
                       MOVE    "007"           TO  WRK-KBN
                   WHEN    "007"
                       MOVE    "008"           TO  WRK-KBN
                   WHEN    "063"
                   WHEN    "072"  THRU  "075"
                       MOVE    "009"           TO  WRK-KBN
               END-EVALUATE
           END-IF
      *
      *    詳細区分が違う場合は処理抜ける
           IF        ( WRK-PARA-KBN    NOT =   SPACE)
               AND   ( WRK-PARA-KBN    NOT =   "0" )
               IF      WRK-KBN(3:1)    NOT =   WRK-PARA-KBN
                   GO  TO  220-IHO-SYAHO-SYUKEI-EXT
               END-IF
           END-IF
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-IHO-SYAHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保分老人保健分集計処理
      *****************************************************************
       220-ROU-SYAHO-SYUKEI-SEC    SECTION.
      *
      *    老人でない場合処理抜ける
           IF      RECE10-RJNHKNNUM    =   SPACE
               GO  TO  220-ROU-SYAHO-SYUKEI-EXT
           END-IF
      *
           MOVE    "2"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
           EVALUATE    RECE10-KYURATE
               WHEN    9
                   MOVE    "1"             TO  WRK-SYUBETUKBN
               WHEN    8
                   MOVE    "2"             TO  WRK-SYUBETUKBN
           END-EVALUATE
      *
      *    詳細区分分類
           IF      RECE10-KOHNUM(1)    NOT =   SPACE
               MOVE    "001"           TO  WRK-KBN
           ELSE
               MOVE    "002"           TO  WRK-KBN
           END-IF
      *
      *    詳細区分が違う場合は処理抜ける
           IF        ( WRK-PARA-KBN    NOT =   SPACE)
               AND   ( WRK-PARA-KBN    NOT =   "0" )
               IF      WRK-KBN(3:1)    NOT =   WRK-PARA-KBN
                   GO  TO  220-ROU-SYAHO-SYUKEI-EXT
               END-IF
           END-IF
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-ROU-SYAHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保分公費保険分集計処理
      *****************************************************************
       220-KOH-SYAHO-SYUKEI-SEC    SECTION.
      *
      *    公費でない場合処理抜ける
           IF      RECE10-KOHNUM(1)    =   SPACE
               GO  TO  220-KOH-SYAHO-SYUKEI-EXT
           END-IF
      *
           MOVE    "3"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
      *
      *    レセプト種別区分分類
           IF        ( RECE10-RJNHKNNUM    NOT =   SPACE )
               OR    ( RECE10-HKNNUM       NOT =   SPACE )
               GO  TO  220-KOH-SYAHO-SYUKEI-EXT
           ELSE
               IF      RECE10-KOHNUM(2)    NOT =   SPACE
      *            公費と公費の併用欄
                   MOVE    "1"             TO  WRK-SYUBETUKBN
               ELSE
      *            公費単独欄
                   MOVE    "2"             TO  WRK-SYUBETUKBN
               END-IF
           END-IF
      *
      *    詳細区分に公費番号セット
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       4
                   OR  RECE10-KOHNUM(IDY)  =   SPACE
      *
      *        特定疾患無しのとき
               IF      RECE10-KOHNUM(IDY)  =   "091"
                   MOVE    "051"           TO  RECE10-KOHNUM(IDY)
               END-IF
      *
               MOVE    RECE10-KOHNUM(IDY)  TO  WRK-KBN
               COMPUTE IDX     =   IDY     +   1
      *
      *        保健集計処理
               PERFORM 230-SYUKEI-CHK-SEC
           END-PERFORM
      *
           .
       220-KOH-SYAHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保分医療保険分集計処理
      *****************************************************************
       220-IHO-KOKHO-SYUKEI-SEC   SECTION.
      *
      *    一般・退職でない場合は処理抜ける
           IF        ( RECE10-RJNHKNNUM    NOT =   SPACE )
               OR    ( RECE10-HKNNUM-S         =   SPACE )
               GO  TO  220-IHO-KOKHO-SYUKEI-EXT
           END-IF
      *
           MOVE    "1"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    "001"               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
           EVALUATE    RECE10-HKNNUM-S
               WHEN    "060"
                   IF      RECE10-AGE          >=  70
                       EVALUATE    RECE10-KYURATE
                           WHEN    9
                               MOVE    "1"         TO  WRK-SYUBETUKBN
                           WHEN    8
                               MOVE    "2"         TO  WRK-SYUBETUKBN
                       END-EVALUATE
                   ELSE
                       IF      RECE10-AGE      <   3
                           MOVE    "4"         TO  WRK-SYUBETUKBN
                       ELSE
                           MOVE    "3"         TO  WRK-SYUBETUKBN
                       END-IF
                   END-IF
               WHEN    "067"
                   IF      RECE10-AGE          >=  70
                       EVALUATE    RECE10-KYURATE
                           WHEN    9
                               MOVE    "6"         TO  WRK-SYUBETUKBN
                           WHEN    8
                               MOVE    "7"         TO  WRK-SYUBETUKBN
                       END-EVALUATE
                   ELSE
                       IF      RECE10-AGE      <   3
                           MOVE    "9"         TO  WRK-SYUBETUKBN
                       ELSE
                           IF      RECE10-HONKZKKBN    =   "1"
                               MOVE    "5"         TO  WRK-SYUBETUKBN
                           ELSE
                               MOVE    "8"         TO  WRK-SYUBETUKBN
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-IHO-KOKHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保分老人保健分集計処理
      *****************************************************************
       220-ROU-KOKHO-SYUKEI-SEC    SECTION.
      *
      *    老人でない場合は処理抜ける
           IF      RECE10-RJNHKNNUM    =   SPACE
               GO  TO  220-ROU-KOKHO-SYUKEI-EXT
           END-IF
      *
           MOVE    "2"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    "001"               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
           EVALUATE    RECE10-KYURATE
               WHEN    9
                   MOVE    "1"             TO  WRK-SYUBETUKBN
               WHEN    8
                   MOVE    "2"             TO  WRK-SYUBETUKBN
           END-EVALUATE
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-ROU-KOKHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保健集計処理
      *****************************************************************
       230-SYUKEI-CHK-SEC          SECTION.
      *
      *    キーがない場合は処理抜ける
           IF        ( WRK-SYUBETUKBN  =   SPACE )
               OR    ( WRK-KBN         =   SPACE )
               OR    ( IDX             =   ZERO  )
               GO  TO  230-SYUKEI-CHK-EXT
           END-IF
      *
      *    レセ種別チェック
           EVALUATE    WRK-TEISYUTU    ALSO    WRK-HKNKBN
               ALSO    WRK-SYUBETUKBN  ALSO    WRK-PARA-SYUBETUKBN
      *        指定なし
               WHEN    ANY ALSO    ANY ALSO    ANY ALSO    SPACE
               WHEN    ANY ALSO    ANY ALSO    ANY ALSO    ZERO
      *        社保医保老人９割
               WHEN    "1" ALSO    "1" ALSO    "1" ALSO    "01"
      *        社保医保老人８割
               WHEN    "1" ALSO    "1" ALSO    "2" ALSO    "02"
      *        社保医保本人
               WHEN    "1" ALSO    "1" ALSO    "3" ALSO    "03"
      *        社保医保家族
               WHEN    "1" ALSO    "1" ALSO    "4" ALSO    "04"
      *        社保医保三歳
               WHEN    "1" ALSO    "1" ALSO    "5" ALSO    "05"
      *        社保老人９割
               WHEN    "1" ALSO    "2" ALSO    "1" ALSO    "06"
      *        社保老人８割
               WHEN    "1" ALSO    "2" ALSO    "2" ALSO    "07"
      *        社保公費と公費の併用
               WHEN    "1" ALSO    "3" ALSO    "1" ALSO    "08"
      *        社保公費単独
               WHEN    "1" ALSO    "3" ALSO    "2" ALSO    "09"
      *        国保一般老人９割
               WHEN    "2" ALSO    "1" ALSO    "1" ALSO    "10"
      *        国保一般老人８割
               WHEN    "2" ALSO    "1" ALSO    "2" ALSO    "11"
      *        国保一般被保険者
               WHEN    "2" ALSO    "1" ALSO    "3" ALSO    "12"
      *        国保一般三歳
               WHEN    "2" ALSO    "1" ALSO    "4" ALSO    "13"
      *        国保退職本人
               WHEN    "2" ALSO    "1" ALSO    "5" ALSO    "14"
      *        国保退職老人９割
               WHEN    "2" ALSO    "1" ALSO    "6" ALSO    "15"
      *        国保退職老人８割
               WHEN    "2" ALSO    "1" ALSO    "7" ALSO    "16"
      *        国保退職被扶養者
               WHEN    "2" ALSO    "1" ALSO    "8" ALSO    "17"
      *        国保退職三歳
               WHEN    "2" ALSO    "1" ALSO    "9" ALSO    "18"
      *        国保老人９割
               WHEN    "2" ALSO    "2" ALSO    "1" ALSO    "19"
      *        国保老人８割
               WHEN    "2" ALSO    "2" ALSO    "2" ALSO    "20"
                   CONTINUE
      *        指定したレセ種別が正しくない場合は処理抜ける
               WHEN    OTHER
                   GO  TO  230-SYUKEI-CHK-EXT
           END-EVALUATE
      *
      *    患者別集計処理
           PERFORM 240-PTINF-SYUKEI-SEC
      *
           .
       230-SYUKEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者別集計処理
      *****************************************************************
       240-PTINF-SYUKEI-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TOKEI
           MOVE    ZERO                TO  WRK-RENNUM
      *
      *    患者連番最大９まで
           PERFORM UNTIL ( FLG-TOKEI   =    1 )
                   OR    ( WRK-RENNUM  >=   9 )
               ADD     1               TO  WRK-RENNUM
      *        統計読み込み
               INITIALIZE                  TOKEI-REC
               MOVE    WRK-SKYKBN      TO  TOKEI-SKYKBN
               MOVE    WRK-TEISYUTU    TO  TOKEI-TEISYUTU
               MOVE    WRK-HKNJANUM    TO  TOKEI-HKNJANUM
               MOVE    WRK-SYOKBN      TO  TOKEI-SYOKBN
               MOVE    WRK-SRYYMFLG    TO  TOKEI-SRYYMFLG
               MOVE    WRK-NYUGAIKBN   TO  TOKEI-NYUGAIKBN
               MOVE    WRK-ZAIKBN      TO  TOKEI-ZAIKBN
               MOVE    WRK-HKNKBN      TO  TOKEI-HKNKBN
               MOVE    WRK-SYUBETUKBN  TO  TOKEI-SYUBETUKBN
               MOVE    WRK-KBN         TO  TOKEI-KBN
               EVALUATE    WRK-PARA-JYUNBAN
                   WHEN    "1"
                       MOVE    SPACE           TO  WRK-KEY2
                       MOVE    RECE10-PTNUM    TO  WRK-KEY2-PTNUM
                       MOVE    PTINF-KANANAME  TO  WRK-KEY2-KANANAME
                       MOVE    WRK-RENNUM      TO  WRK-KEY2-RENNUM
                       MOVE    RECE10-TOTALTEN(IDX)
                                               TO  WRK-KEY2-TENSU
                       MOVE    WRK-KEY2        TO  TOKEI-JYUNBAN
                   WHEN    "2"
                       MOVE    SPACE           TO  WRK-KEY3
                       MOVE    RECE10-PTNUM    TO  WRK-KEY3-PTNUM
                       MOVE    PTINF-KANANAME  TO  WRK-KEY3-KANANAME
                       MOVE    WRK-RENNUM      TO  WRK-KEY3-RENNUM
                       MOVE    RECE10-TOTALTEN(IDX)
                                               TO  WRK-KEY3-TENSU
                       MOVE    WRK-KEY3        TO  TOKEI-JYUNBAN
                   WHEN    OTHER
                       MOVE    SPACE           TO  WRK-KEY1
                       MOVE    RECE10-PTNUM    TO  WRK-KEY1-PTNUM
                       MOVE    PTINF-KANANAME  TO  WRK-KEY1-KANANAME
                       MOVE    WRK-RENNUM      TO  WRK-KEY1-RENNUM
                       MOVE    RECE10-TOTALTEN(IDX)
                                               TO  WRK-KEY1-TENSU
                       MOVE    WRK-KEY1        TO  TOKEI-JYUNBAN
               END-EVALUATE
               PERFORM 900-TOKEI-READ-SEC
           END-PERFORM
      *
      *    患者データ無ければ新規作成
           MOVE    WRK-SRYYM               TO  TOKEI-SRYYM
           MOVE    RECE10-PTNUM            TO  TOKEI-PTNUM
           MOVE    WRK-RENNUM              TO  TOKEI-RENNUM
           MOVE    PTINF-NAME              TO  TOKEI-NAME
           MOVE    RECE10-JNISSU(IDX)      TO  TOKEI-NISSU
           MOVE    RECE10-TOTALTEN(IDX)    TO  TOKEI-TENSU
           WRITE   TOKEI-REC
      *
           .
       240-PTINF-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       300-PRINT-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-KEY
           MOVE    ZERO                TO  CNT-PAGE
           MOVE    ZERO                TO  CNT-LINE
      *
      *    統計ファイル読み込み
           INITIALIZE                  TOKEI-REC
           PERFORM 900-TOKEI-NEXT-SEC
      *
           PERFORM UNTIL ( FLG-TOKEI   =   1 )
      *
      *        請求、未請求分
               IF     TOKEI-SKYKBN     =   "1"
      *            帳票編集<見出し>処理
                   PERFORM 310-HEAD-HEN-SEC
      *            帳票編集<明細>処理
                   PERFORM 320-BODY-HEN-SEC
                       UNTIL ( FLG-TOKEI   =   1 )
                       OR    ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                       OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                       OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
                       OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
               END-IF
      *
      *        請求しない分
               IF      TOKEI-SKYKBN    =   "2"
      *            帳票編集<見出し>処理２
                   PERFORM 310-HEAD-HEN2-SEC
      *            帳票編集<明細>処理２
                   PERFORM 320-BODY-HEN2-SEC
                       UNTIL ( FLG-TOKEI   =   1 )
                       OR    ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               END-IF
           END-PERFORM
      *
           .
       300-PRINT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
      *    初期処理
           PERFORM 310-HEAD-KEY-SET-SEC
      *
      *    分類セット
           EVALUATE    WRK-TEISYUTU    ALSO    WRK-SYOKBN
               WHEN    "1"     ALSO    "1"
                   MOVE    "（社保分）　　　"  TO  WRK-PRT1-BUNRUI
               WHEN    "1"     ALSO    "2"
                   MOVE    "（社保返戻分）　"  TO  WRK-PRT1-BUNRUI
               WHEN    "1"     ALSO    "3"
                   MOVE    "（社保月遅れ分）"  TO  WRK-PRT1-BUNRUI
               WHEN    "2"     ALSO    "1"
                   MOVE    "（国保分）　　　"  TO  WRK-PRT1-BUNRUI
               WHEN    "2"     ALSO    "2"
                   MOVE    "（国保返戻分）　"  TO  WRK-PRT1-BUNRUI
               WHEN    "2"     ALSO    "3"
                   MOVE    "（国保月遅れ分）"  TO  WRK-PRT1-BUNRUI
           END-EVALUATE
      *
           MOVE    WRK-TEISYUTU    TO  CSV-TEISYUTU
           MOVE    WRK-SYOKBN      TO  CSV-SYOKBN
      *
      *    国保の場合は保険者名セット
           IF      WRK-TEISYUTU    =   "2"
               MOVE    SPACE           TO  HKNJAINF-REC
               INITIALIZE                  HKNJAINF-REC
               MOVE    WRK-PARA-HOSPID TO  HKNJA-HOSPID
               MOVE    WRK-HKNJANUM    TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC    TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-READ-SEC
               IF      FLG-HKNJAINF        =   ZERO
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    HKNJA-HKNJANAME TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   0
                       MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-PRT2-HKNJANAME
                                               CSV-HKNJANAME
                   END-IF
               END-IF
               MOVE    WRK-HKNJANUM    TO  WRK-PRT2-HKNJANUM
                                           CSV-HKNJANUM
               MOVE    SPACE           TO  WRK-PRT2-FILLER
           END-IF
      *
      *    医療機関名セット
           PERFORM 310-HEAD-SYSKANRI-HEN-SEC
      *
      *    帳票クリア
           PERFORM 310-HEAD-SET-SEC
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理２
      *****************************************************************
       310-HEAD-HEN2-SEC           SECTION.
      *
      *    初期処理
           PERFORM 310-HEAD-KEY-SET-SEC
      *
      *    分類セット
           MOVE    "（請求しない分）"  TO  WRK-PRT1-BUNRUI
      *
           MOVE    "4"                 TO  CSV-SYOKBN
      *
      *    医療機関名セット
           PERFORM 310-HEAD-SYSKANRI-HEN-SEC
      *
      *    帳票クリア
           PERFORM 310-HEAD-SET-SEC
      *
           .
       310-HEAD-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>初期処理
      *****************************************************************
       310-HEAD-KEY-SET-SEC        SECTION.
      *
      *    キーセット
           MOVE    SPACE               TO  WRK-KEY
           MOVE    ZERO                TO  WRK-SRYYMFLG
           MOVE    TOKEI-SKYKBN        TO  WRK-SKYKBN
           MOVE    TOKEI-TEISYUTU      TO  WRK-TEISYUTU
           MOVE    TOKEI-HKNJANUM      TO  WRK-HKNJANUM
           MOVE    TOKEI-SYOKBN        TO  WRK-SYOKBN
           MOVE    ZERO                TO  SUM-AREA
      *
      *    帳票タイトル初期セット
           MOVE    SPACE               TO  WRK-PRT1-AREA
           MOVE    "　　　　　　　　　　　　　　　　　　　　　　　　　"
                                       TO  WRK-PRT1-HOSPNAME
           MOVE    "　　　　　　　　"  TO  WRK-PRT1-BUNRUI
           MOVE    "　＜＜　保険請求確認リスト　＞＞　　　　　"
                                       TO  WRK-PRT1-TITLE
           MOVE    "　　　　　　"      TO  WRK-PRT1-FILLER1
           MOVE    "　　"              TO  WRK-PRT1-FILLER2
           MOVE    "分　"              TO  WRK-PRT1-FILLER3
           MOVE    "　　"              TO  WRK-PRT1-FILLER4
           MOVE    "頁"                TO  WRK-PRT1-FILLER5
           MOVE    WRK-DENPYMD         TO  WRK-PRT1-DENPPRTYM
      *
      *    保険者名クリア
           MOVE    SPACE               TO  WRK-PRT2-AREA
      *
           INITIALIZE                      CSV-REC
           MOVE    0                   TO  CSV-RENNUM
                                           CSV-NISSU
                                           CSV-TENSU
           MOVE    WRK-PARA-DENPPRTYM(1:6)
                                       TO  CSV-SKYYM
           MOVE    "00"                TO  CSV-SRYKA
      *
           .
       310-HEAD-KEY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>医療機関情報処理
      *****************************************************************
       310-HEAD-SYSKANRI-HEN-SEC   SECTION.
      *
      *    医療機関基本情報読み込み
           PERFORM 900-SYS1001-READ-SEC
      *
      *    医療機関名セット
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    SYS-1001-HOSPNAME   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   0
               COMPUTE KANACHK-MAX     =   KANACHK-MAX +   1
               PERFORM UNTIL ( KANACHK-MAX >=  50 )
                   MOVE    "　"
                           TO  KANACHK-OUT-INPUT(KANACHK-MAX:2)
                   COMPUTE KANACHK-MAX     =   KANACHK-MAX +   2
               END-PERFORM
               MOVE    KANACHK-OUT-INPUT(1:50)
                                       TO  WRK-PRT1-HOSPNAME
           END-IF
      *
           .
       310-HEAD-SYSKANRI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票クリア処理
      *****************************************************************
       310-HEAD-SET-SEC            SECTION.
      *
           MOVE    5               TO  CNT-LINE
      *
           INITIALIZE                  HCMSL55
           MOVE    SPACE           TO  HCMSL55
      *
           .
       310-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
      *    グループ見出しセット
           PERFORM 3201-BODY-HEN-STR-SEC
      *
      *    患者情報
           PERFORM 3202-BODY-SUB-HEN-SEC
      *
      *    区分合計
           PERFORM 3202-KBN-BODY-END-SEC
      *
      *    グループ合計セット
           PERFORM 3203-BODY-HEN-END-SEC
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理２
      *****************************************************************
       320-BODY-HEN2-SEC           SECTION.
      *
      *    グループ見出しセット
           PERFORM 3201-BODY-HEN-STR2-SEC
      *
      *    患者情報
           PERFORM 3202-BODY-SUB-HEN2-SEC
      *
      *    グループ合計セット
           PERFORM 3203-BODY-HEN-END2-SEC
      *
           .
       320-BODY-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>セット処理
      *****************************************************************
       320-BODY-SET-SEC             SECTION.
      *
      *    データがない時は処理抜ける
           IF      WRK-PRT-LINE     =   SPACE
               GO  TO  320-BODY-SET-EXT
           END-IF
      *
           ADD     1                   TO  CNT-LINE
      *
           IF      CNT-LINE        >=  55
               PERFORM 390-PRINT-OUT-SEC
               PERFORM 310-HEAD-SET-SEC
           END-IF
      *
      *    データを帳票にセットする
           MOVE    WRK-PRT-LINE        TO  HCMSL55-MOJI(CNT-LINE)
           .
       320-BODY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>見出し処理
      *****************************************************************
       3201-BODY-HEN-STR-SEC       SECTION.
      *
      *    入外区分見出し
           IF      TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN
               PERFORM 3201-NYUGAIKBN-BODY-HEN-SEC
           END-IF
      *
      *    在総診区分見出し
           IF      TOKEI-ZAIKBN    NOT =   WRK-ZAIKBN
               PERFORM 3201-ZAIKBN-BODY-HEN-SEC
           END-IF
      *
      *    診療月見出し
           IF      TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG
               PERFORM 3201-SRYYM-BODY-HEN-SEC
           END-IF
      *
      *    保険区分見出し
           IF      TOKEI-HKNKBN    NOT =   WRK-HKNKBN
               MOVE    TOKEI-HKNKBN    TO  WRK-HKNKBN
               MOVE    SPACE           TO  WRK-SYUBETUKBN
               MOVE    SPACE           TO  WRK-KBN
           END-IF
      *
      *    種別区分見出し
           IF      TOKEI-SYUBETUKBN    NOT =   WRK-SYUBETUKBN
               PERFORM 3201-SYUBETU-BODY-HEN-SEC
           END-IF
      *
      *    詳細区分見出し
           IF      TOKEI-KBN           NOT =   WRK-KBN
               PERFORM 3201-KBN-BODY-HEN-SEC
           END-IF
      *
           .
       3201-BODY-HEN-STR-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>見出し処理２
      *****************************************************************
       3201-BODY-HEN-STR2-SEC      SECTION.
      *
      *    提出先入外区分見出し
           IF        ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
               OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
               PERFORM 3201-TEISYUTU-BODY-HEN-SEC
           END-IF
      *
      *    診療月見出し
           IF        ( TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG )
               PERFORM 3201-SRYYM-BODY-HEN-SEC
           END-IF
      *
      *    保険区分見出し
           IF      TOKEI-HKNKBN    NOT =   WRK-HKNKBN
               MOVE    TOKEI-HKNKBN    TO  WRK-HKNKBN
               MOVE    SPACE           TO  WRK-SYUBETUKBN
               MOVE    SPACE           TO  WRK-KBN
           END-IF
      *
      *    種別区分見出し
           IF      TOKEI-SYUBETUKBN    NOT =   WRK-SYUBETUKBN
               PERFORM 3201-SYUBETU-BODY-HEN-SEC
           END-IF
      *
      *    詳細区分見出し
           IF      TOKEI-KBN           NOT =   WRK-KBN
               PERFORM 3201-KBN-BODY-HEN-SEC
           END-IF
      *
           .
       3201-BODY-HEN-STR2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-TEISYUTU-BODY-HEN-SEC  SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           EVALUATE    TOKEI-TEISYUTU  ALSO    TOKEI-NYUGAIKBN
               WHEN    "1"     ALSO    "1"
                   MOVE    "＜社保外来＞"  TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "2"
                   MOVE    "＜社保入院＞"  TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "1"
                   MOVE    "＜国保外来＞"  TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "2"
                   MOVE    "＜国保入院＞"  TO  WRK-PRT-LINE
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-TEISYUTU  TO  WRK-TEISYUTU
           MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
           MOVE    0               TO  WRK-SRYYMFLG
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           MOVE    WRK-TEISYUTU    TO  CSV-TEISYUTU
           EVALUATE    WRK-NYUGAIKBN
               WHEN    "1"
                   MOVE    "2"     TO  CSV-NYUGAIKBN
               WHEN    "2"
                   MOVE    "1"     TO  CSV-NYUGAIKBN
           END-EVALUATE
      *
           .
       3201-TEISYUTU-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-NYUGAIKBN-BODY-HEN-SEC SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           EVALUATE    TOKEI-NYUGAIKBN
               WHEN    "1"
                   MOVE    "＜外来＞"  TO  WRK-PRT-LINE
               WHEN    "2"
                   MOVE    "＜入院＞"  TO  WRK-PRT-LINE
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
           MOVE    0               TO  WRK-SRYYMFLG
           MOVE    SPACE           TO  WRK-ZAIKBN
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           EVALUATE    WRK-NYUGAIKBN
               WHEN    "1"
                   MOVE    "2"     TO  CSV-NYUGAIKBN
               WHEN    "2"
                   MOVE    "1"     TO  CSV-NYUGAIKBN
           END-EVALUATE
      *
           .
       3201-NYUGAIKBN-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-ZAIKBN-BODY-HEN-SEC    SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           IF      TOKEI-ZAIKBN    =   "2"
               MOVE    "＜在総診または在医総を算定したレセ＞"
                                   TO  WRK-PRT-LINE
           END-IF
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
           MOVE    0               TO  WRK-SRYYMFLG
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           MOVE    WRK-ZAIKBN      TO  CSV-ZAIKBN
      *
           .
       3201-ZAIKBN-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    診療年月見出し
      *****************************************************************
       3201-SRYYM-BODY-HEN-SEC     SECTION.
      *
      *    月遅れ分以外は処理抜ける
           IF      TOKEI-SYOKBN    NOT =   "3"
               MOVE    TOKEI-SRYYMFLG  TO  WRK-SRYYMFLG
               MOVE    SPACE           TO  CSV-SRYYM
               GO  TO  3201-SRYYM-BODY-HEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-PRT-LINE
      *
      *    対象年月編集
           MOVE    TOKEI-SRYYM         TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           STRING  "＜"                DELIMITED  BY  SIZE
                   WRK-HENYMDG(1:16)   DELIMITED  BY  SIZE
                   "＞"                DELIMITED  BY  SIZE
                               INTO    WRK-PRT-LINE
           END-STRING
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-SRYYMFLG  TO  WRK-SRYYMFLG
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           MOVE    TOKEI-SRYYM     TO  CSV-SRYYM
      *
           .
       3201-SRYYM-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    種別区分見出し
      *****************************************************************
       3201-SYUBETU-BODY-HEN-SEC   SECTION.
      *
           MOVE    SPACE               TO  WRK-PRT-LINE
      *
           EVALUATE    TOKEI-TEISYUTU      ALSO    TOKEI-HKNKBN
               ALSO    TOKEI-SYUBETUKBN    ALSO    TOKEI-KBN
               WHEN    "1" ALSO    "1" ALSO    "1" ALSO    "001"
                   MOVE    "＊医保（７０歳以上９割）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "1" ALSO    ANY
                   MOVE    "＊医保単独（７０歳以上９割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "2" ALSO    "001"
                   MOVE    "＊医保（７０歳以上８割）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "2" ALSO    ANY
                   MOVE    "＊医保単独（７０歳以上８割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "3" ALSO    "001"
                   MOVE    "＊医保（本人）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "3" ALSO    ANY
                   MOVE    "＊医保単独（本人）"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "4" ALSO    "001"
                   MOVE    "＊医保（家族）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "4" ALSO    ANY
                   MOVE    "＊医保単独（家族）"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "5" ALSO    "001"
                   MOVE    "＊医保（３歳）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "5" ALSO    ANY
                   MOVE    "＊医保単独（３歳）"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "2" ALSO    "1" ALSO    ANY
                   MOVE    "＊老人９割"            TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "2" ALSO    "2" ALSO    ANY
                   MOVE    "＊老人８割"            TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "3" ALSO    "1" ALSO    ANY
                   MOVE    "＊公費と公費の併用"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "3" ALSO    "2" ALSO    ANY
                   MOVE    "＊公費単独"            TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "1" ALSO    ANY
                   MOVE    "＊一般（７０歳以上９割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "2" ALSO    ANY
                   MOVE    "＊一般（７０歳以上８割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "3" ALSO    ANY
                   MOVE    "＊一般被保険者"        TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "4" ALSO    ANY
                   MOVE    "＊一般（３歳未満）"    TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "5" ALSO    ANY
                   MOVE    "＊退職（本人）"        TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "6" ALSO    ANY
                   MOVE    "＊退職（７０歳以上９割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "7" ALSO    ANY
                   MOVE    "＊退職（７０歳以上８割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "8" ALSO    ANY
                   MOVE    "＊退職（被扶養者）"    TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "9" ALSO    ANY
                   MOVE    "＊退職（３歳未満）"    TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "2" ALSO    "1" ALSO    ANY
                   MOVE    "＊老人９割"            TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "2" ALSO    "2" ALSO    ANY
                   MOVE    "＊老人８割"            TO  WRK-PRT-LINE
               WHEN    OTHER
                   GO  TO  3201-SYUBETU-BODY-HEN-EXT
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-SYUBETUKBN    TO  WRK-SYUBETUKBN
           MOVE    WRK-SYUBETUKBN      TO  CSV-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
      *
           .
       3201-SYUBETU-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    詳細区分
      *****************************************************************
       3201-KBN-BODY-HEN-SEC       SECTION.
      *
      *    国保の場合は抜ける
           IF      WRK-TEISYUTU    =   "2"
               MOVE    TOKEI-KBN       TO  WRK-KBN
               MOVE    SPACE           TO  CSV-KBN
               GO  TO  3201-KBN-BODY-HEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-PRT-LINE
      *
           EVALUATE    TOKEI-HKNKBN    ALSO    TOKEI-KBN
               WHEN    "1"     ALSO    "001"
                   MOVE    TOKEI-KBN                   TO  WRK-KBN
                   GO  TO  3201-KBN-BODY-HEN-EXT
               WHEN    "1"     ALSO    "002"
                   MOVE    "・０１（政）"              TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "003"
                   MOVE    "・０２（船）"              TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "004"
                   MOVE    "・０３（日）"              TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "005"
                   MOVE    "・０４（日特）"            TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "006"
                   MOVE    "・３１〜３４（共）"        TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "007"
                   MOVE    "・０６（組）"              TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "008"
                   MOVE    "・０７（自）"              TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "009"
                   MOVE    "・６３・７２〜７５（退）"  TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "001"
                   MOVE    "・老人と公費の併用"        TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "002"
                   MOVE    "・老人単独"                TO  WRK-PRT-LINE
               WHEN    "3"     ALSO    ANY
      *            公費区分
                   PERFORM 3201-HKNNUM-HENSYU-SEC
               WHEN    OTHER
                   GO  TO  3201-KBN-BODY-HEN-EXT
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-KBN       TO  WRK-KBN
           MOVE    WRK-KBN         TO  CSV-KBN
      *
           .
       3201-KBN-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費保険区分編集処理
      *****************************************************************
       3201-HKNNUM-HENSYU-SEC      SECTION.
      *
      *    保険番号読込み
           INITIALIZE                  HKNNUM-REC
           MOVE   WRK-PARA-HOSPID  TO  HKN-HOSPID
           MOVE   TOKEI-KBN        TO  HKN-HKNNUM
           MOVE   HKNNUM-REC       TO  MCPDATA-REC
           MOVE   "DBSELECT"       TO  MCP-FUNC
           MOVE   "HKNNUM-KEY2"    TO  ORC-DBPATH
           CALL   "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "HKNNUM-KEY2"   TO  ORC-DBPATH
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           PERFORM UNTIL   FLG-HKNNUM      =   1
               IF        ( HKN-TEKSTYMD    <=  WRK-SYSYMD )
                   AND   ( HKN-TEKEDYMD    >=  WRK-SYSYMD )
                   IF      HKN-HKNNUM  =   "027"
      *                老人保険のとき
                       IF    ( SYS-1001-HOSPSBT    =   HKN-PAYKBN(1:1) )
                       AND   ( SYS-1001-ROUPAYKBN  =   HKN-PAYKBN(2:1) )
                           STRING  "（"            DELIMITED  BY  SIZE
                               HKN-TANSEIDONAME    DELIMITED  BY  SPACE
                                   "）"            DELIMITED  BY  SIZE
                                                   INTO   WRK-PRT-LINE
                           END-STRING
                           MOVE    1                   TO  FLG-HKNNUM
                       END-IF
                   ELSE
      *                特定疾患のとき
                       IF      HKN-HKNNUM  =   "051"
                           MOVE    "（特定疾患）"  TO   WRK-PRT-LINE
                       ELSE
      *                その他の保険のとき
                           STRING  "（"            DELIMITED  BY  SIZE
                               HKN-TANSEIDONAME    DELIMITED  BY  SPACE
                                   "）"            DELIMITED  BY  SIZE
                                                   INTO   WRK-PRT-LINE
                           END-STRING
                       END-IF
                       MOVE    1                   TO  FLG-HKNNUM
                   END-IF
               END-IF
      *
               IF      FLG-HKNNUM           =   ZERO
                   MOVE    "HKNNUM-KEY2"       TO  ORC-DBPATH
                   PERFORM 900-HKNNUM-READ-SEC
               ELSE
                   MOVE    HKN-HBTNUM      TO  WRK-Z4
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    "・"      TO  KANACHK-MAE-INPUT(1:2)
                   MOVE    WRK-Z4 (3:2)    TO  KANACHK-MAE-INPUT(3:2)
                   MOVE    WRK-PRT-LINE    TO  KANACHK-MAE-INPUT(5:)
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF          KANACHK-RC      =   0
                       MOVE    KANACHK-OUT-INPUT   TO  WRK-PRT-LINE
                   END-IF
               END-IF 
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE    "HKNNUM-KEY2"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       3201-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3202-BODY-SUB-HEN-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT4-AREA
           MOVE    ZERO            TO  IDX
      *
           PERFORM UNTIL ( FLG-TOKEI       =   1 )
                   OR    ( WRK-SKYKBN      NOT =   TOKEI-SKYKBN )
                   OR    ( WRK-TEISYUTU    NOT =   TOKEI-TEISYUTU )
                   OR    ( WRK-HKNJANUM    NOT =   TOKEI-HKNJANUM )
                   OR    ( WRK-SYOKBN      NOT =   TOKEI-SYOKBN )
                   OR    ( WRK-SRYYMFLG    NOT =   TOKEI-SRYYMFLG )
                   OR    ( WRK-NYUGAIKBN   NOT =   TOKEI-NYUGAIKBN )
                   OR    ( WRK-ZAIKBN      NOT =   TOKEI-ZAIKBN )
                   OR    ( WRK-HKNKBN      NOT =   TOKEI-HKNKBN )
                   OR    ( WRK-SYUBETUKBN  NOT =   TOKEI-SYUBETUKBN )
                   OR    ( WRK-KBN         NOT =   TOKEI-KBN )
      *        患者編集
               PERFORM 3202-BODY-SUB-SET-SEC
      *        患者別集計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
      *    患者情報セット
           IF      IDX                 >   ZERO
               MOVE    WRK-PRT4-AREA   TO  WRK-PRT-LINE
               PERFORM 320-BODY-SET-SEC
           END-IF
      *
           .
       3202-BODY-SUB-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理２
      *****************************************************************
       3202-BODY-SUB-HEN2-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT4-AREA
           MOVE    ZERO            TO  IDX
      *
           PERFORM UNTIL ( FLG-TOKEI       =   1 )
                   OR    ( WRK-SKYKBN      NOT =   TOKEI-SKYKBN )
                   OR    ( WRK-TEISYUTU    NOT =   TOKEI-TEISYUTU )
                   OR    ( WRK-SRYYMFLG    NOT =   TOKEI-SRYYMFLG )
                   OR    ( WRK-NYUGAIKBN   NOT =   TOKEI-NYUGAIKBN )
                   OR    ( WRK-HKNKBN      NOT =   TOKEI-HKNKBN )
                   OR    ( WRK-SYUBETUKBN  NOT =   TOKEI-SYUBETUKBN )
                   OR    ( WRK-KBN         NOT =   TOKEI-KBN )
      *        患者編集
               PERFORM 3202-BODY-SUB-SET-SEC
      *        患者別集計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
      *    患者情報セット
           IF      IDX                 >   ZERO
               MOVE    WRK-PRT4-AREA   TO  WRK-PRT-LINE
               PERFORM 320-BODY-SET-SEC
           END-IF
      *
           .
       3202-BODY-SUB-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3202-BODY-SUB-SET-SEC       SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    "　"                TO  WRK-PRT4-FILLER1(IDX)
           MOVE    SPACE               TO  WRK-PRT4-FILLER2(IDX)
           MOVE    "日"                TO  WRK-PRT4-FILLER3(IDX)
           MOVE    "点"                TO  WRK-PRT4-FILLER4(IDX)
      *
           MOVE    SPACE               TO  WRK-HENPTNUM
           MOVE    ZERO                TO  IDZ
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   20
               IF      TOKEI-PTNUM(IDY:1)  NOT =  SPACE
                   COMPUTE IDZ =   IDZ +   1
                   MOVE    TOKEI-PTNUM(IDY:1)   TO  WRK-HENPTNUM(IDZ:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-HENPTNUM2
           MOVE    8                   TO  IDZ
      *
           PERFORM VARYING     IDY     FROM    8   BY  -1
                   UNTIL       IDY     <   1
               IF      WRK-HENPTNUM(IDY:)   NOT =  SPACE
                   MOVE    WRK-HENPTNUM(IDY:1)  TO  WRK-HENPTNUM2(IDZ:1)
                   COMPUTE IDZ =   IDZ -   1
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDY     FROM    IDZ   BY  -1
                   UNTIL       IDY     <   1
               MOVE    " "             TO  WRK-HENPTNUM2(IDY:1)
           END-PERFORM
      *
           MOVE    WRK-HENPTNUM2       TO  WRK-PRT4-PTNUM(IDX)
      *
           MOVE    TOKEI-NISSU         TO  WRK-Z5
           MOVE    WRK-Z5              TO  WRK-PRT4-NISSU(IDX)
           MOVE    TOKEI-TENSU         TO  WRK-Z8
           MOVE    WRK-Z8              TO  WRK-PRT4-TENSU(IDX)
           MOVE    TOKEI-PTNUM         TO  CSV-PTNUM
           MOVE    TOKEI-RENNUM        TO  CSV-RENNUM
           MOVE    TOKEI-NISSU         TO  CSV-NISSU
           MOVE    TOKEI-TENSU         TO  CSV-TENSU
      *
      *    患者名セット
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    TOKEI-NAME          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF          KANACHK-RC      =   0
               COMPUTE KANACHK-MAX     =   KANACHK-MAX +   1
               PERFORM UNTIL ( KANACHK-MAX >=  18 )
                   MOVE    "　"    TO  KANACHK-OUT-INPUT(KANACHK-MAX:2)
                   COMPUTE KANACHK-MAX     =   KANACHK-MAX +   2
               END-PERFORM
               MOVE    KANACHK-OUT-INPUT(1:18) TO  WRK-PRT4-NAME(IDX)
                                                   CSV-NAME
           ELSE
               MOVE    "　　　　　　　　　"    TO  WRK-PRT4-NAME(IDX)
               MOVE    SPACE                   TO  CSV-NAME
           END-IF
      *
      *    ＣＳＶファイル出力
           INITIALIZE                RECECSV-REC
           STRING  CSV-SKYYM         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SRYKA         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SYOKBN        DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-TEISYUTU      DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-HKNJANUM      DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-HKNJANAME     DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-NYUGAIKBN     DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-ZAIKBN        DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SRYYM         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SYUBETUKBN    DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-KBN           DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-PTNUM         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-RENNUM        DELIMITED  BY  SIZE
                   ","               DELIMITED  BY  SIZE
                   CSV-NAME          DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-NISSU         DELIMITED  BY  SIZE
                   ","               DELIMITED  BY  SIZE
                   CSV-TENSU         DELIMITED  BY  SIZE
                             INTO    RECECSV-REC
           END-STRING
           WRITE   RECECSV-REC
      *
      *    集計
           ADD     1                   TO  SUM-GOK-KENSU(1)
                                           SUM-GOK-KENSU(2)
                                           SUM-GOK-KENSU(3)
           ADD     TOKEI-NISSU         TO  SUM-GOK-NISSU(1)
                                           SUM-GOK-NISSU(2)
                                           SUM-GOK-NISSU(3)
           ADD     TOKEI-TENSU         TO  SUM-GOK-TENSU(1)
                                           SUM-GOK-TENSU(2)
                                           SUM-GOK-TENSU(3)
      *
      *    患者情報セット
           IF      IDX             =   3
               MOVE    WRK-PRT4-AREA   TO  WRK-PRT-LINE
               PERFORM 320-BODY-SET-SEC
               MOVE    SPACE           TO  WRK-PRT4-AREA
               MOVE    ZERO            TO  IDX
           END-IF
      *
           .
       3202-BODY-SUB-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    区分合計行セット
      *****************************************************************
       3202-KBN-BODY-END-SEC       SECTION.
      *
      *     MOVE    SPACE               TO  WRK-PRT-LINE-AREA
      *
           MOVE    SUM-GOK-KENSU(1)    TO  WRK-PRT-KENSU
           MOVE    SUM-GOK-NISSU(1)    TO  WRK-PRT-NISSU
           MOVE    SUM-GOK-TENSU(1)    TO  WRK-PRT-TENSU
           MOVE    "　　　　　　　　　小計"
                                       TO  WRK-PRT-GOKEI
      *    合計行セット
           PERFORM 3203-BODY-END-SET-SEC
      *
           MOVE    ZERO                TO  SUM-GOK-TBL(1)
      *
           .
       3202-KBN-BODY-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    合計行セット
      *****************************************************************
       3203-BODY-END-SET-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT3-AREA
      *
           MOVE    "　"            TO  WRK-PRT3-FILLER1
           MOVE    "件"            TO  WRK-PRT3-FILLER2
           MOVE    "日"            TO  WRK-PRT3-FILLER3
           MOVE    "点"            TO  WRK-PRT3-FILLER4
           MOVE    WRK-PRT-GOKEI   TO  WRK-PRT3-GOKEI
           MOVE    WRK-PRT-KENSU   TO  WRK-Z5
           MOVE    WRK-Z5          TO  WRK-PRT3-KENSU
           MOVE    WRK-PRT-NISSU   TO  WRK-Z5
           MOVE    WRK-Z5          TO  WRK-PRT3-NISSU
           MOVE    WRK-PRT-TENSU   TO  WRK-Z8
           MOVE    WRK-Z8          TO  WRK-PRT3-TENSU
           MOVE    WRK-PRT3-AREA   TO  WRK-PRT-LINE
      *
      *    合計行セット
           PERFORM 320-BODY-SET-SEC
      *
           .
       3203-BODY-END-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3203-BODY-HEN-END-SEC       SECTION.
      *
      *    種別合計
           IF        ( WRK-KBN         =   "001" )
               OR    ( FLG-TOKEI       =   1 )
               MOVE    SPACE           TO  WRK-SYUBETUKBN
           END-IF
      *
      *    保険区分合計
           IF        ( TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG )
               OR    ( TOKEI-HKNKBN    NOT =   WRK-HKNKBN )
               OR    ( FLG-TOKEI       =   1 )
               PERFORM 3203-HKNKBN-BODY-END-SEC
           END-IF
      *
      *    レセプト合計、在総診合計
           IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
               OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
               OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
               OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
               OR    ( TOKEI-ZAIKBN    NOT =   WRK-ZAIKBN )
               OR    ( FLG-TOKEI       =   1 )
               IF      WRK-TEISYUTU    =   "1"
                   PERFORM 3203-ZAIKBN-BODY-END-SEC
                   IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                       OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
                       OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                       OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
                       OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
                       OR    ( FLG-TOKEI       =   1 )
                       OR    ( CNT-LINE        >=  55 )
                       PERFORM 390-PRINT-OUT-SEC
                       PERFORM 310-HEAD-SET-SEC
                   END-IF
               ELSE
                   PERFORM 3203-ZAIKBN-BODY-END-SEC
                   IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                       OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
                       OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                       OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
                       OR    ( FLG-TOKEI       =   1 )
                       OR    ( CNT-LINE        >=  55 )
                       PERFORM 390-PRINT-OUT-SEC
                       PERFORM 310-HEAD-SET-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       3203-BODY-HEN-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理２
      *****************************************************************
       3203-BODY-HEN-END2-SEC      SECTION.
      *
      *    種別合計
           IF        ( WRK-KBN         =   "001" )
               OR    ( FLG-TOKEI       =   1 )
               MOVE    SPACE           TO  WRK-SYUBETUKBN
               MOVE    SPACE           TO  WRK-KBN
           END-IF
      *
      *    保険区分合計
           IF        ( TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG )
               OR    ( TOKEI-HKNKBN    NOT =   WRK-HKNKBN )
               OR    ( FLG-TOKEI       =   1 )
               MOVE    SPACE           TO  WRK-HKNKBN
               MOVE    SPACE           TO  WRK-SYUBETUKBN
               MOVE    SPACE           TO  WRK-KBN
           END-IF
      *
      *    レセプト合計
           IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
               OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
               OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
               OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
               OR    ( TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG )
               OR    ( FLG-TOKEI       =   1 )
               PERFORM 3203-ZAIKBN-BODY-END-SEC
               IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                   OR    ( FLG-TOKEI       =   1 )
                   OR    ( CNT-LINE        >=  55 )
                   PERFORM 390-PRINT-OUT-SEC
                   PERFORM 310-HEAD-SET-SEC
               END-IF
           END-IF
      *
           .
       3203-BODY-HEN-END2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    保険区分合計行セット
      *****************************************************************
       3203-HKNKBN-BODY-END-SEC    SECTION.
      *
      *    国保は処理抜ける
           IF      WRK-TEISYUTU    =   "2"
               GO  TO  3203-HKNKBN-BODY-END-EXT
           END-IF
      *
      *     MOVE    SPACE               TO  WRK-PRT-LINE-AREA
      *
           MOVE    SUM-GOK-KENSU(2)    TO  WRK-PRT-KENSU
           MOVE    SUM-GOK-NISSU(2)    TO  WRK-PRT-NISSU
           MOVE    SUM-GOK-TENSU(2)    TO  WRK-PRT-TENSU
           EVALUATE    WRK-HKNKBN
               WHEN    "1" 
                   MOVE    "　　　　　　　医保合計"
                                       TO  WRK-PRT-GOKEI
               WHEN    "2"
                   MOVE    "　　　　　　　老人合計"
                                       TO  WRK-PRT-GOKEI
               WHEN    "3"
                   MOVE    "　　　　　　　公費合計"
                                       TO  WRK-PRT-GOKEI
               WHEN    OTHER
                   MOVE    "　　　　　　　　　　　"
                                       TO  WRK-PRT-GOKEI
           END-EVALUATE
      *
      *    合計行セット
           PERFORM 3203-BODY-END-SET-SEC
      *
           MOVE    SPACE               TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
           MOVE    ZERO                TO  SUM-GOK-TBL(1)
           MOVE    ZERO                TO  SUM-GOK-TBL(2)
      *
           .
       3203-HKNKBN-BODY-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    在総診区分合計行セット
      *****************************************************************
       3203-ZAIKBN-BODY-END-SEC    SECTION.
      *
      *     MOVE    SPACE               TO  WRK-PRT-LINE-AREA
      *
           MOVE    SUM-GOK-KENSU(3)    TO  WRK-PRT-KENSU
           MOVE    SUM-GOK-NISSU(3)    TO  WRK-PRT-NISSU
           MOVE    SUM-GOK-TENSU(3)    TO  WRK-PRT-TENSU
           EVALUATE    WRK-ZAIKBN
               WHEN    "1" 
                   MOVE    "　　　　　レセプト合計"
                                       TO  WRK-PRT-GOKEI
               WHEN    "2"
                   MOVE    "在総診及び在医総分合計"
                                       TO  WRK-PRT-GOKEI
               WHEN    OTHER
                   MOVE    "　　　　　　　　　　　"
                                       TO  WRK-PRT-GOKEI
           END-EVALUATE
      *    合計行セット
           PERFORM 3203-BODY-END-SET-SEC
      *
           MOVE    SPACE               TO  WRK-ZAIKBN
           MOVE    SPACE               TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
           MOVE    ZERO                TO  SUM-AREA
      *
           .
       3203-ZAIKBN-BODY-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
      *    頁セット
           ADD     1               TO  CNT-PAGE
           MOVE    CNT-PAGE        TO  WRK-Z4
           MOVE    WRK-Z4          TO  WRK-PRT1-PAGE
      *    見出しセット
           MOVE    WRK-PRT1-AREA   TO  HCMSL55-MOJI(2)
           MOVE    WRK-PRT2-AREA   TO  HCMSL55-MOJI(4)
      *
      *    印刷処理
           INITIALIZE                              ORCSPRTAREA
           MOVE    "INS"                       TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM         TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY        TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP      TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-DENPPRTYM          TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD         TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID        TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY       TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"               TO  SPRT-PRTID
           MOVE    "保険請求確認リスト"        TO  SPRT-TITLE
           MOVE    HCMSL55                     TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID         TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID           TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM          TO  SPRT-PRTNM
           MOVE    "1"                         TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       390-PRINT-OUT-EXT.
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
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT                  RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"               USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
           END-IF
      *
           MOVE    1               TO  FLG-END
                                       FLG-ERR
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
           IF          CNT-PAGE    =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG014 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関基本情報読み込み
      *****************************************************************
       900-SYS1001-READ-SEC        SECTION.
      *
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-SYSYMD      TO  ORC-DBYMD
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF        ( MCP-RC      =   ZERO )
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF        ( MCP-RC      =   ZERO )
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
                   INITIALIZE                  SYS-1001-REC
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
            .
       900-SYS1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE10
           MOVE    SPACE               TO  WRK-KEY
      *
      *    請求管理ＤＢ検索
           MOVE    SPACE                   TO  RECE10-REC
           INITIALIZE                          RECE10-REC
           MOVE    WRK-PARA-HOSPID         TO  RECE10-HOSPID
           MOVE    WRK-PARA-DENPPRTYM      TO  RECE10-SKYYM
      *    保険者が指定されている場合は条件にセット
           IF          WRK-PARA-HKNJANUM   NOT =   SPACE
               MOVE    WRK-PARA-HKNJANUM   TO  RECE10-XXSTHKNJANUM
               MOVE    WRK-PARA-HKNJANUM   TO  RECE10-XXEDHKNJANUM
           ELSE
               MOVE    "00000000"          TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           END-IF
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC          =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECE10
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
     *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           MOVE    WRK-PATH        TO  ORC-DBPATH
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-RECE10
               MOVE    MCPDATA-REC     TO  RECE10-REC
           ELSE
               MOVE    1                   TO  FLG-RECE10
               GO  TO  900-SEIKYU-READ-EXT
           END-IF
      *
      *    提出先チェック
           IF        ( RECE10-HKNNUM-S =   "060" )
               OR    ( RECE10-HKNNUM-S =   "067" )
               IF      WRK-PARA-TEISYUTU   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
               MOVE    "2"             TO  WRK-TEISYUTU
           ELSE
               IF      WRK-PARA-TEISYUTU   =   "2"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
               MOVE    "1"             TO  WRK-TEISYUTU
           END-IF
      *
      *    保険者チェック
           IF        ( WRK-PARA-HKNJANUM   NOT =   SPACE )
               AND   ( WRK-PARA-HKNJANUM   NOT =   RECE10-HKNJANUM-S )
               GO  TO  900-SEIKYU-READ-SEC
           END-IF
      *
      *    入外区分チェック
           EVALUATE    RECE10-NYUGAIKBN
               WHEN    "1"
                   IF      WRK-PARA-NYUGAIKBN  =   "2"
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
                   MOVE    "2"         TO  WRK-NYUGAIKBN
               WHEN    "2"
                   IF      WRK-PARA-NYUGAIKBN  =   "1"
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
                   MOVE    "1"         TO  WRK-NYUGAIKBN
           END-EVALUATE
      *
      *    診療年月
           MOVE    RECE10-SRYYM    TO  WRK-SRYYM
      *
           MOVE    ZERO            TO  WRK-HENSRYYM-G
                                       WRK-SRYYMFLG
           MOVE    RECE10-SRYYM    TO  WRK-HENSRYYM
           MOVE    WRK-PARA-DENPPRTYM  TO  WRK-HENDENPYM
           COMPUTE WRK-HENSYY      =   WRK-HENDENP-YY
                                   -   WRK-HENSRY-YY
           COMPUTE WRK-HENSMM      =   WRK-HENDENP-MM
                                   -   WRK-HENSRY-MM
           IF      WRK-HENSYY      <=  0
               MOVE    WRK-HENSMM  TO  WRK-SRYYMFLG
           ELSE
               COMPUTE WRK-SRYYMFLG    =   WRK-HENSYY * 12
                                       +   WRK-HENSMM
           END-IF
      *    在総診チェック
           IF          RECE10-ZAITAKU      =   "1"
               IF      WRK-PARA-ZAIKBN     =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
               MOVE    "2"             TO  WRK-ZAIKBN
           ELSE
               IF      WRK-PARA-ZAIKBN     =   "2"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
               MOVE    "1"             TO  WRK-ZAIKBN
           END-IF
      *
           IF      WRK-PATH        =   "SEIKYU-KEY5"
      *        請求区分チェック（請求しない分）
               IF      RECE10-SKYKBN   NOT =   "0"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
               MOVE    "2"             TO  WRK-SKYKBN
               MOVE    "00000000"      TO  WRK-HKNJANUM
               MOVE    "3"             TO  WRK-SYOKBN
               MOVE    "1"             TO  WRK-ZAIKBN
           ELSE
      *        請求区分チェック（請求分）
               MOVE    "1"             TO  WRK-SKYKBN
               IF      WRK-TEISYUTU    =   "2"
                   MOVE    RECE10-HKNJANUM-S   TO  WRK-HKNJANUM
               ELSE
                   MOVE    "00000000"          TO  WRK-HKNJANUM
               END-IF
      *        返戻分チェック
               IF      RECE10-HENREIYM     =   "000000"
                   IF      WRK-SRYYM       =   WRK-PARA-DENPPRTYM
                       MOVE    "1"             TO  WRK-SYOKBN
                   ELSE
                       MOVE    "3"             TO  WRK-SYOKBN
                   END-IF
               ELSE
                   MOVE    "2"             TO  WRK-SYOKBN
               END-IF
               IF        ( WRK-PARA-SYOKBN     =   "1" )
                   OR    ( WRK-PARA-SYOKBN     =   "2" )
                   OR    ( WRK-PARA-SYOKBN     =   "3" )
                   IF      WRK-SYOKBN      NOT =   WRK-PARA-SYOKBN
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    RECE10-HOSPID       TO  PTINF-HOSPID
           MOVE    RECE10-PTID         TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM 900-PTINF-READ-SEC
           IF        ( FLG-PTINF           =   ZERO )
               AND   ( PTINF-TSTPTNUMKBN   =   "1"  )
               GO  TO  900-SEIKYU-READ-SEC
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタークローズ
      *****************************************************************
       900-SEIKYU-CLOSE-SEC        SECTION.
      *
           MOVE    WRK-PATH        TO  ORC-DBPATH
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスタ読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
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
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
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
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
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
      *    保険者情報マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "HKNJAINF-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "HKNJAINF-KEY"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC     TO  HKNJAINF-REC
               ELSE
                   MOVE    1               TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "HKNJAINF-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別ファイル読込
      *****************************************************************
       900-TOKEI-READ-SEC          SECTION.
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
      *    レセプト種別ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC         SECTION.
      *
           READ    TOKEI-FILE     NEXT
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
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
       900-DBDISCONNECT-EXT.
           EXIT.
