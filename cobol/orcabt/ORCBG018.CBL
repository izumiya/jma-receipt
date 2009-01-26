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
       PROGRAM-ID.                 ORCBG018.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : チェックマスタ・薬剤情報マスタ経過措置薬剤更新
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/09/07    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    経過措置薬剤情報
           SELECT  IN01-FILE       ASSIGN  BG0181
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  IN01-KEY
                                   FILE    STATUS  IS  STS-IN01.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    経過措置薬剤情報
       FD  IN01-FILE.
       01  IN01-REC.
           03  IN01-KEY.
               05  IN01-MSTKBN         PIC X(01).
               05  IN01-PROCKBN        PIC X(01).
               05  IN01-SRYCD-OLD      PIC X(09).
           03  IN01-SRYCD-NEW          PIC X(09).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    経過措置薬剤情報
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01PARA//
                                   BY          //BG0181//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
           COPY    "HCMSL55.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-IN01            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-IN01            PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
           03  FLG-CHKSND          PIC 9(01).
           03  FLG-WKCHKSND        PIC 9(01).
           03  FLG-YAKUJYO         PIC 9(01).
           03  FLG-WKYAKUJYO       PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(03).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-SEQ             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  RECEERR             PIC X(100).
           03  WRK-PARA-SRYYMD     PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-SYMD            PIC X(08).
           03  WRK-ZOGENPTN        PIC X(01).
           03  WRK-ZOGEN           PIC S9(05).
           03  WRK-EDTYMD1         PIC X(10).
           03  WRK-EDTYMD3         PIC X(22).
           03  WRK-YUKOEDYMD-N     PIC X(22).
           03  WRK-SAKYMD-N        PIC X(22).
           03  WRK-MSTKBN          PIC X(01).
           03  WRK-PROCKBN         PIC X(01).
           03  WRK-ZZZZ9-G.
               05  WRK-ZZZZ9       PIC ZZZZ9.
           03  WRK-SRYCD           PIC X(09).
           03  WRK-MSTMSG          PIC X(20).
      *
       01  KEY-AREA.
           03  KEY-MSTKBN                  PIC X(01).
           03  KEY-PROCKBN                 PIC X(01).
      *
       01  HEAD-AREA.
           03  HEAD-FL1                    PIC X(05).
           03  HEAD-NAME-OLD               PIC X(76).
           03  HEAD-NAME-NEW               PIC X(70).
      *
       01  MEISAI-AREA.
           03  MEISAI-NO                   PIC Z(04).
           03  MEISAI-FL1                  PIC X(01).
           03  MEISAI-SRYCD-OLD            PIC X(09).
           03  MEISAI-FL2                  PIC X(01).
           03  MEISAI-NAME-OLD             PIC X(62).
           03  MEISAI-FL3                  PIC X(01).
           03  MEISAI-SRYCD-NEW            PIC X(09).
           03  MEISAI-FL4                  PIC X(01).
           03  MEISAI-NAME-NEW             PIC X(62).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-LINE-MAX              PIC 9(05)   VALUE 55.
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  WKCHKSND-REC.
           COPY    "CPCHKSND.INC"  REPLACING   //CHKSND-//
                                   BY          //WKCHKSND-//.
      *
           COPY    "CPYAKUJYO.INC"  REPLACING  //YAKUJYO-//
                                   BY          //WKYAKUJYO-//.
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    チェックマスタ２
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
      *    薬剤情報マスタ
           COPY    "CPYAKUJYO.INC".
      *
      *    診療行為コード置き換えマスタ
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
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
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
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
                                               WRK-PARA-SRYYMD
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBG018"      TO  JOB-PGID
           MOVE    "チェックマスタ・薬剤情報マスタ経過措置薬剤更新"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    "BG01801"       TO  BG0181-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BG0181-TERMID
           MOVE    SPA-HOSPNUM     TO  BG0181-HOSPNUM
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           PERFORM 800-MCNDATE-SEC
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
      *    有効期限
      *    MOVE    WRK-SRYYMD          TO  WRK-SYMD
      *    PERFORM 800-SEIWA-HEN-SEC
      *    MOVE    WRK-EDTYMD3         TO  WRK-YUKOEDYMD-N
      *
      *    作成日編集
      *    MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
      *    PERFORM 800-SEIWA-HEN-SEC
      *    MOVE    WRK-EDTYMD3         TO  WRK-SAKYMD-N
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-INFILE-EDIT-SEC
      *
           OPEN    INPUT   IN01-FILE
      *
           PERFORM 900-IN01-READ-SEC
      *
           PERFORM UNTIL ( FLG-IN01 NOT =   ZERO )
                     OR  ( FLG-END  NOT =   ZERO )
      *
               MOVE    IN01-MSTKBN     TO  KEY-MSTKBN
               MOVE    IN01-PROCKBN    TO  KEY-PROCKBN
               MOVE    ZERO            TO  CNT-SEQ
      *
               PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                        OR   ( FLG-END     NOT =   ZERO )
                        OR   ( KEY-MSTKBN  NOT =   IN01-MSTKBN )
                        OR   ( KEY-PROCKBN NOT =   IN01-PROCKBN )
      *
      *            ヘッダー編集処理
                   PERFORM 2001-HEAD-HEN-SEC
      *
      *            明細行編集処理
                   PERFORM 2001-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM 2001-PRINT-SEC
      *
               END-PERFORM
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
      *    入力ファイル編集処理
      *****************************************************************
       2001-INFILE-EDIT-SEC            SECTION.
      *
           OPEN    OUTPUT  IN01-FILE
      *
           PERFORM 20011-INFILE-CHKSND-EDIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 20011-INFILE-YAKUJYO-EDIT-SEC
           END-IF
      *
           CLOSE   IN01-FILE
      *
           .
       2001-INFILE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力ファイル編集処理（チェックマスタ）
      *****************************************************************
       20011-INFILE-CHKSND-EDIT-SEC    SECTION.
      *
           PERFORM 900-CHKSND-KEY15-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                     OR  ( FLG-END     NOT =   ZERO )
      *
               MOVE    CHKSND-SRYCD    TO  WRK-SRYCD
               MOVE    WRK-PARA-SRYYMD TO  WRK-SYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( TNS-YUKOEDYMD   =   WRK-PARA-SRYYMD )
                   MOVE    "1"             TO  WRK-ZOGENPTN
                   MOVE    1               TO  WRK-ZOGEN
                   MOVE    WRK-PARA-SRYYMD TO  WRK-SYMD
                   PERFORM 800-CALENDAR-SEC
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       =   ZERO )
                       CONTINUE
                   ELSE
                       PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                       IF    ( FLG-SRYCDCHG        =   ZERO )
                           PERFORM 900-CHKSND-KEY-SEL-SEC
                           IF    ( FLG-WKCHKSND    =   ZERO )
                               MOVE    "1"     TO  WRK-MSTKBN
                               MOVE    "1"     TO  WRK-PROCKBN
                               PERFORM 2001-IN01-EDIT-SEC
                           ELSE
                               PERFORM 2001-CHKSND-EDIT-SEC
                               MOVE    "1"     TO  WRK-MSTKBN
                               MOVE    ZERO    TO  WRK-PROCKBN
                               PERFORM 2001-IN01-EDIT-SEC
                           END-IF
                       ELSE
                           MOVE    "1"         TO  WRK-MSTKBN
                           MOVE    "1"         TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-CHKSND-KEY15-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key15"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-INFILE-CHKSND-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２編集処理
      *****************************************************************
       2001-CHKSND-EDIT-SEC            SECTION.
      *
           MOVE    CHKSND-REC      TO  WKCHKSND-REC
           MOVE    CHG-RJNSRYCD    TO  WKCHKSND-SRYCD
           MOVE    "00000000"      TO  WKCHKSND-YUKOSTYMD
           MOVE    "99999999"      TO  WKCHKSND-YUKOEDYMD
           MOVE    SPACE           TO  WKCHKSND-TERMID
                                       WKCHKSND-OPID
           MOVE    SMCNDATE-YMD    TO  WKCHKSND-CREYMD
                                       WKCHKSND-UPYMD
           MOVE    SMCNDATE-HMS    TO  WKCHKSND-UPHMS
      *
           MOVE    CHKSND-CHKKBN   TO  WKCHKSND-UPCHKSNDKBN
           MOVE    CHKSND-SRYCD    TO  WKCHKSND-UPSRYCD
           MOVE    CHKSND-YUKOSTYMD TO WKCHKSND-UPYUKOSTYMD
           MOVE    CHKSND-YUKOEDYMD TO WKCHKSND-UPYUKOEDYMD
           MOVE    WKCHKSND-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "ins1"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "チェックマスタの更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-CHKSND-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力ファイル編集処理（薬剤情報マスタ）
      *****************************************************************
       20011-INFILE-YAKUJYO-EDIT-SEC   SECTION.
      *
           PERFORM 900-YAKUJYO-ALL-SEL-SEC
      *
           PERFORM UNTIL ( FLG-YAKUJYO  NOT =   ZERO )
                     OR  ( FLG-END     NOT =   ZERO )
      *
               MOVE    YAKUJYO-SRYCD    TO  WRK-SRYCD
               MOVE    WRK-PARA-SRYYMD  TO  WRK-SYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( TNS-YUKOEDYMD   =   WRK-PARA-SRYYMD )
                   MOVE    "1"             TO  WRK-ZOGENPTN
                   MOVE    1               TO  WRK-ZOGEN
                   MOVE    WRK-PARA-SRYYMD TO  WRK-SYMD
                   PERFORM 800-CALENDAR-SEC
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       =   ZERO )
                       CONTINUE
                   ELSE
                       PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                       IF    ( FLG-SRYCDCHG        =   ZERO )
                           PERFORM 900-YAKUJYO-KEY-SEL-SEC
                           IF    ( FLG-WKYAKUJYO    =   ZERO )
                               MOVE    "2"     TO  WRK-MSTKBN
                               MOVE    "1"     TO  WRK-PROCKBN
                               PERFORM 2001-IN01-EDIT-SEC
                           ELSE
                               PERFORM 2001-YAKUJYO-EDIT-SEC
                               MOVE    "2"     TO  WRK-MSTKBN
                               MOVE    ZERO    TO  WRK-PROCKBN
                               PERFORM 2001-IN01-EDIT-SEC
                           END-IF
                       ELSE
                           MOVE    "2"     TO  WRK-MSTKBN
                           MOVE    "1"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-YAKUJYO-ALL-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "all"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-INFILE-YAKUJYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ編集処理
      *****************************************************************
       2001-YAKUJYO-EDIT-SEC           SECTION.
      *
           MOVE    YAKUJYO-REC     TO  WKYAKUJYO-REC
           MOVE    CHG-RJNSRYCD    TO  WKYAKUJYO-SRYCD
           MOVE    SPACE           TO  WKYAKUJYO-NAME
                                       WKYAKUJYO-TERMID
                                       WKYAKUJYO-OPID
           MOVE    SMCNDATE-YMD    TO  WKYAKUJYO-CREYMD
                                       WKYAKUJYO-UPYMD
           MOVE    SMCNDATE-HMS    TO  WKYAKUJYO-UPHMS
      *
           MOVE    WKYAKUJYO-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_yakujyo"    TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "薬剤情報マスタの更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-YAKUJYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力ファイル編集処理
      *****************************************************************
       2001-IN01-EDIT-SEC              SECTION.
      *
           INITIALIZE                  IN01-REC
           MOVE    WRK-MSTKBN      TO  IN01-MSTKBN
           MOVE    WRK-PROCKBN     TO  IN01-PROCKBN
           MOVE    WRK-SRYCD       TO  IN01-SRYCD-OLD
           MOVE    CHG-RJNSRYCD    TO  IN01-SRYCD-NEW
           WRITE   IN01-REC
      *
           .
       2001-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       2001-HEAD-HEN-SEC               SECTION.
      *
           COMPUTE CNT-PAGE            =   CNT-PAGE    +   1
      *
           MOVE    CNT-PAGE            TO  WRK-ZZZZ9
      *
           INITIALIZE                  HCMSL55
      *
           MOVE    SPACE               TO   WRK-MSTMSG
           IF    ( IN01-MSTKBN         =    "1" )
               MOVE    "チェックマスタ" TO  WRK-MSTMSG
           ELSE
               MOVE    "薬剤情報マスタ" TO  WRK-MSTMSG
           END-IF
      *
           STRING
               "＊＊　"                            DELIMITED   BY  SIZE
               WRK-MSTMSG                          DELIMITED   BY  SPACE
                "経過措置薬剤更新リスト"           DELIMITED   BY  SIZE
               "　＊＊　　"                        DELIMITED   BY  SIZE
               "有効期限："                        DELIMITED   BY  SIZE
               WRK-PARA-SRYYMD (1:4)               DELIMITED   BY  SIZE
               "."                                 DELIMITED   BY  SIZE
               WRK-PARA-SRYYMD (5:2)               DELIMITED   BY  SIZE
               "."                                 DELIMITED   BY  SIZE
               WRK-PARA-SRYYMD (7:2)               DELIMITED   BY  SIZE
               "　　　　　　　　　　　　　　　　"  DELIMITED   BY  SIZE
               "（"                                DELIMITED   BY  SIZE
               "作成日："                          DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(1:4)            DELIMITED   BY  SIZE
               "."                                 DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(5:2)            DELIMITED   BY  SIZE
               "."                                 DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(7:2)            DELIMITED   BY  SIZE
               "）"                                DELIMITED   BY  SIZE
               "PAGE "                             DELIMITED   BY  SIZE
               WRK-ZZZZ9-G                         DELIMITED   BY  SIZE
               INTO    HCMSL55-MOJI (1)
           END-STRING
      *
           IF    ( WRK-MSTMSG  =   "1" )
               STRING
                   "※注意：薬剤と病名の区分のみを対象としています。"
                                                   DELIMITED   BY  SIZE
                   "他の区分については対応していません。"
                                                   DELIMITED   BY  SIZE
               INTO    HCMSL55-MOJI (2)
               END-STRING
           END-IF
      *
           IF    ( IN01-PROCKBN    =   ZERO )
               STRING
                   "経過措置薬剤より"              DELIMITED   BY  SIZE
                   WRK-MSTMSG                      DELIMITED   BY  SPACE
                   "を複写しました。"              DELIMITED   BY  SIZE
               INTO    HCMSL55-MOJI (4)
               END-STRING
           ELSE
               STRING
                   "既に"                          DELIMITED BY SIZE
                   WRK-MSTMSG                      DELIMITED   BY  SPACE
                   "に登録済か、"                  DELIMITED BY SIZE
                   "複写先となる薬剤が存在しません。"
                                                   DELIMITED BY SIZE
               INTO    HCMSL55-MOJI (4)
               END-STRING
           END-IF
      *
           INITIALIZE                      HEAD-AREA
           MOVE    ALL "　"            TO  HEAD-NAME-OLD
                                           HEAD-NAME-NEW
           STRING  "経過措置薬剤"      DELIMITED   BY  SIZE
           INTO    HEAD-NAME-OLD
           END-STRING
      *
           STRING  "複写先薬剤"        DELIMITED   BY  SIZE
           INTO    HEAD-NAME-NEW
           END-STRING
           MOVE    HEAD-AREA           TO  HCMSL55-MOJI (5)
           MOVE    ALL "-"             TO  HCMSL55-MOJI (6)
      *
           MOVE    6                   TO  CNT-LINE
      *
           .
      *
       2001-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       2001-BODY-HEN-SEC               SECTION.
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
                    OR   ( KEY-MSTKBN  NOT =   IN01-MSTKBN )
                    OR   ( KEY-PROCKBN NOT =   IN01-PROCKBN )
                    OR   ( CNT-LINE       >=   CONST-LINE-MAX )
      *
               PERFORM 20011-BODY-NAIYO-HEN-SEC
      *
               PERFORM 900-IN01-READ-SEC
      *
           END-PERFORM
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
           INITIALIZE                      MEISAI-AREA
      *
           MOVE    CNT-SEQ             TO  MEISAI-NO
      *
           MOVE    IN01-SRYCD-OLD      TO  MEISAI-SRYCD-OLD
           MOVE    IN01-SRYCD-OLD      TO  WRK-SRYCD
           MOVE    WRK-PARA-SRYYMD     TO  WRK-SYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           MOVE    TNS-NAME            TO  MEISAI-NAME-OLD
      *
           IF    ( IN01-SRYCD-NEW      =   SPACE )
               MOVE    "なし"
                                           TO  MEISAI-SRYCD-NEW
           ELSE
               MOVE    IN01-SRYCD-NEW      TO  MEISAI-SRYCD-NEW
               MOVE    IN01-SRYCD-NEW      TO  WRK-SRYCD
               MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-NAME        TO  MEISAI-NAME-NEW
               ELSE
                   PERFORM 900-TENSU-KEY24-SEL-SEC
                   MOVE    TNS-NAME        TO  MEISAI-NAME-NEW
               END-IF
           END-IF
      *
           INSPECT MEISAI-NAME-OLD REPLACING  ALL "  "  BY  "　"
           INSPECT MEISAI-NAME-NEW REPLACING  ALL "  "  BY  "　"
      *
           MOVE    MEISAI-AREA         TO  HCMSL55-MOJI (CNT-LINE)
      *
           .
       20011-BODY-NAIYO-HEN-EXT.
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
           MOVE    SPACE           TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"   TO  SPRT-PRTID
           IF    ( KEY-MSTKBN    =   "1" )
               MOVE    "チェックマスタ経過措置薬剤更新"
                                   TO  SPRT-TITLE
           ELSE
               MOVE    "薬剤情報マスタ経過措置薬剤更新"
                                   TO  SPRT-TITLE
           END-IF
           MOVE    HCMSL55         TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
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
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
           MOVE    BG0181          TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
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
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           END-IF
      *
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    経過措置薬剤情報読込み処理
      *****************************************************************
       900-IN01-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-IN01
      *
           READ    IN01-FILE           NEXT
               AT  END
               MOVE    1           TO  FLG-IN01
           END-READ
      *
           .
       900-IN01-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY15-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKSND
      *
           INITIALIZE                  CHKSND-REC
           MOVE    WRK-PARA-HOSPNUM TO CHKSND-HOSPNUM
           MOVE    "1"             TO  CHKSND-CHKKBN
           MOVE    WRK-PARA-SRYYMD TO  CHKSND-YUKOSTYMD
                                       CHKSND-YUKOEDYMD
      *
           MOVE    CHKSND-REC      TO  MCPDATA-REC
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key15"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKSND-REC
           ELSE
               INITIALIZE              CHKSND-REC
               MOVE    1           TO  FLG-CHKSND
           END-IF
      *
           .
       900-CHKSND-KEY15-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理(KEY)
      *****************************************************************
       900-CHKSND-KEY15-FET-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key15"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKSND-REC
           ELSE
               INITIALIZE              CHKSND-REC
               MOVE    1           TO  FLG-CHKSND
           END-IF
      *
           .
       900-CHKSND-KEY15-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-WKCHKSND
      *
           MOVE    CHKSND-REC      TO  WKCHKSND-REC
           MOVE    CHG-RJNSRYCD    TO  WKCHKSND-SRYCD
           MOVE    "99999999"      TO  WKCHKSND-YUKOSTYMD
           MOVE    "00000000"      TO  WKCHKSND-YUKOEDYMD
      *
           MOVE    WKCHKSND-REC    TO  MCPDATA-REC
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  WKCHKSND-REC
           ELSE
               INITIALIZE              WKCHKSND-REC
               MOVE    1           TO  FLG-WKCHKSND
           END-IF
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKSND-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ検索処理
      *****************************************************************
       900-YAKUJYO-ALL-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-YAKUJYO
      *
           INITIALIZE                  YAKUJYO-REC
           MOVE    WRK-PARA-HOSPNUM TO YAKUJYO-HOSPNUM
      *
           MOVE    YAKUJYO-REC      TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "all"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  YAKUJYO-REC
           ELSE
               INITIALIZE              YAKUJYO-REC
               MOVE    1           TO  FLG-YAKUJYO
           END-IF
      *
           .
       900-YAKUJYO-ALL-SEL-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ検索処理
      *****************************************************************
       900-YAKUJYO-ALL-FET-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-YAKUJYO
      *
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "all"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  YAKUJYO-REC
           ELSE
               INITIALIZE              YAKUJYO-REC
               MOVE    1           TO  FLG-YAKUJYO
           END-IF
      *
           .
       900-YAKUJYO-ALL-FET-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ検索処理
      *****************************************************************
       900-YAKUJYO-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-WKYAKUJYO
      *
           MOVE    YAKUJYO-REC     TO  WKYAKUJYO-REC
           MOVE    CHG-RJNSRYCD    TO  WKYAKUJYO-SRYCD
      *
           MOVE    WKYAKUJYO-REC   TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  WKYAKUJYO-REC
           ELSE
               INITIALIZE              WKYAKUJYO-REC
               MOVE    1           TO  FLG-WKYAKUJYO
           END-IF
      *
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-YAKUJYO-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コード置換マスタ検索処理
      *****************************************************************
       900-SRYCDCHG-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYCDCHG
      *
           INITIALIZE                  SRYCDCHG-REC
           MOVE    WRK-PARA-HOSPNUM TO CHG-HOSPNUM
           MOVE    WRK-SRYCD        TO  CHG-IPNSRYCD
      *
           MOVE    SRYCDCHG-REC    TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYCDCHG-REC
           ELSE
               INITIALIZE              SRYCDCHG-REC
               MOVE    1           TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "tbl_srycdchg"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYCDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM TO TNS-HOSPNUM
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-SYMD        TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY24-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM TO TNS-HOSPNUM
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key24"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key24"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY24-SEL-EXT.
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
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBINSERT-EXT.
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
