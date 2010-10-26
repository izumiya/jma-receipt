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
       PROGRAM-ID.             ORCBINF1.
      *****************************************************************
      *  ★修正時には必ずプログラム識別番号をリセットすること
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定点調査
      *  コンポーネント名  : 感染症サーベイランス報告データ作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  09/11/30    NACL−伊藤    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-伊藤    10/ 2/ 8  数字編集小数点見直し
      *  04.05.00    NACL-伊藤    10/10/26  数字編集小数点見直し
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    感染症データ
           SELECT  INFEC-FILE      ASSIGN  WRK-PARA-INFECFILE
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-INFEC.
      *
      *    エラーファイル
           SELECT  ERR-FILE        ASSIGN  WRK-PARA-ERRFILE
                                   FILE    STATUS  IS  STS-ERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    感染症データ
       FD  INFEC-FILE.
       01  INFEC-REC               PIC X(3000).
      *
      *    エラーファイル
       FD  ERR-FILE.
       01  ERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *01  PRGSKBNO                PIC X(08)  VALUE "20091218".
       01  PRGSKBNO                PIC X(08)  VALUE "20101026".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-INFEC                       PIC X(02).
           03  STS-ERR                         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-JYURRK                      PIC 9(01).
           03  FLG-PTBYOMEI                    PIC 9(01).
           03  FLG-SRYKARRK                    PIC 9(01).
           03  FLG-INFECTION                   PIC 9(01).
      *
           03  FLG-SYORI                       PIC 9(01).
           03  FLG-CHK-END                     PIC 9(01).
           03  FLG-INFURU                      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-INFEC                       PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDU                             PIC 9(03).
           03  IDV                             PIC 9(03).
           03  IDW                             PIC 9(03).
           03  IDX                             PIC 9(03).
           03  IDY                             PIC 9(03).
           03  IDZ                             PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                  PIC 9(10).
           03  KEY-OLD.
               05  KEY-O-PTID                  PIC 9(10).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(32).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
               05  WRK-PARA-SRYYMD             PIC X(08).
               05  WRK-PARA-NYUGAIKBN          PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-INFECFILE          PIC X(128).
               05  WRK-PARA-ERRFILE            PIC X(128).
      *
           03  WRK-HENSYU-AREA.
               05  WRK-SRYYMD                  PIC 9(08).
               05  WRK-S-SRYCD                 PIC X(09).
               05  WRK-M-SRYCD                 PIC X(09).
               05  WRK-SYOSAIKBN               PIC 9(01).
               05  WRK-SYOSINYMD               PIC 9(08).
               05  WRK-BYOMEICD                PIC X(07).
               05  WRK-BYOSRYYMD               PIC 9(08).
               05  WRK-TENKIKBN                PIC X(01).
               05  WRK-TENKIYMD                PIC 9(08).
               05  WRK-SYOUNIKA                PIC 9(01).
      *
      *    出力データ編集用
           03  WRK-REC                         PIC X(3000).
           03  WRK-REC-LENGTH                  PIC 9(04).
           03  WRK-DATA-HENSYU-AREA.
               05  WRK-DATA                    PIC X(200).
               05  WRK-NUM                     PIC ZZZZZZZZZ9.99999.
               05  WRK-NAGASA                  PIC 9(03).
               05  WRK-ST                      PIC 9(02).
               05  WRK-SYOSUU                  PIC 9(01).
               05  WRK-END                     PIC 9(01).
      *
           03  WRK-INFECERR                    PIC X(200).
           03  WRK-ERRMSG                      PIC X(300).
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
      *    インフルエンザ関連病名コード
       01  TBL-INFURU-AREA.
           03  FILLER              PIC X(07)  VALUE  "4871001".
           03  FILLER              PIC X(07)  VALUE  "8842079".
           03  FILLER              PIC X(07)  VALUE  "8842080".
           03  FILLER              PIC X(07)  VALUE  "8842081".
           03  FILLER              PIC X(07)  VALUE  "8842082".
           03  FILLER              PIC X(07)  VALUE  "8830710".
           03  FILLER              PIC X(07)  VALUE  "8830711".
           03  FILLER              PIC X(07)  VALUE  "8830712".
           03  FILLER              PIC X(07)  VALUE  "8830713".
           03  FILLER              PIC X(07)  VALUE  "8830714".
           03  FILLER              PIC X(07)  VALUE  "8830715".
           03  FILLER              PIC X(07)  VALUE  "8830716".
           03  FILLER              PIC X(07)  VALUE  "8830717".
           03  FILLER              PIC X(07)  VALUE  "8830719".
           03  FILLER              PIC X(07)  VALUE  "8830718".
           03  FILLER              PIC X(07)  VALUE  "8830720".
           03  FILLER              PIC X(07)  VALUE  "8830721".
           03  FILLER              PIC X(07)  VALUE  "8830722".
           03  FILLER              PIC X(07)  VALUE  "8830723".
           03  FILLER              PIC X(07)  VALUE  "8830724".
           03  FILLER              PIC X(07)  VALUE  "8830725".
           03  FILLER              PIC X(07)  VALUE  "8830726".
           03  FILLER              PIC X(07)  VALUE  "8830727".
           03  FILLER              PIC X(07)  VALUE  "8830728".
           03  FILLER              PIC X(07)  VALUE  "3810006".
           03  FILLER              PIC X(07)  VALUE  "8843828".
           03  FILLER              PIC X(07)  VALUE  "8830730".
           03  FILLER              PIC X(07)  VALUE  "8830731".
           03  FILLER              PIC X(07)  VALUE  "8832283".
           03  FILLER              PIC X(07)  VALUE  "8845197".
           03  FILLER              PIC X(07)  VALUE  "8843940".
           03  FILLER              PIC X(07)  VALUE  "8845217".
       01  TBL-INFURU-AREA-R       REDEFINES  TBL-INFURU-AREA.
           03  TBL-INFURU-OCC      OCCURS   32  INDEXED  IDX-INFURU.
               05  TBL-INFURU-BYOMEICD    PIC X(07).
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
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
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    感染症プルーフ
       01  INFECTION-REC.
           COPY    "CPINFECTION.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE                   DIVISION
                                   USING   COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL  (FLG-END =   1)  OR
                                          (FLG-JYURRK =   1)
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  TTNS-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SRYYMD
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-INFECFILE
                                               WRK-PARA-ERRFILE
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM 900-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBINF1"          TO  JOB-PGID
           MOVE    "感染症データ作成"  TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    医療機関基本情報
           INITIALIZE                  SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-SRYYMD     TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                           TO  WRK-INFECERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT              INFEC-FILE
           CLOSE                       INFEC-FILE
      *
           OPEN    OUTPUT              INFEC-FILE
      *
      *    受診履歴読み込み
           INITIALIZE                  JYURRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JYURRK-HOSPNUM
           MOVE    WRK-PARA-SRYYMD     TO  JYURRK-SRYYMD
           MOVE    WRK-PARA-SRYYMD     TO  JYURRK-UPSRYYMD
           MOVE    WRK-PARA-NYUGAIKBN  TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key21"             TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-INIT-SYORI-SEC
           PERFORM UNTIL     (FLG-JYURRK  =   1)
                       OR    ( KEY-NEW     NOT =   KEY-OLD )
                       OR    (PTINF-TSTPTNUMKBN   =   "1")
               MOVE    ZERO                TO  FLG-SYORI
               PERFORM 2002-HENSYU-SYORI-SEC
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者毎の編集処理
      *****************************************************************
       2001-INIT-SYORI-SEC         SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
      *    テスト患者のときは読み飛ばす
           MOVE    JYURRK-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    JYURRK-PTID         TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
      *    テスト患者の時読み飛ばし
           IF     (FLG-PTINF           =   ZERO)  AND
                  (PTINF-TSTPTNUMKBN   =   "1" )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM UNTIL ( FLG-JYURRK      =   1 )
                       OR    ( KEY-NEW     NOT =   KEY-OLD )
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key21"             TO  MCP-PATHNAME
                   PERFORM 900-JYURRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
           END-IF
           .
       2001-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       2002-HENSYU-SYORI-SEC       SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM UNTIL ( FLG-JYURRK      =   1 )
                   OR    ( KEY-NEW     NOT =   KEY-OLD )
               PERFORM 20021-INFECTION-SEC
           END-PERFORM
           .
       2002-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       20021-INFECTION-SEC         SECTION.
      *
           IF      WRK-PARA-NYUGAIKBN  =   1
               MOVE    3                   TO  WRK-SYOSAIKBN
           END-IF
      *    受診履歴の内容判定
           PERFORM UNTIL   ( FLG-JYURRK    =   1 )  OR
                           ( KEY-NEW   NOT =   KEY-OLD )
      *        診察料、投薬料、検査料
               PERFORM 200211-SRYCD-HEN-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key21"             TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-SEC
           END-PERFORM
      *    傷病名の内容判定
           PERFORM 20022-BYOMEI-HENSYU-SEC
      *
      *DLT インフルエンザは検査が必須である
      *DLT 小児科外来診療料(WRK-SYOUNIKA=1)は
      *DLT   医薬品のみ、傷病名のみでも可とする
      *DLT IF    ( WRK-S-SRYCD     NOT =   SPACE )  OR
      *DLT      (( WRK-SYOUNIKA        =   1     )  AND
      *DLT      (( WRK-M-SRYCD     NOT =   SPACE )  OR
      *DLT       ( WRK-BYOMEICD    NOT =   SPACE )))
      *
      *    インフルエンザは検査または医薬品
      *    または傷病名があれば対象とする
           IF    ( WRK-S-SRYCD     NOT =   SPACE )  OR
                 ( WRK-M-SRYCD     NOT =   SPACE )  OR
                 ( WRK-BYOMEICD    NOT =   SPACE )
      *        診療日をセットする
      *        備忘録  傷病名のみの場合は診療開始日を診療日にセット
               MOVE    WRK-PARA-SRYYMD     TO  WRK-SRYYMD
      *        感染症プルーフ存在チェック
               PERFORM 2004-INFECTION-READ-SEC
               IF      FLG-INFECTION       =   1
                   IF      WRK-SYOSINYMD       =   ZERO
                       PERFORM 200212-SYOSINYMD-SEC
                   END-IF
      *            出力処理
                   PERFORM 2003-OUT-SYORI-SEC
                   PERFORM 2004-INFECTION-INSERT-SEC
               END-IF
           END-IF
           .
       20021-INFECTION-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       200211-SRYCD-HEN-SEC        SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15)  OR
                              (JYURRK-ZAINUM(IDX)  =   ZERO)
      *
               MOVE    SPACE                   TO  SRYACT-REC
               INITIALIZE                      SRYACT-REC
               MOVE    JYURRK-HOSPNUM          TO  SRY-HOSPNUM
               MOVE    JYURRK-NYUGAIKBN        TO  SRY-NYUGAIKBN
               MOVE    JYURRK-PTID             TO  SRY-PTID
               MOVE    JYURRK-SRYKA            TO  SRY-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)      TO  SRY-SRYYM
               MOVE    JYURRK-ZAINUM(IDX)      TO  SRY-ZAINUM
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACT-REC
                   MOVE    1               TO  FLG-SRYACT
               END-IF
               MOVE    ZERO                   TO  FLG-CHK-END
               PERFORM UNTIL  (FLG-SRYACT     =   1)  OR
                              (FLG-CHK-END    =   1)
                   EVALUATE    SRY-SRYKBN
                      WHEN     "11"
      *                   初診
                          PERFORM 2002111-SRYKBN-11-SEC
                      WHEN     "12"
      *                   再診
                          IF      WRK-SYOSAIKBN       =   ZERO
                              PERFORM 2002112-SRYKBN-12-SEC
                          END-IF
                      WHEN     "13"
      *                   指導
                          IF      WRK-SYOSAIKBN       =   ZERO
                              PERFORM 2002113-SRYKBN-13-SEC
                          END-IF
      **              WHEN     "14"
      **                  往診
      **                  IF      WRK-SYOSAIKBN       =   ZERO
      **                      PERFORM 2002114-SRYKBN-14-SEC
      **                  END-IF
                      WHEN     "21"
                      WHEN     "22"
                      WHEN     "23"
      *                   投薬
                          IF      WRK-M-SRYCD         =   SPACE
                              PERFORM 2002115-SRYKBN-20-SEC
                          END-IF
                      WHEN     "60"
      *                   検査
                          IF      WRK-S-SRYCD         =   SPACE
                              PERFORM 2002116-SRYKBN-60-SEC
                          END-IF
                      WHEN     OTHER
                          MOVE     1              TO   FLG-CHK-END
                   END-EVALUATE
      *
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACT-READ-SEC
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-PERFORM
           .
       200211-SRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料判定処理
      *****************************************************************
       2002111-SRYKBN-11-SEC       SECTION.
      *
           MOVE    1                   TO  WRK-SYOSAIKBN
      *
      *    PERFORM VARYING     IDY     FROM    1   BY  1
      *            UNTIL      (IDY     >    5)  OR
      *                       (SRY-SRYCD (IDY) =   SPACE )
      *        IF       SRY-SRYCD(IDY)(1:1)    =   "1"
      *            MOVE    SRY-SRYCD(IDY)      TO  TNS-SRYCD
      *            PERFORM 900-TENSU-READ-SEC
      *            IF      TNS-DATAKBN         =   "1"
      *                MOVE    1                   TO  WRK-SYOSAIKBN
      *            ELSE
      *            END-IF
      *        ELSE
      *            IF  (SRY-SRYCD(IDY)         =   "099110001") OR
      *                (SRY-SRYCD(IDY)         =   "830000020")
      *                MOVE    1                   TO  WRK-SYOSAIKBN
      *            END-IF
      *        END-IF
      *    END-PERFORM
           .
       2002111-SRYKBN-11-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診料判定処理
      *****************************************************************
       2002112-SRYKBN-12-SEC       SECTION.
      *
           MOVE    2                   TO  WRK-SYOSAIKBN
           .
       2002112-SRYKBN-12-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料判定処理
      *****************************************************************
       2002113-SRYKBN-13-SEC       SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5)  OR
                              (SRY-SRYCD (IDY) =   SPACE)
               EVALUATE    SRY-SRYCD(IDY)
      *            小児科外来診療料(院外処方 初診)
                   WHEN    "113003510"
      *            小児科外来診療料(院内処方 初診)
                   WHEN    "113003710"
                       MOVE    1                   TO  WRK-SYOSAIKBN
                       MOVE    1                   TO  WRK-SYOUNIKA
      *
      *            小児科外来診療料(院外処方 再診)
                   WHEN    "113003610"
      *            小児科外来診療料(院内処方 再診)
                   WHEN    "113003810"
                       MOVE    2                   TO  WRK-SYOSAIKBN
                       MOVE    1                   TO  WRK-SYOUNIKA
               END-EVALUATE
           END-PERFORM
           .
       2002113-SRYKBN-13-EXT.
           EXIT.
      *
      *****************************************************************
      *    往診判定処理
      *****************************************************************
       2002114-SRYKBN-14-SEC       SECTION.
      *
           .
       2002114-SRYKBN-14-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬料判定処理
      *****************************************************************
       2002115-SRYKBN-20-SEC       SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5)  OR
                              (SRY-SRYCD (IDY) =   SPACE)
               EVALUATE    SRY-SRYCD(IDY)
      *            タミフル
                   WHEN    "610443074"
                   WHEN    "610462002"
      *            リレンザ
                   WHEN    "660443018"
      *            イナビル
                   WHEN    "622012101"
                       MOVE    SRY-SRYCD(IDY)      TO  WRK-M-SRYCD
                       MOVE    5                   TO  IDY
               END-EVALUATE
           END-PERFORM
           .
       2002115-SRYKBN-20-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査料判定処理
      *****************************************************************
       2002116-SRYKBN-60-SEC       SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5)  OR
                              (SRY-SRYCD (IDY) =   SPACE)
               EVALUATE    SRY-SRYCD(IDY)
      *            インフルエンザ
                   WHEN    "160042210"
                   WHEN    "160042310"
                   WHEN    "160169450"
                       MOVE    SRY-SRYCD(IDY)      TO  WRK-S-SRYCD
                       MOVE    5                   TO  IDY
               END-EVALUATE
           END-PERFORM
           .
       2002116-SRYKBN-60-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診算定日処理
      *****************************************************************
       200212-SYOSINYMD-SEC        SECTION.
      *
      *    診療科履歴読込
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    PTINF-PTID          TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYKARRK-READ-SEC
           ELSE
               INITIALIZE                      SRYKARRK-REC
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
      *
           PERFORM UNTIL    FLG-SRYKARRK   =   1
               IF      KARRK-SYOSINYMD2    NOT =   SPACE
                   IF      KARRK-SYOSINYMD2        >   WRK-SYOSINYMD
                       MOVE    KARRK-SYOSINYMD2    TO  WRK-SYOSINYMD
                   END-IF
               END-IF
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYKARRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       200212-SYOSINYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名情報編集処理
      *****************************************************************
       20022-BYOMEI-HENSYU-SEC     SECTION.
      *
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    PTINF-PTID          TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM UNTIL    FLG-PTBYOMEI   =   1
               PERFORM 200221-BYOMEI-HENSYU2-SEC
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       20022-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名編集処理２
      *****************************************************************
       200221-BYOMEI-HENSYU2-SEC   SECTION.
      *
      *    削除レコード
           IF      PTBYO-DLTFLG        =   "1"
               GO  TO  200221-BYOMEI-HENSYU2-EXT
           END-IF
      *
      *    基本病名コード
           IF      PTBYO-KHNBYOMEICD   =   SPACE
               GO  TO  200221-BYOMEI-HENSYU2-EXT
           END-IF
      *
      *    診療開始日
      *    診療開始日は該当日と同一の場合を対象とするよう変更
           IF      PTBYO-SRYYMD        =   WRK-PARA-SRYYMD
               CONTINUE
           ELSE
      **       IF      PTBYO-SRYYMD        <   WRK-PARA-SRYYMD
      **           IF      PTBYO-TENKIKBN      =   SPACE
      **               CONTINUE
      **           ELSE
      **               IF      PTBYO-TENKIYMD      >=  WRK-PARA-SRYYMD
      **                   CONTINUE
      **               ELSE
      **                   GO  TO  200221-BYOMEI-HENSYU2-EXT
      **               END-IF
      **           END-IF
      **       ELSE
                   GO  TO  200221-BYOMEI-HENSYU2-EXT
      **       END-IF
           END-IF
      *
      *    入外は無視する
      *    疑いフラグは無視する
      *    レセプト診療科は無視する
      *
      *    インフルエンザ関連があるか検索
      *
           SET     IDX-INFURU          TO  1
           SEARCH  TBL-INFURU-OCC VARYING  IDX-INFURU
                   AT  END
                       MOVE    ZERO                TO  FLG-INFURU
                   WHEN    PTBYO-KHNBYOMEICD   =   TBL-INFURU-BYOMEICD
                                                   (IDX-INFURU)
                       MOVE    1                   TO  FLG-INFURU
           END-SEARCH
      *
           IF      FLG-INFURU          =   1
               MOVE    PTBYO-KHNBYOMEICD   TO  WRK-BYOMEICD
               MOVE    PTBYO-SRYYMD        TO  WRK-BYOSRYYMD
               MOVE    PTBYO-TENKIKBN      TO  WRK-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  WRK-TENKIYMD
           END-IF
           .
       200221-BYOMEI-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       2003-OUT-SYORI-SEC          SECTION.
      *
           IF      CNT-INFEC           =   ZERO
               PERFORM 400-I-REC-SEC
           END-IF
      *
           PERFORM 400-V-REC-SEC
      *
      *    IF      STS-INFEC           =   "00"
      *        CONTINUE
      *    ELSE
      *        MOVE    SPACE               TO  WRK-INFECERR
      *        STRING "感染症データ 書き込みエラー STS="
      *                                        DELIMITED  BY  SIZE
      *                STS-INFEC               DELIMITED  BY  SIZE
      *                                        INTO    WRK-INFECERR
      *        END-STRING
      *        PERFORM 500-ERR-HENSYU-SEC
      *        PERFORM 500-COBABORT-SEC
      *    END-IF
           .
       2003-OUT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症プルーフ処理
      *****************************************************************
       2004-INFECTION-READ-SEC     SECTION.
      *
           INITIALIZE                      INFECTION-REC
           MOVE    SPA-HOSPNUM         TO  INFECT-HOSPNUM
           MOVE    SMCNDATE-YMD        TO  INFECT-SENDYMD
           MOVE    PTINF-PTID          TO  INFECT-PTID
      *    インフルエンザ
           MOVE    1                   TO  INFECT-DISEASE
           MOVE    WRK-SRYYMD          TO  INFECT-SRYYMD
      *
           MOVE    INFECTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_infection"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_infection"     TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-INFECTION-READ-SEC
           ELSE
               INITIALIZE                      INFECTION-REC
               MOVE    1                   TO  FLG-INFECTION
           END-IF
      *
           MOVE    "tbl_infection"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2004-INFECTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症プルーフ処理
      *****************************************************************
       2004-INFECTION-INSERT-SEC   SECTION.
      *
           INITIALIZE                      INFECTION-REC
           MOVE    SPA-HOSPNUM         TO  INFECT-HOSPNUM
           MOVE    SMCNDATE-YMD        TO  INFECT-SENDYMD
           MOVE    PTINF-PTID          TO  INFECT-PTID
      *    インフルエンザ
           MOVE    1                   TO  INFECT-DISEASE
           MOVE    WRK-SRYYMD          TO  INFECT-SRYYMD
      *
           MOVE    WRK-S-SRYCD         TO  INFECT-SRYCD
           MOVE    WRK-M-SRYCD         TO  INFECT-MEDSRYCD
           MOVE    WRK-SYOSAIKBN       TO  INFECT-SYOSAI
           IF      WRK-SYOSINYMD   NOT =   ZERO
               MOVE    WRK-SYOSINYMD       TO  INFECT-SYOSINYMD
           END-IF
           MOVE    WRK-BYOMEICD        TO  INFECT-BYOMEICD
           IF      WRK-BYOSRYYMD   NOT =   ZERO
               MOVE    WRK-BYOSRYYMD       TO  INFECT-BYOSRYYMD
           END-IF
           MOVE    WRK-TENKIKBN        TO  INFECT-TENKIKBN
           IF      WRK-TENKIYMD    NOT =   ZERO
               MOVE    WRK-TENKIYMD        TO  INFECT-TENKIYMD
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  INFECT-CREYMD
           MOVE    SPACE               TO  INFECT-UPYMD
           MOVE    SMCNDATE-HMS        TO  INFECT-UPHMS
      *
           MOVE    INFECTION-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_infection"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCBINF1 INFECTION INS ERR:"  MCP-RC  "."
           END-IF
           .
       2004-INFECTION-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE   INFEC-FILE
      *
      **   IF      CNT-INFEC           =   ZERO
      **       MOVE    "処理対象のデータがありませんでした"
      **                                   TO  WRK-INFECERR
      **       PERFORM 500-ERR-HENSYU-SEC
      **   END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "ORCBINF1 OUTPUT CNT : " CNT-INFEC
           DISPLAY "*** ORCBINF1 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ（医療機関情報）作成処理
      *****************************************************************
       400-I-REC-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
           MOVE    ZERO                TO  WRK-END
      *    レコード識別
           MOVE    "I"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    都道府県
           MOVE    SYS-1001-PREFNUM    TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    データ作成年月日（マシン日付）
           MOVE    SMCNDATE-YMD        TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    データ作成時間（マシン時間）
           MOVE    SMCNDATE-HMS        TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    医療機関ＩＤ
           MOVE    SYS-1001-HOSPID     TO  WRK-DATA
           MOVE    24                  TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    プログラム識別番号
           MOVE    PRGSKBNO            TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 4002-MOJI-HENSYU-SEC
      *
           PERFORM 4001-FILE-WRITE-SEC
           .
       400-I-REC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ（発生情報）作成処理
      *****************************************************************
       400-V-REC-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
           MOVE    ZERO                TO  WRK-END
      *    レコード識別
           MOVE    "V"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    病原体区分
      *****とりあえずインフルエンザ
           MOVE    1                   TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    性別
           MOVE    PTINF-SEX           TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    郵便番号
           MOVE    PTINF-HOME-POST     TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    診療日
           MOVE    WRK-SRYYMD          TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    診療行為コード
           MOVE    WRK-S-SRYCD         TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    医薬品コード
           MOVE    WRK-M-SRYCD         TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    初診再診区分
           IF        WRK-SYOSAIKBN     =   0
               MOVE    SPACE               TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 4002-MOJI-HENSYU-SEC
           ELSE
               MOVE    WRK-SYOSAIKBN       TO  WRK-NUM
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 4003-NUM-HENSYU-SEC
           END-IF
      *    初診算定日
           IF        WRK-SYOSINYMD     =   0
               MOVE    SPACE               TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 4002-MOJI-HENSYU-SEC
           ELSE
               MOVE    WRK-SYOSINYMD       TO  WRK-NUM
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 4003-NUM-HENSYU-SEC
           END-IF
      *    傷病名コード
           MOVE    WRK-BYOMEICD        TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    診療開始日
           IF        WRK-BYOSRYYMD     =   0
               MOVE    SPACE               TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 4002-MOJI-HENSYU-SEC
           ELSE
               MOVE    WRK-BYOSRYYMD       TO  WRK-NUM
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 4003-NUM-HENSYU-SEC
           END-IF
      *    転帰区分
           MOVE    WRK-TENKIKBN        TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    転帰日
           IF        WRK-TENKIYMD      =   0
               MOVE    SPACE               TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 4002-MOJI-HENSYU-SEC
           ELSE
               MOVE    WRK-TENKIYMD        TO  WRK-NUM
               MOVE    8                   TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 4003-NUM-HENSYU-SEC
           END-IF
      *
           PERFORM 4001-FILE-WRITE-SEC
           .
       400-V-REC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       4001-FILE-WRITE-SEC         SECTION.
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
      *
           MOVE    WRK-REC             TO  INFEC-REC
           WRITE   INFEC-REC
           ADD     1                   TO  CNT-INFEC
           .
       4001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       4002-MOJI-HENSYU-SEC        SECTION.
      *
           IF      WRK-DATA            =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW <       1
                   IF      WRK-DATA (IDW:1)    NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-DATA (1:IDW)
                                               TO  WRK-REC
                       ELSE
                           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                               DELIMITED   BY  SIZE
                                   WRK-DATA(1:IDW)
                                               DELIMITED   BY  SIZE
                                   INTO        WRK-REC
                           END-STRING
                       END-IF
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END             =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
           .
       4002-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.99999の編集
      *****************************************************************
       4003-NUM-HENSYU-SEC         SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
      *
               PERFORM VARYING IDW FROM    WRK-ST    BY  1
                       UNTIL   IDW >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  -   IDW
                        IF      WRK-SYOSUU       >   ZERO
                          COMPUTE IDU     =    11  + WRK-SYOSUU
                          COMPUTE IDZ     =    IDU - IDW  +  1
                          PERFORM VARYING IDV FROM   IDU  BY  -1
                                  UNTIL   IDV <      12
                            IF  WRK-NUM(IDV:1)   =   "0"
                                COMPUTE IDZ      =   IDZ  -  1
                                IF  IDV              =    12
                                    COMPUTE IDZ      =    IDZ  -  1
                                END-IF
                            ELSE
                                MOVE    11       TO  IDV
                            END-IF
                          END-PERFORM
                        END-IF
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                        END-STRING
                        COMPUTE WRK-REC-LENGTH   =   WRK-REC-LENGTH   +
                                                     IDZ
                        MOVE    10               TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
           .
       4003-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   ERR-FILE
           IF      STS-ERR             =   ZERO
               CLOSE   ERR-FILE
           ELSE
               OPEN    OUTPUT              ERR-FILE
      *
               MOVE    WRK-INFECERR        TO  ERR-REC
               WRITE   ERR-REC
               CLOSE   ERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-INFECERR        TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBINF1 "         DELIMITED  BY  SIZE
                   WRK-INFECERR        DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
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
       910-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスタＲＥＡＤ
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-JYURRK
               MOVE    MCPDATA-REC         TO  JYURRK-REC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-PTID         TO  KEY-N-PTID
           END-IF
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴読込
      *****************************************************************
       900-SRYKARRK-READ-SEC       SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYKARRK
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC 
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           .
       900-SRYKARRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名読込
      *****************************************************************
       900-PTBYO-READ-SEC          SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC         TO  PTBYOMEI-REC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症読込
      *****************************************************************
       900-INFECTION-READ-SEC      SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-INFECTION
               MOVE    MCPDATA-REC         TO  INFECTION-REC
           ELSE
               MOVE    1                   TO  FLG-INFECTION
           END-IF
           .
       900-INFECTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数読込
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    WRK-PARA-SRYYMD     TO  TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                       MOVE    ZERO            TO  FLG-TENSU
      *
      *                点数マスタ退避ワークに編集する
                       PERFORM 9001-TTNS-HEN-SEC
                   ELSE
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    1                   TO  FLG-TENSU
                   END-IF
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC           SECTION.
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
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC           SECTION.
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
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC        SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       900-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
